package br.gov.lexml.xloom.processor

import java.io.InputStream
import java.net.URI
import java.net.URL
import org.w3c.dom.Document
import org.w3c.dom.Node
import org.xml.sax.InputSource
import br.gov.lexml.xloom.model._
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.dom.DOMResult
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.sax.SAXSource
import javax.xml.transform.stream.StreamSource
import javax.xml.transform.Source
import javax.xml.transform.TransformerFactory
import javax.xml.transform.URIResolver
import javax.xml.xpath.XPathFactory
import org.w3c.dom.Element
import grizzled.slf4j.Logging
import scala.language.implicitConversions

object QNames {
  val ns = "http://www.lexml.gov.br/schema/xloom"
  def qn(n: String) = QName(n, Some(ns))

  val nsXsl = "http://www.w3.org/1999/XSL/Transform"

  val stylesheet = QName("stylesheet", Some(nsXsl))
  val include = qn("include")
  val _match = qn("match")
  val replace = qn("replace")
  val insertBefore = qn("insertBefore")
  val insertAfter = qn("insertAfter")
  val clear = qn("clear")
  val replaceChildren = qn("replaceChildren")
  val addFirst = qn("addFirst")
  val addLast = qn("addLast")
  val clearChildren = qn("clearChildren")
  val clearAttributes = qn("clearAttributes")
  val setAttributes = qn("setAttributes")
  val removeAttributes = qn("removeAttributes")
  val externalXsl = qn("externalXsl")
  val prefixId = qn("prefixId")
  val _thisEl = qn("this")

  val href = QName("href")
  val select = QName("select")
  val _this = QName("this")
  val preProcess = QName("preProcess")
  val prefix = QName("prefix")
  val idAttr = QName("id")
}

class XLoomProcessor(resolver: URIResolver) extends Logging {
  class XLoomDocumentProcessor(base: Option[String]) extends Logging {
    private[this] val Q = QNames
    private[this] def getDocument(href: String): XElement = {
      val n = readDocument(resolver.resolve(href, base.orNull))
      XNode.node2XNode(n.getDocumentElement).asInstanceOf[XElement]
    }
    private[this] def processCommands(nl: List[XNode]): XNode => List[XNode] = {
      def id(x: XNode) = List(x)
      val cmds = nl collect {
        case e: XElement => processCommand(e)
      }
      cmds.foldLeft(id _) { case (f1, f) => f1 andThen { _.flatMap(f) } }
    }
    private[this] def processCommand(el: XElement): XNode => List[XNode] = el.name match {
      case Q.include => {
        val href = el.attributes(Q.href)
        val select = el.attributes.get(Q.select)
        val preProcess = el.attributes.get(Q.preProcess).collect({ case "true" => true }).getOrElse(false)
        val subCmd = processCommands(el.children)
        val doc1 = getDocument(href)
        val nl1 = if (preProcess) {
          processIncludes(doc1)
        } else { List(doc1) }
        val proc: XNode => List[XNode] = select match {
          case None => doc => List(doc)
          case Some(xpt) => doc => {
            val xp = XPathFactory.newInstance().newXPath()
            xp.setNamespaceContext(el)
            doc.query(xp.compile(xpt))
          }
        }        
        _ => nl1.flatMap(proc).flatMap(subCmd)
      }
      case Q.prefixId => {
        val prefix = el.attributes(Q.prefix)       
        _.replace({
            case e : XElement if e.attributes.contains(Q.idAttr) => {
              val oldId = e.attributes(Q.idAttr)
              val newElement = e copy (attributes = e.attributes + (Q.idAttr -> (prefix + oldId)))              
              List(newElement)
            }
        } : PartialFunction[XNode,List[XNode]])
      }

      case Q._match => {
        val select = el.attributes(Q.select)
        val xp = XPathFactory.newInstance().newXPath()
        xp.setNamespaceContext(el)
        val xpe = xp.compile(select)

        val subCmd = processCommands(el.children)
        _.replaceByXPath(xpe, subCmd)
      }

      case Q.replace => { (n: XNode) =>
        def hasThis(el: XElement) = el.attributes.get(Q._this).collect({ case "true" => true }).
          getOrElse(false)
        val replaceThis: PartialFunction[XNode, List[XNode]] = {
          case el: XElement if hasThis(el) => List(n)
          case el: XElement if el.name == Q._thisEl => List(n)
        }

        if (hasThis(el)) {
          el.children.flatMap(_.topDownUntil(replaceThis))
        } else {
          el.children
        }
      }

      case Q.insertBefore => n => el.children :+ n
      case Q.insertAfter => n => n :: el.children
      case Q.clear => _ => List()
      case Q.replaceChildren => n => List(n.replaceChildren(el.children))
      case Q.addFirst => n => List(n.insertBeforeFirstChild(el.children))
      case Q.addLast => n => List(n.addAfterLastChild(el.children))
      case Q.clearChildren => n => List(n.clearChildren)
      case Q.externalXsl => {
        val href = el.attributes(Q.href)
        val xslSrc = resolver.resolve(href, base.orNull)
        val tf = TransformerFactory.newInstance().newTransformer(xslSrc)
        _.applyTransformer(tf)
      }
      case Q.stylesheet => {
        val xslSrc = new DOMSource(XNode.xnode2Node(el))
        val tf = TransformerFactory.newInstance().newTransformer(xslSrc)
        _.applyTransformer(tf)
      }
      case Q.clearAttributes => n => List(n.clearAttributes)
      case Q.setAttributes => {
        n => List(n.modifyAttributes(m => m ++ el.attributes))
      }
      case Q.removeAttributes => n => List(n.modifyAttributes(m => m -- el.attributes.keys))
      case qn => n => List(n)
    }

    def processIncludes(xnode: XNode): List[XNode] = {
      def processIncludeTD: XNode => List[XNode] = _.topDownUntil(processInclude)
      def processInclude: PartialFunction[XNode, List[XNode]] = {
        case e: XElement if e.name == Q.include => {
          val nl = processCommand(e)(null)
          nl.flatMap(processIncludeTD)
        }
      }
      processIncludeTD(xnode)
    }
  }

  /**
   * Processa diretivas de XLoom em uma árvore.
   * 
   * @param n Raiz da árvore a ser processada
   * @returns Lista de nós representando a floresta resultante do 
   * 		  processamento.
   */
  def processIncludeL(n: Node): List[Node] = {        
    val ownerDoc = n match {
      case d : Document => d
      case n => n.getOwnerDocument
    }
    val base = Option(ownerDoc).flatMap(n => Option(n.getBaseURI)).filter(!_.isEmpty)
    new XLoomDocumentProcessor(base).processIncludes(XNode.node2XNode(n)).
      map(XNode.xnode2Node)
  }

  /**
   * Processa as diretivas XLoom de uma árvore, retornanado apenas
   * o primeiro elemento do resultado ou <code>null</code> se o resultado
   * for vazio.
   * 
   * @param n raiz da ávore a ser processada
   * @returns primeiro elemento do resultado ou <code>null</code> se o
   *          resultado for vazio.
   */
  def processIncludeN(n: Node): Node = {
    processIncludeL(n).headOption.map(_.getParentNode).orNull
  }

  /**
   * Faz o parse e processamento de diretivas XLoom de um documento,
   * retornando o primeiro elemento do resultado.
   */
  def processInclude(s: Source): Node = s match {
    case ss: StreamSource => processInclude(ss.getInputStream(), ss.getSystemId())
    case ds: DOMSource => processIncludeN(ds.getNode())
    case ss: SAXSource => processInclude(ss.getInputSource())
    case s => processIncludeN(readDocument(s))
  }

  /**
   * Faz o parse e processamento de diretivas XLoom de um documento,
   * retornando o primeiro elemento do resultado.
   */
  def processInclude(is: InputStream, systemID: String): Node = {
    val isrc = new InputSource(is)
    isrc.setSystemId(systemID)
    processIncludeN(readDocument(isrc))
  }

  /**
   * Faz o parse e processamento de diretivas XLoom de um documento,
   * retornando o primeiro elemento do resultado.
   */
  def processInclude(is: InputSource): Node = processIncludeN(readDocument(is))

  /**
   * Faz o parse e processamento de diretivas XLoom de um documento,
   * retornando o primeiro elemento do resultado.
   */
  def processInclude(uri: URI): Document = {
    processInclude(resolver.resolve(uri.toASCIIString(), "")).asInstanceOf[Document]
  }
  /**
   * Faz o parse e processamento de diretivas XLoom de um documento,
   * retornando o primeiro elemento do resultado.
   */
  def processInclude(url: URL): Document = {
    processInclude(url.toURI())
  }
  /**
   * Faz o parse e processamento de diretivas XLoom de um documento,
   * retornando o primeiro elemento do resultado.
   */
  def processInclude(uri: String): Document = {
    processInclude(URI.create(uri))
  }
  
  private[this] def readDocument(s: InputSource): Document = {
    val dbf = DocumentBuilderFactory.newInstance()
    dbf.setNamespaceAware(true)
    val db = dbf.newDocumentBuilder()
    val d = db.parse(s)
    if (d.getBaseURI() == null && s.getSystemId() != null) {
      d.setDocumentURI(s.getSystemId())
    }
    d
  }
  private[this] def readDocument(s: Source): Document = {
    val tf = TransformerFactory.newInstance.newTransformer
    val dr = new DOMResult()
    tf.transform(s, dr)
    val d = dr.getNode().asInstanceOf[Document]
    if (d.getBaseURI() == null && s.getSystemId() != null) {
      d.setDocumentURI(s.getSystemId())
    }
    d
  }
}

