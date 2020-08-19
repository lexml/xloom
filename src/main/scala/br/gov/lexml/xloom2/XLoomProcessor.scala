package br.gov.lexml.xloom2

import javax.xml.transform.Result
import javax.xml.transform.Source
import scales.utils._
import scales.xml._
import scales.utils.ScalesUtils._
import scales.xml.ScalesXml._
import org.xml.sax.InputSource
import scales.xml.impl.XmlUtils
import scales.utils.collection.path._
import javax.xml.transform.URIResolver
import scales.xml.trax.ScalesResult
import br.gov.lexml.xloom.model.XNode
import br.gov.lexml.xloom.model.XElement
import javax.xml.transform.TransformerFactory
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.Document
import scala.annotation.tailrec
import java.net.URI
import javax.xml.transform.stream.StreamSource
import java.net.URL
import org.apache.commons.io.FileUtils
import java.io.File
import java.io.InputStream
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.sax.SAXSource
import javax.xml.transform.dom.DOMResult
import java.io.ByteArrayOutputStream
import javax.xml.transform.stream.StreamResult
import java.io.ByteArrayInputStream
import java.io.StringReader
import grizzled.slf4j.Logging
import scala.language.implicitConversions

case class XLoomResult(doc: Doc) {
  def asSource: Source = doc
  def asSafeSource: Source = {
    new StreamSource(new StringReader(asString(doc)))
  }
}

trait Processor {
  this : Logging =>
  def resolver: URIResolver
  def process(srcXml: XmlTree, base: Option[String]): Doc

  implicit def doc2xloomResult(d: Doc): XLoomResult = XLoomResult(d)

  def process(source: InputSource): XLoomResult = process(loadXml(source), Option(source.getSystemId()))
  def process(s: Source): XLoomResult = {
    val timeStart = System.nanoTime
    val res = s match {
      case ss: StreamSource ⇒ process(ss.getInputStream(), ss.getSystemId())
      case ss: SAXSource ⇒ process(ss.getInputSource())
      case s =>
        val t = TransformerFactory.newInstance.newTransformer
        val sr = ScalesResult()
        t.transform(s, sr)
        process(sr.doc.rootElem)
    }
    val timeEnd = System.nanoTime
    val micros = (timeEnd - timeStart) / 1000
    logger.info("xloom processed in %d microseconds".format(micros))
    res
  }

  def process(is: InputStream, systemID: String): XLoomResult = {
    val isrc = new InputSource(is)
    isrc.setSystemId(systemID)
    process(is)
  }

  def process(uri: URI): XLoomResult = {
    process(resolver.resolve(uri.toASCIIString(), ""))
  }

  def process(url: URL): XLoomResult = {
    process(url.toURI())
  }

  def process(uri: String): XLoomResult = {
    process(URI.create(uri))
  }
}

class XLoomProcessor(val resolver: URIResolver) extends XmlUtils with Processor with Logging {
  object C {
    val Xloom = Namespace("http://www.lexml.gov.br/schema/xloom")
    val Xsl = Namespace("http://www.w3.org/1999/XSL/Transform")
    val Stylesheet = Xsl("stylesheet")
    val Include = Xloom("include")
    val Match = Xloom("match")
    val Replace = Xloom("replace")
    val InsertBefore = Xloom("insertBefore")
    val InsertAfter = Xloom("insertAfter")
    val Clear = Xloom("clear")
    val ReplaceChildren = Xloom("replaceChildren")
    val AddFirst = Xloom("addFirst")
    val AddLast = Xloom("addLast")
    val ClearChildren = Xloom("clearChildren")
    val ClearAttributes = Xloom("clearAttributes")
    val SetAttributes = Xloom("setAttributes")
    val RemoveAttributes = Xloom("removeAttributes")
    val ExternalXsl = Xloom("externalXsl")
    val PrefixId = Xloom("prefixId")
    val ThisEl = Xloom("this")
  }

  val href: AttributeQName = NoNamespaceQName("href")
  val select: AttributeQName = NoNamespaceQName("select")
  val _this: AttributeQName = NoNamespaceQName("this")
  //  val preProcess  : AttributeQName 	= NoNamespaceQName("preProcess")
  val prefix: AttributeQName = NoNamespaceQName("prefix")
  val idAttr: AttributeQName = NoNamespaceQName("id")

  def process(srcXml: XmlTree, base: Option[String] = None): Doc = {
    val topPath = top(srcXml)
    val res = ProcessInstance(base).process(topPath)
    Doc(rootPath(res).tree)
  }

  private[this] case class ProcessInstance(base: Option[String]) {

    type ElemPath = Path[XmlItem, Elem, XCC]

    private[XLoomProcessor] def process(path: ElemPath): ElemPath = {
      val xloomElements = path \\* C.Include
      if (xloomElements.isEmpty) { path }
      else {
        val r = foldPositions(xloomElements)(processInclude)
        r match {
          case Left(p) => process(p)
          case _ => path
        }
      }
    }

    type ElemFoldOp = FoldOperation[XmlItem, Elem, XCC]

    type ElemProc = ElemPath => ElemFoldOp

    @tailrec
    private[this] def extractNamespaces(p: ElemPath, m: Map[String, String] = Map()): Map[String, String] = {
      val m1 = m ++ p.tree.section.namespaces
      p.top match {
        case t: Top[_, _, _] => m1
        case pp: ElemPath => extractNamespaces(pp, m1)
        case _ => sys.error("unexpected at extractNamespaces")
      }
    }

    private[this] def processInclude(includePath: ElemPath): ElemFoldOp = {
      val hrefValue = text((includePath *@ href).head)
      val selectValue = (includePath *@ select).headOption.map(text(_))
      /*    val preProcessValue = (path *@ preProcess)
    						.headOption.map(text(_))
    						.collect({ case "true" ⇒ true })
    						.getOrElse(false) */
      val elem1 = getDocument(hrefValue)
      val inputElements = selectValue match {
        case None => List(elem1)
        case Some(xpathExpression) => {
          import scales.xml.jaxen._
          val namespaces = extractNamespaces(includePath)          
          val xpath = ScalesXPath(xpathExpression, namespaces)
          xpath.xmlPaths(top(elem1)).map(_.tree).toList
        }
      }
      val cmdElems = includePath.\*.toSeq.filter(_.isRight)      

      val finalElements = cmdElems.foldLeft(inputElements) {
        (inputElems, commandPath) =>
          inputElems.flatMap {
            elem =>
              val cmdPathName = name(commandPath)
              processCommand(cmdPathName, commandPath, top(elem))
          }
      }
      Replace(finalElements)
    }

    private[this] def processCommand(
      commandName: QName, commandPath: ElemPath, inputPath: ElemPath): List[XmlTree] = {
      partialProcessCommand.lift((commandName, commandPath, inputPath)).getOrElse {
        logger.warn("xloom: command not supported: " + commandName)
        List(inputPath.tree)
      }
    }

    private[this] val partialProcessCommand: PartialFunction[(QName, ElemPath, ElemPath), List[XmlTree]] = {
      case (C.Stylesheet, commandPath, inputPath) => processStyleSheet(commandPath, inputPath)      
    }

    private[this] def processStyleSheet(commandPath: XmlPath, inputPath: XmlPath): List[XmlTree] = {
      val (commandSource ,inputSource ) = if (
          sys.props.getOrElse("lexml.xloom.serializeBeforeTransform","false").toLowerCase == "true") {
	      val commandString = asString(commandPath.tree)
	      val inputString = asString(inputPath.tree)
	      (new StreamSource(new StringReader(commandString)),
	       new StreamSource(new StringReader(inputString)))  
      } else {
        (commandPath.tree : Source,inputPath.tree : Source)
      }
            
      val tf = TransformerFactory.newInstance().newTransformer(commandSource)
      val sr = ScalesResult()
      tf.transform(inputSource, sr)
      List(sr.doc.rootElem)
    }

    private[this] def getDocument(href: String): XmlTree = {
      readDocument(resolver.resolve(href, base.orNull))
    }
    private[this] def readDocument(s: InputSource): Doc = {
      loadXml(s)
    }
    private[this] def readDocument(s: Source): Doc = {
      val tf = TransformerFactory.newInstance.newTransformer
      val dr = ScalesResult()
      tf.transform(s, dr)
      dr.doc
    }
  }
}
