package br.gov.lexml.xloom.model
import scala.ref.WeakReference
import org.w3c.dom.Attr
import org.w3c.dom.CDATASection
import org.w3c.dom.Document
import org.w3c.dom.Element
import org.w3c.dom.Node
import org.w3c.dom.NodeList
import org.w3c.dom.Text
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.xpath.XPathConstants
import javax.xml.xpath.XPathExpression
import javax.xml.transform.Transformer
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.dom.DOMResult
import org.w3c.dom.DocumentFragment
import javax.xml.transform.TransformerFactory
import org.apache.commons.io.output.StringBuilderWriter
import javax.xml.transform.stream.StreamResult
import javax.xml.namespace.NamespaceContext
import scala.language.implicitConversions

abstract sealed class XNode {
  val domNode: Option[Node]
  final val asNode = XNode.xnode2Node(this)
  final def query(xp: XPathExpression) = XNode.query(this, xp)

  final def replaceByXPath(xp: XPathExpression, f: XNode ⇒ List[XNode]) = XNode.replaceByXPath(xp, f)(this)
  final def applyTransformer(tf: Transformer) = XNode.applyTransformer(tf)(this)

  final def topDownUntil(f: PartialFunction[XNode, List[XNode]]) = XNode.topDownUntil(f)(this)
  final def replace(f: PartialFunction[XNode, List[XNode]]) = XNode.replace(f)(this)
  final def replace(nl: List[XNode]) = nl

  final val remove: List[XNode] = Nil
  final def insertBefore(nl: List[XNode]) = this :: nl
  final def addAfter(nl: List[XNode]) = nl :+ this

  def replaceChildren(f: List[XNode] ⇒ List[XNode]): XNode = this
  final def replaceChildren(nl: List[XNode]): XNode = replaceChildren(_ ⇒ nl)
  final lazy val clearChildren = replaceChildren(_ ⇒ Nil)
  final def insertBeforeFirstChild(nl: List[XNode]): XNode = replaceChildren(nll ⇒ nl ++ nll)
  final def addAfterLastChild(nl: List[XNode]): XNode = replaceChildren(nll ⇒ nll ++ nl)

  def modifyAttributes(f: Map[QName, String] ⇒ Map[QName, String]) = this
  final def clearAttribute(name: QName) = modifyAttributes(m ⇒ m - name)
  final lazy val clearAttributes = modifyAttributes(_ ⇒ Map())
  final def addOrReplaceAttribute(name: QName, newValue: String) = modifyAttributes(m ⇒ m + (name -> newValue))
  final def pretty(): String = {
    val sb = new StringBuilderWriter()
    val tf = TransformerFactory.newInstance().newTransformer().transform(new DOMSource(asNode),
      new StreamResult(sb))
    sb.close()
    sb.toString
  }

}

final case class XText(text: String, isCData: Boolean = false, domNode: Option[Node] = None) extends XNode

final case class XElement(name: QName, attributes: Map[QName, String] = Map(), children: List[XNode] = Nil, context: Map[String, String] = Map(), domNode: Option[Node] = None) extends XNode with NamespaceContext {
  override def replaceChildren(f: List[XNode] ⇒ List[XNode]): XNode =
    this copy (children = f(children))
  override def modifyAttributes(f: Map[QName, String] ⇒ Map[QName, String]) =
    this copy (attributes = f(attributes))

  lazy val reverseContext = context.toSeq.map(x ⇒ (x._2, x._1)).toMap

  override def getNamespaceURI(prefix: String): String = reverseContext.get(prefix).orNull
  override def getPrefix(namespaceURI: String): String = context.get(namespaceURI).orNull
  class ListIterator[T](val _l: List[T]) extends java.util.Iterator[T] {
    var l = _l
    override def hasNext() = !l.isEmpty
    override def next() = {
      val x = l.head
      l = l.tail
      x
    }
    override def remove() {
      l = l.tail
    }
  }
  override def getPrefixes(namespaceURI: String) = {
    new ListIterator(context.get(namespaceURI).toList)
  }
}

final case class QName(label: String, namespace: Option[String] = None)

final case class PrefixContext(prefixMap: Map[String, String] = Map(), updates: Set[String] = Set()) {
  val genPrefixPrefix = "p"
  val genPattern = ("^" + genPrefixPrefix + "(\\d+)$").r
  lazy val nextGenPrefix = {
    val nextNum = (0 +: prefixMap.values.collect({ case genPattern(num) ⇒ num.toInt }).toSeq).max + 1
    genPrefixPrefix + nextNum
  }
  def update(context: Map[String, String]) = copy(
    prefixMap = prefixMap ++ context,
    updates = (context.toSet -- prefixMap.toSet).map(_._1))
  def update(ns: String) = if (prefixMap contains ns) {
    (prefixMap(ns), this)
  } else {
    (nextGenPrefix, copy(prefixMap = prefixMap + (ns -> nextGenPrefix), updates = updates + ns))
  }
  lazy val clearUpdates = (updates, copy(updates = Set()))
  def update(el: XElement): PrefixContext = {
    val (_, ctx1) = clearUpdates
    val ctx2 = ctx1.update(el.context)
    val namespaces = (el.name.namespace :: el.attributes.keys.map(_.namespace).toList).flatten
    val ctx3 = namespaces.foldLeft(ctx2) { case (ctx, ns) ⇒ ctx.update(ns)._2 }
    ctx3
  }
}

object XNode {
  def nodeListToList(nl: NodeList) =
    (0 until nl.getLength).map(nl.item _).toList

  def query(n: Node, xp: XPathExpression): List[Node] = {
    xp.evaluate(n, XPathConstants.NODESET) match {
      case nl: NodeList ⇒ nodeListToList(nl)
    }
  }
  def query(n: XNode, xp: XPathExpression): List[XNode] = {
    query(n.asNode, xp) flatMap (node2XNodeWithContext(_))
  }
  implicit def node2XNode(node: Node) = node2XNodeWithContext(node).get

  def node2XNodeWithContext(node: Node, context: Map[String, String] = Map()): Option[XNode] = node match {
    case d: Document ⇒ node2XNodeWithContext(d.getDocumentElement)
    case cd: CDATASection ⇒ Some(XText(cd.getData, true, Some(cd)))
    case t: Text ⇒ Some(XText(t.getData, false, Some(t)))
    case e: Element ⇒ {
      val qname = QName(Option(e.getLocalName).getOrElse(e.getTagName),
        Option(e.getNamespaceURI))
      val attrs = e.getAttributes
      val overrides = (0 until attrs.getLength).map(attrs.item _).collect({
        case a: Attr if a.getName.startsWith("xmlns:") ⇒ {
          val prefix = a.getName.substring("xmlns:".length)
          val uri = a.getValue
          uri -> prefix
        }
      }).toMap
      val attrMap = (0 until attrs.getLength).map(attrs.item _).collect({
        case a: Attr if !a.getName.startsWith("xml:") &&
          !a.getName.startsWith("xmlns:") ⇒ {
          val qname = QName(Option(a.getLocalName).getOrElse(a.getName),
            Option(a.getNamespaceURI))
          qname -> a.getValue
        }
      }).toMap
      val newContext = context ++ overrides
      assert(e.getChildNodes != null)
      val children = nodeListToList(e.getChildNodes).flatMap(node2XNodeWithContext(_, newContext))
      Some(XElement(qname, attrMap, children, newContext, Some(e)))
    }
    case _ ⇒ None
  }

  val empty_pf = new PartialFunction[Any, Nothing] {
    def isDefinedAt(x: Any) = false
    def apply(x: Any): Nothing = sys.error("undefined")
    override def orElse[A1, B1](that: PartialFunction[A1, B1]): PartialFunction[A1, B1] = that
    override def lift = (x: Any) ⇒ None
  }
  def empty[A, B]: PartialFunction[A, B] = empty_pf.asInstanceOf[PartialFunction[A, B]]

  def fold[A](f: XText ⇒ A, h: (XElement, List[A]) ⇒ A,
    stopFunc: PartialFunction[XElement, A] = empty): XNode ⇒ A = {
    def foldit(n: XNode): A = n match {
      case e: XElement ⇒ stopFunc.lift(e) match {
        case Some(x) ⇒ x
        case _ ⇒ h(e, e.children.map(foldit))
      }
      case t: XText ⇒ f(t)
    }
    foldit _
  }

  def topDownUntil(func: PartialFunction[XNode, List[XNode]]) = {
    def f(x: XNode) = List(x)
    def h(el: XElement, r: List[List[XNode]]) = {
      f(el.copy(children = r.flatten))
    }
    fold[List[XNode]](f, h, func)
  }

  def topDownUntil2(func: PartialFunction[XNode, List[XNode]]): XNode ⇒ Option[List[XNode]] = {
    (xnode: XNode) ⇒
      type Res = (List[XNode], Boolean)
      def f(x: XNode): Res = (List(x), false)
      def h(el: XElement, r: List[Res]): Res = {
        val (rl, bl) = r.unzip
        (List(el.copy(children = rl.flatten)), bl.foldLeft(false)(_ || _))
      }
      val pf: PartialFunction[List[XNode], Res] = {
        case x ⇒ (x, true)
      }
      val (r, changed) = fold[Res](f, h, func.andThen(pf))(xnode)
      if (changed) { Some(r) } else { None }
  }

  def replace(func: PartialFunction[XNode, List[XNode]]) = {
    val f = func.orElse({ case x: XNode ⇒ List(x) }: PartialFunction[XNode, List[XNode]])
    def h(el: XElement, r: List[List[XNode]]) = {
      f(el.copy(children = r.flatten))
    }
    fold[List[XNode]](f, h)
  }

  def replaceByDOMNode(domNodes: Set[Node], func: XNode ⇒ List[XNode]): XNode ⇒ List[XNode] = {
    def f(n: XNode) = n.domNode match {
      case Some(domNode) if domNodes contains domNode ⇒ func(n)      
      case _ ⇒ List(n)
    }
    def h(el: XElement, r: List[List[XNode]]) = 
      f(el.copy(children = r.flatten))

    fold[List[XNode]](f, h)
  }

  def replaceByXPath(xp: XPathExpression, func: XNode ⇒ List[XNode]): XNode ⇒ List[XNode] = (xnode: XNode) ⇒ {
    val domNode = xnode.domNode.get
    val domNodes = nodeListToList(xp.evaluate(domNode, XPathConstants.NODESET).asInstanceOf[NodeList]).toSet    
    replaceByDOMNode(domNodes, func)(xnode)
  }

  def applyTransformer(tf: Transformer): XNode ⇒ List[XNode] = (xnode: XNode) ⇒ {
    val dr = new DOMResult()        
    tf.transform(new DOMSource(xnode.asNode.getOwnerDocument), dr)    
    dr.getNode() match {
      case df: DocumentFragment ⇒ nodeListToList(df.getChildNodes) flatMap (node2XNodeWithContext(_))
      case n ⇒ node2XNodeWithContext(n).toList
    }
  }

  def xnode2Node(xnode: XNode): Node = {
    val doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument()
    var prefMap = Map[String, String]()
    var prefCount = 0
    def prefixFor(ns: String) = prefMap.get(ns) match {
      case None ⇒ {
        val prefix = "p" + prefCount
        prefCount = prefCount + 1
        prefMap = prefMap + (ns -> prefix)
        prefix
      }
      case Some(prefix) ⇒ prefix
    }

    def create(n: XNode, prefixContext: PrefixContext = PrefixContext()): Node = n match {
      case t: XText if t.isCData ⇒ doc.createCDATASection(t.text)
      case t: XText ⇒ doc.createTextNode(t.text)
      case e: XElement ⇒ {
        val (updates, nextPrefixContext) = prefixContext.update(e).clearUpdates
        val prefixMap = nextPrefixContext.prefixMap
        val el = e.name match {
          case QName(label, None) ⇒ doc.createElement(label)
          case QName(label, Some(ns)) ⇒ {
            val e = doc.createElementNS(ns, label)
            val prefix = prefixMap(ns)
            e.setPrefix(prefix)
            e
          }
        }
        for { ns ← updates } {
          val prefix = prefixMap(ns)
          val attr = doc.createAttribute("xmlns:" + prefix)
          attr.setValue(ns)
          el.setAttributeNode(attr)
        }
        assert(el.getOwnerDocument == doc)
        for { (qname, value) ← e.attributes } {
          qname match {
            case QName(label, qname) if qname.isEmpty || qname == e.name.namespace ⇒ {
              val attr = doc.createAttribute(label)
              attr.setValue(value)
              el.setAttributeNode(attr)
            }
            case QName(label, Some(ns)) ⇒ {
              val attr = doc.createAttributeNS(ns, label)
              attr.setValue(value)
              //attr.setPrefix(prefixMap(ns))
              el.setAttributeNodeNS(attr)
            }
          }
        }
        e.children.map(create(_, nextPrefixContext)).foreach(el.appendChild)
        el
      }
    }
    val r = create(xnode) match {
      case e: Element ⇒ {
        doc.appendChild(e)
        e
      }
      case n ⇒ {
        val df = doc.createDocumentFragment()
        df.appendChild(n)
        n
      }
    }
    assert(r.getOwnerDocument != null)
    r
  }
}
