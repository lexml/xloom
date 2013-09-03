
import java.io.BufferedWriter
import java.io.FileWriter
import scala.xml.PrettyPrinter
import org.apache.commons.io.output.StringBuilderWriter
import org.w3c.dom.Document
import org.w3c.dom.Node
import br.gov.lexml.xloom.processor.XLoomProcessor
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import javax.xml.transform.OutputKeys
import javax.xml.transform.TransformerFactory
import br.gov.lexml.xloom.processor.DefaultURIResolver


object RunInclude {

  def main(args: Array[String]): Unit = {
    val dbf = DocumentBuilderFactory.newInstance()
    dbf.setNamespaceAware(true)
    val db = dbf.newDocumentBuilder()
    val doc = db.parse(args(0))
    val proc = new XLoomProcessor(new DefaultURIResolver)
    val node = proc.processIncludeN(doc.getDocumentElement)
    if(args.length > 1) {
    	val bw = new BufferedWriter(new FileWriter(args(1)))
    	bw.write(render(node))
    	bw.close()
    } else {
    	println(render(node))
    }

  }

  val idTransf = {    
    val t = TransformerFactory.newInstance.newTransformer
    t.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
    t.setOutputProperty(OutputKeys.ENCODING, "UTF-8");       
    t
  }

  /*def render(document: Document): String = {
    val format = new OutputFormat(document)
    format.setLineWidth(65)
    format.setIndenting(true)
    format.setIndent(2)
    val out = new java.io.StringWriter()
    val serializer = new XMLSerializer(out, format)
    serializer.serialize(document)
    out.toString()
  }*/
  def render(n: Node): String = {
/*    val di = n.getOwnerDocument().getImplementation().asInstanceOf[DOMImplementationLS]
    val lso = di.createLSOutput()
    val resbs = new ByteArrayOutputStream()
    lso.setByteStream(resbs)
    val lss = di.createLSSerializer()
    lss.write(n,lso)
    new String(resbs.toByteArray()) */
    val ds = new DOMSource(n)
    val sb = new StringBuilderWriter()
    val sr = new StreamResult(sb)
    idTransf.transform(ds, sr)
    sb.close()
    /*val r = XML.loadString(sb.toString)
    new PrettyPrinter(150,2).format(r)*/
    sb.toString()
  }

}