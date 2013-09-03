package br.gov.lexml.xloom.processor

import javax.xml.transform.URIResolver
import java.net.URI
import javax.xml.transform.stream.StreamSource
import grizzled.slf4j.Logging



class DefaultURIResolver extends URIResolver with Logging {
	override def resolve(href : String, base : String) = {
	  logger.info("resolve: href = " + href + ", base = " + base)
	  val uri1 = new URI(href)
	  val uri2 = if(!uri1.isAbsolute && base != null && !base.isEmpty) {
	    new URI(base).resolve(uri1) } else { uri1 }
	  logger.info("uri2 = "+ uri2.toASCIIString())
	  new StreamSource(uri2.toASCIIString())	  
	}
}