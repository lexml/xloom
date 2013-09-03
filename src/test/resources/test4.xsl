<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:madoc="http://www.lexml.gov.br/madoc">
	<xsl:template match="Parlamentar">
		<madoc:Option>
			<xsl:attribute name="display"><xsl:value-of
				select="./NomeParlamentar/text()" /></xsl:attribute>
			<xsl:attribute name="value"><xsl:value-of
				select="./CodigoParlamentar/text()" /></xsl:attribute>
		</madoc:Option>
	</xsl:template>
	<xsl:template match="/">
			<xsl:apply-templates select="//Parlamentar"/>
	</xsl:template>
</xsl:stylesheet> 
