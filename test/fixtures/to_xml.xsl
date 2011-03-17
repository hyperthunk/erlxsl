<?xml version="1.0"?>
<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:erlxsl="http://erlxsl.org/schemas/random-transform"
    version="1.0">

    <xsl:output method="xml"
                indent="yes"
                encoding="UTF-8"
                omit-xml-declaration="yes"/>

    <!--  strip off any whitespace  -->
    <xsl:strip-space elements="*"/>

    <!-- get the root element -->
    <xsl:variable name="root" select="/child::node()[count(descendant::node()) &gt; 0]"/>

    <xsl:template match="/">
        <!--<xsl:variable name="root" select="/child::node()[count(descendant::node()) &gt; 0]"/>-->
        <xsl:variable name="title" select="name($root)"/>
        <html>
            <head>
                <style>table tr td {border: 1px solid; border-style: thick;}</style>
                <title>
                    <xsl:value-of select="$title"/>
                </title>
            </head>
            <body>
                <table>
                    <tr>
                        <td>
                            <xsl:value-of select="name($root)"/>
                        </td>
                        <td>
                            <table>
                                <tr>
                                    <td>
                                        <xsl:apply-templates select="$root/*"/>
                                    </td>
                                </tr>
                            </table>
                        </td>
                    </tr>
                </table>
            </body>
        </html>
    </xsl:template>

    <xsl:template name="erlxsl:process-item" match="*[count(child::*) &gt; 0]">
        <tr>
            <td>
                <xsl:value-of select="concat(name(), ': ')"/>
            </td>
            <td>
                <xsl:if test="count(attribute::*) &gt; 0">
                    <table>
                        <xsl:apply-templates select="attribute::*"/>
                    </table>
                </xsl:if>
                <xsl:if test="count(child::*) &gt; 0">
                    <table>
                        <xsl:apply-templates select="child::*"/>
                    </table>
                </xsl:if>
            </td>
        </tr>
    </xsl:template>

    <xsl:template name="erlxsl:process-element" match="*[count(child::*) &lt;= 0]">
        <tr>
            <td>
                <xsl:value-of select="concat(name(), ': ')"/>
            </td>
            <td>
                <table>
                    <tr>
                        <xsl:value-of select="."/>
                    </tr>
                    <xsl:apply-templates select="attribute::*"/>
                </table>
            </td>
        </tr>
    </xsl:template>

    <xsl:template name="erlxsl:process-attributes" match="@*">
        <tr>
            <td>
                <xsl:value-of select="concat(name(), ': ')"/>
            </td>
            <td>
                <i>
                    <xsl:value-of select="."/>
                </i>
            </td>
        </tr>
    </xsl:template>

</xsl:stylesheet>
