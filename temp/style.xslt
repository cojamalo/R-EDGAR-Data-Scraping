<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:param name="asPage" />
	<xsl:param name="numberDecimalSeparator" select=" '.' " xml:space="preserve"/>
  <xsl:param name="numberGroupSeparator" select=" ',' " xml:space="preserve"/>
  <xsl:param name="numberGroupSize" select=" '3' "/>
  <xsl:param name="showFlags" />
	<xsl:param name="source" />

	<xsl:param name="otherStandardPrefix"/>
	<xsl:param name="memberDetails">true</xsl:param>
  <xsl:param name="authMode">0</xsl:param>
  <xsl:param name="firstmemberref">false</xsl:param>
  <xsl:decimal-format name="currency" digit="D" />
	<xsl:output method="html" omit-xml-declaration="yes" />
	<xsl:preserve-space elements="label" />

<xsl:variable name="majorversion" select="substring-before(/InstanceReport/Version,'.')"/>
  <xsl:key name="keyElement" match="Row" use="ElementName"/>
  <xsl:key name="keyDimension" match="DimensionInfo" use="concat(dimensionId,Id)"/>
 
  <xsl:template match="/">
	<xsl:choose>
		<xsl:when test="$asPage = 'true'">
			<html>
			<head>
				<title/>
				<link rel="stylesheet" type="text/css" href="report.css"/>
            <script type="text/javascript" src="Show.js">/* Do Not Remove This Comment */</script>
			<script type="text/javascript">
							function toggleNextSibling (e) {
							if (e.nextSibling.style.display=='none') {
							e.nextSibling.style.display='block';
							} else {
							e.nextSibling.style.display='none';
							}
							}</script>
          </head>
			<body>
				
				<!-- GET VIEW -->
				<xsl:apply-templates select="InstanceReport" />

			</body>
			</html>
		</xsl:when>
		<xsl:otherwise>

			<!-- GET VIEW -->
			<xsl:apply-templates select="InstanceReport" />

		</xsl:otherwise>
	</xsl:choose>
</xsl:template>

	<xsl:template name="notify">
		<xsl:param name="pos" />
		<xsl:param name="type" />
		
		<xsl:if test="count(../../Rows/Row[not(FootnoteIndexer='')]) &gt; 1">0|</xsl:if>

    <xsl:if test="$type = '-1' or $type = 0">
			<xsl:if test="not( '' = FootnoteIndexer )"><xsl:value-of select="Id" />|</xsl:if>
		</xsl:if>

		<xsl:if test="$type = '1' or $type = 0">
			<xsl:for-each select="../../Rows/Row/Cells/Cell[ Id = $pos and not( FootnoteIndexer = '' ) ]"><xsl:if test="position() = 1"><xsl:value-of select="Id" />|</xsl:if></xsl:for-each>
		</xsl:if>
			
	</xsl:template>

	<xsl:template match="InstanceReport">
		<!-- if the columns, rows or cells have FootnoteIndexer -->
		<xsl:variable name="hasFootnotes" select="count((Columns/Column | Rows | Rows/Row/Cells/Cell)[not(''=FootnoteIndexer)]) &gt; 0"/>

		<xsl:variable name="anyWithNotes">
			<xsl:if test="$hasFootnotes">|<xsl:for-each select="Columns/Column"><xsl:call-template name="notify"><xsl:with-param name="pos" select="Id" /><xsl:with-param name="type" select="0" /></xsl:call-template></xsl:for-each></xsl:if>
		</xsl:variable>
		
		<xsl:variable name="colsWithNotes">
			<xsl:if test="$hasFootnotes">|<xsl:for-each select="Columns/Column"><xsl:call-template name="notify"><xsl:with-param name="pos" select="Id" /><xsl:with-param name="type" select="1" /></xsl:call-template></xsl:for-each></xsl:if>
		</xsl:variable>

		<xsl:variable name="headsWithNotes">
			<xsl:if test="$hasFootnotes">|<xsl:for-each select="Columns/Column"><xsl:call-template name="notify"><xsl:with-param name="pos" select="Id" /><xsl:with-param name="type" select="-1" /></xsl:call-template></xsl:for-each></xsl:if>
		</xsl:variable>
		
		<xsl:variable name="idx" select="1" />
		<xsl:variable name="isBarChartTable" select="string-length( ../BarChartImageFileName ) = 0" />
		<xsl:variable name="isOuterReport" select=". = /InstanceReport" />
		<xsl:variable name="rounding" select="string-length( RoundingOption ) &gt; 0" />

		<xsl:if test="$isOuterReport">
			<span style="display: none;">v<xsl:value-of select="Version" /></span>
		</xsl:if>

		<xsl:choose>
		<xsl:when test="$isBarChartTable">

			<table class="report" border="0" cellspacing="2">
				<xsl:if test="$isOuterReport">
					<xsl:attribute name="id">
						<xsl:value-of select="generate-id( ReportName )" />
					</xsl:attribute>
				</xsl:if>

				<!-- write the column headers -->
				<xsl:if test="not( HasEmbeddedReports = 'true' )">
					<xsl:call-template name="viewHead">
						<xsl:with-param name="colsWithNotes" select="$anyWithNotes" />
						<xsl:with-param name="hasFootnotes" select="$hasFootnotes" />
						<xsl:with-param name="idx"          select="$idx" />
						<xsl:with-param name="rounding"     select="$rounding" />
					</xsl:call-template>
				</xsl:if>

	<!-- write the table -->
				<xsl:call-template name="viewBody">
					<xsl:with-param name="colsWithNotes" select="$colsWithNotes" />
					<xsl:with-param name="headsWithNotes" select="$headsWithNotes" />
					
					<xsl:with-param name="hasFootnotes" select="$hasFootnotes" />
					<xsl:with-param name="idx"            select="$idx" />
					<xsl:with-param name="rounding"        select="$rounding" />
				</xsl:call-template>

				<!-- if this report is not embedded write the footnotes on the inside -->
				<xsl:if test="$isOuterReport">
					<xsl:call-template name="innerFootnotes">
						<xsl:with-param name="colsWithNotes" select="$anyWithNotes" />
					</xsl:call-template>
				</xsl:if>
			</table>

			<!-- if this report is embedded write the footnotes on the outside -->
			<xsl:if test="not( $isOuterReport )">
				<xsl:call-template name="outerFootnotes">
            <xsl:with-param name="colsWithNotes" select="$anyWithNotes"/>
          </xsl:call-template>
			</xsl:if>			
			
			<!-- write the definition and reference lookup table -->
			<xsl:if test="$isOuterReport">
				<div style="display: none;">
				<xsl:for-each select="//Row[generate-id(.)=generate-id(key('keyElement',ElementName)[1])]">
					<xsl:sort order="ascending" select="ElementName"/>
					<xsl:if test="string-length(ElementName) &gt; 0">
                <xsl:call-template name="authRefData"/>
              </xsl:if>
				</xsl:for-each>
				<xsl:if test="$memberDetails='true' and $majorversion &lt; 3">
              <xsl:for-each select="//Row[ string-length( .//Label[@Id=0]/@Key) &gt; 0 and not(.//Label[@Id=0]/@Key = following::Row/.//Label[@Id=0]/@Key ) ]">
                <xsl:sort order="ascending" select=".//Label[@Id=0]/@Key"/>
                <xsl:call-template name="authRefData">
                  <xsl:with-param name="Name" select="translate(.//Label[@Id=0]/@Key,':','=')"/>
                </xsl:call-template>
              </xsl:for-each>
            </xsl:if>
            <xsl:if test="$memberDetails='true'">
              <xsl:for-each select="//Segment[IsDefaultForEntity != 'true']/DimensionInfo[
                generate-id(.)=generate-id(key('keyDimension',concat(dimensionId,Id))[1])]">
                <xsl:variable name="dim" select="translate(dimensionId,':','_')"/>
                <xsl:variable name="mem" select="translate(Id,':','_')"/>
                <xsl:call-template name="authRefData">
                  <xsl:with-param name="Name" select="concat($dim,'=',$mem)"/>
                </xsl:call-template>
              </xsl:for-each>
            </xsl:if>
          </div>
			</xsl:if>

		</xsl:when>
		<xsl:otherwise>

			<xsl:variable name="localPath"><xsl:call-template name="basename">
				<xsl:with-param name="path" select="../BarChartImageFileName" />
			</xsl:call-template></xsl:variable>

			<img alt="Bar Chart" src="{$source}{$localPath}" />
			
			<!-- if this report is embedded write the footnotes on the outside -->
			<xsl:call-template name="outerFootnotes">
				<xsl:with-param name="colsWithNotes" select="$anyWithNotes" />
			</xsl:call-template>			

		</xsl:otherwise>
		</xsl:choose>
    </xsl:template>
	
	

    <xsl:template name="viewHead">
        <xsl:param name="colsWithNotes" />
        <xsl:param name="hasFootnotes" />
        <xsl:param name="idx" />
        <xsl:param name="rounding" />

          <xsl:variable name="displayLabelColumn" select="string-length( DisplayLabelColumn ) = 0 or DisplayLabelColumn = 'true'" />

	<tr>
		<xsl:variable name="rowSpan" select="1 + ( count( Columns/Column[ Labels/Label[ $idx ][ contains( @Label, ' Ended' ) ] ] ) &gt; 0 )" />
		
		<xsl:if test="$displayLabelColumn">
			<xsl:variable name="hasLabelFootnotes" select="count( Rows/Row[ string-length( FootnoteIndexer ) &gt; 0 ] )" />

			<th class="tl">
				<xsl:attribute name="colspan"><xsl:value-of select="1 + ( $hasLabelFootnotes &gt; 0 )" /></xsl:attribute>
					
				<xsl:choose>
				<xsl:when test="ShowElementNames = 'true'">Label</xsl:when>
				<xsl:otherwise>
					<xsl:attribute name="rowspan"><xsl:value-of select="$rowSpan" /></xsl:attribute>

					<div style="width: 200px;"><strong>
						<xsl:value-of select="ReportName"/>
						<br />
							
						<xsl:if test="$rounding">
							<xsl:value-of select="RoundingOption" />
						</xsl:if>
	
					</strong></div>
				</xsl:otherwise>
				</xsl:choose>
			</th>
		</xsl:if>

		<xsl:if test="ShowElementNames = 'true'">
			<th class="tl">
				<strong>Element</strong>
			</th>
		</xsl:if>

		<xsl:call-template name="promoMgr">
			<xsl:with-param name="colsWithNotes" select="$colsWithNotes" />
			<xsl:with-param name="hasFootnotes" select="$hasFootnotes" />
			<xsl:with-param name="idx"          select="$idx" />
		</xsl:call-template>
	</tr>

	<xsl:if test="string-length( ShowElementNames ) = 0 or ShowElementNames != 'true'">
		<xsl:if test="count( Columns/Column[ Labels/Label[ $idx ][ contains( @Label, ' Ended' ) ] ] ) &gt; 0">
		<tr>
			<xsl:for-each select="Columns/Column[ not( LabelColumn = 'true' )]">

				<xsl:call-template name="header">
					<xsl:with-param name="colsWithNotes" select="$colsWithNotes" />
					<xsl:with-param name="position"     select="Id" />
				</xsl:call-template>

			</xsl:for-each>
		</tr>
		</xsl:if>
	</xsl:if>

    </xsl:template>



    <xsl:template name="promoMgr">
        <xsl:param name="colsWithNotes" />
        <xsl:param name="hasFootnotes" />
        <xsl:param name="idx" />

        <xsl:choose>
		<xsl:when test="ShowElementNames = 'true'">
			<xsl:for-each select="Columns/Column">
				<th class="th">
					<xsl:attribute name="colspan">
						 <xsl:call-template name="colCount">
							<xsl:with-param name="colsWithNotes" select="$colsWithNotes" />
							<xsl:with-param name="hasFootnotes"  select="$hasFootnotes" />
							<xsl:with-param name="idx"           select="$idx" />
							<xsl:with-param name="position"      select="position()" />
							<xsl:with-param name="thisLabel"     select="Labels/Label[$idx]/@Label" />
						</xsl:call-template>
					</xsl:attribute>

					<xsl:text>Value</xsl:text>
				</th>
			</xsl:for-each>
		</xsl:when>
        <xsl:when test="count( Columns/Column[ Labels/Label[ $idx ][ contains( @Label, ' Ended' ) ] ] ) &gt; 0">

            <xsl:for-each select="Columns/Column">
                <xsl:if test="not( Labels/Label[$idx]/@Label = preceding-sibling::Column[1]/Labels/Label[$idx]/@Label )">

					<th class="th">
						<xsl:attribute name="colspan">
							 <xsl:call-template name="colCount">
								<xsl:with-param name="colsWithNotes" select="$colsWithNotes" />
								<xsl:with-param name="hasFootnotes"  select="$hasFootnotes" />
								<xsl:with-param name="idx"           select="$idx" />
								<xsl:with-param name="position"      select="position()" />
								<xsl:with-param name="thisLabel"     select="Labels/Label[$idx]/@Label" />
							</xsl:call-template>
						</xsl:attribute>

						<xsl:if test="count( Labels/Label[$idx][ contains( @Label, ' Ended' ) ] ) &gt; 0">
 							<xsl:value-of select="Labels/Label[$idx]/@Label" />
 						</xsl:if>
					</th>

                </xsl:if>
            </xsl:for-each>

        </xsl:when>
        <xsl:otherwise>

            <xsl:for-each select="Columns/Column">
				<xsl:call-template name="header">
					<xsl:with-param name="colsWithNotes" select="$colsWithNotes" />
					<xsl:with-param name="position" select="Id" />
				</xsl:call-template>
            </xsl:for-each>

        </xsl:otherwise>
        </xsl:choose>
    </xsl:template>



    <xsl:template name="colCount">
        <xsl:param name="colsWithNotes" />
        <xsl:param name="hasFootnotes" />
        <xsl:param name="idx" />
        <xsl:param name="position" />
        <xsl:param name="thisLabel" />

        <xsl:variable name="numCols">
             <xsl:call-template name="countContiguous">
                <xsl:with-param name="idx" select="$idx" />
                <xsl:with-param name="position" select="$position" />
                <xsl:with-param name="thisLabel" select="$thisLabel" />
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="numIndices">
            <xsl:choose>
                <xsl:when test="$hasFootnotes">
                     <xsl:call-template name="countIndices">
                        <xsl:with-param name="colsWithNotes" select="$colsWithNotes" />
                        <xsl:with-param name="numCols"         select="$numCols" />
                        <xsl:with-param name="position"     select="$position" />
                    </xsl:call-template>
                </xsl:when>

                <xsl:otherwise>0</xsl:otherwise>
            </xsl:choose>
        </xsl:variable>

        <xsl:value-of select="$numCols + $numIndices" />

    </xsl:template>



    <xsl:template name="countContiguous">
        <xsl:param name="idx" />
        <xsl:param name="position" />
        <xsl:param name="thisLabel" />

        <xsl:choose>
        <xsl:when test="count( following-sibling::Column[ labelColumn = 'true' or not( Labels/Label[$idx]/@Label = $thisLabel ) ] ) &gt; 0">

            <xsl:for-each select="following-sibling::Column[ labelColumn = 'true' or not( Labels/Label[$idx]/@Label = $thisLabel )  ]">
                <xsl:if test="position() = 1">
                    <xsl:value-of select="count( preceding-sibling::Column[ position() &gt;= $position ] )" />
                </xsl:if>
            </xsl:for-each>

        </xsl:when>
        <xsl:otherwise>

            <xsl:value-of select="count( following-sibling::Column/Labels/Label[$idx][ @Label = $thisLabel ] ) + 1" />

        </xsl:otherwise>
        </xsl:choose>
    </xsl:template>



    <xsl:template name="countIndices">
        <xsl:param name="colsWithNotes" />
        <xsl:param name="numCols" />
        <xsl:param name="position" />

        <xsl:for-each select="../../Rows/Row">
            <xsl:sort order="descending" select="count( Cells/Cell[
                position() &gt;= $position            and
                position() &lt;  $position + $numCols and
                contains( $colsWithNotes, concat( '|', Id, '|' ) )
            ] )"/>
              <xsl:if test="position() = 1">
                <xsl:value-of select="count( Cells/Cell[
                        position() &gt;= $position            and
                        position() &lt;  $position + $numCols and
                        contains( $colsWithNotes, concat( '|', Id, '|' ) )
                ] )"/>
            </xsl:if>
        </xsl:for-each>

    </xsl:template>



    <xsl:template name="header">
		<xsl:param name="colsWithNotes" />
		<xsl:param name="position" />

		<!-- is there a footnote anywhere in this column? -->
		<xsl:variable name="hasFootnoteColumn" select="contains( $colsWithNotes, concat( '|', $position, '|' ) )" />
		
		<!-- if so, is there a footnote on THIS column header? -->
		<xsl:variable name="hasFootnoteHeader" select="$hasFootnoteColumn and string-length( FootnoteIndexer ) &gt; 0" />

		<th class="th">
			<xsl:if test="$hasFootnoteColumn and not( $hasFootnoteHeader )">
				<xsl:attribute name="colspan">2</xsl:attribute>
			</xsl:if>

			<xsl:choose>
			<xsl:when test="../../ShowElementNames = 'true'">Value</xsl:when>
			<xsl:otherwise>

				<xsl:for-each select="Labels/Label[ not( contains( @Label, ' Ended' ) ) ]">
					<div>
						<xsl:value-of select="@Label" /><xsl:if test="not( position() = last() )"><xsl:value-of select="../../LabelSeparator" /></xsl:if>
					</div>
				</xsl:for-each>

			</xsl:otherwise>
			</xsl:choose>
		</th>
		
		<xsl:if test="$hasFootnoteHeader">
		<th class="th">
			<sup><xsl:value-of select="FootnoteIndexer" /></sup>
		</th>
		</xsl:if>
    </xsl:template>



    <xsl:template name="viewBody">
		<xsl:param name="colsWithNotes" />
		<xsl:param name="headsWithNotes" />
		
		<xsl:param name="hasFootnotes" />
		<xsl:param name="idx" />
		<xsl:param name="rounding" />

		<xsl:variable name="labelPosition" />
		<xsl:variable name="showElementNames" select="ShowElementNames = 'true'" />
		<xsl:variable name="displayLabelColumn" select="string-length( DisplayLabelColumn  ) = 0 or DisplayLabelColumn = 'true'" />
		<xsl:variable name="hasLabelFootnotes" select="count( Rows/Row[ string-length( FootnoteIndexer ) &gt; 0 ] )" />
		<xsl:for-each select="Rows/Row">

			<xsl:choose>
			<xsl:when test="IsReportTitle = 'true'"/>
			<xsl:when test="Id = 1 and IsSegmentTitle = 'true' and contains( ReportName, Label )"/>
			<xsl:otherwise>

			<tr>
				<xsl:call-template name="rowStyle" />

				<xsl:variable name="custom">
				<xsl:choose>
                <xsl:when test="IsSegmentTitle='true'"/>
                <xsl:when test="IsAbstractGroupTitle='true'"/>
                <xsl:when test="ElementPrefix='us-gaap_'"/>
                <xsl:when test="ElementPrefix='dei_'"/>
                <xsl:when test="ElementPrefix='invest_'"/>
                <xsl:when test="ElementPrefix='rr_'"/>
                <xsl:when test="ElementPrefix=$otherStandardPrefix"/>
                <xsl:otherwise>custom</xsl:otherwise>
              </xsl:choose>
				</xsl:variable>

				<xsl:if test="$displayLabelColumn">
              <td class="pl {$custom}" style="border-bottom: 0px;" valign="top">
                <xsl:call-template name="authRefLink"/>
              </td>
              <xsl:if test="$hasLabelFootnotes &gt; 0">
                <td class="th" style="border-bottom: 0px;">
                  <sup>
                    <xsl:value-of select="FootnoteIndexer"/>
                  </sup>
                </td>
              </xsl:if>
            </xsl:if>

            <xsl:if test="$showElementNames">
				<td class="th" style="border-bottom: 0px;">
					<xsl:value-of select="ElementName" />
				</td>
				</xsl:if>
				
				<xsl:call-template name="perCell">
					<xsl:with-param name="colsWithNotes" select="$colsWithNotes" />
					<xsl:with-param name="headsWithNotes" select="$headsWithNotes" />
					
					
					<xsl:with-param name="hasFootnotes" select="$hasFootnotes" />
					<xsl:with-param name="labelPosition" select="$labelPosition" />
					<xsl:with-param name="rounding"     select="$rounding" />
				</xsl:call-template>

			</tr>

			</xsl:otherwise>
			</xsl:choose>

			<xsl:if test="IsSubReportEnd = 'true' and not( position() = last() ) and not( following-sibling::Row[1]/IsSegmentTitle = 'true' )">
			<xsl:call-template name="reportBreak">
				<xsl:with-param name="colsWithNotes" select="$colsWithNotes" />
				<xsl:with-param name="labelPosition" select="$labelPosition" />
			</xsl:call-template>
			</xsl:if>
		</xsl:for-each>

    </xsl:template>



    <xsl:template name="reportBreak">
        <xsl:param name="colsWithNotes" />
        <xsl:param name="labelPosition" />

		<xsl:variable name="idxs" select="count( Columns/Column[ contains( $colsWithNotes, concat( '|', Id, '|' ) ) ] )" />
		<xsl:variable name="cols" select="count( Columns/Column )" />

		<tr>
			<td colspan="{ $cols + $idxs + 1 }" style="height: 1em;"><hr /></td>
		</tr>
    </xsl:template>



    <xsl:template name="rowStyle">
				<xsl:attribute name="class">
						<xsl:choose>
								<xsl:when test="IsReportTitle = 'true' or IsSegmentTitle = 'true'">rh</xsl:when>
								<xsl:when test="IsCalendarTitle = 'true'">rc</xsl:when>
								<xsl:when test="((position() + 1) mod 2 = 0)">re<xsl:if test="IsTotalLabel = 'true'">u</xsl:if></xsl:when>
								<xsl:otherwise>ro<xsl:if test="IsTotalLabel = 'true'">u</xsl:if></xsl:otherwise>
						</xsl:choose>
						<xsl:if test="$showFlags = 'true' and @FlagID > 0">Flag<xsl:value-of select="@FlagID" /></xsl:if>
				</xsl:attribute>
    </xsl:template>

    <xsl:template name="authRefLink">
			<xsl:choose>
				<xsl:when test="not(../../../IsTransposed = 'true') and $memberDetails='true' and string-length(.//Label[@Id=0]/@Key)> 0 and $majorversion &lt; 3">
        <a class="a" href="javascript:void(0);" onclick="top.Show.showAR( this, 'defref_{translate(.//Label[@Id=0]/@Key,':','=')}', window );">
          <xsl:choose>
            <xsl:when test="IsAbstractGroupTitle = 'true'">
              <strong>
                <xsl:value-of select="normalize-space( Label )"/>
              </strong>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="normalize-space( Label )"/>
            </xsl:otherwise>
          </xsl:choose>
        </a>
      </xsl:when>
      <xsl:when test="not(../../../IsTransposed = 'true') and $memberDetails='true' and $majorversion &gt; 2 and string-length( ElementName ) = 0 and $firstmemberref='true'">
        <xsl:variable name="dim" select="(following-sibling::Row[MCU][1]/MCU/contextRef/Segments/Segment[IsDefaultForEntity != 'true'])[position()=1]/DimensionInfo/dimensionId"/>
        <xsl:variable name="mem" select="(following-sibling::Row[MCU][1]/MCU/contextRef/Segments/Segment[IsDefaultForEntity != 'true'])[position()=1]/DimensionInfo/Id"/>
        <a class="a" href="javascript:void(0);" onclick="top.Show.showAR( this, 'defref_{translate(concat($dim,'=',$mem),':','_')}', window );">
          <xsl:choose>
            <xsl:when test="IsAbstractGroupTitle = 'true'">
              <strong>
                <xsl:value-of select="normalize-space( Label )"/>
              </strong>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="normalize-space( Label )"/>
            </xsl:otherwise>
          </xsl:choose>
        </a>
      </xsl:when>
      <xsl:when test="not(../../../IsTransposed = 'true') and $memberDetails='true' and $majorversion &gt; 2 and string-length( ElementName ) = 0">
        <xsl:variable name="dim" select="(following-sibling::Row[MCU][1]/MCU/contextRef/Segments/Segment[IsDefaultForEntity != 'true'])[position()=last()]/DimensionInfo/dimensionId"/>
        <xsl:variable name="mem" select="(following-sibling::Row[MCU][1]/MCU/contextRef/Segments/Segment[IsDefaultForEntity != 'true'])[position()=last()]/DimensionInfo/Id"/>
        <a class="a" href="javascript:void(0);" onclick="top.Show.showAR( this, 'defref_{translate(concat($dim,'=',$mem),':','_')}', window );">
          <xsl:choose>
            <xsl:when test="IsAbstractGroupTitle = 'true'">
              <strong>
                <xsl:value-of select="normalize-space( Label )"/>
              </strong>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="normalize-space( Label )"/>
            </xsl:otherwise>
          </xsl:choose>
        </a>
      </xsl:when>
      <xsl:when test="../../../IsTransposed = 'true' or IsReportTitle = 'true' or string-length( ElementName ) = 0">
					<div class="a">
						<xsl:if test="Level != '0'">
							<xsl:attribute name="style">margin-left: <xsl:value-of select="Level" />em;</xsl:attribute>
						</xsl:if>

						<xsl:choose>
						<xsl:when test="IsAbstractGroupTitle = 'true'">
							<strong>
                <xsl:value-of select="normalize-space( Label )"/>
              </strong>
						</xsl:when>


						<xsl:otherwise>				
							<xsl:value-of select="normalize-space( Label )" />
						</xsl:otherwise>
						</xsl:choose>

					 </div>
				</xsl:when>
				<xsl:otherwise>
					<a class="a" href="javascript:void(0);" onclick="top.Show.showAR( this, 'defref_{ElementName}', window );">

						