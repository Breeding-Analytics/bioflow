---
title: "Single Trial Analysis Report"
author: "Contact:<a href = 'https://github.com/Breeding-Analytics/bioflow' target = '_blank'>Breeding Analytics Team, OneCGIAR</a> breedinganalytics@cgiar.org"
date: "August 13, 2024"  
output: html_document
params:
  toDownload: FALSE
---








### Objectives of Single-Trial Analysis

The objective of this dashboard is to help scientist to understand the following points:

1. Overall number of entries and environments included in the single trial analysis (input)

2. High-level summary statistics of the phenotypic information included (input)

3. Observed spatial variation in the different environments when coordinates of the field exist (input)

4. Genetic variance and other genetic parameters observed in the different environments for the different traits (output)

5. Individual adjusted means for each trait by environment combination (output)

6. Phenotypic correlation between environments for the traits present (output)

Understanding these data features should allow the scientist to identify trait by environments combinations that have enough genetic signal and take the decision of which to include in the multi-trial analysis. It should also allow the scientist to assess the quality of the trials conducted and take corrective measures (e.g., change service providers, improve practices, etc.).  




No coordinates available. Skipping planting map.

### Entries and traits by environment table

The following table allows to see how many locations had data for the different traits. You may want to review if the phenotyping capacity can deal with the complexity of the trait (e.g., genotype by environment interaction) or if more resources should be deployed. Also you may want to check if you collected information from all the trials conducted.

<p>&nbsp;</p>

<div style="border: 1px solid #ddd; padding: 5px; overflow-x: scroll; width:100%; "><table class="table table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> environment </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> ACRW </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> CYTHAAJ </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> TRW </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> SHI </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> BYTHAAJ </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> DMRYAJ </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> malt </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> BIOM </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> VW </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> VPP </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> BYTHA </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> gluc </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> TRWD </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> YPSP </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> NOCR </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> CYTHA </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> DM </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> mg </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> NCRPSP </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> zn </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> FYTHAAJ </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> RYTHA </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> DMRY </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> FYTHA </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> RYTHAAJ </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> YPP </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> fe </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> NOPH </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> prot </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> VPSP </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> NCRPP </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> CRW </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> STAR </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> NCRW </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> NOPS </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> RF </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> ca </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> sucr </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> HI </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> fruc </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> bc </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> tc </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> Number of entries </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Barranca </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
</tbody>
</table></div>

<p>&nbsp;</p>

### Summary statistics

The following table allows you to verify some quality metrics (KPIs) for the different trait by environment combinations.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitSta-label" for="reportBuilder_1-traitSta">Trait to filter:</label>
<div>
<select id="reportBuilder_1-traitSta" class="shiny-input-select"><option value="ACRW" selected>ACRW</option>
<option value="BYTHAAJ">BYTHAAJ</option>
<option value="BIOM">BIOM</option>
<option value="BYTHA">BYTHA</option>
<option value="CYTHAAJ">CYTHAAJ</option>
<option value="CYTHA">CYTHA</option>
<option value="ca">ca</option>
<option value="fruc">fruc</option>
<option value="gluc">gluc</option>
<option value="fe">fe</option>
<option value="mg">mg</option>
<option value="malt">malt</option>
<option value="prot">prot</option>
<option value="bc">bc</option>
<option value="STAR">STAR</option>
<option value="sucr">sucr</option>
<option value="tc">tc</option>
<option value="zn">zn</option>
<option value="HI">HI</option>
<option value="NOCR">NOCR</option>
<option value="NCRPP">NCRPP</option>
<option value="NCRPSP">NCRPSP</option>
<option value="NOPH">NOPH</option>
<option value="NOPS">NOPS</option>
<option value="RF">RF</option>
<option value="DM">DM</option>
<option value="DMRYAJ">DMRYAJ</option>
<option value="DMRY">DMRY</option>
<option value="YPP">YPP</option>
<option value="YPSP">YPSP</option>
<option value="SHI">SHI</option>
<option value="FYTHAAJ">FYTHAAJ</option>
<option value="FYTHA">FYTHA</option>
<option value="TRWD">TRWD</option>
<option value="TRW">TRW</option>
<option value="RYTHAAJ">RYTHAAJ</option>
<option value="RYTHA">RYTHA</option>
<option value="VPP">VPP</option>
<option value="CRW">CRW</option>
<option value="VPSP">VPSP</option>
<option value="NCRW">NCRW</option>
<option value="VW">VW</option></select>
<script type="application/json" data-for="reportBuilder_1-traitSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="reportBuilder_1-out36f4ec91d5a8c66b" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Field view

The following heatmaps allow you to inspect the spatial trends in the different fields to take corrective measures in the next season and understand why some variance components may look the way they look.


<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitStaFieldView-label" for="reportBuilder_1-traitStaFieldView">Trait to filter:</label>
<div>
<select id="reportBuilder_1-traitStaFieldView" class="shiny-input-select"><option value="ACRW" selected>ACRW</option>
<option value="CYTHAAJ">CYTHAAJ</option>
<option value="TRW">TRW</option>
<option value="SHI">SHI</option>
<option value="BYTHAAJ">BYTHAAJ</option>
<option value="DMRYAJ">DMRYAJ</option>
<option value="malt">malt</option>
<option value="BIOM">BIOM</option>
<option value="VW">VW</option>
<option value="VPP">VPP</option>
<option value="BYTHA">BYTHA</option>
<option value="gluc">gluc</option>
<option value="TRWD">TRWD</option>
<option value="YPSP">YPSP</option>
<option value="NOCR">NOCR</option>
<option value="CYTHA">CYTHA</option>
<option value="DM">DM</option>
<option value="mg">mg</option>
<option value="NCRPSP">NCRPSP</option>
<option value="zn">zn</option>
<option value="FYTHAAJ">FYTHAAJ</option>
<option value="RYTHA">RYTHA</option>
<option value="DMRY">DMRY</option>
<option value="FYTHA">FYTHA</option>
<option value="RYTHAAJ">RYTHAAJ</option>
<option value="YPP">YPP</option>
<option value="fe">fe</option>
<option value="NOPH">NOPH</option>
<option value="prot">prot</option>
<option value="VPSP">VPSP</option>
<option value="NCRPP">NCRPP</option>
<option value="CRW">CRW</option>
<option value="STAR">STAR</option>
<option value="NCRW">NCRW</option>
<option value="NOPS">NOPS</option>
<option value="RF">RF</option>
<option value="ca">ca</option>
<option value="sucr">sucr</option>
<option value="HI">HI</option>
<option value="fruc">fruc</option>
<option value="bc">bc</option>
<option value="tc">tc</option></select>
<script type="application/json" data-for="reportBuilder_1-traitStaFieldView" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-out80462a7db6181d19" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Output parameters 

<p>&nbsp;</p>

This barplot allows you to see the variance components values and ratios for the trait by environment combinations and identify good quality trials.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitSta0-label" for="reportBuilder_1-traitSta0">Trait to filter:</label>
<div>
<select id="reportBuilder_1-traitSta0" class="shiny-input-select"><option value="ACRW" selected>ACRW</option>
<option value="BYTHAAJ">BYTHAAJ</option>
<option value="BIOM">BIOM</option>
<option value="BYTHA">BYTHA</option>
<option value="CYTHAAJ">CYTHAAJ</option>
<option value="CYTHA">CYTHA</option>
<option value="ca">ca</option>
<option value="fruc">fruc</option>
<option value="gluc">gluc</option>
<option value="fe">fe</option>
<option value="mg">mg</option>
<option value="malt">malt</option>
<option value="prot">prot</option>
<option value="bc">bc</option>
<option value="STAR">STAR</option>
<option value="sucr">sucr</option>
<option value="tc">tc</option>
<option value="zn">zn</option>
<option value="HI">HI</option>
<option value="NOCR">NOCR</option>
<option value="NCRPP">NCRPP</option>
<option value="NCRPSP">NCRPSP</option>
<option value="NOPH">NOPH</option>
<option value="NOPS">NOPS</option>
<option value="RF">RF</option>
<option value="DM">DM</option>
<option value="DMRYAJ">DMRYAJ</option>
<option value="DMRY">DMRY</option>
<option value="YPP">YPP</option>
<option value="YPSP">YPSP</option>
<option value="SHI">SHI</option>
<option value="FYTHAAJ">FYTHAAJ</option>
<option value="FYTHA">FYTHA</option>
<option value="TRWD">TRWD</option>
<option value="TRW">TRW</option>
<option value="RYTHAAJ">RYTHAAJ</option>
<option value="RYTHA">RYTHA</option>
<option value="VPP">VPP</option>
<option value="CRW">CRW</option>
<option value="VPSP">VPSP</option>
<option value="NCRW">NCRW</option>
<option value="VW">VW</option></select>
<script type="application/json" data-for="reportBuilder_1-traitSta0" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-outbfe4cb015d1ddacb" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

<!--html_preserve--><div class="shiny-input-panel">
<div class="shiny-flow-layout">
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-parameterMetrics-label" for="reportBuilder_1-parameterMetrics">Parameter to filter:</label>
<div>
<select id="reportBuilder_1-parameterMetrics" class="shiny-input-select"><option value="plotH2" selected>plotH2</option>
<option value="CV" selected>CV</option>
<option value="r2" selected>r2</option>
<option value="V_designation" selected>V_designation</option>
<option value="V_rowF" selected>V_rowF</option>
<option value="V_colF" selected>V_colF</option>
<option value="V_repF" selected>V_repF</option>
<option value="V_s(row, col)" selected>V_s(row, col)</option>
<option value="V_residual" selected>V_residual</option>
<option value="mean" selected>mean</option></select>
<script type="application/json" data-for="reportBuilder_1-parameterMetrics" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-parameterMetricsBy-label" for="reportBuilder_1-parameterMetricsBy">x-axis</label>
<div>
<select id="reportBuilder_1-parameterMetricsBy" class="shiny-input-select"><option value="environment" selected>environment</option>
<option value="trait">trait</option></select>
<script type="application/json" data-for="reportBuilder_1-parameterMetricsBy" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
</div>
</div><!--/html_preserve-->
<p>&nbsp;</p>

The following barplot is designed to provide a high-level view of estimated parameters such as reliability, heritabiliy, coefficient of variation and others.

<p>&nbsp;</p>

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-out96368973c6b0ffa2" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Predictions

The adjusted means in the following visuualizations are the result of fitting a experimental-design agnostic mixed model where everything that can be fitted will be fitted in order to remove as much spatial noise as possible. That means that if a trial has block and incomplete block information both will be fitted. If the trial has also row and column information it will also be fitted together with a spatial kernel (Rodriguez-Alvarez et al., 2018). These table of adjusted means will be used as input information for the multi-trial analysis. We recommend you to don't take any selection decision at this point and wait until the multi-trial analysis is fitted.

The following table allows you to check the trait by environment adjusted means for the different individuals in wide format.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-envSta-label" for="reportBuilder_1-envSta">Environment to filter:</label>
<div>
<select id="reportBuilder_1-envSta" class="shiny-input-select"><option value="Barranca" selected>Barranca</option></select>
<script type="application/json" data-for="reportBuilder_1-envSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="reportBuilder_1-outbda387f14a753ecb" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

The following boxplot allows you to see the distribution of predicted values by trait (y-axis) in the different environments to double check that everything looks OK.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitStaBox-label" for="reportBuilder_1-traitStaBox">Trait to filter:</label>
<div>
<select id="reportBuilder_1-traitStaBox" class="shiny-input-select"><option value="ACRW" selected>ACRW</option>
<option value="BYTHAAJ" selected>BYTHAAJ</option>
<option value="BIOM" selected>BIOM</option>
<option value="BYTHA" selected>BYTHA</option>
<option value="CYTHAAJ" selected>CYTHAAJ</option>
<option value="CYTHA" selected>CYTHA</option>
<option value="ca" selected>ca</option>
<option value="fruc" selected>fruc</option>
<option value="gluc" selected>gluc</option>
<option value="fe" selected>fe</option>
<option value="mg" selected>mg</option>
<option value="malt" selected>malt</option>
<option value="prot" selected>prot</option>
<option value="bc" selected>bc</option>
<option value="STAR" selected>STAR</option>
<option value="sucr" selected>sucr</option>
<option value="tc" selected>tc</option>
<option value="zn" selected>zn</option>
<option value="HI" selected>HI</option>
<option value="NOCR" selected>NOCR</option>
<option value="NCRPP" selected>NCRPP</option>
<option value="NCRPSP" selected>NCRPSP</option>
<option value="NOPH" selected>NOPH</option>
<option value="NOPS" selected>NOPS</option>
<option value="RF" selected>RF</option>
<option value="DM" selected>DM</option>
<option value="DMRYAJ" selected>DMRYAJ</option>
<option value="DMRY" selected>DMRY</option>
<option value="YPP" selected>YPP</option>
<option value="YPSP" selected>YPSP</option>
<option value="SHI" selected>SHI</option>
<option value="FYTHAAJ" selected>FYTHAAJ</option>
<option value="FYTHA" selected>FYTHA</option>
<option value="TRWD" selected>TRWD</option>
<option value="TRW" selected>TRW</option>
<option value="RYTHAAJ" selected>RYTHAAJ</option>
<option value="RYTHA" selected>RYTHA</option>
<option value="VPP" selected>VPP</option>
<option value="CRW" selected>CRW</option>
<option value="VPSP" selected>VPSP</option>
<option value="NCRW" selected>NCRW</option>
<option value="VW" selected>VW</option></select>
<script type="application/json" data-for="reportBuilder_1-traitStaBox" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-out44d9f5ebc1a3a2ae" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Per-environment merit estimates of top entries

In the following plot you can observe the comparison between the top 30 entries from each entry type category for the different traits. If a category has less than a 30 entries all individuals are displayed. This should allow you to identify the top entries in each environment. We would NOT recommend you to use this for selection of parents or products. Wait until you have the results of the multi-trial analysis and selection indices.

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitStaComp-label" for="reportBuilder_1-traitStaComp">Trait to filter:</label>
<div>
<select id="reportBuilder_1-traitStaComp" class="shiny-input-select"><option value="ACRW" selected>ACRW</option>
<option value="BYTHAAJ" selected>BYTHAAJ</option>
<option value="BIOM" selected>BIOM</option>
<option value="BYTHA" selected>BYTHA</option>
<option value="CYTHAAJ" selected>CYTHAAJ</option>
<option value="CYTHA" selected>CYTHA</option>
<option value="ca" selected>ca</option>
<option value="fruc" selected>fruc</option>
<option value="gluc" selected>gluc</option>
<option value="fe" selected>fe</option>
<option value="mg" selected>mg</option>
<option value="malt" selected>malt</option>
<option value="prot" selected>prot</option>
<option value="bc" selected>bc</option>
<option value="STAR" selected>STAR</option>
<option value="sucr" selected>sucr</option>
<option value="tc" selected>tc</option>
<option value="zn" selected>zn</option>
<option value="HI" selected>HI</option>
<option value="NOCR" selected>NOCR</option>
<option value="NCRPP" selected>NCRPP</option>
<option value="NCRPSP" selected>NCRPSP</option>
<option value="NOPH" selected>NOPH</option>
<option value="NOPS" selected>NOPS</option>
<option value="RF" selected>RF</option>
<option value="DM" selected>DM</option>
<option value="DMRYAJ" selected>DMRYAJ</option>
<option value="DMRY" selected>DMRY</option>
<option value="YPP" selected>YPP</option>
<option value="YPSP" selected>YPSP</option>
<option value="SHI" selected>SHI</option>
<option value="FYTHAAJ" selected>FYTHAAJ</option>
<option value="FYTHA" selected>FYTHA</option>
<option value="TRWD" selected>TRWD</option>
<option value="TRW" selected>TRW</option>
<option value="RYTHAAJ" selected>RYTHAAJ</option>
<option value="RYTHA" selected>RYTHA</option>
<option value="VPP" selected>VPP</option>
<option value="CRW" selected>CRW</option>
<option value="VPSP" selected>VPSP</option>
<option value="NCRW" selected>NCRW</option>
<option value="VW" selected>VW</option></select>
<script type="application/json" data-for="reportBuilder_1-traitStaComp" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-out49bbba508088d04b" style="width:100%;height:400px;"></div><!--/html_preserve-->


### Correlation between environments

The following plot aims to show the correlation between BLUEs or BLUPs (depending on the parameter settings) among the different environments for the traits available in order to identify if there is one or more environments that do not align with the target population of environments (i.e., negatively correlated with the main cluster across most environments). You may want to exclude such environments from the multi-trial analysis (MTA) to ensure that selected entries in the MTA achieve genetic gain in the main cluster of environments.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitStaCor-label" for="reportBuilder_1-traitStaCor">Trait to filter:</label>
<div>
<select id="reportBuilder_1-traitStaCor" class="shiny-input-select"><option value="ACRW" selected>ACRW</option>
<option value="BYTHAAJ" selected>BYTHAAJ</option>
<option value="BIOM" selected>BIOM</option>
<option value="BYTHA" selected>BYTHA</option>
<option value="CYTHAAJ" selected>CYTHAAJ</option>
<option value="CYTHA" selected>CYTHA</option>
<option value="ca" selected>ca</option>
<option value="fruc" selected>fruc</option>
<option value="gluc" selected>gluc</option>
<option value="fe" selected>fe</option>
<option value="mg" selected>mg</option>
<option value="malt" selected>malt</option>
<option value="prot" selected>prot</option>
<option value="bc" selected>bc</option>
<option value="STAR" selected>STAR</option>
<option value="sucr" selected>sucr</option>
<option value="tc" selected>tc</option>
<option value="zn" selected>zn</option>
<option value="HI" selected>HI</option>
<option value="NOCR" selected>NOCR</option>
<option value="NCRPP" selected>NCRPP</option>
<option value="NCRPSP" selected>NCRPSP</option>
<option value="NOPH" selected>NOPH</option>
<option value="NOPS" selected>NOPS</option>
<option value="RF" selected>RF</option>
<option value="DM" selected>DM</option>
<option value="DMRYAJ" selected>DMRYAJ</option>
<option value="DMRY" selected>DMRY</option>
<option value="YPP" selected>YPP</option>
<option value="YPSP" selected>YPSP</option>
<option value="SHI" selected>SHI</option>
<option value="FYTHAAJ" selected>FYTHAAJ</option>
<option value="FYTHA" selected>FYTHA</option>
<option value="TRWD" selected>TRWD</option>
<option value="TRW" selected>TRW</option>
<option value="RYTHAAJ" selected>RYTHAAJ</option>
<option value="RYTHA" selected>RYTHA</option>
<option value="VPP" selected>VPP</option>
<option value="CRW" selected>CRW</option>
<option value="VPSP" selected>VPSP</option>
<option value="NCRW" selected>NCRW</option>
<option value="VW" selected>VW</option></select>
<script type="application/json" data-for="reportBuilder_1-traitStaCor" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-out38838cc04515606c" style="width:100%;height:400px;"></div><!--/html_preserve-->


### References of methods used

Velazco, J. G., Rodriguez-Alvarez, M. X., Boer, M. P., Jordan, D. R., Eilers, P. H., Malosetti, M., & Van Eeuwijk, F. A. (2017). Modelling spatial trends in sorghum breeding field trials using a two-dimensional P-spline mixed model. Theoretical and Applied Genetics, 130, 1375-1392.

Rodriguez-Alvarez, M. X., Boer, M. P., van Eeuwijk, F. A., & Eilers, P. H. (2018). Correcting for spatial heterogeneity in plant breeding experiments with P-splines. Spatial Statistics, 23, 52-71.

R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

Boer M, van Rossum B (2022). LMMsolver: Linear Mixed Model Solver. R package version 1.0.4.9000.

<p>&nbsp;</p>

