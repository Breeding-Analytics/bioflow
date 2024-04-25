---
title: "Single Trial Analysis Report"
author: "Contact:<a href = 'https://github.com/Breeding-Analytics/bioflow' target = '_blank'>Breeding Analytics Team, OneCGIAR</a> breedinganalytics@cgiar.org"
date: "April 25, 2024"  
output: html_document
params:
  toDownload: FALSE
---








### Objectives

The objective of this dashboard is to help scientist to understand the following points:

1. Overall number of entries and environments included in the single trial analysis

2. High-level summary statistics of the phenotypic information included

3. Genetic variance and other genetic parameters observed in the different environments for the different traits

4. Observed spatial variation in the different environments when coordinates of the field exist

5. Individual adjusted means for each trait by environment combination

6. Phenotypic correlation between environments for the traits present

Understanding these data features should allow the scientist to identify trait by environments combinations that have enough genetic signal and take the decision of which to include in the multi-trial analysis. It should also allow the scientist to assess the quality of the trials conducted and take corrective measures (e.g., change service providers, improve practices, etc.).  

### Entries and traits by environment table

The following table allows to see how many locations had data for the different traits. You may want to review if the phenotyping capacity can deal with the complexity of the trait (e.g., genotype by environment interaction) or if more resources should be deployed. Also you may want to check if you collected information from all the trials conducted.

<p>&nbsp;</p>

<div style="border: 1px solid #ddd; padding: 5px; overflow-x: scroll; width:100%; "><table class="table table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> environment </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> YLDTONHA </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> HT_AVG </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> FLW50 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> YLD_TON </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> ZINC </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> PERCENT_HEAD_RICE </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> Number of entries </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> TMeLS-R (Drt)_Stage 1_2022_Wet_IN_CT_RP_IGKV </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 235 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TMeLS-R (Drt)_Stage 1_2022_Wet_IN_JH_RN_BAU </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 235 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TMeLS-R (Drt)_Stage 1_2022_Wet_IN_TR_WT_ICAR </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 235 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TMeLS-R (Drt)_Stage 1_2022_Wet_IN_UP_PY_SHUATS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 235 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TMeLS-R (Drt)_Stage 1_2022_Wet_IN_WB_PU_ZDRPRS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 234 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TMeLS-R_Stage 1_2023_Wet_IN_CT_RP_IGKV </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 188 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TMeLS-R_Stage 1_2023_Wet_IN_JH_HZ_CRURRS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 188 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TMeLS-R_Stage 1_2023_Wet_IN_JH_RN_BAU </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 187 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TMeLS-R_Stage 1_2023_Wet_IN_UP_PY_SHUATS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 188 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TMeLS-R_Stage 1_2023_Wet_IN_WB_PU_ZDRPRS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 188 </td>
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
<select id="reportBuilder_1-traitSta" class="shiny-input-select"><option value="YLD_TON" selected>YLD_TON</option>
<option value="YLDTONHA">YLDTONHA</option>
<option value="FLW50">FLW50</option>
<option value="HT_AVG">HT_AVG</option>
<option value="ZINC">ZINC</option>
<option value="PERCENT_HEAD_RICE">PERCENT_HEAD_RICE</option></select>
<script type="application/json" data-for="reportBuilder_1-traitSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="reportBuilder_1-outb9fe21f5cdb81cba" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

This barplot allows you to see the variance components values and ratios for the trait by environment combinations and identify good quality trials.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitSta0-label" for="reportBuilder_1-traitSta0">Trait to filter:</label>
<div>
<select id="reportBuilder_1-traitSta0" class="shiny-input-select"><option value="YLD_TON" selected>YLD_TON</option>
<option value="YLDTONHA">YLDTONHA</option>
<option value="FLW50">FLW50</option>
<option value="HT_AVG">HT_AVG</option>
<option value="ZINC">ZINC</option>
<option value="PERCENT_HEAD_RICE">PERCENT_HEAD_RICE</option></select>
<script type="application/json" data-for="reportBuilder_1-traitSta0" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-oute360007972d4d569" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-parameterMetrics-label" for="reportBuilder_1-parameterMetrics">Parameter to filter:</label>
<div>
<select id="reportBuilder_1-parameterMetrics" class="shiny-input-select"><option value="plotH2" selected>plotH2</option>
<option value="CV" selected>CV</option>
<option value="r2" selected>r2</option>
<option value="mean" selected>mean</option></select>
<script type="application/json" data-for="reportBuilder_1-parameterMetrics" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->
<p>&nbsp;</p>

The following barplot is designed to provide a high-level view of estimated parameters such as reliability, heritabiliy, coefficient of variation and others.

<p>&nbsp;</p>

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-out4ea0d86b80f461b6" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Field view

The following heatmaps allow you to inspect the spatial trends in the different fields to take corrective measures in the next season and understand why some variance components may look the way they look.


<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitStaFieldView-label" for="reportBuilder_1-traitStaFieldView">Trait to filter:</label>
<div>
<select id="reportBuilder_1-traitStaFieldView" class="shiny-input-select"><option value="YLDTONHA" selected>YLDTONHA</option>
<option value="HT_AVG">HT_AVG</option>
<option value="FLW50">FLW50</option>
<option value="YLD_TON">YLD_TON</option>
<option value="ZINC">ZINC</option>
<option value="PERCENT_HEAD_RICE">PERCENT_HEAD_RICE</option></select>
<script type="application/json" data-for="reportBuilder_1-traitStaFieldView" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-outb660f11622756f59" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Predictions

The adjusted means in the following visuualizations are the result of fitting a experimental-design agnostic mixed model where everything that can be fitted will be fitted in order to remove as much spatial noise as possible. That means that if a trial has block and incomplete block information both will be fitted. If the trial has also row and column information it will also be fitted together with a spatial kernel (Rodriguez-Alvarez et al., 2018). These table of adjusted means will be used as input information for the multi-trial analysis. We recommend you to don't take any selection decision at this point and wait until the multi-trial analysis is fitted.

The following table allows you to check the trait by environment adjusted means for the different individuals in wide format.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-envSta-label" for="reportBuilder_1-envSta">Environment to filter:</label>
<div>
<select id="reportBuilder_1-envSta" class="shiny-input-select"><option value="TMeLS-R_Stage 1_2023_Wet_IN_JH_HZ_CRURRS" selected>TMeLS-R_Stage 1_2023_Wet_IN_JH_HZ_CRURRS</option>
<option value="TMeLS-R_Stage 1_2023_Wet_IN_JH_RN_BAU">TMeLS-R_Stage 1_2023_Wet_IN_JH_RN_BAU</option>
<option value="TMeLS-R_Stage 1_2023_Wet_IN_WB_PU_ZDRPRS">TMeLS-R_Stage 1_2023_Wet_IN_WB_PU_ZDRPRS</option>
<option value="TMeLS-R (Drt)_Stage 1_2022_Wet_IN_CT_RP_IGKV">TMeLS-R (Drt)_Stage 1_2022_Wet_IN_CT_RP_IGKV</option>
<option value="TMeLS-R (Drt)_Stage 1_2022_Wet_IN_UP_PY_SHUATS">TMeLS-R (Drt)_Stage 1_2022_Wet_IN_UP_PY_SHUATS</option>
<option value="TMeLS-R (Drt)_Stage 1_2022_Wet_IN_JH_RN_BAU">TMeLS-R (Drt)_Stage 1_2022_Wet_IN_JH_RN_BAU</option>
<option value="TMeLS-R (Drt)_Stage 1_2022_Wet_IN_TR_WT_ICAR">TMeLS-R (Drt)_Stage 1_2022_Wet_IN_TR_WT_ICAR</option>
<option value="TMeLS-R (Drt)_Stage 1_2022_Wet_IN_WB_PU_ZDRPRS">TMeLS-R (Drt)_Stage 1_2022_Wet_IN_WB_PU_ZDRPRS</option>
<option value="TMeLS-R_Stage 1_2023_Wet_IN_CT_RP_IGKV">TMeLS-R_Stage 1_2023_Wet_IN_CT_RP_IGKV</option>
<option value="TMeLS-R_Stage 1_2023_Wet_IN_UP_PY_SHUATS">TMeLS-R_Stage 1_2023_Wet_IN_UP_PY_SHUATS</option></select>
<script type="application/json" data-for="reportBuilder_1-envSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="reportBuilder_1-out0ca2c1a003b196bd" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

The following boxplot allows you to see the distribution of predicted values by trait (y-axis) in the different environments to double check that everything looks OK.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitStaBox-label" for="reportBuilder_1-traitStaBox">Trait to filter:</label>
<div>
<select id="reportBuilder_1-traitStaBox" class="shiny-input-select"><option value="YLD_TON" selected>YLD_TON</option>
<option value="YLDTONHA" selected>YLDTONHA</option>
<option value="FLW50" selected>FLW50</option>
<option value="HT_AVG" selected>HT_AVG</option>
<option value="ZINC" selected>ZINC</option>
<option value="PERCENT_HEAD_RICE" selected>PERCENT_HEAD_RICE</option></select>
<script type="application/json" data-for="reportBuilder_1-traitStaBox" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-out8ffc76121b7a76b8" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Correlation between environments

The following plot aims to show the correlation between BLUEs or BLUPs (depending on the parameter settings) among the different environments for the traits available in order to identify if there is one or more environments that do not align with the target population of environments (i.e., negatively correlated with the main cluster across most environments). You may want to exclude such environments from the multi-trial analysis (MTA) to ensure that selected entries in the MTA achieve genetic gain in the main cluster of environments.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitStaCor-label" for="reportBuilder_1-traitStaCor">Trait to filter:</label>
<div>
<select id="reportBuilder_1-traitStaCor" class="shiny-input-select"><option value="YLD_TON" selected>YLD_TON</option>
<option value="YLDTONHA" selected>YLDTONHA</option>
<option value="FLW50" selected>FLW50</option>
<option value="HT_AVG" selected>HT_AVG</option>
<option value="ZINC" selected>ZINC</option>
<option value="PERCENT_HEAD_RICE" selected>PERCENT_HEAD_RICE</option></select>
<script type="application/json" data-for="reportBuilder_1-traitStaCor" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-out654a8b2141214d56" style="width:100%;height:400px;"></div><!--/html_preserve-->


### References of methods used

Velazco, J. G., Rodriguez-Alvarez, M. X., Boer, M. P., Jordan, D. R., Eilers, P. H., Malosetti, M., & Van Eeuwijk, F. A. (2017). Modelling spatial trends in sorghum breeding field trials using a two-dimensional P-spline mixed model. Theoretical and Applied Genetics, 130, 1375-1392.

Rodriguez-Alvarez, M. X., Boer, M. P., van Eeuwijk, F. A., & Eilers, P. H. (2018). Correcting for spatial heterogeneity in plant breeding experiments with P-splines. Spatial Statistics, 23, 52-71.

R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

Boer M, van Rossum B (2022). LMMsolver: Linear Mixed Model Solver. R package version 1.0.4.9000.

<p>&nbsp;</p>

