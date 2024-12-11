---
title: "Single Trial Analysis Report"
author: "Contact:<a href = 'https://github.com/Breeding-Analytics/bioflow' target = '_blank'>Breeding Analytics Team, OneCGIAR</a> breedinganalytics@cgiar.org"
date: "December 03, 2024"  
output: html_document
params:
  toDownload: FALSE
---








### Objectives of Single-Trial Analysis

The objective of this dashboard is to help scientist to understand the following points:

1. Overall number of designations and environments included in the single trial analysis (input)

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

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="staApp_1-outd1c96541b4e868cc" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Summary statistics

The following table allows you to verify some quality metrics (KPIs) for the different trait by environment combinations.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitSta-label" for="staApp_1-traitSta">Trait to filter:</label>
<div>
<select id="staApp_1-traitSta" class="shiny-input-select"><option value="PH_ALL" selected>PH_ALL</option>
<option value="GYKGPHA">GYKGPHA</option>
<option value="DTF">DTF</option></select>
<script type="application/json" data-for="staApp_1-traitSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="staApp_1-outc63c1d13bad8491a" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Field view

The following heatmaps allow you to inspect the spatial trends in the different fields to take corrective measures in the next season and understand why some variance components may look the way they look.


<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitStaFieldView-label" for="staApp_1-traitStaFieldView">Trait to filter:</label>
<div>
<select id="staApp_1-traitStaFieldView" class="shiny-input-select"><option value="PH_ALL" selected>PH_ALL</option>
<option value="DTF">DTF</option>
<option value="GYKGPHA">GYKGPHA</option></select>
<script type="application/json" data-for="staApp_1-traitStaFieldView" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-out9dc280a3c39b8f83" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Output parameters 

<p>&nbsp;</p>

This barplot allows you to see the variance components values and ratios for the trait by environment combinations and identify good quality trials.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitSta0-label" for="staApp_1-traitSta0">Trait to filter:</label>
<div>
<select id="staApp_1-traitSta0" class="shiny-input-select"><option value="PH_ALL" selected>PH_ALL</option>
<option value="GYKGPHA">GYKGPHA</option>
<option value="DTF">DTF</option></select>
<script type="application/json" data-for="staApp_1-traitSta0" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-out60ddf8e2427717e5" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

<!--html_preserve--><div class="shiny-input-panel">
<div class="shiny-flow-layout">
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-parameterMetrics-label" for="staApp_1-parameterMetrics">Parameter to filter:</label>
<div>
<select id="staApp_1-parameterMetrics" class="shiny-input-select"><option value="plotH2_designation" selected>plotH2_designation</option>
<option value="CV_designation" selected>CV_designation</option>
<option value="r2_designation" selected>r2_designation</option>
<option value="V_designation_designation" selected>V_designation_designation</option>
<option value="V_repF_designation" selected>V_repF_designation</option>
<option value="V_iBlockF_designation" selected>V_iBlockF_designation</option>
<option value="V_residual_designation" selected>V_residual_designation</option>
<option value="mean_designation" selected>mean_designation</option>
<option value="V_rowF_designation" selected>V_rowF_designation</option>
<option value="V_colF_designation" selected>V_colF_designation</option>
<option value="V_s(row, col)_designation" selected>V_s(row, col)_designation</option></select>
<script type="application/json" data-for="staApp_1-parameterMetrics" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-parameterMetricsBy-label" for="staApp_1-parameterMetricsBy">View x-axis by:</label>
<div>
<select id="staApp_1-parameterMetricsBy" class="shiny-input-select"><option value="environment" selected>environment</option>
<option value="trait">trait</option></select>
<script type="application/json" data-for="staApp_1-parameterMetricsBy" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
</div>
</div><!--/html_preserve-->
<p>&nbsp;</p>

The following barplot is designed to provide a high-level view of estimated parameters such as reliability, heritabiliy, coefficient of variation and others.

<p>&nbsp;</p>

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-outad4fc3aad6b091f4" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Modeling parameters

This section aims to provide the modeling table for the analysis in order to keep track of which environments were used in the analysis, what was the final model used for each trait and other potentially important parameters for future reference.

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="staApp_1-out5493abed2e7a1275" style="width:100%;height:auto;"></div><!--/html_preserve-->


### Predictions

The adjusted means in the following visuualizations are the result of fitting a experimental-design agnostic mixed model where everything that can be fitted will be fitted in order to remove as much spatial noise as possible. That means that if a trial has block and incomplete block information both will be fitted. If the trial has also row and column information it will also be fitted together with a spatial kernel (Rodriguez-Alvarez et al., 2018). These table of adjusted means will be used as input information for the multi-trial analysis. We recommend you to don't take any selection decision at this point and wait until the multi-trial analysis is fitted.

The following table allows you to check the trait by environment adjusted means for the different individuals in wide format.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-envSta-label" for="staApp_1-envSta">Environment to filter:</label>
<div>
<select id="staApp_1-envSta" class="shiny-input-select"><option value="2022_WS_Agusan de Norte, Philippines_Agusan de Norte, Philippines-2022" selected>2022_WS_Agusan de Norte, Philippines_Agusan de Norte, Philippines-2022</option>
<option value="2023_DS_Laguna, Philippines_Laguna, Philippines-2023">2023_DS_Laguna, Philippines_Laguna, Philippines-2023</option>
<option value="2022_WS_Albay, Philippines_Albay, Philippines-2022">2022_WS_Albay, Philippines_Albay, Philippines-2022</option>
<option value="2022_WS_Negros, Philippines_Negros, Philippines-2022">2022_WS_Negros, Philippines_Negros, Philippines-2022</option>
<option value="2022_WS_Nueva Ecija, Philippines_Nueva Ecija, Philippines-2022">2022_WS_Nueva Ecija, Philippines_Nueva Ecija, Philippines-2022</option>
<option value="2022_WS_Laguna, Philippines_Laguna, Philippines-2022">2022_WS_Laguna, Philippines_Laguna, Philippines-2022</option>
<option value="2023_DS_Nueva Ecija, Philippines_Nueva Ecija, Philippines-2023">2023_DS_Nueva Ecija, Philippines_Nueva Ecija, Philippines-2023</option>
<option value="2022_WS_Murunkan, Sri Lanka_Murunkan, Sri Lanka stress-2022">2022_WS_Murunkan, Sri Lanka_Murunkan, Sri Lanka stress-2022</option>
<option value="2022_WS_Puttalam, Sri Lanka_Puttalam, Sri Lanka Early salinity non-stress">2022_WS_Puttalam, Sri Lanka_Puttalam, Sri Lanka Early salinity non-stress</option>
<option value="2022_WS_Murunkan, Sri Lanka_Murunkan, Sri Lanka Early drought non-stress">2022_WS_Murunkan, Sri Lanka_Murunkan, Sri Lanka Early drought non-stress</option>
<option value="2022_WS_Isabela, Philippines_Isabela, Philippines-2022">2022_WS_Isabela, Philippines_Isabela, Philippines-2022</option>
<option value="2023_DS_Isabela, Philippines_Isabela, Philippines-2023">2023_DS_Isabela, Philippines_Isabela, Philippines-2023</option>
<option value="2023_DS_Negros, Philippines_Negros, Philippines-2023">2023_DS_Negros, Philippines_Negros, Philippines-2023</option>
<option value="2022_WS_Khajura, Nepal_Khajura, Nepal Early non-stress-2022">2022_WS_Khajura, Nepal_Khajura, Nepal Early non-stress-2022</option>
<option value="2022_WS_Khajura, Nepal_Khajura, Nepal Early stress-2022">2022_WS_Khajura, Nepal_Khajura, Nepal Early stress-2022</option>
<option value="2022_WS_CARDI, Cambodia_CARDI, Cambodia Early drought stress">2022_WS_CARDI, Cambodia_CARDI, Cambodia Early drought stress</option>
<option value="2022_WS_Kushtia, Bangladesh_Kushtia, Bangladesh Early stress-2022">2022_WS_Kushtia, Bangladesh_Kushtia, Bangladesh Early stress-2022</option>
<option value="2022_WS_Kushtia, Bangladesh_Kushtia, Bangladesh stress-2022">2022_WS_Kushtia, Bangladesh_Kushtia, Bangladesh stress-2022</option>
<option value="2022_WS_Kushtia, Bangladesh_Kushtia, Bangladesh non-stress-2022">2022_WS_Kushtia, Bangladesh_Kushtia, Bangladesh non-stress-2022</option>
<option value="2022_WS_CARDI, Cambodia_CARDI, Cambodia Early submergence stress">2022_WS_CARDI, Cambodia_CARDI, Cambodia Early submergence stress</option>
<option value="2022_WS_Mziva_Mziva, Mozambique-2022">2022_WS_Mziva_Mziva, Mozambique-2022</option>
<option value="2022_WS_RCOL_RCOL, Mozambique-2022">2022_WS_RCOL_RCOL, Mozambique-2022</option>
<option value="2022_WS_Can Tho, Vietnam_Can Tho, Vietnam Early stress-2022">2022_WS_Can Tho, Vietnam_Can Tho, Vietnam Early stress-2022</option>
<option value="2022_WS_Mziva_Mziva, Mozambique">2022_WS_Mziva_Mziva, Mozambique</option>
<option value="2022_WS_Dakawa, Morogoro_Dakawa, Morogoro, Tanzania-2022">2022_WS_Dakawa, Morogoro_Dakawa, Morogoro, Tanzania-2022</option>
<option value="2022_WS_Bagamoyo_Bagamoyo, Tanzania-2022">2022_WS_Bagamoyo_Bagamoyo, Tanzania-2022</option>
<option value="2022_WS_Mwea_Mwea, Kenya-2022">2022_WS_Mwea_Mwea, Kenya-2022</option>
<option value="2022_WS_Kyela, Mbeya_Kyela, Mbeya, Tanzania-2022">2022_WS_Kyela, Mbeya_Kyela, Mbeya, Tanzania-2022</option>
<option value="2022_WS_Dakawa, Morogoro_Dakawa, Morogoro, Tanzania">2022_WS_Dakawa, Morogoro_Dakawa, Morogoro, Tanzania</option>
<option value="2022_WS_Ifakara, Morogoro_Ifakara, Morogoro, Tanzania-2022">2022_WS_Ifakara, Morogoro_Ifakara, Morogoro, Tanzania-2022</option>
<option value="2023_WS_Dakawa, Morogoro_Dakawa, Morogoro, Tanzania-2023">2023_WS_Dakawa, Morogoro_Dakawa, Morogoro, Tanzania-2023</option>
<option value="2023_WS_TARI, Morogoro_TARI, Morogoro, Tanzania-2023">2023_WS_TARI, Morogoro_TARI, Morogoro, Tanzania-2023</option>
<option value="2023_WS_Ifakara, Morogoro_Ifakara, Morogoro, Tanzania-2023">2023_WS_Ifakara, Morogoro_Ifakara, Morogoro, Tanzania-2023</option></select>
<script type="application/json" data-for="staApp_1-envSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="staApp_1-outaad787126908ce27" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

The following boxplot allows you to see the distribution of predicted values by trait (y-axis) in the different environments to double check that everything looks OK.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitStaBox-label" for="staApp_1-traitStaBox">Trait to filter:</label>
<div>
<select id="staApp_1-traitStaBox" class="shiny-input-select"><option value="PH_ALL" selected>PH_ALL</option>
<option value="GYKGPHA" selected>GYKGPHA</option>
<option value="DTF" selected>DTF</option></select>
<script type="application/json" data-for="staApp_1-traitStaBox" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-out9e9ae73a4d94dde0" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Per-environment merit estimates of top designations (entries)

In the following plot you can observe the comparison between the top 30 designations from each entry type category for the different traits. If a category has less than a 30 designations all individuals are displayed. This should allow you to identify the top designations in each environment. We would NOT recommend you to use this for selection of parents or products. Wait until you have the results of the multi-trial analysis and selection indices.

<!--html_preserve--><div class="shiny-input-panel">
<div class="shiny-flow-layout">
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitStaComp-label" for="staApp_1-traitStaComp">Trait to filter:</label>
<div>
<select id="staApp_1-traitStaComp" class="shiny-input-select"><option value="PH_ALL" selected>PH_ALL</option>
<option value="GYKGPHA">GYKGPHA</option>
<option value="DTF">DTF</option></select>
<script type="application/json" data-for="staApp_1-traitStaComp" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitStaCompEnv-label" for="staApp_1-traitStaCompEnv">Environment</label>
<div>
<select id="staApp_1-traitStaCompEnv" class="shiny-input-select"><option value="2022_WS_Agusan de Norte, Philippines_Agusan de Norte, Philippines-2022" selected>2022_WS_Agusan de Norte, Philippines_Agusan de Norte, Philippines-2022</option>
<option value="2022_WS_Albay, Philippines_Albay, Philippines-2022">2022_WS_Albay, Philippines_Albay, Philippines-2022</option>
<option value="2022_WS_Bagamoyo_Bagamoyo, Tanzania-2022">2022_WS_Bagamoyo_Bagamoyo, Tanzania-2022</option>
<option value="2022_WS_Can Tho, Vietnam_Can Tho, Vietnam Early stress-2022">2022_WS_Can Tho, Vietnam_Can Tho, Vietnam Early stress-2022</option>
<option value="2022_WS_CARDI, Cambodia_CARDI, Cambodia Early drought stress">2022_WS_CARDI, Cambodia_CARDI, Cambodia Early drought stress</option>
<option value="2022_WS_CARDI, Cambodia_CARDI, Cambodia Early submergence stress">2022_WS_CARDI, Cambodia_CARDI, Cambodia Early submergence stress</option>
<option value="2022_WS_Dakawa, Morogoro_Dakawa, Morogoro, Tanzania">2022_WS_Dakawa, Morogoro_Dakawa, Morogoro, Tanzania</option>
<option value="2022_WS_Dakawa, Morogoro_Dakawa, Morogoro, Tanzania-2022">2022_WS_Dakawa, Morogoro_Dakawa, Morogoro, Tanzania-2022</option>
<option value="2022_WS_Ifakara, Morogoro_Ifakara, Morogoro, Tanzania-2022">2022_WS_Ifakara, Morogoro_Ifakara, Morogoro, Tanzania-2022</option>
<option value="2022_WS_Isabela, Philippines_Isabela, Philippines-2022">2022_WS_Isabela, Philippines_Isabela, Philippines-2022</option>
<option value="2022_WS_Khajura, Nepal_Khajura, Nepal Early non-stress-2022">2022_WS_Khajura, Nepal_Khajura, Nepal Early non-stress-2022</option>
<option value="2022_WS_Khajura, Nepal_Khajura, Nepal Early stress-2022">2022_WS_Khajura, Nepal_Khajura, Nepal Early stress-2022</option>
<option value="2022_WS_Kushtia, Bangladesh_Kushtia, Bangladesh Early stress-2022">2022_WS_Kushtia, Bangladesh_Kushtia, Bangladesh Early stress-2022</option>
<option value="2022_WS_Kushtia, Bangladesh_Kushtia, Bangladesh non-stress-2022">2022_WS_Kushtia, Bangladesh_Kushtia, Bangladesh non-stress-2022</option>
<option value="2022_WS_Kushtia, Bangladesh_Kushtia, Bangladesh stress-2022">2022_WS_Kushtia, Bangladesh_Kushtia, Bangladesh stress-2022</option>
<option value="2022_WS_Kyela, Mbeya_Kyela, Mbeya, Tanzania-2022">2022_WS_Kyela, Mbeya_Kyela, Mbeya, Tanzania-2022</option>
<option value="2022_WS_Laguna, Philippines_Laguna, Philippines-2022">2022_WS_Laguna, Philippines_Laguna, Philippines-2022</option>
<option value="2022_WS_Murunkan, Sri Lanka_Murunkan, Sri Lanka Early drought non-stress">2022_WS_Murunkan, Sri Lanka_Murunkan, Sri Lanka Early drought non-stress</option>
<option value="2022_WS_Murunkan, Sri Lanka_Murunkan, Sri Lanka stress-2022">2022_WS_Murunkan, Sri Lanka_Murunkan, Sri Lanka stress-2022</option>
<option value="2022_WS_Mwea_Mwea, Kenya-2022">2022_WS_Mwea_Mwea, Kenya-2022</option>
<option value="2022_WS_Mziva_Mziva, Mozambique">2022_WS_Mziva_Mziva, Mozambique</option>
<option value="2022_WS_Mziva_Mziva, Mozambique-2022">2022_WS_Mziva_Mziva, Mozambique-2022</option>
<option value="2022_WS_Negros, Philippines_Negros, Philippines-2022">2022_WS_Negros, Philippines_Negros, Philippines-2022</option>
<option value="2022_WS_Nueva Ecija, Philippines_Nueva Ecija, Philippines-2022">2022_WS_Nueva Ecija, Philippines_Nueva Ecija, Philippines-2022</option>
<option value="2022_WS_Puttalam, Sri Lanka_Puttalam, Sri Lanka Early salinity non-stress">2022_WS_Puttalam, Sri Lanka_Puttalam, Sri Lanka Early salinity non-stress</option>
<option value="2022_WS_RCOL_RCOL, Mozambique-2022">2022_WS_RCOL_RCOL, Mozambique-2022</option>
<option value="2023_DS_Isabela, Philippines_Isabela, Philippines-2023">2023_DS_Isabela, Philippines_Isabela, Philippines-2023</option>
<option value="2023_DS_Laguna, Philippines_Laguna, Philippines-2023">2023_DS_Laguna, Philippines_Laguna, Philippines-2023</option>
<option value="2023_DS_Negros, Philippines_Negros, Philippines-2023">2023_DS_Negros, Philippines_Negros, Philippines-2023</option>
<option value="2023_DS_Nueva Ecija, Philippines_Nueva Ecija, Philippines-2023">2023_DS_Nueva Ecija, Philippines_Nueva Ecija, Philippines-2023</option>
<option value="2023_WS_Dakawa, Morogoro_Dakawa, Morogoro, Tanzania-2023">2023_WS_Dakawa, Morogoro_Dakawa, Morogoro, Tanzania-2023</option>
<option value="2023_WS_Ifakara, Morogoro_Ifakara, Morogoro, Tanzania-2023">2023_WS_Ifakara, Morogoro_Ifakara, Morogoro, Tanzania-2023</option>
<option value="2023_WS_TARI, Morogoro_TARI, Morogoro, Tanzania-2023">2023_WS_TARI, Morogoro_TARI, Morogoro, Tanzania-2023</option></select>
<script type="application/json" data-for="staApp_1-traitStaCompEnv" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-out9cbe99ec91695a16" style="width:100%;height:400px;"></div><!--/html_preserve-->


### Correlation between environments

The following plot aims to show the correlation between BLUEs or BLUPs (depending on the parameter settings) among the different environments for the traits available in order to identify if there is one or more environments that do not align with the target population of environments (i.e., negatively correlated with the main cluster across most environments). You may want to exclude such environments from the multi-trial analysis (MTA) to ensure that selected designations in the MTA achieve genetic gain in the main cluster of environments.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitStaCor-label" for="staApp_1-traitStaCor">Trait to filter:</label>
<div>
<select id="staApp_1-traitStaCor" class="shiny-input-select"><option value="PH_ALL" selected>PH_ALL</option>
<option value="GYKGPHA" selected>GYKGPHA</option>
<option value="DTF" selected>DTF</option></select>
<script type="application/json" data-for="staApp_1-traitStaCor" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-out808a7af17c8ff09f" style="width:100%;height:400px;"></div><!--/html_preserve-->


### References of methods used

Velazco, J. G., Rodriguez-Alvarez, M. X., Boer, M. P., Jordan, D. R., Eilers, P. H., Malosetti, M., & Van Eeuwijk, F. A. (2017). Modelling spatial trends in sorghum breeding field trials using a two-dimensional P-spline mixed model. Theoretical and Applied Genetics, 130, 1375-1392.

Rodriguez-Alvarez, M. X., Boer, M. P., van Eeuwijk, F. A., & Eilers, P. H. (2018). Correcting for spatial heterogeneity in plant breeding experiments with P-splines. Spatial Statistics, 23, 52-71.

R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

Boer M, van Rossum B (2022). LMMsolver: Linear Mixed Model Solver. R package version 1.0.4.9000.

<p>&nbsp;</p>

