---
title: "Multi-Trial Analysis ASReml-R Report"
author: "Contact:<a href = 'https://github.com/Breeding-Analytics/bioflow' target = '_blank'>Breeding Analytics Team, OneCGIAR</a> breedinganalytics@cgiar.org"
date: "February 11, 2025"  
output: html_document
params:
  toDownload: FALSE
---
  








### Objectives of Multi-Trial Analysis

The objective of this dashboard is to help scientist to understand the following points:
  
1. Overall number of entries and effect types included in the multi trial analysis (input)

2. High-level view of the phenotypic adjusted means included in the analysis (input)

3. Number of entries connecting the different environments per trait (input)

3. Phenotypic correlation between environments for the traits present (input)

4. Across environment KPIs per trait such as reliability and variance component ratios (output) 

5. Individual across environment predictions for each trait (output) 

6. Individual sensitivity and stability values across environments (output)

7. Percent check comparison against the different benchmark varieties present in the dataset (output)

8. Genetic correlation between the traits (output)

Understanding these data features should allow the scientist to identify which traits express more genotype by environment interactions and how they should be selected. It should also allow the scientist to assess the correlation between traits and how the product profile should be addressed to maximize genetic gains while developing the needed varieties at the same time. Materials with the highest performance and sensitivity to the environment could be potential nominations for advancement although we recommend to don't select new parents or products until the results from the multi-trial analysis are analyzed with a selection index.  

### Metrics: Table of parameters

The following table aims to make a high-level assessment of the different types of entries included in the analysis across environments.

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="mtaASREMLApp_1-out6e5aba84a49eaddc" style="width:100%;height:auto;"></div><!--/html_preserve-->

### Metadata: Map of trials planted

The following map allows you to assess the location where trials are planted.

No coordinates available. Skipping planting map.

### Predictions: By environment merit distribution

The following boxplot allows you to inspect the distribution of adjusted means (y-axis) from the different environments for each trait that were used as input for the analysis. The environments are sorted by environmental mean to understand the slope of the regression fitted in the Finlay-Wilkinson model. It is recommended that you have at least 6 environments to fit the random regressions over the environmental indices or other weather and soil variables

<p>&nbsp;</p>

<!--html_preserve--><div class="shiny-input-panel">
<div class="shiny-flow-layout">
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaASREMLApp_1-boxplotMtaPredsType-label" for="mtaASREMLApp_1-boxplotMtaPredsType">Effect type</label>
<div>
<select id="mtaASREMLApp_1-boxplotMtaPredsType" class="shiny-input-select"><option value="environment_designation" selected>environment_designation</option>
<option value="environment">environment</option>
<option value="designation">designation</option>
<option value="(Intercept)">(Intercept)</option></select>
<script type="application/json" data-for="mtaASREMLApp_1-boxplotMtaPredsType" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
<div>
<div class="col-sm-12"></div>
</div>
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaASREMLApp_1-boxplotMtaPredsTrait-label" for="mtaASREMLApp_1-boxplotMtaPredsTrait">Trait</label>
<div>
<select id="mtaASREMLApp_1-boxplotMtaPredsTrait" class="shiny-input-select"><option value="GY" selected>GY</option>
<option value="MOI">MOI</option>
<option value="EH">EH</option>
<option value="PH">PH</option></select>
<script type="application/json" data-for="mtaASREMLApp_1-boxplotMtaPredsTrait" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="mtaASREMLApp_1-outb92372947da17d3f" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Predictions: Connectivity between environments

The following heatmap and histogram allows you to assess the connectivity (gennotypes in common) between different environments for a given trait. Our recommendation is that each pair of environments should at least have 30 genotypes in common in order to estimate genetic correlations and at least 2-3 entries in common to adjust across environment means. We classify as low everything below 30, intermediate connectivity between 30 to 60, and high everything above 60.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaASREMLApp_1-traitMtaConnect-label" for="mtaASREMLApp_1-traitMtaConnect"></label>
<div>
<select id="mtaASREMLApp_1-traitMtaConnect" class="shiny-input-select"><option value="GY" selected>GY</option>
<option value="MOI">MOI</option>
<option value="EH">EH</option>
<option value="PH">PH</option></select>
<script type="application/json" data-for="mtaASREMLApp_1-traitMtaConnect" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaASREMLApp_1-out7078d8c9f1bfa9eb" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Predictions: Correlations between environments

The following heatmap and histogram allows to assess the genetic correlations among the different environment one trait at a time. If the user modeled GxE, this matrix is calculated as the correlation between the environment-specific estimates for individuals. If a pure main-effect model is specified this matrix is calculated as the correlation between the single-trial analysis estimates. We recommend that only environments that are on-average positively correlated with the main cluster (i.e., rG>0) are included in the multi-trial analysis to guarantee decent rates of genetic gain and adapted products in the TPE.

<p>&nbsp;</p>

<!--html_preserve--><div class="shiny-input-panel">
<div class="shiny-flow-layout">
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaASREMLApp_1-corrplotMtaPredsType-label" for="mtaASREMLApp_1-corrplotMtaPredsType">Effect type</label>
<div>
<select id="mtaASREMLApp_1-corrplotMtaPredsType" class="shiny-input-select" multiple="multiple"><option value="environment_designation" selected>environment_designation</option>
<option value="environment">environment</option>
<option value="designation">designation</option>
<option value="(Intercept)">(Intercept)</option></select>
<script type="application/json" data-for="mtaASREMLApp_1-corrplotMtaPredsType">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
<div>
<div class="col-sm-12"></div>
</div>
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaASREMLApp_1-traitPredictionsCorrelation-label" for="mtaASREMLApp_1-traitPredictionsCorrelation">Trait</label>
<div>
<select id="mtaASREMLApp_1-traitPredictionsCorrelation" class="shiny-input-select"><option value="GY" selected>GY</option>
<option value="MOI">MOI</option>
<option value="EH">EH</option>
<option value="PH">PH</option></select>
<script type="application/json" data-for="mtaASREMLApp_1-traitPredictionsCorrelation" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaASREMLApp_1-outb842f3f2fd763c78" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Predictions: Biplot by trait

The following graph allows to see the clustering of different genotypes in the TPE for each trait. This can help you identify clusters of environments and better define the TPE.

<p>&nbsp;</p>

<!--html_preserve--><div class="shiny-input-panel">
<div class="shiny-flow-layout">
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaASREMLApp_1-biplotMtaPredsType-label" for="mtaASREMLApp_1-biplotMtaPredsType">Effect type:</label>
<div>
<select id="mtaASREMLApp_1-biplotMtaPredsType" class="shiny-input-select" multiple="multiple"><option value="environment_designation" selected>environment_designation</option>
<option value="environment">environment</option>
<option value="designation">designation</option>
<option value="(Intercept)">(Intercept)</option></select>
<script type="application/json" data-for="mtaASREMLApp_1-biplotMtaPredsType">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
<div>
<div class="col-sm-12"></div>
</div>
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaASREMLApp_1-traitBiplot-label" for="mtaASREMLApp_1-traitBiplot">Trait:</label>
<div>
<select id="mtaASREMLApp_1-traitBiplot" class="shiny-input-select"><option value="GY" selected>GY</option>
<option value="MOI">MOI</option>
<option value="EH">EH</option>
<option value="PH">PH</option></select>
<script type="application/json" data-for="mtaASREMLApp_1-traitBiplot" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaASREMLApp_1-outc598d7d9da443c8e" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Predictions: Merit estimates of top entries

In the left-side plot you can observe the comparison between the top 100 entries from each effect type category for the different traits. If a category has less than a 100 entries all individuals are displayed. This should allow you to identify the entries that could potentially become parents or nominated for advanced stages of evaluation. We would recommend you to wait until a selection index is calculated. In the right-side plot you can see a boxplot of the entire distribution of values for each effectType category so you know until which trait-values the rest of the entries that are not plotted in the left-side plot reach. 


<!--html_preserve--><div class="shiny-input-panel">
<div class="shiny-flow-layout">
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaASREMLApp_1-scatterMtaPredsType-label" for="mtaASREMLApp_1-scatterMtaPredsType">Effect type:</label>
<div>
<select id="mtaASREMLApp_1-scatterMtaPredsType" class="shiny-input-select" multiple="multiple"><option value="environment_designation" selected>environment_designation</option>
<option value="environment">environment</option>
<option value="designation">designation</option>
<option value="(Intercept)">(Intercept)</option></select>
<script type="application/json" data-for="mtaASREMLApp_1-scatterMtaPredsType">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
<div>
<div class="col-sm-12"></div>
</div>
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaASREMLApp_1-scatterMtaPredsTrait-label" for="mtaASREMLApp_1-scatterMtaPredsTrait">Trait</label>
<div>
<select id="mtaASREMLApp_1-scatterMtaPredsTrait" class="shiny-input-select"><option value="GY" selected>GY</option>
<option value="MOI">MOI</option>
<option value="EH">EH</option>
<option value="PH">PH</option></select>
<script type="application/json" data-for="mtaASREMLApp_1-scatterMtaPredsTrait" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
<div>
<div class="col-sm-12"></div>
</div>
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaASREMLApp_1-scatterMtaPredsEnvir-label" for="mtaASREMLApp_1-scatterMtaPredsEnvir">Environment</label>
<div>
<select id="mtaASREMLApp_1-scatterMtaPredsEnvir" class="shiny-input-select"><option value="(Intercept)" selected>(Intercept)</option>
<option value="1_AGT21A-EVALIITWC-03-1">1_AGT21A-EVALIITWC-03-1</option>
<option value="1_AGT21A-EVALIITWC-04-1">1_AGT21A-EVALIITWC-04-1</option>
<option value="2_AGT21A-EVALIITWC-01-1">2_AGT21A-EVALIITWC-01-1</option>
<option value="2_AGT21A-EVALIITWC-02-1">2_AGT21A-EVALIITWC-02-1</option>
<option value="2_AGT21A-EVALIITWC-04-2">2_AGT21A-EVALIITWC-04-2</option>
<option value="3_AGT21A-EVALIITWC-04-3">3_AGT21A-EVALIITWC-04-3</option>
<option value="4_AGT21A-EVALIITWC-04-4">4_AGT21A-EVALIITWC-04-4</option>
<option value="5_AGT21A-EVALIITWC-04-5">5_AGT21A-EVALIITWC-04-5</option>
<option value="6_AGT21A-EVALIITWC-04-6">6_AGT21A-EVALIITWC-04-6</option>
<option value="8_AGT21A-EVALIITWC-04-8">8_AGT21A-EVALIITWC-04-8</option></select>
<script type="application/json" data-for="mtaASREMLApp_1-scatterMtaPredsEnvir" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaASREMLApp_1-outb45c76b8b632441e" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Predictions: Table of estimates 

The following table allows you to inspect the trait predictions in wide format together with the QTL profile (in case those are available) to understand the type of data that would be used to calculate a selection index (e.g., desire index).

<p>&nbsp;</p>

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="mtaASREMLApp_1-out6f8573964baa2ac5" style="width:100%;height:auto;"></div><!--/html_preserve-->

### Predictions: Correlations between traits

The following heatmap and histogram allows to see the genetic correlations among traits calculated using across environment estimates of merit for the different traits. This can be used to understand the implications of selecting for a set of traits to achieve a product profile and make neccesary adjustment to the selection strategy.

<p>&nbsp;</p>
  
<!--html_preserve--><div class="shiny-input-panel">
<div class="shiny-flow-layout">
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaASREMLApp_1-corrplotTraitMtaPredsType-label" for="mtaASREMLApp_1-corrplotTraitMtaPredsType">Effect type</label>
<div>
<select id="mtaASREMLApp_1-corrplotTraitMtaPredsType" class="shiny-input-select"><option value="environment_designation" selected>environment_designation</option>
<option value="environment">environment</option>
<option value="designation">designation</option>
<option value="(Intercept)">(Intercept)</option></select>
<script type="application/json" data-for="mtaASREMLApp_1-corrplotTraitMtaPredsType" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
<div>
<div class="col-sm-12"></div>
</div>
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaASREMLApp_1-corrplotTraitMtaPredsEnv-label" for="mtaASREMLApp_1-corrplotTraitMtaPredsEnv">Environment</label>
<div>
<select id="mtaASREMLApp_1-corrplotTraitMtaPredsEnv" class="shiny-input-select"><option value="(Intercept)" selected>(Intercept)</option>
<option value="1_AGT21A-EVALIITWC-03-1">1_AGT21A-EVALIITWC-03-1</option>
<option value="1_AGT21A-EVALIITWC-04-1">1_AGT21A-EVALIITWC-04-1</option>
<option value="2_AGT21A-EVALIITWC-01-1">2_AGT21A-EVALIITWC-01-1</option>
<option value="2_AGT21A-EVALIITWC-02-1">2_AGT21A-EVALIITWC-02-1</option>
<option value="2_AGT21A-EVALIITWC-04-2">2_AGT21A-EVALIITWC-04-2</option>
<option value="3_AGT21A-EVALIITWC-04-3">3_AGT21A-EVALIITWC-04-3</option>
<option value="4_AGT21A-EVALIITWC-04-4">4_AGT21A-EVALIITWC-04-4</option>
<option value="5_AGT21A-EVALIITWC-04-5">5_AGT21A-EVALIITWC-04-5</option>
<option value="6_AGT21A-EVALIITWC-04-6">6_AGT21A-EVALIITWC-04-6</option>
<option value="8_AGT21A-EVALIITWC-04-8">8_AGT21A-EVALIITWC-04-8</option></select>
<script type="application/json" data-for="mtaASREMLApp_1-corrplotTraitMtaPredsEnv" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaASREMLApp_1-out35e1d9c608593e62" style="width:100%;height:400px;"></div><!--/html_preserve-->


### Modeling parameters

This section aims to provide the modeling table for the analysis in order to keep track of which environments were used in the analysis, what was the final model used for each trait and other potentially important parameters for future reference.

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="mtaASREMLApp_1-outf2c0fd58877d1e0e" style="width:100%;height:auto;"></div><!--/html_preserve-->


### References of methods used

R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

Butler, D.G., Cullis, B.R., Gilmour, A.R., Gogel, B.G. and Thompson, R. (2023). ASReml-R: estimates variance components under a general linear mixed model by residual maximum likelihood (REML).

<p>&nbsp;</p>
  
  
  
