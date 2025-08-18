---
title: "Multi-Trial Analysis Report"
author: "Contact:<a href = 'https://github.com/Breeding-Analytics/bioflow' target = '_blank'>Breeding Analytics Team, OneCGIAR</a> breedinganalytics@cgiar.org"
date: "July 24, 2025"  
output: html_document
params:
  toDownload: FALSE
  # modelUsed: NULL
---









### Objectives of Multi-Trial Analysis

The objective of this dashboard is to help scientist to understand the following points:

1. Overall number of entries and effect types included in the multi trial analysis (input)

2. High-level view of the phenotypic adjusted means included in the analysis (input)

3. Number of entries connecting the different environments per trait (input)

4. Phenotypic correlation between environments for the traits present (input)

5. Across environment KPIs per trait such as reliability and variance component ratios (output) 

6. Individual across environment predictions for each trait (output) 

7. Individual sensitivity and stability values across environments (output)

8. Percent check comparison against the different benchmark varieties present in the dataset (output)

9. Genetic correlation between the traits (output)

Understanding these data features should allow the scientist to identify which traits express more genotype by environment interactions and how they should be selected. It should also allow the scientist to assess the correlation between traits and how the product profile should be addressed to maximize genetic gains while developing the needed varieties at the same time. Materials with the highest performance and sensitivity to the environment could be potential nominations for advancement although we recommend to don't select new parents or products until the results from the multi-trial analysis are analyzed with a selection index.  

### Metrics: Table of parameters

The following table aims to make a high-level assessment of the different types of entries included in the analysis across environments.

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="mtaLMMsolveApp_1-out2db429e2ef2fa95b" style="width:100%;height:auto;"></div><!--/html_preserve-->

### Metadata: Map of trials planted

The following map allows you to assess the location where trials are planted.

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaLMMsolveApp_1-outb00670e997bcae65" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Predictions: By environment merit distribution

The following boxplot allows you to inspect the distribution of adjusted means (y-axis) from the different environments for each trait that were used as input for the analysis. The environments are sorted by environmental mean to understand the slope of the regression fitted in the Finlay-Wilkinson model. It is recommended that you have at least 6 environments to fit the random regressions over the environmental indices or other weather and soil variables

<p>&nbsp;</p>

<!--html_preserve--><div class="shiny-input-panel">
<div class="shiny-flow-layout">
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaLMMsolveApp_1-boxplotMtaPredsType-label" for="mtaLMMsolveApp_1-boxplotMtaPredsType">Effect type</label>
<div>
<select id="mtaLMMsolveApp_1-boxplotMtaPredsType" class="shiny-input-select"><option value="designation" selected>designation</option></select>
<script type="application/json" data-for="mtaLMMsolveApp_1-boxplotMtaPredsType" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
<div>
<div class="col-sm-12"></div>
</div>
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaLMMsolveApp_1-boxplotMtaPredsTrait-label" for="mtaLMMsolveApp_1-boxplotMtaPredsTrait">Trait</label>
<div>
<select id="mtaLMMsolveApp_1-boxplotMtaPredsTrait" class="shiny-input-select"><option value="Grain_Moisture" selected>Grain_Moisture</option>
<option value="Ear_Height_cm">Ear_Height_cm</option>
<option value="Plant_Height_cm">Plant_Height_cm</option>
<option value="Yield_Mg_ha">Yield_Mg_ha</option></select>
<script type="application/json" data-for="mtaLMMsolveApp_1-boxplotMtaPredsTrait" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
</div>
</div><!--/html_preserve-->

<div style = "width:auto; height:auto; overflow:auto">

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaLMMsolveApp_1-out9a52ae776f4a2326" style="width:100%;height:400px;"></div><!--/html_preserve-->
</div>

### Predictions: Connectivity between environments

The following heatmap and histogram allows you to assess the connectivity (gennotypes in common) between different environments for a given trait. Our recommendation is that each pair of environments should at least have 30 genotypes in common in order to estimate genetic correlations and at least 2-3 entries in common to adjust across environment means. We classify as low everything below 30, intermediate connectivity between 30 to 60, and high everything above 60.

<p>&nbsp;</p>

<!--html_preserve--><div class="shiny-input-panel">
<div class="shiny-flow-layout">
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaLMMsolveApp_1-traitMtaConnect-label" for="mtaLMMsolveApp_1-traitMtaConnect">Trait</label>
<div>
<select id="mtaLMMsolveApp_1-traitMtaConnect" class="shiny-input-select"><option value="Grain_Moisture" selected>Grain_Moisture</option>
<option value="Ear_Height_cm">Ear_Height_cm</option>
<option value="Plant_Height_cm">Plant_Height_cm</option>
<option value="Yield_Mg_ha">Yield_Mg_ha</option></select>
<script type="application/json" data-for="mtaLMMsolveApp_1-traitMtaConnect" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
<div>
<div class="form-group shiny-input-container">
<div class="checkbox">
<label>
<input id="mtaLMMsolveApp_1-freqConnect" type="checkbox" class="shiny-input-checkbox" checked="checked"/>
<span>Show frequency distribution</span>
</label>
</div>
</div>
</div>
</div>
</div><!--/html_preserve-->

<div style = "width:auto; height:auto; overflow:auto">

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaLMMsolveApp_1-out2a9791c2e9fca0ad" style="width:100%;height:400px;"></div><!--/html_preserve-->

</div>

### Predictions: Correlations between environments

The following heatmap and histogram allows to assess the genetic correlations among the different environment one trait at a time. If the user modeled GxE, this matrix is calculated as the correlation between the environment-specific estimates for individuals. We recommend that only environments that are on-average positively correlated with the main cluster (i.e., rG>0) are included in the multi-trial analysis to guarantee decent rates of genetic gain and adapted products in the TPE.

<p>&nbsp;</p>



<div style = "width:auto; height:auto; overflow:auto">

No interaction present. Skipping correlation plot.

</div>

### Predictions: Biplot by trait

The following graph allows to see the clustering of different genotypes in the TPE for each trait. This can help you identify clusters of environments and better define the TPE.

<p>&nbsp;</p>



<div style = "width:auto; height:auto; overflow:auto">

No interaction present. Skipping biplot.

</div>

### Predictions: Merit estimates of top entries

In the left-side plot you can observe the comparison between the top 100 entries from each effect type category for the different traits. If a category has less than a 100 entries all individuals are displayed. This should allow you to identify the entries that could potentially become parents or nominated for advanced stages of evaluation. We would recommend you to wait until a selection index is calculated. In the right-side plot you can see a boxplot of the entire distribution of values for each effectType category so you know until which trait-values the rest of the entries that are not plotted in the left-side plot reach. 


<!--html_preserve--><div class="shiny-input-panel">
<div class="shiny-flow-layout">
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaLMMsolveApp_1-scatterMtaPredsType-label" for="mtaLMMsolveApp_1-scatterMtaPredsType">Effect type:</label>
<div>
<select id="mtaLMMsolveApp_1-scatterMtaPredsType" class="shiny-input-select"><option value="designation" selected>designation</option></select>
<script type="application/json" data-for="mtaLMMsolveApp_1-scatterMtaPredsType" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaLMMsolveApp_1-scatterMtaPredsTrait-label" for="mtaLMMsolveApp_1-scatterMtaPredsTrait">Trait</label>
<div>
<select id="mtaLMMsolveApp_1-scatterMtaPredsTrait" class="shiny-input-select"><option value="Grain_Moisture" selected>Grain_Moisture</option>
<option value="Ear_Height_cm">Ear_Height_cm</option>
<option value="Plant_Height_cm">Plant_Height_cm</option>
<option value="Yield_Mg_ha">Yield_Mg_ha</option></select>
<script type="application/json" data-for="mtaLMMsolveApp_1-scatterMtaPredsTrait" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaLMMsolveApp_1-scatterMtaPredsEnvir-label" for="mtaLMMsolveApp_1-scatterMtaPredsEnvir">Environment</label>
<div>
<select id="mtaLMMsolveApp_1-scatterMtaPredsEnvir" class="shiny-input-select"><option value="(Intercept)" selected>(Intercept)</option></select>
<script type="application/json" data-for="mtaLMMsolveApp_1-scatterMtaPredsEnvir" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaLMMsolveApp_1-scatterMtaPredsTop-label" for="mtaLMMsolveApp_1-scatterMtaPredsTop">Number of Entries</label>
<input id="mtaLMMsolveApp_1-scatterMtaPredsTop" type="number" class="shiny-input-number form-control" value="100" data-update-on="change" min="1"/>
</div>
</div>
</div>
</div><!--/html_preserve-->

<div style = "width:auto; height:auto; overflow:auto">

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaLMMsolveApp_1-outeda6a58deb1cbde2" style="width:100%;height:400px;"></div><!--/html_preserve-->

</div>

### Predictions: Table of estimates 

The following table allows you to inspect the trait predictions in wide format together with the QTL profile (in case those are available) to understand the type of data that would be used to calculate a selection index (e.g., desire index).

<p>&nbsp;</p>

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="mtaLMMsolveApp_1-out5cf0baf44eaeece9" style="width:100%;height:auto;"></div><!--/html_preserve-->

### Predictions: Correlations between traits

The following heatmap and histogram allows to see the genetic correlations among traits calculated using across environment estimates of merit for the different traits. This can be used to understand the implications of selecting for a set of traits to achieve a product profile and make neccesary adjustment to the selection strategy.

<p>&nbsp;</p>

<!--html_preserve--><div class="shiny-input-panel">
<div class="shiny-flow-layout">
<div>
<div class="form-group shiny-input-container">
<div class="checkbox">
<label>
<input id="mtaLMMsolveApp_1-freqCor" type="checkbox" class="shiny-input-checkbox" checked="checked"/>
<span>Show frequency distribution</span>
</label>
</div>
</div>
</div>
</div>
</div><!--/html_preserve-->

<div style = "width:auto; height:auto; overflow:auto">

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaLMMsolveApp_1-out5938d4572cbc61bd" style="width:100%;height:400px;"></div><!--/html_preserve-->

</div>

### Percent check comparison to top 30 entries

The following plots allow the user to compare the top 30 entries against the different checks/benchmarks present in the dataset for each trait. The table below that shows the wide-table of all possible comparisons for all traits.

<p>&nbsp;</p>



<div style = "width:auto; height:auto; overflow:auto">

No checks found. Skipping percent check comparison.
</div>



### Modeling parameters

This section aims to provide the modeling table for the analysis in order to keep track of which environments were used in the analysis, what was the final model used for each trait and other potentially important parameters for future reference.

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="mtaLMMsolveApp_1-out7ee53e54c534a8d4" style="width:100%;height:auto;"></div><!--/html_preserve-->


### References of methods used

Finlay, K. W., & Wilkinson, G. N. (1963). The analysis of adaptation in a plant-breeding programme. Australian journal of agricultural research, 14(6), 742-754.

Henderson Jr, C. R. (1982). Analysis of covariance in the mixed model: higher-level, nonhomogeneous, and random regressions. Biometrics, 623-640.

Odegard, J., Indahl, U., Stranden, I., & Meuwissen, T. H. (2018). Large-scale genomic prediction using singular value decomposition of the genotype matrix. Genetics Selection Evolution, 50(1), 1-12.

Xiang, T., Christensen, O. F., & Sørensen, A. C. (2016). Genomic evaluation by including dominance effects and inbreeding depression for purebred and crossbred performance with a unified model. Genetics Selection Evolution, 48(1), 84

Batista, L. G., Mello, V. H., Souza, A. P., & Margarido, G. R. A. (2022). Genomic prediction with allele dosage information in highly polyploid species. Theoretical and Applied Genetics, 135(2), 723–739.

R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

Boer M, van Rossum B (2022). LMMsolver: Linear Mixed Model Solver. R package version 1.0.4.9000.

Covarrubias-Pazaran G. (2024). lme4breeding: enabling genetic evaluation in the era of genomic data. bioRxiv, 2024-05.

Covarrubias-Pazaran G. (2016). Genome assisted prediction of quantitative traits using the R package sommer. PLoS ONE 11(6):1-15.


