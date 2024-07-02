---
title: "Multi-Trial Analysis Report"
author: "Contact:<a href = 'https://github.com/Breeding-Analytics/bioflow' target = '_blank'>Breeding Analytics Team, OneCGIAR</a> breedinganalytics@cgiar.org"
date: "July 01, 2024"  
output: html_document
params:
  toDownload: FALSE
---









### Objectives of Multi-Trial Analysis

The objective of this dashboard is to help scientist to understand the following points:

1. Overall number of entries and entry types included in the multi trial analysis (input)

2. High-level of the phenotypic adjusted means included in the analysis (input)

3. Number of entries connecting the different environments per trait (input)

3. Phenotypic correlation between environments for the traits present (input)

4. Across environment KPIs per trait such as reliability and variance component ratios (output) 

5. Individual across environment predictions for each trait (output) 

6. Individual sensitivity values according to the Finlay-Wilkinson model (Finlay & Wilkinson, 1963) (output)

7. Percent check comparison against the different benchmark varieties present in the dataset (output)

8. Genetic correlation between the traits (output)

Understanding these data features should allow the scientist to identify which traits express more genotype by environment interactions and how they should be selected. It should also allow the scientist to assess the correlation between traits and how the product profile should be addressed to maximize genetic gains while developing the needed varieties at the same time. Materials with the highest performance and sensitivity to the environment could be potential nominations for advancement although we recommend to don't select new parents or products until the results from the multi-trial analysis are analyzed with a selection index.  

### Number of individuals per trait and entry type

The following table aims to make a high-level assessment of the different types of entries included in the analysis across environments.

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="mtaExploreApp_1-out3cdd78cc7e15aa7e" style="width:100%;height:auto;"></div><!--/html_preserve-->

### Map of trials planted

The following map allows you to assess the location where trials are planted.

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaExploreApp_1-out1fb6f013fcc935d7" style="width:100%;height:400px;"></div><!--/html_preserve-->

### By environment merit distribution

The following boxplot allows you to inspect the distribution of adjusted means (y-axis) from the different environments for each trait that were used as input for the analysis. The environments are sorted by environmental mean to understand the slope of the regression fitted in the Finlay-Wilkinson model. It is recommended that you have at least 6 environments to fit the random regressions over the environmental indices or other weather and soil variables

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaExploreApp_1-traitMta-label" for="mtaExploreApp_1-traitMta"></label>
<div>
<select id="mtaExploreApp_1-traitMta" class="shiny-input-select"><option value="Yield_Mg_ha" selected>Yield_Mg_ha</option>
<option value="Ear_Height_cm">Ear_Height_cm</option></select>
<script type="application/json" data-for="mtaExploreApp_1-traitMta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="mtaExploreApp_1-oute0d430547db9d5e7" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Connectivity between environments

The following heatmap and histogram allows you to assess the connectivity (gennotypes in common) between different environments for a given trait. Our recommendation is that each pair of environments should at least have 30 genotypes in common in order to estimate genetic correlations and at least 2-3 entries in common to adjust across environment means. We classify as low everything below 30, intermediate connectivity between 30 to 60, and high everything above 60.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaExploreApp_1-traitMtaConnect-label" for="mtaExploreApp_1-traitMtaConnect"></label>
<div>
<select id="mtaExploreApp_1-traitMtaConnect" class="shiny-input-select"><option value="Yield_Mg_ha" selected>Yield_Mg_ha</option>
<option value="Ear_Height_cm">Ear_Height_cm</option></select>
<script type="application/json" data-for="mtaExploreApp_1-traitMtaConnect" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaExploreApp_1-outee224a8e06e7c6ea" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Genetic correlations between environments

The following heatmap and histogram allows to assess the genetic correlations among the different environment one trait at a time. If the user modeled GxE, this matrix is calculated as the correlation between the environment-specific estimates for individuals. If a pure main-effect model is specified this matrix is calculated as the correlation between the single-trial analysis estimates. We recommend that only environments that are on-average positively correlated with the main cluster (i.e., rG>0) are included in the multi-trial analysis to guarantee decent rates of genetic gain and adapted products in the TPE.

<p>&nbsp;</p>



Only one level for environment found in predictions (you fitted a main effect model maybe?). Skipping correlation plot.

<p>&nbsp;</p>

### Biplot by trait

The following graph allows to see the clustering of different genotypes in the TPE for each trait. This can help you identify clusters of environments and better define the TPE.

<p>&nbsp;</p>



Only one level for environment found in predictions (you fitted a main effect or a Finlay-Wilkinson model maybe?). Skipping biplot.


<p>&nbsp;</p>

### Across-environment metrics

The following barplot aims to help you inspect the across environment estimates for multiple parameters from the multi-trial analysis such as across-environment reliability.

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaExploreApp_1-parameterMetrics2-label" for="mtaExploreApp_1-parameterMetrics2">Parameter:</label>
<div>
<select id="mtaExploreApp_1-parameterMetrics2" class="shiny-input-select"><option value="V.designation" selected>V.designation</option>
<option value="V.location" selected>V.location</option>
<option value="V.environment" selected>V.environment</option>
<option value="V.Residual" selected>V.Residual</option>
<option value="mean.designation" selected>mean.designation</option>
<option value="mean.location" selected>mean.location</option>
<option value="mean.environment" selected>mean.environment</option>
<option value="mean.Residual" selected>mean.Residual</option>
<option value="CV.designation" selected>CV.designation</option>
<option value="CV.location" selected>CV.location</option>
<option value="CV.environment" selected>CV.environment</option>
<option value="CV.Residual" selected>CV.Residual</option>
<option value="r2.designation" selected>r2.designation</option>
<option value="r2.location" selected>r2.location</option>
<option value="r2.environment" selected>r2.environment</option>
<option value="r2.Residual" selected>r2.Residual</option>
<option value="nEnv" selected>nEnv</option></select>
<script type="application/json" data-for="mtaExploreApp_1-parameterMetrics2" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="mtaExploreApp_1-outfa48fbdadc3fcc30" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

The following graph allows you to assess the proportion of variance going to different components for the different traits. Above the bars you can see the value of the variance component for each factor. The residual variance is all the variance that could not be explained by the main effect and sensitivity effects. This values should be considered carefully depending of the genetic evaluation model used. For example, when sing the rrBLUP model the variance components reflect the marker variance and may look very small, but we can't conclude that there is not eough genetic signal.

<p>&nbsp;</p>


<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="mtaExploreApp_1-out5f4cc24fd8912c4a" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Merit estimates of top entries

In the left-side plot you can observe the comparison between the top 100 entries from each entry type category for the different traits. If a category has less than a 100 entries all individuals are displayed. This should allow you to identify the entries that could potentially become parents or nominated for advanced stages of evaluation. We would recommend you to wait until a selection index is calculated. In the right-side plot you can see a boxplot of the entire distribution of values for each entryType category so you know until which trait-values the rest of the entries that are not plotted in the left-side plot reach. 


<!--html_preserve--><div class="shiny-input-panel">
<div class="shiny-flow-layout">
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaExploreApp_1-traitMta2-label" for="mtaExploreApp_1-traitMta2">Trait</label>
<div>
<select id="mtaExploreApp_1-traitMta2" class="shiny-input-select"><option value="Yield_Mg_ha" selected>Yield_Mg_ha</option>
<option value="Ear_Height_cm">Ear_Height_cm</option></select>
<script type="application/json" data-for="mtaExploreApp_1-traitMta2" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaExploreApp_1-envCovMta2-label" for="mtaExploreApp_1-envCovMta2">Environment</label>
<div>
<select id="mtaExploreApp_1-envCovMta2" class="shiny-input-select"><option value="(Intercept)" selected>(Intercept)</option></select>
<script type="application/json" data-for="mtaExploreApp_1-envCovMta2" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaExploreApp_1-out3509c730cc07740a" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>




### Trait by trait-sensitivity plots

The following plots allow you to identify individuals with high performance for a trait but also with high or low sensitivity to the environmental means from a weather covariate or just the environmental means.



No random regression model found. Skipping sensitivity plot.


<p>&nbsp;</p>










### Genetic correlations between traits

The following heatmap and histogram allows to see the genetic correlations among traits calculated using across environment estimates of merit for the different traits. This can be used to understand the implications of selecting for a set of traits to achieve a product profile and make neccesary adjustment to the selection strategy.

<p>&nbsp;</p>

<!--html_preserve--><div class="shiny-input-panel">
<div class="shiny-flow-layout">
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaExploreApp_1-envCovMtaTraits-label" for="mtaExploreApp_1-envCovMtaTraits">Environment</label>
<div>
<select id="mtaExploreApp_1-envCovMtaTraits" class="shiny-input-select"><option value="(Intercept)" selected>(Intercept)</option></select>
<script type="application/json" data-for="mtaExploreApp_1-envCovMtaTraits" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaExploreApp_1-out8c64d86df863d1ad" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Predictions 

The following table allows you to inspect the trait predictions in wide format together with the QTL profile (in case those are available) to understand the type of data that would be used to calculate a selection index (e.g., desire index).

<p>&nbsp;</p>

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="mtaExploreApp_1-outcd376b233eed5530" style="width:100%;height:auto;"></div><!--/html_preserve-->


### References of methods used

Finlay, K. W., & Wilkinson, G. N. (1963). The analysis of adaptation in a plant-breeding programme. Australian journal of agricultural research, 14(6), 742-754.

Henderson Jr, C. R. (1982). Analysis of covariance in the mixed model: higher-level, nonhomogeneous, and random regressions. Biometrics, 623-640.

Odegard, J., Indahl, U., Stranden, I., & Meuwissen, T. H. (2018). Large-scale genomic prediction using singular value decomposition of the genotype matrix. Genetics Selection Evolution, 50(1), 1-12.

R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

Boer M, van Rossum B (2022). LMMsolver: Linear Mixed Model Solver. R package version 1.0.4.9000.

Covarrubias-Pazaran G. (2024). lme4breeding: enabling genetic evaluation in the era of genomic data. bioRxiv, 2024-05.

Covarrubias-Pazaran G. (2016). Genome assisted prediction of quantitative traits using the R package sommer. PLoS ONE 11(6):1-15.

<p>&nbsp;</p>


