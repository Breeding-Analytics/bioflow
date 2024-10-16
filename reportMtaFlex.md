---
title: "Multi-Trial Analysis Report"
author: "Contact:<a href = 'https://github.com/Breeding-Analytics/bioflow' target = '_blank'>Breeding Analytics Team, OneCGIAR</a> breedinganalytics@cgiar.org"
date: "October 11, 2024"  
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

6. Individual sensitivity and stability values across environments (output)

7. Percent check comparison against the different benchmark varieties present in the dataset (output)

8. Genetic correlation between the traits (output)

Understanding these data features should allow the scientist to identify which traits express more genotype by environment interactions and how they should be selected. It should also allow the scientist to assess the correlation between traits and how the product profile should be addressed to maximize genetic gains while developing the needed varieties at the same time. Materials with the highest performance and sensitivity to the environment could be potential nominations for advancement although we recommend to don't select new parents or products until the results from the multi-trial analysis are analyzed with a selection index.  

### Number of individuals per trait and entry type

The following table aims to make a high-level assessment of the different types of entries included in the analysis across environments.

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="mtaExpApp_1-outfb7603c3e54325a4" style="width:100%;height:auto;"></div><!--/html_preserve-->

### Map of trials planted

The following map allows you to assess the location where trials are planted.

No coordinates available. Skipping planting map.

### By environment merit distribution

The following boxplot allows you to inspect the distribution of adjusted means (y-axis) from the different environments for each trait that were used as input for the analysis. The environments are sorted by environmental mean to understand the slope of the regression fitted in the Finlay-Wilkinson model. It is recommended that you have at least 6 environments to fit the random regressions over the environmental indices or other weather and soil variables

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaExpApp_1-traitMta-label" for="mtaExpApp_1-traitMta"></label>
<div>
<select id="mtaExpApp_1-traitMta" class="shiny-input-select"><option value="YLD_TON" selected>YLD_TON</option></select>
<script type="application/json" data-for="mtaExpApp_1-traitMta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="mtaExpApp_1-outf77d535b1842a0b6" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Connectivity between environments

The following heatmap and histogram allows you to assess the connectivity (gennotypes in common) between different environments for a given trait. Our recommendation is that each pair of environments should at least have 30 genotypes in common in order to estimate genetic correlations and at least 2-3 entries in common to adjust across environment means. We classify as low everything below 30, intermediate connectivity between 30 to 60, and high everything above 60.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaExpApp_1-traitMtaConnect-label" for="mtaExpApp_1-traitMtaConnect"></label>
<div>
<select id="mtaExpApp_1-traitMtaConnect" class="shiny-input-select"><option value="YLD_TON" selected>YLD_TON</option></select>
<script type="application/json" data-for="mtaExpApp_1-traitMtaConnect" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaExpApp_1-outd1eb0caa082283ca" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Genetic correlations between environments

The following heatmap and histogram allows to assess the genetic correlations among the different environment one trait at a time. If the user modeled GxE, this matrix is calculated as the correlation between the environment-specific estimates for individuals. If a pure main-effect model is specified this matrix is calculated as the correlation between the single-trial analysis estimates. We recommend that only environments that are on-average positively correlated with the main cluster (i.e., rG>0) are included in the multi-trial analysis to guarantee decent rates of genetic gain and adapted products in the TPE.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaExpApp_1-traitPredictionsCorrelation-label" for="mtaExpApp_1-traitPredictionsCorrelation"></label>
<div>
<select id="mtaExpApp_1-traitPredictionsCorrelation" class="shiny-input-select"><option value="YLD_TON" selected>YLD_TON</option></select>
<script type="application/json" data-for="mtaExpApp_1-traitPredictionsCorrelation" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaExpApp_1-outcf09b24eee07649b" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Biplot by trait

The following graph allows to see the clustering of different genotypes in the TPE for each trait. This can help you identify clusters of environments and better define the TPE.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaExpApp_1-traitBiplot-label" for="mtaExpApp_1-traitBiplot"></label>
<div>
<select id="mtaExpApp_1-traitBiplot" class="shiny-input-select"><option value="YLD_TON" selected>YLD_TON</option></select>
<script type="application/json" data-for="mtaExpApp_1-traitBiplot" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaExpApp_1-out1c54b9db0d963527" style="width:100%;height:400px;"></div><!--/html_preserve-->


<p>&nbsp;</p>

### Across-environment metrics

The following barplot aims to help you inspect the across environment estimates for multiple parameters from the multi-trial analysis such as across-environment reliability.

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaExpApp_1-parameterMetrics2-label" for="mtaExpApp_1-parameterMetrics2">Parameter:</label>
<div>
<select id="mtaExpApp_1-parameterMetrics2" class="shiny-input-select" multiple="multiple"><option value="V.designation">V.designation</option>
<option value="V.location">V.location</option>
<option value="V.environment">V.environment</option>
<option value="V.Residual">V.Residual</option>
<option value="mean.designation">mean.designation</option>
<option value="mean.location">mean.location</option>
<option value="mean.environment">mean.environment</option>
<option value="mean.Residual">mean.Residual</option>
<option value="CV.designation">CV.designation</option>
<option value="CV.location">CV.location</option>
<option value="CV.environment">CV.environment</option>
<option value="CV.Residual">CV.Residual</option>
<option value="r2.designation" selected>r2.designation</option>
<option value="r2.location" selected>r2.location</option>
<option value="r2.environment" selected>r2.environment</option>
<option value="r2.Residual" selected>r2.Residual</option>
<option value="nEnv">nEnv</option></select>
<script type="application/json" data-for="mtaExpApp_1-parameterMetrics2">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="mtaExpApp_1-out9bb553f8346bfa8f" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

The following graph allows you to assess the proportion of variance going to different components for the different traits. Above the bars you can see the value of the variance component for each factor. The residual variance is all the variance that could not be explained by the main effect and sensitivity effects. This values should be considered carefully depending of the genetic evaluation model used. For example, when sing the rrBLUP model the variance components reflect the marker variance and may look very small, but we can't conclude that there is not eough genetic signal.

<p>&nbsp;</p>


<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="mtaExpApp_1-out1874d7b42e1f4b80" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Merit estimates of top entries

In the left-side plot you can observe the comparison between the top 100 entries from each entry type category for the different traits. If a category has less than a 100 entries all individuals are displayed. This should allow you to identify the entries that could potentially become parents or nominated for advanced stages of evaluation. We would recommend you to wait until a selection index is calculated. In the right-side plot you can see a boxplot of the entire distribution of values for each entryType category so you know until which trait-values the rest of the entries that are not plotted in the left-side plot reach. 


<!--html_preserve--><div class="shiny-input-panel">
<div class="shiny-flow-layout">
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaExpApp_1-traitMta2-label" for="mtaExpApp_1-traitMta2">Trait</label>
<div>
<select id="mtaExpApp_1-traitMta2" class="shiny-input-select"><option value="YLD_TON" selected>YLD_TON</option></select>
<script type="application/json" data-for="mtaExpApp_1-traitMta2" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaExpApp_1-envCovMta2-label" for="mtaExpApp_1-envCovMta2">Environment</label>
<div>
<select id="mtaExpApp_1-envCovMta2" class="shiny-input-select"><option value="(Intercept)" selected>(Intercept)</option>
<option value="L.Stage2BIRU2024Wet001">L.Stage2BIRU2024Wet001</option>
<option value="L.Stage2BIBBGI2024Wet008">L.Stage2BIBBGI2024Wet008</option>
<option value="L.Stage2BIRT2024Wet001">L.Stage2BIRT2024Wet001</option>
<option value="L.Stage2TZPWKI2024Wet004">L.Stage2TZPWKI2024Wet004</option>
<option value="L.Stage2TZMODK2024Wet009">L.Stage2TZMODK2024Wet009</option></select>
<script type="application/json" data-for="mtaExpApp_1-envCovMta2" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaExpApp_1-oute3a334e3ad3a2a77" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>




### Trait by trait-sensitivity plots

The following plots allow you to identify individuals with high performance for a trait but also with high or low sensitivity to the environmental means from a weather covariate or just the environmental means.

<!--html_preserve--><div class="shiny-input-panel">
<div class="shiny-flow-layout">
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaExpApp_1-traitMta3-label" for="mtaExpApp_1-traitMta3">Trait</label>
<div>
<select id="mtaExpApp_1-traitMta3" class="shiny-input-select"><option value="YLD_TON" selected>YLD_TON</option></select>
<script type="application/json" data-for="mtaExpApp_1-traitMta3" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaExpApp_1-envCovMta3-label" for="mtaExpApp_1-envCovMta3">Level 1</label>
<div>
<select id="mtaExpApp_1-envCovMta3" class="shiny-input-select"><option value="(Intercept)" selected>(Intercept)</option>
<option value="L.Stage2BIRU2024Wet001">L.Stage2BIRU2024Wet001</option>
<option value="L.Stage2BIBBGI2024Wet008">L.Stage2BIBBGI2024Wet008</option>
<option value="L.Stage2BIRT2024Wet001">L.Stage2BIRT2024Wet001</option>
<option value="L.Stage2TZPWKI2024Wet004">L.Stage2TZPWKI2024Wet004</option>
<option value="L.Stage2TZMODK2024Wet009">L.Stage2TZMODK2024Wet009</option></select>
<script type="application/json" data-for="mtaExpApp_1-envCovMta3" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaExpApp_1-envCovMta4-label" for="mtaExpApp_1-envCovMta4">Level 2</label>
<div>
<select id="mtaExpApp_1-envCovMta4" class="shiny-input-select"><option value="(Intercept)" selected>(Intercept)</option>
<option value="L.Stage2BIRU2024Wet001">L.Stage2BIRU2024Wet001</option>
<option value="L.Stage2BIBBGI2024Wet008">L.Stage2BIBBGI2024Wet008</option>
<option value="L.Stage2BIRT2024Wet001">L.Stage2BIRT2024Wet001</option>
<option value="L.Stage2TZPWKI2024Wet004">L.Stage2TZPWKI2024Wet004</option>
<option value="L.Stage2TZMODK2024Wet009">L.Stage2TZMODK2024Wet009</option></select>
<script type="application/json" data-for="mtaExpApp_1-envCovMta4" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaExpApp_1-out22166d75922f7ada" style="width:100%;height:400px;"></div><!--/html_preserve-->


<p>&nbsp;</p>

### Percent check comparison to top 30 entries

The following plots allow the user to compare the top 30 entries against the different checks/benchmarks present in the dataset for each trait. The table below that shows the wide-table of all possible comparisons for all traits.


<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaExpApp_1-checkMta-label" for="mtaExpApp_1-checkMta">Check to compare to:</label>
<div>
<select id="mtaExpApp_1-checkMta" class="shiny-input-select"><option value="IR16A3838" selected>IR16A3838</option>
<option value="IR16A3891">IR16A3891</option>
<option value="IR16A4085">IR16A4085</option>
<option value="IR16A4261">IR16A4261</option>
<option value="IRRI 104">IRRI 104</option>
<option value="IRRI 123">IRRI 123</option>
<option value="IRRI 154">IRRI 154</option>
<option value="IRRI 156">IRRI 156</option>
<option value="IRRI 168">IRRI 168</option>
<option value="IRRI 174">IRRI 174</option>
<option value="IRRI 211">IRRI 211</option>
<option value="IRRI 212">IRRI 212</option>
<option value="IRRI 220">IRRI 220</option>
<option value="IRRI 221">IRRI 221</option>
<option value="IRRI 223">IRRI 223</option>
<option value="IRRI 224">IRRI 224</option>
<option value="SUPA">SUPA</option>
<option value="TARI RIC 1">TARI RIC 1</option>
<option value="TARI RIC 3">TARI RIC 3</option>
<option value="TXD 306">TXD 306</option></select>
<script type="application/json" data-for="mtaExpApp_1-checkMta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaExpApp_1-outfd15b3057776d571" style="width:100%;height:400px;"></div><!--/html_preserve-->

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="mtaExpApp_1-outaee33abe9dd10fb1" style="width:100%;height:auto;"></div><!--/html_preserve-->


### Genetic correlations between traits

The following heatmap and histogram allows to see the genetic correlations among traits calculated using across environment estimates of merit for the different traits. This can be used to understand the implications of selecting for a set of traits to achieve a product profile and make neccesary adjustment to the selection strategy.

<p>&nbsp;</p>

<!--html_preserve--><div class="shiny-input-panel">
<div class="shiny-flow-layout">
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="mtaExpApp_1-envCovMtaTraits-label" for="mtaExpApp_1-envCovMtaTraits">Environment</label>
<div>
<select id="mtaExpApp_1-envCovMtaTraits" class="shiny-input-select"><option value="(Intercept)" selected>(Intercept)</option>
<option value="L.Stage2BIRU2024Wet001">L.Stage2BIRU2024Wet001</option>
<option value="L.Stage2BIBBGI2024Wet008">L.Stage2BIBBGI2024Wet008</option>
<option value="L.Stage2BIRT2024Wet001">L.Stage2BIRT2024Wet001</option>
<option value="L.Stage2TZPWKI2024Wet004">L.Stage2TZPWKI2024Wet004</option>
<option value="L.Stage2TZMODK2024Wet009">L.Stage2TZMODK2024Wet009</option></select>
<script type="application/json" data-for="mtaExpApp_1-envCovMtaTraits" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaExpApp_1-out303a0da1ad0373bf" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Predictions 

The following table allows you to inspect the trait predictions in wide format together with the QTL profile (in case those are available) to understand the type of data that would be used to calculate a selection index (e.g., desire index).

<p>&nbsp;</p>

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="mtaExpApp_1-out86154c5880eb5342" style="width:100%;height:auto;"></div><!--/html_preserve-->

### Modeling parameters

This section aims to provide the modeling table for the analysis in order to keep track of which environments were used in the analysis, what was the final model used for each trait and other potentially important parameters for future reference.

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="mtaExpApp_1-out3db18dffa6c79f55" style="width:100%;height:auto;"></div><!--/html_preserve-->


### References of methods used

Finlay, K. W., & Wilkinson, G. N. (1963). The analysis of adaptation in a plant-breeding programme. Australian journal of agricultural research, 14(6), 742-754.

Henderson Jr, C. R. (1982). Analysis of covariance in the mixed model: higher-level, nonhomogeneous, and random regressions. Biometrics, 623-640.

Odegard, J., Indahl, U., Stranden, I., & Meuwissen, T. H. (2018). Large-scale genomic prediction using singular value decomposition of the genotype matrix. Genetics Selection Evolution, 50(1), 1-12.

R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

Boer M, van Rossum B (2022). LMMsolver: Linear Mixed Model Solver. R package version 1.0.4.9000.

Covarrubias-Pazaran G. (2024). lme4breeding: enabling genetic evaluation in the era of genomic data. bioRxiv, 2024-05.

Covarrubias-Pazaran G. (2016). Genome assisted prediction of quantitative traits using the R package sommer. PLoS ONE 11(6):1-15.

<p>&nbsp;</p>


