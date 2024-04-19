---
title: "Multi-Trial Analysis Report"
author: ""
date: "2023-11-03"
output: html_document
params:
  toDownload: FALSE
---









### Number of individuals per trait and entry type

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="reportBuilder_1-outc7f51ada36cc04e1" style="width:100%;height:auto;"></div><!--/html_preserve-->

### By environment merit distribution

The following boxplot allows to see the distribution of predicted values (y-axis) in the different environments for each **trait**. At the top of each boxplot the reliability (r2) for that environment by trait combination can be reviewed.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitMta-label" for="reportBuilder_1-traitMta"></label>
<div>
<select id="reportBuilder_1-traitMta" class="shiny-input-select"><option value="Ear_Height_cm" selected>Ear_Height_cm</option></select>
<script type="application/json" data-for="reportBuilder_1-traitMta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="reportBuilder_1-out2f942799070c4431" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Connectivity between environments

The following heatmap allows the user to review the connectivity (gennotypes in common) between different environments.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitMtaConnect-label" for="reportBuilder_1-traitMtaConnect"></label>
<div>
<select id="reportBuilder_1-traitMtaConnect" class="shiny-input-select"><option value="Ear_Height_cm" selected>Ear_Height_cm</option></select>
<script type="application/json" data-for="reportBuilder_1-traitMtaConnect" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-out91cdc2d9141ba60c" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Genetic correlations between environments

The following graph allows to see the genetic correlations among the different environment one **trait** at a time. If the user modeled GxE, this matrix is calculated as the correlation between the environment-specific estimates for individuals. If a pure main-effect model is specified this matrix is calculated as the correlation between the single-trial analysis estimates.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitPredictionsCorrelation-label" for="reportBuilder_1-traitPredictionsCorrelation"></label>
<div>
<select id="reportBuilder_1-traitPredictionsCorrelation" class="shiny-input-select"><option value="Ear_Height_cm" selected>Ear_Height_cm</option></select>
<script type="application/json" data-for="reportBuilder_1-traitPredictionsCorrelation" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->


<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-out82231682be65a6e7" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Biplot by trait

The following graph allows to see the performance of the genotypes over the enevironments for each **trait**.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitBiplot-label" for="reportBuilder_1-traitBiplot"></label>
<div>
<select id="reportBuilder_1-traitBiplot" class="shiny-input-select"><option value="Ear_Height_cm" selected>Ear_Height_cm</option></select>
<script type="application/json" data-for="reportBuilder_1-traitBiplot" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-outc1cb60f85f3d112d" style="width:100%;height:400px;"></div><!--/html_preserve-->


<p>&nbsp;</p>

### Across-environment metrics

The following barplot aims to help you check the across environment estimates for multiple parameters from the multi-trial analysis.

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-parameterMetrics2-label" for="reportBuilder_1-parameterMetrics2">Parameter:</label>
<div>
<select id="reportBuilder_1-parameterMetrics2" class="shiny-input-select"><option value="mean" selected>mean</option>
<option value="r2" selected>r2</option>
<option value="Vg" selected>Vg</option>
<option value="nEnv" selected>nEnv</option>
<option value="Vr" selected>Vr</option></select>
<script type="application/json" data-for="reportBuilder_1-parameterMetrics2" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="reportBuilder_1-outbd3cd0b71f5c74b9" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>


### Variance component proportions for traits (across environments)

The following graph allows to see the proportion of variance going to different components. Above the bars you can see the value of the variance component for each factor.

<p>&nbsp;</p>


<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="reportBuilder_1-outa9529dae7c296928" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Across-environment merit estimates

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitMta2-label" for="reportBuilder_1-traitMta2"></label>
<div>
<select id="reportBuilder_1-traitMta2" class="shiny-input-select"><option value="Ear_Height_cm" selected>Ear_Height_cm</option></select>
<script type="application/json" data-for="reportBuilder_1-traitMta2" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-out3ae6988c068cecdb" style="width:100%;height:400px;"></div><!--/html_preserve-->


<p>&nbsp;</p>










<p>&nbsp;</p>







### Genetic correlations between traits

The following graph allows to see the genetic correlations among traits using across environment estimates.

<p>&nbsp;</p>

Only one trait fitted. Skipping correlation plot.

<p>&nbsp;</p>

### Predictions 

The following table allows to check the trait predictions in wide format together with the QTL profile in case those are available.

<p>&nbsp;</p>

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="reportBuilder_1-out85210191a7bdc9cf" style="width:100%;height:auto;"></div><!--/html_preserve-->


### References of methods used

Finlay, K. W., & Wilkinson, G. N. (1963). The analysis of adaptation in a plant-breeding programme. Australian journal of agricultural research, 14(6), 742-754.

Henderson Jr, C. R. (1982). Analysis of covariance in the mixed model: higher-level, nonhomogeneous, and random regressions. Biometrics, 623-640.

Odegard, J., Indahl, U., Stranden, I., & Meuwissen, T. H. (2018). Large-scale genomic prediction using singular value decomposition of the genotype matrix. Genetics Selection Evolution, 50(1), 1-12.

R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

Boer M, van Rossum B (2022). LMMsolver: Linear Mixed Model Solver. R package version 1.0.4.9000.

Covarrubias-Pazaran G. 2016. Genome assisted prediction of quantitative traits using the R package sommer. PLoS ONE 11(6):1-15.

<p>&nbsp;</p>


