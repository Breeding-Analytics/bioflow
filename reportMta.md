---
title: "Multi-Trial Analysis Report"
author: "Contact:<a href = 'https://github.com/Breeding-Analytics/bioflow' target = '_blank'>Breeding Analytics Team, OneCGIAR</a> breedinganalytics@cgiar.org"
date: "April 24, 2024"  
output: html_document
params:
  toDownload: FALSE
---









### Number of individuals per trait and entry type

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="reportBuilder_1-out0eccd3a245071f2a" style="width:100%;height:auto;"></div><!--/html_preserve-->

### By environment merit distribution

The following boxplot allows to see the distribution of predicted values (y-axis) in the different environments for each **trait**. At the top of each boxplot the reliability (r2) for that environment by trait combination can be reviewed.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitMta-label" for="reportBuilder_1-traitMta"></label>
<div>
<select id="reportBuilder_1-traitMta" class="shiny-input-select"><option value="YLDTONHA" selected>YLDTONHA</option>
<option value="HT_AVG">HT_AVG</option>
<option value="FLW50">FLW50</option>
<option value="YLD_TON">YLD_TON</option>
<option value="ZINC">ZINC</option>
<option value="PERCENT_HEAD_RICE">PERCENT_HEAD_RICE</option></select>
<script type="application/json" data-for="reportBuilder_1-traitMta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="reportBuilder_1-outc8a06895002c7483" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Connectivity between environments

The following heatmap allows the user to review the connectivity (gennotypes in common) between different environments.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitMtaConnect-label" for="reportBuilder_1-traitMtaConnect"></label>
<div>
<select id="reportBuilder_1-traitMtaConnect" class="shiny-input-select"><option value="YLDTONHA" selected>YLDTONHA</option>
<option value="HT_AVG">HT_AVG</option>
<option value="FLW50">FLW50</option>
<option value="YLD_TON">YLD_TON</option>
<option value="ZINC">ZINC</option>
<option value="PERCENT_HEAD_RICE">PERCENT_HEAD_RICE</option></select>
<script type="application/json" data-for="reportBuilder_1-traitMtaConnect" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-outbcba85db399cbf61" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Genetic correlations between environments

The following graph allows to see the genetic correlations among the different environment one **trait** at a time. If the user modeled GxE, this matrix is calculated as the correlation between the environment-specific estimates for individuals. If a pure main-effect model is specified this matrix is calculated as the correlation between the single-trial analysis estimates.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitPredictionsCorrelation-label" for="reportBuilder_1-traitPredictionsCorrelation"></label>
<div>
<select id="reportBuilder_1-traitPredictionsCorrelation" class="shiny-input-select"><option value="YLDTONHA" selected>YLDTONHA</option>
<option value="HT_AVG">HT_AVG</option>
<option value="FLW50">FLW50</option>
<option value="YLD_TON">YLD_TON</option>
<option value="ZINC">ZINC</option>
<option value="PERCENT_HEAD_RICE">PERCENT_HEAD_RICE</option></select>
<script type="application/json" data-for="reportBuilder_1-traitPredictionsCorrelation" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->


<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-out30a645373552ba6d" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Biplot by trait

The following graph allows to see the performance of the genotypes over the enevironments for each **trait**.


<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitBiplot-label" for="reportBuilder_1-traitBiplot"></label>
<div>
<select id="reportBuilder_1-traitBiplot" class="shiny-input-select"><option value="YLDTONHA" selected>YLDTONHA</option>
<option value="HT_AVG">HT_AVG</option>
<option value="FLW50">FLW50</option>
<option value="YLD_TON">YLD_TON</option></select>
<script type="application/json" data-for="reportBuilder_1-traitBiplot" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-out1312a8cf0abb8679" style="width:100%;height:400px;"></div><!--/html_preserve-->


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

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="reportBuilder_1-outf518689fac53663e" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>


### Variance component proportions for traits (across environments)

The following graph allows to see the proportion of variance going to different components. Above the bars you can see the value of the variance component for each factor.

<p>&nbsp;</p>


<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="reportBuilder_1-outf334dd62965a9870" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Across-environment merit estimates of top entries

In the following plot you can observe the comparison between the top 100 entries from each entry type category. If a category has less than a 100 entries all individuals are displayed.

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitMta2-label" for="reportBuilder_1-traitMta2"></label>
<div>
<select id="reportBuilder_1-traitMta2" class="shiny-input-select"><option value="YLD_TON" selected>YLD_TON</option>
<option value="HT_AVG">HT_AVG</option>
<option value="YLDTONHA">YLDTONHA</option>
<option value="FLW50">FLW50</option>
<option value="PERCENT_HEAD_RICE">PERCENT_HEAD_RICE</option>
<option value="ZINC">ZINC</option></select>
<script type="application/json" data-for="reportBuilder_1-traitMta2" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-outbfeff0a7f031e2d1" style="width:100%;height:400px;"></div><!--/html_preserve-->


<p>&nbsp;</p>










<p>&nbsp;</p>

### Percent check comparison to top 30 entries

The following plots allow the user to compare the tested entries versus the checks.


<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-checkMta-label" for="reportBuilder_1-checkMta">Check to compare to:</label>
<div>
<select id="reportBuilder_1-checkMta" class="shiny-input-select"><option value="ABHISHEK" selected>ABHISHEK</option>
<option value="CO 51">CO 51</option>
<option value="CR DHAN 320">CR DHAN 320</option>
<option value="CR dhan 415">CR dhan 415</option>
<option value="DRR dhan 42">DRR dhan 42</option>
<option value="DRR Dhan 44">DRR Dhan 44</option>
<option value="GONTRA BIDHAN-3">GONTRA BIDHAN-3</option>
<option value="HAZARIDHAN">HAZARIDHAN</option>
<option value="Indira Aerobic-1">Indira Aerobic-1</option>
<option value="IR 64">IR 64</option>
<option value="IR16A3838">IR16A3838</option>
<option value="IR16A3891">IR16A3891</option>
<option value="IR16A4085">IR16A4085</option>
<option value="IR16A4261">IR16A4261</option>
<option value="IRRI 104">IRRI 104</option>
<option value="IRRI 123">IRRI 123</option>
<option value="IRRI 148">IRRI 148</option>
<option value="IRRI 154">IRRI 154</option>
<option value="IRRI 156">IRRI 156</option>
<option value="IRRI 163">IRRI 163</option>
<option value="IRRI 168">IRRI 168</option>
<option value="IRRI 174">IRRI 174</option>
<option value="IRRI 254">IRRI 254</option>
<option value="IRRI 264">IRRI 264</option>
<option value="LALAT">LALAT</option>
<option value="MTU 1010">MTU 1010</option>
<option value="R-RF-105">R-RF-105</option>
<option value="R-RF-127">R-RF-127</option>
<option value="SAHBHAGI DHAN">SAHBHAGI DHAN</option>
<option value="SAMBHA MAHSURI">SAMBHA MAHSURI</option>
<option value="SHUATS DHAN-1">SHUATS DHAN-1</option>
<option value="SHUATS DHAN-2">SHUATS DHAN-2</option>
<option value="SHUATS DHAN-3">SHUATS DHAN-3</option>
<option value="SHUATS DHAN-4">SHUATS DHAN-4</option>
<option value="SWARNA">SWARNA</option>
<option value="Vandana">Vandana</option></select>
<script type="application/json" data-for="reportBuilder_1-checkMta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-out8fb0acf88667f782" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Genetic correlations between traits

The following graph allows to see the genetic correlations among traits using across environment estimates.

<p>&nbsp;</p>

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="reportBuilder_1-outd4bc0659b4c6b14f" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Predictions 

The following table allows to check the trait predictions in wide format together with the QTL profile in case those are available.

<p>&nbsp;</p>

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="reportBuilder_1-outbba493a3aafceebb" style="width:100%;height:auto;"></div><!--/html_preserve-->


### References of methods used

Finlay, K. W., & Wilkinson, G. N. (1963). The analysis of adaptation in a plant-breeding programme. Australian journal of agricultural research, 14(6), 742-754.

Henderson Jr, C. R. (1982). Analysis of covariance in the mixed model: higher-level, nonhomogeneous, and random regressions. Biometrics, 623-640.

Odegard, J., Indahl, U., Stranden, I., & Meuwissen, T. H. (2018). Large-scale genomic prediction using singular value decomposition of the genotype matrix. Genetics Selection Evolution, 50(1), 1-12.

R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

Boer M, van Rossum B (2022). LMMsolver: Linear Mixed Model Solver. R package version 1.0.4.9000.

Covarrubias-Pazaran G. 2016. Genome assisted prediction of quantitative traits using the R package sommer. PLoS ONE 11(6):1-15.

<p>&nbsp;</p>


