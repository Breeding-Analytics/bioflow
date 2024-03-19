---
title: "MTA Report"
author: ""
date: "2023-11-03"
output: html_document
params:
  toDownload: FALSE
---








```
## NULL
```

### By environment merit distribution

The following boxplot allows to see the distribution of predicted values (y-axis) in the different environments for each **trait**. Within each boxplot the reliability (r2) for that environment by trait combination can be reviewed.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaApp_1-traitMta-label" for="mtaApp_1-traitMta"></label>
<div>
<select id="mtaApp_1-traitMta" class="shiny-input-select"><option value="YLD_TON" selected>YLD_TON</option>
<option value="FLW50">FLW50</option>
<option value="HT_AVG">HT_AVG</option></select>
<script type="application/json" data-for="mtaApp_1-traitMta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="mtaApp_1-out5fb5a355ac89fa85" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Connectivity between environments

The following heatmap allows the user to review the connectivity (gennotypes in common) between different environments.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaApp_1-traitMtaConnect-label" for="mtaApp_1-traitMtaConnect"></label>
<div>
<select id="mtaApp_1-traitMtaConnect" class="shiny-input-select"><option value="YLD_TON" selected>YLD_TON</option>
<option value="FLW50">FLW50</option>
<option value="HT_AVG">HT_AVG</option></select>
<script type="application/json" data-for="mtaApp_1-traitMtaConnect" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="mtaApp_1-out26d2f3149813e687" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Genetic correlations between environments

The following graph allows to see the genetic correlations among the different environment one **trait** at a time. If the user modeled GxE, this matrix is calculated as the correlation between the environment-specific estimates for individuals. If a pure main-effect model is specified this matrix is calculated as the correlation between the single-trial analysis estimates.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaApp_1-traitPredictionsCorrelation-label" for="mtaApp_1-traitPredictionsCorrelation"></label>
<div>
<select id="mtaApp_1-traitPredictionsCorrelation" class="shiny-input-select"><option value="YLD_TON" selected>YLD_TON</option>
<option value="FLW50">FLW50</option>
<option value="HT_AVG">HT_AVG</option></select>
<script type="application/json" data-for="mtaApp_1-traitPredictionsCorrelation" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->


<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="mtaApp_1-out326a3bd2e4713730" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Biplot by trait

The following graph allows to see the performance of the genotypes over the enevironments for each **trait**.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaApp_1-traitBiplot-label" for="mtaApp_1-traitBiplot"></label>
<div>
<select id="mtaApp_1-traitBiplot" class="shiny-input-select"><option value="YLD_TON" selected>YLD_TON</option>
<option value="FLW50">FLW50</option>
<option value="HT_AVG">HT_AVG</option></select>
<script type="application/json" data-for="mtaApp_1-traitBiplot" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaApp_1-outa1d81150d699124e" style="width:100%;height:400px;"></div><!--/html_preserve-->


<p>&nbsp;</p>

### Across-environment metrics

The following barplot aims to help you check the across environment estimates for multiple parameters from the multi-trial analysis.

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaApp_1-parameterMetrics2-label" for="mtaApp_1-parameterMetrics2">Parameter:</label>
<div>
<select id="mtaApp_1-parameterMetrics2" class="shiny-input-select"><option value="mean" selected>mean</option>
<option value="r2" selected>r2</option>
<option value="Vg" selected>Vg</option>
<option value="Vr" selected>Vr</option></select>
<script type="application/json" data-for="mtaApp_1-parameterMetrics2" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="mtaApp_1-outce143409dc76b2ad" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>


### Variance component proportions (across environments)

The following graph allows to see the proportion of variance going to different components. Above the bars you can see the value of the variance component for each factor.

<p>&nbsp;</p>


<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="mtaApp_1-outfca600c01358fee6" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Across-environment merit estimates

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaApp_1-traitMta2-label" for="mtaApp_1-traitMta2"></label>
<div>
<select id="mtaApp_1-traitMta2" class="shiny-input-select"><option value="YLD_TON" selected>YLD_TON</option>
<option value="HT_AVG">HT_AVG</option>
<option value="FLW50">FLW50</option></select>
<script type="application/json" data-for="mtaApp_1-traitMta2" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="mtaApp_1-out392e3273224edf0c" style="width:100%;height:400px;"></div><!--/html_preserve-->


<p>&nbsp;</p>










<p>&nbsp;</p>

### Genetic correlations between traits

The following graph allows to see the genetic correlations among traits using across environment estimates.

<p>&nbsp;</p>

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaApp_1-out931a8f68a63ef3c1" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Predictions 

The following table allows to check the trait predictions in wide format together with the QTL profile in case those are available.

<p>&nbsp;</p>

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="mtaApp_1-outb7428b0fe813ef2f" style="width:100%;height:auto;"></div><!--/html_preserve-->




