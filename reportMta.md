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

### By environment distribution

The following boxplot allows to see the distribution of predicted values (y-axis) in the different environments for each **trait**.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaApp_1-traitMta-label" for="mtaApp_1-traitMta"></label>
<div>
<select id="mtaApp_1-traitMta" class="shiny-input-select"><option value="DTF" selected>DTF</option>
<option value="GYKGPHA">GYKGPHA</option>
<option value="Zn">Zn</option></select>
<script type="application/json" data-for="mtaApp_1-traitMta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaApp_1-outc3bc4fa152b3a0bb" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Across-environment estimates

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaApp_1-traitMta2-label" for="mtaApp_1-traitMta2"></label>
<div>
<select id="mtaApp_1-traitMta2" class="shiny-input-select"><option value="DTF_QTL" selected>DTF_QTL</option>
<option value="GYKGPHA_QTL">GYKGPHA_QTL</option>
<option value="Zn_QTL">Zn_QTL</option></select>
<script type="application/json" data-for="mtaApp_1-traitMta2" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="mtaApp_1-outedb6c1c3b08a8023" style="width:100%;height:400px;"></div><!--/html_preserve-->


<p>&nbsp;</p>

### Trait by trait-sensitivity plots

The following plots allow the user to select individual not only based on the scale of the trait but also by the sensitivity of the germplasm to the environmental covariates. These plots are only constructed if the MET model included the GxE in the modeling.

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaApp_1-traitMta3-label" for="mtaApp_1-traitMta3"></label>
<div>
<select id="mtaApp_1-traitMta3" class="shiny-input-select"><option value="DTF" selected>DTF</option>
<option value="GYKGPHA">GYKGPHA</option>
<option value="Zn">Zn</option></select>
<script type="application/json" data-for="mtaApp_1-traitMta3" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="mtaApp_1-outdd0d0838afba27d4" style="width:100%;height:400px;"></div><!--/html_preserve-->


<p>&nbsp;</p>

### Genetic correlations between environments

The following graph allows to see the genetic correlations among the different environment one **trait** at a time. If the user modeled GxE, this matrix is calculated as the correlation between the environment-specific estimates for individuals. If a pure main-effect model is specified this matrix is calculated as the correlation between the single-trial analysis estimates.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaApp_1-traitPredictionsCorrelation-label" for="mtaApp_1-traitPredictionsCorrelation"></label>
<div>
<select id="mtaApp_1-traitPredictionsCorrelation" class="shiny-input-select"><option value="DTF" selected>DTF</option>
<option value="GYKGPHA">GYKGPHA</option>
<option value="Zn">Zn</option></select>
<script type="application/json" data-for="mtaApp_1-traitPredictionsCorrelation" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->


<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaApp_1-out37608a0d5384ce4a" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

## Genetic correlations between traits

The following graph allows to see the genetic correlations among traits using across environment estimates.

<p>&nbsp;</p>

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaApp_1-out9c48dcaffd7fe663" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

## Biplot by trait

The following graph allows to see the performance of the genotypes over the enevironments for each **trait**.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaApp_1-traitBiplot-label" for="mtaApp_1-traitBiplot"></label>
<div>
<select id="mtaApp_1-traitBiplot" class="shiny-input-select"><option value="DTF" selected>DTF</option>
<option value="GYKGPHA">GYKGPHA</option>
<option value="Zn">Zn</option></select>
<script type="application/json" data-for="mtaApp_1-traitBiplot" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaApp_1-outefcfd731fab4ee67" style="width:100%;height:400px;"></div><!--/html_preserve-->


<p>&nbsp;</p>

### Predictions 

The following table allows to check the trait predictions in wide format together with the QTL profile in case those are available.

<p>&nbsp;</p>

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="mtaApp_1-out91df653566b701aa" style="width:100%;height:auto;"></div><!--/html_preserve-->




