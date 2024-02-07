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

### Trait distribution by environment

The following boxplot allows to see the distribution of predicted values (y-axis) in the different environments for each **trait**.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaApp_1-traitMta-label" for="mtaApp_1-traitMta"></label>
<div>
<select id="mtaApp_1-traitMta" class="shiny-input-select"><option value="GY" selected>GY</option></select>
<script type="application/json" data-for="mtaApp_1-traitMta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaApp_1-outac3d789e8fc9196c" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Trait distribution across environment

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaApp_1-traitMta2-label" for="mtaApp_1-traitMta2"></label>
<div>
<select id="mtaApp_1-traitMta2" class="shiny-input-select"><option value="GY_QTL" selected>GY_QTL</option></select>
<script type="application/json" data-for="mtaApp_1-traitMta2" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaApp_1-out29a344fc750d7fa5" style="width:100%;height:400px;"></div><!--/html_preserve-->


<p>&nbsp;</p>

### Genetic correlations between environments

The following graph allows to see the genetic correlations of the different environment for each **trait**.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaApp_1-traitPredictionsCorrelation-label" for="mtaApp_1-traitPredictionsCorrelation"></label>
<div>
<select id="mtaApp_1-traitPredictionsCorrelation" class="shiny-input-select"><option value="GY" selected>GY</option></select>
<script type="application/json" data-for="mtaApp_1-traitPredictionsCorrelation" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->


<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaApp_1-outccab9b9ab54b3ea9" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

## Genetic correlations between traits

The following graph allows to see the genetic correlations among traits using across environment estimates.

<p>&nbsp;</p>

Only one trait fitted. Skipping correlation plot.

<p>&nbsp;</p>

## Biplot by trait

The following graph allows to see the performance of the genotypes over the enevironments for each **trait**.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaApp_1-traitBiplot-label" for="mtaApp_1-traitBiplot"></label>
<div>
<select id="mtaApp_1-traitBiplot" class="shiny-input-select"><option value="GY" selected>GY</option></select>
<script type="application/json" data-for="mtaApp_1-traitBiplot" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaApp_1-outa0d01792b140934a" style="width:100%;height:400px;"></div><!--/html_preserve-->


<p>&nbsp;</p>

### Predictions 

The following table allows to check the trait predictions in wide format.

<p>&nbsp;</p>

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="mtaApp_1-out9ead163c04cc9029" style="width:100%;height:auto;"></div><!--/html_preserve-->




