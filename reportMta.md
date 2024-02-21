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
<select id="mtaApp_1-traitMta" class="shiny-input-select"><option value="Ear_Height_cm" selected>Ear_Height_cm</option>
<option value="Plant_Height_cm">Plant_Height_cm</option></select>
<script type="application/json" data-for="mtaApp_1-traitMta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaApp_1-out8dc8a19d9bf6dfb4" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Across-environment estimates

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaApp_1-traitMta2-label" for="mtaApp_1-traitMta2"></label>
<div>
<select id="mtaApp_1-traitMta2" class="shiny-input-select"><option value="Ear_Height_cm" selected>Ear_Height_cm</option>
<option value="Plant_Height_cm">Plant_Height_cm</option></select>
<script type="application/json" data-for="mtaApp_1-traitMta2" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="mtaApp_1-out501e8f680d8dfaf9" style="width:100%;height:400px;"></div><!--/html_preserve-->


<p>&nbsp;</p>

### Trait by trait-sensitivity plots

The following plots allow the user to select individual not only based on the scale of the trait but also by the sensitivity of the germplasm to the environmental covariates. These plts are only constructed if the MET model included the GxE in the modeling.

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaApp_1-traitMta3-label" for="mtaApp_1-traitMta3"></label>
<div>
<select id="mtaApp_1-traitMta3" class="shiny-input-select"><option value="Ear_Height_cm" selected>Ear_Height_cm</option>
<option value="Plant_Height_cm">Plant_Height_cm</option></select>
<script type="application/json" data-for="mtaApp_1-traitMta3" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="mtaApp_1-out8c98e7656d4c9311" style="width:100%;height:400px;"></div><!--/html_preserve-->


<p>&nbsp;</p>

### Genetic correlations between environments

The following graph allows to see the genetic correlations of the different environment for each **trait**.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaApp_1-traitPredictionsCorrelation-label" for="mtaApp_1-traitPredictionsCorrelation"></label>
<div>
<select id="mtaApp_1-traitPredictionsCorrelation" class="shiny-input-select"><option value="Ear_Height_cm" selected>Ear_Height_cm</option>
<option value="Plant_Height_cm">Plant_Height_cm</option></select>
<script type="application/json" data-for="mtaApp_1-traitPredictionsCorrelation" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->


<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaApp_1-outd6f18cc603b09f3b" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

## Genetic correlations between traits

The following graph allows to see the genetic correlations among traits using across environment estimates.

<p>&nbsp;</p>

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaApp_1-out05e8160d28f3395a" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

## Biplot by trait

The following graph allows to see the performance of the genotypes over the enevironments for each **trait**.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaApp_1-traitBiplot-label" for="mtaApp_1-traitBiplot"></label>
<div>
<select id="mtaApp_1-traitBiplot" class="shiny-input-select"><option value="Ear_Height_cm" selected>Ear_Height_cm</option>
<option value="Plant_Height_cm">Plant_Height_cm</option></select>
<script type="application/json" data-for="mtaApp_1-traitBiplot" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaApp_1-out5e514a8638f79315" style="width:100%;height:400px;"></div><!--/html_preserve-->


<p>&nbsp;</p>

### Predictions 

The following table allows to check the trait predictions in wide format together with the QTL profile in case those are available.

<p>&nbsp;</p>

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="mtaApp_1-outce904a03c2d34023" style="width:100%;height:auto;"></div><!--/html_preserve-->




