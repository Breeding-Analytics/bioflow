---
title: "MTA Report"
author: ""
date: "2023-11-03"
output: html_document
runtime: shiny
---






### Distribution by trait

The following boxplot allows to see the distribution of predicted values (y-axis) in the different environments for each **trait**.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaApp_1-traitMta-label" for="mtaApp_1-traitMta"></label>
<div>
<select id="mtaApp_1-traitMta" class="shiny-input-select"><option value="FLW50" selected>FLW50</option>
<option value="HT_AVG">HT_AVG</option></select>
<script type="application/json" data-for="mtaApp_1-traitMta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaApp_1-outa9704b71942a2621" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Environmental correlations

The following graph allows to see the genetic correlations of the different environment for each **trait**.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaApp_1-traitPredictionsCorrelation-label" for="mtaApp_1-traitPredictionsCorrelation"></label>
<div>
<select id="mtaApp_1-traitPredictionsCorrelation" class="shiny-input-select"><option value="FLW50" selected>FLW50</option>
<option value="HT_AVG">HT_AVG</option></select>
<script type="application/json" data-for="mtaApp_1-traitPredictionsCorrelation" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->


<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaApp_1-out8f52e9bf926296c1" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

## Trait correlations

The following graph allows to see the genetic correlations of the different traits for across environments.

<p>&nbsp;</p>

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaApp_1-outb0cafc2e98e75f3b" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

## Biplot by trait

The following graph allows to see the performance of the genotypes over the enevironments for each **trait**.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaApp_1-traitBiplot-label" for="mtaApp_1-traitBiplot"></label>
<div>
<select id="mtaApp_1-traitBiplot" class="shiny-input-select"><option value="FLW50" selected>FLW50</option>
<option value="HT_AVG">HT_AVG</option></select>
<script type="application/json" data-for="mtaApp_1-traitBiplot" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaApp_1-out63d860e2d79794dc" style="width:100%;height:400px;"></div><!--/html_preserve-->


<p>&nbsp;</p>

### Predictions 

The following table allows to check the trait predictions in wide format.

<p>&nbsp;</p>

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="mtaApp_1-out031afc2f86440eb3" style="width:100%;height:auto;"></div><!--/html_preserve-->




