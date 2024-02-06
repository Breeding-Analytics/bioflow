---
title: "Optimal Cross Selection Report"
author: ""
date: "December 2023"
output: html_document
params:
 toDownload: FALSE
---








### Cross performance table

The following table provides an overview of the performance of the predicted crosses for the different traits. There should be as many crosses as requested in the interface. If you requested different number of crosses or degrees, the treatment column allows you to visualize the results from different runs.

<p>&nbsp;</p>

NULL
The trait used for OCS was:  desireIndex<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="ocsApp_1-out038dc1d2815e8503" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Number of crosses per parent

The following barplot allows you identify the number of times (y-axis) that a parent (x-axis) is suggested to be used in the crossing block for a given treatment (number of crosses by degrees).

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="ocsApp_1-environ-label" for="ocsApp_1-environ">Treatment:</label>
<div>
<select id="ocsApp_1-environ" class="shiny-input-select" multiple="multiple"><option value="desireIndex ~ 20 crosses * 30 degrees" selected>desireIndex ~ 20 crosses * 30 degrees</option></select>
<script type="application/json" data-for="ocsApp_1-environ">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="ocsApp_1-outfbb08c455de8c1c5" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Parent performance table

This table shows the performance of the parents selected by the OCS algorithm. The parental means are extracted from the multi-trial analysis results.

<p>&nbsp;</p>

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="ocsApp_1-out687eb97d062df0df" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Treatment comparison

The following table summarizes the trait means for the different OCS runs (number of crosses and degrees treatments). 

<p>&nbsp;</p>

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="ocsApp_1-oute20e0b77a96a55fc" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Cross performance per parent

The following boxplot allows you to see the performance of the different crosses predicted (dots in the boxplot) grouped by the different parents (x-axis).

<p>&nbsp;</p>
<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="ocsApp_1-traitFilterPredictions2D2-label" for="ocsApp_1-traitFilterPredictions2D2">Trait:</label>
<div>
<select id="ocsApp_1-traitFilterPredictions2D2" class="shiny-input-select" multiple="multiple"><option value="desireIndex" selected>desireIndex</option></select>
<script type="application/json" data-for="ocsApp_1-traitFilterPredictions2D2">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="ocsApp_1-out7d672e8756a7decc" style="width:100%;height:400px;"></div><!--/html_preserve-->






