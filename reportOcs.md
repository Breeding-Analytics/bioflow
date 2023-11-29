---
title: "Optimal Cross Selection Report"
author: ""
date: "December 2023"
output: html_document
runtime: shiny
---




### Cross performance table

The following table provides an overview of the performance of the predicted crosses for the different traits. There should be as many crosses as requested in the interface. If you requested different number of crosses or degrees, the treatment column allows you to visualize the results from different runs.

<p>&nbsp;</p>

The trait used for OCS was:  YLD_TON<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item-overflow-hidden html-fill-item" id="ocsApp_1-out20e64696c79beddb" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Number of crosses per parent

The following barplot allows you identify the number of times (y-axis) that a parent (x-axis) is suggested to be used in the crossing block for a given treatment (number of crosses by degrees).

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="ocsApp_1-environ-label" for="ocsApp_1-environ">Treatment:</label>
<div>
<select id="ocsApp_1-environ" class="shiny-input-select" multiple="multiple"><option value="YLD_TON ~ 20 crosses * 30 degrees" selected>YLD_TON ~ 20 crosses * 30 degrees</option>
<option value="YLD_TON ~ 20 crosses * 60 degrees" selected>YLD_TON ~ 20 crosses * 60 degrees</option>
<option value="YLD_TON ~ 20 crosses * 90 degrees" selected>YLD_TON ~ 20 crosses * 90 degrees</option></select>
<script type="application/json" data-for="ocsApp_1-environ">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item-overflow-hidden html-fill-item" id="ocsApp_1-out4c9d8eafb7c49714" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Parent performance table

This table shows the performance of the parents selected by the OCS algorithm. The parental means are extracted from the multi-trial analysis results.

<p>&nbsp;</p>

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item-overflow-hidden html-fill-item" id="ocsApp_1-out73fd369bc87fc40f" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Treatment comparison

The following table summarizes the trait means for the different OCS runs (number of crosses and degrees treatments). 

<p>&nbsp;</p>

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item-overflow-hidden html-fill-item" id="ocsApp_1-out33d4ef25dc133138" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Cross performance per parent

The following boxplot allows you to see the performance of the different crosses predicted (dots in the boxplot) grouped by the different parents (x-axis).

<p>&nbsp;</p>
<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="ocsApp_1-traitFilterPredictions2D2-label" for="ocsApp_1-traitFilterPredictions2D2">Trait:</label>
<div>
<select id="ocsApp_1-traitFilterPredictions2D2" class="shiny-input-select" multiple="multiple"><option value="YLD_TON" selected>YLD_TON</option>
<option value="HT_AVG" selected>HT_AVG</option></select>
<script type="application/json" data-for="ocsApp_1-traitFilterPredictions2D2">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item-overflow-hidden html-fill-item" id="ocsApp_1-out49a995be1de598dd" style="width:100%;height:400px;"></div><!--/html_preserve-->






