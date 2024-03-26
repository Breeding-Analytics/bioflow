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
<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="ocsApp_1-out57616f5a68649c42" style="width:100%;height:auto;"></div><!--/html_preserve-->

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

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="ocsApp_1-out9bbb9b4fd0d0f00b" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Parent performance table

This table shows the performance of the parents selected by the OCS algorithm. The parental means are extracted from the multi-trial analysis results.

<p>&nbsp;</p>

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="ocsApp_1-out2adf9520a5c5eedd" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Treatment comparison

The following table summarizes the trait means for the different OCS runs (number of crosses and degrees treatments). 

<p>&nbsp;</p>

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="ocsApp_1-out668820662fab288a" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Cross performance per parent

The following boxplot allows you to see the performance of the different crosses predicted (dots in the boxplot) grouped by the different parents (x-axis).

<p>&nbsp;</p>
<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="ocsApp_1-traitFilterPredictions2D2-label" for="ocsApp_1-traitFilterPredictions2D2">Trait:</label>
<div>
<select id="ocsApp_1-traitFilterPredictions2D2" class="shiny-input-select" multiple="multiple"><option value="desireIndex" selected>desireIndex</option>
<option value="Yield_Mg_ha" selected>Yield_Mg_ha</option>
<option value="Plant_Height_cm" selected>Plant_Height_cm</option></select>
<script type="application/json" data-for="ocsApp_1-traitFilterPredictions2D2">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="ocsApp_1-out821be75e8f2c4278" style="width:100%;height:400px;"></div><!--/html_preserve-->



### References of methods used

Kinghorn, B. (1999). 19. Mate Selection for the tactical implementation of breeding programs. Proceedings of the Advancement of Animal Breeding and Genetics, 13, 130-133.

Woolliams, J. A., Berg, P., Dagnachew, B. S., & Meuwissen, T. H. E. (2015). Genetic contributions and their optimization. Journal of Animal Breeding and Genetics, 132(2), 89-99.

https://alphagenes.roslin.ed.ac.uk/wp/wp-content/uploads/2019/05/01_OptimalContributionSelection.pdf?x44213

R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

https://github.com/gaynorr/QuantGenResources

<p>&nbsp;</p>




