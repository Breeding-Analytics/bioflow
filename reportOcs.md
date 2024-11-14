---
title: "Optimal Cross Selection Report"
author: "Contact:<a href = 'https://github.com/Breeding-Analytics/bioflow' target = '_blank'>Breeding Analytics Team, OneCGIAR</a> breedinganalytics@cgiar.org"
date: "November 14, 2024"  
output: html_document
params:
 toDownload: FALSE
---








### Objectives of Optimal Cross Selection

The objective of this dashboard is to help scientist to understand the following points:

1. Optimal crosses for the trade off between performance and genetic variance specified

2. Expected performance of crosses by parent

3. Performance of parents selected to go into the crossing block

4. Number of crosses proposed per parent selected to go into the crossing block

5. Difference in mean and variance observed for different degree runs

Understanding these data features should allow the scientist to identify which crosses should be performed in the crossing block. It should also allow the scientist to assess the relationship between the degree selected (aggressiveness) and the number of parents selected and times that each parent shows up.  

Keep in mind that the suggested crosses may over-represent some parents depending on the degrees selected. Some users tend to ask if they can constraint the number of times a parent should be used. Although this is possible, this implies that the user is looking for a lower sub-optimal solution since the specification of that constraint implies that the user is looking for keeping even more variance (higher degrees). 

### Cross performance table

The following table provides an overview of the performance of the predicted/proposed crosses across different traits. There should be as many crosses as requested in the interface. If you requested different number of crosses or degrees, the treatment column allows you to visualize the results from different runs. Remember that zero degree implies more interest in gain and 90 degrees implies more interest in genetic variance.

<p>&nbsp;</p>

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="ocsApp_1-out5ef3bc9a4d4bf3d4" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Cross performance per parent

The following boxplot allows you to see the performance of the different crosses predicted/proposed (dots in the boxplot) grouped by the different parents selected to go into the crossing block (x-axis).

<p>&nbsp;</p>
<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="ocsApp_1-traitFilterPredictions2D2-label" for="ocsApp_1-traitFilterPredictions2D2">Trait:</label>
<div>
<select id="ocsApp_1-traitFilterPredictions2D2" class="shiny-input-select" multiple="multiple"><option value="desireIndex" selected>desireIndex</option>
<option value="Pollen_DAP_days" selected>Pollen_DAP_days</option>
<option value="Plant_Height_cm" selected>Plant_Height_cm</option>
<option value="Yield_Mg_ha" selected>Yield_Mg_ha</option></select>
<script type="application/json" data-for="ocsApp_1-traitFilterPredictions2D2">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="ocsApp_1-out01af364323789bb1" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Parent performance table

This table shows the performance of the parents selected by the OCS algorithm. The parental means are extracted from the multi-trial analysis results.

<p>&nbsp;</p>

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="ocsApp_1-out04a6a1f247c03bc7" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Number of crosses per parent

The following barplot allows you to assess the number of times (y-axis) that a given parent (x-axis) is suggested to be used in the crossing block for certain OCS treatment (number of crosses by degrees specifications).

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="ocsApp_1-environ-label" for="ocsApp_1-environ">Treatment:</label>
<div>
<select id="ocsApp_1-environ" class="shiny-input-select" multiple="multiple"><option value="desireIndex ~ 70 crosses * 30 degrees" selected>desireIndex ~ 70 crosses * 30 degrees</option></select>
<script type="application/json" data-for="ocsApp_1-environ">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="ocsApp_1-outd8c7ca46bdc7db83" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Treatment comparison

The following table summarizes the trait means for the different OCS runs (number of crosses and degrees treatments) in case you want to compare what would have been the resulting means and variances under different degrees or number of crosses. 

<p>&nbsp;</p>

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="ocsApp_1-out51d28e23c3e96301" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### References of methods used

Kinghorn, B. (1999). 19. Mate Selection for the tactical implementation of breeding programs. Proceedings of the Advancement of Animal Breeding and Genetics, 13, 130-133.

Woolliams, J. A., Berg, P., Dagnachew, B. S., & Meuwissen, T. H. E. (2015). Genetic contributions and their optimization. Journal of Animal Breeding and Genetics, 132(2), 89-99.

https://alphagenes.roslin.ed.ac.uk/wp/wp-content/uploads/2019/05/01_OptimalContributionSelection.pdf?x44213

R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

https://github.com/gaynorr/QuantGenResources

<p>&nbsp;</p>




