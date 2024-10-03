---
title: "Predicted Genetic Gain Report"
author: "Contact:<a href = 'https://github.com/Breeding-Analytics/bioflow' target = '_blank'>Breeding Analytics Team, OneCGIAR</a> breedinganalytics@cgiar.org"
date: "October 03, 2024"  
output: html_document
params:
 toDownload: FALSE
---








### Summary metrics

The following table allows you to review the different metrics calculated for the genetic gain analysis. You can filter the metrics using the search bar.

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="reportBuilder_1-out5c5b357cfcc6b5cc" style="width:100%;height:auto;"></div><!--/html_preserve-->

### Predicted genetic gain

The following density plot allows you to see the expected change in the next generation compared to the current generation given the current genetic gain parameters (accuracy, intensity, genetic variance) obtained in a particular MET for the selected traits.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitFilterPredictions2D2-label" for="reportBuilder_1-traitFilterPredictions2D2">Trait:</label>
<div>
<select id="reportBuilder_1-traitFilterPredictions2D2" class="shiny-input-select"><option value="Yield_Mg_ha" selected>Yield_Mg_ha</option>
<option value="Ear_Height_cm">Ear_Height_cm</option></select>
<script type="application/json" data-for="reportBuilder_1-traitFilterPredictions2D2" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-out6020813ea698ce9a" style="width:100%;height:400px;"></div><!--/html_preserve-->




### References of methods used

Lush, J. L. (2013). Animal breeding plans. Read Books Ltd.

Mrode, R. A. (2014). Linear models for the prediction of animal breeding values. Cabi.

R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

<p>&nbsp;</p>


