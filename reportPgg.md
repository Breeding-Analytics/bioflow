---
title: "Predicted Genetic Gain Report"
author: "Contact:<a href = 'https://github.com/Breeding-Analytics/bioflow' target = '_blank'>Breeding Analytics Team, OneCGIAR</a> breedinganalytics@cgiar.org"
date: "November 07, 2024"  
output: html_document
params:
  toDownload: FALSE
---








### Summary metrics

The following table allows you to review the different metrics calculated for the genetic gain analysis. You can filter the metrics using the search bar.

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="pggApp_1-out0cc7fe54d029b2ff" style="width:100%;height:auto;"></div><!--/html_preserve-->

### Predicted genetic gain

The following density plot allows you to see the expected change in the next generation compared to the current generation given the current genetic gain parameters (accuracy, intensity, genetic variance) obtained in a particular MET for the selected traits.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="pggApp_1-traitFilterPredictions2D2-label" for="pggApp_1-traitFilterPredictions2D2">Trait:</label>
<div>
<select id="pggApp_1-traitFilterPredictions2D2" class="shiny-input-select"><option value="Pollen_DAP_days" selected>Pollen_DAP_days</option>
<option value="Plant_Height_cm">Plant_Height_cm</option>
<option value="Yield_Mg_ha">Yield_Mg_ha</option></select>
<script type="application/json" data-for="pggApp_1-traitFilterPredictions2D2" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="pggApp_1-outfb24fc1e349c8dfc" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Barplot for metrics 

The following barplot allows you to compare the parameters with respect to the classification variables.

<!--html_preserve--><div class="shiny-input-panel">
<div class="shiny-flow-layout">
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="pggApp_1-parameterMetricsBy-label" for="pggApp_1-parameterMetricsBy">Trait:</label>
<div>
<select id="pggApp_1-parameterMetricsBy" class="shiny-input-select"><option value="Pollen_DAP_days" selected>Pollen_DAP_days</option>
<option value="Plant_Height_cm">Plant_Height_cm</option>
<option value="Yield_Mg_ha">Yield_Mg_ha</option></select>
<script type="application/json" data-for="pggApp_1-parameterMetricsBy" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="pggApp_1-parameterMetrics-label" for="pggApp_1-parameterMetrics">Parameter to filter:</label>
<div>
<select id="pggApp_1-parameterMetrics" class="shiny-input-select"><option value="r" selected>r</option>
<option value="r2">r2</option>
<option value="sigmaG">sigmaG</option>
<option value="meanG">meanG</option>
<option value="max.G">max.G</option>
<option value="cycleLength">cycleLength</option>
<option value="i">i</option>
<option value="R">R</option>
<option value="PGG">PGG</option>
<option value="nEnvs">nEnvs</option>
<option value="nInds">nInds</option>
<option value="nIndsSel">nIndsSel</option></select>
<script type="application/json" data-for="pggApp_1-parameterMetrics" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
</div>
</div><!--/html_preserve-->
<p>&nbsp;</p>

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="pggApp_1-out318124851661e276" style="width:100%;height:400px;"></div><!--/html_preserve-->

### References of methods used

Lush, J. L. (2013). Animal breeding plans. Read Books Ltd.

Mrode, R. A. (2014). Linear models for the prediction of animal breeding values. Cabi.

R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

<p>&nbsp;</p>


