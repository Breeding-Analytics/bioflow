---
title: "Model-Based Outlier Detection Report"
author: "Contact:<a href = 'https://github.com/Breeding-Analytics/bioflow' target = '_blank'>Breeding Analytics Team, OneCGIAR</a> breedinganalytics@cgiar.org"
date: "October 22, 2026"  
output: html_document
params:
  toDownload: TRUE
---




<div style = "width:auto; height:auto; overflow:auto">


</div>



### Table of modifications

The following table shows the modifications that have been saved in your analysis object and that will be applied to the raw phenotypic data when this specific time stamp (ID) is selected/used in other analytical modules (e.g., single occurrence analysis module).

<!--html_preserve--><div class="shiny-input-panel">
<div class="shiny-flow-layout">
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="qaStaApp_1-traitQa-label" for="qaStaApp_1-traitQa">Trait to visualize:</label>
<div>
<select id="qaStaApp_1-traitQa" class="shiny-input-select"><option value="Ear_Height_cm" selected>Ear_Height_cm</option>
<option value="Grain_Moisture">Grain_Moisture</option></select>
<script type="application/json" data-for="qaStaApp_1-traitQa" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="qaStaApp_1-out581918126a93d089" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Diagnostic plot after modifications
After remove the outlier in te step before, you can see in the plots the final behavior of the residual test.

<!--html_preserve--><div class="shiny-input-panel">
<div class="shiny-flow-layout">
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="qaStaApp_1-traitQaResP-label" for="qaStaApp_1-traitQaResP">Trait to visualize:</label>
<div>
<select id="qaStaApp_1-traitQaResP" class="shiny-input-select"><option value="Plant_Height_cm" selected>Plant_Height_cm</option>
<option value="Ear_Height_cm">Ear_Height_cm</option>
<option value="Yield_Mg_ha">Yield_Mg_ha</option>
<option value="Grain_Moisture">Grain_Moisture</option></select>
<script type="application/json" data-for="qaStaApp_1-traitQaResP" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
<div>
<div class="col-sm-12"></div>
</div>
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="qaStaApp_1-envQaResP-label" for="qaStaApp_1-envQaResP">Environment to vizualize:</label>
<div>
<select id="qaStaApp_1-envQaResP" class="shiny-input-select"><option value="NYS1y2021" selected>NYS1y2021</option>
<option value="TXH2y2021">TXH2y2021</option></select>
<script type="application/json" data-for="qaStaApp_1-envQaResP" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
</div>
</div><!--/html_preserve-->

<div style = "width:auto; height:900px; overflow:auto">

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="qaStaApp_1-outec63565eafbdb4e1" style="width:100%;height:400px;"></div><!--/html_preserve-->
</div>

<p>&nbsp;</p>

### Outlier warnings

The following table shows a summary about a percent of outliers found it.

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="qaStaApp_1-out814a5f0b93b07af1" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### References on methods used

Tukey, J. W. (1977). Exploratory Data Analysis. Section 2C.

McGill, R., Tukey, J. W. and Larsen, W. A. (1978). Variations of box plots. The American Statistician, 32, 12–16. doi:10.2307/2683468.

Velleman, P. F. and Hoaglin, D. C. (1981). Applications, Basics and Computing of Exploratory Data Analysis. Duxbury Press.


<p>&nbsp;</p>





