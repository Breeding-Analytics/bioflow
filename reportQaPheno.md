---
title: "Quality Assurance Phenotypic Report"
author: "Contact:<a href = 'https://github.com/Breeding-Analytics/bioflow' target = '_blank'>Breeding Analytics Team, OneCGIAR</a> breedinganalytics@cgiar.org"
date: "August 19, 2024"  
output: html_document
params:
  toDownload: FALSE
---








### Table of modifications
<p>&nbsp;</p>

The following table shows the modifications that have been saved in your analysis object and that will be applied to the raw phenotypic data when this specific time stamp (ID) is selected/used in other analytical modules (e.g., single trial analysis module).

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitQa-label" for="reportBuilder_1-traitQa">Trait:</label>
<div>
<select id="reportBuilder_1-traitQa" class="shiny-input-select"><option value="Yield_Mg_ha" selected>Yield_Mg_ha</option>
<option value="Plant_Height_cm">Plant_Height_cm</option>
<option value="Ear_Height_cm">Ear_Height_cm</option></select>
<script type="application/json" data-for="reportBuilder_1-traitQa" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->


<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="reportBuilder_1-outd9bf1e556ce4c26c" style="width:100%;height:auto;"></div><!--/html_preserve-->



<p>&nbsp;</p>

### Boxplot of modifications

The following boxplot allows you to visualize which records were tagged as outliers and will be ignored in posterior analyses if this QA time stamp is selected in the posterior analyses.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitQaBox-label" for="reportBuilder_1-traitQaBox">Trait:</label>
<div>
<select id="reportBuilder_1-traitQaBox" class="shiny-input-select"><option value="Yield_Mg_ha" selected>Yield_Mg_ha</option>
<option value="Plant_Height_cm">Plant_Height_cm</option>
<option value="Ear_Height_cm">Ear_Height_cm</option></select>
<script type="application/json" data-for="reportBuilder_1-traitQaBox" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="reportBuilder_1-outc660d1b83fecd8c1" style="width:100%;height:400px;"></div><!--/html_preserve-->



### References on methods used

Tukey, J. W. (1977). Exploratory Data Analysis. Section 2C.

McGill, R., Tukey, J. W. and Larsen, W. A. (1978). Variations of box plots. The American Statistician, 32, 12–16. doi:10.2307/2683468.

Velleman, P. F. and Hoaglin, D. C. (1981). Applications, Basics and Computing of Exploratory Data Analysis. Duxbury Press.


<p>&nbsp;</p>





