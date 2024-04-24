---
title: "Quality Assurance Phenotypic Report"
author: "Contact:<a href = 'https://github.com/Breeding-Analytics/bioflow' target = '_blank'>Breeding Analytics Team, OneCGIAR</a> breedinganalytics@cgiar.org"
date: "April 24, 2024"  
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
<select id="reportBuilder_1-traitQa" class="shiny-input-select"><option value="YLD_TON" selected>YLD_TON</option>
<option value="YLDTONHA">YLDTONHA</option></select>
<script type="application/json" data-for="reportBuilder_1-traitQa" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->


<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="reportBuilder_1-outdde5f776276f4756" style="width:100%;height:auto;"></div><!--/html_preserve-->



<p>&nbsp;</p>

### Boxplot of modifications

The following boxplot allows you to visualize which records were tagged as outliers and will be ignored in posterior analyses if this QA time stamp is selected in the posterior analyses.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitQaBox-label" for="reportBuilder_1-traitQaBox">Trait:</label>
<div>
<select id="reportBuilder_1-traitQaBox" class="shiny-input-select"><option value="YLD_TON" selected>YLD_TON</option>
<option value="YLDTONHA">YLDTONHA</option></select>
<script type="application/json" data-for="reportBuilder_1-traitQaBox" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="reportBuilder_1-out9a6366d663eb1ab4" style="width:100%;height:400px;"></div><!--/html_preserve-->



### References on methods used

Tukey, J. W. (1977). Exploratory Data Analysis. Section 2C.

McGill, R., Tukey, J. W. and Larsen, W. A. (1978). Variations of box plots. The American Statistician, 32, 12–16. doi:10.2307/2683468.

Velleman, P. F. and Hoaglin, D. C. (1981). Applications, Basics and Computing of Exploratory Data Analysis. Duxbury Press.


<p>&nbsp;</p>





