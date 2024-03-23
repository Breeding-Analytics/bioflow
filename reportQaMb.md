---
title: "Quality Assurance Mb Report"
author: ""
date: "December 2023"
output: html_document
params:
  toDownload: FALSE
---






```
NULL
```


### Modifications table
<p>&nbsp;</p>

The following table shows the modifications saved to be applied for the raw phenotypic data when is used in one of the analytical modules.

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="qaStaApp_1-traitQaMb-label" for="qaStaApp_1-traitQaMb">Trait:</label>
<div>
<select id="qaStaApp_1-traitQaMb" class="shiny-input-select"><option value="Ear_Height_cm-residual" selected>Ear_Height_cm-residual</option>
<option value="Yield_Mg_ha-residual">Yield_Mg_ha-residual</option></select>
<script type="application/json" data-for="qaStaApp_1-traitQaMb" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="qaStaApp_1-out1492353285ace3e2" style="width:100%;height:auto;"></div><!--/html_preserve-->


<p>&nbsp;</p>

### Modifications plot

The following plot allows you to visualize which records were tagged as outliers and will be ignored in posterior analyses if this QA time stamp is added to the posterior analyses.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="qaStaApp_1-traitQaMbBox-label" for="qaStaApp_1-traitQaMbBox">Trait:</label>
<div>
<select id="qaStaApp_1-traitQaMbBox" class="shiny-input-select"><option value="Ear_Height_cm-residual" selected>Ear_Height_cm-residual</option>
<option value="Yield_Mg_ha-residual">Yield_Mg_ha-residual</option></select>
<script type="application/json" data-for="qaStaApp_1-traitQaMbBox" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->



<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="qaStaApp_1-out9f8608313e15c1de" style="width:100%;height:400px;"></div><!--/html_preserve-->



### References on methods used

Tukey, J. W. (1977). Exploratory Data Analysis. Section 2C.

McGill, R., Tukey, J. W. and Larsen, W. A. (1978). Variations of box plots. The American Statistician, 32, 12–16. doi:10.2307/2683468.

Velleman, P. F. and Hoaglin, D. C. (1981). Applications, Basics and Computing of Exploratory Data Analysis. Duxbury Press.

<p>&nbsp;</p>
