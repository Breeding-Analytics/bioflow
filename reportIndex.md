---
title: "Selection Index Report"
author: "Contact:<a href = 'https://github.com/Breeding-Analytics/bioflow' target = '_blank'>Breeding Analytics Team, OneCGIAR</a> breedinganalytics@cgiar.org"
date: "October 10, 2024"  
output: html_document
params:
 toDownload: FALSE
---









### Objectives of a Selection Index

The objective of this dashboard is to help scientist to understand the following points:

1. Individual across environment predictions for each trait (input) 

2. Individual sensitivity values from the Finlay-Wilkinson model (Finlay & Wilkinson, 1963) (input)

3. Relative distance between the desired values (target product profiles) and the population means (output)

4. Expected response to selection in each trait after using the index (output)

5. Relationship between the index with the input traits (output)

Understanding these data features should allow the scientist to identify which traits are driving the desire selection index (Pesek & Baker, 1969) and understand what is the approximate expected change of the population in the next generation. We would recommend to use the index as the input trait for an optimal contribution selection algorithm.

### Across-environment trait table

The following table allows you to inspect the across-environment predictions for the different entries (rows) and different traits (columns) that were used as input to calculate the selection index.

<p>&nbsp;</p>

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="indexDesireApp_1-out9c344c8dc13a3548" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Modeling table

The following table aims to keep record of the desire values selected and corresponding weights for this run.

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="indexDesireApp_1-out71bfec70500a54c2" style="width:100%;height:auto;"></div><!--/html_preserve-->


### Radar Plot

This spider plot allows you to visualize the distance between the population means and the target values (hopefully coming from your product profile).

<p>&nbsp;</p>

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="indexDesireApp_1-out5574ab6530e728e2" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Expected Response Plot

The following plot allows the user to visualize the expected response in the new generation given the individuals selected (different proportions can be investigated) using the selection index.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="indexDesireApp_1-proportionTrait-label" for="indexDesireApp_1-proportionTrait">Selected proportion</label>
<input id="indexDesireApp_1-proportionTrait" type="number" class="shiny-input-number form-control" value="0.1" min="0.001" max="1" step="0.05"/>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="indexDesireApp_1-out58b1a67119c37ef4" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Index versus Trait Plot

The following plot allows you to visualize the relationship between the selection index with other traits. This can be specially useful for selecting materials with high total merit (index) and stability for yield (for example, for product development).

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="indexDesireApp_1-traitMtaScatter-label" for="indexDesireApp_1-traitMtaScatter">Trait:</label>
<div>
<select id="indexDesireApp_1-traitMtaScatter" class="shiny-input-select"><option value="Height" selected>Height</option>
<option value="AreaDron">AreaDron</option>
<option value="Biomass">Biomass</option></select>
<script type="application/json" data-for="indexDesireApp_1-traitMtaScatter" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="indexDesireApp_1-outd986daa29705448c" style="width:100%;height:400px;"></div><!--/html_preserve-->







### References of methods used

Pesek, J., & Baker, R. J. (1969). Desired improvement in relation to selection indices. Canadian journal of plant science, 49(6), 803-804.

Ceron-Rojas, J. J., & Crossa, J. (2018). Linear selection indices in modern plant breeding (p. 256). Springer Nature.

R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

<p>&nbsp;</p>

