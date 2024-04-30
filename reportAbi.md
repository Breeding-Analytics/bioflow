---
title: "Accelerated Breeding Initiative (ABI) Dashboard"
author: "Contact:<a href = 'https://github.com/Breeding-Analytics/bioflow' target = '_blank'>Breeding Analytics Team, OneCGIAR</a> breedinganalytics@cgiar.org"
date: "April 30, 2024"  
output: html_document
params:
 toDownload: FALSE
---









### Objectives of the ABI dashboard

The objective of this dashboard is to help leadership to understand the following points for a given pipeline:

1. Data available and used for the analysis

2. Connectivity and correlation between environments for traits phenotyped

3. Key performance indicators (KPIs) associated to the pipeline (e.g., trial heritabilities, variance components, etc.)

4. Genetic correlation between traits and distance between the desired and current population

5. Realized genetic gain associated to the pipeline

Understanding these data features should allow the leadership to understand the level of investment in a given pipeline, the quality of the data generated and the complexity of the phenotyped traits (oligogenic or polygenic, level of GxE) which may indicate how feasible is to reach a product profile. Also the realized genetic gain should allow the leadership to monitor the effectiveness of the program to increase gains and deliver better products.

### Data availability

The following visualization shows which data types was used for this pipeline. It allows to see whether the data has been QA and the summary values for the different data types.

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="abiDashboard_1-traitMta-label" for="abiDashboard_1-traitMta">Trait:</label>
<div>
<select id="abiDashboard_1-traitMta" class="shiny-input-select"><option value="Yield_Mg_ha" selected>Yield_Mg_ha</option>
<option value="Ear_Height_cm">Ear_Height_cm</option>
<option value="Plant_Height_cm">Plant_Height_cm</option></select>
<script type="application/json" data-for="abiDashboard_1-traitMta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<p>&nbsp;</p>

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="abiDashboard_1-out0a59146b36a76b96" style="width:100%;height:400px;"></div><!--/html_preserve-->


### Map of trials planted

The following map allows you to assess the location where trials are planted.


<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="abiDashboard_1-out1768607cd8a54b47" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Base metrics

The following barplot shows the value of the different parameters calculated per trial during the single trial analysis run. The view can be modified by trait and by parameter.

Variance component proportions

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="abiDashboard_1-traitSta-label" for="abiDashboard_1-traitSta">Trait:</label>
<div>
<select id="abiDashboard_1-traitSta" class="shiny-input-select"><option value="Yield_Mg_ha" selected>Yield_Mg_ha</option>
<option value="Plant_Height_cm">Plant_Height_cm</option>
<option value="Ear_Height_cm">Ear_Height_cm</option></select>
<script type="application/json" data-for="abiDashboard_1-traitSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="abiDashboard_1-out1bca79fd6c9cfd0f" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Barplot for parameter values

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="abiDashboard_1-parameterMetrics-label" for="abiDashboard_1-parameterMetrics">Parameter:</label>
<div>
<select id="abiDashboard_1-parameterMetrics" class="shiny-input-select"><option value="plotH2" selected>plotH2</option>
<option value="CV" selected>CV</option>
<option value="r2" selected>r2</option>
<option value="mean" selected>mean</option></select>
<script type="application/json" data-for="abiDashboard_1-parameterMetrics" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="abiDashboard_1-out2d020760393ecea2" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Connectivity between the environments

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="abiDashboard_1-traitMtaConnect-label" for="abiDashboard_1-traitMtaConnect"></label>
<div>
<select id="abiDashboard_1-traitMtaConnect" class="shiny-input-select"><option value="Yield_Mg_ha" selected>Yield_Mg_ha</option>
<option value="Ear_Height_cm">Ear_Height_cm</option>
<option value="Plant_Height_cm">Plant_Height_cm</option></select>
<script type="application/json" data-for="abiDashboard_1-traitMtaConnect" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="abiDashboard_1-oute071bbdce70e4a6c" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Correlation between environments

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="abiDashboard_1-traitStaCor-label" for="abiDashboard_1-traitStaCor">Trait:</label>
<div>
<select id="abiDashboard_1-traitStaCor" class="shiny-input-select"><option value="Yield_Mg_ha" selected>Yield_Mg_ha</option>
<option value="Plant_Height_cm" selected>Plant_Height_cm</option>
<option value="Ear_Height_cm" selected>Ear_Height_cm</option></select>
<script type="application/json" data-for="abiDashboard_1-traitStaCor" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="abiDashboard_1-outcd92335eaad89eac" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Trait view

The following graphs aim to sow the genetic correlation between traits using across environment estimates of genetic merit. In addition, the radar plot displays the population means and the target values for the product profile to show the differences between these two and see how big are the gaps.

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="abiDashboard_1-outa1bf342741debbde" style="width:100%;height:400px;"></div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="abiDashboard_1-out234f4052acb2f2bd" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Selection effectiveness

The following graph display the expected gain after the selection of parents and crosses for the next generation. The density plots show the base population (red), the selected population of parents (blue), and the predicted distribution of the crosses to be made (green). The distribution of selected parents and future crosses come from the optimal cross selection (OCS) run.

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="abiDashboard_1-outb7f318f48b740831" style="width:100%;height:400px;"></div><!--/html_preserve-->




### Selection history

The following graph shows the realized genetic gain for this pipeline. The x-axis represents the year of origin or release of the material and the y-axis represents the trait value.



<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="abiDashboard_1-traitSta3-label" for="abiDashboard_1-traitSta3">Trait:</label>
<div>
<select id="abiDashboard_1-traitSta3" class="shiny-input-select"><option value="Yield_Mg_ha" selected>Yield_Mg_ha</option>
<option value="Plant_Height_cm">Plant_Height_cm</option>
<option value="Ear_Height_cm">Ear_Height_cm</option></select>
<script type="application/json" data-for="abiDashboard_1-traitSta3" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="abiDashboard_1-out1c43c7ed6b588d0a" style="width:100%;height:400px;"></div><!--/html_preserve-->

