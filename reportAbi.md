---
title: "Accelerated Breeding Initiative Dashboard"
author: ""
date: "2023-11-03"
output: html_document
params:
 toDownload: FALSE
---







NULL

### Data use

The following visualization shows which data types was used for this pipeline. It allows to see whether the data has been QA and the summary values for the different data types.

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="abiDashboard_1-traitMta-label" for="abiDashboard_1-traitMta">Trait:</label>
<div>
<select id="abiDashboard_1-traitMta" class="shiny-input-select"><option value="Yield_Mg_ha" selected>Yield_Mg_ha</option>
<option value="Plant_Height_cm">Plant_Height_cm</option></select>
<script type="application/json" data-for="abiDashboard_1-traitMta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<p>&nbsp;</p>

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="abiDashboard_1-out62fd1c3a22c147f7" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Base metrics

The following barplot shows the value of the different parameters calculated per trial during the single trial analysis run. The view can be modified by trait and by parameter.

Variance component proportions

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="abiDashboard_1-traitSta-label" for="abiDashboard_1-traitSta">Trait:</label>
<div>
<select id="abiDashboard_1-traitSta" class="shiny-input-select"><option value="Yield_Mg_ha" selected>Yield_Mg_ha</option>
<option value="Plant_Height_cm">Plant_Height_cm</option></select>
<script type="application/json" data-for="abiDashboard_1-traitSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="abiDashboard_1-out26504e05bc81d357" style="width:100%;height:400px;"></div><!--/html_preserve-->

Barplot for parameter values

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="abiDashboard_1-parameterMetrics-label" for="abiDashboard_1-parameterMetrics">Parameter:</label>
<div>
<select id="abiDashboard_1-parameterMetrics" class="shiny-input-select"><option value="plotH2" selected>plotH2</option>
<option value="CV" selected>CV</option>
<option value="r2" selected>r2</option></select>
<script type="application/json" data-for="abiDashboard_1-parameterMetrics" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->


<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="abiDashboard_1-outf22dd8ce90464f9f" style="width:100%;height:400px;"></div><!--/html_preserve-->

Correlation between environments

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="abiDashboard_1-traitStaCor-label" for="abiDashboard_1-traitStaCor">Trait:</label>
<div>
<select id="abiDashboard_1-traitStaCor" class="shiny-input-select"><option value="Yield_Mg_ha" selected>Yield_Mg_ha</option>
<option value="Plant_Height_cm" selected>Plant_Height_cm</option></select>
<script type="application/json" data-for="abiDashboard_1-traitStaCor" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="abiDashboard_1-out1723547b7cbdd570" style="width:100%;height:400px;"></div><!--/html_preserve-->

Connectivity between the environments

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="abiDashboard_1-traitMtaConnect-label" for="abiDashboard_1-traitMtaConnect"></label>
<div>
<select id="abiDashboard_1-traitMtaConnect" class="shiny-input-select"><option value="Yield_Mg_ha" selected>Yield_Mg_ha</option>
<option value="Plant_Height_cm">Plant_Height_cm</option></select>
<script type="application/json" data-for="abiDashboard_1-traitMtaConnect" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="abiDashboard_1-out962d724bab3dffc6" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Trait view

The following graphs aim to sow the genetic correlation between traits using across environment estimates of genetic merit. In addition, the radar plot displays the population means and the target values for the product profile to show the differences between these two and see how big are the gaps.


### Selection results

The following graph display the expected gain after the selection of parents and crosses for the next generation. The density plots show the base population (red), the selected population of parents (blue), and the predicted distribution of the crosses to be made (green). The distribution of selected parents and future crosses come from the optimal cross selection (OCS) run.


### Selection history

The following graph shows the realized genetic gain for this pipeline. The x-axis represents the year of origin or release of the material and the y-axis represents the trait value.


