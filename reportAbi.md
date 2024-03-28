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

The following table shows which data was available and used for this pipeline. It allows to see whether the data has been QA and the summary values for the different data types.

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="abiDashboard_1-traitMta-label" for="abiDashboard_1-traitMta">Trait:</label>
<div>
<select id="abiDashboard_1-traitMta" class="shiny-input-select"><option value="Yield_Mg_ha" selected>Yield_Mg_ha</option>
<option value="Plant_Height_cm">Plant_Height_cm</option></select>
<script type="application/json" data-for="abiDashboard_1-traitMta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<p>&nbsp;</p>

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="abiDashboard_1-outbe76461480ebadc8" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Base metrics

The following barplot shows the value of the different parameters calculated per trial during the single trial analysis run. The view can be modified by trait and by parameter.


### Trait view

The following graphs aim to sow the genetic correlation between traits using across environment estimates of genetic merit. In addition, the radar plot displays the population means and the target values for the product profile to show the differences between these two and see how big are the gaps.


### Selection results

The following graph display the expected gain after the selection of parents and crosses for the next generation. The density plots show the base population (red), the selected population of parents (blue), and the predicted distribution of the crosses to be made (green). The distribution of selected parents and future crosses come from the optimal cross selection (OCS) run.


### Selection history

The following graph shows the realized genetic gain for this pipeline. The x-axis represents the year of origin or release of the material and the y-axis represents the trait value.


