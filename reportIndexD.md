---
title: "Selection Index Report"
author: ""
date: "December 2023"
output: html_document
params:
 toDownload: FALSE
---







### Across-environment trait table

The following table allows you to inspect the across-environment predictions for the different entries (rows) and different traits (columns).

<p>&nbsp;</p>

NULL
<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="indexDesireApp_1-out58b1030610ba9f4e" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Radar Plot

This plot allows the user to visualize the distance between the population means and the target values.

<p>&nbsp;</p>

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="indexDesireApp_1-outc3b02b10e930d010" style="width:100%;height:400px;"></div><!--/html_preserve-->


### Expected Response Plot

This plot allows the user to visualize the expected response in the new generation given the individuals selected (best 20 percent) using the selection index.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="indexDesireApp_1-proportionTrait-label" for="indexDesireApp_1-proportionTrait">Selected proportion</label>
<input id="indexDesireApp_1-proportionTrait" type="number" class="shiny-input-number form-control" value="0.1" min="0.001" max="1" step="0.05"/>
</div><!--/html_preserve-->

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="indexDesireApp_1-out721ed9cd6801ef82" style="width:100%;height:400px;"></div><!--/html_preserve-->

