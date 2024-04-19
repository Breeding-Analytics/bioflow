---
title: "Single Trial Analysis Report"
author: ""
date: "2023-11-03"
output: html_document
params:
 toDownload: FALSE
---







### Entries and traits by environment table

The following table allows to see which locations had data for the different traits.

<p>&nbsp;</p>

<div style="border: 1px solid #ddd; padding: 5px; overflow-x: scroll; width:100%; "><table class="table table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> environment </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> shtwt </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> rtwt </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> Number of entries </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2014_test_location_Kasese solgs trial_Kasese solgs trial </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 374 </td>
  </tr>
</tbody>
</table></div>

<p>&nbsp;</p>

### Summary statistics

The following table allows you to verify different quality metrics (KPIs) for the different environments. The selector button allows you to filter by trait.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitSta-label" for="reportBuilder_1-traitSta">Trait:</label>
<div>
<select id="reportBuilder_1-traitSta" class="shiny-input-select"><option value="shtwt" selected>shtwt</option>
<option value="rtwt">rtwt</option></select>
<script type="application/json" data-for="reportBuilder_1-traitSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

Table of statistics:

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="reportBuilder_1-out9ad8637128a3cb88" style="width:100%;height:auto;"></div><!--/html_preserve-->

Variance components ratios per environment 

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitSta0-label" for="reportBuilder_1-traitSta0">Trait:</label>
<div>
<select id="reportBuilder_1-traitSta0" class="shiny-input-select"><option value="shtwt" selected>shtwt</option>
<option value="rtwt">rtwt</option></select>
<script type="application/json" data-for="reportBuilder_1-traitSta0" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-out1c4818b6068b6c9d" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-parameterMetrics-label" for="reportBuilder_1-parameterMetrics">Parameter:</label>
<div>
<select id="reportBuilder_1-parameterMetrics" class="shiny-input-select"><option value="plotH2" selected>plotH2</option>
<option value="CV" selected>CV</option>
<option value="r2" selected>r2</option>
<option value="mean" selected>mean</option></select>
<script type="application/json" data-for="reportBuilder_1-parameterMetrics" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

Barplot for parameter values

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-outc0b69604bc9f3013" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>







### Predictions

The following table allows you to check the trait predictions in wide format and filter them by environment.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-envSta-label" for="reportBuilder_1-envSta">Environment:</label>
<div>
<select id="reportBuilder_1-envSta" class="shiny-input-select"><option value="2014_test_location_Kasese solgs trial_Kasese solgs trial" selected>2014_test_location_Kasese solgs trial_Kasese solgs trial</option></select>
<script type="application/json" data-for="reportBuilder_1-envSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->


<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="reportBuilder_1-outc25d57236d0d11df" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

The following boxplot allows you to see the distribution of predicted values by trait (y-axis) in the different environments.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitStaBox-label" for="reportBuilder_1-traitStaBox">Trait:</label>
<div>
<select id="reportBuilder_1-traitStaBox" class="shiny-input-select"><option value="shtwt" selected>shtwt</option>
<option value="rtwt" selected>rtwt</option></select>
<script type="application/json" data-for="reportBuilder_1-traitStaBox" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-out098f0e588e83e09b" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Correlation between environments

The following plot aims to show the correlation between BLUEs or BLUPs (depending on the parameter settings) among the different environments.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="reportBuilder_1-traitStaCor-label" for="reportBuilder_1-traitStaCor">Trait:</label>
<div>
<select id="reportBuilder_1-traitStaCor" class="shiny-input-select"><option value="shtwt" selected>shtwt</option>
<option value="rtwt" selected>rtwt</option></select>
<script type="application/json" data-for="reportBuilder_1-traitStaCor" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->


<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="reportBuilder_1-out8329eaedddb7c548" style="width:100%;height:400px;"></div><!--/html_preserve-->


### References of methods used

Velazco, J. G., Rodriguez-Alvarez, M. X., Boer, M. P., Jordan, D. R., Eilers, P. H., Malosetti, M., & Van Eeuwijk, F. A. (2017). Modelling spatial trends in sorghum breeding field trials using a two-dimensional P-spline mixed model. Theoretical and Applied Genetics, 130, 1375-1392.

Rodriguez-Alvarez, M. X., Boer, M. P., van Eeuwijk, F. A., & Eilers, P. H. (2018). Correcting for spatial heterogeneity in plant breeding experiments with P-splines. Spatial Statistics, 23, 52-71.

R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

Boer M, van Rossum B (2022). LMMsolver: Linear Mixed Model Solver. R package version 1.0.4.9000.

<p>&nbsp;</p>

