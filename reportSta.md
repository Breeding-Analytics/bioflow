---
title: "STA Report"
author: ""
date: "2023-11-03"
output: html_document
params:
 toDownload: FALSE
---







### Entries and traits by environment

The following table allows to see which locations had data for the different traits.

<p>&nbsp;</p>

NULL
<div style="border: 1px solid #ddd; padding: 5px; overflow-x: scroll; width:100%; "><table class="table table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> environment </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> GY </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> EH </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> MOI </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> # of entries </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 136 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 136 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 136 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 136 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 136 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 192 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 192 </td>
  </tr>
</tbody>
</table></div>

<p>&nbsp;</p>

### Summary statistics

The following table allows you to verify different quality metrics (KPIs) for the different environments. The selector button allows you to filter by trait.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitSta-label" for="staApp_1-traitSta">Trait:</label>
<div>
<select id="staApp_1-traitSta" class="shiny-input-select"><option value="GY" selected>GY</option>
<option value="EH">EH</option>
<option value="MOI">MOI</option></select>
<script type="application/json" data-for="staApp_1-traitSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="staApp_1-outc79cb8e4a1117c7f" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Predictions 

The following table allows you to check the trait predictions in wide format and filter them by environment.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-envSta-label" for="staApp_1-envSta">Environment:</label>
<div>
<select id="staApp_1-envSta" class="shiny-input-select"><option value="1" selected>1</option>
<option value="3">3</option>
<option value="6">6</option>
<option value="4">4</option>
<option value="7">7</option>
<option value="5">5</option>
<option value="2">2</option></select>
<script type="application/json" data-for="staApp_1-envSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->


<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="staApp_1-outc34ea9891a3c73db" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Boxplot

The following boxplot allows you to see the distribution of predicted values by trait (y-axis) in the different environments.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitStaBox-label" for="staApp_1-traitStaBox">Trait:</label>
<div>
<select id="staApp_1-traitStaBox" class="shiny-input-select"><option value="GY" selected>GY</option>
<option value="EH" selected>EH</option>
<option value="MOI" selected>MOI</option></select>
<script type="application/json" data-for="staApp_1-traitStaBox" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-out28f6557f3fd3817a" style="width:100%;height:400px;"></div><!--/html_preserve-->
