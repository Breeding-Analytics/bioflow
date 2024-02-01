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
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> Fruit length in cm|cycle 2 main plant|COMP:0000490 </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> # of entries </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2012 Genomic Selection_ Training Population field 1 Sendusu </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 317 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2014 Genomic Selection_ Training Population field 2 Sendusu </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 320 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2015 Genomic Selection_ Training Population, Mbarara </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 298 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2017 Genomic Selection_ Validation population trial Sendusu </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 206 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018 Genomic Selection_ Validation population_ Sendusu </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 600 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019 Genomic Selection_ Validation population_ Mbarara </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 600 </td>
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
<select id="staApp_1-traitSta" class="shiny-input-select"><option value="Fruit length in cm|cycle 2 main plant|COMP:0000490" selected>Fruit length in cm|cycle 2 main plant|COMP:0000490</option></select>
<script type="application/json" data-for="staApp_1-traitSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="staApp_1-out1a5591fa1645cdca" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Predictions 

The following table allows you to check the trait predictions in wide format and filter them by environment.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-envSta-label" for="staApp_1-envSta">Environment:</label>
<div>
<select id="staApp_1-envSta" class="shiny-input-select"><option value="2019 Genomic Selection_ Validation population_ Mbarara" selected>2019 Genomic Selection_ Validation population_ Mbarara</option>
<option value="2018 Genomic Selection_ Validation population_ Sendusu">2018 Genomic Selection_ Validation population_ Sendusu</option></select>
<script type="application/json" data-for="staApp_1-envSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->


<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="staApp_1-out6440c1fe93d6aa9a" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Boxplot

The following boxplot allows you to see the distribution of predicted values by trait (y-axis) in the different environments.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitStaBox-label" for="staApp_1-traitStaBox">Trait:</label>
<div>
<select id="staApp_1-traitStaBox" class="shiny-input-select"><option value="Fruit length in cm|cycle 2 main plant|COMP:0000490" selected>Fruit length in cm|cycle 2 main plant|COMP:0000490</option></select>
<script type="application/json" data-for="staApp_1-traitStaBox" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-outc8263807725652c0" style="width:100%;height:400px;"></div><!--/html_preserve-->
