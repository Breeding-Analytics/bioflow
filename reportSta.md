---
title: "STA Report"
author: ""
date: "2023-11-03"
output: html_document
params:
 toDownload: FALSE
---







### Entries and traits by environment table

The following table allows to see which locations had data for the different traits.

<p>&nbsp;</p>

NULL
<div style="border: 1px solid #ddd; padding: 5px; overflow-x: scroll; width:100%; "><table class="table table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> environment </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> GYKGPHA </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> Number of entries </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2018_Fanaye </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018_Kogoni </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018_Niono </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018_VDK </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020_Fanaye </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 45 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020_Mbe </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 45 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020_Ndiaye </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 45 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_Fanaye </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 45 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_Ndiaye </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 66 </td>
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
<select id="staApp_1-traitSta" class="shiny-input-select"><option value="GYKGPHA" selected>GYKGPHA</option></select>
<script type="application/json" data-for="staApp_1-traitSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="staApp_1-outed15a6bbbe450661" style="width:100%;height:auto;"></div><!--/html_preserve-->

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-parameterMetrics-label" for="staApp_1-parameterMetrics">Parameter:</label>
<div>
<select id="staApp_1-parameterMetrics" class="shiny-input-select"><option value="plotH2" selected>plotH2</option>
<option value="CV" selected>CV</option>
<option value="r2" selected>r2</option>
<option value="Vg" selected>Vg</option>
<option value="Vr" selected>Vr</option></select>
<script type="application/json" data-for="staApp_1-parameterMetrics" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-outfd29d4ab595e3b7f" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Predictions

The following table allows you to check the trait predictions in wide format and filter them by environment.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-envSta-label" for="staApp_1-envSta">Environment:</label>
<div>
<select id="staApp_1-envSta" class="shiny-input-select"><option value="2018_Kogoni" selected>2018_Kogoni</option>
<option value="2018_Niono">2018_Niono</option>
<option value="2018_VDK">2018_VDK</option>
<option value="2018_Fanaye">2018_Fanaye</option>
<option value="2021_Ndiaye">2021_Ndiaye</option>
<option value="2020_Mbe">2020_Mbe</option>
<option value="2021_Fanaye">2021_Fanaye</option>
<option value="2020_Fanaye">2020_Fanaye</option>
<option value="2020_Ndiaye">2020_Ndiaye</option></select>
<script type="application/json" data-for="staApp_1-envSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->


<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="staApp_1-out984e82857166c9c9" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

The following plot allows you to see the distribution of predicted values by trait (y-axis) in the different environments.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitStaBox-label" for="staApp_1-traitStaBox">Trait:</label>
<div>
<select id="staApp_1-traitStaBox" class="shiny-input-select"><option value="GYKGPHA" selected>GYKGPHA</option></select>
<script type="application/json" data-for="staApp_1-traitStaBox" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-out0dd902d9387c2bb9" style="width:100%;height:400px;"></div><!--/html_preserve-->
