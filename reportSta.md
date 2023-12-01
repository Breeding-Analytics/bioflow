---
title: "STA Report"
author: ""
date: "2023-11-03"
output: html_document
runtime: shiny
---



### Entries and traits by environment

The following table allows to see which locations had data for the different traits.

<p>&nbsp;</p>

NULL
<div style="border: 1px solid #ddd; padding: 5px; overflow-x: scroll; width:100%; "><table class="table table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> environment </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> HT_AVG </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> YLD_TON </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> # of entries </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2021_WS_BHU-NONSTRESS_ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 288 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_WS_BHU-STRESS_ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 288 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_WS_IGKV-NONSTRESS_ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 288 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_WS_IGKV-STRESS_ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 288 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_WS_IRRI-HYD-NONSTRESS_ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 288 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_WS_IRRI-HYD-STRESS_ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 288 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_WS_SHUATS-STRESSMOD_ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 288 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_WS_SHUATS-STRESSSEV_ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 288 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023_DS_IRRIHQ-S2_ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 43 </td>
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
<select id="staApp_1-traitSta" class="shiny-input-select"><option value="HT_AVG" selected>HT_AVG</option>
<option value="YLD_TON">YLD_TON</option></select>
<script type="application/json" data-for="staApp_1-traitSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item-overflow-hidden html-fill-item" id="staApp_1-outb6be834e2f3d1bbc" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Predictions 

The following table allows you to check the trait predictions in wide format and filter them by environment.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-envSta-label" for="staApp_1-envSta">Environment:</label>
<div>
<select id="staApp_1-envSta" class="shiny-input-select"><option value="2021_WS_BHU-STRESS_" selected>2021_WS_BHU-STRESS_</option>
<option value="2021_WS_BHU-NONSTRESS_">2021_WS_BHU-NONSTRESS_</option>
<option value="2021_WS_SHUATS-STRESSMOD_">2021_WS_SHUATS-STRESSMOD_</option>
<option value="2021_WS_SHUATS-STRESSSEV_">2021_WS_SHUATS-STRESSSEV_</option>
<option value="2021_WS_IRRI-HYD-STRESS_">2021_WS_IRRI-HYD-STRESS_</option>
<option value="2021_WS_IRRI-HYD-NONSTRESS_">2021_WS_IRRI-HYD-NONSTRESS_</option>
<option value="2021_WS_IGKV-NONSTRESS_">2021_WS_IGKV-NONSTRESS_</option>
<option value="2021_WS_IGKV-STRESS_">2021_WS_IGKV-STRESS_</option>
<option value="2023_DS_IRRIHQ-S2_">2023_DS_IRRIHQ-S2_</option></select>
<script type="application/json" data-for="staApp_1-envSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->


<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item-overflow-hidden html-fill-item" id="staApp_1-outc9b1e7dc40ce2df9" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Boxplot

The following boxplot allows you to see the distribution of predicted values by trait (y-axis) in the different environments.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitStaBox-label" for="staApp_1-traitStaBox">Trait:</label>
<div>
<select id="staApp_1-traitStaBox" class="shiny-input-select" multiple="multiple"><option value="HT_AVG" selected>HT_AVG</option>
<option value="YLD_TON" selected>YLD_TON</option></select>
<script type="application/json" data-for="staApp_1-traitStaBox">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item-overflow-hidden html-fill-item" id="staApp_1-outefef1279dfe79373" style="width:100%;height:400px;"></div><!--/html_preserve-->
