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
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> FLW50 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> HT_AVG </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> YLDTONHA </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> Number of entries </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2023_BD-CG-CM-BRRI </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 192 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023_BD-DA-GZ-BRRI </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 192 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023_BD-DA-GZ-BSMRAU </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 192 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023_BD-KH-ST-BINA </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 192 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023_BD-RP-RP-BRRI </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 192 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023_IN-CT-RP-IGKV </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 188 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023_IN-MH-RG-KJ-DBSKKV </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 188 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023_IN-OR-CU-NRRI </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 188 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023_IN-TG-HY-IIRR </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 188 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023_IN-TG-HY-PJTSAU </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 188 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023_IN-UP-VA-BHU </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 188 </td>
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
<select id="staApp_1-traitSta" class="shiny-input-select"><option value="FLW50" selected>FLW50</option>
<option value="HT_AVG">HT_AVG</option>
<option value="YLDTONHA">YLDTONHA</option></select>
<script type="application/json" data-for="staApp_1-traitSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

Table of statistics:

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="staApp_1-out2e8dd5f237d0fe2e" style="width:100%;height:auto;"></div><!--/html_preserve-->

Variance components ratios per environment 

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitSta0-label" for="staApp_1-traitSta0">Trait:</label>
<div>
<select id="staApp_1-traitSta0" class="shiny-input-select"><option value="FLW50" selected>FLW50</option>
<option value="HT_AVG">HT_AVG</option>
<option value="YLDTONHA">YLDTONHA</option></select>
<script type="application/json" data-for="staApp_1-traitSta0" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="staApp_1-out14c98ecf9601f563" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-parameterMetrics-label" for="staApp_1-parameterMetrics">Parameter:</label>
<div>
<select id="staApp_1-parameterMetrics" class="shiny-input-select"><option value="plotH2" selected>plotH2</option>
<option value="CV" selected>CV</option>
<option value="r2" selected>r2</option></select>
<script type="application/json" data-for="staApp_1-parameterMetrics" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

Barplot for parameter values

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="staApp_1-outd99dbae47e593c53" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Predictions

The following table allows you to check the trait predictions in wide format and filter them by environment.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-envSta-label" for="staApp_1-envSta">Environment:</label>
<div>
<select id="staApp_1-envSta" class="shiny-input-select"><option value="2023_BD-CG-CM-BRRI" selected>2023_BD-CG-CM-BRRI</option>
<option value="2023_BD-DA-GZ-BSMRAU">2023_BD-DA-GZ-BSMRAU</option>
<option value="2023_BD-RP-RP-BRRI">2023_BD-RP-RP-BRRI</option>
<option value="2023_BD-KH-ST-BINA">2023_BD-KH-ST-BINA</option>
<option value="2023_BD-DA-GZ-BRRI">2023_BD-DA-GZ-BRRI</option>
<option value="2023_IN-CT-RP-IGKV">2023_IN-CT-RP-IGKV</option>
<option value="2023_IN-OR-CU-NRRI">2023_IN-OR-CU-NRRI</option>
<option value="2023_IN-MH-RG-KJ-DBSKKV">2023_IN-MH-RG-KJ-DBSKKV</option>
<option value="2023_IN-TG-HY-IIRR">2023_IN-TG-HY-IIRR</option>
<option value="2023_IN-TG-HY-PJTSAU">2023_IN-TG-HY-PJTSAU</option>
<option value="2023_IN-UP-VA-BHU">2023_IN-UP-VA-BHU</option></select>
<script type="application/json" data-for="staApp_1-envSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->


<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="staApp_1-out3ae6a2938407cc81" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

The following boxplot allows you to see the distribution of predicted values by trait (y-axis) in the different environments.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitStaBox-label" for="staApp_1-traitStaBox">Trait:</label>
<div>
<select id="staApp_1-traitStaBox" class="shiny-input-select"><option value="FLW50" selected>FLW50</option>
<option value="HT_AVG" selected>HT_AVG</option>
<option value="YLDTONHA" selected>YLDTONHA</option></select>
<script type="application/json" data-for="staApp_1-traitStaBox" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="staApp_1-outefd0fd635b82333e" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Correlation between environments

The following plot aims to show the correlation between BLUEs or BLUPs (depending on the parameter settings) among the different environments.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitStaCor-label" for="staApp_1-traitStaCor">Trait:</label>
<div>
<select id="staApp_1-traitStaCor" class="shiny-input-select"><option value="FLW50" selected>FLW50</option>
<option value="HT_AVG" selected>HT_AVG</option>
<option value="YLDTONHA" selected>YLDTONHA</option></select>
<script type="application/json" data-for="staApp_1-traitStaCor" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->


<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="staApp_1-out09aef3483999f994" style="width:100%;height:400px;"></div><!--/html_preserve-->
