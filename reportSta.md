---
title: "Single Trial Analysis Report"
author: "Contact:<a href = 'https://github.com/Breeding-Analytics/bioflow' target = '_blank'>Breeding Analytics Team, OneCGIAR</a> breedinganalytics@cgiar.org"
date: "May 21, 2024"  
output: html_document
params:
  toDownload: FALSE
---








### Objectives of Single-Trial Analysis

The objective of this dashboard is to help scientist to understand the following points:

1. Overall number of entries and environments included in the single trial analysis (input)

2. High-level summary statistics of the phenotypic information included (input)

3. Observed spatial variation in the different environments when coordinates of the field exist (input)

4. Genetic variance and other genetic parameters observed in the different environments for the different traits (output)

5. Individual adjusted means for each trait by environment combination (output)

6. Phenotypic correlation between environments for the traits present (output)

Understanding these data features should allow the scientist to identify trait by environments combinations that have enough genetic signal and take the decision of which to include in the multi-trial analysis. It should also allow the scientist to assess the quality of the trials conducted and take corrective measures (e.g., change service providers, improve practices, etc.).  






### Entries and traits by environment table

The following table allows to see how many locations had data for the different traits. You may want to review if the phenotyping capacity can deal with the complexity of the trait (e.g., genotype by environment interaction) or if more resources should be deployed. Also you may want to check if you collected information from all the trials conducted.

<p>&nbsp;</p>

<div style="border: 1px solid #ddd; padding: 5px; overflow-x: scroll; width:100%; "><table class="table table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> environment </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> Yield </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> Number of entries </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2021_AMAN_Barishal_CE </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_AMAN_Barishal_RLR-LD </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_AMAN_Barishal_RLR-SD </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_AMAN_Chattogram_RLR-LD </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_AMAN_Chattogram_RLR-SD </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_AMAN_Dhaka_FFS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_AMAN_Dhaka_RLR-LD </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_AMAN_Dhaka_RLR-SD </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_AMAN_Khulna_CE </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_AMAN_Khulna_RLR-LD </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_AMAN_Khulna_RLR-SD </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_AMAN_Mymensingh_RLR-LD </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_AMAN_Rajshahi_FFS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_AMAN_Rajshahi_RLR-LD </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_AMAN_Rajshahi_RLR-SD </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_AMAN_Rangpur_FFS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_AMAN_Rangpur_RLR-LD </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_AMAN_Rangpur_RLR-SD </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_AMAN_Sylhet_FFS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_AMAN_Sylhet_RLR-LD </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_BORO_Barishal_Long Duration </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 11 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_BORO_Barishal_Short Duration </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 11 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_BORO_Chattogram_Short Duration </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_BORO_Dhaka_Long Duration </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_BORO_Dhaka_Short Duration </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_BORO_Khulna_Long Duration </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_BORO_Khulna_Short Duration </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_BORO_Rajshahi_Long Duration </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_BORO_Rajshahi_Short Duration </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_BORO_Rangpur_Long Duration </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_BORO_Rangpur_Short Duration </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_BORO_Sylhet_Long Duration </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_BORO_Sylhet_Short Duration </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_AMAN_BARISHAL_TIDAL SUB </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_AMAN_CHATTOGRAM_CE </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 11 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_AMAN_CHATTOGRAM_TIDAL SUB </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_AMAN_DHAKA_RLR-LD MID MATURITY </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_AMAN_DHAKA_RLR-SD </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_AMAN_KHULNA_CE </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_AMAN_KHULNA_RLR-SD </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_AMAN_MYMENSINGH_FFS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_AMAN_MYMENSINGH_RLR-LD BR11 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_AMAN_MYMENSINGH_RLR-LD MID MATURITY </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_AMAN_RAJSHAHI_RLR-LD SWARNA </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_AMAN_RAJSHAHI_RLR-SD </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_AMAN_RANGPUR_FFS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_AMAN_RANGPUR_RLR-LD BR11 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_AMAN_RANGPUR_RLR-LD FINE RICE </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_AMAN_RANGPUR_RLR-LD SWARNA </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_AMAN_SYLHET_FFS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_BORO_BARISHAL_IRRIGATED LOWLAND RICE-LONG DURATION </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_BORO_BARISHAL_SALINE ECOSYSTEM </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_BORO_CHATTOGRAM_HILL ECOSYSTEM </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_BORO_DHAKA_HAOR ECOSYSTEM </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_BORO_DHAKA_IRRIGATED LOWLAND RICE-LONG DURATION </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_BORO_DHAKA_IRRIGATED LOWLAND RICE-SHORT DURATION </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_BORO_KHULNA_IRRIGATED LOWLAND RICE-LONG DURATION </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_BORO_KHULNA_IRRIGATED LOWLAND RICE-SHORT DURATION </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_BORO_KHULNA_SALINE ECOSYSTEM </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_BORO_RAJSHAHI_IRRIGATED LOWLAND RICE-LONG DURATION </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_BORO_RAJSHAHI_IRRIGATED LOWLAND RICE-SHORT DURATION </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_BORO_RANGPUR_IRRIGATED LOWLAND RICE-LONG DURATION </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_BORO_RANGPUR_IRRIGATED LOWLAND RICE-SHORT DURATION </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_BORO_SYLHET_HAOR ECOSYSTEM </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023_AMAN_BARISHAL_TIDAL SUBMERGENCE </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023_AMAN_CHATTOGRAM_RLR-LD BR11 DOMINANTS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023_AMAN_DHAKA_RLR-LD MID MATURITY DOMINANTS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023_AMAN_DHAKA_RLR-SD </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023_AMAN_KHULNA_CE </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023_AMAN_KHULNA_RLR-SD </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023_AMAN_MYMENSINGH_RLR-LD MID MATURITY DOMINANTS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023_AMAN_RANGPUR_RLR-LD FINE RICE DOMINANTS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023_AMAN_RANGPUR_RLR-LD SWARNA DOMINANTS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023_AMAN_SYLHET_FFS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
</tbody>
</table></div>

<p>&nbsp;</p>

### Summary statistics

The following table allows you to verify some quality metrics (KPIs) for the different trait by environment combinations.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitSta-label" for="staApp_1-traitSta">Trait to filter:</label>
<div>
<select id="staApp_1-traitSta" class="shiny-input-select"><option value="Yield" selected>Yield</option></select>
<script type="application/json" data-for="staApp_1-traitSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="staApp_1-outbca3b74fc085e8b1" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>



<p>&nbsp;</p>





### Output parameters 

<p>&nbsp;</p>

This barplot allows you to see the variance components values and ratios for the trait by environment combinations and identify good quality trials.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitSta0-label" for="staApp_1-traitSta0">Trait to filter:</label>
<div>
<select id="staApp_1-traitSta0" class="shiny-input-select"><option value="Yield" selected>Yield</option></select>
<script type="application/json" data-for="staApp_1-traitSta0" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-oute6fdd565f172506b" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-parameterMetrics-label" for="staApp_1-parameterMetrics">Parameter to filter:</label>
<div>
<select id="staApp_1-parameterMetrics" class="shiny-input-select"><option value="plotH2" selected>plotH2</option>
<option value="CV" selected>CV</option>
<option value="r2" selected>r2</option>
<option value="mean" selected>mean</option></select>
<script type="application/json" data-for="staApp_1-parameterMetrics" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->
<p>&nbsp;</p>

The following barplot is designed to provide a high-level view of estimated parameters such as reliability, heritabiliy, coefficient of variation and others.

<p>&nbsp;</p>

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-out8ef9b31d5214fb3b" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Predictions

The adjusted means in the following visuualizations are the result of fitting a experimental-design agnostic mixed model where everything that can be fitted will be fitted in order to remove as much spatial noise as possible. That means that if a trial has block and incomplete block information both will be fitted. If the trial has also row and column information it will also be fitted together with a spatial kernel (Rodriguez-Alvarez et al., 2018). These table of adjusted means will be used as input information for the multi-trial analysis. We recommend you to don't take any selection decision at this point and wait until the multi-trial analysis is fitted.

The following table allows you to check the trait by environment adjusted means for the different individuals in wide format.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-envSta-label" for="staApp_1-envSta">Environment to filter:</label>
<div>
<select id="staApp_1-envSta" class="shiny-input-select"><option value="2022_BORO_RANGPUR_IRRIGATED LOWLAND RICE-SHORT DURATION" selected>2022_BORO_RANGPUR_IRRIGATED LOWLAND RICE-SHORT DURATION</option>
<option value="2022_BORO_RAJSHAHI_IRRIGATED LOWLAND RICE-SHORT DURATION">2022_BORO_RAJSHAHI_IRRIGATED LOWLAND RICE-SHORT DURATION</option>
<option value="2022_BORO_CHATTOGRAM_HILL ECOSYSTEM">2022_BORO_CHATTOGRAM_HILL ECOSYSTEM</option>
<option value="2022_BORO_DHAKA_IRRIGATED LOWLAND RICE-SHORT DURATION">2022_BORO_DHAKA_IRRIGATED LOWLAND RICE-SHORT DURATION</option>
<option value="2022_BORO_KHULNA_IRRIGATED LOWLAND RICE-SHORT DURATION">2022_BORO_KHULNA_IRRIGATED LOWLAND RICE-SHORT DURATION</option>
<option value="2021_BORO_Sylhet_Short Duration">2021_BORO_Sylhet_Short Duration</option>
<option value="2021_BORO_Khulna_Short Duration">2021_BORO_Khulna_Short Duration</option>
<option value="2021_BORO_Dhaka_Short Duration">2021_BORO_Dhaka_Short Duration</option>
<option value="2021_BORO_Rangpur_Short Duration">2021_BORO_Rangpur_Short Duration</option>
<option value="2021_BORO_Barishal_Short Duration">2021_BORO_Barishal_Short Duration</option>
<option value="2021_BORO_Rajshahi_Short Duration">2021_BORO_Rajshahi_Short Duration</option>
<option value="2022_AMAN_RANGPUR_FFS">2022_AMAN_RANGPUR_FFS</option>
<option value="2022_AMAN_MYMENSINGH_RLR-LD BR11">2022_AMAN_MYMENSINGH_RLR-LD BR11</option>
<option value="2022_AMAN_SYLHET_FFS">2022_AMAN_SYLHET_FFS</option>
<option value="2022_AMAN_MYMENSINGH_FFS">2022_AMAN_MYMENSINGH_FFS</option>
<option value="2022_AMAN_KHULNA_RLR-SD">2022_AMAN_KHULNA_RLR-SD</option>
<option value="2022_AMAN_RAJSHAHI_RLR-SD">2022_AMAN_RAJSHAHI_RLR-SD</option>
<option value="2022_AMAN_DHAKA_RLR-SD">2022_AMAN_DHAKA_RLR-SD</option>
<option value="2021_AMAN_Rangpur_RLR-SD">2021_AMAN_Rangpur_RLR-SD</option>
<option value="2021_AMAN_Dhaka_RLR-SD">2021_AMAN_Dhaka_RLR-SD</option>
<option value="2021_AMAN_Khulna_RLR-SD">2021_AMAN_Khulna_RLR-SD</option>
<option value="2021_AMAN_Chattogram_RLR-SD">2021_AMAN_Chattogram_RLR-SD</option>
<option value="2021_AMAN_Barishal_RLR-SD">2021_AMAN_Barishal_RLR-SD</option>
<option value="2021_AMAN_Rajshahi_RLR-SD">2021_AMAN_Rajshahi_RLR-SD</option>
<option value="2022_AMAN_CHATTOGRAM_CE">2022_AMAN_CHATTOGRAM_CE</option>
<option value="2021_AMAN_Barishal_CE">2021_AMAN_Barishal_CE</option>
<option value="2022_AMAN_KHULNA_CE">2022_AMAN_KHULNA_CE</option>
<option value="2021_AMAN_Khulna_CE">2021_AMAN_Khulna_CE</option>
<option value="2022_BORO_RAJSHAHI_IRRIGATED LOWLAND RICE-LONG DURATION">2022_BORO_RAJSHAHI_IRRIGATED LOWLAND RICE-LONG DURATION</option>
<option value="2022_BORO_DHAKA_IRRIGATED LOWLAND RICE-LONG DURATION">2022_BORO_DHAKA_IRRIGATED LOWLAND RICE-LONG DURATION</option>
<option value="2022_BORO_BARISHAL_IRRIGATED LOWLAND RICE-LONG DURATION">2022_BORO_BARISHAL_IRRIGATED LOWLAND RICE-LONG DURATION</option>
<option value="2022_BORO_KHULNA_IRRIGATED LOWLAND RICE-LONG DURATION">2022_BORO_KHULNA_IRRIGATED LOWLAND RICE-LONG DURATION</option>
<option value="2021_BORO_Barishal_Long Duration">2021_BORO_Barishal_Long Duration</option>
<option value="2021_BORO_Khulna_Long Duration">2021_BORO_Khulna_Long Duration</option>
<option value="2023_AMAN_SYLHET_FFS">2023_AMAN_SYLHET_FFS</option>
<option value="2023_AMAN_DHAKA_RLR-SD">2023_AMAN_DHAKA_RLR-SD</option>
<option value="2023_AMAN_KHULNA_RLR-SD">2023_AMAN_KHULNA_RLR-SD</option>
<option value="2023_AMAN_KHULNA_CE">2023_AMAN_KHULNA_CE</option>
<option value="2021_BORO_Rajshahi_Long Duration">2021_BORO_Rajshahi_Long Duration</option>
<option value="2021_BORO_Sylhet_Long Duration">2021_BORO_Sylhet_Long Duration</option>
<option value="2021_BORO_Rangpur_Long Duration">2021_BORO_Rangpur_Long Duration</option>
<option value="2021_BORO_Dhaka_Long Duration">2021_BORO_Dhaka_Long Duration</option>
<option value="2021_AMAN_Sylhet_FFS">2021_AMAN_Sylhet_FFS</option>
<option value="2021_AMAN_Rangpur_FFS">2021_AMAN_Rangpur_FFS</option>
<option value="2021_AMAN_Dhaka_FFS">2021_AMAN_Dhaka_FFS</option>
<option value="2023_AMAN_CHATTOGRAM_RLR-LD BR11 DOMINANTS">2023_AMAN_CHATTOGRAM_RLR-LD BR11 DOMINANTS</option>
<option value="2021_AMAN_Dhaka_RLR-LD">2021_AMAN_Dhaka_RLR-LD</option>
<option value="2021_AMAN_Rangpur_RLR-LD">2021_AMAN_Rangpur_RLR-LD</option>
<option value="2021_AMAN_Barishal_RLR-LD">2021_AMAN_Barishal_RLR-LD</option>
<option value="2022_AMAN_RANGPUR_RLR-LD BR11">2022_AMAN_RANGPUR_RLR-LD BR11</option>
<option value="2021_AMAN_Rajshahi_RLR-LD">2021_AMAN_Rajshahi_RLR-LD</option>
<option value="2023_AMAN_BARISHAL_TIDAL SUBMERGENCE">2023_AMAN_BARISHAL_TIDAL SUBMERGENCE</option>
<option value="2022_AMAN_CHATTOGRAM_TIDAL SUB">2022_AMAN_CHATTOGRAM_TIDAL SUB</option>
<option value="2022_AMAN_BARISHAL_TIDAL SUB">2022_AMAN_BARISHAL_TIDAL SUB</option>
<option value="2021_BORO_Chattogram_Short Duration">2021_BORO_Chattogram_Short Duration</option>
<option value="2022_BORO_KHULNA_SALINE ECOSYSTEM">2022_BORO_KHULNA_SALINE ECOSYSTEM</option>
<option value="2022_BORO_BARISHAL_SALINE ECOSYSTEM">2022_BORO_BARISHAL_SALINE ECOSYSTEM</option>
<option value="2022_BORO_DHAKA_HAOR ECOSYSTEM">2022_BORO_DHAKA_HAOR ECOSYSTEM</option>
<option value="2022_BORO_SYLHET_HAOR ECOSYSTEM">2022_BORO_SYLHET_HAOR ECOSYSTEM</option>
<option value="2022_BORO_RANGPUR_IRRIGATED LOWLAND RICE-LONG DURATION">2022_BORO_RANGPUR_IRRIGATED LOWLAND RICE-LONG DURATION</option>
<option value="2022_AMAN_DHAKA_RLR-LD MID MATURITY">2022_AMAN_DHAKA_RLR-LD MID MATURITY</option>
<option value="2022_AMAN_MYMENSINGH_RLR-LD MID MATURITY">2022_AMAN_MYMENSINGH_RLR-LD MID MATURITY</option>
<option value="2022_AMAN_RANGPUR_RLR-LD FINE RICE">2022_AMAN_RANGPUR_RLR-LD FINE RICE</option>
<option value="2022_AMAN_RANGPUR_RLR-LD SWARNA">2022_AMAN_RANGPUR_RLR-LD SWARNA</option>
<option value="2021_AMAN_Sylhet_RLR-LD">2021_AMAN_Sylhet_RLR-LD</option>
<option value="2021_AMAN_Mymensingh_RLR-LD">2021_AMAN_Mymensingh_RLR-LD</option>
<option value="2022_AMAN_RAJSHAHI_RLR-LD SWARNA">2022_AMAN_RAJSHAHI_RLR-LD SWARNA</option>
<option value="2021_AMAN_Rajshahi_FFS">2021_AMAN_Rajshahi_FFS</option>
<option value="2021_AMAN_Khulna_RLR-LD">2021_AMAN_Khulna_RLR-LD</option>
<option value="2021_AMAN_Chattogram_RLR-LD">2021_AMAN_Chattogram_RLR-LD</option>
<option value="2023_AMAN_DHAKA_RLR-LD MID MATURITY DOMINANTS">2023_AMAN_DHAKA_RLR-LD MID MATURITY DOMINANTS</option>
<option value="2023_AMAN_MYMENSINGH_RLR-LD MID MATURITY DOMINANTS">2023_AMAN_MYMENSINGH_RLR-LD MID MATURITY DOMINANTS</option>
<option value="2023_AMAN_RANGPUR_RLR-LD FINE RICE DOMINANTS">2023_AMAN_RANGPUR_RLR-LD FINE RICE DOMINANTS</option>
<option value="2023_AMAN_RANGPUR_RLR-LD SWARNA DOMINANTS">2023_AMAN_RANGPUR_RLR-LD SWARNA DOMINANTS</option></select>
<script type="application/json" data-for="staApp_1-envSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="staApp_1-out371a5da0a7e027c2" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

The following boxplot allows you to see the distribution of predicted values by trait (y-axis) in the different environments to double check that everything looks OK.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitStaBox-label" for="staApp_1-traitStaBox">Trait to filter:</label>
<div>
<select id="staApp_1-traitStaBox" class="shiny-input-select"><option value="Yield" selected>Yield</option></select>
<script type="application/json" data-for="staApp_1-traitStaBox" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-out23a21c14c13c6ea8" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Per-environment merit estimates of top entries

In the following plot you can observe the comparison between the top 30 entries from each entry type category for the different traits. If a category has less than a 30 entries all individuals are displayed. This should allow you to identify the top entries in each environment. We would NOT recommend you to use this for selection of parents or products. Wait until you have the results of the multi-trial analysis and selection indices.

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitStaComp-label" for="staApp_1-traitStaComp">Trait to filter:</label>
<div>
<select id="staApp_1-traitStaComp" class="shiny-input-select"><option value="Yield" selected>Yield</option></select>
<script type="application/json" data-for="staApp_1-traitStaComp" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-out24f7726ba9f5941c" style="width:100%;height:400px;"></div><!--/html_preserve-->


### Correlation between environments

The following plot aims to show the correlation between BLUEs or BLUPs (depending on the parameter settings) among the different environments for the traits available in order to identify if there is one or more environments that do not align with the target population of environments (i.e., negatively correlated with the main cluster across most environments). You may want to exclude such environments from the multi-trial analysis (MTA) to ensure that selected entries in the MTA achieve genetic gain in the main cluster of environments.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitStaCor-label" for="staApp_1-traitStaCor">Trait to filter:</label>
<div>
<select id="staApp_1-traitStaCor" class="shiny-input-select"><option value="Yield" selected>Yield</option></select>
<script type="application/json" data-for="staApp_1-traitStaCor" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-outd1b412bbcca4c45a" style="width:100%;height:400px;"></div><!--/html_preserve-->


### References of methods used

Velazco, J. G., Rodriguez-Alvarez, M. X., Boer, M. P., Jordan, D. R., Eilers, P. H., Malosetti, M., & Van Eeuwijk, F. A. (2017). Modelling spatial trends in sorghum breeding field trials using a two-dimensional P-spline mixed model. Theoretical and Applied Genetics, 130, 1375-1392.

Rodriguez-Alvarez, M. X., Boer, M. P., van Eeuwijk, F. A., & Eilers, P. H. (2018). Correcting for spatial heterogeneity in plant breeding experiments with P-splines. Spatial Statistics, 23, 52-71.

R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

Boer M, van Rossum B (2022). LMMsolver: Linear Mixed Model Solver. R package version 1.0.4.9000.

<p>&nbsp;</p>

