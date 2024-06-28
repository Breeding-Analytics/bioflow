---
title: "Single Trial Analysis Report"
author: "Contact:<a href = 'https://github.com/Breeding-Analytics/bioflow' target = '_blank'>Breeding Analytics Team, OneCGIAR</a> breedinganalytics@cgiar.org"
date: "June 26, 2024"  
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
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> YLDTONF </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> Number of entries </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2018_DS_Bogura, Bangladesh_IRSEA-BD_BG-IYT-2018-DS-1 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 384 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_DS_BRRI Regional Station Cumilla_IRSEA-BD_CG_CM_BRRI-IYT-2019-DS-1 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 359 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_DS_Gazipur, Bangladesh_IRSEA-BD_DA_GZ-IYT-2019-DS-1 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 359 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_DS_Rajshahi, Bangladesh_IRSEA-BD_RS_RS-IYT-2019-DS-1 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 359 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_DS_Sathkira, Bangladesh_IRSEA-BD_KH_ST-IYT-2019-DS-1 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 359 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_WS_BINA Regional Station Barishal_IRSEA-BD_BA_BS_BINA-IYT-2019-WS-1 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 374 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_WS_BRRI Regional Station Cumilla_IRSEA-BD_CG_CM_BRRI-IYT-2019-WS-1 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 384 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_WS_BRRI, Rajshahi, Rajshahi, Bangladesh_IRSEA-BD_RS_RS_BRRI-IYT-2019-WS-1 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 372 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_WS_BSMRAU, Gazipur, Dhaka, Bangladesh_IRSEA-BD_DA_GZ_BSMRAU-IYT-2019-WS-1 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 383 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_WS_Corteva, Ayodhya, UP, India_IRSEA-IN_UP_AY_CORT-IYT-2019-WS-1 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 397 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_WS_Corteva, Ayodhya, UP, India_IRSEA-IN_UP_AY_CORT-IYT-2019-WS-2 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 373 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_WS_Corteva, Gosaigunj, UP, India_IRSEA-IN_UP_GO_CORT-IYT-2019-WS-1 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 397 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_WS_Corteva, Gosaigunj, UP, India_IRSEA-IN_UP_GO_CORT-IYT-2019-WS-2 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 373 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_WS_Corteva, Lucknow, UP, India_IRSEA-IN_UP_LU_CORT-IYT-2019-WS-2 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 397 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_WS_Corteva, Lucknow, UP, India_IRSEA-IN_UP_LU_CORT-IYT-2019-WS-3 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 373 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_WS_Corteva, Prayagraj, UP, India_IRSEA-IN_UP_PR_CORT-IYT-2019-WS-1 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 397 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_WS_Corteva, Prayagraj, UP, India_IRSEA-IN_UP_PR_CORT-IYT-2019-WS-2 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 373 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_WS_IGKV, Raipur, Chhattisgarh, India_IRSEA-IN_CT_RP_IGKV-IYT-2019-WS-1 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 386 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_WS_RARS, Maruteru, West Godavari, Andhra Pradesh, India_IRSEA-IN_AD_WG_RARS-IYT-2019-WS-1 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 386 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020_DS_ACI Research Field RDA Bogura_IRSEA-BD_RS_BO_SP-IYT-2020-DS-1 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 362 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020_DS_BINA Regional Station Barishal_IRSEA-BD_BA_BS_BINA-IYT-2020-DS-1 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 362 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020_DS_BRRI Regional Station Habiganj_IRSEA-BD_SY_HA_BN-IYT-2020-DS-1 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 362 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020_DS_BSMRAU, Gazipur, Dhaka, Bangladesh_IRSEA-BD_DA_GZ_BSMRAU-IYT-2020-DS-1 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 362 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2021_WS_BRRI, Rajshahi, Rajshahi, Bangladesh_IRRIGATED-BD_RS_RS_BRRI-AYT-2021-WS-1 </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 100 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_WS_BD_CG_CM_BRRI_2022WS EST006 BD BRRI Cumilla TMeLS-I </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 215 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_WS_BD_DA_GZ_BSMRAU_2022WS EST006 BD BSMRAU TMeLS-I </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 215 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_WS_BD_DA_GZ_BSMRAU_2022WS TMeLS-I S2 BD BSMRAU </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 54 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_WS_BD_RS_RS_BRRI_2022WS EST006 BD BRRI Rajshahi TMeLS-I </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 215 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_WS_BD_RS_RS_BRRI_2022WS TMeLS-I S2 BD BRRI Rajshahi </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 54 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_WS_IN_AD_KR_ANGRAU_2022WS EST006 IN ANGRAU TMeLS-I </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 235 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_WS_IN_CT_RP_IGKV_2022WS EST006 IN IGKV TMeLS-I </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 235 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_WS_IN_OR_CU_NRRI_2022WS EST006 IN NRRI TMeLS-I </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 235 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_WS_IN_TG_HY_IIRR_2022WS EST006 IN IIRR TMeLS-I </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 235 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_WS_IN_TG_NA_PJTSAU_2022WS EST006 IN PJTSAU TMeLS-I </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 235 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2022_WS_IN_UP_VA_BHU_2022WS EST006 IN BHU TMeLS-I </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 235 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023WS TMeLS-I EST006 S2 BD BINA_BD-KH-ST-BINA </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 48 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023WS TMeLS-I EST006 S2 BD BRRI Cumilla_BD-CG-CM-BRRI </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 48 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023WS TMeLS-I EST006 S2 BD BRRI HQ_BD-DA-GZ-BRRI </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 48 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023WS TMeLS-I EST006 S2 BD BRRI Rangpur_BD-RP-RP-BRRI </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 48 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023WS TMeLS-I EST006 S2 BD BSMRAU_BD-DA-GZ-BSMRAU </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 48 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023WS TMeLS-R (Drt) EST006 S2 IN BAU Nonstress_IN-JH-RN-BAU-NON-STRESS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023WS TMeLS-R (Drt) EST006 S2 IN BAU Stress_IN-JH-RN-BAU-STRESS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023WS TMeLS-R (Drt) EST006 S2 IN CRURRS Nonstress_IN-JH-HZ-CRURRS-NON-STRESS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023WS TMeLS-R (Drt) EST006 S2 IN CRURRS Stress_IN-JH-HZ-CRURRS-STRESS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023WS TMeLS-R (Drt) EST006 S2 IN IGKV Nonstress_IN-CT-RP-IGKV-NON-STRESS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023WS TMeLS-R (Drt) EST006 S2 IN IGKV Stress_IN-CT-RP-IGKV-STRESS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023WS TMeLS-R (Drt) EST006 S2 IN JDA-RRS Nonstress_IN-WB-BN-JDA-RRS-NON-STRESS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023WS TMeLS-R (Drt) EST006 S2 IN JDA-RRS Stress_IN-WB-BN-JDA-RRS-STRESS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023WS TMeLS-R (Drt) EST006 S2 IN SHUATS Nonstress_IN-UP-PY-SHUATS-NON-STRESS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023WS TMeLS-R (Drt) EST006 S2 IN SHUATS Stress_IN-UP-PY-SHUATS-STRESS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023WS TMeLS-R (Drt) EST006 S2 IN ZDRPRS Nonstress_IN-WB-PU-ZDRPRS-NON-STRESS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 35 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2023WS TMeLS-R (Drt) EST006 S2 IN ZDRPRS Stress_IN-WB-PU-ZDRPRS-STRESS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 35 </td>
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
<select id="staApp_1-traitSta" class="shiny-input-select"><option value="YLDTONF" selected>YLDTONF</option></select>
<script type="application/json" data-for="staApp_1-traitSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="staApp_1-outb0b90383f26fa151" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Field view

The following heatmaps allow you to inspect the spatial trends in the different fields to take corrective measures in the next season and understand why some variance components may look the way they look.


<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitStaFieldView-label" for="staApp_1-traitStaFieldView">Trait to filter:</label>
<div>
<select id="staApp_1-traitStaFieldView" class="shiny-input-select"><option value="YLDTONF" selected>YLDTONF</option></select>
<script type="application/json" data-for="staApp_1-traitStaFieldView" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-out6ced334b0ba91319" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Output parameters 

<p>&nbsp;</p>

This barplot allows you to see the variance components values and ratios for the trait by environment combinations and identify good quality trials.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitSta0-label" for="staApp_1-traitSta0">Trait to filter:</label>
<div>
<select id="staApp_1-traitSta0" class="shiny-input-select"><option value="YLDTONF" selected>YLDTONF</option></select>
<script type="application/json" data-for="staApp_1-traitSta0" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-outaa58915c4951b6f2" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-parameterMetrics-label" for="staApp_1-parameterMetrics">Parameter to filter:</label>
<div>
<select id="staApp_1-parameterMetrics" class="shiny-input-select"><option value="plotH2" selected>plotH2</option>
<option value="CV" selected>CV</option>
<option value="r2" selected>r2</option>
<option value="V_designation" selected>V_designation</option>
<option value="V_rowF" selected>V_rowF</option>
<option value="V_colF" selected>V_colF</option>
<option value="V_repF" selected>V_repF</option>
<option value="V_iBlockF" selected>V_iBlockF</option>
<option value="V_s(row, col)" selected>V_s(row, col)</option>
<option value="V_residual" selected>V_residual</option>
<option value="mean" selected>mean</option></select>
<script type="application/json" data-for="staApp_1-parameterMetrics" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->
<p>&nbsp;</p>

The following barplot is designed to provide a high-level view of estimated parameters such as reliability, heritabiliy, coefficient of variation and others.

<p>&nbsp;</p>

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-out56693f7c952014df" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Predictions

The adjusted means in the following visuualizations are the result of fitting a experimental-design agnostic mixed model where everything that can be fitted will be fitted in order to remove as much spatial noise as possible. That means that if a trial has block and incomplete block information both will be fitted. If the trial has also row and column information it will also be fitted together with a spatial kernel (Rodriguez-Alvarez et al., 2018). These table of adjusted means will be used as input information for the multi-trial analysis. We recommend you to don't take any selection decision at this point and wait until the multi-trial analysis is fitted.

The following table allows you to check the trait by environment adjusted means for the different individuals in wide format.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-envSta-label" for="staApp_1-envSta">Environment to filter:</label>
<div>
<select id="staApp_1-envSta" class="shiny-input-select"><option value="2019_WS_Corteva, Ayodhya, UP, India_IRSEA-IN_UP_AY_CORT-IYT-2019-WS-2" selected>2019_WS_Corteva, Ayodhya, UP, India_IRSEA-IN_UP_AY_CORT-IYT-2019-WS-2</option>
<option value="2019_WS_Corteva, Gosaigunj, UP, India_IRSEA-IN_UP_GO_CORT-IYT-2019-WS-2">2019_WS_Corteva, Gosaigunj, UP, India_IRSEA-IN_UP_GO_CORT-IYT-2019-WS-2</option>
<option value="2019_WS_Corteva, Lucknow, UP, India_IRSEA-IN_UP_LU_CORT-IYT-2019-WS-3">2019_WS_Corteva, Lucknow, UP, India_IRSEA-IN_UP_LU_CORT-IYT-2019-WS-3</option>
<option value="2019_WS_Corteva, Prayagraj, UP, India_IRSEA-IN_UP_PR_CORT-IYT-2019-WS-1">2019_WS_Corteva, Prayagraj, UP, India_IRSEA-IN_UP_PR_CORT-IYT-2019-WS-1</option>
<option value="2019_WS_Corteva, Lucknow, UP, India_IRSEA-IN_UP_LU_CORT-IYT-2019-WS-2">2019_WS_Corteva, Lucknow, UP, India_IRSEA-IN_UP_LU_CORT-IYT-2019-WS-2</option>
<option value="2019_WS_Corteva, Ayodhya, UP, India_IRSEA-IN_UP_AY_CORT-IYT-2019-WS-1">2019_WS_Corteva, Ayodhya, UP, India_IRSEA-IN_UP_AY_CORT-IYT-2019-WS-1</option>
<option value="2019_WS_Corteva, Prayagraj, UP, India_IRSEA-IN_UP_PR_CORT-IYT-2019-WS-2">2019_WS_Corteva, Prayagraj, UP, India_IRSEA-IN_UP_PR_CORT-IYT-2019-WS-2</option>
<option value="2019_WS_Corteva, Gosaigunj, UP, India_IRSEA-IN_UP_GO_CORT-IYT-2019-WS-1">2019_WS_Corteva, Gosaigunj, UP, India_IRSEA-IN_UP_GO_CORT-IYT-2019-WS-1</option>
<option value="2023WS TMeLS-R (Drt) EST006 S2 IN CRURRS Stress_IN-JH-HZ-CRURRS-STRESS">2023WS TMeLS-R (Drt) EST006 S2 IN CRURRS Stress_IN-JH-HZ-CRURRS-STRESS</option>
<option value="2023WS TMeLS-R (Drt) EST006 S2 IN CRURRS Nonstress_IN-JH-HZ-CRURRS-NON-STRESS">2023WS TMeLS-R (Drt) EST006 S2 IN CRURRS Nonstress_IN-JH-HZ-CRURRS-NON-STRESS</option>
<option value="2020_DS_BINA Regional Station Barishal_IRSEA-BD_BA_BS_BINA-IYT-2020-DS-1">2020_DS_BINA Regional Station Barishal_IRSEA-BD_BA_BS_BINA-IYT-2020-DS-1</option>
<option value="2022_WS_BD_CG_CM_BRRI_2022WS EST006 BD BRRI Cumilla TMeLS-I">2022_WS_BD_CG_CM_BRRI_2022WS EST006 BD BRRI Cumilla TMeLS-I</option>
<option value="2022_WS_BD_RS_RS_BRRI_2022WS EST006 BD BRRI Rajshahi TMeLS-I">2022_WS_BD_RS_RS_BRRI_2022WS EST006 BD BRRI Rajshahi TMeLS-I</option>
<option value="2022_WS_BD_DA_GZ_BSMRAU_2022WS EST006 BD BSMRAU TMeLS-I">2022_WS_BD_DA_GZ_BSMRAU_2022WS EST006 BD BSMRAU TMeLS-I</option>
<option value="2023WS TMeLS-I EST006 S2 BD BRRI Cumilla_BD-CG-CM-BRRI">2023WS TMeLS-I EST006 S2 BD BRRI Cumilla_BD-CG-CM-BRRI</option>
<option value="2023WS TMeLS-I EST006 S2 BD BSMRAU_BD-DA-GZ-BSMRAU">2023WS TMeLS-I EST006 S2 BD BSMRAU_BD-DA-GZ-BSMRAU</option>
<option value="2023WS TMeLS-I EST006 S2 BD BINA_BD-KH-ST-BINA">2023WS TMeLS-I EST006 S2 BD BINA_BD-KH-ST-BINA</option>
<option value="2023WS TMeLS-I EST006 S2 BD BRRI HQ_BD-DA-GZ-BRRI">2023WS TMeLS-I EST006 S2 BD BRRI HQ_BD-DA-GZ-BRRI</option>
<option value="2023WS TMeLS-I EST006 S2 BD BRRI Rangpur_BD-RP-RP-BRRI">2023WS TMeLS-I EST006 S2 BD BRRI Rangpur_BD-RP-RP-BRRI</option>
<option value="2022_WS_IN_AD_KR_ANGRAU_2022WS EST006 IN ANGRAU TMeLS-I">2022_WS_IN_AD_KR_ANGRAU_2022WS EST006 IN ANGRAU TMeLS-I</option>
<option value="2022_WS_IN_CT_RP_IGKV_2022WS EST006 IN IGKV TMeLS-I ">2022_WS_IN_CT_RP_IGKV_2022WS EST006 IN IGKV TMeLS-I </option>
<option value="2022_WS_IN_UP_VA_BHU_2022WS EST006 IN BHU TMeLS-I">2022_WS_IN_UP_VA_BHU_2022WS EST006 IN BHU TMeLS-I</option>
<option value="2022_WS_IN_TG_NA_PJTSAU_2022WS EST006 IN PJTSAU TMeLS-I ">2022_WS_IN_TG_NA_PJTSAU_2022WS EST006 IN PJTSAU TMeLS-I </option>
<option value="2022_WS_IN_TG_HY_IIRR_2022WS EST006 IN IIRR TMeLS-I">2022_WS_IN_TG_HY_IIRR_2022WS EST006 IN IIRR TMeLS-I</option>
<option value="2022_WS_IN_OR_CU_NRRI_2022WS EST006 IN NRRI TMeLS-I">2022_WS_IN_OR_CU_NRRI_2022WS EST006 IN NRRI TMeLS-I</option>
<option value="2020_DS_ACI Research Field RDA Bogura_IRSEA-BD_RS_BO_SP-IYT-2020-DS-1">2020_DS_ACI Research Field RDA Bogura_IRSEA-BD_RS_BO_SP-IYT-2020-DS-1</option>
<option value="2020_DS_BRRI Regional Station Habiganj_IRSEA-BD_SY_HA_BN-IYT-2020-DS-1">2020_DS_BRRI Regional Station Habiganj_IRSEA-BD_SY_HA_BN-IYT-2020-DS-1</option>
<option value="2020_DS_BSMRAU, Gazipur, Dhaka, Bangladesh_IRSEA-BD_DA_GZ_BSMRAU-IYT-2020-DS-1">2020_DS_BSMRAU, Gazipur, Dhaka, Bangladesh_IRSEA-BD_DA_GZ_BSMRAU-IYT-2020-DS-1</option>
<option value="2019_DS_Sathkira, Bangladesh_IRSEA-BD_KH_ST-IYT-2019-DS-1">2019_DS_Sathkira, Bangladesh_IRSEA-BD_KH_ST-IYT-2019-DS-1</option>
<option value="2019_DS_Gazipur, Bangladesh_IRSEA-BD_DA_GZ-IYT-2019-DS-1">2019_DS_Gazipur, Bangladesh_IRSEA-BD_DA_GZ-IYT-2019-DS-1</option>
<option value="2019_DS_BRRI Regional Station Cumilla_IRSEA-BD_CG_CM_BRRI-IYT-2019-DS-1">2019_DS_BRRI Regional Station Cumilla_IRSEA-BD_CG_CM_BRRI-IYT-2019-DS-1</option>
<option value="2019_DS_Rajshahi, Bangladesh_IRSEA-BD_RS_RS-IYT-2019-DS-1">2019_DS_Rajshahi, Bangladesh_IRSEA-BD_RS_RS-IYT-2019-DS-1</option>
<option value="2022_WS_BD_DA_GZ_BSMRAU_2022WS TMeLS-I S2 BD BSMRAU">2022_WS_BD_DA_GZ_BSMRAU_2022WS TMeLS-I S2 BD BSMRAU</option>
<option value="2021_WS_BRRI, Rajshahi, Rajshahi, Bangladesh_IRRIGATED-BD_RS_RS_BRRI-AYT-2021-WS-1">2021_WS_BRRI, Rajshahi, Rajshahi, Bangladesh_IRRIGATED-BD_RS_RS_BRRI-AYT-2021-WS-1</option>
<option value="2019_WS_BRRI Regional Station Cumilla_IRSEA-BD_CG_CM_BRRI-IYT-2019-WS-1">2019_WS_BRRI Regional Station Cumilla_IRSEA-BD_CG_CM_BRRI-IYT-2019-WS-1</option>
<option value="2019_WS_BSMRAU, Gazipur, Dhaka, Bangladesh_IRSEA-BD_DA_GZ_BSMRAU-IYT-2019-WS-1">2019_WS_BSMRAU, Gazipur, Dhaka, Bangladesh_IRSEA-BD_DA_GZ_BSMRAU-IYT-2019-WS-1</option>
<option value="2019_WS_BINA Regional Station Barishal_IRSEA-BD_BA_BS_BINA-IYT-2019-WS-1">2019_WS_BINA Regional Station Barishal_IRSEA-BD_BA_BS_BINA-IYT-2019-WS-1</option>
<option value="2019_WS_BRRI, Rajshahi, Rajshahi, Bangladesh_IRSEA-BD_RS_RS_BRRI-IYT-2019-WS-1">2019_WS_BRRI, Rajshahi, Rajshahi, Bangladesh_IRSEA-BD_RS_RS_BRRI-IYT-2019-WS-1</option>
<option value="2022_WS_BD_RS_RS_BRRI_2022WS TMeLS-I S2 BD BRRI Rajshahi">2022_WS_BD_RS_RS_BRRI_2022WS TMeLS-I S2 BD BRRI Rajshahi</option>
<option value="2018_DS_Bogura, Bangladesh_IRSEA-BD_BG-IYT-2018-DS-1">2018_DS_Bogura, Bangladesh_IRSEA-BD_BG-IYT-2018-DS-1</option>
<option value="2019_WS_RARS, Maruteru, West Godavari, Andhra Pradesh, India_IRSEA-IN_AD_WG_RARS-IYT-2019-WS-1">2019_WS_RARS, Maruteru, West Godavari, Andhra Pradesh, India_IRSEA-IN_AD_WG_RARS-IYT-2019-WS-1</option>
<option value="2019_WS_IGKV, Raipur, Chhattisgarh, India_IRSEA-IN_CT_RP_IGKV-IYT-2019-WS-1">2019_WS_IGKV, Raipur, Chhattisgarh, India_IRSEA-IN_CT_RP_IGKV-IYT-2019-WS-1</option>
<option value="2023WS TMeLS-R (Drt) EST006 S2 IN BAU Stress_IN-JH-RN-BAU-STRESS">2023WS TMeLS-R (Drt) EST006 S2 IN BAU Stress_IN-JH-RN-BAU-STRESS</option>
<option value="2023WS TMeLS-R (Drt) EST006 S2 IN ZDRPRS Stress_IN-WB-PU-ZDRPRS-STRESS">2023WS TMeLS-R (Drt) EST006 S2 IN ZDRPRS Stress_IN-WB-PU-ZDRPRS-STRESS</option>
<option value="2023WS TMeLS-R (Drt) EST006 S2 IN BAU Nonstress_IN-JH-RN-BAU-NON-STRESS">2023WS TMeLS-R (Drt) EST006 S2 IN BAU Nonstress_IN-JH-RN-BAU-NON-STRESS</option>
<option value="2023WS TMeLS-R (Drt) EST006 S2 IN ZDRPRS Nonstress_IN-WB-PU-ZDRPRS-NON-STRESS">2023WS TMeLS-R (Drt) EST006 S2 IN ZDRPRS Nonstress_IN-WB-PU-ZDRPRS-NON-STRESS</option>
<option value="2023WS TMeLS-R (Drt) EST006 S2 IN IGKV Stress_IN-CT-RP-IGKV-STRESS">2023WS TMeLS-R (Drt) EST006 S2 IN IGKV Stress_IN-CT-RP-IGKV-STRESS</option>
<option value="2023WS TMeLS-R (Drt) EST006 S2 IN IGKV Nonstress_IN-CT-RP-IGKV-NON-STRESS">2023WS TMeLS-R (Drt) EST006 S2 IN IGKV Nonstress_IN-CT-RP-IGKV-NON-STRESS</option>
<option value="2023WS TMeLS-R (Drt) EST006 S2 IN JDA-RRS Nonstress_IN-WB-BN-JDA-RRS-NON-STRESS">2023WS TMeLS-R (Drt) EST006 S2 IN JDA-RRS Nonstress_IN-WB-BN-JDA-RRS-NON-STRESS</option>
<option value="2023WS TMeLS-R (Drt) EST006 S2 IN SHUATS Nonstress_IN-UP-PY-SHUATS-NON-STRESS">2023WS TMeLS-R (Drt) EST006 S2 IN SHUATS Nonstress_IN-UP-PY-SHUATS-NON-STRESS</option>
<option value="2023WS TMeLS-R (Drt) EST006 S2 IN JDA-RRS Stress_IN-WB-BN-JDA-RRS-STRESS">2023WS TMeLS-R (Drt) EST006 S2 IN JDA-RRS Stress_IN-WB-BN-JDA-RRS-STRESS</option>
<option value="2023WS TMeLS-R (Drt) EST006 S2 IN SHUATS Stress_IN-UP-PY-SHUATS-STRESS">2023WS TMeLS-R (Drt) EST006 S2 IN SHUATS Stress_IN-UP-PY-SHUATS-STRESS</option></select>
<script type="application/json" data-for="staApp_1-envSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="staApp_1-out8670cb78277ec019" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

The following boxplot allows you to see the distribution of predicted values by trait (y-axis) in the different environments to double check that everything looks OK.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitStaBox-label" for="staApp_1-traitStaBox">Trait to filter:</label>
<div>
<select id="staApp_1-traitStaBox" class="shiny-input-select"><option value="YLDTONF" selected>YLDTONF</option></select>
<script type="application/json" data-for="staApp_1-traitStaBox" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-out6807d618779bca5d" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Per-environment merit estimates of top entries

In the following plot you can observe the comparison between the top 30 entries from each entry type category for the different traits. If a category has less than a 30 entries all individuals are displayed. This should allow you to identify the top entries in each environment. We would NOT recommend you to use this for selection of parents or products. Wait until you have the results of the multi-trial analysis and selection indices.

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitStaComp-label" for="staApp_1-traitStaComp">Trait to filter:</label>
<div>
<select id="staApp_1-traitStaComp" class="shiny-input-select"><option value="YLDTONF" selected>YLDTONF</option></select>
<script type="application/json" data-for="staApp_1-traitStaComp" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-out442af683407166d3" style="width:100%;height:400px;"></div><!--/html_preserve-->


### Correlation between environments

The following plot aims to show the correlation between BLUEs or BLUPs (depending on the parameter settings) among the different environments for the traits available in order to identify if there is one or more environments that do not align with the target population of environments (i.e., negatively correlated with the main cluster across most environments). You may want to exclude such environments from the multi-trial analysis (MTA) to ensure that selected entries in the MTA achieve genetic gain in the main cluster of environments.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitStaCor-label" for="staApp_1-traitStaCor">Trait to filter:</label>
<div>
<select id="staApp_1-traitStaCor" class="shiny-input-select"><option value="YLDTONF" selected>YLDTONF</option></select>
<script type="application/json" data-for="staApp_1-traitStaCor" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-oute1b943b468ac9f33" style="width:100%;height:400px;"></div><!--/html_preserve-->


### References of methods used

Velazco, J. G., Rodriguez-Alvarez, M. X., Boer, M. P., Jordan, D. R., Eilers, P. H., Malosetti, M., & Van Eeuwijk, F. A. (2017). Modelling spatial trends in sorghum breeding field trials using a two-dimensional P-spline mixed model. Theoretical and Applied Genetics, 130, 1375-1392.

Rodriguez-Alvarez, M. X., Boer, M. P., van Eeuwijk, F. A., & Eilers, P. H. (2018). Correcting for spatial heterogeneity in plant breeding experiments with P-splines. Spatial Statistics, 23, 52-71.

R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

Boer M, van Rossum B (2022). LMMsolver: Linear Mixed Model Solver. R package version 1.0.4.9000.

<p>&nbsp;</p>

