---
title: "Single Trial Analysis Report"
author: "Contact:<a href = 'https://github.com/Breeding-Analytics/bioflow' target = '_blank'>Breeding Analytics Team, OneCGIAR</a> breedinganalytics@cgiar.org"
date: "August 22, 2024"  
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




No coordinates available. Skipping planting map.

### Entries and traits by environment table

The following table allows to see how many locations had data for the different traits. You may want to review if the phenotyping capacity can deal with the complexity of the trait (e.g., genotype by environment interaction) or if more resources should be deployed. Also you may want to check if you collected information from all the trials conducted.

<p>&nbsp;</p>

<div style="border: 1px solid #ddd; padding: 5px; overflow-x: scroll; width:100%; "><table class="table table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> environment </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> Oil (%) </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> Grain Yield (ton/ha) </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> Days to Flowering </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> Lodging Score (1-5) </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> Seed Weight (g) </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> Days to Maturity </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> Shattering Score (1-5) </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> Protein (%) </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> Plant Height (cm) </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> Number of entries </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2015/2016_Kenya_Kajiado </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 23 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2015/2016_Kenya_Kitengela </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 23 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2015/2016_Kenya_Narok </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2015/2016_Kenya_Thika </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 23 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2016_Kenya_Kitengela </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2016_Kenya_Thika </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2016/2017_Kenya_Machakos </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 20 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2016/2017_Kenya_Thika </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 20 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2016/2017_Mali_Cinzana </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 22 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2016/2017_Mali_Kita </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 22 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2017_Kenya_Kamur </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2017_Kenya_Machakos </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 20 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2017_Kenya_Madrugada </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2017_Kenya_Mombasa </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 20 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2017/2018_Kenya_Machakos </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 20 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2017/2018_Malawi_Baka </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2017/2018_Malawi_Bvumbwe </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2017/2018_Malawi_Chilanga </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2017/2018_Malawi_Chitala </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2017/2018_Malawi_Chitedze </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2017/2018_Malawi_Mpale </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2017/2018_Malawi_Mtunthama </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2017/2018_Malawi_Nkhozo </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2017/2018_Mali_Cinzana </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2017/2018_Mali_Kita </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 11 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2017/2018_Mali_Koutiala </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018_Cameroon_Foumbot </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 41 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018_Cameroon_Garoua </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018_Cameroon_Mbalmayo </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 43 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018_Cameroon_Yaounde </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 42 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018_Malawi_Bwanje </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018_Malawi_Chipata </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018_Malawi_Mpale (Dowa 1) </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018_Malawi_Mpale (Dowa 2) </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018_Uganda_Bweyale </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 34 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018_Uganda_Jinja </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 35 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018_Uganda_Lira </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 35 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018_Uganda_Mubuku </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 35 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018/2019_Malawi_Baka </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018/2019_Malawi_Bvumbwe </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018/2019_Malawi_Chilanga </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018/2019_Malawi_Chipata </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018/2019_Malawi_Chitala </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018/2019_Malawi_Chitedze </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018/2019_Malawi_Lisungwi </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018/2019_Malawi_Mpale </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018/2019_Mozambique_Angonia </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 32 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018/2019_Mozambique_Gurue </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 32 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018/2019_Mozambique_Phoenix </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018/2019_Rwanda_Bugesera </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 25 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018/2019_Rwanda_Nyagatare </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 25 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018/2019_Rwanda_Rubona </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 25 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018/2019_Zimbabwe_ART Farm </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018/2019_Zimbabwe_Banket </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018/2019_Zimbabwe_Bindura </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018/2019_Zimbabwe_Chisumbanje </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018/2019_Zimbabwe_Harare </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018/2019_Zimbabwe_Kadoma </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018/2019_Zimbabwe_Mutare </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018/2019_Zimbabwe_Panmure </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018/2019_Zimbabwe_RARS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018/2019_Zimbabwe_Stapleford </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Benin_Djidja </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Benin_Glazoue </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Benin_Kerou </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Benin_Nikki </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Benin_Savalou </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Cameroon_Foumbot </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 21 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Cameroon_Mbalmayo </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 21 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Cameroon_Nkolbisson </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 21 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Ethiopia_Jimma </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Ethiopia_Pawe </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Ghana_Bamahu </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Ghana_Ejura </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Ghana_Fumesua </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Ghana_Nyankpala </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Malawi_Bwanje </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Malawi_Chilanga </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Malawi_Chipata </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Malawi_Domasi </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Malawi_Mpale </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Mali_Kita </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Mali_Koutiala </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Mali_M Pessoba </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Mali_Sikasso </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Nigeria_Ibadan </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 45 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Nigeria_Zaria </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 42 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Sudan_Wad Medani </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 44 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Uganda_Abi Zardi </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 35 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Uganda_Jinja </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 35 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Uganda_Kabanyolo </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 35 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Uganda_Kigumba </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 32 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Uganda_Mubuku </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 35 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Zimbabwe_Chisumbanje </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019_Zimbabwe_Panmure </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Kenya_Bungoma </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Kenya_Kiambu </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Kenya_Kibwezi </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Kenya_Njoro </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Malawi_Bvumbwe </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Malawi_Chilanga </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Malawi_Chipata </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Malawi_Chitala </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Malawi_Chitedze </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Malawi_Mbabzi </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Malawi_Mbawa </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Malawi_Mpale </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Mozambique_Angonia </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 31 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Mozambique_Gurue </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 31 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Zambia_Chibombo (IITA_SARAH) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Zambia_Chongwe (MRI Small) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Zambia_Kabwe </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Zambia_Kapilyomba </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Zambia_Kasama </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Zambia_Mpongwe </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Zimbabwe_Banket </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Zimbabwe_Chisumbanje </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Zimbabwe_Harare (CBI) </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Zimbabwe_Harare (Seed Co) </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Zimbabwe_Kadoma </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Zimbabwe_Mutare </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Zimbabwe_Panmure </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Zimbabwe_RARS </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2019/2020_Zimbabwe_Stapleford </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020_Malawi_Bwanje </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020_Malawi_Chilanga </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020_Malawi_Domasi </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020_Malawi_Mpale </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020_Myanmar_Lawksawk </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 21 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020_Myanmar_Pindaya </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 21 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2020_Myanmar_Yezin_NPT </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> ✅ </td>
   <td style="text-align:right;"> 20 </td>
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
<select id="staApp_1-traitSta" class="shiny-input-select"><option value="Grain Yield (ton/ha)" selected>Grain Yield (ton/ha)</option>
<option value="Days to Maturity">Days to Maturity</option>
<option value="Protein (%)">Protein (%)</option>
<option value="Oil (%)">Oil (%)</option>
<option value="Plant Height (cm)">Plant Height (cm)</option>
<option value="Days to Flowering">Days to Flowering</option>
<option value="Seed Weight (g)">Seed Weight (g)</option>
<option value="Lodging Score (1-5)">Lodging Score (1-5)</option>
<option value="Shattering Score (1-5)">Shattering Score (1-5)</option></select>
<script type="application/json" data-for="staApp_1-traitSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="staApp_1-out8cb9c92e681ec78b" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>



<p>&nbsp;</p>





### Output parameters 

<p>&nbsp;</p>

This barplot allows you to see the variance components values and ratios for the trait by environment combinations and identify good quality trials.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitSta0-label" for="staApp_1-traitSta0">Trait to filter:</label>
<div>
<select id="staApp_1-traitSta0" class="shiny-input-select"><option value="Grain Yield (ton/ha)" selected>Grain Yield (ton/ha)</option>
<option value="Days to Maturity">Days to Maturity</option>
<option value="Protein (%)">Protein (%)</option>
<option value="Oil (%)">Oil (%)</option>
<option value="Plant Height (cm)">Plant Height (cm)</option>
<option value="Days to Flowering">Days to Flowering</option>
<option value="Seed Weight (g)">Seed Weight (g)</option>
<option value="Lodging Score (1-5)">Lodging Score (1-5)</option>
<option value="Shattering Score (1-5)">Shattering Score (1-5)</option></select>
<script type="application/json" data-for="staApp_1-traitSta0" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-out264f5324e55daf62" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

<!--html_preserve--><div class="shiny-input-panel">
<div class="shiny-flow-layout">
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-parameterMetrics-label" for="staApp_1-parameterMetrics">Parameter to filter:</label>
<div>
<select id="staApp_1-parameterMetrics" class="shiny-input-select"><option value="plotH2" selected>plotH2</option>
<option value="CV" selected>CV</option>
<option value="r2" selected>r2</option>
<option value="V_designation" selected>V_designation</option>
<option value="V_residual" selected>V_residual</option>
<option value="mean" selected>mean</option></select>
<script type="application/json" data-for="staApp_1-parameterMetrics" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
<div>
<div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-parameterMetricsBy-label" for="staApp_1-parameterMetricsBy">View x-axis by:</label>
<div>
<select id="staApp_1-parameterMetricsBy" class="shiny-input-select"><option value="environment" selected>environment</option>
<option value="trait">trait</option></select>
<script type="application/json" data-for="staApp_1-parameterMetricsBy" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div>
</div>
</div>
</div><!--/html_preserve-->
<p>&nbsp;</p>

The following barplot is designed to provide a high-level view of estimated parameters such as reliability, heritabiliy, coefficient of variation and others.

<p>&nbsp;</p>

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-outce742e5cb2388e70" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Predictions

The adjusted means in the following visuualizations are the result of fitting a experimental-design agnostic mixed model where everything that can be fitted will be fitted in order to remove as much spatial noise as possible. That means that if a trial has block and incomplete block information both will be fitted. If the trial has also row and column information it will also be fitted together with a spatial kernel (Rodriguez-Alvarez et al., 2018). These table of adjusted means will be used as input information for the multi-trial analysis. We recommend you to don't take any selection decision at this point and wait until the multi-trial analysis is fitted.

The following table allows you to check the trait by environment adjusted means for the different individuals in wide format.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-envSta-label" for="staApp_1-envSta">Environment to filter:</label>
<div>
<select id="staApp_1-envSta" class="shiny-input-select"><option value="2019_Malawi_Mpale" selected>2019_Malawi_Mpale</option>
<option value="2020_Malawi_Mpale">2020_Malawi_Mpale</option>
<option value="2020_Myanmar_Lawksawk">2020_Myanmar_Lawksawk</option>
<option value="2019_Malawi_Domasi">2019_Malawi_Domasi</option>
<option value="2020_Malawi_Bwanje">2020_Malawi_Bwanje</option>
<option value="2020_Myanmar_Yezin_NPT">2020_Myanmar_Yezin_NPT</option>
<option value="2019_Malawi_Chilanga">2019_Malawi_Chilanga</option>
<option value="2020_Malawi_Chilanga">2020_Malawi_Chilanga</option>
<option value="2019_Malawi_Chipata">2019_Malawi_Chipata</option>
<option value="2020_Malawi_Domasi">2020_Malawi_Domasi</option>
<option value="2019_Malawi_Bwanje">2019_Malawi_Bwanje</option>
<option value="2020_Myanmar_Pindaya">2020_Myanmar_Pindaya</option>
<option value="2017/2018_Malawi_Chilanga">2017/2018_Malawi_Chilanga</option>
<option value="2019_Ghana_Fumesua">2019_Ghana_Fumesua</option>
<option value="2019_Mali_Kita">2019_Mali_Kita</option>
<option value="2017/2018_Malawi_Nkhozo">2017/2018_Malawi_Nkhozo</option>
<option value="2019_Benin_Djidja">2019_Benin_Djidja</option>
<option value="2019_Mali_Koutiala">2019_Mali_Koutiala</option>
<option value="2018_Cameroon_Foumbot">2018_Cameroon_Foumbot</option>
<option value="2019_Ghana_Nyankpala">2019_Ghana_Nyankpala</option>
<option value="2019_Mali_Sikasso">2019_Mali_Sikasso</option>
<option value="2019_Benin_Glazoue">2019_Benin_Glazoue</option>
<option value="2018_Cameroon_Mbalmayo">2018_Cameroon_Mbalmayo</option>
<option value="2017/2018_Malawi_Bvumbwe">2017/2018_Malawi_Bvumbwe</option>
<option value="2018_Cameroon_Yaounde">2018_Cameroon_Yaounde</option>
<option value="2019_Ghana_Bamahu">2019_Ghana_Bamahu</option>
<option value="2019_Benin_Nikki">2019_Benin_Nikki</option>
<option value="2019_Cameroon_Mbalmayo">2019_Cameroon_Mbalmayo</option>
<option value="2019_Cameroon_Nkolbisson">2019_Cameroon_Nkolbisson</option>
<option value="2016/2017_Mali_Cinzana">2016/2017_Mali_Cinzana</option>
<option value="2017/2018_Mali_Kita">2017/2018_Mali_Kita</option>
<option value="2019_Sudan_Wad Medani">2019_Sudan_Wad Medani</option>
<option value="2017/2018_Malawi_Chitala">2017/2018_Malawi_Chitala</option>
<option value="2019_Benin_Savalou">2019_Benin_Savalou</option>
<option value="2017/2018_Malawi_Chitedze">2017/2018_Malawi_Chitedze</option>
<option value="2019_Benin_Kerou">2019_Benin_Kerou</option>
<option value="2019_Nigeria_Ibadan">2019_Nigeria_Ibadan</option>
<option value="2019_Mali_M Pessoba">2019_Mali_M Pessoba</option>
<option value="2018_Cameroon_Garoua">2018_Cameroon_Garoua</option>
<option value="2016/2017_Mali_Kita">2016/2017_Mali_Kita</option>
<option value="2017/2018_Malawi_Baka">2017/2018_Malawi_Baka</option>
<option value="2017/2018_Malawi_Mtunthama">2017/2018_Malawi_Mtunthama</option>
<option value="2017/2018_Malawi_Mpale">2017/2018_Malawi_Mpale</option>
<option value="2019_Ghana_Ejura">2019_Ghana_Ejura</option>
<option value="2017/2018_Mali_Cinzana">2017/2018_Mali_Cinzana</option>
<option value="2017/2018_Mali_Koutiala">2017/2018_Mali_Koutiala</option>
<option value="2019_Nigeria_Zaria">2019_Nigeria_Zaria</option>
<option value="2019_Ethiopia_Pawe">2019_Ethiopia_Pawe</option>
<option value="2019_Ethiopia_Jimma">2019_Ethiopia_Jimma</option>
<option value="2019/2020_Zambia_Chongwe (MRI Small)">2019/2020_Zambia_Chongwe (MRI Small)</option>
<option value="2019/2020_Zambia_Mpongwe">2019/2020_Zambia_Mpongwe</option>
<option value="2019/2020_Zambia_Kasama">2019/2020_Zambia_Kasama</option>
<option value="2019/2020_Zimbabwe_Harare (Seed Co)">2019/2020_Zimbabwe_Harare (Seed Co)</option>
<option value="2019/2020_Zimbabwe_Chisumbanje">2019/2020_Zimbabwe_Chisumbanje</option>
<option value="2019/2020_Zimbabwe_Banket">2019/2020_Zimbabwe_Banket</option>
<option value="2019/2020_Zimbabwe_Panmure">2019/2020_Zimbabwe_Panmure</option>
<option value="2019/2020_Zimbabwe_Kadoma">2019/2020_Zimbabwe_Kadoma</option>
<option value="2019_Zimbabwe_Chisumbanje">2019_Zimbabwe_Chisumbanje</option>
<option value="2019/2020_Zimbabwe_RARS">2019/2020_Zimbabwe_RARS</option>
<option value="2019/2020_Zimbabwe_Harare (CBI)">2019/2020_Zimbabwe_Harare (CBI)</option>
<option value="2019_Zimbabwe_Panmure">2019_Zimbabwe_Panmure</option>
<option value="2019/2020_Zambia_Kapilyomba">2019/2020_Zambia_Kapilyomba</option>
<option value="2019/2020_Zimbabwe_Stapleford">2019/2020_Zimbabwe_Stapleford</option>
<option value="2019/2020_Zambia_Chibombo (IITA_SARAH)">2019/2020_Zambia_Chibombo (IITA_SARAH)</option>
<option value="2019/2020_Zimbabwe_Mutare">2019/2020_Zimbabwe_Mutare</option>
<option value="2019/2020_Zambia_Kabwe">2019/2020_Zambia_Kabwe</option>
<option value="2018/2019_Mozambique_Angonia">2018/2019_Mozambique_Angonia</option>
<option value="2018_Malawi_Mpale (Dowa 1)">2018_Malawi_Mpale (Dowa 1)</option>
<option value="2016_Kenya_Thika">2016_Kenya_Thika</option>
<option value="2018/2019_Malawi_Chipata">2018/2019_Malawi_Chipata</option>
<option value="2018/2019_Malawi_Chilanga">2018/2019_Malawi_Chilanga</option>
<option value="2018_Malawi_Chipata">2018_Malawi_Chipata</option>
<option value="2018/2019_Mozambique_Gurue">2018/2019_Mozambique_Gurue</option>
<option value="2016/2017_Kenya_Thika">2016/2017_Kenya_Thika</option>
<option value="2016_Kenya_Kitengela">2016_Kenya_Kitengela</option>
<option value="2018/2019_Mozambique_Phoenix">2018/2019_Mozambique_Phoenix</option>
<option value="2017/2018_Kenya_Machakos">2017/2018_Kenya_Machakos</option>
<option value="2018/2019_Malawi_Lisungwi">2018/2019_Malawi_Lisungwi</option>
<option value="2016/2017_Kenya_Machakos">2016/2017_Kenya_Machakos</option>
<option value="2017_Kenya_Machakos">2017_Kenya_Machakos</option>
<option value="2017_Kenya_Mombasa">2017_Kenya_Mombasa</option>
<option value="2018_Malawi_Bwanje">2018_Malawi_Bwanje</option>
<option value="2018_Malawi_Mpale (Dowa 2)">2018_Malawi_Mpale (Dowa 2)</option>
<option value="2015/2016_Kenya_Kajiado">2015/2016_Kenya_Kajiado</option>
<option value="2015/2016_Kenya_Thika">2015/2016_Kenya_Thika</option>
<option value="2015/2016_Kenya_Kitengela">2015/2016_Kenya_Kitengela</option>
<option value="2017_Kenya_Madrugada">2017_Kenya_Madrugada</option>
<option value="2017_Kenya_Kamur">2017_Kenya_Kamur</option>
<option value="2018/2019_Rwanda_Nyagatare">2018/2019_Rwanda_Nyagatare</option>
<option value="2018/2019_Rwanda_Bugesera">2018/2019_Rwanda_Bugesera</option>
<option value="2018/2019_Rwanda_Rubona">2018/2019_Rwanda_Rubona</option>
<option value="2019_Uganda_Jinja">2019_Uganda_Jinja</option>
<option value="2018/2019_Zimbabwe_Banket">2018/2019_Zimbabwe_Banket</option>
<option value="2019/2020_Kenya_Bungoma">2019/2020_Kenya_Bungoma</option>
<option value="2018/2019_Malawi_Bvumbwe">2018/2019_Malawi_Bvumbwe</option>
<option value="2018/2019_Zimbabwe_Harare">2018/2019_Zimbabwe_Harare</option>
<option value="2018/2019_Malawi_Baka">2018/2019_Malawi_Baka</option>
<option value="2018/2019_Malawi_Chitala">2018/2019_Malawi_Chitala</option>
<option value="2018/2019_Malawi_Chitedze">2018/2019_Malawi_Chitedze</option>
<option value="2018/2019_Zimbabwe_Chisumbanje">2018/2019_Zimbabwe_Chisumbanje</option>
<option value="2019_Uganda_Mubuku">2019_Uganda_Mubuku</option>
<option value="2018/2019_Zimbabwe_Kadoma">2018/2019_Zimbabwe_Kadoma</option>
<option value="2018_Uganda_Mubuku">2018_Uganda_Mubuku</option>
<option value="2018/2019_Zimbabwe_ART Farm">2018/2019_Zimbabwe_ART Farm</option>
<option value="2019_Uganda_Kabanyolo">2019_Uganda_Kabanyolo</option>
<option value="2018/2019_Zimbabwe_Bindura">2018/2019_Zimbabwe_Bindura</option>
<option value="2018/2019_Zimbabwe_Panmure">2018/2019_Zimbabwe_Panmure</option>
<option value="2018/2019_Zimbabwe_Mutare">2018/2019_Zimbabwe_Mutare</option>
<option value="2019/2020_Kenya_Njoro">2019/2020_Kenya_Njoro</option>
<option value="2019/2020_Kenya_Kiambu">2019/2020_Kenya_Kiambu</option>
<option value="2018/2019_Zimbabwe_RARS">2018/2019_Zimbabwe_RARS</option>
<option value="2018/2019_Zimbabwe_Stapleford">2018/2019_Zimbabwe_Stapleford</option>
<option value="2018/2019_Malawi_Mpale">2018/2019_Malawi_Mpale</option>
<option value="2018_Uganda_Bweyale">2018_Uganda_Bweyale</option>
<option value="2019_Uganda_Abi Zardi">2019_Uganda_Abi Zardi</option>
<option value="2019_Uganda_Kigumba">2019_Uganda_Kigumba</option>
<option value="2019/2020_Kenya_Kibwezi">2019/2020_Kenya_Kibwezi</option>
<option value="2018_Uganda_Jinja">2018_Uganda_Jinja</option>
<option value="2018_Uganda_Lira">2018_Uganda_Lira</option>
<option value="2019_Cameroon_Foumbot">2019_Cameroon_Foumbot</option>
<option value="2019/2020_Mozambique_Gurue">2019/2020_Mozambique_Gurue</option>
<option value="2019/2020_Malawi_Chitala">2019/2020_Malawi_Chitala</option>
<option value="2019/2020_Malawi_Bvumbwe">2019/2020_Malawi_Bvumbwe</option>
<option value="2019/2020_Malawi_Mbawa">2019/2020_Malawi_Mbawa</option>
<option value="2019/2020_Mozambique_Angonia">2019/2020_Mozambique_Angonia</option>
<option value="2019/2020_Malawi_Mpale">2019/2020_Malawi_Mpale</option>
<option value="2019/2020_Malawi_Mbabzi">2019/2020_Malawi_Mbabzi</option>
<option value="2019/2020_Malawi_Chilanga">2019/2020_Malawi_Chilanga</option>
<option value="2019/2020_Malawi_Chitedze">2019/2020_Malawi_Chitedze</option>
<option value="2019/2020_Malawi_Chipata">2019/2020_Malawi_Chipata</option></select>
<script type="application/json" data-for="staApp_1-envSta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="staApp_1-out58477f509c8809cf" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

The following boxplot allows you to see the distribution of predicted values by trait (y-axis) in the different environments to double check that everything looks OK.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitStaBox-label" for="staApp_1-traitStaBox">Trait to filter:</label>
<div>
<select id="staApp_1-traitStaBox" class="shiny-input-select"><option value="Grain Yield (ton/ha)" selected>Grain Yield (ton/ha)</option>
<option value="Days to Maturity" selected>Days to Maturity</option>
<option value="Protein (%)" selected>Protein (%)</option>
<option value="Oil (%)" selected>Oil (%)</option>
<option value="Plant Height (cm)" selected>Plant Height (cm)</option>
<option value="Days to Flowering" selected>Days to Flowering</option>
<option value="Seed Weight (g)" selected>Seed Weight (g)</option>
<option value="Lodging Score (1-5)" selected>Lodging Score (1-5)</option>
<option value="Shattering Score (1-5)" selected>Shattering Score (1-5)</option></select>
<script type="application/json" data-for="staApp_1-traitStaBox" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-out39a1a6ec8672a9dd" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Per-environment merit estimates of top entries

In the following plot you can observe the comparison between the top 30 entries from each entry type category for the different traits. If a category has less than a 30 entries all individuals are displayed. This should allow you to identify the top entries in each environment. We would NOT recommend you to use this for selection of parents or products. Wait until you have the results of the multi-trial analysis and selection indices.

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitStaComp-label" for="staApp_1-traitStaComp">Trait to filter:</label>
<div>
<select id="staApp_1-traitStaComp" class="shiny-input-select"><option value="Grain Yield (ton/ha)" selected>Grain Yield (ton/ha)</option>
<option value="Days to Maturity" selected>Days to Maturity</option>
<option value="Protein (%)" selected>Protein (%)</option>
<option value="Oil (%)" selected>Oil (%)</option>
<option value="Plant Height (cm)" selected>Plant Height (cm)</option>
<option value="Days to Flowering" selected>Days to Flowering</option>
<option value="Seed Weight (g)" selected>Seed Weight (g)</option>
<option value="Lodging Score (1-5)" selected>Lodging Score (1-5)</option>
<option value="Shattering Score (1-5)" selected>Shattering Score (1-5)</option></select>
<script type="application/json" data-for="staApp_1-traitStaComp" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-outfbed53bc6742a577" style="width:100%;height:400px;"></div><!--/html_preserve-->


### Correlation between environments

The following plot aims to show the correlation between BLUEs or BLUPs (depending on the parameter settings) among the different environments for the traits available in order to identify if there is one or more environments that do not align with the target population of environments (i.e., negatively correlated with the main cluster across most environments). You may want to exclude such environments from the multi-trial analysis (MTA) to ensure that selected entries in the MTA achieve genetic gain in the main cluster of environments.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="staApp_1-traitStaCor-label" for="staApp_1-traitStaCor">Trait to filter:</label>
<div>
<select id="staApp_1-traitStaCor" class="shiny-input-select"><option value="Grain Yield (ton/ha)" selected>Grain Yield (ton/ha)</option>
<option value="Days to Maturity" selected>Days to Maturity</option>
<option value="Protein (%)" selected>Protein (%)</option>
<option value="Oil (%)" selected>Oil (%)</option>
<option value="Plant Height (cm)" selected>Plant Height (cm)</option>
<option value="Days to Flowering" selected>Days to Flowering</option>
<option value="Seed Weight (g)" selected>Seed Weight (g)</option>
<option value="Lodging Score (1-5)" selected>Lodging Score (1-5)</option>
<option value="Shattering Score (1-5)" selected>Shattering Score (1-5)</option></select>
<script type="application/json" data-for="staApp_1-traitStaCor" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="staApp_1-out9481f466730ed019" style="width:100%;height:400px;"></div><!--/html_preserve-->


### References of methods used

Velazco, J. G., Rodriguez-Alvarez, M. X., Boer, M. P., Jordan, D. R., Eilers, P. H., Malosetti, M., & Van Eeuwijk, F. A. (2017). Modelling spatial trends in sorghum breeding field trials using a two-dimensional P-spline mixed model. Theoretical and Applied Genetics, 130, 1375-1392.

Rodriguez-Alvarez, M. X., Boer, M. P., van Eeuwijk, F. A., & Eilers, P. H. (2018). Correcting for spatial heterogeneity in plant breeding experiments with P-splines. Spatial Statistics, 23, 52-71.

R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

Boer M, van Rossum B (2022). LMMsolver: Linear Mixed Model Solver. R package version 1.0.4.9000.

<p>&nbsp;</p>

