---
title: "F1 QA/QC Report"
author: "Contact:<a href = 'https://github.com/Breeding-Analytics/bioflow' target = '_blank'>Breeding Analytics Team, OneCGIAR</a> breedinganalytics@cgiar.org"
date: "September 03, 2025"  
output: html_document
params:
  toDownload: FALSE
---






### Inputs and outputs of F1 (QA/QC)


1. INPUT: Genotypic information of F1 material and Parents to be verified 

2. INPUT: Pedigree information connecting each F1 individual to two parents 

3. INPUT: Genotype QA/QC results 

3. OUTPUT: Probability that the F1 individual matches the genotype of the parents 

4. OUTPUT: across environment predictions for each trait (output) 

7. Individual sensitivity and stability values across environments (output)

8. Percent check comparison against the different benchmark varieties present in the dataset (output)

9. Genetic correlation between the traits (output)

Understanding these data features should allow the scientist to identify which traits express more genotype by environment interactions and how they should be selected. It should also allow the scientist to assess the correlation between traits and how the product profile should be addressed to maximize genetic gains while developing the needed varieties at the same time. Materials with the highest performance and sensitivity to the environment could be potential nominations for advancement although we recommend to don't select new parents or products until the results from the multi-trial analysis are analyzed with a selection index.  

### Verification results
<p>&nbsp;</p>

The following table shows the values associated for the individuals on different metrics such as probability of matching the expected genotype, heterozygosity, among others.

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="hybridityApp_1-traitQa-label" for="hybridityApp_1-traitQa">Verification measure:</label>
<div>
<select id="hybridityApp_1-traitQa" class="shiny-input-select"><option value="probMatch" selected>probMatch</option>
<option value="heteroMexp">heteroMexp</option>
<option value="avgScore">avgScore</option>
<option value="nMarkers">nMarkers</option>
<option value="HasPed">HasPed</option>
<option value="ParentHasGeno">ParentHasGeno</option>
<option value="parHetFilter">parHetFilter</option>
<option value="heteroMp">heteroMp</option>
<option value="heteroDeviation">heteroDeviation</option>
<option value="nScore2">nScore2</option>
<option value="HasGeno">HasGeno</option></select>
<script type="application/json" data-for="hybridityApp_1-traitQa" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="hybridityApp_1-outea94d0e08a60fc8f" style="width:100%;height:400px;"></div><!--/html_preserve-->


<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="hybridityApp_1-outbd01cfdb784b8f15" style="width:100%;height:auto;"></div><!--/html_preserve-->



### Verification Metrics

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="hybridityApp_1-out9b347d50ead9417b" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### Verification Modeling

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="hybridityApp_1-outfc136666c41a2f89" style="width:100%;height:auto;"></div><!--/html_preserve-->

<p>&nbsp;</p>

### References on methods used

Tukey, J. W. (1977). Exploratory Data Analysis. Section 2C.

Velleman, P. F. and Hoaglin, D. C. (1981). Applications, Basics and Computing of Exploratory Data Analysis. Duxbury Press.


<p>&nbsp;</p>



