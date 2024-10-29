---
title: "Multi-Trial Analysis Report"
author: "Contact:<a href = 'https://github.com/Breeding-Analytics/bioflow' target = '_blank'>Breeding Analytics Team, OneCGIAR</a> breedinganalytics@cgiar.org"
date: "October 27, 2024"  
output: html_document
params:
  toDownload: FALSE
---









### Objectives of Multi-Trial Analysis

The objective of this dashboard is to help scientist to understand the following points:

1. Overall number of entries and entry types included in the multi trial analysis (input)

2. High-level of the phenotypic adjusted means included in the analysis (input)

3. Number of entries connecting the different environments per trait (input)

3. Phenotypic correlation between environments for the traits present (input)

4. Across environment KPIs per trait such as reliability and variance component ratios (output) 

5. Individual across environment predictions for each trait (output) 

6. Individual sensitivity and stability values across environments (output)

7. Percent check comparison against the different benchmark varieties present in the dataset (output)

8. Genetic correlation between the traits (output)

Understanding these data features should allow the scientist to identify which traits express more genotype by environment interactions and how they should be selected. It should also allow the scientist to assess the correlation between traits and how the product profile should be addressed to maximize genetic gains while developing the needed varieties at the same time. Materials with the highest performance and sensitivity to the environment could be potential nominations for advancement although we recommend to don't select new parents or products until the results from the multi-trial analysis are analyzed with a selection index.  

### Number of individuals per trait and entry type

The following table aims to make a high-level assessment of the different types of entries included in the analysis across environments.

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="mtaLMMsolveApp_1-out64911809b875e1cf" style="width:100%;height:auto;"></div><!--/html_preserve-->

### Map of trials planted

The following map allows you to assess the location where trials are planted.

No coordinates available. Skipping planting map.

### By environment merit distribution

The following boxplot allows you to inspect the distribution of adjusted means (y-axis) from the different environments for each trait that were used as input for the analysis. The environments are sorted by environmental mean to understand the slope of the regression fitted in the Finlay-Wilkinson model. It is recommended that you have at least 6 environments to fit the random regressions over the environmental indices or other weather and soil variables

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaLMMsolveApp_1-traitMta-label" for="mtaLMMsolveApp_1-traitMta"></label>
<div>
<select id="mtaLMMsolveApp_1-traitMta" class="shiny-input-select"></select>
<script type="application/json" data-for="mtaLMMsolveApp_1-traitMta" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="mtaLMMsolveApp_1-out499d652070a9a372" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Connectivity between environments

The following heatmap and histogram allows you to assess the connectivity (gennotypes in common) between different environments for a given trait. Our recommendation is that each pair of environments should at least have 30 genotypes in common in order to estimate genetic correlations and at least 2-3 entries in common to adjust across environment means. We classify as low everything below 30, intermediate connectivity between 30 to 60, and high everything above 60.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaLMMsolveApp_1-traitMtaConnect-label" for="mtaLMMsolveApp_1-traitMtaConnect"></label>
<div>
<select id="mtaLMMsolveApp_1-traitMtaConnect" class="shiny-input-select"></select>
<script type="application/json" data-for="mtaLMMsolveApp_1-traitMtaConnect" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item" id="mtaLMMsolveApp_1-out4aac1789d307ec09" style="width:100%;height:400px;"></div><!--/html_preserve-->

### Genetic correlations between environments

The following heatmap and histogram allows to assess the genetic correlations among the different environment one trait at a time. If the user modeled GxE, this matrix is calculated as the correlation between the environment-specific estimates for individuals. If a pure main-effect model is specified this matrix is calculated as the correlation between the single-trial analysis estimates. We recommend that only environments that are on-average positively correlated with the main cluster (i.e., rG>0) are included in the multi-trial analysis to guarantee decent rates of genetic gain and adapted products in the TPE.

<p>&nbsp;</p>



Only one level for environment found in predictions (you fitted a main effect model maybe?). Skipping correlation plot.

<p>&nbsp;</p>

### Biplot by trait

The following graph allows to see the clustering of different genotypes in the TPE for each trait. This can help you identify clusters of environments and better define the TPE.

<p>&nbsp;</p>



Only one level for environment found in predictions (you fitted a main effect or a Finlay-Wilkinson model maybe?). Skipping biplot.


<p>&nbsp;</p>

### Across-environment metrics

The following barplot aims to help you inspect the across environment estimates for multiple parameters from the multi-trial analysis such as across-environment reliability.

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="mtaLMMsolveApp_1-parameterMetrics2-label" for="mtaLMMsolveApp_1-parameterMetrics2">Parameter:</label>
<div>
<select id="mtaLMMsolveApp_1-parameterMetrics2" class="shiny-input-select" multiple="multiple"></select>
<script type="application/json" data-for="mtaLMMsolveApp_1-parameterMetrics2">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="mtaLMMsolveApp_1-outbadf379c69f5c0b4" style="width:100%;height:400px;"></div><!--/html_preserve-->

<p>&nbsp;</p>

The following graph allows you to assess the proportion of variance going to different components for the different traits. Above the bars you can see the value of the variance component for each factor. The residual variance is all the variance that could not be explained by the main effect and sensitivity effects. This values should be considered carefully depending of the genetic evaluation model used. For example, when sing the rrBLUP model the variance components reflect the marker variance and may look very small, but we can't conclude that there is not eough genetic signal.

<p>&nbsp;</p>




























