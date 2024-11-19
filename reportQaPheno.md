---
title: "Quality Assurance Phenotypic Report"
author: "Contact:<a href = 'https://github.com/Breeding-Analytics/bioflow' target = '_blank'>Breeding Analytics Team, OneCGIAR</a> breedinganalytics@cgiar.org"
date: "November 19, 2024"  
output: html_document
params:
  toDownload: FALSE
---








### Table of modifications
<p>&nbsp;</p>

The following table shows the modifications that have been saved in your analysis object and that will be applied to the raw phenotypic data when this specific time stamp (ID) is selected/used in other analytical modules (e.g., single trial analysis module).

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="qaPhenoApp_1-traitQa-label" for="qaPhenoApp_1-traitQa">Trait:</label>
<div>
<select id="qaPhenoApp_1-traitQa" class="shiny-input-select"><option value="Canopy" selected>Canopy</option>
<option value="NDVI">NDVI</option>
<option value="Mean.Plant.Height">Mean.Plant.Height</option>
<option value="Average.Leaf.Angle">Average.Leaf.Angle</option>
<option value="Canopy.Chlorophyll.Content">Canopy.Chlorophyll.Content</option>
<option value="CI.Green">CI.Green</option>
<option value="CI.Red.Edge">CI.Red.Edge</option>
<option value="FiPAR">FiPAR</option>
<option value="LCI">LCI</option>
<option value="Leaf.Area.Index">Leaf.Area.Index</option>
<option value="Leaf.Chlorophyll.Content">Leaf.Chlorophyll.Content</option>
<option value="Max.Plant.Height">Max.Plant.Height</option>
<option value="Mean.Plant.Height.Heterogeneity">Mean.Plant.Height.Heterogeneity</option>
<option value="MCARI">MCARI</option>
<option value="MTCI">MTCI</option>
<option value="NDRE">NDRE</option>
<option value="Plant.Biovolume">Plant.Biovolume</option>
<option value="Plant.Biovolume.Heterogeneity">Plant.Biovolume.Heterogeneity</option></select>
<script type="application/json" data-for="qaPhenoApp_1-traitQa" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->


<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="qaPhenoApp_1-out2594d4843c91351d" style="width:100%;height:auto;"></div><!--/html_preserve-->



<p>&nbsp;</p>

### Boxplot of modifications

The following boxplot allows you to visualize which records were tagged as outliers and will be ignored in posterior analyses if this QA time stamp is selected in the posterior analyses.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="qaPhenoApp_1-traitQaBox-label" for="qaPhenoApp_1-traitQaBox">Trait:</label>
<div>
<select id="qaPhenoApp_1-traitQaBox" class="shiny-input-select"><option value="Canopy" selected>Canopy</option>
<option value="NDVI">NDVI</option>
<option value="Mean.Plant.Height">Mean.Plant.Height</option>
<option value="Average.Leaf.Angle">Average.Leaf.Angle</option>
<option value="Canopy.Chlorophyll.Content">Canopy.Chlorophyll.Content</option>
<option value="CI.Green">CI.Green</option>
<option value="CI.Red.Edge">CI.Red.Edge</option>
<option value="FiPAR">FiPAR</option>
<option value="LCI">LCI</option>
<option value="Leaf.Area.Index">Leaf.Area.Index</option>
<option value="Leaf.Chlorophyll.Content">Leaf.Chlorophyll.Content</option>
<option value="Max.Plant.Height">Max.Plant.Height</option>
<option value="Mean.Plant.Height.Heterogeneity">Mean.Plant.Height.Heterogeneity</option>
<option value="MCARI">MCARI</option>
<option value="MTCI">MTCI</option>
<option value="NDRE">NDRE</option>
<option value="Plant.Biovolume">Plant.Biovolume</option>
<option value="Plant.Biovolume.Heterogeneity">Plant.Biovolume.Heterogeneity</option></select>
<script type="application/json" data-for="qaPhenoApp_1-traitQaBox" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="shiny-plot-output html-fill-item" id="qaPhenoApp_1-out36c6617328dec28f" style="width:100%;height:400px;"></div><!--/html_preserve-->



### References on methods used

Tukey, J. W. (1977). Exploratory Data Analysis. Section 2C.

McGill, R., Tukey, J. W. and Larsen, W. A. (1978). Variations of box plots. The American Statistician, 32, 12–16. doi:10.2307/2683468.

Velleman, P. F. and Hoaglin, D. C. (1981). Applications, Basics and Computing of Exploratory Data Analysis. Duxbury Press.


<p>&nbsp;</p>





