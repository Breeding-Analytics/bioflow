---
title: "Quality Assurance Genotype Report"
author: "Contact:<a href = 'https://github.com/Breeding-Analytics/bioflow' target = '_blank'>Breeding Analytics Team, OneCGIAR</a> breedinganalytics@cgiar.org"
date: "September 02, 2025"  
output: html_document
params:
  toDownload: FALSE
---







### Overall Summary
<p>&nbsp;</p>

This table presents a side-by-side comparison of our dataset before and after applying the selected filtering steps, highlighting changes in key metrics: number of loci, number of individuals, overall missingness, mean individual heterozygosity, and mean minor-allele frequency (MAF).

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="qaGenoApp_1-outb75a168c568e0d49" style="width:100%;height:auto;"></div><!--/html_preserve-->

### Variant distribution

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="qaGenoApp_1-out87beffec6c054f31" style="width:100%;height:auto;"></div><!--/html_preserve-->



This plot shows the genomic positions of SNPs before (blue) and after (red) imputation, highlighting how the filtering process modify the distribution relative to the original dataset.
<div class="figure">
<img src="figure/unnamed-chunk-4-1.png" alt="plot of chunk unnamed-chunk-4" width="100%" />
<p class="caption">plot of chunk unnamed-chunk-4</p>
</div>

### Table of modifications
<p>&nbsp;</p>

The following table shows the modifications that have been saved in your analysis object and that will be applied to the raw genotype data when this specific time stamp (ID) is selected/used in other analytical modules (e.g., multi trial analysis module).

<!--html_preserve--><div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="qaGenoApp_1-out1dd33f577ede9fc0" style="width:100%;height:auto;"></div><!--/html_preserve-->


<p>&nbsp;</p>

### References on methods used

Tukey, J. W. (1977). Exploratory Data Analysis. Section 2C.

Velleman, P. F. and Hoaglin, D. C. (1981). Applications, Basics and Computing of Exploratory Data Analysis. Duxbury Press.


<p>&nbsp;</p>



