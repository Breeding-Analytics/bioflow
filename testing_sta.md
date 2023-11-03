---
title: "xxxx"
subtitle: "gggg"
author: "fff"
date: "`rrrr"  
# date last modified: March 9, 2023

  doc_title: "Single-Trial Analysis"
  doc_subtitle: ""
  # is_full_report: TRUE
  # is_extended_report: TRUE
output: 
  rmdformats::robobook:
  # html_document:
    # toc: TRUE
    toc_depth: 4
    # toc_float:
    #   collapsed: FALSE
    #   smooth_scroll: FALSE
---

<style>
.main-container {
  max-width: 200%;
  margin-left: 0;
  margin-right: 0;
}
.tab-content {
  margin-bottom: 50px;
}
.book .book-body .page-inner {
  max-width: 2000px;
}
</style>




## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


```r
summary(cars)
```

```
##      speed           dist       
##  Min.   : 4.0   Min.   :  2.00  
##  1st Qu.:12.0   1st Qu.: 26.00  
##  Median :15.0   Median : 36.00  
##  Mean   :15.4   Mean   : 42.98  
##  3rd Qu.:19.0   3rd Qu.: 56.00  
##  Max.   :25.0   Max.   :120.00
```

## Including Plots

You can also embed plots, for example:

![plot of chunk pressure](figure/pressure-1.png)

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.


