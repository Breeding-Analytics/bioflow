---
title: "Predicted Genetic Report"
author: ""
date: "December 2023"
output: html_document
runtime: shiny
---




### Predicted genetic gain

The following density plot allows you to see the expected change in the next generation compared to the current generation given the current genetic gain parameters (accuracy, intensity, genetic variance) obtained in a particular MET for the selected traits.

<p>&nbsp;</p>

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" id="pggApp_1-traitFilterPredictions2D2-label" for="pggApp_1-traitFilterPredictions2D2">Trait:</label>
<div>
<select id="pggApp_1-traitFilterPredictions2D2" class="shiny-input-select"><option value="YLD_TON" selected>YLD_TON</option>
<option value="HT_AVG">HT_AVG</option></select>
<script type="application/json" data-for="pggApp_1-traitFilterPredictions2D2" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
</div>
</div><!--/html_preserve-->

<!--html_preserve--><div class="plotly html-widget html-widget-output shiny-report-size shiny-report-theme html-fill-item-overflow-hidden html-fill-item" id="pggApp_1-out7717314117a74d39" style="width:100%;height:400px;"></div><!--/html_preserve-->




## Phenotypic Correlation of STA BLUEs between trials  {.tabset .tabset-pills}    

  

### YLD_TON {.tabset .tabset-pills}       

<div id="htmlwidget-f238f30e10b2d585947e" style="width:100%;height:504px;" class="plotly html-widget "></div>
<script type="application/json" data-for="htmlwidget-f238f30e10b2d585947e">{"x":{"data":[{"orientation":"v","width":[1.1724137931034448,1.1724137931034591,1.1724137931034448,1.1724137931034591,1.1724137931034448,1.1724137931034591,1.1724137931034448,1.1724137931034591,1.1724137931034448,1.1724137931034591,1.1724137931034448,1.1724137931034591,1.1724137931034448,1.1724137931034448,1.1724137931034591,1.1724137931034591,1.1724137931034448,1.1724137931034448,1.1724137931034377,1.1724137931034591,1.1724137931034306,1.1724137931034306,1.1724137931034591,1.1724137931034306,1.1724137931034591,1.1724137931034306,1.1724137931034306,1.1724137931034591,1.1724137931034306,1.1724137931034306],"base":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],"x":[43.37931034482758,44.551724137931032,45.724137931034477,46.896551724137922,48.068965517241374,49.241379310344826,50.41379310344827,51.586206896551715,52.758620689655167,53.931034482758619,55.103448275862064,56.275862068965509,57.448275862068961,58.620689655172406,59.793103448275858,60.965517241379303,62.137931034482754,63.310344827586199,64.482758620689651,65.655172413793096,66.827586206896541,68,69.172413793103445,70.34482758620689,71.517241379310335,72.689655172413779,73.862068965517238,75.034482758620683,76.206896551724128,77.379310344827587],"y":[1,5,1,5,8,9,22,15,11,16,22,18,41,23,18,21,14,15,37,21,15,18,9,6,14,7,4,1,0,3],"text":"","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(89,89,89,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":23.305936073059364,"r":7.3059360730593621,"b":37.260273972602747,"l":37.260273972602747},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[41.034482758620683,79.724137931034477],"tickmode":"array","ticktext":["50","60","70"],"tickvals":[50,60,70],"categoryorder":"array","categoryarray":["50","60","70"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.6529680365296811,"tickwidth":0.66417600664176002,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y","title":{"text":"weight","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-2.0500000000000003,43.049999999999997],"tickmode":"array","ticktext":["0","10","20","30","40"],"tickvals":[0,9.9999999999999982,20,29.999999999999996,40],"categoryorder":"array","categoryarray":["0","10","20","30","40"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.6529680365296811,"tickwidth":0.66417600664176002,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x","title":{"text":"count","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.8897637795275593,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498}},"hovermode":"closest","height":504,"barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"35495b63642d":{"x":{},"type":"bar"}},"cur_data":"35495b63642d","visdat":{"35495b63642d":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>




### HT_AVG {.tabset .tabset-pills}       

<div id="htmlwidget-df386e9bd1f9d6b676a0" style="width:100%;height:504px;" class="plotly html-widget "></div>
<script type="application/json" data-for="htmlwidget-df386e9bd1f9d6b676a0">{"x":{"data":[{"orientation":"v","width":[1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256,1.2413793103448256],"base":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],"x":[43.448275862068968,44.689655172413794,45.931034482758626,47.172413793103452,48.413793103448278,49.65517241379311,50.896551724137936,52.137931034482762,53.379310344827587,54.620689655172413,55.862068965517246,57.103448275862078,58.344827586206904,59.58620689655173,60.827586206896555,62.068965517241381,63.310344827586206,64.551724137931046,65.793103448275872,67.034482758620697,68.275862068965523,69.517241379310349,70.758620689655174,72,73.241379310344826,74.482758620689651,75.724137931034477,76.965517241379317,78.206896551724142,79.448275862068968],"y":[1,2,3,3,14,6,12,15,35,21,15,19,25,38,21,20,20,30,14,18,12,21,9,10,6,5,1,3,0,1],"text":"","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(89,89,89,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.654490106544898,"r":7.3059360730593621,"b":40.608828006088288,"l":37.260273972602747},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[40.965517241379317,81.931034482758619],"tickmode":"array","ticktext":["50","60","70","80"],"tickvals":[50,60,70,80],"categoryorder":"array","categoryarray":["50","60","70","80"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.6529680365296811,"tickwidth":0.66417600664176002,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y","title":{"text":"weight","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-1.9000000000000001,39.899999999999999],"tickmode":"array","ticktext":["0","10","20","30"],"tickvals":[0,10,20,30],"categoryorder":"array","categoryarray":["0","10","20","30"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.6529680365296811,"tickwidth":0.66417600664176002,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x","title":{"text":"count","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.8897637795275593,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498}},"hovermode":"closest","height":504,"barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"35497fc6d4c8":{"x":{},"type":"bar"}},"cur_data":"35497fc6d4c8","visdat":{"35497fc6d4c8":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>


![plot of chunk myBoxplot](figure/myBoxplot-1.png)





