corPlotPredictions <- function(predictions, traitPredictionsCorrelation=NULL, unitOfCorrelation=NULL,
                               correlatedAcross=NULL, valueForCorrelation=NULL,
                               checkboxCluster=NULL, checkboxText=NULL, checkboxAxis=NULL){

  if(!is.null(nrow(predictions))){

    if(!is.null(traitPredictionsCorrelation)){
      predictions = predictions[which(predictions$trait %in% traitPredictionsCorrelation ),]
    }
    wide <- stats::reshape(predictions[,c(unitOfCorrelation,correlatedAcross, valueForCorrelation)],
                           direction = "wide", idvar = unitOfCorrelation,
                           timevar = correlatedAcross, v.names = valueForCorrelation, sep= "")
    colnames(wide) <- gsub(valueForCorrelation,"",colnames(wide))
    wide2 <- as.data.frame(wide[,-c(1)]); colnames(wide2) <- colnames(wide)[-c(1)]
    nagm <- round(stats::cor(wide2, use="pairwise.complete.obs"),2)
    mydata4 <- as.data.frame(as.table(nagm)); mydata4$mytext <- paste(mydata4$Var1, mydata4$Var2,sep=".")
    mydata4$X1 <- as.numeric(as.factor(mydata4$Var1))
    mydata4$X2 <- as.numeric(as.factor(mydata4$Var2))

    if(!is.null(checkboxCluster)){
      A <- matrix(NA, nrow=max(mydata4$X1), ncol = max(mydata4$X2))
      A[as.matrix(mydata4[,c("X1","X2")])] = mydata4[,"Freq"]
      A[lower.tri(A)] <- t(A)[lower.tri(A)] # fill the lower triangular
      rownames(A) <- colnames(A) <- levels(as.factor(mydata4$Var1))
      nas <- which(is.na(A), arr.ind = TRUE)
      if(nrow(nas) > 0){A[nas]=stats::median(A[lower.tri(A)], na.rm = TRUE)}
      hc <- stats::hclust(stats::dist(A), "ave")
      Ao <- A[hc$order,hc$order]

      Adf <- as.data.frame(as.table(Ao)) # converts a matrix in a data frame
      colnames(Adf) <- c("Var1","Var2","Freq")
      Adf$X1 <- as.numeric(as.factor(Adf$Var1))
      Adf$X2 <- as.numeric(as.factor(Adf$Var2))
      mydata4 <- Adf
      mydata4$mytext <- paste(mydata4$Var1,mydata4$Var2, sep=".")
    }

    fig <-  plotly::plot_ly(mydata4, x = mydata4[,"X1"], y = mydata4[,"X2"], z = mydata4[,"Freq"],
                            color = mydata4[,"Freq"], text=mydata4[,"mytext"], colors = c('#BF382A', '#0C4B8E'))
    fig <- fig %>%  plotly::add_heatmap()
    ## add text inside the corplot
    if(!is.null(checkboxText)){
      fig <- fig %>%  plotly::add_annotations(text =mydata4[,"Freq"],
                                              x = mydata4[,"X1"],
                                              y = mydata4[,"X2"],
                                              xref = 'x', yref = 'y',
                                              showarrow = FALSE, font=list(color='white')) #
    }
    ## add axis labels
    if(!is.null(checkboxAxis)){
      fig <- fig %>%
        plotly::layout(yaxis = list(dtick = 1, ticktext = unique(mydata4[,"Var1"]), tickmode="array", tickvals = unique(mydata4[,"X1"]) ),
                       xaxis = list(dtick = 1, ticktext = unique(mydata4[,"Var2"]), tickmode="array", tickvals = unique(mydata4[,"X2"]) )

        )
    }

    fig
  }

}

histPlotPredictions <- function(fieldinstPredictionsHistTraits=NULL, yaxisPredictionsHistTraits=NULL, valuePredictionsHistTraits=NULL, checkboxScalePredictions=NULL){

  # predictions <- dtHistPredictionstraits()$predictions
  predictions <- predictions[which(predictions$environment %in% fieldinstPredictionsHistTraits ),]
  if(length(yaxisPredictionsHistTraits) > 1){ # many traits, we need to scale and overlap
    fig <- plotly::plot_ly(alpha = 0.6)
    for(iTrait in 1:length(yaxisPredictionsHistTraits)){
      if(!is.null(checkboxScalePredictions)){
        fig <- fig %>% plotly::add_histogram(x = as.vector(scale(predictions[which(predictions$trait %in% yaxisPredictionsHistTraits[iTrait]),valuePredictionsHistTraits])), histnorm = "probability", name=yaxisPredictionsHistTraits[iTrait] )
      }else{
        fig <- fig %>% plotly::add_histogram(x = as.vector((predictions[which(predictions$trait %in% yaxisPredictionsHistTraits[iTrait]),valuePredictionsHistTraits])), histnorm = "probability", name=yaxisPredictionsHistTraits[iTrait] )
      }
    }
    fig <- fig %>% plotly::layout(barmode = "overlay",xaxis = list(title = "Scaled value"), yaxis = list(title = "Probability" ))
    fig
  }else{ # if only one trait
    if(!is.null(checkboxScalePredictions)){
      fig <- plotly::plot_ly(data = predictions, x = as.vector(scale(predictions[which(predictions$trait %in% yaxisPredictionsHistTraits),valuePredictionsHistTraits])), type = "histogram", histnorm = "probability" )
    }else{
      fig <- plotly::plot_ly(data = predictions, x = as.vector(predictions[which(predictions$trait %in% yaxisPredictionsHistTraits),valuePredictionsHistTraits]), type = "histogram", histnorm = "probability" )
    }
    fig = fig %>% plotly::layout(xaxis = list(title = "Value"), yaxis = list(title = "Probability" ))
  }
  return(fig)

}
