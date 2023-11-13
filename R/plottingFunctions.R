radarPlot <-  function(mydata, environmentPredictionsRadar2=NULL,traitFilterPredictionsRadar2=NULL,proportion=NULL,meanGroupPredictionsRadar=NULL,
                       fontSizeRadar=12, r0Radar=NULL, neRadar=NULL, plotSdRadar=FALSE){


  mydata = mydata[which( (mydata$environment %in% environmentPredictionsRadar2) &
                           (mydata$trait %in% traitFilterPredictionsRadar2)
  ),]
  if(!is.null(environmentPredictionsRadar2) & !is.null(traitFilterPredictionsRadar2)){
    mm <- stats::aggregate(predictedValue~trait, data=mydata, FUN=mean, na.rm=TRUE)
    mmsd <- stats::aggregate(predictedValue~trait, data=mydata, FUN=stats::sd, na.rm=TRUE)
    mm2 <- stats::aggregate(predictedValue~trait, data=mydata, FUN=min, na.rm=TRUE)
    mm3 <- stats::aggregate(predictedValue~trait, data=mydata, FUN=max, na.rm=TRUE)
    namesA <- mm[,1]
    meanA <- mm[,2]/(mm3[,2])

    # meanA <- rep(0.75,nrow(mm))
    fig <-  plotly::plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    )
    fig <- fig %>%
      plotly::add_trace(
        r = meanA, #c(39, 28, 8, 7, 28, 39),
        theta =paste(1:length(namesA),namesA,sep="."),# c('A','B','C', 'D', 'E', 'A'),
        name = 'Breeding population means',
        text=round(mm[,2],3)
      )
    ## add product profile means
    if(!is.null(meanGroupPredictionsRadar)){
      meanB <- as.numeric(unlist(strsplit(meanGroupPredictionsRadar,",")))
      if(length(meanB) == length(mm[,2])){
        meanB <- meanB + mm[,2]
        # r2 = (meanB*0.75)/mm[,2]
        r2 = meanB/(mm3[,2])
        dToSearch <- meanB #- mm[,2]
        fig <- fig %>%
          plotly::add_trace(
            r = r2, # c(1.5, 10, 39, 31, 15, 1.5),
            theta = paste(1:length(namesA),namesA,sep="."),# c('A','B','C', 'D', 'E', 'A'),
            name = "Desired population", #'Group B'
            # mode="text",
            text=round(dToSearch,2)
            # textfont = list(color = '#000000', size = fontSizeRadar)
          )
      }
    }
    # add SD polygon
    if(plotSdRadar){
      meanC <- (mm[,2]+mmsd[,2])/(mm3[,2])
      fig <- fig %>%
        plotly::add_trace(
          r = meanC, # c(1.5, 10, 39, 31, 15, 1.5),
          theta = paste(1:length(namesA),namesA,sep="."),# c('A','B','C', 'D', 'E', 'A'),
          name = "Mean + Standard Deviation from pop", #'Group B'
          text=round(mm[,2]+mmsd[,2],3)
        )
      meanD <- (mm[,2]-mmsd[,2])/(mm3[,2])
      fig <- fig %>%
        plotly::add_trace(
          r = meanD, # c(1.5, 10, 39, 31, 15, 1.5),
          theta = paste(1:length(namesA),namesA,sep="."),# c('A','B','C', 'D', 'E', 'A'),
          name = "Mean - Standard Deviation from pop", #'Group B'
          text=round(mm[,2]-mmsd[,2],3)
        )
    }
    # add selection limits polygon
    ## add product profile means'
    if(!is.null(r0Radar)){
      meanR <- as.numeric(unlist(strsplit(r0Radar,",")))/100
      if(length(meanR) == length(mm[,2])){
        meanR <- mm[,2] + ((meanR*mm[,2])*neRadar*2) # selection limits 2*Ne*R0
        r3 = (meanR*0.75)/mm[,2]
        dToSearchR <- meanR #- mm[,2]
        fig <- fig %>%
          plotly::add_trace(
            r = r3, # c(1.5, 10, 39, 31, 15, 1.5),
            theta = paste(1:length(namesA),namesA,sep="."),# c('A','B','C', 'D', 'E', 'A'),
            name = "Selection limits", #'Group B'
            # mode="text",
            text=round(dToSearchR,2)
            # textfont = list(color = '#000000', size = fontSizeRadar)
          )
      }
    }
    return(fig)
  }

}

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

histPlotPredictions <- function(environmentPredictionsHistTraits=NULL, yaxisPredictionsHistTraits=NULL, valuePredictionsHistTraits=NULL, checkboxScalePredictions=NULL){

  # predictions <- dtHistPredictionstraits()$predictions
  predictions <- predictions[which(predictions$environment %in% environmentPredictionsHistTraits ),]
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
