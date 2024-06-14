goodLevels <- function(object, analysisId){
  '%!in%' <- function(x,y)!('%in%'(x,y))
  metaPheno <- object$metadata$pheno
  predictions <- object$predictions[which(object$predictions$analysisId == analysisId ),]
  # add missing columns
  keep <- which(metaPheno$parameter %!in% c("trait","designation","environment","rep","row","col","iBlock") )
  if(length(keep) > 0){
    toExtractFromData <- metaPheno[keep, "value"]
    tpe <- unique(object$data$pheno[,c("environment",toExtractFromData)])
    colnames(tpe) <- cgiarBase::replaceValues(colnames(tpe), Search = metaPheno$value, Replace = metaPheno$parameter )
    predictions <- merge(predictions, tpe, by="environment", all.x = TRUE)
  }
  # add missing weather
  weather <- object$metadata$weather
  if(!is.null(weather)){
    weather$traitParameter <- paste(weather$trait, weather$parameter, sep="_")
    wide <- reshape(weather[,-which(colnames(weather)%in%c("trait","parameter"))], direction = "wide", idvar = "environment",
                    timevar = c("traitParameter"), v.names = "value", sep= "_")
    colnames(wide) <- gsub("value_","",colnames(wide))
    predictions <- merge(predictions, wide, by="environment", all.x = TRUE)
  }
  # extract
  availableTraits <- metaPheno[which(metaPheno$parameter %in% c("trait")),"value"]
  factorPerTrait <- vector(mode="list", length = length(availableTraits)); names(factorPerTrait) <- availableTraits
  availableFactors <- metaPheno[which(metaPheno$parameter %in% c("environment","year","season","location","trial","study","management")),"parameter"]

  for(iTrait in availableTraits){ # iTrait = availableTraits[1]
    provPred <- predictions[which(predictions$trait == iTrait),]
    provPred <- provPred[which(!is.na(provPred$predictedValue)),]
    for(iFactor in availableFactors){ # iFactor = availableFactors[1]
      if(length(na.omit(unique(provPred[,iFactor]))) > 1){factorPerTrait[[iTrait]] <- c(factorPerTrait[[iTrait]] , iFactor)}
    }
  }
  return(factorPerTrait)
}

dependencyPlot <- function(){
  mm <- matrix(
    NA, nrow = 5, ncol = 8
  )
  # colnames(mm) <- c("QA","STA","MTA","INDEX","OCS","RGG","PGG")
  colnames(mm) <- c("Quality Assurance","Single Trial Analysis","Multi Trial Analysis","Selection Index","Optimal Cross Selection","Realized Genetic Gain","Predicted Genetic Gain", "Population Structure")
  rownames(mm) <- c("QTL", "Weather","Pedigree", "Genotype", "Phenotype")
  mm[5,] = c(2,2,2,2,2,2,2,0)
  mm[4,] = c(1,0,1,0,2,0,0,2)
  mm[3,] = c(1,0,1,0,2,2,2,0)
  mm[2,] = c(0,0,1,0,0,0,0,0)
  mm[1,] = c(0,0,1,0,0,0,0,0)
  mydata4 <- cgiarBase::matToTab(mm, symmetric = FALSE)
  mydata4$label <- ifelse( mydata4$Freq == 0, "-", ifelse(mydata4$Freq == 2, "Required", "Optional" ) )

  p <- ggplot2::ggplot(data = mydata4, ggplot2::aes(Var2, Var1, fill = Freq))+
    ggplot2::geom_tile(color = "black")+
    ggplot2::scale_fill_gradient2(high = "#038542", mid = "aquamarine3", low = "white",
                                  midpoint = 1, limit = c(0,2), space = "Lab",
                                  name="Pearson\nCorrelation") +
    ggplot2::theme_minimal()+
    ggplot2::ylab("") + ggplot2::xlab("") +
    ggplot2::coord_fixed() +
    ggplot2::theme(strip.text.x = ggplot2::element_text(size=10, color="black"),
                   axis.text.x = ggplot2::element_text(angle = 45, size=12, vjust=0.5,
                                                       hjust=0.1, # this one control the top labels position
                                                       lineheight=0.75 ),
                   axis.text.y = ggplot2::element_text(size=12),
                   legend.position = "none",
                   plot.title = ggplot2::element_text(color="grey32", size=18, face="bold.italic") )  +
    ggplot2::geom_text(ggplot2::aes(label = label ), color = "grey18", size = 3) +
    ggplot2::ggtitle("Data dependencies across modules") +
    ggplot2::scale_x_discrete(
      position = "bottom",
      guide = ggplot2::guide_axis(position = "top")
    )
  return(p)
}

plotDensitySelected <-  function(object,environmentPredictionsRadar2, traitFilterPredictionsRadar2,
                                 meanGroupPredictionsRadar, proportion=0.2,
                                 analysisId, trait, desirev, scaled, title=NULL){

  mydata <- object$predictions
  mydata <- mydata[which(mydata$analysisId %in% analysisId),]

  mydata = mydata[which( (mydata$environment %in% environmentPredictionsRadar2) &
                           (mydata$trait %in% traitFilterPredictionsRadar2)
  ),]
  mydata$selected <- "no"
  ## pop means
  mm <- stats::aggregate(predictedValue~trait, data=mydata, FUN=mean, na.rm=TRUE)
  ## if user provides threshold values let them know the selection differentials
  if(length(as.numeric(unlist(strsplit(meanGroupPredictionsRadar,",")))) == nrow(mm)){
    ## add product profile means'
    mm$threshold <- as.numeric(unlist(strsplit(meanGroupPredictionsRadar,","))) + mm[,2]
    mm$desire <- mm$threshold - mm$predictedValue
    # index calculation
    resInd <- cgiarPipeline::indexDesire(
      phenoDTfile= object, # input data structure
      analysisId=analysisId, # analysis to be picked from predictions database
      environmentToUse = environmentPredictionsRadar2,
      trait= trait, # traits to include in the index
      desirev = as.numeric(unlist(strsplit(desirev,","))), # vector of desired values
      scaled=scaled, # whether predicted values should be scaled or not
      verbose=FALSE # sh
    )
    myIndex <- resInd$predictions
    mtas <- resInd$status[which(resInd$status$module == "indexD"),"analysisId"]; mtaId <- mtas[length(mtas)]
    myIndex <- myIndex[which(myIndex$analysisId == mtaId),]
    myIndex <- myIndex[with(myIndex, order(-predictedValue)), ]
    selected <- myIndex[1:round(nrow(myIndex)*proportion),"designation"]
    mydata$selected[which(mydata$designation %in% selected)] <- "yes"
    # update mm
    mm2 <- stats::aggregate(predictedValue~trait, data=mydata[which(mydata$selected == "yes"),], FUN=mean, na.rm=TRUE)
  }

  if(length(as.numeric(unlist(strsplit(meanGroupPredictionsRadar,",")))) == nrow(mm)){
    p <- ggplot2::ggplot(mydata, ggplot2::aes(x=predictedValue, color=selected, fill = selected)) +
      ggplot2::geom_histogram() +
      ggplot2::ylab("Frequency") +
      ggplot2::geom_rug(sides="t", length = ggplot2::unit(0.3, "cm")) +
      ggplot2::facet_wrap(~trait, ncol=3, scales = "free") +
      ggplot2::geom_vline(data = mm, ggplot2::aes(xintercept = predictedValue), linetype="dotdash", col="red") +
      ggplot2::geom_vline(data = mm2, ggplot2::aes(xintercept = predictedValue), linetype="dotdash", col="cadetblue")
  }else{
    p <- ggplot2::ggplot(mydata, ggplot2::aes(x=predictedValue, color=selected, fill = selected)) +
      ggplot2::geom_histogram() +
      ggplot2::ylab("Frequency") +
      ggplot2::geom_rug(sides="t", length = ggplot2::unit(0.3, "cm")) +
      ggplot2::facet_wrap(~trait, ncol=3, scales = "free") +
      ggplot2::geom_vline(data = mm, ggplot2::aes(xintercept = predictedValue), linetype="dotdash", col="red")
  }
  if(!is.null(title)){
    p <- p + ggplot2::ggtitle(title)
  }
  # fig2 <-  plotly::ggplotly(p)
  # fig2
  p

}

radarPlot <-  function(mydata, environmentPredictionsRadar2=NULL,traitFilterPredictionsRadar2=NULL,
                       proportion=NULL,meanGroupPredictionsRadar=NULL,
                       fontSizeRadar=12, r0Radar=NULL, neRadar=NULL, plotSdRadar=FALSE, title=NULL){


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
    if(!is.null(title)){
      fig <- fig %>% plotly::layout(title=title)
    }
    return(fig)
  }

}

corPlotPredictions <- function(predictions, traitPredictionsCorrelation=NULL, unitOfCorrelation=NULL,
                               correlatedAcross=NULL, valueForCorrelation=NULL,
                               checkboxCluster=FALSE, checkboxText=FALSE, checkboxAxis=FALSE){

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

    if(checkboxCluster){
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
                            color = mydata4[,"Freq"], #text=mydata4[,"mytext"],
                            colors = c('#BF382A', '#0C4B8E'))
    fig <- fig %>%  plotly::add_heatmap()
    ## add text inside the corplot
    if(checkboxText){
      fig <- fig %>%  plotly::add_annotations(text =mydata4[,"Freq"],
                                              x = mydata4[,"X1"],
                                              y = mydata4[,"X2"],
                                              xref = 'x', yref = 'y',
                                              showarrow = FALSE, font=list(color='white')) #
    }
    ## add axis labels
    if(checkboxAxis){
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
