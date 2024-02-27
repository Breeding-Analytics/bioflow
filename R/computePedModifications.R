computePedModifications <- function(
    M = NULL, # markers are assumed to come centered
    Tb1=NULL,
    Tb2=NULL,
    Limp1=14000,
    Limp2=17000,
    QCc1=1.2,
    QCc2=2,
    GmatFL1=0.2,
    GmatFL2=1.6,
    GmatFL3=0.7,
    GmatML1=0.2,
    GmatML2=0.7,
    GmatD_FL1=0.2,
    HetL1=5,
    HetF1=5,
    HetM1=5,
    Fimp1=16000,
    Mimp1=16000,
    QcTest=1000,
    QcTestG1=1400,
    QcTestG2=1200
){

M <- apply(M,2,sommer::imputev)
Gmat <- sommer::A.mat(M)
load("InputInfo.RData")
Tb2=as.data.frame(data.table::fread(Tb2, sep = '\t', header = T))[,-c(1:11)]

if(length(unlist(strsplit(Tb2[1,1],split="")))==2){
  valid_IUPAC <- c('A', 'C', 'G', 'T', 'U', 'W', 'S', 'M', 'K', 'R', 'Y', 'B', 'D', 'H', 'V', 'N')
  double_code <- c("AA","TT","CC","GG","AT","TA","AC","CA","AG","GA","TC","CT","TG","GT","CG","GC","NN")
  for (i in 1:length(valid_IUPAC)){
    Tb2[Tb2 == double_code[i]] <- valid_IUPAC[i]
  }
}
Ncomp=dim(Tb1)[2]
#save(Gmat,Tb1,Tb2,Tb3,Tb4,finaltable,file="check.RData")
###Usign APPLY
tryF=function(x,Limp1,Limp2,QCc1,QCc2,GmatFL1,GmatFL2,GmatFL3,GmatML1,GmatML2,GmatD_FL1,HetL1,HetF1,HetM1,Fimp1,Mimp1,QcTest,QcTestG1,QcTestG2){
  tst=x
  pos=list()
  posG=list()
  for (k in 1:length(tst)) {
    pos[[k]]=which(names(Tb2)%in%tst[k])
    posG[[k]]=which(colnames(Gmat)%in%tst[k])
  }
  pos=unlist(pos)
  posG=unlist(posG)
  impL=length(which(Tb2[,pos[1]]=="N"))
  impF=length(which(Tb2[,pos[2]]=="N"))
  impM=length(which(Tb2[,pos[3]]=="N"))
  hetsL=(length(which(Tb2[,pos[1]]%in%c("A","C","G","T","N")==FALSE))*100)/dim(Tb2)[1]
  hetsF=(length(which(Tb2[,pos[2]]%in%c("A","C","G","T","N")==FALSE))*100)/dim(Tb2)[1]
  hetsM=(length(which(Tb2[,pos[3]]%in%c("A","C","G","T","N")==FALSE))*100)/dim(Tb2)[1]

  triplebyMk=data.frame(Combi=apply(Tb2[,pos],1,paste,collapse = ""))
  if(Ncomp==3){
    wrtst=merge(triplebyMk,Tb3,by="Combi")[,c(2,3)]
  }else{
    wrtst=merge(triplebyMk,Tb4,by="Combi")[,c(2,3)]
  }
  tmp=data.frame(apply(wrtst,2,sum,na.rm=T))
  percent=tmp[1,]/tmp[2,]
  #impL=impLFM[pos[1]]
  #impF=impLFM[pos[2]]
  #impM=impLFM[pos[3]]
  #hetsL=hetsLFM[pos[1]]
  #hetsF=hetsLFM[pos[2]]
  #hetsM=hetsLFM[pos[3]]
  FL=Gmat[posG[1],posG[2]]
  ML=Gmat[posG[1],posG[3]]
  D_FL=diag(Gmat)[posG[1]]-FL
  Limp=ifelse(impL<Limp1,"XXX", ifelse(impL>Limp2,"SAMPLE_FAIL","SAMPLE_POOR"))
  QCc=ifelse(percent<QCc1,"Allele_QC_PASS", ifelse(percent>QCc2,"Allele_QC_FAIL","Allele_QC_BORDERLINE"))
  GmatFL=ifelse(Gmat[2]<GmatFL1,"FEMALE_GMAT_WRONG", ifelse(Gmat[2]>GmatFL3,"FEMALE_HIGH_GMAT",ifelse(Gmat[2]>=GmatFL1 & Gmat[2]<=GmatFL2,"FEMALE_BORDERLINE_GMAT","XXX")))
  GmatML=ifelse(Gmat[3]<GmatML1,"MALE_GMAT_WRONG", ifelse(Gmat[3]>=GmatML1 & Gmat[3]<=GmatML2,"MALE_BORDERLINE_GMAT","XXX"))
  GmatD_FL=ifelse(D_FL<GmatD_FL1,"PROBABLE_SELF","XXX")
  HetL=ifelse(hetsL>HetL1,"LINE_XS_HETS","XXX")
  HetF=ifelse(hetsF>HetF1,"FEMALE_XS_HETS","XXX")
  HetM=ifelse(hetsM>HetM1,"MALE_XS_HETS","XXX")
  Fimp=ifelse(impF>Fimp1,"FEMALE_BAD_SAMPLE","XXX")
  Mimp=ifelse(impM>Mimp1,"MALE_BAD_SAMPLE","XXX")

  if(Limp=="SAMPLE_FAIL"){
    condtest="SAMPLE_FAIL"
  }else{
    condtest=paste0(Limp,"_",QCc,"_",GmatFL,"_",GmatML,"_",HetL,"_",HetF,"_",HetM,"_",GmatD_FL,"_",Fimp,"_",Mimp)
  }

  finalclass=finaltable[which(finaltable$Condicional==condtest),c(2,3)]
  if(length(finalclass)==0){finalclass=c("NoFind",2)}

  if(tmp[2,]<QcTest){
    if(impL>QcTestG1){finalclass=c("NO ASSESSMENT-LINE SAMPLE TOO POOR",2)}#group 1
    if(impL<=QcTestG1 & impF>QcTestG2 & impM>QcTestG2){finalclass=c("NO ASSESSMENT-FEMALE AND MALE SAMPLES TOO POOR",2)}else{#group 2
      if(impF>impM){
        if(GmatML=="XXX"){GmatMLe="MALE GMAT PASS"}else{GmatMLe=GmatML}
        finalclass=c(paste0("NO QC TEST. NO FEMALE ASSESSMENT - FEMALE SAMPLE TOO POOR. ",GmatMLe),2)
      }#group 3
      if(impM>impF){
        if(GmatFL=="XXX"){GmatFLe="FEMALE GMAT PASS"}else{GmatFLe=GmatFL}
        finalclass=c(paste0("NO QC TEST. NO MALE ASSESSMENT - MALE SAMPLE TOO POOR. ",GmatFLe),2)
      }#group 4
    }
  }
  tmp=rbind(tmp,percent,QCc,impL,Limp,Gmat[2],GmatFL,Gmat[3],GmatML,hetsL,HetL,hetsF,HetF,hetsM,HetM,D_FL,GmatD_FL,impF,Fimp,impM,Mimp,finalclass[,1],finalclass[,2])
  return(tmp)
}

#40 min more less
library(pbapply)
pboptions(type="timer",style=4)
final1=data.frame(Tb1,t(data.frame(pbapply(Tb1,1,function(x) tryF(x,Limp1,Limp2,QCc1,QCc2,GmatFL1,GmatFL2,GmatFL3,GmatML1,GmatML2,GmatD_FL1,HetL1,HetF1,HetM1,Fimp1,Mimp1,QcTest,QcTestG1,QcTestG2)))))
names(final1)[6:27]=c("QCPer_wrong","QCPer_wrong_class","LineImputed","Line_imputed_class","G_matrix_Line_Female","G_matrix_Line_Female_Class","G_matrix_Line_Male","G_matrix_Line_Male_Class","Hets_Line","Hets_Line_class","Hets_Female",
                      "Hets_Female_class","Hets_Male","Hets_Male_class","Diagonal_minus_Female_G_matrix","Diagonal_Female_G_matrix_class","Imputed_Female","Imputed_Female_class","Imputed_Male","Imputed_Male_class","Final_Classification","KEEP_0")
rownames(final1)=NULL
#gc()
#write.csv(final1,"QC2Parents.csv",row.names=F)

return(final1)
###########################################################################
}
