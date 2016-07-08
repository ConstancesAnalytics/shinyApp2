#test
library('shiny')
library('ggplot2')  # for the diamonds dataset
library('DT')
library('dplyr')
library('reshape2')
#setwd("U:/R/App9")
# load table "para"
load('./paracl_aff_rom.RData')

dic_nom_para=read.csv('dic_nom_para.csv',header = TRUE, sep=';')
dic_nom_para$nom =as.character(dic_nom_para$nom)
dic_nom_para$variable =as.character(dic_nom_para$variable)
names(para)
source('./function.R')

para_cols <- gsub("PARACL_","", colnames(para))
colnames(para) <- para_cols


#selections des variavles dans le catalogues des donnees.
para <- select(para, VDL_BiAvCorr,VDL_BiSaCorr,VDL_CeProAmbMono,VDL_ExaReal,VDL_OeDrAvCorr,VDL_OeDrSaCorr,VDL_OeGaAvCorr,VDL_OeGaSaCorr,VDP_BiAvCorr,VDP_BiSaCorr,VDP_CeProAmbMono,VDP_ExaReal,VDP_OeDrAvCorr,VDP_OeDrSaCorr,VDP_OeGaAvCorr,VDP_OeGaSaCorr,AUD_AuDr1000,AUD_AuDr2000,AUD_AuDr4000,AUD_AuDr500,AUD_AuDr8000,AUD_AuGa1000,AUD_AuGa2000,AUD_AuGa4000,AUD_AuGa500,AUD_AuGa8000,AUD_ExaReal,BIO_Alat,BIO_ChoHDL,BIO_ChoTot,BIO_Crea,BIO_Gam,BIO_Glyc,BIO_Trig,HEM_AspSer,HAN_ExaReal,HAN_MesToHan,HAU_ExaReal,HAU_MesTail,OMB_ExaReal,OMB_MesPeri,POI_ExaReal,POI_MesPoi,TAI_ExaReal,TAI_MesToTai,DEN_ConcDen1,DEN_DerExaDen,DEN_GinGiv,DEN_LesMuq,DEN_NbDenDefCar,DEN_NbDenDefExt,DEN_NbDenDefObt,DEN_NbDenDefSai,DEN_NbProAdj,DEN_NbProCon,DEN_Orth,DEN_PlaBac,DEN_PreTar,DEN_SilAnf16,DEN_SilAnf26,DEN_SilAnf36,DEN_SilAnf46,DEN_TrbATM,ECG_ExaReal,HEM_BasoPhi,HEM_EosiPhi,HEM_GloBla,HEM_GloRou,HEM_Hemato,HEM_Hemo,HEM_Lympho,HEM_Monocy,HEM_NeuPhi,HEM_Plaq,HEM_VolGlobMoy,SAN_DiffPrel,SAN_ExaReal,SAN_HeFinDerRepa,SAN_HeuPrel,BAU_Gluc,BAU_Leuco,BAU_Nitr,BAU_Prot,BAU_Sang,BIR_Creat,BIR_Gluc,BIR_MicAlb,BIR_Prot,URI_ExaReal,URI_HeuRec,SAN_Regle,SOC_AgJrBil,SOC_AidMedEta,SOC_BenCMU,SOC_BenRMIRSA,SOC_CES_Antenne,SOC_CES_NCes,SOC_Cin12Mois,SOC_CMUBase,SOC_CMUComp,SOC_Con6Mois,SOC_CouvCompConst,SOC_CouvCompDecl,SOC_JeuVoIns,SOC_NConstances,SOC_PbAchatNourr,SOC_PrChar100,SOC_Precar,SOC_Proprio,SOC_QHerbDiff,SOC_QMatDiff,SOC_RenTravSocial,SOC_ScPrec,SOC_Sex,SOC_Spo12Mois,SOC_Vac12Mois,SPI_CriAcc,SPI_CriRepr,SPI_CVF1,SPI_CVF2,SPI_CVF3,SPI_ExaReal,SPI_VEMS1,SPI_VEMS2,SPI_VEMS3,bras,resp_proc,TAR_ExaReal,TAR_ExaUni,TAR_TenArtDiaBraRefDr,TAR_TenArtDiaBraRefGa,TAR_TenArtDiaDr,TAR_TenArtDiaGa,TAR_TenArtDiaOrt,TAR_TenArtOrt,TAR_TenArtSysBraRefDr,TAR_TenArtSysBraRefGa,TAR_TenArtSysDr,TAR_TenArtSysGa,TAR_TenArtSysOrt,TRA_TraHomeo,SOC_Cah_Paracl,SOC_CES_Appli,SOC_HomeTime,SOC_DNaissance,SOC_DatExam)





para_cols<-colnames(para)

#selection des noms des variables

#rename(para, VDL.binoculaire.avec.correction = VDL_BiAvCorr)




para$resp_proc= as.factor(para$resp_proc)
para$bras =as.factor(para$bras)



para$AUD_AuDr500 =as.factor(as.numeric(as.character(para$AUD_AuDr500)))
para$AUD_AuDr1000=as.factor(as.numeric(as.character(para$AUD_AuDr1000)))
para$AUD_AuDr2000=as.factor(as.numeric(as.character(para$AUD_AuDr2000)))
para$AUD_AuDr4000=as.factor(as.numeric(as.character(para$AUD_AuDr4000)))
para$AUD_AuDr8000=as.factor(as.numeric(as.character(para$AUD_AuDr8000)))
para$AUD_AuGa500 =as.factor(as.numeric(as.character(para$AUD_AuGa500)))
para$AUD_AuGa1000=as.factor(as.numeric(as.character(para$AUD_AuGa1000)))
para$AUD_AuGa2000=as.factor(as.numeric(as.character(para$AUD_AuGa2000)))
para$AUD_AuGa4000=as.factor(as.numeric(as.character(para$AUD_AuGa4000)))
para$AUD_AuGa8000=as.factor(as.numeric(as.character(para$AUD_AuGa8000)))
#S?paration entre les variables Numerique et facteur

para_dic=para[names(dict_para)]


para_1 <- para_dic[ , sapply(dict_para, function(x) length(x)==2)]
para_2 <- para_dic[,!sapply(dict_para, function(x) length(x)==2) ]
para_1_corr <- as.data.frame(apply(para_1, 2, function(x) as.numeric( x)))
para_2_corr <- as.data.frame(apply(para_2, 2, function(x) as.factor( x)))

para_else= para[setdiff(para_cols,names(dict_para))]
para <- cbind(para_1_corr, para_2_corr,para_else)

#Ajouter des variables
#IMC
para$HAU_MesTail2=as.numeric(para$HAU_MesTail)*0.01
para$IMC=(para$POI_MesPoi)/(para$HAU_MesTail2^2)
para$class_IMC=as.factor(cut(para$IMC, breaks = c(12,18.5,24.9,29.9,70), right = FALSE, include.lowest = TRUE))
#class_BIO_ChoHDL
para$class_BIO_ChoHDL=cut(para$BIO_ChoHDL, breaks = c(0.25,0.9, 3.5), right = FALSE,include.lowest = TRUE)
#class_BIO_Trig
para$class_BIO_Trig=cut(para$BIO_Trig, breaks = c(0.2,2.5,30), right = FALSE,include.lowest = TRUE)
#class_BIO_ChoTot
para$class_BIO_ChoTot=cut(para$BIO_ChoTot, breaks = c(1.5,2.2,6.0,15), right = FALSE,include.lowest = TRUE)
#LDL
para$BIO_ChoTot_LDL = borne(para$BIO_ChoTot, 1.5 ,15)
para$BIO_ChoHDL_LDL= borne(para$BIO_ChoHDL, 0.25 ,3.5)
para$BIO_Trig_LDL  =borne(para$BIO_Trig, 0.1 ,3.75)

para$ldl= para$BIO_ChoTot_LDL-para$BIO_ChoHDL_LDL-(para$BIO_Trig_LDL/2.2)

para$class_ldl=cut(para$ldl, breaks = c(-1,4.2,11), right = FALSE,include.lowest = TRUE)


para$par_ces <- para$SOC_CES_NCes



# manual modifications (a voir)
para$SOC_NConstances <- as.integer(para$SOC_NConstances)
para$SOC_Sex <- as.factor(para$SOC_Sex)
para$SOC_CES_NCes <- as.factor(para$SOC_CES_NCes)
para$SOC_DNaissance <- as.Date(para$SOC_DNaissance, "%Y-%m-%d")
para$SOC_DatExam <- as.Date(para$SOC_DatExam, "%Y-%m-%d")


#Calcul de l'age
para$age=as.numeric(((para$SOC_DatExam-para$SOC_DNaissance )/365.5))
levels(para$SOC_Sex)<-c('M','F')
para$clas_age3=cut(floor(para$age), breaks = c(18,30,60,100), right = FALSE)
levels(para$clas_age3) <- c('18-29 ans','30-59 ans', '60 ans et plus')


dcast(para, SOC_Sex + clas_age3 ~ class_IMC )


para$clas_age5=cut(floor(para$age), breaks = c(18,35,45,55,65,74), right = FALSE,include.lowest = TRUE)
levels(para$clas_age5) <- c('18-34 ans','35-44 ans','45-54 ans', '55-64 ans','65-74 ans')


para$clas_age45an <- cut(floor(para$age), breaks = c(18,45,100), right = FALSE)
levels(para$clas_age45an) <- c('18-45 ans','45 ans et plus')
# define para_num et others
para_num  <- para[ , sapply(para,  is.numeric)]
para_num$SOC_DatExam<- para$SOC_DatExam
#para_fac  <- para[ , sapply(para,  is.factor)]

para_num$CESantenne <- para$SOC_CES_Antenne

#all.list_num<-colnames(para_num)







