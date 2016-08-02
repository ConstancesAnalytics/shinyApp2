library('shiny')
library('ggplot2')  # for the diamonds dataset
library('DT')
library('dplyr')
library('reshape2')
source('./function.R')

# ----------------- load para file

load('./paracl_aff_rom.RData')
colnames(para) <- gsub("PARACL_","", colnames(para))

# ----------------- load dictionnary
dic_nom_para <- read.csv('dic_nom_para.csv',header = TRUE, sep=';', encoding = "UTF-8")
dic_nom_para$nom <- as.character(dic_nom_para$nom)
dic_nom_para$variable <- as.character(dic_nom_para$variable)



# ----------------- define bounds paraclinique

bounds_para <- list(POI_MesPoi =c( 40,200 ),
                POI_ExaReal =c( 1,2 ),
                HAU_MesTail =c( 40 ,200 ),
                HAU_ExaReal =c( 1 ,2 ),
                TAI_MesToTai=c(60,150),
                TAI_ExaReal=c(1,2),
                HAN_MesToHan=c(60,150),
                HAN_ExaReal=c(1,2),
                OMB_MesPeri=c(60,150),
                OMB_ExaReal=c(1,2),
                SPI_VEMS1 =c( 1,7 ),
                SPI_VEMS2 =c( 1,7 ),
                SPI_VEMS3 =c( 1,7 ),
                SPI_CVF1 =c( 1,7 ),
                SPI_CVF2 =c( 1,7 ),
                SPI_CVF3 =c( 1,7 ),
                TAR_TenArtSysDr =c( 80 ,250 ),
                TAR_TenArtSysGa =c( 80 ,250 ),
                TAR_TenArtDiaDr =c( 30 ,120 ),
                TAR_TenArtDiaGa =c( 30 ,120 ),
                TAR_TenArtDiaBraRefDr=c( 30 ,120 ),
                TAR_TenArtDiaBraRefGa=c( 30 ,120 ),
                TAR_TenArtSysBraRefGa=c(80,250),
                TAR_TenArtSysBraRefDr=c(80,250),
                TAR_TenArtSysOrt=c(1,2),
                TAR_TenArtSysOrt=c(80,250),
                TAR_TenArtDiaOrt=c( 30 ,120 ),
                VDL_OeDrSaCorr =c(0,12 ),
                VDL_OeDrAvCorr =c(0,12 ),
                VDL_OeGaAvCorr =c(0,12 ),
                VDL_OeGaSaCorr =c(0,12 ),
                VDL_BiAvCorr =c(0,12 ),
                VDL_BiSaCorr =c(0,12 ),
                VDL_CeProAmbMono =c(1,2),
                VDL_ExaReal =c(1,2 ),
                BIO_Glyc =c( 2.5 ,20 ),
                BIO_Crea =c(10 ,2000 ),
                BIO_Gam =c( 2 ,300 ),
                BIO_Alat =c( 2 ,200 ),
                BIO_ChoTot =c( 1.5,15  ),
                BIO_ChoHDL =c( 0.25 ,3.5 ),
                BIO_Trig =c( 0.1 ,30 ),
                HEM_GloBla =c( 1.0 ,50 ),
                HEM_GloRou =c( 2 ,7 ),
                HEM_VolGlobMoy =c( 60 ,120 ),
                HEM_Hemato=c(0.25,0.7),
                HEM_Hemo =c( 50 ,200 ),
                HEM_Plaq =c( 50 ,700 ),
                HEM_NeuPhi =c( 15 ,90 ),
                HEM_EosiPhi =c(0.5,20 ),
                HEM_BasoPhi =c(0.5,10 ),
                HEM_Lympho =c( 5,80 ),
                HEM_Monocy =c( 1,30 ),
                BIR_MicAlb =c( 5 ,500 ),
                BIR_Gluc =c( 0 ,30 ),
                BIR_Prot =c( 0.05 ,5 ),
                BIR_Creat =c( 2 ,30 ),
                VDP_OeDrSaCorr = c(1.5,2,3,4,5,6,7,8,10,14,20,28 ),
                VDP_OeDrAvCorr = c(1.5,2,3,4,5,6,7,8,10,14,20,28 ),
                VDP_OeGaAvCorr = c(1.5,2,3,4,5,6,7,8,10,14,20,28 ),
                VDP_OeGaSaCorr = c(1.5,2,3,4,5,6,7,8,10,14,20,28 ),
                VDP_BiAvCorr= c(1.5,2,3,4,5,6,7,8,10,14,20,28 ),
                VDP_BiSaCorr= c(1.5,2,3,4,5,6,7,8,10,14,20,28 ),
                VDP_ExaReal =c(1,2 ),
                VDP_CeProAmbMono=c(1,2 ),
                AUD_ExaReal=c(1,2 ),
                AUD_AuDr500 = c(-10 , -5 , 0 , 5 , 10 , 15, 20 , 25 , 30 , 35, 40 , 45, 50 , 55 , 60, 65 , 70, 75 , 80 , 85, 90 ),
                AUD_AuDr1000 = c(-10 , -5 , 0 , 5 , 10 , 15, 20 , 25 , 30 , 35, 40 , 45, 50 , 55 , 60, 65 , 70, 75 , 80 , 85, 90 ),
                AUD_AuDr2000 = c(-10 , -5 , 0 , 5 , 10 , 15, 20 , 25 , 30 , 35, 40 , 45, 50 , 55 , 60, 65 , 70, 75 , 80 , 85, 90 ),
                AUD_AuDr4000 =c(-10 , -5 , 0 , 5 , 10 , 15, 20 , 25 , 30 , 35, 40 , 45, 50 , 55 , 60, 65 , 70, 75 , 80 , 85, 90 ),
                AUD_AuDr8000 =c(-10 , -5 , 0 , 5 , 10 , 15, 20 , 25 , 30 , 35, 40 , 45, 50 , 55 , 60, 65 , 70, 75 , 80 , 85, 90 ),
                AUD_AuGa500 =c(-10 , -5 , 0 , 5 , 10 , 15, 20 , 25 , 30 , 35, 40 , 45, 50 , 55 , 60, 65 , 70, 75 , 80 , 85, 90 ),
                AUD_AuGa1000 = c(-10 , -5 , 0 , 5 , 10 , 15, 20 , 25 , 30 , 35, 40 , 45, 50 , 55 , 60, 65 , 70, 75 , 80 , 85, 90 ),
                AUD_AuGa2000 = c(-10 , -5 , 0 , 5 , 10 , 15, 20 , 25 , 30 , 35, 40 , 45, 50 , 55 , 60, 65 , 70, 75 , 80 , 85, 90 ),
                AUD_AuGa4000 = c(-10 , -5 , 0 , 5 , 10 , 15, 20 , 25 , 30 , 35, 40 , 45, 50 , 55 , 60, 65 , 70, 75 , 80 , 85, 90),
                AUD_AuGa8000 =c(-10 , -5 , 0 , 5 , 10 , 15, 20 , 25 , 30 , 35, 40 , 45, 50 , 55 , 60, 65 , 70, 75 , 80 , 85, 90 ),
                ECG_ExaReal=c(1,2 ),
                ldl=c(0,11),
                SAN_DiffPrel=c(1,2),
                SAN_ExaReal=c(1,2),
                URI_ExaReal=c(1,2),
                SAN_Regle=c(1,2),
                SOC_AidMedEta=c(1,2),
                SOC_BenCMU=c(1,2),
                SOC_BenRMIRSA=c(1,2),
                SOC_Cin12Mois=c(1,2),
                SOC_CMUBase=c(1,2),
                SOC_CMUComp=c(1,2),
                SOC_Con6Mois=c(1,2),
                SOC_CouvCompConst=c(1,2),
                SOC_CouvCompDecl=c(1,2),
                SOC_JeuVoIns=c(1,2),
                SOC_PbAchatNourr=c(1,2),
                SOC_PrChar100=c(1,2),
                SOC_Precar=c(1,2),
                SOC_Proprio=c(1,2),
                SOC_QHerbDiff=c(1,2),
                SOC_QMatDiff=c(1,2),
                SOC_RenTravSocial=c(1,2),
                SOC_ScPrec=c(1,2),
                SOC_Spo12Mois=c(1,2),
                SOC_Vac12Mois=c(1,2),
                SPI_ExaReal=c(1,2),
                SPI_CriRepr=c(1,2),
                SPI_CriAcc=c(1,2),
                TAR_ExaUni=c(1,2),
                TRA_TraHomeo=c(1,2))


# ----------------- select variables

para <- select(para, ### ELIE : select variables from dictionnary
               VDL_BiAvCorr,
               VDL_BiSaCorr,
               VDL_CeProAmbMono,
               VDL_ExaReal,
               VDL_OeDrAvCorr,
               VDL_OeDrSaCorr,
               VDL_OeGaAvCorr,
               VDL_OeGaSaCorr,
               VDP_BiAvCorr,
               VDP_BiSaCorr,
               VDP_CeProAmbMono,
               VDP_ExaReal,
               VDP_OeDrAvCorr,
               VDP_OeDrSaCorr,
               VDP_OeGaAvCorr,
               VDP_OeGaSaCorr,
               AUD_AuDr1000,
               AUD_AuDr2000,
               AUD_AuDr4000,
               AUD_AuDr500,
               AUD_AuDr8000,
               AUD_AuGa1000,
               AUD_AuGa2000,
               AUD_AuGa4000,
               AUD_AuGa500,
               AUD_AuGa8000,
               AUD_ExaReal,
               BIO_Alat,
               BIO_ChoHDL,
               BIO_ChoTot,
               BIO_Crea,
               BIO_Gam,
               BIO_Glyc,
               BIO_Trig,
               HEM_AspSer,
               HAN_ExaReal,
               HAN_MesToHan,
               HAU_ExaReal,
               HAU_MesTail,
               OMB_ExaReal,
               OMB_MesPeri,
               POI_ExaReal,
               POI_MesPoi,
               TAI_ExaReal,
               TAI_MesToTai,
               DEN_ConcDen1,
               DEN_DerExaDen,
               DEN_GinGiv,
               DEN_LesMuq,
               DEN_NbDenDefCar,
               DEN_NbDenDefExt,
               DEN_NbDenDefObt,
               DEN_NbDenDefSai,
               DEN_NbProAdj,
               DEN_NbProCon,
               DEN_Orth,
               DEN_PlaBac,
               DEN_PreTar,
               DEN_SilAnf16,
               DEN_SilAnf26,
               DEN_SilAnf36,
               DEN_SilAnf46,
               DEN_TrbATM,
               ECG_ExaReal,
               HEM_BasoPhi,
               HEM_EosiPhi,
               HEM_GloBla,
               HEM_GloRou,
               HEM_Hemato,
               HEM_Hemo,
               HEM_Lympho,
               HEM_Monocy,
               HEM_NeuPhi,
               HEM_Plaq,
               HEM_VolGlobMoy,
               SAN_DiffPrel,
               SAN_ExaReal,
               SAN_HeFinDerRepa,
               SAN_HeuPrel,
               BAU_Gluc,
               BAU_Leuco,
               BAU_Nitr,
               BAU_Prot,
               BAU_Sang,
               BIR_Creat,
               BIR_Gluc,
               BIR_MicAlb,
               BIR_Prot,
               URI_ExaReal,
               URI_HeuRec,
               SAN_Regle,
               SOC_AgJrBil,
               SOC_AidMedEta,
               SOC_BenCMU,
               SOC_BenRMIRSA,
               SOC_CES_Antenne,
               SOC_CES_NCes,
               SOC_Cin12Mois,
               SOC_CMUBase,
               SOC_CMUComp,
               SOC_Con6Mois,
               SOC_CouvCompConst,
               SOC_CouvCompDecl,
               SOC_JeuVoIns,
               SOC_NConstances,
               SOC_PbAchatNourr,
               SOC_PrChar100,
               SOC_Precar,
               SOC_Proprio,
               SOC_QHerbDiff,
               SOC_QMatDiff,
               SOC_RenTravSocial,
               SOC_ScPrec,
               SOC_Sex,
               SOC_Spo12Mois,
               SOC_Vac12Mois,
               SPI_CriAcc,
               SPI_CriRepr,
               SPI_CVF1,
               SPI_CVF2,
               SPI_CVF3,
               SPI_ExaReal,
               SPI_VEMS1,
               SPI_VEMS2,
               SPI_VEMS3,
               bras,
               resp_proc,
               TAR_ExaReal,
               TAR_ExaUni,
               TAR_TenArtDiaBraRefDr,
               TAR_TenArtDiaBraRefGa,
               TAR_TenArtDiaDr,
               TAR_TenArtDiaGa,
               TAR_TenArtDiaOrt,
               TAR_TenArtOrt,
               TAR_TenArtSysBraRefDr,
               TAR_TenArtSysBraRefGa,
               TAR_TenArtSysDr,
               TAR_TenArtSysGa,
               TAR_TenArtSysOrt,
               TRA_TraHomeo,
               SOC_Cah_Paracl,
               SOC_CES_Appli,
               SOC_HomeTime,
               SOC_DNaissance,
               SOC_DatExam)





# ----------------- clean variables

para$resp_proc <-  as.factor(para$resp_proc)
para$bras <- as.factor(para$bras)
para$AUD_AuDr500  <- as.numeric(as.character(para$AUD_AuDr500))
para$AUD_AuDr1000 <- as.numeric(as.character(para$AUD_AuDr1000))
para$AUD_AuDr2000 <- as.numeric(as.character(para$AUD_AuDr2000))
para$AUD_AuDr4000 <- as.numeric(as.character(para$AUD_AuDr4000))
para$AUD_AuDr8000 <- as.numeric(as.character(para$AUD_AuDr8000))
para$AUD_AuGa500  <- as.numeric(as.character(para$AUD_AuGa500))
para$AUD_AuGa1000 <- as.numeric(as.character(para$AUD_AuGa1000))
para$AUD_AuGa2000 <- as.numeric(as.character(para$AUD_AuGa2000))
para$AUD_AuGa4000 <- as.numeric(as.character(para$AUD_AuGa4000))
para$AUD_AuGa8000 <- as.numeric(as.character(para$AUD_AuGa8000))


# ----------------- add more variables

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

para$ldl= as.numeric(para$BIO_ChoTot_LDL-para$BIO_ChoHDL_LDL-(para$BIO_Trig_LDL/2.2))
para$class_ldl=as.factor(cut(para$ldl, breaks = c(-1,4.2,11), right = FALSE,include.lowest = TRUE))

#SEXe level
levels(para$SOC_Sex)<-c('M','F')

#Tour de taille

para$RTH=round(para$TAI_MesToTai/para$HAN_MesToHan,2)

para$class_rth=as.factor(ifelse( ((para$SOC_Sex==2 & para$RTH <=0.8 ) | (para$SOC_Sex==1 & para$RTH <=0.95 )),'Normal', 'Ob?sit? abdominale'))


para$VEMS =apply(para[,c('SPI_VEMS1','SPI_VEMS2','SPI_VEMS3')],1, max, na.rm=FALSE)
para$CVF =apply(para[,c('SPI_CVF1','SPI_CVF1','SPI_CVF1')],1, max, na.rm=FALSE)

para$Insuf_resp=round(para$VEMS/para$CVF*100,2)
para$Class_Insuf_resp=as.factor(cut(para$Insuf_resp, breaks = c(0,70,500),  right = FALSE,include.lowest = TRUE))

para$VDL_AC_Mal = ifelse(para$VDL_OeDrAvCorr > para$VDL_OeGaAvCorr ,para$VDL_OeDrAvCorr ,para$VDL_OeGaAvCorr)
para$VDL_CLASS_AC_Mal= as.factor(ifelse(para$VDL_AC_Mal >3 ,'NORMAL','MALVOYANCE'))


para$VDL_SC_Mal = ifelse(para$VDL_OeDrSaCorr > para$VDL_OeGaSaCorr ,para$VDL_OeDrSaCorr ,para$VDL_OeGaSaCorr)
para$VDL_CLASS_SC_Mal= as.factor(ifelse(para$VDL_SC_Mal >3 ,'NORMAL','MALVOYANCE'))

para$par_ces <- para$SOC_CES_NCes



# manual modifications (a voir)
para$SOC_NConstances <- as.integer(para$SOC_NConstances)
para$SOC_Sex <- as.factor(para$SOC_Sex)
para$SOC_CES_NCes <- as.factor(para$SOC_CES_NCes)
para$SOC_DNaissance <- as.Date(para$SOC_DNaissance, "%Y-%m-%d")
para$SOC_DatExam <- as.Date(para$SOC_DatExam, "%Y-%m-%d")
para$SOC_anne <- as.factor(format(para$SOC_DatExam, "%Y"))
para$SOC_moisanne <- as.factor(format(para$SOC_DatExam, "%m%y"))


# ----------------- Calcul de l'age

para$age=as.numeric(((para$SOC_DatExam-para$SOC_DNaissance )/365.5))

para$clas_age3=cut(floor(para$age), breaks = c(18,30,60,100), right = FALSE)
levels(para$clas_age3) <- c('18-29 ans','30-59 ans', '60 ans et plus')

para$clas_age5=cut(floor(para$age), breaks = c(18,35,45,55,65,74), right = FALSE,include.lowest = TRUE)
levels(para$clas_age5) <- c('18-34 ans','35-44 ans','45-54 ans', '55-64 ans','65-74 ans')

para$clas_age45an <- cut(floor(para$age), breaks = c(18,45,100), right = FALSE)
levels(para$clas_age45an) <- c('18-45 ans','45 ans et plus')



# ----------------- split numeric variables from others

para_num  <- para[ , sapply(para,  is.numeric)]
para_num$SOC_DatExam <- para$SOC_DatExam
para_num$SOC_moisanne <- para$SOC_moisanne
para_num$SOC_anne <- para$SOC_anne
para_num$CESantenne <- para$SOC_CES_Antenne


# ----------------- create clean para table (by excluding outliers)

para_bounds <- para
for (col in names(bounds_para)) {
    vect <- bounds_para[[col]]
    if(length(vect) == 2) {
        if(identical(vect, c(1,2))) {
            para_bounds[[col]][!(para_bounds[[col]] %in% vect)] <- NA
        }
        else {
            para_bounds[[col]][(para_bounds[[col]] < vect[1]) | (para_bounds[[col]] > vect[2])] <- NA
        }
    }
    else {
        para_bounds[[col]][!(para_bounds[[col]] %in% vect)] <- NA
    }
}
