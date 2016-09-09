library('shiny')
library('ggplot2')  # for the diamonds dataset
library('DT')
library('dplyr')
library('reshape2')
source('./function.R')

# ----------------- load para file

load('./paracl_romain.RData')
test <- as.numeric(unlist(strsplit("1, 2, 3", "[,]")))

# ----------------- load dictionnary
dic_nom_para <- read.csv('dictionnaire_para.csv',header = TRUE, sep=';', encoding = "UTF-8")
dic_nom_para$nom <- as.character(dic_nom_para$nom)
dic_nom_para$variable <- as.character(dic_nom_para$variable)

# ----------------- select variables

para <- para[,colnames(para) %in% dic_nom_para$variable]

# ----------------- type variables

type_para <- list(
    par_ces = "num",
    VDL_BiAvCorr = "num",
    VDL_BiSaCorr = "num",
    VDL_CeProAmbMono = "factor",
    VDL_ExaReal = "factor",
    VDL_OeDrAvCorr = "num",
    VDL_OeDrSaCorr = "num",
    VDL_OeGaAvCorr = "num",
    VDL_OeGaSaCorr = "num",
    VDP_BiAvCorr = "num",
    VDP_BiSaCorr = "num",
    VDP_CeProAmbMono = "factor",
    VDP_ExaReal = "factor",
    VDP_OeDrAvCorr = "num",
    VDP_OeDrSaCorr = "num",
    VDP_OeGaAvCorr = "num",
    VDP_OeGaSaCorr = "num",
    AUD_AuDr1000 = "num",
    AUD_AuDr2000 = "num",
    AUD_AuDr4000 = "num",
    AUD_AuDr500 = "num",
    AUD_AuDr8000 = "num",
    AUD_AuGa1000 = "num",
    AUD_AuGa2000 = "num",
    AUD_AuGa4000 = "num",
    AUD_AuGa500 = "num",
    AUD_AuGa8000 = "num",
    AUD_ExaReal = "factor",
    BIO_Alat = "num",
    BIO_ChoHDL = "num",
    BIO_ChoTot = "num",
    BIO_Crea = "num",
    BIO_Gam = "num",
    BIO_Glyc = "num",
    BIO_Trig = "num",
    HEM_AspSer = "factor",
    HAN_ExaReal = "factor",
    HAN_MesToHan = "num",
    HAU_ExaReal = "factor",
    HAU_MesTail = "num",
    OMB_ExaReal = "factor",
    OMB_MesPeri = "num",
    POI_ExaReal = "factor",
    POI_MesPoi = "num",
    TAI_ExaReal = "factor",
    TAI_MesToTai = "num",
    DEN_ConcDen1 = "num",
    DEN_DerExaDen = "num",
    DEN_GinGiv = "num",
    DEN_LesMuq = "num",
    DEN_NbDenDefCar = "num",
    DEN_NbDenDefExt = "num",
    DEN_NbDenDefObt = "num",
    DEN_NbDenDefSai = "num",
    DEN_NbProAdj = "num",
    DEN_NbProCon = "num",
    DEN_Orth = "num",
    DEN_PlaBac = "num",
    DEN_PreTar = "num",
    DEN_SilAnf16 = "factor",
    DEN_SilAnf26 = "factor",
    DEN_SilAnf36 = "factor",
    DEN_SilAnf46 = "factor",
    DEN_TrbATM = "num",
    ECG_ExaReal = "factor",
    HEM_BasoPhi = "num",
    HEM_EosiPhi = "num",
    HEM_GloBla = "num",
    HEM_GloRou = "num",
    HEM_Hemato = "num",
    HEM_Hemo = "num",
    HEM_Lympho = "num",
    HEM_Monocy = "num",
    HEM_NeuPhi = "num",
    HEM_Plaq = "num",
    HEM_VolGlobMoy = "num",
    SAN_DiffPrel = "factor",
    SAN_ExaReal = "factor",
    SAN_HeFinDerRepa = "time",
    SAN_HeuPrel = "time",
    BAU_Gluc = "factor", ## A NETTOYER
    BAU_Leuco = "factor", ## A NETTOYER
    BAU_Nitr = "factor", ## A NETTOYER
    BAU_Prot = "factor", ## A NETTOYER
    BAU_Sang = "factor", ## A NETTOYER
    BIR_Creat = "num",
    BIR_Gluc = "num",
    BIR_MicAlb = "num",
    BIR_Prot = "num",
    URI_ExaReal = "factor",
    URI_HeuRec = "heure",
    SAN_Regle = "factor",
    SOC_AgJrBil = "num",
    SOC_AidMedEta = "factor",
    SOC_BenCMU = "factor",
    SOC_BenRMIRSA = "factor",
    SOC_CES_Antenne = "factor",
    SOC_CES_NCes = "factor",
    SOC_Cin12Mois = "factor",
    SOC_CMUBase = "factor",
    SOC_CMUComp = "factor",
    SOC_Con6Mois = "factor",
    SOC_CouvCompConst = "factor",
    SOC_CouvCompDecl = "factor",
    SOC_JeuVoIns = "factor",
    SOC_NConstances = "factor",
    SOC_PbAchatNourr = "factor",
    SOC_PrChar100 = "factor",
    SOC_Precar = "num",
    SOC_Proprio = "factor",
    SOC_QHerbDiff = "factor",
    SOC_QMatDiff = "factor",
    SOC_RenTravSocial = "factor",
    SOC_ScPrec = "num",
    SOC_Sex = "factor",
    SOC_Spo12Mois = "factor",
    SOC_Vac12Mois = "factor",
    SPI_CriAcc = "factor",
    SPI_CriRepr = "factor",
    SPI_CVF1 = "num",
    SPI_CVF2 = "factor",
    SPI_CVF3 = "factor",
    SPI_ExaReal = "factor",
    SPI_VEMS1 = "num",
    SPI_VEMS2 = "num",
    SPI_VEMS3 = "num",
    bras = "factor",
    resp_proc = "factor",
    TAR_ExaReal = "factor",
    TAR_ExaUni = "factor",
    TAR_TenArtDiaBraRefDr = "num",
    TAR_TenArtDiaBraRefGa = "num",
    TAR_TenArtDiaDr = "num",
    TAR_TenArtDiaGa = "num",
    TAR_TenArtDiaOrt = "num",
    TAR_TenArtOrt = "num",
    TAR_TenArtSysBraRefDr = "num",
    TAR_TenArtSysBraRefGa = "num",
    TAR_TenArtSysDr = "num",
    TAR_TenArtSysGa = "num",
    TAR_TenArtSysOrt = "num",
    TRA_TraHomeo = "num",
    SOC_Cah_Paracl = "factor",
    SOC_CES_Appli = "factor",
    SOC_HomeTime = "time",
    SOC_DNaissance = "date",
    SOC_DatExam = "date",
    ldl = "num")

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

for(var in names(type_para)) {
    if(type_para[[var]] == "num") {para[[var]] <- as.numeric(para[[var]])}
    else if(type_para[[var]] == "factor") {para[[var]] <- as.factor(para[[var]])}
    else if(type_para[[var]] == "date") {para[[var]] <- as.Date(para[[var]])}
}


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
for (var in dic_nom_para$variable) {
    vect <- as.numeric(unlist(strsplit(as.character(dic_nom_para[which(dic_nom_para$variable == var),]$borne), "[,]")))
    if(length(vect) == 2) {
        if(identical(vect, c(1,2))) {
            para_bounds[[var]][!(para_bounds[[var]] %in% vect)] <- NA
        }
        else {
            para_bounds[[var]][(para_bounds[[var]] < vect[1]) | (para_bounds[[var]] > vect[2])] <- NA
        }
    }
    else {
        para_bounds[[var]][!(para_bounds[[var]] %in% vect)] <- NA
    }
}

para_bounds$CESantenne <-  para$SOC_CES_Antenne



