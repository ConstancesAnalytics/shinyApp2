load('./para_elie.RData')
para_cols <- gsub("PARACL_","", colnames(para))
colnames(para) <- para_cols

para$SOC_DNaissance <- as.Date(para$SOC_DNaissance, "%Y-%m-%d")
para$SOC_DatExam <- as.Date(para$SOC_DatExam, "%Y-%m-%d")

para$age=as.numeric(((para$SOC_DatExam-para$SOC_DNaissance )/365.5))


para$resp_proc <- 'OK'
para$bras <- 'unknown'

para$resp_proc <- ifelse(!is.na(para$TAR_TenArtSysDr) & !is.na(para$TAR_TenArtDiaDr) & (para$TAR_TenArtSysDr < para$TAR_TenArtDiaDr), 'notOK_test_inversion', para$resp_proc)
para$resp_proc <- ifelse(!is.na(para$TAR_TenArtSysGa) & !is.na(para$TAR_TenArtDiaGa) & (para$TAR_TenArtSysGa < para$TAR_TenArtDiaGa), 'notOK_test_inversion', para$resp_proc)
para$resp_proc <- ifelse(!is.na(para$TAR_TenArtSysBraRefDr) & !is.na(para$TAR_TenArtDiaBraRefDr) & (para$TAR_TenArtSysBraRefDr < para$TAR_TenArtDiaBraRefDr), 'notOK_ref_inversion', para$resp_proc)
para$resp_proc <- ifelse(!is.na(para$TAR_TenArtSysBraRefGa) & !is.na(para$TAR_TenArtDiaBraRefGa) & (para$TAR_TenArtSysBraRefGa < para$TAR_TenArtDiaBraRefGa), 'notOK_ref_inversion', para$resp_proc)
para$resp_proc <- ifelse(!is.na(para$TAR_TenArtSysOrt) & !is.na(para$TAR_TenArtDiaOrt) & (para$TAR_TenArtSysOrt < para$TAR_TenArtDiaOrt), 'notOK_ortho_inversion', para$resp_proc)


for(i in 1:nrow(para)) {
  print(i)
  if (!is.na(para$TAR_TenArtSysDr[i]) & !is.na(para$TAR_TenArtDiaDr[i]) & !is.na(para$TAR_TenArtSysGa[i]) & !is.na(para$TAR_TenArtDiaGa[i])) {
    if(para$resp_proc[i] == 'OK') {
      if (para$TAR_TenArtSysDr[i] > para$TAR_TenArtSysGa[i]) {para$bras[i] = 'D'}
      if (para$TAR_TenArtSysDr[i] < para$TAR_TenArtSysGa[i]) {para$bras[i] = 'G'}
      if (para$TAR_TenArtSysDr[i] == para$TAR_TenArtSysGa[i]) {
        if (para$TAR_TenArtDiaDr[i] > para$TAR_TenArtDiaGa[i]) {para$bras[i] = 'D'}
        if (para$TAR_TenArtDiaDr[i] < para$TAR_TenArtDiaGa[i]) {para$bras[i] = 'G'}
        if (para$TAR_TenArtDiaDr[i] == para$TAR_TenArtDiaGa[i]) {para$bras[i] = 'G'} ## A VERIFIER POUR D OU G QUAND TOUT EST EGAL
      }
      if(!is.na(para$TAR_ExaUni[i]) & (para$TAR_ExaUni[i] == 1)) {para$resp_proc[i] <- 'Ok_test_uniBUTboth'}
    }
  }
  else if(!is.na(para$TAR_TenArtSysDr[i]) & !is.na(para$TAR_TenArtDiaDr[i]) & is.na(para$TAR_TenArtSysGa[i]) & is.na(para$TAR_TenArtDiaGa[i])) {
    para$resp_proc[i] <- ifelse(!is.na(para$TAR_ExaUni[i]) & (para$TAR_ExaUni[i] == 1), 'OK', 'notOK_test_missingvalues_notuni')
    if(para$resp_proc[i] == 'OK') {
      para$bras[i] = 'D'
    }
  }
  else if(is.na(para$TAR_TenArtSysDr[i]) & is.na(para$TAR_TenArtDiaDr[i]) & !is.na(para$TAR_TenArtSysGa[i]) & !is.na(para$TAR_TenArtDiaGa[i])) {
    para$resp_proc[i] <- ifelse(!is.na(para$TAR_ExaUni[i]) & (para$TAR_ExaUni[i] == 1), 'OK', 'notOK_test_missingvalues_notuni')
    if(para$resp_proc[i] == 'OK') {
      para$bras[i] = 'G'
    }
  }
  else {para$resp_proc[i] = 'notOK_test_missingvalues'}
  if(grepl('^OK', para$resp_proc[i])) {
    if( (!is.na(para$TAR_TenArtSysBraRefDr[i]) & !is.na(para$TAR_TenArtDiaBraRefDr[i])) | (!is.na(para$TAR_TenArtSysBraRefGa[i]) & !is.na(para$TAR_TenArtDiaBraRefGa[i])) ) {} else {para$resp_proc[i] = 'notOK_ref_missingvalues'}
  }
  if(grepl('^OK',para$resp_proc[i])) {
    if( (para$bras[i] == 'G') & !is.na(para$TAR_TenArtSysBraRefDr[i]) & !is.na(para$TAR_TenArtDiaBraRefDr[i]) ) {para$resp_proc[i] = 'notOK_ref_mauvais_bras'}
    if( (para$bras[i] == 'D') & !is.na(para$TAR_TenArtSysBraRefGa[i]) & !is.na(para$TAR_TenArtDiaBraRefGa[i]) ) {para$resp_proc[i] = 'notOK_ref_mauvais_bras'}
  }
  #----------------- Ortho
  if(grepl('^OK',para$resp_proc[i])) {
    if(para$age[i] > 65) {
      if(is.na(para$TAR_TenArtSysOrt[i]) | is.na(para$TAR_TenArtDiaOrt[i])) {para$resp_proc[i] = 'notOK_ortho_missingvalues'}
    }
  }
}


as.data.frame(table(para$resp_proc))

# ----------------- add more variables

#IMC
para$HAU_MesTail2 <- as.numeric(para$HAU_MesTail)*0.01
para$IMC <- (para$POI_MesPoi)/(para$HAU_MesTail2^2)
para$class_IMC <- as.factor(cut(para$IMC, breaks = c(12,18.5,24.9,29.9,70), right = FALSE, include.lowest = TRUE))

#class_BIO_ChoHDL
para$class_BIO_ChoHDL <- cut(para$BIO_ChoHDL, breaks = c(0.25,0.9, 3.5), right = FALSE,include.lowest = TRUE)

#class_BIO_Trig
para$class_BIO_Trig <- cut(para$BIO_Trig, breaks = c(0.2,2.5,30), right = FALSE,include.lowest = TRUE)

#class_BIO_ChoTot
para$class_BIO_ChoTot <- cut(para$BIO_ChoTot, breaks = c(1.5,2.2,6.0,15), right = FALSE,include.lowest = TRUE)

#LDL
para$BIO_ChoTot_LDL <- borne(para$BIO_ChoTot, 1.5 ,15)
para$BIO_ChoHDL_LDL <- borne(para$BIO_ChoHDL, 0.25 ,3.5)
para$BIO_Trig_LDL   <- borne(para$BIO_Trig, 0.1 ,3.75)

para$ldl <- as.numeric(para$BIO_ChoTot_LDL-para$BIO_ChoHDL_LDL-(para$BIO_Trig_LDL/2.2))
para$class_ldl <- as.factor(cut(para$ldl, breaks = c(-1,4.2,11), right = FALSE,include.lowest = TRUE))

#SEXe level
levels(para$SOC_Sex) <- c('M','F')

#Tour de taille

para$RTH  <- round(para$TAI_MesToTai/para$HAN_MesToHan,2)

para$class_rth  <- as.factor(ifelse( ((para$SOC_Sex==2 & para$RTH <=0.8 ) | (para$SOC_Sex==1 & para$RTH <=0.95 )),'Normal', 'Ob?sit? abdominale'))


para$VEMS  <- apply(para[,c('SPI_VEMS1','SPI_VEMS2','SPI_VEMS3')],1, max, na.rm=FALSE)
para$CVF  <- apply(para[,c('SPI_CVF1','SPI_CVF1','SPI_CVF1')],1, max, na.rm=FALSE)

para$Insuf_resp  <- round(para$VEMS/para$CVF*100,2)
para$Class_Insuf_resp  <- as.factor(cut(para$Insuf_resp, breaks = c(0,70,500),  right = FALSE,include.lowest = TRUE))

para$VDL_AC_Mal  <-  ifelse(para$VDL_OeDrAvCorr > para$VDL_OeGaAvCorr ,para$VDL_OeDrAvCorr ,para$VDL_OeGaAvCorr)
para$VDL_CLASS_AC_Mal  <- as.factor(ifelse(para$VDL_AC_Mal >3 ,'NORMAL','MALVOYANCE'))


para$VDL_SC_Mal  <-  ifelse(para$VDL_OeDrSaCorr > para$VDL_OeGaSaCorr ,para$VDL_OeDrSaCorr ,para$VDL_OeGaSaCorr)
para$VDL_CLASS_SC_Mal  <-  as.factor(ifelse(para$VDL_SC_Mal >3 ,'NORMAL','MALVOYANCE'))

para$par_ces <- para$SOC_CES_NCes



# manual modifications (a voir)
para$SOC_NConstances <- as.integer(para$SOC_NConstances)
para$SOC_Sex <- as.factor(para$SOC_Sex)
para$SOC_CES_NCes <- as.factor(para$SOC_CES_NCes)
para$SOC_anne <- as.factor(format(para$SOC_DatExam, "%Y"))
para$SOC_moisanne <- as.factor(format(para$SOC_DatExam, "%m%y"))

save(para, file = "paracl_romain.RData")



