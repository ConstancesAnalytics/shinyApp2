setwd("~/gir_r_app/shinyApp2")

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

save(para, file = "paracl_romain.RData")


as.data.frame(table(para$resp_proc))
