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

for(var in dic_nom_para$variable) {
    if(!is.na(dic_nom_para[which(dic_nom_para$variable == var),]$type)){
        if(dic_nom_para[which(dic_nom_para$variable == var),]$type == "num") {para[[var]] <- as.numeric(para[[var]])}
        else if(dic_nom_para[which(dic_nom_para$variable == var),]$type == "factor") {para[[var]] <- as.factor(para[[var]])}
        else if(dic_nom_para[which(dic_nom_para$variable == var),]$type == "date") {para[[var]] <- as.Date(para[[var]])}
    }
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



