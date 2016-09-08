library('shiny')
library('ggplot2')  # for the diamonds dataset
library('DT')
library('dplyr')
library('reshape2')


TDB <- function(tbl,var, sexe, c_age) {

    # remove rows with empty var or age cat
    tbl <- filter(tbl, !is.na(tbl[[var]]), !is.na(tbl[[c_age]]))

    #tbl[[var]] <- as.factor(gsub("^$", "VIDE", tbl[[var]]))  ## on ne pourrait pas mettre tbl[[var = ""]] <- "VIDE"

    # remove unused levels
    tbl[[var]] <- droplevels(tbl[[var]])

    # create tables with absolute numbers
    tbl_freq_s_a <- dcast(tbl, tbl[[sexe]] + tbl[[c_age]] ~ tbl[[var]], length )[,-c(1,2)]
    tbl_freq_a <- dcast(tbl,  tbl[[c_age]] ~ tbl[[var]],length)[,-c(1)]
    tbl_freq_s_e <- rbind(dcast(tbl,  tbl[[sexe]] ~ tbl[[var]],length)[,-c(1)], dcast(tbl,  . ~ tbl[[var]], length)[,-c(1)])

    # create tables with frequencies
    tbl_pr_s_a <- round(tbl_freq_s_a/apply(tbl_freq_s_a,1,sum,na.rm=TRUE)*100,2)
    tbl_pr_a <- round(tbl_freq_a/apply(tbl_freq_a,1,sum,na.rm=TRUE)*100,2)
    tbl_pr_s_e <- round(tbl_freq_s_e/apply(tbl_freq_s_e,1,sum,na.rm=TRUE)*100,2)

    # create a vector with alternate positions for absolute values and %
    nClass <- length(levels(tbl[[var]]))
    vect_c <- NULL
    for (i in 1:nClass)
    {vect_c <- append(vect_c,c(i,nClass+i))
    vect_c }

    # create combined tables
    tbl_M_F <- cbind(tbl_freq_s_a, tbl_pr_s_a)[, vect_c]
    tbl_E   <- cbind(tbl_freq_a, tbl_pr_a)[, vect_c]
    tbl_A   <- cbind(tbl_freq_s_e, tbl_pr_s_e)[, vect_c]

    # create a vector with alternate positions for age classes
    nClass_age <- length(levels(tbl[[c_age]]))
    h <- 1:(3*nClass_age)
    vect_a <- c(rbind(matrix(h, nrow = nClass_age), (3*nClass_age+1):(3*nClass_age+3)))

    # create combined table
    tdb <- rbind(tbl_M_F,tbl_E,tbl_A)[vect_a,]
    tdb
}



graph<-function(x, classe){
  
  if(classe=='clas_age3') {
    y=dim(x)[2]/2
    vec=seq(from = 2, to = 2*y, by = 2)
    lign=c(1:3,5:7,9:11)
    pl=x[lign , vec]
    colnames(pl) <- gsub("\\.1$","", colnames(pl))
    colnames(pl) <- gsub("\\)","[", colnames(pl))
    datm <- melt(cbind(pl, ind = rownames(pl)), id.vars = c('ind'))

    Data <- group_by(datm,ind) %>% mutate(pos = cumsum(value) - (0.5 * value))

    Data$ind   <- factor(Data$ind , levels=c(9,8,7,6,5,4,3,2,1))


    ggplot(Data,aes(x = ind, y = value,fill = variable)) +
        geom_bar(aes(fill = variable),stat="identity",width=0.4) +
        geom_text(aes(label = ifelse(value> 3, value,' '), y =pos), size = 4,colour="#FFFFFF")+
        scale_fill_manual(values=c("#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e"))+
        annotate("text", x = 9, y =-30, label = "Hommes") +
        annotate("text", x = 9, y =-10, label = "18-29 ans") +
        annotate("text", x = 8, y =-10, label = "30-59 ans") +
        annotate("text", x = 7, y =-10, label = "60 ans et plus") +
        annotate("text", x = 6, y =-30, label = "Femmes") +
        annotate("text", x = 6, y =-10, label = "18-29 ans") +
        annotate("text", x = 5, y =-10, label = "30-59 ans") +
        annotate("text", x = 4, y =-10, label = "60 ans et plus") +
        annotate("text", x = 3, y =-30, label = "Ensemble") +
        annotate("text", x = 3, y =-10, label = "18-29 ans") +
        annotate("text", x = 2, y =-10, label = "30-59 ans") +
        annotate("text", x = 1, y =-10, label = "60 ans et plus") +
        ylab("Pourcentage")+
        xlab(" ")+
        coord_flip() +
        theme(#axis.line=element_blank(),
            #axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            #axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="top",
            panel.background=element_blank(),
            #panel.border=element_blank(),
            #panel.grid.major=element_blank(),
            #panel.grid.minor=element_blank(),
            plot.background=element_blank())

  }
  else if(classe=='clas_age5') {
    y=dim(x)[2]/2
    vec=seq(from = 2, to = 2*y, by = 2)
    #Pour 5 classes
    lign=c(1:5,7:11,13:17)
    pl=x[lign , vec]
    
    colnames(pl) <- gsub("\\.1$","", colnames(pl))
    colnames(pl) <- gsub("\\)","[", colnames(pl))
    datm <- melt(cbind(pl, ind = rownames(pl)), id.vars = c('ind'))
    
    Data <- group_by(datm,ind) %>% mutate(pos = cumsum(value) - (0.5 * value))
    
    
    #classes 5
    Data$ind   <- factor(Data$ind , levels=c(15,14,13,12,11,10,9,8,7,6,5,4,3,2,1))
    
    
    
    
    #Pour 5 class
    ggplot(Data,aes(x = ind, y = value,fill = variable)) +
      geom_bar(aes(fill = variable),stat="identity",width=0.4) +
      geom_text(aes(label = ifelse(value> 3, value,' '), y =pos), size = 4,colour="#FFFFFF")+
      scale_fill_manual(values=c("#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e"))+
      annotate("text", x = 15, y =-30, label = "Hommes") +
      annotate("text", x = 15, y =-10, label = "18-34 ans") +
      annotate("text", x = 14, y =-10, label = "35-44 ans") +
      annotate("text", x = 13, y =-10, label = "45-54 ans") +
      annotate("text", x = 12, y =-10, label = "55-64 ans") +
      annotate("text", x = 11, y =-10, label = "64 ans et plus") +
      annotate("text", x = 10, y =-30, label = "Femmes") +
      annotate("text", x = 10, y =-10, label = "18-34 ans") +
      annotate("text", x = 9, y =-10, label = "35-44 ans") +
      annotate("text", x = 8, y =-10, label = "45-54 ans") +
      annotate("text", x = 7, y =-10, label = "55-64 ans") +
      annotate("text", x = 6, y =-10, label = "64 ans et plus") +
      annotate("text", x = 5, y =-30, label = "Ensemble") +
      annotate("text", x = 5, y =-10, label = "18-34 ans") +
      annotate("text", x = 4, y =-10, label = "35-44 ans") +
      annotate("text", x = 3, y =-10, label = "45-54 ans") +
      annotate("text", x = 2, y =-10, label = "55-64 ans") +
      annotate("text", x = 1, y =-10, label = "64 ans et plus") +
      ylab("Pourcentage")+
      xlab(" ")+
      coord_flip() +
      theme(#axis.line=element_blank(),
        #axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        #axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="top",
        panel.background=element_blank(),
        #panel.border=element_blank(),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
        plot.background=element_blank())
    }
else if(classe=='clas_age45an'){
  #Pour 2 class
  y=dim(x)[2]/2
  vec=seq(from = 2, to = 2*y, by = 2)
  #Pour 2 classes
  lign=c(1:2,4:5,7:8)
  pl=x[lign , vec]
  
  colnames(pl) <- gsub("\\.1$","", colnames(pl))
  colnames(pl) <- gsub("\\)","[", colnames(pl))
  datm <- melt(cbind(pl, ind = rownames(pl)), id.vars = c('ind'))
  
  Data <- group_by(datm,ind) %>% mutate(pos = cumsum(value) - (0.5 * value))
  #classes 2
  Data$ind   <- factor(Data$ind , levels=c(6,5,4,3,2,1))
  #Pour 2 class
  ggplot(Data,aes(x = ind, y = value,fill = variable)) +
    geom_bar(aes(fill = variable),stat="identity",width=0.4) +
    geom_text(aes(label = ifelse(value> 3, value,' '), y =pos), size = 4,colour="#FFFFFF")+
    scale_fill_manual(values=c("#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e"))+
    annotate("text", x = 6, y =-30, label = "Hommes") +
    annotate("text", x = 6, y =-10, label = "18-45 ans") +
    annotate("text", x = 5, y =-10, label = "45 ans et plus") +
    annotate("text", x = 4, y =-30, label = "Femmes") +
    annotate("text", x = 4, y =-10, label = "18-29 ans") +
    annotate("text", x = 3, y =-10, label = "45 ans et plus") +
    annotate("text", x = 2, y =-30, label = "Ensemble") +
    annotate("text", x = 2, y =-10, label = "18-45 ans") +
    annotate("text", x = 1, y =-10, label = "45 ans et plus") +
    ylab("Pourcentage")+
    xlab(" ")+
    coord_flip() +
    theme(#axis.line=element_blank(),
      #axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      #axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="top",
      panel.background=element_blank(),
      #panel.border=element_blank(),
      #panel.grid.major=element_blank(),
      #panel.grid.minor=element_blank(),
      plot.background=element_blank())
}}


load('./paracl_romain.RData')



para$age=as.numeric(((para$SOC_DatExam-para$SOC_DNaissance )/365.5))

para$clas_age3=cut(floor(para$age), breaks = c(18,30,60,100), right = FALSE)
levels(para$clas_age3) <- c('18-29 ans','30-59 ans', '60 ans et plus')

para$clas_age5=cut(floor(para$age), breaks = c(18,35,45,55,65,74), right = FALSE,include.lowest = TRUE)
levels(para$clas_age5) <- c('18-34 ans','35-44 ans','45-54 ans', '55-64 ans','65-74 ans')

para$clas_age45an <- cut(floor(para$age), breaks = c(18,45,100), right = FALSE)
levels(para$clas_age45an) <- c('18-45 ans','45 ans et plus')


all=TDB(para,'class_IMC', 'SOC_Sex', 'clas_age45an')



p<-graph(all, 'clas_age45an')
p

apply

x <- all

y=dim(x)[2]/2
vec=seq(from = 2, to = 2*y, by = 2)
#Pour 3 classes
lign=c(1:3,5:7,9:11)
#Pour 5 classes
#lign=c(1:5,7:11,13:17)
#Pour 2 classes
#lign=c(1:2,4:5,7:8)
pl=x[lign , vec]

colnames(pl) <- gsub("\\.1$","", colnames(pl))
colnames(pl) <- gsub("\\)","[", colnames(pl))
datm <- melt(cbind(pl, ind = rownames(pl)), id.vars = c('ind'))

Data <- group_by(datm,ind) %>% mutate(pos = cumsum(value) - (0.5 * value))

#classes 3
Data$ind   <- factor(Data$ind , levels=c(9,8,7,6,5,4,3,2,1))
#classes 2
#Data$ind   <- factor(Data$ind , levels=c(6,5,4,3,2,1))
#classes 5
#Data$ind   <- factor(Data$ind , levels=c(15,14,13,12,11,10,9,8,7,6,5,4,3,2,1))




#Pour 5 class
ggplot(Data,aes(x = ind, y = value,fill = variable)) +
    geom_bar(aes(fill = variable),stat="identity",width=0.4) +
    geom_text(aes(label = ifelse(value> 3, value,' '), y =pos), size = 4,colour="#FFFFFF")+
    scale_fill_manual(values=c("#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e"))+
     annotate("text", x = 15, y =-30, label = "Hommes") +
     annotate("text", x = 15, y =-10, label = "18-34 ans") +
     annotate("text", x = 14, y =-10, label = "35-44 ans") +
     annotate("text", x = 13, y =-10, label = "45-54 ans") +
     annotate("text", x = 12, y =-10, label = "55-64 ans") +
     annotate("text", x = 11, y =-10, label = "64 ans et plus") +
     annotate("text", x = 10, y =-30, label = "Femmes") +
     annotate("text", x = 10, y =-10, label = "18-34 ans") +
     annotate("text", x = 9, y =-10, label = "35-44 ans") +
     annotate("text", x = 8, y =-10, label = "45-54 ans") +
     annotate("text", x = 7, y =-10, label = "55-64 ans") +
     annotate("text", x = 6, y =-10, label = "64 ans et plus") +
     annotate("text", x = 5, y =-30, label = "Ensemble") +
     annotate("text", x = 5, y =-10, label = "18-34 ans") +
     annotate("text", x = 4, y =-10, label = "35-44 ans") +
     annotate("text", x = 3, y =-10, label = "45-54 ans") +
     annotate("text", x = 2, y =-10, label = "55-64 ans") +
     annotate("text", x = 1, y =-10, label = "64 ans et plus") +
    ylab("Pourcentage")+
    xlab(" ")+
    coord_flip() +
    theme(#axis.line=element_blank(),
        #axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        #axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="top",
        panel.background=element_blank(),
        #panel.border=element_blank(),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
        plot.background=element_blank())








#Pour 2 class
ggplot(Data,aes(x = ind, y = value,fill = variable)) +
    geom_bar(aes(fill = variable),stat="identity",width=0.4) +
    geom_text(aes(label = ifelse(value> 3, value,' '), y =pos), size = 4,colour="#FFFFFF")+
    scale_fill_manual(values=c("#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e"))+
     annotate("text", x = 6, y =-30, label = "Hommes") +
     annotate("text", x = 6, y =-10, label = "18-45 ans") +
     annotate("text", x = 5, y =-10, label = "45 ans et plus") +
     annotate("text", x = 4, y =-30, label = "Femmes") +
     annotate("text", x = 4, y =-10, label = "18-29 ans") +
     annotate("text", x = 3, y =-10, label = "45 ans et plus") +
     annotate("text", x = 2, y =-30, label = "Ensemble") +
     annotate("text", x = 2, y =-10, label = "18-45 ans") +
     annotate("text", x = 1, y =-10, label = "45 ans et plus") +
    ylab("Pourcentage")+
    xlab(" ")+
    coord_flip() +
    theme(#axis.line=element_blank(),
        #axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        #axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="top",
        panel.background=element_blank(),
        #panel.border=element_blank(),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
        plot.background=element_blank())





#Pour trois class
ggplot(Data,aes(x = ind, y = value,fill = variable)) +
    geom_bar(aes(fill = variable),stat="identity",width=0.8) +
    geom_text(aes(label = ifelse(value> 3, value,' '), y =pos), size = 5,colour="#FFFFFF")+
    scale_fill_manual(values=c("#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e"))+
    annotate("text", x = 9, y =-30, label = "Hommes") +
    annotate("text", x = 9, y =-10, label = "18-29 ans") +
    annotate("text", x = 8, y =-10, label = "30-59 ans") +
    annotate("text", x = 7, y =-10, label = "60 ans et plus") +
    annotate("text", x = 6, y =-30, label = "Femmes") +
    annotate("text", x = 6, y =-10, label = "18-29 ans") +
    annotate("text", x = 5, y =-10, label = "30-59 ans") +
    annotate("text", x = 4, y =-10, label = "60 ans et plus") +
    annotate("text", x = 3, y =-30, label = "Ensemble") +
    annotate("text", x = 3, y =-10, label = "18-29 ans") +
    annotate("text", x = 2, y =-10, label = "30-59 ans") +
    annotate("text", x = 1, y =-10, label = "60 ans et plus") +
    ylab("Pourcentage")+
    xlab(" ")+
    coord_flip() +
    theme(#axis.line=element_blank(),
        #axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        #axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="top",
        panel.background=element_blank(),
        #panel.border=element_blank(),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
        plot.background=element_blank())


