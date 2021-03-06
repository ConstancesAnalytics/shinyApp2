resumer <- function (x) {
  name = deparse(substitute(x))
  N=length(x)
  Moyenne=round(mean(x,na.rm=TRUE),2)
  ecart_type=round(sd(x,na.rm=TRUE),2)
  Variance=round(var(x,na.rm=TRUE),2)
  min=min(x,na.rm=TRUE)
  q25=quantile(x, probs=0.25, na.rm=TRUE)
  q50=quantile(x, probs=0.5, na.rm=TRUE)
  q75=quantile(x, probs=0.75, na.rm=TRUE)
  max=max(x,na.rm=TRUE)
  Na=table(is.na(x))[2]
  h=as.data.frame(cbind(N,Moyenne,ecart_type,Variance, min, q25, q50,q75, max,  Na))
  rownames(h) <- name
  return(as.data.frame(h))
}
resumer_sans_nom <- function (x, var = "") {
  N=length(x)
  Moyenne=round(mean(x,na.rm=TRUE),2)
  ecart_type=round(sd(x,na.rm=TRUE),2)
  Variance=round(var(x,na.rm=TRUE),2)
  min=min(x,na.rm=TRUE)
  q25=quantile(x, probs=0.25, na.rm=TRUE)
  q50=quantile(x, probs=0.5, na.rm=TRUE)
  q75=quantile(x, probs=0.75, na.rm=TRUE)
  max=max(x,na.rm=TRUE)
  Na=table(is.na(x))[2]
  h=as.data.frame(cbind(N,Moyenne,ecart_type,Variance, min, q25, q50,q75, max,  Na))
  rownames(h) <- var
  return(as.data.frame(h))
}


resumer_borne <- function (x, vect=NULL) {
  if(length(vect) == 2) {
    l <- x[!((x < vect[1]) | (x > vect[2]))]
    df <- resumer(l)
    df$hb <- length(x) - length(l)
    return(df)
  }

  if(length(vect) > 2) {
    l <- x[!(!(x %in% vect))]
    df <- resumer(l)
    df$hb <- length(x) - length(l)
    return(df)
  }
}





borne <-function (x,inf,sup,y=NA) { x=ifelse( x >= inf & x <= sup, x,y)
x}



#tapply(para$PARACL_VDP_OeDrSaCorr, factor(para$PARACL_SOC_CES_Antenne), resumer_borne, vect=vecte))

resumer_bis<-function (x, inf=NULL, sup=NULL, vect=NULL) {
  name = deparse(substitute(x))
  N=length(x)
  Moyenne=round(mean(x,na.rm=TRUE),2)
  ecart_type=round(sd(x,na.rm=TRUE),2)
  Variance=round(var(x,na.rm=TRUE),2)
  min=min(x,na.rm=TRUE)
  q25=quantile(x, probs=0.25, na.rm=TRUE)
  q50=quantile(x, probs=0.5, na.rm=TRUE)
  q75=quantile(x, probs=0.75, na.rm=TRUE)
  max=max(x,na.rm=TRUE)
  Na=table(is.na(x))[2]
  if (is.null(inf) & is.null(sup) & is.null(vect))
    stop("You must precise vect OR inf-sup")
  if(!(is.null(inf) & is.null(sup)) & is.null(vect))
  {x=ifelse( (x >= inf) & (x <= sup), x, 9999)
  hb=table(x>sup)[2] }
  if((is.null(inf) & is.null(sup)) & !is.null(vect))
  {hb<-table((!x %in% vect)&(!is.na(x)))[2]}
  h=as.data.frame(cbind(N,Moyenne,ecart_type,Variance, min, q25, q50,q75, max,  Na, hb))
  rownames(h) <- name
  return(h)
}

# -------------------------------------------------------------- TDB

tbl = para_bounds
var = "HAU_MesTail"
var1 = 'SOC_BenCMU'
var2 = "clas_age3"
quant <- quantile(tbl[[var]], na.rm = TRUE) ## donnne les valeurs pour 0%, 25%, 50%, 75% et 100%
tbl[[var]] <- cut(floor(tbl[[var]]), breaks = c(quant[[1]], quant[[2]], quant[[3]], quant[[4]], quant[[5]]), right = FALSE, include.lowest = TRUE)
levels(tbl[[var]]) <- c(paste(quant[[1]], "-", quant[[2]]), paste(quant[[2]], "-", quant[[3]]), paste(quant[[3]], "-", quant[[4]]), paste(quant[[4]], "-", quant[[5]]))



TDB <- function(tbl, var, var1, var2) {

    # remove rows with empty var or age cat
    tbl <- filter(tbl, !is.na(tbl[[var]]), !is.na(tbl[[var2]]), !is.na(tbl[[var1]]), tbl[[var]] != "", tbl[[var]] != " ")

    # remove unused levels
    tbl[[var]] <- droplevels(tbl[[var]])
    tbl[[var1]] <- droplevels(tbl[[var1]])
    tbl[[var2]] <- droplevels(tbl[[var2]])

    # create tables with absolute numbers
    tbl_freq_m_f <- dcast(tbl, tbl[[var1]] + tbl[[var2]] ~ tbl[[var]], length )[,-c(1,2)]
    tbl_freq_e <- dcast(tbl,  tbl[[var2]] ~ tbl[[var]],length)[,-c(1)]
    tbl_freq_a <- rbind(dcast(tbl,  tbl[[var1]] ~ tbl[[var]],length)[,-c(1)], dcast(tbl,  . ~ tbl[[var]], length)[,-c(1)])

    # create tables with frequencies
    tbl_pr_m_f <- round(tbl_freq_m_f/apply(tbl_freq_m_f,1,sum,na.rm=TRUE)*100,2)
    tbl_pr_e <- round(tbl_freq_e/apply(tbl_freq_e,1,sum,na.rm=TRUE)*100,2)
    tbl_pr_a <- round(tbl_freq_a/apply(tbl_freq_a,1,sum,na.rm=TRUE)*100,2)

    # create a vector with alternate positions for absolute values and %
    nClass <- length(levels(tbl[[var]]))
    vect_c <- NULL
    for (i in 1:nClass)
    {vect_c <- append(vect_c,c(i,nClass+i))
    vect_c }

    # create combined tables
    tbl_M_F <- cbind(tbl_freq_m_f, tbl_pr_m_f)[, vect_c]
    tbl_E   <- cbind(tbl_freq_e, tbl_pr_e)[, vect_c]
    tbl_A   <- cbind(tbl_freq_a, tbl_pr_a)[, vect_c]

    # create a vector with alternate positions for age classes
    nClass_age <- length(levels(tbl[[var2]]))
    nvar1 <- length(levels(tbl[[var1]])) + 1
    h <- 1:(nvar1*nClass_age)
    vect_a <- c(rbind(matrix(h, nrow = nClass_age), (nvar1*nClass_age+1):(nvar1*nClass_age+nvar1)))

    # create combined table
    tdb <- rbind(tbl_M_F,tbl_E,tbl_A)[vect_a,]
    tdb
}




tbl_char <- function(x, df, var1, var2){

  # remove unused levels
  df[[var1]] <- droplevels(tbl[[var1]])
  df[[var2]] <- droplevels(tbl[[var2]])

  label1 <- append(levels(df[[var2]]), "Ensemble")
  label2 <- append(levels(df[[var1]]), "Ensemble")

  # format the table
  y <- sapply(x, function(x) iconv(x,  "UTF-8", "latin1"))
  c_age <- rep(label1, length(label2))
  c_vide <- rep("", length(label1) * length(label2))
  r_vide <- rep('', ncol(y)+1)

  y_bind1 <- cbind(c_vide, c_age, y)
  y_bind2 <- c("", r_vide)
  for(i in 1:length(label2)) {
      nstart <- 1 + (i-1)*length(label1)
      nstop <- i*length(label1)
      y_bind2 <- rbind(y_bind2, c(label2[i], r_vide), y_bind1[nstart:nstop,])
  }
  all_ch <- y_bind2[-1, ]
  colnames(all_ch)[1] <- var1
  colnames(all_ch)[2] <- var2
  for (i in 2:(length(colnames(all_ch))/2)) {
      colnames(all_ch)[2*i] <- "%"
  }

  return(all_ch)
}

# --------------------------------------------------------------

# tbl = para
# var = "HAU_MesTail"
# var1 = 'SOC_BenCMU'
# var2 = "clas_age3"
# quant <- quantile(tbl[[var]], na.rm = TRUE) ## donnne les valeurs pour 0%, 25%, 50%, 75% et 100%
# tbl[[var]] <- cut(floor(tbl[[var]]), breaks = c(quant[[1]], quant[[2]], quant[[3]], quant[[4]], quant[[5]]), right = FALSE, include.lowest = TRUE)
# levels(tbl[[var]]) <- c(paste(quant[[1]], "-", quant[[2]]), paste(quant[[2]], "-", quant[[3]]), paste(quant[[3]], "-", quant[[4]]), paste(quant[[4]], "-", quant[[5]]))


# x <- tdb
#
# var1 <- c_age
# var2 <- sexe
# tbl_char(x, para, c_age, sexe)

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
      geom_bar(aes(fill = variable),stat="identity",width=0.7) +
      geom_text(aes(label = ifelse(value> 3, value,' '), y =pos), size = 5,colour="#FFFFFF")+
      scale_fill_manual(values=c("#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e"))+
      annotate("text", x = 9, y =-10, label = "Hommes") +
      annotate("text", x = 9, y =-5, label = "18-29 ans") +
      annotate("text", x = 8, y =-5, label = "30-59 ans") +
      annotate("text", x = 7, y =-5, label = "60 ans et plus") +
      annotate("text", x = 6, y =-10, label = "Femmes") +
      annotate("text", x = 6, y =-5, label = "18-29 ans") +
      annotate("text", x = 5, y =-5, label = "30-59 ans") +
      annotate("text", x = 4, y =-5, label = "60 ans et plus") +
      annotate("text", x = 3, y =-10, label = "Ensemble") +
      annotate("text", x = 3, y =-5, label = "18-29 ans") +
      annotate("text", x = 2, y =-5, label = "30-59 ans") +
      annotate("text", x = 1, y =-5, label = "60 ans et plus") +
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
        plot.background=element_blank(),
        axis.title.x = element_text(face="plain",
                                    colour="black",
                                    size=12,hjust=0.54))

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
      geom_bar(aes(fill = variable),stat="identity",width=0.7) +
      geom_text(aes(label = ifelse(value> 3, value,' '), y =pos), size = 5,colour="#FFFFFF")+
      scale_fill_manual(values=c("#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e"))+
      annotate("text", x = 15, y =-10, label = "Hommes") +
      annotate("text", x = 15, y =-5, label = "18-34 ans") +
      annotate("text", x = 14, y =-5, label = "35-44 ans") +
      annotate("text", x = 13, y =-5, label = "45-54 ans") +
      annotate("text", x = 12, y =-5, label = "55-64 ans") +
      annotate("text", x = 11, y =-5, label = "64 ans et plus") +
      annotate("text", x = 10, y =-10, label = "Femmes") +
      annotate("text", x = 10, y =-5, label = "18-34 ans") +
      annotate("text", x = 9, y =-5, label = "35-44 ans") +
      annotate("text", x = 8, y =-5, label = "45-54 ans") +
      annotate("text", x = 7, y =-5, label = "55-64 ans") +
      annotate("text", x = 6, y =-5, label = "64 ans et plus") +
      annotate("text", x = 5, y =-10, label = "Ensemble") +
      annotate("text", x = 5, y =-5, label = "18-34 ans") +
      annotate("text", x = 4, y =-5, label = "35-44 ans") +
      annotate("text", x = 3, y =-5, label = "45-54 ans") +
      annotate("text", x = 2, y =-5, label = "55-64 ans") +
      annotate("text", x = 1, y =-5, label = "64 ans et plus") +
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
        plot.background=element_blank(),
        axis.title.x = element_text(face="plain",
                                    colour="black",
                                    size=12,hjust=0.54))
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
      geom_bar(aes(fill = variable),stat="identity",width=0.7) +
      geom_text(aes(label = ifelse(value> 3, value,' '), y =pos), size = 5,colour="#FFFFFF")+
      scale_fill_manual(values=c("#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e","#5f3b55","#5b9ab1","#9ebb83", "#ef7d56", "#c86e7e"))+
      annotate("text", x = 6, y =-10, label = "Hommes") +
      annotate("text", x = 6, y =-5, label = "18-45 ans") +
      annotate("text", x = 5, y =-5, label = "45 ans et plus") +
      annotate("text", x = 4, y =-10, label = "Femmes") +
      annotate("text", x = 4, y =-5, label = "18-29 ans") +
      annotate("text", x = 3, y =-5, label = "45 ans et plus") +
      annotate("text", x = 2, y =-10, label = "Ensemble") +
      annotate("text", x = 2, y =-5, label = "18-45 ans") +
      annotate("text", x = 1, y =-5, label = "45 ans et plus") +
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
        plot.background=element_blank(),
        axis.title.x = element_text(face="plain",
                                    colour="black",
                                    size=12,hjust=0.54))
  }}








