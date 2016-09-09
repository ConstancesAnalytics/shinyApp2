library('shiny')
library('ggplot2')
library('DT')
library('dplyr')
library('scales')
library('reshape2')
library('plotly')



shinyServer(function(input, output) {

para_date <- reactive({
    out <- subset(para, (input$dateRange[1] <= SOC_DatExam) & (input$dateRange[2] >= SOC_DatExam))
    out
})

para_num_date <- reactive({
    out <- subset(para_num, (input$dateRange[1] <= SOC_DatExam) & (input$dateRange[2] >= SOC_DatExam))
    out
})

para_bounds_date <- reactive({
    out <- subset(para_bounds, (input$dateRange[1] <= SOC_DatExam) & (input$dateRange[2] >= SOC_DatExam))
    out
})

para1 <- reactive({
     para <- subset(para, (input$dateRange[1] <= SOC_DatExam) & (input$dateRange[2] >= SOC_DatExam))
     nom_var0 <- subset(dic_nom_para, categorie==input$VAR)
     vect_select0 <- c(nom_var0$variable, 'CESantenne','SOC_CES_NCes' ,'SOC_DatExam','par_ces', 'clas_age5','clas_age45an','clas_age3','SOC_Sex','SOC_moisanne','SOC_anne', 'SOC_NConstances')
     para20 <- para %>%  select(which(names(para) %in% vect_select0))
     para20
})

para_num1 <- reactive({
    para_num0 <- subset(para_num,input$dateRange[1]<= SOC_DatExam & input$dateRange[2]>= SOC_DatExam  )
    nom_var1 <- subset(dic_nom_para, categorie==input$VAR)
    vect_select <- c(nom_var1$variable,'CESantenne','SOC_CES_NCes' ,'SOC_DatExam','par_ces', 'clas_age5','clas_age45an','clas_age3','SOC_Sex')
    para_num20 <- para_num0 %>% select( which(names(para_num) %in% vect_select)  )
    para_num20
})

# ---------------------------- Panel 1

para1_num <- reactive({
    nom_var <- subset(dic_nom_para, categorie==input$panel1var1)
    vect_select <- c(nom_var$variable,'CESantenne','SOC_CES_NCes' ,'SOC_DatExam','par_ces', 'clas_age5','clas_age45an','clas_age3','SOC_Sex')
    out <- para_num_date() %>% select( which(names(para_num) %in% vect_select)  )
    out
})

output$datatable1 <- DT::renderDataTable({
    dttbl <- para1_num()

    ifelse(input$CES %in% levels(dttbl$CESantenne),
           dttbl_CESfilt <- dttbl %>% filter(CESantenne == input$CES) %>% select(-CESantenne,-SOC_DatExam,-par_ces ) ,
           dttbl_CESfilt <- dttbl %>% select(-CESantenne,-SOC_DatExam,-par_ces))

    temp_df <- data.frame(var_names = colnames(dttbl_CESfilt)) %>% left_join(dic_nom_para, by = c("var_names" = "variable"))    ## on peut mettre ça
    colnames(dttbl_CESfilt) <- temp_df$nom                                                                                      ## dans global
    dttbl_sum <- as.data.frame(t(sapply(dttbl_CESfilt, resumer)))
    DT::datatable(
        dttbl_sum , options = list(
            lengthMenu = list(c(5, 30, -1), c('5', '15', 'All')),
            pageLength = 30,
            paging  = FALSE
        )
    )
})

# ---------------------------- Panel 2

para2_num <- reactive({
    nom_var <- subset(dic_nom_para, categorie==input$panel2var2)
    vect_select <- c(nom_var$variable,'CESantenne','SOC_CES_NCes' ,'SOC_DatExam','par_ces', 'clas_age5','clas_age45an','clas_age3','SOC_Sex')
    out <- para_num_date() %>% select( which(names(para_num) %in% vect_select)  )
    out
})


output$panel2var3 <- renderUI({
    all.list_num <- colnames(para2_num())
    dic_nom_para_rest <- dic_nom_para %>% filter(variable %in% all.list_num)
    selectInput("panel2var3", "variable", choices = dic_nom_para_rest$nom , selected = dic_nom_para_rest$nom[1])
})


output$panel2_text <- renderText({
    var_tmp <- dic_nom_para$variable[which(dic_nom_para$nom == input$panel2var3)]
    nom <- dic_nom_para %>% filter(variable==var_tmp) %>% select(nom)
    paste("You have selected", nom)
})

output$panel2_summary <- DT::renderDataTable({
    var_tmp <- dic_nom_para$variable[which(dic_nom_para$nom==input$panel2var3)]
    data_summary <-resumer_sans_nom(para2_num()[,var_tmp], "Tous CES")
    DT::datatable(
        data_summary , options = list(
            paging  = FALSE,
            searching =FALSE
        )
    )
})

output$datatable2 <- DT::renderDataTable({
    var_tmp <- dic_nom_para$variable[which(dic_nom_para$nom == input$panel2var3)]
    dttbl <- do.call(rbind, tapply(para2_num()[,var_tmp], para2_num()[,input$panel2var1], resumer_borne, vect = get(var_tmp, bounds_para)))
    DT::datatable(
        dttbl, options = list(
            paging  = FALSE,
            lengthChange = FALSE
        )
    )
})

# ---------------------------- Panel 3


# ---------------------------- Panel 4

para4 <- reactive({
    nom_var <- dic_nom_para %>% filter(categorie==input$panel4var3)
    vect_select <- c(nom_var$variable, 'CESantenne','SOC_CES_NCes' ,'SOC_DatExam','par_ces', 'clas_age5','clas_age45an','clas_age3','SOC_Sex','SOC_moisanne','SOC_anne', 'SOC_NConstances')
    out <- para_bounds_date() %>%  select( which(names(para_bounds_date()) %in% vect_select) )
    out
})

output$panel4var4 <- renderUI({
    #para_fac <- para4()[ , sapply(para4(),  is.factor)]
    all.list_num <- colnames(para4())
    dic_nom_para_sel <- dic_nom_para %>% filter(variable %in% all.list_num)
    selectInput("panel4var4", "variable", choices = dic_nom_para_sel$nom , selected = dic_nom_para_sel$nom[1])
})

output$datatable4  <- DT::renderDataTable({

    ifelse(input$panel4var1 %in% levels(para4()$CESantenne),
           para_tmp <- para4() %>% filter(CESantenne == input$panel4var1) %>% select(-CESantenne,-SOC_DatExam,-par_ces ) ,
           para_tmp <- para4() )

    var_tmp <- dic_nom_para$variable[which(dic_nom_para$nom==input$panel4var4)]
    if (is.numeric(para_tmp[[var_tmp]])){
        min_var <- min(para_tmp[[var_tmp]], na.rm = TRUE)
        max_var <- max(para_tmp[[var_tmp]], na.rm = TRUE)
        inter <- (max_var - min_var)/4
        para_tmp[[var_tmp]] <- cut(floor(para_tmp[[var_tmp]]), breaks = c(min_var, min_var + inter, min_var + 2*inter, min_var + 3*inter,max_var), right = FALSE, include.lowest = TRUE)
        levels(para_tmp[[var_tmp]]) <- c(paste(min_var, "-", min_var + inter), paste(min_var + inter, "-", min_var + 2*inter), paste(min_var + 2*inter, "-", min_var + 3*inter), paste(min_var + 3*inter, "-", max_var))
    }
    all <- TDB(para_tmp, var_tmp,'SOC_Sex', input$panel4var2 )
    lab <- append(levels(para_tmp[,input$panel4var2]),c('Ensemble'))
    data_sum <- tbl_char(all,lab)
    DT::datatable(
        data_sum, options = list(
            lengthMenu = list(c(5, 30, -1), c('5', '15', 'All')),
            pageLength = 30,
            paging  = FALSE,
            searching = FALSE
        )
    )
})

# ---------------------------- Panel 5


para5 <- reactive({
    nom_var <- dic_nom_para %>% filter(categorie==input$panel5var3)
    vect_select <- c(nom_var$variable, 'CESantenne','SOC_CES_NCes' ,'SOC_DatExam','par_ces', 'clas_age5','clas_age45an','clas_age3','SOC_Sex','SOC_moisanne','SOC_anne', 'SOC_NConstances')
    out <- para_bounds_date() %>%  select( which(names(para_bounds_date()) %in% vect_select) )
    out
})

output$panel5var4 <- renderUI({
    all.list_num <- colnames(para5())
    dic_nom_para_sel <- dic_nom_para %>% filter(variable %in% all.list_num)
    selectInput("panel5var4", "variable", choices = dic_nom_para_sel$nom , selected = dic_nom_para_sel$nom[1])
})

Plot1 <- reactive({

    ifelse(input$panel5var1 %in% levels(para5()$CESantenne),
           para_tmp <- para5() %>% filter(CESantenne == input$panel5var1) %>% select(-CESantenne,-SOC_DatExam,-par_ces ) ,
           para_tmp <- para5() )

    var_tmp <- dic_nom_para$variable[which(dic_nom_para$nom==input$panel5var4)]
    if (is.numeric(para_tmp[[var_tmp]])){
        quant <- quantile(para_tmp[[var_tmp]], na.rm = TRUE) ## donnne les valeurs pour 0%, 25%, 50%, 75% et 100%
        para_tmp[[var_tmp]] <- cut(para_tmp[[var_tmp]], breaks = c(quant[[1]], quant[[2]], quant[[3]], quant[[4]], quant[[5]]), right = FALSE, include.lowest = TRUE)
        #min_var <- min(para_tmp[[var_tmp]], na.rm = TRUE)
        #max_var <- max(para_tmp[[var_tmp]], na.rm = TRUE)
        #inter <- (max_var - min_var)/4
        #para_tmp[[var_tmp]] <- cut(floor(para_tmp[[var_tmp]]), breaks = c(min_var, min_var + inter, min_var + 2*inter, min_var + 3*inter,max_var), right = FALSE, include.lowest = TRUE)
        levels(para_tmp[[var_tmp]]) <- c(paste(quant[[1]], "-", quant[[2]]), paste(quant[[2]], "-", quant[[3]]), paste(quant[[3]], "-", quant[[4]]), paste(quant[[4]], "-", quant[[5]]))
    }

    all <- TDB(para_tmp, var_tmp,'SOC_Sex', input$panel5var2 )
   graph(all, input$panel5var2)


})

output$plot5 <- renderPlot({
    p <- Plot1()
    print(p)
})


output$downloadPlot<-downloadHandler(
    filename = function() {
        paste('plot', '.png', sep='')
    },
    content=function(file){
        png(file)
        print(Plot1())
        dev.off()
    })


# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------





  output$variable3 <- renderUI({
    para<-para1()
    all.list_num<-c(colnames(para),c('SOC_moisanne','SOC_anne','CESantenne'))
    dic_nom_para_rest<-dic_nom_para %>% filter(variable %in%all.list_num)
    selectInput("variable3", "variable3", choices = dic_nom_para_rest$nom, selected = dic_nom_para_rest$nom[1])
  })

  output$variable4 <- renderUI({
    para<-para1()
    all.list_num<-colnames(para)
    dic_nom_para_rest<-dic_nom_para %>% filter(variable %in%all.list_num)
    selectInput("variable4", "variable4", choices = dic_nom_para_rest$nom, selected = dic_nom_para_rest$nom[1])
  })


  output$variable6a <- renderUI({
    para<-para1()
    para_fac  <- para[ , sapply(para,  is.factor)]
    all.list_num<-colnames(para_fac)
    dic_nom_para_rest<-dic_nom_para %>% filter(variable %in%all.list_num)
    selectInput("variable6a", "variable6a", choices = dic_nom_para_rest$nom , selected = dic_nom_para_rest$nom[1])
  })

  output$variable7a <- renderUI({
    para<-para_num1()
    all.list_num<-colnames(para)
    dic_nom_para_rest<-dic_nom_para %>% filter(variable %in%all.list_num)
    selectInput("variable7a", "variable7a", choices = dic_nom_para_rest$nom , selected = dic_nom_para_rest$nom[1])
  })
  output$variable7b <- renderUI({
    para<-para_num1()
    all.list_num<-colnames(para)
    dic_nom_para_rest<-dic_nom_para %>% filter(variable %in%all.list_num)
    selectInput("variable7b", "variable7b", choices = dic_nom_para_rest$nom , selected = dic_nom_para_rest$nom[1])
  })
  output$variable7c <- renderUI({
    para<-para1()
    all.list_num<-colnames(para)
    dic_nom_para_rest<-dic_nom_para %>% filter(variable %in%all.list_num)
    selectInput("variable7c", "variable7c", choices = dic_nom_para_rest$nom , selected = dic_nom_para_rest$nom[1])
  })

  output$variable8 <- renderUI({
    para<-para_num1()
    all.list_num<-colnames(para)
    dic_nom_para_rest<-dic_nom_para %>% filter(variable %in%all.list_num)
    selectInput("variable8", "variable8", choices = dic_nom_para_rest$nom , selected = dic_nom_para_rest$nom[1])
  })
  output$variable9 <- renderUI({
    para<-para1()
    all.list_num<-c(colnames(para),'SOC_NConstances')
    #dic_nom_para_rest<-dic_nom_para %>% filter(variable %in%all.list_num)
    selectInput("variable9", "variable9", choices = all.list_num , selected = all.list_num[1:2], multiple = TRUE)
  })
  output$variable10a <- renderUI({
    para<-para1()
    para_fac  <- para[ , sapply(para,  is.factor)]
    all.list_num<-colnames(para_fac)
    dic_nom_para_rest<-dic_nom_para %>% filter(variable %in%all.list_num)
    selectInput("variable10a", "variable10a", choices = dic_nom_para_rest$nom , selected = dic_nom_para_rest$nom[1])
  })
  output$variable10b <- renderUI({
    para<-para1()
    all.list_num<-colnames(para)
    dic_nom_para_rest<-dic_nom_para %>% filter(variable %in%all.list_num)
    selectInput("variable10b", "variable10b", choices = dic_nom_para_rest$nom , selected = dic_nom_para_rest$nom[1])
  })





  output$text1 <- renderText({

    var_tmp=dic_nom_para$variable[which(dic_nom_para$nom==input$variable2)]

    nom=dic_nom_para %>% filter(variable==var_tmp)%>% select(nom)
    paste("You have selected", nom  )
  })

  output$summary  <- DT::renderDataTable({
    para_num<-para_num1()
    para<-para1()
    var_tmp=dic_nom_para$variable[which(dic_nom_para$nom==input$variable2)]

    var_tmp=input$variable2
    var_tmp2=dic_nom_para$variable[which(dic_nom_para$nom==var_tmp)]
    data_summary <-resumer_sans_nom(para_num[,var_tmp2])
    DT::datatable(
      data_summary , options = list(
        paging  = FALSE,
        searching =FALSE
      )
    )
  })


  output$datatable3  <- DT::renderDataTable({
    para<-para1()
    var_tmp3=dic_nom_para$variable[which(dic_nom_para$nom==input$variable3)]
    var_tmp4=dic_nom_para$variable[which(dic_nom_para$nom==input$variable4)]
    data_sum2 <- dcast(para, para[, var_tmp3] ~ para[ ,var_tmp4],length)
    DT::datatable(
      data_sum2, options = list(
        lengthMenu = list(c(5, 30, -1), c('5', '15', 'All')),
        pageLength = 30
      )
    )
  })








output$plot2 <- renderPlot({
  para_num<-para_num1()
  para<-para1()
  var_tmp7a=dic_nom_para$variable[which(dic_nom_para$nom==input$variable7a)]
  var_tmp7b=dic_nom_para$variable[which(dic_nom_para$nom==input$variable7b)]
  var_tmp7c=dic_nom_para$variable[which(dic_nom_para$nom==input$variable7c)]
    p1 <- ggplot(para_num, aes(x =para[, var_tmp7a], y = para[,var_tmp7b])) + geom_point(aes(color = para[,var_tmp7c]),size=0.5)

   print(p1)
     }, height = 700, width = 1200)

output$plot3 <- renderPlotly({
  para_num<-para_num1()
  para<-para1()
  var_tmp8=dic_nom_para$variable[which(dic_nom_para$nom==input$variable8)]
    p <- ggplot(para, aes(as.factor(para[, input$variable10]), para[,var_tmp8]  )) + geom_boxplot()
    p3 <- ggplotly(p)  %>% layout(autosize = F,  height = 700,width = 1200)
    p3

     })

output$mytable1 <- DT::renderDataTable({
  para<-para1()
  #var_tmp9=dic_nom_para$variable[which(dic_nom_para$nom==input$variable9)]
  DT::datatable(para[ , input$variable9,drop = FALSE],
                filter = 'top',extensions = 'Buttons', options = list(
                  dom = 'Bfrtip',
                  lengthMenu = list(c(5, 30, -1), c('5', '15', 'All')),
                  pageLength = 30,
                  buttons =
                    list('copy', 'print', list(
                      extend = 'collection',
                      buttons = c('csv', 'excel', 'pdf'),
                      text = 'Download'
                    ))


                ))
})


output$plot4 <- renderPlot({
  para_num<-para_num1()
  para<-para1()
  var_tmp10a=dic_nom_para$variable[which(dic_nom_para$nom==input$variable10a)]
  var_tmp10b=dic_nom_para$variable[which(dic_nom_para$nom==input$variable10b)]
  p <- ggplot(para, aes(as.factor(para[, var_tmp10a]),  para[,var_tmp10b]  )) + geom_boxplot()+ geom_jitter()
  print(p)

}, height = 700, width = 1200)


})