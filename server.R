library('shiny')
library('ggplot2')  # for the diamonds dataset
library('DT')
library('dplyr')
library('scales')
library('reshape2')
library('plotly')



shinyServer(function(input, output) {


para1 <- reactive({
     para<-subset(para,input$dateRange[1]<= SOC_DatExam & input$dateRange[2]>= SOC_DatExam )
      #para %>%filter(input$dateRange[1]<= SOC_DatExam, input$dateRange[2]>= SOC_DatExam )
     nom_var0=subset(dic_nom_para, categorie==input$VAR)
     vect_select0=c(nom_var0$variable,'CESantenne','SOC_CES_NCes' ,'SOC_DatExam','par_ces', 'clas_age5','clas_age45an','clas_age3','SOC_Sex','SOC_moisanne','SOC_anne', 'SOC_NConstances')
     para20<-para %>%  select( which(names(para) %in% vect_select0))
     para20

     })

para_num1 <- reactive({
    para_num0<-subset(para_num,input$dateRange[1]<= SOC_DatExam & input$dateRange[2]>= SOC_DatExam  )
    nom_var1=subset(dic_nom_para, categorie==input$VAR)
    vect_select=c(nom_var1$variable,'CESantenne','SOC_CES_NCes' ,'SOC_DatExam','par_ces', 'clas_age5','clas_age45an','clas_age3','SOC_Sex')
    para_num20<-para_num0 %>% select( which(names(para_num) %in% vect_select)  )
    para_num20
    })


  output$variable<- renderUI({
    para<-para1()
    selectInput("variable", "variable", choices = names(para), selected = names(para)[1])
  })


  output$variable2 <- renderUI({
    para_num<-para_num1()
    all.list_num<-colnames(para_num)
    dic_nom_para_rest<-dic_nom_para %>% filter(variable %in%all.list_num)
    selectInput("variable2", "variable2", choices =dic_nom_para_rest$nom , selected = dic_nom_para_rest$nom[1])
  })

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

  output$variable5a <- renderUI({
    para<-para1()
    para_fac  <- para[ , sapply(para,  is.factor)]
    all.list_num<-colnames(para_fac)
    dic_nom_para_rest<-dic_nom_para %>% filter(variable %in%all.list_num)
    selectInput("variable5a", "variable5a", choices = dic_nom_para_rest$nom , selected = dic_nom_para_rest$nom[1])
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


  output$datatable1  <- DT::renderDataTable({
    para_num<-para_num1()
    ifelse(input$CES %in% levels(para_num$CESantenne),
      para_num_CESfilt <-  para_num%>%filter(CESantenne == input$CES)%>%select(-CESantenne,-SOC_DatExam,-par_ces ) ,
      para_num_CESfilt <-  para_num%>%select(-CESantenne,-SOC_DatExam,-par_ces))
    data_sum <- as.data.frame(t(sapply(para_num_CESfilt, resumer)))
    DT::datatable(
      data_sum , options = list(
        lengthMenu = list(c(5, 30, -1), c('5', '15', 'All')),
        pageLength = 30
      )
    )

   # input$dateRange[1]
  #  input$dateRange[2]
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
    para_num<-para_num1()
    para<-para1()
    var_tmp=dic_nom_para$variable[which(dic_nom_para$nom==input$variable2)]

    data_sum2 <- do.call(rbind, tapply(para_num[,var_tmp], para[,input$variable02], resumer_borne, vect = get(var_tmp, dict_para)))
    DT::datatable(
      data_sum2, options = list(
        paging  = FALSE,
        lengthChange = FALSE
      )
    )
  })

  output$datatable4  <- DT::renderDataTable({
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



output$datatable5  <- DT::renderDataTable({
    para<-para1()
    var_tmp5a=dic_nom_para$variable[which(dic_nom_para$nom==input$variable5a)]
    all<-TDB(para, var_tmp5a,'SOC_Sex', input$variable5b )
    lab<-append(levels(para[,input$variable5b]),c('Ensemble'))
    data_sum2 <- tbl_char(all,lab)
    DT::datatable(
      data_sum2, options = list(
        lengthMenu = list(c(5, 30, -1), c('5', '15', 'All')),
        pageLength = 30
      )
    )
  })

output$plot1 <- renderPlot({
  para<-para1()
  var_tmp6a=dic_nom_para$variable[which(dic_nom_para$nom==input$variable6a)]
  all1<-TDB(para,var_tmp6a,'SOC_Sex', 'clas_age3' )
    p<-graph(all1)
    print(p)
  },height = 700, width = 1200)


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