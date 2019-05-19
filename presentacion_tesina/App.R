library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(haven)


###### 50

empleos1_50=read_dta("est_descr_empleos50.dta", encoding = "utf8")
empleos2_50 <- read_dta("empleos_f_ranking50.dta" , encoding = "utf8")
empleos2_50=transform(empleos2_50, year = as.numeric(year))
empleos2_50$date=as.Date(empleos2_50$date)
head(empleos2_50$date)


empleos_sec_50 <- read_dta("empleos_f_ranking_sector_1_50.dta", encoding = "utf8")
empleos_sec_50$date=as.Date(empleos_sec_50$date)
head(empleos_sec_50$date)
empleos_sec=transform(empleos_sec_50, year = as.numeric(year))
empleos1_50$NOM_MUN <- factor(empleos1_50$NOM_MUN , levels=unique(empleos1_50$NOM_MUN ))
empleos2_50$NOM_MUN <- factor(empleos2_50$NOM_MUN , levels=unique(empleos2_50$NOM_MUN ))
empleos_sec_50$NOM_MUN <- factor(empleos_sec_50$NOM_MUN , levels=unique(empleos_sec_50$NOM_MUN ))

empleos_sec_50$NOM_MUN=gsub(" de ", " \n de ", empleos_sec_50$NOM_MUN)
empleos1_50$NOM_MUN=gsub(" de ", " \n de ", empleos1_50$NOM_MUN)
empleos2_50$NOM_MUN=gsub(" de ", " \n de ", empleos2_50$NOM_MUN)

empleos_sec_50$NOM_MUN=gsub(" el ", " \n el ", empleos_sec_50$NOM_MUN)
empleos1_50$NOM_MUN=gsub(" el ", " \n el ", empleos1_50$NOM_MUN)
empleos2_50$NOM_MUN=gsub(" el ", " \n el ", empleos2_50$NOM_MUN)


empleos_sec_50$NOM_MUN=gsub(" Juan ", " Juan \n", empleos_sec_50$NOM_MUN)
empleos1_50$NOM_MUN=gsub(" Juan ", " Juan \n ", empleos1_50$NOM_MUN)
empleos2_50$NOM_MUN=gsub(" Juan ", " Juan \n  ", empleos2_50$NOM_MUN)


###### 100

empleos1_100=read_dta("est_descr_empleos100.dta", encoding = "utf8")
empleos2_100 <- read_dta("empleos_f_ranking100.dta" , encoding = "utf8")
empleos2_100=transform(empleos2_100, year = as.numeric(year))
empleos2_100$date=as.Date(empleos2_100$date)
head(empleos2_100$date)


empleos_sec_100 <- read_dta("empleos_f_ranking_sector_1_100.dta", encoding = "utf8")
empleos_sec_100$date=as.Date(empleos_sec_100$date)
head(empleos_sec_100$date)
empleos_sec=transform(empleos_sec_100, year = as.numeric(year))
empleos1_100$NOM_MUN <- factor(empleos1_100$NOM_MUN , levels=unique(empleos1_100$NOM_MUN ))
empleos2_100$NOM_MUN <- factor(empleos2_100$NOM_MUN , levels=unique(empleos2_100$NOM_MUN ))
empleos_sec_100$NOM_MUN <- factor(empleos_sec_100$NOM_MUN , levels=unique(empleos_sec_100$NOM_MUN ))

empleos_sec_100$NOM_MUN=gsub(" de ", " \n de ", empleos_sec_100$NOM_MUN)
empleos1_100$NOM_MUN=gsub(" de ", " \n de ", empleos1_100$NOM_MUN)
empleos2_100$NOM_MUN=gsub(" de ", " \n de ", empleos2_100$NOM_MUN)

empleos_sec_100$NOM_MUN=gsub(" el ", " \n el ", empleos_sec_100$NOM_MUN)
empleos1_100$NOM_MUN=gsub(" el ", " \n el ", empleos1_100$NOM_MUN)
empleos2_100$NOM_MUN=gsub(" el ", " \n el ", empleos2_100$NOM_MUN)


empleos_sec_100$NOM_MUN=gsub(" Juan ", " Juan \n", empleos_sec_100$NOM_MUN)
empleos1_100$NOM_MUN=gsub(" Juan ", " Juan \n ", empleos1_100$NOM_MUN)
empleos2_100$NOM_MUN=gsub(" Juan ", " Juan \n  ", empleos2_100$NOM_MUN)

###### 200

empleos1_200=read_dta("est_descr_empleos200.dta", encoding = "utf8")
empleos2_200 <- read_dta("empleos_f_ranking200.dta" , encoding = "utf8")
empleos2_200=transform(empleos2_200, year = as.numeric(year))
empleos2_200$date=as.Date(empleos2_200$date)
head(empleos2_200$date)


empleos_sec_200 <- read_dta("empleos_f_ranking_sector_1_200.dta", encoding = "utf8")
empleos_sec_200$date=as.Date(empleos_sec_200$date)
head(empleos_sec_200$date)
empleos_sec=transform(empleos_sec_200, year = as.numeric(year))
empleos1_200$NOM_MUN <- factor(empleos1_200$NOM_MUN , levels=unique(empleos1_200$NOM_MUN ))
empleos2_200$NOM_MUN <- factor(empleos2_200$NOM_MUN , levels=unique(empleos2_200$NOM_MUN ))
empleos_sec_200$NOM_MUN <- factor(empleos_sec_200$NOM_MUN , levels=unique(empleos_sec_200$NOM_MUN ))

empleos_sec_200$NOM_MUN=gsub(" de ", " \n de ", empleos_sec_200$NOM_MUN)
empleos1_200$NOM_MUN=gsub(" de ", " \n de ", empleos1_200$NOM_MUN)
empleos2_200$NOM_MUN=gsub(" de ", " \n de ", empleos2_200$NOM_MUN)

empleos_sec_200$NOM_MUN=gsub(" el ", " \n el ", empleos_sec_200$NOM_MUN)
empleos1_200$NOM_MUN=gsub(" el ", " \n el ", empleos1_200$NOM_MUN)
empleos2_200$NOM_MUN=gsub(" el ", " \n el ", empleos2_200$NOM_MUN)


empleos_sec_200$NOM_MUN=gsub(" Juan ", " Juan \n", empleos_sec_200$NOM_MUN)
empleos1_200$NOM_MUN=gsub(" Juan ", " Juan \n ", empleos1_200$NOM_MUN)
empleos2_200$NOM_MUN=gsub(" Juan ", " Juan \n  ", empleos2_200$NOM_MUN)


## cambio sectorial
c_sec50=read_dta("cambio_sectorial_50.dta", encoding = "utf8")
c_sec100=read_dta("cambio_sectorial_100.dta", encoding = "utf8")
c_sec200=read_dta("cambio_sectorial_200.dta", encoding = "utf8")
# temps####
# App ####

shinyApp(
  shinyUI(
    navbarPage(title= strong("Crecimiento en M\u00E9xico", style= "color:Black"),
               tabPanel("", fluidRow(
                 column(width = 6),
                 column(width = 6)
               ), icon= icon("home")),
               
               tabPanel("Crecimiento y Salario", fluidPage( sidebarPanel("Tasas promedio de 2010 a 2018. Podemos interpretar los 
                                                                         resultados como n\u00FAmero de veces que creci\u00F3 el empleo con respecto en promedio durante 2010 a 2018.", pre = "", 
                                                                         radioButtons("pop1", label = h3("Poblaci\u00F3n municipal  (en miles)"), 
                                                                                            choices = list("50" = 1, "100" = 2, "200" = 3), selected = 1)),
                                            mainPanel(plotlyOutput("T_emp_sal")))),
               #tabPanel("Mapas", fluidPage( sidebarPanel("Crecimiento del empleo respecto a 2010", pre = "", 
               #                                          radioButtons("pop0", label = h3("Poblaci\u00F3n municipal  (en miles)"), 
              #                                                        choices = list("50" = 1, "100" = 2, "200" = 3), selected = 1)),
                #                            mainPanel(plotOutput("Mapa_empleo")))),   
               tabPanel("Primeros Municipios", 
                        tabsetPanel(
                          tabPanel("Empleo", 
                                   fluidPage(
                                     sidebarPanel("Nivel de empleo",   radioButtons("F501", label = h3("Primeros municipios en salario y empleo"), 
                                                                       choices = list("Primeros 50" = 1, "Primeros 20" = 2, "Todos" = 3),selected = 2),  
                                                  
                                                  radioButtons("pop2", label = h3("Poblaci\u00F3n municipal  (en miles)"), 
                                                                     choices = list("50" = 1, "100" = 2, "200" = 3), selected = 1),
                                                  
                                                  
                                                                      sliderInput("year1Input", "Fecha", 2000, 2018, c(2010, 2018), pre = "")), 
                                     mainPanel(tags$h3("Porcentaje empleo respecto al 2010"), 
                                               #plotlyOutput("Niv_empleo"),  
                                               plotlyOutput("Niv_empleo_Fa")
                                                                                              ))),
                        
                          
                        tabPanel("Salario", 
                        fluidPage(sidebarPanel("Salario real (sep 2018=100)",   
                        radioButtons("F5011", label = h3("Primeros municipios en salario y empleo"), 
                         choices = list("Primeros 50" = 1, "Primeros 20" = 2, "Todos" = 3), selected = 2),
                        radioButtons("pop21", label = h3("Poblaci\u00F3n municipal  (en miles)"), 
                                     choices = list("50" = 1, "100" = 2, "200" = 3), selected = 1),  
                        sliderInput("year2Input", "Fecha", 2000, 2018, c(2010, 2018), pre = "")), 
                        mainPanel(tags$h3("Porcentaje salario respecto al 2010")
                                  #,plotlyOutput("Niv_sal")
                                  , plotlyOutput("Niv_sal_Fa")
                                  ))
                                            ))), 
               tabPanel("\u00DAltimos Municipios", 
                        tabsetPanel(tabPanel("Empleo", 
                                              fluidPage(sidebarPanel("Nivel de empleo",   
                                                        radioButtons("L501", label = h3("\u00DAltimos municipios en salario y empleo"),
                                                                          choices = list("\u00DAltimos 50" = 1, "\u00DAltimos 20" = 2, "Todos" = 3),selected = 2), 
                                                        radioButtons("pop3", label = h3("Poblaci\u00F3n municipal  (en miles)"), 
                                                                     choices = list("50" = 1, "100" = 2, "200" = 3), selected = 1),
                                                        sliderInput("year3Input", "Fecha", 2000, 2018, c(2010, 2018), pre = "")), 
                                              mainPanel(tags$h3("Porcentaje empleo respecto al 2010"),
                                                        #plotlyOutput("Niv_empleoL"), 
                                                        plotlyOutput("Niv_empleo_FaL")
                                                                                                              ))), 
                        tabPanel("Salario", fluidPage(sidebarPanel("Salario real (sep 2018=100)",   radioButtons("L5012", label = h3("\u00DAltimos municipios en salario y empleo"), 
                                                                                                                                                               choices = list("\u00DAltimos 50" = 1, "\u00DAltimos 20" = 2, "Todos" = 3),
                                                                                                                                                               selected = 2),  
                                                                   
                                                                   radioButtons("pop32", label = h3("Poblaci\u00F3n municipal  (en miles)"), 
                                                                                choices = list("50" = 1, "100" = 2, "200" = 3), selected = 1),
                                                                   sliderInput("year4Input", "Fecha", 2000, 2018, c(2010, 2018), pre = "")), 
                                                                                              mainPanel(#plotlyOutput("Niv_sal_FaL")
                                                                                                        #,  
                                                                                                plotlyOutput("Niv_sal_FaL_niv")
                                                                                                        )
                                                                )))),
               
               tabPanel("Sector Primeros ", tabsetPanel(
                 
                 tabPanel("Empleo", 
                          fluidPage(
                            sidebarPanel("Nivel de empleo",  uiOutput("sectorOutput"), radioButtons("F501_sec", label = h3("Primeros municipios en salario y empleo"), 
                                                                                                                               choices = list("Primeros 50" = 1, "Primeros 20" = 2, "Todos" = 3),
                                                                                                                               selected = 2) , 
                                         radioButtons("pop4", label = h3("Poblaci\u00F3n municipal  (en miles)"), 
                                                      choices = list("50" = 1, "100" = 2, "200" = 3), selected = 1),
                                         sliderInput("year5Input", "Fecha", 2000, 2018, c(2000, 2018), pre = "")), 
                            mainPanel( tags$h3("Porcentaje empleo respecto al 2010"), plotlyOutput("niv_empleo_sec")
                                     
                                      )
                            ))    ,
                 tabPanel("Salario", 
                          fluidPage(
                            sidebarPanel("Salario real (sep 2018=100)", uiOutput("sector15Output"),
                                         sliderInput("year15Input", "Fecha", 2000, 2018, c(2000, 2018), pre = ""),
                                         radioButtons("F5012_sec", label = h3("Primeros municipios en salario y empleo"), 
                                                      choices = list("Primeros 50" = 1, "Primeros 20" = 2, "Todos" = 3),
                                                      selected = 2) , 
                                         radioButtons("pop42", label = h3("Poblaci\u00F3n municipal  (en miles)"), 
                                                      choices = list("50" = 1, "100" = 2, "200" = 3), selected = 1)), 
                            mainPanel(   plotlyOutput("Niv_salF_sec")
                                         
                                         )))
      
                 
                 )),
                                                                  
               
               tabPanel("Sector \u00DAltimos", tabsetPanel(tabPanel("Empleo", fluidPage(sidebarPanel("Nivel de empleo",  uiOutput("sector2Output"), checkboxGroupInput("L501_sec", label = h3("\u00DAltimos municipios en salario y empleo"), 
                                                                                                                                                          choices = list("\u00DAltimos 50" = 1, "\u00DAltimos 20" = 2, "Todos" = 3),
                                                                                                                                                          selected = 2) ,  
                                                                                                     
                                                                                                     radioButtons("pop5", label = h3("Poblaci\u00F3n municipal  (en miles)"), 
                                                                                                                  choices = list("50" = 1, "100" = 2, "200" = 3), selected = 1), 
                                                                                                     
                                                                                                     sliderInput("year6Input", "Fecha", 2000, 2018, c(2010, 2018), pre = "")), 
                                                                            mainPanel(plotlyOutput("niv_empleo_sec2")
                                                                                      
                                                                                      ))),
                                                           tabPanel("Salario", 
                                                                                        fluidPage(
                                                                                          sidebarPanel("Salario real (sep 2018=100)", uiOutput("sector20Output"),
                                                                                                       sliderInput("year20Input", "Fecha", 2000, 2018, c(2000, 2018), pre = ""),
                                                                                                       radioButtons("L5020_sec", label = h3("Primeros municipios en salario y empleo"), 
                                                                                                                    choices = list("\u00DAltimos 50" = 1, "\u00DAltimos 20" = 2, "Todos" = 3),
                                                                                                                    selected = 2) , 
                                                                                                       radioButtons("pop20", label = h3("Poblaci\u00F3n municipal  (en miles)"), 
                                                                                                                    choices = list("50" = 1, "100" = 2, "200" = 3), selected = 1)), 
                                                                                          mainPanel(   plotlyOutput("Niv_sal_sec_T20")
                                                                                                      
                                                                                                       )))
                                                                       
                                                                       
                                                           )),
               tabPanel("Proporci\u00F3n", tabsetPanel(tabPanel("Proporci\u00F3n", fluidPage(sidebarPanel("Proporci\u00F3n de trabajadores en el sector econ\u00F3mico respecto al total de trabajadores (anual)",
                                                                                                          radioButtons("primeros", label = h3("Crecimiento"), 
                                                                                                                       choices = list("Primeros 20" = 1, "\u00DAltimos 20" = 2), selected = 1),
                                                                                                          radioButtons("pop7", label = h3("Poblaci\u00F3n municipal  (en miles)"), 
                                                                                                                       choices = list("50" = 1, "100" = 2, "200" = 3), selected = 1), 
                                                                                                          sliderInput("year7Input", "Fecha", 2010, 2018, c(2010, 2018), pre = "")), 
                                                                                             mainPanel(plotlyOutput("share_sector")
                                                                                                       
                                                                                             )))
                                                       
                                                       
               ))
               
              
               
               
    )),
  shinyServer(function(input, output, session) {

    #slide1 -----------------------------------------------------------------
    
    output$value <- renderPrint({ input$pop1 })

    empleos_pop<- reactive({ 
      
      if (input$pop1==1){
      x<-empleos1_50 
      }
      if (input$pop1==2){
        x<-  empleos1_100
      }
      if (input$pop1==3){
        x<-  empleos1_200
      }
      return(x)
    })
    
   # observe({
   #   print(empleos_pop()) #this helps to debugging
  #  })
    
    output$T_emp_sal <- renderPlotly({
      
      a=ggplot(empleos_pop(), aes(prom_cta, prom_cing,  color=(CVE_ENT))) +
        geom_point(aes(text=sprintf("Estado: %s<br>Municipio: %s <br> salario 2010: %s <br> trabajadores 2010: %s ", NOM_ENT, NOM_MUN, round(prom_cing, digits = 3), round(prom_cta, digits = 3) ))) +
        ylab("Salario") +
        xlab("Trabajadores") +
        ggtitle("Tasa relativa de crecimiento 2018/2010") +
        geom_abline(slope=0, intercept=1)+
        geom_vline(xintercept = 1) +
        theme(plot.title = element_text(hjust = 0.5), legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
      
      ggplotly(a, tooltip="text")
    })
    
    #slide2 -----------------------------------------------------------------------
    output$value <- renderPrint({ input$F501 })
    output$value <- renderPrint({ input$pop2 })
    
    empleos_pop2<- reactive({ 
      
      if (input$pop2==1){
        x<-empleos2_50 
      }
      if (input$pop2==2){
        x<-  empleos2_100
      }
      if (input$pop2==3){
        x<-  empleos2_200
      }

      return(x)
    })
    
    
    tempo<- reactive({ 
      empleos_pop2() %>%
        filter(year >= input$year1Input[1],
               year <= input$year1Input[2])  %>%
               {if (input$F501==1) filter(.,first50==1) else 
                 if (input$F501==2) filter(.,first20==1) else filter(.,first20>=0)}
        })

    output$Niv_empleo <- renderPlotly({
    p <- ggplot(data = tempo(), aes(x = date, y = cta))+
      geom_line(aes(color=NOM_ENT, text=sprintf("Municipio: %s", NOM_MUN))) +
     # facet_wrap(~NOM_MUN)+
      scale_x_date() +
      ylab("% Trabajadores respecto a 2010") +
      xlab("") +
      theme(plot.title = element_text(hjust = 0.5), legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    ggplotly(p)
    })
    
    tempo2<- reactive({ 
      empleos_pop2() %>%
        filter(year >= input$year1Input[1],
               year <= input$year1Input[2]) %>%
               {if (input$F501==1) filter(.,first50==1) else 
                 if (input$F501==2) filter(.,first20==1) else filter(.,first20>=0)}
    })

    output$Niv_empleo_Fa <- renderPlotly({
      p <- ggplot(data = tempo2(), aes(x = date, y = cta))+
        geom_line(aes( text=sprintf("Estado: %s", NOM_ENT))) +
        facet_wrap(~NOM_MUN)+
        scale_x_date() +
        ylab("% Trabajadores respecto a 2010") +
        xlab("") +
        theme(panel.spacing.x=unit(0.5, "pt"), panel.spacing.y=unit(8, "pt"), strip.text = element_text(size=9 , face="bold"),
              strip.background = element_rect(colour=NA, fill=NA),
              legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle = 90, hjust = 1))
      
      ggplotly(p)
    })
    

    
    
    
    #slide3 ----------------------------------------------------------------------------
    
    output$value <- renderPrint({ input$F5011 })
    output$value <- renderPrint({ input$pop21})
    
    empleos_pop21<- reactive({ 
      
      if (input$pop21==1){
        x<-empleos2_50 
      }
      if (input$pop21==2){
        x<-  empleos2_100
      }
      if (input$pop21==3){
        x<-  empleos2_200
      }

      return(x)
    })
    
    
    tempo3<- reactive({ 
      empleos_pop21() %>%
        filter(year >= input$year2Input[1],
               year <= input$year2Input[2])  %>%
               {if (input$F5011==1) filter(.,first50==1) else 
                 if (input$F5011==2) filter(.,first20==1) else filter(.,first20>=0)}
    })

    output$Niv_sal <- renderPlotly({
      p <- ggplot(data = tempo3(), aes(x = date, y = cing))+
        geom_line(aes(color=NOM_MUN, text=sprintf("Estado: %s", NOM_ENT))) +
        # facet_wrap(~NOM_MUN)+
        scale_x_date() +
        ylab("Ingreso mensual") +
        xlab("") +
        theme(plot.title = element_text(hjust = 0.5), legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
      
      ggplotly(p)
    })
    tempo4<- reactive({ 
      empleos_pop21() %>%
        filter(year >= input$year2Input[1],
               year <= input$year2Input[2])%>%
        {if (input$F5011==1) filter(.,first50==1) else 
          if (input$F5011==2) filter(.,first20==1) else filter(.,first20>=0)}
    })


    output$Niv_sal_Fa <- renderPlotly({
      p <- ggplot(data = tempo4(), aes(x = date, y = cing))+
        geom_line(aes( text=sprintf("Estado: %s", NOM_ENT))) +
        facet_wrap(~NOM_MUN)+
        scale_x_date() +
        ylab("Ingreso promedio mensual respecto a 2010") +
        xlab("") +
        theme(panel.spacing.x=unit(0.5, "pt"), panel.spacing.y=unit(8, "pt"), strip.text = element_text(size=9 , face="bold"),
              strip.background = element_rect(colour=NA, fill=NA),
              legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle = 90, hjust = 1))
      
      ggplotly(p)
    })
    #slide4 -----------------------------------------------------------------------------
    output$value <- renderPrint({ input$L501 })
    output$value <- renderPrint({ input$pop3 })
    
    empleos_pop3<- reactive({ 
      
      if (input$pop3==1){
        x<-empleos2_50 
      }
      if (input$pop3==2){
        x<-  empleos2_100
      }
      if (input$pop3==3){
        x<-  empleos2_200
      }

      return(x)
    })
    
    tempo5<- reactive({ 
      empleos_pop3() %>%
        filter(year >= input$year3Input[1],
               year <= input$year3Input[2])  %>%
               {if (input$L501==1) filter(.,last50==1) else 
                 if (input$L501==2) filter(.,last20==1) else filter(.,last20>=0)}
    })
    output$Niv_empleoL <- renderPlotly({
      p <- ggplot(data = tempo5(), aes(x = date, y = cta))+
        geom_line(aes(color=NOM_MUN, text=sprintf("Estado: %s", NOM_ENT))) +
        # facet_wrap(~NOM_MUN)+
        scale_x_date() +
        ylab("% Trabajadores respecto al 2010") +
        xlab("") +
        theme(plot.title = element_text(hjust = 0.5), legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
      
      ggplotly(p)
    })
    
    tempo6<- reactive({ 
      empleos_pop3() %>%
        filter(year >= input$year3Input[1],
               year <= input$year3Input[2])%>%
        filter(last20==1)
    })

    output$Niv_empleo_FaL <- renderPlotly({
      p <- ggplot(data = tempo6(), aes(x = date, y = cta))+
        geom_line(aes( text=sprintf("Estado: %s", NOM_ENT))) +
        facet_wrap(~NOM_MUN)+
        scale_x_date() +
        ylab("% Trabajadores respecto al 2010") +
        xlab("") +
        theme(panel.spacing.x=unit(0.5, "pt"), panel.spacing.y=unit(8, "pt"), strip.text = element_text(size=9 , face="bold"),
              strip.background = element_rect(colour=NA, fill=NA),
              legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle = 90, hjust = 1))
      
      ggplotly(p)
    })
    

    
    #slide5 ----------------------------------------------------------------------
    output$value <- renderPrint({ input$L5012 })
    output$value <- renderPrint({ input$pop32 })
    
    empleos_pop32<- reactive({ 
      
      if (input$pop32==1){
        x<-empleos2_50 
      }
      if (input$pop32==2){
        x<-  empleos2_100
      }
      if (input$pop32==3){
        x<-  empleos2_200
      }

      return(x)
    })
    
    
    tempo7<- reactive({ 
      empleos_pop32()%>%
        filter(year >= input$year4Input[1],
               year <= input$year4Input[2])  %>%
               {if (input$L5012==1) filter(.,last50==1) else 
                 if (input$L5012==2) filter(.,last20==1) else filter(.,last20>=0)}
    })
    output$Niv_sal_FaL <- renderPlotly({
      p <- ggplot(data = tempo7(), aes(x = date, y = cing))+
        geom_line(aes(color=NOM_MUN, text=sprintf("Estado: %s", NOM_ENT))) +
        # facet_wrap(~NOM_MUN)+
        scale_x_date() +
        ylab("Ingreso mensual") +
        xlab("") +
        theme(plot.title = element_text(hjust = 0.5), legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
      
      ggplotly(p)
    })
    tempo8<- reactive({ 
      empleos_pop32() %>%
        filter(year >= input$year4Input[1],
               year <= input$year4Input[2])%>%
        filter(last20==1)
    })
    
    output$Niv_sal_FaL_niv <- renderPlotly({
      p <- ggplot(data = tempo8(), aes(x = date, y = cing))+
        geom_line(aes( text=sprintf("Estado: %s", NOM_ENT))) +
        facet_wrap(~NOM_MUN)+
        scale_x_date() +
        ylab("Ingreso promedio mensual respecto a 2010") +
        xlab("") +
        theme(panel.spacing.x=unit(0.5, "pt"), panel.spacing.y=unit(8, "pt"), strip.text = element_text(size=9 , face="bold"),
              strip.background = element_rect(colour=NA, fill=NA),
              legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle = 90, hjust = 1))
      
      ggplotly(p)
    })
    
    #slide 6 -----------------------------------------------------------------------
    output$value <- renderPrint({ input$F501_sec})
    output$value <- renderPrint({ input$pop4 })
    
    empleos_pop4<- reactive({ 
      
      if (input$pop4==1){
        x<-empleos_sec_50 
      }
      if (input$pop4==2){
        x<-  empleos_sec_100
      }
      if (input$pop4==3){
        x<-  empleos_sec_200
      }

      return(x)
    })
    
    output$sectorOutput <- renderUI({
      selectInput("sectorInput", "Sector",
                  sort(unique(empleos_pop4()$sector_economico_1)),
                  selected = "Comercio")
    })
    
    const<- reactive({ 
      if (is.null(input$sectorInput)) {
        return(NULL)
      }    
      empleos_pop4() %>%
        filter( 
                year >= input$year5Input[1],
               year <= input$year5Input[2],
              sector_economico_1==input$sectorInput)  %>%
              {if (input$F501_sec==1) filter(.,first50==1) else 
                if (input$F501_sec==2) filter(.,first20==1) else filter(.,first20>=0)} })
    
    output$niv_empleo_sec <- renderPlotly({
    p <- ggplot(data = const(), aes(x = date, y = cta) , pch = NA)+
      geom_line(aes( text=sprintf("Estado: %s", NOM_ENT))) +
      facet_wrap(~NOM_MUN)+
      scale_x_date() +
      ylab("% Trabajadores respecto al 2010") +
      xlab("") +
      theme(panel.spacing.x=unit(0.5, "pt"), panel.spacing.y=unit(8, "pt"), strip.text = element_text(size=9 , face="bold"),
            strip.background = element_rect(colour=NA, fill=NA),
            legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(p)   })
    

   # mainPanel(plotlyOutput("Niv_salF_sec"),
    #          plotlyOutput("Niv_sal_FaL_sec")))
    
    # slide 6.5 --------------------------------------------------------------------
    output$value <- renderPrint({ input$F5012_sec})
    output$value <- renderPrint({ input$pop42 })
    
    empleos_pop42<- reactive({ 
      
      if (input$pop42==1){
        x<-empleos_sec_50 
      }
      if (input$pop42==2){
        x<-  empleos_sec_100
      }
      if (input$pop42==3){
        x<-  empleos_sec_200
      }

      return(x)
    })
    output$sector15Output <- renderUI({
      selectInput("sector15Input", "Sector",
                  sort(unique(empleos_sec$sector_economico_1)),
                  selected = "Comercio")
    })
    const15<- reactive({ 
      if (is.null(input$sector15Input)) {
        return(NULL)
      }    
      empleos_pop42() %>%
        filter( 
          year >= input$year5Input[1],
          year <= input$year5Input[2],
          sector_economico_1==input$sector15Input)  %>%
          {if (input$F5012_sec==1) filter(.,first50==1) else 
            if (input$F5012_sec==2) filter(.,first20==1) else filter(.,first20>=0)} })
    
    output$Niv_salF_sec <- renderPlotly({
      p <- ggplot(data = const15(), aes(x = date, y = cing) , pch = NA)+
        geom_line(aes( text=sprintf("Estado: %s", NOM_ENT))) +
        facet_wrap(~NOM_MUN)+
        scale_x_date() +
        ylab("% Ingreso respecto al 2010") +
        xlab("") +
        theme(panel.spacing.x=unit(0.5, "pt"), panel.spacing.y=unit(8, "pt"), strip.text = element_text(size=9 , face="bold"),
              strip.background = element_rect(colour=NA, fill=NA),
              legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle = 90, hjust = 1))
      
      ggplotly(p)   })

    # mainPanel(plotlyOutput("Niv_salF_sec"),
    #slide 7 -----------------------------------------------------------------------
    output$value <- renderPrint({ input$L501_sec})
    output$value <- renderPrint({ input$pop5 })
    
    empleos_pop5<- reactive({ 
      
      if (input$pop5==1){
        x<-empleos_sec_50 
      }
      if (input$pop5==2){
        x<-  empleos_sec_100
      }
      if (input$pop5==3){
        x<-  empleos_sec_200
      }

      return(x)
    })
    output$sector2Output <- renderUI({
      selectInput("sector2Input", "Sector",
                  sort(unique(empleos_pop5()$sector_economico_1)),
                  selected = "Comercio")
    })
    
    const2<- reactive({ 
      if (is.null(input$sector2Input)) {
        return(NULL)
      }    
      empleos_pop5() %>%
        filter( 
          year >= input$year6Input[1],
          year <= input$year6Input[2],
          sector_economico_1==input$sector2Input)  %>%
          {if (input$L501_sec==1) filter(.,last50==1) else 
            if (input$L501_sec==2) filter(.,last20==1) else filter(.,last20>=0)} })
    
    output$niv_empleo_sec2 <- renderPlotly({
      p <- ggplot(data = const2(), aes(x = date, y = cta))+
        geom_line(aes( text=sprintf("Estado: %s", NOM_ENT))) +
        facet_wrap(~NOM_MUN)+
        scale_x_date() +
        ylab("Trabajadores") +
        xlab("") +
        expand_limits(y=0)+
        theme(panel.spacing.x=unit(0.5, "pt"), panel.spacing.y=unit(8, "pt"), strip.text = element_text(size=9 , face="bold"),
              strip.background = element_rect(colour=NA, fill=NA),
         legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle = 90, hjust = 1))
      
      ggplotly(p)   })
    
    


  #slide 8 ########################
  output$value <- renderPrint({ input$L5020_sec})
  output$value <- renderPrint({ input$pop20 })
  
  empleos_pop20<- reactive({ 
    
    if (input$pop20==1){
      x<-empleos_sec_50 
    }
    if (input$pop20==2){
      x<-  empleos_sec_100
    }
    if (input$pop20==3){
      x<-  empleos_sec_200
    }

    return(x)
  })
  output$sector20Output <- renderUI({
    selectInput("sector20Input", "Sector",
                sort(unique(empleos_sec$sector_economico_1)),
                selected = "Comercio")
  })
  const201<- reactive({ 
    if (is.null(input$sector20Input)) {
      return(NULL)
    }    
    empleos_pop20() %>%
      filter( 
        year >= input$year20Input[1],
        year <= input$year20Input[2],
        sector_economico_1==input$sector20Input)  %>%
        {if (input$L5020_sec==1) filter(.,last50==1) else 
          if (input$L5020_sec==2) filter(.,last20==1) else filter(.,last20>=0)} })
  
  output$Niv_sal_sec_T20<- renderPlotly({
    p <- ggplot(data = const201(), aes(x = date, y = cing) , pch = NA)+
      geom_line(aes( text=sprintf("Estado: %s", NOM_ENT))) +
      facet_wrap(~NOM_MUN)+
      scale_x_date() +
      ylab("% Ingreso respecto al 2010") +
      xlab("") +
      theme(panel.spacing.x=unit(0.5, "pt"), panel.spacing.y=unit(8, "pt"), strip.text = element_text(size=9 , face="bold"),
            strip.background = element_rect(colour=NA, fill=NA),
            legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(p)   })
  

  
  
  
# slide 9 -------------------------------------------------
  output$value <- renderPrint({ input$primeros})
  output$value <- renderPrint({ input$pop7 })
  output$value <- renderPrint({ input$year7Input})
  empleos_pop7<- reactive({ 
    
    if (input$pop7==1 & input$primeros==1){
      x<-c_sec50 %>%
       filter( first20==1)
    }
    if (input$pop7==1 & input$primeros==2){
      x<-c_sec50 %>%
        filter( last20==1)
    }
    if (input$pop7==2 & input$primeros==1){
      x<-  c_sec100 %>%
        filter(  first20==1)
    }
    if (input$pop7==2 & input$primeros==2){
      x<-c_sec100 %>%
        filter(  last20==1)
    }
    if (input$pop7==3 & input$primeros==1){
      x<-  c_sec200 %>%
        filter( first20==1)
    }
    if (input$pop7==3 & input$primeros==2){
      x<-c_sec200 %>%
        filter(  last20==1)
    }
    return(x)
  })

  const7<- reactive({ 
    empleos_pop7() %>%
      filter( 
        id >= input$year7Input[1],
        id <= input$year7Input[2]) })
  
  output$share_sector<- renderPlotly({
   x<- ggplot( const7(), aes(color=sector_economico_1, y=share, x=id)) + 
      geom_line()+
      facet_wrap(~NOM_MUN)+
      theme(panel.spacing.x=unit(0.5, "pt"), panel.spacing.y=unit(8, "pt"), strip.text = element_text(size=9 , face="bold"),
            strip.background = element_rect(colour=NA, fill=NA),
            legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_blank())
    
    ggplotly(x)  })
  
  })
  )
  
  
  
  

