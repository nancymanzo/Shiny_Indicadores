library(shinydashboard)
library(shiny)
library(plotly)
library(dashboardthemes)
library(shinythemes)
library(shinybusy)

library(readxl)
library(dplyr)
library(DT)
library(scales)
library(ggplot2)
library(tidyverse)



indicador_6<-read.csv("indicador_6.csv")

indicador_6 %>%
  mutate(MES=factor(MES,
                    levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                             "Septiembre", "Octubre","Noviembre", "Diciembre")),
         FECHA= factor(FECHA, levels=c(2019, 2020, 2021)))->indicador_6


indicador_6 %>%
  group_by(FECHA, MES) %>% 
  summarise(`MUJERES VICTIMAS DE VIOLENCIA DE GENERO`=sum(TOTAL.DE.MUJERES.VICTIMAS.DE.VIOLENCIA.DE.GENERO.ATENDIDAS),
            `ORDENES DE PROTECCION`=sum(TOTAL.DE.MUJERES.CANALIZADAS.PARA.OTORGAMIENTO.DE.ORDENES.DE.PROTECCION),
            `MEDIDAS DE PROTECCION`=sum(TOTAL.DE.MUJERES.CANALIZADAS.PARA.OTORGAMIENTO.DE.MEDIDAS.DE.PROTECCION),
            `LÍNEA BASE`=scales::percent(sum((`ORDENES DE PROTECCION`+`MEDIDAS DE PROTECCION`)/`MUJERES VICTIMAS DE VIOLENCIA DE GENERO`), 0.1)) -> tabla_6

tabla_6 %>% 
  pivot_longer(names_to = "TIPO",
               values_to = "TOTAL",
               cols=c("MUJERES VICTIMAS DE VIOLENCIA DE GENERO",
                      "ORDENES DE PROTECCION",
                      "MEDIDAS DE PROTECCION"))-> t6


ggplot(t6) +
  aes(x = MES, y = TOTAL, colour = TIPO, group=TIPO) +
  geom_line(size = 1.5) + geom_point(size = 3)+
  labs(x="", y="", title = "Indicador 6",
       color = "TIPO") +
  theme_minimal()+   
  facet_wrap(vars(FECHA))+
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(
    values = c(
      `MUJERES VICTIMAS DE VIOLENCIA DE GENERO` = "#D98CBC",
      `ORDENES DE PROTECCION` = "#C91682",
      `MEDIDAS DE PROTECCION` = "#7E3794"))+
  theme(legend.position = "bottom")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))->gr6



indicador_7<-read.csv("indicador_7.csv")


indicador_7 %>%
  mutate(MES=factor(MES,
                    levels=c("Enero", "Febrero", "Marzo","Abril", "Mayo", "Junio","Julio", "Agosto",
                             "Septiembre", "Octubre","Noviembre", "Diciembre")))->indicador_7

indicador_7 %>% 
  group_by(FECHA, MES) %>% 
  summarise(`MUJERES VICTIMAS DE VIOLENCIA DE GENERO`=sum(TOTAL.DE.MUJERES.VICTIMAS.DE.VIOLENCIA.DE.GENERO.ATENDIDAS),
            `TOTAL DE MUJERES VICTIMAS DE VIOLENCIA DE GENERO QUE SOLICITARON UNA MEDIDA DE PROTECCION SIN TENER UNA CANALIZACION FORMAL`=sum(TOTAL.DE.MUJERES.VICTIMAS.DE.VIOLENCIA.DE.GENERO.QUE.SOLICITARON.UNA.MEDIDA.DE.PROTECCION.SIN.TENER.UNA.CANALIZACION.FORMAL),
            `TOTAL DE MUJERES VICTIMAS DE VIOLENCIA DE GENERO QUE SOLICITARON UNA ORDEN DE PROTECCION SIN TENER UNA CANALIZACION FORMAL`=sum(TOTAL.DE.MUJERES.VICTIMAS.DE.VIOLENCIA.DE.GENERO.QUE.SOLICITARON.UNA.ORDEN.DE.PROTECCION.SIN.TENER.UNA.CANALIZACION.FORMAL),
            `LINEA BASE`=scales::percent(sum(`TOTAL DE MUJERES VICTIMAS DE VIOLENCIA DE GENERO QUE SOLICITARON UNA MEDIDA DE PROTECCION SIN TENER UNA CANALIZACION FORMAL`/`MUJERES VICTIMAS DE VIOLENCIA DE GENERO`), 0.1)) -> tabla_7


tabla_7 %>% 
  pivot_longer(names_to = "TIPO",
               values_to = "TOTAL",
               cols=c("MUJERES VICTIMAS DE VIOLENCIA DE GENERO",
                      "TOTAL DE MUJERES VICTIMAS DE VIOLENCIA DE GENERO QUE SOLICITARON UNA MEDIDA DE PROTECCION SIN TENER UNA CANALIZACION FORMAL",
                      "TOTAL DE MUJERES VICTIMAS DE VIOLENCIA DE GENERO QUE SOLICITARON UNA ORDEN DE PROTECCION SIN TENER UNA CANALIZACION FORMAL"))-> t7

t7 %>% 
  ggplot() +
  aes(x = MES, y = TOTAL, colour = TIPO, group=TIPO) +
  geom_line(size = 1.5) + geom_point(size = 3)+
  labs(x="", y="", title = "Indicador 7",
       color = "TIPO") +
  theme_minimal()+   
  facet_wrap(vars(FECHA))+
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(
    values = c(
      `MUJERES VICTIMAS DE VIOLENCIA DE GENERO` = "#D98CBC",
      `TOTAL DE MUJERES VICTIMAS DE VIOLENCIA DE GENERO QUE SOLICITARON UNA MEDIDA DE PROTECCION SIN TENER UNA CANALIZACION FORMAL` = "#C91682",
      `TOTAL DE MUJERES VICTIMAS DE VIOLENCIA DE GENERO QUE SOLICITARON UNA ORDEN DE PROTECCION SIN TENER UNA CANALIZACION FORMAL` = "#7E3794"))+
  theme(legend.position = "bottom")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) -> gr7







ui <- dashboardPage(
  title = "Indicadores de la AVGM de Jalisco",
  skin = "purple",
  
  dashboardHeader(title = tags$p("Indicadores AVGM de Jalisco",
                                 style = "color = #fffff; font-size: 2rem"),
                  tags$li(class = "dropdown",
                          tags$style(".main-header {max-height: 60px}"),
                          tags$style(".main-header .logo {height: 60px;}"),
                          tags$style(".sidebar-toggle {height: 60px; padding-top: 20px !important;}"),
                          tags$style(".navbar {min-height:60px !important}"),
                          tags$style(".dropdown {height: 60px; padding-top: 10px !important;}") 
                  ), titleWidth = "25%"),
  
  
  dashboardSidebar(
    tags$style(".left-side, .main-sidebar {padding-top: 103px}"),
    sidebarMenu(
      menuItem("Inicio",tabName = "inicio", icon = icon("home")),
      menuItem("Objetivo 1", tabName ="objetivo1", icon = icon("th-list")),
      
      menuItem("Objetivo 2", tabName ="objetivo2", icon = icon("th-list")),
               
                 # menuSubItem("Ind6", 
                 #             tabName = "Ind6", icon("circle")),
                 # 
                 # menuSubItem("Ind7", 
                 #             tabName = "Ind7", icon("circle"))),
        
      menuItem("Objetivo 3", tabName ="objetivo3", icon = icon("th-list")),
      menuItem("Objetivo 4", tabName ="objetivo4", icon = icon("th-list")),
      menuItem("Objetivo 5", tabName ="objetivo5", icon = icon("th-list")),
      menuItem("Objetivo 6", tabName ="objetivo6", icon = icon("th-list")),
      menuItem("Objetivos generales", tabName ="objetivosgenerales", icon = icon("chart-line"))
      
      
    )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName =  "inicio",
              fluidRow(width = 6,
                       box(
                         title = "INDICADORES DE LA AVGM",
                         status = "danger",
                         solidHeader = TRUE,
                         
                         tags$h2("¿De qué va esto?"),
                         tags$p("Información a añadir"),
                         tags$hr(),
                         
                       ),
                       box(
                         width = 6, 
                         height = "100%",
                         id = "table",
                         title = "Ubicación de los postes")
              )),
      
      
      tabItem(tabName = "objetivo2",
              tags$style(".info-box-content p { font-size: 2.5rem; }"),
              
              fluidRow(width=10, 
                       h2(align="center","Indicador 6:", style="color:black"), 
                       h4(align="center", "Porcentaje de mujeres víctimas de violencia por razones de género atendidas y canalizadas para otorgamiento de orden de protección y/o medidas de protección.", style="color:gray"),
                       box(
                         width=12,  
                         valueBox("Indicador 2021", "60.5%",icon=icon("chart-area"),color="fuchsia"),
                         valueBox("Indicador 2020", "55.3%", icon=icon("arrow-circle-down"), color="purple"),
                         valueBox("Indicador 2019", "61.0%", icon=icon("arrow-circle-up"), color="maroon")),
                
                       
             box(width = 6, 
                 # height = "100%",
                 # id = "table",
                 # title = "Calculo del indicador mensual (Histórico).",
                 dataTableOutput(outputId = "table")),

              
              tabBox(
                width = 6,
                height = "100%",
                tabPanel(title = "Gráfica", plotlyOutput("gr6")),
                tabPanel(title = "Mapa")), 
             
             hr(), 
             hr(),
             hr(),
             hr(),
             hr(),
             hr(),            
             hr(), 
             hr(), 
             hr(),
             hr(),
             hr(),
             hr(),
             hr(),
             hr(),
             hr(),
             hr(), 
             hr(), 
             hr(),
             hr(),
             hr(),
             hr(),
             hr(),
             hr(),
             hr(), 
             hr(), 
             hr(),
             hr(),
             hr(),
             hr(),
             hr(),
             hr(),
             
             
             h4(""),
          tabItem(tabName =  "objetivo2",
             fluidRow(width=10, 
                      h2(align="center","Indicador 7:", style="color:black"), 
                      h4(align="center", "Porcentaje de mujeres víctimas de violencia por razones de género que solicitaron y obtuvieron orden y/o medida de protección.", style="color:gray"),
                      box(
                        width=12,  
                        valueBox("Indicador 2021", "60.5%",icon=icon("chart-area"),color="fuchsia"),
                        valueBox("Indicador 2020", "55.3%", icon=icon("arrow-circle-down"), color="purple"),
                        valueBox("Indicador 2019", "61.0%", icon=icon("arrow-circle-up"), color="maroon")),
                      
                      
                      box(width = 6, 
                          # height = "100%",
                          # id = "table",
                          # title = "Calculo del indicador mensual (Histórico).",
                          dataTableOutput(outputId = "table7")),
                      
                      
                      tabBox(
                        width = 6,
                        height = "100%",
                        tabPanel(title = "Gráfica", plotlyOutput("gr7")),
                        tabPanel(title = "Mapa")),
                      
      ))
   ))
))

)



server <- function(input, output) {

    output$gr6 <- renderPlotly({
      
      ggplotly(gr6) %>% 
        layout(title = "Indicador 6",
               legend = list(orientation = 'v', 
                             x = 0, y = -1), 
               xaxis = list(side = "bottom"),legend = list(side="bottom"))
      
      
          })
    
    output$table <- renderDataTable({
      tabla_6 %>% datatable(extensions = 'Buttons',
                            options = list(dom = 'Blfrtip',
                                           buttons = c('copy', 'excel', 'print'),
                                           lengthMenu = list(c(5,15,25,50,-1),
                                                             c(5,15,25,50,"All"))))
      
    })
    
    output$table7 <- renderDataTable({
      tabla_7 %>% 
      datatable(extensions = 'Buttons',
                options = list(dom = 'Blfrtip',
                               buttons = c('copy', 'excel', 'print'),
                               lengthMenu = list(c(3,5,10,25,50,-1),
                                                 c(3,5,10,25,50,"All"))))
      
    })
    output$gr7 <- renderPlotly({
    ggplotly(gr7)%>% 
      layout(title = "Indicador 7",
             legend = list(orientation = 'v', 
                           x = 0, y = -1), 
             xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
