library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(tidyverse)
library(openxlsx)
library(sf)
library(abjutils)
library(brazilmaps)
library(geojson)
library(leafletR)
library(rgdal)
library(rjson)

#sudo systemctl start shiny-server
#setwd("/srv/shiny-server/bolsa-merenda")
##### Read db

# Pagamentos - Municipal
pagamentos_alunos_mun <- read_csv("data/dados_por_municipio.csv")

# Base mapas
regional <- read_csv("data/dados_por_regional_sedese.csv") %>%
  mutate(Diretoria.Regional_2 = rm_accent(`Diretoria Regional`))


# mg dados
mg_dados <- openxlsx::read.xlsx("data/mg_dados.xlsx", sheet = 1)

mg_dados <- mg_dados %>%
  select(Código.IBGE, Município, Mesorregião, Diretoria.Regional) %>%
  mutate(Município = str_to_upper(rm_accent(Município)))

# Mapa
url <- 'https://raw.githubusercontent.com/ods-sedese/bolsa-merenda/master/data/regionais_mg.geojson'
geojson <- rjson::fromJSON(file=url)

g <- list(
  fitbounds = "locations",
  visible = FALSE
)

# Valores
valores <- pagamentos_alunos_mun %>%
  summarise(`Valor repassado aos beneficiários` = sum(`Valor repassado aos beneficiários`),
            `Número de famílias potenciais beneficiárias` = sum(`Número de famílias extremamente pobres potenciais beneficiárias`) + sum(`Número de famílias pobres potenciais beneficiárias`),
            `Número de estudantes potenciais beneficiários` = sum(`Número de alunos extremamente pobres potenciais beneficiários`) + sum(`Número de alunos pobres potenciais beneficiários`),
            `Percentual de pessoas extremamente pobres atendidas` = sum(`Número de alunos extremamente pobres que receberam o benefício`) / sum(`Número de alunos extremamente pobres potenciais beneficiários`) * 100,
            `Percentual de pessoas pobres atendidas` = sum(`Número de alunos pobres que receberam o benefício`) / sum(`Número de alunos pobres potenciais beneficiários`) * 100,
            `Percentual de famílias extremamente pobres atendidas` = sum(`Número de famílias extremamente pobres que receberam o benefício`) / sum(`Número de famílias extremamente pobres potenciais beneficiárias`) * 100,
            `Percentual de famílias pobres atendidas` = sum(`Número de famílias pobres que receberam o benefício`) / sum(`Número de famílias pobres potenciais beneficiárias`) * 100) %>%
  mutate(`Valor repassado aos beneficiários` = format(`Valor repassado aos beneficiários`, big.mark = ".", decimal.mark = ","),
         `Número de famílias potenciais beneficiárias` = format(`Número de famílias potenciais beneficiárias`, big.mark = ".", decimal.mark = ","),
         `Número de estudantes potenciais beneficiários` = format(`Número de estudantes potenciais beneficiários`, big.mark = ".", decimal.mark = ","),
         `Percentual de pessoas extremamente pobres atendidas` = format(`Percentual de pessoas extremamente pobres atendidas`, big.mark = ".", decimal.mark = ",", digits = 3),
         `Percentual de pessoas pobres atendidas` = format(`Percentual de pessoas pobres atendidas`, big.mark = ".", decimal.mark = ",", digits = 3),
         `Percentual de famílias extremamente pobres atendidas` = format(`Percentual de famílias extremamente pobres atendidas`, big.mark = ".", decimal.mark = ",", digits = 3),
         `Percentual de famílias pobres atendidas` = format(`Percentual de famílias pobres atendidas`, big.mark = ".", decimal.mark = ",", digits = 3)
         )

###################################
#                                 #
#           Dashboard             #
#                                 #
###################################

# Dashboard
header <- dashboardHeader(title = "Bolsa Merenda",
                          tags$li(div(href = 'http://social.mg.gov.br',
                                    tags$img(src = 'images/sedese_menor.png',
                                        title = "Sedese", height = "50px"),
                                    style="text-align: center;"),
                                  class = "dropdown"))
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dados Gerais",
             tabName = "dados_gerais", 
             icon = icon("chart-bar")),
    menuItem("GitHub", icon = icon("fab fa-github"), href = "https://github.com/ods-sedese/bolsa-merenda")
  )
)
body <- dashboardBody(
  tags$head(tags$style(HTML(' /* body */
                                .content-wrapper, .right-side {
                                background-color: white;
                                }
        /* main sidebar */
        .skin-black .main-sidebar {
                              background-color: #383d40;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: white;
                              }
        /* other links in the sidebarmenu */

        .skin-black .main-sidebar .sidebar .sidebar-menu a{
                              background-color: white;
                              color: black;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: light-gray;
                              }
        /* toggle button when hovered  */                    
         .skin-black .main-header .navbar .sidebar-toggle:hover{
                              background-color: light-gray;
                              }
'))),
  tabItems(
    tabItem(tabName = "dados_gerais",
            fluidRow(
              infoBox("Valor transferido", paste("R$", valores$`Valor repassado aos beneficiários`, sep = " "), icon = icon("donate"), width = 3, color = "olive"),
              infoBoxOutput("n_potenciais_beneficiarios", width = 3),
              infoBoxOutput("perc_beneficiarios_ext_pobre", width = 3),
              infoBoxOutput("perc_beneficiarios_pobre", width = 3)
            ),
            fluidRow(
              column(selectInput("publico", "Público de Análise:",
                                 c("Pessoa" = "pessoa",
                                   "Família" = "familia"),
                          selected = "pessoa"), offset = 4, width = 3, align = "center")),
            fluidRow(
              column(plotlyOutput("mapa_ext_pobres"), width = 6),
              column(plotlyOutput("mapa_pobres"), width = 6)
            ),
            fluidRow(
              column(h3("Tabela com informações municipalizadas"),width =  10, offset = 1, align = "center", DT::DTOutput('table_municipio'),style = "background: white; margin-top: 20px; width: 80%")
            )
    )
  )
)

# Define UI ----
ui <- dashboardPage(header,sidebar,body, skin = "black")

# Define server logic ----
server <- function(input, output) {

  ##################################
  #                                #
  #           InfoBox              #
  #                                #
  ##################################
  
  # Total
  output$n_potenciais_beneficiarios <- renderInfoBox({
    if (input$publico == "pessoa") {
      infoBox(
        HTML(paste("Número de estudantes potencialmente",br(), "beneficiários")), 
                   valores$`Número de estudantes potenciais beneficiários`, 
                   icon = icon("user-friends"), color = "yellow") 
    }
    else {
      infoBox(
        HTML(paste("Número de famílias potencialmente",br(), "beneficiárias")), 
                   valores$`Número de famílias potenciais beneficiárias`, 
                   icon = icon("user-friends"), color = "yellow")
    }
  })
  
  # Extremamente Pobre
  output$perc_beneficiarios_ext_pobre <- renderInfoBox({
    if (input$publico == "pessoa") {
      infoBox(
        HTML(paste("Percentual de estudantes extremamente",br(), "pobres atendidos")), 
        paste(valores$`Percentual de pessoas extremamente pobres atendidas`, "%", sep = ""), 
        icon = icon("chart-line"), color = "purple") 
    }
    else {
      infoBox(
        HTML(paste("Percentual de famílias extremamente",br(), "pobres atendidas")), 
        paste(valores$`Percentual de famílias extremamente pobres atendidas`, "%", sep = ""), 
        icon = icon("chart-line"), color = "purple")
    }
  })
  
  # Pobre  
  output$perc_beneficiarios_pobre <- renderInfoBox({
    if (input$publico == "pessoa") {
      infoBox(
        HTML(paste("Percentual de estudantes",br(), "pobres atendidos")), 
        paste(valores$`Percentual de pessoas pobres atendidas`, "%", sep = ""), 
        icon = icon("chart-line"), color = "purple")
    }
    else {
      infoBox(
        HTML(paste("Percentual de famílias",br(), "pobres atendidas")), 
        paste(valores$`Percentual de famílias pobres atendidas`, "%", sep = ""), 
        icon = icon("chart-line"), color = "purple")
    }    
  })

  ##################################
  #                                #
  #   Mapa Extremamente pobres     #
  #                                #
  ##################################
  
  output$mapa_ext_pobres <- renderPlotly({
    if (input$publico == "pessoa") {
      regional_2 <- regional %>%
        mutate(var_selecionada_ext_pobre = `Percentual de alunos extremamente pobres que receberam o benefício`)
      titulo_ext_pobre = "\nPorcentagem de estudantes extremamente pobres atendidos"

    }
    else {
      regional_2 <- regional %>%
        mutate(var_selecionada_ext_pobre = `Percentual de famílias extremamente pobres que receberam o benefício`)
      titulo_ext_pobre = "\nPorcentagem de famílias extremamente pobres atendidas"
    }
    mapa_ext_pobres <- plot_ly()
    mapa_ext_pobres <- mapa_ext_pobres %>%
      add_trace(type = "scattergeo", 
                mode = "text",
                geojson=geojson,
                locations=regional_2$Diretoria.Regional_2,
                text = paste("<b>",regional_2$`Diretoria Regional`,"<b>", sep = ""),
                locationmode = "geojson-id",
                featureidkey="properties.Drtr_Rg",
                textfont = list(size = 8, color = "black"),
                hoverinfo = "skip"
      )
    mapa_ext_pobres <- mapa_ext_pobres %>%
      add_trace(
        type="choropleth",
        geojson=geojson,
        locations=regional_2$Diretoria.Regional_2,
        z=regional_2$var_selecionada_ext_pobre,
        colorscale= list(c(0, "rgb(255, 255, 255)"), c(1, "rgb(255,127,0)")),
        zmin = 0,
        zmax = 100,
        text = regional_2$`Diretoria Regional`,
        featureidkey="properties.Drtr_Rg",
        hovertemplate = paste("%{text}: %{z:.1f}%<extra></extra>"),
        marker = list(line = list(color = "#000"))
      )
    mapa_ext_pobres <- mapa_ext_pobres %>% colorbar(title = "<b>Porcentagem<br> </b>",
                                                    nticks = 10, ticklen = 10)
    mapa_ext_pobres <- mapa_ext_pobres %>% layout(
      title = titulo_ext_pobre,
      geo = g,
      autosize = TRUE,
      margin = list(
        l = 0,
        r = 0,
        b = 0,
        t = 0,
        pad = 0),
      hovermode='closest'
    )  
  })#
  
  ##################################
  #                                #
  #         Mapa pobres            #
  #                                #
  ##################################
  
  output$mapa_pobres <- renderPlotly({
    
    if (input$publico == "pessoa") {
      regional_2 <- regional %>%
        mutate(var_selecionada_pobre = `Percentual de alunos pobres que receberam o benefício`)
      titulo_pobre = "\nPorcentagem de estudantes pobres atendidos"
    }
    else {
      regional_2 <- regional %>%
        mutate(var_selecionada_pobre = `Percentual de famílias pobres que receberam o benefício`)
      titulo_pobre = "\nPorcentagem de famílias pobres atendidas"
    }
    mapa_pobres <- plot_ly()
    mapa_pobres <- mapa_pobres %>%
      add_trace(type = "scattergeo", 
                mode = "text",
                geojson=geojson,
                locations=regional_2$Diretoria.Regional_2,
                text = paste("<b>",regional_2$`Diretoria Regional`,"<b>", sep = ""),
                locationmode = "geojson-id",
                featureidkey="properties.Drtr_Rg",
                textfont = list(size = 8),
                hoverinfo = "skip"
      )
    mapa_pobres <- mapa_pobres %>%
      add_trace(
        type="choropleth",
        geojson=geojson,
        locations=regional_2$Diretoria.Regional_2,
        z=regional_2$var_selecionada_pobre,
        colorscale= list(c(0, "rgb(255, 255, 255)"), c(1, "rgb(255,127,0)")), #rgb(153, 51, 153)
        zmin = 0,
        zmax = 100,
        text = regional_2$`Diretoria Regional`,
        featureidkey="properties.Drtr_Rg",
        hovertemplate = paste("%{text}: %{z:.1f}%<extra></extra>"),
        marker = list(line = list(color = "#000"))
      )
    mapa_pobres <- mapa_pobres %>% colorbar(title = "<b>Porcentagem<br> </b>",
                                            nticks = 10, ticklen = 10)
    mapa_pobres <- mapa_pobres %>% layout(
      title = titulo_pobre,
      geo = g,
      autosize = TRUE,
      margin = list(
        l = 0,
        r = 0,
        b = 0,
        t = 0,
        pad = 0),
      hovermode='closest'
    ) 
  })
  
  ##################################
  #                                #
  #             Tabela             #
  #                                #
  ##################################
  
  output$table_municipio <- DT::renderDataTable({
    
    if (input$publico == "pessoa") {
      tabela <- pagamentos_alunos_mun %>%
        select(1:9,16)
      
      datatable(tabela, 
                rownames = FALSE,
                options = list(scrollX = TRUE)
      ) %>%
        formatPercentage("Percentual de alunos extremamente pobres que receberam o benefício", 2) %>%
        formatPercentage("Percentual de alunos pobres que receberam o benefício", 2) %>%
        formatCurrency("Valor repassado aos beneficiários", "R$ ", mark = ".", dec.mark = ",") %>%
        formatRound("Número de alunos extremamente pobres potenciais beneficiários", mark = ".", digits = 0) %>%
        formatRound("Número de alunos extremamente pobres que receberam o benefício", mark = ".", digits = 0) %>%
        formatRound("Número de alunos pobres potenciais beneficiários", mark = ".", digits = 0) %>%
        formatRound("Número de alunos pobres que receberam o benefício", mark = ".", digits = 0)
    }
    else {
        tabela <- pagamentos_alunos_mun %>%
          select(1:3,10:16)
  
        datatable(tabela, 
                  rownames = FALSE,
                  options = list(scrollX = TRUE)
        ) %>%
          formatPercentage("Percentual de famílias extremamente pobres que receberam o benefício", 2) %>%
          formatPercentage("Percentual de famílias pobres que receberam o benefício", 2) %>%
          formatCurrency("Valor repassado aos beneficiários", "R$ ", mark = ".", dec.mark = ",") %>%
          formatRound("Número de famílias extremamente pobres potenciais beneficiárias", mark = ".", digits = 0) %>%
          formatRound("Número de famílias extremamente pobres que receberam o benefício", mark = ".", digits = 0) %>%
          formatRound("Número de famílias pobres potenciais beneficiárias", mark = ".", digits = 0) %>%
          formatRound("Número de famílias pobres que receberam o benefício", mark = ".", digits = 0)
        
    }
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
