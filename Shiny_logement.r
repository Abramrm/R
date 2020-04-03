rm(list=ls())

library(dplyr)
library(data.table)
library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(plotly)
library(leaflet)

#install.packages("ggplot2")
#install.packages("plotly")
#install.packages("leaflet")

# Import des donnees a partir de l'adresse URL du fichier
URL_data <- "https://cadastre.data.gouv.fr/data/etalab-dvf/latest/csv/2019/full.csv.gz"
GZ_data <- "full.csv.gz"
download.file(url = URL_data, dest = GZ_data)
data <- read.csv(gzfile(GZ_data), encoding = "UTF-8")

glimpse(data)
# Test du code sur les 1000 premieres lignes
data_deb <- data[1:100000,]

data_select <-  data_deb %>%
  select(valeur_fonciere, code_postal, code_departement, type_local,
         surface_reelle_bati, surface_terrain, longitude, latitude)
glimpse(data_select)

table_stat <- data_select %>%
  group_by(code_departement) %>% 
  summarise(number=n(), 
            moy_srb = mean(valeur_fonciere, na.rm = T),
            max_srb = max(valeur_fonciere, na.rm = T), 
            min_srb = min(valeur_fonciere, na.rm = T))

dep_cp <- data_select %>% 
            select(code_departement, longitude, latitude) %>%
            distinct()

Prefectures_ulr <- "https://www.insee.fr/fr/statistiques/fichier/3720946/departement2019-csv.zip"
Prefecture_fichier <- "departement2019-csv.zip"
download.file(url = Prefectures_ulr, dest = Prefecture_fichier)
Pref <- read.csv(unzip(Prefecture_fichier) , encoding = "UTF-8")
Pref$dep = as.factor(Pref$dep)

Pref2 <- Pref %>% 
          select(dep, cheflieu, ncc) %>%
          left_join(data_select, by = c("dep" = "code_departement"))


# Creation de l'application Shiny
ui <- fluidPage(
  titlePanel("Prix immobilier par departement"),
  sidebarLayout(
    sidebarPanel(
    selectInput('dep', 'Numero de departement', choices = unique(table_stat$code_departement))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Plot', plotly::plotlyOutput('plot_stat')),
        tabPanel('Map', leafletOutput('map')),
        tabPanel('Table', DT::DTOutput("table_stat_apart"))
      )
    )
  )
)
server <- function(input, output, session){
  stat_apart <- function(){
    data_select %>%
      filter(code_departement == input$dep) %>% 
      summarise(number=n(), 
                moy_srb = mean(valeur_fonciere, na.rm = T),
                max_srb = max(valeur_fonciere, na.rm = T), 
                min_srb = min(valeur_fonciere, na.rm = T))
  }
  plot_trends <- function(){
    data_select %>% 
      filter(code_departement == input$dep) %>% 
      ggplot(aes(x = valeur_fonciere)) +
      geom_density()
  }

  output$plot_stat <- plotly::renderPlotly({
    plot_trends()
  })
  
  output$table_stat_apart <- DT::renderDT({
    DT::datatable(stat_apart())
  })

  output$map <- leaflet::renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(2.59, 46.34, zoom = 5.4)
  })
}
shinyApp(ui = ui, server = server)
