library(shiny)
library(leaflet)
library(tidyverse)
library(rvest)
library(raster)
library(sf)
library(rgeos)

pop <- tibble(
  name = c("Baden-Württemberg","Bayern","Berlin",
           "Brandenburg","Bremen","Hamburg","Hessen",
           "Mecklenburg-Vorpommern","Niedersachsen","Nordrhein-Westfalen",
           "Rheinland-Pfalz","Saarland","Sachsen","Sachsen-Anhalt",
           "Schleswig-Holstein","Thüringen"),
  Pop = c(11069533,13076721,3644826,2511917,
          682986,1841179,6265809,1609675,7982448,17932651,4084844,
          990509,4077937,2208321,2896712,2143145)
)


ui <- bootstrapPage(
  
  absolutePanel(
    id = "controls", class = "panel panel-default",
    top = 120, left = 10, width = 300, height = "auto",fixed = TRUE, style = "z-index:500; background: #FFFFFF;padding: 8px;border: 1px solid #CCC;",
    HTML('<button data-toggle="collapse" data-target="#panel">Informationen</button>'),
    tags$div(id = 'panel',  class="collapse",
             tags$h2("SARS-CoV-2: Fallzahlen in Deutschland"),
             tags$p("Hier sind ausschließlich Fälle aufgelistet, die dem RKI über den Meldeweg oder offizielle Quellen mitgeteilt wurden.
         Da es sich um eine sehr dynamische Situation handelt, kann es zu Abweichungen zwischen der RKI-Tabelle und Angaben anderer Stellen,
         etwa der betroffenen Bundesländer, kommen."),
             h3(textOutput("gesamt")),
             tags$hr(style="border-color: black;"),
             tags$p("Quelle: Robert Koch Institut"),
             p(textOutput("dateText")),
             tags$p("Autor:"),
             tags$a("Yongguang Lin", href="https://github.com/banappl3"))
  ),
  leafletOutput("mymap", width = "100%", height = 900)
)

server <- function(input, output, session) {
  
  autoInvalidate <- reactiveTimer(300000)
  
  observe({
    autoInvalidate()
  })
  
  output$dateText <- renderText({
    autoInvalidate()
    paste("Stand:", Sys.time())
  })
  
  data <- reactivePoll(
    intervalMillis = 300000,
    session,
    checkFunc = function(){
      Sys.time()
    },
    valueFunc = function(){
      url <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html"
      corona <- url %>%
        xml2::read_html() %>%
        html_nodes(xpath='//*[@id="main"]/div[1]/table') %>%
        html_table()
      corona <- corona[[1]]
      
      names(corona) <- NULL
      corona <- corona[-c(1), ]
      
      names(corona) = c("name", "Faelle", "Diff", "tage","tageinz","dead")
      corona <- as.tibble(corona)
    }
  )
  
  output$gesamt <- renderText({
    
    corona_data <- data() 
    gesamt <- corona_data %>%
      dplyr::select(name,Faelle,dead) %>% 
      filter(name == "Gesamt")
    paste("Gesamtfälle:", gesamt$Faelle, "Todesfälle:", gesamt$dead)
  })
  
  output$mymap <- renderLeaflet({
    
    corona_data <- data() 
    
    corona_ger <- corona_data %>% 
      slice(1:(n()-1)) %>%
      dplyr::select(name,Faelle,tageinz,dead)
    corona_ger$Faelle <- str_replace(corona_ger$Faelle, "\\.", "")
    corona_ger$Faelle <- str_replace(corona_ger$Faelle, "\\.", "")
    corona_ger$Faelle <- as.numeric(corona_ger$Faelle)
    
    corona_ger$tageinz <- str_replace(corona_ger$tageinz, "\\,", ".")
    corona_ger$tageinz <- as.numeric(corona_ger$tageinz)
    
    corona_ger$dead <- str_replace(corona_ger$dead, "\\.", "")
    corona_ger$dead <- as.numeric(corona_ger$dead)
    
    corona_ger["Pop"] <- pop$Pop
    corona_ger <- corona_ger %>% 
      mutate(per_k = (Faelle/Pop)*100000)
    
    corona_inf <- colorNumeric(palette="viridis", domain = corona_ger$Faelle, na.color="transparent")
    
    DEU1 <- raster::getData("GADM", country="DEU", level=1)
    deu_states <- st_as_sf(DEU1)
    
    deu_states <- deu_states %>% 
      rename(name = "NAME_1")
    
    corona_ger_sf <- deu_states
    corona_ger_sf["Faelle"] <- corona_ger$Faelle
    corona_ger_sf["Pop"] <- corona_ger$Pop
    corona_ger_sf["per_k"] <- corona_ger$tageinz
    corona_ger_sf["dead"] <- corona_ger$dead
    
    labels_total <- sprintf(
      "<strong>%s</strong><br/><b>Neuinfektion: %s</b><br/>Infektionen: %s <br/>Todesfälle: %s",
      corona_ger_sf$name, 
      formatC(corona_ger_sf$per_k, format="f", big.mark=".",decimal.mark=",", digits=2),
      formatC(corona_ger_sf$Faelle, format="f", big.mark=".",decimal.mark=",", digits=0),
      formatC(corona_ger$dead, format="f", big.mark=".", digits=0)
    ) %>% lapply(htmltools::HTML)
    
    
    leaflet(corona_ger_sf, options = leafletOptions(zoomControl = FALSE)) %>%
      setView(11, 50, zoom = 6) %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE) 
      ) %>% 
      
      addPolygons(group = "Infektion",
                  weight = 2,
                  fillColor = ~corona_inf(Faelle),
                  fillOpacity = 0.6,
                  label = labels_total,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>% 

      
      addLayersControl(
        position = "topleft",
        overlayGroups = c("Infektion"),
        options = layersControlOptions(collapsed = FALSE) 
      ) %>% 
      hideGroup(c("Fälle pro 100.000 Einwohner"))
  })
}
shinyApp(ui, server)
