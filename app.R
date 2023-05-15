library(shiny)
library(googleway)
library(dplyr)
library(sf)
library(readxl)
library(colorRamps)


## Data
# Google Maps API
gm_api_key <- "AIzaSyDiDjJXNXRz1cFawQXYyXQDMUGsCxch910"

# Syria district gdb database
file_gdb <- "data/SYR_AdminBoundaries_candidate.gdb/"
sda_gdb <- st_read(file_gdb)

# HSOS syria 03/2023 data
file_hsos <- "data/REACH_SYR_HSOS_Dataset_March2023_NWS.xlsx"
hsos <- read_excel(path=file_hsos, sheet="Dataset")


match_col_expr_IDP <- "Three most common IDP shelter types -"
input_colnames <- select(hsos, contains(match_col_expr_IDP)) %>% colnames()
input_colnames

#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(c('green','yellow', 'red'))


ui <- fluidPage(
  tags$h1("SYRIA HSOS"),
  tags$h2("Northwest Syria - March 2023"),
  fluidRow(
    column(
      width = 3,
      selectInput(inputId = "inputVar", label = "Repoted common IDP shelter type:", multiple = FALSE, 
                  choices = input_colnames, selected = "Three most common IDP shelter types - Tent")
      ),
    column(
      width = 9,
      height = 150,
      google_mapOutput(outputId = "map")
    )
  )
)

server <- function(input, output) {
  data <- reactive({

      data_color <-hsos %>%
      mutate(logical_tent = case_when(!! rlang::sym(input$inputVar) == "Yes" ~ TRUE,
                                      !! rlang::sym(input$inputVar) == "No" ~ FALSE,
                                      !!rlang::sym(input$inputVar) %in%  c("No","Yes") ~ NA,
                                      .default = NA)) %>%
      group_by(`District p-code`) %>%
      summarise(prop_tent = mean(logical_tent, na.rm = TRUE)) %>%
      inner_join(x = sda_gdb,
                 by = c("admin2Pcode" ="District p-code")) %>%
      mutate(color_col = rbPal(10)[as.numeric(cut(prop_tent, breaks = 10))],
             district_name = toupper(admin2Name_en),
             info = paste0(district_name, " | ", round(prop_tent,2)))
   
  })
  
  output$map <- renderGoogle_map({
      req(input$inputVar)
      data() %>%
      google_map(key = gm_api_key) %>%
      add_polygons(polyline = "Shape", 
                   mouse_over = "info",
                   fill_colour = "color_col",
                   update_map_view = TRUE)
  })
}

shinyApp(ui = ui, server = server)