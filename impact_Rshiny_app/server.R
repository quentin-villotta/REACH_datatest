

server <- function(input, output) {
  
  # Load data
  df_path <- "~/Documents/Projets/R_projects/impact_data_test/data/data_reach.xlsx"
  df <- read_excel(path=df_path)
  
  
  df_sum <- summarizer_all(df, everything())
  
  
  
  df %>%
    mutate(sum_house = rowSums(across(c(`Number household member boy under5 years old`, `Number household member _girl_under5 years old`,
                                        `Number household member boy_5_17 years old`, 
                                        `household_girl_5_17`,`number adult household members years old` ))),
           compare = ifelse(sum_house == `Total household number`, TRUE, FALSE)) %>% 
    select(InterviewID, compare, `Total household number`,sum_house) %>% 
    filter(compare == FALSE)
  
  
  # drinking_water_source
  
  Improved_water_source <- c("Protected dug well" ,"Piped water to yard or plot",
                             "Piped water into dwelling (house)","Bottled water", "Tube well or borehole", "Public tap or standpipe",
                             "Protected spring")
  
  
  Unimproved_water_source <- c("Cart with small tank or drum","Unprotected dug wel", "Unprotected spring",
                               "Surface water", "Tanker-truck", "Rainwater collection")
  
  
  
  
  df<- df %>%
    mutate(improved_water_source = case_when(drinking_water_source %in% Improved_water_source ~ "Improved water source"  ,
                                             drinking_water_source %in% Unimproved_water_source ~ 'Unimproved water source',
                                             drinking_water_source == "Other" ~ NA)) 
  
  
  ########## TAB : Descriptive

num_col<- df %>% 
  select_if(is.numeric) %>%
  colnames()

group_col<- df %>% 
  select_if(is.character) %>%
  colnames()

# Box plot
output$solo_hist_plot <- renderPlotly({
  # Génération du graphique
  p1 <- ggplot(data, aes(x = .data[[input$variablenum]], group = .data[[input$groupcol]] ,color=.data[[input$groupcol]] )) + 
    geom_histogram() +
    labs(x = input$variablenum) +
    theme_classic()
  ggplotly(p1)
})

# Boxplot
output$solo_box_plot <- renderPlotly({
  # Génération du graphique
  p2 <- ggplot(data, aes(y = .data[[input$variablenum]], x = .data[[input$groupcol]] ,color=.data[[input$groupcol]] )) + 
    geom_boxplot() +
    labs(x = input$variablenum) + 
    theme_ipsum() + 
    theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=1)) 
  ggplotly(p2)
})

  ########## TAB : Correltion
  

  # output$plot1 <- renderPlot({
  #   data <- histdata[seq_len(input$slider)]
  #   hist(data)
  # })
}