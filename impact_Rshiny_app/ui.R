ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "REACH Syria : Data Test"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Descriptive", tabName = "descriptive", icon = icon("dashboard"))
      # menuItem("Correlation", tabName = "correlation", icon = icon("th"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "descriptive",
              h2("Descriptive Analysis variable"),
            # First part of the page
              fluidRow(
                # A static valueBox
                valueBox(2, "Rounds : Endline - Baseline", icon = icon("credit-card"),width = 3)
              ),
              # Second part
              fluidRow(
                box(title = "Variable numérique:",
                    solidHeader = TRUE,
                    status = "primary",
                    width = 4,
                    height = 150,
                    selectInput(inputId = "variablenum",
                                label = NULL,
                                choices = num_col,
                                selected = num_col[6])),

                box(title = "Group by variable:",
                    solidHeader = TRUE,
                    status = "primary",
                    width = 4,
                    height = 150,
                    selectInput(inputId = "groupcol",
                                label = NULL,
                                choices = group_col,
                                selected = group_col[2])),

              ),
              fluidRow(
              tabBox(
                # title = "First tabBox",
                width = 6L,  height = 400L,
                side = "right",
                id = "tabset1",
                tabPanel("Histogram", box(plotlyOutput("solo_hist_plot", width = 570, height = 320L))),
                tabPanel("Boxplot", box(plotlyOutput("solo_box_plot", width = 570, height = 320L )))
              )
            )
      )
    )
  )
)
