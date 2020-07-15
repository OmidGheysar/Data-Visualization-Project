source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/uploadRequiredLibraries.R")
uploadRequiredLibraries()

# dat <- readRDS("05_22.rds")

ui <- dashboardPage(
  dashboardHeader(title = "COVID 19 Simulatoin"),
  dashboardSidebar(  
    # =============================================
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
      # =============================================
    )
  ),
  dashboardBody(
    # UiDesign(ui),
    UiRt_Only_Manual()
  ),
  
)


# server1 <- ServerDesingOver()

# ui <- UiRt_Only_Manual()
server <- ServerRt_Only_Manual()


shinyApp(ui, server)

