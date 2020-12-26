source("uploadRequiredLibraries.R")
uploadRequiredLibraries()

ui <- fluidPage(
  # sliderInput("n", "Number of points", 1, 100, 50),
  downloadButton("report", "Generate report")
)

server <- function(input, output, session) {
  output$report <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      # params <- list(n = input$n)
      
      id <- showNotification(
        "Rendering report...", 
        duration = NULL, 
        closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)


      source("PassingCarDataToRmarkDown.R")
      rmarkdown::render("paramPass.Rmd",
                        output_file = file,
                        params = list(df = df,
                                      db = db,
                                      dp3 = dp3,
                                      dp4 = dp4,
                                      dp5 = dp5,
                                      dp6 = dp6,
                                      dp7 = dp7,
                                      ivs = ivs,
                                      dvs = dvs,
                                      sth = sth)
      )
    }
  )
}

shinyApp(ui, server)
