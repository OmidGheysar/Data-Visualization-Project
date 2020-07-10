
createDataFrame <- function(input) {
  
  df <- data.frame(
    Name = c("R0 value",
             "p.trace value",
             "p.trace_app value",
             "p.symp value",
             "iso_delay_traced_max value",
             "iso_delay_untraced_sd_max value",
             "sd_contact_rate1 value",
             "days"),
    Value = as.character(c(input$R0,
                           input$p.trace,
                           input$p.trace_app,
                           input$p.symp,
                           input$iso_delay_traced_max,
                           input$iso_delay_untraced_sd_max,
                           input$sd_contact_rate1,
                           input$day)),
    stringsAsFactors = FALSE)
  
  return(df)
}