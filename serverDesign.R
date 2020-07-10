
serverDesign <- function(dat,input) {

  
  output<- returnPlot(dat,input$radioY,
                      input$R0,
                      input$p.trace,
                      input$p.trace_app,
                      input$p.symp,
                      input$iso_delay_traced_max,
                      input$iso_delay_untraced_sd_max,
                      input$sd_contact_rate1)
  return(output)
}