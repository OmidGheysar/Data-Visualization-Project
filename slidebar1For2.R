slidebar1For2 <- function() {
  
  ui <- fluidPage(

sliderInput("R0", "R0",
            min = 2, max = 3,
            value = 2.5, step = 0.5),

        sliderInput("p.trace", "p.trace",
                    min = 0, max = 1,
                    value = .5, step = 0.25),

        sliderInput("p.trace_app", "p.trace_app",
                    min = 0, max = 1,
                    value = .5, step = 0.25),

        sliderInput("p.symp", "p.symp",
                    min = 0.6, max = 0.8,
                    value = .7, step = 0.1),

        sliderInput("iso_delay_traced_max", "iso",
                    min = 1, max = 4,
                    value = 2, step = 1),

        sliderInput("iso_delay_untraced_sd_max", "iso_delay",
                    min = 1, max = 5,
                    value = 1, step = 4),
    #     # 
        sliderInput("sd_contact_rate1", "sd_contact_rate1",
                    min = 0.3, max = 0.8,
                    value = 0,3, , step = .5),
        shinyWidgets::sliderTextInput("sd_contact_rate1","sd_contact",
                                      choices=c(0.3, 0.6, 0.8),
                                      selected=0.3, grid = T),

        sliderInput("day", "day",
                    min = 0, max = 31,
                    value = 20, step = 1)
  )
  return(ui)
}





