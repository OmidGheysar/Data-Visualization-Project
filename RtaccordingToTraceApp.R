
RtBasedonAppTrace <- function(dat,
                                 days,
                                 R,
                                 p.tr = 0,
                                 p.trace_ap = 100,
                                 p.sym,
                                 iso_delay_traced = 1,
                                 iso_delay_untraced,
                                 sd_contact){


select64000Scenarios <- function(dat,
                            days,
                            R,
                            p.tr,
                            p.trace_ap,
                            p.sym,
                            iso_delay_traced,
                            iso_delay_untraced,
                            sd_contact) {
  
  scenarios<-dat %>% filter(R0==R &
                              p.trace==0&
                              # p.trace_app==p.trace_ap&
                              p.symp== p.sym&
                              iso_delay_traced_max==iso_delay_traced&
                              # iso_delay_untraced_sd_max==iso_delay_untraced&
                              sd_contact_rate1==sd_contact) %>% filter(day==days) %>%  
    select(p.trace_app,iso_delay_untraced_sd_max,"day":"n.active_Q_95") 
  return(scenarios)
}


aes_x <- "p.trace_app"
aes_y <- "Revenue"
aes_col <- "iso_delay_untraced_sd_max"
aes_grp <- "iso_delay_untraced_sd_max"

results<- select64000Scenarios(dat, days, R, p.tr, p.trace_ap, p.sym,
                          iso_delay_traced, iso_delay_untraced, sd_contact)
# dat <- readRDS("Newdata.rds")
# results<- select64000Scenarios(dat,10 ,3,.5,.5,.7,2,1,.3)
long_results <- results %>% gather(Quarter, Revenue, Rt_Q_05:Rt_Q_95) 
p <- ggplot(long_results,
            aes(x=eval(as.name(aes_x)),
                y=eval(as.name(aes_y)),
                color=factor(eval(as.name(aes_col))),
                group=eval(as.name(aes_grp)))) +
  stat_summary(geom="pointrange",
               fun.y  = "median",
               fun.ymin = function(x) quantile(x, .25),
               fun.ymax = function(x) quantile(x, .75),size=2) +
  stat_summary(geom="line",
               fun.y = "median",size=1)+ylim(0,1.5)+
  labs(y="Reproductive Number", x="",
       color="")
p <- p + geom_hline(yintercept=1,
                    linetype='dotdash',
                    alpha=0.6)
p <- p + labs(title="Colors show delay to isolation for untraced & distancing cases")
p <- ggplotly(p)
x <- list(
  title = "Fraction of people using contact tracing app"
)
y <- list(
  title = "Reproductive Number"
)

return(p %>% layout(xaxis = x, yaxis = y,  margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),title=list(x=1))) 
  
}
  
UiRt_Only_App <- function(){
  
  
  ui <- fluidPage(
    # Application title
    titlePanel("Parameters of Scenarios"),
    
    sidebarLayout(
      # Sidebar with a slider and selection inputs
      sidebarPanel(
        
        shinyWidgets::sliderTextInput("sd_contactforApp","
                                      Strength of physical distancing (contact rate)",
                                      choices=c(0.3, 0.6, 0.8),
                                      selected=0.3, grid = T),
        sliderInput("iso_delay_untracedforApp",
                    "Delay to isolation for untraced & distancing cases:",
                    min = 1,  max = 5, value = 1, step = 4),
        
        selectInput("selectionApp", "Select something", choices = c("Descision Making Parameters", "All Parameters")),
        conditionalPanel(
          "input.selectionApp == 'All Parameters'",
          sliderInput("R0forApp",
                      "R0:",
                      min = 2,  max = 3, value = 2, step = .5),
          
          sliderInput("p.symforApp",
                      "Fraction of cases that are symptomatic",
                      min = .6,  max = .8, value = .6, step = .1),
          
          sliderInput("daysforApp",
                      "days:",
                      min = 0,  max = 30,  value = 20)
        ),
        hr(),
        h3("Assumpations"),
        tableOutput("tableApp")
      ),
      
      # Show Word Cloud
      mainPanel(
        plotlyOutput("plotRt_Only_App"),
        h4(code("Impact of digital contact tracing for 
                Reproductive Number of covid-19"))
      )
    )
  )
  return(ui)
  
}  


# server <- function(input, output, session) {
# 
#   output$tableApp <- renderTable({
#     data.frame(
#       Name = c("R0 ",
#                "Fraction of cases that are symptomatic",
#                "Delay to isolation for untraced & distancing cases",
#                "days",
#                "Delay to isolation for traced cases (days)",
#                "Fraction of people using contact tracing app",
#                "Fraction of cases manually traced",
#                "Strength of physical distancing (contact rate)"),
#       Value = as.character(c(input$R0forApp,
#                              input$p.symforApp,
#                              input$iso_delay_untracedforApp,
#                              input$daysforApp,
#                              "input$iso_delay_tracecedforApp",
#                              0,
#                              "None",
#                              input$sd_contactforApp)),
#       stringsAsFactors = FALSE)
#   })
#   
# }
# 
# shinyApp(ui, server)



plotRt_Only_App <- function(input){
  
  myPlot <- RtBasedonAppTrace(dat,
                              day = input$daysforApp,
                              R = input$R0forApp,
                              p.tr = 0,
                              p.trace_ap = 100,
                              p.sym = input$p.symforApp,
                              iso_delay_traced=1,
                              iso_delay_untraced= input$iso_delay_untracedforApp,
                              sd_contact = input$sd_contactforApp)
  return(myPlot)
  
}




  
