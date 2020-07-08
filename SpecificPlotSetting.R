
SpecificSettingOfPlotDays <- function(nameOfcolumn,p) {

  if ( nameOfcolumn=="n.active") {
    # p <- p+  ylim(100,500)
  } else if ( nameOfcolumn=="Rt") {
    p <- p+  ylim(0,2.7)+
      geom_hline(yintercept=1, linetype="twodash", 
                 color = "red", size=.7)
  } else if ( FALSE) {
  } else {
  }
  
  return(p)
}

