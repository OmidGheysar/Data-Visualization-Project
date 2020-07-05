

select100Scenarios <- function(dat, R,p.tr,p.trace_ap,p.sym,iso_delay_traced,
                               iso_delay_untraced,sd_contact) {
  
  scenarios100<-dat %>% filter(R0==R &
                                 p.trace==p.tr&
                                 p.trace_app==p.trace_ap&
                                 p.symp== p.sym&
                                 iso_delay_traced_max==iso_delay_traced&
                                 iso_delay_untraced_sd_max==iso_delay_untraced&
                                 sd_contact_rate1==sd_contact) %>% select("day","Rt","n.active")
  return(scenarios100)
}


# output<- select100Scenarios(dat,2,0,0,.8,1,5,.8)
# 
# 
# for (i in 0:31){
#   resultofDay31<-output %>% filter(day==1) %>% select("day","Rt","n.active")
#   print(dim(resultofDay31))
# }
