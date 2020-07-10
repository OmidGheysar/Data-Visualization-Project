

 run_sim_shiny(n_initial = 10,
              tmax = 30,
              R0 = 2.5,
              p_symp = 1.0,
              p_trace = 1,
              p_trace_app = 0,
              p_trace_app_comp = 0,
              man_traced_delay = 2,
              import_rate = 0,
              contact_rate = 0.6,
              Rt_window = 7)


 change_to_factor <- function(x){
   x <- change_to_character(x)
   as.factor(x)
 } 
 
 change_to_character <- function(x){
   x <- change_to_factor(x)
   as.character(x)
 }
 
 change_to_character("1")
