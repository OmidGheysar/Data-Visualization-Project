run_sim_shiny <- function(n_initial = 10,
                          tmax = 30,
                          R0 = 2.5,
                          p_symp = 1.0,
                          p_trace = 1,
                          p_trace_app = 0,
                          p_trace_app_comp = 0,
                          man_traced_delay = 2,
                          import_rate = 0,
                          contact_rate = 0.6,
                          Rt_window = 7){
  # Hard-coded parameters for this demo
  dt <- 1
  max_active_cases <- 5000
  do_variable_trace <- FALSE
  infect_dur <- 14
  incub_params <- list(dist='lognormal',meanlog=1.57, sdlog=0.65)
  serial_int_params <- list(dist='gamma', shape=2.29, rate=0.36)
  iso_delay_params <- list(
    dist='uniform',
    traced_min=man_traced_delay,
    traced_max=man_traced_delay,
    untraced_min=5,
    untraced_max=5,
    untraced_pd_min=5,
    untraced_pd_max=5
  )
  sec_infect_params <- list(type='Hellewell', disp=0.58) # From JH2020 but with larger disp.
  import_params <- list(
    type='constant',
    rate=import_rate,
    iso_lengths=c(0), # no self-isolation of imports
    iso_p.group=c(1)
  )
  phys_dist_params <- list(
    pd_pop_frac = 0.8,
    pd_contact_rate1 = contact_rate,
    pd_contact_rate2 = contact_rate,
    pd_change_t = 999
  )
  
  sim_params <- epi.branch.sim::initialize_sim_params(
    R0, infect_dur, do_variable_trace, p_trace,
    p_trace_app, p_trace_app_comp, p_symp, dt,
    incub_params, serial_int_params,
    iso_delay_params, sec_infect_params,
    import_params, phys_dist_params
  )
  sim_status <- epi.branch.sim::initialize_sim_status(0,n_initial)
  state_df   <- epi.branch.sim::create_state_df(n_initial,sim_params, sim_status, initialize=TRUE)
  record_df  <- epi.branch.sim::create_record_df(state_df, sim_status, initialize=TRUE)
  
  timemax <- ceiling(tmax/dt)
  # Set up vectors to track key metrics per time step
  n_total <- rep(NA,timemax) # number of total cases
  n_active <- rep(NA,timemax) # number of active cases
  n_incub <- rep(NA,timemax) # number of cases incubating
  n_symp <- rep(NA,timemax) # number of cases with symptoms
  n_asymp <- rep(NA,timemax) # number of cases not showing symptoms but past incubation
  n_iso <- rep(NA,timemax) # number of cases isolated
  n_new_sec <- rep(NA,timemax) # number of new secondary cases
  n_new_imp <- rep(NA,timemax) # number of new imported cases
  Reff <- rep(NA,timemax)
  Rt <- rep(NA,timemax) # average number of new infections / new cases for past Rt_count days
  
  # Calculate some initial values
  initial_Rt <- mean(state_df$n_sec_infects)
  
  for (ii in 1:timemax){
    # Take a step forward
    out <- epi.branch.sim::step_simulation(sim_status, state_df, record_df, sim_params)
    sim_status <- out$status
    state_df <- out$state
    record_df <- out$record
    # Track key metrics
    n_total[ii] <-nrow(record_df) # all cases ever
    n_active[ii] <-nrow(state_df) # incub + sympt + asympt
    n_incub[ii] <- sum(state_df$status=='incubation')
    n_symp[ii] <- sum(state_df$status=='symptomatic')
    n_asymp[ii] <- sum(state_df$status=='asymptomatic')
    n_iso[ii] <- sum(record_df$s_status=='isolated') # get this from record_df!
    n_new_sec[ii] <-out$new_sec_cases # new sec cases in last dt
    n_new_imp[ii] <- out$new_imp_cases # new imp cases in last dt
    Reff[ii] <- mean(record_df$n_sec_infects)
    # Find cases in record_df that began in the last Rt_window days
    record_df_Rt <- subset(record_df, t_inf > sim_status$t-Rt_window)
    Rt[ii] <- mean(record_df_Rt$n_sec_infects)
  }
  
  # Collapse metrics into daily counts (right now assuming dt=1, need a way to sum it up)
  day=0:(timemax*dt)
  bin_every = 1/dt
  nd_total <- c(n_initial,n_total[seq(0,timemax,bin_every)])
  nd_active <- c(n_initial,n_active[seq(0,timemax,bin_every)])
  nd_incub <- c(n_initial,n_incub[seq(0,timemax,bin_every)])
  nd_symp <- c(0,n_symp[seq(0,timemax,bin_every)])
  nd_asymp <- c(0,n_asymp[seq(0,timemax,bin_every)])
  nd_iso <- c(0,n_iso[seq(0,timemax,bin_every)])
  nd_new_S <- c(0,colSums(matrix(n_new_sec,nrow=bin_every)))
  nd_new_I <- c(0,colSums(matrix(n_new_imp,nrow=bin_every)))
  nd_Reff <- c(R0,Reff[seq(0,timemax,bin_every)])
  nd_Rt <- c(initial_Rt,Rt[seq(0,timemax,bin_every)])
  # Create output dataframe for this run
  results <- data.frame(
    # scenario parameters
    "R0" = R0,
    "p_symp" = p_symp,
    "n_initial" = n_initial,
    "p_trace" = p_trace,
    "p_trace_app" = p_trace_app,
    "p_trace_app_comp" = p_trace_app_comp,
    "man_traced_delay" = man_traced_delay,
    "import_rate" = import_rate,
    "contact_rate" = contact_rate,
    "Rt_window" = Rt_window,
    # results
    "day" = day,
    "n_total" = nd_total,
    "n_active" = nd_active,
    "n_incub" = nd_incub,
    "n_symp" = nd_symp,
    "n_asymp" = nd_asymp,
    "n_iso" = nd_iso,
    "n_new_S" = nd_new_S,
    "n_new_I" = nd_new_I,
    "Reff" = nd_Reff,
    "Rt" = nd_Rt
  )
  # Return data frame
  return(results)
}

plot_new_cases <- function(results,
                           ylim=NULL,
                           logy=FALSE,
                           plot_quantiles_dark =c(0.25,0.75),
                           plot_quantiles_light=c(0.05,0.90),
                           title='New cases per day',
                           subtitle=NULL,
                           caption=NULL){
  paired.cols <- RColorBrewer::brewer.pal(12, "Paired")
  
  # Tidy data for plot
  results <- as_tibble(results) %>%
    dplyr::select(
      day,
      n_new_S
    ) %>% 
    dplyr::group_by(day) %>%
    dplyr::summarise(
      new_cases_05 = quantile(n_new_S, 0.05),
      new_cases_25 = quantile(n_new_S, 0.25),
      new_cases = median(n_new_S),
      new_cases_75 = quantile(n_new_S, 0.75),
      new_cases_90 = quantile(n_new_S, 0.90)
    ) %>%
    dplyr::ungroup()
  
  p <- ggplot2::ggplot(results, ggplot2::aes(x=day)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(y=new_cases,
                   ymin=new_cases_05,
                   ymax=new_cases_90),
      fill=paired.cols[1],
      alpha=0.8
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(y=new_cases,
                   ymin=new_cases_25,
                   ymax=new_cases_75),
      fill=paired.cols[2],
      alpha=0.5
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y=new_cases),
      color=paired.cols[2],
      size=1.2
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y=new_cases),
      color=paired.cols[2],
      size=3
    ) +
    ggplot2::labs(x="Day", y=NULL) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme_classic(base_size=16)
  
  # Add optional titles and labels
  if (!is.null(title)){
    p <- p + ggplot2::labs(title=title)
  }
  if (!is.null(subtitle)){
    p <- p + ggplot2::labs(subtitle=subtitle)
  }
  if (!is.null(caption)){
    p <- p + ggplot2::labs(caption=caption)
  }
    
  # Use a log y scale if set
  if (logy){
    p <- p + ggplot2::scale_y_log10(labels=scales::comma)
  }
  
  # Use a user-defined max y value (for comparison across models)
  if (!is.null(ylim)){
    p <- p + ggplot2::coord_cartesian(ylim = ylim)
  }
  
  return(p)
}

plot_Rt <- function(results,
                    ylim=NULL,
                    plot_quantiles_dark =c(0.25,0.75),
                    plot_quantiles_light=c(0.05,0.90),
                    title='Reproductive number Rt',
                    subtitle=NULL,
                    caption=NULL){
  paired.cols <- RColorBrewer::brewer.pal(12, "Paired")
  
  # Tidy data for plot
  results <- as_tibble(results) %>%
    dplyr::select(
      day,
      Rt
    ) %>% 
    dplyr::group_by(day) %>%
    dplyr::summarise(
      Rt_05 = quantile(Rt, 0.05, na.rm=TRUE),
      Rt_25 = quantile(Rt, 0.25, na.rm=TRUE),
      Rt_median = median(Rt, na.rm=TRUE),
      Rt_75 = quantile(Rt, 0.75, na.rm=TRUE),
      Rt_90 = quantile(Rt, 0.90, na.rm=TRUE)
    ) %>%
    dplyr::ungroup()
  
  p <- ggplot2::ggplot(results, ggplot2::aes(x=day)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(y=Rt_median,
                   ymin=Rt_05,
                   ymax=Rt_90),
      fill=paired.cols[7],
      alpha=0.8
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(y=Rt_median,
                   ymin=Rt_25,
                   ymax=Rt_75),
      fill=paired.cols[8],
      alpha=0.5
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y=Rt_median),
      color=paired.cols[8],
      size=1.2
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y=Rt_median),
      color=paired.cols[8],
      size=3
    ) +
    ggplot2::geom_hline(
      yintercept=1,
      linetype='dotdash'
    ) +
    ggplot2::labs(x="Day", y=NULL) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme_classic(base_size=16)
  
  # Add optional titles and labels
  if (!is.null(title)){
    p <- p + ggplot2::labs(title=title)
  }
  if (!is.null(subtitle)){
    p <- p + ggplot2::labs(subtitle=subtitle)
  }
  if (!is.null(caption)){
    p <- p + ggplot2::labs(caption=caption)
  }
  
  # Use a user-defined max y value (for comparison across models)
  if (!is.null(ylim)){
    p <- p + ggplot2::coord_cartesian(ylim = ylim)
  }
  
  return(p)
}