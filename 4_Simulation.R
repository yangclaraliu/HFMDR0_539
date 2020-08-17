# odin stochastic discrete
sir_model <- odin::odin({
  ## Core equations for transitions between compartments:
  update(S[]) <- S[i] - n_SE[i]
  update(E[]) <- E[i] + n_SE[i] - n_EI[i]
  update(I[]) <- I[i] + n_EI[i] - n_IR[i]
  update(R[]) <- R[i] + n_IR[i]
  
  ## Individual probabilities of transition:
  p_SE[] <- 1 - exp(-beta * I[i] / N[i])
  p_EI <- 1 - exp(-delta)
  p_IR <- 1 - exp(-gamma)
  
  ## Draws from binomial distributions for numbers changing between
  ## compartments:
  n_SE[] <- rbinom(S[i], p_SE[i])
  n_EI[] <- rbinom(E[i], p_EI)
  n_IR[] <- rbinom(I[i], p_IR)
  
  ## Total population size
  N[] <- S[i] + E[i] + I[i] + R[i]
  
  ## Initial states:
  initial(S[]) <- S_ini
  initial(E[]) <- E_ini
  initial(I[]) <- I_ini
  initial(R[]) <- R_ini
  
  ## User defined parameters - default in parentheses:
  S_ini <- user(999)
  E_ini <- user(0)
  I_ini <- user(1)
  R_ini <- user(0)
  beta <- user(0.2)
  gamma <- user(0.1)
  delta <- user(0.2)
  
  ## Number of replicates
  nsim <- user(100)
  dim(N) <- nsim
  dim(S) <- nsim
  dim(E) <- nsim
  dim(I) <- nsim
  dim(R) <- nsim
  dim(p_SE) <- nsim
  dim(n_SE) <- nsim
  dim(n_EI) <- nsim
  dim(n_IR) <- nsim
})

R0_range <- seq(2,10,0.5)/7
R0_estimates <- list()
sim <- list()

data.frame(seroprevalence = seq(0.3, 0.7, 0.1)) %>% 
  mutate(R_ini = 1000*seroprevalence,
         I_ini = 1,
         S_ini = 1000 - R_ini - I_ini) -> init_cond

for(m in 1:nrow(init_cond)){
  print(paste0("Initial condition #: ", m,"."))
  for(i in 1:length(R0_range)){
    print(paste0("R0 range # ", 100*round(i/length(R0_range), 2), "%."))
    x <- sir_model(S_ini = init_cond$S_ini[m],
                   I_ini = init_cond$I_ini[m],
                   R_ini = init_cond$R_ini[m],
                   nsim = 5000,
                   gamma = 1/7,
                   beta = R0_range[i]*1/7)
    res <- x$run(0:100)
    
    res %>% 
      data.table::as.data.table() %>% 
      gather(key = "c[sim]",
             value = "value",
             -step) %>% 
      separate(`c[sim]`, into = c("c", "sim"), sep = "\\[") %>% 
      mutate(sim = gsub("\\]","",sim) %>% as.numeric) %>% 
      pivot_wider(names_from = c,
                  values_from = value) %>% 
      group_by(sim) %>% 
      mutate(S_min = min(S),
             inc = init_cond$S_ini[m] + 1 - S) %>% 
      mutate(inc = lag(S),
             inc = if_else(is.na(inc), init_cond$S_ini[m], inc),
             inc = inc-S,
             inc_rk = rank(desc(inc), ties.method = "first"),
             cum_inc = init_cond$S_ini[m] + 1 - S) %>% 
      filter(inc_rk == 1) -> param
    
    sapply(1:nrow(param), function (j){
      get_R_SARS(incidence = param$cum_inc[j],
                 t = param$step[j]+1,
                 incubation = 5)
    }) %>% 
      data.frame(R0 = .) -> R0_estimates[[i]]
  }
  R0_estimates -> sim[[m]]
}

rm(i,j,m)
p_list <- list()
for(i in 1:nrow(init_cond)){
  sim[[i]] %>% 
    bind_rows(.id = "range") %>% 
    mutate(range = as.numeric(range)) %>% 
    group_by(range) %>% 
    summarise(R0_mean = mean(R0)/(1-init_cond$seroprevalence[i]))  %>% 
    mutate(R0_true = R0_range*7) %>% 
    ggplot(., aes(x = R0_true,
                  y = R0_mean))+
    geom_point() +
    geom_smooth(method = "lm", se = F) +
    theme_bw() +
    xlim(0,10) +
    ylim(0,10) +
    theme(panel.grid = element_blank(),
          title = element_text(size = 20)) +
    labs(x = "R0 (true)",
         y = "R0 (estimated)",
         title = paste0("Seroprevalence = ", init_cond$seroprevalence[i])) +
    geom_abline(aes(slope = 1,
                    intercept = 0)) -> p_list[[i]]
}


ggsave(plot = do.call(ggpubr::ggarrange,  p_list),
       filename = "figs/simulation_results.png",
       width = 20,
       height = 15)
