R_SARS %>% 
  filter(incubation_period == 5) %>% 
  group_by(year, subtype) %>% 
  summarise(R0_25 = quantile(R0, 0.25),
          med = quantile(R0, 0.5),
          R0_75 = quantile(R0, 0.75),
    LL_25 = quantile(R0_adj_LL, 0.25),
    LL = quantile(R0_adj_LL, 0.5),
    LL_75 = quantile(R0_adj_LL, 0.75),
    
    UL_25 = quantile(R0_adj_UL, 0.25),
    UL = quantile(R0_adj_UL, 0.5),
    UL_75 = quantile(R0_adj_UL, 0.75)) %>% 
  mutate(R0 = paste0(round(med,2), "[",round(R0_25,2),", ",round(R0_75,2),"]")) %>% 
  dplyr::select(-c(med,R0_25,R0_75)) %>% 
  mutate(LL_est = paste0(round(LL,2), "[",round(LL_25,2),", ",round(LL_75,2),"]")) %>% 
  dplyr::select(-c(LL,LL_25,LL_75)) %>% 
  mutate(UL_est = paste0(round(UL,2), "[",round(UL_25,2),", ",round(UL_75,2),"]")) %>% 
  dplyr::select(-c(UL,UL_25,UL_75)) %>% 
  mutate(LL_est = if_else(subtype != "CV-A16", as.character(NA), LL_est)) %>% 
  pivot_longer(cols = c(R0, LL_est, UL_est)) %>% 
  filter(!is.na(value)) %>% 
  mutate(name = substr(name,1,2),
         cn = paste(subtype, name)) %>% 
  dplyr::select(-subtype, -name) %>% 
  pivot_wider(names_from = cn,
              values_from = value) %>% 
  write.csv(., file = "results/R0_table.csv")

R_SARS %>% 
  filter(incubation_period == 5) %>% 
  group_by(year, subtype) %>% 
  dplyr::select(R0) %>% 
  filter(year >= 2017) %>% 
  # mutate(phase = if_else(year < 2016, "pre", "post")) %>% 
  group_by(subtype) -> test # %>% 
  # group_split() 
  
R_SARS %>% 
  filter(incubation_period == 5,
         subtype == "CV-A6") %>% 
  pull(R0) %>% 
  quantile(., c(0.25,0.5,0.75))

kruskal.test(R0 ~ subtype, test)


test %>% 
  map(dplyr::select, subtype, phase) %>% 
  map(distinct) %>% 
  bind_rows()

hist(test[[2]]$R0)
hist(test[[6]]$R0)

wilcox.test(test[[1]]$R0, # pre, EV-A71
       test[[2]]$R0) # pre, CV-A16

quantile(test[[1]]$R0, c(0.25,0.5,0.75))
quantile(test[[2]]$R0, c(0.25,0.5,0.75))
quantile(test[[6]]$R0, c(0.25,0.5,0.75))
quantile(test[[5]]$R0, c(0.25,0.5,0.75))

kruskal.test(R0~phase,
             data = test[[3]])

