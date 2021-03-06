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
  # mutate(phase = if_else(year < 2016, "pre", "post")) %>%  
  # group_by(subtype, phase) %>% 
  group_by(subtype) %>% 
  summarise(LL = quantile(R0, 0.25),
            ME = quantile(R0, 0.5),
            UL = quantile(R0, 0.75)) %>% View()
  
R_SARS %>% 
  filter(incubation_period == 5) %>%
  filter(year > 2016) %>% 
  kruskal.test(R0 ~ subtype, data = .)

R_SARS %>% 
  filter(incubation_period == 5) %>% 
  group_by(year, subtype) %>% 
  dplyr::select(R0) %>% 
  # filter(year >= 2017) %>% 
  mutate(phase = if_else(year < 2016, "pre", "post")) %>% 
  group_by(subtype, phase) %>% group_split() -> test # %>% 
  # group_split() 

test %>% 
  map(dplyr::select, subtype, phase) %>% 
  map(distinct) %>% 
  bind_rows()

wilcox.test(test[[5]]$R0, # pre, EV-A71
            test[[6]]$R0) # pre, CV-A16




R_SARS %>% 
  filter(incubation_period == 5,
         subtype == "CV-A6") %>% 
  pull(R0) %>% 
  quantile(., c(0.25,0.5,0.75))

R_SARS %>% 
kruskal.test(R0 ~ subtype, test)





hist(test[[2]]$R0)
hist(test[[6]]$R0)



quantile(test[[1]]$R0, c(0.25,0.5,0.75))
quantile(test[[2]]$R0, c(0.25,0.5,0.75))
quantile(test[[6]]$R0, c(0.25,0.5,0.75))
quantile(test[[5]]$R0, c(0.25,0.5,0.75))

kruskal.test(R0~phase,
             data = test[[3]])

R_SARS %>% 
  filter(incubation_period == 5) %>% 
  pivot_longer(cols = c(R0, R0_adj_LL, R0_adj_UL),
               names_to = "strat",
               values_to = "R0") %>%
  mutate(phase = if_else(year < 2016, "pre", "post")) %>% 
  mutate(strat = factor(strat,
                        levels = c("R0","R0_adj_LL", "R0_adj_UL"),
                        labels = c("Raw Estimates", 
                                   "Adjusted Estimates\n(Optimistic)",
                                   "Adjusted Estimates\n(Conservative)")),
         year = factor(year)) %>% 
  group_by(subtype,
           strat) %>% 
  # summarise(stats1 = median(R0),
  #           stats1 = quantile(R0, 0.25))
  filter(strat == "Raw Estimates") %>% 
  # filter(strat == "Adjusted Estimates\n(Optimistic)") %>% 
  # filter(strat == "Adjusted Estimates\n(Conservative)") %>% 
  filter(subtype == "EV-A71") %>% 
  kruskal.test(R0 ~ phase, data = .)

"OutbreakSummary_20200703.xlsx" %>% 
  read_excel() %>% 
  dplyr::select(`病原学检测结果或排除原因`,
                `学校类型`,
                `发生场所`) %>% 
  setNames(c("serotype",
             "school_type",
             "venue")) %>% 
  group_by(serotype, venue) %>% 
  tally() %>% 
  left_join("OutbreakSummary_20200703.xlsx" %>% 
              read_excel() %>% 
              dplyr::select(`病原学检测结果或排除原因`,
                            `学校类型`,
                            `发生场所`) %>% 
              setNames(c("serotype",
                         "school_type",
                         "venue")) %>% 
              group_by(serotype) %>% 
              tally(),
            by = "serotype") %>% 
  mutate(r = n.x/n.y) %>% 
  dplyr::select(serotype, venue, r) %>% 
  pivot_wider(names_from = venue,
              values_from = r) %>% 
  data.frame() %>% View()
  xlsx::write.xlsx2(x = .,
            file = "Outbreaks_by_School_Type.xlsx")

  "OutbreakSummary_20200703.xlsx" %>% 
    read_excel() %>% 
    dplyr::select("省份",
      `病原学检测结果或排除原因`,
                  `学校类型`,
                  `发生场所`) %>% 
    setNames(c("province",
               "serotype",
               "school_type",
               "venue")) %>% 
    group_by(province, serotype) %>% 
    tally() %>% 
    pivot_wider(names_from = serotype,
                values_from = n) %>% View()

  outbreaks_char %>% 
    group_by(subtype) %>% 
    dplyr::select(subtype, exposed) %>% 
    group_split() -> to_test

  wilcox.test(to_test[[2]]$exposed, to_test[[3]]$exposed)
  wilcox.test(to_test[[2]]$exposed, to_test[[1]]$exposed)
  t.test(to_test[[2]]$exposed, to_test[[3]]$exposed)
  t.test(to_test[[2]]$exposed, to_test[[1]]$exposed)
  
  