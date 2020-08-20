pdfSI = R0::generation.time("weibull", c(3.7, 2.6))
res <- list()
for(i in 1:539){
  skip_to_next <- F
  tryCatch({
    history %>%
      ungroup %>% 
      mutate(ID = as.numeric(ID)) %>% 
      filter(ID == i, t <= IGP) %>% 
      pull(inc) %>% 
      estimate.R(.,
                 methods=c("EG", "ML"),
                 pdfSI) %>% 
      .$estimates %>% 
      unlist() %>% 
      .[grepl(".R$|conf.int1|conf.int2", names(.))] %>% 
      as_tibble() %>% 
      mutate(ID = i) -> res[[i]]},
    error = function(e){
      skip_to_next <<- TRUE
    })
  if(skip_to_next){next}
}

do.call(rbind, res) %>% 
  mutate(ID = as.character(ID)) %>% 
  full_join(outbreaks, by = "ID") -> combined

R_Cori <- R_WT <- list()

for(i in 1:539){
  skip_to_next <- F
  tryCatch({
    history_full %>% 
      filter(ID == i) %>% 
      pull(IGP) %>% unique -> w
    
    history_full %>% 
      filter(ID == i) %>% 
      dplyr::select(t, inc) %>% 
      mutate(q = list(rep(t, inc))) %>% 
      dplyr::select(ID,q) %>% 
      distinct() %>% 
      unnest(cols = q) -> tmp
    
    estimate_R(incid = tmp$q,
               method = "non_parametric_si",
               config = make_config(list(
                 si_distr = pdfSI$GT,
                 t_start = 2,
                 t_end = w))) %>% 
      .$R %>% 
      unlist %>% 
      .[c(3,5,11)]  %>% unlist %>% c(., i) -> R_Cori[[i]]
    
    wallinga_teunis(incid = tmp$q, 
                    method = "non_parametric_si",
                    config = list(si_distr = pdfSI$GT, n_sim = 1000,
                                  t_start = 2, t_end = w))%>% 
      .$R %>% 
      unlist %>% 
      .[c(3,5,6)] %>% unlist %>% c(., i) -> R_WT[[i]]},
    error = function(e){
      skip_to_next <<- TRUE
    })
  if(skip_to_next){next}
}

R_Cori %>% 
  do.call(rbind, .) %>% 
  as_tibble %>% 
  setNames(c("CO.R",
             "CO.conf.int1",
             "CO.conf.int2",
             "ID")) -> R_Cori

R_WT %>% 
  do.call(rbind, .) %>% 
  as_tibble %>% 
  setNames(c("WT.R",
             "WT.conf.int1",
             "WT.conf.int2",
             "ID")) -> R_WT

combined %>% 
  mutate(ID = as.numeric(ID)) %>% 
  full_join(R_Cori, by = "ID") %>% 
  full_join(R_WT, by = "ID") -> all_R

write_rds(all_R, "results/all_R.rds")

all_R <- read_rds("results/all_R.rds")

all_R %>% 
  mutate(ID = as.character(ID)) %>% 
  left_join(R_SARS %>% filter(incubation_period == 5), by = c("ID", "cases", 
                                                              "subtype", "Age", 
                                                              "date", "year", 
                                                              "month", "UL","LL", "Mean","cum_inc","IGP")) %>% 
  mutate(EG.verify = if_else(R0 <= EG.conf.int2 & R0 >= EG.conf.int1, T, F),
         ML.verify = if_else(R0 <= ML.conf.int2 & R0 >= EG.conf.int1, T, F),
         CO.verify = if_else(R0 <= EG.conf.int2 & R0 >= CO.conf.int1, T, F),
         EG.verify_LL = if_else(R0_adj_LL <= EG.conf.int2 & R0_adj_LL >= EG.conf.int1, T, F),
         ML.verify_LL = if_else(R0_adj_LL <= ML.conf.int2 & R0_adj_LL >= EG.conf.int1, T, F),
         CO.verify_LL = if_else(R0_adj_LL <= EG.conf.int2 & R0_adj_LL >= CO.conf.int1, T, F),
         EG.verify_UL = if_else(R0_adj_UL <= EG.conf.int2 & R0_adj_UL >= EG.conf.int1, T, F),
         ML.verify_UL = if_else(R0_adj_UL <= ML.conf.int2 & R0_adj_UL >= EG.conf.int1, T, F),
         CO.verify_UL = if_else(R0_adj_UL <= EG.conf.int2 & R0_adj_UL >= CO.conf.int1, T, F)) -> verify_num

length(which(verify_num$EG.verify == T))/length(which(!is.na(verify_num$EG.verify == T)))
length(which(verify_num$ML.verify == T))/length(which(!is.na(verify_num$ML.verify == T)))
length(which(verify_num$CO.verify == T))/length(which(!is.na(verify_num$CO.verify == T)))

length(which(verify_num$EG.verify_LL == T))/length(which(!is.na(verify_num$EG.verify_LL == T)))
length(which(verify_num$ML.verify_LL == T))/length(which(!is.na(verify_num$ML.verify_LL == T)))
length(which(verify_num$CO.verify_LL == T))/length(which(!is.na(verify_num$CO.verify_LL == T)))

length(which(verify_num$EG.verify_UL == T))/length(which(!is.na(verify_num$EG.verify_UL == T)))
length(which(verify_num$ML.verify_UL == T))/length(which(!is.na(verify_num$ML.verify_UL == T)))
length(which(verify_num$CO.verify_UL == T))/length(which(!is.na(verify_num$CO.verify_UL == T)))

all_R %>% 
  mutate(ID = as.character(ID)) %>% 
  left_join(R_SARS %>% filter(incubation_period == 5), by = c("ID", "cases", 
                                                              "subtype", "Age", 
                                                              "date", "year", 
                                                              "month", "UL","LL", "Mean","cum_inc","IGP")) %>% 
  dplyr::select(-c(EG.conf.int1, EG.conf.int2,
                   ML.conf.int1, ML.conf.int2,
                   CO.conf.int1, CO.conf.int2,
                   WT.conf.int1, WT.conf.int2,
                   WT.R)) %>% 
  mutate(R0 = as.numeric(R0)) %>% 
  pivot_longer(cols = c(EG.R, ML.R, CO.R),
               names_to = "y_metric",
               values_to = "y") %>%
  pivot_longer(cols = c(R0, R0_adj_LL, R0_adj_UL),
               names_to = "x_metric",
               values_to = "x") %>% 
  # filter(x <= 30, y <= 30) %>% 
  mutate(x_metric = factor(x_metric,
                        levels = c("R0","R0_adj_LL", "R0_adj_UL"),
                        labels = c("Raw Estimates", 
                                   "Adjusted Estimates\n(Optimistic)",
                                   "Adjusted Estimates\n(Conservacitve)"))) %>% 
  filter(x_metric == "Raw Estimates") %>% 
  ggplot() +
  geom_point(aes(x = x, y = y, color = y_metric), size = 5, alpha = 0.5) +
  scale_x_log10() +
  scale_y_log10() +
  # facet_wrap(~x_metric, ncol = 1) +
  geom_abline(intercept = 0, 
              slope = 1,
              size = 1.2) +
  cowplot::theme_cowplot() +
  ggsci::scale_color_nejm(labels = c("Wallinga & Lipsitch\n(2007)",
                                         "White & Pagano\n(2009)",
                                         "Cori et al.\n(2013)")) +
  labs(x = "R0 - Method in this study",
       y = "Re - Additional Methods",
       color = "") +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        strip.text = element_text(size = 25),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25),
        panel.border = element_rect(color = "black")) -> p

ggsave("figs/All_R.png",
       plot = p,
       width = 20,
       height = 15)
