plot_adj <- function(subtype_tmp, strat_tmp){
  all_subtypes <- R_SARS$subtype %>% unique %>% sort
  color_id <- which(all_subtypes == subtype_tmp)
  ylim_tmp <- case_when(subtype_tmp == "CV-A16" ~ c(0,60),
                        subtype_tmp == "CV-A6" ~ c(0, 60),
                        subtype_tmp == "EV-A71" ~ c(0,60))
  
  R_SARS %>% 
    filter(incubation_period == 5) %>% 
    pivot_longer(cols = c(R0, R0_adj_LL, R0_adj_UL),
                 names_to = "strat",
                 values_to = "R0") %>%
    mutate(strat = factor(strat,
                          levels = c("R0","R0_adj_LL", "R0_adj_UL"),
                          labels = c("Raw Estimates", 
                                     "Adjusted Estimates\n(Optimistic)",
                                     "Adjusted Estimates\n(Conservative)")),
           year = factor(year)) %>%
    filter(subtype == subtype_tmp,
           strat == strat_tmp) %>% 
    # mutate(year = factor(year, levels = 2011:2018)) %>% 
    ggplot(.,aes(x = year,
                 y = R0,
                 fill = subtype)) +
    geom_boxplot(outlier.shape = NA)+  
    geom_point(position = position_jitterdodge(jitter.width = 0.1),
               alpha = 0.1) +
    # ylim(0,30) +
    coord_cartesian(ylim = ylim_tmp) + 
    # facet_grid(subtype~strat) +
    theme_cowplot() +
    scale_fill_manual(values = subtype_colors[color_id]) +
    theme(panel.border = element_rect(color = "black"),
          strip.background = element_rect(fill = NA, color = "black"),
          axis.text.x = element_text(angle = 90),
          axis.text = element_text(size = 20),
          axis.title = element_text(size = 25),
          legend.text = element_text(size = 20),
          legend.position = "top",
          strip.text = element_text(size = 20)) +
    scale_x_discrete(breaks = c(2011:2018), drop = F) +
    geom_vline(xintercept = 5.5, linetype = 2) +
    labs(x = " ",
         fill = " ") -> p_tmp
  return(p_tmp)
}
