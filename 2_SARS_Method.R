if(!file.exists("results/R_SARS.rds")){
  sapply(3:7, function(y){
    sapply(1:539, function(x){
      get_R_SARS(incidence = outbreaks %>% dplyr::select(ID, IGP, cum_inc) %>% distinct() %>% pull(cum_inc) %>% .[x], 
                 t = outbreaks %>% dplyr::select(ID, IGP, cum_inc) %>% distinct() %>% pull(IGP) %>% .[x], 
                 incubation = y)
    })
  }) %>% 
    as_tibble %>% 
    rowid_to_column(var = "ID") %>% 
    pivot_longer(cols = starts_with("V"),
                 names_to = "incubation_period",
                 values_to = "R0") %>% 
    mutate(incubation_period = parse_number(incubation_period) + 2) -> R_SARS
  
  # adjustment by sero-prevalence and age
  R_SARS %<>% 
    mutate(incubation_period = factor(incubation_period),
           ID = as.character(ID)) %>%
    left_join(outbreaks, by = "ID") %>%
    mutate(R0_adj_mean = if_else(!is.na(Mean), R0/(1-Mean), as.numeric(NA)),
           R0_adj_LL = if_else(!is.na(LL), R0/(1-LL), as.numeric(NA)),
           R0_adj_UL = if_else(!is.na(UL), R0/(1-UL), as.numeric(NA)),
           R0_adj_LL = if_else(is.na(R0_adj_LL), R0_adj_mean, R0_adj_LL),
           R0_adj_UL = if_else(is.na(R0_adj_UL), R0_adj_mean, R0_adj_UL),
           R0_adj_LL = if_else(is.na(R0_adj_LL), R0, R0_adj_LL),
           R0_adj_UL = if_else(is.na(R0_adj_UL), R0, R0_adj_UL)) %>% 
    dplyr::select(-R0_adj_mean) 
  
  write_rds(R_SARS, 
            path = "results/R_SARS.rds")
} else {R_SARS <- read_rds("results/R_SARS.rds")}

R_SARS %>% 
  # filter(R0 < 50) %>% 
  mutate(year = factor(year),
         incubation_period = paste0("Incubation Period = ", incubation_period, " days.")) %>% 
  ggplot() +
  geom_boxplot(aes(x = year,
                   y = R0,
                   fill = subtype)) +
               #alpha = 0.2,
               #size = 1.2) +
  coord_cartesian(ylim = c(0,50)) +
  #cowplot::theme_cowplot() +  
  theme_bw()+
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm() +
  theme(legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        strip.background = element_rect(fill = "NA"),
        strip.text = element_text(size = 20),
        axis.text.x = element_text(angle = 90),
        legend.position = "bottom",
        panel.grid = element_blank()) +
  labs(fill = "Incubation Period",
       x = "Year",
       y = "R0") +
  facet_grid(subtype~incubation_period) -> p

ggsave(plot = p,
       filename = "figs/SA_IP.png",
       width = 20,
       height = 15)

R_SARS %>% 
  filter(incubation_period == 5) %>% 
  pivot_longer(cols = c(R0, R0_adj_LL, R0_adj_UL),
               names_to = "strat",
               values_to = "R") %>%
  filter(R <= 50) %>% 
  mutate(Age = factor(Age, levels = 0:6,
                      labels = paste0(c(0:5,"6+"), " Years"))) %>% 
  ggplot(.) +
  geom_histogram(aes(x = R, 
                     color = subtype,
                     fill = subtype)) +
  facet_grid(subtype~Age) +
  scale_color_nejm() +
  scale_fill_nejm() +
  theme_bw() +
  labs(x = "R0 estimates", y = "Count", fill = "", color = "") +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA),
        strip.text = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20)) -> p

ggsave(plot = p,
       filename = "figs/SA_Age.png",
       width = 30,
       height = 15)

rm(p)

source("plot_adj.R")

subtype_colors <- ggsci::pal_nejm()(3)
plot_adj("CV-A16","Raw Estimates") -> p1
plot_adj("CV-A16","Adjusted Estimates\n(Optimistic)") -> p2
plot_adj("CV-A16","Adjusted Estimates\n(Conservative)") -> p3
plot_adj("CV-A6","Raw Estimates") -> p4
plot_adj("CV-A6","Adjusted Estimates\n(Optimistic)") -> p5
plot_adj("EV-A71","Raw Estimates") -> p6
plot_adj("EV-A71","Adjusted Estimates\n(Optimistic)") -> p7


plot_grid(
  # first row
  p1 + 
    labs(y = "",
         title = "Raw Estimates") + 
    theme(axis.text.x = element_blank(),
          legend.position = "none") +
    geom_text(aes(x = 0.6, 
                  y = 30, 
                  label = "(a)", 
                  fill = NA), 
              size = 8,
              hjust = 0,
              vjust = 1) +
    ylim(0,30),
  NULL,
  p2 + 
    labs(y = "",
         title = "Adjusted Estimates (Optimistic)") + 
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none") +
    geom_text(aes(x = 0.6, 
                  y = 30, 
                  label = "(b)", 
                  fill = NA), 
              size = 8,
              hjust = 0,
              vjust = 1)+
    ylim(0,30), 
  NULL,
  p3 + 
    labs(y = "",
         title = "Adjusted Estimates (Conservative)") + 
    theme(legend.position = "none",
                            axis.text.y = element_blank()) +
    geom_text(aes(x = 0.6, 
                  y = 30, 
                  label = "(c)", 
                  fill = NA), 
              size = 8,
              hjust = 0,
              vjust = 1)+
    ylim(0,30), 
  # second row
  NULL, NULL, NULL, NULL, NULL,
  # third row
  p4 + 
    labs(title = "Raw Estimates") +
    theme(axis.text.x = element_blank(),
             legend.position = "none") +
    geom_text(aes(x = 0.6, 
                  y = 30, 
                  label = "(d)", 
                  fill = NA), 
              size = 8,
              hjust = 0,
              vjust = 1) +
    ylim(0,30),
  NULL,
  p5 + 
    labs(y = "",
         title = "Adjusted Estimates") + 
    theme(axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            legend.position = "none")+
    geom_text(aes(x = 0.6, 
                  y = 30, 
                  label = "(e)", 
                  fill = NA), 
              size = 8,
              hjust = 0,
              vjust = 1)+
    ylim(0,75),
  NULL,
  NULL,
  # fourth row
  NULL, NULL, NULL, NULL, NULL,
  # fifth row
  p6 + 
    labs(y = "",
         title = "Raw Estimates") + 
    theme(legend.position = "none") +
    geom_text(aes(x = 0.6, 
                  y = 30, 
                  label = "(f)", 
                  fill = NA), 
              size = 8,
              hjust = 0,
              vjust = 1), 
  NULL,
  p7+ 
    labs(y = "",
         title = "Adjusted Estimates") + 
    theme(legend.position = "none",
                           axis.text.y = element_blank())+
    geom_text(aes(x = 0.6, 
                  y = 30, 
                  label = "(g)", 
                  fill = NA), 
              size = 8,
              hjust = 0,
              vjust = 1),
  NULL,
  # get_legend(x),
  align = "hv",
  rel_widths = c(1,-0.1, 1, -0.1,1),
  rel_heights = c(1,-0.15,1,-0.15,1),
  ncol = 5, 
  nrow = 5) -> p 

ggsave(plot = p,
       filename = "figs/fig_4.png",
       width = 15,
       height = 15)
