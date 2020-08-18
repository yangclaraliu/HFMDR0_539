R_SARS %>% 
  filter(incubation_period == 5) %>%
  filter(R0 <= 50) %>%
  mutate(ID = as.numeric(ID)) %>% 
  left_join(outbreaks_char %>% mutate(ID = 1:n()), by = c("ID", "subtype")) %>% 
  pivot_longer(cols = c(exposed, infected, duration),
               values_to = "x",
               names_to = "x_metric") %>% 
  
  mutate(x_metric = factor(x_metric,
                           levels = c("exposed", "infected", "duration"),
                           labels = c("Total Exposed Individuals",
                                      "Total Infected Individuals",
                                      "Outbreak Duration"))) %>% 
  ggplot(., aes(x = x, 
                y = R0,
                color = subtype)) +
  geom_point()+
  scale_color_nejm()+
  theme_bw()+
  facet_wrap(~x_metric, scales = "free") +
  geom_smooth(aes(fill = subtype),
              #              formula = formula,
              se = F,
              method = "lm") +
  labs(x = "Outbreak Characteristics") +
  scale_fill_nejm()+
  stat_fit_glance(label.x = "right", 
                  label.y = "top", 
                  #geom = "text", 
                  method = "lm",
                  #method.args = list(formula = formula),
                  aes(label = paste("P-value = ", 
                                    signif(..p.value.., digits = 2), 
                                    sep = ""))) -> p

ggsave(plot = p,
       filename = "figs/fig_5.png",
       width = 15,
       height = 5)
