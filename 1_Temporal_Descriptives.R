outbreaks %>% 
  group_by(year, subtype) %>% 
  tally() %>% 
  ungroup %>% 
  mutate(year = as.numeric(year)) %>% 
  ggplot(., aes(x = year,
                y = n,
                group = subtype,
                color = subtype))+
  geom_point() +
  geom_line() +
  theme_bw()+
  scale_color_nejm()+
  theme(legend.position = "none") +
  labs(x = "",
       y = "# Outbreaks",
       color = "") -> p_1

outbreaks %>% 
  group_by(year, subtype) %>% 
  mutate(cases = as.numeric(cases)) %>% 
  summarise(n = sum(cases)) %>% 
  ungroup %>% 
  mutate(year = as.numeric(year)) %>% 
  ggplot(., aes(x = year,
                y = n,
                group = subtype,
                color = subtype))+
  geom_point() +
  geom_line() +
  theme_bw()+
  scale_color_nejm()+
  theme(legend.position = "none")+
  labs(x = "Year",
       y = "# Infected Children",
       color = "") -> p_3

outbreaks %>% 
  group_by(year, month, subtype) %>% 
  tally() %>% 
  pivot_wider(names_from = subtype,
              values_from = n) %>% 
  ungroup %>% 
  mutate(`CV-A16` = replace(`CV-A16`, is.na(`CV-A16`), 0),
         `EV-A71` = replace(`CV-A16`, is.na(`EV-A71`), 0),
         `CV-A6` = replace(`CV-A16`, is.na(`CV-A6`), 0),
         month = factor(month, levels = 1:12)) %>% 
  pivot_longer(`CV-A16`:`CV-A6`) %>%
  filter(!(name == "CV-A6" & year <= 2014)) %>% 
  dplyr::select(-year) %>% 
  ggplot(., aes(x = month , 
                y = value,
                color = name)) +
  geom_boxplot() +
  facet_wrap(~name) +
  theme_bw()+
  scale_color_nejm()+
  labs(y = "",
       x = "",
       color = "") -> p_2

outbreaks %>% 
  group_by(year, month, subtype) %>% 
  mutate(cases = as.numeric(cases)) %>% 
  summarise(n = sum(cases)) %>% 
  pivot_wider(names_from = subtype,
              values_from = n) %>% 
  ungroup %>%
  mutate(`CV-A16` = replace(`CV-A16`, is.na(`CV-A16`), 0),
         `EV-A71` = replace(`CV-A16`, is.na(`EV-A71`), 0),
         `CV-A6` = replace(`CV-A16`, is.na(`CV-A6`), 0),
         month = factor(month, levels = 1:12)) %>% 
  pivot_longer(`CV-A16`:`CV-A6`) %>%
  filter(!(name == "CVA6" & year <= 2014)) %>% 
  dplyr::select(-year) %>% 
  ggplot(., aes(x = month, 
                y = value,
                color = name)) +
  geom_boxplot() +
  facet_wrap(~name) +
  theme_bw()+
  scale_color_nejm()+
  labs(y = "",
       x = "Months",
       color = "") -> p_4

p_1 + theme(axis.text.x = element_text(size = 10)) -> p_1
p_2 + theme(axis.text.x = element_text(size = 10), strip.background = element_rect(fill = NA)) -> p_2
p_3 + theme(axis.text.x = element_text(size = 10)) -> p_3
p_4 + theme(axis.text.x = element_text(size = 10), strip.background = element_rect(fill = NA)) -> p_4

ggarrange(p_1, 
          p_2, 
          p_3, 
          p_4,
          widths = c(1,3,1,3),
          common.legend = T,
          legend = "top",
          labels = c("a",
                     "b",
                     "c",
                     "d"),
          font.label = list(size = 10, color = "black")) -> p

# ggsave(plot = p,
#        filename = "figs/fig_2.png",
#        width = 15,
#        height = 5)
