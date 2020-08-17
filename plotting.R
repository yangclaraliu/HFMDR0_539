library(tidyverse)
library(readxl)
library(janitor)
library(ggsci)
library(ggpubr)
library(ggpmisc)

#figure 1
readxl::read_excel("ZhongLiuChang_Data_3.xlsx", 
                   sheet = 7) %>% 
  .[,c(2,5:7)] %>% 
  setNames(c("inc",
             "year",
             "months",
             "virus_type")) %>% 
  mutate(months = factor(months,
                         labels = 1:12),
                        # labels = c("Jan",
                        #            "Feb",
                        #            "Mar",
                        #            "Apr",
                        #            "May",
                        #            "Jun",
                        #            "Jul",
                        #            "Aug",
                        #            "Sep",
                        #            "Oct",
                        #            "Nov",
                        #            "Dec")),
         virus_type = if_else(virus_type == "CoxA16", "CV-A16", virus_type),
         virus_type = if_else(virus_type == "CVA6", "CV-A6", virus_type),
         virus_type = factor(virus_type)) -> tab

tab %>% 
  group_by(year, virus_type) %>% 
  tally() %>% 
  #summarise(n = sum(inc)) %>% 
  ungroup %>% 
  mutate(year = as.numeric(year)) %>% 
  ggplot(., aes(x = year,
                y = n,
                group = virus_type,
                color = virus_type))+
  geom_point() +
  geom_line() +
  theme_bw()+
  scale_color_nejm()+
  theme(legend.position = "none") +
  labs(x = "",
       y = "# Outbreaks",
       color = "") -> p_1

tab %>% 
  group_by(year, virus_type) %>% 
  #tally() %>% 
  summarise(n = sum(inc)) %>% 
  ungroup %>% 
  mutate(year = as.numeric(year)) %>% 
  ggplot(., aes(x = year,
                y = n,
                group = virus_type,
                color = virus_type))+
  geom_point() +
  geom_line() +
  theme_bw()+
  scale_color_nejm()+
  theme(legend.position = "none")+
  labs(x = "Year",
       y = "# Infected Children",
       color = "") -> p_3

tab %>% 
  group_by(year, months, virus_type) %>% 
  tally %>%
  pivot_wider(names_from = virus_type,
              values_from = n) %>% 
  ungroup %>%
  replace(., is.na(.), 0) %>% 
  pivot_longer(`CV-A16`:`CV-A6`) %>%
  filter(!(name == "CVA6" & year <= 2014)) %>% 
  dplyr::select(-year) %>% 
  ggplot(., aes(x = months , 
                y = value,
                color = name)) +
  geom_boxplot() +
  facet_wrap(~name) +
  theme_bw()+
  scale_color_nejm()+
  labs(y = "",
       x = "",
       color = "")-> p_2

tab %>% 
  group_by(year, months, virus_type) %>% 
#  tally %>%
  summarise(n = sum(inc)) %>% 
  pivot_wider(names_from = virus_type,
              values_from = n) %>% 
  ungroup %>%
  replace(., is.na(.), 0) %>% 
  pivot_longer(`CV-A16`:`CV-A6`) %>%
  filter(!(name == "CVA6" & year <= 2014)) %>% 
  dplyr::select(-year) %>% 
  ggplot(., aes(x = months , 
                y = value,
                color = name)) +
  geom_boxplot() +
  facet_wrap(~name) +
  theme_bw()+
  scale_color_nejm()+
  labs(y = "",
       x = "Months",
       color = "")-> p_4

p_1 + theme(axis.text.x = element_text(size = 10)) -> p_1
p_2 + theme(axis.text.x = element_text(size = 10)) -> p_2
p_3 + theme(axis.text.x = element_text(size = 10)) -> p_3
p_4 + theme(axis.text.x = element_text(size = 10)) -> p_4

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

ggsave(plot = p,
       filename = "fig_1.pdf",
       width = 15,
       height = 5,
       useDingbats=FALSE)

ggsave(plot = p,
       filename = "fig_1.png",
       width = 15,
       height = 5)

#figure 2

readxl::read_excel("ZhongLiuChang_Data_3.xlsx", 
                   sheet = 1) %>% 
  setNames(c("virus_type",
             "R0",
             "yr",
             "n_exposed",
             "n_infected",
             "n_duration")) %>% 
  mutate(virus_type = if_else(virus_type == "CoxA16", "CV-A16", virus_type),
         virus_type = if_else(virus_type == "CVA6", "CV-A6", virus_type),
         virus_type = factor(virus_type)) -> tab

R0_threshold = 30



tab %>% 
  filter(R0 <= R0_threshold) %>% 
  ggplot(., aes(x = n_exposed, 
                y = R0,
                color = virus_type)) +
  geom_point()+
  scale_color_nejm()+
  theme_bw()+
  labs(x = "Total Exposed Children",
       y = "R0",
       color = "")+
  geom_smooth(aes(fill = virus_type),
              formula = formula,
              se = F,
              method = "lm")+
  scale_fill_nejm()+
  ylim(0,30)+ 
  stat_fit_glance(label.x = "right", 
                  label.y = "top", 
                  #geom = "text", 
                  method = "lm",
                  #method.args = list(formula = formula),
                  aes(label = paste("P-value = ", 
                                    signif(..p.value.., digits = 2), 
                                    sep = "")))  -> p_1


tab %>% 
  filter(R0 <= R0_threshold) %>% 
  ggplot(., aes(x = n_infected, 
                y = R0,
                color = virus_type)) +
  geom_point()+
  scale_color_nejm()+
  theme_bw()+
  labs(x = "Total Infected Children",
       y = "R0",
       color = "")+
  geom_smooth(aes(fill = virus_type),
              se = F,
              method = "lm")+
  scale_fill_nejm()+
  ylim(0,30)+ 
  stat_fit_glance(label.x = "right", 
                  label.y = "top", 
                  #geom = "text", 
                  aes(label = paste("P-value = ", 
                                    signif(..p.value.., digits = 2), 
                                    sep = ""))) -> p_2

tab %>% 
  filter(R0 <= R0_threshold) %>% 
  ggplot(., aes(x = n_duration, 
                y = R0,
                color = virus_type)) +
  geom_point()+
  scale_color_nejm()+
  theme_bw()+
  labs(y = "R0",
       x = "Outbreak Duration",
       color = "")+
  geom_smooth(aes(fill = virus_type),
              se = F,
              method = "lm")+
  scale_fill_nejm()+
  ylim(0,30)+ 
  stat_fit_glance(label.x = "right", 
                  label.y = "top", 
                  #geom = "text", 
                  aes(label = paste("P-value = ", 
                                    signif(..p.value.., digits = 2), 
                                    sep = "")))  -> p_3

ggarrange(p_1, p_2, p_3, 
          ncol = 3,
          common.legend = T) -> p

ggsave(plot = p,
       filename = "fig_2.pdf",
       width = 15,
       height = 5,
       useDingbats=FALSE)

ggsave(plot = p,
       filename = "fig_2.png",
       width = 15,
       height = 5)

### figure 3
yr <- tab$yr %>% unique %>% sort
vline.level <- yr[ceiling(length(yr)*4/5)]

tab %>% 
  filter(R0 < R0_threshold) %>% 
  mutate(virus_type = if_else(virus_type == "CoxA16", "CV-A16", virus_type),
         virus_type = if_else(virus_type == "CVA6", "CV-A6", virus_type),
         virus_type = factor(virus_type)) %>% 
  # mutate(phase = if_else(yr <= 2016, "Pre-Vaccine Introduction","Post-Vaccine Introduction"),
  #        phase = factor(phase, levels = c("Pre-Vaccine Introduction",
  #                                         "Post-Vaccine Introduction")),
  #        ) %>% 
  ggplot(., aes(x = yr,
                y = R0,
                fill = virus_type)) +
  geom_boxplot(outlier.shape = NA)+
  geom_point(position = position_jitterdodge(jitter.width = 0.05),
             alpha = 0.1)+
  scale_fill_nejm()+
  theme_bw() +
  labs(x = "Phase",
       fill = "")+
  facet_wrap(~virus_type)+
  geom_vline(xintercept = which(yr == vline.level) - 0.5,
             linetype = 2) -> p

ggsave(plot = p,
       filename = "fig_3.pdf",
       width = 15,
       height = 5,
       useDingbats=FALSE)

ggsave(plot = p,
       filename = "fig_3.png",
       width = 15,
       height = 5)

# descriptives
tab %>% 
  mutate(virus_type = "All") %>% 
  bind_rows(tab) %>% 
  pivot_longer(cols = starts_with("n")) %>% 
  ggplot(aes(x = virus_type,
             y = value,
             fill = virus_type))+
  geom_boxplot() +
  facet_wrap(~name,
             scales = "free")+
  theme_bw()+
  scale_fill_manual(values = c("white",(pal_nejm("default")(3))))+
  labs(fill = "") +
  theme(legend.position = "top") -> p

ggsave(plot = p,
       filename = "fig_4.png",
       width = 15,
       height = 5)
