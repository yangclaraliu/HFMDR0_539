# Loading packages, data, data wrangling and cleaning
if(!require(pacman)) install.packages("pacman")
p_load(tidyverse, magrittr, readxl, R0, EpiEstim, earlyR, incidence, cowplot, 
       ggsci, lubridate, ggpubr, odin, ggpmisc)

Sys.setlocale("LC_ALL", "Chinese")

outbreaks_raw <- read_excel("epicurves_0806.xlsx", 
                            sheet = 1,
                            col_types = "text") 
colnames(outbreaks_raw)

outbreaks_char <- read_excel("outbreak_characteristics.xlsx") %>% 
  setNames(c("subtype", "R0", "year","exposed","infected","duration")) %>% 
  mutate( subtype = case_when(subtype == "CoxA16" ~ "CV-A16",
                              subtype == "CVA6" ~ "CV-A6",
                              TRUE ~ subtype)) %>% 
  dplyr::select(-R0)

seroprevalence <- read_excel("seroprevalence.xlsx",
                             col_types = "text") %>% 
  pivot_wider(names_from = statistics,
              values_from = seroprevalence) %>% 
  mutate_at(vars(c("Mean", "LL", "UL")),
            as.numeric)

# SARS-CoV method
f <- function(inc, t, r) {((r^(t+1)-1)/(r-1)) - inc}
get_R_SARS <- function(incidence, 
                       t, 
                       incubation){
  uniroot(f, 
          inc = incidence, 
          t = (t-1)/incubation, 
          lower = 0, 
          upper = 50,
          extendInt = "yes") %>% .$root -> root
  return(root)
}

# translate labels
outbreaks_raw %>% 
  dplyr::select("序号", 
                "病原学检测结果或排除原因",
                "首次报告首例病人发病时间",
                "中位年级/年龄",
                "发病数...15",
                # "初始增长期天数",
                # "复件-初始增长期天数",
                starts_with("Day"))  -> tmp

colnames(tmp)[1:5] <- c("ID",
                        "subtype",
                        "time",
                        "Age",
                        "cases")
                        # "IGP",
                        # "IGP_copy")

tmp %>% 
  pivot_longer(starts_with("Day"),
               names_to = "t",
               values_to = "inc") %>% 
  mutate(inc = as.numeric(inc),
         t = parse_number(t),
         # IGP = as.numeric(IGP),
         Age = parse_number(Age)) %>% 
  filter(!is.na(inc)) -> history

rm(tmp)

history %<>% 
  group_by(ID) %>% 
  mutate(peak = rank(desc(inc),
                     ties.method = "first")) %>% 
  filter(peak == 1) %>% 
  rename(IGP = t) %>% 
  dplyr::select(ID, IGP) %>% 
  right_join(history, by = "ID") %>% 
  # filter(t <= IGP) %>% 
  mutate(#cum_inc = sum(inc),
         Age = if_else(Age >=6, 6, Age) %>% as.character,
         subtype = case_when(subtype == "CoxA16" ~ "CV-A16",
                             subtype == "CVA6" ~ "CV-A6",
                             TRUE ~ subtype)) %>%
  left_join(seroprevalence, by = c("Age" = "age",
                                   "subtype"))

history %<>% 
  mutate(excel_date = as.numeric(time),
         excel_date = as.Date(excel_date,
                        origin = "1899-12-30")) %>% 
  separate(time, into = c("date", "hour"), sep = " ", remove = F) %>% 
  mutate(date = ymd(date),
         date = if_else(is.na(date), excel_date, date)) %>% 
  dplyr::select(-time, -excel_date, -hour) %>% 
  mutate(year = year(date),
         month = month(date))

history -> history_full
history %>% 
  filter(t <= IGP) %>% 
  group_by(ID) %>% 
  mutate(cum_inc = sum(inc)) -> history

history %>% 
  dplyr::select(ID, subtype, date, Age, cases, year, month, IGP, cum_inc, Mean, LL, UL) %>% 
  distinct -> outbreaks