## Basic reproduction number of Enterovirus 71, Coxsackievirus A16 and A6: evidence from outbreaks of hand, foot and mouth disease in China between 2011 and 2018

Zhong Zhang<sup>1,2</sup>, Yang Liu<sup>3</sup>, Fengfeng Liu<sup>4</sup>, Minrui Ren<sup>4</sup>, Taoran Nie<sup>4</sup>, Jinzhao Cui<sup>4</sup>, Zhaorui Chang<sup>4</sup>, Zhongjie Li<sup>4</sup>  

<sup>1</sup> Nanjing Municipal Center for Disease Control and Prevention, Nanjing, 210003, China  
<sup>2</sup> Chinese Field Epidemiology Training Program (CFETP), Beijing, 100050, China  
<sup>3</sup> Centre for Mathematical Modelling of Infectious Diseases, Department of Infectious Disease Epidemiology, Faculty of Epidemiology and Population Health, London School of Hygiene & Tropical Medicine  
<sup>4</sup> Division of Infectious disease, Key Laboratory of Surveillance and Early Warning on Infectious Disease，Chinese Center for Disease Control and Prevention，Beijing 102206，China  

#### Overview
`0_LoadData.R` loads and cleans data.  
`1_Temporal_Descriptives.R` explores within and between year seasonality (Figure 3).  
`2_SARS_Methods.R` calculates R<sub>0</sub> using the method from Choi and Pak [https://jech.bmj.com/content/57/10/831] (Figure 4). Age-stratified results and sensitivity analyses around incubation periods are also generated here.  
`3_ALL_R_Comparison.R` compares R<sub>0</sub> and R<sub>e</sub>.  
`4_Simulation.R` validates methods usedin `2_SARS_Methods.R`.  
`5_Confounding.R` explores residual confounding (Figure 5).  
`6_Inline_Stats.r` calculates statistics presented via inline texts (e.g., Whitney-Mann U tests, Kruskal-Wallis tests); generates result tables in the appendix.  

#### Dependencies
**R Packages:**  
[cowplot](https://github.com/wilkelab/cowplot), 
[earlyR](https://www.repidemicsconsortium.org/earlyR/),
[EpiEstim](https://cran.r-project.org/web/packages/EpiEstim/EpiEstim.pdf),
[ggpmisc](https://cran.r-project.org/web/packages/ggpmisc/index.html),
[ggpubr](https://cran.r-project.org/web/packages/ggpubr/index.html),
[ggsci](https://nanx.me/ggsci/articles/ggsci.html),
[incidence](https://cran.r-project.org/web/packages/incidence/vignettes/overview.html),
[lubridate](https://lubridate.tidyverse.org/),
[magrittr](https://cran.r-project.org/web/packages/magrittr/index.html),
[odin](https://cran.r-project.org/web/packages/odin/readme/README.html),
[readxl](https://readxl.tidyverse.org/),
[R0](https://cran.r-project.org/web/packages/R0/R0.pdf),
[tidyverse](https://dplyr.tidyverse.org/).   
