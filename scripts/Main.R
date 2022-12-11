setwd("~/Documents/zeropita/git.v1/") # Directory to run analysis
source("./scr/Load.Libraries.R")

# Load data
Leak.cutoff <- 0 # Threshhold for PoAL [ml/min]
file <- "./data/data.v1.csv"
source("./scr/Read.Data.R")
d2 <- read.Data(file = file, Leak.cutoff = Leak.cutoff)

OutDir <- "results/" # Directory to save outputs

### A bit reshape of data
dt.pre <- d2 %>% 
  mutate(Pack.year = BI/20.0) %>%
  mutate(Diagnosis = ifelse(Diagnosis == "LK", "Lung carcinoma", "Metastasis/Benign"))
dt <- dt.pre %>%
  na.omit()
paste("Number of case without complete clinical information:", nrow(dt.pre)-nrow(dt))
print("What varibles contained incomplete information?")
is.na(dt.pre) %>% apply(., 2, summary)


#####################
### Main analysis ###
#####################
# Overview, By Leak
mygt <- dt %>% 
  dplyr::select(Age, Sex, BMI, Pack.year, FEV1, DM, IVD, Alb,  
                Tumor_size, Diagnosis, Lobe, Approach, Procedure, 
                Adhesion, IncompleteFissure, ope_min, Bleeding, LeakTest_Bubble, PGA, fibrin, 
                drain_remove, PAL, Pleurodesis, ReOpe_ReDrain, Discharge, PoAL) %>%
  tbl_summary(by = PoAL,
              type = c(IVD, DM)~ "categorical",
              digit = list(c(Age,BMI,Pack.year,FEV1,ope_min,Bleeding,drain_remove,Discharge) ~ 1)
              ) %>%
  add_overall() %>% 
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Postoperative AirLeak**") %>%
  bold_labels() %>%
  italicize_levels()
mygt
mygt %>% 
  as_flex_table() %>% 
  flextable::save_as_docx(path = paste(OutDir, "Overview_byLeak.docx", sep = ""))

# Multivariate analysis; "PoAL ~ Pre/Intra-operative factors"
res.glm <- glm(
  data = dt %>% 
    dplyr::select(PoAL, 
                  Age, Sex, BMI, Pack.year, Tumor_size, Lobe, Procedure, 
                  Adhesion, IncompleteFissure#, ope_min, Bleeding, LeakTest_Bubble, fibrin
                  ), 
  PoAL~., family=binomial)
summary(res.glm)
mygt.glm <- res.glm %>%
  tbl_regression(
    exponentiate = TRUE, 
    pvalue_fun = ~style_pvalue(.x, digits = 2),
  ) %>% 
  #add_global_p() %>%
  bold_p(t = 0.05) %>%
  bold_labels() %>%
  italicize_levels()
mygt.glm
mygt.glm %>%
  as_flex_table() %>% 
  flextable::save_as_docx(path = paste(OutDir, "glm_Multivar.docx", sep = ""))

