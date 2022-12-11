read.Data <- function(file = file, Leak.cutoff = Leak.cutoff){
  print(paste("Loadng", file))
  d <- read_csv(file) %>% 
    #  column_to_rownames("opeID") %>% 
    dplyr::select(Inclusion, why_excluded, Age, Sex, BMI, BI, Alb, SVC_VC, FEV1_0, DLCO_DLCO, PMH,DM,IVD, 
                  Approach, Procedure, Lobe, side, n.seg, ope_min, Bleeding, Adhesion, IncompleteFissure,
                  neoveil, reinforce, fibrin, path2, Tumor_size, Ope_date,
                  suture, LeakTest_Fi, LeakTest1, 
                  Leak.Max.vol, 
                  #Leak.3hAfter.vol, Leak.3hBefore.vol, fluid.3h.27h, 
                  Pleurodesis, PAL, ReOpe_ReDrain, drain_remove, Discharge,
    ) %>% 
    dplyr::mutate(Sex = factor(Sex, levels = c("Female","Male"))) %>%
    dplyr::mutate(PGA = ifelse(neoveil==1|reinforce==1, 1, 0)) %>%
    dplyr::mutate(Year = sapply(str_split(Ope_date, pattern = "/"), 
                                function(s)s[[1]]) %>% as.factor()) %>%
    dplyr::mutate(Diagnosis = ifelse(path2=="MALT", "Benign", path2)) %>% 
    dplyr::mutate(Diagnosis = factor(Diagnosis, levels = c("Benign", "Meta", "LK"))) %>%
    dplyr::mutate(Procedure = factor(Procedure, levels = c("Segmentectomy", "Lobectomy"))) %>%
    dplyr::mutate(Lobe = ifelse(Lobe=="Upper"|Lobe=="Multi", "Upper", "LowMid")) %>%
    dplyr::mutate(FEV1 = FEV1_0) %>%
    dplyr::mutate(LeakTest_Bubble = LeakTest_Fi) %>%
    dplyr::mutate(PoAL = ifelse(Leak.Max.vol<=Leak.cutoff, 0, 1)) %>% 
    dplyr::mutate(N.seg = ifelse(
      n.seg<=2, "1or2", ifelse(
        n.seg == 3, "3", ifelse(
          n.seg>=4, "4-", "Unknown"
        )))) %>%
    dplyr::mutate(across(.cols = c("BMI","BI","ope_min"), .fns = ~{as.numeric(.)})) %>%
    dplyr::mutate(across(.cols = c("PAL","Pleurodesis","ReOpe_ReDrain","Inclusion","LeakTest1","suture"),
                         .fns = ~{replace_na(., 0)})) %>%
    dplyr::mutate(across(.cols = c("DM","IVD","Adhesion","IncompleteFissure","LeakTest_Bubble","PAL"),
                         .fns = ~{ifelse(.==1, "present", "absent")})) %>%
    dplyr::mutate(across(.cols = c("PGA", "fibrin"),
                         .fns = ~{ifelse(.==1, "used", "unused")})) %>%
    dplyr::mutate(across(.cols = c("Pleurodesis", "ReOpe_ReDrain"),
                         .fns = ~{ifelse(.==1, "required", "not-required")}))
    
  

  # Inclusion
  l.in1 <- grep("Segmentectomy|Lobectomy", d$Procedure)
  l.in2 <- grep("VATS|RATS", d$Approach)
  l.in3 <- grep("Else", d$path2, invert = TRUE)
  l.in <- intersect(l.in1, l.in2) %>% intersect(l.in3)
#  print(paste("Inclusded cases:", length(l.in)))
  
  # Exclusion
  l.ex <- which(!d$Inclusion==1)
#  print(paste("Excluded cases:", d$why_excluded[intersect(l.in, l.ex)]))
  l.select <- setdiff(l.in, l.ex)
  
  d2 <- d[l.select, ] %>%
    dplyr::select(-Inclusion, -why_excluded, -SVC_VC, -DLCO_DLCO, -PMH, -side, 
                  -neoveil, -reinforce, -path2, -Ope_date, -suture,
                  -Leak.Max.vol, -LeakTest1, -n.seg, -FEV1_0)
}
