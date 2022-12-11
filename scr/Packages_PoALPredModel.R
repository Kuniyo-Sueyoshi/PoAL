# FUNCTION; Make PoAL-prediction score matrix
make.scoreMat <- function(data = data, d.coefs = d.coefs){
  d.score <- data %>%
    dplyr::mutate(Sex = ifelse(Sex == "Male", d.coefs$SexMale, 0)) %>%
    dplyr::mutate(BMI = ifelse(BMI=="BMI<22", d.coefs$`BMIBMI<22`, 0)) %>%
    dplyr::mutate(Lobe = ifelse(Lobe == "Upper", d.coefs$LobeUpper, 0)) %>%
    dplyr::mutate(Procedure = ifelse(Procedure=="Lobectomy", d.coefs$ProcedureLobectomy, 0)) %>%
    dplyr::mutate(IncompleteFissure = ifelse(IncompleteFissure==1, d.coefs$IncompleteFissure, 0)) %>% 
    dplyr::mutate(Adhesion = ifelse(Adhesion==1, d.coefs$Adhesion, 0))
  return(d.score)
}

### FUNCTION; case-wise sum-up of PoAL-prediction scores
sum.scoreMat <- function(d.score = d.score){
  d.scoreSum <- data.frame(PoAL = d.score$PoAL,
                           scoreSum = apply(
                             X = d.score %>% dplyr::select(-PoAL),
                             MARGIN = 1,
                             FUN = sum
                           ))
  return(d.scoreSum)
}

# FUNCTION; poAL prediction model ##
predict.PoAL <- function(data_train = data_train, data_test = data_test){
  # Training
  model.glm <- glm(data = data_train, 
                   PoAL~., family=binomial)
  coefs <- round(coefficients(model.glm) *2) /2
  #coefs <- round(coefficients(model.glm))
  d.coefs <- coefs %>% t() %>% as.data.frame
  # Prediction (=~ Test)
  d.score <- make.scoreMat(data = data_test, d.coefs = d.coefs)
  d.scoreSum <- sum.scoreMat(d.score = d.score)
  # Determine ScoreSum cutoff level to estimate npv 0.8
  ROC <- roc(response=d.scoreSum$PoAL, predictor=d.scoreSum$scoreSum)
  AUC <- ROC$auc %>% round(digits = 3)
  cutoffs <- pROC::coords(ROC, ret=c("threshold", "sensitivity", "specificity", "ppv", "npv"))
  # cutoff.score <- cutoffs$threshold[cutoffs$npv > .7] %>%  na.omit() %>% max() %>% round(digits = 2)
  cutoff.score <- pROC::coords(ROC,x="best", best.method="closest.topleft")$threshold
  # npv; Negative Predictive value (True non-PoAL cases with the total score below a threshold)
  tbl <- d.scoreSum %>% filter(scoreSum <= cutoff.score) %>% dplyr::select(PoAL)
  npv <- round(sum(tbl$PoAL == 0)/nrow(tbl), digits = 3)
  p.below.threshold = nrow(tbl)/nrow(data_test)
  tbl.p <- d.scoreSum %>% filter(scoreSum > cutoff.score) %>% dplyr::select(PoAL)
  ppv <- round(sum(tbl.p$PoAL == 1)/nrow(tbl.p), digits = 3)
  # Output
  v.out <- c(coefs, threshold = cutoff.score, 
             npv = npv, ppv = ppv, 
             p.below.threshold = p.below.threshold, AUC = AUC)
  return(v.out)
}
