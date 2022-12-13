setwd("./") # Please set directory to run analysis
source("./scr/Load.Libraries.R")

# Load data
file <- "./data/data.mock.csv"
d2 <- read_csv(file = file)
OutDir <- "results/" # Directory to save outputs

### A bit reshape of data
dt <- d2 %>% 
  mutate(Pack.year = BI/20.0) %>%
  mutate(Diagnosis = ifelse(Diagnosis == "LK", "Lung carcinoma", "Metastasis/Benign")) %>%
  na.omit()

#############################
### PoAL scoring model  ###
############################## 

## Pre-process ##
# Decide cutoff value to discretize significant continuous variables (i.e. BMI)
roc.BMI <- roc(response=dt$PoAL, predictor=dt$BMI)
plot(roc.BMI, print.thres = "best", legacy.axes = TRUE)
cutoff.BMI <- pROC::coords(roc.BMI, x="best")$threshold %>% round(., digits = 0)
print(paste("Best cutoff of BMI for PoAL prediction = ", cutoff.BMI))

# Discretize variables 
d.discr <- dt %>% 
  dplyr::mutate(Sex = factor(Sex, levels = c("Female", "Male"))) %>%
  dplyr::mutate(BMI = ifelse(BMI >= 22, "BMI>=22", "BMI<22")) %>%
  dplyr::mutate(BMI = factor(BMI, levels = c("BMI>=22", "BMI<22")))

# Check whether PoAL-predictive factors remain significant after "BMI" discretization
# We will check this by the backward stepwise reguressino model using all (significant + non-significant) variables
res.glm <- glm(data = d.discr %>%
                 dplyr::select(Sex, BMI, Lobe, Procedure, Adhesion, IncompleteFissure, # Significant factors
                               Age, Pack.year, FEV1, DM,IVD, Alb, Diagnosis, Tumor_size, Approach, # Insignificant factors  
                               PoAL), 
               PoAL~., family=binomial)
step.glm <- step(res.glm, trace = F)
summary(step.glm)
## The end of Pre-process ##

############################
# Data prepare for PoAL-prediction model 
d.model <- d.discr %>% 
  dplyr::select(Sex, BMI, Lobe, Procedure, IncompleteFissure, Adhesion, # Robustly significant factors
                PoAL)

#### 5-fold cross validation (PoAL status-stratified)
# parameters
set.seed(999) # For reproducibility
n_fold <- 5
test_rate <- 1/n_fold # Model_construct : Test = 4 : 1
# PoAL(+) subgroup
PoAL.posi <- d.model$PoAL == 1
data_size <- sum(PoAL.posi)
fold_i.posi <- sample(rep(1:n_fold, length.out = data_size))
# PoAL(-) subgroup
data_size <- sum(!PoAL.posi)
fold_i.nega <- sample(rep(1:n_fold, length.out = data_size))
# PoAL(+) + PoAL(-)
fold_i <- rep(NA, nrow(d.model))
fold_i[which(PoAL.posi)] <- fold_i.posi
fold_i[which(!PoAL.posi)] <- fold_i.nega

# Load a package that contains three funcitons #
## FUNCTION1; Make PoAL-prediction score matrix
## FUNCTION2; case-wise sum-up of PoAL-prediction scores
## FUNCTION3; poAL prediction model 
source("./scr/Packages_PoALPredModel.R")

# initialize result table
d.res <- matrix(NA, nrow = n_fold, ncol = ncol(d.model)+5)
### cross validation 
for (i in 1:n_fold) {
  # split data into train data and test data
  test_i <- which(fold_i == i)
  data_test <- d.model[test_i, ]
  data_train <- d.model[-test_i, ]
  # Predict
  res <- predict.PoAL(data_train, data_test)
  d.res[i, ] <- res %>% round(., digits = 2)
  colnames(d.res) <- names(res)
}
d.res <- d.res %>% as.data.frame()
d.res
print("Mean value of PoAL score model's performance in 5-flod cross validation:")
print(paste("mean npv =", d.res$npv %>% mean())) # mean npv
print(paste("mean ppv =", d.res$ppv %>% mean())) # mean ppv
print(paste("mean AUC =", d.res$AUC %>% mean())) # mean AUC

# Show AUC computed in the 5-flod cross validation  
g.auc <- ggplot(
  data = data.frame(
    x = rep("AUC", nrow(d.res)),
    AUC = d.res$AUC),
  aes(x = x, y = AUC)
) + 
  geom_boxplot() + 
  geom_jitter(width = .1) + 
  ylim(0,1) + 
  theme_classic() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
g.auc  
ggsave(paste(OutDir, "AUC.jpg", sep = ""), width = 1.2, height = 3)


###  We now focus on a representative the PoAL scoreing model.1 
N.bestmodel <- 1 # Choose one of models produced in 5-fold validation 
d.coefs.bestmodel <- d.res[N.bestmodel,]
# Here we'll apply the model.1 to the entire cohort (n = 485)
d.score.bestmodel <- make.scoreMat(data = d.model, d.coefs = d.coefs.bestmodel)
d.scoreSum <- sum.scoreMat(d.score = d.score.bestmodel)
# ROC curve: PoAL ~ total score of the model.1
ROC <- roc(response=d.scoreSum$PoAL, predictor=d.scoreSum$score)
print(paste("Gross AUC = ", round(ROC$auc, digits = 1)))
cutoff <- pROC::coords(ROC, x="best",
                       ret=c("threshold", "sensitivity", "specificity", "ppv", "npv"), 
                       best.method="closest.topleft")
cutoff
threshold <- d.res$threshold[N.bestmodel]
idx.below.threshold <- which(d.scoreSum$scoreSum <= threshold)
d.scoreSum[idx.below.threshold, ] %>% dplyr::select(PoAL) %>% table()
df.below.threshold <- dt[idx.below.threshold, ] %>% dplyr::select(PoAL, LeakTest_Bubble) %>% table()
df.below.threshold
# npv
df.below.threshold[1,1]/sum(df.below.threshold[,1])*100

g.roc <- ggroc(ROC, #aes = c("linetype", "color"),
               size = 1, 
               legacy.axes = TRUE) + 
  geom_abline(color = "dark grey", size = 0.5) + # Draw line of AUROC=0.5
  #ggtitle("ROC of PoAL-predicting model") + 
  geom_text(x = 0.5, y = 0.5, 
            label = paste("AUC: ", round(ROC$auc, digits = 2), sep = "")) + 
  theme_classic()
g.roc
ggsave(paste(OutDir, "ROC.jpg", sep=""), width = 4, height = 4)
