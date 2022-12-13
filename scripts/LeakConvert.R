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

####################
### Leak Convert ### 
####################
### Alluvial ###
colors <- rev(hcl.colors(2, "set 2"))
dt.3 <- dt %>%
  dplyr::select(Age, Sex, BMI, Pack.year, Alb, FEV1, Procedure, Lobe, Tumor_size, Year, 
                ope_min, LeakTest_Bubble, PoAL) %>%
  dplyr::mutate(freq = rep(1, nrow(dt))) %>% 
  dplyr::mutate(LeakTest = ifelse(LeakTest_Bubble=="present", "Bubble (+)", "Bubble (-)")) %>% 
  dplyr::mutate(PoAL = ifelse(PoAL==1, "PoAL (+)", "PoAL (-)")) %>% 
  na.omit()
# Seg
dt.seg <- dt.3 %>%
  dplyr::filter(grepl("Segmentectomy",Procedure))
# c(Bubble-PoAL-, Bubble-PoAL+, Bubble+PoAL-, Bubble+PoAL+)
num <- dt.seg %>% 
  dplyr::select(PoAL, LeakTest) %>% table() %>% as.vector() 
num.y <- cumsum(rev(num)) - rev(num)/2
g1 <- ggplot(data = dt.seg,
             aes(axis1 = LeakTest, axis2 = PoAL, y = freq)) +
  geom_alluvium(aes(fill = PoAL),
                curve_type = "cubic") +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("LeakTest", "PoAL"),
                   expand = c(0.15, 0.05)) +
  scale_fill_manual(values = colors) +
  theme_void() + ggtitle("Segmentectomy") + 
  annotate("text", x=1.25, y=num.y[1], label=paste("n=",rev(num)[1],sep =""), color = "red") + 
  annotate("text", x=1.25, y=num.y[2], label=paste("n=",rev(num)[2],sep =""), color = "red") + 
  annotate("text", x=1.25, y=num.y[3], label=paste("n=",rev(num)[3],sep ="")) + 
  annotate("text", x=1.26, y=num.y[4], label=paste("n=",rev(num)[4],sep =""))
# Lob
dt.lob <- dt.3 %>%
  dplyr::filter(grepl("Lobectomy", Procedure))
# c(Bubble-PoAL-, Bubble-PoAL+, Bubble+PoAL-, Bubble+PoAL+)
num <- dt.lob %>% 
  dplyr::select(PoAL, LeakTest) %>% table() %>% as.vector() 
num.y <- cumsum(rev(num)) - rev(num)/2
g2 <- ggplot(data = dt.lob,
             aes(axis1 = LeakTest, axis2 = PoAL, y = freq)) +
  geom_alluvium(aes(fill = PoAL),
                curve_type = "cubic") +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("LeakTest", "PoAL"),
                   expand = c(0.15, 0.05)) +
  scale_fill_manual(values = colors) + 
  theme_void() + ggtitle("Lobectomy") + 
  annotate("text", x=1.25, y=num.y[1], label=paste("n=",rev(num)[1],sep =""), color = "red") + 
  annotate("text", x=1.25, y=num.y[2], label=paste("n=",rev(num)[2],sep =""), color = "red") + 
  annotate("text", x=1.25, y=num.y[3], label=paste("n=",rev(num)[3],sep ="")) + 
  annotate("text", x=1.25, y=num.y[4], label=paste("n=",rev(num)[4],sep =""))
# Seg & Lob
g.allu <- cowplot::plot_grid(g1, g2, nrow = 2)
g.allu

# Bar plot
dt1 <- dt.3 %>% 
  dplyr::filter(LeakTest=="Bubble (+)") %>%
  dplyr::select(Procedure, PoAL, freq)　%>%
  dplyr::group_by(Procedure, PoAL) %>%
  dplyr::summarise(Total = sum(freq)) 
db1 <- data.frame(
  lab = c("Segmentectomy", "Lobectomy"),
  rate = c(
    dt1$Total[1]/(dt1$Total[1] + dt1$Total[2]) * 100,
    dt1$Total[3]/(dt1$Total[3] + dt1$Total[4]) * 100)
)
dt2 <- dt.3 %>% 
  dplyr::filter(LeakTest=="Bubble (-)") %>%
  dplyr::select(Procedure, PoAL, freq)　%>%
  dplyr::group_by(Procedure, PoAL) %>%
  dplyr::summarise(Total = sum(freq)) 
db2 <- data.frame(
  lab = c("Segmentectomy", "Lobectomy"),
  rate = c(
    dt2$Total[2]/(dt2$Total[1] + dt2$Total[2]) * 100,
    dt2$Total[4]/(dt2$Total[3] + dt2$Total[4]) * 100)
)
mytheme <- theme_classic() + 
  theme(axis.title.x=element_blank(), legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  theme(axis.text = element_text(size = 12)) + 
  theme(axis.title = element_text(size = 14))
gb1 <- ggplot(db1, aes(x = lab, y = rate, fill = lab)) + 
  geom_bar(stat = "identity", color = "black") + 
  ylim(0,80) + ggtitle("Bubble (+)") + ylab("Leak cessation rate (% PoAL-)") + 
  scale_fill_manual(values = c("white", "grey")) +
  theme(plot.title = element_text(color = "red")) + 
#  geom_signif(comparisons = list(c("Lobectomy", "Segmentectomy")), 
#              map_signif_level=TRUE, color = "red") + 
  mytheme
gb2 <- ggplot(db2, aes(x = lab, y = rate, fill = lab)) + 
  geom_bar(stat = "identity", color = "black") + 
  ylim(0,80) + ggtitle("Bubble (-)") + ylab("Leak commencement rate (% PoAL+)") + 
  scale_fill_manual(values = c("white", "grey")) +
#  geom_signif(comparisons = list(c("Lobectomy", "Segmentectomy")), 
#              map_signif_level=TRUE, color = "red") + 
  mytheme
g.bar <- cowplot::plot_grid(gb1, gb2, nrow = 1)
g.bar
# Alluviate + Bar
#g.conv <- cowplot::plot_grid(g.allu, g.bar, nrow = 1, rel_widths  = c(1,.7))
g.conv <- cowplot::plot_grid(
  g.allu + theme(plot.margin = unit(c(.5, .5, .5, .5), "cm")),
  g.bar + theme(plot.margin = unit(c(.5, .5, .5, .5), "cm")), 
  nrow = 1, rel_widths  = c(1,.7))
g.conv
ggsave(paste(OutDir, "LeakConvrt.jpg", sep=""), width = 8, height = 5)
