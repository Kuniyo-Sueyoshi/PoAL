# PoAL (postoperative air leak) analysis
The current repository includes codes used in the study entitled "Postoperative air leak prediction for early-drain removal: a retrospective study".

# Contents
1. scripts/ (Main analyses and scripts)
   - Main.R
    Main alalysis to determine PoAL-associated factors. (Related to Table 1, Table 2, Table 3)
   - LeakConvert.R
    Calculate and illustrate the air leak cessation rate and leak commencement rate. (Related to Figure 1)
   - ModelPrediction.R
     
2. data/ (Expression data, external data, etc. to execute analyses)
   - TCGA/
3. scr/ (Supplementary tables provided in the study, Input/Output tables in the analyses)
   - TableS5_IPA_Regulators_KIRC_Stroma.txt
4. results/ ()



# Tested environment
```R
R> sessionInfo()

```
R version 3.6.3 (2020-02-29)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: OS X  12.2.1

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

locale:
[1] ja_JP.UTF-8/ja_JP.UTF-8/ja_JP.UTF-8/C/ja_JP.UTF-8/ja_JP.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] ggsignif_0.6.4    cowplot_1.1.1     ggalluvial_0.12.3 pROC_1.18.0       MASS_7.3-58.1    
 [6] forcats_0.5.2     stringr_1.4.1     dplyr_1.0.10      purrr_0.3.5       readr_2.1.3      
[11] tidyr_1.2.1       tibble_3.1.8      ggplot2_3.4.0     tidyverse_1.3.2   flextable_0.8.3  
[16] gtsummary_1.6.2  

loaded via a namespace (and not attached):
 [1] httr_1.4.4          bit64_4.0.5         vroom_1.6.0         jsonlite_1.8.3     
 [5] modelr_0.1.10       assertthat_0.2.1    googlesheets4_1.0.1 cellranger_1.1.0   
 [9] gdtools_0.2.3       pillar_1.8.1        backports_1.4.1     glue_1.6.2         
[13] uuid_1.1-0          digest_0.6.30       rvest_1.0.3         colorspace_2.0-3   
[17] htmltools_0.5.3     plyr_1.8.8          pkgconfig_2.0.3     broom_1.0.1        
[21] haven_2.5.1         scales_1.2.1        officer_0.4.4       tzdb_0.3.0         
[25] timechange_0.1.1    googledrive_2.0.0   farver_2.1.1        generics_0.1.3     
[29] ellipsis_0.3.2      withr_2.5.0         cli_3.4.1           magrittr_2.0.3     
[33] crayon_1.5.2        readxl_1.4.1        evaluate_0.18       fs_1.5.2           
[37] fansi_1.0.3         broom.helpers_1.9.0 xml2_1.3.3          tools_3.6.3        
[41] data.table_1.14.6   hms_1.1.2           gargle_1.2.1        lifecycle_1.0.3    
[45] munsell_0.5.0       reprex_2.0.2        zip_2.2.2           compiler_3.6.3     
[49] systemfonts_1.0.2   rlang_1.0.6         grid_3.6.3          gt_0.8.0           
[53] rstudioapi_0.14     labeling_0.4.2      base64enc_0.1-3     rmarkdown_2.18     
[57] gtable_0.3.1        DBI_1.1.3           R6_2.5.1            lubridate_1.9.0    
[61] knitr_1.41          bit_4.0.5           fastmap_1.1.0       utf8_1.2.2         
[65] stringi_1.7.8       parallel_3.6.3      Rcpp_1.0.9          vctrs_0.5.1        
[69] dbplyr_2.2.1        tidyselect_1.2.0    xfun_0.35


# Author of this repository
* Kuniyo Sueyoshi
* Department of Thoracic Surgery, St Luke's International Hospital, Akashi-Cho 9-1, Chuo-ku, Tokyo, Japan
* 103011ms@gmail.com

# License
Please site the artile; "..."
