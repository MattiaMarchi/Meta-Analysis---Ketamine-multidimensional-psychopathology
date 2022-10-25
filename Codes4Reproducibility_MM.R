###----------------------------------------------------------------------------------------
###---------------Meta Analysis - Ketamine multidimensional psychopathology----------------
###----------------------------------------------------------------------------------------
library(meta)
library(metafor)
library(tidyverse)
library(metaforest)

###----------------------------------------------------------------------------------------
###-----------------------------1. Meta Analysis of Anxiety--------------------------------
###----------------------------------------------------------------------------------------
#Import data
Anx_data <- structure(list(Author = structure(1:6, .Label = c("Anderson et al. 2017", "Domany et al. 2020", "Fedgchin et al. 2019", "Murrough et al. 2015", "Price et al. 2014", "Taylor et al. 2018"), class = "factor"),
                           Outcome = structure(c(3L, 2L, 4L, 1L, 6L, 5L), .Label = c("Anxiety CAST Subscale","BAI", "CAS", "GAD-7", "LSAS", "STAI-S"), class = "factor"),
                           Diagnosis = structure(c(1L, 3L, 2L, 5L, 2L, 4L), .Label = c("Bipolar/Unipolar Depression (moderate or severe)", "MDD", "MDD, bipolar depression, depression NOS, dysthymia", "Social Anxiety Disorder", "Transdiagnostic"), class = "factor"),
                           Time.point = structure(c(1L, 3L, 1L, 4L, 2L, 2L), .Label = c("1 month", "24h", "3 days", "7 days"), class = "factor"), Nt = c(33L, 9L, 115L, 12L, 36L, 18L), Mt = c(4.96, 3.75, 5.8, 6, 42.31, 66.1), SDt = c(4.62, 1.64, 5.94, 2.6, 16.31, 30.9), Nc = c(37L, 9L, 113L, 12L, 21L, 17L), Mc = c(4.43, 16.45, 7.2, 8.2, 44.1, 86.1), SDc = c(0.89, 6.79, 6.01, 3.9, 16.33, 30),
                           Ketamine_Dose = structure(c(3L, 2L, 1L, 3L, 3L, 3L), .Label = c("E 56 mg", "K 0.2 mg/kg", "K 0.5 mg/kg"), class = "factor"), Ketamine_Dose_clean = structure(c(3L, 2L, 1L, 3L, 3L, 3L), .Label = c("", "0.2", "0.5"), class = "factor")), row.names = c(NA, -6L), class = "data.frame")
#Perform meta-analysis
mre_ANX <- metacont(n.e = Nt, mean.e = Mt, sd.e = SDt,
                    n.c = Nc, mean.c = Mc, sd.c = SDc,
                    studlab = Author, data = Anx_data,
                    sm = "SMD", method.tau = "DL")
mre_ANX
#forestplot
forest.meta(mre_ANX, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Ketamine", label.c = "Control",
            label.left = "Favours ketamine", label.right = "Favours control")
#--------------------------------subgroup: use tp vs ECT adjuvant
Anx_data$use <- c("ECT", "Tp", "Tp", "Tp", "Tp", "Tp")
Anx_data$use <- as.factor(Anx_data$use)
Anx_data2 <- as.data.frame(Anx_data)
Anx_data2 <- subset(Anx_data2, Anx_data$use != "ECT")
mre_ANXsubg <- metacont(n.e = Nt, mean.e = Mt, sd.e = SDt,
                        n.c = Nc, mean.c = Mc, sd.c = SDc,
                        studlab = Author, data = Anx_data2,
                        sm = "SMD", method.tau = "DL")
mre_ANXsubg
#forestplot
forest.meta(mre_ANXsubg, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Ketamine", label.c = "Control",
            label.left = "Favours ketamine", label.right = "Favours control")

###----------------------------------------------------------------------------------------
###-----------------------------2. Meta Analysis of Cognition------------------------------
###----------------------------------------------------------------------------------------
#----1. Delayed recall
dr_data <- structure(list(Author = structure(c(3L, 8L, 9L, 10L), .Label = c("Alizadeh et al. 2015", 
                                                                            "Anderson et al.", "Anderson et al. 2017", "Chen M et al. 2018", 
                                                                            "Domany et al. 2020", "Dong et al. 2019", "Fernie et al. 2017", 
                                                                            "Keilp et al. 2021", "Loo et al. 2012", "Ochs-Ross et al. 2020", 
                                                                            "Pradhan et al. 2018", "Rasmussen et al. 2014", "Ray-Griffith et al. 2017", 
                                                                            "Sharma et al. 2019", "Shiroma et al. 2020", "Singh et al. 2015", 
                                                                            "Yoosefi et al. 2014", "Zhang et al. 2018", "Zhong et al. 2016", 
                                                                            "Zou et al. 2021"), class = "factor"),
                          Nt = c(33L, 39L, 22L, 72L), Nc = c(37L, 39L, 24L, 65L), Mt = c(6.7, 0.44, 6.7, 7.6), Mc = c(7.26, -0.4, 9.5, 8.3), SDt = c(2.67, 1.7, 0.6, 2.3), SDc = c(2.63, 2.27, 2.2, 2.6)), class = "data.frame", row.names = c(2L,23L, 32L, 46L))
#Perform meta-analysis
mre_dr <- metacont(n.e = Nt, mean.e = Mt, sd.e = SDt,
                   n.c = Nc, mean.c = Mc, sd.c = SDc,
                   studlab = Author, data = dr_data,
                   sm = "SMD", method.tau = "DL")
mre_dr
#forestplot
forest.meta(mre_dr, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Ketamine", label.c = "Control",
            label.left = "Favours ketamine", label.right = "Favours control")
#-----2. Total learning
tl_data <- structure(list(Author = structure(c(3L, 9L, 10L), .Label = c("Alizadeh et al. 2015", 
                                                                        "Anderson et al.", "Anderson et al. 2017", "Chen M et al. 2018", 
                                                                        "Domany et al. 2020", "Dong et al. 2019", "Fernie et al. 2017", 
                                                                        "Keilp et al. 2021", "Loo et al. 2012", "Ochs-Ross et al. 2020", 
                                                                        "Pradhan et al. 2018", "Rasmussen et al. 2014", "Ray-Griffith et al. 2017", 
                                                                        "Sharma et al. 2019", "Shiroma et al. 2020", "Singh et al. 2015", 
                                                                        "Yoosefi et al. 2014", "Zhang et al. 2018", "Zhong et al. 2016", 
                                                                        "Zou et al. 2021"), class = "factor"), Nt = c(33L, 22L, 72L), 
                          Nc = c(37L, 24L, 65L), Mt = c(22.3, 24, 22.5), Mc = c(23.5, 28.1, 24.5), SDt = c(4.7, 7.8, 4.1), SDc = c(5.6, 5.4, 5.3)), class = "data.frame", row.names = c(3L, 31L, 45L))
#Perform meta-analysis
mre_tl <- metacont(n.e = Nt, mean.e = Mt, sd.e = SDt,
                   n.c = Nc, mean.c = Mc, sd.c = SDc,
                   studlab = Author, data = tl_data,
                   sm = "SMD", method.tau = "DL")
mre_tl
#forestplot
forest.meta(mre_tl, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Ketamine", label.c = "Control",
            label.left = "Favours ketamine", label.right = "Favours control")
#-----3. Retention
ret_data <- structure(list(Author = structure(c(3L, 9L), .Label = c("Alizadeh et al. 2015", 
                                                                    "Anderson et al.", "Anderson et al. 2017", "Chen M et al. 2018", 
                                                                    "Domany et al. 2020", "Dong et al. 2019", "Fernie et al. 2017", 
                                                                    "Keilp et al. 2021", "Loo et al. 2012", "Ochs-Ross et al. 2020", 
                                                                    "Pradhan et al. 2018", "Rasmussen et al. 2014", "Ray-Griffith et al. 2017", 
                                                                    "Sharma et al. 2019", "Shiroma et al. 2020", "Singh et al. 2015", 
                                                                    "Yoosefi et al. 2014", "Zhang et al. 2018", "Zhong et al. 2016", 
                                                                    "Zou et al. 2021"), class = "factor"),
                           Nt = c(33L, 22L), Nc = c(37L, 24L), Mt = c(73.3, 85.2), Mc = c(77.1, 85.4), SDt = c(24, 15), SDc = c(16.1, 11.3)), class = "data.frame", row.names = c(4L, 33L))
#Perform meta-analysis
mre_ret <- metacont(n.e = Nt, mean.e = Mt, sd.e = SDt,
                    n.c = Nc, mean.c = Mc, sd.c = SDc,
                    studlab = Author, data = ret_data,
                    sm = "SMD", method.tau = "DL")
mre_ret
#forestplot
forest.meta(mre_ret, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Ketamine", label.c = "Control",
            label.left = "Favours ketamine", label.right = "Favours control")
#-----4. Recognition discrimination
rd_data <- structure(list(Author = structure(c(3L, 8L, 9L), .Label = c("Alizadeh et al. 2015", 
                                                                       "Anderson et al.", "Anderson et al. 2017", "Chen M et al. 2018", 
                                                                       "Domany et al. 2020", "Dong et al. 2019", "Fernie et al. 2017", 
                                                                       "Keilp et al. 2021", "Loo et al. 2012", "Ochs-Ross et al. 2020", 
                                                                       "Pradhan et al. 2018", "Rasmussen et al. 2014", "Ray-Griffith et al. 2017", 
                                                                       "Sharma et al. 2019", "Shiroma et al. 2020", "Singh et al. 2015", 
                                                                       "Yoosefi et al. 2014", "Zhang et al. 2018", "Zhong et al. 2016", 
                                                                       "Zou et al. 2021"), class = "factor"),
                          Nt = c(33L, 39L, 22L), Nc = c(37L, 39L, 24L), Mt = c(9.52, 0.01, 10.3), Mc = c(9.52, 0.24, 11.2), SDt = c(2.33, 1.11, 1.2), SDc = c(2.33, 1.16, 1)), class = "data.frame", row.names = c(5L, 24L, 34L))
#Perform meta-analysis
mre_rd <- metacont(n.e = Nt, mean.e = Mt, sd.e = SDt,
                   n.c = Nc, mean.c = Mc, sd.c = SDc,
                   studlab = Author, data = rd_data,
                   sm = "SMD", method.tau = "DL")
mre_rd
#forestplot
forest.meta(mre_rd, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Ketamine", label.c = "Control",
            label.left = "Favours ketamine", label.right = "Favours control")
#-----5. Letter fluency
lf_data <- structure(list(Author = structure(c(3L, 8L, 9L), .Label = c("Alizadeh et al. 2015", "Anderson et al.", "Anderson et al. 2017", "Chen M et al. 2018", 
                                                                       "Domany et al. 2020", "Dong et al. 2019", "Fernie et al. 2017", 
                                                                       "Keilp et al. 2021", "Loo et al. 2012", "Ochs-Ross et al. 2020", 
                                                                       "Pradhan et al. 2018", "Rasmussen et al. 2014", "Ray-Griffith et al. 2017", 
                                                                       "Sharma et al. 2019", "Shiroma et al. 2020", "Singh et al. 2015", 
                                                                       "Yoosefi et al. 2014", "Zhang et al. 2018", "Zhong et al. 2016", 
                                                                       "Zou et al. 2021"), class = "factor"),
                          Nt = c(33L, 39L, 22L), Nc = c(37L, 39L, 24L), Mc = c(39.5, 0.5, 53.1), Mt = c(35.6, 0.35, 30.6), SDt = c(13.4, 0.87, 11.6), SDc = c(14.1, 1.14, 9.8)), class = "data.frame", row.names = c(6L, 25L, 35L))
#Perform meta-analysis
mre_lf <- metacont(n.e = Nt, mean.e = Mt, sd.e = SDt,
                   n.c = Nc, mean.c = Mc, sd.c = SDc,
                   studlab = Author, data = lf_data,
                   sm = "SMD", method.tau = "DL")
mre_lf
#forestplot
forest.meta(mre_lf, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Ketamine", label.c = "Control",
            label.left = "Favours ketamine", label.right = "Favours control")
#-----6. Category fluency
cf_data <- structure(list(Author = structure(c(3L, 8L, 9L), .Label = c("Alizadeh et al. 2015","Anderson et al.", "Anderson et al. 2017", "Chen M et al. 2018", 
                                                                       "Domany et al. 2020", "Dong et al. 2019", "Fernie et al. 2017", 
                                                                       "Keilp et al. 2021", "Loo et al. 2012", "Ochs-Ross et al. 2020", 
                                                                       "Pradhan et al. 2018", "Rasmussen et al. 2014", "Ray-Griffith et al. 2017", 
                                                                       "Sharma et al. 2019", "Shiroma et al. 2020", "Singh et al. 2015", 
                                                                       "Yoosefi et al. 2014", "Zhang et al. 2018", "Zhong et al. 2016", 
                                                                       "Zou et al. 2021"), class = "factor"),
                          Nt = c(33L, 39L, 22L), Nc = c(37L, 39L, 24L), Mc = c(17.1, 0.08, 25.9), Mt = c(16.5, 0.06, 19.4), SDt = c(5.6, 0.92, 4.5), SDc = c(4.1, 1.14, 4.1)), class = "data.frame", row.names = c(7L, 26L, 36L))
#Perform meta-analysis
mre_cf <- metacont(n.e = Nt, mean.e = Mt, sd.e = SDt,
                   n.c = Nc, mean.c = Mc, sd.c = SDc,
                   studlab = Author, data = cf_data,
                   sm = "SMD", method.tau = "DL")
mre_cf
#forestplot
forest.meta(mre_cf, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Ketamine", label.c = "Control",
            label.left = "Favours ketamine", label.right = "Favours control")
#-----7. AMI-SF
amisf_data <- structure(list(Author = structure(c(3L, 9L), .Label = c("Alizadeh et al. 2015", "Anderson et al.", "Anderson et al. 2017", "Chen M et al. 2018", 
                                                                      "Domany et al. 2020", "Dong et al. 2019", "Fernie et al. 2017", 
                                                                      "Keilp et al. 2021", "Loo et al. 2012", "Ochs-Ross et al. 2020", 
                                                                      "Pradhan et al. 2018", "Rasmussen et al. 2014", "Ray-Griffith et al. 2017", 
                                                                      "Sharma et al. 2019", "Shiroma et al. 2020", "Singh et al. 2015", 
                                                                      "Yoosefi et al. 2014", "Zhang et al. 2018", "Zhong et al. 2016", 
                                                                      "Zou et al. 2021"), class = "factor"),
                             Nt = c(33L, 22L), Nc = c(37L, 24L), Mc = c(35.4, 40), Mt = c(35.1, 38.5), SDt = c(10, 9.9), SDc = c(10.4, 11.6)), class = "data.frame", row.names = c(8L, 39L))
#Perform meta-analysis
mre_amisf <- metacont(n.e = Nt, mean.e = Mt, sd.e = SDt,
                      n.c = Nc, mean.c = Mc, sd.c = SDc,
                      studlab = Author, data = amisf_data,
                      sm = "SMD", method.tau = "DL")
mre_amisf
#forestplot
forest.meta(mre_amisf, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Ketamine", label.c = "Control",
            label.left = "Favours ketamine", label.right = "Favours control")
#-----8. Copy
copy_data <- structure(list(Author = structure(c(3L, 9L), .Label = c("Alizadeh et al. 2015", "Anderson et al.", "Anderson et al. 2017", "Chen M et al. 2018", 
                                                                     "Domany et al. 2020", "Dong et al. 2019", "Fernie et al. 2017", 
                                                                     "Keilp et al. 2021", "Loo et al. 2012", "Ochs-Ross et al. 2020", 
                                                                     "Pradhan et al. 2018", "Rasmussen et al. 2014", "Ray-Griffith et al. 2017", 
                                                                     "Sharma et al. 2019", "Shiroma et al. 2020", "Singh et al. 2015", 
                                                                     "Yoosefi et al. 2014", "Zhang et al. 2018", "Zhong et al. 2016", 
                                                                     "Zou et al. 2021"), class = "factor"),
                            Nt = c(33L, 22L), Nc = c(37L, 24L), Mt = c(35.3, 35.7), Mc = c(34.4, 35.9), SDt = c(1.2, 0.6), SDc = c(3, 0.4)), class = "data.frame", row.names = c(9L, 28L))
#Perform meta-analysis
mre_copy <- metacont(n.e = Nt, mean.e = Mt, sd.e = SDt,
                     n.c = Nc, mean.c = Mc, sd.c = SDc,
                     studlab = Author, data = copy_data,
                     sm = "SMD", method.tau = "DL")
mre_copy
#forestplot
forest.meta(mre_copy, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Ketamine", label.c = "Control",
            label.left = "Favours ketamine", label.right = "Favours control")
#-----9. Immediate recall
ir_data <- structure(list(Author = structure(c(3L, 8L, 9L), .Label = c("Alizadeh et al. 2015","Anderson et al.", "Anderson et al. 2017", "Chen M et al. 2018", 
                                                                       "Domany et al. 2020", "Dong et al. 2019", "Fernie et al. 2017", 
                                                                       "Keilp et al. 2021", "Loo et al. 2012", "Ochs-Ross et al. 2020", 
                                                                       "Pradhan et al. 2018", "Rasmussen et al. 2014", "Ray-Griffith et al. 2017", 
                                                                       "Sharma et al. 2019", "Shiroma et al. 2020", "Singh et al. 2015", 
                                                                       "Yoosefi et al. 2014", "Zhang et al. 2018", "Zhong et al. 2016", 
                                                                       "Zou et al. 2021"), class = "factor"),
                          Nt = c(33L, 39L, 22L), Nc = c(37L, 39L, 24L), Mt = c(19.5, -0.02, 21.5), Mc = c(19.7, -0.16, 29.8), SDt = c(6, 1.22, 10.1), SDc = c(6.7, 1.5, 6.8)), class = "data.frame", row.names = c(10L, 22L, 29L))
#Perform meta-analysis
mre_ir <- metacont(n.e = Nt, mean.e = Mt, sd.e = SDt,
                   n.c = Nc, mean.c = Mc, sd.c = SDc,
                   studlab = Author, data = ir_data,
                   sm = "SMD", method.tau = "DL")
mre_ir
#forestplot
forest.meta(mre_ir, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Ketamine", label.c = "Control",
            label.left = "Favours ketamine", label.right = "Favours control")
#-----10. Attention
attention_data <- structure(list(Author = structure(c(10L, 18L), .Label = c("Alizadeh et al. 2015", "Anderson et al.", "Anderson et al. 2017", "Chen M et al. 2018", 
                                                                            "Domany et al. 2020", "Dong et al. 2019", "Fernie et al. 2017", 
                                                                            "Keilp et al. 2021", "Loo et al. 2012", "Ochs-Ross et al. 2020", 
                                                                            "Pradhan et al. 2018", "Rasmussen et al. 2014", "Ray-Griffith et al. 2017", 
                                                                            "Sharma et al. 2019", "Shiroma et al. 2020", "Singh et al. 2015", 
                                                                            "Yoosefi et al. 2014", "Zhang et al. 2018", "Zhong et al. 2016", 
                                                                            "Zou et al. 2021"), class = "factor"),
                                 Nt = c(72L, 43L), Nc = c(65L, 34L), Mt = c(2.8, 39.2), Mc = c(2.7, 40.5), SDt = c(0.08, 13.29), SDc = c(0.06, 9.13)), class = "data.frame", row.names = c(41L, 56L))
#Perform meta-analysis
mre_att <- metacont(n.e = Nt, mean.e = Mt, sd.e = SDt,
                    n.c = Nc, mean.c = Mc, sd.c = SDc,
                    studlab = Author, data = attention_data,
                    sm = "SMD", method.tau = "DL")
mre_att
#forestplot
forest.meta(mre_att, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Ketamine", label.c = "Control",
            label.left = "Favours ketamine", label.right = "Favours control")
#-----11. Visual learning
vl_data <- structure(list(Author = structure(c(10L, 18L), .Label = c("Alizadeh et al. 2015","Anderson et al.", "Anderson et al. 2017", "Chen M et al. 2018", 
                                                                     "Domany et al. 2020", "Dong et al. 2019", "Fernie et al. 2017", 
                                                                     "Keilp et al. 2021", "Loo et al. 2012", "Ochs-Ross et al. 2020", 
                                                                     "Pradhan et al. 2018", "Rasmussen et al. 2014", "Ray-Griffith et al. 2017", 
                                                                     "Sharma et al. 2019", "Shiroma et al. 2020", "Singh et al. 2015", 
                                                                     "Yoosefi et al. 2014", "Zhang et al. 2018", "Zhong et al. 2016", 
                                                                     "Zou et al. 2021"), class = "factor"),
                          Nt = c(72L, 43L), Nc = c(65L, 34L), Mt = c(0.92, 39.93), Mc = c(0.95, 37.32), SDt = c(0.11, 10.89), SDc = c(0.1, 10.09)), class = "data.frame", row.names = c(42L, 59L))
#Perform meta-analysis
mre_vl <- metacont(n.e = Nt, mean.e = Mt, sd.e = SDt,
                   n.c = Nc, mean.c = Mc, sd.c = SDc,
                   studlab = Author, data = vl_data,
                   sm = "SMD", method.tau = "DL")
mre_vl
#forestplot
forest.meta(mre_vl, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Ketamine", label.c = "Control",
            label.left = "Favours ketamine", label.right = "Favours control")
#-----12. Working memory
wm_data <- structure(list(Author = structure(c(10L, 18L), .Label = c("Alizadeh et al. 2015", "Anderson et al.", "Anderson et al. 2017", "Chen M et al. 2018", 
                                                                     "Domany et al. 2020", "Dong et al. 2019", "Fernie et al. 2017", 
                                                                     "Keilp et al. 2021", "Loo et al. 2012", "Ochs-Ross et al. 2020", 
                                                                     "Pradhan et al. 2018", "Rasmussen et al. 2014", "Ray-Griffith et al. 2017", 
                                                                     "Sharma et al. 2019", "Shiroma et al. 2020", "Singh et al. 2015", 
                                                                     "Yoosefi et al. 2014", "Zhang et al. 2018", "Zhong et al. 2016", 
                                                                     "Zou et al. 2021"), class = "factor"),
                          Nt = c(72L, 43L), Nc = c(65L, 34L), Mt = c(3, 42.2), Mc = c(3, 42.95), SDt = c(0.08, 12.02), SDc = c(0.07, 9.75)), class = "data.frame", row.names = c(43L, 57L))
#Perform meta-analysis
mre_wm <- metacont(n.e = Nt, mean.e = Mt, sd.e = SDt,
                   n.c = Nc, mean.c = Mc, sd.c = SDc,
                   studlab = Author, data = wm_data,
                   sm = "SMD", method.tau = "DL")
mre_wm
#forestplot
forest.meta(mre_wm, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Ketamine", label.c = "Control",
            label.left = "Favours ketamine", label.right = "Favours control")
#-----13. MMSE
mmse_data <- structure(list(Author = structure(c(13L, 20L), .Label = c("Alizadeh et al. 2015", "Anderson et al.", "Anderson et al. 2017", "Chen M et al. 2018", 
                                                                       "Domany et al. 2020", "Dong et al. 2019", "Fernie et al. 2017", 
                                                                       "Keilp et al. 2021", "Loo et al. 2012", "Ochs-Ross et al. 2020", 
                                                                       "Pradhan et al. 2018", "Rasmussen et al. 2014", "Ray-Griffith et al. 2017", 
                                                                       "Sharma et al. 2019", "Shiroma et al. 2020", "Singh et al. 2015", 
                                                                       "Yoosefi et al. 2014", "Zhang et al. 2018", "Zhong et al. 2016", 
                                                                       "Zou et al. 2021"), class = "factor"),
                            Nt = c(8L, 76L), Nc = c(8L, 81L), Mt = c(27.2, 28.12), Mc = c(26.3, 28.07), SDt = c(2.6, 0.96), SDc = c(2.5, 0.91)), class = "data.frame", row.names = c(49L, 66L))
#Perform meta-analysis
mre_mmse <- metacont(n.e = Nt, mean.e = Mt, sd.e = SDt,
                     n.c = Nc, mean.c = Mc, sd.c = SDc,
                     studlab = Author, data = mmse_data,
                     sm = "SMD", method.tau = "DL")
mre_mmse
#forestplot
forest.meta(mre_mmse, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Ketamine", label.c = "Control",
            label.left = "Favours ketamine", label.right = "Favours control")

###----------------------------------------------------------------------------------------
###-----------------------------3. Meta Analysis of QoL------------------------------------
###----------------------------------------------------------------------------------------
#Import data
QoL_data <- structure(list(Author = structure(1:3, .Label = c("Anderson et al.", "Fedgchin et al.", "Ochs-Ross et al."), class = "factor"),
                           Nt = c(33L, 115L, 70L), Nc = c(37L, 113L, 64L), Mt = c(53, 0.224, 6.2), Mc = c(54.8, 0.181, 4.4), SDt = c(26.4, 0.2481, 22.78), SDc = c(30.2, 0.2495, 20.6)), class = "data.frame", row.names = c(NA, -3L))
#Perform meta-analysis
mre_QoL <- metacont(n.e = Nt, mean.e = Mt, sd.e = SDt,
                    n.c = Nc, mean.c = Mc, sd.c = SDc,
                    studlab = Author, data = QoL_data,
                    sm = "SMD", method.tau = "DL")
mre_QoL
#forestplot
forest.meta(mre_QoL, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Ketamine", label.c = "Control",
            label.left = "Favours ketamine", label.right = "Favours control")

###----------------------------------------------------------------------------------------
###-----------------------------4. Meta Analysis of Social functioning---------------------
###----------------------------------------------------------------------------------------
#Import data
SF_data <- structure(list(Author = structure(1:2, .Label = c("Fedgchin et al. 2019", "Ochs-Ross et al. 2020"), class = "factor"),
                          Nt = c(115L, 72L), Nc = c(113L, 65L), Mc = c(16, -3.8), Mt = c(13, -6.1), SDc = c(9.7, 6.35), SDt = c(9.32, 8.35)), class = "data.frame", row.names = c(NA, -2L))
#Perform meta-analysis
mre_SF <- metacont(n.e = Nt, mean.e = Mt, sd.e = SDt,
                   n.c = Nc, mean.c = Mc, sd.c = SDc,
                   studlab = Author, data = SF_data,
                   sm = "SMD", method.tau = "DL")
mre_SF
#forestplot
forest.meta(mre_SF, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Ketamine", label.c = "Control",
            label.left = "Favours ketamine", label.right = "Favours control")

###----------------------------------------------------------------------------------------
###-----------------------------5. Meta Analysis of Depression-----------------------------
###----------------------------------------------------------------------------------------
dep_data <- structure(list(Author = structure(c(1L, 3L, 2L, 4L, 5L, 6L, 7L, 8L, 9L,10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L), .Label = c("Alizadeh et al. 2015", "Altinay et al. 2019", "Anderson et al. 2017", "Chen et al. 2018", "Domany et al. 2020", "Dong et al. 2019", "Fedgchin et al. 2019", "Fernie et al. 2017", "Keilp et al. 2021", "Kheirabadi et al. 2019", 
                                                                                                                                                                                         "Loo et al. 2012", "Murrough et al. 2015", "Ochs-Ross et al. 2020", "Pradhan et al. 2018", "Price et al. 2014", "Rasmussen et al. 2014", "Ray-Griffith et al. 2017", "Salloum et al. 2019", "Sharma et al. 2019", "Shiroma et al. 2020", "Singh et. 2015", "Taylor et al. 2018", 
                                                                                                                                                                                         "Yoosefi et. 2014", "Zarate et al. 2012", "Zhang et al. 2018", "Zhong et al. 2016", "Zou et al. 2021"), class = "factor"), 
                           TRD = c(0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 1L, 0L), Disorder = structure(c(4L, 10L, 9L, 9L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 8L, 9L, 6L, 9L, 2L, 10L, 5L, 2L, 4L, 9L, 7L, 4L, 1L, 10L, 9L, 2L), .Label = c("Bip Depre", "Depre", "Depre (ogni tipo)", "MDD", "MDD + ansia", "PTSD", "SAD", "Suicidio transdiagn", "TRD", "Unip/Bip Depre"), class = "factor"), 
                           Use = structure(c(1L, 1L, 1L, 2L, 2L, 1L, 3L, 1L, 2L, 2L, 1L, 2L, 3L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 1L), .Label = c("Anest + ECT", "Tp ev", "Tp spray"), class = "factor"), Depre = structure(c(3L, 4L, 1L, 2L, 4L, 2L, 4L, 3L, 3L, 3L, 1L, 4L, 4L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 4L, 1L, 1L, 4L, 2L, 3L, 2L), .Label = c("", "HAM-D", "HRSD", "MADRS"), class = "factor"), 
                           Timepoint = structure(c(7L, 3L, 1L, 4L, 5L, 1L, 3L, 1L, 1L, 3L, 1L, 8L, 3L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 6L, 1L, 1L, 2L, 1L, 9L, 1L), .Label = c("", "1 day", "1 month", "14 days", "2 hours", "3 days", "6 ECT sessions", "7 days", "8 ECT sessions"), class = "factor"), Days.Followup = c(NA, 28L, NA, 14L, 3L, 28L, 28L, 28L, 1L, 28L, 28L, 7L, 28L, NA, 1L, NA, NA, NA, NA, NA, 3L, 1L, NA, 1L, 28L, 21L, 28L),
                           Nt = c(22L, 33L, NA, 24L, 7L, 43L, 115L, 20L, 39L, 16L, NA, 12L, 72L, NA, NA, NA, 8L, NA, NA, NA, 11L, NA, NA, 40L, 43L, 30L, 76L), Mt = c(14.18, 16.8, NA, 15.54, 43.9, 6.4, -19, 14.08, 19.5, 19.4, NA, 21.7, 25.4, NA, NA, NA, 12.8, NA, NA, NA, -13.4, NA, NA, 23.73, 22.63, 14.8, 8.69), SDt = c(11.83, 13.6, NA, 6.14, 26, 1.22, 13.86, 8.08, 9.4, 1.5, NA, 13.1, 12.7, NA, NA, NA, 4.6, NA, NA, NA, 3.03, NA, NA, 10.32, 0.95, 0.256, 4.15),
                           Nc = c(20L, 37L, NA, 24L, 7L, 45L, 113L, 20L, 39L, 15L, NA, 12L, 65L, NA, NA, NA, 8L, NA, NA, NA, 10L, NA, NA, 38L, 34L, 30L, 81L), Mc = c(14.33, 14.8, NA, 16.7, 59.4, 7.62, -14.8, 12.08, 23.8, 12.9, NA, 22.2, 28.7, NA, NA, NA, 14.3, NA, NA, NA, -2.3, NA, NA, 30.68, 24.6, 16.814, 8.97), SDc = c(9.46, 11.4, NA, 8.17, 17.2, 2.77, 15.07, 9.86, 8.9, 2.6, NA, 13.1, 10.11, NA, NA, NA, 5.1, NA, NA, NA, 3.38, NA, NA, 5.5, 1.03, 0.256, 4.82),
                           Country = structure(c(5L, 7L, 1L, 6L, 8L, 3L, 8L, 7L, 9L, 5L, 2L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 4L, 9L, 9L, 9L, 5L, 9L, 3L, 3L, 3L), .Label = c("", "Australia", "China", "India", "Iran", "Taiwan", "UK", "US", "USA"), class = "factor"),
                           Study.Design = structure(c(2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 2L, 3L, 2L, 2L, 2L), .Label = c("", "RCT", "RCT crossover"), class = "factor"),
                           N.participants....Females. = structure(c(13L, 18L, 1L, 16L, 6L, 21L, 7L, 12L, 20L, 10L, 15L, 25L, 2L, 24L, 27L, 26L, 23L, 14L, 8L, 17L, 9L, 5L, 11L, 3L, 19L, 22L, 4L), .Label = c("", "137", "15 (53%)", "157 (57.3 %)", "18 (38.9%)", "18 (65.6%)", "228 (71.1%)", "25 (53.85)", "30 (60%)", "31 (29%)", "31 (45.16%)", "40 (55%)", "42 (69%)", "45 (64.4 %)", "46 (28)", "48 (75%)", "54 (14.8 %)", "70 (62.9%)", "77 (53.25 %)", "78", "88 (54.5%)", "90 (60%)", "Tot 16; Fem:13 (81.25%) ", "Tot 20; Fem:12 (60%) ", "Tot 24; Fem 16 (66.7%)", "Tot 38, Fem: 24", "tot 57 Fem:30"), class = "factor"),
                           N.participants.tot = c(42L, 70L, NA, 48L, 18L, 88L, 228L, 40L, 78L, 31L, 46L, 24L, 137L, 20L, 57L, 38L, 16L, 45L, 25L, 54L, 30L, 18L, 31L, 15L, 77L, 90L, 157L), X..Females.tot = structure(c(22L, 17L, 1L, 24L, 20L, 11L, 23L, 12L, 15L, 4L, 3L, 21L, 16L, 14L, 7L, 18L, 25L, 19L, 10L, 2L, 14L, 5L, 6L, 8L, 9L, 14L, 13L), .Label = c("", "14.8", "28", "29", "38.9", "45.16", "52.6", "53", "53.23", "53.85", "54.5", "55", "57.3", "60", "60.3", "62", "62.9", "63.2", "64.4", "65.6", "66.7", "69", "71.1", "75", "81.25"), class = "factor"),
                           N.Ketamine.Esketamine....Females. = structure(c(9L, 12L, 1L, 7L, 22L, 17L, 3L, 6L, 14L, 15L, 8L, 27L, 20L, 2L, 26L, 24L, 23L, 13L, 4L, 10L, 25L, 21L, 5L, 19L, 16L, 11L, 18L), .Label = c("", "10 (Keta + TIMBER psychoterapy) Fem:8", "115 (70.4%)", "12 (66.7%)", "17 (41.18%)", "20 (55%)", "21 (87.5%)", "22 (11)", "22 (72.7%)", "25 (12 %)", "30 ( 53.3 %)", "33 (66.7%)", "35 (65.7%)", "39 (22)", "4 (25%)", "43 (55.81 %)", "43 (58.1%)", "67 (64.2%)", "7 (3 drop out)", "72 (62.5%)", "9 (22.2%)", "9 (65.6%)", "ECT + keta 8; Fem: 6 (75%)", "ECT + keta: 21 Fem:16", "Keta 0.20 n.9 (56%) - Keta 0.40 n 11 (64%)", "keta: tot: 36, Fem: 20 (56%)", "Tot 12; Fem 8 (66.7%)"), class = "factor"),
                           N.controls....Females. = structure(c(9L, 14L, 1L, 7L, 22L, 16L, 4L, 8L, 15L, 17L, 10L, 26L, 18L, 2L, 25L, 23L, 24L, 3L, 5L, 11L, 3L, 21L, 6L, 20L, 13L, 12L, 19L), .Label = c("", "(Placebo + TIMBER psychoterapy) tot 10; Fem:4", "10 (60%)", "113 (71.7%)", "13 (46.2%) ", "14 (46.66%)", "15 (62.5%)", "20 (55%)", "20 (65%)", "24 (17)", "29 (17.2%)", "30 (66.7%)", "34 (50%)", "37 (59.5%)", "39 (25)", "45 (51.1%)", "5 (33.3%)", "65 (61.5%)", "70 (67.1 %)", "8 (1 drop)", "9 (55.56%)", "9 (65.6%)", "ECT + methohexital: 17 Fem:8", "ECT+Methohexital 8; Fem 7 (87.5%)", "midazolam: Tot: 21, Fem: 10 (48%)", "Tot 12; Fem 8 (66.7%)"), class = "factor"),
                           Mean.Age = structure(c(5L, 24L, 1L, 22L, 6L, 7L, 18L, 23L, 9L, 10L, 15L, 14L, 26L, 11L, 20L, 21L, 12L, 17L, 8L, 1L, 13L, 2L, 16L, 19L, 3L, 4L, 25L), .Label = c("", "29.7", "30.2", "30.6", "34.7", "35.4", "36.2", "38", "38.4", "39.1", "40.7", "40.9", "42.2", "42.4", "43.2", "43.7", "44.6", "46.3", "46.7", "46.8", "47.7", "48.6", "50.9", "54.4", "65.7", "70"), class = "factor"),
                           Mean.Age.Ketamine = structure(c(5L, 23L, 1L, 20L, 7L, 8L, 18L, 22L, 9L, 12L, 16L, 17L, 26L, 10L, 21L, 19L, 14L, 15L, 6L, 24L, 13L, 2L, 11L, 1L, 3L, 4L, 25L), .Label = c("", "30.78", "31.47", "32.1", "34.3", "34.42", "35.1", "36.8", "37.2", "39.1", "40.87", "41.7", "41.8", "43.6", "44.7", "45.2", "45.8", "46.4", "47", "48.5", "48.6", "51.8", "52.2", "54.4", "65.76", "70"), class = "factor"), 
                           Intervention = structure(c(5L, 6L, 1L, 14L, 12L, 2L, 4L, 3L, 14L, 14L, 6L, 14L, 15L, 14L, 14L, 11L, 9L, 14L, 14L, 14L, 13L, 14L, 10L, 14L, 7L, 8L, 5L), .Label = c("", "ECT+Keta (0.3 mg/kg)", "ECT+Keta (0.5-1 mg/kg)", "Esketamine (56 mg)+AD", "Ketamine (0.3 mg/kg)+Propofol+ECT", "Ketamine (0.5 mg/kg)+ECT", "Ketamine (0.5 mg/kg)+Propofol+ECT", "Ketamine (0.8 mg/kg)+ECT", "Ketamine (1 mg/kg mean)+ECT", "Ketamine (1-2 mg/kg)+ECT", "Ketamine (1.05 mg/kg mean)+ECT", "Ketamine 0.2 mg/kg", "Ketamine 0.4 mg/kg", "Ketamine 0.5 mg/kg", "Ketamine 28-84 mg + newly AD"), class = "factor"),
                           Control = structure(c(15L, 13L, 1L, 12L, 12L, 4L, 2L, 5L, 9L, 19L, 13L, 8L, 14L, 16L, 10L, 7L, 6L, 10L, 3L, 11L, 12L, 12L, 18L, 18L, 17L, 17L, 15L), .Label = c("", "AD+PBO", "ECT", "ECT+PBO", "ECT+propofol", "Methohexital (1 mg/kg) + ECT", "Methohexital (1.04 mg/kg mean) + ECT", "Midazolam", "Midazolam (0.02 mg/kg)", "Midazolam (0.045 mg/kg)", "Midazolam (0.045 mg/kg) + 1 keta", "PBO", "PBO+ECT", "PBO+newly AD", "PBO+Propofol+ECT", "Placebo + TIMBER psychoterapy", "Propofol + ECT", "Thiopental (2-3 mg/kg) + ECT", "Thiopental (3 mg/kg) + ECT"), class = "factor")),
                      row.names = c(NA, -27L), class = "data.frame")

#Perform meta-analysis
mre_DEP <- metacont(n.e = Nt, mean.e = Mt, sd.e = SDt,
                    n.c = Nc, mean.c = Mc, sd.c = SDc,
                    studlab = Author, data = dep_data,
                    sm = "SMD", method.tau = "DL")
mre_DEP
#forestplot
forest.meta(mre_DEP, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Ketamine", label.c = "Control", allstudies = F,
            label.left = "Favours ketamine", label.right = "Favours control")

###----------------------------------------------------------------------------------------
###--------------------------------6. Meta-regression--------------------------------------
###----------------------------------------------------------------------------------------
#Anxiety ES ~ Depre ES
#Merge
dep_data$ES <- mre_DEP$TE
data.frame(dep_data$Author, dep_data$ES)
AnxDep_data <- merge(Anx_data, dep_data, by = "Author")
str(AnxDep_data)
mreg_data <- AnxDep_data
mreg_data$Mean.Age <- as.character(mreg_data$Mean.Age)
mreg_data$Mean.Age <- as.numeric(mreg_data$Mean.Age)
mreg_data$X..Females.tot <- as.character(mreg_data$X..Females.tot)
mreg_data$X..Females.tot <- as.numeric(mreg_data$X..Females.tot)
mreg_data$Females_tot <- mreg_data$X..Females.tot
mreg_data$Days.Followup <- as.character(mreg_data$Days.Followup)
mreg_data$Days.Followup <- as.numeric(mreg_data$Days.Followup)
summary(mreg_data$Diagnosis)
str(mreg_data)
mre_ANX2 <- metacont(n.e = Nt.x, mean.e = Mt.x, sd.e = SDt.x,
                     n.c = Nc.x, mean.c = Mc.x, sd.c = SDc.x,
                     studlab = Author, data = mreg_data,
                     sm = "SMD", method.tau = "DL")
mre_ANX2
mreg_uni_depreES <- metareg(mre_ANX2, ~ ES)
mreg_uni_depreES
#ANX ~ days follow-up
mreg_uni_daysFU <- metareg(mre_ANX2, ~ Days.Followup)
mreg_uni_daysFU
#ANX ~ Mean age
mreg_uni_Age <- metareg(mre_ANX2, ~ Mean.Age)
mreg_uni_Age
#ANX ~ Females
mreg_uni_Female <- metareg(mre_ANX2, ~ Females_tot)
mreg_uni_Female
#ANX ~ use
mreg_uni_use <- metareg(mre_ANX2, ~ Use)
mreg_uni_use
#ANX ~ country
mreg_uni_country <- metareg(mre_ANX2, ~ Country)
mreg_uni_country
#ANX ~ TRD
mreg_uni_TRD <- metareg(mre_ANX2, ~ TRD)
mreg_uni_TRD
#Multivariate ANX ~ Age + ES dep
mreg_multi <- metareg(mre_ANX2, ~ Mean.Age + ES)
mreg_multi
mreg_multi2 <- metareg(mre_ANX2, ~ Ketamine_Dose + Mean.Age)
mreg_multi2
###----------------------------------------------------------------------------------------
###-------------------7. Meta-analysis of safety and tolerability--------------------------
###----------------------------------------------------------------------------------------
#-------------------Adverse effects
#Import data
AE_data <- structure(list(Author = structure(1:21, .Label = c("Alizadeh et al. 2015", "Anderson et al. 2017", "Chen M et al. 2018", "Domany et al. 2020", 
                                                              "Dong et al. 2019", "Fedgchin et al. 2019", "Fernie et al. 2017", 
                                                              "Keilp et al. 2021", "Loo et al. 2012", "Murrough et al. 2015", 
                                                              "Ochs-Ross et al. 2020", "Price et al. 2014", "Rasmussen et al. 2014", 
                                                              "Ray-Griffith et al. 2017", "Singh et. 2015", "Taylor et al. 2018", 
                                                              "Yoosefi et. 2014", "Zarate et al. 2012", "Zhang et al. 2018", 
                                                              "Zhong et al. 2016", "Zou et al. 2021"), class = "factor"),
                          Nt = c(22L, 33L, NA, 7L, NA, 115L, 20L, 39L, 26L, 12L, 72L, NA, NA, NA, 11L, 18L, NA, 41L, NA, 30L, 76L), Nc = c(22L, 37L, NA, 7L, NA, 113L, 20L, 39L, 25L, 12L, 65L, NA, NA, NA, 10L, 18L, NA, 38L, NA, 30L, 81L),
                          EventsT = c(0L, 15L, NA, 2L, NA, 93L, 0L, 10L, 2L, 5L, 51L, NA, NA, NA, 8L, 15L, NA, 36L, NA, 0L, 27L), EventsC = c(0L, 10L, NA, 1L, NA, 64L, 1L, 12L, 0L, 0L, 39L, NA, NA, NA, 5L, 4L, NA, 16L, NA, 0L, 25L)), class = "data.frame", row.names = c(NA, -21L))
#Perform meta-analysis
m_reAE <- metabin(event.e = EventsT, n.e = Nt,
                  event.c = EventsC, n.c = Nc,
                  studlab = Author, data = AE_data, sm = "OR")
m_reAE
#Forest Plot
forest.meta(m_reAE, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Ketamine", label.c = "Control", allstudies = F,
            label.left = "Favours ketamine", label.right = "Favours control")

#-------------------Drop outs due to adverse effects
#Import data
DAE_data <- structure(list(Author = structure(1:21, .Label = c("Alizadeh et al. 2015","Anderson et al. 2017", "Chen M et al. 2018", "Domany et al. 2020", 
                                                               "Dong et al. 2019", "Fedgchin et al. 2019", "Fernie et al. 2017", 
                                                               "Keilp et al. 2021", "Loo et al. 2012", "Murrough et al. 2015", 
                                                               "Ochs-Ross et al. 2020", "Price et al. 2014", "Rasmussen et al. 2014", 
                                                               "Ray-Griffith et al. 2017", "Singh et. 2015", "Taylor et al. 2018", 
                                                               "Yoosefi et. 2014", "Zarate et al. 2012", "Zhang et al. 2018", 
                                                               "Zhong et al. 2016", "Zou et al. 2021"), class = "factor"),
                           Nt = c(22L, 33L, NA, 7L, NA, 117L, 20L, 39L, 26L, 12L, 72L, NA, NA, NA, 11L, 18L, 17L, 14L, 43L, 30L, 76L), Nc = c(22L, 37L, NA, 7L, NA, 113L, 20L, 39L, 25L, 12L, 65L, NA, NA, NA, 10L, 18L, 14L, 12L, 34L, 30L, 81L),
                           EventsT = c(0L, 0L, NA, 0L, NA, 1L, 0L, 5L, 0L, 0L, 4L, NA, NA, NA, 1L, 0L, 1L, 0L, 0L, 0L, 0L), EventsC = c(0L, 0L, NA, 0L, NA, 2L, 0L, 4L, 0L, 0L, 2L, NA, NA, NA, 0L, 0L, 0L, 0L, 0L, 0L, 0L)), class = "data.frame", row.names = c(NA, -21L))
#Perform meta-analysis
m_reDAE <- metabin(event.e = EventsT, n.e = Nt,
                   event.c = EventsC, n.c = Nc,
                   studlab = Author, data = DAE_data, sm = "OR")
m_reDAE
#Forest Plot
forest.meta(m_reDAE, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Ketamine", label.c = "Control", allstudies = F,
            label.left = "Favours ketamine", label.right = "Favours control")

#-------------------Drop outs due to any cause
#Import data
DO_data <- structure(list(Author = structure(1:21, .Label = c("Alizadeh et al. 2015", "Anderson et al. 2017", "Chen M et al. 2018", "Domany et al. 2020", 
                                                              "Dong et al. 2019", "Fedgchin et al. 2019", "Fernie et al. 2017", 
                                                              "Keilp et al. 2021", "Loo et al. 2012", "Murrough et al. 2015", 
                                                              "Ochs-Ross et al. 2020", "Price et al. 2014", "Rasmussen et al. 2014", 
                                                              "Ray-Griffith et al. 2017", "Singh et. 2015", "Taylor et al. 2018", 
                                                              "Yoosefi et. 2014", "Zarate et al. 2012", "Zhang et al. 2018", 
                                                              "Zhong et al. 2016", "Zou et al. 2021"), class = "factor"),
                          Nt = c(22L, 33L, NA, 7L, 55L, 117L, 20L, NA, 26L, 12L, 72L, NA, NA, NA, 11L, 18L, 17L, 14L, 43L, 30L, 76L), Nc = c(22L, 37L, NA, 7L, 62L, 113L, 20L, NA, 25L, 12L, 65L, NA, NA, NA, 10L, 18L, 14L, 12L, 34L, 30L, 81L),
                          EventsT = c(0L, 14L, NA, 0L, 12L, 6L, 7L, NA, 20L, 0L, 10L, NA, NA, NA, 1L, 1L, 2L, 3L, 0L, 0L, 9L), EventsC = c(2L, 19L, NA, 0L, 17L, 6L, 7L, NA, 15L, 0L, 6L, NA, NA, NA, 0L, 0L, 0L, 1L, 0L, 0L, 11L)), class = "data.frame", row.names = c(NA, -21L))
#Perform meta-analysis
m_reDO <- metabin(event.e = EventsT, n.e = Nt,
                  event.c = EventsC, n.c = Nc,
                  studlab = Author, data = DO_data, sm = "OR")
m_reDO
#Forest Plot
forest.meta(m_reDO, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Ketamine", label.c = "Control", allstudies = F,
            label.left = "Favours ketamine", label.right = "Favours control")