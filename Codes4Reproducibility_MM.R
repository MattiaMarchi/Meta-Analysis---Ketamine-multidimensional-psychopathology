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
                           Outcome = structure(c(3L, 2L, 4L, 1L, 6L, 5L), .Label = c("Anxiety CAST Subscale", "BAI", "CAS", "GAD-7", "LSAS", "STAI-S"), class = "factor"),
                           Diagnosis = structure(c(1L, 3L, 2L, 5L, 2L, 4L), .Label = c("Bipolar/Unipolar Depression (moderate or severe)", "MDD", "MDD, bipolar depression, depression NOS, dysthymia", "Social Anxiety Disorder", "Transdiagnostic"), class = "factor"),
                           Time.point = structure(c(1L, 3L, 1L, 4L, 2L, 2L), .Label = c("1 month", "24h", "3 days", "7 days"), class = "factor"), Nt = c(33L, 9L, 115L, 12L, 36L, 18L), Mt = c(4.96, 3.75, 5.8, 6, 42.31, 66.1), SDt = c(4.62, 1.64, 5.94, 2.6, 16.31, 30.9), Nc = c(37L, 9L, 113L, 12L, 21L, 17L), Mc = c(4.43, 16.45, 7.2, 8.2, 44.1, 86.1),
                           SDc = c(0.89, 6.79, 6.01, 3.9, 16.33, 30), class = "data.frame"))
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
dep_data <- structure(list(Author = structure(1:21, .Label = c("Alizadeh et al. 2015", "Anderson et al. 2017", "Chen M et al. 2018", "Domany et al. 2020", "Dong et al. 2019", "Fedgchin et al. 2019", "Fernie et al. 2017", "Keilp et al. 2021", "Loo et al. 2012", "Murrough et al. 2015", 
                                                               "Ochs-Ross et al. 2020", "Price et al. 2014", "Rasmussen et al. 2014", "Ray-Griffith et al. 2017", "Singh et. 2015", "Taylor et al. 2018", "Yoosefi et. 2014", "Zarate et al. 2012", "Zhang et al. 2018", "Zhong et al. 2016", "Zou et al. 2021"), class = "factor"), 
                           Outcome = structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L), .Label = c("", "Depression"), class = "factor"), TRD = c(0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 1L, 0L),
                           Disorder = structure(c(4L, 8L, 7L, 3L, 4L, 4L, 4L, 4L, 4L, 6L, 7L, 7L, 2L, 8L, 7L, 5L, 4L, 1L, 8L, 7L, 2L), .Label = c("Bip Depre", "Depre", "Depre (ogni tipo)", "MDD", "SAD", "Suicidio transdiagn","TRD", "Unip/Bip Depre"), class = "factor"),
                           Use = structure(c(1L, 1L, 2L, 2L, 1L, 3L, 1L, 2L, 1L, 2L, 3L, 2L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 1L), .Label = c("Anest + ECT", "Tp ev", "Tp spray"), class = "factor"), Depre = structure(c(3L, 4L, 2L, 4L, 2L, 4L, 3L, 3L, 1L, 4L, 4L, 1L, 1L, 2L, 4L, 1L, 1L, 4L, 2L, 3L, 2L), .Label = c("", "HAM-D", "HRSD", "MADRS"), class = "factor"), 
                           Timepoint = structure(c(7L, 3L, 4L, 5L, 1L, 3L, 1L, 1L, 1L, 8L, 3L, 1L, 1L, 1L, 6L, 1L, 1L, 2L, 1L, 9L, 1L), .Label = c("", "1 day", "1 month", "14 days", "2 hours", "3 days", "6 ECT sessions", "7 days", "8 ECT sessions"), class = "factor"), Days.Followup = c(NA, 28L, 14L, 3L, 28L, 28L, 28L, 1L, 28L, 7L, 28L, 1L, NA, NA, 3L, 1L, NA, 1L, 28L, 21L, 28L),
                           Nt = c(22L, 33L, 24L, 7L, 43L, 115L, 20L, 39L, NA, 12L, 72L, NA, NA, 8L, 11L, NA, NA, 40L, 43L, 30L, 76L), Mt = c(14.18, 16.8, 15.54, 43.9, 6.4, -19, 14.08, 19.5, NA, 21.7, 25.4, NA, NA, 12.8, -13.4, NA, NA, 23.73, 22.63, 14.8, 8.69), SDt = c(11.83, 13.6, 6.14, 26, 1.22, 13.86, 8.08, 9.4, NA, 13.1, 12.7, NA, NA, 4.6, 3.03, NA, NA, 10.32, 0.95, 0.256, 4.15),
                           Nc = c(20L, 37L, 24L, 7L, 45L, 113L, 20L, 39L, NA, 12L, 65L, NA, NA, 8L, 10L, NA, NA, 38L, 34L, 30L, 81L), Mc = c(14.33, 14.8, 16.7, 59.4, 7.62, -14.8, 12.08, 23.8, NA, 22.2, 28.7, NA, NA, 14.3, -2.3, NA, NA, 30.68, 24.6, 16.814, 8.97), SDc = c(9.46, 11.4, 8.17, 17.2, 2.77, 15.07, 9.86, 8.9, NA, 13.1, 10.11, NA, NA, 5.1, 3.38, NA, NA, 5.5, 1.03, 0.256, 4.82), 
                           Country = structure(c(3L, 5L, 4L, 6L, 2L, 6L, 5L, 7L, 1L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 3L, 7L, 2L, 2L, 2L), .Label = c("Australia", "China", "Iran", "Taiwan", "UK", "US", "USA"), class = "factor"),
                           Study.Design = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 1L), .Label = c("RCT", "RCT crossover"), class = "factor"), N.participants....Females. = structure(c(10L, 13L, 12L, 5L, 16L, 6L, 9L, 15L, 11L, 19L, 1L, 21L, 20L, 18L, 7L, 4L, 8L, 2L, 14L, 17L, 3L),
                                                                                                                                                                                                                                         .Label = c("137", "15 (53%)", "157 (57.3 %)", "18 (38.9%)", "18 (65.6%)", "228 (71.1%)", "30 (60%)", "31 (45.16%)", "40 (55%)", "42 (69%)", "46 (28)", "48 (75%)", "70 (62.9%)", "77 (53.25 %)", "78", "88 (54.5%)", 
                                                                                                                                                                                                                                                    "90 (60%)", "Tot 16; Fem:13 (81.25%) ", "Tot 24; Fem 16 (66.7%)","Tot 38, Fem: 24", "tot 57 Fem:30"), class = "factor"),
                           N.participants.tot = c(42L, 70L, 48L, 18L, 88L, 228L, 40L, 78L, 46L, 24L, 137L, 57L, 38L, 16L, 21L, 18L, 31L, 78L, 77L, 60L, 157L), X = c(NA, NA, 36L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 60L, 137L), X..Females.tot = structure(c(17L, 13L, 19L, 15L, 7L, 18L, 8L, 11L, 1L, 16L, 12L, 4L, 14L, 20L, 10L, 2L, 3L, 5L, 6L, 10L, 9L), .Label = c("28", "38.9", "45.16", "52.6", "53", "53.23", "54.5", "55", "57.3", "60", "60.3", "62", "62.9", "63.2", "65.6", "66.7", "69", "71.1", "75", "81.25"), class = "factor"),
                           N.Ketamine.Esketamine....Females. = structure(c(5L, 8L, 6L, 16L, 12L, 1L, 3L, 9L, 4L, 21L, 13L, 20L, 18L, 17L, 19L, 15L, 2L, 10L, 11L, 7L, 14L), .Label = c("115 (70.4%)", "17 (41.18%)", "20 (55%)", "22 (11)", "22 (72.7%)", "24", "30 ( 53.3 %)", "33 (66.7%)", "39 (22)", "40", "43 (55.81 %)", "43 (58.1%)", "72 (62.5%)", "76", "9 (22.2%)", "9 (65.6%)", "ECT + keta 8; Fem: 6 (75%)", "ECT + keta: 21 Fem:16", "Keta 0.20 n.9 (56%) - Keta 0.40 n 11 (64%)", "keta: tot: 36, Fem: 20 (56%)", "Tot 12; Fem 8 (66.7%)"), class = "factor"),
                           N.controls....Females. = structure(c(5L, 10L, 6L, 17L, 13L, 2L, 4L, 12L, 7L, 21L, 14L, 20L, 18L, 19L, 1L, 16L, 3L, 11L, 9L, 8L, 15L), .Label = c("10 (60%)", "113 (71.7%)", "14 (46.66%)", "20 (55%)", "20 (65%)", "24", "24 (17)", "30 (66.7%)", "34 (50%)", "37 (59.5%)", "38", "39 (25)", "45 (51.1%)", "65 (61.5%)", "81", "9 (55.56%)", "9 (65.6%)", "ECT + methohexital: 17 Fem:8", "ECT+Methohexital 8; Fem 7 (87.5%)", "midazolam: Tot: 21, Fem: 10 (48%)", "Tot 12; Fem 8 (66.7%)"), class = "factor"),
                           nota.su.N = structure(c(1L, 1L, 2L, 3L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 4L, 1L, 1L, 1L), .Label = c("", "aggiungi femmine", "depression data from unpublished", "unpublished data"), class = "factor"), Age.Mean.tot..SD. = structure(c(2L, 2L, 2L, 6L, 2L, 11L, 2L, 1L, 1L, 8L, 14L, 1L, 1L, 7L, 9L, 3L, 10L, 12L, 4L, 5L, 13L), .Label = c("", "/", "29.72 (11.05)", "30.21 (10.15)", "30.6 (9.15)", "35.4 (9.0)", "40.9 ± 14.1", "42.4 ± 13.3", "43 (11.59)", "43.83", "46.3 (11.2)", "46.7 (10.4)", "65.69 (3.92) ", "70 (4.52)"), class = "factor"),
                           Mean.Age.Ketamine = structure(c(5L, 19L, 16L, 6L, 7L, 14L, 18L, 8L, 12L, 13L, 21L, 17L, 15L, 11L, 10L, 2L, 9L, 1L, 3L, 4L, 20L), .Label = c("", "30.78", "31.47", "32.1", "34.3", "35.1", "36.8", "37.2", "40.87", "41.8", "43.6", "45.2", "45.8", "46.4", "47", "48.5", "48.6", "51.8", "52.2", "65.76", "70"), class = "factor"), Age.Mean..SD..Ketamine = structure(c(5L, 18L, 15L, 6L, 7L, 13L, 17L, 8L, 11L, 12L, 20L, 16L, 14L, 10L, 21L, 2L, 9L, 1L, 3L, 4L, 19L),
                                                                                                                                                                                                                                                                                                                                                                                                  .Label = c("", "30.78 (13.50)", "31.47 (11.47)", "32.1 ( 9.9)", "34.3 (10.7)", "35.1 (8.7)", "36.8 (15.1)", "37.2 (12.9)", "40.87", "43.6 ± 14.6", "45.2(15.6)", "45.8 ± 15.2", "46.4 (11.2)", "47.0 (13.2)", "48.5 (11.0)", "48.6 (11.4)", "51.8 (10.0)", "52.2 (11.9)", "65.76 (3.98)", "70.6 (4.79)", "Keta 0.20 age 44.7 (13.38) - Keta 0.40 age 41.8 (11.63)"), class = "factor"),
                           Age.Mean..SD..controls = structure(c(5L, 19L, 17L, 7L, 6L, 14L, 18L, 10L, 11L, 9L, 21L, 13L, 16L, 8L, 12L, 3L, 15L, 1L, 2L, 4L, 20L), .Label = c("", "28.62 (8.06)", "28.67 (8.66)", "29.2 (8.0)", "35.1 (12.4)", "35.7 (12.8)", "35.8 (9.9)", "38.1 ± 13.9", "39.1 ± 10.6", "39.6 (13)", "41.4(12.0)", "42.7 (10.89)", "43.8 (10.9)", "46.8 (11.4)", "47", "48.6 (7.2)", "48.6 (8.1)", "49.9 (12.5)", "56.4 (12.4)", "65.62 (3.92)", "69.4 (4.15)"), class = "factor"), Intervention = structure(c(4L, 5L, 13L, 11L, 1L, 3L, 2L, 13L, 5L, 13L, 14L, 13L, 10L, 8L, 12L, 13L, 9L, 13L, 6L, 7L, 4L), .Label = c("ECT+Keta (0.3 mg/kg)", "ECT+Keta (0.5-1 mg/kg)", "Esketamine (56 mg)+AD", "Ketamine (0.3 mg/kg)+Propofol+ECT", "Ketamine (0.5 mg/kg)+ECT", "Ketamine (0.5 mg/kg)+Propofol+ECT", "Ketamine (0.8 mg/kg)+ECT", "Ketamine (1 mg/kg mean)+ECT", "Ketamine (1-2 mg/kg)+ECT", "Ketamine (1.05 mg/kg mean)+ECT", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         "Ketamine 0.2 mg/kg", "Ketamine 0.4 mg/kg", "Ketamine 0.5 mg/kg", "Ketamine 28-84 mg + newly AD"), class = "factor"),
                           Control = structure(c(12L, 10L, 9L, 9L, 2L, 1L, 3L, 7L, 10L, 6L, 11L, 8L, 5L, 4L, 9L, 9L, 14L, 14L, 13L, 13L, 12L), .Label = c("AD+PBO", "ECT+PBO", "ECT+propofol", "Methohexital (1 mg/kg) + ECT", "Methohexital (1.04 mg/kg mean) + ECT", "Midazolam", "Midazolam (0.02 mg/kg)", "Midazolam (0.045 mg/kg)", "PBO", "PBO+ECT", "PBO+newly AD", "PBO+Propofol+ECT", "Propofol + ECT", "Thiopental (2-3 mg/kg) + ECT"), class = "factor")), row.names = c(NA, -21L), class = "data.frame")
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