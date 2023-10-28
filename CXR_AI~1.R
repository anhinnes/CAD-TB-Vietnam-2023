# Analysis of CXR data for "AI Manuscript". 
# 
# This file used to produce one of the tables in the manuscript using data from both:
# Active Case Finding (ACF) and 
# Intensive Case Finding (ICF) campaigns. 
# 
# Requirements:
#   ACF and ICF analytical data files. 
# 
# Code prepared by Andres Martinez <amartinez@fhi360.org>
#   
# Revisions: 
# Mar 07, 2023 Initial version. 
# Aug 09, 2023 Added optimal threshold calculation via function. 
# Sep 13, 2023 Added plotting functionality. 
# Sep 15, 2023 Removed parts not needed. 

# Clear work space:
rm(list=ls()) 

# Working directory:
setwd("C:/Users/AMartinez/OneDrive - Family Health International/Vietnam TB Manuscripts/AI_Calibration_Programmatic Intro to Vietnam")
# This folder is mapped to the project Sharepoint site. 

# Load data sets ####

# ACF data for years 2020-2022:
# da <- haven::read_dta("C:/Users/AMartinez/OneDrive - Family Health International/Vietnam TB Manuscripts/AI_Calibration_Programmatic Intro to Vietnam/Data/cx_full_analytical_combined_feb232023.dta")
da <- haven::read_dta("2.Data/Final/cx_full_analytical_combined_mar272023.dta")

# The file abode deprecated the following earlier version:
# da_dep <- haven::read_dta("C:/Users/AMartinez/OneDrive - Family Health International/Vietnam TB Manuscripts/AI_Calibration_Programmatic Intro to Vietnam/Data/archived/cxr_full_analytical_combined_aimanuscript.dta")

# ICF data for batches 1, 2 & 3:
di <- haven::read_dta("2.Data/Final/icf_hf_analytical_v2.dta")

# Prep data ####

# Create certain data subsets to simplify things further below. 

# ACF:  
addmargins(table(da$year, useNA = "always"))
da20 <- subset(da, year==2020)
da21 <- subset(da, year==2021)
da22 <- subset(da, year==2022)

# ICF:
addmargins(table(di$batch, useNA = "always"))
di1 <- subset(di, batch==1)
di2 <- subset(di, batch==2)
di3 <- subset(di, batch==3)

# Manuscript Tables ####

# N of participants screened with CXR (i.e., have an AI score):
table(da$year, useNA = "always")
table(da$year[!is.na(da$aiscore)], useNA = "always")

table(di$batch, useNA = "always")
table(di$batch[!is.na(di$ai_score)], useNA = "always")

# N of Xpert tests conducted:
table(da$year[!is.na(da$gxf)], useNA = "always")

# N of cases with an AI Score below and at or above threshold
table(da$year[da$aiscore < 0.4], useNA = "always")
table(da$year[da$aiscore >= 0.4], useNA = "always")

# N of cases with Xpert result and an AI Score below and at or above threshold
addmargins(table(da$aithres40, da$year)) # Threshold was 0.4
# The following for the same result (verification):
table(da$year[da$aiscore >= 0.4 & !is.na(da$gxf)], useNA = "always")
table(da$year[da$aiscore < 0.4  & !is.na(da$gxf)], useNA = "always")

# Proportion of cases with an AI Score below threshold for which an Xpert test was conducted 
table(da$year[!is.na(da$gxf)], useNA = "always")

# N of cases with CXR and valid Xpert results:
table(da$year[!is.na(da$gxf)], useNA = "always")
table(da$year[!is.na(da$gxf_pnt)], useNA = "always")
addmargins(table(da$year, da$gxf_pnt, useNA = "always"))

# N of cases with CXR and valid Xpert results:
table(di$batch, di$xpert_pos, useNA = "always")
table(di$batch[!is.na(di$xpert_pos)])
addmargins(table(di$batch, di$xpert_pos))

## X-tab AI TB & Dr TB ####
# For all cases, and only including cases with a Xpert test
str(di$aitb)
di$aitb.f <- factor(di$aitb, 
                    levels = c(1, 2),
                    labels = c('Yes', 'No'))
table(di$aitb, di$aitb.f)                      
                      
str(di$drtb)
di$drtb.f <- factor(di$drtb, 
                    levels = c(1, 2),
                    labels = c('Presumptive', 'Negative'))
table(di$drtb, di$drtb.f)                      

str(di$xpert_pos)
di$xpert_pos.f <- factor(di$xpert_pos, 
                    levels = c(0, 1),
                    labels = c('No', 'Yes'))
table(di$xpert_pos, di$xpert_pos.f)                      

# Batch 1, all cases and then Cases with valid xPert test result: 
addmargins(table(di$drtb.f[di$batch==1]))
addmargins(table(di$drtb.f[di$batch==1 & !is.na(di$xpert_pos.f)]))
addmargins(table(di$drtb.f[di$batch==1], di$xpert_pos.f[di$batch==1]))

addmargins(table(di$aitb.f[di$batch==1]))
addmargins(table(di$aitb.f[di$batch==1 & !is.na(di$xpert_pos.f)]))
addmargins(table(di$aitb.f[di$batch==1], di$xpert_pos.f[di$batch==1]))

addmargins(table(di$aitb.f[di$batch==1], di$drtb.f[di$batch==1]))
table(di$aitb.f[di$batch==1 & !is.na(di$xpert_pos.f)], di$drtb.f[di$batch==1 & !is.na(di$xpert_pos.f)])
addmargins(table(di$xpert_pos.f[di$batch==1 & di$aitb.f=='Yes' & di$drtb.f=='Presumptive']))
prop.table(table(di$xpert_pos.f[di$batch==1 & di$aitb.f=='Yes' & di$drtb.f=='Presumptive']))

addmargins(table(di$xpert_pos.f[di$batch==1 & di$aitb.f=='Yes' & di$drtb.f=='Negative']))
prop.table(table(di$xpert_pos.f[di$batch==1 & di$aitb.f=='Yes' & di$drtb.f=='Negative']))

addmargins(table(di$xpert_pos.f[di$batch==1 & di$aitb.f=='No' & di$drtb.f=='Presumptive']))
prop.table(table(di$xpert_pos.f[di$batch==1 & di$aitb.f=='No' & di$drtb.f=='Presumptive']))

addmargins(table(di$xpert_pos.f[di$batch==1 & di$aitb.f=='No' & di$drtb.f=='Negative']))
prop.table(table(di$xpert_pos.f[di$batch==1 & di$aitb.f=='No' & di$drtb.f=='Negative']))


# Batch 2, all cases and then Cases with valid xPert test result: 
addmargins(table(di$drtb.f[di$batch==2]))
addmargins(table(di$drtb.f[di$batch==2 & !is.na(di$xpert_pos.f)]))
addmargins(table(di$drtb.f[di$batch==2], di$xpert_pos.f[di$batch==2]))

addmargins(table(di$aitb.f[di$batch==2]))
addmargins(table(di$aitb.f[di$batch==2 & !is.na(di$xpert_pos.f)]))
addmargins(table(di$aitb.f[di$batch==2], di$xpert_pos.f[di$batch==2]))

addmargins(table(di$aitb.f[di$batch==2], di$drtb.f[di$batch==2]))
table(di$aitb.f[di$batch==2 & !is.na(di$xpert_pos.f)], di$drtb.f[di$batch==2 & !is.na(di$xpert_pos.f)])
addmargins(table(di$xpert_pos.f[di$batch==2 & di$aitb.f=='Yes' & di$drtb.f=='Presumptive']))
prop.table(table(di$xpert_pos.f[di$batch==2 & di$aitb.f=='Yes' & di$drtb.f=='Presumptive']))

addmargins(table(di$xpert_pos.f[di$batch==2 & di$aitb.f=='Yes' & di$drtb.f=='Negative']))
prop.table(table(di$xpert_pos.f[di$batch==2 & di$aitb.f=='Yes' & di$drtb.f=='Negative']))

addmargins(table(di$xpert_pos.f[di$batch==2 & di$aitb.f=='No' & di$drtb.f=='Presumptive']))
prop.table(table(di$xpert_pos.f[di$batch==2 & di$aitb.f=='No' & di$drtb.f=='Presumptive']))

addmargins(table(di$xpert_pos.f[di$batch==2 & di$aitb.f=='No' & di$drtb.f=='Negative']))
prop.table(table(di$xpert_pos.f[di$batch==2 & di$aitb.f=='No' & di$drtb.f=='Negative']))


# Batch 3, all cases and then Cases with valid xPert test result: 
addmargins(table(di$drtb.f[di$batch==3]))
addmargins(table(di$drtb.f[di$batch==3 & !is.na(di$xpert_pos.f)]))
addmargins(table(di$drtb.f[di$batch==3], di$xpert_pos.f[di$batch==3]))

addmargins(table(di$aitb.f[di$batch==3]))
addmargins(table(di$aitb.f[di$batch==3 & !is.na(di$xpert_pos.f)]))
addmargins(table(di$aitb.f[di$batch==3], di$xpert_pos.f[di$batch==3]))

addmargins(table(di$aitb.f[di$batch==3], di$drtb.f[di$batch==3]))
table(di$aitb.f[di$batch==3 & !is.na(di$xpert_pos.f)], di$drtb.f[di$batch==3 & !is.na(di$xpert_pos.f)])

addmargins(table(di$xpert_pos.f[di$batch==3 & di$aitb.f=='Yes' & di$drtb.f=='Presumptive']))
prop.table(table(di$xpert_pos.f[di$batch==3 & di$aitb.f=='Yes' & di$drtb.f=='Presumptive']))

addmargins(table(di$xpert_pos.f[di$batch==3 & di$aitb.f=='Yes' & di$drtb.f=='Negative']))
prop.table(table(di$xpert_pos.f[di$batch==3 & di$aitb.f=='Yes' & di$drtb.f=='Negative']))

addmargins(table(di$xpert_pos.f[di$batch==3 & di$aitb.f=='No' & di$drtb.f=='Presumptive']))
prop.table(table(di$xpert_pos.f[di$batch==3 & di$aitb.f=='No' & di$drtb.f=='Presumptive']))

addmargins(table(di$xpert_pos.f[di$batch==3 & di$aitb.f=='No' & di$drtb.f=='Negative']))
prop.table(table(di$xpert_pos.f[di$batch==3 & di$aitb.f=='No' & di$drtb.f=='Negative']))


# ..............................................................................
# ..............................................................................
# ROC Objects ####

## ACF ROC Objects ####
unique(da$province)

roas <- list()
comment(roas) <- "ROC objects for ACF data subsets"

attr(da$province, "labels")
roas[["y20ap"]] <- pROC::roc(da20$gxf_pnt, da20$aiscore)
roas[["y21ap"]] <- pROC::roc(da21$gxf_pnt, da21$aiscore)
roas[["y22ap"]] <- pROC::roc(da22$gxf_pnt, da22$aiscore)
attr(roas$y20ap, "label") <- "All Provinces, 2020"
attr(roas$y21ap, "label") <- "All Provinces, 2021"
attr(roas$y22ap, "label") <- "All Provinces, 2022"

table(da20$province)
attr(da20$province, "labels")
roas[["y20p1"]] <- pROC::roc(da20$gxf_pnt[da20$province==1], da20$aiscore[da20$province==1])
roas[["y20p2"]] <- pROC::roc(da20$gxf_pnt[da20$province==2], da20$aiscore[da20$province==2])
roas[["y20p3"]] <- pROC::roc(da20$gxf_pnt[da20$province==3], da20$aiscore[da20$province==3])
roas[["y20p4"]] <- pROC::roc(da20$gxf_pnt[da20$province==4], da20$aiscore[da20$province==4])
roas[["y20p5"]] <- pROC::roc(da20$gxf_pnt[da20$province==5], da20$aiscore[da20$province==5])
roas[["y20p6"]] <- pROC::roc(da20$gxf_pnt[da20$province==6], da20$aiscore[da20$province==6])
roas[["y20p7"]] <- pROC::roc(da20$gxf_pnt[da20$province==7], da20$aiscore[da20$province==7])
attr(roas$y20p1, "label") <- "An Giang, 2020"
attr(roas$y20p2, "label") <- "Can Tho, 2020"
attr(roas$y20p3, "label") <- "Dong Nai, 2020"
attr(roas$y20p4, "label") <- "Nghe An, 2020"
attr(roas$y20p5, "label") <- "Tay Ninh, 2020"
attr(roas$y20p6, "label") <- "Thai Binh, 2020"
attr(roas$y20p7, "label") <- "Tien Giang, 2020"

table(da21$province)
attr(da21$province, "labels")
roas[["y21p2"]] <- pROC::roc(da21$gxf_pnt[da21$province==2], da21$aiscore[da21$province==2])
roas[["y21p4"]] <- pROC::roc(da21$gxf_pnt[da21$province==4], da21$aiscore[da21$province==4])
roas[["y21p5"]] <- pROC::roc(da21$gxf_pnt[da21$province==5], da21$aiscore[da21$province==5])
roas[["y21p6"]] <- pROC::roc(da21$gxf_pnt[da21$province==6], da21$aiscore[da21$province==6])
roas[["y21p7"]] <- pROC::roc(da21$gxf_pnt[da21$province==7], da21$aiscore[da21$province==7])
roas[["y21p8"]] <- pROC::roc(da21$gxf_pnt[da21$province==8], da21$aiscore[da21$province==8])
attr(roas$y21p2, "label") <- "Can Tho, 2021"
attr(roas$y21p4, "label") <- "Nghe An, 2021"
attr(roas$y21p5, "label") <- "Tay Ninh, 2021"
attr(roas$y21p6, "label") <- "Thai Binh, 2021"
attr(roas$y21p7, "label") <- "Tien Giang, 2021"
attr(roas$y21p8, "label") <- "Dong Thap, 2021"

table(da22$province)
attr(da22$province, "labels")
roas[["y22p1"]] <- pROC::roc(da22$gxf_pnt[da22$province==1], da22$aiscore[da22$province==1])
roas[["y22p2"]] <- pROC::roc(da22$gxf_pnt[da22$province==2], da22$aiscore[da22$province==2])
roas[["y22p5"]] <- pROC::roc(da22$gxf_pnt[da22$province==5], da22$aiscore[da22$province==5])
roas[["y22p7"]] <- pROC::roc(da22$gxf_pnt[da22$province==7], da22$aiscore[da22$province==7])
attr(roas$y22p1, "label") <- "An Giang, 2022"
attr(roas$y22p2, "label") <- "Can Tho, 2022"
attr(roas$y22p5, "label") <- "Tay Ninh, 2022"
attr(roas$y22p7, "label") <- "Tien Giang, 2022"

acf.labels <- rep(NA, length(roas))
for (i in 1:length(roas)) { acf.labels[i] <- attr(roas[[i]], "label") }
acf.labels

# In case we want to compare against the version containing the trace results.  
# acf20wtrace <- pROC::roc(da20$gxf_pos, da20$aiscore)
# acf21wtrace <- pROC::roc(da21$gxf_pos, da21$aiscore)
# acf22wtrace <- pROC::roc(da22$gxf_pos, da22$aiscore)

## ICF ROC Objects ####
rois <- list()
comment(rois) <- "ROC objects for ICF data subsets"

str(di$province)
attr(da$province, "labels")
rois[["b1ap"]] <- pROC::roc(di1$xpert_pos, di1$ai_score)
rois[["b2ap"]] <- pROC::roc(di2$xpert_pos, di2$ai_score)
rois[["b3ap"]] <- pROC::roc(di3$xpert_pos, di3$ai_score)
attr(rois$b1ap, "label") <- "All Provinces, batch 1"
attr(rois$b2ap, "label") <- "All Provinces, batch 2"
attr(rois$b3ap, "label") <- "All Provinces, batch 3"

table(di1$province)
rois[["b1p1"]] <- pROC::roc(di1$xpert_pos[di1$province=="An Giang"], di1$ai_score[di1$province=="An Giang"])
rois[["b1p5"]] <- pROC::roc(di1$xpert_pos[di1$province=="Tay Ninh"], di1$ai_score[di1$province=="Tay Ninh"])
rois[["b1p6"]] <- pROC::roc(di1$xpert_pos[di1$province=="Thai Binh"], di1$ai_score[di1$province=="Thai Binh"])
rois[["b1p7"]] <- pROC::roc(di1$xpert_pos[di1$province=="Tien Giang"], di1$ai_score[di1$province=="Tien Giang"])
rois[["b1p8"]] <- pROC::roc(di1$xpert_pos[di1$province=="Dong Thap"], di1$ai_score[di1$province=="Dong Thap"])
attr(rois$b1p1, "label") <- "An Giang, batch 1"
attr(rois$b1p5, "label") <- "Tay Ninh, batch 1"
attr(rois$b1p6, "label") <- "Thai Binh, batch 1"
attr(rois$b1p7, "label") <- "Tien Giang, batch 1"
attr(rois$b1p8, "label") <- "Dong Thap, batch 1"

table(di2$province)
rois[["b2p1"]] <- pROC::roc(di2$xpert_pos[di2$province=="An Giang"], di2$ai_score[di2$province=="An Giang"])
rois[["b2p5"]] <- pROC::roc(di2$xpert_pos[di2$province=="Tay Ninh"], di2$ai_score[di2$province=="Tay Ninh"])
rois[["b2p6"]] <- pROC::roc(di2$xpert_pos[di2$province=="Thai Binh"], di2$ai_score[di2$province=="Thai Binh"])
rois[["b2p7"]] <- pROC::roc(di2$xpert_pos[di2$province=="Tien Giang"], di2$ai_score[di2$province=="Tien Giang"])
rois[["b2p8"]] <- pROC::roc(di2$xpert_pos[di2$province=="Dong Thap"], di2$ai_score[di2$province=="Dong Thap"])
attr(rois$b2p1, "label") <- "An Giang, batch 2"
attr(rois$b2p5, "label") <- "Tay Ninh, batch 2"
attr(rois$b2p6, "label") <- "Thai Binh, batch 2"
attr(rois$b2p7, "label") <- "Tien Giang, batch 2"
attr(rois$b2p8, "label") <- "Dong Thap, batch 2"

table(di3$province)
rois[["b3p1"]] <- pROC::roc(di3$xpert_pos[di3$province=="An Giang"], di3$ai_score[di3$province=="An Giang"])
rois[["b3p5"]] <- pROC::roc(di3$xpert_pos[di3$province=="Tay Ninh"], di3$ai_score[di3$province=="Tay Ninh"])
rois[["b3p6"]] <- pROC::roc(di3$xpert_pos[di3$province=="Thai Binh"], di3$ai_score[di3$province=="Thai Binh"])
rois[["b3p7"]] <- pROC::roc(di3$xpert_pos[di3$province=="Tien Giang"], di3$ai_score[di3$province=="Tien Giang"])
rois[["b3p8"]] <- pROC::roc(di3$xpert_pos[di3$province=="Dong Thap"], di3$ai_score[di3$province=="Dong Thap"])
attr(rois$b3p1, "label") <- "An Giang, batch 3"
attr(rois$b3p5, "label") <- "Tay Ninh, batch 3"
attr(rois$b3p6, "label") <- "Thai Binh, batch 3"
attr(rois$b3p7, "label") <- "Tien Giang, batch 3"
attr(rois$b3p8, "label") <- "Dong Thap, batch 3"

icf.labels <- rep(NA, length(rois))
for (i in 1:length(rois)) { icf.labels[i] <- attr(rois[[i]], "label") }
icf.labels

# ..............................................................................
# ..............................................................................
# Area under ROC ####

# Example -- note CI type:
pROC::auc(roas$y20ap); pROC::ci.auc(roas$y20ap)

# Area under ROC & 95% confidence intervals
(auc.acf <- cbind(do.call(rbind, lapply(roas, pROC::auc)), 
                 do.call(rbind, lapply(roas, pROC::ci.auc))))
(auc.icf <- cbind(do.call(rbind, lapply(rois, pROC::auc)), 
                 do.call(rbind, lapply(rois, pROC::ci.auc))))
colnames(auc.acf) <- colnames(auc.icf) <- c("AUC", "LB95%CI", "C95%CI", "UB95%CI")
auc.acf
auc.icf

# ..............................................................................
# ..............................................................................
# Coordinates ####

coordas <- lapply(roas, function(x) pROC::coords(x, "all", input='threshold', ret="all", transpose = FALSE))
coordis <- lapply(rois, function(x) pROC::coords(x, "all", input='threshold', ret="all", transpose = FALSE))
comment(coordas) <- "ROC objects for ACF data subsets"
comment(coordis) <- "ROC objects for ICF data subsets"

# Export:
# openxlsx::write.xlsx(coordas, file = paste0("1.Code/Results/_Coordinates-ACF-", Sys.Date(), ".xlsx"))
# openxlsx::write.xlsx(coordis, file = paste0("1.Code/Results/_Coordinates-ICF-", Sys.Date(), ".xlsx"))

# ..............................................................................
# ..............................................................................
# Sensitivity, specificity and PPV at threshold(s) shown:

rbind(
  round(pROC::coords(roas$y20ap, x=0.5, input='threshold', ret=c("threshold", "sensitivity", "specificity", "ppv"), transpose = FALSE),3),
  round(pROC::coords(roas$y21ap, x=0.5, input='threshold', ret=c("threshold", "sensitivity", "specificity", "ppv"), transpose = FALSE),3),
  round(pROC::coords(roas$y22ap, x=0.5, input='threshold', ret=c("threshold", "sensitivity", "specificity", "ppv"), transpose = FALSE),3),
  round(pROC::coords(rois$b1ap,  x=0.5, input='threshold', ret=c("threshold", "sensitivity", "specificity", "ppv"), transpose = FALSE),3),
  round(pROC::coords(rois$b2ap,  x=0.5, input='threshold', ret=c("threshold", "sensitivity", "specificity", "ppv"), transpose = FALSE),3), 
  round(pROC::coords(rois$b3ap,  x=0.5, input='threshold', ret=c("threshold", "sensitivity", "specificity", "ppv"), transpose = FALSE),3))

rbind(
  round(pROC::coords(roas$y20ap, x=0.4, input='threshold', ret=c("threshold", "sensitivity", "specificity", "ppv"), transpose = FALSE),3),
  round(pROC::coords(roas$y21ap, x=0.4, input='threshold', ret=c("threshold", "sensitivity", "specificity", "ppv"), transpose = FALSE),3),
  round(pROC::coords(roas$y22ap, x=0.4, input='threshold', ret=c("threshold", "sensitivity", "specificity", "ppv"), transpose = FALSE),3),
  round(pROC::coords(rois$b1ap,  x=0.4, input='threshold', ret=c("threshold", "sensitivity", "specificity", "ppv"), transpose = FALSE),3),
  round(pROC::coords(rois$b2ap,  x=0.4, input='threshold', ret=c("threshold", "sensitivity", "specificity", "ppv"), transpose = FALSE),3),
  round(pROC::coords(rois$b3ap,  x=0.4, input='threshold', ret=c("threshold", "sensitivity", "specificity", "ppv"), transpose = FALSE),3))

rbind(
  round(pROC::coords(roas$y20ap, x=0.6, input='threshold', ret=c("threshold", "sensitivity", "specificity", "ppv"), transpose = FALSE),3),
  round(pROC::coords(roas$y21ap, x=0.6, input='threshold', ret=c("threshold", "sensitivity", "specificity", "ppv"), transpose = FALSE),3),
  round(pROC::coords(roas$y22ap, x=0.6, input='threshold', ret=c("threshold", "sensitivity", "specificity", "ppv"), transpose = FALSE),3),
  round(pROC::coords(rois$b1ap,  x=0.6, input='threshold', ret=c("threshold", "sensitivity", "specificity", "ppv"), transpose = FALSE),3),
  round(pROC::coords(rois$b2ap,  x=0.6, input='threshold', ret=c("threshold", "sensitivity", "specificity", "ppv"), transpose = FALSE),3), 
  round(pROC::coords(rois$b3ap,  x=0.6, input='threshold', ret=c("threshold", "sensitivity", "specificity", "ppv"), transpose = FALSE),3))

# Confidence intervals  for Sensitivity, Specificity and PPV (based on bootstrapping)
set.seed(123)
# This approach needs to be checked --- the intervals produced in some cases may not include the AUC from pROC::auc
pROC::ci.coords(roas$y20ap, x=0.6, input = "threshold", ret=c("sensitivity", "specificity", "ppv", "accuracy"))
pROC::ci.coords(roas$y21ap, x=0.6, input = "threshold", ret=c("sensitivity", "specificity", "ppv", "accuracy"))
pROC::ci.coords(roas$y22ap, x=0.6, input = "threshold", ret=c("sensitivity", "specificity", "ppv", "accuracy"))
pROC::ci.coords(rois$b1ap,  x=0.6, input = "threshold", ret=c("sensitivity", "specificity", "ppv", "accuracy"))
pROC::ci.coords(rois$b2ap,  x=0.6, input = "threshold", ret=c("sensitivity", "specificity", "ppv", "accuracy"))
pROC::ci.coords(rois$b3ap,  x=0.6, input = "threshold", ret=c("sensitivity", "specificity", "ppv", "accuracy"))

pROC::ci.coords(rois$b1ap,  x=0.6, input = "threshold", ret=c("sensitivity", "specificity", "ppv", "accuracy"))
pROC::ci.coords(rois$b1ap,  x=0.6, input = "threshold", ret=c("sensitivity", "specificity", "ppv", "accuracy"))
pROC::ci.coords(rois$b1ap,  x=0.6, input = "threshold", ret=c("sensitivity", "specificity", "ppv", "accuracy"))
pROC::ci.coords(rois$b1ap,  x=0.6, input = "threshold", ret=c("sensitivity", "specificity", "ppv", "accuracy"))
pROC::ci.coords(rois$b1ap,  x=0.6, input = "threshold", ret=c("sensitivity", "specificity", "ppv", "accuracy"))

# ..............................................................................
# ..............................................................................
# N above/below threshold ####
coordis$b1ap$threshold
str(rois$b1ap,1)

# AI Score
length(rois$b1ap$original.predictor)
summary(rois$b1ap$original.predictor)
head(rois$b1ap$original.predictor,10)

# Thresholds
length(rois$b1ap$thresholds)

addmargins(table(rois$b1ap$original.predictor >= 0.6))
addmargins(table(rois$b2ap$original.predictor >= 0.6))
addmargins(table(rois$b3ap$original.predictor >= 0.6))

hist(rois$b1ap$original.predictor)

str(table(rois$b1ap$original.predictor > rois$b1ap$thresholds[251]))

c(rois$b1ap$thresholds[251], table(rois$b1ap$original.predictor > rois$b1ap$thresholds[251]))

cbind(rois$b1ap$thresholds, table(rois$b1ap$original.predictor > rois$b1ap$thresholds[250]))

rois$b1ap$thresholds[c(250, 251, 251, 253, 260)]




# ..............................................................................
# ..............................................................................
# Area under PRC ####
# Create certain data subsets
prdat20 <- na.omit((da20[, c("gxf_pnt", "aiscore")]))
prdat21 <- na.omit((da21[, c("gxf_pnt", "aiscore")]))
prdat22 <- na.omit((da22[, c("gxf_pnt", "aiscore")]))
pricfb1 <- na.omit((di1[, c("xpert_pos", "ai_score")]))
pricfb2 <- na.omit((di2[, c("xpert_pos", "ai_score")]))
pricfb3 <- na.omit((di3[, c("xpert_pos", "ai_score")]))

library(precrec)
precrec::auc(evalmod(labels = as.factor(prdat20$gxf_pnt), scores = as.numeric(prdat20$aiscore)))
precrec::auc(evalmod(labels = as.factor(prdat21$gxf_pnt), scores = as.numeric(prdat21$aiscore)))
precrec::auc(evalmod(labels = as.factor(prdat22$gxf_pnt), scores = as.numeric(prdat22$aiscore)))
precrec::auc(evalmod(labels = as.factor(pricfb1$xpert_pos), scores = as.numeric(pricfb1$ai_score)))
precrec::auc(evalmod(labels = as.factor(pricfb2$xpert_pos), scores = as.numeric(pricfb2$ai_score)))
precrec::auc(evalmod(labels = as.factor(pricfb3$xpert_pos), scores = as.numeric(pricfb3$ai_score)))

# ..............................................................................
# ..............................................................................
# Threshold monitoring ####

# Function to optimize threshold selection
optimthres <- function(rocObj, minSensitivity = 0.95, targetPPV = 0.20) {
  # Calculate coordinates
  coordsdf <- pROC::coords(rocObj, "all", 
                           input='threshold', 
                           ret = c("threshold", "sensitivity", "specificity", "precision"), 
                           transpose = FALSE)
  
  # Select cases with sensitivity above minimum:
  dat <- subset(coordsdf, sensitivity > minSensitivity)
  
  # Drop case for which threshold is negative (should be the first case)
  dat <- subset(dat, threshold > 0 )
  
  # Calculate distance from observed precision to target precision
  dat$ppvdistance <- abs(dat$precision - targetPPV)

  # Select cases with smallest distance between target and observed PPV
  opt <- dat[dat$ppvdistance==min(dat$ppvdistance),]

  return(list("coords" = coordsdf, "opt" = opt))
  }

## Calculate optimal thresholds ####
optimthres(roas$y20ap)$opt

lapply(roas, function(x) optimthres(x)$opt)
lapply(rois, function(x) optimthres(x)$opt)

do.call(rbind, lapply(roas, function(x) optimthres(x)$opt))
do.call(rbind, lapply(rois, function(x) optimthres(x)$opt))

#...............................................................................
#...............................................................................
# Threshold score monitoring and performance measures by groups ####
# Groups: age and history of tuberculosis (TB) treatment

## ACF ROC Objects by Group ####
roas.group <- list()
comment(roas.group) <- "ROC objects for ACF data subsets, by group (age or old TB)"
table(da20$elderly, useNA = "always")
table(da21$elderly, useNA = "always")
table(da22$elderly, useNA = "always")

roas.group[["y20ap_lt60"]] <- pROC::roc(da20$gxf_pnt[da20$elderly==0], da20$aiscore[da20$elderly==0])
roas.group[["y21ap_lt60"]] <- pROC::roc(da21$gxf_pnt[da21$elderly==0], da21$aiscore[da21$elderly==0])
roas.group[["y22ap_lt60"]] <- pROC::roc(da22$gxf_pnt[da22$elderly==0], da22$aiscore[da22$elderly==0])
attr(roas.group$y20ap_lt60, "label") <- "All Provinces, 2020, < 60 (non-elderly)"
attr(roas.group$y21ap_lt60, "label") <- "All Provinces, 2021, < 60 (non-elderly)"
attr(roas.group$y22ap_lt60, "label") <- "All Provinces, 2022, < 60 (non-elderly)"

roas.group[["y20ap_gt60"]] <- pROC::roc(da20$gxf_pnt[da20$elderly==1], da20$aiscore[da20$elderly==1])
roas.group[["y21ap_gt60"]] <- pROC::roc(da21$gxf_pnt[da21$elderly==1], da21$aiscore[da21$elderly==1])
roas.group[["y22ap_gt60"]] <- pROC::roc(da22$gxf_pnt[da22$elderly==1], da22$aiscore[da22$elderly==1])
attr(roas.group$y20ap_gt60, "label") <- "All Provinces, 2020, >= 60 (elderly)"
attr(roas.group$y21ap_gt60, "label") <- "All Provinces, 2021, >= 60 (elderly)"
attr(roas.group$y22ap_gt60, "label") <- "All Provinces, 2022, >= 60 (elderly)"

table(da20$oldtb, useNA = "always")
table(da21$oldtb, useNA = "always")
table(da22$oldtb, useNA = "always")

roas.group[["y20ap_nooldtb"]] <- pROC::roc(da20$gxf_pnt[da20$oldtb==0], da20$aiscore[da20$oldtb==0])
roas.group[["y21ap_nooldtb"]] <- pROC::roc(da21$gxf_pnt[da21$oldtb==0], da21$aiscore[da21$oldtb==0])
roas.group[["y22ap_nooldtb"]] <- pROC::roc(da22$gxf_pnt[da22$oldtb==0], da22$aiscore[da22$oldtb==0])
attr(roas.group$y20ap_nooldtb, "label") <- "All Provinces, 2020, no old TB"
attr(roas.group$y21ap_nooldtb, "label") <- "All Provinces, 2021, no old TB"
attr(roas.group$y22ap_nooldtb, "label") <- "All Provinces, 2022, no old TB"

roas.group[["y20ap_oldtb"]] <- pROC::roc(da20$gxf_pnt[da20$oldtb==1], da20$aiscore[da20$oldtb==1])
roas.group[["y21ap_oldtb"]] <- pROC::roc(da21$gxf_pnt[da21$oldtb==1], da21$aiscore[da21$oldtb==1])
roas.group[["y22ap_oldtb"]] <- pROC::roc(da22$gxf_pnt[da22$oldtb==1], da22$aiscore[da22$oldtb==1])
attr(roas.group$y20ap_oldtb, "label") <- "All Provinces, 2020, with old TB"
attr(roas.group$y21ap_oldtb, "label") <- "All Provinces, 2021, with old TB"
attr(roas.group$y22ap_oldtb, "label") <- "All Provinces, 2022, with old TB"

str(roas.group, 1)

# Performance measures and CIs
do.call(rbind, lapply(roas.group, function(x) optimthres(x)$opt))
pROC::ci.coords(roas.group$y20ap_lt60, x=0.545, input = "threshold", ret=c("sensitivity", "specificity", "ppv", "accuracy"))

#...............................................................................
#...............................................................................
# Plot Precision-Recall ####

# Plots:
# Recall/sensitivity (x-axis) v. PPV/precision
# Threshold (x-axis) v. PPV/precision

plotopth <- function(rocObj, campaign.type = NULL) {
  coord <- pROC::coords(rocObj, "all", input='threshold', ret = "all")
  prevalence2plot = mean((coord$tp + coord$fn) / (coord$tp + coord$fp + coord$tn + coord$fn))
  precision2plot = optimthres(rocObj)$opt[1,"precision"]
  threshold2plot = optimthres(rocObj)$opt[1,"threshold"]
  coord2 <- subset(coord, threshold!="-Inf" & threshold!="Inf")
  
  p1 <- ggplot(coord2, aes(x)) +
    geom_point(aes(y = precision, x = sensitivity)) +
    geom_line(aes(y = precision, x = sensitivity)) +
    # Plot line to indicate prevalence:
    geom_hline(yintercept = prevalence2plot, color = "red") +
    annotate("text", x = 0.025, y = prevalence2plot, hjust = 0, vjust = 1.5, color = "red", 
             label= paste0("Prevalence: ", round(100*prevalence2plot, 1), "%")) +
    # Shade area with Sensitivity > 0.95:
    geom_vline(xintercept = 0.95, linetype = "dotted", alpha = 0.5) +
    annotate("rect", xmin = 0.95, xmax = 1, ymin = 0, ymax = 1, alpha = .25) +
    # annotate("segment", x = 0.925, xend = 0.975, y = 0.05, yend = 0.05) +
    # annotate("text", x = 0.925, y = 0.05, hjust = 1, vjust = 0.5, label = "Sensitivity > 95%  ") +
    annotate("text", label = "Sensitivity > 95%", 
             x = 0.975, y = 0.5, angle = 90, hjust = 0.5, vjust = 0.5, size = 5) +
    # Plot line for precision closest to 20% within Sensitivity > 0.95:
    geom_hline(yintercept = precision2plot) +
    annotate("text", x = 0.025, y = precision2plot, hjust = 0, vjust = -0.5, 
             label= paste0("Precision closest to 20% with sensitivity > 95%: ", round(100*precision2plot,1), "%")) +
    scale_x_continuous("Sensitivity", 
                       limits = c(0, 1), expand = expansion(0, 0), 
                       breaks = seq(0, 1, 0.05), labels = paste0(seq(0, 100, 5), "%")) + 
    scale_y_continuous("Precision", 
                       limits = c(0, 1), expand = expansion(0, 0),
                       breaks = seq(0, 1, 0.05),  labels = paste0(seq(0, 100, 5), "%")) + 
    theme_bw() + 
    theme(panel.grid.minor = element_blank())
  
  p2 <- ggplot(coord2, aes(x)) +
    geom_point(aes(y = precision, x = threshold)) +
    geom_line(aes(y = precision, x = threshold)) +
    # Plot line for precision closest to 20% within Sensitivity > 0.95:
    geom_hline(yintercept = precision2plot) +
    # Plot line for threshold at chosen precision:
    geom_vline(xintercept = threshold2plot) +
    annotate("text", x = threshold2plot, y = 0.5*precision2plot, hjust = 0, vjust = 0.5, 
             label= paste0("  Threshold: ", round(threshold2plot,3))) +
    scale_x_continuous("Threshold", breaks = seq(0,1, 0.05), limits = c(0, 1), expand = expansion(0, 0)) + 
    scale_y_continuous("Precision", 
                       limits = c(0, 1), expand = expansion(0, 0),
                       breaks = seq(0, 1, 0.05),  labels = paste0(seq(0, 100, 5), "%")) + 
    theme_bw() + 
    theme(panel.grid.minor = element_blank())
  
  title <- ggdraw() + 
    draw_label(paste(campaign.type, attr(rocObj, "label")), size = 18, x = 0, hjust = 0) +
    draw_label(paste0("N = ", length(rocObj$predictor)),
               size = 12, x = 0, y = 0.15, hjust = 0) +
    theme(plot.margin = margin(0, 0, 20, 10))
  
  plot.row2 <- cowplot::plot_grid(p1, p2, ncol=2)
  plot.full <- cowplot::plot_grid(title, plot.row2, nrow = 2, rel_heights = c(.1, 1))
  
  return(plot.full)
}

# Example:
plotopth(roas$y21ap, "ACF")

addmargins(table(da$province, da$year))
addmargins(table(di$province, di$batch))

## Save images ####
# BEWARE: RUNNING WILL REPLACE IMAGES ON DISK
resave.images <- 0
stopifnot(resave.images==1)

# NOTE: This loop doesn't produce the plots as expected. 
# for (i in 1:length(roas)) {
#   png(paste0("1.Code/Results/ACF ", attr(roas[[i]], "label"), ".png"), width = 1200, height = 900)
#   plotopth(roas[[i]], "ACF")
#   dev.off() 
# }

length(roas)
png(paste0("1.Code/Results/ACF ", attr(roas[[01]], "label"), ".png"), width = 1200, height = 900); plotopth(roas[[01]], "ACF"); dev.off() 
png(paste0("1.Code/Results/ACF ", attr(roas[[02]], "label"), ".png"), width = 1200, height = 900); plotopth(roas[[02]], "ACF"); dev.off() 
png(paste0("1.Code/Results/ACF ", attr(roas[[03]], "label"), ".png"), width = 1200, height = 900); plotopth(roas[[03]], "ACF"); dev.off() 
png(paste0("1.Code/Results/ACF ", attr(roas[[04]], "label"), ".png"), width = 1200, height = 900); plotopth(roas[[04]], "ACF"); dev.off() 
png(paste0("1.Code/Results/ACF ", attr(roas[[05]], "label"), ".png"), width = 1200, height = 900); plotopth(roas[[05]], "ACF"); dev.off() 
png(paste0("1.Code/Results/ACF ", attr(roas[[06]], "label"), ".png"), width = 1200, height = 900); plotopth(roas[[06]], "ACF"); dev.off() 
png(paste0("1.Code/Results/ACF ", attr(roas[[07]], "label"), ".png"), width = 1200, height = 900); plotopth(roas[[07]], "ACF"); dev.off() 
png(paste0("1.Code/Results/ACF ", attr(roas[[08]], "label"), ".png"), width = 1200, height = 900); plotopth(roas[[08]], "ACF"); dev.off() 
png(paste0("1.Code/Results/ACF ", attr(roas[[09]], "label"), ".png"), width = 1200, height = 900); plotopth(roas[[09]], "ACF"); dev.off() 
png(paste0("1.Code/Results/ACF ", attr(roas[[10]], "label"), ".png"), width = 1200, height = 900); plotopth(roas[[10]], "ACF"); dev.off() 
png(paste0("1.Code/Results/ACF ", attr(roas[[11]], "label"), ".png"), width = 1200, height = 900); plotopth(roas[[11]], "ACF"); dev.off() 
png(paste0("1.Code/Results/ACF ", attr(roas[[12]], "label"), ".png"), width = 1200, height = 900); plotopth(roas[[12]], "ACF"); dev.off() 
png(paste0("1.Code/Results/ACF ", attr(roas[[13]], "label"), ".png"), width = 1200, height = 900); plotopth(roas[[13]], "ACF"); dev.off() 
png(paste0("1.Code/Results/ACF ", attr(roas[[14]], "label"), ".png"), width = 1200, height = 900); plotopth(roas[[14]], "ACF"); dev.off() 
png(paste0("1.Code/Results/ACF ", attr(roas[[15]], "label"), ".png"), width = 1200, height = 900); plotopth(roas[[15]], "ACF"); dev.off() 
png(paste0("1.Code/Results/ACF ", attr(roas[[16]], "label"), ".png"), width = 1200, height = 900); plotopth(roas[[16]], "ACF"); dev.off() 
png(paste0("1.Code/Results/ACF ", attr(roas[[17]], "label"), ".png"), width = 1200, height = 900); plotopth(roas[[17]], "ACF"); dev.off() 
png(paste0("1.Code/Results/ACF ", attr(roas[[18]], "label"), ".png"), width = 1200, height = 900); plotopth(roas[[18]], "ACF"); dev.off() 
png(paste0("1.Code/Results/ACF ", attr(roas[[19]], "label"), ".png"), width = 1200, height = 900); plotopth(roas[[19]], "ACF"); dev.off() 
png(paste0("1.Code/Results/ACF ", attr(roas[[20]], "label"), ".png"), width = 1200, height = 900); plotopth(roas[[20]], "ACF"); dev.off() 

length(rois)
png(paste0("1.Code/Results/ICF ", attr(rois[[01]], "label"), ".png"), width = 1200, height = 900); plotopth(rois[[01]], "ICF"); dev.off() 
png(paste0("1.Code/Results/ICF ", attr(rois[[02]], "label"), ".png"), width = 1200, height = 900); plotopth(rois[[02]], "ICF"); dev.off() 
png(paste0("1.Code/Results/ICF ", attr(rois[[03]], "label"), ".png"), width = 1200, height = 900); plotopth(rois[[03]], "ICF"); dev.off() 
png(paste0("1.Code/Results/ICF ", attr(rois[[04]], "label"), ".png"), width = 1200, height = 900); plotopth(rois[[04]], "ICF"); dev.off() 
png(paste0("1.Code/Results/ICF ", attr(rois[[05]], "label"), ".png"), width = 1200, height = 900); plotopth(rois[[05]], "ICF"); dev.off() 
png(paste0("1.Code/Results/ICF ", attr(rois[[06]], "label"), ".png"), width = 1200, height = 900); plotopth(rois[[06]], "ICF"); dev.off() 
png(paste0("1.Code/Results/ICF ", attr(rois[[07]], "label"), ".png"), width = 1200, height = 900); plotopth(rois[[07]], "ICF"); dev.off() 
png(paste0("1.Code/Results/ICF ", attr(rois[[08]], "label"), ".png"), width = 1200, height = 900); plotopth(rois[[08]], "ICF"); dev.off() 
png(paste0("1.Code/Results/ICF ", attr(rois[[09]], "label"), ".png"), width = 1200, height = 900); plotopth(rois[[09]], "ICF"); dev.off() 
png(paste0("1.Code/Results/ICF ", attr(rois[[10]], "label"), ".png"), width = 1200, height = 900); plotopth(rois[[10]], "ICF"); dev.off() 
png(paste0("1.Code/Results/ICF ", attr(rois[[11]], "label"), ".png"), width = 1200, height = 900); plotopth(rois[[11]], "ICF"); dev.off() 
png(paste0("1.Code/Results/ICF ", attr(rois[[12]], "label"), ".png"), width = 1200, height = 900); plotopth(rois[[12]], "ICF"); dev.off() 
png(paste0("1.Code/Results/ICF ", attr(rois[[13]], "label"), ".png"), width = 1200, height = 900); plotopth(rois[[13]], "ICF"); dev.off() 
png(paste0("1.Code/Results/ICF ", attr(rois[[14]], "label"), ".png"), width = 1200, height = 900); plotopth(rois[[14]], "ICF"); dev.off() 
png(paste0("1.Code/Results/ICF ", attr(rois[[15]], "label"), ".png"), width = 1200, height = 900); plotopth(rois[[15]], "ICF"); dev.off() 
png(paste0("1.Code/Results/ICF ", attr(rois[[16]], "label"), ".png"), width = 1200, height = 900); plotopth(rois[[16]], "ICF"); dev.off() 
png(paste0("1.Code/Results/ICF ", attr(rois[[17]], "label"), ".png"), width = 1200, height = 900); plotopth(rois[[17]], "ICF"); dev.off() 
png(paste0("1.Code/Results/ICF ", attr(rois[[18]], "label"), ".png"), width = 1200, height = 900); plotopth(rois[[18]], "ICF"); dev.off() 

#...............................................................................
#...............................................................................
# SANDBOX ####
# ..............................................................................
# ..............................................................................

# An Giang
attr(da$province, "labels")
# nrow(subset(da, province==1 & year==2020 & aiscore<.4))
# nrow(subset(da, province==1 & year==2020 & aiscore<.4 & is.na(gxf_pnt)))
addmargins(table(da$gxf_pnt[da$province==1 & da$aiscore < 0.4 & da$year==2020], useNA = "ifany"))
addmargins(table(da$gxf_pnt[da$province==1 & da$aiscore < 0.4 & da$year==2020]))
addmargins(table(da$gxf_pnt[da$province==1 & da$aiscore < 0.4 & da$year==2021], useNA = "ifany"))
addmargins(table(da$gxf_pnt[da$province==1 & da$aiscore < 0.4 & da$year==2021]))
addmargins(table(da$gxf_pnt[da$province==1 & da$aiscore < 0.4 & da$year==2022], useNA = "ifany"))
addmargins(table(da$gxf_pnt[da$province==1 & da$aiscore < 0.4 & da$year==2022]))

# Tay Ninh
attr(da$province, "labels")
addmargins(table(da$gxf_pnt[da$province==5 & da$aiscore < 0.4 & da$year==2020], useNA = "ifany"))
addmargins(table(da$gxf_pnt[da$province==5 & da$aiscore < 0.4 & da$year==2020]))
addmargins(table(da$gxf_pnt[da$province==5 & da$aiscore < 0.4 & da$year==2021], useNA = "ifany"))
addmargins(table(da$gxf_pnt[da$province==5 & da$aiscore < 0.4 & da$year==2021]))
addmargins(table(da$gxf_pnt[da$province==5 & da$aiscore < 0.4 & da$year==2022], useNA = "ifany"))
addmargins(table(da$gxf_pnt[da$province==5 & da$aiscore < 0.4 & da$year==2022]))

# Tien Giang
attr(da$province, "labels")
addmargins(table(da$gxf_pnt[da$province==7 & da$aiscore < 0.4 & da$year==2020], useNA = "ifany"))
addmargins(table(da$gxf_pnt[da$province==7 & da$aiscore < 0.4 & da$year==2020]))
addmargins(table(da$gxf_pnt[da$province==7 & da$aiscore < 0.4 & da$year==2021], useNA = "ifany"))
addmargins(table(da$gxf_pnt[da$province==7 & da$aiscore < 0.4 & da$year==2021]))
addmargins(table(da$gxf_pnt[da$province==7 & da$aiscore < 0.4 & da$year==2022], useNA = "ifany"))
addmargins(table(da$gxf_pnt[da$province==7 & da$aiscore < 0.4 & da$year==2022]))

# Nghe An
attr(da$province, "labels")
addmargins(table(da$gxf_pnt[da$province==4 & da$aiscore < 0.4 & da$year==2020], useNA = "ifany"))
addmargins(table(da$gxf_pnt[da$province==4 & da$aiscore < 0.4 & da$year==2020]))
addmargins(table(da$gxf_pnt[da$province==4 & da$aiscore < 0.4 & da$year==2021], useNA = "ifany"))
addmargins(table(da$gxf_pnt[da$province==4 & da$aiscore < 0.4 & da$year==2021]))
addmargins(table(da$gxf_pnt[da$province==4 & da$aiscore < 0.4 & da$year==2022], useNA = "ifany"))
addmargins(table(da$gxf_pnt[da$province==4 & da$aiscore < 0.4 & da$year==2022]))

# Thai Binh
attr(da$province, "labels")
addmargins(table(da$gxf_pnt[da$province==6 & da$aiscore < 0.4 & da$year==2020], useNA = "ifany"))
addmargins(table(da$gxf_pnt[da$province==6 & da$aiscore < 0.4 & da$year==2020]))
addmargins(table(da$gxf_pnt[da$province==6 & da$aiscore < 0.4 & da$year==2021], useNA = "ifany"))
addmargins(table(da$gxf_pnt[da$province==6 & da$aiscore < 0.4 & da$year==2021]))
addmargins(table(da$gxf_pnt[da$province==6 & da$aiscore < 0.4 & da$year==2022], useNA = "ifany"))
addmargins(table(da$gxf_pnt[da$province==6 & da$aiscore < 0.4 & da$year==2022]))

# For Facility Data, by Province
addmargins(table(di$xpert_pos[di$province=="An Giang" & di$ai_score < 0.6 & di$batch==1], useNA = "ifany"))
addmargins(table(di$xpert_pos[di$province=="An Giang" & di$ai_score < 0.6 & di$batch==2], useNA = "ifany"))
addmargins(table(di$xpert_pos[di$province=="An Giang" & di$ai_score < 0.6 & di$batch==3], useNA = "ifany"))

addmargins(table(di$xpert_pos[di$province=="Thai Binh" & di$ai_score < 0.6 & di$batch==1], useNA = "ifany"))
addmargins(table(di$xpert_pos[di$province=="Thai Binh" & di$ai_score < 0.6 & di$batch==2], useNA = "ifany"))
addmargins(table(di$xpert_pos[di$province=="Thai Binh" & di$ai_score < 0.6 & di$batch==3], useNA = "ifany"))

addmargins(table(di$xpert_pos[di$province=="Tay Ninh" & di$ai_score < 0.6 & di$batch==1], useNA = "ifany"))
addmargins(table(di$xpert_pos[di$province=="Tay Ninh" & di$ai_score < 0.6 & di$batch==2], useNA = "ifany"))
addmargins(table(di$xpert_pos[di$province=="Tay Ninh" & di$ai_score < 0.6 & di$batch==3], useNA = "ifany"))

addmargins(table(di$xpert_pos[di$province=="Tien Giang" & di$ai_score < 0.6 & di$batch==1], useNA = "ifany"))
addmargins(table(di$xpert_pos[di$province=="Tien Giang" & di$ai_score < 0.6 & di$batch==2], useNA = "ifany"))
addmargins(table(di$xpert_pos[di$province=="Tien Giang" & di$ai_score < 0.6 & di$batch==3], useNA = "ifany"))


head(sort(di$ai_score[di$province=="An Giang" & di$batch==1]))
head(sort(di$ai_score[di$province=="An Giang" & di$batch==2]))
head(sort(di$ai_score[di$province=="An Giang" & di$batch==3]))

head(sort(di$ai_score[di$province=="An Giang" & di$batch==1 & di$xpert_pos==1]))
head(sort(di$ai_score[di$province=="An Giang" & di$batch==2 & di$xpert_pos==1]))
head(sort(di$ai_score[di$province=="An Giang" & di$batch==3 & di$xpert_pos==1]))


# ..............................................................................
# ..............................................................................
# Production (Table 7 in manuscript)
tail(subset(pROC::coords(roa20, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roa20$predictor)
tail(subset(pROC::coords(roa21, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roa21$predictor)
tail(subset(pROC::coords(roa22, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roa22$predictor)

tail(subset(pROC::coords(roib1, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roib1$predictor)
tail(subset(pROC::coords(roib2, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roib2$predictor)
tail(subset(pROC::coords(roib3, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roib3$predictor)

tail(subset(pROC::coords(roa20, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)

# Need for Provinces # 1, 4, 5, 6, 7 (2 and 3 not needed). 
attr(da$province, "labels")
acf.labels

# An Giang (province==1)
tail(subset(pROC::coords(roa20_prov1, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roa20_prov1$predictor)
tail(subset(pROC::coords(roa22_prov1, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roa22_prov1$predictor)

tail(subset(pROC::coords(roi1_prov1, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roi1_prov1$predictor)
tail(subset(pROC::coords(roi2_prov1, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roi2_prov1$predictor)
tail(subset(pROC::coords(roi3_prov1, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roi3_prov1$predictor)

# Nghe An (province==4)
tail(subset(pROC::coords(roa20_prov4, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roa20_prov4$predictor)

tail(subset(pROC::coords(roa21_prov4, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roa21_prov4$predictor)

tail(subset(pROC::coords(roa22_prov4, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)

# Tay Ninh (province==5)
tail(subset(pROC::coords(roa20_prov5, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roa20_prov5$predictor)
tail(subset(pROC::coords(roa21_prov5, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roa21_prov5$predictor)
tail(subset(pROC::coords(roa22_prov5, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roa22_prov5$predictor)

tail(subset(pROC::coords(roi1_prov5, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roi1_prov5$predictor)
tail(subset(pROC::coords(roi2_prov5, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roi2_prov5$predictor)
tail(subset(pROC::coords(roi3_prov5, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roi3_prov5$predictor)


# Thai Binh (province==6) 
tail(subset(pROC::coords(roa20_prov6, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roa20_prov6$predictor)
tail(subset(pROC::coords(roa21_prov6, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roa21_prov6$predictor)
tail(subset(pROC::coords(roa22_prov6, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)

tail(subset(pROC::coords(roi1_prov6, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roi1_prov6$predictor)
tail(subset(pROC::coords(roi2_prov6, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roi2_prov6$predictor)
tail(subset(pROC::coords(roi3_prov6, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roi3_prov6$predictor)

# Tien Giang  (province==7)
tail(subset(pROC::coords(roa20_prov7, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roa20_prov7$predictor)
tail(subset(pROC::coords(roa21_prov7, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roa21_prov7$predictor)
tail(subset(pROC::coords(roa22_prov7, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roa22_prov7$predictor)

tail(subset(pROC::coords(roi1_prov7, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roi1_prov7$predictor)
tail(subset(pROC::coords(roi2_prov7, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roi2_prov7$predictor)
tail(subset(pROC::coords(roi3_prov7, "all", input='threshold', ret = returnCoords, transpose = FALSE), sensitivity > 0.95), n=1)
length(roi3_prov7$predictor)

