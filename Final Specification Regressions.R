##############################################
#######  data selection Regressions ##########
##############################################

library(plm)
library(dplyr)
library(xlsx)

fullDataRaw.data <- read.xlsx("./Data Sets/34states12yearsAdults.xlsx", sheetIndex = 1)
fullDataRaw.data <- tbl_df(fullDataRaw.data)
fullDataRaw.data

fullDataRaw.white <- filter(fullDataRaw.data, race == "White")
fullDataRaw.black  <- filter(fullDataRaw.data, race == "Black")

############## Base lm Regressions #######################################
allraces.DC.reg <- lm(drugcrime_ar ~ decrim_dummy + mmj + median_income + 
                      bls_unemprate + colorado_legal+ factor(st_name) + factor(year) + factor(race),
                      data = fullDataRaw.data)

allraces.HC.reg <- lm(heroincoke_ar ~ decrim_dummy + mmj + + median_income + 
                      bls_unemprate+ colorado_legal+ factor(st_name) + factor(year) + factor(race), 
                      data = fullDataRaw.data)
allraces.MJ.reg <- lm(marijuana_ar ~ decrim_dummy + mmj + median_income + 
                      bls_unemprate+ colorado_legal+ factor(st_name) + factor(year) + factor(race), 
                      data = fullDataRaw.data)

black.DC.reg <- lm(drugcrime_ar ~ decrim_dummy + mmj + median_income + bls_unemprate+ colorado_legal+ 
                   factor(st_name) + factor(year), data = fullDataRaw.black)
black.HC.reg <- lm(heroincoke_ar ~ decrim_dummy + mmj + median_income + bls_unemprate+ colorado_legal+
                   factor(st_name) + factor(year), data = fullDataRaw.black)
black.MJ.reg <- lm(marijuana_ar ~ decrim_dummy + mmj + median_income + bls_unemprate+ colorado_legal+
                   factor(st_name) + factor(year), data = fullDataRaw.black)

white.DC.reg <- lm(drugcrime_ar ~ decrim_dummy + mmj + median_income + bls_unemprate+ colorado_legal+
                   factor(st_name) + factor(year), data = fullDataRaw.white)
white.HC.reg <- lm(heroincoke_ar ~ decrim_dummy + mmj + median_income + bls_unemprate+ colorado_legal+
                   factor(st_name) + factor(year), data = fullDataRaw.white)
white.MJ.reg <- lm(marijuana_ar ~ decrim_dummy + mmj + median_income + bls_unemprate+ colorado_legal+
                   factor(st_name) + factor(year), data = fullDataRaw.white)


################# PLM Regressions ###########################################

# Raw data for main regression

allRaceReg.data <-fullDataRaw.data

allRaceReg.data$id <- group_indices(allRaceReg.data, st_code, race)



plm.allraces.DC.reg <- plm(drugcrime_ar ~ decrim_dummy + mmj + median_income + bls_unemprate + 
                      colorado_legal+ factor(race), data = allRaceReg.data,
                      index = c("id", "year"), model = "within", effect = "twoways")
plm.allraces.HC.reg <- plm(heroincoke_ar ~ decrim_dummy + mmj + median_income + bls_unemprate + 
                               colorado_legal+ factor(race), data = allRaceReg.data,
                           index = c("id", "year"), model = "within", effect = "twoways")
plm.allraces.MJ.reg <- plm(marijuana_ar ~ decrim_dummy + mmj + median_income + bls_unemprate + 
                               colorado_legal+ factor(race), data = allRaceReg.data,
                           index = c("id", "year"), model = "within", effect = "twoways")


summary(plm.allraces.DC.reg)



plm.black.DC.reg <- plm(drugcrime_ar ~ decrim_dummy + mmj + median_income +  bls_unemprate + colorado_legal, 
                        data = fullDataRaw.black, index=c("st_name", "year"), 
                        model = "within", effect = "twoways")
plm.black.HC.reg <- plm(heroincoke_ar ~ decrim_dummy + mmj + median_income + bls_unemprate + colorado_legal, 
                        data = fullDataRaw.black, index=c("st_name", "year"), 
                        model = "within", effect = "twoways")
plm.black.MJ.reg <- plm(marijuana_ar ~ decrim_dummy + mmj + median_income + bls_unemprate + colorado_legal, 
                        data = fullDataRaw.black, index=c("st_name", "year"), 
                        model = "within", effect = "twoways")

plm.white.DC.reg <- plm(drugcrime_ar ~ decrim_dummy + mmj + median_income + bls_unemprate + colorado_legal, 
                        data = fullDataRaw.white, index=c("st_name", "year"), 
                        model = "within", effect = "twoways")
plm.white.HC.reg <- plm(heroincoke_ar ~ decrim_dummy + mmj + median_income + bls_unemprate + colorado_legal, 
                        data = fullDataRaw.white, index=c("st_name", "year"), 
                        model = "within", effect = "twoways")
plm.white.MJ.reg <- plm(marijuana_ar ~ decrim_dummy + mmj + median_income + bls_unemprate + colorado_legal, 
                        data = fullDataRaw.white, index=c("st_name", "year"), 
                        model = "within", effect = "twoways")


################# No colorado legal Regressions ###########################################

noco.allraces.DC.reg <- plm(drugcrime_ar ~ decrim_dummy + mmj + median_income + bls_unemprate  
                               , data = allRaceReg.data,
                           index = c("id", "year"), model = "within", effect = "twoways")
noco.allraces.HC.reg <- plm(heroincoke_ar ~ decrim_dummy + mmj + median_income + bls_unemprate  
                               , data = allRaceReg.data,
                           index = c("id", "year"), model = "within", effect = "twoways")
noco.allraces.MJ.reg <- plm(marijuana_ar ~ decrim_dummy + mmj + median_income + bls_unemprate  
                               , data = allRaceReg.data,
                           index = c("id", "year"), model = "within", effect = "twoways")


noco.black.DC.reg <- plm(drugcrime_ar ~ decrim_dummy + mmj + median_income +  bls_unemprate , 
                        data = fullDataRaw.black, index=c("st_name", "year"), 
                        model = "within", effect = "twoways")
noco.black.HC.reg <- plm(heroincoke_ar ~ decrim_dummy + mmj + median_income + bls_unemprate , 
                        data = fullDataRaw.black, index=c("st_name", "year"), 
                        model = "within", effect = "twoways")
noco.black.MJ.reg <- plm(marijuana_ar ~ decrim_dummy + mmj + median_income + bls_unemprate , 
                        data = fullDataRaw.black, index=c("st_name", "year"), 
                        model = "within", effect = "twoways")

noco.white.DC.reg <- plm(drugcrime_ar ~ decrim_dummy + mmj + median_income + bls_unemprate , 
                        data = fullDataRaw.white, index=c("st_name", "year"), 
                        model = "within", effect = "twoways")
noco.white.HC.reg <- plm(heroincoke_ar ~ decrim_dummy + mmj + median_income + bls_unemprate , 
                        data = fullDataRaw.white, index=c("st_name", "year"), 
                        model = "within", effect = "twoways")
noco.white.MJ.reg <- plm(marijuana_ar ~ decrim_dummy + mmj + median_income + bls_unemprate , 
                        data = fullDataRaw.white, index=c("st_name", "year"), 
                        model = "within", effect = "twoways")




