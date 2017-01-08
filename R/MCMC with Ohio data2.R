######## MCMC
setwd("C:/HJH/0710")

library(MCMCpack)
library(ggplot2)
Ohio = read.csv('OhioFinalData.csv')
###recode cluster
Ohio$Cluster[Ohio$Cluster==1] = 4
Ohio$Cluster[Ohio$Cluster==3] = 1
Ohio$Cluster[Ohio$Cluster==4] = 3

Ohio$Cluster = as.factor(Ohio$Cluster)
Ohio$StoreID = as.factor(Ohio$StoreID)
Ohio = Ohio[Ohio$TimePeriodNumeric==1,]


vblist = c('TestCell' , 'Cluster' , 'GFV_Moscato_750mL_AUP_NoMissing1',
           'Barefoot_750mL_AUP_NoMissing' , 'Vendange_1.5L_AUP__NoMissing' , 'Yellow_Tail_750mL_AUP_NoMissing',
           'GFV_Moscato_750mL_Display_activity', 'Sutter_Home_1.5L_AUP_NoMissing', 'Yellow_Tail_1.5L_AUP_NoMissing',
           'AllMoscato_DollarShare_InPercent_PreTestmean')

### Dependent variable: AllMoscato_DollarShare_InPercent
f.fixed = formula(AllMoscato_DollarShare_InPercent ~ TestCell + Cluster + GFV_Moscato_750mL_AUP_NoMissing1
                + Barefoot_750mL_AUP_NoMissing + Vendange_1.5L_AUP__NoMissing + Yellow_Tail_750mL_AUP_NoMissing
                + GFV_Moscato_750mL_Display_activity + Sutter_Home_1.5L_AUP_NoMissing + Yellow_Tail_1.5L_AUP_NoMissing
                + AllMoscato_DollarShare_InPercent_PreTestmean)
f.rand = formula(~ GFV_Moscato_750mL_AUP_NoMissing1
                 + Barefoot_750mL_AUP_NoMissing + Vendange_1.5L_AUP__NoMissing + Yellow_Tail_750mL_AUP_NoMissing
                 + GFV_Moscato_750mL_Display_activity + Sutter_Home_1.5L_AUP_NoMissing + Yellow_Tail_1.5L_AUP_NoMissing
                 + AllMoscato_DollarShare_InPercent_PreTestmean)
v.rand = sapply(vblist[-c(1,2)], function(x) {var(Ohio[,names(Ohio)==x])})

Allmos = MCMChregress(fixed = f.fixed, random = ~ 1, group = 'StoreID',
                      r = 1, R = diag(c(1)), data = Ohio, burnin = 2000, mcmc = 10000, thin = 2)

Allmos.s = summary(Allmos$mcmc)
Allmos.quantile = data.frame(Allmos.s$quantiles[1:12, c(1,3,5)])
Allmos.quantile$Betas = rownames(Allmos.quantile)

p = ggplot(Allmos.quantile[2:4,], aes(x = Betas, y = X50., ymax = X97.5., ymin = X2.5.))
p = p + geom_point(size = 4) + geom_errorbar(width = 0.2) + ylab(NULL) + ggtitle('95% CI for coefficient of Cluster and TestCell')
p = p + geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed")
p

###### Dependent Variable: GFV_ALL_DollarShare_InPercent
f.fixed = formula(GFV_ALL_DollarShare_InPercent ~ TestCell + Cluster + GFV_Moscato_750mL_AUP_NoMissing1
                  + Barefoot_750mL_AUP_NoMissing + Vendange_1.5L_AUP__NoMissing + Yellow_Tail_750mL_AUP_NoMissing
                  + GFV_Moscato_750mL_Display_activity + Sutter_Home_1.5L_AUP_NoMissing + Yellow_Tail_1.5L_AUP_NoMissing
                  + AllMoscato_DollarShare_InPercent_PreTestmean)

AllGFV = MCMChregress(fixed = f.fixed, random = ~ 1, group = 'StoreID',
                      r = 1, R = diag(c(1)), data = Ohio, burnin = 2000, mcmc = 10000, thin = 2)

AllGFV.s = summary(AllGFV$mcmc)
AllGFV.quantile = data.frame(AllGFV.s$quantiles[1:12, c(1,3,5)])
AllGFV.quantile$Betas = rownames(AllGFV.quantile)

p = ggplot(AllGFV.quantile[2:4,], aes(x = Betas, y = X50., ymax = X97.5., ymin = X2.5.))
p = p + geom_point(size = 4) + geom_errorbar(width = 0.2) + ylab(NULL) + ggtitle('95% CI for coefficient of Cluster and TestCell')
p = p + geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed")
p

############ Dependent variable: GFV_AllMoscato_DollarShare_InPercent
f.fixed = formula(GFV_AllMoscato_DollarShare_InPercent ~ TestCell + Cluster + GFV_Moscato_750mL_AUP_NoMissing1
                  + Barefoot_750mL_AUP_NoMissing + Vendange_1.5L_AUP__NoMissing + Yellow_Tail_750mL_AUP_NoMissing
                  + GFV_Moscato_750mL_Display_activity + Sutter_Home_1.5L_AUP_NoMissing + Yellow_Tail_1.5L_AUP_NoMissing
                  + AllMoscato_DollarShare_InPercent_PreTestmean)

MosGFV = MCMChregress(fixed = f.fixed, random = ~ 1, group = 'StoreID',
                      r = 1, R = diag(c(1)), data = Ohio, burnin = 2000, mcmc = 10000, thin = 2)

MosGFV.s = summary(MosGFV$mcmc)
MosGFV.quantile = data.frame(MosGFV.s$quantiles[1:12, c(1,3,5)])
MosGFV.quantile$Betas = rownames(MosGFV.quantile)

p = ggplot(MosGFV.quantile[2:4,], aes(x = Betas, y = X50., ymax = X97.5., ymin = X2.5.))
p = p + geom_point(size = 4) + geom_errorbar(width = 0.2) + ylab(NULL) + ggtitle('95% CI for coefficient of Cluster and TestCell')
p

############### Dependent Variable: GFV_WhiteMoscato_DollarShare_InPercent
f.fixed = formula(GFV_WhiteMoscato_DollarShare_InPercent ~ TestCell + Cluster + GFV_Moscato_750mL_AUP_NoMissing1
                  + Barefoot_750mL_AUP_NoMissing + Vendange_1.5L_AUP__NoMissing + Yellow_Tail_750mL_AUP_NoMissing
                  + GFV_Moscato_750mL_Display_activity + Sutter_Home_1.5L_AUP_NoMissing + Yellow_Tail_1.5L_AUP_NoMissing
                  + AllMoscato_DollarShare_InPercent_PreTestmean)

WtMosGFV = MCMChregress(fixed = f.fixed, random = ~ 1, group = 'StoreID',
                      r = 1, R = diag(c(1)), data = Ohio, burnin = 2000, mcmc = 10000, thin = 2)

WtMosGFV.s = summary(WtMosGFV$mcmc)
WtMosGFV.quantile = data.frame(WtMosGFV.s$quantiles[1:12, c(1,3,5)])
WtMosGFV.quantile$Betas = rownames(WtMosGFV.quantile)

p = ggplot(WtMosGFV.quantile[2:4,], aes(x = Betas, y = X50., ymax = X97.5., ymin = X2.5.))
p = p + geom_point(size = 4) + geom_errorbar(width = 0.2) + ylab(NULL) + ggtitle('95% CI for coefficient of Cluster and TestCell')
p


