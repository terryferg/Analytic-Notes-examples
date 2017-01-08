#Fitting the correlation structure (aka do we want rand int or rand int & slope etc)

#AR1 structure - 
gls_pre_drizly <- gls(NAV_log_total_liters ~ week_ind ,
                      data = pre_data,
                      correlation = corAR1(form = ~ week_ind | store_id))
summary(gls_pre_drizly)
plot(ACF(gls_pre_drizly), alpha = 0.01, main = "AR(1) Cor")

#ARMA structure - 
gls_pre_drizly <- gls(NAV_log_total_liters ~ week_ind ,
                      data = pre_data,
                      correlation = corARMA(c(0.4, 0.1, 0.4, 0.2), form = ~ week_ind | store_id, p = 2, q = 2))
summary(gls_pre_drizly)
plot(ACF(gls_pre_drizly), alpha = 0.01, main = "AR(2)MA(2) Cor")

#AR1 with a random intercept - 
gls_pre_drizly <- lme(NAV_log_total_liters ~ week_ind ,
                      data = pre_data,
                      random = ~ 1 | store_id, 
                      correlation = corAR1(form = ~ week_ind | store_id))
summary(gls_pre_drizly)
plot(ACF(gls_pre_drizly), alpha = 0.01, main = "AR(1) Cor w/ Random Int")

#AR1 with a random slope and intercept - 
gls_pre_drizly <- lme(NAV_log_total_liters ~ week_ind ,
                      data = pre_data,
                      random = ~ 1 + week_ind | store_id, 
                      correlation = corAR1(form = ~ week_ind | store_id))
summary(gls_pre_drizly)
plot(ACF(gls_pre_drizly), alpha = 0.01, main = "AR(1) w/ Random Int and Slope")

#AR1 with random Int, slope and quadratic slope - Error in logLik.reStruct(object, conLin) : NA/NaN/Inf in foreign function call (arg 3)
gls_pre_drizly <- lme(NAV_log_total_liters ~ week_ind ,
                      data = pre_data,
                      random = ~ week_ind + I(week_ind^2) | store_id, 
                      correlation = corAR1(form = ~ week_ind | store_id))
summary(gls_pre_drizly)
plot(ACF(gls_pre_drizly), alpha = 0.01, main = "AR(1) w/ Random Int, Slope and Slope^2")

#ARMA with a random intercept - working
gls_pre_drizly <- lme(NAV_log_total_liters ~ week_ind ,
                      data = pre_data,
                      random = ~ 1 | store_id, 
                      correlation = corARMA(c(0.4, 0.4), form = ~ week_ind | store_id, p = 1, q = 1))
summary(gls_pre_drizly)
plot(ACF(gls_pre_drizly), alpha = 0.01, main = "AR(1)MA(1) w/ Random Int")


################################################################################################################
####Fit the actual model
#ar1 with rand Int for cases of new amsterdam all sku's
lme2=lme(NAV_log_total_liters~ad_dummy2+size_account,
         data=drizly_depletions,
         random= ~1|store_id,
         correlation=corAR1(form=~week_ind|store_id),
         na.action='na.omit')

summary(lme2)
plot(ACF(lme2),alpha=.01,main="AR(1) with Random Int")
plot(lme2)
#qqnorm(lme2, ~ranef(store_id,level=2))
qqnorm(resid(lme2))
abline(0,1)
#test to see if residuals are normal - p<.1 means not normal
shapiro.test(resid(lme2))

lme2=lme(NAV_log_total_liters~ad_dummy1+size_account,
         data=drizly_depletions,
         random= ~1|store_id,
         correlation=corARMA(c(0.4, 0.4), form = ~ week_ind | store_id, p = 1, q = 1),
         na.action='na.omit')

summary(lme2)
plot(ACF(lme2),alpha=.01,main="AR(1) with Random Int")
plot(lme2)
#qqnorm(lme2, ~ranef(store_id,level=2))
qqnorm(resid(lme2))
abline(0,1)
#test to see if residuals are normal - p<.1 means not normal
shapiro.test(resid(lme2))