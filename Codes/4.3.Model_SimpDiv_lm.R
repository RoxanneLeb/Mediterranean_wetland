
## 06/2018 - Leberger et al. 2018. Mediterranean wetland conservation in the context of climate and land-cover change.


### 4.1. Beta-Sorensen response to several variables using beta-regression model
###############################################

# Load table
change_tab <- read.table('/Users/localadmin/Documents/PhD/Paper3_Wetlands/Data_to_publish/Table_all.txt', h=T)
head(change_tab)


## Simpson diversity
#-----------------

# distribution
hist(change_tab$div_poc, breaks=100)

div_poc2 <- change_tab$div_poc[change_tab$div_poc < 2] # remove outliers


## 1. Tmean diff
#---

lm_tmean2 <- lm(div_poc2 ~ change_tab $tmean_diff[change_tab $div_poc < 2])

hist(residuals(lm_tmean2), breaks=100)

summary(lm_tmean2)
Call:
# lm(formula = div_poc2 ~ change_tab$tmean[change_tab$div_poc < 
    # 2])

# Residuals:
     # Min       1Q   Median       3Q      Max 
# -0.93565 -0.12569  0.01067  0.10700  1.13378 

# Coefficients:
                                         # Estimate Std. Error t value Pr(>|t|)
# (Intercept)                              -0.05128    0.06185  -0.829    0.409
# change_tab$tmean[change_tab$div_poc < 2]  0.05131    0.11001   0.466    0.642

# Residual standard error: 0.2966 on 107 degrees of freedom
  # (125 observations deleted due to missingness)
# Multiple R-squared:  0.002029,	Adjusted R-squared:  -0.007297 
# F-statistic: 0.2176 on 1 and 107 DF,  p-value: 0.6418

AIC(lm_tmean2) # 48.35372


## 2. Pmean diff
#---

lm_pmean2 <- lm(div_poc2 ~ change_tab $pmean_diff[change_tab $div_poc < 2])

summary(lm_pmean2)
# Call:
# lm(formula = div_poc2 ~ change_tab$pmean[change_tab$div_poc < 
    # 2])

# Residuals:
     # Min       1Q   Median       3Q      Max 
# -0.94792 -0.13740  0.00978  0.10479  1.14802 

# Coefficients:
                                           # Estimate Std. Error t value Pr(>|t|)
# (Intercept)                              -0.0342275  0.0301578  -1.135    0.259
# change_tab$pmean[change_tab$div_poc < 2]  0.0002782  0.0003341   0.833    0.407

# Residual standard error: 0.2959 on 107 degrees of freedom
  # (125 observations deleted due to missingness)
# Multiple R-squared:  0.006442,	Adjusted R-squared:  -0.002844 
# F-statistic: 0.6937 on 1 and 107 DF,  p-value: 0.4068

AIC(lm_pmean2) # 47.87074


## 2.2. Tmean diff : Pmean diff
#---

lm_tpmean2 <- lm(div_poc2 ~ change_tab $tmean_diff[change_tab $div_poc < 2] : change_tab $pmean_diff[change_tab $div_poc < 2])
summary(lm_tpmean2)
# Call:
# lm(formula = div_poc2 ~ change_tab$tmean[change_tab$div_poc < 
    # 2]:change_tab$pmean[change_tab$div_poc < 2])

# Residuals:
     # Min       1Q   Median       3Q      Max 
# -0.93353 -0.13597  0.00916  0.10277  1.14734 

# Coefficients:
                                                                                    # Estimate Std. Error t value Pr(>|t|)
# (Intercept)                                                                       -2.650e-02  2.916e-02  -0.909    0.366
# change_tab$tmean[change_tab$div_poc < 2]:change_tab$pmean[change_tab$div_poc < 2]  7.146e-05  5.474e-04   0.131    0.896

# Residual standard error: 0.2969 on 107 degrees of freedom
  # (125 observations deleted due to missingness)
# Multiple R-squared:  0.0001592,	Adjusted R-squared:  -0.009185 
# F-statistic: 0.01704 on 1 and 107 DF,  p-value: 0.8964

AIC(lm_tpmean2) # 48.55779


## 3. Tmax diff
#---

lm_tmax2 <- lm(div_poc2 ~ change_tab $tmax_diff[change_tab $div_poc < 2])

AIC(lm_tmax2) # 47.70942

summary(lm_tmax2)
# Call:
# lm(formula = div_poc2 ~ change_tab$tmax[change_tab$div_poc < 
    # 2])

# Residuals:
     # Min       1Q   Median       3Q      Max 
# -0.91458 -0.11836  0.00264  0.11701  1.13453 

# Coefficients:
                                        # Estimate Std. Error t value Pr(>|t|)
# (Intercept)                             -0.05946    0.04628  -1.285    0.202
# change_tab$tmax[change_tab$div_poc < 2]  0.03320    0.03594   0.924    0.358

# Residual standard error: 0.2957 on 107 degrees of freedom
  # (125 observations deleted due to missingness)
# Multiple R-squared:  0.007911,	Adjusted R-squared:  -0.001361 
# F-statistic: 0.8532 on 1 and 107 DF,  p-value: 0.3577

## 4. Tmin diff
#---

lm_tmin2 <- lm(div_poc2 ~ change_tab $tmin_diff[change_tab $div_poc < 2])

summary(lm_tmin2)
# Call:
# lm(formula = div_poc2 ~ change_tab$tmin_diff[change_tab$div_poc < 
    # 2])

# Residuals:
     # Min       1Q   Median       3Q      Max 
# -0.93628 -0.11507  0.00584  0.10384  1.12201 

# Coefficients:
                                        # Estimate Std. Error t value Pr(>|t|)
# (Intercept)                             -0.05438    0.04394  -1.237    0.219
# change_tab$tmin[change_tab$div_poc < 2]  0.01538    0.01798   0.855    0.394

# Residual standard error: 0.2959 on 107 degrees of freedom
  # (125 observations deleted due to missingness)
# Multiple R-squared:  0.006791,	Adjusted R-squared:  -0.002491 
# F-statistic: 0.7316 on 1 and 107 DF,  p-value: 0.3943

AIC(lm_tmin2) # 47.83241

## 5. Tseas diff
#---

lm_tseas2 <- lm(div_poc2 ~ change_tab $tseas_diff[change_tab $div_poc < 2])

summary(lm_tseas2)
# Call:
# lm(formula = div_poc2 ~ change_tab$tseas[change_tab$div_poc < 
    # 2])

# Residuals:
     # Min       1Q   Median       3Q      Max 
# -0.93919 -0.12709  0.01065  0.09938  1.16963 

# Coefficients:
                                         # Estimate Std. Error t value Pr(>|t|)
# (Intercept)                              -0.01918    0.03253  -0.589    0.557
# change_tab$tseas[change_tab$div_poc < 2] -0.06878    0.16821  -0.409    0.683

# Residual standard error: 0.2967 on 107 degrees of freedom
  # (125 observations deleted due to missingness)
# Multiple R-squared:  0.00156,	Adjusted R-squared:  -0.007771 
# F-statistic: 0.1672 on 1 and 107 DF,  p-value: 0.6835

AIC(lm_tseas2) # 48.40499


## 5. Pwet diff
#---

lm_pwet2 <- lm(div_poc2 ~ change_tab $pwet_diff[change_tab $div_poc < 2])

summary(lm_pwet2)
# Call:
# lm(formula = div_poc2 ~ change_tab$pwet[change_tab$div_poc < 
    # 2])

# Residuals:
     # Min       1Q   Median       3Q      Max 
# -0.97464 -0.13658  0.01583  0.11080  1.12682 

# Coefficients:
                                         # Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                             -0.025754   0.027986  -0.920    0.360  
# change_tab$pwet[change_tab$div_poc < 2]  0.003117   0.001672   1.864    0.065 .
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.2922 on 107 degrees of freedom
  # (125 observations deleted due to missingness)
# Multiple R-squared:  0.03146,	Adjusted R-squared:  0.02241 
# F-statistic: 3.475 on 1 and 107 DF,  p-value: 0.06503

AIC(lm_pwet2) # 45.09116


## 5. Pdry diff
#---

lm_pdry2 <- lm(div_poc2 ~ change_tab $pdry_diff[change_tab$div_poc < 2])

summary(lm_pdry2)
# Call:
# lm(formula = div_poc2 ~ change_tab$pdry_diff[change_tab$div_poc < 
    # 2])

# Residuals:
     # Min       1Q   Median       3Q      Max 
# -0.93100 -0.13809  0.01227  0.09958  1.15147 

# Coefficients:
                                         # Estimate Std. Error t value Pr(>|t|)
# (Intercept)                             -0.024705   0.029066  -0.850    0.397
# change_tab$pdry[change_tab$div_poc < 2]  0.001462   0.009310   0.157    0.876

# Residual standard error: 0.2969 on 107 degrees of freedom
  # (125 observations deleted due to missingness)
# Multiple R-squared:  0.0002303,	Adjusted R-squared:  -0.009113 
# F-statistic: 0.02465 on 1 and 107 DF,  p-value: 0.8755

AIC(lm_pdry2)


## 6. Pseas diff
#---

lm_pseas2 <- lm(div_poc2 ~ change_tab $pseas_diff[change_tab$div_poc < 2])

summary(lm_pseas2)
# Call:
# lm(formula = div_poc2 ~ change_tab$pseas_diff[change_tab$div_poc < 
    # 2])

# Residuals:
     # Min       1Q   Median       3Q      Max 
# -0.94985 -0.11844  0.01683  0.11221  1.15446 

# Coefficients:
                                          # Estimate Std. Error t value Pr(>|t|)
# (Intercept)                              -0.024605   0.028306  -0.869    0.387
# change_tab$pseas[change_tab$div_poc < 2]  0.004656   0.004392   1.060    0.291

# Residual standard error: 0.2953 on 107 degrees of freedom
  # (125 observations deleted due to missingness)
# Multiple R-squared:  0.0104,	Adjusted R-squared:  0.001146 
# F-statistic: 1.124 on 1 and 107 DF,  p-value: 0.2915

AIC(lm_pseas2) # 47.43616


## 6. Log(agri PoC)
#---

lm_agri2 <- lm(div_poc2 ~ log(change_tab $Agri_poc[change_tab$div_poc < 2]+100))

summary(lm_agri2)
# Call:
# lm(formula = div_poc2 ~ log(change_tab$Agri_poc[change_tab$div_poc < 
    # 2] + 100))

# Residuals:
     # Min       1Q   Median       3Q      Max 
# -0.93366 -0.13622  0.01328  0.10062  1.15216 

# Coefficients:
                                                       # Estimate Std. Error t value Pr(>|t|)
# (Intercept)                                            -0.08561    0.17561  -0.487    0.627
# log(change_tab$Agri_poc[change_tab$div_poc < 2] + 100)  0.01190    0.03440   0.346    0.730

# Residual standard error: 0.2967 on 107 degrees of freedom
  # (125 observations deleted due to missingness)
# Multiple R-squared:  0.001118,	Adjusted R-squared:  -0.008218 
# F-statistic: 0.1197 on 1 and 107 DF,  p-value: 0.73

AIC(lm_agri2) # 48.45327


## 7. log(urb PoC)
#---

lm_urb2 <- lm(div_poc2 ~ log(change_tab $urb_poc[change_tab$div_poc < 2]+100))

summary(lm_urb2)
# Call:
# lm(formula = div_poc2 ~ log(change_tab$urb_poc[change_tab$div_poc < 
    # 2] + 100))

# Residuals:
     # Min       1Q   Median       3Q      Max 
# -0.89305 -0.11221  0.01986  0.10221  1.14500 

# Coefficients:
                                                      # Estimate Std. Error t value Pr(>|t|)
# (Intercept)                                           -0.18646    0.12978  -1.437    0.154
# log(change_tab$Urb_poc[change_tab$div_poc < 2] + 100)  0.02682    0.02113   1.269    0.207

# Residual standard error: 0.2947 on 107 degrees of freedom
  # (125 observations deleted due to missingness)
# Multiple R-squared:  0.01484,	Adjusted R-squared:  0.005631 
# F-statistic: 1.612 on 1 and 107 DF,  p-value: 0.207

AIC(lm_urb2) # 46.9457


## 8. log(art_wet PoC)
#---

lm_artwet2 <- lm(div_poc2 ~ log(change_tab $artwet_poc[change_tab$div_poc < 2]+100))

summary(lm_artwet2)
# Call:
# lm(formula = div_poc2 ~ log(change_tab$artwet_poc[change_tab$div_poc < 
    # 2] + 100))

# Residuals:
     # Min       1Q   Median       3Q      Max 
# -0.93946 -0.11149  0.01402  0.09697  1.14301 

# Coefficients:
                                                      # Estimate Std. Error t value Pr(>|t|)
# (Intercept)                                            0.09393    0.19321   0.486    0.628
# log(change_tab$Art_poc[change_tab$div_poc < 2] + 100) -0.02398    0.03833  -0.626    0.533

# Residual standard error: 0.2964 on 107 degrees of freedom
  # (125 observations deleted due to missingness)
# Multiple R-squared:  0.003646,	Adjusted R-squared:  -0.005666 
# F-statistic: 0.3915 on 1 and 107 DF,  p-value: 0.5328

AIC(lm_artwet2) # 48.17705


## 9. log(sea PoC)
#---

lm_sea2 <- lm(div_poc2 ~ log(change_tab $sea_poc[change_tab$div_poc < 2]+100))

summary(lm_sea2)
# Call:
# lm(formula = div_poc2 ~ log(change_tab$sea_poc[change_tab$div_poc < 
    # 2] + 100))

# Residuals:
     # Min       1Q   Median       3Q      Max 
# -0.92448 -0.13025  0.00768  0.10167  1.15412 

# Coefficients:
                                                      # Estimate Std. Error t value Pr(>|t|)
# (Intercept)                                           -0.17359    0.19122  -0.908    0.366
# log(change_tab$Sea_poc[change_tab$div_poc < 2] + 100)  0.03085    0.03944   0.782    0.436

# Residual standard error: 0.296 on 107 degrees of freedom
  # (125 observations deleted due to missingness)
# Multiple R-squared:  0.005687,	Adjusted R-squared:  -0.003606 
# F-statistic: 0.612 on 1 and 107 DF,  p-value: 0.4358

AIC(lm_sea2) # 47.95351


## 10. log(Natwet loss)
#---

lm_natwet2 <- lm(div_poc2 ~ log(natwat_glm_poi[change_tab$div_poc < 2]+1))
summary(lm_natwet2)
# Call:
# lm(formula = div_poc2 ~ log(natwat_glm_poi[change_tab$div_poc < 
    # 2] + 1))

# Residuals:
     # Min       1Q   Median       3Q      Max 
# -0.76766 -0.10218  0.02043  0.09811  1.11111 

# Coefficients:
                                                # Estimate Std. Error t value Pr(>|t|)
# (Intercept)                                     -0.05755    0.05386  -1.069    0.288
# log(natwat_glm_poi[change_tab$div_poc < 2] + 1)  0.02438    0.02484   0.981    0.329

# Residual standard error: 0.2742 on 99 degrees of freedom
  # (133 observations deleted due to missingness)
# Multiple R-squared:  0.009632,	Adjusted R-squared:  -0.0003715 
# F-statistic: 0.9629 on 1 and 99 DF,  p-value: 0.3289

AIC(lm_natwet2) # 29.23573
