
## 06/2018 - Leberger et al. 2018. Mediterranean wetland conservation in the context of climate and land-cover change.


### 4.1. Beta-Sorensen response to several variables using beta-regression model
###############################################

# Load table
change_tab <- read.table('/Users/localadmin/Documents/PhD/Paper3_Wetlands/Data_to_publish/Table_all.txt', h=T)
head(change_tab)

# Replacement of 0 and 1 values by very closed ones in Beta_sor_1991_2010 to apply the betareg function ]0:1[
transform_Betasor <- (change_tab $Beta_sor_1991_2010*(length(change_tab $Beta_sor_1991_2010)-1)+0.5)/length(change_tab $Beta_sor_1991_2010)

# Loading library
library(betareg)

## 1. Tmean diff
#-----

beta_Betasor_tmean <- betareg(transform_Betasor ~ tmean_diff, data= change_tab, link='logit')
summary(beta_Betasor_tmean)
# Call:
# betareg(formula = transform_Betasor ~ tmean_diff, data = change_tab, link = "logit")
#
# Standardized weighted residuals 2:
    # Min      1Q  Median      3Q     Max 
# -1.7207 -0.7287 -0.0448  0.4980  5.3652 
#
# Coefficients (mean model with logit link):
            # Estimate Std. Error z value Pr(>|z|)
# (Intercept)   0.1116     0.1892   0.590    0.555
# tmean_diff   -0.4131     0.3394  -1.217    0.224
#
# Phi coefficients (precision model with identity link):
      # Estimate Std. Error z value Pr(>|z|)    
# (phi)   3.5733     0.4274    8.36   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#
# Type of estimator: ML (maximum likelihood)
# Log-likelihood: 10.78 on 3 Df
# Pseudo R-squared: 0.01149
# Number of iterations: 14 (BFGS) + 2 (Fisher scoring) 


## 2. Tmax diff
#-----

beta_Betasor_tmax <- betareg(transform_Betasor ~ tmax_diff, data= change_tab, link='logit')
summary(beta_Betasor_tmax)
# Call:
# betareg(formula = transform_Betasor ~ tmax_diff, data = change_tab, link = "logit")

# Standardized weighted residuals 2:
    # Min      1Q  Median      3Q     Max 
# -1.7449 -0.6624 -0.0650  0.5088  5.4496 

# Coefficients (mean model with logit link):
            # Estimate Std. Error z value Pr(>|z|)   
# (Intercept)   0.2439     0.1385   1.761  0.07822 . 
# tmax_diff    -0.3355     0.1090  -3.077  0.00209 **

# Phi coefficients (precision model with identity link):
      # Estimate Std. Error z value Pr(>|z|)    
# (phi)   3.8343     0.4627   8.287   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

# Type of estimator: ML (maximum likelihood)
# Log-likelihood: 14.62 on 3 Df
# Pseudo R-squared: 0.06918
# Number of iterations: 14 (BFGS) + 1 (Fisher scoring)


## 3. Tmin diff
#-----

beta_Betasor_tmin <- betareg(transform_Betasor ~ tmin_diff, data= change_tab, link='logit')
summary(beta_Betasor_tmin)
# Call:
# betareg(formula = transform_Betasor ~ tmin_diff, data = change_tab, link = "logit")

# Standardized weighted residuals 2:
    # Min      1Q  Median      3Q     Max 
# -1.8279 -0.7021 -0.0861  0.5261  5.1307 

# Coefficients (mean model with logit link):
            # Estimate Std. Error z value Pr(>|z|)
# (Intercept) -0.14710    0.13605  -1.081    0.280
# tmin_diff    0.02918    0.05595   0.522    0.602

# Phi coefficients (precision model with identity link):
      # Estimate Std. Error z value Pr(>|z|)    
# (phi)   3.5344     0.4222   8.372   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

# Type of estimator: ML (maximum likelihood)
# Log-likelihood: 10.18 on 3 Df
# Pseudo R-squared: 0.002172
# Number of iterations: 14 (BFGS) + 1 (Fisher scoring) 


## 4. Tseas diff
#-----

beta_Betasor_tseas <- betareg(transform_Betasor ~ tseas_diff, data= change_tab, link='logit')
summary(beta_Betasor_tseas)
# Call:
# betareg(formula = transform_Betasor ~ tseas_diff, data = change_tab, link = "logit")

# Standardized weighted residuals 2:
    # Min      1Q  Median      3Q     Max 
# -1.8170 -0.7311 -0.1321  0.5155  5.1242 

# Coefficients (mean model with logit link):
            # Estimate Std. Error z value Pr(>|z|)
# (Intercept)  -0.1048     0.1006  -1.042    0.297
# tseas_diff    0.1264     0.5224   0.242    0.809

# Phi coefficients (precision model with identity link):
      # Estimate Std. Error z value Pr(>|z|)    
# (phi)   3.5275     0.4212   8.374   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

# Type of estimator: ML (maximum likelihood)
# Log-likelihood: 10.07 on 3 Df
# Pseudo R-squared: 0.0004703
# Number of iterations: 13 (BFGS) + 1 (Fisher scoring) 


## 4. Pmean diff
#-----

beta_Betasor_pmean <- betareg(transform_Betasor ~ pmean_diff, data= change_tab, link='logit')
summary(beta_Betasor_pmean)
# Call:
# betareg(formula = transform_Betasor ~ pmean_diff, data = change_tab, link = "logit")

# Standardized weighted residuals 2:
    # Min      1Q  Median      3Q     Max 
# -1.9152 -0.6713 -0.0743  0.5021  5.2531 

# Coefficients (mean model with logit link):
             # Estimate Std. Error z value Pr(>|z|)
# (Intercept) -0.060273   0.093441  -0.645    0.519
# pmean_diff  -0.001025   0.001028  -0.997    0.319

# Phi coefficients (precision model with identity link):
      # Estimate Std. Error z value Pr(>|z|)    
# (phi)   3.5581     0.4254   8.365   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

# Type of estimator: ML (maximum likelihood)
# Log-likelihood: 10.55 on 3 Df
# Pseudo R-squared: 0.007927
# Number of iterations: 14 (BFGS) + 1 (Fisher scoring) 

## 5. Pdry diff
#-----

beta_Betasor_pdry <- betareg(transform_Betasor ~ pdry_diff, data= change_tab, link='logit')
summary(beta_Betasor_pdry)
# Call:
# betareg(formula = transform_Betasor ~ pdry_diff, data = change_tab, link = "logit")

# Standardized weighted residuals 2:
    # Min      1Q  Median      3Q     Max 
# -1.8415 -0.7419 -0.0960  0.5102  5.1246 

# Coefficients (mean model with logit link):
            # Estimate Std. Error z value Pr(>|z|)
# (Intercept) -0.08492    0.08948  -0.949    0.343
# pdry_diff    0.01252    0.02891   0.433    0.665

# Phi coefficients (precision model with identity link):
      # Estimate Std. Error z value Pr(>|z|)    
# (phi)   3.5316     0.4218   8.373   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

# Type of estimator: ML (maximum likelihood)
# Log-likelihood: 10.14 on 3 Df
# Pseudo R-squared: 0.001482
# Number of iterations: 13 (BFGS) + 1 (Fisher scoring) 

## 6. Pwet diff
#-----

beta_Betasor_pwet <- betareg(transform_Betasor ~ pwet_diff, data= change_tab, link='logit')
summary(beta_Betasor_pwet)
# Call:
# betareg(formula = transform_Betasor ~ pwet_diff, data = change_tab, link = "logit")

# Standardized weighted residuals 2:
    # Min      1Q  Median      3Q     Max 
# -1.8945 -0.7206 -0.0411  0.5011  5.2317 

# Coefficients (mean model with logit link):
             # Estimate Std. Error z value Pr(>|z|)
# (Intercept) -0.091857   0.087618  -1.048    0.294
# pwet_diff   -0.002039   0.005087  -0.401    0.689

# Phi coefficients (precision model with identity link):
      # Estimate Std. Error z value Pr(>|z|)    
# (phi)   3.5307     0.4217   8.373   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

# Type of estimator: ML (maximum likelihood)
# Log-likelihood: 10.12 on 3 Df
# Pseudo R-squared: 0.001243
# Number of iterations: 14 (BFGS) + 1 (Fisher scoring) 

## 7. Pseas diff
#-----

beta_Betasor_pseas <- betareg(transform_Betasor ~ pseas_diff, data= change_tab, link='logit')
summary(beta_Betasor_pseas)
# Call:
# betareg(formula = transform_Betasor ~ pseas_diff, data = change_tab, link = "logit")

# Standardized weighted residuals 2:
    # Min      1Q  Median      3Q     Max 
# -1.8808 -0.7072 -0.0422  0.5298  5.1163 

# Coefficients (mean model with logit link):
            # Estimate Std. Error z value Pr(>|z|)
# (Intercept) -0.09148    0.08714  -1.050    0.294
# pseas_diff   0.01846    0.01349   1.368    0.171

# Phi coefficients (precision model with identity link):
      # Estimate Std. Error z value Pr(>|z|)    
# (phi)   3.5867     0.4292   8.356   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

# Type of estimator: ML (maximum likelihood)
# Log-likelihood: 10.98 on 3 Df
# Pseudo R-squared: 0.01486
# Number of iterations: 13 (BFGS) + 1 (Fisher scoring)

## 1. Urb PoC
#-----

beta_Betasor_tseas <- betareg(transform_Betasor ~ change_tab $urb_poc, data= change_tab, link='logit')
summary(beta_Betasor_tseas)
# Call:
# betareg(formula = transform_Betasor ~ change_tab$urb_poc, data = change_tab, link = "logit")

# Standardized weighted residuals 2:
    # Min      1Q  Median      3Q     Max 
# -1.8416 -0.7182 -0.0981  0.5158  5.2053 

# Coefficients (mean model with logit link):
                     # Estimate Std. Error z value Pr(>|z|)
# (Intercept)        -1.105e-01  8.758e-02  -1.262    0.207
# change_tab$urb_poc  4.226e-06  2.944e-06   1.435    0.151

# Phi coefficients (precision model with identity link):
      # Estimate Std. Error z value Pr(>|z|)    
# (phi)   3.6053     0.4317   8.351   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

# Type of estimator: ML (maximum likelihood)
# Log-likelihood: 11.28 on 3 Df
# Pseudo R-squared: 0.02137
# Number of iterations: 13 (BFGS) + 1 (Fisher scoring) 



beta_Betasor_Urb_log <- betareg(transform_Betasor ~ log(change_tab $urb_poc+1), data= change_tab, link='logit')
summary(beta_Betasor_Urb_log)
# Call:
# betareg(formula = transform_Betasor ~ log(change_tab$urb_poc + 1), data = change_tab, link = "logit")

# Standardized weighted residuals 2:
    # Min      1Q  Median      3Q     Max 
# -1.8217 -0.6293 -0.0983  0.4652  5.1357 

# Coefficients (mean model with logit link):
                            # Estimate Std. Error z value Pr(>|z|)
# (Intercept)                 -0.32730    0.20251  -1.616    0.106
# log(change_tab$urb_poc + 1)  0.04656    0.03630   1.282    0.200

# Phi coefficients (precision model with identity link):
      # Estimate Std. Error z value Pr(>|z|)    
# (phi)   3.5791     0.4282   8.358   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

# Type of estimator: ML (maximum likelihood)
# Log-likelihood: 10.87 on 3 Df
# Pseudo R-squared: 0.01296
# Number of iterations: 14 (BFGS) + 1 (Fisher scoring) 

## 2. Agri PoC
#-----

beta_Betasor_agri <- betareg(transform_Betasor ~ change_tab $agri_poc, data= change_tab, link='logit')
summary(beta_Betasor_agri)
# Call:
# betareg(formula = transform_Betasor ~ change_tab$agri_poc, data = change_tab, link = "logit")

# Standardized weighted residuals 2:
    # Min      1Q  Median      3Q     Max 
# -1.9139 -0.6463 -0.0701  0.4864  5.2395 

# Coefficients (mean model with logit link):
                      # Estimate Std. Error z value Pr(>|z|)  
# (Intercept)         -0.0039839  0.0945185  -0.042   0.9664  
# change_tab$agri_poc -0.0007127  0.0003271  -2.178   0.0294 *

# Phi coefficients (precision model with identity link):
      # Estimate Std. Error z value Pr(>|z|)    
# (phi)    3.696      0.444   8.324   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

# Type of estimator: ML (maximum likelihood)
# Log-likelihood: 12.67 on 3 Df
# Pseudo R-squared: 0.04391
# Number of iterations: 13 (BFGS) + 1 (Fisher scoring) 


beta_Betasor_agri_log <- betareg(transform_Betasor ~ log(change_tab $agri_poc+100), data= change_tab, link='logit')
summary(beta_Betasor_agri_log)
# Call:
# betareg(formula = transform_Betasor ~ log(change_tab$agri_poc + 100), data = change_tab, link = "logit")

# Standardized weighted residuals 2:
    # Min      1Q  Median      3Q     Max 
# -1.8615 -0.7155 -0.1009  0.5422  5.2142 

# Coefficients (mean model with logit link):
                               # Estimate Std. Error z value Pr(>|z|)
# (Intercept)                      0.7556     0.5400   1.399    0.162
# log(change_tab$agri_poc + 100)  -0.1687     0.1061  -1.589    0.112

# Phi coefficients (precision model with identity link):
      # Estimate Std. Error z value Pr(>|z|)    
# (phi)   3.6094     0.4323   8.349   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

# Type of estimator: ML (maximum likelihood)
# Log-likelihood: 11.34 on 3 Df
# Pseudo R-squared: 0.02072
# Number of iterations: 14 (BFGS) + 1 (Fisher scoring) 


## 3. Artificial wetlands PoC
#-----

beta_Betasor_artwet <- betareg(transform_Betasor ~ change_tab $artwet_poc, data= change_tab, link='logit')
summary(beta_Betasor_artwet)
# Call:
# betareg(formula = transform_Betasor ~ change_tab$artwet_poc, data = change_tab, link = "logit")

# Standardized weighted residuals 2:
    # Min      1Q  Median      3Q     Max 
# -1.8258 -0.7236 -0.1017  0.5282  5.1461 

# Coefficients (mean model with logit link):
                     # Estimate Std. Error z value Pr(>|z|)
# (Intercept)        -1.059e-01  8.920e-02  -1.187    0.235
# change_tab$artwet_poc  7.612e-05  1.062e-04   0.717    0.474

# Phi coefficients (precision model with identity link):
      # Estimate Std. Error z value Pr(>|z|)    
# (phi)   3.5433     0.4234   8.369   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

# Type of estimator: ML (maximum likelihood)
# Log-likelihood: 10.32 on 3 Df
# Pseudo R-squared: 0.004466
# Number of iterations: 15 (BFGS) + 2 (Fisher scoring) 


beta_Betasor_artwet_log <- betareg(transform_Betasor ~ log(change_tab $artwet_poc +30), data= change_tab, link='logit')
summary(beta_Betasor_artwet_log)
# Call:
# betareg(formula = transform_Betasor ~ log(change_tab$artwet_poc + 30), data = change_tab, link = "logit")

# Standardized weighted residuals 2:
    # Min      1Q  Median      3Q     Max 
# -1.8738 -0.6951 -0.0473  0.5052  5.1085 

# Coefficients (mean model with logit link):
                             # Estimate Std. Error z value Pr(>|z|)
# (Intercept)                   0.09056    0.30921   0.293    0.770
# log(change_tab$artwet_poc + 30) -0.04557    0.07371  -0.618    0.536

# Phi coefficients (precision model with identity link):
      # Estimate Std. Error z value Pr(>|z|)    
# (phi)   3.5377     0.4226   8.371   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

# Type of estimator: ML (maximum likelihood)
# Log-likelihood: 10.23 on 3 Df
# Pseudo R-squared: 0.002943
# Number of iterations: 13 (BFGS) + 1 (Fisher scoring) 


## 4. Sea wetlands PoC
#-----

beta_Betasor_sea <- betareg(transform_Betasor ~ change_tab $sea_poc, data= change_tab, link='logit')
summary(beta_Betasor_sea)
# Call:
# betareg(formula = transform_Betasor ~ change_tab$sea_poc, data = change_tab, link = "logit")

# Standardized weighted residuals 2:
    # Min      1Q  Median      3Q     Max 
# -1.8302 -0.7214 -0.1094  0.5187  5.1247 

# Coefficients (mean model with logit link):
                     # Estimate Std. Error z value Pr(>|z|)
# (Intercept)        -9.768e-02  8.936e-02  -1.093    0.274
# change_tab$sea_poc  4.066e-05  1.506e-04   0.270    0.787

# Phi coefficients (precision model with identity link):
      # Estimate Std. Error z value Pr(>|z|)    
# (phi)   3.5279     0.4213   8.374   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

# Type of estimator: ML (maximum likelihood)
# Log-likelihood: 10.08 on 3 Df
# Pseudo R-squared: 0.0005832
# Number of iterations: 13 (BFGS) + 2 (Fisher scoring) 


beta_Betasor_sea_log <- betareg(transform_Betasor ~ log(change_tab $sea_poc+100), data= change_tab, link='logit')
summary(beta_Betasor_sea_log)
# Call:
# betareg(formula = transform_Betasor ~ log(change_tab$sea_poc + 100), data = change_tab, link = "logit")

# Standardized weighted residuals 2:
    # Min      1Q  Median      3Q     Max 
# -1.8276 -0.7228 -0.1065  0.5051  5.1286 

# Coefficients (mean model with logit link):
                              # Estimate Std. Error z value Pr(>|z|)
# (Intercept)                   -0.28139    0.59501  -0.473    0.636
# log(change_tab$sea_poc + 100)  0.03931    0.12271   0.320    0.749

# Phi coefficients (precision model with identity link):
      # Estimate Std. Error z value Pr(>|z|)    
# (phi)   3.5289     0.4214   8.374   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

# Type of estimator: ML (maximum likelihood)
# Log-likelihood:  10.1 on 3 Df
# Pseudo R-squared: 0.0008172
# Number of iterations: 14 (BFGS) + 2 (Fisher scoring) 



## 5. Natural wetlands PoC
#-----

summary(change_tab $natwet_poc)
beta_Betasor_sea_log <- betareg(transform_Betasor ~ log(natwet_poc +2), data= change_tab, link='logit')
summary(beta_Betasor_sea_log)
# Call:
# betareg(formula = transform_Betasor ~ log(natwet_poc + 2), data = change_tab, link = "logit")

# Standardized weighted residuals 2:
    # Min      1Q  Median      3Q     Max 
# -1.8040 -0.7270 -0.0908  0.5243  5.1445 

# Coefficients (mean model with logit link):
                    # Estimate Std. Error z value Pr(>|z|)
# (Intercept)           0.2931     0.5134   0.571    0.568
# log(natwet_poc + 2)  -0.6054     0.7932  -0.763    0.445

# Phi coefficients (precision model with identity link):
      # Estimate Std. Error z value Pr(>|z|)    
# (phi)   3.5445     0.4235   8.369   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

# Type of estimator: ML (maximum likelihood)
# Log-likelihood: 10.34 on 3 Df
# Pseudo R-squared: 0.004649
# Number of iterations: 12 (BFGS) + 1 (Fisher scoring) 


beta_Betasor_loss_log <- betareg(transform_Betasor ~ log(natwat_glm_poi+1), link='logit')
summary(beta_Betasor_loss_log)
# Call:
# betareg(formula = transform_Betasor ~ log(natwat_glm_poi + 1), link = "logit")

# Standardized weighted residuals 2:
    # Min      1Q  Median      3Q     Max 
# -1.6372 -0.8261 -0.1134  0.5499  4.9889 

# Coefficients (mean model with logit link):
                        # Estimate Std. Error z value Pr(>|z|)  
# (Intercept)             -0.30803    0.18347  -1.679   0.0932 .
# log(natwat_glm_poi + 1)  0.10467    0.08438   1.240   0.2148  

# Phi coefficients (precision model with identity link):
      # Estimate Std. Error z value Pr(>|z|)    
# (phi)   3.3984     0.4202   8.088 6.07e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

# Type of estimator: ML (maximum likelihood)
# Log-likelihood: 8.782 on 3 Df
# Pseudo R-squared: 0.01306
# Number of iterations: 14 (BFGS) + 1 (Fisher scoring) 

