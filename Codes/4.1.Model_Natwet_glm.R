

## 06/2018 - Leberger et al. 2018. Mediterranean wetland conservation in the context of climate and land-cover change.


### 4.1. Natural wetland response to several variables using glm Poisson
###############################################

change_tab <- read.table('/Users/localadmin/Documents/PhD/Paper3_Wetlands/Data_to_publish/Table_all.txt', h=T)
head(change_tab)

## Transform Natural wetland to apply poisson distribution: Natural wetland loss

natwat_change <- change_tab

# Change values of Natwet=0 to include no change within the analysis
natwat_change$Natwet_poc[natwat_change$Natwet_poc >= 0] <- NA # 36 sites removed... (31 = 0 Natwet change)
natwat_glm_poi <- round(natwat_change $Natwet_poc*-100)

# Distribution
hist(natwat_glm_poi)


## 1.1. Tmean diff
#---

glm_poi_tmean <- glm(natwat_glm_poi ~ change_tab $tmean_diff, family='poisson')
summary(glm_poi_tmean)
# Call:
# glm(formula = natwat_glm_poi ~ change_tab$tmean_diff, family = "poisson")
#
# Deviance Residuals: 
   # Min      1Q  Median      3Q     Max  
# -5.898  -4.153  -2.604   1.254  15.834  
#
# Coefficients:
                      # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)            2.36267    0.04615  51.198   <2e-16 ***
# change_tab$tmean_diff  0.60263    0.06613   9.113   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for poisson family taken to be 1)
#
    # Null deviance: 4402.0  on 199  degrees of freedom
# Residual deviance: 4318.1  on 198  degrees of freedom
  # (36 observations deleted due to missingness)
# AIC: 5051.2
#
# Number of Fisher Scoring iterations: 6


## 1.2. Tmean diff : PA decreasing cover
#---

# Hypothesis: Natural wetland loss increase in area with increasing temperatures and low protection level

# less protected area : higher numbers as they should be correlated with wetland loss (hypothesis)
pa_level_inv <- change_tab$pa_level
pa_level_inv[change_tab$pa_level == 100] <- 1
pa_level_inv[change_tab$pa_level == 50] <- 2
pa_level_inv[change_tab$pa_level == 0] <- 3

glm_poi_tmean_pa <- glm(natwat_glm_poi ~ change_tab $tmean_diff : pa_level_inv, family='poisson')
summary(glm_poi_tmean_pa)
# Call:
# glm(formula = natwat_glm_poi ~ change_tab$tmean_diff:pa_level_inv, 
    # family = "poisson")
#
# Deviance Residuals: 
   # Min      1Q  Median      3Q     Max  
# -6.700  -4.045  -2.265   1.342  16.212  
#
# Coefficients:
                                   # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                         2.32388    0.03286   70.72   <2e-16 ***
# change_tab$tmean_diff:pa_level_inv  0.32041    0.01932   16.59   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for poisson family taken to be 1)
#
    # Null deviance: 4402.0  on 199  degrees of freedom
# Residual deviance: 4133.5  on 198  degrees of freedom
  # (36 observations deleted due to missingness)
# AIC: 4866.5
#
# Number of Fisher Scoring iterations: 6

	# => Hypothesis confirmed !


## 1.3. PA decreasing cover
#---

glm_poi_pa <- glm(natwat_glm_poi ~ pa_level_inv, family='poisson')
summary(glm_poi_pa)
# Call:
# glm(formula = natwat_glm_poi ~ pa_level_inv, family = "poisson")

# Deviance Residuals: 
   # Min      1Q  Median      3Q     Max  
# -6.611  -4.041  -2.192   1.216  15.930  


# Coefficients:
             # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   2.13658    0.04386   48.71   <2e-16 ***
# pa_level_inv  0.31593    0.01971   16.03   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for poisson family taken to be 1)

    # Null deviance: 4402.0  on 199  degrees of freedom
# Residual deviance: 4146.9  on 198  degrees of freedom
  # (36 observations deleted due to missingness)
# AIC: 4880

# Number of Fisher Scoring iterations: 5



## 2. Pmean diff
#----

glm_poi_pmean <- glm(natwat_glm_poi ~ change_tab $pmean_diff, family='poisson')
summary(glm_poi_pmean)
# Call:
# glm(formula = natwat_glm_poi ~ change_tab$pmean_diff, family = "poisson")

# Deviance Residuals: 
   # Min      1Q  Median      3Q     Max  
# -6.875  -4.211  -2.509   1.197  14.855  

# Coefficients:
                        # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)            2.7410462  0.0180220 152.094   <2e-16 ***
# change_tab$pmean_diff -0.0019673  0.0002263  -8.692   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for poisson family taken to be 1)

    # Null deviance: 4402.0  on 199  degrees of freedom
# Residual deviance: 4326.4  on 198  degrees of freedom
  # (36 observations deleted due to missingness)
# AIC: 5059.4

# Number of Fisher Scoring iterations: 6

## Pmean diff : pa_level
#----

glm_poi_pmean_pa <- glm(natwat_glm_poi ~ change_tab $pmean_diff : pa_level_inv, family='poisson')
summary(glm_poi_pmean_pa)
# Call:
# glm(formula = natwat_glm_poi ~ change_tab$pmean_diff:pa_level_inv, 
    # family = "poisson")

# Deviance Residuals: 
   # Min      1Q  Median      3Q     Max  
# -6.414  -4.155  -2.483   1.313  15.267  

# Coefficients:
                                     # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                         2.7169321  0.0183598 147.983   <2e-16 ***
# change_tab$pmean_diff:pa_level_inv -0.0014323  0.0001491  -9.608   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for poisson family taken to be 1)

    # Null deviance: 4402.0  on 199  degrees of freedom
# Residual deviance: 4307.5  on 198  degrees of freedom
  # (36 observations deleted due to missingness)
# AIC: 5040.6

# Number of Fisher Scoring iterations: 6


## 2.2.Tmean diff : Pmean diff
#----

glm_poi_tpmean <- glm(natwat_glm_poi ~ change_tab $tmean_diff: change_tab $pmean_diff, family='poisson')
summary(glm_poi_tpmean)
# Call:
# glm(formula = natwat_glm_poi ~ change_tab$tmean_diff:change_tab$pmean_diff, 
    # family = "poisson")
# Deviance Residuals: 
   # Min      1Q  Median      3Q     Max  
# -7.090  -4.124  -2.465   1.150  14.840  
# Coefficients:
                                              # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                  2.7001118  0.0186679  144.64   <2e-16 ***
# change_tab$tmean_diff:change_tab$pmean_diff -0.0045495  0.0003691  -12.33   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Dispersion parameter for poisson family taken to be 1)
    # Null deviance: 4402.0  on 199  degrees of freedom
# Residual deviance: 4249.3  on 198  degrees of freedom
  # (36 observations deleted due to missingness)
# AIC: 4982.3
# Number of Fisher Scoring iterations: 6


cor.test(change_tab $tmean_diff, change_tab $pmean_diff)
	# Pearson's product-moment correlation
# data:  change_tab$tmean_diff and change_tab$pmean_diff
# t = -5.2596, df = 234, p-value = 3.256e-07
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
 # -0.4347966 -0.2060027
# sample estimates:
       # cor 
# -0.3251496 


## 3. Tmax diff
#----

glm_poi_tmax <- glm(natwat_glm_poi ~ change_tab $tmax_diff, family='poisson')
summary(glm_poi_tmax)
# Call:
# glm(formula = natwat_glm_poi ~ change_tab$tmax, family = "poisson")

# Deviance Residuals: 
    # Min       1Q   Median       3Q      Max  
# -7.3404  -4.1107  -2.2838   0.6346  15.6854  

# Coefficients:
                # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      2.92131    0.02207  132.36   <2e-16 ***
# change_tab$tmax -0.25338    0.02021  -12.54   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Dispersion parameter for poisson family taken to be 1)

    # Null deviance: 4402.0  on 199  degrees of freedom
# Residual deviance: 4248.7  on 198  degrees of freedom
  # (36 observations deleted due to missingness)
# AIC: 4981.7
# Number of Fisher Scoring iterations: 6


## 4. Tmin diff
#----

glm_poi_tmin <- glm(natwat_glm_poi ~ change_tab $tmin_diff, family='poisson')
summary(glm_poi_tmin)
# Call:
# glm(formula = natwat_glm_poi ~ change_tab$tmin, family = "poisson")
# Deviance Residuals: 
   # Min      1Q  Median      3Q     Max  
# -5.805  -4.170  -2.636   1.080  15.675  

# Coefficients:
                # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      2.53936    0.03246  78.229  < 2e-16 ***
# change_tab$tmin  0.08484    0.01111   7.637 2.23e-14 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Dispersion parameter for poisson family taken to be 1)
    # Null deviance: 4402.0  on 199  degrees of freedom
# Residual deviance: 4344.5  on 198  degrees of freedom
  # (36 observations deleted due to missingness)
# AIC: 5077.6
# Number of Fisher Scoring iterations: 6


## 5. Tseas diff
#----

glm_poi_tseas <- glm(natwat_glm_poi ~ change_tab $tseas_diff, family='poisson')
summary(glm_poi_tseas)
# Call:
# glm(formula = natwat_glm_poi ~ change_tab$tseas, family = "poisson")
# Deviance Residuals: 
   # Min      1Q  Median      3Q     Max  
# -7.797  -3.734  -2.401   1.269  16.047  
# Coefficients:
                 # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       2.72943    0.01828  149.29   <2e-16 ***
# change_tab$tseas -1.61578    0.09363  -17.26   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for poisson family taken to be 1)
    # Null deviance: 4402.0  on 199  degrees of freedom
# Residual deviance: 4104.1  on 198  degrees of freedom
  # (36 observations deleted due to missingness)
# AIC: 4837.2
# Number of Fisher Scoring iterations: 6


## 6. Pdry diff
#----

glm_poi_pdry <- glm(natwat_glm_poi ~ change_tab $pdry_diff, family='poisson')
summary(glm_poi_pdry)
# Call:
# glm(formula = natwat_glm_poi ~ change_tab$pdry_diff, family = "poisson")

# Deviance Residuals: 
    # Min       1Q   Median       3Q      Max  
# -5.6265  -4.2627  -2.6775   0.8577  14.5189  

# Coefficients:
                     # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           2.74223    0.01811 151.437   <2e-16 ***
# change_tab$pdry_diff  0.01504    0.00601   2.503   0.0123 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for poisson family taken to be 1)

    # Null deviance: 4402.0  on 199  degrees of freedom
# Residual deviance: 4395.8  on 198  degrees of freedom
  # (36 observations deleted due to missingness)
# AIC: 5128.9

# Number of Fisher Scoring iterations: 6


## 7. Pwet diff
#----

glm_poi_pwet <- glm(natwat_glm_poi ~ change_tab $pwet_diff, family='poisson')
summary(glm_poi_pwet)
# Call:
# glm(formula = natwat_glm_poi ~ change_tab$pwet_diff, family = "poisson")

# Deviance Residuals: 
    # Min       1Q   Median       3Q      Max  
# -5.6303  -4.3159  -2.7273   0.9185  14.3256  

# Coefficients:
                       # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           2.7343406  0.0182361 149.941   <2e-16 ***
# change_tab$pwet_diff -0.0008129  0.0011550  -0.704    0.482    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for poisson family taken to be 1)

    # Null deviance: 4402.0  on 199  degrees of freedom
# Residual deviance: 4401.5  on 198  degrees of freedom
  # (36 observations deleted due to missingness)
# AIC: 5134.6
# Number of Fisher Scoring iterations: 6


## 8. Pseas diff
#----

glm_poi_pseas <- glm(natwat_glm_poi ~ change_tab $pseas_diff, family='poisson')
summary(glm_poi_pseas)
# Call:
# glm(formula = natwat_glm_poi ~ change_tab$pseas_diff, family = "poisson")

# Deviance Residuals: 
    # Min       1Q   Median       3Q      Max  
# -6.2570  -4.2531  -2.4307   0.6487  14.5960  

# Coefficients:
                      # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           2.731207   0.018093 150.955  < 2e-16 ***
# change_tab$pseas_diff 0.011645   0.002225   5.233 1.67e-07 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for poisson family taken to be 1)

    # Null deviance: 4402.0  on 199  degrees of freedom
# Residual deviance: 4374.8  on 198  degrees of freedom
  # (36 observations deleted due to missingness)
# AIC: 5107.9
# Number of Fisher Scoring iterations: 6


## 1. Urb PoC
#----

glm_poi_urb <- glm(natwat_glm_poi ~ change_tab $urb_poc, family='poisson')
summary(glm_poi_urb)
# Call:
# glm(formula = natwat_glm_poi ~ change_tab$urb_poc, family = "poisson")

# Deviance Residuals: 
    # Min       1Q   Median       3Q      Max  
# -5.5734  -4.3420  -2.7473   0.8658  14.2718  

# Coefficients:
                     # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         2.743e+00  1.811e-02 151.444   <2e-16 ***
# change_tab$urb_poc -2.997e-06  1.175e-06  -2.551   0.0108 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for poisson family taken to be 1)

    # Null deviance: 4402  on 199  degrees of freedom
# Residual deviance: 4393  on 198  degrees of freedom
  # (36 observations deleted due to missingness)
# AIC: 5126.1
# Number of Fisher Scoring iterations: 6

## With Logarithm:

glm_poi_urb_log <- glm(natwat_glm_poi ~ log(change_tab $urb_poc+1), family='poisson')
summary(glm_poi_urb_log)
# Call:
# glm(formula = natwat_glm_poi ~ log(change_tab$urb_poc + 1), family = "poisson")
# Deviance Residuals: 
   # Min      1Q  Median      3Q     Max  
# -8.219  -3.948  -2.321   0.949  15.641  
# Coefficients:
                            # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                 2.006029   0.047299   42.41   <2e-16 ***
# log(change_tab$urb_poc + 1) 0.135896   0.007572   17.95   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for poisson family taken to be 1)
    # Null deviance: 4402.0  on 199  degrees of freedom
# Residual deviance: 4064.2  on 198  degrees of freedom
  # (36 observations deleted due to missingness)
# AIC: 4797.2
# Number of Fisher Scoring iterations: 6

##  => Logarithm : improve AIC greatly ! (and change slope)



## 2. Agri PoC
#----

glm_poi_agri <- glm(natwat_glm_poi ~ change_tab $agri_poc, family='poisson')
summary(glm_poi_agri)
# Call:
# glm(formula = natwat_glm_poi ~ change_tab$agri_poc, family = "poisson")
# Deviance Residuals: 
    # Min       1Q   Median       3Q      Max  
# -5.5728  -4.3360  -2.7172   0.8613  14.3159  
# Coefficients:
                      # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          2.743e+00  1.900e-02 144.365   <2e-16 ***
# change_tab$agri_poc -2.525e-05  2.496e-05  -1.012    0.312    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for poisson family taken to be 1)
    # Null deviance: 4402.0  on 199  degrees of freedom
# Residual deviance: 4400.9  on 198  degrees of freedom
  # (36 observations deleted due to missingness)
# AIC: 5134
# Number of Fisher Scoring iterations: 6

## With Logarithm:

hist(log(change_tab $agri_poc+100), breaks=100)

glm_poi_urb_log <- glm(natwat_glm_poi ~ log(change_tab $agri_poc+100), family='poisson')
summary(glm_poi_urb_log)
# Call:
# glm(formula = natwat_glm_poi ~ log(change_tab$agri_poc + 100), 
    # family = "poisson")
# Deviance Residuals: 
    # Min       1Q   Median       3Q      Max  
# -5.7379  -4.3080  -2.7002   0.8493  14.6336  
# Coefficients:
                               # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                     2.62811    0.09756  26.939   <2e-16 ***
# log(change_tab$agri_poc + 100)  0.02068    0.01829   1.131    0.258    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for poisson family taken to be 1)
    # Null deviance: 4402.0  on 199  degrees of freedom
# Residual deviance: 4400.7  on 198  degrees of freedom
  # (36 observations deleted due to missingness)
# AIC: 5133.8
# Number of Fisher Scoring iterations: 6

## => Logarithm : improve AIC slightly + change sign 

plot(natwat_glm_poi ~ log(change_tab $agri_poc+100))


## 3. Art_wet PoC
#----

glm_poi_artwet <- glm(natwat_glm_poi ~ change_tab $artwet_poc, family='poisson')
summary(glm_poi_artwet)
# Call:
# glm(formula = natwat_glm_poi ~ change_tab$artwet_poc, family = "poisson")

# Deviance Residuals: 
   # Min      1Q  Median      3Q     Max  
# -5.463  -4.219  -2.628   0.833  14.499  

# Coefficients:
                    # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        2.703e+00  1.835e-02   147.2   <2e-16 ***
# change_tab$artwet_poc 1.084e-05  5.312e-07    20.4   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for poisson family taken to be 1)

    # Null deviance: 4402.0  on 199  degrees of freedom
# Residual deviance: 4159.1  on 198  degrees of freedom
  # (36 observations deleted due to missingness)
# AIC: 4892.2
# Number of Fisher Scoring iterations: 6

## Logarithm:

summary(change_tab $artwet_poc)

hist(log(change_tab $artwet_poc+30), breaks=100)

glm_poi_artwet_log <- glm(natwat_glm_poi ~ log(change_tab $artwet_poc+30), family='poisson')
summary(glm_poi_artwet_log)
# Call:
# glm(formula = natwat_glm_poi ~ log(change_tab$artwet_poc + 30), 
    # family = "poisson")
#
# Deviance Residuals: 
   # Min      1Q  Median      3Q     Max  
# -8.948  -3.826  -2.237   1.242  14.338  
#
# Coefficients:
                             # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                  1.355826   0.044881   30.21   <2e-16 ***
# log(change_tab$artwet_poc + 30) 0.297991   0.007896   37.74   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for poisson family taken to be 1)
#
    # Null deviance: 4402.0  on 199  degrees of freedom
# Residual deviance: 3345.8  on 198  degrees of freedom
  # (36 observations deleted due to missingness)
# AIC: 4078.8
#
# Number of Fisher Scoring iterations: 5

## => Logarithm: improve AIC


## 4. Sea PoC
#----

glm_poi_sea <- glm(natwat_glm_poi ~ change_tab $sea_poc, family='poisson')
summary(glm_poi_sea)
# Call:
# glm(formula = natwat_glm_poi ~ change_tab$sea_poc, family = "poisson")

# Deviance Residuals: 
    # Min       1Q   Median       3Q      Max  
# -5.5770  -4.3473  -2.7574   0.8518  14.2585  

# Coefficients:
                     # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         2.744e+00  1.827e-02 150.237   <2e-16 ***
# change_tab$sea_poc -1.064e-04  4.754e-05  -2.238   0.0252 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for poisson family taken to be 1)

    # Null deviance: 4402.0  on 199  degrees of freedom
# Residual deviance: 4396.3  on 198  degrees of freedom
  # (36 observations deleted due to missingness)
# AIC: 5129.4

# Number of Fisher Scoring iterations: 6


## Logarithm

summary(change_tab $sea_poc)

hist(change_tab $sea_poc, breaks=100)
hist(log(change_tab $sea_poc+100), breaks=100)

glm_poi_sea_log <- glm(natwat_glm_poi ~ log(change_tab $sea_poc+100), family='poisson')
summary(glm_poi_sea_log)
# Call:
# glm(formula = natwat_glm_poi ~ log(change_tab$sea_poc + 100), 
    # family = "poisson")
#
# Deviance Residuals: 
    # Min       1Q   Median       3Q      Max  
# -5.5635  -4.3322  -2.7247   0.8793  14.2873  
#
# Coefficients:
                              # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                    2.87187    0.10978  26.160   <2e-16 ***
# log(change_tab$sea_poc + 100) -0.02879    0.02304  -1.249    0.212    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for poisson family taken to be 1)
#
    # Null deviance: 4402.0  on 199  degrees of freedom
# Residual deviance: 4400.5  on 198  degrees of freedom
  # (36 observations deleted due to missingness)
# AIC: 5133.5
#
# Number of Fisher Scoring iterations: 6


## => Logarithm: worse for AIC



