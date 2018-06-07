
## 06/2018 - Leberger et al. 2018. Mediterranean wetland conservation in the context of climate and land-cover change.

### 2.1. Kruskal-Wallis test: variable ~ Protection cover
###############################################

## !! for Kruskal-Wallis test => distribution (variance) should be similar between classes => remove outliers ! (present in percentage of change)
# use of post-hoc Conover test https://stats.stackexchange.com/questions/141856/what-is-the-difference-between-various-kruskal-wallis-post-hoc-tests
# more powerfull than Dunn test and difference of distribution between classes does not matter

## Load data
tab1 <- read.table('../Data/Table_all.txt', h=T)
head(tab1)
summary(tab1)

## Load library
library(conover.test)


## I. Climate variables
##-----------

## 1. Tmean
#---

# Distribution

hist(tab1 $tmean_diff) 

## Boxplot

boxplot(tab1 $tmean_diff ~ as.factor(tab1 $pa_cover_cat), xlab='pa_cover_cat', ylab='PTA_clim')

## K-W test

krusk_tmean <- kruskal.test(tmean_diff ~ as.factor(pa_cover_cat), data= tab1)
krusk_tmean
	# Kruskal-Wallis rank sum test
# data:  tmean_diff by as.factor(pa_cover_cat)
# Kruskal-Wallis chi-squared = 51.18, df = 2, p-value = 7.697e-12

## Conover post-hoc test

conover.test(tab1$tmean_diff, as.factor(tab1 $pa_cover_cat), altp=T)

  # Kruskal-Wallis rank sum test
#
# data: x and group
# Kruskal-Wallis chi-squared = 51.1803, df = 2, p-value = 0
#
#
                           # Comparison of x by group                            
                                # (No adjustment)                                
# Col Mean-|
# Row Mean |          0        100
# ---------+----------------------
     # 100 |   6.219977
         # |    0.0000*
         # |
      # 50 |  -1.781999  -6.805175
         # |     0.0761    0.0000*
#
# alpha = 0.05
# Reject Ho if p <= alpha

median(tab1 $tmean_diff[tab1$pa_cover_cat == 0]) # 0.69998
median(tab1 $tmean_diff[tab1$pa_cover_cat == 50]) # 0.9600244
median(tab1 $tmean_diff[tab1$pa_cover_cat == 100]) # 0.4160526


## 2. Tmax
#---

## Distribution

hist(tab1 $tmax_diff) 

## Boxplot

boxplot(tab1 $tmax_diff ~ as.factor(tab1 $pa_cover_cat), xlab='Protection coverage', ylab='Tmax change') 
# Increase in Maximum temperature is higher in protected sites

## K-W test

krusk_tmax <- kruskal.test(tmax_diff ~ as.factor(pa_cover_cat), data= tab1)
krusk_tmax

	# Kruskal-Wallis rank sum test
# data:  tmax_diff by as.factor(pa_cover_cat)
# Kruskal-Wallis chi-squared = 15.957, df = 2, p-value = 0.0003428

## Conover post-hoc test

conover.test(tab1$tmax_diff, as.factor(tab1 $pa_cover_cat), altp=T)

  # Kruskal-Wallis rank sum test
#
# data: x and group
# Kruskal-Wallis chi-squared = 15.9569, df = 2, p-value = 0
#
                           # Comparison of x by group                            
                                # (No adjustment)                                
# Col Mean-|
# Row Mean |          0        100
# ---------+----------------------
     # 100 |  -4.085218
         # |    0.0001*
         # |
      # 50 |  -2.309916   0.770358
         # |    0.0218*     0.4419
# alpha = 0.05
# Reject Ho if p <= alpha


## 3. Tmin
#---

## Boxplot
hist(tab1 $tmin_diff) 

boxplot(tab1 $tmin_diff ~ as.factor(tab1 $pa_cover_cat), xlab='Protection coverage', ylab='Tmin change') 
# Increase in Minimum temperature is the highest in intermediate protected coverage and the lowest in high protection coverage

## K-W test
krusk_tmin <- kruskal.test(tmin_diff ~ as.factor(pa_cover_cat), data= tab1)
krusk_tmin
	# Kruskal-Wallis rank sum test

# data:  tmin_diff by as.factor(pa_cover_cat)
# Kruskal-Wallis chi-squared = 17.131, df = 2, p-value = 0.0001906

## Conover post-hoc test

conover.test(tab1$tmin_diff, as.factor(tab1 $pa_cover_cat), altp=T)

  # Kruskal-Wallis rank sum test
#
# data: x and group
# Kruskal-Wallis chi-squared = 17.1306, df = 2, p-value = 0
#
#
                           # Comparison of x by group                            
                                # (No adjustment)                                
# Col Mean-|
# Row Mean |          0        100
# ---------+----------------------
     # 100 |   2.474566
         # |    0.0141*
         # |
      # 50 |  -2.010655  -4.090955
         # |    0.0455*    0.0001*

# alpha = 0.05
# Reject Ho if p <= alpha



## 4. Tseas
#---

## Boxplot
hist(tab1 $tseas_diff) 

boxplot(tab1 $tseas_diff ~ as.factor(tab1 $pa_cover_cat), xlab='Protection coverage', ylab='Tseas change') 
# Increase in temperature seasonnality is the highest high protection coverage

## K-W test

krusk_tseas <- kruskal.test(tseas_diff ~ as.factor(pa_cover_cat), data= tab1)
krusk_tseas
	# Kruskal-Wallis rank sum test

# data:  tseas_diff by as.factor(pa_cover_cat)
# Kruskal-Wallis chi-squared = 46.841, df = 2, p-value = 6.74e-11

## Conover post-hoc test

conover.test(tab1$tseas_diff, as.factor(tab1 $pa_cover_cat), altp=T)
  # Kruskal-Wallis rank sum test

# data: x and group
# Kruskal-Wallis chi-squared = 46.8409, df = 2, p-value = 0


                           # Comparison of x by group                            
                                # (No adjustment)                                
# Col Mean-|
# Row Mean |          0        100
# ---------+----------------------
     # 100 |  -7.024064
         # |    0.0000*
         # |
      # 50 |  -0.521046   4.992158
         # |     0.6028    0.0000*

# alpha = 0.05
# Reject Ho if p <= alpha


## 5. Pmean
#---

## Boxplot
hist(tab1 $pmean_diff) 

boxplot(tab1 $pmean_diff ~ as.factor(tab1 $pa_cover_cat), xlab='Protection coverage', ylab='Pmean change') 
# Increase in precipitation mean is the highest high protection coverage, Decrease in precipitation mean is highest in intermediate protection coverage.

## K-W test
krusk_pmean <- kruskal.test(pmean_diff ~ as.factor(pa_cover_cat), data= tab1)
krusk_pmean
	# Kruskal-Wallis rank sum test

# data:  pmean_diff by as.factor(pa_cover_cat)
# Kruskal-Wallis chi-squared = 42.385, df = 2, p-value = 6.255e-10

## Conover post-hoc test

conover.test(tab1$pseas_diff, as.factor(tab1 $pa_cover_cat), altp=T)
  # Kruskal-Wallis rank sum test

# data: x and group
# Kruskal-Wallis chi-squared = 7.8231, df = 2, p-value = 0.02


                           # Comparison of x by group                            
                                # (No adjustment)                                
# Col Mean-|
# Row Mean |          0        100
# ---------+----------------------
     # 100 |   2.703234
         # |    0.0074*
         # |
      # 50 |   0.456740  -1.648920
         # |     0.6483     0.1005

# alpha = 0.05
# Reject Ho if p <= alpha



## 6. Pdry
#---

## Boxplot
hist(tab1 $pdry_diff) 

boxplot(tab1 $pdry_diff ~ as.factor(tab1 $pa_cover_cat), xlab='Protection coverage', ylab='Pdry change') 
# Increase in precipitation of the driest month is the lowest high protection coverage, highest in low protection coverage.

## K-W test
krusk_pdry <- kruskal.test(pdry_diff ~ as.factor(pa_cover_cat), data= tab1)
krusk_pdry
	# Kruskal-Wallis rank sum test

# data:  pdry_diff by as.factor(pa_cover_cat)
# Kruskal-Wallis chi-squared = 11.237, df = 2, p-value = 0.003631

## Conover post-hoc test

conover.test(tab1$pdry_diff, as.factor(tab1 $pa_cover_cat), altp=T)
  # Kruskal-Wallis rank sum test

# data: x and group
# Kruskal-Wallis chi-squared = 11.2367, df = 2, p-value = 0


                           # Comparison of x by group                            
                                # (No adjustment)                                
# Col Mean-|
# Row Mean |          0        100
# ---------+----------------------
     # 100 |   2.947648
         # |    0.0035*
         # |
      # 50 |   2.873631   0.727001
         # |    0.0044*     0.4680

# alpha = 0.05
# Reject Ho if p <= alpha


## 7. Pwet
#---

## Boxplot
hist(tab1 $pwet_diff) 

boxplot(tab1 $pwet_diff ~ as.factor(tab1 $pa_cover_cat), xlab='Protection coverage', ylab='Pwet change') 
# Increase in precipitation of the wettest month is the lowest intermediate protection coverage, highest in high protection coverage.

## K-W test

krusk_pwet <- kruskal.test(pwet_diff ~ as.factor(pa_cover_cat), data= tab1)
krusk_pwet
	# Kruskal-Wallis rank sum test

# data:  pwet_diff by as.factor(pa_cover_cat)
# Kruskal-Wallis chi-squared = 8.4793, df = 2, p-value = 0.01441

## Conover post-hoc test

conover.test(tab1$pwet_diff, as.factor(tab1 $pa_cover_cat), altp=T)
  # Kruskal-Wallis rank sum test

# data: x and group
# Kruskal-Wallis chi-squared = 8.4793, df = 2, p-value = 0.01


                           # Comparison of x by group                            
                                # (No adjustment)                                
# Col Mean-|
# Row Mean |          0        100
# ---------+----------------------
     # 100 |  -2.133063
         # |    0.0340*
         # |
      # 50 |   0.868188   2.606992
         # |     0.3862    0.0097*

# alpha = 0.05
# Reject Ho if p <= alpha

# Median for each PA level
median(tab1$pwet_diff[tab1$pa_cover_cat == 0]) # [1] -3.385285
median(tab1$pwet_diff[tab1$pa_cover_cat == 50]) # -6.675771
median(tab1$pwet_diff[tab1$pa_cover_cat == 100]) # 0.9600277



## 8. Pseas
#---

## Boxplot
hist(tab1 $pseas_diff) 

boxplot(tab1 $pseas_diff ~ as.factor(tab1 $pa_cover_cat), xlab='Protection coverage', ylab='Pseas change') 
# Decrease in precipitation of the wettest month is the highest protection coverage.

## K-W test

krusk_pseas <- kruskal.test(pseas_diff ~ as.factor(pa_cover_cat), data= tab1)
krusk_pseas
	# Kruskal-Wallis rank sum test
#
# data:  pseas_diff by as.factor(pa_cover_cat)
# Kruskal-Wallis chi-squared = 7.8231, df = 2, p-value = 0.02001

## Conover post-hoc test

conover.test(tab1$pseas_diff, as.factor(tab1 $pa_cover_cat), altp=T)
  # Kruskal-Wallis rank sum test
#
# data: x and group
# Kruskal-Wallis chi-squared = 7.8231, df = 2, p-value = 0.02
#
#
                           # Comparison of x by group                            
                                # (No adjustment)                                
# Col Mean-|
# Row Mean |          0        100
# ---------+----------------------
     # 100 |   2.703234
         # |    0.0074*
         # |
      # 50 |   0.456740  -1.648920
         # |     0.6483     0.1005
#
# alpha = 0.05
# Reject Ho if p <= alpha


# Median for each PA level
median(tab1$pseas_diff[tab1$pa_cover_cat == 0]) # 0.6510794
median(tab1$pseas_diff[tab1$pa_cover_cat == 50]) # 0.9489354
median(tab1$pseas_diff[tab1$pa_cover_cat == 100]) # -1.892517


## II. Land-cover variables
##-----------

## 1. Urb
#---

## Boxplot
hist(tab1 $urb_poc, breaks=100) 
hist(tab1 $urb_poc[tab1 $urb_poc < 5000], breaks=100) 
length(tab1 $urb_poc[tab1 $urb_poc < 5000]) # 230

boxplot(tab1 $urb_poc ~ as.factor(tab1 $pa_cover_cat), xlab='Protection coverage', ylab='urb change', ylim=c(0, 3000)) 
# Increase in urnabisation is the highest in the intermediate protection coverage.

## K-W test
krusk_pseas <- kruskal.test(urb_poc ~ as.factor(pa_cover_cat), data= tab1)
krusk_pseas
	# Kruskal-Wallis rank sum test

# data:  urb_poc by as.factor(pa_cover_cat)
# Kruskal-Wallis chi-squared = 8.2609, df = 2, p-value = 0.01608

## Conover post-hoc test

conover.test(tab1$urb_poc[tab1 $urb_poc <5000], g=tab1$pa_cover_cat[tab1 $urb_poc <5000], altp=T)
  # Kruskal-Wallis rank sum test

# data: x and group
# Kruskal-Wallis chi-squared = 8.9077, df = 2, p-value = 0.01
                           # Comparison of x by group                            
                                # (No adjustment)                                
# Col Mean-|
# Row Mean |          0        100
# ---------+----------------------
     # 100 |  -0.370020
         # |     0.7117
         # |
      # 50 |  -2.854495  -2.742048
         # |    0.0047*    0.0066*

# alpha = 0.05
# Reject Ho if p <= alpha

## log(urb)

summary(tab1 $urb_poc[tab1 $urb_poc <5000])
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0.00   12.57  153.45  559.66  610.00 4961.11 

# Median for each PA level
median(tab1$urb_poc[tab1$pa_cover_cat == 0 & tab1 $urb_poc < 5000]) #
median(tab1$urb_poc[tab1$pa_cover_cat == 50 & tab1 $urb_poc < 5000]) #
median(tab1$urb_poc[tab1$pa_cover_cat == 100 & tab1 $urb_poc < 5000]) #


## 2. Agri
#---

## Distribution

hist(tab1 $agri_poc, breaks=100) 
hist(tab1 $agri_poc[tab1 $agri_poc < 1000], breaks=100) 
length(tab1 $agri_poc[tab1 $agri_poc < 1000]) # 223

## Boxplot

boxplot(tab1 $agri_poc ~ as.factor(tab1 $pa_cover_cat), xlab='Protection coverage', ylab='agricultural change', ylim=c(0, 2000)) 
# Increase in urnabisation is the similar in all protection coverage. # importance of taking into account site management !

## K-W test

krusk_agri <- kruskal.test(agri_poc ~ as.factor(pa_cover_cat), data= tab1)
krusk_agri
	# Kruskal-Wallis rank sum test
#
# data:  agri_poc by as.factor(pa_cover_cat)
# Kruskal-Wallis chi-squared = 0.89274, df = 2, p-value = 0.6399

## Conover post-hoc test

conover.test(tab1$agri_poc[tab1 $agri_poc <1000], g=tab1$pa_cover_cat[tab1 $agri_poc <1000], altp=T)
  # Kruskal-Wallis rank sum test

# data: x and group
# Kruskal-Wallis chi-squared = 1.5647, df = 2, p-value = 0.46


                           # Comparison of x by group                            
                                # (No adjustment)                                
# Col Mean-|
# Row Mean |          0        100
# ---------+----------------------
     # 100 |  -0.346315
         # |     0.7294
         # |
      # 50 |  -1.223261  -1.036723
         # |     0.2225     0.3010

# alpha = 0.05
# Reject Ho if p <= alpha

         
# Median for each PA level
median(tab1$agri_poc[tab1$pa_cover_cat == 0 & tab1 $agri_poc <1000]]) #
median(tab1$agri_poc[tab1$pa_cover_cat == 50 & tab1 $agri_poc <1000]]) #
median(tab1$agri_poc[tab1$pa_cover_cat == 100 & tab1 $agri_poc <1000]]) #


## 3. Art_wet
#---

## Distribution

hist(tab1 $artwet_poc, breaks=100) 
hist(tab1 $artwet_poc[tab1 $artwet_poc<1000], breaks=100) 
length(tab1 $artwet_poc[tab1 $artwet_poc<1000])

## Boxplot

boxplot(tab1 $artwet_poc ~ as.factor(tab1 $pa_cover_cat), xlab='Protection coverage', ylab='Art_wet change', ylim=c(0, 2000)) 
# Increase in artificial wetland is the highest in intermediate protection coverage.

## K-W test

krusk_agri <- kruskal.test(artwet_poc ~ as.factor(pa_cover_cat), data= tab1)
krusk_agri
	# Kruskal-Wallis rank sum test
#
# data:  artwet_poc by as.factor(pa_cover_cat)
# Kruskal-Wallis chi-squared = 5.982, df = 2, p-value = 0.05024

## Conover post-hoc test

conover.test(tab1$artwet_poc[tab1 $artwet_poc<1000], g=tab1$pa_cover_cat[tab1 $artwet_poc<1000], altp=T)
  # Kruskal-Wallis rank sum test
#
# data: x and group
# Kruskal-Wallis chi-squared = 9.4706, df = 2, p-value = 0.01
#
#
                           # Comparison of x by group                            
                                # (No adjustment)                                
# Col Mean-|
# Row Mean |          0        100
# ---------+----------------------
     # 100 |  -1.715400
         # |     0.0877
         # |
      # 50 |  -3.114930  -1.976112
         # |    0.0021*    0.0494*
#
# alpha = 0.05
# Reject Ho if p <= alpha


## 4. Sea
#---

## Boxplot
hist(tab1 $sea_poc, breaks=100) 
length(tab1$sea_poc[tab1$sea_poc < 100]) # 236-221 => 15 removed

## Boxplot
boxplot(tab1$sea_poc[tab1$sea_poc < 100] ~ as.factor(tab1 $pa_cover_cat[tab1$sea_poc < 100]))
boxplot(tab1 $sea_poc ~ as.factor(tab1 $pa_cover_cat), xlab='Protection coverage', ylab='Art_wet change', ylim=c(0, 100)) 
# Increase in artificial wetland is the highest in intermediate protection coverage.

## K-W test

krusk_sea <- kruskal.test(sea_poc[tab1$sea_poc < 100] ~ as.factor(pa_cover_cat[tab1$sea_poc < 100]), data= tab1)
krusk_sea
	# Kruskal-Wallis rank sum test
#
#data:  sea_poc[tab1$sea_poc < 100] by as.factor(pa_cover_cat[tab1$sea_poc < 100])
#Kruskal-Wallis chi-squared = 2.9903, df = 2, p-value = 0.2242

## Conover post-hoc test

conover.test(tab1$sea_poc[tab1$sea_poc < 100], g=tab1$pa_cover_cat[tab1$sea_poc < 100], altp=T)
  # Kruskal-Wallis rank sum test
#
# data: x and group
# Kruskal-Wallis chi-squared = 2.9903, df = 2, p-value = 0.22
#
#
                           # Comparison of x by group                            
                                # (No adjustment)                                
# Col Mean-|
# Row Mean |          0        100
# ---------+----------------------
     # 100 |  -0.878769
         # |     0.3805
         # |
      # 50 |   0.952467   1.700156
         # |     0.3419     0.0905
#
# alpha = 0.05
# Reject Ho if p <= alpha


# Median for each PA level
median(tab1$sea_poc[tab1$pa_cover_cat == 0]) # 0
median(tab1$sea_poc[tab1$pa_cover_cat == 50]) # 0
median(tab1$sea_poc[tab1$pa_cover_cat == 100]) # 0


## 5. Natural Wetland
#---

conover.test(tab1$natwet_poc[tab1$natwet_poc < 0], g= tab1 $pa_cover_cat[tab1$natwet_poc < 0], altp=T)
  # Kruskal-Wallis rank sum test
#
# data: x and group
# Kruskal-Wallis chi-squared = 6.6733, df = 2, p-value = 0.04
#
#
                           # Comparison of x by group                            
                                # (No adjustment)                                
# Col Mean-|
# Row Mean |          0        100
# ---------+----------------------
     # 100 |  -1.974843
         # |    0.0497*
         # |
      # 50 |   0.589502   2.211123
         # |     0.5562    0.0282*
#
# alpha = 0.05
# Reject Ho if p <= alpha

median(tab1$natwet_poc[tab1 $pa_cover_cat == 0]) #  -0.0538804
median(tab1$natwet_poc[tab1 $pa_cover_cat == 50]) #  -0.07142857
median(tab1$natwet_poc[tab1 $pa_cover_cat == 100]) # -0.04091653


## III. PTA Climate and PTA Land-cover
##-----------

## 1. PTA climate
#---

## Boxplot
boxplot(tab1$pta_clim ~ tab1$pa_cover_cat)

## Conover post-hoc test

conover.test(tab1$pta_clim, g=tab1$pa_cover_cat, altp=T)
  # Kruskal-Wallis rank sum test
#
# data: x and group
# Kruskal-Wallis chi-squared = 2.0892, df = 2, p-value = 0.35
#
#
                           # Comparison of x by group                            
                                # (No adjustment)                                
# Col Mean-|
# Row Mean |          0        100
# ---------+----------------------
     # 100 |   0.505526
         # |     0.6137
         # |
      # 50 |  -0.983571  -1.444581
         # |     0.3263     0.1499
#
# alpha = 0.05
# Reject Ho if p <= alpha


## 2. PTA land-cover
#---

## Boxplot
boxplot(tab1$pta_lu ~ tab1$pa_cover_cat)

## Conover post-hoc test

conover.test(tab1$pta_lu, g=tab1$pa_cover_cat, altp=T)
  # Kruskal-Wallis rank sum test
#
# data: x and group
# Kruskal-Wallis chi-squared = 1.1867, df = 2, p-value = 0.55
#
#
                           # Comparison of x by group                            
                                # (No adjustment)                                
# Col Mean-|
# Row Mean |          0        100
# ---------+----------------------
     # 100 |   1.086172
         # |     0.2785
         # |
      # 50 |   0.534342  -0.289656
         # |     0.5936     0.7723

# alpha = 0.05
# Reject Ho if p <= alpha


## IV. Species
##-----------

## 1. Beta-Sorrensen
#---

## Boxplot
boxplot(tab1$beta_sor_1991_2010 ~ tab1$pa_cover_cat)

table(tab1$pa_cover_cat[!is.na(tab1$beta_sor_1991_2010)])
#  0  50 100 
#  9  19  83 

## Conover post-hoc test

conover.test(tab1$beta_sor_1991_2010, g=tab1$pa_cover_cat, altp=T)

  # Kruskal-Wallis rank sum test
# data: x and group
# Kruskal-Wallis chi-squared = 10.8771, df = 2, p-value = 0
                           # Comparison of x by group                            
                                # (No adjustment)                                
# Col Mean-|
# Row Mean |          0        100
# ---------+----------------------
     # 100 |  -3.127623
         # |    0.0023*
         # |
      # 50 |  -3.342919  -1.003084
         # |    0.0011*     0.3181
# alpha = 0.05
# Reject Ho if p <= alpha

median(tab1$beta_sor_1991_2010[tab1 $pa_cover_cat == 0], na.rm=T) #  0.1948052
median(tab1$beta_sor_1991_2010[tab1 $pa_cover_cat == 50], na.rm=T) #  0.5384615
median(tab1$beta_sor_1991_2010[tab1 $pa_cover_cat == 100], na.rm=T) # 0.4683544





