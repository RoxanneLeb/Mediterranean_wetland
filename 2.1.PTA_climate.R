
## 06/2018 - Leberger et al. 2018. Mediterranean wetland conservation in the context of climate and land-cover change.


### 2.1. Climate PTA (Partial Triadic Analysis)
###############################################


## I. Data Preparation
#---

# Load climate data (representative from 1990 and from 2005)
table_all <- read.table('/Users/localadmin/Documents/PhD/Paper3_Wetlands/Data_to_publish/Table_all.txt', h=T)

# Table overview
dim(table_all)
head(table_all)
summary(table_all)

# Create table with climate variables from 1990
clim_1990 <- data.frame(table_all[,c(18, 21, 24, 27, 30, 33, 36, 39)], rep('1990', dim(table_all)[1])) 
colnames(clim_1990) <- c('tmean', 'tmax', 'tmin', 'tseas', 'pmean', 'pdry', 'pwet', 'pseas', 'year') 

# Create table with climate variables from 2005
clim_2005 <- data.frame(table_all[,c(19, 22, 25, 28, 31, 34, 37, 40)], rep('2005', dim(table_all)[1])) 
colnames(clim_2005) <- c('tmean', 'tmax', 'tmin', 'tseas', 'pmean', 'pdry', 'pwet', 'pseas', 'year') 

# Combine the two tables
clim_tab1 <- rbind(clim_1990, clim_2005)
head(clim_tab1, 20)


## II. PTA analysis
#---

# Load library
library(ade4)

# Run the PTA
wit1 <- withinpca(clim_tab1[,1:8], clim_tab1$year) # clim_tab1[,1:8]: variables used, clim_tab1$year: periods considered
3 # set dimentions/axis number to work on
kta1 <- ktab.within(wit1, colnames=rep(table_all$id, 2))
kta2 <- t(kta1)
pta_clim <- pta(kta2, scann = T)
3 # set dimentions/axis numer to work on

## 2.1 PTA overview

pta_clim

# Variance explained by 2 or 3 axis
pta_clim $eig
(pta_clim $eig[1] + pta_clim $eig[2])/sum(pta_clim $eig) # 2 axis: 0.78
(pta_clim $eig[1] + pta_clim $eig[2] + pta_clim $eig[3])/sum(pta_clim $eig) # 3 axis: 0.90

## 2.2. Scatter plot on the compromise (Figure S2)

# Scatter plot (Figure S2)
scatter(pta_clim, posieig='none', clab.row=0) #
s.class(pta_clim $li[,1:2], fac = as.factor(table_all $pa_cover_cat), col = c("#D73027", "#fee090", "#4575b4"), add.plot = TRUE, cstar = 0, cellipse = 0, clabel = 0)

## 2.3. Temporal analysis

# Plot change of each variable between 1990 and 2005
s.corcircle(pta_clim $Tco, fullcircle=T)


## III. Euclidean distance between variables
#---

# Load library
library(flexclust) # to compute euclidean distances

# Euclidean distances (variables)
tab_dist <- dist2(pta_clim$Tco[1:8,], pta_clim$Tco[9:16,])
c(tab_dist[1,1], tab_dist[2,2],tab_dist[3,3],tab_dist[4,4],tab_dist[5,5],tab_dist[6,6],tab_dist[7,7],tab_dist[8,8])*100 # *100 to have a better lecture for classification
## On 3 Axis :
#   tmean.2005 tmin.2005 tmax.2005 tseas.2005 pmean.2005 pwet.2005  pdry.2005 pseas.2005
#    4.066566  16.962716 11.644850 10.808258   8.775443   6.817503  2.816696  5.043891


## IV. Euclidean distance between sites
#---

# prepare tables
df_clim <- data.frame(pta_clim$Tli, pta_clim$TL, seq(1:dim(pta_clim$Tli)[1])) 

xy <- data.frame(matrix(0, ncol=8, nrow=2))
vect_tab <- rep(NA, dim(table_all)[1]) # length= 236

row_id <- as.numeric(levels(pta_clim $TL[1:dim(pta_clim $TL)[1]/2,2])) # length = 236

# run loop to produce eucliean distance from PTA output for each site
cpt <- 1
for(i in row_id){
	xy[c(1,2),] <- df_clim[,1:6][df_clim[,5] == i,] # length(df_clim) = 2*236 = 472
	vect_tab[cpt] <- dist2(xy[1,1:3], xy[2,1:3])
	cpt <- cpt+1
}

vect_tab
vtab_clim <- data.frame(table_all$id, vect_tab, table_all$pa_cover_cat)
colnames(vtab_clim) <- c('id', 'vect_clim', 'pa_level')
head(vtab_clim, 10)

# write.table(vtab_clim, '/Users/localadmin/Documents/PhD/Paper3_Wetlands/results_236_hydro/pta/pta_climlim.txt')

