
## 06/2018 - Leberger et al. 2018. Mediterranean wetland conservation in the context of climate and land-cover change.


### 2.1. Climate and Land-cover PTA (Partial Triadic Analysis)
###############################################


## I. Data Preparation
#---

# Load climate data (representative from 1990 and from 2005)
table_all <- read.table('../Data/Table_all.txt', h=T)

# Table overview
dim(table_all)
head(table_all)
summary(table_all)


## To build the COMPROMISE between 1990 and 2005 (taking into account natural wetland within the variables)
#---------------------------------------

# Create table with climate variables from 1990
clim_lc_1990 <- data.frame(table_all[,c(18, 21, 24, 27, 30, 33, 36, 39, 9, 44, 49, 54, 59)], rep('1990', dim(table_all)[1])) 
colnames(clim_lc_1990) <- c('tmean', 'tmax', 'tmin', 'tseas', 'pmean', 'pdry', 'pwet', 'pseas', 'nat_wet', 'agri', 'art_wet', 'urb', 'sea', 'year') 

# Create table with climate variables from 2005
clim_lc_2005 <- data.frame(table_all[,c(19, 22, 25, 28, 31, 34, 37, 40, 10, 45, 50, 55, 60)], rep('2005', dim(table_all)[1])) 
colnames(clim_lc_2005) <- c('tmean', 'tmax', 'tmin', 'tseas', 'pmean', 'pdry', 'pwet', 'pseas','nat_wet', 'agri', 'art_wet', 'urb', 'sea', 'year') 

# Combine the two tables
clim_lc_tab1 <- rbind(clim_lc_1990, clim_lc_2005)
head(clim_lc_tab1, 20)


## II. PTA analysis
#---

# Load library
library(ade4)

# Run the PTA
wit1 <- withinpca(clim_lc_tab1[,1:13], clim_lc_tab1$year) # clim_lc_tab1[,1:8]: variables used, clim_lc_tab1$year: periods considered
3 # set dimentions/axis number to work on
kta1 <- ktab.within(wit1, colnames=rep(table_all$id, 2))
kta2 <- t(kta1)
pta_clim_lc <- pta(kta2, scann = T)
3 # set dimentions/axis numer to work on

## 2.1 PTA overview

pta_clim

# Variance explained by 2 or 3 axis
pta_clim_lc $eig
(pta_clim_lc $eig[1] + pta_clim_lc $eig[2])/sum(pta_clim_lc $eig) # 2 axis: 0.52
(pta_clim_lc $eig[1] + pta_clim_lc $eig[2] + pta_clim_lc $eig[3])/sum(pta_clim_lc $eig) # 3 axis: 0.63

## 2.2. Scatter plot on the compromise (Figure S2)

# Scatter plot (Figure S2)
scatter(pta_clim, posieig='none', clab.row=0) #
s.class(pta_clim_lc $li[,1:2], fac = as.factor(table_all $pa_cover_cat), col = c("#D73027", "#fee090", "#4575b4"), add.plot = TRUE, cstar = 0, cellipse = 0, clabel = 0)


#--------------------------------------- END To build the compromise


## TEMPORAL analysis (within natural wetland as variable)
#---------------------------------------

# Load data without natural wetlands


# Create table with climate variables from 1990
clim_lc_1990 <- data.frame(table_all[,c(18, 21, 24, 27, 30, 33, 36, 39, 44, 49, 54, 59)], rep('1990', dim(table_all)[1])) 
colnames(clim_lc_1990) <- c('tmean', 'tmax', 'tmin', 'tseas', 'pmean', 'pdry', 'pwet', 'pseas', 'agri', 'art_wet', 'urb', 'sea', 'year') 

# Create table with climate variables from 2005
clim_lc_2005 <- data.frame(table_all[,c(19, 22, 25, 28, 31, 34, 37, 40, 45, 50, 55, 60)], rep('2005', dim(table_all)[1])) 
colnames(clim_lc_2005) <- c('tmean', 'tmax', 'tmin', 'tseas', 'pmean', 'pdry', 'pwet', 'pseas', 'agri', 'art_wet', 'urb', 'sea', 'year') 

# Combine the two tables
clim_lc_tab2 <- rbind(clim_lc_1990, clim_lc_2005)
head(clim_lc_tab2, 20)


## II. PTA analysis
#---


# Load library
library(ade4)

# Run the PTA
wit1 <- withinpca(clim_lc_tab2[,1:12], clim_lc_tab2$year) # lc_tab1[,1:5]: variables used, lc_tab1$year: periods considered
3 # set dimentions/axis number to work on
kta1 <- ktab.within(wit1, colnames=rep(table_all$id, 2))
kta2 <- t(kta1)
pta_clim_lc2  <- pta(kta2, scann = T)
3 # set dimentions/axis numer to work on


## 2.3. Temporal analysis

# Plot change of each variable between 1990 and 2005
s.corcircle(pta_clim_lc2 $Tco, fullcircle=T)


## III. Euclidean distance between variables
#---

# Load library
library(flexclust) # to compute euclidean distances

# Euclidean distances (variables)
tab_dist <- dist2(pta_clim_lc2 $Tco[1:12,], pta_clim_lc2 $Tco[13:24,])
c(tab_dist[1,1], tab_dist[2,2],tab_dist[3,3],tab_dist[4,4],tab_dist[5,5],tab_dist[6,6],tab_dist[7,7],tab_dist[8,8],tab_dist[9,9],tab_dist[10,10],tab_dist[11,11],tab_dist[12,12])*100 # *100 to have a better lecture for classification
## On 3 Axis :
#   tmean.2005  tmax.2005  tmin.2005  tseas.2005  pmean.2005  pdry.2005   pwet.2005  pseas.2005  agri.2005  art_wet.2005  urb.2005   sea.2005
#    3.652849   12.130466  11.884416    8.517592    6.323372   2.557820    6.928655.   5.777386  17.741948     9.460286   20.888940  2.646404


## IV. Euclidean distance between sites
#---

# prepare tables
df_clim_lc <- data.frame(pta_clim_lc2 $Tli, pta_clim_lc2 $TL, seq(1:dim(pta_clim_lc2 $Tli)[1])) 

xy <- data.frame(matrix(0, ncol=12, nrow=2))
vect_tab <- rep(NA, dim(table_all)[1]) # length= 236

row_id <- as.numeric(levels(pta_clim_lc2 $TL[1:dim(pta_clim_lc2 $TL)[1]/2,2])) # length = 236

# run loop to produce eucliean distance from PTA output for each site
cpt <- 1
for(i in row_id){
	xy[c(1,2),] <- df_clim_lc[,1:3][df_clim_lc[,5] == i,] # length(df_clim) = 2*236 = 472
	vect_tab[cpt] <- dist2(xy[1,1:3], xy[2,1:3])
	cpt <- cpt+1
}

vect_tab
vtab_clim <- data.frame(table_all$id, vect_tab, table_all$pa_cover_cat)
colnames(vtab_clim) <- c('id', 'vect_clim_lu', 'pa_level')
head(vtab_clim, 10)

# write.table(vtab_clim, 'pta_clim_lu.txt')

