
## 06/2018 - Leberger et al. 2018. Mediterranean wetland conservation in the context of lcate and land-cover change.


### 2.2. Land-cover PTA (Partial Triadic Analysis)
###############################################


## I. Data Preparation
#---

# Load land-cover data (1990 and 2005)
table_all <- read.table('Table_all.txt', h=T)

# Table overview
dim(table_all)
head(table_all)
summary(table_all)

## To build the COMPROMISE between 1990 and 2005 (taking into account natural wetland within the variables)
#---------------------------------------

# Create table with lcate variables from 1990 (take perc_90)
lc_1990 <- data.frame(table_all[,c(9, 44, 49, 54, 59)], rep('1990', dim(table_all)[1])) 
colnames(lc_1990) <- c('nat_wet', 'agri', 'art_wet', 'urb', 'sea', 'year')  

# Create table with lcate variables from 2005 (take or perc_05)
lc_2005 <- data.frame(table_all[,c(10, 45, 50, 55, 60)], rep('2005', dim(table_all)[1])) 
colnames(lc_2005) <- c('nat_wet', 'agri', 'art_wet', 'urb', 'sea', 'year')  

# Combine the two tables
lc_tab1 <- rbind(lc_1990, lc_2005)
head(lc_tab1, 20)


## II. PTA analysis
#---

# Load library
library(ade4)

# Run the PTA
wit1 <- withinpca(lc_tab1[,1:5], lc_tab1$year) # lc_tab1[,1:5]: variables used, lc_tab1$year: periods considered
3 # set dimentions/axis number to work on
kta1 <- ktab.within(wit1, colnames=rep(table_all$id, 2))
kta2 <- t(kta1)
pta_lc <- pta(kta2, scann = T)
3 # set dimentions/axis numer to work on

## 2.1 PTA overview

pta_lc

# Variance explained by 2 or 3 axis
pta_lc $eig
(pta_lc $eig[1] + pta_lc $eig[2])/sum(pta_lc $eig) # 2 axis: 0.56
(pta_lc $eig[1] + pta_lc $eig[2] + pta_lc $eig[3])/sum(pta_lc $eig) # 3 axis: 0.76

## 2.2. Scatter plot on the compromise (Figure S2)

# Scatter plot (Figure S2)
scatter(pta_lc, posieig='none', clab.row=0) #
s.class(pta_lc $li[,1:2], fac = as.factor(table_all $pa_cover_cat), col = c("#D73027", "#fee090", "#4575b4"), add.plot = TRUE, cstar = 0, cellipse = 0, clabel = 0)

#--------------------------------------- END To build the compromise


## TEMPORAL analysis (within natural wetland as variable)
#---------------------------------------

# Load data without natural wetlands

# Create table with lcate variables from 1990
lc_1990 <- data.frame(table_all[,c(44, 49, 54, 59)], rep('1990', dim(table_all)[1])) 
colnames(lc_1990) <- c('agri', 'art_wet', 'urb', 'sea', 'year')  

# Create table with lcate variables from 2005
lc_2005 <- data.frame(table_all[,c(45, 50, 55, 60)], rep('2005', dim(table_all)[1])) 
colnames(lc_2005) <- c('agri', 'art_wet', 'urb', 'sea', 'year')  

# Combine the two tables
lc_tab2 <- rbind(lc_1990, lc_2005)
head(lc_tab2, 20)

## 2.3. Temporal analysis

# Run the PTA
wit1 <- withinpca(lc_tab2[,1:4], lc_tab2$year) # lc_tab2[,1:5]: variables used, lc_tab2$year: periods considered
3 # set dimentions/axis number to work on
kta1 <- ktab.within(wit1, colnames=rep(table_all$id, 2))
kta2 <- t(kta1)
pta_lc <- pta(kta2, scann = T)
3 # set dimentions/axis numer to work on


# Plot change of each variable between 1990 and 2005
s.corcircle(pta_lc $Tco, fullcircle=T)


## III. Euclidean distance between variables
#---

# Load library
library(flexclust) # to compute euclidean distances

# Euclidean distances (variables)
tab_dist <- dist2(pta_lc$Tco[1:4,], pta_lc$Tco[5:8,])
c(tab_dist[1,1], tab_dist[2,2],tab_dist[3,3],tab_dist[4,4])*100 # *100 to have a better lecture for classification
## On 3 Axis :
#    agri.2005  art_wet.2005  urb.2005    sea.2005
#     15.3630681  3.5351191  8.8963043  0.6082286


## IV. Euclidean distance between sites
#---

# prepare tables
df_lc <- data.frame(pta_lc$Tli, pta_lc$TL, seq(1:dim(pta_lc$Tli)[1])) 

xy <- data.frame(matrix(0, ncol=8, nrow=2))
vect_tab <- rep(NA, dim(table_all)[1]) # length= 236

row_id <- as.numeric(levels(pta_lc $TL[1:dim(pta_lc $TL)[1]/2,2])) # length = 236

# run loop to produce eucliean distance from PTA output for each site
cpt <- 1
for(i in row_id){
	xy[c(1,2),] <- df_lc[,1:6][df_lc[,5] == i,] # length(df_lc) = 2*236 = 472
	vect_tab[cpt] <- dist2(xy[1,1:3], xy[2,1:3])
	cpt <- cpt+1
}

vect_tab
vtab_lc <- data.frame(table_all$id, vect_tab, table_all$pa_cover_cat)
colnames(vtab_lc) <- c('id', 'vect_lc', 'pa_level')
head(vtab_lc, 10)

# write.table(vtab_lc, 'PTA_lc.txt')

