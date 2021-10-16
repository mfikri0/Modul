library(tidyverse)
library(plm)
library(zoo)
library(RcppRoll)
library(Rcpp)



# Load the data
data <- read.csv("https://raw.githubusercontent.com/mfikri0/APJPH/main/Data_APJPH2.csv",fileEncoding = 'UTF-8-BOM')
data_u <- filter(data, X.REF. == "TRUE")

# Separate the data
data_nat <- filter(data, Region == "National")
data_JKT <- filter(data, Region == "Jakarta")
data_Jabar <- filter(data, Region == "West Java")
data_Jateng <- filter(data, Region == "Central Java")
data_DIY <- filter(data, Region == "Special Region of Yogyakarta")
data_Jatim <- filter(data, Region == "East Java")
data_Banten <- filter(data, Region == "Banten")
data_Bali <- filter(data, Region == "Bali")


#Delete Date and Region
data_nat <- subset(data_nat, select = -c(Date,Region))
data_JKT <- subset(data_JKT, select = -c(Date,Region))
data_Jabar <- subset(data_Jabar, select = -c(Date,Region))
data_Jateng <- subset(data_Jateng, select = -c(Date,Region))
data_DIY <- subset(data_DIY, select = -c(Date,Region))
data_Jatim <- subset(data_Jatim, select = -c(Date,Region))
data_Banten <- subset(data_Banten, select = -c(Date,Region))
data_Bali <- subset(data_Bali, select = -c(Date,Region))


# Correlation Matrices
library(Hmisc)

## National
nat.cor <- cor(data_nat, method = c("pearson"))
nat.rcorr <- rcorr(as.matrix(data_nat))
nat.rcorr.p <- nat.rcorr$P

## DKI Jakarta
JKT.cor <- cor(data_JKT, method = c("pearson"))
JKT.rcorr <- rcorr(as.matrix(data_JKT))
JKT.rcorr.p <- JKT.rcorr$P

## West Java
Jabar.cor <- cor(data_Jabar, method = c("pearson"))
Jabar.rcorr <- rcorr(as.matrix(data_Jabar))
Jabar.rcorr.p <- Jabar.rcorr$P

## Central Java
Jateng.cor <- cor(data_Jateng, method = c("pearson"))
Jateng.rcorr <- rcorr(as.matrix(data_Jateng))
Jateng.rcorr.p <- Jateng.rcorr$P

## Special Region of Yogyakarta
DIY.cor <- cor(data_DIY, method = c("pearson"))
DIY.rcorr <- rcorr(as.matrix(data_DIY))
DIY.rcorr.p <- DIY.rcorr$P

## East Java
Jatim.cor <- cor(data_Jatim, method = c("pearson"))
Jatim.rcorr <- rcorr(as.matrix(data_Jatim))
Jatim.rcorr.p <- Jatim.rcorr$P

## Banten
Banten.cor <- cor(data_Banten, method = c("pearson"))
Banten.rcorr <- rcorr(as.matrix(data_Banten))
Banten.rcorr.p <- Banten.rcorr$P

## Bali
Bali.cor <- cor(data_Bali, method = c("pearson"))
Bali.rcorr <- rcorr(as.matrix(data_Bali))
Bali.rcorr.p <- Bali.rcorr$P


# Extract the results
## National
write.csv(nat.rcorr$r, "D:\\Documents\\College\\Riset\\APJPH\\RCORR_natr.csv",row.names = TRUE)
write.csv(nat.rcorr$P, "D:\\Documents\\College\\Riset\\APJPH\\RCORR_natP.csv",row.names = TRUE)

## DKI Jakarta
write.csv(JKT.rcorr$r, "D:\\Documents\\College\\Riset\\APJPH\\RCORR_JKTr.csv",row.names = TRUE)
write.csv(JKT.rcorr$P, "D:\\Documents\\College\\Riset\\APJPH\\RCORR_JKTP.csv",row.names = TRUE)

## West Java
write.csv(Jabar.rcorr$r, "D:\\Documents\\College\\Riset\\APJPH\\RCORR_Jabarr.csv",row.names = TRUE)
write.csv(Jabar.rcorr$P, "D:\\Documents\\College\\Riset\\APJPH\\RCORR_JabarP.csv",row.names = TRUE)

## Central Java
write.csv(Jateng.rcorr$r, "D:\\Documents\\College\\Riset\\APJPH\\RCORR_Jatengr.csv",row.names = TRUE)
write.csv(Jateng.rcorr$P, "D:\\Documents\\College\\Riset\\APJPH\\RCORR_JatengP.csv",row.names = TRUE)

## Special Region of Yogyakarta
write.csv(DIY.rcorr$r, "D:\\Documents\\College\\Riset\\APJPH\\RCORR_DIYr.csv",row.names = TRUE)
write.csv(DIY.rcorr$P, "D:\\Documents\\College\\Riset\\APJPH\\RCORR_DIYP.csv",row.names = TRUE)

## East Java
write.csv(Jatim.rcorr$r, "D:\\Documents\\College\\Riset\\APJPH\\RCORR_Jatimr.csv",row.names = TRUE)
write.csv(Jatim.rcorr$P, "D:\\Documents\\College\\Riset\\APJPH\\RCORR_JatimP.csv",row.names = TRUE)

## Banten
write.csv(Banten.rcorr$r, "D:\\Documents\\College\\Riset\\APJPH\\RCORR_Bantenr.csv",row.names = TRUE)
write.csv(Banten.rcorr$P, "D:\\Documents\\College\\Riset\\APJPH\\RCORR_BantenP.csv",row.names = TRUE)

## Bali
write.csv(Bali.rcorr$r, "D:\\Documents\\College\\Riset\\APJPH\\RCORR_Balir.csv",row.names = TRUE)
write.csv(Bali.rcorr$P, "D:\\Documents\\College\\Riset\\APJPH\\RCORR_BaliP.csv",row.names = TRUE)


# Visualize
library(corrplot)
corrplot(nat.cor, type = 'lower', diag = FALSE, addCoef.col = 'black', tl.col = 'black')
corrplot(JKT.cor)
corrplot(Jabar.cor)
corrplot(Jateng.cor)
corrplot(DIY.cor)
corrplot(Jatim.cor)
corrplot(Banten.cor)
corrplot(Bali.cor)


## Using ggcorrplot
library(ggcorrplot)
### National
ggcorrplot(round(nat.cor , 2), hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method = "circle",
           colors = c("indianred1", "white", "seagreen3"),
           title = "Correlogram of National Data",
           ggtheme = theme_bw)

### JKT
ggcorrplot(round(JKT.cor , 2), hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method = "circle",
           colors = c("indianred1", "white", "seagreen3"),
           title = "Correlogram of Regional Data - Jakarta",
           ggtheme = theme_bw)

### Jabar
ggcorrplot(round(Jabar.cor , 2), hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method = "circle",
           colors = c("indianred1", "white", "seagreen3"),
           title = "Correlogram of Regional Data - West Java",
           ggtheme = theme_bw)

### Jateng
ggcorrplot(round(Jateng.cor , 2), hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method = "circle",
           colors = c("indianred1", "white", "seagreen3"),
           title = "Correlogram of Regional Data - Central Java",
           ggtheme = theme_bw)

### DIY
ggcorrplot(round(DIY.cor , 2), hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method = "circle",
           colors = c("indianred1", "white", "seagreen3"),
           title = "Correlogram of Regional Data - Yogyakarta",
           ggtheme = theme_bw)

### Jatim
ggcorrplot(round(Jatim.cor , 2), hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method = "circle",
           colors = c("indianred1", "white", "seagreen3"),
           title = "Correlogram of Regional Data - East Java",
           ggtheme = theme_bw)

### Banten
ggcorrplot(round(Banten.cor , 2), hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method = "circle",
           colors = c("indianred1", "white", "seagreen3"),
           title = "Correlogram of Regional Data - Banten",
           ggtheme = theme_bw)

### Bali
ggcorrplot(round(Bali.cor , 2), hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method = "circle",
           colors = c("indianred1", "white", "seagreen3"),
           title = "Correlogram of Regional Data - Bali",
           ggtheme = theme_bw)
