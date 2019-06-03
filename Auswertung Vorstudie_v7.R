#Auswertung Vorstudie


#1. Allgemein
#Libaries laden
library(mosaic) # Zugpferd
library(effects) # Effektplots für ANOVA-Modelle
library(openxlsx) # Excel-Dateien schreiben
library(corrgram) # Korrelationsdiagramme
library(GGally) # Korrelationsdiagramme
library(vcd) # Effektstärkemaße für Nominaldaten
library(readxl) #Exelfiles einlesen
library(ggplot2) #Multiplotter
library(reshape2)#Histogrammmultiplot
library(boot) #Bootstrapping
library(distrEx) # Hellingercalcualtion
library(statip) #Hellingercalcuation2
library(SamplingBigData) #maxbaum

#Daten einlesen

#Verzeichnis angeben und Daten speichern in d.set
setwd("E:/Studium/Psychologie/Bachelorarbeit")
d.set <- read_excel("DatenVorstudie_final.xlsx")

#Bereinigung des Datensatzes
#Cut 1 Zeile
#Cut leere Spalten
d.clean <- d.set [-1, c(-1,-2,-3,-4,-5,-6)]

#Create Datasets for Topics
d.nanoh <- d.clean [, c(13:26)]
d.vegh <- d.clean [, c(1:12)]

#Daten auf Skala von -8 bis 8 bringen
data.n <- data.matrix(d.nanoh, rownames.force = NA)
d.nano = data.n - 8

data.mv <- data.matrix(d.vegh, rownames.force = NA)
d.veg = data.mv - 8

#define length of both parts
N.n = 14
N.v = 12

#and leght for corresponding Matrices
M.n = N.n * N.n
M.v = N.v * N.v

#2. Deskreptive Statistik

#2.1 Einzelhistogramme

#Histogramme werden in Plotordner gespeichert
setwd("E:/Studium/Psychologie/Bachelorarbeit/Auswertung Plots")

#2.1.1 Nanotechnologie

#Determine Bin-width for each column (employing Scott's normal reference rule)

n.n = array(1:N.n, dim=1)
sig.n = array(1:N.n, dim=1)
BW.n = array(1:N.n, dim=1)
BWest.n = array(1:N.n, dim=1)

for (i1.n in 1:N.n){
  n.n [i1.n] = length(d.nano [,i1.n])
  sig.n [i1.n]  = sd(d.nano [,i1.n])
  
  BW.n [i1.n] = 3.5 * sig.n [i1.n]/(n.n [i1.n]^(1/3))
  BWest.n [i1.n] = round(BW.n [i1.n], digits = 0)
}


#Für Nanotechnologie alle werden automatisch erstellt undgesaved
for (i2.n in 1:N.n){
  pdf(sprintf("hist_nano%s.pdf", i2.n))
  hist(d.nano [,i2.n],
       main=sprintf("Histogram of Question %s on Nanotechnology", i2.n),
       xlab= "Agreement to Question",
       border = "blue",
       col="green",
       xlim=c(-10,10),
       las=1, 
       breaks= seq(-10,10,by=BWest.n [i2.n]))
  dev.off()
}

#2.1.2 Vegetarismus

#Determine Bin-width for each column (employing Scott's normal reference rule)

n.v = array(1:N.v, dim=1)
sig.v = array(1:N.v, dim=1)
BW.v = array(1:N.v, dim=1)
BWest.v = array(1:N.v, dim=1)

for (i1.v in 1:N.v){
  n.v [i1.v] = length(d.veg [,i1.v])
  sig.v [i1.v]  = sd(d.veg [,i1.v])
  
  BW.v [i1.v] = 3.5 * sig.v [i1.v]/(n.v [i1.v]^(1/3))
  BWest.v [i1.v] = round(BW.v [i1.v], digits = 0)
}

#Für Vegetrismus alle werden automatisch erstellt undgesaved
for (i2.v in 1:N.v){
  pdf(sprintf("hist_veg%s.pdf", i2.v))
  hist(d.veg [,i2.v],
       main=sprintf("Histogram of Question %s on Vegetarism", i2.v),
       xlab= "Agreement to Question",
       border = "blue",
       col="green",
       xlim=c(-10,10),
       las=1, 
       breaks= seq(-8,8,by=BWest.v [i2.v]))
  dev.off()
}

setwd("E:/Studium/Psychologie/Bachelorarbeit")

#2.2 Analysis of Opinions of Texts

#2.2.1 Nanotechnology

#2.2.1.1 Shapiro-Wilk to find out if the data is normally distributed

#perform shapiro for all datasets
psharp.n = array(1:N.n, dim=1)
psharpG.n = array(1:N.n, dim=1)

for(i8.n in 1:N.n){
 psharp.n [i8.n] = shapiro.test(d.nano [,i8.n])$p.value
}

#make date interpretable by comparing to significance p<0.1 (Royston (1995)) code significant difference from Gauss as 0 and Guass as 1
for(i9.n in 1:N.n){
  if (psharp.n [i9.n] < 0.1){
    psharpG.n [i9.n] = 0
  }
  else{
    psharpG.n [i9.n] = 1
  }
}

#show the interpretable significance results psharpG.n
show(psharpG.n)


#2.2.1.2 Mean
#calculate the mean values for each distribution

mean.n = array(1:N.n, dim=1)

for (i3.n in 1:N.n){
  mean.n [i3.n] = mean(d.nano [,i3.n])
}

#2.2.1.3 Mann-Whitney U test
#Decide if the differece between tests is signifikant (for non-parametric reasonable distributions)

#Matrix of paiwise p values

p.n = matrix(1:M.n, byrow =TRUE,nrow=N.n)

for (i5.n in 1:N.n) {
  for (i4.n in 1:N.n){
    p.n [i5.n,i4.n] = wilcox.test(d.nano [,i5.n],d.nano [,i4.n],paired=TRUE)$p.value
  }
}


#transform to see which values are above 0.05 (no significant differences between the two distributions)

ps.n = matrix(1:M.n, byrow =TRUE,nrow=N.n)

#get rid of all NaN values
for (i5.n in 1:N.n) {
  for (i4.n in 1:N.n){
    if (is.nan(p.n [i5.n,i4.n])){
      p.n [i5.n,i4.n] = 1
    }
  }
}

#make date interpretable by comparing to significance p<0.05 code significant difference  as 0 and Guass as 1
for (i6.n in 1:N.n) {
  for (i7.n in 1:N.n){
    if(p.n [i6.n,i7.n] < 0.05){
      ps.n [i6.n,i7.n] = 0
    }
    else{
      ps.n [i6.n,i7.n] = 1
    }
  }
}

#show the interpretable significance results ps
show(ps.n)

#2.2.1.3 Bootstrap test

#write a wrapper for mean (that accepts two arguments, one the data vector and one the permuted indices, then use these to form a call to mean 
#---  here mean.fun does this (note we turn on removing NA's by default otherwise it wouldn't work))
mean.fun <- function(dat, idx) mean(dat[idx], na.rm = TRUE)

#use boot and save the data for each set providing mean and sd

bootmn.n = array(1:N.n, dim = 1)
bootsd.n = array(1:N.n, dim = 1)

for(i10.n in 1:N.n){
  boot.out <- boot(data = d.nano [,i10.n], statistic = mean.fun, R = 1000, sim = "ordinary")
  bootmn.n [i10.n] = apply(boot.out$t, 2, mean)
  bootsd.n [i10.n] = apply(boot.out$t, 2, sd)
}

#Ergebnisse in einer Matrix zusammen stellen
#rows for table
T.n = 2* N.n

#Table
boot.n = matrix(1:T.n, byrow=TRUE, nrow = N.n)

#Füllen Mittelwerte
for (i11.n in 1:N.n){
  boot.n [i11.n, 1] = bootmn.n [i11.n]
}

#Füllen std
for (i12.n in 1:N.n){
  boot.n [i12.n, 2] = bootsd.n [i12.n]
}

#Show results with means and corresponding std deviations in a table
show(boot.n)

#now test if in 1sigma der is an overlap between means
boots.n = matrix(1:M.n, byrow =TRUE,nrow=N.n)

#make date interpretable by comparing to significance of 1 sigma overlap code significant difference  as 0 and not as 1
for (i12.n in 1:N.n) {
  for (i13.n in 1:N.n){
    if( xor(boot.n [i12.n,1] + boot.n [i12.n,2] < boot.n [i13.n,1] - boot.n [i13.n,2], boot.n [i12.n,1] - boot.n [i12.n,2] > boot.n [i13.n,1] + boot.n [i13.n,2]) ){
      boots.n [i12.n,i13.n] = 0
    }
    else{
      boots.n [i12.n,i13.n] = 1
    }
  }
}

#show the interpretable significance results boots
show(boots.n)




#2.2.1 Vegetarism

#2.2.1.1 Shapiro-Wilk to find out if the data is normally distributed

#perform shapiro for all datasets
psharp.v = array(1:N.v, dim=1)
psharpG.v = array(1:N.v, dim=1)

for(i8.v in 1:N.v){
  psharp.v [i8.v] = shapiro.test(d.veg [,i8.v])$p.value
}

#make date interpretable by comparing to significance p<0.1 (Royston (1995)) code significant difference from Gauss as 0 and Guass as 1
for(i9.v in 1:N.v){
  if (psharp.v [i9.v] < 0.1){
    psharpG.v [i9.v] = 0
  }
  else{
    psharpG.v [i9.v] = 1
  }
}

#show the interpretable significance results psharpG.n
show(psharpG.v)


#2.2.1.2 Mean
#calculate the mean values for each distribution

mean.v = array(1:N.v, dim=1)

for (i3.v in 1:N.v){
  mean.v [i3.v]  = mean(d.veg [,i3.v])
}

#2.2.1.3 Mann-Whitney U test
#Decide if the differece between tests is signifikant (for non-parametric reasonable distributions)

#Matrix of paiwise p values

p.v = matrix(1:M.v, byrow =TRUE,nrow=N.v)

for (i5.v in 1:N.v) {
  for (i4.v in 1:N.v){
    p.v [i5.v,i4.v] = wilcox.test(d.veg [,i5.v],d.veg [,i4.v],paired=TRUE)$p.value
  }
}


#transform to see which values are above 0.05 (no significant differences between the two distributions)

ps.v = matrix(1:M.v, byrow =TRUE,nrow=N.v)

#get rid of all NaN values
for (i5.v in 1:N.v) {
  for (i4.v in 1:N.v){
    if (is.nan(p.v [i5.v,i4.v])){
      p.v [i5.v,i4.v] = 1
    }
  }
}

#make date interpretable by comparing to significance p<0.05 code significant difference  as 0 and Guass as 1
for (i6.v in 1:N.v) {
  for (i7.v in 1:N.v){
    if(p.v [i6.v,i7.v] < 0.05){
      ps.v [i6.v,i7.v] = 0
    }
    else{
      ps.v [i6.v,i7.v] = 1
    }
  }
}

#show the interpretable significance results ps
show(ps.v)

#2.2.1.3 Bootstrap test

#wrapper for mean gets taken from the nanotechnology part so no need to define here

#use boot and save the data for each set providing mean and sd

bootmn.v = array(1:N.v, dim = 1)
bootsd.v = array(1:N.v, dim = 1)

for(i10.v in 1:N.v){
  boot2.out <- boot(data = d.veg [,i10.v], statistic = mean.fun, R = 1000, sim = "ordinary")
  bootmn.v [i10.v] = apply(boot2.out$t, 2, mean)
  bootsd.v [i10.v] = apply(boot2.out$t, 2, sd)
}

#Ergebnisse in einer Matrix zusammen stellen
#rows for table
T.v = 2* N.v

#Table
boot.v = matrix(1:T.v, byrow=TRUE, nrow = N.v)

#Füllen Mittelwerte
for (i11.v in 1:N.v){
  boot.v [i11.v, 1] = bootmn.v [i11.v]
}

#Füllen std
for (i12.v in 1:N.v){
  boot.v [i12.v, 2] = bootsd.v [i12.v]
}

#Show results with means and corresponding std deviations in a table
show(boot.v)

#now test if in 1sigma der is an overlap between means
boots.v = matrix(1:M.v, byrow =TRUE,nrow=N.v)

#make date interpretable by comparing to significance of 1 sigma overlap code significant difference  as 0 and not as 1
for (i12.v in 1:N.v) {
  for (i13.v in 1:N.v){
    if( xor(boot.v [i12.v,1] + boot.v [i12.v,2] < boot.v [i13.v,1] - boot.v [i13.v,2], boot.v [i12.v,1] - boot.v [i12.v,2] > boot.v [i13.v,1] + boot.v [i13.v,2]) ){
      boots.v [i12.v,i13.v] = 0
    }
    else{
      boots.v [i12.v,i13.v] = 1
    }
  }
}

#show the interpretable significance results boots
show(boots.v)

#3. Auswahlkriterium

#3.1 Nanotechnologie

#creation of densitiy distribution for each text

helldis.n = matrix(1:M.n, byrow =TRUE,nrow=N.n)

for (i15.n in 1:N.n) {
  for (i14.n in 1:N.n){
    helldis.n [i15.n,i14.n] = try(hellinger(d.nano [,i15.n],d.nano [,i14.n]))
  }
}

show(helldis.n)

helldisc.n = matrix(1:M.n, byrow =TRUE,nrow=N.n)

for (i16.n in 1:N.n) {
  for (i17.n in 1:N.n){
    if (helldis.n [i16.n,i17.n] != "Error in stats::integrate(g, lower, upper) : roundoff error was detected\n"){
     helldisc.n [i16.n,i17.n] = hellinger(d.nano [,i16.n],d.nano [,i17.n])
    }
    else{
      helldisc.n [i16.n,i17.n] = 0
    }
  }
}

show(helldisc.n)

#now determine maximum distance spread
#compare datensätze

hellnoneg.n <- helldisc.n [c(-9,-10,-11,-12,-13,-14),c(-9,-10,-11,-12,-13,-14)]
hellnopos.n <- helldisc.n [c(-1,-2,-3,-4), c(-1,-2,-3,-4)]
hellnoneu.n <- helldisc.n [c(-5,-6,-7,-8),c(-5,-6,-7,-8)]

npos.n = 4
nneu.n = 4
nneg.n = 6

#finde maxima pos-neg
maxpb.n = array(1:npos.n, dim = 1)  

for (i18.n in 1:npos.n) {
  maxpb.n [i18.n] = which.max(hellnoneu.n [i18.n,])
}

#finde maxima neg-pos
maxbp.n = array(1:nneg.n, dim = 1)

for (i19.n in 1:nneg.n) {
  maxbp.n [i19.n] = which.max(hellnoneu.n [i19.n + 4,])
}

#wir beginnen mit der maximalen Distanz nagativ-positiv um ein größmögliches Spektrum aufzuspannen
#Reduktion des Datensatzes für die Wahl des zweiten Partners für Pos/Neg

#nun die zweite iteration mit dem maximalen Datensatz jeweils entfernt (1,11)

#neue datensätze
hellnoneu1m.n <- hellnoneu.n [c(-1),c(-1)]
hellnoneu7m.n <- hellnoneu.n [c(-7),c(-7)]

#finden der neuen maxima:

#finde maxima pos-neg2
maxpb2.n = array(1:npos.n, dim = 1)  

for (i20.n in 1:npos.n) {
  maxpb2.n [i20.n] = which.max(hellnoneu7m.n [i20.n,])
}

#finde maxima neg-pos2
maxbp2.n = array(1:nneg.n, dim = 1)

for (i21.n in 1:nneg.n) {
  maxbp2.n [i21.n] = which.max(hellnoneu1m.n [i21.n + 3,])
}

#auswahl ist: (2,12)
#gesamtauswahl für pos/neg: (1,2,11,12)

#nun werden die mittleren Elemente konstruiert.
#Diese müssen eine möglichst neutrale Gesamtdistanz zwischen positiv nd negativ aufweisen

Mneu.n = nneu.n*nneu.n
neu2.n = 2* nneu.n

tdistneu.n = matrix(1:Mneu.n, byrow =TRUE,nrow=nneu.n)


for (i22.n in 1:nneu.n){
    tdistneu.n [i22.n, 1] = helldisc.n [i22.n + 4, 1] 
}
for (i23.n in 1:nneu.n){
  tdistneu.n [i23.n, 2] = helldisc.n [i23.n + 4, 2] 
}
for (i24.n in 1:nneu.n){
  tdistneu.n [i24.n, 3] = helldisc.n [i24.n + 4, 11] 
}
for (i25.n in 1:nneu.n){
  tdistneu.n [i25.n, 4] = helldisc.n [i25.n + 4, 12] 
}

show(tdistneu.n)

#bestimmen der gesamten summe für alle selektierten punkte
mindist.n = array(1:nneu.n, dim = 1)

for (i26.n in 1:nneu.n) {
  mindist.n [i26.n] = tdistneu.n [i26.n,1] + tdistneu.n [i26.n,2] - tdistneu.n [i26.n,3] - tdistneu.n [i26.n,4]
}

show(mindist.n)

#die kelinsten elemente sind das 1. und 2. neutrale also text 5 und 6
#die Menge aller Texte ist: (1,2,5,6,11,12)


#3.2 Vegetarismus

#creation of densitiy distribution for each text

helldis.v = matrix(1:M.v, byrow =TRUE,nrow=N.v)

for (i15.v in 1:N.v) {
  for (i14.v in 1:N.v){
    helldis.v [i15.v,i14.v] = try(hellinger(d.veg [,i15.v],d.veg [,i14.v]))
  }
}

show(helldis.v)

helldisc.v = matrix(1:M.v, byrow =TRUE,nrow=N.v)

for (i16.v in 1:N.v) {
  for (i17.v in 1:N.v){
    if (helldis.v [i16.v,i17.v] != "Error in stats::integrate(g, lower, upper) : roundoff error was detected\n"){
      helldisc.v [i16.v,i17.v] = hellinger(d.veg [,i16.v],d.veg [,i17.v])
    }
    else{
      helldisc.v [i16.v,i17.v] = 0
    }
  }
}

show(helldisc.v)

#now determine maximum distance spread
#compare datensätze

hellnoneg.v <- helldisc.v [c(-9,-10,-11,-12,-13,-14),c(-9,-10,-11,-12,-13,-14)]
hellnopos.v <- helldisc.v [c(-1,-2,-3,-4), c(-1,-2,-3,-4)]
hellnoneu.v <- helldisc.v [c(-5,-6,-7,-8),c(-5,-6,-7,-8)]

npos.v = 4
nneu.v = 4
nneg.v = 4

#finde maxima pos-neg
maxpb.v = array(1:npos.v, dim = 1)  

for (i18.v in 1:npos.v) {
  maxpb.v [i18.v] = which.max(hellnoneu.v [i18.v,])
}

#finde maxima neg-pos
maxbp.v = array(1:nneg.v, dim = 1)

for (i19.v in 1:nneg.v) {
  maxbp.v [i19.v] = which.max(hellnoneu.v [i19.v + 4,])
}

#wir beginnen mit der maximalen Distanz nagativ-positiv um ein größmögliches Spektrum aufzuspannen
#Reduktion des Datensatzes für die Wahl des zweiten Partners für Pos/Neg

#nun die zweite iteration mit dem maximalen Datensatz jeweils entfernt (2,14)

#neue datensätze
hellnoneu2m.v <- hellnoneu.v [c(-2),c(-2)]
hellnoneu8m.v <- hellnoneu.v [c(-8),c(-8)]

#finden der neuen maxima:

#finde maxima pos-neg2
maxpb2.v = array(1:npos.v, dim = 1)  

for (i20.v in 1:npos.v) {
  maxpb2.v [i20.v] = which.max(hellnoneu8m.v [i20.v,])
}

#finde maxima neg-pos2
maxbp2.v = array(1:nneg.v, dim = 1)

for (i21.v in 1:nneg.v) {
  maxbp2.v [i21.v] = which.max(hellnoneu2m.v [i21.v + 3,])
}

#auswahl ist: (3,11)
#gesamtauswahl für pos/neg: (2,3,11,12)

#nun werden die mittleren Elemente konstruiert.
#Diese müssen eine möglichst neutrale Gesamtdistanz zwischen positiv nd negativ aufweisen

Mneu.v = nneu.v*nneu.v
neu2.v = 2* nneu.v

tdistneu.v = matrix(1:Mneu.v, byrow =TRUE,nrow=nneu.v)


for (i22.v in 1:nneu.v){
  tdistneu.v [i22.v, 1] = helldisc.v [i22.v + 4, 2] 
}
for (i23.v in 1:nneu.v){
  tdistneu.v [i23.v, 2] = helldisc.v [i23.v + 4, 3] 
}
for (i24.v in 1:nneu.v){
  tdistneu.v [i24.v, 3] = helldisc.v [i24.v + 4, 11] 
}
for (i25.v in 1:nneu.v){
  tdistneu.v [i25.v, 4] = helldisc.v [i25.v + 4, 12] 
}

show(tdistneu.v)

#bestimmen der gesamten summe für alle selektierten punkte
mindist.v = array(1:nneu.v, dim = 1)

for (i26.v in 1:nneu.v) {
  mindist.v [i26.v] = tdistneu.v [i26.v,1] + tdistneu.v [i26.v,2] - tdistneu.v [i26.v,3] - tdistneu.v [i26.v,4]
}

show(mindist.v)

#die kelinsten elemente sind das 2. und 4. neutrale also text 6 und 8
#die Menge aller Texte ist: (2,3,6,8,11,12)

#A. Testarea

#Bootstrapping

boot.out <- boot(data = d, statistic = mean.fun, R = 1000, sim = "ordinary")

# Bootstrap 95% CI for R-Squared

# function to obtain R-Squared from the data
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
}
# bootstrapping with 1000 replications
results <- boot(data=mtcars, statistic=rsq,
                R=1000, formula=mpg~wt+disp)

# view results
results
plot(results)

# get 95% confidence interval
boot.ci(results, type="bca")

test = seq(-8,8,by=2)
test2 = seq(-9,9,by=BWest.n [1])

ist(x, breaks=c(-4,-3,-2,-1,0,1,2,3,4,5))
#provide breakpoints for hist myself und enforce -8,8

hist(...,breaks=seq(-8,10,by=my.bin.width),...)

#Auswahlkriterium Flächenoverlaps:
HellingerDist(d.nano [,1],d.nano [,2])





#for gauss
#examplecode
in.f1f2 <- function(x, mu1, mu2, sd1, sd2) {
  f1 <- dnorm(x, mean=mu1, sd=sd1)
  f2 <- dnorm(x, mean=mu2, sd=sd2)
  pmin(f1, f2)
}

mu1 <- 2;    sd1 <- 2
mu2 <- 1;    sd2 <- 1

xs <- seq(min(mu1 - 3*sd1, mu2 - 3*sd2), max(mu1 + 3*sd1, mu2 + 3*sd2), .01)
f1 <- dnorm(xs, mean=mu1, sd=sd1)
f2 <- dnorm(xs, mean=mu2, sd=sd2)

plot(xs, f1, type="l", ylim=c(0, max(f1,f2)), ylab="density")
lines(xs, f2, lty="dotted")
ys <- min.f1f2(xs, mu1=mu1, mu2=mu2, sd1=sd1, sd2=sd2)
xs <- c(xs, xs[1])
ys <- c(ys, ys[1])
polygon(xs, ys, col="gray")

### only works for sd1 = sd2
SMD <- (mu1-mu2)/sd1
2 * pnorm(-abs(SMD)/2)

### this works in general
integrate(min.f1f2, -Inf, Inf, mu1=mu1, mu2=mu2, sd1=sd1, sd2=sd2)

#kd max tree opt
prob.n = array(1:N.n, dim = 1)
prob2.n = rep(6/N.n,N.n)

for (i18.n in 1:N.n){
  prob.n [i18.n] = 1
}

x = 1 - helldisc.n

show(x)

lpm2_kdtree(
  prob2.n,
  x [1,],  
  m=14,
  algorithm = "kdtree", 
  maxCheck = 3,
  termDist = 0.07,
  resample = 10
)


helldisc.n [8,]
rep(N.n,N.n)


#Notes
#Scott's normal reference rule tries to minimize the bias in variance
#of the histogram compared with the data set, while assuming normally distributed data.