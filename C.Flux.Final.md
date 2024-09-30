---
title: "CO2.Flux"
output:
  html_document:
    df_print: paged
  html_notebook: default
  pdf_document: default
---

#Libraries Needed
```{r}
#Libraries
library(dplyr) 
library(tidyverse)
library(ggplot2)
library(plotrix)
library(ggplot2)
library(gtable)
library(grid)
#library(Ryacas)
#library(psych)
library(nlme)
#library(piecewiseSEM)
#library(hypervolume)
#library(multcomp)
#library(AICcmodavg)
library(drc)
library(sjPlot)    # to visualizing mixed-effects models
library(effects)   # to visualizing mixed-effects models
library(lme4)      # "golden standard" for mixed-effects modelling in R (no p-values)
library(lmerTest)  # p-values for MEMs based on the Satterthwaite approximation
library(report)    # mainly for an "report" function
library(emmeans)   # post-hoc analysis
library(knitr)     # beautifying tables
library(sjstats)   # ICC - intraclass-correlation coefficient
library(caret)     # ML, model comparison & utility functions
```


```{r}
#Multiplot
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))}
  if (numPlots == 1) {
    print(plots[[1]])} else {
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      for (i in 1:numPlots) {
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))}}}
```

#Set working directory and import data
```{r}
#set working directory
setwd("~/OneDrive - Old Dominion University/Ongoing Research/SG-Carbon.Ecotone/R.Data")

#import flux data
CO2data.raw <- read.csv("C.Flux.csv")
```

#Preparation of Data for Graphing
```{r}
#Rename Plot
CO2data.raw$Plot<- CO2data.raw$Plot.n.Corrected.A
#Change Year to character
CO2data.raw$Year.Character <- as.character(CO2data.raw$Year)

#Seagrass Prep
#Filter for just Seagrass at CJ
CO2data.seagrass1 <- CO2data.raw %>%
  filter(Location %in% c("CJ1","CJ2", "CJ3"))
#SB-Mangrove Prep
#Filter for just Mangrove at SB
CO2data.SB.MF1 <- CO2data.raw %>%
  filter(Location %in% c("SB1","SB2", "SB3"))

#PU-Mangrove Prep
#Filter for just Mangrove at SB
CO2data.PU.MF1 <- CO2data.raw %>%
  filter(Location %in% c("PU1","PU2", "PU3"))

#Dataset with predictions and confidence intervals
library(readxl)
CI.Values.Flux<-read_excel("~/OneDrive - Old Dominion University/Ongoing Research/SG-Carbon.Ecotone/R.Data/Confidence Interval Values - Flux.xlsx", sheet = NULL)
```

#Regression Equation- CO2 -Seagrass
```{r}

#Export to JMP
write.csv(CO2data.seagrass1,"~/OneDrive - Old Dominion University/Ongoing Research/SG-Carbon.Ecotone/R.Data/CO2.SG.df.csv", row.names = FALSE)

#Used sigma plot 
#Linear Regression
#y= y0+a*x
#y0= -.2733
#a= 0.0171

#r2=0.2654
#p<0.0001

fun.CJCO2<-function(x){
  -0.2733+0.0171*x
}
```


#Plot points
```{r}
#Seagrass
CO2FluxSeagrass <- ggplot(CO2data.seagrass1) +
  xlab("Plot (m)") + #x-label
  ylab("CO2 Flux [µmol m-2 s-1]")+ #y-label
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none") +#Removes gray gridlines 
  geom_ribbon(data = CI.Values.Flux, aes(x = CJ.Flux.Plot, ymin = CJ.Flux.CI.Low, ymax= CJ.Flux.CI.High), color = "black", linetype= "dotted", fill = "gray", alpha = 0.7)+
  geom_jitter(data = CO2data.seagrass1, aes(x=Plot, y=CO2.Flux, color = Year.Character), size = 1.5)+
  stat_function(xlim =c(-38,20), fun =fun.CJCO2 , colour = "black")+#function curve for SBMF CO2 flux
   scale_color_manual(values = c("2020" = "skyblue2", "2021" = "blue"))+
  scale_x_continuous(limits=c(-40,27), breaks=c(-40,-30, -20, -10, 0, 10, 20, 30))+ # x-axis breaks
  scale_y_continuous(limits = c(-1.5, 7), breaks=c(-1.5, 0, 1.5, 3, 4.5, 6)) #Sets breaks on y axis
  
  
print(CO2FluxSeagrass)
```
Regression Equations SB Mangrove Flux
```{r}
#Export to JMP
write.csv(CO2data.SB.MF1,"~/OneDrive - Old Dominion University/Ongoing Research/SG-Carbon.Ecotone/R.Data/CO2data.SB.MF1.csv", row.names = FALSE)

#Ran in SigmaPlot- Regression Wizard
#Lorentzian Peak 3P
#y = a/(1+((x-x0)/b)^2)

#estimates
#a= 2.9268
#b= 8.3461
#x0= -8.1620

#r2 = 0.3518

fun.SBCO2<-function(x) {
  (2.9268/(1+((x--8.1620)/8.3461)^2))
}


#plot curve to see what it looks like
curve(fun.SBCO2, -30,30, ylab = "cover")

#find peak value (x)
optimize(fun.SBCO2, interval = c(-30,30), maximum = TRUE) 
# Peak value = -8.162

#find peak height (y)
 (2.9268/(1+(-8.162--8.1620)/8.3461)^2)
#Peak height value = 2.9268

#Width of a peak (Full Width at half the maximum)
#half the maximum peak height(z)
2.9268/2
z= 1.4634
fun.SBCO2(x=-16.508098)
fun.SBCO2(x=0.18410)

#Peak Width

#Full width half maximum
FWHM.SB <- 0.18410-(-16.508098)
#16.692198

#Half width of half maximum
HWHM.SB <-FWHM.SB/2
#8.346099
```


Plot SB Mangroves
```{r}
#SB Mangroves
CO2FluxSBMangrove <- ggplot(CO2data.SB.MF1) +
  xlab("Plot (m)") + #x-label
  ylab("CO2 Flux [µmol m-2 s-1]")+ #y-label
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none") +#Removes gray gridlines 
  geom_ribbon(data = CI.Values.Flux, aes(x = SB.Flux.Plot, ymin = SB.Flux.CI.Low, ymax= SB.Flux.CI.High), color = "black", linetype= "dotted", fill = "gray", alpha = 0.7)+
geom_rect(data=CO2data.SB.MF1, mapping=aes(xmin=-0.6905601, xmax=3.047878, ymin=-1.5, ymax=7), fill = "green3", alpha=0.008)+ # box
  geom_jitter(data = CO2data.SB.MF1, aes(x=Plot, y=CO2.Flux, color = Year.Character), size = 1.5)+
 #Change point colors
  stat_function(xlim =c(-22,16), fun =fun.SBCO2 , colour = "black", )+#function curve for SBMF CO2 flux
  geom_vline(xintercept=1.178655, linetype='dashed', color='black', size=1)+
   scale_color_manual(values = c("2020" = "darkolivegreen3", "2021" = "darkgreen"))+
    scale_x_continuous(limits=c(-25,25), breaks=c(-20, -10, 0, 10, 20))+ # x-axis breaks
  scale_y_continuous(limits = c(-1.5,7), breaks=c(-1.5, 0, 1.5, 3, 4.5, 6)) #Sets breaks on y axis

print(CO2FluxSBMangrove)

```


Regression Equations PU Mangrove Flux
```{r}
#Export 
write.csv(CO2data.PU.MF1,"~/OneDrive - Old Dominion University/Ongoing Research/SG-Carbon.Ecotone/R.Data/CO2data.PU.MF1.csv", row.names = FALSE)

#From SigmaPlot
#Lorentzian 3P
#Ran in SigmaPlot- Using Dynamic Fitter
#Lorentzian Peak 3P
#y = a/(1+((x-x0)/b)^2)

#estimates
#a= 3.5271
#b= 4.3026
#x0= -4.3700

#r2 = 0.2320

fun.PUCO2<-function(x) {
  (3.5271/(1+((x--4.3700)/4.3026)^2))
}


#plot curve to see what it looks like
curve(fun.PUCO2, -25,25, ylab = "cover")

#find peak value (x)
optimize(fun.PUCO2, interval = c(-25,25), maximum = TRUE) 
# Peak value = -4.37002

#find peak height (y)
(3.5271/(1+((-4.37002--4.3700)/4.3026)^2))

#Peak height value = 3.5271

#Width of a peak (Full Width at half the maximum)
#half the maximum (z)
3.5271/2
#1.76355
fun.PUCO2(x=-8.67260)
fun.PUCO2(x=-0.06740)

#Peak Width

#Full width half maximum
FWHM.PU <- -0.06740--8.67260
#8.6052

#Half width of half maximum
HWHM.PU <-FWHM.PU/2
#4.3026
```

#Needs to be finalized with new values (18 Aug. 2023)- For new AbCov values
```{r}
#Plot points for Mangroves @PU
CO2FluxPUMangrove <- ggplot(CO2data.PU.MF1) +
  xlab("Plot (m)") + #x-label
  ylab("CO2 Flux [µmol m-2 s-1]")+ #y-label
  ylim(-1.5,7) + #y-axis values
  scale_y_continuous(breaks=c(-1.5, 0, 1.5, 3, 4.5, 6))+ #Sets breaks on y axis
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none")+ #Removes gray gridlines 
  geom_ribbon(data = CI.Values.Flux, aes(x = PU.Flux.Plot, ymin = PU.Flux.CI.Low, ymax=PU.Flux.CI.High), color = "black", linetype= "dotted", fill = "gray", alpha = 0.5)+
    geom_rect(data=CO2data.PU.MF1, mapping=aes(xmin=-0.4032305, xmax=1.653239, ymin=-1.5, ymax=7), fill = "green3", alpha=0.008)+ # box
# box from abvgrnd cover
  geom_jitter(data = CO2data.PU.MF1, aes(x=Plot, y=CO2.Flux, color = Year.Character), size = 1.5)+
  stat_function(xlim =c(-32,22), fun =fun.PUCO2 , colour = "black", )+#function curve for PU CO2 flux
  geom_vline(xintercept=0.625, linetype='dashed', color='black', size=1)+
   scale_color_manual(values = c("2020" = "darkolivegreen3", "2021" = "darkgreen"))+
      scale_x_continuous(limits=c(-35,25), breaks=c(-30, -20, -10, 0, 10, 20, 30)) # x-axis breaks


print(CO2FluxPUMangrove)
```


```{r}
multiplot(CO2FluxSeagrass, CO2FluxSBMangrove, CO2FluxPUMangrove, cols=3)

```

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *
Cmd+Option+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Cmd+Shift+K* to preview the HTML file). 

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.

