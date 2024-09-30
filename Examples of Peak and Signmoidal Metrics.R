#Example of Lorentzian Peak distribution to calculate peak widths and height
#define function
fun.SBCO2<-function(x) {
  (2.9268/(1+((x--8.1620)/8.3461)^2))
}
#plot curve to check shape
curve(fun.SBCO2, -30,30, ylab = "cover")

#find peak x value (x)
optimize(fun.SBCO2, interval = c(-30,30), maximum = TRUE) 
# Peak value = -8.162

#find peak height (y value)
(2.9268/(1+(-8.162--8.1620)/8.3461)^2)
#Peak height value = 2.9268

#Width of a peak (Full width at half the maximum)
#half the maximum peak height(z=y/2)
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

#######################
#Sigmoidal distribution metrics of inflection point and area of change

#define function:
fun.SBCO2<-function(x) {
  (2.9268/(1+((x--8.1620)/8.3461)^2))
}
#plot curve to visualize
curve(fun.MF.SB.Sig, -32,32, ylab = "cover")

#Calculate first derivative
D(expression(44.585 + -14.3125/(1+exp(-(x-1.6314)/1.0975))), 'x')

#Function of first derivative
fun.MFSB.TOCkg.D1 <-function(x){-(14.3125 * (exp(-(x - 1.6314)/1.0975) * (1/1.0975))/(1 + exp(-(x - 
                                                                                                  1.6314)/1.0975))^2)}
#Plot first derivative for visualization
curve(fun.MFSB.TOCkg.D1, -32,32) 

#produces 1st derivative minimum (in this case)/ inflection point of original equation
optimize(fun.MFSB.TOCkg.D1, interval = c(-32,32) ) #returns maximum
#minimum
#[1] 1.6314 #<- This is the inflection point for MFSB TOC

#Y value for inflection point using minimum
fun.MFSB.TOCkg.D1_Y <- 44.585 + (-14.3125/(1+exp(-(-1.6314-1.6314)/1.0975)))
print(fun.MFSB.TOCkg.D1_Y) #
#43.8885 <-----Y Value of Inflection Point

#Calculation of 2nd Derivative
D(expression(-(14.3125 * (exp(-(x - 1.6314)/1.0975) * (1/1.0975))/(1 + exp(-(x - 
                                                                               1.6314)/1.0975))^2)), 'x')
#2nd Derivative
fun.MFSB.TOCkg.D2 <- function(x) {14.3125 * (exp(-(x - 1.6314)/1.0975) * (1/1.0975) * (1/1.0975))/(1 + 
                                 exp(-(x - 1.6314)/1.0975))^2 - 14.3125 * (exp(-(x - 1.6314)/1.0975) * 
                               (1/1.0975)) * (2 * (exp(-(x - 1.6314)/1.0975) * (1/1.0975) *
                                    (1 + exp(-(x - 1.6314)/1.0975))))/((1 + exp(-(x - 1.6314)/1.0975))^2)^2
  }
#Plot of 2nd Derivative
curve(fun.MFSB.TOCkg.D2, -32,32)

optimize(fun.MFSB.TOCkg.D2, interval = c(-32,1000), maximum = TRUE) # Max of 2nd derivative
optimize(fun.MFSB.TOCkg.D2, interval = c(-32,32)) # Minimum of 2nd derivative

#MF.SB AoT
#0.1860423 to 3.076766

