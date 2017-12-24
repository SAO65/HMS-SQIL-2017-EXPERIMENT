# Does teamwork improve clinician and executive leaders’ individual performance? 
# A reproducible analysis of an experiment at the Harvard Medical School

# Stefano Olgiati, PhD, MS1*, Ankur Kalra, MD2, Alessandro Danovi, MS1, Marco Costa, MD, PhD, MBA2

# 1 University of Bergamo, Bergamo, Italy
# 2 Case Western Reserve University School of Medicine, Cleveland, Ohio, United States
# * Corresponding author: stefano.olgiati@unibg.it


# NORMALIZATION AND PLOTS
par(mar = c(5,4,4,2) + 0.1) ## default is c(5,4,4,2) + 0.1

coefficient_of_variation <- 0.25
mean_baseline <- 36
(sd_baseline <- mean_baseline * coefficient_of_variation)
(firstQuantile <- qnorm(0.025, mean=mean_baseline, sd=sd_baseline))
(lastQuantile <- qnorm(0.975, mean=mean_baseline, sd=sd_baseline))

x <- seq(0,72,0.01)

plot(x, dnorm(x, mean_baseline, sd_baseline), 
     type="l",
     lwd=2,
     lty=1,
     ylim=c(0,.1),
     yaxt="n",
     bty="n",
     main="HMS SQIL Experiment 04-2017 \n Expected Sampling Distributions",
     xlab="Score",
     ylab="",
     axes = FALSE)


axis(1, at = c(firstQuantile, mean_baseline, lastQuantile), 
     labels = c(round(firstQuantile,1), mean_baseline, round(lastQuantile, 1)), 
     col.axis="black", 
     tck=-.01,
     las=1)

abline(v=firstQuantile ,lty=2,col="black", lwd=.5)
abline(v=lastQuantile,lty=2,col="black", lwd=.5)
abline(v=mean_baseline,lty=1,col="black", lwd=.5)
abline(h=1/72,lty=2,col="black", lwd=2)
text(36, .09, "25% Variation Coefficient")
text(36, .08, "95% Confidence Interval")
text(11, .099, "very high   <<")
text(60, .099, ">>  very low")
text(41, .099, "> low")
text(31, .099, "high <")


lines(x, dnorm(x,mean_baseline/2, sd_baseline), 
      col="blue",
      lwd=2)

lines(x, dnorm(x,mean_baseline*1.5, sd_baseline), 
      col="red",
      lwd=2)


legend(-3,.07,  
       c("Uniform Performance","High Performance", "Baseline Performance", "Low Performance"),
       lty=c(2,1,1,1),
       lwd=c(2.5,2.5,2.5,2.5),
       col=c("black", "blue","black", "red"),
       bty = "n")