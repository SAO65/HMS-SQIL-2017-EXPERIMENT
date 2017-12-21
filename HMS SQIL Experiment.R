# DATA FROM THE HMS SQIL 04-2017 EXPERIMENT
TeamScore <- c(42,64,38,60,54,52,64)
AverageGroupScore <- c(51.25,65.6,51.25,61.3,60,61,57.2)
BestIndividualScore<-c(46,57,38,50,56,52,30)


# EXPLORATORY DATA ANALYSIS
(mean_TeamScore <- mean(TeamScore))
(sd_TeamScore <- sd(TeamScore))
(firstQuantile <- qnorm(0.025, mean=mean_TeamScore, sd=sd_TeamScore))
(lastQuantile <- qnorm(0.975, mean=mean_TeamScore, sd=sd_TeamScore))

(mean_AverageGroupScore <- mean(AverageGroupScore))
(sd_AverageGroupScore <- sd(AverageGroupScore))
(firstQuantile <- qnorm(0.025, mean=mean_AverageGroupScore, sd=sd_AverageGroupScore))
(lastQuantile <- qnorm(0.975, mean=mean_AverageGroupScore, sd=sd_AverageGroupScore))

(mean_BestIndividualScore <- mean(BestIndividualScore))
(sd_BestIndividualScore <- sd(BestIndividualScore))
(firstQuantile <- qnorm(0.025, mean=mean_BestIndividualScore, sd=sd_BestIndividualScore))
(lastQuantile <- qnorm(0.975, mean=mean_BestIndividualScore, sd=sd_BestIndividualScore))



strip.data <- data.frame(BestIndividualScore, AverageGroupScore, TeamScore)
stripchart(strip.data, 
           vertical=TRUE,
           axes=F,
           ylim=c(0,72),
           col=c("skyblue", "blue", "red"),
           cex=1,
           lty = "solid",
           lwd = 2,
           method="jitter",
           yaxt="n",
           bty="n",
           ylab="Score",
           main="Experiment HMS SQIL 04-2017 \n April 2017, Boston (MA) \n Scatterplot")
axis(1, at=c(1,2,3), labels = c("Best Individual","Group","Team"))
axis(2, at=c(0,36,72), las=1)
axis(4, pos=3.2, at=c(round(min(TeamScore), 0), round(mean(TeamScore), 0),round(max(TeamScore),0)), col.axis="black", las=2, cex.axis=0.7, tck=-.01)
axis(4, pos=2.2, at=c(round(min(AverageGroupScore), 0), round(mean(AverageGroupScore), 0),round(max(AverageGroupScore),0)), col.axis="black", las=2, cex.axis=0.7, tck=-.01)
axis(4, pos=1.2, at=c(round(min(BestIndividualScore), 0), round(mean(BestIndividualScore), 0),round(max(BestIndividualScore),0)), col.axis="black", las=2, cex.axis=0.7, tck=-.01)
abline(h=36, lty=2)
text(2, 39, "WORSE THAN BASELINE ABOVE")
text(2, 33, "BETTER THAN BASELINE BELOW")
text(2, 15, "Number of participants = 42")
text(2, 10, "Number of teams = 7")
text(2, 5, "Smaller Axes = min - median - max")
summary(strip.data)


# NORMALIZATION AND PLOTS
par(mar = c(5,4,4,2) + 0.1) ## default is c(5,4,4,2) + 0.1

coefficient_of_variation <- 0.25
mean_baseline <- 36
sd_baseline <- mean_baseline * coefficient_of_variation

(firstQuantile <- qnorm(0.025, mean=mean_baseline, sd=sd_baseline))
(lastQuantile <- qnorm(0.975, mean=mean_baseline, sd=sd_baseline))

x <- seq(0,72,0.01)

plot(x, dnorm(x, mean_baseline, sd_baseline), 
     type="l",
     lwd=2,
     lty=2,
     ylim=c(0,.1),
     yaxt="n",
     bty="n",
     main="HMS SQIL Experiment 04-2017 \n April 2017, Boston (MA) \n Sampling Distribution",
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
abline(v=mean,lty=1,col="black", lwd=.5)
text(36, .09, "25% Variation Coefficient")
text(36, .08, "95% Confidence Interval")
text(11, .099, "very high   <<")
text(60, .099, ">>  very low")
text(41, .099, "> low")
text(31, .099, "high <")

lines(x, dnorm(x,mean_BestIndividualScore, sd_BestIndividualScore), 
      col="skyblue",
      lwd=2)

lines(x, dnorm(x,mean_AverageGroupScore, sd_AverageGroupScore), 
      col="blue",
      lwd=2)

lines(x, dnorm(x,mean_TeamScore, sd_TeamScore), 
      col="red",
      lwd=2)

legend(-3,.07,  
       c("Baseline","Best Individual", "Group", "Team"),
       lty=c(2,1,1,1),
       lwd=c(2.5,2.5,2.5,2.5),
       col=c("black", "skyblue","blue", "red"),
       bty = "n")


# Actual Result 1
set.seed(1)
BaselineScore <- sample(rnorm(x, mean_baseline, sd_baseline), size = 7, replace = TRUE)
var.test(TeamScore, BaselineScore)
t.test(TeamScore, BaselineScore)

# Actual Result 2
var.test(TeamScore, AverageGroupScore)
t.test(TeamScore, AverageGroupScore)

# Actual Result 3
var.test(TeamScore, BestIndividualScore)
t.test(TeamScore, BestIndividualScore)

