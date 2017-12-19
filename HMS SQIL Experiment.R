# DATA FROM THE HMS SQIL 04-2017 EXPERIMENT
BestIndividualScore<-c(46,57,38,50,56,52,30)
AverageGroupScore <- c(51.25,65.6,51.25,61.3,60,61,57.2)
TeamScore <- c(42,64,38,60,54,52,64)


# EXPLORATORY DATA ANALYSIS
(mean_BestIndividualScore <- mean(BestIndividualScore))
(sd_BestIndividualScore <- sd(BestIndividualScore))
(firstQuantile <- qnorm(0.05, mean=mean_BestIndividualScore, sd=sd_BestIndividualScore))
(lastQuantile <- qnorm(0.95, mean=mean_BestIndividualScore, sd=sd_BestIndividualScore))

(mean_AverageGroupScore <- mean(AverageGroupScore))
(sd_AverageGroupScore <- sd(AverageGroupScore))
(firstQuantile <- qnorm(0.05, mean=mean_AverageGroupScore, sd=sd_AverageGroupScore))
(lastQuantile <- qnorm(0.95, mean=mean_AverageGroupScore, sd=sd_AverageGroupScore))

(mean_TeamScore <- mean(TeamScore))
(sd_TeamScore <- sd(TeamScore))
(firstQuantile <- qnorm(0.05, mean=mean_TeamScore, sd=sd_TeamScore))
(lastQuantile <- qnorm(0.95, mean=mean_TeamScore, sd=sd_TeamScore))


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
           main="Experiment HMS SQIL 04-2017 \n April 2017, Boston (MA)")
axis(1, at=c(1,2,3), labels = c("Best Individual","Group","Team"))
axis(2, at=c(0,36,72), las=1)
axis(4, pos=3.2, at=c(round(min(TeamScore), 0), round(mean(TeamScore), 0),round(max(TeamScore),0)), col.axis="black", las=2, cex.axis=0.7, tck=-.01)
axis(4, pos=2.2, at=c(round(min(AverageGroupScore), 0), round(mean(AverageGroupScore), 0),round(max(AverageGroupScore),0)), col.axis="black", las=2, cex.axis=0.7, tck=-.01)
axis(4, pos=1.2, at=c(round(min(BestIndividualScore), 0), round(mean(BestIndividualScore), 0),round(max(BestIndividualScore),0)), col.axis="black", las=2, cex.axis=0.7, tck=-.01)
abline(h=36, lty=2)
text(2.05, 40, "WORSE THAN BASELINE ABOVE")
text(2.05, 32, "BETTER THAN BASELINE BELOW")
text(2.05, 10, "Number of participants = 42")
text(2.05, 15, "Number of teams = 7")
summary(strip.data)
