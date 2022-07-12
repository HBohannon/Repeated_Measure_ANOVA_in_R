#Load Libraries

library("rcompanion")
library("fastR2")
library("car")


breakfast1 <- breakfast[1:33,1:7]

keeps <- c("Participant.Code", "Treatment.Group", "Age..y.", "Sex", "Height..m.", "Baseline.Resting.Metabolic.Rate..kcal.d.", "Follow.Up.Resting.Metabolic.Rate..kcal.d.")
breakfast2 <- breakfast1[keeps]

#Baseline measure.
breakfast3 <- breakfast2[,1:5]
breakfast3$repdat <- breakfast2$Baseline.Body.Mass..kg.
breakfast3$contrasts <- "T1"

#Folow-up measure.
breakfast4 <- breakfast2[,1:5]
breakfast4$repdat <- breakfast2$Follow.Up.Body.Mass..kg.
breakfast4$contrasts <- "T2"

#Binding
breakfast5 <- rbind(breakfast3, breakfast4)

#Test for Normality

plotNormalHistogram(breakfast1$Baseline.Body.Mass..kg.)
plotNormalHistogram(breakfast1$Follow.Up.Body.Mass..kg.)

#Looks normal, so no need for transformation

#Testing for Homogeneity of Variance

leveneTest(repdat ~ Treatment.Group*contrasts, data=breakfast5)

#It was not significant, and the assumption was met

RManova2 <- aov(repdat~contrasts+Error(Participant.Code), breakfast5)
summary(RManova2)

#No significance here either.
