library(caret)
library(readr)
library(corrplot)
library(ggplot2)
library(arules)
library(arulesViz)

rawData <- read.transactions("ElectronidexTransactions2017.csv", format = "basket", sep =",", rm.duplicates = TRUE)
rawData

inspect(rawData)
length(rawData)
size(rawData)
LIST(rawData)
itemLabels(rawData)

itemFrequency(rawData)
itemFrequencyPlot(rawData, type = "absolute", topN = 20)
#Seems imac, hp laptop, cyberpower pc seem to be the most sold.

defaultApriori <- apriori(rawData, parameter = list(supp = 0.1, conf = 0.8))
inspect(defaultApiori)
#Default we have none

rule1 <- apriori(rawData, parameter = list(supp = 0.05, conf = 0.8))
inspect(rule1)
#none

rule2 <- apriori(rawData, parameter = list(supp = 0.05, conf = 0.2))
inspect(rule2)
#9 rules made
summary(rule2)
#lift 1.000-1.591 (avg 1.414)

rule3 <- apriori(rawData, parameter = list(supp = 0.01, conf = 0.8))
inspect(rule3)
#none

rule4 <- apriori(rawData, parameter = list(supp = 0.01, conf = 0.5))
inspect(rule4)
#19 rules 
summary(rule4)
#lift 1.952-3.103 (avg 2.234)

rule5 <- apriori(rawData, parameter = list(supp = 0.1, conf = 0.15))
inspect(rule5)


rule6 <- apriori(rawData, parameter = list(supp = 0.01, conf = 0.6))
inspect(rule6)
#1 rule
#lift 3.102

rule7 <- apriori(rawData, parameter = list(supp = 0.001, conf = 0.55))
inspect(rule7)
summary(rule7)
#5584 rules lift 2.147-17.697 (3.43)


rule8 <- apriori(rawData, parameter = list(supp = 0.001, conf = 0.9))
inspect(rule8)
summary(rule8)
#197 rules lift 3.532-9.065 (4.384)
#This seems like good extreme for confidence

#now lets try to find the highest support
Srule1 <- apriori(rawData, parameter = list(supp = 0.1, conf = 0.001))
inspect(Srule1)
summary(Srule1)
#10 rules lift 1

Srule2 <- apriori(rawData, parameter = list(supp = 0.2, conf = 0.001))
inspect(Srule2)
#1 rules, lift 1

#seems support being .1 is good value for the extreme

#now lets find a balance of both

Brule <- apriori(rawData, parameter = list(supp = 0.05, conf = 0.9))
inspect(Brule)
#0 rules

Brule2 <- apriori(rawData, parameter = list(supp = 0.04, conf = 0.9))
inspect(Brule2)
#0 rules

Brule2 <- apriori(rawData, parameter = list(supp = 0.005, conf = 0.7))
inspect(Brule2)
#3 rules lift 2.7-4.1

Brule3 <- apriori(rawData, parameter = list(supp = 0.005, conf = 0.6))
inspect(Brule3)
summary(Brule3)
#54 rules lift 2.3-4.186 (avg 2.725)

inspect(head(sort( Brule3,decreasing = TRUE, by = "confidence")))
inspect(head(sort( Brule3,decreasing = TRUE, by = "support")))
inspect(head(sort( Brule3,decreasing = TRUE, by = "lift")))
inspect(head(sort( Brule3,decreasing = TRUE, by = "count")))

ItemRules <- subset(Brule3, items %in% "iMac")

is.redundant(Brule3)
#Seems there are no redundant entries

?plot

plot(Brule3[], method="graph", control=list(type="items")) 
