#analysis on -@Religion experiment_ethnic-religion-table and its implementation on boxplots

relData <- read.table(file = "Schelling_on_GIS-@Religion experiment_ethnic-religion-table.csv", header = T
                      , sep = ",", skip = 6, fill = T)

sortedRelData = relData[order(relData$X.run.number.),]

#first: filter on the housing contraints == false

sortedRelDataF = sortedRelData[sortedRelData$tie.houses.to.religion == "false",]

#Ethnic dissimilarity index - BetaRel

boxplot(EGJ ~ sortedRelDataF$beta.rel, data = sortedRelDataF, xlab = expression(paste(beta ["Rel"]," (no housing constraint)"))
        , ylab = "Dissimilarity Index (D)", col = "lightgrey", main = "Dissimilarity Index on Ethnic-Religion Scenario", ylim = c(0,1) )
boxplot(CHN ~ sortedRelDataF$beta.rel, data = sortedRelDataF
        , col = "red", main = "Dissimilarity Index on Ethnic-Religion Scenario", add = T)
boxplot(EGS ~ sortedRelDataF$beta.rel, data = sortedRelDataF
        , col = "green", main = "Dissimilarity Index on Ethnic-Religion Scenario", add = T)
boxplot(OTH ~ sortedRelDataF$beta.rel, data = sortedRelDataF
        , col = "black", main = "Dissimilarity Index on Ethnic-Religion Scenario", add = T)


abline(h=0.24, col="red", lwd=2)
abline(h=0.42, col="red", lwd=2)
abline(h=0.04, col="red", lwd=2)
abline(h=0.2, col="red", lwd=2)

boxplot(EGJ ~ sortedRelDataF$beta.rel, data = sortedRelDataF, xlab = ""
        , ylab = "", col = "lightgrey", main = "EGJ", ylim = c(0,1), cex.main = 2.5 )
boxplot(CHN ~ sortedRelDataF$beta.rel, data = sortedRelDataF, xlab = ""
        , ylab = "" , col = "maroon", main = "CHINESE", ylim = c(0,1), cex.main = 2.5  )
boxplot(EGS ~ sortedRelDataF$beta.rel, data = sortedRelDataF, xlab = ""
        , ylab = "" , col = "green", main = "EGS", ylim = c(0,1), cex.main = 2.5   )
boxplot(OTH ~ sortedRelDataF$beta.rel, data = sortedRelDataF, xlab = ""
        , ylab = "" , col = "black", main = "OTHER", ylim = c(0,1) , cex.main = 2.5  )

legend("topright", inset=.02, title="Ethnicity",
       c("EGJ","CHINESE","EGS", "OTHER"), fill=c("grey", "red", "green", "black"), horiz=F, cex = 0.8)


#excess Avg Simpson

boxplot(sortedRelDataF$simpson.index ~ sortedRelDataF$beta.rel, data = sortedRelDataF, xlab = ""
        , ylab = "", col = "orange", main = "Simpson Index", ylim = c(0,0.3), cex.main = 2.5  )
abline(h=0.018, col="red", lwd=2)

#Moran-I

boxplot(sortedRelDataF$moranI ~ sortedRelDataF$beta.rel, data = sortedRelDataF, xlab = ""
        , ylab = "", col = "blue", main = "Moran-I", ylim = c(0.4,0.9), cex.main = 2.5  )
abline(h=0.58, col="red", lwd=2)


#second: filter on the housing contraints == true

sortedRelDataT = sortedRelData[sortedRelData$tie.houses.to.religion == "true",]

boxplot(EGJ ~ sortedRelDataF$beta.rel, data = sortedRelDataT, xlab = expression(paste(beta ["Rel"]," (with housing constraint)"))
        , ylab = "Dissimilarity Index (D)", col = "lightgrey", main = "Dissimilarity Index on Ethnic-Religion Scenario", ylim = c(0,1) )
boxplot(CHN ~ sortedRelDataF$beta.rel, data = sortedRelDataT
        , col = "red", main = "Dissimilarity Index on Ethnic-Religion Scenario", add = T)
boxplot(EGS ~ sortedRelDataF$beta.rel, data = sortedRelDataT
        , col = "green", main = "Dissimilarity Index on Ethnic-Religion Scenario", add = T)
boxplot(OTH ~ sortedRelDataF$beta.rel, data = sortedRelDataT
        , col = "black", main = "Dissimilarity Index on Ethnic-Religion Scenario", add = T)

abline(h=0.24, col="red", lwd=2)
abline(h=0.42, col="red", lwd=2)
abline(h=0.04, col="red", lwd=2)
abline(h=0.2, col="red", lwd=2)

boxplot(EGJ ~ sortedRelDataT$beta.rel, data = sortedRelDataT, xlab = ""
        , ylab = "", col = "lightgrey", main = "EGJ", ylim = c(0,1), cex.main = 2.5,  par(bg = "#f7f7f7")  )
boxplot(CHN ~ sortedRelDataT$beta.rel, data = sortedRelDataT, xlab = ""
        , ylab = "" , col = "red", main = "CHINESE", ylim = c(0,1), cex.main = 2.5,  par(bg = "#f7f7f7") )
boxplot(EGS ~ sortedRelDataT$beta.rel, data = sortedRelDataT, xlab = ""
        , ylab = "" , col = "green", main = "EGS", ylim = c(0,1), cex.main = 2.5,  par(bg = "#f7f7f7") )
boxplot(OTH ~ sortedRelDataT$beta.rel, data = sortedRelDataT, xlab = ""
        , ylab = "" , col = "black", main = "OTHER",ylim = c(0,1), cex.main = 2.5,  par(bg = "#f7f7f7")  )

legend("topright", inset=.02, title="Ethnicity",
       c("EGJ","CHINESE","EGS", "OTHER"), fill=c("grey", "red", "green", "black"), horiz=F, cex = 0.8)


#excess Avg Simpson

boxplot(sortedRelDataT$simpson.index ~ sortedRelDataT$beta.rel, data = sortedRelDataT, xlab = ""
        , ylab = "", col = "orange", main = "Simpson Index", ylim = c(0,0.3), cex.main = 2.5,  par(bg = "#f7f7f7"))
abline(h=0.018, col="red", lwd=2)

#Moran-I 

boxplot(sortedRelDataT$moranI ~ sortedRelDataT$beta.rel, data = sortedRelDataT, xlab = ""
        , ylab = "", col = "blue", main = "Moran-I", ylim = c(0.4,0.9) , cex.main = 2.5, par(bg = "#f7f7f7")
)
abline(h=0.58, col="red", lwd=2)

