#analysis on Schelling_on_GIS-@SES experiment_ethnic-SES-table and its implementation on boxplots

sesData <- read.table(file = "Schelling_on_GIS-@SES experiment_ethnic-SES-table.csv", header = T
                      , sep = ",", skip = 6, fill = T)



sortedSESData = sesData[order(sesData$X.run.number.),]

formattable::formattable(sortedSESData[1:10,]) #create format table


#first: filter on the housing contraints == false

sortedSESDataF = sortedSESData[sortedSESData$tie.houses.to.ses == "false",]

#Ethnic dissimilarity index - BetaSES

boxplot(EGJ ~ sortedSESDataF$beta.ses, data = sortedSESDataF, xlab = expression(paste(beta ["SES"]," (no housing constraint)"))
       , ylab = "Dissimilarity Index (D)", col = "lightgrey", main = "Dissimilarity Index on Ethnic-SES Scenario", ylim = c(0,1) )
boxplot(CHN ~ sortedSESDataF$beta.ses, data = sortedSESDataF, xlab = expression(paste(beta ["SES"]," (no housing constraint)"))
        , ylab = "Dissimilarity Index (D)" , col = "red", main = "Dissimilarity Index on Ethnic-SES Scenario", add = T)
boxplot(EGS ~ sortedSESDataF$beta.ses, data = sortedSESDataF, xlab = expression(paste(beta ["SES"]," (no housing constraint)"))
        , ylab = "Dissimilarity Index (D)" , col = "green", main = "Dissimilarity Index on Ethnic-SES Scenario", add = T)
boxplot(OTH ~ sortedSESDataF$beta.ses, data = sortedSESDataF, xlab = expression(paste(beta ["SES"]," (no housing constraint)"))
        , ylab = "Dissimilarity Index (D)" , col = "black", main = "Dissimilarity Index on Ethnic-SES Scenario", add = T)


abline(h=0.24, col="red", lwd=2)
abline(h=0.42, col="red", lwd=2)
abline(h=0.04, col="red", lwd=2)
abline(h=0.2, col="red", lwd=2)

legend("topright", inset=.02, title="Ethnicity",
       c("EGJ","CHINESE","EGS", "OTHER"), fill=c("grey", "red", "green", "black"), horiz=F, cex = 0.8)

boxplot(EGJ ~ sortedSESDataF$beta.ses, data = sortedSESDataF, xlab = "", ylab = ""
        , col = "lightgrey", main = "EGJ", ylim = c(0,1), cex.main = 2.5)
boxplot(CHN ~ sortedSESDataF$beta.ses, data = sortedSESDataF, xlab = ""
        , ylab = "" , col = "maroon", main = "CHINESE", ylim = c(0,1), cex.main = 2.5)
boxplot(EGS ~ sortedSESDataF$beta.ses, data = sortedSESDataF, xlab = ""
        , ylab = "" , col = "green", main = "EGS", ylim = c(0,1), cex.main = 2.5)
boxplot(OTH ~ sortedSESDataF$beta.ses, data = sortedSESDataF, xlab = ""
        , ylab = "" , col = "black", main = "OTHER", ylim = c(0,1), cex.main = 2.5)

legend("topright", inset=.02, title="Ethnicity",
       c("EGJ"), fill=c("grey"), horiz=F, cex = 0.8)


#excess Avg Simpson

boxplot(sortedSESDataF$simpson.index ~ sortedSESDataF$beta.ses, data = sortedSESDataF, xlab = ""
        , ylab = "", col = "orange", main = "Simpson Index", ylim = c(0,0.3), cex.main = 2.5 )
abline(h=0.018, col="red", lwd=2)



#Moran-I

boxplot(sortedSESDataF$moranI ~ sortedSESDataF$beta.ses, data = sortedSESDataF, xlab = ""
        , ylab = "", col = "blue", main = "Moran-I", ylim = c(0.4,0.9) , cex.main = 2.5)
abline(h=0.58, col="red", lwd=2)


#second: filter on the housing contraints == true

sortedSESDataT = sortedSESData[sortedSESData$tie.houses.to.ses == "true",]

boxplot(EGJ ~ sortedSESDataF$beta.ses, data = sortedSESDataT, xlab = ""
        , ylab = "Dissimilarity Index (D)", col = "lightgrey", main = "Dissimilarity Index on Ethnic-SES Scenario", ylim = c(0,1) )
boxplot(CHN ~ sortedSESDataF$beta.ses, data = sortedSESDataT
        , col = "red", main = "Dissimilarity Index on Ethnic-SES Scenario", add = T)
boxplot(EGS ~ sortedSESDataF$beta.ses, data = sortedSESDataT
        , col = "green", main = "Dissimilarity Index on Ethnic-SES Scenario", add = T)
boxplot(OTH ~ sortedSESDataF$beta.ses, data = sortedSESDataT
        , col = "black", main = "Dissimilarity Index on Ethnic-SES Scenario", add = T)

abline(h=0.24, col="red", lwd=2)
abline(h=0.42, col="red", lwd=2)
abline(h=0.04, col="red", lwd=2)
abline(h=0.2, col="red", lwd=2)



boxplot(EGJ ~ sortedSESDataT$beta.ses, data = sortedSESDataT, xlab = ""
        , ylab = "", col = "lightgrey", main = "EGJ", ylim = c(0,1), cex.main = 2.5, par(bg = "#f7f7f7") )
boxplot(CHN ~ sortedSESDataT$beta.ses, data = sortedSESDataT, xlab = ""
        , ylab = "" , col = "maroon", main = "CHINESE", ylim = c(0,1), cex.main = 2.5, par(bg = "#f7f7f7"))
boxplot(EGS ~ sortedSESDataT$beta.ses, data = sortedSESDataT, xlab = ""
        , ylab = "" , col = "green", main = "EGS", ylim = c(0,1), cex.main = 2.5, par(bg = "#f7f7f7"))
boxplot(OTH ~ sortedSESDataT$beta.ses, data = sortedSESDataT, xlab = ""
        , ylab = "" , col = "black", main = "OTHER", ylim = c(0,1), cex.main = 2.5, par(bg = "#f7f7f7"))

legend("topright", inset=.02, title="Ethnicity",
       c("EGJ","CHINESE","EGS", "OTHER"), fill=c("grey", "red", "green", "black"), horiz=F, cex = 0.8)


#excess Avg Simpson

boxplot(sortedSESDataT$simpson.index ~ sortedSESDataT$beta.ses, data = sortedSESDataT, xlab = ""
        , ylab = "", col = "orange", main = "Simpson Index", ylim = c(0,0.3), cex.main = 2.5, par(bg = "#f7f7f7") )
abline(h=0.018, col="red", lwd=2)

#Moran-I

boxplot(sortedSESDataT$moranI ~ sortedSESDataT$beta.ses, data = sortedSESDataT, xlab = ""
        , ylab = "", col = "blue", main = "Moran-I", ylim = c(0.4,0.9), cex.main = 2.5, par(bg = "#f7f7f7")  )
abline(h=0.58, col="red", lwd=2)

nrow(sesData)
