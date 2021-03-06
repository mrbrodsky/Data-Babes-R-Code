#Load Libraries

library("car")
library("caret")
library("gvlma")
library("predictmeans")
library("e1071")
library("lmtest")
library("readxl")
library("ggplot2")

#Import Datasets

Data1RegularConventional <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data1RegularConventional.xlsx")
View(Data1RegularConventional)
Data2RegularReformulated <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data2RegularReformulated.xlsx")
View(Data2RegularReformulated)
Data3RegularAllAreasAllFormulations <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data3AllAreasAllFormulations.xlsx")
View(Data3AllAreasAllFormulations)
Data4MidGradeConventional <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data4MidGradeConventional.xlsx")
View(Data4MidGradeConventional)
Data5MidGradeReformulated <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data5MidGradeReformulated.xlsx")
View(Data5MidGradeReformulated)
Data6MidGradeAllAreasAllFormulations <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data6MidGradeAllAreasAllFormulations.xlsx")
View(Data6MidGradeAllAreasAllFormulations)
Data7PremiumConventional <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data7PremiumConventional.xlsx")
View(Data7PremiumConventional)
Data8PremiumReformulated <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data8PremiumReformulated.xlsx")
View(Data8PremiumReformulated)
Data9PremiumAllAreasAllFormulations <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data9PremiumAllAreasAllFormulations.xlsx")
View(Data9PremiumAllAreasAllFormulations)
Data10AllGradesConventional <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data10AllGradesConventional.xlsx")
View(Data10AllGradesConventional)
Data11AllGradesReformulated <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data11AllGradesReformulated.xlsx")
View(Data11AllGradesReformulated)
Data12AllGradesAreasAndFormulations <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/Data12AllGradesAreasAndFormulations.xlsx")
View(Data12AllGradesAreasAndFormulations)
SeriesReport_20220608110458_31191e <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/SeriesReport-20220608110458_31191e.xlsx")
View(SeriesReport_20220608110458_31191e)
SeriesReport_20220608110510_4a84c3 <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/SeriesReport-20220608110510_4a84c3.xlsx")
View(SeriesReport_20220608110510_4a84c3)
SeriesReport_20220608110516_3befcc <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/SeriesReport-20220608110516_3befcc.xlsx")
View(SeriesReport_20220608110516_3befcc)
SeriesReport_20220608110521_4d3d85 <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/SeriesReport-20220608110521_4d3d85.xlsx")
View(SeriesReport_20220608110521_4d3d85)
SeriesReport_20220608110525_4e78be <- read_excel("EntityCoursework/SeriesReport-20220608110458_31191e/SeriesReport-20220608110525_4e78be.xlsx")
View(SeriesReport_20220608110525_4e78be)

#Asking the Questions
# What will future gas prices look like based on previous trends?
# How do gas prices differ among regions?

#Testing Assumptions for Linear Regression

#1. There is a linear relationship between x and y.
#Create a Scatterplot and Look at the Shape
scatter.smooth(x = Data12AllGradesAreasAndFormulations$Date, y= Data12AllGradesAreasAndFormulations$USAllAll)

# The data is not linear
# Examine the Residuals for Patterns-- 
# Now looking at Nonlinear Regression

#2. Test for Homoscedasticity: The error term is normally distributed.
lmMod <- lm(Date ~ USAllAll, data = Data12AllGradesAreasAndFormulations)
par(mfrow = c(2,2))
plot(lmMod)

#3. Homogenity of variance: the variance of the error terms is constant for all values of x.
lmtest::bptest(lmMod)
#NCV Test

car::ncvTest(lmMod)

#4. The x's are fixed and measured without error. (In other words, the x's can be considered as known constants.)
# x's are dates, so yes they are fixed and constant


#5. Multicollinearity: the observations are independent.


#6. Lack of outliers