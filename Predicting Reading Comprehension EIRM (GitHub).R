
# Install required packages
install.packages(c("eirm", "reshape2")) 

# Activate all required packages
# for data wrangling
library("dplyr")
library("reshape2")
library("Matrix")
library("lme4")
library("blme")
library("optimx")

# for explanatory IRT modeling
library("eirm")

# The project
original_data <- read.csv("Merged 12.csv", header = TRUE)

head(original_data, 10)
nrow(original_data)
ncol(original_data)

?melt.data.frame

long_data <- reshape2::melt(original_data, 
                           id.vars=c("CNTSTUID","ReadingStrategySum","Multilingualism","HomeLanguage","Gender"),
                           variable.name = "items", 
                           value.name = "responses")

head(long_data, 10)
nrow(long_data)
ncol(long_data)

long_data <- long_data[order(long_data$CNTSTUID, long_data$items),]

head(long_data, 20)


# Adding the text features. First, item format:

MC = c('CR424Q02S', 'CR424Q03S', 'CR424Q07S', 'CR560Q10S', 'CR560Q03S', 'CR560Q06S', 'CR560Q08S', 'CR545Q02S', 'CR545Q03S', 'CR545Q06S', 'CR545Q07S', 'CR559Q01S', 'CR559Q04S', 'CR559Q03S', 'CR559Q06S', 'CR220Q02S', 'CR220Q04S', 'CR220Q05S', 'CR220Q06S', 'CR067Q01S', 'CR456Q01S', 'CR547Q02S', 'CR547Q03S', 'CR547Q06S', 'CR547Q07S', 'CR547Q10S', 'CR540Q01S', 'CR540Q03S', 'CR540Q05S', 'CR540Q06S', 'CR542Q01S', 'CR542Q05S', 'CR542Q08S', 'CR542Q09S', 'CR455Q04S', 'CR455Q05S', 'CR550Q04S', 'CR550Q05S', 'CR550Q06S', 'CR055Q01S', 'CR111Q01S', 'CR446Q03S', 'CR546Q01S', 'CR546Q04S', 'CR546Q07S', 'CR549Q04S', 'CR549Q06S', 'CR549Q10S', 'CR549Q12S', 'CR549Q13S', 'CR558Q02S', 'CR558Q06S', 'CR558Q09S', 'CR558Q10S', 'CR437Q01S', 'CR437Q06S', 'CR561Q01S', 'CR561Q03S', 'CR561Q04S', 'CR561Q06S', 'CR561Q08S', 'CR562Q02S', 'CR562Q05S', 'CR562Q07S', 'CR564Q01S', 'CR564Q02S', 'CR564Q03S', 'CR564Q04S', 'CR565Q01S', 'CR565Q03S', 'CR565Q08S', 'CR565Q09S', 'CR404Q03S', 'CR404Q06S', 'CR404Q07S', 'CR453Q01S', 'CR453Q05S', 'CR553Q02S', 'CR553Q01S', 'CR553Q05S', 'CR553Q07S', 'CR569Q01S', 'CR569Q02S', 'CR569Q03S', 'CR569Q04S', 'CR466Q03S', 'CR412Q01S', 'CR412Q05S', 'CR412Q06S', 'CR432Q06S', 'CR543Q01S', 'CR543Q03S', 'CR543Q04S', 'CR543Q09S', 'CR543Q10S', 'CR543Q13S', 'CR552Q01S', 'CR552Q09S', 'CR552Q06S', 'CR460Q05S', 'CR460Q06S', 'CR554Q01S', 'CR554Q02S', 'CR554Q03S', 'CR554Q05S', 'CR556Q01S', 'CR556Q03S', 'CR556Q04S', 'CR556Q05S', 'CR556Q10S', 'CR556Q12S', 'CR544Q04S', 'CR544Q06S', 'CR544Q10S', 'CR544Q12S', 'CR544Q14S', 'CR227Q01S', 'CR227Q02S', 'CR541Q01S', 'CR541Q03S', 'CR541Q05S', 'CR541Q10S', 'CR563Q10S', 'CR563Q07S', 'CR563Q02S', 'CR563Q03S', 'CR551Q01S', 'CR551Q06S', 'CR551Q08S', 'CR551Q09S', 'CR551Q10S', 'CR102Q07S', 'CR567Q04S', 'CR567Q06S', 'CR567Q13S', 'CR567Q08S', 'CR567Q10S', 'CR567Q11S', 'CR568Q14S', 'CR568Q05S', 'CR568Q15S', 'CR568Q08S', 'CR568Q10S', 'CR570Q01S', 'CR570Q02S', 'CR570Q04S', 'CR570Q05S', 'CR570Q06S', 'CR570Q08S', 'CR573Q01S', 'CR573Q02S', 'CR573Q03S', 'CR573Q04S', 'CR566Q04S', 'CR566Q05S', 'CR566Q14S', 'CR566Q06S', 'CR566Q09S')
OR = c('DR545Q04C', 'DR559Q08C', 'CR220Q01S', 'DR067Q04C', 'DR067Q05C', 'DR456Q02C', 'DR456Q06C', 'DR547Q09C', 'DR540Q04C', 'DR542Q02C', 'DR420Q02C', 'DR420Q10C', 'DR420Q06C', 'DR420Q09C', 'DR455Q02C', 'DR455Q03C', 'DR550Q09C', 'DR550Q10C', 'DR550Q07C', 'DR055Q02C', 'DR055Q03C', 'DR055Q05C', 'DR111Q02BC', 'DR111Q06C', 'DR446Q06C', 'DR546Q03C', 'DR549Q05C', 'DR558Q04C', 'DR558Q12C', 'DR437Q07C', 'DR561Q07C', 'DR562Q03C', 'DR562Q06C', 'DR564Q05C', 'DR565Q02C', 'DR565Q05C', 'DR404Q10AC', 'DR404Q10BC', 'DR453Q04C', 'DR453Q06C', 'DR553Q04C', 'DR553Q06C', 'CR104Q01S', 'CR104Q02S', 'CR104Q05S', 'DR569Q06C', 'DR466Q02C', 'CR466Q06S', 'DR412Q08C', 'DR432Q01C', 'DR432Q05C', 'DR543Q15C', 'DR219Q01C', 'DR219Q01EC', 'DR219Q02C', 'DR552Q03C', 'DR552Q11C', 'DR552Q04C', 'DR552Q08C', 'DR460Q01C', 'DR406Q01C', 'DR406Q05C', 'DR406Q02C', 'DR554Q07C', 'DR556Q09C', 'DR544Q07C', 'DR544Q13C', 'DR227Q03C', 'DR227Q06C', 'DR541Q04C', 'DR541Q09C', 'DR541Q11C', 'DR563Q09C', 'DR563Q13C', 'DR563Q14C', 'DR551Q05C', 'DR551Q11C', 'DR102Q04C', 'DR102Q05C', 'DR567Q03C', 'DR568Q06C', 'DR568Q13C', 'DR570Q10C', 'DR573Q06C', 'DR566Q03C', 'DR566Q12C')

long_data$itemformat <- ifelse(long_data$items %in% MC, "MC",
                               ifelse(long_data$items %in% OR, "OR",
                                      "WARNING"))
print(table(long_data$itemformat))

head(long_data, 10)

# Adding the text features. Second, cognitive process subscale:

locate_info = c('CR560Q10S', 'CR547Q03S', 'CR540Q01S', 'CR550Q06S', 'CR446Q03S', 'CR546Q01S', 'CR558Q09S', 'CR561Q06S', 'CR553Q01S', 'CR553Q05S', 'CR553Q07S', 'CR569Q04S', 'CR412Q01S', 'CR543Q13S', 'CR552Q01S', 'CR554Q02S', 'CR556Q01S', 'CR563Q10S', 'CR563Q07S', 'CR563Q02S', 'CR551Q01S', 'CR551Q08S', 'CR567Q04S', 'CR567Q06S', 'CR567Q13S', 'CR568Q14S', 'CR568Q15S', 'CR568Q08S', 'CR566Q14S', 'CR566Q09S', 'DR545Q04C', 'DR547Q09C', 'DR420Q02C', 'DR420Q09C', 'DR455Q03C', 'DR550Q09C', 'DR550Q10C', 'CR104Q01S', 'CR104Q02S', 'CR104Q05S', 'DR466Q02C', 'CR466Q06S', 'DR432Q01C', 'DR219Q01C', 'DR460Q01C', 'DR563Q09C', 'DR563Q13C', 'DR563Q14C', 'DR102Q05C')
understand = c('CR424Q02S', 'CR560Q03S', 'CR560Q06S', 'CR560Q08S', 'CR545Q02S', 'CR545Q03S', 'CR559Q01S', 'CR559Q04S', 'CR559Q03S', 'CR559Q06S', 'CR220Q05S', 'CR220Q06S', 'CR067Q01S', 'CR456Q01S', 'CR547Q02S', 'CR547Q06S', 'CR547Q07S', 'CR547Q10S', 'CR540Q05S', 'CR542Q01S', 'CR542Q05S', 'CR542Q08S', 'CR542Q09S', 'CR455Q04S', 'CR455Q05S', 'CR550Q04S', 'CR550Q05S', 'CR055Q01S', 'CR546Q07S', 'CR549Q04S', 'CR549Q06S', 'CR549Q10S', 'CR549Q13S', 'CR558Q02S', 'CR437Q01S', 'CR561Q01S', 'CR561Q03S', 'CR562Q02S', 'CR562Q05S', 'CR562Q07S', 'CR564Q02S', 'CR564Q03S', 'CR564Q04S', 'CR565Q01S', 'CR565Q08S', 'CR404Q03S', 'CR404Q06S', 'CR404Q07S', 'CR453Q05S', 'CR553Q02S', 'CR569Q01S', 'CR569Q02S', 'CR569Q03S', 'CR466Q03S', 'CR412Q05S', 'CR412Q06S', 'CR543Q01S', 'CR543Q03S', 'CR543Q09S', 'CR543Q10S', 'CR460Q05S', 'CR460Q06S', 'CR554Q01S', 'CR554Q03S', 'CR554Q05S', 'CR556Q04S', 'CR556Q10S', 'CR544Q04S', 'CR544Q06S', 'CR227Q02S', 'CR541Q03S', 'CR541Q05S', 'CR563Q03S', 'CR551Q10S', 'CR567Q08S', 'CR567Q11S', 'CR568Q05S', 'CR568Q10S', 'CR570Q01S', 'CR570Q02S', 'CR570Q04S', 'CR570Q05S', 'CR570Q06S', 'CR570Q08S', 'CR573Q01S', 'CR573Q02S', 'CR573Q03S', 'CR573Q04S', 'CR566Q06S', 'DR559Q08C', 'CR220Q01S', 'DR067Q04C', 'DR456Q02C', 'DR456Q06C', 'DR540Q04C', 'DR542Q02C', 'DR420Q10C', 'DR550Q07C', 'DR055Q02C', 'DR055Q03C', 'DR055Q05C', 'DR111Q06C', 'DR446Q06C', 'DR546Q03C', 'DR549Q05C', 'DR437Q07C', 'DR562Q03C', 'DR562Q06C', 'DR564Q05C', 'DR404Q10AC', 'DR404Q10BC', 'DR553Q04C', 'DR569Q06C', 'DR412Q08C', 'DR219Q01EC', 'DR219Q02C', 'DR552Q03C', 'DR552Q04C', 'DR552Q08C', 'DR406Q01C', 'DR406Q05C', 'DR406Q02C', 'DR554Q07C', 'DR556Q09C', 'DR227Q03C', 'DR227Q06C', 'DR551Q05C', 'DR102Q04C', 'DR567Q03C', 'DR573Q06C', 'DR566Q03C')
reflect_evaluate = c('CR424Q03S', 'CR424Q07S', 'CR545Q06S', 'CR545Q07S', 'CR220Q02S', 'CR220Q04S', 'CR540Q03S', 'CR540Q06S', 'CR111Q01S', 'CR546Q04S', 'CR549Q12S', 'CR558Q06S', 'CR558Q10S', 'CR437Q06S', 'CR561Q04S', 'CR561Q08S', 'CR564Q01S', 'CR565Q03S', 'CR565Q09S', 'CR453Q01S', 'CR432Q06S', 'CR543Q04S', 'CR552Q09S', 'CR552Q06S', 'CR556Q03S', 'CR556Q05S', 'CR556Q12S', 'CR544Q10S', 'CR544Q12S', 'CR544Q14S', 'CR227Q01S', 'CR541Q01S', 'CR541Q10S', 'CR551Q06S', 'CR551Q09S', 'CR102Q07S', 'CR567Q10S', 'CR566Q04S', 'CR566Q05S', 'DR067Q05C', 'DR420Q06C', 'DR455Q02C', 'DR111Q02BC', 'DR558Q04C', 'DR558Q12C', 'DR561Q07C', 'DR565Q02C', 'DR565Q05C', 'DR453Q04C', 'DR453Q06C', 'DR553Q06C', 'DR432Q05C', 'DR543Q15C', 'DR552Q11C', 'DR544Q07C', 'DR544Q13C', 'DR541Q04C', 'DR541Q09C', 'DR541Q11C', 'DR551Q11C', 'DR568Q06C', 'DR568Q13C', 'DR570Q10C', 'DR566Q12C')

long_data$cogprocess <- ifelse(long_data$items %in% locate_info, "locate info",
                               ifelse(long_data$items %in% understand, "understand",
                                      ifelse(long_data$items %in% reflect_evaluate, "reflect & evaluate",
                                      "WARNING")))
print(table(long_data$cogprocess))

head(long_data, 10)

# cores
library(plyr)
library(doParallel)
detectCores()
# Assign all of the cores to our simulation! Or, you can select fewer. 
# For example, makecluster(2) for using only two cores
cl <- makeCluster(detectCores())


# Recoding bilinguals, trilinguals, and multilinguals as multilinguals
head(long_data)

long_data$Multilingualism_new <- ifelse(long_data$Multilingualism == 1, "monolingual",
                               ifelse(long_data$Multilingualism %in% c(2, 3, 4), "multilingual",
                                             "WARNING"))

table(long_data$Multilingualism)
table(long_data$Multilingualism_new)


# Some missing values in the responses are originally coded as 6, 7, 8, 9, 96, 98, & 99.
# recoding them as NA here.
# also changing some variables to "factor"/"categorical"
table(long_data$responses)

typeof(long_data)

long_data_new <- long_data
missing_codes <- c(6,7,8,9,96,98,99)

table(long_data_new$responses)

long_data_new$responses[long_data_new$responses %in% missing_codes] <- NA
table(long_data_new$responses)

# No need to do anything about missings in ReadingStrategySum
table(long_data_new$ReadingStrategySum)

# missing in multilingualism and Multilingualism_new
table (long_data_new$Multilingualism)
long_data_new$Multilingualism[long_data_new$Multilingualism %in% missing_codes] <- NA
long_data_new$Multilingualism = as.factor(long_data_new$Multilingualism)
class(long_data_new$Multilingualism)
table(long_data_new$Multilingualism)

table (long_data_new$Multilingualism_new)
long_data_new$Multilingualism_new[long_data_new$Multilingualism_new == 'WARNING'] <- NA
long_data_new$Multilingualism_new = as.factor(long_data_new$Multilingualism_new)
class(long_data_new$Multilingualism_new)
table(long_data_new$Multilingualism_new)

# No need to do anything about missings in Gender
table (long_data_new$Gender)
class(long_data_new$Gender)
long_data_new$Gender = as.factor(long_data_new$Gender)
class(long_data_new$Gender)

# missing in HomeLanguage
table (long_data_new$HomeLanguage)
long_data_new$HomeLanguage[long_data_new$HomeLanguage %in% missing_codes] <- NA
long_data_new$HomeLanguage = as.factor(long_data_new$HomeLanguage)
table(long_data_new$HomeLanguage)

# Also making other variables as factor
long_data_new$CNTSTUID = as.factor(long_data_new$CNTSTUID)
long_data_new$responses = as.factor(long_data_new$responses)
long_data_new$itemformat = as.factor(long_data_new$itemformat)
long_data_new$cogprocess = as.factor(long_data_new$cogprocess)

class(long_data_new$cogprocess)
head(long_data_new)
class(long_data_new$items)


# eirm models
model.reading0 <- eirm::eirm(formula = "responses ~ 1 + (1|CNTSTUID)", data = long_data_new)
print(model.reading0)



# only test features
model.reading1 <- eirm::eirm(formula = "responses ~ 1 + itemformat + cogprocess + (1|CNTSTUID)", data = long_data_new)

print(model.reading1)
plot(model.reading1)
save(model.reading1, file = "model1.RData")

model.reading1.1 <- eirm::eirm(formula = "responses ~ 1 + ReadingStrategySum + HomeLanguage + Multilingualism_new + Gender + (1|CNTSTUID)", data = long_data_new)
print(model.reading1.1)
save(model.reading1.1, file = "model1.1.RData")


# only person features
model.reading2 <- eirm::eirm(formula = "responses ~ 1 + Gender + HomeLanguage + Multilingualism_new + ReadingStrategySum + (1|CNTSTUID)", data = long_data_new)

print(model.reading2)
plot(model.reading2)
save(model.reading2, file = "model2.RData")

# Person features + homelang*multilingualism
model.reading3 <- eirm::eirm(formula = "responses ~ 1 + Gender + HomeLanguage + Multilingualism + ReadingStrategySum + HomeLanguage*Multilingualism + (1|CNTSTUID)", data = long_data_new)

print(model.reading3)
plot(model.reading3)
save(model.reading3, file = "model3.RData")
load('model3.RData')


marginalplot(model.reading3, predictors = c("HomeLanguage", "Multilingualism"))

# Both test and person features 
model.reading4 <- eirm::eirm(formula = "responses ~ 1 + itemformat + cogprocess + Gender + HomeLanguage + Multilingualism_new + ReadingStrategySum + (1|CNTSTUID)", data = long_data_new)

print(model.reading4)
plot(model.reading4)
save(model.reading4, file = "model4.RData")


model.reading4.1 <- eirm::eirm(formula = "responses ~ 1 + Gender + HomeLanguage + Multilingualism_new + ReadingStrategySum + itemformat + cogprocess + (1|CNTSTUID)", data = long_data_new)
print(model.reading4.1)
save(model.reading4.1, file = "model4.1.RData")


marginalplot(model.reading3, predictors = c("HomeLanguage", "Multilingualism"))

# Full model: test & person features + all interactions
model.reading5 <- eirm::eirm(formula = "responses ~ 1 + itemformat + cogprocess + Gender + HomeLanguage + Multilingualism_new + ReadingStrategySum + itemformat*Gender + itemformat*HomeLanguage + itemformat*Multilingualism_new + itemformat*ReadingStrategySum + cogprocess*Gender + cogprocess*HomeLanguage + cogprocess*Multilingualism_new + cogprocess*ReadingStrategySum + (1|CNTSTUID)", data = long_data_new)

print(model.reading5)
plot(model.reading5)
save(model.reading5, file = "model5.RData")



model.reading5.1 <- eirm::eirm(formula = "responses ~ 1 + Gender + HomeLanguage + Multilingualism_new + ReadingStrategySum + itemformat + cogprocess + itemformat*Gender + itemformat*HomeLanguage + itemformat*Multilingualism_new + itemformat*ReadingStrategySum + cogprocess*Gender + cogprocess*HomeLanguage + cogprocess*Multilingualism_new + cogprocess*ReadingStrategySum + (1|CNTSTUID)", data = long_data_new)
print(model.reading5.1)
save(model.reading5.1, file = "model5.1.RData")
load('model5.1.RData')



marginalplot(model.reading5, predictors = c("itemformat", "Gender"))


# Full model: test & person features + only significant interactions
model.reading6 <- eirm::eirm(formula = "responses ~ 1 + itemformat + cogprocess + Gender + HomeLanguage + Multilingualism_new + ReadingStrategySum + itemformat*Gender + cogprocess*HomeLanguage + cogprocess*ReadingStrategySum + (1|CNTSTUID)", data = long_data_new)

print(model.reading6)
plot(model.reading6)
save(model.reading6, file = "model6.RData")


model.reading6.1 <- eirm::eirm(formula = "responses ~ 1 + Gender + HomeLanguage + Multilingualism_new + ReadingStrategySum + itemformat + cogprocess + itemformat*Gender + cogprocess*HomeLanguage + cogprocess*ReadingStrategySum + (1|CNTSTUID)", data = long_data_new)
print(model.reading6.1)
save(model.reading6.1, file = "model6.1.RData")
load('model6.1.RData')
marginalplot(model.reading6.1, predictors = c("itemformat", "Gender"))
marginalplot(model.reading6.1, predictors = c("HomeLanguage", "cogprocess"))
marginalplot(model.reading6.1, predictors = c("cogprocess", "ReadingStrategySum"))
class(long_data_new$ReadingStrategySum)


summary(model.reading0$model)
summary(model.reading2$model)
summary(model.reading4.1$model)
summary(model.reading5$model)
summary(model.reading5.1$model)
summary(model.reading6.1$model)


# descriptives for item properties
table(long_data_new$cogprocess)

