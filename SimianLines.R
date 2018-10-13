# Import the data
palms <- read.csv("C:/Users/PC/Desktop/Palm-Lines-Final.csv", stringsAsFactors = FALSE)

# Remove case of 42 older siblings
palms$Older.Siblings[206] <- NA
# Turn "N/A" into actual NAs
palms[palms=="N/A"] <- NA

# Convert columns into appropriate types
numeric.cols <- c("Simian.Right", "Simian.Left", "Simian.Either", "Age", "Older.Siblings",
                  "Older.Siblings.Same.Sex", "Asian", "White", "Black", "Hispanic")
palms[numeric.cols] <- unlist(lapply(palms[numeric.cols], FUN = as.numeric))

fiveFactor.cols <- c("Belief", "Belief.Importance", "Engagement")
palms[fiveFactor.cols] <- lapply(palms[fiveFactor.cols], 
                                  FUN = function(x) factor(as.numeric(x), levels = 1:5, ordered = TRUE))

thirteenFactor.cols <- c("Extraversion", "Agreeableness", "Conscientiousness", 
                         "Emotional.Stability", "Openness.to.Experiences")
palms[thirteenFactor.cols] <- lapply(palms[thirteenFactor.cols], 
                                  FUN = function(x) factor(as.numeric(x), levels = -6:6, ordered = TRUE))

# Generate a list of (valid) medical conditions
conditions <- unique(unlist(strsplit(palms$Medical.Conditions, ";")))
remove <- c(2,11,21)
conditions <- conditions[setdiff(1:38, remove)]

# Create a dataframe for presence medical conditions in each individual
medicalConditions <- data.frame(matrix(ncol = length(conditions), nrow = nrow(palms)))
colnames(medicalConditions) = conditions
for(i in 1:nrow(medicalConditions)){
  medicalConditions[i,] <- conditions %in% strsplit(palms$Medical.Conditions, ";")[[i]]
}
medicalConditions$dyspraxia <- medicalConditions$dyspraxia | medicalConditions$`Possible autism and dyspraxia`
medicalConditions$`High Blood Pressure` <- medicalConditions$`High Blood Pressure` | medicalConditions$`High blood pressure`
medicalConditions$`Possible autism and dyspraxia` <- NULL 
medicalConditions$`High blood pressure` <- NULL
medicalConditions <- lapply(medicalConditions, as.numeric)

# Merge this dataframe with palms data
palms <- cbind(palms, medicalConditions)

# Chi squared tests for Simian lines and other variables - three are significant
chisq.test(table(palms$Simian.Either, palms$Older.Siblings))
chisq.test(table(palms$Simian.Either, palms$Belief))
chisq.test(table(palms$Simian.Either, palms$Age))

# Example model for EDA - link between age and Simian lines
ageModel <- glm(Simian.Either ~ Age, family = binomial(link="logit"), data=palms)
summary(ageModel)
# Plotting this model
newdat <- data.frame(Age=seq(min(palms$Age), max(palms$Age), len=100))
newdat$Simian = predict(ageModel, newdata = newdat, type="response")
plot(Simian.Either~Age, data=palms)
lines(Simian~Age, newdat, col="blue", lwd=2)

### Most common conditions are analysed individually. 
### anxiety, allergies, vision problems, depression, insomnia, anaemia, respiratory problems

anxietyModel <- glm(Anxiety ~ Age + Ethnicity + Gender + Simian.Either, family = binomial(link="logit"), data=palms)
summary(anxietyModel)
# Only gender is significant

allergiesModel <- glm(Allergies ~ Age + Ethnicity + Gender + Simian.Either, family = binomial(link="logit"), data=palms)
summary(allergiesModel)
# Only age is significant

visionModel <- glm(`Vision problems` ~ Age + Ethnicity + Gender + Simian.Either, family = binomial(link="logit"), data=palms)
summary(visionModel)
# No variables significant

depressionModel <- glm(Depression ~ Age + Ethnicity + Gender + Simian.Either, family = binomial(link="logit"), data=palms)
summary(depressionModel)
# Gender and Simian.Either both significant

insomniaModel <- glm(Insomnia ~ Age + Ethnicity + Gender + Simian.Either, family = binomial(link="logit"), data=palms)
summary(insomniaModel)
# Simian.Either significant

anaemiaModel <- glm(Anaemia ~ Age + Ethnicity + Gender + Simian.Either, family = binomial(link="logit"), data=palms)
summary(anaemiaModel)
# Only gender is significant - it is well known that women are more at risk

respiratoryModel <- glm(`Respiratory problems` ~ Age + Ethnicity + Gender + Simian.Either, family = binomial(link="logit"), data=palms)
summary(respiratoryModel)
# Only ethniticty (white) is significant

## Converting ordinal variables into binary
palms$Belief.BIN <- palms$Belief > 3
palms$Belief.Imp.BIN <- palms$Belief.Importance > 3
palms$Engagement.BIN <- palms$Engagement > 3
palms$Extraversion.BIN <- palms$Extraversion > 0
palms$Agreeableness.BIN <- palms$Agreeableness > 0
palms$Conscientiousness.BIN <- palms$Conscientiousness > 0
palms$Emotional.Stab.BIN <- palms$Emotional.Stability > 0
palms$Openness.BIN <- palms$Openness.to.Experiences > 0

## GLMs for binary belief/personality data

beliefModel <- glm(Belief.BIN ~ Age + Ethnicity + Gender + Simian.Either, family=binomial(link="logit"), data=palms)
summary(beliefModel)
# Ethnicity and Simian.Either highly significant 

beliefImpModel <- glm(Belief.Imp.BIN ~ Age + Ethnicity + Gender + Simian.Either, family=binomial(link="logit"), data=palms)
summary(beliefImpModel)
# Ethnicity significant

engagementModel <- glm(Engagement.BIN ~ Age + Ethnicity + Gender + Simian.Either, family=binomial(link="logit"), data=palms)
summary(engagementModel)
# Ethnicity significant

extraversionModel <- glm(Extraversion.BIN ~ Age + Ethnicity + Gender + Simian.Either, family=binomial(link="logit"), data=palms)
summary(extraversionModel)
# None significant

agreeablenessModel <- glm(Agreeableness.BIN ~ Age + Ethnicity + Gender + Simian.Either, family=binomial(link="logit"), data=palms)
summary(agreeablenessModel)
# Gender significant

conscientiousnessModel <- glm(Conscientiousness.BIN ~ Age + Ethnicity + Gender + Simian.Either, family=binomial(link="logit"), data=palms)
summary(conscientiousnessModel)
# Age and Simian.Either significant

emotionalModel <- glm(Emotional.Stab.BIN ~ Age + Ethnicity + Gender + Simian.Either, family=binomial(link="logit"), data=palms)
summary(emotionalModel)
# Age and gender significant

opennessModel <- glm(Openness.BIN ~ Age + Ethnicity + Gender + Simian.Either, family=binomial(link="logit"), data=palms)
summary(opennessModel)
# None significant



