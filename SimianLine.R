
#source(file = "SimianLine.R")

setwd("/home/samuel/Documents/warwickMathSys/Medical Statistics with Advanced Topics/Project")

df <- read.csv(file="Palm lines.csv", header=TRUE)

names(df) #names of headers of data frame

dim(df) #dimensions of data frame

head(df, n=10) #first 10 rows of data frame

Timestamp=df[,c(1)]
PalmLinesRH=df[,c(2)]
PalmLinesLH=df[,c(3)]
DominantH=df[,c(4)]
Age=df[,c(5)]
Gender=df[,c(6)]
Sexual_orient=df[,c(7)]
Ethnicity=df[,c(8)]
Occupation=df[,c(9)]
Education_level=df[,c(10)]
Uni_subject=df[,c(11)]
Believe_God=df[,c(12)]
Belief_importance=df[,c(13)]
Religious_practice_engagement=df[,c(14)]
Conditions=df[,c(15)]
Older_siblings=df[,c(16)]
Older_siblings_samesex=df[,c(17)]
Extroverted_enthusiastic=df[,c(18)]
Critical_quarrelsome=df[,c(19)]
Dependable_selfdisciplined=df[,c(20)]
Anxious_upset=df[,c(21)]
Open_new_experiences=df[,c(22)]
Reserved_quiet=df[,c(23)]
Sympathetic_warm=df[,c(24)]
Disorganised_careless=df[,c(25)]
Calm_emotionally_stable=df[,c(26)]
Conventional_uncreative=df[,c(27)]

#m=0
#f=0
#other=0

#for (i in c(1:418)){
# if(Gender[i]=="Male") {
#m=m+1
#}
#else if(Gender[i]=="Female"){
#f=f+1
#}
#else{
#other=other+1
#}
#}

##########################################################
#Palm Lines - D simian lines

table(PalmLinesRH)
#A 212
#B 105
#C 50
#D 51

table(PalmLinesLH)
#A 206
#B 104
#C 49
#D 59

##########################################################################
table(DominantH)
#Ambidexter 1
#Ambidextrous 1
#Left hand 53
#Right hand 363

###############################################################################
mean(Age) #=28.9

#install.packages("ggplot2")
#library(ggplot2)
png("Hist_Age.png")
plot = hist(Age)
dev.off()
#ggsave(plot,file="Hist_age.pdf")

##################################################################################
table(Gender)
#183 males
#232 females
#1 non-binary
#2 Prefer not to say

##################################################################################
table(Ethnicity)
#write.table(table(Ethnicity), "Ethnicity.csv", sep="\t", row.names=FALSE, col.names=FALSE)

####################################################################################	
table(Occupation)

table(Education_level)

table(Uni_subject)

######################################################################################
#Faith 1 strongly disagree - 5 strongly agree

table(Believe_God)
#1 94
#2 64
#3 74
#4 53
#5 133

table(Belief_importance)
#1 61
#2 55
#3 109
#4 77
#5 116

table(Religious_practice_engagement)
#1 208
#2 56
#3 60
#4 31
#5 63

#####################################################################

table(Conditions)


#######################################################################

table(Older_siblings)
#0 192
#1 132
#2 54
#3 22
#4 11
#5 4
#6 1
#8 1
#42 1 not likely!

table(Older_siblings_samesex)
#0 272
#1 86
#2 32
#3 6

#Personality 1 disagree strongly - 7 agree strongly

table(Extroverted_enthusiastic)
#1 23
#2 40
#3 73
#4 85
#5 101
#6 65
#7 31

table(Critical_quarrelsome)
#1 31
#2 86
#3 75
#4 82
#5 98
#6 37
#7 9

table(Dependable_selfdisciplined)

#1 1
#2 16
#3 26
#4 75
#5 99
#6 125
#7 76

table(Anxious_upset)
#1 41
#2 93
#3 66
#4 77
#5 73
#6 50
#7 18

table(Open_new_experinces)
#2 14
#3 31
#4 59
#5 132
#6 123
#7 59

table(Reserved_quiet)
#1 26
#2 72
#3 82
#4 75
#5 76
#6 64
#7 23

table(Sympathetic_warm)
#1 2
#2 15
#3 23
#4 58
#5 110
#6 123
#7 83

table(Disorganised_careless)

#1 80
#2 103
#3 71
#4 66
#5 56
#6 30
#7 12

table(Calm_emotionally_stable)
#1 6
#2 31
#3 62
#4 93
#5 86
#6 93
#7 47

table(Conventional_uncreative)
#1 69
#2 91
#3 96
#4 76
#5 49
#6 27
#7 10

#############################################################################

#Contingincy tables

ct=table(PalmLinesLH,PalmLinesRH)

chisq.test(ct)

#######################################################
ct=table(Gender, PalmLinesLH)
chisq.test(ct)
ct=table(Gender,PalmLinesRH)
chisq.test(ct)

#######################################################
table(Age, PalmLinesLH)
table(Age, PalmLinesRH)
#######################################################
table(Conditions,PalmLinesLH)
table(Conditions,PalmLinesRH)
########################################################
ct=table(Older_siblings, PalmLinesLH)
chisq.test(ct)
ct=table(Older_siblings, PalmLinesRH)
chisq.test(ct)

ct=table(Older_siblings_samesex, PalmLinesLH)
chisq.test(ct)
ct=table(Older_siblings_samesex, PalmLinesRH)
chisq.test(ct)


########################################## Faith and Palm lines
#seems relationship faith and simian lines

ct=table(Believe_God,PalmLinesLH)
chisq.test(ct)
ct=table(Believe_God,PalmLinesRH)
chisq.test(ct)

ct=table(Belief_importance,PalmLinesLH)
chisq.test(ct)
ct=table(Belief_importance,PalmLinesRH)
chisq.test(ct)

ct=table(Religious_practice_engagement,PalmLinesLH)
chisq.test(ct)
ct=table(Religious_practice_engagement,PalmLinesRH)
chisq.test(ct)


######################################################## Personality

ct=table(Extroverted_enthusiastic,PalmLinesLH)
chisq.test(ct)
ct=table(Extroverted_enthusiastic,PalmLinesRH)
chisq.test(ct)

ct=table(Critical_quarrelsome, PalmLinesLH)
chisq.test(ct)
ct=table(Critical_quarrelsome, PalmLinesRH)
chisq.test(ct)

ct=table(Dependable_selfdisciplined,PalmLinesLH)
chisq.test(ct)
ct=table(Dependable_selfdisciplined,PalmLinesRH)
chisq.test(ct)

ct=table(Anxious_upset,PalmLinesLH)
chisq.test(ct)
ct=table(Anxious_upset,PalmLinesRH)
chisq.test(ct)

ct=table(Open_new_experiences,PalmLinesLH)
chisq.test(ct)
ct=table(Open_new_experiences,PalmLinesRH)
chisq.test(ct)

ct=table(Reserved_quiet,PalmLinesLH)
chisq.test(ct)
ct=table(Reserved_quiet,PalmLinesRH)
chisq.test(ct)

ct=table(Sympathetic_warm,PalmLinesLH)
chisq.test(ct)
ct=table(Sympathetic_warm,PalmLinesRH)
chisq.test(ct)

ct=table(Disorganised_careless,PalmLinesLH)
chisq.test(ct)
ct=table(Disorganised_careless,PalmLinesRH)
chisq.test(ct)

ct=table(Calm_emotionally_stable,PalmLinesLH)
chisq.test(ct)
ct=table(Calm_emotionally_stable,PalmLinesRH)
chisq.test(ct)

ct=table(Conventional_uncreative,PalmLinesLH)
chisq.test(ct)
ct=table(Conventional_uncreative,PalmLinesRH)
chisq.test(ct)



###################################################################GLMs




















