
##### Introduction #####

library(foreign)
bes15 <- read.dta("BES2015_W4_v4.0.dta")

# 31,546 observations with 673 variables

# which variables do we need for our model?

#Political Variables
#- Partylab #
-# evCameron, evMiliband, evSturgeon, evClegg
- #evSNP, evCon, evLabour, evLD
- #nSatdemscot, nsatdemuk, nsatdemeu
- #nApprovescotgov, napproveukgov
- #Scotshare
- #nScotdevmax
#Nationality
#- nBritnatid
#- nScotnatid
#- nEngnatid
#Economic variables
-# npovertyrisk
- #Nunemprisk
- #Npersonalecon12dec
- #Ngenecon12dec
- #Ncutslocal
- #Ncutsnatl
- #Ncutsnhs
- #Ncostoutlook
- #nEconoutlook, neconoutlookdec
- #Deficit
- #Nimmigecon
#Demographics
- #Employment, employmentdum
- #Gender
- #Housing
- #Id
- #Age, age group
- #Edlevel, highed
- # Al_scale, lr_scale
- #Referendum votes
- #Scotrefdum

# start with referendum votes to make binary variable

##### Referendum votes ######

table(bes15$profile_scotref_vote)
bes15$scotrefdum <- factor(bes15$profile_scotref_vote, levels = c("I voted 'No' (Scotland should not be an independent country)",
                                                                  "I voted 'Yes' (Scotland should be an independent country)")) # remove "don't know" answers

table(bes15$scotrefdum)


bes15$nscotrefdum <- as.numeric(bes15$scotrefdum)
table(bes15$nscotrefdum)

bes15$nscotrefdum <- ifelse((bes15$nscotrefdum == "1"), 0, 1)
table(bes15$nscotrefdum)


##### Political Variables #####
## partisanship
table(bes15$partyId)

library(dplyr)

bes15$partylab <- recode(bes15$partyId, "'Conservative' = '1. Conservative';
                         'Labour' = '2. Labour'; 'Liberal Democrat' = '3. Liberal Democrat';
                         'Scottish National Party (SNP)' = '4. SNP';
                         'United Kingdom Independence Party (UKIP)' = '5. UKIP';
                         'Green Party' = '6. Green Party'")

bes15 <- bes15 %>% 
  mutate(partylab = case_when(
    partyId == 'Conservative' ~ '1.Conservative',
    partyId == 'Labour' ~ '2. Labour',
    partyId == 'Liberal Democrat' ~ '3. Liberal Democrat',
    partyId == 'Scottish National Party (SNP)' ~ '4. SNP',
    partyId == 'United Kingdom Independence Party (UKIP)' ~ '5. UKIP',
    partyId == 'Green Party' ~ '6. Green Party',
    partyId == NA ~ 'None'
  ))


bes15$partylab2 <- factor(bes15$partylab, levels = c("1.Conservative", "2. Labour",
                                                    "3. Liberal Democrat", "4. SNP",
                                                    "5. UKIP", "6. Green Party", "None"))
table(bes15$partylab2)

## politician allegiance

table(bes15$likeCameron) # David Cameron (Conservative)
bes15$evCameron <- factor(bes15$likeCameron, levels = c("Strongly dislike", "1", "2",
                                                        "3", "4", "5", "6", "7", "8", 
                                                        "9", "Strongly like"))


table(bes15$likeMiliband) # Ed Miliband (Labour)
bes15$evMiliband <- factor(bes15$likeMiliband, levels = c("Strongly dislike", "1", "2",
                                                          "3", "4", "5", "6", "7", "8", 
                                                          "9", "Strongly like"))
table(bes15$evMiliband)

table(bes15$likeSturgeon) # Nicola Sturgeon (SNP)
bes15$evSturgeon <- factor(bes15$likeSturgeon, levels = c("Strongly dislike", "1", "2",
                                                          "3", "4", "5", "6", "7", "8", 
                                                          "9", "Strongly like"))

table(bes15$likeClegg) # Nick Clegg (Lib Dem)
bes15$evClegg <- factor(bes15$likeClegg, levels =  c("Strongly dislike", "1", "2",
                                                      "3", "4", "5", "6", "7", "8", 
                                                      "9", "Strongly like"))


table(bes15$likeSNP) # Include SNP because Nicola Sturgeon had just taken over SNP for Alex Salmond
bes15$evSNP <- factor(bes15$likeSNP, levels = c("Strongly dislike", "1", "2",
                                                "3", "4", "5", "6", "7", "8", 
                                                "9", "Strongly like"))
table(bes15$evSNP)


## include all important political parties
# Conservatives
bes15$evCon <- factor(bes15$likeCon, levels =  c("Strongly dislike", "1", "2",
                                                 "3", "4", "5", "6", "7", "8", 
                                                 "9", "Strongly like"))
table(bes15$evCon) # check
table(bes15$likeCon)

# Labour
bes15$evLabour <- factor(bes15$likeLab, levels = c("Strongly dislike", "1", "2",
                                                   "3", "4", "5", "6", "7", "8", 
                                                   "9", "Strongly like"))
table(bes15$likeLab)

table(bes15$evLab) # check

# Lib Dem
bes15$evLD <- factor(bes15$likeLD, levels =  c("Strongly dislike", "1", "2",
                                               "3", "4", "5", "6", "7", "8", 
                                               "9", "Strongly like"))
table(bes15$evLD)


# satisfaction with democracy
table(bes15$satDemScot) # Scot Dem
bes15$satdemscot <- factor(bes15$satDemScot, levels = c("Very dissatisfied", "A little dissatisfied",
                                                        "Fairly satisfied", "Very satisfied"))
table(bes15$satdemscot)



table(bes15$satDemUK) # UK Dem
bes15$satdemuk <- factor(bes15$satDemUK, levels = c("Very dissatisfied", "A little dissatisfied",
                                                    "Fairly satisfied", "Very satisfied"))
table(bes15$satdemuk)
bes15$nsatdemuk <- as.numeric(bes15$satdemuk)

# approve of Scottish/UK govt
table(bes15$approveScotGovt)
bes15$approvescotgov <- factor(bes15$approveScotGovt, levels = c("Strongly disapprove", "Disapprove",
                                                                 "Neither approve nor disapprove",
                                                                 "Approve", "Strongly approve"))
table(bes15$approvescotgov)

table(bes15$approveUKGovt)
bes15$approveukgov <- factor(bes15$approveUKGovt, levels = c("Strongly disapprove", "Disapprove",
                                                             "Neither approve nor disapprove",
                                                             "Approve", "Strongly approve"))
table(bes15$approveukgov)


# Fair Share of resources?
table(bes15$scotFairShare)
bes15$scotfairsharedec <- factor(bes15$scotFairShare, levels = c("Much less than its fair share",
                                                                 "A little less than its fair share",
                                                                 "More or less its fair share",
                                                                 "A little more than its fair share",
                                                                 "Much more than its fair share"))
# DevoMax
table(bes15$scotDevoMax)
bes15$scotdevmax <- factor(bes15$scotDevoMax, levels = c("It should have many fewer powers than it does now",
                                                         "It should have fewer powers than it does now",
                                                         "It should have about the same powers as it does now",
                                                         "It should have some more powers",
                                                         "It should have many more powers"))
table(bes15$scotdevmax)
bes15$nscotdevmax <- recode(bes15$scotdevmax, "'It should have many fewer powers than it does now' = 1;
                            'It should have fewer powers than it does now' = 2;
                            'It should have about the same powers as it does now' = 3;
                            'It should have some more powers' = 4;
                            'It should have many more powers' = 5")


##### Nationality #####
# British NatID (noNA)
table(bes15$britishness)
bes15$britnatid <- factor(bes15$britishness, c("Not at all British","2",
                                               "3", "4", "5", "6", "Very strongly British"))
table(bes15$britnatid)


# Scottish NatID (noNA)
table(bes15$scottishness)
bes15$scotnatid <- factor(bes15$scottishness, levels = c("Not at all Scottish","2",
                                                "3", "4", "5", "6", "Very strongly Scottish"))
table(bes15$scotnatid)


# English NatID (noNA)
table(bes15$englishness)
bes15$engnatid <- factor(bes15$englishness, levels = c("Not at all English", "2",
                                                       "3", "4", "5", "6", "Very strongly English"))
table(bes15$engnatid)


##### Economic Variables #####
# risks
table(bes15$riskPoverty) # Poverty
bes15$povertyrisk <- factor(bes15$riskPoverty, levels = c("Very unlikely", "Fairly unlikely",
                                                          "Neither likely nor unlikely", 
                                                          "Fairly likely", "Very likely"))
table(bes15$povertyrisk)

table(bes15$riskUnemployment) # unemployment
bes15$unemprisk <- factor(bes15$riskUnemployment, levels = c("Very unlikely", "Fairly unlikely",
                                                             "Neither likely nor unlikely", 
                                                             "Fairly likely", "Very likely"))
table(bes15$unemprisk)

# improvements
table(bes15$econPersonalRetro)
bes15$personalecon12 <- factor(bes15$econPersonalRetro, levels = c("Got a lot worse",
                                                                   "Got a little worse",
                                                                   "Stayed the same",
                                                                   "Got a little better",
                                                                   "Got a lot better"))


table(bes15$econGenRetro)
bes15$genecon12<- factor(bes15$econGenRetro, levels = c("Got a lot worse",
                                                        "Got a little worse",
                                                        "Stayed the same",
                                                        "Got a little better",
                                                        "Got a lot better"))
table(bes15$genecon12)


# public services
table(bes15$cutsTooFarLocal)
bes15$cutslocal <- factor(bes15$cutsTooFarLocal, levels = c("Not gone nearly far enough",
                                                            "Not gone far enough",
                                                            "About right", "Gone too far", 
                                                            "Gone much too far"))
table(bes15$cutslocal)

table(bes15$cutsTooFarNational)
bes15$cutsnatl <- factor(bes15$cutsTooFarNational, levels = c("Not gone nearly far enough",
                                                              "Not gone far enough",
                                                              "About right", "Gone too far", 
                                                              "Gone much too far"))
table(bes15$cutsnatl)

table(bes15$cutsTooFarNHS)
bes15$cutsnhs <- factor(bes15$cutsTooFarNHS, levels = c("Not gone nearly far enough",
                                                        "Not gone far enough",
                                                        "About right", "Gone too far", 
                                                        "Gone much too far"))
table(bes15$cutsnhs)

# perspective
table(bes15$changeCostLive)
bes15$costoutlook <- factor(bes15$changeCostLive, levels = c("Getting a lot lower",
                                                             "Getting a little lower",
                                                             "Staying about the same",
                                                             "Getting a little higher",
                                                             "Getting a lot higher"))
table(bes15$costoutlook) 

table(bes15$changeEconomy)
bes15$econoutlook <- factor(bes15$changeEconomy, levels = c("Getting a lot worse",
                                                            "Getting a little worse",
                                                            "Staying about the same",
                                                            "Getting a little better",
                                                            "Getting a lot better"))
table(bes15$econoutlook) # increasing

bes15$neconoutlookdec <- as.numeric(bes15$econoutlookdec)
table(bes15$neconoutlookdec) # decreasing
table(bes15$neconoutlook) # increasing

table(bes15$inequalityChange)
bes15$ineqchg <- factor(bes15$inequalityChange, levels = c("Larger", "Smaller", "About the same"))
table(bes15$ineqchg)

table(bes15$howToReduceDeficit)
bes15$deficit <- factor(bes15$howToReduceDeficit, levels = c("Only by increasing taxes", 
                                                    "Mainly by increasing taxes, but also by cutting spending",
                                                    "An equal balance of spending cuts and tax increases",
                                                    "Mainly by cutting spending, but with some tax increases",
                                                    "Only by cutting spending"))
table(bes15$deficit)

table(bes15$immigEcon) # immigration on economy
bes15$immigecon <- factor(bes15$immigEcon, levels = c("Bad for economy", "2", "3", "4",
                                                      "5", "6", "Good for economy"))
table(bes15$immigecon)


##### Demographics #####
table(bes15$workingStatus) # Employment
bes15$employment <- factor(bes15$workingStatus, c("Working full time (30 or more hours per week)",
                                                  "Working part time (8-29 hours a week)",
                                                  "Working part time (less than 8 hours a week)",
                                                  "Unemployed and looking for work",
                                                  "Full time university student",
                                                  "Other full time student",
                                                  "Retired", "Not in paid work for any other reason",
                                                  "Other"))

bes15$employmentdum <- ifelse(bes15$employmentdum %in% c("Working full time (30 or more hours per week)", "Working part time (8-29 hours a week)",
                                   "Working part time (less than 8 hours a week)"), "1. Employed", "0. Unemployed/Retired")
  
table(bes15$employmentdum)


table(bes15$gender)

table(bes15$profile_house_tenure)
bes15$housing <- as.numeric(bes15$profile_house_tenure)
table(bes15$housing)

bes15 <- bes15 %>% 
  mutate(housing2 = case_when(
    housing == 1 ~ "1. Own/Part-own",
    housing == 2 ~ "1. Own/Part-own",
    housing == 3 ~ "1. Own/Part-own",
    housing == 4 ~ "2. Rent",
    housing == 5 ~ "2. Rent",
    housing == 6 ~ "2. Rent",
    housing == 7 ~ "3. Live with family/friends",
    housing == 8 ~ "3. Live with family/friends"
  ))
  
table(bes15$housing2)

head(bes15$id)
class(bes15$id)

table(bes15$Age)
table(bes15$ageGroup)

table(bes15$edlevel)


##### Subset the data #####
bes15small <- subset(bes15, select = c(id, wt, scotrefdum, partylab, evCameron, evMiliband, evSturgeon, evClegg,
                                       evSNP, evCon, evLabour, evLD, nsatdemscot, nsatdemuk,
                                       nsatdemeu, napprovescotgov, napproveukgov, scotfairsharedec,
                                       nscotdevmax, nbritnatid, nscotnatid, nengnatid, npovertyrisk,
                                       nunemprisk, npersonalecon12dec, ngenecon12dec, personalecon12, 
                                       genecon12, ncutslocal, ncutsnatl, ncutsnhs, ncostoutlook,
                                       neconoutlook, neconoutlookdec, deficit, nimmigecon,
                                       employment, employmentdum, gender, housing, Age, ageGroup,
                                       edlevel, highed, al_scale, lr_scale, eurefdum))

bes15_small <- bes15 %>% 
  select(c(id, wt, nscotrefdum, partylab, evCameron, evMiliband, evSturgeon, evClegg, evSNP,
           evLabour, evLD, evCon, satdemscot, satdemuk, approvescotgov, approveukgov,
           scotfairsharedec, scotdevmax, britnatid, scotnatid, engnatid, povertyrisk,
           unemprisk, personalecon12, genecon12, cutslocal, cutsnatl, cutsnhs,
           costoutlook, econoutlook, ineqchg, immigecon, employmentdum, housing2,
           Age, ageGroup, edlevel, gender))

##### Regressions #####
head(bes15_small)

summary(bes15_small$ageGroup)
table(bes15_small$housing2)


bes15_small <- bes15_small %>% 
  filter(!ageGroup == "Under 18")

bes15_m1 <- glm(nscotrefdum ~ ageGroup + gender + edlevel + housing2 + employmentdum, family = binomial, data = bes15_small, weights = wt) # demographics model
summary(bes15_m1)

bes15_m2 <- glm(nscotrefdum ~ ageGroup + gender + edlevel + housing2 + employmentdum + britnatid + scotnatid, family = binomial, data = bes15_small, weights = wt) # national identity model
summary(bes15_m2)

bes15_m3 <- glm(nscotrefdum ~ ageGroup + gender + edlevel + housing2 + employmentdum + britnatid + 
                  scotnatid + partylab + evCameron + evMiliband + evSturgeon + evClegg + satdemscot, 
                family = binomial, data = bes15_small, 
                na.action = na.exclude, weights = wt) # political model
summary(bes15_m3)

bes15_m4 <- glm(nscotrefdum ~ ageGroup + gender + edlevel + housing2 + employmentdum + britnatid + 
                  scotnatid + partylab + evCameron + evMiliband + evSturgeon + evClegg + satdemscot +
                  cutslocal + cutsnatl + cutsnhs + unemprisk + povertyrisk + econoutlook,
                family = binomial, data = bes15_small,
                na.action = na.exclude, weights = wt) # economic model
summary(bes15_m4)

bes15_small$indyrefvotepredict <- predict.glm(bes15_m3, type = "response", na.action = na.exclude) # political model support scores

summary(bes15_small$indyrefvotepredict)
summary(bes15_small$indyrefvotepredict2)

library(lattice)

plottingobject <- densityplot( ~ indyrefvotepredict | nscotrefdum, 
                               data = bes15_small,
                               layout = c(1,2), aspect = 1, col = "darkblue",
                               plot.points = "rug",
                               strip=function(...) strip.default(..., style = 1))

plottingobject # see 0.6 to 0.8 as the target group to be persuaded

plotobject2 <- densityplot( ~ indyrefvotepredict2 | nscotrefdum,
                            data = bes15_small,
                            layout = c(1,2), aspect = 1, col = "darkblue",
                            plot.points = "rug",
                            strip=function(...) strip.default(..., style = 1))
plotobject2

##### Targeting #####
# Plot Object 2
bes15_scotindysupport <- bes15_small %>% 
  filter(indyrefvotepredict >= 0.8) # supporters of referendum

bes15_scotindyreject <- bes15_small %>% 
  filter(indyrefvotepredict <= 0.2) # people who oppose the referendum

bes15_scotindypersuade <- bes15_small %>% 
  filter(indyrefvotepredict >= 0.2 & indyrefvotepredict <=0.8)

bes15_small$indyref2_predict <- ifelse((bes15_small$indyrefvotepredict >= 0.8), 1, 0) # create binary to see whether they voted yes or no according to prediction

bes15_small$indyref2_predict <- factor(bes15_small$indyref2_predict,
                                       levels = c(0, 1), labels = c("NO", "YES"))

predict_table <- table(bes15_small$indyref2_predict, bes15_small$nscotrefdum)
predict_table

(predict_accuracy <- ((predict_table[1,1] + predict_table[2,2]) / sum(predict_table)))
## 88.8% accuracy rating on political model predictions

