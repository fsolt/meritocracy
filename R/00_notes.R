library(haven)
library(dplyr)

# Pew 2007
p2007 <- read_sav("data/dataset_Religious_Landscape_Survey_Data/Religious Landscape Survey Data - Continental US.sav")
p2007fips <- read_sav("data/dataset_Religious_Landscape_Survey_Data/FIPS Continental US.sav")

p2007x <- cbind(p2007, p2007fips[, 2]) %>% transmute(
  resp = psraid,
  fips2 = fips,
  state = as.numeric(state),
  rej_merit = ifelse(q5c<=2, q5c-1, NA),
  income = ifelse(income<=9, income, NA), # 1 to 9
  educ = ifelse(educ<=7, educ, NA), # 1 to 7
  age = ifelse(age<99, age, NA),
  male = ifelse(sex==1, 1, 0),
  white = ifelse(race==1 & hisp!=1, 1, 0),
  ideo = 6 - ifelse(ideo<=5, ideo, NA), # 1 to 5
  attend = 7 - ifelse(q20<=6, q20, NA)) %>%  # 1 to 6
rename(fips = fips2)

p2007x <- data.frame(
    resp = p2007$psraid,
    #    fips = p2007$qfips,
    state = as.numeric(p2007$state),
    rej_merit = ifelse(p2007$q5c<=2, p2007$q5c-1, NA),
    income = ifelse(p2007$income<=9, p2007$income, NA), # 1 to 9
    educ = ifelse(p2007$educ<=7, p2007$educ, NA), # 1 to 7
    age = ifelse(p2007$age<99, p2007$age, NA),
    male = ifelse(p2007$sex==1, 1, 0),
    white = ifelse(p2007$race==1 & p2007$hisp!=1, 1, 0),
    #    union = ifelse(p2007$labor<=3, 1, ifelse(p2007$labor==4, 0, NA)), # not asked this survey
    ideo = 6 - ifelse(p2007$ideo<=5, p2007$ideo, NA), # 1 to 5
    attend = 7 - ifelse(p2007$q20<=6, p2007$q20, NA) # 1 to 6
)
p2007x$partyid <- mapvalues(p2007$party, 
                            from = c(1:5, 9), 
                            to = c(5, 1, 3, 3, 3, NA))
p2007x$partyid[p2007$partyln==1] <- 4
p2007x$partyid[p2007$partyln==2] <- 2

# p2007x$unemp <- ifelse(p2007$employ==3 & p2007$employ2==4, 1, 0) # not asked this survey
# p2007x$unemp[p2007$employ==9 | p2007$employ2==9] <- NA

p2007x$year <- 2007
p2007x.w <- p2007x[p2007x$white==1, -c(8)]

t1m1.07.flat <- glm(formula = rej_merit~income+
                        educ+age+male+partyid+ideo+attend,
                    data=p2007x.w, family=binomial(link="logit"))

t1m1.07.x <- glmer(formula = rej_merit~income+
                       educ+age+male+partyid+ideo+attend+
                       (1+income|state),
                   data=p2007x.w, family=binomial(link="logit"))

# compare with other datasets
# t1m1.06.x <- glmer(formula = rej_merit~income+
#                        educ+age+male+partyid+ideo+attend+
#                        (1|state),
#                    data=p2006x.w, family=binomial(link="logit"))
# 
# t1m1.05.x <- glmer(formula = rej_merit~income+
#                        educ+age+male+partyid+ideo+attend+
#                        (1|state),
#                    data=p2005x.w, family=binomial(link="logit"))

# Cross-national
ga12 <- read_sav("Pew/Pew Research Global Attitudes Project Spring 2012 Dataset for web/Pew Research Global Attitudes Project Spring 2012 Dataset for web.sav")
names(ga12) <- tolower(names(ga12))

ga12x <- data.frame(
    resp = ga12$psraid,
    cc = as.numeric(ga12$country),
    rej_merit = ifelse(ga12$q84<=2, ga12$q84-1, NA),
    #q156    income = ifelse(ga12$income<=9, ga12$income, NA), # 1 to 9
    #q154    educ = ifelse(ga12$educ<=7, ga12$educ, NA), # 1 to 7
    age = ifelse(ga12$q142<99, ga12$q142, NA),
    male = ifelse(ga12$q141==1, 1, 0),
    #q159    white = ifelse(ga12$race==1 & ga12$hisp!=1, 1, 0),
    #na    union = ifelse(ga12$labor<=3, 1, ifelse(ga12$labor==4, 0, NA)),
    #na    ideo = 6 - ifelse(ga12$ideo<=5, ga12$ideo, NA), # 1 to 5
    attend = 7 - ifelse(ga12$q153<=6, ga12$q153, NA) # 1 to 6
)
cn <- data.frame(cc = attr(ga12$country, "labels"))
cn$country <- row.names(cn)
ga12x <- left_join(ga12x, cn)

# Get county & CZ mobility data
# see Steele 2015