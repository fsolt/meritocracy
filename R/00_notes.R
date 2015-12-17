library(haven)
library(readxl)
library(readr)
library(dplyr)
library(magrittr)
library(lme4)
library(interplot)

# Commuting Zones
if (!file.exists("data/cw_cty00_cz.csv")) {
  temp <- tempfile(fileext = ".zip")
  download.file("http://www.equality-of-opportunity.org/images/crosswalks.zip", temp)
  cw <- read_dta(unzip(temp, "cw_cty00_cz.dta"))
  unlink(temp)
  write_csv(cw, "data/cw_cty00_cz.csv")
}

cz <- read_csv("data/cw_cty00_cz.csv") %>% 
  select(fips = county_id,
         cz = cz)

# Individual-Level Data: Pew 2007 Religious Landscape Survey
p2007 <- read_sav("data/dataset_Religious_Landscape_Survey_Data/Religious Landscape Survey Data - Continental US.sav")
p2007fips <- read_sav("data/dataset_Religious_Landscape_Survey_Data/FIPS Continental US.sav")

p2007x <- merge(p2007, p2007fips) %>% 
  transmute(resp = psraid,
            fips2 = as.numeric(fips),
            state = as.numeric(state),
            rej_merit = ifelse(q5c<=2, q5c-1, NA),
            income = ifelse(income<=9, income, NA), # 1 to 9
            educ = ifelse(educ<=7, educ, NA), # 1 to 7
            age = ifelse(age<99, age, NA),
            male = ifelse(sex==1, 1, 0),
            white = ifelse(race==1 & hisp!=1, 1, 0),
            ideo_con = 6 - ifelse(ideo<=5, ideo, NA), # 1 to 5
            attend = 7 - ifelse(q20<=6, q20, NA)) %>%  # 1 to 6
  rename(fips = fips2)
p2007x$partyid_rep <- plyr::mapvalues(p2007$party, 
                                      from = c(1:5, 9), 
                                      to = c(5, 1, 3, 3, 3, NA))
p2007x$partyid_rep[p2007$partyln==1] <- 4
p2007x$partyid_rep[p2007$partyln==2] <- 2

p2007x %<>% left_join(cz, by="fips") 

# 2004 Election Data
fips_cnty <- read_csv("https://raw.githubusercontent.com/raypereda/fips-county-codes/master/lib/national.txt", 
                      col_types="ccccc") 
names(fips_cnty) <- tolower(gsub(" ", "_", names(fips_cnty)))
fips_cnty$fips <- as.numeric(do.call(paste0, c(fips_cnty[, c(2,3)])))
fips_cnty$county <- tolower(gsub(" County| Parish", "", fips_cnty$county_name))
fips_cnty$county <- gsub(" ", "", fips_cnty$county)

bush04 <- read_tsv("http://bactra.org/election/vote-counts-with-NE-aggregated")
bush04$perc_bush04 <- with(bush04, Bush/(Bush+Kerry+Nader))
names(bush04) <- tolower(names(bush04))
bush04$county <- tolower(gsub(" County| Parish", "", bush04$county))
bush04$county <- gsub("saint", "st.", bush04$county)
bush04$county <- gsub(" ", "", bush04$county)
bush04$county[(bush04$state=="LA"|bush04$state=="MS") & bush04$county=="jeffdavis"] <- "jeffersondavis"
bush04$county[(bush04$state=="ME") & bush04$county=="linc"] <- "lincoln"
bush04$county[(bush04$state=="ME") & bush04$county=="andr"] <- "androscoggin"
bush04$county[(bush04$state=="ME") & bush04$county=="pen-s"] <- "penobscot"
bush04$county[(bush04$state=="ME") & bush04$county=="som-s"] <- "somerset"
bush04$county[(bush04$state=="ME") & bush04$county=="oxf-s"] <- "oxford"
bush04$county[(bush04$state=="MA") & bush04$county=="hamd"] <- "hamden"
bush04$county[(bush04$state=="MA") & bush04$county=="esse"] <- "essex"
bush04$county[(bush04$state=="MA") & bush04$county=="hams"] <- "hampshire"
bush04$county[(bush04$state=="NH") & bush04$county=="graf"] <- "grafton"
bush04$county[(bush04$state=="NY") & bush04$county=="manhattan"] <- "newyork"
bush04$county[(bush04$state=="NY") & bush04$county=="statenisland"] <- "richmond"
bush04$county[(bush04$state=="NY") & bush04$county=="brooklyn"] <- "kings"
bush04$county[(bush04$state=="VT") & bush04$county=="fran"] <- "franklin"
bush04$county[(bush04$state=="VT") & bush04$county=="wins"] <- "windsor"
bush04$county[(bush04$state=="VT") & bush04$county=="addi"] <- "addison"
bush04$county[(bush04$state=="VT") & bush04$county=="gris"] <- "grandisle"
bush04$county[(bush04$state=="VT") & bush04$county=="oran"] <- "orange"
bush04$county[(bush04$state=="VA") & bush04$county=="manassas"] <- "manassascity"
bush04$county[(bush04$state=="VA") & bush04$county=="norton"] <- "nortoncity"

bush04_cnty <- left_join(bush04, fips_cnty, by=c("state", "county"))
missing <- bush04_cnty[is.na(bush04_cnty$fips), 1:8] # election results still without fips due to county name inconsistencies
bush04_cnty <- bush04_cnty[!is.na(bush04_cnty$fips), ] # keep only results that already have fips
remaining <- anti_join(fips_cnty, bush04, by=c("state", "county")) %>%
  arrange(state) # fips without election results

missing$county0 <- missing$county # move county names to a tempvar
missing$county <- NA

states <- unique(missing$state)
states <- states[states != "AK"] # nothing to be done with Alaska election results--no breakdown in data
for(i in 1:length(states)) {
  t.rem <- remaining$county[remaining$state==states[i]] # fips without election results, one state at a time
  missing$county[missing$state==states[i]] <- lapply(missing$county0[missing$state==states[i]], function (ii) agrep(ii, t.rem, value=T, max.distance=.2)) # find matches to county name by state
}
missing$county <- unlist(lapply(missing$county, function(ii) ii[1])) # use closest match to county name
missing <- left_join(missing, fips_cnty, by=c("state", "county")) # now merge; some results still without fips in Maine, otherwise good
missing$county0 <- NULL # drop tempvar

bush04_cnty %<>% rbind(missing) 

cz_bush04 <- left_join(bush04_cnty, cz, by="fips") %>% 
  group_by(cz) %>% 
  summarize(bush04_cz = sum(bush)/sum(bush+kerry+nader))

# Equality of Opportunity
if (!file.exists("data/eq_of_opp_online_data.xls")) {
  download.file("http://www.equality-of-opportunity.org/images/online_data_tables.xls", "data/eq_of_opp_online_data.xls")
}

cz_eo <- read_excel("data/eq_of_opp_online_data.xls", 
                    sheet = "Online Data Table 5", 
                    skip = 49,
                    col_types = c("numeric", "text", "text", 
                                  rep("numeric", 32))) %>% 
  filter(!is.na(CZ)) %>% 
  select(cz = CZ,
         am_cz = `AM, 80-82 Cohort`,
         rm_cz = `RM, 80-82 Cohort`,
         p_c5p1_cz = `P(Child in Q5 | Parent in Q1), 80-85 Cohort`) 

cz_other <- read_excel("data/eq_of_opp_online_data.xls", 
                       sheet = "Online Data Table 8", 
                       skip = 6,
                       col_types = c("numeric", "text", "text", 
                                     rep("numeric", 38))) %>%
  filter(!CZ==-1) %>% 
  transmute(cz = CZ,
            gini_cz = Gini,
            income_cz = `Household Income per capita`/10000,
            black_cz = `Frac. Black`,
            pop_cz = `Census 2000 population`/100000,
            seg_race_cz = `Racial Segregation`,
            seg_inc_cz = `Income Segregation`,
            seg_pov_cz = `Segregation of Poverty (<p25)`,
            seg_aff_cz = `Segregation of Affluence (>p75)`) 

cz_all <- left_join(cz_eo, cz_bush04, by="cz") %>% 
  left_join(cz_other, by="cz")


# Merge data
p2007x1 <- left_join(p2007x, cz_all, by="cz")

p2007x1_w <- p2007x1 %>% filter(white==1) %>% select(-white)


m1_flat <- glm(formula = rej_merit~income+gini_cz+income:gini_cz+
                 educ+age+male+partyid_rep+ideo_con+attend+
                 income_cz+black_cz+bush04_cz+pop_cz,
               data=p2007x1_w, family=binomial(link="logit"))

m1 <- glmer(formula = rej_merit~income+gini_cz+income:gini_cz+
              educ+age+male+partyid_rep+ideo_con+attend+
              income_cz+black_cz+bush04_cz+pop_cz+
              (1+income|cz),
            data=p2007x1_w, family=binomial(link="logit"))

interplot(m1, "gini_cz", "income")

m2 <- glmer(formula = rej_merit~income+rm_cz+income:rm_cz+
              educ+age+male+partyid_rep+ideo_con+attend+
              gini_cz+income_cz+black_cz+bush04_cz+pop_cz+
              (1+income|cz),
            data=p2007x1_w, family=binomial(link="logit"))

interplot(m2, "rm_cz", "income")

m3 <- glmer(formula = rej_merit~income+rm_cz+income:rm_cz+
              educ+age+male+partyid_rep+ideo_con+attend+
              gini_cz+income_cz+black_cz+bush04_cz+pop_cz+
              seg_race_cz + seg_inc_cz + 
              (1+income|cz),
            data=p2007x1_w, family=binomial(link="logit"))

m4 <- glmer(formula = rej_merit~income+rm_cz+income:rm_cz+
              educ+age+male+partyid_rep+ideo_con+attend+
              gini_cz+income_cz+black_cz+bush04_cz+pop_cz+
              seg_race_cz + seg_pov_cz + seg_aff_cz + 
              (1+income|cz),
            data=p2007x1_w, family=binomial(link="logit"))

m5 <- glmer(formula = rej_merit~income+rm_cz+
              educ+age+male+partyid_rep+ideo_con+attend+
              gini_cz+income_cz+black_cz+bush04_cz+pop_cz+
              seg_race_cz + seg_inc_cz + seg_inc_cz:gini_cz+ 
              (1+income|cz),
            data=p2007x1_w, family=binomial(link="logit"))






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

# see Steele 2015