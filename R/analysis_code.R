rm(list=ls(all=TRUE))

# set working directory
#sink(file = "log_analysis.log", append = TRUE)
print(paste0("Start date and time: ", lubridate::now(tzone = "GMT")))

# Import packages
library(foreign)
library(dplyr)
library(ggplot2)
library(stargazer)
library(acs)
library(noncensus)
library(diagis)
library(sandwich)

# robust standard error function
rb_se <- function(var_name, w = NULL) {
  if (is.null(w)) {
    temp <- lm(var_name ~ 1)
  } else {
    temp <- lm(var_name ~ 1, weights = w) 
  }
  sqrt(diag(vcovHC(temp, type = "HC1")))
}

# function to round to 3 digits
round3 <- function(x) {
  sprintf("%.3f", round(x, 3))
}

# function to combine mean and standard error
mean_se <- function(mean, se) {
  paste0(round3(mean), " (", round3(se), ")")
}

###########################
# Import Data

# CPS
cps <- read.dta("march2016_cps_small.dta")
# keep only the adults
cps <- cps[cps$a_age >= 18,]

# Facebook data
cc <- read.csv("facebook_survey_data.csv", stringsAsFactors=FALSE)

# Load GfK survey: Spring 2016 Climate Change in the American Mind
load("gfk_2016.RData")
#########################################
# Clean and prepare variables

# standardized CPS weights
cps$weight <- (cps$hsup_wgt/sum(cps$hsup_wgt))*nrow(cps)

# gender
# baseline group: female
# Facebook survey
cc$ps_male <- cc$Gend == "Male"
# GfK survey
w16$ps_male <- w16$PPGENDER == "Male"
# CPS
cps$ps_male <- cps$a_sex == "Male"

# age
# baseline group: 65 and older
# Facebook survey
cc$ps_age_18_24 <- cc$Age == "18-24"
cc$ps_age_25_34 <- cc$Age == "25-34"
cc$ps_age_35_44 <- cc$Age == "35-44"
cc$ps_age_45_64 <- cc$Age == "45-64"
# GfK survey
w16$a_age <- as.numeric(as.character(w16$PPAGE))
w16$a_age[w16$a_age < 18] <- mean(w16$a_age[w16$a_age >= 18])
w16$ps_age_18_24 <- w16$a_age >= 18 & w16$a_age <= 24
w16$ps_age_25_34 <- w16$a_age >= 25 & w16$a_age <= 34
w16$ps_age_35_44 <- w16$a_age >= 35 & w16$a_age <= 44
w16$ps_age_45_64 <- w16$a_age >= 45 & w16$a_age <= 64
# CPS
cps$ps_age_18_24 <- cps$a_age >= 18 & cps$a_age <= 24
cps$ps_age_25_34 <- cps$a_age >= 25 & cps$a_age <= 34
cps$ps_age_35_44 <- cps$a_age >= 35 & cps$a_age <= 44
cps$ps_age_45_64 <- cps$a_age >= 45 & cps$a_age <= 64


# education
# baseline group: Did not graduate high school
# Facebook survey
cc$ps_edu_hs <- cc$Edu == "High school graduate, GED, or alternative"
cc$ps_edu_so <- cc$Edu == "Some college, or associates degree"
cc$ps_edu_ba <- cc$Edu == "Bachelor's (college) degree or equivalent"
cc$ps_edu_gr <- cc$Edu == "Graduate or professional degree (e.g., Master's Degree, M.D., Ph.D., J.D., MBA)"
# GfK survey
edu_l <- levels(w16$PPEDUC)
w16$ps_edu_hs <- w16$PPEDUC == edu_l[11]
w16$ps_edu_so <- w16$PPEDUC %in% edu_l[12:13]
w16$ps_edu_ba <- w16$PPEDUC == edu_l[14]
w16$ps_edu_gr <- w16$PPEDUC %in% edu_l[15:16]
# CPS
edu_l <- levels(cps$a_hga)
cps$ps_edu_hs <- cps$a_hga == edu_l[10]
cps$ps_edu_so <- cps$a_hga %in% edu_l[11:13]
cps$ps_edu_ba <- cps$a_hga == edu_l[14]
cps$ps_edu_gr <- cps$a_hga %in% edu_l[15:17]

# race
# baseline group: Other
# Facebook survey
cc$ps_r_h <- cc$Hisp == unique(cc$Hisp)[2]
cc$ps_r_w <- cc$Race == "White or Caucasian"
cc$ps_r_b <- cc$Race == "Black or African-American"
# GfK survey
w16$ps_r_h <- w16$PPETHM == "Hispanic"
w16$ps_r_h[is.na(w16$ps_r_h)] <- FALSE
w16$ps_r_w <- w16$PPETHM == "White, Non-Hispanic"
w16$ps_r_w[is.na(w16$ps_r_w)] <- FALSE
w16$ps_r_b <- w16$PPETHM == "Black, Non-Hispanic"
# CPS survey
w16$ps_r_b[is.na(w16$ps_r_b)] <- FALSE
cps$ps_r_h <- cps$pehspnon == "Yes"
cps$ps_r_w <- cps$prdtrace == "White only" & cps$pehspnon == "No"
cps$ps_r_b <- cps$prdtrace == "Black only" & cps$pehspnon == "No"

# metropolitan status 
# Facebook survey
cc$ps_metro <- cc$met_status == "Metropolitan"
cc$ps_metro[is.na(cc$met_status)] <- FALSE
# GfK survey
w16$ps_metro <- w16$PPMSACAT == "Metro"
# CPS survey
cps$ps_metro <- cps$gtmetsta == "Metropolitan"

# Census region
# regions
ne <- c("Connecticut", "Maine", "Massachusetts", 
        "New Hampshire", "Rhode Island", "Vermont")
ma <- c("New Jersey", "New York", "Pennsylvania")
enc <- c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin")
wnc <- c("Iowa", "Kansas", "Minnesota", "Missouri", 
         "Nebraska", "North Dakota", "South Dakota")
sa <- c("Delaware", "Florida", "Georgia", "Maryland", 
        "North Carolina", "South Carolina", "Virginia", 
        "District of Columbia", "West Virginia")
esc <- c("Alabama", "Kentucky", "Mississippi", "Tennessee")
wsc <- c("Arkansas", "Louisiana", "Oklahoma", "Texas")
mt <- c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", 
        "New Mexico", "Utah", "Wyoming")
pac <- c("Alaska", "California", "Hawaii", "Oregon", "Washington")

# Facebook survey
# baseline group: Northeast
# Facebook survey
cc$State[cc$State == "District of Columbia (Washington, DC)"] <-
  "District of Columbia"
cc$ps_midwest <- cc$State %in% c(enc, wnc)
cc$ps_south <- cc$State %in% c(sa, esc, wsc)
cc$ps_west <- cc$State %in% c(mt, pac)
# GfK survey
w16$ps_midwest <- w16$PPREG4 == "Midwest"
w16$ps_south <- w16$PPREG4 == "South"
w16$ps_west <- w16$PPREG4 == "West"
# CPS
cps$ps_midwest <- cps$gereg == "Midwest"
cps$ps_south <- cps$gereg == "South"
cps$ps_west <- cps$gereg == "West"


###############################
# Weights for Facebook survey
# generate respondent ID
cc$r_id <- paste0("FB-", 1:nrow(cc))
cps$r_id <- paste0("cps-", 1:nrow(cps))
cc$facebook <- 1
cps$facebook <- 0
cc$weight_ps <- 1
cps$treatment <- 1
cc$treatment <- 0
cps$weight_ps <- cps$weight
# variables in common
common_vars <- c(names(cc[grep("ps_", names(cc))]),
                 "facebook", "r_id", "weight_ps", "treatment")
# combine into one dataset 
pdat <- na.omit(rbind(cc[,common_vars], cps[,common_vars]))
# generate interaction effects
pdat$ps_male_age_18_24 <- pdat$ps_male * pdat$ps_age_18_24
pdat$ps_male_age_25_34 <- pdat$ps_male * pdat$ps_age_25_34
pdat$ps_male_age_35_44 <- pdat$ps_male * pdat$ps_age_35_44
pdat$ps_male_age_45_64 <- pdat$ps_male * pdat$ps_age_45_64
pdat$ps_metro_midwest <- pdat$ps_metro * pdat$ps_midwest
pdat$ps_metro_south <- pdat$ps_metro * pdat$ps_south
pdat$ps_metro_west <- pdat$ps_metro * pdat$ps_west

# logistic regression 
ps_md <- glm(facebook ~ .,
             data = pdat[,names(pdat)[grepl("ps_|facebook", names(pdat))]],
             family = "binomial", weights = pdat$weight_ps)
# generate predicted probability of being included in the Facebook survey
pdat$ps_md_pred <- predict(ps_md, type = "response")
# merge in the predicted probabilities 
pred_fb <- data.frame(r_id = pdat$r_id[pdat$facebook == 1], 
           ps_md_pred = pdat$ps_md_pred[pdat$facebook == 1])
cc <- merge(x = cc, y = pred_fb, all.x = TRUE, by = "r_id")
# standardize the weights
cc$md_weight <- (1/cc$ps_md_pred)/sum((1/cc$ps_md_pred))*nrow(cc)
# check the weights
summary(cc$md_weight)

##########################
# Weights for Gfk survey
# generate respondent ID
w16$r_id <- paste0("FB-", 1:nrow(w16))
cps$r_id <- paste0("cps-", 1:nrow(cps))
w16$w16 <- 1
cps$w16 <- 0
w16$weight_ps <- 1
cps$treatment <- 1
w16$treatment <- 0
cps$weight_ps <- cps$weight
# variables in common
common_vars <- c(names(w16[grep("ps_", names(w16))]),
                 "w16", "r_id", "weight_ps", "treatment")

# combine into one dataset 
pdat <- na.omit(rbind(w16[,common_vars], cps[,common_vars]))
# generate interaction effects
pdat$ps_male_age_18_24 <- pdat$ps_male * pdat$ps_age_18_24
pdat$ps_male_age_25_34 <- pdat$ps_male * pdat$ps_age_25_34
pdat$ps_male_age_35_44 <- pdat$ps_male * pdat$ps_age_35_44
pdat$ps_male_age_45_64 <- pdat$ps_male * pdat$ps_age_45_64
pdat$ps_metro_midwest <- pdat$ps_metro * pdat$ps_midwest
pdat$ps_metro_south <- pdat$ps_metro * pdat$ps_south
pdat$ps_metro_west <- pdat$ps_metro * pdat$ps_west
# logistic regression
ps_md <- glm(w16 ~ .,
             data = pdat[,names(pdat)[grepl("ps_|w16", names(pdat))]],
             family = "binomial", weights = pdat$weight_ps)
# generate the predicted probabilities 
pdat$ps_md_pred <- predict(ps_md, type = "response")
# merge in the predicted probabilities 
pred_fb <- data.frame(r_id = pdat$r_id[pdat$w16 == 1], 
                      ps_md_pred = pdat$ps_md_pred[pdat$w16 == 1])
w16 <- merge(x = w16, y = pred_fb, all.x = TRUE)
# standardize the weights
w16$md_weight <- (1/w16$ps_md_pred)/sum((1/w16$ps_md_pred), na.rm = TRUE)*nrow(w16)
w16$md_weight[is.na(w16$md_weight)] <- 1 # mean impute the missing weights
# check the weights
summary(w16$md_weight)
# correlation between the weights we generated and the ones provided by GfK
cor(w16$weight1, w16$md_weight)

#######################################
# Generate summary statistics

# political party
# Democrat
cc$ps_pid_d <- cc$x140 == "Democrat"
cc$ps_pid_d[cc$x141 == "Democratic Party"] <- TRUE
w16$ps_pid_d <- w16$X140 == "Democrat"
w16$ps_pid_d[w16$X141 == "Democratic party"] <- TRUE
# Republican
cc$ps_pid_r <- cc$x140 == "Republican"
cc$ps_pid_r[cc$x141 == "Republican Party"] <- TRUE
w16$ps_pid_r <- w16$X140 == "Republican"
w16$ps_pid_r[w16$X141 == "Republican party"] <- TRUE
# Independent 
cc$ps_pid_i <- FALSE
cc$ps_pid_i[cc$x141 == "Neither"] <- TRUE
w16$ps_pid_i <- FALSE
w16$ps_pid_i[w16$X141 == "Neither"] <- TRUE

# political ideology 
cc$ps_ideology <- as.numeric(factor(cc$x142, 
                                    levels = unique(cc$x142)[c(1,3,2,4,5)]))
w16$X142 <- as.character(w16$X142)
w16$X142[w16$X142 == "Refused"] <- "Moderate, middle of the road"
w16$X142[is.na(w16$X142)] <- "Moderate, middle of the road"
w16$ps_ideology <- as.numeric(factor(w16$X142, 
                                     levels = unique(w16$X142)[c(4,2,1,5,3)]))

# generate an indicator variable for Facebook in the GfK dataset
w16$facebook <- 0
# variables in common 
common_vars <- c(names(w16[grep("ps_", names(w16))]),
                 "md_weight", "r_id", "facebook")


# add in new variables for 65 and above and voter registeration 
cc$ps_age_65_above <- cc$Age == "65 or older"
w16$ps_age_65_above <- w16$a_age >= 65
cc$registered <- cc$x143a == "Registered"
w16$registered <- w16$X143a == "Registered"
# add these to the variables in common 
common_vars <- c(common_vars, "ps_age_65_above", "registered")

# combine to form a big dataset 
pdat <- rbind(cc[,common_vars], w16[,common_vars])
# summary Statistics: unweighted
unweighted <- pdat %>% group_by(facebook) %>% dplyr::summarise(
  N = n(),
  male = mean(ps_male),
  age_18_24 = mean(ps_age_18_24),
  age_25_34 = mean(ps_age_25_34),
  age_35_44 = mean(ps_age_35_44),
  age_45_64 = mean(ps_age_45_64),
  age_65_above = mean(ps_age_65_above),
  college = mean(ps_edu_ba),
  graduate = mean(ps_edu_gr),
  hispanic = mean(ps_r_h),
  black = mean(ps_r_b),
  white = mean(ps_r_w),
  democrat = mean(ps_pid_d),
  republican = mean(ps_pid_r),
  independent = mean(ps_pid_i),
  ideology = mean(ps_ideology),
  registered = mean(registered)
)
# summary Statistics: weighted
weighted <- pdat %>% group_by(facebook) %>% dplyr::summarise(
  N = n(),
  male = weighted.mean(ps_male, w = md_weight),
  age_18_24 = weighted.mean(ps_age_18_24, w = md_weight),
  age_25_34 = weighted.mean(ps_age_25_34, w = md_weight),
  age_35_44 = weighted.mean(ps_age_35_44, w = md_weight),
  age_45_64 = weighted.mean(ps_age_45_64, w = md_weight),
  age_65_above = weighted.mean(ps_age_65_above, w = md_weight),
  college = weighted.mean(ps_edu_ba, w = md_weight),
  graduate = weighted.mean(ps_edu_gr, w = md_weight),
  hispanic = weighted.mean(ps_r_h, w = md_weight),
  black = weighted.mean(ps_r_b, w = md_weight),
  white = weighted.mean(ps_r_w, w = md_weight),
  democrat = weighted.mean(ps_pid_d, w = md_weight),
  republican = weighted.mean(ps_pid_r, w = md_weight),
  independent = weighted.mean(ps_pid_i, w = md_weight),
  ideology = weighted.mean(ps_ideology, w = md_weight),
  registered = weighted.mean(registered, w = md_weight)
)
# prepare data for the table
dem_res <- data.frame(t(unweighted), t(weighted))
for (i in 1:ncol(dem_res)) {
  dem_res[,i] <- sprintf("%.2f", round(dem_res[,i], 2))
}
dem_res[1,] <- c("No", "No", "Yes", "Yes")
names(dem_res) <- c("GfK", "Facebook",
                    "GfK", "Facebook")
# make the table
dem_res <- data.frame(variables = c("Weighted", "N", "Prop. Male",
                        "Prop. Age 18-24", "Prop. Age 25-34",
                        "Prop. Age 35-44", "Prop. Age 45-64",
                        "Prop. Age 65 and Above", "Prop. with College Degree",
                        "Prop. with Graduate Degree", "Prop. Hispanic",
                        "Prop. Black", "Prop. White", "Prop. Democrat",
                        "Prop. Republican", "Prop. Independent", 
                        "Mean Political Ideology", "Prop. Registered to Vote"),
                      dem_res)
row.names(dem_res) <- NULL

###################################
# Revision: add in ACS Data
# load in the two datasets
load("ss16pusa_clean_merged.RData")
load("ss16pusb_clean_merged.RData")
# combine the datasets
ss16 <- rbind(ss16pusa, ss16pusb)
# remove the invidual datasets
rm("ss16pusa")
rm("ss16pusb")
# standardize the weights
ss16$weights <- as.numeric(ss16$PWGTP)/sum(as.numeric(ss16$PWGTP))*nrow(ss16)
# get the proportions
acs_function <- function(my_formula, my_weights = NULL) {
  if (!is.null(weights)) {
    lm(as.formula(my_formula), data = ss16, weights = my_weights)$coefficients
  } else{
    lm(as.formula(my_formula), data = ss16)$coefficients
  }
}
shorten_vars <- c("Weighted", "N", "Prop. Male",
                  "Prop. Age 18-24", "Prop. Age 25-34",
                  "Prop. Age 35-44", "Prop. Age 45-64",
                  "Prop. Age 65 and Above", "Prop. with College Degree",
                  "Prop. with Graduate Degree", "Prop. Hispanic",
                  "Prop. Black", "Prop. White")
formula_dem <- c("SEX == 1 ~ 1", "AGEP %in% 18:24 ~ 1", "AGEP %in% 25:34 ~ 1",
                 "AGEP %in% 35:44 ~ 1", "AGEP %in% 45:64 ~ 1", "AGEP >= 65 ~ 1",
                 "SCHL == 21 ~ 1", "SCHL %in% 22:24 ~ 1", "HISP != '01' ~ 1",
                 "RAC1P == 2 ~ 1", "RAC1P == 1 ~ 1")
ss16_w <- do.call(rbind, lapply(formula_dem, FUN = acs_function, 
                                my_weights = ss16$weights))
ss16_w <- data.frame(variables = shorten_vars,
           ACS.w = c("Yes", nrow(ss16),
                     sprintf("%.2f", round(ss16_w, 2))))
ss16_u <- do.call(rbind, lapply(formula_dem, FUN = acs_function))
ss16_u <- data.frame(variables = shorten_vars,
                     ACS.u = c("No", nrow(ss16),
                               sprintf("%.2f", round(ss16_u, 2))))
# combine the unweighted and weighted summary statistics
ACS_res <- merge(ss16_u, ss16_w)
ACS_res$variables <- factor(ACS_res$variables, levels = shorten_vars)
ACS_res <- ACS_res[order(ACS_res$variables),]
# merge into the summary statistics table
full_dem <- merge(x = dem_res, y = ACS_res, all.x = TRUE)
# clean up the summary statistics table
full_dem$variables <- factor(full_dem$variables, levels = dem_res$variables)
full_dem <- full_dem[order(full_dem$variables), c(1, 2, 3, 6, 4, 5, 7)]
# output the result 
# Table 1
stargazer(full_dem, summary = FALSE, rownames = FALSE)

#####################################################
# Climate change public opinion questions 
compare_func <- function(fb_var, w16_var, value, compare_name, 
                         weight_type = "md_weight", output_long = TRUE) {
  u_cc <- mean(cc[,fb_var] %in% value, na.rm = TRUE)
  u_cc_se <- rb_se(var = cc[,fb_var] %in% value)
  u_w16 <- mean(w16[,w16_var] %in% value, na.rm = TRUE)
  u_w16_se <- rb_se(var = w16[,w16_var] %in% value)
  w_cc <- weighted.mean(cc[,fb_var] %in% value, w = cc[,weight_type], na.rm = TRUE)
  w_cc_se <- rb_se(var = cc[,fb_var] %in% value, w = cc[,weight_type])
  w_w16 <- weighted.mean(w16[,w16_var] %in% value, w = w16[,weight_type], na.rm = TRUE)
  w_w16_se <- rb_se(var = w16[,w16_var] %in% value, w = w16[,weight_type])
  w_w16_b <- weighted.mean(w16[,w16_var] %in% value, w = w16$weight1)
  w_w16_se_b <- rb_se(var = w16[,w16_var] %in% value, w = w16$weight1)
  wide_res <- data.frame(compare_name, u_cc, u_w16, 
                         u_cc_se, u_w16_se,
                         diff_u = u_cc - u_w16,
                         w_cc, w_w16, w_cc_se, w_w16_se, diff_w = w_cc - w_w16)
  long_res <- rbind(data.frame(compare_name, type = "Unweighted", 
                               survey = c("Facebook", "GfK"),
                               mean = c(u_cc, u_w16),
                               se = c(u_cc_se, u_w16_se)),
                    data.frame(compare_name, type = "Weighted", 
                               survey = c("Facebook", "GfK", 
                                          "Gfk (with Gfk Weights)"),
                               mean = c(w_cc, w_w16, w_w16_b),
                               se = c(w_cc_se, w_w16_se, w_w16_se_b)))
  if (output_long) {
    long_res
  } else {
    wide_res
  }
}
# fix wording in question x73
cc$x73 <- ifelse(cc$x73a == "", cc$x73b, cc$x73a)
cc$x73[cc$x73 == "Most scientists think global warming is <u>not</u> happening"] <-
  "Most scientists think global warming is not happening"
# residence question
cc$rent <- cc$ACS_Residence == "I rent my current residence"
w16$rent <- w16$PPRENT == "Rented for cash"
cc$owned <- cc$ACS_Residence %in% c("I or someone in my household own my current residence free and clear",
                                    "I or someone in my household own my current residence with a mortgage or a loan")
w16$owned <- w16$PPRENT == "Owned or being bought by you or someone in your household"
# generate the summary statistics
compare_res <- rbind(
  compare_func(fb_var = "x65", w16_var = "X65", value = "Yes", 
               compare_name = "Global warming is happening"),
  compare_func("x67", "X67", "Caused mostly by human activities", 
               "Global warming is\nmostly human-caused"),
  compare_func("x73", "X73", "Most scientists think global warming is happening", 
               "Most scientists think global\nwarming is happening"),
  compare_func("x78", "X78", c("Somewhat worried", "Very worried"), 
               "Worried about global warming"),
  compare_func("x79", "X79", c("A great deal", "A moderate amount"), 
               "Global warming will harm\nme personally"),
  compare_func("X85", "X85", c("A great deal", "A moderate amount"), 
               "Global warming will harm\nfuture generations"),
  compare_func("x86", "X86", c("A great deal", "A moderate amount"), 
               "Global warming will harm\nplants and animals"),
  compare_func("x137", "X137", c("Somewhat support", "Strongly support"), 
               "Support funding research into\nrenewable energy sources")
)

# make the plot 
# Figure 1
compare_res$compare_name <- factor(compare_res$compare_name, 
                                   levels = rev(unique(compare_res$compare_name)))
ggplot(compare_res[compare_res$survey %in% c("Facebook", "GfK"),], 
       aes(x = compare_name, y = mean, 
                        ymin = qnorm(0.025)*se + mean,
                        ymax = qnorm(0.975)*se + mean, color = survey)) + 
  geom_linerange(position = position_dodge(width = 1)) + coord_flip() +
  geom_point(position = position_dodge(width = 1)) +
  facet_wrap(~type, ncol = 1) + theme_bw() + #ylim(0, 1) + 
  scale_color_manual(values = c("darkorange", "blue", "darkgreen"), 
                     name = "Survey Type") +
  theme(legend.position="bottom") + ylab("Proportion of Respondents Who Agreed") +
  xlab("Survey Question Statements")
ggsave("figure_1.pdf", 
       width = 7, height = 7, dpi = 300)

# compare differences

# unweighted
u_diff <- reshape(compare_res[compare_res$type == "Unweighted",], 
                  timevar = c("survey"),
                  idvar = c("compare_name"),
                  direction = "wide")
table_a2 <- data.frame(survey_item = gsub(pattern = "\n", replacement = " ", u_diff$compare_name),
                       fb = mean_se(u_diff$mean.Facebook, u_diff$se.Facebook),
                       gfk = mean_se(u_diff$mean.GfK, u_diff$se.GfK))
# Table A2
stargazer(table_a2, summary = FALSE, rownames = FALSE)

# weighted
w_diff <- reshape(compare_res[compare_res$type == "Weighted",], 
        timevar = c("survey"),
        idvar = c("compare_name"),
        direction = "wide")
table_a3 <- data.frame(survey_item = gsub(pattern = "\n", replacement = " ", w_diff$compare_name),
                       fb_ipw = mean_se(w_diff$mean.Facebook, w_diff$se.Facebook),
                       gfk_ipw = mean_se(w_diff$mean.GfK, w_diff$se.GfK),
                       gfk_gfkw = mean_se(w_diff$`mean.Gfk (with Gfk Weights)`,
                                          w_diff$`se.Gfk (with Gfk Weights)`))
# Table A3
stargazer(table_a3, summary = FALSE, rownames = FALSE)

# mean of differences
round(mean(u_diff$mean.Facebook - u_diff$mean.GfK), 3)
round(mean(w_diff$mean.Facebook - w_diff$mean.GfK), 3)
# sd of differences
round(sd(u_diff$mean.Facebook - u_diff$mean.GfK), 3)
round(sd(w_diff$mean.Facebook - w_diff$mean.GfK), 3)

#####################################
# Compare ACS Demographics
# veteran
w_vet <- weighted.mean(cc$ACS_Mil == "I was on active duty in the past, but not now", w = cc$md_weight)
w_vet_se <- rb_se(var_name = cc$ACS_Mil == "I was on active duty in the past, but not now",
                  w = cc$md_weight)
u_vet <- weighted.mean(cc$ACS_Mil == "I was on active duty in the past, but not now")
u_vet_se <- rb_se(var_name = cc$ACS_Mil == "I was on active duty in the past, but not now")
# born in the US
w_us <- weighted.mean(cc$ACS_US == "Yes, I was born in the U.S.", w = cc$md_weight)
w_us_se <- rb_se(cc$ACS_US == "Yes, I was born in the U.S.", w = cc$md_weight)
u_us <- weighted.mean(cc$ACS_US == "Yes, I was born in the U.S.")
u_us_se <- rb_se(cc$ACS_US == "Yes, I was born in the U.S.")
# own one's residence 
w_own <- weighted.mean(cc$ACS_Residence %in% unique(cc$ACS_Residence)[c(1,3)], w = cc$md_weight)
w_own_se <- rb_se(cc$ACS_Residence %in% unique(cc$ACS_Residence)[c(1,3)], w = cc$md_weight)
u_own <- weighted.mean(cc$ACS_Residence %in% unique(cc$ACS_Residence)[c(1,3)])
u_own_se <- rb_se(cc$ACS_Residence %in% unique(cc$ACS_Residence)[c(1,3)])

# Combine 
compare_acs <- data.frame(question = c(rep("Veteran", 2), 
                                       rep("Born in the US", 2), rep("Own Residence", 2)),
                          type = rep(c("Unweighted", "Weighted"), 3), 
                          mean = c(u_vet, w_vet, u_us, w_us, u_own, w_own),
                          se = c(u_vet_se, w_vet_se, u_us_se, w_us_se, u_own_se, w_own_se))
# Revision: add in the ACS
# fix the tenure variable
ss16$TEN_v <- ss16$TEN
ss16$TEN_v[is.na(ss16$TEN)] <- 5
mean(ss16$TEN_v %in% 1:2)
acs2016_3 <- c(lm(MIL == 2 ~ 1, data = ss16)$coefficients,
  lm(MIL == 2 ~ 1, data = ss16, weights = ss16$weights)$coefficients,
  lm(CIT == 1 ~ 1, data = ss16)$coefficients,
  lm(CIT == 1 ~ 1, data = ss16, weights = ss16$weights)$coefficients,
  lm(TEN_v %in% 1:2 ~ 1, data = ss16)$coefficients,
  lm(TEN_v %in% 1:2 ~ 1, data = ss16, weights = ss16$weights)$coefficients)
compare_acs$acs_2016 <- acs2016_3
# Figure A3
ggplot(compare_acs, aes(x = question, y = mean, ymin = qnorm(0.025)*se + mean,
                        ymax = qnorm(0.975)*se + mean)) +
  geom_point(size = 1.5) + geom_linerange() + ylim(0, 1) +
  geom_point(mapping = aes(x = question, y = acs_2016), color = "red", shape = 3) +
  facet_wrap(~type, ncol = 1) + coord_flip() + theme_bw() + xlab("ACS Outcomes") + 
  ylab("Proportion of Respondents")
ggsave("figure_a3.pdf", width = 7, height = 3, dpi = 300)
# Table A4
table_a4 <- data.frame(survey_items = unique(compare_acs$question),
           facebook_u = mean_se(compare_acs$mean[compare_acs$type == "Unweighted"],
                                compare_acs$se[compare_acs$type == "Unweighted"]),
           facebook_w = mean_se(compare_acs$mean[compare_acs$type == "Weighted"],
                                compare_acs$se[compare_acs$type == "Weighted"]),
           acs_u = round3(compare_acs$acs_2016[compare_acs$type == "Unweighted"]),
           acs_w = round3(compare_acs$acs_2016[compare_acs$type == "Weighted"]))
table_a4 <- table_a4[c(1, 3, 2),]
stargazer(table_a4, summary = FALSE, rownames = FALSE)

######################################
# Analyze Facebook spending data

# load in the data
fbs <- read.csv("facebook_ads_data.csv", 
                stringsAsFactors = FALSE)
# remove the ads that had no money spent on it
fbs <- fbs[fbs$Amount.Spent..USD. > 0,]
# calculate cost per complete
sum(fbs$Amount.Spent..USD.)/nrow(cc)
# summary statistics: link clicks per ad
summary(fbs$Link.Clicks, na.rm = TRUE)
# make histrogram of cost per link click
names(fbs)[names(fbs) == "CPC..Cost.per.Link.Click...USD."] <- "cost_per_click"
# summary statistics: cost per link click
summary(fbs$cost_per_click)
sd(fbs$cost_per_click)
# total number of link clicks
sum(fbs$Link.Clicks, na.rm = TRUE)
# Figure A1
ggplot(fbs, aes(x = cost_per_click)) + geom_histogram(binwidth=1) +
  theme_bw() + xlab("Cost per Link Click (in USD)") + ylab("Count")
ggsave("figure_a1.pdf", dpi = 300, width = 4, height = 3)

######################################
# Load GfK survey: Fall 2017 Climate Change in the American Mind

load("gfk_2017.RData")

# Summary statistics on Page A4
data.frame(Variables = c("Own their residence", "Own their residence",
                        "Veterans", "Veterans",
                        "U.S.-born", "U.S.-born"),
           Weighted = rep(c("No", "Yes"), 3),
  Proportion = round(c(lm(PPRENT == "Owned or being bought by you or someone in your household" ~ 1, data = w17)$coefficient,
lm(PPRENT == "Owned or being bought by you or someone in your household" ~ 1, data = w17, weights =  w17$weight)$coefficient,
lm(X1475 == "I was on active duty in the past, but not now" ~ 1, data = w17)$coefficient,
lm(X1475 == "I was on active duty in the past, but not now" ~ 1, data = w17, weights =  w17$weight)$coefficient,
lm(X1476 == "Yes, I was born in the U.S." ~ 1, data = w17)$coefficient,
lm(X1476 == "Yes, I was born in the U.S." ~ 1, data = w17, weights =  w17$weight)$coefficient), 2))


print(paste0("End date and time: ", lubridate::now(tzone = "GMT")))