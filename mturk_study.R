### READ DATA
d_usa = read.csv("./data/mturk_data_all.csv")

### RELEVEL VARIABLES AND CONVERT FROM STRING TO NUMERIC

# PD variable. Choice A is Cooperation
d_usa[,"PD"] = ifelse(d_usa[,"PD_choice"]=="Choice A", 1, 0)

# Relevel treatments
d_usa$treatment = as.factor(d_usa$treatment)
d_usa$treatment = relevel(d_usa$treatment, ref="N")

# Binary treatment: some norms versus no norms
d_usa$treatment_binary = ifelse(d_usa$treatment=="N", "No norm", "Some norm")
d_usa$treatment_binary = relevel(as.factor(d_usa$treatment_binary), ref="No norm")

# Validation Check
# Retribution
d_usa[,"retribution"] = ifelse(d_usa[,"retribution_check"]=="Strongly agree", 5,
                               ifelse(d_usa[,"retribution_check"]=="Somewhat agree", 4,
                                      ifelse(d_usa[,"retribution_check"]=="Neither agree nor disagree", 3,
                                             ifelse(d_usa[,"retribution_check"]=="Somewhat disagree", 2, 1))))

# Retribution
d_usa[,"community"] = ifelse(d_usa[,"community_check"]=="Strongly agree", 5,
                             ifelse(d_usa[,"community_check"]=="Somewhat agree", 4,
                                    ifelse(d_usa[,"community_check"]=="Neither agree nor disagree", 3,
                                           ifelse(d_usa[,"community_check"]=="Somewhat disagree", 2, 1))))

# Covariates
# educ
d_usa$educ = as.factor(as.character(d_usa$educ))

# income
d_usa$inc_family = as.factor(d_usa$inc_family)
levels(d_usa$inc_family)[levels(d_usa$inc_family)==""] = NA
d_usa$inc_family = relevel(d_usa$inc_family, ref="Greater than $75,000")

# gender
d_usa$gender = as.factor(as.character(d_usa$gender))
d_usa$gender[d_usa$gender == "Other"] = NA
d_usa$gender = relevel(d_usa$gender, ref="Male")
levels(d_usa$gender)[levels(d_usa$gender)==""] = NA

# political orientation
d_usa$pol_1 = as.numeric(as.character(d_usa$pol_1))

# race (factor)
d_usa$race = as.factor(as.character(d_usa$race))
levels(d_usa$race)[levels(d_usa$race)==""] = NA
d_usa$race = relevel(d_usa$race, ref="White")

# age (numeric)
d_usa$year_birth_1 = as.numeric(as.character(d_usa$year_birth_1))
d_usa$age = 2020 - d_usa$year_birth_1

# religion (facto)r
d_usa$religion = as.factor(as.character(d_usa$religion))
d_usa$religion[d_usa$religion == "Jewish"] = "Other"
d_usa$religion = relevel(d_usa$religion, ref="None")


### MODELS

# model with all treatments
m1 = glm(PD ~ treatment, data=d_usa, family=binomial(link="logit"))
summary(m1)

m1.cov = glm(PD ~ treatment + educ+ inc_family + gender + pol_1 + race + age + religion, data=d_usa, family = "binomial")
summary(m1.cov)


models = list(m1, m1.cov)

# create latex table
stargazer(models,
          title = paste("Cooperation Norms and Prisoner's Dilemma"),
          header = FALSE, label = "table1_appendix",
          # align=TRUE,
          column.labels = c("M1", "M2"),
          model.numbers = FALSE,
          dep.var.caption = "",
          dep.var.labels = "Cooperation",
          # covariate.labels = labels.main,
          # omit  = c( "gender1","income","rural1","educ","age",
          #            "ind_sector2","ind_sector3","ind_sector4", "ind_sector5",
          #            "nothing_all",
          #            "cityHangzhou", "cityNanjing", "cityShanghai", "cityWenzhou" ),
          # omit = rownames(summary(models[[4]])$coef)[-c(1,2)],
          order = colnames(norm), #, "cityHangzhou", "cityNanjing", "cityShanghai", "cityWenzhou"),
          # add.lines = list(c("Controls",rep(c("No", "Yes"),5))),
          omit.stat=c("LL","ser","f"), no.space=TRUE, type = "text",
          font.size = "footnotesize", digits = 2,
          column.sep.width = "-8pt", notes.label = "",
          single.row = TRUE,
          out = "tableD1.html")




# Validation Checks
# retribution
m2 = lm(retribution ~ treatment, data=d_usa)
summary(m2)

# community
m3 = lm(community ~ treatment, data=d_usa)
summary(m3)
