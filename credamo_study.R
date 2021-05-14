######################################################
# C R E D A M O   S T U D Y
######################################################

# D A T A
######################################################
d_china = read.csv("./data/credamo_data_all.csv")

# remove people who did not pass the attention check 
# (correct answer is 5)
d_china = d_china[d_china[,"attention_check"] == 5,]


#  RECIPROCITY  AND  COOPERATION  WITH  STRANGERS
######################################################
m1 = glm(cooperation ~ treatment + city + population +  age + own_company + gender +
           education + income + employment_status + hukou, data = d_china, family = "binomial")
summary(m1)


#  B Y   C I T Y
######################################################
# without controls
m2 = glm(cooperation ~ treatment*city , data = d_china, family = "binomial")
summary(m2)

# with controls
m3 = glm(cooperation ~ treatment*city + population +  age + own_company + gender +
           education + income + employment_status + hukou, data = d_china, family = "binomial")
summary(m3)


#  N O N - M A N A G E R S   A N D   M A N A G E R S
######################################################
m4 = glm(cooperation ~ treatment*city +  age + own_company + gender +
           education + income + employment_status + hukou,
         data = d_china[d_china$population=="non-managers",], family = "binomial")
summary(m4)

m5 = glm(cooperation ~ treatment*city + own_company +  age  + gender +
           education + income + employment_status + hukou,
         data = d_china[d_china$population=="managers",], family = "binomial")
summary(m5)


models = list(m1, m2, m3, m4, m5)

# create latex table (TABLE 6)
stargazer(models,
          title = paste("Reciprocity Business Norms and Prisoner's Dilemma"),
          header = FALSE, label = "table5",
          # align=TRUE,
          column.labels = c("M1", "M2", "M3", "M4", "M5"),
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
          out = "table_6.html")




#  S I M U L A T I O N S  (F I G U R E   3)
######################################################
model_id = 5

# EXTRACT  COEFFICIENTS AND VAR-COVAR MATRIX
betas = coef(models[[model_id]])    # coefs
var.cov = vcov(models[[model_id]])  # variance-covariance matrix

M = 1000; m = 1000

# this is func g(.) in King, Tomz, & Wittenberg (2000)
inv.logit = function(x) 1 / (1+exp(-x))

# sample from multivariate Normal distribution: M random draws
betas.sim = mvrnorm(M, betas, var.cov)
  
# get values at their means (for the average person in the sample)
Xc = effect("treatment*city", models[[model_id]])$model.matrix

# set Xc matrix to values for the average person in Nanjing
nanjing = d_china[d_china$city=="Nanjing" & d_china$population=="managers", ]

# own_company
own_company = prop.table(table(nanjing[, "own_company"]))
own_company = own_company[2:length(own_company)]
# age
age = mean(nanjing[, "age"])
# male
male = prop.table(table(nanjing[, "gender"]))
male = male[2:length(male)]
# educ
educ = prop.table(table(nanjing[, "education"]))
educ = educ[2:length(educ)]
# income (add 0 percent for 'less than 30k')
income = prop.table(table(nanjing[, "income"]))
income = income[2:length(income)]
income = c(income[1:2], 0, income[3])
# employment
employment = prop.table(table(nanjing[, "employment_status"]))
employment = employment[2:length(employment)]
# hukou
hukou = prop.table(table(nanjing[, "hukou"]))
hukou = hukou[2:length(hukou)]
# add fixed values for Nanjing's average person to Xc matrix
Xc[,7:21] = sapply(c(own_company,age,male,educ,income,employment,hukou), function(x) rep(x, nrow(Xc)))

# multiply betas by predictors
xb = t(apply(betas.sim, 1, function(x) Xc %*% x) )

# obtain probabilities
prob.sim = inv.logit(xb)

# use these probabilities to reduce fundamental uncertainty by drawing from a Bernoulli distribution.
binom_draws = apply(prob.sim,2, function(prob_vec) sapply(prob_vec, function(x) mean(rbinom(m, size=1, prob=x))) )

# Get 95% confidence intervals
prob_ci95 = apply(binom_draws, 2, function(col) quantile(col, probs=c(0.0275, 0.975)))
prob_ci95 = rbind(prob_ci95, mean=colMeans(binom_draws))


# FIGURE 3
############
png("./figure_3.png",units="in",width=6,height=4.2,res=150)

op = par(mfrow = c(1,1), oma=c(0,2,0,0), mar = c(3,3,2,1) + 0.1)

city = "cityNanjing"

# exctract values for Nanjing
ids_city = which(Xc[,city] == 1)
vals = t(prob_ci95[,ids_city])

# fix x-axis  
ids_x = c(0.2,0.8)

# empty plot
plot(c(-0.1,1.1),c(0,1), xaxt="n",yaxt="n",bty='n',pch='',ylab = "", xlab = "")

# add horizontal line
abline(h=0.5, lty=2, col="gray80")
  
# values + 95% CIs 
points(ids_x, vals[,"mean"], type="p", pch=19, cex=1.2)
segments(ids_x,vals[,"2.75%"],ids_x,vals[,"97.5%"], lwd=1.5)
  
box()
  
# x-axis
axis(1,at=ids_x, labels=,c("No Cooperation Norm", "Reciprocity Norm"),
     tick=FALSE,line=-0.5,cex.axis=1,lwd.ticks=0.5)
  
# y-axis
axis(2, at=seq(0,1,0.2), las=1,line=0,cex.axis=0.8)
title(ylab="P(Cooperation)",outer = TRUE, line = 0,cex.lab=1.1)

# add title
city_label = strsplit(city, "city")[[1]][2]
mtext(city_label, side=3,line=0.5,cex=1.2,adj=0,font=2, at=0.45)

par(op)
dev.off()



### R O B U S T N E S S   C H E C K
#######################################

# In our main results, we used all data available except for those 
# observations that did not pass the attetion check at the end of the 
# survey.
#
# In addition to this check, we also asked respondents two questions 
# to confirm that they understood the Prisoner's dilemma. Below,
# we present results using these two questions to exclude participants
# who may have not understood the Prisoner's dilemma. The correct 
# answer for the first example was 16.5 Yuans, while the correct answer
# for the second example was 6.6 Yuans.
#
# Before conducting the analysis, we correct some answers that we believe 
# are right but appear as mistaken due to confusing phrasing in the way that 
# participants were told to enter their answers. The question said that 
# participants should NOT include words or punctuation, which respondents
# interpreted differently when entering their answers. For instace, in the 
# example 1, some participants entered "165", thus omiting the punctuation
# in the correct answer (= 16.5). Others entered only 16, thus omitting 
# ".6" as part of the answer. In 3 cases, participants entered the wrong 
# decimal (16.3 and 16.6), which we assumed was a mistake that did not show
# a lack of understanding from participants.

# Example 1. The correct answer is 16.5 and we assume that 
# all answers that include the number 16 are correct.
mode(d_china[,"PD_test_1"]) = "character"
d_china[,"PD_test_1"] = ifelse( grepl("16",d_china[,"PD_test_1"]), 
                                "16.5", 
                                d_china[,"PD_test_1"])

# Example 2. The correct answer is 6.6 and we assume that 
# all answers that include the number 6 are correct.
mode(d_china[,"PD_test_2"]) = "character"
d_china[,"PD_test_2"] = ifelse( grepl("6",d_china[,"PD_test_2"]), 
                                "6.6", 
                                d_china[,"PD_test_2"])

# EXCLUSION CRITERIA 
#
# We cannot be entirely sure if participants who did not respond
# correctly to the toy examples in Prisoner's Dilemma did not understand
# the game. The following options are possible:
#
#   1. Some participants understood the PD but rushed through the 
#       examples and did not answer correctly the tests (1 and/or 2).
#       This results in no data exclusion. (The main findings in 
#       the article follow this generous interpretation).
# 
#   2. Some participants understood the PD and answered correctly
#       only one of the tests (1 or 2) -- while the other test was 
#       incorrect. Hence, participants who answered at least 1 test 
#       correctly should be included in the sample.
# 
#   3. Participants who did not answer correctly both tests did not
#      understand the PD at all. Hence, participants with no correct
#      answers should be excluded.

# EXCLUSION CRITERION # 2
crit_2 = d_china[,"PD_test_1"]=="16.5" | d_china[,"PD_test_2"]=="6.6"

# EXCLUSION CRITERION # 3
crit_3 = d_china[,"PD_test_1"]=="16.5" & d_china[,"PD_test_2"]=="6.6"



# A N A L Y S I S (C R I T E R I O N   # 2)
##############################################
m1 = glm(cooperation ~ treatment + city + population +  age + own_company + gender +
           education + income + employment_status + hukou, data = d_china[crit_2,], family = "binomial")
summary(m1)


# BY CITY
###########
# without controls
m2 = glm(cooperation ~ treatment*city , data = d_china[crit_2,], family = "binomial")
summary(m2)

# with controls
m3 = glm(cooperation ~ treatment*city + population +  age + own_company + gender +
           education + income + employment_status + hukou, data = d_china[crit_2,], family = "binomial")
summary(m3)


# MANAGERS AND NON-MANAGERS
#############################
m4 = glm(cooperation ~ treatment*city +  age + own_company + gender +
           education + income + employment_status + hukou,
         data = d_china[d_china$population=="non-managers" & crit_2,], family = "binomial")
summary(m4)

m5 = glm(cooperation ~ treatment*city + own_company +  age  + gender +
           education + income + employment_status + hukou,
         data = d_china[d_china$population=="managers" & crit_2,], family = "binomial")
summary(m5)


# A N A L Y S I S (C R I T E R I O N   # 3)
##############################################
m1 = glm(cooperation ~ treatment + city + population +  age + own_company + gender +
           education + income + employment_status + hukou, data = d_china[crit_3,], family = "binomial")
summary(m1)


# BY CITY
###########
# without controls
m2 = glm(cooperation ~ treatment*city , data = d_china[crit_3,], family = "binomial")
summary(m2)

# with controls
m3 = glm(cooperation ~ treatment*city + population +  age + own_company + gender +
           education + income + employment_status + hukou, data = d_china[crit_3,], family = "binomial")
summary(m3)


# MANAGERS AND NON-MANAGERS
#############################
m4 = glm(cooperation ~ treatment*city +  age + own_company + gender +
           education + income + employment_status + hukou,
         data = d_china[d_china$population=="non-managers" & crit_3,], family = "binomial")
summary(m4)

m5 = glm(cooperation ~ treatment*city + own_company +  age  + gender +
           education + income + employment_status + hukou,
         data = d_china[d_china$population=="managers" & crit_3,], family = "binomial")
summary(m5)
