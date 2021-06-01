
# read data
d = read.csv("./data/yangzi-delta_data_all.csv")


#############################################################

###  P R I S O N E R' S   D I L E M M A
d[,'pd'] = ifelse(d$pt_w3=='a' | d$pa_w3=='a', 1, 0)


### N O R M   E N F O R C E M E N T   (2 0 0 9)

# ALL norms
all.norms = list(
  c('a20a_w2', 'a21a_w2', 'b11a_w2', 'd12a_w2', 'd13a_w2', 'd14a_w2', 'e17a_w2'),
  c('a20b_w2', 'a21b_w2', 'b11b_w2', 'd12b_w2', 'd13b_w2', 'd14b_w2', 'e17b_w2'),
  c('a20c_w2', 'a21c_w2', 'b11c_w2', 'd12c_w2', 'd13c_w2',     NA   , 'e17c_w2'),
  c('a20d_w2', 'a21d_w2', 'b11d_w2', 'd12d_w2', 'd13d_w2', 'd14c_w2', 'e17d_w2'),
  c('a20e_w2', 'a21e_w2', 'b11e_w2', 'd12e_w2', 'd13e_w2', 'd14d_w2', 'e17e_w2') 
)

m_all = matrix(NA, nrow(d), 5)
colnames(m_all) = c("nothing", 
                    "gossip", 
                    "tit_tat", 
                    "punishment", 
                    "community")

# create independent variable by summing across columns 
# and diving by length of vector
for(set in 1:length(all.norms)){
  # select variables  
  vars = all.norms[[set]][!is.na(all.norms[[set]])]
  # create vector summing values "yes" (==1)
  vec = rowMeans(d[, vars] == 1)
  # add
  m_all[,set] = vec 
}

# add columns
d = cbind(d, m_all)


# STRONG norms
norms.strong = list(
  c('b11a_w2', 'd12a_w2', 'd13a_w2', 'd14a_w2', 'e17a_w2'),
  c('b11b_w2', 'd12b_w2', 'd13b_w2', 'd14b_w2', 'e17b_w2'),
  c('b11c_w2', 'd12c_w2', 'd13c_w2',            'e17c_w2'),
  c('b11d_w2', 'd12d_w2', 'd13d_w2', 'd14c_w2', 'e17d_w2'),
  c('b11e_w2', 'd12e_w2', 'd13e_w2', 'd14d_w2', 'e17e_w2')
)

# empty matrix for strong norms
m_strong = matrix(NA, nrow(d), 5)
strong.names = c("s_nothing", 
                 "s_gossip", 
                 "s_tit_tat", 
                 "s_punishment", 
                 "s_community")
colnames(m_strong) = strong.names


# create independent variables by summing across columns 
# and diving by length of vector
for(set in 1:length(norms.strong)){
  
  # select variables
  vars = norms.strong[[set]]
  
  # create vector summing values "yes" (==1)
  vec = rowMeans(d[, vars] == 1)
  
  # add
  m_strong[,set] = vec
}

# add columns
d = cbind(d, m_strong)


###  C O N T R O L S
# names of controls
controls = c( "gender",
              "income",
              "rural",
              "educ",
              "age",
              "age_2",
              "city",
              "ind_sector",
              "no_norm")

# Income
d[,"income"] = d[,'k7_w3']*1000

# Gender
d[, "gender"] = as.factor(d$a1_w3)

# Age
d[,"age"] = d$age_w3

# Age^2
d[,"age_2"] = d[,"age"]^2

# Rural
d[,"rural"] = as.factor(d$a4_w3)

# Education (years)
d[,"educ"] = d$a6a_w3

# City
d[,"city"] = relevel( factor(d[,"city_w3"]), ref="Changzhou" )


# Industrial sector
d[,"ind_sector"] = ifelse(d[,"sector1_w3"]==1, 1, 
                          ifelse(d[,"sector2_w3"]==1, 2, 
                            ifelse(d[,"sector3_w3"]==1, 3, 
                              ifelse(d[,"sector4_w3"]==1, 4, 5))))

d[,"ind_sector"] = as.factor(d[,"ind_sector"])

# Indicator for CEOs that mention nothing happens across all norms
d[,"no_norm"] = ifelse(d$nothing==1, 1, 0)



#  R U N    M O D E L S
########################################

# Define labels for x-axis in plots
labels.main = c("Nothing happens", 
                "Negative gossip", 
                "Retaliation", 
                "Punishment", 
                "Community sanction")

labels.controls = c( "Gender",
                     "Income",
                     "Rural",
                     "Education (years)",
                     "Age",
                     "Medicine sector",
                     "Mechanical sector",
                     "Transportation sector",
                     "Electronic sector",
                     "Nothing happens (all)") 


#   A L L   N O R M S
#########################
models_all = list()
for( j in 1:length(all.norms) ){ 
  
  # estimate 2 models per norm enforcement: with and without control
  for(i in 1:2){
    
    # select variables
    all.var = c(colnames(m_all)[j])
    if(j!=1 & i==2) all.var = c(all.var, controls)
    
    # remove no-norm variable (when needed)
    if(j==1 & i==2) all.var = c(all.var, controls[-1*grep("norm", controls)])  
    
    # create formula
    f = as.formula( paste("pd ~ ", paste(all.var, collapse = "+")  ) )
    
    # estimate simple logistic regression
    m = glm(f , family=binomial(link="logit"), data = d)
    summary(m)
    
    # save models
    save = j*2; if(i==1) save = save - 1
    # store models 
    models_all[[save]] = m
  }
}

# create latex table
stargazer(models_all,
            title = paste("Prisoner's Dilemma and Norm Enforcement"),
            header = FALSE, label = "table3",
            column.labels = c("M1$a$", "M1$b$", 
                              "M2$a$", "M2$b$", 
                              "M3$a$", "M3$b$", 
                              "M4$a$", "M4$b$", 
                              "M5$a$", "M5$b$"),
            model.numbers = FALSE,
            dep.var.caption = "",
            dep.var.labels = "Cooperation",
            order = c(colnames(m_all), rownames(summary(models_all[[4]])$coef)[-c(1,2)]),
            omit.stat=c("LL","ser","f"), no.space=TRUE, type = "text",
            font.size = "footnotesize", digits = 2,
            column.sep.width = "-8pt", notes.label = "",
            out = "./results/table_3.html" )


#   S T R O N G   N O R M S
#########################
models_strong = list()
for( j in 1:length(norms.strong) ){ 
  
  # estimate 2 models per norm enforcement: with and without controls
  for(i in 1:2){
    # select variables
    all.var = strong.names[j]
    if(j!=1 & i==2) all.var = c(all.var, controls)
    
    # remove no-norm variable (when needed)
    if(j==1 & i==2) all.var = c(all.var, controls[-1*grep("norm", controls)]) 
    
    # formula
    f = as.formula( paste("pd ~ ", paste(all.var, collapse = "+")  ) )
    
    # estimate logistic regression
    m = glm(f , family=binomial(link="logit"), data = d)
    
    # save models
    save = j*2; if(i==1) save = save - 1
    
    # store models
    models_strong[[save]] = m
  }
}

# create latex table
stargazer(models_strong,
            title = paste("Prisoner's Dilemma and Strong Norms"),
            header = FALSE, label = "table4",
            column.labels = c("M1$a$", "M1$b$", 
                              "M2$a$", "M2$b$", 
                              "M3$a$", "M3$b$", 
                              "M4$a$", "M4$b$", 
                              "M5$a$", "M5$b$"),
            model.numbers = FALSE,
            dep.var.caption = "",
            dep.var.labels = "Cooperation",
            order = c(colnames(m_strong), rownames(summary(models_strong[[4]])$coef)[-c(1,2)]),
            omit.stat=c("LL","ser","f"), no.space=TRUE, type = "text",
            font.size = "footnotesize", digits = 2,
            column.sep.width = "-8pt", notes.label = "",
            out = "./results/table_4.html")


################################################################################


### F I G U R E   1
###############################################

# Simulate confidence intervals

# VARIABLES (include 1 vector for intercept)
X  = cbind(1,d[,c("s_tit_tat",controls)])

# city levels
lev.city = levels(X$city)
for(i in 1:length(lev.city)){
  X[,paste0('city', lev.city[i])] = ifelse(X$city == lev.city[i], 1, 0)
}

# ind sector levels
lev.ind = levels(X$ind_sector)
for(i in 1:length(lev.ind)){
  X[,paste0('ind_sector',i)] = ifelse(X$ind_sector == lev.ind[i], 1, 0)
}

# exclude city and ind_sector variables (level indicators already exist)
X = X[,!(names(X) %in% c('city','ind_sector'))]

# exclude city and ind_sector that are reference categories in 
# the logistic regression
X = X[,!(names(X) %in% c('cityChangzhou', 'ind_sector1'))]

# make names match names in regression models
names(X)[grep("gender", names(X))] = "gender1"
names(X)[grep("rural", names(X))] = "rural1"

# convert to matrix
X = data.matrix( X[,c(1:8,10:17,9)] ) 


# Extract  coefs and covariance matrix
betas = coef(models_strong[[6]])   
var.cov = vcov(models_strong[[6]])


# SIMULATE C. I. 
##################

# simulation params
M = 1000; m = 1000

# this is function g(.) in King, Tomz, & Wittenberg (2000)
inv.logit = function(x) 1 / (1+exp(-x))  

# sample from multivariate normal distribution: M random draws
betas.sim = mvrnorm(M, betas, var.cov)

# fix values at their means using effect() function
Xc = effect(strong.names[3], models_strong[[6]])$model.matrix

# multiply with simulated parameters
xb = betas.sim %*% t(Xc)

# obtain probabilities
prob.sim = inv.logit(xb)

# use these probabilities to reduce fundamental uncertainty by drawing 
# from a Bernoulli distribution.
binom_draws = apply(prob.sim,2, function(prob_vec) {
  sapply(prob_vec, function(x) mean(rbinom(m, size=1, prob=x)))
  } )

# Get 95% confidecne intervals
prob_ci95 = apply(binom_draws, 2, function(col) quantile(col, probs=c(0.0275, 0.975)))
prob_ci95 = rbind(prob_ci95, mean=colMeans(binom_draws))

colnames(prob_ci95) = paste0(rep("val",5), Xc[,"s_tit_tat"])


### P L O T   F I G U R E   1
########################

png("./results/figure_1.png", height = 4.5, width = 5.2, res = 150,units="in")

op = par(mfcol = c(1,1),oma = c(3.5,3.5,1.5,0.5) + 0.1,mar = c(1,0,1,0) + 0.1)

# empty plot and build piece by piece
plot(c(-0.05,0.8,1.05), c(0,0.5,1), main="", xlab="", ylab="",
     xaxt = 'n', yaxt = 'n', bty = 'n', type="n")
axis(1, at=seq(0,1,0.25), cex.axis=0.8, tick=FALSE, 
     col='black', col.ticks = 'black', line=-0.5)
axis(2, at=seq(0,1,0.2), tick=TRUE, 
     las=1, cex.axis=0.7, hadj=0.6)
abline(h=0.5, lty=2, col="gray80")
box()

# plot coefficients with confidence intervals
for(val in 1:length(Xc[,"s_tit_tat"])){
  
  y0 = prob_ci95[1, val]
  y1 = prob_ci95[2, val]
  
  # add estimates and CI
  segments(seq(0,1,0.25)[val], y0, seq(0,1,0.25)[val], y1, lwd=1.5, lty=1)
  points(seq(0,1,0.25)[val], prob_ci95[3, val], pch=19, cex=1.2) 
}

title(ylab="P(Cooperation)",outer = TRUE, line = 2,cex.lab=1)
title(xlab="Norm Enforcement", outer=TRUE, line = 1, cex.lab=1)
mtext("Retaliation", side=3, line=0.5, cex=1, font = 2)

par(op)
dev.off()


################################################################################


### T A B L E   5
#####################

models_inter = list()
for( j in 1:length(norms.strong) ){ # latex tables
  
  # estimate 2 models per norm enforcement: with and without control
  for(i in 1:2){
    
    # select variables
    all.var = strong.names[j]
    
    if(j!=1 & i==2) all.var = c(all.var, controls)
    
    # remove no_norm variable
    if(j==1 & i==2) all.var = c(all.var, controls[-1*grep("norm", controls)])
    
    # create formula
    var = "city"
    interaction = paste0(all.var[1],"*",var)
    all.var = all.var[c(-1,-grep(var,all.var))]
    all.var = c(interaction, all.var)
    f = as.formula( paste("pd ~ ", paste(all.var, collapse = "+")  ) )
    
    # estimate logistic regression
    m = glm(f , family=binomial(link="logit"), data = d)
    
    # save models
    save = j*2; if(i==1) save = save - 1
    
    # store models
    models_inter[[save]] = m
  }
}

stargazer(models_inter,
          title = paste("Prisoner's Dilemma and Strong Norms"),
          header = FALSE, label = "table5",
          column.labels = c("M1$a$", "M1$b$", 
                            "M2$a$", "M2$b$", 
                            "M3$a$", "M3$b$", 
                            "M4$a$", "M4$b$", 
                            "M5$a$", "M5$b$"),
          model.numbers = FALSE,
          dep.var.caption = "",
          dep.var.labels = "Cooperation",
          order = c(strong.names, rownames(summary(models_inter[[4]])$coef)[-c(1,2)]), 
          omit.stat=c("LL","ser","f"), no.space=TRUE, type = "text",
          font.size = "footnotesize", digits = 2,
          column.sep.width = "-8pt", notes.label = "",
          out = "./results/table_5.html")


################################################################################


### F I G U R E   2
#################################################################

model_ids = seq(2,10,2)

store.sims = list()
for(pred in 1:length(strong.names)){
  
  # Extract coefs and covariance matrix
  betas = coef(models_inter[[model_ids[pred]]])    
  var.cov = vcov(models_inter[[model_ids[pred]]]) 
  
  # simulation params
  M = 1000; m = 1000
  
  # this is func g(.) in King, Tomz, & Wittenberg (2000)
  inv.logit = function(x) 1 / (1+exp(-x))  
  
  # sample from multivariate normal distribution: M random draws
  betas.sim = mvrnorm(M, betas, var.cov)
  
  # get values at their means
  Xc = effect(paste0(strong.names[pred],"*city"), 
              models_inter[[model_ids[[pred]]]])$model.matrix
  
  xb = betas.sim %*% t(Xc)
  # obtain probabilities
  prob.sim = inv.logit(xb)
  
  # use these probabilities to reduce fundamental uncertainty 
  # by drawing from a Bernoulli distribution.
  binom_draws = apply(prob.sim,2, function(prob_vec) {
    sapply(prob_vec, function(x) mean(rbinom(m, size=1, prob=x)))
    } )
  
  # Get 95% confidence intervals
  prob_ci95 = apply(binom_draws, 2, function(col) {
    quantile(col, probs=c(0.0275, 0.975))
    })
  prob_ci95 = rbind(prob_ci95, mean=colMeans(binom_draws))
  
  store.sims[[pred]] = prob_ci95
}

# The following code produces 3 dfferent figures for models 3, 4, and 5
cities = c("Changzhou","Hangzhou","Nanjing","Shanghai","Wenzhou")
norm_labels = c("b",   "a",
                "Retaliation",
                "Punishment",
                "Community Sanctions")


### P L O T   F I G U R E   2
########################

png("./results/figure_2.png",
    units="in", width=7, height=5.5, res=150)

op = par(mfrow = c(2,2), oma=c(0,2,0,0), mar = c(3,3,2,1) + 0.1)

for(model in 3:5){
  if(model==3){
    city = c("cityNanjing", "cityWenzhou")
  } else{
    city = c("cityShanghai")
  }
  
  Xc = effect(paste0(strong.names[model],"*city"), 
              models_inter[[model_ids[[model]]]])$model.matrix
  
  for(c in 1:length(city)){
    
    # Start plot
    ids_city = which(Xc[,city[c]] == 1)
    vals = t(store.sims[[model]][,ids_city])
    
    ids_tittat = seq(0,1,0.25)
    
    # empty plot
    plot(c(-0.1,1.1),c(0,1),xaxt="n",yaxt="n", bty='n', pch='', 
         ylab = "", xlab = "")
    
    # add horizontal line at h=0.5
    abline(h=0.5, lty=2, col="gray80")
    
    # add predicted probabilities
    points(ids_tittat, vals[,3], type="p", pch=19, cex=1.2)
    lines(ids_tittat, vals[,3], lwd=1, lty=2)
    
    # add CI
    segments(ids_tittat,vals[,1],ids_tittat,vals[,2], lwd=1.5)
    
    box()
    
    # x-axis
    axis(1,at=seq(0,1,0.25),tick=FALSE,line=-0.5,cex.axis=0.8,lwd.ticks=0.5)
    
    # y-axis
    axis(2, at=seq(0,1,0.2), las=1,line=0,cex.axis=0.8)
    
    # add labels
    city_label = strsplit(city[c], "city")[[1]][2]
    title = paste0(norm_labels[model]," (",city_label,")")
    
    if(model==3){
      mtext(title, side=3,line=0.5,cex=0.8,adj=0,font=2, at=0.15)  
    } else if(model==4){
      mtext(title, side=3,line=0.5,cex=0.8,adj=0,font=2, at=0.13)
    } else{
      mtext(title, side=3,line=0.5,cex=0.8,adj=0,font=2, at=-0.02)
    }
    
  }
}

# add common y-axis label
title(ylab="P(Cooperation)", outer = TRUE, line = 0,cex.lab=1.3)

par(op)
dev.off()
