# ------------------------------------------------------------
# Multinomial logistic regression for LPA profile membership
# and average marginal effects (AMEs) across respondent characteristics
#
# Overview:
#   1) Construct respondent characteristics (gender, age, education, hukou, marriage) for 2017/2018/2021
#   2) Merge characteristics with the k=5 LPA profile assignments (lpa_k5)
#   3) Fit multinomial logit model of profile membership (reference = Forthright Moderates)
#   4) Compute AMEs using marginaleffects::avg_slopes()
#   5) Plot and save AME figures for gender, education, and hukou
# ------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(stringr)
library(nnet)
library(marginaleffects)

# ---- 1) Construct respondent characteristics by year ----
# Variables:
#   gender: respondent gender
#   age: computed as survey year - birth year (as available in CGSS)
#   educlass: education code (will be recoded into 3 categories below)
#   hukou: hukou status (recoded into 2 categories below)
#   marriage: marital status code (mapped to Married vs Not Married below)

#2017:
demo2017 <- cgss2017[, c("a2", "a31", "a7a", "a18", "a69")]
colnames(demo2017) <- c("gender", "age", "educlass", "hukou", "marriage")
demo2017$age <- 2017 - demo2017$age
demo2017$year <- "2017"

#2018
demo2018 <- cgss2018[,c("a2","a31","a7a","a18", "a69")]
colnames(demo2018) <- c("gender", "age", "educlass", "hukou", "marriage")
demo2018$age <- 2018 - demo2018$age
demo2018$year <- "2018"

#2021
demo2021 <- cgss2021[, c("A2", "A3_1", "A7a", "A18", "A69")]
colnames(demo2021) <- c("gender", "age", "educlass", "hukou", "marriage")
demo2021$age <- 2021 - demo2021$age
demo2021$year <- "2021"

# Merge respondent characteristics across waves
demo1721 <- rbind(demo2017, demo2018, demo2021)

# ---- 2) Recode covariates ----
# Education: collapse into three categories (1/2/3) according to original coding ranges

demo1721$educlass <- ifelse(demo1721$educlass < 5, 1,
                         ifelse(demo1721$educlass < 9, 2,
                                ifelse(demo1721$educlass < 14, 3, NA)))

# Hukou: binary recode (1 vs 2)
demo1721$hukou <- ifelse(demo1721$hukou == 1, 1, 2)

# Marriage: collapse multiple codes into Married vs Not Married
demo1721$marriage_factor <- factor(demo1721$marriage,
                                 levels = c(1, 2, 3, 4, 5, 6, 7),
                                 labels = c("Not Married",            
                                            "Not Married",          
                                            "Married",   
                                            "Married",    
                                            "Not Married", 
                                            "Not Married", 
                                            "Not Married"))


# ---- 3) Merge covariates with LPA results (k = 5) ----
analysis_lpa_k5 <- cbind(demo1721, lpa_k5)

# Set labels for clusters 
analysis_lpa_k5$Cluster <- factor(analysis_lpa_k5$Cluster,
                                  levels = c("0", "3", "4", "2", "1"),
                                  labels = c("Passionate Egalitarians", 
                                             "Genuine Neutrals", 
                                             "Forthright Moderates",
                                             "Evasive Traditionalists",
                                             "Unembellished Traditionalists"))



# Choose the reference category for the multinomial logit model
analysis_lpa_k5$Cluster <- relevel(analysis_lpa_k5$Cluster, ref = "Forthright Moderates")

# Ensure key predictors are treated as factors
analysis_lpa_k5$educlass <- as.factor(analysis_lpa_k5$educlass)

# Use complete cases for model fitting and AME computation
analysis_lpa_k5_complete <- analysis_lpa_k5[complete.cases(analysis_lpa_k5),]
analysis_lpa_k5_complete$gender <- as.factor(analysis_lpa_k5_complete$gender)

# ---- 4) Fit multinomial logistic regression ----

model <- multinom(Cluster ~ gender + educlass + hukou + marriage_factor + age + year, 
                  data = analysis_lpa_k5_complete)

# Model summary and coefficient table
summary(model)
coef(summary(model))

#Wald z-tests and approximate two-sided p-values

z <- summary(model)$coefficients/summary(model)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
round(p, 4)


# ---- 5) Average marginal effects (AMEs) ----
# avg_slopes() returns average marginal effects (by outcome category) for all predictors

play <- avg_slopes(model)

cluster_order <- c("Passionate Egalitarians",
                   "Genuine Neutrals",
                   "Forthright Moderates",
                   "Evasive Traditionalists",
                   "Unembellished Traditionalists")

# ------------------------------------------------------------
# 5a) AME of gender
# ------------------------------------------------------------
ame_gender <- play[play$term == "gender", ]
ame_gender$group <- factor(ame_gender$group, levels = cluster_order)

p <- ggplot(data = ame_gender) + geom_point(aes(x = group, y = estimate)) + 
  geom_errorbar(aes(x = group, ymin = conf.low, ymax = conf.high, group = term), width = 0.1) + coord_flip() +
  geom_hline(yintercept = 0, color = "grey") + xlab("") + ylab("")

p <- p + theme(panel.border = element_rect(linetype = "solid", fill=NA), 
               panel.background = element_rect(fill="white"),
               strip.background = element_rect(fill = "white")
)

p <- p + theme(axis.text.y = element_text(face = "bold", 
                                          size = 10,
                                          hjust = 0.5,
                                          vjust = 0.5,
                                          lineheight = 0.9)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

p


png(filename = "AMEgender.png",units = "in", width = 10, height = 5, res = 500)
print (p)
dev.off()

# ------------------------------------------------------------
# 5b) AME of education (contrasts vs the baseline category)
# ------------------------------------------------------------
ame_edu <- play[play$term == "educlass", ]
ame_edu$group <- factor(ame_edu$group, levels = cluster_order)

# Label the education contrasts (assuming the baseline is level 1)
ame_edu$contrast <- factor(ame_edu$contrast,
                           levels = c("2 - 1", "3 - 1"),
                           labels = c("High School Equivalent", "College and Above"))
# Rename the contrast column for clarity in ggplot aesthetics
colnames(ame_edu)[3] <- "Education"

p <- ggplot(data = ame_edu) + 
  geom_point(aes(x = group, y = estimate, group = Education, shape = Education)) + 
  #facet_wrap(.~ indicator, ncol = 1) +
  geom_errorbar(aes(x = group, ymin = conf.low, ymax = conf.high, 
                    group = Education, color = Education), 
                width = 0.1, position = "dodge",) + 
  coord_flip() + scale_color_grey(start = 0.5, end = 0.2) +
  geom_hline(yintercept = 0, color = "grey") + xlab("") + ylab("") 

p <- p + theme(legend.position = c(.82, .8), 
               legend.text = element_text()) 

p <- p + theme(panel.border = element_rect(linetype = "solid", fill=NA), 
               panel.background = element_rect(fill="white"),
               strip.background = element_rect(fill = "white"))

p <- p + theme(axis.text.y = element_text(face = "bold", 
                                          size = 10,
                                          hjust = 0.5,
                                          vjust = 0.5,
                                          lineheight = 0.9)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


p
png(filename = "AMEducation.png",units = "in", width = 10, height = 5, res = 500)
print (p)
dev.off()

# ------------------------------------------------------------
# 5c) AME of hukou
# ------------------------------------------------------------
ame_hukou <- play[play$term == "hukou", ]
ame_hukou$group <- factor(ame_hukou$group, levels = cluster_order)


p <- ggplot(data = ame_hukou) + geom_point(aes(x = group, y = estimate)) + 
  geom_errorbar(aes(x = group, ymin = conf.low, ymax = conf.high, group = term), width = 0.1) + coord_flip() +
  geom_hline(yintercept = 0, color = "grey") + xlab("") + ylab("")

p <- p + theme(panel.border = element_rect(linetype = "solid", fill=NA), 
               panel.background = element_rect(fill="white"),
               strip.background = element_rect(fill = "white")
)

p <- p + theme(axis.text.y = element_text(face = "bold", 
                                          size = 10,
                                          hjust = 0.5,
                                          vjust = 0.5,
                                          lineheight = 0.9)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

p
png(filename = "AMEhukou.png",units = "in", width = 10, height = 4, res = 500)
print (p)
dev.off()

