#########################
#########################
#### Clear Workspace ####
#########################
#########################

rm(list = ls()) 
# clear global environment to remove all loaded data sets, functions and so on.

###################
###################
#### Libraries ####
###################
###################

library(easypackages) # enables the libraries function
suppressPackageStartupMessages(
  libraries("ggplot2", # visualisation
            "pastecs", # descriptive statistics
            "car", # for durbin watson test
            "rstatix", # univarate outliers
            "ggpubr", # for creating easily publication ready plots
            "GGally",
            "MASS" # for lda() function
))

setwd("C:/R Portfolio/MANOVA")
ocdData <- read.delim("OCD.Dat", header = T)
str(ocdData)

#########################
# Reorder Factor Levels #
#########################

ocdData$Group <- factor(ocdData$Group, 
                        levels = c("CBT", "BT", "No Treatment Control"), 
                        labels = c("CBT", "BT", "NT"))

####################
####################
# Explore the data #
####################
####################

##########
# Graphs #
##########

ocdMelt <- melt(ocdData, id = c("Group"), measured = c("Actions", "Thoughts"))
names(ocdMelt) <- c("Group", "Outcome_Measure", "Frequency")

################
# Scatter Plot #
################

ocdScatter <- ggplot(ocdData, aes(Actions, Thoughts))
ocdScatter + geom_point() + geom_smooth(method = "lm") + 
  labs(x = "Number of Obsession-Related Behaviours", 
       y = "Number of Obsession-Related Thoughts") + 
  facet_wrap(~ Group, ncol = 3) + theme_classic()
imageFile <- paste("C:/R Portfolio/MANOVA", "16 OCD Scatter.png", sep = "/")
ggsave(file = imageFile)

###########
# Barplot #
###########

ocdBar <- ggplot(ocdMelt, aes(Group, Frequency, fill = Outcome_Measure))
ocdBar + stat_summary(fun = mean, geom = "bar", position = "dodge") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", 
             position = position_dodge(width = 0.90), width = 0.2) + 
  labs(x = "Treatment Group", y = "Number of Thoughts/Actions", 
       fill = "Outcome Measure") + 
  scale_y_continuous(breaks = seq(0, 20, by = 2))

imageFile <- paste("C:/R Portfolio/MANOVA", "16 OCD Bar.png", sep = "/")
ggsave(file = imageFile)

###########
# Boxplot #
###########

ocdBoxplot <- ggplot(ocdMelt, aes(Group, Frequency, colour = Outcome_Measure))
ocdBoxplot + geom_boxplot() + labs(x = "Treatment Group", y = "Number of Thoughts/Actions", colour = "Outcome Measure") + scale_y_continuous(breaks = seq(0, 20, by = 2))
imageFile <- paste("C:/R Portfolio/MANOVA","16 OCD Boxplot.png",sep="/")
ggsave(file = imageFile)

##########################
# Descriptive Statistics #
##########################

options(digits = 3)
by(ocdData$Actions, ocdData$Group, stat.desc, basic = FALSE)
by(ocdData$Thoughts, ocdData$Group, stat.desc, basic = FALSE)
options(digits = 7)

#####################
#####################
# Check Assumptions #
#####################
#####################

# Adequate sample size. Rule of thumb: the n in each cell > the number of outcome variables.
# Independence of the observations. Each subject should belong to only one group. There is no relationship between the observations in each group. Having repeated measures for the same participants is not allowed. The selection of the sample should be completely random.
# Absense of univariate or multivariate outliers.
# Multivariate normality. The R function mshapiro_test( )[in the rstatix package] can be used to perform the Shapiro-Wilk test for multivariate normality.
# Absence of multicollinearity. The dependent (outcome) variables cannot be too correlated to each other. No correlation should be above r = 0.90 [Tabachnick and Fidell (2012)}.
# Linearity between all outcome variables for each group.
#Homogeneity of variances. The Levene's test can be used to test the equality of variances between groups. Non-significant values of Levene's test indicate equal variance between groups.
# Homogeneity of variance-covariance matrices. The Box's M Test can be used to check the equality of covariance between the groups. This is the equivalent of a multivariate homogeneity of variance. This test is considered as highly sensitive. Therefore, significance for this test is determined at alpha = 0.001.

##############################
# Check Adequate Sample Size #
##############################

ocdData %>%
  group_by(Group) %>%
  summarise(N = n())

# There are more rows than variables, so this assumption is met

################################
# Identify univariate outliers #
################################

# Univariate outliers can be easily identified using box plot methods, implemented in the R function identify_outliers() [rstatix package].

# Group the data by Group and then, identify outliers in the Actions variable:
  
ocdData %>%
  group_by(Group) %>%
  identify_outliers(Actions)

# No outliers

# Group the data by Group and then, identify outliers in the Thoughts variable:

ocdData %>%
  group_by(Group) %>%
  identify_outliers(Thoughts)

# No outliers

################################
# Detect multivariate outliers #
################################

# Multivariate outliers are data points that have an unusual combination of values on the outcome (or dependent) variables.

# In MANOVA setting, the Mahalanobis distance is generally used to detect multivariate outliers. The distance tells us how far an observation is from the center of the cloud, taking into account the shape (covariance) of the cloud as well.

# The function mahalanobis_distance() [rstatix package] can be easily used to compute the Mahalanobis distance and to flag multivariate outliers. Read more in the documentation of the function.

# This metric needs to be calculated by groups:

# Compute distance by groups and filter outliers
ocdData %>%
  group_by(Group) %>%
  mahalanobis_distance() %>%
  filter(is.outlier == TRUE) %>%
  as.data.frame()

# No outliers

# Another test is to use aq.plot(

aq.plot(ocdData[, 2:3])


#########################################
# Check univariate normality assumption #
#########################################

# The normality assumption can be checked by computing Shapiro-Wilk test for each outcome variable at each level of the grouping variable. If the data is normally distributed, the p-value should be greater than 0.05.

ocdData %>%
  group_by(Group) %>%
  shapiro_test(Actions, Thoughts) %>%
  arrange(variable)

# You can also create QQ plot for each group. QQ plot draws the correlation between a given data and the normal distribution.

# QQ plot of Actions
ggqqplot(ocdData, "Actions", facet.by = "Group",
         ylab = "Actions", ggtheme = theme_bw())

# QQ plot of Thoughts
ggqqplot(ocdData, "Thoughts", facet.by = "Group",
         ylab = "Thoughts", ggtheme = theme_bw())

##########################
# Multivariate normality #
##########################

ocdData %>%
  select(Actions, Thoughts) %>%
  mshapiro_test()

# The test is not significant, so we can assume multivariate normality

##############################
# Identify multicollinearity #
##############################

# Ideally the correlation between the outcome variables should be moderate, not too high. A correlation above 0.9 is an indication of multicollinearity, which is problematic for MANOVA.

# In other hand, if the correlation is too low, you should consider running separate one-way ANOVA for each outcome variable.

# Compute pairwise Pearson correlation coefficients between the outcome variable. In the following R code, we'll use the function cor_test() [rstatix package]. If you have more than two outcome variables, consider using the function cor_mat():
  
ocdData %>% cor_test(Actions, Thoughts)

# There is no multicollinearity

##############################
# Check linearity assumption #
##############################

# The pairwise relationship between the outcome variables should be linear for each group. This can be checked visually by creating a scatter plot matrix using the R function ggpairs() [GGally package]. In our example, we have only one pair:
  
# Create a scatterplot matrix by group #

results <- ocdData %>%
  select(Actions, Thoughts, Group) %>%
  group_by(Group) %>% 
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results

# Show the plots #

results$plots

# Non-linear relationships were detected, so the options are:

# transform or remove the concerned outcome variables
# run the analysis anyway. There will be a loss of some power.

###################################################
# Check the homogeneity of covariances assumption #
###################################################

# This can be evaluated using the Box's M-test 

box_m(ocdData[, c("Actions", "Thoughts")], ocdData$Group)

# The test is not statistically significant (i.e., p < 0.180), so the data have not violated the assumption of homogeneity of variance-covariance matrices.

# Note that, if you have balanced design (i.e., groups with similar sizes), you don't need to worry too much about violation of the homogeneity of variances-covariance matrices and you can continue your analysis.

# However, having an unbalanced design is problematic. Possible solutions include: 1) transforming the dependent variables; 2) running the test anyway, but using Pillai's multivariate statistic instead of Wilks' statistic.

###############################################
# Check the homogneity of variance assumption #
###############################################

# For each of the outcome variables, the one-way MANOVA assumes that there are equal variances between groups. This can be checked using the Levene's test of equality of variances. Key R function: levene_test() [rstatix package].

# Procedure:
  
# Gather the outcome variables into key-value pairs
# Group by variable
# Compute the Levene's test

ocdData %>% 
  gather(key = "variable", value = "value", Actions, Thoughts) %>%
  group_by(variable) %>%
  levene_test(value ~ Group)

# The Levene's test is not significant (p >0.05), so there was homogeneity of variances.

# As there is not have homogeneity of variances, you can try to transform the outcome (dependent) variable to correct for the unequal variances.

# Alternatively, you can continue, but accept a lower level of statistical significance (alpha level) for your MANOVA result. Additionally, any follow-up univariate ANOVAs will need to be corrected for this violation (i.e., you will need to use different post-hoc tests).

################
################
# MANOVA Model #
################
################

# Four distinct types of multivariate statistics can be used for computing MANOVA, including "Pillai", "Wilks", "Hotelling-Lawley", or "Roy".

# The most commonly recommended multivariate statistic to use is Wilks' Lambda.

# However, Pillai's Trace is more robust and is recommended when you have unbalanced design and also have a statistically significant Box's M result (as in our example, see previous section).

# Note that, "Pillai" is the default in the R Manova() function [car package].

model <- lm(cbind(Actions, Thoughts) ~ Group, ocdData)
Manova(model, test.statistic = "Pillai")

# There was a statistically significant difference between the Groups on the combined dependent variables (Actions and Thoughts), F(4, 54) = 2.56, p 0.049.

##################
# Post Hoc Tests #
##################

# A statistically significant one-way MANOVA can be followed up by univariate one-way ANOVA examining, separately, each dependent variable. The goal is to identify the specific dependent variables that contributed to the significant global effect.

# Compute univariate one-way ANOVA:

# Gather the outcome variables into key-value pairs
# Group by variable
# Compute one-way ANOVA test
# Note that, there are different R function to compute one-way ANOVA depending whether the assumptions are met or not:

# anova_test() [rstatix]: can be used when normality and homogeneity of variance assumptions are met
# welch_anova_test() [rstatix]: can be used when the homogeneity of variance assumption is violated, as in our example.
# kruskal_test() [rstatix]: Kruskal-Wallis test, a non parametric alternative of one-way ANOVA test

# Group the data by variable
grouped.data <- ocdData %>%
  gather(key = "variable", value = "value", Actions, Thoughts) %>%
  group_by(variable)

# Do welch one way anova test
grouped.data %>% welch_anova_test(value ~ Group)
# or do Kruskal-Wallis test
grouped.data %>% kruskal_test(value ~ Group)
# or use aov()
grouped.data %>% anova_test(value ~ Group)

# Note that, as there are two dependent variables, we need to apply Bonferroni multiple testing correction needs to be applied by decreasing the the level to declare statistical significance.

# To do this, the classic alpha level (0.05) must be divided by the number of tests (or dependent variables, here 2). This leads to a significance acceptance criteria of p < 0.025 rather than p < 0.05 because there are two dependent variables.

#########################################
# Compute multiple pairwise comparisons #
#########################################

# A statistically significant univariate ANOVA can be followed up by multiple pairwise comparisons to determine which groups are different.

# The R functions tukey_hsd() [rstatix package] can be used to compute Tukey post-hoc tests if the homogeneity of variance assumption is met.

# If you had violated the assumption of homogeneity of variances, as in our example, you might prefer to run a Games-Howell post-hoc test. It's also possible to use the function pairwise_t_test() [rstatix] with the option pool.sd = FALSE and var.equal = FALSE .

pwc <- ocdData %>%
  gather(key = "variables", value = "value", Actions, Thoughts) %>%
  group_by(variables) %>%
  games_howell_test(value ~ Group) %>%
  select(-estimate, -conf.low, -conf.high) # Remove details
pwc

# All pairwise comparisons were not significant for each of the outcome variables (Actions and Thoughts).

##########################################
# Visualization: box plots with p-values #
##########################################

pwc <- pwc %>% add_xy_position(x = "Group")
test.label <- create_test_label(
  description = "MANOVA", statistic.text = quote(italic("F")),
  statistic = 2.56, p = "0.049", parameter = "4,54",
  type = "expression", detailed = TRUE
)

ggboxplot(
  ocdData, x = "Group", y = c("Actions", "Thoughts"), 
  merge = TRUE, palette = "jco"
) + 
  stat_pvalue_manual(
    pwc, hide.ns = TRUE, tip.length = 0, 
    step.increase = 0.1, step.group.by = "variables",
    color = "variables"
  ) +
  labs(
    subtitle = test.label,
    caption = get_pwc_label(pwc, type = "expression")
  )

###############################################
# Follow Up MANOVA with Discriminant Analysis #
###############################################

ocdDFA <- lda(Group ~ Actions + Thoughts, ocdData)
predict(ocdDFA)
plot(ocdDFA)
