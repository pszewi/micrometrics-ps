# ----------------
# setup
# ----------------
# Template of R script to answer problem set
# Group number: 
# Group composition: A, B, and C

# Get the username
user <- Sys.info()["user"]
print(user)

# Define file path conditionally
if (user == "erick") {
    filepath <- "/home/erick/TEMP/"
} else if (user == "pszewi") {
    filepath <- "/FILE/PATH/A/"
} else if (user == "B") {
    filepath <- "/FILE/PATH/B/"
} else if (user == "C") {
    filepath <- "/FILE/PATH/C/"
} else {    
    filepath <- ""  # Default case if user is not listed
}

# Print the selected file path
print(paste("File path set to:", filepath))


# disabling a linter
# nolint: pipe_continuation_linter.

# ----------------
# imports
# ---------------

# for ritest package uncomment and run this line:
# install.packages(
#   "ritest",
#   repos = c("https://grantmcdermott.r-universe.dev", "https://cloud.r-project.org")
# )


library(dplyr)
library(stargazer)
library(tidyr)
library(RCT)
library(sandwich)
library(lmtest)
library(boot)
library(hdm)
library(ritest)

# ---------------
# funcs
# ---------------

# function that calculates the difference in means between groups
diff_mean <- function(data, treatment_d) {
		#this function needs re-writing but one should pass the parameters as 
		# df[cols] and df$treatment_d (i hate tidyverse syntax)
    treated <- filter(data, {{treatment_d}}==1)
    untreated <- filter(data, {{treatment_d}}==0)
    
    test <- t.test(treated, untreated, alternative = "two.sided", var.equal = FALSE)
    
    mean_diff <- test$estimate[1] - test$estimate[2]
    std_err <- test$stderr
    
    # assigns the results to a 2x2 dataframe, because will be needed for the proper functioning of the table
		# TODO: would be nice to turn it off also for casual use
    rtrn <- data.frame(
        mean_diff = rep(mean_diff, 2),
        std_err = rep(std_err, 2)
    )

    colnames(rtrn) <- c(paste0(colnames(data),"_mean_diff"), 
                        paste0(colnames(data),"_std_err"))
    rownames(rtrn) <- NULL

    rtrn
}

# function that creates the table
make_table1 <- function(df, treat_col, x_vect){
	initial_table <- df |> 
	  select({{treat_col}}, x_vect) |>
	  group_by({{treat_col}}) |>
	  summarise(across(everything(), list(mean = mean, sd = sd)))
	
	# calculate the differences in mean over a loop
	results <- list()
	for (col in x_vect) {
			# pass the input to another func 
	    results[[col]] <- diff_mean(df[col], select({{df}}, {{treat_col}}))
	}
	results <- as.data.frame(do.call(cbind, results))
	
	TABLE.1 <- cbind(initial_table, results)

	# pivot table for desired format	
	TABLE.1_pivoted <- TABLE.1 |>
	    pivot_longer(cols = !{{treat_col}}, names_to = "temp", values_to = "value") |>
	    separate(temp, into = c("variable", "stat"), sep = "_(?=mean|sd|std_err)") |>
	    mutate(variable = gsub('\\..*', '', variable)) |>
	    pivot_wider(names_from = stat, values_from = value) |>
	    pivot_wider(names_from = {{treat_col}}, values_from = c(mean, sd))

	TABLE.1_pivoted
}

# count controls and treated to be added in the table
reg_count <- function(model){
	# first extract treatment var ASSUMMING IT'S FIRST IN THE FORMULA
	form <- as.character(model$call[2])
	treat_var <- regmatches(form, regexec("~ (\\w+)",form))[[1]][2]

	# compute counts - assumes 1 treatment group
	t_count <- sum(model[['model']][treat_var])
	c_count <- nrow(model[['model']]) - t_count
	
	c(t_count, c_count)
}

# Construct the line vector 
add_lines <- function(models){
	counts = list(c("Treated"), c("Control"))

	for(i in seq_along(models)){
		model_count = reg_count(models[[i]])
		counts[[1]][[i+1]] = model_count[1]
		counts[[2]][[i+1]] = model_count[2]

	}
	counts
}

# runs a model loop, must pass a vector of formulas, df
# TODO: i have overcomplicated my life by including the vcov arg here, should've just applied it to a list of models post-factum
run_models <- function(forms, df, vcov='none'){

	models = list()
	se = list()
	for (i in seq_along(forms)){
	    reg = lm(as.formula(forms[i]), df)
			reg$call[[2]] <- as.formula(forms[i])
	    models[[i]] = reg
		if(vcov!='none'){
			# Adjust standard errors
			se[[i]] <- c(sqrt(diag(vcovHC(reg, type = vcov))))
		}
	}
	list('models'=models, 'standard.errs'=se)
}

# --------------------
# ------EXE 1---------
# --------------------

df <- read.csv("files/jtrain2.csv", sep = ";")

# subset for desired columns, make TABLE.1
cols <- c("age", "educ", "black", "hisp", "nodegree", "re74", "re75")
TABLE.1 <- make_table1(df, train, cols)

# From the table it can be seen that the covariates "nodegree" and "hisp" are significant (with "hisp" being significant at 10%, and "nodegree" at 5%)
# The fact that most variables are well balanced is to be expected, as this data comes from a randomly assigned experiment. 
# The lack of balance on "nodegree" and "hisp" may stem from the restriction of the sample, as "hisp" was balanced in the original sample, 
# The case was the same for the "nodgree" variable.

# b)---------------------------------

reg1 <- lm("re78 ~ train", df)

# The coefficient on train essentially gives us the mean difference of re78 between the treatment and control group. 
# Being positive, it could be interpreted that attaining treatment may have a positive effect on real earnings during 1978 measured i 1982's 1000USD, i.e. being treated is correlated with earning 1793USD more in 1978 (measured in 1982).

# c)---------------------------------
forms = c("train", "train + age + educ + black + hisp",
          "train + age + educ + black + hisp + re74 + re75")
forms <- paste0("re78 ~ ", forms)

models.1 <- run_models(forms, df)

TABLE.2 <- stargazer(
    models.1[[1]], type='text',
		add.lines = add_lines(models.1[[1]])
)

# After introducing the variables educ and black the coefficient on treatment becomes smaller, by about ~0.11 (~$110 in real earnings).
# Therefore, it may be argued that the results are sensitive to important covariates, but since the effect is fairly small
# and the coefficient changes very slightly at the introduction of additional covariates, it appears that generally it is not.


# d)---------------------------------

influence_train <- data.frame(train = (dfbeta(models.1[[1]][[3]])[, 2]))

# get the 3,5,10th lowest obs
smallest_b <- influence_train |>
  arrange(train) |>
  head(10) |>
  slice(c(3,5,10))

biggest_b <- influence_train |>
  arrange(desc(train)) |>
  head(10) |>
  slice(c(3,5,10))

df_restricted <- df[-c(as.numeric(rownames(smallest_b)), as.numeric(rownames(biggest_b))),]

reg2 = lm("re78 ~ train + age + educ + black + hisp + re74 + re75", df_restricted)

stargazer(reg2, type='text')

# Yes, it appears that a relatively large part of the effect is driven by observations at the ends of the distribution, as the coefficient shrinks by another ~0.18 ($180) and thus (~0.26 in total)


# ------------
# ---EXE 2----
# ------------
df.2 <- read.csv("files/jtrain3.csv", sep = ";")

# a)---------------------------------

# subsetting for columns
cols.2 <- c("age", "educ", "black", "hisp", "re74", "re75")

# make an intermediate table
TABLE.1.2 <- make_table1(df.2, train, cols.2)
TABLE.1.2 <- TABLE.1.2 |>
	mutate(variable = (paste0(variable, "_2")))

TABLE.1 <- rbind(TABLE.1, TABLE.1.2)
# b)---------------------------------

set.seed(12345)

# function that randomly allocates treatment 
random_alloc <- function(df, col_name){
	vals <- rnorm(n=nrow(df))
	df["random_vals"] <- vals
	df_sorted  <- df |>
						arrange(random_vals)
	df_sorted[col_name] <- c(rep_len(1, (nrow(df)/2)),rep_len(0, (nrow(df)/2)), 0)
	df_sorted
}


df.2 <- random_alloc(df.2,'treated')

# c)---------------------------------
assignments <- treatment_assign(df.2, share_control = 0.5,
					strata_varlist = c("Q1"),
					missfits = "global",
					n_t=1,
					key='random_vals')

df.2 <- left_join(df.2, assignments$data, by='random_vals')
df.2 <- df.2 |> 
	select(!c(strata, missfit)) |>
	rename(treated_2= "treat")

print(cor.test(df.2$treated, df.2$treated_2))

# As one can see,the correlation is not significant, as both of the treatment variables were assigned randomly. 


# d) ---------------------------------
TABLE.1.3 <- make_table1(df.2, treated, cols.2)
TABLE.1.3 <- TABLE.1.3 |>
	mutate(variable = (paste0(variable, "_3")))

TABLE.1 <- rbind(TABLE.1, TABLE.1.3)  |>
	mutate_if(is.numeric, round, 3)

stargazer(TABLE.1, type='text', summary=FALSE, digits = 1)

# Now, the table shows three versions of the variables - the initial ones, 
# the jtrain3 (with subscript _2) and the re-randomized variables (_3)
# One can see that the variables from group _2 are significant, with only hisp_2 not being significant
# This is surely because of the use of the non-experimental control units in the sample
# The variables from group _3 were re-randomized and therefore nothing is significant.


# e) ---------------------------------
forms.2 = c("treated", "treated + age + educ + black + hisp",
          "treated + age + educ + black + hisp + re74 + re75")
forms.2 <- paste0("re78 ~ ", forms.2)

models.2 <- run_models(forms.2, df.2)
models.2 <- mapply(c, models.1, models.2)

TABLE.2 = stargazer(
    models.2[[1]], type='text',
		add.lines = add_lines(models.2[[1]])
)

# This is not a very unexpected result given that in here we re-randomized the data with individuals that actually did not receive treatment. 
# Therefore, the coefficient shrinks as after the re-randomization, the new treatment group has not experienced the effect of treatment (as they did not receive it)


# f) ---------------------------------
forms.3 = c("train", "train + age + educ + black + hisp",
          "train + age + educ + black + hisp + re74 + re75")
forms.3 <- paste0("re78 ~ ", forms.3)

models.3 <- run_models(forms.3, df.2)
models.3 <- mapply(c, models.2, models.3)

TABLE.2 = stargazer(
    models.3[[1]], type='text',
		add.lines = add_lines(models.3[[1]])
)

# This is a regression using the original treatment assignment and the non-experimental group
# At first the coefficient appears to be strongly negative, but as we add covariates it tends to 0, which is understandable.
# This is an understandable result, because the people in the control group may come from different backgrounds and have different characteristics. 
# Only after controlling for the other variables we may be attaining a "comparable" group, with similar covariates.

## ---------------------------------
# ----------- EXE 3 ---------------
# ---------------------------------

# a) --------------------------------

# Option A: formula interface
lasso_y <- rlasso(re78 ~ age + educ + black + hisp + re74 + re75, data=df)

# Extract selected controls = nonzero coefficients (excluding intercept)
b <- coef(lasso_y)
selected <- names(b)[b != 0]
selected <- setdiff(selected, "(Intercept)")

selected
# This prints the covariates LASSO kept.

# --- Step 2: Post-LASSO OLS of re78 on train + selected controls ---
rhs <- c("train", selected)

post_formula <- as.formula(
  paste("re78 ~", paste(rhs, collapse = " + "))
)

ols_post <- lm(post_formula, data = df)

# Robust (HC1) standard errors (recommended)
rob_vcov <- vcovHC(ols_post, type = "HC1")
post_results <- coeftest(ols_post, vcov. = rob_vcov)

post_results
post_results["train", ]  # coefficient, SE, t, p-value for train

# Optional: also show conventional summary
summary(ols_post)

# b) ------------------------------

# ---------------------------------
# ----------- EXE 4 ---------------
# ---------------------------------

library(writexl)
library(grf)
library(stargazer)
library(ggplot2)
library(dplyr)
library(modelsummary)
library(sandwich)

# a1) 
logit_model <- glm(train ~ age + educ + black + hisp + re74,
                   data = jtrain3, family = binomial(link = "logit"))
summary(logit_model)

jtrain3$ps_logit <- predict(logit_model, type = "response")

psummary <- jtrain3 %>%
  mutate(group = ifelse(train == 1, "Treated", "Control")) %>%
  group_by(group) %>%
  summarise(
    min    = min(ps_logit, na.rm = TRUE),
    p25    = quantile(ps_logit, 0.25, na.rm = TRUE),
    median = median(ps_logit, na.rm = TRUE),
    p75    = quantile(ps_logit, 0.75, na.rm = TRUE),
    max    = max(ps_logit, na.rm = TRUE),
    n      = n(),
    .groups = "drop"
  )
print(psummary)

#excel table
write_xlsx(psummary, "pset1_q4a1_logit_summary.xlsx")

#latex table
psummary_latex <- psummary %>%
  mutate(
    min = c("2.22e-16", "4.05e-05"),
    p25 = c("1.8e-05", "0.314602"),
    median = c("0.000551", "0.657362"),
    p75 = c("0.012464", "0.770008"),
    max = c("0.834787", "0.892703"),
    n = c("2490", "185")
  ) %>%
  rename(
    Group = group,
    Min = min,
    p25 = p25,
    Median = median,
    p75 = p75,
    Max = max,
    N = n
  )

datasummary_df(
  psummary_latex,
  output = "pset1_q4a1_logit_summary.tex",
  title = "Summary statistics of estimated propensity scores (Logit)"
)
#end of latex table.

#a2)
X_mat <- as.matrix(jtrain3[, c("age", "educ", "black", "hisp", "re74")])

rf_model <- probability_forest(
  X = X_mat,
  Y = as.factor(jtrain3$train),
  num.trees = 2000,
  seed = 123
)

jtrain3$ps_rf <- predict(rf_model)$predictions[, 2]

#way to get the table in excel:
psummary_rf <- jtrain3 %>%
  mutate(group = ifelse(train == 1, "Treated", "Control")) %>%
  group_by(group) %>%
  summarise(
    min    = min(ps_rf, na.rm = TRUE),
    p25    = quantile(ps_rf, 0.25, na.rm = TRUE),
    median = median(ps_rf, na.rm = TRUE),
    p75    = quantile(ps_rf, 0.75, na.rm = TRUE),
    max    = max(ps_rf, na.rm = TRUE),
    n      = n(),
    .groups = "drop"
  )

print(psummary_rf)

#excel table
write_xlsx(psummary_rf, "pset1_q4a2_rf_summary.xlsx")

#latex table
psummary_rf_latex <- psummary_rf %>%
  mutate(
    min    = sprintf("%.6f", min),
    p25    = sprintf("%.6f", p25),
    median = sprintf("%.6f", median),
    p75    = sprintf("%.6f", p75),
    max    = sprintf("%.6f", max),
    n      = as.character(n)
  ) %>%
  rename(
    group  = group,
    min    = min,
    p25    = p25,
    median = median,
    p75    = p75,
    max    = max,
    n      = n
  )

datasummary_df(
  psummary_rf_latex,
  output = "pset1_q4a2_rf_summary.tex",
  title = "Summary statistics of estimated propensity scores (Random Forest)"
)
#end of latex table.


##### TA way of getting the summary statistics:
cat("\n--- Propensity Score Summary (Random Forest) ---\n")
cat("\ntreated:\n")
print(summary(jtrain3$ps_rf[jtrain3$train == 1]))
cat("\ncontrol:\n")
print(summary(jtrain3$ps_rf[jtrain3$train == 0]))


#a3)
#trimming:
trimmed <- jtrain3$ps_rf <= 0.8

#(i) the implied cutoff maxW =0 e(hat)(X)
cat("Implied cutoff max_{W=0} e_hat:", 
    round(max(jtrain3$ps_rf[jtrain3$train == 0]), 3), "\n")

#(ii) the number and fraction of treated units trimmed
#trimming diagnostics:
cat("\n--- Trimming Diagnostics ---\n")
cat("Observations before trimming:", nrow(jtrain3), "\n")
cat("Observations after trimming:", sum(trimmed), "\n")
cat("number of treatment units before trimming:", sum(jtrain3$train == 1), "\n")
cat("treatment units trimmed:", sum(jtrain3$train == 1 & !trimmed), "\n")
cat("fraction of treatment units trimmed:", 
    sum(jtrain3$train == 1 & !trimmed) / sum(jtrain3$train == 1), "\n")

#(iii) a brief characterization of which treated units are trimmed
vars <- c("age", "educ", "black", "hisp", "re74")

treated_trimmed <- jtrain3 %>%
  filter(train == 1 & !trimmed)

treated_kept <- jtrain3 %>%
  filter(train == 1 & trimmed)

comparison_table <- data.frame(
  variable = vars,
  mean_trimmed = sapply(vars, function(v) mean(treated_trimmed[[v]], na.rm = TRUE)),
  mean_kept    = sapply(vars, function(v) mean(treated_kept[[v]], na.rm = TRUE))
)

comparison_table$difference <- comparison_table$mean_trimmed - comparison_table$mean_kept

cat("\n--- Covariate Means: Trimmed Treated vs Kept Treated ---\n")
print(comparison_table)

#excel table
write_xlsx(comparison_table, "pset1_q4a3_comparison_table.xlsx")

#latex table
comparison_table_latex <- comparison_table %>%
  mutate(
    mean_trimmed = sprintf("%.6f", mean_trimmed),
    mean_kept    = sprintf("%.6f", mean_kept),
    difference   = sprintf("%.6f", difference)
  ) %>%
  rename(
    Variable = variable,
    "Mean trimmed" = mean_trimmed,
    "Mean kept" = mean_kept,
    Difference = difference
  )

datasummary_df(
  comparison_table_latex,
  output = "pset1_q4a3_comparison_table.tex",
  title = "Covariate means: trimmed treated vs kept treated"
)
#end of latex table. 


#The treated units which were hard to match (to control units, that is) are trimmed
#and treated units which were not hard to match are kept. This translates to an 
#attempt to minimize the differences in the samples of treated and control regarding 
#their distribution of propensity scores, for these specific covariates. Thereby, 
#improving overlap. The treated individuals with no comparable controls have been 
#removed from the sample. 
#Overall, the trimmed group are younger, less educated, more likely to be black, 
#slightly less likely to be hispanic, and have lower pre-treatment earnings. 


#a4)
#creating trimmed for logit and repeating the trimmed of rf for clarity (->trimmed_rf)
trimmed_logit <- jtrain3$ps_logit <= 0.8
trimmed_rf    <- jtrain3$ps_rf    <= 0.8
#col1 (full sample and controls):
col1 <- lm(re78 ~ age + educ + black + hisp + re74, data = jtrain3)
#col2 (trimmed logit):
col2 <- lm(re78 ~ age + educ + black + hisp + re74, data = jtrain3[trimmed_logit, ])
#col3 (trimmed rf):
col3 <- lm(re78 ~ age + educ + black + hisp + re74, data = jtrain3[trimmed_rf, ])
#col4-col6 (placebo):
col4 <- lm(re75 ~ age + educ + black + hisp + re74, data = jtrain3)
col5 <- lm(re75 ~ age + educ + black + hisp + re74, data = jtrain3[trimmed_logit, ])
col6 <- lm(re75 ~ age + educ + black + hisp + re74, data = jtrain3[trimmed_rf, ])


#creating table 3 -> put models in a named list
models <- list(
  "Col 1" = col1,
  "Col 2" = col2,
  "Col 3" = col3,
  "Col 4" = col4,
  "Col 5" = col5,
  "Col 6" = col6
)

coef_map <- c(
  "(Intercept)" = "Constant",
  "train"       = "Train",
  "age"         = "Age",
  "educ"        = "Education",
  "black"       = "Black",
  "hisp"        = "Hispanic",
  "re74"        = "1974 earnings"
)

#creating latex table:
modelsummary(
  models,
  vcov = "HC1",                         # robust SEs
  stars = c("*" = .1, "**" = .05, "***" = .01),
  statistic = "({std.error})",         # SEs in parentheses
  coef_map = coef_map,
  gof_omit = "AIC|BIC|Log.Lik|RMSE|F",  # keep table cleaner
  output = "table3.tex"
)
# b) ------------------------------

# ---------------------------------
# ----------- EXE 5 ---------------
# ---------------------------------

# a) ------------------------------

# Neyman's estimator is generally unbiased in finite sample, however, it's variance is biased, when there is heterogeneity in treatment effects.

# If we assume that treatment heterogeneity exist, the bias disappears only when we have that our sample is a random sample from an infinite population (so a classic random sampling assumption). 


# b) ------------------------------
# Fischer's inference is interested in testing null hypotheses under which we can infer all of the potential outcomes from observed ones. 
# It takes as the null hypothesis that for all units, the potential outcomes are equal to each other. 
# This procedure then relies on a choice of a proper test statistic. As an example, the authors choose the difference in means by treatment status.
# After choosing the test statistic, it is computed. Then, they calculate a p-value as the probability of observing a higher test statistic value given a randomly re-assigned treatment (but keeping proportions of treated and control equal in the sample).
# This is done by randomly reassinging treatment to units and computing the difference in means for a large number of runs. Then, the null hypothesis is rejected if the p-value (the fraction of test statistics with larger value within all runs) is small enough. 

# we run ritest with a reg model because it supports only those types of objects,
# thankfully regressing the variable on the treatment and a constant gives us a difference in means

ritest(lm("re78 ~ train", df), "train", reps=1000000, seed=123456)

# By using a sufficiently high value of repetitions we arrive at a similar p-value as the authors.

# c) ------------------------------

# The approach of the authors could be criticized for the lack of use of the covariates (something that is rectified later). This approach assumes that the individuals do not differ within the sample. 
# However, in the original article there exist small differences in the sample and accounting for them by the use of stratification for these covariates (maybe not all but at least gender/education) could make sure that the sample is randomized properly.

# d) ------------------------------


# 1) --------------------

# Briefly, HC1 simply re-weighs the variance-covariance matrix of the regression using a n/n-k term.

# HC3 errors actually re-weigh the diagonal of the v-cov matrix using residual matrix M_[] in order to balance out the importance of errors which values may be correlated with variables and may be larger compared to the others.

# 2) -------------


 forms = c("train", "train + age + educ + black + hisp",
           "train + age + educ + black + hisp + re74 + re75")
 forms <- paste0("re78 ~ ", forms)
 
 models.5 <- run_models(forms, df, vcov="HC3")
 
 TABLE.HC3 <- stargazer(
     models.5[[1]], type='text',
 		add.lines = add_lines(models.5[[1]]),
	se = models.5[[2]]
 )

# computing the dfbeta
influence_train <- data.frame(train = (dfbeta(models.5[[1]][[3]])[, 2]))

# get the 3,5,10th lowest obs
smallest_b <- influence_train |>
  arrange(train) |>
  head(10) |>
  slice(c(3,5,10))

biggest_b <- influence_train |>
  arrange(desc(train)) |>
  head(10) |>
  slice(c(3,5,10))

df_restricted <- df[-c(as.numeric(rownames(smallest_b)), as.numeric(rownames(biggest_b))),]

reg5 = lm("re78 ~ train + age + educ + black + hisp + re74 + re75", df_restricted)

TABLE.HC3.INFL <- stargazer(reg5, se=list(c(sqrt(diag(vcovHC(reg5, type = "HC3"))))),  type='text')



# 3) -----------


# bootstrapping function
boot_se <- function(model, R = 1000) {
  data <- model$model  # extract the data used in fitting
  
  boot_fn <- function(data, indices) {
    d <- data[indices, ]
    coef(lm(formula(model), data = d))
  }
  
  b <- boot(data, boot_fn, R = R)
  apply(b$t, 2, sd)  # SD of each coefficient across bootstrap samples
}

se_list <- lapply(models.5[[1]], boot_se)
TABLE.BOOT <- stargazer(models.5[[1]], se = se_list, type = "text")

TABLE.BOOT.INFL <- stargazer(reg5, se = list(boot_se(reg5)), type = "text")

# Bootstrapping standard errors relies on repeatedly re-sampling the sample (with replacement, many times)
# Then re-running the model many times, in order to see how the coefficient changes
# Then we construct the standard error from the variation in the computed coefficient.

# 4) -----------

# While the standard errors in the case of the HC3 errors and the bootstrapped errors are larger (as expected)
# The conclusions within our model do not change given that the significance of coefficients does not change
# between different error sets
#TODO : RELATE TO DATACOLADA
