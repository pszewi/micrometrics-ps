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
library(dplyr)
library(stargazer)
library(tidyr)
library(RCT)
library(sandwich)
library(lmtest)
library(boot)

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

# TODO: COMMENT ON THE TOPIC!

# b)---------------------------------

reg1 <- lm("re78 ~ train", df)

# TODO: INTERPRET THE COEFFICIENT!

# c)---------------------------------
forms = c("train", "train + age + educ + black + hisp",
          "train + age + educ + black + hisp + re74 + re75")
forms <- paste0("re78 ~ ", forms)

models.1 <- run_models(forms, df)

TABLE.2 <- stargazer(
    models.1[[1]], type='text',
		add.lines = add_lines(models.1[[1]])
)

# TODO: ADD COMMENTS ON THE TOPIC!
# TODO: CLEANUP THE WORKFLOW HERE IF HAVE FREE TIME

# d)---------------------------------

influence_train <- data.frame(train = (dfbeta(models.1[[3]])[, 2]))

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

stargazer(reg2, type='latex')
# TODO: ADD COMMENTS ON THE TOPIC!


# ------------
# ---EXE 2----
# ------------
df.2 <- read.csv("files/jtrain3.csv", sep = ";")
# View(df)

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

# TODO: ADD SOME COMMENTS ON SIGNIFICANCE

# d) ---------------------------------
TABLE.1.3 <- make_table1(df.2, treated, cols.2)
TABLE.1.3 <- TABLE.1.3 |>
	mutate(variable = (paste0(variable, "_3")))

TABLE.1 <- rbind(TABLE.1, TABLE.1.3)  |>
	mutate_if(is.numeric, round, 3)

# final moment when printing the table in latex
stargazer(TABLE.1, type='latex', summary=FALSE, digits = 1)
# TODO: write the comments!!

# e) ---------------------------------
forms.2 = c("treated", "treated + age + educ + black + hisp",
          "treated + age + educ + black + hisp + re74 + re75")
forms.2 <- paste0("re78 ~ ", forms.2)

models.2 <- run_models(forms.2, df.2)
models.2 <- c(models.1, models.2)

TABLE.2 = stargazer(
    models.2[[1]], type='latex',
		add.lines = add_lines(models.2[[1]])
)

# TODO COMMENT ON FINDINGS!

# f) ---------------------------------
forms.3 = c("train", "train + age + educ + black + hisp",
          "train + age + educ + black + hisp + re74 + re75")
forms.3 <- paste0("re78 ~ ", forms.3)

models.3 <- run_models(forms.3, df.2)
models <- c(models.1, models.2, models.3)

TABLE.2 = stargazer(
    models[[1]], type='latex',
		add.lines = add_lines(models[[1]])
)


# TODO COMMENT ON FINDINGS!
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

# a) ------------------------------
# b) ------------------------------

# ---------------------------------
# ----------- EXE 5 ---------------
# ---------------------------------

# a) ------------------------------
# Answer yet to be found

# b) ------------------------------
# TODO: Describe Fischer's inference:

# first going to get that post-treatment difference

diff_p_val <- diff_mean(data=df['re78'], treatment_d =df$train)

print(paste0("The mean difference is in fact: ", round(diff_p_val[1,1], 2)))

# makes a vector of 1s and 0s 
# WARNING: The paper references 240 control individuals but there is actually 260 in our dataset!!!!
tc <- c(rep(1, 185), rep(0, 260))
n_trials <- 10^5

stat_val <- vector(mode="numeric", n_trials)
for(i in c(1:n_trials)){
	stat_val[i] <- diff_mean(df['re78'], sample(tc))[1,1]

}

print(paste0("The p-val is: ", sum(stat_val>1.79)/n_trials))
# Therefore, we do not arrive at the same p-value

# TODO: Describe the findings!!!!

# c) ------------------------------

# TODO: READ AND DESCRIBE!!!

# d) ------------------------------
# 1) --------------------
# TODO: DESCRIBE HERE 


# 2) -------------
 forms = c("train", "train + age + educ + black + hisp",
           "train + age + educ + black + hisp + re74 + re75")
 forms <- paste0("re78 ~ ", forms)
 
 models.5 <- run_models(forms, df, vcov="HC3")
 
 TABLE.2.5 <- stargazer(
     models.5[[1]], type='text',
 		add.lines = add_lines(models.5[[1]]),
	se = models.5[[2]]
 )

# 3) -----------

help(boot)

# TODO: slop to be verified and corrected 
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
tab <- stargazer(models.5, se = se_list, type = "text")

# TODO: DESCRIBE
# 4) -----------
# TODO: DESCRIBE
