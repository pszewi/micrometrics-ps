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
} else if (user == "Jakub") {
    filepath <- "/FILE/PATH/A/"
} else if (user == "Nicolò") {
    filepath <- "/Users/bogiano1945/Desktop/directory ps1/"
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
library(hdm)

# ---------------
# funcs
# ---------------

# function that calculates the difference in means between groups
diff_mean <- function(data, treatment_d) {
    treated <- filter(data, {{treatment_d}}==1)
    untreated <- filter(data, {{treatment_d}}==0)
    
    test <- t.test(treated, untreated, alternative = "two.sided", var.equal = FALSE)
    
    mean_diff <- test$estimate[1] - test$estimate[2]
    std_err <- test$stderr
    
    
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
make_table <- function(df, treat_col, x_vect){
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

# --------------------
# ------EXE 1---------
# --------------------

df <- read.csv("files/jtrain2.csv", sep = ";")

# subset for desired columns, make TABLE.1
cols <- c("age", "educ", "black", "hisp", "nodegree", "re74", "re75")
TABLE.1 <- make_table(df, train, cols)

# TODO: COMMENT ON THE TOPIC!

# b)---------------------------------

reg1 <- lm("re78 ~ train", df)

# TODO: INTERPRET THE COEFFICIENT!

# c)---------------------------------
forms = c("train", "train + age + educ + black + hisp",
          "train + age + educ + black + hisp + re74 + re75")

models.1 = list()
for (i in seq_along(forms)){
    reg = lm(paste0("re78 ~ ", forms[i]), df)
    models.1[[i]] = reg
}

TABLE.2 <- stargazer(
    models.1, type='text'
)

# TODO: ADD COMMENTS ON THE TOPIC!
# TODO: CLEANUP THE WORKFLOW HERE IF HAVE FREE TIME

# d)---------------------------------

influence_train <- data.frame(train = (dfbeta(models[[3]])[, 2]))

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
TABLE.1.2 <- make_table(df.2, train, cols.2)
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
TABLE.1.3 <- make_table(df.2, treated, cols.2)
TABLE.1.3 <- TABLE.1.3 |>
	mutate(variable = (paste0(variable, "_3")))

TABLE.1 <- rbind(TABLE.1, TABLE.1.3)

# TODO: write the comments!!

# e) ---------------------------------
forms = c("treated", "treated + age + educ + black + hisp",
          "treated + age + educ + black + hisp + re74 + re75")

models.2 = list()
for (i in seq_along(forms)){
    reg = lm(paste0("re78 ~ ", forms[i]), df.2)
    # print(summary(reg))
    models.2[[i]] = reg
}

TABLE.2 = stargazer(
    models.1, models.2, type='text'
)

# TODO COMMENT ON FINDINGS!

# f) ---------------------------------
forms = c("train", "train + age + educ + black + hisp",
          "train + age + educ + black + hisp + re74 + re75")


models.3 = list()
for (i in seq_along(forms)){
    reg = lm(paste0("re78 ~ ", forms[i]), df.2)
    # print(summary(reg))
    models.3[[i]] = reg
}

TABLE.2 = stargazer(
    models.1, models.2, models.3, type='text'
)
# TODO COMMENT ON FINDINGS!

# ---------------------------------
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
# b) ------------------------------
# c) ------------------------------
# d) ------------------------------

