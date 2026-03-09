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

# ---------------
# funcs
# ---------------
diff_mean <- function(x, treatment_d) {
    treated <- x[treatment_d == 1]
    untreated <- x[treatment_d == 0]
    
    test <- t.test(treated, untreated, alternative = "two.sided", var.equal = FALSE)
    
    mean_diff <- test$estimate[1] - test$estimate[2]
    std_err <- test$stderr
    
    
    rtrn <- data.frame(
        mean_diff = rep(mean_diff, 2),
        std_err = rep(std_err, 2)
    )

    colnames(rtrn) <- c(paste0(colnames(x),"_mean_diff"), 
                        paste0(colnames(x),"_std_err"))
    rownames(rtrn) <- NULL

    rtrn
}



# --------------------
# ------EXE 1---------
# --------------------

df <- read.csv("files/jtrain2.csv", sep = ";")
# View(df)

# subsetting for columns
cols <- c("age", "educ", "black", "hisp", "nodegree", "re74", "re75")
TABLE.1 <- df %>%
  select(train, all_of(cols)) %>%
  group_by(train) %>%
  summarise(across(everything(), list(mean = mean, sd = sd)))

# calculate the differences in mean
results <- list()
for (col in cols) {
    results[[col]] <- diff_mean(df[col], df['train'])
}
results <- as.data.frame(do.call(cbind, results))

TABLE.1 <- cbind(TABLE.1, results)

# view(TABLE.1_p)

TABLE.1_p <- TABLE.1 %>%
    pivot_longer(cols = !train, names_to = "temp", values_to = "value") %>%
    separate(temp, into = c("variable", "stat"), sep = "_(?=mean|sd|std_err)") %>%
    mutate(variable = gsub('\\..*', '', variable)) %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    pivot_wider(names_from = train, values_from = c(mean, sd))


# b)

reg1 <- lm("re78 ~ train", df)

# c)
forms = c("train", "train + age + educ + black + hisp",
          "train + age + educ + black + hisp + re74 + re75")

models = list()
for (i in seq_along(forms)){
    reg = lm(paste0("re78 ~ ", forms[i]), df)
    # print(summary(reg))
    models[[i]] = reg
}

stargazer(
    models, type='text'
)

# d)

influence_train <- data.frame(train = (dfbeta(models[[3]])[, 2]))

# get the 3,5,10th lowest obs
smallest_b <- influence_train %>%
  arrange(train) %>%
  head(10) %>%
  slice(c(3,5,10))

biggest_b <- influence_train %>%
  arrange(desc(train)) %>%
  head(10) %>%
  slice(c(3,5,10))

df_restricted <- df[-c(as.numeric(rownames(smallest_b)), as.numeric(rownames(biggest_b))),]

reg2 = lm("re78 ~ train + age + educ + black + hisp + re74 + re75", df_restricted)

stargazer(reg2, type='text')



# ------------
# ---EXE 2----
# ------------
df.2 <- read.csv("files/jtrain3.csv", sep = ";")
# View(df)

# a)

# subsetting for columns
cols <- c("age", "educ", "black", "hisp", "re74", "re75")
TABLE.2 <- df.2 %>%
  select(train, all_of(cols)) %>%
  group_by(train) %>%
  summarise(across(everything(), list(mean = mean, sd = sd)))

# calculate the differences in mean
results <- list()
for (col in cols) {
    results[[col]] <- diff_mean(df.2[col], df['train'])
}
results <- as.data.frame(do.call(cbind, results))

TABLE.2 <- cbind(TABLE.2, results)


TABLE.2_p <- TABLE.2 %>%
    pivot_longer(cols = !train, names_to = "temp", values_to = "value") %>%
    separate(temp, into = c("variable", "stat"), sep = "_(?=mean|sd|std_err)") %>%
    mutate(variable = gsub('\\..*', '', variable)) %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    pivot_wider(names_from = train, values_from = c(mean, sd))


TABLE_combined <- rbind(TABLE.1_p, TABLE.2_p)

# b)

set.seed(12345)

random_alloc <- function(df, col_name){
	vals <- rnorm(n=nrow(df))
	df["random_vals"] <- vals
	df_sorted  <- df |>
						arrange(random_vals)
	df_sorted[col_name] <- c(rep_len(1, (nrow(df)/2)),rep_len(0, (nrow(df)/2)), 0)
	df_sorted[-'random_vals']
}


df.2 <- random_alloc(df.2, 'treatment')

# c)
# TODO write down this func properly so it works 
#df.2 <- treatment_assign(df.2, share_control = 0.5, n_t=1, key='random_vals')

# d) 
# cor.test(df.2$treatment, df.2$treated_2)





