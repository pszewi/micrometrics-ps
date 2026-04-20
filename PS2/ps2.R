# ----------------
# setup
# ----------------
# Template of R script to answer problem set
# Group number: 
# Group composition: Madelief van Weerdenburg, Nicollo Zambello, Jakub Przewoski 

# Get the username
user <- Sys.info()["user"]
print(user)

# Define file path conditionally
if (user == "erick") {
    filepath <- "/home/erick/TEMP/"
} else if (user == "pszewi") {
    filepath <- "~/win/Documents/Studies/MSc Econ/subjects/2-semester/2-micrometrics/problem sets/PS2/"
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

# installing and importing libraries as needed 
cran_packages <- c("dplyr", "stargazer", "tidyr", "sandwich",
                   "lmtest", "boot", "hdm", "writexl", "grf",
                   "ggplot2", "modelsummary", "TwoWayFEWeights",
									 "fixest", 'bacondecomp')

for (pkg in cran_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ---------------
set.seed("12345")
# ---------------
# ---------------
# funcs
# ---------------


# ---------------
# EXE 1
# ---------------

df.1 <- read.csv("files/pset_2.csv", sep=';')
# a)
# b)
# c)
# d)
# e)
# f)
df_f <- tibble(obs = 1 : 6 )%>%
     mutate (
       state = floor( 0.9 + obs / 3 )
     )%>%
     group_by(state)%>%
     mutate(year = row_number())%>%
     ungroup() %>%
	mutate (
		D = as.numeric((state == 1 & year == 3) | (state == 2 & year %in% c(2, 3))),
#Creates simulated outcomes
Y = 0.1 + 0.02 * (year ==2) + 0.05 * (D ==1) + runif(n()) / 100,
Y2 = 0.1 + 0.02 * (year ==2) + 0.05 * (D ==1) + 0.3 * (state == 2 & year == 3 ) + runif(n()) / 100,
Y3 = 0.1 + 0.02 * (year ==2) + 0.05 * (D ==1) + 0.4 * (state == 2 & year == 3 ) + runif(n()) / 100,
Y4 = 0.1 + 0.02 * (year ==2) + 0.05 * (D ==1) + 0.5 * (state == 2 & year == 3 ) + runif(n()) / 100)

print(df_f)

summary(feols(Y ~ D | state + year, data = df_f))
summary(feols(Y2 ~ D | state + year, data = df_f))
summary(feols(Y3 ~ D | state + year, data = df_f))
summary(feols(Y4 ~ D | state + year, data = df_f))

# g)

twowayfeweights(data=df_f, "Y", "state", "year", "D")
twowayfeweights(data=df_f, "Y2", "state", "year", "D")
twowayfeweights(data=df_f, "Y3", "state", "year", "D")
twowayfeweights(data=df_f, "Y4", "state", "year", "D")

# I guess that since the weights seem to be the same across time, the difference stems from the comparison from the handling of the comparison between treated for one period and treated for periods before? I mean in the last one we get that both units are treated, I would expect the weights to change
# TODO: Check the weights

# h)

init_stpop <- df.1 |>
	filter(year=="1956") |>
	mutate(init_stpop=stpop) |>
	select(st, init_stpop)

df_h <- inner_join(df.1, init_stpop, by="st") |>
	mutate(IMP_UNILATERAL = ifelse(lfdivlaw <= year, 1, 0))

feols(div_rate ~ IMP_UNILATERAL | st + year, data=df_h,
			weights= ~init_stpop)

# TODO: For this we need to impute something here or drop all of the observations in that year to maintain a balanced panel

# bacon(div_rate ~ IMP_UNILATERAL, df_h,
# 			id_var = "st", time_var="year")
# TODO: to be finished

# i)

# j)
# k)
# l)

# ---------------
# LaTeX export helper (used in EXE 2 output/ files)
# ---------------
write_latex_table <- function(df, file, caption = NULL, label = NULL,
                              digits = 3, align = NULL) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  fmt <- function(x) if (is.numeric(x)) formatC(x, digits = digits, format = "f") else as.character(x)
  body <- as.data.frame(lapply(df, fmt), stringsAsFactors = FALSE, check.names = FALSE)
  esc  <- function(s) gsub("_", "\\\\_", s)
  header <- paste(esc(colnames(body)), collapse = " & ")
  rows   <- apply(body, 1, function(r) paste(esc(r), collapse = " & "))
  if (is.null(align)) align <- paste0("l", paste(rep("r", ncol(body) - 1), collapse = ""))
  out <- c(
    "\\begin{table}[!htbp]\\centering",
    if (!is.null(caption)) paste0("\\caption{", caption, "}"),
    if (!is.null(label))   paste0("\\label{", label, "}"),
    paste0("\\begin{tabular}{", align, "}"),
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    paste0(rows, " \\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}"
  )
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  writeLines(out, file)
}

# ---------------
# EXE 2
# ---------------

# a)

df.2 <- read.csv("files/expanded_data.csv")
summary(df.2)
# does the intro of unilateral divorce law have an effect on divorce rates at county level
table(df.2$year, df.2$lfdivlaw)

# drop treated states (adopted before pre-period)
df.2c <- df.2 |> filter(!(st %in% c("AK", "OK")))

# pivot wider to arrange as a difference in outcomes
df_wide <- df.2 |>
  pivot_wider(
    id_cols = c(st, county_id, lfdivlaw),
    names_from = year,
    values_from = c(div_rate_sim, education_rate, childcare_availability,
                    unemployment_rate, median_income, urbanization,
                    marriage_rate, religious_adherence, alcohol_consumption,
                    domestic_violence_rate, women_labor_force_participation,
                    housing_cost, crime_rate, social_services_spending)
  )

# outcome
Y.2 <- df_wide$div_rate_sim_1978 - df_wide$div_rate_sim_1968

# treatment
W.2 <- as.integer(df_wide$lfdivlaw > 1968 & df_wide$lfdivlaw <= 1978)

# covariates
covariate_cols <- grep("_1968$", names(df_wide), value = TRUE)
X.2 <- df_wide |>
  select(all_of(covariate_cols), -div_rate_sim_1968) |>
  mutate(urbanization_1968 = as.integer(urbanization_1968 == "Urban")) |>
  as.matrix()

# cluster variable (treatment assigned at state level)
clusters.2 <- as.integer(as.factor(df_wide$st))

# causal forest with clustering
# we cluster at the treatment assignment level to make sure our SEs are not too narrow 
cf <- causal_forest(
  X = X.2,
  Y = Y.2,
  W = W.2,
  clusters = clusters.2,
  honesty = TRUE
)

# computing the ATE
as.data.frame(average_treatment_effect(cf)) |> round(3)

# TODO: Treatment is insignificant - as in the case of the 1c. Moreover, we find sign reversion. This could be just due to random variation, but it could also be due to different handling of the weighting of CATEs, or estimation. Additionally, there could be differences due to the sample restrictions that we have seen. 


# ----
# Restricted sample
# ----

# Here I estimate the same effect for the restricted sample as in the exercise 1c to make sure that they are comparable - again the effect changes sign (here it is also smaller by 0.05 than the previous effect). This suggests that we in fact have heterogeneity in treatment effects, and that matching them on their covariates is important - after all we find an effect that is completely different.
# I.e. the regression in 1c did not incorporate these effects (in the same way) when estimating the ATE.

df_w_r <- df_wide |>
	filter(lfdivlaw %in% c(1969, 1970, 1971, 1972, 1973, 2000))

# outcome
Y.2_r <- df_w_r$div_rate_sim_1978 - df_w_r$div_rate_sim_1968

# treatment
W.2_r <- as.integer(df_w_r$lfdivlaw > 1968 & df_w_r$lfdivlaw <= 1973)

# covariates
covariate_cols <- grep("_1968$", names(df_w_r), value = TRUE)
X.2_r <- df_w_r |>
  select(all_of(covariate_cols), -div_rate_sim_1968) |>
  mutate(urbanization_1968 = as.integer(urbanization_1968 == "Urban")) |>
  as.matrix()

# cluster variable (treatment assigned at state level)
clusters.2_r <- as.integer(as.factor(df_w_r$st))

# causal forest with clustering
cf_r <- causal_forest(
  X = X.2_r,
  Y = Y.2_r,
  W = W.2_r,
  clusters = clusters.2_r,
  honesty = TRUE
)

# computing the ATE
average_treatment_effect(cf_r)

# ----
# End of restricted sample
# ----

# export ATE table (full + restricted)
ate_full <- average_treatment_effect(cf)
ate_rest <- average_treatment_effect(cf_r)
ate_df <- data.frame(
  Sample       = c("Full", "Restricted (1969-1973 vs 2000)"),
  Estimate     = c(ate_full["estimate"], ate_rest["estimate"]),
  `Std. Error` = c(ate_full["std.err"],  ate_rest["std.err"]),
  check.names  = FALSE
)
ate_df$`t value` <- ate_df$Estimate / ate_df$`Std. Error`
write_latex_table(
  ate_df, "output/ex2a_ate.tex",
  caption = "Causal forest ATE on $\\Delta$ divorce rate (1978$-$1968)",
  label   = "tab:ex2a_ate"
)



# b)

# i) best linear projection
best_linear_projection(cf, X.2)


# The best linear projection functions estimates a regression of the CATE_i on a constant and the set of covariates (doubly robust). This implies that under the assumption of a linear relation between the CATEs and the covariates, the best linear projection should show us which covariates have a significant effect on the CATEs. Intuitively, this means that it could inform us as to where our model exhibits most of the heterogeneity. 


# As far as I understand, this implies that the CATE is predicted by some of the covariates - this would suggest that some covariates would be inducing heterogeneity in the model. In that case, the model should exhibit heterogeneity given the values of the religious adherence, domestic_violence_rate and women_labour_force_participation.

# ii) targeting operator characteristic
tau_hat <- predict(cf)$predictions
toc <- rank_average_treatment_effect(cf, priorities = tau_hat)
plot(toc)

# export BLP + AUTOC together
blp_mat <- best_linear_projection(cf, X.2)
blp_df  <- data.frame(
  Covariate    = rownames(blp_mat),
  Estimate     = blp_mat[, "Estimate"],
  `Std. Error` = blp_mat[, "Std. Error"],
  `t value`    = blp_mat[, "t value"],
  `Pr(>|t|)`   = blp_mat[, "Pr(>|t|)"],
  check.names  = FALSE,
  row.names    = NULL
)
autoc_est <- as.numeric(toc$estimate[1])
autoc_se  <- as.numeric(toc$std.err[1])
autoc_row <- data.frame(
  Covariate    = "AUTOC (priorities = CATE)",
  Estimate     = autoc_est,
  `Std. Error` = autoc_se,
  `t value`    = autoc_est / autoc_se,
  `Pr(>|t|)`   = 2 * pnorm(-abs(autoc_est / autoc_se)),
  check.names  = FALSE
)
blp_toc_df <- rbind(blp_df, autoc_row)
write_latex_table(
  blp_toc_df, "output/ex2b_blp_toc.tex",
  caption = "Best Linear Projection of CATE on covariates, with AUTOC (Targeting Operator Characteristic area)",
  label   = "tab:ex2b_blp_toc"
)

# Note: We don't have to do any extra train/test splitting because "predict()" makes out-of-bag predictions

# The Targeting Operator Characteristic is a curve comparing the benefit of treating only a certain fraction
# q of units chosen by some priority rule (in our case it is the size of the predicted treatment effect), to the overall average treatment effect. Therefore, it should identify (if statistically significant) whether some units benefit more from treatment then others. 


# It seems that there is heterogeneity, given that the TOC is significant. This implies that the benefit of treating some units is higher than others (given their covariates). Thereore, this suggest heterogeneity of the CATEs.

# iii) plot dist of cate throughout important variables

# picking covariates suggested by the best linear projection as well as education (i thought  might be interesting)
plot_df <- data.frame(
  tau = tau_hat,
  education = X.2[, "education_rate_1968"],
  participation = X.2[, "women_labor_force_participation_1968"],
  religion = X.2[, "religious_adherence_1968"],
	violence = X.2[, "domestic_violence_rate_1968"]

)

# Faceted panel for multiple variables:
  plot_long <- plot_df |>
    pivot_longer(-tau, names_to = "variable", values_to = "value")

  ggplot(plot_long, aes(x = value, y = tau)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "loess", color = "blue", linewidth=1) +
		geom_hline(yintercept =0, linetype="dashed") + 
    facet_wrap(~variable, scales = "free_x") +
    labs(x = "Covariate Value", y = "Estimated CATE") +
    theme_minimal()
ggsave("output/plot_cate.png")

# The chosen variables were plotted and in fact confirm what was found in the previous exercises - CATEs seem to differ across the values of covariates for some variables.


# c)
#TODO: INTERPRET
# It appears that all three ways of analysing heterogeneity point to its existence. We see that that religious adherenece in a given county, it's women labour participation rate as well as the domestic violence rate are all significant at the conventional significance levels, implying that they can be used for predicting the CATEs.
# Moreover, the TOC is also statistically significant according to the chosen priority rule, implying that some units are impacted by the treatment more than others. 
# Lastly, the graphs above show the covariates chosen by the best linear projection and their effect on the CATEs.
# Therefore, I would argue that the heterogenous treatment effects are present in the data. 

# d)
cf_dishonest <- causal_forest(
  X = X.2,
  Y = Y.2,
  W = W.2,
  clusters = clusters.2,
  honesty = FALSE
)

# computing the ATE and BLP
average_treatment_effect(cf_dishonest)
best_linear_projection(cf_dishonest, X.2)

# export honesty vs no-honesty comparison (ATE + BLP side-by-side)
ate_h  <- average_treatment_effect(cf)
ate_nh <- average_treatment_effect(cf_dishonest)
blp_h  <- best_linear_projection(cf, X.2)
blp_nh <- best_linear_projection(cf_dishonest, X.2)

honesty_df <- data.frame(
  Covariate     = c("ATE", rownames(blp_h)),
  `Honest Est.` = c(ate_h["estimate"],  blp_h[, "Estimate"]),
  `Honest SE`   = c(ate_h["std.err"],   blp_h[, "Std. Error"]),
  `No-honesty Est.` = c(ate_nh["estimate"], blp_nh[, "Estimate"]),
  `No-honesty SE`   = c(ate_nh["std.err"],  blp_nh[, "Std. Error"]),
  check.names   = FALSE,
  row.names     = NULL
)
write_latex_table(
  honesty_df, "output/ex2d_honesty.tex",
  caption = "Effect of disabling honesty: ATE and BLP coefficients",
  label   = "tab:ex2d_honesty"
)

# toc
tau_hat_dishonest <- predict(cf_dishonest)$predictions
toc_dishonest <- rank_average_treatment_effect(cf_dishonest, priorities = tau_hat_dishonest)
plot(toc_dishonest)

plot_df <- data.frame(
  tau = tau_hat_dishonest,
 education = X.2[, "education_rate_1968"],
  participation = X.2[, "women_labor_force_participation_1968"],
  religion = X.2[, "religious_adherence_1968"],
	violence = X.2[, "domestic_violence_rate_1968"]

)

# Faceted panel for multiple variables:
  plot_long_dishonest <- plot_df |>
    pivot_longer(-tau, names_to = "variable", values_to = "value")

  ggplot(plot_long_dishonest, aes(x = value, y = tau)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "loess", color = "blue", linewidth=1) +
		geom_hline(yintercept =0, linetype="dashed") + 
    facet_wrap(~variable, scales = "free_x") +
    labs(x = "Covariate Value", y = "Estimated CATE") +
    theme_minimal()
ggsave("output/plot_cate_dishonest.png")

# Honesty is the procedure of splitting the data, such that the no tree in the random forest uses a single observation/sample for both placing the splits (building the tree) and predicting the values of the CATEs. 
# It is important for the computation of the average treatment effect, because it allows for the unbiased estimation of the point estimates and the variance. Without honesty, we would find that the bias of the estimator is not bound, and that the condifence intervals of the coefficients would be too small, therefore destroying inference. 
# This problem is related to the fact that our data would be used twice - both for creation of the tree and for the estimation of the CATEs

# TODO: WRITE UP TEXT - WHY IMPORTANT FOR COMPUTING ATEs
#TODO: INTERPRET
