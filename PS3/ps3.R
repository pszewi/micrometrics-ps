# ----------------
# setup
# ----------------
# Template of R script to answer problem set
# Group number: 30
# Group composition: Madelief van Weerdenburg, Nicollo Zambello, Jakub Przewoski

# Get the username
user <- Sys.info()["user"]
print(user)

# Define file path conditionally
if (user == "erick") {
  filepath <- "/home/erick/TEMP/"
} else if (user == "pszewi") {
  filepath <- "~/win/Documents/Studies/MSc Econ/subjects/2-semester/2-micrometrics/problem sets/PS3/"
} else if (user == "bogiano1945") {
  filepath <- "/Users/bogiano1945/Desktop"
} else if (user == "C") {
  filepath <- "/FILE/PATH/C/"
} else {
  filepath <- "" # Default case if user is not listed
}

setwd(filepath)
# Print the selected file path
print(paste("File path set to:", filepath))

# ----------------
# imports
# ---------------

# installing and importing libraries as needed
cran_packages <- c(
  "dplyr", "stargazer", "tidyr", "sandwich",
  "lmtest", "openxlsx", "grf",
  "ggplot2", "modelsummary", "rdrobust",
  "rddensity", "lpdensity", "patchwork",
  "cowplot", "gridGraphics",
  "haven", "fixest", "stargazer"
)

for (pkg in cran_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# custom little utility for making tables (stored in a separate file)
source("rdrobust_modelsummary.R")
# ---------------
set.seed("12345")
# ---------------
# ---------------
# funcs
# ---------------


# ---------------
# EXE 1
# ---------------
df1 <- read.csv("stata_files/pset_3.csv", sep = ";")

# ---- a) -------
exe1a <- rdplot(
  y = df1$T, x = df1$X, x.label = "Running variable",
  y.label = "Treatment Variable"
)
exe1a
ggsave("output/exe1a.png", exe1a$rdplot)


# Clearly seems sharp, as there are only 2 bins, one below cutoff
# and one above cutoff. Since there are no observations above the
# cutoff that did not adopt the treatment, then this is a sharp RD design.

# ---- b) -------

covariates <- c(
  "hischshr1520m", "i89", "vshr_islam1994", "partycount",
  "lpop1994", "merkezi", "merkezp", "subbuyuk", "buyuk"
)

results_b <- sapply(covariates, function(var) {
  est <- rdrobust(y = df1[[var]], x = df1$X)
  c(
    band = round(est$bws[1, 1], 3),
    tau = round(est$coef[1], 3),
    pval = round(est$pv[1], 3),
    obsnum = sum(est$N_h[1], est$N_h[2])
  )
})

table_1 <- t(results_b)
colnames(table_1) <- c(
  "MSE-Optimal Band.", "RD Estimator", "p-value",
  "eff. num. of obs."
)
table_1 <- cbind(Label = rownames(table_1), table_1)
rownames(table_1) <- 1:nrow(table_1)
print(table_1)


hs <- createStyle(
  textDecoration = "BOLD", fontName = "Arial Narrow"
)

stargazer(table_1,
  type = "latex", out = "output/Table_1.tex",
  title = "Exercise 1b (table)"
)

# ---- c) -------
titles <- c(
  hischshr1520m = "Male High School Share(15–20)",
  i89 = "Islamic Mayor 1989",
  vshr_islam1994 = "Islamic Vote Share(1994)",
  partycount = "# of Parties Receiving votes (1994)",
  lpop1994 = "Log Population (1994)",
  merkezi = "District Center",
  merkezp = "Province Center",
  subbuyuk = "Sub-Metro Center",
  buyuk = "Metro Center"
)

plots <- lapply(covariates, function(v) {
  invisible(capture.output(
    p <- rdplot(df1[[v]], df1$X, c = 0)$rdplot
  ))
  p + ggplot2::labs(title = titles[v])
})

wrap_plots(plots, ncol = 3)

plots2 <- wrap_plots(plots, ncol = 3)


dpi <- 150
png(filename = "output/Graph_1.png", width = (650 * (dpi / 72)), height = (450 * (dpi / 72)), res = dpi, bg = "white")
plots2
dev.off()


# ---- d) -------

# Histograms of observations to the left and right of the density
band <- rdrobust(df1$Y, df1$X)
h_l <- -band$bws[1]
h_r <- band$bws[1]

Histogram <- function() {
  hist(df1$X[df1$X >= h_l & df1$X < 0],
    col = "blue",
    breaks = 10, xlim = c(-30, 30),
    main = "Histogram around cutoff",
    xlab = "Islamic vote margin in 1994 ",
    ylab = "Frequency"
  )
  hist(df1$X[df1$X >= 0 & df1$X <= h_r],
    col = "red",
    breaks = 10, add = TRUE
  )
}
Histogram()

# Plot of density of X
rdd_d <- rddensity(X = df1$X)
p_density <- rdplotdensity(rdd = rdd_d, X = df1$X)$Estplot

p_density_2 <- {
  p_density +
    labs(
      title = "Density of running variable",
      x = "Score (centered at cutoff)",
      y = "Density"
    )
}
p_density_2

plots_1d <- plot_grid(
  ~ Histogram(),
  p_density_2,
  ncol = 2
)

dpi <- 150
png(filename = "output/Graph_2.png", width = (650 * (dpi / 72)), height = (450 * (dpi / 72)), res = dpi, bg = "white")
plots_1d
dev.off()

# ---- e) -------

disc_test <- data.frame(
  "T Stat." = rdd_d$test$t_jk,
  "P-value" = rdd_d$test$p_jk
)
t(disc_test)


table_Q1e <- t(disc_test)
stargazer(table_Q1e, type = "latex", out = "output/Table_Q1e.tex",
					title="Exercise 1e")


# Rddensity tests whether there exists a discontinuity in the running variable.
# Here, the test fails to reject the null of continuity in the running variable’s
# density at the cutoff (p = 0.163), indicating no evidence of manipulation.
# This supports the validity of the RD design, as it does not provide any evidence
# of manipulation of the running variable around the cutoff.

# ---- f) -------

cutoffs <- seq(-10, 10, by = 5)
cutoffs <- cutoffs[cutoffs != 0]
placebo_results <- lapply(cutoffs, function(c) {
  subset <- if (c < 0) df1[df1$X < 0, ] else df1[df1$X >= 0, ]
  est <- rdrobust(y = subset$Y, x = subset$X, c = c)
  ci <- est$ci
  c(cutoff = c, tau = est$coef[1], ci_low = ci[1], ci_high = ci[4])
})
placebo_df <- as.data.frame(do.call(rbind, placebo_results))
plot_Q1f <- ggplot(placebo_df, aes(x = cutoff, y = tau)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal()
plot_Q1f

dpi <- 150
png(filename = "output/Graph_Q1f.png", width = (650 * (dpi / 72)), height = (450 * (dpi / 72)), res = dpi, bg = "white")
plot_Q1f
dev.off()

# In this part we test if alternative discontinuities exist using placebo RD tests.
# If the RD is valid, then the discontinuity should only appear at the true cutoff,
# which is zero. At other cutoffs (-10, -5, 5, and 10) the estimated treatment
# effect should be close to zero and not statistically significant. In the plot made
# by the code above, it is clear that none of these 4 placebo cutoffs are statistically
# different from zero, and thus we do not find evidence of discontinuities at
# alternative cutoffs.

# ---- g) -------
exe1g <- rdplot(
  y = df1$Y, x = df1$X, x.label = "Running variable",
  y.label = "Outcome",
  nbins = 40,
  kernel = "triangular",
)
ggsave("output/exe1g.png", exe1g$rdplot)


# ---- h) -------
# Triangular kernel is the default
# It gives more weight to observations close to the cutoff
# and less weight to observations close to the boundary
summary(rdrobust(y = df1$Y, x = df1$X, all = TRUE))

# Answer: We can see that with the triangular kernel,
# the effect is significant at the 0.05 significance level.

# Uniform kernel that treats every observation with the same weight
summary(rdrobust(y = df1$Y, x = df1$X, kernel = "uniform", all = TRUE))

# With the uniform kernel, we get that the effect is significant at the 5%
# level as well.

# Generally the results differ, because of the kernel choice. The kernel
# defines in what way the observations are weighted in relation to their
# distance from the cutoff. With the uniform kernel, we have that the
# observations are weighted in the same way, while the triangular kernel
# discounts observations further away from the cutoff.
# In this case, we can see that there must be some observations further
# away from the cutoff that have more extreme values, therefore making
# the solution more significant.

# export
models_1h <- list(
  Triangular = rdrobust(y = df1$Y, x = df1$X, all = TRUE),
  Uniform = rdrobust(y = df1$Y, x = df1$X, kernel = "uniform", all = TRUE)
)

rdrobust_modelsummary(
  models_1h,
  estimate_types = "Conventional",
  output = "output/exe1h.tex",
	title = "Exercise 1h"
)


# saving the optimal bandwidth
opt_i <- rdbwselect(y = df1$Y, x = df1$X)$bws[1, 1]

# ---- i) -------


# Btw this is already the centered version because our cutoff is 0!
df1$X2 <- df1$X^2
df1$X3 <- df1$X^3
df1$X4 <- df1$X^4
df1$XT <- df1$T * df1$X
df1$XT2 <- df1$T * df1$X2
df1$XT3 <- df1$T * df1$X3
df1$XT4 <- df1$T * df1$X4

exe1i <- lm(Y ~ T + X + X2 + X3 + X4 + XT + XT2 + XT3 + XT4, data = df1)
summary(exe1i)

stargazer(exe1i, out="output/exe1i.tex", type="latex", title = "Exercise 1i")

# ---- j) -------

# implementing the weights for the triangular kernel
df1 <- df1 |>
	mutate(
		wght = ifelse(abs((X/opt_i)) <= 1, 1 - abs((X/opt_i)), 0)
	)

subsample <- df1 |> filter((X < opt_i) & (X > -opt_i))

exe1j_u <- lm(" Y ~ X + T + as.factor(T):X", data = subsample)
summary(exe1j_u)
exe1j_k <- lm(" Y ~ X + T + as.factor(T):X", data = subsample, weights=wght)
summary(exe1j_k)

stargazer(exe1j_u, exe1j_k, out="output/exe1j.tex", type="latex", title = "Exercise 1j")

# Answer: 
# exe1j_u essentiall estimates an unweighted uniform kernel regression
# since we have that all observations have equal weight, but that weight
# is 1. Therefore, the value of the coefficient differs from the value off
# the value of the coefficient in h). 
# exe1j_k essentially estimates the triangular kernel regression and  
# thus returns the same value as 
# summary(rdrobust(y = df1$Y, x = df1$X, all = TRUE))


# ---- k) -------

bnds <- seq(0.5, 1.5, by = 0.25)

results_k_trian <- lapply(bnds, function(var) {
  bndwdth <- var * opt_i
  est <- rdrobust(y = df1$Y, x = df1$X, h = c(bndwdth, bndwdth))
  ci <- est$ci
  c(
    tau = est$coef[1],
    ci_low = ci[1], ci_high = ci[4]
  )
})

results_k_trian

results_k_unif <- lapply(bnds, function(var) {
  bndwdth <- var * opt_i
  est <- rdrobust(
    y = df1$Y, x = df1$X,
    h = c(bndwdth, bndwdth), kernel = "uniform"
  )
  ci <- est$ci
  c(
    tau = est$coef[1],
    ci_low = ci[1], ci_high = ci[4]
  )
})

results_k_unif


to_df <- function(res, kernel_name) {
  out <- as.data.frame(do.call(rbind, res))
  names(out) <- c("tau", "ci_low", "ci_high")
  out$mult <- bnds
  out$kernel <- kernel_name
  out
}

plot_df <- bind_rows(
  to_df(results_k_trian, "Triangular"),
  to_df(results_k_unif, "Uniform")
)

# lock the row order so triangular sits on top
plot_df$kernel <- factor(plot_df$kernel, levels = c(
  "Triangular",
  "Uniform"
))

ggplot(plot_df, aes(x = mult, y = tau)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(ymin = ci_low, ymax = ci_high)) +
  facet_wrap(~kernel, ncol = 1) +
  labs(
    x = "Bandwidth multiplier (× opt_i)",
    y = expression(hat(tau)),
    title = "RD estimate across bandwidths, by kernel"
  ) +
  theme_minimal()

ggsave("output/graph_3.png")

# We can see that as we relax the bandwidth, our results become more significant
# This is consistent with the idea that as we relax the bandwith, our
# observations may become less comparable, which decreases our internal
# viability. This is especially visible for the estimation with the uniform
# kernel, where we see that the model becomes significant faster, which is
# explained by the comment in exercise h)

# ---------------
# EXE 2
# ---------------

df2 <- read_dta("stata_files/fraud_pcenter_final.dta") |>
  mutate(
    dst = as.numeric(`_dist`),
    dist = dst,
    temp = if_else(cov == 0, -dist, dist),
    instr = if_else(temp >= 0, 1, 0),
  ) |>
  filter(conflict != 1)


# ----------------
# ---- a) -------
# ----------------

# RD plot of treatment status against the proxy-based running variable

exe2a <- rdplot(df2$cov, df2$temp,
  kernel = "triangular",
  x.label = "Running Variable",
  y.label = "Treatment Variable",
  title = "Treatment Variable against Proxy-Based Running Variable"
)

ggsave("output/exe2a.png", exe2a$rdplot)


summary(rdrobust(df2$cov, df2$temp))
# the treatment rd together and export
models <- list(
  Treatment = rdrobust(y = df2$cov, x = df2$temp)
)

rdrobust_modelsummary(
  models,
  estimate_types = "Conventional",
  output = "output/exe2a_table.tex",
	title = "Exercise 2a (table)"
)


# Answer:
# This is a fuzzy rdd as we have that after the cutoff the treatment does not
# switch to 1 with probability equal to 1.
# Note that due to the error, we have that some observations below the cutoff
# already start being treated (have coverage), likely due to measurement error
# of the proxy.

# The validity of the spatial rdd relies on the classic assumptions:
# The potential outcome functions must be continuous in the treatment boundary
# The probability of treatment must jump at the cutoff and there must be no
# manipulation around it.
# Also, for every part of the boundary (i.e. for every segment), there must
# be at least one polling center on each side of the boundary so that we can
# carry out our comparison

# ----------------
# ---- b) -------
# ----------------

# Answer:
# Question: When does having a proxy for longitude keep the design sharp?
# I guess the design will stay sharp if the error from the proxy does not
# affect the distance, i.e., if we knew that the longitude of the boundary
# doesn't change within the sample (therefore would not be a problem,
# because there would be not much variation in the distance)
# In other words, we would have to be moving along a longitude line,
# so that minimal variation in the longitude does not have a large effect
# on the results.

# ----------------
# ---- c) -------
# ----------------


# Defining the outcomes, getting the optimal bandwidth and then estimating
# the RDDs as regressions with fixed effects as specified by the author
# (but fuzzy instead of sharp)

outcomes <- c("comb_ind", "comb")

# Function to get optimal bandwidth
get_bw <- function(y, data) {
  bw <- rdbwselect(
    y = data[[paste0("vote_", y)]],
    x = data$temp,
    cluster = data$segment50
  )
  as.numeric(bw$bws[1, 1])
}

hopt <- list()
means <- list()

# Getting the optimal bandwith
for (v in outcomes) {
  d_all <- df2 |> filter(ind_seg50 == 1)

  hopt[[v]] <- get_bw(v, d_all)

  for (r in 1:2) {
    hopt[[paste0(v, "_", r)]] <- get_bw(
      v,
      d_all |> filter(region2 == r)
    )
  }

  means[[v]] <- df2 |>
    filter(cov == 0, ind_seg50 == 1, dist <= hopt[[v]]) |>
    summarise(m = mean(.data[[paste0("vote_", v)]], na.rm = TRUE)) |>
    pull(m)

  for (r in 1:2) {
    means[[paste0(v, "_", r)]] <- df2 |>
      filter(
        cov == 0,
        ind_seg50 == 1,
        region2 == r,
        abs(temp) <= hopt[[paste0(v, "_", r)]]
      ) |>
      summarise(m = mean(.data[[paste0("vote_", v)]], na.rm = TRUE)) |>
      pull(m)
  }
}


# Making the table
star_code <- function(p_value) {
  case_when(
    p_value < 0.01 ~ "***",
    p_value < 0.05 ~ "**",
    p_value < 0.10 ~ "*",
    TRUE ~ ""
  )
}

# Since the rdd is fuzzy, we have to run it with an iv approach

fuzzy_iv_results <- list()

fit_fuzzy_iv <- function(sample_data, outcome, bandwidth) {
  yvar <- paste0("vote_", outcome)
  d <- sample_data |>
    filter(
      .data$ind_seg50 == 1,
      abs(.data$temp) <= bandwidth,
      !is.na(.data[[yvar]]),
      !is.na(.data$cov),
      !is.na(.data$temp),
      !is.na(.data$segment50)
    )

  # here we run the iv fuzzy regression with fixed effects, as in the paper.
  # By including cov + cov:temp in the endog. part, and running the 2nd stage
  # with just temp, we get that the final specification is
  # y ~ cov_fit + temp + cov_fit:temp (i.e. correct)
  feols(
    as.formula(paste0(yvar, " ~ temp | segment50 | cov + cov:temp ~ instr + instr:temp")),
    data = d,
    cluster = ~segment50
  )
}

# quick test of the first and second stage for all regions
summary(fit_fuzzy_iv(sample_data = df2, outcome = "comb_ind", bandwidth = hopt[["comb_ind"]]), stage = 1)
summary(fit_fuzzy_iv(sample_data = df2, outcome = "comb_ind", bandwidth = hopt[["comb_ind"]]), stage = 2)


# running the loop for everything
for (v in outcomes) {
  fuzzy_iv_results[[paste0("iv_all_", v)]] <- fit_fuzzy_iv(
    sample_data = df2,
    outcome = v,
    bandwidth = hopt[[v]]
  )

  fuzzy_iv_results[[paste0("iv_region1_", v)]] <- fit_fuzzy_iv(
    sample_data = df2 |> filter(region2 == 1),
    outcome = v,
    bandwidth = hopt[[paste0(v, "_1")]]
  )

  fuzzy_iv_results[[paste0("iv_region2_", v)]] <- fit_fuzzy_iv(
    sample_data = df2 |> filter(region2 == 2),
    outcome = v,
    bandwidth = hopt[[paste0(v, "_2")]]
  )
}

iv_cell <- function(model) {
  ct <- coeftable(model)
  estimate <- ct["fit_cov", "Estimate"]
  se <- ct["fit_cov", "Std. Error"]
  p_value <- ct["fit_cov", "Pr(>|t|)"]

  paste0(
    sprintf("%.3f", estimate),
    star_code(p_value),
    " (",
    sprintf("%.3f", se),
    ")"
  )
}

fuzzy_iv_table <- tibble(
  variable = c("Indicator", "Vote Share"),
  `All regions` = c(
    iv_cell(fuzzy_iv_results[["iv_all_comb_ind"]]),
    iv_cell(fuzzy_iv_results[["iv_all_comb"]])
  ),
  `Region 1` = c(
    iv_cell(fuzzy_iv_results[["iv_region1_comb_ind"]]),
    iv_cell(fuzzy_iv_results[["iv_region1_comb"]])
  ),
  `Region 2` = c(
    iv_cell(fuzzy_iv_results[["iv_region2_comb_ind"]]),
    iv_cell(fuzzy_iv_results[["iv_region2_comb"]])
  )
)

print(fuzzy_iv_table)

fuzzy_iv_latex <- c(
  "\\begin{tabular}{lccc}",
  "\\hline\\hline",
  "Variable & All regions & Region 1 & Region 2 \\\\",
  "\\hline",
  paste0(
    fuzzy_iv_table$variable,
    " & ",
    fuzzy_iv_table$`All regions`,
    " & ",
    fuzzy_iv_table$`Region 1`,
    " & ",
    fuzzy_iv_table$`Region 2`,
    " \\\\"
  ),
  "\\hline",
  "\\multicolumn{4}{l}{\\footnotesize IV estimates: cov instrumented by 1\\{temp $\\ge$ 0\\}} \\\\",
  "\\multicolumn{4}{l}{\\footnotesize Segment50 fixed effects included; clustered standard errors by segment50 in parentheses.} \\\\",
  "\\multicolumn{4}{l}{\\footnotesize * p $<$ 0.10, ** p $<$ 0.05, *** p $<$ 0.01.} \\\\",
  "\\hline\\hline",
  "\\end{tabular}"
)

writeLines(fuzzy_iv_latex, "output/exe2c.tex")

# Answer:
# Under the assumption of local monotonicity (needed for the IV interpretation),
# we get that this IV should identify the LATE for people at the boundary.
# However, this identification could be compromised due to possible measurement
# error (since _dist variable here is a proxy).
# The interpretation of that effect would be the effect of 2G coverage on
# election fraud. From the generated table, one can see that the estimates
# are generally smaller than the original estimates. "All regions" become
# more negative with only the Indicator coefficients being significant.
# For "Region 1" (Southeast), both coefficients became more negative
# For the "Region 2" (Northeast), coefficients shrunk more towards 0,
# with the Vote Share coefficient switching sign.
# Generally, it should be noted that all coefficients are at most barely
# significant at the 10% level in this setting
# and that the Southeast region is still driving those results.
