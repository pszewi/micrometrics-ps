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
  "rddensity", "lpdensity", "patchwork"
)

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
df1 <- read.csv("files/pset_3.csv", sep = ";")

# ---- a) -------
rdplot(
  y = df1$T, x = df1$X, x.label = "Running variable",
  y.label = "Treatment Variable"
)


# Clearly seems sharp, as there are only 2 bins, one below cutoff
# and one above cutoff. Since there are no observations above the
# cutoff that did not adopt the treatment, then this is a sharp RD design.

# ---- b) -------

# TODO: Some things from the tutorial that need to be adjusted to work!
covariates <- c(
  "hischshr1520m", "i89", "vshr_islam1994", "partycount",
  "lpop1994", "merkezi", "merkezp", "subbuyuk", "buyuk"
)

results_b <- sapply(covariates, function(var) {
  est <- rdrobust(y = df1[[var]], x = df1$X)
  c(
    band = est$bws[1, 1],
    tau = round(est$coef[1], 3),
    pval = round(est$pv[3], 3),
    obsnum = sum(est$N_h[1], est$N_h[2])
  )
})

table_1 <- t(results)
colnames(table_1) <- c(
  "MSE-Optimal Bandwidth", "RD Estimator", "p-value",
  "effective number of obervations"
)
print(table_1)

# TODO: export

# ---- c) -------


plots <- lapply(covariates, function(v) {
  invisible(capture.output(
    p <- rdplot(df1[[v]], df1$x, c = 0, title = v)$rdplot
  ))
  p
})

wrap_plots(plots, ncol = 3)

# TODO: title the graphs
# TODO: export

# ---- d) -------

# Histograms of observations to the left and right of the density
band <- rdrobust(df1$Y, df1$X)
h_l <- -band$bws[1]
h_r <- band$bws[1]
hist(df1$X[df1$X >= h_l & df1$X < 0],
  col = "blue",
  breaks = 10, xlim = c(-30, 30)
)
hist(df1$X[df1$X >= 0 & df1$X <= h_r], col = "red", breaks = 10, add = TRUE)


# Plot of density of X
rdd_d <- rddensity(X = df1$X)
rdplotdensity(rdd = rdd_d, X = df1$X)

# TODO: Polish, combine and export

# ---- e) -------

disc_test <- data.frame(
  "T Stat." = rdd$test$t_jk,
  "P-value" = rdd$test$p_jk
)

# Rddensity tests whether there exists a discontinuity in the running variable
# TODO: by means of ....

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
ggplot(placebo_df, aes(x = cutoff, y = tau)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal()

# TODO: write the text

# ---- g) -------
rdplot(
  y = df1$Y, x = df1$X, x.label = "Running variable",
  y.label = "Outcome",
  nbins = 40
)
# TODO: Verify whether the correct plot

# ---- h) -------
# TODO: for now I'm running that on X but it says that we should run it on T,
# but T is not continuous - it's a dummy

# Triangular kernel is the default
# It gives more weight to observations close to the cutoff
# and less weight to observations close to the boundary
summary(rdrobust(y = df1$Y, x = df1$X))
# Uniform kernel that treats every observation with the same weight
summary(rdrobust(y = df1$Y, x = df1$X, kernel = "uniform"))

# TODO: Describe the differences, talk about the interpretation
# TODO: Verify if by the "linear polynomial" they just meant the
# default regression, or whether you should adjust the p/q parameters!

# ---- i) -------

# Btw this is already the centered version because our cutoff is 0!
df1$X2 <- df1$X^2
df1$X3 <- df1$X^3
df1$X4 <- df1$X^4
df1$XT <- df1$T * df1$X
df1$XT2 <- df1$T * df1$X2
df1$XT3 <- df1$T * df1$X3
df1$XT4 <- df1$T * df1$X4

summary(lm(Y ~ T + X + X2 + X3 + X4 + XT + XT2 + XT3 + XT4, data = df1))


# ---- j) -------

opt_i <- rdbwselect(y = df1$Y, x = df1$X)$bws[1, 1]

subsample <- df1 |> filter((X < opt_i) & (X > -opt_i))

summary(lm("Y~X+T", data = subsample))

# TODO: figure out whether this is actually correct,
# ---- k) -------

bnds <- seq(0.5, 1.5, by = 0.25)

results_j_trian <- lapply(bnds, function(var) {
  bndwdth <- var * opt_i
  est <- rdrobust(y = df1$Y, x = df1$X, h = c(bndwdth, bndwdth))
  ci <- est$ci
  c(
    tau = est$coef[1],
    ci_low = ci[1], ci_high = ci[4]
  )
})

results_j_trian

results_j_unif <- lapply(bnds, function(var) {
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

results_j_unif


to_df <- function(res, kernel_name) {
  out <- as.data.frame(do.call(rbind, res))
  names(out) <- c("tau", "ci_low", "ci_high")
  out$mult <- bnds
  out$kernel <- kernel_name
  out
}

plot_df <- bind_rows(
  to_df(results_j_trian, "Triangular"),
  to_df(results_j_unif, "Uniform")
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

#TODO: Write the interpretations!

# ---------------
# EXE 2
# ---------------


# ---- a) -------
# ---- b) -------
# ---- c) -------
