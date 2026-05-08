# ----------------
# setup hello
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
  "xlsx", "openxlsx", "cowplot", "gridGraphics"
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
df1 <- read.csv("stata_files/pset_3.csv", sep = ";")

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

table_1 <- t(results_b)
colnames(table_1) <- c(
  "MSE-Optimal Bandwidth", "RD Estimator", "p-value",
  "effective number of obervations"
)
print(table_1)

hs <- createStyle(
  textDecoration = "BOLD", fontName = "Arial Narrow"
)

openxlsx::write.xlsx(table_1, "Table_1.xlsx", 
                     rowNames = T, headerStyle = hs, colWidths="auto")

# ---- c) -------
titles <- c(
  hischshr1520m = "Male High School Share(15–20)",
  i89           = "Islamic Mayor 1989",
  vshr_islam1994 = "Islamic Vote Share(1994)",
  partycount    = "# of Parties Receiving votes (1994)",
  lpop1994      = "Log Population (1994)",
  merkezi       = "District Center",
  merkezp       = "Province Center",
  subbuyuk      = "Sub-Metro Center",
  buyuk         = "Metro Center"
)

plots <- lapply(covariates, function(v) {
  invisible(capture.output(
    p <- rdplot(df1[[v]], df1$x, c = 0)$rdplot
  ))
  p + ggplot2::labs(title = titles[v])
})

wrap_plots(plots, ncol = 3)

plots2 <- wrap_plots(plots, ncol = 3)


dpi = 150
png(filename = "Graph_1.png", width= (650 * (dpi/72)), height= (450 * (dpi/72)), res = dpi, bg = "white")
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
     breaks = 10, add = TRUE)
}
Histogram()

# Plot of density of X
rdd_d <- rddensity(X = df1$X)
p_density <- rdplotdensity(rdd = rdd_d, X = df1$X)$Estplot

p_density_2 <- {p_density +
  labs(title = "Density of running variable",
       x = "Score (centered at cutoff)",
       y = "Density")}
p_density_2

plots_1d <- plot_grid(
  ~Histogram(), 
  p_density_2,
  ncol = 2
)

dpi = 150
png(filename = "Graph_2.png", width= (650 * (dpi/72)), height= (450 * (dpi/72)), res = dpi, bg = "white")
plots_1d
dev.off()

# ---- e) -------

disc_test <- data.frame(
  "T Stat." = rdd_d$test$t_jk,
  "P-value" = rdd_d$test$p_jk
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
  nbins = 40,
)

rdplot(
  y = df1$Y, x = df1$X, x.label = "Running variable",
  y.label = "Outcome",
  nbins = 40,
	kernel="triangular",
)


# ---- h) -------
# TODO: for now I'm running that on X but it says that we should run it on T,
# but T is not continuous - it's a dummy

# Triangular kernel is the default
# It gives more weight to observations close to the cutoff
# and less weight to observations close to the boundary
summary(rdrobust(y = df1$Y, x = df1$X))

# Answer: We can see that with the triangular kernel,
# the effect is only significant at the 10% level, but not other conventional
# significance levels. 

# Uniform kernel that treats every observation with the same weight
summary(rdrobust(y = df1$Y, x = df1$X, kernel = "uniform"))

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

df2 <- read.csv(
  "stata_files/fraud_pcenter_final.csv",
  sep = ";",
  check.names = FALSE,
  fileEncoding = "latin1",
  na.strings = c("", ".", "NA")
)

# In some imports, "_dist" may become "X_dist"
run_var <- intersect(c("_dist", "X_dist"), names(df2))[1]
if (length(run_var) == 0 || is.na(run_var)) {
  stop("Proxy-based signed running variable not found. Check whether the column is named '_dist' or 'X_dist'.")
}

# Keep the same estimation sample used in the one-dimensional analysis
df2 <- df2 |>
  filter(conflict == 0, ind_seg50 == 1) |>
  mutate(
    run = .data[[run_var]],
    post = as.integer(run >= 0)
  )

# ----------------
# helper functions
# ----------------

# MSE-optimal bandwidth for fuzzy RD
get_h_fuzzy <- function(data, yvar) {
  d <- data |>
    filter(!is.na(.data[[yvar]]), !is.na(cov), !is.na(run))
  
  bw <- rdbwselect(
    y = d[[yvar]],
    x = d$run,
    fuzzy = d$cov,
    c = 0,
    p = 1,
    kernel = "triangular",
    bwselect = "mserd"
  )
  
  as.numeric(bw$bws[1, 1])
}

# Fuzzy RD point estimate with segment fixed effects:
# Wald estimate = reduced-form jump / first-stage jump
fuzzy_rd_fe <- function(data, yvar) {
  d <- data |>
    filter(!is.na(.data[[yvar]]), !is.na(cov), !is.na(run))
  
  h <- get_h_fuzzy(d, yvar)
  
  d_loc <- d |>
    filter(abs(run) <= h)
  
  rf <- lm(
    as.formula(
      paste0(yvar, " ~ post + run + post:run + factor(segment50)")
    ),
    data = d_loc
  )
  
  fs <- lm(
    cov ~ post + run + post:run + factor(segment50),
    data = d_loc
  )
  
  tau_rf <- unname(coef(rf)["post"])
  tau_fs <- unname(coef(fs)["post"])
  
  data.frame(
    outcome = yvar,
    bandwidth = h,
    n = nobs(rf),
    reduced_form = tau_rf,
    first_stage = tau_fs,
    point_estimate = tau_rf / tau_fs
  )
}

# ----------------
# ---- a) -------
# ----------------

# RD plot of treatment status against the proxy-based running variable
png("pset_3_ex2_a_treatment_proxy_rdplot.png", width = 900, height = 700)
rdplot(
  y = df2$cov,
  x = df2$run,
  c = 0,
  p = 1,
  kernel = "triangular",
  x.label = "Running Variable",
  y.label = "Treatment Variable",
  title = "Treatment Variable against Proxy-Based Running Variable"
)
dev.off()

# First-stage RD estimate (treatment on the proxy-based running variable)
first_stage_rd <- rdrobust(
  y = df2$cov,
  x = df2$run,
  c = 0,
  p = 1,
  kernel = "triangular",
  bwselect = "mserd"
)

first_stage_table <- data.frame(
  Conventional_Estimate = round(first_stage_rd$coef[1], 4),
  Conventional_SE = round(first_stage_rd$se[1], 4),
  Conventional_pvalue = round(first_stage_rd$pv[1], 4),
  Bandwidth_Left = round(first_stage_rd$bws[1, 1], 4),
  Bandwidth_Right = round(first_stage_rd$bws[1, 2], 4)
)

print(first_stage_table)

openxlsx::write.xlsx(
  first_stage_table,
  "pset_3_ex2_a_first_stage.xlsx",
  rowNames = FALSE,
  overwrite = TRUE
)

# Comment:
# This is a fuzzy RD: the proxy-based score does not pin down coverage perfectly.
# The design is valid if potential outcomes are continuous at the cutoff, treatment
# probability jumps at zero, and there is no precise sorting/manipulation there
# (plus local monotonicity if we want a fuzzy-RD/LATE interpretation).

# ----------------
# ---- b) -------
# ----------------

# Comment:
# If the coverage boundary were approximately horizontal (east-west / constant latitude),
# distance to the boundary would depend almost only on latitude. Then using only a proxy
# for longitude would barely distort the running variable, so the RD design would remain
# essentially the same as in Gonzalez (2021).

# ----------------
# ---- c) -------
# ----------------

# Partial replication of Columns 1, 3 and 5 of Table 2 under the proxy-longitude setting
res_c <- bind_rows(
  fuzzy_rd_fe(df2, "vote_comb_ind") |> mutate(sample = "All regions"),
  fuzzy_rd_fe(filter(df2, region2 == "East"), "vote_comb_ind") |> mutate(sample = "Southeast region"),
  fuzzy_rd_fe(filter(df2, region2 == "North"), "vote_comb_ind") |> mutate(sample = "Northwest region"),
  fuzzy_rd_fe(df2, "vote_comb") |> mutate(sample = "All regions"),
  fuzzy_rd_fe(filter(df2, region2 == "East"), "vote_comb") |> mutate(sample = "Southeast region"),
  fuzzy_rd_fe(filter(df2, region2 == "North"), "vote_comb") |> mutate(sample = "Northwest region")
)

table_2_proxy <- res_c |>
  mutate(
    outcome = recode(
      outcome,
      vote_comb_ind = "At least one station with Category C fraud",
      vote_comb = "Share of votes under Category C fraud"
    ),
    point_estimate = round(point_estimate, 4)
  ) |>
  select(outcome, sample, point_estimate) |>
  pivot_wider(names_from = sample, values_from = point_estimate)

print(table_2_proxy)

openxlsx::write.xlsx(
  table_2_proxy,
  "pset_3_ex2_c_table2_proxy_point_estimates.xlsx",
  rowNames = FALSE,
  overwrite = TRUE
)
# Comment:
# Read these as fuzzy-RD local effects of actual coverage for centers whose treatment
# status changes at the proxy-based cutoff. Relative to the sharp design, longitude
# measurement error weakens the first stage, so the estimates can be noisier and may
# differ from the original Table 2 magnitudes.
