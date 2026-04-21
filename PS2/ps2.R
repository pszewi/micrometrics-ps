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
} else if (user == "bogiano1945") {
    filepath <- "/Users/bogiano1945/Desktop"
} else if (user == "C") {
    filepath <- "/FILE/PATH/C/"
} else {    
    filepath <- ""  # Default case if user is not listed
}

setwd(filepath)
# Print the selected file path
print(paste("File path set to:", filepath))


# disabling a linter
# nolint: pipe_continuation_linter.

# ----------------
# imports
# ---------------

# installing and importing libraries as needed 
cran_packages <- c("dplyr", "stargazer", "tidyr", "sandwich",
                   "lmtest", "boot", "hdm", "openxlsx", "grf",
                   "ggplot2", "modelsummary", "TwoWayFEWeights",
									 "fixest", 'bacondecomp', "haven", "plm")

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
# EXE 1
# ---------------

pset_2 <- read.csv("files/pset_2.csv",sep=';')

# -------- a --------
#Given that divorce rates are
#an average computed in each state and the variable stpop provides the population in
#each of these states, which is the weight you should use when reporting the evolution
#of divorce rates or a regression of divorce rates on unilateral divorce laws to 
#match the analysis in Wolfers (2006)?

#Wolfers uses the metric of number of divorces per 1,000 people per each US state 
#for the div_rate variable. This creates a general metric with which you can properly 
#compare between states, but when one wants to report the evolution of divorce rates 
#or to run a regression of divorce rates on unilateral divorce laws, it is necessary to 
#give each state the proper weight in order to not over account for smaller states and 
#under account for larger states. (not having a bias toward smaller states).
#The weight type we use for this is Analytic Weights (aweight / cellsize) as it 
#corrects for cell means with different group sizes (stpop). Furthermore, it corrects
#for the heteroskedasticity in the standard deviations, which would naturally
#occur due to to different group sizes (here, the state populations).



# -------- b --------
df <- pset_2

df <- df %>%
  mutate(year = as.numeric(year))%>%
  arrange(st, year, div_rate, stpop)%>%
  mutate(
    reformed = lfdivlaw >= 1968 & lfdivlaw <= 1988,
    group = ifelse(reformed, "Reform states", "Control states")
  )

df_collapsed <- df %>%
  group_by(year, group) %>%
  summarise(
    avg_div_rate = weighted.mean(div_rate, stpop, na.rm = TRUE),
    .groups = "drop"
  )

df_diff <- df_collapsed %>%
  pivot_wider(names_from = group, values_from = avg_div_rate) %>%
  mutate(Difference = `Reform states` - `Control states`) %>%
  pivot_longer(cols = c("Reform states", "Control states", "Difference"),
               names_to = "group", values_to = "avg_div_rate")

#run for the plot (i)
ggplot(df_diff, aes(x = year, y = avg_div_rate, color = group, linetype = group, 
                    linewidth = group)) + geom_line() +
  scale_color_manual(values = c(
    "Reform states" = "black",
    "Control states" = "grey50",
    "Difference" = "black"
  )) +
  scale_linetype_manual(values = c(
    "Reform states" = "solid",
    "Control states" = "solid",
    "Difference" = "dashed"
  )) +
  scale_linewidth_manual(values = c(
    "Reform states" = 1,
    "Control states" = 1,
    "Difference" = 0.5
  )) +
  geom_vline(xintercept = 1969, linetype = "solid", linewidth = 0.4) +
  geom_vline(xintercept = 1977, linetype = "solid", linewidth = 0.4) +
  annotate("segment", x = 1968, xend = 1988, y = 0.15, yend = 0.15) +
  annotate("text", x = 1983, y = 0.35, 
           label = "Friedberg's sample", size = 3) +
  scale_x_continuous(breaks = seq(1956, 1998, by = 2)) +
  scale_y_continuous(limits = c(0, 7.5), breaks = 0:7) +
  labs(
    x = "Year",
    y = "Divorce rate: 
    Divorces per 1,000 persons per year",
    color = NULL, linetype = NULL, linewidth = NULL,
    caption = "Figure 1: average divorce rate: reform and control states"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    legend.position = "top",
    legend.key.width = unit(1.5, "cm"),
    plot.caption = element_text(hjust = 0.5, face = "bold", size = 9)
  )
ggsave("output/graph1.png")

## (II)
dfII <- pset_2

dfII <- dfII %>%
  mutate(year = as.numeric(year)) %>%
  arrange(st, year, div_rate, stpop) %>%
  mutate(
    group = ifelse(lfdivlaw >= 1969 & lfdivlaw <= 1973, "Early reform states (1969-1973)",
                   ifelse(lfdivlaw == 2000, "Late reform states (2000)", NA))
  ) %>%
  filter(!is.na(group)) 

dfII_collapsed <- dfII %>%
  group_by(year, group) %>%
  summarise(
    avg_div_rate = weighted.mean(div_rate, stpop, na.rm = TRUE),
    .groups = "drop"
  )

dfII_diff <- dfII_collapsed %>%
  pivot_wider(names_from = group, values_from = avg_div_rate) %>%
  mutate(Difference = `Early reform states (1969-1973)` - `Late reform states (2000)`) %>%
  pivot_longer(cols = c("Early reform states (1969-1973)", "Late reform states (2000)", "Difference"),
               names_to = "group", values_to = "avg_div_rate")

#run for the plot (ii)
ggplot(dfII_diff, aes(x = year, y = avg_div_rate, color = group, linetype = group, 
                    linewidth = group)) + geom_line() +
  scale_color_manual(values = c(
    "Early reform states (1969-1973)" = "black",
    "Late reform states (2000)" = "grey50",
    "Difference" = "black"
  )) +
  scale_linetype_manual(values = c(
    "Early reform states (1969-1973)" = "solid",
    "Late reform states (2000)" = "solid",
    "Difference" = "dashed"
  )) +
  scale_linewidth_manual(values = c(
    "Early reform states (1969-1973)" = 1,
    "Late reform states (2000)" = 1,
    "Difference" = 0.5
  )) +
  geom_vline(xintercept = 1968.5, linetype = "solid", linewidth = 0.4) +
  scale_x_continuous(breaks = seq(1956, 1978, by = 2), limits = c(1956, 1978)) +
  scale_y_continuous(limits = c(0, 7.5), breaks = 0:7) +
  labs(
    x = "Year",
    y = "Divorce rate: 
    Divorces per 1,000 persons per year",
    color = NULL, linetype = NULL, linewidth = NULL,
    caption = "Figure 2: average divorce rate"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    legend.position = "top",
    legend.key.width = unit(1.5, "cm"),
    plot.caption = element_text(hjust = 0.5, face = "bold", size = 9)
  )
ggsave("output/graph2.png")

#Our results indeed support the parallel trends assumption. The graphs show a 
#stable pre-treatment difference between the two groups, with divergence 
#happening after the reform cutoff. Both reform and control states exhibit 
#broadly similar upward trajectories in divorce rates. The stable difference is 
#easily visible through the dashed line, which  does not appear to move much 
#and hovers just above 1. Thus, there is no evidence of diverging pre-trends. 
#The only possible issue is that the pre-period is relatively short. 

# -------- c --------
#setup (steps i, ii, and iii)
dfIII <- pset_2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year == 1968 | year == 1978) %>%
  mutate(
    group = ifelse(lfdivlaw >= 1969 & lfdivlaw <= 1973, "Early reform states (1969-1973)",
                   ifelse(lfdivlaw == 2000, "Late reform states (2000)", NA))) %>%
  filter(!is.na(group)) %>%
  mutate(
    UNILATERAL = ifelse(lfdivlaw >= 1969 & lfdivlaw <= 1973, 1, 0),
    POST = ifelse(year == 1978, 1, 0),
    POST_UNILATERAL = UNILATERAL * POST) 

# (i) Pooled OLS
summary(lm(div_rate ~ POST_UNILATERAL + POST, weights =stpop, data = dfIII))


# (ii)
summary(lm(div_rate ~ POST_UNILATERAL + POST + UNILATERAL, weights = stpop, data = dfIII))

# Regression (i) pools all observations and omits UNILATERAL. Since early-reform
# states already had higher divorce rates than late-reform states before 1969,
# POST_UNILATERAL picks up both the baseline level gap and any treatment effect.
# The weighted estimate is 1.7007 divorces per 1,000 people (p < 0.001), which
# overstates the causal effect for exactly this reason.
# Regression (ii) adds UNILATERAL, so the interaction POST_UNILATERAL now
# isolates the change-in-change. The weighted DiD coefficient collapses to
# -0.0050 and is indistinguishable from zero (p ~ 0.99). The manual DiD from
# cell means in (d) reproduces this exactly. UNILATERAL itself is 1.71 and
# POST is 2.13, both significant - confirming that almost all of the apparent
# effect in (i) was pre-existing level difference and a common time shift,
# not a causal response to the law.

# Taking the weighted DiD at face value, the introduction of unilateral
# divorce laws in 1969-1973 had essentially no measurable effect on divorce
# rates by 1978 in this restricted sample.



# -------- d --------

means <- dfIII %>%
  group_by(UNILATERAL, POST) %>%
  summarise(avg_div_rate = weighted.mean(div_rate, stpop, na.rm = TRUE), .groups = "drop")

y11 <- means$avg_div_rate[means$UNILATERAL==1 & means$POST==1]
y10 <- means$avg_div_rate[means$UNILATERAL==1 & means$POST==0]
y01 <- means$avg_div_rate[means$UNILATERAL==0 & means$POST==1]
y00 <- means$avg_div_rate[means$UNILATERAL==0 & means$POST==0]
d1 <- y11 - y10
d2 <- y01 - y00
did <- d1 - d2


tab <- data.frame(
  POST        = c("POST=1", "POST=0", "Difference 1"),
  UNILATERAL_1 = c(y11, y10, d1),
  UNILATERAL_0 = c(y01, y00, d2),
  Difference_2 = c(y11 - y01, y10 - y00, did)
)

write.xlsx(tab, "output/TABLE_1.xlsx")


# -------- e --------
df_e <- pset_2 %>%
  mutate(year = as.numeric(year))%>%
  mutate(lfdivlaw = as.numeric(lfdivlaw))%>%
  arrange(st, year, div_rate, stpop)%>%
  filter(year <= 1988) %>%
  mutate(IMP_UNILATERAL = ifelse(lfdivlaw <= year, 1, 0))


#(i)
summary(feols(div_rate ~ IMP_UNILATERAL | st + year, data = df_e, weights = ~stpop))

#(ii)
summary(feols(div_rate ~ IMP_UNILATERAL | st + year + st[year], data = df_e, weights = ~stpop ))

#(iii)
summary(feols(div_rate ~ IMP_UNILATERAL | st + year + st[year] + st[I(year^2)], data = df_e, weights = ~stpop ))


# (i) FE only: IMP_UNILATERAL = -0.055 (not significant). Level-only two-way FE
# picks up that those treated early had higher divorce rates before treatment and that
# divorce rates were already trending up everywhere; with no allowance for
# state-specific trajectories, the treatment dummy absorbs some of that ongoing
# trend with the wrong sign.
#
# (ii) Adding state-specific linear trends: IMP_UNILATERAL = +0.477***. Once
# each state gets its own trend, the treatment dummy measures only the
# deviation from that trend - and there is a clear, positive short-run jump.
# This matches Wolfers (2006) Table 2 col (2).
#
# (iii) Adding quadratic trends: +0.334***. Positive and significant, but
# smaller.

# Reason results change: all three specifications force the treatment effect
# into a single number. If the effect is dynamic (rises then fades), a richer
# trend specification will soak up part of the response and shift the
# coefficient. The three specifications agree ONLY under the (strong)
# assumption that in the absence of the treatment, divorce rates would have
# evolved identically up to state fixed effects - i.e., no state-specific trends. 


# -------- f --------

#Creates simulated observations
df_f <- tibble(obs = 1 : 6 )%>%
  mutate (
    state = floor( 0.9 + obs / 3 )
  )%>%
  group_by(state)%>%
  mutate(year = row_number())%>%
  ungroup() %>%
  mutate (
    D = as.numeric ((state == 1 & year == 3) | (state == 2 & year %in% c (2, 3))),
    #Creates simulated outcomes
    Y = 0.1 + 0.02 * (year ==2) + 0.05 * (D ==1) + runif(n()) / 100 ,
    Y2 = 0.1 + 0.02 * (year ==2) + 0.05 * (D ==1) + 0.3 * (state == 2 & year == 3 ) + runif(n()) / 100,
    Y3 = 0.1 + 0.02 * (year ==2) + 0.05 * (D ==1) + 0.4 * (state == 2 & year == 3 ) + runif(n()) / 100,
    Y4 = 0.1 + 0.02 * (year ==2) + 0.05 * (D ==1) + 0.5 * (state == 2 & year == 3 ) + runif(n()) / 100
  )
print(df_f)

summary(feols(Y ~ D | state + year, data = df_f))
summary(feols(Y2 ~ D | state + year, data = df_f))
summary(feols(Y3 ~ D | state + year, data = df_f))
summary(feols(Y4 ~ D | state + year, data = df_f))

# TWFE is consistent for Y because treatment effects are homogeneous across treated
# observations. It is not consistent for Y2, Y3 or Y4, because staggered adoption with
# heterogeneous treatment effects makes the TWFE coefficient a weighted average with
# some negative weights. As heterogeneity increases, the estimate falls and can even
# become negative although all treatment effects are positive.

# -------- g --------
# The function returns a list object and can also export the group-time weights.

twfe_weights_sim <- twowayfeweights(
  data = df_f,
  Y = "Y",
  G = "state",
  T = "year",
  D = "D",
  type = "feTR",
  summary_measures = TRUE,
  path = "TABLE_2_TWFE_weights_simulation.csv"
)

print(twfe_weights_sim)
weights_sim_table <- read.csv("TABLE_2_TWFE_weights_simulation.csv")
print(weights_sim_table)

write.xlsx(weights_sim_table, "output/TWFE_weights_simulation.xlsx", rowNames = FALSE)


# The sign changes because staggered TWFE gives a negative weight to the already-treated
# cell. When that cell has the largest treatment effect (as in Y4), the weighted average
# can become negative even if all underlying treatment effects are positive.


# -------- h --------
# Goodman-Bacon decomposition on the Wolfers sample (1956-1988)

df_h <- pset_2 %>%
  mutate(year = as.numeric(year),
         lfdivlaw = as.numeric(lfdivlaw)) %>%
  filter(year <= 1988) %>%
  arrange(st, year) %>%
  group_by(st) %>%
  mutate(init_stpop = first(stpop)) %>%
  ungroup() %>%
  mutate(IMP_UNILATERAL = ifelse(lfdivlaw <= year, 1, 0))

# (ii) weighted TWFE regression on full sample
summary(feols(div_rate ~ IMP_UNILATERAL | st + year,
              data = df_h,
              weights = ~ init_stpop))

# Check where the problem is
colSums(is.na(df_h[, c("div_rate", "IMP_UNILATERAL", "st", "year")]))

df_h %>%
  group_by(st) %>%
  summarise(missing_div_rate = sum(is.na(div_rate)), .groups = "drop") %>%
  filter(missing_div_rate > 0)

# bacon() needs BOTH:
# - no NA values
# - balanced panel

bad_states <- df_h %>%
  group_by(st) %>%
  summarise(any_missing = any(is.na(div_rate) |
                                is.na(IMP_UNILATERAL) |
                                is.na(year)),
            .groups = "drop") %>%
  filter(any_missing) %>%
  pull(st)

df_h_bacon <- df_h %>%
  filter(!st %in% bad_states) %>%
  mutate(year = as.integer(year),
         IMP_UNILATERAL = as.integer(IMP_UNILATERAL))

# Check that the panel is now balanced and complete
colSums(is.na(df_h_bacon[, c("div_rate", "IMP_UNILATERAL", "st", "year")]))

df_h_bacon %>%
  count(st) %>%
  summarise(min_n = min(n), max_n = max(n))

# (iii) Goodman-Bacon decomposition

bacon_h <- bacon(
  div_rate ~ IMP_UNILATERAL,
  data = df_h_bacon,
  id_var = "st",
  time_var = "year"
)

bacon_h

# Plot
ggplot(bacon_h, aes(x = weight, y = estimate, shape = type)) +
  geom_point() +
  labs(x = "Weight", y = "Estimate", shape = "Comparison type") +
  theme_classic()
ggsave("output/goodmanbacon.png")

# Goodman-Bacon weights are non-negative by construction. The diagnostic
# concern is how much weight sits on comparisons that use already-treated
# units as controls (Later vs Earlier Treated) or on always-treated controls
# (Later vs Always Treated), because under heterogeneous effects these carry
# contaminated estimates. Here Later-vs-Earlier carries ~7% weight and
# Later-vs-Always ~10% with a large negative estimate (-1.18), which pulls
# the TWFE coefficient down - consistent with Wolfers's point that a single
# coefficient misstates a dynamic response.


# -------- i --------
# Event-study regressions following the assignment instructions.
# Omitted category: tau = -1
# Left bin: tau <= -10 -> tau = -10
# Right bin: tau >= 15 -> tau = 15

df_i <- pset_2 %>%
  mutate(
    year = as.numeric(year),
    lfdivlaw = as.numeric(lfdivlaw)
  ) %>%
  filter(year >= 1956 & year <= 1988) %>%
  arrange(st, year) %>%
  mutate(
    rel_time = ifelse(lfdivlaw == 2000, -10, year - lfdivlaw),
    rel_time = pmax(-10, pmin(15, rel_time)),
    year_trend = year - 1955
  )

# (i) State and year FE only
es_i <- feols(
  div_rate ~ i(rel_time, ref = -1) | st + year,
  data = df_i,
  weights = ~ stpop,
  cluster = ~ st
)
summary(es_i)

# (ii) Add state-specific linear trends
es_ii <- feols(
  div_rate ~ i(rel_time, ref = -1) | st + year + st[year_trend],
  data = df_i,
  weights = ~ stpop,
  cluster = ~ st
)
summary(es_ii)

# (iii) Add state-specific linear and quadratic trends
es_iii <- feols(
  div_rate ~ i(rel_time, ref = -1) | st + year + st[year_trend] + st[I(year_trend^2)],
  data = df_i,
  weights = ~ stpop,
  cluster = ~ st
)
summary(es_iii)


# The event-study suggests little evidence of strong pre-trends, a clear short-run rise
# in divorce after reform, and then a gradual fade-out. This dynamic pattern is exactly
# what the single-coefficient regressions in point e cannot show.


# -------- j --------
# Create one graph with coefficients and 95% confidence intervals for the 3 event-study regressions.

extract_event_study <- function(model, spec_name){
  cf <- coef(model)
  ci <- confint(model)
  
  out <- data.frame(
    term = names(cf),
    estimate = as.numeric(cf),
    conf.low = ci[, 1],
    conf.high = ci[, 2],
    stringsAsFactors = FALSE
  ) %>%
    filter(grepl("rel_time::", term)) %>%
    mutate(
      rel_time = as.numeric(sub(".*::", "", term)),
      specification = spec_name
    )
  
  ref_row <- data.frame(
    term = "rel_time::-1",
    estimate = 0,
    conf.low = 0,
    conf.high = 0,
    rel_time = -1,
    specification = spec_name,
    stringsAsFactors = FALSE
  )
  
  bind_rows(out, ref_row) %>%
    arrange(rel_time)
}

es_plot_df <- bind_rows(
  extract_event_study(es_i, "FE only"),
  extract_event_study(es_ii, "FE + state linear trends"),
  extract_event_study(es_iii, "FE + state linear and quadratic trends")
)

write.xlsx(es_plot_df, "output/event_study_coefficients.xlsx", rowNames = FALSE)

p_es <- ggplot(es_plot_df, aes(x = rel_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = -1, linetype = "dotted") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_point() +
  geom_line() +
  facet_wrap(~ specification, ncol = 1) +
  scale_x_continuous(breaks = c(-10, -8, -6, -4, -2, -1, 0, 2, 4, 6, 8, 10, 12, 14, 15)) +
  labs(
    x = "Years relative to reform",
    y = "Effect on divorce rate",
    caption = "Figure 4: event-study coefficients and 95% confidence intervals"
  ) +
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0.5, face = "bold", size = 9))

print(p_es)
ggsave("output/event_study_coefficients.png", plot = p_es, width = 8, height = 10)

# The graph makes the timing very clear: coefficients are highest just after reform,
# then decline over time, while the leads stay close to zero. This visually supports
# Wolfers's interpretation of a temporary rather than permanent effect.


# -------- k --------

# Friedberg (1998) concludes that unilateral divorce laws had a positive and persistent
# effect on divorce. Wolfers (2006) instead finds mostly a short-run increase, and he
# explains the difference by showing that a one-coefficient model masks the dynamic reversal.

# -------- l --------
# Sun and Abraham (2021) event-study using fixest::sunab()

df_l <- pset_2 %>%
  mutate(
    year = as.numeric(year),
    lfdivlaw = as.numeric(lfdivlaw)
  ) %>%
  filter(year >= 1956 & year <= 1988) %>%
  mutate(
    cohort_sa = case_when(
      lfdivlaw < 1956 ~ 1956,
      TRUE ~ lfdivlaw
    ),
    rel_time_sa = case_when(
      lfdivlaw == 2000 ~ -1000,
      TRUE ~ year - lfdivlaw
    ),
    rel_time_sa = case_when(
      lfdivlaw == 2000 ~ -1000,
      rel_time_sa <= -10 ~ -10,
      rel_time_sa >= 15 ~ 15,
      TRUE ~ rel_time_sa
    )
  )

sa_es <- feols(
  div_rate ~ sunab(cohort_sa, rel_time_sa, ref.c = 2000, ref.p = c(-1, -1000)) | st + year,
  data = df_l,
  weights = ~ stpop,
  cluster = ~ st
)
summary(sa_es)
summary(sa_es, agg = "ATT")

# Save a Sun-Abraham event-study graph
png("output/Sun_Abraham_event_study.png", width = 900, height = 600)
iplot(
  sa_es,
  main = "Sun and Abraham (2021) event-study",
  xlab = "Years relative to reform",
  ylab = "Effect on divorce rate",
  ci_level = 0.95
)
dev.off()

# Sun-Abraham corrects the staggered TWFE event-study for contamination from already-
# treated groups being used as controls. The corrected estimates should keep the same
# qualitative pattern as Wolfers: a short-run increase in divorce followed by fade-out.


# ---------------
# EXE 2
# ---------------

# a)

df.2 <- read.csv("files/expanded_data.csv")

#checking some initial statistics
summary(df.2)
table(df.2$year, df.2$lfdivlaw)

# does the intro of unilateral divorce law have an effect on divorce rates at county level

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
  ) |>
filter(lfdivlaw %in% c(1969, 1970, 1971, 1972, 1973, 2000))


# outcome
Y.2 <- df_wide$div_rate_sim_1978 - df_wide$div_rate_sim_1968

# treatment
W.2 <- as.integer(df_wide$lfdivlaw > 1968 & df_wide$lfdivlaw <= 1973)

# covariates
covariate_cols <- grep("_1968$", names(df_wide), value = TRUE)
X.2 <- df_wide |>
  select(all_of(covariate_cols), -div_rate_sim_1968) |>
  mutate(urbanization_1968 = as.integer(urbanization_1968 == "Urban")) |>
  as.matrix()

# cluster variable (treatment assigned at state level)
clusters.2 <- as.integer(as.factor(df_wide$st))

# causal forest with clustering
cf <- causal_forest(
  X = X.2,
  Y = Y.2,
  W = W.2,
  clusters = clusters.2,
  honesty = TRUE
)

# computing the ATE
average_treatment_effect(cf)

# Treatment is insignificant - as in the case of the 1c. Moreover, we find sign reversion. This could be just due to random variation, but it could also be due to the fact that the causal forest conditions on covariates when estimating the CATEs. If the treatment assignment is not prefectly randomly assigned (but maybe is given covariates), conditioning on the covariates would be changing the estimand.


# export ATE table 
ate_full <- average_treatment_effect(cf)
ate_df <- data.frame(
  Sample       = c("1969-1973 vs 2000"),
  Estimate     = c(ate_full["estimate"]),
  `Std. Error` = c(ate_full["std.err"]),
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


# The resuts would suggest that some covariates would be inducing heterogeneity in the model. In that case, the model should exhibit heterogeneity given the values of the religious adherence, domestic_violence_rate and women_labour_force_participation.

# ii) targeting operator characteristic
tau_hat <- predict(cf)$predictions
toc <- rank_average_treatment_effect(cf, priorities = tau_hat)
png("toc.png")
plot(toc)
dev.off()



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
# q of units chosen by some priority rule (in our case it is the size of the predicted treatment effect), to the overall average treatment effect. Therefore, it should identify (if statistically significant) whether treating only some units has a higher TE than treating all. 


# It seems that there is heterogeneity, given that the TOC is significant. This implies that the benefit of treating some units is higher than others (given their covariates).

# iii) plot dist of cate throughout important variables

# picking covariates suggested by the best linear projection as well as education (i thought it might be interesting)
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
	geom_smooth(method = "loess", method.args = list(degree = 1), color = "blue", linewidth=1) +
	geom_hline(yintercept =0, linetype="dashed") + 
	facet_wrap(~variable, scales = "free_x") +
	labs(x = "Covariate Value", y = "Estimated CATE") +
	theme_minimal()

ggsave("output/plot_cate.png")

# The chosen variables were plotted and in fact confirm what was found in the previous exercises - CATEs seem to differ across the values of covariates for some variables.


# c)
# It appears that all three ways of analysing heterogeneity point to its existence. We see that that religious adherenece in a given county, it's women labour participation rate as well as the domestic violence rate are all significant at the conventional significance levels, implying that they can be used for predicting the CATEs.
# Moreover, the TOC is also statistically significant according to the chosen priority rule, implying that treating only a fraction of units may result in higher TE than treating all (confirms heterogeneity). 
# Lastly, the graphs above show the covariates chosen by the best linear projection and their effect on the CATEs.
# Therefore, I would argue that the heterogenous treatment effects are present in the data. 

# Moreover, these results suggest multiple things about our data:
# - Although the graph of the CATE shows the confidence intervals are different from 0, we don't really see any heterogenous effects. Moreover, the BLP does not suggest it as a significant predictor of CATEs. Therefore, counties with more education are not more likely to have higher treatment effects (my hypothesis was: more education -> more ideologically/financially independent)
# - Counties where women participate more in the labour force appear to have higher CATEs. The mechanism could be: participate more -> more financially independent. Moreover, the effect seems to stabilise at some point (it's not strictly monotonous). This could be interpreted as women needing only a certain level of income to become more independent. 
# - Counties with low religious adherence have higher CATEs (people are less religious -> no shame from divorce in the church?)
# - It seems that counties with higher domestic violence rates also have higher treatment effects. Mechanism: more violence -> Vicitm more likely to separate.


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

png("toc_dishonest.png")
plot(toc_dishonest)
dev.off()


plot_df_dh <- data.frame(
  tau = tau_hat_dishonest,
 education = X.2[, "education_rate_1968"],
  participation = X.2[, "women_labor_force_participation_1968"],
  religion = X.2[, "religious_adherence_1968"],
	violence = X.2[, "domestic_violence_rate_1968"]

)

# Faceted panel for multiple variables:
plot_long_dishonest <- plot_df_dh |>
	pivot_longer(-tau, names_to = "variable", values_to = "value")

ggplot(plot_long_dishonest, aes(x = value, y = tau)) +
	geom_point(alpha = 0.1) +
	geom_smooth(method = "loess", method.args = list(degree = 1), color = "blue", linewidth=1) +
	geom_hline(yintercept =0, linetype="dashed") + 
	facet_wrap(~variable, scales = "free_x") +
	labs(x = "Covariate Value", y = "Estimated CATE") +
	theme_minimal()
ggsave("output/plot_cate_dishonest.png")

# Honesty is the procedure of splitting the data, such that the no tree in the random forest uses a single observation/sample for both placing the splits (building the tree) and predicting the values of the CATEs.
# It is important for the computation of the average treatment effect, because it allows for the unbiased estimation of the point estimates and the variance. Without honesty, we would find that the bias of the estimator is not bound, and that the condifence intervals of the coefficients would be too small, therefore destroying inference.
# This problem is related to the fact that our data would be used twice - both for creation of the tree and for the estimation of the CATEs

#This issue mainly affects estimation of the conditional average treatment effect because it relies on unbiased averages within the leaves of the tree, conditional on how the tree is built. Lack of honesty introduces adaptive bias and invalidates standard inference, leading to biased conditional average treatment effect estimates and overly narrow confidence intervals.
#In this setting we estimate the ATE using doubly-robust methods, so the lack of honesty does not change it much. This would not be the case if we our sample was small or if the overlap assumption was violated.
