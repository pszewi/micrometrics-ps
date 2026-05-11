library(modelsummary)

rdrobust_to_modelsummary <- function(model, estimate_types = "Robust") {
  available_types <- rownames(model$coef)
  missing_types <- setdiff(estimate_types, available_types)

  if (length(missing_types) > 0) {
    stop(
      "These estimate types are not available in the rdrobust object: ",
      paste(missing_types, collapse = ", ")
    )
  }

  tidy <- data.frame(
    term = estimate_types,
    estimate = as.numeric(model$coef[estimate_types, 1]),
    std.error = as.numeric(model$se[estimate_types, 1]),
    statistic = as.numeric(model$z[estimate_types, 1]),
    p.value = as.numeric(model$pv[estimate_types, 1]),
    conf.low = as.numeric(model$ci[estimate_types, 1]),
    conf.high = as.numeric(model$ci[estimate_types, 2]),
    stringsAsFactors = FALSE
  )

  glance <- data.frame(
    nobs = sum(model$N),
    n_left = model$N[1],
    n_right = model$N[2],
    n_eff = sum(model$N_h),
    n_eff_left = model$N_h[1],
    n_eff_right = model$N_h[2],
    h_left = model$bws["h", "left"],
    h_right = model$bws["h", "right"],
    b_left = model$bws["b", "left"],
    b_right = model$bws["b", "right"],
    p = model$p,
    q = model$q,
    kernel = model$kernel,
    bwselect = model$bwselect,
    vce = model$vce,
    stringsAsFactors = FALSE
  )

  out <- list(tidy = tidy, glance = glance)
  class(out) <- "modelsummary_list"
  out
}

rdrobust_modelsummary <- function(models,
                                  estimate_types = "Robust",
                                  output = NULL,
                                  coef_map = NULL,
                                  gof_map = NULL,
                                  ...) {
  if (inherits(models, "rdrobust")) {
    models <- list("RD estimate" = models)
  }

  converted_models <- lapply(
    models,
    rdrobust_to_modelsummary,
    estimate_types = estimate_types
  )

  if (is.null(coef_map)) {
    coef_map <- setNames(estimate_types, estimate_types)
  }

  if (is.null(gof_map)) {
    gof_map <- data.frame(
      raw = c("nobs", "n_eff", "h_left", "h_right", "kernel", "bwselect"),
      clean = c(
        "Observations",
        "Effective observations",
        "Bandwidth left",
        "Bandwidth right",
        "Kernel",
        "Bandwidth selector"
      ),
      fmt = c(0, 0, 3, 3, NA, NA),
      stringsAsFactors = FALSE
    )
  }

  modelsummary(
    converted_models,
    output = output,
    coef_map = coef_map,
    gof_map = gof_map,
    statistic = "({std.error})",
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    ...
  )
}
