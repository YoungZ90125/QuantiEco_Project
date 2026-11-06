# models.R — the three analyses and the decision summary.
#   Analysis 1 (predictions 1a, 1b): wood stork and white ibis, one negative-binomial GLM per species.
#   Analysis 2 (predictions 2a, 2b): eight species; the two foraging groups compared in one GLMM.
#   Analysis 3 (prediction 1c): year-to-year population change in the course's r + sN framework.
# Water terms (built in water.R): D10 = breeding-season depth (March-June) and W10 = previous wet-season
# depth (June-November), both centred at their 1992-2024 medians and measured in units of 10 cm.
# A rate ratio exp(b) is the factor by which the expected number of nesting pairs changes per +10 cm.

# ---- Analysis 1: core species (predictions 1a, 1b) ------------------------------------------

# Annual nesting pairs for the given species (region "all"), joined to the water index (1992-2024).
# year_c is the year centred at 2008, the middle of 1992-2024, used for the linear trend.
prepare_counts <- function(counts, water, species_codes) {
  counts |>
    dplyr::filter(region == "all", species %in% species_codes) |>
    dplyr::inner_join(water[c("year", "D10", "W10")], by = "year") |>
    dplyr::transmute(species, year, pairs = as.integer(round(count)), D10, W10, year_c = year - 2008) |>
    dplyr::arrange(species, year)
}

# Negative-binomial GLM of nesting pairs on breeding-season depth, previous wet-season depth and trend.
fit_core <- function(df) {
  MASS::glm.nb(pairs ~ D10 + W10 + year_c, data = df)
}

# Rate ratios exp(b) with Wald 95 % CIs, without p-values (used for the robustness refits).
wald_rr <- function(model, terms = c("D10", "W10")) {
  est <- unname(stats::coef(model)[terms])
  se <- unname(sqrt(diag(stats::vcov(model))[terms]))
  z <- stats::qnorm(0.975)
  tibble::tibble(term = terms, rr = exp(est), lo = exp(est - z * se), hi = exp(est + z * se))
}

# Rate ratios with Wald CIs, plus a likelihood-ratio test for each term: the model is refitted without
# the term on the same data (model$model) and the two fits are compared.
rate_ratios <- function(model, terms = c("D10", "W10")) {
  p <- vapply(terms, function(term) {
    reduced <- MASS::glm.nb(stats::update(stats::formula(model), paste(". ~ . -", term)), data = model$model)
    stats::anova(reduced, model)$`Pr(Chi)`[2]
  }, numeric(1))
  dplyr::mutate(wald_rr(model, terms), p_lrt = unname(p))
}

# Decision rule for one prediction: "supported" if the whole CI lies on the predicted side of `null`
# (1 for rate ratios, 0 for regression coefficients), "contradicted" if it lies wholly on the other
# side, otherwise "not supported".
classify_effect <- function(lo, hi, predicted = c("below", "above"), null = 1) {
  predicted <- match.arg(predicted)
  ci_below <- hi < null   # the whole CI is below the null value
  ci_above <- lo > null   # the whole CI is above the null value
  on_predicted_side <- if (predicted == "below") ci_below else ci_above
  on_other_side <- if (predicted == "below") ci_above else ci_below
  ifelse(on_predicted_side, "supported", ifelse(on_other_side, "contradicted", "not supported"))
}

# "Robust": every robustness estimate lies on the predicted side of `null`.
robust_direction <- function(values, predicted = c("below", "above"), null = 1) {
  predicted <- match.arg(predicted)
  if (predicted == "below") all(values < null) else all(values > null)
}

# Model checks: DHARMa simulated-residual tests (uniformity, dispersion) and the autocorrelation of the
# residuals at lags 1-5 (element 1 of acf() is lag 0, so elements 2-6 are lags 1-5).
core_diagnostics <- function(model, seed = 20261104) {
  sim <- DHARMa::simulateResiduals(model, n = 1000, seed = seed)
  list(p_uniform = DHARMa::testUniformity(sim, plot = FALSE)$p.value,
       p_dispersion = DHARMa::testDispersion(sim, plot = FALSE)$p.value,
       acf = as.numeric(stats::acf(stats::residuals(model, type = "pearson"), lag.max = 5, plot = FALSE)$acf[2:6]))
}

# Robustness checks set in advance:
#   R1 drops the year trend;
#   R2 adds log(pairs in the previous year), i.e. density dependence (so the first year is lost);
#   R3 refits the model leaving out one year at a time and keeps the smallest and largest rate ratio.
core_robustness <- function(df) {
  df <- dplyr::arrange(df, year)
  r1 <- wald_rr(MASS::glm.nb(pairs ~ D10 + W10, data = df))

  with_lag <- df |>
    dplyr::mutate(log_lag = log(dplyr::lag(pairs))) |>
    dplyr::filter(!is.na(log_lag))
  r2 <- wald_rr(MASS::glm.nb(pairs ~ D10 + W10 + year_c + log_lag, data = with_lag))

  leave_one_out <- dplyr::bind_rows(lapply(seq_len(nrow(df)), function(i) wald_rr(fit_core(df[-i, ]))))
  r3 <- leave_one_out |>
    dplyr::group_by(term) |>
    dplyr::summarise(R3_min = min(rr), R3_max = max(rr), .groups = "drop") |>
    tidyr::pivot_longer(c(R3_min, R3_max), names_to = "check", values_to = "rr") |>
    dplyr::mutate(lo = NA_real_, hi = NA_real_)

  dplyr::bind_rows(dplyr::mutate(r1, check = "R1"), dplyr::mutate(r2, check = "R2"), r3) |>
    dplyr::select(check, term, rr, lo, hi)
}

# ---- Analysis 2: community (predictions 2a, 2b) ---------------------------------------------

# The eight species meeting the inclusion rule (>= 30 years, median >= 50 pairs, nesting in >= 80 % of
# years): white ibis, wood stork, snowy egret, great egret, tricolored heron, great blue heron,
# little blue heron and black-crowned night heron.
community_species <- c("whib", "wost", "sneg", "greg", "trhe", "gbhe", "lbhe", "bcnh")

# Counts for the eight species with their foraging group from the dataset's species table:
# "high grader" (requires concentrated prey) or "exploiter" (flexible forager); NA for the two
# unlabelled species (little blue heron, black-crowned night heron).
community_data <- function(counts, water, species_list) {
  groups <- species_list |>
    dplyr::filter(species %in% community_species) |>
    dplyr::transmute(species, group = dplyr::na_if(foraging_type, ""))
  prepare_counts(counts, water, community_species) |>
    dplyr::left_join(groups, by = "species") |>
    dplyr::mutate(group = factor(group, levels = c("exploiter", "high grader"))) |>
    dplyr::select(species, group, year, pairs, D10, W10, year_c)
}

# The Analysis 1 model fitted to each of the eight species separately (for the forest plot).
# cd is the output of community_data().
species_effects <- function(cd) {
  dplyr::bind_rows(lapply(split(cd, cd$species), function(d) {
    dplyr::mutate(wald_rr(fit_core(d)), species = d$species[1], group = d$group[1])
  })) |>
    dplyr::select(species, group, term, rr, lo, hi)
}

# GLMM specifications, tried in this order (the fallback order was fixed before fitting). Terms:
#   species + species:year_c                  an intercept and a linear trend for each species;
#   group:D10 + group:W10                     one D and one W effect per foraging group (the tested effects);
#   (1 | year)                                a random effect of year, shared by all species;
#   (0 + D10 | species), (0 + W10 | species)  species-level variation around the group effects.
# The second specification drops the species-level variation; the third also drops the year effect.
glmm_formulas <- list(
  full = pairs ~ species + species:year_c + group:D10 + group:W10 + (1 | year) + (0 + D10 | species) + (0 + W10 | species),
  no_random_slopes = pairs ~ species + species:year_c + group:D10 + group:W10 + (1 | year),
  no_year_re = pairs ~ species + species:year_c + group:D10 + group:W10
)

# Negative-binomial GLMM (glmmTMB, nbinom2) with a separate dispersion parameter for each species.
fit_glmm <- function(formula, data) {
  glmmTMB::glmmTMB(formula, dispformula = ~ species, family = glmmTMB::nbinom2, data = data)
}

# A fit counts as converged when the optimiser reports code 0 and the Hessian is positive definite.
glmm_ok <- function(m) {
  !is.null(m) && identical(as.integer(m$fit$convergence), 0L) && isTRUE(m$sdr$pdHess)
}

# Fits the specifications in `formulas` in order and returns the first that converges, with its name (a
# fit that stops with an error counts as not converged). cd6 is community_data() restricted to the six
# species with a foraging group.
fit_group_glmm <- function(cd6, formulas = glmm_formulas) {
  for (spec in names(formulas)) {
    m <- tryCatch(fit_glmm(formulas[[spec]], cd6), error = function(e) NULL)
    if (glmm_ok(m)) return(list(model = m, spec_used = spec))
  }
  stop("No GLMM specification converged")
}

# Group-level rate ratios per +10 cm, one row each:
#   D_high, D_expl   breeding-season (D) effect for high graders and for exploiters;
#   D_ratio          high grader / exploiter ratio of the two D effects (prediction 2a), with a
#                    likelihood-ratio test against a model in which both groups share one D effect;
#   W_high, W_expl   previous wet-season (W) effect for each group (prediction 2b).
group_effects <- function(fit, cd6) {
  b <- glmmTMB::fixef(fit$model)$cond   # fixed-effect coefficients (log scale)
  V <- stats::vcov(fit$model)$cond      # and their covariance matrix
  z <- stats::qnorm(0.975)
  d_high <- "grouphigh grader:D10"; d_expl <- "groupexploiter:D10"
  w_high <- "grouphigh grader:W10"; w_expl <- "groupexploiter:W10"
  # On the log scale a ratio of two rate ratios is a difference, with variance Var(a) + Var(b) - 2 Cov(a, b).
  est <- c(b[[d_high]], b[[d_expl]], b[[d_high]] - b[[d_expl]], b[[w_high]], b[[w_expl]])
  se <- sqrt(c(V[d_high, d_high], V[d_expl, d_expl], V[d_high, d_high] + V[d_expl, d_expl] - 2 * V[d_high, d_expl],
               V[w_high, w_high], V[w_expl, w_expl]))
  shared_d <- fit_glmm(stats::update(glmm_formulas[[fit$spec_used]], . ~ . - group:D10 + D10), cd6)
  p <- stats::anova(shared_d, fit$model)$`Pr(>Chisq)`[2]
  tibble::tibble(quantity = c("D_high", "D_expl", "D_ratio", "W_high", "W_expl"),
                 estimate = exp(est), lo = exp(est - z * se), hi = exp(est + z * se),
                 p = c(NA, NA, p, NA, NA))
}

# Sensitivity: the group effects under each of the three specifications. The primary result is
# the first specification that converges.
group_effects_by_spec <- function(cd6) {
  dplyr::bind_rows(lapply(names(glmm_formulas), function(spec) {
    m <- tryCatch(fit_glmm(glmm_formulas[[spec]], cd6), error = function(e) NULL)
    if (!glmm_ok(m)) {
      return(tibble::tibble(spec = spec, converged = FALSE, quantity = c("D_high", "D_expl", "D_ratio", "W_high", "W_expl"),
                            estimate = NA_real_, lo = NA_real_, hi = NA_real_, p = NA_real_))
    }
    dplyr::mutate(group_effects(list(model = m, spec_used = spec), cd6), spec = spec, converged = TRUE, .before = 1)
  }))
}

# ---- Analysis 3: population dynamics in the course's r + sN framework (prediction 1c) --------

# Year-to-year transitions for one species: per-capita rate g = ln(N[t+1] / N[t]) (the differential
# form of the per-capita growth rate over one year), N[t] in thousands of pairs, and the water
# conditions of the nesting year t + 1 (breeding season D, preceding wet season W).
# x[-n] drops the last year (values at t); x[-1] drops the first year (values at t + 1).
make_transitions <- function(df) {
  df <- dplyr::arrange(df, year)
  stopifnot(all(diff(df$year) == 1))
  n <- nrow(df)
  tibble::tibble(year_t = df$year[-n], N_t = df$pairs[-n] / 1000, g = log(df$pairs[-1] / df$pairs[-n]),
                 D10_next = df$D10[-1], W10_next = df$W10[-1])
}

# g = r + s * N + bD * D + bW * W, by least squares: r is the intercept, s the density dependence,
# bD and bW the water effects.
fit_rsn <- function(tr) {
  stats::lm(g ~ N_t + D10_next + W10_next, data = tr)
}

# Model checks: residual normality (Shapiro-Wilk), residual autocorrelation at lags 1-5 (elements 2-6
# of acf()) and the largest Cook's distance.
rsn_diagnostics <- function(model) {
  r <- stats::residuals(model)
  list(p_shapiro = stats::shapiro.test(r)$p.value,
       acf = as.numeric(stats::acf(r, lag.max = 5, plot = FALSE)$acf[2:6]),
       max_cooks = max(stats::cooks.distance(model)))
}

# Water scenarios from the observed 1992-2024 values: favourable = shallow breeding season after a
# wet wet-season (D 10th percentile, W 90th); unfavourable = the reverse; median = both at medians.
water_scenarios <- function(wi) {
  q <- function(x, p) unname(stats::quantile(x, p))
  tibble::tibble(scenario = c("favourable", "median", "unfavourable"),
                 D10 = c(q(wi$D10, 0.1), stats::median(wi$D10), q(wi$D10, 0.9)),
                 W10 = c(q(wi$W10, 0.9), stats::median(wi$W10), q(wi$W10, 0.1)))
}

# Equilibrium nesting population (where g = 0): K = -(r + bD * D + bW * W) / s.
k_value <- function(r, s, bD, bW, D10, W10) {
  -(r + bD * D10 + bW * W10) / s
}

# K for each scenario: the plug-in value of the formula, with a 95 % interval from the 2.5 and 97.5 %
# percentiles of K over draws from the coefficients' multivariate normal distribution.
# `coefs` and `vcov` are in the order (Intercept), N_t, D10_next, W10_next, i.e. r, s, bD, bW.
# K is reported only if the 95 % CI of s (the N_t coefficient) lies entirely below 0; that CI is
# t-based when the residual degrees of freedom `df` are given (as for the lm fit), normal otherwise.
k_scenarios <- function(coefs, vcov, scen, n_draws = 4000, seed = 20261104, df = Inf) {
  coefs <- unname(coefs); vcov <- unname(as.matrix(vcov))
  crit <- if (is.finite(df)) stats::qt(0.975, df) else stats::qnorm(0.975)
  s_upper <- coefs[2] + crit * sqrt(vcov[2, 2])   # upper 95 % limit of s
  out <- dplyr::mutate(scen, K = NA_real_, K_lo = NA_real_, K_hi = NA_real_)
  if (s_upper >= 0) return(out)                   # no clear density dependence: K is not reported
  set.seed(seed)
  draws <- MASS::mvrnorm(n_draws, mu = coefs, Sigma = vcov)
  for (i in seq_len(nrow(scen))) {
    out$K[i] <- k_value(coefs[1], coefs[2], coefs[3], coefs[4], scen$D10[i], scen$W10[i])
    k_draws <- k_value(draws[, 1], draws[, 2], draws[, 3], draws[, 4], scen$D10[i], scen$W10[i])
    out$K_lo[i] <- unname(stats::quantile(k_draws, 0.025))
    out$K_hi[i] <- unname(stats::quantile(k_draws, 0.975))
  }
  out
}

# R4: refit leaving out one transition at a time; the smallest and largest value of each coefficient.
rsn_robustness <- function(tr) {
  refits <- t(vapply(seq_len(nrow(tr)), function(i) stats::coef(fit_rsn(tr[-i, ])), numeric(4)))   # one row per refit
  terms <- c("N_t", "D10_next", "W10_next")
  tibble::tibble(term = terms, min = unname(apply(refits[, terms], 2, min)), max = unname(apply(refits[, terms], 2, max)))
}

# ---- Decision summary ---------------------------------------------------------------------------

# Combined outcome of prediction 1c (bD and bW together): "supported" if both coefficients are
# supported, "partly supported" if one is; "contradicted" if either is contradicted.
combine_outcomes <- function(outcomes) {
  if (any(outcomes == "contradicted")) return("contradicted")
  if (all(outcomes == "supported")) return("supported")
  if (any(outcomes == "supported")) return("partly supported")
  "not supported"
}

# One row per tested quantity, labelled by hypothesis (1, 2) and prediction (1a-1c, 2a-2b).
# Species keep their input order.
# core_dec: species, term (D10/W10), rr, lo, hi, robust. g_eff: output of group_effects().
# rsn_dec: species, term (D10_next/W10_next), estimate, lo, hi, robust.
decision_summary <- function(core_dec, g_eff, rsn_dec) {
  # Analysis 1: the rows for one model term, one per species.
  core_rows <- function(model_term, prediction, predicted) {
    core_dec |>
      dplyr::filter(term == model_term) |>
      dplyr::transmute(prediction = prediction, unit = species, estimate = rr, lo, hi,
                       outcome = classify_effect(lo, hi, predicted), robust)
  }
  # Analysis 2: `units` maps quantity codes of group_effects() to row labels.
  group_rows <- function(units, prediction, predicted) {
    g_eff |>
      dplyr::filter(quantity %in% names(units)) |>
      dplyr::transmute(prediction = prediction, unit = unname(units[quantity]), estimate, lo, hi,
                       outcome = classify_effect(lo, hi, predicted), robust = NA)
  }
  # Analysis 3: bD is predicted below 0 and bW above 0; then one combined row per species.
  rsn <- dplyr::mutate(rsn_dec, outcome = ifelse(term == "D10_next", classify_effect(lo, hi, "below", null = 0),
                                                 classify_effect(lo, hi, "above", null = 0)))
  species <- unique(rsn$species)
  combined <- tibble::tibble(prediction = "1c", unit = paste0(species, ": combined"),
                             estimate = NA_real_, lo = NA_real_, hi = NA_real_,
                             outcome = vapply(species, function(s) combine_outcomes(rsn$outcome[rsn$species == s]),
                                              character(1), USE.NAMES = FALSE),
                             robust = NA)
  dplyr::bind_rows(
    core_rows("D10", "1a", "below"),
    core_rows("W10", "1b", "above"),
    dplyr::transmute(rsn, prediction = "1c",
                     unit = paste0(species, ": ", ifelse(term == "D10_next", "bD", "bW")), estimate, lo, hi, outcome, robust),
    combined,
    group_rows(c(D_ratio = "high grader / exploiter (D)"), "2a", "below"),
    group_rows(c(W_high = "high grader (W)", W_expl = "exploiter (W)"), "2b", "above")
  ) |>
    dplyr::mutate(hypothesis = substr(prediction, 1, 1), .before = 1)
}

# ---- Exploratory comparison: snail kite (not one of the five predictions) -------------------------

# Snail kite (snki), wood stork and white ibis over the kite years (1996-2021), in the format of
# prepare_counts(). kite: data/raw/kite_nests_everglades.csv (year, nests).
kite_data <- function(counts, kite, water) {
  kite_rows <- dplyr::transmute(kite, region = "all", species = "snki", year, count = nests)
  dplyr::bind_rows(counts, kite_rows) |>
    prepare_counts(water, c("snki", "wost", "whib")) |>
    dplyr::filter(year %in% kite$year) |>
    dplyr::mutate(species = factor(species, levels = c("snki", "wost", "whib")))
}

# One negative-binomial model for the three species, fitted with the Analysis 2 machinery (fit_glmm: a
# dispersion parameter per species; the same convergence rule). Terms: an intercept, a trend, a D effect
# and a W effect for each species, and a year random effect shared by the three species. The fallback,
# set before fitting, drops the year effect.
kite_formulas <- list(
  year_re = pairs ~ species + species:year_c + species:D10 + species:W10 + (1 | year),
  no_year_re = pairs ~ species + species:year_c + species:D10 + species:W10
)

fit_kite_joint <- function(d3) fit_group_glmm(d3, kite_formulas)

# Rate ratios per +10 cm with Wald 95 % CIs: each species' D and W effect, and the kite's effect relative
# to each wading bird (kite / wood stork, kite / white ibis; on the log scale a difference, with variance
# Var(a) + Var(b) - 2 Cov(a, b)), with a Wald p-value for the ratio.
kite_effects <- function(fit) {
  b <- glmmTMB::fixef(fit$model)$cond
  V <- stats::vcov(fit$model)$cond
  z <- stats::qnorm(0.975)
  label <- c(snki = "kite", wost = "wood stork", whib = "white ibis")
  rows <- list()
  for (term in c("D10", "W10")) {
    coef_name <- function(sp) paste0("species", sp, ":", term)
    for (sp in names(label)) {
      k <- coef_name(sp)
      rows[[length(rows) + 1]] <- tibble::tibble(term = term, quantity = label[[sp]], log_est = b[[k]], se = sqrt(V[k, k]))
    }
    k <- coef_name("snki")
    for (sp in c("wost", "whib")) {
      w <- coef_name(sp)
      rows[[length(rows) + 1]] <- tibble::tibble(term = term, quantity = paste("kite /", label[[sp]]), log_est = b[[k]] - b[[w]],
                                                 se = sqrt(V[k, k] + V[w, w] - 2 * V[k, w]))
    }
  }
  dplyr::bind_rows(rows) |>
    dplyr::transmute(term, quantity, estimate = exp(log_est), lo = exp(log_est - z * se), hi = exp(log_est + z * se),
                     p = ifelse(grepl("/", quantity), 2 * stats::pnorm(-abs(log_est / se)), NA_real_))
}
