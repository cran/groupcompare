# Bootstrap for differences of various statistics
bootstrap <- function(x, statistic, alternative="two.sided", alpha=0.05, Q=c(0.25, 0.5, 0.75), qt=7, R=3000) {
  bootdif <- function(data, indices) {
    res <- statistic(data, indices, Q=Q, qt=qt)
    return(res)
  }

  bootres <- boot::boot(data=x, statistic=bootdif, R=R)
  theta_hat <- bootres$t0
  theta_boot <- bootres$t
  theta_se <- apply(theta_boot, 2, sd, na.rm = TRUE)

  cilist <- lapply(1:ncol(theta_boot), function(i) {
    boot_dist <- na.omit(theta_boot[, i])
    orig_est <- theta_hat[i]
    boot_se <- theta_se[i]

    normal_ci <- if (alternative == "greater") {
      c(lower = orig_est - qnorm(1 - alpha) * boot_se, upper = Inf)
    } else if (alternative == "less") {
      c(lower = -Inf, upper = orig_est + qnorm(1 - alpha) * boot_se)
    } else {
      c(lower = orig_est - qnorm(1 - alpha / 2) * boot_se, upper = orig_est + qnorm(1 - alpha / 2) * boot_se)
    }

    basic_ci <- if (alternative == "greater") {
      c(lower = 2 * orig_est - quantile(boot_dist, probs = 1 - alpha, na.rm = TRUE), upper = Inf)
    } else if (alternative == "less") {
      c(lower = -Inf, upper = 2 * orig_est - quantile(boot_dist, probs = alpha, na.rm = TRUE))
    } else {
      c(lower = 2 * orig_est - quantile(boot_dist, probs = 1 - alpha / 2, na.rm = TRUE), upper = 2 * orig_est - quantile(boot_dist, probs = alpha / 2, na.rm = TRUE))
    }

    percent_ci <- if (alternative == "greater") {
      c(lower = quantile(boot_dist, probs = alpha, na.rm = TRUE), upper = Inf)
    } else if (alternative == "less") {
      c(lower = -Inf, upper = quantile(boot_dist, probs = 1 - alpha, na.rm = TRUE))
    } else {
      c(lower = quantile(boot_dist, probs = alpha / 2, na.rm = TRUE), upper = quantile(boot_dist, probs = 1 - alpha / 2, na.rm = TRUE))
    }

    z0 <- qnorm(mean(boot_dist < orig_est, na.rm = TRUE))
    n <- length(na.omit(x[,1]))
    jackknife_values <- sapply(1:n, function(j) statistic(x[-j, ], Q=Q, qt=qt)[i])
    jackknife_values <- na.omit(jackknife_values)
    a <- if (length(jackknife_values) > 1) {
      sum((mean(jackknife_values) - jackknife_values)^3) /
        (6 * (sum((mean(jackknife_values) - jackknife_values)^2)^(3/2)))
    } else {
      0
    }

    bca_ci <- if (alternative == "greater") {
      lower_p <- pnorm(z0 + (z0 + qnorm(alpha)) / (1 - a * (z0 + qnorm(alpha))))
      c(lower = quantile(boot_dist, probs = lower_p, na.rm = TRUE), upper = Inf)
    } else if (alternative == "less") {
      upper_p <- pnorm(z0 + (z0 + qnorm(1 - alpha)) / (1 - a * (z0 + qnorm(1 - alpha))))
      c(lower = -Inf, upper = quantile(boot_dist, probs = upper_p, na.rm = TRUE))
    } else {
      lower_p <- pnorm(z0 + (z0 + qnorm(alpha / 2)) / (1 - a * (z0 + qnorm(alpha / 2))))
      upper_p <- pnorm(z0 + (z0 + qnorm(1 - alpha / 2)) / (1 - a * (z0 + qnorm(1 - alpha / 2))))
      c(lower = quantile(boot_dist, probs = lower_p, na.rm = TRUE), upper = quantile(boot_dist, probs = upper_p, na.rm = TRUE))
    }

    list(
      normal = normal_ci,
      basic = basic_ci,
      percent = percent_ci,
      bca = bca_ci
    )
  })

  ciresults <- lapply(cilist, function(ci) {
    matrix(unlist(ci), ncol = 2, byrow = TRUE, dimnames = list(names(ci), c("lower", "upper")))
  })

  if (!is.null(names(statistic(x, 1:nrow(x), Q=Q, qt=qt)))) {
    names(ciresults) <- names(statistic(x, 1:nrow(x), Q=Q, qt=qt))
  } else {
    names(ciresults) <- c(paste0("Stat_", 1:length(ciresults)))
  }
  return(ciresults)
}