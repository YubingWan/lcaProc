
#' Entrophy Function
#' This function allows you to calculate the entrophy.
#' @param p p value .
#' @keywords cats
#' @export
#' @examples
#' entropy()
entropy <- function(p) {
  sum(-p*log(p))
}

#' Entrophy calculation Function
#' @export
entropy_cal <- function(lc) {
  round(((entropy(lc$P) - mean(apply(lc$posterior, 1, entropy), na.rm=TRUE)) / entropy(lc$P)), 3)
}

#' Summary Function
#' @export
mk_res_df <- function(
  res_list = NULL,
  model_type = "Simple"
) {
  res_df <- data.frame(
    Model_Type = model_type,
    Model_Case = paste0("LC_", seq(length(res_list))),
    Entropy  = unlist(lapply(res_list, entropy_cal)),
    N        = unlist(lapply(res_list, function(x) x[["N"]])),
    npar     = unlist(lapply(res_list, function(x) x[["npar"]])),
    resid.df = unlist(lapply(res_list, function(x) x[["resid.df"]])),
    LL_ratio = unlist(lapply(res_list, function(x) x[["Gsq"]])),
    AIC      = unlist(lapply(res_list, function(x) x[["aic"]])),
    BIC      = unlist(lapply(res_list, function(x) x[["bic"]])),
    Chisq    = unlist(lapply(res_list, function(x) x[["Chisq"]])),
    stringsAsFactors = F
  )
}

#' BS Function
#' @export
bs_lca_res <- function(
  f = NULL,
  data = NULL,
  indices
) {
  dd  <- data[indices, ] # allows boot to select sample
  # lc1 <- poLCA(f, dd, nclass=1, verbose=F)
  lc2 <- poLCA(f, dd, nclass=2, verbose=F)
  lc3 <- poLCA(f, dd, nclass=3, verbose=F)
  lc4 <- poLCA(f, dd, nclass=4, verbose=F)
  lc5 <- poLCA(f, dd, nclass=5, verbose=F)
  lc6 <- poLCA(f, dd, nclass=6, verbose=F)
  # lc7 <- poLCA(f, dd, nclass=7, verbose=F)
  ## results
  results <- data.frame(
    Model    = paste0("Mod_", 2:6),
    Entropy  = c(entropy_cal(lc2), entropy_cal(lc3), entropy_cal(lc4), entropy_cal(lc5), entropy_cal(lc6)),
    N        = c(lc2$N, lc3$N, lc4$N, lc5$N, lc6$N),
    npar     = c(lc2$npar, lc3$npar, lc4$npar, lc5$npar, lc6$npar),
    resid.df = c(lc2$resid.df, lc3$resid.df, lc4$resid.df, lc5$resid.df, lc6$resid.df),
    LL_ratio = c(lc2$Gsq, lc3$Gsq, lc4$Gsq, lc5$Gsq, lc6$Gsq),
    AIC      = c(lc2$aic, lc3$aic, lc4$aic, lc5$aic, lc6$aic),
    BIC      = c(lc2$bic, lc3$bic, lc4$bic, lc5$bic, lc6$bic)
  )
  return(results)
}

#' Make result Function
#' @export
make_lca_res <- function(
  f = NULL,
  dat = NULL,
  seed = 1,
  maxiter = 1000,
  nrep = 1,
  verbose=F,
  mod_id = "Mod_0_",
  ...
) {
  set.seed(seed)
  lc1 <- poLCA(f, dat, nclass=1, verbose = verbose, nrep = nrep, ...)
  lc2 <- poLCA(f, dat, nclass=2, verbose = verbose, nrep = nrep, ...)
  lc3 <- poLCA(f, dat, nclass=3, verbose = verbose, nrep = nrep, ...)
  lc4 <- poLCA(f, dat, nclass=4, verbose = verbose, nrep = nrep, ...)
  lc5 <- poLCA(f, dat, nclass=5, verbose = verbose, nrep = nrep, ...)
  lc6 <- poLCA(f, dat, nclass=6, verbose = verbose, nrep = nrep, ...)

  ## results
  results <- data.frame(
    Model    = paste0(mod_id, 1:6),
    Entropy  = c(NA, entropy_cal(lc2), entropy_cal(lc3), entropy_cal(lc4), entropy_cal(lc5), entropy_cal(lc6)),
    N        = c(lc1$N, lc2$N, lc3$N, lc4$N, lc5$N, lc6$N),
    npar     = c(lc1$npar, lc2$npar, lc3$npar, lc4$npar, lc5$npar, lc6$npar),
    resid.df = c(lc1$resid.df, lc2$resid.df, lc3$resid.df, lc4$resid.df, lc5$resid.df, lc6$resid.df),
    LL_ratio = c(lc1$Gsq, lc2$Gsq, lc3$Gsq, lc4$Gsq, lc5$Gsq, lc6$Gsq),
    AIC      = c(lc1$aic, lc2$aic, lc3$aic, lc4$aic, lc5$aic, lc6$aic),
    BIC      = c(lc1$bic, lc2$bic, lc3$bic, lc4$bic, lc5$bic, lc6$bic)
  )
  return(results)
}

#' Make result Function
#' @export
round_factor <- function(var) var <- as.factor(round(var))

#' Make Summary of statistics from lm regression output
#' @export
summarize_stats <- function(mod_tmp) {
  summy <- summary(mod_tmp)
  coeff <- mod_tmp$coefficients
  anova <- anova(mod_tmp, test = "Chisq")
  coefd <- coeff + c(0, rep(coeff[1], length(coeff) - 1))
  stat_tmp <- c(
    str_c(round(coefd, 4), str_c(round(summy$coefficients[, "Std. Error"], 2), ")"), sep = " ("),
    round(summy$fstatistic["value"], 2),
    round(anova$`Pr(>F)`[1], 4),
    round(summy$adj.r.squared, 4)
  )
  names(stat_tmp) <- c(str_c("mn_sd_grp", seq(length(coefd))), "F_val", "P_val", "R2_adj")
  return(stat_tmp)
}

#' Plot mean by cluster
#' @export
plot_mean_by_cluster <- function(
  df,
  color = NULL,
  linetype = NULL
) {
  # browser()
  color    <- enquo(color)
  linetype <- enquo(linetype)

  if(!is.null(rlang:::get_expr(color)) &
     is.null(rlang:::get_expr(linetype))) {
    plot_out <- ggplot(
      df,
      aes(
        y = Mean,
        x = PredClass,
        color = !!color
      )
    )
  } else if(!is.null(rlang:::get_expr(color)) &
            !is.null(rlang:::get_expr(linetype))) {
    plot_out <- ggplot(
      df,
      aes(
        y = Mean,
        x = PredClass,
        color = !!color,
        linetype = !!linetype
      )
    )
  }

  plot_out +
    geom_line() +
    geom_point() +
    scale_x_continuous(
      breaks=c(1, 2, 3),
      labels=c("Cluster 1", "Cluster 2", "Cluster 3")
    ) +
    labs(x="", y="Mean") +
    theme_bw(base_size = 12, base_family="") +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour="grey", size=0.5),
      axis.text = element_text(size=12),
      axis.title = element_text(size=12),
      axis.line = element_line(colour="black"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

#' Plot mode BICs Function
#' @export
plot_model_BICs <- function(df = NULL) {
  ggplot(
    df,
    aes(x = Model_Case, y = BIC, group = 1)
  ) +
    geom_point(size = 2) +
    geom_line(size = 0.5) + #, group = Model_Case, color = Case
    labs(x = "", y = "") +      #, title = "BIC Values of Estimated Models" facet_grid(Case ~ ., scales="free") +
    theme_bw(base_size = 16, base_family = "") + # scale_color_discrete(name = "Sample Case") +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour="grey", size=0.5),
      axis.text = element_text(size=16),
      axis.title = element_text(size=16),
      axis.line = element_line(colour="black"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

#' Plot Profile Function
#' @export
plot_profile <- function(
  df,
  x = NULL,
  y = NULL,
  group = NULL,
  xlab = "",
  ylab = "Mean"
) {
  # browser()
  x <- enquo(x)
  y <- enquo(y)
  group <- enquo(group)

  ggplot(
    df,
    aes(y = !!y, x = !!x, group = !!group)
  ) +
    geom_line(aes(linetype = !!group)) +
    geom_point(aes(shape = !!group), size = 2) +
    labs(x = xlab, y = ylab) +
    theme_bw(base_size = 15, base_family = "") +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey", size = 0.1),
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 15),
      axis.line  = element_line(colour = "black"),
      axis.text.x = element_text(angle = 60, hjust = 1)
    ) +
    scale_shape(solid = FALSE)
}

