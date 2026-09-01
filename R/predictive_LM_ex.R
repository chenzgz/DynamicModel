#' External Validation for Landmarking Models
#'
#' This function performs external validation of landmarking models on an
#' external dataset. It evaluates discrimination (C-index and AUC), overall
#' prediction performance (Brier score), calibration, and clinical utility
#' using decision curve analysis (DCA) at multiple landmark times.
#'
#' @usage Predictive_LM_ex(
#'   object,
#'   ex_data,
#'   n_group = 10,
#'   seq_len = NULL,
#'   dca_thresholds = seq(0.05, 0.50, by = 0.01)
#' )
#'
#' @param object An object inheriting from class "LMf" or "Vs_LM".
#' @param ex_data An external validation dataset with variable names identical
#'   to those in the model-building dataset.
#' @param n_group Integer, number of groups used for calibration assessment.
#'   Default is 10.
#' @param seq_len Optional numeric vector of landmark times to evaluate.
#'   If \code{NULL}, the landmark times stored in the model object are used.
#' @param dca_thresholds Numeric vector specifying the threshold probabilities
#'   used for decision curve analysis (DCA). The default is
#'   \code{seq(0.05, 0.50, by = 0.01)}.
#'
#' @return A list containing:
#' \itemize{
#'   \item{time: The sequence of landmark times used for evaluation.}
#'   \item{cindex: Concordance index values for each landmark time.}
#'   \item{BS: Brier score values for each landmark time.}
#'   \item{AUC: AUC values for each landmark time.}
#'   \item{calibration_slope_with_ci: Calibration slope with confidence intervals.}
#'   \item{all_cal_autual: Complete calibration results.}
#'   \item{dca_results: Decision curve analysis results, including threshold
#'     probabilities and net benefit for the landmark model, treat-all
#'     strategy, and treat-none strategy.}
#' }
#'
#' @importFrom dynpred cutLM
#' @importFrom survival survfit Surv
#' @importFrom Hmisc cut2
#' @export
#'
Predictive_LM_ex <- function(
    object,
    ex_data,
    n_group = 10,
    seq_len = NULL,
    dca_thresholds = seq(0.05, 0.50, by = 0.01)
) {

  # ============================================
  # 1. Load required packages
  # ============================================

  if (!require("survivalROC", quietly = TRUE)) {
    install.packages("survivalROC")
    library(survivalROC)
  }

  if (!require("dynpred", quietly = TRUE)) {
    install.packages("dynpred")
    library(dynpred)
  }

  if (!require("Hmisc", quietly = TRUE)) {
    install.packages("Hmisc")
    library(Hmisc)
  }


  # ============================================
  # 2. Extract model components
  # ============================================

  TSet <- object$data
  Model <- object$Model

  if (is.null(seq_len)) {
    sl <- object$tw$sl
  } else {
    sl <- seq_len
  }

  nsl <- length(sl)

  id <- object$id
  w <- object$tw$w
  time <- object$time
  status <- object$status
  rtime <- object$rtime
  cov <- object$cov
  func_covars <- object$func_covars
  func_lms <- object$func_lms


  # ============================================
  # 3. Preserve original survival outcomes
  # ============================================

  # cutLM() will modify/truncate time and status.
  # Therefore, preserve the original external
  # survival time and event indicator for DCA.

  ex_data$original_time <- ex_data[[time]]
  ex_data$original_status <- ex_data[[status]]


  # ============================================
  # 4. Prepare landmark validation data
  # ============================================

  Vset <- NULL

  if (is.null(cov$vary)) {

    # ------------------------------------------
    # Only fixed covariates
    # ------------------------------------------

    fixed1 <- unique(
      c(
        id,
        cov$fixed,
        "original_time",
        "original_status"
      )
    )

    for (j in seq_along(sl)) {

      LM <- dynpred::cutLM(
        data = ex_data,
        outcome = list(
          time = time,
          status = status
        ),
        LM = sl[j],
        horizon = sl[j] + w,
        covs = list(
          fixed = fixed1,
          varying = cov$vary
        )
      )

      Vset <- rbind(
        Vset,
        LM
      )
    }

  } else {

    # ------------------------------------------
    # Time-varying covariates
    # ------------------------------------------

    fixed_dca <- unique(
      c(
        cov$fixed,
        "original_time",
        "original_status"
      )
    )

    for (j in seq_along(sl)) {

      LM <- dynpred::cutLM(
        data = ex_data,
        outcome = list(
          time = time,
          status = status
        ),
        LM = sl[j],
        horizon = sl[j] + w,
        covs = list(
          fixed = fixed_dca,
          varying = cov$vary
        ),
        format = "long",
        id = id,
        rtime = rtime,
        right = FALSE
      )

      Vset <- rbind(
        Vset,
        LM
      )
    }
  }


  # ============================================
  # 5. Order data by ID
  # ============================================

  Vset <- Vset[
    order(Vset[[id]]),
  ]


  # ============================================
  # 6. Add interaction terms
  # ============================================

  Vset1 <- list()
  Vset1$data <- Vset
  Vset1$lm_col <- "LM"

  Vset_i <- add_interactions(
    Vset1,
    c(cov$fixed, cov$vary),
    func_covars = func_covars,
    func_lms = func_lms,
    sl = sl
  )

  Vset_2 <- Vset_i$data


  # ============================================
  # 7. Initialize performance results
  # ============================================

  cindex <- score <- auc <- rep(
    NA,
    nsl
  )

  cal_pred <- data.frame()

  # Store DCA results
  dca_all <- list()


  # ============================================
  # 8. Evaluate each landmark time
  # ============================================

  for (i in seq_len(nsl)) {

    Vdata <- Vset_2[
      Vset_2$LM == sl[i],
    ]

    # Skip if no observations
    if (nrow(Vdata) == 0) {

      warning(
        paste(
          "No data available at landmark time",
          sl[i]
        )
      )

      next
    }


    # ------------------------------------------
    # C-index
    # ------------------------------------------

    cindex[i] <- cal_cindex(
      model = Model,
      data = Vdata,
      time,
      status
    )


    # ------------------------------------------
    # Brier score
    # ------------------------------------------

    score[i] <- cal_brierscore(
      model = Model,
      Tdata = TSet$data,
      Vdata = Vdata,
      width = w,
      tout = sl[i],
      time,
      status
    )


    # ------------------------------------------
    # AUC
    # ------------------------------------------

    auc[i] <- cal_auc(
      model = Model,
      data = Vdata,
      pred.t = sl[i] + w,
      time,
      status
    )


    # ==========================================
    # Individual predictions
    # ==========================================

    all_pred1 <- individual_predict(
      model = Model,
      data = Vdata,
      sl[i],
      w
    )


    # ==========================================
    # Calibration data
    # ==========================================

    all_surv <- cbind(
      Vdata[[id]],
      all_pred1,
      rep(sl[i], nrow(Vdata)),
      Vdata[[time]] - sl[i],
      Vdata[[status]]
    )

    colnames(all_surv) <- c(
      "id",
      "surv",
      "time_points",
      "auctual_time",
      "auctual_status"
    )

    cal_pred <- rbind(
      cal_pred,
      all_surv
    )


    # ==========================================
    # Decision curve analysis
    # ==========================================

    dca_tmp <- cal_dca(
      pred_surv = all_pred1,
      data = Vdata,
      s = sl[i],
      w = w,
      original_time = "original_time",
      original_status = "original_status",
      thresholds = dca_thresholds,
      model_label = "Landmark model"
    )

    if (!is.null(dca_tmp)) {

      dca_all[[length(dca_all) + 1]] <-
        dca_tmp
    }
  }


  # ============================================
  # 9. Calibration
  # ============================================

  cal_pred$occur_status <- ifelse(
    cal_pred$auctual_time < w &
      cal_pred$auctual_status == 1,
    1,
    0
  )

  all_cal_autual <- data.frame()


  for (i in unique(cal_pred$time_points)) {

    cal_pred_sub <- cal_pred[
      cal_pred$time_points == i,
    ]

    if (nrow(cal_pred_sub) < n_group) {

      warning(
        paste(
          "Insufficient data at time",
          i,
          "for binning:",
          nrow(cal_pred_sub),
          "obs vs required",
          n_group
        )
      )

      next
    }


    if (length(unique(cal_pred_sub$surv)) < 2) {

      warning(
        paste(
          "Insufficient surv value variation at time",
          i,
          "- skipping"
        )
      )

      next
    }


    # ------------------------------------------
    # Prediction groups
    # ------------------------------------------

    cal_pred_sub$pred_group <-
      Hmisc::cut2(
        cal_pred_sub$surv,
        g = n_group
      )

    bin_results <- data.frame()


    for (bin in levels(
      cal_pred_sub$pred_group
    )) {

      bin_data <- cal_pred_sub[
        cal_pred_sub$pred_group == bin,
      ]


      if (nrow(bin_data) < 3) {

        warning(
          paste(
            "Bin",
            bin,
            "has too few observations:",
            nrow(bin_data)
          )
        )

        next
      }


      # ----------------------------------------
      # Kaplan-Meier estimate
      # ----------------------------------------

      km_fit <- survival::survfit(
        survival::Surv(
          auctual_time,
          auctual_status
        ) ~ 1,
        data = bin_data
      )

      km_summary <- summary(
        km_fit,
        times = w,
        extend = TRUE
      )


      actual_survival <- ifelse(
        length(km_summary$surv) > 0,
        km_summary$surv[1],
        NA
      )

      se_survival <- ifelse(
        length(km_summary$std.err) > 0,
        km_summary$std.err[1],
        NA
      )


      if (!is.na(actual_survival) &&
          !is.na(se_survival)) {

        lower_survival <- max(
          0,
          actual_survival -
            1.96 * se_survival
        )

        upper_survival <- min(
          1,
          actual_survival +
            1.96 * se_survival
        )

      } else {

        lower_survival <-
          upper_survival <- NA
      }


      bin_results <- rbind(
        bin_results,
        data.frame(
          time_point = i,
          pred_group = bin,
          n_patients = nrow(bin_data),
          mean_predicted_survival =
            mean(bin_data$surv),
          actual_survival =
            actual_survival,
          lower_survival =
            lower_survival,
          upper_survival =
            upper_survival
        )
      )
    }


    all_cal_autual <- rbind(
      all_cal_autual,
      bin_results
    )
  }


  # ============================================
  # 10. Calibration slope
  # ============================================

  calibration_slope_with_ci <-
    calculate_calibration_slope(
      all_cal_autual
    )


  # ============================================
  # 11. Combine DCA results
  # ============================================

  if (length(dca_all) > 0) {

    dca_results <- do.call(
      rbind,
      dca_all
    )

    dca_results <- dca_results[
      order(
        dca_results$time_point,
        dca_results$variable,
        dca_results$threshold
      ),
    ]

  } else {

    dca_results <- NULL
  }


  # ============================================
  # 12. Return results
  # ============================================

  return(
    list(
      time = sl,
      cindex = cindex,
      BS = score,
      AUC = auc,
      calibration_slope_with_ci =
        calibration_slope_with_ci,
      all_cal_autual =
        all_cal_autual,
      dca_results =
        dca_results
    )
  )
}
