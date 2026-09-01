#' External Validation for Joint Models
#'
#' This function performs external validation of a joint model on an external
#' dataset. It evaluates discrimination (C-index and AUC), overall prediction
#' error (Brier score), calibration, and clinical utility using decision curve
#' analysis (DCA) at multiple prediction time points.
#'
#' @usage predictive_JM_ex(
#'   object,
#'   newdata,
#'   seq_len,
#'   w,
#'   n_group = 10,
#'   dca_thresholds = seq(0.05, 0.50, by = 0.01)
#' )
#'
#' @param object A fitted joint model object from the \code{jm} function.
#'
#' @param newdata A data.frame containing the external validation data.
#'
#' @param seq_len Numeric vector specifying the prediction time points at which
#'   model performance is evaluated.
#'
#' @param w Numeric value specifying the prediction window length.
#'
#' @param n_group Integer specifying the number of groups used for calibration
#'   assessment. Default is 10.
#'
#' @param dca_thresholds Numeric vector specifying the threshold probabilities
#'   used for decision curve analysis (DCA). The default is
#'   \code{seq(0.05, 0.50, by = 0.01)}.
#'
#' @return A list containing:
#' \itemize{
#'   \item{time: The sequence of prediction time points used for evaluation.}
#'   \item{cindex: Concordance index values for each prediction time point.}
#'   \item{BS: Brier score values for each prediction time point.}
#'   \item{AUC: Time-dependent AUC values for each prediction time point.}
#'   \item{calibration_slope_with_ci: Calibration slope estimates with
#'     confidence intervals.}
#'   \item{all_cal_autual: Complete calibration results.}
#'   \item{dca_results: Decision curve analysis results, including threshold
#'     probabilities and net benefit for the joint model, treat-all strategy,
#'     and treat-none strategy.}
#' }
#'
#' @importFrom JMbayes2 tvBrier tvAUC
#' @export
#'
predictive_JM_ex <- function(
    object,
    newdata,
    seq_len,
    w,
    n_group = 10,
    dca_thresholds = seq(0.05, 0.50, by = 0.01)
) {

  c_index <- Brier_score <- AUC <- c()

  all_cal_autual <- data.frame()

  # Store DCA results
  dca_all <- list()


  for (i in seq_along(seq_len)) {

    timepoint <- seq_len[i]


    # ==========================================
    # 1. Predictive performance
    # ==========================================

    c_index[i] <- tvC_index(
      object,
      newdata,
      Tstart = timepoint,
      Dt = w
    )

    Brier_score[i] <- tvBrier(
      object,
      newdata,
      Tstart = timepoint,
      Dt = w
    )$Brier

    AUC[i] <- tvAUC(
      object,
      newdata,
      Tstart = timepoint,
      Dt = w
    )$auc


    # ==========================================
    # 2. Calibration
    # ==========================================

    cal_autual_all <- calibration_re_JM(
      object = object,
      data = newdata,
      Tstart = timepoint,
      Dt = w,
      n_groups = n_group
    )$cal_results

    all_cal_autual <- rbind(
      all_cal_autual,
      cal_autual_all
    )


    # ==========================================
    # 3. Decision curve analysis
    # ==========================================

    pred_dca <- individual_JM_predict(
      object,
      newdata,
      Tstart = timepoint,
      Dt = w
    )

    dca_tmp <- cal_dca(
      pred_surv = pred_dca$preds,
      data = pred_dca,
      s = timepoint,
      w = w,
      original_time = "Time",
      original_status = "event",
      thresholds = dca_thresholds,
      model_label = "Joint model"
    )

    if (!is.null(dca_tmp)) {

      dca_all[[length(dca_all) + 1]] <-
        dca_tmp

    }
  }


  # ==========================================
  # 4. Calibration slope
  # ==========================================

  calibration_slope_with_ci <-
    calculate_calibration_slope(
      all_cal_autual
    )


  # ==========================================
  # 5. Combine DCA results
  # ==========================================

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


  # ==========================================
  # 6. Return results
  # ==========================================

  return(
    list(
      time = seq_len,
      cindex = c_index,
      BS = Brier_score,
      AUC = AUC,
      calibration_slope_with_ci =
        calibration_slope_with_ci,
      all_cal_autual = all_cal_autual,
      dca_results = dca_results
    )
  )
}
