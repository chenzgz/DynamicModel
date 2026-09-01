#' Plot Decision Curve Analysis Results
#'
#' This function plots decision curve analysis (DCA) results for dynamic
#' prediction models at selected prediction time points. It displays the
#' net benefit across a range of threshold probabilities and compares the
#' prediction model with the treat-all and treat-none strategies.
#' The function allows flexible customization of curve colors, line types,
#' axis ranges, axis labels, font sizes, legend position, and facet layout.
#'
#' @usage plot_DCA(
#'   dca_results,
#'   time_points = NULL,
#'   model_label = NULL,
#'   colors = NULL,
#'   linetypes = NULL,
#'   linewidth = 1.1,
#'   xlim = NULL,
#'   ylim = NULL,
#'   x_breaks = NULL,
#'   y_breaks = NULL,
#'   x_percent = TRUE,
#'   percent_accuracy = 1,
#'   xlab = "Threshold probability",
#'   ylab = "Net benefit",
#'   axis_title_size = 16,
#'   axis_title_face = "bold",
#'   axis_text_size = 13,
#'   axis_text_color = "black",
#'   strip_text_size = 14,
#'   strip_text_face = "bold",
#'   legend_position = "bottom",
#'   legend_text_size = 13,
#'   legend_title = NULL,
#'   family = "sans",
#'   base_size = 15,
#'   facet_nrow = 1,
#'   facet_ncol = NULL,
#'   panel_spacing = 1,
#'   show_treat_all = TRUE,
#'   show_treat_none = TRUE,
#'   title = NULL,
#'   title_size = 16,
#'   title_face = "bold"
#' )
#'
#' @param dca_results A data.frame containing decision curve analysis results.
#'   The data should include the variables \code{time_point},
#'   \code{variable}, \code{label}, \code{threshold}, and
#'   \code{net_benefit}.
#'
#' @param time_points Numeric vector specifying the prediction time points to
#'   be displayed. If \code{NULL}, all available prediction time points are
#'   plotted.
#'
#' @param model_label Character string specifying the name of the prediction
#'   model displayed in the legend, such as \code{"Landmark model"} or
#'   \code{"Joint model"}. If \code{NULL}, the model label is automatically
#'   obtained from \code{dca_results}.
#'
#' @param colors Named character vector specifying the colors of the prediction
#'   model, treat-all strategy, and treat-none strategy. If \code{NULL},
#'   default colors are used.
#'
#' @param linetypes Named character vector specifying the line types of the
#'   prediction model, treat-all strategy, and treat-none strategy.
#'   If \code{NULL}, default line types are used.
#'
#' @param linewidth Numeric value specifying the width of the decision curves.
#'   Default is 1.1.
#'
#' @param xlim Numeric vector of length two specifying the lower and upper
#'   limits of the x-axis. If \code{NULL}, the range is determined
#'   automatically.
#'
#' @param ylim Numeric vector of length two specifying the lower and upper
#'   limits of the y-axis. If \code{NULL}, the range is determined
#'   automatically.
#'
#' @param x_breaks Numeric vector specifying the breaks on the x-axis.
#'   If \code{NULL}, breaks are determined automatically.
#'
#' @param y_breaks Numeric vector specifying the breaks on the y-axis.
#'   If \code{NULL}, breaks are determined automatically.
#'
#' @param x_percent Logical indicating whether threshold probabilities on
#'   the x-axis should be displayed as percentages. Default is \code{TRUE}.
#'
#' @param percent_accuracy Numeric value specifying the rounding accuracy
#'   used when threshold probabilities are displayed as percentages.
#'   Default is 1.
#'
#' @param xlab Character string specifying the x-axis title.
#'   Default is \code{"Threshold probability"}.
#'
#' @param ylab Character string specifying the y-axis title.
#'   Default is \code{"Net benefit"}.
#'
#' @param axis_title_size Numeric value specifying the font size of the
#'   x- and y-axis titles. Default is 16.
#'
#' @param axis_title_face Character string specifying the font face of the
#'   axis titles. Common options include \code{"plain"}, \code{"bold"},
#'   and \code{"italic"}. Default is \code{"bold"}.
#'
#' @param axis_text_size Numeric value specifying the font size of the
#'   axis tick labels. Default is 13.
#'
#' @param axis_text_color Character string specifying the color of the
#'   axis tick labels. Default is \code{"black"}.
#'
#' @param strip_text_size Numeric value specifying the font size of the
#'   facet labels representing prediction time points. Default is 14.
#'
#' @param strip_text_face Character string specifying the font face of the
#'   facet labels. Default is \code{"bold"}.
#'
#' @param legend_position Character string specifying the legend position.
#'   Common options include \code{"bottom"}, \code{"top"}, \code{"left"},
#'   \code{"right"}, and \code{"none"}. Default is \code{"bottom"}.
#'
#' @param legend_text_size Numeric value specifying the font size of the
#'   legend text. Default is 13.
#'
#' @param legend_title Character string specifying the legend title.
#'   If \code{NULL}, no legend title is displayed.
#'
#' @param family Character string specifying the font family used in the plot.
#'   Default is \code{"sans"}.
#'
#' @param base_size Numeric value specifying the base font size used by the
#'   plotting theme. Default is 15.
#'
#' @param facet_nrow Integer specifying the number of rows used to arrange
#'   multiple prediction time panels. Default is 1.
#'
#' @param facet_ncol Integer specifying the number of columns used to arrange
#'   multiple prediction time panels. If \code{NULL}, the number of columns
#'   is determined automatically.
#'
#' @param panel_spacing Numeric value specifying the spacing between facet
#'   panels, expressed in lines. Default is 1.
#'
#' @param show_treat_all Logical indicating whether the treat-all strategy
#'   should be displayed. Default is \code{TRUE}.
#'
#' @param show_treat_none Logical indicating whether the treat-none strategy
#'   should be displayed. Default is \code{TRUE}.
#'
#' @param title Character string specifying the overall plot title.
#'   If \code{NULL}, no title is displayed.
#'
#' @param title_size Numeric value specifying the font size of the overall
#'   plot title. Default is 16.
#'
#' @param title_face Character string specifying the font face of the overall
#'   plot title. Default is \code{"bold"}.
#'
#' @return A \code{ggplot} object displaying decision curves of the prediction
#'   model together with the treat-all and treat-none strategies across
#'   threshold probabilities at the selected prediction time points.
#'
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap labs
#' @importFrom ggplot2 scale_color_manual scale_linetype_manual
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 coord_cartesian theme_classic theme
#' @importFrom ggplot2 element_text element_blank waiver
#' @importFrom scales percent_format
#' @importFrom grid unit
#'
#' @export
plot_DCA <- function(
    dca_results,
    time_points = NULL,
    model_label = NULL,

    # ---------- Curve settings ----------
    colors = NULL,
    linetypes = NULL,
    linewidth = 1.1,

    # ---------- Axis ranges ----------
    xlim = NULL,
    ylim = NULL,

    # ---------- Axis breaks ----------
    x_breaks = NULL,
    y_breaks = NULL,
    x_percent = TRUE,
    percent_accuracy = 1,

    # ---------- Axis labels ----------
    xlab = "Threshold probability",
    ylab = "Net benefit",
    axis_title_size = 16,
    axis_title_face = "bold",
    axis_text_size = 13,
    axis_text_color = "black",

    # ---------- Facet labels ----------
    strip_text_size = 14,
    strip_text_face = "bold",

    # ---------- Legend ----------
    legend_position = "bottom",
    legend_text_size = 13,
    legend_title = NULL,

    # ---------- Font ----------
    family = "sans",
    base_size = 15,

    # ---------- Facets ----------
    facet_nrow = 1,
    facet_ncol = NULL,
    panel_spacing = 1,

    # ---------- Reference strategies ----------
    show_treat_all = TRUE,
    show_treat_none = TRUE,

    # ---------- Title ----------
    title = NULL,
    title_size = 16,
    title_face = "bold"
) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }

  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("Package 'scales' is required.")
  }


  # ============================================
  # 1. Check required variables
  # ============================================

  required_vars <- c(
    "time_point",
    "variable",
    "label",
    "threshold",
    "net_benefit"
  )

  missing_vars <- setdiff(
    required_vars,
    names(dca_results)
  )

  if (length(missing_vars) > 0) {
    stop(
      "The following variables are missing from dca_results: ",
      paste(missing_vars, collapse = ", ")
    )
  }


  # ============================================
  # 2. Select prediction times
  # ============================================

  if (!is.null(time_points)) {

    dca_results <- dca_results[
      dca_results$time_point %in% time_points,
    ]

  }

  if (nrow(dca_results) == 0) {
    stop(
      "No DCA results are available for the selected prediction times."
    )
  }


  # ============================================
  # 3. Determine model label
  # ============================================

  # If model_label is not specified,
  # obtain it automatically from DCA results
  if (is.null(model_label)) {

    model_labels <- unique(
      as.character(
        dca_results$label[
          dca_results$variable == "risk"
        ]
      )
    )

    model_labels <- model_labels[
      !is.na(model_labels) &
        model_labels != ""
    ]

    if (length(model_labels) >= 1) {

      model_label <- model_labels[1]

    } else {

      model_label <- "Prediction model"

    }
  }


  # ============================================
  # 4. Rename DCA strategies
  # ============================================

  dca_results$method <- ifelse(
    dca_results$variable == "risk",
    model_label,
    ifelse(
      dca_results$variable == "all",
      "Treat all",
      ifelse(
        dca_results$variable == "none",
        "Treat none",
        as.character(dca_results$label)
      )
    )
  )


  # ============================================
  # 5. Select strategies
  # ============================================

  if (!show_treat_all) {

    dca_results <- dca_results[
      dca_results$method != "Treat all",
    ]

  }

  if (!show_treat_none) {

    dca_results <- dca_results[
      dca_results$method != "Treat none",
    ]

  }


  # ============================================
  # 6. Set order of curves
  # ============================================

  method_order <- c(
    model_label,
    "Treat all",
    "Treat none"
  )

  method_order <- method_order[
    method_order %in%
      unique(dca_results$method)
  ]

  dca_results$method <- factor(
    dca_results$method,
    levels = method_order
  )


  # ============================================
  # 7. Default colors
  # ============================================

  if (is.null(colors)) {

    colors <- stats::setNames(
      c(
        "#000000",
        "#666666",
        "#999999"
      ),
      c(
        model_label,
        "Treat all",
        "Treat none"
      )
    )
  }


  # ============================================
  # 8. Default line types
  # ============================================

  if (is.null(linetypes)) {

    linetypes <- stats::setNames(
      c(
        "solid",
        "dashed",
        "dotdash"
      ),
      c(
        model_label,
        "Treat all",
        "Treat none"
      )
    )
  }


  # ============================================
  # 9. Prediction-time labels
  # ============================================

  time_order <- sort(
    unique(dca_results$time_point)
  )

  # Automatically distinguish year / years
  make_time_label <- function(x) {

    ifelse(
      x == 1,
      paste0("Prediction time = ", x, " year"),
      paste0("Prediction time = ", x, " years")
    )

  }

  dca_results$time_label <-
    make_time_label(
      dca_results$time_point
    )

  dca_results$time_label <- factor(
    dca_results$time_label,
    levels = make_time_label(time_order)
  )


  # ============================================
  # 10. Basic figure
  # ============================================

  p <- ggplot2::ggplot(
    dca_results,
    ggplot2::aes(
      x = threshold,
      y = net_benefit,
      color = method,
      linetype = method
    )
  ) +

    ggplot2::geom_line(
      linewidth = linewidth,
      na.rm = TRUE
    ) +

    ggplot2::facet_wrap(
      ~ time_label,
      nrow = facet_nrow,
      ncol = facet_ncol
    ) +

    ggplot2::labs(
      x = xlab,
      y = ylab,
      color = legend_title,
      linetype = legend_title,
      title = title
    )


  # ============================================
  # 11. Curve colors
  # ============================================

  used_colors <- colors[
    names(colors) %in% method_order
  ]

  p <- p +
    ggplot2::scale_color_manual(
      values = used_colors,
      breaks = method_order
    )


  # ============================================
  # 12. Line types
  # ============================================

  used_linetypes <- linetypes[
    names(linetypes) %in% method_order
  ]

  p <- p +
    ggplot2::scale_linetype_manual(
      values = used_linetypes,
      breaks = method_order
    )


  # ============================================
  # 13. X-axis
  # ============================================

  if (x_percent) {

    p <- p +
      ggplot2::scale_x_continuous(
        breaks = if (is.null(x_breaks)) {
          ggplot2::waiver()
        } else {
          x_breaks
        },
        labels = scales::percent_format(
          accuracy = percent_accuracy
        )
      )

  } else {

    p <- p +
      ggplot2::scale_x_continuous(
        breaks = if (is.null(x_breaks)) {
          ggplot2::waiver()
        } else {
          x_breaks
        }
      )
  }


  # ============================================
  # 14. Y-axis
  # ============================================

  if (!is.null(y_breaks)) {

    p <- p +
      ggplot2::scale_y_continuous(
        breaks = y_breaks
      )
  }


  # ============================================
  # 15. Coordinate ranges
  # ============================================

  if (!is.null(xlim) ||
      !is.null(ylim)) {

    p <- p +
      ggplot2::coord_cartesian(
        xlim = xlim,
        ylim = ylim
      )
  }


  # ============================================
  # 16. Theme
  # ============================================

  p <- p +

    ggplot2::theme_classic(
      base_size = base_size,
      base_family = family
    ) +

    ggplot2::theme(

      axis.title = ggplot2::element_text(
        size = axis_title_size,
        face = axis_title_face,
        family = family
      ),

      axis.text = ggplot2::element_text(
        size = axis_text_size,
        colour = axis_text_color,
        family = family
      ),

      strip.background =
        ggplot2::element_blank(),

      strip.text =
        ggplot2::element_text(
          size = strip_text_size,
          face = strip_text_face,
          family = family
        ),

      legend.position =
        legend_position,

      legend.text =
        ggplot2::element_text(
          size = legend_text_size,
          family = family
        ),

      legend.title =
        ggplot2::element_text(
          size = legend_text_size,
          family = family
        ),

      plot.title =
        ggplot2::element_text(
          size = title_size,
          face = title_face,
          family = family,
          hjust = 0.5
        ),

      panel.spacing =
        grid::unit(
          panel_spacing,
          "lines"
        )
    )

  return(p)
}
