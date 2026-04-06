library(data.table)
library(ggplot2)
library(caret)
library(ROCR)
library(kableExtra)
library(gridExtra)
library(grid)
library(parallel)
library(doParallel)
library(e1071)


#' @title
#' Initialize Parallel Backend
#'
#' @description
#' Registers a PSOCK cluster using all available cores minus one.
#'
#' @return \code{cl} (\code{SOCKcluster}) --- The cluster object.  Pass to
#'   \code{parallel::stopCluster()} when finished.
#'
#' @keywords internal
setup_parallel <- function() {
  n_cores <- parallel::detectCores() - 1
  cl      <- parallel::makePSOCKcluster(names = n_cores)
  doParallel::registerDoParallel(cl = cl)
  message("Registered parallel backend with ", n_cores, " cores.")
  return(cl)
}


#' @title
#' Load Training Data
#'
#' @description
#' Reads \code{HaitiPixels.csv} via \code{data.table::fread} and converts the
#' multi-class \code{Class} column into a binary \code{Tarp} factor.
#'
#' @param path \code{chr} --- Path to the training CSV.
#'   Defaults to \code{data/HaitiPixels.csv} relative to the project root.
#'
#' @return \code{data.frame} --- Columns \code{Red}, \code{Green},
#'   \code{Blue}, \code{Tarp} (factor with levels \code{No}, \code{Yes}).
#'
#' @keywords internal
load_training_data <- function(path = here::here("data", "HaitiPixels.csv")) {
  dt <- data.table::fread(input = path)
  stopifnot(
    "Training CSV must contain columns: Class, Red, Green, Blue" =
      all(c("Class", "Red", "Green", "Blue") %in% names(dt)),
    "Training data contains NAs in color columns" =
      !anyNA(dt[, .(Red, Green, Blue)])
  )
  dt[, Tarp := factor(
    x      = data.table::fifelse(test = Class == "Blue Tarp", yes = "Yes", no = "No"),
    levels = c("No", "Yes")
  )]
  dt[, Class := NULL]
  return(as.data.frame(dt))
}

#' @title
#' Load Holdout Data
#'
#' @description
#' Reads multiple holdout text files from a directory, combines them via
#' \code{data.table::rbindlist}, and assigns \code{Tarp} labels based on
#' whether the filename contains \code{"NON"} or \code{"NOT"}.
#'
#' @param dir \code{chr} --- Directory containing the holdout \code{.txt} files.
#'   Defaults to \code{data/holdout} relative to the project root.
#'
#' @return \code{data.frame} --- Columns \code{Red}, \code{Green},
#'   \code{Blue}, \code{Tarp} (factor with levels \code{No}, \code{Yes}).
#'
#' @keywords internal
load_holdout_data <- function(dir = here::here("data")) {
  files <- list.files(path = dir, pattern = "\\.txt$", full.names = TRUE)
  stopifnot("No .txt holdout files found in directory" = length(files) > 0)

  holdout <- data.table::rbindlist(l = lapply(files, function(file) {
    lines <- readLines(con = file)[-c(1:8)]
    dt    <- data.table::fread(text = paste(lines, collapse = "\n"), drop = 1L)
    data.table::setnames(
      x   = dt,
      new = c("X", "Y", "MapX", "MapY", "Lat", "Lon", "Red", "Green", "Blue")
    )
    dt[, Tarp := data.table::fifelse(
      test = grepl(pattern = "NON|NOT", x = file),
      yes  = "No",
      no   = "Yes"
    )]
    dt
  }))
  
  holdout[, Tarp := factor(x = Tarp, levels = c("No", "Yes"))]
  holdout[, c("X", "Y", "MapX", "MapY", "Lat", "Lon") := NULL]
  stopifnot(
    "Holdout data must contain columns: Red, Green, Blue" =
      all(c("Red", "Green", "Blue") %in% names(holdout)),
    "Holdout data contains NAs in color columns" =
      !anyNA(holdout[, .(Red, Green, Blue)])
  )
  return(as.data.frame(holdout))
}


#' @title
#' Create Cross-Validation Control
#'
#' @description
#' Builds a \code{caret::trainControl} object for 10-fold stratified CV
#' with AUC as the summary metric.
#'
#' @param data \code{data.frame} --- Training data with a \code{Tarp} column.
#' @param seed \code{int} --- Random seed for fold creation. Defaults to \code{1}.
#'
#' @return \code{out} (\code{list}) --- Components:
#'   \describe{
#'     \item{ctrl}{\code{trainControl} --- The control object.}
#'     \item{folds}{\code{list} --- Named list of training-row indices.}
#'   }
#'
#' @keywords internal
make_cv_control <- function(data, seed = 1) {
  set.seed(seed)
  folds <- caret::createFolds(y = data$Tarp, k = 10, list = TRUE, returnTrain = TRUE)
  
  ctrl <- caret::trainControl(
    method          = "cv",
    number          = 10,
    index           = folds,
    classProbs      = TRUE,
    allowParallel   = TRUE,
    savePredictions = TRUE,
    summaryFunction = caret::twoClassSummary
  )
  
  return(list(ctrl = ctrl, folds = folds))
}


#' @title
#' Compute Performance Metrics Across Thresholds
#'
#' @description
#' Evaluates a trained model at a range of probability thresholds and
#' returns balanced accuracy, kappa, sensitivity, specificity, precision,
#' F1, and false positive rate for each.
#'
#' @param model \code{train} --- A \code{caret::train} object with \code{savePredictions = TRUE}.
#' @param thresholds \code{dbl} --- Numeric vector of thresholds. Defaults to \code{seq(0.1, 0.9, by = 0.1)}.
#'
#' @return \code{data.frame} --- One row per threshold with metric columns.
#'
#' @keywords internal
compute_thresholds <- function(model, thresholds = seq(from = 0.1, to = 0.9, by = 0.1)) {
  stopifnot(
    "model must be a caret train object" = inherits(model, "train"),
    "thresholds must be in [0, 1]"       = all(thresholds >= 0 & thresholds <= 1)
  )
  stats <- c("Balanced Accuracy", "Kappa", "Sensitivity", "Specificity", "Precision", "F1")
  
  set.seed(1)
  values     <- caret::thresholder(x = model, threshold = thresholds, statistics = stats)
  values$FPR <- round(x = 1 - values$Specificity, digits = 4)
  
  drop_cols <- intersect(
    x = names(values),
    y = c("parameter", "k", "alpha", "lambda", "mtry", "C", "sigma")
  )
  if (length(drop_cols) > 0) {
    values <- values[, !(names(values) %in% drop_cols), drop = FALSE]
  }
  
  names(values)[names(values) == "prob_threshold"] <- "Threshold"
  return(values)
}


#' @title
#' Train a Model With Timing
#'
#' @description
#' Wraps \code{caret::train} with \code{system.time} so elapsed seconds
#' are captured alongside the fitted model.
#'
#' @param formula \code{formula} --- Model formula (e.g., \code{Tarp ~ Red + Blue + Green}).
#' @param data \code{data.frame} --- Training data.
#' @param method \code{chr} --- A \code{caret} method string (e.g., \code{"glm"}, \code{"svmRadial"}).
#' @param ctrl \code{trainControl} --- Control object from \code{make_cv_control()}.
#' @param ... Additional arguments forwarded to \code{caret::train()}.
#'
#' @return \code{out} (\code{list}) --- Components:
#'   \describe{
#'     \item{model}{\code{train} --- The fitted caret model.}
#'     \item{elapsed}{\code{dbl} --- Wall-clock seconds.}
#'   }
#'
#' @keywords internal
train_timed <- function(formula, data, method, ctrl, ...) {
  stopifnot(
    "formula must be a formula object"              = inherits(formula, "formula"),
    "method must be a non-empty character string"    = is.character(method) && nchar(method) > 0
  )
  set.seed(1)
  elapsed <- system.time(expr = {
    model <- caret::train(
      form      = formula,
      data      = data,
      method    = method,
      metric    = "ROC",
      trControl = ctrl,
      ...
    )
  })[["elapsed"]]
  
  return(list(model = model, elapsed = elapsed))
}


#' @title
#' Color Channel Density Plot
#'
#' @description
#' Creates a density plot of a single color channel split by \code{Tarp}
#' status, with rug marks and dashed mean lines.
#'
#' @param data \code{data.frame} --- Must contain the column named in
#'   \code{x_var} and a \code{Tarp} factor.
#' @param x_var \code{chr} --- Column name to plot on the x-axis.
#' @param title \code{chr} --- Plot title.
#'
#' @return \code{ggplot} --- A ggplot2 density plot.
#'
#' @keywords internal
plot_color_density <- function(data, x_var, title) {
  group_means <- stats::aggregate(
    stats::as.formula(paste(x_var, "~ Tarp")),
    data = data,
    FUN  = mean
  )
  
  ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = .data[[x_var]], fill = Tarp, color = Tarp)) +
    ggplot2::geom_density(alpha = 0.1) +
    ggplot2::geom_rug(alpha = 0.3) +
    ggplot2::geom_vline(
      data      = group_means,
      mapping   = ggplot2::aes(xintercept = .data[[x_var]], color = Tarp),
      linetype  = "dashed",
      linewidth = 0.6
    ) +
    ggplot2::scale_fill_manual(values = c("#141E3C", "#eb5f0c")) +
    ggplot2::scale_color_manual(values = c("#141E3C", "#eb5f0c")) +
    ggplot2::labs(title = title) +
    ggplot2::theme_linedraw()
}


#' @title
#' Single Metric vs. Threshold Line Chart
#'
#' @description
#' Draws a line-and-point chart of one performance metric across
#' probability thresholds.
#'
#' @param data \code{data.frame} --- Output of \code{compute_thresholds()}.
#' @param y_var \code{chr} --- Column name for the y-axis metric.
#' @param title \code{chr} --- Plot title.
#'
#' @return \code{ggplot} --- A ggplot2 line chart.
#'
#' @keywords internal
plot_metric_vs_threshold <- function(data, y_var, title) {
  ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = Threshold, y = .data[[y_var]])) +
    ggplot2::geom_line(color = "#232D4B", linewidth = 1) +
    ggplot2::geom_point(color = "#E57200", size = 2) +
    ggplot2::labs(title = title, x = "Threshold", y = NULL) +
    ggplot2::scale_x_continuous(breaks = seq(from = 0.1, to = 0.9, by = 0.2)) +
    ggplot2::theme_linedraw() +
    ggplot2::theme(
      axis.text    = ggplot2::element_text(face = "bold"),
      plot.title   = ggplot2::element_text(face = "bold", hjust = 0.5),
      axis.title.x = ggplot2::element_text(face = "bold")
    )
}


#' @title
#' Threshold Metrics Grid
#'
#' @description
#' Arranges four threshold-vs-metric plots (FPR, Balanced Accuracy, Kappa,
#' F1) in a 2x2 grid with a shared title.
#'
#' @param thresh_data \code{data.frame} --- Output of \code{compute_thresholds()}.
#' @param model_name \code{chr} --- Label for the figure title (e.g., \code{"Logistic"}).
#' @param subtitle \code{chr} --- Optional annotation string
#'   (e.g., hyperparameter values).  Defaults to \code{NULL}.
#'
#' @return Invisible \code{NULL}; called for its side effect of drawing.
#'
#' @keywords internal
plot_threshold_grid <- function(thresh_data, model_name, subtitle = NULL) {
  p1 <- plot_metric_vs_threshold(thresh_data, "FPR", "FPR")
  p2 <- plot_metric_vs_threshold(thresh_data, "Balanced Accuracy", "Balanced Accuracy")
  p3 <- plot_metric_vs_threshold(thresh_data, "Kappa", "Kappa")
  p4 <- plot_metric_vs_threshold(thresh_data, "F1", "F1")
  
  plots <- lapply(X = list(p1, p2, p3, p4), FUN = function(p) {
    p + ggplot2::labs(x = NULL, y = NULL)
  })
  
  gridExtra::grid.arrange(
    p1, p2, p3, p4,
    ncol = 2,
    top  = grid::textGrob(
      label = paste0("Summary Statistics by Threshold (", model_name, ")"),
      gp    = grid::gpar(fontsize = 14, font = 2, hjust = 0.5)
    ),
    bottom = grid::textGrob(
      label = "Threshold",
      gp    = grid::gpar(fontsize = 13, font = 2, hjust = 0.5)
    ),
    grobs = plots
  )
  
  if (!is.null(subtitle)) {
    grid::grid.text(
      label = subtitle,
      x     = 0.5,
      y     = 0.90,
      gp    = grid::gpar(fontsize = 10, font = 2)
    )
  }
}


#' @title
#' Compute Per-Fold AUC Values
#'
#' @description
#' Iterates over the resampling folds stored in a \code{caret} model and
#' computes ROCR prediction objects and AUC values for each fold.
#'
#' @param model \code{train} --- A fitted caret model with resampling indices.
#' @param data \code{data.frame} --- The training data used to fit \code{model}.
#'
#' @return \code{list} --- Components:
#'   \describe{
#'     \item{roc_data}{\code{list} --- ROCR prediction objects, one per fold.}
#'     \item{auc_values}{\code{dbl} --- AUC percentages, one per fold.}
#'     \item{resamps}{\code{list} --- Named resampling index lists.}
#'   }
#'
#' @keywords internal
compute_fold_aucs <- function(model, data) {
  resamps        <- model$control$index
  names(resamps) <- paste("Fold", seq_along(resamps))

  roc_data   <- vector(mode = "list", length = length(resamps))
  auc_values <- numeric(length = length(resamps))

  for (i in seq_along(resamps)) {
    fold_data     <- data[resamps[[i]], ]
    pred_probs    <- stats::predict(object = model, newdata = fold_data, type = "prob")[, "Yes"]
    roc_data[[i]] <- ROCR::prediction(predictions = pred_probs, labels = fold_data$Tarp)
    auc_values[i] <- round(
      x      = as.numeric(ROCR::performance(roc_data[[i]], measure = "auc")@y.values),
      digits = 5
    ) * 100
  }

  list(roc_data = roc_data, auc_values = auc_values, resamps = resamps)
}


#' @title
#' Compute Out-of-Fold ROC Curve and AUC
#'
#' @description
#' Extracts out-of-fold predictions from a \code{caret} model at the best
#' hyperparameters and computes the aggregate ROC curve and AUC via ROCR.
#'
#' @param model \code{train} --- A fitted caret model with \code{savePredictions = TRUE}.
#'
#' @return \code{list} --- Components:
#'   \describe{
#'     \item{roc}{\code{performance} --- ROCR performance object (TPR vs FPR).}
#'     \item{auc}{\code{dbl} --- AUC as a percentage.}
#'   }
#'
#' @keywords internal
compute_oof_roc <- function(model) {
  best  <- model$bestTune
  preds <- model$pred
  for (nm in names(best)) {
    preds <- preds[preds[[nm]] == best[[nm]], ]
  }
  preds <- preds[complete.cases(preds[, c("No", "obs")]), ]

  rates <- ROCR::prediction(
    predictions    = preds$No,
    labels         = preds$obs,
    label.ordering = c("Yes", "No")
  )
  avg_roc <- ROCR::performance(prediction.obj = rates, measure = "tpr", x.measure = "fpr")
  avg_auc <- round(
    x      = as.numeric(ROCR::performance(rates, measure = "auc")@y.values),
    digits = 5
  ) * 100

  list(roc = avg_roc, auc = avg_auc)
}


#' @title
#' Per-Fold ROC Curves
#'
#' @description
#' Plots an overall out-of-fold ROC curve (bold) with individual fold
#' curves overlaid, plus AUC values in the legend.
#'
#' @param model \code{train} --- A fitted caret model.
#' @param data \code{data.frame} --- The training data used to fit \code{model}.
#' @param title \code{chr} --- Plot title.  Defaults to \code{"ROC Curves"}.
#' @param note \code{chr} --- Optional annotation below the title. Defaults to \code{NULL}.
#' @param xlim \code{dbl} --- X-axis limits.  Defaults to \code{c(0, 1)}.
#' @param ylim \code{dbl} --- Y-axis limits.  Defaults to \code{c(0, 1)}.
#'
#' @return Invisible \code{NULL}; called for its side effect of drawing.
#'
#' @keywords internal
plot_roc_curves <- function(model, data, title = "ROC Curves", note = NULL, xlim = c(0, 1), ylim = c(0, 1)) {

  fold_info <- compute_fold_aucs(model, data)
  oof       <- compute_oof_roc(model)

  graphics::plot(
    x        = oof$roc,
    col      = "#232D4B",
    lwd      = 2.5,
    main     = title,
    xlim     = xlim,
    ylim     = ylim,
    xlab     = "False Positive Rate",
    ylab     = "True Positive Rate",
    font.lab = 2,
    bg       = "grey"
  )

  legend_text  <- paste0("OOF (AUC = ", format(oof$auc, nsmall = 3), ")")
  fold_colors  <- grDevices::rainbow(n = length(fold_info$resamps) - 1)
  fold_counter <- 0

  for (i in seq_along(fold_info$resamps)) {
    if (!is.null(fold_info$roc_data[[i]])) {
      fold_perf <- ROCR::performance(
        prediction.obj = fold_info$roc_data[[i]],
        measure        = "tpr",
        x.measure      = "fpr"
      )
      legend_text <- c(
        legend_text,
        paste0(
          names(fold_info$resamps)[i], " (AUC = ",
          format(round(fold_info$auc_values[i], digits = 5), nsmall = 3), ")"
        )
      )
      if (i != 1) {
        fold_counter <- fold_counter + 1
        graphics::lines(
          x   = fold_perf@x.values[[1]],
          y   = fold_perf@y.values[[1]],
          col = fold_colors[fold_counter],
          lwd = 0.8
        )
      }
    }
  }

  graphics::lines(x = xlim, y = ylim, col = "#E57200")

  graphics::legend(
    "bottomright",
    legend    = legend_text,
    col       = c("#232D4B", fold_colors),
    lwd       = 1.5,
    cex       = 0.95,
    bg        = "grey95",
    text.font = 2
  )

  if (!is.null(note)) {
    graphics::mtext(text = note, side = 3, font = 2, cex = 0.9)
  }
}


#' @title
#' Confusion Matrix Heatmap
#'
#' @description
#' Renders a \code{caret::confusionMatrix} as a ggplot2 tile plot with
#' frequency labels.
#'
#' @param conf_matrix \code{confusionMatrix} --- Output of \code{caret::confusionMatrix()}.
#' @param title \code{chr} --- Plot title.
#'
#' @return \code{ggplot} --- A ggplot2 tile plot.
#'
#' @keywords internal
plot_confusion_matrix <- function(conf_matrix, title) {
  conf_tab      <- as.data.frame(conf_matrix$table)
  conf_tab$Freq <- round(x = conf_tab$Freq, digits = 1)
  
  ggplot2::ggplot(
    data = conf_tab,
    mapping = ggplot2::aes(y = Prediction, x = Reference, fill = Freq)
  ) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(low = "white", high = "#ea9c4e") +
    ggplot2::labs(title = title) +
    ggplot2::geom_text(
      mapping = ggplot2::aes(label = Freq, fontface = "bold"),
      color   = "black",
      size    = 4
    ) +
    ggplot2::geom_tile(color = "black", fill = "black", alpha = 0) +
    ggplot2::theme_linedraw() +
    ggplot2::theme(
      legend.position  = "none",
      axis.text        = ggplot2::element_text(size = 10),
      plot.title       = ggplot2::element_text(face = "bold", hjust = 0.5),
      axis.title       = ggplot2::element_text(face = "bold", size = 10),
      axis.ticks       = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(linewidth = 0.25, linetype = 1, color = "gray45"),
      panel.border     = ggplot2::element_blank()
    )
}


#' @title
#' Styled Kable Table
#'
#' @description
#' Applies a consistent visual style (Cambria font, striped rows, solid
#' header border) to a \code{data.frame} rendered via \code{kableExtra}.
#'
#' @param df \code{data.frame} --- The table to render.
#' @param caption \code{chr} --- HTML caption string.
#' @param bold_col1 \code{lgl} --- If \code{TRUE}, bolds the first column
#'   and adds a solid right border.  Defaults to \code{TRUE}.
#'
#' @return \code{kableExtra} --- A styled kable object.
#'
#' @keywords internal
styled_kable <- function(df, caption, bold_col1 = TRUE) {
  tbl <- df |>
    kableExtra::kbl(caption = caption, align = "c", booktabs = TRUE) |>
    kableExtra::kable_classic(full_width = FALSE, html_font = "Cambria") |>
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "condensed"),
      position          = "center",
      fixed_thead       = TRUE
    ) |>
    kableExtra::row_spec(row = 0, bold = TRUE, extra_css = "border-bottom: 2px solid") |>
    kableExtra::row_spec(row = seq_len(nrow(df)), extra_css = "border-bottom: 2px solid gray;") |>
    kableExtra::row_spec(row = nrow(df), extra_css = "border-bottom: 2px solid;")
  
  if (bold_col1) {
    tbl <- tbl |>
      kableExtra::column_spec(column = 1, bold = TRUE, border_right = "0.5px solid black")
  }

  if (ncol(df) > 2) {
    for (col_idx in 2:(ncol(df) - 1)) {
      tbl <- tbl |>
        kableExtra::column_spec(column = col_idx, border_right = "0.5px dashed gray")
    }
  }

  return(tbl)
}


#' @title
#' Evaluate Model on Holdout Data with Custom Threshold
#'
#' @description
#' Predicts on holdout data using a custom probability threshold, then
#' computes AUC, Balanced Accuracy, Kappa, F1, FPR, and deploy time.
#'
#' @param model \code{train} --- A fitted caret model.
#' @param holdout \code{data.frame} --- Holdout data with a \code{Tarp} column.
#' @param threshold \code{dbl} --- Probability threshold for classifying as "Yes".
#'
#' @return \code{data.frame} --- One row with columns: Threshold, AUROC,
#'   Balanced Accuracy, Kappa, F1, FPR, Deploy Time (s).
#'
#' @keywords internal
evaluate_holdout <- function(model, holdout, threshold) {
  start <- proc.time()

  set.seed(1)
  prob <- stats::predict(object = model, newdata = holdout, type = "prob")
  pred <- factor(
    x      = ifelse(test = prob$Yes > threshold, yes = "Yes", no = "No"),
    levels = c("No", "Yes")
  )
  stopifnot("Predictions length must match holdout rows" = length(pred) == nrow(holdout))

  elapsed <- (proc.time() - start)[["elapsed"]]

  rate <- ROCR::prediction(predictions = prob[, "Yes"], labels = holdout$Tarp)
  auc  <- as.numeric(ROCR::performance(prediction.obj = rate, measure = "auc")@y.values)

  cm <- caret::confusionMatrix(data = pred, reference = holdout$Tarp, positive = "Yes")

  data.frame(
    Threshold           = threshold,
    AUROC               = round(x = auc, digits = 5) * 100,
    "Balanced Accuracy" = round(x = cm$byClass["Balanced Accuracy"], digits = 4) * 100,
    Kappa               = round(x = cm$overall["Kappa"], digits = 4) * 100,
    F1                  = round(x = cm$byClass["F1"], digits = 4) * 100,
    FPR                 = round(x = 1 - cm$byClass["Specificity"], digits = 4) * 100,
    "Deploy Time (s)"   = round(x = elapsed, digits = 2),
    check.names         = FALSE,
    row.names           = NULL
  )
}


#' @title
#' Holdout vs Training ROC Comparison Plot
#'
#' @description
#' Overlays ROC curves for a model evaluated on both the training and
#' holdout datasets, with AUC values in the legend.
#'
#' @param model \code{train} --- A fitted caret model.
#' @param holdout \code{data.frame} --- Holdout data with a \code{Tarp} column.
#' @param train_data \code{data.frame} --- Training data with a \code{Tarp} column.
#' @param title \code{chr} --- Plot title.
#' @param note \code{chr} --- Optional annotation below the title. Defaults to \code{NULL}.
#' @param xlim \code{dbl} --- X-axis limits. Defaults to \code{c(0, 1)}.
#' @param ylim \code{dbl} --- Y-axis limits. Defaults to \code{c(0, 1)}.
#'
#' @return Invisible \code{NULL}; called for its side effect of drawing.
#'
#' @keywords internal
plot_roc_holdout_comparison <- function(model, holdout, train_data, title, note = NULL, xlim = c(0, 1), ylim = c(0, 1)) {
  generate_roc <- function(model, data, label) {
    set.seed(1)
    pred  <- stats::predict(object = model, newdata = data, type = "prob")
    rates <- ROCR::prediction(
      predictions    = pred[, "No"],
      labels         = data$Tarp,
      label.ordering = c("Yes", "No")
    )
    roc <- ROCR::performance(prediction.obj = rates, measure = "tpr", x.measure = "fpr")
    auc <- round(
      x      = as.numeric(ROCR::performance(rates, measure = "auc")@y.values),
      digits = 4
    ) * 100
    list(roc = roc, auc = auc, label = label)
  }

  roc_holdout <- generate_roc(model = model, data = holdout,    label = "Holdout")
  roc_train   <- generate_roc(model = model, data = train_data, label = "Training")

  graphics::plot(
    x        = roc_holdout$roc,
    col      = "#E57200",
    lwd      = 2,
    main     = title,
    cex.main = 1.2,
    xlim     = xlim,
    ylim     = ylim,
    xlab     = "False Positive Rate",
    ylab     = "True Positive Rate",
    font.lab = 2,
    bg       = "grey"
  )

  graphics::plot(x = roc_train$roc, col = "#232D4B", lwd = 2, add = TRUE)

  graphics::legend(
    "bottomright",
    legend    = c(
      paste0("Holdout (AUC = ", format(roc_holdout$auc, nsmall = 3), ")"),
      paste0("Training (AUC = ", format(roc_train$auc, nsmall = 3), ")")
    ),
    col       = c("#E57200", "#232D4B"),
    lwd       = 2,
    cex       = 0.8,
    bg        = "grey95",
    text.font = 2
  )

  if (!is.null(note)) {
    graphics::mtext(text = note, side = 3, font = 2, cex = 0.9)
  }
}


#' @title
#' SVM Kernel Comparison Plot
#'
#' @description
#' Plots AUC by cost for radial vs linear SVM kernels on the same chart.
#'
#' @param radial_model \code{train} --- A fitted caret svmRadial model.
#' @param linear_model \code{train} --- A fitted caret svmLinear model.
#' @param title \code{chr} --- Plot title.
#'
#' @return \code{ggplot} --- A ggplot2 line chart.
#'
#' @keywords internal
plot_svm_kernel_comparison <- function(radial_model, linear_model, title) {
  df <- data.frame(
    Cost   = radial_model$results$C,
    Radial = round(x = radial_model$results$ROC, digits = 4) * 100,
    Linear = round(x = linear_model$results$ROC, digits = 4) * 100
  )

  ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = Cost)) +
    ggplot2::geom_line(mapping = ggplot2::aes(y = Radial, color = "Radial"), linewidth = 0.5) +
    ggplot2::geom_line(mapping = ggplot2::aes(y = Linear, color = "Linear"), linewidth = 0.5) +
    ggplot2::geom_point(mapping = ggplot2::aes(y = Radial), color = "#E57200", size = 1.5) +
    ggplot2::geom_point(mapping = ggplot2::aes(y = Linear), color = "#232D4B", size = 1.5) +
    ggplot2::scale_color_manual(values = c("Radial" = "#232D4B", "Linear" = "#E57200")) +
    ggplot2::labs(
      title    = title,
      subtitle = "Note: x-axis is log-transformed",
      x        = "Cost",
      y        = "AUC (CV)",
      color    = "Kernel"
    ) +
    ggplot2::scale_x_continuous(breaks = df$Cost, trans = "log2") +
    ggplot2::theme_linedraw() +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", hjust = 0.5, size = 10),
      plot.subtitle = ggplot2::element_text(face = "bold", size = 7, hjust = 0.5),
      axis.title    = ggplot2::element_text(face = "bold", size = 8),
      axis.text     = ggplot2::element_text(size = 7),
      legend.text   = ggplot2::element_text(face = "bold", size = 7),
      legend.title  = ggplot2::element_blank(),
      legend.position       = "right",
      legend.key.size       = ggplot2::unit(0.3, "cm"),
      legend.box.background = ggplot2::element_rect(color = "gray", linewidth = 0.1)
    )
}


#' @title
#' 3D Support Vector Visualization
#'
#' @description
#' Creates a 3D plotly scatter plot showing training data colored by Tarp
#' status with support vectors highlighted for both radial and linear kernels.
#'
#' @param data \code{data.frame} --- Training data with Red, Green, Blue, Tarp columns.
#' @param cost \code{dbl} --- Cost parameter for SVMs.
#' @param gamma \code{dbl} --- Gamma parameter for the radial kernel.
#' @param title \code{chr} --- Plot title.
#'
#' @return \code{plotly} --- A plotly subplot with radial (left) and linear (right) panels.
#'
#' @keywords internal
plot_svm_support_vectors <- function(data, cost, gamma, title) {
  model_radial <- e1071::svm(
    formula = Tarp ~ Red + Green + Blue,
    data    = data,
    kernel  = "radial",
    cost    = cost,
    gamma   = gamma
  )
  model_linear <- e1071::svm(
    formula = Tarp ~ Red + Green + Blue,
    data    = data,
    kernel  = "linear",
    cost    = cost
  )

  sv_radial <- data[model_radial$index, ]
  sv_linear <- data[model_linear$index, ]

  sv_min_radial <- sv_radial[sv_radial$Tarp == "Yes", ]
  sv_maj_radial <- sv_radial[sv_radial$Tarp == "No", ]
  sv_min_linear <- sv_linear[sv_linear$Tarp == "Yes", ]
  sv_maj_linear <- sv_linear[sv_linear$Tarp == "No", ]

  p_radial <- plotly::plot_ly() |>
    plotly::add_markers(
      data   = data, x = ~Green, y = ~Red, z = ~Blue,
      color  = ~Tarp, colors = c("#141E3C", "#E57200"),
      marker = list(size = 1.2, opacity = 0.5),
      name   = ~paste("Tarp =", Tarp), legendgroup = ~Tarp
    ) |>
    plotly::add_markers(
      data   = sv_min_radial, x = ~Green, y = ~Red, z = ~Blue,
      color  = I("red"), marker = list(size = 1.8, opacity = 0.7),
      name   = "Radial Minority SVs", legendgroup = "Radial SVs"
    ) |>
    plotly::add_markers(
      data   = sv_maj_radial, x = ~Green, y = ~Red, z = ~Blue,
      color  = I("blue"), marker = list(size = 1.8, opacity = 0.7),
      name   = "Radial Majority SVs", legendgroup = "Radial SVs"
    )

  p_linear <- plotly::plot_ly() |>
    plotly::add_markers(
      data   = sv_min_linear, x = ~Green, y = ~Red, z = ~Blue,
      color  = I("purple"), marker = list(size = 1.8, opacity = 0.7),
      name   = "Linear Minority SVs", legendgroup = "Linear SVs"
    ) |>
    plotly::add_markers(
      data   = sv_maj_linear, x = ~Green, y = ~Red, z = ~Blue,
      color  = I("green"), marker = list(size = 1.8, opacity = 0.7),
      name   = "Linear Majority SVs", legendgroup = "Linear SVs"
    )

  plotly::subplot(p_radial, p_linear, nrows = 1, shareX = TRUE, shareY = TRUE) |>
    plotly::layout(
      title = list(text = title, xref = "paper", x = 0.5),
      annotations = list(list(
        text      = "Minority Class (Tarp=Yes) | Majority Class (Tarp=No)",
        xref      = "paper", yref = "paper",
        x         = 0.5, y = 1.0,
        showarrow = FALSE
      )),
      showlegend = TRUE,
      legend = list(
        x           = 1, y = 1,
        xanchor     = "right", yanchor = "top",
        borderwidth = 1, bordercolor = "gray",
        bgcolor     = "white",
        font        = list(color = "black")
      ),
      font = list(size = 12, family = "bold", color = "black")
    )
}
