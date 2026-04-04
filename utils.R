# =============================================================================

# utils.R — Helper functions for the Disaster Relief classification project

# 

# Source this file at the top of analysis.Rmd:

# source(here::here(“scripts”, “utils.R”))

# =============================================================================

library(tidyverse)
library(caret)
library(ROCR)
library(kableExtra)
library(gridExtra)
library(grid)
library(patchwork)
library(ggpubr)
library(tictoc)
library(parallel)
library(doParallel)

# –– Parallel Backend —————————————————

#’ Initialize a parallel backend, reserving one core.
#’ Returns the cluster object so it can be stopped later.
setup_parallel <- function() {
n_cores <- detectCores() - 1
cl <- makePSOCKcluster(n_cores)
registerDoParallel(cl)
message(“Registered parallel backend with “, n_cores, “ cores.”)
return(cl)
}

# –– Data Loading —————————————————––

#’ Load and prepare the training data.
#’
#’ @param path Path to HaitiPixels.csv (default: data/HaitiPixels.csv)
#’ @return A data.frame with columns Red, Green, Blue, Tarp (factor).
load_training_data <- function(path = here::here(“data”, “HaitiPixels.csv”)) {
df <- read.csv(path, header = TRUE)
df$Tarp <- factor(ifelse(df$Class == “Blue Tarp”, “Yes”, “No”),
levels = c(“No”, “Yes”))
df <- subset(df, select = -Class)
return(df)
}

#’ Load and prepare the holdout data from multiple text files.
#’
#’ @param dir Directory containing the holdout .txt files
#’            (default: data/holdout)
#’ @return A data.frame with columns Red, Green, Blue, Tarp (factor).
load_holdout_data <- function(dir = here::here(“data”, “holdout”)) {
files <- list.files(dir, pattern = “*.txt”, full.names = TRUE)

holdout <- do.call(rbind, lapply(files, function(file) {
lines <- readLines(file)[-c(1:8)]
csv <- read.csv(text = lines, header = FALSE, sep = “”)[-1]
colnames(csv) <- c(“X”, “Y”, “MapX”, “MapY”, “Lat”, “Lon”,
“B1”, “B2”, “B3”)
csv$Tarp <- ifelse(grepl(“NON|NOT”, file), “No”, “Yes”)
csv
}))

holdout$Tarp <- factor(holdout$Tarp, levels = c(“No”, “Yes”))
holdout <- holdout %>%
rename(Red = B1, Green = B2, Blue = B3) %>%
select(Red, Green, Blue, Tarp)

return(holdout)
}

# –– Cross-Validation Setup ––––––––––––––––––––––

#’ Create a standard trainControl object for 10-fold CV.
#’
#’ @param data Training data (used to create stratified folds on Tarp)
#’ @param seed Random seed (default: 1)
#’ @return A list with components `ctrl` (trainControl) and `folds`.
make_cv_control <- function(data, seed = 1) {
set.seed(seed)
folds <- createFolds(data$Tarp, k = 10, list = TRUE, returnTrain = TRUE)

ctrl <- trainControl(
method      = “cv”,
number      = 10,
index       = folds,
classProbs  = TRUE,
allowParallel = TRUE,
savePredictions = TRUE,
summaryFunction = twoClassSummary
)

return(list(ctrl = ctrl, folds = folds))
}

# –– Threshold Analysis ———————————————––

#’ Compute performance metrics across a range of probability thresholds.
#’
#’ @param model A caret train object with savePredictions = TRUE.
#’ @param thresholds Numeric vector of thresholds to evaluate
#’                   (default: seq(0.1, 0.9, by = 0.1)).
#’ @return A data.frame with one row per threshold.
compute_thresholds <- function(model,
thresholds = seq(0.1, 0.9, by = 0.1)) {
stats <- c(“Balanced Accuracy”, “Kappa”, “Sensitivity”,
“Specificity”, “Precision”, “F1”)

set.seed(1)
values <- thresholder(model, threshold = thresholds, statistics = stats)
values$FPR <- round(1 - values$Specificity, 4)

# Drop model-specific hyperparameter columns if present

drop_cols <- intersect(
names(values),
c(“parameter”, “k”, “alpha”, “lambda”, “mtry”, “C”, “sigma”)
)
if (length(drop_cols) > 0) {
values <- values[, !(names(values) %in% drop_cols), drop = FALSE]
}

values <- values %>% rename(Threshold = prob_threshold)
return(values)
}

# –– Plotting Helpers —————————————————

#’ Line chart for a single metric across thresholds.
#’
#’ @param data  Output of compute_thresholds().
#’ @param y_var Column name (string) for the y-axis metric.
#’ @param title Plot title.
#’ @return A ggplot object.
plot_metric_vs_threshold <- function(data, y_var, title) {
ggplot(data, aes(x = Threshold, y = .data[[y_var]])) +
geom_line(color = “#232D4B”, linewidth = 1) +
geom_point(color = “#E57200”, size = 2) +
labs(title = title, x = “Threshold”, y = NULL) +
scale_x_continuous(breaks = seq(0.1, 0.9, by = 0.2)) +
theme_linedraw() +
theme(
axis.text    = element_text(face = “bold”),
plot.title   = element_text(face = “bold”, hjust = 0.5),
axis.title.x = element_text(face = “bold”)
)
}

#’ 2×2 grid of threshold metric plots for a given model.
#’
#’ @param thresh_data Output of compute_thresholds().
#’ @param model_name  String used in the figure title (e.g., “Logistic”).
#’ @param subtitle    Optional subtitle string (e.g., hyperparameter info).
plot_threshold_grid <- function(thresh_data, model_name, subtitle = NULL) {
p1 <- plot_metric_vs_threshold(thresh_data, “FPR”, “FPR”)
p2 <- plot_metric_vs_threshold(thresh_data, “Balanced Accuracy”,
“Balanced Accuracy”)
p3 <- plot_metric_vs_threshold(thresh_data, “Kappa”, “Kappa”)
p4 <- plot_metric_vs_threshold(thresh_data, “F1”, “F1”)

plots <- list(p1, p2, p3, p4) %>%
purrr::map(~ .x + labs(x = NULL, y = NULL))

grid.arrange(
p1, p2, p3, p4,
ncol = 2,
top = textGrob(
paste0(“Summary Statistics by Threshold (”, model_name, “)”),
gp = gpar(fontsize = 14, font = 2, hjust = 0.5)
),
bottom = textGrob(
“Threshold”,
gp = gpar(fontsize = 13, font = 2, hjust = 0.5)
),
grobs = plots
)

if (!is.null(subtitle)) {
grid.text(subtitle, x = 0.5, y = 0.90,
gp = gpar(fontsize = 10, font = 2))
}
}

#’ Plot per-fold ROC curves with an overall out-of-fold curve.
#’
#’ @param model      A caret train object.
#’ @param data       The training data.
#’ @param title      Plot title string.
#’ @param note       Optional annotation below the title.
#’ @param xlim, ylim Axis limits (default: c(0,1) for both).
plot_roc_curves <- function(model, data, title = “ROC Curves”,
note = NULL,
xlim = c(0, 1), ylim = c(0, 1)) {

resamps <- model$control$index
names(resamps) <- paste(“Fold”, seq_along(resamps))

roc_data   <- list()
auc_values <- numeric(length(resamps))

for (i in seq_along(resamps)) {
fold_data  <- data[resamps[[i]], ]
pred_probs <- predict(model, newdata = fold_data, type = “prob”)[, “Yes”]
roc_data[[i]] <- ROCR::prediction(pred_probs, fold_data$Tarp)
auc_values[i] <- round(
as.numeric(ROCR::performance(roc_data[[i]], “auc”)@y.values), 5
) * 100
}

rates   <- ROCR::prediction(model$pred$No, model$pred$obs,
label.ordering = c(“Yes”, “No”))
avg_roc <- ROCR::performance(rates, “tpr”, x.measure = “fpr”)
avg_auc <- round(
as.numeric(ROCR::performance(rates, “auc”)@y.values), 5
) * 100

plot(avg_roc, col = “#232D4B”, lwd = 2.5, main = title,
xlim = xlim, ylim = ylim,
xlab = “False Positive Rate”, ylab = “True Positive Rate”,
font.lab = 2, bg = “grey”)

legend_text <- paste0(“OOF (AUC = “, format(avg_auc, nsmall = 3), “)”)

fold_counter <- 0
for (i in seq_along(resamps)) {
if (!is.null(roc_data[[i]])) {
fold_perf <- ROCR::performance(roc_data[[i]], “tpr”, x.measure = “fpr”)
legend_text <- c(
legend_text,
paste0(names(resamps)[i], “ (AUC = “,
format(round(auc_values[i], 5), nsmall = 3), “)”)
)
if (i != 1) {
fold_counter <- fold_counter + 1
lines(fold_perf@x.values[[1]], fold_perf@y.values[[1]],
col = rainbow(length(resamps) - 1)[fold_counter], lwd = 0.8)
}
}
}

lines(x = xlim, y = ylim, col = “#E57200”)

legend(“bottomright”, legend = legend_text,
col = c(”#232D4B”, rainbow(length(resamps))[-1]),
lwd = 1.5, cex = 0.95, bg = “grey95”, text.font = 2)

if (!is.null(note)) {
mtext(note, side = 3, font = 2, cex = 0.9)
}
}

#’ Render a confusion matrix as a ggplot heatmap tile.
#’
#’ @param conf_matrix Output of confusionMatrix().
#’ @param title       Plot title.
#’ @return A ggplot object.
plot_confusion_matrix <- function(conf_matrix, title) {
conf_tab <- data.frame(conf_matrix$table)
conf_tab$Freq <- round(conf_tab$Freq, 1)

ggplot(conf_tab, aes(y = Prediction, x = Reference, fill = Freq)) +
scale_x_discrete(position = “top”) +
geom_tile() +
scale_fill_gradient(low = “white”, high = “#ea9c4e”) +
labs(title = title) +
geom_text(aes(label = Freq, fontface = “bold”), color = “black”, size = 4) +
geom_tile(color = “black”, fill = “black”, alpha = 0) +
theme_linedraw() +
theme(
legend.position = “none”,
axis.text       = element_text(size = 10),
plot.title      = element_text(face = “bold”, hjust = 0.5),
axis.title      = element_text(face = “bold”, size = 10),
axis.ticks      = element_blank(),
panel.grid.major = element_line(linewidth = 0.25, linetype = 1,
color = “gray45”),
panel.border = element_blank()
)
}

# –– Table Formatting —————————————————

#’ Apply a consistent kable style to a data.frame.
#’
#’ @param df      A data.frame.
#’ @param caption HTML caption string.
#’ @param bold_col1 If TRUE, bold the first column and add a solid right border.
#’ @return A styled kable object.
styled_kable <- function(df, caption, bold_col1 = TRUE) {
tbl <- df %>%
kbl(caption = caption, align = “c”, booktabs = TRUE) %>%
kable_classic(full_width = FALSE, html_font = “Cambria”) %>%
kable_styling(
bootstrap_options = c(“striped”, “condensed”),
position = “center”,
fixed_thead = TRUE
) %>%
row_spec(0, bold = TRUE, extra_css = “border-bottom: 2px solid”) %>%
row_spec(
seq_len(nrow(df)),
extra_css = “border-bottom: 2px solid gray;”
) %>%
row_spec(nrow(df), extra_css = “border-bottom: 2px solid;”)

if (bold_col1) {
tbl <- tbl %>%
column_spec(1, bold = TRUE, border_right = “0.5px solid black”)
}

# Add dashed borders between remaining columns

for (col_idx in 2:(ncol(df) - 1)) {
tbl <- tbl %>%
column_spec(col_idx, border_right = “0.5px dashed gray”)
}

return(tbl)
}

# –– Model Training Wrapper ––––––––––––––––––––––

#’ Train a model with timing, using a consistent seed and control object.
#’
#’ @param formula  Model formula (e.g., Tarp ~ Red + Blue + Green).
#’ @param data     Training data.
#’ @param method   caret method string (e.g., “glm”, “lda”, “svmRadial”).
#’ @param ctrl     trainControl object from make_cv_control().
#’ @param …      Additional arguments passed to caret::train().
#’ @return A list with components `model` (caret train object) and
#’         `elapsed` (training time in seconds).
train_timed <- function(formula, data, method, ctrl, …) {
tic()
set.seed(1)
model <- train(
formula,
data      = data,
method    = method,
metric    = “ROC”,
trControl = ctrl,
…
)
elapsed <- toc(quiet = TRUE)
time_sec <- elapsed$toc - elapsed$tic

return(list(model = model, elapsed = time_sec))
}

# –– Density Plot Helper ————————————————

#’ Create a density plot of a color channel split by Tarp status.
#’
#’ @param data    Data.frame with the color column and a Tarp column.
#’ @param x_var   String name of the column to plot.
#’ @param title   Plot title.
#’ @return A ggplot object.
plot_color_density <- function(data, x_var, title) {
ggdensity(
data, x = x_var,
add = “mean”, rug = TRUE,
color = “Tarp”, fill = “Tarp”,
alpha = 0.1,
palette = c(”#141E3C”, “#eb5f0c”),
title = title,
ggtheme = theme_linedraw()
)
}
