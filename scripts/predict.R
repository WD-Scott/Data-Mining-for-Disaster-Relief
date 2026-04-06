# predict.R — Classify satellite pixels as blue tarp (displaced persons) or not
#
# Loads the saved Penalized Logistic Regression model and predicts on new
# pixel-level RGB data. Run `make report` first to generate models/plr_model.rds.
#
# Usage:
#   Rscript scripts/predict.R                        # uses sample data
#   Rscript scripts/predict.R path/to/pixels.csv     # uses your own CSV

source(here::here("scripts", "utils.R"))


# --- Load saved model --------------------------------------------------------

model_path <- here::here("models", "plr_model.rds")
if (!file.exists(model_path)) {
  stop(
    "Model not found at ", model_path, ".\n",
    "Run `make report` to train and save the PLR model first.",
    call. = FALSE
  )
}
model <- readRDS(model_path)


# --- Load or generate pixel data ---------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

if (length(args) >= 1) {
  pixels <- data.table::fread(input = args[1])
  stopifnot(
    "Input CSV must contain columns: Red, Green, Blue" =
      all(c("Red", "Green", "Blue") %in% names(pixels))
  )
  pixels <- as.data.frame(pixels)
} else {
  message("No input file provided -- using sample pixel data for demonstration.\n")
  pixels <- data.frame(
    Red   = c(151, 44, 120, 60, 100),
    Green = c(137, 58, 112, 92, 95),
    Blue  = c(117, 98, 96, 145, 88)
  )
}


# --- Predict ------------------------------------------------------------------

threshold <- 0.6

probs <- stats::predict(object = model, newdata = pixels, type = "prob")
pixels$Prob_Tarp <- round(probs$Yes, 4)
pixels$Predicted <- ifelse(probs$Yes > threshold, "Tarp", "Not Tarp")

cat("Threshold:", threshold, "\n\n")
print(pixels)
cat(
  "\nSummary:", sum(pixels$Predicted == "Tarp"), "of", nrow(pixels),
  "pixels classified as tarp.\n"
)
