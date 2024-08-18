library(tidyverse)
library(ROCR)
library(leaps)
library(class)
library(ggplot2)
library(gridExtra)
library(MASS)
library(caTools)
library(boot)
library(jtools)
library(splines)
library(gam)
library(ggpubr)
library(grid)
library(gridGraphics)
library(patchwork)
library(caret)
library(scatterplot3d)
library(huxtable)
library(pROC)
library(knitr)
library(plotly)
library(glmnet)
library(kableExtra)
library(plotrix)
library(cowplot)
library(reshape2)
library(parallel)
library(doParallel)
library(randomForest)
library(tictoc)
library(e1071)
library(kernlab)
library(likert)

no_cores<-detectCores() - 1
cl<-makePSOCKcluster(no_cores)
registerDoParallel(cl)

data<-read.csv("~/Documents/DS6030/Project/HaitiPixels.csv", header=TRUE)

data$Tarp<-ifelse(data$Class == 'Blue Tarp', 'Yes', 'No')
data$Tarp<-as.factor(data$Tarp)
data<-subset(data, select=-c(Class))

setwd("/Users/wyattscott/Documents/DS6030/Project/Hold Out Data")

files<-list.files(pattern="*.txt", full.names=TRUE)

# Cleaning Holdout
holdout<-do.call(rbind, lapply(files, function(file) {
  lines<-readLines(file)[-c(1:8)]
  csv<-read.csv(text=lines, header=FALSE, sep="")[-1]
  colnames(csv)<-c("X", "Y", "MapX", 
                   "MapY", "Lat", "Lon", 
                   "B1", "B2", "B3")
  csv$Tarp<-ifelse(grepl("NON|NOT", file), "No", "Yes")
  csv
})) 

holdout$Tarp<-factor(holdout$Tarp, levels=c("No", "Yes"))

# Quick distribution check
Blue<-ggdensity(data, x="Blue", add="mean", rug=TRUE, color="Tarp", fill="Tarp",
                alpha=0.1, palette=c("#141E3C", "#eb5f0c"), 
                title="Blue", ggtheme=theme_linedraw())

Green<-ggdensity(data, x="Green", add="mean", rug=TRUE, color="Tarp", fill="Tarp",
                 alpha=0.1, palette=c("#141E3C", "#eb5f0c"), 
                 title="Green", ggtheme=theme_linedraw())

Red<-ggdensity(data, x="Red", add="mean", rug=TRUE, color="Tarp", fill="Tarp",
               alpha=0.1, palette=c("#141E3C", "#eb5f0c"), 
               title="Red", ggtheme=theme_linedraw())

B1<-ggdensity(holdout, x="B1", add="mean", rug=TRUE, color="Tarp", fill="Tarp",
              alpha=0.1, palette=c("#141E3C", "#eb5f0c"), 
              title="B1", ggtheme=theme_linedraw())

B2<-ggdensity(holdout, x="B2", add="mean", rug=TRUE, color="Tarp", fill="Tarp",
              alpha=0.1, palette=c("#141E3C", "#eb5f0c"), 
              title="B2", ggtheme=theme_linedraw())

B3<-ggdensity(holdout, x="B3", add="mean", rug=TRUE, color="Tarp", fill="Tarp",
              alpha=0.1, palette=c("#141E3C", "#eb5f0c"), 
              title="B3", ggtheme=theme_linedraw())

combined.holdout<-B1+Red+B2+Green+B3+Blue
combined.holdout + plot_layout(guides="collect", ncol=2) + 
  plot_annotation(title='Figure 1.1: Holdout vs. Train Data - Color Distributions by Tarp Status') & 
  theme(plot.title=element_text(face='bold', size=12, hjust=0.5),
        axis.ticks=element_blank(), 
        axis.title=element_blank(), 
        axis.text.y=element_blank(),
        legend.position=("bottom"),
        legend.text=element_text(face=2), legend.title=element_text(face=2),
        legend.background=element_rect(fill="white", color="grey35"))


holdout<-holdout %>% 
  rename(Red=B1, 
         Green=B2, 
         Blue=B3)

holdout<-subset(holdout, select=-c(X, Y, MapX, MapY, Lat, Lon))

# EDA
eda1<-data.frame("Tarp Status" = c("No", "Yes"),
                 "Training" = c("61,219", "2,022"),
                 "Holdout" = c("1,989,697", "14,480"),
                 "Training" = c(96.8, 3.2),
                 "Holdout"= c(99.27, 0.01))
eda1<-eda1 %>% 
  rename("Tarp Status" = "Tarp.Status",
         "Training " = "Training.1",
         "Holdout " = "Holdout.1")
eda1 %>%
  knitr::kable(caption="<b>Table 1.1: Class Imbalance<b>", align="c", bold=T) %>%
  kable_classic(full_width=F, html_font = "Cambria") %>% 
  kable_styling(bootstrap_options=c("striped", "condensed"), position="center", fixed_thead=T) %>%
  row_spec(0, bold=TRUE, extra_css="border-bottom: 3px solid") %>% 
  add_header_above(c("", "Count"=2, "Percent"=2), bold=TRUE) %>% 
  row_spec(seq(1, nrow(eda1), 1), extra_css="border-bottom: 2px solid gray;") %>%
  row_spec(2, extra_css="border-bottom: 2px solid black;") %>% 
  column_spec(1, bold=T, border_right="0.5px solid black") %>% 
  column_spec(2, border_right="0.5px dashed gray") %>% 
  column_spec(3, border_right="0.5px dashed gray") %>% 
  column_spec(4, border_right="0.5px dashed gray")

holdout_subset<-holdout %>%
  sample_n(size=60000, replace=FALSE)
data_subset<-data %>% 
  sample_n(size=60000, replace=FALSE)
data_subset$Source<-"Training"
holdout_subset$Source<-"Holdout"
combined_data<-rbind(data_subset, holdout_subset)

combined_plot<-plot_ly(combined_data, x = ~Green, y = ~Red, z = ~Blue,
                       color = ~interaction(Source, Tarp), colors = colorRamp(c("black", "skyblue", "purple", "#E57200")),
                       type = "scatter3d", mode = "markers",
                       marker = list(size=1.6, opacity = 0.6, symbol = "circle-open"),
                       showlegend = TRUE) %>%
  layout(scene = list(aspectmode = "data", 
                      xaxis = list(title = "Green"),
                      yaxis = list(title = "Red"),
                      zaxis = list(title = "Blue")),
         title = "Figure 1.2: RGB Values by Dataset and Tarp Status",
         font = list(size = 12, family = "bold", color = "black"),
         legend = list(x = 0.75, y = 1, itemsizing = "constant", tracegroupgap = 10, traceorder = "normal", 
                       borderwidth = 0.5, bordercolor = "gray", 
                       bgcolor = "white", font = list(color = "black"), 
                       title = list(text = "Dataset & Tarp Status", font = list(size=12, family="bold"))))

combined_plot

# Methods
set.seed(1)
folds<-createFolds(data$Tarp, k=10, list=TRUE, returnTrain=TRUE)
ctrl<-trainControl(method="cv", number=10,index=folds, classProbs=TRUE, 
                   allowParallel=TRUE, savePredictions=TRUE,
                   summaryFunction=twoClassSummary)


stats<-c("Balanced Accuracy", "Kappa", "Sensitivity", "Specificity", "Precision", "F1")
thresholds=seq(0.1, 0.9, by=0.1)
thresh<-function(model) {
  set.seed(1)
  values<-thresholder(model, threshold=thresholds, statistics=stats)
  values$FPR<-1-values$Specificity
  return(values)
}

## Plotting functions
create_line_chart <- function(data, x_var, y_var, plot_title) {
  plot <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_line(color = "#232D4B", linewidth = 1) +
    geom_point(color = "#E57200", size = 2) +
    labs(title = plot_title, x = "Threshold", y = y_var) +
    scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
    theme_linedraw()+
    theme(axis.text = element_text(face = "bold"),
          plot.title = element_text(face = "bold", hjust = 0.5),
          axis.title.x = element_text(face = "bold"),
          axis.title.y = element_blank())
  return(plot)
}

plot_rocs<-function(model, data, plot_title = "ROC curves", note = NULL, 
                    xlim = c(0, 1), ylim = c(0, 1)) {
  require(ROCR)
  resamps <- model$control$index
  names(resamps) <- paste("Fold", seq_along(resamps))
  roc_data <- list()
  auc_values <- numeric(length(resamps))
  
  for (i in seq_along(resamps)) {
    fold_data <- data[resamps[[i]], ]
    pred_probs <- predict(model, newdata = fold_data, type = "prob")[, "Yes"]
    class_labels <- fold_data$Tarp
    roc_data[[i]] <- ROCR::prediction(pred_probs, class_labels)
    auc_values[i] <- round(as.numeric(ROCR::performance(roc_data[[i]], measure = "auc")@y.values), 5)*100
  }
  
  rates <- ROCR::prediction(model$pred$No, model$pred$obs, label.ordering = c("Yes", "No"))
  avg_roc <- ROCR::performance(rates, measure = "tpr", x.measure = "fpr")
  avg_auc <- ROCR::performance(rates, measure = "auc")@y.values
  avg_auc<-round(as.numeric(avg_auc), 5)*100
  
  plot(avg_roc, col = "#232D4B", lwd = 2.5, main = plot_title, 
       xlim = xlim, ylim = ylim, xlab = "False Positive Rate", 
       ylab = "True Positive Rate", font.lab = 2, bg='grey')
  
  legend_text <- paste("OOF (AUC = ", format(avg_auc, nsmall = 3), ")", sep = "")
  
  fold_counter <- 0
  for (i in 1:length(resamps)) {
    if (!is.null(roc_data[[i]])) {
      fold_plot <- ROCR::performance(roc_data[[i]], measure = "tpr", x.measure = "fpr")
      fpr <- fold_plot@x.values[[1]]
      tpr <- fold_plot@y.values[[1]]
      legend_text <- c(legend_text, 
                       paste(names(resamps)[i], " (AUC = ", 
                             format(round(auc_values[i], 5), nsmall = 3), ")", sep= ""))
      if (i != 1) {
        fold_counter <- fold_counter + 1
        lines(fpr, tpr, col = rainbow(length(resamps) - 1)[fold_counter], lwd=0.8)
      }
    }
  }
  
  lines(x = xlim, y = ylim, col = '#E57200')
  legend("bottomright", legend = legend_text, 
         col = c("#232D4B", rainbow(length(resamps))[-1]), 
         lwd = 1.5, cex = 0.95, bg = 'grey95', text.font = 2)
  
  if (!is.null(note)) {mtext(note, side=3, font=2, cex=0.9)}
}

plot_confusion_matrix <- function(conf_matrix, title) {
  conf_tab <- data.frame(conf_matrix$table)
  conf_tab$Freq <- round(conf_tab$Freq, 1)
  plt <- ggplot(data = conf_tab, aes(y = Prediction, x = Reference, fill = Freq)) +
    scale_x_discrete(position = "top") +
    geom_tile(data = conf_tab, aes(fill = Freq)) +
    scale_fill_gradient(low = "white", high = "#ea9c4e") +
    labs(title = title) +
    geom_text(aes(label = Freq, fontface="bold"), color = 'black', size = 4) +
    geom_tile(color = "black", fill = "black", alpha = 0) +
    theme_linedraw() +
    theme(legend.position = "none",
          axis.text = element_text(size = 10),
          plot.title = element_text(face = 2, hjust = 0.5),
          axis.title = element_text(face = 2, size = 10),
          axis.ticks = element_blank(),
          panel.grid.major = element_line(linewidth = 0.25, linetype = 1, color = "gray45"),
          panel.border = element_blank())
  return(plt)
}

# Logistic
tic()
set.seed(1)
log<-train(Tarp~Red+Blue+Green, 
           data=data, 
           method="glm", 
           trControl=ctrl, 
           family="binomial",
           metric="ROC")
run_time<-toc()
log_time<-run_time$toc - run_time$tic

log_results <- data.frame(
  ROC = round(log$results$ROC, 4)*100,
  Sensitivity = round(log$results$Sens, 4)*100,
  Specificity = round(log$results$Spec, 4)*100)

log_results %>%
  kbl(caption = "<b>Table 3.1: Logistic Results<b>", 
      align="ccccc", booktabs=T) %>%
  kable_classic(full_width=F,html_font="Cambria") %>% 
  kable_styling(bootstrap_options=c("striped", "condensed"), position="center", fixed_thead=T) %>%
  row_spec(0, bold=TRUE, extra_css="border-bottom: 2px solid") %>%
  row_spec(seq(1, nrow(log_results), 1), extra_css="border-bottom: 2px solid gray;") %>%
  row_spec(1, extra_css="border-bottom: 3px solid;") %>% 
  column_spec(1, border_right="0.5px dashed gray") %>%
  column_spec(2, border_right="0.5px dashed gray")

log.thresh<-thresh(log)
log.thresh<-subset(log.thresh, select = -c(parameter))
log.thresh<-log.thresh %>% 
  rename("Threshold" = "prob_threshold")
log.thresh$FPR<-round(log.thresh$FPR, 4)
line<-create_line_chart(log.thresh, "Threshold", "FPR", "FPR")
line2<-create_line_chart(log.thresh, "Threshold", "`Balanced Accuracy`", "Balanced Accuracy")
line3<-create_line_chart(log.thresh, "Threshold", "Kappa", "Kappa")
line4<-create_line_chart(log.thresh, "Threshold", "F1", "F1")
p<-list(line, line2, line3, line4) %>%
  purrr::map(~ .x + labs(x=NULL, y=NULL))
grid.arrange(line, line2, line3, line4, ncol=2,
             top=textGrob("Figure 3.1: Summary Statistics by Threshold (Logistic)", 
                          gp = gpar(fontsize=14, font=2, hjust=0.5)),
             bottom=textGrob("Thresholds", gp=gpar(fontsize=13, font=2, hjust=0.5)),
             grobs=p)

plot_rocs(log, data, plot_title="Figure 3.2: ROC Curves (Logistic)", xlim=c(0, 0.08), ylim=c(0.75, 1))

log_finmd <- data.frame(Betas = c("β₀ (Intercept)", "β₁ (Red)", "β₂ (Blue)", "β₃ (Green)"),
                        Coefficients = c(0.2098, -0.2603, 0.4724, -0.2183))

footnotes <- c("Degrees of Freedom: Total = 63,240 | Residual = 63,237.",
               "Deviance: Null = 17,900 | Residual = 1,770.",
               "AIC = 1,778.")

log_finmd %>%
  kbl(caption = "<b>Table 3.2: Logistic finalModel<b>", 
      align="lc", booktabs=T) %>%
  kable_classic(full_width=F,html_font="Cambria") %>% 
  kable_styling(bootstrap_options=c("striped", "condensed"), position="center", fixed_thead=T) %>%
  row_spec(0, bold=TRUE, extra_css="border-bottom: 2px solid") %>%
  row_spec(seq(1, nrow(log_finmd), 1), extra_css="border-bottom: 2px solid gray;") %>%
  row_spec(4, extra_css="border-bottom: 2px solid;") %>% 
  column_spec(1, bold=T, border_right="0.5px solid black") %>% 
  add_footnote(footnotes, notation="none")

log.final<-log.thresh %>% 
  slice(9)

log.final$AUROC<-log$results$ROC

vif_df <- data.frame(
  "Predictor" = c("Red", "Green", "Blue"),
  "VIFs" = c(224.006, 259.507, 371.083))

cor_df<- data.frame(
  "Predictor" = c("Red", "Green", "Blue"),
  "Red" = c(1.00, 0.98, 0.94),
  "Green" = c(0.98, 1.00, 0.97),
  "Blue" = c(0.94, 0.97, 1.00))

table1<-kbl(vif_df, halign = 't',
            align = "lc", booktabs = T) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  kable_styling(bootstrap_options = c("striped", "condensed"), position = "center", fixed_thead = T) %>%
  row_spec(0, bold = TRUE, extra_css = "border-bottom: 2px solid") %>%
  row_spec(seq(1, nrow(vif_df), 1), extra_css = "border-bottom: 2px solid gray;") %>%
  row_spec(2, extra_css = "border-bottom: 2px solid;") %>% 
  column_spec(1, bold=T, border_right = "0.5px solid black")

table2 <- kbl(cor_df, halign = 't',  
              align = "ccccc", booktabs = T) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  kable_styling(bootstrap_options = c("striped", "condensed"), position = "center", fixed_thead = T) %>%
  row_spec(0, bold = TRUE, extra_css = "border-bottom: 2px solid") %>%
  row_spec(seq(1, nrow(cor_df), 1), extra_css = "border-bottom: 2px solid gray;") %>%
  row_spec(2, extra_css = "border-bottom: 2px solid;") %>% 
  column_spec(1, bold=T, border_right = "0.5px solid black") %>%
  column_spec(2, border_right = "0.5px dashed gray") %>%
  column_spec(3, border_right = "0.5px dashed gray")

tables_div <- sprintf('<div style="text-align: center;"><p><b>Table 3.3: Correlation Matrix and VIFs</b></p>%s%s</div>', table1, table2)
htmltools::HTML(tables_div)

# LDA
tic()
set.seed(1)
lda<-train(Tarp~log(Red)+log(Blue)+log(Green), 
           data=data, 
           method="lda",
           preProcess=c("center","scale"),
           metric="ROC",
           trControl=ctrl)
run_time<-toc()
lda_time<-run_time$toc - run_time$tic

lda_results <- data.frame(
  ROC = round(lda$results$ROC, 4)*100,
  Sensitivity = round(lda$results$Sens, 4)*100,
  Specificity = round(lda$results$Spec, 4)*100)

lda_results %>%
  kbl(caption = "<b>Table 4.1: LDA Results<b>", 
      align="ccccc", booktabs=T) %>%
  kable_classic(full_width=F,html_font="Cambria") %>% 
  kable_styling(bootstrap_options=c("striped", "condensed"), position="center", fixed_thead=T) %>%
  row_spec(0, bold=TRUE, extra_css="border-bottom: 2px solid") %>%
  row_spec(seq(1, nrow(lda_results), 1), extra_css="border-bottom: 2px solid gray;") %>%
  row_spec(1, extra_css="border-bottom: 2px solid;") %>% 
  column_spec(1, border_right="0.5px dashed gray") %>%
  column_spec(2, border_right="0.5px dashed gray")

lda.thresh<-thresh(lda)
lda.thresh<-subset(lda.thresh, select = -c(parameter))
lda.thresh<-lda.thresh %>% 
  rename("Threshold" = "prob_threshold")
lda.thresh$FPR<-round(lda.thresh$FPR, 4)
line<-create_line_chart(lda.thresh, "Threshold", "FPR", "FPR")
line2<-create_line_chart(lda.thresh, "Threshold", "`Balanced Accuracy`", "Balanced Accuracy")
line3<-create_line_chart(lda.thresh, "Threshold", "Kappa", "Kappa")
line4<-create_line_chart(lda.thresh, "Threshold", "F1", "F1")
p<-list(line, line2, line3, line4) %>%
  purrr::map(~ .x + labs(x=NULL, y=NULL))
grid.arrange(line, line2, line3, line4, ncol=2,
             top=textGrob("Figure 4.1: Summary Statistics by Threshold (LDA)", 
                          gp=gpar(fontsize=14,font=2,hjust=0.5)),
             bottom=textGrob("Thresholds", gp=gpar(fontsize=13,font=2,hjust=0.5)), grobs=p)

plot_rocs(lda, data, plot_title="Figure 4.2: ROC Curves (LDA)", xlim=c(0, 0.15), ylim=c(0.85, 1))

lda_finmd <- data.frame(
  "Tarp Status" = c("No", "Yes"),
  "Prior Probs" = c(96.80, 3.19),
  "log(Red)" = c(-0.009, 0.288),
  "log(Blue)" = c(-0.0363, 1.099),
  "log(Green)" = c(-0.018, 0.556))

lda_finmd<-lda_finmd %>% 
  rename("Tarp Status" = "Tarp.Status",
         "Prior Probs" = "Prior.Probs",
         "log(Red)" = "log.Red.",
         "log(Blue)" = "log.Blue.",
         "log(Green)" = "log.Green.")

lda_ldcoefs<- data.frame(
  "Predictor" = c("log(Red)", "log(Blue)", "log(Green)"),
  LD1 = c(-4.527, 5.566, -0.777))

table1 <- kbl(lda_ldcoefs, valign = 't',
              align = "lc", booktabs = T) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  kable_styling(bootstrap_options = c("striped", "condensed"), position = "center", fixed_thead = T) %>%
  row_spec(0, bold = TRUE, extra_css = "border-bottom: 2px solid") %>%
  row_spec(seq(1, nrow(lda_finmd), 1), extra_css = "border-bottom: 2px solid gray;") %>%
  row_spec(3, extra_css = "border-bottom: 2px solid;") %>% 
  column_spec(1, bold=T, border_right = "0.5px solid black")

table2 <- kbl(lda_finmd, valign = 't',  
              align = "ccccc", booktabs = T) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  add_header_above(c(" ", " ", "Group Means" = 3), bold=TRUE) %>% 
  kable_styling(bootstrap_options = c("striped", "condensed"), position = "center", fixed_thead = T) %>%
  row_spec(0, bold = TRUE, extra_css = "border-bottom: 2px solid") %>%
  row_spec(seq(1, nrow(lda_finmd), 1), extra_css = "border-bottom: 2px solid gray;") %>%
  row_spec(2, extra_css = "border-bottom: 2px solid;") %>% 
  column_spec(1, bold=T, border_right = "0.5px solid black") %>%
  column_spec(2, border_right = "0.5px dashed gray") %>%
  column_spec(3, border_right = "0.5px dashed gray") %>% 
  column_spec(4, border_right = "0.5px dashed gray")

tables_div <- sprintf('<div style="text-align: center;"><p><b>Table 4.2: LDA finalModel</b></p>%s%s</div>', table1, table2)
htmltools::HTML(tables_div)

lda.final<-lda.thresh %>% 
  slice(9)

lda.final$AUROC<-lda$results$ROC

# QDA
tic()
set.seed(1)
qda<-train(Tarp~Red+Blue+Green, 
           data=data, 
           method="qda",
           metric="ROC",
           trControl=ctrl)
run_time<-toc()
qda_time<-run_time$toc - run_time$tic

qda_results <- data.frame(
  ROC = round(qda$results$ROC, 4)*100,
  Sensitivity = round(qda$results$Sens, 4)*100,
  Specificity = round(qda$results$Spec, 4)*100)

qda_results %>%
  kbl(caption = "<b>Table 5.1: QDA Results<b>", 
      align="ccccc", booktabs=T) %>%
  kable_classic(full_width=F,html_font="Cambria") %>% 
  kable_styling(bootstrap_options=c("striped", "condensed"), position="center", fixed_thead=T) %>%
  row_spec(0, bold=TRUE, extra_css="border-bottom: 2px solid") %>%
  row_spec(seq(1, nrow(qda_results), 1), extra_css="border-bottom: 2px solid gray;") %>%
  row_spec(1, extra_css="border-bottom: 2px solid;") %>% 
  column_spec(1, border_right="0.5px dashed gray") %>%
  column_spec(2, border_right="0.5px dashed gray")

qda.thresh<-thresh(qda)
qda.thresh<-subset(qda.thresh, select = -c(parameter))
qda.thresh<-qda.thresh %>% 
  rename("Threshold"="prob_threshold")
qda.thresh$FPR<-round(qda.thresh$FPR, 4)
line<-create_line_chart(qda.thresh, "Threshold", "FPR", "FPR")
line2<-create_line_chart(qda.thresh, "Threshold", "`Balanced Accuracy`", "Balanced Accuracy")
line3<-create_line_chart(qda.thresh, "Threshold", "Kappa", "Kappa")
line4<-create_line_chart(qda.thresh, "Threshold", "F1", "F1")
p = list(line, line2, line3, line4) %>% map(~.x + labs(x=NULL, y=NULL))
grid.arrange(line, line2, line3, ncol=2,
             top=textGrob("Figure 5.1: Summary Statistics by Threshold (QDA)", 
                          gp=gpar(fontsize=14,font=2,hjust=0.5)),
             bottom=textGrob("Thresholds", gp=gpar(fontsize=13,font=2,hjust=0.5)), grobs=p)

plot_rocs(qda, data, plot_title="Figure 5.2: ROC Curves (QDA)", xlim=c(0, 0.15), ylim=c(0.85, 1))

qda_finmd <- data.frame(
  "Tarp Status" = c("No", "Yes"),
  "Prior Probs" = c(96.80, 3.19),
  Red = c(162.76, 169.66),
  Blue = c(122.49, 205.04),
  Green = c(152.58, 186.42))

qda_finmd<-qda_finmd %>% 
  rename("Tarp Status" = "Tarp.Status",
         "Prior Probs" = "Prior.Probs")

qda_finmd %>%
  kbl(caption = "<b>Table 5.2: QDA finalModel<b>", 
      align="ccccc", booktabs=T) %>%
  kable_classic(full_width=F,html_font="Cambria") %>% 
  add_header_above(c(" ", " ", "Group Means"=3), bold=TRUE) %>% 
  kable_styling(bootstrap_options=c("striped", "condensed"), position="center", fixed_thead=T) %>%
  row_spec(0, bold=TRUE, extra_css="border-bottom: 2px solid") %>%
  row_spec(seq(1, nrow(qda_finmd), 1), extra_css="border-bottom: 2px solid gray;") %>%
  row_spec(2, extra_css="border-bottom: 2px solid;") %>% 
  column_spec(1, bold=T, border_right="0.5px solid black") %>%
  column_spec(2, border_right="0.5px dashed gray") %>%
  column_spec(3, border_right="0.5px dashed gray") %>% 
  column_spec(4, border_right="0.5px dashed gray")

qda.final<-qda.thresh %>% 
  slice(8)

qda.final$AUROC<-qda$results$ROC

# KNN
tic()
set.seed(1)
knn<-train(Tarp~Red+Blue+Green, 
           data=data, 
           method="knn",
           tuneLength=10,
           metric="ROC",
           trControl=ctrl)
run_time<-toc()
knn_time<-run_time$toc - run_time$tic

knn_df1<-data.frame(K=c(5, 7, 9, 11, 13, 15, 17, 19, 21, 23),
                    AUC=c(knn$results$ROC))

knn_df1$AUC<-round(knn_df1$AUC, 5)*100

ggplot(knn_df1, aes(x = K, y=AUC)) +
  geom_line(color = "#232D4B", linewidth = 0.5) +
  geom_point(color = "#E57200", size = 1.5) +
  labs(title = "Figure 6.1: AUC by K Values", x = "K", y = "AUC (CV)")+
  scale_x_continuous(breaks = c(5, 7, 9, 11, 13, 15, 17, 19, 21, 23)) +
  theme_linedraw() +
  theme(
    plot.title = element_text(face = 2, hjust = 0.5, size=8),
    axis.title = element_text(face = 2, size=7),
    axis.title.x = element_text(face = 2),
    axis.text=element_text(size=6))

knn.thresh<-thresh(knn)
knn.thresh<-subset(knn.thresh, select = -c(k))
knn.thresh<-knn.thresh %>% 
  rename("Threshold"="prob_threshold")
knn.thresh$FPR<-round(knn.thresh$FPR, 4)
line<-create_line_chart(knn.thresh, "Threshold", "FPR", "FPR")
line2<-create_line_chart(knn.thresh, "Threshold", "`Balanced Accuracy`", "Balanced Accuracy")
line3<-create_line_chart(knn.thresh, "Threshold", "Kappa", "Kappa")
line4<-create_line_chart(knn.thresh, "Threshold", "F1", "F1")
p = list(line, line2, line3, line4) %>% map(~.x + labs(x=NULL, y=NULL))
grid.arrange(line, line2, line3, line4, ncol=2,
             top=textGrob("Figure 6.2: Summary Statistics by Threshold (KNN)\n",
                          gp=gpar(fontsize=14,font=2,hjust=0.5)),
             bottom=textGrob("Thresholds", gp=gpar(fontsize=13,font=2,hjust=0.5)), grobs=p)
grid.text("k = 23", x = 0.5, y = 0.90, gp = gpar(fontsize = 10, font = 2))

plot_rocs(knn, data, plot_title="Figure 6.3: ROC Curves (KNN)", 
          note="k = 23", xlim=c(0, 0.007), ylim=c(0, 1))

knn_finmd <- data.frame(`Tarp Status` = c("No", "Yes"),
                        Outcome = c("61,219", "2,022"))

footnotes <- c("k = 23.")

knn_finmd<-knn_finmd %>% 
  rename("Tarp Status" = "Tarp.Status")

knn_finmd %>%
  kbl(caption = "<b>Table 6.2: KNN finalModel<b>", 
      align="lc", booktabs=T) %>%
  kable_classic(full_width=F,html_font="Cambria") %>% 
  kable_styling(bootstrap_options=c("striped", "condensed"), position="center", fixed_thead=T) %>%
  row_spec(0, bold=TRUE, extra_css="border-bottom: 2px solid") %>%
  row_spec(seq(1, nrow(knn_finmd), 1), extra_css="border-bottom: 2px solid gray;") %>%
  row_spec(2, extra_css="border-bottom: 2px solid;") %>% 
  column_spec(1, bold=T, border_right="0.5px solid black") %>%
  add_footnote(footnotes, notation="none")

knn.final<-knn.thresh %>% 
  slice(6)

knn.final$AUROC<-knn$results$ROC[10]

# Penalized Logistic Regression (Elastic Net)
tic()
set.seed(1)
plr<-train(Tarp~Red+Blue+Green, 
           data=data, 
           method="glmnet",
           tuneLength=20,
           allowParallel=TRUE,
           metric="ROC",
           trControl=ctrl)
run_time<-toc()
plr_time<-run_time$toc - run_time$tic

plrBst<-data.frame("Hyperparameter"=c("α", "λ"),
                   "Values"=c("0.1474", "2e−05"))

plrBst %>%
  knitr::kable(caption="<b>Table 7.1: PLR bestTune<b>", align=c("c","c"), bold=T) %>%
  kable_classic(full_width=F, html_font = "Cambria") %>% 
  kable_styling(bootstrap_options=c("striped", "condensed"), fixed_thead=T) %>%
  row_spec(0, bold=TRUE, extra_css="border-bottom: 3px solid") %>% 
  row_spec(seq(1, nrow(plrBst), 1), extra_css="border-bottom: 2px solid gray;") %>%
  row_spec(2, extra_css="border-bottom: 2px solid;") %>% 
  column_spec(1, bold=T, border_right="0.5px solid black")

plr.thresh<-thresh(plr)
plr.thresh<-plr.thresh %>% 
  rename("Threshold"="prob_threshold")
plr.thresh$FPR<-round(plr.thresh$FPR, 4)
line<-create_line_chart(plr.thresh, "Threshold", "FPR", "FPR")
line2<-create_line_chart(plr.thresh, "Threshold", "`Balanced Accuracy`", "Balanced Accuracy")
line3<-create_line_chart(plr.thresh, "Threshold", "Kappa", "Kappa")
line4<-create_line_chart(plr.thresh, "Threshold", "F1", "F1")
p = list(line, line2, line3, line4) %>% map(~.x + labs(x=NULL, y=NULL))
grid.arrange(line, line2, line3, line4, ncol=2,
             top=textGrob("Figure 7.1: Summary Statistics by Threshold (PLR)\n", 
                          gp=gpar(fontsize=14,font=2,hjust=0.5)),
             bottom=textGrob("Thresholds", gp=gpar(fontsize=13,font=2,hjust=0.5)), grobs=p)
grid.text("α = 0.1474 | λ = 2e-05", x = 0.5, y = 0.90, gp = gpar(fontsize = 10, font = 2))

plot_rocs(plr, data, plot_title="Figure 7.2: ROC Curves (PLR)", note="α = 0.1474 | λ = 2e-05", 
          xlim=c(0, 0.4), ylim=c(0.75, 1))

plr.final<-plr.thresh %>% 
  slice(6)

plr.final$AUROC<-0.98303

plr.final$Sensitivity<-round(plr.final$Sensitivity, 4)*100
plr.final$`Balanced Accuracy`<-round(plr.final$`Balanced Accuracy`, 4)*100
plr.final$Specificity<-round(plr.final$Specificity, 4)*100
plr.final$Kappa<-round(plr.final$Kappa, 4)*100
plr.final$Precision<-round(plr.final$Precision, 4)*100
plr.final$FPR<-round(plr.final$FPR, 4)*100
plr.final$F1<-round(plr.final$F1, 4)*100
plr.final$AUROC<-round(plr.final$AUROC, 5)*100
plr.final$alpha<-round(plr.final$alpha, 4)

plr.final<-plr.final %>% 
  rename("α"="alpha",
         "λ"="lambda")

plr.final %>%
  kbl(caption = "<b>Table 7.2: PLR Model Statistics<b>", align="c") %>%
  kable_classic(full_width=T,html_font="Cambria") %>% 
  kable_styling(bootstrap_options=c("striped", "condensed"), position="center", fixed_thead=T) %>%
  row_spec(0, bold=TRUE, extra_css="border-bottom: 2px solid") %>%
  column_spec(1, border_right="0.5px dashed gray") %>%
  column_spec(2, border_right="0.5px dashed gray") %>% 
  column_spec(3, border_right="0.5px dashed gray") %>% 
  column_spec(4, border_right="0.5px dashed gray") %>% 
  column_spec(5, border_right="0.5px dashed gray") %>% 
  column_spec(6, border_right="0.5px dashed gray") %>%
  column_spec(7, border_right="0.5px dashed gray") %>% 
  column_spec(8, border_right="0.5px dashed gray") %>%
  column_spec(9, border_right="0.5px dashed gray") %>% 
  column_spec(10, border_right="0.5px dashed gray")

# Random Forest
tic()
set.seed(1)
mtryGrid<-expand.grid(mtry=c(1, 2))
rf<-train(Tarp~Red+Green+Blue, 
          data=data,
          tuneGrid=mtryGrid,
          ntree=500,
          method="rf",
          metric="ROC",
          trControl=ctrl)
run_time<-toc()
rf_time<-run_time$toc - run_time$tic

rf_results <- data.frame(
  mtry = c("✭1", "2"),
  ROC = round(rf$results$ROC, 4)*100,
  Sensitivity = round(rf$results$Sens, 4)*100,
  Specificity = round(rf$results$Spec, 4)*100)

footnotes <- c(
  "ROC was used to select optimal model.",
  "Optimal hyperparameter value is mtry = 1.")

rf_results %>%
  kbl(caption = "<b>Table 8.1: RF Results<b>", 
      align="ccccc", booktabs=T) %>%
  kable_classic(full_width=F,html_font="Cambria") %>% 
  kable_styling(bootstrap_options=c("striped", "condensed"), position="center", fixed_thead=T) %>%
  row_spec(0, bold=TRUE, extra_css="border-bottom: 2px solid") %>%
  row_spec(seq(1, nrow(rf_results), 1), extra_css="border-bottom: 2px solid gray;") %>%
  row_spec(2, extra_css="border-bottom: 2px solid;") %>% 
  column_spec(1, bold=T, border_right="0.5px solid black") %>%
  column_spec(2, border_right="0.5px dashed gray") %>%
  column_spec(3, border_right="0.5px dashed gray") %>%
  add_footnote(footnotes, notation="none")

rf.thresh<-thresh(rf)
rf.thresh<-rf.thresh %>% 
  rename("Threshold"="prob_threshold")
rf.thresh$FPR<-round(rf.thresh$FPR, 4)
line<-create_line_chart(rf.thresh, "Threshold", "FPR", "FPR")
line2<-create_line_chart(rf.thresh, "Threshold", "`Balanced Accuracy`", "Balanced Accuracy")
line3<-create_line_chart(rf.thresh, "Threshold", "Kappa", "Kappa")
line4<-create_line_chart(rf.thresh, "Threshold", "F1", "F1")
p = list(line, line2, line3, line4) %>% map(~.x + labs(x=NULL, y=NULL))
grid.arrange(line, line2, line3, line4, ncol=2,
             top=textGrob("Figure 8.1: Summary Statistics by Threshold (RF)\n", 
                          gp=gpar(fontsize=14,font=2,hjust=0.5)),
             bottom=textGrob("Thresholds", gp=gpar(fontsize=13,font=2,hjust=0.5)), grobs=p)
grid.text("ntree = 500 | mtry = 1", x = 0.5, y = 0.90, gp = gpar(fontsize = 10, font = 2))

plot_rocs(rf, data, plot_title="Figure 8.2: ROC Curves (RF)", 
          note="ntree = 500 | mtry = 1", xlim=c(0, 0.01), ylim=c(0, 1))

rf_finmd <- data.frame("Predicted" = c("No", "Yes"),
                       "No" = c("61,149", "118"),
                       "Yes" =c("70", "1,904"),
                       "Class Error" = c("0.001143", "0.058358"))

footnotes <- c("ntree = 500 | mtry = 1.",
               "OOB estimate of error rate: 0.3%.")

rf_finmd<-rf_finmd %>% 
  rename("Class Error"="Class.Error")

rf_finmd %>%
  kbl(caption = "<b>Table 8.2: RF finalModel<b>", align="lccc") %>%
  kable_classic(full_width=F,html_font="Cambria") %>% 
  kable_styling(bootstrap_options=c("striped", "condensed"), position="center", fixed_thead=T) %>%
  row_spec(0, bold=TRUE, extra_css="border-bottom: 2px solid") %>%
  add_header_above(c("", "Reference"=2, ""), bold=TRUE, extra_css="border=right: 0.5px dashed gray") %>% 
  row_spec(seq(0, nrow(rf_finmd), 1), extra_css="border-bottom: 2px solid gray;") %>%
  row_spec(2, extra_css="border-bottom: 2px solid;") %>% 
  column_spec(1, bold=T, border_right="0.5px solid black") %>%
  column_spec(2, border_right="0.5px dashed gray") %>%
  column_spec(3, border_right="0.5px dashed gray") %>%
  add_footnote(footnotes, notation="none")

rf.final<-rf.thresh %>% 
  slice(5)

rf.final$AUROC<-0.99968

rf.final$Sensitivity<-round(rf.final$Sensitivity, 4)
rf.final$`Balanced Accuracy`<-round(rf.final$`Balanced Accuracy`, 4)
rf.final$Specificity<-round(rf.final$Specificity, 4)
rf.final$Kappa<-round(rf.final$Kappa, 4)
rf.final$Precision<-round(rf.final$Precision, 4)
rf.final$FPR<-round(rf.final$FPR, 4)
rf.final$F1<-round(rf.final$F1, 4)

# Support Vector Machine
set.seed(1)
svmR<-train(Tarp~Red+Green+Blue, 
            data=data,
            method="svmRadial",
            tuneLength=10,
            trControl=ctrl)

lingrid<-expand.grid(C=c(0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128))
set.seed(1)
svmL<-train(Tarp~Red+Green+Blue, 
            data=data,
            method="svmLinear",
            tuneGrid=lingrid,
            trControl=ctrl)

svmtest_df<-data.frame("Radial"=c(round(svmR$results$ROC, 4)*100),
                       "Linear"=c(round(svmL$results$ROC, 4)*100),
                       "Cost" = c(svmR$results$C))

ggplot(svmtest_df, aes(x = Cost)) +
  geom_line(aes(y = Radial, color = "Radial", linetype = "Radial"), linewidth = 0.5) +
  geom_line(aes(y = Linear, color = "Linear", linetype = "Linear"), linewidth = 0.5) +
  geom_point(aes(y = Radial), color = "#E57200", size = 1.5) +
  geom_point(aes(y = Linear), color = "#232D4B", size = 1.5) +
  scale_color_manual(values = c("Radial" = "#232D4B", "Linear" = "#E57200")) +
  scale_linetype_manual(values = c("Radial" = "solid", "Linear" = "solid")) +
  labs(title = "Figure 9.1: AUC — Linear vs. Radial Kernel", x = "Cost", 
       y = "AUC (CV)", subtitle="Note: x-axis is log-transformed",
       color = "Kernel Type", linetype = "Kernel Type") +
  scale_x_continuous(breaks = c(0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128), trans = "log2") +
  theme_linedraw() +
  theme(
    plot.title = element_text(face = 2, hjust = 0.5, size=8),
    axis.title = element_text(face = 2, size=7),
    plot.subtitle = element_text(face=2, size=6, hjust=0.5),
    axis.title.x = element_text(face = 2),
    axis.text=element_text(size=6),
    legend.text = element_text(face = 2, size=5),
    legend.title = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    legend.key.size = unit(0.3, "cm"),
    legend.box.background = element_rect(color = "gray", size = 0.1, linetype = "solid")) +
  guides(color = guide_legend(override.aes = list(shape = NULL)))

model_radial<-svm(Tarp~Red + Green + Blue, data=data, kernel="radial", C=128, gamma=8.2977)
model_linear<-svm(Tarp~Red + Green + Blue, data=data, kernel= "linear", C=128)
predicted_values_radial<-predict(model_radial, newdata=data)
predicted_values_linear<-predict(model_linear, newdata=data)
support_vectors_radial<-data[model_radial$index, ]
support_vectors_linear<-data[model_linear$index, ]
minority_indices <- which(data$Tarp == "Yes")
majority_indices <- which(data$Tarp == "No")
support_vectors_minority_radial <- support_vectors_radial[support_vectors_radial$Tarp == "Yes", ]
support_vectors_majority_radial <- support_vectors_radial[support_vectors_radial$Tarp == "No", ]
support_vectors_minority_linear <- support_vectors_linear[support_vectors_linear$Tarp == "Yes", ]
support_vectors_majority_linear <- support_vectors_linear[support_vectors_linear$Tarp == "No", ]

scatterplot_radial <- plot_ly() %>%
  add_markers(data = data, x = ~Green, y = ~Red, z = ~Blue, color = ~Tarp, colors = c('#141E3C', '#e57200'),
              marker = list(size = 1.2, opacity = 0.5), name = ~paste("Tarp =", Tarp), legendgroup = ~Tarp) %>%
  add_markers(data = support_vectors_minority_radial, x = ~Green, y = ~Red, z = ~Blue,
              color = I("red"), marker = list(size = 1.8, opacity=0.7),
              name = "Radial Minority SVs", legendgroup = "Radial SVs") %>%
  add_markers(data = support_vectors_majority_radial, x = ~Green, y = ~Red, z = ~Blue,
              color = I("blue"), marker = list(size = 1.8, opacity=0.7),
              name = "Radial Majority SVs", legendgroup = "Radial SVs") %>%
  add_surface(x = matrix(seq(min(data$Green), max(data$Green), length.out = 20), nrow = 20),
              y = matrix(seq(min(data$Red), max(data$Red), length.out = 20), nrow = 20),
              z = matrix(seq(min(data$Blue), max(data$Blue), length.out = 20), nrow = 20),
              surfacecolor = matrix(as.numeric(predicted_values_radial), nrow = 20),
              colorscale = "Viridis", showscale = FALSE)

scatterplot_linear <- plot_ly() %>%
  add_markers(data = support_vectors_minority_linear, x = ~Green, y = ~Red, z = ~Blue,
              color = I("purple"), marker = list(size = 1.8, opacity=0.7),
              name = "Linear Minority SVs", legendgroup = "Linear SVs") %>%
  add_markers(data = support_vectors_majority_linear, x = ~Green, y = ~Red, z = ~Blue,
              color = I("green"), marker = list(size = 1.8, opacity=0.7),
              name = "Linear Majority SVs", legendgroup = "Linear SVs") %>%
  add_surface(x = matrix(seq(min(data$Green), max(data$Green), length.out = 20), nrow = 20),
              y = matrix(seq(min(data$Red), max(data$Red), length.out = 20), nrow = 20),
              z = matrix(seq(min(data$Blue), max(data$Blue), length.out = 20), nrow = 20),
              surfacecolor = matrix(as.numeric(predicted_values_linear), nrow = 20),
              colorscale = "Viridis", showscale = FALSE)

subplot <- subplot(scatterplot_radial, scatterplot_linear, nrows = 1, shareX = TRUE, 
                   shareY = TRUE, titleX = TRUE, titleY = TRUE)

subplot <- subplot %>%
  layout(title=list(text="Figure 9.2: Linear and Radial Support Vectors", xref="paper", x=0.5),
         annotations = list(list(text="Minority Class (Tarp=Yes) | Majority Class (Tarp=No)", 
                                 xref="paper", yref = "paper", x = 0.5, y = 1.0, showarrow = FALSE)),
         xaxis = list(title = "Green"), yaxis = list(title = "Red"), zaxis = list(title = "Blue"), 
         showlegend = TRUE, legend = list(x = 1, y = 1, xanchor = "right", yanchor = "top",
                                          title = "Legend", itemsizing = "constant", tracegroupgap = 10, 
                                          traceorder = "normal", borderwidth=1, bordercolor="gray", 
                                          bgcolor="white", font = list(color = "black")),
         font = list(size = 12, family = "bold", color = "black"))

subplot

tic()
set.seed(1)
svmgrid1<-expand.grid(C=0.25, sigma=seq(1, 3, 0.25))
set.seed(1)
svmradial<-train(Tarp~Red+Green+Blue, 
                 data=data,
                 method="svmRadial",
                 tuneGrid=svmgrid1,
                 trControl=ctrl)
run_time<-toc()
svm_time<-run_time$toc - run_time$tic

lingrid<-expand.grid(C=0.25)
set.seed(1)
svmL2<-train(Tarp~Red+Green+Blue, 
             data=data,
             method="svmLinear",
             tuneGrid=lingrid,
             trControl=ctrl)

svmR2_results <- data.frame(
  γ = c("ROC", "Sensitivity", "Specificty"),
  "1.00" = c(99.960,	99.819,	92.637),
  "✭1.25" = c(99.962,	99.824,	92.582),
  "1.50" = c(99.961,	99.838,	92.333),
  "1.75" = c(99.960,	99.846,	92.432),
  "2.00" = c(99.958,	99.851,	92.383),
  "2.25" = c(99.954,	99.856,	92.482),
  "2.50" = c(99.949,	99.864,	92.797),
  "2.75" = c(99.944,	99.860,	92.630),
  "3.00" = c(99.940,	99.863,	92.580))

footnotes <- c(
  "cost held constant at 0.25.",
  "ROC was used to select optimal model."
)

svmR2_results<-svmR2_results %>% 
  rename("1.00" = "X1.00",
         "✭1.25"="X.1.25",
         "1.50" = "X1.50",
         "1.75" = "X1.75",
         "2.00" = "X2.00",
         "2.25" = "X2.25",
         "2.50" = "X2.50",
         "2.75" = "X2.75",
         "3.00" = "X3.00")

svmR2_results %>%
  kbl(caption = "<b>Table 9.1: SVM Radial Results<b>", 
      align="ccccc", booktabs=T) %>%
  kable_classic(full_width=T,html_font="Cambria") %>% 
  kable_styling(bootstrap_options=c("striped", "condensed"), position="center", fixed_thead=T) %>%
  row_spec(0, bold=TRUE, extra_css="border-bottom: 2px solid") %>%
  row_spec(seq(1, nrow(svmR2_results), 1), extra_css="border-bottom: 2px solid gray;") %>%
  row_spec(3, extra_css="border-bottom: 2px solid;") %>% 
  column_spec(1, bold=TRUE, border_right="2px solid;") %>%
  column_spec(2, border_right="0.5px dashed gray") %>%
  column_spec(3, border_right="0.5px dashed gray") %>%
  column_spec(4, border_right="0.5px dashed gray") %>%
  column_spec(5, border_right="0.5px dashed gray") %>%
  column_spec(6, border_right="0.5px dashed gray") %>%
  column_spec(7, border_right="0.5px dashed gray") %>%
  column_spec(8, border_right="0.5px dashed gray") %>%
  column_spec(9, border_right="0.5px dashed gray") %>%
  add_footnote(footnotes, notation="none")

model_radial<-svm(Tarp~Red + Green + Blue, data=data, kernel="radial", C=0.25, gamma=1.25)
model_linear<-svm(Tarp~Red + Green + Blue, data=data, kernel= "linear", C=0.25)
predicted_values_radial<-predict(model_radial, newdata=data)
predicted_values_linear<-predict(model_linear, newdata=data)
support_vectors_radial<-data[model_radial$index, ]
support_vectors_linear<-data[model_linear$index, ]
minority_indices <- which(data$Tarp == "Yes")
majority_indices <- which(data$Tarp == "No")
support_vectors_minority_radial <- support_vectors_radial[support_vectors_radial$Tarp == "Yes", ]
support_vectors_majority_radial <- support_vectors_radial[support_vectors_radial$Tarp == "No", ]
support_vectors_minority_linear <- support_vectors_linear[support_vectors_linear$Tarp == "Yes", ]
support_vectors_majority_linear <- support_vectors_linear[support_vectors_linear$Tarp == "No", ]

scatterplot_radial <- plot_ly() %>%
  add_markers(data = data, x = ~Green, y = ~Red, z = ~Blue, color = ~Tarp, colors = c('#141E3C', '#e57200'),
              marker = list(size = 1.2, opacity = 0.5), name = ~paste("Tarp =", Tarp), legendgroup = ~Tarp) %>%
  add_markers(data = support_vectors_minority_radial, x = ~Green, y = ~Red, z = ~Blue,
              color = I("red"), marker = list(size = 1.8, opacity=0.7),
              name = "Radial Minority SVs", legendgroup = "Radial SVs") %>%
  add_markers(data = support_vectors_majority_radial, x = ~Green, y = ~Red, z = ~Blue,
              color = I("blue"), marker = list(size = 1.8, opacity=0.7),
              name = "Radial Majority SVs", legendgroup = "Radial SVs") %>%
  add_surface(x = matrix(seq(min(data$Green), max(data$Green), length.out = 20), nrow = 20),
              y = matrix(seq(min(data$Red), max(data$Red), length.out = 20), nrow = 20),
              z = matrix(seq(min(data$Blue), max(data$Blue), length.out = 20), nrow = 20),
              surfacecolor = matrix(as.numeric(predicted_values_radial), nrow = 20),
              colorscale = "Viridis", showscale = FALSE)

scatterplot_linear <- plot_ly() %>%
  add_markers(data = support_vectors_minority_linear, x = ~Green, y = ~Red, z = ~Blue,
              color = I("purple"), marker = list(size = 1.8, opacity=0.7),
              name = "Linear Minority SVs", legendgroup = "Linear SVs") %>%
  add_markers(data = support_vectors_majority_linear, x = ~Green, y = ~Red, z = ~Blue,
              color = I("green"), marker = list(size = 1.8, opacity=0.7),
              name = "Linear Majority SVs", legendgroup = "Linear SVs") %>%
  add_surface(x = matrix(seq(min(data$Green), max(data$Green), length.out = 20), nrow = 20),
              y = matrix(seq(min(data$Red), max(data$Red), length.out = 20), nrow = 20),
              z = matrix(seq(min(data$Blue), max(data$Blue), length.out = 20), nrow = 20),
              surfacecolor = matrix(as.numeric(predicted_values_linear), nrow = 20),
              colorscale = "Viridis", showscale = FALSE)

subplot <- subplot(scatterplot_radial, scatterplot_linear, nrows = 1, shareX = TRUE, 
                   shareY = TRUE, titleX = TRUE, titleY = TRUE)

subplot <- subplot %>%
  layout(title=list(text="Figure 9.3: Linear and Radial Support Vectors", xref="paper", x=0.5),
         annotations = list(list(text="Minority Class (Tarp=Yes) | Majority Class (Tarp=No)", 
                                 xref="paper", yref = "paper", x = 0.5, y = 1.0, showarrow = FALSE)),
         xaxis = list(title = "Green"), yaxis = list(title = "Red"), zaxis = list(title = "Blue"), 
         showlegend = TRUE, legend = list(x = 1, y = 1, xanchor = "right", yanchor = "top",
                                          title = "Legend", itemsizing = "constant", tracegroupgap = 10, 
                                          traceorder = "normal", borderwidth=1, bordercolor="gray", 
                                          bgcolor="white", font = list(color = "black")),
         font = list(size = 12, family = "bold", color = "black"))

subplot

svmR_results <- data.frame(
  Statistics = c("ROC", "Sensitivity", "Specificity"),
  Radial = c(99.962, 99.824, 92.582),
  Linear = c(99.743, 99.897, 88.477))

footnotes <- c(
  "cost = 0.25.",
  "γ = 1.25.")

svmR_results %>%
  kbl(caption = "<b>Table 9.2: SVM Results, Radial vs. Linear Kernel<b>", 
      align="lcc", booktabs=T) %>%
  kable_classic(full_width=F,html_font="Cambria") %>% 
  kable_styling(bootstrap_options=c("striped", "condensed"), position="center", fixed_thead=T) %>%
  row_spec(0, bold=TRUE, extra_css="border-bottom: 2px solid") %>%
  row_spec(seq(1, nrow(svmR_results), 1), extra_css="border-bottom: 2px solid gray;") %>%
  row_spec(3, extra_css="border-bottom: 2px solid;") %>% 
  column_spec(1, bold=T, border_right="0.5px solid black") %>%
  column_spec(2, border_right="0.5px dashed gray") %>% 
  add_footnote(footnotes, notation="none")

svm.thresh<-thresh(svmradial)
svm.thresh<-svm.thresh %>% 
  rename("Threshold"="prob_threshold")
svm.thresh$FPR<-round(svm.thresh$FPR, 4)
line<-create_line_chart(svm.thresh, "Threshold", "FPR", "FPR")
line2<-create_line_chart(svm.thresh, "Threshold", "`Balanced Accuracy`", "Balanced Accuracy")
line3<-create_line_chart(svm.thresh, "Threshold", "Kappa", "Kappa")
line4<-create_line_chart(svm.thresh, "Threshold", "F1", "F1")
p = list(line, line2, line3, line4) %>% map(~.x + labs(x=NULL, y=NULL))
grid.arrange(line, line2, line3, line4, ncol=2,
             top=textGrob("Figure 9.4: Summary Statistics by Threshold (SVM)\n", 
                          gp=gpar(fontsize=14,font=2,hjust=0.5)),
             bottom=textGrob("Thresholds", gp=gpar(fontsize=13,font=2,hjust=0.5)), grobs=p)
grid.text("cost = 0.25 | γ = 1.25", x = 0.5, y = 0.90, gp = gpar(fontsize = 10, font = 2))

svmgrid1<-expand.grid(C=0.25, sigma=1.25)
set.seed(1)
svmradial<-train(Tarp~Red+Green+Blue, 
                 data=data,
                 method="svmRadial",
                 tuneGrid=svmgrid1,
                 trControl=ctrl)

plot_rocs(svmradial, data, plot_title="Figure 9.5: ROC Curves (SVM, Radial)", 
          note="cost = 0.25 | γ = 1.25", xlim=c(0, 0.02), ylim=c(0.6, 1))

svm.final<-svm.thresh %>% 
  slice(9)

svm.final$AUROC<-99.961

svm.final$Sensitivity<-round(svm.final$Sensitivity, 4)*100
svm.final$`Balanced Accuracy`<-round(svm.final$`Balanced Accuracy`, 4)*100
svm.final$Specificity<-round(svm.final$Specificity, 4)*100
svm.final$Kappa<-round(svm.final$Kappa, 4)*100
svm.final$Precision<-round(svm.final$Precision, 4)*100
svm.final$FPR<-round(svm.final$FPR, 4)*100
svm.final$F1<-round(svm.final$F1, 4)*100
svm.final$sigma<-round(svm.final$sigma, 4)

svm.final<-svm.final %>% 
  rename("γ" = "sigma",
         "cost" = "C")

svm.final %>%
  kbl(caption = "<b>Table 9.3: SVM Model Statistics<b>", align="c") %>%
  kable_classic(full_width=T,html_font="Cambria") %>% 
  kable_styling(bootstrap_options=c("striped", "condensed"), position="center", fixed_thead=T) %>%
  row_spec(0, bold=TRUE, extra_css="border-bottom: 2px solid") %>%
  column_spec(1, border_right="0.5px dashed gray") %>%
  column_spec(2, border_right="0.5px dashed gray") %>% 
  column_spec(3, border_right="0.5px dashed gray") %>% 
  column_spec(4, border_right="0.5px dashed gray") %>% 
  column_spec(5, border_right="0.5px dashed gray") %>% 
  column_spec(6, border_right="0.5px dashed gray") %>%
  column_spec(7, border_right="0.5px dashed gray") %>% 
  column_spec(8, border_right="0.5px dashed gray") %>%
  column_spec(9, border_right="0.5px dashed gray") %>% 
  column_spec(10, border_right="0.5px dashed gray")

# Model Comparison
plr.final$AUROC<-plr.final$AUROC/100
plr.final$`Balanced Accuracy`<-plr.final$`Balanced Accuracy`/100
plr.final$Kappa<-plr.final$Kappa/100
plr.final$F1<-plr.final$F1/100
plr.final$FPR<-plr.final$FPR/100

svm.final$AUROC<-svm.final$AUROC/100
svm.final$`Balanced Accuracy`<-svm.final$`Balanced Accuracy`/100
svm.final$Kappa<-svm.final$Kappa/100
svm.final$F1<-svm.final$F1/100
svm.final$FPR<-svm.final$FPR/100

train_comp<-(plr.final)
train_comp<-as.data.frame(train_comp)
train_comp<-subset(train_comp, select=-c(α, λ))
svm.final2<-subset(svm.final, select=-c(γ, cost))
rf.final2<-subset(rf.final, select=-c(mtry))

train_comp <- rbind(log.final, lda.final, qda.final, 
                    knn.final, train_comp, rf.final2, svm.final2)

train_comp <- train_comp %>% 
  add_column(Model = c("Logistic", "LDA", "QDA", "$KNN^{*}$", "$PLR^{†}$", "$RF^{‡}$", "$SVM^{§}$"))

train_comp <- train_comp %>% 
  relocate(Model)

time_df <- data.frame(
  Model = c("Logistic", "LDA", "QDA", "$KNN^{*}$", "$PLR^{†}$", "$RF^{‡}$", "$SVM^{§}$"),
  `Train Time` = c(log_time, lda_time, qda_time, knn_time, plr_time, rf_time, svm_time))

train_comp$Kappa <- round(train_comp$Kappa, 4)*100
train_comp$`Balanced Accuracy`<-round(train_comp$`Balanced Accuracy`,4)*100
train_comp$F1 <- round(train_comp$F1, 4)*100
train_comp$FPR <- round(train_comp$FPR, 4)*100
train_comp$AUROC <- round(train_comp$AUROC, 5)*100
train_comp$`Train Time (secs)`<-time_df$`Train.Time`
train_comp$`Train Time (secs)` <- round(train_comp$`Train Time`, 2)
train_comp<-subset(train_comp, select=-c(Precision, Sensitivity, Specificity))

summary_range<-data.frame(
  Variable=names(train_comp)[-1],
  Range=sapply(train_comp[-1], function(x) diff(range(x))))

summary_range<-summary_range %>% 
  rename("Statistic"="Variable")

rownames(summary_range) <- NULL

Ranges<-data.frame(Range=c("", "", "1.67", "5.63", "13.96", "0.53", "11.53", "55.62"))

new_row <- data.frame(Model = NA, Threshold = NA, AUROC= NA, `Balanced Accuracy` = NA, Kappa = NA,
                      F1 = NA, FPR = NA, `Train Time (secs)` = NA)

new_row[c("Model", "Threshold")] <- c("Range", NA)
new_row$Balanced.Accuracy <- Ranges$Range[4]
new_row$Kappa <- Ranges$Range[5]
new_row$F1 <- Ranges$Range[6]
new_row$FPR <- Ranges$Range[7]
new_row$Train.Time..secs.<-Ranges$Range[8]
new_row$AUROC<-Ranges$Range[3]

new_row<-new_row %>% 
  rename("Balanced Accuracy" = "Balanced.Accuracy",
         "Train Time (secs)" = "Train.Time..secs.")

train_comp <- rbind(train_comp, new_row)
train_comp <- train_comp %>% relocate("AUROC", .after = "Threshold")

table<-train_comp %>%
  kbl(caption="<b>Table 10.1: Model Summary Statistics (Cross-Validation)<b>", align="lcccccccc", booktabs=T) %>%
  kable_classic(full_width=T,html_font="Cambria") %>% 
  kable_styling(bootstrap_options=c("striped", "condensed"), position="center", fixed_thead=T) %>%
  row_spec(0, bold=TRUE, extra_css="border-bottom: 3px solid") %>%
  row_spec(seq(1, nrow(train_comp), 1), extra_css="border-top: 2px solid gray;") %>%
  row_spec(8, extra_css="border-top: 3px dashed gray;") %>% 
  row_spec(8, extra_css="border-bottom: 2px solid;") %>% 
  column_spec(1, border_right="0.5px solid black") %>%
  column_spec(3, border_left="0.5px dashed gray") %>%
  column_spec(4, border_left="0.5px dashed gray") %>%
  column_spec(5, border_left="0.5px dashed gray") %>%
  column_spec(6, border_left="0.5px dashed gray") %>% 
  column_spec(7, border_left="0.5px dashed gray") %>% 
  column_spec(8, border_left="0.5px dashed gray")

table

testmodels<-function(model, finalmodel) {
  start_time<-Sys.time()
  set.seed(1)
  prob<-predict(model, newdata=holdout, type="prob")
  pred<-as.factor(ifelse(prob$Yes > finalmodel$Threshold, "Yes", "No"))
  rate<-prediction(prob[, 2], holdout$Tarp)
  auc<-performance(rate, "auc")
  conf<-confusionMatrix(pred, holdout$Tarp)
  cols<-colnames(finalmodel)
  end_time<-Sys.time()
  elapsed_time<-end_time-start_time
  stats<-data.frame(AUROC=auc@y.values[[1]],
                    Threshold=finalmodel$Threshold,
                    `Balanced Accuracy`=0.5*(conf$byClass[["Specificity"]]+conf$byClass[["Sensitivity"]]),
                    Kappa=conf$overall[["Kappa"]],
                    FPR=1-conf$byClass[["Specificity"]],
                    F1=conf$byClass[["F1"]],
                    'Run Time'=elapsed_time)
  
  return(stats)
}

set.seed(1)
log.pred<-testmodels(log, log.final)
lda.pred<-testmodels(lda, lda.final)
qda.pred<-testmodels(qda, qda.final)
knn.pred<-testmodels(knn, knn.final)
plr.pred<-testmodels(plr, plr.final)
rf.pred<-testmodels(rf, rf.final)
svm.pred<-testmodels(svmradial, svm.final)

combined_table<-bind_rows(
  log.pred %>% mutate(Model="Logistic"),
  lda.pred %>% mutate(Model="LDA"),
  qda.pred %>% mutate(Model="QDA"),
  knn.pred %>% mutate(Model="$KNN^{*}$"),
  plr.pred %>% mutate(Model="$PLR^{†}$"),
  rf.pred %>% mutate(Model="$RF^{‡}$"),
  svm.pred %>% mutate(Model="$SVM^{§}$"))

combined_table<-combined_table %>%
  select(Model, AUROC, Threshold, `Balanced.Accuracy`, Kappa, FPR, F1, `Run.Time`)

combined_table<-combined_table %>% 
  rename("Balanced Accuracy" = "Balanced.Accuracy",
         "Test Time (secs)" = "Run.Time")

combined_table<-combined_table %>%
  mutate(across(AUROC:`Test Time (secs)`, ~ round(., digits=4)))

combined_table$`Test Time (secs)` <- sub(" secs", "", combined_table$`Test Time (secs)`)

combined_table$`Test Time (secs)`<-as.numeric(combined_table$`Test Time (secs)`)
combined_table$AUROC <- round(combined_table$AUROC, 5)*100
combined_table$`Balanced Accuracy` <- round(combined_table$`Balanced Accuracy`, 4)*100
combined_table$Kappa <- round(combined_table$Kappa, 4)*100
combined_table$F1 <- round(combined_table$F1, 4)*100
combined_table$FPR <- round(combined_table$FPR, 4)*100
combined_table$`Test Time (secs)` <- round(combined_table$`Test Time (secs)`, 2)

summary_range <- combined_table %>%
  summarise(across(AUROC:`Test Time (secs)`, ~diff(range(.))))


Ranges<-data.frame(Range=c("", "3.28", "", "16.81", "32.21", "0.44", "33.76", "332.33"))

new_row <- data.frame(Model = NA, Threshold = NA, AUROC = NA, Kappa = NA,
                      F1 = NA, FPR = NA,`Test Time (secs)` = NA)

new_row[c("Model", "Threshold")] <- c("Range", NA)
new_row$AUROC <- Ranges$Range[2]
new_row$Balanced.Accuracy <- Ranges$Range[4]
new_row$Kappa <- Ranges$Range[5]
new_row$F1 <- Ranges$Range[6]
new_row$FPR <- Ranges$Range[7]
new_row$Test.Time..secs.<-Ranges$Range[8]

new_row<-new_row %>% 
  rename("Balanced Accuracy" = "Balanced.Accuracy",
         "Test Time (secs)" = "Test.Time..secs.")

combined_table <- rbind(combined_table, new_row)
combined_table <- combined_table %>% relocate("Threshold", .after = "Model")
combined_table <- combined_table %>% relocate("FPR", .after = "F1")

combinedtab<-combined_table %>%
  kbl(caption = "<b>Table 10.2: Model Summary Statistics (Holdout Data)<b>", 
      align="lcccccccc", booktabs=T) %>%
  kable_classic(full_width=T,html_font="Cambria") %>% 
  kable_styling(bootstrap_options=c("striped", "condensed"), position="center", fixed_thead=T) %>%
  row_spec(0, bold=TRUE, extra_css="border-bottom: 3px solid") %>%
  row_spec(seq(1, nrow(combined_table), 1), extra_css="border-top: 2px solid gray;") %>%
  row_spec(8, extra_css="border-top: 3px dashed gray;") %>% 
  row_spec(8, extra_css="border-bottom: 2px solid;") %>% 
  column_spec(1, border_right="0.5px solid black") %>% 
  column_spec(3, border_left="0.5px dashed gray") %>%
  column_spec(4, border_left="0.5px dashed gray") %>%
  column_spec(5, border_left="0.5px dashed gray") %>%
  column_spec(6, border_left="0.5px dashed gray") %>% 
  column_spec(7, border_left="0.5px dashed gray") %>% 
  column_spec(8, border_left="0.5px dashed gray")

add_footnote(combinedtab, c("k = 23.", "α = 0.1474 ┃ λ = 2e-05.", 
                            "mtry = 1 ┃ ntree = 500.", "cost = 0.25 ┃ γ = 1.25."), notation="symbol")

plot_ROC_holdtrain<-function(model, holdout, data, 
                             xlim=c(0,1), ylim=c(0,1), 
                             title="", note=NULL) {
  set.seed(1)
  generate_ROC <- function(model, data, label) {
    pred <- predict(model, newdata = data, type = "prob")
    rates <- ROCR::prediction(pred[, "No"], data$Tarp, label.ordering = c('Yes', 'No'))
    roc <- ROCR::performance(rates, measure = "tpr", x.measure = "fpr")
    auc <- ROCR::performance(rates, measure = "auc")@y.values
    auc <- round(as.numeric(auc), 4)*100
    return(list(roc=roc, auc=auc, label=label))
  }
  
  roc_holdout<-generate_ROC(model, holdout, "Holdout")
  roc_data<-generate_ROC(model, data, "Training")
  plot(roc_holdout$roc, col="#E57200", lwd=2, main=title, cex.main=1.2, xlim=xlim, ylim=ylim,
       xlab="False Positive Rate", ylab="True Positive Rate", font.lab=2, bg='grey')
  plot(roc_data$roc, col="#232D4B", lwd=2, add=TRUE)
  auc_holdout<-roc_holdout$auc
  auc_data<-roc_data$auc
  legend_label_holdout<-paste("Holdout (AUC =", auc_holdout, ")", sep = " ")
  legend_label_data<-paste("Training (AUC =", auc_data, ")", sep = " ")
  legend("bottomright", legend = c(legend_label_holdout, legend_label_data),
         col = c("#E57200", "#232D4B"), lwd = 2, cex = 0.8, bg = 'grey95', text.font = 2)
  
  if (!is.null(note)) {mtext(note, side=3, font=2, cex=0.9)}
}

stopCluster(cl)
registerDoSEQ()