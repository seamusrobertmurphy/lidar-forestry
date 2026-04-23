## Reproduce the model-comparison and univariate tables reported in
## 04-biomass-model/index.qmd (§4.8). Runs seven algorithms on the
## 150 simulated plots drawn from the Carr covariate stack with 10-fold
## CV, reports full-data and CV RMSE, full MAE, and the RMSE ratio.
##
## Mirrors the pipeline from the EFI assessment repo, adapted for the
## Sierra sample: e1071::tune.randomForest for the RF/SVM tune grid and
## caret::train for the preProcess-honouring kernel SVMs, GLM, and the
## caretEnsemble stack.

suppressPackageStartupMessages({
  library(terra); library(caret); library(kernlab); library(e1071)
  library(randomForest); library(caretEnsemble)
  library(ModelMetrics); library(glmnet)
})

dtm       <- rast("data/dtm_1m.tif")
slope     <- rast("data/slope_1m.tif")
northness <- rast("data/northness_1m.tif")
eastness  <- rast("data/eastness_1m.tif")
chm       <- rast("data/chm_1m.tif")
spp       <- rast("data/species.tif")
chm_r     <- resample(chm, dtm, method = "bilinear")
spp_r     <- resample(spp, dtm, method = "near")
chm_r[chm_r < 1.3] <- NA
covs <- c(dtm, slope, northness, eastness, chm_r, spp_r)
names(covs) <- c("elev", "slope", "asp_cos", "asp_sin", "chm", "species")

set.seed(20230718)
bb  <- ext(covs)
pts <- data.frame(x = runif(150, bb[1] + 5, bb[2] - 5),
                  y = runif(150, bb[3] + 5, bb[4] - 5))
plot_cov <- cbind(pts, terra::extract(covs, as.matrix(pts)))
plot_cov$wsvha_L <- with(plot_cov,
  pmax(0, 18 * chm + 0.8 * slope + 30 * (species == 2) - 5 +
         rnorm(nrow(plot_cov), 0, 25)))
plot_cov <- na.omit(plot_cov)
cat(sprintf("plot_cov retained: %d rows\n", nrow(plot_cov)))

preds  <- c("elev", "slope", "asp_cos", "asp_sin", "chm", "species")
df     <- plot_cov[, c(preds, "wsvha_L")]
X_full <- plot_cov[, preds]
y_full <- plot_cov$wsvha_L
cv10   <- trainControl(method = "cv", number = 10, savePredictions = "final")
pp     <- c("BoxCox", "center", "scale")

row <- function(label, full_rmse, cv_rmse, full_mae) {
  ratio <- if (is.finite(full_rmse) && is.finite(cv_rmse) && cv_rmse > 0)
    full_rmse / cv_rmse else NA
  data.frame(model = label,
             full_rmse = round(full_rmse, 2),
             cv_rmse   = round(cv_rmse,   2),
             full_mae  = round(full_mae,  2),
             rmse_ratio = round(ratio, 2))
}

out <- list()

set.seed(1)
tR <- tune.randomForest(X_full, y_full, mtry = 2:6, ntree = 50,
                        tunecontrol = tune.control(sampling = "cross", cross = 10))
m <- tR$best.model
out[[1]] <- row(sprintf("RF (e1071, mtry=%d, ntree=50)", m$mtry),
                RMSE(predict(m, X_full), y_full),
                sqrt(tR$best.performance),
                MAE(predict(m, X_full), y_full))

set.seed(1)
tC <- train(wsvha_L ~ ., data = df, method = "rf",
            metric = "RMSE", tuneLength = 6,
            trControl = cv10, preProcess = pp)
out[[2]] <- row(sprintf("RF (caret, mtry=%d, ntree=500)", tC$bestTune$mtry),
                RMSE(predict(tC, X_full), y_full),
                min(tC$results$RMSE),
                MAE(predict(tC, X_full), y_full))

set.seed(1)
tSR <- train(wsvha_L ~ ., data = df, method = "svmRadial",
             metric = "RMSE", tuneLength = 6,
             trControl = cv10, preProcess = pp)
out[[3]] <- row(sprintf("SVM Radial (C=%.2f, sigma=%.3f)",
                        tSR$bestTune$C, tSR$bestTune$sigma),
                RMSE(predict(tSR, X_full), y_full),
                min(tSR$results$RMSE),
                MAE(predict(tSR, X_full), y_full))

set.seed(1)
tSL <- train(wsvha_L ~ ., data = df, method = "svmLinear",
             metric = "RMSE", tuneLength = 6,
             trControl = cv10, preProcess = pp)
out[[4]] <- row("SVM Linear (caret)",
                RMSE(predict(tSL, X_full), y_full),
                min(tSL$results$RMSE),
                MAE(predict(tSL, X_full), y_full))

set.seed(1)
tG <- train(wsvha_L ~ ., data = df, method = "glm",
            metric = "RMSE",
            trControl = cv10, preProcess = pp)
out[[5]] <- row("GLM (caret)",
                RMSE(predict(tG, X_full), y_full),
                min(tG$results$RMSE),
                MAE(predict(tG, X_full), y_full))

set.seed(1)
mlist <- caretList(wsvha_L ~ ., data = df, trControl = cv10,
                   methodList = c("glm", "svmLinear", "rf"),
                   tuneLength = 3, preProcess = pp,
                   continue_on_fail = TRUE)
stk <- caretStack(mlist, method = "glmnet", metric = "RMSE",
                  trControl = trainControl(method = "cv", number = 10,
                                           savePredictions = "final"),
                  tuneLength = 4)
out[[6]] <- row("Ensemble GLMnet",
                NA_real_, min(stk$ens_model$results$RMSE), NA_real_)

model_tbl <- do.call(rbind, out)
print(model_tbl, row.names = FALSE)

## Univariate linear regressions. Drop predictors that are constant on
## the CHM-filtered plot set (species collapses to 2 everywhere once
## non-vegetated pixels are removed).
uni_vars <- c("chm", "elev", "asp_cos", "slope", "asp_sin")
uni <- lapply(uni_vars, function(v) {
  m <- lm(as.formula(paste("wsvha_L ~", v)), data = plot_cov)
  s <- summary(m)
  data.frame(predictor = v,
             r2        = round(s$r.squared, 4),
             coef      = round(coef(m)[2],  3),
             p_value   = signif(s$coefficients[2, 4], 3))
})
uni_tbl <- do.call(rbind, uni)
print(uni_tbl, row.names = FALSE)

write.csv(model_tbl, "data/model_comparison.csv", row.names = FALSE)
write.csv(uni_tbl,   "data/univariate_lm.csv",    row.names = FALSE)

cat("build_model_comparison.R complete\n")
