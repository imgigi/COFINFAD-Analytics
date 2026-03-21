# ============================================================
# Data Preparation Script for COFINFAD Shiny Application
# Pre-processes raw CSV -> optimized RDS files
# Includes: grouped imputation, feature engineering,
#           pre-computed clusters, pre-trained models, spatial data
# ============================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(glmnet)
library(cluster)

cat("=== Loading raw data ===\n")
customers_raw <- read.csv("../customer_data (1).csv", stringsAsFactors = FALSE)
transactions_raw <- read.csv("../transactions_data (1).csv", stringsAsFactors = FALSE)
cat("Customers:", nrow(customers_raw), "rows,", ncol(customers_raw), "cols\n")
cat("Transactions:", nrow(transactions_raw), "rows,", ncol(transactions_raw), "cols\n")

# ============================================================
# 1. Clean Customer Data
# ============================================================
cat("\n=== Cleaning customer data ===\n")

customers <- customers_raw %>%
  mutate(
    first_tx = as.Date(first_tx),
    last_tx = as.Date(last_tx),
    last_survey_date = as.Date(last_survey_date),
    last_transaction_date = as.Date(last_transaction_date),
    first_transaction_date = as.Date(first_transaction_date),
    savings_account = (savings_account == "True"),
    credit_card = (credit_card == "True"),
    personal_loan = (personal_loan == "True"),
    investment_account = (investment_account == "True"),
    insurance_product = (insurance_product == "True"),
    bill_payment_user = (bill_payment_user == "True"),
    auto_savings_enabled = (auto_savings_enabled == "True"),
    city = trimws(sub(",.*", "", location)),
    department = trimws(sub(".*,", "", location)),
    age_group = cut(age, breaks = c(17, 25, 35, 45, 55, 65, 100),
                    labels = c("18-25","26-35","36-45","46-55","56-65","65+")),
    churn_risk = factor(
      case_when(churn_probability < 0.2 ~ "Low",
                churn_probability < 0.35 ~ "Medium",
                TRUE ~ "High"),
      levels = c("Low","Medium","High")
    ),
    income_bracket = factor(income_bracket, levels = c("Low","Medium","High","Very High")),
    customer_segment = factor(customer_segment, levels = c("inactive","occasional","regular","power")),
    clv_segment = factor(clv_segment, levels = c("Bronze","Silver","Gold","Platinum")),
    feedback_sentiment = factor(feedback_sentiment, levels = c("Negative","Neutral","Positive")),
    recency_days = as.numeric(as.Date("2024-01-01") - last_tx),
    tenure_months = round(customer_tenure, 1),
    income_numeric = as.numeric(income_bracket)
  ) %>%
  # Grouped median imputation for credit_utilization_ratio (by income_bracket)
  group_by(income_bracket) %>%
  mutate(credit_utilization_ratio = ifelse(
    is.na(credit_utilization_ratio),
    median(credit_utilization_ratio, na.rm = TRUE),
    credit_utilization_ratio
  )) %>%
  ungroup() %>%
  # Feature engineering
  mutate(
    product_diversity_ratio = active_products / 5,
    ticket_resolution_gap = support_tickets_count * (1 - coalesce(resolved_tickets_ratio, 0))
  )

cat("Missing credit_utilization_ratio after grouped imputation:",
    sum(is.na(customers$credit_utilization_ratio)), "\n")

# ============================================================
# 2. Process Transactions
# ============================================================
cat("\n=== Processing transactions ===\n")

transactions <- transactions_raw %>%
  mutate(
    date = as.Date(date),
    month = floor_date(date, "month"),
    is_weekend = wday(date) %in% c(1, 7)
  )

# Monthly aggregation by type (for time-series chart)
monthly_summary <- transactions %>%
  group_by(month, type) %>%
  summarise(total_amount = sum(amount, na.rm = TRUE),
            avg_amount = mean(amount, na.rm = TRUE),
            tx_count = n(), .groups = "drop")

# Monthly total (for date range reference)
monthly_total <- transactions %>%
  group_by(month) %>%
  summarise(total_amount = sum(amount, na.rm = TRUE),
            avg_amount = mean(amount, na.rm = TRUE),
            tx_count = n(), .groups = "drop")

# Per-customer transaction aggregation
customer_tx_agg <- transactions %>%
  group_by(customer_id) %>%
  summarise(total_amount = sum(amount, na.rm = TRUE),
            avg_amount = mean(amount, na.rm = TRUE),
            sd_amount = sd(amount, na.rm = TRUE),
            tx_count = n(), .groups = "drop")

# Per-customer by type pivot
customer_type_agg <- transactions %>%
  group_by(customer_id, type) %>%
  summarise(type_amount = sum(amount, na.rm = TRUE),
            type_count = n(), .groups = "drop") %>%
  pivot_wider(names_from = type, values_from = c(type_amount, type_count), values_fill = 0)

# ============================================================
# 3. Merge
# ============================================================
cat("\n=== Building analytics dataset ===\n")

analytics_data <- customers %>%
  left_join(customer_tx_agg %>% select(customer_id, sd_amount), by = "customer_id") %>%
  left_join(customer_type_agg, by = "customer_id") %>%
  mutate(
    spending_volatility = coalesce(sd_amount, 0),
    across(starts_with("type_amount_"), ~coalesce(., 0)),
    across(starts_with("type_count_"), ~coalesce(., 0))
  )

# ============================================================
# 4. RFM / Clustering Features
# ============================================================
cat("\n=== Preparing clustering features ===\n")

rfm_data <- analytics_data %>%
  select(customer_id, recency_days, tx_count, total_tx_volume,
         avg_tx_value, spending_volatility, customer_tenure,
         credit_utilization_ratio, weekend_transaction_ratio,
         active_products, satisfaction_score, churn_probability,
         income_bracket, customer_segment, clv_segment, age_group,
         churn_risk, acquisition_channel,
         starts_with("type_amount_"), starts_with("type_count_")) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))

# ============================================================
# 5. Pre-compute Clusters
# ============================================================
cat("\n=== Pre-computing clusters (this may take a few minutes) ===\n")

cluster_presets <- list(
  rfm = list(name = "RFM Features",
             features = c("tx_count", "avg_tx_value", "customer_tenure")),
  behavioral = list(name = "Full Behavioral",
                    features = c("tx_count", "avg_tx_value", "total_tx_volume",
                                 "weekend_transaction_ratio", "customer_tenure",
                                 "credit_utilization_ratio")),
  engagement = list(name = "Engagement",
                    features = c("active_products", "satisfaction_score",
                                 "credit_utilization_ratio", "spending_volatility",
                                 "customer_tenure"))
)

all_feat <- unique(unlist(lapply(cluster_presets, function(p) p$features)))
rfm_clean <- rfm_data %>%
  select(customer_id, all_of(all_feat),
         income_bracket, customer_segment, clv_segment, churn_risk, acquisition_channel) %>%
  drop_na()
cat("Clustering dataset:", nrow(rfm_clean), "customers\n")

compute_wss <- function(sc, labels, k) {
  wss <- 0
  for (i in 1:k) {
    pts <- sc[labels == i, , drop = FALSE]
    if (nrow(pts) > 0) wss <- wss + sum(sweep(pts, 2, colMeans(pts))^2)
  }
  wss
}

precompute_preset <- function(preset, data) {
  feats <- preset$features
  cat("  Preset:", preset$name, "\n")

  sc <- scale(as.matrix(data[, feats]))
  pca <- prcomp(sc, center = FALSE, scale. = FALSE)
  n_pcs <- min(3, ncol(sc))
  pca_df <- as.data.frame(pca$x[, 1:n_pcs])
  colnames(pca_df) <- paste0("PC", 1:n_pcs)
  ve <- summary(pca)$importance[2, 1:n_pcs]

  n <- nrow(sc)
  set.seed(42)
  sil_n <- min(3000, n)
  sil_idx <- sample(n, sil_n)
  sc_sil <- sc[sil_idx, ]
  d_sil <- dist(sc_sil)

  # ---- K-Means ----
  cat("    K-Means...")
  km_lab <- matrix(NA_integer_, nrow = n, ncol = 7)
  km_wss <- km_sil <- numeric(7)
  for (ki in 1:7) {
    k <- ki + 1
    set.seed(42)
    km <- kmeans(sc, k, nstart = 25, iter.max = 100)
    km_lab[, ki] <- km$cluster
    km_wss[ki] <- km$tot.withinss
    set.seed(42)
    km_s <- kmeans(sc_sil, k, nstart = 10, iter.max = 50)
    km_sil[ki] <- mean(silhouette(km_s$cluster, d_sil)[, 3])
  }
  colnames(km_lab) <- paste0("k", 2:8)
  cat(" done\n")

  # ---- Hierarchical (Ward) ----
  cat("    Hierarchical...")
  hc_lab <- matrix(NA_integer_, nrow = n, ncol = 7)
  hc_wss <- hc_sil <- numeric(7)
  hc_n <- min(8000, n)
  set.seed(42)
  hc_idx <- sample(n, hc_n)
  hc_obj <- hclust(dist(sc[hc_idx, ]), method = "ward.D2")
  for (ki in 1:7) {
    k <- ki + 1
    hc_cut <- cutree(hc_obj, k)
    cen <- sapply(1:k, function(i) colMeans(sc[hc_idx, ][hc_cut == i, , drop = FALSE]))
    if (n > hc_n) {
      hc_lab[, ki] <- apply(sc, 1, function(r) which.min(colSums((cen - r)^2)))
    } else {
      hc_lab[, ki] <- hc_cut
    }
    hc_wss[ki] <- compute_wss(sc, hc_lab[, ki], k)
    hc_sil[ki] <- mean(silhouette(hc_lab[sil_idx, ki], d_sil)[, 3])
  }
  colnames(hc_lab) <- paste0("k", 2:8)
  cat(" done\n")

  # ---- PAM (CLARA) ----
  cat("    PAM/CLARA...")
  pam_lab <- matrix(NA_integer_, nrow = n, ncol = 7)
  pam_wss <- pam_sil <- numeric(7)
  for (ki in 1:7) {
    k <- ki + 1
    set.seed(42)
    cl <- clara(sc, k, metric = "euclidean", samples = 50,
                sampsize = min(500, n))
    pam_lab[, ki] <- cl$clustering
    pam_wss[ki] <- compute_wss(sc, cl$clustering, k)
    pam_sil[ki] <- mean(silhouette(cl$clustering[sil_idx], d_sil)[, 3])
  }
  colnames(pam_lab) <- paste0("k", 2:8)
  cat(" done\n")

  list(
    name = preset$name,
    features = feats,
    pca_scores = pca_df,
    var_explained = ve,
    methods = list(
      kmeans = list(labels = km_lab, wss = km_wss, sil = km_sil),
      hclust = list(labels = hc_lab, wss = hc_wss, sil = hc_sil),
      pam    = list(labels = pam_lab, wss = pam_wss, sil = pam_sil)
    )
  )
}

cluster_precomputed <- list(
  customer_ids = rfm_clean$customer_id,
  demographics = rfm_clean %>% select(customer_id, income_bracket, customer_segment,
                                       clv_segment, churn_risk, acquisition_channel),
  raw_features = rfm_clean %>% select(customer_id, all_of(all_feat)),
  presets = lapply(cluster_presets, precompute_preset, data = rfm_clean)
)

# ============================================================
# 6. Pre-train Models
# ============================================================
cat("\n=== Pre-training glmnet models ===\n")

model_df <- analytics_data %>%
  select(churn_probability, age, tx_count, avg_tx_value, total_tx_volume,
         satisfaction_score, credit_utilization_ratio, customer_tenure,
         active_products, nps_score, weekend_transaction_ratio,
         support_tickets_count, resolved_tickets_ratio, app_logins_frequency,
         feature_usage_diversity, spending_volatility, household_size,
         failed_transactions, product_diversity_ratio, ticket_resolution_gap) %>%
  drop_na()

y <- model_df$churn_probability
X <- as.matrix(model_df %>% select(-churn_probability))
cat("Training data:", nrow(X), "obs x", ncol(X), "features\n")

set.seed(42)
cv_lasso <- cv.glmnet(X, y, alpha = 1, nfolds = 10)
fit_lasso <- glmnet(X, y, alpha = 1)

set.seed(42)
cv_ridge <- cv.glmnet(X, y, alpha = 0, nfolds = 10)
fit_ridge <- glmnet(X, y, alpha = 0)

model_precomputed <- list(
  lasso = list(cv = cv_lasso, fit = fit_lasso),
  ridge = list(cv = cv_ridge, fit = fit_ridge),
  X = X, y = y,
  X_means = colMeans(X),
  X_colnames = colnames(X)
)
cat("Lasso lambda.min:", round(cv_lasso$lambda.min, 6), "\n")
cat("Ridge lambda.min:", round(cv_ridge$lambda.min, 6), "\n")

# ============================================================
# 7. Spatial Data
# ============================================================
cat("\n=== Preparing spatial data ===\n")

dept_stats <- customers %>%
  group_by(department) %>%
  summarise(
    customer_count = n(),
    avg_churn_prob = mean(churn_probability, na.rm = TRUE),
    avg_income = mean(as.numeric(income_bracket), na.rm = TRUE),
    avg_clv = mean(customer_lifetime_value, na.rm = TRUE),
    avg_satisfaction = mean(satisfaction_score, na.rm = TRUE),
    .groups = "drop"
  )

tryCatch({
  library(sf)
  library(rnaturalearth)
  col_sf <- ne_states(country = "colombia", returnclass = "sf")

  # Normalise department name for Bogota
  col_sf$dept_match <- col_sf$name
  col_sf$dept_match[grepl("Bogot|Capital", col_sf$name, ignore.case = TRUE)] <- "Bogotá"

  dept_sf <- col_sf %>%
    select(dept_match, geometry) %>%
    left_join(dept_stats, by = c("dept_match" = "department"))

  # Report any unmatched departments
  unmatched <- dept_stats$department[!dept_stats$department %in% dept_sf$dept_match]
  if (length(unmatched) > 0) {
    cat("  Unmatched departments:", paste(unmatched, collapse = ", "), "\n")
    cat("  Attempting fuzzy match...\n")
    for (um in unmatched) {
      best <- col_sf$dept_match[which.min(adist(um, col_sf$dept_match))]
      cat("    ", um, " -> ", best, "\n")
    }
  }

  saveRDS(dept_sf, "data/dept_spatial.rds")
  cat("  Spatial data saved\n")
}, error = function(e) {
  cat("  Warning:", conditionMessage(e), "\n")
  cat("  Install sf + rnaturalearth for choropleth map\n")
})

# ============================================================
# 8. Save
# ============================================================
cat("\n=== Saving ===\n")

saveRDS(customers, "data/customers.rds")
saveRDS(analytics_data, "data/analytics_data.rds")
saveRDS(monthly_summary, "data/monthly_summary.rds")
saveRDS(monthly_total, "data/monthly_total.rds")
saveRDS(rfm_data, "data/rfm_data.rds")
saveRDS(cluster_precomputed, "data/cluster_precomputed.rds")
saveRDS(model_precomputed, "data/model_precomputed.rds")
saveRDS(dept_stats, "data/dept_stats.rds")

cat("\n=== Data preparation complete! ===\n")
cat("Files saved to data/ directory:\n")
cat("  customers.rds, analytics_data.rds, monthly_summary.rds,\n")
cat("  monthly_total.rds, rfm_data.rds, cluster_precomputed.rds,\n")
cat("  model_precomputed.rds, dept_stats.rds, dept_spatial.rds\n")
