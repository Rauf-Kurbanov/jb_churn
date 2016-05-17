rm(list=ls())
gc()
setwd("/home/rauf/Programs/semester_2/jb_churn")

source("jb_churn/cv.R")
df <- read.csv("purchases_org_direct.csv")

library(rminer)
library(randomForest)
# library(pROC)
# library(kknn)
# library(e1071)
library(plyr)
library(PerformanceAnalytics)
library(zoo)

na_cols <- which(apply(df, 2, function(x) {all(is.na(x))} ))
df <- df[, -na_cols]
bad_rows <- which(is.na(df$stock_price))
df <- df[-bad_rows, ]

# license_expiration_date - Блин! точняк
# subscription_expiration_date
bad_cols <- c("id", "days", "calculated_price", "discount_applied", "discount_desc", "is_active", "refund", 
              "custom_discount", "custom_discount_desc", "youtrack_license_type", 'id.1', 'code', 'type', 
              'order_id', "stock_id", "customer_id", "license_expiration_date",
              "reseller_id", "region", "channel", "market_region", "customer_type", "reseller_currency", "css_id", 
              "vat_id", "customer_name", "printable_name", "spellings", "weight", 
              "customer_location", "iso", "name")

bc_num <- names(df) %in% bad_cols 
df <- df[, which(!bc_num)]
# head(df)

## setting column types
numeric_cols <- c("quantity", "amount_in_currency", "amount_in_usd", "original_price", "stock_price", 
                  "stock_price_in_usd", "item_revenue", "discount_perc", "discount_in_currency", "discount_in_usd")
are_numeric <- names(df) %in% numeric_cols
num_nums <- which(are_numeric)

date_cols <- c("placed_date", "processed_date", "subscription_expiration_date", "license_expiration_date")
are_date <- names(df) %in% date_cols
date_nums <- which(are_date)

are_factor <- !(are_numeric | are_date)
factor_cols <- names(df)[which(are_factor)]
fac_nums <- which(are_factor)

df[, fac_nums] <- lapply(df[ ,fac_nums] , factor)
# df[, date_nums] <- lapply(df[ , date_nums] , function (x) {as.Date(x, "%m/%d/%Y")})
df[, date_nums] <- lapply(df[ , date_nums] , function (x) {as.Date(x, "%Y-%m-%d")})
df[, num_nums] <- lapply(df[ , num_nums] , as.numeric)

df$sku <- as.character(df$sku)
df$sku <- sapply(df$sku, function(x) {strsplit(x, split = "-")[[1]][2]})
df$sku <- factor(df$sku)

# Only intellij idea
idea_lines <- which( as.character(df$sku) == 'II' )
df <- df[idea_lines, ]
df$sku = NULL
# head(df)

## filling missing values

t <- na.locf(df$license_expiration_date)
df$subscription_expiration_date <- na.locf(df$subscription_expiration_date)
##
with_na_cols <- sapply(df, function(x) {any(is.na(x))})
each_na_rows <- sapply(df[, with_na_cols], function(x) which(is.na(x)))
all_na_rows <- Reduce(union, each_na_rows, vector())
df <- df[-all_na_rows, ]

# 2 - fold CV
dlen <- dim(df)[1]
range <- 1:dlen
test_set <- sample(range, dlen %/% 4)
train_set <- range %in% test_set
train_set <- range[train_set]

## dead
past_due_renew_fac <- levels(df$license_type)[2]
num_targ <- as.numeric(df$license_type == past_due_renew_fac)
df <- transform(df, dead = num_targ)

df <- df[ , !(names(df) %in% c("license_type"))]

## outdated
# summaryse_pd)
# hist(se_pd, xlim = c(0, 100), breaks = 500, ylim = c(0, 1000))
se_pd <- as.numeric(df$subscription_expiration_date - df$placed_date)
se_prd <- as.numeric(df$subscription_expiration_date - df$processed_date)

outdated <- factor(se_pd < 0)

outdated <- revalue(outdated, c("FALSE"="on_time", "TRUE"="outdated"))
# summary(outdated)
df <- transform(df, outdated = outdated)


##

chart.Correlation(df[, names(df) %in% c("stock_price", "stock_price_in_usd", "original_price")])

df$original_price <- NULL
##

# any(is.na(df))
# sapply(df, function(x) {any(is.na(x))})
# 
# with_na_cols <- sapply(df, function(x) {any(is.na(x))})
# each_na_rows <- sapply(df[, with_na_cols], function(x) which(is.na(x)))
# all_na_rows <- Reduce(union, each_na_rows, vector())
# df <- df[-all_na_rows, ]


rf.fit <- randomForest(dead ~ . , df[train_set, ])

# head(df)
# class(df$dead)

rf.pred <- predict(rf.fit, df[test_set, ])
rf.pred <- as.numeric(rf.pred > 0)

mmetric(y=factor(df[test_set, ]$dead), x=factor(rf.pred), metric = 'ALL')
# mmetric(y=factor(df[test_set, ]$dead), x=factor(rf.pred), metric = 'F1')
t <- mmetric(y=factor(df[test_set, ]$dead), x=factor(rf.pred), metric = 'ALL')

# library(pROC)
# plot.roc(df[test_set, ]$dead, rf.pred)

rf.cv(df = df, formula = formula(dead ~ . - customer_status), nchuncks = 5)
knn.cv(df = df, formula = formula(dead ~ . - customer_status), nchuncks = 5)
svm.cv(df = df, formula = formula(dead ~ . - customer_status), nchuncks = 5)


