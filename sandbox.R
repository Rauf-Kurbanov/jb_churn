library(corrplot)

mm <- as.matrix(df[, names(df) %in% numeric_cols])
nmm <- apply(mm, 2, function(x)(x-min(x))/(max(x)-min(x)))
corrplot(nmm, method="number")

View(head(df[df$dead == 1, ], 50))

tt <- df$license_expiration_date - df$placed_date
tt <- as.numeric(tt)
summary(tt)
hist(tt)

pp <- df$license_expiration_date - df$processed_date
pp <- as.numeric(pp)
summary(pp)
hist(pp)

dat <- data.frame(
  group = rep(c("Новые \nпользователи", "Ушедшие \nпользователи"), each = length(df$dead)),
  x = rep(actual.payments$date.fac[-c(1, 24:29)], 2),#rep(1:10, 2),
  y = c(actual.payments$new.users[-c(1, 24:29)], -actual.payments$died.users[-c(1, 24:29)])
)

library(ggplot2)

ggplot(dat, aes(x=x, y=y, fill=group)) + 
  geom_bar(stat="identity", position="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 20), 
        axis.text.y = element_text(angle = 90, hjust = 1, size = 18), 
        legend.text=element_text(size=20),
        axis.title=element_text(size=20),
        title=element_text(size=20)) +
  labs(size= 20,
       x = "Месяцы",
       y = "Количество пользователей",
       title = "Динамика абонентской базы по месяцам") +
  scale_fill_manual(values=c("dodgerblue4", "grey11"))

dead_times <- df[df$dead == 1, ]

pd
prd
se

se_pd <- as.numeric(df$subscription_expiration_date - df$placed_date)
se_prd <- as.numeric(df$subscription_expiration_date - df$processed_date)

summary(se_pd)
hist(se_pd, xlim = c(0, 100), breaks = 500, ylim = c(0, 1000))
outdated <- factor(se_pd < 0)
library(plyr)
outdated <- revalue(outdated, c("FALSE"="on_time", "TRUE"="outdated"))
summary(outdated)

mby_dead <- outdated == levels(outdated)[1]
View(head(df[mby_dead, ], 50))

View(head(df[df$dead == 1, ], 50))


summary(se_prd)


## pie chart

#########################
##########################################################################################################################
outd_targ <- as.numeric(outdated) - 1
df <- transform(df, outdated = outd_targ)
rf.cv(df = df, formula = formula(outdated ~ . - placed_date - subscription_expiration_date), nchuncks = 5)


summary(rf.fit)

plot(rf.fit)

importance(rf.fit)

varImpPlot(rf.fit)

plot.roc(df[test_set, ]$dead, rf.pred)

# getTree(rf.fit)

# library(randomForest)
# rf.fit <- randomForest(dead ~ ., df[train_set, ])
class(df$dead)
# df$dead <- factor(df$dead)

train <- df[train_set, ]
test <- df[test_set, ]

library(e1071)
#Full Data set can be used for cross validation
knn.cross <- tune.knn(x = df, y = df$dead ,tunecontrol=tune.control(sampling = "cross"), cross=10)
#Summarize the resampling results set
summary(knn.cross)


kknn.fit <- kknn(dead ~ . - dead, train, test)
# knn.fit <- kknn.fit$fitted.values
knn.pred <- kknn.fit$fitted.values
knn.pred <- as.numeric(knn.pred > 0)
mmetric(y=factor(df[test_set, ]$dead), x=factor(knn.pred), metric = 'ALL')

knn.cv(df = df, formula = formula(dead ~ . - customer_status), nchuncks = 5)


# length(knn.fit$fitted.values)
# dim(test)
# knn.pred <- predict(knn.fit, test)
# knn.pred

svm.fit <- svm(dead ~ ., train)
svm.pred <- predict(svm.fit, test)

svm.pred <- predict(svm.fit, df[test_set, ])
svm.pred <- as.numeric(svm.pred < 0)
xx <- svm.pred
yy <- df[test_set, ]$dead
mmetric(y=factor(df[test_set, ]$dead), x=factor(svm.pred), metric = 'ALL')

summary(yy)

###############################################################################################

# 'customer_location' not found
# lm.lazy.fit <- lm(dead~quantity + amount_in_currency + amount_in_usd + original_price + stock_price + 
#                   stock_price_in_usd + item_revenue + discount_perc + discount_in_currency + discount_in_usd +
#                   vat_id_verified,
#                   stock_id, customer_id, reseller_id, license_expiration_date, customer_status,
#                  customer_location, customer_type, trusted, term, account_manager, iso, name,
#                   currency, region, channel, market_region, email_region
#                   , data=df)
glm.lazy.fit <- glm(dead~., data = df)
summary(glm.lazy.fit)


# glm.lazy.fit <- glm(dead~quantity + amount_in_currency + amount_in_usd + original_price + stock_price + 
#                   stock_price_in_usd + item_revenue + discount_perc + discount_in_currency + discount_in_usd +
#                  vat_id_verified, data=df)
# summary(glm.lazy.fit)
# factor_cols
# lapply(df, class)
names(df)

# head(df[,1:10])
# head(df[,11:20])
# head(df[,21:dim(df)[2]])
# any(lapply(df, function(x) {any(is.na(x))}))
class(df$item_revenue)
summary(df$item_revenue)

# Кажется processed_date и placed_date одно и то же. Хотя R выдаёт что-то странное. Надо проверить, 
# если что на них можно забить или наоборот получится неплохая фича.
# apply(df, 2, class)
# date in range 2012-01-02 - 2015-12-31
pd <- as.Date(df$processed_date, "%m/%d/%Y")
pcd <- as.Date(df$placed_date, "%m/%d/%Y")
# as.numeric(pd - pcd)
# which(as.character(df$placed_date) != as.character(df$processed_date)
# head(df[c('"placed_date", "processed_date"'), ], 15)
# df[ , c("placed_date", "processed_date")]

# amount_in_currency и amount_in_usd выражаются друг через друга линейно, что ожидаемо, но там есть какое-то 
# поведение. Может эта штука завязана на курсе валюты и тоже можно фигануть интересную фичулю.
aic <- as.numeric(df$amount_in_currency)
aius <- as.numeric(df$amount_in_usd)
# plot(aic, aius)

# Здесть что-то странное. Кажется эта разница информативна, но есть серьёзные аутлаеры, с природой которых стоит 
# разобраться. 
sp <- as.numeric(df$stock_price)
op <- as.numeric(df$original_price)
# hist(sp - op)
summary(sp-op)

summary(df$type)

tmp <- df[df$dead == 1, ]
# head(tmp)


# pca <- prcomp(df,
#                  center = TRUE,
#                  scale. = TRUE) 
num_cols <- sapply(df, function(x) {class(x) == 'numeric'})
num.df <- df[, num_cols]

# num.df <- log(num.df)
pca <- prcomp(num.df,
              center = TRUE,
              scale. = TRUE) 

summary(pca)

head()


