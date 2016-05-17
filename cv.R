rf.cv <- function(df, formula, nchuncks) {
  library(randomForest)
  df.size <- dim(df)[1]
  ind = 1 : df.size
  ind.perm <- sample(ind, df.size, replace = F)
  chunk.size <- df.size / (nchuncks + 1)
  splitted <- split(ind.perm, ceiling(ind/chunk.size))
  test.set <- splitted[1][[1]]
  train.chunks <- splitted[-1]
  
  err.on.chunk <- function(chunk) {
    chunk <- chunk[[1]]
    model <- randomForest(formula, data=df[chunk, ])
    pred <- predict(model, df[chunk, ])
    pred <- as.numeric(pred > 0)
    mmetric(y=factor(df[chunk, ]$dead), x=factor(pred), metric = 'ALL')
  }
  
  errors <- err.on.chunk(train.chunks[1])
  metric_names <- names(errors)
  for (i in 2:nchuncks) {
    errors <- c(errors, err.on.chunk(train.chunks[i]))
  }
  
  errmat = matrix( 
    errors, 
    nrow=length(errors) / nchuncks, 
    ncol=nchuncks
  ) 
  
  means <- apply(errmat, 1, mean)
  names(means) <- metric_names
  sds <- apply(errmat, 1, sd)
  names(sds) <- metric_names
  
  cat("\nFormula :\n")
  print(formula)
  cat("\nMetric means :\n")
  print(means)
  cat("\nMetric standart deviations :\n")
  print(sds)
}

knn.cv <- function(df, formula, nchuncks) {
  library(randomForest)
  df.size <- dim(df)[1]
  ind = 1 : df.size
  ind.perm <- sample(ind, df.size, replace = F)
  chunk.size <- df.size / (nchuncks + 1)
  splitted <- split(ind.perm, ceiling(ind/chunk.size))
  test.set <- splitted[1][[1]]
  train.chunks <- splitted[-1]
  
  err.on.chunk <- function(chunk) {
    chunk <- chunk[[1]]
    model <- kknn(formula, df[chunk, ], df[chunk, ])
    # pred <- predict(model, df[chunk, ])
    pred <- model$fitted.values
    pred <- as.numeric(pred > 0)
    mmetric(y=factor(df[chunk, ]$dead), x=factor(pred), metric = 'ALL')
  }
  
  errors <- err.on.chunk(train.chunks[1])
  metric_names <- names(errors)
  for (i in 2:nchuncks) {
    errors <- c(errors, err.on.chunk(train.chunks[i]))
  }
  
  errmat = matrix( 
    errors, 
    nrow=length(errors) / nchuncks, 
    ncol=nchuncks
  ) 
  
  means <- apply(errmat, 1, mean)
  names(means) <- metric_names
  sds <- apply(errmat, 1, sd)
  names(sds) <- metric_names
  
  cat("\nFormula :\n")
  print(formula)
  cat("\nMetric means :\n")
  print(means)
  cat("\nMetric standart deviations :\n")
  print(sds)
}

svm.cv <- function(df, formula, nchuncks) {
  df.size <- dim(df)[1]
  ind = 1 : df.size
  ind.perm <- sample(ind, df.size, replace = F)
  chunk.size <- df.size / (nchuncks + 1)
  splitted <- split(ind.perm, ceiling(ind/chunk.size))
  test.set <- splitted[1][[1]]
  train.chunks <- splitted[-1]
  
    err.on.chunk <- function(chunk) {
    chunk <- chunk[[1]]
    model <- svm(formula, df[chunk, ])
    pred <- predict(model, df[chunk, ])
    pred <- as.numeric(pred < 0)
    xx <- pred
    yy <- df[test_set, ]$dead
    # mmetric(y=yy, x=xx, metric = 'ALL')
    mmetric(y=factor(df[chunk, ]$dead), x=factor(pred), metric = 'ALL')
    
  }
  
  errors <- err.on.chunk(train.chunks[1])
  metric_names <- names(errors)
  for (i in 2:nchuncks) {
    errors <- c(errors, err.on.chunk(train.chunks[i]))
  }
  
  errmat = matrix( 
    errors, 
    nrow=length(errors) / nchuncks, 
    ncol=nchuncks
  ) 
  
  means <- apply(errmat, 1, mean)
  names(means) <- metric_names
  sds <- apply(errmat, 1, sd)
  names(sds) <- metric_names
  
  cat("\nFormula :\n")
  print(formula)
  cat("\nMetric means :\n")
  print(means)
  cat("\nMetric standart deviations :\n")
  print(sds)
}

