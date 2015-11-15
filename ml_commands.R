install.packages("rpart")
install.packages("dplyr")
install.packages("caret")

library(dplyr)
library(rpart)
library(caret)

mydf <- mutate(mydf, 
       is.good.maven = ifelse(good.maven >= good.pop & good.maven >= good.conn & good.maven >= good.random, 1, 0),
       is.good.conn = ifelse(good.conn >= good.pop & good.conn >= good.maven & good.conn >= good.random, 1, 0),
       is.good.pop = ifelse(good.pop >= good.maven & good.pop >= good.conn & good.pop >= good.random, 1, 0),
       is.bad.maven = ifelse(bad.maven >= bad.pop & bad.maven >= bad.conn & bad.maven >= bad.random, 1, 0),
       is.bad.conn = ifelse(bad.conn >= bad.pop & bad.conn >= bad.maven & bad.conn >= bad.random, 1, 0),
       is.bad.pop = ifelse(bad.pop >= bad.maven & bad.pop >= bad.conn & bad.pop >= bad.random, 1, 0)
       )

trainIndex <- createDataPartition(mydf$good.maven, p=.6, list=FALSE, times = 1)
training <- mydf[trainIndex, ]
testing <- mydf[-trainIndex, ]

good.mavenFit <- train(good.maven ~ rating + review.count + good.review.count + bad.review.count + good.maven.count + bad.maven.count + good.pop.count + bad.pop.count + good.conn.count + bad.conn.count + good.random.count + bad.random.count,
                       data=training,method="rf",
                       trControl=trainControl(method="cv",number=5),
                       prox=TRUE,allowParallel=TRUE)

good.mavenPred <- as.integer(predict(good.mavenFit, newdata=testing))

good.connFit <- train(good.conn ~ rating + review.count + good.review.count + bad.review.count + good.maven.count + bad.maven.count + good.pop.count + bad.pop.count + good.conn.count + bad.conn.count + good.random.count + bad.random.count,
                       data=training,method="rf",
                       trControl=trainControl(method="cv",number=5),
                       prox=TRUE,allowParallel=TRUE)

good.connPred <- as.integer(predict(good.connFit, newdata=testing))

good.popFit <- train(good.pop ~ rating + review.count + good.review.count + bad.review.count + good.maven.count + bad.maven.count + good.pop.count + bad.pop.count + good.conn.count + bad.conn.count + good.random.count + bad.random.count,
                      data=training,method="rf",
                      trControl=trainControl(method="cv",number=5),
                      prox=TRUE,allowParallel=TRUE)

good.popPred <- as.integer(predict(good.popFit, newdata=testing))

good.randomFit <- train(good.random ~ rating + review.count + good.review.count + bad.review.count + good.maven.count + bad.maven.count + good.pop.count + bad.pop.count + good.conn.count + bad.conn.count + good.random.count + bad.random.count,
                     data=training,method="rf",
                     trControl=trainControl(method="cv",number=5),
                     prox=TRUE,allowParallel=TRUE)

good.randomPred <- as.integer(predict(good.randomFit, newdata=testing))





