install.packages("openNLP")
install.packages("openNLPmodels.en")
install.packages("tm")
install.packages("jsonlite")
install.packages("caret")

library(ggplot2) 
library(jsonlite)
library(tm)
library(openNLP)
library(openNLPmodels.en)
library(caret)

business <- stream_in(file("yelp_academic_dataset_business.json"))
checkin <- stream_in(file("yelp_academic_dataset_checkin.json"))
review <- stream_in(file("yelp_academic_dataset_review.json"))
tip <- stream_in(file("yelp_academic_dataset_tip.json"))
user <- stream_in(file("yelp_academic_dataset_user.json"))

number_raw_reviews <- 500
word_list_size <- 25
review_size <- 75

MAVEN <- 1
CONN <- 2
POP <- 3
RANDOM <- 4

ids_from_testing <- function(testing)
{
    names <- testing$name
    names <- names[!duplicated(names)]
    
    df <- data.frame(stringsAsFactors=FALSE)
    for(name in names)
    {
        ids <- business$business_id[business$name == name]    
        business_id <- NA
        for(id in ids) {
            if(is.na(business_id)) {
                business_id <- id
            }
            else if(business$stars[business$business_id == id] >
                        business$stars[business$business_id == business_id])
            {
                business_id <- id
            }
        }
        good_reviews <- review[review$business_id == business_id & review$stars >3, ]
        bad_reviews <- review[review$business_id == business_id & review$stars < 3, ]
        
        df <- rbind(df, data.frame(business_id, nrow(good_reviews), nrow(bad_reviews)))
    }
    colnames(df) <- c("id", "good.count", "bad.count")
    df
}

correction <- function()
{
    names <- training$name
    for(name in names) {
        business_id <- business$business_id[business$name == name & business$review_count >= 500][1]
        target_business <- business[business$business_id == business_id, ]
        target_reviews <- review[review$business_id == business_id, ]
        ids <- splitids(target_business, target_reviews, user)
        popuserids <- ids[[1]]
        conuserids <- ids[[2]]
        infuserids <- ids[[3]]
        randuserids <- ids[[4]]
        rsize <- 10000
        training$good.pop.count[training$name == name] <- good_size(target_reviews, popuserids, target_business, rsize)
        training$good.conn.count[training$name == name] <- good_size(target_reviews, conuserids, target_business, rsize)
        training$good.maven.count[training$name == name] <- good_size(target_reviews, infuserids, target_business, rsize)
        training$good.random.count[training$name == name] <- good_size(target_reviews, randuserids, target_business, rsize)
        
        training$bad.pop.count[training$name == name] <- bad_size(target_reviews, popuserids, target_business, rsize)
        training$bad.conn.count[training$name == name] <- bad_size(target_reviews, conuserids, target_business, rsize)
        training$bad.maven.count[training$name == name] <- bad_size(target_reviews, infuserids, target_business, rsize)
        training$bad.random.count[training$name == name] <- bad_size(target_reviews, randuserids, target_business, rsize)
    }
}

find_ids <- function() {
    print("first subset")
    target_businesses <- business[business$review_count >= 500, ]
    business_ids <- target_businesses$business_id
    ids <- data.frame(stringsAsFactors=FALSE)
    
    print("find business ids")
    for(id in business_ids) {
        good_reviews <- review[review$business_id == id & review$stars >3, ]
        bad_reviews <- review[review$business_id == id & review$stars < 3, ]
        if(nrow(good_reviews) > 150 & nrow(bad_reviews) > 150) {
            ids <- rbind(ids, data.frame(id, nrow(good_reviews), nrow(bad_reviews)))
        }
    }
    colnames(ids) <- c("id", 
                       "good.count",
                       "bad.count")
    
    write.csv(ids, file = "ids.csv",row.names=FALSE)
}

#find_ids()

extractPOS <- function(x, thisPOSregex) {
    x <- as.String(x)
    wordAnnotation <- annotate(x, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
    POSAnnotation <- annotate(x, Maxent_POS_Tag_Annotator(), wordAnnotation)
    POSwords <- subset(POSAnnotation, type == "word")
    tags <- sapply(POSwords$features, '[[', "POS")
    thisPOSindex <- grep(thisPOSregex, tags)
    tokenizedAndTagged <- sprintf("%s/%s", x[POSwords][thisPOSindex], tags[thisPOSindex])
    untokenizedAndTagged <- paste(tokenizedAndTagged, collapse = " ")
    untokenizedAndTagged
}

good <- function(target_reviews, target_business) {
    my_reviews <- target_reviews[target_reviews$stars > 3, ]
    ssize <- ifelse(dim(my_reviews)[1]>=number_raw_reviews, number_raw_reviews, dim(my_reviews)[1])
    review_sample <- my_reviews[sample(nrow(my_reviews), ssize), ]
    review_text <- paste(review_sample$text, collapse=" ")
    nouns_review_text <- gsub("/NN", "", extractPOS(review_text, ".*NN$"))
    review_source <- VectorSource(nouns_review_text)
    corpus <- Corpus(review_source)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    my_stopwords <- unlist(strsplit(tolower(paste(unlist(lapply(target_business$categories,  function (x) gsub("(.*)s$", "\\1", x))), collapse=" ")), " "))
    
    corpus <- tm_map(corpus, removeWords, c(stopwords("english"), "place", "spot", "way", my_stopwords))
    dtm <- DocumentTermMatrix(corpus)
    dtm2 <- as.matrix(dtm)
    frequency <- colSums(dtm2)
    frequency <- sort(frequency, decreasing=TRUE)
    frequency
}

bad <- function(target_reviews, target_business) {
    my_reviews <- target_reviews[target_reviews$stars < 3, ]
    ssize <- ifelse(dim(my_reviews)[1]>=number_raw_reviews,number_raw_reviews,dim(my_reviews)[1])
    review_sample <- my_reviews[sample(nrow(my_reviews), ssize), ]
    review_text <- paste(review_sample$text, collapse=" ")
    nouns_review_text <- gsub("/NN", "", extractPOS(review_text, ".*NN$"))
    review_source <- VectorSource(nouns_review_text)
    corpus <- Corpus(review_source)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    my_stopwords <- unlist(strsplit(tolower(paste(unlist(lapply(target_business$categories,  function (x) gsub("(.*)s$", "\\1", x))), collapse=" ")), " "))
    
    corpus <- tm_map(corpus, removeWords, c(stopwords("english"), "place", "spot", "way", my_stopwords))
    dtm <- DocumentTermMatrix(corpus)
    dtm2 <- as.matrix(dtm)
    frequency <- colSums(dtm2)
    frequency <- sort(frequency, decreasing=TRUE)
    frequency
}

normalize <- function(good, bad) {
    # for(name in names(good)){
    #    if(!(is.na(bad[name]))) {
    #         if(good[name] >= bad[name]) {
    #                good[name] = good[name] - bad[name]
    #                bad[name] = 0
    #            }
    #            else
    #            {
    #                bad[name] = bad[name] - good[name]
    #                good[name] = 0
    #            }
    #        }
    #    }
    good <- sort(good, decreasing=TRUE)
    bad <- sort(bad, decreasing=TRUE)
    list(good, bad)
}

similarity <- function(x, y) {
    count <- 0
    names_x <- names(x)
    count <- sum(sapply(names(y), function(z) ifelse(z %in% names_x, 1, 0)))
    100 * (count / length(y))
}

maven_good <- function(target_reviews, user_ids, target_business, rsize) {
    ssize <- 0
    frequency <- NA
    my_reviews <- data.frame()
    f1 <- function(id) {
        target_reviews$text[target_reviews$stars > 3 & target_reviews$user_id == id]
    }
    tvec <- sapply(user_ids, f1)
    ssize <- ifelse(length(tvec) >= rsize, rsize, length(tvec))
    if(ssize > 0)
    {   
        review_sample <- head(tvec, ssize)
        review_text <- paste(review_sample, collapse=" ")
        nouns_review_text <- gsub("/NN", "", extractPOS(review_text, ".*NN$"))
        review_source <- VectorSource(nouns_review_text)
        corpus <- Corpus(review_source)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, removePunctuation)
        my_stopwords <- unlist(strsplit(tolower(paste(unlist(lapply(target_business$categories,  function (x) gsub("(.*)s$", "\\1", x))), collapse=" ")), " "))
        
        corpus <- tm_map(corpus, removeWords, c(stopwords("english"), "place", "spot", "way", my_stopwords))
        dtm <- DocumentTermMatrix(corpus)
        dtm2 <- as.matrix(dtm)
        frequency <- colSums(dtm2)
        frequency <- sort(frequency, decreasing=TRUE)
    }
    list(frequency, ssize)
}


good_size <- function(target_reviews, user_ids, target_business, rsize) {
    ssize <- 0
    frequency <- NA
    my_reviews <- data.frame()
    f1 <- function(id) {
        target_reviews$text[target_reviews$stars > 3 & target_reviews$user_id == id]
    }
    tvec <- sapply(user_ids, f1)
    ssize <- ifelse(length(tvec) >= rsize, rsize, length(tvec))
    ssize
}

maven_bad <- function(target_reviews, user_ids, target_business, rsize) {
    
    ssize <- 0
    frequency <- NA
    
    f1 <- function(id) {
        target_reviews$text[target_reviews$stars < 3 & target_reviews$user_id == id]
    }
    tvec <- sapply(user_ids, f1)
    ssize <- ifelse(length(tvec) >= rsize, rsize, length(tvec))
    
    if(ssize > 0)
    {
        review_sample <- head(tvec, ssize)
        review_text <- paste(review_sample, collapse=" ")
        strExtract <- extractPOS(review_text, ".*NN$")
        nouns_review_text <- gsub("/NN", "", strExtract)
        review_source <- VectorSource(nouns_review_text)
        corpus <- Corpus(review_source)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, removePunctuation)
        my_stopwords <- unlist(strsplit(tolower(paste(unlist(lapply(target_business$categories,  function (x) gsub("(.*)s$", "\\1", x))), collapse=" ")), " "))
        
        corpus <- tm_map(corpus, removeWords, c(stopwords("english"), "place", "spot", "way", my_stopwords))
        dtm <- DocumentTermMatrix(corpus)
        dtm2 <- as.matrix(dtm)
        frequency <- colSums(dtm2)
        frequency <- sort(frequency, decreasing=TRUE)
    }
    list(frequency, ssize)
}

bad_size <- function(target_reviews, user_ids, target_business, rsize) {
    ssize <- 0
    frequency <- NA
    
    f1 <- function(id) {
        target_reviews$text[target_reviews$stars < 3 & target_reviews$user_id == id]
    }
    tvec <- sapply(user_ids, f1)
    ssize <- ifelse(length(tvec) >= rsize, rsize, length(tvec))
    
    ssize
}

plot < function(good, bad) {
    wf <- data.frame(word=names(good), freq=good)   
    p <- ggplot(subset(wf, freq>10), aes(word, freq))    
    p <- p + geom_bar(stat="identity")   
    p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
    p  
    
    wf <- data.frame(word=names(bad), freq=bad)   
    p <- ggplot(subset(wf, freq>10), aes(word, freq))    
    p <- p + geom_bar(stat="identity")   
    p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
    p   
}

splitids <- function(target_business, reviews, user) {
    target_reviews <- reviews[reviews$business_id == target_business$business_id, ]
    user_ids <- target_reviews$user_id
    target_users <- user[user$user_id %in% user_ids, ]
    popuserids <- data.frame("name"=character(), "value"=integer(), stringsAsFactors=FALSE)
    conuserids <- data.frame("name"=character(), "value"=integer(), stringsAsFactors=FALSE)
    infuserids <- data.frame("name"=character(), "value"=integer(), stringsAsFactors=FALSE)
    randuserids <- data.frame("name"=character(), "value"=integer(), stringsAsFactors=FALSE)
    
    # Loop through users in target reviews
    for(id in user_ids)
    {
        ustar <- max(target_reviews[target_reviews$user_id == id, ]$stars)
        u <- target_users[target_users$user_id == id, ]
        friends <- unlist(u$friends)
        name <- id
        popuserids <- rbind(popuserids, data.frame("name"=name, "value"=length(friends)))
        
        for(fid in friends)
        {
            if(is.element(fid, user_ids))
            {   
                fstar <- max(target_reviews[target_reviews$user_id == fid, ]$stars)
                if(0 == nrow(conuserids[conuserids$name == name, ])) 
                {
                    conuserids <- rbind(conuserids, data.frame("name"=name, "value"=1))
                }
                else
                {
                    conuserids$value[conuserids$name == name] <-
                        conuserids$value[conuserids$name == name] + 1
                }
                # Influential if review scores are equal
                if(ustar == fstar)
                { 
                    if(0 == nrow(infuserids[infuserids$name == name, ])) 
                    {
                        infuserids <- rbind(infuserids, data.frame("name"=name, "value"=1))
                    }
                    else
                    {
                        infuserids$value[infuserids$name == name] <-
                            infuserids$value[infuserids$name == name] + 1
                    }
                }
            }
        }
    }
    popuserids <- popuserids[order(popuserids$value, decreasing=TRUE), ]
    conuserids <- conuserids[order(conuserids$value, decreasing=TRUE), ]
    infuserids <- infuserids[order(infuserids$value, decreasing=TRUE), ]
    randuserids <- user_ids[sample(length(user_ids), length(user_ids))]
    
    list(as.vector(popuserids$name),
         as.vector(conuserids$name),
         as.vector(infuserids$name),
         randuserids)
}


profile <- function(target_business, reviews, user) {
    target_reviews <- reviews[reviews$business_id == target_business$business_id, ]
    user_ids <- target_reviews$user_id
    target_users <- user[user$user_id %in% user_ids, ]
    popuserids <- vector(mode="character")
    conuserids <- vector(mode="character")
    infuserids <- vector(mode="character")
    randuserids <- vector(mode="character")
    maven_good_count <- 0
    maven_bad_count<- 0
    conuser_good_count <- 0
    conuser_bad_count<- 0
    popuser_good_count <- 0
    popuser_bad_count<- 0
    randuser_good_count <- 0
    randuser_bad_count<- 0
    retvec <- vector(mode="numeric")
    
    print("GOOD")
    good <- good(target_reviews, target_business)
    print("BAD")
    bad <- bad(target_reviews, target_business)
    retval <- normalize(good, bad)
    good <- retval[[1]]
    bad <- retval[[2]]
    
    print("SPLITIDS")
    ids <- splitids(target_business, reviews, user)
    popuserids <- ids[[1]]
    conuserids <- ids[[2]]
    infuserids <- ids[[3]]
    randuserids <- ids[[4]]
    
    print("MAVEN GOOD")
    maven_good <- maven_good(target_reviews, infuserids, target_business, review_size)
    maven_good_count <- maven_good[[2]]
    maven_good <- maven_good[[1]]
    
    print("CONN GOOD")
    conuser_good <- maven_good(target_reviews, conuserids, target_business, review_size)
    conuser_good_count <- conuser_good[[2]]
    conuser_good <- conuser_good[[1]]
    
    print("POP GOOD")
    popuser_good <- maven_good(target_reviews, popuserids, target_business, review_size)
    popuser_good_count <- popuser_good[[2]]
    popuser_good <- popuser_good[[1]]
    
    print("RAND GOOD")
    randuser_good <- maven_good(target_reviews, randuserids, target_business, review_size)
    randuser_good_count <- randuser_good[[2]]
    randuser_good <- randuser_good[[1]]
    
    print("MAVEN BAD")
    maven_bad <- maven_bad(target_reviews, infuserids, target_business, review_size)
    maven_bad_count <- maven_bad[[2]]
    maven_bad <- maven_bad[[1]]
    retval <- normalize(maven_good, maven_bad)
    maven_good <- retval[[1]]
    maven_bad <- retval[[2]]
    
    print("CONN BAD")
    conuser_bad <- maven_bad(target_reviews, conuserids, target_business, review_size)
    conuser_bad_count <- conuser_bad[[2]]
    conuser_bad <- conuser_bad[[1]]
    retval <- normalize(conuser_good, conuser_bad)
    conuser_good <- retval[[1]]
    conuser_bad <- retval[[2]]
    
    print("POP BAD")
    popuser_bad <- maven_bad(target_reviews, popuserids, target_business, review_size)
    popuser_bad_count <- popuser_bad[[2]]
    popuser_bad <- popuser_bad[[1]]
    retval <- normalize(popuser_good, popuser_bad)
    popuser_good <- retval[[1]]
    popuser_bad <- retval[[2]]
    
    print("RAND BAD")
    randuser_bad <- maven_bad(target_reviews, randuserids, target_business, review_size)
    randuser_bad_count <- randuser_bad[[2]]
    randuser_bad <- randuser_bad[[1]]
    retval <- normalize(randuser_good, randuser_bad)
    randuser_good <- retval[[1]]
    randuser_bad <- retval[[2]]
    
    good <- head(good, word_list_size)
    bad <- head(bad, word_list_size)
    maven_good <- head(maven_good, word_list_size)
    maven_bad <- head(maven_bad, word_list_size)
    conuser_good <- head(conuser_good, word_list_size)
    conuser_bad <- head(conuser_bad, word_list_size)
    popuser_good <- head(popuser_good, word_list_size)
    popuser_bad <- head(popuser_bad, word_list_size)
    randuser_good <- head(randuser_good, word_list_size)
    randuser_bad <- head(randuser_bad, word_list_size)
    
    retvec["good.maven"] <- similarity(good, maven_good)    
    retvec["good.pop"] <- similarity(good, popuser_good)
    retvec["good.conn"] <- similarity(good, conuser_good)
    retvec["good.random"] <- similarity(good, randuser_good)
    retvec["bad.maven"] <- similarity(bad, maven_bad)
    retvec["bad.pop"] <- similarity(bad, popuser_bad)
    retvec["bad.conn"] <- similarity(bad, conuser_bad)
    retvec["bad.random"] <- similarity(bad, randuser_bad)
    retvec["good.maven.count"] <- maven_good_count    
    retvec["good.pop.count"] <- popuser_good_count
    retvec["good.conn.count"] <- conuser_good_count
    retvec["good.random.count"] <- randuser_good_count
    retvec["bad.maven.count"] <- maven_bad_count
    retvec["bad.pop.count"] <- popuser_bad_count
    retvec["bad.conn.count"] <- conuser_bad_count
    retvec["bad.random.count"] <- randuser_bad_count
    
    print("DONE")
    retvec
}


profile_pred <- function(target_business, reviews, user, modelList) {
    target_reviews <- reviews[reviews$business_id == target_business$business_id, ]
    user_ids <- target_reviews$user_id
    target_users <- user[user$user_id %in% user_ids, ]
    popuserids <- vector(mode="character")
    conuserids <- vector(mode="character")
    infuserids <- vector(mode="character")
    randuserids <- vector(mode="character")
    maven_good_count <- 0
    maven_bad_count<- 0
    conuser_good_count <- 0
    conuser_bad_count<- 0
    popuser_good_count <- 0
    popuser_bad_count<- 0
    randuser_good_count <- 0
    randuser_bad_count<- 0
    good.method <- 0
    bad.method <- 0
    good.pred <- 0
    bad.pred <- 0
    retvec <- vector(mode="numeric")
    
    print("GOOD")
    Rprof("good_full.prof")
    good <- good(target_reviews, target_business)
    Rprof(NULL)
    print("BAD")
    Rprof("bad_full.prof")
    bad <- bad(target_reviews, target_business)
    Rprof(NULL)
    retval <- normalize(good, bad)
    good <- retval[[1]]
    bad <- retval[[2]]
    
    print("SPLITIDS")
    ids <- splitids(target_business, reviews, user)
    popuserids <- ids[[1]]
    conuserids <- ids[[2]]
    infuserids <- ids[[3]]
    randuserids <- ids[[4]]
    
    good.review.count <- good_size(target_reviews, user_ids, target_business, number_raw_reviews)
    bad.review.count <- bad_size(target_reviews, user_ids, target_business, number_raw_reviews)  
    review.count <- good.review.count + bad.review.count
    
    good.maven.count <- good_size(target_reviews, infuserids, target_business, 10000)
    bad.maven.count <- bad_size(target_reviews, infuserids, target_business, 10000)  
    
    good.conn.count <- good_size(target_reviews, conuserids, target_business, 10000)
    bad.conn.count <- bad_size(target_reviews, conuserids, target_business, 10000)
    
    good.pop.count <- good_size(target_reviews, popuserids, target_business, 10000)
    bad.pop.count <- bad_size(target_reviews, popuserids, target_business, 10000)
    
    good.random.count <- good_size(target_reviews, randuserids, target_business, 10000)
    bad.random.count <- bad_size(target_reviews, randuserids, target_business, 10000)
    
    predictions <- predict_scores(modelList, target_business$stars, review.count, good.review.count, bad.review.count, good.maven.count, bad.maven.count, good.pop.count, bad.pop.count, good.conn.count, bad.conn.count, good.random.count, bad.random.count)
    
    good.pop.pred <- predictions[[1]]
    good.conn.pred <- predictions[[2]]
    good.maven.pred <- predictions[[3]]
    good.rand.pred <- predictions[[4]]
    bad.pop.pred <- predictions[[5]]
    bad.conn.pred <- predictions[[6]]
    bad.maven.pred <- predictions[[7]]
    bad.rand.pred <- predictions[[8]]
    
    print(sprintf("good.maven.pred=%d, good.conn.pred=%d, good.pop.pred=%d, good.rand.pred=%d", good.maven.pred, good.conn.pred, good.pop.pred, good.rand.pred))
    
    Rprof("good_sample.prof")
    if(good.maven.pred >= good.pop.pred &
           good.maven.pred >= good.conn.pred &
           good.maven.pred >= good.rand.pred)
    {
        #print("MAVEN GOOD")
        good.method <- MAVEN
        good.pred <- good.maven.pred 
        pred_good <- maven_good(target_reviews, infuserids, target_business, review_size)
        pred_good_count <- pred_good[[2]]
        pred_good <- pred_good[[1]]
    }
    else if (good.conn.pred >= good.pop.pred &
                 good.conn.pred >= good.maven.pred &
                 good.conn.pred >= good.rand.pred)
    {
        #print("CONN GOOD")
        good.method <- CONN
        good.pred <- good.conn.pred 
        pred_good <- maven_good(target_reviews, conuserids, target_business, review_size)
        pred_good_count <- pred_good[[2]]
        pred_good <- pred_good[[1]]
    }
    else if(good.pop.pred >= good.conn.pred &
                good.pop.pred >= good.maven.pred &
                good.pop.pred >= good.rand.pred)
    {
        #print("POP GOOD")
        good.method <- POP
        good.pred <- good.pop.pred 
        pred_good <- maven_good(target_reviews, popuserids, target_business, review_size)
        pred_good_count <- pred_good[[2]]
        pred_good <- pred_good[[1]]
    }
    else
    {
        #print("RAND GOOD")
        good.method <- RANDOM
        good.pred <- good.rand.pred 
        pred_good <- maven_good(target_reviews, randuserids, target_business, review_size)
        pred_good_count <- pred_good[[2]]
        pred_good <- pred_good[[1]]
    }
    Rprof(NULL)
    Rprof("bad_sample.prof")
    if(bad.maven.pred >= bad.pop.pred &
           bad.maven.pred >= bad.conn.pred &
           bad.maven.pred >= bad.rand.pred)
    {
        #print("MAVEN BAD")
        bad.method <- MAVEN
        bad.pred <- bad.maven.pred 
        pred_bad <- maven_bad(target_reviews, infuserids, target_business, review_size)
        pred_bad_count <- pred_bad[[2]]
        pred_bad <- pred_bad[[1]]
        retval <- normalize(pred_good, pred_bad)
        pred_good <- retval[[1]]
        pred_bad <- retval[[2]]
    }
    else if(bad.conn.pred >= bad.pop.pred &
                bad.conn.pred >= bad.maven.pred &
                bad.conn.pred >= bad.rand.pred)
    {
        #print("CONN BAD")
        bad.method <- CONN
        bad.pred <- bad.conn.pred
        
        pred_bad <- maven_bad(target_reviews, conuserids, target_business, review_size)
        pred_bad_count <- pred_bad[[2]]
        pred_bad <- pred_bad[[1]]
        retval <- normalize(pred_good, pred_bad)
        pred_good <- retval[[1]]
        pred_bad <- retval[[2]]
    }
    else if(bad.pop.pred >= bad.conn.pred &
                bad.pop.pred >= bad.maven.pred &
                bad.pop.pred >= bad.rand.pred)
    {
        #print("POP BAD")
        bad.method <- POP
        bad.pred <- bad.pop.pred
        
        pred_bad <- maven_bad(target_reviews, popuserids, target_business, review_size)
        pred_bad_count <- pred_bad[[2]]
        pred_bad <- pred_bad[[1]]
        retval <- normalize(pred_good, pred_bad)
        pred_good <- retval[[1]]
        pred_bad <- retval[[2]]
    }
    else 
    {
        #print("RAND BAD")
        bad.method <- RANDOM
        bad.pred <- bad.rand.pred
        
        pred_bad <- maven_bad(target_reviews, randuserids, target_business, review_size)
        pred_bad_count <- pred_bad[[2]]
        pred_bad <- pred_bad[[1]]
        retval <- normalize(pred_good, pred_bad)
        pred_good <- retval[[1]]
        pred_bad <- retval[[2]]
    }
    Rprof(NULL)
    good <- head(good, word_list_size)
    bad <- head(bad, word_list_size)
    pred_good <- head(pred_good, word_list_size)
    pred_bad <- head(pred_bad, word_list_size)
    
    retvec["good.sim"] <- similarity(good, pred_good)    
    retvec["bad.sim"] <- similarity(bad, pred_bad)
    retvec["good.sim.count"] <- pred_good_count 
    retvec["bad.sim.count"] <- pred_bad_count 
    retvec["good.method"] <- good.method
    retvec["good.pred"] <- good.pred 
    retvec["bad.method"] <- bad.method
    retvec["bad.pred"] <- bad.pred 
    
    print("DONE")
    retvec
}

start <- function(iterations) {
    print("first subset")
    target_businesses <- business[business$review_count >= 500, ]
    business_ids <- target_businesses$business_id
    ids <- read.csv("ids.csv")
    busids <- as.vector(ids$id)
    busids <- busids[sample(length(busids), length(busids))]
    
    print("initialize dataframe")
    df <- data.frame(stringsAsFactors=FALSE)
    
    simscores <- vector(mode="numeric")
    iterations <- ifelse(iterations <= length(busids), iterations, length(busids))
    
    print(sprintf("iterations=%d", iterations))
    for (i in 1:iterations) {
        id <- busids[i]
        target_business <- target_businesses[target_businesses$business_id == id, ]
        simscores <- profile(target_business, review, user)      
        df <- rbind(df, data.frame(target_business$name, 
                                   target_business$stars,  
                                   simscores["good.maven"],
                                   simscores["good.pop"],
                                   simscores["good.conn"],
                                   simscores["good.random"],
                                   simscores["bad.maven"],
                                   simscores["bad.pop"],
                                   simscores["bad.conn"],
                                   simscores["bad.random"],
                                   target_business$review_count,
                                   ids$good.count[ids$id == id],
                                   ids$bad.count[ids$id == id],
                                   simscores["good.maven.count"],
                                   simscores["good.pop.count"],
                                   simscores["good.conn.count"],
                                   simscores["good.random.count"],
                                   simscores["bad.maven.count"],
                                   simscores["bad.pop.count"],
                                   simscores["bad.conn.count"],
                                   simscores["bad.random.count"]))
        
        print(sprintf("#%d %s,rating=%g, count=%d, good count=%d, bad count=%d", i, target_business$name, target_business$stars, target_business$review_count, ids$good.count[ids$id == id], ids$bad.count[ids$id == id]))
        print("=>")
        print(sprintf("\tgood.maven=%g, bad.maven=%g, good.maven.count=%d, bad.maven.count=%d", simscores["good.maven"], simscores["bad.maven"], simscores["good.maven.count"], simscores["bad.maven.count"]))
        print(sprintf("\tgood.conn=%g, bad.conn=%g, good.conn.count=%d, bad.conn.count=%d", simscores["good.conn"], simscores["bad.conn"], simscores["good.conn.count"], simscores["bad.conn.count"]))
        print(sprintf("\tgood.pop=%g, bad.pop=%g, good.pop.count=%d, bad.pop.count=%d", simscores["good.pop"], simscores["bad.pop"], simscores["good.pop.count"], simscores["bad.pop.count"]))
        print(sprintf("\tgood.rand=%g, bad.rand=%g, good.rand.count=%d, bad.rand.count=%d", simscores["good.random"], simscores["bad.random"], simscores["good.random.count"], simscores["bad.random.count"]))
        print("<=")
        
        mydf <- df
        colnames(mydf) <- c("name", 
                            "rating",
                            "good.maven",
                            "good.pop",
                            "good.conn", 
                            "good.random",
                            "bad.maven",
                            "bad.pop",
                            "bad.conn",
                            "bad.random",
                            "review.count",
                            "good.review.count",
                            "bad.review.count",
                            "good.maven.count",
                            "good.pop.count",
                            "good.conn.count",
                            "good.random.count",
                            "bad.maven.count",
                            "bad.pop.count",
                            "bad.conn.count",
                            "bad.random.count")
        write.csv(mydf, file = "results.csv",row.names=FALSE)
        
    }
    
    colnames(df) <- c("name", 
                      "rating",
                      "good.maven",
                      "good.pop",
                      "good.conn",
                      "good.random",
                      "bad.maven",
                      "bad.pop",
                      "bad.conn",
                      "bad.random",
                      "review.count",
                      "good.review.count",
                      "bad.review.count",
                      "good.maven.count",
                      "good.pop.count",
                      "good.conn.count",
                      "good.random.count",
                      "bad.maven.count",
                      "bad.pop.count",
                      "bad.conn.count",
                      "bad.random.count")
    write.csv(df, file = "results.csv",row.names=FALSE)
    df
}

start_pred <- function(iterations) {
    modelList <- create_models()
    
    print("first subset")
    target_businesses <- business[business$review_count >= 500, ]
    business_ids <- target_businesses$business_id
    ids <- read.csv("ids.csv")
    busids <- as.vector(ids$id)
    busids <- busids[sample(length(busids), iterations)]
    
    print("initialize dataframe")
    df <- data.frame(stringsAsFactors=FALSE)
    
    simscores <- vector(mode="numeric")
    iterations <- ifelse(iterations <= length(busids), iterations, length(busids))
    
    print(sprintf("iterations=%d", iterations))
    for (i in 1:iterations) {
        id <- busids[i]
        target_business <- target_businesses[target_businesses$business_id == id, ]
        simscores <- profile_pred(target_business, review, user, modelList)      
        df <- rbind(df, data.frame(target_business$name, 
                                   target_business$stars,  
                                   simscores["good.sim"],
                                   simscores["bad.sim"],
                                   target_business$review_count,
                                   ids$good.count[ids$id == id],
                                   ids$bad.count[ids$id == id],
                                   simscores["good.sim.count"],
                                   simscores["bad.sim.count"],
                                   simscores["good.method"],
                                   simscores["good.pred"],
                                   simscores["bad.method"],
                                   simscores["bad.pred"]))
        
        print(sprintf("#%d %s,rating=%g, count=%d, good count=%d, bad count=%d", i, target_business$name, target_business$stars, target_business$review_count, ids$good.count[ids$id == id], ids$bad.count[ids$id == id]))
        print("=>")
        print(sprintf("\tgood.sim=%g, bad.sim=%g, good.sim.count=%d, bad.sim.count=%d, good.method=%d, good.pred=%d, bad.method=%d, bad.pred=%d", simscores["good.sim"], simscores["bad.sim"], simscores["good.sim.count"], simscores["bad.sim.count"], simscores["good.method"], simscores["good.pred"], simscores["bad.method"], simscores["bad.pred"]))
        print("<=")
        
        mydf <- df
        colnames(mydf) <- c("name", 
                            "rating",
                            "good.sim",
                            "bad.sim",
                            "review.count",
                            "good.review.count",
                            "bad.review.count",
                            "good.sim.count",
                            "bad.sim.count",
                            "good.method",
                            "good.pred",
                            "bad.method",
                            "bad.pred")
        write.csv(mydf, file = "pred.csv",row.names=FALSE)
        
    }
    
    colnames(df) <- c("name", 
                      "rating",
                      "good.sim",
                      "bad.sim",
                      "review.count",
                      "good.review.count",
                      "bad.review.count",
                      "good.sim.count",
                      "bad.sim.count",
                      "good.method",
                      "good.pred",
                      "bad.method",
                      "bad.pred")
    write.csv(df, file = "pred.csv",row.names=FALSE)
    df
}


create_models <- function() {
    good.mavenFit <- NA
    good.connFit <- NA
    good.popFit <- NA
    good.randomFit <- NA
    bad.mavenFit <- NA
    bad.connFit <- NA
    bad.popFit <- NA
    bad.randomFit <- NA
    
    training <- read.csv("training.csv", stringsAsFactors=FALSE)
    good.mavenFit <- train(good.maven ~ rating + review.count + good.review.count + good.maven.count + good.conn.count,
                           data=training,method="rf",
                           trControl=trainControl(method="cv",number=5),
                           prox=TRUE,allowParallel=TRUE)
    
    good.connFit <- train(good.conn ~ rating + review.count + good.review.count + good.maven.count + good.conn.count,
                          data=training,method="rf",
                          trControl=trainControl(method="cv",number=5),
                          prox=TRUE,allowParallel=TRUE)
    
    good.popFit <- train(good.pop ~ rating + review.count + good.review.count + good.pop.count ,
                         data=training,method="rf",
                         trControl=trainControl(method="cv",number=5),
                         prox=TRUE,allowParallel=TRUE)
    
    
    good.randomFit <- train(good.random ~ rating + review.count + good.review.count + good.random.count,
                            data=training,method="rf",
                            trControl=trainControl(method="cv",number=5),
                            prox=TRUE,allowParallel=TRUE)
    
    bad.mavenFit <- train(bad.maven ~ rating + review.count + bad.review.count + bad.maven.count + bad.conn.count,
                          data=training,method="rf",
                          trControl=trainControl(method="cv",number=5),
                          prox=TRUE,allowParallel=TRUE)
    
    bad.connFit <- train(bad.conn ~ rating + review.count + bad.review.count + bad.maven.count + bad.conn.count,
                         data=training,method="rf",
                         trControl=trainControl(method="cv",number=5),
                         prox=TRUE,allowParallel=TRUE)
    
    bad.popFit <- train(bad.pop ~ rating + review.count + bad.review.count + bad.pop.count,
                        data=training,method="rf",
                        trControl=trainControl(method="cv",number=5),
                        prox=TRUE,allowParallel=TRUE)
    
    
    bad.randomFit <- train(bad.random ~ rating + review.count + bad.review.count + bad.random.count,
                           data=training,method="rf",
                           trControl=trainControl(method="cv",number=5),
                           prox=TRUE,allowParallel=TRUE)
    
    
    list(good.mavenFit,
         good.connFit,
         good.popFit,
         good.randomFit,
         bad.mavenFit,
         bad.connFit,
         bad.popFit,
         bad.randomFit)
}

predict_scores <- function(modelList, rating, review.count, good.review.count, bad.review.count, good.maven.count, bad.maven.count, good.pop.count, bad.pop.count, good.conn.count, bad.conn.count, good.random.count, bad.random.count) {
    
    good.mavenFit <- modelList[[1]]
    good.connFit <- modelList[[2]]
    good.popFit <- modelList[[3]]
    good.randomFit <- modelList[[4]]
    bad.mavenFit <- modelList[[5]]
    bad.connFit <- modelList[[6]]
    bad.popFit <- modelList[[7]]
    bad.randomFit <- modelList[[8]]
    
    testing <- data.frame(rating, review.count, good.review.count, bad.review.count, good.maven.count, good.pop.count, good.conn.count, good.random.count, bad.maven.count, bad.pop.count, bad.conn.count, bad.random.count)
    colnames(testing) <- c( "rating",
                            "review.count",
                            "good.review.count",
                            "bad.review.count",
                            "good.maven.count",
                            "good.pop.count",
                            "good.conn.count",
                            "good.random.count",
                            "bad.maven.count",
                            "bad.pop.count",
                            "bad.conn.count",
                            "bad.random.count")
    good.mavenPred <- as.integer(predict(good.mavenFit, newdata=testing))
    
    good.connPred <- as.integer(predict(good.connFit, newdata=testing))
    
    good.popPred <- as.integer(predict(good.popFit, newdata=testing))
    
    good.randomPred <- as.integer(predict(good.randomFit, newdata=testing))
    
    bad.mavenPred <- as.integer(predict(bad.mavenFit, newdata=testing))
    
    bad.connPred <- as.integer(predict(bad.connFit, newdata=testing))
    
    bad.popPred <- as.integer(predict(bad.popFit, newdata=testing))
    
    bad.randomPred <- as.integer(predict(bad.randomFit, newdata=testing))
    
    list(good.popPred,
         good.connPred,
         good.mavenPred,
         good.randomPred,
         bad.popPred,
         bad.connPred,
         bad.mavenPred,
         bad.randomPred)
}


mydf <- start_pred(25)

