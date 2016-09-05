library(sentiment)
library(Rstem)
library(ggplot2)
library(twitteR)
library(rjson)
library(tm)
library(SnowballC)
library(streamR)
library(plyr)
library(MASS)

shinyServer(
  function(input, output, session){
    
    accessToken <- "3305908472-DRhbTrXosw6ThTTrjwgyE8NXUE9army4DLDMxNU"
    accessTokenSecret <- "I1w24Ug6TBfAW2Yh2o6Ih5fpskfGqyMSA3WuV9wrYsBhe"
    apiKey <- "oDWe7MhxIF7WLi5t9bzrXXN97"
    apiKeySecret <- "MwIAQx2EqMDtcDb6M9GwQt26VtIgwax5BmR3rEuMAaYfpdaXaZ"
    
    setup_twitter_oauth(apiKey,apiKeySecret,accessToken,accessTokenSecret)
    token <- get("oauth_token", twitteR:::oauth_cache)
    token$cache()
    
    observeEvent(input$do, {
      liveTweets <- searchTwitter(searchString = "Obama+Trump+Ted Cruz+Marco+Donald+Barack",n = 1000)
      liveTweets <- sapply(liveTweets, function(x) x$getText())
      
      #cleaning the live tweets
      liveTweets <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", liveTweets)
      liveTweets <- gsub("@\\w+", "", liveTweets)
      liveTweets <- gsub("[[:punct:]]", "", liveTweets)
      liveTweets <- gsub("[[:digit:]]", "", liveTweets)
      liveTweets <- gsub("http\\w+", "", liveTweets)
      liveTweets <- gsub("[ \t]{2,}", "", liveTweets)
      liveTweets <- gsub("^\\s+|\\s+$", "", liveTweets)
      liveTweets <- tolower(liveTweets)
      liveTweets <- liveTweets[!is.na(liveTweets)]
      names(liveTweets) <- NULL
      
      #calculating polarity of live tweets
      tweetPolarityLive <- classify_polarity(textColumns = as.data.frame(liveTweets),algorithm = "bayes")
      finalPolaritytweetsLive <- tweetPolarityLive[,4]
      finalTweetPolarityDFLive <- data.frame(text = liveTweets,Polarity = finalPolaritytweetsLive)
      finalTweetPolarityDFObamaLive <- finalTweetPolarityDFLive[which(grepl("obama",finalTweetPolarityDFLive$text)),]
      finalTweetPolarityDFTrumpLive <- finalTweetPolarityDFLive[which(grepl("trump",finalTweetPolarityDFLive$text)),]
      finalTweetPolarityDFBushLive <- finalTweetPolarityDFLive[which(grepl("bush",finalTweetPolarityDFLive$text)),]
      finalTweetPolarityDFMarcoLive <- finalTweetPolarityDFLive[which(grepl("marco",finalTweetPolarityDFLive$text)),]
      finalTweetPolarityDFTedLive <- finalTweetPolarityDFLive[which(grepl("ted",finalTweetPolarityDFLive$text)),]
      
      output$sentimentA <- renderPlot({
        if (input$Candidates == "All") {
          ggplot(finalTweetPolarityDFLive, aes(x=Polarity)) + geom_bar(aes(y=..count.., fill=Polarity)) + labs("Polarities","Number of tweets")
        }
        else if (input$Candidates == "Barack Obama") {
          ggplot(finalTweetPolarityDFObamaLive, aes(x=Polarity)) + geom_bar(aes(y=..count.., fill=Polarity)) + labs("Polarities","Number of tweets")
        }
        
        else if (input$Candidates == "Donald Trump") {
          ggplot(finalTweetPolarityDFTrumpLive, aes(x=Polarity)) + geom_bar(aes(y=..count.., fill=Polarity)) + labs("Polarities","Number of tweets")
        }
        else if (input$Candidates == "George W. Bush") {
          ggplot(finalTweetPolarityDFBushLive, aes(x=Polarity)) + geom_bar(aes(y=..count.., fill=Polarity)) + labs("Polarities","Number of tweets")
        }
        else if (input$Candidates == "Marco Rubio") {
          ggplot(finalTweetPolarityDFMarcoLive, aes(x=Polarity)) + geom_bar(aes(y=..count.., fill=Polarity)) + labs("Polarities","Number of tweets")
        }
        else if (input$Candidates == "Ted Cruz") {
          ggplot(finalTweetPolarityDFTedLive, aes(x=Polarity)) + geom_bar(aes(y=..count.., fill=Polarity)) + labs("Polarities","Number of tweets")
        }
        
        
      })      
      
    })
    
    
    tweetFiles <- list.files(path = "data/",pattern = "*.json")
    setwd("data")
    dailyTweet <- lapply(tweetFiles, parseTweets)
    setwd("..")
    finalTweetDF <- do.call(rbind, dailyTweet)
    finalTweetsText <- as.list(finalTweetDF$text)
    
    liveTweets <- searchTwitter(searchString = "Obama+Trump+Ted Cruz+Marco",n = 1000)
    liveTweets <- sapply(liveTweets, function(x) x$getText())
    
    #cleaning the stored tweets
    finalTweetsText <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", finalTweetsText)
    finalTweetsText <- gsub("@\\w+", "", finalTweetsText)
    finalTweetsText <- gsub("[[:punct:]]", "", finalTweetsText)
    finalTweetsText <- gsub("[[:digit:]]", "", finalTweetsText)
    finalTweetsText <- gsub("http\\w+", "", finalTweetsText)
    finalTweetsText <- gsub("[ \t]{2,}", "", finalTweetsText)
    finalTweetsText <- gsub("^\\s+|\\s+$", "", finalTweetsText)
    finalTweetsText <- tolower(finalTweetsText)
    finalTweetsText <- finalTweetsText[!is.na(finalTweetsText)]
    names(finalTweetsText) <- NULL
    
    #calculate polarity of the text present as 1:Positive, 2:Negative 3: Neutral
    tweetPolarity <- classify_polarity(textColumns = as.data.frame(finalTweetsText),algorithm = "bayes")
    finalPolaritytweets <- tweetPolarity[,4]
    finalTweetPolarityDF <- data.frame(text = finalTweetsText,Polarity = finalPolaritytweets)
    finalTweetPolarityDFObama <- finalTweetPolarityDF[which(grepl("obama",finalTweetPolarityDF$text)),]
    finalTweetPolarityDFTrump <- finalTweetPolarityDF[which(grepl("trump",finalTweetPolarityDF$text)),]
    finalTweetPolarityDFBush <- finalTweetPolarityDF[which(grepl("bush",finalTweetPolarityDF$text)),]
    finalTweetPolarityDFMarco <- finalTweetPolarityDF[which(grepl("marco",finalTweetPolarityDF$text)),]
    finalTweetPolarityDFTed <- finalTweetPolarityDF[which(grepl("ted",finalTweetPolarityDF$text)),]
    
    obamaRatio <- sum(finalTweetPolarityDFObama$Polarity=="positive")/sum(finalTweetPolarityDFObama$Polarity=="negative")
    trumpRatio <- sum(finalTweetPolarityDFTrump$Polarity=="positive")/sum(finalTweetPolarityDFTrump$Polarity=="negative")
    bushRatio <- sum(finalTweetPolarityDFBush$Polarity=="positive")/sum(finalTweetPolarityDFBush$Polarity=="negative")
    marcoRatio <- sum(finalTweetPolarityDFMarco$Polarity=="positive")/sum(finalTweetPolarityDFMarco$Polarity=="negative")
    tedRatio <- sum(finalTweetPolarityDFTed$Polarity=="positive")/sum(finalTweetPolarityDFTed$Polarity=="negative")
    
    ratios <- c(obamaRatio,trumpRatio,bushRatio,marcoRatio,tedRatio)
    
    #cleaning the live tweets
    liveTweets <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", liveTweets)
    liveTweets <- gsub("@\\w+", "", liveTweets)
    liveTweets <- gsub("[[:punct:]]", "", liveTweets)
    liveTweets <- gsub("[[:digit:]]", "", liveTweets)
    liveTweets <- gsub("http\\w+", "", liveTweets)
    liveTweets <- gsub("[ \t]{2,}", "", liveTweets)
    liveTweets <- gsub("^\\s+|\\s+$", "", liveTweets)
    liveTweets <- tolower(liveTweets)
    liveTweets <- liveTweets[!is.na(liveTweets)]
    names(liveTweets) <- NULL
    
    #calculating polarity of live tweets
    tweetPolarityLive <- classify_polarity(textColumns = as.data.frame(liveTweets),algorithm = "bayes")
    finalPolaritytweetsLive <- tweetPolarityLive[,4]
    finalTweetPolarityDFLive <- data.frame(text = liveTweets,Polarity = finalPolaritytweetsLive)
    finalTweetPolarityDFObamaLive <- finalTweetPolarityDFLive[which(grepl("obama",finalTweetPolarityDFLive$text)),]
    finalTweetPolarityDFTrumpLive <- finalTweetPolarityDFLive[which(grepl("trump",finalTweetPolarityDFLive$text)),]
    finalTweetPolarityDFBushLive <- finalTweetPolarityDFLive[which(grepl("bush",finalTweetPolarityDFLive$text)),]
    finalTweetPolarityDFMarcoLive <- finalTweetPolarityDFLive[which(grepl("marco",finalTweetPolarityDFLive$text)),]
    finalTweetPolarityDFTedLive <- finalTweetPolarityDFLive[which(grepl("ted",finalTweetPolarityDFLive$text)),]
    
        output$sentiment <- renderPlot({
      if (input$Candidates == "All") {
        ggplot(finalTweetPolarityDF, aes(x=Polarity)) + geom_bar(aes(y=..count.., fill=Polarity)) + labs("Polarities","Number of tweets")
      }
      else if (input$Candidates == "Barack Obama") {
        ggplot(finalTweetPolarityDFObama, aes(x=Polarity)) + geom_bar(aes(y=..count.., fill=Polarity)) + labs("Polarities","Number of tweets")
      }
      
      else if (input$Candidates == "Donald Trump") {
        ggplot(finalTweetPolarityDFTrump, aes(x=Polarity)) + geom_bar(aes(y=..count.., fill=Polarity)) + labs("Polarities","Number of tweets")
      }
      else if (input$Candidates == "George W. Bush") {
        ggplot(finalTweetPolarityDFBush, aes(x=Polarity)) + geom_bar(aes(y=..count.., fill=Polarity)) + labs("Polarities","Number of tweets")
      }
      else if (input$Candidates == "Marco Rubio") {
        ggplot(finalTweetPolarityDFMarco, aes(x=Polarity)) + geom_bar(aes(y=..count.., fill=Polarity)) + labs("Polarities","Number of tweets")
      }
      else if (input$Candidates == "Ted Cruz") {
        ggplot(finalTweetPolarityDFTed, aes(x=Polarity)) + geom_bar(aes(y=..count.., fill=Polarity)) + labs("Polarities","Number of tweets")
      }
        
      
    })
    
      output$summary <- renderPrint({
        if (input$Candidates == "All") {
          summary(finalTweetPolarityDF)
        }
        else if (input$Candidates == "Barack Obama") {
          summary(finalTweetPolarityDFObama)
        }
        
        else if (input$Candidates == "Donald Trump") {
          summary(finalTweetPolarityDFTrump)
        }
        else if (input$Candidates == "George W. Bush") {
          summary(finalTweetPolarityDFBush)
        }
        else if (input$Candidates == "Marco Rubio") {
          summary(finalTweetPolarityDFMarco)
        }
        else if (input$Candidates == "Ted Cruz") {
          summary(finalTweetPolarityDFTed)
        }
      })
    
      output$pie <- renderPlot({
        pie(x = ratios,labels = c("Barack Obama","Donald Trump","George Bush","Marco Rubio","Ted Cruz"),main = "Popularity based on Polarity of tweets")
      })
      
      output$sentimentA <- renderPlot({
        if (input$Candidates == "All") {
          ggplot(finalTweetPolarityDFLive, aes(x=Polarity)) + geom_bar(aes(y=..count.., fill=Polarity)) + labs("Polarities","Number of tweets")
        }
        else if (input$Candidates == "Barack Obama") {
          ggplot(finalTweetPolarityDFObamaLive, aes(x=Polarity)) + geom_bar(aes(y=..count.., fill=Polarity)) + labs("Polarities","Number of tweets")
        }
        
        else if (input$Candidates == "Donald Trump") {
          ggplot(finalTweetPolarityDFTrumpLive, aes(x=Polarity)) + geom_bar(aes(y=..count.., fill=Polarity)) + labs("Polarities","Number of tweets")
        }
        else if (input$Candidates == "George W. Bush") {
          ggplot(finalTweetPolarityDFBushLive, aes(x=Polarity)) + geom_bar(aes(y=..count.., fill=Polarity)) + labs("Polarities","Number of tweets")
        }
        else if (input$Candidates == "Marco Rubio") {
          ggplot(finalTweetPolarityDFMarcoLive, aes(x=Polarity)) + geom_bar(aes(y=..count.., fill=Polarity)) + labs("Polarities","Number of tweets")
        }
        else if (input$Candidates == "Ted Cruz") {
          ggplot(finalTweetPolarityDFTedLive, aes(x=Polarity)) + geom_bar(aes(y=..count.., fill=Polarity)) + labs("Polarities","Number of tweets")
        }
        
        
      })
  }
)