
# loading the packages for scraping the data from the website (https://news.yahoo.com/):
library(dplyr) # for pipes and the data_frame function
library(rvest) # webscraping
library(stringr) # to deal with strings and to clean up our data

#Specifying the url for desired website to be scraped
url1 <- 'https://news.yahoo.com/sri-lanka-bombings-tell-us-224027409.html'
url2 <- 'https://news.yahoo.com/facebook-beats-profit-estimates-sets-aside-3-billion-003700391--sector.html'
url3 <- 'https://news.yahoo.com/student-loan-crisis-borrowers-dont-understand-costs-193650429.html'
url4 <- 'https://news.yahoo.com/wait-listed-dream-college-may-125741694.html'
url5 <- 'https://news.yahoo.com/school-principal-sparks-outrage-dress-163619494.html'
url6 <- 'https://news.yahoo.com/baltimore-violins-combat-violence-060944652.html'
url7 <- 'https://news.yahoo.com/man-apos-intentionally-apos-drove-104836078.html'
url8 <- 'https://news.yahoo.com/muslim-woman-viral-anti-islam-protest-photo-020027886.html'
url9 <- 'https://news.yahoo.com/death-toll-sri-lanka-bombing-attacks-rises-359-041452977.html'
url10 <-'https://news.yahoo.com/extinction-rebellion-activists-glue-themselves-to-london-stock-exchange-on-final-day-of-protests-084457909.html'



#Reading the HTML code from the website
webpage1 <- read_html(url1)
webpage2 <- read_html(url2)
webpage3 <- read_html(url3)
webpage4 <- read_html(url4)
webpage5 <- read_html(url5)
webpage6 <- read_html(url6)
webpage7 <- read_html(url7)
webpage8 <- read_html(url8)
webpage9 <- read_html(url9)
webpage10 <- read_html(url10)


#Using CSS selectors to scrap the text section
article_data_html1 <- html_nodes(webpage1,'.canvas-body')
article_data_html2 <- html_nodes(webpage2,'.canvas-body')
article_data_html3 <- html_nodes(webpage3,'.canvas-body')
article_data_html4 <- html_nodes(webpage4,'.canvas-body')
article_data_html5 <- html_nodes(webpage5,'.canvas-body')
article_data_html6 <- html_nodes(webpage6,'.canvas-body')
article_data_html7 <- html_nodes(webpage7,'.canvas-body')
article_data_html8 <- html_nodes(webpage8,'.canvas-body')
article_data_html9 <- html_nodes(webpage9,'.canvas-body')
article_data_html10 <- html_nodes(webpage10,'.canvas-body')



#Converting the ranking data to text
article_data1 <- html_text(article_data_html1)
article_data2 <- html_text(article_data_html2)
article_data3 <- html_text(article_data_html3)
article_data4 <- html_text(article_data_html4)
article_data5 <- html_text(article_data_html5)
article_data6 <- html_text(article_data_html6)
article_data7 <- html_text(article_data_html7)
article_data8 <- html_text(article_data_html8)
article_data9 <- html_text(article_data_html9)
article_data10 <-html_text(article_data_html10)

#Let's have a quick look at the second article
head(article_data2)

#Loading more packages for text analysis
library(tm)
library(SnowballC)
library(stringi)
library(wordcloud)

#Combining all the article texts 
combined.txt<-c(article_data1,article_data2,article_data3, article_data4, article_data5,
                article_data6, article_data7, article_data8, article_data9, article_data10)

#Making a corpus out of the combined text
Web.Data.Corpus <- VCorpus(VectorSource(combined.txt))

inspect(Web.Data.Corpus)
Web.Data.Corpus[[1]]$content[1]
Web.Data.Corpus[[1]]$meta
inspect(Web.Data.Corpus)

# Text analysis - Preprocessing 
clean.corpus<-function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <-tm_map(corpus, stemDocument, language = "english") # Perform Stemming
  return(corpus)
}

cleaned.corpus <- clean.corpus(Web.Data.Corpus) # applying the above cleaning function to the corpus
remove(Web.Data.Corpus) # removing the raw corpus
cleaned.corpus[[1]]$content[1]

#Making a DocumentTermMatrix(dtm)
dtm <- DocumentTermMatrix(cleaned.corpus)
dtm <- removeSparseTerms(dtm, 0.8)
m <- as.matrix(dtm)
dim(m)
inspect(dtm)

#Implementing the hierarchical clustering and ploting the dendrogram
docsdissim <- dist(m, method = "euclidean") # Distance Measure
# Hierarchical clustering using Complete Linkage
h <- hclust(as.dist(docsdissim), method = "complete") # Group Results
# Plot the obtained dendrogram
plot(h, cex = 0.6, hang = -1)

#Frequency counting
v <- sort(colSums(m),decreasing=TRUE) #sorting the matrix with decreasing order by summing the columns
d <- data.frame(word = names(v),freq=v) # creating a data frame out of it
str(v)
head(d, 5)
rownames(dtm)

#Word Cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=75, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


#Plotting the 5 most freuent words with the frequencies they appear in each document

#As we already know the 5 most frequent words from the above code 
my_words <- c("said", "school", "attack", "peopl", "student")

dtm5 <- DocumentTermMatrix(cleaned.corpus, control=list(dictionary = my_words))
m2 <- as.matrix(dtm5)
d2 <- data.frame(m2)
d2 <- data.frame(tibble::rowid_to_column(d2, "DocID"))

#Plotting the upper data frame
plot(Col3 <- gvisColumnChart(d2, yvar = c("said", "school", "attack", "peopl", "student"),
                             xvar = 'DocID',
                             options=list(title="Top 5 words",
                                          vAxis="{title:'Frequencies'}",
                                          hAxis= "{title:'Documents'}",
                                          titleTextStyle="{color:'black',fontName:'Courier',fontSize:20}",
                                          bar="{groupWidth:'100%'}")))
