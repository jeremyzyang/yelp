# yelp 

# install.packages('RJSONIO')
# require(RJSONIO)    
# require(jsonlite)
# review <- stream_in(file("yelp_academic_dataset_review.json"))
# review <- flatten(review,recursive=T)

install.packages('readr')
library(readr)
library(stringr)
library(dplyr)
options(dplyr.width = Inf)

b = read_csv('yelp_business.csv')
#r = read_csv('yelp_review.csv')
u = read_csv('yelp_user.csv')
#t = read_csv('yelp_tip.csv')

r = read_csv('review_notext.csv')

# select columns and merge 

# business
head(b)
dim(b)
colnames(b)
b <- b[,c(72:75,90:97)]

cat <- collect(select(b, categories))[[1]]
cat2 <- strsplit(cat,',')
length(cat2)

c = vector()

for (i in 1:length(cat2)){
        len = length(cat2[[i]])
        c[i] = cat2[[i]][len]
        print (i)
}

c[1:20]
c <- str_replace_all(c,"\\[|'| ",'')
b = mutate(b,main_category = c)
head(b)

# review
head(r)
dim(r)
colnames(r)
r <- r[,-6]

# user
head(u)
dim(u)
colnames(u)
u <- u[,-c(15,18)]

# tip
head(t)
dim(t)
colnames(t)
t <- t[,-c(4,5)]

all <- left_join(b, r, by='business_id') %>%
        left_join(., t, by='business_id') 

all <- left_join(b,r,by='business_id')

head(all)
dim(all)
colnames(all)

names(all)[11] <- 'star_business'
names(all)[16] <- 'star_user'

write.csv(all,'yelp_review.csv',row.names=F)
rm(list=ls())

head(select(all,categories),20)

# processing text
library(tm)
library(slam)
require("tm.lexicon.GeneralInquirer")

text <- r %>% select(text) %>% collect %>% .[["text"]]
text <- collect(select(r, text))[[1]]
length(text)
test = text[1:10000]
test

df = split(text, ceiling(seq_along(text)/10000))
length(df)
class(df[[1]])
#rm(text)

GetCorpus <-function(textVector){
        doc.corpus <- Corpus(VectorSource(textVector))
        doc.corpus <- tm_map(doc.corpus, content_transformer(tolower))
        doc.corpus <- tm_map(doc.corpus, content_transformer(removeNumbers))
        doc.corpus <- tm_map(doc.corpus, content_transformer(removePunctuation))
        doc.corpus <- tm_map(doc.corpus, content_transformer(removeWords), stopwords("english"))
#        doc.corpus <- tm_map(doc.corpus, stemDocument)
#        doc.corpus <- tm_map(doc.corpus, stripWhitespace)
#        doc.corpus <- tm_map(doc.corpus, PlainTextDocument)
        return(doc.corpus)
        }

test = df[[1]]
c.test = GetCorpus(df[[1]])
c.test = GetCorpus(test)

class(df[[1]])
dtm = DocumentTermMatrix(c.test)
dim(dtm)

positive <- as.numeric(tm_term_score(dtm,terms_in_General_Inquirer_categories("Positiv"))) 
negative <- as.numeric(tm_term_score(dtm,terms_in_General_Inquirer_categories("Negativ"))) 
len <- as.matrix(rollup(dtm, 2, na.rm=TRUE, FUN = sum))

p <- data.frame()

for (i in 1:length(df)) {
        test = df[[i]]
        corpus <- GetCorpus(test)
        print (paste0('step',i,'.1'))
        dtm <- DocumentTermMatrix(corpus)
        print (paste0('step',i,'.2'))
        positive <- as.numeric(tm_term_score(dtm,terms_in_General_Inquirer_categories("Positiv"))) 
        negative <- as.numeric(tm_term_score(dtm,terms_in_General_Inquirer_categories("Negativ"))) 
        len <- rowSums(as.matrix(dtm))
        #len <- apply(dtm,2,sum)
        #len <- as.matrix(rollup(dtm, 1, na.rm=TRUE, FUN = sum))
        q <- cbind(positive,negative,len)
        p <- rbind(p,q)
        print (paste0('step',i,'.3'))
        df[[i]] <- 'NULL'
}

write.csv(s,'review_notext.csv',row.names=F)

ls()
rm(c.test,corpus,df,dtm,i,len,negative,p,positive,q,r,test,text)

# install.packages("tm.lexicon.GeneralInquirer", repos="http://datacube.wu.ac.at", type="source")
# tm_term_score(TermDocumentMatrix(corpus,
#                                  control = list(removePunctuation = TRUE)),
#               terms_in_General_Inquirer_categories("Positiv"))

positive <- as.numeric(tm_term_score(dtm,terms_in_General_Inquirer_categories("Positiv"))) 
negative <- as.numeric(tm_term_score(dtm,terms_in_General_Inquirer_categories("Negativ"))) 
len <- rollup(dtm, 2, na.rm=TRUE, FUN = sum)

# user and gender

u = read_csv('yelp_user.csv')

head(u)
dim(u)
colnames(u)
u <- u[,-c(15,18)]

name <- collect(select(u,name))[[1]]
name[1:10]

gen <- as.data.frame(gender(name))
gen <- gen[!duplicated(gen[,'name']),]

dim(gen)
head(gen)

data$firstname <- name

head(data)
head(gen)

final <- left_join(u,gen,by='name')
head(final)
colnames(final)
final <- final[,-c(25,26)]

write.csv(final,'yelp_user.csv',row.names=F)

head(final)
tail(final)