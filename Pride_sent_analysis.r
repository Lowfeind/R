library(rtweet)
tweet2 <- tweet
is.data.frame(tweet2)
is.data.frame(tweet)
tweet1 <- tweet
tweet1 <- as.data.frame(tweet1)
tweet2 <- as.data.frame(tweet2)

#TWEETS FROM 24TH JUNE TO 10TH JULY
tweetcompleto <- rbind(tweet1, tweet2)

#Sample of 10k tweets
set.seed(12345)
tweetcampione <- sample(tweetcompleto$text, 10000, replace = F)

library(tm)
library(textclean)
library(igraph)
library(ggraph)
library(ggplot2)

#Data-cleaning (URL, EMOTICON, HASHTAG, NON ASCII)
tweetcampione <- replace_url(tweetcampione, mgsub = "")
tweetcampione <- replace_emoticon(tweetcampione, mgsub="")
tweetcampione <- replace_hash(tweetcampione,mgsub="")
tweetcampione <- replace_non_ascii(tweetcampione, mgsub="")
tweetcampione

#CORPUS creation
corpus <- VCorpus(VectorSource(tweetcampione)) 
summary(corpus)

#Pre-processing
corpus <- tm_map(corpus, content_transformer(tolower))

corpus <- tm_map(corpus, removeNumbers)

corpus <- tm_map(corpus, removeWords, stopwords("english"))

removeTwitterHandles <- function(x) gsub("@\\S+", "", x) 
corpus <- tm_map(corpus, content_transformer(removeTwitterHandles))

corpus <- tm_map(corpus, removeNumbers)

corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, "amp")

corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, stripWhitespace)

summary(corpus)

corpus2 <- tm_map(corpus, removeWords, c("pride", "happi", "celebr", "day", "year", "month", "last", "via"))

corpus2 <- tm_map(corpus2, removePunctuation)

corpus2 <- tm_map(corpus2, stripWhitespace)

#STEMMING
library("SnowballC")
corpus <- tm_map(corpus, stemDocument)
corpus2 <- tm_map(corpus2, stemDocument)

# Term-document matrix
termdoc <- TermDocumentMatrix(corpus) #9406 termini
termdoc2 <- TermDocumentMatrix(corpus2) #9398
termdoc_nospar <- removeSparseTerms(termdoc, 0.992) #192 termini 
termdoc_nospar2 <- removeSparseTerms(termdoc2, 0.992) #184 termini

termdoc_matrix <- as.matrix(termdoc_nospar)
termdoc_matrix2 <- as.matrix(termdoc_nospar2)

v <- sort(rowSums(termdoc_matrix),decreasing=TRUE) 
v2 <- sort(rowSums(termdoc_matrix2),decreasing=TRUE)
df_ordinato <- data.frame(word = names(v),freq=v) 
df_ordinato2 <- data.frame(word = names(v2),freq=v2)

findAssocs(termdoc_nospar, terms = findFreqTerms(termdoc_nospar, lowfreq = 500), corlimit = 0.10)
#lgbtq and member

#WordCloud 
library("wordcloud")
library("RColorBrewer")
par(bg="midnightblue") 
png(file="WordCloud2.png",width=600,height=500, bg="midnightblue")
wordcloud(df_ordinato$word[1:192], df_ordinato$freq[1:192], col=rainbow(length(df_ordinato$word), alpha=0.9), random.order=FALSE, rot.per=0.3 )
title(main = "Word cloud #Pride2020", font.main = 1, col.main= "gold", cex.main = 2)
dev.off()


library('syuzhet')

# corpus into dataframe
dfcorpus <- data.frame(text=unlist(sapply(corpus,'[', "content")), stringsAsFactors=F)
dfcorpus2<- data.frame(text=unlist(sapply(corpus2, '[', "content")), stringsAsFactors = F)

sentiment <- get_nrc_sentiment(dfcorpus$text, language = "english")

sentimentitot <- data.frame(colSums(sentiment))

names(sentimentitot)[1] <- "count"
sentimentitot <- cbind("sentiment" = rownames(sentimentitot), sentimentitot)
rownames(sentimentitot) <- NULL

sentimentitot2<-sentimentitot[1:8,]
posneg <- sentimentitot[9:10, ]

# barplot
library("ggplot2")
qplot(sentiment, data=sentimentitot2, weight=count, geom="bar",fill=sentiment)+ggtitle("Sentimenti")

qplot(sentiment, data=posneg, weight=count, geom = "bar", fill=sentiment)+ggtitle("Sentimenti")



#Grapf of words with 184 words -except "pride", "happi", "celebr", "day", "year", "month", "last", "via"-
library(igraph)
library(ggraph)
library(ggplot2)

par(mar=c(1, 2, 1, 2)+0.1)

matriceadiacenza <- termdoc_matrix2 %*% t(termdoc_matrix2)#ADIACENCY MATRIX
grafotermini <- graph_from_adjacency_matrix(matriceadiacenza, "undirected", diag = F, weighted = T)

E(grafotermini)$width <- (E(grafotermini)$weight)*0.1
E(grafotermini)$color <- "green"
E(grafotermini)$alpha <- (E(grafotermini)$weight)*0.1
which.max(E(grafotermini)$weight)
E(grafotermini)$weight[14151]

Matches <- E(grafotermini)$weight

#PLOTS WITH GGPLOT
ggraph(grafotermini, layout = "fr") +
  geom_edge_link(aes(edge_width = Matches, edge_alpha = Matches), edge_colour = "springgreen4") +
  geom_node_point(size=(sqrt(graph.strength(grafotermini)))*0.1, color="gold") +
  geom_node_text(aes(label = name), col = "darkred", size = 4) +
  theme(legend.position = "right") +
  labs(title = "Grafo dei termini", subtitle = "Layout Fruchterman Reingold")

ggraph(grafotermini, layout = layout.kamada.kawai(grafotermini)) +
  geom_edge_link(aes(edge_width = Matches, edge_alpha = Matches), edge_colour = "springgreen4") +
  geom_node_point(size=(sqrt(graph.strength(grafotermini)))*0.1, color="gold") +
  geom_node_text(aes(label = name), col = "darkred", size = 4) +
  theme(legend.position = "right") +
  labs(title = "Grafo dei termini", subtitle = "Layout Kamada Kawai")

ggraph(grafotermini, layout = layout_nicely(grafotermini)) +
  geom_edge_link(aes(edge_width = Matches, edge_alpha = Matches), edge_colour = "springgreen4") +
  geom_node_point(size=(sqrt(graph.strength(grafotermini)))*0.1, color="gold") +
  geom_node_text(aes(label = name), col = "darkred", size = 4) +
  theme(legend.position = "right") +
  labs(title = "Grafo dei termini", subtitle = "Layout Nicely")

ggraph(grafotermini, layout = layout.mds(grafotermini)) +
  geom_edge_link(aes(edge_width = Matches, edge_alpha = Matches), edge_colour = "springgreen4") +
  geom_node_point(size=(sqrt(graph.strength(grafotermini)))*0.1, color="gold") +
  geom_node_text(aes(label = name), col = "darkred", size = 4) +
  theme(legend.position = "right") +
  labs(title = "Grafo dei termini", subtitle = "Layout Multi-Dimensional Scaling")

ggraph(grafotermini, layout =layout_randomly(grafotermini)) +
  geom_edge_link(aes(edge_width = Matches, edge_alpha = Matches), edge_colour = "springgreen4") +
  geom_node_point(size=(sqrt(graph.strength(grafotermini)))*0.1, color="gold") +
  geom_node_text(aes(label = name), col = "darkred", size = 4) +
  theme(legend.position = "right") +
  labs(title = "Grafo dei termini", subtitle = "Layout Randomly")

ggraph(grafotermini, layout =layout.davidson.harel(grafotermini)) +
  geom_edge_link(aes(edge_width = Matches, edge_alpha = Matches), edge_colour = "springgreen4") +
  geom_node_point(size=(sqrt(graph.strength(grafotermini)))*0.1, color="gold") +
  geom_node_text(aes(label = name), col = "darkred", size = 4) +
  theme(legend.position = "right") +
  labs(title = "Grafo dei termini", subtitle = "Layout Davison Harel")

ggraph(grafotermini, layout =layout.auto(grafotermini)) +
  geom_edge_link(aes(edge_width = Matches, edge_alpha = Matches), edge_colour = "springgreen4") +
  geom_node_point(size=(sqrt(graph.strength(grafotermini)))*0.1, color="gold") +
  geom_node_text(aes(label = name), col = "darkred", size = 4) +
  theme(legend.position = "right") +
  labs(title = "Grafo dei termini", subtitle = "Layout Automatico")

degree <- degree(grafotermini,v=V(grafotermini),loops=FALSE)

mean(degree)
var(degree)
degree.distribution(grafotermini)

V <- V(grafotermini)

diameter(grafotermini,weights=E(grafotermini)$weights)

farthest_vertices(grafotermini) #can communiti

edge_density (grafotermini)

vcount(grafotermini)

ecount(grafotermini)

betg<-betweenness(grafotermini,directed=F,normalized=T,weights=E(grafotermini)$weights)
which.max(betg)
which.min(betg)

closg<-closeness(grafotermini,normalized=T,weights=E(grafotermini)$weights)
which.max(closg)
which.min(closg)

distance_table(grafotermini, directed=FALSE)

#COMMUNITY DETECTION
par(bg = "white")
par(mar=c(2, 2, 1, 1) + 0.1)
c1 <- cluster_fast_greedy(grafotermini)
modularity(c1)
c1$modularity
plot(c1, grafotermini)
length(c1)
sizes(c1)

c2 <- cluster_optimal(grafotermini)
plot(c1, vertex.color=membership(c1), grafotermini)

member <- c1$membership
member1 <- which(member==1)
member2 <- which(member==2)
member3 <- which(member==3)
member4 <- which(member==4)
member5 <- which(member==5)
member6 <- which(member==6)

grafo1 <- grafotermini - vertices(c(member2, member3, member4, member5, member6))
Matches1 <- E(grafo1)$weight
ggraph(grafo1, layout = "fr") +
  geom_edge_link(aes(edge_width = Matches1, edge_alpha = Matches1), edge_colour = "springgreen4") +
  geom_node_point(size=(sqrt(graph.strength(grafo1)))*0.3, color="gold") +
  geom_node_text(aes(label = name), col = "darkred", size = 4) +
  theme(legend.position = "right") +
  labs(title = "Grafo dei termini", subtitle = "Layout Fruchterman Reingold")
degree(grafo1)
max_cliques(grafo1, min=20)

grafo2 <- grafotermini - vertices(c(member1, member3, member4, member5, member6))
Matches2 <-E(grafo2)$weight
ggraph(grafo2, layout = "fr") +
  geom_edge_link(aes(edge_width = Matches2, edge_alpha = Matches2), edge_colour = "springgreen4") +
  geom_node_point(size=(sqrt(graph.strength(grafo2)))*0.3, color="gold") +
  geom_node_text(aes(label = name), col = "darkred", size = 4) +
  theme(legend.position = "right") +
  labs(title = "Grafo dei termini", subtitle = "Layout Fruchterman Reingold")
degree(grafo2)
max_cliques(grafo2)


grafo3 <- grafotermini - vertices(c(member1, member2, member4, member5, member6))
Matches3 <-E(grafo3)$weight
ggraph(grafo3, layout = "fr") +
  geom_edge_link(aes(edge_width = Matches3, edge_alpha = Matches3), edge_colour = "springgreen4") +
  geom_node_point(size=(sqrt(graph.strength(grafo3)))*0.3, color="gold") +
  geom_node_text(aes(label = name), col = "darkred", size = 4) +
  theme(legend.position = "right") +
  labs(title = "Grafo dei termini", subtitle = "Layout Fruchterman Reingold")
degree(grafo3)
max_cliques(grafo3)

grafo4 <- grafotermini - vertices(c(member1, member2, member3, member5, member6))
Matches4 <-E(grafo4)$weight
ggraph(grafo4, layout = "fr") +
  geom_edge_link(aes(edge_width = Matches4, edge_alpha = Matches4), edge_colour = "springgreen4") +
  geom_node_point(size=(sqrt(graph.strength(grafo4)))*0.3, color="gold") +
  geom_node_text(aes(label = name), col = "darkred", size = 4) +
  theme(legend.position = "right") +
  labs(title = "Grafo dei termini", subtitle = "Layout Fruchterman Reingold")
degree(grafo4)
max_cliques(grafo4, min = 29)

grafo5 <- grafotermini - vertices(c(member1, member2, member3, member4, member6))
Matches5 <-E(grafo5)$weight
ggraph(grafo5, layout = "fr") +
  geom_edge_link(aes(edge_width = Matches5, edge_alpha = Matches5), edge_colour = "springgreen4") +
  geom_node_point(size=(sqrt(graph.strength(grafo5)))*0.3, color="gold") +
  geom_node_text(aes(label = name), col = "darkred", size = 4) +
  theme(legend.position = "right") +
  labs(title = "Grafo dei termini", subtitle = "Layout Fruchterman Reingold")
degree(grafo5)
max_cliques(grafo5, min = 38)

grafo6 <- grafotermini - vertices(c(member1, member2, member3, member4, member5))
Matches6 <-E(grafo6)$weight
ggraph(grafo6, layout = "fr") +
  geom_edge_link(aes(edge_width = Matches6, edge_alpha = Matches6), edge_colour = "springgreen4") +
  geom_node_point(size=(sqrt(graph.strength(grafo6)))*0.3, color="gold") +
  geom_node_text(aes(label = name), col = "darkred", size = 4) +
  theme(legend.position = "right") +
  labs(title = "Grafo dei termini", subtitle = "Layout Fruchterman Reingold")
degree(grafo6)
max_cliques(grafo6)


#BIGRAMS AND TRIGRAMS

library(udpipe)
library(lattice)
udmodel_en <- udpipe_download_model(language = "english")
udmodel_en <- udpipe_load_model(udmodel_en)

dfcorpus2m <- as.matrix(dfcorpus2)
dfcorpus2c <- as.character(dfcorpus2m)

x <- udpipe_annotate(udmodel_en, x = dfcorpus2c)
x <- as.data.frame(x)

x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats2 <- subset(stats, ngram==2 & freq > 3)
stats2$key <- factor(stats2$keyword, levels = rev(stats2$keyword))
barchart(key ~ freq, data = head(stats2, 30), col = "cadetblue", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")

stats3 <- subset(stats, ngram==3 & freq>3)
stats3$key <- factor(stats3$keyword, levels = rev(stats3$keyword))
barchart(key ~ freq, data = head(stats3, 30), col = "cadetblue", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")
