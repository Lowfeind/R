# Sent_Analysis
Project on sentiment made through the software R. The library employed was "rtweet". The main topic of the analysis was Pride 2020.
The tweet data have been extract uder the hashtag #Pride2020. Retweets have been excluded. The data have been extract from 24th June 2020 to 10th July 2020.
Overall, the tweets were 19354 and were all in English. From all these tweets a non-bernoullian sample of 10000 units has been extracted.
The first step was data-cleaning, in detail removal of: URLs, emoticons, hashtags, non-ASCII charachters.

After creating the corpus, the next step was pre-processing. This phase coincided with:
1 - word normalisation (all words in lowercase);
2 - numbers removal;
3 - stopwords removal;
4 - tags removal;
5 - other corrections;
6 - stemming through the library "SnowballC"

Subsequently, Term-Doc matrix has been created, where the rows were the 9406 types and the columns were the 10000 tweets/documents. Because text data, just like
in this case, are often sparse, a core task was sparseness reduction. The result was a dataframe with 192 words in a decreasing order of frequency.

The sentiment analysis was carried out through the library "syuzhet". The output was a dataframe, where rows were documents and columns were the "sentiments" such as:
anger, anticipation, disgust, fear, disgust, joy, sadness, surprise, trust, negative and positive. For each tweet has been specified the frequency of each sentiment.

To provide more information social network analysis techniques have been employed.
First of all, high frequency words have been excluded because they do not provide so much information. Therefore, a new corpus has been created and all the operations
made on the first corpus have been made on this second corpus as well. 

The second Ter-Doc matrix is composed of 184 words and it has been used to determine the adiaceny matrix. The latter is important in order to create the graphs and
highlight the relationships among the words.

To improve graph interpretation community detection has been employed. The main goal was to identify tight groups in the network. To create the communities, the
"fast greedy" algorithm has been used and the output was 6 communities. The main network and the 6 communities were represented through the software "Gephi"

The last phase of the whole analysis was bigrams and trigrams creation, i.e., the most frequent sequences of 2-3 words. 
