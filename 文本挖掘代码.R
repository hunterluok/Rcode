install.packages("Rwordseg", repos="http://R-Forge.R-project.org")
segmentCN("画角色的人啊是不是我的")
insertWords(c("错过了","一场梦"))   #同时改变多个词典
insertWords("是不是")
deleteWords("错过")                #已经系统自动生成的是没有办法改变的
listDict()                        #查看已有的词典。
t1<-"那些年哥到底错过了些什么，是游戏还是感情，还是该学习的知识，终究一场梦，幻想用来憧憬，现实用来经历"
segmentCN(t1)


#词库下载地址：  http://pinyin.sogou.com/dict/，可以选择需要的分类词典下载。
#             http://pinyin.sogou.com/dict/cate/index/429

installDict("C:\\Users\\lenovo\\Desktop\\歌手人名大全【官方推荐】.scel", dictname ="names")
segmentCN("2015年的几部开年戏出现了唐嫣的呻吟")

installDict("C:\\Users\\lenovo\\Desktop\\textmini\\阿里巴巴产品词汇－服装.scel", dictname ="ali")
installDict("C:\\Users\\lenovo\\Desktop\\textmini\\服饰词汇.scel", dictname ="fushi")
data<-readLines("D:\\software\\R\\R-3.3.2\\library\\Rwordseg\\dict\\ali.dic",encoding="UTF-8")

d1<-gsub("[0-9 0 1 2 3 4 5 6 7 8 9 < > ~]","",data)
d1<-segmentCN(d1)
d1[1:2]

stopwords<-unlist(d1,stringsAsFactors=FALSE)
stopwords[50:100]

removeStopWords <- function(x,stopwords) {
temp <- character(0)
index <- 1
xLen <- length(x)
while (index <= xLen) {
if(length(stopwords[stopwords==x[index]]) <1)
temp<- c(temp,x[index])
index <- index +1
}
temp
}



da2<-lapply(d1,removeStopWords,stopwords)
da2[1:2]

words <- lapply(hlzjTemp2,strsplit," ")
wordsNum <- table(unlist(words))
wordsNum <- sort(wordsNum) #排序
wordsData <- data.frame(words =names(wordsNum), freq = wordsNum)
library(wordcloud) #加载画词云的包
weibo.top150 <- tail(wordsData,150) #取前150个词
colors=brewer.pal(8,"Dark2")
wordcloud(wt100$words,wt100$freq.Freq,scale=c(8,0.5),colors=colors,random.order=F)







