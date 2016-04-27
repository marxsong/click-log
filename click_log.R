rm(list=ls())
.libPaths('/usr/lib64/R/library')
library(RMySQL)

con<-dbConnect(RMySQL::MySQL(),host='10.4.20.116',port=3306,dbname='ods',username='biuser',password='biuser')
dbSendQuery(con, "SET character_set_client = gbk")
dbSendQuery(con, "SET character_set_connection = gbk")
dbSendQuery(con, "SET character_set_results = gbk")

query <- paste('SELECT  DATA_DT,u_mid,log_id,GROUP_CONCAT(click_module SEPARATOR \'', '\') as path FROM ods.click_log_aapp',
               'WHERE u_mid  in( SELECT a.mid FROM ots.u_member a JOIN ots.t_city b on a.citycode=b.code', 
                'LEFT JOIN (select a.mid from t_blacklist_a a)e on a.mid=e.mid',
                'WHERE b.ProID=82 AND e.mid is null) AND DATA_DT >=\'2015-11-01\'',
               'GROUP BY DATA_DT,u_mid,log_id',
               'ORDER BY u_mid',sep=' ')


res<-dbSendQuery(con,query)

cq_click_path_info <- dbFetch(res, n = -1) # fetch all results
dbClearResult(res)
dbDisconnect(con)


head(cq_click_path_info)


library(RWeka)
library(bitops)
library(RCurl)
library(digest)
library(ROAuth)
library(rjson)
library(tm)

rem_words <- function(x){
  x <- unlist(strsplit(x,' '))
  x <- x[cumsum(rle(as.character(x))$lengths)]  #去掉相邻重复
  x <- paste(x,collapse = " ")
  x
}


#找到各主页面跳转其他页面分布
path<- cq_click_path_info$path
#对path进行处理，去掉相邻重复的模块
path <- unlist(lapply(path,rem_words))
head(path)

myCorpus <- Corpus(VectorSource(path))
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
txtTdmBi <- TermDocumentMatrix(myCorpus, control = list(tokenize = BigramTokenizer))

termFrequency <- rowSums(as.matrix(txtTdmBi))
termFrequency <- subset(termFrequency, termFrequency>=10)

termFrequency <- sort(termFrequency, decreasing = TRUE)
termFrequency <- as.data.frame(termFrequency)
head(termFrequency,20)


#计算单次访问平均查看酒店次数
termNum <- apply(as.matrix(txtTdmBi), 1, function(x) {sum(x!=0)})
termNum <- sort(termNum, decreasing = TRUE)
termNum <- as.data.frame(termNum)
head(termNum,20)

termMean <- rowMeans(as.matrix(txtTdmBi))
termMean <- sort(termMean, decreasing = TRUE)
termMean <- as.data.frame(termMean)
head(termMean,20)

termMax <- apply(as.matrix(txtTdmBi), 1, function(x) {max(x)})
termMax <- sort(termMax, decreasing = TRUE)
termMax <- as.data.frame(termMax)
head(termMax,20)

termAll <- cbind(head(termFrequency,10),head(termNum,10),head(termMean,10),head(termMax,10))
termAll$Mean_Num <- round(termAll$termFrequency/termAll$termNum,2)
