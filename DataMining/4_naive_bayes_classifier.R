# 나이브베이트 분류기 예제

# install.packages('ElemStatLearn') # spam 이메일 데이터셋
# install.packages('e1071')
library(e1071)

data(spam, package="ElemStatLearn")
spam[1,]
str(spam)

num_feature <- ncol(spam)
num_observation <- nrow(spam)
num_sapm <- length(which(spam[,58]=='spam'))
num_email <- length(which(spam[,58]=='email'))

cv = sample(nrow(spam), floor(nrow(spam) * 0.9), replace=FALSE)
train = spam[cv,]
test = spam[-cv,]

# m <- naiveBayes(spam ~ ., data = spam)
m <- naiveBayes(train[,-num_feature], train[,num_feature], laplace=1)
predict(m, test[,-num_feature])
table(predict(m, test[,-num_feature]), test$spam)





# 가사를 통한 음악 장르 구분 
# TODO: 마지막 테스트 데이터 검증 부분
# TODO: label 처리 부분
##################################################################
# http://www.cs.uu.nl/docs/vakken/b3dar/dar-lecture-1.pdf 참고

setwd("C:\\Users\\Coupang\\gdrive_bt22dr\\업무 노트\\R\\kku")
setwd("/Users/bt22dr/gdrive_bt22dr/업무 노트/R/kku")

# install.packages("tm")
library(tm)

# 지정 디렉터리에서 가사 텍스트 읽기
lyrics.metal <- Corpus(DirSource("./data/lyrics/Metal", pattern="*.txt"), 
                       readerControl=list(reader=readPlain))
meta(lyrics.metal, "genre") <- "Metal"
lyrics.blues <- Corpus(DirSource("./data/lyrics/Blues", pattern="*.txt"), 
                       readerControl=list(reader=readPlain))
meta(lyrics.blues, "genre") <- "Blues"

# 하나의 corpus로 병합
lyrics <- c(lyrics.metal,lyrics.blues)
lyrics

# corpus에 전처리 수행
lyrics <- tm_map(lyrics, stripWhitespace)
# lyrics <- tm_map(lyrics, tolower)
lyrics <- tm_map(lyrics, removeWords,stopwords("english"))

# 트레이닝 데이터 스플릿
lyrics.train <- c(sample(1:20,15),sample(21:40,15))

# document-term matrix 추출
lyrics.train.dtm <- DocumentTermMatrix(lyrics[lyrics.train])
dim(lyrics.train.dtm)

# 문서에서 10% 이하로 나타나는 term 제거
lyrics.train.dtm <- removeSparseTerms(lyrics.train.dtm,0.9)
dim(lyrics.train.dtm)

# 트레이닝 데이터에 존재하는 term의 사전 
lyrics.dict <- dimnames(lyrics.train.dtm)[[2]]
lyrics.dict

# 테스트 데이터도 동일한 term을 갖도록 위에서 만든 사전 사용
lyrics.test.dtm <- DocumentTermMatrix(lyrics[-lyrics.train], list(dictionary=lyrics.dict))
lyrics.test.dtm.bin <- inspect(lyrics.test.dtm)
lyrics.test.dtm.bin <- lyrics.test.dtm.bin > 0
lyrics.test.dtm.bin <- as.data.frame(lyrics.test.dtm.bin)
for(i in 1:length(lyrics.test.dtm.bin)){lyrics.test.dtm.bin[,i] <- as.factor(lyrics.test.dtm.bin[,i])}

# 트레이닝 데이터 변환
# dtm에서 일반 matrix로 변환하여 Bernoulli model을 위한 binary화 수행
lyrics.train.dtm.bin <- inspect(lyrics.train.dtm)
lyrics.train.dtm.bin <- lyrics.train.dtm.bin > 0
lyrics.train.dtm.bin <- as.data.frame(lyrics.train.dtm.bin)
for(i in 1:length(lyrics.train.dtm.bin)){lyrics.train.dtm.bin[,i] <- as.factor(lyrics.train.dtm.bin[,i])}

# class label 추출
lyrics.lab <- as.vector(unlist(lapply(lyrics, meta, tag="genre")))
lyrics.lab <- as.factor(lyrics.lab)
lyrics.lab <- c(rep("Metal", 20), rep("Blues", 20))

# 모델 생성
lyrics.nb <- naiveBayes(lyrics.train.dtm.bin, as.factor(lyrics.lab[lyrics.train]), laplace=1)

# 테스트 샘플로 예측
lyrics.nb.pred <- predict(lyrics.nb, lyrics.test.dtm.bin)

# confusion matrix 관찰
table(lyrics.nb.pred,lyrics.lab[-lyrics.train])

lyrics.nb$table$blood
lyrics.nb$table$baby




















