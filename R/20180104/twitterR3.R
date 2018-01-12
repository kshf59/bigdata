install.packages("base64enc") # 한글 인코딩 관련 라이브러리
install.packages(c("RCurl","twitteR","ROAuth")) # twitteR 포함 인증관련 라이브러리
install.packages("KoNLP") # 한글처리를 위한 라이브러리
install.packages("plyr")
install.packages("tm")
install.packages("devtools")
devtools::install_github("lchiffon/wordcloud2")
library(base64enc)
library(RCurl)
library(twitteR)
library(ROAuth)
library(KoNLP)
library(wordcloud)
library(plyr)
library(tm)
library(wordcloud2)

reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authroize"

consumerKey <- 'UG6Qqz01Veq0bTgdq4ojdj2AS'
consumerSecret <- '5dBHeIWRWWgkfkaY8gnyjHFp0lUQ68mPDVy0uxpKUDkDiF4Yqu'
accesstoken <- '167293174-aa7sJU5BJnpPP3B07JB22ZOB8NAwyu5PKGa3Uswc'
accesstokensecret <- 'AblwlRf7UwipDbrvY0MqATzingpnuLh9akUdpXj2kxfrg'

cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL="https://api.twitter.com/oauth/request_token",
                         accessURL="https://api.twitter.com/oauth/access_token",
                         authURL="https://api.twitter.com/oauth/authroize")

cred$handshake(cainfo = system.file("CurlSSL","cacert.pem", package = "RCurl"))
# 인증처리
setup_twitter_oauth(consumerKey,consumerSecret,accesstoken,accesstokensecret)

# 해당 단어를 언급한 글을 가지고 오고 싶을 때 사용
#keyword <- enc2utf8("빕스") # 한글처리
#tweets <- searchTwitter(keyword,n=10) # 가지고 오고 싶은 내용 검색
#tmp <- unlist(tweets, use.names = TRUE)
#head(tweets)

# 트위터에 키워드로 검색, 시작날짜, 지역코드(우리나라만 적용), 가져올 개수를 옵션에 대입
# 지역코드를 넣을려면 geocode='35.874,128.246,400km' 해당 부분을 넣으면 된다.

keyword <- enc2utf8("쿠우쿠우")
# since 와 until로 날짜 지정해줘봤자 7일분 지정
twitter <- searchTwitter(keyword,
                         since='2017-10-01',
                         until='2017-12-31',
                         lang = "ko",
                         n=1500)
twitter.df <- twListToDF(twitter) # data.frame 형태로 해당 트위터의 정보를 추출한다
names(twitter.df)
twitter.df <- twitter.df[twitter.df$retweetCount!=0,]
twitter.text <- twitter.df$text # mention text만 추출
# gsub(지울글자, 어떤글자로 , 데이터)
twitter.text <- gsub("ㅠㅠ", "", twitter.text)
twitter.text <- gsub("\n", "", twitter.text)
twitter.text <- gsub("\r", "", twitter.text)
twitter.text <- gsub("RT", "", twitter.text)
twitter.text <- gsub("http","", twitter.text)
twitter.text <- gsub("co", "", twitter.text)
twitter.text <- gsub("ㅋ","", twitter.text)
twitter.text <- gsub("짱쿠우","",twitter.text)
twitter.text <- gsub("슬쩍","",twitter.text)
twitter.text <- gsub("잡음","",twitter.text)
twitter.text <- gsub("때헝","",twitter.text)
twitter.text <- gsub("오즈임","",twitter.text)
twitter.text <- gsub("주삼","",twitter.text)
twitter.text <- gsub("스리","",twitter.text)
twitter.text <- gsub("희희인","",twitter.text)
twitter.text <- gsub("쿠우라편은","",twitter.text)
twitter.text <- gsub("00","",twitter.text)
twitter.text <- gsub("쿠우님","",twitter.text)
twitter.text <- gsub("쿠우쿠우","",twitter.text)
twitter.text <- gsub("오공이","",twitter.text)
twitter.text <- gsub("쿠우","",twitter.text)
twitter.text <- gsub("블레이드","",twitter.text)
twitter.text <- gsub("바쿠우라","",twitter.text)
twitter.text <- gsub("세계쿠우가","",twitter.text)
twitter.text <- gsub("보쿠우미","",twitter.text)
twitter.text <- gsub("ㅎㅎ","",twitter.text)
twitter.text <- gsub("00","",twitter.text)
twitter.text <- gsub("희희인","",twitter.text)
twitter.text <- gsub("쿠우라편은","",twitter.text)
twitter.text <- gsub("오레오","",twitter.text)
twitter.text <- gsub("뫄뫄즈","",twitter.text)
twitter.text <- gsub("세계","",twitter.text)
twitter.text <- gsub("보미","",twitter.text)
twitter.text <- gsub("트친소","",twitter.text)
twitter.text <- gsub("암주선행컷","",twitter.text)
twitter.text <- gsub("가담항설","",twitter.text)
twitter.text <- gsub("트친들","",twitter.text)
twitter.text <- gsub("세계가","",twitter.text)
twitter.text <- gsub("란지님","",twitter.text)
twitter.text <- gsub("갓친사","",twitter.text)
twitter.text <- gsub("▽","",twitter.text)
twitter.text <- gsub("캇우라","",twitter.text)
twitter.text <- gsub("woo89","",twitter.text)
twitter.text <- gsub("토큐본진","",twitter.text)
twitter.text <- gsub("51","",twitter.text)
twitter.text <- gsub("31","",twitter.text)
twitter.text <- gsub("23","",twitter.text)
twitter.text <- gsub("0","",twitter.text)
twitter.text <- gsub("이치죠상은","",twitter.text)
twitter.text <- gsub("켄마","",twitter.text)
# 문자분리 extracNoun:KoNLP 패키의 함수로 명사를 추출한다.
result_nouns <- Map(extractNoun, twitter.text)
# 쓸모없는 문자들을 제거한다. 특히 영자의 경우 tm의 stopwords를 활용한다.
result_wordsvec <- unlist(result_nouns, use.name = F)
# 영어 삭제(이거 하면 데이터 다날아감 이유를 모르겠음)
#result_wordsvec <- result_wordsvec[-which(result_wordsvec %in% stopwords("english"))]
# ?????
result_wordsvec <- gsub("[[:punct:]]","",result_wordsvec)
# 2개이상 글자만
result_wordsvec <- Filter(function(x){nchar(x) >=2}, result_wordsvec)
result_wordsvec <- Filter(function(x) { nchar(x) < 6 }, result_wordsvec)


# 단어별 카운팅
twitter_count <- table(result_wordsvec)
tmp <- head(sort(twitter_count,decreasing = T),100)
# 컬러세팅
pal <- brewer.pal(12,"Paired")
# 폰트세팅
windowsFonts(malgun=windowsFont("Arial"))
# 그리기
#wordcloud(names(tmp),
#          freq=tmp,
#          scale=c(3,0.5),          
#          min.freq = 7,
#          random.order = F,
#          random.color = T,
#          rot.per = 0.4,
#          colors = brewer.pal(8,"Dark2"),
#          family="malgun"
#          )
wordcloud2(data = tmp,
           size = 0.7,
           color = "random-light",
           backgroundColor = "black",
           rotateRatio = 0.8,
           shape = 'pentagon'
)
