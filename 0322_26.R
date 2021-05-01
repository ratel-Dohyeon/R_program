#2021년 3월 22일 월 

install.packages("foreign") #??????지 ??????

#라이브러리
library(foreign) #SPSS ?????? 로드
library(dplyr)   #???처리
library(ggplot2) #???각화
library(readxl)  #?????? ?????? 불러??????
library(KoNLP)
library(rJava)
library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(KoNLP)
library(rJava)
library(dplyr)
library(ggplot2)
library(stringr)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(stringr)
library(KoNLP)
library(rJava)
library(dplyr)
library(ggplot2)
library(readxl)
library(treemap)
library(plotly)
library(dygraphs)
library(xts)
library(ggiraphExtra)
library(tibble)
library(maps)
library(mapproj)
library(kormaps2014)
library(ggplot2)
library(dplyr)
library(devtools)
library(ggmap)
library(stringr)
library(leaflet)
library(ggiraphExtra)
library(tibble)
library(maps)
library(mapproj)
library(kormaps2014)
library(ggplot2)
library(dplyr)
library(devtools)
library(ggmap)
library(stringr)
library(leaflet)

fruits <- read.table("data/fruits.txt")

fruits

fruits <- read.table("data/fruits.txt", header = T)

fruits

str(fruits)

fruits2 <- read.table("data/fruits_2.txt")


fruits2

fruits2 <- read.table("data/fruits_2.txt",skip=2) #?????? 2??? ???????????? 불러??????
fruits2

fruits2 <- read.table("data/fruits_2.txt", nrows=2) #???줄만 출력
fruits2

fruits3 <- read.table("data/fruits.txt", header = T,nrows=2)

fruits3

fruits3 <- read.table("data/fruits.txt", header = F, skip=2, nrows=2)

fruits3

fruit3 <- read.csv("data/fruits_3.csv")
fruit3

fruit4 <- read.csv("data/fruits_4.csv")

fruit4

fruit4 <- read.csv("data/fruits_4.csv", header=F)

fruit4

label <- c("NO","NAME","PRICE","QTY")

fruit4 <-read.csv("data/fruits_4.csv", header=F, col.name = label) #???벨이 ????????? ?????????

fruit4

# 패키지 설치
#install.packages("rJava")
#install.packages("memoise")
#install.packages("KoNLP")
#install.packages("Sejong")
#install.packages("hash")
#install.packages("tau")
#install.packages("RSQLite")
#install.packages("devtools")

# java 폴더 경로 설정
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.281")
library(KoNLP)
library(rJava)
library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(KoNLP)
library(rJava)
library(dplyr)
library(ggplot2)
library(stringr)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(stringr)
library(KoNLP)
library(rJava)
library(dplyr)
library(ggplot2)
library(readxl)
library(treemap)
library(plotly)
library(dygraphs)
library(xts)
library(ggiraphExtra)
library(tibble)
library(maps)
library(mapproj)
library(kormaps2014)
library(ggplot2)
library(dplyr)
library(devtools)
library(ggmap)
library(stringr)
library(leaflet)
library(ggiraphExtra)
library(tibble)
library(maps)
library(mapproj)
library(kormaps2014)
library(ggplot2)
library(dplyr)
library(devtools)
library(ggmap)
library(stringr)
library(leaflet)


useNIADic()

txt<-readLines("Data/hiphop.txt")

head(txt)

#install.packages("stringr")

library(stringr)

txt <- str_replace_all(txt, "\\W", " ") #일괄수정

head(txt)



extractNoun("대한민국의 영토는 한번도와 그 부속도서로 한다")

nouns <-extractNoun(txt)
nouns


wordcount<- table(unlist(nouns))

wordcount


#데이터 프레임 변환 
df_word <- as.data.frame(wordcount, stringsAsFactors = F )


#변수명 수정
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)

#두글자 단어 이상 추출
df_word <- filter(df_word, nchar(word) >=2) 


#빈도수 높은순으로 정렬
top_20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)

top_20 


#패키지 설치
#install.packages("wordcloud")
#워드 클라우드 만들기
#패키지 로드(올린다)

library(wordcloud)
library(RColorBrewer)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(KoNLP)
library(rJava)
library(dplyr)
library(ggplot2)
library(stringr)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(stringr)
library(KoNLP)
library(rJava)
library(dplyr)
library(ggplot2)
library(readxl)
library(treemap)
library(plotly)
library(dygraphs)
library(xts)
library(ggiraphExtra)
library(tibble)
library(maps)
library(mapproj)
library(kormaps2014)
library(ggplot2)
library(dplyr)
library(devtools)
library(ggmap)
library(stringr)
library(leaflet)
library(ggiraphExtra)
library(tibble)
library(maps)
library(mapproj)
library(kormaps2014)
library(ggplot2)
library(dplyr)
library(devtools)
library(ggmap)
library(stringr)
library(leaflet)


pal <- brewer.pal(8,"Dark2")

set.seed(1234) #난수 고정

wordcloud(words = df_word$word, #단어
          freq = df_word$freq, # 빈도
          min.freq = 2,        # 최소단어 빈도
          max.words = 200,     # 표현 단어 수
          random.order = F,    # 고빈도 단어 중앙배치
          rot.per = .1,         # 회전단어 비율
          scale = c(4, 0.3),   # 단어크기 범위
          colors = pal)        # 색깔 목록

#------------------------------------------------------------
pal <- brewer.pal(9,"Blues")[5:9]
set.seed(1234)
          
wordcloud(words = df_word$word, #단어
          freq = df_word$freq, # 빈도
          min.freq = 2,        # 최소단어 빈도
          max.words = 200,     # 표현 단어 수
          random.order = F,    # 고빈도 단어 중앙배치
          rot.per = .1,         # 회전단어 비율
          scale = c(4, 0.3),   # 단어크기 범위
          colors = pal)        # 색깔 목록



twitter <- read.csv("data/twitter.csv",
                    header = T,
                    stringsAsFactors = F,
                    fileEncoding = "UTF-8")
twitter



twitter <-rename(twitter,
                 no = 번호,
                 id = 계정이름,
                 data = 작성일,
                 tw = 내용)
twitter

head(twitter, 5)


twitter$tw <- str_replace_all(twitter$tw,"\\W", " ") #띄어쓰기 공백처리

head(twitter$tw)



#트위터 명사 추출 
nouns <- extractNoun(twitter$tw)

#추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount <-table(unlist(nouns))

#데이터 프레임으로 변환 
df_word <- as.data.frame(wordcount, stringsAsFactors = F)

#변수명 수정
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)


df_word

library(ggplot2)
library(dplyr)

df_word <- filter(df_word, nchar(word) >= 2)


top20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)



order <- arrange(top20, freq)$word

ggplot(data = top20, aes(x= word, y=freq))+
  ylim(0, 2500)+
  geom_col()+
  coord_flip(limit = order)+
  geom_text(aes(label= freq), hjust=10.3)


pal <- brewer.pal(8, "Dark2")[5:9]
set.seed(1234)

wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 10,
          max.word = 200,
          random.order = F,
          rot.per = .1,
          scale = c(6,0.2),
          colors = pal)


#----------------------------------------------------------------------------------

#워드 클라우드 데이터 분석 
#install.packages("rJava")
#install.packages("memoise")
#install.packages("KoNLP")
#install.packages("Sejong")
#install.packages("hash")
#install.packages("tau")
#install.packages("RSQLite")
#install.packages("devtools")
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.281")

library(stringr)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(KoNLP)
library(rJava)
library(dplyr)
library(ggplot2)
library(stringr)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(stringr)
library(KoNLP)
library(rJava)
library(dplyr)
library(ggplot2)
library(readxl)
library(treemap)
library(plotly)
library(dygraphs)
library(xts)
library(ggiraphExtra)
library(tibble)
library(maps)
library(mapproj)
library(kormaps2014)
library(ggplot2)
library(dplyr)
library(devtools)
library(ggmap)
library(stringr)
library(leaflet)
library(ggiraphExtra)
library(tibble)
library(maps)
library(mapproj)
library(kormaps2014)
library(ggplot2)
library(dplyr)
library(devtools)
library(ggmap)
library(stringr)
library(leaflet)

jtxt<-readLines("Data/jeju.txt") #데이터 불러오기
  jtxt

jtxt <- str_replace_all(jtxt, "\\W", " ") #공백 일괄수정
  jtxt
  head(jtxt)

nounsj <-extractNoun(jtxt) #명사 추출
  head(nounsj)

#추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성  
wordcountj<- table(unlist(nounsj))
  wordcountj
  
  
#데이터 프레임 변환 
df_wordj <- as.data.frame(wordcountj, stringsAsFactors = F )


#변수명 수정
df_wordj <- rename(df_wordj,
                  word = Var1,
                  freq = Freq)
  df_wordj


df_wordj <- filter(df_wordj, nchar(word) >= 2)
  df_wordj


jtop20 <- df_wordj %>% 
  arrange(desc(freq)) %>% 
  head(20)

  jtop20


orderj <- arrange(jtop20, freq)$word
  orderj

ggplot(data = jtop20, aes(x= word, y=freq))+
  ylim(0, 2500)+
  geom_col()+
  coord_flip(limit = orderj)+
  geom_text(aes(label= freq), hjust=10.3)

palj <- brewer.pal(8, "Blues")[16:9]
set.seed(1234)

#워드 클라우드 생성
wordcloud(words = df_wordj$word,
          freq = df_wordj$freq,
          min.freq = 10,
          max.word = 200,
          random.order = F,
          rot.per = .1,
          scale = c(6,0.2),
          colors = palj)


# 2021년 03월 23일 화

#install.packages("rJava")
#install.packages("memoise")
#install.packages("KoNLP")
#install.packages("Sejong")
#install.packages("hash")
#install.packages("tau")
#install.packages("RSQLite")
#install.packages("devtools")
#install.packages("wordcloud2")
#install.packages("treemap")
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.281")

library(stringr)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(stringr)
library(KoNLP)
library(rJava)
library(dplyr)
library(ggplot2)
library(readxl)
library(treemap)
library(plotly)
library(dygraphs)
library(xts)
library(ggiraphExtra)
library(tibble)
library(maps)
library(mapproj)
library(kormaps2014)
library(ggplot2)
library(dplyr)
library(devtools)
library(ggmap)
library(stringr)
library(leaflet)
library(ggiraphExtra)
library(tibble)
library(maps)
library(mapproj)
library(kormaps2014)
library(ggplot2)
library(dplyr)
library(devtools)
library(ggmap)
library(stringr)
library(leaflet)

useSejongDic()

data1 <- readLines("data/seoul_new.txt")
  data1

#데이터(data1) 중 명사 추출 후 nouns에 할당
data2 <-sapply(data1, extractNoun, USE.NAMES = F) #'USE.NAMES=F'=이름은 없습니다 인거임
  data2


#추출명사 30개만 출력
head(unlist(data2),30)


data3 <- unlist(data2)

#gsub("수정전","수정후", 데이터)
data3 <- gsub("\\d+","", data3)
data3 <- gsub("서울시","", data3)
data3 <- gsub("서울","", data3)
data3 <- gsub("요청","", data3)
data3 <- gsub("제안","", data3)
data3 <- gsub(" ","", data3)
data3 <- gsub("-","", data3)
  data3


write(unlist(data3), "seoul_22.txt")

data4 <- read.table("seoul_22.txt")

nrow(data4)

wordcount <- table(data4)
  wordcount

head(sort(wordcount, decreasing = T), 20)

data4


data3 <- gsub("OO","", data3)
data3 <- gsub("개선","", data3)
data3 <- gsub("문제","", data3)
data3 <- gsub("관리","", data3)
data3 <- gsub("민원","", data3)
data3 <- gsub("이용","", data3)
data3 <- gsub("관련","", data3)
data3 <- gsub("시장","", data3)
  table(data3)
  head(data3,20)


write(unlist(data3), "seoul_3.txt")
data4 <- read.table("seoul_3.txt")
wordcount <- table(data4)
  wordcount

head(sort(wordcount, decreasing = T), 20)

#그래픽 출력
library(RColorBrewer)
palet <- brewer.pal(9,"Set3")

wordcloud(names(wordcount), 
          freq = wordcount, 
          scale = c(5,1), 
          rot.per = 0.25, 
          min.freq = 1,
          random.order = F, 
          random.color = T, 
          colors = palet)

legend(0.3,1,"서울시 응답소 요청사항 분석",
       cex=0.8, 
       fill=NA, 
       border=NA, 
       bg="white",
       text.col="red", 
       text.font=2, 
       box.col="red")


data1<- readLines("data/remake.txt")
data1

data2<- sapply(data1, extractNoun, USE.NAMES = F)
data2

data3<- unlist(data2)

data3<- Filter(function(x) {nchar(x) <= 10}, data3)
data3

head(unlist(data3),30)

data3 <- gsub("\\d+","",data3) #모든 숫자 없애버려


data3 <- gsub("쌍수","쌍꺼풀", data3)
data3 <- gsub("쌍커플","쌍꺼풀", data3)
data3 <- gsub("메부리코","메부리코", data3)
data3 <- gsub("\\.","", data3)
data3 <- gsub("","", data3)
data3 <- gsub(" ","", data3)
data3 <- gsub("\\","", data3)

data3

#공백 제거하기

write(unlist(data3),"remake_22.txt")
data4<-read.table("remake_22.txt")
data4
nrow(data4)
wordcount <-table(data4)
wordcount

head(sort(wordcount, decreasing =T),20) #필요한 단거어 20개만 가져와

data3 <- Filter(function(x) {nchar(x) >= 2}, data3)
write(unlist(data3),"remake_22.txt")
data4<-read.table("remake_22.txt")
  data4
nrow(data4)
wordcount <-table(data4)
  wordcount

head(sort(wordcount, decreasing =T), 30)


palete <- brewer.pal(9, "Set3")

wordcloud(names(wordcount), 
          freq = wordcount, 
          scale = c(5,1), 
          rot.per = 0.25, 
          min.freq = 2, 
          random.order = F, 
          random.color = T,
          colors = palete)

legend(0.3, 1,"여고생들이 선호하는 성형수술 부위",
       cex=0.8,
       fill=NA,
       border=NA,
       bg="white",
       text.col = "red",
       text.font=2,
       box.col="red")

#애국가 분석 
word_data <- readLines("data/애국가(가사).txt")
  word_data
  
word_data2 <- sapply(word_data, extractNoun, USE.NAMES = F)
  word_data2

add_words <-c("백두산","남산","철갑","가을","하늘","달")
  add_words


buildDictionary(user_dic = data.frame(add_words, rep("ncn", length(add_words))), replace_usr_dic = T)

add_words2 <-sapply(word_data, extractNoun, USE.NAMES = F) 
  add_words2

undata <- unlist (word_data2)

word_table <- table(undata)
word_table

undata2 <- Filter(function(x) {nchar(x)>=2}, undata)

word_table2 <- table(undata2)

#word_table2를 내림차순 정렬
sort(word_table2, decreasing = T)

wordcloud2(word_table2, 
           color = "random-light", 
           backgroundColor = "black")

wordcloud2(word_table2, 
           fontFamily = "맑은 고딕", 
           size = 1.2, 
           color = "random-light", 
           backgroundColor = "black", 
           shape="star")


useSystemDic()
useSejongDic()
useNIADic()


#치킨집 예제
ck<-read_excel("data/치킨집_가공.xlsx")
  head(ck)

addr<-substr(ck$소재지전체주소, 12, 16) #소재지전체주소의 12~16번째 문자 가져오기
  head(addr)

addr_num <- gsub("[0-9]","",addr) #숫자제거
addr_trim <-gsub(" ","", addr_num)#공백제거
  head(addr_trim)

addr_count <- addr_trim %>% table() %>% data.frame()
  head(addr_count)

#도표 생성(트리맵)
treemap(addr_count, index = ".", vSize = "Freq", title = "서대문구 동별 치킨집 분표") #도표 생성

#데이터 정렬
arrange(addr_count, desc(Freq)) %>% head() #데이터 정렬

#------------------------------------------------------------------------

#강남구 커피

coff<-read_excel("data/gangnam_coffee.xlsx")
head(coff)

coff_addr <-substr(coff$소재지전체주소, 11, 16) #소재지전체주소의 11~16번째 문자 가져오기
head(coff_addr)

coff_addr_num <- gsub("[0-9]","",coff_addr) #숫자제거
coff_addr_trim <-gsub(" ","", coff_addr_num)#공백제거
  head(coff_addr_trim)

coff_addr_count <- coff_addr_trim %>% table() %>% data.frame()
  head(coff_addr_count)

treemap(coff_addr_count, index = ".", vSize = "Freq", title = "강남구 동별 카페 분표") #도표 생성
  
arrange(coff_addr_count, desc(Freq)) %>% head() #데이터 정렬


#서울시 대기환경정보 미세먼지

sky <- read_excel("data/dustdata.xlsx")
  sky
head(sky, 10)

sky_dust <- sky %>% 
  group_by(area) %>%
  summarise(mean_finedust = mean(finedust))
sky_dust

distinct(sky %>% filter(is.na(finedust)) %>% 
           group_by(area) %>% 
           select(area,finedust) %>% 
           arrange(area)) 

sky$finedust <- ifelse(is.na(sky$finedust) == T, 0, sky$finedust)
  
  sky  
  
sky_dust <- sky %>% 
  group_by(area) %>%
  summarise(mean_finedust = mean(finedust))
  sky_dust
  
top1<- sky_dust %>% 
  arrange(desc(mean_finedust)) %>% 
  head(1)
top1
  
bot1 <- sky_dust %>% 
  arrange(mean_finedust) %>% 
  head(1)
bot1

sky_dust_12 <- rbind(top1, bot1)
  sky_dust_12

ggplot(sky_dust_12,aes(x=area,y=mean_finedust, fill = area)) + 
    geom_col() +
    geom_text(aes(label=round(mean_finedust,1)),vjust=4,fontface='bold') +
    xlab("지역") +
    ylab("평균미세먼지")


sky_dust_box <- sky %>%
  filter (area == '마포구' | area == '영등포구') %>% 
  group_by(area) %>% 
  select(area,finedust)

ggplot(sky_dust_box, aes(x = area, y = finedust, fill = area )) +
  geom_boxplot() +
  xlab("지역") +
  ylab("평균미세먼지")






#2021년 3월 24일 수요일

install.packages("plotly")
install.packages("dygraphs") #시계열 그래프 


library(plotly)
library(dygraphs)
library(xts)
library(ggiraphExtra)
library(tibble)
library(maps)
library(mapproj)
library(kormaps2014)
library(ggplot2)
library(dplyr)
library(devtools)
library(ggmap)
library(stringr)
library(leaflet)
library(ggiraphExtra)
library(tibble)
library(maps)
library(mapproj)
library(kormaps2014)
library(ggplot2)
library(dplyr)
library(devtools)
library(ggmap)
library(stringr)
library(leaflet)




p <- ggplot(data=mpg, aes(x=displ, y=hwy, col=drv))+geom_point()
  ggplotly(p)


p <- ggplot(data=diamonds, aes(x=cut, fill=clarity)) +
  geom_bar(position = "dodge")
ggplotly(p)


economics <- ggplot2::economics
head(economics)
  
eco <- xts(economics$unemploy, order.by = economics$date) #시간 속성을 주는 함수
head(eco)  
  
#그래프 생성   
dygraph(eco) #그냥 그래프
dygraph(eco) %>% dyRangeSelector() #레인지 표 추가    
  
#저축률  
eco_a <- xts(economics$psavert, order.by = economics$date) # 시기별 저축율
eco_a

#실업자수
eco_b <- xts(economics$unemploy / 1000, order.by = economics$date) #시기별 실업자수 
eco_b

#데이터 합치기
eco2 <-cbind(eco_a, eco_b)#시기별 저축율과 실업자수 합치기
  
#변수명 변경
colnames(eco2) <- c("psavert","unemploy")
head(eco2)  

#그래프 그리기
dygraph(eco2) %>% dyRangeSelector()




#성적 그래프
score <-read.csv("data/학생별과목별성적_국영수_new.csv")

score_t <-score #데이터 복사 
  score_t

score_t1 <- ggplot(data=score_t, aes( x = 이름, y = 점수, fill = 과목)) +
  geom_col(position = "dodge")  #데이터 가져와서 표 만들기 

ggplotly(score_t1)



#미세먼지 시계열


#데이터 가져와서 "dust"에 넣어
dust <- read_excel("data/dustdata.xlsx")

#데이터를 가져와서 frame화 해서 "dust1"에 넣어
dust1<-as.data.frame(read_excel('data/dustdata.xlsx'))

#"dust1"에 있는 finedust"을 na 처리해서 필터링 후에 "dust_YO"에 넣어 그리고 "area"에 용산구 넣어
dust_YO<- dust1 %>% filter(!is.na(dust1$finedust)) %>% 
  filter(area=="용산구")

#"dust1"에 있는 finedust"을 na 처리해서 필터링 후에 "dust_Y"에 넣어 그리고 "area"에 영등포구 넣어
dust_Y <-dust1 %>% filter(!is.na(dust1$finedust)) %>% 
  filter(area=="영등포구")

#???
dust_YO$yyyymmdd<-as.Date(dust_YO$yyyymmdd)
dust_Y$yyyymmdd<-as.Date(dust_Y$yyyymmdd)


dust_YO<-xts(dust_YO$finedust, order.by = dust_YO$yyyymmdd)
dust_Y<-xts(dust_Y$finedust, order.by = dust_Y$yyyymmdd)


dust_YO_Y <-cbind(dust_YO, dust_Y) #데이터 결합
head(dust_YO_Y)

colnames(dust_YO_Y)<-c('용산구','영등포구')  #결합한 데이터에 용산구와 영등포구 이름을 넣어라

dygraph(dust_YO_Y) %>% dyRangeSelector() #그래프 생성 






#t-test
#데이터 준비
mpg <- as.data.frame(ggplot2::mpg)
library(dplyr)


mpg_diff <- mpg %>% 
  select(class, cty) %>% # "cty" 줄만 가져옴 
  filter(class %in% c("compact", "suv")) # "compact", "suv"만 가지고 옴 
head(mpg_diff)  
table(mpg_diff$class)  

#t-test 실행
t.test(data = mpg_diff,       cty ~  class,        var.equal = T)  
#       데이터 지정      비교변수 ~ 비교집단   


#데이터 작업  
mpg_diff2 <- mpg %>% 
  select(fl, cty) %>% 
  filter(fl %in% c( "r", "p" ))
table(mpg_diff2$fl)

#t-test 실행 
t.test(data = mpg_diff2, cty ~ fl, var.equal = T)
  
  
  
  
#상관 관계

economics <- as.data.frame(ggplot2 :: economics)

#상관 분석
cor.test(economics$unemploy, economics$pce)  


  
#상관 행렬 _ 여러변수의 관련성, 상관계수를 행렬로
head(mtcars)  
  
car_cor <- cor(mtcars) #상관행렬 생성
round(car_cor,2) #소수점 셋째 자리에서 반올림해서 출력

  
  
  
#상관 행렬 히트 맵

install.packages("corrplot")
library(corrplot)  
  
corrplot(car_cor)

corrplot(car_cor, method = "number")


#다양한 파라미터 지정 

col <- colorRampPalette(c("#bb4444","#ee9988","#ffffff","#77aadd", "#4477aa"))

corrplot(car_cor, 
         method = "color", #색깔표현
         col = col(200), #색상 200개 한정
         type ="lower", #왼쪽 아래 행렬만 표시
         order = "hclust", #유사한 상관계수끼리 군집화
         addCoef.col = "black", #상관계수 색깔
         tl.col = "black", #변수명 색깔
         tl.srt = 45, #변수명 45도 기울임
         diag = F #대각행렬 제외
         )
  
  
  
  
#2021년 3월 25일 목요일 
  

install.packages("ggiraphExtra") #단계구분도 패키지
install.packages("ggplot")
install.packages("maps") # 지도 패키지 
install.packages("mapproj")
install.packages("stringi")
install.packages("devtools")
install.packages("ggplot2")
install.packages("ggmap")
install.packages("stringr")
install.packages("leaflet")


#devtools 여기서 바로 가져온다 
devtools::install_github("cardiomoon/kormaps2014") #:: <- 얘는 가저온다는 뜻 
devtools::install_github("dkahle/ggmap")

library(ggiraphExtra)
library(tibble)
library(maps)
library(mapproj)
library(kormaps2014)
library(ggplot2)
library(dplyr)
library(devtools)
library(ggmap)
library(stringr)
library(leaflet)
library(ggiraphExtra)
library(tibble)
library(maps)
library(mapproj)
library(kormaps2014)
library(ggplot2)
library(dplyr)
library(devtools)
library(ggmap)
library(stringr)
library(leaflet)



#데이터 준비 
str(USArrests)
  head(USArrests)

#행 이름이 지역명으로 되어 있으므로 행 이름을 "state" 로 변경
crime <- rownames_to_column(USArrests, var = "state")
  head(crime)


# "crime$state"를 소문자로 변경   
crime$state <- tolower(crime$state) 
  head(crime)
  str(crime)

state_map <- map_data("state") #데이터 프레임 형태로 변경
  head(state_map)

  
#단계구분도 만들기 
ggChoropleth(data = crime,        # 지도에 표현할 데이터
             aes(fill = Murder,   # 색깔로 표현할 변수
                 map_id = state), # 지역 기준 변수
                 map = state_map) # 지도 데이터


#단계구분도 만들기 -> 옵션( interactive = T) 추가 
ggChoropleth(data = crime,        # 지도에 표현할 데이터
             aes(fill = Murder,   # 색깔로 표현할 변수
                 map_id = state), # 지역 기준 변수
                 map = state_map, # 지도 데이터
                 interactive = T) # 인터랙티브    


head(korpop1)
head(korpop2)
head(korpop3)


#인구 구분도

str(changeCode(korpop1))

#이름 바꾸기 
korpop1 <- rename(korpop1,
                  pop = 총인구_명,
                  name = 행정구역별_읍면동)

str(changeCode(kormap1))

#단계 구분도 만들기

korpop1$name <- iconv(korpop1$name,"UTF-8", "CP949") #한글화(한글 깨짐 해결)


ggChoropleth(data = korpop1, # 지도에 표현할 데이터
             aes(fill = pop, # 색깔로 표현할 변수
                 map_id = code, # 지역 기준 변수
                 tooltip = name), # 지도 위에 표시할 지역명
             map = kormap1, # 지도 데이터
             interactive = T) # 인터랙티브


#결핵 환자수 
str(changeCode(tbc))

tbc$name <- iconv(tbc$name,"UTF-8", "CP949") #한글화(한글 깨짐 해결)

#지도 그래프 
ggChoropleth(data = tbc,            # 지도에 표현할 데이터
             aes(fill = NewPts,     # 색깔로 표현할 변수
                 map_id = code,     # 지역 기준 변수
                 tooltip = name),   # 지도 위에 표시할 지역명
             map = kormap1,         # 지도 데이터
             interactive = T)       # 인터랙티브


#구글
googleAPIkey = "AIzaSyA4cPDdFLXPxTffwKoGcIzd_-_gK60tPM0" #구글 API 등록 
register_google(googleAPIkey)

gg_seoul <- get_googlemap("seoul", zoom = 6, maptype = "roadmap")

ggmap(gg_seoul) #지도 뿌리기 

#대전역
geo_code <- enc2utf8("대전역") %>% geocode() #위도와 경도 정보 저장
geo_data <- as.numeric(geo_code)


#대전역의 위치 정보를 가져온 후 구글지도 호출 
get_googlemap(center = geo_data ,  maptype = "roadmap",zoom = 13)
geom_point(data =geo_code, aes(x=geo_code$lon, y=geo_code$lat) )


#데이터 불러오기
loc <- read.csv("data/서울_강동구_공영주차장_위경도.csv", header = T)

loc


kd <- get_map("Amsa-dong", zoom=13, maptype="roadmap")


kor.map <- ggmap(kd)+geom_point(data = loc, aes(x = LON,  y = LAT),
                                size = 3,
                                alpha = 0.7,
                                color = "red")
kor.map + geom_text(data = loc, aes(x = LON,  y = LAT+0.001, label = 주차장명), size=3)                                
ggsave("data/kd.png", dpi=500)


loc2 <- str_sub(loc$주차장명, start= -2, end= -2)
loc2

colors <-c()

for (i in 1:length(loc2)){
  
  if(loc2[i] =="구"){
    colors <- c(colors, "red")}
  
  else{
    colors <-c(colors,"blue")}
}

length(colors) #갯수 확인!!

kd <- get_map("Amsa-dong", zoom=13, maptype="roadmap")
kor.map <- ggmap(kd)+geom_point(data = loc, aes(x = LON,  y = LAT),
                                size = 3,
                                alpha = 0.7,
                                color = colors)

kor.map + geom_text(data = loc, aes(x = LON,  y = LAT+0.001, label = 주차장명), size=3)

#==============================================================================

#서울시 각 구청 위치정보

loc3 <- read.csv("data/서울시구청위치정보_new.csv", header = T)

loc3

se <- get_map("seoul", zoom=13, maptype="roadmap") #지도를 가지고 온다 

kor.map <- ggmap(se)+geom_point(data = loc3, aes(x = LON,  y = LAT),
                                size = 5,
                                alpha = 1,
                                color = "green")

#지도 그려 
kor.map + geom_text(data = loc3, aes(x = LON,  y = LAT + 0.01, label = name), size=3)


#지하철 

loc_sub <- read.csv("data/서울지하철2호선위경도정보.csv", header =T)
loc_sub2 <- read.csv("data/서울지하철3호선역위경도정보.csv", header =T)

lab_name <- c("2호선","3호선") #이름 생성 
lab_color <-c("green", "red")
center <- c(mean(loc_sub2$LON) - 0.03, mean(loc_sub2$LAT))
kor <- get_map(center, zoom=11, maptype = "roadmap") #지도를 가지고와

kor.map <- ggmap(kor)+geom_point(data=loc_sub, aes(x=LON, y=LAT), size = 3, color="green")
  geom_point(data=loc_sub2, aes(x=LON, y=LAT), size = 3, color="red")

kor.map+geom_text(data=loc_sub, aes(x=LON, y=LAT+0.005, label = 역명), size=3)
  geom_text(data=loc_sub2, aes(x=LON, y=LAT+0.005, label = 역명), size=3)

  
m <- leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng=174.768, lat=-36.852,
            popup="The birthpleace of R")
m  
  
  

#스타벅스
sb <- read.csv("data/starbucks.csv")
sb

leaflet(sb) %>% 
  setView(lng = 126.9784, lat = 37.566, zoom = 11) %>% 
  addProviderTiles("Esri.WorldTopoMap") %>% 
  addCircles(lng = ~ long, lat = ~ lat, color="#006633")
  
  
#맛집


mm <- read.csv("data/강원도으뜸음식점.csv",header=T)

head(mm)

mm2 <- get_map("gangwon", zoom = 9, maptype = "roadmap")
ggmap(mm2)

maping <- ggmap(mm2) +
  geom_point(data = mm,
             aes(x = 경도, y = 위도),
             size = 3,
             alpha = 0.5,
             color = "red")
maping

maping + geom_text(data = mm, aes(x = 경도, y = 위도+0.002, label = 업소명), size = 3)


#인터렛티브 지도
leaflet(mm) %>% 
  setView(lng  = 127.7919, lat = 37.93294, zoom = 8) %>% 
  addProviderTiles("Esri.WorldTopoMap") %>% 
  addCircles(lng = ~경도, lat = ~위도, color="#006633")

 
  

#2021년 3월 26일 금요일

library(ggiraphExtra)
library(tibble)
library(maps)
library(mapproj)
library(kormaps2014)
library(ggplot2)
library(dplyr)
library(devtools)
library(ggmap)
library(stringr)
library(leaflet)


exam <- read.csv("data/csv_exam.csv")
  exam

  
#index = 데이터 위치 또는 순서 의미 값
#indexing = 인덱스를 이용해 데이터 추출
#exam[] = 다 가져와 (내장 함수)  
  
exam[]
  
exam[1,] #1행 추출
exam[2,] #2행 추출

exam[exam$class == 1,] #class가 1인 행만 추출
exam[exam$math >= 80,] #수학점수가 80점 이상인 행만 추출 

#1반 이면서 수학 점수가 50점 이상인 행만 추출  
exam[exam$class==1 & exam$math >=50, ]

#영어점수가 90점 미만이거나 과학 점수가 50점 미만 
exam[exam$english < 90 & exam$science < 50, ]

#1반 이면서 수학 점수가 50점 이상인 행만 추출  
exam %>% filter(class == 1 & math >= 50)

#영어점수가 90점 미만이거나 과학 점수가 50점 미만 


exam[,1] #첫번째 열만 추출 
exam[,2] #두번째 열만 추출 
exam[,3] #세번째 열만 추출 


exam[,"class"] #class만 추출 
exam[,"math"] #math만 추출 
exam[,c("class", "math", "english")] #"class", "math", "english" 변수 추출 
exam[1,3] #행, 변수 모두 인덱스
exam[4,"english"] #행 인덱스, 열 변수명


#행 부등호 조건 , 열 변수명 
exam[exam$math >= 50, c("english", "science")]


#연속변수 = Numeric : 키(131,144,145) 


var1 <- c(1,2,3,1,2)
  var1

var2 <- factor(c(1,2,3,1,2))
  var2


var1 + 2 # Numeric변수로 연산
var2 + 2 # factor 변수로 연산(연산 안됨)

class(var1) #변수타입 확인

class(var2) #변수타입 확인


#factor 구성변수 확인
  levels(var1)
  levels(var2)

var3 <- c("a","b","b","c") #변수로 생성 
var3

var4 <- factor(c("a","b","b","c")) #문자로 된 factor 변수 생성 
var4


class(var3)#변수타입 확인
class(var4)#변수타입 확인


#mean = numeric 변수에만 적용 
mean(var1) #numeric
mean(var2) #factor


var2 <-as.numeric(var2) #변수 타입을 numeric으로 변경 "as." = 변경
class(var2) #변수 타입 확인 
levels(var2) #변수 범주 확인 


a <- 1
a

b<-"hello"
b

class(a)
class(b)


#데이터 프레임 만들기 
x1 <- data.frame(var1 = c(1,2,3),
                 var2 = c("a","b","c"))
x1


x2 <- matrix(c(1:12), 
             ncol = 2)
x2  

#1~20행으로 2행*5열*2차원
x3 <- array(1:20, 
            dim = c(2,5,2))
x3
  

x4 <- list(f1 = a,  #벡터
           f2 = x1, #데이터 프레임
           f3 = x2, #매트릭스
           f4 = x3) #어레이

x4  

class(x4) #데이터 타입 확인 
  
  

mpg <- ggplot2::mpg
x <- boxplot(mpg$cty)  
x  
  
x$stats[,1]    #요약 통계랑 추출 
x$stats[,1][3] #중앙값 추출
x$stats[,1][2] #1분위수 추출 
  
  
#===============================================================================
station_data1 <- read.csv("data/지하철역별_주소_전화번호.csv")
str(station_data1)

googleAPIkey = "AIzaSyA4cPDdFLXPxTffwKoGcIzd_-_gK60tPM0" #API가져오기
register_google(googleAPIkey) #API등록 

station_code <- as.character(station_data1$"구주소") #구주소를 character 타입으로 바꿈 
  station_code

station_code <- geocode(station_code) #위도와 경도를 가지고오기 위한 함수
  station_code


station_code_final <- cbind(station_data1, station_code) #두 데이터 결합
  head(station_code_final)

#아파트 실거래가

apart_data <- read.csv("data/아파트_실거래가.csv")
  head(apart_data)

apart_data$전용면적 = round(apart_data$전용면적)
  head(apart_data)

count(apart_data, 전용면적) %>% arrange(desc(n))


apart_data_85 <-subset(apart_data, 전용면적 == "85")
  head(apart_data_85)

apart_data_85$거래금액 <- gsub(",","",apart_data_85$거래금액)
  head(apart_data_85)

apart_data_85_cost <- aggregate(as.integer(거래금액)~단지명,apart_data_85, mean)
  head(apart_data_85_cost)

#"거래금액" = "as.integer(거래금액)"으로 변경 
apart_data_85_cost <-rename(apart_data_85_cost,"거래금액" = "as.integer(거래금액)")
  head(apart_data_85_cost)


apart_data_85 <- apart_data_85[!duolicated(apart_data_85$단지명), ]
  head(apart_data_85)

  
apart_data_85 <- left_join(apart_data_85,apart_data_85_cost, by="단지명")
  head(apart_data_85)

apart_data_85 <-apart_data_85 %>%
  select("단지명","시군구","번지","전용면적","거래금액.y")
  head(apart_data_85)


apart_data_85<-rename(apart_data_85,"거래금액"="거래금액.y")
  head(apart_data_85)

#"시군구"와 "번지" 열을 결합
apart_address <- paste(apart_data_85$"시군구",apart_data_85$"번지")
  head(apart_data_85)

  paste("i","love","you")
  paste("i","love","you", spe="-")
  paste("i","love","you", spe="")
  paste0("i","love","you")

  

#"시군구"와 "번지" 열을 결합후 데이터프레임화
apart_address <- paste(apart_data_85$"시군구",apart_data_85$"번지") %>% 
data.frame()    

head(apart_address)

apart_address<-rename(apart_address,"주소"=".")
head(apart_address)


#character로 변환하여 위도와 경도 가져옴 
apart_address_code <-as.character(apart_address$"주소") %>% 
  geocode()

apart_code_final <- cbind(apart_data_85, apart_address, apart_address_code) %>% 
  select("단지명","전용면적","거래금액","주소", lon, lat)
apart_code_final


#홍대입구역
hongdea_map <- get_googlemap("hongdea station", maptype = "roadmap", zoom = 15)
hongdea_map

#홍대입구영 지도에 지하철 정보 및 아파트 정보 일괄 표시 

ggmap(hongdea_map)+
  geom_point(data = station_code_final, aes(x=lon , y=lat), colour = "red", size=3)+
  geom_text(data = station_code_final, aes(label=역명, vjust= -1))+
  geom_point(data = station_code_final, aes(x=lon, y=lat))+
  geom_text(data = station_code_final, aes(label = 단지명, vjust= -1))+
  geom_text(data = station_code_final, aes(label = 거래금액, vjust= -1))

#=================================================================================

#5호선 지하철역 주변 강서구 아파트 가격 알아보기(25평) 

googleAPIkey = "AIzaSyA4cPDdFLXPxTffwKoGcIzd_-_gK60tPM0" #API가져오기
register_google(googleAPIkey) #API등록 


#5호선 데이터   
line5 <-read_excel("data/line5.xlsx")
line5
str(line5)

line_code <- as.character(line5$"지번주소")  #지번주소를 character 타입으로 바꿈 
line_code <- geocode(line_code) #character로 변환하여 위도와 경도 가져옴
line_code

line_code_final <- cbind(line5, line_code)

head(line_code_final)



#아파트 데이터
apt <- read_excel("data/apartment.xlsx", skip = 15)
  apt 
  head(apt)
  str(apt)

apt_data <- rename(apt, "전용면적" = "전용면적(㎡)")
  head(apt_data)

table(is.na(apt$전용면적))
  apt_data$전용면적 <- as.numeric(apt_data$전용면적)

apt_data$전용면적 = round(apt_data$전용면적)
  head(apt_data)

count(apt_data,전용면적) %>% 
  arrange(desc(n))

apt_data_85 <- subset(apt_data, 전용면적 == "85")
head(apt_data)

apt_data_85 <- rename(apt_data_85, "거래금액" = "거래금액(만원)")
head(apt_data_85)

apt_data_85$거래금액 <- gsub(",", "", apt_data_85$거래금액)
head(apt_data_85)

apt_data_85_cost <- aggregate(as.integer(거래금액) ~단지명, apt_data_85, mean)
head(apt_data_85_cost)

apt_data_85_cost <- rename(apt_data_85_cost, "거래금액" = "as.integer(거래금액)")
head(apt_data_85_cost)


apt_data_85 <- apt_data_85[!duplicated(apt_data_85$단지명),]
head(apt_data_85)


apt_data_85 <- left_join(apt_data_85, apt_data_85_cost, by = "단지명")
head(apt_data_85)

apt_data_85 <- apt_data_85 %>% 
  select("단지명", "시군구", "번지", "전용면적", "거래금액.x")
head(apt_data_85)

apt_data_85 <- rename(apt_data_85, "거래금액" = "거래금액.x")
head(apt_data_85)

apt_address <- paste(apt_data_85$"시군구", apt_data_85$"번지")
head(apt_address)

apt_address <- paste(apt_data_85$"시군구", apt_data_85$"번지") %>% 
  data.frame()
head(apt_address)

apt_address <- rename(apt_address, "주소" = ".")
head(apt_address)

apt_address_code <- as.character(apt_address$"주소") %>% 
  geocode()


apt_code_final <- cbind(apt_data_85, apt_address, apt_address_code) %>% 
  select("단지명", "전용면적", "거래금액", "주소", lon, lat)
head(apt_code_final)

Gangseo_map <- get_googlemap("Gangseo-gu", maptype = "roadmap", zoom = 15)


#성의없는 지도
ggmap(Gangseo_map) + 
  geom_point(data = line_code_final, aes(x= lon, y= lat), colour = "red", size = 3) +
  geom_text(data = line_code_final, aes(label=역명, vjust=-1)) +
  geom_point(data = apt_code_final, aes(x = lon, y= lat)) +
  geom_text(data = apt_code_final, aes(label = 단지명, vjust = -1)) +
  geom_text(data = apt_code_final, aes(label= 거래금액, vjust = 1))


#이쁜지도
leaflet(apt_code_final) %>% 
  setView(lng = 126.825472, lat = 37.560142, zoom = 12) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addMarkers(~lon, ~lat, popup = ~거래금액, label = ~단지명)





















































  

  
  
  
  
  
  
  
  
  
  
  
  





