library(dplyr)
exam<-read.csv("data/csv_exam.csv")
exam


exam %>% filter(class==1) #클래스 1만 추출해라

# '%>%' (파이프 연산자):단축키 = '컨트롤+시프트+M'
# '<-' : 얘는 데이터를 '넣어라'임
# '!' : 얘는 not(아니다)임

exam %>% filter(class==2)
exam %>% filter(class==4)

exam %>% filter(class !=1) #1반이 아닌것을 추출해라
exam %>% filter(class !=3)


exam %>% filter(math <50) #수학이 50점 미만
exam %>% filter(english >=80) #영어가 80점 이상
exam %>% filter(english <=70) #영어가 70점 이하
exam %>% filter(english <=80) #영어가 80점 이하

exam %>% filter(class ==1 & math >=50) #조건 두가지, 1반이면서 수학이 50점 이상 
exam %>% filter(class ==2 & english >=80) #조건 두가지, 2반이면서 영어가 80점 이상
exam %>% filter(math>=80 & science>=80) #조건 두가지, 수학,과학 80점 이상

exam %>% filter(math >=90 | english >=90) 

#'|' : 얘는 'or' 둘중 하나 만족하면 뭐 해라


exam %>% filter(english < 90 | science < 50) #영어가 90 이상이거나 과학이 50이거나
exam %>% filter(class==1 | class==3 | class==5)# 클래스가 1,2,5반에 해당되면 추출 

exam %>% filter(class ==1 & math >=80)
exam %>% filter(class==1)

exam %>% filter(class==1 & english >=80)

exam %>% filter(class %in% c(1,3,5)) #1,3,5반 추출
#'%in%' 얘는 안에 있는거 워쩌구 하라는거(매칭확인)

#1,2,5반이면서 수학,과학 80 이상 

exam %>% filter(class %in% c(1,3,5) & math >=80 & science>=80)


class1<- exam %>% filter(class==1)
class2<- exam %>% filter(class==2)

mean(class1$math) #1반의 수학평균
mean(class2$math) #2반의 수학평균

mean(class1$english)
mean(class2$english)


mpg<-as.data.frame(ggplot2::mpg)
#Q1
mpg1<- mpg %>% filter(displ <=4 )
mpg2<- mpg %>% filter(displ >=5 )

mean(mpg1$hwy)
mean(mpg2$hwy)

#Q2
mpg_audi<-mpg %>% filter(manufacturer == "audi") #audi 추출

mpg_toyota<-mpg %>% filter(manufacturer == "toyota") #toyota 추출

#Q3
mpg_new <- mpg %>% filter(manufacturer%in% C("chevrot"))

#filter = 행만 가져옴 
#select = 열만 가져옴

exam %>% select(math) #math만 추출
exam %>% select(english) #english만 추출
exam %>% select(class, math, english)


exam %>% select(-math) #math 빼고 가져옴
exam %>% select(-math, -english) #math, english 빼고 가져옴

exam %>% 
  filter(class==1) %>% #class 1 가져옴
  select(english) # 영어 가져옴


exam %>% 
  select(id,math) %>% 
  head(8)

#1
mpg1 <- mpg %>% select(class,cty)

#2


exam %>% arrange(math) #math 값 오름차순 정렬
exam %>% arrange(desc(math))#math 값 내림차순 정렬
exam %>% arrange(class, math) #클래스로 오름차순 정렬 후 매스 오름차순 정렬

#1
exam %>% arrange(desc(class), desc(math)) #클래스 내림 매스 내림

#2
exam %>% arrange(desc(class), desc(math), desc(english))

#3
exam %>% 
  filter(class==4) %>% #4반 한정
  arrange(desc(math)) #수학 내림차순


exam %>% 
  mutate(total = math + science + english) %>%  #총합 변수 추가
  head #일부 추출
                                                #'mutate' : 변수 한방에 추가하는 함수

exam %>% 
  mutate(total = math+english+science, # 총합 변수 추가  
         mean = (math+english+science)/3 ) %>% # 총 평균 추가
  head #일부 추출


exam %>% 
  mutate(test = ifelse(science >= 60, "pass", "fail")) %>% 
  head


exam %>% 
  mutate(test1 = ifelse(math >= 90, "A", 
                        ifelse(math >= 80, "B", "c"))) %>% 
  head(10)


exam %>% 
  mutate(total = math + english + science) %>% #총합 변수 추가
  arrange(total) %>% # 총합 변수 기준 정렬
  head(10)



mpg11<-mpg %>% select(hwy,cty)

mpg_new <- mpg11 %>% 
  mutate(hap = cty + hwy) %>%
  mutate(pyg = hap/2) %>%
  arrange(desc(pyg) %>% 
  head(3)

mpg_new


exam %>% 
  summarise(mean_math = mean(math)) #math 평균 산출

exam %>% 
  group_by(class) %>% #클래스별로 분리
  summarise(mean_math = mean(math)) #매스 평균산출 


exam %>% 
  group_by(class) %>% # 클래스별 분리
  summarise(mean_math = mean(math), #매스 평균
            sum_math = sum(math),#매스 합계
            median_math = median(math), #매스 중앙값
            n = n())#학생수

mpg %>% 
  group_by(manufacturer, drv) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  head(10)


library(readxl)

#1
sample <-read_excel("data/sample1.xlsx")

#2
sample %>% select(ID)

#3
sample %>% select(ID, AREA, Y17_CNT)

#4
sample %>% select(-AREA, -Y17_CNT)

#5
sample %>% 
  filter(AREA == "서울")

#6
sample %>% 
  filter(AREA=="서울" & Y17_CNT >= 10)

#7 시댕








test1 <- data.frame(id = seq(1,5, by = 1),
                    midterm = c(60, 80, 70, 90, 85)
                    )

test2 <-data.frame(id = seq(1,5, by = 1),
                   final = c(70, 83, 65, 95, 80)
                    )
  

total <- left_join(test1,test2, by = "id")
  


name <- data.frame(class =seq(1,5, by = 1),
                   teacher = c("kim", "lee", "park", "choi", "jung"))

exam_new <- left_join(exam, name, by = "class")

exam_new

group_a <- data.frame(id = seq(1,5, by=1),
                      test = c(60, 80, 70, 90, 85))

group_b <- data.frame(id = seq(6,10, by=1),
                      test = c(70, 83, 65, 95, 80))


group_a
group_b


group_all <- bind_rows(group_a,group_b) #데이터 합처서 할당

group_all

m_history <- read_excel("data/sample2_m_history.xlsx")

  m_history

f_history <- read_excel("data/sample3_f_history.xlsx")

  f_history

exdata_bindjoin <- bind_rows(m_history, f_history) #세로결합 바인드 함수

  exdata_bindjoin



jeju17 <- read_excel("data/sample4_y17_history.xlsx")  

jeju16 <- read_excel("data/sample5_y16_history.xlsx")  

jeju17
jeju16
  

#left_join
y17_16 <- left_join(jeju17, jeju16, by="ID")
  y17_16


#inner_join
y17_16_inner <- inner_join(jeju17, jeju16, by="ID")
  y17_16_inner


#full_join
y17_16_full<-full_join(jeju17, jeju16, by="ID")
  y17_16_full

  
df <- data.frame(sex = c("M", "F", NA, "M","F"),
                score = c(5, 4, 3, 4, NA))
df

is.na(df)#결측치 확인

table(is.na(df)) #테이블에 결측치 확인

table(is.na(df$sex)) #성별 결측치
table(is.na(df$score)) # 스코어 결측치

mean(df$score) #평균산출
sum(df$score) #합계산출


df %>% filter(is.na(score)) #NA 데이터만 출력
df %>% filter(!is.na(score))#NA 제외 출력

df_nomiss <- df %>% filter(!is.na(score)) #결측치 제거
mean(df_nomiss$score) # 스코어 평균 산출
sum(df_nomiss$score)

#스코어, 성별 결측치 제외
df_nomiss<- df %>% filter(!is.na(score) & !is.na(sex))
  df_nomiss


df_nomiss2 <-na.omit(df) #모든 변수의 결측치 제거 
  df_nomiss2

#1
data1 <- read.csv("data/2013년_프로야구선수_성적.csv")
data1

#2
data_a <- data1 %>% filter(경기 >= 120 )
  data_a

#3
data_b <- data1 %>% filter(경기 >= 120 & 득점 >= 80 )
  data_b

#4
data_c <- data1 %>% filter(포지션 %in% c("1루수","3루수"))
  data_c

#5
data1 %>% select(선수명, 포지션, 팀)

#6
data1 %>% select(순위:타수)

#7
data1 %>% select(-홈런, -타점, -도루)

#8
data1 %>% select( filter(타수>400), 선수명, 팀, 경기, 타수)























