install.packages("foreign") #패키지 설치
library(foreign) #SPSS 파일 로드
library(dplyr)   #전처리
library(ggplot2) #시각화
library(readxl)  #엑셀 파일 불러오기

raw_welfare <-read.spss(file = "Data/Koweps_hpc10_2015_beta1.sav", to.data.frame = T) #데이터 불러오기
  raw_welfare

welfare <- raw_welfare #복사
  welfare


  
head(welfare)
tail(welfare)
View(welfare) # 데이터를엑셀처럼 보여주는거
dim(welfare)  # 행 열 갯수 보여주는거 
str(welfare)
summary(welfare)

welfare <- rename(welfare,
                  sex = h10_g3,          #성별
                  birth = h10_g4,        #태어난 연도
                  marriage = h10_g10,    #혼인상태
                  religion = h10_g11,    #종교
                  income = p1002_8aq1,   #월급
                  code_job = h10_eco9,   #직종코드
                  code_region = h10_reg7 #지역코드
                  )

#성별에 따륹 월급 차이

class(welfare$sex) #변수검토
table(welfare$sex) #데이터 빈도 확인

welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex) #결측치 처리
  table(is.na(welfare$sex)) #결측치 확인 

welfare$sex <-ifelse(welfare$sex == 1, "male", "female") #성별 항목 이름 부여 (welfare$sex가 1이면 남자 아니면 여자)
  table(welfare$sex)

qplot(welfare$sex) #성별 그래프


class(welfare$income) #변수 검토
summary(welfare$income) #변수 요약

qplot(welfare$income) #소득 그래프
qplot(welfare$income) + xlim(0, 1000) #그래프에 x축 한정 

#전처리

#이상치 확인
summary(welfare$income)

#이상치 결측 처리 -> 범위: 1~9999
welfare$income <- ifelse(welfare$income %in% c(0,9999), NA, welfare$income) 

#결측치 확인
table(is.na(welfare$income))

#성별에 따른 월급차이 

sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))

sex_income

ggplot(data = sex_income, aes(x=sex, y=mean_income)) + geom_col() #그래프 


#변수 검토
class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth) #그래프 출력
table(is.na(welfare$birth)) #결측치 확인

#이상 결측치 확인
welfare$birth <- ifelse(welfare$birth==9999, NA, welfare$birth) #9999면 생일에 넣고 아니면 냅둬
table(is.na(welfare$birth))


#파생변수 만들기

welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)

age_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))

head(age_income)

ggplot(data = age_income, aes(x = age,  y = mean_income)) + geom_line()


#연령대에 따른 월급차이

#전처리

welfare <- welfare %>% 
  mutate(ageg = ifelse(age < 30, "young",
                        ifelse(age <= 59, "middle", "old")))
table(welfare$ageg)

qplot(welfare$ageg)

#연령대별 월급 평균표 만들기
ageg_income <- welfare %>% #welfare에서 
  filter(!is.na(income)) %>% #인컴의 결측값을 제끼고
  group_by(ageg) %>% #ageg에 넣어라 
  summarise(mean_income = mean(income)) #그것 인컴의 평균을 구하고 mean_income에 넣고 요약해라


ageg_income
  
#막대정렬 : 초,중,노년 나이순
ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) + #그래프 생성 
  geom_col()+
  scale_x_discrete(limits = c("young","middle", "old" )) #나이별 정렬


#연령대 및 성별 원급 차이

#전처리

sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg,sex) %>% #연령, 성별
  summarise(mean_income = mean(income))

sex_income

ggplot(data = sex_income,aes(x = ageg , y = mean_income, fill = sex))+
  geom_col(position = "dodge")+
  scale_x_discrete(limits = c("young","middle", "old" )) #나이별 정렬

#나이 및 성별 월급차이

sex_age <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age,sex) %>% 
  summarise(mean_income = mean(income))
head(sex_age)

ggplot(data = sex_age, aes(x=age, y=mean_income, col=sex)) + geom_line() #그래프 생성


#직업별 월급차이

#변수 검토
class(welfare$code_job)
table(welfare$code_job)


#전처리
library(readxl)

#불러오기
list_job <- read_excel("Data/Koweps_Codebook.xlsx", col_names = T, sheet = 2)
  head(list_job)
  
  dim(list_job)

  
#welfare에 직업명 결합
  
  
  welfare <-left_join(welfare, list_job, id = "code_job")

  welfare %>% 
    filter(!is.na(code_job)) %>% 
    select(code_job, job) %>% #칼럼만 가지고 오는거
    head(10)

  
job_income <-welfare %>%
  filter(!is.na(job)& !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))
head(job_income)  

#상위 10개 추출

top10 <- job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)

top10

#그래프 만들기 

ggplot(data = top10, aes(reorder(job, mean_income), y = mean_income))+
  geom_col() +
  coord_flip() #90도 회전

bottom10 <- job_income %>% 
  arrange(mean_income) %>% 
  head(10)

bottom10

#그래프 만들기
ggplot(data = bottom10, aes(x = reorder(job, -mean_income),
                            y = mean_income))+
  geom_col()+
  coord_flip()+
  ylim(0,850)


#성별 직업빈도


job_male <- welfare %>% 
  filter(!is.na(job) & sex=="male") %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)

job_male

job_female <- welfare %>% 
  filter(!is.na(job) & sex=="female") %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)

job_female

#남자 그래프
ggplot(data = job_male, aes(x=reorder(job,n), y=n))+
  geom_col()+
  coord_flip()


#여자 그래프
ggplot(data = job_female, aes(x=reorder(job,n), y=n))+
  geom_col()+
  coord_flip()


#종교 유무에 따른 이혼율
#religion


#변수 검토

class(welfare$religion)
table(welfare$religion)



#종교 유무 이름 부여
welfare$religion <- ifelse(welfare$religion == 1, "yes", "no")
table(welfare$religion)

qplot(welfare$religion) #빈도 그래프


#혼인 상태 확인 


#변수 검토
class(welfare$marriage)
table(welfare$marriage)


#이혼여부 전처리 


welfare$group_marriage <- ifelse(welfare$marriage == 1, "marriage",
                          ifelse(welfare$marriage == 3, "divorce", NA))
table(welfare$group_marriage)
table(is.na(welfare$group_marriage))

qplot(welfare$group_marriage)


#종교 유무에 따른 이혼율 표 만들기 

religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100,1))

religion_marriage


divorce <- religion_marriage %>% 
  filter(group_marriage == "divorce") %>% 
  select(religion, pct)

divorce

#그래프
ggplot(data = divorce, aes(religion, y=pct)) + geom_col()


#1 결혼 유무와 소득 관계

welfare$group_marriage <- ifelse(welfare$marriage == 1, "yes",
                          ifelse(welfare$marriage == 5, "no", NA))

marriage_income <-welfare %>% 
  filter(!is.na(group_marriage) & !is.na(income)) %>% 
  group_by(group_marriag) %>% 
  summarise(mean_income = mean(income))

marriage_income






#2 결혼 유무와 남여 소득의 관계


group_marriage_don <- ifelse(welfare$sex == 1, "female",
                      ifelse(welfare$sex == 2, "male", NA))



group_marriage
group_marriage_don





















