english <- c(90,80,60,70) #?˜?–´? ?ˆ˜
math <- c(50,60,100,20) #?ˆ˜?•™? ?ˆ˜

df_midterm <- data.frame(english, math)
df_midterm

class <- c(1,1,2,2) #ë°?

df_midterm <- data.frame(english, math, class)

df_midterm

mean(df_midterm$english) #df_midterm?˜englishë¡? ?‰ê·? ?‚°ì¶?

mean(df_midterm$math) #df_midterm?˜ mathë¡? ?‰ê·? ?‚°ì¶? *'$'<- ?–˜?Š” ?•ˆ?— ?ˆ?‹¤?Š” ?‘œ?‹œ

mean_eng <-mean(df_midterm$english) #english?˜ ?‰ê· ì„ mean_eng?— ?•Œ? ¤?„£?Œ
  mean_eng

mean_math <-mean(df_midterm$math)  #math?˜ ?‰ê· ì„ mean_math?— ?•Œ? ¤?„£?Œ
  mean_math

df_midterm <- data.frame(     
              english = c(90,80,60,70),
              math = c(50,60,100,20),
              class = c(1,1,2,2)
                                    )
  

#?˜¼??„œ ?•´ë³´ê¸°

sales <- data.frame( fruit = c("?‚¬ê³?","?”¸ê¸?","?ˆ˜ë°?"),
                     price = c(1800, 1500, 3000),
                     vol = c(24, 38, 13))

sales



#Qê°€ê²©í‰ê·?,?Œë§¤ëŸ‰ ?‰ê·?


mean_price <- mean(sales$price)
  mean_price

mean_vol <- mean(sales$vol)
   mean_vol

install.packages("readxl")   
library(readxl)   

data_exam <- read_excel("c:/Work_r/Data/excel_exam.xlsx") #? ˆ??€ê²½ë¡œ(ë¹¡ì‹  ê²½ë¡œ)

data_exam
   

##### ?˜„?¬ ?””? ‰?† ë¦? ?œ„ì¹? ì¶œë ¥ #####
setwd("c:/Work_R")
getwd()   #?‚´ ?‘?—… ?””?…?† ë¦¬ë?? ê°€? ¸??€?¼ 


mean(data_exam$english)   
mean(data_exam$science)   

m_eng <-mean(data_exam$english) 
  m_eng
m_sci <-mean(data_exam$science)
  m_sci
   
df_exam_novar <- read_excel("Data/excel_exam_novar.xlsx", col_names=F) # ì²«í–‰ ë³€?ˆ˜ëª…ìœ¼ë¡? ?¸?‹ F=ë°˜ë“œ?‹œ ??€ë¬¸ì
df_exam_novar
   
df_exam_sheet <- read_excel("data/excel_exam_sheet.xlsx", sheet = 3) #?—‘??€?˜ 3ë²? ?‹œ?Š¸ë¥? ?—´?–´?¼ 
df_exam_sheet
   
#?—‘??€ ?—´?•Œ?Š” ?Š˜ "" ?´ê±? ?“°?Š”ê±°ì„



df_csv_exam <- read.csv("data/csv_exam.csv")
df_csv_exam 

df_csv_exam <- read.csv("data/csv_exam.csv", stringsAsFactors = F) # stringsAsFactors = ë²”ì£¼?˜• 1 or 2 

df_csv_exam


name <- data.frame(class = c(seq(1,5, by=1),
                   teacher= c("kim", "lee", "park", "choi", "jung"))
name


name1<- data.frame(class = c(seq(1,5, by=1),
                   teacher= c("kim", "lee", "park", "choi", "jung"),
                   stringsAsFactors = F )
name1


df_midterm <- data.frame(english = c(90,80,60,70),
                         math = c(50,60,100,20),
                         class = c(1,1,2,2))

df_midterm

write.csv(df_midterm, file = "df_midterm.csv")


exam <- read.csv("data/csv_exam.csv")
exam

head(exam) #?•?—?„œë¶€?„° 6?–‰ê¹Œì?€ ë¶ˆëŸ¬?˜´

head(exam, 10) #?•?—?„œë¶€?„° 10?–‰ê¹Œì?€ ë¶ˆëŸ¬?˜´

tail(exam) #?’¤?—?„œë¶€?„° 6?–‰ê¹Œì?€ ë¶ˆëŸ¬?˜´

tail(exam, 10) #?’¤?—?„œë¶€?„° 10?–‰ê¹Œì?€ ë¶ˆëŸ¬?˜´

View(exam) #?—‘??€ ì¶œë ¥

dim(exam) #?–‰,?—´ ì¶œë ¥

str(exam) #?°?´?„° ?†?„± ?™•?¸

summary(exam) #?š”?•½ ?†µê³„ëŸ‰ ì¶œë ¥

mpg<- as.data.frame(ggplot2::mpg) #?Œ¨?‚¤ì§€ ggpl2?˜ mpg ?°?´?„°ë¡? ?”„? ˆ?„?œ¼ë¡? ê°€? ¸?˜´
                                  #as.data.frame=?°?´?„° ?†?„±?„ ?°?´?„° ?˜•?ƒœë¡? ë°”ê¿ˆ 
mpg

View(mpg)

head(mpg, 10)

tail(mpg, 10)

str(mpg)

dim(mpg)

#int : ? •?ˆ˜
#num : ?†Œ?ˆ˜? 

summary(mpg)

install.packages("dplyr") #ë³€?ˆ˜ëª? ë°”ê¾¸?Š” ?Œ¨?‚¤ì§€
library(dplyr)

df_raw <- data.frame(var1 = c(1,2,1),
                     var2 = c(2,3,2))
df_raw

df_new <- df_raw

df_new

df_new <-rename(df_new, v2=var2) #df_new ?•ˆ?— ?ˆ?Š” var2ë¥? v2ë¡? ë°”ê¾¼?‹¤

df_new

df_raw

df_new



#?˜¼??„œ ?•´ë³´ê¸°


df_mpg11<-mpg
mpg

  df_mpg11

df_mpg11 <- rename(df_mpg11, city = cty, highway = hwy)

  df_mpg11

head(df_mpg11)


#?ŒŒ?ƒë³€?ˆ˜:?•„?š”?— ?”°?¼?—´(ì¹¼ëŸ¼) ?•˜?‚˜ ?” ë§Œë“¬

df<-data.frame(var1 = c(4,3,8),
               var2 = c(2,6,1))
  df

df$var_sum<-df$var1 + df$var2 #var_sum ?ŒŒ?ƒë³€?ˆ˜ ?ƒ?„±:?—´(ì¹¼ëŸ¼) ì¶”ê?€ + ?—°?‚°
  df

df$var_mean <- (df$var1 + df$var2)/2
  df

mpg$total <- (mpg$cty + mpg$hwy)/2
  mpg

head(mpg)

mean(mpg$total)

summary(mpg$total)

hist(mpg$total) #?ˆ?†Œ?† ê·¸ë¨

#ì¡°ê±´ë¬?

ifelse(mpg$total >=20, "pass", "fail")

mpg$test <- ifelse(mpg$total >=20, "pass", "fail")
  mpg

head(mpg,20) #?°?´?„° ?™•?¸

table(mpg$test) #?—°ë¹„í•©ê²©ë¹ˆ?„?ƒ?„±

library(ggplot2) #ggplot2 ë¡œë“œ
qplot(mpg$test) # ?—°ë¹? ?•©ê²? ë¹ˆë„ ë§‰ë?€ ê·¸ë˜?”„ ?ƒ?„±


mpg$grade <- ifelse(mpg$total >= 30, "A",
                    ifelse(mpg$total >= 20, "B", "C"))

head(mpg)

# ?˜¼? ?•´ë³´ê¸° total?„ ê¸°ì?€?œ¼ë¡? A,B,C,D?“±ê¸‰ìœ¼ë¡? ë¶€?—¬
#total >= 30 A, >= 20 B, >= 15, C, Dë³€?ˆ˜ ë¶€?—¬


mpg$grade1<- ifelse(mpg$total >= 30, "A",
                    ifelse(mpg$total >= 20,"B",
                           ifelse(mpg$total >= 15,"C","D" )))
head(mpg,20)

table(mpg$grade1) #ê·¸ë ˆ?´?“œ ë¹ˆë„ ?™•?¸

qplot(mpg$grade1) #ê·¸ë˜?”„ ?ƒ?„±


table(mpg$grade)




mpg$grade2<- ifelse(mpg$total >= 30, "A",
                    ifelse(mpg$total >= 25,"B",
                           ifelse(mpg$total >= 20,"C","D" )))
head(mpg,10)
table(mpg$grade2)
qplot(mpg$grade2)
dim(mpg)

#ë¶„ì„?„? „

install.packages("ggplor2")
library(ggplot2)

midwest <- as.data.frame(ggplot2::midwest)

  midwest

  head(midwest,10)

midwest1<-midwest

midwest1 <-rename(midwest1, total = poptotal, asian = popasian)

  head(midwest1,10)


midwest1$per <-(midwest1$asian / midwest1$total)*100

  head(midwest1,10)
  hist(midwest1$per)

midwest1$mean_per <- mean(midwest1$per)
     
  mean_per

midwest1$popop <- ifelse(midwest1$per > midwest1$mean_per,"large", "small")
  

head(midwest1, 10)


#ë¬¸ì œ?•´ê²?

excel_exam <- read_excel("c:/Work_r/Data/excel_exam.xlsx") #? ˆ??€ê²½ë¡œ(ë¹¡ì‹  ê²½ë¡œ)
excel_exam1 <- excel_exam
  excel_exam1 #?™•?¸

#1. ?ˆ˜?•™ ?˜?–´ ê³¼í•™ ?•©ê³? ì¶œë ¥
head(excel_exam1,10)
excel_exam1 <- rename(excel_exam1, ma = math, en = english, sci = science)
 excel_exam1 #?™•?¸

excel_exam1$class_sum <- excel_exam1$ma + excel_exam1$en + excel_exam1$sci
 excel_exam1 #?™•?¸


  
#2. ?ˆ˜?•™ ?˜?–´ ê³¼í•™ ?‰ê·? ì¶œë ¥
 
excel_exam1$class_mean_ma <- mean(excel_exam1$ma)
  excel_exam1 #?ˆ˜?•™

excel_exam1$class_mean_en <- mean(excel_exam1$en)
  excel_exam1 #?˜?–´

excel_exam1$class_mean_sci <- mean(excel_exam1$sci)
  excel_exam1 #ê³¼í•™

excel_exam1$class_mean_total <- mean(class_mean_ma + class_mean_en + class_mean_sci)/3
  excel_exam1 #ì´? ?‰ê·?

  
#3. ?‰ê·? 60?  ?´?ƒ = pass ?‚˜ë¨¸ì?€ = fail, ?ŒŒ?ƒë³€?ˆ˜

  excel_exam1$test1 <- ifelse(excel_exam1$class_sum/3 > 60,"pass", "fail" )  
  excel_exam1  
  

#4. 3?˜ ê²°ê³¼ ë¹ˆë„ ê·¸ë˜?”„

  qplot(excel_exam1$test1)


#5. ?‰ê·? 80?  ?´?ƒ A 70?´?ƒ B, ?‚˜ë¨¸ì?€ C ë¶€?—¬ ?ŒŒ?ƒë³€?ˆ˜ 

  excel_exam1$test2<-ifelse(excel_exam1$class_sum/3 > 80,"A",
                            ifelse(excel_exam1$class_sum/3 > 70,"B","C" ))
  excel_exam1

  
head(test2, 10)
tail(test2, 7)
qplot(test2)

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  



