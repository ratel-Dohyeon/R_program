english <- c(90,80,60,70) #??΄? ?
math <- c(50,60,100,20) #??? ?

df_midterm <- data.frame(english, math)
df_midterm

class <- c(1,1,2,2) #λ°?

df_midterm <- data.frame(english, math, class)

df_midterm

mean(df_midterm$english) #df_midterm?englishλ‘? ?κ·? ?°μΆ?

mean(df_midterm$math) #df_midterm? mathλ‘? ?κ·? ?°μΆ? *'$'<- ?? ?? ??€? ??

mean_eng <-mean(df_midterm$english) #english? ?κ· μ mean_eng? ?? €?£?
  mean_eng

mean_math <-mean(df_midterm$math)  #math? ?κ· μ mean_math? ?? €?£?
  mean_math

df_midterm <- data.frame(     
              english = c(90,80,60,70),
              math = c(50,60,100,20),
              class = c(1,1,2,2)
                                    )
  

#?Ό?? ?΄λ³΄κΈ°

sales <- data.frame( fruit = c("?¬κ³?","?ΈκΈ?","?λ°?"),
                     price = c(1800, 1500, 3000),
                     vol = c(24, 38, 13))

sales



#Qκ°κ²©νκ·?,?λ§€λ ?κ·?


mean_price <- mean(sales$price)
  mean_price

mean_vol <- mean(sales$vol)
   mean_vol

install.packages("readxl")   
library(readxl)   

data_exam <- read_excel("c:/Work_r/Data/excel_exam.xlsx") #? ??κ²½λ‘(λΉ‘μ  κ²½λ‘)

data_exam
   

##### ??¬ ?? ? λ¦? ?μΉ? μΆλ ₯ #####
setwd("c:/Work_R")
getwd()   #?΄ ?? ??? λ¦¬λ?? κ°? Έ???Ό 


mean(data_exam$english)   
mean(data_exam$science)   

m_eng <-mean(data_exam$english) 
  m_eng
m_sci <-mean(data_exam$science)
  m_sci
   
df_exam_novar <- read_excel("Data/excel_exam_novar.xlsx", col_names=F) # μ²«ν λ³?λͺμΌλ‘? ?Έ? F=λ°λ? ??λ¬Έμ
df_exam_novar
   
df_exam_sheet <- read_excel("data/excel_exam_sheet.xlsx", sheet = 3) #???? 3λ²? ??Έλ₯? ?΄?΄?Ό 
df_exam_sheet
   
#??? ?΄?? ? "" ?΄κ±? ?°?κ±°μ



df_csv_exam <- read.csv("data/csv_exam.csv")
df_csv_exam 

df_csv_exam <- read.csv("data/csv_exam.csv", stringsAsFactors = F) # stringsAsFactors = λ²μ£Ό? 1 or 2 

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

head(exam) #???λΆ?° 6?κΉμ? λΆλ¬?΄

head(exam, 10) #???λΆ?° 10?κΉμ? λΆλ¬?΄

tail(exam) #?€??λΆ?° 6?κΉμ? λΆλ¬?΄

tail(exam, 10) #?€??λΆ?° 10?κΉμ? λΆλ¬?΄

View(exam) #??? μΆλ ₯

dim(exam) #?,?΄ μΆλ ₯

str(exam) #?°?΄?° ??± ??Έ

summary(exam) #??½ ?΅κ³λ μΆλ ₯

mpg<- as.data.frame(ggplot2::mpg) #?¨?€μ§ ggpl2? mpg ?°?΄?°λ‘? ?? ??Όλ‘? κ°? Έ?΄
                                  #as.data.frame=?°?΄?° ??±? ?°?΄?° ??λ‘? λ°κΏ 
mpg

View(mpg)

head(mpg, 10)

tail(mpg, 10)

str(mpg)

dim(mpg)

#int : ? ?
#num : ??? 

summary(mpg)

install.packages("dplyr") #λ³?λͺ? λ°κΎΈ? ?¨?€μ§
library(dplyr)

df_raw <- data.frame(var1 = c(1,2,1),
                     var2 = c(2,3,2))
df_raw

df_new <- df_raw

df_new

df_new <-rename(df_new, v2=var2) #df_new ?? ?? var2λ₯? v2λ‘? λ°κΎΌ?€

df_new

df_raw

df_new



#?Ό?? ?΄λ³΄κΈ°


df_mpg11<-mpg
mpg

  df_mpg11

df_mpg11 <- rename(df_mpg11, city = cty, highway = hwy)

  df_mpg11

head(df_mpg11)


#??λ³?:??? ?°?Ό?΄(μΉΌλΌ) ?? ? λ§λ¬

df<-data.frame(var1 = c(4,3,8),
               var2 = c(2,6,1))
  df

df$var_sum<-df$var1 + df$var2 #var_sum ??λ³? ??±:?΄(μΉΌλΌ) μΆκ? + ?°?°
  df

df$var_mean <- (df$var1 + df$var2)/2
  df

mpg$total <- (mpg$cty + mpg$hwy)/2
  mpg

head(mpg)

mean(mpg$total)

summary(mpg$total)

hist(mpg$total) #??? κ·Έλ¨

#μ‘°κ±΄λ¬?

ifelse(mpg$total >=20, "pass", "fail")

mpg$test <- ifelse(mpg$total >=20, "pass", "fail")
  mpg

head(mpg,20) #?°?΄?° ??Έ

table(mpg$test) #?°λΉν©κ²©λΉ???±

library(ggplot2) #ggplot2 λ‘λ
qplot(mpg$test) # ?°λΉ? ?©κ²? λΉλ λ§λ? κ·Έλ? ??±


mpg$grade <- ifelse(mpg$total >= 30, "A",
                    ifelse(mpg$total >= 20, "B", "C"))

head(mpg)

# ?Ό? ?΄λ³΄κΈ° total? κΈ°μ??Όλ‘? A,B,C,D?±κΈμΌλ‘? λΆ?¬
#total >= 30 A, >= 20 B, >= 15, C, Dλ³? λΆ?¬


mpg$grade1<- ifelse(mpg$total >= 30, "A",
                    ifelse(mpg$total >= 20,"B",
                           ifelse(mpg$total >= 15,"C","D" )))
head(mpg,20)

table(mpg$grade1) #κ·Έλ ?΄? λΉλ ??Έ

qplot(mpg$grade1) #κ·Έλ? ??±


table(mpg$grade)




mpg$grade2<- ifelse(mpg$total >= 30, "A",
                    ifelse(mpg$total >= 25,"B",
                           ifelse(mpg$total >= 20,"C","D" )))
head(mpg,10)
table(mpg$grade2)
qplot(mpg$grade2)
dim(mpg)

#λΆμ?? 

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


#λ¬Έμ ?΄κ²?

excel_exam <- read_excel("c:/Work_r/Data/excel_exam.xlsx") #? ??κ²½λ‘(λΉ‘μ  κ²½λ‘)
excel_exam1 <- excel_exam
  excel_exam1 #??Έ

#1. ?? ??΄ κ³Όν ?©κ³? μΆλ ₯
head(excel_exam1,10)
excel_exam1 <- rename(excel_exam1, ma = math, en = english, sci = science)
 excel_exam1 #??Έ

excel_exam1$class_sum <- excel_exam1$ma + excel_exam1$en + excel_exam1$sci
 excel_exam1 #??Έ


  
#2. ?? ??΄ κ³Όν ?κ·? μΆλ ₯
 
excel_exam1$class_mean_ma <- mean(excel_exam1$ma)
  excel_exam1 #??

excel_exam1$class_mean_en <- mean(excel_exam1$en)
  excel_exam1 #??΄

excel_exam1$class_mean_sci <- mean(excel_exam1$sci)
  excel_exam1 #κ³Όν

excel_exam1$class_mean_total <- mean(class_mean_ma + class_mean_en + class_mean_sci)/3
  excel_exam1 #μ΄? ?κ·?

  
#3. ?κ·? 60?  ?΄? = pass ?λ¨Έμ? = fail, ??λ³?

  excel_exam1$test1 <- ifelse(excel_exam1$class_sum/3 > 60,"pass", "fail" )  
  excel_exam1  
  

#4. 3? κ²°κ³Ό λΉλ κ·Έλ?

  qplot(excel_exam1$test1)


#5. ?κ·? 80?  ?΄? A 70?΄? B, ?λ¨Έμ? C λΆ?¬ ??λ³? 

  excel_exam1$test2<-ifelse(excel_exam1$class_sum/3 > 80,"A",
                            ifelse(excel_exam1$class_sum/3 > 70,"B","C" ))
  excel_exam1

  
head(test2, 10)
tail(test2, 7)
qplot(test2)

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  



