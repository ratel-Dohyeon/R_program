library(dplyr)
exam <- read.csv("data/csv_exam.csv") #?°?΄?° λΆλ¬?€κΈ?

mean(df$score, na.rm = T) #κ²°μΈ‘μΉλ?? ? ?Έ?κ³? ?κ·?
sum(df$score, na.rm = T)  #κ²°μΈ‘μΉλ?? ? ?Έ?κ³? ?©κ³?

exam[c(3,8,15), "math"] <- NA   #3,8,15 ?? NA ? ?Ή

exam %>% summarise(mean_math = mean(math))  # ?κ·? ?°μΆ?

exam %>% summarise(mean_math = mean(math, na.rm = T)) #κ²°μΈ‘μΉ? ? ?Έ?κ³? ?κ·? ?°μΆ? 

exam %>% summarise(mean_math = mean(math, na.rm = T),     #κ²°μΈ‘μΉ? ? ?Έ?κ³? ?κ·? ?°μΆ?
                   sum_math = sum(math, na.rm = T),       #κ²°μΈ‘μΉ? ? ?Έ?κ³? ?©κ³? ?°μΆ?
                   median_math = median(math, na.rm = T)) #κ²°μΈ‘μΉ? ? ?Έ?κ³? μ€μκ°? ?°μΆ?

mean(exam$math, na.rm = T) #κ²°μΈ‘μΉ? ? ?Έ?κ³? math ?κ·? ?°μΆ?

exam$math <- ifelse(is.na(exam$math), 55, exam$math) #math? NAκ° ??Όλ©? 55?£?΄?Ό, ??λ§κ³ 
  table(is.na(exam$math)) #κ²°μΈ‘μΉ? λΉλ? ??±
  
#?Ό??΄λ³΄κΈ°
  
mpg <- as.data.frame(ggplot2::mpg)

mpg[c(65,124,131,153,212),"hwy"] <-NA  


#1

table(is.na(mpg)) #κ²°μΈ‘μΉ? μ²΄ν¬ = 5κ°? λ°κ²¬

table(is.na(mpg$drv)) #κ²°μΈ‘μΉ? ??΄
table(is.na(mpg$hwy)) #κ²°μΈ‘μΉ? = 5κ°?

#2
  
mpg %>%
  filter(!is.na(mpg$hwy)) %>% #κ²°μΈ‘μΉ? ? ?Έ 
  group_by(drv) %>%   #drvλ³? λΆλ¦¬
  summarise(mean_hwy = mean(hwy)) # hwy ?κ·? κ΅¬νκΈ? 


outlier <- data.frame(sex = c(1,2,1,3,2,1),
                      score = c(5,4,3,4,2,6))

outlier

table(outlier$sex) #??΄λΈ? ??Έ
table(outlier$score) #??΄λΈ? ??Έ


outlier$sex <- ifelse(outlier$sex==3, NA, outlier$sex)
  outlier  

outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
  outlier

  
outlier %>% 
  filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score = mean(score))
    
  
mpg <- as.data.frame(ggplot2::mpg)
boxplot(mpg$hwy)  
  

boxplot(mpg$hwy)$stats #??κ·Έλ¦Ό ?΅κ³μΉ μΆλ ₯


mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA,mpg$hwy) #κ·Ήλ¨μΉ? κ²°μΈ‘ μ²λ¦¬

table(is.na(mpg$hwy))  
  
  
mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy, na.rm = T))
  

library(ggplot2)

ggplot(data = mpg, aes(x = displ, y = hwy)) # x = displ, y = hwyλ‘? μ§? ?΄ λ°°κ²½ ??±

ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point(color="red", size=6) #λ°°κ²½? ?°? ? μΆκ?


ggplot(data = mpg, aes(x = displ, y = hwy)) + #?°?΄?° μΆ?
  geom_point(color="red", size=1) + #κ·Έλ? μ’λ₯ 
  xlim(3,6) + #?ΈλΆ?€? (XμΆ?)
  ylim(20,30) #?ΈλΆ?€? (YμΆ?)
  
options(scipen = 99) #? ?λ‘? ?κΈ°ν? ?΅
options(scipen = 0) #μ§?λ‘? ?κΈ°ν? ?΅
                                                              
#1
ggplot(data = mpg, aes(x=cty, y=hwy))+ geom_point(color="red", size=1)

#2
ggplot(data = midwest, aes(x = poptotal, y = popasian)) + #?°?΄?° μΆ?
  geom_point(color="red", size=1) + #κ·Έλ? μ’λ₯ 
  xlim(0,500000) + #?ΈλΆ?€? (XμΆ?)
  ylim(0,10000) #?ΈλΆ?€? (YμΆ?)


mpg <- as.data.frame(ggplot2::mpg)

df_mpg <- mpg %>%
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))

df_mpg

ggplot(data = df_mpg, aes(x=drv, y=mean_hwy))+geom_col() #?κ· νλ₯? λ§λ€κ³? κ·Έλ? ??±

ggplot(data = df_mpg, aes(reorder(x=drv,-mean_hwy), y=mean_hwy))+geom_col() #κ·Έλ? ?¬κΈ°μ ? ? ¬
#?΄??Έμ§??? ?€λ₯Έκ±°? κ·Έλ?? ? ¬κ³? ?¨? ?«? ? ? ¬?? ?€λ₯Έλ―
#?¨?€μ§λ§λ€ λͺλ ?΄κ° ?€λ₯Όμ ??.


ggplot(data = mpg, aes(x=drv))+geom_bar() #xκ°λ§ λ³΄μ¬μ§?

ggplot(data = mpg, aes(x=hwy))+geom_bar() # ??λ£? ?΄?©??¬ κ·Έλ? ??± 


#?Ό?? ?΄λ³΄κΈ°

#1

#?κ· ν ??±
df <- mpg %>% 
  filter(class=="suv") %>% 
  group_by(manufacturer) %>% 
  summarise(mean_cty=mean(cty)) %>% 
  arrange(desc(mean_cty)) %>% 
  head(5)

#κ·Έλ? ??±
ggplot(data = df, aes(x = reorder(manufacturer, -mean_cty),
                      y = mean_cty)) + geom_col()

# λ¨Όμ? ?°?΄?°?λ₯? ??±?κ³? κ·? ?°?΄?°λ₯? κ·Έλ?λ‘? λΆλ¬?€? κ³Όμ 


#2
ggplot(data=mpg, aes(x=class))+geom_bar()


ggplot(data=economics, aes(x=date, y=unemploy)) + geom_line() #κΊΎμ??° κ·Έλ?


#?Ό?? ?΄λ³΄κΈ°

p <- ggplot(data =economics, aes(x = date, y=psavert)) + geom_line(color="#880E4F", size = 1) 

p

min <- as.Date("2002-1-1")
max <- as.Date("2010-1-1")

p+scale_x_date(limits = c(min,max))


#?μ§μ 1
ggplot(data =economics, aes(x = date, y=psavert)) + 
  geom_line(color="#880E4F", size = 1) +
  geom_hline(yintercept = mean(economics$psavert))


#?? κ·Έλ¦Ό λ§λ€κΈ?
ggplot(data=mpg, aes(x = drv, y = hwy)) + geom_boxplot()


#?Ό? ?΄λ³΄κΈ°
class_mpg <- mpg %>% 
  filter(class %in% c("compact","subcompact","suv"))

  ggplot(data = class_mpg, aes(x=class, y = cty))+ 
  geom_boxplot()

library(readxl)  
  



install.packages("descr")
  library(descr)

exdata1 <- read_excel("data/sample1.xlsx")
exdata1


freq_test <- freq(exdata1$AREA, plot = F)
  freq_test


stem(exdata1$AGE)

hist(exdata1$AGE) #??€? κ·Έλ¨

hist(exdata1$AGE, xlim = c(0,60), ylim = c(0,5), main = "AGEλΆν¬") 
#?°?? ?Έ κ°μ ???? = ??€? κ·Έλ¨


dist_sex<-table(exdata1$SEX)

barplot(dist_sex, ylim = c(0,8))


barplot(dist_sex, ylim = c(0,8), main = "barplot",
        xlab = "sex",
        ylab = "seq",
        names = c("?¬?±","?¨?±"))

barplot(dist_sex, ylim = c(0,8), 
        main = "barplot", #? ? λͺ?
        xlab = "sex", #κ°λ‘λΌλ²?
        ylab = "freq", #?Έλ‘λΌλ²?
        names = c("?¬?±","?¨?±"), #?΄λ¦?
        col = c("pink","navy")) #μ»¬λ¬

boxplot(exdata1$Y17_CNT, exdata1$Y16_CNT,
        ylim = c(0,60),
        main = "??λ³? κ±΄μ",
        names = c("17? κ±΄μ", "16? κ±΄μ"),
        col = c("green", "yellow"))



data_18 <-read.csv("data/0318_data.csv")


data_18

#2


resident <- table(data_18$resident2, data_18$gender2)
  

resident







