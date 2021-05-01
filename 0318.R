library(dplyr)
exam <- read.csv("data/csv_exam.csv") #?°?´?„° ë¶ˆëŸ¬?˜¤ê¸?

mean(df$score, na.rm = T) #ê²°ì¸¡ì¹˜ë?? ? œ?™¸?•˜ê³? ?‰ê·?
sum(df$score, na.rm = T)  #ê²°ì¸¡ì¹˜ë?? ? œ?™¸?•˜ê³? ?•©ê³?

exam[c(3,8,15), "math"] <- NA   #3,8,15 ?–‰?— NA ?• ?‹¹

exam %>% summarise(mean_math = mean(math))  # ?‰ê·? ?‚°ì¶?

exam %>% summarise(mean_math = mean(math, na.rm = T)) #ê²°ì¸¡ì¹? ? œ?™¸?•˜ê³? ?‰ê·? ?‚°ì¶? 

exam %>% summarise(mean_math = mean(math, na.rm = T),     #ê²°ì¸¡ì¹? ? œ?™¸?•˜ê³? ?‰ê·? ?‚°ì¶?
                   sum_math = sum(math, na.rm = T),       #ê²°ì¸¡ì¹? ? œ?™¸?•˜ê³? ?•©ê³? ?‚°ì¶?
                   median_math = median(math, na.rm = T)) #ê²°ì¸¡ì¹? ? œ?™¸?•˜ê³? ì¤‘ì•™ê°? ?‚°ì¶?

mean(exam$math, na.rm = T) #ê²°ì¸¡ì¹? ? œ?™¸?•˜ê³? math ?‰ê·? ?‚°ì¶?

exam$math <- ifelse(is.na(exam$math), 55, exam$math) #math?— NAê°€ ?ˆ?œ¼ë©? 55?„£?–´?¼, ?•„?‹˜ë§ê³ 
  table(is.na(exam$math)) #ê²°ì¸¡ì¹? ë¹ˆë„?‘œ ?ƒ?„±
  
#?˜¼??•´ë³´ê¸°
  
mpg <- as.data.frame(ggplot2::mpg)

mpg[c(65,124,131,153,212),"hwy"] <-NA  


#1

table(is.na(mpg)) #ê²°ì¸¡ì¹? ì²´í¬ = 5ê°? ë°œê²¬

table(is.na(mpg$drv)) #ê²°ì¸¡ì¹? ?Œ?Š´
table(is.na(mpg$hwy)) #ê²°ì¸¡ì¹? = 5ê°?

#2
  
mpg %>%
  filter(!is.na(mpg$hwy)) %>% #ê²°ì¸¡ì¹? ? œ?™¸ 
  group_by(drv) %>%   #drvë³? ë¶„ë¦¬
  summarise(mean_hwy = mean(hwy)) # hwy ?‰ê·? êµ¬í•˜ê¸? 


outlier <- data.frame(sex = c(1,2,1,3,2,1),
                      score = c(5,4,3,4,2,6))

outlier

table(outlier$sex) #?…Œ?´ë¸? ?™•?¸
table(outlier$score) #?…Œ?´ë¸? ?™•?¸


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
  

boxplot(mpg$hwy)$stats #?ƒ?ê·¸ë¦¼ ?†µê³„ì¹˜ ì¶œë ¥


mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA,mpg$hwy) #ê·¹ë‹¨ì¹? ê²°ì¸¡ ì²˜ë¦¬

table(is.na(mpg$hwy))  
  
  
mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy, na.rm = T))
  

library(ggplot2)

ggplot(data = mpg, aes(x = displ, y = hwy)) # x = displ, y = hwyë¡? ì§€? •?•´ ë°°ê²½ ?ƒ?„±

ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point(color="red", size=6) #ë°°ê²½?— ?‚°? ?„ ì¶”ê?€


ggplot(data = mpg, aes(x = displ, y = hwy)) + #?°?´?„° ì¶?
  geom_point(color="red", size=1) + #ê·¸ë˜?”„ ì¢…ë¥˜ 
  xlim(3,6) + #?„¸ë¶€?„¤? •(Xì¶?)
  ylim(20,30) #?„¸ë¶€?„¤? •(Yì¶?)
  
options(scipen = 99) #? •?ˆ˜ë¡? ?‘œê¸°í•˜?Š” ?˜µ
options(scipen = 0) #ì§€?ˆ˜ë¡? ?‘œê¸°í•˜?Š” ?˜µ
                                                              
#1
ggplot(data = mpg, aes(x=cty, y=hwy))+ geom_point(color="red", size=1)

#2
ggplot(data = midwest, aes(x = poptotal, y = popasian)) + #?°?´?„° ì¶?
  geom_point(color="red", size=1) + #ê·¸ë˜?”„ ì¢…ë¥˜ 
  xlim(0,500000) + #?„¸ë¶€?„¤? •(Xì¶?)
  ylim(0,10000) #?„¸ë¶€?„¤? •(Yì¶?)


mpg <- as.data.frame(ggplot2::mpg)

df_mpg <- mpg %>%
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))

df_mpg

ggplot(data = df_mpg, aes(x=drv, y=mean_hwy))+geom_col() #?‰ê· í‘œë¥? ë§Œë“¤ê³? ê·¸ë˜?”„ ?ƒ?„±

ggplot(data = df_mpg, aes(reorder(x=drv,-mean_hwy), y=mean_hwy))+geom_col() #ê·¸ë˜?”„ ?¬ê¸°ìˆœ ? •? ¬
#?–´?˜?¸ì§€?‘??€ ?‹¤ë¥¸ê±°?„ ê·¸ë˜?”„? •? ¬ê³? ?‹¨?ˆœ ?ˆ«? ? •? ¬??€ ?‹¤ë¥¸ë“¯
#?Œ¨?‚¤ì§€ë§ˆë‹¤ ëª…ë ?–´ê°€ ?‹¤ë¥¼ìˆ˜ ?ˆ?Œ.


ggplot(data = mpg, aes(x=drv))+geom_bar() #xê°’ë§Œ ë³´ì—¬ì§?

ggplot(data = mpg, aes(x=hwy))+geom_bar() # ?›?ë£? ?´?š©?•˜?—¬ ê·¸ë˜?”„ ?ƒ?„± 


#?˜¼??„œ ?•´ë³´ê¸°

#1

#?‰ê· í‘œ ?ƒ?„±
df <- mpg %>% 
  filter(class=="suv") %>% 
  group_by(manufacturer) %>% 
  summarise(mean_cty=mean(cty)) %>% 
  arrange(desc(mean_cty)) %>% 
  head(5)

#ê·¸ë˜?”„ ?ƒ?„±
ggplot(data = df, aes(x = reorder(manufacturer, -mean_cty),
                      y = mean_cty)) + geom_col()

# ë¨¼ì?€ ?°?´?„°?‘œë¥? ?ƒ?„±?•˜ê³? ê·? ?°?´?„°ë¥? ê·¸ë˜?”„ë¡? ë¶ˆëŸ¬?˜¤?Š” ê³¼ì •


#2
ggplot(data=mpg, aes(x=class))+geom_bar()


ggplot(data=economics, aes(x=date, y=unemploy)) + geom_line() #êº¾ì?€?‚° ê·¸ë˜?”„


#?˜¼??„œ ?•´ë³´ê¸°

p <- ggplot(data =economics, aes(x = date, y=psavert)) + geom_line(color="#880E4F", size = 1) 

p

min <- as.Date("2002-1-1")
max <- as.Date("2010-1-1")

p+scale_x_date(limits = c(min,max))


#?ˆ˜ì§ì„ 1
ggplot(data =economics, aes(x = date, y=psavert)) + 
  geom_line(color="#880E4F", size = 1) +
  geom_hline(yintercept = mean(economics$psavert))


#?ƒ? ê·¸ë¦¼ ë§Œë“¤ê¸?
ggplot(data=mpg, aes(x = drv, y = hwy)) + geom_boxplot()


#?˜¼? ?•´ë³´ê¸°
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

hist(exdata1$AGE) #?ˆ?Š¤?† ê·¸ë¨

hist(exdata1$AGE, xlim = c(0,60), ylim = c(0,5), main = "AGEë¶„í¬") 
#?—°?†? ?¸ ê°’ì„ ?‚˜??€?ƒ„ = ?ˆ?Š¤?† ê·¸ë¨


dist_sex<-table(exdata1$SEX)

barplot(dist_sex, ylim = c(0,8))


barplot(dist_sex, ylim = c(0,8), main = "barplot",
        xlab = "sex",
        ylab = "seq",
        names = c("?—¬?„±","?‚¨?„±"))

barplot(dist_sex, ylim = c(0,8), 
        main = "barplot", #?‘œ ? œëª?
        xlab = "sex", #ê°€ë¡œë¼ë²?
        ylab = "freq", #?„¸ë¡œë¼ë²?
        names = c("?—¬?„±","?‚¨?„±"), #?´ë¦?
        col = c("pink","navy")) #ì»¬ëŸ¬

boxplot(exdata1$Y17_CNT, exdata1$Y16_CNT,
        ylim = c(0,60),
        main = "?…„?„ë³? ê±´ìˆ˜",
        names = c("17?…„ ê±´ìˆ˜", "16?…„ ê±´ìˆ˜"),
        col = c("green", "yellow"))



data_18 <-read.csv("data/0318_data.csv")


data_18

#2


resident <- table(data_18$resident2, data_18$gender2)
  

resident







