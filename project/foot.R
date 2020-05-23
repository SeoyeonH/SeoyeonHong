
# 내 발은 왕발인가 아닌가...----------------------
# 데이터 불러오기
getwd()
setwd("C:/Users/hongs/R.works/R-and-Statistics/project")

library(tidyverse)
library(readxl)

df <- read_excel('data/2015_7차_직접측정 데이터.xlsx')
df <- as.data.frame(df)


# 데이터 미리보기
str(df)
summary(df)

colnames(df)


# '발' 치수를 나타내는 컬럼만 추출 
names(df[, grepl('발', colnames(df))])


# 성별, 나이, 발치수만 가져오기 
foot <- df %>% 
  select(`ⓞ_02_성별`, `ⓞ_06_나이_반올림`, `①_118_발너비`, `①_119_발직선길이`)

colnames(foot) <- c('sex', 'age', 'f_width', 'f_length')

str(foot)


# 형변환
foot$sex <- as.factor(foot$sex)


# 결측치 제거
sum(is.na(foot))

foot <- na.omit(foot)

sum(is.na(foot))


# 발치수 히스토그램 보기------------------------
library(ggplot2)

ggplot(data=foot, aes(x=f_length)) + 
  geom_histogram(color='black', fill='white', binwidth=3)


# 성별에 따른 발치수 히스토그램-----------------

# foot %>% 
#   group_by(sex) %>% 
#   summarise(median=median(f_length))

library(plyr)

f_grpm <- ddply(foot, 'sex', summarise, grp.mean=mean(`f_length`, na.rm=T))
f_grpm

ggplot(data=foot, aes(x=f_length, fill=sex, color=sex)) + 
  geom_histogram(binwidth=3, alpha=0.5, position='identity') + 
  geom_vline(data=f_grpm, aes(xintercept=grp.mean, color=sex), linetype='dashed') +
  scale_color_brewer(palette="Accent") +
  scale_fill_brewer(palette="Accent") +
  labs(title="성별에 따른 발 길이의 분포", x="발길이(mm)", y = "Count") +
  theme_classic()


# 내 발 사이즈(245)는 평균을 넘는가?--------------------
# 20대 후반(26~29) 여성만 가져오기
summary(foot$age)

footF26 <- foot %>%
  filter((age >= 26) & (age <= 29) & (sex == '여'))

footF26

shapiro.test(footF26$f_length) 
## 정규성 검정 pass 못함


# 중위수, 3분위수, 최댓값 가져오기
summary(footF26$f_length)

# 중위수
medianF26 <- summary(footF26$f_length)[3]
medianF26

# 3분위수(75%)
q3F26 <- summary(footF26$f_length)[5]
q3F26

# 최댓값
maxF26 <- summary(footF26$f_length)[6]
maxF26


# 히스토그램에 나타내기
ggplot(data=footF26, aes(x=f_length)) + 
  geom_histogram(color='black', fill='white', binwidth=1) + 
  geom_vline(aes(xintercept=medianF26), color='black', linetype='dashed') +
  geom_text(aes(x=234, label='median', y=21), col="black", text=element_text(size=11)) +
  geom_vline(aes(xintercept=q3F26), color='black', linetype='dashed') +
  geom_text(aes(x=241, label='75%', y=21), col="black", text=element_text(size=11)) +
  geom_vline(aes(xintercept=maxF26), color='black', linetype='dashed') +
  geom_text(aes(x=265, label='max', y=21), col="black", text=element_text(size=11)) +
  geom_vline(aes(xintercept=245), color='red', linetype='dashed') +
  geom_text(aes(x=245, label='me', y=21), col="red", text=element_text(size=11)) +
  labs(title="26세~29세 여성 발 길이 분포", x="발길이(mm)", y = "Count")


# 20대 전체로 히스토그램 나타내기
footF20 <- foot %>% 
  filter((age >= 20) & (age <= 29) & (sex == '여'))

shapiro.test(footF20$f_length) 
## 정규성 검정 pass 못함

# 중위수
medianF20 <- summary(footF20$f_length)[3]
medianF20

# 3분위수(75%)
q3F20 <- summary(footF20$f_length)[5]
q3F20

# 최댓값
maxF20 <- summary(footF20$f_length)[6]
maxF20

ggplot(data=footF20, aes(x=f_length)) + 
  geom_histogram(color='black', fill='white', binwidth=1) + 
  geom_vline(aes(xintercept=medianF20), color='black', linetype='dashed') +
  geom_text(aes(x=233, label='median', y=50), col="black", text=element_text(size=11)) +
  geom_vline(aes(xintercept=q3F20), color='black', linetype='dashed') +
  geom_text(aes(x=240, label='75%', y=50), col="black", text=element_text(size=11)) +
  geom_vline(aes(xintercept=maxF20), color='black', linetype='dashed') +
  geom_text(aes(x=265, label='max', y=50), col="black", text=element_text(size=11)) +
  geom_vline(aes(xintercept=245), color='red', linetype='dashed') +
  geom_text(aes(x=245, label='me', y=50), col="red", text=element_text(size=11)) +
  labs(title="20대 여성의 발 길이 분포", x="발길이(mm)", y = "Count")


# 키와 발 크기 상관관계-------------------------------
# 필요한 데이터만 가져오기
colnames(df)

h_foot <- df %>% 
  select(`ⓞ_02_성별`, `ⓞ_06_나이_반올림`, `①_118_발너비`, `①_003_키`)

colnames(h_foot) <- c('sex', 'age', 'foot', 'height')


# 전처리
# h_foot$height <- h_foot$height * 0.1 
# head(h_foot$height)

str(h_foot)

h_foot$sex <- as.factor(h_foot$sex)

sum(is.na(h_foot))

h_foot <- na.omit(h_foot)


# 상관분석
cor <- cor(h_foot[, c(2:4)])
cor

plot(h_foot[, c(2:4)])

pairs(h_foot[, c(2:4)], panel=panel.smooth)

library(corrplot)
corrplot(cor, method='number')

# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(cor, histogram=TRUE, pch=19)

# 키와 발 길이만 
cor(x=h_foot$height, y=h_foot$foot)

cor.test(x=h_foot$height, y=h_foot$foot)
## 유의확률이 0.05 보다 작으므로 귀무가설 기각
## 키와 발 길이 간에 상관성이 있다는 가설이 유의미하다. 
