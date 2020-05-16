
# 한국인 키는 정규분포를 따를까?---------------
# 데이터 불러오기
getwd()
setwd("C:/Users/hongs/R.works/SelfStudy/R-and-Statistics/project")

library(tidyverse)
library(readxl)

df <- read_excel('data/2015_7차_직접측정 데이터.xlsx')
df <- as.data.frame(df)

str(df)
summary(df)

# 성별, 키, 몸무게 컬럼만 가져오기
size <- df %>% 
  select(`ⓞ_02_성별`, `①_003_키`, `①_031_몸무게`)

colnames(size) <- c('sex', 'height', 'weight')

str(size)

# factor 형변환
size$sex <- as.factor(size$sex)

str(size)
summary(size)

# 결측치 제거하기
sum(is.na(size)) 

size <- na.omit(size)

sum(is.na(size))


# 키 히스토그램 그리기---------------------------------
# 키만 변수로 저장
height <- (size$height)*0.1

summary(height)

x_range <- seq(130, 200, by=2)

# hist
hist(height, breaks=x_range,
     col='gray', main='전체 키',
     xlab='height(cm)', 
     ylab='frequency')
legend('topright', 'height', fill='gray')

# ggplot
library(ggplot2)

h <- ggplot(size, aes(x=height)) +
  geom_histogram(color='black', fill='white', binwidth=10) 

# with mean (na.rm=TRUE)
h + geom_vline(aes(xintercept=mean(height)),
               color='blue', linetype='dashed', size=1)

# with density
ggplot(size, aes(x=height)) + 
  geom_histogram(aes(y=..density..), color='black', fill='white', binwidth=10) +
  geom_density(alpha=.2, fill="#FF6666")


# 성별에 따른 키 히스토그램 그리기--------------------------------
# 그룹별 평균 구하기
library(plyr)

h_grpm <- ddply(size, 'sex', summarise, grp.mean=mean(height, na.rm=T))
h_grpm

h_grp <- ggplot(size, aes(x=height, color=sex)) +
  geom_histogram(binwidth=10, fill='white', alpha=0.5, position='identity') +
  geom_vline(data=h_grpm, aes(xintercept=grp.mean, color=sex), linetype='dashed')
# geom_histogram(position='dodge)
# theme(legend.position='top)
h_grp

# different color
h_grp + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) 

h_grp + scale_color_brewer(palette='Dark2')

h_grp + scale_color_grey() + theme_classic() 
# for more color options : http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually

# show them at onece
# par(mfrow=c(2, 2))
# plot(manual, pch=1)

# fill color
ggplot(size, aes(x=height, fill=sex, color=sex)) +
  geom_histogram(binwidth=10, position='identity', alpha=0.5)

# use facet
ggplot(size, aes(x=height)) + 
  geom_histogram(binwidth=10, color='black', fill='white') + 
  facet_grid(sex ~ .) +
  geom_vline(data=h_grpm, aes(xintercept=grp.mean), 
             color='red', linetype='dashed') +
  theme(legend.position='none')

# customizing
ggplot(size, aes(x=height, color=sex, fill=sex)) + 
  geom_histogram(binwidth=10, position='identity', alpha=0.5) + 
  geom_vline(data=h_grpm, aes(xintercept=grp.mean, color=sex), linetype='dashed') +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  labs(title="성별에 따른 키의 분포", x="Height(cm)", y = "Count") +
  theme_classic()

# scale_color_brewer(palette="Paired") 
# scale_color_brewer(palette="Accent")

# with 'Accent'
ggplot(size, aes(x=height, color=sex, fill=sex)) + 
  geom_histogram(binwidth=10, position='identity', alpha=0.5) + 
  geom_vline(data=h_grpm, aes(xintercept=grp.mean, color=sex), linetype='dashed') +
  scale_color_brewer(palette="Accent") +
  scale_fill_brewer(palette="Accent") +
  labs(title="성별에 따른 키의 분포", x="Height(cm)", y = "Count") +
  theme_classic()


# 남, 여 키 평균과 분산----------------------------
# 남, 여 키만 따로 변수로 저장 
male_h <- size[size$sex == '남', 'height'] * 0.1
female_h <- size[size$sex == '여', 'height'] * 0.1

# 평균
mean(male_h)
mean(female_h) 

# 두 평균은 차이가 있을까? YES
t.test(male_h, female_h, var.equal=FALSE)
  # 귀무가설 기각
  # 평균의 차이는 0이 아니다. 

# 분산
var(male_h) 
var(female_h) 

# 두 분산은 차이가 있을까? YES/NO
var.test(male_h, female_h)
  # 유의확률을 0.05로 설정한다면...
  # 평균의 차이는 0이 아니다. 

