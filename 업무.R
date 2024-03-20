install.packages("forecast")
install.packages("tseries")
install.packages("reshape")
install.packages("reshape2")
install.packages("nycflights13")
install.packages("kears")
install.packages("reticulate")
install.packages("tensorflow")
install.packages("tfruns")
install.packages("zeallot")
install.packages("timetk")
install.packages("proxy")
install.packages("plyr")
install.packages("TTR")
install.packages("tidytext")
install.packages("tidyverse")
install.packages("tm")
install.packages("openxlsx")
install.packages("devtools")
install.packages("KoNLP")
devtools::install_github("rstudio/keras")
install.packages("wordcloud2")

library(plyr)
library(TTR)
library(forecast)
library(tseries)
library(KoNLP)
library(tidytext)
library(dplyr)
library(tidyverse)
library(reshape)
library(reshape2)
library(lubridate)
library(nycflights13)
library(keras)
library(tensorflow)
library(ggplot2)
library(timetk)
library(proxy)
library(tm)
library(readxl)
library(openxlsx)
library(wordcloud2)
############
useNIADic()
###########
bertdata=read.xlsx("남해안_해양관광_1차가공.xlsx")
###########
maintext.t=tibble(bertdata)
###########
word1=gsub("[[:punct:]]", " ",maintext.t)
word2=gsub("\\d+", " ",word1)
word3=tibble(word2)
word4=extractNoun(word3)
word5=tibble(word4)
word6=word5 %>% count(word4,sort=TRUE)
word7=word6 %>% filter(nchar(word4)>1)
write.xlsx(word7,"c:/r/남해안_해양관광_워클.xlsx")
word8=read.xlsx("c:/Users/pnh17/OneDrive/바탕 화면/업무/3.내곡지구/DB/전처리/서초구_워클.xlsx")
wordcloud2(word8,size=0.5,fontFamily = '나눔바른고딕')

#########2번째 워드클라우드
data=read.xlsx("1-1,1-2_data_가치체계.xlsx")
maintext.t=tibble(data)
#########데이터 전처리(특수문자 제거 및 숫자제거)

word1=gsub("[[:punct:]]", " ",maintext.t)
word2=gsub("\\d+", " ",word1)
word3=tibble(seq = 1:length(word2),text = word2)
#########데이터 띄워쓰기 분리
word4=word3 %>% 
  unnest_tokens(
    input = word2,
    output = "word"
  )
word0=word4 %>% filter(nchar(word)>1)
#########데이터 저장 후, 수동 전처리(이 단계에서는 "인재양성 및 최고인재 같은 단어들 빼는 단계")
write.xlsx(word0,"c:/a/out/word/1차전처리.xlsx")
word5=read.xlsx("c:/a/out/word/1차전처리.xlsx")
#########형태소 분석
word6=extractNoun(word5$word)
worda=unlist(word6)
wordb=tibble(worda)
#########빈도 측정
word7=wordb %>% count(worda,sort=TRUE) %>% filter(nchar(worda)>1)
#########단절음 수동 제거(사유는 "삶"과 같이 필요한 단어 존재) + 변수명 word로 바꾸기
write.xlsx(word7,"c:/a/out/word/2차전처리.xlsx") 
word8=read.xlsx("c:/a/out/word/2차전처리.xlsx")
#########4글자로 된 "인재양성"과 같은 단어 불러오기
word9=read.xlsx("c:/a/out/word/분리단어.xlsx")
#########분리단어 빈도측정
word10=word9 %>% count(word)
#########rbind로 형태소분석 데이터와 분리데이터 묶어주기
word11=rbind(word8,word10)
word12=arrange(word11,desc(n))
#########워드클라우드 제작
wordcloud2(word12)
############빈도 데이터 끌고와서 워드클라우드 하기
wordb=read.xlsx("c:/a/out/word/워드클라우드 데이터.xlsx")
wordcloud2(wordb)

###########SNA##################
maintext.t=tibble(bertdata)
###########문서번호 라벨링
word1=tibble(seq = 1:length(maintext.t$word),text = maintext.t$word)
###########띄워쓰기로 토큰화
word2=word1 %>% 
  unnest_tokens(
    input = text,
    output = "word"
  )
##########1개의 문자열 삭제
word3=word2 %>% filter(nchar(word)>1)
##########수동전처리
write.xlsx(word3,"c:/a/out/word/1차전처리sna.xlsx")
word4=read.xlsx("c:/a/out/word/1차전처리sna.xlsx")
##########형태소 분석
word5=extractNoun(word4$word)
word6=tibble(word5)
word7=cbind(word6,word4$seq)
##########2차수동전처리
write.xlsx(word7,"c:/a/out/word/2차전처리SNA.xlsx") 
word8=read.xlsx("c:/a/out/word/2차전처리SNA.xlsx")
##########분리단어SNA불러오기
word9=read.xlsx("c:/a/out/word/분리단어SNA.xlsx")
##########분리단어와 묶어주기
word10=rbind(word8,word9)
##########
word11=word10 %>% group_by(seq) %>% summarise(word=paste(word,collapse = " "))
g=Corpus(VectorSource(word11$word))
h=TermDocumentMatrix(g)
i=as.matrix(h)
j=t(data.frame(i))
k=data.frame(j)
write.xlsx(k,"c:/a/out/word/mat1.xlsx")
l=read.xlsx("c:/a/out/word/mat1.xlsx")
##########
data= read.xlsx("c:/a/out/word/mat2.xlsx")

dolbyus.cor=cor(data)

library(sna)

snaplot<- function(y){
  for(i in 1:16){
    for(j in 1:16){
      if (abs(dolbyus.cor[i,j])>y) dolbyus.cor[i,j]=1 else dolbyus.cor[i,j]=0}}
  print(dolbyus.cor)
  gplot(dolbyus.cor,displaylabels = T, boxed.labels = F)
}

fix(snaplot) 

snaplot(0.13)

#########################################

vect01 = c(179,2240,773,841,867,1005,1744,2146,2320,2708,2195,4951,2264,2265,2192,2235,1712)
vect02 = c(6966,9222,6985,6556,6787,9413,10529,9300,9205,12055,11500,13413,14233,13973,13422,15059,16511,21596,21241,17822,20406,21313,16311)
vect03 = c(50000,60000,80000,110000,130000,140000,150000,170000,180000,190000,220000,280000,380000,490000,530000,570000,650000,790000,950000,1110000,
           1310000,1610000,2040000,2660000,3390000,4250000,5230000,6270000,7400000,8470000,9550000,10410000,10470000,11160000,12060000,12910000,
           13950000,14590000,14930000,15400000,15900000,16430000,16790000,17330000,17940000,18440000,18870000,19400000,20120000,20990000,21800000,
           22530000,23200000,23680000,24370000,24910000)
vect04 = c(31000,31000,41000,52000,71000,72000,83000,84000,104000,105000,115000,155000,197000,258000,279000,300000,341000,403000,485000,556000,638000,
           770000,922000,1099000,1312000,1525000,1765000,2006000,2249000,2463000,2654000,2826000,2885000,3325000,3977000,4029000,4212000,4315000,
           4307000,4268000,4289000,4322000,4313000,4304000,4306000,4309000,4293000,4326000,4370000,4424000,4460000,4496000,4521000,4495000,4506000,4498000)
vect05 = c(406875,413970,411951,412065,431131,440021,439012,439998,446541,454288,451258,451477,445401,438163,435107,429025,416167)
vect06 = c(357707,353839,350717,345403,338633,333114,322494,303068,292893,287945)
vect07 = c(939,1323,1846,2367,3236,4635,3484,2356,3073,3125,3176,3152)
vect08 = c(11140,12175,14201,13231,17241,13335,15346,17502)
vect09 = c(18256,18955,19544,25468,23304,23429,24682,18117)
vect10 = c(41649,42589,43774,44930,45090,46227,47396,48019,49800,51150)
vect11 = c(137689,137521,137204,136805,136138,136517,136432,135386,134617,133018,131591,131969)
vect12 = c(3498,3539,3642,3721,3804,3887,4115,4344,4573,4707)
vect13 = c(19650926,19410811,16732830,16991676,17032004,16572203,15356829,17957794,14755088,18268273,23768875,24997216,18654084,18501641)
vect14 = c(15663,19849,28802)
vect15 = c(11323,14256,17827,19423)
vect16 = c(7572,8505,10849,11501)
vect17 = c(4941,13555,17301,22189)
vect18 = c(3109,2944,5890,7795)
vect19 = c(1733086,1431471,1488201,2183074,2681238,2732394,3141766,4237895,4907390,5455643,4252716,5008011,5480408,
           5197303,5162803,4749688,4800536,5362782,5690433,7205470,7694892,7179123,7278741,7415520,7464520,7577505,
           6672966,6662887,5490364,5238189,5277508,5825838,4134749,4531998,5252075)
ts01 = ts(vect19,start = 1988)
ts01

str(ts01)

par(mfrow=c(1,2))
ts.plot(ts01)  #- 비정상성 시계열 데이터
log <- diff(log(ts01)) # 로그+차분 수행 
log
plot(log) #추세선 확인

ts.plot(ts01,col="red")

acf(na.omit(ts01), main = "자기상관함수", col = "red")
pacf(na.omit(ts01), main="부분자기상관함수", col="red")

plot(ts01, main="원 시계열 자료") # 시계열 자료 시각화 
plot(SMA(ts01, n=1), main="1년 단위 이동평균법으로 평활")
plot(SMA(ts01, n=2), main="2년 단위 이동평균법으로 평활")
plot(SMA(ts01, n=3), main="3년 단위 이동평균법으로 평활")

par(mfrow=c(1,2))
ts.plot(ts01)
diff <- diff(ts01,difference = 1)
plot(diff)
arima <- auto.arima(ts01) # 시계열 데이터 이용.
arima

model <- arima(ts01, order=c(3,5,4))
model
tsdiag(model)
Box.test(model$residuals, lag = 1, type = "Ljung")
fore <- forecast(model,h=8) # 향후 h년 예측
fore

write.xlsx(fore,"c:/r/ .xlsx")

par(mfrow=c(1,2))
plot(fore) # 향후 10년 예측치 시각화 

m=decompose(ts01)

model2 <- forecast(model, h = 6) # 향후 6개월 예측치 시각화 
plot(model2)
##################################

fc = holt(ts01, h=15)
fc2 = holt(ts01, damped = TRUE, phi = 0.9, h=15)
autoplot(ts01) +
  autolayer(fc, series="홀트 기법", PI=FALSE) +
  autolayer(fc2, series="감쇠 홀트 기법", PI=FALSE) +
  ggtitle("홀트 기법으로 얻은 예측값") + xlab("연도") +
  ylab("호주 항공객 (백만 명)") +
  guides(colour=guide_legend(title="예측값"))
fc
fc2
fc3 = holt(fc2, damped = TRUE)
summarise(fc2)
accuracy(fore)
accuracy(fc2)
##############################################
a1.ts <- ts(vect01, start=c(2006), end=c(2022), freq=12)
a1.ts
nValid <- 36
nTrain <- length(a1.ts)-nValid

train.ts <- window(a1.ts, start=c(1991,1), end=c(1991,nTrain))
valid.ts <- window(a1.ts, start=c(1991,nTrain+1), end=c(1991,nTrain+nValid))

