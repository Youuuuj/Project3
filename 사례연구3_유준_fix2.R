# 사례연구3


# 목차 
# 1. 다중회귀분석
# 2. 시계열분석


# 1. 다중회귀분석

data(state)  # 데이터 불러오기
state.x77



# (1) state 데이터셋을 load하고, state.x77 dataset을 데이터프레임으로 변환하고, 
# Life Exp 변수를 Life.Exp로 HS Grad변수를 HS.Grad로 변경하시오.

str(state.x77)

state_df <- as.data.frame(state.x77)  # 데이터프레임으로 변경

str(state_df)  # 구조확인
head(state_df)

names(state_df)[c(4,6)] <- c('Life.Exp','HS.Grad')  # 컬럼네임 변경
names(state_df)

# 보여주는 순서 str(state.77) -> str(state_df) -> names


# (2) Life Expectancy 변수를 종속변수로 설정하고 나머지 변수를 독립변수로 설정하여 회귀분석을 실시하시오. 
# 실시 후 결과에 대해 해석하시오.

# 귀무가설 : Life.Exp는 변수들에 따라 변화가 없다.
# 대립가설 : Life.Exp는 변수들에 따라 변화가 있다.

state_lm <- lm(formula = Life.Exp ~ ., data = state_df)
state_lm

library(car)
vif(state_lm)  # 분산팽창요인값이 10 미만의 값을 갖고 있기때문에 다중공선성의 문제가 없다.
summary(state_lm)


# 해석
# 총 변동 중 회귀직선에 의해 73.62%가 설명된다.
# 유의수준 0.001에서 귀무가설을 기각한다.
# 수정된 결정계수 : 0.6922
# 최종결론 : 변수들은 Life.Exp에 영향을 미친다고 볼 수 있다. 
           


# (3) (2)번 회귀모형에서 Income, Illiteracy, Area 변수를 제외하고 회귀분석을 실시하고 결과에 대해 해석하시오
# 귀무가설 : Life.Exp는 변수들에 따라 변화가 없다.
# 대립가설 : Life.Exp는 변수들에 따라 변화가 있다.

state_lm2 <- lm(formula = Life.Exp ~ Population + Murder + HS.Grad + Frost, data = state_df)
state_lm2

vif(state_lm2)  # 분산팽창요인 값 문제 없음.
summary(state_lm2)          


# 해석
# 분산팽창요인값이 10 미만의 값을 갖고 있기때문에 다중공선성의 문제 없다.
# 총 변동 중 회귀직선에 의해 73.6%가 설명된다.
# 유의 수준 0.001에서 귀무가설을 기각한다.
# 수정된 결정계수 : 0.7126
# 최종 결론 : Population, Murder, HS.Grad, Frost변수는 Life.Exp에 영향을 미친다고 볼 수 있다. 



# (4) Life Expectancy 변수를 종속변수로 설정하고
#     HS.Grad와 Murder 변수를 예측변수(predictor variable)로 설정하여 회귀분석을 실시하시오.
# 귀무가설 : Life.Exp는 변수들에 따라 변화가 없다.
# 대립가설 : Life.Exp는 변수들에 따라 변화가 있다.

state_lm3 <- lm(formula = Life.Exp ~ Murder + HS.Grad, data = state_df)
state_lm3

vif(state_lm3)
summary(state_lm3)


# 해석
# 분산팽창요인값이 10 미만의 값을 갖고 있기때문에 다중공선성의 문제 없다.
# 총 변동 중 회귀직선에 의해 66.28%가 설명된다.
# 유의 수준 0.001에서 귀무가설을 기각한다.
# 수정된 결정계수 : 0.6485
# 최종 결론: Murder, HS.Grad변수는 Life.Exp에 영향을 미친다고 볼 수 있다. 


# (5) 전 인구의 55%가 고졸이고 살인비율이 10만명당 8명일 때 Life Expectancy 결과값을 예측하시오

fitted.values(state_lm3)  # 각 컬럼별 적합된 값을 구함

# Life.exp =  70.29708 + -0.23709 * Murder + 0.04389 * HS.Grad  -  회귀방정식
70.29708 + -0.23709 * 8/100000 + 0.04389 * 0.55  # 회귀방정식을 적용하여 모델의 적합값계산



# (6) (4)번에서 처럼 2개의 독립변수, 1개의 종속변수의 데이터와 fit된 회귀평면(fitted regression plane)을 3D 그래프로 시각화하시오
install.packages("scatterplot3d")  # 있으면 제외
library(scatterplot3d)

Murder <- state_df$Murder
HS.Grad <- state_df$HS.Grad
Life.Exp <- state_df$Life.Exp

X11()
state_3d <- scatterplot3d(Murder,HS.Grad,Life.Exp, angle = 40,
                          type = 'h',
                          pch = 20,
                          scale.y = 0.7,
                          highlight.3d = TRUE,  # 기본이 T
                          box = TRUE,  # 기본이 T
                          grid = TRUE, # 기본이 T
                          mar = c(3, 4, 4, 3))
                          
state_3d$plane3d(state_lm3)




# 2. 과거 10년간 일별 KOSPI 지수(종가기준) 데이터를 기준으로 시계열분석을 실시하시오.


# 시계열 분석의 순서
# 1단계: 시계열 자료 생성
# 2단계: 시계열 자료 분석
# 3단계: 모형 추정, 생성
# 4단계: 모형 진단(모형 타당성 검정)
# 5단계: 미래 예측


# 1단계: 시계열 자료 생성
# 1-1단계 : 데이터 가져오기와 전처리
getwd()
setwd('C:/Users/You/Desktop/빅데이터/빅데이터 수업자료/R/dataset2')
kospi <- read.csv('kospi.csv', header = T)
str(kospi)
View(kospi)

# 1-2단계 : 시간순으로 정렬
kospi <- kospi[order(kospi$일자),]
kospi

# 1-3단계 : 시계열 객체생성
kospi_ts <- ts(kospi$종가, start=c(2011,229), frequency = 240)  # 시계열 데이터로 변환
str(kospi_ts)
print(kospi_ts)


# 2단계: 시계열 자료 분석
# 2-1단계 : 시계열 요소분해 시각화 ----- (2)의 답
kospi_decompose <- decompose(kospi_ts)
attributes(kospi_decompose)
plot(kospi_decompose)


# 2-2단계 : 각각의 변동요인 제거한 시각화 ----- (1),(3)의 답
par(mfrow=c(2,2))
plot(kospi_ts, type = 'l' , main = '추세선', ylab = '종가')
plot(kospi_ts - kospi_decompose$seasonal, main = '계절 요인 제거 그래프', ylab = '종가')
plot(kospi_ts - kospi_decompose$trend, main = '추세 요인 제거 그래프', ylab = '종가')
plot(kospi_ts - kospi_decompose$seasonal - kospi_decompose$trend, main = '불규칙요인 그래프',ylab = '종가')



# 2-3단계 : 정상성 시계열 변환
par(mfrow=c(3,1))
plot(kospi_ts, type = 'l' , main = '추세선', ylab = '종가')
plot(diff(kospi_ts, differences = 1), main = '1차 차분')  # 평균 정상화
plot(diff(log(kospi_ts), differences = 1),  main = 'log 차분')  # 분산 정상화


# 3단계:  ARIMA 시계열 모형 추정, 생성
# 3-1단계 : ARIMA모형 추정
# install.packages('forecast')
library(forecast)
kospi_arima = auto.arima(kospi_ts)
kospi_arima

# 3-2단계 : ARIMA모형 생성
kospi_model <- arima(kospi_ts, order = c(3,1,2))
kospi_model


# 4단계: 모형 진단(모형 타당성 검정)
# 4-1단계 : 자기 상관 함수에 의한 모형 진단
tsdiag(kospi_model)
# p-value값이 0.05이상으로 분포 -> 현재 ARIMA모형은 매우 양호한 시계열 모형


# 4-2단계 : Box-Ljung검정에 의한 잔차향 모형 진단
Box.test(kospi_model$residuals, lag = 1, type = "Ljung")
# P-value가 0.05이상이기에 모형이 통계적으로 적절하다고 본다.

# 5단계: 미래 예측
kospi_forecasts=forecast(kospi_model) 
kospi_forecasts

par(mfrow=c(1,1))
plot(kospi_forecasts)
# 해석
# 진한 파란선이 점추정과 같은 회귀선
# 가장 높은 옅은 파랑영역(80%)과 회색영역(95%)이 신뢰구간인 구간예측값(=예측결과)이다.


# 결론 : 10년간 일별 KOSPI지수를 시계열분석 방법을 이용해 이후 2년간의 기간에 대해 예측했다.

