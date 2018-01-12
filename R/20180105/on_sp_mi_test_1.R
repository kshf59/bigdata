# 스파크에서 불려오기
# 1. 접속
library(sparklyr)
library(dplyr)
# 2. 스파크 테이블에 접속; 캐시에 저장
tbl_cache(sc, 'sales2017')
# 3. 스파크 테이블 값 저장
sales2017_tbl <- tbl(sc, 'sales2017')
# 4. 스파크 테이블 값을 R 데이터프레임으로 변환
(sales2017 <- data.frame(sales2017_tbl))
# 데이터 핸들링 관련 패키지
library(dplyr)
# 그래픽 관련 패키지
library(ggplot2)
# 전체 매장 : 상관관계 
#plot(sales2017[,3:11])
# 서초 매장 : 상관관계 
seocho <- subset(sales2017, locat == "서초", select = c(dt, temp, humi, pm10, pm2, ozone, nitrogen, carbon, sulfurous, sales)) 
#plot(seocho[,2:10])
# ggplot2 관련함수 정의 
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
panel.lm <- function(x, y, col = par("col"), bg = NA, pch = par("pch"), cex = 1, col.smooth = "black", ...){
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  abline(stats::lm(y ~ x), col = col.smooth, ...)
}
# 전체 데이터 탐색
pairs(sales2017[,3:11], pch = ".", upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
# 서초 매장 데이터 탐색
pairs(seocho[,2:10], pch = ".", upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
# 매장별 매출액 비교
ggplot(data = sales2017, aes(x=substr(dt,1,6), y = sales, fill=locat)) +  geom_boxplot()
# 잠실 매장 데이터 준비
jamsil <- subset(sales2017, locat == "잠실", select = c(dt, temp, humi, pm10, sales)) 
# 잠실 매장 상관관계
pairs(jamsil[,2:5], pch = ".", upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
# 잠실 매장 데이터 탐색
ggplot(data = sales2017.tmp, aes(x=substr(dt,1,6), y = sales2, fill=locat)) +  geom_boxplot()


write.csv(jamsil, "jamsil.csv", sep=",")





