library(readxl)
seocho <- read_excel("seocho.xlsx", col_types = c("numeric", 
                                                  "numeric", "numeric", "numeric", "numeric"))
View(seocho)


library(dplyr)

test[1:31,1] <- "1월"
test[32:59,1] <- "2월"
test[60:90,1] <- "3월"
test[91:120,1] <- "4월"
test[121:151,1] <- "5월"
test[152:181,1] <- "6월"
test[182:212,1] <- "7월"
test[213:243,1] <- "8월"
test[244:273,1] <- "9월"
test[274:304,1] <- "10월"
test[305:334,1] <- "11월"
test[335:365,1] <- "12월"
library(ggplot2)
# 1~12월 온도 차이
seocho.1 <- seocho %>% group_by(date) %>% summarise(avg = mean(temperature))
g <- ggplot(seocho.1,aes(x=date,y=avg, fill = date)) + geom_bar(stat="identity")
g
# 날짜와 미세먼지 상관관계 
g2 <- ggplot(seocho, aes(x = date, y = dust, fill = date)) + geom_boxplot()
g2 <- g2 + theme(axis.text.x = element_text(angle=10, hjust=1, vjust=1))
g2

# 온도와, 매출의 상관관계
g4 <- seocho %>% select(date, temperature, sales) %>% filter(date %in% c("12월","8월"))
g4 <- ggplot(g4, aes(x = sales, y = temperature, color = date)) + geom_line() + geom_smooth(method = "lm")
g4



