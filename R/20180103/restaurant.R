library(readxl)
restaurant <- read_excel("restaurant.xlsx", 
                         col_types = c("numeric", "text", "text", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric"))
View(restaurant)

library(dplyr) #rename 함수를 쓰기위함
res <- rename(restaurant,
            문항 = 설문집계표,
            구분 = X__2,
            총계 = X__3,
            남합계 = X__4,
            남20 = X__5,
            남30 = X__6,
            남40 = X__7,
            남50 = X__8,
            여합계 = X__9,
            여20 = X__10,
            여30 = X__11,
            여40 = X__12,
            여50 = X__13
)
res1 <- res[-1:-3,-1]
res2 <- res1[-35:-42,]
res2[2:5,1] <-"방문 횟수"
res2[6:9,1] <- "방문 이유"
res2[10:14,1] <- "반찬 선호도"
res2[15:19,1] <- "선호 메뉴"
res2[20:24,1] <- "음식의 맛"
res2[25:29,1] <- "종업원 친절도"
res2[30:34,1] <- "위생 상태"

res_total <- res2[1,] # 인원종합
res_visit <- res2[2:5,] # 방문횟수
res_reason <- res2[6:9,] # 방문이유
res_prefer <- res2[10:14,] # 반찬 선호도
res_pfmenu <- res2[15:19,] # 선호 메뉴
res_taste <- res2[20:24,] # 음식의 맛
res_kind <- res2[25:29,] # 종업원 친절도
res_hygiene <- res2[30:34,] # 위생상태

# 1.연령별 구성비 구하기
res_total$twenty <- res_total$남20 + res_total$여20 # 20대총원 
res_total$thirty <- res_total$남30 + res_total$여30 # 30대총원
res_total$forty <- res_total$남40 + res_total$여40 # 40대 총원
res_total$fifty <- res_total$남50 + res_total$여50 # 50대 총원


install.packages("plotrix") # pie3D를 사용하기 위해 설치한다.
library(plotrix)

res_total2 <- c(res_total$twenty, res_total$thirty, res_total$forty, res_total$fifty)
#그냥 파이 그래프
pie(res_total2,  
    init.angle = 90, # 시작각도 90도 지정
    col=rainbow(length(res_total2)), # 색깔을 레인보우로
    label=c("20대","30대","40대","50대"), # label명 지정
    clockwise = T # 시계방향 T, 반시계 방향 F
    )
legend(0.5,1,c("20대","30대","40대","50대"), cex=0.6, fill=rainbow(length(res_total)))

gd_rate <- round(res_total2/sum(res_total2)*100, 1) #남녀 성별 백분율 구하고
gd_labels <- paste(gd_rate, "%") # 그걸 퍼센트로 낸다
# 3D파이 그래프
pie3D(res_total2,
      main= "연령별 구성비",
      col=rainbow(length(res_total2)), 
      labels=gd_labels,
      explode = 0.05
)
legend(0.5,1,c("20대","30대","40대","50대"), cex=0.6, fill=rainbow(length(res_total2)))



# 2.성별 구성비 구하기
res_total3 <- c(res_total$남합계, res_total$여합계)
# 그냥 파이그래프
pie(res_total3,  
    init.angle = 90, # 시작각도 90도 지정
    col=rainbow(length(res_total3)), # 색깔을 레인보우로
    label=c("남자","여자"), # label명 지정
    clockwise = T # 시계방향 T, 반시계 방향 F
)
gd_rate2 <- round(res_total3/sum(res_total3)*100, 1)
gd_labels2 <- paste(gd_rate2, "%") 
# 3D 파이그래
pie3D(res_total3,
    main= "성별 구성비",
    col=rainbow(length(res_total3)),
    labels=gd_labels2,
    explode = 0.05
)
legend(0.5,1,c("남자","여자"), cex=0.6, fill=rainbow(length(res_total3)))

# 방문 횟수
res_total4 <- c(res_visit$총계)
barplot(res_total4,
        main = "이용 횟수",
        xlab = "방문 횟수",
        ylab = "인원",
        col=rainbow(4),
        names.arg=c("0~1회","2~3회","3~4회","매일")
        )

# 반찬선호도
res_total5 <- c(res_prefer$총계)
barplot(res_total5,
        main = "반찬 선호도",
        xlab = "선호도",
        ylab = "인원",
        col=rainbow(4),
        names.arg=c("매우만족","만족","보통","불만족","매우불만족")
        )

# 선호메뉴
res_total6 <- c(res_pfmenu$총계)
barplot(res_total6,
        main = "선호메뉴",
        xlab = "선호도",
        ylab = "인원",
        col=rainbow(4),
        names.arg = c("매우만족","만족","보통","불만족","매우불만족")
        )

# 종업원 친절도
res_total7 <- c(res_kind$총계)
barplot(res_total7,
        main = "종업원 친절도",
        xlab = "선호도",
        ylab = "인원",
        col = rainbow(4),
        names.arg = c("매우만족","만족","보통","불만족","매우불만족")
        )
