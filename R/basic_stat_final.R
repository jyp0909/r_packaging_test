# install.packages(c('RPostgreSQL', 'tidyverse', 'lubridate', 'reshape2', 'plyr', 'dplyr', 'car', 'ggplot2', 'stringr', 'zoo'))
# "C:\Program Files\R\R-3.6.0\bin\Rscript.exe" "C:\Users\minjo\OneDrive\바탕 화면\기초통계 작업\evt_ocr0726_최종v2.R"
options(warn=-1)
suppressMessages(library(tidyverse));suppressMessages(library(lubridate));suppressMessages(library(reshape2));
suppressMessages(library(plyr));suppressMessages(library(dplyr));suppressMessages(library(RPostgreSQL));
suppressMessages(require(zoo));suppressMessages(require(car));suppressMessages(require(ggplot2));suppressMessages(require(stringr))

pop.data.load <- function(user, password){
  #################################################################
  #### DECLARE POSTGRES DB NAME; HOST IP; PORT; FROM STATEMENT ####
  dbname<-"smartcity"; host<-"192.168.0.52"; port<-5433; from<-"smartcity.ods_lake.dj_pop_emd"###

  require(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  con <- dbConnect(drv, dbname = dbname, host = host, port = port,
                   user = user, password = password)

  set_utf8 <- function(x){
    # Declare UTF-8 encoding on all character columns:
    chr <- sapply(x, is.character)
    x[, chr] <- lapply(x[, chr, drop = FALSE], `Encoding<-`, "UTF-8")
    # Same on column names:
    Encoding(names(x)) <- "UTF-8"
    x
  }

  df <- set_utf8(dbGetQuery(con, statement=as.character(paste("select * from", from, sep = " "))))
  return(df)
}



lddf.load <- function(user, password){
  #################################################################
  #### DECLARE POSTGRES DB NAME; HOST IP; PORT; FROM STATEMENT ####
  dbname<-"smartcity"; host<-"192.168.0.52"; port<-5433; from<-"smartcity.ods_lake.evt_ocr"
  #################################################################

  require(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  con <- dbConnect(drv, dbname = dbname, host = host, port = port,
                   user = user, password = password)

  set_utf8 <- function(x){
    # Declare UTF-8 encoding on all character columns:
    chr <- sapply(x, is.character)
    x[, chr] <- lapply(x[, chr, drop = FALSE], `Encoding<-`, "UTF-8")
    # Same on column names:
    Encoding(names(x)) <- "UTF-8"
    x
  }

  df <- set_utf8(dbGetQuery(con, statement=as.character(paste("select * from", from, sep = " "))))
  return(df)
}

set_utf8 <- function(x){
  # Declare UTF-8 encoding on all character columns:
  chr <- sapply(x, is.character)
  x[, chr] <- lapply(x[, chr, drop = FALSE], `Encoding<-`, "UTF-8")
  # Same on column names:
  Encoding(names(x)) <- "UTF-8"
  x
}



popdf <- pop.data.load(user="postgres", password="postgres")
#view(popdf)


testdf <- lddf.load(user="postgres", password="postgres")
#head(testdf)

raw <- testdf %>% mutate(date=as.Date(rgs_date)) %>%
  filter(evt_id %in% c('119UC001', '112UC001', 'WPSSF110')) %>%
  filter(date >= "2016-06-01", date <= "2018-07-01") %>%
  mutate(dayofweek = factor(weekdays(date), levels=c("일요일","월요일","화요일","수요일","목요일","금요일", "토요일")),
         time=car::recode(as.numeric(evt_ocr_hh), "0:6='새벽'; 6:9='아침'; 9:12='오전 낮'; 12:17='오후 낮'; 17:20='저녁'; 20:24='밤'"),
         season=car::recode(as.numeric(evt_ocr_mm), "3:5='봄'; 6:8='여름'; 9:11='가을'; c(12,1,2)='겨울'"),
         weekend_weekday=car::recode(dayofweek, "c('일요일','토요일')='주말'; c('월요일','화요일','수요일','목요일','금요일')='주중'"),
         duration = car::recode(round((ymd_hms(evt_end_ymd_hms) - ymd_hms(evt_ocr_ymd_hms)) / 60, 2),
                                "0:15='15분 미만';15:30='15분~30분미만';30:45='30분~45분미만';45:60='45분~60분미만';60:75='60분~75분미만';75:100000000000000000000000000000000000000000='75분이상'"),
         evt_sub_code = ifelse(trms_sys_cd=="WES", "기상특보", 
                               ifelse(trms_sys_cd=="APL", "대기특보", 
                                      ifelse(trms_sys_cd=="LSS", "산사태경보", 
                                             ifelse(trms_sys_cd=="WLS", "수위경보", 
                                                    ifelse(trms_sys_cd=="WPC", "수질경보",
                                                           ifelse(evt_id=='119UC001' & evt_id_sub_cd=="화재", "화재",
                                                                  ifelse(evt_id=='112UC001' & evt_id_sub_cd=="화재","타기관 기타",
                                                                         ifelse(evt_id_sub_cd %in% c("내용확인불가","화재","구조요청","소음" ,"노점상", "기타_타기관","서비스요청","청탁금지법"), "타기관 기타",
                                                                                ifelse(evt_id_sub_cd %in% c("상담문의","변사자","비상벨","경비업체요청" ,"가출 등", "분실습득", "FTX", "자살", "실종(실종아동 등)","미귀가자"), "기타경찰업무",
                                                                                       ifelse(evt_id_sub_cd %in% c("교통사고","교통불편","교통위반","사망·대형사고" ,"인피도주"), "교통",
                                                                                              ifelse(evt_id_sub_cd %in% c("시비","행패소란","청소년비행","무전취식승차","주취자","질서유지","보호조치","위험방지","기타경범"), "질서유지",
                                                                                                     ifelse(evt_id_sub_cd %in% c("폭력","사기","공갈","협박","도박","재물손괴","주거침입","풍속영업","수배불심자","기타형사범","데이트폭력"), "기타범죄",
                                                                                                            ifelse(evt_id_sub_cd %in% c("살인","강도","치기","절도","납치감금","성폭력","가정폭력","아동학대(가정내)","아동학대(기타)","강력범죄"), "중요범죄",
                                                                                                                   ifelse(evt_id_sub_cd == "구급", "구급",
                                                                                                                          ifelse(evt_id_sub_cd == "구조", "구조",
                                                                                                                                 ifelse(evt_id_sub_cd == "기타", "기타", "NA")))))))))))))))))


popdf1 <- popdf  ##데이터 복사
popdf1$dong.pop <- popdf1$val ##
popdf1$gu.pop <- c(rep(sum(popdf1$val[1:16]), 16), rep(sum(popdf1$val[17:33]), 17), rep(sum(popdf1$val[34:56]), 23), rep(sum(popdf1$val[57:67]), 11), rep(sum(popdf1$val[68:79]), 12))
popdf1$sigungu_nm2 <- c(rep("동구", 16), rep("중구", 17), rep("서구", 23), rep("유성구", 11), rep("대덕구", 12))
popdf1 <- popdf1[,c(1,4,5)]
raw2 <- raw
raw2 <- merge(raw, popdf1, by = 'adm_dr_cd')


donggu <- unique(raw2$gu.pop[raw2$sigungu_nm == "동구"])
junggu <- unique(raw2$gu.pop[raw2$sigungu_nm == "중구"])
seogu <- unique(raw2$gu.pop[raw2$sigungu_nm == "서구"])
daedukgu <- unique(raw2$gu.pop[raw2$sigungu_nm == "대덕구"])
yusunggu <- unique(raw2$gu.pop[raw2$sigungu_nm == "유성구"])

gupop <- data.frame(sigungu_nm = c("동구", "서구", "중구", "유성구", "대덕구"), pop = c(donggu, seogu, junggu, yusunggu, daedukgu))
dongpop <- popdf[, c(2,3)]
colnames(dongpop)[2] <- "pop"

#####################################################사용자 정의 함수#####################################################
Mode = function(x){
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}


stat.gu.dong_time <- function(df) {
  if (names(df)[1] == "sigungu_nm"){
    names(df)[1] <- c("구")}
  df$합 <- apply(df[2:ncol(df)], 1, sum)
  df$평균 <- round(apply(df[2:(ncol(df)-1)], 1, mean), 3)
  df$표준편차 <- round(apply(df[2:(ncol(df)-2)], 1, sd), 3)
  df$중앙값 <- round(apply(df[2:(ncol(df)-3)], 1, median), 3)
  df$최빈값 <- apply(df[2:(ncol(df)-4)], 1, Mode)
  return(invisible(df))
  }


stat.gudong_time <- function(df){
    names(df)[1:2] <- c("구", "동")
    df$합 <- apply(df[3:ncol(df)], 1, sum)
    df$평균 <- round(apply(df[3:(ncol(df)-1)], 1, mean), 3)
    df$표준편차 <- round(apply(df[3:(ncol(df)-2)], 1, sd), 3)
    df$중앙값 <- round(apply(df[3:(ncol(df)-3)], 1, median), 3)
    df$최빈값 <- apply(df[3:(ncol(df)-4)], 1, Mode)
  return(invisible(df))
    }


stat.gu.dong <- function(df) {
  if (names(df)[1] == "sigungu_nm"){
    names(df)[1] <- c("구")}
  df$합 <- apply(df[2:ncol(df)], 1, sum)
  df$평균 <- round(apply(df[2:(ncol(df)-1)], 1, mean), 3)
  df$표준편차 <- round(apply(df[2:(ncol(df)-2)], 1, sd), 3)
  df$중앙값 <- round(apply(df[2:(ncol(df)-3)], 1, median), 3)
  df$최빈값 <- apply(df[2:(ncol(df)-4)], 1, Mode)
  View(df)
}


stat.gudong <- function(df){
  names(df)[1:2] <- c("구", "동")
  df$합 <- apply(df[3:ncol(df)], 1, sum)
  df$평균 <- round(apply(df[3:(ncol(df)-1)], 1, mean), 3)
  df$표준편차 <- round(apply(df[3:(ncol(df)-2)], 1, sd), 3)
  df$중앙값 <- round(apply(df[3:(ncol(df)-3)], 1, median), 3)
  df$최빈값 <- apply(df[3:(ncol(df)-4)], 1, Mode)
  View(df)
  }


stat.gu.dong_pop <- function(df) {
  if (names(df)[1] == "sigungu_nm"){
    names(df)[1] <- c("구")}
  df$합 <- apply(df[2:(ncol(df)-1)], 1, sum)
  df$평균 <- round(apply(df[2:(ncol(df)-2)], 1, mean), 3)
  df$표준편차 <- round(apply(df[2:(ncol(df)-3)], 1, sd), 3)
  df$중앙값 <- round(apply(df[2:(ncol(df)-4)], 1, median), 3)
  df$최빈값 <- apply(df[2:(ncol(df)-5)], 1, Mode)
  return(invisible(df))}


stat.gudong_pop <- function(df){
  names(df)[1:2] <- c("구", "동")
  df$합 <- apply(df[3:(ncol(df)-1)], 1, sum)
  df$평균 <- round(apply(df[3:(ncol(df)-2)], 1, mean), 3)
  df$표준편차 <- round(apply(df[3:(ncol(df)-3)], 1, sd), 3)
  df$중앙값 <- round(apply(df[3:(ncol(df)-4)], 1, median), 3)
  df$최빈값 <- apply(df[3:(ncol(df)-5)], 1, Mode)
  return(invisible(df))}


evt_gu_common_func <- function(df, category){
  if (category == "season"){
    dtc.gu.계절 <- dcast(ddply(df, .(sigungu_nm, season), summarise, count = length(season)), sigungu_nm ~ season)
    dtc.gu.계절 <- dtc.gu.계절[, c(1,4,5,2,3)]
    dtc.gu.계절[is.na(dtc.gu.계절)] <- 0
    dtc.gu.name <- rbind(dtc.gu.계절[1], "전체")
    dtc.gu.계절2 <- cbind(dtc.gu.name, rbind(dtc.gu.계절[2:ncol(dtc.gu.계절)], sapply(dtc.gu.계절[2:ncol(dtc.gu.계절)], sum)))
    stat.gu.dong(dtc.gu.계절2)
  } else if (category == "dayofweek"){
    dtc.gu.요일 <- dcast(ddply(df, .(sigungu_nm, dayofweek), summarise, count = length(dayofweek)), sigungu_nm ~ dayofweek)
    dtc.gu.요일 <- dtc.gu.요일[, c(1,3,4,5,6,7,8,2)]
    dtc.gu.요일[is.na(dtc.gu.요일)] <- 0
    dtc.gu.name <- rbind(dtc.gu.요일[1], "전체")
    dtc.gu.요일2 <- cbind(dtc.gu.name, rbind(dtc.gu.요일[2:ncol(dtc.gu.요일)], sapply(dtc.gu.요일[2:ncol(dtc.gu.요일)], sum)))
    stat.gu.dong(dtc.gu.요일2)
  } else if (category == "daybyday"){
    daybyday(df, "gu")
  } else if (category == "weekend_weekday"){
    dtc.gu.주중말 <- dcast(ddply(df, .(sigungu_nm, weekend_weekday), summarise, count = length(weekend_weekday)), sigungu_nm ~ weekend_weekday)
    dtc.gu.주중말 <- dtc.gu.주중말[, c(1,3,2)]
    dtc.gu.주중말[is.na(dtc.gu.주중말)] <- 0
    dtc.gu.name <- rbind(dtc.gu.주중말[1], "전체")
    dtc.gu.주중말2 <- cbind(dtc.gu.name, rbind(dtc.gu.주중말[2:ncol(dtc.gu.주중말)], sapply(dtc.gu.주중말[2:ncol(dtc.gu.주중말)], sum)))
    stat.gu.dong(dtc.gu.주중말2)
  } else if (category == "weekbyweek"){
    weekbyweek(df, "gu")
  } else if (category == "time"){
    dtc.gu.시간대 <- dcast(ddply(df, .(sigungu_nm, time), summarise, count = length(time)), sigungu_nm ~ time)
    dtc.gu.시간대 <- dtc.gu.시간대[, c(1,3,4,5,6,7,2)]
    dtc.gu.시간대[is.na(dtc.gu.시간대)] <- 0
    dtc.gu.name <- rbind(dtc.gu.시간대[1], "전체")
    dtc.gu.시간대2 <- cbind(dtc.gu.name, rbind(dtc.gu.시간대[2:ncol(dtc.gu.시간대)], sapply(dtc.gu.시간대[2:ncol(dtc.gu.시간대)], sum)))
    stat.gu.dong(dtc.gu.시간대2)
  } else if (category == "timebytime") {
    dtc.gu.시간대시간 <- dcast(ddply(df, .(sigungu_nm, time), summarise, count = length(time)), sigungu_nm ~ time)
    dtc.gu.시간대시간[is.na(dtc.gu.시간대시간)] <- 0
    gu.tname <- rbind(dtc.gu.시간대시간[1], "전체")
    dtc.gu.시간대시간2 <- cbind(gu.tname, rbind(dtc.gu.시간대시간[2:ncol(dtc.gu.시간대시간)], sapply(dtc.gu.시간대시간[2:ncol(dtc.gu.시간대시간)], sum)))
    timebytime(stat.gu.dong_time(dtc.gu.시간대시간2), "gu")
  } else if (category == 'duration'){
    dtc.gu.소요시간 <- dcast(ddply(df, .(sigungu_nm, duration), summarise, count = length(duration)), sigungu_nm ~ duration)
    if (ncol(dtc.gu.소요시간) == 7) {
      dtc.gu.소요시간 <- dcast(ddply(df, .(sigungu_nm, duration), summarise, count = length(duration)), sigungu_nm ~ duration)
      dtc.gu.소요시간$`15분~30분미만` <- apply(dtc.gu.소요시간[c(3,ncol(dtc.gu.소요시간))], 1, sum)
      dtc.gu.소요시간 <- dtc.gu.소요시간[!colnames(dtc.gu.소요시간) == "NA"]
      dtc.gu.소요시간[is.na(dtc.gu.소요시간)] <- 0
      dtc.gu.name <- rbind(dtc.gu.소요시간[1], "전체")
      dtc.gu.소요시간2 <- cbind(dtc.gu.name, rbind(dtc.gu.소요시간[2:ncol(dtc.gu.소요시간)], sapply(dtc.gu.소요시간[2:ncol(dtc.gu.소요시간)], sum)))
      stat.gu.dong(dtc.gu.소요시간2)
    } else if (ncol(dtc.gu.소요시간) != 7){
      dtc.gu.소요시간 <- dcast(ddply(df, .(sigungu_nm, duration), summarise, count = length(duration)), sigungu_nm ~ duration)
      dtc.gu.소요시간 <- dtc.gu.소요시간[!colnames(dtc.gu.소요시간) == "NA"]
      dtc.gu.소요시간[is.na(dtc.gu.소요시간)] <- 0
      dtc.gu.name <- rbind(dtc.gu.소요시간[1], "전체")
      dtc.gu.소요시간2 <- cbind(dtc.gu.name, rbind(dtc.gu.소요시간[2:ncol(dtc.gu.소요시간)], sapply(dtc.gu.소요시간[2:ncol(dtc.gu.소요시간)], sum)))
      stat.gu.dong(dtc.gu.소요시간2)
    }
  }
}


evt_all_common_func <- function(df, category){
  if (category == "season"){
    dtc.계절 <- dcast(ddply(df, .(sigungu_nm, adm_dr_nm, season), summarise, count = length(season)), sigungu_nm + adm_dr_nm ~ season)
    dtc.계절 <- dtc.계절[, c(1,2,5,6,3,4)]
    dtc.계절[is.na(dtc.계절)] <- 0
    dtc.name <- rbind(dtc.계절[1:2], c("전체", "통계"))
    dtc.계절2 <- cbind(dtc.name, rbind(dtc.계절[3:ncol(dtc.계절)], sapply(dtc.계절[3:ncol(dtc.계절)], sum)))
    stat.gudong(dtc.계절2)
  } else if (category == "dayofweek"){
    dtc.요일 <- dcast(ddply(df, .(sigungu_nm, adm_dr_nm, dayofweek), summarise, count = length(dayofweek)), sigungu_nm + adm_dr_nm  ~ dayofweek)
    dtc.요일 <- dtc.요일[, c(1,2,4,5,6,7,8,9,3)]
    dtc.요일[is.na(dtc.요일)] <- 0
    dtc.name <- rbind(dtc.요일[1:2], c("전체", "통계"))
    dtc.요일2 <- cbind(dtc.name, rbind(dtc.요일[3:ncol(dtc.요일)], sapply(dtc.요일[3:ncol(dtc.요일)], sum)))
    stat.gudong(dtc.요일2)
  } else if (category == "daybyday"){
    daybyday(df, "all")
  } else if (category == "weekend_weekday"){
    dtc.주중말 <- dcast(ddply(df, .(sigungu_nm, adm_dr_nm, weekend_weekday), summarise, count = length(weekend_weekday)), sigungu_nm + adm_dr_nm  ~ weekend_weekday)
    dtc.주중말 <- dtc.주중말[, c(1,2,4,3)]
    dtc.주중말[is.na(dtc.주중말)] <- 0
    dtc.name <- rbind(dtc.주중말[1:2], c("전체", "통계"))
    dtc.주중말2 <- cbind(dtc.name, rbind(dtc.주중말[3:ncol(dtc.주중말)], sapply(dtc.주중말[3:ncol(dtc.주중말)], sum)))
    stat.gudong(dtc.주중말2)
  } else if (category == "weekbyweek"){
    weekbyweek(df, "all")
  } else if (category == "time"){
    dtc.시간대 <- dcast(ddply(df, .(sigungu_nm, adm_dr_nm, time), summarise, count = length(time)), sigungu_nm + adm_dr_nm  ~ time)
    dtc.시간대 <- dtc.시간대[, c(1,2,4,5,6,7,8,3)]
    dtc.시간대[is.na(dtc.시간대)] <- 0
    dtc.name <- rbind(dtc.시간대[1:2], c("전체", "통계"))
    dtc.시간대2 <- cbind(dtc.name, rbind(dtc.시간대[3:ncol(dtc.시간대)], sapply(dtc.시간대[3:ncol(dtc.시간대)], sum)))
    stat.gudong(dtc.시간대2)
  } else if (category == 'timebytime') {
    dtc.시간대시간 <- dcast(ddply(df, .(sigungu_nm, adm_dr_nm, time), summarise, count = length(time)), sigungu_nm + adm_dr_nm  ~ time)
    dtc.시간대시간[is.na(dtc.시간대시간)] <- 0
    dtc.time.name <- rbind(dtc.시간대시간[1:2], c("전체", "통계"))
    dtc.시간대시간2 <- cbind(dtc.time.name, rbind(dtc.시간대시간[3:ncol(dtc.시간대시간)], sapply(dtc.시간대시간[3:ncol(dtc.시간대시간)], sum)))
    timebytime(stat.gudong_time(dtc.시간대시간2), "all")
  } else if (category == 'duration'){
    dtc.소요시간 <- dcast(ddply(df, .(sigungu_nm, adm_dr_nm, duration), summarise, count = length(duration)), sigungu_nm + adm_dr_nm  ~ duration)
    if (ncol(dtc.소요시간) == 8) {
      dtc.소요시간$`15분~30분미만` <- apply(dtc.소요시간[c(4,ncol(dtc.소요시간))], 1, sum)
      dtc.소요시간 <- dtc.소요시간[!colnames(dtc.소요시간) == "NA"]
      dtc.소요시간[is.na(dtc.소요시간)] <- 0
      dtc.소요시간[is.na(dtc.소요시간)] <- 0
      dtc.name <- rbind(dtc.소요시간[1:2], c("전체", "통계"))
      dtc.소요시간2 <- cbind(dtc.name, rbind(dtc.소요시간[3:ncol(dtc.소요시간)], sapply(dtc.소요시간[3:ncol(dtc.소요시간)], sum)))
      stat.gudong(dtc.소요시간2)
    } else if (ncol(dtc.소요시간) != 8){
      dtc.소요시간 <- dtc.소요시간[!colnames(dtc.소요시간) == "NA"]
      dtc.소요시간[is.na(dtc.소요시간)] <- 0
      dtc.name <- rbind(dtc.소요시간[1:2], c("전체", "통계"))
      dtc.소요시간2 <- cbind(dtc.name, rbind(dtc.소요시간[3:ncol(dtc.소요시간)], sapply(dtc.소요시간[3:ncol(dtc.소요시간)], sum)))
      stat.gudong(dtc.소요시간2)
    }
  }
}


evt_gudong_func <- function(df, category, unit) {
  if (unit == "gu"){
    if(category == "all"){
      dtc.gu.sub <- dcast(ddply(df, .(sigungu_nm, evt_sub_code), summarise, count = length(evt_sub_code)), sigungu_nm ~ evt_sub_code)
      dtc.gu.sub[is.na(dtc.gu.sub)] <- 0
      dtc.gu.name <- rbind(dtc.gu.sub[1], "전체")
      dtc.gu.sub2 <- cbind(dtc.gu.name, rbind(dtc.gu.sub[2:ncol(dtc.gu.sub)], sapply(dtc.gu.sub[2:ncol(dtc.gu.sub)], sum)))
      stat.gu.dong(dtc.gu.sub2)
    }
    else if (category != "all"){
      evt_gu_common_func(df, category)
    }
  }
  else if (unit == "all"){
    if (category == "all") {
      dtc.sub <- dcast(ddply(df, .(sigungu_nm, adm_dr_nm, evt_sub_code), summarise, count = length(evt_sub_code)), sigungu_nm + adm_dr_nm ~ evt_sub_code)
      dtc.sub[is.na(dtc.sub)] <- 0
      dtc.name <- rbind(dtc.sub[1:2], c("전체", "통계"))
      dtc.sub2 <- cbind(dtc.name, rbind(dtc.sub[3:ncol(dtc.sub)], sapply(dtc.sub[3:ncol(dtc.sub)], sum)))
      stat.gudong(dtc.sub2)
    }
    else if (category != "all"){
      evt_all_common_func(df, category)
    }
  }
}


evt_all_func <- function(df, category, unit) {
  if (unit == "gu"){
    if(category == "all"){
      dtc.gu.all <- dcast(ddply(df, .(sigungu_nm, evt_id), summarise, 합 = sum(one)), sigungu_nm ~ evt_id)
      colnames(dtc.gu.all) <- c('구', '112센터 긴급영상 제공서비스', '119 긴급출동 지원서비스', '사회적 약자 지원서비스')
      dtc.gu.all[is.na(dtc.gu.all)] <- 0
      dtc.gu.name <- rbind(dtc.gu.all[1], "전체")
      dtc.gu.all2 <- cbind(dtc.gu.name, rbind(dtc.gu.all[2:ncol(dtc.gu.all)], sapply(dtc.gu.all[2:ncol(dtc.gu.all)], sum)))
      stat.gu.dong(dtc.gu.all2)
    }
    else if (category != "all"){
      evt_gu_common_func(df, category)
    }
  }
  else if (unit == "all"){
    if (category == "all") {
      dtc.all <- dcast(ddply(df, .(sigungu_nm, adm_dr_nm, evt_id), summarise, 합 = sum(one)), sigungu_nm + adm_dr_nm ~ evt_id)
      colnames(dtc.all) <- c('구', '동', '112센터 긴급영상 제공서비스', '119 긴급출동 지원서비스', '사회적 약자 지원서비스')
      dtc.all[is.na(dtc.all)] <- 0
      dtc.name <- rbind(dtc.all[1:2], c("전체", "통계"))
      dtc.all2 <- cbind(dtc.name, rbind(dtc.all[3:ncol(dtc.all)], sapply(dtc.all[3:ncol(dtc.all)], sum)))
      stat.gudong(dtc.all2)
    }
    else if (category != "all"){
      evt_all_common_func(df, category)
    }
  }
}


evt_pop112_func <- function(df, unit, fromdate, todate){
  if(unit == "gu"){
    dtc.gu.인구 <- dcast(ddply(df, .(sigungu_nm, evt_sub_code), summarise, count = length(evt_sub_code)), sigungu_nm ~ evt_sub_code)
    dtc.gu.인구 <- merge(dtc.gu.인구, gupop, by = "sigungu_nm")
    dtc.gu.인구[is.na(dtc.gu.인구)] <- 0
    gu.tname <- rbind(dtc.gu.인구[1], "전체")
    dtc.gu.인구2 <- cbind(gu.tname, rbind(dtc.gu.인구[2:ncol(dtc.gu.인구)], sapply(dtc.gu.인구[2:ncol(dtc.gu.인구)], sum)))
    basicstatpop112(stat.gu.dong_pop(dtc.gu.인구2), "gu", fromdate, todate)
  } else if (unit == "all"){
    dtc.인구 <- dcast(ddply(df, .(sigungu_nm, adm_dr_nm, evt_sub_code), summarise, count = length(evt_sub_code)), sigungu_nm + adm_dr_nm  ~ evt_sub_code)
    dtc.인구 <- merge(dtc.인구, dongpop, by = "adm_dr_nm")
    dtc.인구 <- dtc.인구[,c(2,1,3:ncol(dtc.인구))] %>% arrange(sigungu_nm,adm_dr_nm)
    dtc.인구[is.na(dtc.인구)] <- 0
    dtc.pop.name <- rbind(dtc.인구[1:2], c("전체", "통계"))
    dtc.인구2 <- cbind(dtc.pop.name, rbind(dtc.인구[3:ncol(dtc.인구)], sapply(dtc.인구[3:ncol(dtc.인구)], sum)))
    basicstatpop112(stat.gudong_pop(dtc.인구2), "all",fromdate, todate)
  }
}


evt_pop119_func <- function(df, unit, fromdate, todate){
  if(unit == "gu"){
    dtc.gu.인구 <- dcast(ddply(df, .(sigungu_nm, evt_sub_code), summarise, count = length(evt_sub_code)), sigungu_nm ~ evt_sub_code)
    dtc.gu.인구 <- merge(dtc.gu.인구, gupop, by = "sigungu_nm")
    dtc.gu.인구[is.na(dtc.gu.인구)] <- 0
    gu.tname <- rbind(dtc.gu.인구[1], "전체")
    dtc.gu.인구2 <- cbind(gu.tname, rbind(dtc.gu.인구[2:ncol(dtc.gu.인구)], sapply(dtc.gu.인구[2:ncol(dtc.gu.인구)], sum)))
    basicstatpop119(stat.gu.dong_pop(dtc.gu.인구2), "gu", fromdate, todate)
  } else if (unit == "all"){
    dtc.인구 <- dcast(ddply(df, .(sigungu_nm, adm_dr_nm, evt_sub_code), summarise, count = length(evt_sub_code)), sigungu_nm + adm_dr_nm  ~ evt_sub_code)
    dtc.인구 <- merge(dtc.인구, dongpop, by = "adm_dr_nm")
    dtc.인구[is.na(dtc.인구)] <- 0
    dtc.인구 <- dtc.인구[,c(2,1,3:ncol(dtc.인구))] %>% arrange(sigungu_nm,adm_dr_nm)
    dtc.pop.name <- rbind(dtc.인구[1:2], c("전체", "통계"))
    dtc.인구2 <- cbind(dtc.pop.name, rbind(dtc.인구[3:ncol(dtc.인구)], sapply(dtc.인구[3:ncol(dtc.인구)], sum)))
    basicstatpop119(stat.gudong_pop(dtc.인구2), "all", fromdate, todate)
  }
}


evt_popall_func <- function(df, unit, fromdate, todate){
  if(unit == "gu"){
    dtc.gu.인구 <- dcast(ddply(df, .(sigungu_nm, trms_sys_cd), summarise, count = length(trms_sys_cd)), sigungu_nm ~ trms_sys_cd)
    dtc.gu.인구 <- merge(dtc.gu.인구, gupop, by = "sigungu_nm")
    dtc.gu.인구[is.na(dtc.gu.인구)] <- 0
    gu.tname <- rbind(dtc.gu.인구[1], "전체")
    dtc.gu.인구2 <- cbind(gu.tname, rbind(dtc.gu.인구[2:ncol(dtc.gu.인구)], sapply(dtc.gu.인구[2:ncol(dtc.gu.인구)], sum)))
    basicstatpopall(stat.gu.dong_pop(dtc.gu.인구2), "gu", fromdate, todate)
  } else if (unit == "all"){
    dtc.인구 <- dcast(ddply(df, .(sigungu_nm, adm_dr_nm, trms_sys_cd), summarise, count = length(trms_sys_cd)), sigungu_nm + adm_dr_nm  ~ trms_sys_cd)
    dtc.인구 <- merge(dtc.인구, dongpop, by = "adm_dr_nm")
    dtc.인구 <- dtc.인구[,c(2,1,3:ncol(dtc.인구))] %>% arrange(sigungu_nm,adm_dr_nm)
    dtc.인구[is.na(dtc.인구)] <- 0
    dtc.pop.name <- rbind(dtc.인구[1:2], c("전체", "통계"))
    dtc.인구2 <- cbind(dtc.pop.name, rbind(dtc.인구[3:ncol(dtc.인구)], sapply(dtc.인구[3:ncol(dtc.인구)], sum)))
    basicstatpopall(stat.gudong_pop(dtc.인구2), "all", fromdate, todate)
  }
}
  
  
timebytime <- function(df, unit) {
  df$`새벽 시간당 발생건수` <- (df$`새벽`) / 6
  df$`아침 시간당 발생건수` <- (df$`아침`) / 3
  df$`오전 시간당 발생건수` <- (df$`오전 낮`) / 3
  df$`오후 시간당 발생건수` <- (df$`오후 낮`) / 5
  df$`저녁 시간당 발생건수` <- (df$`저녁`) / 3
  df$`밤 시간당 발생건수` <- (df$`밤`) / 4
  if(unit == "gu"){
    df[, c(13:18)] <- round(df[, c(13:18)], 3)
    df <- df[,c(1,3,13,4,14,5,15,6,16,7,17,2,18,8,9,10,11,12)]
  } else if (unit == "all"){
    df[, c(14:19)] <- round(df[, c(14:19)], 3)
    df <- df[,c(1,2,4,14,5,15,6,16,7,17,8,18,3,19,9,10,11,12,13)]
  }
  View(df)}




daybyday <- function(df, unit) {
  if (unit == "gu"){
    dtc.gu.요일별1일당 <- dcast(ddply(df, .(sigungu_nm, dayofweek), summarise, count = length(dayofweek)), sigungu_nm ~ dayofweek)
    dtc.gu.요일별1일당[is.na(dtc.gu.요일별1일당)] <- 0
    gu.dname <- rbind(dtc.gu.요일별1일당[1], "전체")
    dtc.gu.요일별1일당2 <- cbind(gu.dname, rbind(dtc.gu.요일별1일당[2:ncol(dtc.gu.요일별1일당)], sapply(dtc.gu.요일별1일당[2:ncol(dtc.gu.요일별1일당)], sum)))
    df2 <- stat.gu.dong_time(dtc.gu.요일별1일당2)
    df2$`일요일 평균 발생건수` <- df2$일요일 / nrow(df[df$dayofweek == '일요일',])
    df2$`월요일 평균 발생건수` <- df2$월요일 / nrow(df[df$dayofweek == '월요일',])
    df2$`화요일 평균 발생건수` <- df2$화요일 / nrow(df[df$dayofweek == '화요일',])
    df2$`수요일 평균 발생건수` <- df2$수요일 / nrow(df[df$dayofweek == '수요일',])
    df2$`목요일 평균 발생건수` <- df2$목요일 / nrow(df[df$dayofweek == '목요일',])
    df2$`금요일 평균 발생건수` <- df2$금요일 / nrow(df[df$dayofweek == '금요일',])
    df2$`토요일 평균 발생건수` <- df2$토요일 / nrow(df[df$dayofweek == '토요일',])
    df2[, c(14:20)]<-round(df2[, c(14:20)], 3)
    df2 <- df2[,c(1,3,15,4,16,5,17,6,18,7,19,8,20,2,14,9,10,11,12,13)]
  } else if (unit == "all"){
    dtc.요일별1일당 <- dcast(ddply(df, .(sigungu_nm, adm_dr_nm, dayofweek), summarise, count = length(dayofweek)), sigungu_nm + adm_dr_nm  ~ dayofweek)
    dtc.요일별1일당[is.na(dtc.요일별1일당)] <- 0
    dtc.dname <- rbind(dtc.요일별1일당[1:2], c("전체", "통계"))
    dtc.요일별1일당2 <- cbind(dtc.dname, rbind(dtc.요일별1일당[3:ncol(dtc.요일별1일당)], sapply(dtc.요일별1일당[3:ncol(dtc.요일별1일당)], sum)))
    df2 <- stat.gudong_time(dtc.요일별1일당2)
    df2$`일요일 평균 발생건수` <- df2$일요일 / nrow(df[df$dayofweek == '일요일',])
    df2$`월요일 평균 발생건수` <- df2$월요일 / nrow(df[df$dayofweek == '월요일',])
    df2$`화요일 평균 발생건수` <- df2$화요일 / nrow(df[df$dayofweek == '화요일',])
    df2$`수요일 평균 발생건수` <- df2$수요일 / nrow(df[df$dayofweek == '수요일',])
    df2$`목요일 평균 발생건수` <- df2$목요일 / nrow(df[df$dayofweek == '목요일',])
    df2$`금요일 평균 발생건수` <- df2$금요일 / nrow(df[df$dayofweek == '금요일',])
    df2$`토요일 평균 발생건수` <- df2$토요일 / nrow(df[df$dayofweek == '토요일',])
    df2[, c(15:21)] <- round(df2[, c(15:21)], 3)
    df2 <- df2[,c(1,2,4,16,5,17,6,18,7,19,8,20,9,21,3,15,10,11,12,13,14)]
  }
  View(df2)
}


weekbyweek <- function(df, unit) {
  wd.total <- nrow(df[df$dayofweek == '월요일',]) + nrow(df[df$dayofweek == '화요일',]) + nrow(df[df$dayofweek == '수요일',]) + nrow(df[df$dayofweek == '목요일',]) + nrow(df[df$dayofweek == '금요일',])
  wk.total <- nrow(df[df$dayofweek == '일요일',]) + nrow(df[df$dayofweek == '토요일',])
  if(unit == "gu"){
    dtc.gu.주중주말1일당 <- dcast(ddply(df, .(sigungu_nm, weekend_weekday), summarise, count = length(weekend_weekday)), sigungu_nm ~ weekend_weekday)
    dtc.gu.주중주말1일당[is.na(dtc.gu.주중주말1일당)] <- 0
    gu.wname <- rbind(dtc.gu.주중주말1일당[1], "전체")
    dtc.gu.주중주말1일당2 <- cbind(gu.wname, rbind(dtc.gu.주중주말1일당[2:ncol(dtc.gu.주중주말1일당)], sapply(dtc.gu.주중주말1일당[2:ncol(dtc.gu.주중주말1일당)], sum)))
    df2 <- stat.gu.dong_time(dtc.gu.주중주말1일당2)
    df2$`주중 1일당 발생건수` <- (df2$주중) / wd.total
    df2$`주말 1일당 발생건수` <- (df2$주말) / wk.total
    df2[, c(9:10)] <- round(df2[, c(9:10)], 3)
    df2 <- df2[, c(1,3,9,2,10,4,5,6,7,8)]
  } else if (unit == "all"){
    dtc.주중주말1일당 <- dcast(ddply(df, .(sigungu_nm, adm_dr_nm, weekend_weekday), summarise, count = length(weekend_weekday)), sigungu_nm + adm_dr_nm  ~ weekend_weekday)
    dtc.주중주말1일당[is.na(dtc.주중주말1일당)] <- 0
    dtc.wname <- rbind(dtc.주중주말1일당[1:2], c("전체", "통계"))
    dtc.주중주말1일당2 <- cbind(dtc.wname, rbind(dtc.주중주말1일당[3:ncol(dtc.주중주말1일당)], sapply(dtc.주중주말1일당[3:ncol(dtc.주중주말1일당)], sum)))
    df2 <- stat.gudong_time(dtc.주중주말1일당2)
    df2$`주중 1일당 발생건수` <- (df2$주중) / wd.total
    df2$`주말 1일당 발생건수` <- (df2$주말) / wk.total
    df2[, c(10:11)] <- round(df2[, c(10:11)], 3)
    df2 <- df2[, c(1,2,4,10,3,11,5,6,7,8,9)]
  }
  View(df2)
}


basicstatpop112 <- function(df, unit, fromdate, todate){
  df$개월수 <- round(as.numeric(as.Date(todate)-as.Date(fromdate)) / 30)
  df$중요범죄인구당 <- round((df$중요범죄)/(df$pop)/df$개월수, 5)
  df$기타범죄인구당 <- round((df$기타범죄)/(df$pop)/df$개월수, 5)
  df$질서유지인구당 <- round((df$질서유지)/(df$pop)/df$개월수, 5)
  df$교통인구당 <- round((df$교통)/(df$pop)/df$개월수, 5)
  df$기타경찰업무인구당 <- round((df$기타경찰업무)/(df$pop)/df$개월수, 5)
  df$타기관기타인구당 <- round((df$`타기관 기타`)/(df$pop)/df$개월수, 5)
  df$전체인구당 <- round((df$합)/(df$pop)/df$개월수, 3)
  if (unit == "gu"){
    df <- df[,c(1,8,14,5,15,4,16,6,17,2,18,3,19,7,20,9,21)]
    colnames(df)[2] <- "인구수"
  } else if (unit == "all"){
    df <- df[,c(1,2,9,15,6,16,5,17,7,18,3,19,4,20,8,21,10,22)]
    colnames(df)[3] <- "인구수"
  }
  View(df)
}


basicstatpop119<-function(df, unit, fromdate, todate){
  df$개월수<-round(as.numeric(as.Date(todate)-as.Date(fromdate))/30)
  df$화재인구당<- round((df$화재)/(df$pop)/df$개월수, 5)
  df$구급인구당<- round((df$구급)/(df$pop)/df$개월수, 5)
  df$구조인구당<- round((df$구조)/(df$pop)/df$개월수, 5)
  df$기타인구당<- round((df$기타)/(df$pop)/df$개월수, 5)
  df$전체인구당<-round((df$합)/(df$pop)/df$개월수, 5)
  if (unit == "gu"){
    df <- df[,c(1,6,12,5,13,2,14,3,15,4,16,7,17)]
    colnames(df)[2] <- "인구수"
  } else if (unit == "all"){
    df <- df[,c(1,2,7,13,6,14,3,15,4,16,5,17,8,18)]
    colnames(df)[3] <- "인구수"
  }
  View(df)
}


basicstatpopall <- function(df, unit, fromdate, todate){
  df$개월수 <- round(as.numeric(as.Date(todate)-as.Date(fromdate))/30)
  df$`112인구당` <- round((df$`112`)/(df$pop)/df$개월수, 5)
  df$`119인구당` <- round((df$`119`)/(df$pop)/df$개월수, 5)
  df$사회적약자인구당 <- round((df$`WP1`)/(df$pop)/df$개월수, 5)
  df$전체인구당<-round((df$합)/(df$pop)/df$개월수, 5)
  if (unit == "gu"){
    df <- df[,c(1,5,11,2,12,3,13,4,14,6,15)]
    colnames(df)[8] <- "사회적약자"
    colnames(df)[2] <- "인구수"
  } else if (unit == "all"){
    df <- df[,c(1,2,6,12,3,13,4,14,5,15,7,16)]
    colnames(df)[9] <- "사회적약자"
    colnames(df)[3] <- "인구수"
  }
  View(df)
}  


basicstatpop <- function(df, eventcode, unit, fromdate, todate){
  if (eventcode == "112"){
    if (unit == 'gu'){
      df$mes <- round(as.numeric(as.Date(todate)-as.Date(fromdate))/30)
      df$중요범죄인구당 <- (df$중요범죄)/(df$gu.pop)/df$mes
      df$기타범죄인구당 <- (df$기타범죄)/(df$gu.pop)/df$mes
      df$질서유지인구당 <- (df$질서유지)/(df$gu.pop)/df$mes
      df$교통인구당 <- (df$교통)/(df$gu.pop)/df$mes
      df$기타경찰업무인구당 <- (df$기타경찰업무)/(df$gu.pop)/df$mes
      df$타기관기타인구당 <- (df$타기관기타)/(df$gu.pop)/df$mes

    } else if (unit == 'all'){
      df$mes <- round(as.numeric(as.Date(todate)-as.Date(fromdate))/30)
      df$중요범죄인구당 <- (df$중요범죄)/(df$dong.pop)/df$mes
      df$기타범죄인구당 <- (df$기타범죄)/(df$dong.pop)/df$mes
      df$질서유지인구당 <- (df$질서유지)/(df$dong.pop)/df$mes
      df$교통인구당 <- (df$교통)/(df$dong.pop)/df$mes
      df$기타경찰업무인구당 <- (df$기타경찰업무)/(df$dong.pop)/df$mes
      df$타기관기타인구당 <- (df$타기관기타)/(df$dong.pop)/df$mes
    } else if (eventcode == "119"){
      if (unit == 'gu'){
        df$mes <- round(as.numeric(as.Date(todate)-as.Date(fromdate))/30)
        df$화재인구당 <- (df$화재)/(df$gu.pop)/df$mes
        df$구급인구당 <- (df$구급)/(df$gu.pop)/df$mes
        df$구조인구당 <- (df$구조)/(df$gu.pop)/df$mes
        df$기타인구당 <- (df$기타)/(df$gu.pop)/df$mes

      } else if (unit == 'all'){
        df$mes <- round(as.numeric(as.Date(todate)-as.Date(fromdate))/30)
        df$화재인구당 <- (df$중요범죄)/(df$dong.pop)/df$mes
        df$구급인구당 <- (df$기타범죄)/(df$dong.pop)/df$mes
        df$구조인구당 <- (df$질서유지)/(df$dong.pop)/df$mes
        df$기타인구당 <- (df$교통)/(df$dong.pop)/df$mes
        df$기타경찰업무인구당 <- (df$기타경찰업무)/(df$dong.pop)/df$mes
        df$타기관기타인구당 <- (df$타기관기타)/(df$dong.pop)/df$mes
      }
    }
  }
}

#####################################################사용자 정의 함수#####################################################






##########################################################basicstat##########################################################
# 인자 : R.script --v/--c
args <- commandArgs(trailingOnly = TRUE)
VorC <- args[1]
eventcodecode <- args[2]
category <- args[3]
unit <- args[4]
fromdate <- args[5]
todate <- args[6]

# 1) --v or --c
# 2) all, 119, 112, 사회적약자
# 3) all / dayofweek / daybyday / time / timebytime / season / weekend_weekday / weekbyweek / duration / pop
# 4) all / gu
# 5) fromdate "2000-01-01"
# 6) todate "2020-12-31"

basicstat <- function(eventcode, category, unit, fromdate, todate){
  data <- raw
  data1 <- select(data, evt_id, evt_sub_code, evt_id_sub_cd, sigungu_nm, adm_dr_nm, trms_sys_cd, date, dayofweek, time, season, weekend_weekday, duration)
  data1 <- data1 %>%
    filter(evt_id %in% c('119UC001', '112UC001', 'WPSSF110')) %>%
    dplyr::filter(date > fromdate & date < todate) %>%
    group_by(sigungu_nm, adm_dr_nm) %>%
    arrange(sigungu_nm, adm_dr_nm)
  if (eventcode == "all"){
    data2 <- data1
    data2$one <- 1 # for sum
    if (category !='pop'){
      evt_all_func(data2, category, unit)
    } else if (category == 'pop'){
      evt_popall_func(data2, unit, fromdate, todate)
    }
  } else if (eventcode == "112"){
    data2 <- filter(data1, evt_id == c("112UC001"))
    if (category != 'pop'){
      evt_gudong_func(data2, category, unit)
    } else if (category == 'pop'){
      evt_pop112_func(data2, unit, fromdate, todate)
    }
  } else if (eventcode == "119"){
    data2 <- filter(data1, evt_id == c("119UC001"))
    data2$one <- 1 # for sum
    if (category != 'pop'){
      evt_gudong_func(data2, category, unit)
    } else if (category == 'pop'){
      evt_pop119_func(data2, unit, fromdate, todate)
    }
  } else if (eventcode == "사회적약자") {
    data2 <- filter(data1, evt_id == c("WPSSF110"))
    data2$one <- 1 # for sum}
    if (category != 'pop'){
      evt_gudong_func(data2, category, unit)
    } else if (category == 'pop'){
      print("not yet")
    }
  }}

##########################################################basicstat##########################################################


# basicstat(eventcode, category, unit, fromdate, todate)
# 가: 지역별 이벤트 발생 현황 분석
# basicstat("all", "all", "gu", "2015-11-26", "2017-10-25")
# basicstat("all", "all", "all", "2015-11-26", "2017-10-25")
# basicstat("119", "all", "gu", "2015-11-26", "2017-10-25")
# basicstat("119", "all", "all", "2015-11-26", "2017-10-25")
# basicstat("112", "all", "gu", "2015-11-26", "2017-10-25")
# basicstat("112", "all", "all", "2015-11-26", "2017-10-25")
# basicstat("사회적약자", "all", "gu", "2015-11-26", "2017-10-25")
# basicstat("사회적약자", "all", "all", "2015-11-26", "2017-10-25")
# 
# #나: 지역별 시간대 구분에 따른 이벤트 발생 현황 분석
# basicstat("all", "time", "gu", "2015-11-26", "2017-10-25")
# basicstat("all", "time", "all", "2015-11-26", "2017-10-25")
# basicstat("112", "time", "gu", "2015-11-26", "2017-10-25")
# basicstat("112", "time", "all", "2015-11-26", "2017-10-25")
# basicstat("119", "time", "gu", "2015-11-26", "2017-10-25")
# basicstat("119", "time", "all", "2015-11-26", "2017-10-25")
# basicstat("사회적약자", "time", "gu", "2015-11-26", "2017-10-25")
# basicstat("사회적약자", "time", "all", "2015-11-26", "2017-10-25")
# 
# #시간대별 시간당 이벤트 발생 건수
# basicstat("all", "timebytime", "gu", "2015-11-26", "2017-10-25")
# basicstat("all", "timebytime", "all", "2015-11-26", "2017-10-25")
# basicstat("112", "timebytime", "gu", "2015-11-26", "2017-10-25")
# basicstat("112", "timebytime", "all", "2015-11-26", "2017-10-25")
# basicstat("119", "timebytime", "gu", "2015-11-26", "2017-10-25")
# basicstat("119", "timebytime", "all", "2015-11-26", "2017-10-25")
# basicstat("사회적약자", "timebytime", "gu", "2015-11-26", "2017-10-25")
# basicstat("사회적약자", "timebytime", "all", "2015-11-26", "2017-10-25")
# 
# #다: 지역별 요인에 따른 이벤트 발생 현황 분석
# basicstat("all", "dayofweek", "gu", "2015-11-26", "2017-10-25")
# basicstat("all", "dayofweek", "all", "2015-11-26", "2017-10-25")
# basicstat("112", "dayofweek", "gu", "2015-11-26", "2017-10-25")
# basicstat("112", "dayofweek", "all", "2015-11-26", "2017-10-25")
# basicstat("119", "dayofweek", "gu", "2015-11-26", "2017-10-25")
# basicstat("119", "dayofweek", "all", "2015-11-26", "2017-10-25")
# basicstat("사회적약자", "dayofweek", "gu", "2015-11-26", "2017-10-25")
# basicstat("사회적약자", "dayofweek", "all", "2015-11-26", "2017-10-25")
# 
# #요일별 1일당 평균 이벤트 발생건수
# basicstat("all", "daybyday", "gu", "2015-11-26", "2017-10-25")
# basicstat("all", "daybyday", "all", "2015-11-26", "2017-10-25")
# basicstat("112", "daybyday", "gu", "2015-11-26", "2017-10-25")
# basicstat("112", "daybyday", "all", "2015-11-26", "2017-10-25")
# basicstat("119", "daybyday", "gu", "2015-11-26", "2017-10-25")
# basicstat("119", "daybyday", "all", "2015-11-26", "2017-10-25")
# basicstat("사회적약자", "daybyday", "gu", "2015-11-26", "2017-10-25")
# basicstat("사회적약자", "daybyday", "all", "2015-11-26", "2017-10-25")
# 
# # #라: 지역별 주중/주말에 따른 이벤트 발생 현황 분석
# basicstat("all", "weekend_weekday", "gu", "2015-11-26", "2017-10-25")
# basicstat("all", "weekend_weekday", "all", "2015-11-26", "2017-10-25")
# basicstat("112", "weekend_weekday", "gu", "2015-11-26", "2017-10-25")
# basicstat("112", "weekend_weekday", "all", "2015-11-26", "2017-10-25")
# basicstat("119", "weekend_weekday", "gu", "2015-11-26", "2017-10-25")
# basicstat("119", "weekend_weekday", "all", "2015-11-26", "2017-10-25")
# basicstat("사회적약자", "weekend_weekday", "gu", "2015-11-26", "2017-10-25")
# basicstat("사회적약자", "weekend_weekday", "all", "2015-11-26", "2017-10-25")
# 
# #주중/주말 1일당 이벤트 발생건수
# basicstat("all", "weekbyweek", "gu", "2015-11-26", "2017-10-25")
# basicstat("all", "weekbyweek", "all", "2015-11-26", "2017-10-25")
# basicstat("112", "weekbyweek", "gu", "2015-11-26", "2017-10-25")
# basicstat("112", "weekbyweek", "all", "2015-11-26", "2017-10-25")
# basicstat("119", "weekbyweek", "gu", "2015-11-26", "2017-10-25")
# basicstat("119", "weekbyweek", "all", "2015-11-26", "2017-10-25")
# basicstat("사회적약자", "weekbyweek", "gu", "2015-11-26", "2017-10-25")
# basicstat("사회적약자", "weekbyweek", "all", "2015-11-26", "2017-10-25")
# 
# #마: 지역별 계절에 따른 이벤트 발생 현황 분석
# basicstat("all", "season", "gu", "2015-11-26", "2017-10-25")
# basicstat("all", "season", "all", "2015-11-26", "2017-10-25")
# basicstat("112", "season", "gu", "2015-11-26", "2017-10-25")
# basicstat("112", "season", "all", "2015-11-26", "2017-10-25")
# basicstat("119", "season", "gu", "2015-11-26", "2017-10-25")
# basicstat("119", "season", "all", "2015-11-26", "2017-10-25")
# basicstat("사회적약자", "season", "gu", "2015-11-26", "2017-10-25")
# basicstat("사회적약자", "season", "all", "2015-11-26", "2017-10-25")
# 
# #바: 지역별 진행소요시간에 따른 이벤트 발생 현황 분석
# basicstat("all", "duration", "gu", "2015-11-26", "2017-10-25")
# basicstat("all", "duration", "all", "2015-11-26", "2017-10-25")
# basicstat("112", "duration", "gu", "2015-11-26", "2017-10-25")
# basicstat("112", "duration", "all", "2015-11-26", "2017-10-25")
# basicstat("119", "duration", "gu", "2015-11-26", "2017-10-25")
# basicstat("119", "duration", "all", "2015-11-26", "2017-10-25")
# basicstat("사회적약자", "duration", "gu", "2015-11-26", "2017-10-25")
# basicstat("사회적약자", "duration", "all", "2015-11-26", "2017-10-25")
# 
# #인구 1인당 이벤트 발생건수 분석
# basicstat("all", "pop", "gu", "2015-11-26", "2017-10-25")
# basicstat("all", "pop", "all", "2015-11-26", "2017-10-25")
# basicstat("112", "pop", "gu", "2015-11-26", "2017-10-25")
# basicstat("112", "pop", "all", "2015-11-26", "2017-10-25")
# basicstat("119", "pop", "gu", "2015-11-26", "2017-10-25")
# basicstat("119", "pop", "all", "2015-11-26", "2017-10-25")
# basicstat("사회적약자", "pop", "gu", "2015-11-26", "2017-10-25")
# basicstat("사회적약자", "pop", "all", "2015-11-26", "2017-10-25")
































