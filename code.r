# 设置工作路径
getwd()
setwd("D:/临时文件/Internet_Energy/datas")

# 加载R工具包
library(haven) # 数据导入
library(xlsx) # 数据导出
library(dplyr) # 数据预处理
library(psych) # 描述性统计
library(ggplot2) # 数据可视化
library(stargazer) # 结果输出

# 导入与合并数据
data1 <- read_stata("internet.dta")
data2 <- read_stata("energy.dta")
data3 <- read_stata("know.dta")
data <- merge(data1, data2, by = "interview_key", all = T)
data <- merge(data, data3, by = "interview_key", all = T)
rm(data1)
rm(data2)
rm(data3)

# 选入观测变量及观测样本
names(data)
data <- na.omit(data)
myvars <- data[c(1, 2, 3, 4, 6, 8, 9, 10, 11, 12, 13, 15, 25, 26, 34, 35, 45: 56, 57)]
data <- myvars
rm(myvars)

# 变量重命名
names(data)
names(data)[c(2: 4)] <- c("id", "province", "internet")
names(data)[5] <- "phone"
names(data)[c(10: 16)] <- c("cadres", "party", "nonagri", "income", "traffic", 
                            "area1", "area2")
names(data)[c(17: 21)] <- c("trad1", "trad2", "trad3", "trad4", "trad5")
names(data)[c(22: 27)] <- c("modern1", "modern2", "modern3", "modern4", "modern5", "eletricity")
names(data)[28] <- "other"
names(data)[29] <- "know"
names(data)
summary(data)

# 数据计算
attach(data)
data$south = ifelse(province %in% c("湖南省", "湖北省"), 1, 0)
data$trad <- trad1 + trad2
data$bio <- trad3 + trad4 + trad5
data$modern <- modern1 + modern2 + modern3 + modern4 + modern5
data$trad[data$trad != 0] <- 1
data$bio[data$bio != 0] <- 1
data$modern[data$modern != 0] <- 1
data$area <- area1 + area2
data$lnincome <- log(income)
data$phone <- phone / people
detach(data)

# 导出数据
write.xlsx(data, "data.xlsx")

# 描述性统计分析
describe(data)

# 基准回归模型——logistic回归模型
## 生物质能
bio_1 <- glm(bio ~ internet, data, family = binomial())
summary(bio_1)
bio_2 <- glm(bio ~ internet + age + health + education + cadres + party + nonagri, data, family = binomial())
summary(bio_2)
bio_3 <- glm(bio ~ internet + age + health + education + cadres + party + nonagri +
               lnincome + people + + area, data, family = binomial())
summary(bio_3)
bio_4 <- glm(bio ~ internet + age + health + education + cadres + party + nonagri +
               lnincome + people + area + traffic + south, data, family = binomial())
summary(bio_4)
p <- predict.glm(bio_4, type = "response")
log_odds <- log(p / (1 - p))
plot_data <- data.frame(
  internet = data$internet,
  log_odds = log_odds
)
ggplot(plot_data, aes(internet, log_odds)) +
  geom_point(alpha = 0.3, size = 5, color = "#d38d0b") +
  geom_smooth(color = "#1172c2e3", fill = "#54abf3") +
  theme_minimal()
## 低劣能源
trad_1 <- glm(trad ~ internet, data, family = binomial())
summary(trad_1)
trad_2 <- glm(trad ~ internet + age + health + education + cadres + party + nonagri, data, family = binomial())
summary(trad_2)
trad_3 <- glm(trad ~ internet + age + health + education + cadres + party + nonagri +
                lnincome + people + + area, data, family = binomial())
summary(trad_3)
trad_4 <- glm(trad ~ internet + age + health + education + cadres + party + nonagri +
                lnincome + people + area + traffic + south, data, family = binomial())
summary(trad_4)
p <- predict.glm(trad_4, type = "response")
log_odds <- log(p / (1 - p))
plot_data <- data.frame(
  internet = data$internet,
  log_odds = log_odds
)
ggplot(plot_data, aes(internet, log_odds)) +
  geom_point(alpha = 0.3, size = 5, color = "#d38d0b") +
  geom_smooth(color = "#1172c2e3", fill = "#54abf3") +
  theme_minimal()
## 商品能源
modern_1 <- glm(modern ~ internet, data, family = binomial())
summary(modern_1)
modern_2 <- glm(modern ~ internet + age + health + education + cadres + party + nonagri, data, family = binomial())
summary(modern_2)
modern_3 <- glm(modern ~ internet + age + health + education + cadres + party + nonagri +
                  lnincome + people + + area, data, family = binomial())
summary(modern_3)
modern_4 <- glm(modern ~ internet + age + health + education + cadres + party + nonagri +
                  lnincome + people + area + traffic + south, data, family = binomial())
summary(modern_4)
p <- predict.glm(modern_4, type = "response")
log_odds <- log(p / (1 - p))
plot_data <- data.frame(
  internet = data$internet,
  log_odds = log_odds
)
ggplot(plot_data, aes(internet, log_odds)) +
  geom_point(alpha = 0.3, size = 5, color = "#d38d0b") +
  geom_smooth(color = "#1172c2e3", fill = "#54abf3") +
  theme_minimal()

# 内生性问题解决——PSM


# 稳健性检验——替换核心解释变量
bio_sub <- glm(bio ~ phone + age + health + education + cadres + party + nonagri +
                 lnincome + people + area + traffic + south, data, family = binomial())
summary(bio_sub)
trad_sub <- glm(trad ~ phone + age + health + education + cadres + party + nonagri +
                  lnincome + people + area + traffic + south, data, family = binomial())
summary(trad_sub)
modern_sub <- glm(modern ~ phone + age + health + education + cadres + party + nonagri +
                    lnincome + people + area + traffic + south, data, family = binomial())
summary(modern_sub)

# 稳健性检验——替换回归模型
## 生物质能
bio_5 <- glm(bio ~ internet + age + health + education + cadres + party + nonagri +
               lnincome + people + area + traffic + south, data, family = binomial(link = "probit"))
summary(bio_5)
## 低劣能源
trad_5 <- glm(trad ~ internet + age + health + education + cadres + party + nonagri +
                lnincome + people + area + traffic + south, data, family = binomial(link = "probit"))
summary(trad_5)
## 商品能源
modern_5 <- glm(modern ~ internet + age + health + education + cadres + party + nonagri +
                  lnincome + people + area + traffic + south, data, family = binomial(link = "probit"))
summary(modern_5)

# 异质性探究——分地域
## 数据分类
table(data$south)
data_south <- subset(data, south == 1)
data_north <- subset(data, south == 0)
## logistic回归模型
bio_south <- glm(bio ~ internet + age + health + education + cadres + party + nonagri +
                   lnincome + people + area + traffic, data_south, family = binomial())
summary(bio_south)
bio_north <- glm(bio ~ internet + age + health + education + cadres + party + nonagri +
                   lnincome + people + area + traffic, data_north, family = binomial())
summary(bio_north)
trad_south <- glm(trad ~ internet + age + health + education + cadres + party + nonagri +
                    lnincome + people + area + traffic, data_south, family = binomial())
summary(trad_south)
trad_north <- glm(trad ~ internet + age + health + education + cadres + party + nonagri +
                    lnincome + people + area + traffic, data_north, family = binomial())
summary(trad_north)
modern_south <- glm(modern ~ internet + age + health + education + cadres + party + nonagri +
                      lnincome + people + area + traffic, data_south, family = binomial())
summary(modern_south)
modern_north <- glm(modern ~ internet + age + health + education + cadres + party + nonagri +
                      lnincome + people + area + traffic, data_north, family = binomial())
summary(modern_north)

# 异质性检验——分教育水平
## 数据分类
table(data$education)
data_high <-  subset(data, education >= 9)
data_low <- subset(data, education <= 8)
## logistic回归模型
bio_high <- glm(bio ~ internet + age + health + education + cadres + party + nonagri +
                  lnincome + people + area + traffic, data_high, family = binomial())
summary(bio_high)
bio_low <- glm(bio ~ internet + age + health + education + cadres + party + nonagri +
                 lnincome + people + area + traffic, data_low, family = binomial())
summary(bio_low)
trad_high <- glm(trad ~ internet + age + health + education + cadres + party + nonagri +
                   lnincome + people + area + traffic, data_high, family = binomial())
summary(trad_high)
trad_low <- glm(trad ~ internet + age + health + education + cadres + party + nonagri +
                  lnincome + people + area + traffic, data_low, family = binomial())
summary(trad_low)
modern_high <- glm(modern ~ internet + age + health + education + cadres + party + nonagri +
                     lnincome + people + area + traffic, data_high, family = binomial())
summary(modern_high)
modern_low <- glm(modern ~ internet + age + health + education + cadres + party + nonagri +
                    lnincome + people + area + traffic, data_low, family = binomial())
summary(modern_low)

# CMP估计
## 第一阶段——计算拟合值
know <- lm(know ~ internet + age + health + education + cadres + party + nonagri +
             lnincome + people + area + traffic + south, data)
summary(know)
pred_know <- predict(know)
## 第二阶段——中介效应检验
bio_me <- glm(bio ~ pred_know + age + health + education + cadres + party + nonagri +
                lnincome + people + area + traffic + south, family = binomial(), data)
summary(bio_me)
trad_me <- glm(trad ~ pred_know + age + health + education + cadres + party + nonagri +
                 lnincome + people + area + traffic + south, family = binomial(), data)
summary(trad_me)
modern_me <- glm(modern ~ pred_know + age + health + education + cadres + party + nonagri +
                   lnincome + people + area + traffic + south, family = binomial(), data)
summary(modern_me)

# 输出结果
## 描述性统计
stargazer(data, title = "results", align = T, type = "text", no.space = TRUE, out = "des.html")
## 基准回归
stargazer(bio_1, bio_2, bio_3, bio_4, title = "results", align = F, type = "text", no.space = TRUE, out = "bio_logit.html")
stargazer(trad_1, trad_2, trad_3, trad_4, title = "results", align = F, type = "text", no.space = TRUE, out = "trad_logit.html")
stargazer(modern_1, modern_2, modern_3, modern_4, title = "results", align = F, type = "text", no.space = TRUE, out = "modern_logit.html")
## 替换核心解释变量
stargazer(bio_sub, trad_sub, modern_sub, title = "results", align = F, type = "text", no.space = TRUE, out = "var_sub.html")
## 替换回归模型
stargazer(bio_5, trad_5, modern_5, title = "results", align = F, type = "text", no.space = TRUE, out = "probit_sub.html")
## 分地域
stargazer(bio_south, bio_north, trad_south, trad_north, modern_south, modern_north, title = "results", align = F, type = "text", no.space = TRUE, out = "south_north.html")
## 分教育水平
stargazer(bio_high, bio_low, trad_high, trad_low, modern_high, modern_low, title = "results", align = F, type = "text", no.space = TRUE, out = "high_low.html")
## 影响机制探究
stargazer(know, bio_me, trad_me, modern_me, title = "results", align = F, type = "text", no.space = TRUE, out = "me.html")

