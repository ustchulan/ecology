
rm(list = ls())
# 加载库
library(tidyverse)
library(ade4)
library(car)
library('caret')

data(doubs)
#answer 1
# 按场地(summarize fish abundance data by sites)汇总鱼类丰度数据
total_fish = rowSums(doubs$fish) 
# 合并env和fish数据
env_fish = cbind(doubs$env, total_fish) 

#answer 2
library("ggplot2")
env_fish %>% gather(-fish_total, key = "value", value = "env") %>%
  ggplot(aes(x = env, y = fish_total)) +
  gemo_point() +
  gemo_smooth(method = "loess", se = FALSE) +
  facet_wrap(-value, scales = "free") +
  theme_bw()

#annswer 3
# 找出没有鱼的站点
no_fish_sites <- env_fish$site[env_fish$total_fish == 0]
# 删除没有鱼的站点
if (length(no_fish_sites) > 0) {
  env_fish <- env_fish[!env_fish$site %in% no_fish_sites, ]
}
# 删除包含空值或异常值的行
env_fish <- env_fish[complete.cases(env_fish), ]
env_fish <- env_fish[apply(env_fish, 1, function(x) all(abs(scale(x)) < 3)), ]

#answer 4

library(caret)
# 排除零方差变量和异常值
# 找出零方差变量
zero_var <- nearZeroVar(env_fish[,-1])
# 找出异常值
outliers <- apply(env_fish[,-1], 1, function(x) any(abs(scale(x)) > 3))
# 将零方差变量和异常值从数据集中删除
env_fish <- env_fish[, !colnames(env_fish) %in% zero_var]
env_fish <- env_fish[!outliers, ]

#answer 5
# 删除高度相关的特征
env_fishCor <-  cor(env_fish[ ,-12])
highlyCor<- findCorrelation(env_fishCor, cutoff = .75)
env_fish <- env_fish[,-highlyCor] 
comboInfo <- findLinearCombos(env_fish)
comboInfo
env_fish[, -comboInfo$remove]
dim(env_fish)

#第二节
#answer 6
# 分割数据为训练集和测试集
set.seed(123) # 设置随机种子，以便结果可重复
trainIndex <- createDataPartition(env_fish$total_fish, p = 0.8, list = FALSE)
train <- env_fish[trainIndex, ]
test <- env_fish[-trainIndex, ]

# 可视化训练集的特征和目标
x<-as.matrix(env_fish[ ,1:6])
y<-as.factor(env_fish$fish_abundance)
featurePlot(x,y,plot="density",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales=list(x=list(relation="free"),
                        y=list(relation="free")))

#answer 7
set.seed(777)
names(getModelInfo())
model1<-rpart(formula = fish_abundance ~.,
              data = training,
              control=rpart.control(minsplit=2),
              method = "anova")
library(rpart.plot)
rpart.plot(model1)

# evaluating the model
model1.pred <- predict(model1, test)
library(Metrics)
rmse(actual=test$fish_abundance,
     predicted=model1.pred)

# Build the model with random forests
fitControl<-trainControl(
  method = "repeatedcv",
  number = 30,
  repeats = 30)

set.seed(888)
model2<-train(fish_abundance~.,data=training,
              method="rf",
              trControl=fitControl,
              metric="RMSE",
              verbose=T)
model2

