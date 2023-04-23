
rm(list = ls())
# 加载库
library(tidyverse)
library(ade4)
library(car)
library('caret')

#answer 1
# 按场地(summarize fish abundance data by sites)汇总鱼类丰度数据
# 合并env和fish数据
data(doubs)
env <- rownames_to_column(doubs$env, var = "site")
total_fish <- rowSums(doubs$fish)                     
env_fish <- cbind(env,total_fish)  

#answer 2
library(ggplot2)
#visualize the features of the new env_fish set
env_fish %>% 
  gather(-total_fish, key = "value", value = "env") %>%
  ggplot(aes(x = env, y = total_fish)) +
  geom_point()+
  geom_smooth(se = FALSE) +
  facet_wrap(~value, scales = "free") +
  theme_bw()                                          

#annswer 3

# 删除没有鱼的站点
no_fish_sites <- env_fish$site[env_fish$total_fish == 0]
if (length(no_fish_sites) > 0) {
  env_fish <- env_fish[!env_fish$site %in% no_fish_sites, ]
}

# 删除包含空值或异常值的行
# 检测离群值的阈值
threshold <- 3

# 检测并删除包含离群值的任何列的所有行
for (col in colnames(env_fish)) {
  if (is.numeric(env_fish[[col]])) {  # 只对数值类型的列进行处理
    z_scores <- abs((env_fish[[col]] - mean(env_fish[[col]])) / sd(env_fish[[col]]))  # 计算z-score
    env_fish <- env_fish[(z_scores < threshold), ]  # 保留z-score小于阈值的行
  }
}

# 显示删除离群值后的数据框
print(env_fish)

#answer 4
# 排除零方差变量和异常值
# 找出零方差变量
library(caret)
zero_var <- nearZeroVar(env_fish[,-1])
# 找出异常值
outliers <- apply(env_fish[,-1], 1, function(x) any(abs(scale(x)) > 3))
# 将零方差变量和异常值从数据集中删除
env_fish <- env_fish[, !colnames(env_fish) %in% zero_var]
env_fish <- env_fish[!outliers, ]

#answer 5
# 删除高度相关的特征
cor_matrix <- cor(env_fish[, -1])  
highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.75)  
env_fish <- env_fish[, -highly_correlated] 
print(env_fish)

#第二节
#answer 6
# 分割数据为训练集和测试集
set.seed(1020)
assignment<-sample(1:2,
                   size=nrow(env_fish),
                   prob = c(0.75,0.25),
                   replace = TRUE)

training<-env_fish[assignment==1,]
test<-env_fish[assignment==2,]

#训练树，使用网格搜索建立模型
env_tfs_model<-rpart(formula=tfs~.,
                     data=training,
                     control = rpart.control(minsplit = 2),
                     method="anova")
print(env_tfs_model)
# 可视化提升树
rpart.plot(env_tfs_model)
#预测bt模型在测试集中的表现
pred<-predict(object=env_tfs_model,
              newdata = test)
pred
#评估bt模型优劣
rmse(actual=test$tfs,
     predicted=pred)

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

