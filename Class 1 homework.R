#安装"ade4"程序包#
install.packages("ade4") 

#载入‘ade4’程序包#
library(ade4)

#读取"ade4"程序包数"doubs"数据集#
dobus<- data("doubs",package = "ade4")

#查看数据对象"doubs"的类型#
class(doubs)

#查看数据对象"doubs"中变量以及变量的类型#
str(doubs)

#教师运行的代码
rm(list =ls())# clean the environment space
data(doubs, package = "ade4") # read the doubs dataset form ade4 package

class(doubs) # identify the date class

doubs # view the date profile

spe <- doubs$fish # extract date elements

environment <- doubus$env
spa <- doubs$xy

head(species) # view the species date
names(species)
nrow(species)
ncol(species)
dim(species)

# 数据可视化
plot(spa,asp = 1, type = "n", xlab= "x(km)",ylab = "y(km)")
lines(spa, col = "light blue")
text(spa, row.names(spa),cex = 0.5,col = "red")
text(70, 10, "upstream",cex = 0.8, col = "red")
text(20, 120, "downstream",cex = 0.8, col = "red")

range(spe) # species abundance range
(ab <- table(unlist(spe)))
barplot(ab, las = 1, xlab = "Abundance degree", ylab = "Frequency", col = gray(5:0/5))

par(mfrow = c(2, 2))#split several window to view  把图板块分成两行两列
plot(spa, asp = 1, col = "brown", cex = spe$Satr, xlab = "x(km)", ylab = "y(km)")
lines(spa,col = "light blue")
text(150,10, "upstream", cex = 0.8, col = "blue")
text(20,120, "downstream", cex= 0.8, col = "blue")

plot(spa, asp = 1, col = "brown", cex = spe$Thth, xlab = "x(km)", ylab = "y(km)")
lines(spa,col = "light blue")
text(150,10, "upstream", cex = 0.8, col = "blue")
text(20,120, "downstream", cex= 0.8, col = "blue")

plot(spa, asp = 1, col = "brown", cex = spe$Baba, xlab = "x(km)", ylab = "y(km)")
lines(spa,col = "light blue")
text(150,10, "upstream", cex = 0.8, col = "blue")
text(20,120, "downstream", cex= 0.8, col = "blue")

plot(spa, asp = 1, col = "brown", cex = spe$Gogo, xlab = "x(km)", ylab = "y(km)")
lines(spa,col = "light blue")
text(150,10, "upstream", cex = 0.8, col = "blue")
text(20,120, "downstream", cex= 0.8, col = "blue")

spe <- species[-8,]
env <- environment[-8,]
spa <- space[-8,]