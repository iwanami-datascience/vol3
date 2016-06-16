#********************************************************
# 層別解析
#********************************************************

#========================================================
# データの生成
#========================================================
# 相関のある2つの乱数 x, z を発生させる関数
rcorr <- function(n=1000, r=1) {
	x <<- rnorm(n) 
	z <- rnorm(n) 
	z <<- r*x + sqrt(1-r^2)*z 
}

# 相関が0.8の乱数を500個発生させた
set.seed(202)
rcorr(500, 0.8)
# 確認
plot(x, z)
cor(x, z)

# x, zからyを作成
e <- rnorm(500)
y <- 1.5*x + 1.1*z + e

#========================================================
# 層別
#========================================================
# zを離散化してデータに追加
z.str <- cut(z, c(-Inf, -1, 0, 1, Inf), labels=c("1","2","3","4"))
data <- data.frame(x, y, z, z.str)

# 層に分けない場合
resultX <- lm(data$y~data$x)
summary(resultX)

#---------------------------------------------------------
＃ 4層に分けて層別解析
df.str <- split(data, z.str)

resultX1 <- lm(df.str$'1'$y~df.str$'1'$x)
summary(resultX1)

resultX2 <- lm(df.str$'2'$y~df.str$'2'$x)
summary(resultX2)

resultX3 <- lm(df.str$'3'$y~df.str$'3'$x)
summary(resultX3)

resultX4 <- lm(df.str$'4'$y~df.str$'4'$x)
summary(resultX4)

# xの回帰係数を統合
numer <- 0
denom <- 0

for (i in 1:4) {
	eval(parse(text=paste("beta <- summary(resultX", i, ")$coefficients[2,1]", sep="")))
	eval(parse(text=paste("se <- summary(resultX", i, ")$coefficients[2,2]", sep="")))
	numer <- numer + beta*(1/se^2)
	denom <- denom + 1/se^2
}

pooled_beta <- numer/denom
pooled_beta

#---------------------------------------------------------
# 図7
plot(data$x, data$y, pch=as.numeric(data$z.str), xlim=c(min(data$x),max(data$x)), ylim=c(min(data$y),max(data$y)), xlab = "x",ylab = "y",cex=1.1)
abline(0,1.5, lwd = 8, col='gray')
abline(resultX, lwd = 4)

# 図8
par(mfrow=c(2,2), mar = c(3, 3, 2, 2), mgp = c(1, 0, 0)) 
plot(df.str$'1'$x, df.str$'1'$y, pch=1, xlim=c(min(data$x),max(data$x)), ylim=c(min(data$y),max(data$y)), xlab = "x",ylab = "y",xaxt="n", yaxt="n")
abline(resultX1)
abline(1.1*mean(df.str$'1'$z),1.5, lwd = 4, col='gray')

plot(df.str$'2'$x, df.str$'2'$y, pch=2, xlim=c(min(data$x),max(data$x)), ylim=c(min(data$y),max(data$y)), xlab = "x",ylab = "y",xaxt="n", yaxt="n")
abline(resultX2)
abline(1.1*mean(df.str$'2'$z),1.5, lwd = 4, col='gray')

plot(df.str$'3'$x, df.str$'3'$y, pch=3, xlim=c(min(data$x),max(data$x)), ylim=c(min(data$y),max(data$y)), xlab = "x",ylab = "y",xaxt="n", yaxt="n")
abline(resultX3)
abline(1.1*mean(df.str$'3'$z),1.5, lwd = 4, col='gray')

plot(df.str$'4'$x, df.str$'4'$y, pch=4, xlim=c(min(data$x),max(data$x)), ylim=c(min(data$y),max(data$y)), xlab = "x",ylab = "y",xaxt="n", yaxt="n")
abline(resultX4)
abline(1.1*mean(df.str$'4'$z),1.5, lwd = 4, col='gray')