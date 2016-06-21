#Supplemental Material 6：解析に用いたRコード
### データの読み込み ###
CI_data = read.csv("data.csv", header = T)

# 割り当て変数zの定義
ivec1 = CI_data$cm_dummy #処置群（CMを見た群）を示すベクトル
ivec0 = rep(1, nrow(CI_data))-ivec1　#対照群（CMを見ていない群）を示すベクトル

## 傾向スコア(logi$fitted)の推定 ##
logi=glm(cm_dummy ~ TVwatch_day + age + sex + marry_dummy + child_dummy + inc + pmoney + area_kanto +area_tokai + area_keihanshin + job_dummy1 + job_dummy2 + job_dummy3 + job_dummy4 + job_dummy5 + job_dummy6 + job_dummy7  + fam_str_dummy1 + fam_str_dummy2 + fam_str_dummy3 + fam_str_dummy4, family=binomial(link="logit") , data = CI_data)
CI_data2 = data.frame(CI_data, logi$fitted) # 傾向スコアデータの結合

CI_data_treated = subset(CI_data2, CI_data$cm_dummy==1)　# 処置群のみのデータ
CI_data_untreated = subset(CI_data2, CI_data$cm_dummy==0)　# 対照群のみのデータ

###########################################
### IPW調整前の各変数の平均/標準偏差(表2) ###
###########################################
# 処置群／gamedummy
aveT_gd = mean(CI_data_treated$gamedummy) 
sdT_gd = sd(CI_data_treated$gamedummy)
# 対照群／gamedummy
aveU_gd = mean(CI_data_untreated$gamedummy)
sdU_gd = sd(CI_data_untreated$gamedummy)
# 差
diff_gd = aveT_gd - aveU_gd
# 処置群／gamecount
aveT_gc = mean(CI_data_treated$gamecount)
sdT_gc = sd(CI_data_treated$gamecount)
# 対照群／gamecount
aveU_gc = mean(CI_data_untreated$gamecount)
sdU_gc = sd(CI_data_untreated$gamecount)
# 差
diff_gc = aveT_gc - aveU_gc
# 処置群／gamesecond
aveT_gs = mean(CI_data_treated$gamesecond)
sdT_gs = sd(CI_data_treated$gamesecond)
# 対照群／gamesecond
aveU_gs = mean(CI_data_untreated$gamesecond)
sdU_gs = sd(CI_data_untreated$gamesecond)
# 差
diff_gs = aveT_gs - aveU_gs

#######################
### 平均処置効果(表4) ###
#######################
ivec = cbind(ivec1,ivec0)
iestp1 = (ivec1/logi$fitted)
iestp0 = (ivec0/(1-logi$fitted))
iestp = iestp1+iestp0

ipwe_gd = lm(CI_data$gamedummy ~ ivec+0, weights=iestp)
summary(ipwe_gd)
ipwe_gc = lm(CI_data$gamecount ~ ivec+0, weights=iestp)
summary(ipwe_gc)
ipwe_gs = lm(CI_data$gamesecond ~ ivec+0, weights=iestp)
summary(ipwe_gs)

###################################
## 処置群における平均処置効果(表5)###
###################################
iestp1_ATT = ivec1
iestp0_ATT = ivec0*logi$fitted/(1-logi$fitted)
iestp_ATT = iestp1_ATT+iestp0_ATT
ipwe_ATT_gd = lm(CI_data$gamedummy ~ ivec+0, weights=iestp_ATT)
summary(ipwe_ATT_gd)
ipwe_ATT_gc = lm(CI_data$gamecount ~ ivec+0, weights=iestp_ATT)
summary(ipwe_ATT_gc)
ipwe_ATT_gs = lm(CI_data$gamesecond ~ ivec+0, weights=iestp_ATT)
summary(ipwe_ATT_gs)

############################################
## ゲーム利用秒数に関する調整効果の確認(表6) ##
############################################
# 線形回帰の場合
ME_treated_gamesecond = lm(gamesecond ~ child_dummy + area_kanto +area_tokai + area_keihanshin + T + F1 + F2 + F3 + M1 + M2 ,weights=(1/logi.fitted) ,data=CI_data_treated)
ME_untreated_gamesecond = lm(gamesecond ~ child_dummy + area_kanto +area_tokai + area_keihanshin + T + F1 + F2 + F3 + M1 + M2,weights=(1/(1-logi.fitted)), data=CI_data_untreated)
summary(ME_treated_gamesecond)
summary(ME_untreated_gamesecond)

# タイプⅠトービットの場合
install.packages("VGAM")
library(VGAM)
ME_treated_gamesecond_t <- vglm(gamesecond ~ child_dummy + area_kanto +area_tokai + area_keihanshin + T + F1 + F2 + F3 + M1 + M2, tobit(Lower=0), weights=(1/logi.fitted), data=CI_data_treated)
ME_untreated_gamesecond_t <- vglm(gamesecond ~ child_dummy + area_kanto +area_tokai + area_keihanshin + T + F1 + F2 + F3 + M1 + M2, tobit(Lower=0), weights=(1/(1-logi.fitted)), data=CI_data_untreated)
summary(ME_treated_gamesecond_t)
summary(ME_untreated_gamesecond_t)

#############################################
## 「子供ありの層」を除外してATTを推定(表7) ##
#############################################
CI_data_ltd = subset(CI_data, CI_data$cm_dummy==0 | CI_data$child_dummy==0) # CMを見た「子供有り」サンプルを除いたデータセット
ivec1_ltd = CI_data_ltd$cm_dummy #処置群（CMを見た群）を示すベクトル
ivec0_ltd = rep(1, nrow(CI_data_ltd))-ivec1_ltd　#対照群（CMを見ていない群）を示すベクトル
logi2=glm(cm_dummy ~ TVwatch_day + age + sex + marry_dummy + child_dummy + inc + pmoney  + area_kanto +area_tokai + area_keihanshin + job_dummy1 + job_dummy2 + job_dummy3 + job_dummy4 + job_dummy5 + job_dummy6 + job_dummy7  + fam_str_dummy1 + fam_str_dummy2 + fam_str_dummy3 + fam_str_dummy4, family=binomial(link="logit") , data = CI_data_ltd)

ivec_ltd = cbind(ivec1_ltd,ivec0_ltd)
iestp1_ltd = ivec1_ltd
iestp0_ltd = ivec0_ltd*logi2$fitted/(1-logi2$fitted)
iestp_ltd = iestp1_ltd+iestp0_ltd
ipwe_ltd_gd = lm(CI_data_ltd$gamedummy ~ ivec_ltd+0, weights=iestp_ltd)
summary(ipwe_ltd_gd)
ipwe_ltd_gc = lm(CI_data_ltd$gamecount ~ ivec_ltd+0, weights=iestp_ltd)
summary(ipwe_ltd_gc)
ipwe_ltd_gs = lm(CI_data_ltd$gamesecond ~ ivec_ltd+0, weights=iestp_ltd)
summary(ipwe_ltd_gs)

####################################
## 対照群における平均処置効果(付表2) ##
####################################
iestp1_ATU = ivec1*(1-logi$fitted)/logi$fitted
iestp0_ATU = ivec0
iestp_ATU = iestp1_ATU+iestp0_ATU
ipwe_ATU_gd = lm(CI_data$gamedummy ~ ivec+0, weights=iestp_ATU)
summary(ipwe_ATU_gd)
ipwe_ATU_gc = lm(CI_data$gamecount ~ ivec+0, weights=iestp_ATU)
summary(ipwe_ATU_gc)
ipwe_ATU_gs = lm(CI_data$gamesecond ~ ivec+0, weights=iestp_ATU)
summary(ipwe_ATU_gs)

################################################
## M2層のサンプルのみを用いてTEUを推定(付表3) ##
################################################
CI_data_M2 = subset(CI_data2, CI_data2$M2==1) # M2層のみのデータ
ivec1_M2 = CI_data_M2$cm_dummy #処置群（CMを見た群）を示すベクトル
ivec0_M2 = rep(1, nrow(CI_data_M2))-ivec1_M2　#対照群（CMを見ていない群）を示すベクトル

ivec_M2 = cbind(ivec1_M2,ivec0_M2)
iestp1_M2 = ivec1_M2*(1-CI_data_M2$logi.fitted)/CI_data_M2$logi.fitted
iestp0_M2 = ivec0_M2
iestp_M2 = iestp1_M2+iestp0_M2
ipwe_M2_gd = lm(CI_data_M2$gamedummy ~ ivec_M2+0, weights=iestp_M2)
summary(ipwe_M2_gd)
ipwe_M2_gc = lm(CI_data_M2$gamecount ~ ivec_M2+0, weights=iestp_M2)
summary(ipwe_M2_gc)
ipwe_M2_gs = lm(CI_data_M2$gamesecond ~ ivec_M2+0, weights=iestp_M2)
summary(ipwe_M2_gs)

)####################################
## TVwatch_dayのヒストグラム(図1) ##
####################################
par(oma = c(0, 0, 0, 0))
par(mfrow=c(1,1)) 
hist(CI_data_untreated$TVwatch_day, col = "#c8c8cb", border = "#c8c8cb",main="", xlab = "", breaks = 50)
hist(CI_data_treated$TVwatch_day, col = "#00000090", border = "#00000090", breaks = 50, add = TRUE)
legend("topright", legend=c(legend="処置群（CM接触群）", "対照群（CM非接触群）"), col=c("#00000090", "#c8c8cb"), cex = 1.3, pch=15)

####################################
## IPWによる補正効果の棒グラフ(図2) ##
####################################
par(oma = c(0, 0, 0, 0))
par(mfrow=c(1,3)) 
gamedummy_bar <- cbind(アプリ利用ダミーの平均処置効果 = c(0.002, 0.032))
labels <- c("IPW調整前", "IPW調整後")
barplot1 = barplot(gamedummy_bar, legend.text = labels, args.legend = list(x = 2.95, y = 0.05), beside = TRUE,
ylim=c(-0.01,0.05), space=1)
arrows(barplot1[1,], 0.002 - 1.96*0.0053,barplot1[1,], 0.002 + 1.96*0.0053, code=3, lwd=1,angle = 90, length = 0.1)
arrows(barplot1[2,], 0.032 + 1.96*0.0053,barplot1[2,], 0.032 - 1.96*0.0053, code=3, lwd=1,angle = 90, length = 0.1)

gamecount_bar <- cbind(アプリ利用回数の平均処置効果 = c(-1.485, 5.320))
labels <- c("IPW調整前", "IPW調整後")
barplot2 = barplot(gamecount_bar, legend.text = labels, args.legend = list(x = 2.95, y = 8), beside = TRUE, ylim=c(-4,8), space=1)
arrows(barplot2[1,], -1.485 - 1.96*1.099,barplot2[1,], -1.485 + 1.96*1.099, code=3, lwd=1, angle = 90, length = 0.1)
arrows(barplot2[2,], 5.320 + 1.96*1.181,barplot2[2,], 5.320 - 1.96*1.181, code=3, lwd=1, angle = 90, length = 0.1)

gamesecond_bar <- cbind(アプリ利用秒数の平均処置効果 = c(-629.6, 1503.9))
labels <- c("IPW調整前", "IPW調整後")
barplot3 = barplot(gamesecond_bar, legend.text = labels, args.legend = list(x = 2.95, y = 3000), beside = TRUE,
                 ylim=c(-2000,3000), space=1)
arrows(barplot3[1,], -629.6 - 1.96*351.24,barplot3[1,], -629.6 + 1.96*351.24, code=3, lwd=1, angle = 90, length = 0.1)
arrows(barplot3[2,], 1503.9 + 1.96*381.8,barplot3[2,], 1503.9 - 1.96*381.8, code=3, lwd=1, angle = 90, length = 0.1) 

############################
## 参考：居住地域の棒グラフ ##
############################
par(oma = c(0, 0, 0, 0))
par(mfrow=c(1,1)) 
area_bar <- cbind(関東 = c(0.131, 0.063), 京浜 = c(0.701, 0.509), 東海 = c(0.093, 0.124), 京阪神 = c(0.075, 0.303))
labels <- c("処置群（CM接触群）", "対照群（CM非接触群）")
    barplot(area_bar, legend.text = labels, args.legend = list(x = 12, y = 0.7), beside = TRUE)
