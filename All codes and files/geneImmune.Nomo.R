
#install.packages("rms")


library(rms)              #引用包
riskFile="risk.txt"       #风险输入文件
cliFile="clinical.txt"    #临床数据文件
setwd("C:\\Users\\78570\\Desktop\\122geneImmune\\31.Nomo")     #修改工作目录

#读取风险输入文件
risk=read.table(riskFile, header=T, sep="\t", check.names=F, row.names=1)
risk=risk[,c("futime", "fustat", "riskScore")]

#读取临床数据文件
cli=read.table(cliFile, header=T, sep="\t", check.names=F, row.names=1)

#合并数据
samSample=intersect(row.names(risk), row.names(cli))
risk1=risk[samSample,,drop=F]
cli=cli[samSample,,drop=F]
rt=cbind(risk1, cli)
paste(colnames(rt)[3:ncol(rt)],collapse="+")

#数据打包
dd <- datadist(rt)
options(datadist="dd")
#生成函数
f <- cph(Surv(futime, fustat) ~ riskScore+Age+Gender+Grade+Stage+T+M+N, x=T, y=T, surv=T, data=rt, time.inc=1)
surv <- Survival(f)
#建立nomogram
nom <- nomogram(f, fun=list(function(x) surv(1, x), function(x) surv(3, x), function(x) surv(5, x)), 
    lp=F, funlabel=c("1-year survival", "3-year survival", "5-year survival"), 
    maxscale=100, 
    fun.at=c(0.99, 0.9, 0.8, 0.7, 0.5, 0.3,0.1,0.01))  

#nomogram可视化
pdf(file="Nomogram.pdf",height=8.5,width=9.5)
plot(nom)
dev.off()

#calibration curve
time=5    #预测年限
f <- cph(Surv(futime, fustat) ~ riskScore+Age+Gender+Grade+Stage+T+M+N, x=T, y=T, surv=T, data=rt, time.inc=time)
cal <- calibrate(f, cmethod="KM", method="boot", u=time, m=75, B=1000)
pdf(file="calibration.pdf", width=9.5, height=8.5)
plot(cal,
	 xlab=paste0("Nomogram-Predicted Probability of ", time, "-Year OS"),
	 ylab=paste0("Actual ", time, "-Year OS(proportion)"),
	 col="red", sub=T)
dev.off()



