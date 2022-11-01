

#install.packages("glmnet")
#install.packages("survival")
#install.packages('survminer')


#���ð�
library(glmnet)
library(survival)
library(survminer)
inputFile="uniSigExp.txt"      #��������������ı��������ļ�
setwd("C:\\Users\\78570\\Desktop\\122geneImmune\\26.model")         #���ù���Ŀ¼
rt=read.table(inputFile, header=T, sep="\t", row.names=1, check.names=F)    #��ȡ�����ļ�

#COXģ�͹���
multiCox=coxph(Surv(futime, fustat) ~ ., data = rt)
multiCox=step(multiCox, direction="both")
multiCoxSum=summary(multiCox)

#���ģ�������Ϣ
outMultiTab=data.frame()
outMultiTab=cbind(
		          coef=multiCoxSum$coefficients[,"coef"],
		          HR=multiCoxSum$conf.int[,"exp(coef)"],
		          HR.95L=multiCoxSum$conf.int[,"lower .95"],
		          HR.95H=multiCoxSum$conf.int[,"upper .95"],
		          pvalue=multiCoxSum$coefficients[,"Pr(>|z|)"])
outMultiTab=cbind(id=row.names(outMultiTab),outMultiTab)
write.table(outMultiTab, file="multiCox.txt", sep="\t", row.names=F, quote=F)

#��������ļ�
score=predict(multiCox, type="risk", newdata=rt)
coxGene=rownames(multiCoxSum$coefficients)
coxGene=gsub("`", "", coxGene)
outCol=c("futime", "fustat", coxGene)
risk=as.vector(ifelse(score>median(score), "high", "low"))
outTab=cbind(rt[,outCol], riskScore=as.vector(score), risk)
write.table(cbind(id=rownames(outTab),outTab), file="risk.txt", sep="\t", quote=F, row.names=F)

#����ɭ��ͼ
pdf(file="multi.forest.pdf", width=10, height=6, onefile=FALSE)
ggforest(multiCox,
		 data=rt,
         main = "Hazard ratio",
         cpositions = c(0.02,0.22, 0.4), 
         fontsize = 0.7, 
         refLabel = "reference", 
         noDigits = 2)
dev.off()

