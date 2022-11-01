
#if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#BiocManager::install("limma")

#install.packages("survival")
#install.packages("survminer")


#���ð�
library(limma)
library(survival)
library(survminer)
expFile="symbol.txt"     #���������ļ�
cliFile="time.txt"       #�ٴ������ļ�
gene="LRFN4"              #��������
setwd("C:\\Users\\78570\\Desktop\\122geneImmune\\08.survival")     #���ù���Ŀ¼

#��ȡ�����ļ������������ļ�����
rt=read.table(expFile, header=T, sep="\t", check.names=F)
rt=as.matrix(rt)
rownames(rt)=rt[,1]
exp=rt[,2:ncol(rt)]
dimnames=list(rownames(exp), colnames(exp))
data=matrix(as.numeric(as.matrix(exp)), nrow=nrow(exp), dimnames=dimnames)
data=avereps(data)
data=data[gene,,drop=F]

#ɾ��������Ʒ
group=sapply(strsplit(colnames(data),"\\-"), "[", 4)
group=sapply(strsplit(group,""), "[", 1)
group=gsub("2", "1", group)
data=data[,group==0,drop=F]
colnames(data)=gsub("(.*?)\\-(.*?)\\-(.*?)\\-(.*?)\\-.*", "\\1\\-\\2\\-\\3", colnames(data))
data=t(data)
data=avereps(data)

#��ȡ��������
cli=read.table(cliFile, header=T, sep="\t", check.names=F, row.names=1)
cli$futime=cli$futime/365

#���ݺϲ���������
sameSample=intersect(row.names(data), row.names(cli))
data=data[sameSample,,drop=F]
cli=cli[sameSample,,drop=F]
rt=cbind(cli, data)

#���ݻ���������λֵ����Ʒ����
Type=ifelse(rt[,gene]<=median(rt[,gene]), "Low", "High")

#�Ƚϸߵͱ�����֮���������죬�õ�������pֵ
diff=survdiff(Surv(futime, fustat) ~ Type, data=rt)
pValue=1-pchisq(diff$chisq, df=1)
if(pValue<0.001){
	pValue="p<0.001"
}else{
	pValue=paste0("p=", sprintf("%.03f",pValue))
}
fit <- survfit(Surv(futime, fustat) ~ Type, data = rt)
#print(surv_median(fit))
		
#������������
surPlot=ggsurvplot(fit, 
		           data=rt,
		           conf.int=T,
		           pval=pValue,
		           pval.size=6,
		           surv.median.line = "hv",
		           legend.title=gene,
		           legend.labs=c("High level", "Low level"),
		           xlab="Time(years)",
		           break.time.by = 1,
		           palette=c("mediumvioletred", "deepskyblue"),
		           risk.table=TRUE,
		       	   risk.table.title="",
		           risk.table.col = "strata",
		           risk.table.height=.25)

#�����������
pdf(file=paste0(gene, ".surv.pdf"), onefile=FALSE, width=6.5, height=5.5)
print(surPlot)
dev.off()

