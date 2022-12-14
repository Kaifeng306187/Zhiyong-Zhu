
#if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#BiocManager::install("limma")

#install.packages("ggplot2")
#install.packages("ggpubr")
#install.packages("ggExtra")


#引用包
library(limma)
library(ggplot2)
library(ggpubr)
library(ggExtra)

gene="AKAP12"         #基因名称
pFilter=0.05        #pvalue过滤条件
immuneFile="CIBERSORT-Results.txt"   #免疫浸润结果文件
expFile="symbol.txt"                 #表达输入文件
setwd("C:\\Users\\78570\\Desktop\\122geneImmune\\12.immuneCor")     #设置工作目录

#读取免疫细胞浸润结果文件，并对数据进行整理
immune=read.table(immuneFile, header=T, sep="\t", check.names=F, row.names=1)
immune=immune[immune[,"P-value"]<pFilter,]
immune=as.matrix(immune[,1:(ncol(immune)-3)])

#读取表达文件，并对输入文件整理
rt=read.table(expFile, header=T, sep="\t", check.names=F)
rt=as.matrix(rt)
rownames(rt)=rt[,1]
exp=rt[,2:ncol(rt)]
dimnames=list(rownames(exp),colnames(exp))
data=matrix(as.numeric(as.matrix(exp)), nrow=nrow(exp), dimnames=dimnames)
data=avereps(data)
data=data[gene,,drop=F]

#删掉正常样品
group=sapply(strsplit(colnames(data),"\\-"), "[", 4)
group=sapply(strsplit(group,""), "[", 1)
group=gsub("2", "1", group)
data=data[,group==0,drop=F]
exp=t(data)

#样品取交集
sameSample=intersect(row.names(immune), row.names(exp))
immune1=immune[sameSample, ,drop=F]
exp1=exp[sameSample, ,drop=F]

#相关性检验
outTab=data.frame()
x=log2(as.numeric(exp1[,1])+1)
#按免疫细胞循环
for(j in colnames(immune1)[1:22]){
	y=as.numeric(immune1[,j])
	if(sd(y)>0.001){
		df1=as.data.frame(cbind(x,y))
		corT=cor.test(x, y, method="spearman")
		cor=corT$estimate
		pValue=corT$p.value
		p1=ggplot(df1, aes(x, y)) + 
			xlab(gene) + ylab(j) + 
			geom_point()+ geom_smooth(method="lm",formula=y~x) + theme_bw()+
			stat_cor(method = 'spearman', aes(x =x, y =y))
		if(pValue<pFilter){
			pdf(file=paste0(j, ".pdf"), width=5, height=4.75)
			print(p1)
			dev.off()
			outTab=rbind(outTab, cbind(Cell=j, pValue))
		}
	}
}
write.table(outTab, file="immuneCor.result.txt", sep="\t", row.names=F, quote=F)



