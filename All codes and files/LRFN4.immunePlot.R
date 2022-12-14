

#install.packages("corrplot")


library(corrplot)                   #引用包
immFile="CIBERSORT-Results.txt"     #免疫细胞浸润的结果文件
pFilter=0.05                        #免疫细胞浸润结果过滤条件
setwd("C:\\Users\\78570\\Desktop\\122geneImmune\\10.immunePlot")    #设置工作目录

#读取免疫细胞浸润的结果文件，并对数据进行整理
immune=read.table(immFile, header=T, sep="\t", check.names=F, row.names=1)
immune=immune[immune[,"P-value"]<pFilter,]
immune=as.matrix(immune[,1:(ncol(immune)-3)])
data=t(immune)

#绘制柱状图
col=rainbow(nrow(data), s=0.7, v=0.7)
pdf(file="barplot.pdf", width=22, height=10)
par(las=1,mar=c(8,5,4,16),mgp=c(3,0.1,0),cex.axis=1.5)
a1=barplot(data,col=col,yaxt="n",ylab="Relative Percent",xaxt="n",cex.lab=1.8)
a2=axis(2,tick=F,labels=F)
axis(2,a2,paste0(a2*100,"%"))
axis(1,a1,labels=F)
par(srt=60,xpd=T);text(a1,-0.02,colnames(data),adj=1,cex=0.5);par(srt=0)
ytick2=cumsum(data[,ncol(data)]);ytick1=c(0,ytick2[-length(ytick2)])
legend(par('usr')[2]*0.98,par('usr')[4],legend=rownames(data),col=col,pch=15,bty="n",cex=1.3)
dev.off()

#删除正常样品
group=sapply(strsplit(colnames(data),"\\-"), "[", 4)
group=sapply(strsplit(group,""), "[", 1)
group=gsub("2", "1", group)
data=data[,group==0,drop=F]

#绘制免疫细胞相关性的图形
pdf(file="corrplot.pdf", width=13, height=13)
par(oma=c(0.5,1,1,1.2))
M=cor(immune)
corrplot(M,
         method = "color",
         order = "hclust",
         tl.col="black",
         addCoef.col = "black",
         number.cex = 0.8,
         col=colorRampPalette(c("deepskyblue", "white", "mediumvioletred"))(50)
         )
dev.off()



