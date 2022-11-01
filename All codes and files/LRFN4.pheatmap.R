

#install.packages("pheatmap")
#install.packages("vioplot")


#���ð�
library(vioplot)
library(pheatmap)
input="CIBERSORT-Results.txt"      #����ϸ�������ļ�
pFilter=0.05                       #����ϸ����������������
setwd("C:\\Users\\78570\\Desktop\\122geneImmune\\11.heatmap")      #���ù���Ŀ¼

#��ȡ���߽���ļ����������ݽ�������
immune=read.table("CIBERSORT-Results.txt", header=T, sep="\t", check.names=F, row.names=1)
immune=immune[immune[,"P-value"]<pFilter,,drop=F]
immune=as.matrix(immune[,1:(ncol(immune)-3)])
data=t(immune)

#������������Ŀ
group=sapply(strsplit(colnames(data),"\\-"), "[", 4)
group=sapply(strsplit(group,""), "[", 1)
group=gsub("2", "1", group)
conNum=length(group[group==1])       #��������Ʒ��Ŀ
treatNum=length(group[group==0])     #��������Ʒ��Ŀ

#������ͼ��ע���ļ�
Type=c(rep("Normal",conNum), rep("Tumor",treatNum))
names(Type)=colnames(data)
Type=as.data.frame(Type)

#������ͼ
pdf(file="heatmap.pdf", width=12, height=6)
pheatmap(data, 
         annotation=Type, 
         color = colorRampPalette(c(rep("deepskyblue",1), rep("white",1), rep("mediumvioletred",3)))(100),
         cluster_cols =F,
         show_colnames=F,
         fontsize = 8,
         fontsize_row=7,
         fontsize_col=5)
dev.off()


#����С����ͼ
data=t(data)
outTab=data.frame()
pdf(file="vioplot.pdf", width=13, height=8)
par(las=1, mar=c(10,6,3,3))
x=c(1:ncol(data))
y=c(1:ncol(data))
xMax=ncol(data)*3-2
plot(x,y,
     xlim=c(0,xMax),ylim=c(min(data),max(data)+0.02),
     main="", xlab="", ylab="Fraction",
     pch=21,
     col="white",
     xaxt="n")

#��ÿ������ϸ��ѭ��������С����ͼ��������Ʒ����ɫ��ʾ��������Ʒ�ú�ɫ��ʾ
for(i in 1:ncol(data)){
	if(sd(data[1:conNum,i])==0){
	  	data[1,i]=0.00001
	}
	if(sd(data[(conNum+1):(conNum+treatNum),i])==0){
	    data[(conNum+1),i]=0.00001
	}
	conData=data[1:conNum,i]
	treatData=data[(conNum+1):(conNum+treatNum),i]
	vioplot(conData,at=3*(i-1),lty=1,add = T,col = 'deepskyblue')
	vioplot(treatData,at=3*(i-1)+1,lty=1,add = T,col = 'mediumvioletred')
	wilcoxTest=wilcox.test(conData, treatData)
	p=wilcoxTest$p.value
	if(p<pFilter){
	    cellPvalue=cbind(Cell=colnames(data)[i], pvalue=p)
		outTab=rbind(outTab, cellPvalue)
	}
	mx=max(c(conData,treatData))
	lines(c(x=3*(i-1)+0.2,x=3*(i-1)+0.8),c(mx,mx))
	text(x=3*(i-1)+0.5, y=mx+0.02, labels=ifelse(p<0.001, paste0("p<0.001"), paste0("p=",sprintf("%.03f",p))), cex = 0.8)
}
legend("topright", 
       c("Normal", "Tumor"),
       lwd=5,bty="n",cex=1.2,
       col=c("deepskyblue","mediumvioletred"))
text(seq(1,xMax,3),-0.05,xpd = NA,labels=colnames(data),cex = 1,srt = 45,pos=2)
dev.off()

#�������ϸ����pֵ�����ļ�
write.table(outTab,file="diff.result.txt",sep="\t",row.names=F,quote=F)

