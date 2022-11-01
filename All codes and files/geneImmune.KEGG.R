

#install.packages("colorspace")
#install.packages("stringi")
#install.packages("ggplot2")

#if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#BiocManager::install("org.Hs.eg.db")
#BiocManager::install("DOSE")
#BiocManager::install("clusterProfiler")
#BiocManager::install("enrichplot")
#BiocManager::install("pathview")


#���ð�
library("clusterProfiler")
library("org.Hs.eg.db")
library("enrichplot")
library("ggplot2")
library("pathview")

pvalueFilter=0.05     #pֵ��������
qvalueFilter=1        #�������pֵ��������
setwd("C:\\Users\\78570\\Desktop\\122geneImmune\\17.KEGG")      #���ù���Ŀ¼

#��ȡ�����ļ�
rt=read.table("corResult.txt", header=T, sep="\t", check.names=F)

#��������ת��Ϊ����id
genes=as.vector(rt[,2])
entrezIDs=mget(genes, org.Hs.egSYMBOL2EG, ifnotfound=NA)
entrezIDs=as.character(entrezIDs)
rt=cbind(rt, entrezIDs)
gene=entrezIDs[entrezIDs!="NA"]        #ȥ������idΪNA�Ļ���
Cor=as.vector(rt[,3])
names(Cor)=gene

#������ɫ����
colorSel="qvalue"
if(qvalueFilter>0.05){
	colorSel="pvalue"
}

#kegg��������
kk <- enrichKEGG(gene = gene, organism = "hsa", pvalueCutoff =1, qvalueCutoff =1)
KEGG=as.data.frame(kk)
KEGG$geneID=as.character(sapply(KEGG$geneID,function(x)paste(rt$Gene[match(strsplit(x,"/")[[1]],as.character(rt$entrezID))],collapse="/")))
KEGG=KEGG[(KEGG$pvalue<pvalueFilter & KEGG$qvalue<qvalueFilter),]
#���渻�����
write.table(KEGG,file="KEGG.txt",sep="\t",quote=F,row.names = F)

#������ʾTerm��Ŀ
showNum=30
if(nrow(KEGG)<showNum){
	showNum=nrow(KEGG)
}

#��״ͼ
pdf(file="barplot.pdf",width = 9,height = 7)
barplot(kk, drop = TRUE, showCategory = showNum, color = colorSel)
dev.off()

#����ͼ
pdf(file="bubble.pdf",width = 9,height = 7)
dotplot(kk, showCategory = showNum, orderBy = "GeneRatio",color = colorSel)
dev.off()

#����ͨ·ͼ
keggId="hsa04115"       #ѡ��ͨ·������ͨ·ͼ����(�����޸�)
pv.out=pathview(gene.data=Cor, pathway.id = keggId, species = "hsa", out.suffix = "pathview")

