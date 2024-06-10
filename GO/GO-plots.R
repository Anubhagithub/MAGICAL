library(ggplot2)
data3 = read.csv("/home/nikola/review_magical/go-analysis/GO-bp-input-for-plots.csv")
a = data3[which(data3$gi == "SL"),]
a.avggo = data.frame(a[,c(1)])
colnames(a.avggo)[1] = "SL"
b = data3[which(data3$gi == "SV"),]
b.avggo = data.frame(b[,c(1)])
colnames(b.avggo)[1] = "SV"
b.avggo[572:794,] = ""
c = data3[which(data3$gi == "NOT"),]
c.avggo = data.frame(c[,c(1)])
colnames(c.avggo)[1] = "NOT"
c.avggo[158:794,] = ""
new = cbind(a.avggo,b.avggo,c.avggo)
newcp = new
newcp$SV = as.numeric(newcp$SV)
newcp$NOT = as.numeric(newcp$NOT)
##newcp2 = na.omit(newcp)
###plot
gg <- melt(newcp)
gg2 = na.omit(gg)
##gg <- melt(newcp2) do not use this instead use newcp with NAs 
tiff("histogram-avggo-final.tiff",width = 1200, height = 1000, res = 300)
par(mar=c(0.5,2.5,0.5,0.5),cex.axis=1.4, font.axis=4,cex.lab=1.4, font.lab=4)
ggplot(gg2, aes(x=value, fill=variable,alpha = 0.4)) + xlab("Average GO terms")+
  geom_histogram(binwidth=10)+scale_fill_manual(values = c("#980043", "#008B8B","#969696"))+
  facet_grid(variable~.)+theme_classic() + theme(legend.position="none")
dev.off()
###horizontal box plot for JI
############################################################################
#data3cp = data3
data3$gi <- factor(data3$gi, levels = c("NOT", "SV", "SL"))
tiff("JI-horizontal.tiff",width = 1200, height = 1000, res = 300)
par(mar=c(0.5,2.5,0.5,0.5),cex.axis=1.4, font.axis=4,cex.lab=1.4, font.lab=4)
#levels(data3$gi) <- c("SL", "SV", "NOT") #not needed
ggplot(data3, aes(x=gi,y=jc, fill = gi, alpha = 0.4)) +
  xlab("")+ ylab("Jacaard Index")+
  scale_y_continuous(limits=c(0,0.5))+
  geom_boxplot(outlier.shape = NA) +theme_classic() + theme(legend.position="none")+
  scale_fill_manual(values=c("#969696","#008B8B","#980043"))+
  coord_flip()
dev.off()
###density plot for JI
tiff("density-JI-bp-final2.tiff",width = 1200, height = 1000, res = 300)
par(mar=c(0.5,2.5,0.5,0.5),cex.axis=1.4, font.axis=4,cex.lab=1.4, font.lab=4)
ggplot(data3, aes(x = jc, y = stat(density), colour = gi)) +xlab("Jaccard Index")+theme_classic() +
  geom_line(stat = "density")+scale_color_manual(values=c("#980043", "#008B8B","#969696"))
dev.off()
