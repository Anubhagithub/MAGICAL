###Jacaard index of SL, SV and NOT pairs in Biological process 
sl.biogrid = read.csv("/home/user/MAGICAL/GO/biogrid-sl-genename-intr.csv")
sv.biogrid = read.csv("/home/user/MAGICAL/GO/biogrid-sv-genename-intr.csv")
not0 = read.csv("/home/user/MAGICAL/GO/not-0th-genename-intr.csv")
not0 = not0[,c(1,2)]
not1 = read.csv("/home/user/MAGICAL/GO/not-1st-genename-intr.csv")
not1 = not1[,c(1,2)]
not2 = unique(rbind(not0, not1))
onto.bp2 = read.csv("/home/user/MAGICAL/GO/ontology-bp.csv")
go.gn = read.csv("/home/user/MAGICAL/GO/foo", sep = "", header = F)
go.gn2 = go.gn[,c(2,3)]
colnames(go.gn2)[1] = "genename"
colnames(go.gn2)[2] = "id"
merge = merge(onto.bp2, go.gn2, by = "id")
merge = merge[order(merge$genename,decreasing = FALSE), ]
merge2 = merge[,c(2,1)]
#write.csv(merge2, "merge2.csv", row.names = F, quote = F)
merge3 = aggregate(merge2$id~merge2$genename,FUN = toString)
anyNA(merge3)
go.genename = merge3
colnames(go.genename)[1] = "genename"
colnames(go.genename)[2] = "id"
head(go.genename)
sl.bio = sl.biogrid
colnames(sl.bio)[1] = "genename"
head(sl.bio)
library(plyr)
tot_g1 = join(sl.bio,go.genename) 
anyNA(tot_g1)
tot.g1 = tot_g1
sl.bio2 = sl.biogrid
colnames(sl.bio2)[2] = "genename"
head(sl.bio2)
tot_g2 = join(sl.bio2,go.genename) 
anyNA(tot_g2)
tot.g2 = tot_g2
tot = cbind(tot.g1, tot.g2) 
totcp = tot[,c(1,3,5,6)] 
totcp3 = na.omit(totcp) 
#intr = data.frame(intersect(totcp3$id, totcp3$id.1))
file = totcp3
file$intr <- Map(intersect, strsplit(file$id,", "), strsplit(file$id.1, ", "))
file$union <- Map(union, strsplit(file$id,", "), strsplit(file$id.1, ", "))
file2 = file
file2$intr = as.character(file2$intr) 
file2$union = as.character(file2$union) 
file3 = file2[which(file2$intr != "character(0)"),] 
#file3 = file2[which(file2$union != "character(0)"),]
for(i in 1:nrow(file3)){
  x = unlist(strsplit(file3$id,", ")[i])
  y = unlist(strsplit(file3$id.1,", ")[i])
  file3$avggo[i] = (length(x)+length(y))/2
}
file4 = file3[,c(1,3,5,6,7)]
file5 = file4
file5$intr2 = as.character(file5$intr)
file5$union2 = as.character(file5$union)
for(i in 1:nrow(file5)){
  a = unlist(strsplit(file5$intr2,", ")[i])
  b = unlist(strsplit(file5$union2,", ")[i])
  file5$overlap[i] = length(a)
  file5$jc[i] = length(a)/length(b)
}
sl.bp.jc = file5
#####################################################################
### JC of SV pairs in BP
sv.bio = sv.biogrid
colnames(sv.bio)[1] = "genename"
#head(sv.bio)
library(plyr)
tot_g1 = join(sv.bio,go.genename) 
anyNA(tot_g1)
tot.g1 = tot_g1
sv.bio2 = sv.biogrid
colnames(sv.bio2)[2] = "genename"
head(sv.bio2)
tot_g2 = join(sv.bio2,go.genename) 
anyNA(tot_g2)
tot.g2 = tot_g2
tot = cbind(tot.g1, tot.g2) 
totcp = tot[,c(1,3,5,6)] 
totcp3 = na.omit(totcp) 
file = totcp3
file$intr <- Map(intersect, strsplit(file$id,", "), strsplit(file$id.1, ", "))
file$union <- Map(union, strsplit(file$id,", "), strsplit(file$id.1, ", "))
file2 = file
file2$intr = as.character(file2$intr) 
file2$union = as.character(file2$union) 
file3 = file2[which(file2$intr != "character(0)"),] 
##identify number of GO terms for nboth the gene pair and average them
for(i in 1:nrow(file3)){
  x = unlist(strsplit(file3$id,", ")[i])
  y = unlist(strsplit(file3$id.1,", ")[i])
  file3$avggo[i] = (length(x)+length(y))/2
}
#file3 = file2[which(file2$union != "character(0)"),]
file4 = file3[,c(1,3,5,6,7)]
file5 = file4
file5$intr2 = as.character(file5$intr)
file5$union2 = as.character(file5$union)
for(i in 1:nrow(file5)){
  a = unlist(strsplit(file5$intr2,", ")[i])
  b = unlist(strsplit(file5$union2,", ")[i])
  file5$overlap[i] = length(a)
  file5$jc[i] = length(a)/length(b)
}
sv.bp.jc = file5
#####NOT pairs
### JC of NOT pairs in BP
not.bio = not2
colnames(not.bio)[1] = "genename"
#head(sv.bio)
library(plyr)
tot_g1 = join(not.bio,go.genename) 
anyNA(tot_g1)
tot.g1 = tot_g1
not.bio2 = not2
colnames(not.bio2)[2] = "genename"
head(sv.bio2)
tot_g2 = join(not.bio2,go.genename) 
anyNA(tot_g2)
tot.g2 = tot_g2
tot = cbind(tot.g1, tot.g2) 
totcp = tot[,c(1,3,5,6)] 
totcp3 = na.omit(totcp) 
file = totcp3
file$intr <- Map(intersect, strsplit(file$id,", "), strsplit(file$id.1, ", "))
file$union <- Map(union, strsplit(file$id,", "), strsplit(file$id.1, ", "))
file2 = file
file2$intr = as.character(file2$intr) 
file2$union = as.character(file2$union) 
file3 = file2[which(file2$intr != "character(0)"),] 
##identify number of GO terms for nboth the gene pair and average them
for(i in 1:nrow(file3)){
  x = unlist(strsplit(file3$id,", ")[i])
  y = unlist(strsplit(file3$id.1,", ")[i])
  file3$avggo[i] = (length(x)+length(y))/2
}
#file3 = file2[which(file2$union != "character(0)"),]
file4 = file3[,c(1,3,5,6,7)]
file5 = file4
file5$intr2 = as.character(file5$intr)
file5$union2 = as.character(file5$union)
for(i in 1:nrow(file5)){
  a = unlist(strsplit(file5$intr2,", ")[i])
  b = unlist(strsplit(file5$union2,", ")[i])
  file5$overlap[i] = length(a)
  file5$jc[i] = length(a)/length(b)
}
not.bp.jc = file5
####box plot
data = data.frame(sl.bp.jc[,c(5,8,9)])
#colnames(data)[1] = "jc"
data$gi = "SL"
data2 = data.frame(sv.bp.jc[,c(5,8,9)])
#colnames(data2)[1] = "jc"
data2$gi = "SV"
datanot = data.frame(not.bp.jc[,c(5,8,9)])
datanot$gi = "NOT"
data3 = rbind(data, data2,datanot)
write.csv(data3,"/home/user/MAGICAL/GO/GO-bp-input-for-plots.csv", row.names = F, quote = F)
