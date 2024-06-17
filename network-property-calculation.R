###Read the input file
###calculating the network properties
ppi = read.csv("/home/MAGICAL/Data/biogrid-entrez-pairs", sep = "\t", header = F) #377641
ppiuni = unique(ppi) #159541
ppiuni2 = ppiuni[!duplicated(t(apply(ppiuni, 1, sort))),] #107023
ppiuni3 <- ppiuni2[ppiuni2$V1 != ppiuni2$V2,] #104128
anyNA(ppiuni3)
#network creation
library(igraph)
Network3 <- graph_from_data_frame(ppiuni3 , directed=FALSE)
abc = as.data.frame(degree(Network3))
abc$names <- rownames(abc)
abc$degree = degree(Network3)
abc = abc[, -1]
row.names(abc) = NULL
abc$betwn <- betweenness(Network3)
abc$close = closeness(Network3)
abc$core = coreness(Network3)
abc$constraint = constraint(Network3)
abc$ecc = eccentricity(Network3)
ec <- evcent(Network3)$vector
abc$eigen_cen = ec
hs <- hub_score(Network3)$vector
abc$hub_score = hs
abc$neighbor1 <- neighborhood.size(Network3, order = 1)
abc$neighbor2 <- neighborhood.size(Network3, order = 2)
abc$neighbor3 <- neighborhood.size(Network3, order = 3)
abc$neighbor4 <- neighborhood.size(Network3, order = 4)
abc$neighbor5 <- neighborhood.size(Network3, order = 5)
abc$neighbor6 <- neighborhood.size(Network3, order = 6)
leiden2 = cluster_leiden(
  Network3,
  objective_function = "CPM",
  weights = NULL,
  resolution_parameter = 0.005,
  beta = 0.01,
  initial_membership = NULL,
  n_iterations = 2,
  vertex_weights = NULL
)
abc$leiden = leiden2$membership
abc$leiden = leiden2$membership
triangle = count_triangles(Network3)
abc$triangle = triangle
#common neighbors
n = c()
mat = matrix(ncol = 0, nrow = 0)
n2 =data.frame(mat)
for(i in 1:26978){
  n = as.numeric(neighbors(Network3,i))
  n = length(n)
  n2 = rbind(n2,n)
}
abc$common_neighbor = n2$X19L
write.csv(abc, "biogrid-entrez-nodeiwse.csv", quote = F, row.names = F)
####after calculating the nodewise network properties we mapped the SL, SV,
####from CGIdb, BioGRID, SynLethDB, along with NOT pairs to this network
####calculating the nodewise and pairwise properties for all SL, SV and NOT pairs
##file = read.csv("/home/mito/srf2023/biogrid-new-ppi/crispr/biogrid-entrez-nodeiwse.csv")
####sldb sl
# sldb = read.csv("/home/mito/srf2023/expsl2.csv", sep = ",")
# sldb = sldb[,c(2,4)]
# sldb = unique(sldb)
# sldb = sldb[!duplicated(t(apply(sldb, 1, sort))),]
# sldb <- sldb[sldb$n1.identifier != sldb$n2.identifier,]
# anyNA(sldb)
# start = proc.time()
# library(doParallel)
# library(foreach)
# n.cores <- parallel::detectCores() - 1
# c1 <- parallel::makeCluster(
#   n.cores,
#   type = "FORK"
# )
# doParallel::registerDoParallel(c1)
# net_cp = file
# not_cp = sldb
# result = c()
# result = foreach(x = 1:nrow(not_cp),.packages='doParallel',.combine = 'rbind') %dopar% {
#   foreach(y = 1:nrow(net_cp), .packages='doParallel',.combine = 'rbind') %do%
#     {
#       if(grepl(not_cp[x,1], net_cp[y,1]) && nchar(not_cp[x,1]) == nchar(net_cp[y,1]))
#       {
#         foreach(z = 1:nrow(net_cp), .packages='doParallel',.combine = 'rbind') %do%
#           {
#             if(grepl(not_cp[x,2], net_cp[z,1]) && nchar(not_cp[x,2]) == nchar(net_cp[z,1]))
#             {
#               c(net_cp[y,], net_cp[z,])
#               #list2 = rbind(list2, c(net_cp[y,], net_cp[z,]))
#               #c(paste(c(net_cp[y,], net_cp[z,])))
#               #break
#             }
#           }
#       }
#     }
# }
# final = as.data.frame(result)
# end = proc.time()
# time_taken = end - start
# stopCluster(c1)
# print(dim(result))
# write.csv(result, "sldb-sl-nodewise.csv", quote = F, row.names = F)
# ###sldb.sr
# sldb = read.csv("/home/mito/srf2023/Human_SR.csv", sep = ",")
# sldb = sldb[,c(2,4)]
# sldb = unique(sldb)
# sldb = sldb[!duplicated(t(apply(sldb, 1, sort))),]
# sldb <- sldb[sldb$n1.identifier != sldb$n2.identifier,]
# start = proc.time()
# library(doParallel)
# library(foreach)
# n.cores <- parallel::detectCores() - 1
# c1 <- parallel::makeCluster(
#   n.cores,
#   type = "FORK"
# )
# doParallel::registerDoParallel(c1)
# net_cp = file
# not_cp = sldb
# result = c()
# result = foreach(x = 1:nrow(not_cp),.packages='doParallel',.combine = 'rbind') %dopar% {
#   foreach(y = 1:nrow(net_cp), .packages='doParallel',.combine = 'rbind') %do%
#     {
#       if(grepl(not_cp[x,1], net_cp[y,1]) && nchar(not_cp[x,1]) == nchar(net_cp[y,1]))
#       {
#         foreach(z = 1:nrow(net_cp), .packages='doParallel',.combine = 'rbind') %do%
#           {
#             if(grepl(not_cp[x,2], net_cp[z,1]) && nchar(not_cp[x,2]) == nchar(net_cp[z,1]))
#             {
#               c(net_cp[y,], net_cp[z,])
#               #list2 = rbind(list2, c(net_cp[y,], net_cp[z,]))
#               #c(paste(c(net_cp[y,], net_cp[z,])))
#               #break
#             }
#           }
#       }
#     }
# }
# final = as.data.frame(result)
# end = proc.time()
# time_taken = end - start
# stopCluster(c1)
# print(dim(result))
# write.csv(result, "sldb-sv-nodewise.csv", quote = F, row.names = F)
# ###################################################################################
# ##not trial
# not = read.csv("/home/mito/srf2023/not3/datafram_t00.csv")
# not = unique(not)
# not = not[!duplicated(t(apply(not, 1, sort))),]
# not <- not[not$gene1 != not$gene2,]
# start = proc.time()
# library(doParallel)
# library(foreach)
# n.cores <- parallel::detectCores() - 1
# c1 <- parallel::makeCluster(
#   n.cores,
#   type = "FORK"
# )
# doParallel::registerDoParallel(c1)
# net_cp = file
# not_cp = not
# result = c()
# result = foreach(x = 1:nrow(not_cp),.packages='doParallel',.combine = 'rbind') %dopar% {
#   foreach(y = 1:nrow(net_cp), .packages='doParallel',.combine = 'rbind') %do%
#     {
#       if(grepl(not_cp[x,1], net_cp[y,1]) && nchar(not_cp[x,1]) == nchar(net_cp[y,1]))
#       {
#         foreach(z = 1:nrow(net_cp), .packages='doParallel',.combine = 'rbind') %do%
#           {
#             if(grepl(not_cp[x,2], net_cp[z,1]) && nchar(not_cp[x,2]) == nchar(net_cp[z,1]))
#             {
#               c(net_cp[y,], net_cp[z,])
#               #list2 = rbind(list2, c(net_cp[y,], net_cp[z,]))
#               #c(paste(c(net_cp[y,], net_cp[z,])))
#               #break
#             }
#           }
#       }
#     }
# }
# final = as.data.frame(result)
# end = proc.time()
# time_taken = end - start
# stopCluster(c1)
# print(dim(result))
# write.csv(result, "not0-nodewise.csv", quote = F, row.names = F)
# # ##not1
# not = read.csv("/home/mito/srf2023/not3/datafram_t01.csv", header = F)
# print(head(not))
# not = unique(not)
# not = not[!duplicated(t(apply(not, 1, sort))),]
# not <- not[not$V1 != not$V2,]
# print(head(not))
# start = proc.time()
# library(doParallel)
# library(foreach)
# n.cores <- parallel::detectCores() - 1
# c1 <- parallel::makeCluster(
#   n.cores,
#   type = "FORK"
# )
# doParallel::registerDoParallel(c1)
# net_cp = file
# not_cp = not
# result = c()
# result = foreach(x = 1:nrow(not_cp),.packages='doParallel',.combine = 'rbind') %dopar% {
#   foreach(y = 1:nrow(net_cp), .packages='doParallel',.combine = 'rbind') %do%
#     {
#       if(grepl(not_cp[x,1], net_cp[y,1]) && nchar(not_cp[x,1]) == nchar(net_cp[y,1]))
#       {
#         foreach(z = 1:nrow(net_cp), .packages='doParallel',.combine = 'rbind') %do%
#           {
#             if(grepl(not_cp[x,2], net_cp[z,1]) && nchar(not_cp[x,2]) == nchar(net_cp[z,1]))
#             {
#               c(net_cp[y,], net_cp[z,])
#               #list2 = rbind(list2, c(net_cp[y,], net_cp[z,]))
#               #c(paste(c(net_cp[y,], net_cp[z,])))
#               #break
#             }
#           }
#       }
#     }
# }
# final = as.data.frame(result)
# end = proc.time()
# time_taken = end - start
# stopCluster(c1)
# print(dim(result))
# write.csv(result, "not1-nodewise.csv", quote = F, row.names = F)
# ###biogrid
# sv = read.csv("/home/mito/comm_detect/new_netprop/positive_intr_biogrid.csv", sep = "\t", header = F)
# sv = unique(sv)
# sv = sv[!duplicated(t(apply(sv, 1, sort))),]
# sv <- sv[sv$V1 != sv$V2,]
# anyNA(sv)
# start = proc.time()
# library(doParallel)
# library(foreach)
# n.cores <- parallel::detectCores() - 1
# c1 <- parallel::makeCluster(
#   n.cores,
#   type = "FORK"
# )
# doParallel::registerDoParallel(c1)
# net_cp = file
# not_cp = sv
# result = c()
# result = foreach(x = 1:nrow(not_cp),.packages='doParallel',.combine = 'rbind') %dopar% {
#   foreach(y = 1:nrow(net_cp), .packages='doParallel',.combine = 'rbind') %do%
#     {
#       if(grepl(not_cp[x,1], net_cp[y,1]) && nchar(not_cp[x,1]) == nchar(net_cp[y,1]))
#       {
#         foreach(z = 1:nrow(net_cp), .packages='doParallel',.combine = 'rbind') %do%
#           {
#             if(grepl(not_cp[x,2], net_cp[z,1]) && nchar(not_cp[x,2]) == nchar(net_cp[z,1]))
#             {
#               c(net_cp[y,], net_cp[z,])
#               #list2 = rbind(list2, c(net_cp[y,], net_cp[z,]))
#               #c(paste(c(net_cp[y,], net_cp[z,])))
#               #break
#             }
#           }
#       }
#     }
# }
# final = as.data.frame(result)
# end = proc.time()
# time_taken = end - start
# stopCluster(c1)
# print(dim(result))
# write.csv(result, "biogrid-sv-nodewise.csv", quote = F, row.names = F)
# ###biogrid sl
# sl = read.csv("/home/mito/comm_detect/new_netprop/negative_intr_biogrid.csv", sep = "\t", header = F)
# sl = unique(sl)
# sl = sl[!duplicated(t(apply(sl, 1, sort))),]
# sl <- sl[sl$V1 != sl$V2,]
# anyNA(sl)
# start = proc.time()
# library(doParallel)
# library(foreach)
# n.cores <- parallel::detectCores() - 1
# c1 <- parallel::makeCluster(
#   n.cores,
#   type = "FORK"
# )
# doParallel::registerDoParallel(c1)
# net_cp = file
# not_cp = sl
# result = c()
# result = foreach(x = 1:nrow(not_cp),.packages='doParallel',.combine = 'rbind') %dopar% {
#   foreach(y = 1:nrow(net_cp), .packages='doParallel',.combine = 'rbind') %do%
#     {
#       if(grepl(not_cp[x,1], net_cp[y,1]) && nchar(not_cp[x,1]) == nchar(net_cp[y,1]))
#       {
#         foreach(z = 1:nrow(net_cp), .packages='doParallel',.combine = 'rbind') %do%
#           {
#             if(grepl(not_cp[x,2], net_cp[z,1]) && nchar(not_cp[x,2]) == nchar(net_cp[z,1]))
#             {
#               c(net_cp[y,], net_cp[z,])
#               #list2 = rbind(list2, c(net_cp[y,], net_cp[z,]))
#               #c(paste(c(net_cp[y,], net_cp[z,])))
#               #break
#             }
#           }
#       }
#     }
# }
# final = as.data.frame(result)
# end = proc.time()
# time_taken = end - start
# stopCluster(c1)
# print(dim(result))
# write.csv(result, "biogrid-sl-nodewise.csv", quote = F, row.names = F)
# sl = read.csv("/home/mito/comm_detect/new_netprop/exp_SL_entrezid.csv")
# sl = unique(sl)
# sl = sl[!duplicated(t(apply(sl, 1, sort))),]
# sl <- sl[sl$geneid1 != sl$geneid2,]
# start = proc.time()
# library(doParallel)
# library(foreach)
# n.cores <- parallel::detectCores() - 1
# c1 <- parallel::makeCluster(
#   n.cores,
#   type = "FORK"
# )
# doParallel::registerDoParallel(c1)
# net_cp = file
# not_cp = sl
# result = c()
# result = foreach(x = 1:nrow(not_cp),.packages='doParallel',.combine = 'rbind') %dopar% {
#   foreach(y = 1:nrow(net_cp), .packages='doParallel',.combine = 'rbind') %do%
#     {
#       if(grepl(not_cp[x,1], net_cp[y,1]) && nchar(not_cp[x,1]) == nchar(net_cp[y,1]))
#       {
#         foreach(z = 1:nrow(net_cp), .packages='doParallel',.combine = 'rbind') %do%
#           {
#             if(grepl(not_cp[x,2], net_cp[z,1]) && nchar(not_cp[x,2]) == nchar(net_cp[z,1]))
#             {
#               c(net_cp[y,], net_cp[z,])
#               #list2 = rbind(list2, c(net_cp[y,], net_cp[z,]))
#               #c(paste(c(net_cp[y,], net_cp[z,])))
#               #break
#             }
#           }
#       }
#     }
# }
# final = as.data.frame(result)
# end = proc.time()
# time_taken = end - start
# stopCluster(c1)
# print(dim(result))
# write.csv(result, "cgidb-sl-nodewise.csv", quote = F, row.names = F)
# ###cgidb sv####
# sv = read.csv("/home/mito/comm_detect/new_netprop/exp_SV_entrezid.csv", sep = ",")
# sv = unique(sv)
# sv = sv[!duplicated(t(apply(sv, 1, sort))),]
# sv <- sv[sv$geneid1 != sv$geneid2,]
# start = proc.time()
# library(doParallel)
# library(foreach)
# n.cores <- parallel::detectCores() - 1
# c1 <- parallel::makeCluster(
#   n.cores,
#   type = "FORK"
# )
# doParallel::registerDoParallel(c1)
# net_cp = file
# not_cp = sv
# result = c()
# result = foreach(x = 1:nrow(not_cp),.packages='doParallel',.combine = 'rbind') %dopar% {
#   foreach(y = 1:nrow(net_cp), .packages='doParallel',.combine = 'rbind') %do%
#     {
#       if(grepl(not_cp[x,1], net_cp[y,1]) && nchar(not_cp[x,1]) == nchar(net_cp[y,1]))
#       {
#         foreach(z = 1:nrow(net_cp), .packages='doParallel',.combine = 'rbind') %do%
#           {
#             if(grepl(not_cp[x,2], net_cp[z,1]) && nchar(not_cp[x,2]) == nchar(net_cp[z,1]))
#             {
#               c(net_cp[y,], net_cp[z,])
#               #list2 = rbind(list2, c(net_cp[y,], net_cp[z,]))
#               #c(paste(c(net_cp[y,], net_cp[z,])))
#               #break
#             }
#           }
#       }
#     }
# }
# final = as.data.frame(result)
# end = proc.time()
# time_taken = end - start
# stopCluster(c1)
# print(dim(result))
# write.csv(result, "cgidb-sv-nodewise.csv", quote = F, row.names = F)
# ###################################################################################
# ##pairwise
ppi = read.csv("/home/mito/srf2023/biogrid-new-ppi/new-entrez/biogrid-entrez-pairs", sep = "\t", header = F)
ppiuni = unique(ppi)
ppiuni2 = ppiuni[!duplicated(t(apply(ppiuni, 1, sort))),]
ppiuni3 <- ppiuni2[ppiuni2$V1 != ppiuni2$V2,] #104128
anyNA(ppiuni3)
#network creation
library(igraph)
Network2 <- graph_from_data_frame(ppiuni3 , directed=FALSE)
print(diameter(Network2, directed = F)) #26
# sl.net = read.csv("biogrid-sl-nodewise.csv")
# inp2 = sl.net
# inp3 = inp2[, c(1,19,2,20,3,21,4,22,5,23,6,24,7,25,8,26,9,27,10,28,11,29,12,30,13,31,14,32,15,33,16,34,17,35,18,36)]
# inp3$degree = as.integer(inp3$degree)
# inp3$degree.1 = as.integer(inp3$degree.1)
# inp3$betwn = as.integer(inp3$betwn)
# inp3$betwn.1 = as.integer(inp3$betwn.1)
# inp3$close = as.integer(inp3$close)
# inp3$close.1 = as.integer(inp3$close.1)
# inp3$core = as.integer(inp3$core)
# inp3$core.1 = as.integer(inp3$core.1)
# inp3$constraint = as.integer(inp3$constraint)
# inp3$constraint.1 = as.integer(inp3$constraint.1)
# inp3$ecc = as.integer(inp3$ecc)
# inp3$ecc.1 = as.integer(inp3$ecc.1)
# inp3$eigen_cen = as.integer(inp3$eigen_cen)
# inp3$eigen_cen.1 = as.integer(inp3$eigen_cen.1)
# inp3$hub_score = as.integer(inp3$hub_score)
# inp3$hub_score.1 = as.integer(inp3$hub_score.1)
# inp3$neighbor1 = as.integer(inp3$neighbor1)
# inp3$neighbor1.1 = as.integer(inp3$neighbor1.1)
# inp3$neighbor2 = as.integer(inp3$neighbor2)
# inp3$neighbor2.1 = as.integer(inp3$neighbor2.1)
# inp3$neighbor3 = as.integer(inp3$neighbor3)
# inp3$neighbor3.1 = as.integer(inp3$neighbor3.1)
# inp3$neighbor4 = as.integer(inp3$neighbor4)
# inp3$neighbor4.1 = as.integer(inp3$neighbor4.1)
# inp3$neighbor5 = as.integer(inp3$neighbor5)
# inp3$neighbor5.1 = as.integer(inp3$neighbor5.1)
# inp3$neighbor6 = as.integer(inp3$neighbor6)
# inp3$neighbor6.1 = as.integer(inp3$neighbor6.1)
# inp3$triangle = as.integer(inp3$triangle)
# inp3$triangle.1 = as.integer(inp3$triangle.1)
# inp3$common_neighbor = as.integer(inp3$common_neighbor)
# inp3$common_neighbor.1 = as.integer(inp3$common_neighbor.1)
# #calculating avg for all properties
# inp3$avgdeg = (inp3$degree+inp3$degree.1)/2
# inp3$avgbet = (inp3$betwn+inp3$betwn.1)/2
# inp3$avgclose = (inp3$close+inp3$close.1)/2
# inp3$avgcore = (inp3$core+inp3$core.1)/2
# inp3$avgcons = (inp3$constraint+inp3$constraint.1)/2
# inp3$avgecc = (inp3$ecc+inp3$ecc.1)/2
# inp3$avgeigen_cen = (inp3$eigen_cen+inp3$eigen_cen.1)/2
# inp3$avghub_score = (inp3$hub_score+inp3$hub_score.1)/2
# inp3$avgneighbor1 = (inp3$neighbor1+inp3$neighbor1.1)/2
# inp3$avgneighbor2 = (inp3$neighbor2+inp3$neighbor2.1)/2
# inp3$avgneighbor3 = (inp3$neighbor3+inp3$neighbor3.1)/2
# inp3$avgneighbor4 = (inp3$neighbor4+inp3$neighbor4.1)/2
# inp3$avgneighbor5 = (inp3$neighbor5+inp3$neighbor5.1)/2
# inp3$avgneighbor6 = (inp3$neighbor6+inp3$neighbor6.1)/2
# inp3$avgtriangle = (inp3$triangle+inp3$triangle.1)/2
# inp3$avgcommon_neighbor = (inp3$common_neighbor+inp3$common_neighbor.1)/2
# inp3cp = inp3[, c(1:2,31,32,37:52)]
# inp4 = na.omit(inp3cp)
# inp4cp = inp4
# inp4cp.same = inp4cp[which(inp4cp$leiden == inp4cp$leiden.1),]
# inp4cp.same$comm.detect = 1
# inp4cp.diff = inp4cp[which(inp4cp$leiden != inp4cp$leiden.1),]
# inp4cp.diff$comm.detect = 0
# inp4 = rbind(inp4cp.same, inp4cp.diff)
# inp4 = inp4[,c(1,2,5:21)]
# dim(inp4)
# colnames(inp4)[1] = "gene1"
# colnames(inp4)[2] = "gene2"
# inp4$gene1 = as.factor(inp4$gene1)
# inp4$gene2 = as.factor(inp4$gene2)
# for (i in 1:length(inp4$gene1)){
#   V = inp4$gene1[i]
#   U = inp4$gene2[i]
#   inp4$shortest_path[i] <- distances(Network2, v = V, to = U)
# }
# #inp4 = inp4[, -18]
# inp4$cohesion = 0
# for (i in 1:length(inp4$gene1)){
#   if (inp4$shortest_path[i] > 1) {
#     V = inp4$gene1[i]
#     U = inp4$gene2[i]
#     print(i)
#     inp4$cohesion[i] <- vertex.connectivity(Network2, source = V,target = U)
#   }
# }
# inp4[inp4$shortest_path<=1,]$cohesion=NA
# #inp4 = inp4[, -19]
# inp4 = na.omit(inp4)
# inp4$adhesion = 0
# for (i in 1:length(inp4$gene1)){
#   V = inp4$gene1[i]
#   U = inp4$gene2[i]
#   print(i)
#   inp4$adhesion[i] <- edge.connectivity(Network2, source = V, target = U)
# }
# inp4 = na.omit(inp4)
# inp4cp1 = inp4
# inp4 = inp4cp1
# library(stringr)
# inp4$shortest_path <- str_replace(inp4$shortest_path, "Inf", "26")
# inp4$shortest_path = as.integer(inp4$shortest_path)
# print(dim(inp2))
# print(dim(inp4))
# write.csv(inp4, "biogrid-sl-pairwise-new-netprop", row.names = F, quote = F)
# ###biogrid sv
# sv.net = read.csv("biogrid-sv-nodewise.csv")
# inp2 = sv.net
# inp3 = inp2[, c(1,19,2,20,3,21,4,22,5,23,6,24,7,25,8,26,9,27,10,28,11,29,12,30,13,31,14,32,15,33,16,34,17,35,18,36)]
# inp3$degree = as.integer(inp3$degree)
# inp3$degree.1 = as.integer(inp3$degree.1)
# inp3$betwn = as.integer(inp3$betwn)
# inp3$betwn.1 = as.integer(inp3$betwn.1)
# inp3$close = as.integer(inp3$close)
# inp3$close.1 = as.integer(inp3$close.1)
# inp3$core = as.integer(inp3$core)
# inp3$core.1 = as.integer(inp3$core.1)
# inp3$constraint = as.integer(inp3$constraint)
# inp3$constraint.1 = as.integer(inp3$constraint.1)
# inp3$ecc = as.integer(inp3$ecc)
# inp3$ecc.1 = as.integer(inp3$ecc.1)
# inp3$eigen_cen = as.integer(inp3$eigen_cen)
# inp3$eigen_cen.1 = as.integer(inp3$eigen_cen.1)
# inp3$hub_score = as.integer(inp3$hub_score)
# inp3$hub_score.1 = as.integer(inp3$hub_score.1)
# inp3$neighbor1 = as.integer(inp3$neighbor1)
# inp3$neighbor1.1 = as.integer(inp3$neighbor1.1)
# inp3$neighbor2 = as.integer(inp3$neighbor2)
# inp3$neighbor2.1 = as.integer(inp3$neighbor2.1)
# inp3$neighbor3 = as.integer(inp3$neighbor3)
# inp3$neighbor3.1 = as.integer(inp3$neighbor3.1)
# inp3$neighbor4 = as.integer(inp3$neighbor4)
# inp3$neighbor4.1 = as.integer(inp3$neighbor4.1)
# inp3$neighbor5 = as.integer(inp3$neighbor5)
# inp3$neighbor5.1 = as.integer(inp3$neighbor5.1)
# inp3$neighbor6 = as.integer(inp3$neighbor6)
# inp3$neighbor6.1 = as.integer(inp3$neighbor6.1)
# inp3$triangle = as.integer(inp3$triangle)
# inp3$triangle.1 = as.integer(inp3$triangle.1)
# inp3$common_neighbor = as.integer(inp3$common_neighbor)
# inp3$common_neighbor.1 = as.integer(inp3$common_neighbor.1)
# #calculating avg for all properties
# inp3$avgdeg = (inp3$degree+inp3$degree.1)/2
# inp3$avgbet = (inp3$betwn+inp3$betwn.1)/2
# inp3$avgclose = (inp3$close+inp3$close.1)/2
# inp3$avgcore = (inp3$core+inp3$core.1)/2
# inp3$avgcons = (inp3$constraint+inp3$constraint.1)/2
# inp3$avgecc = (inp3$ecc+inp3$ecc.1)/2
# inp3$avgeigen_cen = (inp3$eigen_cen+inp3$eigen_cen.1)/2
# inp3$avghub_score = (inp3$hub_score+inp3$hub_score.1)/2
# inp3$avgneighbor1 = (inp3$neighbor1+inp3$neighbor1.1)/2
# inp3$avgneighbor2 = (inp3$neighbor2+inp3$neighbor2.1)/2
# inp3$avgneighbor3 = (inp3$neighbor3+inp3$neighbor3.1)/2
# inp3$avgneighbor4 = (inp3$neighbor4+inp3$neighbor4.1)/2
# inp3$avgneighbor5 = (inp3$neighbor5+inp3$neighbor5.1)/2
# inp3$avgneighbor6 = (inp3$neighbor6+inp3$neighbor6.1)/2
# inp3$avgtriangle = (inp3$triangle+inp3$triangle.1)/2
# inp3$avgcommon_neighbor = (inp3$common_neighbor+inp3$common_neighbor.1)/2
# inp3cp = inp3[, c(1:2,31,32,37:52)]
# inp4 = na.omit(inp3cp)
# inp4cp = inp4
# inp4cp.same = inp4cp[which(inp4cp$leiden == inp4cp$leiden.1),]
# inp4cp.same$comm.detect = 1
# inp4cp.diff = inp4cp[which(inp4cp$leiden != inp4cp$leiden.1),]
# inp4cp.diff$comm.detect = 0
# inp4 = rbind(inp4cp.same, inp4cp.diff)
# inp4 = inp4[,c(1,2,5:21)]
# dim(inp4)
# colnames(inp4)[1] = "gene1"
# colnames(inp4)[2] = "gene2"
# inp4$gene1 = as.factor(inp4$gene1)
# inp4$gene2 = as.factor(inp4$gene2)
# for (i in 1:length(inp4$gene1)){
#   V = inp4$gene1[i]
#   U = inp4$gene2[i]
#   inp4$shortest_path[i] <- distances(Network2, v = V, to = U)
# }
# #inp4 = inp4[, -18]
# inp4$cohesion = 0
# for (i in 1:length(inp4$gene1)){
#   if (inp4$shortest_path[i] > 1) {
#     V = inp4$gene1[i]
#     U = inp4$gene2[i]
#     print(i)
#     inp4$cohesion[i] <- vertex.connectivity(Network2, source = V,target = U)
#   }
# }
# inp4[inp4$shortest_path<=1,]$cohesion=NA
# #inp4 = inp4[, -19]
# inp4 = na.omit(inp4)
# inp4$adhesion = 0
# for (i in 1:length(inp4$gene1)){
#   V = inp4$gene1[i]
#   U = inp4$gene2[i]
#   print(i)
#   inp4$adhesion[i] <- edge.connectivity(Network2, source = V, target = U)
# }
# inp4 = na.omit(inp4)
# inp4$shortest_path <- str_replace(inp4$shortest_path, "Inf", "26")
# inp4$shortest_path = as.integer(inp4$shortest_path)
# print(dim(inp2))
# print(dim(inp4))
# write.csv(inp4, "biogrid-sv-pairwise-new-netprop", row.names = F, quote = F)
# ####cgidb
# ####sl
# sl.net = read.csv("cgidb-sl-nodewise.csv")
# inp2 = sl.net
# inp3 = inp2[, c(1,19,2,20,3,21,4,22,5,23,6,24,7,25,8,26,9,27,10,28,11,29,12,30,13,31,14,32,15,33,16,34,17,35,18,36)]
# inp3$degree = as.integer(inp3$degree)
# inp3$degree.1 = as.integer(inp3$degree.1)
# inp3$betwn = as.integer(inp3$betwn)
# inp3$betwn.1 = as.integer(inp3$betwn.1)
# inp3$close = as.integer(inp3$close)
# inp3$close.1 = as.integer(inp3$close.1)
# inp3$core = as.integer(inp3$core)
# inp3$core.1 = as.integer(inp3$core.1)
# inp3$constraint = as.integer(inp3$constraint)
# inp3$constraint.1 = as.integer(inp3$constraint.1)
# inp3$ecc = as.integer(inp3$ecc)
# inp3$ecc.1 = as.integer(inp3$ecc.1)
# inp3$eigen_cen = as.integer(inp3$eigen_cen)
# inp3$eigen_cen.1 = as.integer(inp3$eigen_cen.1)
# inp3$hub_score = as.integer(inp3$hub_score)
# inp3$hub_score.1 = as.integer(inp3$hub_score.1)
# inp3$neighbor1 = as.integer(inp3$neighbor1)
# inp3$neighbor1.1 = as.integer(inp3$neighbor1.1)
# inp3$neighbor2 = as.integer(inp3$neighbor2)
# inp3$neighbor2.1 = as.integer(inp3$neighbor2.1)
# inp3$neighbor3 = as.integer(inp3$neighbor3)
# inp3$neighbor3.1 = as.integer(inp3$neighbor3.1)
# inp3$neighbor4 = as.integer(inp3$neighbor4)
# inp3$neighbor4.1 = as.integer(inp3$neighbor4.1)
# inp3$neighbor5 = as.integer(inp3$neighbor5)
# inp3$neighbor5.1 = as.integer(inp3$neighbor5.1)
# inp3$neighbor6 = as.integer(inp3$neighbor6)
# inp3$neighbor6.1 = as.integer(inp3$neighbor6.1)
# inp3$triangle = as.integer(inp3$triangle)
# inp3$triangle.1 = as.integer(inp3$triangle.1)
# inp3$common_neighbor = as.integer(inp3$common_neighbor)
# inp3$common_neighbor.1 = as.integer(inp3$common_neighbor.1)
# #calculating avg for all properties
# inp3$avgdeg = (inp3$degree+inp3$degree.1)/2
# inp3$avgbet = (inp3$betwn+inp3$betwn.1)/2
# inp3$avgclose = (inp3$close+inp3$close.1)/2
# inp3$avgcore = (inp3$core+inp3$core.1)/2
# inp3$avgcons = (inp3$constraint+inp3$constraint.1)/2
# inp3$avgecc = (inp3$ecc+inp3$ecc.1)/2
# inp3$avgeigen_cen = (inp3$eigen_cen+inp3$eigen_cen.1)/2
# inp3$avghub_score = (inp3$hub_score+inp3$hub_score.1)/2
# inp3$avgneighbor1 = (inp3$neighbor1+inp3$neighbor1.1)/2
# inp3$avgneighbor2 = (inp3$neighbor2+inp3$neighbor2.1)/2
# inp3$avgneighbor3 = (inp3$neighbor3+inp3$neighbor3.1)/2
# inp3$avgneighbor4 = (inp3$neighbor4+inp3$neighbor4.1)/2
# inp3$avgneighbor5 = (inp3$neighbor5+inp3$neighbor5.1)/2
# inp3$avgneighbor6 = (inp3$neighbor6+inp3$neighbor6.1)/2
# inp3$avgtriangle = (inp3$triangle+inp3$triangle.1)/2
# inp3$avgcommon_neighbor = (inp3$common_neighbor+inp3$common_neighbor.1)/2
# inp3cp = inp3[, c(1:2,31,32,37:52)]
# inp4 = na.omit(inp3cp)
# inp4cp = inp4
# inp4cp.same = inp4cp[which(inp4cp$leiden == inp4cp$leiden.1),]
# inp4cp.same$comm.detect = 1
# inp4cp.diff = inp4cp[which(inp4cp$leiden != inp4cp$leiden.1),]
# inp4cp.diff$comm.detect = 0
# inp4 = rbind(inp4cp.same, inp4cp.diff)
# inp4 = inp4[,c(1,2,5:21)]
# dim(inp4)
# colnames(inp4)[1] = "gene1"
# colnames(inp4)[2] = "gene2"
# inp4$gene1 = as.factor(inp4$gene1)
# inp4$gene2 = as.factor(inp4$gene2)
# for (i in 1:length(inp4$gene1)){
#   V = inp4$gene1[i]
#   U = inp4$gene2[i]
#   inp4$shortest_path[i] <- distances(Network2, v = V, to = U)
# }
# #inp4 = inp4[, -18]
# inp4$cohesion = 0
# for (i in 1:length(inp4$gene1)){
#   if (inp4$shortest_path[i] > 1) {
#     V = inp4$gene1[i]
#     U = inp4$gene2[i]
#     print(i)
#     inp4$cohesion[i] <- vertex.connectivity(Network2, source = V,target = U)
#   }
# }
# inp4[inp4$shortest_path<=1,]$cohesion=NA
# #inp4 = inp4[, -19]
# inp4 = na.omit(inp4)
# inp4$adhesion = 0
# for (i in 1:length(inp4$gene1)){
#   V = inp4$gene1[i]
#   U = inp4$gene2[i]
#   print(i)
#   inp4$adhesion[i] <- edge.connectivity(Network2, source = V, target = U)
# }
# inp4 = na.omit(inp4)
# inp4$shortest_path <- str_replace(inp4$shortest_path, "Inf", "26")
# inp4$shortest_path = as.integer(inp4$shortest_path)
# print(dim(inp2))
# print(dim(inp4))
# write.csv(inp4, "cgidb-sl-pairwise-new-netprop", row.names = F, quote = F)
# ###sldb
# ###sl
# sl.net = read.csv("sldb-sl-nodewise.csv")
# inp2 = sl.net
# inp3 = inp2[, c(1,19,2,20,3,21,4,22,5,23,6,24,7,25,8,26,9,27,10,28,11,29,12,30,13,31,14,32,15,33,16,34,17,35,18,36)]
# inp3$degree = as.integer(inp3$degree)
# inp3$degree.1 = as.integer(inp3$degree.1)
# inp3$betwn = as.integer(inp3$betwn)
# inp3$betwn.1 = as.integer(inp3$betwn.1)
# inp3$close = as.integer(inp3$close)
# inp3$close.1 = as.integer(inp3$close.1)
# inp3$core = as.integer(inp3$core)
# inp3$core.1 = as.integer(inp3$core.1)
# inp3$constraint = as.integer(inp3$constraint)
# inp3$constraint.1 = as.integer(inp3$constraint.1)
# inp3$ecc = as.integer(inp3$ecc)
# inp3$ecc.1 = as.integer(inp3$ecc.1)
# inp3$eigen_cen = as.integer(inp3$eigen_cen)
# inp3$eigen_cen.1 = as.integer(inp3$eigen_cen.1)
# inp3$hub_score = as.integer(inp3$hub_score)
# inp3$hub_score.1 = as.integer(inp3$hub_score.1)
# inp3$neighbor1 = as.integer(inp3$neighbor1)
# inp3$neighbor1.1 = as.integer(inp3$neighbor1.1)
# inp3$neighbor2 = as.integer(inp3$neighbor2)
# inp3$neighbor2.1 = as.integer(inp3$neighbor2.1)
# inp3$neighbor3 = as.integer(inp3$neighbor3)
# inp3$neighbor3.1 = as.integer(inp3$neighbor3.1)
# inp3$neighbor4 = as.integer(inp3$neighbor4)
# inp3$neighbor4.1 = as.integer(inp3$neighbor4.1)
# inp3$neighbor5 = as.integer(inp3$neighbor5)
# inp3$neighbor5.1 = as.integer(inp3$neighbor5.1)
# inp3$neighbor6 = as.integer(inp3$neighbor6)
# inp3$neighbor6.1 = as.integer(inp3$neighbor6.1)
# inp3$triangle = as.integer(inp3$triangle)
# inp3$triangle.1 = as.integer(inp3$triangle.1)
# inp3$common_neighbor = as.integer(inp3$common_neighbor)
# inp3$common_neighbor.1 = as.integer(inp3$common_neighbor.1)
# #calculating avg for all properties
# inp3$avgdeg = (inp3$degree+inp3$degree.1)/2
# inp3$avgbet = (inp3$betwn+inp3$betwn.1)/2
# inp3$avgclose = (inp3$close+inp3$close.1)/2
# inp3$avgcore = (inp3$core+inp3$core.1)/2
# inp3$avgcons = (inp3$constraint+inp3$constraint.1)/2
# inp3$avgecc = (inp3$ecc+inp3$ecc.1)/2
# inp3$avgeigen_cen = (inp3$eigen_cen+inp3$eigen_cen.1)/2
# inp3$avghub_score = (inp3$hub_score+inp3$hub_score.1)/2
# inp3$avgneighbor1 = (inp3$neighbor1+inp3$neighbor1.1)/2
# inp3$avgneighbor2 = (inp3$neighbor2+inp3$neighbor2.1)/2
# inp3$avgneighbor3 = (inp3$neighbor3+inp3$neighbor3.1)/2
# inp3$avgneighbor4 = (inp3$neighbor4+inp3$neighbor4.1)/2
# inp3$avgneighbor5 = (inp3$neighbor5+inp3$neighbor5.1)/2
# inp3$avgneighbor6 = (inp3$neighbor6+inp3$neighbor6.1)/2
# inp3$avgtriangle = (inp3$triangle+inp3$triangle.1)/2
# inp3$avgcommon_neighbor = (inp3$common_neighbor+inp3$common_neighbor.1)/2
# inp3cp = inp3[, c(1:2,31,32,37:52)]
# inp4 = na.omit(inp3cp)
# inp4cp = inp4
# inp4cp.same = inp4cp[which(inp4cp$leiden == inp4cp$leiden.1),]
# inp4cp.same$comm.detect = 1
# inp4cp.diff = inp4cp[which(inp4cp$leiden != inp4cp$leiden.1),]
# inp4cp.diff$comm.detect = 0
# inp4 = rbind(inp4cp.same, inp4cp.diff)
# inp4 = inp4[,c(1,2,5:21)]
# dim(inp4)
# colnames(inp4)[1] = "gene1"
# colnames(inp4)[2] = "gene2"
# inp4$gene1 = as.factor(inp4$gene1)
# inp4$gene2 = as.factor(inp4$gene2)
# for (i in 1:length(inp4$gene1)){
#   V = inp4$gene1[i]
#   U = inp4$gene2[i]
#   inp4$shortest_path[i] <- distances(Network2, v = V, to = U)
# }
# #inp4 = inp4[, -18]
# inp4$cohesion = 0
# for (i in 1:length(inp4$gene1)){
#   if (inp4$shortest_path[i] > 1) {
#     V = inp4$gene1[i]
#     U = inp4$gene2[i]
#     print(i)
#     inp4$cohesion[i] <- vertex.connectivity(Network2, source = V,target = U)
#   }
# }
# inp4[inp4$shortest_path<=1,]$cohesion=NA
# #inp4 = inp4[, -19]
# inp4 = na.omit(inp4)
# inp4$adhesion = 0
# for (i in 1:length(inp4$gene1)){
#   V = inp4$gene1[i]
#   U = inp4$gene2[i]
#   print(i)
#   inp4$adhesion[i] <- edge.connectivity(Network2, source = V, target = U)
# }
# inp4 = na.omit(inp4)
# inp4$shortest_path <- str_replace(inp4$shortest_path, "Inf", "26")
# inp4$shortest_path = as.integer(inp4$shortest_path)
# print(dim(inp2))
# print(dim(inp4))
# write.csv(inp4, "sldb-sl-pairwise-new-netprop", row.names = F, quote = F)
# ##cgidb sv
# sv.net = read.csv("cgidb-sv-nodewise.csv")
# inp2 = sv.net
# inp3 = inp2[, c(1,19,2,20,3,21,4,22,5,23,6,24,7,25,8,26,9,27,10,28,11,29,12,30,13,31,14,32,15,33,16,34,17,35,18,36)]
# inp3$degree = as.integer(inp3$degree)
# inp3$degree.1 = as.integer(inp3$degree.1)
# inp3$betwn = as.integer(inp3$betwn)
# inp3$betwn.1 = as.integer(inp3$betwn.1)
# inp3$close = as.integer(inp3$close)
# inp3$close.1 = as.integer(inp3$close.1)
# inp3$core = as.integer(inp3$core)
# inp3$core.1 = as.integer(inp3$core.1)
# inp3$constraint = as.integer(inp3$constraint)
# inp3$constraint.1 = as.integer(inp3$constraint.1)
# inp3$ecc = as.integer(inp3$ecc)
# inp3$ecc.1 = as.integer(inp3$ecc.1)
# inp3$eigen_cen = as.integer(inp3$eigen_cen)
# inp3$eigen_cen.1 = as.integer(inp3$eigen_cen.1)
# inp3$hub_score = as.integer(inp3$hub_score)
# inp3$hub_score.1 = as.integer(inp3$hub_score.1)
# inp3$neighbor1 = as.integer(inp3$neighbor1)
# inp3$neighbor1.1 = as.integer(inp3$neighbor1.1)
# inp3$neighbor2 = as.integer(inp3$neighbor2)
# inp3$neighbor2.1 = as.integer(inp3$neighbor2.1)
# inp3$neighbor3 = as.integer(inp3$neighbor3)
# inp3$neighbor3.1 = as.integer(inp3$neighbor3.1)
# inp3$neighbor4 = as.integer(inp3$neighbor4)
# inp3$neighbor4.1 = as.integer(inp3$neighbor4.1)
# inp3$neighbor5 = as.integer(inp3$neighbor5)
# inp3$neighbor5.1 = as.integer(inp3$neighbor5.1)
# inp3$neighbor6 = as.integer(inp3$neighbor6)
# inp3$neighbor6.1 = as.integer(inp3$neighbor6.1)
# inp3$triangle = as.integer(inp3$triangle)
# inp3$triangle.1 = as.integer(inp3$triangle.1)
# inp3$common_neighbor = as.integer(inp3$common_neighbor)
# inp3$common_neighbor.1 = as.integer(inp3$common_neighbor.1)
# #calculating avg for all properties
# inp3$avgdeg = (inp3$degree+inp3$degree.1)/2
# inp3$avgbet = (inp3$betwn+inp3$betwn.1)/2
# inp3$avgclose = (inp3$close+inp3$close.1)/2
# inp3$avgcore = (inp3$core+inp3$core.1)/2
# inp3$avgcons = (inp3$constraint+inp3$constraint.1)/2
# inp3$avgecc = (inp3$ecc+inp3$ecc.1)/2
# inp3$avgeigen_cen = (inp3$eigen_cen+inp3$eigen_cen.1)/2
# inp3$avghub_score = (inp3$hub_score+inp3$hub_score.1)/2
# inp3$avgneighbor1 = (inp3$neighbor1+inp3$neighbor1.1)/2
# inp3$avgneighbor2 = (inp3$neighbor2+inp3$neighbor2.1)/2
# inp3$avgneighbor3 = (inp3$neighbor3+inp3$neighbor3.1)/2
# inp3$avgneighbor4 = (inp3$neighbor4+inp3$neighbor4.1)/2
# inp3$avgneighbor5 = (inp3$neighbor5+inp3$neighbor5.1)/2
# inp3$avgneighbor6 = (inp3$neighbor6+inp3$neighbor6.1)/2
# inp3$avgtriangle = (inp3$triangle+inp3$triangle.1)/2
# inp3$avgcommon_neighbor = (inp3$common_neighbor+inp3$common_neighbor.1)/2
# inp3cp = inp3[, c(1:2,31,32,37:52)]
# inp4 = na.omit(inp3cp)
# inp4cp = inp4
# inp4cp.same = inp4cp[which(inp4cp$leiden == inp4cp$leiden.1),]
# inp4cp.same$comm.detect = 1
# inp4cp.diff = inp4cp[which(inp4cp$leiden != inp4cp$leiden.1),]
# inp4cp.diff$comm.detect = 0
# inp4 = rbind(inp4cp.same, inp4cp.diff)
# inp4 = inp4[,c(1,2,5:21)]
# dim(inp4)
# colnames(inp4)[1] = "gene1"
# colnames(inp4)[2] = "gene2"
# inp4$gene1 = as.factor(inp4$gene1)
# inp4$gene2 = as.factor(inp4$gene2)
# for (i in 1:length(inp4$gene1)){
#   V = inp4$gene1[i]
#   U = inp4$gene2[i]
#   inp4$shortest_path[i] <- distances(Network2, v = V, to = U)
# }
# #inp4 = inp4[, -18]
# inp4$cohesion = 0
# for (i in 1:length(inp4$gene1)){
#   if (inp4$shortest_path[i] > 1) {
#     V = inp4$gene1[i]
#     U = inp4$gene2[i]
#     #print(i)
#     inp4$cohesion[i] <- vertex.connectivity(Network2, source = V,target = U)
#   }
# }
# inp4[inp4$shortest_path<=1,]$cohesion=NA
# #inp4 = inp4[, -19]
# inp4 = na.omit(inp4)
# inp4$adhesion = 0
# for (i in 1:length(inp4$gene1)){
#   V = inp4$gene1[i]
#   U = inp4$gene2[i]
#   #print(i)
#   inp4$adhesion[i] <- edge.connectivity(Network2, source = V, target = U)
# }
# inp4 = na.omit(inp4)
# inp4$shortest_path <- str_replace(inp4$shortest_path, "Inf", "26")
# inp4$shortest_path = as.integer(inp4$shortest_path)
# print(dim(inp2))
# print(dim(inp4))
# write.csv(inp4, "cgidb-sv-pairwise-new-netprop", row.names = F, quote = F)
# #######sldb sv
# sv.net = read.csv("sldb-sv-nodewise.csv")
# inp2 = sv.net
# inp3 = inp2[, c(1,19,2,20,3,21,4,22,5,23,6,24,7,25,8,26,9,27,10,28,11,29,12,30,13,31,14,32,15,33,16,34,17,35,18,36)]
# inp3$degree = as.integer(inp3$degree)
# inp3$degree.1 = as.integer(inp3$degree.1)
# inp3$betwn = as.integer(inp3$betwn)
# inp3$betwn.1 = as.integer(inp3$betwn.1)
# inp3$close = as.integer(inp3$close)
# inp3$close.1 = as.integer(inp3$close.1)
# inp3$core = as.integer(inp3$core)
# inp3$core.1 = as.integer(inp3$core.1)
# inp3$constraint = as.integer(inp3$constraint)
# inp3$constraint.1 = as.integer(inp3$constraint.1)
# inp3$ecc = as.integer(inp3$ecc)
# inp3$ecc.1 = as.integer(inp3$ecc.1)
# inp3$eigen_cen = as.integer(inp3$eigen_cen)
# inp3$eigen_cen.1 = as.integer(inp3$eigen_cen.1)
# inp3$hub_score = as.integer(inp3$hub_score)
# inp3$hub_score.1 = as.integer(inp3$hub_score.1)
# inp3$neighbor1 = as.integer(inp3$neighbor1)
# inp3$neighbor1.1 = as.integer(inp3$neighbor1.1)
# inp3$neighbor2 = as.integer(inp3$neighbor2)
# inp3$neighbor2.1 = as.integer(inp3$neighbor2.1)
# inp3$neighbor3 = as.integer(inp3$neighbor3)
# inp3$neighbor3.1 = as.integer(inp3$neighbor3.1)
# inp3$neighbor4 = as.integer(inp3$neighbor4)
# inp3$neighbor4.1 = as.integer(inp3$neighbor4.1)
# inp3$neighbor5 = as.integer(inp3$neighbor5)
# inp3$neighbor5.1 = as.integer(inp3$neighbor5.1)
# inp3$neighbor6 = as.integer(inp3$neighbor6)
# inp3$neighbor6.1 = as.integer(inp3$neighbor6.1)
# inp3$triangle = as.integer(inp3$triangle)
# inp3$triangle.1 = as.integer(inp3$triangle.1)
# inp3$common_neighbor = as.integer(inp3$common_neighbor)
# inp3$common_neighbor.1 = as.integer(inp3$common_neighbor.1)
# #calculating avg for all properties
# inp3$avgdeg = (inp3$degree+inp3$degree.1)/2
# inp3$avgbet = (inp3$betwn+inp3$betwn.1)/2
# inp3$avgclose = (inp3$close+inp3$close.1)/2
# inp3$avgcore = (inp3$core+inp3$core.1)/2
# inp3$avgcons = (inp3$constraint+inp3$constraint.1)/2
# inp3$avgecc = (inp3$ecc+inp3$ecc.1)/2
# inp3$avgeigen_cen = (inp3$eigen_cen+inp3$eigen_cen.1)/2
# inp3$avghub_score = (inp3$hub_score+inp3$hub_score.1)/2
# inp3$avgneighbor1 = (inp3$neighbor1+inp3$neighbor1.1)/2
# inp3$avgneighbor2 = (inp3$neighbor2+inp3$neighbor2.1)/2
# inp3$avgneighbor3 = (inp3$neighbor3+inp3$neighbor3.1)/2
# inp3$avgneighbor4 = (inp3$neighbor4+inp3$neighbor4.1)/2
# inp3$avgneighbor5 = (inp3$neighbor5+inp3$neighbor5.1)/2
# inp3$avgneighbor6 = (inp3$neighbor6+inp3$neighbor6.1)/2
# inp3$avgtriangle = (inp3$triangle+inp3$triangle.1)/2
# inp3$avgcommon_neighbor = (inp3$common_neighbor+inp3$common_neighbor.1)/2
# inp3cp = inp3[, c(1:2,31,32,37:52)]
# inp4 = na.omit(inp3cp)
# inp4cp = inp4
# inp4cp.same = inp4cp[which(inp4cp$leiden == inp4cp$leiden.1),]
# inp4cp.same$comm.detect = 1
# inp4cp.diff = inp4cp[which(inp4cp$leiden != inp4cp$leiden.1),]
# inp4cp.diff$comm.detect = 0
# inp4 = rbind(inp4cp.same, inp4cp.diff)
# inp4 = inp4[,c(1,2,5:21)]
# dim(inp4)
# colnames(inp4)[1] = "gene1"
# colnames(inp4)[2] = "gene2"
# inp4$gene1 = as.factor(inp4$gene1)
# inp4$gene2 = as.factor(inp4$gene2)
# for (i in 1:length(inp4$gene1)){
#   V = inp4$gene1[i]
#   U = inp4$gene2[i]
#   inp4$shortest_path[i] <- distances(Network2, v = V, to = U)
# }
# #inp4 = inp4[, -18]
# inp4$cohesion = 0
# for (i in 1:length(inp4$gene1)){
#   if (inp4$shortest_path[i] > 1) {
#     V = inp4$gene1[i]
#     U = inp4$gene2[i]
#     #print(i)
#     inp4$cohesion[i] <- vertex.connectivity(Network2, source = V,target = U)
#   }
# }
# inp4[inp4$shortest_path<=1,]$cohesion=NA
# #inp4 = inp4[, -19]
# inp4 = na.omit(inp4)
# inp4$adhesion = 0
# for (i in 1:length(inp4$gene1)){
#   V = inp4$gene1[i]
#   U = inp4$gene2[i]
#   #print(i)
#   inp4$adhesion[i] <- edge.connectivity(Network2, source = V, target = U)
# }
# inp4 = na.omit(inp4)
# inp4$shortest_path <- str_replace(inp4$shortest_path, "Inf", "26")
# inp4$shortest_path = as.integer(inp4$shortest_path)
# print(dim(inp2))
# print(dim(inp4))
# write.csv(inp4, "sldb-sv-pairwise-new-netprop", row.names = F, quote = F)
###not0
library(dplyr)
library(stringr)
# inp2 = read.csv("not0-nodewise.csv")
# inp3 = inp2[, c(1,19,2,20,3,21,4,22,5,23,6,24,7,25,8,26,9,27,10,28,11,29,12,30,13,31,14,32,15,33,16,34,17,35,18,36)]
# inp3$degree = as.integer(inp3$degree)
# inp3$degree.1 = as.integer(inp3$degree.1)
# inp3$betwn = as.integer(inp3$betwn)
# inp3$betwn.1 = as.integer(inp3$betwn.1)
# inp3$close = as.integer(inp3$close)
# inp3$close.1 = as.integer(inp3$close.1)
# inp3$core = as.integer(inp3$core)
# inp3$core.1 = as.integer(inp3$core.1)
# inp3$constraint = as.integer(inp3$constraint)
# inp3$constraint.1 = as.integer(inp3$constraint.1)
# inp3$ecc = as.integer(inp3$ecc)
# inp3$ecc.1 = as.integer(inp3$ecc.1)
# inp3$eigen_cen = as.integer(inp3$eigen_cen)
# inp3$eigen_cen.1 = as.integer(inp3$eigen_cen.1)
# inp3$hub_score = as.integer(inp3$hub_score)
# inp3$hub_score.1 = as.integer(inp3$hub_score.1)
# inp3$neighbor1 = as.integer(inp3$neighbor1)
# inp3$neighbor1.1 = as.integer(inp3$neighbor1.1)
# inp3$neighbor2 = as.integer(inp3$neighbor2)
# inp3$neighbor2.1 = as.integer(inp3$neighbor2.1)
# inp3$neighbor3 = as.integer(inp3$neighbor3)
# inp3$neighbor3.1 = as.integer(inp3$neighbor3.1)
# inp3$neighbor4 = as.integer(inp3$neighbor4)
# inp3$neighbor4.1 = as.integer(inp3$neighbor4.1)
# inp3$neighbor5 = as.integer(inp3$neighbor5)
# inp3$neighbor5.1 = as.integer(inp3$neighbor5.1)
# inp3$neighbor6 = as.integer(inp3$neighbor6)
# inp3$neighbor6.1 = as.integer(inp3$neighbor6.1)
# inp3$triangle = as.integer(inp3$triangle)
# inp3$triangle.1 = as.integer(inp3$triangle.1)
# inp3$common_neighbor = as.integer(inp3$common_neighbor)
# inp3$common_neighbor.1 = as.integer(inp3$common_neighbor.1)
# #calculating avg for all properties
# inp3$avgdeg = (inp3$degree+inp3$degree.1)/2
# inp3$avgbet = (inp3$betwn+inp3$betwn.1)/2
# inp3$avgclose = (inp3$close+inp3$close.1)/2
# inp3$avgcore = (inp3$core+inp3$core.1)/2
# inp3$avgcons = (inp3$constraint+inp3$constraint.1)/2
# inp3$avgecc = (inp3$ecc+inp3$ecc.1)/2
# inp3$avgeigen_cen = (inp3$eigen_cen+inp3$eigen_cen.1)/2
# inp3$avghub_score = (inp3$hub_score+inp3$hub_score.1)/2
# inp3$avgneighbor1 = (inp3$neighbor1+inp3$neighbor1.1)/2
# inp3$avgneighbor2 = (inp3$neighbor2+inp3$neighbor2.1)/2
# inp3$avgneighbor3 = (inp3$neighbor3+inp3$neighbor3.1)/2
# inp3$avgneighbor4 = (inp3$neighbor4+inp3$neighbor4.1)/2
# inp3$avgneighbor5 = (inp3$neighbor5+inp3$neighbor5.1)/2
# inp3$avgneighbor6 = (inp3$neighbor6+inp3$neighbor6.1)/2
# inp3$avgtriangle = (inp3$triangle+inp3$triangle.1)/2
# inp3$avgcommon_neighbor = (inp3$common_neighbor+inp3$common_neighbor.1)/2
# inp3cp = inp3[, c(1:2,31,32,37:52)]
# inp4 = na.omit(inp3cp)
# inp4cp = inp4
# inp4cp.same = inp4cp[which(inp4cp$leiden == inp4cp$leiden.1),]
# inp4cp.same$comm.detect = 1
# inp4cp.diff = inp4cp[which(inp4cp$leiden != inp4cp$leiden.1),]
# inp4cp.diff$comm.detect = 0
# inp4 = rbind(inp4cp.same, inp4cp.diff)
# inp4 = inp4[,c(1,2,5:21)]
# dim(inp4)
# colnames(inp4)[1] = "gene1"
# colnames(inp4)[2] = "gene2"
# inp4$gene1 = as.factor(inp4$gene1)
# inp4$gene2 = as.factor(inp4$gene2)
# for (i in 1:length(inp4$gene1)){
#   V = inp4$gene1[i]
#   U = inp4$gene2[i]
#   inp4$shortest_path[i] <- distances(Network2, v = V, to = U)
# }
# #inp4 = inp4[, -18]
# inp4$cohesion = 0
# for (i in 1:length(inp4$gene1)){
#   if (inp4$shortest_path[i] > 1) {
#     V = inp4$gene1[i]
#     U = inp4$gene2[i]
#     print(i)
#     inp4$cohesion[i] <- vertex.connectivity(Network2, source = V,target = U)
#   }
# }
# inp4[inp4$shortest_path<=1,]$cohesion=NA
# #inp4 = inp4[, -19]
# inp4 = na.omit(inp4)
# inp4$adhesion = 0
# for (i in 1:length(inp4$gene1)){
#   V = inp4$gene1[i]
#   U = inp4$gene2[i]
#   print(i)
#   inp4$adhesion[i] <- edge.connectivity(Network2, source = V, target = U)
# }
# inp4 = na.omit(inp4)
# inp4$shortest_path <- str_replace(inp4$shortest_path, "Inf", "26")
# inp4$shortest_path = as.integer(inp4$shortest_path)
# print(dim(inp2))
# print(dim(inp4))
# write.csv(inp4, "not0.pairwiseprop.csv", row.names = F, quote = F)
# ###not1
# inp2 = read.csv("not1-nodewise.csv")
# inp3 = inp2[, c(1,19,2,20,3,21,4,22,5,23,6,24,7,25,8,26,9,27,10,28,11,29,12,30,13,31,14,32,15,33,16,34,17,35,18,36)]
# inp3$degree = as.integer(inp3$degree)
# inp3$degree.1 = as.integer(inp3$degree.1)
# inp3$betwn = as.integer(inp3$betwn)
# inp3$betwn.1 = as.integer(inp3$betwn.1)
# inp3$close = as.integer(inp3$close)
# inp3$close.1 = as.integer(inp3$close.1)
# inp3$core = as.integer(inp3$core)
# inp3$core.1 = as.integer(inp3$core.1)
# inp3$constraint = as.integer(inp3$constraint)
# inp3$constraint.1 = as.integer(inp3$constraint.1)
# inp3$ecc = as.integer(inp3$ecc)
# inp3$ecc.1 = as.integer(inp3$ecc.1)
# inp3$eigen_cen = as.integer(inp3$eigen_cen)
# inp3$eigen_cen.1 = as.integer(inp3$eigen_cen.1)
# inp3$hub_score = as.integer(inp3$hub_score)
# inp3$hub_score.1 = as.integer(inp3$hub_score.1)
# inp3$neighbor1 = as.integer(inp3$neighbor1)
# inp3$neighbor1.1 = as.integer(inp3$neighbor1.1)
# inp3$neighbor2 = as.integer(inp3$neighbor2)
# inp3$neighbor2.1 = as.integer(inp3$neighbor2.1)
# inp3$neighbor3 = as.integer(inp3$neighbor3)
# inp3$neighbor3.1 = as.integer(inp3$neighbor3.1)
# inp3$neighbor4 = as.integer(inp3$neighbor4)
# inp3$neighbor4.1 = as.integer(inp3$neighbor4.1)
# inp3$neighbor5 = as.integer(inp3$neighbor5)
# inp3$neighbor5.1 = as.integer(inp3$neighbor5.1)
# inp3$neighbor6 = as.integer(inp3$neighbor6)
# inp3$neighbor6.1 = as.integer(inp3$neighbor6.1)
# inp3$triangle = as.integer(inp3$triangle)
# inp3$triangle.1 = as.integer(inp3$triangle.1)
# inp3$common_neighbor = as.integer(inp3$common_neighbor)
# inp3$common_neighbor.1 = as.integer(inp3$common_neighbor.1)
# #calculating avg for all properties
# inp3$avgdeg = (inp3$degree+inp3$degree.1)/2
# inp3$avgbet = (inp3$betwn+inp3$betwn.1)/2
# inp3$avgclose = (inp3$close+inp3$close.1)/2
# inp3$avgcore = (inp3$core+inp3$core.1)/2
# inp3$avgcons = (inp3$constraint+inp3$constraint.1)/2
# inp3$avgecc = (inp3$ecc+inp3$ecc.1)/2
# inp3$avgeigen_cen = (inp3$eigen_cen+inp3$eigen_cen.1)/2
# inp3$avghub_score = (inp3$hub_score+inp3$hub_score.1)/2
# inp3$avgneighbor1 = (inp3$neighbor1+inp3$neighbor1.1)/2
# inp3$avgneighbor2 = (inp3$neighbor2+inp3$neighbor2.1)/2
# inp3$avgneighbor3 = (inp3$neighbor3+inp3$neighbor3.1)/2
# inp3$avgneighbor4 = (inp3$neighbor4+inp3$neighbor4.1)/2
# inp3$avgneighbor5 = (inp3$neighbor5+inp3$neighbor5.1)/2
# inp3$avgneighbor6 = (inp3$neighbor6+inp3$neighbor6.1)/2
# inp3$avgtriangle = (inp3$triangle+inp3$triangle.1)/2
# inp3$avgcommon_neighbor = (inp3$common_neighbor+inp3$common_neighbor.1)/2
# inp3cp = inp3[, c(1:2,31,32,37:52)]
# inp4 = na.omit(inp3cp)
# inp4cp = inp4
# inp4cp.same = inp4cp[which(inp4cp$leiden == inp4cp$leiden.1),]
# inp4cp.same$comm.detect = 1
# inp4cp.diff = inp4cp[which(inp4cp$leiden != inp4cp$leiden.1),]
# inp4cp.diff$comm.detect = 0
# inp4 = rbind(inp4cp.same, inp4cp.diff)
# inp4 = inp4[,c(1,2,5:21)]
# dim(inp4)
# colnames(inp4)[1] = "gene1"
# colnames(inp4)[2] = "gene2"
# inp4$gene1 = as.factor(inp4$gene1)
# inp4$gene2 = as.factor(inp4$gene2)
# for (i in 1:length(inp4$gene1)){
#   V = inp4$gene1[i]
#   U = inp4$gene2[i]
#   inp4$shortest_path[i] <- distances(Network2, v = V, to = U)
# }
# #inp4 = inp4[, -18]
# inp4$cohesion = 0
# for (i in 1:length(inp4$gene1)){
#   if (inp4$shortest_path[i] > 1) {
#     V = inp4$gene1[i]
#     U = inp4$gene2[i]
#     print(i)
#     inp4$cohesion[i] <- vertex.connectivity(Network2, source = V,target = U)
#   }
# }
# inp4[inp4$shortest_path<=1,]$cohesion=NA
# #inp4 = inp4[, -19]
# inp4 = na.omit(inp4)
# inp4$adhesion = 0
# for (i in 1:length(inp4$gene1)){
#   V = inp4$gene1[i]
#   U = inp4$gene2[i]
#   print(i)
#   inp4$adhesion[i] <- edge.connectivity(Network2, source = V, target = U)
# }
# inp4 = na.omit(inp4)
# inp4$shortest_path <- str_replace(inp4$shortest_path, "Inf", "26")
# inp4$shortest_path = as.integer(inp4$shortest_path)
# print(dim(inp2))
# print(dim(inp4))
# write.csv(inp4, "not1.pairwiseprop.csv", row.names = F, quote = F)
#############################MODEL-BUILDING#######################################
###MAGICAL-CORE
sl = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/biogrid-sl-pairwise-new-netprop")
sl$gi = "SL"
sv = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/biogrid-sv-pairwise-new-netprop")
sv$gi = "SV"
sl2 = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/cgidb-sl-pairwise-new-netprop")
sl2$gi = "SL"
sv2 = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/cgidb-sv-pairwise-new-netprop")
sv2$gi = "SV"
sl3 = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/sldb-sl-pairwise-new-netprop")
sl3$gi = "SL"
sv3 = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/sldb-sv-pairwise-new-netprop")
sv3$gi = "SV"
not = read.csv("/home/nikola/biogrid-new-ppi/new-entrez/not0.pairwiseprop.csv")
not$gi = "NOT"
traindata = unique(rbind(sl,sl2,sl3,sv,sv2,sv3,not))
table(traindata$gi)
traindata$gi = as.factor(traindata$gi)
traindatabal = DMwR::SMOTE(gi ~ ., traindata, perc.under = 200)
traindatabal$gene1 = as.integer(traindatabal$gene1)
traindatabal$gene2 = as.integer(traindatabal$gene2)
write.csv(traindatabal, "magical-core-data.csv", row.names = F, quote = F)
#training = traindatabal
traindatabal = read.csv("/home/nikola/biogrid-new-ppi/magical-imp-prop-new-entrez/figures/magical-core-data.csv")
#####OR####
traindatabal = read.csv("/home/nikola/biogrid-new-ppi/magical-imp-prop-new-entrez/magical-bal-data-with-geneids.csv")
traindata$gi = as.factor(traindata$gi)
traindatabal$gene1 = as.integer(traindatabal$gene1)
traindatabal$gene2 = as.integer(traindatabal$gene2)
index = sample(2, nrow(traindatabal), replace = TRUE, prob = c(0.70, 0.30))
train = traindatabal[index == 1,]
test = traindatabal[index == 2,]
train.magical = train[,c(2,10,15,18,20,21)]
test.magical = test[,c(2,10,15,18,20,21)]
training = train.magical
testing = test.magical
library(randomForest)
magical = randomForest(gi ~ . , data = training, importance = T)
prediction = predict(magical, newdata = testing[-6])
predict_gi = predict(magical, newdata = testing)
testing$predict_gi = predict_gi
cnf_mat = table(testing$gi, testing$predict_gi)
cnf_mat
accuracy = sum(diag(cnf_mat)/sum(cnf_mat))
saveRDS(magical, "magical-core.rds")
##Similar to the MAGICAL-CORE network properties for MAGICAL-COMBINED was calculated
###MAGICAL-COMBINED
sl = read.csv("/home/nikola/string-full-nocuff/biogrid-sl-pairwise-new-netprop")
sl$gi = "SL"
sv = read.csv("/home/nikola/string-full-nocuff/biogrid-sv-pairwise-new-netprop")
sv$gi = "SV"
sl2 = read.csv("/home/nikola/string-full-nocuff/cgidb-sl-pairwise-new-netprop")
sl2$gi = "SL"
sv2 = read.csv("/home/nikola/string-full-nocuff/cgidb-sv-pairwise-new-netprop")
sv2$gi = "SV"
sl3 = read.csv("/home/nikola/string-full-nocuff/sldb-sl-pairwise-new-netprop")
sl3$gi = "SL"
sv3 = read.csv("/home/nikola/string-full-nocuff/sldb-sv-pairwise-new-netprop")
sv3$gi = "SV"
not = read.csv("/home/nikola/string-full-nocuff/not-ensp-5kof26k-pairwise-new-netprop")
not$gi = "NOT"
traindata = unique(rbind(sl,sl2,sl3,sv,sv2,sv3,not))
table(traindata$gi)
traindata$gi = as.factor(traindata$gi)
traindata$gene1 = as.factor(traindata$gene1)
traindata$gene2 = as.factor(traindata$gene2)
traindatabal = DMwR::SMOTE(gi ~ ., traindata, perc.under = 200)
#traindatabal = DMwR::SMOTE(gi ~ ., traindata[,-c(1,2)], perc.under = 200)
table(traindatabal$gi)
write.csv(traindatabal, "magical-combined-data.csv", row.names = F, quote = F)
#training = traindatabal
traindatabal = read.csv("/home/nikola/biogrid-new-ppi/magical-imp-prop-new-entrez/figures/magical-combined-data.csv")
#traindatabal$gene1 = as.factor(traindatabal$gene1)
#traindatabal$gene2 = as.factor(traindatabal$gene2)
traindatabal$gi = as.factor(traindatabal$gi)
traindatabal = traindatabal[,-c(1,2)]
index = sample(2, nrow(traindatabal), replace = TRUE, prob = c(0.70, 0.30))
train = traindatabal[index == 1,]
test = traindatabal[index == 2,]
training = train[,c(2,10,15,18,20,21)]
testing = test[,c(2,10,15,18,20,21)]
set.seed(123)
model.pred = randomForest(gi ~ . , data = training, importance = T)
prediction = predict(model.pred, newdata = testing[-6])
predict_gi = predict(model.pred, newdata = testing)
testing$predict_gi = predict_gi
cnf_mat = table(testing$gi, testing$predict_gi)
cnf_mat
accuracy = sum(diag(cnf_mat)/sum(cnf_mat))
saveRDS(model.pred, "magical-combined.rds")
