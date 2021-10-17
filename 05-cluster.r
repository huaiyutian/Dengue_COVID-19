library(pvclust)
npi <- read.csv("00.data/02.PHSM_HMB.csv",row.names=1)

pdf("02.fig/Fig_S14/Fig_S13_cluster.pdf")
# The hierarchical clustering analysis of Bootstrap (the number of Bootstrap is 10000) was carried out. Ward method
# The dissimilarity matrix based on correlation were adopted as follows:
result <- pvclust(npi, method.dist="euclidean", method.hclust="ward.D2", nboot=10000, parallel=TRUE)
plot(result)
pvrect(result, alpha=0.95)

dev.off()

# For the cluster with Au P value > 0.95, the hypothesis of "non-existence of clustering" was rejected at the significance level of 0.05.
# Roughly speaking, we can think of these highlighted clusters as not just "appearing to exist" due to sampling errors
# And if we increase the number of observations, we can see them steadily.
seplot(result, identify=TRUE)
msplot(result, edges=x)
