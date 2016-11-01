## tasks on unempstates.csv

dataset = load.data.unempstates()

dim(dataset)
# as a result for this case
# the No. of rows, the No. of instances, is 50
# the No. of columns, the No. of features, is 416
# scale function works by columns
dataset = scale(dataset) ## normalize the data
labels = rownames(dataset)
labels

## section 1 and section 2
data.pc = do.pca(dataset, labels, 
                 do.screeplot = T, 
                 do.scatter = T, 
                 do.loadingplot = T)

## section 3
data.mds = do.mds(dataset, labels, do.scatter = T)

## section 4
kcount = 4
## kcount = 8

## k-means clustering
clu = do.kmeans(dataset, labels, k = kcount)$cluster

## plot clustering results from kmeans
plot(data.mds, type = "n", main = 'MDS maps for the states')
text(data.mds, labels, col = rainbow(kcount)[clu])

## hierarchical clustering
## distance method: h-clustering with single-link
clu2 = do.hclust(dataset, labels, dist.mtd='single', k=kcount, do.dendrogram = T)

## plot clustering results from hclust
plot(data.mds, type = "n", main = 'MDS maps for the states')
text(data.mds, labels, col = rainbow(kcount)[clu2])

## distance method: h-clustering with complete-link
clu2 = do.hclust(dataset, labels, dist.mtd='complete', k=kcount, do.dendrogram = T)

## plot clustering results from hclust
plot(data.mds, type = "n", main = 'MDS maps for the states')
text(data.mds, labels, col = rainbow(kcount)[clu2])

## distance method: h-clustering with average-link
clu2 = do.hclust(dataset, labels, dist.mtd='average', k=kcount, do.dendrogram = T)

## plot clustering results from hclust
plot(data.mds, type = "n", main = 'MDS maps for the states')
text(data.mds, labels, col = rainbow(kcount)[clu2])

## section 5
wssplot(dataset, nc=10)
# clu = kmeans(dataset, centers=4, nstart=10)
# clu
# hc = hclust(dist(dataset), method='complete')
# hc

print(cluster.purity(clu2, clu))
print(cluster.entropy(clu2, clu))


## tasks on book-info.csv and book-copurchase.csv
dataset = load.data.books()

## section 1,2,3,4
data.mds <- books.mds(dataset, 
                       do.scatter = T, 
                       do.scatter.cluster = T, 
                       do.clust='kmeans')
data.mds <- books.mds(dataset, 
                       do.scatter = F, 
                       do.scatter.cluster = T, 
                       do.clust='hclust', 
                       dist.mtd='single')
data.mds <- books.mds(dataset, 
                       do.scatter = F, 
                       do.scatter.cluster = T, 
                       do.clust='hclust', 
                       dist.mtd='complete')
data.mds <- books.mds(dataset, 
                       do.scatter = F, 
                       do.scatter.cluster = T, 
                       do.clust='hclust', 
                       dist.mtd='average')

conf <- table(data.mds$Class, data.mds$clu)
conf
error <- (conf[1,1]+conf[2,2])/(sum(conf[1,])+sum(conf[2,]))
error

data.mds$name[data.mds$Class=="liberal" & data.mds$clu==2]
data.mds$name[data.mds$Class=="conservative" & data.mds$clu==1]
