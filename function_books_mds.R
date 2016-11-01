library('ggplot2')
library(plyr) # for recoding data

theme_set( theme_bw( base_family="Helvetica")) # the font changign doesn't work for Mac
theme_update(plot.title = element_text( size=11,vjust=1,face='bold'),
             axis.title.x = element_text( size=12),
             axis.title.y = element_text( size=12,angle=90 ),
             axis.text.x = element_text( size=10),
             axis.text.y = element_text( size=10,hjust=1 ))

books.mds <- function(dataset,
                      do.scatter=F,
                      do.scatter.cluster=F,
                      do.clust='kmeans',
                      dist.mtd='single') {
  get.dist <- function(m) {
    dist(m %*% t(m))
  }
  
  data1 = distance.matrix(dataset)
  ## use either kmeans or hclust
  if (do.clust=='kmeans') { clu = do.kmeans(data1,NULL,k=2)$cluster }
  else if (do.clust=='hclust') { 
    if (dist.mtd=='single') {
      clu = do.hclust(data1,NULL,dist.mtd='single',k=2)
    }
    else if (dist.mtd=='complete') {
      clu = do.hclust(data1,NULL,dist.mtd='complete',k=2)
    }
    else if (dist.mtd=='average') {
      clu = do.hclust(data1,NULL,dist.mtd='average',k=2)
    }
    else {}
  }
  else {}
  
  print(dim(data1))
  print(head(data1[,1:12]))  
  data.dist = get.dist(data1)
  # write distance matrix to a file
  write.csv(as.matrix(data.dist), "distance_matrix.csv")
  lbls = dataset$name
  Class = mapvalues(dataset$Class,from=c(1, 3),to=c("liberal", "conservative") )
  data.mds = cmdscale(data.dist)
  
  if (do.scatter) {
    # plot based on variable "Class"
    tmp_data = data.frame(x=data.mds[,1],y=data.mds[,2],name=lbls,Class=Class)
    p = ggplot(aes(x=x,y=y, shape=Class, color=Class), data=tmp_data) +
      geom_point(size=4,alpha=0.5) +
      geom_text(aes(x=x,y=y, shape=Class, color=Class, label=name), size=3)
    print(p)
    # plot(data.mds, type = 'n')
    # text(data.mds, labels=lbls)       
  }
  
  data2 = data.frame(x=data.mds[,1],y=data.mds[,2],name=lbls,Class=Class,clu=factor(clu))
  
  if (do.scatter.cluster) {
    # plot based on variable "Class", and compare with clusters
    p = ggplot(aes(x=x,y=y,shape=Class,color=clu), data=data2) +
      geom_point(size=4,alpha=0.5) +
      geom_text(aes(x=x,y=y,shape=Class,color=clu,label=name), size=3)
    print(p)
  }
  
  print(cluster.purity(clu,Class))
  print(cluster.entropy(clu,Class))
  
  # data.mds
  data2
}