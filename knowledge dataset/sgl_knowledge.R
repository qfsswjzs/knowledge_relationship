# Title     : sgl_knowledge.R
# Author    : wujie
# Time      : 2021/4/15


library(spectralGraphTopology)
library(igraph)
library(pals)
library(viridis)
library(latex2exp)
library(huge)

setwd("/Users/wujie/Documents/Projects/Pycharm/graduation_project/knowledge dataset")

sgl_knowledge<- function(train_data, k = 3) {
  #df <- read.csv("Train.csv", header = FALSE)
  Nnodes = ncol(train_data)
  #names <- matrix(unlist(read.csv("catalog_number.csv", encoding="UTF-8")[, 1]))
  df_names <- read.csv("catalog.csv", header = FALSE, nrows = Nnodes)
  names <- t(matrix(unlist(df_names), nrow = nrow(df_names)))
  names <- names[2, 1:Nnodes]
  # Y <- matrix(as.numeric(unlist(df)), nrow = nrow(df))
  Y = train_data
  graph <- learn_k_component_graph(t(Y) , w0 = "qp", beta = 2000, k = k, alpha = 0.01)
  net <- graph_from_adjacency_matrix(graph$Adjacency, mode = "undirected", weighted = TRUE)
  # colors <- brewer.reds(100)
  # c_scale <- colorRamp(colors)
  # E(net)$color = apply(c_scale(abs(E(net)$weight) / max(abs(E(net)$weight))), 1,
  #                      function(x) rgb(x[1]/255, x[2]/255, x[3]/255))
  # V(net)$color = "pink"
  # setEPS()
  # postscript(paste0("knowledge_graph_k", toString(k), ".ps"))
  # plot(net, vertex.label = names,
  #      vertex.size = 3,
  #      vertex.label.dist = 1,
  #      vertex.label.family = "Helvetica",
  #      vertex.label.cex = .3,
  #      vertex.label.color = "black"
  #      )
  # dev.off()
      colors <- c("#34495E", "#706FD3", "#FF5252", "#33D9B2", "#34ACE0")
      clusters <- array(0, length(names))
      for (i in c(1:length(names))) {
        if (names[i] == 1) {
          clusters[i] = 1
        } else if (names[i] == 2) {
          clusters[i] = 2
        } else {
          clusters[i] = 3
        }
      }
      V(net)$cluster <- clusters
      E(net)$color <- apply(as.data.frame(get.edgelist(net)), 1,
                           function(x) ifelse(V(net)$cluster[x[1]] == V(net)$cluster[x[2]],
                                              colors[V(net)$cluster[x[1]]], brewer.greys(5)[2]))
      V(net)$color <- colors[clusters]
      gr = .5 * (1 + sqrt(5))
      setEPS()
      postscript("full-k3.ps", family = "Times", height = 5, width = gr * 3.5)
      plot(net, vertex.label = NA,
           vertex.size = 3)
      dev.off()

  return (list(theta = graph$Laplacian))
}



