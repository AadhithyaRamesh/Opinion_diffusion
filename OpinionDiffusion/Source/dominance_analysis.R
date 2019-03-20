library(hashmap)
library(igraph)

edges = read.csv("../Data/Edge_dataframe_demonet.csv")[, -1]
nodes = read.csv("../Data/Vertice_dataframe_demonet.csv")[, -1]

print(head(nodes))

dominance_analysis = function(nodes, edges)
{
  # Creatte network from edge and vetex list
  network = graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
  
  print(betweenness(network))
  
  #Sort Users by Dominance descending
  sorted_nodes_by_dominance_desc = nodes[order(-nodes$dominance),]
  
  # Filter Dominance 
  print(sorted_nodes_by_dominance_desc[sorted_nodes_by_dominance_desc$dominance >= 4, ])

  # Write to CSV
  # write.csv(sorted_nodes_by_dominance_desc[sorted_nodes_by_dominance_desc$dominance >= 4, ], file = "Sorted_by_dominance_womenmarch.csv")
  
  # Create Dominance Histogram
  dominance_histogram = hist(nodes$dominance, main = "Dominance Histogram", xlab = "Dominance", breaks = seq(min(nodes$dominance), max(nodes$dominance), l = 20))
  
  
  
  # Filtering dominance and degree to find densely connected components
  
  densely_connected_nodes = data.frame(User_name = character(), Dominance = double())

  dominance_filtered_sorted_nodes = sorted_nodes_by_dominance_desc[abs(sorted_nodes_by_dominance_desc$dominance) <= 3, ]

  for (vertex in V(network))
  {
    # Creating list of neighbours and their degrees
    
    neighbour_list = neighbors(network, v = vertex)
    neighbour_degrees = degree(network, v = neighbour_list)
    
    if( length(neighbour_list) == 0)

    {
      next  
    } 

    if (V(network)[vertex]$name %in% dominance_filtered_sorted_nodes$User_name)
    {

      # degree_node = degree(network, v = vertex)

      if (mean(neighbour_degrees) >= 6)
      {

        # n = neighbors(network, v = vertex)
        densely_connected_nodes = rbind(densely_connected_nodes, data.frame(User_name = V(network)[vertex]$name, Dominance = V(network)[vertex]$dominance))
      }
    }

  }

  # print(densely_connected_nodes$User_name)
  
  
  # Visualisation of the network

  dominance_network = graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

  V(dominance_network)$size = V(dominance_network)$dominance * 3 + 10#abs(min(dominance_network[,3]))
  V(dominance_network)$color = V(dominance_network)$dominance >= 2
  plot.igraph(dominance_network,vertex.label=NA,layout=layout.fruchterman.reingold)
  
  # print(head(sorted_nodes_by_dominance_desc, 15))
  return (dominance_histogram)
}

# dominance_histogram = dominance_analysis(nodes, edges)
