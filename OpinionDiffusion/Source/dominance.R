library(hashmap)
library(igraph)

#Calculates the dominance of each node in the provided network
dominance = function(network, nodes)
{
  #Matrix to store flow between nodes
  attention_flow = data.frame(Flow_from_user = character(), Flow_to_user = character(), flow = double())

  dominance = c()

  #Magnitude of externality
  sigma = rnorm(length(V(network)))

  #Risk-aversion is modelled as rho
  rho = 1

  #Count of independent users
  count = 0
  
  #Iterate over each vertex of the network
  for (vertex in V(network))
  {

    #Find all neighbours of selected vertex
    n = neighbors(network, v = vertex)
    

    #Find degree of the selected vertex
    degree_node = degree(network, v = vertex, mode = "total")

    #Find individual degrees of all neighbours of the selected vertex
    neighbour_degrees = degree(network, v = n)
  
    neighbour_attributes = vertex.attributes(network, n)  
    
    vertex_name = vertex.attributes(network, vertex)$name
    
    # if(vertex_name=="__baddog")
    # {
    #   print(vertex_name)
    # }

    # if(length(n) == 0)
    # {
    #   count = count +1
    #   next
    # }
    
    ln_term = 0
    
    #Iterate over neighbours of the selected vertex
    for (index in seq(1,length(neighbour_degrees)))
    {
      degree = neighbour_degrees[index]

      #Calculate flow from/to selected vertex to/from the selected neighbour
      flow = log(((1+degree_node)/(1+degree)), base = exp(1))
      # print(flow)

      # #To check flow zero
      # if(flow == 0)
      # {
      #   print(degree)
      #   print(degree_node)
      # }
            
      #Add flow to sum (Used in dominance calculation)
      ln_term = ln_term + log(((1+degree_node)/(1+degree)), base = exp(1))

# 
#       if(length(neighbour_attributes$name[index])==0)
#       {
#         
#         neighbour_name = "EMPTY"
#       }
#       else
#       {
#         neighbour_name = neighbour_attributes$name[index]
#       }
# 
#       if(length(flow)==0)
#       {
#         flow = 0
#         print(neighbour_name)
#       }
#       


      # attention_flow = rbind(attention_flow, data.frame("Flow_from_user" = vertex_name, "Flow_to_user" = neighbour_name, "flow" = flow, fix.empty.names = TRUE), stringsAsFactors = FALSE)
      # attention_flow = rbind(attention_flow, data.frame("Flow_from_user" = neighbour_name, "Flow_to_user" = vertex_name, "flow" = flow, fix.empty.names = TRUE), stringsAsFactors = FALSE)
      
      
    }
    

    # print(ln_term)
    # Calculate dominance
    dominance = c(dominance, (1 + ((1 / (length(n) * rho)) * ln_term)))

  }
  print(length(dominance))
  print(length(V(network)))
  #Add dominance to nodes as an attribute
  dominance_nodes = cbind(nodes, dominance)
  dominance_nodes[is.na(dominance_nodes)] = 0
  
  # print(dominance_nodes$dominance + abs(min(dominance_nodes$dominance)))+

  
    # return(attention_flow)
  return(dominance_nodes)
}

edges = read.csv("../Data/Edge_dataframe_demonet.csv")[, -1]
nodes = read.csv("../Data/Vertice_dataframe_demonet.csv")[, -1]

direction_matrix = data.frame(Flow_from_user = character(), Flow_to_user = character(), flow = double())
# print(nrow(edges))
# for(index in 1:nrow(edges))
# {
#   matrix_row_forward = data.frame("Flow_from_user" = edges[index,]$tweeters, "Flow_to_user" = edges[index,]$retweeters, "flow" = 1, stringsAsFactors = FALSE)
#   matrix_row_backward = data.frame("Flow_from_user" = edges[index,]$retweeters, "Flow_to_user" = edges[index,]$tweeters, "flow" = -1, stringsAsFactors = FALSE)
#   direction_matrix = rbind(direction_matrix, matrix_row_forward)
#   direction_matrix = rbind(direction_matrix, matrix_row_backward)
# 
#   }
# print("Direction Matrix done")

# Convert edges and nodes dataframes to an igraph object
network = graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

# Calls the dominance function.
# nodes = dominance(network, nodes)
# print(nodes)
print(nrow(nodes))
print("Done")
# write.csv(direction_matrix, "../Data/Direction_matrix.csv")
# write.csv(nodes, "../Data/Attention_flow.csv")
print("Write Done")