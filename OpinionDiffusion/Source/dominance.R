library(hashmap)
library(igraph)

dominance = function(network, nodes)
{
  dominance = c()
  sigma = rnorm(length(V(network)))
  for (vertex in V(network))
  {
  
    n = neighbors(network, v = vertex)
    degree_node = degree(network, v = vertex)

    neighbour_degrees = degree(network, v = n)

    ln_term = 0
    for (degree in neighbour_degrees)
    {
      ln_term = ln_term + log((1+degree_node)/(1+degree), base = exp(1))
    }
    dominance = c(dominance, 1 + (1 / (length(n) * 0.8) * ln_term))
    ln_term = 0
    
  }
  nodes = cbind(nodes, dominance)
  return(nodes)
}

