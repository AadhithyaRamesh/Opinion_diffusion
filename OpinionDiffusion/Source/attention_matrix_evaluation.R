library(hashmap)
library(igraph)

attention_flow = read.csv("../Data/Attention_flow.csv")
direction_matrix = read.csv("../Data/Direction_matrix.csv")

opposite_edges = data.frame(Flow_from_user = character(), Flow_to_user = character(), flow = double())

for(index in 1:nrow(attention_flow))
{
  if(attention_flow$flow[index][1] > 0)
  {
    attention_flow$flow[index] = 1
  }
  else if(attention_flow$flow[index][1] < 0)
  {
    attention_flow$flow[index] = -1
  }
}

attention_sorted = attention_flow[order(attention_flow$Flow_from_user, attention_flow$Flow_to_user), ]
direction_sorted = direction_matrix[order(direction_matrix$Flow_from_user, direction_matrix$Flow_to_user), ]

write.csv(attention_sorted, "../Data/Attention_matrix_sorted.csv")
write.csv(direction_sorted, "../Data/Direction_matrix_sorted.csv")

for(index in 1:nrow(attention_sorted))
{
  if(attention_sorted$flow[index] != direction_sorted$flow[index])
  {
    vertex_name = attention_sorted$Flow_from_user[index]
    neighbour_name = attention_sorted$Flow_to_user[index]
    flow = attention_sorted$flow[index]
    opposite_edges = rbind(opposite_edges, data.frame("Flow_from_user" = vertex_name, "Flow_to_user" = neighbour_name, "flow" = flow, fix.empty.names = TRUE), stringsAsFactors = FALSE)
  }
}
write.csv(opposite_edges, "../Results/opposite_edges.csv")
print(mean(attention_sorted$flow == direction_sorted$flow))