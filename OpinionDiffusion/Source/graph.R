# Load igraph
library(igraph)

name1 = c('A','B','C','D','E','F')
name2 = c('C','A','F','A','B','D')
nodes = c('A', 'B', 'C', 'D', 'E', 'F')
type = factor(c(1,1,2,2,1,2))

friends = data.frame(name1, name2, type)
nodes = data.frame(nodes, type)


# Inspect the first few rows of the dataframe 'friends'
head(friends)

# Convert friends matrix to an igraph object
g <- graph_from_data_frame(d = friends, vertices = nodes, directed = TRUE)

print(vertex_attr(g))
# Make a very basic plot of the network
plot(g)

