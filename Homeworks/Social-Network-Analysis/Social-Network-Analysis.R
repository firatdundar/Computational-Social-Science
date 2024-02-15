setwd("")

install.packages ("statnet", dependencies = TRUE, INSTALL_opts = '--no-lock')
install.packages ("igraph", dependencies = TRUE, INSTALL_opts = '--no-lock')
install.packages ("igraph", dependencies = TRUE, INSTALL_opts = '--no-lock')
install.packages ("intergraph", dependencies = TRUE, INSTALL_opts = '--no-lock')

unlink ("C:/Users/firat/AppData/Local/R/win-library/4.2/00LOCK", recursive =TRUE)
library (igraph)
library (statnet)
library (intergraph)

# Load the data
load("hw3.RData")

# Extract the names of the professors
names <- as.character(node.attr$name) # Extract names
names # Print names

# Create a matrix for the professors
mat <- as.matrix(coauthorship) # Convert the data frame into an adjacency matrix
colnames(mat) <- names # Assign column names
rownames(mat) <- names # Assign row names

# Attribute info data frame
rownames(node.attr) <- node.attr$name # Assign row names of the attribute info data frame
node.attr$name <- NULL # Delete the column with the professors' names

# Check for matching names
matching_names <- colnames(mat) %in% rownames(node.attr)
print(matching_names)

graph <- graph.adjacency(mat, mode = "undirected")
graph

graph<-set_vertex_attr(graph, "num.publications", value = node.attr$num.publications)
graph<-set_vertex_attr(graph, "affiliation", value = node.attr$affiliation)
graph<-set_vertex_attr(graph, "subfield", value = node.attr$subfield)
graph

# Get unique affiliations
unique_affiliations <- unique(V(graph)$affiliation)

# Manually assign colors to affiliations
affiliation_colors <- rainbow(length(unique_affiliations))

# Create a color vector for each node based on its affiliation
node_colors <- affiliation_colors[match(V(graph)$affiliation, unique_affiliations)]

# Update the plot function with the new color vector
par(mar=c(0,0,0,0)) # get rid of plot margins
plot(graph, #our network
     vertex.size = 10, # the size of nodes
     vertex.label = V(graph)$name, # what do we want to display as node labels? 
     vertex.label.cex = 0.6, # the size of node labels
     vertex.label.color = "black", # the colour of node labels
     edge.color = "black", # the colour of edges
     edge.arrow.size = 0.5, # edge arrow size
     vertex.color = node_colors, # use the color vector
     layout=layout_with_kk # the layout of the network
)

# Calculate degree centrality
dc_total <- igraph::degree(graph, mode = "total")

# Print degree centrality values
print(dc_total)

# Find the highest degree value
max_degree <- max(dc_total)
cat("Maximum Degree Value:", max_degree, "\n")

# Find the node(s) with the highest degree
nodes_highest_degree <- which(dc_total == max_degree)
cat("Node(s) with Highest Degree:", nodes_highest_degree, "\n")

# Print the corresponding rows from the attribute data
print(node.attr[nodes_highest_degree, ])

# Eigenvector centrality
ec <- round(igraph::evcent(graph)$vector, 2)

# Print eigenvector centrality values
print(ec)

# Find the highest eigenvector centrality value
max_ec <- max(ec)
cat("Maximum Eigenvector Centrality Value:", max_ec, "\n")

# Find the node with the highest eigenvector centrality
node_highest_ec <- which.max(ec)
cat("Node with Highest Eigenvector Centrality:", node_highest_ec, "\n")

# Print the corresponding row from the attribute data
print(node.attr[node_highest_ec, ])

# Betweenness centrality
bc <- round(igraph::betweenness(graph), 2)

# Print betweenness centrality values
print(bc)

# Find the highest betweenness centrality value
max_bc <- max(bc)
cat("Maximum Betweenness Centrality Value:", max_bc, "\n")

# Find the node with the highest betweenness centrality
node_highest_bc <- which.max(bc)
cat("Node with Highest Betweenness Centrality:", node_highest_bc, "\n")

# Print the corresponding row from the attribute data
print(node.attr[node_highest_bc, ])

# Louvain method for community detection
louvain_communities <- igraph::cluster_louvain(graph)

# Print the communities and the number of nodes in each community
print(louvain_communities)

# Interpretation of Louvain results
# Researchers within the same Louvain community are likely to have strong collaboration ties.
# Patterns in affiliations or research interests may be reflected in the community structure.

# Walktrap algorithm for community detection
walktrap_communities <- igraph::cluster_walktrap(graph)

# Print the communities and the number of nodes in each community
print(walktrap_communities)

# Interpretation of Walktrap results
# The Walktrap algorithm identifies communities based on local neighborhood structure.
# Nodes within the same Walktrap community may collaborate due to close proximity in the network.

# Plot the network with colors representing communities for both algorithms
par(mfrow=c(1,2))  # Set up a 1x2 grid for plots

# Louvain method
plot(louvain_communities, graph, main="Louvain Community Detection", vertex.size=10, vertex.label=NA)

# Interpretation of Louvain plot
# Observe the network's structure based on Louvain communities.
# Nodes with the same color belong to the same community, indicating potential collaboration patterns.


# Walktrap algorithm
plot(walktrap_communities, graph, main="Walktrap Community Detection", vertex.size=10, vertex.label=NA)

# Interpretation of Walktrap plot
# Examine the network layout according to Walktrap communities.
# Nodes with the same color are in the same community, revealing potential patterns in local connectivity.

