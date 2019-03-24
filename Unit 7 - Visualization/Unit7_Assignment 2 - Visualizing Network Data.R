Sys.setlocale("LC_ALL", "C")

################################################################################
#                       Problem 1.1 - Summarizing the Data                     #
################################################################################
edges <- read.csv('edges.csv')
users <- read.csv('users.csv')
nrow(users)
str(edges)
(2*146)/59

str(users)
table(users$school, users$locale)

table(users$school, users$gender)

################################################################################
#                    Problem 2.1 - Creating a Network                          #
################################################################################
install.packages('igraph')
library(igraph)
?graph.data.frame

g <- graph.data.frame(edges, F, users)
plot(g, vertex.size=5, vertex.label=NA)

sort(degree(g))
table(degree(g))

V(g)$size <- degree(g)/2+2
plot(g, vertex.label=NA)

################################################################################
#                  Problem 3.1 - Coloring Vertices                             #
################################################################################
V(g)$color <- 'black'
V(g)$color[V(g)$gender=='A'] <- 'red'
V(g)$color[V(g)$gender=='B'] <- 'gray'
plot(g, vertex.label=NA)

V(g)$color <- 'black'
V(g)$color[V(g)$school=='A'] <- 'red'
V(g)$color[V(g)$school=='AB'] <- 'gray'
plot(g, vertex.label=NA)
table(users$locale)

V(g)$color <- 'black'
V(g)$color[V(g)$locale=='A'] <- 'red'
V(g)$color[V(g)$locale=='B'] <- 'gray'
plot(g, vertex.label=NA)

?igraph.plotting
