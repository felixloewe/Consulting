# Datensatz ERGM-tauglich machen


A <- get.adjacency(GraphYear[[i]])
v.attrs <- get.data.frame(GraphYear[[i]], what = "vertices")
e.attrs <- get.data.frame(GraphYear[[i]], what = "edges")
head(e.attrs)
Graph1.s <- network::as.network(as.matrix(A), directed = T)
network::set.vertex.attribute(Graph1.s, "Country_Code", v.attrs$name)
network::set.vertex.attribute(Graph1.s, "Country_Name", v.attrs$Country_Name)
network::set.vertex.attribute(Graph1.s, "Continent", v.attrs$Continent)
network::set.vertex.attribute(Graph1.s, "Region", v.attrs$Region)
network::set.edge.attribute(Graph1.s, "Year", e.attrs$Year)
network::set.edge.attribute(Graph1.s, "Value", e.attrs$Value)
network::set.edge.attribute(Graph1.s, "Data_Source", e.attrs$Data_Source)
network::set.edge.attribute(Graph1.s, "IsMirror", e.attrs$IsMirror)
network::set.edge.attribute(Graph1.s, "PRIO_Weapons_Code", e.attrs$PRIO_Weapons_Code)

