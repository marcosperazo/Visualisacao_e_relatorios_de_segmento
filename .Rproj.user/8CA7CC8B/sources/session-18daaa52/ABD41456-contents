## Análise de Clusters
# Curso: MBA Data Science INFNET

# Instalação e carregamento dos pacotes utilizados
pacotes <- c("plotly", 
             "tidyverse", 
             "ggrepel", 
             "knitr", 
             "kableExtra", 
             "reshape2", 
             "misc3d", 
             "plot3D", 
             "cluster",
             "factoextra",
             "readr",
             "sf",
             "ade4",
             "readxl",
             "dplyr")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Carregamento de todo o ambiente
load("ambiente_completo.RData")


# Carregamento dos dados
dados_originais <- read_excel("Pumpkin_Seeds_Dataset.xlsx")

#Exclusão da coluna Class
dados <- dados_originais  %>% select(-Class)


head(dados)        # Mostra as primeiras linhas
str(dados)         # Estrutura das variáveis
summary(dados)     # Estatísticas básicas

#Normalização dos dados
dados_padronizados <- scale(dados)

set.seed(223)
# Método do cotovelo (WSS)
fviz_nbclust(dados_padronizados, kmeans, method = "wss") +
  ggplot2::geom_vline(xintercept = 2, linetype = 2, color = "red") +
  ggplot2::labs(title = "Elbow Method (WSS)")


# Método da silhueta
fviz_nbclust(dados_padronizados, kmeans, method = "silhouette") +
  ggplot2::labs(title = "Silhouette Method")


# executar kmeans (nstart alto para evitar mínimos locais)
set.seed(223)
km_res <- kmeans(dados_padronizados, centers = 2, nstart = 25)
print(km_res)

# Visualização
fviz_cluster(km_res, data = dados_padronizados, ellipse.type = "convex",
             ggtheme = theme_minimal(), main = "K-means (k=2)")

# Comparar com classes reais
print(table(Cluster = km_res$cluster, TrueClass = dados_originais$Class))


# Elaboração da clusterização hierárquica
cluster_hier <- agnes(x = dados_padronizados, method = "single")

# O input é a matriz de distâncias obtida anteriormente

# Method é o tipo de encadeamento:

## "complete": encadeamento completo (furthest neighbor ou complete linkage)
## "single": encadeamento único (nearest neighbor ou single linkage)
## "average": encadeamento médio (between groups ou average linkage)

# Definição do esquema hierárquico de aglomeração

# As distâncias para as combinações em cada estágio
coeficientes <- sort(cluster_hier$height, decreasing = FALSE) 
coeficientes


esquema <- as.data.frame(cbind(cluster_hier$merge, coeficientes))
names(esquema) <- c("Cluster1", "Cluster2", "Coeficientes")
esquema

# Visualização do esquema hierárquico de aglomeração
esquema %>%
  kable(row.names = T) %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Construção do dendrograma
dev.off()
fviz_dend(x = cluster_hier)

# Dendrograma com visualização dos clusters (definição de 2 clusters)
fviz_dend(x = cluster_hier,
          k = 2,
          k_colors = c("deeppink4", "darkviolet"),
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          lwd = 1,
          ggtheme = theme_bw())

# Criando variável categórica para indicação do cluster no banco de dados
## O argumento 'k' indica a quantidade de clusters
dados_padronizados$cluster_H <- factor(cutree(tree = cluster_hier, k = 2))

# Visualização da base de dados com a alocação das observações nos clusters
dados_padronizados %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

save.image(file = "ambiente_completo.RData")
