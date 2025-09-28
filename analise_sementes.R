## Análise de Clusters
# Curso: MBA Data Science INFNET

# Instalação e carregamento dos pacotes utilizados
pacotes <- c("plotly", 
             "tidyverse", 
             "ggrepel", 
             "knitr", 
             "kableExtra", 
             "reshape2",
             "PerformanceAnalytics",
             "psych",
             "ltm",
             "Hmisc",
             "misc3d", 
             "plot3D", 
             "cluster",
             "factoextra",
             "clusterCrit",
             "mclust",
             "clusterSim",
             "aricode",
             "fossil",
             "dendextend",
             "fpc",
             "NbClust",
             "ggplot2",
             "gridExtra",
             "tidyverse",
             "plotly",
             "ClusterR",
             "reshape2",
             "GGally",
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

# Reduzindo a quantidade de registros
set.seed(223)  # definindo uma semente para garantir a reprodutibilidade
dados_reduzidos <- dados_originais %>% slice_sample(n = 500)

dados_reduzidos <- as.data.frame(dados_reduzidos)

# Pairplot
ggpairs(dados_reduzidos, 
        columns = 1:12,         # Apenas variáveis numéricas
        aes(color = Class),  # Cor por classe
        upper = list(continuous = "points"), 
        lower = list(continuous = "smooth"), 
        diag = list(continuous = "densityDiag")) +
  theme_linedraw()

#Exclusão da coluna Class
dados <- dados_reduzidos %>% dplyr::select(-Class)

head(dados)        # Mostra as primeiras linhas
str(dados)         # Estrutura das variáveis
summary(dados)     # Estatísticas básicas

#Normalização dos dados
dados_padronizados <- scale(dados)

ground_truth <- dados_reduzidos$Class
n_rows <- nrow(dados_padronizados)
k_clusters <- 2 # numero de clusters

# --- K-MEANS ---
set.seed(223)
kmeans_res <- kmeans(dados_padronizados, centers = k_clusters, nstart = 25)

# --- PAM ---
pam_res <- pam(dados_padronizados, k = k_clusters)

# --- HIERÁRQUICO ---
dist_matrix <- dist(dados_padronizados, method = "euclidean")
hc_res <- hclust(dist_matrix, method = "ward.D2") # linkage Ward
hc_clusters <- cutree(hc_res, k = k_clusters)

# --- MÉTRICAS INTERNAS ---

# Silhueta
sil_kmeans <- silhouette(kmeans_res$cluster, dist(dados_padronizados))
sil_pam <- silhouette(pam_res$clustering, dist(dados_padronizados))
sil_hc <- silhouette(hc_clusters, dist(dados_padronizados))

# WSS (Within-cluster Sum of Squares)
wss_kmeans <- kmeans_res$tot.withinss
wss_pam <- sum(sapply(1:k_clusters, function(k) sum((dados_padronizados[pam_res$clustering == k, ] - pam_res$medoids[k, ])^2)))
wss_hc <- sum(sapply(1:k_clusters, function(k) {
  cluster_points <- dados_padronizados[hc_clusters == k, , drop = FALSE]
  center <- colMeans(cluster_points)
  sum(rowSums((cluster_points - center)^2))
}))

# Davies-Bouldin
db_kmeans <- index.DB(dados_padronizados, kmeans_res$cluster)$DB
db_pam <- index.DB(dados_padronizados, pam_res$clustering)$DB
db_hc <- index.DB(dados_padronizados, hc_clusters)$DB

# --- RESULTADOS ---
cat("Resultados (Métricas Internas):\n")
cat("K-Means - Silhueta:", mean(sil_kmeans[, 3]),
    " WSS:", wss_kmeans,
    " DBI:", db_kmeans, "\n")

cat("PAM     - Silhueta:", mean(sil_pam[, 3]),
    " WSS:", wss_pam,
    " DBI:", db_pam, "\n")

cat("Hierárq - Silhueta:", mean(sil_hc[, 3]),
    " WSS:", wss_hc,
    " DBI:", db_hc, "\n")

resultados <- data.frame(
  Algoritmo = c("K-Means", "PAM", "Hierárquico"),
  Silhueta = c(mean(sil_kmeans[, 3]), mean(sil_pam[, 3]), mean(sil_hc[, 3])),
  WSS = c(wss_kmeans, wss_pam, wss_hc),
  DBI = c(db_kmeans, db_pam, db_hc)
)

print(resultados)

# Gráfico Silhueta
p1 <- ggplot(resultados, aes(x = Algoritmo, y = Silhueta, fill = Algoritmo)) +
  geom_bar(stat = "identity") +
  ggtitle("Comparação - Coeficiente de Silhueta") +
  theme_minimal()

p1

# Gráfico WSS
p2 <- ggplot(resultados, aes(x = Algoritmo, y = WSS, fill = Algoritmo)) +
  geom_bar(stat = "identity") +
  ggtitle("Comparação - WSS (Within-cluster Sum of Squares)") +
  theme_minimal()

p2

# Gráfico DBI
p3 <- ggplot(resultados, aes(x = Algoritmo, y = DBI, fill = Algoritmo)) +
  geom_bar(stat = "identity") +
  ggtitle("Comparação - Davies-Bouldin Index") +
  theme_minimal()

p3

# Organizar em grid 1x3
grid.arrange(p1, p2, p3, nrow = 3)


########################


# Coeficientes de correlação de Pearson para cada par de variáveis
rho <- rcorr(as.matrix(dados_padronizados), type="pearson")

corr_coef <- rho$r # Matriz de correlações
corr_sig <- round(rho$P, 5) # Matriz com p-valor dos coeficientes





# Elaboração de um mapa de calor das correlações de Pearson entre as variáveis
ggplotly(
  dados_padronizados %>%
    cor() %>%
    melt() %>%
    rename(Correlação = value) %>%
    ggplot() +
    geom_tile(aes(x = Var1, y = Var2, fill = Correlação)) +
    scale_fill_viridis_b() +
    labs(x = NULL, y = NULL) +
    theme_bw(base_size = 6))

# Teste de esfericidade de Bartlett
cortest.bartlett(dados_padronizados)

###p.value é igaual a zero

# Elaboração da análise fatorial por componentes principais
fatorial <- principal(dados_padronizados,
                      nfactors = ncol(dados_padronizados),
                      rotate = "none",
                      scores = TRUE)

cum_var <- fatorial$Vaccounted["Cumulative Var", ]
cum_var

# Encontra o menor número de componentes que atinge ou ultrapassa 95%
num_comp <- which(cum_var >= 0.95)[1]
num_comp

variancia_compartilhada <- as.data.frame(fatorial$Vaccounted) %>% 
  slice(1:3)

rownames(variancia_compartilhada) <- c("Autovalores",
                                       "Prop. da Variância",
                                       "Prop. da Variância Acumulada")

# Variância compartilhada pelas variáveis originais para a formação de cada fator
round(variancia_compartilhada, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)


pca_df <- data.frame(
  fatorial$scores[, 1:2], 
  classe_da_semente = dados_reduzidos$Class
)



ggplot(pca_df, aes(x = PC1, y = PC2, color = classe_da_semente)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(title = "PCA - Classe da Semente",
       x = "Componente Principal 1",
       y = "Componente Principal 2") +
  theme_minimal()


###############################

#Exclusão da coluna Class
dados_pca_df <- pca_df %>% dplyr::select(-classe_da_semente)

head(dados_pca_df)        # Mostra as primeiras linhas
str(dados_pca_df)         # Estrutura das variáveis
summary(dados_pca_df)     # Estatísticas básicas

ground_truth_pca_df <- pca_df$classe_da_semente
n_rows_pca_df <- nrow(dados_pca_df)
k_clusters_pca_df <- 2 # numero de clusters

# --- K-MEANS ---
set.seed(223)
kmeans_res_pca_df <- kmeans(dados_pca_df, centers = k_clusters, nstart = 25)

# --- PAM ---
pam_res_pca_df <- pam(dados_pca_df, k = k_clusters)

# --- HIERÁRQUICO ---
dist_matrix_pca_df <- dist(dados_pca_df, method = "euclidean")
hc_res_pca_df <- hclust(dist_matrix_pca_df, method = "ward.D2") # linkage Ward
hc_clusters_pca_df <- cutree(hc_res_pca_df, k = k_clusters)

# --- MÉTRICAS INTERNAS ---

# Silhueta
sil_kmeans_pca_df <- silhouette(kmeans_res_pca_df$cluster, dist(dados_pca_df))
sil_pam_pca_df <- silhouette(pam_res_pca_df$clustering, dist(dados_pca_df))
sil_hc_pca_df <- silhouette(hc_clusters_pca_df, dist(dados_pca_df))

# WSS (Within-cluster Sum of Squares)
wss_kmeans_pca_df <- kmeans_res_pca_df$tot.withinss
wss_pam_pca_df <- sum(sapply(1:k_clusters_pca_df, function(k) sum((dados_pca_df[pam_res_pca_df$clustering == k, ] - pam_res_pca_df$medoids[k, ])^2)))
wss_hc_pca_df <- sum(sapply(1:k_clusters_pca_df, function(k) {
  cluster_points <- dados_pca_df[hc_clusters == k, , drop = FALSE]
  center <- colMeans(cluster_points)
  sum(rowSums((cluster_points - center)^2))
}))

# Davies-Bouldin
db_kmeans_pca_df <- index.DB(dados_pca_df, kmeans_res_pca_df$cluster)$DB
db_pam_pca_df <- index.DB(dados_pca_df, pam_res_pca_df$clustering)$DB
db_hc_pca_df <- index.DB(dados_pca_df, hc_clusters_pca_df)$DB

# --- RESULTADOS ---
cat("Resultados (Métricas Internas):\n")
cat("K-Means - Silhueta:", mean(sil_kmeans_pca_df[, 3]),
    " WSS:", wss_kmeans_pca_df,
    " DBI:", db_kmeans_pca_df, "\n")

cat("PAM     - Silhueta:", mean(sil_pam_pca_df[, 3]),
    " WSS:", wss_pam_pca_df,
    " DBI:", db_pam_pca_df, "\n")

cat("Hierárq - Silhueta:", mean(sil_hc_pca_df[, 3]),
    " WSS:", wss_hc_pca_df,
    " DBI:", db_hc_pca_df, "\n")

resultados_pca_df <- data.frame(
  Algoritmo = c("K-Means", "PAM", "Hierárquico"),
  Silhueta = c(mean(sil_kmeans_pca_df[, 3]), mean(sil_pam_pca_df[, 3]), mean(sil_hc_pca_df[, 3])),
  WSS = c(wss_kmeans_pca_df, wss_pam_pca_df, wss_hc_pca_df),
  DBI = c(db_kmeans_pca_df, db_pam, db_hc_pca_df)
)

print(resultados_pca_df)

# Gráfico Silhueta
p4 <- ggplot(resultados_pca_df, aes(x = Algoritmo, y = Silhueta, fill = Algoritmo)) +
  geom_bar(stat = "identity") +
  ggtitle("Comparação - Coeficiente de Silhueta") +
  theme_minimal()

p4

# Gráfico WSS
p5 <- ggplot(resultados, aes(x = Algoritmo, y = WSS, fill = Algoritmo)) +
  geom_bar(stat = "identity") +
  ggtitle("Comparação - WSS (Within-cluster Sum of Squares)") +
  theme_minimal()

p5

# Gráfico DBI
p6 <- ggplot(resultados, aes(x = Algoritmo, y = DBI, fill = Algoritmo)) +
  geom_bar(stat = "identity") +
  ggtitle("Comparação - Davies-Bouldin Index") +
  theme_minimal()

p6

# Organizar em grid 1x3
grid.arrange(p4, p5, p6, nrow = 3)




save.image(file = "ambiente_completo.RData")


