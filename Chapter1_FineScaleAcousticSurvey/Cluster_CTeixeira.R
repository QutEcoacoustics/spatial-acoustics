##Cluster Analysis
##Quantitative methods in R
##Camila Teixeira
##Modified: Marina D. A. Scarpelli
##12.11.2019

############################################
rm(list = ls())
library(tidyverse)
getDataPath <- function (...) {
  return(file.path("C:/Users/n10393021/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

getDataPath <- function (...) {
  return(file.path("C:/Users/Nina Scarpelli/OneDrive - Queensland University of Technology/Documents/PhD/Project",  ...))
}

##SM4 Fieldwork - Reading the data - using the normalised table output from normilisingindices.R - after normalising and excluding highly correlated ones

df <- read.csv(getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SummaryIndices_Channel1", "SummaryIndices_Channel1_WindRemoved.csv"), row.names = 23) %>% 
  separate(., col = FileName, into = c("Point", "Date", "beginning_rec"), sep = "_", remove = FALSE)

#df <- select(df, -c(TemporalEntropy, EntropyOfAverageSpectrum))

#correlation matrix between normalised indices
#cor <- abs(cor(df[2:8], use = "complete.obs", method = "spearman")) %>% 
  #write.csv(getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SummaryIndices_Channel1", "correlationmatrix4.csv")) #Highly correlated indices removed

# The cluster need to be done by steps. First of all we need to verify if the data is clusterable
library(factoextra)

#This dataset has been prepared according to the steps above: first removing indices with NA values; Second removing the highly correlated indices and then grouping the dataset according to the timing - both 4 and 8 groups per day. 4 groups are the following: (1) 00:00 to 06:00 - night; (2) 06:00 to 12:00 - morning; (3) 12:00 to 18:00 - afternoon; (4) 18:00 to 00:00 - night. 8 groups are the following: (1) 00:00 to 03:00 - early night; (2) 03:00 to 06:00 - late night; (3) 06:00 to 09:00 - early morning; (4) 09:00 to 12:00 - late morning; (5) 12:00 to 15:00 - ealy afternoon,  (6) 15:00 to 18:00 - late aftternoon; (7) 18:00 to 21:00 - early evening; (5) 21:00 to 00:00 - late evening.
cluster_summary_channel1_df <- read.csv(getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SummaryIndices_Channel1", "Cluster_preparation.csv"), row.names = 1) #After highly correlated removed; also after classification of 4 and 8 timing groups

get_clust_tendency(cluster_summary_channel1_df[8], n = 17714, graph = F) #Result Hopking's stat: 0.0748 - Total dataset; Results index by index: BGN - 0.05396606; 

df <- filter(cluster_summary_channel1_df, categorical_time_8groups == "early_evening") #Result Hopking's stat: evening - 0.07362925; morning -  0.1398465; afternoon - 0.1623928; night - 0.07013735; early_night - 0.07555385; late_night - 0.08163642; early_morning - 0.1373064; late_morning: 0.2403525; early_afternoon - 0.1992878; late_afternoon - 0.1990164; early_evening - 0.08941015; late_evening - 0.09246284;


get_clust_tendency(df[8:14], n = 3168, graph = F)

d <- pairs(cluster_summary_channel1_df[2:10]) 



dist.matrix <- dist(df[c(2, 4:12, 14:15)], method = "euclidean")

#Calculation of the cluster - hierarchical clusters usind ward.d2 method (Phillips et al, 2018)
cluster_summary_channel1 <- hclust(dist.matrix, method = "average")

plot(cluster_summary_channel1, hang=-1)

# No gráfico acima vemos que temos um grupo de mata 1a e outro de 2a. Mas tem uma execão a area 4. O que sera que tem na area 4? Se fossem dados reais eu teria que ir ver o que aconteceu, pois claramente as variáveis ambientais separaram mata 1a de 2a. O que isso pode querer dizer? Bom pode querer dizer que ela tem característica diferentes, ou que eu errei ao classificá-la no campo, ou é uma mata 1a com carcterísticas diferentes. Tem mais coisas? Nada muito relevante. Meu agrupamento separa esses dois tipos de ambientes. Posso aplicar em várias situações diferentes. 

# Verificando o quanto o cluster de fato representa a matriz:

# quando eu faço a matriz reversa eu tenho uma matriz cofenética, ou seja, é a matriz a partir do cluste, no exemplo da aula teórica sria o seguinte: eu construiria uma matriz cofenética que teria o seguintes valores:


# A distância de A para B e de B para C sera o valor de 5.7, que é a media das dusa distâncias da matriz original, pois usamos o método de UPGMA, ou seja é a distância do B para o agrupamento A e C.
# Olha que sacada! Pode ser que nos meus dados a distância de um ponto para o outro varie muito e na hora que eu uso a media para fazer a distâncias dos agrupamentos haja muita discrepância. Então tanto o cluster quanto a minha matriz cofenética (que são a mesma coisa, né?), não representam muito bem a minha matriz original, certo? Ele tá distorcendo, porque usando as medias le não está representando nem uma coisa nem outra. Então eu posso fazer a distância simples ou a completa? Poderia, mas ele também vai distorcer. Então o que vai acontecer? Toda as vezes que eu olhar para o cluster eu tenho que perguntar, ele representa bem meus dados originais? Como sei isso? Eu calculo a matriz cofenética e calculo o quanto ela me se parece com a minha matriz original. Usando uma métrica de correlação (comando cor que é a correlação de Pearson). Se a matriz original e a cofenética são muito parecidas, com a correlação perto de 1, eu fico muito confiante que o cluster representa bem meus dados originais. Se não, eu não devo usar o cluster, poise le não irá representar meus dados reias. Para que serve o cluster? Só para representar a matriz graficamente! Quando eu olho a matriz é difícil de interpreter, aí uso o cluster para interpreter, se ele não representa a matriz original então eu não deveria usar ele para nada!

matriz.cof<-cophenetic(cluster_summary_channel1) # calculando a matriz cofenética, lembrando que é do cluster original. Ë uma outra matriz que foi baseada no cluster  é voltar o cluster para uma matriz.

cor(matriz.cof, dist.matrix) # agora calculo o coeficiente de correlação cofenético  que é a correlação da matriz cofenética (que é o meu cluster) e da matriz original. Ou seja, é uma medida de 0 a 1 de quanto o cluster representou os seus dados.

# No nosso exemplo o valor foi 0.7386 

#Usualmente o corte é feito em 0.7  a maioria dos autores na literature usa esse valor, mas ele é subjetivo! Se o valor do seu coeficiente é 0.7 ou mais quer dizer que seu cluster representa bem sua matriz original, se não, não representa. Por outro lado se seu coeficiente de correlação cofenética ficou abaixo disso, significa que seu cluster não representa muito bem sua matriz original. Assim, você abandona o cluster. Não presta para nada! Aí o que eu faço? Aí posso usar uma PCA (análise de component principal), que é um pouco mais flexivel! O cluster tem uma interpretação intuitive, mas as vezes ele não presta para nada! 
# O CALCULA DO COEFICIENTE DE CORRELAÇÃO COFENÉTICO É OBRIGATÓRIO!!!! SEMPRE TEM QUE USAR! Se um cluster não tem esse coeficiente eu não sei se o cluster representa ou não os meus dados originais! 
# Hipótese da banca futura == você nunca sabe quem vai estar na banca!

#Testing which is the best K number of clusters using the three available methods - with the whole dataset the cluster tendency was not good, so we split the data into groups according to the time of the beginning of the recording
library(factoextra)
fviz_nbclust(df, FUNcluster = hcut, method = "wss", k.max = 20, verbose = T)

fviz_nbclust(df, FUNcluster = hcut, method = "gap_stat", k.max = 20, verbose = T)

fviz_nbclust(df, FUNcluster = hcut, method = "silhouette", k.max = 20, verbose = T)

cut_avg <- cutree(cluster_summary_channel1, h = 50.5) #splitting the results into 20 clusters - arbitrary i looked the dendogram and simply decided this was a good number#

cluster_summary_channel1_df <- mutate(df, cluster = cut_avg, cluster_order = cluster_summary_channel1) #assigning the cluster number to the df - now you'll be able to inspect# 

#write.csv(cluster_summary_channel2_df, getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SummaryIndices_Channel1", "20Cluster1.csv"))
 

#cluster_summary_channel1 <- read.csv(getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SummaryIndices_Channel1", "20Cluster.csv"))

#df1 <- select(cluster_summary_channel1_df, 21:26)
#df_total <- cbind(df1, df)
#write.csv(df_total, getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SummaryIndices_Channel1", "Cluster_preparation.csv"))
  #separate(., col = FileName, into = c("Point", "Date", "beginning_rec"), sep = "_", remove = FALSE) %>%
  #select(., -c(X.1, X.2, X.3)) %>%
  #write.csv(., getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SummaryIndices_Channel1", "20Cluster1.csv"))
  #mutate(., FID = paste(FileName, ResultMinute, sep = "_"))
#row.names(cluster_summary_channel1_df) <- cluster_summary_channel1_df$FID
#df <- select(cluster_summary_channel1_df, -c(Snr, EntropyOfVarianceSpectrum, SptDensity)) %>% 
  #separate(., col = FileName, into = c("Point", "Date", "beginning_rec"), sep = "_", remove = FALSE) %>% 
  #write.csv(., getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SummaryIndices_Channel1", "Cluster_preparation.csv"))

#cluster_summary_channel1_df <- read.csv(getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SummaryIndices_Channel1", "Cluster_preparation.csv"), row.names = 27) %>% #After highly correlated removed; also after classification of 4 and 8 timing groups
  #select(-X.1) %>% 
  #select(X, Activity, EventsPerSecond, ClusterCount, everything())
  

df <- filter(cluster_summary_channel1_df, categorical_time_4groups == "evening")




res.dist <- get_dist(df[2:13], stand = TRUE, method = "euclidean")
summary(res.dist)

fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

dist.matrix <- dist(df[2:13], method = "euclidean")

library(cluster)

earlyevening_cluster <- fanny(df[2:13], metric = "euclidean", k = 5, diss = F, memb.exp = 1)

library(mclust)

evening_cluster <- Mclust(df[2:13])


matriz.cof <- cophenetic(cluster_morning) # calculando a matriz cofenética, lembrando que é do cluster original. Ë uma outra matriz que foi baseada no cluster  é voltar o cluster para uma matriz.

coph_morning <- cor(matriz.cof, dist.matrix)

library(factoextra)
fviz_nbclust(df[2:13], FUNcluster = kmeans, method = "wss", k.max = 30, verbose = T)

fviz_nbclust(df[2:13], FUNcluster = kmeans, method = "gap_stat", k.max = 25, verbose = T)

fviz_nbclust(df[2:13], FUNcluster = kmeans, method = "silhouette", k.max = 25, verbose = T)
  

  count <- count(cluster_summary_channel1_df, cluster) #number of observations per cluster#

inspection_minutes <- filter(cluster_summary_channel1_df, cluster == "20") %>% 
  sample_n(., size = 3, replace = F) %>% 
  #write.csv(., getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SummaryIndices_Channel1", "20ClusterInspection6_SummaryIndices.csv"))




#################################################
#5.6- Reconhecendo e destacando grupos. Como tenho dois tipos de ambiente diferentes pode ser que o meu cluster pode ter grupos. Entao podemos destacar grupos. Serve para eu marcar grupos, mas sem sentido biológico.

# esse método é um pouco grosseiro

plot(cluster.amb, hang=-1, main=NULL) # esse método só funciona com o cluster na vertical, então eu uso o hang =-1 para ele ficar esticatinho. Main= NULL é para tirar o título que ele cria.

rect.hclust(cluster.amb, h=2) # esse comando vai ler o objeto, e só funciona com o cluster aberto. Você pode pedir para ele fazer um corte. Não é muito legal, mas dá pra interpreter. Então eu vou fazer um corte nas distâncias > ou = a 2. Então eu coloco h=2 (h= altura). Vai mostrar 4 agrupamentos. Ë bom para tentar entender o que está acontecendo no cluster. Se quiser fazer outro corte, tem que fechar e mudar o h e rodar de novo.

plot(cluster.amb, hang=-1, main=NULL)
rect.hclust(cluster.amb, k=2) # outra possibilidade é fazer por números de agrupamentos. Você pede para o R o tentar fazer um número de agrupamentos. Aí usamos o k= número de agrupamentos que eu quero. 

grupos<-cutree(cluster.amb, k=2) # Se você quiser fazer uma lista de cada grupo que pertence a cada agrupamento que você fez. Aí crio um objeto que eu chamei de grupos e uso a função com o k ou h. ela é legal porque depois eu posso fazer teste estateticos das unidades amostrais que pertencem a um grupo ou outro. Então ao inves de fazer manualmente. Ele extrai quem pertece a cada grupo.

grupos


#Colocando nomes no cluster: quando a gente olha nossa planilha de dados, tinhamos a variável como a divisão dos ambientes. Será que em conjunto as condiçòes ambientais diferem se eu estou na primária pu secundária? Essa pergunta pode se transformer em um teste de hipótese, certo? Mas ele é um pouquinho diferente. Se eu pegasse uma por uma das variáveis eu faria um teste t para cada, moleza! Mas aqui é diferente, poise u tenho as 4 variáveis de uma vez, eu posso testar cada uma de uma vez, é válido. Mas posso estar interessado em verificar o conjunto. 4 variáveis são poucas daria para testar uma a uma, mas e se eu tivesse umas 12 variáveis? Não rola fazer 12 teste t, fica difícil discutir isso depois. Como que eu faço para testar todas de uma vez? Tem várias alternativas. A 1a, que nem é um teste, é tentar visualizer, e tentar entender se os agrupamentos formados pelas distâncias formadas tem-se uma organizaçào de acordo com o ambiente.


cluster2.amb<-as.dendrogram(cluster.amb) # adicionar no meu cluster os nomes no final. Eu crio um objeto (cluster2.amb) para facilitar na hora de fazer o cluster, ok? Faço as.dendogram pois quero o meu cluster na horizontal.

plot(cluster2.amb, horiz=T) # fazendo o cluster na horizontal

labels(cluster2.amb) # são os nomes das unidades


códigos<-ifelse(Ambiente=="Mata_Primária", "MP", "MS") # faço um codigo para colocar quem é 1a e quem é 2a. Vou usar o ifelse (é uma função que fala assim: se uma coisa for de uma forma ela é assim senão ela é assado). Então o ifelse agrupa isso num código bem simples.A função tá falando assim: ifelse 9Ambiente==”Mata Primária”, eu quero que ele coloque “MP”, senão for eu quero que ele coloque “MS”. 

Códigos # aparece na mesma ordem da minha planilha

novos.nomes<-paste(row.names(dados), códigos, sep="-") # vou criar uma nova variável, função paste faz as duas coisas se juntarem emu ma só. A função é a seguinte  paste (row.names(dados)  que é os nomes na minha planilha carregamos a planilha de novo usando row.names=1, lembram? , códigos, sep=”-“)  ele dá os nomes associados com qual o ambiente que ela está associado.

# Até aqui eu crio um código com os nomes associados ao tipo de ambiente.

cluster.amb$labels<-novos.nomes # crio um objeto com uma parte do meu cluster original que é o cluste.amb$labels <- novos. Nomes que é o objeto que criei com os nomes associados com o tipo de ambiente.

cluster2.amb<-as.dendrogram(cluster.amb) # inform que o cluster2.amb é o cluster original na versão as.dendograma 

par(mar=c(3,2,1,5)) # ajustando o tamanho das margens
plot(cluster2.amb, horiz=T) # ploto e fica fácil de entender o que está acontecendo




##################################

##Outras medidas de distância

dim(dados) # me mostra as dimensões dos meus dados, ou seja quantas linhas e colunas eu tenho. No exemplo aqui 30 40, ou seja, 30 linha e 40 colunas.

especies<-dados[,7:40] # pegando os dados que vou prcisar para fazer a matriz e cluster. Da 7 em diante eu tenho abundância de espécies. Não precisa padronizar, pois todas elas estao sendo medidas em abundância, ou seja, a mesma unidade de medida! E seu eu tiver dados discrepantes? Não é a padronização que resolve, e sim a transformação. Posso usar a transformação por log, por exemplo. Nào vamos fazer isso aqui.

dist.jac<-vegdist(especies, method="jaccard", binary=T) # matriz de distancia com o coeficiente de Jaccard  a semalhança das minhas UA de acordo com a composição de espécies. A função vegdist tem metrícas mais ecológicas. Veja mais métricas em ?vegdist. Tem uma lista das métricas que ele tem e fala de cada uma. O pacote vegan tem uma documentação muito boa. Os nossos dados vejam eles tem abundâncias então eu tenho que dizer para o R que eles são binaries, logo uso binary=T. 

cluster.jac<-hclust(dist.jac, method="average") # vamos fazer o cluster. Method= “average” que igual ao UPGMA.

cluster.jac$labels<-novos.nomes # uso os novos nomes para já me mostrar se é mata 1a ou 2a.

par(mar=c(3,2,1,5)) # margem
plot(as.dendrogram(cluster.jac), horiz=T) # plot

# mostra as mesmas áreas de coleta, mas considera as áreas em relação a presence e ausência de espécies. A escala parte do ) e vai até 0.5, o limite dela é 1. A distância 0.1 de diferença significa que eles tem uma semlhança de 0.9, ou seja, quanto mais próximos do 0.0 mais semelhantes eles são. Tenho uma separação bem bacana de MP e MS, mas olha a danada da area4 aí de novo!!! Olha que divertido!!:) Me sugere que essa area4 ela é muito diferentona mesmo. Pois, tanto nos cluster quanto aqui ela está diferente do resto. Será que ela é mata 1a mesmo? Ou sera que ela é muito diferente?




# O ponto de separação na mata1a está no 0.48 e da 2a está no 0.35, Sendo que eu tenho um corte em MP, assim eu teria 3 grupos, ao invés de 2. Na mata 1aria o grau de diferença tá sempre mais para a esquerda, o que significa o que? O mais paraecido na mata 1a está em torno de 0.2 0.3 e da mata 2a entre 0.1 e 0.2, ou seja, quando eu pegar dois pontos de mata 2a e compara eles tem em media uma composição mais próxima do que comparando pontos da mata 1a. Mata 1a e 2a tendem a ser muito diferentes com exceçào da area4. Mata 1a tende a ser mais diferenças entre si do que a 2a. Isso está ligado a diversidade de espécies e heterogeneidade ambiental!!! Isso tudo é a mesm coisa que dizer que a mata 2a é mais homogenea na composição de espécies.

matriz.cof.jac<-cophenetic(cluster.jac) # fazendo a matriz coefenética.
cor(matriz.cof.jac, dist.jac) # fazendo o ceficiente de correlação cofenética

# o resultado foi 0.787, assim o cluster que interpretamos pode ser aceito, ele não difere muito da matriz original!!! 

#Com bray-curtis: vamos agora fazer por Bray-curtis. Eu levo em consideração a abundancia das espécies. 

dist.bray<-vegdist(especies, method="bray")
cluster.bray<-hclust(dist.bray, method="average")
cluster.bray$labels<-novos.nomes

par(mar=c(3,2,1,5))
plot(as.dendrogram(cluster.bray), horiz=T)

matriz.cof.bray<-cophenetic(cluster.bray) # fazendo a matriz coefenética.
cor(matriz.cof.bray, dist.bray) # fazendo o ceficiente de correlação cofenética

# mostra um pouco diferente do Jaccard.
