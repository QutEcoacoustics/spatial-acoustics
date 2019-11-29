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


##SM4 Fieldwork - Reading the data - using the normalised table output from normilisingindices.R - after normalising and excluding highly correlated ones

df <- read.csv(getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SpectralIndices_Channel1", "SummaryIndices_Channel1_WindRemoved.csv"), row.names = 23)

#Loading necessary package

library(vegan)

# Para construer um cluster tem que ser por etapas. A 1a etapa é construir uma matriz de distância:

dist.matrix <- dist(df[c(2, 4:12, 14:15)], method = "euclidean")


cluster_summary_channel1 <- hclust(dist.matrix, method = "ward.D2")

plot(cluster_summary_channel1, hang=-1)

cut_avg <- cutree(cluster_summary_channel1, k = 20) #splitting the results into 20 clusters - arbitrary i looked the dendogram and simply decided this was a good number#

cluster_summary_channel1_df <- mutate(df, cluster = cut_avg) #assigning the cluster number to the df - now you'll be able to inspect#
 count(cluster_summary_channel1_df,cluster) #number of observations per cluster#

inspection_minutes4 <- filter(cluster_summary_channel1_df, cluster == "4") %>% 
  sample_n(., size = 10, replace = F) %>% 
  write.csv(., getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SummaryIndices_Channel1", "20ClusterInspection4_SummaryIndices.csv"))


write.csv(cluster_summary_channel1_df, getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SummaryIndices_Channel1", "ThirsClusterAnalysis_20clusters.csv"))

#correlation matrix between normalised indices
cor <- abs(cor(df[2:16], use = "complete.obs", method = "spearman")) %>% 
  write.csv(getDataPath("Fieldwork_Bowra", "Oct2019", "WindRemoval_SpectralIndices_Channel1", "correlationmatrix_afterwindremoval.csv"))
pairs(df[2:16])

library(ggplot2)
p <- ggplot(cluster_summary_channel1_df, aes(x = PointData, y = AcousticComplexity, color = factor(cluster))) +
  geom_jitter()
ggsave(p, "C:/Users/Nina Scarpelli/OneDrive - Queensland University of Technology/Documents/PhD/Project/Chapter1_FineScaleAcousticSurvey/FirstClusterAnalysis_6clusters.jpg")

library(ape)
plot(as.phylo(cluster_summary_channel1), type = "fan", tip.colors = )

# crio um objeto uso a função hclust (objeto da minha matriz, method = “average” uso average porque o método que eu quero para montar o cluster é o UPGMA, a distância é a media).

plot(cluster_summary_channel1, hang=-1) # para ver o cluster! hang = -1 serve para esticar os raminhos, sem ele fica esquisito demais, vamos fazer?!. 

# os numerous que aparecem no cladograma são os numerous das linhas (linha 1, linha 2, linha n).

# analisando o cluster eu posso ver que eu tenho, de cara, dois grandes agrupamentos. Mostrar os nós. Se você quiser você pode pedir para o R traçar linha e discutir. É um ponto de corte. Melhor é interpretar o que estamos enxergando. No eixo y o valor vai de 0 a 3.0, nesse caso, mas pode ir de 0 a infinito, depende da dist6ancia que você tem. Não é uma escala interpretável. 

library(MASS)
s1 <- sammon(cluster_summary_channel1)

# Melhorando o aspecto do seu cluster

#Cluster na horizontal: porque ele dá mais espaço para escrever nomes!

plot(as.dendrogram(cluster_summary_channel1), horiz=T) # eu transformer para um novo format e aceita o cluster na horizontal, que é bom para botar nomes. Fica mais legível!!). A interpretação não muda só o aspecto dele.
#Colocando os nomes das unidades amostrais: porque o r coloca os numerous das linhas, e eu preciso dos nomes das unidades amostrais.

#Desfazer e refazer a leitura dos dados: vou primeiro desler os dados e depois vou refazer a leitura 

detach(dados) # limpa a memoria do R

rm(dados) # remove o objeto se eu colocar o objeto aqui ele não conseguir ler.

dados<-read.table("insetos.txt", h=T, row.names=1) # vou ler os dados e colocar row.names=1, ou seja, a coluna 1 é o nome das minhas variáveis, gerlamente, a gente usa numerous para indicar nossas UA, mas se eu quero que as unidades tenha nomes eu posso fazer a coluna UA com o nome das nos unidades amostrais. Com esse comando o R vai entender que o nome das linhas do meu cluster deverá ser o que está escrito na primeira coluna!.

attach(dados)

#Refazendo as etapas, pois a numeração das colunas mudou: a primeira coluna é uma coluna de nomes, então a numeração das colunas mudou, assim a temperature não está mais na coluna 4 e sim na 3, certo?

var.amb<-dados[,3:6]

var.amb.pad<-decostand(var.amb, method="standardize")

dist.amb<-dist(var.amb.pad, method="euclid")

cluster.amb<-hclust(dist.amb, method="average")

plot(as.dendrogram(cluster.amb), horiz=T) # agora aparece com os nomes. Existe um comando para criar a janela maior, mas ele nem sempre é necessario.






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


# No gráfico acima vemos que temos um grupo de mata 1a e outro de 2a. Mas tem uma execão a area 4. O que sera que tem na area 4? Se fossem dados reais eu teria que ir ver o que aconteceu, pois claramente as variáveis ambientais separaram mata 1a de 2a. O que isso pode querer dizer? Bom pode querer dizer que ela tem característica diferentes, ou que eu errei ao classificá-la no campo, ou é uma mata 1a com carcterísticas diferentes. Tem mais coisas? Nada muito relevante. Meu agrupamento separa esses dois tipos de ambientes. Posso aplicar em várias situações diferentes. 

# Verificando o quanto o cluster de fato representa a matriz:

# quando eu faço a matriz reversa eu tenho uma matriz cofenética, ou seja, é a matriz a partir do cluste, no exemplo da aula teórica sria o seguinte: eu construiria uma matriz cofenética que teria o seguintes valores:


# A distância de A para B e de B para C sera o valor de 5.7, que é a media das dusa distâncias da matriz original, pois usamos o método de UPGMA, ou seja é a distância do B para o agrupamento A e C.
# Olha que sacada! Pode ser que nos meus dados a distância de um ponto para o outro varie muito e na hora que eu uso a media para fazer a distâncias dos agrupamentos haja muita discrepância. Então tanto o cluster quanto a minha matriz cofenética (que são a mesma coisa, né?), não representam muito bem a minha matriz original, certo? Ele tá distorcendo, porque usando as medias le não está representando nem uma coisa nem outra. Então eu posso fazer a distância simples ou a completa? Poderia, mas ele também vai distorcer. Então o que vai acontecer? Toda as vezes que eu olhar para o cluster eu tenho que perguntar, ele representa bem meus dados originais? Como sei isso? Eu calculo a matriz cofenética e calculo o quanto ela me se parece com a minha matriz original. Usando uma métrica de correlação (comando cor que é a correlação de Pearson). Se a matriz original e a cofenética são muito parecidas, com a correlação perto de 1, eu fico muito confiante que o cluster representa bem meus dados originais. Se não, eu não devo usar o cluster, poise le não irá representar meus dados reias. Para que serve o cluster? Só para representar a matriz graficamente! Quando eu olho a matriz é difícil de interpreter, aí uso o cluster para interpreter, se ele não representa a matriz original então eu não deveria usar ele para nada!



matriz.cof<-cophenetic(cluster.amb) # calculando a matriz cofenética, lembrando que é do cluster original. Ë uma outra matriz que foi baseada no cluster  é voltar o cluster para uma matriz.

cor(matriz.cof, dist.amb) # agora calculo o coeficiente de correlação cofenético  que é a correlação da matriz cofenética (que é o meu cluster) e da matriz original. Ou seja, é uma medida de 0 a 1 de quanto o cluster representou os seus dados.

# No nosso exemplo o valor foi 0.7386 

#Usualmente o corte é feito em 0.7  a maioria dos autores na literature usa esse valor, mas ele é subjetivo! Se o valor do seu coeficiente é 0.7 ou mais quer dizer que seu cluster representa bem sua matriz original, se não, não representa. Por outro lado se seu coeficiente de correlação cofenética ficou abaixo disso, significa que seu cluster não representa muito bem sua matriz original. Assim, você abandona o cluster. Não presta para nada! Aí o que eu faço? Aí posso usar uma PCA (análise de component principal), que é um pouco mais flexivel! O cluster tem uma interpretação intuitive, mas as vezes ele não presta para nada! 
# O CALCULA DO COEFICIENTE DE CORRELAÇÃO COFENÉTICO É OBRIGATÓRIO!!!! SEMPRE TEM QUE USAR! Se um cluster não tem esse coeficiente eu não sei se o cluster representa ou não os meus dados originais! 
# Hipótese da banca futura == você nunca sabe quem vai estar na banca!

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
