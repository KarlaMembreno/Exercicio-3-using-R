
#UNIVERSIDADE FEDERAL DE PERNAMBUCO
#DEPARTAMENTO DE CIÊNCIA POLÍTICA
#ANÁLISE DE DADOS
#PROF. Rodrigo Martins
#Aluna: Karla J. Membreño, Lista#3
##############################

#Instalando e carregando pacotes
install.packages("installr")
install.packages("tydiverse")
install.packages("poliscidata")
install.packages("hrbrthemes")
install.packages("scales")
library(poliscidata)
library(tidyverse)
library(hrbrthemes)
?gss
banco<-gss

####EXERCICIO 1######
# Utilizando o banco world do pacote poliscidata, faça um  
# histograma que também indique a média  e um boxplot 
# da variável gini10
# Descreva o que você pode observar a partir deles.

#Criando o banco de dados
?world
banco_world<-world

#Construindo o histograma e indicando a média

ggplot(banco_world,aes(gini10))+geom_histogram(color="blue", fill="blue")+geom_vline(aes(xintercept = mean(gini10, na.rm = T))
,color="red")

#O histograma mostra que os valores são muito variados. 
#Além disso na parte direita do histograma observamos pelo menos valores atípicos "64aprox" e "74 aprox".
#Por outro lado,podemos observar uma linha (vermelha) que separa os dados, essa linha indica a média, 
#é dizer o centro da distribuição da mostra. Podesse observar que para a variável "gini10" a média da distribuição
#está ubicada aproximadamente no valor 40. 

#Comprobando a média para a variável (gini10)
banco_world %>% select(gini10) %>% summarise(mean_= mean(gini10, na.rm = TRUE))
mean= 40.12941

# Construindo o boxplot (gini10)

ggplot(banco_world, aes(gini10)) +  geom_boxplot()

#Respeito aos dados representados no boxplot da variável gini10,
#podemos observar que os dados são asimétricos com direção à direita 
#(nos valores iniciais alguns paises não estão muito próximos da média 
#e logo no final muitos mais países se afastam dos valores centrais 
#(ver linha da direita).
# Por sua vez, vemos que a a mairoia dos países 
#tem um indice de gini ubicado entre o valor 33 e 47 aproximadamente.
#No entanto, alguns países tem um indice de gini mais baixo que 20 
#e mais alto que 64 aproximadamente. 
#O valor outlier que mais se afasta dos dados 
#é o valor 74. 

##### EXERCICIO 2##### 
#Utilizando as funções de manipulação de dados da aula passada,
# faça uma tabela que sumarize a media (função mean), 
# mediana (funcao median) e o desvio padrão (fundao sd) da 
# renda per capta (variável gdppcap08), agrupada por tipo de regime 
# (variável democ).
?world

banco_world %>%  group_by(democ) %>%
  summarise(media = mean(gdppcap08, na.rm = TRUE), 
            mediana = median(gdppcap08, na.rm = TRUE), 
            desvio = sd(gdppcap08, na.rm = TRUE))

#democ  media mediana desvio
#<fct>  <dbl>   <dbl>  <dbl>
#1 No     9243.    4388 14200.
#2 Yes   16351.   11660 15656.
#3 NA    30881    30881    NA 

# Explique a diferença entre valores das médias e medianas.
# Ilustre com a explicação com gráfico de boxplot.

ggplot(banco_world, aes(x=gdppcap08, y=democ)) +  geom_boxplot(outlier.colour="red", outlier.shape = 14)+
geom_jitter(width = 0.2, shape=16, size=2)

#No boxplot podese observar que as maioria das democracias se concentram em um valor superior acima da sua
#mediana (linha central da caixa, 2do quartil), chegando até o terceiro quartil(75% percentil) e presentando 
#dados melhor distribuidos do que nos regimes não democráticos, onde a mediana fica num desequilibrio respeito
#dos demais dados e os valores mínimos e máximos estão mais extrapolados. Nas democracias observamos apenas um 
#outlier e nas não democracias observamos pelo menos quatro.

# Os dados corroboram a hipótese da relação entre democracia
# e desempenho economico?

# Sim. Para este conjunto de dados podese observar que a média do índice de gini 
#dos países democráticos supera por muito a aqueles regimes que não são democráticos.
#Por outro lado, se avaliamos a mediana em ambos tipos de regimes vemos que esse valor
#que separa os dados pela mitade também é superior nas democracias do que nos regimes 
#não democráticos. Finalmente, se avaliamos o desvio padrão deste conjunto de dados, 
#encontramos que regimes democráticos presentam maior uniformidade nos dados e que por
#tanto, tendem a se desviar muito menos do seu valor respeito à média do que os regimes 
#não democráticos onde os dados estão muito mais dispersos. Isto significa efetivamente 
#que o índice de gininos regimes não democráticos tende a ser muito mais desigual do que
#nos regimes democráticos e que por tanto, o desempenho económico e a democracia guaram
#uma relação causal. 


##### EXERCICIO 3##### 
# Carregue o banco states que está no pacote poliscidata 
# Mantenha apenas as variáveis obama2012, conpct_m, hs_or_more,
# prcapinc, blkpct10, south, religiosity3, state

library(poliscidata)
?states
banco_states <-states

banco_states %>% select(obama2012, conpct_m, hs_or_more,prcapinc, blkpct10, south, religiosity3, state)


##### EXERCICIO 4##### 
# Carregue o banco nes que está no pacote poliscidata
# Mantenha apenas as variáveis obama_vote, ftgr_cons, dem_educ3,
# income5, black, south, relig_imp, sample_state
?nes
banco_nes<- nes
str(nes)

banco_nes %>% select (obama_vote, ftgr_cons, dem_educ3, income5, black, south, relig_imp, sample_state)


##### EXERCICIO 5##### 
# As variáveis medem os mesmos conceitos, voto no obama em 2012, 
# conservadorismo, educação, renda, cor, norte-sul, 
# religiosidade e estado. A diferença é que o nes é um banco de
# dados com surveys individuais e o states é um banco de dados
# com informações por estado

# Faça um gráfico para cada banco representando o nível de
# conservadorismo. Comente as conclusões que podemos ter
# a partir deles sobre o perfil do eleitorado estadunidense.
# Para ajudar, vocês podem ter mais informações sobre os bancos
# de dados digitando ?states e ?nes, para ter uma descrição das
# variáveis


library(scales)
library(ggbeeswarm)
?states
?nes

banco_states %>% summarise (media = mean(conpct_m, na.rm = TRUE),
                           minimo = min( conpct_m, na.rm = TRUE), 
                          maximo = max( conpct_m, na.rm = TRUE))

banco_nes %>% summarise (media= mean (ftgr_cons, na.rm = TRUE),
                         minimo = min( ftgr_cons, na.rm = TRUE), 
                         maximo = max( ftgr_cons, na.rm = TRUE))

#Nivel de conservadurismo banco de dados "states"
ggplot(banco_states, aes(conpct_m, "")) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))


#Nivel de conservadurismo banco de dados "nes"

ggplot(banco_nes, aes(ftgr_cons,""))+geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) 

# Nos dois banco de dados o nível de conservadurismo respeito da população está ubicado no segundo quartil 
#do gráfico de violino. Por tanto, o perfil do eleitorado estadounidense indica que o 50% dele
# está ubicado numa faixa media de conservadurismo, no entanto o 50% da outra parte do eleitorado
#se ubica en categorias extremas de conservadurismo(25% muito pouco conservadores e 25% extremadamente
# conservadores).

##### EXERCICIO 6##### 
# Qual é o tipo de gráfico apropriado para descrever a variável
# de voto em obama nos dois bancos de dados?
# Justifique e elabore os gráficos

#Para o banco de dados nes, as variáveis a serem representadas são de tipo "Categórica Nominal", os valores
# ocupam uma categoria bivariada(1= para aqueles que sim votarão no Obama e, 0= para aqueles que não o fizeram).
# Como são categorias que precisam se diferenciar, um gráfico adequado é o "gráfico de barra".

ggplot (banco_nes, aes (obama_vote))+ geom_bar()


# Para o banco de dados States as variáveis a serem representadas são de tipo "Numéricas" com escalas contínuas, 
#Dito isto, poderíamos fazer uso de um gráfico de "densidade" pois permite observar o comportamento desses valores.

ggplot(banco_states, aes(x= obama2012))+geom_density(color="red")


##### EXERCICIO 7##### 
# Crie dois bancos de dados a partir do banco nes, um apenas com
# respondentes negros e outro com não-negros. A partir disso, faça
# dois gráficos com a proporção de votos no obama.
# O que você pode afirmar a partir dos gráficos?
# Você diria que existe uma relação entre voto em Obama e cor?

# CRIANDO O BANCO DE DADOS "negros"

negros<-banco_nes %>% select (black,obama_vote)%>% filter(black=="Yes",na.rm=TRUE)
str(negros)
summary(negros)
head(negros)
count(negros)
#1016

# Mais detalhes sobre os negros
# No. negros que sim votaram por Obama

negros_sim<-banco_nes %>% select (black, obama_vote) %>%  filter(black=="Yes",na.rm = TRUE, obama_vote==1)

count(negros_sim) 
#781

#No. de negros que não votaram por Obama

negros_que_não<-banco_nes %>% select(black, obama_vote) %>%  filter(black=="Yes",na.rm = TRUE, obama_vote==0)

count(negros_que_não)
#17


# CRIANDO O BANCO DE DADOS "não negros"

não_negros<-banco_nes %>% select (black, obama_vote) %>%  filter(black=="No",na.rm = TRUE)
str(não_negros)
summary(não_negros)
head(não_negros)
count(não_negros)
#4,871

# Mais detalhes sobre não negros
# No. não negros que sim votaram por Obama

não_negros_que_sim <-banco_nes %>% select (black, obama_vote) %>%  filter(black=="No",na.rm = TRUE, obama_vote==1)
head(não_negros_que_sim)
count(não_negros_que_sim)
#1707

# No. não negros que não votaram por Obama
não_negros_que_não<-banco_nes %>% select (black, obama_vote) %>%  filter(black=="No",na.rm = TRUE, obama_vote==0)
head(não_negros_que_não)
count(não_negros_que_não)
#1771

#Criando os gráficos

#Para os negros
ggplot(negros, aes (obama_vote))+ geom_bar(color="black", fill="black")

#Para os não negros
ggplot(não_negros, aes (obama_vote))+ geom_bar(color="black", fill="white")
 
#Todas as observações 
ggplot(banco_nes, aes(black,obama_vote))+  geom_violin()
        
#RESPOSTA. Analisados os dados e gráficos podese concluir que os eleitores negros
#mostraram uma tendencia maior respeito do voto no Obama. Os não negros
# tiveram tendencias bem uniformes(quase fifty-fifty).

##### EXERCICIO 8##### 
# A partir do banco de dados states, faça uma comparação semelhante.
# Faça um gráfico com as porcentagens de votos em Obama para estados
# que estão acima da mediana da porcentagem de população negra nos estados,
# e outro gráfico com as porcentagens de votos em Obama para os estados
# que estão abaixo da mediana de população negra.

pop_negra_acima_mean<-banco_states %>% select (blkpct10,obama2012)%>% filter(blkpct10 > mean(blkpct10))
str(pop_negra_acima_mean)

pop_negra_abaixo_mean<-banco_states %>% select (blkpct10,obama2012)%>% filter(blkpct10 < mean(blkpct10))
str(pop_negra_abaixo_mean)

#Criando os gráficos para cada grupo

ggplot(pop_negra_acima_mean, aes (blkpct10,obama2012))+ geom_boxplot (color="red", fill="gray")

ggplot(pop_negra_abaixo_mean, aes (blkpct10, obama2012))+ geom_boxplot (color="black", fill="white")

# O que você pode afirmar a partir dos gráficos?
# Podemos chegar a mesma conclusão anterior?

#A mediana dos votos da população negra estatal acima da média é de 50%. Os Estados ubicados no segundo 
#e terceiro quartil prefiriram votar no Obama e só uma pequena quantidade se encontram en valores diferentes. 
# No que respeita aos Estados com População não negra abaixo da média, vemos que a maioria destes Estados
#estão ubicados apenas no segundo quartil e só uma pequena quantidade no terceiro quartil. Isto significa
#que Estados onde a população está abaixo da média de negros votaram menos no Obama.
# Dito o anterior, as conclusões feitas no banco "nes" também se confirmam no banco "states".


##### EXERCICIO 9##### 
# A partir da varíavel X do banco df abaixo

df <- data.frame(x = cos(seq(-50,50,0.5)))
count(df)  
str(df)

# Faça os tipos de gráficos que representem esse tipo de variável
# comentando as diferenças entre elas e qual seria a mais adequada

ggplot(df, aes(x))+ geom_boxplot()
ggplot(df, aes(x, ""))+ geom_violin()
ggplot(df, aes(x))+ geom_density()
ggplot(df, aes(x))+ geom_histogram(color= "white",fill="pink")

#Os gráficos criados para esta data frame são gráficos que podem ser usados para 
#representar variáveis cuantitativas. No entanto, se quisermos escolher o melhor 
#entre eles teriamos que escolher o tipo histograma, pois neste caso estamos tratando
#com uma variável cuantitativa continua e por tanto este gráfico poderia representar
#melhor esses dados já que oferece uma panorama bem detalhada da mostra.


##### EXERCICIO 10##### 
# responsa as questões teóricas abaixo

#Esquema 1.2

#As formas de manipulação eleitoral são diferentes entre países conforme o nível de democracia.
#Ou mais especificamente,

#O nível de democracia afeta o tipo de manipulação eleitoral

#Variável independente        Teoria               Variável dependente
#(Nível de democracia)__________Causal__________> (Manipulação Eleitoral)
                                                 #Tipologia Schedler, 2013
      #                                                     #
      #                                                     # 
      #                                                     # Operacionalização
      # Operacionalização                                   #                                                       
      #                                                     #
      #
#Variável independente __________Hipótese__________> Variável dependente mensurada
#mensurada
#(Classificação  The Economist)                     #(Relatórios MOEs-OEA, Surveys, OGE)
#(0 a 3,99) -regimes autoritários,                   0= Se houve, 1=Se não houve
#( 4 a 5,99)- regimes híbridos

#Como a hipótese proposta é descritiva, usarei vários métodos para lidar com os aspectos de validade
#e confiabilidade. Da mesma forma, para realizar o estudo usarei uma variedade de dados 
#de diferentes fontes de informação.

#Eu sei que a validade externa e confiabilidade são bem difícis de mensurar neste tipo de estudos.
#Se no futuro eu podesse estabelecer alguma outra medida eu farei. 
#No que respeita aos dados eles já estão disponbilizados mais não codificados ainda 
#(Eu terei que recopilar e criar a base de dados).







