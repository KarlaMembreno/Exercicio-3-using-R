
#UNIVERSIDADE FEDERAL DE PERNAMBUCO
#DEPARTAMENTO DE CI�NCIA POL�TICA
#AN�LISE DE DADOS
#PROF. Rodrigo Martins
#Aluna: Karla J. Membre�o, Lista#3
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
# Utilizando o banco world do pacote poliscidata, fa�a um  
# histograma que tamb�m indique a m�dia  e um boxplot 
# da vari�vel gini10
# Descreva o que voc� pode observar a partir deles.

#Criando o banco de dados
?world
banco_world<-world

#Construindo o histograma e indicando a m�dia

ggplot(banco_world,aes(gini10))+geom_histogram(color="blue", fill="blue")+geom_vline(aes(xintercept = mean(gini10, na.rm = T))
,color="red")

#O histograma mostra que os valores s�o muito variados. 
#Al�m disso na parte direita do histograma observamos pelo menos valores at�picos "64aprox" e "74 aprox".
#Por outro lado,podemos observar uma linha (vermelha) que separa os dados, essa linha indica a m�dia, 
#� dizer o centro da distribui��o da mostra. Podesse observar que para a vari�vel "gini10" a m�dia da distribui��o
#est� ubicada aproximadamente no valor 40. 

#Comprobando a m�dia para a vari�vel (gini10)
banco_world %>% select(gini10) %>% summarise(mean_= mean(gini10, na.rm = TRUE))
mean= 40.12941

# Construindo o boxplot (gini10)

ggplot(banco_world, aes(gini10)) +  geom_boxplot()

#Respeito aos dados representados no boxplot da vari�vel gini10,
#podemos observar que os dados s�o asim�tricos com dire��o � direita 
#(nos valores iniciais alguns paises n�o est�o muito pr�ximos da m�dia 
#e logo no final muitos mais pa�ses se afastam dos valores centrais 
#(ver linha da direita).
# Por sua vez, vemos que a a mairoia dos pa�ses 
#tem um indice de gini ubicado entre o valor 33 e 47 aproximadamente.
#No entanto, alguns pa�ses tem um indice de gini mais baixo que 20 
#e mais alto que 64 aproximadamente. 
#O valor outlier que mais se afasta dos dados 
#� o valor 74. 

##### EXERCICIO 2##### 
#Utilizando as fun��es de manipula��o de dados da aula passada,
# fa�a uma tabela que sumarize a media (fun��o mean), 
# mediana (funcao median) e o desvio padr�o (fundao sd) da 
# renda per capta (vari�vel gdppcap08), agrupada por tipo de regime 
# (vari�vel democ).
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

# Explique a diferen�a entre valores das m�dias e medianas.
# Ilustre com a explica��o com gr�fico de boxplot.

ggplot(banco_world, aes(x=gdppcap08, y=democ)) +  geom_boxplot(outlier.colour="red", outlier.shape = 14)+
geom_jitter(width = 0.2, shape=16, size=2)

#No boxplot podese observar que as maioria das democracias se concentram em um valor superior acima da sua
#mediana (linha central da caixa, 2do quartil), chegando at� o terceiro quartil(75% percentil) e presentando 
#dados melhor distribuidos do que nos regimes n�o democr�ticos, onde a mediana fica num desequilibrio respeito
#dos demais dados e os valores m�nimos e m�ximos est�o mais extrapolados. Nas democracias observamos apenas um 
#outlier e nas n�o democracias observamos pelo menos quatro.

# Os dados corroboram a hip�tese da rela��o entre democracia
# e desempenho economico?

# Sim. Para este conjunto de dados podese observar que a m�dia do �ndice de gini 
#dos pa�ses democr�ticos supera por muito a aqueles regimes que n�o s�o democr�ticos.
#Por outro lado, se avaliamos a mediana em ambos tipos de regimes vemos que esse valor
#que separa os dados pela mitade tamb�m � superior nas democracias do que nos regimes 
#n�o democr�ticos. Finalmente, se avaliamos o desvio padr�o deste conjunto de dados, 
#encontramos que regimes democr�ticos presentam maior uniformidade nos dados e que por
#tanto, tendem a se desviar muito menos do seu valor respeito � m�dia do que os regimes 
#n�o democr�ticos onde os dados est�o muito mais dispersos. Isto significa efetivamente 
#que o �ndice de gininos regimes n�o democr�ticos tende a ser muito mais desigual do que
#nos regimes democr�ticos e que por tanto, o desempenho econ�mico e a democracia guaram
#uma rela��o causal. 


##### EXERCICIO 3##### 
# Carregue o banco states que est� no pacote poliscidata 
# Mantenha apenas as vari�veis obama2012, conpct_m, hs_or_more,
# prcapinc, blkpct10, south, religiosity3, state

library(poliscidata)
?states
banco_states <-states

banco_states %>% select(obama2012, conpct_m, hs_or_more,prcapinc, blkpct10, south, religiosity3, state)


##### EXERCICIO 4##### 
# Carregue o banco nes que est� no pacote poliscidata
# Mantenha apenas as vari�veis obama_vote, ftgr_cons, dem_educ3,
# income5, black, south, relig_imp, sample_state
?nes
banco_nes<- nes
str(nes)

banco_nes %>% select (obama_vote, ftgr_cons, dem_educ3, income5, black, south, relig_imp, sample_state)


##### EXERCICIO 5##### 
# As vari�veis medem os mesmos conceitos, voto no obama em 2012, 
# conservadorismo, educa��o, renda, cor, norte-sul, 
# religiosidade e estado. A diferen�a � que o nes � um banco de
# dados com surveys individuais e o states � um banco de dados
# com informa��es por estado

# Fa�a um gr�fico para cada banco representando o n�vel de
# conservadorismo. Comente as conclus�es que podemos ter
# a partir deles sobre o perfil do eleitorado estadunidense.
# Para ajudar, voc�s podem ter mais informa��es sobre os bancos
# de dados digitando ?states e ?nes, para ter uma descri��o das
# vari�veis


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

# Nos dois banco de dados o n�vel de conservadurismo respeito da popula��o est� ubicado no segundo quartil 
#do gr�fico de violino. Por tanto, o perfil do eleitorado estadounidense indica que o 50% dele
# est� ubicado numa faixa media de conservadurismo, no entanto o 50% da outra parte do eleitorado
#se ubica en categorias extremas de conservadurismo(25% muito pouco conservadores e 25% extremadamente
# conservadores).

##### EXERCICIO 6##### 
# Qual � o tipo de gr�fico apropriado para descrever a vari�vel
# de voto em obama nos dois bancos de dados?
# Justifique e elabore os gr�ficos

#Para o banco de dados nes, as vari�veis a serem representadas s�o de tipo "Categ�rica Nominal", os valores
# ocupam uma categoria bivariada(1= para aqueles que sim votar�o no Obama e, 0= para aqueles que n�o o fizeram).
# Como s�o categorias que precisam se diferenciar, um gr�fico adequado � o "gr�fico de barra".

ggplot (banco_nes, aes (obama_vote))+ geom_bar()


# Para o banco de dados States as vari�veis a serem representadas s�o de tipo "Num�ricas" com escalas cont�nuas, 
#Dito isto, poder�amos fazer uso de um gr�fico de "densidade" pois permite observar o comportamento desses valores.

ggplot(banco_states, aes(x= obama2012))+geom_density(color="red")


##### EXERCICIO 7##### 
# Crie dois bancos de dados a partir do banco nes, um apenas com
# respondentes negros e outro com n�o-negros. A partir disso, fa�a
# dois gr�ficos com a propor��o de votos no obama.
# O que voc� pode afirmar a partir dos gr�ficos?
# Voc� diria que existe uma rela��o entre voto em Obama e cor?

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

#No. de negros que n�o votaram por Obama

negros_que_n�o<-banco_nes %>% select(black, obama_vote) %>%  filter(black=="Yes",na.rm = TRUE, obama_vote==0)

count(negros_que_n�o)
#17


# CRIANDO O BANCO DE DADOS "n�o negros"

n�o_negros<-banco_nes %>% select (black, obama_vote) %>%  filter(black=="No",na.rm = TRUE)
str(n�o_negros)
summary(n�o_negros)
head(n�o_negros)
count(n�o_negros)
#4,871

# Mais detalhes sobre n�o negros
# No. n�o negros que sim votaram por Obama

n�o_negros_que_sim <-banco_nes %>% select (black, obama_vote) %>%  filter(black=="No",na.rm = TRUE, obama_vote==1)
head(n�o_negros_que_sim)
count(n�o_negros_que_sim)
#1707

# No. n�o negros que n�o votaram por Obama
n�o_negros_que_n�o<-banco_nes %>% select (black, obama_vote) %>%  filter(black=="No",na.rm = TRUE, obama_vote==0)
head(n�o_negros_que_n�o)
count(n�o_negros_que_n�o)
#1771

#Criando os gr�ficos

#Para os negros
ggplot(negros, aes (obama_vote))+ geom_bar(color="black", fill="black")

#Para os n�o negros
ggplot(n�o_negros, aes (obama_vote))+ geom_bar(color="black", fill="white")
 
#Todas as observa��es 
ggplot(banco_nes, aes(black,obama_vote))+  geom_violin()
        
#RESPOSTA. Analisados os dados e gr�ficos podese concluir que os eleitores negros
#mostraram uma tendencia maior respeito do voto no Obama. Os n�o negros
# tiveram tendencias bem uniformes(quase fifty-fifty).

##### EXERCICIO 8##### 
# A partir do banco de dados states, fa�a uma compara��o semelhante.
# Fa�a um gr�fico com as porcentagens de votos em Obama para estados
# que est�o acima da mediana da porcentagem de popula��o negra nos estados,
# e outro gr�fico com as porcentagens de votos em Obama para os estados
# que est�o abaixo da mediana de popula��o negra.

pop_negra_acima_mean<-banco_states %>% select (blkpct10,obama2012)%>% filter(blkpct10 > mean(blkpct10))
str(pop_negra_acima_mean)

pop_negra_abaixo_mean<-banco_states %>% select (blkpct10,obama2012)%>% filter(blkpct10 < mean(blkpct10))
str(pop_negra_abaixo_mean)

#Criando os gr�ficos para cada grupo

ggplot(pop_negra_acima_mean, aes (blkpct10,obama2012))+ geom_boxplot (color="red", fill="gray")

ggplot(pop_negra_abaixo_mean, aes (blkpct10, obama2012))+ geom_boxplot (color="black", fill="white")

# O que voc� pode afirmar a partir dos gr�ficos?
# Podemos chegar a mesma conclus�o anterior?

#A mediana dos votos da popula��o negra estatal acima da m�dia � de 50%. Os Estados ubicados no segundo 
#e terceiro quartil prefiriram votar no Obama e s� uma pequena quantidade se encontram en valores diferentes. 
# No que respeita aos Estados com Popula��o n�o negra abaixo da m�dia, vemos que a maioria destes Estados
#est�o ubicados apenas no segundo quartil e s� uma pequena quantidade no terceiro quartil. Isto significa
#que Estados onde a popula��o est� abaixo da m�dia de negros votaram menos no Obama.
# Dito o anterior, as conclus�es feitas no banco "nes" tamb�m se confirmam no banco "states".


##### EXERCICIO 9##### 
# A partir da var�avel X do banco df abaixo

df <- data.frame(x = cos(seq(-50,50,0.5)))
count(df)  
str(df)

# Fa�a os tipos de gr�ficos que representem esse tipo de vari�vel
# comentando as diferen�as entre elas e qual seria a mais adequada

ggplot(df, aes(x))+ geom_boxplot()
ggplot(df, aes(x, ""))+ geom_violin()
ggplot(df, aes(x))+ geom_density()
ggplot(df, aes(x))+ geom_histogram(color= "white",fill="pink")

#Os gr�ficos criados para esta data frame s�o gr�ficos que podem ser usados para 
#representar vari�veis cuantitativas. No entanto, se quisermos escolher o melhor 
#entre eles teriamos que escolher o tipo histograma, pois neste caso estamos tratando
#com uma vari�vel cuantitativa continua e por tanto este gr�fico poderia representar
#melhor esses dados j� que oferece uma panorama bem detalhada da mostra.


##### EXERCICIO 10##### 
# responsa as quest�es te�ricas abaixo

#Esquema 1.2

#As formas de manipula��o eleitoral s�o diferentes entre pa�ses conforme o n�vel de democracia.
#Ou mais especificamente,

#O n�vel de democracia afeta o tipo de manipula��o eleitoral

#Vari�vel independente        Teoria               Vari�vel dependente
#(N�vel de democracia)__________Causal__________> (Manipula��o Eleitoral)
                                                 #Tipologia Schedler, 2013
      #                                                     #
      #                                                     # 
      #                                                     # Operacionaliza��o
      # Operacionaliza��o                                   #                                                       
      #                                                     #
      #
#Vari�vel independente __________Hip�tese__________> Vari�vel dependente mensurada
#mensurada
#(Classifica��o  The Economist)                     #(Relat�rios MOEs-OEA, Surveys, OGE)
#(0 a 3,99) -regimes autorit�rios,                   0= Se houve, 1=Se n�o houve
#( 4 a 5,99)- regimes h�bridos

#Como a hip�tese proposta � descritiva, usarei v�rios m�todos para lidar com os aspectos de validade
#e confiabilidade. Da mesma forma, para realizar o estudo usarei uma variedade de dados 
#de diferentes fontes de informa��o.

#Eu sei que a validade externa e confiabilidade s�o bem dif�cis de mensurar neste tipo de estudos.
#Se no futuro eu podesse estabelecer alguma outra medida eu farei. 
#No que respeita aos dados eles j� est�o disponbilizados mais n�o codificados ainda 
#(Eu terei que recopilar e criar a base de dados).







