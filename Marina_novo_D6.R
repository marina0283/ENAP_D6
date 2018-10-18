## Aula 1

install.packages(c("tidyverse","magrittr"))
install.packages("dplyr")
library(tidyverse)
library(magrittr)

# Reescreva a expressão abaixo utilizando o %>%: round(mean(divide_by(sum(1:10),3)),digits = 1)

round(mean(divide_by(sum(1:10),3)),digits = 1)

1:10 %>% #cria um vetor de 1 a 10
  sum() %>% #soma os valores do vetor
  divide_by(3) %>% #divide o resultado da soma anterior por 3
  mean() %>% #calcula a mÃ©dia da divisÃ£o anterior.
  round(1) #arredonda o nÃºmero com apenas uma casa decimal (ou round(digits = 1))

# 2. Sem rodar, diga qual a saida do codigo abaixo. Consulte o help das funções caso precise.

2 %>% #começa com o valor 2
  add(2) %>%  #adiciona 2 ao valor 2, resultando em 4
  c(6, NA) %>% #insere o valor 4 no inicio do vetor "6, NA"
  mean(na.rm = T) %>%  #calcula a mÃ©dia dos valores do vetor, desconsiderando "NA"
  equals(5) #retorna o resultado de 5 equals ( = ) 5, ou seja, TRUE


decisoes

glimpse(decisoes) #uma forma diferente de ver o conjunto de dados. É possiel saber que tipos de variaveis ha no meu conjunto de dados, visualizar os primeiros valores dessa variavel

#Cinco funções principais do dplyr:
#select: selecionar colunas
#filter: filtrar linhas
#mutate: criar colunas
#summarise: sumarizar colunas
#arrange: ordenar linhas

#antes do select, sempre chamar o pacote dplyr,uma vez que há essa função em diversos pacotes.

#SELECT
#Utilizar starts_with(x) -> começa com x
#ends_with(x) -> termina com x
#contais(x) -> coluna que contenha x
#matches(x) -> o valor é exatamente x
#one_of (x) -> um dos valores numa lista de vetores é x


decisoes %>%
  select(id_decisao, n_processo, municipio, juiz)

decisoes %>%
  select(id_decisao:municipio) #de id_decisao até municipio

decisoes_select <- decisoes %>%
  select(id_decisao, n_processo, municipio, juiz)


#selecionar as colunas que acabam com "cisao"

decisoes %>%
  select(ends_with("cisao"))

#tirar as colunas de texto = 'txt_decisao" e classe/assunto = 'classe_assunto'

decisoes %>%
  select(-txt_decisao, -classe_assunto)

#FILTER
#Se o select é selecionar variáveis, o filter é filtrar observações
#use "," ou "&" para "e" e "|" para "ou"
#condições separadas por vírgulas? É o mesmo que separar por &

decisoes %>%
  select (n_processo, id_decisao, municipio, juiz) %>%
  filter(municipio == 'São Paulo')


library(lubridate) #esse pacote serve para manusear data. É importante sempre chamar "library" porque é assim que se carrega um pacote

decisoes %>%
  select (id_decisao, municipio, data_decisao, juiz) %>%
  #município igual a Campinas ou Jaú, OU dia da decisão maior ou igual a 25
  filter(municipio %in% c('Campinas','Jaú') & day(dmy(data_decisao)) >= 25) #municipio "in" seguido de uma lista. 'day(dmy(data_decisao))' pega o dia da decisão. Para mês, month. Para ano, year.

decisoes %>%
  select (id_decisao, municipio, data_decisao, juiz) %>%
  #data maior ou igual a 01-01-2017 e menor ou igual a 01-01-2018
  filter(dmy(data_decisao) >= dmy("01-01-2017"),
         dmy(data_decisao) <= dmy("01-01-2018"))


#mais ação

decisoes %>%
  select(juiz) %>%
  #filtra juizes que têm 'Z' ou 'z' no nome
  filter (str_detect(juiz, regex("z", ignore_case = TRUE))) %>% #NÃO ENTENDI NADA
  #conta e ordena os juÃ�zes em ordem decrescente
  count(juiz, sort = TRUE) %>%
  head(5)


#filtrar apenas casos em que o id_decisao não é NA

decisoes %>%
  filter(!is.na(id_decisao)) #is.na é uma função que traz valores lógicos. A negação da função é !is.na

decisoes %>%
  filter(complete.cases(.)) #complete.cases() é uma função que traz apenas linhas em que todas as colunas possuem dados completos. O "." representa o conjunto de dados "decisoes"


#mutate em ação

decisoes %>%
  select (n_processo, data_decisao, data_registro) %>%
  mutate(tempo = dmy(data_registro) - dmy(data_decisao)) #mutate cria uma nova variável ou transforma o tipo do dado



#crie uma coluna binária "drogas" que vale TRUE se no texto da decisão algo é falado de drogas e FALSE caso contrário - dica: srt_detect


decisoes %>%
  filter(!is.na(txt_decisao)) %>%
  mutate(txt_decisao_minusculas = tolower(txt_decisao),  #tolower transforma tudo em minúsculo
         droga = str_detect(txt_decisao_minusculas, "droga|entorpecente|psicotrópico|maconha|haxixe|cocaína")) %>% #esse monte de "ou" funciona dentro das aspas, mesmo. O str é assim.
  dplyr::select(n_processo,droga)
#no mutate, se eu crio uma variável na linha acima, já posso usá-la na linha abaixo.



#summarise -> agrupar dados e transformar esses dados

decisoes %>%
  select(n_processo, municipio, data_decisao) %>%
  #pega ano da decisÃ£o
  mutate(ano_julgamento = year(dmy(data_decisao)),
         #pega o ano do processo 0057003-20,2017,8,26,0000 -> "2017"
         ano_proc = str_sub(n_processo,12,15), #substring! str_sub serve para pegar a informaÃ§Ã£o contida na posiÃ§Ã£o 12 atÃ© a posiÃ§Ã£o 15, ou seja, O ANO CONTIDO NO NÃMERO DO PROCESSO.
         #transforma o ano em inteiro
         ano_proc = as.numeric(ano_proc),
         #calcula o tempo em anos
         tempo_anos = ano_julgamento - ano_proc) %>% #calcula a quantidade de anos que demorou para um processo ser julgado (data da decisÃ£o - data do processo)
  group_by(municipio) %>%
  summarise (n = n(), #a funÃ§Ã£o "n" conta os registros.
             media_anos = mean(tempo_anos),
             min_anos = min(tempo_anos),
             max_anos = max(tempo_anos))



decisoes %>%
  filter(!is.na(txt_decisao)) %>%
  mutate(tamanho = str_length(txt_decisao)) %>%
  group_by(juiz) %>%
  summarise (n = n(),
             tamanho_mediana = median(tamanho)) %>% #mediana Ã© o nÃºmero de caracteres da decisÃ£o
  filter(n >= 10) %>%
  arrange(desc(tamanho_mediana)) %>% #arrange ordena a variÃ¡vel. É um "sort"
  head(5)


