## SIMULADO

## Parte 1 ----
# 1. Carregue o arquivo `decisoes.rds` em um objeto chamado `decisoes`.


# 2. Separe a coluna `classe_assunto` em duas colunas, uma contendo a `classe` 
# e outra contendo o `assunto`


# 3. Elabore um data.frame em que as linhas sejam o assunto, as colunas sejam os anos e os valores 
# sejam as quantidades de decisões 
# Dica: agrupar por assunto e ano e fazer o spread


## Parte 2 ----

# 1. Caregue os pacotes "tidyverse", "survey" e "srvyr"


# 2. Leia o conjunto de dados 'api' do pacote survey usando o comando data(api)


# 3. Elimine os objetos 'apiclus1', 'apiclus2', 'apipop' e 'apistrat'
# mantendo apenas o objeto apisrs


# 4. crie o objeto 'pesos' selecionando as colunas 'stypr' e 'pw' do objeto 'apisrs'


# 5. crie o objeto 'escolas_notas' selecionando as colunas 
# 'cds', 'stype', 'api99' e 'api00' do objeto apisrs


# 6. Remova as duplicatas (linhas em duplicidade) do arquivo `pesos` que você criou no passo 4


# 7. Quantas linhas tem o novo objeto `pesos`, sem as duplicidades


# 8. Traga a variável `pw` para `escola_notas`, criando um novo objeto `amostra` 
# resultado da união (join) do objeto `pesos` ao objeto `escolas_notas` 
# dica use left_join, com `escola_notas` na esquerda.


# 9. Crie o objeto tbl_svy `amostra_expandida` expandindo a amostra aleatória simples (`amostra`)
# usando apenas a variável (coluna) "pw", contendo o peso amostral. 


# 10. Faça um gráfico de barras comparando a variação média 
# das notas de 1999 (`api99`) e 2000 (`api00`) por tipo de escola (`stype`) 
# utilize as estimativas intervalares para construir barras com o intervalo de confiança

#Exercício

#"objeto tbl_svy" significa um objeto que envelopa um dataframe com um desenho amostral

data(api) #Carrega os dados de exemplo do pacote survey data(api)

amostra_expandida <- apisrs %>% #apisrs é a amostra aleatória simples (esse conjunto de dados pra api já explica a amostra ("srs" = simple random sample))
  #a amostra_expandida é um objeto tbl_svy. Não tem data.frame visível aqui pois ele está envelopado dentro do objeto
  #se eu quiser ver apenas o data.frame? Há duas opções:
  #amostra_expandida$variables
  #amostra >%> data.frame()
  as_survey(weight = pw) %>% #Aqui estou dizendo "tr0=ate isso como um survey". É preciso informar o fpc=fpc ou o weight = pw
  #Estou EXPANDINDO a amostra aleatória simples por PESO (weight)
  mutate(nivel=case_when( # Criando variavel "nivel" que recebe Fundamental ou Medio a depender do valor da coluna stype.
    stype=="E"~"Fundamental", #se E, então Fundamental
    stype=="M"~"Fundamental", #se M, então Fundamental
    stype=="H"~"Medio"        #se H, então Médio
  ))

#Fica mais fácil criar um novo data.frame ANTES DO GRÁFICO, embora seja possível fazer dentro do pipe.
#Variação média de notas de 1999 e 2000 por nível.
#Crio um data.frame com as médias das diferenças das notas de 99 e 2000 por nível (Fundamental ou Médio)
#usando os intervalos (vartype = "ci")
variacao_media <- amostra_expandida %>%
  mutate(api_diff=api00 - api99) %>% #crio uma NOVA COLUNA com a diferença das notas dos anos 2000 - as notas do ano 1999(AO INVÉS DE COLOCAR DENTRO DO SURVEY_MEAN)
  group_by(nivel) %>% #se a minha COMPARAÇÃO está por nível, então preciso AGRUPAR POR NÍVEL.
  #eu terei linhas cuja singularidade será por nível.
  #No caso, então, terei duas linhas, pois são dois níveis: fundamental e médio.
  summarize(api_diff_media = survey_mean(api_diff, #quando eu quero A MÉDIA DE UMA VARIÁVEL QUANTITATIVA, uso survey_mean. EU PRECISO NOMEAR ESSA VARIÁVEL
                                         #quando a variável não for quantitativa, mas qualitativa, o survey_mean retorna PROPORÇÕES do group by.
                                         #se eu não coloco nada dentro do survey_mean, ele retorna PROPORÇÕES do group by.
                                         #o summarize vem sempre depois de um group_by
                                         #se em algum momento o summarize vem sem o group_by antes, eu sumarizo pelo data.frame inteiro. Sem ser por grupo.
                                         vartype = "ci")) #"ci" é o vartype para intervalos de confiança.
#no enunciado do exercício diz: "utilize as estimativas intervalares". POR ISSO USO "ci".

#ggplot
variacao_media %>% #A LÓGICA DO GGPLOT É IR ADICIONANDO QUANTAS CAMADAS EU QUISER NO GRÁFICO
  ggplot(aes(x = nivel, #dentro do ggplor eu coloco as estéticas (AES) de cor, forma. No meu eixo X eu quero nível.
             y = api_diff_media,  #no meu Y eu quero a diferença média.
             fill = nivel, #quero adicionar um preenchimento nas barras. FILL deixa as colunas cinzas
             color=nivel, #color gera cor para as barras. É NECESSÁRIO TER FILL para ter COLOR
             ymax = api_diff_media_upp, #preciso de ymax e ymin para construir uma barra de erro. _upp no máximo da variável criada anteriormente
             ymin = api_diff_media_low)) + #_low no mínimo da variável criada anteriormente
  #essas duas variáveis aqui em cima (api_diff_media_upp e api_diff_media_low) foram criadas ali no "summarize" quando especifiquei que meu vartype é "ci".
  #Automaticamente o survey_mean cria uma diferença máxima e uma diferença mínima
  geom_bar(stat = "identity", #aqui estamos contstruindo um gráfico de barras, ou seja, criando uma camada das barras. 
           #Construímoas as barras usando as estéticas que informamos antes.
           #stat = "identity" (como ele vai olhar o dado que entra). Ou seja, o tamanho das colunas vai ser exatamente o tamanho que está em Y
           #significa que eu estou falando que as estéticas serão idênticas em X e Y
           alpha = 0.6) + #é um outro atributo do geom_bar que significa "transparência". 
  #é só um embelezamento. Quanto menor o valor, mais clara fica a cor.
  #a próxima geometria que tem que entrar no gráfico é a geometria de barra de erro. Essa geometria demanda Y máximo e Y mínimo (que ficam fora e dentro da barra do gráfico, respectivamente).
  #Como eu defini a cor pelo nível no gráfico de barras, a barra de erro vai ter a cor do nível. Se não tiver a linha "color = algumacoisa", a barra de erro ficará preta.
  geom_errorbar(width = 0, #usamos aqui dois atributos que não são obrigatórios no geom_errorbar. width = zero deixa a largura do traço horizontal zerado, invisível.
                size=3) + #aqui indico que o traço vertical fica mais grosso
  geom_text(aes(label=round(api_diff_media,0)), #geom_text adiciona mais uma camada ao gráfico. Uma camada de texto.
            #label coloca nomes sobre as barras. Uso round para arredondar a média para 0 casas decimais.   
            color = "black", #aqui eu mudo a cor do label/do texto, que é um número, para PRETO.  
            size = 5, #aumento o tamanho do texto para 5 para ficar mais visível
            #o número preto fica exatamente no meio da barra porque puxou as definições da estética, ou seja, está colocando o Y exatamente aonde está o Y
            vjust = -0.5, #ajustes de posição vertical e 
            hjust = -0.5 #de posição horizontal
  ) 