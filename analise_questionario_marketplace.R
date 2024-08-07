# Marketplace survey 

#Instlando e carregando pacotes-------------------------------------------------
loadpackages = function(x){
  for(i in x){
    if(!require(i, character.only = TRUE)){
      install.packages(i, dependencies = TRUE)
      library(i, character.only = TRUE)
    }
  }
}

loadpackages(c("tidyverse","readxl","readr","scales","dplyr","wordcloud","tm","stringi",
               "widyr","gridExtra","highcharter","ggrepel","tidytext"))

## Analysis of closed questions------------------------------------------------

# Importação dos dados
# Por se tratar de dados de consultoria confidenciais, o caminho do banco de dados não será explicitado.
# No entanto, o banco de dados se chama 'data'

## Data subdivision ---------

questions <- data.frame(names = colnames(data)[1:21],
                        code = paste0('Q',1:21))

# Define uma função 'translation' para traduzir valores específicos de espanhol para inglês.
translation = function(x){
  return(
    case_when(
      x == 'Algunas veces' ~ 'Sometimes',
      x == 'SÃ­' ~ 'Yes',
      # x == 'No' ~ x,  # Este caso é comentado porque 'No' já é compreensível em inglês.
      x == 'As veces' ~ 'Sometimes',
      x == 'No me acuerdo' ~ 'I don`t remember',
      x == 'Tal vez' ~ 'Maybe',
      T ~ x  # Caso padrão que retorna o valor original se não houver correspondência.
    )
  )
}

# Seleciona as primeiras 21 colunas do dataframe 'data' e as atribui a 'qdata'.
qdata <- data %>% select(1:21)

# Renomeia as colunas de 'qdata' para 'Q1' a 'Q21'.
colnames(qdata) <- paste0('Q',1:21)

# Aplica a função de tradução para valores específicos de várias colunas de 'qdata'.
qdata <- qdata %>% 
  mutate(Q1 = translation(Q1),
         Q2 = translation(Q2),
         Q3 = translation(Q3),
         Q9 = translation(Q9),
         Q10 = translation(Q10),
         Q12 = translation(Q12),
         Q16 = translation(Q16),
         Q17 = translation(Q17),
         Q18 = translation(Q18),
         # Aplica uma tradução específica para valores da coluna 'Q4'.
         Q4 = case_when(
           Q4 == 'Sugerencias de productos y categorÃ­as especÃ­ficas' ~ 'Suggestions for specific products and categories',
           T ~ 'Any product from the Marketplace, regardless of category')  # Valor padrão para outros casos.
  )


BDRdata <- data %>% select(22:26)

N <- nrow(qdata)


# Graphs ----

## Fechadas ----
graf1 <-qdata %>% 
  group_by(Q1) %>% summarise(n=n()) %>% 
  ggplot(aes(x="", y=n, fill=Q1)) +
  geom_bar(stat="identity", width=1, color="black")+
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position = "bottom") +
  scale_fill_manual(values = '#4daf4a') +
  geom_text(aes(label = percent(n/N,accuracy = 1)),
            position = position_stack(vjust = .5))+
  labs(fill = '',
       title = 'Do you know where the "Marketplace" is located in app?')

graf2<-qdata %>% 
  group_by(Q2) %>% summarise(n=n()) %>% 
  ggplot(aes(x="", y=n, fill=Q2)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c('#4daf4a','#ff7f00','#e4211c')) +
  geom_text(aes(x=1.6,label = percent(n/N,accuracy = 1)),
            position = position_stack(vjust = .5)) +
  labs(fill = '',
       title = 'Do you have difficulty showing them how to acess the "Marketplace"?')

grid.arrange(graf1, graf2, ncol=2)

graf3<-qdata %>% 
  group_by(Q3) %>% summarise(n=n()) %>% 
  ggplot(aes(x="", y=n, fill=Q3)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c('#e4211c','#4daf4a')) +
  geom_text(aes(x=1.6,label = percent(n/N,accuracy = 1)),
            position = position_stack(vjust = .5)) +
  labs(fill = '',
       title = 'Do you receive Marketplace Tasks on your daily routes?')

graf4 <-qdata %>% 
  group_by(Q4) %>% summarise(n=n()) %>% 
  ggplot(aes(x="", y=n, fill=Q4)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position = "bottom")  +
  scale_fill_manual(values = c('#e4211c','#4daf4a')) +
  geom_text(aes(label = percent(n/N,accuracy = 1)),
            position = position_stack(vjust = .5)) +
  labs(fill = '',
       title = 'How do the Marketplace Tasks appear to you when you receive them?')

grid.arrange(graf3, graf4, ncol=2)


qdata %>% 
  group_by(Q7) %>% summarise(n=n()) %>% 
  ggplot()+
  geom_histogram(stat="identity",aes(x=Q7, y=n/N),
                 fill = 'darkgray', color = 'black')+
  theme_bw()+
  scale_x_continuous(breaks = 0:10,limits = c(0,10.5))+
  scale_y_continuous(limits = c(0,.3),labels = scales::percent)+
  labs(x = 'Answer', y = 'Frequency',
       title = 'How difficult is it to validate Marketplace Tasks in your opinion?(0-10)')

qdata %>% 
  group_by(Q9) %>% summarise(n=n()) %>% 
  ggplot(aes(x="", y=n, fill=Q9)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c('#4daf4a','#ff7f00','#e4211c')) +
  geom_text(aes(label = percent(n/N,accuracy = 1)),
            position = position_stack(vjust = .5)) +
  labs(fill = '',
       title = 'Do you encounter any kind of resistance from POCs when you try to sell products from the Marketplace?')

qdata %>% 
  group_by(Q10) %>% summarise(n=n()) %>% 
  ggplot(aes(x="", y=n, fill=Q10)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  theme_void()  +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c('#4daf4a','#ff7f00','#e4211c')) +
  geom_text(aes(label = percent(n/N,accuracy = 1)),
            position = position_stack(vjust = .5)) +
  labs(fill = '',
       title = 'Is the following sentence true for your experience? "POCs always give me a reason NOT to buy Marketplace products"')

qdata %>% 
  group_by(Q12) %>% summarise(n=n()) %>% 
  ggplot(aes(x="", y=n, fill=Q12)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  theme_void()  +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c('#e4211c','#4daf4a')) +
  geom_text(aes(x= 1.6,label = percent(n/N,accuracy = 1)),
            position = position_stack(vjust = .5)) +
  labs(fill = '',
       title = 'Do Marketplace Tasks impact your results and variable compensation?')


qdata %>% 
  group_by(Q14) %>% summarise(n=n()) %>% 
  ggplot()+
  geom_histogram(stat="identity",aes(x=Q14, y=n/N),
                 fill = 'darkgray', color = 'black')+
  theme_bw()+
  scale_x_continuous(breaks = 0:10,limits = c(0,10.5))+
  scale_y_continuous(limits = c(0,.31),labels = scales::percent)+
  labs(x = 'Answer', y = 'Frequency',
       title = 'From 0 to 10, how prepared do you feel to carry out and complete your Marketplace Tasks?')


qdata %>% 
  group_by(Q16) %>% summarise(n=n()) %>% 
  ggplot(aes(x="", y=n, fill=Q16)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  theme_void()  +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c('#ff7f00','#e4211c','#4daf4a')) +
  geom_text(aes(x=1.6,label = percent(n/N,accuracy = 1)),
            position = position_stack(vjust = .5)) +
  labs(fill = '',
       title = 'Have you been trained about Marketplace and its products by your manager?')


qdata %>% 
  group_by(Q17) %>% summarise(n=n()) %>% 
  ggplot(aes(x="", y=n, fill=Q17)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  scale_fill_manual(values = c('#ff7f00','#e4211c','#4daf4a')) +
  geom_text(aes(label = percent(n/N,accuracy = 1)),
            position = position_stack(vjust = .5)) +
  labs(fill = '',
       title = 'Do you think that Marketplace training would help you validate more tasks and have a more positive impact on your results?')

qdata %>% 
  group_by(Q18) %>% summarise(n=n()) %>% 
  ggplot(aes(x="", y=n, fill=Q18)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  scale_fill_manual(values = c('#ff7f00','#e4211c','#4daf4a')) +
  geom_text(aes(label = percent(n/N,accuracy = 1)),
            position = position_stack(vjust = .5)) +
  labs(fill = '',
       title = 'Would you like to receive new Marketplace trainings to deepen your knowledge?')

## Analysis of open questions ----------------------------------------------------

# O processo de tratamento e análise de dados envolve substituir caracteres especiais,
# remover espaços extras e padronizar termos. Depois, os textos são transformados em um corpus, 
# onde são limpos de números e palavras irrelevantes. Uma matriz de termos-documentos é criada 
# para contar a frequência dos termos. Finalmente, são geradas visualizações como nuvem de palavras
# e histograma para identificar os produtos mais mencionados no "Marketplace"

textos <- qdata$Q5
textos <- str_replace_all(textos,","," ")
textos <- str_replace_all(textos,"\\."," ")
textos <- str_squish(textos)
textos <- tolower(textos)
textos <- str_replace_all(textos,"tiempo aire","tiempo_aire")
textos <- str_replace_all(textos,"buull","bull")
textos <- str_replace_all(textos,"red bull","red_bull")
textos <- str_replace_all(textos,"redbull","red_bull")
textos <- str_replace_all(textos,"viña","vino")
textos <- str_replace_all(textos,"vinos","vino")
textos <- str_replace_all(textos,"nestlé","nestle")
textos <- str_replace_all(textos,"cigarros","cigarro")
textos <- str_replace_all(textos,"todos","todas")
textos <- str_replace_all(textos,"todo","todas")
textos <- str_replace_all(textos,"existencias","todas")
textos <- str_replace_all(textos,"existencia","todas")
textos <- str_replace_all(textos,"existen","todas")
textos <- str_replace_all(textos,"tequilasos","tequilas")
textos <- str_replace_all(textos,"mayonesas","mayonesa")
textos <- str_replace_all(textos,"disponibles","disponible")
textos <- str_replace_all(textos,"dispobibles","disponible")
textos <- str_replace_all(textos,"electrolitos","electrolit")
textos <- str_replace_all(textos,"bebidas preparadas","bebidas_preparadas")
textos <- str_replace_all(textos,"hidratantes","hidratante")
textos <- str_replace_all(textos,"sopas","sopa")

corp = VCorpus(VectorSource(textos), readerControl = list(language = "spanish"))
corp <- corp %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace)
corp <- tm_map(corp, content_transformer(tolower))
corp <- tm_map(corp, removeWords, stopwords("spanish"))

dtm <- TermDocumentMatrix(corp)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

df <- df %>% 
  filter(!word %in% c('vendo','etx','etc','bat','productos','estan','comprar'))
# agua, agua nestle e agua pureza

df %>%
  filter(row_number(desc(freq)) <= 20) %>%
  hchart("wordcloud",
         hcaes(name = word, weight = freq))

df %>% 
  filter(freq > 3) %>% 
  arrange(freq) %>% 
  ggplot()+
  geom_histogram(stat="identity",aes(x=reorder(word, -freq), y=freq),
                 fill = 'darkgray', color = 'black')+
  theme_bw()+
  labs(x = 'Terms', y = 'Mentions',
       title = 'In the Marketplace Tasks to offer any product, what are the products that you offer to the POC?',
       subtitle = 'Terms mentioned more than three times')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


### Why do you offer these products?

# O tratamento de dados neste código começa com a criação de um dataframe contendo 
# as respostas à pergunta "Como você acha que podemos melhorar o treinamento?". 
# Em seguida, são removidas as palavras irrelevantes (stopwords), convertidos os 
# textos para maiúsculas e retirados caracteres especiais. O texto é dividido em tokens, 
# com a remoção de valores nulos. As co-ocorrências de palavras em pares são então contadas, 
# criando expressões compostas. Finalmente, as 30 expressões mais frequentes são visualizadas
# em uma nuvem de palavras, destacando as combinações mais comuns nas respostas.

palavras <- qdata$Q6

palavras = tibble(ID = 1:length(palavras),
                  `Como você acha que podemos melhorar o treinamento?` = palavras)

my_stopwords = tibble(word = c("de", "con", "en", "a", "la", "y","es",
                               "un", "mas", "para","el","los","al","nos",
                               "que","una","las","lo","más","q","estos",
                               "este","xq","no","por","se","my"))
palavras = tibble(id = palavras$ID,
                  keyword = palavras$`Como você acha que podemos melhorar o treinamento?`
)

palavras$keyword <- stri_trans_general(palavras$keyword, "Latin-ASCII")

palavras = palavras %>%
  mutate(keyword = toupper(keyword)) %>%
  unnest(keyword) %>%
  na.omit() %>%
  unnest_tokens(word, keyword) %>%
  anti_join(my_stopwords)

palavras = palavras %>%
  pairwise_count(word, id, sort = TRUE, upper = FALSE) %>%
  mutate(expressao = paste(item1,item2))

palavras %>%
  filter(row_number(desc(n)) <= 30) %>%
  hchart("wordcloud",
         hcaes(name = expressao, weight = n))



### Why did you choose that option for the difficulty of validating Marketplace Tasks?

# fácil

palavras <- (qdata %>% filter(Q7<=5))$Q8

palavras = tibble(ID = 1:length(palavras),
                  `Como você acha que podemos melhorar o treinamento?` = palavras)

my_stopwords = tibble(word = c("de", "con", "en", "a", "la", "y","es",
                               "un", "mas", "para","el","los","al","nos",
                               "que","una","las","lo","más","q","estos",
                               "este","xq","no","por","se","my"))
palavras = tibble(id = palavras$ID,
                  keyword = palavras$`Como você acha que podemos melhorar o treinamento?`
)

palavras$keyword <- stri_trans_general(palavras$keyword, "Latin-ASCII")

palavras = palavras %>%
  mutate(keyword = toupper(keyword)) %>%
  unnest(keyword) %>%
  na.omit() %>%
  unnest_tokens(word, keyword) %>%
  anti_join(my_stopwords)

palavras = palavras %>%
  pairwise_count(word, id, sort = TRUE, upper = FALSE) %>%
  mutate(expressao = paste(item1,item2))

palavras %>%
  filter(row_number(desc(n)) <= 30) %>%
  hchart("wordcloud",
         hcaes(name = expressao, weight = n))

# dificíl

palavras <- (qdata %>% filter(Q7>5))$Q8

palavras = tibble(ID = 1:length(palavras),
                  `Como você acha que podemos melhorar o treinamento?` = palavras)

my_stopwords = tibble(word = c("de", "con", "en", "a", "la", "y","es",
                               "un", "mas", "para","el","los","al","nos",
                               "que","una","las","lo","más","q","estos",
                               "este","xq","no","por","se","my"))
palavras = tibble(id = palavras$ID,
                  keyword = palavras$`Como você acha que podemos melhorar o treinamento?`
)

palavras$keyword <- stri_trans_general(palavras$keyword, "Latin-ASCII")

palavras = palavras %>%
  mutate(keyword = toupper(keyword)) %>%
  unnest(keyword) %>%
  na.omit() %>%
  unnest_tokens(word, keyword) %>%
  anti_join(my_stopwords)

palavras = palavras %>%
  pairwise_count(word, id, sort = TRUE, upper = FALSE) %>%
  mutate(expressao = paste(item1,item2))

palavras %>%
  filter(row_number(desc(n)) <= 30) %>%
  hchart("wordcloud",
         hcaes(name = expressao, weight = n))



### What is the main reason, answer or argument that POCs give you when they don`t want to buy Marketplace products?

palavras <- qdata$Q11
palavras = tibble(ID = 1:length(palavras),
                  `Como você acha que podemos melhorar o treinamento?` = palavras)

my_stopwords = tibble(word = c("de", "con", "en", "a", "la", "y","es",
                               "un", "mas", "para","el","los","al","nos",
                               "que","una","las","lo","más","q","estos",
                               "este","xq","no","por","se","my","estan"))
palavras = tibble(id = palavras$ID,
                  keyword = palavras$`Como você acha que podemos melhorar o treinamento?`
)

palavras$keyword <- stri_trans_general(palavras$keyword, "Latin-ASCII")

palavras = palavras %>%
  mutate(keyword = toupper(keyword)) %>%
  unnest(keyword) %>%
  na.omit() %>%
  unnest_tokens(word, keyword) %>%
  anti_join(my_stopwords)

palavras = palavras %>%
  pairwise_count(word, id, sort = TRUE, upper = FALSE) %>%
  mutate(expressao = paste(item1,item2))

palavras %>%
  filter(row_number(desc(n)) <= 30) %>%
  hchart("wordcloud",
         hcaes(name = expressao, weight = n))


### How do Marketplace Tasks impact your variable compensation?

palavras <- qdata$Q13

palavras = tibble(ID = 1:length(palavras),
                  `Como você acha que podemos melhorar o treinamento?` = palavras)

my_stopwords = tibble(word = c("de", "con", "en", "a", "la", "y","es",
                               "un", "mas", "para","el","los","al","nos",
                               "que","una","las","lo","más","q","estos",
                               "este","xq","no","por","se","my"))
palavras = tibble(id = palavras$ID,
                  keyword = palavras$`Como você acha que podemos melhorar o treinamento?`
)

palavras$keyword <- stri_trans_general(palavras$keyword, "Latin-ASCII")

palavras = palavras %>%
  mutate(keyword = toupper(keyword)) %>%
  unnest(keyword) %>%
  na.omit() %>%
  unnest_tokens(word, keyword) %>%
  anti_join(my_stopwords)

palavras = palavras %>%
  pairwise_count(word, id, sort = TRUE, upper = FALSE) %>%
  mutate(expressao = paste(item1,item2))

palavras %>%
  filter(row_number(desc(n)) <= 30) %>%
  hchart("wordcloud",
         hcaes(name = expressao, weight = n))


### Please choose why you chose that option.

# despreparado

palavras <- (qdata %>% filter(Q14<8))$Q15

palavras = tibble(ID = 1:length(palavras),
                  `Como você acha que podemos melhorar o treinamento?` = palavras)

my_stopwords = tibble(word = c("de", "con", "en", "a", "la", "y","es",
                               "un", "mas", "para","el","los","al","nos",
                               "que","una","las","lo","más","q","estos",
                               "este","xq","no","por","se","my"))
palavras = tibble(id = palavras$ID,
                  keyword = palavras$`Como você acha que podemos melhorar o treinamento?`
)

palavras$keyword <- stri_trans_general(palavras$keyword, "Latin-ASCII")

palavras = palavras %>%
  mutate(keyword = toupper(keyword)) %>%
  unnest(keyword) %>%
  na.omit() %>%
  unnest_tokens(word, keyword) %>%
  anti_join(my_stopwords)

palavras = palavras %>%
  pairwise_count(word, id, sort = TRUE, upper = FALSE) %>%
  mutate(expressao = paste(item1,item2))

palavras %>%
  filter(row_number(desc(n)) <= 30) %>%
  hchart("wordcloud",
         hcaes(name = expressao, weight = n))

# preparado

palavras <- (qdata %>% filter(Q14>=8))$Q15

palavras = tibble(ID = 1:length(palavras),
                  `Como você acha que podemos melhorar o treinamento?` = palavras)

my_stopwords = tibble(word = c("de", "con", "en", "a", "la", "y","es",
                               "un", "mas", "para","el","los","al","nos",
                               "que","una","las","lo","más","q","estos",
                               "este","xq","no","por","se","my"))
palavras = tibble(id = palavras$ID,
                  keyword = palavras$`Como você acha que podemos melhorar o treinamento?`
)

palavras$keyword <- stri_trans_general(palavras$keyword, "Latin-ASCII")

palavras = palavras %>%
  mutate(keyword = toupper(keyword)) %>%
  unnest(keyword) %>%
  na.omit() %>%
  unnest_tokens(word, keyword) %>%
  anti_join(my_stopwords)

palavras = palavras %>%
  pairwise_count(word, id, sort = TRUE, upper = FALSE) %>%
  mutate(expressao = paste(item1,item2))

palavras %>%
  filter(row_number(desc(n)) <= 30) %>%
  hchart("wordcloud",
         hcaes(name = expressao, weight = n))

