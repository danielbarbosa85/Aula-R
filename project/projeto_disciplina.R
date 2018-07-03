#Nomes: Daniel de Campos Barbosa e Guilherme Miranda da Silva
# Descrição dos dados: https://tech.instacart.com/3-million-instacart-orders-open-sourced-d40d29ead6f2
# Estamos trabalhando com somente uma amostra do total de pedidos. O dataset abaixo não possui 3 milhões de pedidos ;)
library( tidyverse )
library(ggplot2)

departments <- read_csv("project/departments.csv")                   # Cadastro de Departamentos
aisles <- read_csv("project/aisles.csv")                             # Cadastro de "Corredores"
products <- read_csv("project/products.csv")                         # Cadastro de Produtos

insta_orders <- read_csv( "project/orders_instacart.csv" )           # Amostra de pedidos de usuários
insta_products <- read_csv( "project/order_products_instacart.csv" ) # Produtos que compõe os pedidos


#1 # Quantos dos produtos do cadastro nunca foram comprados?
products %>%
  anti_join(insta_products, by="product_id") %>%
  count()

#2 # Crie um dataframe com os dados combinados de produtos, corredores e departamentos. 
prod_depart_aisles <-
  products %>%
  left_join(departments, by="department_id") %>%
  left_join(aisles, by="aisle_id")

#3 # Quais as 10 combinações corredor + departamento que possuem mais produtos cadastrados? Use o dataframe da atividade #2.
prod_depart_aisles %>%
  count(aisle_id, department_id, aisle, department) %>%
  arrange(desc(n)) %>%
  head(10) -> top_aisle_depart

(top_aisle_depart)


#4 # Qual o percentual de pedidos que possuem algum produto dos pares 'corredor + departamento' da atividade anterior?
products_orders <- insta_orders %>%
  left_join(insta_products, by="order_id") %>%
  left_join(products, by="product_id")

products_orders %>%
  mutate(in_top = department_id %in% pull(top_aisle_depart, department_id)) %>%
  filter(in_top == TRUE) %>%
  distinct(order_id) %>%
  select(order_id) -> orders_in_top

resposta <- orders_in_top %>% count() / insta_orders %>% count() * 100

paste("Percentual de pedidos que possuem algum produto dos pares 'corredor + departamento'", resposta, "%")

#5 # Crie um novo dataframe de produtos em pedidos retirando aqueles produtos que não estão categorizados (usar resultado das atividades 3 e 4)

orders_products <- prod_depart_aisles %>% 
  filter(aisle != 'missing' | department != 'missing') %>%
  left_join(insta_products, by="product_id") %>%
  left_join(insta_orders, by="order_id") %>%
  select(product_id, product_name, aisle_id, aisle, department_id, department, order_id, user_id)


#6 # Crie um dataframe que combine todos os dataframes através das suas chaves de ligação. Para produtos de pedidos, use o dataframe da atividade 4
# Transforme as variáveis user_id, department e aisle em factor
# Transforme a variável order_hour_of_day em um factor ordenado (ordered)

# Este dataframe deverá ser utilizado em todas as atividades seguintes

insta_dataset <- products_orders %>%
  left_join(departments, by="department_id") %>%
  left_join(aisles, by="aisle_id") %>%
  mutate(user_id = factor(user_id),
         department = factor(department),
         aisle = factor(aisle)) %>%
  mutate(order_hour_of_day = factor(order_hour_of_day, ordered = TRUE))


#7 # Identifique os 5 horários com maior quantidade de usuários que fizeram pedidos
insta_dataset %>%
  count(order_hour_of_day) %>%
  arrange(desc(n)) %>%
  head(5) %>%
  select(order_hour_of_day) -> top_hours

#8 # Quais os 15 produtos mais vendidos nestes 5 horários? Identifique os produtos e a quantidade total nestes horários (total geral, não por hora)
insta_dataset %>% 
  filter(order_hour_of_day %in% pull(top_hours)) %>% 
  count(product_id, product_name) %>%
  arrange(desc(n)) %>%
  head(15) %>%
  mutate(quantidade_total = n) %>%
  select(product_id, product_name, quantidade_total) -> top_15_products

#9 # Calcule a média de vendas por hora destes 15 produtos ao longo do dia,
# e faça um gráfico de linhas mostrando a venda média por hora destes produtos. 
# Utilize o nome do produto para legenda da cor da linha.
# Você consegue identificar algum produto com padrão de venda diferente dos demais? 

insta_dataset %>%
  filter(product_id %in% pull(top_15_products %>% select(product_id))) %>%
  count(product_name, order_hour_of_day, order_dow) %>%
  group_by(product_name, order_hour_of_day) %>%
  summarise(media = mean(n)) %>%
  ungroup() %>%
  ggplot(aes(x = order_hour_of_day, y = media, group = product_name, colour = product_name, shape = product_name)) +
  geom_line() +
  ggtitle("Média de Vendas por Hora") +
  xlab("Horas") +
  ylab("Média de vendas")

paste("OS 15 produtos seguem o mesmo padrão de média, aumentando a partir das 5 horas, tendo a maior media entre os horários de 10 as 16, e depois começar a diminuir.")



#10 # Calcule as seguintes estatísticas descritivas sobre a quantidade de pedidos por dia, para cada hora do dia. O resultado final deve ser exibido para cada hora do dia:
# Média, Desvio Padrão, Mediana, Mínimo e Máximo
# Considerando os valores calculados, você acredita que a distribuição por hora é gaussiana? 

insta_dataset %>%
  count(order_dow, order_hour_of_day) %>%
  group_by(order_hour_of_day) %>%
  summarise(media = mean(n),
            desvio_padrao = sd(n),
            mediana = median(n),
            minimo = min(n),
            maximo = max(n)) %>%
  ungroup() -> products_by_hour

products_by_hour %>% View()

paste("Analisando os dados acreditamos que a distribuição é gaussiana, pois a média tem apenas um pico, as 14 horas,
      e analisando os outros dados aparenta gerar um curva normal.")

#11 # Faça um gráfico da média de quantidade de produtos por hora, com 1 desvio padrão para cima e para baixo em forma de gráfico de banda
products_by_hour %>%
  ggplot(aes(x = order_hour_of_day, y = media, group = "order_hour_of_day", ymin = media - desvio_padrao, ymax = media + desvio_padrao)) +
  geom_ribbon(fill = "lightgray", alpha = 0.7) +
  geom_line(color = "red") +
  ggtitle("Média de Quantidade de Produtos por Hora") +
  xlab("Horas") +
  ylab("Média")

#12 # Visualize um boxplot da quantidade de pedidos por hora nos 7 dias da semana. O resultado deve ter order_dow como eixo x.

insta_dataset %>%
  distinct(order_id, order_dow, order_hour_of_day) %>%
  count(order_dow, order_hour_of_day) %>%
  ggplot(aes(x = order_dow, y = n, group = order_dow)) +
  geom_boxplot() + 
  scale_x_continuous( breaks = 0:6 ) +
  scale_y_continuous( breaks = seq(from = 0, to = 2500, by = 250 )) +
  labs( x = "Dias da Semana"
        , y = "Quantidade de Pedidos por Hora"
        , title = "Boxplot da Quantidade de Pedidos por Hora") +
  theme_bw()


#13 # Identifique, por usuário, o tempo médio entre pedidos

insta_dataset %>%
  group_by(user_id) %>%
  summarise(tempo_medio = mean(days_since_prior_order)) %>%
  ungroup() -> tempo_medio_entre_pedidos


#14 # Faça um gráfico de barras com a quantidade de usuários em cada tempo médio calculado

tempo_medio_entre_pedidos %>%
  ggplot(aes(x = tempo_medio)) +
  geom_bar(fill = "green", color = "green", alpha = 0.6) +
  scale_x_continuous( breaks = seq( from = 0, to = 30, by = 3 ) ) +
  scale_y_continuous( breaks = seq( from = 0, to = 40000, by = 2500 )) +
  ggtitle("Quantidade de Usuários por Tempo Médio entre Compras") +
  xlab("Tempo Médio") +
  ylab("Quantidade de Usuários")

#15 # Faça um gráfico de barras com a quantidade de usuários em cada número de dias desde o pedido anterior. Há alguma similaridade entre os gráficos das atividades 14 e 15? 

insta_dataset %>%
  distinct(user_id, days_since_prior_order) %>%
  ggplot(aes(x = days_since_prior_order)) +
  geom_bar(fill = "blue", color = "blue", alpha = 0.6) +
  ggtitle("Quantidade de Usuários por Tempo entre Compras") +
  scale_x_continuous( breaks = seq( from = 0, to = 30, by = 3 ) ) +
  scale_y_continuous( breaks = seq( from = 0, to = 40000, by = 2500 )) +
  xlab("Tempo (em Dias)") +
  ylab("Quantidade de Usuários")

paste("Os gráficos das atividades 14 e 15 seguem o mesmo padrão.")

#16 # Repita o gráfico da atividade 14 mantendo somente os usuários com no mínimo 5 pedidos. O padrão se mantém?

insta_orders %>% select(user_id) %>% count()
insta_orders %>% distinct(user_id) %>% count()
paste("A quantidate de user_id e distinct(user_id) é igual na tabela insta_orders.")

paste("Como não foi localizado mais de um order_id para cada usuário, será considerado o order_number a quantidade de pedidos.")

insta_dataset %>% 
  group_by(user_id) %>%
  summarise(qtd_pedidos = max(order_number)) %>%
  ungroup() %>%
  filter(qtd_pedidos >= 5) %>%
  select(user_id) -> usuario_com_mais_de_4_pedidos

insta_dataset %>%
  filter(user_id %in% pull(usuario_com_mais_de_4_pedidos)) %>%
  group_by(user_id) %>%
  summarise(tempo_medio = mean(days_since_prior_order)) %>%
  ungroup() %>%
  ggplot(aes(x = tempo_medio)) +
  geom_bar(fill = "green", color = "green", alpha = 0.6) +
  scale_x_continuous( breaks = seq( from = 0, to = 30, by = 3 ) ) +
  scale_y_continuous( breaks = seq( from = 0, to = 40000, by = 2500 )) +
  ggtitle("Quantidade de Usuários por Tempo Médio entre Compras") +
  xlab("Tempo Médio") +
  ylab("Quantidade de Usuários")

paste("Sim, o padrão se mantém.")

#17 # O vetor abaixo lista todos os IDs de bananas maduras em seu estado natural.
# Utilizando este vetor, identifique se existem pedidos com mais de um tipo de banana no mesmo pedido.
bananas <- c(24852, 13176, 39276, 37067, 29259)

insta_dataset %>%
  filter(product_id %in% bananas) %>%
  count(order_id) %>%
  filter(n > 1) %>%
  select(order_id)-> orders_with_more_one_type_bananas

#18 # Se existirem, pedidos resultantes da atividade 17, conte quantas vezes cada tipo de banana aparece nestes pedidos com mais de um tipo de banana.
# Após exibir os tipos de banana, crie um novo vetor de id de bananas contendo somente os 3 produtos de maior contagem de ocorrências

insta_dataset %>%
  filter(order_id %in% pull(orders_with_more_one_type_bananas)) %>%
  filter(product_id %in% bananas) %>%
  group_by(product_id) %>%
  summarise(qtd = n()) %>%
  ungroup() %>%
  arrange(desc(qtd)) %>%
  head(3) %>% 
  select(product_id) -> top_3_bananas

top_3_bananas <- pull(top_3_bananas)

#19 # Com base no vetor criado na atividade 18, conte quantos pedidos de, em média, são feitos por hora em cada dia da semana. 

insta_dataset %>%
  filter(product_id %in% top_3_bananas) %>%
  group_by(order_dow, order_hour_of_day) %>%
  summarise(media = mean(n())) %>%
  ungroup() -> media_pedidos_bananas

#20 # Faça um gráfico dos pedidos de banana da atividade 19. O gráfico deve ter o dia da semana no eixo X, a hora do dia no eixo Y, 
# e pontos na intersecção dos eixos, onde o tamanho do ponto é determinado pela quantidade média de pedidos de banana 
# nesta combinação de dia da semana com hora

media_pedidos_bananas %>%
  ggplot(aes(x = order_dow, y = order_hour_of_day, size = media)) +
  geom_point(colour = "blue", alpha = 0.7) +
  ggtitle("Média de Pedidos de Bananas") +
  xlab("Dia da Semana") +
  ylab("Hora do Dia") + 
  scale_x_continuous( breaks = 0:6 )


#21 # Faça um histograma da quantidade média calculada na atividade 19, facetado por dia da semana

media_pedidos_bananas %>%
  ggplot(aes(x = media)) +
  geom_histogram(bins = 150) +
  facet_wrap( ~ order_dow, ncol = 3 ) +
  theme_bw()


#22 # Teste se há diferença nas vendas por hora entre os dias 3 e 4 usando o teste de wilcoxon e utilizando a simulação da aula de testes

wilcox.test(media ~ order_dow, 
            data = media_pedidos_bananas, 
            alternative = "two.sided", 
            subset = order_dow %in% c(3, 4), 
            conf.int = TRUE)