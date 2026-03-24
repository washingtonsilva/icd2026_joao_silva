# ============================================================
# DOCUMENTACAO DO SCRIPT
# ============================================================
# Arquivo: 01_introducao.R
# Autor(a): <seu nome>
# Data: <dd/mm/aaaa>
# Objetivo: entender os fundamentos da linguagem R


# ATALHO PARA CRIAR SEÇÕS DE CÓDIGO: CTRL + SHIFT + R

# ==========================================
# BLOCO 0 - Configuracoes globais
# ==========================================

# Ajusta localidade para portugues (datas, mensagens e formatos)
Sys.setlocale("LC_ALL", "pt_BR.UTF-8")


# ==========================================
# BLOCO 1 - R como uma grande calculadora
# ==========================================

# Carrega o pacote se ele tiver sido instalado
library(tidyverse)


# Operacoes aritmeticas basicas

## adicao
15 + 7

## subtracao
20 - 6

## multiplicacao
8 * 9

## divisao
84 / 7

## potenciacao
2^5

# Predencia de operacoes matemáticas
# parenteses primeiro, depois potenciacao, multiplicacao e divisao,
# e por ultimo adicao e subtracao

# parentese primeiro
(15 + 7) * 2
84 / (7 + 5)

# ---------------------------------
# Funcoes matematicas importantes
# ---------------------------------

# logaritmo natural
log(100)

# logaritmo base 10
log10(100)

# funcao exponencial (e elevado a x)
exp(1)

# valor absoluto
abs(-45)

# raiz quadrada
sqrt(225)

# arredondamento para 2 casas decimais
round(3.14159, digits = 2)


# ============================================================
# BLOCO 2 - Tipos Atômicos e classes
# ============================================================

# Os tipos de dados definem como os dados
# são armazenados na memória.

# tipo double e classe numeric
a <- 3.14
a
# função que retorna o tipo do objeto
typeof(a)
# função que retorna a classe do objeto
class(a)

# character
b <- "João"
b

# logical
c <- TRUE
c

d <- FALSE
d

# NaN (Not a Number) representa um valor indefinido
e <- 0 / 0
e

# Inf (Infinity) representa um valor infinito
f <- 1 / 0
f

# coerção de logical para numeric
# TRUE = 1 e FALSE = 0
f <- as.numeric(c)
f


# ============================================================
# BLOCO 3 - Vetores numericos e vetorizacao
# ============================================================

# Atalho de teclado para inserir <- :
# Alt + - (Windows/Linux)

# Cria dois vetores numericos com dados de receita e custo diarios

receita_diaria <- c(9200, 8700, 10100, 9800, 11050)
# exibe o vetor criado usando a funcao print()
print(receita_diaria)
# classe do vetor criado
class(receita_diaria)

custo_diario <- c(6400, 6000, 7200, 6800, 7600)
# exibe o vetor criado sem usar a funcao print()
custo_diario
# classe do vetor criado
class(custo_diario)


# Vetorizacao: operacoes elemento a elemento
lucro_diario <- receita_diaria - custo_diario
margem_diaria <- lucro_diario / receita_diaria

# ---------------------------------------
# Funcoes úteis para vetores numericos
# ---------------------------------------

# logaritmo da receita diária
log(receita_diaria)

# receita total da semana
sum(receita_diaria)

# receita media da semana
mean(receita_diaria)

# tamanho do vetor de receita
# Nesse caso é o número de dias registrados
length(receita_diaria)

# receita minima da semana
min(receita_diaria)

# receita maxima da semana
max(receita_diaria)

# vendo a ajuda de uma funcao
?mean
?length


# ============================================================
# BLOCO 4 - Vetores
# ============================================================

# vetor de caracteres (strings) com nome da empresa
nome_empresa <- c("Loja A", "Loja B", "Loja C")
# exibe o vetor criado
nome_empresa
# classe do vetor criado
class(nome_empresa)

# vetor logico (booleano) indicando se a meta de vendas foi batida
meta_batida <- c(TRUE, FALSE, TRUE)
# exibe o vetor criado
meta_batida
# classe do vetor criado
class(meta_batida)


# ============================================================
# BLOCO 5 - Criando uma tibble com dados ficticios de vendas
# e primeiros exemplos com dplyr
# ============================================================

# Neste bloco vamos criar um pequeno conjunto de dados
# semelhante a um banco de dados simples de vendas.

# A ideia é trabalhar com:
# - data da venda
# - receita obtida
# - despesa do dia
# - loja responsável pela venda


# ------------------------------------------------------------
# Cria uma tibble com dados ficticios
# ------------------------------------------------------------

vendas_diarias <- tibble(
  data = seq(as.Date("2026-02-17"), by = "day", length.out = 10),
  receita = c(9200, 8700, 10100, 9800, 11050, 10400, 9900, 11500, 12100, 11700),
  despesa = c(6400, 6000, 7200, 6800, 7600, 7100, 6900, 7900, 8300, 8100),
  loja = c(
    "Centro", "Centro", "Shopping", "Shopping", "Online",
    "Online", "Centro", "Shopping", "Online", "Centro"
  )
) |>
  mutate(
    lucro = receita - despesa,
    margem = lucro / receita
  )

# Visualiza as primeiras linhas da tabela
head(vendas_diarias)

# Mostra a estrutura do objeto
glimpse(vendas_diarias)

# Resumo estatístico das colunas numéricas
summary(vendas_diarias)

# Classe do objeto
class(vendas_diarias)

# PERGUNTAR PARA A TURMA TURMA
# “Se vocês fossem gerentes/adm. dessa empresa, que tipo de perguntas
# fariam sobre esses dados?”
# MINHA RESPOSTA
# É exatamente isso que vamos aprender a responder com o dplyr.”


# ------------------------------------------------------------
# Primeiros exemplos simples com dplyr
# ------------------------------------------------------------

# Exemplo 1
# Pergunta de negócio:
# Quero ver apenas algumas colunas da tabela

vendas_diarias |>
  select(data, loja, receita)


# Exemplo 2
# Pergunta de negócio:
# Em quais dias o lucro foi maior que 3000?

vendas_diarias |>
  filter(lucro > 3000)


# Exemplo 3
# Pergunta de negócio:
# Quais registros pertencem à loja Centro?

vendas_diarias |>
  filter(loja == "Centro")


# Exemplo 4
# Pergunta de negócio:
# Quais foram os dias com maior receita?

vendas_diarias |>
  arrange(desc(receita))


# Exemplo 5
# Pergunta de negócio:
# Qual foi a receita média por loja?

receita_por_loja <- vendas_diarias |>
  group_by(loja) |>
  summarise(
    receita_media = mean(receita),
    lucro_medio = mean(lucro)
  )
# exibe o resultado
receita_por_loja


# Exemplo 6
# Pergunta de negócio:
# Quantos dias de vendas temos para cada loja?

vendas_diarias |>
  group_by(loja) |>
  summarise(
    n_dias = n()
  )


# Exemplo 7
# Pergunta de negócio:
# Em quais dias a receita ficou entre 10000 e 11500?

vendas_diarias |>
  filter(between(receita, 10000, 11500))

# ============================================================
# BLOCO 6 - Manipulação de dados com dplyr e uso do pipe
# ============================================================

# O operador pipe |> permite encadear operações
# de forma mais legível.

# Em vez de escrever:

# resultado <- funcao3(funcao2(funcao1(dados)))

# podemos escrever:

# dados |>
#   funcao1() |>
#   funcao2() |>
#   funcao3()


# ------------------------------------------------------------
# Boas práticas no RStudio
# ------------------------------------------------------------

# LEMBRAR DE MOSTRAR PARA OS ALUNOS:

# 1) Configurar o RStudio para usar o pipe nativo |>

# 2) Atalho de teclado para inserir pipe
# Ctrl + Shift + M (Windows/Linux)

# 3) Formatar código automaticamente
# Code -> Reformat Selection

# 4) (OPCIPONAL) Mostrar como reorganizar os painéis do RStudio
# Console, Environment, Source, Files etc.


# ------------------------------------------------------------
# Exemplo 1 - selecionar e ordenar dados
# ------------------------------------------------------------

# Pergunta de negócio:
# Quero ver as receitas ordenadas da maior para a menor

vendas_diarias |>
  select(data, loja, receita) |>
  arrange(desc(receita))


# ------------------------------------------------------------
# Exemplo 2 - filtrar e selecionar
# ------------------------------------------------------------

# Pergunta:
# Quais foram os dias em que o lucro foi maior que 3000?

vendas_diarias |>
  filter(lucro > 3000) |>
  select(data, loja, receita, lucro)


# ------------------------------------------------------------
# Exemplo 3 - resumo estatístico
# ------------------------------------------------------------

# Receita média geral

vendas_diarias |>
  summarise(
    receita_media = mean(receita),
    lucro_medio = mean(lucro)
  )


# ------------------------------------------------------------
# Exemplo 4 - análise por loja
# ------------------------------------------------------------

# Receita média e lucro médio por loja

vendas_diarias |>
  group_by(loja) |>
  summarise(
    receita_media = mean(receita),
    lucro_medio = mean(lucro),
    n_dias = n()
  )


# ------------------------------------------------------------
# Exemplo 5 - ordenar resultados agregados
# ------------------------------------------------------------

# Qual loja teve maior receita média?

vendas_diarias |>
  group_by(loja) |>
  summarise(
    receita_media = mean(receita)
  ) |>
  arrange(desc(receita_media))


# ============================================================
# BLOCO 7 - Visualizacao de dados com ggplot2
# ============================================================

# Grafico 1 de barras da receita media por loja
ggplot(receita_por_loja, aes(x = loja, y = receita_media)) +
  geom_col()

# Grafico 2 de barras da receita media por loja em ordem crescente
# reorder() é uma função que reordena os níveis de um fator com base em
# outra variável
ggplot(receita_por_loja, aes(x = reorder(loja, receita_media), y = receita_media)) +
  geom_col()

# Grafico 3 de barras da receita media por loja
# coord_flip() é uma função que inverte os eixos x e y
ggplot(receita_por_loja, aes(x = loja, y = receita_media)) +
  geom_col() +
  coord_flip()

# Grafico 4 de barras da receita media por loja em ordem decrescente
ggplot(receita_por_loja, aes(x = reorder(loja, receita_media), y = receita_media)) +
  geom_col() +
  coord_flip()

# Grafico de linha da receita diaria
ggplot(vendas_diarias, aes(x = data, y = receita)) +
  geom_line()

# Grafico de dispersao entre despesa e receita
ggplot(vendas_diarias, aes(x = despesa, y = receita)) +
  geom_point()

# Grafico de pontos com tamanho e cor variando por lucro e loja
ggplot(vendas_diarias, aes(x = data, y = lucro, size = receita, color = loja)) +
  geom_point()


# ============================================================
# BLOCO 8 - Resolução dos exercícios propostos nos slides
# ============================================================


# Solução do Ex. 1

## cria o vetor de custos
custos_semanais <- c(5400, 6100, 5900, NA, 6300, 6000)

## cálculo do custo total usando a função sum() e removendo NA
custo_total <- sum(custos_semanais, na.rm = TRUE)
custo_total


# Solução do Ex. 2

## cálculo do custo médio usando a função mean() e removendo NA
custo_medio <- mean(custos_semanais, na.rm = TRUE)
custo_medio


# Solução do Ex. 3

## cálculo do custo mínimo usando a função min()
custo_minimo <- min(custos_semanais, na.rm = TRUE)
custo_minimo

## cálculo do custo máximo usando a função max()
custo_maximo <- max(custos_semanais, na.rm = TRUE)
custo_maximo


# Solução do Ex. 4

# seleciona apenas a coluna com o nome da loja
vendas_diarias |>
  select(loja)


# Solução do Ex. 5

# filtra somente os dias em que a margem foi maior que 0.30
vendas_diarias |>
  filter(margem > 0.30)


# Solução do Ex. 6

# mantém apenas as linhas em que as duas condições
# são verdadeiras ao mesmo tempo
vendas_diarias |>
  filter(loja == "Online" & lucro > 3000)


# Solução do Ex. 7

# ordena os registros da maior para a menor despesa
vendas_diarias |>
  arrange(desc(despesa))


# Solução do Ex. 8

# resume a tabela em um único valor: a média da despesa
vendas_diarias |>
  summarise(
    despesa_media = mean(despesa)
  )


# Solução do Ex. 9

# agrupa os dados por loja para calcular uma média
# separada para cada uma
vendas_diarias |>
  group_by(loja) |>
  summarise(
    margem_media = mean(margem)
  )


# Solução do Ex. 10

# define os dados e data no eixo x e despesa no eixo y
ggplot(vendas_diarias, aes(x = data, y = despesa)) +
  # desenha uma linha para mostrar a evolução da despesa
  geom_line()



# Solução do Ex. 11

# define receita no eixo x e lucro no eixo y
ggplot(vendas_diarias, aes(x = receita, y = lucro)) +
  # desenha um ponto para cada observação
  geom_point()


# Solução do Ex. 12

# define data no eixo x e margem no eixo y
ggplot(vendas_diarias, aes(x = data, y = margem)) +
  # mostra o valor observado em cada dia
  geom_point() +
  # liga os pontos para destacar a evolução ao longo do tempo
  geom_line()


# Solução do Ex. 13


# 1. Calculamos o lucro médio por loja
lucro_por_loja <- vendas_diarias |>
  group_by(loja) |>
  summarise(
    lucro_medio = mean(lucro)
  )

# exibe o resultado
lucro_por_loja

# 2. Fazemos o gráfico de barras do lucro médio por loja
ggplot(lucro_por_loja, aes(x = loja, y = lucro_medio)) +
  geom_col()



# ------------------------- FIM -------------------------------------------#
