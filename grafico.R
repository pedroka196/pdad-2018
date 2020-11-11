library(survey)
library(tidyverse)
library(openxlsx)
library(data.table)

#funcoes
source("funcoes.R",encoding = "UTF-8")

# Abertura da base e do dicionário
dicionario_moradia = read.xlsx("dados/Dicionario_de_Variaveis_PDAD_2018.xlsx",sheet = 2,startRow = 2)
pdad_moradia = fread("dados/mor2018_31ras.csv")
pdad_domicilio = fread("dados/dom2018_31ras.csv")
# Alternativamente, pode fazer a leitura 
# diretamente das URLs
# pdad_moradia:
## http://www.codeplan.df.gov.br/wp-content/uploads/2020/06/mor2018_31ras.csv
# pdad_domicilio
## http://www.codeplan.df.gov.br/wp-content/uploads/2020/06/dom2018_31ras.csv
# dicionario_moradia
## http://www.codeplan.df.gov.br/wp-content/uploads/2019/06/Dicionario_de_Variaveis_PDAD_2018.xlsx


# Base de domicilio para obter a data de visita
pdad_domicilio = pdad_domicilio %>%
  select(A01nFicha,datavisita)

pdad_moradia = merge(pdad_moradia,
      pdad_domicilio,
      all.x = T,
      by.x = "A01nFicha",
      by.y = "A01nFicha")

# Remoção da base
rm(pdad_domicilio)


# Padronização dos nomes das variaveis do dicionário
names(dicionario_moradia) <- c("Coluna","Descricao","Valor","Descricao.Valor")

# Ignora o que for numero ou não é observado
dicionario_moradia <- dicionario_moradia %>%
  fill(Coluna,Descricao,.direction = "down") %>%
  filter(Valor != "Vazio", Valor != "-",
         Descricao.Valor != "-")

# Variáveis que não devem virar fator
variaveis_numericas = c("G07","G11","G16","G17","G18","G19","G201","G202","G203","G204","H012","H013","E17","E18")

# Aplicação do dicionário na base
for(i in unique(dicionario_moradia$Coluna)){
  dict = dicionario_moradia %>% filter(Coluna == i)
  if(!(i %in% variaveis_numericas)){
    pdad_moradia[[i]] = factor(x = pdad_moradia[[i]],levels = dict$Valor,labels = dict$Descricao.Valo)
  }
  
}

# Conserto de variáveis, que foram lidas como caracteres
pdad_moradia$FATOR_PROJ = as.numeric(sub(",",".",pdad_moradia$FATOR_PROJ))
pdad_moradia$datavisita = as.Date.character(pdad_moradia$datavisita,"%m/%d/%Y")

# Criação do desenho da amostra
pdad_design =
  svydesign(ids = ~A01nFicha,
            strata = ~A01ra,
            data = pdad_moradia,
            nest = T,
            weights = ~FATOR_PROJ)

post.pop <- pdad_moradia %>%
  group_by(POS_ESTRATO) %>% # Agrupar por pos-estrato
  summarise(Freq=max(POP_AJUSTADA_PROJ)) %>%
  # Fazer conversão porque vem em integer64 e a estimação erra
  mutate(POS_ESTRATO = as.numeric(POS_ESTRATO))

# pos-estraficiação
amostra <- survey::postStratify(pdad_design,~POS_ESTRATO,post.pop)

# ignorar casos omissos ou não informados
lista_ignorar_salarios = c(0,77777,88888,99999)
lista_ignorar = c(0,66666,77777,88888,99999)

# criação das variáveis de rendimento e deflator
rendimentos = update(
  amostra,
  Deflator = fator_IPCA(datavisita),
  Trab_Principal = ifelse(G16 %in% lista_ignorar_salarios,0,G16),
  Trab_Secundario = ifelse(G19 %in% lista_ignorar,0,G19),
  Aposentadoria = ifelse(G201 %in% lista_ignorar,0,G201),
  Pensao = ifelse(G202 %in% lista_ignorar,0,G202),
  Outros_Rendimentos =ifelse(G203 %in% lista_ignorar,0,G203),
  Beneficios_Sociais = ifelse(G204 %in% lista_ignorar,0,G204),
  
  # IPCA
  Trab_Principal = Trab_Principal * Deflator,
  Trab_Secundario = Trab_Secundario * Deflator,
  Aposentadoria = Aposentadoria * Deflator,
  Pensao = Pensao * Deflator,
  Outros_Rendimentos = Outros_Rendimentos * Deflator,
  Beneficios_Sociais = Beneficios_Sociais * Deflator,
  
  Beneficios_previdenciarios = Pensao + Aposentadoria,
  Rendimentos_Trabalho = Trab_Principal + Trab_Secundario,
  Todos_Rendimentos = Rendimentos_Trabalho +
    Beneficios_previdenciarios + Outros_Rendimentos + Beneficios_Sociais
)

# separação de cada caso analisado
salarios = subset(rendimentos,Rendimentos_Trabalho>0)
previdenciario = subset(rendimentos,Beneficios_previdenciarios>0)
beneficios_sociais = subset(rendimentos,Beneficios_Sociais>0)
rendimentos = subset(rendimentos,Todos_Rendimentos>0)

# População em cada grupo
populacao_rendimentos = as.data.frame(svytotal(~A01ra,rendimentos,na.rm = T)/1000) %>%
  mutate(RA = gsub("A01ra","",row.names(.)),
         Conta = "Todos os rendimentos")
populacao_trabalha = as.data.frame(svytotal(~A01ra,salarios,na.rm = T)/1000) %>%
  mutate(RA = gsub("A01ra","",row.names(.)),
         Conta = "Rendimentos do trabalho")
populacao_previdencia = as.data.frame(svytotal(~A01ra,previdenciario,na.rm = T)/1000) %>%
  mutate(RA = gsub("A01ra","",row.names(.)),
         Conta = "Benefícios previdenciários")
populacao_beneficios_sociais = as.data.frame(svytotal(~A01ra,beneficios_sociais,na.rm = T)/1000) %>%
  mutate(RA = gsub("A01ra","",row.names(.)),
         Conta = "Benefícios sociais")

# unificação dos resultados populacionais
populacao = bind_rows(
  populacao_rendimentos,
  populacao_beneficios_sociais,
  populacao_trabalha,
  populacao_previdencia
)
rm(populacao_rendimentos,
   populacao_beneficios_sociais,
   populacao_trabalha,
   populacao_previdencia
)
# Quantis de renda
quantis = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
quantis_todos_rendimentos = svyby(
  by = ~ A01ra,
  formula = ~ Todos_Rendimentos,
  # keep.var = F,
  ci = T,
  design = rendimentos,
  FUN = svyquantile,
  quantiles = quantis,
  na.rm = T,
  # se = TRUE,
  verbose = T
)
quantis_todos_rendimentos$Conta = "Todos os rendimentos"

quantis_previdencia = svyby(
  by = ~ A01ra,
  formula = ~ Beneficios_previdenciarios,
   # keep.var = F,
  ci = T,
  design = previdenciario,
  FUN = svyquantile,
  quantiles = quantis,
  na.rm = T,
  # se = TRUE,
  verbose = T
)
quantis_previdencia$Conta = "Benefícios previdenciários"

# Trabalhos apenas
quantis_salarios = svyby(
  by = ~ A01ra,
  formula = ~ Rendimentos_Trabalho,
   # keep.var = F,
  ci = T,
  design = salarios,
  FUN = svyquantile,
  quantiles = quantis,
  na.rm = T,
  # se = TRUE,
  verbose = T
)

quantis_salarios$Conta = "Rendimentos do trabalho"

# Quantis selecionados
quantis_beneficios_sociais = svyby(
  by = ~ A01ra,
  formula = ~ Beneficios_Sociais,
   # keep.var = F,
  ci = T,
  design = beneficios_sociais,
  FUN = svyquantile,
  quantiles = quantis,
  na.rm = T,
  # se = TRUE,
  verbose = T
)
quantis_beneficios_sociais$Conta = "Benefícios sociais"

# unificação dos dados e remoção das estimativas individuais
decis = bind_rows(quantis_todos_rendimentos,quantis_previdencia,quantis_beneficios_sociais,quantis_salarios)
rm(quantis_beneficios_sociais,quantis_previdencia,quantis_salarios,quantis_todos_rendimentos)

# Eixo X e nomes da base
classifica_percentis = ifelse(quantis<=0.5,paste0(quantis*100,"% mais pobres"),paste0((1-quantis)*100,"% mais ricos"))
nomes_percentis = c("RA",quantis,paste0("SE.",quantis),"Contas")

names(decis) = nomes_percentis

# Decis medios
decis_medios =  decis %>%
  select(-c(paste0("SE.",quantis))) %>%
  gather(key = "Percentil",value = "Renda",-c(RA,Contas)) %>%
  mutate(#Percentil = factor(x = Percentil,labels = unique(classifica_percentis)),
         Renda = round(Renda,0)) %>%
  ungroup()

# intervalos de confiança
decis_ICs =  decis %>%
  select(-c(paste0(quantis))) %>%
  gather(key = "Percentil",value = "IC",-c(RA,Contas)) %>%
  mutate(Percentil = gsub("SE[.]","",Percentil)) %>%
  ungroup()

# unificação das bases média e de intervalo de confiança
decis2 = merge(decis_medios,decis_ICs,by = c("RA","Percentil","Contas"))

decis2 = decis2 %>%
  mutate(Superior = round(Renda + IC),
         Inferior = round(Renda - IC))

filtros = c("Todos os rendimentos","Rendimentos do trabalho","Benefícios sociais","Benefícios previdenciários")
# Cria lista de gráfico
graficos = list()

# Tema utilizado nos gráficos
tema = theme(text = element_text(family = "sans"),
             line = element_line(size = 0.5),
             plot.title = element_text(size = 20,hjust = 0.5,face = "bold"),
             plot.subtitle = element_text(size = 13,hjust = 0.5),
             plot.caption = element_text(hjust = 0.5,size = 12),
             axis.line = element_line(size=0.5),
             axis.title.y = element_blank(),
             axis.text = element_text(colour = "black"),
             title = element_text(hjust = 0.5),
             axis.title.x = element_blank(),
             legend.title = element_blank(),
             legend.position = "right")

for(i in filtros){
  graficos[[i]]  = decis2 %>%
    filter(Contas == i) %>%
    arrange(RA,Renda) %>%
    ggplot(aes(x = Percentil,y = reorder(RA,Renda))) +
    geom_tile(aes(fill = Renda)) + 
    geom_text(aes(label = format(Renda,big.mark = ".",decimal.mark = ",")),colour = "black") +
    labs(
      title = "Rendimento médio mensal por decis de renda, Brasília, por RA, 2018.",
      subtitle = paste0(i," | Em R$ ago/2020"),
      caption = "Elaborado por Pedro Henrique Oliveira. Fonte: PDAD 2018. Codeplan.",
      fill = "Renda: "
    )+
    theme_classic()+
    tema +
    scale_x_discrete(labels = classifica_percentis) +
    scale_fill_viridis_c(option = "inferno",begin = 1,end = 0.6,values = c(0,1))
  
  ic = paste0("IC ",i)
  graficos[[ic]]  = decis2 %>%
    filter(Contas == i) %>%
    arrange(RA,Renda) %>%
    ggplot(aes(x = Percentil,y = reorder(RA,Renda))) +
    geom_tile(aes(fill = Renda)) + 
    geom_text(aes(label = paste0(
      format(Inferior, big.mark = ".", decimal.mark = ","),
      " - ",
      format(Superior, big.mark = ".", decimal.mark = ",")
    )), colour = "black") +
    labs(
      title = "Rendimento médio mensal por decis de renda, Brasília, por RA, 2018.",
      subtitle = paste0(ic," | Em R$ ago/2020"),
      caption = "Elaborado por Pedro Henrique Oliveira. Fonte: PDAD 2018. Codeplan.",
      fill = "Renda: "
    )+
    theme_classic()+
    tema +
    scale_x_discrete(labels = classifica_percentis) +
    scale_fill_viridis_c(option = "inferno",begin = 1,end = 0.6,values = c(0,1))
  
  
  ggsave(
    filename = paste0("plots/", i, ".png"),
    plot = graficos[[i]],
    dpi = 320,
    width = unit(16, "in"),
    height = unit(9, "in")
  )
  
  ggsave(
    filename = paste0("plots/", ic, ".png"),
    plot = graficos[[ic]],
    dpi = 320,
    width = unit(16, "in"),
    height = unit(9, "in")
  )
}


save(decis2,graficos,file = "graficos_salvos.RDS")