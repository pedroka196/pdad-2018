library(tidyverse)
#library(BETS)
library(zoo)
library(sidrar)

# Print o status na hora de gerar o arquivo
log_string <- function(string_entrada){
  # A função plota um log com
  # espaço acima antes e depois
  string = as.character(string_entrada)
  caracteres_espaco = 2
  tamanho_string = nchar(string_entrada)
  barras = paste0(rep.int("=",times = (tamanho_string+2*caracteres_espaco)),collapse="")
  espaco = paste0(rep.int(" ",times = caracteres_espaco),collapse="")
  string_entrada = paste0(espaco,string,espaco,collapse = "")
  
  cat("\n\n",barras,"\n",string_entrada,"\n",barras,"\n\n")
  
  
}

# Corrigir com icon
converter_nome = function(variavel){
  variavel = as.character(variavel)
  variavel = iconv(x = variavel, from = 'utf8',to = 'ASCII//TRANSLIT')
  
  return(variavel)
}

# Funcao converter
converter = function(variavel){
  variavel = as.character(variavel)
  variavel = gsub(pattern = "[R][$]",replacement = "",variavel)
  variavel = gsub(pattern = " ",replacement = "",variavel)
  variavel = gsub(pattern = "[.]",replacement = "",variavel)
  variavel = gsub(pattern = ",",replacement = ".",variavel)
  variavel = ifelse(variavel == "",0,variavel)
  variavel = as.numeric(variavel)
  
  return(variavel)
}

# Função ajusta data
ajusta_data = function(variavel){
  variavel = as.character(variavel)
  variavel = as.Date.character(variavel,format = "%d/%m/%Y")
  
  return(variavel)
}

# Remoção caracteres nome
remove_nome = function(variavel){
  variavel = as.character(variavel)
  variavel = gsub(pattern = " ",replacement = "_",variavel)
  variavel = gsub(pattern = "[.]",replacement = "",variavel)
  variavel = gsub(pattern = ",",replacement = ".",variavel)
  variavel = as.numeric(variavel)
  
  return(variavel)
}

correlacionador = function(base,eliminar){
  # Elimina o desnecessário
  base3 = base %>%
    select(-one_of(eliminar))
  
  # Quantidade de linhas
  n_linhas = length(names((base3)))
  n_colunas = n_linhas+2
  
  # Cria a matriz
  nomes_colunas = c("MEDIA","SD",names(base3))
  nomes_linhas = names(base3)
  matriz = matrix(0,nrow = n_linhas,ncol = n_colunas,dimnames = list(nomes_linhas,nomes_colunas))
  
  
  for(i in nomes_linhas){
    for(j in nomes_colunas){
      if(i == j){
        matriz[i,j] = 1
      }
      else{
      #   # Se for media
        if(j %in% c("MEDIA","SD")){
            if(j == "MEDIA"){
              matriz[i,j] = mean(base3[,i],na.rm = T)
            }
            if(j == "SD"){
              matriz[i,j] = sd(base3[,i],na.rm = T)
            }
        }
        else{
          matriz[i,j] = cor(base3[i],base3[j],use = "complete.obs")
        }
      }
      cat("\n Coluna: ",j," Linha:",i)
      }
      
     
    }
  # matriz = as.data.frame(matriz,row.names = nomes_linhas)
  matriz = cbind.data.frame(nomes_linhas,matriz)
  return(matriz)
}

fator_IPCA = function(variavel_data,periodo = NULL){
  if(missing(variavel_data)){
    stop("Variável data não presente")
  }
  if(is.null(periodo)){
    periodo = as.Date("2020-08-01")
  }
  
  ultimo = format(periodo,"%Y%m")
  ANO = as.integer(format(periodo,"%Y"))
  
  ipca = get_sidra(x = 1737,
                   variable = 2266,
                   period = paste0("200908-",ultimo)
  )
  
  variavel_data = format(variavel_data,"%Y-%m")
  
  ipca2 = ipca %>%
    mutate(Data = as.Date.character(x = paste0(`Mês (Código)`,"01"),format = "%Y%m%d"),
           ANO = as.integer(format(Data,"%Y")),
           Mes = as.integer(format(Data,"%m")),
           Valor = Valor/100)
  
 
  ipca_ind       <- ipca2$Valor
  names(ipca_ind) <- format(ipca2$Data,"%Y-%m")
  # Cria vetor com o fator que deverá multiplicar os valores das séries temporais, de forma a colocá-las todas a preços de DEZEMBRO de 2019.
  
  ipca_ind = ipca_ind/last(ipca_ind)
  
  fator = ipca_ind
  
  deflator = 1
  for(i in names(fator)){
    deflator = ifelse(variavel_data == i, 1 + (1-fator[i]),deflator)
    cat("\n Ano: ",i)
  }
  
  return(deflator)
  
}


agregador = function(base,variaveis_grupo,variaveis_id,variaveis_calculadas){
  if(missing(variaveis_id)){
    variaveis_id = NULL
  }
  base_final = base %>%
    mutate( # Quantidade de servidores, sem distinção
      Quantidade_diferente = ifelse(is.null(variaveis_id),
                                    n(),
                                    n_distinct(vars(variaveis_id),na.rm = T))) %>%
    group_by_at(.vars = variaveis_grupo) %>%
    summarise(# Quantidade de servidores
      Quantidade_Igual = n(),
      Quantidade_diferente = max(Quantidade_diferente))
      
  
  # medias
  for(i in variaveis_calculadas){
    # Base com valores médios
    base_media = base %>%
        group_by_at(.vars = variaveis_grupo) %>%
        # Médias dos arquivos
        summarise_at(.vars = vars(i),.funs = ~mean(.,na.rm = T))
      # Renomeia base

      # medianas
      base_mediana = base %>%
        group_by_at(.vars = variaveis_grupo) %>%
        # Médias dos arquivos
        summarise_at(.vars = vars(i),.funs = ~median(.,na.rm = T))
      # Renomeia base
      
      # mínimos
      base_minima = base %>%
        group_by_at(.vars = variaveis_grupo) %>%
        # Médias dos arquivos
        summarise_at(.vars = vars(i),.funs = ~min(.,na.rm = T))

      # máximos
      base_maximo = base %>%
        group_by_at(.vars = variaveis_grupo) %>%
        # Médias dos arquivos
        summarise_at(.vars = vars(i),.funs = ~max(.,na.rm = T))

      # desvio padrão
      base_desvios = base %>%
        group_by_at(.vars = variaveis_grupo) %>%
        # Médias dos arquivos
        summarise_at(.vars = vars(i),.funs = ~sd(.,na.rm = T))
      
      # Base com soma
      base_total = base %>%
        group_by_at(.vars = variaveis_grupo) %>%
        # Médias dos arquivos
        summarise_at(.vars = vars(i),.funs = ~sum(.,na.rm = T))
      # Renomeia base
      
      medias = paste0("MEDIA_",i)
      minimos = paste0("MIN_",i)
      maximos = paste0("MAX_",i)
      medianas = paste0("MEDIANA_",i)
      desvios = paste0("SD_",i)
      total = paste0("TOTAL_",i)
      
      # Mesclagem de base
      base_final[medias] = base_media[i]
      base_final[desvios] = base_media[i]
      base_final[medianas] = base_mediana[i]
      base_final[maximos] = base_maximo[i]
      base_final[minimos] = base_minima[i]
      base_final[total] = base_total[i]
      
    
    cat("\n",i)
  }
  
  
  return(base_final)
}

# Funcao multi_spread, para fazer spread de valores múltiplos
multi_spread <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}

# Função que gera data.frame do histograma 
histograma = function(base, minimo = 0, maximo = 35000,divisoes = 7,variavel,criterios){
  grupos = (maximo-minimo)/divisoes
  grupos = c(minimo,grupos*(1:divisoes))
  
  # Nomes das variáveis
  nomes = paste0(grupos[-length(grupos)],"-",grupos[-1])
  nomes = c(nomes,paste0("Acima de",grupos[length(grupos)]))
  # nomes = as.factor(nomes)

  # Loops
  base$CLASSIFICACAO_HISTOGRAMA = ""
  base$Classificacao = 0
  for(i in 2:length(grupos)){
    # Base 
    # base = base %>%
    base$CLASSIFICACAO_HISTOGRAMA = ifelse(base[variavel] <= grupos[i] & base[variavel] > grupos[i-1],nomes[i-1],base$CLASSIFICACAO_HISTOGRAMA)
    base$Classificacao = ifelse(base[variavel] <= grupos[i] & base[variavel] > grupos[i-1],i-1,base$Classificacao)
    if(i == 2){
      # base = base %>%
      base$CLASSIFICACAO_HISTOGRAMA = ifelse(base[variavel] <= minimo,paste0("NEGATIVO-",minimo),base$CLASSIFICACAO_HISTOGRAMA)
      base$Classificacao = ifelse(base[variavel] <= minimo,0,base$Classificacao)
    }
    if(i == length(grupos)){
      # base = base %>%
      base$CLASSIFICACAO_HISTOGRAMA = ifelse(base[variavel] > grupos[i],nomes[i],base$CLASSIFICACAO_HISTOGRAMA)
      base$Classificacao = ifelse(base[variavel] > grupos[i],i,base$Classificacao)
    }
  }
  
  criterios_quantidade = c(criterios,'Classificacao',"CLASSIFICACAO_HISTOGRAMA")
  base_agregada = base %>%
    # arrange(Classificacao) %>%
    group_by_at(.vars = criterios_quantidade) %>%
    summarise(Quantidade = n()) %>% 
    ungroup() %>%
    group_by_at(.vars = criterios) %>%
    mutate(Percentual = Quantidade/sum(Quantidade,na.rm = T)) %>%
    arrange(Classificacao)
  
  # base_agregada = base_agregada %>% 
    # select(-Classificacao)
  # 
  base_agregada$CLASSIFICACAO_HISTOGRAMA = factor(x = base_agregada$CLASSIFICACAO_HISTOGRAMA,
                                                  levels = unique(base_agregada$CLASSIFICACAO_HISTOGRAMA))
  return(base_agregada)
  
} 

# Função que gera percentis de renda
distribuicao_percentil = function(base,criterios,variaveis,divisao = 4){
  percentual = 1/divisao
  
  # criterios = rlang::sym(criterios)
  # Cria os centis analisados
  percentis = percentual*(1:(divisao))
  
  # Cria os rótulos em porcentagem
  p_names <- map_chr(percentis, ~paste0(.x*100, "%"))
  
  # Função
  p_funs <- map(percentis, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
    set_names(nm = p_names)
  
  histograma <- base %>%
    group_by_at(.vars = criterios) %>%
    summarise_at(vars(variaveis),funs(!!!p_funs))
  
  return(histograma)
}

converte_data_texto = function(data){
  mes_char = c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ")
  mes_char_comp = c("JANEIRO","FEVEREIRO","MARÇO","ABRIL","MAIO","JUNHO","JULHO","AGOSTO","SETEMBRO","OUTUBRO","NOVEMBRO","DEZEMBRO")
  mes_numero = 1:12
  
  data = toupper(as.character(data))
  for(i in mes_numero){
    data = sub(x = data,pattern = mes_char_comp[i],replacement = i)
    data = sub(x = data,pattern = mes_char[i],replacement = i)
    # cat(mes_char[i],'\n\n')
  }
    
  return(data)
  
}

cacador_strings = function(string_input,string_procurada,contido_string = T){

  retorno = c()
  for(i in string_procurada){
    retorno = c(retorno,grep(pattern = i,x = string_input,fixed = T))
  }
  if(contido_string == T){
    retorno = string_input[retorno]
  }
  else{
    retorno = string_input[-retorno]
  }

  return(retorno)
  
}
