# Funcao getTse, desenvolvida por Leonardo Barone

# Carregar a base com resultado total do estado de SP, para obter a base com os resultados do municipio

getTse<-function(link){
  
  # Cria um nome temporario que o arquivo baixado recebera
  
  pasta.temporaria = file.path(getwd(), "/temp_folder")
  dir.create(pasta.temporaria)
  nome.temporario = file.path(pasta.temporaria, "temp")
  
  # Faz o donwload do link e armazena arquivo temporario
  download.file(link, nome.temporario, quiet = T)
  print("Download concluido")
  
  # Unzip do arquivo temporario
  unzip(nome.temporario, exdir = pasta.temporaria)
  
  # Produz uma lista dos arquivos novos na pasta
  lista.arquivos <- list.files(pasta.temporaria)
  
  # Gera um data frame vazio que será o output da funcao
  dados.output<-data.frame()
  
  # Loop - para cada i de 1 até o tamanho da lista
  for (i in lista.arquivos){
    
    # Gerar o caminho e nome do arquivo combinando pasta e o arquivo i
    nome.arquivo <- file.path(pasta.temporaria, i)
    
    # Extrai a extensao do arquivo (ultimos 3 caracteres do nome)
    extensao.arquivo <- substr(nome.arquivo, (nchar(nome.arquivo)-2), nchar(nome.arquivo))
    
    # Se extensao do arquivo eh igual a txt, seguir
    if (extensao.arquivo=="txt"){
      
      # Obtem as 10 primeiras linhas do arquivo (se houver)
      linhas.arquivo <- length(readLines(nome.arquivo, n=10))
      
      # Se o numero de linhas for maior que 9, seguir
      if (linhas.arquivo>9){
        
        # Imprime no console o nome do arquivo
        print(paste("Arquivo", i, "aberto com sucesso!"))
        
        # Abre o arquivo de dados com o nome 'dados'
        # Opcoes: separador = ; , quote = " e enconding = latin1
        dados <- read.table(nome.arquivo, sep=";", quote="\"",
                            fill = TRUE, fileEncoding="latin1",
                            stringsAsFactors = F, skip = 1)
        
        # Acrescente os dados ao data frame dados.output (empilhar) 
        dados.output <- rbind(dados.output, dados)        
      }
    } 
    
    # Remove o arquivo aberto
    file.remove(nome.arquivo)
  }
  # Remove a pasta temporaria
  file.remove(pasta.temporaria)
  
  # Mensagem final
  print("Arquivo gerado com sucesso!")
  
  # Retorna dados.output
  return(dados.output)
}