atv.antiox <- function(x, cn, cp=NULL, disp, rep=NULL, ic=NULL, n="output.pdf")
  # x=tabela de dados; cn=valores do controle negativo; cp=valores do controle positivo; disp=nome variavel independente dos graficos de dipersao;
  # rep=numero das colunas em replicata, caso existentes; ic=valor para o calculo da concentracano de inibicao; n=nome do arquivo de saida dos graficos
{
  
  ## Checando se esta em replicata e calculando a media caso positivo:
  if(!is.null(rep)) {		# Se indicado pelo usuario:
    
    media <- matrix(NA, nrow(x),length(rep))    # Criando objeto para o calculo das medias
    
    for(i in length(rep):1) {    # Para todas as colunas indicadas pelo usuario:	
      
      media[,i] <- as.matrix(x[rep[i]])    # Armazenando as colunas replicadas no novo objeto
      x[rep[i]] <- NULL    # Atualizando a matriz de dados: retirada das colunas em replicata
      
    }
    
    media <- apply(media,1,mean,na.rm=TRUE)    # Calculo da media
    x$media <- media    # Atualizando a matriz de dados: adicionando a coluna media
    
  }
  
  
  x2 <- NULL    # Objeto para reorganizacao dos dados
  f <- NULL     # Objeto para armazenar o numero da coluna que tiver fatores
  d <- NULL     # Objeto para armazenar o numero da coluna que o usuario escolheu para gerar os graficos
  n.col <- NULL # Objeto para armazenar os numeros das colunas que serao geradas a partir do calculo dos indices
  
  
  ## Calculo dos i�ndices:
  for(i in 1:ncol(x)) {    # Para todas as colunas na entrada de dados
    
    if(class(x[,i]) == "factor") {    # Para colunas que forem fatores (ex. especie1, especie2):
      
      f <- i    # Caso exista, armazena o numero da coluna
      x2 <- data.frame(c(x2,x[i]))    # Acidiona essa coluna no novo objeto de dados (x2)
      
    }
    
    else if(colnames(x[i]) == disp) {    # Para a coluna escolhida para gerar os graficos de dispersao:
      
      d <- i    # Armazena o numero da coluna
      x2 <- data.frame(c(x2,x[i]))    # Adiciona essa coluna no novo objeto de dados (x2)
      
    }
    
    else {    # Para todas as colunas numericas, calculo dos indices:
      
      if(is.null(cp)) {    # Modelo 1: amostras possuem absorbancia menor que o controle (ex. metodo de DPPH); nao exige o controle positivo
        
        count <- (mean(cn)-x[i])/mean(cn)*100               # Calculo dos indices
        colnames(count) <- paste0("atv_",colnames(x[i]))    # Mudança do nome da coluna
        x2 <- data.frame(c(x2,x[i],count))                  # Adiciona a coluna das absorbancias seguida da coluna das atividades calculadas
        n.col <- c(n.col,ncol(x2))                          # Armazena o numero da coluna das atividades calculadas (para a construcao dos graficos)
        
      }
      
      else {    # Modelo 2: amostras possuem absorbancia maior que o controle (ex. metodo de FRAP); exige o controle positivo
        
        count <- (x[i]-mean(cn))/mean(cp-mean(cn))*100      # Calculo dos indices
        colnames(count) <- paste0("atv_",colnames(x[i]))    # Mudança do nome da coluna
        x2 <- data.frame(c(x2,x[i],count))                  # Adiciona a coluna das absorbancias seguida da coluna das atividades calculadas
        n.col <- c(n.col,ncol(x2))                          # Armazena o numero da coluna das atividades calculadas (para a construcao dos graficos)
        
      }
      
    }
    
  }
  
  
  ## Geracao dos graficos e calculo dos ICs:
  
  pdf(n)    # Abertura do arquivo em pdf, que ira conter os graficos
  
  if(!is.null(f)) {     # Geracao dos graficos por fator, caso exista
    
    if(!is.null(ic)){    # Se indicado pelo usuario:
      
      ICs <- data.frame(levels(x2[,f]))    # Objeto ira conter os ICs calculados, ordenados pelos fatores
      colnames(ICs) <- "amostra"    # Mudando o nome da coluna
      
      for(i in n.col) {    # Para todos os indices (colunas) calculados
        
        col <- data.frame(rep(NA,length(levels(x2[,f]))))    # Criando uma coluna para cada indice
        colnames(col) <- paste0("IC",ic,"_",colnames(x2[i-1]))    # Nomeando as colunas com o valor do IC e o nome da coluna de referencia
        ICs <- data.frame(c(ICs,col))    # Adicionando no objeto que irá conter os ICs
        
      }
      
    }
    
    for(i in levels(x2[,f])) {    # Para cada fator existente
      
      for(j in n.col) {     # E indice (coluna) calculado
        
        # Gerando o grafico de dispersao:
        plot(x2[x2[,f]==i,d], x2[x2[,f]==i,j], main=paste(i,"-",colnames(x2[j-1])), xlab=colnames(x2[d]), ylab="Atividade")
        # Gerando a regressao linear:
        rl <- lm(x2[x2[,f]==i,j] ~ x2[x2[,f]==i,d])
        # Plotando a regressao linear no grafico:
        abline(rl)
        
        if(round(summary(rl)$coefficients[1,1],5)>0) {    # Se o intercepto foi positivo:
          
          # Gerando as equacoes da reta e o R-squared:
          eq.r2 <- paste0("y = ",round(summary(rl)$coefficients[2,1],5)," x + ",round(summary(rl)$coefficients[1,1],5),"\nR-squared = ",round(summary(rl)$adj,5))
          
        }
        
        else {    # Se o intercepto for negativo:
          
          # Gerando as equacoes da reta e o R-squared:
          eq.r2 <- paste0("y = ",round(summary(rl)$coefficients[2,1],5)," x ",round(summary(rl)$coefficients[1,1],5),"\nR-squared = ",round(summary(rl)$adj,5))
          
        }
        
        # Plotando as equacoes nos graficos:
        mtext(eq.r2,4)
        
        # Se indicado pelo usuario, ira calcular os ICs:
        if(!is.null(ic)) {
          
          IC <- (ic-round(summary(rl)$coefficients[1,1],5))/round(summary(rl)$coefficients[2,1],5)    # Calculo utilizando a equacao da reta gerada
          ICs[ICs[,1]==i,paste0("IC",ic,"_",colnames(x2[j-1]))] <- IC    # Colocando no objeto
          
        }
        
      }
      
    }
    
  }
  
  else {    # Geracao dos graficos por coluna, caso nao tenha fatores
    
    if(!is.null(ic)){    # Se indicado pelo usuario:
      
      ICs <- data.frame(colnames(x2[n.col-1]),rep(NA,length(n.col)))    # Objeto ira conter os ICs calculados, ordenados pelos indices calculados (colunas)
      colnames(ICs) <- c("amostra",paste0("IC",ic))    # Mudando o nome da coluna
      
    }
    
    for(j in n.col) {    # Para cada indice (coluna) calculado
      
      # Gerando o grafico de dispersao:
      plot(x2[,d], x2[,j], main=colnames(x2[j-1]), xlab=colnames(x2[d]), ylab="Atividade")
      # Gerando a regressao linear:
      rl <- lm(x2[,j] ~ x2[,d])
      # Plotando a regressao linear no grafico:
      abline(rl)
      
      
      if(round(summary(rl)$coefficients[1,1],5)>0) {    # Se o intercepto foi positivo:
        
        # Gerando as equacoes da reta e o R-squared:
        eq.r2 <- paste0("y = ",round(summary(rl)$coefficients[2,1],5)," x + ",round(summary(rl)$coefficients[1,1],5),"\nR-squared = ",round(summary(rl)$adj,5))
        
      }
      
      else {    # Se o intercepto for negativo:
        
        # Gerando as equacoes da reta e o R-squared:
        eq.r2 <- paste0("y = ",round(summary(rl)$coefficients[2,1],5)," x ",round(summary(rl)$coefficients[1,1],5),"\nR-squared = ",round(summary(rl)$adj,5))
        
      }
      
      # Plotando as equacoes nos graficos:
      mtext(eq.r2,4)
      
      # Se indicado pelo usuario, ira calcular os ICs:
      if(!is.null(ic)) {
        
        IC <- (ic-round(summary(rl)$coefficients[1,1],5))/round(summary(rl)$coefficients[2,1],5)    # Calculo utilizando a equacao da reta gerada
        ICs[ICs[,1]==colnames(x2[j-1]),2] <- IC    # Colocando no objeto
        
      }
      
    }
    
  }
  
  dev.off()    # Fecha o arquivo pdf
  
  
  output <- list(Atividade=x2)    # Gerando objeto de saida com a tabela das atividades calculadas
  
  if(!is.null(ic)){    # Se indicado pelo usuario:
    
    output$ICs <- ICs    # Acrescentando a tabela dos ICs no objeto de saida
    
  }
  
  return(output)    # Retornando os dados
  
}




atv.antiox              package:nenhum                R Documentation


Cálculo da Atividade Antioxidante e Concentração de Inibição

Description:
  
  Irá calcular a taxa de atividade antioxidante de n amostras, em relação a um padrão, a partir dos valores de absorbância. Calcula também a concentração de inibição da amostra.

Usage:
  
  atv.antiox(x, cn, cp=NULL, disp, rep=NULL, ic=NULL, n=”output.pdf”)

Arguments:
  
  x	Data frame ou matriz contendo os valores de absorbância das amostras.
cn	Vetor contendo um ou mais valores de absorbâncias referentes ao controle negativo do experimento.
cp	Vetor contendo um ou mais valores de absorbâncias referentes ao controle positivo do experimento.
disp	Nome da coluna que será usada como variável independente nos gráficos de dispersão.
rep	Caso as amostras estejam em replicata, indicar o número das colunas.
ic 	Valor para o calculo da concentração de inibição (IC).
n	Nome do pdf que conterá os gráficos.

Details:
  
  Será realizada a média para ambos os controles (cn e cp). Na ausência de cp, a função parte do princípio que as amostras tem uma absorbância menor que o controle negativo. Já na presença do cp, parte-se do principio que as amostras tem uma absorbância maior que o controle negativo.

Value:
  
  Arquivo em pdf contendo os gráficos de dispersão com a equação da reta e o R2 para todas as amostras.

Se ic não for nulo, uma lista contendo duas tabelas: uma com as atividades calculadas e outra com os ICs. Se ic for nulo, apenas a tabela das atividades.

Note:
  
  Pode conter valores faltantes (NA).

Author(s):
  
  Raquel Dietsche Monfardini

  ## Exemplo 1

Concentracao <- c(0.25,0.5,0.75,1,1.25)
Amostra <- rep(c("Carqueja","Eucalipto","Alecrim"),each=5)
rep1 <- c(0.874,0.802,0.742,0.637,NA,0.858,0.797,0.633,0.592,0.485,0.913,0.826,0.761,0.69,0.625)
rep2 <- c(0.908,0.802,0.755,0.628,0.584,0.867,0.734,0.659,0.519,0.487,0.924,0.858,0.739,0.718,0.646)
rep3 <- c(0.916,0.793,0.761,0.618,0.652,0.879,0.751,0.681,0.534,0.506,0.957,0.844,0.776,0.713,0.618)
dados <- data.frame(Amostra,Concentracao,rep1,rep2,rep3)

atv.antiox(dados,cn=c(1.083,1.084,1.036),disp="Concentracao",rep=c(3,4,5),ic=50,n="exemplo_1.pdf")


Concentracao <- c(0,34,66,100,134,166,200)
Amostra <- rep(c("BHT", "Kelly", "Adri", "Ju"),each=5)
rep1 <- c(0.874,0.802,0.742,0.637,NA,0.858,0.797,0.633,0.592,0.485,0.913,0.826,0.761,0.69,0.625)

dados <- data.frame(Amostra,Concentracao,rep1,rep2,rep3)

atv.antiox(dados,cn=c(1.083,1.084,1.036),disp="Concentracao",rep=c(3,4,5),ic=50,n="exemplo_1.pdf")


## Exemplo 2
Concentracao <- c(0.25,0.5,0.75,1,1.25)
Carqueja <- c(0.908,0.802,0.755,0.628,0.584)
Eucalipto <- c(0.867,0.734,0.659,0.519,0.487)
Alecrim <- c(0.924,0.858,0.739,0.718,0.646)
dados <- data.frame(Concentracao,Carqueja,Eucalipto,Alecrim)

atv.antiox(dados,cn=c(1.083,1.084,1.036),disp="Concentracao",n="exemplo_2.pdf")


## Exemplo 3
Concentracao <- c(0.25,0.5,0.75,1,1.25)
Amostra <- rep(c("Carqueja","Eucalipto","Alecrim"),each=5)
Metanol <- c(0.257,0.405,0.513,0.666,0.821,0.302,0.455,0.613,0.761,0.904,0.203,0.354,0.614,0.669,0.801)
Etanol <- c(0.312,0.421,0.514,0.618,0.723,0.201,0.323,0.411,0.517,0.605,0.218,0.233,0.423,0.501,0.622)
Agua <- c(0.401,0.462,0.527,0.578,0.641,0.256,0.302,0.367,0.423,0.479,0.116,0.302,0.358,0.431,0.496)
dados <- data.frame(Amostra,Concentracao,Metanol,Etanol,Agua)

atv.antiox(dados,cn=c(0.111,0.115,0.112),cp=c(0.530,0.520,0.521),disp="Concentracao",ic=90,n="exemplo_3.pdf")
