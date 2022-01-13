############################################################################# 
##              TESTE DE QUI QUADRADO PARA INDEPEND?NCIA                   ##
##                           Profa. Ana Tereza                             ##
##                            outubro de 2014                              ##
#############################################################################

#  Teste de X2 para independência: verifica se existe associação entre duas 
## variáveis qualitativas

## Restrições: 
## a) As categorias das variáveis devem ser independentes, pois caso contrário deve
## ser utilizado o teste de McNemar.
## b) Tabela de entrada dupla (n>25): nenhuma frequência esperada pode ser menor que
## 5. Além disso, com este n deve-se utilizar a correção de Yates. Se a frequência
## esperada mínima não for alcançada, deve-se utilizar o teste Exato de Fisher.
## c) Tabelas de entrada dupla, mas com 2xC (ou seja, duas linhas e mais de 2 colunas):
## o X2 pode ser calculado se todos as frequências esperadas forem >=1.
## d) Tabelas LxC (ou seja, mais de duas linhas e mais de duas colunas): o teste de X2
## é o procedimento seguro se a frequência esperada média for maior ou igual a 6,
## com alfa = 0,05; e frequência esperada média maior ou igual a 10, caso o alfa=0,01.
## A frequência esperada média pode ser obtida por meio da divisão do n pelo número
## de caselas.

chisq.test(ec2,correct = TRUE)

x=chisq.test(ec2,correct=TRUE)

x$expected
x$stdres  
# stdres = Standard Residuals = Resíduos Ajustados

## Exemplo 1: Uma companhia de seguros analisou a frequência com que 2000 segurados 
## (1000 homens e 1000 mulheres) buscaram atendimento em um hospital 
## (Bussab e Moretin, 2003)

##              Homens  Mulheres
## Usaram       100     150
## Não usaram   900     850

## Deseja-se saber se a frequência com que o segurado usa o serviço hospitalar está
## relacionado ao sexo.

## CONSTRUÇãO DA HIPÓTESE NULA
## Hipótese: a proporção de homens que busca atendimento é a mesma das mulheres
## (ou seja, o acesso ao serviço de saçude é igual entre os dois sexos)
## Sob H0, busca-se a frequ?ncia esperada caso as vari?veis sejam independentes


## CONSTRU??O DA TABELA DE CONTING?NCIA
## No comando matrix criamos um vetor com as entradas linhas e colunas, 
## utilizando como sequ?ncia de preenchimento as linhas. Primeiro a primeira
## linha depois a segunda linha, e fixamos o n?mero de colunas 
## (no caso, duas colunas, uma para homens e outra para mulheres)

saude=matrix(c(100,150,900,850),ncol=2,byrow=TRUE) ## preenchimento por linha

saude
## Podemos nomear as linhas e as colunas:

rownames(saude)=c("Usam","Não Usam")
colnames(saude)=c("Homens", "Mulheres")
saude


## REALIZAÇÃO DO TESTE DE QUI QUADRADO

## Verifica??o das restri??es:

## a) As categorias das variáveis são independentes? R: Sim

## b) Qual é o tipo de tabela de entrada dupla? R: 2x2

## c) O n é maior que 25? R: Sim, então deve-se utilizar a correção de Yates.

## As frequências esperadas são maiores ou iguais a 5?

## R: Sim

## Realizar o Teste de Qui Quadrado
chisq.test(saude,correct=TRUE)

y=chisq.test(saude,correct=TRUE)

y$expected
y$stdres


## Exemplo 2) Barley

(barley=matrix(c(64,16,34,46),
               ncol=2,
               byrow=TRUE))

lines.names=c("Ctrl","Calor")
col.names=c("Vivo","Morto")

dimnames(barley)=list(lines.names,col.names)
barley

chisq.test(barley,correct=TRUE)
dchisq(barley,1)

TeaTasting
## Ho: N?o existe uma associa??o entre a ordem de coloca??o
## do leite e a capacidade de identifica??o da mulher

TeaTasting=matrix(c(3,1,1,3),
                  nrow=2,
                  dimnames=list(Guess=c("leite","cha"),
                                Truth=c("leite","cha")))
TeaTasting

## n < 20 --> Exato de Fisher
## FE < 5 --> Exato de Fisher

fisher.test(TeaTasting)


## Teste de associa??o de estado civil com os g?neros

## Ho: N?o existe uma associa??o entre o estado civil 
## e os g?neros, ou seja, as FA s?o iguais as FE

civil=matrix(c(4,3,7,2),nrow=2,byrow=TRUE,
             dimnames=list(EC=c("casado","solteiro"),
                       generos=c("F","M")))
barplot(civil)

## n < 25 --> Exato de Fisher
## FE < 5 --> Exato de Fisher

fisher.test(civil)

