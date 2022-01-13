data=matrix(c(96,123,82,112,100,204,177,218,188,200),nrow=2,byrow=5)
data
rownames(data)=c("Sim","Não")
colnames(data)=c("Cascavel","Corbélia","Toledo","Palotina","Medianeira")
data

prop.table(data)*100
prop.table(data,2)*100
# se eu fizer o percentual da matriz e colocar o nome do arquivo seguido de (data, 2),
# isso significa que faremos o percentual em relação ao total das colunas.
# se eu fizer (data, 1), farei o percentual em relação às linhas.

barplot(prop.table(data,2),beside=TRUE,legend.text = rownames(data))

chisq.test(data)

prop.table(data,2)

p=c(0.32,0.41,0.27,0.37,0.33) # proporções das pessoas que separam o lixo em cada cidade
N=length(p) # tamanho da amostra em cada cidade
value=critical.range=c() # valor crítico
sig=c() # nível de significância

for(i in 1:(N-1))
{for (j in (i+1):N)
{
  value=c(value,(abs(p[i]-p[j])))
  critical.range=c(critical.range,
                   sqrt(qchisq(0.95,4))*sqrt(p[i]*(1-p[i])/300 + p[j]*(1-p[j])/300))
}
  }
sig=ifelse(value>critical.range,"Sim","Não")
marascuilo=data.frame(prop.diff=round(value,3),critical.value=round(critical.range,3),sig)
row.names.temp=c()
for(i in 1:(N-1))
{for (j in (i+1):N)
  {row.names.temp=c(row.names.temp,paste("p",i,"-","p",j,sep = ""))}}
rownames(marascuilo)=row.names.temp
marascuilo

barplot(p*100,
        names=c("Cascavel","Corbélia","Toledo","Palotina","Medianeira"),
        ylab="Percentual",
        ylim = c(0,40))


# EXEMPLO

ec=matrix(c(1,6,4,2,0,3,2,0),nrow=2,byrow=4)
ec
rownames(ec)=c("F","M")
colnames(ec)=c("Solteiro","Casado","Namorando","Noivo")
ec

chisq.test(ec,correct=TRUE)


ec2=matrix(c(10,65,43,52,190,135,157,148),nrow=2,byrow=4)
ec2
rownames(ec2)=c("F","M")
colnames(ec2)=c("Solteiro","Casado","Namorando","Noivo")
ec2

chisq.test(ec2,correct=TRUE)

prop.table(ec2,2)

p=c(0.05,0.325,0.215,0.26) # proporções das mulheres em relação ao estado civil
N=length(p) # tamanho da amostra 
value=critical.range=c() # valor crítico
sig=c() # nível de significância

for(i in 1:(N-1))
{for (j in (i+1):N)
{
  value=c(value,(abs(p[i]-p[j])))
  critical.range=c(critical.range,
                   sqrt(qchisq(0.95,3))*sqrt(p[i]*(1-p[i])/200 + p[j]*(1-p[j])/200))
}
}
sig=ifelse(value>critical.range,"Sim","Não")
marascuilo=data.frame(prop.diff=round(value,3),critical.value=round(critical.range,3),sig)
row.names.temp=c()
for(i in 1:(N-1))
{for (j in (i+1):N)
{row.names.temp=c(row.names.temp,paste("p",i,"-","p",j,sep = ""))}}
rownames(marascuilo)=row.names.temp
marascuilo
