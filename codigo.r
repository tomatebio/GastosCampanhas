# baixar aquivo do site http://inter01.tse.jus.br/spceweb.consulta.receitasdespesas2014/abrirTelaReceitasCandidato.action

# Escolha o partido/UF e candidato.


# Colocar o arquivo na mesma pasta que este script e carregar o arquivo


nomedoarquivo="bolsonaro.csv"

politico<-read.table(file=nomedoarquivo, encoding ="latin1", sep=";",header = T,na.strings = "",as.is=T)

#vendo a estrutura dos dados
str(politico)
# Temos que converter o valor para numero e formatar os pontos e vigulas
politico$Valor.R.<-sub("R\\$","",politico$Valor.R.)
politico$Valor.R.<-sub("\\.","",politico$Valor.R.)
politico$Valor.R.<-sub(",",".",politico$Valor.R.)

politico$Valor.R.<-as.numeric(politico$Valor.R.)

# Agora podemos ver o quanto foi arrecadado
total<-sum(politico$Valor.R.)
# Abaixo é apenas para ver melhor
print(format(total, big.mark=" "))

# vendo a menor doação feita, a médiana, media e a maxima
summary(politico$Valor.R.)
hist(politico$Valor.R.,main= "Distribuição das doações",xlab="em R\\$")
# Quem deu $?
# Há duas colunas de doador, "doador" e "doador originario". Em alguns partidos o doador esta como "comite financeiro" e o nome de quem realmente deu o dinheiro esta em "doador originario", para outros o campo  doador originario esta vazio pois o nome o doador já esta escrito no campo "doador". Vou guardar esta info em uma nova coluna nomedoacao
politico$nomedoacao<-ifelse(is.na(politico$Doador.Originário),politico$Doador,politico$Doador.Originário)




## primeiro somar todas as doações por nome do doador,
dt<-aggregate(politico$Valor.R.,by=list(politico$nomedoacao),FUN =sum)
# tranformar o resultado do anterior em uma tabela
row.names(dt)<-dt[,1]
#colocando em ordem decrescente
dt<-dt[order(dt[,2],decreasing=T),]

# numero de doadores
n.doadores<-length(dt[,1])
print(n.doadores)

#media por doador
media.doador<-total/n.doadores
print(media.doador)




png(paste(nomedoarquivo,".png"))
#Fazer um grafico
par(mar=c(4,20.1,4.1,2.1))
# Como o número de doadores pode ser muito grande a opção é de ver apenas os que doaram mais. Nesse caso os 15 maiores doadores
barplot(dt[1:15,2],names.arg = dt[1:15,1],horiz = T,las=1,xlab = "Reais")
dev.off()
