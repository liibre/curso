#### Codigo para gerar os graficos apresentados na aula ####
## gRaficos 18.07.19 por Sara Mortara

#### Carregando pacotes ####
## pacote necessarios para rodar todos os graficos
# pacotes para o ggplot2
library(latticeExtra)
library(ggplot2)
# pacotes para paleta de cores
library(wesanderson)
library(RColorBrewer)
# pacote para grafico 3D
library(plotrix)
# pacote para dotchart
require(graphics)

## Criando data frame para grafico de barras 3d NAO e ggplot2
d <- read.table(text=' x   y     z
t1   5   high
t1   2   low
t1   4   med
t2   8   high
t2   1   low
t2   3   med
t3  50   high
t3  12   med
t3  35   low', header=TRUE)

## definindo os niveis do objeto
levels(d$z) <- c("high", "med", "low")

## fazendo um grafico HORROROSO
cloud(y~x+z, d, panel.3d.cloud=panel.3dbars, col.facet=d$z, 
      xbase=0.4, ybase=0.4, scales=list(arrows=FALSE, col=1), 
      par.settings = list(axis.line = list(col = "transparent")),
      main="meu gráfico 3D")

## criando uma paleta de cores MARAVILHOSA usando o pacote wesanderson <3
pal <- wes_palette(3, name = "Zissou1", type = "continuous")


## fazendo o grafico de barras melhor
p <- ggplot(data=d, aes(x=x, y=y, fill=z)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=pal[3:1])+
  theme_classic()
p


## criando um grafico de pizza 3d NAAAAAAAAOOOOOOOO 
slices <- c(10, 12, 4, 16, 8) 
lbls <- c("US", "UK", "Australia", "Germany", "France")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie3D(slices,labels=lbls,explode=0.1,
   main="Pie Chart of Countries ")

## criando o grafico de pizza em 2D
pie(slices,labels = lbls, col=rainbow(length(lbls)),
   main="Pie Chart of Countries")


## o mesmo grafico em barra
nomes <- c("US", "UK", "Australia", "Germany", "France")
names(pct) <- nomes
perc <- paste0(pct, "%")
par(mai=c(1,1,1,1))
barplot(pct, horiz=TRUE, las=1, xlim=c(0, max(pct)+3))
text(x=pct+1.1,
     y=c(0.7, 1.9, 3.1, 4.3, 5.5),
     labels=perc)

## agora com as barras ordenadas
nomes <- c("US", "UK", "Australia", "Germany", "France")
perc <- paste0(sort(pct), "%")
names(pct) <- nomes
par(mai=c(1,1,1,1))
barplot(sort(pct), horiz=TRUE, las=1, xlim=c(0, max(pct)+3))
text(x=sort(pct)+1.1,
     y=c(0.7, 1.9, 3.1, 4.3, 5.5),
     labels=perc)

## mais graficos de barras
# create dummy data
set.seed(42)
d2 <- data.frame(
  name=letters[1:5],
  value=sample(seq(4,15),5),
  sd=c(1,0.2,3,2,4)
)

barplot(value ~ name, data=d2, las=1, bty='l')

## grafico de barras com desvio padrao
barplot(value ~ name, data=d2, las=1, bty='l', ylim=c(0, 18))
arrows(x0=c(0.66, 1.88, 3.07, 4.25, 5.48),
         y0=d2$value+d2$sd,
         y1=d2$value-d2$sd, angle=90, length=0.1, code=3)


## grafico de pontos com desvio padrao
plot(x=1:5, d2$value, las=1, bty='l', ylim=c(0, 18), pch=19, xaxt='n',
     xlab="names", ylab="value")
axis(1, at=1:5, labels=d2$name)
arrows(x0=1:5,
         y0=d2$value+d2$sd,
         y1=d2$value-d2$sd, angle=90, length=0.05, code=3)


## um grafico que NAO devemos fazer 
d3 <- data.frame(media=c(6.7, 12.3),
                 sd=c(0.4, 0.98),
                 tratamento=c(1, 2))
plot(media ~ c(1,2), data=d3, las=1, bty='l',
     xlab="tratamento", pch=19, xaxt='n', ylim=c(2, 15))
axis(1, at=1:2, labels=c("1", "2"))
arrows(x0=1:2,
         y0=d3$media+d3$sd,
         y1=d3$media-d3$sd, angle=90, length=0.05, code=3)


## diagrama de pontos sem ordenacao
dotchart(log(islands, 10), xlab="log10(area) (log10(sq. miles)", cex.lab=0.9)

## diagrama de pontos com ordenacao 
dotchart(log(islands[order(islands)], 10),
          xlab="log10(area) (log10(sq. miles)", cex.lab=0.9)
  

## grafico de dispersao 
data(trees)
plot(log(Volume) ~ log(Girth), data=trees,
     pch=15, col="black", cex=2,
     xlab="log(Diameter)")
abline(lm(log(Volume) ~ log(Girth), data=trees))

## grafico de dispersao 
plot(log(Volume) ~ log(Girth), data=trees,
     pch=1, col="black", cex=1,
     xlab="log(Diameter)", las=1)
abline(lm(log(Volume) ~ log(Girth), data=trees))


## grafico de dispersao com nova cor 
mycol <- rgb(1, 68, 33, max = 255, alpha = 125)
plot(log(Volume) ~ log(Girth), data=trees,
     pch=19, cex=1,
     xlab="log(Diameter)", las=1,
     col=mycol, bty="l")
abline(lm(log(Volume) ~ log(Girth), data=trees))


## perfil dos alunxs da disciplina
nivel <- c("iniciante", "intermediário", "experiente")
perc <- c(73.3, 20, 6.7)
alunxs <- data.frame(nivel, perc)
alunxs


## grafico de barras do perfil
barplot(perc ~ nivel, data=alunxs)

## alterando a ordem do fator
alunxs$nivel <- factor(alunxs$nivel,
                       levels= c("iniciante",
                                 "intermediário",
                                 "experiente"))
alunxs$nivel


## agora vamos refazer o gráfico
barplot(perc ~ nivel, data=alunxs)

## las=1 <3 
barplot(perc ~ nivel, data=alunxs,
        las=1)

## usando xlab e ylab
barplot(perc ~ nivel, data=alunxs,
        las=1,
        xlab="Nível de conhecimento em R",
        ylab="Porcentagem (%) de alunxs")

## mudando a cor com o argumento col
barplot(perc ~ nivel, data=alunxs,
        las=1,
        xlab="Nível de conhecimento em R",
        ylab="Porcentagem (%) de alunxs",
        col=1)

## argumento col em nome
barplot(perc ~ nivel, data=alunxs,
        las=1,
        xlab="Nível de conhecimento em R",
        ylab="Porcentagem (%) de alunxs",
        col='black')

## argumento col em hexadecimal
barplot(perc ~ nivel, data=alunxs,
        las=1,
        xlab="Nível de conhecimento em R",
        ylab="Porcentagem (%) de alunxs",
        col="#000000")

## argumento col com funcao rgb
barplot(perc ~ nivel, data=alunxs,
        las=1,
        xlab="Nível de conhecimento em R",
        ylab="Porcentagem (%) de alunxs",
        col=rgb(0, 0, 0, maxColorValue=255))

## voltando ao padrão de cores original
barplot(perc ~ nivel, data=alunxs,
        las=1,
        xlab="Nível de conhecimento em R",
        ylab="Porcentagem (%) de alunxs")

## barplot com grupos 
d.Titanic <- as.data.frame(Titanic)
head(d.Titanic)
barplot(Freq ~ Class + Survived, data = d.Titanic,
        subset = Age == "Adult" & Sex == "Male",
        ylab = "N of Passengers", legend = TRUE)


## ---- echo=FALSE---------------------------------------------------------
#par(mfrow = 2:1, mgp = c(3,1,0)/2, mar = .1+c(3,3:1))
d.Titanic <- as.data.frame(Titanic)
barplot(Freq ~ Class + Survived, data = d.Titanic,
        subset = Age == "Adult" & Sex == "Male",
        ylab = "N of Passengers",
        legend = TRUE)

##      ## Corresponding table :
     xt <- xtabs(Freq ~ Survived + Class + Sex,
                 d.Titanic,
                 subset = Age=="Adult")
     # Alternatively, a mosaic plot :
     mosaicplot(xt[,,"Male"], main="", color=TRUE)


## grafico de dispersao com modelo linear
m1 <- lm(log(dist) ~ log(speed), data = cars)
plot(log(dist) ~ log(speed), data = cars,
     las=1, bty='l', pch=19)
abline(m1)

## mudando para uma cor transparente 
mycol <- rgb(0, 0, 0, maxColorValue=255, alpha=125)
m1 <- lm(log(dist) ~ log(speed), data = cars)
plot(log(dist) ~ log(speed), data = cars,
     las=1, bty='l', pch=19,
     col=mycol)
abline(m1)

# inserindo uma imagem externa
library(png)
library(grid)
pic <- readPNG("../figs/car.png")
plot(log(dist) ~ log(speed), data = cars,
     las=1, bty='l', pch=19,
     col=mycol)
abline(m1)
grid.raster(pic, .25, .83, width=.2)


## um grafico mais complicado usando layout
x <- log(cars$speed)
y <- log(cars$dist)
zones <- matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
layout(zones, widths=c(4/5, 1/5), heights=c(1/5, 4/5))
xhist <- hist(x, plot=FALSE)
yhist <- hist(y, plot=FALSE)
top <- max(c(xhist$counts, yhist$counts))
plot(x,y, las=1, xlab="log(speed)", ylab="log(dist)",
     col=mycol, bty='l', pch=19)
abline(m1)
grid.raster(pic, .16, .63, width=.15)
par(mar=c(0,3,1,1))
barplot(xhist$counts, axes=FALSE, ylim=c(0, top),
        space=0, col=mycol)
par(mar=c(3,0,1,1))
barplot(yhist$counts, axes=FALSE, xlim=c(0, top),
        space=0, horiz=TRUE, col=mycol)


## lendo os dados de salario
sal <- read.csv("../data/salarios.csv")
head(sal)
# criando modelos lineares
mh <- lm(salario ~ experiencia, data=sal[sal$sexo=="H",])
mm <- lm(salario ~ experiencia, data=sal[sal$sexo=="M",])
coefh <- coef(mh)
coefm <- coef(mm)
# definindo os limites dos eixos
limy <- c(min(sal$salario),max(sal$salario))
limx <- c(min(sal$experiencia),max(sal$experiencia))
## definindo os nomes dos eixos
labx <- "Experiência (anos)"
laby <- "Salário (R$)"


## construindo o grafico com duas janelas
# define parametros graficos
par(mfrow=c(1,2), las=1, bty="l")
# plot dos valores de salario dos homens
plot(salario ~ experiencia, data=sal[sal$sexo=="H",], 
     col="tomato",
     ylim=limy, xlim=limx,
     ylab=laby, xlab=labx)
# linha do previsto pelo modelo a + bx
abline(a=coefh[1], b=coefh[2],
       col='tomato', lwd=2)
mtext("A", 3, adj=0, font=2)
grid.raster(readPNG("../figs/male.png"), .16, .75, width=.10)
## plot do salario das mulheres
plot(salario ~ experiencia, data=sal[sal$sexo=="M",], 
     col="navy",
     ylim=limy, xlim=limx,
     ylab="", xlab=labx)
mtext("B", 3, adj=0, font=2)
grid.raster(readPNG("../figs/female.png"), .66, .75, width=.08)
abline(a=coefm[1], b=coefm[2],
       col='navy', lwd=2)


## grafico de serie temporal 
data(longley)
head(longley)

## em forma de barras
barplot(GNP ~ Year, data = longley)

## em linha melhor 
plot(GNP ~ Year, data = longley, type='l',
     xaxt='n', las=1, bty='l', lty=2, lwd=2)
axis(1, at=longley$Year)


## mas em linha continua 
plot(GNP ~ Year, data = longley, type='l',
     xaxt='n', las=1, bty='l', lty=1, lwd=2)
axis(1, at=longley$Year)

