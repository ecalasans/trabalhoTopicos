library(dplyr)

base <- read.csv("BaseDPEvolucaoMensalCisp.csv", sep = ';')

porMes <- group_by(base, mes_ano) %>% summarise(estupro=sum(estupro), celular=sum(roubo_celular), 
                                             tentHomicidio=sum(tentat_hom), homDoloso=sum(hom_doloso), homCulposo = sum(hom_culposo), 
                                             rouboVeiculo=sum(roubo_veiculo), estelionato=sum(estelionato), pessDesap=sum(pessoas_desaparecidas))

porAISP <- group_by(base, AISP) %>% summarise(estupro=sum(estupro), celular=sum(roubo_celular), tentHomicidio=sum(tentat_hom), 
                                          homDoloso=sum(hom_doloso), homCulposo = sum(hom_culposo), rouboVeiculo=sum(roubo_veiculo), 
                                          estelionato=sum(estelionato), pessDesap=sum(pessoas_desaparecidas))

kCelularEstuproMes <- kmeans(data.frame(porMes$celular, porMes$estupro), centers = 3)
kCelularEstuproAISP <- kmeans(data.frame(porAISP$celular, porAISP$estupro), centers = 3)

kEstuproTentHomicidioMes <- kmeans(data.frame(porMes$estupro, porMes$tentHomicidio), centers = 3)
kEstuproTentHomicidioAISP <- kmeans(data.frame(porAISP$estupro, porAISP$tentHomicidio), centers = 3)

kRouboVeiculoTentHomicidioMes <- kmeans(data.frame(porMes$rouboVeiculo, porMes$tentHomicidio), centers = 4)
kRouboVeiculoTentHomicidioAISP <- kmeans(data.frame(porAISP$rouboVeiculo, porAISP$tentHomicidio), centers = 4)

porMes <- data.frame(porMes, kCelularEstuproMes$cluster, kEstuproTentHomicidioMes$cluster, kRouboVeiculoTentHomicidioMes$cluster)
porAISP <- data.frame(porAISP, kEstuproTentHomicidioAISP$cluster, kEstuproTentHomicidioAISP$cluster, kRouboVeiculoTentHomicidioAISP$cluster)

#Plotagem dos graficos
#Celular x Estupro por mes
cores = kCelularEstuproMes$cluster+1
with(porMes, plot(celular,estupro, col=cores, 
                  main = "Roubos de Celular x Casos de Estupro no RJ\npor Mês - 2003 a Jun/2017",
                  xlab = "Roubos de Celular", ylab ="Casos de Estupro"))
    points(x = kCelularEstuproMes$centers, col=2:4, pch=17, cex=1)
    legend("topright", pch = 19, col = unique(cores), 
           legend = c("Cluster 1", "Cluster 2", "Cluster 3"))
dev.copy(png, file = "celEstuproMes.png")
dev.off()

#Celulares x Estupro por AISP
cores = kCelularEstuproAISP$cluster+2
with(porAISP, plot(celular,estupro, col=cores, 
                   main = "Roubos de Celular x Casos de Estupro no RJ\npor AISP - 2003 a Jun/2017",
                  xlab = "Roubos de Celular", ylab ="Casos de Estupro"))
  points(x = kCelularEstuproAISP$centers, col=3:5, pch=17, cex=1)
  legend("topright", pch = 19, col = unique(cores), 
         legend = c("Cluster 1", "Cluster 2", "Cluster 3"))
dev.copy(png, file = "celEstuproAISP.png")
dev.off()

#Celular x Tentativa de Homicídio por mes
cores = kEstuproTentHomicidioMes$cluster+1
with(porMes, plot(estupro, tentHomicidio, col=cores, 
                  main = "Casos de Estupro x Tentativas de Homicídio no RJ\npor Mês - 2003 a Jun/2017",
                  xlab = "Casos de Estupro", ylab ="Tentativas de Homicídio"))
points(x = kEstuproTentHomicidioMes$centers, col=2:4, pch=17, cex=1)
legend("topright", pch = 19, col = unique(cores), 
       legend = c("Cluster 1", "Cluster 2", "Cluster 3"))
dev.copy(png, file = "estHomicidioMes.png")
dev.off()

#Celular x Tentativa de Homicídio por AISP
cores = kEstuproTentHomicidioAISP$cluster+3
with(porAISP, plot(estupro, tentHomicidio, col=cores, 
                   main = "Casos de Estupro x Tentativas de Homicídio no RJ\npor AISP - 2003 a Jun/2017",
                   xlab = "Casos de Estupro", ylab ="Tentativas de Homicídio"))
points(x = kEstuproTentHomicidioAISP$centers, col=4:6, pch=17, cex=1)
legend("topright", pch = 19, col = unique(cores), 
       legend = c("Cluster 1", "Cluster 2", "Cluster 3"))
dev.copy(png, file = "estHomicidioAISP.png")
dev.off()

#Veículos x Tentativa de Homicídio por mes
cores = kRouboVeiculoTentHomicidioMes$cluster+2
with(porMes, plot(rouboVeiculo, tentHomicidio, col=cores, 
                  main = "Roubo de Veículos x Tentativas de Homicídio no RJ\npor Mês - 2003 a Jun/2017",
                  xlab = "Roubo de Veículos", ylab ="Tentativas de Homicídio"))
points(x = kRouboVeiculoTentHomicidioMes$centers, col=3:6, pch=17, cex=1)
legend("topright", pch = 19, col = unique(cores), 
       legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"))
dev.copy(png, file = "rouboHomicidioMes.png")
dev.off()

#Roubo de Veículos x Tentativa de Homicídio por AISP
cores = kRouboVeiculoTentHomicidioAISP$cluster
with(porAISP, plot(rouboVeiculo, tentHomicidio, col=cores, 
                   main = "Roubo de Veículos x Tentativas de Homicídio no RJ\npor AISP - 2003 a Jun/2017",
                   xlab = "Roubo de Veículos", ylab ="Tentativas de Homicídio"))
points(x = kRouboVeiculoTentHomicidioAISP$centers, col=1:4, pch=17, cex=1)
legend("topright", pch = 19, col = unique(cores), 
       legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"))
dev.copy(png, file = "rouboHomicidioAISP.png")
dev.off()



