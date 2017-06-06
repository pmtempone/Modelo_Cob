#Random forest # Wed May 24 09:58:34 2017 ------------------------------

library(caret)
library(rpart)
library(rattle)
library(rpart.plot)
library(pROC)
library(rpart.utils)
library(randomForest)
library(funModeling)
library(dplyr)
library(ranger)
library(corrplot) ## library wto plot that matrix
library(RColorBrewer) # to use the color pallete brewer.pal


#revision de correlacion de variables

correlacion <- as.matrix(as.data.frame(cor(train[,status_train$variable[c(3,5:9,11,13,16)]],use = 'pairwise.complete.obs')))

corrplot(correlacion, method="circle",#col=brewer.pal(n=10, name="PuOr"),
         type="lower", # only display upper diagonal
         tl.col="red", tl.cex = 0.9, tl.srt=90, #label color, size and rotation
         diag=FALSE, # dont print diagonal (var against itself)
         is.corr = T # accept a any matrix, mic in this case (not a correlation element),
         
)

#random forest con ranger # Wed May 24 10:19:03 2017 ------------------------------


#definicion  funcion ganancia para nuestro problema----


ganancia = function( probs, clases, prob )
{
  suma = 0 ;
  largo = length( clases ) ;
  #largo = length( abril_dataset_validation$clase_binaria1 )
  for( i in 1:largo )
  {
    if( probs[ i, "1"]  > prob   ){ suma <- suma + if( clases[i]=="1" ) { 4000-3*2.5 } else { -2.5*3 }  
    } ;
  }
  
  return( suma )
}


#Busca el punto de corte optimo, desde 0.01 a 0.10-----

umbral_ganancia_optimo = function( probs, clases)
{
  
  vgan_maxima = -9999999.0 ;
  vumbral =  0 ;
  
  
  #itero de 0.02 a 0.10  en incrementos de 0.01
  for( i in 0:180 ) 
  {
    vgan = ganancia(  probs, clases, 0.010 + i/1000 )
    # vgan = ganancia(   abril_validation_prediccion$prediction, abril_dataset_validation$clase_binaria1, 0.020 + 0/1000 )
    
    if( vgan > vgan_maxima )
    {
      vgan_maxima =  vgan ;
      vumbral =  0.010 +  i/1000 ;
    }
    
  }
  
  return( vumbral )
}

#generacion archivos----

archivo_entrada <- "C:/Users/pablo.tempone/Documents/Barrido/validation_ranger1.txt"
archivo_salida  <- "C:/Users/pablo.tempone/Documents/Barrido/salida_ranger_umbral.txt"

#escribo los  titulos  del archivo salida
if( !file.exists( archivo_salida) )
{
  cat( "fecha", "archivo", "algoritmo", "nulos", "tipo_umbral", "num.trees", "vmin.node.size", "umbral_promedio", "ganancia_promedio", "tiempo_promedio", "ganancias" , "umbrales",  "\n", sep="\t", file=archivo_salida, fill=FALSE, append=FALSE )
}

lineas_archivo <-  length( readLines(archivo_salida) )  - 1

#imputo los nulos, ya que ranger no acepta nulos----

#Otra opcion de imputacion, comentada
train[is.na(train)] <- 99999999
validation[is.na(validation)] <- 99999999
test_barrido[is.na(test_barrido)] <- 99999999

metodo_imputacion  <-  "99999999"


#algoritmo-----

semilla <- c( 102191, 200177, 410551, 552581)

linea <- 1
train$target <- factor(train$target)
validation$target <- factor(validation$target)

for(  vnum.trees  in  c( 50, 100, 200, 500, 800, 1000) )
{
  for(  vmin.node.size  in  c( 10000, 5000, 3000, 2000, 1000, 800, 700, 600, 500, 300, 200, 100, 50, 20) )
  {
    
    # El umbral  es  0.1
    ganancias <- c() 
    tiempos   <- c()
    
    if( linea > lineas_archivo  )
    {
      
        
        # generacion del modelo sobre los datos de training
        t0 =  Sys.time()
        fit.ranger.m1   <- ranger(target ~ ., data = train[,c(2,3,5:16)] , num.trees=vnum.trees,  min.node.size=vmin.node.size, probability=TRUE,num.threads = 3 )	
        t1 = Sys.time()
        tiempos[s] <-  as.numeric(  t1 - t0, units = "secs")
        
        
        # calculo la ganancia normalizada  en testing con probabilidad corte  0.1
        pred.ranger.validation  = predict(  fit.ranger.m1, validation  )
        ganancias[s] = ganancia( pred.ranger.validation$predictions,  validation$target,  0.1  )
        
      
      
      cat( format(Sys.time(), "%Y%m%d %H%M%S"), archivo_entrada, "ranger", metodo_imputacion, "umbral_fijo",  vnum.trees, vmin.node.size, 0.1, mean(ganancias), mean(tiempos), ganancias, "\n", sep="\t", file=archivo_salida, fill=FALSE, append=TRUE )
      
      
    }
    linea <-  linea + 1
    
  }
}

#analisis de parametros # Wed May 24 16:22:19 2017 ------------------------------

archivo_umbrales <- "C:/Users/pablo.tempone/Documents/Barrido/salida_ranger_umbral_variable.txt"
if( !file.exists( archivo_umbrales) )
{
  cat( "fecha", "archivo", "algoritmo", "nulos", "tipo_umbral", "num.trees", "vmin.node.size", "umbral_promedio", "ganancia_promedio", "tiempo_promedio", "ganancias" , "umbrales",  "\n", sep="\t", file=archivo_umbrales, fill=FALSE, append=FALSE )
}
linea <- 1
lineas_archivo <-  length( readLines(archivo_umbrales) )  - 1


for(  vnum.trees  in  c( 50, 500, 1000) )
{
  for(  vmin.node.size  in  c( 10000,15000)){
    
    # El umbral  es  0.1
    ganancias <- c() 
    tiempos   <- c()
    
    if( linea > lineas_archivo  )
    {
      
      
      # generacion del modelo sobre los datos de training
      t0 =  Sys.time()
      fit.ranger.m2   <- ranger(target ~ ., data = train[,c(2,3,5:16)] , num.trees=vnum.trees,  min.node.size=vmin.node.size, probability=TRUE,num.threads = 3 )	
      t1 = Sys.time()
      tiempos[s] <-  as.numeric(  t1 - t0, units = "secs")
      
      
      # calculo la ganancia normalizada  en testing con probabilidad corte  variable
      pred.ranger.validation  = predict(  fit.ranger.m2, validation  )

      # Se optimizan el  umbral de la probabilidad en   validation
      umbrales <- c()
      
      # generacion del modelo sobre los datos de training del 49%
      fit.ranger.m2   <- ranger(target ~ ., data = train[,c(2,3,5:16)] , num.trees=vnum.trees,  min.node.size=vmin.node.size, probability=TRUE,num.threads = 3 )	
      
      
      # determino el umbral optimo
  
      umbrales[s]  <-  umbral_ganancia_optimo( pred.ranger.validation$predictions,  validation$target ) 
      
      
 
      
      
      # calculo la ganancia normalizada  en testing
      ranger_testing_prediccion  = predict(  fit.ranger.m2,  test_barrido )
      ganancias[s] = ganancia( ranger_testing_prediccion$predictions,  test_barrido$target,  umbrales[s]  )
      
      
      
      cat( format(Sys.time(), "%Y%m%d %H%M%S"), archivo_entrada, "ranger", metodo_imputacion, "umbral_variable",  vnum.trees, vmin.node.size, umbrales, mean(ganancias), mean(tiempos), ganancias, "\n", sep="\t", file=archivo_umbrales, fill=FALSE, append=TRUE )
      
      
    }
    linea <-  linea + 1
    
  }
}

#ver casos
levels(validation$target) <- c("0","1")

levels(train$target) <- c("0","1")


fit.ranger.m2   <- ranger(target ~ ., data = train[,c(2,3,5:16)] , num.trees=500,  min.node.size=10000, probability=TRUE,num.threads = 3 )	

pred.ranger.validation  = predict(fit.ranger.m2, validation)

pred.ranger.validationn <- ifelse(pred.ranger.validation$predictions[,2] >= .12, 1,0)

table("predicho"=pred.ranger.validationn, "observado"=validation$target)



g <- roc(target ~ pred.ranger.validationn, data = validation)
plot(g, col="red")
g

pred.ranger.test  = predict(fit.ranger.m2, test_barrido)

pred.ranger.testn <- ifelse(pred.ranger.test$predictions[,2] > .05, 1,0)

densidad_pred <- data.frame(prob=pred.ranger.test$predictions[,2],target=factor(test_barrido$target))
densidad_pred$dia <- test_barrido$nro_dia_habil
densidad_pred$target <- as.character(densidad_pred$target)
table("predicho"=pred.ranger.testn, "observado"=test_barrido$target)

#punto de corte variable al dia
ggplot(data = densidad_pred,mapping = aes(x=prob,fill=target))+geom_density(alpha=0.5)+facet_grid(dia ~ .)+
 scale_x_continuous(breaks=seq(0,1,.025))+ theme(axis.title.y =element_blank(),
                                                 axis.text.y=element_blank(),
                                                 axis.ticks.y=element_blank())+labs(x="Probabilidad",fill="Tiene plata?")



densidad_pred$pred[densidad_pred$dia<13] <- ifelse(densidad_pred$prob[densidad_pred$dia<13] > .025, 1,0)
densidad_pred$pred[densidad_pred$dia>=13] <- ifelse(densidad_pred$prob[densidad_pred$dia>=13] > .0125, 1,0)

table("predicho"=densidad_pred$pred[densidad_pred$dia<13], "observado"=densidad_pred$target[densidad_pred$dia<13])
table("predicho"=densidad_pred$pred[densidad_pred$dia>=13], "observado"=densidad_pred$target[densidad_pred$dia>=13])

table("predicho"=densidad_pred$pred, "observado"=densidad_pred$target)


#----prueba con validation----

pred.ranger.val  = predict(fit.ranger.m2, validation)

pred.ranger.valn <- ifelse(pred.ranger.val$predictions[,2] > .05, 1,0)

densidad_pred.val <- data.frame(prob=pred.ranger.val$predictions[,2],target=factor(validation$target))
densidad_pred.val$dia <- validation$nro_dia_habil
densidad_pred.val$target <- as.character(densidad_pred.val$target)
table("predicho"=pred.ranger.valn, "observado"=validation$target)

#punto de corte variable al dia
ggplot(data = densidad_pred.val,mapping = aes(x=prob,fill=target))+geom_density(alpha=0.5)+facet_grid(dia ~ .)+
  scale_x_continuous(breaks=seq(0,1,.025))+ theme(axis.title.y =element_blank(),
                                                  axis.text.y=element_blank(),
                                                  axis.ticks.y=element_blank())+labs(x="Probabilidad",fill="Tiene plata?")


densidad_pred.val$pred[densidad_pred.val$dia<13] <- ifelse(densidad_pred.val$prob[densidad_pred.val$dia<13] > .025, 1,0)
densidad_pred.val$pred[densidad_pred.val$dia>=13] <- ifelse(densidad_pred.val$prob[densidad_pred.val$dia>=13] > .0125, 1,0)

table("predicho"=densidad_pred.val$pred[densidad_pred.val$dia<13], "observado"=densidad_pred.val$target[densidad_pred.val$dia<13])
table("predicho"=densidad_pred.val$pred[densidad_pred.val$dia>=13], "observado"=densidad_pred.val$target[densidad_pred.val$dia>=13])


table("predicho"=densidad_pred.val$pred, "observado"=densidad_pred.val$target)
