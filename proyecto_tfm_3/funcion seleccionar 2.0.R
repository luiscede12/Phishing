
seleccionar<-function(dataf,listconti,listclass,vardep)
  
{
  library(MASS)  
  library(dplyr)
  
  archivo<-dataf
  
    if (any(listclass==c(""))==TRUE)
  { 
    archivo<-archivo[,c(listconti,vardep)]
  } 
  
  if (any(listclass==c(""))==FALSE)
  { 
    if (any(listconti==c(""))==FALSE)
    {archivo<-archivo[,c(listconti,listclass,vardep)]}
    
    if (any(listconti==c(""))==TRUE)
    {archivo<-archivo[,c(listclass,vardep)]}
  } 
  
  # Estandarización a 0-1
  if (any(listconti==c(""))==FALSE)
  {
    normFunc <- function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}
    archivo[c(listconti)] <- apply(archivo[c(listconti)], 2, normFunc)
  }
  # LAS DE CLASE A FACTOR
  if (any(listclass==c(""))==FALSE)
  {
    archivo[c(listclass,vardep)]<-
      lapply(archivo[c(listclass,vardep)],factor)
  }
  

  formu1<-paste("factor(",vardep,")~.")
  formu2<-paste("factor(",vardep,")~1")

  # full.model <- glm(formula(formu1), data = archivo, family = binomial)
  #  step.model <- full.model %>% stepAIC(trace = TRUE)

  full<-glm(formu1,data=archivo,family = binomial(link="logit"))
  null<-glm(formu2,data=archivo,family = binomial(link="logit"))

    seleccion<-stepAIC(null,scope=list(upper=full),direction="both")
  cosa<-attr(terms(seleccion), "term.labels")

  # cosa<-attr(terms(step.model), "term.labels")
  
    # Para ver los efectos escogidos
    if (any(listclass==c(""))==FALSE)
  {
    listclass <- listclass[listclass %in%cosa]

    if (any(listconti==c(""))==FALSE)
    {
      listconti <- listconti[listconti %in%cosa]
    }

    }


  if (any(listclass==c(""))==TRUE)
  {
    listconti <- listconti[listconti %in%cosa]
  }

  if (any(listclass==c(""))==TRUE)
  {
    archivo<-archivo[,c(listconti,vardep)]
  }

  if (any(listclass==c(""))==FALSE)
  {
    if (any(listconti==c(""))==FALSE)
    {archivo<-archivo[,c(listconti,listclass,vardep)]}
    if (any(listconti==c(""))==TRUE)
    {archivo<-archivo[,c(listclass,vardep)]}
  }


  detach("package:dplyr", unload=TRUE)
  
return(list(archivo,listconti,listclass,vardep))  
}


# a<-seleccionar(archivo,listconti,listclass,vardep)


