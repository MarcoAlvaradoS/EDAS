#'Summary of database
#'
#'@param dataset A database to summary
#'@return A summary of data base
#'@import DataExplorer
#'@export
Summ_data <- function(dataset){
  int_dat <- introduce(dataset)
  cat(paste("La base de datos consta de", int_dat$columns, "columnas de", int_dat$rows,
        "observaciones cada una y con un total de" , int_dat$total_missing_values,
        "valores perdidos.\n"))
   cat(paste("De estas columnas,", int_dat$discrete_columns,
        "son discretas,", int_dat$continuous_columns, "son continuas y", int_dat$all_missing_columns,
        "estan completamente vacias."))
}

#'Eliminate empty columns of dataset
#'
#'@param dataset A database to summary
#'@param perc A numeric value.
#'@return A dataset without empty columns
#'@export
all_miss_col <- function(dataset, perc){
  int_dat <- introduce(dataset)
  if(int_dat$all_missing_columns == 0){
    warning("No se tienen columnas completamente vacias.")
    return(dataset)
    }
  col_na <- which(colMeans(is.na(dataset))>(perc/100))
  return(dataset[,-col_na])
}

#'Change columns class
#'
#'@param dataset A database to use
#'@param columns A character vector with the names of columns to change
#'@param prop A character value with the class propriety: "factor" or "double"
#'@import tidyverse
#'@return A dataset object.
#'@export

cambio_prop <- function(dataset, columns, prop){
  if (!is.data.frame(dataset)) {return(warning("dataset is not a data.frame object"))}
  if (prop == "factor") {return(dataset %>% mutate_at(names(dataset[columns]), as.factor))}
  if (prop == "double") {return(dataset %>% mutate_at(names(dataset[columns]), as.double))}
}


#'Impute missing values
#'
#'@param dataset A database to use
#'@param method A character value with method name: "omit" or "RF"
#'@import missForest
#'@return A dataset object.
#'@export
imp_data <- function(dataset, method){
  if (!method %in% c("omit", "RF")) {warning("method not found");return(dataset)}
  if (method == "omit") {return(na.omit(dataset))}
  if (method == "RF") {return(missForest(dataset)$ximp)}
}

#'Summary of outliers and influence points
#'
#'@param data A database to use
#'@param var_est Acharacter value with the column name to study
#'@importFrom stats lm
#'@importFrom stats na.omit
#'@import olsrr
#'@import dplyr
#'@return A cat summary
#'@export
out_inf_points <- function(data, var_est){
  data_aux <- data[which(sapply(data, class)=="numeric")]
  data_aux2 <- data_aux[-which(names(data_aux)==var_est)]
  model <- lm(data_aux[[var_est]] ~ ., data = data_aux2)
  
  ati <- ols_plot_resid_stud_fit(model, print_plot = FALSE)
  out <- ati$plot$data[as.vector(ati$plot$data["color"] == "outlier"),]$obs
  inf <- ols_plot_cooksd_bar(model, print_plot = FALSE)
  ifp <- inf$plot$data[as.vector(inf$plot$data["color"] == "outlier"),]$obs
  
  a <- paste("Cuidado, se identificaron las siguientes observaciones con datos atipicos **",
             toString(out), "**\n Se recomienda verificar que esta no sea un error de captura y, de ser asi, eliminarla.")
  b <- paste("Cuidado, se identificaron que las siguientes observaciones tienen una alta influencia **",
             toString(ifp), "**\n Se recomienda verificar que esta no sea un error de captura y, de ser asi, eliminarla.")
  c <- "No se encontraron observaciones con datos atipicos."
  d <- "No se encontraron observaciones con una alta influencia"
  cat(if_else(length(out)>0, a, c), "\n")
  cat(if_else(length(ifp)>0, b, d),"\n")
  
}

#'Summary of all data
#'
#'@param data A database to use
#'@param var_est Acharacter value with the column name to study, NULL by default
#'@import olsrr
#'@import dplyr
#'@return A cat summary
#'@export
Summ_all_data <- function(data, var_est=NULL){
  int_dat <- introduce(data)
  ss1 <- paste("La base de datos consta de", int_dat$columns, "columnas de", int_dat$rows,
               "observaciones cada una y con un total de" , int_dat$total_missing_values,
               "valores perdidos.\n",
               "De estas columnas,", int_dat$discrete_columns,
               "son discretas,", int_dat$continuous_columns, "son continuas y", int_dat$all_missing_columns,
               "estan completamente vacias.")

  if(is.null(var_est)){
    return(cat(ss1))
  }else{
    data_aux <- data[which(sapply(data, class)=="numeric")]
    data_aux2 <- data_aux[-which(names(data_aux)==var_est)]
    model <- lm(data_aux[[var_est]] ~ ., data = data_aux2)

    ati <- ols_plot_resid_stud_fit(model, print_plot = FALSE)
    out <- ati$plot$data[as.vector(ati$plot$data["color"] == "outlier"),]$obs
    inf <- ols_plot_cooksd_bar(model, print_plot = FALSE)
    ifp <- inf$plot$data[as.vector(inf$plot$data["color"] == "outlier"),]$obs

    a <- paste("Cuidado, se identificaron las siguientes observaciones con datos atipicos **",
               toString(out), "**\n Se recomienda verificar que esta no sea un error de captura y, de ser asi, eliminarla.")
    b <- paste("Cuidado, se identificaron que las siguientes observaciones tienen una alta influencia **",
               toString(ifp), "**\n Se recomienda verificar que esta no sea un error de captura y, de ser asi, eliminarla.")
    c <- "No se encontraron observaciones con datos at?picos."
    d <- "No se encontraron observaciones con una alta influencia"
    cat(ss1, "\n")
    cat(if_else(length(out)>0, a, c), "\n")
    cat(if_else(length(ifp)>0, b, d), "\n")
  }
}

#'Create animated graphics
#'
#'@param data A database to use
#'@param varA1 A character value with the column of x axis
#'@param two_var A logical value: T if the graphic contains variable y; F if not
#'@param varA2 A character value with the column of y axis
#'@param group A character value with the column of group
#'@param type_graf A character value with the type of graphic
#'@import tidyverse
#'@import ggplot2
#'@import gganimate
#'@return A plot object
#'@export
animar <- function(data, varA1, two_var, varA2, group, type_graf){
  if("" %in% c(varA1, two_var, group, type_graf) | "NULL" %in% c(varA1, two_var, group, type_graf)){return()}

  data <- data %>%
    mutate(group1 = data[[group]], aux1 = data[[varA1]])

  if(two_var){
    if(varA2 %in% c("NULL", "")){return()}
    graf <- data %>%
      ggplot(aes_string(x= varA1, y= varA2))

    graf <- switch (type_graf,
                    "Scatterplot" = graf + geom_point(aes(colour=group1)) +
                      scale_colour_hue() +
                      transition_states(group1,
                                        transition_length = 3,
                                        state_length = 2) + shadow_mark() +
                      labs(title = paste("Scatterplot", varA1, "vs", varA2, "by", group),colour = group, subtitle = paste("State:" , "{closest_state}")),
                    "Columns" = graf + geom_col(aes_string(fill=varA1, group = 1L)) +
                      transition_states(aux1, wrap = FALSE) + shadow_mark() +
                      labs(title = "Grafico de barras", subtitle =  paste("Suma total de", if_else(class(data[[varA1]])=="factor", varA2, varA1),
                                                                          "by", if_else(class(data[[varA1]])=="factor", varA1, varA2)))
    )
  }else{
    graf <- switch (type_graf,
                    "Columns" = data %>%
                      group_by(group1) %>%
                      rename(temp = varA1) %>%
                      summarise(Media = mean(temp)) %>%
                      ggplot(aes(x= group1, y = Media)) +
                      geom_col(aes(fill=group1, group = 1L)) +
                      transition_states(group1, wrap = FALSE) +
                      shadow_mark() +
                      geom_text(aes(label = round(Media, 2), y = Media + 0.07), position = position_dodge(0.9)) +
                      labs(title = "Grafico de barras", subtitle =  paste("Media de", varA1,
                                                                          "by", group), fill = group)
    )
  }

  return(graf)
}


#'Create neural network
#'
#'@param data A database to use
#'@param var_est A character value with the column of x axis
#'@param vPerc A logical value: T if the graphic contains variable y; F if not
#'@param unitsE A character value with the column of y axis
#'@param activationE A character value with the column of group
#'@param lDrop A character value with the type of graphic
#'@param epochsE ---
#'@param batch_sizeE ---
#'@param validation_splitE ---
#'@import tidyverse
#'@import tidyr
#'@import keras
#'@return A plot object
#'@export
redes <- function(data, var_est, vPerc, unitsE, activationE, lDrop= NULL, epochsE, batch_sizeE, validation_splitE){
  if (!sum(sapply(data[-which(names(data)==var_est)], class) %in% c("numeric", "integer")) == length(data) -1) {
    cat(paste("Solo se admiten variables numericas como variables explicativas"))
  }else{

    modelF <- function(model,training,trainLabels, test, testLabels,testtarget, data){

      model %>%
        compile(loss = 'categorical_crossentropy',
                optimizer = 'adam',
                metrics = 'accuracy')

      history <- model %>%
        fit(training,
            trainLabels,
            epochs = epochsE,
            batch_size = batch_sizeE,
            validation_split = validation_splitE)

      ptest <- model %>%
        evaluate(test, testLabels)

      pred <- model %>%
        predict_classes(test)
      tble <- table(Predicted = pred, Actual = testtarget)

      return(list(model=model, ptest = ptest, predT = tble, data = data, plt = history))
    }

    aux <- which(names(data)==var_est)
    aux1 <- as.numeric(length(table(data[[var_est]])))
    data[[var_est]] <- as.numeric(data[[var_est]])
    data[[var_est]] <- data[[var_est]] - 1
    data <- as.matrix(data)
    data[,-aux] <- normalize(data[,-aux])
    dimnames(data) <- NULL
    ind <- sample(2, nrow(data), replace = T, prob = c((vPerc/100), 1-(vPerc/100)))
    training <- data[ind==1, -aux]
    test <- data[ind==2, -aux]
    trainingtarget <- data[ind==1, aux]
    testtarget <- data[ind==2, aux]
    trainLabels <- to_categorical(trainingtarget)
    testLabels <- to_categorical(testtarget)

    model <- keras_model_sequential()

    model %>%
      layer_dense(units = unitsE[1], activation = activationE[1], input_shape = c(ncol(data)-1))

    if (length(unitsE)==1) {
      model %>%
        layer_dense(units = aux1, activation = activationE[2])

      return(modelF(model,training,trainLabels, test, testLabels,testtarget,data))
    }else{
      model %>%
        layer_dropout(rate = (lDrop[1]/100))
    }

    if (length(unitsE)==2) {
      model %>%
        layer_dense(units = aux1, activation = activationE[2])

      return(modelF(model,training,trainLabels, test, testLabels,testtarget, data))
    }else{
      model %>%
        layer_dropout(rate = (lDrop[2]/100))
    }

    if (length(unitsE)==3) {
      model %>%
        layer_dense(units = aux1, activation = activationE[3])
      return(modelF(model,training,trainLabels, test, testLabels,testtarget, data))
    }
  }
}

#'Create model
#'
#'@param data A database to use
#'@param var_est A character value with the column of x axis
#'@param var_exp A logical value: T if the graphic contains variable y; F if not
#'@param intercep A character value with the column of y axis
#'@param method A character value with the column of group
#'@param percTest A character value with the type of graphic
#'@param ln_vars ---
#'@param ln_vars_tr ---
#'@param trC_method ---
#'@param trC_num ---
#'@param trC_rep ---
#'@import tidyverse
#'@import rsample
#'@import caret
#'@return A plot object
#'@export
crear_modelo <- function(data, var_est, var_exp, intercep=NULL, method, percTest,
                         ln_vars=F, ln_vars_tr = NULL, trC_method = "none",
                         trC_num, trC_rep){
  data <- data[c(var_est, var_exp)]

  if(ln_vars){data <- data %>% mutate_at(ln_vars_tr, log)}
  if(trC_num == 0){trC_num<- NA}
  if(trC_num == 0){trC_num<- NA}

  aux <- as.numeric(which(names(data)==var_est))
  data <- data %>% mutate(y = data[[var_est]])
  data <- data[-aux]
  train_test <- initial_split(data = data, (percTest/100))
  train <- training(train_test)
  test <- testing(train_test)

  if(intercep){
    model <- train(
      y ~ .,
      train,
      method = method,
      trControl = trainControl(
        method = trC_method,
        number = trC_num,
        repeats = trC_rep,
        verboseIter = F
      )
    )
  }else{
    model <- train(
      y ~ -1 + .,
      train,
      method = method,
      trControl = trainControl(
        method = trC_method,
        number = trC_num,
        repeats = trC_rep,
        verboseIter = F
      )
    )
  }

  return(list(modelo = model, test=test))
}


#'Significant correlations
#'
#'@param base A database to use
#'@param umbrales A numeric value with Significant umbrals
#'@importFrom stats cor
#'@return Significant correlations
#'@export
corSig <- function(base, umbrales){
  cormat <- cor(base)
  aux <- 0
  cat("Las variables:\n")
  for (umbral in umbrales) {
    for (i in 1:(nrow(cormat)-1)) {
      for (j in (i+1):ncol(cormat)) {
        if (abs(cormat[i,j]) > umbral & abs(cormat[i,j]) < umbral+.1) {
          cat(names(base)[i], "y", names(base)[j], "tienen una correlacion de", round(cormat[i,j],4), "\n")
          aux <- aux + 1
        }
      }
    }
  }
  
  if(aux/ncol(base) > 1/3){
    cat(paste("Dado que", aux, "de las columnas tiene una correlación significativa, es muy probable que se puedan reducir dimensiones."))
  }else{
    cat(paste("Dado que solo", aux, "de las columnas tiene una correlación significativa, va a ser complicado reducir dimensiones."))
  }   
}
