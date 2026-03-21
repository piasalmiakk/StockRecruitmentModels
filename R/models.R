
#' @importFrom dplyr tibble mutate
#' @importFrom minpack.lm nlsLM



## - BEVERTON & HOLT - ##
###  MODEL  ###
#' @title Beverton & Holt model.
#' @description Fits a Beverton & Holt model using nls.
#' @param data Takes in the dataset
#' @param a_start Lets user choose a starting value for a
#' @param b_start Lets user choose a starting value for b
#' @examples
#' data <- tibble(
#' SSB = c(1000,2000,3000,4000,5000),
#' Recruitment = c(1500,2500,3500,4500,5500))
#'
#' fit <- bh_model(data)
#' coef(fit)
#'
#' pred <- predict_bh(fit,data)
#' head(pred)
#' @return returns a nlsLM model
#' @export

bh_model <- function(data, a_start = NULL, b_start = NULL) {

  scale_factor <- max(data$SSB, na.rm = TRUE)

  data <- data |>
    mutate(SSB = SSB / scale_factor)

  # setting good starting parameters if not given
  if (is.null(a_start)) {
    a_start = max(data$Recruitment, na.rm = TRUE)/max(data$SSB, na.rm = TRUE)
  }

  if (is.null(b_start)) {
    b_start <- mean(data$SSB, na.rm = TRUE)
  }


  # fitting nls
  nlsLM(
    Recruitment ~ a * SSB / (1 + SSB / b),
    data = data,
    start = list(
      a = a_start,
      b = b_start
    ),
    lower = c(1e-6, 1e-6),
    control = nls.lm.control(maxiter = 500)
  )
}


###  PREDICTION ###
#' Beverton & Holt prediction.
#' @description Predicts new data to plot the Beverton & Holt model.
#' @param model Takes in the Ricker model
#' @param data Takes in the dataset
#' @examples
#' data <- tibble(
#' SSB = c(1000,2000,3000,4000,5000),
#' Recruitment = c(1500,2500,3500,4500,5500))
#'
#' fit <- bh_model(data)
#' coef(fit)
#'
#' pred <- predict_bh(fit,data)
#' head(pred)
#' @param model,data
#' model takes in the Beverton & Holt model
#' data takes in the dataset
#' @return returns a predicted dataset
#' @export

predict_bh <- function(model, data) {

  scale_factor <- max(data$SSB, na.rm = TRUE)

  newdata <- data.frame(
    SSB = seq(min(data$SSB), max(data$SSB), length.out = 100)
  )

  newdata$SSB_scaled <- newdata$SSB / scale_factor

  newdata$Recruitment_pred <- predict(
    model,
    newdata = data.frame(SSB = newdata$SSB_scaled)
  )

  return(newdata)
}


## - RICKER - ##
###  MODEL  ###

#' Ricker model.
#' @param data Takes in the dataset
#' @param a_start Lets user choose a starting value for a
#' @param b_start Lets user choose a starting value for b
#' @description Fits a Ricker model using nls
#' @examples
#' data <- tibble(
#' SSB = c(1000,2000,3000,4000,5000),
#' Recruitment = c(1500,2500,3500,4500,5500))
#'
#' fit <- ricker_model(data)
#' coef(fit)
#'
#' pred <- predict_ricker(fit,data)
#' head(pred)
#' @return returns a nlsLM model
#' @export

ricker_model <- function(data, a_start = NULL, b_start = NULL){

  # setting good starting parameters if not given
  if (is.null(a_start)) {
    a_start = max(data$Recruitment / data$SSB, na.rm = TRUE)
  }

  if (is.null(b_start)) {
    b_start <- 1- mean(data$SSB, na.rm = TRUE)
  }

  # fitting nls
  nlsLM(
    Recruitment ~ a * SSB * exp(-b * SSB),
    data = data,
    start = list(a = a_start, b = b_start),
    lower = c(1e-6, 1e-6),
    control = nls.lm.control(maxiter = 500)
  )
}

###  PREDICTIONS ###

#' Ricker prediction.
#' @description Predicts new data to plot the Ricker model.
#' @param model Takes in the Ricker model
#' @param data Takes in the dataset
#' @examples
#' data <- tibble(
#' SSB = c(1000,2000,3000,4000,5000),
#' Recruitment = c(1500,2500,3500,4500,5500))
#'
#' fit <- ricker_model(data)
#' coef(fit)
#'
#' pred <- predict_ricker(fit,data)
#' head(pred)
#' @return returns a predicted dataset
#' @export

predict_ricker <- function(model,data){

  newdata <- tibble(
    SSB = seq(min(data$SSB), max(data$SSB),
              length.out = 100)
  )

  newdata$Recruitment_pred <- predict(model, newdata = newdata)

  return(newdata)
}
## - HOCKEY STICK - ##
###  MODEL  ###
#' Hockey Stick model.
#' @description Fits a Hockey Stick model using nls.
#' @param data Takes in the dataset
#' @param a_start Lets user choose a starting value for a
#' @param b_start Lets user choose a starting value for b
#' @examples
#' data <- tibble(
#' SSB = c(1000,2000,3000,4000,5000),
#' Recruitment = c(1500,2500,3500,4500,5500))
#'
#' fit <- hockey_model(data)
#' coef(fit)
#'
#' pred <- predict_hockey(fit,data)
#' head(pred)
#' @return returns a nlsLM model
#' @export

hockey_model <- function(data, a_start = NULL, b_start = NULL){

  # setting good starting parameters if not given
  if (is.null(a_start)) {
    a_start = max(data$Recruitment / data$SSB, na.rm = TRUE)
  }

  if (is.null(b_start)) {
    b_start <- median(data$SSB, na.rm = TRUE)
  }

  # fitting nls
  nlsLM(
    Recruitment ~ ifelse(SSB < b, a * SSB, a * b),
    data = data,
    start = list(a = a_start, b = b_start),
    lower = c(1e-6, 1e-6),
    control = nls.lm.control(maxiter = 500)
  )
}

### PREDICTION ###

#' Hockey Stick prediction.
#' #' @description Predicts new data to plot the Hockey Stick model.
#' @param model Takes in the Hockey Stick model
#' @param data Takes in the dataset
#' @examples
#' data <- tibble(
#' SSB = c(1000,2000,3000,4000,5000),
#' Recruitment = c(1500,2500,3500,4500,5500))
#'
#' fit <- hockey_model(data)
#' coef(fit)
#'
#' pred <- predict_hockey(fit,data)
#' head(pred)
#' @return returns a predicted dataset
#' @export

predict_hockey <- function(model, data){

  newdata <- tibble(
    SSB = seq(min(data$SSB), max(data$SSB), length.out = 100)
  )

  newdata$Recruitment_pred <- predict(model, newdata = newdata)

  return(newdata)
}
