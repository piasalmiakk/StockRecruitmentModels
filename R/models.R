
# @import bslib, nav_panel, eller bslib::

## - BEVERTON & HOLT - ##
###  MODEL  ###
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

test4 <- bh_model1(test1,NULL,NULL)

###  PREDICTION ###
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

# ui <- bslib: blahba

## - RICKER - ##
###  MODEL  ###

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
predict_hockey <- function(model, data){

  newdata <- tibble(
    SSB = seq(min(data$SSB), max(data$SSB), length.out = 100)
  )

  newdata$Recruitment_pred <- predict(model, newdata = newdata)

  return(newdata)
}
