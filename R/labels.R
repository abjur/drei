lab_hora_one <- function(x) {
  sinal <- ""
  if (x < 0) {
    sinal <- "-"
  }
  x <- abs(x)
  dias <- x %/% 24
  if (dias == 1) {
    lab_dia <- paste0(dias, " dia e ")
  } else if (dias > 1) {
    lab_dia <- paste0(dias, " dias e ")
  } else {
    lab_dia <- ""
  }
  horas <- round(x %% 24)
  if (horas <= 1) {
    lab_hora <- paste0(horas, " hora")
  } else {
    lab_hora <- paste0(horas, " horas")
  }
  paste0(sinal, lab_dia, lab_hora)
}

#' Transforma horas em labels
#'
#' @param x numero de horas
#'
#' @export
lab_hora <- function(x) {
  purrr::map_chr(x, lab_hora_one)
}
