# Vedi anche file esporta_cubi_px.R, dove la funzione data2px era stata creata 
# per la prima volta

# Scrivere un'altra versione, che si ferma alla creazione dell'array (senza
# trasformare l'array in px, che necessita del pacchetto pxR)

# Scrivo una funzione che fa i passi 1-4

#' Trasforma un cubo di dati da data frame a oggetto di classe 'px'
#' 
#' @param .data cubo di dati in forma di data frame
#' @param id.vars vettore character con i nomi delle variabili categoriali del cubo di dati
#' @param measure.vars vettore character con i nomi delle colonne che contengono le statistiche del cubo
#' @param variable.name nome da dare alla colonna che conterrà il nome della misura (default = "misura")
#' 
#' @examples 
#' data(c09C)
#' 
#' str(c09C)
#' head(c09C)
#' 
#' # Trasforma il data frame c09C in oggetto di classe 'px'
#' c09C_px <- data2px(c09C, 
#'                    id.vars = c("anno", "posizione", "sesso", "statuto"),
#'                    measure.vars = c("p10", "p25", "p50", "p75", "p90"))
#'                    
#' class(c09C_px)
#' 
#' @export
data2px <- function(.data, id.vars, measure.vars, variable.name = "misura"){
  # 1. melt
  .data_melt <- reshape2::melt(.data, id.vars = id.vars, measure.vars = measure.vars, variable.name = "misura")
  
  # 2. Spostamento della colonna "misura" al primo posto
  colonne <- colnames(.data_melt)
  k <- which(colonne == variable.name)
  .data_melt <- .data_melt[, c(colonne[k], colonne[-k])]
  
  # 3. Trasformazione del cubo da data frame a array
  k <- ncol(.data_melt) - 1L
  dim_array <- sapply(.data_melt[ , 1:k], function(x) length(unique(x)))
  dimnames_array <- lapply(.data_melt[ , 1:k], function(x) as.character(unique(x)))
  
  # Inverto l'ordine di dim_array e dimnames_array
  dim_array <- rev(dim_array)
  dimnames_array <- rev(dimnames_array)
  
  .data_array <- array(data = .data_melt$value, dim = dim_array, dimnames = dimnames_array)
  
  # 4. L'array viene trasformato in un oggetto di class px, che viene poi esportato
  .data_px <- pxR::as.px.array(.data_array)
  .data_px
}
# write.px(c08C_px, filename = "c08C.px")
