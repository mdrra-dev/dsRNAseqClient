
#' @export
ds.createSE<- function(rnaseq, metadata, coldata_column = NULL, rowdata = NULL, rowdata_column = NULL, newobj_name = NULL, datasources = NULL){

  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }

  # Check if required new object name is missing
  if(is.null(newobj_name)){

    stop(sprintf("Missing new object name."))
  }

  # Check classes on server
  class_rnaseq <- dsBaseClient:::checkClass(datasources, rnaseq)
  class_metadata <- dsBaseClient:::checkClass(datasources, metadata)

  # Check rnaseq object class on all the servers
  if (!all(class_rnaseq %in% "data.frame")) {
    stop(sprintf("rnaseq not a dataframe on all the servers"))
  }

  # Check metadata object class on all the servers
  if (!all(class_metadata %in% "data.frame")) {
    stop(sprintf("metadata not a dataframe on all the server"))
  }

  if (!is.null(rowdata)){
    class_rowdata <- dsBaseClient:::checkClass(datasources, rowdata)

    if (!all(class_rowdata %in% "data.frame")) {
      stop(sprintf("rowdata not a dataframe on all the servers"))
    }
  }

  coldata_col_expr  <- if (is.null(coldata_column)) "NULL" else shQuote(coldata_column)
  rowdata_obj_expr  <- if (is.null(rowdata))        "NULL" else rowdata           # simbolo server (non quotato)
  rowdata_col_expr  <- if (is.null(rowdata_column)) "NULL" else shQuote(rowdata_column)

  function_call <- paste0("createSEDS(", rnaseq, ", ", metadata,", ", coldata_col_expr, ", ", rowdata_obj_expr, ", ", rowdata_col_expr,")")

  DSI::datashield.assign.expr(datasources, newobj_name, function_call)

}
