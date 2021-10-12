
translate_to_english <- function(table) {
   dictionary_file <- system.file("extdata", "dictionary_ALS.yaml", package = "alsr")
   dictionary <- labelmachine::lama_read(dictionary_file)

   labelmachine::lama_translate(table, dictionary,
                  Parameter = Parameter(Parameter),
                  Result = Result(Result),
                  Unit = Unit(Unit))
}




