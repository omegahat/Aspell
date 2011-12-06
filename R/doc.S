
# Some classes to represent spelling mistakes
# A token giving the location within the original text.

   # A class with two slots - offset and length, or an integer vector of length 2.
setClass("AspellToken", representation(offset = "integer", length = "integer"))


# The entire replacement - original mis-spelled word, correction and the location  in the text.
setClass("SpellingReplacement",
           representation(correction = "character",
                          word = "character",
                          location = "AspellToken")
                         )


# Classes for representing callbacks for the document processing that goes on.
# These can be used elsewhere.
# A callback is simply a function.
# But a CallbackResult provides a way for us to get at the result.
setClass("Callback", contains = "function")
setClass("CallbackResult", representation("Callback", variable = "character"))


# A function to generate a callback with a access to the result
DocSpeller =
function(correct = TRUE, class = "CallbackResult", elClass = "SpellingReplacement")
{
  replacements <- list()

  h =  function(word, tok, txt, speller) {
    alt = aspell(word, suggests = TRUE, speller = speller)[[1]]

    cat("Correction for", word, "\n 0 for leave 'as is'\n")
    which = menu(alt)

    if(which != 0)  {
      i = length(replacements)  + 1    
      replacements[[i]] <<- new(elClass, correction = alt[which], word = word, location = new("AspellToken", offset = as.integer(tok[1] + 1),
                                                                                                          length = as.integer(tok[2])))
      names(replacements)[i] <<- word

      return(replacements[[i]])

      if(correct) {
        addCorrection(speller, .pairs = c(word, alt[which]))
      }
    } else if(correct) {
       addToList(word, speller)
    }

    FALSE
  }

 o = new("CallbackResult", h)
 o@variable = "replacements"

 o    
}  


collectWords =
function(class = "CallbackResult")
{
  words <- character(0)
  
  f = function(word, tok, txt, speller)  {
     words <<- c(words, word)    
  }

  new(class, f, variable = "words")
}  




setGeneric("spellDoc",
             function(doc, wordHandler = DocSpeller(),
                       checker = docChecker(speller), speller = getSpeller(conf), conf = createSpellConfig())
                        standardGeneric("spellDoc")
          )

spellDocCB =
          function(doc,
                   wordHandler = DocSpeller(),
                   checker = docChecker(speller), speller = getSpeller(conf), conf = createSpellConfig())
          {

            o = wordHandler
            wordHandler = as(wordHandler, "Callback")
            callNextMethod()
            getResult(o)
          }  

setMethod("spellDoc",  c(wordHandler = "CallbackResult"), spellDocCB)
setMethod("spellDoc",  c(wordHandler = "missing"), spellDocCB)


setGeneric("getResult", function(obj, ...) standardGeneric("getResult"))

setMethod("getResult", "CallbackResult",
             function(obj, ...) {
                v = mget(obj@variable,  environment(as(obj, "function")))
                if(length(obj@variable) == 1)
                  return(v[[1]])

                v
             })

setMethod("spellDoc", "connection",
          function(doc,
                   wordHandler = DocSpeller(),
                   checker = docChecker(speller), speller = getSpeller(conf), conf = createSpellConfig()) {
              doc = paste(readLines(doc), collapse = "\n")
              callNextMethod()
          }
          )

setMethod("spellDoc",
          c(wordHandler = "function"),
          function(doc,
                   wordHandler = DocSpeller(),
                   checker = docChecker(speller), speller = getSpeller(conf), conf = createSpellConfig())
          {
            if(!missing(checker))
              reset(checker)

            if(is.character(doc) && file.exists(doc)) 
              doc = paste(readLines(doc), collapse = "\n")            

            process(checker, doc)

            while(TRUE) {

              tok = Next(checker)
              if(tok[1] == 0 || tok[2] == 0)
                break

              word = substring(doc, tok[1] + 1, sum(tok))
              wordHandler(word, tok, doc, speller)
            }

   
            invisible(checker)
          })

docChecker =
function(speller = getSpeller(), class = "AspellDocChecker")
{
  o = .Call("Raspell_newDocumentChecker", speller@ref, PACKAGE = "Aspell")
  new(class, ref = o)
}  

reset =
function(checker)
{
    .Call("Raspell_documentCheckerReset", checker@ref, PACKAGE = "Aspell")
}


process =
function(checker, text)
{
    .Call("Raspell_documentCheckerProcess", checker@ref, paste(as.character(text), collapse = "\n"), PACKAGE = "Aspell")
}


Next =
function(checker)
{
    .Call("Raspell_documentCheckerNext", checker@ref, PACKAGE = "Aspell")
}

