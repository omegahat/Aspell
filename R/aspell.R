
setClass("AspellConfig", representation(ref = "externalptr"))
setClass("AspellSpeller", representation(ref = "externalptr"))
setClass("AspellDocChecker", representation(ref = "externalptr"))

setClass("DictInfo",
          representation(name = "character",
                         code = "character",
                         jargon = "character",
                         size =  "integer",
                         moduleName = "character"))


setClass("ModuleInfo",
          representation(name = "character",
                         order_num = "numeric",
                         lib_dir = "character",
                         dict_dirs =  "character",
                         dict_exts = "character"))


setClass("KeyInfo",
          representation(name = "character",
                         type = "integer",
                         def = "character",
                         desc =  "character",
#                         flags = "integer",
                         flags = "logical",                         
                         other_data = "integer"))


setMethod("show", "DictInfo",
           function(object) {
              show(paste(object@name, object@code, object@jargon, object@size, object@moduleName))
           })


#setMethod("$", "AspellConfig",
"$.AspellConfig" = 
           function(x, name) {
               ids = getSpellInfo(x, el = NULL, order = FALSE)
               i = pmatch(name, ids)
               if(is.na(i)) {
                 stop("name must be one of ", paste(ids, collapse = ", "))
               }
               as.vector(getSpellConfig(x, .values = ids[i]))
           }
#)

"$<-.AspellConfig" = 
           function(x, name, value) {
               v = list()
               v[[name]] = value
               setSpellConfig(x, .values = v)
               x
           }


names.AspellConfig =
function(x)
  getSpellInfo(x, el = NULL)

# Need to make a straight call to getSpellConfig.
"[.AspellConfig" = 
  function(x, i, j, ..., drop = TRUE) {
               ids = c(i,  ...)
               getSpellConfig(x, .values = as.character(ids))
   }


createKeyInfo =
function(name, type, def, desc, flags, other_data, class = "KeyInfo")
{
  new(class,
         name = name, type = type, def = def, desc = desc,  flags = flags, other_data = other_data)
}

createSpellConfig =
function(..., .values = NULL, class = "AspellConfig")
{
  conf = .Call("Raspell_getConfig", PACKAGE = "Aspell")
  conf = new(class, ref = conf)

  if(nargs() > 0) {
    k = match.call()
    k[[1]] = as.name("setSpellConfig")
    k$conf = conf
    eval(k)
  }

  conf
}

getSpellConfig =
function(conf = createSpellConfig(), ..., .values = NULL)
{
   if(missing(.values))  
      .values = as.character(unlist(list(...)))
   else
      .values = as.character(.values)

   info = getSpellInfo(conf)

   target = .values

   if(length(target) == 0)
     target = names(info)
   
   sapply(target,
           function(id) {
	      .Call("Raspell_configRetrieve", conf@ref, id, info[[id]]@type, PACKAGE = "Aspell")
          })

}


setSpellConfig = 
function(conf, ..., .values =  NULL)
{
   if(missing(.values))
     .values = list(...)


      # Convert the values to aspell values.
   .values = lapply(.values, function(x) if(is.logical(x))  c("false", "true")[x +1] else x)

   counts = sapply(.values, length)
   if(any(counts > 1)) {
     ids = names(.values)[counts > 1]
     
        # Check the types are lists and then
     info = getSpellInfo(conf, order = FALSE)

     w = sapply(info[ids], function(x) names(x@type) == "list")
     if(any(!w))
       stop("attempting to set multiple values for an aspell configuration option ", paste(ids[!w], collapse=","), " that does not support list vales")


       # Clear any existing lists for which we have multiple values.
     .Call("Raspell_setConfig", conf@ref, paste("clear", ids, sep = "-"),  rep("", length(ids)), PACKAGE = "Aspell")
     

     .values = c(.values[counts == 1], unlist(.values[counts > 1]))     
     names(.values)[seq(sum(counts == 1) + 1, length(.values))] = paste("add-", rep(ids, counts[counts > 1]), sep = "")
   } else
     .values = unlist(.values)

   .Call("Raspell_setConfig", conf@ref, names(.values),  as.character(.values), PACKAGE = "Aspell")
}

getSpellInfo =
function(conf = createSpellConfig(), extra = TRUE, el = createKeyInfo, order = TRUE)
{
   if(!is.null(el) && !is.function(el))
      stop("el must be a function that accepts 6 arguments")

   o = .Call("Raspell_getElements", conf@ref, as.logical(extra), el, PACKAGE = "Aspell")

     # If el is NULL, we just return the names. So we just sort these.
   if(is.character(o)) {
     if(order)
        return(sort(o))
     else
        return(o)
   } else {
      # otherwise, arrange the list in alphabetical order for names.
     if(order)
       o[order(names(o))]
     else
       return(o)
   }
}

createDictInfo =
function(name, code, jargon, size, sizeStr, moduleName)
{
  new("DictInfo", name = name, code = code, jargon = jargon, size = size, moduleName = moduleName)
}


getDictInfo =
function(conf = createSpellConfig(), el = createDictInfo)
{
  .Call("Raspell_getDictInfoList", conf@ref, el , PACKAGE = "Aspell")
}


createModuleInfo =
function(name, order_num, lib_dir, dict_dirs, dict_exts, class = "ModuleInfo")
{
  new(class, name = name, order_num = order_num, lib_dir = lib_dir, dict_dirs = dict_dirs,
             dict_exts = dict_exts)
}  

getModuleInfo =
function(conf = createSpellConfig(), el = createModuleInfo)
{
  if(is(conf, "AspellConfig"))
    conf = conf@ref
  .Call("Raspell_getModuleInfoList", conf, el , PACKAGE = "Aspell")
}


getConfig =
function(speller, class = "AspellConfig")
{
  conf = .Call("Raspell_getSpellerConfig", speller@ref, PACKAGE = "Aspell")
  new(class, ref = conf)
}  


getSpeller =
function(conf = createSpellConfig(), class = "AspellSpeller")
{
  speller = .Call("Raspell_getSpeller", conf@ref, PACKAGE = "Aspell")
  new(class, ref = speller)
}


setGeneric("aspell", function(words, suggests = FALSE, speller = getSpeller()) {
                      standardGeneric("aspell")
           })


setGeneric("spell", function(words, suggests = FALSE, speller = getSpeller()) {
                        # Note this dispatches to aspell. It is just an alias.  
                      standardGeneric("aspell")
           })


setOldClass(c("file", "connection"))
setOldClass(c("url", "connection"))
setOldClass(c("textConnection", "connection"))


setMethod("aspell", "connection",
          function(words, suggests = FALSE,  speller = getSpeller())
          {
             h = if(suggests) DocSpeller() else collectWords()
             spellDoc(words, speller = speller)
          })


setMethod("aspell", "ANY",
          function(words, suggests = FALSE,  speller = getSpeller())
          {

             vals = lapply(as.character(words), function(w) .Call("Raspell_spell", speller@ref, w, as.logical(suggests), PACKAGE = "Aspell"))

             if(!suggests)
               vals = unlist(vals)
             
             names(vals) = words
             
             vals
           })


saveWordLists =
function(speller)
{
   .Call("Raspell_saveWordLists", speller@ref, PACKAGE = "Aspell")
}  




addToList =
function(words, speller, session = TRUE)
{

    .Call("Raspell_addToList", speller@ref, as.character(words), as.logical(session), PACKAGE = "Aspell")
}

addCorrection =
function(speller, ..., .words, .pairs)
{
  if(!missing(.pairs)) {
    if(length(.pairs) %%2 != 0) 
       stop("Need even number of words for mis-spelled-spelled pairs")

    idx = seq(1, by = 2, length = length(.pairs)/2)
    .words = .pairs[idx + 1]
    names(.words) = .pairs[idx]

  } else  if(missing(.words)) {

    .words = list(...)
    ids = names(.words)

  }

  if(any(names(.words) == ""))
    stop("All words must have a name")

  
  .Call("Raspell_storeReplacement", speller@ref, names(.words), as.character(.words), PACKAGE = "Aspell")
}  

clearSession =
function(speller)
{

    .Call("Raspell_clearSession", speller@ref, PACKAGE = "Aspell")
}







WordListEnum = c(personal = 1, main = 2, session = 3)

getWordList =
function(speller = getSpeller(), which = names(WordListEnum))
{
  if(is.character(which)) {
     which = pmatch(which, names(WordListEnum))
     if(any(is.na(which)))
       stop("Invalid value of which in getWordList")
   }

  if( any(!(which %in% WordListEnum) ))
    stop("Invalid integer value for which in getWordList")

  tmp = lapply(which, function(i) .Call("Raspell_getWordList", speller@ref, as.integer(i), PACKAGE = "Aspell"))

  if(length(which) == 1)
    tmp[[1]]
  else {
    names(tmp) = names(WordListEnum)[which]
    tmp
  }
}  

# Convenience methods
setMethod("$", "AspellSpeller",
           function(x, name) {
              if(name %in% c("spell", "aspell", "suggest")) {
                return(
                        function(words, suggests = name == "suggests") {
                            aspell(words, suggests, x)
                        })
              } else if(!is.na(pmatch(name, "config")))
                return(getConfig(x))
              else if(name %in% names(WordListEnum))
                return(getWordList(x, name))
              else if(name == "save") {
                  return(function() saveWordLists(x))
              }


              stop("Unrecognized instruction ", name, " for AspellSpeller")
           })

setMethod("$<-", "AspellSpeller",
            function(x, name, value) {
                 if( name %in% c("session", "personal"))
                    addToList(value, x, session = (name == "session"))
                 else if(name == "correct") {
                    addCorrection(x, .words = value)               
                 } else
                   stop("Only recognize session and personal as fields to set for AspellSpeller.")                 

                  x
            })

