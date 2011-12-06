#include "aspell.h"
#include "Rdefines.h"

static SEXP R_makeSpellReference(void *ptr, const char *tag, R_CFinalizer_t fin);
static void *R_getSpellReference(SEXP el, const char *tag);
static SEXP Raspell_createDictInfo(const AspellDictInfo *info, SEXP fn);
static SEXP Raspell_createKeyInfo(const AspellKeyInfo *info, SEXP f);

static void Raspell_freeSpeller(SEXP el);
static void Raspell_freeDocumentChecker(SEXP el);
static void Raspell_freeConfig(SEXP el);


static SEXP Raspell_convertList(const AspellStringList *els);
static SEXP Raspell_convertStringEnumeration(AspellStringEnumeration *els);





typedef struct {
    char *name;
    int value;
} BitMeaning;


const BitMeaning KeyInfoBitMeanings [] = {
    { "modifiable", 1 << 0},
    { "utf8", 1 << 1},
    { "hidden", 1 << 2},
    { "common", 1 << 4}
};

SEXP
getBitSettings(int val, const BitMeaning *meanings, int numEls)
{
    SEXP els, names;
    int i;

    PROTECT(els = NEW_LOGICAL(numEls));
    PROTECT(names = NEW_CHARACTER(numEls));
    for(i = 0; i < numEls; i++) {
	SET_STRING_ELT(names, i, COPY_TO_USER_STRING(meanings[i].name));
	if(val & meanings[i].value) {
	    LOGICAL(els)[i] = 1;
	}
    }

    SET_NAMES(els, names);
    UNPROTECT(2);
    return(els);
}

/**************************************************************************************************/


/* 
  Get an AspellConfig object.
*/
SEXP
Raspell_getConfig()
{
  AspellConfig *cnf;

  cnf = new_aspell_config();
  return(R_makeSpellReference(cnf, "AspellConfig", Raspell_freeConfig));
}

SEXP
Raspell_setConfig(SEXP config, SEXP names, SEXP values)
{
  int i, n;
  AspellConfig *conf;
  SEXP ans;

  conf = (AspellConfig *) R_getSpellReference(config, "AspellConfig");

  n = GET_LENGTH(names);
  PROTECT(ans = NEW_INTEGER(n));
  for(i = 0; i < n; i++) {
      INTEGER(ans)[i] = aspell_config_replace(conf, CHAR(STRING_ELT(names, i)), CHAR(STRING_ELT(values, i)));
  }

  UNPROTECT(1);

  return(ans);
}

/*
  If func == NULL, then return just the names.
*/

SEXP
Raspell_getElements(SEXP config, SEXP extra, SEXP func)
{
  int i, n, len = 10, nprotect = 0;
  AspellConfig *conf;
  AspellKeyInfoEnumeration *els;
  const AspellKeyInfo *info;
  SEXP names, values;

  conf = (AspellConfig *) R_getSpellReference(config, "AspellConfig");


  els = aspell_config_possible_elements(conf, LOGICAL(extra)[0]);

  if(!els) {
#if 0
    fprintf(stderr, "No elements in aspell config\n");
#endif
    return(R_NilValue);
  }

  i = 0;
  PROTECT(names = NEW_CHARACTER(len));
  PROTECT(values = allocVector(VECSXP, len));
  nprotect = 2;

  while( (info = aspell_key_info_enumeration_next(els) ) ) {

    if(!info) {
      PROBLEM "Empty info returned from info_enumeration_next: %s\n",	aspell_config_error_message(conf)
	  WARN;
      break;
    }

    if(i == len) {
      len *= 2;
      PROTECT(names = SET_LENGTH(names, len));
      PROTECT(values = SET_LENGTH(values, len));
      nprotect += 2;
    }

    SET_STRING_ELT(names, i, COPY_TO_USER_STRING(info->name));
    if(GET_LENGTH(func))
      SET_VECTOR_ELT(values, i, Raspell_createKeyInfo(info, func));

    i++;
  }

  if(len > i) {
      PROTECT(names = SET_LENGTH(names, i));
      PROTECT(values = SET_LENGTH(values, i));
      nprotect += 2;
  }



  if(GET_LENGTH(func))
    SET_NAMES(values, names);
  else
    values = names;

  delete_aspell_key_info_enumeration(els);

  UNPROTECT(nprotect);

  return(values);
}


static const char *
getKeyInfoTypeName(enum AspellKeyInfoType type)
{
  const char *ptr = NULL;

  if(type == AspellKeyInfoString)
    ptr = "string";
  else if(type == AspellKeyInfoInt)
    ptr = "int";
  else if(type == AspellKeyInfoBool)
    ptr = "boolean";
  else if(type == AspellKeyInfoList)
    ptr = "list";

  return(ptr);
}

static SEXP
Raspell_createKeyInfo(const AspellKeyInfo *info, SEXP f)
{
  SEXP e, ans, tmp;

  PROTECT(e = allocVector(LANGSXP, 7));

  SETCAR(e, f); tmp = CDR(e);

  SETCAR(tmp, mkString(info->name)); tmp = CDR(tmp);
  PROTECT(ans = ScalarInteger(info->type));
  SETCAR(tmp, ans); tmp = CDR(tmp);
  SET_NAMES(ans, mkString(getKeyInfoTypeName(info->type)));
  SETCAR(tmp, mkString(info->def)); tmp = CDR(tmp);
  SETCAR(tmp, mkString(info->desc ? info->desc : "")); tmp = CDR(tmp);
  SETCAR(tmp, getBitSettings(info->flags, KeyInfoBitMeanings, sizeof(KeyInfoBitMeanings)/sizeof(KeyInfoBitMeanings[0])) /* ScalarInteger(info->flags)*/ ); tmp = CDR(tmp);

  SETCAR(tmp, ScalarInteger(info->other_data)); 


  ans = Rf_eval(e, R_GlobalEnv);

  UNPROTECT(2);

  return(ans);
}


static SEXP
Raspell_convertStringEnumeration(AspellStringEnumeration *els)
{
    SEXP ans;
    const char *w;

    int i = 0, len = 10, nprotect = 0;

    PROTECT(ans = NEW_CHARACTER(len));
    nprotect = 1;

    while( (w = aspell_string_enumeration_next(els)) != 0) {
	if(i == len) {
	    len *= 2;
	    PROTECT(ans = SET_LENGTH(ans,  len));
	    nprotect ++;
	}

	SET_STRING_ELT(ans, i, COPY_TO_USER_STRING(w));
	i++;
    }

    if(i < len) {
	PROTECT(ans = SET_LENGTH(ans,  i));
	nprotect ++;
    }

    UNPROTECT(nprotect);

    return(ans);
}




static SEXP
Raspell_convertList(const AspellStringList *lst)
{
    return(Raspell_convertStringEnumeration(aspell_string_list_elements(lst)));
}


SEXP
Raspell_configRetrieve(SEXP config, SEXP id, SEXP type)
{
  AspellConfig *conf;
  const char *name;
  SEXP ans = R_NilValue;

  conf = (AspellConfig *) R_getSpellReference(config, "AspellConfig");

  name = CHAR(STRING_ELT(id, 0));

  switch(INTEGER(type)[0]) {
      case AspellKeyInfoBool:
	ans = ScalarLogical(aspell_config_retrieve_bool(conf, name));
	break;
      case AspellKeyInfoInt:
	ans = ScalarInteger(aspell_config_retrieve_int(conf, name));
	break;
      case AspellKeyInfoString:
	ans = mkString(aspell_config_retrieve(conf, name));
	break;
      case AspellKeyInfoList:
      {
  	  AspellStringList * lst = new_aspell_string_list();
	  AspellMutableContainer * lst0  = aspell_string_list_to_mutable_container(lst);

	  if(aspell_config_retrieve_list(conf, name, lst0) != -1)
	      ans = Raspell_convertList(lst);
      }
	break;
  }


  return(ans);
}



/************************************************************************************/

SEXP
Raspell_getDictInfoList(SEXP config, SEXP func)
{
  AspellConfig *conf;
  struct AspellDictInfoList *dlist;
  struct AspellDictInfoEnumeration *els;
  const AspellDictInfo *el;
  SEXP ans, names;
  int i = 0, len = 10, nprotect = 0;

  if(GET_LENGTH(config)) {
     conf = (AspellConfig *) R_getSpellReference(config, "AspellConfig");
  } else {
     conf = new_aspell_config();
  }


  dlist = get_aspell_dict_info_list(conf);


  if(!GET_LENGTH(config)) {
    delete_aspell_config(conf);
  }

  els = aspell_dict_info_list_elements(dlist);

  len = aspell_dict_info_list_size(dlist);
  len = 10;

  PROTECT(names = NEW_CHARACTER(len));
  PROTECT(ans = allocVector(VECSXP, len));
  nprotect = 2;

  while( (el = aspell_dict_info_enumeration_next(els)) != NULL) {
    if(i == len) {
      len *= 2;
      PROTECT(ans = SET_LENGTH(ans, len * 2));
      PROTECT(names = SET_LENGTH(names, len * 2));

      nprotect += 2;
    }

    SET_VECTOR_ELT(ans, i, Raspell_createDictInfo(el, func));   
    SET_STRING_ELT(names, i, mkChar(el->name));

    i++;
  }


  if(len > i) {
      PROTECT(ans = SET_LENGTH(ans, i));
      PROTECT(names = SET_LENGTH(names, i));

      nprotect += 2;
  }

  UNPROTECT(nprotect);
 
  SET_NAMES(ans, names);

  delete_aspell_dict_info_enumeration(els);

  return(ans);
}

SEXP
Raspell_createDictInfo(const AspellDictInfo *info, SEXP fn)
{
  SEXP e, ans;
  PROTECT(e = allocVector(LANGSXP, 7));
  SETCAR(e, fn);
  SETCAR(CDR(e), mkString(info->name));
  SETCAR(CDR(CDR(e)), mkString(info->code));
  SETCAR(CDR(CDR(CDR(e))), mkString(info->jargon));
  SETCAR(CDR(CDR(CDR(CDR(e)))), ScalarInteger(info->size));
  SETCAR(CDR(CDR(CDR(CDR(CDR(e))))), mkString(info->size_str));
  SETCAR(CDR(CDR(CDR(CDR(CDR(CDR(e)))))), mkString(info->module->name));

  ans = Rf_eval(e, R_GlobalEnv);
  UNPROTECT(1);

  return(ans);
}




SEXP
Raspell_createModuleInfo(const AspellModuleInfo *info, SEXP fn)
{
  SEXP e, ans;

  PROTECT(e = allocVector(LANGSXP, 6));

  SETCAR(e, fn);
  SETCAR(CDR(e), mkString(info->name));
  SETCAR(CDR(CDR(e)), ScalarReal(info->order_num));
  SETCAR(CDR(CDR(CDR(e))), mkString(info->lib_dir ? info->lib_dir : ""));
/*XXX Leave this one blank for now as it is causing seg faults. */
  if(0 && info->dict_dirs && !aspell_string_list_empty(info->dict_dirs)) {
      SETCAR(CDR(CDR(CDR(CDR(e)))),  Raspell_convertList(info->dict_dirs)); 
  } else {
      PROTECT(ans = NEW_CHARACTER(1));
      SET_STRING_ELT(ans, 0, R_NaString);
      SETCAR(CDR(CDR(CDR(CDR(e)))),  ans); 
      UNPROTECT(1);
  }

  SETCAR(CDR(CDR(CDR(CDR(CDR(e))))), Raspell_convertList(info->dict_exts));

  ans = Rf_eval(e, R_GlobalEnv);
  UNPROTECT(1);

  return(ans);
}



SEXP
Raspell_getModuleInfoList(SEXP config, SEXP func)
{
  AspellConfig *conf;
  struct AspellModuleInfoList *dlist;
  struct AspellModuleInfoEnumeration *els;
  const AspellModuleInfo *el;
  SEXP ans = NULL, names = NULL;
  int i = 0, len = 10, nprotect = 0;

  if(GET_LENGTH(config)) {
     conf = (AspellConfig *) R_getSpellReference(config, "AspellConfig");
  } else {
     conf = new_aspell_config();
  }


  dlist = get_aspell_module_info_list(conf);


  if(!GET_LENGTH(config)) {
    delete_aspell_config(conf);
  }

  els = aspell_module_info_list_elements(dlist);

  len = 10;

  PROTECT(names = NEW_CHARACTER(len));
  PROTECT(ans = allocVector(VECSXP, len));
  nprotect = 2;

  while( (el = aspell_module_info_enumeration_next(els)) != NULL) {
    if(i == len) {
      len *= 2;
      PROTECT(ans = SET_LENGTH(ans, len * 2));
      PROTECT(names = SET_LENGTH(names, len * 2));

      nprotect += 2;
    }

    SET_VECTOR_ELT(ans, i, Raspell_createModuleInfo(el, func));   
    SET_STRING_ELT(names, i, mkChar(el->name));

    i++;
  }


  if(len > i) {
      PROTECT(ans = SET_LENGTH(ans, i));
      PROTECT(names = SET_LENGTH(names, i));

      nprotect += 2;
  }

  UNPROTECT(nprotect);
 
  SET_NAMES(ans, names);

  delete_aspell_module_info_enumeration(els);

  return(ans);
}



SEXP
Raspell_getSpeller(SEXP config)
{
  AspellConfig *conf;
  AspellSpeller *speller;
  AspellCanHaveError *err;

  conf = (AspellConfig *) R_getSpellReference(config, "AspellConfig");

  err = new_aspell_speller(conf);
  if(aspell_error_number(err) != 0) {
    PROBLEM  "%s", aspell_error_message(err) 
    ERROR;
  }

  speller = to_aspell_speller(err);

  return(R_makeSpellReference(speller, "AspellSpeller", Raspell_freeSpeller));
}


SEXP
Raspell_getSpellerConfig(SEXP speller)
{
  AspellConfig *cnf;
  AspellSpeller *spell = (AspellSpeller *) R_getSpellReference(speller, "AspellSpeller");

  cnf = aspell_speller_config(spell);
  return(R_makeSpellReference(cnf, "AspellConfig", NULL));
}



SEXP
Raspell_spell(SEXP speller, SEXP words, SEXP suggests)
{
  AspellSpeller *spell = (AspellSpeller *) R_getSpellReference(speller, "AspellSpeller");
  const AspellWordList * suggestions;
  const char * word;
  AspellStringEnumeration *els;
  SEXP ans;

  word = CHAR(STRING_ELT(words, 0));

  if(! LOGICAL(suggests)[0]) {
     int status  = aspell_speller_check(spell, word, strlen(word));

     return(ScalarLogical(status));
  }

  suggestions = aspell_speller_suggest(spell, word, strlen(word));
  els = aspell_word_list_elements(suggestions);

  ans = Raspell_convertStringEnumeration(els);


  delete_aspell_string_enumeration(els); 
     
  return(ans);
}


SEXP
Raspell_addToList(SEXP speller, SEXP s_word, SEXP session)
{
  AspellSpeller *spell = (AspellSpeller *) R_getSpellReference(speller, "AspellSpeller");
  const char * word;
  int status;
  int i, n;
  SEXP ans;

  n = GET_LENGTH(s_word);
  PROTECT(ans = NEW_INTEGER(n));
  for(i = 0; i < n; i++) {
      word = CHAR(STRING_ELT(s_word, i));
      if(LOGICAL(session)[0])
	  status = aspell_speller_add_to_session(spell, word, strlen(word));
      else 
	  status = aspell_speller_add_to_personal(spell, word, strlen(word));
      INTEGER(ans)[i] = status;
  }

  UNPROTECT(1);

  return(ans);
}

SEXP
Raspell_storeReplacement(SEXP speller, SEXP original, SEXP corrections)
{
  AspellSpeller *spell = (AspellSpeller *) R_getSpellReference(speller, "AspellSpeller");
  SEXP ans;
  int i, n;

  n = GET_LENGTH(original);

  PROTECT(ans = NEW_INTEGER(n));

  for(i = 0; i < n; i++) {
      char *mis, *cor;
      mis = CHAR(STRING_ELT(original, i));
      cor = CHAR(STRING_ELT(corrections, i));

      INTEGER(ans)[i] = aspell_speller_store_replacement(spell, mis, strlen(mis), cor, strlen(cor));
  }

  UNPROTECT(1);

  return(ans);
}


SEXP
Raspell_clearSession(SEXP speller)
{
  AspellSpeller *spell = (AspellSpeller *) R_getSpellReference(speller, "AspellSpeller");
  int status;
  status = aspell_speller_clear_session(spell);
  if(status == 0) {
      PROBLEM "aspell_speller_clear_session  %s", aspell_speller_error_message(spell)
	  ERROR;
  }

  return(ScalarInteger(status));
}

/* Cause the word lists to be saved. */
SEXP
Raspell_saveWordLists(SEXP speller)
{
  AspellSpeller *spell = (AspellSpeller *) R_getSpellReference(speller, "AspellSpeller");
  return(ScalarInteger(aspell_speller_save_all_word_lists(spell)));
}



/* XXXX  this is having problems but doesn't seem to be us alone. 
    The example-c in aspell itself doesn't work and fails in the same way.
      example-c  en
     followed by p or P or m.
*/
SEXP
Raspell_getWordList(SEXP speller, SEXP which)
{
  AspellSpeller *spell = (AspellSpeller *) R_getSpellReference(speller, "AspellSpeller");
  const AspellWordList *list = NULL;
  AspellStringEnumeration *els;
  

  switch(INTEGER(which)[0]) {
      case 1:
	  list =  aspell_speller_personal_word_list(spell);
	  break;
      case 2:
	  list =  aspell_speller_main_word_list(spell);
	  break;
      case 3:
	  list =  aspell_speller_session_word_list(spell);
	  break;
      default:
	  PROBLEM "Invalid type"
	      ERROR;
  }

  if(!list) {
      return(R_NilValue);
  }

  els = aspell_word_list_elements(list);

  if(els) {
      return(Raspell_convertStringEnumeration(els));
  }

  return(R_NilValue);
}


/*********************************************************************/

SEXP
Raspell_newDocumentChecker(SEXP speller)
{
    AspellSpeller *spell = (AspellSpeller *) R_getSpellReference(speller, "AspellSpeller");
    AspellCanHaveError *tmp;
    AspellDocumentChecker *doc;

    tmp = new_aspell_document_checker(spell);
    doc = to_aspell_document_checker(tmp);

    return(R_makeSpellReference(doc, "DocumentChecker", Raspell_freeDocumentChecker));
}

SEXP
Raspell_documentCheckerReset(SEXP sdoc)
{
    AspellDocumentChecker *doc = (AspellDocumentChecker *) R_getSpellReference(sdoc, "DocumentChecker");
    aspell_document_checker_reset(doc);

    return(R_NilValue);
}


SEXP
Raspell_documentCheckerProcess(SEXP sdoc, SEXP text)
{
    AspellDocumentChecker *doc = (AspellDocumentChecker *) R_getSpellReference(sdoc, "DocumentChecker");
    char *txt;

    txt = CHAR(STRING_ELT(text, 0));
    aspell_document_checker_process(doc, txt, strlen(txt));

    return(R_NilValue);
}


SEXP
Raspell_documentCheckerNext(SEXP sdoc)
{
    AspellDocumentChecker *doc = (AspellDocumentChecker *) R_getSpellReference(sdoc, "DocumentChecker");
    struct AspellToken tok;
    SEXP ans;

    tok = aspell_document_checker_next_misspelling(doc);
    ans = NEW_INTEGER(2);
    INTEGER(ans)[0] = tok.offset;
    INTEGER(ans)[1] = tok.len;
    return(ans);
}



/*********************************************************************/


/* Package a C object into the external pointer type we need for R. */
SEXP
R_makeSpellReference(void *ptr, const char *tag, R_CFinalizer_t fin)
{
    SEXP ref = R_MakeExternalPtr(ptr, Rf_install(tag), R_NilValue);
    PROTECT(ref);
    if(fin)
	R_RegisterCFinalizer(ref, fin);

    UNPROTECT(1);
  return(ref);
}


/* Dereference an external pointer type, checking the tag to verify it is what we expect. */
void *
R_getSpellReference(SEXP el, const char *tag)
{
  void *ptr;
  if(TYPEOF(el) != EXTPTRSXP)
     PROBLEM "Reference must be an external pointer"
       ERROR;

  if(R_ExternalPtrTag(el) != Rf_install(tag)) {
        PROBLEM "Invalid aspell pointer type, expecting %s", tag
	ERROR;
  }

  ptr = R_ExternalPtrAddr(el);

  return(ptr);
}



/* Finalizer */
static void
Raspell_freeSpeller(SEXP el)
{
    AspellSpeller *sp = R_getSpellReference(el, "AspellSpeller");
    if(sp) {
#ifdef R_ASPELL_DEBUG
	fprintf(stderr, "Freeing speller reference %p\n", sp);
#endif
	delete_aspell_speller(sp);
    }
}

static void
Raspell_freeDocumentChecker(SEXP el)
{
    AspellDocumentChecker *sp = (AspellDocumentChecker *) R_getSpellReference(el, "DocumentChecker");
    if(sp) {
#ifdef R_ASPELL_DEBUG
	fprintf(stderr, "Freeing document checker %p\n", sp);
#endif
	delete_aspell_document_checker(sp);
    }
}


static void
Raspell_freeConfig(SEXP el)
{
    AspellConfig *sp = (AspellConfig *) R_getSpellReference(el, "AspellConfig");
    if(sp) {
#ifdef R_ASPELL_DEBUG
	fprintf(stderr, "Freeing config reference %p\n", sp);
#endif
	delete_aspell_config(sp);
    }
}




/* Just a test in the context of R. */
void
checkMain()
{
  AspellConfig *conf;
  AspellKeyInfoEnumeration *els;
  const AspellKeyInfo *info;

  conf = new_aspell_config();
  els = aspell_config_possible_elements(conf, 1);
  while( (info = aspell_key_info_enumeration_next(els) ) ) {
      fprintf(stderr, "%s\n", info->name);
  }


}


