#include <aspell.h>
#include <stdio.h>

int
main(int argc, char *argv[])
{
  AspellConfig *conf;
  const char *tmp;
  char *option = "extra-dicts";

  struct AspellModuleInfoList *dlist;
  struct AspellModuleInfoEnumeration *els;
  const AspellModuleInfo *el;
  

  conf = new_aspell_config();

  dlist = get_aspell_module_info_list(conf);
  els = aspell_module_info_list_elements(dlist);

  while( (el = aspell_module_info_enumeration_next(els) ) ) {
      AspellStringEnumeration *els;
      const char *w;

      fprintf(stderr, "%s  (dict_dirs %p)\n", el->name, (void *) el->dict_dirs);
      if(el->dict_dirs) {
	  els = aspell_string_list_elements(el->dict_dirs);
	  while( (w = aspell_string_enumeration_next(els))) {
	      fprintf(stderr, "\t%s", w);
	  }
	  fprintf(stderr, "\n");
      }
  }
  return(0);
}


