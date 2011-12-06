#include <aspell.h>
#include <stdio.h>

int
main(int argc, char *argv[])
{
  AspellConfig *conf;
  const char *tmp;
  char *option = "extra-dicts";


  AspellStringList * lst = new_aspell_string_list();
  AspellMutableContainer * lst0  = aspell_string_list_to_mutable_container(lst);
  AspellStringEnumeration *els;

  if(argc > 1)
      option  = argv[1];

  conf = new_aspell_config();
  tmp = aspell_config_retrieve(conf, option);
  fprintf(stderr, "%s = %s\n", option, tmp);

  aspell_config_retrieve_list(conf, option, lst0);
  els = aspell_string_list_elements(lst);
  while( (tmp = aspell_string_enumeration_next(els) ) ) {
      fprintf(stderr, "%s\n", tmp);
  }

  return(0);
}


