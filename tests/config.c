#include <aspell.h>
#include <stdio.h>

int
main(int argc, char *argv[])
{
  AspellConfig *conf;
  AspellKeyInfoEnumeration *els;
  const AspellKeyInfo *info;

  conf = new_aspell_config();
  els = aspell_config_possible_elements(conf, 1);
  while( (info = aspell_key_info_enumeration_next(els) ) ) {
      fprintf(stderr, "%s\n", info->name);
  }

  return(0);
}


