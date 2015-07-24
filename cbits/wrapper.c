
#include "libmyo.h"
#include <stdio.h>

void myo_string_free(libmyo_string_t s) {
  libmyo_string_free(s);
}

void myo_error_details_free(libmyo_error_details_t t) {
 if (t != NULL) libmyo_free_error_details(t);
}

void myo_hub_free(libmyo_hub_t h) {
  if (h != NULL) libmyo_shutdown_hub(h, NULL);
}
