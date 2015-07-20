
#include "libmyo.h"

void myo_string_free(libmyo_string_t s) {
  libmyo_string_free(s);
}

void myo_error_details_free(libmyo_error_details_t t) {
 libmyo_free_error_details(t);
}

void myo_hub_free(libmyo_hub_t h) {
  libmyo_shutdown_hub(h, 0);
}
