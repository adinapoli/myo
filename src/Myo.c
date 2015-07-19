
#include "libmyo.h"

#include <string.h>

#include "wrapper.h"

void inline_c_Myo_0_8a356f0fa910bb8b8831c109c51000723e3c4d4b(libmyo_error_details_t ed_27_inline_c_0, libmyo_result_t * resPtr_inline_c_1) {

      libmyo_result_t r = libmyo_error_kind(ed_27_inline_c_0);
      memmove(resPtr_inline_c_1
             , &r
             , sizeof(libmyo_result_t)
             );
      
}


const char * inline_c_Myo_1_79bcd9dad570978d588a7fdfd305a8c8c5dc82db(libmyo_error_details_t ed_27_inline_c_0) {
return ( libmyo_error_cstring(ed_27_inline_c_0) );
}

