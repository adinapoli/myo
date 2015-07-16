
#include "libmyo.h"

#include "wrapper.h"

const char * inline_c_Myo_0_3a9c88b16684b490ec0f2dc0ca3475f15368669f(libmyo_string_t sPtr_inline_c_0) {
return ( libmyo_string_c_str(sPtr_inline_c_0) );
}


libmyo_string_t inline_c_Myo_1_4b63f941b6242d3f6d1097e0525796564b6e56d4(uint64_t i_inline_c_0) {
return ( libmyo_mac_address_to_string(i_inline_c_0) );
}


uint64_t inline_c_Myo_2_509e55339657b08ea171a116a2858442cc7a7b25(const char * sPtr_inline_c_0) {
return ( libmyo_string_to_mac_address(sPtr_inline_c_0) );
}


void inline_c_Myo_3_af19345a370682aeab6dd9755d4bb8c75cfc9b0f(libmyo_string_t ms_inline_c_0) {
 libmyo_string_free(ms_inline_c_0);
}


void inline_c_Myo_4_b0ad3c36952e696871add43043768fdb17c05c20(libmyo_error_details_t ed_inline_c_0) {
 libmyo_free_error_details(ed_inline_c_0) ;
}


const char * inline_c_Myo_5_32efc38eb0c8f0fc8503c30e16a62d2c7ddebc83(libmyo_error_details_t ed_inline_c_0) {
return ( libmyo_error_cstring(ed_inline_c_0) );
}

