
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


libmyo_result_t * inline_c_Myo_4_de73268ef2c3e74609f8183e83c04411e39c4165(libmyo_error_details_t * ed_27_inline_c_0) {

    libmyo_result_t* res;
    *res = libmyo_error_kind(&ed_27_inline_c_0);
    return res;
    
}


void inline_c_Myo_5_002b75103729a3a34143de3db6b4a63e48289d4c(libmyo_error_details_t * ed_27_inline_c_0) {
 libmyo_free_error_details(&ed_27_inline_c_0) ;
}


const char * inline_c_Myo_6_265969732feb68613d4b5f04437a6c1df0f9f582(libmyo_error_details_t * ed_27_inline_c_0) {
return ( libmyo_error_cstring(&ed_27_inline_c_0) );
}

