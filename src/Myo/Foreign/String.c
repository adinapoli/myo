
#include "libmyo.h"

#include "wrapper.h"

const char * inline_c_Myo_Foreign_String_0_817cf39fc030b767c64bf15739f8ce47d8341d52(libmyo_string_t ms_inline_c_0) {
return ( libmyo_string_c_str(ms_inline_c_0) );
}


libmyo_string_t inline_c_Myo_Foreign_String_1_322eef913ad66043ba6f6f6786116e88e2976984(uint64_t i_inline_c_0) {
return (
    libmyo_mac_address_to_string(i_inline_c_0)
    );
}


uint64_t inline_c_Myo_Foreign_String_2_509e55339657b08ea171a116a2858442cc7a7b25(const char * sPtr_inline_c_0) {
return ( libmyo_string_to_mac_address(sPtr_inline_c_0) );
}

