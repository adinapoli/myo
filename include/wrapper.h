
#ifndef wrapper_h
#define wrapper_h

void myo_string_free(libmyo_string_t* s) {
  libmyo_string_free(*s);
}

#endif
