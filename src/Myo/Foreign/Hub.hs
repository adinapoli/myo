{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Myo.Foreign.Hub where

import qualified Language.C.Inline as C

import Myo.Foreign.Types
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Alloc

C.context myoCtx
C.include "libmyo.h"
C.include "<string.h>"

{-
Initialize a connection to the hub.
\a application_identifier must follow a reverse domain name format (ex. com.domainname.appname). Application
identifiers can be formed from the set of alphanumeric ASCII characters (a-z, A-Z, 0-9). The hyphen (-) and
underscore (_) characters are permitted if they are not adjacent to a period (.) character (i.e. not at the start or
end of each segment), but are not permitted in the top-level domain. Application identifiers must have three or more
segments. For example, if a company's domain is example.com and the application is named hello-world, one could use
"com.example.hello-world" as a valid application identifier. \a application_identifier can be NULL or empty.
@returns libmyo_success if the connection is successfully established, otherwise:
 - libmyo_error_runtime if a connection could not be established
 - libmyo_error_invalid_argument if \a out_hub is NULL
 - libmyo_error_invalid_argument if \a application_identifier is longer than 255 characters
 - libmyo_error_invalid_argument if \a application_identifier is not in the proper reverse domain name format
-}

initHub :: MyoHub -> ApplicationID -> ErrorDetails -> IO Result
initHub h aid e = withForeignPtr h $ \h' ->
  withForeignPtr e $ \e' -> do
    alloca $ \resPtr -> do
     [C.block| void {
             libmyo_result_t r = libmyo_init_hub( $(libmyo_hub_t h')
                                   , $(const char* aid)
                                   , $(libmyo_error_details_t e'));
             memmove($(libmyo_result_t* resPtr) ,&r, sizeof(libmyo_result_t));
            }
     |]
     peek resPtr

-- /// Free the resources allocated to a hub.
-- /// @returns libmyo_success if shutdown is successful, otherwise:
-- ///  - libmyo_error_invalid_argument if \a hub is NULL
-- ///  - libmyo_error if \a hub is not a valid hub
-- LIBMYO_EXPORT
-- libmyo_result_t libmyo_shutdown_hub(libmyo_hub_t hub, libmyo_error_details_t* out_error);
