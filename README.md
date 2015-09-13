# Myo - Haskell bindings to the Myo armband by Thalmic Labs

This library ships Haskell bindings to the WebSockets API of the
Myo Armband, by Thalmic Labs. To use it, refer to `example/WS.hs`

# Native API

**Note: This part of library is a working in progress. Expect alpha quality
software. Please contribute to speed up its development.**

If installed with the flag `foreign-api` set to True, this library also
exposes a 1:1 mapping between the C library (_libmyo_) and the Haskell world.
Please bear in mind that such API comes with severe limitations:

* At the moment of writing, Mac OS X is the only platform supported
* A very small API subset has been implemented (PR welcome!)
* It's author's playground to explore the use of `inline-c` and automatic
memory deallocation with `ForeignPtr`s and such, therefore some functions
sigsegv or they do not deallocate correctly. Please send a patch, I will
owe you a 🍺  !

# Contributions

PR and external contributions are welcome! Please open an issue about the
part of the API you would like to implement and we'll work together.
