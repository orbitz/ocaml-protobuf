ocaml-protobuf is a Protobuf implementation for Ocaml.  It provides a fairly
straight forward API.  Consuming a Protobuf message is through a Monad
interface, and building Protobuf messages is through a Buffer-like interface.

Examples can be seen in protobuf_test.ml.

#### Known Issues

* Not heavily tested

* No protoc support yet (don't hold your breath)

### Release Notes:

#### 0.0.0

* Mostly tested functionality in protobuf_test.ml

* Supports Monad model for consuming messages

* Supports Buffer-like model for producing messages
