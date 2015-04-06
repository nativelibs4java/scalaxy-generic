[![Maven Central](https://img.shields.io/maven-central/v/com.nativelibs4java/scalaxy-generic_2.11.svg)]() [![Build Status](https://travis-ci.org/nativelibs4java/scalaxy-generic.svg?branch=master)](https://travis-ci.org/nativelibs4java/scalaxy-generic) [![Join the chat at https://gitter.im/nativelibs4java/Scalaxy](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/nativelibs4java/Scalaxy?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) 

Provides a `Generics[A]` typeclass that behaves a bit like `Numeric[A]`, except it allows for any method to be called (using `scala.Dynamic`).

Also provides AST simplifications that let Scalaxy/Reified erase `Numeric[A]` and `Generic[A]` away when `A` is known (which is the case when passing `TypeTag[A]` around. 
