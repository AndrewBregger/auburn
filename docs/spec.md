# Language Specification
A typed general purpose scripting language using a register-base virtual machine. The
language will have the ability to generate WASM to be ran on a browser. Since
the language will run on a browser it will be possible to be run as a client
side browser language. While this isn't the intention of building this
language, it is going to be a general purpose language that can be used in the
place of languages like Python.

## Keywords

* struct
* pub
* use
* enum
* trait
* extend
* fn
* let
* mut
* type
* Type
* if
* else
* elif
* while
* loop
* for
* stack_alloc

## Operators

There are standard arithmetic operators both in binary and unary arity.

## Syntax

### Variable

```
// initializing constant variable
let x = 1.0

// initializing mutable variable
mut x = 1.0

// initializing constant variable with type
let x: f32 = 1.0

// uninitialized constant variable with type
let x: f32

// uninitialized constant variable without type, error
let x
```

### Function

```
// function id with one parameter x with a returning expression body
fn id(x f32) f32 = ~~~~x

// function add with multiple parameters x and y of the same type
// with a returning expression
fn add(x, y f32) f32 = x + y

// function foo with no parameter and a returning expression body
fn foo() {
    ..
}
```
When the function body is marked with an '=' then the return type is inferred by the
following expression.

### Structure
```
struct Foo {
    x f32
    pub y i32    
}
```

### Method
```
struct Foo {
    x f32
    pub y i32    

    // struct construction/ associated function
    fn new(x f32, y i32) Self { Self { x, y } }
 
    // constant associated method
    fn foo(val String) {
        println("{} {}", val, self.x)
    }

    // mutable associated method
    fn set_x(x f32) {
        self.x = x
    }
}
```

There are two ways to initialize a structure, these methods are lied out below:
```
// construction of Foo. By default, a structure is is allocated on the heap
let foo = Foo.new(1.0, 2) // type is Foo

// construction of Foo on stack. This is a special allocation of an object.
let foo = stack_alloc Foo.new(1.0, 2) // type is StackAllocated[Foo]
```
