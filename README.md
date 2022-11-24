# KongScript

## A JavaScript preprocessor with fancier syntax

- P.S: KongScript is not afraid to use modern JS features in its compiler for the sake of simplicity. You may want to
  run your output through Babel or a similar preprocessor!

### You Should Know

- Everything is in the ***CONCEPT*** stage. Some features may be unimplemented!
- The file extension is `.kong`
- The compiler does **NOT** preserve comments.
  - In the examples, the comments are added-in
- **Some examples are not KongScript output (because they haven't been implemented), or are modified examples.**
- The code output is automatically minified; in examples where it *is* KongScript out, the code has been prettified for
  your enjoyment.

### TODOs

- `and` operator
- Imports & Exports
- Macros (Still in concept stage)
- Throwing
- Try/Catch
- Format strings
- Other JS features that I forgot about

### Syntactic sugar by example

#### Practical equivalents

```js
// Hey, did you read the You Should Know section
// of this page? You really should!

fun add(x y) {
    return x + y
}

add(x y)

// Equivalent.
let add = (x y) -> x + y 
```

```js
function add(x, y) {
    return x + y;
}

add(x, y);

// Equivalent
let add = (x, y) => x + y;
```

##### Cool pattern matching B)

```js
let user_in = prompt("Give me a number!");

let value = parseInt(user_in);

if value matches 0..11 {
    console.log("I am > -1 and < 11!");
} elif value matches 11..21 {
    console.log("I am > 10 and < 21!");
}

// or...

match value {
    0..11 {
        // Blah
    }
    11..21 {
        // Blah
    }
    default {
        // Blah
    }
}
```

```js
let user_in = prompt("Give me a number!");

let value = parseInt(user_in);

if(value > -1 && value < 11) {
    // blah
} else if (value > 10 && value < 21) {
    // blah
}

// or...

switch(true) {
    case (value > -1 && value < 11):/*Blah*/break;
    case (value > 10 && value < 21):/*Blah*/break;
    default:/*Blah*/break;
}
```

##### Objects

```js
// `obj` keyword is necessary for compiler to be able to differentiate scopes
// from objects.
let my_object = obj {
  key = 120
}
```

```js
let my_object = {
    key: 120
}
```

##### Array ranges

```js
// Array of 1 to 10.
let array = 1..11;
```

```js
// KongScript compile-time output
let array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
```

##### Array range indexing

```js
let array = 1..21;

// 10 through 20. Index with :
let array_slice = array[9..21];

console.log(array_slice)
```

```js
// NOTE: Prettified for your enjoyment.
let array = [1, 2, 3, 4, 5, 
    6, 7, 8, 9, 10, 
    11, 12, 13, 14, 
    15, 16, 17, 18, 
    19, 20];

// 10 through 20.
let array_slice = array.slice(9, 21);

console.log(array_slice);
```

##### Regex pattern matching

```js
let regex = r"[A-Za-z0-9\-_]"gim

let input = prompt("Input an alphanumeric value");

if not input matches regex {
  alert("The input doesn't match the pattern!!! WHAT HAVE YOU DONE!?!?!?");
  throw "Oopsie :(";
}

alert("Thank you :)");
```

```js
let regex = /[A-Za-z0-9\-_]/gim

let input = prompt("Input an alphanumeric value");

if(!regex.test(input)) {
    alert("The input doesn't match the pattern!!! WHAT HAVE YOU DONE!?!?!?");
    throw "Oopsie :(";
}

alert("Thank you :)");
```

##### Classes

```js
class MyClass {
  // If there is no body, then the constructor
  // will expand to `this.param_name = param_name`
  constructor(name)
  
  fun greet() {
    // @name is an alias for this.name.
    console.log(f"Hello {@name}!");
  }
  
  get nickname {
    return @name + "y"
  }
  
  set task(task) {
    task.run()
  }
  
  // Valid JS syntax
  get [3] {
    // 3 to the power of 3, (27).
    return 3 ** 3
  }
}
```

```js
class MyClass {
    constructor(name) {
        this.name = name;
    }

    greet() {
        console.log(`Hello ${this.name}`);
    }

    get nickname() {
        return this.name + "y"
    }

    get [3]() {
        return 3 ** 3
    }
}
```

##### Basic class pattern matching

```js
class MyClass {
  constructor(value);
}
class OtherClass {
  constructor(other_value);
}

// Technically an object...
let cool_class = new OtherClass(30);

// So far, this is as far as one can go
// with pattern matching in classes... this is a fancy(?)
// instanceof.
if cool_class matches MyClass {
  console.log(cool_class.value)
} elif cool_class matches OtherClass {
  console.log(cool_class.other_value)
}
```

```js
class MyClass {
    constructor(value) {
        this.value = value;
    }
}

class OtherClass {
    constructor(other_value) {
        this.other_value = other_value;
    }
}

let cool_class = new OtherClass(30);

if(cool_class instanceof MyClass) {
    console.log(cool_class.value)
} else if(cool_class instanceof OtherClass) {
    console.log(cool_class.other_value)
}
```

##### Type annotations (not enforced (yet?))

```ts
// Types are Python-style in the sense that they are not enforced. :(
// You can also do `let num: asdjhfiou9dsh = 30;` -- there
// are no types built into the compiler.
let num: int = 30;

// An example of this not being
// enforced. NEVER, EVER do this!! PleasE! :(
let str: str = 30;

let thing: MyClass = new MyClass("Pentaflip Jones");

// An example of this feature being
// properly used. This is the _only_
// way in which you should use the feature.
str = "";
```

```js
let num = 30;

let str = 30;
```

###### List of official types

- Arrays: `[<your type/s here>]`
- Strings: `str`
- Booleans: `bool`
- Numbers `num`
- Integer: `int`
- Float: `float`
- Big Int: `big`
- Symbol: `symbol`
- Null: `null`
- Undefined: `undef`
- Nullable: `<your type here>?`
- Object: `obj`
- Any class name

##### Primitive compile-time math evaluation

Variables are not supported (yet?).

```js
let my_num = 64 % 2;

console.log(my_num);
```

```js
let my_num = 0;

console.log(my_num);
```

##### You can't have spaces in-between dots.

This is valid JS:

```js
console.log("Hello world");
```

This is not valid KS:

```js
console . log("Hello world");
```
