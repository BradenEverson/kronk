# Kronk - A Really Dumb First Language

I don't know why I named it `Kronk`, but I did.

`Kronk` is a crazy ruled dynamically typed interpretted language with currently minimal feature support. It's pretty much just my experimental repo for playing around with parsing and executing language stuff

## Current Features
`Kronk` currently supports:
- Literals including Number, String, Bool and Nil
- Basic arithmetic expressions with implicit type conversion (yippee)
- Variable assignment
- Variable reassignment, add assignment and incrementing (both ++{var} and {var}++)
- Comment ignoring :O
- Branching logic with `if` statements
- Looping techniques through `while` and `for` style loops
- `print`ing and `roar`ing
    - Note: `roar` is a legendary and life changing keyword that will print your message to standard out more expressively than before. Instead of ending this expression in a semicolon, if must end with a bang `!`

`hello.kronk`:
```c
var foo = "Hello";
var bar = " World!";

roar foo + bar!
```

Execute using `kronk hello.kronk`:

```bash
$ kronk samples/hello.kronk 
HELLO WORLD!!!!
```

Or even cooler, here's some nicer looking features that Kronk supports:

```c
var flag = false;

for (var i = 1; !flag; var i = i + 1) {
    if (i == 50) {
        print "done!";
        var flag = true;
    } else if (i == 25) {
        print "halfway there!";
    } else {
        print i + "/50";
    }
}
```

## The best feature by far: Error handling.
Kronk has support for tokenization and parser errors, they give helpful insight on what may be wrong in a program with syntax you don't know and docs that don't exist:

### Token Error
```c
var foo = 1;


this is all valid but $ is not
```
Running the `kronk` interpretter over this will give us the error:
```c
token error: Unrecognized token: `$`
 -> samples/invalid_tokens.kronk:4:23 
 | this is all valid but $ is not
 | ----------------------^
```

### Parser Error
```c
while (var foo = false) {
    print "This file is super wrong"
};
```

Running `kronk` here will give us:
```c
parser error: Unexpected token: `var`
 -> samples/invalid_parser.kronk:1:10 
 | while (var foo = false) {
 |        ~~~
```

Fixing that error gives:

```c
parser error: Expected `;` after `This file is super wrong`
 -> samples/invalid_parser.kronk:2:35 
 |     print "This file is super wrong"
 |           ~~~~~~~~~~~~~~~~~~~~~~~~~
```

And fixing that one gives:

```c
parser error: Unexpected token: `;`
 -> samples/invalid_parser.kronk:3:2 
 | };
 |  ~
```


