# Kronk - A Really Dumb First Language

I don't know why I named it `Kronk`, but I did.

`Kronk` is a crazy ruled dynamically typed interpretted language with currently minimal feature support. It's pretty much just my experimental repo for playing around with parsing and executing language stuff

`Kronk` currently supports:
- Literals including Number, String, Bool and Nil
- Basic arithmetic expressions with implicit type conversion (yippee)
- Variable assignment
- Branching logic with `if` statements
- Looping techniques through `while` and `for` style loops

`hello.kronk`:
```
var foo = "Hello";
var bar = " World!";

print foo + bar;
```

Execute using `kronk hello.kronk`:

```bash
$ kronk samples/hello.kronk 
Hello World!
```

Or even cooler, here's some nicer looking features that Kronk supports:

```
var flag = false;
var i = 1;

while (!flag) {
    if (i == 50) {
        print "done!";
        var flag = true;
    } else if (i == 25) {
        print "halfway there!";
    } else {
        print i + "/50";
    }

    var i = i + 1;
}
```
