# Syntax

The syntax of Amulet is primarily a mixture of OCaml and Haskell. It uses indentation to determine blocks of code.

## Naming conventions + allowed identifier symbols

* Identifiers can be composed of any alphanumeric character and a series of symbols: `+`, `-`, `<`, `>`, `.`, `/`, `|`, `\`, `=`, `_`, `#`, `’`, `~`, `@`, `:`.

> Allow arbitrary Unicode identifiers blacklisting some characters: such as ``"[](){},;`"'"``. We could define ⊕, ∨, ∧, ¬, μ, η
> However it would requiring [the Unicode data](http://www.unicode.org/Public/UNIDATA/UnicodeData.txt) and excluding everything in [Other and Separator](http://www.fileformat.info/info/unicode/category/index.htm)

* You can also use double backticks to have a multi-character variable name: ``` ``foo`` ```.
* Modules, types and type constructors should be written using PascalCase.
* Variables, function names, type parameters and record field names should be lowercase.

## Comments
Comments come in two forms: line comments (comments which last from the delimiter until a line break) and block comments (comments which last until the terminating block).

Line comments are started with `;`.
Block comments are started with `(*` and finished with `*)`. These can be nested to allow easier commenting out of code.

> **To consider** How would we handle doc comments? Something like `(** .. *)` + markdown support?

> Also: How do we handle compiler pragmas? Do we even need them?

## Type annotations

* The basics of types (`Foo`, `Foo a` `Foo a => [a]`, `a -> b`).
* :: used to mark a type
* _ can be used to mark a type wildcard (not a type variable, just something you don’t want to explicitly write). So you could have `[_]` to constrain it to be a list of something.

## Type declarations
Types can only be declared in a module. There are two forms of type declarations:

* Aliases: allowing referencing a longer type name as a shorter one (or renaming it if it conflicts or any other reason)
* Type definitions: used for creating sum and product types.

### Type names
Type names are defined as an identifier followed by a series of free type variables and/or concrete types.
If type parameters are specified within the type name then the parameters that are specified within the RHS of the definition _must_ exist on the LHS.

> **o:** This is probably more fit for the type document, but are types allowed to be parametrized over constraints?
>
> Consider
> ```haskell
> data Box (c :: * -> Constraint) = forall a. c a => Box a
> ```
>
> This could be used for boxing up all types of a certain type class together, say, in a list.
> **p:** I'm not sure yet. I can see the benefits but it depends on whether you'd have to type out the type constraints when you use it too: the only use I see for this to save rewriting constraints?
> **q:** `[Box "Hello", Box 1, Box ()] :: Box Show`
> **r:** Right. Can we move the whole existential types discussion to the types docs and I'll have a look over how they work: I'm really confused how a variable vanishes into nowhere, though I can also see their use.
>
>
> I always presumed `type A a = [a]` was the same as `type A = ∀ a . [a]`
> **s:** > I always presumed `type A a = [a]` was the same as `type A = ∀ a . [a]`
>
>
> Haskell is _hard_, man.
> **t:** What do you think about this? Some of me likes this as it prevents typos, however it could be a pain too.
> **u:** What about existentially-qualified types?
> **v:** Can you clarify: Do you mean that every free type variable in the RHS must be bound by the LHS?
> **w:** Yeah. Iff there are variables specified on the LHS then all RHS must be bound, otherwise you can use whatever and they'll be implicitly added in order of appearance.
> **x:** What about just requiring explicit foralls in type definitions? (pretty sure Haskell does this)
> **y:** I don't feel comfortable with that. (and no, Haskell doesn't; just for existentially-qualified types)
> **z:** Consider that
>
>
> ```haskell
> data Box a = Box a
> ```
> has a different kind (* -> *) from
> ```haskell
> data Box = forall a. Show a => Box a
> ```
> which is just *.
> **aa:** Do you have an example of Haskell doing that?
> I just tried `data Test = Test a` and `type Test = Rational a` both of which didn't work
> **ab:** In that case you have to either quantify existentially (with -XExistentialQuantification) or add a type parameter. They're not the same.
> **ac:** Right. Urrm. Maybe we should explicitly require parameters. Seems odd as it isn't required in type annotations though.
>
>
> RE Existential Quantification: I wasn't really aware of this before - I'm slightly confused about how this even translates to a type system: you've got a free variable which vanishes into nowhere.
> **ad:** I was suggesting copying Haskell (not requiring explicit foralls for type parameters)
> **ae:** So they have to be bound on the LHS?
> **af:** Do we even need existentials? I'm sure they could be _useful_, but I don't actually use them at all: And, everywhere that they're used could be replaced by a (granted, less-type-safe) ⊤.

### Type aliases
The `type` keyword is used to define a type alias:

```
type <name> = <type>
```

### Type definitions
All record and product types are defined using the `data` keyword followed by a type name:

```bnf
definition = “data” name “=” (record | sum_type)
```

#### Records
Record definitions are composed of a series of identifier keys to type pairs separated by commas within braces.

```bnf
record_pair = identifier “::” type
record      = “{“ {record_pair “,”} “}”
```

#### Sum types
Sum types are composed of one or more product-type-constructors, each followed by the types the product type is composed of.

```bnf
product_type = identifier {type “ “}
sum_type = {“|” product_type}
```

##### GADT syntax
> A possible GADT syntax for the future:
> ```haskell
> data Foo tvs = Foo1 bs :: Foo tvs
>              | Foo2 cs :: Foo tvs
> ```

### Type classes
Type classes are composed of a type name with one or more type variables (they cannot have a kind of `*`). They can optionally contain a series of constraints before the primary type definition.

#### Type classes
Type classes are marked with the `class` keyword. You can then specify a series of constraints on various parameters before specifying the actual type. All constraints’ parameters must appear in the type class’s definition.

> Maybe use `trait` or `interface`

The beginning of the type class’s body is marked with the `where` keyword and an indent. The body contains a series of variables with type annotations with optional definitions. It is possible for all definitions to be filled and depend on one another: when creating an implementation Amulet will determine if you have provided sufficient information for a complete definition.

#### Type class implementations
Implementations share a similar syntax to type class definitions, using `impl` instead of `class`. However, instead of following the type class with type parameters you can use any type expression.

The body of the implementation uses an identical syntax to that of type classes.

##### Named implementations
* PascalCase
* Require a name but allow specifying a default? Or not?
* If they are named, can we use them as a constraint?
* How do you use specify them? Use them? What is the type of a function using a particular implementation - could we change constraints (see above point)?
   * In Idris `show @{Foo}` has type `Bar -> String` rather than `Foo a => a -> String`. I don’t know which one makes more sense.
   * ~~However given `[List] Show a => Show (List a) where` trying to do `show @{List}` gives you “Can’t find implementation for Show a`.~~ This works with Idris: it just ignores whatever type parameter you actually use.

I’m thinking something like:
```haskell
impl Foo = Show Bar where
    show (Bar x) = “Bar: “ ++ show x
```

For using them I’d propose `show#Foo bar`.

## Expressions

### Literals
#### Numeric literals
Numeric literals can be written as base 2, 8, 10 or 16 numbers.

Binary numbers are prefixed by 0b, hexadecimal numbers are prefixed with 0x, and octal numbers are prefixed with `0o`. None of the previous can contain an exponent or decimal place.

Base 10 numbers can contain a decimal point and an exponent.

A number can have infinitely repeated decimals after the decimal point (possibly directly) prefixed by a “~”.

All numbers can be prefixed with a positive or negative sign, as well as allowing _ between any two digits. This allows breaking a number up into nibbles or thousands.

Numbers with a decimal point are inferred to be of a type with an instance of `Coerce Rational a`, while numbers without a decimal point (though optionally with an exponent) are inferred to be types with an instance of `Coerce Integer a`.

#### Character literals
Characters are composed of a single character code between single quotes (`’`). A character code can be composed of:

* A single character (such as `a`)
* An escape character (`\’`, `\n`, `\r`, \t`, `\\`, `\”`)
* A decimal character code: \123`
* A hex character code: `\xAB`
* The character can be prefixed with `u` to convert it to a unicode character. This also allows writing UTF8 literals:

* A UTF8 literal `\uABCD` This can be a hex code of either 4 or 8 characters.

> Require `;` or `u` to terminate the literal if more than 4 characters:`\uABCDE;`?

#### String literals
Strings are written as a series of character codes between double quotes (`”`). Prefixing with a `u` will convert it to a unicode string, also allowing UTF8 literals.

#### Unit literal
An empty pair of parenthesis () are considered a unit value.

### Primary expressions

#### Function application
Any two expressions following another are considered a function application. If functions are applied to a lesser number of arguments than they expect, this is partial application (which also means that functions are automatically curried).

Function application associates to the left. That is, `a b c d` is interpreted as `(((a b) c) d)`.

#### Infix operators
Infix (binary) operators are implemented as functions which take two arguments. By convention a binary operator should exclusively be composed of symbols.

You can convert arbitrary functions of type `a -> b -> c` to infix operators by surrounding their name with backticks (`` ` ``).

##### Operator sections

Operator sections are partially-applied binary operators, written in infix form for clarity. An operator can either be applied to their left or right operand, which gives rise to the two categories of operator sections: either _left_ or _right.

A right operator section is an operator applied to their left operand, as in `(e +)`, which is fully equivalent to `(λx. e + x)`. A left operator section is applied to their right operand, as in `(+ e)`, which is fully equivalent to `(λx. x + e)`.

##### Infix-style function definitions
Function definitions can also also be written in infix form with the arguments specified before and after the operator.

##### Defining operators
Operators must be defined using `let` statements with the additional `op` statement. An associativity (`left` or `right`) can be specified as well as a precedence. Precedence must be an integer between 0 and 256.

```haskell
let op left x |> f = f x
```

#### Lambda expressions

Lambda expressions form the base of any functional language worth its salt. In Amulet, there are two ways to express a lambda abstraction: Either `\x₁ … xₙ -> e` or `λx₁ … xₙ -> e`.

Lambda expressions have the highest precedence of any expression.

#### List Literals

Linked lists are represented in Amulet through repeated usage of the `(:)` value constructor, which can get tiring. To aid in the creation of lists, a comma-separated list of expressions delimited by square brackets (`[]`) is interpreted as a list.

Also related to list literals are vector literals. Vectors are, in Amulet, semantically equivalent to a Lua table or a C array; They contain a sequence of values in contiguous memory without pointers to the following/previous element. These, however, are delimited by square brackets with pound signs on the _inner_ side: While `[1, 2, 3]` is a list, `[# 1, 2, 3 #]` is a vector.

#### Tuple literals
Tuples are composed of two or more values within a pair of parenthesis (0 values are considered a unit, 1 value is just for grouping expressions). Each expression within a tuple is separated with a comma.

#### Record literals
Record types are composed of a series of comma separated, key-value pairs between matching braces:

```haskell
{ ident1 = expr1, ident2 = expr2, }
```

You can create a new record using existing fields of another record by prefixing the pairs with `expr |`.

### Control flow

#### If expressions
If expressions allow different expressions to be evaluated depending on the value of a condition. The condition _must_ be of type `Bool`.

If statements are started with `if`, followed by a condition, the `then` keyword then the expression to be evaluated on a `True` value. This should be followed by the `else`

This expression can either appear on the same line or on a new line followed by an indent. Each expression’s indentation are independent, so one can use a complex multiline expression and the other a single variable. This allows if expressions to be chained.

All branches’ types must be unifiable.

```haskell
-- Simple single line
print $ if a then b else c

-- Multiple line if statements
print $ if a then b
        else
           let c = doSomethingFancy a
              d = moreFancyThings
          in c d d

-- Chaining
print $ if a then b
        else if c then d
        else e
```

#### Cond expression
Amulet provides a way to simplify the writing of long `if`-`else if` chains. These expressions are started with the word `cond` (coming from the Lisp macro [of the same name](http://www.lispworks.com/documentation/HyperSpec/Body/m_cond.htm#cond). This keyword is then followed by multiple lines of the form `| condition -> expression`.

All branches’ types must be unifiable.

If Amulet cannot prove that the `cond` expression is total then a warning will be emitted and an fallback branch will be emitted which produces an error.

#### Match expressions
Match expressions provide a way to branch based off of a series of patterns. They follow a similar syntax to `cond` expressions. They start with `match <expr> with` and are followed by multiple lines of the form `| pattern -> expression`. An optional guard can be placed after the condition (which must evaluate to a `Bool`).

All branches’ types must be unifiable.

If Amulet cannot prove that the `match` expression is total then a warning will be emitted and fallback branch will be emitted which produces an error.

### Let bindings
Let bindings are used to create a scope with one new variable. These expressions are composed of the binding and the expression to evaluate with this new variable. This expression can either appear after an explicit `in` or on a new line with the same indentation level as the let expression.

Let bindings come in three key forms (destructing assignments, function creation and recursive function(s) creation).

> **To consider:** How can we handle binding multiple variables? Some possibilities:
>
> * Explicit `in`
> * `and` between variables
> * Indentation magic
>
> Also adding this quote:
> > Non-recursive requires you to think about dependencies more which makes you try to decouple things which I think is good. However it sometimes makes things harder than they need to be.

#### Destructuring bind
A destructuring bind is simply composed of a pattern followed by an equals sign. This is simply sugar for a pattern matching expression:

```haskell
let (x, y) = f
g $ x y
```
is equivalent to
```haskell
match f with
| (x, y) -> g $ x y
```

As with `match` expressions a warning will be produced if the expression isn’t total.

#### Function creation
Let expressions can be used to create functions. These can either be written in normal or infix form.

If in normal form then the function name is given followed by a series of arguments. This is followed by an equals sign and the function body. If the function is an operator then it must be surrounded by parentheses.

Infix form is similar, but the function name is given between the two arguments. If the function name is not an infix operator then it must be surrounded in backticks to convert it into one.

#### Recursive binds
The syntax for function creation can be expanded to allow recursive functions using the `rec` keyword. The body of the function will be defined in the scope including the variable.

Mutually recursive functions can be defined by separating functions with the `and` keyword.

> See above comment on multiple variables

### `do` notation

## Patterns

Patterns are used in the left-hand-side of where and let bindings, in the clauses of matches and in the declarations of functions.

There are N <!-- todo: replace this by the actual number --> different patterns that can occur in an Amulet program. Though this is a fixed set, they can be combined to match against more complicated data.

### Wildcards

Denoted by `_`, this pattern matches anything and does no binding. As an example, `let _ = x in e` discards the value x (and is optimized to `e`). Formally, the set of free variables of an expression in the scope created by a `_` binder is the same as the set of free variables of the expression.

### Literal Patterns

These match against a given literal, that is, a number, string, or character, and bind nothing. Matching on a string is equivalent to matching on a vector of characters.

### List and Vector Patterns

List pattern matching syntax exists as syntactic sugar for repeated matching against the `(:)` constructor. They match against a list of exact size. `[x,y,z]` is equivalent to `x:y:z:[]`, where `x`, `y` and `z` are patterns. For matching against a list of at least N elements, the syntax `[x,y,z] :- xs` is allowed, where `x`, `y`, `z`, and `xs` are patterns. This is equivalent to `x:y:z:xs`.

The same sugar exists for vectors, but this is builtin rather than sugar. `[# x, y, z #]` matches against a 3-element vector, while `[# x, y, z #] :- xs` matches against a vector of _at least_ 3 elements. The typical way of matching against the head/tail of a vector is `[#x#] :- xs`.

### Capturing patterns

These can be any valid identifier, matching anything and binding them to the given name. These are semantically equivalent to `name@_`.

### At-patterns

At-patterns are a way to give an otherwise nameless pattern a name. An at-pattern will bind its name to the match of the pattern it was created from in the resulting scope. These take the form of `name@pattern`.

### Constructor Patterns

> One of the nice things about F# is the ability to [define custom patterns](http://stackoverflow.com/a/2429874). This can be saved that for later, probably as an extension. Also look at pattern guards and view patterns (they seem equivalent to [Active Patterns](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#pattern-guards)).

Constructor patterns match against a value constructor, and thus are used for destructuring abstract data, such as lists. There are 2 forms of constructor patterns.

#### Product-type Patterns

> Allow infix patterns (such as ``a `Foo` b``)

These match against a given constructor of a sum type (implying that the constructor must unify with the given type signature, and that fields must unify with the constructor’s signature), without any named fields.

The basic form is `Name`. This matches against a nullary constructor. For constructors with more fields, the form is `(Name p₁ … pₙ)`. This binds everything that the set of `p₁ … pₙ` binds. For example, `(Ratio _ _)` doesn’t bind anything.

#### Record Patterns

There are 3 forms of record patterns: Empty record patterns, Explicitly matched record patterns and ‘punned’ record patterns. These all share the form `Name{...}`.

* Empty record patterns, in contrast to nullary-constructor patterns, match against an arbitrary number of fields, binding none of them. This means that spiritually equivalent to `Name{}` is spiritually equivalent to `(Name _ _ _)`, with enough `_`s to match all of the fields.
* Explicitly matched record patterns match against record fields, and explicitly give them a name. For example, given a record type `type T α β = { x :: α, y :: β }`, `T{ foo <- x, bar <- y }` binds `foo` and `bar` to `x` and `y`, respectively. The left-hand-side of such a binder can be an arbitrary pattern.
* Punned record patterns extend explicitly matched record patterns by implicitly adding a left-hand-side binder that matches the right hand side. For example, `T{x,y}` is equivalent to `T{x <- x, y <- y}`. Furthermore, _record wildcards_ allow the author not to explicitly concern themselves with the field names, by extending punned records to allow for a wildcard `..` to appear inside the braces: These are equivalent to writing out puns of all of the fields.

## Importing and modules

* Open and module =
* Exporting
* Access modifiers
   * Public: accessible anywhere
   * Internal: accessible in just this compilation unit (so the current project)
   * Private: just in this module scope.

> Thoughts on importing/exporting semantics? I like the access modifiers as
>
> 1.  they give greater control
> 2. you specify what to export at the definition point rather than at the top.
>
> Not sure how re-exporting would work though.
> Also worth looking at [Idris' way of doing things](http://docs.idris-lang.org/en/latest/tutorial/modules.html)