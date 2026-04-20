# treesitter

a parser generator & incremental parser library.

it build a concrete syntax tree. i.e. not abstract syntax tree (AST) as in, it does not omit tokens like parenthesis, semicolons, operators, comments, etc. (AST omits as the parser/lexers have embedded information to handle the syntax tree) as it is primarily used for the concrete syntactical workflows.

## grammar definitions

- it is written in javascript
- Each grammar is a JavaScript module that exports a call to `grammar()` with rules defined using DSL-like functions (like `seq`, `choice`, `repeat`, `token`).

```javascript
// following DSL describes the context-free grammar of the
// language. i.e. valid tokens

module.exports = grammar({
  name: 'c',

  rules: {
  translation_unit: $ => repeat($.external_declaration),

    external_declaration: $ =>
    choice($.function_definition, $.declaration),

  function_definition: $ =>
    seq($.declaration_specifiers,
      $.declarator,
      $.compound_statement),
  }
})
```

- above grammar is compiled by Tree-sitter into a binary parse table using a GLR-based algorithm

so basically, you write grammar definition in a DSL that compiles to a state machine (LR parser tables). The treesitter continues to incrementally parse. the consumer keeps listening for treesitter.

it should be noted that it is not a semantic analyzer or a Language Server.

so its helpful to think of tree sitter as a advanced regex rather than a parser from compilers/interpreters which not only manages to select tokens but also keep the syntax tree in context.

## query language

to query for token via tree-sitter you can use the tree sitter's s-expression query syntax. example:

```scheme
(function_definition
  name: (identifier) @func.name)
```

matches every `function_definition` node and captures its identifier child as `@func.name`.

the structual match might looks like this

```scheme
(if_statement
  condition: (binary_expression
    operator: "=="))
```

## usage

tree sitter itself is a C library (with bindings to Rust, Python, Node, etc.).

a typical workflow would look like this:

1. load grammar (compiled `.so`/`.dylib`/`.dll` generated from the JS grammar)

2. make a parser:

   ```python
   from tree_sitter import Language, Parser

   Language.build_library(
     'build/my-languages.so',
     ['tree-sitter-c', 'tree-sitter-python'])

   C_LANGUAGE = Language('build/my-languages.so', 'c')

   parser = Parser()
   parser.set_language(C_LANGUAGE)

   tree = parser.parse(bytes("int main() {}", "utf8"))
   ```

3. `tree` is a Tree sitter syntax tree object that can be traversed programmatically.

4. run queries:

   ```python
   from tree_sitter import Query

   query = Query(C_LANGUAGE, """
     (function_definition
       declarator: (function_declarator
         declarator: (identifier) @func.name))
   """)

   captures = query.captures(tree.root_node)
   for node, name in captures:
       print(name, node.text)
   ```
