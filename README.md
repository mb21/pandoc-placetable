# Pandoc-placetable Filter

A Pandoc filter that replaces code blocks (that have the class `table`) with tables generated from CSV.
The CSV is read from the code block and from an optional external CSV file and concatenated. There's a
flag to enable parsing of inline markdown.

Some usage examples:

    ```{.table file="foo.csv"}
    ```

    ```{.table}
    some,values
    even,"with spaces"
    "with the inlineMarkdown flag enabled","we _can_ write **Markdown** here"
    ```

    ```{.table file="foo.csv" header=yes}
    Fruit,Quantity,Price
    ```

    ```{.table file="foo.csv" header=yes aligns=LRCRR caption="my caption" delimiter="," quotechar="\"" }
    ```
All attributes are optional and are specified as follows:

- file: The path to the CSV file that is appended to the code block's content
- header: If set to `yes`, then the first row of data is interpreted as the table headers.
- aligns: For each column, one letter (L, R or C) that specifies the column's alignment.
- caption: The table caption.
- delimiter: A one-character string used in the CSV to separate fields, defaults to `,`.
  For characters not allowed standing alone in Pandoc markdown attributes, use `\t` for a
  tab and `\s` for a space.
- quotechar: A one-character string that may be used in the CSV to quote fields containing
  special characters, defaults to `"`.

When compiled with the `inlineMarkdown` flag, the CSV and the caption may contain markdown that will
be interpreted. The CSV may even contain multiline markdown in a cell (the field need to be quoted,
of course). Note that the flag causes Pandoc to be required as a dependency so it will take a while
to build.

## Installation

    cabal update
    cabal install pandoc-placetable

or with the `inlineMarkdown` flag (see above):

    cabal install -f inlineMarkdown pandoc-placetable

## Usage

Prepare a markdown file containing a [fenced code block](http://pandoc.org/README.html#fenced-code-blocks)
like the ones above, then:

    pandoc --filter pandoc-placetable input.md
