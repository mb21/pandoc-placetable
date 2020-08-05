# pandoc-placetable filter

**Note: currently only compatible with pandoc < 2.10**,
since that release changed the way tables are represented in the document AST substantially.

A Pandoc filter that replaces [fenced code blocks](http://pandoc.org/README.html#fenced-code-blocks)
(that have the class `table`) with tables generated from CSV. The CSV is read from the code block
and from an optional external CSV file (or URL) and concatenated. There's an option to enable
parsing of inline markdown.

Some usage examples:

    ```{.table file="foo.csv"}
    ```

    ```{.table}
    some,values
    even,"with spaces"
    ```

    ```{.table file="https://docs.google.com/spreadsheets/my-publish-to-web-sheet-key&output=csv"}
    ```

    ```{.table inlinemarkdown=yes}
    "when compiled with the inlineMarkdown flag","we _can_ write **Markdown** here"
    ```

    ```{.table file="foo.csv" header=yes}
    Fruit,Quantity,Price
    ```

    ```{.table file="foo.csv" header=yes aligns=LRCRR inlinemarkdown=yes
         caption="my **caption**" delimiter="," quotechar="\"" }
    ```


## Usage

Prepare a markdown file containing a fenced code block like the ones above, then:

    pandoc --filter pandoc-placetable input.md


Alternatively, the program can be used as an ad-hoc csv-reader and run without a markdown file.
Then the options can be provided as command-line arguments. For example:

    $ pandoc-placetable --file=foo.csv --widths="0.2 0.8" | pandoc -f json -o output.html

Or also:

    $ cat foo.csv | pandoc-placetable --widths="0.2 0.8" | pandoc -f json -o output.html

(For this use-case, you might not even need pandoc-placetable; pandoc 2.9.2 and later ships
with a simple built-in csv-reader: `pandoc foo.csv -o output.html`)

All options are optional and are specified as follows (cf. `pandoc-placetable -h`):

- **csv**: The path or URL to the CSV file that is appended to the code block's content
- **file**: synonym for `csv`
- **header**: If set to `yes`, then the first row of data is interpreted as the table headers.
- **aligns**: For each column, one letter (L, R or C) that specifies the column's alignment.
- **widths**: For each column, a number specifying its width as a fraction of the page width,
  e.g. `widths="0.5 0.2 0.3"`.
- **inlinemarkdown**: If set to yes, interprets the caption and CSV fields as markdown.
  This requires pandoc-placetable to be compiled with the `inlineMarkdown` flag (see below).
- **caption**: The table caption.
- **delimiter**: A one-character string used in the CSV to separate fields, defaults to `,`.
  For characters not allowed standing alone in Pandoc markdown attributes, use `\t` for a
  tab and `\s` for a space.
- **quotechar**: A one-character string that may be used in the CSV to quote fields containing
  special characters, defaults to `"`.

If there is an `id` set (e.g. `{.table #my-id}`), the table will be wrapped in a `div` with
that `id` so it can be referenced.

## Installation

### Using cabal and Hackage

    cabal update
    cabal install pandoc-placetable

### Using stack

    git clone git@github.com:mb21/pandoc-placetable.git
    cd pandoc-placetable
    stack install pandoc-placetable

### The inlineMarkdown flag

When compiled with the `inlineMarkdown` flag, the `inlinemarkdown=yes`option is available to
have CSV and the caption be interpreted as markdown. Note that the flag causes Pandoc to be
required as a dependency so it will take a while to build.

    cabal install -f inlineMarkdown pandoc-placetable

or:

    cd pandoc-placetable
    stack install --flag pandoc-placetable:inlineMarkdown pandoc-placetable


## Acknowledgments

The idea and syntax for this filter was proposed by @jgm on
[pandoc-discuss](https://groups.google.com/forum/#!topic/pandoc-discuss/kBdJU_JktzI)
and first implemented by @baig's [pandoc-csv2table](https://github.com/baig/pandoc-csv2table),
over which I consider this filter an [improvement](https://github.com/mb21/pandoc-placetable/issues/1).
