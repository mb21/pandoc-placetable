# pandoc-placetable filter

A Pandoc filter that replaces [fenced code blocks](http://pandoc.org/README.html#fenced-code-blocks)
(that have the class `table`) with tables generated from CSV. The CSV is read from the code block
and from an optional external CSV file and concatenated. There's an option to enable parsing of
inline markdown.

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
All attributes are optional and are specified as follows:

- **file**: The path or URL to the CSV file that is appended to the code block's content
- **header**: If set to `yes`, then the first row of data is interpreted as the table headers.
- **aligns**: For each column, one letter (L, R or C) that specifies the column's alignment.
- **inlinemarkdown**: If set to yes, interprets the caption and CSV fields as markdown.
  This requires pandoc-placetable to be compiled with the `inlineMarkdown` flag (see below).
- **caption**: The table caption.
- **delimiter**: A one-character string used in the CSV to separate fields, defaults to `,`.
  For characters not allowed standing alone in Pandoc markdown attributes, use `\t` for a
  tab and `\s` for a space.
- **quotechar**: A one-character string that may be used in the CSV to quote fields containing
  special characters, defaults to `"`.

## Installation

    cabal update
    cabal install pandoc-placetable

When compiled with the `inlineMarkdown` flag, the `inlinemarkdown=yes`option is available to
have CSV and the caption be interpreted as markdown. Note that the flag causes Pandoc to be
required as a dependency so it will take a while to build.

    cabal install -f inlineMarkdown pandoc-placetable

## Usage

Prepare a markdown file containing a fenced code block like the ones above, then:

    pandoc --filter pandoc-placetable input.md

## Acknowledgments

The idea and syntax for this filter was proposed by @jgm on
[pandoc-discuss](https://groups.google.com/forum/#!topic/pandoc-discuss/kBdJU_JktzI)
and first implemented by @baig's [pandoc-csv2table](https://github.com/baig/pandoc-csv2table),
over which I consider this filter an [improvement](https://github.com/mb21/pandoc-placetable/issues/1).
