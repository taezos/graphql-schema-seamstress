# Graphql Stitch-Vomit

This CLI tool will stitch your schemas **if and only if** they are organized this way

<small>
*Plural for schema is schemata or schemas. [mirriam webster](https://www.merriam-webster.com/dictionary/schema)*
</small>

``` sh
+--my-schema-dir
|  |--person.gql
|  |--dog.gql
   ( etc.. )
```

## Commands
``` sh
Graphql Stitch-Vomit: A graphql schema stitcher.

Usage: graphql-stitch-vomit --src-dir ARG --output ARG
  Graphql Stitch-Vomit is a cli tool that will stitch all your schemas and vomit
  them into one file.

Available options:
  -h,--help                Show this help text
  --src-dir ARG            Schema source directory path.
  --output ARG             Output path for the schema stitched result.
```

