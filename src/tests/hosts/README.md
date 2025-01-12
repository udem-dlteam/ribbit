# Host tests

## Rationale

Instead of testing **scheme** code, host tests make sure that the compiler generates
**a valid RVM** according to meta information inside of it. This includes feature, replace,
primitive and other types of meta information. This meta information is annotated with 
`@@(` and `)@@` tags.

## How to run
To run those tests, go into the `src` directory and run : 

```
HOST={your host} make check-feature
```

## Inner workings

These test works like this :
 - First, a script counts the number of `visible` and `hidden` tags inside the test.
 - Then, the test file (template rvm) is passed to the compiler with the `--rvm` test
 - The output is checked to make sure that the `visible` tags are still present and that the `hidden` tags got removed

