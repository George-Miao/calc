# Calc - a simple calculator language

## Design

Simple and easy-to-use

Arithmetic operations:

```
// Add
> 10.1 + 1000
< 1010.1
// Times and referencing previous results
> @1 * 8
< 8080.8
// Minus and referencing multiple previous results
> @1 - @2
< 7070.7
// Using functions
> sin(@1) // or sin @1
< 0.85474701071
```
