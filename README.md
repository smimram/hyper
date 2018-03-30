# Hyper

[Hypergraph](https://en.wikipedia.org/wiki/Hypergraph) [rewriting](https://en.wikipedia.org/wiki/Rewriting) in order to manipulate presentations of [PROPs](https://en.wikipedia.org/wiki/PRO_(category_theory)).

## Example session

Let's define the theory for [monoids](https://en.wikipedia.org/wiki/Monoid). We first need to add in generators for multiplication and unit
```
op m : 2 -> 1
op u : 0 -> 1
```
then we throw in the rules for associativity and unitality on left and right
```
rule ass : (m*1);m => (1*m);m
rule u-l : (u*1);m => 1
rule u-r : (1*u);m => 1
```
Note that `;` is sequential composition and `*` is tensor product.

We can then display the graph corresponding to a morphism
```
show (m*2);(m*1);m
```
or display it graphically
```
plot (m*2);(m*1);m
```
we can normalize it
```
normalize (m*2);(m*1);m
```
or even show the normalization
```
plotnormalize (m*2);(m*1);m
```

## Documentation

There is no proper documentation at the moment, the list of available commands can be found in [lang.ml](https://github.com/smimram/hyper/blob/master/src/lang.ml).
