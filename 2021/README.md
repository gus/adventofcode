# 2021

Replace `N` with the day (`1..25`) you are interested in:

```
make dN build
cat examples/dN.input | targets/dN
```

Some days have alternate solutions:

```
make dNalt build
cat examples/dN.input | targets/dNalt
```

Running benchmarks (where appropriate):

```
make dN[alt] bench
```
