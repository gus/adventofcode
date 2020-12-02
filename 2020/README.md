# 2020

Replace `N` with the day (`1..25`) you are interested in:

```
make dayN build
cat examples/dayN.input | targets/dayN
```

Some days have alternate solutions:

```
make dayNalt build
cat examples/dayN.input | targets/dayNalt
```


Running benchmarks:

```
make dayN[alt] bench
```
