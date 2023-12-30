# gus-aoc-haskell

```sh
# example inputs
cat ./inputs/<YYYY>/d<D>.example.<M>.txt | stack run <YYYY>d<D>

# problem input
cat ./inputs/<YYYY>/d<D>.txt | stack run <YYYY>d<D>
```

Where:

- `<YYYY>` represents the 4-digit year
- `<D>` represents the day's problem
- `<M>` represents some integer `[1..]`, assuming a file exists with that `M`

To run with profiling (might cause a recompile of everything):

```sh
cat ./inputs/<YYYY>/d<D>[.example.<M>].txt | stack run --profile -- <YYYY>d<D> +RTS -s -p

# N ... seconds ... later

cat ./<YYYY>d<D>.prof
```
