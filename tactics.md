# Files defining tactis

* src\lean\init\tactics.lean
* src\lean\init\meta.lean

# List of tactics I've found so far.

* split (if & match)
* cases (inductive)
* injection 
* assumption
* generalize v
* apply f
* rfl
* have name : h := ...
* suffices
* rw
* erw
* simp
* dsimp
* simp_arith
* first
* constructor
* focus [tactics]
* next
* unfold !!!!

## Termination
```
termination_by [func_name] [params...] => [decreasing_term]
```

```
decreasing_by
  simp_wf
  [tactics]
```
