### Building

```
$ gprbuild
```

### Testing

```
$ ./test-all.py
```

#### Why are you hand rolling a testing framework instead of using `gnattest`?

It's part of the Ada-on-Windows experience.

```
$ gnattest -PAdventOfCode2023.gpr
Segmentation fault
```
