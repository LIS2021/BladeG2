# BladeG2
## Compile
Install dependencies and just run
```bash
make
```

## Execute
There are two files.

The virtual machine (interpreter) is the executable `vm`. It takes the
input code from stdin. For instance
```bash
./vm < tests/prog1.txt
```
runs `tests/prog1.txt`.

Blade is implemented by the executable `blade`. It takes the input code
from stdin an prints to stdout the repaired version. For instance
```bash
./blade < tests/prog1.txt > tests/prog1-rep.txt
```
repairs `tests/prog1.txt` and writes the repaired version in
`tests/prog1-rep.txt`.

# Credits
The graph and max_flow algorithms are based on https://github.com/dsainati1/maxflow