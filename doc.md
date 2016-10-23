# Install

```bash
sudo apt-get install prolog
```

# start

```bash
./start

#ou

prolog -q -f connect4.pl
start.

# ou

prolog
consult(connect4).
```


# compile

```bash
swipl --goal=start --stand_alone=true -o start -c connect4.pl
```