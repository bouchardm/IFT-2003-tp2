# Install

```bash
sudo apt-get install prolog
```

# start

```bash
./start

#ou

prolog -q -f tp1.pl
start.

# ou

prolog
consult(tp1).
```


# compile

```bash
swipl --goal=start --stand_alone=true -o start -c tp1.pl
```