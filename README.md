# Verteilte Systeme - Winter Semester 2012 - Aufgabe 1

## Server starten

```
$ erl -name term1 -setcockie example
3> server:start(). % Registriert 'wk' local
```


## Client starten

```
$ erl -name term2 -setcockie example
1> nodes(). % ['term1@local']
2> client:start('term1@local'). % client:start() % is local
```
