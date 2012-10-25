# Verteilte Systeme - Winter Semester 2012 - Aufgabe 1

## Server starten

```
$ erl -name term1 -setcockie example
1> net_admin:ping('term2@local').
2> nodes(). % ['term2@local']
3> server:start(). % Registriert 'wk' global
```


## Client starten

```
$ erl -name term2 -setcockie example
1> nodes(). % ['term1@local']
2> client:start(wk).
```

# Local 'schnell test'
```
$ erl
1> client:start(server:start()).
```