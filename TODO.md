## TODO

- Better handling of '}', avoid new lines


### Scoped blocks

```
{
  scopedStmtList
}
```

should be parsed with the `do` notation like:

```
do
  scopedStmtList

```

### Intersperse comments

```
if /* make it better */ true { }
```

Will generate:

```
if
  # make it better
  true
```

which currently isn't valid igo.
