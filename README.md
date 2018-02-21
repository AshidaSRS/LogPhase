# com.shin.LogPhase
Contact:
Telegram: @Ashida
email: ashida.shin@gmail.com (subject: [LogPhase])

# HOW
Add ```@spy``` to a function and will get debug logs

from:
```scala
  @spy def sum(a: Int, b: Int) = a+b

  sum(1,2)
```
to:

```scala 
def sum(a: Int, b: Int) = {
  println(s"[ENTRY] sum - $a $b");
  def runMethod = a + (b);
  val _logRun = runMethod;
  println(s"[EXIT] sum - $_logRun);
  _logRun
}

```

#TODO

- Improve all, it's a mess
- Put options in ``@spy`` annotation
- Put options like: ``println``, ``logger``, etc.