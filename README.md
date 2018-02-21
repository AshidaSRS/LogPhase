# com.shin.LogPhase
Contact:  
* Telegram: @Ashida  
* Email: ashida.shin  __[at]__ gmail __[dot]__ com   
(subject: [LogPhase])

# HOW
Add ```@spy``` to a function and will get debug logs

from:
```scala
  @spy 
  def sum(a: Int, b: Int) = a + b
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

# TODO

- Improve all, it's a mess
- Add options in ``@spy`` annotation
- Add options like: ``println``, ``logger``, etc.