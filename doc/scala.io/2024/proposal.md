Title:

Recognizing regular patterns in heterogeneous sequences

Elevator pitch: 

We present decision procedure which allows us to distinguish patterns
in untyped, insecure data.  Scala types are insufficient to represent
RTEs (regular type expressions) at compile time.  The abstraction
relies on run-time type reflection.  We address both practical and
theoretical concerns.

Description:

Run-time type-based reflection is a powerful tool which is used to
solve certain problems which are out of reach to purely statically typed
programming languages.  The JVM-based implementation Scala allows a
running program to maker certain type-based decisions which cannot be
made at compile time. Notably, pattern matching and dynamic method
dispatch allow the running Scala program to make run-time decisions
based on characteristics of input data, the type of which cannot be
known at compile time.

In this expose, we present yet another kind of run-time based type
decision which allows us to arbitrate among various regular patterns
in otherwise untyped data.  We call these patterns RTEs (regular type
expressions).

The Scala type system allows us to specify sequences such as
`Seq[Int]` or `Seq[String]`, i.e., homogeneous sequences of elements
of a specified type.  We would additionally like to specify sequences
of heterogeneous but regular types.  Regular types cannot be
statically supported by the Scala type system.  However, we have
implemented this abstraction the help of run-time reflection.

We assume the audience to already be familiar with string-based
regular expressions (REs). REs are used to distinguish strings which
follow a regular pattern such as $a(a|b)^*b$, the set of strings
beginning with the character `a`, ending with `b`, and with zero or
more `a` or `b` (or both) characters falling in between.  We
generalize this familiar concept to define expressions which specify a
sequence beginning with an integer, ending with a string, and with
zero or more integers or strings (or both) falling in between.

```
  val I:Rte = Singleton(classOf[Int])
  val S:Rte = Singleton(classOf[String])

  val r:Rte = I ++ (I | S).* ++ S // sugar for Cat(I, Star(Or(I, S)), S)
```

The implementation of Regular Type Expressions (RTEs) in Scala
involves several libraries.

1) `genus`: Embed a Simple type system (SETS) into Scala's run-time.
`genus` adds intersection, union, and complement types to Scala
(2.13), as well an singleton and predicate types.

2) `rte`: Define an ADT-like class hierarchy atop SETS to define a set
of operators for regular-expression in terms of types.  We say
*ADT-like*, because of limitations in the Scala compiler, we cannot
implement a actual sealed ADT.

3) `xymbolyco`: Construct deterministic finite automata, `Dfa`, from
the regular type expressions.  This library manipulates DFAs with
operations such as `minimize`, `intersection`, `union`, `xor`,
and `extract-rte`.

The run-time flow consists of:

4) Instantiating an instance of the ADT `Rte`,
5) Building a `Dfa` from a given `Rte`
6) Simulating the `Dfa` given a sequence, `Seq[Any]`.

There are several questions addressing practical concerns such as
performance and memory consumption.  Additionally, there are many
theoretical questions which investigate the limitations of the
generalization from classical character based regular expressions to
regular type expressions.  Some of these concerns include habitation
and vacuity checks (given an `Rte`, can we determine whether all or no
sequence will match).  Given two types (in the SETS sense) determine
whether one is a subtype of the other, and whether either is empty.
Subtype determination is important for guaranteeing that finite
automata be deterministic.  Unfortunately, the subtype relation cannot
always be determined (for several interesting theoretical reasons).  We
present a clever procedure for DFA construction which is guaranteed to be
deterministic, even when the subtype relation cannot be determined.

We expect some audience members will find this abstraction
thought-provoking.  However, we fully suspect others will find the
concept objectionable as in many cases we are fighting to undo some of
the guarantees which the Scala compiler endeavors to provide.

The project is available here: https://github.com/jimka2001/scala-rte
<!--  LocalWords:  Scala JVM DFAs
 -->
