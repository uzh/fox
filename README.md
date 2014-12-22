<img src="./foxLogo.jpg" alt="Fox Logo" width ="80" height="70"> FoxPSL: Fast Optimized eXtended PSL [![Build Status](https://travis-ci.org/uzh/fox.svg?branch=master)](https://travis-ci.org/uzh/fox/branches) [![Codacy Badge](https://www.codacy.com/project/badge/31c2972e3ae440e4ba80d7cd55759bc0)](https://www.codacy.com/public/uzh/fox)
=====================================================

How to Compile the Project
--------------------------
Ensure Java 7 (or higher) is available on the system, verify with `java -version` on the command line.

Install SBT: http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html

Check out the project:
```
git clone git://github.com/uzh/fox.git fox
cd fox
````

Run this from inside the fox *project folder* to ensure the Signal/Collect dependency is present and in the right place:
```
git clone git://github.com/uzh/signal-collect.git ../signal-collect
````

To generate the JAR file, run:
```
sbt assembly
```

To generate an Eclipse project, run:
```
sbt eclipse
```

How to try out an example
-------------------------

Run the `assembly` command according to the instructions above, then execute:
```
./fox.sh examples/movies.psl
```
to run the inferencer on the `movies.psl` file.

If there is no shell available, you can also run `java -Xmx5000m -cp ./target/scala-2.11/fox-assembly-1.0-SNAPSHOT.jar com.signalcollect.psl.CommandLinePslInferencer examples/movies.psl`.


How to use the Domain Specific Language (DSL)
---------------------------------------------
The input to the system is a description of individuals, predicates, facts and rules in a domain-specific language for PSL. In the following we describe features of the DSL; you find an example file that combines them all in the examples folder (https://github.com/uzh/fox/blob/master/examples/feature-complete.psl).

It is possible to explicitly list individuals in a domain, and optionally assign them to a class. By convention, individuals are always lower-case names, in order to distinguish them from upper-case variables.

```
class Person: anna, bob
class Party:  demo, repub
class Woman:  anna
individuals:  ufo
```

In this example, our domain consists of two individuals of class `Person = { anna, bob }`, two of class `Party = { demo, repub }`, one of class `Woman = { anna }` and one individual without any class `ufo`.
Classes are not mutually exclusive and the same individual can have multiple classes (`anna`).
Besides explicitly mentioned individuals, Fox can automatically infer other individuals and their class from facts and rules.

For each predicate, you can specify the classes of its arguments:

```
predicate: retired(_)
predicate: professor(Person)
predicate: teaches(Person, Course, Person)
```

In the above example, the predicate `retired` takes one argument of any class, while `professor` takes one argument of class `Person`. This means that in the grounding phase the only individuals that will be used to ground `professor` will be those of class Person, greatly reducing the number of grounded predicates produced.
The same holds for `teaches`, which takes a first argument of class `Person`, a second of class `Course`, and a third of class `Person`.

You can define predicate properties such as functionality, partial functionality, inverse functionality and symmetry:

```
predicate [Functional]: votes(Person, Party)
predicate [Symmetric]:  friends(Person, Person)
```

In the example above, the functional property on `votes` means that the votes for different parties that a certain person can cast must sum up to 1. The symmetry property of `married` means that for all individuals `a`, `b`, if `married(a,b) = x` then `married(b,a) = x`. 

After the predicates definition, you can state some facts about your domain and their truth values. If the truth value is not set, it is consider to be 1.

```
fact:                    professor(bob)
fact [truthValue = 0.8]: friends(bob, carl)
fact [0.9]:              !votes(anna, demo)
```

In our domain, `bob` is a professor with truth value 1 and `bob` is a friend of `carl` with truth value 0.8. Although `carl` was not mentioned as a member of `Person` before, it will be inferred because he is the second argument in a `friends` fact.  Moreover, `anna` does not (negation `!`) vote for `demo` with truth value 0.9 (you can omit `truthValue`), which means `votes(anna,demo)=0.1`.

The most important part of modeling a domain are the rules, which are Horn rules with disjunctive heads, i.e. rules of the form 
B<sub>1</sub>  &&  ...  &&  B<sub>n</sub> => H<sub>1</sub> || ... || H<sub>m</sub>
where H<sub>i</sub> for `i=1, ..., m` are literals, B<sub>j</sub> for `j=1, ..., n` are atoms and the symbols &&, || and => represent conjunction, disjunction and implication.

In the following example, the upper-case names represent the variables in the rules:
```
rule [weight=5]:                    votes(A,P) && friends(A,B)  => votes(B,P)
rule [3, distanceMeasure = linear]: young(P) => !retired(P)
rule:                               professor(P) => EXISTS [C,S] teaches(P,C,S) || retired(P)
```

Each rule can have an associated weight that represents how strict it is and an associated distance measure (e.g. linear, squared) that describes the shape of the penalty function for breaking this rule.

There is also the existential quantifier `EXISTS [variables]`, which can only appear in the head. Moreover, if the weight is not specified, the rule is considered to be a hard rule.
