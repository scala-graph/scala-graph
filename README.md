# Graph for Scala

This is the source code repository and issue tracker for [Graph for Scala](http://www.scala-graph.org).

[Questions or any feedback](https://groups.google.com/forum/#!forum/scala-graph) are appreciated.
Please use GitHub issues for proven issues or enhancement requests but not for questions. 

You are also welcome as a co-contributor.

Have fun with Graph for Scala.

Peter

## Branches

**1.x** started in 2011. It has evolved by paying high attention to version compatibility.

**2.x** started in 2019 to make some significant improvements that also need new, simplified signatures.
New features include

* multiple sources for directed hyperedges
* easy edge class definition using case classes thus easy definition of ADTs of edges
* improved functional graph handling.

Shipment is due shortly after Scala 2.13.11 is published because
<a href="https://github.com/scala/bug/issues/12623">this fix</a> will help to further simplify user code.

## Roadmap

* Complete migration of random graphs for 2.x.
* Investigate whether and how the JSON module should be retained and migrated to 2.x or dropped. Migrate it if needed.
* Remove internal <code>State</code> by possibly also changing node references to IDs.
* Add support for algorithms by enrichment that use fluent properties.
* Reimplement immutable graphs to be based on a persistent data structure.
