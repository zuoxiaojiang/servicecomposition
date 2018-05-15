BucketServiceComb.scala:
The driver program of this project, line 10 setting the user query with a conjunctive query, line 11 setting the existed services/service instances set with a use case, line 12 setting
the existed services/service instances set with a simulated data source. If you wanna use one of the service sets you must commented out the other one.

DataUtil.scala
In this class you can define your query just like line 39, and set the variable query as your query, you can define the existed service set like line 23 to line 36, and you must set
the sub-goals in query just like line 4 to line 17. In line 47 you can determin which sub-goals contributes to simulation result, and line 48 determin the size of simulation result.
If you wanna run a use case with a light data source, you can set your test set in line 50.