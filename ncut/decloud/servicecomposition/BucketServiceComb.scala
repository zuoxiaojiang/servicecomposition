package ncut.decloud.servicecomposition

import java.util.Date

import scala.collection.mutable

object BucketServiceComb extends App {

  //proposed user query, existed source services and service instances
  val query = DataUtil.query
    val services = DataUtil.services//an use case of services and service instances
//  val services = DataUtil.simuServices//simulated services and service instances

  //generate the map of join attributes in query and their corresponding relation set, for example : "ID" -> {Movie, Revenue}
  val joinAttrInQ = CombUtil.genQueryJoinAttrs(query)

  //build a services and service instances graph, each service or instance denote a graph node, and each edge between two
  //nodes denote the join attribute of them
  val g1 = CombUtil.initServiceGraph(services.toSet,joinAttrInQ)

  //a set of combination generated from buckets' elements by Cartesian product
  var candidateComb = Set[Set[QueryService]]()
  //call the function of queryRewriting
  queryRewriting(query,services)


  /**
    * given an user query and based on the existed services and instances to find all executable contained service rewritings
    * and executable equal service rewritings, and print the time cost information
    * @param query user query formed as conjunctive query, an instance of Class QueryService
    * @param views a list of services and service instances, each of them is an instance of Class QueryService
    */
  def queryRewriting(query:QueryService,views:List[QueryService]): Unit ={
    val timestamps:mutable.ListBuffer[Date] = mutable.ListBuffer()  //store the timestamp of each phase

    //create bucket and allocate elements for each sub-goal of query
    println("Creating buckets ......")
    timestamps.append(new Date())
    val buckets = createBuckets(query,views)
    timestamps.append(new Date())

    //Save all possible combination results in the variable candiateCombin collection
    println("Generating all possible combinations by Cartesian product ......")
    timestamps.append(new Date())
    val arrBuckets = buckets.toArray
    genCandiateComb(arrBuckets,arrBuckets(0).map(x=>Set(x)),arrBuckets(1),1)
    timestamps.append(new Date())

    //check each combination of candiateCombin, generate the candidate service compositions for witch is satisfied
    //the join condition , time-window constraints and data constraints
    println("Generating candidate service compositions......")
    timestamps.append(new Date())
    var candiateQuery = Set[QueryService]()
    candidateComb.foreach{x =>
      candiateQuery += CombUtil.genCandiateQuery(x,query,g1,0)
    }
    timestamps.append(new Date())

    //check whether the candidate service composition is executable, and instance the services in the composition
    println("Executable checking......")
    timestamps.append(new Date())
    val execplans = CombUtil.findExecPlans(candiateQuery,query)
    timestamps.append(new Date())

    //result output
    CombUtil.printResult(query,candidateComb,execplans,timestamps.toList)

  }

  /**
    * create bucket and allocate elements for each sub-goal of query
    * @param query user query formed as conjunctive query, an instance of Class QueryService
    * @param views a list of services and service instances, each of them is an instance of Class QueryService
    * @return the set of sub-goals of user query, whose bucket attributes have been allocated
    */
  def createBuckets(query:QueryService,views:List[QueryService]): Set[Set[QueryService]] ={
    var buckets = mutable.HashSet[Set[QueryService]]()
    var times = 0
    query.subQueries.foreach { g => //g denote each sub-goal of query
      views.foreach { v => //v denote each service or service instance of views
        //check whether time-window is contained, whether exists the g in v and whether g's variables are head variables in query,then are also head variables in v
        if(v.winContains(query) && v.subQueries.contains(g) && (g.oColumns & query.oColumns).subsetOf(v.oColumns & g.oColumns)){
          //check data constraints and add g to bucket if data constraints are satisfied
          var falseNum = 0
          query.constraints.foreach { cst1 =>
            v.constraints.foreach { cst2 =>
              if (cst1._1.equals(cst2._1) && !CombUtil.cstCompare(cst1, cst2)._3 )
                falseNum += 1
            }
          }
          if (falseNum == 0){
            g.addToBuckets(v)
            times += 1
          }
        }
      }
      buckets += g.bucket
    }
    buckets.toSet
  }

  /**
    * Save all possible combination results in the variable candiateCombin collection, eg.the Cartesian product of all elements in each bucket.
    * @param arrBuckets the array of buckets from query
    * @param set1 the start element of bucket array
    * @param set2 the second element of bucket array
    * @param i the index of current union position
    */
  def genCandiateComb(arrBuckets: Array[Set[QueryService]],set1:Set[Set[QueryService]],set2:Set[QueryService],i:Int): Unit ={
    if(i < arrBuckets.length){
      var union = Set[Set[QueryService]]()
      set1.foreach{v1=>
        set2.foreach{v2=>
          union += v1.union(Set(v2))
        }
      }
      candidateComb = union
      if(i+1 < arrBuckets.length)
        genCandiateComb(arrBuckets,union,arrBuckets(i+1),i+1)
    }
  }
}
