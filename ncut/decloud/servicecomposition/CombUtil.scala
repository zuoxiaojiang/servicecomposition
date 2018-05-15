package ncut.decloud.servicecomposition

import java.io.{File, FileWriter}
import java.util.Date

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object CombUtil {

  /**
    * print the bucketservicecompositon result on console
    * @param query user query formed as conjunctive query, an instance of Class QueryService
    * @param candidateComb candiate service compositions
    * @param execplans  final executable equal plans and contained plans
    * @param timestampList  timestamp of each phase
    */
  def printResult(query:QueryService, candidateComb: Set[Set[QueryService]], execplans:(Set[QueryService],Set[QueryService]),timestampList:List[Date]): Unit ={
    val executableEqPlans = execplans._1
    val executableInPlans = execplans._2

    val creBucketStart = timestampList.head
    val bucketEnd = timestampList(1)
    val genCandiComStart = timestampList(2)
    val genCandiComEnd = timestampList(3)
    val genCandiQStrat = timestampList(4)
    val genCandiQEnd = timestampList(5)
    val findExecPlansStart = timestampList(6)
    val findExecPlansEnd = timestampList(7)

    var bucketsizes:Set[Int] = Set()
    query.subQueries.foreach(x => {println(x.name + "'s bucket size is :" + x.bucket.size); println(x.bucket);bucketsizes+=x.bucket.size})
    println()

    println("Eqlplans are : ")
    executableEqPlans.foreach(x=> printEqQueries(x))
    println()
    println("Incplans are : ")
    executableInPlans.foreach(x=> printInQueries(x))
    println()
    val totalPlans:Long = executableEqPlans.size + executableInPlans.size
    println("Candiate plansNum is : " + candidateComb.size)
    println("Execuatable equal plansNum is : " + executableEqPlans.size)
    println("Execuatable contained plansNum is : " + executableInPlans.size)
    println("Creating buckets costs : ")
    val t1 = printTimeCost(creBucketStart,bucketEnd)
    println("Finding all possible combinations costs : " )
    val t2 = printTimeCost(genCandiComStart,genCandiComEnd)
    println("Generating candidate queries costs : ")
    val t3 = printTimeCost(genCandiQStrat,genCandiQEnd)
    println("Finding executable plans costs : ")
    val t4 = printTimeCost(findExecPlansStart,findExecPlansEnd)
    println("Total plans is : " + (executableEqPlans.size+executableInPlans.size))
    println("Total time is : " + (t1+t2+t3+t4))

    if(totalPlans >0) {
      val tt = (t1 + t2 + t3 + t4).toFloat
      println("Per plan time is : ", tt / totalPlans)
    }

  }

  //      val record = bucketsizes.max + "," + candidateComb.size +","+executableEqPlans.size+","+executableInPlans.size+","+ totalPlans +","+ tt+","+(tt/totalPlans)
  //      writeToFile(filepath, record)

  /**
    * generate the map of join attributes in query and their corresponding relation set, for example : "ID" -> {Movie, Revenue}
    * @param Q user query formed as conjunctive query, an instance of Class QueryService
    * @return the map formed as key is type of string denoting the join attribute, value is type of list which loads sub-goals owning this key attribute in query
    */
  def genQueryJoinAttrs(Q: QueryService) :Map[String, ListBuffer[QueryService]] = {
    val queryMap:mutable.Map[String, ListBuffer[QueryService]] = mutable.Map()
    Q.subQueries.foreach(g => {
      g.oColumns.foreach(attr => {
        queryMap.get(attr) match {
          case Some(n) => n.append(g)
          case None => queryMap+= (attr -> ListBuffer(g))
        }
      })
    })
    queryMap.filter(e => e._2.size > 1).toMap
  }

  /**
    * build a services and service instances graph, each service or instance denotes a graph node, and each edge between two nodes denotes the join attribute of them
    * this method is to check whether the service composition could join each other to generate a candidate rewriting
    * @param services a set of services and service instances, each of them is an instance of Class QueryService
    * @param joinAttrInQ join attributes and its corresponding map to sub-goals in query
    * @return
    */
  def initServiceGraph(services:Set[QueryService], joinAttrInQ:Map[String, ListBuffer[QueryService]]): ServiceGraph ={
    val g1 = new ServiceGraph()
    services.foreach({v =>
      if(v.oColumns == null){
        var unionClmns = Set[String]()
        v.subQueries.foreach(x => unionClmns = x.oColumns.union(unionClmns))
        v.oColumns = unionClmns
      }
      g1.addNode(v,joinAttrInQ)
    })
    g1
  }

  /**
    * check whether the two given data constraints have intersect,
    * @param cst1 data constraints formed as a tuple (attribute, left of the interval, right of the interval)
    * @param cst2 data constraints formed as a tuple (attribute, left of the interval, right of the interval)
    * @return if have intersect then return the intersect and true, else return false
    */
  def cstCompare(cst1:(String,Int,Int), cst2:(String,Int,Int)):(Int,Int,Boolean) = {
    if (!(cst1._2>cst2._3||cst1._3<cst2._2 || cst2._2>cst1._3 ||cst2._3<cst1._2)){
      val left = math.max(cst1._2,cst2._2)
      val right = math.min(cst1._3,cst2._3)

      (left,right,true) //intersect
    }
    else
      (0,0,false) //disjoint
  }


  /**
    * check the executable of candidate rewriting service compositions
    * @param candiateQueries a set of candidate service composition
    * @param query user query formed as conjunctive query, an instance of Class QueryService
    * @return executable equal rewriting plan collection and executable contianed rewriting plans collection
    */
  def findExecPlans(candiateQueries : Set[QueryService], query:QueryService):(Set[QueryService],Set[QueryService]) = {
//    var plansNum = 0
    var executableEqPlans = Set[QueryService]() //a set of executable equal rewriting plan
    var executableInPlans = Set[QueryService]() //a set of executable contained rewriting plan
    val rangeMap = mutable.HashMap[Set[(String,Int,Int)],Boolean]()

    candiateQueries.foreach(q => {
      //q.oColumns is not empty indicates q is a candidate service composition generated from method genCandiateQuery
      if(q.oColumns.nonEmpty){
        //get additional constraints
        val q_query = q.contains(query) //check if q contains query, and if q intersects with query
        val query_q = query.contains(q) //check if query contains q, and if query intersects with q
        var q_query_interset = q_query._1.union(query_q._1).diff(q.constraints) //get intersect of q and query

        //seek for equal rewriting, if q contains query and there exists services in  q, then generate instances for services in q
        if(q_query._2.equals("contain")){
          //if the result set contains an equal rewriting that is more concise than the current q, the current candidate composition need not be reconsidered.
          if(!executableEqPlans.exists(x =>
            x.subQueries.subsetOf(q.subQueries)
          )){
            //if there exists a service in q, then add the additional constraints to service to generate the instance for service, if additional constraints don't satisfied service then discard it
            if(q.subQueries.exists(_.serviceType==0)){
              q.subQueries.foreach{ x=>
                if(x.serviceType == 0){
                  if(q_query_interset.nonEmpty){
                    q_query_interset = x.genInstanc(x.oColumns&query.oColumns,"",q_query_interset,query.windows)
                  }else{
                    x.genInstanc(x.oColumns&query.oColumns,"",Set(),query.windows)
                  }
                }
              }
              if(q_query_interset.isEmpty){
                //if there is a more redundant service composition than the current composition, replace it with the current composition, otherwise add it to the result set directly
                if(executableEqPlans.exists(x =>
                  q.subQueries.subsetOf(x.subQueries)
                )){
                  executableEqPlans = executableEqPlans.filter(x => !q.subQueries.subsetOf(x.subQueries))
                  executableEqPlans += q
                }else{
                  executableEqPlans += q
                }
              }
            } else {
                if(query.dataContains(q)._2.equals("contain")){
                  if(query.winContains(q)){
                    //if there is a more redundant service composition than the current composition, replace it with the current composition, otherwise add it to the result set directly
                    if(executableEqPlans.exists(x =>
                      q.subQueries.subsetOf(x.subQueries)
                    )){
                      executableEqPlans = executableEqPlans.filter(x => !q.subQueries.subsetOf(x.subQueries))
                      executableEqPlans += q
                    }else{
                      executableEqPlans += q
                    }
                  }else{
                    if(!rangeMap.getOrElse(q.constraints,false)){
                      executableInPlans += q
                      rangeMap.put(q.constraints,true)
                    }
                  }
                }
              }
          }
        }

        //seek for contained rewritings
        else if(q_query._2.equals("overlap")){
          //if the result set doesn't exist the composition which outputs the same content as the current q, add the q to result
          if(!rangeMap.getOrElse(q.constraints,false)){
            //if there is a service in q, it's necessary to instantiate the service, and if the constraints are found not all applied to the service in the instantiation process, the service composition is not executable.
            if(q.subQueries.exists(_.serviceType==0)){

              q.subQueries.foreach{ x=>
                if(x.serviceType == 0){
                  if(q_query_interset.nonEmpty){
                    val input = x.oColumns&query.oColumns
                    q_query_interset = x.genInstanc(input,"",q_query_interset,query.windows)
                  }else{
                    x.genInstanc(x.oColumns&query.oColumns,"",Set(),query.windows)
                  }
                }
              }
              if(q_query_interset.isEmpty){
                executableInPlans += q
                rangeMap.put(q.constraints,true)
              }

            }
            //if there is only service instances in q, then add q to the result directly
            else{
              if(query.dataContains(q)._2.equals("contain")){
                executableInPlans += q
                rangeMap.put(q.constraints,true)
              }
            }
          }
        }
      }
    })
    (executableEqPlans,executableInPlans)
  }

  /**
    * generate a candidate rewriting service composition based on the all possible service combination by Cartesian product,
    * mainly determines whether time-windows and constraints are mutually satisfied in the combination
    * @param vs a composition of services which is going to be determined whether is a candidate rewriting
    * @param query user query formed as conjunctive query, an instance of Class QueryService
    * @param g1 the service graph
    * @param algorithm  algorithm type, 0 denote the bucket algorithm
    * @return the checked composition q, if q.oColumns is empty then q is not a candidate composition, else is a candidate composition
    */
  def genCandiateQuery(vs : Set[QueryService],query : QueryService, g1: ServiceGraph,algorithm: Int): QueryService ={
    val arrVs = vs.toArray
    var q = new QueryService("",Set(),Set(),Set(),(0,0))
    //get the time-window intersect
    var j = 0
    var range = 2147483647
    var slide = 0
    while(j < arrVs.length) {
      if(arrVs(j).windows != null){
        if(arrVs(j).windows._1 < range){
          range = arrVs(j).windows._1
        }
        if(arrVs(j).windows._2 > slide){
          slide = arrVs(j).windows._2
        }
      }
      j+=1
    }
    var canJoin1 = true
    if(algorithm == 0){
      //check whether these services in vs can join
      arrVs.foreach(_.visited=false)
      val b = new ArrayBuffer[QueryService]()
      g1.dfs(arrVs(0),vs,b)
      canJoin1 = b.toSet.size == vs.size
    }
    if(canJoin1) {
      var i = 0
      //the union of data constraints
      var unionCsts = arrVs(0).constraints
      while(i < arrVs.length){
        unionCsts = unionCsts.union(arrVs(i).constraints)
        i+=1
      }
      //reduce data constraints, check whether there is a constraint conflict
      var canJoin = true
      val combinCsts = unionCsts.groupBy(_._1).map{x =>
        x._2.reduce{(_1,_2) =>
          val r = cstCompare(_1,_2)
          if(!r._3)
            canJoin = false
          (x._1,r._1,r._2)
        }
      }.toSet
      //if satisfied the constraints then return the q
      if(canJoin)
        q = new QueryService("",vs,query.oColumns,combinCsts,(range,slide))
    }
    q
  }


  /**
    * print the executable equal service composition on console
    * @param q an equal executable plan
    */
  def printEqQueries(q: QueryService): Unit ={
    if(q.constraints.isEmpty){
      println("Equivalent query consists of services :")
      println(q.serviceType + " -- " + q.name + " -- " + q.subQueries + " -- " + q.oColumns.toString() + " -- " +  q.constraints + " -- " +q.windows)
      q.subQueries.foreach(x => println(x.name))
    }else{
      println("Equivalent query consists of services :")
      println(q.serviceType + " -- " + q.name + " -- " + q.subQueries + " -- " + q.oColumns.toString() + " -- " +  q.constraints + " -- " +q.windows)
      q.subQueries.foreach(x => {
        if(x.ops != null){
          println(", and ops is : " + x.ops)
        }
        println()
      })
    }
  }

  /**
    * print the executable contained service composition on console
    * @param q a contained executable plan
    */
  def printInQueries(q: QueryService): Unit ={
    if(q.constraints.isEmpty){
      println("Include query consists of services :")
      println(q.serviceType + " -- " + q.name + " -- " + q.subQueries + " -- " + q.oColumns.toString() + " -- " +  q.constraints + " -- " +q.windows)
      q.subQueries.foreach(x => println(x.name))
    }else{
      println(q.serviceType + " -- " + q.name + " -- " + q.subQueries + " -- " + q.oColumns.toString() + " -- " +  q.constraints + " -- " +q.windows)
      println("Include query consists of services :")
      q.subQueries.foreach(x => {
        println(x.name + x.subQueries + x.oColumns + "     ")
        if(x.ops != null){
          println(", and ops is : " + x.ops  + "     ")
        }
      })
    }
  }

  /**
    * print the time cost based on the two given timestamps
    * @param starttime start timestamp
    * @param endtime end timestamp
    * @return the interval of the two timestamps
    */
  def printTimeCost(starttime:Date,endtime:Date): Long ={
    val between = endtime.getTime - starttime.getTime
    val day = between / (24 * 60 * 60 * 1000)
    val hour = between / (60 * 60 * 1000) - day * 24
    val min = (between / (60 * 1000)) - day * 24 * 60 - hour * 60
    val s = between / 1000 - day * 24 * 60 * 60 - hour * 60 * 60 - min * 60
    val ms = between - day * 24 * 60 * 60 * 1000 - hour * 60 * 60 * 1000 - min * 60 * 1000 - s * 1000
    println(min + "minutes" + s + "seconds" + ms + "milliseconds")
    s*1000 + ms
  }



}
