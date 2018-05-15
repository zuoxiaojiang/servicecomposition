package ncut.decloud.servicecomposition

import java.util.Date

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object MiniConServiceComb {

  //default constructor of MCD
  class MCD(Service: QueryService, subGoals: Set[QueryService]){
    var s : QueryService = Service
    var gc : Set[QueryService] = subGoals
    var inputs : Set[(String, Int, Int)] = Set()
    var outputs : Set[String] = Set()
    var addition : Set[(String, Int, Int)] = Set()

    override def toString: String = {
      super.toString
//      s.name +" ," + gc.toString() + inputs.toString() + outputs.toString() + addition.toString()
      s.name + " " + gc.toString()
    }
  }

  var joinAttrInQ :Map[String, ListBuffer[QueryService]] = Map()
  var g1:ServiceGraph = _
  var times = 0

  def genEachMCD(querySubgoal: QueryService, service: QueryService, query: QueryService, joinAttrs:Set[String]):(Boolean,MCD) = {
    val mcd = new MCD(service, Set())
    var canGenerate = false

    if (service.subQueries.contains(querySubgoal)){
      val serviceSubgoal = service.subQueries.find(_ == querySubgoal).get

      val querySubHead = query.oColumns & querySubgoal.oColumns
      val serviceSubHead = service.oColumns & serviceSubgoal.oColumns

      if (querySubHead.subsetOf(serviceSubHead)){
        mcd.gc += querySubgoal
        canGenerate = true
        val existentialAttrs = serviceSubgoal.oColumns &~ serviceSubHead  //existentialAttrs

        val joinAttrInService = existentialAttrs & joinAttrs &~ querySubHead  //extra join attr in service

        joinAttrInService.foreach(attr => {
          val subgoalsInAttr = joinAttrInQ.getOrElse(attr,Set())

          subgoalsInAttr.foreach({subgoal =>
            val returnValue = genEachMCD(subgoal, service, query, joinAttrs-attr)
            mcd.gc ++= returnValue._2.gc
            canGenerate &&= returnValue._1
          })
        })
      }
    }
    (canGenerate, mcd)
  }

  def formMCDs(Q:QueryService, S: Set[QueryService]) :Set[MCD] = {
    //init MCDs
    var MCDs:Set[MCD] = Set()
    println("Start ......")
    val joinAttr = joinAttrInQ.keySet
    //querySubgoal: subgoal of Q; service: service in S; serviceSubgoal: subgoal in servcie.
    Q.subQueries.foreach(querySubgoal => {
      S.foreach(service => {
        if (service.winContains(Q)){
          val cmpQueryService = Q.dataContains(service)
          if (!MCDs.exists(x => x.s == service && x.gc.contains(querySubgoal)) && !cmpQueryService._2.equals("uncontain")){
            val result = genEachMCD(querySubgoal, service, Q, joinAttr)
            if (result._1){
//              if(service.serviceType == 0)
//                result._2.outputs = service.oColumns
              MCDs += result._2
            }
          }
        }
      })
    })
    MCDs
  }

  def combineMCDs(MCDs : Set[MCD]) :mutable.Map[Set[QueryService], Set[Set[QueryService]]] = {
    val comMap :mutable.Map[Set[QueryService], Set[Set[QueryService]]] = mutable.Map()
    MCDs.foreach(mcd => {
      comMap.keySet.foreach(key => {
        if ((mcd.gc & key).isEmpty ){
          times += 1
          comMap.get(key.union(mcd.gc)) match {
            case Some(coms) =>  comMap(key.union(mcd.gc)) = coms ++ comMap(key).map(x=> x+mcd.s)
            case _ =>  comMap.put(key.union(mcd.gc), comMap.getOrElse(key,Set()).map(com => com+mcd.s))
          }
        }
      })
      comMap.get(mcd.gc) match {
        case Some(coms) => comMap(mcd.gc) += Set(mcd.s)
        case _ => comMap.put(mcd.gc, Set(Set(mcd.s)))
      }
    })
    comMap
  }

  def genRewritingService( comMap : mutable.Map[Set[QueryService], Set[Set[QueryService]]], query: QueryService): Set[QueryService] = {
    var rewritings : Set[QueryService]= Set()
    comMap.getOrElse(query.subQueries, Set()).foreach{x =>
      rewritings += CombUtil.genCandiateQuery(x,query,g1,1)
    }
    rewritings
  }

  def main(args: Array[String]): Unit = {

    val timestamps = mutable.ListBuffer[Date]()//保存各阶段时间戳

    //数据源
    val query = DataUtil.query
//    val S = DataUtil.services.toSet//例子中数据源集合
    val S = DataUtil.simuServices.toSet//模拟数据源集合

    //获取查询中主键以及主键对应的关系
    joinAttrInQ = CombUtil.genQueryJoinAttrs(query)
    println(joinAttrInQ)

    //构建服务图，并初始化服务输出属性
    g1 = CombUtil.initServiceGraph(S,joinAttrInQ)

    //构建MCD
    timestamps.append(new Date) //start time of form  mcds
    val MCDs = formMCDs(query,S)
    timestamps.append(new Date)

    //组合MCD
    timestamps.append(new Date)
    val comMap = combineMCDs(MCDs)
    timestamps.append(new Date)

    //通过MCD组合生成候选查询服务
    timestamps.append(new Date)
    val rewritings = genRewritingService(comMap,query)
    timestamps.append(new Date)

    //对候选查询服务进行可执行检查
    timestamps.append(new Date)
    val execplans = CombUtil.findExecPlans(rewritings,query)
    timestamps.append(new Date)

    //结果打印
    CombUtil.printResult(query,comMap(query.subQueries),execplans,timestamps.toList)
//    comMap.foreach(x => println(x))
  }

}
