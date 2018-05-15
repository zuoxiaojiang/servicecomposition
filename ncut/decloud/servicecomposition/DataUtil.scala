package ncut.decloud.servicecomposition

object DataUtil {
  //data source
  val vesselinfo = new QueryService("vesselinfo", Set("mmsi","imo","callsign", "name","type","length","width","positionType","eta","draught"))
  val vesseltraj = new QueryService("vesseltraj", Set("mmsi","long","lat","speed"))
  val vesseltravelinfo = new QueryService("vesseltravelinof", Set("imo","dest","source" ))
//    val vesseltraj = new QueryService("vesseltraj", Set("mmsi","long","lat","speed"))
//    val vesseltravelinfo = new QueryService("vesseltravelinof", Set("mmsi","dest","source" ))
//    val Movie = new QueryService("Movie",Set("ID", "Title", "Year", "Genre"))
//    val Revenues = new QueryService("Revenues", Set("ID", "Amount"))
//    val Director = new QueryService("Director", Set("ID", "Dir"))

  val Movie = new QueryService("Movie",Set("ID", "Title", "Year", "Genre","Dir"))
  val Revenues = new QueryService("Revenues", Set("ID", "Amount"))
  val Director = new QueryService("Director", Set("Dir", "Age"))
  val Actor = new QueryService("Actor",Set("ID","Dir"))

  val POSI:Int = Double.PositiveInfinity.toInt  //Positive infinity
  val NEGA:Int = Double.NegativeInfinity.toInt  //Negative infinity

  //service instances and services
  val S1 = new QueryService("S1",Set(vesselinfo,vesseltraj),Set(("mmsi",3000,POSI),("speed",30,POSI)),(5,1))
  val S2 = new QueryService("S2",Set(vesselinfo,vesseltraj),Set("mmsi","callsign","draught","speed"),Set(("speed",30,POSI)),(5,2))
  val S3 = new QueryService("S3",Set(vesseltraj),Set(("speed",NEGA,30)),(5,2))
  val S4 = new QueryService("S4",Set(vesseltraj,vesseltravelinfo),Set(),(5,2))
  val S5 = new QueryService("S1", Set(Movie,Revenues),  Set(("ID",5000,POSI),("Amount",200,POSI)),(5,2))
  val S6 = new QueryService("S2", Set(Movie, Revenues), Set(),(5,2))
  val S7 = new QueryService("S3", Set(Revenues), Set(("Amount",NEGA,50)),(5,2))
  val S8 = new QueryService("S4", Set(Movie, Director), Set(("ID",NEGA,3000)),(5,2))
  val S9 = new QueryService("S9",Set(vesselinfo),Set("mmsi"),Set(),(5,2))
  val S10 = new QueryService("S2",Set(vesseltravelinfo), Set( "dest", "source"),Set(("imo",3000,POSI)),(5,2))
  val S11 = new QueryService("S3",Set(vesseltraj),Set("mmsi", "speed"),Set(("speed",NEGA,30)),(5,1))
  val S12 = new QueryService("S1",Set(vesseltraj,vesselinfo), Set("mmsi", "draught", "speed"),Set(("imo",NEGA,2000)),(5,2))
  val S13 = new QueryService("S4",Set(vesseltravelinfo,vesselinfo), Set("mmsi", "draught", "dest"), Set(("imo",3000, POSI),("speed",30,POSI)),(5,2))
  val S14 = new QueryService("S5",Set(vesseltraj,vesselinfo), Set(("mmsi",1000, POSI)),(5,1))

  //queries
  val query1 = new QueryService("Q", Set(Movie, Revenues,Director),Set("ID","Age","Amount"),Set(("Amount",100,POSI)),(5,2))
  val query2 = new QueryService("Q", Set(vesseltravelinfo, vesseltraj,vesselinfo),Set("mmsi","draught", "speed","dest"),Set(("speed",40,POSI)),(5,4))
  val query3 = new QueryService("Q", Set(vesselinfo, vesseltraj),Set("mmsi","callsign"),Set(("speed",40,POSI)),(5,4))

//  val query5 = new QueryService("Q", Set(vesseltravelinfo, vesseltraj,vesselinfo),Set("mmsi","draught","dest","speed"),Set(("speed",40,POSI)),(5,4))
//  val query4 = new QueryService("Q", Set(Movie,  Director), Set("ID", "Age"), Set(),(5,2))

  val query:QueryService = query2
  val simulSource:Array[QueryService] = Array[QueryService](Movie,Revenues,Director,vesseltraj,vesseltravelinfo,vesselinfo)
  val simuServices:List[QueryService] = SourceSImulation.geneViews(DataUtil.simulSource,query,1000)

  val services = List(S10,S11,S12,S13,S14)
//  val services = List(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13)
//  val services = List(S1,S2,S3,S4)
//  val services = List(S5,S6,S7,S8)
}
