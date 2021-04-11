package u05lab.code
import u05lab.code.Kind.Kind

import scala.collection.mutable


object Kind extends Enumeration {
  type Kind = Value
  val RETIRED, FAILED, SUCCEEDED = Value
}

trait ExamsResult {
  import Kind._
  def getKind: Kind
  def getEvaluation: Option[Int]
  def cumLaude: Boolean
}

trait ExamsManager {
  def createNewCall(call: String)
  def addStudentResult(call: String, student: String, result: ExamsResult)
  def getAllStudentsFromCall(call: String): Set[String]
  def getEvaluationsMapFromCall(call: String): Map[String, Int]
  def getResultsMapFromStudent(student: String): Map[String, String]
  def getBestResultFromStudent(student: String): Option[Int]
}

object ExamsResult {
  def failed(): ExamsResult = ExamsResultImpl(Kind.FAILED, Option.empty, laude = false)
  def retired(): ExamsResult = ExamsResultImpl(Kind.RETIRED, Option.empty, laude = false)
  def succeededCumLaude(): ExamsResult = ExamsResultImpl(Kind.SUCCEEDED, Option(30), laude = true)
  def succeeded(evaluation: Int): ExamsResult = {
    if (evaluation > 30 || evaluation < 18) throw new IllegalArgumentException()
    ExamsResultImpl(Kind.SUCCEEDED, Option(evaluation), laude = false)
  }

  private case class ExamsResultImpl(kind: Kind, evaluation: Option[Int], laude: Boolean) extends ExamsResult {
    override def getKind: Kind = kind
    override def getEvaluation: Option[Int] = evaluation
    override def cumLaude: Boolean = laude

    override def toString: String = kind match {
      case Kind.RETIRED => "RETIRED"
      case Kind.FAILED => "FAILED"
      case Kind.SUCCEEDED if cumLaude => "SUCCEEDED(30L)"
      case _ => s"SUCCEEDED(${evaluation.get})"
    }
  }
}

object ExamsManager {
  def apply(): ExamsManager = ExamManagerImpl()

  private case class ExamManagerImpl() extends ExamsManager {
    private val calls: mutable.Map[String, mutable.Set[(String, ExamsResult)]] = mutable.Map()

    override def createNewCall(call: String): Unit = {
      if (calls.contains(call)) throw new IllegalArgumentException()
      calls += (call -> mutable.Set())
    }

    override def addStudentResult(call: String, student: String, result: ExamsResult): Unit = {
      if (calls(call).exists(e => e._1 == student)) throw new IllegalArgumentException()
      calls(call).add((student, result))
    }

    override def getAllStudentsFromCall(call: String): Set[String] = calls(call).map(e => e._1).toSet

    override def getEvaluationsMapFromCall(call: String): Map[String, Int] = {
      calls(call).filter(e => e._2.getEvaluation.isDefined).map(e => e._1 -> e._2.getEvaluation.get).toMap
    }

    override def getResultsMapFromStudent(student: String): Map[String, String] = {
      calls.map(e => e._1 -> e._2.find(e => e._1 == student).map(e => e._2.toString))
        .filter(e => e._2.isDefined)
        .map(e => e._1 -> e._2.get)
        .toMap
    }

    override def getBestResultFromStudent(student: String): Option[Int] = {
      calls.values.flatten.filter(e => e._1 == student).map(e => e._2.getEvaluation).max
    }
  }
}
