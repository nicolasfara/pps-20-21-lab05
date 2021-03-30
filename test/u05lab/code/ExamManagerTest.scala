package u05lab.code

import org.junit.jupiter.api.Assertions.{assertEquals, assertFalse, assertThrows, assertTrue}
import org.junit.jupiter.api.Test


class ExamManagerTest {
  private lazy val em = ExamsManager()

  @Test
  def testExamResultBasicBehaviour(): Unit ={
    assertEquals(ExamsResult.failed().getKind, Kind.FAILED)
    assertFalse(ExamsResult.failed().getEvaluation.isDefined)
    assertFalse(ExamsResult.failed().cumLaude)
    assertEquals(ExamsResult.failed().toString, "FAILED")

    assertEquals(ExamsResult.retired().getKind, Kind.RETIRED)
    assertFalse(ExamsResult.retired().getEvaluation.isDefined)
    assertFalse(ExamsResult.retired().cumLaude)
    assertEquals(ExamsResult.retired().toString, "RETIRED")

    assertEquals(ExamsResult.succeededCumLaude().getKind, Kind.SUCCEEDED)
    assertEquals(ExamsResult.succeededCumLaude().getEvaluation, Option(30))
    assertTrue(ExamsResult.succeededCumLaude().cumLaude)
    assertEquals(ExamsResult.succeededCumLaude().toString, "SUCCEEDED(30L)")

    assertEquals(ExamsResult.succeeded(28).getKind, Kind.SUCCEEDED)
    assertEquals(ExamsResult.succeeded(28).getEvaluation, Option(28))
    assertFalse(ExamsResult.succeeded(28).cumLaude)
    assertEquals(ExamsResult.succeeded(28).toString, "SUCCEEDED(28)")
  }

  @Test
  def optionalTestEvaluationCantBeGraterThan30(): Unit = {
    assertThrows(classOf[IllegalArgumentException], () => ExamsResult.succeeded(32))
  }

  @Test
  def optionalTestEvaluationCantBeSmallerThan18(): Unit = {
    assertThrows(classOf[IllegalArgumentException], () => ExamsResult.succeeded(15))
  }

  private def prepareExam(): Unit = {
    em.createNewCall("gennaio")
    em.createNewCall("febbraio")
    em.createNewCall("marzo")

    em.addStudentResult("gennaio", "rossi", ExamsResult.failed())
    em.addStudentResult("gennaio", "bianchi", ExamsResult.retired())
    em.addStudentResult("gennaio", "verdi", ExamsResult.succeeded(28))
    em.addStudentResult("gennaio", "neri", ExamsResult.succeededCumLaude())

    em.addStudentResult("febbraio", "rossi", ExamsResult.failed())
    em.addStudentResult("febbraio", "bianchi", ExamsResult.succeeded(20))
    em.addStudentResult("febbraio", "verdi", ExamsResult.succeeded(30))

    em.addStudentResult("marzo", "rossi", ExamsResult.succeeded(25))
    em.addStudentResult("marzo", "bianchi", ExamsResult.succeeded(25))
    em.addStudentResult("marzo", "viola", ExamsResult.failed())
  }

  @Test
  def testExamsManager(): Unit = {
    prepareExam()

    assertEquals(em.getAllStudentsFromCall("gennaio"), Set("rossi", "bianchi", "verdi", "neri"))
    assertEquals(em.getAllStudentsFromCall("marzo"), Set("rossi", "bianchi", "viola"))

    assertEquals(em.getEvaluationsMapFromCall("gennaio").size, 2)
    assertEquals(em.getEvaluationsMapFromCall("gennaio")("verdi"), 28)
    assertEquals(em.getEvaluationsMapFromCall("gennaio")("neri"), 30)

    assertEquals(em.getEvaluationsMapFromCall("febbraio").size, 2)
    assertEquals(em.getEvaluationsMapFromCall("febbraio")("bianchi"), 20)
    assertEquals(em.getEvaluationsMapFromCall("febbraio")("verdi"), 30)

    assertEquals(em.getResultsMapFromStudent("rossi").size, 3)
    assertEquals(em.getResultsMapFromStudent("rossi")("gennaio"), "FAILED")
    assertEquals(em.getResultsMapFromStudent("rossi")("febbraio"), "FAILED")
    assertEquals(em.getResultsMapFromStudent("rossi")("marzo"), "SUCCEEDED(25)")

    assertEquals(em.getResultsMapFromStudent("bianchi").size, 3)
    assertEquals(em.getResultsMapFromStudent("bianchi")("gennaio"), "RETIRED")
    assertEquals(em.getResultsMapFromStudent("bianchi")("febbraio"), "SUCCEEDED(20)")
    assertEquals(em.getResultsMapFromStudent("bianchi")("marzo"), "SUCCEEDED(25)")

    assertEquals(em.getResultsMapFromStudent("neri").size, 1)
    assertEquals(em.getResultsMapFromStudent("neri")("gennaio"), "SUCCEEDED(30L)")
  }

  @Test
  def optionalTestExamsManagement(): Unit = {
    prepareExam()

    assertEquals(em.getBestResultFromStudent("rossi"), Option(25))
    assertEquals(em.getBestResultFromStudent("bianchi"), Option(25))
    assertEquals(em.getBestResultFromStudent("neri"), Option(30))
    assertEquals(em.getBestResultFromStudent("viola"), Option.empty)
  }

  @Test
  def optionalTestCantCreateACallTwice(): Unit = {
    prepareExam()
    assertThrows(classOf[IllegalArgumentException], () => em.createNewCall("marzo"))
  }

  @Test
  def optionalTestCantRegisterAnEvaluationTwice(): Unit = {
    prepareExam()
    assertThrows(classOf[IllegalArgumentException], () => em.addStudentResult("gennaio", "verdi", ExamsResult.failed()))
  }
}
