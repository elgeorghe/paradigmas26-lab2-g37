// =====================================================================
// Ejercicio 1: Modelar la jerarquía de entidades
// =====================================================================
import scala.io.Source
/**
 * Clase base abstracta para todas las entidades nombradas.
 *
 * Una entidad nombrada es una expresión del texto que refiere a un objeto
 * del mundo real (persona, lugar, organización, tecnología, etc.).
 *
 * @param text el texto tal como aparece en el corpus
 */
abstract class NamedEntity(val text: String) {

  /**
   * Retorna el tipo de la entidad como String.
   * Ejemplo: "Person", "University", "ProgrammingLanguage"
   *
   * TODO (Ejercicio 1): Implementar en cada subclase concreta.
   */
  def entityType: String = "NamedEntity"

  /**
   * Retorna una línea de descripción de la entidad para el informe.
   *
   * Al usar entityType aquí, este método funciona correctamente para cualquier
   * subclase sin necesidad de redefinirlo. Esto es polimorfismo.
   */
  def describe: String = s"[$entityType] $text"
}
  class ProgrammingLanguage(text: String) extends NamedEntity(text) { override def entityType = "ProgrammingLanguage"}
  class Organization(text: String) extends NamedEntity(text) { override def entityType = "Organization"}
  class Person(text: String) extends NamedEntity(text) { override def entityType = "Person"}
  class University(text: String) extends NamedEntity(text) { override def entityType = "University"}
  class Place(text: String) extends NamedEntity(text) { override def entityType = "Place"}

object EntityClassifier {
  private def loadDictionary(filepath: String): Set[String] = {
    val source = Source.fromFile(filepath)
    try{
      source.getLines().toSet
    } finally {
      source.close()
    }
  }

  private val languagesWords = loadDictionary("data/languages.txt")
  private val organizationsWords = loadDictionary("data/organizations.txt")
  private val peopleWords = loadDictionary("data/people.txt")
  private val universitiesWords = loadDictionary("data/universities.txt")
  private val placesWords = loadDictionary("data/places.txt")

  def classify(text: String): NamedEntity = {
    if (languagesWords.contains(text)) new ProgrammingLanguage(text)
    else if (organizationsWords.contains(text)) new Organization(text)
    else if (peopleWords.contains(text)) new Person(text)
    else if (universitiesWords.contains(text)) new University(text)
    else if (placesWords.contains(text)) new Place(text)
    else new NamedEntity(text) {}
  }
}
// =====================================================================
// TODO (Ejercicio 1): Completar la jerarquía de entidades
//
// Implementar las clases faltantes.
//
// Jerarquía esperada:
//
//   NamedEntity
//   ├── Person
//   ├── Organization
//   │   └── University
//   ├── Place
//   └── Technology
//       └── ProgrammingLanguage
//
// Luego de implementar las clases, este código debe compilar:
//
//   val entities: List[NamedEntity] = List(
//     new Person("Alan Turing"),
//     new University("MIT"),
//     new ProgrammingLanguage("Scala"),
//     new Place("San Francisco")
//   )
//   entities.foreach(e => println(e.describe))
// =====================================================================
