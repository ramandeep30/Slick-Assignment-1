package repository

import Connection.{DBComponent, MySqlComponent, PostgresComponent}

import scala.concurrent.Future

case class Employee(id: Int,name: String, experience: Double)

trait EmployeeTable {

  this: DBComponent =>
  import driver.api._

  private[repository] class EmployeeTable(tag: Tag) extends Table[Employee](tag, "experienced_employee") {

    val id = column[Int]("id", O.PrimaryKey)
    val name = column[String]("name")
    val experience = column[Double]("experience")

    def * = (id,name,experience) <> (Employee.tupled, Employee.unapply)

  }
  val employeeTableQuery = TableQuery[EmployeeTable]

}

trait EmployeeRepo extends EmployeeTable {

  this: DBComponent =>

  import driver.api._

  def create = db.run(employeeTableQuery.schema.create)

  def insert (emp: Employee) =  db.run {
    employeeTableQuery += emp
  }

  def delete(exp: Double) = {
    val query = employeeTableQuery.filter(x=>x.experience === exp)
    val action = query.delete
    db.run(action)
  }

  def updateName(id: Int,name: String):Future[Int] = {
    val query = employeeTableQuery.filter(_.id===id)
      .map(_.name).update(name)
    db.run(query)
  }

  def upsert(employee: Employee): Future[Int] = {
    val query = employeeTableQuery.insertOrUpdate(employee)
    employeeTableQuery+=employee
    db.run(query)
  }

  def getAll: Future[List[Employee]] = db.run{
    employeeTableQuery.to[List].result
  }

  def updateTuple(id: Int, values: (String, Double)): Future[Int] = {
    val query = employeeTableQuery.filter(_.id === id).map(e => (e.name, e.experience)).update(values)
    db.run(query)
  }
}

object EmployeeRepo extends EmployeeRepo with MySqlComponent