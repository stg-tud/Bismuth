package de.tu_darmstadt.informatik.st.reform

import de.tu_darmstadt.informatik.st.reform.entity.Document
import de.tu_darmstadt.informatik.st.reform.entity.*
import de.tu_darmstadt.informatik.st.reform.npm.IIndexedDB
import de.tu_darmstadt.informatik.st.reform.repo.Repository

case class Repositories(
    projects: Repository[Project],
    users: Repository[User],
    hiwis: Repository[Hiwi],
    supervisors: Repository[Supervisor],
    contractSchemas: Repository[ContractSchema],
    paymentLevels: Repository[PaymentLevel],
    salaryChanges: Repository[SalaryChange],
    requiredDocuments: Repository[Document],
    contracts: Repository[Contract],
)

object Repositories {
  def apply()(using indexedDb: IIndexedDB): Repositories = this(
    Repository("project", Project.empty),
    Repository("user", User.empty),
    Repository("hiwi", Hiwi.empty),
    Repository("supervisor", Supervisor.empty),
    Repository("contract-schema", ContractSchema.empty),
    Repository("payment-level", PaymentLevel.empty),
    Repository("salary-change", SalaryChange.empty),
    Repository("required-document", Document.empty),
    Repository("contracts", Contract.empty),
  )
}
