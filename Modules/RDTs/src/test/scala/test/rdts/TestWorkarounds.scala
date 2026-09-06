package test.rdts

import munit.Compare
import rdts.base.Uid

given munit.Compare[rdts.base.Uid, String] = new Compare[Uid, String]:
    override def isEqual(obtained: Uid, expected: String): Boolean = Uid.unwrap(obtained) == expected

val isGithubCi: Boolean = Option(System.getenv("GITHUB_WORKFLOW")).exists(_.nonEmpty)
