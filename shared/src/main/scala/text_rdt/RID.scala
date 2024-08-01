package text_rdt

type RID = String

given canEqualRidRidOrNull: CanEqual[RID, RID | Null] = CanEqual.derived
given canEqualRidOrNullRid: CanEqual[RID | Null, RID] = CanEqual.derived
