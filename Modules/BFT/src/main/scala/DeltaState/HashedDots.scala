package DeltaState

case class HashedDots (
                        dots: Set[HashedDot]
                      ):
  def verifySignatures: Boolean =
    var sigValid = List.empty[Boolean]
    for (d <- dots)
      sigValid = sigValid :+ d.verifySignature 
      
    !sigValid.contains(false)
      
