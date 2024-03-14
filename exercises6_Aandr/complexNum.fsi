module complexNum

type complexNum
    val ( +. ) : complexNum*complexNum -> complexNum
    val ( -. ) : complexNum*complexNum -> complexNum
    val ( -.. ): complexNum -> complexNum 
    val ( /. ) : complexNum*complexNum -> complexNum
    val ( *. ) : complexNum*complexNum -> complexNum
    val toflts: complexNum -> float*float
    val make : float * float -> complexNum