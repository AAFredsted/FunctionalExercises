module complexNum
type complexNum = C of float * float
    let ( +. ) C(a1, bi1) C(a2, bi2) = C((a1 + a2), (bi1 + bi2))
    let ( -. ) C(a1, bi1) C(a2, bi2) = C((a1 - a2), (bi1 - bi2))
    let ( -.. ) C(a, bi) = C(-a, -bi)
    let ( *. ) C(a1, bi1) C(a2, bi2) = C((a1 * a2- bi1 * bi2), (bi1 * a2 - a1 - bi2));;
    let ( /. ) (cn1: complexNum) (cn2: complexNum) =            
                                    let numIn C(a, bi)= {
                                        ia = a/(a*a + bi*bi);
                                        ibi = -bi/( a*a + bi*bi);
                                        (ia, ibi)
                                    }
                                    cn *. numIn cn2;;s
                            

