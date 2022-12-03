#lang reader "volk.rkt"


izznaczi
  dodaj-jednu x
  + x 1

izznaczi
  prosty? n
  povratny-nehaj
      testuj
        lambda
          n i
          ako
            >= i n
            , istinny
            ako
              =
                modul n i
                , 0
              , lozsny
              testuj n $ + i 1
    testuj n 2

izznaczi
  prazdny
  cituj ()

izznaczi
  prvy-n-prosti n
  ako
    = n 1
    prazdny
    ako
      prosty? n
      pridaj n
        prvy-n-prosti
          - n 1
      prvy-n-prosti
        - n 1

prvy-n-prosti 100

prosty? 10

prosty? 97



