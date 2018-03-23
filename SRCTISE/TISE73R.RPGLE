000100040220      *------------------------------------------------------------------------*
000200160408      * REPERIMENTO SALDI CLIENTE e competenze dio logistica                   *
000300040220      *------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000800040220
000900041220     D*-------------------
001000041220     D* DS DI PROCEDURA
001100041220     D*-------------------
001200160408     D TISE70DS      E DS
001201160408     D TISE72DS      E DS
001202160408     D TISE73DS      E DS
017800041220     C* RICEVIMENTO PARAMETRI
017900040219     C     *ENTRY        PLIST
018000160408     C                   PARM                    TISE73DS
018001160408     C* competenze e ricavi totali
018002160408     c                   clear                   tise70ds
018003160408     c                   eval      I70TLA = I73TLA
018004160408     c                   eval      I70TLE = I73TLE
018005160408     c                   eval      I70KSC = I73KSC
018006160408     c                   eval      I70CTR = I73CTR
018007160408     c                   eval      I70DAI = I73DAI
018008160408     c                   eval      I70DAF = I73DAF
018009160408     c                   call      'TISE70R'
018010160408     c                   parm                    tise70ds
018011160408     c                   eval      O73NSP = O70NSP
018012160408     c                   eval      O73NCO = O70NCO
018013160408     c                   eval      O73PKG = O70PKG
018014160408     c                   eval      O73VOL = O70VOL
018015160408     c                   eval      O73RIC = O70RIC
018016160408     c                   eval      O73RIR = O70RIR
018017160408     c                   eval      O73RIP = O70RIP
020501160408     C* competeze di logistica : è un di cui delle competenze totali
020502160408     c                   clear                   tise72ds
020503160408     c                   eval      I72TLA = I73TLA
020504160408     c                   eval      I72TLE = I73TLE
020505160408     c                   eval      I72KSC = I73KSC
020506160408     c                   eval      I72CTR = I73CTR
020507160408     c                   eval      I72DAI = I73DAI
020508160408     c                   eval      I72DAF = I73DAF
020509160408     c                   call      'TISE72R'
020510160408     c                   parm                    tise72ds
020511160408     c                   eval      O73ICMP =O72ICMP
020600160408     C                   seton                                        lr
