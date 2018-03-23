000100010323     H DECEDIT('0,') DATEDIT(*YMD.)
000200010323
000300010323      *------------------------------------------------------------------------*
000400010323      *                                                                        *
000500010323      *          AGGIORNAMENTO ORM ALLA FASE 900                               *
000600010323      *                                                                        *
000700010323      *------------------------------------------------------------------------*
000800010323
000900010323     FFNORM06L  UF   E           K DISK
001000010323     F**FNORM06L  IF   E           K DISK
001100010323     FFNBL403L  IF   E           K DISK
001200010323     FFNORF00F  O    E             DISK
001300010323     F**qsysprt   O    f  132        PRINTER
001400010323
001500010323     D kbl4trc         S                   LIKE(BL4trc)
001600010323     D kbl4n14         S                   LIKE(BL4n14)
001700010323
001800010323     D Parm01          DS
001900010323     D  poaggiorna                    3
002000010323
002100010323      *------------------------------------------------------------------------*
002200010323      *                                                                        *
002300010323      *          OPERAZIONI INIZIALI                                           *
002400010323      *                                                                        *
002500010323      *------------------------------------------------------------------------*
002600010323
002700010323     C     *ENTRY        PLIST
002800010323     C                   PARM                    parm01
002900010323
003000010323     C                   movel     poaggiorna    kpor              3 0
003100010323
003200010323     C     kfnbl4        klist
003300010323     C                   kfld                    Kbl4trc
003400010323     C                   kfld                    kbl4n14
003500010323     C                   movel     'M'           kbl4trc
003600010323
003700010323      *****************************************************************
003800010323      *                                                               *
003900010323      *    ROUTINE PRINCIPALE                                         *
004000010323      *                                                               *
004100010323      *****************************************************************
004200010323
004300010323     C     kpor          setll     fnorm06l
004400010323     C                   do        *hival
004500010323     C     kpor          reade     fnorm06l
004600010323     C                   if        %eof(fnorm06l)
004700010323     C                   leave
004800010323     C                   endif
004900010323
005000010323     C                   if        ormfao >= 900
005100010323     C                   iter
005200010323     C                   endif
005300010323
005400010323     C                   if        ormdfo >= 20010315
005500010323     C                   iter
005600010323     C                   endif
005700010323
005800010323     C                   clear                   worm             14
005900010323     C                   move      ormpoe        wpoe              3
006000010323     C                   move      ormnsr        wnsr              2
006100010323     C                   move      ormnor        wnor              7
006200010323     C                   move      ormnrv        wnrv              2
006300010323     C                   eval      worm = wpoe + wnsr
006400010323     C                             + wnor + wnrv
006500010323     C                   movel     worm          kbl4n14
006600010323     C     kfnbl4        chain     fnbl403l
006700010323     C                   if        %found(fnbl403l)
006800010323     C                             and bl4dtr <> 0
006900010323     C                   exsr      Sr_Aggiorna
007000010323     C                   endif
007100010323
007200010323     C                   enddo
007300010323
007400010323     C                   seton                                        lr
007500010323
007600010323      *****************************************************************
007700010323      * Modifico ORM in fase 900 e creo la fase
007800010323      *****************************************************************
007900010323     C     Sr_Aggiorna   BEGSR
008000010323
008100010323      * Aggiorno ORM e scrivo fase
008200010323     C                   z-add     bl4dtr        ormdfo
008300010323     C                   z-add     *zeros        ormofo
008400010323     C                   z-add     900           ormfao
008500010323     C                   eval      ormflt = *blanks
008600010323     C                   eval      ormdtt = *zeros
008700010323     C**                 except    riga
008800010323     C                   update    fnorm000
008900010323
009000010323     C                   eval      orfatb = *blanks
009100010323     C                   eval      orfpoe = ormpoe
009200010323     C                   eval      orfnsr = ormnsr
009300010323     C                   eval      orfnor = ormnor
009400010323     C                   eval      orfnrv = ormnrv
009500010323     C                   eval      orfpog = ormpoe
009600010323     C                   eval      orfdae = ormdfo
009700010323     C                   eval      orfore = ormofo
009800010323     C                   eval      orffar = ormfao
009900010323     C                   eval      orfcar = *blanks
010000010323     C                   eval      orfpue = 'BATCH'
010100010323     C                   eval      orfftr = *blanks
010200010323     C                   eval      orfdtr = *zeros
010300010323     C                   write     fnorf000
010400010323     C                   feod      fnorf00f
010500010323
010600010323     C                   endsr
010700010323
010800010323     O**qsysprt   e            riga        1
010900010323     O**                       ormtor              +1
011000010323     O**                       ormpoe              +1
011100010323     O**                       ormnor        z     +1
011200010323     O**                       ormtco              +1
011300010323     O**                       ormcar              +1
011400010323     O**                       ormrsr              +1
011500010323     O**                       ormpor              +1
011600010323     O**                       ormfao              +1
011700010323     O**                       ormdfo              +1
011800010323     O**                       ormflt              +1
011900010323     O**                       ormdtt              +1
