000100040225     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200001006
000300040225      *------------------------------------------------------------------------*
000400040415      * Dati errati in bolla  - DUT
000500040225      *------------------------------------------------------------------------*
000600111107     FTNTBE01L  IF   E           K DISK
000700111107     fTitas30c  if   e           k Disk
000800111107     ffiar531c  if a e           k Disk
000900111107     fFiar500r  if   e             Disk    rename(fiar5000:fiar50rr)
001000040415     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001100111107      *
001200111107     d AR5n            s                   like(AR5trd)  dim(50)
001300111107     d dAR5          e ds                  inz
001400111107     d dtasflo       e ds
001500040302
001600040415     iTitas000      01
001700040415     iTitas010      02
001800040415     iTitasp00      03
001900040225
002000001006      *------------------------------------------------------------------------*
002100040415     C     *entry        plist
002200040415     c                   parm                    aGGIORNA          1
002300111107     C     Kar5f         KLIST
002400040415     C                   KFLD                    ar5aas
002500040415     C                   KFLD                    ar5lnp
002600040415     C                   KFLD                    ar5nrs
002700040415     C                   KFLD                    ar5nsp
002800111107     C                   KFLD                    ar5trd
002900111107     C     Ktas          KLIST
003000111107     C                   KFLD                    ar5aas
003100111107     C                   KFLD                    ar5lnp
003200111107     C                   KFLD                    ar5nrs
003300111107     C                   KFLD                    ar5nsp
003400111107     c
003500111107      * Effettuo caricamento dei tipi record FIAR5 da NON trasmettere
003600111107     c                   clear                   ii                3 0
003700111107     c                   clear                   AR5n
003800111107     c                   eval      TBEcod   = 'AR5'
003900111107     c     TBEcod        setll     TNTBE000
004000111107     c     TBEcod        reade     TNTBE000
004100111107do  1c                   DOW       Not %Eof(TNTBE01L)
004200111107if  2c                   if        TBEatb   = *blanks
004300111107     c                   eval      dAR5 = TBEuni
004400111107if  3c                   IF        §AR5trmi <> 'S'
004500111107     c                   add       1             ii
004600111107     c                   movel     TBEke1        AR5n(ii)
004700111107e   3c                   ENDIF
004800111107e   2c                   endif
004900111107     c     TBEcod        reade     TNTBE000
005000111107e   1c                   ENDDO
005100040225
005200111107     c                   Read      Fiar500r
005300040226
005400111107    1c                   dow       not %eof(fiar500r)
005500040415     c                   clear                   wsiwrite          1
005600111107     c     kar5f         chain     fiar531c                           30
005700111107     c
005800111107     c                   if        %found(fiar531c) and ar5trd<>'GEN'
005900111107     c                   except    erroreS
006000111107     c                   select
006100111107     c                   when      ar5trd='BAN'
006200111107     c                   add       1             totbans           9 0
006300111107     c                   when      ar5trd='BNB'
006400111107     c                   add       1             totbnbs           9 0
006500111107     c                   when      ar5trd='RCC'
006600111107     c                   add       1             totrccs           9 0
006700111107     c                   when      ar5trd='EMD'
006800111107     c                   add       1             totemds           9 0
006900111107     c                   other
007000111107     c                   add       1             totS              9 0
007100111107     c                   ENDSL
007200111107     c
007300111107     c                   else
007400111107     c
007500111107     c                   if        not %found(fiar531c) and ar5trd<>'GEN'
007600040420    4c                   endif
007700111107    4c                   endif
007800111107     c
007900111107     c                   if        not %found(fiar531c) and ar5trd= 'GEN'
008000111107     c                   except    erroreN
008100111107     c                   select
008200111107     c                   when      ar5trd='GEN'
008300111107     c                   add       1             totgenN           9 0
008400111107     c                   endsl
008500111107     c                   exsr      aggfiar5
008600111107    4c                   endif
008700111107     c
008800111107     c                   select
008900111107     c                   when      ar5trd='GEN'
009000111107     c                   add       1             totgen            9 0
009100111107     c                   when      ar5trd='BAN'
009200111107     c                   add       1             totban            9 0
009300111107     c                   when      ar5trd='BNB'
009400111107     c                   add       1             totbnb            9 0
009500111107     c                   when      ar5trd='RCC'
009600111107     c                   add       1             totrcc            9 0
009700111107     c                   when      ar5trd='EMD'
009800111107     c                   add       1             totemd            9 0
009900111107     c                   other
010000111107     c                   add       1             tot               9 0
010100111107     c                   ENDSL
010200040415
010300040225
010400111107     c                   Read      Fiar500r
010500040415    1c                   EndDo
010600010202
010700111107     c                   except    totale
010800111107     c
010900040225     c                   Eval      *Inlr = *On
011000111107     c*--------------------------------------------
011100111107     c     AggFIAR5      BEGSR
011200111107     c
011300111107     c                   if        ar5trd='EMD'
011400111107     c                   leavesr
011500111107     c                   endif
011600111107     c
011700111107      * - fleggo come ricevuto in filiale
011800111107     C                   eval      AR5ft1 = 'R'
011900111107     C                   eval      AR5dt1 = 20111107
012000111107      * - fleggo come ricevuto in sede
012100111107     C                   eval      AR5ft2 = 'R'
012200111107     C                   eval      AR5dt2 = 20111107
012300111107      * - verifico se per il tipo record di FIAR5 è previsto il
012400111107      *   ritorno info al cliente: se non è previsto imposto
012500111107      *   il record come già trasmesso (cioè da non trasmettere).
012600111107     c                   eval      *in30  =  *off
012700111107     c     AR5trd        lookup    AR5n                                   30
012800111107if  3c                   IF        *in30
012900111107     c                   eval      AR5ft3 = 'T'
013000111107     c                   eval      AR5dt3 = *all'9'
013100111107x   3c                   ELSE
013200111107     c                   clear                   AR5ft3
013300111107     c                   clear                   AR5dt3
013400111107e   3c                   ENDIF
013500111107      *
013600111107     C                   WRITE     FIAR5000                             38      IGNORO SE GIA' PRESE
013700111107     c                   select
013800111107     c                   when      ar5trd='GEN'
013900111107     c                   add       1             totgenW           9 0
014000111107     c                   when      ar5trd='BAN'
014100111107     c                   add       1             totbanW           9 0
014200111107     c                   when      ar5trd='BNB'
014300111107     c                   add       1             totbnbW           9 0
014400111107     c                   when      ar5trd='RCC'
014500111107     c                   add       1             totrccW           9 0
014600111107     c                   when      ar5trd='EMD'
014700111107     c                   add       1             totemdW           9 0
014800111107     c                   other
014900111107     c                   add       1             totW              9 0
015000111107     c                   ENDSL
015100111107     c
015200111107     c                   ENDSR
015300111107     OQSYSPRT   E            erroreN     1
015400111107     O                                         +  1 'FIAr5 non ricevuto '
015500111107     O                       ar5aas        z   +  1
015600111107     O                       ar5lnp        z   +  1
015700111107     O                       ar5nrs        z   +  1
015800111107     O                       ar5nsp        2   +  1
015900111107     O                       ar5trd            +  1
016000111107     O                       ar5ft1            +  1
016100111107     O                       ar5dt1            +  1
016200111107     OQSYSPRT   E            erroreS     1
016300111107     O                                         +  1 'FIAr5 ricevuto     '
016400111107     O                       ar5aas        z   +  1
016500111107     O                       ar5lnp        z   +  1
016600111107     O                       ar5nrs        z   +  1
016700111107     O                       ar5nsp        2   +  1
016800111107     O                       ar5trd            +  1
016900111107     O                       ar5ft1            +  1
017000111107     O                       ar5dt1            +  1
017100111107     OQSYSPRT   E            totale      1
017200111107     O                                         +  1 'GEN :'
017300111107     O                       totgen        2   +  1
017400111107     o                                         +  3 'non ricevuto'
017500111107     O                       totgenn       2   +  1
017600111107     OQSYSPRT   E            totale      1
017700111107     O                                         +  1 'BAN :'
017800111107     O                       totban        2   +  1
017900111107     o                                         +  3 '    ricevuto'
018000111107     O                       totbanS       2   +  1
018100111107     OQSYSPRT   E            totale      1
018200111107     O                                         +  1 'BNB :'
018300111107     O                       totbnb        2   +  1
018400111107     o                                         +  3 '    ricevuto'
018500111107     O                       totbnbS       2   +  1
018600111107     OQSYSPRT   E            totale      1
018700111107     O                                         +  1 'EMD :'
018800111107     O                       totemd        2   +  1
018900111107     o                                         +  3 '    ricevuto'
019000111107     O                       totemdS       2   +  1
019100111107     OQSYSPRT   E            totale      1
019200111107     O                                         +  1 'RCC :'
019300111107     O                       totrcc        2   +  1
019400111107     o                                         +  3 '    ricevuto'
019500111107     O                       totrccS       2   +  1
019600111107     OQSYSPRT   E            totale      1
019700111107     O                                         +  1 'ALTRO:'
019800111107     O                       tot           2   +  1
019900111107     o                                         +  3 '    ricevuto'
020000111107     O                       totS          2   +  1
020100111107     OQSYSPRT   E            totale      1
020200111107     O                                         +  1 'Scritti:'
020300111107     O                                         +  1 'GEN:'
020400111107     O                       totgenW       2   +  1
020500111107     O                                         +  2 'BAN:'
020600111107     O                       totbAnW       2   +  1
020700111107     O                                         +  2 'BNB:'
020800111107     O                       totbNbW       2   +  1
020900111107     O                                         +  2 'EMD:'
021000111107     O                       totEMDW       2   +  1
021100111107     O                                         +  2 'RCC:'
021200111107     O                       totRCCW       2   +  1
021300111107     O                                         +  2 'AlTRO:'
021400111107     O                       totw          2   +  1
