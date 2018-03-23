000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300030904      *       prompt di controllo dati ricevuti dal partner
000400000000      *****************************************************************
000500040826     fkfazn11l  if   E           k disk
000600040830     fTnEcf02l  if   E           k disk
000700040819     Ffieu86D   CF   E             WORKSTN
000800040830     F                                     SFILE(eu86ws1:NRR)
000900030116      *****************************************************************
001000030116     d KPJBA         E DS
001100030120     D Tibs36ds      E DS
001200000000     D CNCR80        E DS
001300000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001400040819     D*
001500040819     D fieu86ds      e DS                  inz
001600940926     D*
001700000000     D WLBDAT          DS
001800940926     D  G02DAT                 1      8  0
001900940926     D  G02INV                 9     16  0
002000940926     D  G02ERR                17     17
002100940926     D  G02TGI                18     22  0
002200040831      *
002300040831     D Datg            S              8  0 DIM(50) INZ(0)
002400041013     D Datf            S              8  0 DIM(50) INZ(0)
002500041013     D NrfP            S             15    DIM(50) INZ(' ')
002600030205      *
002700030116     D digits          C                   CONST('0123456789')
002800030116      *****************************************************************
002900040830     c     ksped         klist
003000040830     c                   kfld                    ecfaas
003100040830     c                   kfld                    ecflnp
003200040830     c                   kfld                    ecfnrs
003300040830     c                   kfld                    ecfnsp
003400040830      *
003500000000     C     *ENTRY        PLIST
003600000000     C                   PARM                    KPJBA
003700030120      *
003800030825     C* Imposta la UDATE  da TIME
003900030825     C                   TIME                    W0140            14 0
004000030825     C                   MOVE      W0140         UDATE8            8 0
004100030825     C*
004200030120      *  In base all'utente prende il P.O.
004300030120     C                   CLEAR                   Tibs36Ds
004400030120     c                   EVAL      I36ute = Knmus
004500030120     c                   EVAL      I36Tla = 'L'
004600030120     C                   CALL      'TIBS36R'
004700030120     C                   PARM                    Tibs36Ds
004800030827     c                   movel     KNMUS         ALFA3             3
004900030829     c                   setoff                                       0102
005000030829      * solo per EDP....:
005100030829     c                   if        ALFA3 ='EDP'
005200030829     c                   seton                                        01
005300030829     c                   end
005400030829      * per utenti di filiale:
005500030829     c                   if        ALFA3<>'EDP' and O36Pou <>46
005600030829     c                   seton                                        02
005700030829     c                   end
005800000000      *--------------------------------------------------------------*
005900000000      *             M A I N      L I N E
006000000000      *--------------------------------------------------------------*
006100000000     C                   Z-ADD     1             CODUT
006200000000     C                   CALL      'X§PARUT'
006300000000     C                   PARM                    UT§DSE
006400000000     C                   MOVEL     RAGUT         RSUT             20
006500000000     C                   MOVEL     RAGUT         RSUT2            20
006600000000     C                   MOVEL     REC80         CNCR80
006700040819      *
006800040819     C     INIZIO        tag
006900030825      *
007000040819     C                   Z-ADD     *ZERO         D1DDAL                         DATA INIZIALE
007100030116     C                   Z-ADD     *ZERO         D1DAL                          DATA FINALE
007200040819     c                   clear                   D1NTW
007300030915      *
007400000000      * Emetto il formato video
007500030116     C     Video1        TAG
007600020308     C                   SETOFF                                       90
007700040819     C                   EXFMT     eu86d01
007800000000      *
007900000000     C   KC              GOTO      FINE                                         F3=USCITA
008000000000      *
008100000000      * Eseguo i  controlli sui campi immessi a video
008200000000     C                   EXSR      CONTR
008300000000      *
008400020308      * Se non vi sono errori ma non e' premuto F6 emetto video
008500030915     C   90
008600040819     CorNkf              GOTO      Video1
008700030905      *
008800040819     c                   clear                   fieu86ds
008900030905      * compone il codice numerico
009000040819     c                   z-add     DalAmg        e86dti
009100040819     c                   z-add     AlAmg         e86dtf
009200040819     c                   move      d1ntw         e86ntw
009300000000      *
009400040831      * video lista
009500040830     c                   exsr      sql_video
009600040831     c                   write     eu86wz1
009700040831     c   34              write     eu86wn1
009800040831     c                   dou       *inkl or *inkc
009900040831     c                   exfmt     eu86wc1
010000040831     C                   ENDdo
010100040831     C   KC              GOTO      FINE                                         F3=USCITA
010200040830      *
010300040819      *  richiama il pgm che esegue il consolidamento
010400040830     c                   goto      no_batch
010500040819     c                   movel     fieu86ds      kpjbu
010600040826     c                   movel     'EU87'        kcoaz
010700040826     c     kcoaz         chain     kfazn11l
010800040826     c                   if        not %Found(kfazn11l)
010900040826     c                   call      'FIEU87R'
011000040826     c                   parm                    kpjba
011100040826     c                   else
011200041012     c                   if        ALFA3 ='EDP'
011300040819     c                   call      'BCH09'
011400040819     c                   parm                    kpjba
011500041026     c                   end
011600040819     c                   call      'BCH10'
011700040819     c                   parm                    kpjba
011800040826     c                   end
011900040830     c     no_batch      tag
012000040819      *
012100030116     C                   GOTO      INIZIO
012200030116      *
012300000000     C     FINE          TAG
012400000000     C                   SETON                                        LR
012500000000      *--------------------------------------------------------------*
012600000000      * Routine per i controlli sui campi del formato video          *
012700000000      *--------------------------------------------------------------*
012800000000     C     CONTR         BEGSR
012900000000      *
013000030116     c                   movea     '0000000000'  *in(41)
013100040819      * --------------
013200040819      * controllo immissione delle date
013300040819     C                   Z-ADD     d1DDAL        G02DAT
013400040819     C                   MOVEL     *BLANK        G02ERR
013500040819     C                   CALL      'XSRDA8'
013600040819     C                   PARM                    WLBDAT
013700040819     C     G02ERR        IFEQ      '1'
013800040819     C                   SETON                                        44  90
013900040819     C                   GOTO      endct1
014000040819     C                   ENDIF
014100040819     C                   Z-ADD     G02INV        DalAmg            8 0
014200040819     C                   Z-ADD     G02dat        D1ddal
014300040819      *
014400040819      * Controllo data finale
014500040819     C     d1Dal         ifeq      0
014600040819     C                   Z-ADD     d1DDal        d1Dal
014700040819     C                   End
014800040819      *
014900030116     C                   Z-ADD     d1Dal         G02DAT
015000020624     C                   MOVEL     *BLANK        G02ERR
015100020624     C                   CALL      'XSRDA8'
015200020624     C                   PARM                    WLBDAT
015300020624     C     G02ERR        IFEQ      '1'
015400020624     C                   SETON                                        42  90
015500030116     C                   GOTO      endct1
015600020624     C                   ENDIF
015700030825     c*
015800030116     C                   Z-ADD     G02INV        AlAmG             8 0
015900030116     C                   Z-ADD     G02dat        d1dal
016000040819     c*
016100040819     C     DalAmg        ifgt      AlAmg
016200040819     C                   SETON                                        4590
016300040819     C                   GOTO      endct1
016400040819     C                   END
016500040819      *
016600040819     C     d1ntw         ifeq      *blank
016700040819     C                   SETON                                        4190
016800040819     C                   GOTO      endct1
016900040819     C                   End
017000040819      *
017100030116     C     endct1        ENDSR
017200030904      *--------------------------------------------------------------*
017300040830      *--------------------------------------------------------------*
017400040830     c     sql_video     begsr
017500040830      *
017600040831     c                   seton                                        3334
017700040830     c                   write     eu86wc1
017800040830     c                   setoff                                       33
017900040830      *
018000040831     c                   z-add     0             nrr               5 0
018100040830     c                   z-add     1             srn               4 0
018200040831     c                   clear                   datg
018300041013     c                   clear                   datf
018400041013     c                   clear                   nrfP
018500041013     c                   z-add     0             cnt               3 0
018600040830      *
018700040830     C/EXEC SQL
018800040830     C+ Declare A1 Cursor for
018900040830     c+ select * from (
019000040830     c+ select ecfaas, ecflnp, ecfnrs, ecfnsp, count(*) as quanti from (
019100040830     c+ SELECT ecfaas, ecflnp, ecfnrs, ecfnsp, ecfdtg FROM TNECF00F WHERE
019200040830     c+ ecfntw = :e86ntw and ecfdtg between :e86dti and :e86dtf
019300040830     c+ group by  ecfaas, ecflnp, ecfnrs, ecfnsp, ecfdtg) as a
019400040830     c+ group by ecfaas, ecflnp, ecfnrs, ecfnsp) as b
019500040830     c+ where quanti > 1
019600040830     C/END-EXEC
019700040830      *          apertura cursore
019800040830     C/EXEC SQL
019900040830     C+ OPEN A1
020000040830     C/END-EXEC
020100041012     C                   DOU       SqlCod <> 0
020200040830      *          lettura cursore
020300040830     C/EXEC SQL
020400040830     C+ Fetch Next From A1 Into :ecfaas, :ecflnp, :ecfnrs, :ecfnsp
020500040830     C/END-EXEC
020600040830     C                   SELECT
020700040830     C                   WHEN      SqlCod = 100
020800041012     c* a fine file
020900041012     c                   leave
021000041012     c* x errori
021100041012     C                   WHEN      SqlCod < 0
021200041012     C                   seton                                        H1
021300041012     c                   goto      fine_ciclo
021400041012     c*
021500041012     C                   OTHER
021600041012     c*
021700041013     c                   z-add     0             conta_spe         3 0
021800040830     C     ksped         setll     tnecf02l
021900040830     C     ksped         reade     tnecf02l
022000040830     c                   dow       not %eof(tnecf02l)
022100040831      *
022200040831      * Incasella l'importo nel gruppo di appartenenza
022300040831     c                   z-add     1             Ip                3 0
022400040831     c     ecfDTG        lookup    DatG(Ip)                               21
022500040831     c  n21              do
022600040831     c                   z-add     1             Ip
022700040831     c     *zeros        lookup    DatG(Ip)                               21
022800040831     c                   z-add     ecfDTG        DatG(Ip)
022900041013     c                   z-add     ecfDFP        DatF(Ip)
023000041013     c                   move      ecfNFP        nrfP(Ip)
023100041012      **
023200041013     c                   add       1             conta_spe
023300041013     **
023400041013     c                   if        conta_spe > 1
023500041013     c                   eval      ws1spd = %Trim(%char(ecfaas)) + '/' +
023600041013     c                                      %Trim(%char(ecflnp)) + '/' +
023700041013     c                                      %Trim(%char(ecfnrs)) + '/' +
023800041013     c                                      %Trim(%char(ecfnsp))
023900041013     c                   eval      cnt = conta_spe - 1
024000041013     c                   eval      ws1nf1 = nrfP(cnt)
024100041013     c                   eval      ws1df1 = datF(cnt)
024200041013     c                   eval      ws1dc1 = datG(cnt)
024300041013     c                   eval      ws1nf2 = nrfP(conta_spe)
024400041013     c                   eval      ws1df2 = datF(conta_spe)
024500041013     c                   eval      ws1dc2 = datG(conta_spe)
024600041013      **
024700041013     c                   setoff                                       34
024800040830     c                   add       1             nrr
024900040830     c                   write     eu86ws1
025000041013     c                   end
025100041013     **
025200040831     c                   end
025300040830     **
025400040830     C     ksped         reade     tnecf02l
025500040830     c                   enddo
025600040830      *
025700040830     C                   ENDSL
025800040830      *
025900040830     C                   ENDDO
026000040830     C/EXEC SQL
026100040830     C+ Close A1
026200040830     C/END-EXEC
026300040830     C*
026400041012     c     fine_ciclo    tag
026500040830     C                   ENDSR
026600040830      **********************************************************************
