000100030116     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300030904      *       prompt di controllo dati ricevuti dal partner
000400000000      *****************************************************************
000500130822     FAZORG01L  IF   E           K DISK
000600040826     fkfazn11l  if   E           k disk
000700040830     fTnEcf02l  if   E           k disk
000800040819     Ffieu86D   CF   E             WORKSTN
000900040830     F                                     SFILE(eu86ws1:NRR)
001000030116      *****************************************************************
001100030116     d KPJBA         E DS
001200030120     D Tibs36ds      E DS
001300000000     D CNCR80        E DS
001400130822     D OG143         E DS
001500000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001600040819     D*
001700040819     D fieu86ds      e DS                  inz
001800940926     D*
001900000000     D WLBDAT          DS
002000940926     D  G02DAT                 1      8  0
002100940926     D  G02INV                 9     16  0
002200940926     D  G02ERR                17     17
002300940926     D  G02TGI                18     22  0
002400040831      *
002500040831     D Datg            S              8  0 DIM(50) INZ(0)
002600041013     D Datf            S              8  0 DIM(50) INZ(0)
002700041013     D NrfP            S             15    DIM(50) INZ(' ')
002800030205      *
002900030116     D digits          C                   CONST('0123456789')
003000030116      *****************************************************************
003100040830     c     ksped         klist
003200040830     c                   kfld                    ecfaas
003300040830     c                   kfld                    ecflnp
003400040830     c                   kfld                    ecfnrs
003500040830     c                   kfld                    ecfnsp
003600040830      *
003700000000     C     *ENTRY        PLIST
003800000000     C                   PARM                    KPJBA
003900030120      *
004000030825     C* Imposta la UDATE  da TIME
004100030825     C                   TIME                    W0140            14 0
004200030825     C                   MOVE      W0140         UDATE8            8 0
004300030825     C*
004400030120      *  In base all'utente prende il P.O.
004500030120     C                   CLEAR                   Tibs36Ds
004600030120     c                   EVAL      I36ute = Knmus
004700030120     c                   EVAL      I36Tla = 'L'
004800030120     C                   CALL      'TIBS36R'
004900030120     C                   PARM                    Tibs36Ds
005000030827     c                   movel     KNMUS         ALFA3             3
005100030829     c                   setoff                                       0102
005200030829      * solo per EDP....:
005300030829     c                   if        ALFA3 ='EDP'
005400030829     c                   seton                                        01
005500030829     c                   end
005600030829      * per utenti di filiale:
005700030829     c                   if        ALFA3<>'EDP' and O36Pou <>46
005800030829     c                   seton                                        02
005900030829     c                   end
006000000000      *--------------------------------------------------------------*
006100000000      *             M A I N      L I N E
006200000000      *--------------------------------------------------------------*
006300000000     C                   Z-ADD     1             CODUT
006400000000     C                   CALL      'X§PARUT'
006500000000     C                   PARM                    UT§DSE
006600000000     C                   MOVEL     RAGUT         RSUT             20
006700000000     C                   MOVEL     RAGUT         RSUT2            20
006800000000     C                   MOVEL     REC80         CNCR80
006900040819      *
007000040819     C     INIZIO        tag
007100030825      *
007200040819     C                   Z-ADD     *ZERO         D1DDAL                         DATA INIZIALE
007300030116     C                   Z-ADD     *ZERO         D1DAL                          DATA FINALE
007400040819     c                   clear                   D1NTW
007500030915      *
007600000000      * Emetto il formato video
007700030116     C     Video1        TAG
007800020308     C                   SETOFF                                       90
007900040819     C                   EXFMT     eu86d01
008000000000      *
008100000000     C   KC              GOTO      FINE                                         F3=USCITA
008200000000      *
008300000000      * Eseguo i  controlli sui campi immessi a video
008400000000     C                   EXSR      CONTR
008500000000      *
008600020308      * Se non vi sono errori ma non e' premuto F6 emetto video
008700030915     C   90
008800040819     CorNkf              GOTO      Video1
008900030905      *
009000040819     c                   clear                   fieu86ds
009100030905      * compone il codice numerico
009200040819     c                   z-add     DalAmg        e86dti
009300040819     c                   z-add     AlAmg         e86dtf
009400040819     c                   move      d1ntw         e86ntw
009500000000      *
009600040831      * video lista
009700040830     c                   exsr      sql_video
009800040831     c                   write     eu86wz1
009900040831     c   34              write     eu86wn1
010000040831     c                   dou       *inkl or *inkc
010100040831     c                   exfmt     eu86wc1
010200040831     C                   ENDdo
010300040831     C   KC              GOTO      FINE                                         F3=USCITA
010400040830      *
010500040819      *  richiama il pgm che esegue il consolidamento
010600040830     c                   goto      no_batch
010700040819     c                   movel     fieu86ds      kpjbu
010800040826     c                   movel     'EU87'        kcoaz
010900040826     c     kcoaz         chain     kfazn11l
011000040826     c                   if        not %Found(kfazn11l)
011100040826     c                   call      'FIEU87R'
011200040826     c                   parm                    kpjba
011300040826     c                   else
011400041012     c                   if        ALFA3 ='EDP'
011500040819     c                   call      'BCH09'
011600040819     c                   parm                    kpjba
011700041026     c                   end
011800040819     c                   call      'BCH10'
011900040819     c                   parm                    kpjba
012000040826     c                   end
012100040830     c     no_batch      tag
012200040819      *
012300030116     C                   GOTO      INIZIO
012400030116      *
012500000000     C     FINE          TAG
012600000000     C                   SETON                                        LR
012700000000      *--------------------------------------------------------------*
012800000000      * Routine per i controlli sui campi del formato video          *
012900000000      *--------------------------------------------------------------*
013000000000     C     CONTR         BEGSR
013100000000      *
013200030116     c                   movea     '0000000000'  *in(41)
013300040819      * --------------
013400040819      * controllo immissione delle date
013500040819     C                   Z-ADD     d1DDAL        G02DAT
013600040819     C                   MOVEL     *BLANK        G02ERR
013700040819     C                   CALL      'XSRDA8'
013800040819     C                   PARM                    WLBDAT
013900040819     C     G02ERR        IFEQ      '1'
014000040819     C                   SETON                                        44  90
014100040819     C                   GOTO      endct1
014200040819     C                   ENDIF
014300040819     C                   Z-ADD     G02INV        DalAmg            8 0
014400040819     C                   Z-ADD     G02dat        D1ddal
014500040819      *
014600040819      * Controllo data finale
014700040819     C     d1Dal         ifeq      0
014800040819     C                   Z-ADD     d1DDal        d1Dal
014900040819     C                   End
015000040819      *
015100030116     C                   Z-ADD     d1Dal         G02DAT
015200020624     C                   MOVEL     *BLANK        G02ERR
015300020624     C                   CALL      'XSRDA8'
015400020624     C                   PARM                    WLBDAT
015500020624     C     G02ERR        IFEQ      '1'
015600020624     C                   SETON                                        42  90
015700030116     C                   GOTO      endct1
015800020624     C                   ENDIF
015900030825     c*
016000030116     C                   Z-ADD     G02INV        AlAmG             8 0
016100030116     C                   Z-ADD     G02dat        d1dal
016200040819     c*
016300040819     C     DalAmg        ifgt      AlAmg
016400040819     C                   SETON                                        4590
016500040819     C                   GOTO      endct1
016600040819     C                   END
016700040819      *
016800040819     C     d1ntw         ifeq      *blank
016900040819     C                   SETON                                        4190
017000040819     C                   GOTO      endct1
017100130822     c                   else
017200130822      ***
017300130822     c                   if        d1ntw <>'DPD' and d1ntw <>'FED'
017400130822      * controlla la LNP se network EDI estero
017500130822     c                   z-add     0             dove              3 0
017600130822     c     digits        check     d1ntw         dove
017700130822     c                   if        dove > 0
017800130822      * ci sono alfanumerici
017900130822     C                   SETON                                        4190
018000130822     C                   GOTO      endct1
018100130822     C                   Else
018200130822     c                   move      d1ntw         ntw_num3          3 0
018300130822     c     ntw_num3      chain     azorg01l
018400130822     c                   if        not %Found(azorg01l)
018500130822     C                   SETON                                        4190
018600130822     C                   GOTO      endct1
018700130822     C                   Else
018800130822     c                   eval      og143 = orgde3
018900130822     c                   if        §ogNTW <> 'EEX'
019000130822     C                   SETON                                        4190
019100130822     C                   GOTO      endct1
019200130822     c                   end
019300130822     c                   end
019400130822     C                   End
019500130822      *
019600130822     C                   End
019700130822      ***
019800040819     C                   End
019900040819      *
020000030116     C     endct1        ENDSR
020100030904      *--------------------------------------------------------------*
020200040830      *--------------------------------------------------------------*
020300040830     c     sql_video     begsr
020400040830      *
020500040831     c                   seton                                        3334
020600040830     c                   write     eu86wc1
020700040830     c                   setoff                                       33
020800040830      *
020900040831     c                   z-add     0             nrr               5 0
021000040830     c                   z-add     1             srn               4 0
021100040831     c                   clear                   datg
021200041013     c                   clear                   datf
021300041013     c                   clear                   nrfP
021400041013     c                   z-add     0             cnt               3 0
021500040830      *
021600040830     C/EXEC SQL
021700040830     C+ Declare A1 Cursor for
021800040830     c+ select * from (
021900040830     c+ select ecfaas, ecflnp, ecfnrs, ecfnsp, count(*) as quanti from (
022000040830     c+ SELECT ecfaas, ecflnp, ecfnrs, ecfnsp, ecfdtg FROM TNECF00F WHERE
022100040830     c+ ecfntw = :e86ntw and ecfdtg between :e86dti and :e86dtf
022200040830     c+ group by  ecfaas, ecflnp, ecfnrs, ecfnsp, ecfdtg) as a
022300040830     c+ group by ecfaas, ecflnp, ecfnrs, ecfnsp) as b
022400040830     c+ where quanti > 1
022500040830     C/END-EXEC
022600040830      *          apertura cursore
022700040830     C/EXEC SQL
022800040830     C+ OPEN A1
022900040830     C/END-EXEC
023000041012     C                   DOU       SqlCod <> 0
023100040830      *          lettura cursore
023200040830     C/EXEC SQL
023300040830     C+ Fetch Next From A1 Into :ecfaas, :ecflnp, :ecfnrs, :ecfnsp
023400040830     C/END-EXEC
023500040830     C                   SELECT
023600040830     C                   WHEN      SqlCod = 100
023700041012     c* a fine file
023800041012     c                   leave
023900041012     c* x errori
024000041012     C                   WHEN      SqlCod < 0
024100041012     C                   seton                                        H1
024200041012     c                   goto      fine_ciclo
024300041012     c*
024400041012     C                   OTHER
024500041012     c*
024600041013     c                   z-add     0             conta_spe         3 0
024700040830     C     ksped         setll     tnecf02l
024800040830     C     ksped         reade     tnecf02l
024900040830     c                   dow       not %eof(tnecf02l)
025000040831      *
025100040831      * Incasella l'importo nel gruppo di appartenenza
025200040831     c                   z-add     1             Ip                3 0
025300040831     c     ecfDTG        lookup    DatG(Ip)                               21
025400040831     c  n21              do
025500040831     c                   z-add     1             Ip
025600040831     c     *zeros        lookup    DatG(Ip)                               21
025700040831     c                   z-add     ecfDTG        DatG(Ip)
025800041013     c                   z-add     ecfDFP        DatF(Ip)
025900041013     c                   move      ecfNFP        nrfP(Ip)
026000041012      **
026100041013     c                   add       1             conta_spe
026200041013     **
026300041013     c                   if        conta_spe > 1
026400041013     c                   eval      ws1spd = %Trim(%char(ecfaas)) + '/' +
026500041013     c                                      %Trim(%char(ecflnp)) + '/' +
026600041013     c                                      %Trim(%char(ecfnrs)) + '/' +
026700041013     c                                      %Trim(%char(ecfnsp))
026800041013     c                   eval      cnt = conta_spe - 1
026900041013     c                   eval      ws1nf1 = nrfP(cnt)
027000041013     c                   eval      ws1df1 = datF(cnt)
027100041013     c                   eval      ws1dc1 = datG(cnt)
027200041013     c                   eval      ws1nf2 = nrfP(conta_spe)
027300041013     c                   eval      ws1df2 = datF(conta_spe)
027400041013     c                   eval      ws1dc2 = datG(conta_spe)
027500041013      **
027600041013     c                   setoff                                       34
027700040830     c                   add       1             nrr
027800040830     c                   write     eu86ws1
027900041013     c                   end
028000041013     **
028100040831     c                   end
028200040830     **
028300040830     C     ksped         reade     tnecf02l
028400040830     c                   enddo
028500040830      *
028600040830     C                   ENDSL
028700040830      *
028800040830     C                   ENDDO
028900040830     C/EXEC SQL
029000040830     C+ Close A1
029100040830     C/END-EXEC
029200040830     C*
029300041012     c     fine_ciclo    tag
029400040830     C                   ENDSR
029500040830      **********************************************************************
