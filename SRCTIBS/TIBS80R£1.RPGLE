000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200041220      * tibs80R *----------------------------------------------------*
000300041220      *         - MODIFICA PUNTO OPERATIVO
000400940915      *--------------------------------------------------------------*
000500041220     FTIBS80D   CF   E             WORKSTN
000600951013     FAZORG01L  IF   E           K DISK
000700050103     Ffnfgv06l  IF   E           K DISK
000800050103     Ftntbe01l  IF   E           K DISK
000900020805      *
001000020805      * DEFINIZIONE SCHIERE
001100960302     D*
001200050103     D MSG             S             78    DIM(5) CTDATA PERRCD(1)
001300050214      * DS per TRUL06R - Caricamento £X
001400050214     D TRUL06DS      E DS
001500050214     D  LIN                    1     90  0
001600050214     D                                     DIM(30)
001700050214     D  L1old          s              3  0 DIM(30)
001800050214     D  L1new          s              3  0 DIM(30)
001900050214     D  skipo          s              3    DIM(30)
002000040917      * DS per FNLV55R - Reperimento Terminal di Partenza e di Arrivo
002100040917     D FNLV55DS      E DS
002200040917     D KPJBA         E DS
002300040917     D CNCR80        E DS
002400050414     D og140         E DS
002500040917     D UT§DSE0F      E DS
002600050103     d Dataiso         s               d   datfmt(*iso)
002700050103     d Dataeur         s               d   datfmt(*eur)
002800050103     d
002900050103     d kTbeCod         s                   Like(TbeCod)
003000050103     d kTbeKe1         s                   Like(TbeKe1)
003100050414     d tpoaex          s                   Like(§ogaex)
003200050414     d tpnaex          s                   Like(§ogaex)
003300050103      * Ds date statistiche
003400050103     d dSdf          e ds
003500041220     d
003600041220     D C1livello       C                   CONST('diventerà un 1 livello in par-
003700041220     D                                     tenza:')
003800050103     D C2livello       C                   CONST('diventeranno 2 livello del nu-
003900050103     D                                     ovo')
004000050103     D C2livello2      C                   CONST('terminal indicato:')
004100050103     D Cpoperativo     C                   CONST('Il  punto  operativo')
004200050103     D Cterminal       C                   CONST('Tutti i P.O. del terminal -
004300050103     D                                     partenza')
004400040917     C**********************************************************************
004500050103     c     Ktbe          Klist
004600050103     c                   Kfld                    kTbeCod
004700050103     c                   Kfld                    kTbeKe1
004800050103     C**********************************************************************
004900040917     C     *ENTRY        PLIST
005000040917     C                   PARM                    KPJBA
005100050103      * Calcolo oggi - 3 gg
005200050103     c                   Move      *date         Dataiso
005300050103     c                   subdur    3:*d          Dataiso
005400050103     c                   Move      Dataiso       woggim3           8 0
005500050103     c*
005600040917     C                   Z-ADD     1             CODUT
005700040917     C                   CALL      'X§PARUT'
005800040917     C                   PARM                    UT§DSE0F
005900040917     C                   MOVEL     REC80         CNCR80
006000040917     C                   MOVEL     RAGUT         VIDRSU
006100041220     C                   CLEAR                   V1CPO
006200041220     C                   CLEAR                   V1DPO
006300041220     C                   CLEAR                   V1CTPN
006400041220     C                   CLEAR                   V1CTPO
006500041220     C                   CLEAR                   V1DTPO
006600041220     C                   CLEAR                   V1DTPN
006700050103     C                   CLEAR                   V1cco1
006800050103     C                   CLEAR                   V1cco2
006900050103     C                   CLEAR                   V1cco3
007000050103     C                   CLEAR                   V1cco4
007100050103     C                   CLEAR                   V1cco5
007200041220     C                   CLEAR                   newpo
007300050103     C                   CLEAR                   v1csta
007400050103     c                   setoff                                       010506
007500050103     c                   setoff                                       02
007600040917     c
007700040917     C     FOR01         TAG
007800000828      *
007900041220     C                   EXFMT     BS80D01
008000970102     C                   SETOFF                                       289050
008100040917     C                   SETOFF                                       51
008200040917     C* F3 - FINE
008300040917     C   KC              GOTO      FINE
008400050104     C* F7 - manutenzione organigramma
008500050104     c                   if        *inkg
008600050110     C                   call      'TIBS84C'
008700050104     c                   parm                    kpjba
008800050104     c                   goto      for01
008900050104     c                   endif
009000041220      *  CONTROLLI
009100040917     C                   EXSR      CTRD01
009200040917     c   90              goto      for01
009300000825      *
009400040917     c*
009500050103     c                   if        (not *inkf and not *in01 and not *in02) or
009600050103     c                             (not *inkn and *in01) or
009700050103     c                             (not *inkn and *in02)
009800041220     c                   goto      for01
009900041220     c                   endif
010000040917     C*
010100040917      * F6  - SOTTOMETTO ELABORAZIONE
010200050412    1C     *INKF         IFEQ      *ON
010300041220     C     *INKN         oreq      *ON
010400050103     c
010500050412     c* P.O. 1°livello che diventa 2°livello
010600050412    2c                   if        *in02
010700050214     c* carico in skiera tutti i p.o. 2 livello del terminal che diventa
010800050214     c*  2 liv , che diventano 2 livello del new terminal
010900050214     c* (qualche p.o. infatti potrebber anche cambiare termnal di part
011000050214     c*  in sto caso deve avere una elaborazione a parte, precedente
011100050214     c* questa)
011200050214     c
011300050214     c                   exsr      CARPO
011400050214     c*
011500050214     c                   call      'TIBS81C'
011600041220     c                   parm                    v1cpo
011700041220     c                   parm                    v1ctpo
011800041220     c                   parm                    v1ctpn
011900050214     c                   parm                    skipo
012000050414     c                   parm                    tpoaex
012100050414     c                   parm                    tpnaex
012200050412    2C                   ENDIF
012300050412     c
012400050412     c*  P.O. 2°livello che rimane 2°livello, cambiando terminal partenza
012500050412    2c                   if        not *in01 and not *in02
012600050412     c                   call      'TIBS86C'
012700050412     c                   parm                    v1cpo
012800050412     c                   parm                    v1ctpo
012900050412     c                   parm                    v1ctpn
013000050412     c                   parm                    skipo
013100050414     c                   parm                    tpoaex
013200050414     c                   parm                    tpnaex
013300050412    2C                   ENDIF
013400050412     c
013500050412    1C                   ENDIF
013600900515      *
013700000000     C     FINE          TAG
013800960129     C*
013900040917     c                   clear                   fnlv55ds
014000040917     C                   MOVEL     'C'           D55TLA
014100040917     C                   CALL      'FNLV55R'
014200040917     C                   PARM                    FNLV55DS
014300000000     C                   SETON                                        LR
014400900515      *-----------------------------------------------------***********
014500040917      * CONTROLLI SFL                                       *  CTRD01 *
014600900515      *-----------------------------------------------------***********
014700040917     C     CTRD01        BEGSR
014800050104     c                   setoff                                       010206
014900041220      * PUNTO OPERATIVO
015000041220     C                   MOVE      V1CPO         CODFIL            3 0
015100951025     C                   EXSR      CTRFIL
015200041220    1c                   if        not *in34
015300041220     c                   movel     orgdes        V1DPO
015400040917     c
015500041220   x1C                   ELSE
015600040917     c                   seton                                        509028
015700041220     C                   MOVEL     MSG(1)        V1cMSG
015800041220     c                   goto      endct1
015900041220    1C                   ENDIF
016000041220     c
016100050103     c* Cerco il terminal di partenza  vecchio a udate-3
016200041220     C                   CLEAR                   FNLV55DS
016300041220     C                   MOVEL     'P'           D55TPT
016400041220     C                   MOVE      V1cpo         D55LIN
016500050103     C                   MOVE      woggim3       d55drf
016600041220     C                   CALL      'FNLV55R'
016700041220     C                   PARM                    FNLV55DS
016800041220     c*
016900041220     c                   eval      v1ctpo=d55tfp
017000041220     c* decodifica
017100041220     C                   MOVE      V1Ctpo        CODFIL            3 0
017200041220     C                   EXSR      CTRFIL
017300041220    1c                   if        not *in34
017400041220     c                   movel     orgdes        V1Dtpo
017500050414     C* MEMORIZZO AREA EX AS400 PER QINTER
017600050414     C                   MOVEL     ORGDE0        OG140
017700050414     C                   MOVEL     §OGAEX        TPOAEX
017800041220     c                   else
017900041220     c                   clear                   v1dtpo
018000050414     c                   clear                   TPOAEX
018100041220    1c                   endif
018200050103     c
018300050103     c* Cerco il nuovo terminal di partenza a udate
018400050103     C                   CLEAR                   FNLV55DS
018500050103     C                   MOVEL     'P'           D55TPT
018600050103     C                   MOVE      V1cpo         D55LIN
018700050103     C                   MOVE      *date         d55drf
018800050103     C                   CALL      'FNLV55R'
018900050103     C                   PARM                    FNLV55DS
019000050103     c*
019100050103     c                   eval      v1ctpn=d55tfp
019200050103     c* decodifica
019300050103     C                   MOVE      V1Ctpn        CODFIL            3 0
019400050103     C                   EXSR      CTRFIL
019500050103    1c                   if        not *in34
019600050103     c                   movel     orgdes        V1Dtpn
019700050414     C* MEMORIZZO AREA EX AS400 PER QINTER
019800050414     C                   MOVEL     ORGDE0        OG140
019900050414     C                   MOVEL     §OGAEX        TPnAEX
020000050103     c                   else
020100050103     c                   clear                   v1dtpn
020200050414     c                   clear                   tpnaex
020300050103    1c                   endif
020400050103    c
020500050103     c* Se terminal vecchio e nuovo rimangono uguali, errore
020600050103     c                   if        v1ctpo=v1ctpn
020700050103     c                   clear                   v1csta
020800050103     c                   seton                                        519028
020900050103     C                   MOVEL     MSG(2)        V1cMSG
021000050103     c                   goto      endct1
021100050103    1C                   ENDIF
021200050103     c* Per indicatore 05 on - controllo la data ultima elaborazione statist
021300050103    1c                   if        v1csta>0
021400050103     c     *eur          test(d)                 v1csta                 30
021500050103    2c                   if        *in30
021600050103     c     *dmy          test(d)                 v1csta                 30
021700050103    3c                   if        *in30
021800050103     c                   seton                                        529028
021900050103     C                   MOVEL     MSG(5)        V1cMSG
022000050103     c                   goto      endct1
022100050103     c                   else
022200050103     c* la trasforno in eur
022300050103     c     *dmy          move      v1csta        dataeur
022400050103     c                   move      dataeur       v1csta
022500050103    3c                   endif
022600050103    2c                   endif
022700050103    1c                   endif
022800041220     c
022900041220     c
023000050103     c* P.O. diventa 2 livello in partenza
023100050103     c                   eval      newpo=v1cpo
023200050103    1c                   if        v1ctpo=v1cpo
023300050103     c                   eval      v1cco3=cterminal
023400050103     c                   eval      v1cco4=c2livello
023500050103     c                   eval      v1cco5=c2livello2
023600050103     c                   seton                                        05
023700050103     c
023800050103     c* controllo elaborazioni giornaliere alla data indicata a video
023900050103     c*  se data =0 propongo udate -3
024000050103    2c                   if        v1csta=0
024100050103     c     *iso          move      woggim3       dataeur
024200050103     c                   move      dataeur       v1csta
024300050103    2c                   endif
024400050103     c* Giro la data in *iso
024500050103     c     *eur          move      v1csta        dataiso
024600050103     c                   move      dataiso       wamgsta           8 0
024700050103     c
024800050103     c                   Eval      kTbeCod = 'SDF'
024900050103     c                   Movel(p)  v1cpo         kTbeKe1
025000050103     c     Ktbe          Chain     Tntbe01l
025100050103    2c                   If        %Found(Tntbe01l)
025200050103     c                   Movel     TbeUni        dSdf
025300050103    3c                   If        §SdfParU < wamgsta  or
025400050103     c                             §sdfteru < wamgsta  or
025500050103     c                             §sdfbolu < wamgsta
025600050103     c                   seton                                        529028
025700050103     C                   MOVEL     MSG(4)        V1cMSG
025800050103     c                   goto      endct1
025900050103     c                   else
026000050103     c                   seton                                        06
026100050103    3c                   endif
026200050103    2c                   endif
026300050103     c*
026400050103     c* controllo che non ci siano fgv aperti e non annullati
026500050103     c     v1ctpo        setll     fnfgv06l                               30
026600050103    2c                   if        *in30
026700050103     c                   seton                                        509028
026800050103     C                   MOVEL     MSG(3)        V1cMSG
026900050103     c                   goto      endct1
027000050103    2c                   endif
027100050103     c
027200050103     c                   seton                                        02
027300050103   x1c                   else
027400050103     c                   setoff                                       0501
027500050103     c                   clear                   v1csta
027600050103    1c                   endif
027700050103     c**
027800050103     c* p.o. diventa 1 livello in partenza
027900041220     c                   if        v1ctpn=v1cpo
028000050103     c                   eval      v1cco1=cpoperativo
028100050103     c                   eval      v1cco2=c1livello
028200041220     c                   seton                                        01
028300041220     c                   endif
028400041220     c*
028500900515      *
028600040917     C     ENDCT1        ENDSR
028700951025     C**************************************************************************
028800951025     C*  CONTROLLO UNA FILIALE IN ORGANIGRAMMA
028900951025     C**************************************************************************
029000951025     C     CTRFIL        BEGSR
029100020213
029200951025     C     CODFIL        CHAIN     AZORG01L                           34
029300951025    1C  N34ORGFVA        IFNE      ' '
029400951025     C     ORGFAG        ORNE      'A'
029500951025     C     ORGFAG        ANDNE     'F'
029600951025     C                   SETON                                        34
029700951025    1C                   ENDIF
029800951025     C*
029900951025     C                   ENDSR
030000050214     C**************************************************************************
030100050214     C*  carico p.o. 2 livello che vengono spostati insieme al terminal
030200050214     C**************************************************************************
030300050214     C     CARPO         BEGSR
030400050214     c                   clear                   L1old
030500050214     c                   clear                   L1new
030600050214     C* Carico tabella Filiali Gestite £1
030700050214     C                   CLEAR                   TRUL06DS
030800050214     C                   MOVE      '£1'          D06COD
030900050214     C                   MOVEL     v1cpo         D06KEY
031000050214     C                   MOVE      'S'           D06KEY
031100050214     C                   MOVEL     woggim3       D06drf
031200050214     C                   MOVEL     TRUL06DS      KPJBU
031300050214     C                   CALL      'TRUL06R'
031400050214     C                   PARM                    KPJBA
031500050214     C                   MOVEL     KPJBU         TRUL06DS
031600050214     C                   MOVEA     LIN           L1old
031700050214     c
031800050214     C* Carico tabella Filiali Gestite £1
031900050214     C                   CLEAR                   TRUL06DS
032000050214     C                   MOVE      '£1'          D06COD
032100050214     c                   movel     'L'           d06tla
032200050214     C                   MOVEL     v1ctpn        D06KEY
032300050214     C                   MOVEL     TRUL06DS      KPJBU
032400050214     C                   CALL      'TRUL06R'
032500050214     C                   PARM                    KPJBA
032600050214     C                   MOVEL     KPJBU         TRUL06DS
032700050214     C                   MOVEA     LIN           L1new
032800050214     c
032900050214     c* delle 2 schiere elaboro soltanto i p.o. in comune
033000050214     c                   z-add     1             xx                3 0
033100050214     c                   z-add     0             dd                3 0
033200050214     c                   movea     *zeros        skipo
033300050214     c
033400050214     c                   dow       L1old(XX)>0
033500050214     c     L1old(xx)     lookup    L1new                                  31
033600050214     c                   if        *in31
033700050214     c                   add       1             dd
033800050214     c                   movel     L1old(xx)     skipo(dd)
033900050214     c                   endif
034000050214     c                   add       1             xx
034100050214     c                   enddo
034200050214     c
034300050214     C                   ENDSR
034400020805      **************************************************************************
034500960302** SCHIERA MSG - MESSAGGI DI ERRORE
034600041220Punto Operativo INESISTENTE                                                   01
034700050103Il terminal di Partenza NUOVO non può essere uguale a quello VECCHIO          02
034800050103Esistono ancora dei fogli viaggio partenza aperti: controllare!!!!            03
034900050103Non sono stati elaborati i saldi statistica del p.o. che diventa 2 livello    04
035000050103data errata                                                                   05
