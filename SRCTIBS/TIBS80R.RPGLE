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
003300120202
003400120202     d wForza          s               n   inz(*off)
003500120202     d oldV1Cpo        s                   like(V1Cpo)
003600120202
003700050103      * Ds date statistiche
003800050103     d dSdf          e ds
003900041220     d
004000041220     D C1livello       C                   CONST('diventerà un 1 livello in par-
004100041220     D                                     tenza:')
004200050103     D C2livello       C                   CONST('diventeranno 2 livello del nu-
004300050103     D                                     ovo')
004400050103     D C2livello2      C                   CONST('terminal indicato:')
004500050103     D Cpoperativo     C                   CONST('Il  punto  operativo')
004600050103     D Cterminal       C                   CONST('Tutti i P.O. del terminal -
004700050103     D                                     partenza')
004800040917     C**********************************************************************
004900050103     c     Ktbe          Klist
005000050103     c                   Kfld                    kTbeCod
005100050103     c                   Kfld                    kTbeKe1
005200050103     C**********************************************************************
005300040917     C     *ENTRY        PLIST
005400040917     C                   PARM                    KPJBA
005500050103      * Calcolo oggi - 3 gg
005600050103     c                   Move      *date         Dataiso
005700050103     c                   subdur    3:*d          Dataiso
005800050103     c                   Move      Dataiso       woggim3           8 0
005900050103     c*
006000040917     C                   Z-ADD     1             CODUT
006100040917     C                   CALL      'X§PARUT'
006200040917     C                   PARM                    UT§DSE0F
006300040917     C                   MOVEL     REC80         CNCR80
006400040917     C                   MOVEL     RAGUT         VIDRSU
006500041220     C                   CLEAR                   V1CPO
006600041220     C                   CLEAR                   V1DPO
006700041220     C                   CLEAR                   V1CTPN
006800041220     C                   CLEAR                   V1CTPO
006900041220     C                   CLEAR                   V1DTPO
007000041220     C                   CLEAR                   V1DTPN
007100050103     C                   CLEAR                   V1cco1
007200050103     C                   CLEAR                   V1cco2
007300050103     C                   CLEAR                   V1cco3
007400050103     C                   CLEAR                   V1cco4
007500050103     C                   CLEAR                   V1cco5
007600041220     C                   CLEAR                   newpo
007700050103     C                   CLEAR                   v1csta
007800050103     c                   setoff                                       010506
007900050103     c                   setoff                                       02
008000120202     c                   clear                   V1Cpo
008100040917     c
008200040917     C     FOR01         TAG
008300000828      *
008400041220     C                   EXFMT     BS80D01
008500970102     C                   SETOFF                                       289050
008600040917     C                   SETOFF                                       51
008700040917     C* F3 - FINE
008800040917     C   KC              GOTO      FINE
008900050104     C* F7 - manutenzione organigramma
009000050104     c                   if        *inkg
009100050110     C                   call      'TIBS84C'
009200050104     c                   parm                    kpjba
009300050104     c                   goto      for01
009400050104     c                   endif
009500041220      *  CONTROLLI
009600040917     C                   EXSR      CTRD01
009700040917     c   90              goto      for01
009800000825      *
009900040917     c*
010000050103     c                   if        (not *inkf and not *in01 and not *in02) or
010100050103     c                             (not *inkn and *in01) or
010200050103     c                             (not *inkn and *in02)
010300041220     c                   goto      for01
010400041220     c                   endif
010500040917     C*
010600040917      * F6  - SOTTOMETTO ELABORAZIONE
010700050412    1C     *INKF         IFEQ      *ON
010800041220     C     *INKN         oreq      *ON
010900050103     c
011000050412     c* P.O. 1°livello che diventa 2°livello
011100050412    2c                   if        *in02
011200050214     c* carico in skiera tutti i p.o. 2 livello del terminal che diventa
011300050214     c*  2 liv , che diventano 2 livello del new terminal
011400050214     c* (qualche p.o. infatti potrebber anche cambiare termnal di part
011500050214     c*  in sto caso deve avere una elaborazione a parte, precedente
011600050214     c* questa)
011700050214     c
011800050214     c                   exsr      CARPO
011900050214     c*
012000050214     c                   call      'TIBS81C'
012100041220     c                   parm                    v1cpo
012200041220     c                   parm                    v1ctpo
012300041220     c                   parm                    v1ctpn
012400050214     c                   parm                    skipo
012500050414     c                   parm                    tpoaex
012600050414     c                   parm                    tpnaex
012700050412    2C                   ENDIF
012800050412     c
012900050412     c*  P.O. 2°livello che rimane 2°livello, cambiando terminal partenza
013000050412    2c                   if        not *in01 and not *in02
013100050412     c                   call      'TIBS86C'
013200050412     c                   parm                    v1cpo
013300050412     c                   parm                    v1ctpo
013400050412     c                   parm                    v1ctpn
013500050412     c                   parm                    skipo
013600050414     c                   parm                    tpoaex
013700050414     c                   parm                    tpnaex
013800050412    2C                   ENDIF
013900061205     c
014000061205     c*  P.O. 2°livello che diventa 1°livello                          za
014100061205    2c                   if        *in01
014200061205     c                   call      'TIBS87C'
014300061205     c                   parm                    v1cpo
014400061205     c                   parm                    v1ctpo
014500061205     c                   parm                    v1ctpn
014600061205     c                   parm                    skipo
014700061205     c                   parm                    tpoaex
014800061205     c                   parm                    tpnaex
014900061205    2C                   ENDIF
015000050412     c
015100050412    1C                   ENDIF
015200900515      *
015300000000     C     FINE          TAG
015400960129     C*
015500040917     c                   clear                   fnlv55ds
015600040917     C                   MOVEL     'C'           D55TLA
015700040917     C                   CALL      'FNLV55R'
015800040917     C                   PARM                    FNLV55DS
015900000000     C                   SETON                                        LR
016000900515      *-----------------------------------------------------***********
016100040917      * CONTROLLI SFL                                       *  CTRD01 *
016200900515      *-----------------------------------------------------***********
016300040917     C     CTRD01        BEGSR
016400050104     c                   setoff                                       010206
016500041220      * PUNTO OPERATIVO
016600041220     C                   MOVE      V1CPO         CODFIL            3 0
016700951025     C                   EXSR      CTRFIL
016800041220    1c                   if        not *in34
016900041220     c                   movel     orgdes        V1DPO
017000040917     c
017100041220   x1C                   ELSE
017200040917     c                   seton                                        509028
017300041220     C                   MOVEL     MSG(1)        V1cMSG
017400041220     c                   goto      endct1
017500041220    1C                   ENDIF
017600120202
017700120202      * spengo il flag di forzatura se cambiato il punto operativo da variare
017800120202     c                   IF        oldV1Cpo <> V1Cpo
017900120202     c                   eval      wForza = *off
018000120202     c                   eval      oldV1Cpo = V1Cpo
018100120202     c                   ENDIF
018200041220     c
018300050103     c* Cerco il terminal di partenza  vecchio a udate-3
018400041220     C                   CLEAR                   FNLV55DS
018500041220     C                   MOVEL     'P'           D55TPT
018600041220     C                   MOVE      V1cpo         D55LIN
018700050103     C                   MOVE      woggim3       d55drf
018800041220     C                   CALL      'FNLV55R'
018900041220     C                   PARM                    FNLV55DS
019000041220     c*
019100041220     c                   eval      v1ctpo=d55tfp
019200041220     c* decodifica
019300041220     C                   MOVE      V1Ctpo        CODFIL            3 0
019400041220     C                   EXSR      CTRFIL
019500041220    1c                   if        not *in34
019600041220     c                   movel     orgdes        V1Dtpo
019700050414     C* MEMORIZZO AREA EX AS400 PER QINTER
019800050414     C                   MOVEL     ORGDE0        OG140
019900050414     C                   MOVEL     §OGAEX        TPOAEX
020000041220     c                   else
020100041220     c                   clear                   v1dtpo
020200050414     c                   clear                   TPOAEX
020300041220    1c                   endif
020400050103     c
020500050103     c* Cerco il nuovo terminal di partenza a udate
020600050103     C                   CLEAR                   FNLV55DS
020700050103     C                   MOVEL     'P'           D55TPT
020800050103     C                   MOVE      V1cpo         D55LIN
020900050103     C                   MOVE      *date         d55drf
021000050103     C                   CALL      'FNLV55R'
021100050103     C                   PARM                    FNLV55DS
021200050103     c*
021300050103     c                   eval      v1ctpn=d55tfp
021400050103     c* decodifica
021500050103     C                   MOVE      V1Ctpn        CODFIL            3 0
021600050103     C                   EXSR      CTRFIL
021700050103    1c                   if        not *in34
021800050103     c                   movel     orgdes        V1Dtpn
021900050414     C* MEMORIZZO AREA EX AS400 PER QINTER
022000050414     C                   MOVEL     ORGDE0        OG140
022100050414     C                   MOVEL     §OGAEX        TPnAEX
022200050103     c                   else
022300050103     c                   clear                   v1dtpn
022400050414     c                   clear                   tpnaex
022500050103    1c                   endif
022600050103    c
022700050103     c* Se terminal vecchio e nuovo rimangono uguali, errore
022800050103     c                   if        v1ctpo=v1ctpn
022900120202     c                             and not wForza
023000050103     c                   clear                   v1csta
023100050103     c                   seton                                        519028
023200050103     C                   MOVEL     MSG(2)        V1cMSG
023300120202     c                   eval      wForza = *on
023400050103     c                   goto      endct1
023500050103    1C                   ENDIF
023600050103     c* Per indicatore 05 on - controllo la data ultima elaborazione statist
023700050103    1c                   if        v1csta>0
023800050103     c     *eur          test(d)                 v1csta                 30
023900050103    2c                   if        *in30
024000050103     c     *dmy          test(d)                 v1csta                 30
024100050103    3c                   if        *in30
024200050103     c                   seton                                        529028
024300050103     C                   MOVEL     MSG(5)        V1cMSG
024400050103     c                   goto      endct1
024500050103     c                   else
024600050103     c* la trasforno in eur
024700050103     c     *dmy          move      v1csta        dataeur
024800050103     c                   move      dataeur       v1csta
024900050103    3c                   endif
025000050103    2c                   endif
025100050103    1c                   endif
025200041220     c
025300041220     c
025400050103     c* P.O. diventa 2 livello in partenza
025500050103     c                   eval      newpo=v1cpo
025600050103    1c                   if        v1ctpo=v1cpo
025700050103     c                   eval      v1cco3=cterminal
025800050103     c                   eval      v1cco4=c2livello
025900050103     c                   eval      v1cco5=c2livello2
026000050103     c                   seton                                        05
026100050103     c
026200050103     c* controllo elaborazioni giornaliere alla data indicata a video
026300050103     c*  se data =0 propongo udate -3
026400050103    2c                   if        v1csta=0
026500050103     c     *iso          move      woggim3       dataeur
026600050103     c                   move      dataeur       v1csta
026700050103    2c                   endif
026800050103     c* Giro la data in *iso
026900050103     c     *eur          move      v1csta        dataiso
027000050103     c                   move      dataiso       wamgsta           8 0
027100050103     c
027200050103     c                   Eval      kTbeCod = 'SDF'
027300050103     c                   Movel(p)  v1cpo         kTbeKe1
027400050103     c     Ktbe          Chain     Tntbe01l
027500050103    2c                   If        %Found(Tntbe01l)
027600050103     c                   Movel     TbeUni        dSdf
027700050103    3c                   If        §SdfParU < wamgsta  or
027800050103     c                             §sdfteru < wamgsta  or
027900050103     c                             §sdfbolu < wamgsta
028000050103     c                   seton                                        529028
028100050103     C                   MOVEL     MSG(4)        V1cMSG
028200050103     c                   goto      endct1
028300050103     c                   else
028400050103     c                   seton                                        06
028500050103    3c                   endif
028600050103    2c                   endif
028700050103     c*
028800050103     c* controllo che non ci siano fgv aperti e non annullati
028900050103     c     v1ctpo        setll     fnfgv06l                               30
029000050103    2c                   if        *in30
029100050103     c                   seton                                        509028
029200050103     C                   MOVEL     MSG(3)        V1cMSG
029300050103     c                   goto      endct1
029400050103    2c                   endif
029500050103     c
029600050103     c                   seton                                        02
029700050103   x1c                   else
029800050103     c                   setoff                                       0501
029900050103     c                   clear                   v1csta
030000050103    1c                   endif
030100050103     c**
030200050103     c* p.o. diventa 1 livello in partenza
030300041220     c                   if        v1ctpn=v1cpo
030400050103     c                   eval      v1cco1=cpoperativo
030500050103     c                   eval      v1cco2=c1livello
030600041220     c                   seton                                        01
030700041220     c                   endif
030800041220     c*
030900900515      *
031000040917     C     ENDCT1        ENDSR
031100951025     C**************************************************************************
031200951025     C*  CONTROLLO UNA FILIALE IN ORGANIGRAMMA
031300951025     C**************************************************************************
031400951025     C     CTRFIL        BEGSR
031500020213
031600951025     C     CODFIL        CHAIN     AZORG01L                           34
031700951025    1C  N34ORGFVA        IFNE      ' '
031800951025     C     ORGFAG        ORNE      'A'
031900951025     C     ORGFAG        ANDNE     'F'
032000951025     C                   SETON                                        34
032100951025    1C                   ENDIF
032200951025     C*
032300951025     C                   ENDSR
032400050214     C**************************************************************************
032500050214     C*  carico p.o. 2 livello che vengono spostati insieme al terminal
032600050214     C**************************************************************************
032700050214     C     CARPO         BEGSR
032800050214     c                   clear                   L1old
032900050214     c                   clear                   L1new
033000050214     C* Carico tabella Filiali Gestite £1
033100050214     C                   CLEAR                   TRUL06DS
033200050214     C                   MOVE      '£1'          D06COD
033300050214     C                   MOVEL     v1cpo         D06KEY
033400050214     C                   MOVE      'S'           D06KEY
033500050214     C                   MOVEL     woggim3       D06drf
033600050214     C                   MOVEL     TRUL06DS      KPJBU
033700050214     C                   CALL      'TRUL06R'
033800050214     C                   PARM                    KPJBA
033900050214     C                   MOVEL     KPJBU         TRUL06DS
034000050214     C                   MOVEA     LIN           L1old
034100050214     c
034200050214     C* Carico tabella Filiali Gestite £1
034300050214     C                   CLEAR                   TRUL06DS
034400050214     C                   MOVE      '£1'          D06COD
034500050214     c                   movel     'L'           d06tla
034600050214     C                   MOVEL     v1ctpn        D06KEY
034700050214     C                   MOVEL     TRUL06DS      KPJBU
034800050214     C                   CALL      'TRUL06R'
034900050214     C                   PARM                    KPJBA
035000050214     C                   MOVEL     KPJBU         TRUL06DS
035100050214     C                   MOVEA     LIN           L1new
035200050214     c
035300050214     c* delle 2 schiere elaboro soltanto i p.o. in comune
035400050214     c                   z-add     1             xx                3 0
035500050214     c                   z-add     0             dd                3 0
035600050214     c                   movea     *zeros        skipo
035700050214     c
035800050214     c                   dow       L1old(XX)>0
035900050214     c     L1old(xx)     lookup    L1new                                  31
036000050214     c                   if        *in31
036100050214     c                   add       1             dd
036200050214     c                   movel     L1old(xx)     skipo(dd)
036300050214     c                   endif
036400050214     c                   add       1             xx
036500050214     c                   enddo
036600050214     c
036700050214     C                   ENDSR
036800020805      **************************************************************************
036900960302** SCHIERA MSG - MESSAGGI DI ERRORE
037000041220Punto Operativo INESISTENTE                                                   01
037100120202Il NUOVO Terminal di Partenza è uguale a quello VECCHIO. ENTER per FORZARE!!! 02
037200050103Esistono ancora dei fogli viaggio partenza aperti: controllare!!!!            03
037300050103Non sono stati elaborati i saldi statistica del p.o. che diventa 2 livello    04
037400050103data errata                                                                   05
