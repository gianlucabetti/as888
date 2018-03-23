000100031023     H DECEDIT('0,') DATEDIT(*ymd.)
000200000324     H* FNLRS1R *----------------------------------------------------*
000300000524     H*         - STAMPA L.V. IN ARRIVO NO DDT
000400960404     H*--------------------------------------------------------------*
000500000324     FTNTBE01L  UF A E           K DISK
000600960329     FAZORG01L  IF   E           K DISK
000700000324     FTABEL00F  IF   E           K DISK
000800000328     Ffnfvv01l  IF   E           K DISK
000900000328     Ffnfva02l  IF   E           K DISK
001000040108     FAZCAE05L  IF   E           K DISK
001100030611     FFNLRS1D   CF   E             WORKSTN   SFILE(LRs1dsf:NRR1)
001200030611
001300040109     D MSG             S             78    DIM(14) CTDATA PERRCD(1)             MSG VIDEO
001400040301     D L1              S              3  0 DIM(30)                              FIL.GESTITE
001500040112     D LNAP            S              3  0 DIM(30)                              FIL.GESTITE
001600031031     D nog             S              3  0 DIM(30)                              FIL.GESTITE
001700000324     D slna            S              3  0 DIM(30)                              FIL.GESTITE
001800000329     D sord            S              3  0 DIM(30)                              FIL.GESTITE
001900000329     D sords           S              3  0 DIM(30)                              FIL.GESTITE
002000000329     D snrr            S              2  0 DIM(30)                              FIL.GESTITE
002100000330     D ttr             S              1    DIM(10)                              FIL.GESTITE
002200000329     D CMD1            S              1    DIM(37) CTDATA PERRCD(37)            X QCMDEXC
002300031029     d L6              s              3  0 Dim(30)                              p.o. gest. da fgs
002400031031     d L6lna           s              3  0 Dim(30)                              p.o. gest. da lna
002500031031     d pck             s              3  0 Dim(30)                              p.o. gest. con pick.
002600031023
002700031103     d kKe2            s                   Like(TbeKe2)
002800031028     d provatfp        s              1    Inz('0')
002900031028     d provatfa        s              1    Inz('0')
003000040113     d provafgs        s              1
003100031031     d wpo             s                   Like(V1cFgs)
003200031103     d wpotntbe        s                   Like(V1cFgs)
003300040112     d*wtfa            s                   Like(d55tfa)
003400040109     d kepa            s                   Like(caeepa)
003500040109     d wtfe            s                   Like(caetfe)
003600040113     d savnpg          s                   Like(v1cnpg)
003700040113     d savpfa          s                   Like(v1cpfa)
003800031031     d xx              s              2  0
003900031031     d zz              s              2  0
004000040301     d*noerr           s              1
004100030611
004200000329     D FNLRS1DS      E DS                  inz
004300000329     D FNLv55ds      E DS                  inz
004400040109     D FNLv54ds      E DS                  inz
004500000327     D dslr01        E DS                  inz
004600960326     D KPJBA         E DS
004700960329     D CNCR80        E DS
004800030611     D ut§dse0f      E DS
004900030611     D trul06ds      E DS
005000030611     D  LIN                    1     90  0 DIM(30)                              FIL. COMODO
005100040109     d trul09ds      e ds
005200040109     d  TAR                   19    108  0 dim(30)
005300031028     D dlvd          E DS
005400000327     D ds7n          E DS
005500030611     D ds5pB         E DS
005600000330     D dstv          E DS
005700031028     d og150         e ds
005800031031     d og143         e ds
005900030611
006000030611      ****************************************************************
006100030611      *  RIEPILOGO INDICATORI FILE VIDEO
006200030611      ***************************************************************
006300030611      * 01 - Cambio p.o. gestione
006400030611      * 02 - Video - gestione flag ristampa
006500031028      * 03 - Linea di arrivo con Picking attivo
006600031028      * 04 - Visualizzo testata x nuovi flag
006700030611      * 05 - Cambio p.o. gestione
006800031031      * 06 - On Primo livello  Off secondo livello
006900031105      * 07 - Proteggo ordinamento e I/E
007000030611      * 20 - Video - Gestione SBF + condizionamento tasti funzionali
007100030611      * 21 - Video - Gestione SBF
007200030611      * 23 - Video - Gestione SBF
007300030611      * 28 - Video - MSG Errore
007400030611      * 40 - cambio p.o. gestione
007500030611      * 41 - Video - Zona da
007600030611      * 45 - Video - tipo foglio
007700030611      * 46 - Video - numero foglio
007800030611      * 47 - Video - ordinamento
007900030611      * 48 - Video - stampante A4
008000030611      * 49 - Video - stampante A5
008100030611      ***************************************************************
008200030611      * GESTIONE PIMA VIDEATA - RICHIESTA FOGLIO ARRIVI
008300030611      * Impostazione campi
008400000327     c                   exsr      ried01
008500000327     c     emd01         tag
008600000531     c                   setoff                                       02
008700000330     c                   write     lrs1z01
008800000327     c                   exfmt     lrs1d01
008900000327     c                   clear                   v1zmsg
009000000327     c                   setoff                                       902840
009100000329     c     *inkc         cabeq     '1'           fine
009200000327     c* f18=cambio p.o in gestione
009300000327     c   01*inks         ifeq      *on
009400000327     c                   seton                                        05
009500000327     c                   goto      emd01
009600000327     c                   endif
009700000327     c*
009800000327     C   05              DO
009900031031     C**!!!              MOVEL     v1cfgs        ALF3              3
010000031031     C**!!!ALF3          lookup    NOG                                    30
010100031031     C**!30              SETON                                        4028
010200031031     c**!30              movel     msg(1)        v1zmsg
010300031031     C**!30              GOTO      emd01
010400031031      * Controllo P.o. gestione
010500040301     c*                  eval      noerr = ' '
010600031031     c                   Exsr      Sr_CtrFgs
010700031031     c                   If        *In28
010800031031     c                   GoTo      Emd01
010900031031     c                   EndIf
011000040113     c* reimposto dati del foglio
011100040113     c                   exsr      Sr_ImpFarr
011200040113     c                   setoff                                       05
011300040226     c                   GoTo      Emd01
011400000327     c                   end
011500000327     c*
011600000327     c                   exsr      ctrd01
011700000329     c   90              goto      emd01
011800000531     c                   seton                                        02
011900000324
012000000327     c* GESTIONE SUBFILE - RICHIESTA LNA + OUTQ + ORDINE DI STAMPA
012100000324     c* Caricamento subfile
012200000324     c                   exsr      riedsf
012300000329     c* se non ho caricato nessuna linea di arrivo spengo indicatore SFLDSP
012400000330     c     nrr1          comp      0                                  2020
012500000324     c*
012600000327     c* EMISSIONE SUBFILE
012700000330     c                   write     lrs1z02
012800000324     c                   write     lrs1d01
012900000324
013000000324     c     emetti        tag
013100000324
013200000324     c                   exfmt     lrs1dct
013300040112     c                   setoff                                       280590
013400000324     c                   clear                   v1cmsg
013500000324     c* f3=fine
013600000324     c     *inkc         cabeq     *on           fine
013700000329     c* f12=Ritorno
013800000329     c     *inkl         cabeq     *on           emd01
013900000614     c  n20              goto      emetti
014000000324     c* CONTROLLO DATI del subfile
014100000324     c                   exsr      ctrdsf
014200000330     c
014300000330     c     *in90         ifeq      *on
014400000330     c     *inkf         oreq      *off
014500000330     c     *inke         andeq     *off
014600000330     c                   goto      emetti
014700000330     c                   endif
014800000330     c     *inke         ifeq      *on
014900000330     c                   exsr      aggior
015000000330     c                   exsr      riedsf
015100000330     c                   goto      emetti
015200000330     c                   endif
015300000324     c*
015400000324     c* CONFERMA: Aggiorno tabella lvd e sottometto stampa l.v.
015500000324     c                   exsr      confe
015600960327     C*
015700960326     C     FINE          TAG
015800960521     C*
015900971118     C                   MOVEL     'C'           D55TLA
016000971118     C                   CALL      'FNLV55R'
016100040112     C                   PARM                    fnlv55ds
016200040112     c*
016300040112     C                   MOVEL     'C'           I54TLA
016400040112     C                   CALL      'FNLV54R'
016500040112     C                   PARM                    fnlv54ds
016600960521     C                   SETON                                        LR
016700960521     C*
016800000327     C**************  Riempimento campi prima videata *****************
016900000327     C     RIED01        BEGSR
017000000324     c* P.O. in gestione
017100020424     c                   z-add     simpou        v1cfgs
017200040301     c*                  eval      noerr = 'N'
017300031031     c                   ExSr      Sr_CtrFgs
017400031031
017500031031     c     simtpp        ifeq      '2'
017600031031     c     simtpp        oreq      *blanks
017700031031     C                   eval      *in01 = *off
017800031031     c                   else
017900031031     C                   eval      *in01 = *on
018000031031     c                   endif
018100000324     c* flag ristampa
018200000324     c                   movel     'N'           v1cris
018300000324     c                   clear                   v1cznd
018400000524     c                   move      *hival        v1czna
018500000531     c                   setoff                                       02
018600040108     c* FOGLIO ARRIVI
018700040108     c                   exsr      Sr_ImpFarr
018800031023
018900000324     c                   ENDSR
019000031031
019100031031      *------------------------------------------------------------------------*
019200031031      *    Controllo il p.o. gestione
019300031031      *------------------------------------------------------------------------*
019400031031     c     Sr_CtrFgs     BegSr
019500031031
019600031031      * Il p.o. gestione non può essere un p.o. gestito
019700031031     c     V1cFgs        Lookup    Nog                                    30
019800031031     c                   If        *In30
019900031031     c                   Eval      *In40 = *On
020000031031     c                   Eval      *In28 = *On
020100031031     c                   Eval      V1zMsg = Msg(1)
020200031031     c                   GoTo      EndCtrFgs
020300031031     c                   EndIf
020400031031
020500040301      * Il p.o. gestione deve far parte della £1
020600040301     c***  noerr         ifeq      ' '
020700040301     c***  v1cfgs        andne     simpou
020800040301     c     V1cFgs        Lookup    L1                                     30
020900031031     c                   If        Not *In30
021000031031     c                   Eval      *In40 = *On
021100031031     c                   Eval      *In28 = *On
021200031031     c                   Eval      V1zMsg = Msg(1)
021300031031     c                   GoTo      EndCtrFgs
021400031031     c                   EndIf
021500040301     c***                endif
021600031031
021700031031      * Carico la £6 del P.o. gestione
021800031031     c                   Eval      wpo = V1cFgs
021900031031     c                   ExSr      Sr_Car£6
022000031031     c                   Movea     Lin           L6
022100031031
022200031031      * Controllo il p.o. gestione con simfel
022300031031     c                   If        Simfel = V1cFgs
022400031031      * Sono primo livello
022500031031     c                   Eval      *In06 = *On
022600031031      * Sono secondo livello
022700031031     c                   Else
022800031031     c                   Eval      *In06 = *Off
022900031031     c                   EndIf
023000031031
023100031031      * cerco il terminal di arrivo se sono secondo livello
023200040112     c***                If        Not *In06
023300040112     c***                clear                   fnlv55ds
023400040112     c***                z-add     v1cfgs        d55lin
023500040112     c***                movel     'A'           d55tpt
023600040112     c***                z-add     *date         d55drf
023700040112     c***                call      'FNLV55R'
023800040112     c***                parm                    fnlv55ds
023900040112     c***                If        d55err = *Blanks
024000040112     c***                Eval      wtfa = d55tfa
024100040112     c***                Else
024200040112     c***                Eval      wtfa = V1cfgs
024300040112     c***                EndIf
024400040112     c***                EndIf
024500031103
024600031103      * Pulisco sk di appoggio x p.o. con picking
024700031103     c                   Clear                   Pck
024800031031
024900031031     c     EndCtrFgs     EndSr
025000031031
025100040108      *------------------------------------------------------------------------*
025200040108      *    Impostazione foglio arrivi (categoria/p.o./numero)
025300040108      *------------------------------------------------------------------------*
025400040108     c     Sr_ImpFarr    BegSr
025500040108     c* p.o. foglio
025600040108     c* propongo il p.o. in gestione se è anche ter-arr altrimenti propongo
025700040109     c* il suo ter-arr se su stesso ogas altrimenti propongo sè stesso
025800040109     c                   z-add     v1cfgs        wtfe
025900040113     c                   eval      provafgs=' '
026000040109     c                   exsr      Sr_TerArr
026100040108     c     wtarr         ifeq      '1'
026200040108     c                   z-add     v1cfgs        v1cpfa
026300040108     c* categoria
026400040301     c  n06              exsr      sr_npg
026500040108     c   06              movel     '2'           v1cnpg
026600040108     c                   else
026700040108     c                   clear                   fnlv54ds
026800040108     c                   z-add     v1cfgs        i54lin
026900040108     c                   movel     'A'           i54tpt
027000040108     c                   z-add     *date         i54drf
027100040108     c                   call      'FNLV54R'
027200040108     c                   parm                    fnlv54ds
027300040108     c     o54aslin      ifeq      o54astfa
027400040108     c                   z-add     o54tfa        v1cpfa
027500040108     c                   movel     '2'           v1cnpg
027600040108     c                   else
027700040108     c                   z-add     v1cfgs        v1cpfa
027800040301     c****               movel     '6'           v1cnpg
027900040301     c                   exsr      sr_npg
028000040108     c                   endif
028100040108     c                   endif
028200040108     c* categoria
028300040108     c**n06              movel     '6'           v1cnpg
028400040108     c** 06              movel     '2'           v1cnpg
028500040108     c* numero
028600040113     c     v1cnpg        ifne      savnpg
028700040113     c     v1cpfa        orne      savpfa
028800040108     c                   move      *blanks       v1cnfa
028900040113     c                   endif
029000040113     c                   move      v1cnpg        savnpg
029100040113     c                   z-add     v1cpfa        savpfa
029200040108     c                   endsr
029300040109      *------------------------------------------------------------------------*
029400040109      *    Verifica se p.o. è ter-arr
029500040109      *------------------------------------------------------------------------*
029600040109     c     Sr_TerArr     BegSr
029700040109
029800040109     C                   MOVEL     'A'           KEPA
029900040109     C                   MOVE      *BLANKS       WTARR             1
030000040113     c                   if        provafgs=' '
030100040109     C     KCAE5         SETLL     AZCAE05L
030200040109     C     KCAE5         READE     AZCAE05L                               30
030300040113     c                   else
030400040113     C     KCAE5fgs      SETLL     AZCAE05L
030500040113     C     KCAE5fgs      READE     AZCAE05L                               30
030600040113     c                   endif
030700040109     C     *IN30         DOWEQ     *OFF
030800040109     C     WTARR         ANDEQ     *BLANKS
030900040109     C     CAEATB        IFEQ      *BLANKS
031000040109     C     *date         ANDGE     CAEDDE
031100040109     C     *date         ANDLE     CAEDSC
031200040109     C                   MOVEL     '1'           WTARR
031300040109     C                   END
031400040113     c                   if        provafgs=' '
031500040109     C     KCAE5         READE     AZCAE05L                               30
031600040113     c                   else
031700040113     C     KCAE5fgs      READE     AZCAE05L                               30
031800040113     c                   endif
031900040109     C                   ENDDO
032000040109
032100040109     c                   endsr
032200031031      *------------------------------------------------------------------------*
032300031031      *    Carico la £6
032400031031      *------------------------------------------------------------------------*
032500031031     c     Sr_Car£6      BegSr
032600031031
032700031031     c                   Clear                   Trul06ds
032800031031     c                   Eval      D06Cod = '£6'
032900031031     c                   Movel     wpo           D06Key
033000031031     c                   Movel(p)  Trul06ds      Kpjbu
033100031031     c                   Call      'TRUL06R'
033200031031     c                   Parm                    Kpjba
033300031031     c                   Movel     Kpjbu         Trul06ds
033400031031
033500031031     c                   EndSr
033600031031
033700000327     C**************  Controlli prima videata   ***********************
033800000327     C     CTRd01        BEGSR
033900000328     c                   setoff                                       414546
034000040109     c                   setoff                                       42
034100000327     c* categoria fogli
034200000328    1c     v1cnpg        ifeq      ' '
034300000327     c                   seton                                        459028
034400000327     c                   movel     msg(2)        v1zmsg
034500000327     c                   goto      endct1
034600000328    1c                   endif
034700000328    1c     v1cnpg        ifeq      '?'
034800000327     c                   movel     codut         §kut
034900000327     c                   movel     '7N'          §cod
035000000327     c                   move      ' '           v1cnpg
035100000327     c                   clear                   §key
035200000327     c                   call      'X§TABER'
035300000327     c                   parm                    §kut              1 0
035400000327     c                   parm                    §cod              2
035500000327     c                   parm                    §key              8
035600000327     c                   parm                    §des             30
035700000327     c                   movel     §key          v1cnpg
035800000327     c                   seton                                        90
035900000327     c                   goto      endct1
036000000328    1c                   end
036100000327     c                   movel     '7N'          cod
036200000327     c                   movel(P)  v1cnpg        key
036300000327     c     ktab          chain     tabel00f                           45
036400000328    1c  n45tblflg        ifne      ' '
036500000327     c                   seton                                        45
036600000328   x1c                   else
036700000328    2c     v1cnpg        ifne      '2'
036800000327     c     v1cnpg        andne     '6'
036900000327     c                   seton                                        45
037000000328    2c                   endif
037100000328    1c                   endif
037200000327     c   45              seton                                        9028
037300000327     c   45              movel     msg(2)        v1zmsg
037400000327     c   45              goto      endct1
037500000327     c
037600000327     c                   movel     tbluni        ds7n
037700031031     C* non utilizzabile
037800000328    1c     §7nuti        ifeq      '9'
037900000327     c                   seton                                        452890
038000000327     c                   movel     msg(3)        v1zmsg
038100000327     c                   goto      endct1
038200000328   x1c                   else
038300000327     c* utilizzabile solo dalla primo livello
038400031023    2c**!!!§7nuti        ifeq      '1'
038500031023     c**!!!v1cfgs        andne     simfel
038600031023     c**!!!              seton                                        452890
038700031023     c**!!!              movel     msg(3)        v1zmsg
038800031023     c**!!!              goto      endct1
038900031023    2c**!!!              endif
039000031031      * utilizzabile solo dalla secondo livello
039100031031    2c     §7nuti        ifeq      '2'
039200031031     c     v1cfgs        andeq     simfel
039300031031     c                   seton                                        452890
039400031031     c                   movel     msg(3)        v1zmsg
039500031031     c                   goto      endct1
039600031031    2c                   endif
039700000328    1c                   endif
039800000327     C                   MOVEL     v1cnpg        ds1npg
039900040109     C***                MOVEL     v1cfgs        ds1fgs
040000031031      * se sono un secondo livello ed ho chiesto un foglio di tipo 2 cerco con simfel
040100040109     c**                 If        Not *In06 and ds1npg = 2
040200040109     c**                 Move      wtfa          ds1fgs
040300040109     c**                 EndIf
040400040109     c* p.o. foglio
040500040109     c* il p.o. foglio può essere o il ter-arr di fgs o fgs stesso
040600040109     c     v1cpfa        ifeq      *zeros
040700040109     c                   seton                                        429028
040800040109     c                   movel     msg(14)       v1zmsg
040900040109     c                   goto      endct1
041000040109     c                   endif
041100040109     c     v1cpfa        ifne      v1cfgs
041200040109     c* verifico se il p.o. foglio immesso è ter-arr di fgs
041300040109     c                   z-add     v1cpfa        wtfe
041400040113     c                   eval      provafgs='S'
041500040109     c                   exsr      Sr_TerArr
041600040109     c     wtarr         ifeq      *blanks
041700040109     c                   seton                                        429028
041800040109     c                   movel     msg(14)       v1zmsg
041900040109     c                   goto      endct1
042000040109     c                   endif
042100040109     c                   endif
042200040112     C                   MOVEL     v1cpfa        ds1fgs
042300040109     c
042400000327     c* numero foglio arrivi
042500000328    1c     v1cnfa        ifeq      *blanks
042600000327     c     v1cnfa        oreq      *zeros
042700000327     c                   seton                                        462890
042800000327     c                   movel     msg(4)        v1zmsg
042900000327     c                   goto      endct1
043000000328   x1c                   else
043100000327     c     '?'           scan      v1cnfa                                 30
043200000328    2c   30              do
043300000327     c                   clear                   dslr01
043400000327     c                   movel     'S'           dlrric
043500000327     c                   movel     ds1npg        dlrnpg
043600000327     c                   movel     '2'           dlrtfv
043700000327     C                   MOVEL     'FNLRS1R'     DLRPGM
043800000327     c                   z-add     31129999      dlrgal
043900000327     c                   z-add     99991231      dlraal
044000031023     c**!!!              movel     v1cfgs        dlrfgs
044100040109     c                   movel     v1cpfa        dlrfgs
044200000327     c                   movel     dslr01        kpjbu
044300000327     c                   call      'FNLR02R'
044400000327     c                   parm                    kpjba
044500000327     c                   movel     kpjbu         dslr01
044600000327     c                   movel     dlrnfv        v1cnfa
044700000327     c                   seton                                        90
044800000327     c                   goto      endct1
044900000328    2c                   end
045000000327     c                   testn                   v1cnfa               31
045100000328    2c     *in31         ifeq      *on
045200000328     c                   movel     v1cnfa        w0050             5 0
045300000328     c     kfvv1         chain     fnfvv01l                           30
045400000328    2c                   endif
045500000328    2c     *in31         ifeq      *off
045600000328     c     *in30         oreq      *on
045700000328     c     fvvatb        orne      *blanks
045800000328     c                   seton                                        469028
045900000329     c                   movel     msg(4)        v1zmsg
046000000328     c                   goto      endct1
046100000328    2c                   endif
046200000328    1c                   endif
046300000329     c                   move      v1cnfa        ds1nfa
046400000426     c                   z-add     fvvdfv        ds1dfa
046500000328     c* solo per ristampa:
046600000328     c* zona consegna dal deve essere inferiore di zona consegna al
046700000328     c     v1cris        ifeq      'S'
046800000328     c     v1cznd        andgt     v1czna
046900000328     c                   seton                                        419028
047000000329     c                   movel     msg(5)        v1zmsg
047100000328     c                   goto      endct1
047200000328     c                   endif
047300000327     c     endct1        endsr
047400000324     C**************  Riempimento campi del subfile *******************
047500000324     C     RIEDSF        BEGSR
047600040301     c* Carico schiera dei p.o. di cui fgs è ter-arr su stesso ogas
047700040301     c                   clear                   trul09ds
047800040301     c                   eval      i09tla = 'L'
047900040301     c                   eval      i09mod = 'TFU'
048000040301     c                   eval      i09tfa = v1cfgs
048100040301     c                   Call      'TRUL09R'
048200040301     c                   parm                    trul09ds
048300030611      * PULIZIA  SUBFILE
048400000324     C                   eval         *IN21 = *OFF
048500000330     C                   eval         *IN23 = *OFF
048600000324     C                   WRITE     lrs1dct
048700000324     C                   eval         *IN20 = *ON
048800000324     c                   eval         *IN21 = *ON
048900031028
049000031028     c                   Reset                   provatfp
049100031028     c                   Reset                   provatfa
049200030611      *
049300000329     c                   z-add     1             v1cnbr
049400000329     c                   z-add     0             nrr1              5 0
049500000328     c                   clear                   v1sord
049600031028
049700031028     c                   Movel(p)  V1cfgs        kKe1
049800031028     c     CarLvd        Tag
049900030611      *
050000030611      * 1) - carico dati da tabella "LVD" se ce ne sono
050100000324     c                   z-add     0             x                 2 0
050200000324     c                   clear                   slna
050300031028     c**!!!              movel(P)  v1cfgs        kke1
050400000330     c     ktbe          setll     tntbe01l
050500000324     c     ktbe          reade     tntbe01l                               30
050600000324     c     *in30         doweq     *off
050700000328     c                   movel     tbeuni        dlvd
050800000329      * Se il S.I. è indicato deve essere uguale al mio
050900030611     c                   IF        (TBEsif = knsif or tbesif = *blanks)
051000000427     c                             and tbeatb = *blanks
051100000328     c* devo verificare se la lna memorizzata è ancora valida
051200000328     c* devo cioè verificare che la fgs sia ancora ter-arr di §lvdlna
051300000329     c                   z-add     §lvdlna       wlna
051400000328     c                   exsr      ctrtar
051500031028
051600000328     c     wlnaok        ifeq      'S'
051700030611      *
051800000324     c                   movel     tbeke2        w0020             2 0
051900000324     c     w0020         mult      10            v1sord
052000000324     c                   z-add     §lvdlna       v1slna
052100031031
052200031028     c                   Clear                   og150
052300000324     c     v1slna        chain     azorg01l                           30
052400000324     c  n30              movel     orgdes        v1dlna
052500031028     c  n30              Movel(p)  orgdf0        og150
052600000324     c   30              clear                   v1dlna
052700031031
052800031031      * controllo se la lna ha il picking attivo
052900031031     c                   ExSr      Sr_CtrPck
053000031031
053100031023     c**!!!              movel     §lvdotq       v1sprb
053200030611     c                   movel     §lvdot4       v1sprb4
053300030611     c                   movel     §lvdot5       v1sprb5
053400031023     c**!!!              movel     §lvdbsl       v1sbsl
053500030611      *  Aggancio 5P per: verificare se il p.o. è abilitato alla laser
053600030611      *                 : recuperare i pgm di stampa
053700030611      *                 : reperire tipo stampa e/o code se mancanti in LVD
053800030611     c                   movel     '5P'          cod
053900030611     c                   movel     'B'           w004a
054000030611     c                   move      v1cfgs        w004a
054100030611     c                   movel(P)  w004a         key
054200030611     c     ktab          chain     tabel00f                           30
054300030611     c  n30              movel     tbluni        ds5pb
054400030611     c   30              clear                   ds5pb
054500030611      *      Se stampa su laser non abilitata proteggo campi
054600031023     C**!!!              eval      *in50 = (§5PBTS = *blanks)
054700030611      *      Imposto pgm stampa
054800031023     c**!!!              movel     §5PBpg        V1Hbpg
054900030611     c                   movel     §5PBpl        V1Hbpl
055000030611      *      Imposto tipo stampa se non presente in LVD
055100031023     c**!!!              IF        V1SBSL = *blanks
055200031023     c*! 50              eval      V1SBSL = 'N'
055300031023     c*!N50              eval      V1SBSL = 'S'
055400031023     c**!!!              ENDIF
055500030611      *      Imposto code A4/A5 se non presente in LVD
055600030611     c                   IF        V1SPRB4 = *blanks
055700030611     c                   movel     §5pba4        v1sprb4
055800030611     c                   ENDIF
055900030611     c                   IF        V1SPRB5 = *blanks
056000030611     c                   movel     §5pba5        v1sprb5
056100030611     c                   ENDIF
056200030611      *
056300040301     c***                clear                   v1ssce
056400040301     c***  v1cris        ifeq      'N'
056500000606     c                   movel     'I'           v1ssce
056600040301     c***                endif
056700031028      * dft nuovi flag
056800031029     c   03              Eval      vs1ddt = 'S'
056900031029     c   03              Eval      vs1ass = 'N'
057000031104     c   03              Eval      v1hpck = 'S'
057100031029     c  n03              Clear                   vs1ddt
057200031029     c  n03              Clear                   vs1ass
057300031104     c  n03              Clear                   v1hpck
057400031105      * se sto caricando la tabella x terminal partenza o x terminal di arrivo devo
057500031105      * proteggere l'ordinamento e il flag di includi/escludi
057600031105     c                   If        provatfp = '1' or provatfa ='1'
057700031105     c                   Eval      *In07 = *On
057800031105     c                   Else
057900031105     c                   Eval      *In07 = *Off
058000031105     c                   EndIf
058100031028
058200030611      * memorizzo lna caricata
058300000324     c                   add       1             x
058400000324     c                   z-add     v1slna        slna(x)
058500000324     c                   add       1             nrr1
058600000324     c                   write     lrs1dsf
058700000328     c                   endif
058800000329     c                   endif
058900000324     c     ktbe          reade     tntbe01l                               30
059000000324     c                   enddo
059100040109      * se non ho caricato nessuna lna con il p.o. gestione
059200040109      * provo a cercare su LVD con il terminal di arrivo del p.o. gestione
059300040109     c                   If        x = *Zeros and provatfa = '0'
059400040109     c                   Movel(p)  v1cpfa        kKe1
059500040109     c                   Eval      provatfa = '1'
059600040109     c                   GoTo      CarLvd
059700040109     c                   EndIf
059800040109      * se non ho caricato nessuna lna nè con il p.o. gestione nè con il tfa
059900031028      * provo a cercare su LVD con il terminal di partenza del p.o. gestione
060000031030      * se sono un secondo livello
060100031028     c                   If        x = *Zeros and provatfp = '0'
060200031028     c                   Movel(p)  Simfel        kKe1
060300031028     c                   Eval      provatfp = '1'
060400031028     c                   GoTo      CarLvd
060500031028     c                   EndIf
060600031028
060700000328     c* 2) - carico linee di arrivo dell'area di cui sono ter-arr.
060800040226     c*      La lna di arrivo = v1cfgs la carico sempre
060900040225     c**                 z-add     ds1nfa        wnfa
061000040225     c**   kfva          setll     fnfva02l                               30
061100040225    1c**   *in30         ifeq      *on
061200000329     c* prendo outq di default per stampa bolle
061300000329     c                   movel     '5P'          cod
061400000329     c                   movel     'B'           w004a             4
061500000329     c                   move      v1cfgs        w004a
061600000329     c                   movel(P)  w004a         key
061700000329     c     ktab          chain     tabel00f                           30
061800030611     c  n30              movel     tbluni        ds5pb
061900030611     c   30              clear                   ds5pb
062000040112     c     v1cfgs        ifne      v1cpfa
062100040109     c* se fgs <> da p.o. foglio significa che sto elaborando per
062200040112     c* un secondo livello in arrivo e quindi metto in schiera la £6 di
062300040112     c* fgs
062400040112     c                   movea     l6            lnap
062500040112     c                   else
062600040112     c                   movea     tar           lnap
062700040109     c                   endif
062800000328     c*
062900000329     c                   z-add     1             y                 2 0
063000040112    2c     lnap(y)       dowgt     *zeros
063100040112     c                   z-add     lnap(y)       wlna
063200000329     c                   exsr      ctrtar
063300000330    3c     wlnaok        ifeq      'S'
063400000330     c     wlna          lookup    slna                                   30
063500000330    4c     *in30         ifeq      *off
063600000329     c                   exsr      carlna
063700000329     c                   add       1             nrr1
063800000329     c                   write     lrs1dsf
063900000330    4c                   endif
064000000330    3c                   endif
064100000329     c                   add       1             y
064200000330    2c                   enddo
064300000328     c
064400040225    1c***                endif
064500000330     c                   seton                                        23
064600000324     c*
064700040109     c                   ENDSR
064800031031
064900031031      *------------------------------------------------------------------------*
065000031031      *    Controllo se lna ha il picking
065100031031      *------------------------------------------------------------------------*
065200031031     c     Sr_CtrPck     BegSr
065300031031
065400031031      * lna con picking attivo
065500031031if  1c                   If        §OgPck = 'S'
065600031031     c                   Eval      *In03 = *On
065700031031     c  n04              Eval      *In04 = *On
065800031031
065900031031      * carico la £6 della linea di arrivo
066000031031     c                   Eval      wpo = V1sLna
066100031031     c                   ExSr      Sr_Car£6
066200031031     c                   Movea     Lin           L6lna
066300031031
066400031031      * se la lna è la capofila della £6
066500031031if  2c                   If        V1sLna = L6lna(1)
066600031031      * memorizzo in una sk di appoggio i suoi p.o. gestiti per poi poter impostare i nuovi flag
066700031031      * partendo a leggere dal secondo elemento
066800031031do  3c     2             Do        30            xx
066900031031     c                   If        L6lna(xx) = *Zeros
067000031031     c                   Leave
067100031031     c                   EndIf
067200031031     c                   Eval      zz = 1
067300031031     c     L6lna(xx)     Lookup    Pck                                    30
067400031031if  4c                   If        Not *In30
067500031031     c     *Zeros        Lookup    Pck(zz)                                30
067600031031     c   30              Eval      Pck(zz) = L6lna(xx)
067700031031    4c                   EndIf
067800031031    3c                   EndDo
067900031031    2c                   EndIf
068000031031
068100031031      * lna senza picking attivo
068200031031   x1c                   Else
068300031031     c                   Eval      *In03 = *Off
068400031031      * controllo se è caricata nella sk di appoggio, se c'è vuol dire che la sua capofila ha
068500031031      * il picking attivo
068600031031     c                   Eval      zz = 1
068700031031     c     v1slna        Lookup    Pck                                    30
068800031031     c                   If        *In30
068900031031     c                   Eval      *In03 = *On
069000031031     c  n04              Eval      *In04 = *On
069100031031     c                   EndIf
069200031031    1c                   EndIf
069300031031
069400031031     c                   EndSr
069500031031
069600000328     C**************                       ****************************
069700000328     c     ctrtar        begsr
069800000328     c                   clear                   wlnaok            1
069900000329     c                   z-add     ds1nfa        wnfa
070000000328     c     kfva          setll     fnfva02l
070100000328     c     kfva          reade     fnfva02l                               30
070200000330    1c     *in30         doweq     *off
070300000328     c     wlnaok        andeq     *blanks
070400000330     c     fvattr        ifne      *blanks
070500000330     c     fvattr        lookup    ttr                                    30
070600000330     c                   endif
070700000330     c* escludo fogli di defluenza
070800000330    2c     *in30         ifeq      *off
070900000330     c     fvattr        oreq      *blanks
071000000330    3c     wlna          ifeq      v1cfgs
071100000330     c                   movel     'S'           wlnaok
071200000330   x3c                   else
071300000329     c                   clear                   fnlv55ds
071400000329     c                   z-add     wlna          d55lin
071500000328     c                   movel     'A'           d55tpt
071600000328     c                   z-add     fvalnp        d55lnp
071700000328     c                   z-add     fvadfv        d55drf
071800000328     c                   call      'FNLV55R'
071900000329     c                   parm                    fnlv55ds
072000000330    4c     d55tfa        ifeq      v1cfgs
072100000328     c                   movel     'S'           wlnaok
072200040109     c                   else
072300040109     c     wlna          lookup    l6                                     30
072400040109     c   30              Eval      wlnaok = 'S'
072500000330    4c                   endif
072600000330    3c                   endif
072700000330    2c                   endif
072800000328     c     kfva          reade     fnfva02l                               30
072900000330    1c                   enddo
073000040225     c     wlnaok        ifeq      *blanks
073100040226     c     wlna          andeq     v1cfgs
073200040225     c                   eval      wlnaok = 'S'
073300040225     c                   endif
073400000328     c                   ENDSR
073500000329     C**************  CARICO LNA IN SCHIERA ***************************
073600000329     C     CARLNA        BEGSR
073700000329     c                   add       10            v1sord
073800040301     c***                clear                   v1ssce
073900040301     c***  v1cris        ifeq      'N'
074000000606     c                   movel     'I'           v1ssce
074100040301     c***                endif
074200000329     c                   z-add     wlna          v1slna
074300031031
074400031028     c                   Clear                   og150
074500000329     c     wlna          chain     azorg01l                           30
074600000329     c  n30              movel     orgdes        v1dlna
074700031028     c  n30              Movel(p)  orgdf0        og150
074800000329     c   30              clear                   v1dlna
074900031031
075000031031      * controllo se la lna ha il picking attivo
075100031031     c                   ExSr      Sr_CtrPck
075200031031
075300031023     c**!!!              movel     §5pbol        v1sprb
075400031023     c**!!!              movel     §5PBpg        V1Hbpg
075500030611     c                   movel     §5PBpl        V1Hbpl
075600030611     c                   movel     §5pba4        v1sprb4
075700030611     c                   movel     §5pba5        v1sprb5
075800030611      *      Se stampa su laser non abilitata proteggo campi
075900031023     C**!!!              IF        §5PBTS = *blanks
076000031023     c**!!!              eval      *in50 = *on
076100031023     c**!!!              eval      V1SBSL = 'N'
076200031023     c**!!!              ELSE
076300031023     c**!!!              eval      *in50 = *off
076400031023     c**!!!              eval      V1SBSL = 'S'
076500031023     c**!!!              ENDIF
076600031028
076700031029      * dft nuovi flag
076800031029     c   03              Eval      vs1ddt = 'S'
076900031029     c   03              Eval      vs1ass = 'N'
077000031104     c   03              Eval      v1hpck = 'S'
077100031029     c  n03              Clear                   vs1ddt
077200031029     c  n03              Clear                   vs1ass
077300031104     c  n03              Clear                   v1hpck
077400030611      *
077500000329     c
077600000329     c                   z-add     1             x
077700000329     c     000           lookup    slna(x)                                30
077800000329     c   30              z-add     wlna          slna(x)
077900000329     C                   ENDSR
078000000329     C**************  controlli subfile    ****************************
078100000329     C     CTRDSF        BEGSR
078200031023     c                   setoff                                       4547
078300031023     c                   setoff                                       4849
078400000329     c                   clear                   sord
078500000329     c                   clear                   wsce
078600000329     c                   z-add     1             v1cnbr
078700000329     c                   z-add     1             nrr1
078800000329     c     nrr1          chain     lrs1dsf                            30
078900000329     c     *in30         doweq     *off
079000000329     c*  NUMERO ORDINAMENTO
079100000329     c*  errore se = 0
079200000329     c     v1sord        ifeq      *zeros
079300000329     c                   seton                                        479028
079400000329     c                   movel     msg(6)        v1cmsg
079500000329     c                   goto      ectdsf
079600000329     c                   endif
079700000329     c     v1sord        lookup    sord                                   30
079800000329     c* errore se già immesso
079900000329     c     *in30         ifeq      *on
080000000329     c                   seton                                        479028
080100000329     c                   movel     msg(7)        v1cmsg
080200000329     c                   goto      ectdsf
080300000329     c                   else
080400000329     c* se non ancora immesso memorizzo in schiera
080500000329     c                   z-add     1             x
080600000329     c     000           lookup    sord(x)                                30
080700000329     c   30              z-add     v1sord        sord(x)
080800000329     c   30              z-add     nrr1          snrr(x)
080900000329     c                   endif
081000030611     c*  outq no laser
081100031023     c**!!!v1sprb        ifeq      *blanks
081200031023     c**!!!              seton                                        449028
081300031023     c**!!!              movel     msg(8)        v1cmsg
081400031023     c**!!!              goto      ectdsf
081500031023     c**!!!              endif
081600031023     c**!!!              movea     v1sprb        cmd1(12)
081700031023     c**!!!              movea     cmd1          comman           80
081800031023     C**!!!              z-add     37            lung             15 5
081900031023     c**!!!              call      'QCMDEXC'                            30
082000031023     c**!!!              parm                    comman
082100031023     c**!!!              parm                    lung
082200031023     c*! 30              movel     msg(9)        v1cmsg
082300031023     c*! 30              seton                                        449028
082400031023     c*! 30              goto      ectdsf
082500030611     c*  outq laser controllo solo se abilitata stampa laser
082600030611     c*   A4
082700031023     c**!!!              IF        *IN50 = *off
082800030611      *
082900030611     c     v1sprb4       ifeq      *blanks
083000030611     c                   seton                                        489028
083100030611     c                   movel     msg(8)        v1cmsg
083200030611     c                   goto      ectdsf
083300030611     c                   endif
083400030611     c                   movea     v1sprb4       cmd1(12)
083500031023     c                   movea     cmd1          comman           80
083600031023     C                   z-add     37            lung             15 5
083700030611     c                   call      'QCMDEXC'                            30
083800030611     c                   parm                    comman
083900030611     c                   parm                    lung
084000030611     c   30              movel     msg(9)        v1cmsg
084100030611     c   30              seton                                        489028
084200030611     c   30              goto      ectdsf
084300030611      *   A5
084400030611     c     v1sprb5       ifeq      *blanks
084500030611     c                   seton                                        499028
084600030611     c                   movel     msg(8)        v1cmsg
084700030611     c                   goto      ectdsf
084800030611     c                   endif
084900030611     c                   movea     v1sprb5       cmd1(12)
085000030611     c                   movea     cmd1          comman
085100030611     C                   z-add     37            lung
085200030611     c                   call      'QCMDEXC'                            30
085300030611     c                   parm                    comman
085400030611     c                   parm                    lung
085500030611     c   30              movel     msg(9)        v1cmsg
085600030611     c   30              seton                                        499028
085700030611     c   30              goto      ectdsf
085800030611      *
085900031023     c**!!!              ENDIF
086000030611     c*  Tipo stampa ERRORE SE NON IMMESSO NESSUN TIPO
086100031023     c**!!!V1Sbsl        ifeq      *blanks
086200031023     c**!!!              movel     msg(13)       v1cmsg
086300031023     c**!!!              seton                                        429028
086400031023     c**!!!              goto      ectdsf
086500031023     c**!!!              endif
086600000329     c*  scelta (inclusione/esclusione)
086700040301     c***  v1ssce        ifne      *blanks
086800040301     c***  wsce          andne     *blanks
086900040301     c***  v1ssce        andne     wsce
087000040301     c***                movel     msg(10)       v1cmsg
087100040301     c***                seton                                        459028
087200040301     c***                goto      ectdsf
087300040301     c***                endif
087400000524     c* ERRORE se ristampa con zona dal/al e richiesta la stampa
087500000524     c*                  di più linee di arrivo
087600000531     c     v1cris        ifeq      'S'
087700000524     c     v1cznd        andgt     0
087800000531     c     v1cris        oreq      'S'
087900000531     c     v1czna        andlt     *hival
088000040301     c***  v1ssce        ifeq      'E'
088100040301     c***  v1ssce        oreq      'I'
088200040301     c***  wsce          andne     *blanks
088300040301     c     v1ssce        ifeq      'I'
088400040301     c     wsce          andne     *blanks
088500000524     c                   movel     msg(12)       v1cmsg
088600000524     c                   seton                                        902845
088700000524     c                   goto      ectdsf
088800000524     c                   endif
088900000524     c                   endif
089000040301     c     v1ssce        ifeq      'I'
089100000329     c                   movel     v1ssce        wsce
089200000329     c                   endif
089300000329     c     ectdsf        tag
089400040301     c                   If        v1hpck = 'S'
089500040301     c                   Eval      *In03 = *On
089600040301     c                   else
089700040301     c                   Eval      *In03 = *off
089800040301     c                   EndIf
089900030611      * se errore esco
090000030611     c                   IF        *in90 = *on
090100030611     c                   z-add     nrr1          v1cnbr
090200000329     c                   update    lrs1dsf
090300030611     c                   LEAVE
090400030611     c                   ELSE
090500040301     c                   update    lrs1dsf
090600000329     c                   add       1             nrr1
090700000329     c     nrr1          chain     lrs1dsf                            30
090800030611     c                   ENDIF
090900000329     c                   enddo
091000000606     c* ERRORE se non immessa nessuna scelta
091100040301     c***  *in90         ifeq      *off
091200040301     c***  wsce          andeq     *blanks
091300040301     c***                seton                                        9028
091400040301     c***  v1cznd        ifgt      0
091500040301     c***  v1cris        andeq     'S'
091600040301     c***  v1czna        orlt      *hival
091700040301     c***  v1cris        andeq     'S'
091800040301     c***                movel     msg(12)       v1cmsg
091900040301     c***                else
092000040301     c***                movel     msg(11)       v1cmsg
092100040301     c***                endif
092200040301     c***                endif
092300030611      *
092400000329     c                   endsr
092500000330     C*****  Aggiorna dati a video su tabella lvd e ricarica subfile *******
092600000330     c     aggior        begsr
092700000330     c                   movea     sord          sords
092800000330     c                   sorta     sords
092900000330     c                   z-add     0             wcont             2 0
093000031030     c                   If        kke1 = *Blanks
093100031030     c                   movel(P)  v1cfgs        kke1
093200031030     c                   EndIf
093300000330     c     ktbe          setll     tntbe01l
093400000330    1c                   do        30            x
093500000330    2c     sords(x)      ifgt      *zeros
093600000330     c* memorizzo l'indice del primo elemento maggiore di 0 (mi serve per
093700000330     c* il lancio azioni batch)
093800000330     c     ii            ifeq      *zeros
093900000330     c                   z-add     x             ii                2 0
094000000330     c                   endif
094100000330     c                   z-add     1             y
094200000330     c     sords(x)      lookup    sord(y)                                30
094300000330     c     snrr(y)       chain     lrs1dsf                            30
094400031103      * se non ho caricato la mia LVD devo aggiornare solo i record che ho emesso
094500031103      * nel subfile
094600031103     c                   Movel     kKe1          wpotntbe
094700031103if 2ac                   If        V1cFgs <> wpotntbe
094800031103     c     sord(y)       div       10            w0020
094900031103     c                   Movel     w0020         kKe2
095000031103     c     w0020         mult      10            v1sord
095100031103     c     ktbe1         Chain     Tntbe01l
095200031103     c                   If        %Found(Tntbe01l) and
095300031103     c                             (TBEsif = knsif  or tbesif = *blanks)
095400031103     c                             and tbeatb = *blanks
095500031103     c                   Clear                   TbeUni
095600031103     c                   Z-add     V1slna        §lvdlna
095700031103     c                   Movel     V1sprb4       §lvdot4
095800031103     c                   Movel     V1sprb5       §lvdot5
095900031103     c                   Movel     dlvd          TbeUni
096000031103     c                   Movel     TbeKe1        TbeFlt
096100031103     c* lo abblenco per togliere il sistema informativo dai record scritti
096200031103     c* dalla vecchia versione del pgm quando ancora veniva inmpostato
096300031103     c                   Clear                   TbeSif
096400031103     c                   Update    Tntbe000
096500031103     c                   Else
096600031103     c                   Movel     'LVD'         TbeCod
096700031103     c                   Movel     V1cFgs        TbeKe1
096800031103     c                   Movel     sord(y)       TbeKe2
096900031103     c                   Z-add     V1slna        §lvdlna
097000031103     c                   Movel     V1sprb4       §lvdot4
097100031103     c                   Movel     V1sprb5       §lvdot5
097200031103     c                   Movel     dlvd          TbeUni
097300031103     c                   Movel     TbeKe1        TbeFlt
097400031103     c* lo abblenco per togliere il sistema informativo dai record scritti
097500031103     c* dalla vecchia versione del pgm quando ancora veniva inmpostato
097600031103     c                   Write     Tntbe000
097700031103     c                   EndIf
097800031103      * se no lascio il giro così com'è
097900031103  x2ac                   Else
098000000330     c                   clear                   wok               1
098100000330     c     wok           doweq     ' '
098200000330     c     ktbe          reade     tntbe01l                               31
098300000330     c  n31              IF        (TBEsif = knsif  or tbesif = *blanks)
098400000427     c                             and tbeatb = *blanks
098500000330     c                   movel     'S'           wok
098600000330     c                   endif
098700000330     c   31              movel     'S'           wok
098800000330     c                   enddo
098900000330    3c     *in31         ifeq      *off
099000000330     c                   movel     tbeuni        dlvd
099100000330     c                   z-add     v1slna        §lvdlna
099200031023     c**!!!              movel     v1sprb        §lvdotq
099300030611     c                   movel     v1sprb4       §lvdot4
099400030611     c                   movel     v1sprb5       §lvdot5
099500031023     c**!!!              movel     v1sbsl        §lvdbsl
099600000330     c                   movel     dlvd          tbeuni
099700000330     c                   movel     tbeke2        wcont
099800000809     c                   movel     tbeke1        tbeflt
099900010129     c* lo abblenco per togliere il sistema informativo dai record scritti
100000010129     c* dalla vecchia versione del pgm quando ancora veniva inmpostato
100100010129     c                   move      *blanks       tbesif
100200000330     c                   update    tntbe000
100300000330   x3c                   else
100400000330     c                   clear                   tntbe000
100500000330     c                   add       1             wcont
100600000330     c                   movel     wcont         tbeke2
100700000330     c                   movel     v1cfgs        tbeke1
100800000330     c                   movel     'LVD'         tbecod
100900000330     c                   clear                   dlvd
101000000330     c                   z-add     v1slna        §lvdlna
101100031023     c**!!!              movel     v1sprb        §lvdotq
101200030611     c                   movel     v1sprb4       §lvdot4
101300030611     c                   movel     v1sprb5       §lvdot5
101400031023     c**!!!              movel     v1sbsl        §lvdbsl
101500000330     c                   movel     dlvd          tbeuni
101600000809     c                   movel     tbeke1        tbeflt
101700000330     c                   write     tntbe000
101800000330    3c                   endif
101900031103   2ac                   EndIf
102000000330    2c                   endif
102100000330    1c                   enddo
102200000330     c* cancello eventuali restanti record di tabella
102300031103      * solo se ho caricato la mia LVD
102400031103if  1c                   If        V1cFgs = wpotntbe
102500000330     c     *in31         ifeq      *off
102600000330     c     ktbe          reade     tntbe01l                               30
102700000330     c     *in30         doweq     *off
102800000330      * Se il S.I. è indicato deve essere uguale al mio
102900000330     c                   IF        (TBEsif = knsif  or tbesif = *blanks)
103000000427     c                             and tbeatb = *blanks
103100000330     c                   delete    tntbe000
103200000330     c                   endif
103300000330     c     ktbe          reade     tntbe01l                               30
103400000330     c                   enddo
103500000330     c                   endif
103600031103    1c                   EndIf
103700000330     c                   endsr
103800000329     C**************  Conferma stampa   *******************************
103900000329     C     CONFE         BEGSR
104000000329     c*
104100000330     c* Richiamo AGGIORNAMENTO tabella "LVD"
104200000330     c                   exsr      aggior
104300000329     c*  Lancio azione batch
104400000330    1c     ii            dowle     30
104500000329     c                   z-add     1             y
104600000330     c     sords(ii)     lookup    sord(y)                                30
104700000329     c     snrr(y)       chain     lrs1dsf                            30
104800000606     c     v1ssce        ifeq      'I'
104900040301     c**   wsce          oreq      'E'
105000040301     c**   v1ssce        andeq     *blanks
105100000329     c                   movel     'LRS2'        kcoaz
105200000329     c                   z-add     v1slna        ds1lna
105300000329     c                   movel     v1cris        ds1ris
105400000329     c                   z-add     v1cznd        ds1znd
105500000329     c                   z-add     v1czna        ds1zna
105600030611      *
105700031023     C**!!!              movel     V1Sbsl        ds1d90bsl
105800031023     C**!!!              If        V1Sbsl = 'S'
105900030611     C                   movel     V1Hbpl        ds1d90psl
106000031023     c**!!!              Else
106100031023     C**!!!              movel     V1Hbpg        ds1d90psl
106200031023     c**!!!              Endif
106300030611      *
106400031023     c**!!!              movel     v1sprb        ds1d90prb
106500030611     c                   movel     v1sprb4       ds1d90prb4
106600030611     c                   movel     v1sprb5       ds1d90prb5
106700030611      *
106800030611     c                   move      v1slna        Wv1slna           3
106900030611     c                   movel(P)  'BOLLE'       ds1d90mdb
107000030611     c                   move      Wv1slna       ds1d90mdb
107100030611     c                   eval      ds1d90mdb4 = 'BOLLE' + Wv1slna + 'A4'
107200030611     c                   eval      ds1d90mdb5 = 'BOLLE' + Wv1slna + 'A5'
107300031029      * nuovi flag
107400031029     c                   Eval      ds1ddt = vs1ddt
107500031029     c                   Eval      ds1ass = vs1ass
107600031104     c                   Eval      ds1pck = v1hpck
107700030611      *
107800000330     c                   movel(P)  fnlrs1ds      kpjbu
107900030624     c**                 call      'FNLRS2R'
108000030624     c                   call      'BCH10'
108100000329     c                   parm                    kpjba
108200000329     c                   endif
108300000330     c                   add       1             ii
108400000329     c                   enddo
108500000329     c                   endsr
108600040301     C**************  impostazione categoria foglio *******************
108700040301     C     SR_npg        BEGSR
108800040301     C                   MOVEL     '7N'          COD
108900040301     C                   MOVEL     *BLANKS       KEY
109000040301     C                   MOVEL     '6'           KEY
109100040301     C     KTAB          CHAIN     TABEL                              30
109200040301     c                   movel     tbluni        DS7N
109300040301     c                   if        *in30  or
109400040301     c                             tblflg<>*blanks  or
109500040301     c                             §7NUTI='G'  or
109600040301     c                             §7NUTI='9'
109700040301     C                   MOVEL     '2'           V1cNPG
109800040301     c                   else
109900040301     C                   MOVEL     '6'           V1cNPG
110000040301     c                   endif
110100040301     c                   endsr
110200960326     C**************  OPERAZIONI INIZIALI  ****************************
110300960326     C     *INZSR        BEGSR
110400960326     C*
110500000324     C     *ENTRY        PLIST
110600000324     C                   PARM                    KPJBA
110700000324     c*
110800960326     C                   Z-ADD     1             CODUT
110900960326     C                   CALL      'X§PARUT'
111000030611     C                   PARM                    ut§dse0f
111100000324     C                   MOVEL     RAGUT         RSUT
111200960329     C                   MOVEL     REC80         CNCR80
111300000324     C* CARICO £1
111400040301     C                   CLEAR                   trul06ds
111500040301     C                   MOVE      '£1'          D06COD
111600040301     C                   MOVEL     SIMFEL        D06KEY
111700040301     C                   MOVEL     trul06ds      KPJBU
111800040301     C                   CALL      'TRUL06R'
111900040301     C                   PARM                    KPJBA
112000040301     C                   MOVEL     KPJBU         trul06ds
112100040301     C                   MOVEA     LIN           L1
112200000327     C* CARICO TABELLA FILIALI GESTITE DA ALTRE  SU TUTTO L'AS
112300030611     C                   CLEAR                   trul06ds
112400000327     C                   MOVE      '£6'          D06COD
112500020509     c                   eval      d06esc = 'G'
112600020509     c                   movel     simfel        d06key
112700030611     C                   MOVEL     trul06ds      KPJBU
112800000327     C                   CALL      'TRUL06R'
112900000327     C                   PARM                    KPJBA
113000030611     C                   MOVEL     KPJBU         trul06ds
113100000329     C                   MOVEl     LIN           nog
113200000324     c
113300000324     c     ktbe          klist
113400000324     c                   kfld                    kcod
113500000324     c                   kfld                    kke1
113600000324     c                   movel     'LVD'         kcod
113700031103     c     ktbe1         klist
113800031103     c                   kfld                    kcod
113900031103     c                   kfld                    kke1
114000031103     c                   kfld                    kke2
114100000327     C     KTAB          KLIST
114200000327     C                   KFLD                    CODUT
114300000327     C                   KFLD                    COD
114400000327     C                   KFLD                    KEY
114500000330     C     KTAB2         KLIST
114600000330     C                   KFLD                    CODUT
114700000330     C                   KFLD                    COD
114800000328     c*
114900000328     c     kfvv1         klist
115000000328     c                   kfld                    ds1npg
115100000328     c                   kfld                    w0050
115200031023     c**!!!              kfld                    v1cfgs
115300040109     c                   kfld                    v1cpfa
115400000328     c     kfva          klist
115500000328     c                   kfld                    ds1npg
115600000329     c                   kfld                    wnfa
115700031023     c**!!!              kfld                    v1cfgs
115800040109     c                   kfld                    v1cpfa
115900040108     C     KCAE5         KLIST
116000040108     C                   KFLD                    KEPA
116100040109     C                   KFLD                    Wtfe
116200040113     C     KCAE5fgs      KLIST
116300040113     C                   KFLD                    KEPA
116400040113     C                   KFLD                    Wtfe
116500040113     C                   KFLD                    v1cfgs
116600000324     c*
116700000324     c     *like         define    tbecod        kcod
116800000324     c     *like         define    tbeke1        kke1
116900000327     C     *LIKE         define    TBLCOD        COD
117000000327     C     *LIKE         define    TBLKEY        KEY
117100000329     C     *LIKE         define    §lvdlna       wlna
117200000329     C     *LIKE         define    v1ssce        wsce
117300000329     C     *LIKE         define    fvanfa        wnfa
117400000330     C**
117500000330     C** CARICO TABELLA  TV - TIPI TRAINO DEFLUENZA
117600000330     C                   movel     'TV'          cod
117700000330     C                   z-add     1             w                 4 0
117800000330     C     ktab2         setll     tabel00f
117900000330     C     KTAB2         READE     TABEL00f                               99
118000000330     C*
118100000330    2C     *in99         doweq     *off
118200000330    3C     tblflg        ifeq      ' '
118300000330     C                   movel     tbluni        dstv
118400000330    4C     §tvdef        ifeq      'S'
118500000330     C                   movel     tblkey        ttr(w)
118600000330     C                   add       1             w
118700000330    4C                   endif
118800000330    3C                   ENDIF
118900000330     C*
119000000330     C     ktab2         reade     tabel00f                               99
119100000330    2C                   enddo
119200960326     C*
119300990513     C                   ENDSR
119400030611      *----------------------------------------------------------------
119500990512** MSG
119600040112P.o. in gestione errato                                                                           1
119700000329Categoria foglio mancante o errata                                                                2
119800000329Categoria foglio non utilizzabile dal p.o. in gestione                                            3
119900000329Foglio errato                                                                                     4
120000000329La zona consegna DAL deve essere minore della zona consegna AL                                    5
120100000329Immettere un numero per assegnare l'ordine di stampa                                              6
120200000329Numero già attribuito ad altra linea di arrivo                                                    7
120300000329Stampante obbligatoria                                                                            8
120400000329Stampante inesistente                                                                             9
120500000329La selezione(Incl/Escl),dove effettuata,deve sempre essere uguale (I oppure E)                   10
120600000606Scegliere almeno una linea di arrivo                                                             11
120700040301Per la ristampa per zona scegliere con 'I' UNA SOLA linea di arrivo                              12
120800031023Libero ...................................................                                       13
120900040109P.o. foglio mancante o errato                                                                    14
121000000329**         CMD1
121100000329CHKOBJ OBJ(XXXXXXXXXX) OBJTYPE(*OUTQ)
