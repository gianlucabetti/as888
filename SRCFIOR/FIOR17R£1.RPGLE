000100040315     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200040316
000300040316      *------------------------------------------------------------------------*
000400040316      *                                                                        *
000500040316      *                 ? CONTROLLO ABBINAMENTO ORM ?                          *
000600040316      *                                                                        *
000700040316      *------------------------------------------------------------------------*
000800040316
000900040316      * ATTENZIONE
001000040326
001100040326      * COMPILARE CON - OVRDBF FILE(TITA400F) TOFILE(GAITRAGRPS/TITA400F)
001200070912      *               - OVRDBF FILE(TITAS30C) TOFILE(GAITRAGRPS/TITAS30C)
001300040326
001400040316      * CARICO IL SUBFILE SOLO FINO A 9999
001500040316
001600040316      *------------------------------------------------------------------------*
001700040316
001800040316     fFnorm13l  if   e           k disk
001900040324     fFnorm01l  uf   e           k disk    rename(Fnorm000:Fnorm01)
002000040326     fFnorf00f  o    e           k disk
002100040318     fTabel00f  if   e           k disk
002200130117     fTNTBE01l  if   e           k disk
002300040316     fAzorg01l  if   e           k disk
002400040324     fFiapd01l  if   e           k disk
002500070911     fFnblp01l  uf   e           k disk
002600060214     fFiar401l  if   e           k disk
002700060214     fFiar404l  if   e           k disk     rename(Fiar4000:Fiar404)
002800040325     fFiar601l  if   e           k disk
002900070918     ffnarb01l  uf   e           k disk
003000070920     f                                      infds(arbinfds)
003100070910     Ffiftt02l  if   e           k disk
003200070910     Ffifst02l  if   e           k disk
003300070910     F                                     RENAME(FIFTT000:FIFST000)
003400070913     Ffiftt01l  uf   e           k disk
003500070910     F                                     RENAME(FIFTT000:FIFTT001)
003600070920     f                                     infds(fttinfds)
003700070919     Ffifst01l  uf   e           k disk
003800070910     F                                     RENAME(FIFTT000:FIFST001)
003900070920     f                                     infds(fstinfds)
004000070919     Ffiftd05l  uf   e           k disk
004100070920     f                                      infds(ftdinfds)
004200070919     Ffifsd05l  uf   e           k disk
004300070919     F                                     RENAME(FIFTd000:FIFsd000)
004400070920     f                                      infds(fsdinfds)
004500040326     fTita400f  o    e             disk     usropn
004600070912     fTitas30c  uf   e           k disk     usropn
004700070912     f                                      ignore(titasp00)
004800070920     f                                      infds(tasinfds)
004900110322     Ffnorg01l  IF   E           K DISK    prefix(R_)
005000120612     Ffidst01l  IF   E           K DISK
005100110322     fFiar501l  uf a e           k Disk
005200040316     fFior17d   cf   e             workstn  sfile(or17s01:nrr1)
005300040316
005400040316      *------------------------------------------------------------------------*
005500040316      *   S C H I E R E
005600040316      *------------------------------------------------------------------------*
005700040615     D CM3             S              1    DIM(122) CTDATA PERRCD(61)
005800040615     D CMA4            S              1    DIM(130) CTDATA PERRCD(65)
005900040615     D CMA5            S              1    DIM(130) CTDATA PERRCD(65)
006000070912     d cmd             s             60    dim(02) ctdata perrcd(1)
006100040316     d l1              s              3  0 dim(30)
006200040316     d l6              s              3  0 dim(30)
006300070911     d msg             s             78    dim(26) ctdata perrcd(1)
006400040316     d opz             s              1    dim(20)
006500040316     d sel             s              1    dim(20)
006600040316
006700040316      *------------------------------------------------------------------------*
006800040316      *   V A R I A B I L I
006900040316      *------------------------------------------------------------------------*
007000040318     d codut           s              1  0 inz(1)
007100040615     d comman          s            110
007200040615     d conta           s              2  0
007300040316     d dopz            s             20    inz('25A')
007400060214     d kar4trc         s                   like(ar4Trc) inz('M')
007500060214     d kar4n14         s                   like(ar4N14)
007600040318     d kcod            s                   like(TblCod) inz('3A')
007700040325     d kkey            s                   like(TblKey)
007800040615     d lenght          s             15  5
007900040316     d nrr1            s              4  0
008000040317     d savkpjbu        s                   like(kpjbu)
008100040318     d xx              s              2  0
008200040316     d varfgs          s                   like(V1cFgs)
008300040413     d wabb            s              1    inz(*Off)
008400040316     d wabi            s                   like(UteAut)
008500051114     d wdata           s              8  0
008600070907     d wdataiso        s               d   datfmt(*iso)
008700040615     d wdatibolla      s              9
008800040615     d wdatibolA4      s              9
008900040615     d wdatibolA5      s              9
009000040326     d wfdar           s              1    inz(*Off)
009100040326     d wlib            s             10
009200051114     d woggi           s               d   datfmt(*iso)
009300040325     d wtb1            s              1
009400060214     d wtrc            s                   like(ar4Trc) inz('M')
009500040317     d w002a           s              2
009600040316     d w0140           s             14  0
009700040316     d $errute         s              1    inz(*off)
009800070910     d w_dar           s                   like(ormdar)
009900070910     d w01fvl          s                   like(fttfvl)
010000070910     d w01tsr          s                   like(ftttsr)
010100070910     d w_ndc           s                   like(fttndc)
010200070920     d wimport         s              1
010300070924     d wpdd            s              1
010400110322     d RitiroPDA       s              1
010500070913     d w_olddrt        s                   like(blpdrt)
010600070919     d savft2          s                   like(blpft2)
010700120612     d kfgs            s                   Like(dstfgs)
010800120612     d knfv            s                   Like(dstnfv)
010900120612     d knpg            s                   Like(dstnpg) inz(4)
011000110322     d kTRD            s                   Like(ar5trd) inz('GEN')
011100130117     d kTBEcod         s                   like(TBEcod) inz('OSR')
011200130117     d wORMcor         s              7s 0 inz
011300130117     d wNSRosr         s              2
011400130117     d wLNPosr         s              3
011500040316
011600040316      *------------------------------------------------------------------------*
011700040316      *   D S   I N T E R N E / E S T E R N E
011800040316      *------------------------------------------------------------------------*
011900051114     d                 ds
012000051114     d  blpaas                 1      4  0
012100051114     d  blpmgs                 5      8  0
012200051114     d  blpdsp                 1      8  0
012300051114
012400040318     d  dskOrm         ds
012500040318     d   V2cPoe
012600040318     d   V2cNsr
012700040318     d   V2cNor
012800040318     d   V2cNrv
012900040318
013000040317     d  dskOsr         ds
013100040317     d   Vs1Poe
013200040331     d   Vs1Nsr
013300040317
013400040318     d dsOrm           ds
013500040318     d  dsopoe                 1      3  0
013600040318     d  dsonor                 4     10  0
013700040318     d  dsonsr                11     12  0
013800040318     d  dsonrv                13     14  0
013900040318     d  dsoopz                15     15
014000040318     d  dsofgs                16     18  0
014100040318     d  dsoref                60     61
014200040318     d  dsomio                62     62
014300040318
014400040318     d dsBlp           ds
014500040318     d  dsbaas                 1      4  0
014600040318     d  dsblnp                 5      7  0
014700040318     d  dsbnrs                 8      9  0
014800040318     d  dsbnsp                10     16  0
014900040326     d  dsbf03                17     17
015000040326     d  dsbrsu                23     42
015100040326     d  dsbflg                31     31
015200040318
015300040318     d wlbdat          ds                  inz
015400040318     d  g02dat                 1      8  0
015500040318     d  g02inv                 9     16  0
015600040318     d  g02err                17     17
015700040318     d  g02tgi                18     22  0
015800040324
015900040324     d ds_V2cCra       DS
016000040326     d  v2cRa1
016100040326     d  v2cRa2
016200040326     d  v2cRa3
016300040324
016400040324     d ds_V2cCor       DS
016500040326     d  v2cOr1
016600040326     d  v2cOr2
016700040326     d  v2cOr3
016800040324
016900040324     d ds_V2cCrc       DS
017000040326     d  v2cRc1
017100040326     d  v2cRc2
017200040326     d  v2cRc3
017300040316
017400040316     d                sds
017500040316     d  vtcpgm                 1     10
017600040316
017700040316     d Azuteds       e ds                  extname(Azute00f)
017800040615     d dblp          e ds
017900040316     d dDatiute      e ds
018000070710     d dfar          e ds
018100040316     d dLat          e ds
018200040325     d dorm01        e ds
018300040317     d dOsr          e ds
018400040615     d dtasv         e ds
018500040318     d dsBl4m        e ds
018600040318     d ds3a          e ds
018700070907     d dvpodeco      e ds
018800040324     d ds_cnaco      e ds                  extname(Cnaco00f)
018900040324     d ds_cnind      e ds                  extname(Cnind00f)
019000040324     d ds_cnclp      e ds                  extname(Cnclp00f)
019100040324     d ds_fncls      e ds                  extname(Fncls00f)
019200040317     d Fior05ds      e ds
019300040318     d Fnlv19ds      e ds
019400040316     d Kpjba         e ds
019500040326     d Og143         e ds
019600110322     d dDSTFLR       e ds
019700110322     d dAr5GEN       e ds
019800040316     d Tibs02ds      e ds
019900040316     d Tibs34ds      e ds
020000040324     d Tibs69ds      e ds
020100040316     d Trul06ds      e ds
020200040316     d  lin                    1     90  0 dim(30)
020300040615     d Trul90ds      e ds
020400060516     d fnlv55ds      e ds
020500070907      * - Aggiunta/Sottrazione di n giorni lavorativi ad una data
020600070919     d TRUL52ds      e ds
020700070919
020800070919     d fnlva7ds      e ds
020900070919
021000070919     D* PASSAGGIO DATI A SVALORIZZAZ. DISTINTA PADRONC.  - FNLRE2R -
021100070919     D PARAM3          DS
021200070919     D  PA3PDR                 1      7  0
021300070919     D  PA3TSR                 8      8
021400070919     D  PA3NDC                 9     15  0
021500070919     D  PA3FCT                16     16
021600070919     D  PA3FVD                17     17
021700070919     D  PA3FVT                18     18
021800070919     D  PA3DDC                19     26  0
021900070919     D  PA3sml               256    256
022000070919
022100070920     d TRUL82ds      e ds
022200070919
022300070920     d tasinfds        ds
022400070920     d  tasnrr               397    400i 0
022500070920     d fttinfds        ds
022600070920     d  fttnrr               397    400i 0
022700070920     d fstinfds        ds
022800070920     d  fstnrr               397    400i 0
022900070920     d ftdinfds        ds
023000070920     d  ftdnrr               397    400i 0
023100070920     d fsdinfds        ds
023200070920     d  fsdnrr               397    400i 0
023300070920     d arbinfds        ds
023400070920     d  arbnrr               397    400i 0
023500040316
023600040316      *------------------------------------------------------------------------*
023700070919      *   C O S T A N T I
023800040316      *------------------------------------------------------------------------*
023900040316      *  titolo videata (lunghezza massima 34)
024000040316     d titolo          c                   const('** CONTROLLO ABBINAMENTO O.R-
024100040317     d                                     .M. **')
024200040316      *  Descrizione opzioni (lunghezza massima 78)
024300040317     d dopz1           c                   const('2=Manutenzione   5=Visualizza-
024400040317     d                                        A=Abbina')
024500040316     d dopz2           c                   const(' ')
024600040316
024700070912     I* PREDISPONGO GLI INDICATORI DI TIPO RECORD PER TITAS
024800070912     ITITAS000      22
024900070912     ITITAS010      23
025000040316      *------------------------------------------------------------------------*
025100040316      *  RIEPILOGO INDICATORI
025200040316      *------------------------------------------------------------------------*
025300040316      * 05 - SPROTEGGO P.O. IN GESTIONE
025400040316      * 06 - NON ABILITO TASTO FUNZIONALE F18
025500040316      * 20 - GESTIONE SUBFILE
025600040316      * 21 - GESTIONE SUBFILE
025700040316      * 28 - ERRORE GENERICO DSPF
025800040316      * 30 - Comodo
025900040316      * 40 - ERRORE p.o. in gestione
026000040318      * 41 - ERRORE SFL
026100040318      * 42 - ERRORE bolla
026200040616      * 50 - Orm con bolla in assegnato
026300070912      * 98 - ERRORE ovrdbf di Titas30c
026400070912      * 99 - ERRORE ovrdbf di Tita400f
026500040316
026600040316      *------------------------------------------------------------------------*
026700040316
026800040317      * Errore sull'utente esco dal pgm
026900040316if  1c                   If        $ErrUte = *On
027000040316     c                   Goto      Fine
027100040316e   1c                   EndIf
027200040317
027300040317      * Errore sul p.o. esco dal pgm
027400040317if  1c                   If        *In40 = *On
027500040317     c                   Goto      Fine
027600040317e   1c                   EndIf
027700040316
027800040317      * Carico subfile ?
027900040317     c     Carica        Tag
028000040317     c                   ExSr      Sr_CarSfl
028100040317
028200040317      * Emetto il subfile ?
028300040317     c     EmiS01        Tag
028400040317     c                   ExSr      Sr_EmiSfl
028500040317     c                   Eval      *In28 = *Off
028600040317
028700040316      * F3=Fine
028800040317     c   kc              GoTo      Fine
028900040413
029000040413      * F5=Rivisualizza subfile
029100040413     c   ke              GoTo      Carica
029200040317
029300040316      * F18=Cambio P.O. in gestione
029400040316     c   ks              Eval      *In05 = *On
029500040317     c   ks              GoTo      EmiS01
029600040316      * 05 on --> controllo P.O. in gestione
029700040316if  1c                   If        *In05
029800040316     c                   ExSr      Sr_ContrFgs
029900040316     c  n28              Eval      *In05 = *Off
030000040317     c  n28              GoTo      Carica
030100040317     c   28              GoTo      EmiS01
030200040316e   1c                   EndIf
030300040316      * Controllo p.o. gestione se variato
030400040316if  1c                   If        V1cFgs <> varfgs
030500040316     c                   ExSr      Sr_ContrFgs
030600040317     c  n28              GoTo      Carica
030700040317     c   28              GoTo      EmiS01
030800040316e   1c                   EndIf
030900040316
031000040316      * Leggo il subfile ?
031100040316if  1c                   If        nrr1 > *Zeros
031200040316     c                   ExSr      Sr_LetSfl
031300040413     c                   GoTo      EmiS01
031400040316e   1c                   EndIf
031500040316
031600040316     c     Fine          Tag
031700090403     c* chiusurra pgm
031800090403     C                   MOVEL     'C'           SA2TLA
031900090403     C                   CALL      'FNLSA2R3'
032000090403     C                   PARM                    SA2TLA
032100090403     C                   PARM                    SA2TRC
032200090403     C                   PARM      00000000      SA2DTS
032300090403     C                   PARM      000           WWLNA             3 0
032400040316
032500040316     c                   Eval      *InLr = *On
032600040316
032700040316      *------------------------------------------------------------------------*
032800040316      * - CONTROLLO IL P.O. IN GESTIONE
032900040316      *------------------------------------------------------------------------*
033000040316     c     Sr_ContrFgs   BegSr
033100040316
033200040316     c                   Setoff                                       40
033300040316     c                   Clear                   V1dFgs
033400060926     c**!!!              Clear                   Og148
033500040317     c                   Eval      varfgs = V1cFgs
033600040316
033700040316     c     V1cFgs        Chain     Azorg01L
033800040316if  1c                   If        Not %Found(Azorg01l) or OrgFva <> *Blanks or
033900040316     c                             (OrgFag <> 'A' and OrgFag <> 'F')
034000040316     c                   Eval      *In28 = *On
034100040316     c                   Eval      *In40 = *On
034200040316     c                   Eval      V1cMsg = msg(01)
034300040316     c                   Goto      EndFgs
034400040316e   1c                   EndIf
034500040316
034600040316     c                   movel     OrgDes        V1dFgs
034700060926     c**!!!              Movel     OrgDe8        Og148
034800040316
034900040316      * procedura ORM non attiva
035000060926if  1c**!!!              If        §ogorm <> 'S'
035100060926     c**!!!              Eval      *In28 = *On
035200060926     c**!!!              Eval      *In40 = *On
035300060926     c**!!!              Eval      V1cMsg = msg(02)
035400060926     c**!!!              Goto      EndFgs
035500060926e   1c**!!!              EndIf
035600040317
035700040316      * Carico L6
035800040316     c                   Clear                   Trul06ds
035900040316     c                   Eval      D06Cod = '£6'
036000040316     c                   Movel     V1cFgs        D06Key
036100040316     c                   Movel(p)  Trul06ds      kpjbu
036200040316     c                   Call      'TRUL06R'
036300040316     c                   Parm                    kpjba
036400040316     c                   Movel     kpjbu         Trul06ds
036500040316     c                   Movea     lin           l6
036600040316
036700040316     c     V1cFgs        Lookup    l1                                     30
036800040316if  1c                   If        Not *In30
036900040316     c                   Eval      *In28 = *On
037000040316     c                   Eval      *In40 = *On
037100040316     c                   Eval      V1cMsg = msg(01)
037200040316     c                   Goto      EndFgs
037300040316e   1c                   EndIf
037400040316
037500040316     c     EndFgs        EndSr
037600040316
037700040316      *------------------------------------------------------------------------*
037800040316      * - CARICO IL SUBFILE
037900040316      *------------------------------------------------------------------------*
038000040316     c     Sr_CarSfl     BegSr
038100040316
038200040316      * Imposto le opzioni di scelta
038300040316     c                   Clear                   opz
038400040317     c                   Movel     dopz1         V1dse1
038500040317     c                   Movel     dopz2         V1dse2
038600040316     c                   Movea     dopz          opz
038700040316
038800040316     c                   Eval      recsf = 1
038900040316
039000040316      * Pulisco subfile
039100040316     c                   Clear                   nrr1
039200040316     c                   Eval      *In21 = *Off
039300040316     c                   Write     or17c01
039400040316     c                   Eval      *In20 = *On
039500040316     c                   Eval      *In21 = *On
039600040317
039700040316
039800040316      * Leggo gli orm per p.o. ritiro
039900040316do  1c                   Do        30            xx
040000040316     c     l6(xx)        Setll     Fnorm13l
040100040316do  2c                   Do        *Hival
040200040316     c     l6(xx)        Reade     Fnorm13l
040300040317
040400040317      * Fine file
040500040316if  3c                   If        %Eof (Fnorm13l)
040600040316     c                   Leave
040700040316e   3c                   EndIf
040800040316
040900040316      * carico fino a 9999 record
041000040316if  3c                   If        nrr1 >= 9998
041100040316     c                   Eval      *In28 = *On
041200040317     c                   Eval      V1cMsg = msg(03)
041300040316     c                   Leave
041400040316e   3c                   EndIf
041500040316
041600040316      * Escludo gli annullati
041700040316if  3c                   If        OrmAtb <> *Blanks
041800040316     c                   Iter
041900040316e   3c                   EndIf
042000040316
042100040316      * Devo emettere solo gli orm quadrati in fase 600
042200040316if  3c                   If        OrmFao <> 600
042300040316     c                   iter
042400040316e   3c                   EndIf
042500040615
042600060214      * non deve avere un ar4 abbinato
042700040615     c                   Clear                   worm             14
042800040615     c                   Move      OrmPoe        wpoe              3
042900040615     c                   Move      OrmNsr        wnsr              2
043000040615     c                   Move      OrmNor        wnor              7
043100040615     c                   Move      OrmNrv        wnrv              2
043200040615     c                   Eval      worm = wpoe + wnsr
043300040615     c                             + wnor + wnrv
043400060214     c                   Movel     worm          kar4n14
043500060214     c     kfiar4        Setll     Fiar404l
043600060214     c                   If        %Equal(Fiar404l)
043700040615     c                   iter
043800040615     c                   EndIf
043900040316
044000040317     c                   Clear                   Vs1Opz
044100040317     c                   Eval      Vs1Poe = OrmPoe
044200040317     c                   Eval      Vs1Nsr = OrmNsr
044300040317     c                   Eval      Vs1Nor = OrmNor
044400040317     c                   Eval      Vs1Nrv = OrmNrv
044500040317     c                   Eval      Vs1Rsr = ORmRsr
044600040317     c                   Eval      Vs1Lor = OrmLor
044700040317     c                   Eval      Vs1Fao = OrmFao
044800130117     c                   Eval      VH1cor = ORMcor
044900040316
045000070925     c                   exsr      sr_drt
045100040316      * Inversione data ritiro
045200071107     c****               if        dateu<§vpodrt
045300071107     c****               Z-add     OrmDar        g02inv
045400071107     c****               else
045500070925     c                   Z-add     w_dar         g02inv
045600071107     c****               endif
045700040316     c                   Eval      g02err = '3'
045800040316     c                   Call      'XSRDA8'
045900040316     c                   Parm                    wlbdat
046000040317     c                   Movel     g02dat        Vs1Dar
046100040413
046200040413     c                   Clear                   Vs1Abb
046300040316
046400040316     c                   Add       1             nrr1
046500040316     c                   Write     or17s01
046600040317
046700040317e   2c                   EndDo
046800040317e   1c                   EndDo
046900040316
047000040316     c                   EndSr
047100040316
047200040316      *------------------------------------------------------------------------*
047300040316      * - EMETTO IL SUBFILE
047400040316      *------------------------------------------------------------------------*
047500040316     c     Sr_EmiSfl     BegSr
047600040316
047700040316      * Controllo se recsf è maggiore di nrr
047800040316if  1c                   If        recsf > nrr1
047900040316     c                   Eval      recsf = 1
048000040316e   1c                   EndIf
048100040316
048200040316      * Se non ho caricato emetto una videata di segnalazione
048300040316if  1c                   If        nrr1 = *Zeros
048400040317     c                   Exfmt     or17d01
048500040316e   1c                   EndIf
048600040316
048700040316      * Se ho caricato emetto il subfile
048800040316if  1c                   If        nrr1 > *Zeros
048900040316     c                   Write     or17z01
049000040316     c                   Exfmt     or17c01
049100040316e   1c                   EndIf
049200040316
049300040316     c                   EndSr
049400040316
049500040316      *------------------------------------------------------------------------*
049600040316      * - LEGGO IL SUBFILE
049700040316      *------------------------------------------------------------------------*
049800040316     c     Sr_LetSfl     BegSr
049900040316
050000040316     c                   Clear                   nrr1
050100040316
050200040316do  1c                   Do        *Hival
050300040317     c                   Eval      nrr1 = nrr1 +1
050400040317     c     nrr1          Chain     or17s01                            30
050500040317     c   30              Leave
050600040316
050700040318     c                   Setoff                                       28  41
050800040413     c                   Eval      wabb = *Off
050900040316
051000040316      * Controllo se scelta valida
051100040317     c     Vs1Opz        Lookup    opz                                    30
051200040316if  2c                   If        Not *In30
051300040316     c                   Eval      *In28 = *On
051400040318     c                   Eval      *In41 = *On
051500040317     c                   Eval      V1cMsg = msg(04)
051600040316     c                   Eval      recsf = nrr1
051700040316     c                   Update    or17s01
051800040316     c                   GoTo      EmiS01
051900040317e   2c                   EndIf
052000040325
052100040325      * ORM con numero serie
052200040326      * controllo se scelta impedita nella tab. "OSR"
052300040317if  2c                   If        Vs1Nsr <> *Zeros
052400040317     c                   ExSr      Sr_Serie
052500130117     c***                Move      Vs1Nsr        w002a
052600040325      * Tabella OSR non trovata
052700040325if  3c                   If        *In28
052800040318     c                   Eval      *In41 = *On
052900040317     c                   Eval      V1cMsg = msg(05)
053000130117     c********           Eval      V1cMsg = %replace(wNSRosr:V1cMsg:
053100130117     c********                              %scan('__':V1cMsg))
053200130117     c                   eval      V1Cmsg = 'ERRORE: Ordinante ' +
053300130117     c                             %editc(wORMcor:'X') + ' non abilitato -
053400130117     c                             in tab. OSR. Avvisare CED'
053500040317     c                   Eval      recsf = nrr1
053600040317     c                   Update    or17s01
053700040317     c                   GoTo      EmiS01
053800040317e   3c                   EndIf
053900040317      * Controllo se posso manutenzionare l'ORM
054000040317if  3c                   If        Vs1Opz = '2' and d§OsrMan = 'S'
054100040317     c                   Eval      *In28 = *On
054200040318     c                   Eval      *In41 = *On
054300040317     c                   Eval      V1cMsg = 'Manutenzione' + msg(06)
054400130117     c                   Eval      V1cMsg = %replace(wNSRosr:V1cMsg:
054500040317     c                                      %scan('__':V1cMsg))
054600130117     C                   eval      V1Cmsg = %trim(V1Cmsg) +
054700130117     C                             ' LNP ' + wLNPosr
054800040317     c                   Eval      recsf = nrr1
054900040317     c                   Update    or17s01
055000040317     c                   GoTo      EmiS01
055100040317e   3c                   EndIf
055200040317e   2c                   EndIf
055300040325
055400040317      * Scelta
055500040317s   2c                   Select
055600040317      * Visualizzazione
055700040317w    c                   When      Vs1Opz = '5'
055800040317     c                   Eval      recsf = nrr1
055900040317     c                   ExSr      Sr_Modifica
056000040317     c                   Eval      recsf = nrr1
056100040317     c                   Clear                   Vs1Opz
056200040317     c                   Update    or17s01
056300040326      * Manutenzione
056400040317w    c                   When      Vs1Opz = '2'
056500040317     c                   Eval      recsf = nrr1
056600040317     c                   ExSr      Sr_Modifica
056700040317     c                   Eval      recsf = nrr1
056800040317     c                   Clear                   Vs1Opz
056900040317     c                   Update    or17s01
057000040317      * Abbinamento
057100040317w    c                   When      Vs1Opz = 'A'
057200040317     c                   Eval      recsf = nrr1
057300040324     c                   ExSr      Sr_CarOrm
057400040317     c                   ExSr      Sr_Abbina
057500040317     c                   Eval      recsf = nrr1
057600040317     c                   Clear                   Vs1Opz
057700040413     c                   If        wabb = *On
057800040413     c                   Eval      Vs1Abb = 'A'
057900040413     c                   EndIf
058000040317     c                   Update    or17s01
058100040317e   2c                   EndSl
058200040326
058300040317e   1c                   EndDo
058400040326
058500040317     c                   EndSr
058600040316
058700130121      /free
058800130121       //--------------------------------------------------------------
058900130121       //?Cerco tabella OSR per codice ordinante
059000130121       //--------------------------------------------------------------
059100130121       BEGSR sr_Serie;
059200130121
059300130121         clear dOSR;
059400130121         wORMcor = %dec(%subst(%editc(VH1cor:'X'):1:7):7:0);
059500130121       //?leggo tutta la tabella OSR
059600130121         setll kTBEcod tntbe01l;
059700130121         reade kTBEcod tntbe01l;
059800130121
059900130121         DOW not %eof(tntbe01l);
060000130122           IF  TBEatb = *blanks;
060100130121           dosr = tbeuni;
060200130121           IF d§osrcli  = wORMcor or d§OSRcl2  = wORMcor or
060300130121              d§osrcl3  = wORMcor or d§OSRcl4  = wORMcor or
060400130121              d§osrcl5  = wORMcor or d§OSRcl6  = wORMcor or
060500130121              d§osrcl7  = wORMcor or d§OSRcl8  = wORMcor or
060600130121              d§osrcl9  = wORMcor or d§OSRcl10 = wORMcor or
060700130121              d§osrcl11 = wORMcor or d§OSRcl12 = wORMcor or
060800130121              d§osrcl13 = wORMcor or d§OSRcl14 = wORMcor or
060900130121              d§osrcl15 = wORMcor or d§OSRcl16 = wORMcor;
061000130121             leavesr;
061100130121           ENDIF;
061200130122           ENDIF;
061300130121           reade kTBEcod tntbe01l;
061400130121         ENDDO;
061500130121
061600130121       //?se arrivo qua vuol dire che non ho trovato il cliente in tabella OSR
061700130121         *in28 = *on;
061800130121
061900130121       ENDSR;
062000130121      /end-free
062100040316
062200040317      *------------------------------------------------------------------------*
062300040317      * RICHIAMO IL PROGRAMMA DI MODIFICA O INTERROGAZIONE
062400040317      *------------------------------------------------------------------------*
062500040324     c     Sr_Modifica   BegSr
062600040326
062700040318     c                   Clear                   dsOrm
062800040318     c                   Eval      dsopoe = Vs1Poe
062900040318     c                   Eval      dsonor = Vs1Nor
063000040318     c                   Eval      dsonsr = Vs1Nsr
063100040318     c                   Eval      dsonrv = Vs1Nrv
063200040318     c                   Eval      dsoopz = Vs1Opz
063300040318     c                   Eval      dsofgs = V1cFgs
063400040318     c                   Eval      dsoref = 'SI'
063500040318     c                   Eval      dsomio = 'S'
063600040317     c                   Eval      savkpjbu = kpjbu
063700040318     c                   Eval      kpjbu = dsOrm
063800040326
063900040317s   1c                   Select
064000040317      * Modifica
064100040317w    c                   When      Vs1Opz = '2'
064200040317     c                   Clear                   Fior05ds
064300040317     c                   Call      'FIOR05R'
064400040317     c                   Parm                    kpjba
064500040317     c                   Parm                    Fior05ds
064600040326      * Interrogazione
064700040317w    c                   When      Vs1Opz = '5'
064800040317     c                   Call      'FIOR07R'
064900040317     c                   Parm                    kpjba
065000040317e   1c                   EndSl
065100040317
065200040318     c                   Eval      dsOrm = kpjbu
065300040317     c                   Eval      kpjbu = savkpjbu
065400040326
065500040317     c                   EndSr
065600040324
065700040324      *------------------------------------------------------------------------*
065800040324      * CARICO I DATI DELL'ORM
065900040324      *------------------------------------------------------------------------*
066000040324     c     Sr_CarOrm     BegSr
066100040324
066200040324     c     kOrm          Chain(n)  Fnorm01l
066300040324if  1c                   If        %Found(Fnorm01l)
066400040324
066500040324      * Numero ORM
066600040324     c                   Eval      V2cPoe = OrmPoe
066700040324     c                   Eval      V2cNsr = OrmNsr
066800040324     c                   Eval      V2cNor = OrmNor
066900040324     c                   Eval      V2cNrv = OrmNrv
067000040324      * Tipo ORM
067100040324     c                   Clear                   Tibs02ds
067200040324     c                   Clear                   V2dTor
067300040324     c                   Eval      T02Mod = 'C'
067400040324     c                   Eval      T02Sif = knsif
067500040324     c                   Eval      T02Cod = 'TOR'
067600040324     c                   Eval      T02Ke1 = OrmTor
067700040324     c                   Call      'TIBS02R'
067800040324     c                   Parm                    kpjba
067900040324     c                   Parm                    Tibs02ds
068000040324     c                   If        T02Err = *Blanks
068100040324     c                   Movel     T02Uni        V2dTor
068200040324     c                   EndIf
068300040324      * Data ritiro
068400070925     c***                Z-add     OrmDar        g02inv
068500070925     c***                Movel     '3'           g02err
068600070925     c***                Call      'XSRDA8'
068700070925     c***                Parm                    wlbdat
068800070925     c***                Z-add     g02dat        V2cDar
068900070925     c                   exsr      sr_drt
069000070925      * Data ritiro
069100071107     c***                if        dateu<§vpodrt
069200071107     c***                Z-add     OrmDar        g02inv
069300071107     c***                else
069400070925     c                   z-add     w_dar         g02inv
069500071107     c****               endif
069600070925     c                   Movel     '3'           g02err
069700070925     c                   Call      'XSRDA8'
069800070925     c                   Parm                    wlbdat
069900070925     c                   Z-add     g02dat        V2cDar
070000040324      * Mittente
070100040326     c                   Move      OrmCra        ds_v2cCra
070200040324     c                   Eval      V2cRsr = OrmRsr
070300040324     c                   Eval      V2CInr = OrmInr
070400040324     c                   Eval      V2cCar = OrmCar
070500040324     c                   Eval      V2cLor = OrmLor
070600040324     c                   Eval      V2cPrr = OrmPrr
070700040324     c                   Eval      v2cNar = OrmNar
070800040324      * Ordinante
070900040326     c                   Move      OrmCor        ds_v2cCor
071000040324     c                   Eval      V2cRso = OrmRso
071100040324     c                   Eval      V2CIno = OrmIno
071200040324     c                   Eval      V2cCao = OrmCao
071300040324     c                   Eval      V2cLoo = OrmLoo
071400040324     c                   Eval      V2cPro = OrmPro
071500040324     c                   Eval      v2cNao = OrmNao
071600040324      * Destinatario
071700040326     c                   Move      OrmCrc        ds_v2cCrc
071800040324     c                   Eval      V2cRsc = OrmRsc
071900040324     c                   Eval      V2CInc = OrmInc
072000040324     c                   Eval      V2cCac = OrmCac
072100040324     c                   Eval      V2cLoc = OrmLoc
072200040324     c                   Eval      V2cPrc = OrmPrc
072300040324     c                   Eval      v2cNac = OrmNac
072400040324      * Colli - Peso - Volume
072500040324     c                   Eval      V2cncl = OrmNcl
072600040324     c                   Eval      V2cpkg = OrmPkg
072700040324     c                   Eval      V2cvlm = OrmVlm
072800040409      * Tipo comunicazione ORM
072900040409     c                   Clear                   V2dTco
073000040409     c                   Clear                   Tibs02ds
073100040409     c                   Eval      T02Mod = 'C'
073200040409     c                   Eval      T02Sif = knsif
073300040409     c                   Eval      T02Cod = 'TCO'
073400040409     c                   Eval      T02Ke1 = OrmTco
073500040409     c                   Call      'TIBS02R'
073600040409     c                   Parm                    kpjba
073700040409     c                   Parm                    Tibs02ds
073800040409     c                   Eval      V2dTco = T02Uni
073900040325      * P.O. conegna
074000040325     c                   Clear                   V2dPoc
074100040325     c                   Eval      V2cPoc = OrmPoc
074200040325     c     OrmPoc        Chain     Azorg01l
074300040325     c                   If        %Found(Azorg01l)
074400040325     c                   Eval      V2dPoc = OrgDes
074500040325     c                   EndIf
074600040324      * Chi paga
074700040324     c                   Clear                   V2dPag
074800040324     c                   Clear                   V2dKsc
074900040324     c                   Select
075000040324     c                   When      OrmPag = 'M'
075100040324     c                   Eval      V2dPag = 'Mitt.'
075200040324     c                   When      OrmPag = 'D'
075300040324     c                   Eval      V2dPag = 'Dest.'
075400040324     c                   When      OrmPag = 'O'
075500040326     c                   Eval      V2dPag = 'Ord. '
075600040324     c                   EndSl
075700040324     c                   Eval      V2cKsc = OrmKsc
075800040324     c                   Clear                   Tibs69ds
075900040324     c                   Eval      I69Kac = OrmKsc
076000040324     c                   Call      'TIBS69R'
076100040324     c                   Parm                    Tibs69ds
076200040324     c                   Parm                    ds_cnaco
076300040324     c                   Parm                    ds_cnind
076400040324     c                   Parm                    ds_cnclp
076500040324     c                   Parm                    ds_fncls
076600040324     c                   If        O69Err <> '1'
076700040324     c                   Movel     AcoRag        V2dKsc
076800040324     c                   EndIf
076900040409      * Autotrasportatore
077000040409     c                   Clear                   V2cPdc
077100040409     c                   Clear                   V2dPdc
077200120612     c                   Eval      kNfv = OrmNdc
077300120612     c                   Eval      kFgs = OrmPor
077400120612     c     kDst          Chain     Fidst01l
077500060516      * se non trovo la distinta con il p.o. ritiro controllo se è un p.o. gestito
077600120612     c                   if        not %found(fidst01l)
077700060516     c                   clear                   fnlv55ds
077800060516     c                   eval      d55tpt = '6'
077900060516     c                   move      ormpor        d55lin
078000060516     c                   eval      d55drf = ormddc
078100060516     c                   call      'FNLV55R'
078200060516     c                   parm                    fnlv55ds
078300060516     c                   if        d55err = *blanks and d55tfa <> d55lin
078400120612     c                   move      d55tfa        kfgs
078500120612     c     kdst          chain     fidst01l
078600060516     c                   endif
078700060516     c                   endif
078800120612     c                   If        %Found(Fidst01l)
078900040409     c                   Eval      V2cPdc = DstPdr
079000040409     c                   EndIf
079100040409     c                   Eval      ApdPdr = DstPdr
079200040409     c                   Eval      ApdTip = 'A'
079300040409     c     kApd          Chain     Fiapd01L
079400040409     c                   If        %Found(Fiapd01l)
079500040409     c                   Eval      V2dPdc = ApdRsc
079600040409     c                   EndIf
079700040325
079800040325     c                   Eval      dorm01 = OrmFlo
079900040409      * Commissionato
080000040409     c                   If        §orCom = 'S'
080100040409     c                   Eval      V2dCom = 'COMMISSIONATO'
080200040409     c                   Else
080300040409     c                   Clear                   V2dCom
080400040409     c                   EndIf
080500040616
080600040616      * Bolla in assegnato
080700040616     c                   Eval      *In50 = (§OrPfb = 'A')
080800040324
080900040326e   1c                   EndIf
081000040324
081100040324     c                   EndSr
081200040317
081300040317      *------------------------------------------------------------------------*
081400040317      * GESTIONE ABBINAMENTO ORM A BOLLA
081500040317      *------------------------------------------------------------------------*
081600040324     c     Sr_Abbina     BegSr
081700040318
081800040413     c                   ExSr      Sr_Puld02
081900040413
082000040318do  1c                   Do        *Hival
082100040318     c                   Exfmt     or17d02
082200040318     c                   Setoff                                       28
082300040318
082400040318      * F12=Ritorna a sbfl
082500040413     c   kl              Leave
082600040318      * F7=Interrogazione bolle partenza
082700040318if  2c                   If        *InKg
082800040318     c                   Clear                   dsBlp
082900040615     c                   ExSr      Sr_Puld02
083000040326      * se c'è già la spedizione vado diretta sulla bolla
083100040326if  3c                   If        V2cNsp > *Zeros
083200040326     c                   Eval      dsbaas = V2cAas
083300040326     c                   Eval      dsblnp = V2cLnp
083400040326     c                   Eval      dsbnrs = V2cNrs
083500040326     c                   Eval      dsbnsp = V2cNsp
083600040326     c                   Eval      dsbrsu = rsut
083700040326     c                   Eval      savkpjbu = kpjbu
083800040326     c                   Eval      kpjbu = dsBlp
083900040326     c                   Call      'FNLS05R'
084000040326     c                   Parm                    kpjba
084100040326     c                   Eval      dsBlp = kpjbu
084200040326     c                   Eval      kpjbu = savkpjbu
084300040326    3c                   EndIf
084400040326      * se non c'è la spedizione richiamo il filtro
084500040326if  3c                   If        V2cNsp = *Zeros
084600040326     c                   Eval      dsbflg = '1'
084700040326     c                   Eval      savkpjbu = kpjbu
084800040326     c                   Eval      kpjbu = dsBlp
084900040326     c                   Call      'FNLS04R'
085000040326     c                   Parm                    kpjba
085100040326     c                   Eval      dsBlp = kpjbu
085200040326     c                   Eval      kpjbu = savkpjbu
085300040326    3c                   EndIf
085400040318if  3c                   If        dsbnsp > *Zeros
085500040318     c                   Eval      V2cAas = dsbaas
085600040318     c                   Eval      V2cLnp = dsblnp
085700040318     c                   Eval      V2cNrs = dsbnrs
085800040318     c                   Eval      V2cNsp = dsbnsp
085900040318e   3c                   EndIf
086000040318e   2c                   EndIf
086100040324      * F8=Interrogazione ORM
086200040324if  2c                   If        *InKh
086300040324     c                   Clear                   dsOrm
086400040324     c                   Eval      dsopoe = V2cPoe
086500040324     c                   Eval      dsonor = V2cNor
086600040324     c                   Eval      dsonsr = V2cNsr
086700040324     c                   Eval      dsonrv = V2cNrv
086800040324     c                   Eval      dsoopz = '5'
086900040324     c                   Eval      dsofgs = V1cFgs
087000040324     c                   Eval      dsoref = 'SI'
087100040324     c                   Eval      dsomio = 'S'
087200040324     c                   Eval      savkpjbu = kpjbu
087300040324     c                   Eval      kpjbu = dsOrm
087400040324     c                   Call      'FIOR07R'
087500040324     c                   Parm                    kpjba
087600040324     c                   Eval      dsOrm = kpjbu
087700040324     c                   Eval      kpjbu = savkpjbu
087800040324e   2c                   EndIf
087900040318
088000040318      * controlli
088100040318     c                   ExSr      Sr_CtrD02
088200040318
088300040326     c   28
088400040326     cornkf              Iter
088500040318
088600040318      * F6=Abbina ORM alla bolla
088700040318     c                   ExSr      Sr_AbbOrm
088800070911     c   28              Iter
088900040413     c                   ExSr      Sr_Puld02
089000040318
089100040318e   1c                   EndDo
089200040317
089300040317     c                   EndSr
089400040413
089500040413      *------------------------------------------------------------------------*
089600040413      * PULISCO I DATI DEL VIDEO
089700040413      *------------------------------------------------------------------------*
089800040413     c     Sr_PulD02     BegSr
089900040413
090000040413     c                   Clear                   V2cLnp
090100040413     c                   Clear                   V2cNrs
090200040413     c                   Clear                   V2cNsp
090300040413     c                   Z-add     *year         V2cAas
090400040413     c                   Clear                   V2cCbo
090500040413     c                   Clear                   V2dCbo
090600040413     c                   Clear                   V2cCcm
090700040413     c                   Clear                   V2cRsm
090800040413     c                   Clear                   V2cInm
090900040413     c                   Clear                   V2cCam
091000040413     c                   Clear                   V2cLom
091100040413     c                   Clear                   V2cPrm
091200040413     c                   Clear                   V2cNzm
091300040413     c                   Clear                   V2cCcd
091400040413     c                   Clear                   V2cRsd
091500040413     c                   Clear                   V2cInd
091600040413     c                   Clear                   V2cCad
091700040413     c                   Clear                   V2cLod
091800040413     c                   Clear                   V2cPrd
091900040413     c                   Clear                   V2cNzd
092000040413     c                   Clear                   V2cDrt
092100040413     c                   Clear                   V2cNclb
092200040413     c                   Clear                   V2cPkb
092300040413     c                   Clear                   V2cVlb
092400040413     c                   Clear                   V2cLna
092500040413     c                   Clear                   V2dLna
092600040413     c                   Clear                   V2cPdr
092700040413     c                   Clear                   V2dPdr
092800040413
092900040413     c                   Eval      wfdar = *Off
093000040413
093100040413     c                   EndSr
093200040317
093300040318      *------------------------------------------------------------------------*
093400040318      * - CONTROLLI VIDEATA ABBINAMENTO ORM CON BOLLA
093500040318      *------------------------------------------------------------------------*
093600040318     c     Sr_CtrD02     BegSr
093700040318
093800040318      * Numero bolla obbligatorio
093900040318     c                   If        V2cNsp = *Zeros
094000040318     c                   Eval      *In28 = *On
094100040318     c                   Eval      *In42 = *On
094200040318     c                   Eval      V2cMsg = msg(07)
094300040318     c                   GoTo      EndD02
094400040318     c                   EndIf
094500040318      * Bolla esistente
094600070911     c     kBlp          Chain(N)  Fnblp01l
094700040318     c                   If        Not %Found(Fnblp01l)
094800040318     c                   Eval      *In28 = *On
094900040318     c                   Eval      *In42 = *On
095000040318     c                   Eval      V2cMsg = msg(08)
095100040318     c                   GoTo      EndD02
095200040318     c                   EndIf
095300040318      * Non deve essere già abbinata ad un ORM
095400060214     c     kar4          Setll     Fiar401l
095500060214     c                   If        %Equal(Fiar401l)
095600040318     c                   Eval      *In28 = *On
095700040318     c                   Eval      *In42 = *On
095800040318     c                   Eval      V2cMsg = msg(09)
095900040318     c                   GoTo      EndD02
096000040318     c                   EndIf
096100040318      * Non deve essere una bolla di recupero
096200040325     c                   Clear                   ds3a
096300040325     c                   Eval      kkey = BlpCbo
096400040325     c     kTab          Chain     Tabel00f
096500040325     c                   If        %Found(Tabel00f)
096600040325     c                   Eval      ds3a = TblUni
096700040325     c                   EndIf
096800040325     c                   If        §3aRbl = 'R'
096900040318     c                   Eval      *In28 = *On
097000040318     c                   Eval      *In42 = *On
097100040318     c                   Eval      V2cMsg = msg(11)
097200040318     c                   GoTo      EndD02
097300040325     c                   EndIf
097400040325      * Carico i dati della bolla
097500040325     c                   ExSr      Sr_CarBolla
097600040325
097700040325      * Se l'ORM ha la serie il tipo bolla deve essere uguale a quello in tabella OSR
097800040325     c                   If        V2cNsr <> *Zeros and V2cCbo <> d§OsrCbo
097900040331     c                   Move      d§OsrCbo      w002a
098000040325     c                   Eval      *In28 = *On
098100040325     c                   Eval      *In42 = *On
098200040325     c                   Eval      V2cMsg = msg(12)
098300040325     c                   Eval      V2cMsg = %replace(w002a:V2cMsg:
098400040325     c                                      %scan('__':V2cMsg))
098500040325     c                   GoTo      EndD02
098600040325     c                   EndIf
098700040325
098800040330      * Se ORM è prepagato la bolla deve essere in Franco e avere data fattura > 0
098900040325     c                   If        OrmTor = 'P' and wtb1 <> 'F'
099000040325     c                   Eval      *In28 = *On
099100040325     c                   Eval      *In42 = *On
099200040325     c                   Eval      V2cMsg = msg(13)
099300040325     c                   GoTo      EndD02
099400040325     c                   EndIf
099500040325     c                   If        OrmTor = 'P'
099600040330     c                   Eval      Ar6Trc = '1'
099700040330     c     kAr6          Chain     Fiar601l
099800040330     c                   If        %Found(Fiar601l) and Ar6Dft = *Zeros
099900040325     c                   Eval      *In28 = *On
100000040325     c                   Eval      *In42 = *On
100100040330     c                   Eval      V2cMsg = msg(13)
100200040325     c                   GoTo      EndD02
100300040325     c                   EndIf
100400040325     c                   EndIf
100500040325
100600040325      * Se l'ORM richiede la bolla in Assegnato non posso abbinare una bolla Franco
100700040325     c                   If        §OrPfb = 'A' and wtb1 = 'F'
100800040325     c                   Eval      *In28 = *On
100900040325     c                   Eval      *In42 = *On
101000040326     c                   Eval      V2cMsg = msg(15)
101100040325     c                   GoTo      EndD02
101200040325     c                   EndIf
101300040318
101400040325      * La linea di partenza della bolla deve essere uguale al p.o. ritiro
101500040325     c                   If        OrmPor <> BlpLnp
101600040325     c                   Eval      *In28 = *On
101700040325     c                   Eval      *In42 = *On
101800040325     c                   Eval      V2cMsg = msg(16)
101900040325     c                   GoTo      EndD02
102000040325     c                   EndIf
102100040325
102200040325      * Se l'ORM ha il p.o. consegna la bolla deve avere la linea di arrivo uguale
102300040325     c                   If        OrmPoc > *Zeros and OrmPoc <> BlpLna
102400040325     c                   Eval      *In28 = *On
102500040325     c                   Eval      *In42 = *On
102600040325     c                   Eval      V2cMsg = msg(17)
102700040325     c                   GoTo      EndD02
102800040325     c                   EndIf
102900040325
103000040325      * se l'ORM prevede:
103100040325      *               --> la bolla in porto Assegnato
103200040325      *               --> paga ordinante orm
103300040325      *               --> destinatario orm estero
103400040325      *               --> ordinante orm italia
103500040325     c                   If        (§OrPfb = 'A' and OrmPag = 'O' and
103600040325     c                              OrmNac <> *Blanks and OrmNao = *Blanks)
103700040325      * la bolla deve essere un porto franco
103800040325     c                              and wtb1 = 'A'
103900040325     c                   Eval      *In28 = *On
104000040325     c                   Eval      *In42 = *On
104100040325     c                   Eval      V2cMsg = msg(18)
104200040325     c                   GoTo      EndD02
104300040325     c                   EndIf
104400040326
104500040331      * Il p.o. emissione dell'ORM è DPD, FedEx, EEX
104600040331     c                   Clear                   og143
104700040331     c     Ormpoe        Chain     Azorg01l
104800040331     c                   If        %Found(Azorg01l)
104900040331     c                   Eval      og143 = OrgDe3
105000040331     c                   EndIf
105100040326     c                   If        (§OgNtw = 'DPD' or §OgNtw = 'FED' or
105200040331     c                              §OgNtw = 'EEX')
105300040331      * La bolla abbinata deve essere una bolla export DPD, FedEx, EEX in porto assegnato
105400040331     c                   If        wtb1 <> 'A' or BlpLna <> OrmPoe
105500040326     c                   Eval      *In28 = *On
105600040326     c                   Eval      *In42 = *On
105700040326     c                   Eval      V2cMsg = msg(19)
105800040326     c                   Eval      V2cMsg = %replace(§OgNtw:V2cMsg:
105900040326     c                                      %scan('___':V2cMsg))
106000040331     c                   Eval      %Subst(V2cMsg:53:3) = §OgNtw
106100040326     c                   GoTo      EndD02
106200040326     c                   EndIf
106300040326     c                   EndIf
106400040326
106500040326      * Controllo formale tra la data ritiro ORM e la data ritiro merce
106600070911     c                   Clear                   og143
106700070920     c                   Clear                   wimport
106800070911     c     ormpor        Chain     Azorg01l
106900070911     c                   If        %Found(Azorg01l)
107000070911     c                   Eval      og143 = OrgDe3
107100070920     c                   if        §ogntw='DPD' or §ogntw='EEX' or
107200070920     c                             §ogntw='FED'
107300070920     c                   eval      wimport='S'
107400070920     c                   endif
107500070911     c                   EndIf
107600071107     c****               if        dateu<§vpodrt
107700071107     c***                If        wfdar = *Off and OrmDar <> BlpDrt
107800071107     c***                Eval      *In28 = *On
107900071107     c***                Eval      *In42 = *On
108000071107     c***                Eval      V2cMsg = msg(20)
108100071107     c***                Eval      wfdar = *On
108200071107     c****               GoTo      EndD02
108300071107     c****               EndIf
108400071107     c****               else
108500070907     c                   If        wfdar = *Off and w_Dar <> BlpDrt
108600070920     c                             and wimport=*blanks
108700070907     c                   Eval      *In28 = *On
108800070907     c                   Eval      *In42 = *On
108900070907     c                   Eval      V2cMsg = msg(22)
109000070907     c                   Eval      wfdar = *On
109100070907     c                   GoTo      EndD02
109200070907     c                   EndIf
109300071107     c*****              endif
109400051114
109500051114      * Controllo se la spedizione è stata fatta da + di 10 giorni
109600071107    1c****               if        dateu<§vpodrt
109700071107    2c***                If        blpdsp < wdata
109800071107     c***                Eval      *In28 = *On
109900071107     c***                Eval      *In42 = *On
110000071107     c***                Eval      V2cMsg = msg(21)
110100071107     c***                GoTo      EndD02
110200071107    2c***                EndIf
110300071107   x1c****               else
110400070910     c* Se bolla già partita:
110500070910      * Errore se la spedizione è stata fatta da + di 1 giorno lav.
110600070920    2c                   if        blpft2<>*blanks and blpdsp<wdata
110700070907     c                   Eval      *In28 = *On
110800070907     c                   Eval      *In42 = *On
110900070907     c                   Eval      V2cMsg = msg(23)
111000070907     c                   GoTo      EndD02
111100070920    2c                   endif
111200070920     c* Se cambia la data ritiro:
111300070910      * controllo che per la data ritiro i conteggi non siano già stati
111400070910      * confermati. Se la spedizione è già partita è anche necessario che
111500070910      * i conteggi siano presenti
111600070920    2c                   if        w_dar<>blpdrt
111700070920     c                             and wimport=*blanks
111800070910     C                   MOVEL     ' '           W01TSR
111900070910     C                   MOVEL     'C'           W01FVL
112000070910     C     KFTT          SETLL     FIFTT000                               90
112100070910     C  N90KFTT          SETLL     FIFST000                               90
112200070910     C*
112300070920    3C                   IF        *in90
112400070910     c                   Eval      *In28 = *On
112500070910     c                   Eval      *In42 = *On
112600070910     C                   eval      v2cmsg= msg(24)
112700070910     C                   goto      endd02
112800070920    3C                   endif
112900070919     c* ripeto il controllo per la vecchia data ritiro
113000070919     C                   MOVEL     ' '           W01TSR
113100070919     C                   MOVEL     'C'           W01FVL
113200070919     C     KFTTold       SETLL     FIFTT000                               90
113300070919     C  N90KFTTold       SETLL     FIFST000                               90
113400070919     C*
113500070920    3C                   IF        *in90
113600070919     c                   Eval      *In28 = *On
113700070919     c                   Eval      *In42 = *On
113800070919     C                   eval      v2cmsg= msg(24)
113900070919     C                   goto      endd02
114000070920    3C                   endif
114100070920    3c                   if        blpft2<>*blanks
114200070919     c                   exsr      sr_ctrftt
114300070919     c   28              goto      endd02
114400070920    3c                   endif
114500070920    2c                   endif
114600070910     c
114700071107    1c*****              endif
114800040325
114900040318     c     EndD02        EndSr
115000040325
115100040325      *------------------------------------------------------------------------*
115200040325      * - CARICA I DATI DELLA BOLLA
115300040325      *------------------------------------------------------------------------*
115400040325     c     Sr_Carbolla   BegSr
115500040325
115600040325      * Tipo bolla
115700040325     c                   Clear                   V2dCbo
115800040325     c                   Eval      V2cCbo = BlpCbo
115900040325     c                   Eval      V2dCbo = §3aDes
116000040325     c                   Movel     §3aTb1        wtb1
116100040325
116200040325      * Mittente
116300040325      * se bolla Franco prendo KSC
116400040325     c                   If        wtb1 = 'F'
116500040325     c                   Eval      V2cCcm = BlpKsc
116600040325     c                   EndIf
116700040325      * se bolla Assegnato prendo CCM
116800040325     c                   If        wtb1 = 'A'
116900040325     c                   Eval      V2cCcm = BlpCcm
117000040325     c                   EndIf
117100040325     c                   Eval      V2cRsm = BlpRsm
117200040325     c                   Eval      V2cInm = BlpInm
117300040325     c                   Eval      V2cCam = BlpCam
117400040325     c                   Eval      V2cLom = BlpLom
117500040325     c                   Eval      V2cPrm = BlpPrm
117600040325     c                   Eval      V2cNzm = BlpNzm
117700040325
117800040325      * Destinatario
117900040325      * se bolla Assegnato prendo KSC
118000040325     c                   If        wtb1 = 'A'
118100040325     c                   Eval      V2cCcd = BlpKsc
118200040325      * Cerco anche su Fiar6
118300040326     c                   Eval      Ar6Trc = '1'
118400040325     c     kAr6          Chain     Fiar601l
118500040325     c                   If        %Found(Fiar601l) and Ar6Ksc > *Zeros
118600040325     c                   Eval      V2cCcd = Ar6Ksc
118700040325     c                   EndIf
118800040325     c                   EndIf
118900040325      * se bolla Franco pulisco il codice
119000040325     c                   If        wtb1 = 'F'
119100040325     c                   Clear                   V2cCcd
119200040325     c                   EndIf
119300040325     c                   Eval      V2cRsd = BlpRsd
119400040325     c                   Eval      V2cInd = BlpInd
119500040325     c                   Eval      V2cCad = BlpCad
119600040325     c                   Eval      V2cLod = BlpLod
119700040325     c                   Eval      V2cPrd = BlpPrd
119800040325     c                   Eval      V2cNzd = BlpNzd
119900040325
120000040325      * Data ritiro merce
120100040325     c                   Z-add     BlpDrt        g02inv
120200040325     c                   Movel     '3'           g02err
120300040325     c                   Call      'XSRDA8'
120400040325     c                   Parm                    wlbdat
120500040325     c                   Z-add     g02dat        V2cDrt
120600070913     c                   z-add     blpdrt        w_olddrt
120700040325
120800040325      * Colli - Peso - Volume
120900040325     c                   Eval      V2cnclb = BlpNcl
121000040325     c                   Eval      V2cpkb = BlpPkb
121100040325     c                   Eval      V2cvlb = BlpVlb
121200040325
121300040325      * Linea di arrivo
121400040325     c                   Clear                   V2dLna
121500040325     c                   Eval      V2cLna = BlpLna
121600040325     c     BlpLna        Chain     Azorg01l
121700040325     c                   If        %Found(Azorg01l)
121800040325     c                   Eval      V2dLna = OrgDes
121900040325     c                   EndIf
122000040325
122100040325      * Autotrasportatore
122200040325     c                   Clear                   V2dPdr
122300070924     c                   Clear                   wpdd
122400040325     c                   Eval      V2cPdr = BlpPdr
122500040325     c                   Eval      ApdPdr = BlpPdr
122600040325     c                   Eval      ApdTip = 'A'
122700040325     c     kApd          Chain     Fiapd01L
122800040325     c                   If        %Found(Fiapd01l)
122900040325     c                   Eval      V2dPdr = ApdRsc
123000070924     c                   if        apdpdd='S'
123100070924     c                   eval      wpdd='S'
123200070924     c                   endif
123300040325     c                   EndIf
123400040325
123500040325     c                   EndSr
123600040318
123700040318      *------------------------------------------------------------------------*
123800040318      * - ABBINA ORM ALLA BOLLA
123900040318      *------------------------------------------------------------------------*
124000040318     c     Sr_AbbOrm     BegSr
124100040615
124200070920     c* richaino blp per vedere se nel frattempo la bolla è partita
124300070919     c                   move      blpft2        savft2
124400070920     c     kblp          chain(N)  fnblp01l
124500070919     c* Bolla nel frattempo partita: devo eseguire i controlli per
124600070919     c* bolla partita
124700071107     c****               if        dateu>=§vpodrt and  w_dar<>blpdrt
124800071107     c                   if        w_dar<>blpdrt
124900070920     c                             and wimport=*blanks
125000070920     c                             and savft2=*blanks and savft2<>blpft2
125100070919     c                   exsr      sr_ctrftt
125200070919     c   28              goto      endabbina
125300070919     c                   endif
125400070919     c*
125500040615s   0c                   Select
125600040615      * Se bolla non partita e non borderizzata richiamo la manutenzione bolle
125700040615w   0c                   When      BlpFt2 = *Blanks and BlpDbr = *Zeros
125800040615      * richiamo il programma per la scelta delle stampanti
125900040615     c                   Eval      wdatibolla = 'BollaAdd.'
126000040615     c                   Eval      wdatibolA4 = 'BolAdd_A4'
126100040615     c                   Eval      wdatibolA5 = 'BolAdd_A5'
126200040615     c                   exsr      sr_scestp
126300040615     c   92              goto      endabbina
126400040615
126500040615     c                   clear                   kpjbu
126600040615     c                   Clear                   dblp
126700040615     c                   Clear                   dtasv
126800040615     c                   Eval      §LpAas = V2cAas
126900040615     c                   Eval      §LpLnp = V2cLnp
127000040615     c                   Eval      §LpNrs = V2cNrs
127100040615     c                   Eval      §LpNsp = V2cNsp
127200040615     c                   Eval      §LpFpr = 'M'
127300040615     c                   Call      'FNLS01R'
127400040615     c                   Parm                    kpjba
127500040615     c                   Parm                    dblp
127600040615     c                   Parm                    dtasv
127700040615     c                   Parm                    Trul90ds
127800040615     c                   if        §lpfpr = '4' or §lpfpr = '3'
127900040615     c                             or §lpfpr = '5'
128000040615     c                   eval      wabb = *Off
128100040615     c                   else
128200040615     c                   eval      wabb = *On
128300040615     c                   endif
128400040615
128500060214      * Se bolla borderizzata e non partita aggiorno in automatico solo Fiar4
128600070911      * a partire dalla data decorrenza per data ritiro aggiorno anche
128700070911      * la data ritiro
128800040615w   0c                   When      BlpFt2 = *Blanks and BlpDbr > *Zeros
128900070911      * Aggiorno la data ritiro su fnblp
129000071107     c****               if        dateu>=§vpodrt and blpdrt<>w_dar
129100071107     c                   if        blpdrt<>w_dar
129200070920     c                             and wimport=*blaNKS
129300070911     c                   exsr      sr_aggdrt
129400070920     c   28              goto      endabbina
129500070920     c                   endif
129600060214      * Scrivo Fiar4 rcd 'M'
129700040615     c                   Clear                   Fnlv19ds
129800040615     c                   Clear                   dsbl4m
129900060214     c**!!!              Eval      D19Tbo = 'P'
130000060215     c                   Eval      d19ftr = 'T'
130100040615     c                   Eval      D19Aas = V2cAas
130200040615     c                   Eval      D19Lnp = V2cLnp
130300040615     c                   Eval      D19Nrs = V2cNrs
130400040615     c                   Eval      D19Nsp = V2cNsp
130500040615     c                   Eval      §b4Poe = V2cPoe
130600040615     c                   Eval      §b4Nsr = V2cNsr
130700040615     c                   Eval      §b4Nor = V2cNor
130800040615     c                   Eval      §b4Nrv = V2cNrv
130900040615     c                   Eval      D19Trc = 'M'
131000040615     c                   Eval      D19Nt1 = dsBl4m
131100040615     c                   Call      'FNLV19R'
131200040615     c                   Parm                    Fnlv19ds
131300040615     c                   eval      wabb = *On
131400040615
131500040615      * Se bolla partita aggiorno in automatico
131600040615   x0c                   Other
131700070920      *
131800071107    1c****               if        dateu>=§vpodrt and blpdrt<>w_dar
131900071107    1c                   if        blpdrt<>w_dar
132000070924     c                             and wimport=*blanks and wpdd<>'S'
132100070920      * Aggiorno la data ritiro su fnblp
132200070912     c                   exsr      sr_aggdrt
132300070920     c   28              goto      endabbina
132400070914      * aggiorno i conteggi (richiamo due volte:la seconda per le simulazioni)
132500070914     c                   setoff                                       17
132600070920    2c                   do        2
132700070912     c                   exsr      sr_aggaut
132800070914     c                   seton                                        17
132900070920    2c                   enddo
133000070912      * Aggiorno la data ritiro su titas
133100070912     c                   exsr      sr_aggdrttas
133200070912      * ricalcolo l'affidabilità
133300070912     c                   exsr      sr_ricaff
133400070920    1c                   endif
133500040615
133600060214      * Scrivo Fiar4 rcd 'M'
133700040318     c                   Clear                   Fnlv19ds
133800040326     c                   Clear                   dsbl4m
133900060214     c**!!!              Eval      D19Tbo = 'P'
134000060215     c                   Eval      d19ftr = 'T'
134100040318     c                   Eval      D19Aas = V2cAas
134200040318     c                   Eval      D19Lnp = V2cLnp
134300040318     c                   Eval      D19Nrs = V2cNrs
134400040318     c                   Eval      D19Nsp = V2cNsp
134500040318     c                   Eval      §b4Poe = V2cPoe
134600040318     c                   Eval      §b4Nsr = V2cNsr
134700040318     c                   Eval      §b4Nor = V2cNor
134800040318     c                   Eval      §b4Nrv = V2cNrv
134900040318     c                   Eval      D19Trc = 'M'
135000040318     c                   Eval      D19Nt1 = dsBl4m
135100110322     c
135200110322      * Aggiorno flag PDA su fiar5  prima di aggiornare fiar4
135300110322     c                   exsr      AggioAR5PDA
135400110322     c*
135500040318     c                   Call      'FNLV19R'
135600040318     c                   Parm                    Fnlv19ds
135700040326
135800040326      * Scrivo Fnar4 rcd 'M'
135900060214     c**!!!              Eval      D19Tbo = 'A'
136000060214     c**!!!              Call      'FNLV19R'
136100060214     c**!!!              Parm                    Fnlv19ds
136200040326
136300040326      * Apro il Tita4 della sede
136400040326     c                   If        Not %Open(Tita400f)
136500040615     c                   Eval      lenght = 80
136600040326     c                   Clear                   comman
136700040326     c                   Movel(p)  cmd(1)        comman
136800040326     c                   Eval      %Subst(comman:30:10) = wlib
136900040330     c                   Eval      comman =
137000040330     c                             %trim(comman) + '/TITA400F)'
137100040326     c                   Call      'QCMDEXC'                            99
137200040326     c                   Parm                    comman
137300040326     c                   Parm                    lenght
137400040326     c  n99              Open      Tita400f
137500040326     c                   EndIf
137600040326      * Scrivo Tita4 rcd 'M'
137700040326     c                   If        Not *In99
137800040326     c                   Clear                   Tita4000
137900040326     c                   Eval      Ta4Aas = V2cAas
138000040326     c                   Eval      Ta4Lnp = V2cLnp
138100040326     c                   Eval      Ta4Nrs = V2cNrs
138200040326     c                   Eval      Ta4Nsp = V2cNsp
138300040326     c                   Eval      Ta4Trc = D19Trc
138400040326     c                   Eval      Ta4Not = D19Nt1
138500040326     c                   Write     Tita4000
138600040326     c                   EndIf
138700040318
138800040615      * L'Orm lo passo alla fase 900
138900040413if  1c                   If        wabb = *Off
139000040326     c     kOrm          Chain     Fnorm01l
139100040413if  2c                   If        %Found(Fnorm01l)
139200040615     c                   Eval      OrmFao = 900
139300040326     c                   Z-add     *Date         OrmDfo
139400040326     c                   Time                    OrmOfo
139500040908     c                   Z-add     *Date         OrmDtt
139600070710      * imposto ormeti
139700070710     c                   clear                   dfar
139800070710     c                   clear                   tibs02ds
139900070710     c                   eval      t02mod = 'C'
140000070710     c                   eval      t02sif = knsif
140100070710     c                   eval      t02cod = 'FAR'
140200070710     c                   movel(p)  ormfao        t02ke1
140300070710     c                   call      'TIBS02R'
140400070710     c                   parm                    kpjba
140500070710     c                   parm                    tibs02ds
140600070710     c                   eval      dfar = t02uni
140700070710     c                   eval      ormeti = d§farass
140800040326      * Scrivo la nuova fase
140900040326     c                   ExSr      Sr_WrtOrf
141000040326      * Aggiorno l'ORM
141100040326     c                   Update    Fnorm01
141200040413     c                   Eval      wabb = *On
141300040413e   2c                   EndIf
141400040413
141500040326e   1c                   EndIf
141600040615
141700040615e   0c                   EndSl
141800040318
141900040615     c     endabbina     EndSr
142000040326
142100070919      *------------------------------------------------------------------------*
142200070919      * - Controllo presenza conteggi per bolla già partita
142300070919      *------------------------------------------------------------------------*
142400070919     c     Sr_CtrFtt     BegSr
142500070919     c                   eval      w01tsr='R'
142600070919    2C     blpfpp        IFEQ      'M'
142700070919     C                   Z-ADD     1000          W_ndc
142800070919     C                   ELSE
142900070919     C                   Z-ADD     5000          W_ndc
143000070919    2C                   ENDIF
143100070919     C                   ADD       blpnrt        W_ndc
143200070919     c
143300070919     c     kftt1         setll     fiftt001
143400070919     c                   if        not %equal(fiftt01l)
143500070919     c                   Eval      *In28 = *On
143600070919     c                   Eval      *In42 = *On
143700070919     c                   Eval      V2cMsg = msg(25)
143800070919     c                   endif
143900070919     c                   endsr
144000070911      *------------------------------------------------------------------------*
144100070911      * - Aggiornamento data ritiro su FNBLP
144200070911      *------------------------------------------------------------------------*
144300070911     c     Sr_AggDrt     BegSr
144400070911
144500070920     c     kblp          chain(E)  fnblp01l
144600070920     c                   if        %error
144700070920     c                   eval      *in28='1'
144800070920     c                   Eval      *In42 = *On
144900070920     c                   Eval      V2cMsg = msg(26)
145000070920     c                   goto      enddrt
145100070920     c                   endif
145200090403     c
145300090403     c* Richiamo il pgm per la rielaborazione delle statistiche
145400090403     c                   z-add     blpncl        st2ncl
145500090403     C                   MOVE      blpdrt        SA2DTS            8 0
145600090403     C                   MOVE      'P'           SA2TRC            1
145700090403     C                   exsr      sr_callsa2r3
145800090403     C                   MOVE      'T'           SA2TRC            1
145900090403     C                   exsr      sr_callsa2r3
146000090403     c
146100090403     C                   MOVE      w_dar         SA2DTS            8 0
146200090403     C                   MOVE      'P'           SA2TRC            1
146300090403     C                   exsr      sr_callsa2r3
146400090403     C                   MOVE      'T'           SA2TRC            1
146500090403     C                   exsr      sr_callsa2r3
146600090403     c
146700070911     c                   z-add     w_dar         blpdrt
146800070912     c                   except    aggblp
146900070911
147000070911     c     enddrt        endsr
147100110322      *------------------------------------------------------------------------*
147200110322      * - Aggiornamento flag PDA orm su fiar5
147300110322      *------------------------------------------------------------------------*
147400110322     c     AggioAR5PDA   BegSr
147500110322     c                   eval      RitiroPDA=' '
147600110322     c
147700110322     c     korg          chain     fnorg01l
147800110322    2c                   if        %found(fnorg01l) and R_orgatb=' '
147900110322     c                   eval      kfgs=r_orgfgs
148000110322     c                   eval      knfv=r_orgndc
148100110322     c
148200120612     c     kdst          chain     fidst01l
148300120612    3c                   if        %found(fidst01l) and dstatb=' '
148400110322
148500120612    4c                   if        dstpda='O' or dstpda='E'
148600120612     c                   eval      DDSTFLR =  DstFLR
148700110322    5c                   if        §dstTSTPDA=' ' or §dstTSTPDA='C'
148800110322     c                   eval      RitiroPDA='S'
148900110322    5c                   endif
149000110322    4c                   endif
149100110322     c
149200110322    3c                   endif
149300110322    2c                   endif
149400110322
149500110322     c     kar5          chain     fiar501l
149600110322    1c                   if        %found(fiar501l)
149700110322     c                   movel     ar5uni        dar5gen
149800110322     c* Aggiorno solo se diverso
149900110322    2c                   if        §ar5PDArt<>RitiroPDA
150000110322     c                   eval      §ar5PDArt=RitiroPDA
150100110322     c
150200110322     c                   movel     dar5gen       ar5uni
150300110322     c                   clear                   ar5dt2
150400110324     c                   clear                   ar5ft2
150500110324     c                   clear                   ar5ft3
150600110322     c                   update    fiar5000
150700110322   x2c                   else
150800110322     c                   unlock    fiar501l
150900110322    2c                   endif
151000110322     c
151100110322   x1c                   else
151200110322
151300110322     c* Scrivo solo se = 'Si'
151400110322    2c                   if        ritiroPDA='S'
151500110322     c                   clear                   fiar5000
151600110322     c                   eval      ar5aas=blpaas
151700110322     c                   eval      ar5lnp=blplnp
151800110322     c                   eval      ar5nrs=blpnrs
151900110322     c                   eval      ar5nsp=blpnsp
152000110322     c                   eval      ar5trd='GEN'
152100110322     c                   z-add     *Date         ar5dac
152200110322     c                   time                    ar5orc
152300110322     c                   clear                   dar5gen
152400110322     c                   eval      §ar5PDArt='S'
152500110322     c                   movel     dar5gen       ar5uni
152600110324     c                   clear                   ar5ft2
152700110324     c                   clear                   ar5ft3
152800110322      *
152900110322     C                   WRITE     FIAR5000                             38
153000110322e   2c                   ENDIF
153100110322e   1c                   ENDIF
153200110322     c
153300110322
153400110322     c                   endsr
153500090403      *--------------------------------------------------------------*
153600090403      * Richiamo pgm per rielaborazione statistiche partito e a terra
153700090403      *--------------------------------------------------------------*
153800090403     c     sr_callsa2r3  BegSr
153900090403     C                   CALL      'FNLSA2R3'
154000090403     C                   PARM                    SA2TLA            1
154100090403     C                   PARM                    SA2TRC
154200090403     C                   PARM                    SA2DTS
154300090403     C                   PARM                    WWLNA             3 0
154400090403     C                   PARM                    st2ncl            5 0
154500090403     c                   ENDSR
154600070912      *------------------------------------------------------------------------*
154700070912      * - Aggiornamento data ritiro su TITAS
154800070912      *------------------------------------------------------------------------*
154900070912     c     Sr_AggDrtTas  BegSr
155000070912
155100070912      * Apro il Titas della sede
155200070912    2c                   If        Not %Open(Titas30c)
155300070912     c                   Eval      lenght = 80
155400070912     c                   Clear                   comman
155500070912     c                   Movel(p)  cmd(2)        comman
155600070912     c                   Eval      %Subst(comman:30:10) = wlib
155700070912     c                   Eval      comman =
155800070912     c                             %trim(comman) + '/TITAS30C)'
155900070912     c                   Call      'QCMDEXC'                            98
156000070912     c                   Parm                    comman
156100070912     c                   Parm                    lenght
156200070912     c  n98              Open      TITAS30C
156300070912    2c                   EndIf
156400070914      *
156500070912    2c                   If        Not *In98
156600070912     c     kblp          chain(E)  titas30c
156700070912    3c                   if        %error
156800070919     c* reperisco il RRN del file
156900070920     c     kblp          chain(n)  titas30c
157000070919     C* Valorizzo la DS di procedura
157100070920     C                   CLEAR                   TRUL82DS
157200070920     c                   eval      ul82§rrn = tasnrr
157300070920     C   22              EVAL      UL82§FIL = 'TITAS00F'
157400070920     C   23              EVAL      UL82§FIL = 'TITAS10F'
157500070920     C                   EVAL      UL82§num = 1
157600070920     C                   EVAL      UL82§att = 2
157700070919     C* Effettuo la chiamata al *PGM d utilità
157800070920     C                   CALL(e)   'TRUL82R'
157900070920     C                   PARM                    TRUL82DS
158000070919     c     kblp          chain     titas30c
158100070912    3c                   endif
158200070912     c                   z-add     w_dar         tasdrt
158300070912     c   22              except    agtas0
158400070912     c   23              except    agtas1
158500070912    2c                   endif
158600070912
158700070912     c     enddrttas     endsr
158800070912      *------------------------------------------------------------------------*
158900070913      * - Aggiornamento conteggi autotrasportatori
159000070912      *------------------------------------------------------------------------*
159100070912     c     Sr_AggAut     BegSr
159200070913     c* NUOVA DATA RITIRO
159300070919     c  n17kftt1         chain(E)  fiftt001                           30
159400070919     c   17kftt1         chain(E)  fifst001                           30
159500070919     c* se errore ritento per una volta
159600070919     c                   if        %error
159700070920     c* reperisco il RRN del file
159800070920     c  n17kftt1         chain(n)  fiftt001
159900070920     c     kftt1         chain(n)  fifst001
160000070920     C* Valorizzo la DS di procedura
160100070920     C                   CLEAR                   TRUL82DS
160200070920     c  n17              eval      ul82§rrn = fttnrr
160300070920     c   17              eval      ul82§rrn = fstnrr
160400070920     C  n17              EVAL      UL82§FIL = 'FIFTT00F'
160500070920     C   17              EVAL      UL82§FIL = 'FIFST00F'
160600070920     C                   EVAL      UL82§num = 1
160700070920     C                   EVAL      UL82§att = 2
160800070920     C* Effettuo la chiamata al *PGM d utilità
160900070920     C                   CALL(e)   'TRUL82R'
161000070920     C                   PARM                    TRUL82DS
161100070919     c  n17kftt1         chain     fiftt001                           30
161200070919     c   17kftt1         chain     fifst001                           30
161300070919     c                   endif
161400070914    1c                   if        *in30='0' and fttfvl<>'C'
161500070919     c  n17kftd5         chain(E)  fiftd05l                           30
161600070919     c   17kftd5         chain(E)  fifsd05l                           30
161700070919     c                   if        %error
161800070920     c* reperisco il RRN del file
161900070920     c  n17kftd5         chain(n)  fiftd05l
162000070920     c     kftd5         chain(n)  fifsd05l
162100070920     C* Valorizzo la DS di procedura
162200070920     C                   CLEAR                   TRUL82DS
162300070920     c  n17              eval      ul82§rrn = ftdnrr
162400070920     c   17              eval      ul82§rrn = fsdnrr
162500070920     C  n17              EVAL      UL82§FIL = 'FIFTD00F'
162600070920     C   17              EVAL      UL82§FIL = 'FIFSD00F'
162700070920     C                   EVAL      UL82§num = 1
162800070920     C                   EVAL      UL82§att = 2
162900070920     C* Effettuo la chiamata al *PGM d utilità
163000070920     C                   CALL(e)   'TRUL82R'
163100070920     C                   PARM                    TRUL82DS
163200070919     c  n17kftd5         chain     fiftd05l                           30
163300070919     c   17kftd5         chain     fifsd05l                           30
163400070919     c                   endif
163500070914    2c                   if        *in30='0'
163600070914     c* numero di stop
163700070914     c                   clear                   fnlva7ds
163800070914     c                   eval      ia7pdr=ftdpdr
163900070914     c                   eval      ia7ndc=ftdndc
164000070914     c                   eval      ia7ddc=w_dar
164100070914     c                   eval      ia7ksc=ftdksc
164200070914     c                   eval      ia7rsm=ftdrsc
164300070914     c   17              eval      ia7sml='S'
164400081027     c                   call      'FNLVA7C'
164500070914     c                   parm                    kpjba
164600070914     c                   parm                    fnlva7ds
164700070914     c                   z-add     oa7stp        ftdstp
164800070914     c* data distinta
164900070913     c                   z-add     w_dar         ftdddc
165000070919     c  n17              update    fiftd000
165100070919     c   17              update    fifsd000
165200070914     c* numero spedizioni ritirate
165300070913     c                   eval      fttset=fttset+1
165400070914     c  n17              update    fiftt001
165500070914     c   17              update    fifst001
165600070913     c* se conteggio valorizzato svalorizzo
165700070914    3C     fttfvl        ifeq      'V'
165800070913     C                   MOVEL     FTDPDR        PA3PDR                         *COD. PADRONCINO
165900070913     C                   MOVEL     FTDTSR        PA3TSR                         *TIPO PRESTAZIONE
166000070913     C                   Z-ADD     FTDNDC        PA3NDC                         *NUMERO DISTINTA
166100070913     C                   Z-ADD     FTDDDC        PA3DDC                         *DATA   DISTINTA
166200070913     C                   MOVEL     'N'           PA3FCT                         *FLG COD.TARIFFA
166300070913     C                   MOVEL     'N'           PA3FVD                         *FLG VAL.MANUAL.DETT
166400070913     C                   MOVEL     'N'           PA3FVT                         *FLG VAL.MANUAL.TEST
166500070913     C  n17              MOVEL     ' '           PA3sml                         *FLG VAL.MANUAL.TEST
166600070913     C   17              MOVEL     'S'           PA3sml                         *FLG VAL.MANUAL.TEST
166700070913     C                   MOVEL     PARAM3        KPJBU
166800070913     C  N17              CALL      'FICNE2R'
166900070913     C                   PARM                    KPJBA
167000070913     C   17              CALL      'FICNE9C'
167100070913     C                   PARM                    KPJBA
167200070914    3C                   endif
167300070914    2c                   endif
167400070914    1c                   endif
167500070913     c* VECCHIA DATA RITIRO
167600070919     c  n17kftt1old      chain(E)  fiftt01l
167700070919     c   17kftt1old      chain(E)  fifst01l
167800070919     c                   if        %error
167900070920     c* reperisco il RRN del file
168000070920     c  n17kftt1old      chain(n)  fiftt001
168100070920     c     kftt1old      chain(n)  fifst001
168200070920     C* Valorizzo la DS di procedura
168300070920     C                   CLEAR                   TRUL82DS
168400070920     c  n17              eval      ul82§rrn = fttnrr
168500070920     c   17              eval      ul82§rrn = fstnrr
168600070920     C  n17              EVAL      UL82§FIL = 'FIFTT00F'
168700070920     C   17              EVAL      UL82§FIL = 'FIFST00F'
168800070920     C                   EVAL      UL82§num = 1
168900070920     C                   EVAL      UL82§att = 2
169000070920     C* Effettuo la chiamata al *PGM d utilità
169100070920     C                   CALL(e)   'TRUL82R'
169200070920     C                   PARM                    TRUL82DS
169300070919     c  n17kftt1old      chain     fiftt01l
169400070919     c   17kftt1old      chain     fifst01l
169500070919     c                   endif
169600070914     c                   if        *in30='0'  and fttfvl<>'C'
169700070914     c                   select
169800070919     c*
169900070914     c                   when      fttset-1=0 and ftttbs=0
170000070914     c  n17              delete    fiftt001
170100070914     c   17              delete    fifst001
170200070919     c                   when      fttset-1>0 or fttset-1=0
170300070914     c                   eval      fttset=fttset-1
170400070914     c  n17              update    fiftt001
170500070914     c   17              update    fifst001
170600070914     c* se conteggio valorizzato svalorizzo
170700070914    3C     fttfvl        ifeq      'V'
170800070914     C                   MOVEL     FTtPDR        PA3PDR                         *COD. PADRONCINO
170900070914     C                   MOVEL     FTtTSR        PA3TSR                         *TIPO PRESTAZIONE
171000070914     C                   Z-ADD     FTtNDC        PA3NDC                         *NUMERO DISTINTA
171100070914     C                   Z-ADD     FTtDDC        PA3DDC                         *DATA   DISTINTA
171200070914     C                   MOVEL     'N'           PA3FCT                         *FLG COD.TARIFFA
171300070914     C                   MOVEL     'N'           PA3FVD                         *FLG VAL.MANUAL.DETT
171400070914     C                   MOVEL     'N'           PA3FVT                         *FLG VAL.MANUAL.TEST
171500070914     C  n17              MOVEL     ' '           PA3sml                         *FLG VAL.MANUAL.TEST
171600070914     C   17              MOVEL     'S'           PA3sml                         *FLG VAL.MANUAL.TEST
171700070914     C                   MOVEL     PARAM3        KPJBU
171800070914     C  N17              CALL      'FICNE2R'
171900070914     C                   PARM                    KPJBA
172000070914     C   17              CALL      'FICNE9C'
172100070914     C                   PARM                    KPJBA
172200070914    3C                   endif
172300070914     c                   endsl
172400070914     c                   endif
172500070912     c                   endsr
172600070918      *------------------------------------------------------------------------*
172700070918      * - Aggiornamento bolle arrivi per rialcolo affidabilità
172800070918      *------------------------------------------------------------------------*
172900070918     c     Sr_RicAff     BegSr
173000070918     c     kblp          chain(N)  fnarb01l
173100070918     c                   if        arbdcm>0
173200070918     c     kblp          chain(E)  fnarb01l
173300070918     c                   if        %error
173400070920     C* Valorizzo la DS di procedura
173500070920     C                   CLEAR                   TRUL82DS
173600070920     c                   eval      ul82§rrn = arbnrr
173700070920     C                   EVAL      UL82§FIL = 'FNARB00F'
173800070920     C                   EVAL      UL82§num = 1
173900070920     C                   EVAL      UL82§att = 2
174000070920     C* Effettuo la chiamata al *PGM d utilità
174100070920     C                   CALL(e)   'TRUL82R'
174200070920     C                   PARM                    TRUL82DS
174300070919     c     kblp          chain     fnarb01l
174400070918     c                   endif
174500070918     c                   eval      arbfbs='S'
174600070918     c                   if        arbdce>0
174700070918     c                   clear                   arbdti
174800070918     c                   clear                   arbhti
174900070918     c                   endif
175000070918     c                   update    fnarb000
175100070918     c                   endif
175200070919     c                   endsr
175300040326      *------------------------------------------------------------------------*
175400040326      * - SCRIVO LA FASE DELL'ORM
175500040326      *------------------------------------------------------------------------*
175600040326     c     Sr_WrtOrf     BegSr
175700040326
175800040326     c                   Clear                   Fnorf000
175900040326     c                   Eval      OrfPoe = OrmPoe
176000040326     c                   Eval      OrfNsr = OrmNsr
176100040326     c                   Eval      OrfNor = OrmNor
176200040326     c                   Eval      OrfNrv = OrmNrv
176300040326     c                   Eval      OrfPog = OrmPor
176400040326     c                   Eval      OrfDae = OrmDfo
176500040326     c                   Eval      OrfOre = OrmOfo
176600040326     c                   Eval      OrfFar = OrmFao
176700040326     c                   Eval      OrfPue = knmus
176800040908     c                   If        OrmNdc > *Zeros
176900040908     c                   Eval      OrfFgs = OrmPor
177000040908     c                   Eval      OrfNdc = OrmNdc
177100040908     c                   Eval      OrfDdc = OrmDdc
177200040908     c                   EndIf
177300040326     c                   Write     Fnorf000
177400040326
177500040326     c                   EndSr
177600040615
177700040615      *------------------------------------------------------------------------*
177800040615      * - SCELTA STAMPANTE PER MODIFICA BOLLA
177900040615      *------------------------------------------------------------------------*
178000040615     c     Sr_Scestp     BegSr
178100040615
178200040615     c                   Clear                   kpjbu
178300040615     c                   Eval      *In92 = *Off
178400040615
178500040615      * Richiesta stampanti
178600040615     c                   If        conta = *zeros
178700040615     c                   Clear                   Trul90ds
178800040615     c                   movel     'S'           D90rse
178900040615     c                   movel     'S'           D90rsb
179000040615     c                   call      'TRUL90R'
179100040615     c                   parm                    kpjba
179200040615     c                   parm                    trul90ds
179300040615      * F3=Fine
179400040615     c                   if        D90f3 = '1'
179500040615     c                   eval      *in92 = *on
179600040615     c                   goto      endstp
179700040615     c                   endif
179800040615
179900040615     c                   Eval      lenght = 103
180000040615
180100040615      * Ovrprtf Segnacolli
180200040615     c                   movea     D90pre        cm3(28)
180300040615     c                   movea     D90mde        cm3(50)
180400040615     c                   clear                   comman
180500040615     c                   movea     cm3           comman
180600040615     c                   CALL      'QCMDEXC'                            92
180700040615     c                   PARM                    comman
180800040615     c                   PARM                    lenght
180900040615     c   92              goto      endstp
181000040615
181100040615      * Bolle - Nuovo modulo
181200040615     c                   Eval      lenght = 110
181300040615
181400040615      * Ovrprtf Bolle A4
181500040615     c                   Movea     D90prb4       cma4(30)
181600040615     c                   Movea     D90mdb4       cma4(52)
181700040615     c                   Movea     wdatibolA4    cma4(96)
181800040615     c                   Clear                   Comman
181900040615     c                   Movea     cmA4          Comman
182000040615     c                   Call      'QCMDEXC'                            92
182100040615     c                   Parm                    Comman
182200040615     c                   Parm                    Lenght
182300040615     c   92              goto      endstp
182400040615
182500040615      * Ovrprtf Bolle A5
182600040615     c                   Movea     D90prb5       cmA5(30)
182700040615     c                   Movea     D90mdb5       cmA5(52)
182800040615     c                   Movea     wdatibolA5    cma5(96)
182900040615     c                   Clear                   Comman
183000040615     c                   Movea     cmA5          Comman
183100040615     c                   Call      'QCMDEXC'                            92
183200040615     c                   Parm                    Comman
183300040615     c                   Parm                    Lenght
183400040615     c   92              goto      endstp
183500040615
183600040615     c                   eval      conta = conta +1
183700040615     c                   endif
183800040615
183900040615     c     endstp        EndSr
184000070925      *------------------------------------------------------------------------*
184100070925      * - Reperimento data ritiro in base alla fase orm
184200070925      *------------------------------------------------------------------------*
184300070925     c     Sr_Drt        BegSr
184400070925
184500070925      * data ritiro effettiva
184600070925     C                   clear                   tibs02ds
184700070925     C                   clear                   dfar
184800070925     C                   eval      t02mod='C'
184900070925     C                   eval      t02sif=knsif
185000070925     C                   eval      t02cod='FAR'
185100070925     C                   movel(p)  ormfao        t02ke1
185200070925     C                   call      'TIBS02R'
185300070925     C                   PARM                    kpjba
185400070925     C                   parm                    tibs02ds
185500070925    3C                   if        t02err=*blanks
185600070925     C                   eval      dfar=t02uni
185700070925    3C                   endif
185800070925     c                   clear                   w_dar
185900070925     c* se fase utilizzabile --> data della fase
186000070925    3C     d§fardar      ifeq      'S'
186100070925     C                   Z-ADD     ORMDFO        w_dar
186200070925   x3c                   else
186300070925     c* se l'orm non è in una fase utilizzabile per la data ritiro prendo
186400070925     c* la data ritiro presente sull'orm
186500070925     C                   Z-ADD     ORMDAR        w_dar
186600070925    3c                   endif
186700070925     c                   endsr
186800040318
186900040316      *------------------------------------------------------------------------*
187000040316      * - ROUTINE INIZIALE
187100040316      *------------------------------------------------------------------------*
187200040316     c     *Inzsr        BegSr
187300040316
187400040316     c     *Entry        Plist
187500040316     c                   Parm                    Kpjba
187600040326
187700040316     c     *dtaara       define    §azute        azuteds
187800040316     c     *dtaara       define    §datiute      ddatiute
187900040316     c                   in(E)     *dtaara
188000040316     c                   If        %error  or RSUT = *blanks
188100040316     c                   Clear                   Tibs34ds
188200040316     c                   Call      'TIBS34R'
188300040316     c                   Parm                    Tibs34ds
188400040316     c                   In        *dtaara
188500040316     c                   EndIf
188600040316
188700040316     c                   Clear                   wabi
188800040316     c                   Clear                   dLat
188900040316
189000040316      * Verifica errori e autorità profilo
189100040316s   1c                   Select
189200040317      * se ho errori nei dati utente esco dal pgm
189300040316w   1c                   When      DutErr = 'E'
189400040316     c                   Eval      $errute = *On
189500040317     c                   GoTo      EndInz
189600040317      * se non sono in sede (filiale remota)
189700040317      * --> se 1° livello, abilitazioni al terminal
189800040317      *     se 2° livello, abilitazioni al punto operativo
189900040316w   1c                   When      UteAut = *Blanks
190000040317if  2c                   If        DutLpo = '1'                                 *1° LIVELLO
190100040317     c                   Eval      wabi   = 'TP'
190200040317e   2c                   EndIf
190300040317if  2c                   If        DutLpo = '2'                                 *2° LIVELLO
190400040317     c                   Eval      wabi   = 'PO'
190500040317e   2c                   EndIf
190600040317      * sono in sede carica le abilitazioni del profilo
190700040316x   1c                   Other
190800040316     c                   Eval      wabi = UteAut
190900040317e   1c                   EndSl
191000040317      * controllo se ok l'abilitazione dell'utente
191100040316     c                   Clear                   Tibs02ds
191200040316     c                   Eval      T02Mod = 'C'
191300040316     c                   Eval      T02Sif = knsif
191400040316     c                   Eval      T02Cod = 'LAT'
191500040316     c                   Movel(p)  wabi          T02Ke1
191600040316     c                   Call      'TIBS02R'
191700040316     c                   Parm                    kpjba
191800040316     c                   Parm                    Tibs02ds
191900040317if  1c                   If        T02Err = *Blanks
192000040316     c                   Eval      dLat = T02Uni
192100040317e   1c                   EndIf
192200040317if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
192300040316     c                   Eval      $errute = *On
192400040317e   1c                   EndIf
192500040317      * per errore esco dal pgm
192600040316if  1c                   If        $ErrUte = *On
192700040316     c                   GoTo      EndInz
192800040316e   1c                   EndIf
192900040316
193000040316     c                   Eval      VtcTit = Titolo
193100040318
193200040316      * Imposto il p.o. di gestione
193300040316if  1c                   If        DutLpo = '2' or DutLpo = *Blanks
193400040316     c                   Movel     DutPou        V1cFgs
193500040316     c                   Eval      *In06 = *On
193600040316     c                   Else
193700040316     c                   Movel     Simfel        V1cFgs
193800040325e   1c                   EndIf
193900040317
194000040317      * Carico L1 di simfel
194100040317     c                   Clear                   Trul06ds
194200040317     c                   Eval      D06Cod = '£1'
194300040317     c                   Movel     Simfel        D06Key
194400040317     c                   Movel(p)  Trul06ds      kpjbu
194500040317     c                   Call      'TRUL06R'
194600040317     c                   Parm                    kpjba
194700040317     c                   Movel     kpjbu         Trul06ds
194800040317     c                   Movea     lin           l1
194900040316
195000040316     c                   ExSr      Sr_ContrFgs
195100040326
195200050112      * Se ambiente prova
195300050112     c                   If        %subst(knsif:7:1) = 'P'
195400050112     c                   Eval      wlib = 'GAITRAGRPS'
195500050112     c                   Else
195600050112      * Se ambiente buono FILTRA201
195700040326     c                   Eval      wlib = 'GAITRAGRU '
195800040512     c                   EndIf
195900070907      *
196000070907      * Aggancio tabella "VPODECO" per reperimento data attivazione
196100070907      * data ritiro da orm
196200070907     C                   CLEAR                   tibs02ds
196300070907     c                   clear                   dvpodeco
196400070907     c                   Eval      T02Mod = 'C'
196500070907     c                   Eval      T02Sif = knsif
196600070907     c                   Eval      T02Cod = 'VPO'
196700070907     c                   movel(P)  'DECO'        t02ke1
196800070907     C                   CALL      'TIBS02R'
196900070907     C                   PARM                    KPJBA
197000070907     C                   PARM                    tibs02ds
197100070907    2C                   IF        T02ERR = *BLANKS
197200070907     C                   eval      dvpodeco  = t02uni
197300070907    2C                   ENDIF
197400070907     c*
197500070907     c                   time                    w0140            14 0
197600070907     C* Data del giorno in AAAAMMGG
197700070907     C                   move      w0140         G02DAT
197800070907     C                   movel     *blank        G02ERR
197900070907     C                   call      'XSRDA8'
198000070907     C                   Parm                    wlbdat
198100070907     C                   MOVEL     g02inv        dateu             8 0
198200070907
198300051114      * oggi - 10 gg.calendario
198400071107     c***                if        §vpodrt>dateu
198500071107     c***  *iso          Movel     *date         wdataiso
198600071107     c***                Subdur    10:*d         wdataiso
198700071107     c***                Move      wdataiso      wdata
198800071107     c***
198900071107     c***                else
199000070907     c* oggi - 1  giorno lavorativo
199100070919     c                   clear                   TRUL52ds
199200070919     c                   eval      d52gio=1
199300070919     c                   eval      d52sgn='-'
199400070907     c                   z-add     *date         D52din
199500070907     c                   call      'TRUL52R '
199600070907     c                   parm                    TRUL52ds
199700070907     c                   z-add     D52dfi        wdata
199800071107     c****               endif
199900040318
200000040318     c     kTab          klist
200100040318     c                   kfld                    codut
200200040318     c                   kfld                    kcod
200300040325     c                   kfld                    kkey
200400040324
200500040324     c     kOrm          klist
200600040324     c                   kfld                    Vs1Poe
200700040324     c                   kfld                    Vs1Nsr
200800040324     c                   kfld                    Vs1Nor
200900040324     c                   kfld                    Vs1Nrv
201000040324
201100040324     c     kApd          klist
201200040324     c                   kfld                    ApdTip
201300040324     c                   kfld                    ApdPdr
201400040318
201500040318     c     kBlp          klist
201600040318     c                   kfld                    V2cAas
201700040318     c                   kfld                    V2cLnp
201800040318     c                   kfld                    V2cNrs
201900040318     c                   kfld                    V2cNsp
202000040318
202100060214     c     kar4          klist
202200040413     c                   kfld                    V2cAas
202300040413     c                   kfld                    V2cLnp
202400040413     c                   kfld                    V2cNrs
202500040413     c                   kfld                    V2cNsp
202600040318     c                   kfld                    wtrc
202700040325
202800040325     c     kAr6          klist
202900040325     c                   kfld                    V2cAas
203000040325     c                   kfld                    V2cLnp
203100040325     c                   kfld                    V2cNrs
203200040325     c                   kfld                    V2cNsp
203300040325     c                   kfld                    Ar6Trc
203400040615
203500060214     C     kfiar4        klist
203600060214     C                   kfld                    Kar4trc
203700060214     C                   kfld                    kar4n14
203800070910     c
203900070910     C     KFTT          KLIST
204000070910     C                   KFLD                    blppdr
204100070910     C                   KFLD                    W01FVL
204200070910     C                   KFLD                    W_dar
204300070910     C                   KFLD                    W01TSR
204400070919     C     KFTTold       KLIST
204500070919     C                   KFLD                    blppdr
204600070919     C                   KFLD                    W01FVL
204700070919     C                   KFLD                    blpdrt
204800070919     C                   KFLD                    W01TSR
204900070910     C     kftt1         KLIST
205000070910     C                   KFLD                    blppdr
205100070910     C                   KFLD                    w01tsr
205200070910     C                   KFLD                    W_ndc
205300070910     C                   KFLD                    w_dar
205400070913     C     kftt1old      KLIST
205500070913     C                   KFLD                    blppdr
205600070913     C                   KFLD                    w01tsr
205700070913     C                   KFLD                    W_ndc
205800070913     C                   KFLD                    w_olddrt
205900070919     c     kftd5         klist
206000070919     C                   KFLD                    fttpdr
206100070919     C                   KFLD                    ftttsr
206200070919     C                   KFLD                    fttndc
206300070919     C                   KFLD                    w_olddrt
206400070919     c                   kfld                    blpaas
206500070919     c                   kfld                    blplnp
206600070919     c                   kfld                    blpnrs
206700070919     c                   kfld                    blpnsp
206800110322     C     KORG          KLIST
206900110322     C                   KFLD                    §b4poe
207000110322     C                   KFLD                    §b4nsr
207100110322     C                   KFLD                    §b4nor
207200110322     C                   KFLD                    §b4nrv
207300120612     C     KDST          KLIST
207400110322     C                   KFLD                    knpg
207500110322     C                   KFLD                    knfv
207600110322     C                   KFLD                    kfgs
207700110322     C     Kar5          KLIST
207800110322     C                   KFLD                    BLPAAS
207900110322     C                   KFLD                    BLPLNP
208000110322     C                   KFLD                    BLPNRS
208100110322     C                   KFLD                    BLPNSP
208200110322     C                   KFLD                    KTRD
208300040326
208400040316     c     EndInz        EndSr
208500040326      *------------------------------------------------------------------------*
208600070912     OFNBLP000  E            AGGBLP
208700070912     O                       BLPDRT
208800070912     OTITAS000  E            AGTAS0
208900070912     O                       TASDRT
209000070912     OTITAS010  E            AGTAS1
209100070912     O                       TASDRT
209200040615** CM3
209300040615OVRPRTF FILE(FNLV22P) OUTQ(XXXXXXXXXX) FORMTYPE('ETICH     ')
209400040615 SHARE(*YES)
209500040615** cmA4
209600040615OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
209700040615 SHARE(*YES)          USRDTA('xxxxxxxxx')
209800040615**  cmA5
209900040615OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
210000040615 SHARE(*YES)          USRDTA('xxxxxxxxx')
210100040326** cmd
210200040330OVRDBF FILE(TITA400F) TOFILE(
210300070919OVRDBF FILE(TITAS30C) TOFILE(
210400040326** msg                                                                       *
210500040316P.O. gestione errato                                                                01
210600040316Procedura ORM non attivata                                                          02
210700040316Ampiezza massima subfile superata                                                   03
210800040316Scelta non valida                                                                   04
210900040325Serie __ non gestita - AVVISARE IL CED                                              05
211000040317 ORM non abilitata per serie __                                                     06
211100040318Immettere la spedizione                                                             07
211200040318Spedizione non trovata                                                              08
211300040318Spedizione già abbinata ad un ORM                                                   09
211400040615                                                                                    10
211500040318Bolla di recupero! Non abbinare ad un ORM                                           11
211600040325ORM con serie! Il codice bolla della spedizione abbinata non è __                   12
211700040330Per ORM prepagato la spedizione deve una bolla Franco prepagato                     13
211800040330                                                                                    14
211900040325L'ORM richiede una spedizione in porto Assegnato                                    15
212000040325Abbinare una spedizione avente Linea di partenza uguale al P.O. ritiro              16
212100040325Abbinare una spedizione avente Linea di arrivo uguale al P.O. consegna              17
212200040325Abbinare una spedizione in porto Franco                                             18
212300040331ORM commissionato da p.o. ___ abbinare bolla export     in porto assegnato          19
212400040326Data ritiro ORM diversa da data ritiro merce della bolla. ENTER per forzare.        20
212500051114Spedizione immessa da oltre 10 giorni. Impossibile abbinare                         21
212600070907Data ritiro ORM diversa da data ritiro merce della bolla. ENTER per proseguire.     22
212700070907Spedizione immessa da oltre 1 giorno. Impossibile abbinare                          23
212800070919Impossibile abbinare: già confermati i conteggi per l'autotrasportatore             24
212900070911Impossibile abbinare: per l'autotrasp. non presenti ritiri nella data ritiro        25
213000070920Spedizione al momento in uso da un altro lavoro: impossibile abbinare               26
