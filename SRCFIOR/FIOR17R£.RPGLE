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
006100130619     d**** l1              s              3  0 dim(30)
006200040316     d l6              s              3  0 dim(30)
006300070911     d msg             s             78    dim(26) ctdata perrcd(1)
006400040316     d opz             s              1    dim(20)
006500040316     d sel             s              1    dim(20)
006600130619     d SKpog           s              3    dim(250) inz(*zeros)                 Filiali Gestibili
006700040316
006800040316      *------------------------------------------------------------------------*
006900040316      *   V A R I A B I L I
007000040316      *------------------------------------------------------------------------*
007100040318     d codut           s              1  0 inz(1)
007200040615     d comman          s            110
007300040615     d conta           s              2  0
007400040316     d dopz            s             20    inz('25A')
007500060214     d kar4trc         s                   like(ar4Trc) inz('M')
007600060214     d kar4n14         s                   like(ar4N14)
007700040318     d kcod            s                   like(TblCod) inz('3A')
007800040325     d kkey            s                   like(TblKey)
007900040615     d lenght          s             15  5
008000040316     d nrr1            s              4  0
008100040317     d savkpjbu        s                   like(kpjbu)
008200040318     d xx              s              2  0
008300040316     d varfgs          s                   like(V1cFgs)
008400040413     d wabb            s              1    inz(*Off)
008500040316     d wabi            s                   like(UteAut)
008600051114     d wdata           s              8  0
008700070907     d wdataiso        s               d   datfmt(*iso)
008800040615     d wdatibolla      s              9
008900040615     d wdatibolA4      s              9
009000040615     d wdatibolA5      s              9
009100040326     d wfdar           s              1    inz(*Off)
009200040326     d wlib            s             10
009300051114     d woggi           s               d   datfmt(*iso)
009400040325     d wtb1            s              1
009500060214     d wtrc            s                   like(ar4Trc) inz('M')
009600040317     d w002a           s              2
009700130619     d w003a           s              3
009800040316     d w0140           s             14  0
009900040316     d $errute         s              1    inz(*off)
010000070910     d w_dar           s                   like(ormdar)
010001151104     d wtco            s                   like(ormtco)
010003151106     d wprep           s              1
010004151105     d wb4ado          s                   like(§b4ado)
010005151105     d waggtasD        s              1
010006151105     d waggtasO        s              1
010100070910     d w01fvl          s                   like(fttfvl)
010200070910     d w01tsr          s                   like(ftttsr)
010300070910     d w_ndc           s                   like(fttndc)
010400070920     d wimport         s              1
010500070924     d wpdd            s              1
010600110322     d RitiroPDA       s              1
010700070913     d w_olddrt        s                   like(blpdrt)
010800070919     d savft2          s                   like(blpft2)
010900120612     d kfgs            s                   Like(dstfgs)
011000120612     d knfv            s                   Like(dstnfv)
011100120612     d knpg            s                   Like(dstnpg) inz(4)
011200110322     d kTRD            s                   Like(ar5trd) inz('GEN')
011300130117     d kTBEcod         s                   like(TBEcod) inz('OSR')
011400130117     d wORMcor         s              7s 0 inz
011500130117     d wNSRosr         s              2
011600130117     d wLNPosr         s              3
011700040316
011800040316      *------------------------------------------------------------------------*
011900040316      *   D S   I N T E R N E / E S T E R N E
012000040316      *------------------------------------------------------------------------*
012100051114     d                 ds
012200051114     d  blpaas                 1      4  0
012300051114     d  blpmgs                 5      8  0
012400051114     d  blpdsp                 1      8  0
012500051114
012600040318     d  dskOrm         ds
012700040318     d   V2cPoe
012800040318     d   V2cNsr
012900040318     d   V2cNor
013000040318     d   V2cNrv
013100040318
013200040317     d  dskOsr         ds
013300040317     d   Vs1Poe
013400040331     d   Vs1Nsr
013500040317
013600040318     d dsOrm           ds
013700040318     d  dsopoe                 1      3  0
013800040318     d  dsonor                 4     10  0
013900040318     d  dsonsr                11     12  0
014000040318     d  dsonrv                13     14  0
014100040318     d  dsoopz                15     15
014200040318     d  dsofgs                16     18  0
014300040318     d  dsoref                60     61
014400040318     d  dsomio                62     62
014500040318
014600040318     d dsBlp           ds
014700040318     d  dsbaas                 1      4  0
014800040318     d  dsblnp                 5      7  0
014900040318     d  dsbnrs                 8      9  0
015000040318     d  dsbnsp                10     16  0
015100040326     d  dsbf03                17     17
015200040326     d  dsbrsu                23     42
015300040326     d  dsbflg                31     31
015400040318
015500040318     d wlbdat          ds                  inz
015600040318     d  g02dat                 1      8  0
015700040318     d  g02inv                 9     16  0
015800040318     d  g02err                17     17
015900040318     d  g02tgi                18     22  0
016000040324
016100040324     d ds_V2cCra       DS
016200040326     d  v2cRa1
016300040326     d  v2cRa2
016400040326     d  v2cRa3
016500040324
016600040324     d ds_V2cCor       DS
016700040326     d  v2cOr1
016800040326     d  v2cOr2
016900040326     d  v2cOr3
017000040324
017100040324     d ds_V2cCrc       DS
017200040326     d  v2cRc1
017300040326     d  v2cRc2
017400040326     d  v2cRc3
017500040316
017600040316     d                sds
017700040316     d  vtcpgm                 1     10
017800040316
017900040316     d Azuteds       e ds                  extname(Azute00f)
018000040615     d dblp          e ds
018100040316     d dDatiute      e ds
018200070710     d dfar          e ds
018300040316     d dLat          e ds
018400040325     d dorm01        e ds
018500040317     d dOsr          e ds
018600040615     d dtasv         e ds
018700040318     d dsBl4m        e ds
018800040318     d ds3a          e ds
018900070907     d dvpodeco      e ds
019000040324     d ds_cnaco      e ds                  extname(Cnaco00f)
019100040324     d ds_cnind      e ds                  extname(Cnind00f)
019200040324     d ds_cnclp      e ds                  extname(Cnclp00f)
019300040324     d ds_fncls      e ds                  extname(Fncls00f)
019400130619     d dUTE01        e ds
019500040317     d Fior05ds      e ds
019600040318     d Fnlv19ds      e ds
019700040316     d Kpjba         e ds
019800040326     d Og143         e ds
019900110322     d dDSTFLR       e ds
020000110322     d dAr5GEN       e ds
020100040316     d Tibs02ds      e ds
020200040316     d Tibs34ds      e ds
020300040324     d Tibs69ds      e ds
020400040316     d Trul06ds      e ds
020500040316     d  lin                    1     90  0 dim(30)
020600040615     d Trul90ds      e ds
020700060516     d fnlv55ds      e ds
020701151105     d dtasflo       e ds
020800070907      * - Aggiunta/Sottrazione di n giorni lavorativi ad una data
020900070919     d TRUL52ds      e ds
021000130619
021100130619     d TRUL31DS      e ds
021200070919
021300070919     d fnlva7ds      e ds
021400070919
021500070919     D* PASSAGGIO DATI A SVALORIZZAZ. DISTINTA PADRONC.  - FNLRE2R -
021600070919     D PARAM3          DS
021700070919     D  PA3PDR                 1      7  0
021800070919     D  PA3TSR                 8      8
021900070919     D  PA3NDC                 9     15  0
022000070919     D  PA3FCT                16     16
022100070919     D  PA3FVD                17     17
022200070919     D  PA3FVT                18     18
022300070919     D  PA3DDC                19     26  0
022400070919     D  PA3sml               256    256
022500070919
022600070920     d TRUL82ds      e ds
022700070919
022800070920     d tasinfds        ds
022900070920     d  tasnrr               397    400i 0
023000070920     d fttinfds        ds
023100070920     d  fttnrr               397    400i 0
023200070920     d fstinfds        ds
023300070920     d  fstnrr               397    400i 0
023400070920     d ftdinfds        ds
023500070920     d  ftdnrr               397    400i 0
023600070920     d fsdinfds        ds
023700070920     d  fsdnrr               397    400i 0
023800070920     d arbinfds        ds
023900070920     d  arbnrr               397    400i 0
024000040316
024100040316      *------------------------------------------------------------------------*
024200070919      *   C O S T A N T I
024300040316      *------------------------------------------------------------------------*
024400040316      *  titolo videata (lunghezza massima 34)
024500040316     d titolo          c                   const('** CONTROLLO ABBINAMENTO O.R-
024600040317     d                                     .M. **')
024700040316      *  Descrizione opzioni (lunghezza massima 78)
024800040317     d dopz1           c                   const('2=Manutenzione   5=Visualizza-
024900040317     d                                        A=Abbina')
025000040316     d dopz2           c                   const(' ')
025100040316
025200070912     I* PREDISPONGO GLI INDICATORI DI TIPO RECORD PER TITAS
025300070912     ITITAS000      22
025400070912     ITITAS010      23
025500040316      *------------------------------------------------------------------------*
025600040316      *  RIEPILOGO INDICATORI
025700040316      *------------------------------------------------------------------------*
025800040316      * 05 - SPROTEGGO P.O. IN GESTIONE
025900040316      * 06 - NON ABILITO TASTO FUNZIONALE F18
026000040316      * 20 - GESTIONE SUBFILE
026100040316      * 21 - GESTIONE SUBFILE
026200040316      * 28 - ERRORE GENERICO DSPF
026300040316      * 30 - Comodo
026400040316      * 40 - ERRORE p.o. in gestione
026500040318      * 41 - ERRORE SFL
026600040318      * 42 - ERRORE bolla
026700040616      * 50 - Orm con bolla in assegnato
026800070912      * 98 - ERRORE ovrdbf di Titas30c
026900070912      * 99 - ERRORE ovrdbf di Tita400f
027000040316
027100040316      *------------------------------------------------------------------------*
027200040316
027300040317      * Errore sull'utente esco dal pgm
027400040316if  1c                   If        $ErrUte = *On
027500040316     c                   Goto      Fine
027600040316e   1c                   EndIf
027700040317
027800040317      * Errore sul p.o. esco dal pgm
027900040317if  1c                   If        *In40 = *On
028000040317     c                   Goto      Fine
028100040317e   1c                   EndIf
028200040316
028300040317      * Carico subfile ?
028400040317     c     Carica        Tag
028500040317     c                   ExSr      Sr_CarSfl
028600040317
028700040317      * Emetto il subfile ?
028800040317     c     EmiS01        Tag
028900040317     c                   ExSr      Sr_EmiSfl
029000040317     c                   Eval      *In28 = *Off
029100040317
029200040316      * F3=Fine
029300040317     c   kc              GoTo      Fine
029400040413
029500040413      * F5=Rivisualizza subfile
029600040413     c   ke              GoTo      Carica
029700040317
029800040316      * F18=Cambio P.O. in gestione
029900040316     c   ks              Eval      *In05 = *On
030000040317     c   ks              GoTo      EmiS01
030100040316      * 05 on --> controllo P.O. in gestione
030200040316if  1c                   If        *In05
030300040316     c                   ExSr      Sr_ContrFgs
030400040316     c  n28              Eval      *In05 = *Off
030500040317     c  n28              GoTo      Carica
030600040317     c   28              GoTo      EmiS01
030700040316e   1c                   EndIf
030800040316      * Controllo p.o. gestione se variato
030900040316if  1c                   If        V1cFgs <> varfgs
031000040316     c                   ExSr      Sr_ContrFgs
031100040317     c  n28              GoTo      Carica
031200040317     c   28              GoTo      EmiS01
031300040316e   1c                   EndIf
031400040316
031500040316      * Leggo il subfile ?
031600040316if  1c                   If        nrr1 > *Zeros
031700040316     c                   ExSr      Sr_LetSfl
031800040413     c                   GoTo      EmiS01
031900040316e   1c                   EndIf
032000040316
032100040316     c     Fine          Tag
032200090403     c* chiusurra pgm
032300090403     C                   MOVEL     'C'           SA2TLA
032400090403     C                   CALL      'FNLSA2R3'
032500090403     C                   PARM                    SA2TLA
032600090403     C                   PARM                    SA2TRC
032700090403     C                   PARM      00000000      SA2DTS
032800090403     C                   PARM      000           WWLNA             3 0
032900040316
033000040316     c                   Eval      *InLr = *On
033100040316
033200040316      *------------------------------------------------------------------------*
033300040316      * - CONTROLLO IL P.O. IN GESTIONE
033400040316      *------------------------------------------------------------------------*
033500040316     c     Sr_ContrFgs   BegSr
033600040316
033700040316     c                   Setoff                                       40
033800040316     c                   Clear                   V1dFgs
033900060926     c**!!!              Clear                   Og148
034000040317     c                   Eval      varfgs = V1cFgs
034100040316
034200040316     c     V1cFgs        Chain     Azorg01L
034300040316if  1c                   If        Not %Found(Azorg01l) or OrgFva <> *Blanks or
034400040316     c                             (OrgFag <> 'A' and OrgFag <> 'F')
034500040316     c                   Eval      *In28 = *On
034600040316     c                   Eval      *In40 = *On
034700040316     c                   Eval      V1cMsg = msg(01)
034800040316     c                   Goto      EndFgs
034900040316e   1c                   EndIf
035000040316
035100040316     c                   movel     OrgDes        V1dFgs
035200060926     c**!!!              Movel     OrgDe8        Og148
035300040316
035400040316      * procedura ORM non attiva
035500060926if  1c**!!!              If        §ogorm <> 'S'
035600060926     c**!!!              Eval      *In28 = *On
035700060926     c**!!!              Eval      *In40 = *On
035800060926     c**!!!              Eval      V1cMsg = msg(02)
035900060926     c**!!!              Goto      EndFgs
036000060926e   1c**!!!              EndIf
036100040317
036200040316      * Carico L6
036300040316     c                   Clear                   Trul06ds
036400040316     c                   Eval      D06Cod = '£6'
036500040316     c                   Movel     V1cFgs        D06Key
036600040316     c                   Movel(p)  Trul06ds      kpjbu
036700040316     c                   Call      'TRUL06R'
036800040316     c                   Parm                    kpjba
036900040316     c                   Movel     kpjbu         Trul06ds
037000040316     c                   Movea     lin           l6
037100040316
037200130619     c                   move      V1Cfgs        w003a
037300130619     c     w003a         lookup    SKpog                                  30
037400130619     c**** V1cFgs        Lookup    l1                                     30
037500040316if  1c                   If        Not *In30
037600040316     c                   Eval      *In28 = *On
037700040316     c                   Eval      *In40 = *On
037800040316     c                   Eval      V1cMsg = msg(01)
037900040316     c                   Goto      EndFgs
038000040316e   1c                   EndIf
038100040316
038200040316     c     EndFgs        EndSr
038300040316
038400040316      *------------------------------------------------------------------------*
038500040316      * - CARICO IL SUBFILE
038600040316      *------------------------------------------------------------------------*
038700040316     c     Sr_CarSfl     BegSr
038800040316
038900040316      * Imposto le opzioni di scelta
039000040316     c                   Clear                   opz
039100040317     c                   Movel     dopz1         V1dse1
039200040317     c                   Movel     dopz2         V1dse2
039300040316     c                   Movea     dopz          opz
039400040316
039500040316     c                   Eval      recsf = 1
039600040316
039700040316      * Pulisco subfile
039800040316     c                   Clear                   nrr1
039900040316     c                   Eval      *In21 = *Off
040000040316     c                   Write     or17c01
040100040316     c                   Eval      *In20 = *On
040200040316     c                   Eval      *In21 = *On
040300040317
040400040316
040500040316      * Leggo gli orm per p.o. ritiro
040600040316do  1c                   Do        30            xx
040700040316     c     l6(xx)        Setll     Fnorm13l
040800040316do  2c                   Do        *Hival
040900040316     c     l6(xx)        Reade     Fnorm13l
041000040317
041100040317      * Fine file
041200040316if  3c                   If        %Eof (Fnorm13l)
041300040316     c                   Leave
041400040316e   3c                   EndIf
041500040316
041600040316      * carico fino a 9999 record
041700040316if  3c                   If        nrr1 >= 9998
041800040316     c                   Eval      *In28 = *On
041900040317     c                   Eval      V1cMsg = msg(03)
042000040316     c                   Leave
042100040316e   3c                   EndIf
042200040316
042300040316      * Escludo gli annullati
042400040316if  3c                   If        OrmAtb <> *Blanks
042500040316     c                   Iter
042600040316e   3c                   EndIf
042700040316
042800040316      * Devo emettere solo gli orm quadrati in fase 600
042900040316if  3c                   If        OrmFao <> 600
043000040316     c                   iter
043100040316e   3c                   EndIf
043200040615
043300060214      * non deve avere un ar4 abbinato
043400040615     c                   Clear                   worm             14
043500040615     c                   Move      OrmPoe        wpoe              3
043600040615     c                   Move      OrmNsr        wnsr              2
043700040615     c                   Move      OrmNor        wnor              7
043800040615     c                   Move      OrmNrv        wnrv              2
043900040615     c                   Eval      worm = wpoe + wnsr
044000040615     c                             + wnor + wnrv
044100060214     c                   Movel     worm          kar4n14
044200060214     c     kfiar4        Setll     Fiar404l
044300060214     c                   If        %Equal(Fiar404l)
044400040615     c                   iter
044500040615     c                   EndIf
044600040316
044700040317     c                   Clear                   Vs1Opz
044800040317     c                   Eval      Vs1Poe = OrmPoe
044900040317     c                   Eval      Vs1Nsr = OrmNsr
045000040317     c                   Eval      Vs1Nor = OrmNor
045100040317     c                   Eval      Vs1Nrv = OrmNrv
045200040317     c                   Eval      Vs1Rsr = ORmRsr
045300040317     c                   Eval      Vs1Lor = OrmLor
045400040317     c                   Eval      Vs1Fao = OrmFao
045500130117     c                   Eval      VH1cor = ORMcor
045600040316
045700070925     c                   exsr      sr_drt
045800040316      * Inversione data ritiro
045900071107     c****               if        dateu<§vpodrt
046000071107     c****               Z-add     OrmDar        g02inv
046100071107     c****               else
046200070925     c                   Z-add     w_dar         g02inv
046300071107     c****               endif
046400040316     c                   Eval      g02err = '3'
046500040316     c                   Call      'XSRDA8'
046600040316     c                   Parm                    wlbdat
046700040317     c                   Movel     g02dat        Vs1Dar
046800040413
046900040413     c                   Clear                   Vs1Abb
047000040316
047100040316     c                   Add       1             nrr1
047200040316     c                   Write     or17s01
047300040317
047400040317e   2c                   EndDo
047500040317e   1c                   EndDo
047600040316
047700040316     c                   EndSr
047800040316
047900040316      *------------------------------------------------------------------------*
048000040316      * - EMETTO IL SUBFILE
048100040316      *------------------------------------------------------------------------*
048200040316     c     Sr_EmiSfl     BegSr
048300040316
048400040316      * Controllo se recsf è maggiore di nrr
048500040316if  1c                   If        recsf > nrr1
048600040316     c                   Eval      recsf = 1
048700040316e   1c                   EndIf
048800040316
048900040316      * Se non ho caricato emetto una videata di segnalazione
049000040316if  1c                   If        nrr1 = *Zeros
049100040317     c                   Exfmt     or17d01
049200040316e   1c                   EndIf
049300040316
049400040316      * Se ho caricato emetto il subfile
049500040316if  1c                   If        nrr1 > *Zeros
049600040316     c                   Write     or17z01
049700040316     c                   Exfmt     or17c01
049800040316e   1c                   EndIf
049900040316
050000040316     c                   EndSr
050100040316
050200040316      *------------------------------------------------------------------------*
050300040316      * - LEGGO IL SUBFILE
050400040316      *------------------------------------------------------------------------*
050500040316     c     Sr_LetSfl     BegSr
050600040316
050700040316     c                   Clear                   nrr1
050800040316
050900040316do  1c                   Do        *Hival
051000040317     c                   Eval      nrr1 = nrr1 +1
051100040317     c     nrr1          Chain     or17s01                            30
051200040317     c   30              Leave
051300040316
051400040318     c                   Setoff                                       28  41
051500040413     c                   Eval      wabb = *Off
051600040316
051700040316      * Controllo se scelta valida
051800040317     c     Vs1Opz        Lookup    opz                                    30
051900040316if  2c                   If        Not *In30
052000040316     c                   Eval      *In28 = *On
052100040318     c                   Eval      *In41 = *On
052200040317     c                   Eval      V1cMsg = msg(04)
052300040316     c                   Eval      recsf = nrr1
052400040316     c                   Update    or17s01
052500040316     c                   GoTo      EmiS01
052600040317e   2c                   EndIf
052700040325
052800040325      * ORM con numero serie
052900040326      * controllo se scelta impedita nella tab. "OSR"
053000040317if  2c                   If        Vs1Nsr <> *Zeros
053100040317     c                   ExSr      Sr_Serie
053200130117     c***                Move      Vs1Nsr        w002a
053300040325      * Tabella OSR non trovata
053400040325if  3c                   If        *In28
053500040318     c                   Eval      *In41 = *On
053600040317     c                   Eval      V1cMsg = msg(05)
053700130117     c********           Eval      V1cMsg = %replace(wNSRosr:V1cMsg:
053800130117     c********                              %scan('__':V1cMsg))
053900130117     c                   eval      V1Cmsg = 'ERRORE: Ordinante ' +
054000130117     c                             %editc(wORMcor:'X') + ' non abilitato -
054100130117     c                             in tab. OSR. Avvisare CED'
054200040317     c                   Eval      recsf = nrr1
054300040317     c                   Update    or17s01
054400040317     c                   GoTo      EmiS01
054500040317e   3c                   EndIf
054600040317      * Controllo se posso manutenzionare l'ORM
054700040317if  3c                   If        Vs1Opz = '2' and d§OsrMan = 'S'
054800040317     c                   Eval      *In28 = *On
054900040318     c                   Eval      *In41 = *On
055000040317     c                   Eval      V1cMsg = 'Manutenzione' + msg(06)
055100130117     c                   Eval      V1cMsg = %replace(wNSRosr:V1cMsg:
055200040317     c                                      %scan('__':V1cMsg))
055300130117     C                   eval      V1Cmsg = %trim(V1Cmsg) +
055400130117     C                             ' LNP ' + wLNPosr
055500040317     c                   Eval      recsf = nrr1
055600040317     c                   Update    or17s01
055700040317     c                   GoTo      EmiS01
055800040317e   3c                   EndIf
055900040317e   2c                   EndIf
056000040325
056100040317      * Scelta
056200040317s   2c                   Select
056300040317      * Visualizzazione
056400040317w    c                   When      Vs1Opz = '5'
056500040317     c                   Eval      recsf = nrr1
056600040317     c                   ExSr      Sr_Modifica
056700040317     c                   Eval      recsf = nrr1
056800040317     c                   Clear                   Vs1Opz
056900040317     c                   Update    or17s01
057000040326      * Manutenzione
057100040317w    c                   When      Vs1Opz = '2'
057200040317     c                   Eval      recsf = nrr1
057300040317     c                   ExSr      Sr_Modifica
057400040317     c                   Eval      recsf = nrr1
057500040317     c                   Clear                   Vs1Opz
057600040317     c                   Update    or17s01
057700040317      * Abbinamento
057800040317w    c                   When      Vs1Opz = 'A'
057900040317     c                   Eval      recsf = nrr1
058000040324     c                   ExSr      Sr_CarOrm
058100040317     c                   ExSr      Sr_Abbina
058200040317     c                   Eval      recsf = nrr1
058300040317     c                   Clear                   Vs1Opz
058400040413     c                   If        wabb = *On
058500040413     c                   Eval      Vs1Abb = 'A'
058600040413     c                   EndIf
058700040317     c                   Update    or17s01
058800040317e   2c                   EndSl
058900040326
059000040317e   1c                   EndDo
059100040326
059200040317     c                   EndSr
059300040316
059400130121      /free
059500130121       //--------------------------------------------------------------
059600130121       //?Cerco tabella OSR per codice ordinante
059700130121       //--------------------------------------------------------------
059800130121       BEGSR sr_Serie;
059900130121
060000130121         clear dOSR;
060100130121         wORMcor = %dec(%subst(%editc(VH1cor:'X'):1:7):7:0);
060200130121       //?leggo tutta la tabella OSR
060300130121         setll kTBEcod tntbe01l;
060400130121         reade kTBEcod tntbe01l;
060500130121
060600130121         DOW not %eof(tntbe01l);
060700130122           IF  TBEatb = *blanks;
060800130121           dosr = tbeuni;
060900130121           IF d§osrcli  = wORMcor or d§OSRcl2  = wORMcor or
061000130121              d§osrcl3  = wORMcor or d§OSRcl4  = wORMcor or
061100130121              d§osrcl5  = wORMcor or d§OSRcl6  = wORMcor or
061200130121              d§osrcl7  = wORMcor or d§OSRcl8  = wORMcor or
061300130121              d§osrcl9  = wORMcor or d§OSRcl10 = wORMcor or
061400130121              d§osrcl11 = wORMcor or d§OSRcl12 = wORMcor or
061500130121              d§osrcl13 = wORMcor or d§OSRcl14 = wORMcor or
061600130121              d§osrcl15 = wORMcor or d§OSRcl16 = wORMcor;
061700130121             leavesr;
061800130121           ENDIF;
061900130122           ENDIF;
062000130121           reade kTBEcod tntbe01l;
062100130121         ENDDO;
062200130121
062300130121       //?se arrivo qua vuol dire che non ho trovato il cliente in tabella OSR
062400130121         *in28 = *on;
062500130121
062600130121       ENDSR;
062700130121      /end-free
062800040316
062900040317      *------------------------------------------------------------------------*
063000040317      * RICHIAMO IL PROGRAMMA DI MODIFICA O INTERROGAZIONE
063100040317      *------------------------------------------------------------------------*
063200040324     c     Sr_Modifica   BegSr
063300040326
063400040318     c                   Clear                   dsOrm
063500040318     c                   Eval      dsopoe = Vs1Poe
063600040318     c                   Eval      dsonor = Vs1Nor
063700040318     c                   Eval      dsonsr = Vs1Nsr
063800040318     c                   Eval      dsonrv = Vs1Nrv
063900040318     c                   Eval      dsoopz = Vs1Opz
064000040318     c                   Eval      dsofgs = V1cFgs
064100040318     c                   Eval      dsoref = 'SI'
064200040318     c                   Eval      dsomio = 'S'
064300040317     c                   Eval      savkpjbu = kpjbu
064400040318     c                   Eval      kpjbu = dsOrm
064500040326
064600040317s   1c                   Select
064700040317      * Modifica
064800040317w    c                   When      Vs1Opz = '2'
064900040317     c                   Clear                   Fior05ds
065000040317     c                   Call      'FIOR05R'
065100040317     c                   Parm                    kpjba
065200040317     c                   Parm                    Fior05ds
065300040326      * Interrogazione
065400040317w    c                   When      Vs1Opz = '5'
065500040317     c                   Call      'FIOR07R'
065600040317     c                   Parm                    kpjba
065700040317e   1c                   EndSl
065800040317
065900040318     c                   Eval      dsOrm = kpjbu
066000040317     c                   Eval      kpjbu = savkpjbu
066100040326
066200040317     c                   EndSr
066300040324
066400040324      *------------------------------------------------------------------------*
066500040324      * CARICO I DATI DELL'ORM
066600040324      *------------------------------------------------------------------------*
066700040324     c     Sr_CarOrm     BegSr
066800040324
066801151104     c                   clear                   wtco
066900040324     c     kOrm          Chain(n)  Fnorm01l
067000040324if  1c                   If        %Found(Fnorm01l)
067100040324
067200040324      * Numero ORM
067300040324     c                   Eval      V2cPoe = OrmPoe
067400040324     c                   Eval      V2cNsr = OrmNsr
067500040324     c                   Eval      V2cNor = OrmNor
067600040324     c                   Eval      V2cNrv = OrmNrv
067700040324      * Tipo ORM
067800040324     c                   Clear                   Tibs02ds
067900040324     c                   Clear                   V2dTor
068000040324     c                   Eval      T02Mod = 'C'
068100040324     c                   Eval      T02Sif = knsif
068200040324     c                   Eval      T02Cod = 'TOR'
068300040324     c                   Eval      T02Ke1 = OrmTor
068400040324     c                   Call      'TIBS02R'
068500040324     c                   Parm                    kpjba
068600040324     c                   Parm                    Tibs02ds
068700040324     c                   If        T02Err = *Blanks
068800040324     c                   Movel     T02Uni        V2dTor
068900040324     c                   EndIf
069000040324      * Data ritiro
069100070925     c***                Z-add     OrmDar        g02inv
069200070925     c***                Movel     '3'           g02err
069300070925     c***                Call      'XSRDA8'
069400070925     c***                Parm                    wlbdat
069500070925     c***                Z-add     g02dat        V2cDar
069600070925     c                   exsr      sr_drt
069700070925      * Data ritiro
069800071107     c***                if        dateu<§vpodrt
069900071107     c***                Z-add     OrmDar        g02inv
070000071107     c***                else
070100070925     c                   z-add     w_dar         g02inv
070200071107     c****               endif
070300070925     c                   Movel     '3'           g02err
070400070925     c                   Call      'XSRDA8'
070500070925     c                   Parm                    wlbdat
070600070925     c                   Z-add     g02dat        V2cDar
070700040324      * Mittente
070800040326     c                   Move      OrmCra        ds_v2cCra
070900040324     c                   Eval      V2cRsr = OrmRsr
071000040324     c                   Eval      V2CInr = OrmInr
071100040324     c                   Eval      V2cCar = OrmCar
071200040324     c                   Eval      V2cLor = OrmLor
071300040324     c                   Eval      V2cPrr = OrmPrr
071400040324     c                   Eval      v2cNar = OrmNar
071500040324      * Ordinante
071600040326     c                   Move      OrmCor        ds_v2cCor
071700040324     c                   Eval      V2cRso = OrmRso
071800040324     c                   Eval      V2CIno = OrmIno
071900040324     c                   Eval      V2cCao = OrmCao
072000040324     c                   Eval      V2cLoo = OrmLoo
072100040324     c                   Eval      V2cPro = OrmPro
072200040324     c                   Eval      v2cNao = OrmNao
072300040324      * Destinatario
072400040326     c                   Move      OrmCrc        ds_v2cCrc
072500040324     c                   Eval      V2cRsc = OrmRsc
072600040324     c                   Eval      V2CInc = OrmInc
072700040324     c                   Eval      V2cCac = OrmCac
072800040324     c                   Eval      V2cLoc = OrmLoc
072900040324     c                   Eval      V2cPrc = OrmPrc
073000040324     c                   Eval      v2cNac = OrmNac
073100040324      * Colli - Peso - Volume
073200040324     c                   Eval      V2cncl = OrmNcl
073300040324     c                   Eval      V2cpkg = OrmPkg
073400040324     c                   Eval      V2cvlm = OrmVlm
073500040409      * Tipo comunicazione ORM
073600040409     c                   Clear                   V2dTco
073700040409     c                   Clear                   Tibs02ds
073800040409     c                   Eval      T02Mod = 'C'
073900040409     c                   Eval      T02Sif = knsif
074000040409     c                   Eval      T02Cod = 'TCO'
074100040409     c                   Eval      T02Ke1 = OrmTco
074200040409     c                   Call      'TIBS02R'
074300040409     c                   Parm                    kpjba
074400040409     c                   Parm                    Tibs02ds
074500040409     c                   Eval      V2dTco = T02Uni
074501151104     c                   eval      wtco=ormtco
074600040325      * P.O. conegna
074700040325     c                   Clear                   V2dPoc
074800040325     c                   Eval      V2cPoc = OrmPoc
074900040325     c     OrmPoc        Chain     Azorg01l
075000040325     c                   If        %Found(Azorg01l)
075100040325     c                   Eval      V2dPoc = OrgDes
075200040325     c                   EndIf
075300040324      * Chi paga
075400040324     c                   Clear                   V2dPag
075500040324     c                   Clear                   V2dKsc
075600040324     c                   Select
075700040324     c                   When      OrmPag = 'M'
075800040324     c                   Eval      V2dPag = 'Mitt.'
075900040324     c                   When      OrmPag = 'D'
076000040324     c                   Eval      V2dPag = 'Dest.'
076100040324     c                   When      OrmPag = 'O'
076200040326     c                   Eval      V2dPag = 'Ord. '
076300040324     c                   EndSl
076400040324     c                   Eval      V2cKsc = OrmKsc
076500040324     c                   Clear                   Tibs69ds
076600040324     c                   Eval      I69Kac = OrmKsc
076700040324     c                   Call      'TIBS69R'
076800040324     c                   Parm                    Tibs69ds
076900040324     c                   Parm                    ds_cnaco
077000040324     c                   Parm                    ds_cnind
077100040324     c                   Parm                    ds_cnclp
077200040324     c                   Parm                    ds_fncls
077300040324     c                   If        O69Err <> '1'
077400040324     c                   Movel     AcoRag        V2dKsc
077500040324     c                   EndIf
077600040409      * Autotrasportatore
077700040409     c                   Clear                   V2cPdc
077800040409     c                   Clear                   V2dPdc
077900120612     c                   Eval      kNfv = OrmNdc
078000120612     c                   Eval      kFgs = OrmPor
078100120612     c     kDst          Chain     Fidst01l
078200060516      * se non trovo la distinta con il p.o. ritiro controllo se è un p.o. gestito
078300120612     c                   if        not %found(fidst01l)
078400060516     c                   clear                   fnlv55ds
078500060516     c                   eval      d55tpt = '6'
078600060516     c                   move      ormpor        d55lin
078700060516     c                   eval      d55drf = ormddc
078800060516     c                   call      'FNLV55R'
078900060516     c                   parm                    fnlv55ds
079000060516     c                   if        d55err = *blanks and d55tfa <> d55lin
079100120612     c                   move      d55tfa        kfgs
079200120612     c     kdst          chain     fidst01l
079300060516     c                   endif
079400060516     c                   endif
079500120612     c                   If        %Found(Fidst01l)
079600040409     c                   Eval      V2cPdc = DstPdr
079700040409     c                   EndIf
079800040409     c                   Eval      ApdPdr = DstPdr
079900040409     c                   Eval      ApdTip = 'A'
080000040409     c     kApd          Chain     Fiapd01L
080100040409     c                   If        %Found(Fiapd01l)
080200040409     c                   Eval      V2dPdc = ApdRsc
080300040409     c                   EndIf
080400040325
080500040325     c                   Eval      dorm01 = OrmFlo
080600040409      * Commissionato
080700040409     c                   If        §orCom = 'S'
080800040409     c                   Eval      V2dCom = 'COMMISSIONATO'
080900040409     c                   Else
081000040409     c                   Clear                   V2dCom
081100040409     c                   EndIf
081200040616
081300040616      * Bolla in assegnato
081400040616     c                   Eval      *In50 = (§OrPfb = 'A')
081500040324
081600040326e   1c                   EndIf
081700040324
081800040324     c                   EndSr
081900040317
082000040317      *------------------------------------------------------------------------*
082100040317      * GESTIONE ABBINAMENTO ORM A BOLLA
082200040317      *------------------------------------------------------------------------*
082300040324     c     Sr_Abbina     BegSr
082400040318
082500040413     c                   ExSr      Sr_Puld02
082600040413
082700040318do  1c                   Do        *Hival
082800040318     c                   Exfmt     or17d02
082900040318     c                   Setoff                                       28
083000040318
083100040318      * F12=Ritorna a sbfl
083200040413     c   kl              Leave
083300040318      * F7=Interrogazione bolle partenza
083400040318if  2c                   If        *InKg
083500040318     c                   Clear                   dsBlp
083600040615     c                   ExSr      Sr_Puld02
083700040326      * se c'è già la spedizione vado diretta sulla bolla
083800040326if  3c                   If        V2cNsp > *Zeros
083900040326     c                   Eval      dsbaas = V2cAas
084000040326     c                   Eval      dsblnp = V2cLnp
084100040326     c                   Eval      dsbnrs = V2cNrs
084200040326     c                   Eval      dsbnsp = V2cNsp
084300040326     c                   Eval      dsbrsu = rsut
084400040326     c                   Eval      savkpjbu = kpjbu
084500040326     c                   Eval      kpjbu = dsBlp
084600040326     c                   Call      'FNLS05R'
084700040326     c                   Parm                    kpjba
084800040326     c                   Eval      dsBlp = kpjbu
084900040326     c                   Eval      kpjbu = savkpjbu
085000040326    3c                   EndIf
085100040326      * se non c'è la spedizione richiamo il filtro
085200040326if  3c                   If        V2cNsp = *Zeros
085300040326     c                   Eval      dsbflg = '1'
085400040326     c                   Eval      savkpjbu = kpjbu
085500040326     c                   Eval      kpjbu = dsBlp
085600040326     c                   Call      'FNLS04R'
085700040326     c                   Parm                    kpjba
085800040326     c                   Eval      dsBlp = kpjbu
085900040326     c                   Eval      kpjbu = savkpjbu
086000040326    3c                   EndIf
086100040318if  3c                   If        dsbnsp > *Zeros
086200040318     c                   Eval      V2cAas = dsbaas
086300040318     c                   Eval      V2cLnp = dsblnp
086400040318     c                   Eval      V2cNrs = dsbnrs
086500040318     c                   Eval      V2cNsp = dsbnsp
086600040318e   3c                   EndIf
086700040318e   2c                   EndIf
086800040324      * F8=Interrogazione ORM
086900040324if  2c                   If        *InKh
087000040324     c                   Clear                   dsOrm
087100040324     c                   Eval      dsopoe = V2cPoe
087200040324     c                   Eval      dsonor = V2cNor
087300040324     c                   Eval      dsonsr = V2cNsr
087400040324     c                   Eval      dsonrv = V2cNrv
087500040324     c                   Eval      dsoopz = '5'
087600040324     c                   Eval      dsofgs = V1cFgs
087700040324     c                   Eval      dsoref = 'SI'
087800040324     c                   Eval      dsomio = 'S'
087900040324     c                   Eval      savkpjbu = kpjbu
088000040324     c                   Eval      kpjbu = dsOrm
088100040324     c                   Call      'FIOR07R'
088200040324     c                   Parm                    kpjba
088300040324     c                   Eval      dsOrm = kpjbu
088400040324     c                   Eval      kpjbu = savkpjbu
088500040324e   2c                   EndIf
088600040318
088700040318      * controlli
088800040318     c                   ExSr      Sr_CtrD02
088900040318
089000040326     c   28
089100040326     cornkf              Iter
089200040318
089300040318      * F6=Abbina ORM alla bolla
089400040318     c                   ExSr      Sr_AbbOrm
089500070911     c   28              Iter
089600040413     c                   ExSr      Sr_Puld02
089700040318
089800040318e   1c                   EndDo
089900040317
090000040317     c                   EndSr
090100040413
090200040413      *------------------------------------------------------------------------*
090300040413      * PULISCO I DATI DEL VIDEO
090400040413      *------------------------------------------------------------------------*
090500040413     c     Sr_PulD02     BegSr
090600040413
090700040413     c                   Clear                   V2cLnp
090800040413     c                   Clear                   V2cNrs
090900040413     c                   Clear                   V2cNsp
091000040413     c                   Z-add     *year         V2cAas
091100040413     c                   Clear                   V2cCbo
091200040413     c                   Clear                   V2dCbo
091300040413     c                   Clear                   V2cCcm
091400040413     c                   Clear                   V2cRsm
091500040413     c                   Clear                   V2cInm
091600040413     c                   Clear                   V2cCam
091700040413     c                   Clear                   V2cLom
091800040413     c                   Clear                   V2cPrm
091900040413     c                   Clear                   V2cNzm
092000040413     c                   Clear                   V2cCcd
092100040413     c                   Clear                   V2cRsd
092200040413     c                   Clear                   V2cInd
092300040413     c                   Clear                   V2cCad
092400040413     c                   Clear                   V2cLod
092500040413     c                   Clear                   V2cPrd
092600040413     c                   Clear                   V2cNzd
092700040413     c                   Clear                   V2cDrt
092800040413     c                   Clear                   V2cNclb
092900040413     c                   Clear                   V2cPkb
093000040413     c                   Clear                   V2cVlb
093100040413     c                   Clear                   V2cLna
093200040413     c                   Clear                   V2dLna
093300040413     c                   Clear                   V2cPdr
093400040413     c                   Clear                   V2dPdr
093500040413
093600040413     c                   Eval      wfdar = *Off
093700040413
093800040413     c                   EndSr
093900040317
094000040318      *------------------------------------------------------------------------*
094100040318      * - CONTROLLI VIDEATA ABBINAMENTO ORM CON BOLLA
094200040318      *------------------------------------------------------------------------*
094300040318     c     Sr_CtrD02     BegSr
094400040318
094500040318      * Numero bolla obbligatorio
094600040318     c                   If        V2cNsp = *Zeros
094700040318     c                   Eval      *In28 = *On
094800040318     c                   Eval      *In42 = *On
094900040318     c                   Eval      V2cMsg = msg(07)
095000040318     c                   GoTo      EndD02
095100040318     c                   EndIf
095200040318      * Bolla esistente
095300070911     c     kBlp          Chain(N)  Fnblp01l
095400040318     c                   If        Not %Found(Fnblp01l)
095500040318     c                   Eval      *In28 = *On
095600040318     c                   Eval      *In42 = *On
095700040318     c                   Eval      V2cMsg = msg(08)
095800040318     c                   GoTo      EndD02
095900040318     c                   EndIf
096000040318      * Non deve essere già abbinata ad un ORM
096100060214     c     kar4          Setll     Fiar401l
096200060214     c                   If        %Equal(Fiar401l)
096300040318     c                   Eval      *In28 = *On
096400040318     c                   Eval      *In42 = *On
096500040318     c                   Eval      V2cMsg = msg(09)
096600040318     c                   GoTo      EndD02
096700040318     c                   EndIf
096800040318      * Non deve essere una bolla di recupero
096900040325     c                   Clear                   ds3a
097000040325     c                   Eval      kkey = BlpCbo
097100040325     c     kTab          Chain     Tabel00f
097200040325     c                   If        %Found(Tabel00f)
097300040325     c                   Eval      ds3a = TblUni
097400040325     c                   EndIf
097500040325     c                   If        §3aRbl = 'R'
097600040318     c                   Eval      *In28 = *On
097700040318     c                   Eval      *In42 = *On
097800040318     c                   Eval      V2cMsg = msg(11)
097900040318     c                   GoTo      EndD02
098000040325     c                   EndIf
098100040325      * Carico i dati della bolla
098200040325     c                   ExSr      Sr_CarBolla
098300040325
098400040325      * Se l'ORM ha la serie il tipo bolla deve essere uguale a quello in tabella OSR
098500040325     c                   If        V2cNsr <> *Zeros and V2cCbo <> d§OsrCbo
098600040331     c                   Move      d§OsrCbo      w002a
098700040325     c                   Eval      *In28 = *On
098800040325     c                   Eval      *In42 = *On
098900040325     c                   Eval      V2cMsg = msg(12)
099000040325     c                   Eval      V2cMsg = %replace(w002a:V2cMsg:
099100040325     c                                      %scan('__':V2cMsg))
099200040325     c                   GoTo      EndD02
099300040325     c                   EndIf
099400040325
099500040330      * Se ORM è prepagato la bolla deve essere in Franco e avere data fattura > 0
099600040325     c                   If        OrmTor = 'P' and wtb1 <> 'F'
099700040325     c                   Eval      *In28 = *On
099800040325     c                   Eval      *In42 = *On
099900040325     c                   Eval      V2cMsg = msg(13)
100000040325     c                   GoTo      EndD02
100100040325     c                   EndIf
100200040325     c                   If        OrmTor = 'P'
100300040330     c                   Eval      Ar6Trc = '1'
100400040330     c     kAr6          Chain     Fiar601l
100500040330     c                   If        %Found(Fiar601l) and Ar6Dft = *Zeros
100600040325     c                   Eval      *In28 = *On
100700040325     c                   Eval      *In42 = *On
100800040330     c                   Eval      V2cMsg = msg(13)
100900040325     c                   GoTo      EndD02
101000040325     c                   EndIf
101100040325     c                   EndIf
101200040325
101300040325      * Se l'ORM richiede la bolla in Assegnato non posso abbinare una bolla Franco
101400040325     c                   If        §OrPfb = 'A' and wtb1 = 'F'
101500040325     c                   Eval      *In28 = *On
101600040325     c                   Eval      *In42 = *On
101700040326     c                   Eval      V2cMsg = msg(15)
101800040325     c                   GoTo      EndD02
101900040325     c                   EndIf
102000040318
102100040325      * La linea di partenza della bolla deve essere uguale al p.o. ritiro
102200040325     c                   If        OrmPor <> BlpLnp
102300040325     c                   Eval      *In28 = *On
102400040325     c                   Eval      *In42 = *On
102500040325     c                   Eval      V2cMsg = msg(16)
102600040325     c                   GoTo      EndD02
102700040325     c                   EndIf
102800040325
102900040325      * Se l'ORM ha il p.o. consegna la bolla deve avere la linea di arrivo uguale
103000040325     c                   If        OrmPoc > *Zeros and OrmPoc <> BlpLna
103100040325     c                   Eval      *In28 = *On
103200040325     c                   Eval      *In42 = *On
103300040325     c                   Eval      V2cMsg = msg(17)
103400040325     c                   GoTo      EndD02
103500040325     c                   EndIf
103600040325
103700040325      * se l'ORM prevede:
103800040325      *               --> la bolla in porto Assegnato
103900040325      *               --> paga ordinante orm
104000040325      *               --> destinatario orm estero
104100040325      *               --> ordinante orm italia
104200040325     c                   If        (§OrPfb = 'A' and OrmPag = 'O' and
104300040325     c                              OrmNac <> *Blanks and OrmNao = *Blanks)
104400040325      * la bolla deve essere un porto franco
104500040325     c                              and wtb1 = 'A'
104600040325     c                   Eval      *In28 = *On
104700040325     c                   Eval      *In42 = *On
104800040325     c                   Eval      V2cMsg = msg(18)
104900040325     c                   GoTo      EndD02
105000040325     c                   EndIf
105100040326
105200040331      * Il p.o. emissione dell'ORM è DPD, FedEx, EEX
105300040331     c                   Clear                   og143
105400040331     c     Ormpoe        Chain     Azorg01l
105500040331     c                   If        %Found(Azorg01l)
105600040331     c                   Eval      og143 = OrgDe3
105700040331     c                   EndIf
105800040326     c                   If        (§OgNtw = 'DPD' or §OgNtw = 'FED' or
105900040331     c                              §OgNtw = 'EEX')
106000040331      * La bolla abbinata deve essere una bolla export DPD, FedEx, EEX in porto assegnato
106100040331     c                   If        wtb1 <> 'A' or BlpLna <> OrmPoe
106200040326     c                   Eval      *In28 = *On
106300040326     c                   Eval      *In42 = *On
106400040326     c                   Eval      V2cMsg = msg(19)
106500040326     c                   Eval      V2cMsg = %replace(§OgNtw:V2cMsg:
106600040326     c                                      %scan('___':V2cMsg))
106700040331     c                   Eval      %Subst(V2cMsg:53:3) = §OgNtw
106800040326     c                   GoTo      EndD02
106900040326     c                   EndIf
107000040326     c                   EndIf
107100040326
107200040326      * Controllo formale tra la data ritiro ORM e la data ritiro merce
107300070911     c                   Clear                   og143
107400070920     c                   Clear                   wimport
107500070911     c     ormpor        Chain     Azorg01l
107600070911     c                   If        %Found(Azorg01l)
107700070911     c                   Eval      og143 = OrgDe3
107800070920     c                   if        §ogntw='DPD' or §ogntw='EEX' or
107900070920     c                             §ogntw='FED'
108000070920     c                   eval      wimport='S'
108100070920     c                   endif
108200070911     c                   EndIf
108300071107     c****               if        dateu<§vpodrt
108400071107     c***                If        wfdar = *Off and OrmDar <> BlpDrt
108500071107     c***                Eval      *In28 = *On
108600071107     c***                Eval      *In42 = *On
108700071107     c***                Eval      V2cMsg = msg(20)
108800071107     c***                Eval      wfdar = *On
108900071107     c****               GoTo      EndD02
109000071107     c****               EndIf
109100071107     c****               else
109200070907     c                   If        wfdar = *Off and w_Dar <> BlpDrt
109300070920     c                             and wimport=*blanks
109400070907     c                   Eval      *In28 = *On
109500070907     c                   Eval      *In42 = *On
109600070907     c                   Eval      V2cMsg = msg(22)
109700070907     c                   Eval      wfdar = *On
109800070907     c                   GoTo      EndD02
109900070907     c                   EndIf
110000071107     c*****              endif
110100051114
110200051114      * Controllo se la spedizione è stata fatta da + di 10 giorni
110300071107    1c****               if        dateu<§vpodrt
110400071107    2c***                If        blpdsp < wdata
110500071107     c***                Eval      *In28 = *On
110600071107     c***                Eval      *In42 = *On
110700071107     c***                Eval      V2cMsg = msg(21)
110800071107     c***                GoTo      EndD02
110900071107    2c***                EndIf
111000071107   x1c****               else
111100070910     c* Se bolla già partita:
111200070910      * Errore se la spedizione è stata fatta da + di 1 giorno lav.
111300070920    2c                   if        blpft2<>*blanks and blpdsp<wdata
111400070907     c                   Eval      *In28 = *On
111500070907     c                   Eval      *In42 = *On
111600070907     c                   Eval      V2cMsg = msg(23)
111700070907     c                   GoTo      EndD02
111800070920    2c                   endif
111900070920     c* Se cambia la data ritiro:
112000070910      * controllo che per la data ritiro i conteggi non siano già stati
112100070910      * confermati. Se la spedizione è già partita è anche necessario che
112200070910      * i conteggi siano presenti
112300070920    2c                   if        w_dar<>blpdrt
112400070920     c                             and wimport=*blanks
112500070910     C                   MOVEL     ' '           W01TSR
112600070910     C                   MOVEL     'C'           W01FVL
112700070910     C     KFTT          SETLL     FIFTT000                               90
112800070910     C  N90KFTT          SETLL     FIFST000                               90
112900070910     C*
113000070920    3C                   IF        *in90
113100070910     c                   Eval      *In28 = *On
113200070910     c                   Eval      *In42 = *On
113300070910     C                   eval      v2cmsg= msg(24)
113400070910     C                   goto      endd02
113500070920    3C                   endif
113600070919     c* ripeto il controllo per la vecchia data ritiro
113700070919     C                   MOVEL     ' '           W01TSR
113800070919     C                   MOVEL     'C'           W01FVL
113900070919     C     KFTTold       SETLL     FIFTT000                               90
114000070919     C  N90KFTTold       SETLL     FIFST000                               90
114100070919     C*
114200070920    3C                   IF        *in90
114300070919     c                   Eval      *In28 = *On
114400070919     c                   Eval      *In42 = *On
114500070919     C                   eval      v2cmsg= msg(24)
114600070919     C                   goto      endd02
114700070920    3C                   endif
114800070920    3c                   if        blpft2<>*blanks
114900070919     c                   exsr      sr_ctrftt
115000070919     c   28              goto      endd02
115100070920    3c                   endif
115200070920    2c                   endif
115300070910     c
115400071107    1c*****              endif
115500040325
115600040318     c     EndD02        EndSr
115700040325
115800040325      *------------------------------------------------------------------------*
115900040325      * - CARICA I DATI DELLA BOLLA
116000040325      *------------------------------------------------------------------------*
116100040325     c     Sr_Carbolla   BegSr
116200040325
116300040325      * Tipo bolla
116400040325     c                   Clear                   V2dCbo
116500040325     c                   Eval      V2cCbo = BlpCbo
116600040325     c                   Eval      V2dCbo = §3aDes
116700040325     c                   Movel     §3aTb1        wtb1
116800040325
116900040325      * Mittente
117000040325      * se bolla Franco prendo KSC
117100040325     c                   If        wtb1 = 'F'
117200040325     c                   Eval      V2cCcm = BlpKsc
117300040325     c                   EndIf
117400040325      * se bolla Assegnato prendo CCM
117500040325     c                   If        wtb1 = 'A'
117600040325     c                   Eval      V2cCcm = BlpCcm
117700040325     c                   EndIf
117800040325     c                   Eval      V2cRsm = BlpRsm
117900040325     c                   Eval      V2cInm = BlpInm
118000040325     c                   Eval      V2cCam = BlpCam
118100040325     c                   Eval      V2cLom = BlpLom
118200040325     c                   Eval      V2cPrm = BlpPrm
118300040325     c                   Eval      V2cNzm = BlpNzm
118400040325
118500040325      * Destinatario
118600040325      * se bolla Assegnato prendo KSC
118700040325     c                   If        wtb1 = 'A'
118800040325     c                   Eval      V2cCcd = BlpKsc
118900040325      * Cerco anche su Fiar6
119000040326     c                   Eval      Ar6Trc = '1'
119100040325     c     kAr6          Chain     Fiar601l
119200040325     c                   If        %Found(Fiar601l) and Ar6Ksc > *Zeros
119300040325     c                   Eval      V2cCcd = Ar6Ksc
119400040325     c                   EndIf
119500040325     c                   EndIf
119600040325      * se bolla Franco pulisco il codice
119700040325     c                   If        wtb1 = 'F'
119800040325     c                   Clear                   V2cCcd
119900040325     c                   EndIf
120000040325     c                   Eval      V2cRsd = BlpRsd
120100040325     c                   Eval      V2cInd = BlpInd
120200040325     c                   Eval      V2cCad = BlpCad
120300040325     c                   Eval      V2cLod = BlpLod
120400040325     c                   Eval      V2cPrd = BlpPrd
120500040325     c                   Eval      V2cNzd = BlpNzd
120600040325
120700040325      * Data ritiro merce
120800040325     c                   Z-add     BlpDrt        g02inv
120900040325     c                   Movel     '3'           g02err
121000040325     c                   Call      'XSRDA8'
121100040325     c                   Parm                    wlbdat
121200040325     c                   Z-add     g02dat        V2cDrt
121300070913     c                   z-add     blpdrt        w_olddrt
121400040325
121500040325      * Colli - Peso - Volume
121600040325     c                   Eval      V2cnclb = BlpNcl
121700040325     c                   Eval      V2cpkb = BlpPkb
121800040325     c                   Eval      V2cvlb = BlpVlb
121900040325
122000040325      * Linea di arrivo
122100040325     c                   Clear                   V2dLna
122200040325     c                   Eval      V2cLna = BlpLna
122300040325     c     BlpLna        Chain     Azorg01l
122400040325     c                   If        %Found(Azorg01l)
122500040325     c                   Eval      V2dLna = OrgDes
122600040325     c                   EndIf
122700040325
122800040325      * Autotrasportatore
122900040325     c                   Clear                   V2dPdr
123000070924     c                   Clear                   wpdd
123100040325     c                   Eval      V2cPdr = BlpPdr
123200040325     c                   Eval      ApdPdr = BlpPdr
123300040325     c                   Eval      ApdTip = 'A'
123400040325     c     kApd          Chain     Fiapd01L
123500040325     c                   If        %Found(Fiapd01l)
123600040325     c                   Eval      V2dPdr = ApdRsc
123700070924     c                   if        apdpdd='S'
123800070924     c                   eval      wpdd='S'
123900070924     c                   endif
124000040325     c                   EndIf
124100040325
124200040325     c                   EndSr
124300040318
124400040318      *------------------------------------------------------------------------*
124500040318      * - ABBINA ORM ALLA BOLLA
124600040318      *------------------------------------------------------------------------*
124700040318     c     Sr_AbbOrm     BegSr
124800040615
124900070920     c* richaino blp per vedere se nel frattempo la bolla è partita
125000070919     c                   move      blpft2        savft2
125100070920     c     kblp          chain(N)  fnblp01l
125200070919     c* Bolla nel frattempo partita: devo eseguire i controlli per
125300070919     c* bolla partita
125400071107     c****               if        dateu>=§vpodrt and  w_dar<>blpdrt
125500071107     c                   if        w_dar<>blpdrt
125600070920     c                             and wimport=*blanks
125700070920     c                             and savft2=*blanks and savft2<>blpft2
125800070919     c                   exsr      sr_ctrftt
125900070919     c   28              goto      endabbina
126000070919     c                   endif
126100070919     c*
126200040615s   0c                   Select
126300040615      * Se bolla non partita e non borderizzata richiamo la manutenzione bolle
126400040615w   0c                   When      BlpFt2 = *Blanks and BlpDbr = *Zeros
126500040615      * richiamo il programma per la scelta delle stampanti
126600040615     c                   Eval      wdatibolla = 'BollaAdd.'
126700040615     c                   Eval      wdatibolA4 = 'BolAdd_A4'
126800040615     c                   Eval      wdatibolA5 = 'BolAdd_A5'
126900040615     c                   exsr      sr_scestp
127000040615     c   92              goto      endabbina
127100040615
127200040615     c                   clear                   kpjbu
127300040615     c                   Clear                   dblp
127400040615     c                   Clear                   dtasv
127500040615     c                   Eval      §LpAas = V2cAas
127600040615     c                   Eval      §LpLnp = V2cLnp
127700040615     c                   Eval      §LpNrs = V2cNrs
127800040615     c                   Eval      §LpNsp = V2cNsp
127900040615     c                   Eval      §LpFpr = 'M'
128000040615     c                   Call      'FNLS01R'
128100040615     c                   Parm                    kpjba
128200040615     c                   Parm                    dblp
128300040615     c                   Parm                    dtasv
128400040615     c                   Parm                    Trul90ds
128500040615     c                   if        §lpfpr = '4' or §lpfpr = '3'
128600040615     c                             or §lpfpr = '5'
128700040615     c                   eval      wabb = *Off
128800040615     c                   else
128900040615     c                   eval      wabb = *On
129000040615     c                   endif
129100040615
129200060214      * Se bolla borderizzata e non partita aggiorno in automatico solo Fiar4
129300070911      * a partire dalla data decorrenza per data ritiro aggiorno anche
129400070911      * la data ritiro
129500040615w   0c                   When      BlpFt2 = *Blanks and BlpDbr > *Zeros
129600070911      * Aggiorno la data ritiro su fnblp
129700071107     c****               if        dateu>=§vpodrt and blpdrt<>w_dar
129800071107     c                   if        blpdrt<>w_dar
129900070920     c                             and wimport=*blaNKS
130000070911     c                   exsr      sr_aggdrt
130100070920     c   28              goto      endabbina
130200070920     c                   endif
130300060214      * Scrivo Fiar4 rcd 'M'
130400040615     c                   Clear                   Fnlv19ds
130500040615     c                   Clear                   dsbl4m
130504151104     c
130600060214     c**!!!              Eval      D19Tbo = 'P'
130700060215     c                   Eval      d19ftr = 'T'
130800040615     c                   Eval      D19Aas = V2cAas
130900040615     c                   Eval      D19Lnp = V2cLnp
131000040615     c                   Eval      D19Nrs = V2cNrs
131100040615     c                   Eval      D19Nsp = V2cNsp
131200040615     c                   Eval      §b4Poe = V2cPoe
131300040615     c                   Eval      §b4Nsr = V2cNsr
131400040615     c                   Eval      §b4Nor = V2cNor
131500040615     c                   Eval      §b4Nrv = V2cNrv
131600040615     c                   Eval      D19Trc = 'M'
131700040615     c                   Eval      D19Nt1 = dsBl4m
131800040615     c                   Call      'FNLV19R'
131900040615     c                   Parm                    Fnlv19ds
132000040615     c                   eval      wabb = *On
132100040615
132200040615      * Se bolla partita aggiorno in automatico
132300040615   x0c                   Other
132400070920      *
132500071107    1c****               if        dateu>=§vpodrt and blpdrt<>w_dar
132501151105     c* Impostazione campo per addebito ORM telefonici
132502151105     c                   clear                   wb4ado
132503151106     c* Orm telefonico
132504151106    1c                   if        (wtco='M' or wtco='E')
132505151106     c                   eval      wprep='N'
132506151106     c* se non è un assegnato verifico se prepagato
132507151106    2c                   if        wtb1<>'A'
132508151106     c                   Eval      Ar6Trc = '1'
132509151106     c     kAr6          Chain     Fiar601l
132510151106    3c                   If        %Found(Fiar601l) and Ar6Dft > *Zeros
132511151106     c                   eval      wprep='S'
132512151106    3c                   endif
132513151106    2c                   endif
132514151106     c* no prepagato e no porto assegnato
132515151113    2c                   if        wtb1<>'A' and wprep='N' and wimport=*blanks
132516151105     c                   exsr      sr_b4ado
132517151106    2c                   endif
132518151106    1c                   endif
132600071107    1c                   if        blpdrt<>w_dar
132700070924     c                             and wimport=*blanks and wpdd<>'S'
132800070920      * Aggiorno la data ritiro su fnblp
132900070912     c                   exsr      sr_aggdrt
133000070920     c   28              goto      endabbina
133100070914      * aggiorno i conteggi (richiamo due volte:la seconda per le simulazioni)
133200070914     c                   setoff                                       17
133300070920    2c                   do        2
133400070912     c                   exsr      sr_aggaut
133500070914     c                   seton                                        17
133600070920    2c                   enddo
133700070912      * Aggiorno la data ritiro su titas
133701151105     c                   eval      waggtasD='S'
133702151105     c                   if        wb4ado='S'
133703151105     c                   eval      waggtasO='S'
133704151105     c                   else
133705151105     c                   eval      waggtasO=' '
133706151105     c                   endif
133800151105     c                   exsr      sr_aggtas
133900070912      * ricalcolo l'affidabilità
134000070912     c                   exsr      sr_ricaff
134001151105     c                   else
134002151105     c* se non è cambiata la data ritiro ma c'è addebito per orm telefonico
134003151105     c* aggiorno comunque TITAS
134004151105     c                   if        wb4ado='S'
134005151105     c                   eval      waggtasD=' '
134006151105     c                   eval      waggtasO='S'
134007151105     c                   exsr      sr_aggtas
134008151105     c                   endif
134100070920    1c                   endif
134200040615
134300060214      * Scrivo Fiar4 rcd 'M'
134400040318     c                   Clear                   Fnlv19ds
134500040326     c                   Clear                   dsbl4m
134600060214     c**!!!              Eval      D19Tbo = 'P'
134700060215     c                   Eval      d19ftr = 'T'
134800040318     c                   Eval      D19Aas = V2cAas
134900040318     c                   Eval      D19Lnp = V2cLnp
135000040318     c                   Eval      D19Nrs = V2cNrs
135100040318     c                   Eval      D19Nsp = V2cNsp
135200040318     c                   Eval      §b4Poe = V2cPoe
135300040318     c                   Eval      §b4Nsr = V2cNsr
135400040318     c                   Eval      §b4Nor = V2cNor
135500040318     c                   Eval      §b4Nrv = V2cNrv
135501151105     c                   Eval      §b4ado = wb4ado
135600040318     c                   Eval      D19Trc = 'M'
135700040318     c                   Eval      D19Nt1 = dsBl4m
135800110322     c
135900110322      * Aggiorno flag PDA su fiar5  prima di aggiornare fiar4
136000110322     c                   exsr      AggioAR5PDA
136100110322     c*
136200040318     c                   Call      'FNLV19R'
136300040318     c                   Parm                    Fnlv19ds
136400040326
136500040326      * Scrivo Fnar4 rcd 'M'
136600060214     c**!!!              Eval      D19Tbo = 'A'
136700060214     c**!!!              Call      'FNLV19R'
136800060214     c**!!!              Parm                    Fnlv19ds
136900040326
137000040326      * Apro il Tita4 della sede
137100040326     c                   If        Not %Open(Tita400f)
137200040615     c                   Eval      lenght = 80
137300040326     c                   Clear                   comman
137400040326     c                   Movel(p)  cmd(1)        comman
137500040326     c                   Eval      %Subst(comman:30:10) = wlib
137600040330     c                   Eval      comman =
137700040330     c                             %trim(comman) + '/TITA400F)'
137800040326     c                   Call      'QCMDEXC'                            99
137900040326     c                   Parm                    comman
138000040326     c                   Parm                    lenght
138100040326     c  n99              Open      Tita400f
138200040326     c                   EndIf
138300040326      * Scrivo Tita4 rcd 'M'
138400040326     c                   If        Not *In99
138500040326     c                   Clear                   Tita4000
138600040326     c                   Eval      Ta4Aas = V2cAas
138700040326     c                   Eval      Ta4Lnp = V2cLnp
138800040326     c                   Eval      Ta4Nrs = V2cNrs
138900040326     c                   Eval      Ta4Nsp = V2cNsp
139000040326     c                   Eval      Ta4Trc = D19Trc
139100040326     c                   Eval      Ta4Not = D19Nt1
139200040326     c                   Write     Tita4000
139300040326     c                   EndIf
139400040318
139500040615      * L'Orm lo passo alla fase 900
139600040413if  1c                   If        wabb = *Off
139700040326     c     kOrm          Chain     Fnorm01l
139800040413if  2c                   If        %Found(Fnorm01l)
139900040615     c                   Eval      OrmFao = 900
140000040326     c                   Z-add     *Date         OrmDfo
140100040326     c                   Time                    OrmOfo
140200040908     c                   Z-add     *Date         OrmDtt
140300070710      * imposto ormeti
140400070710     c                   clear                   dfar
140500070710     c                   clear                   tibs02ds
140600070710     c                   eval      t02mod = 'C'
140700070710     c                   eval      t02sif = knsif
140800070710     c                   eval      t02cod = 'FAR'
140900070710     c                   movel(p)  ormfao        t02ke1
141000070710     c                   call      'TIBS02R'
141100070710     c                   parm                    kpjba
141200070710     c                   parm                    tibs02ds
141300070710     c                   eval      dfar = t02uni
141400070710     c                   eval      ormeti = d§farass
141500040326      * Scrivo la nuova fase
141600040326     c                   ExSr      Sr_WrtOrf
141700040326      * Aggiorno l'ORM
141800040326     c                   Update    Fnorm01
141900040413     c                   Eval      wabb = *On
142000040413e   2c                   EndIf
142100040413
142200040326e   1c                   EndIf
142300040615
142400040615e   0c                   EndSl
142500040318
142600040615     c     endabbina     EndSr
142700040326
142800070919      *------------------------------------------------------------------------*
142900070919      * - Controllo presenza conteggi per bolla già partita
143000070919      *------------------------------------------------------------------------*
143100070919     c     Sr_CtrFtt     BegSr
143200070919     c                   eval      w01tsr='R'
143300070919    2C     blpfpp        IFEQ      'M'
143400070919     C                   Z-ADD     1000          W_ndc
143500070919     C                   ELSE
143600070919     C                   Z-ADD     5000          W_ndc
143700070919    2C                   ENDIF
143800070919     C                   ADD       blpnrt        W_ndc
143900070919     c
144000070919     c     kftt1         setll     fiftt001
144100070919     c                   if        not %equal(fiftt01l)
144200070919     c                   Eval      *In28 = *On
144300070919     c                   Eval      *In42 = *On
144400070919     c                   Eval      V2cMsg = msg(25)
144500070919     c                   endif
144600070919     c                   endsr
144700070911      *------------------------------------------------------------------------*
144800070911      * - Aggiornamento data ritiro su FNBLP
144900070911      *------------------------------------------------------------------------*
145000070911     c     Sr_AggDrt     BegSr
145100070911
145200070920     c     kblp          chain(E)  fnblp01l
145300070920     c                   if        %error
145400070920     c                   eval      *in28='1'
145500070920     c                   Eval      *In42 = *On
145600070920     c                   Eval      V2cMsg = msg(26)
145700070920     c                   goto      enddrt
145800070920     c                   endif
145900090403     c
146000090403     c* Richiamo il pgm per la rielaborazione delle statistiche
146100090403     c                   z-add     blpncl        st2ncl
146200090403     C                   MOVE      blpdrt        SA2DTS            8 0
146300090403     C                   MOVE      'P'           SA2TRC            1
146400090403     C                   exsr      sr_callsa2r3
146500090403     C                   MOVE      'T'           SA2TRC            1
146600090403     C                   exsr      sr_callsa2r3
146700090403     c
146800090403     C                   MOVE      w_dar         SA2DTS            8 0
146900090403     C                   MOVE      'P'           SA2TRC            1
147000090403     C                   exsr      sr_callsa2r3
147100090403     C                   MOVE      'T'           SA2TRC            1
147200090403     C                   exsr      sr_callsa2r3
147300090403     c
147400070911     c                   z-add     w_dar         blpdrt
147500070912     c                   except    aggblp
147600070911
147700070911     c     enddrt        endsr
147800110322      *------------------------------------------------------------------------*
147900110322      * - Aggiornamento flag PDA orm su fiar5
148000110322      *------------------------------------------------------------------------*
148100110322     c     AggioAR5PDA   BegSr
148200110322     c                   eval      RitiroPDA=' '
148300110322     c
148400110322     c     korg          chain     fnorg01l
148500110322    2c                   if        %found(fnorg01l) and R_orgatb=' '
148600110322     c                   eval      kfgs=r_orgfgs
148700110322     c                   eval      knfv=r_orgndc
148800110322     c
148900120612     c     kdst          chain     fidst01l
149000120612    3c                   if        %found(fidst01l) and dstatb=' '
149100110322
149200120612    4c                   if        dstpda='O' or dstpda='E'
149300120612     c                   eval      DDSTFLR =  DstFLR
149400110322    5c                   if        §dstTSTPDA=' ' or §dstTSTPDA='C'
149500110322     c                   eval      RitiroPDA='S'
149600110322    5c                   endif
149700110322    4c                   endif
149800110322     c
149900110322    3c                   endif
150000110322    2c                   endif
150100110322
150200110322     c     kar5          chain     fiar501l
150300110322    1c                   if        %found(fiar501l)
150400110322     c                   movel     ar5uni        dar5gen
150500110322     c* Aggiorno solo se diverso
150600110322    2c                   if        §ar5PDArt<>RitiroPDA
150700110322     c                   eval      §ar5PDArt=RitiroPDA
150800110322     c
150900110322     c                   movel     dar5gen       ar5uni
151000110322     c                   clear                   ar5dt2
151100110324     c                   clear                   ar5ft2
151200110324     c                   clear                   ar5ft3
151300110322     c                   update    fiar5000
151400110322   x2c                   else
151500110322     c                   unlock    fiar501l
151600110322    2c                   endif
151700110322     c
151800110322   x1c                   else
151900110322
152000110322     c* Scrivo solo se = 'Si'
152100110322    2c                   if        ritiroPDA='S'
152200110322     c                   clear                   fiar5000
152300110322     c                   eval      ar5aas=blpaas
152400110322     c                   eval      ar5lnp=blplnp
152500110322     c                   eval      ar5nrs=blpnrs
152600110322     c                   eval      ar5nsp=blpnsp
152700110322     c                   eval      ar5trd='GEN'
152800110322     c                   z-add     *Date         ar5dac
152900110322     c                   time                    ar5orc
153000110322     c                   clear                   dar5gen
153100110322     c                   eval      §ar5PDArt='S'
153200110322     c                   movel     dar5gen       ar5uni
153300110324     c                   clear                   ar5ft2
153400110324     c                   clear                   ar5ft3
153500110322      *
153600110322     C                   WRITE     FIAR5000                             38
153700110322e   2c                   ENDIF
153800110322e   1c                   ENDIF
153900110322     c
154000110322
154100110322     c                   endsr
154200090403      *--------------------------------------------------------------*
154300090403      * Richiamo pgm per rielaborazione statistiche partito e a terra
154400090403      *--------------------------------------------------------------*
154500090403     c     sr_callsa2r3  BegSr
154600090403     C                   CALL      'FNLSA2R3'
154700090403     C                   PARM                    SA2TLA            1
154800090403     C                   PARM                    SA2TRC
154900090403     C                   PARM                    SA2DTS
155000090403     C                   PARM                    WWLNA             3 0
155100090403     C                   PARM                    st2ncl            5 0
155200090403     c                   ENDSR
155300070912      *------------------------------------------------------------------------*
155400151105      * - Aggiornamento data ritiro su TITAS  e/o flag per addebito orm telefonico
155500070912      *------------------------------------------------------------------------*
155600151105     c     Sr_AggTas     BegSr
155700070912
155800070912      * Apro il Titas della sede
155900070912    2c                   If        Not %Open(Titas30c)
156000070912     c                   Eval      lenght = 80
156100070912     c                   Clear                   comman
156200070912     c                   Movel(p)  cmd(2)        comman
156300070912     c                   Eval      %Subst(comman:30:10) = wlib
156400070912     c                   Eval      comman =
156500070912     c                             %trim(comman) + '/TITAS30C)'
156600070912     c                   Call      'QCMDEXC'                            98
156700070912     c                   Parm                    comman
156800070912     c                   Parm                    lenght
156900070912     c  n98              Open      TITAS30C
157000070912    2c                   EndIf
157100070914      *
157200070912    2c                   If        Not *In98
157300070912     c     kblp          chain(E)  titas30c
157400070912    3c                   if        %error
157500070919     c* reperisco il RRN del file
157600070920     c     kblp          chain(n)  titas30c
157700070919     C* Valorizzo la DS di procedura
157800070920     C                   CLEAR                   TRUL82DS
157900070920     c                   eval      ul82§rrn = tasnrr
158000070920     C   22              EVAL      UL82§FIL = 'TITAS00F'
158100070920     C   23              EVAL      UL82§FIL = 'TITAS10F'
158200070920     C                   EVAL      UL82§num = 1
158300070920     C                   EVAL      UL82§att = 2
158400070919     C* Effettuo la chiamata al *PGM d utilità
158500070920     C                   CALL(e)   'TRUL82R'
158600070920     C                   PARM                    TRUL82DS
158700070919     c     kblp          chain     titas30c
158800070912    3c                   endif
158801151105     c                   if        waggtasD='S'
158900070912     c                   z-add     w_dar         tasdrt
158901151105     c                   endif
158902151105     c                   if        waggtasO='S'
158903151105     c                   eval      dtasflo=tasflo
158904151105     c                   eval      §floado=wb4ado
158905151105     c                   eval      tasflo =dtasflo
158906151105     c                   endif
159000070912     c   22              except    agtas0
159100070912     c   23              except    agtas1
159200070912    2c                   endif
159300070912
159400070912     c     enddrttas     endsr
159500070912      *------------------------------------------------------------------------*
159600070913      * - Aggiornamento conteggi autotrasportatori
159700070912      *------------------------------------------------------------------------*
159800070912     c     Sr_AggAut     BegSr
159900070913     c* NUOVA DATA RITIRO
160000070919     c  n17kftt1         chain(E)  fiftt001                           30
160100070919     c   17kftt1         chain(E)  fifst001                           30
160200070919     c* se errore ritento per una volta
160300070919     c                   if        %error
160400070920     c* reperisco il RRN del file
160500070920     c  n17kftt1         chain(n)  fiftt001
160600070920     c     kftt1         chain(n)  fifst001
160700070920     C* Valorizzo la DS di procedura
160800070920     C                   CLEAR                   TRUL82DS
160900070920     c  n17              eval      ul82§rrn = fttnrr
161000070920     c   17              eval      ul82§rrn = fstnrr
161100070920     C  n17              EVAL      UL82§FIL = 'FIFTT00F'
161200070920     C   17              EVAL      UL82§FIL = 'FIFST00F'
161300070920     C                   EVAL      UL82§num = 1
161400070920     C                   EVAL      UL82§att = 2
161500070920     C* Effettuo la chiamata al *PGM d utilità
161600070920     C                   CALL(e)   'TRUL82R'
161700070920     C                   PARM                    TRUL82DS
161800070919     c  n17kftt1         chain     fiftt001                           30
161900070919     c   17kftt1         chain     fifst001                           30
162000070919     c                   endif
162100070914    1c                   if        *in30='0' and fttfvl<>'C'
162200070919     c  n17kftd5         chain(E)  fiftd05l                           30
162300070919     c   17kftd5         chain(E)  fifsd05l                           30
162400070919     c                   if        %error
162500070920     c* reperisco il RRN del file
162600070920     c  n17kftd5         chain(n)  fiftd05l
162700070920     c     kftd5         chain(n)  fifsd05l
162800070920     C* Valorizzo la DS di procedura
162900070920     C                   CLEAR                   TRUL82DS
163000070920     c  n17              eval      ul82§rrn = ftdnrr
163100070920     c   17              eval      ul82§rrn = fsdnrr
163200070920     C  n17              EVAL      UL82§FIL = 'FIFTD00F'
163300070920     C   17              EVAL      UL82§FIL = 'FIFSD00F'
163400070920     C                   EVAL      UL82§num = 1
163500070920     C                   EVAL      UL82§att = 2
163600070920     C* Effettuo la chiamata al *PGM d utilità
163700070920     C                   CALL(e)   'TRUL82R'
163800070920     C                   PARM                    TRUL82DS
163900070919     c  n17kftd5         chain     fiftd05l                           30
164000070919     c   17kftd5         chain     fifsd05l                           30
164100070919     c                   endif
164200070914    2c                   if        *in30='0'
164300070914     c* numero di stop
164400070914     c                   clear                   fnlva7ds
164500070914     c                   eval      ia7pdr=ftdpdr
164600070914     c                   eval      ia7ndc=ftdndc
164700070914     c                   eval      ia7ddc=w_dar
164800070914     c                   eval      ia7ksc=ftdksc
164900070914     c                   eval      ia7rsm=ftdrsc
165000070914     c   17              eval      ia7sml='S'
165100081027     c                   call      'FNLVA7C'
165200070914     c                   parm                    kpjba
165300070914     c                   parm                    fnlva7ds
165400070914     c                   z-add     oa7stp        ftdstp
165500070914     c* data distinta
165600070913     c                   z-add     w_dar         ftdddc
165700070919     c  n17              update    fiftd000
165800070919     c   17              update    fifsd000
165900070914     c* numero spedizioni ritirate
166000070913     c                   eval      fttset=fttset+1
166100070914     c  n17              update    fiftt001
166200070914     c   17              update    fifst001
166300070913     c* se conteggio valorizzato svalorizzo
166400070914    3C     fttfvl        ifeq      'V'
166500070913     C                   MOVEL     FTDPDR        PA3PDR                         *COD. PADRONCINO
166600070913     C                   MOVEL     FTDTSR        PA3TSR                         *TIPO PRESTAZIONE
166700070913     C                   Z-ADD     FTDNDC        PA3NDC                         *NUMERO DISTINTA
166800070913     C                   Z-ADD     FTDDDC        PA3DDC                         *DATA   DISTINTA
166900070913     C                   MOVEL     'N'           PA3FCT                         *FLG COD.TARIFFA
167000070913     C                   MOVEL     'N'           PA3FVD                         *FLG VAL.MANUAL.DETT
167100070913     C                   MOVEL     'N'           PA3FVT                         *FLG VAL.MANUAL.TEST
167200070913     C  n17              MOVEL     ' '           PA3sml                         *FLG VAL.MANUAL.TEST
167300070913     C   17              MOVEL     'S'           PA3sml                         *FLG VAL.MANUAL.TEST
167400070913     C                   MOVEL     PARAM3        KPJBU
167500070913     C  N17              CALL      'FICNE2R'
167600070913     C                   PARM                    KPJBA
167700070913     C   17              CALL      'FICNE9C'
167800070913     C                   PARM                    KPJBA
167900070914    3C                   endif
168000070914    2c                   endif
168100070914    1c                   endif
168200070913     c* VECCHIA DATA RITIRO
168300070919     c  n17kftt1old      chain(E)  fiftt01l
168400070919     c   17kftt1old      chain(E)  fifst01l
168500070919     c                   if        %error
168600070920     c* reperisco il RRN del file
168700070920     c  n17kftt1old      chain(n)  fiftt001
168800070920     c     kftt1old      chain(n)  fifst001
168900070920     C* Valorizzo la DS di procedura
169000070920     C                   CLEAR                   TRUL82DS
169100070920     c  n17              eval      ul82§rrn = fttnrr
169200070920     c   17              eval      ul82§rrn = fstnrr
169300070920     C  n17              EVAL      UL82§FIL = 'FIFTT00F'
169400070920     C   17              EVAL      UL82§FIL = 'FIFST00F'
169500070920     C                   EVAL      UL82§num = 1
169600070920     C                   EVAL      UL82§att = 2
169700070920     C* Effettuo la chiamata al *PGM d utilità
169800070920     C                   CALL(e)   'TRUL82R'
169900070920     C                   PARM                    TRUL82DS
170000070919     c  n17kftt1old      chain     fiftt01l
170100070919     c   17kftt1old      chain     fifst01l
170200070919     c                   endif
170300070914     c                   if        *in30='0'  and fttfvl<>'C'
170400070914     c                   select
170500070919     c*
170600070914     c                   when      fttset-1=0 and ftttbs=0
170700070914     c  n17              delete    fiftt001
170800070914     c   17              delete    fifst001
170900070919     c                   when      fttset-1>0 or fttset-1=0
171000070914     c                   eval      fttset=fttset-1
171100070914     c  n17              update    fiftt001
171200070914     c   17              update    fifst001
171300070914     c* se conteggio valorizzato svalorizzo
171400070914    3C     fttfvl        ifeq      'V'
171500070914     C                   MOVEL     FTtPDR        PA3PDR                         *COD. PADRONCINO
171600070914     C                   MOVEL     FTtTSR        PA3TSR                         *TIPO PRESTAZIONE
171700070914     C                   Z-ADD     FTtNDC        PA3NDC                         *NUMERO DISTINTA
171800070914     C                   Z-ADD     FTtDDC        PA3DDC                         *DATA   DISTINTA
171900070914     C                   MOVEL     'N'           PA3FCT                         *FLG COD.TARIFFA
172000070914     C                   MOVEL     'N'           PA3FVD                         *FLG VAL.MANUAL.DETT
172100070914     C                   MOVEL     'N'           PA3FVT                         *FLG VAL.MANUAL.TEST
172200070914     C  n17              MOVEL     ' '           PA3sml                         *FLG VAL.MANUAL.TEST
172300070914     C   17              MOVEL     'S'           PA3sml                         *FLG VAL.MANUAL.TEST
172400070914     C                   MOVEL     PARAM3        KPJBU
172500070914     C  N17              CALL      'FICNE2R'
172600070914     C                   PARM                    KPJBA
172700070914     C   17              CALL      'FICNE9C'
172800070914     C                   PARM                    KPJBA
172900070914    3C                   endif
173000070914     c                   endsl
173100070914     c                   endif
173200070912     c                   endsr
173300070918      *------------------------------------------------------------------------*
173400070918      * - Aggiornamento bolle arrivi per rialcolo affidabilità
173500070918      *------------------------------------------------------------------------*
173600070918     c     Sr_RicAff     BegSr
173700070918     c     kblp          chain(N)  fnarb01l
173800070918     c                   if        arbdcm>0
173900070918     c     kblp          chain(E)  fnarb01l
174000070918     c                   if        %error
174100070920     C* Valorizzo la DS di procedura
174200070920     C                   CLEAR                   TRUL82DS
174300070920     c                   eval      ul82§rrn = arbnrr
174400070920     C                   EVAL      UL82§FIL = 'FNARB00F'
174500070920     C                   EVAL      UL82§num = 1
174600070920     C                   EVAL      UL82§att = 2
174700070920     C* Effettuo la chiamata al *PGM d utilità
174800070920     C                   CALL(e)   'TRUL82R'
174900070920     C                   PARM                    TRUL82DS
175000070919     c     kblp          chain     fnarb01l
175100070918     c                   endif
175200070918     c                   eval      arbfbs='S'
175300070918     c                   if        arbdce>0
175400070918     c                   clear                   arbdti
175500070918     c                   clear                   arbhti
175600070918     c                   endif
175700070918     c                   update    fnarb000
175800070918     c                   endif
175900070919     c                   endsr
176000040326      *------------------------------------------------------------------------*
176100040326      * - SCRIVO LA FASE DELL'ORM
176200040326      *------------------------------------------------------------------------*
176300040326     c     Sr_WrtOrf     BegSr
176400040326
176500040326     c                   Clear                   Fnorf000
176600040326     c                   Eval      OrfPoe = OrmPoe
176700040326     c                   Eval      OrfNsr = OrmNsr
176800040326     c                   Eval      OrfNor = OrmNor
176900040326     c                   Eval      OrfNrv = OrmNrv
177000040326     c                   Eval      OrfPog = OrmPor
177100040326     c                   Eval      OrfDae = OrmDfo
177200040326     c                   Eval      OrfOre = OrmOfo
177300040326     c                   Eval      OrfFar = OrmFao
177400040326     c                   Eval      OrfPue = knmus
177500040908     c                   If        OrmNdc > *Zeros
177600040908     c                   Eval      OrfFgs = OrmPor
177700040908     c                   Eval      OrfNdc = OrmNdc
177800040908     c                   Eval      OrfDdc = OrmDdc
177900040908     c                   EndIf
178000040326     c                   Write     Fnorf000
178100040326
178200040326     c                   EndSr
178300040615
178400040615      *------------------------------------------------------------------------*
178500040615      * - SCELTA STAMPANTE PER MODIFICA BOLLA
178600040615      *------------------------------------------------------------------------*
178700040615     c     Sr_Scestp     BegSr
178800040615
178900040615     c                   Clear                   kpjbu
179000040615     c                   Eval      *In92 = *Off
179100040615
179200040615      * Richiesta stampanti
179300040615     c                   If        conta = *zeros
179400040615     c                   Clear                   Trul90ds
179500040615     c                   movel     'S'           D90rse
179600040615     c                   movel     'S'           D90rsb
179700040615     c                   call      'TRUL90R'
179800040615     c                   parm                    kpjba
179900040615     c                   parm                    trul90ds
180000040615      * F3=Fine
180100040615     c                   if        D90f3 = '1'
180200040615     c                   eval      *in92 = *on
180300040615     c                   goto      endstp
180400040615     c                   endif
180500040615
180600040615     c                   Eval      lenght = 103
180700040615
180800040615      * Ovrprtf Segnacolli
180900040615     c                   movea     D90pre        cm3(28)
181000040615     c                   movea     D90mde        cm3(50)
181100040615     c                   clear                   comman
181200040615     c                   movea     cm3           comman
181300040615     c                   CALL      'QCMDEXC'                            92
181400040615     c                   PARM                    comman
181500040615     c                   PARM                    lenght
181600040615     c   92              goto      endstp
181700040615
181800040615      * Bolle - Nuovo modulo
181900040615     c                   Eval      lenght = 110
182000040615
182100040615      * Ovrprtf Bolle A4
182200040615     c                   Movea     D90prb4       cma4(30)
182300040615     c                   Movea     D90mdb4       cma4(52)
182400040615     c                   Movea     wdatibolA4    cma4(96)
182500040615     c                   Clear                   Comman
182600040615     c                   Movea     cmA4          Comman
182700040615     c                   Call      'QCMDEXC'                            92
182800040615     c                   Parm                    Comman
182900040615     c                   Parm                    Lenght
183000040615     c   92              goto      endstp
183100040615
183200040615      * Ovrprtf Bolle A5
183300040615     c                   Movea     D90prb5       cmA5(30)
183400040615     c                   Movea     D90mdb5       cmA5(52)
183500040615     c                   Movea     wdatibolA5    cma5(96)
183600040615     c                   Clear                   Comman
183700040615     c                   Movea     cmA5          Comman
183800040615     c                   Call      'QCMDEXC'                            92
183900040615     c                   Parm                    Comman
184000040615     c                   Parm                    Lenght
184100040615     c   92              goto      endstp
184200040615
184300040615     c                   eval      conta = conta +1
184400040615     c                   endif
184500040615
184600040615     c     endstp        EndSr
184700070925      *------------------------------------------------------------------------*
184800070925      * - Reperimento data ritiro in base alla fase orm
184900070925      *------------------------------------------------------------------------*
185000070925     c     Sr_Drt        BegSr
185100070925
185200070925      * data ritiro effettiva
185300070925     C                   clear                   tibs02ds
185400070925     C                   clear                   dfar
185500070925     C                   eval      t02mod='C'
185600070925     C                   eval      t02sif=knsif
185700070925     C                   eval      t02cod='FAR'
185800070925     C                   movel(p)  ormfao        t02ke1
185900070925     C                   call      'TIBS02R'
186000070925     C                   PARM                    kpjba
186100070925     C                   parm                    tibs02ds
186200070925    3C                   if        t02err=*blanks
186300070925     C                   eval      dfar=t02uni
186400070925    3C                   endif
186500070925     c                   clear                   w_dar
186600070925     c* se fase utilizzabile --> data della fase
186700070925    3C     d§fardar      ifeq      'S'
186800070925     C                   Z-ADD     ORMDFO        w_dar
186900070925   x3c                   else
187000070925     c* se l'orm non è in una fase utilizzabile per la data ritiro prendo
187100070925     c* la data ritiro presente sull'orm
187200070925     C                   Z-ADD     ORMDAR        w_dar
187300070925    3c                   endif
187400070925     c                   endsr
187500040318
187501151104     C*--- Memorizzazione flag per addebito ORM telefonico -----------*
187502151104     C     sr_b4ado      BEGSR
187511151104     c* di default imposto flag = 'S'
187513151105     c                   eval      Wb4ado='S'
187514151104     c* Pulisco il flag se:
187515151104     c* già presente altra bolla abbinata allo stesso ORM che prevede
187516151104     c* l'addebito
187517151104     c* Teoricamente se presente altra bolla non dovrebbe essere necessario
187518151104     c* testare anche il flag. Ma in manutenzione bolle,se viene modificato
187519151104     c*l'ORM, il flag non viene riaggiornato relativamente al vecchio ORM
187520151104     c* e quindi con questo test potrei recuperare eventuali "buchi" presenti
187521151104     c* e che si sono generati con precedenti manutenzioni
187522151105     c* NON LO FACCIO PERCHE' FIAR404L non ha il CAMPO AR4NOT!!!!
187523151104     c                   movel     'M'           kar4trc
187526151105     c                   Movel     dskorm        kar4n14
187527151105     c     kfiar4        setll     fiar404l
187528151105     c     kfiar4        reade     fiar404l
187529151105     c                   dow       not %eof(fiar404l) and wb4ado='S'
187530151104     c*
187531151105     c****               eval      dsbl4m=ar4not
187532151105     c****               if        §b4ado='S'
187533151105     c                   clear                   wb4ado
187534151105     c****               endif
187535151105     c     kfiar4        reade     fiar404l
187536151104    4c                   enddo
187541151104     c                   endsr
187600040316      *------------------------------------------------------------------------*
187700040316      * - ROUTINE INIZIALE
187800040316      *------------------------------------------------------------------------*
187900040316     c     *Inzsr        BegSr
188000040316
188100040316     c     *Entry        Plist
188200040316     c                   Parm                    Kpjba
188300040326
188400040316     c     *dtaara       define    §azute        azuteds
188500040316     c     *dtaara       define    §datiute      ddatiute
188600040316     c                   in(E)     *dtaara
188700040316     c                   If        %error  or RSUT = *blanks
188800040316     c                   Clear                   Tibs34ds
188900040316     c                   Call      'TIBS34R'
189000040316     c                   Parm                    Tibs34ds
189100040316     c                   In        *dtaara
189200040316     c                   EndIf
189300040316
189400040316     c                   Clear                   wabi
189500040316     c                   Clear                   dLat
189600130619     c                   eval      dUTE01 = UTEfaf
189700040316
189800040316      * Verifica errori e autorità profilo
189900040316s   1c                   Select
190000040317      * se ho errori nei dati utente esco dal pgm
190100040316w   1c                   When      DutErr = 'E'
190200040316     c                   Eval      $errute = *On
190300040317     c                   GoTo      EndInz
190400040317      * se non sono in sede (filiale remota)
190500040317      * --> se 1° livello, abilitazioni al terminal
190600040317      *     se 2° livello, abilitazioni al punto operativo
190700040316w   1c                   When      UteAut = *Blanks
190800040317if  2c                   If        DutLpo = '1'                                 *1° LIVELLO
190900040317     c                   Eval      wabi   = 'TP'
191000040317e   2c                   EndIf
191100040317if  2c                   If        DutLpo = '2'                                 *2° LIVELLO
191200040317     c                   Eval      wabi   = 'PO'
191300040317e   2c                   EndIf
191400040317      * sono in sede carica le abilitazioni del profilo
191500040316x   1c                   Other
191600130619      * carica abilitazioni del profilo
191700130619     c                   IF        §UTEorm <> *blanks
191800130619     c                   eval      wabi = §UTEorm
191900130619     c                   else
192000040316     c                   Eval      wabi = UteAut
192100130619     c                   endif
192200040317e   1c                   EndSl
192300130619
192400040317      * controllo se ok l'abilitazione dell'utente
192500040316     c                   Clear                   Tibs02ds
192600040316     c                   Eval      T02Mod = 'C'
192700040316     c                   Eval      T02Sif = knsif
192800040316     c                   Eval      T02Cod = 'LAT'
192900040316     c                   Movel(p)  wabi          T02Ke1
193000040316     c                   Call      'TIBS02R'
193100040316     c                   Parm                    kpjba
193200040316     c                   Parm                    Tibs02ds
193300040317if  1c                   If        T02Err = *Blanks
193400040316     c                   Eval      dLat = T02Uni
193500040317e   1c                   EndIf
193600040317if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
193700040316     c                   Eval      $errute = *On
193800040317e   1c                   EndIf
193900040317      * per errore esco dal pgm
194000040316if  1c                   If        $ErrUte = *On
194100040316     c                   GoTo      EndInz
194200040316e   1c                   EndIf
194300130619
194400130619      * Reperimento delle filiali gestibili dall'utente
194500130619     c                   clear                   TRUL31DS
194600130619     c                   eval      I31abi = wabi
194700130619     c                   eval      I31cdi = DUTdis
194800130619     c                   eval      I31car = DUTare
194900130619     c                   eval      I31cpo = DUTpou
195000130619     c                   call      'TRUL31R'
195100130619     c                   parm                    KPJBA
195200130619     c                   parm                    TRUL31DS
195300130619     c                   IF        O31pog > *zeros
195400130619     c                   movea     O31pog        SKpog
195500130619     c                   ELSE
195600130619     c                   Eval      $errute = *On
195700130619     c                   GoTo      EndInz
195800130619     c                   ENDIF
195900040316
196000040316     c                   Eval      VtcTit = Titolo
196100040318
196200040316      * Imposto il p.o. di gestione
196300130619if  1c******             If        DutLpo = '2' or DutLpo = *Blanks
196400040316     c                   Movel     DutPou        V1cFgs
196500130619     c******             Eval      *In06 = *On
196600130619     c******             Else
196700130619     c******             Movel     Simfel        V1cFgs
196800130619e   1c******             EndIf
196900040317
197000040317      * Carico L1 di simfel
197100130619     c*****              Clear                   Trul06ds
197200130619     c*****              Eval      D06Cod = '£1'
197300130619     c*****              Movel     Simfel        D06Key
197400130619     c*****              Movel(p)  Trul06ds      kpjbu
197500130619     c*****              Call      'TRUL06R'
197600130619     c*****              Parm                    kpjba
197700130619     c*****              Movel     kpjbu         Trul06ds
197800130619     c*****              Movea     lin           l1
197900040316
198000040316     c                   ExSr      Sr_ContrFgs
198100040326
198200050112      * Se ambiente prova
198300050112     c                   If        %subst(knsif:7:1) = 'P'
198400050112     c                   Eval      wlib = 'GAITRAGRPS'
198500050112     c                   Else
198600050112      * Se ambiente buono FILTRA201
198700040326     c                   Eval      wlib = 'GAITRAGRU '
198800040512     c                   EndIf
198900070907      *
199000070907      * Aggancio tabella "VPODECO" per reperimento data attivazione
199100070907      * data ritiro da orm
199200070907     C                   CLEAR                   tibs02ds
199300070907     c                   clear                   dvpodeco
199400070907     c                   Eval      T02Mod = 'C'
199500070907     c                   Eval      T02Sif = knsif
199600070907     c                   Eval      T02Cod = 'VPO'
199700070907     c                   movel(P)  'DECO'        t02ke1
199800070907     C                   CALL      'TIBS02R'
199900070907     C                   PARM                    KPJBA
200000070907     C                   PARM                    tibs02ds
200100070907    2C                   IF        T02ERR = *BLANKS
200200070907     C                   eval      dvpodeco  = t02uni
200300070907    2C                   ENDIF
200400070907     c*
200500070907     c                   time                    w0140            14 0
200600070907     C* Data del giorno in AAAAMMGG
200700070907     C                   move      w0140         G02DAT
200800070907     C                   movel     *blank        G02ERR
200900070907     C                   call      'XSRDA8'
201000070907     C                   Parm                    wlbdat
201100070907     C                   MOVEL     g02inv        dateu             8 0
201200070907
201300051114      * oggi - 10 gg.calendario
201400071107     c***                if        §vpodrt>dateu
201500071107     c***  *iso          Movel     *date         wdataiso
201600071107     c***                Subdur    10:*d         wdataiso
201700071107     c***                Move      wdataiso      wdata
201800071107     c***
201900071107     c***                else
202000070907     c* oggi - 1  giorno lavorativo
202100070919     c                   clear                   TRUL52ds
202200070919     c                   eval      d52gio=1
202300070919     c                   eval      d52sgn='-'
202400070907     c                   z-add     *date         D52din
202500070907     c                   call      'TRUL52R '
202600070907     c                   parm                    TRUL52ds
202700070907     c                   z-add     D52dfi        wdata
202800071107     c****               endif
202900040318
203000040318     c     kTab          klist
203100040318     c                   kfld                    codut
203200040318     c                   kfld                    kcod
203300040325     c                   kfld                    kkey
203400040324
203500040324     c     kOrm          klist
203600040324     c                   kfld                    Vs1Poe
203700040324     c                   kfld                    Vs1Nsr
203800040324     c                   kfld                    Vs1Nor
203900040324     c                   kfld                    Vs1Nrv
204000040324
204100040324     c     kApd          klist
204200040324     c                   kfld                    ApdTip
204300040324     c                   kfld                    ApdPdr
204400040318
204500040318     c     kBlp          klist
204600040318     c                   kfld                    V2cAas
204700040318     c                   kfld                    V2cLnp
204800040318     c                   kfld                    V2cNrs
204900040318     c                   kfld                    V2cNsp
205000040318
205100060214     c     kar4          klist
205200040413     c                   kfld                    V2cAas
205300040413     c                   kfld                    V2cLnp
205400040413     c                   kfld                    V2cNrs
205500040413     c                   kfld                    V2cNsp
205600040318     c                   kfld                    wtrc
205700040325
205800040325     c     kAr6          klist
205900040325     c                   kfld                    V2cAas
206000040325     c                   kfld                    V2cLnp
206100040325     c                   kfld                    V2cNrs
206200040325     c                   kfld                    V2cNsp
206300040325     c                   kfld                    Ar6Trc
206400040615
206500060214     C     kfiar4        klist
206600060214     C                   kfld                    Kar4trc
206700060214     C                   kfld                    kar4n14
206800070910     c
206900070910     C     KFTT          KLIST
207000070910     C                   KFLD                    blppdr
207100070910     C                   KFLD                    W01FVL
207200070910     C                   KFLD                    W_dar
207300070910     C                   KFLD                    W01TSR
207400070919     C     KFTTold       KLIST
207500070919     C                   KFLD                    blppdr
207600070919     C                   KFLD                    W01FVL
207700070919     C                   KFLD                    blpdrt
207800070919     C                   KFLD                    W01TSR
207900070910     C     kftt1         KLIST
208000070910     C                   KFLD                    blppdr
208100070910     C                   KFLD                    w01tsr
208200070910     C                   KFLD                    W_ndc
208300070910     C                   KFLD                    w_dar
208400070913     C     kftt1old      KLIST
208500070913     C                   KFLD                    blppdr
208600070913     C                   KFLD                    w01tsr
208700070913     C                   KFLD                    W_ndc
208800070913     C                   KFLD                    w_olddrt
208900070919     c     kftd5         klist
209000070919     C                   KFLD                    fttpdr
209100070919     C                   KFLD                    ftttsr
209200070919     C                   KFLD                    fttndc
209300070919     C                   KFLD                    w_olddrt
209400070919     c                   kfld                    blpaas
209500070919     c                   kfld                    blplnp
209600070919     c                   kfld                    blpnrs
209700070919     c                   kfld                    blpnsp
209800110322     C     KORG          KLIST
209900110322     C                   KFLD                    §b4poe
210000110322     C                   KFLD                    §b4nsr
210100110322     C                   KFLD                    §b4nor
210200110322     C                   KFLD                    §b4nrv
210300120612     C     KDST          KLIST
210400110322     C                   KFLD                    knpg
210500110322     C                   KFLD                    knfv
210600110322     C                   KFLD                    kfgs
210700110322     C     Kar5          KLIST
210800110322     C                   KFLD                    BLPAAS
210900110322     C                   KFLD                    BLPLNP
211000110322     C                   KFLD                    BLPNRS
211100110322     C                   KFLD                    BLPNSP
211200110322     C                   KFLD                    KTRD
211300040326
211400040316     c     EndInz        EndSr
211500040326      *------------------------------------------------------------------------*
211600070912     OFNBLP000  E            AGGBLP
211700070912     O                       BLPDRT
211800070912     OTITAS000  E            AGTAS0
211900070912     O                       TASDRT
211901151105     O                       TASFLO
212000070912     OTITAS010  E            AGTAS1
212100070912     O                       TASDRT
212101151105     O                       TASFLO
212200040615** CM3
212300040615OVRPRTF FILE(FNLV22P) OUTQ(XXXXXXXXXX) FORMTYPE('ETICH     ')
212400040615 SHARE(*YES)
212500040615** cmA4
212600040615OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
212700040615 SHARE(*YES)          USRDTA('xxxxxxxxx')
212800040615**  cmA5
212900040615OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
213000040615 SHARE(*YES)          USRDTA('xxxxxxxxx')
213100040326** cmd
213200040330OVRDBF FILE(TITA400F) TOFILE(
213300070919OVRDBF FILE(TITAS30C) TOFILE(
213400040326** msg                                                                       *
213500040316P.O. gestione errato                                                                01
213600040316Procedura ORM non attivata                                                          02
213700040316Ampiezza massima subfile superata                                                   03
213800040316Scelta non valida                                                                   04
213900040325Serie __ non gestita - AVVISARE IL CED                                              05
214000040317 ORM non abilitata per serie __                                                     06
214100040318Immettere la spedizione                                                             07
214200040318Spedizione non trovata                                                              08
214300040318Spedizione già abbinata ad un ORM                                                   09
214400040615                                                                                    10
214500040318Bolla di recupero! Non abbinare ad un ORM                                           11
214600040325ORM con serie! Il codice bolla della spedizione abbinata non è __                   12
214700040330Per ORM prepagato la spedizione deve una bolla Franco prepagato                     13
214800040330                                                                                    14
214900040325L'ORM richiede una spedizione in porto Assegnato                                    15
215000040325Abbinare una spedizione avente Linea di partenza uguale al P.O. ritiro              16
215100040325Abbinare una spedizione avente Linea di arrivo uguale al P.O. consegna              17
215200040325Abbinare una spedizione in porto Franco                                             18
215300040331ORM commissionato da p.o. ___ abbinare bolla export     in porto assegnato          19
215400040326Data ritiro ORM diversa da data ritiro merce della bolla. ENTER per forzare.        20
215500051114Spedizione immessa da oltre 10 giorni. Impossibile abbinare                         21
215600070907Data ritiro ORM diversa da data ritiro merce della bolla. ENTER per proseguire.     22
215700070907Spedizione immessa da oltre 1 giorno. Impossibile abbinare                          23
215800070919Impossibile abbinare: già confermati i conteggi per l'autotrasportatore             24
215900070911Impossibile abbinare: per l'autotrasp. non presenti ritiri nella data ritiro        25
216000070920Spedizione al momento in uso da un altro lavoro: impossibile abbinare               26
