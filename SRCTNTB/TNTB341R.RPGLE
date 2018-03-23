000100040408     h decedit('0,') datedit(*dmy.) option(*nodebugio)
000200040408
000300040408      *------------------------------------------------------------------------*
000400040408      * Gestione tabella "CMR" = Causali ORM
000500040408      *------------------------------------------------------------------------*
000600040408
000700040408     fTntbe01l  uf a e           k disk
000800060728     ftntbe11l  uf a e           k disk    rename(tntbe000:tntbe11)
000900060728     fazlin02l  if   e           k disk
001000060728     fTntb341d  cf   e             workstn sfile(tb341s01:nrr)
001100030805
001200040408      *------------------------------------------------------------------------*
001300040408      *   S C H I E R E
001400040408      *------------------------------------------------------------------------*
001500040408     d $Opz            s             15    dim(06) ctdata perrcd(1)             Decodifica OPZ
001600040408     d $Msg            s             78    dim(07) ctdata perrcd(1)             Messaggi video
001700040408     d $Fld            s             10    dim(10)                              Campi per ricerca
001800040408
001900040408      *------------------------------------------------------------------------*
002000040408      *   V A R I A B I L I
002100040408      *------------------------------------------------------------------------*
002200060728     d nrr             s              4  0
002300060728     d savrec          s                   like(nrr)
002400060728     d savtbeuni       s                   like(tbeuni)
002500040408     d $CarV1          s              1a   inz(*on)
002600040408     d $CarW1          s              1a   inz(*off)
002700040408     d $Err            s              1a   inz(*off)
002800040408     d $Fine           s              1a   inz(*off)
002900040408     d $TipVid         s              1a   inz('1')
003000040408     d Win             s             99a   inz(*zeros)
003100040408     d wTasto          s              2a   inz(*zeros)
003200040408
003300040408      *------------------------------------------------------------------------*
003400040408      *   D S   I N T E R N E / E S T E R N E
003500040408      *------------------------------------------------------------------------*
003600040413     d wlbdat          ds                  inz
003700040413     d  g02dat                 1      8  0
003800040413     d  g02inv                 9     16  0
003900040413     d  g02err                17     17
004000040413     d  g02tgi                18     22  0
004100040413
004200060403     d Tibs34ds      e ds
004300040408     d Azuteds       e ds                  extname(Azute00f)
004400040408     d dDatiute      e ds
004500040408     d kpjba         e ds
004600060403     d dCmr          e ds
004700060403     d dFar          e ds
004800040408     d Tibs02ds      e ds
004900040414     d Tntb341ds     e ds
005000040408
005100040408      * Tracciato record file TNTBE00F
005200040408     d Tntbeds       e ds                  extname(Tntbe00f) inz
005300040408     d xTntbeds      e ds                  extname(Tntbe00f) inz
005400040408     d                                     prefix(TBX:3)
005500040408
005600040408     d                sds
005700040408     d  VTCpgm           *proc
005800040408
005900040408      *------------------------------------------------------------------------*
006000040408      *   C O S T A N T I
006100040408      *------------------------------------------------------------------------*
006200040408     d DigitN          c                   const('0123456789')
006300040408
006400040408      *------------------------------------------------------------------------*
006500040408      *  RIEPILOGO INDICATORI
006600040408      *------------------------------------------------------------------------*
006700040408      *  01 - Record inesistente (inserimento)
006800040408      *  02 - Record esistente   (modifica)
006900040408      *  04 - Record annullato   (ripristino)
007000060728      *  10 - visualizzo subfile lingue
007100060728      *  20 - gestione subfile
007200060728      *  21 - gestione subfile
007300040408      *  22 - Errori in scrittura record (WRITE)
007400060728      *  23 - gestione subfile
007500040408      *------------------------------------------------------------------------*
007600040408
007700040408      * Gestione video
007800040408     c                   Dow       $Fine = *Off
007900040408     c     $TipVid       Caseq     '1'           GesV1
008000040408     c     $TipVid       Caseq     'A'           GesW1
008100060728     c     $TipVid       Caseq     'L'           sr_lingua
008200040408     c                   EndCs
008300040408     c                   EndDo
008400040408
008500040408      * Fine
008600040408     c                   Eval      *InLr = *On
008700040408
008800040408      *------------------------------------------------------------------------*
008900040408      * - GESV1  - Gestione videata selezione codice tabella
009000040408      *------------------------------------------------------------------------*
009100040408     c     GesV1         BegSr
009200060728
009300060728     c                   eval      *in10 = *off
009400040408
009500040408      * Inizializzazione videata
009600040408     c                   If        $CarV1 = *On
009700040408     c                   ExSr      CarV1
009800040408     c                   Move      *Off          $CarV1
009900040408     c                   EndIf
010000040408      * Scrivo la testata
010100040408     c                   Write     Tb341t01
010200040408      *
010300040408      * Se esistono errori sulla videata
010400040408      * emetto la write del formato a indicatori spenti per vedere
010500040408      * le eventuali decodifiche
010600040413     c                   If        *In99
010700040413     c                   Movea     *In           Win
010800040413     c                   Movea     *Zeros        *In(50)
010900040413     c                   Write     Tb341v01
011000040413     c                   Movea     Win           *In
011100040413     c                   EndIf
011200040413
011300040413if  1c                   If        *In05
011400040413     c                   Write     Tb341v01
011500040413     c                   Exfmt     Protect
011600040413x   1c                   Else
011700040413     c                   Exfmt     Tb341v01
011800040413e   1c                   EndIf
011900040413     c                   Eval      *In99 = *Off
012000040413     c                   Clear                   V1dMsg
012100040413     c                   Clear                   wTasto
012200040413
012300040413sel 1c                   Select
012400040408      * F03=Fine
012500040413w   1c                   When      *InKc
012600040413     c                   ExSr      F03v1
012700040413     c                   GoTo      EndGesV1
012800040408      * F12=Ritorno
012900040413w   1c                   When      *InKl
013000040413     c                   ExSr      F12v1
013100040413     c                   GoTo      EndGesV1
013200060728      * F9=Altri dati
013300060728     c                   when      *inki
013400060728     c                   eval      *in10 = *on
013500060728     c                   Eval      $TipVid = 'L'
013600060728     c                   goto      endgesv1
013700040413e   1c                   EndSl
013800040413
013900040408      * Controllo dati immessi a video
014000040408      * (non si fanno se richisto l'annullamento)
014100040413if  1c                   If        Not *InKq
014200040413     c                   ExSr      CtrV1
014300040413e   1c                   EndIf
014400040413
014500040408      * Aggiornamento se non ci sono errori
014600040413if  1c                   If        Not *In99
014700040413     c                             and (*InKf or *InKe or *InKq)
014800040413sel 2c                   Select
014900040413w   2c                   When      *InKe
015000040413     c                   Eval      wTasto = '05'
015100040413w   2c                   When      *InKf
015200040413     c                   Eval      wTasto = '06'
015300040413w   2c                   When      *InKq
015400040413     c                   Eval      wTasto = '16'
015500040413e   2c                   EndSl
015600040413     c                   Eval      $CarW1 = *on
015700040413     c                   Eval      $TipVid = 'A'
015800040413e   1c                   EndIf
015900040413
016000030805     c     EndGesV1      ENDSR
016100040413
016200040413      *------------------------------------------------------------------------*
016300040413      * - CARV1  - Caricamento dati prima videata
016400040413      *------------------------------------------------------------------------*
016500040413     c     CarV1         BegSr
016600040413
016700040413     c                   Clear                   T1opz
016800040413     c                   Movea     '00000'       *In(01)
016900040414
017000040414      * IMMISSIONE
017100040414if  1c                   If        B341Opz = '1'
017200040414     c                   Clear                   dCmr
017300040414     c                   Eval      *In01  = *On
017400040414     c                   Eval      T1opz  = $Opz(01)
017500040414
017600040414x   1c                   Else
017700040413
017800040413      * Aggancio la tabella, se trovo il codice sono in modifica
017900040413      * o ripristino (se record annullato), altrimenti in immissione
018000040413     c                   ExSr      ChnTbe
018100040413
018200040414if  2c                   If        %Found(TNTBE01L)
018300040414     c                   Movel     TbeUni        dCmr
018400040414
018500040413      * MODIFICA/RIPRISTINO
018600040414if  3c                   If        B341Opz = '2'
018700040414if  4c                   If        TbeAtb = *Blanks
018800040413     c                   Eval      *In02  = *On
018900040413     c                   Eval      T1opz  = $Opz(02)
019000040414x   4c                   Else
019100040414     c                   Eval      *In04  = *On
019200040414     c                   Eval      T1opz  = $Opz(06)
019300040414e   4c                   EndIf
019400040414e   3c                   EndIf
019500040414
019600040414      * VISUALIZZAZIONE
019700040414if  3c                   If        B341Opz = '5'
019800040414     c                   Eval      *In05  = *On
019900040414if  4c                   If        TbeAtb = *Blanks
020000040414     c                   Eval      T1opz  = $Opz(05)
020100040414x   4c                   Else
020200040414     c                   Eval      T1opz  = $Opz(06)
020300040414e   4c                   EndIf
020400040414e   3c                   EndIf
020500040413
020600040414e   2c                   EndIf
020700040413e   1c                   EndIf
020800040413
020900040414     c                   Eval      V1cCau = B341Ke1
021000040413     c                   Eval      V1cDes = d§CmrDes
021100040413     c                   Eval      V1cDes1 = d§CmrDes1
021200040413     c                   Move      d§CmrFar      V1cFar
021300040413     c                   Eval      V1cNoc = d§CmrNoc
021400040413     c                   Eval      V1cNoq = d§CmrNoq
021500040413     c                   Eval      V1cNmF = d§CmrNmf
021600040413     c                   Eval      V1cNot = d§CmrNot
021700040414     c                   Eval      V1cAdd = d§CmrAdd
021800040413     c                   Eval      V1cDva = d§CmrDva
021900040413     c                   Eval      V1cCva = d§CmrCva
022000040413     c                   Eval      V1cCcc = d§CmrCcc
022100040413     c                   Eval      V1cNca = d§CmrNca
022200040413     c                   Eval      V1cCca = d§CmrCca
022300041206     c                   Eval      V1cRea = d§CmrRea
022400061218     c                   eval      v1cpda = d§cmrpda
022500061218     c                   eval      v1cinfo = d§cmrinfo
022600061218     c                   eval      v1cfirma = d§cmrfirma
022700040413      * Descrizione Fase
022800040413     c                   Clear                   dFar
022900040413     c                   Clear                   V1dFar
023000040413if  1c                   If        V1cFar <> *Zeros
023100040413     c                   Clear                   Tibs02ds
023200040413     c                   Eval      T02Mod = 'C'
023300040413     c                   Eval      T02Sif = knsif
023400040413     c                   Eval      T02Cod = 'FAR'
023500040413     c                   Movel(p)  V1cFar        T02Ke1
023600040413     c                   Call      'TIBS02R'
023700040413     c                   Parm                    kpjba
023800040413     c                   Parm                    Tibs02ds
023900040413if  2c                   If        T02Err = *Blanks
024000040413     c                   Eval      dFar = T02Uni
024100040413e   2c                   EndIf
024200040413     c                   Eval      V1dFar = d§FarDes
024300040413e   1c                   EndIf
024400040413      * Causale alternativa Vas
024500040413     c                   Clear                   V1dCva
024600040413if  1c                   If        V1cCva <> *Blanks
024700040413     c                   Clear                   Tibs02ds
024800040413     c                   Eval      T02Mod = 'C'
024900040413     c                   Eval      T02Sif = knsif
025000040413     c                   Eval      T02Cod = 'CMR'
025100040413     c                   Movel(p)  V1cCva        T02Ke1
025200040413     c                   Call      'TIBS02R'
025300040413     c                   Parm                    kpjba
025400040413     c                   Parm                    Tibs02ds
025500040413     c                   Eval      V1dCva = T02Uni
025600040413e   1c                   EndIf
025700040413      * Causale x Chiusura ORM automatica
025800040413     c                   Clear                   dCmr
025900040413     c                   Clear                   V1dCca
026000040413if  1c                   If        V1cCca <> *Blanks
026100040413     c                   Clear                   Tibs02ds
026200040413     c                   Eval      T02Mod = 'C'
026300040413     c                   Eval      T02Sif = knsif
026400040413     c                   Eval      T02Cod = 'CMR'
026500040413     c                   Movel(p)  V1cCca        T02Ke1
026600040413     c                   Call      'TIBS02R'
026700040413     c                   Parm                    kpjba
026800040413     c                   Parm                    Tibs02ds
026900040413     c                   Eval      V1dCca = T02Uni
027000040413e   1c                   EndIf
027100040413
027200040413     c                   EndSr
027300040413
027400040413      *------------------------------------------------------------------------*
027500040413      * - F03V1  - Tasto funzionale F03 -> Fine programma
027600040413      *------------------------------------------------------------------------*
027700040413     c     F03V1         BegSr
027800040413
027900040414     c                   Movel     *On           $Fine
028000040414     c                   Movel     'F03'         B341Fxx
028100040413
028200040413     c                   EndSr
028300040413
028400040413      *------------------------------------------------------------------------*
028500040413      * - F12V1  - Tasto funzionale F12 -> Ritorno
028600040413      *------------------------------------------------------------------------*
028700040413     c     F12V1         BegSr
028800040413
028900040414     c                   Movel     *On           $Fine
029000040414     c                   Movel     'F12'         B341Fxx
029100040413
029200040413     c                   EndSr
029300040413
029400040413      *------------------------------------------------------------------------*
029500040413      * - CTRV1  - Controllo prima videata
029600040413      *------------------------------------------------------------------------*
029700040413     c     CtrV1         BegSr
029800040413
029900040413     c                   Movea     *Zeros        *In(50)
030000040413     c                   Clear                   V1dMsg
030100040414
030200040414      * - Causale obbligatoria
030300040414if  1c                   If        *In01 and V1cCau = *Blanks
030400040414     c                   Eval      *In55 = *On
030500040414     c                   Eval      *In99 = *On
030600040414     c                   Movel     $Msg(06)      V1dMsg
030700040414     c                   GoTo      EndCtrV1
030800040414e   1c                   EndIf
030900040420      * - Non deve esistere già
031000040420if  1c                   If        *In01
031100040420     c                   Clear                   Tibs02ds
031200040420     c                   Eval      T02Mod = 'C'
031300040420     c                   Eval      T02Sif = knsif
031400040420     c                   Eval      T02Cod = 'CMR'
031500040420     c                   Movel(p)  V1cCau        T02Ke1
031600040420     c                   Call      'TIBS02R'
031700040420     c                   Parm                    kpjba
031800040420     c                   Parm                    Tibs02ds
031900040420if  2c                   If        T02Err = *Blanks
032000040420     c                   Eval      *In55 = *On
032100040420     c                   Eval      *In99 = *On
032200040420     c                   Movel     $Msg(07)      V1dMsg
032300040420     c                   GoTo      EndCtrV1
032400040420e   2c                   EndIf
032500040420e   1c                   EndIf
032600040413
032700040413      * - Descrizione obbligatoria
032800040413if  1c                   If        V1cDes = *Blanks
032900040413     c                   Eval      *In50 = *On
033000040413     c                   Eval      *In99 = *On
033100040413     c                   Movel     $Msg(01)      V1dMsg
033200040413     c                   GoTo      EndCtrV1
033300040413e   1c                   EndIf
033400040413      * - Descrizione breve obbligatoria
033500040413if  1c                   If        V1cDes1 = *Blanks
033600040413     c                   Eval      *In51 = *On
033700040413     c                   Eval      *In99 = *On
033800040413     c                   Movel     $Msg(01)      V1dMsg
033900040413     c                   GoTo      EndCtrV1
034000040413e   1c                   EndIf
034100040413      * - Fase obbligatoria
034200040413if  1c                   If        V1cFar = *Zeros
034300040413     c                   Eval      *In52 = *On
034400040413     c                   Eval      *In99 = *On
034500040413     c                   Movel     $Msg(02)      V1dMsg
034600040413     c                   GoTo      EndCtrV1
034700040413e   1c                   EndIf
034800040413      *   e deve esistere in tabella FAR
034900040413     c                   Clear                   V1dFar
035000040413     c                   Clear                   dFar
035100040413     c                   Clear                   Tibs02ds
035200040413     c                   Eval      T02Mod = 'C'
035300040413     c                   Eval      T02Sif = knsif
035400040413     c                   Eval      T02Cod = 'FAR'
035500040413     c                   Movel(p)  V1cFar        T02Ke1
035600040413     c                   Call      'TIBS02R'
035700040413     c                   Parm                    kpjba
035800040413     c                   Parm                    Tibs02ds
035900040413if  1c                   If        T02Err <> *Blanks
036000040413     c                   Eval      *In52 = *On
036100040413     c                   Eval      *In99 = *On
036200040413     c                   Movel     $Msg(03)      V1dMsg
036300040413     c                   GoTo      EndCtrV1
036400040413e   1c                   EndIf
036500040413     c                   Eval      dFar = T02Uni
036600040413     c                   Eval      V1dFar = d§FarDes
036700040413      * - La Causale alternativa Vas deve esistere in tabella
036800040413     c                   Clear                   V1dCva
036900040413if  1c                   If        V1cCva <> *Blanks
037000040413     c                   Clear                   Tibs02ds
037100040413     c                   Eval      T02Mod = 'C'
037200040413     c                   Eval      T02Sif = knsif
037300040413     c                   Eval      T02Cod = 'CMR'
037400040413     c                   Movel(p)  V1cCva        T02Ke1
037500040413     c                   Call      'TIBS02R'
037600040413     c                   Parm                    kpjba
037700040413     c                   Parm                    Tibs02ds
037800040413if  2c                   If        T02Err <> *Blanks
037900040413     c                   Eval      *In53 = *On
038000040413     c                   Eval      *In99 = *On
038100040413     c                   Movel     $Msg(04)      V1dMsg
038200040413     c                   GoTo      EndCtrV1
038300040413e   2c                   EndIf
038400040413     c                   Eval      V1dCva = T02Uni
038500040413e   1c                   EndIf
038600040413      * - N.Max causali
038700040413if  1c                   If        V1cNca > *Zeros
038800040413      *   Causale Obbligatoria
038900040413if  2c                   If        V1cCca = *Blanks
039000040413     c                   Eval      *In54 = *On
039100040413     c                   Eval      *In99 = *On
039200040413     c                   Movel     $Msg(05)      V1dMsg
039300040413     c                   GoTo      EndCtrV1
039400040413e   2c                   EndIf
039500040413      *   e deve esistere in tabella
039600040413     c                   Clear                   V1dCca
039700040413     c                   Clear                   Tibs02ds
039800040413     c                   Eval      T02Mod = 'C'
039900040413     c                   Eval      T02Sif = knsif
040000040413     c                   Eval      T02Cod = 'CMR'
040100040413     c                   Movel(p)  V1cCca        T02Ke1
040200040413     c                   Call      'TIBS02R'
040300040413     c                   Parm                    kpjba
040400040413     c                   Parm                    Tibs02ds
040500040413if  2c                   If        T02Err <> *Blanks
040600040413     c                   Eval      *In54 = *On
040700040413     c                   Eval      *In99 = *On
040800040413     c                   Movel     $Msg(04)      V1dMsg
040900040413     c                   GoTo      EndCtrV1
041000040413e   2c                   EndIf
041100040413     c                   Eval      V1dCca = T02Uni
041200040413e   1c                   EndIf
041300040413
041400040413     c     EndCtrV1      EndSr
041500040413
041600040413      *------------------------------------------------------------------------*
041700040413      * - GESW1  - Gestione videata dati relativi alla trasmissione
041800040413      *------------------------------------------------------------------------*
041900040413     c     GesW1         BegSr
042000040413
042100040413      * Inizializzazione videata
042200040413if  1c                   If        $CarW1 = *On
042300040413     c                   Exsr      CarW1
042400040413     c                   Movel     *Off          $CarW1
042500040413e   1c                   EndIf
042600040413
042700040413if  1c                   If        *In05
042800040413     c                   Write     TB341W01
042900040413     c                   Exfmt     PROTECT
043000040413x   1c                   Else
043100040413     c                   Exfmt     TB341W01
043200040413e   1c                   EndIf
043300040413     c                   Eval      *In99 = *Off
043400040413     c                   Clear                   W1MSG
043500040413
043600040413sel 1c                   Select
043700040413      * F12=Ritorno
043800040413w   1c                   When      *InKl
043900040413     c                   ExSr      F12W1
044000040413     c                   GoTo      EndGesW1
044100040413e   1c                   EndSl
044200040413
044300040413      * Controllo dati immessi a video
044400040413     c                   ExSr      CtrW1
044500040413
044600040413      * Aggiornamento se non ci sono errori
044700040413if  1c                   If        Not *In99 and *InKf
044800040413     c                   ExSr      AggTbe
044900040413e   1c                   EndIf
045000040413
045100040413     c     EndGesW1      EndSr
045200040413
045300040413      *------------------------------------------------------------------------*
045400040413      * - CARW1  - Caricamento dati window
045500040413      *------------------------------------------------------------------------*
045600040413     c     CarW1         BegSr
045700040413
045800040413     c                   Movea     *zeros        *in(50)
045900040413
046000040413sel 1c                   Select
046100040413
046200040413      * F5=Ripristino
046300040413w   1c                   When      *InKe   and  *In04
046400040413     c                   Eval      W1FTT = TBEftt
046500040413
046600040413      * F6=Conferma
046700040413w   1c                   When      *InKf
046800040413sel 2c                   Select
046900040413      *   Immissione
047000040413w   2c                   When      *In01
047100040413     c                   Eval      W1FTT = TBXftt
047200040413      *   Modifica / Ripristino
047300040413w   2c                   When      *In02   or    *In04
047400040413     c                   Eval      W1FTT = TBEftt
047500040413e   2c                   EndSl
047600040413
047700040413      * F16=Annullamento
047800040413w   1c                   When      *InKq   and  Not *In04
047900040413     c                   Eval      W1FTT = TBEftt
048000040413
048100040413e   1c                   EndSl
048200040413
048300040413      * Se NON immissione: visualizzo i dati relativi all'ultima
048400040413      *   trasmissione
048500040413if  1c                   If        Not *In01
048600040413     c                   Eval      W1FLT = TBEflt
048700040413     c                   Eval      W1FTR = TBEftr
048800040413if  2c                   If        TBEdtr <> 0
048900040413     c                   Clear                   WLBDAT
049000040413     c                   Z-add     TBEdtr        G02inv
049100040413     c                   Movel     '3'           G02err
049200040413     c                   Call      'XSRDA8'
049300040413     c                   Parm                    WLBDAT
049400040413     c                   Z-add     G02dat        W1DTR
049500040413e   2c                   EndIf
049600040413e   1c                   EndIf
049700040413
049800040413     c                   EndSr
049900040413
050000040413      *------------------------------------------------------------------------*
050100040413      * - CTRW1  - Controllo e decodifica window
050200040413      *------------------------------------------------------------------------*
050300040413     c     CtrW1         BegSr
050400040413
050500040413     c                   Movea     *zeros        *In(50)
050600040413
050700040413     c     EndCtrW1      EndSr
050800040413
050900040413      *------------------------------------------------------------------------*
051000040413      * - F21W1  - Tasto funzionale F12 -> Ritorno
051100040413      *------------------------------------------------------------------------*
051200040413     c     F12W1         BegSr
051300040413
051400040413     c                   Eval      $TipVid = '1'
051500040413
051600040413     c                   EndSr
051700040413
051800040408      *------------------------------------------------------------------------*
051900040408      * - CHNTBE * Aggancio tabella
052000040408      *------------------------------------------------------------------------*
052100040408     c     ChnTbe        BegSr
052200040408
052300040408     c                   Movel     'CMR'         TbeCod
052400040414     c                   Movel(p)  B341Ke1       TbeKe1
052500040414     c                   Movel(p)  B341Ke2       TbeKe2
052600040414     c                   Eval      TbeLin = B341Lin
052700040414     c                   Eval      TbeSif = B341Sif
052800040408     c     kTbe          Chain     Tntbe01l
052900040408
053000040408     c                   EndSr
053100040413
053200040413      *------------------------------------------------------------------------*
053300040413      * - AGGTBE * Aggiornamento tabella
053400040413      *------------------------------------------------------------------------*
053500040413     c     AggTBE        BegSr
053600040413
053700040413sel 1c                   Select
053800040413
053900040413      * F5=Ripristino
054000040413w   1c                   WHEN      wTasto='05'  and  *In04
054100040413     c                   Clear                   TBEatb
054200040413     c                   Clear                   TBEftr
054300040413     c                   UPDATE    TNTBE000
054400040413
054500040413      * F6=Conferma
054600040413w   1c                   WHEN      wTasto='06'
054700040413     c                   ExSr      RieTBE
054800040413sel 2c                   Select
054900040413      *   Immissione
055000040413w   2c                   When      *in01
055100040413     c                   Clear                   TBEflt
055200040413     c                   Clear                   TBEdtr
055300040413     c                   WRITE     TNTBE000                             22
055400040413      *   Modifica / Ripristino
055500040413w   2c                   When      *In02   or    *In04
055600040413     c                   UPDATE    TNTBE000
055700040413e   2c                   EndSl
055800040413
055900040413      * F16=Annullamento
056000040413w   1c                   WHEN      wTasto='16'  and  Not *In04
056100040413     c                   Movel     'A'           TBEatb
056200040413     c                   Clear                   TBEftr
056300040413     c                   UPDATE    TNTBE000
056400040413
056500040413e   1c                   EndSl
056600060728
056700060728      * se annullo tabella devo annullare anche tutte le lingue, stessa cosa se ripristino
056800060728     c                   if        (wTasto='16' and Not *In04) or
056900060728     c                             (wTasto='05' and *In04)
057000060728     c                   clear                   lintntbe
057100060728     c     lintntbe      setgt     azlin02l
057200060728     c                   do        *hival
057300060728     c                   read      azlin02l
057400060728     c                   if        %eof(azlin02l)
057500060728     c                   leave
057600060728     c                   endif
057700060728     c                   eval      tbelin = lintntbe
057800060728     c     ktbe11        chain     tntbe11l
057900060728     c                   if        %found(tntbe11l)
058000060728     c                   select
058100060728      * ripristino
058200060728     c                   when      wTasto='05' and *In04
058300060728     c                   clear                   tbeatb
058400060728      * annullo
058500060728     c                   when      wTasto='16' and Not *In04
058600060728     c                   eval      tbeatb = 'A'
058700060728     c                   endsl
058800060728     c                   clear                   tbedtr
058900060728     c                   clear                   tbeftr
059000060728     c                   update    tntbe11
059100060728     c                   endif
059200060728     c                   enddo
059300060728     c                   leavesr
059400060728     c                   endif
059500060728
059600060728      *  emetto le descrizioni in lingua  se non è annullo/ripristino
059700060728     c                   if        *in01 or *in02
059800060728     c                   eval      *in10 = *off
059900060728     c                   eval      $tipvid = 'L'
060000060728     c                   endif
060100040413
060200040414      * Esco dal programma
060300060728     c***!!!             Movel     *On           $Fine
060400040413
060500040413     c                   EndSr
060600040413
060700040413      *------------------------------------------------------------------------*
060800040413      * - RIETBE * Riempimento dati tabella
060900040413      *------------------------------------------------------------------------*
061000040413     c     RieTBE        BegSr
061100040413
061200040420     c                   Clear                   TbeAtb
061300040420     c                   If        TbxSif <> *Blanks
061400040420     c                   Eval      TbeSif = TbxSif
061500040413     c                   Else
061600040420     c                   Clear                   TbeSif
061700040413     c                   EndIf
061800040420     c                   If        TbxLin <> *Blanks
061900040420     c                   Eval      TbeLin = TbxLin
062000040414     c                   Else
062100040420     c                   Clear                   TbeLin
062200040414     c                   EndIf
062300040420     c                   Eval      TbeApl = TbxApl
062400040420     c                   Eval      TbeCod = 'CMR'
062500040420     c                   Eval      TbeKe1 = V1cCau
062600040420     c                   Clear                   TbeKe2
062700040413
062800040413     c                   Clear                   dCmr
062900040413     c                   Eval      d§CmrDes = V1cDes
063000040413     c                   Eval      d§CmrDes1 = V1cDes1
063100040413     c                   Move      V1cFar        d§CmrFar
063200040413     c                   Eval      d§CmrNoc = V1cNoc
063300040413     c                   Eval      d§CmrNoq = V1cNoq
063400040413     c                   Eval      d§CmrNmf = V1cNmF
063500040413     c                   Eval      d§CmrNot = V1cNot
063600040414     c                   Eval      d§CmrAdd = V1cAdd
063700040413     c                   Eval      d§CmrDva = V1cDva
063800040413     c                   Eval      d§CmrCva = V1cCva
063900040413     c                   Eval      d§CmrCcc = V1cCcc
064000040413     c                   Eval      d§CmrNca = V1cNca
064100040413     c                   Eval      d§CmrCca = V1cCca
064200041206     c                   Eval      d§CmrRea = V1cRea
064300061218     c                   eval      d§cmrpda = v1cpda
064400061218     c                   eval      d§cmrinfo = v1cinfo
064500061218     c                   eval      d§cmrfirma = v1cfirma
064600040413     c                   Movel(p)  dCmr          TBEuni
064700040413
064800040413     c                   Movel     W1FTT         TBEftt
064900040413     c                   Z-add     TBXflt        TBEflt
065000040413     c                   Clear                   TBEftr
065100040413
065200040413     c                   EndSr
065300060728
065400060728      *---------------------------------------------------------------*
065500060728      * gestione tabella in lingua
065600060728      *---------------------------------------------------------------*
065700060728     c     sr_lingua     begsr
065800060728
065900060728      * pulisco il subfile
066000060728     c                   exsr      sr_pulsfl
066100060728      * carico il subfile con i dati che trovo o vuoto
066200060728     c                   exsr      sr_carsfl
066300060728      * emetto il subfile
066400060728     c     emisfl        tag
066500060728     c                   eval      rec01 = 1
066600060728     c                   if        savrec > 0
066700060728     c                   eval      rec01 = savrec
066800060728     c                   endif
066900060728     c                   write     tb341t01
067000060728     c                   write     tb341z01
067100060728     c                   exfmt     tb341c01
067200060728
067300060728      * F12 - Ritorno
067400060728     c                   if        *inkl
067500060728     c                   eval      $tipvid = '1'
067600060728     c                   leavesr
067700060728     c                   endif
067800060728
067900060728     c                   setoff                                       99
068000060728     c                   setoff                                       45
068100060728      * Controlli
068200060728     c  n10              exsr      sr_ctrsfl
068300060728     c   99
068400060728     cornkf              goto      emisfl
068500060728
068600060728      * F6 - Conferma
068700060728     c   kf              exsr      sr_confsfl
068800060728
068900060728     c                   endsr
069000060728
069100060728      *---------------------------------------------------------------*
069200060728      * pulisco il subfile
069300060728      *---------------------------------------------------------------*
069400060728     c     sr_pulsfl     begsr
069500060728
069600060728     c                   clear                   nrr
069700060728     c                   clear                   savrec
069800060728     c                   eval      *in20 = *off
069900060728     c                   eval      *in21 = *off
070000060728     c                   write     tb341c01
070100060728     c                   eval      *in20 = *on
070200060728     c                   eval      *in21 = *on
070300060728
070400060728     c                   endsr
070500060728
070600060728      *---------------------------------------------------------------*
070700060728      * carico il subfile
070800060728      *---------------------------------------------------------------*
070900060728     c     sr_carsfl     begsr
071000060728
071100060728     c                   setoff                                       23
071200060728      * carico un record per ogni lingua inserite in AZLIN
071300060728     c                   clear                   lintntbe
071400060728     c     lintntbe      setgt     azlin02l
071500060728     c                   do        *hival
071600060728     c                   read      azlin02l
071700060728      * fine file
071800060728     c                   if        %eof(azlin02l)
071900060728     c                   leave
072000060728     c                   endif
072100060728     c                   clear                   dcmr
072200060728     c                   eval      s1dlin = lindesita
072300060728     c                   eval      tbecod = 'CMR'
072400060728     c                   eval      tbeke1 = v1ccau
072500060728     c                   clear                   tbeke2
072600060728     c                   eval      tbelin = lintntbe
072700060728     c     ktbe11        chain(n)  tntbe11l
072800060728     c                   if        not %found(tntbe11l)
072900060728     c                   clear                   s1cdes
073000060728     c                   clear                   s1cdes1
073100060728     c                   clear                   s1cdva
073200060728     c                   else
073300060728     c                   eval      dcmr = tbeuni
073400060728     c                   eval      s1cdes = d§cmrdes
073500060728     c                   eval      s1cdes1 = d§cmrdes1
073600060728     c                   eval      s1cdva = d§cmrdva
073700060728     c                   endif
073800060728     c                   eval      s1hcdlin = tbelin
073900060728     c                   eval      nrr = nrr + 1
074000060728     c                   write     tb341s01
074100060728     c                   enddo
074200060728
074300060728     c                   eval      *in23 = *on
074400060728
074500060728     c                   endsr
074600060728
074700060728      *---------------------------------------------------------------*
074800060728      * controllo il subfile
074900060728      *---------------------------------------------------------------*
075000060728     c     sr_ctrsfl     begsr
075100060728
075200060728     c                   clear                   nrr
075300060728     c                   do        *hival
075400060728     c                   eval      nrr = nrr +1
075500060728     c     nrr           chain     tb341s01                           32
075600060728     c                   if        *in32
075700060728     c                   leave
075800060728     c                   endif
075900060728      * deve esserci la descrizione
076000060728     c                   if        s1cdes = *blanks
076100060728     c                   eval      v1cmsg = $msg(01)
076200060728     c                   eval      *in99 = *on
076300060728     c                   eval      *in45 = *on
076400060728     c                   eval      savrec = nrr
076500060728     c                   update    tb341s01
076600060728     c                   leavesr
076700060728     c                   endif
076800060728     c                   update    tb341s01
076900060728     c                   enddo
077000060728
077100060728     c                   endsr
077200060728
077300060728      *---------------------------------------------------------------*
077400060728      * confermo i dati del subfile
077500060728      *---------------------------------------------------------------*
077600060728     c     sr_confsfl    begsr
077700060728
077800060728      * recupero i dati della tabella in italiano
077900060728     c                   clear                   savtbeuni
078000060728     c                   eval      tbecod = 'CMR'
078100060728     c                   eval      tbeke1 = v1ccau
078200060728     c                   clear                   tbeke2
078300060728     c                   clear                   tbelin
078400060728     c     ktbe11        chain(n)  tntbe11l
078500060728     c                   if        %found(tntbe11l)
078600060728     c                   eval      savtbeuni = tbeuni
078700060728     c                   endif
078800060728
078900060728      * leggo il subfile e aggiorno/scrivo i dati in lingua
079000060728     c                   clear                   nrr
079100060728     c                   do        *hival
079200060728     c                   eval      nrr = nrr +1
079300060728     c     nrr           chain     tb341s01                           32
079400060728     c                   if        *in32
079500060728     c                   leave
079600060728     c                   endif
079700060728     c                   eval      tbelin = s1hcdlin
079800060728     c     ktbe11        chain     tntbe11l
079900060728     c                   eval      dcmr = savtbeuni
080000060728     c                   eval      d§cmrdes = s1cdes
080100060728     c                   eval      d§cmrdes1 = s1cdes1
080200060728     c                   eval      d§cmrdva = s1cdva
080300060728     c                   eval      tbeuni = dcmr
080400060728     c                   eval      tbeftt = w1ftt
080500060728     c                   clear                   tbeftr
080600060728     c                   clear                   tbedtr
080700060728      * scrivo
080800060728     c                   if        not %found(tntbe11l)
080900060728     c                   eval      tbecod = 'CMR'
081000060728     c                   eval      tbeke1 = v1ccau
081100060728     c                   clear                   tbeke2
081200060728     c                   eval      tbelin = s1hcdlin
081300060728     c                   write     tntbe11
081400060728     c                   else
081500060728      * aggiorno
081600060728     c                   update    tntbe11
081700060728     c                   endif
081800060728     c                   enddo
081900060728
082000060728      * Esco dal programma
082100060728     c                   Movel     *On           $Fine
082200060728
082300060728     c                   endsr
082400040408
082500040408      *------------------------------------------------------------------------*
082600040408      * - ROUTINE INIZIALE
082700040408      *------------------------------------------------------------------------*
082800040408     c     *InzSr        BegSr
082900040408
083000040408     c     *Entry        Plist
083100040414     c                   Parm                    kpjba
083200040414     c                   Parm                    Tntb341ds
083300060403
083400060403      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
083500060403     c     *dtaara       define    §azute        AZUTEds
083600060403     c     *dtaara       define    §datiute      dDatiUte
083700060403      *
083800060403     c                   clear                   AZUTEds
083900060403     c                   clear                   dDatiUte
084000060403     c                   clear                   TIBS34ds
084100060403     c                   in(E)     *dtaara
084200060403if  1c                   if        %Error  or  RSUT = *blanks
084300060403     c                   call      'TIBS34R'
084400060403     c                   parm                    TIBS34ds
084500060403     c                   in        *dtaara
084600060403e   1c                   endif
084700060403      *-- Verifica errori e autorità profilo
084800060403sel 1c                   SELECT
084900060403      *-- controllo se ho errori nei dati utente
085000060403      *--   nel qual caso NON risulta un profilo abilitato
085100060403w   1c                   WHEN      DUTerr  = 'E'
085200060403     c                   eval      $Fine   = *on
085300060403      *
085400060403      *-- CONTROLLO AUTORITA'
085500060403      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
085600060403      *-- se il chiamante non richiede autorità specifica verificare
085700060403      *--   quella del profilo
085800060403      *-- se il chiamante richiede autorità specifica verificarla,
085900060403      *--  se è blank verificare quella del profilo
086000060403      *
086100060403      * se UTEAUT = *BLANK non siamo in sede
086200060403w   1c                   WHEN      UTEaut  = *blank
086300060403      *
086400060403x   1c                   OTHER
086500060403      *
086600060403e   1c                   ENDSL
086700040408
086800040408      * Aggancio dati generali della tabella in esame
086900040408     c                   Clear                   TbeCod
087000040408     c                   Move      *Zeros        TbeKe1
087100040408     c                   Move      'CMR'         TbeKe1
087200040408     c                   Clear                   TbeKe2
087300040408     c                   Clear                   TbeLin
087400040408     c                   Movel     knsif         TbeSif
087500040408     c     kTbe          Chain     Tntbe01l
087600040408     c                   If        Not %found(Tntbe01l)
087700040408     c                   Clear                   TbeSif
087800040408     c     kTbe          Chain     Tntbe01l
087900040408     c                   EndIf
088000040408     c                   If        %Found(Tntbe01l)
088100040408     c                   Movel     Tntbeds       xTntbeds
088200040408     c                   Else
088300040408     c                   Clear                   xTntbeds
088400040408     c                   EndIf
088500040408
088600040408      * Definizioni chiavi di accesso
088700040408     c     kTbe          klist
088800040408     c                   kfld                    TbeCod
088900040408     c                   kfld                    TbeKe1
089000040408     c                   kfld                    TbeKe2
089100040408     c                   kfld                    TbeLin
089200040408     c                   kfld                    TbeSif
089300040408
089400060728     c     kTbe11        klist
089500060728     c                   kfld                    tbelin
089600060728     c                   kfld                    tbecod
089700060728     c                   kfld                    tbeke1
089800060728     c                   kfld                    tbeke2
089900060728
090000040408     c                   Clear                   Tb341v01
090100040408
090200040408     c                   EndSr
090300040408      *------------------------------------------------------------------------*
090400040413**   -$OPZ-   *
090500040413  Inserimento
090600040413    Modifica
090700040413     Copia
090800040413  Annullamento
090900040413Visualizzazione
091000040413   ANNULLATO
091100040413**   -$MSG-                                                                  *
091200040413Immettere la descrizione                                                       01
091300040413Immettere la fase                                                              02
091400040413Fase non trovata in tabella                                                    03
091500040413Causale non trovata in tabella                                                 04
091600040413Immettere causale x chiusura ORM automatica                                    05
091700040414Immettere la causale                                                           06
091800040420Causale già esistente                                                          07
