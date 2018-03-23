000100020212      *---------------------------------------------------------------*
000200020212      * Gestione tabella "CNU" = Numeratori di Gruppo                 *
000300020212      *---------------------------------------------------------------*
000400020212
000500020212     H DECEDIT('0,') DATEDIT(*DMY.)
000600020212
000700020212      *---------------------------------------------------------------*
000800020212      *   A R C H I V I                                               *
000900020212      *---------------------------------------------------------------*
001000020212      *
001100020212     FAZORG01L  IF   E           K DISK
001200020212      *
001300020212     FTNTBE01L  UF A E           K DISK
001400020212      *
001500020212     FTNTB24D   CF   E             WORKSTN
001600020212
001700020212      *---------------------------------------------------------------*
001800020212      *   S C H I E R E                                               *
001900020212      *---------------------------------------------------------------*
002000020212      *
002100020215     D Opz             s             15    dim(06) ctdata perrcd(1)             Decodifica OPZ
002200020212      *
002300020306     D Msg             s             78    dim(10) ctdata perrcd(1)             Messaggi video
002400020212      *
002500020215     D NmFl            s             10    dim(10)                              Campi per ricerca
002600020212
002700020212      *---------------------------------------------------------------*
002800020212      *   S T R U T T U R E   D A T I                                 *
002900020212      *---------------------------------------------------------------*
003000020212      *
003100020212      * Parametri
003200020212     D KPJBA         e ds
003300020212      *
003400020212      * Passaggio Parametri al pgm TIBS02R
003500020212     D TIBS02DS      e ds                  inz
003600020212     D  T02mod       e                     inz('C')
003700020212     D  T02cod       e                     inz('CNU')
003800020212      *
003900020212      * Tabella CNU = Codici Numeratori di Gruppo
004000020212     D DCNU          e ds                  inz
004100020212      *
004200020212      * Ds reperimento dato utente
004300020212     D DSFIL         e ds                  inz
004400020212      *
004500020212      * Tracciato record file TNTBE00F
004600020212     D TNTBEds       e ds                  extname(TNTBE00F) inz
004700020212     D xTNTBEds      e ds                  extname(TNTBE00F) inz
004800020212     D                                     prefix(TBX:3)
004900020226
005000020226     D WLBDAT          ds                  inz
005100020226     D  G02DAT                 1      8  0
005200020226     D  G02INV                 9     16  0
005300020226     D  G02ERR                17     17
005400020226     D  G02TGI                18     22  0
005500020212
005600020212      *---------------------------------------------------------------*
005700020212      *   V A R I A B I L I                                           *
005800020212      *---------------------------------------------------------------*
005900020212      *
006000020212     D XX              s              2  0 inz
006100020212     D $Err            s              1a   inz(*off)
006200020212     D $Fine           s              1a   inz(*off)
006300020212     D $CarV1          s              1a   inz(*on)
006400020212     D $CarV2          s              1a   inz(*off)
006500020226     D $CarW1          s              1a   inz(*off)
006600020212     D TipVid          s              1a   inz('1')
006700020212     D Win             s             99a   inz(*zeros)
006800020212     D Wpo             s              3a   inz(*zeros)
006900020214     D WpoN            s              1a   inz
007000020301     D wTasto          s              2a   inz(*zeros)
007100020215     D WMax            s                   like(§CNURFI) inz(*hival)
007200020212
007300020212      *---------------------------------------------------------------*
007400020212      *   C O S T A N T I                                             *
007500020212      *---------------------------------------------------------------*
007600020212      *
007700020212     D DigitN          c                   const('0123456789')
007800020212
007900020212      *---------------------------------------------------------------*
008000020212      *   M A I N   L I N E                                           *
008100020212      *---------------------------------------------------------------*
008200020212      *  Riepilogo indicatori utilizzati:                             *
008300020212      *  --------------------------------                             *
008400020212      *  01 - Record inesistente (inserimento)                        *
008500020212      *  02 - Record esistente   (modifica)                           *
008600020212      *  04 - Record annullato   (ripristino)                         *
008700020301      *  20 - Comodo                                                  *
008800020212      *  22 - Errori in scrittura record (WRITE)                      *
008900020212      *---------------------------------------------------------------*
009000020212      *
009100020212      * Operazioni iniziali
009200020212     C                   exsr      RutInz
009300020212      *
009400020212      * Gestione video
009500020212     C     $Fine         doueq     'S'
009600020212     C     TipVid        caseq     '1'           GesV1
009700020212     C     TipVid        caseq     '2'           GesV2
009800020226     C     TipVid        caseq     'A'           GesW1
009900020212     C                   endcs
010000020212     C                   enddo
010100020212      *
010200020212     C                   movel     *on           *inLR
010300020212
010400020212      *---------------------------------------------------------------*
010500020212      * RutInz - Operazioni Iniziali                                  *
010600020212      *---------------------------------------------------------------*
010700020212     C     RutInz        BEGSR
010800020212      *
010900020212      * Ricezione parametri
011000020212     C     *entry        plist
011100020212     C                   parm                    KPJBA
011200020212      *
011300020212      * Definizioni chiavi di accesso
011400020212     C     K05TBE01      klist                                                  *tntbe01l
011500020212     C                   kfld                    TBEcod                         -tabella
011600020212     C                   kfld                    TBEke1                         -chiave uno
011700020212     C                   kfld                    TBEke2                         -chiave due
011800020212     C                   kfld                    TBElin                         -lingua
011900020212     C                   kfld                    TBEsif                         -s.informativo
012000020212      *
012100020212      * Reperimento dati utente
012200020212     C                   call      'XFIL'
012300020212     C                   parm                    DSFIL
012400020212      *
012500020212      * Aggancio dati generali della tabella "numeratori"
012600020212     C                   clear                   TBEcod
012700020212     C                   move      *zeros        TBEke1
012800020212     C                   move      T02cod        TBEke1
012900020212     C                   clear                   TBEke2
013000020212     C                   clear                   TBElin
013100020212     C                   movel     KNSIF         TBEsif
013200020212     C     K05TBE01      chain     TNTBE01L
013300020212     C                   if        not %found(TNTBE01L)
013400020212     C                   clear                   TBEsif
013500020212     C     K05TBE01      chain     TNTBE01L
013600020212     C                   endif
013700020212     C                   if        %found(TNTBE01L)
013800020212     C                   movel     TNTBEds       xTNTBEds
013900020212     C                   else
014000020212     C                   clear                   xTNTBEds
014100020212     C                   endif
014200020212      *
014300020212     C                   ENDSR
014400020212
014500020212      *---------------------------------------------------------------*
014600020212      * GESV1  - Gestione videata selezione codice tabella            *
014700020212      *---------------------------------------------------------------*
014800020212     C     GESV1         BEGSR
014900020212      *
015000020212      * Inizializzazione videata
015100020212if  1C                   if        $CarV1 = *on
015200020212     C                   exsr      CARV1
015300020212     C                   movel     *off          $CarV1
015400020212e   1C                   endif
015500020212      *
015600020212      * Scrivo la testata
015700020212     C                   clear                   T1opz
015800020212     C                   write     TB24T1
015900020212      *
016000020212      * Se esistono errori sulla videata
016100020212      * emetto la write del formato a indicatori spenti per vedere
016200020212      * le eventuali decodifiche
016300020212if  1C                   if        *in99
016400020212     C                   movea     *in           WIN
016500020212     C                   movea     *zeros        *in(50)
016600020212     C                   write     TB24V1
016700020212     C                   movea     WIN           *in
016800020212e   1C                   endif
016900020212      *
017000020212     C                   exfmt     TB24V1
017100020212     C                   eval      *in99 = *off
017200020301     C                   clear                   V1MSG
017300020212      * F3=Fine
017400020212     C                   if        *inKC
017500020212cas 1C                   exsr      F03V1
017600020212     C                   goto      EndGesV1
017700020212e   1C                   endif
017800020212      *
017900020212      * Ricerche
018000020212     C                   clear                   XX
018100020212     C                   clear                   NMFL
018200020301     C     '?'           scan      V1CNU                                  20
018300020301if  1C                   if        *in20
018400020212     C                   add       1             XX
018500020212     C                   eval      NMFL(XX) = 'V1CNU     '
018600020212     C                   exsr      SEARCH
018700020212e   1C                   endif
018800020212      *
018900020301      * Controllo e decodifiche dati immessi a video
019000020301if  1C                   if        not *in99
019100020212     C                   exsr      CTRV1
019200020301e   1C                   endif
019300020212      *
019400020212      * Passaggio alla videata di dettaglio
019500020212if  1C                   if        not *in99
019600020212     C                   eval      $CarV2 = *on
019700020212     C                   eval      TipVid = '2'
019800020212e   1C                   endif
019900020212      *
020000020212     C     EndGesV1      ENDSR
020100020212
020200020212      *---------------------------------------------------------------*
020300020212      * CARV1  - Caricamento dati prima videata                       *
020400020212      *---------------------------------------------------------------*
020500020212     C     CARV1         BEGSR
020600020212      *
020700020212     C                   movea     *zeros        *in(50)
020800020212     C                   movea     '00000'       *in(01)
020900020212     C                   clear                   TB24V1
021000020212      *
021100020212     C                   move      '?'           V1CNU
021200020212      *
021300020212     C                   ENDSR
021400020212
021500020212      *---------------------------------------------------------------*
021600020212      * CTRV1  - Controllo e decodifica prima videata                 *
021700020212      *---------------------------------------------------------------*
021800020212     C     CTRV1         BEGSR
021900020212      *
022000020212     C                   movea     *zeros        *in(50)
022100020212     C                   clear                   V1msg
022200020212      *
022300020212     C                   clear                   V1CNUD
022400020212      * Codice numeratore obbligatorio
022500020212if  1C                   if        V1CNU = *blanks
022600020212     C                   seton                                        50  99
022700020212     C                   eval      V1msg = Msg(01)
022800020212     C                   goto      EndCtrV1
022900020212x   1C                   else
023000020212      * Controllo numericità del codice numeratore
023100020301     C     DigitN        check     V1CNU                                  20
023200020301if  2C                   if        *in20
023300020212     C                   seton                                        50  99
023400020212     C                   eval      V1msg = Msg(02)
023500020212     C                   goto      EndCtrV1
023600020212e   2C                   endif
023700020212      * Controllo/Decodifica codice numeratore
023800020212     C                   reset                   TIBS02DS
023900020212     C                   movel     KNSIF         T02sif
024000020212     C                   movel     V1CNU         T02ke1
024100020212     C                   call      'TIBS02R'
024200020212     C                   parm                    KPJBA
024300020212     C                   parm                    TIBS02DS
024400020212if  2C                   if        T02err = *blanks
024500020212     C                   movel     T02uni        DCNU
024600020212     C                   movel     §CNUdes       V1CNUD
024700020212x   2C                   else
024800020212     C                   clear                   DCNU
024900020212e   2C                   endif
025000020212e   1C                   endif
025100020212      *
025200020212     C     EndCtrV1      ENDSR
025300020212
025400020212      *---------------------------------------------------------------*
025500020212      * F03V1  - Tasto funzionale F03 -> Fine programma               *
025600020212      *---------------------------------------------------------------*
025700020212     C     F03V1         BEGSR
025800020212      *
025900020215     C                   movel     'S'           $Fine                          fine pgm
026000020212      *
026100020212     C                   ENDSR
026200020212
026300020212      *---------------------------------------------------------------*
026400020212      * SEARCH - Ricerche                                             *
026500020212      *---------------------------------------------------------------*
026600020212     C     SEARCH        BEGSR
026700020301      *
026800020301     C                   eval      *in99 = *on
026900020212      *
027000020212do  1C                   do        10            XX
027100020212      *
027200020212if  2C                   if        NMFL(XX) = *blanks
027300020212     C                   leave
027400020212e   2C                   endif
027500020212      *
027600020212sel 2C                   select
027700020212      * Codice numeratore
027800020212w   2C                   when      NMFL(XX) =  'V1CNU     '
027900020212     C                   reset                   TIBS02DS
028000020212     C                   movel     'R'           T02Mod
028100020212     C                   movel     KNSIF         T02Sif
028200020212     C                   call      'TIBS02R'
028300020212     C                   parm                    KPJBA
028400020212     C                   parm                    TIBS02DS
028500020212if  3C                   if        T02Err =  *blanks
028600020212if  4C                   if        T02ke1 <> *blanks
028700020212     C                   movel     T02ke1        V1CNU
028800020212     C                   movel     T02uni        DCNU
028900020212     C                   movel     §CNUDES       V1CNUD
029000020212e   4C                   endif
029100020212x   3C                   else
029200020212     C                   clear                   V1CNUD
029300020212e   3C                   endif
029400020212      *
029500020212e   2C                   endsl
029600020212      *
029700020212e   1C                   enddo
029800020212      *
029900020212     C                   ENDSR
030000020212
030100020212      *---------------------------------------------------------------*
030200020212      * GESV2  - Gestione videata dettaglio dati                      *
030300020212      *---------------------------------------------------------------*
030400020212     C     GESV2         BEGSR
030500020212      *
030600020212      * Inizializzazione videata
030700020212     C                   if        $CarV2 = *on
030800020212     C                   exsr      CarV2
030900020212     C                   move      *off          $CarV2
031000020212     C                   endif
031100020212      * Scrivo la testata
031200020212     C                   write     TB24T1
031300020212      *
031400020212      * Se esistono errori sulla videata
031500020212      * emetto la write del formato a indicatori spenti per vedere
031600020212      * le eventuali decodifiche
031700020212     C                   if        *in99
031800020212     C                   movea     *in           Win
031900020212     C                   movea     *zeros        *in(50)
032000020212     C                   write     TB24V2
032100020212     C                   movea     Win           *in
032200020212     C                   endif
032300020212      *
032400020212if  1C                   if        *in05
032500020212     C                   write     TB24V2
032600020212     C                   exfmt     PROTECT
032700020212x   1C                   else
032800020212     C                   exfmt     TB24V2
032900020212e   1C                   endif
033000020212     C                   eval      *in99 = *off
033100020301     C                   clear                   V2MSG
033200020301     C                   clear                   wTasto
033300020212      *
033400020212sel 1C                   select
033500020212      * F03=Fine
033600020212w   1C                   when      *inKC
033700020212     C                   exsr      F03V1
033800020212     C                   goto      EndGesV2
033900020212      * F12=Ritorno
034000020212w   1C                   when      *inKL
034100020212     C                   exsr      F12V2
034200020212     C                   goto      EndGesV2
034300020212e   1C                   endsl
034400020212      *
034500020212      * Controllo dati immessi a video
034600020212     C                   exsr      CTRV2
034700020212      *
034800020212      * Aggiornamento se non ci sono errori
034900020212if  1C                   if        not *in99
035000020212     C                             and (*inKF or *inKE or *inKQ)
035100020301sel 2C                   select
035200020301w   2C                   when      *inKE
035300020301     C                   eval      wTasto = '05'
035400020301w   2C                   when      *inKF
035500020301     C                   eval      wTasto = '06'
035600020301w   2C                   when      *inKQ
035700020301     C                   eval      wTasto = '16'
035800020301e   2C                   endsl
035900020226     C                   eval      $CarW1 = *on
036000020226     C                   eval      TipVid = 'A'
036100020212e   1C                   endif
036200020212      *
036300020212     C     EndGesV2      ENDSR
036400020212
036500020212      *---------------------------------------------------------------*
036600020212      * CARV2  - Caricamento dati seconda videata                     *
036700020212      *---------------------------------------------------------------*
036800020212     C     CARV2         BEGSR
036900020212      *
037000020212     C                   clear                   T1opz
037100020212     C                   movea     '00000'       *in(01)
037200020212      *
037300020212      * Aggancio la tabella, se trovo il codice sono in modifica
037400020212      * o ripristino (se record annullato), altrimenti in immissione
037500020212     C                   exsr      CHNTBE
037600020212      *
037700020212if  1C                   if        %found(TNTBE01L)
037800020212      *
037900020212      * MODIFICA/RIPRISTINO
038000020212     C                   movel     TBEuni        DCNU
038100020212if  2C                   if        TBEatb = *blanks
038200020212     C                   eval      *in02  = *on
038300020212     C                   eval      T1opz  = Opz(02)
038400020212x   2C                   else
038500020212     C                   eval      *in04  = *on
038600020212     C                   eval      T1opz  = Opz(06)
038700020212e   2C                   endif
038800020212      *
038900020212x   1C                   else
039000020212      *
039100020212      * IMMISSIONE
039200020212     c                   clear                   DCNU
039300020212     C                   eval      *in01  = *on
039400020212     C                   eval      T1opz  = Opz(01)
039500020212      *
039600020212e   1C                   endif
039700020212      *
039800020212     C                   move      V1CNU         V2CNU
039900020212     C                   eval      V2CNUD = §CNUdes
040000020212     C                   eval      V2EDP  = §CNUedp
040100020212     C                   eval      V2FAA  = §CNUfaa
040200020212     C                   eval      V2KDA  = §CNUkda
040300020212     C                   eval      V2FNU  = §CNUfnu
040400020226     C                   eval      V2P1N  = §CNUp1n
040500020212     C                   eval      V2PO1  = §CNUpo1
040600020226     C                   eval      V2P2N  = §CNUp2n
040700020212     C                   eval      V2PO2  = §CNUpo2
040800020212     C                   eval      V2RIN  = §CNUrin
040900020212     C                   eval      V2RFI  = §CNUrfi
041000020212     C                   eval      V2INC  = §CNUinc
041100020212     C                   eval      V2BIO  = §CNUbio
041200020212     C                   eval      V2CTN  = §CNUctn
041300020212     C                   eval      V2NCR  = §CNUncr
041400020308     C                   eval      V2SCA  = §CNUsca
041500020212     c
041600020212     C                   ENDSR
041700020212
041800020212      *---------------------------------------------------------------*
041900020212      * F12V2  - Tasto funzionale F12 -> Ritorno                      *
042000020212      *---------------------------------------------------------------*
042100020212     C     F12V2         BEGSR
042200020212      *
042300020212     C                   eval      TipVid = '1'
042400020212      *
042500020212     C                   ENDSR
042600020212
042700020212      *---------------------------------------------------------------*
042800020212      * CTRV2  - Controllo e decodifica seconda videata               *
042900020212      *---------------------------------------------------------------*
043000020212     C     CTRV2         BEGSR
043100020212      *
043200020212     C                   movea     *zeros        *in(50)
043300020212     C                   clear                   V2msg
043400020212      *
043500020212      * Non si fanno i controlli se richisto l'annullamento
043600020212     C     *inKQ         cabeq     *on           EndCtrV2
043700020212      *
043800020212sel 1C                   select
043900020313      *
044000020212      * Descrizione obbligatoria
044100020306w   1C                   when      V2cnud =  *blanks
044200020212     C                   seton                                        51  99
044300020212     C                   movel     Msg(03)       V2msg
044400020212      *
044500020212      * Numeratore definito "annuale" e "giornaliero"
044600020212w   1C                   when          V2faa = 'S'
044700020212     C                             and V2kda = 'S'
044800020212     C                   seton                                        535499
044900020212     C                   movel     Msg(04)       V2msg
045000020212      *
045100020212      * Range obbligatorio
045200020306     C                   when      V2rin  =  0   and   V2rfi  =  0
045300020212     C                   seton                                        616299
045400020212     C                   movel     Msg(06)       V2msg
045500020212      *
045600020212      * Inizio range errato
045700020313w   1C                   when      V2rin  <  1
045800020313     C                   seton                                        61  99
045900020313     C                   movel     Msg(07)       V2msg
046000020212      *
046100020212      * Fine range errato
046200020306     C                   when      V2rfi  <  V2rin + V2inc
046300020212     C                   seton                                        626499
046400020306     C                   movel     Msg(08)       V2msg
046500020212      *
046600020212      * Incremento progressivo obbligatorio
046700020306w   1C                   when      V2inc  =  0
046800020212     C                   seton                                        64  99
046900020306     C                   movel     Msg(09)       V2msg
047000020215      *
047100020215      * Incremento progressivo errato
047200020306w   1C                   when      V2inc  <  0
047300020215     C                   seton                                        64  99
047400020306     C                   movel     Msg(10)       V2msg
047500020212      *
047600020215e   1C                   endsl
047700020214     C     *in99         cabeq     *on           EndCtrV2
047800020214      *
047900020214      * P.O.1 errato o mancante
048000020306if  1C                   if        V2po1  <> 'TUT' and  V2po1  <> 'FIL'
048100020306     C                                             and  V2po1  <> *blanks
048200020214     C                   movel     V2po1         Wpo
048300020214     C                   movel     V2p1n         WpoN
048400020214     C                   exsr      CtrPO
048500020214if  2C                   if        $Err = *on
048600020214     C                   seton                                        56  99
048700020214     C                   movel     Msg(05)       V2msg
048800020214     C                   goto      EndCtrV2
048900020214e   2C                   endif
049000020214e   1C                   endif
049100020214      *
049200020214      * P.O.2 errato o mancante
049300020306if  1C                   if        V2po2  <> 'TUT' and  V2po2  <> 'FIL'
049400020306     C                                             and  V2po2  <> *blanks
049500020214     C                   movel     V2po2         Wpo
049600020214     C                   movel     V2p2n         WpoN
049700020214     C                   exsr      CtrPO
049800020214if  2C                   if        $Err = *on
049900020214     C                   seton                                        57  99
050000020214     C                   movel     Msg(05)       V2msg
050100020214     C                   goto      EndCtrV2
050200020214e   2C                   endif
050300020214e   1C                   endif
050400020212      *
050500020212     C     EndCtrV2      ENDSR
050600020212
050700020212      *---------------------------------------------------------------*
050800020212      * CTRPO  - Controllo Punto Operativo                            *
050900020212      *---------------------------------------------------------------*
051000020212     C     CTRPO         BEGSR
051100020212      *
051200020212     C                   movel     *off          $Err
051300020212      *
051400020301     C     DigitN        check     Wpo                                    20
051500020301if  1C                   if        *in20
051600020212     C                   eval      $Err = *on
051700020212     C                   goto      EndCtrPO
051800020212e   1C                   endif
051900020212      *
052000020214     C     WpoN          cabeq     'S'           EndCtrPO
052100020214      *
052200020212if  1C                   if        Wpo = *zeros
052300020212     C                   eval      $Err = *on
052400020212     C                   goto      EndCtrPO
052500020212e   1C                   endif
052600020212      *
052700020212     C                   move      Wpo           ORGfil
052800020212     C     ORGfil        chain     AZORG01L
052900020212if  1C                   if        not %found(AZORG01L)
053000020212     C                   eval      $Err = *on
053100020212     C                   goto      EndCtrPO
053200020212e   1C                   endif
053300020212      *
053400020212     C     EndCtrPO      ENDSR
053500020226
053600020226      *---------------------------------------------------------------*
053700020226      * GESW1  - Gestione videata dati relativi alla trasmissione     *
053800020226      *---------------------------------------------------------------*
053900020226     C     GESW1         BEGSR
054000020226      *
054100020226      * Inizializzazione videata
054200020226if  1C                   if        $CarW1 = *on
054300020226     C                   exsr      CARW1
054400020226     C                   movel     *off          $CarW1
054500020226e   1C                   endif
054600020226      *
054700020226if  1C                   if        *in05
054800020226     C                   write     TB24W1
054900020226     C                   exfmt     PROTECT
055000020226x   1C                   else
055100020226     C                   exfmt     TB24W1
055200020226e   1C                   endif
055300020226     C                   eval      *in99 = *off
055400020301     C                   clear                   W1MSG
055500020226      *
055600020226sel 1C                   select
055700020226      * F12=Ritorno
055800020226w   1C                   when      *inKL
055900020226     C                   exsr      F12W1
056000020226     C                   goto      EndGesW1
056100020226e   1C                   endsl
056200020226      *
056300020226      * Controllo dati immessi a video
056400020226     C                   exsr      CTRW1
056500020226      *
056600020226      * Aggiornamento se non ci sono errori
056700020301if  1C                   if        not *in99 and *inKF
056800020226     C                   exsr      AGGTBE
056900020226e   1C                   endif
057000020226      *
057100020226     C     EndGesW1      ENDSR
057200020313
057300020226      *---------------------------------------------------------------*
057400020226      * CARW1  - Caricamento dati window                              *
057500020226      *---------------------------------------------------------------*
057600020226     C     CARW1         BEGSR
057700020226      *
057800020226     C                   movea     *zeros        *in(50)
057900020226      *
058000020226sel 1C                   select
058100020226      *
058200020226      * F5=Ripristino
058300020226w   1C                   when      *inKE   and  *in04
058400020226     C                   eval      W1FTT = TBEftt
058500020226      *
058600020226      * F6=Conferma
058700020226w   1C                   when      *inKF
058800020226sel 2C                   select
058900020226      *   Immissione
059000020226w   2C                   when      *in01
059100020313     C                   eval      W1FTT = TBXftt
059200020226      *   Modifica / Ripristino
059300020226w   2C                   when      *in02   or    *in04
059400020313     C                   eval      W1FTT = TBEftt
059500020226e   2C                   endsl
059600020226      *
059700020226      * F16=Annullamento
059800020226w   1C                   when      *inKQ   and  not *in04
059900020313     C                   eval      W1FTT = TBEftt
060000020226      *
060100020226e   1C                   endsl
060200020226      *
060300020226      * Se NON immissione: visualizzo i dati relativi all'ultima
060400020226      *   trasmissione
060500020226if  1C                   if        not *in01
060600020226     C                   eval      W1FLT = TBEflt
060700020226     C                   eval      W1FTR = TBEftr
060800020226if  2C                   if        TBEdtr <> 0
060900020226     C                   clear                   WLBDAT
061000020226     C                   z-add     TBEdtr        G02inv
061100020226     C                   movel     '3'           G02err
061200020226     C                   call      'XSRDA8'
061300020226     C                   parm                    WLBDAT
061400020226     C                   z-add     G02dat        W1DTR
061500020226e   2C                   endif
061600020226e   1C                   endif
061700020226      *
061800020226     C                   ENDSR
061900020226
062000020226      *---------------------------------------------------------------*
062100020226      * CTRW1  - Controllo e decodifica window                        *
062200020226      *---------------------------------------------------------------*
062300020226     C     CTRW1         BEGSR
062400020226      *
062500020226     C                   movea     *zeros        *in(50)
062600020226      *
062700020226     C     EndCtrW1      ENDSR
062800020226
062900020226      *---------------------------------------------------------------*
063000020226      * F21W1  - Tasto funzionale F12 -> Ritorno                      *
063100020226      *---------------------------------------------------------------*
063200020226     C     F12W1         BEGSR
063300020226      *
063400020226     C                   eval      TipVid = '2'
063500020226      *
063600020226     C                   ENDSR
063700020212
063800020212      *---------------------------------------------------------------*
063900020212      * CHNTBE * Aggancio tabella                                     *
064000020212      *---------------------------------------------------------------*
064100020212     C     CHNTBE        BEGSR
064200020212      *
064300020212     C                   movel     T02cod        TBEcod
064400020212     C                   movel(p)  V1CNU         TBEke1
064500020212     C                   clear                   TBEke2
064600020212     C                   clear                   TBElin
064700020212     C                   movel     KNSIF         TBEsif
064800020212     C     K05TBE01      chain     TNTBE01L
064900020212      * Se non ho trovato il record con il sistema informativo
065000020212      * che ho in linea, lo abblenco
065100020212if  1C                   if        not %found(TNTBE01L)
065200020212     C                   clear                   TBEsif
065300020212     C     K05TBE01      chain     TNTBE01L
065400020212e   1C                   endif
065500020212      *
065600020212     C                   ENDSR
065700020212
065800020212      *---------------------------------------------------------------*
065900020212      * AGGTBE * Aggiornamento tabella                                *
066000020212      *---------------------------------------------------------------*
066100020212     C     AGGTBE        BEGSR
066200020212      *
066300020212sel 1C                   select
066400020212      *
066500020212      * F5=Ripristino
066600020301w   1C                   when      wTasto='05'  and  *in04
066700020212     C                   clear                   TBEatb
066800020212     C                   clear                   TBEftr
066900020212     C                   UPDATE    TNTBE000
067000020212      *
067100020212      * F6=Conferma
067200020301w   1C                   when      wTasto='06'
067300020212sel 2C                   select
067400020212      *   Immissione
067500020212w   2C                   when      *in01
067600020212     C                   exsr      RIETBE
067700020226     C                   clear                   TBEflt
067800020226     C                   clear                   TBEdtr
067900020212     C                   WRITE     TNTBE000                             22
068000020212      *   Modifica / Ripristino
068100020212w   2C                   when      *in02   or    *in04
068200020212     C                   exsr      RIETBE
068300020212     C                   UPDATE    TNTBE000
068400020212e   2C                   endsl
068500020212      *
068600020212      * F16=Annullamento
068700020301w   1C                   when      wTasto='16'  and  not *in04
068800020212     C                   movel     'A'           TBEatb
068900020212     C                   clear                   TBEftr
069000020212     C                   UPDATE    TNTBE000
069100020212      *
069200020212e   1C                   endsl
069300020212      *
069400020212      * Torno alla prima videata che carico come da inizio pgm
069500020212     C                   movel     '1'           TipVid
069600020212     C                   movel     *on           $CarV1
069700020226     C                   movel     *on           $CarV2
069800020226     C                   movel     *on           $CarW1
069900020212     C
070000020212     C                   ENDSR
070100020212
070200020212      *---------------------------------------------------------------*
070300020212      * Riempimento file tabella
070400020212      *---------------------------------------------------------------*
070500020212     C     RIETBE        BEGSR
070600020212      *
070700020212     C                   clear                   TBEatb
070800020212     C                   if        TBXsif <> *blanks
070900020212     C                   movel     KNSIF         TBEsif
071000020212     C                   else
071100020212     C                   clear                   TBEsif
071200020212     C                   endif
071300020212     C                   movel     TBXapl        TBEapl
071400020212     C                   movel     T02cod        TBEcod
071500020212     C                   movel     V2cnu         TBEke1
071600020212     C                   clear                   TBEke2
071700020212      *
071800020212     C                   clear                   DCNU
071900020212     C                   eval      §CNUdes = V2cnud
072000020212     C                   eval      §CNUedp = V2edp
072100020212     C                   eval      §CNUkda = V2kda
072200020212     C                   eval      §CNUfaa = V2faa
072300020212     C                   eval      §CNUfnu = V2fnu
072400020212     C                   eval      §CNUpo1 = V2po1
072500020212     C                   eval      §CNUpo2 = V2po2
072600020212     C                   eval      §CNUp1n = V2p1n
072700020212     C                   eval      §CNUp2n = V2p2n
072800020212     C                   eval      §CNUrin = V2rin
072900020212     C                   eval      §CNUrfi = V2rfi
073000020212     C                   eval      §CNUinc = V2inc
073100020212     C                   eval      §CNUbio = V2bio
073200020212     C                   eval      §CNUctn = V2ctn
073300020212     C                   eval      §CNUncr = V2ncr
073400020308     C                   eval      §CNUsca = V2sca
073500020212     C                   movel(p)  DCNU          TBEuni
073600020212      *
073700020226     C                   movel     W1FTT         TBEftt
073800020212     C                   z-add     TBXflt        TBEflt
073900020212     C                   clear                   TBEftr
074000020212     C
074100020212     C                   ENDSR
074200020212
074300020212** -OPZ-      *
074400020212  Inserimento
074500020212    Modifica
074600020212     Copia
074700020212  Annullamento
074800020212Visualizzazione
074900020212   ANNULLATO
075000020212** -MSG-                                                                     *
075100020313Immettere il codice numeratore                                                 01
075200020313Il codice numeratore deve essere numerico                                      02
075300020313Immettere la descrizione                                                       03
075400020313Se il numeratore è "annuale" non può avere la data in chiave (e viceversa)     04
075500020313Il punto operativo è errato                                                    05
075600020313Immettere il range                                                             06
075700020313Il valore iniziale del range è obbligatorio e deve essere maggiore di zero     07
075800020306Il valore finale deve essere superiore al valore iniziale di almeno 1 increm.  08
075900020313Immettere il numero di incremento progressivo                                  09
076000020313Il numero di incremento progressivo è errato                                   10
