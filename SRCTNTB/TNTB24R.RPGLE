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
004200020212      * Tracciato record file TNTBE00F
004300020212     D TNTBEds       e ds                  extname(TNTBE00F) inz
004400020212     D xTNTBEds      e ds                  extname(TNTBE00F) inz
004500020212     D                                     prefix(TBX:3)
004600020724
004700020724     D TIBS34DS      e ds                  inz
004800020724     D DDATIUTE      e ds                  inz
004900020724     D AZUTEDS       e ds                  inz extname(AZUTE00F)
005000020226
005100020226     D WLBDAT          ds                  inz
005200020226     D  G02DAT                 1      8  0
005300020226     D  G02INV                 9     16  0
005400020226     D  G02ERR                17     17
005500020226     D  G02TGI                18     22  0
005600020212
005700020212      *---------------------------------------------------------------*
005800020212      *   V A R I A B I L I                                           *
005900020212      *---------------------------------------------------------------*
006000020212      *
006100020212     D XX              s              2  0 inz
006200020212     D $Err            s              1a   inz(*off)
006300020212     D $Fine           s              1a   inz(*off)
006400020212     D $CarV1          s              1a   inz(*on)
006500020212     D $CarV2          s              1a   inz(*off)
006600020226     D $CarW1          s              1a   inz(*off)
006700020212     D TipVid          s              1a   inz('1')
006800020212     D Win             s             99a   inz(*zeros)
006900020212     D Wpo             s              3a   inz(*zeros)
007000020214     D WpoN            s              1a   inz
007100020301     D wTasto          s              2a   inz(*zeros)
007200020215     D WMax            s                   like(§CNURFI) inz(*hival)
007300020212
007400020212      *---------------------------------------------------------------*
007500020212      *   C O S T A N T I                                             *
007600020212      *---------------------------------------------------------------*
007700020212      *
007800020212     D DigitN          c                   const('0123456789')
007900020212
008000020212      *---------------------------------------------------------------*
008100020212      *   M A I N   L I N E                                           *
008200020212      *---------------------------------------------------------------*
008300020212      *  Riepilogo indicatori utilizzati:                             *
008400020212      *  --------------------------------                             *
008500020212      *  01 - Record inesistente (inserimento)                        *
008600020212      *  02 - Record esistente   (modifica)                           *
008700020212      *  04 - Record annullato   (ripristino)                         *
008800020301      *  20 - Comodo                                                  *
008900020212      *  22 - Errori in scrittura record (WRITE)                      *
009000020212      *---------------------------------------------------------------*
009100020212      *
009200020212      * Operazioni iniziali
009300020212     C                   exsr      RutInz
009400020212      *
009500020212      * Gestione video
009600020724     C     $Fine         downe     'S'
009700020212     C     TipVid        caseq     '1'           GesV1
009800020212     C     TipVid        caseq     '2'           GesV2
009900020226     C     TipVid        caseq     'A'           GesW1
010000020212     C                   endcs
010100020212     C                   enddo
010200020212      *
010300020212     C                   movel     *on           *inLR
010400020212
010500020212      *---------------------------------------------------------------*
010600020212      * RutInz - Operazioni Iniziali                                  *
010700020212      *---------------------------------------------------------------*
010800020212     C     RutInz        BEGSR
010900020212      *
011000020212      * Ricezione parametri
011100020212     C     *entry        plist
011200020212     C                   parm                    KPJBA
011300020212      *
011400020212      * Definizioni chiavi di accesso
011500020212     C     K05TBE01      klist                                                  *tntbe01l
011600020212     C                   kfld                    TBEcod                         -tabella
011700020212     C                   kfld                    TBEke1                         -chiave uno
011800020212     C                   kfld                    TBEke2                         -chiave due
011900020212     C                   kfld                    TBElin                         -lingua
012000020212     C                   kfld                    TBEsif                         -s.informativo
012100020724      *
012200020724      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
012300020724     C     *dtaara       define    §azute        azuteds
012400020724     C     *dtaara       define    §datiute      ddatiute
012500020724      *
012600020724     C                   clear                   AzUteDs
012700020724     C                   clear                   DDatiUte
012800020724     C                   clear                   Tibs34Ds
012900020724     C                   in(E)     *dtaara
013000020724if  1C                   IF        %Error  or  RSUT = *blanks
013100020724     C                   call      'TIBS34R'
013200020724     C                   parm                    Tibs34Ds
013300020724     C                   in        *dtaara
013400020724e   1C                   ENDIF
013500020724      *-- Verifica errori e autorità profilo
013600020724sel 1C                   SELECT
013700020724      *-- controllo se ho errori nei dati utente
013800020724      *--   nel qual caso NON risulta un profilo abilitato
013900020724w   1C                   WHEN      DUTERR = 'E'
014000020724     C                   eval      $Fine  = 'S'
014100020724      *
014200020724      *-- CONTROLLO AUTORITA'
014300020724      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
014400020724      *-- se il chiamante non richiede autorità specifica verificare
014500020724      *--   quella del profilo
014600020724      *-- se il chiamante richiede autorità specifica verificarla,
014700020724      *--  se è blank verificare quella del profilo
014800020724      *
014900020724      * se UTEAUT = *BLANK non siamo in sede
015000020724w   1C                   WHEN      UTEAUT = *blank
015100020724      *
015200020724x   1c                   OTHER
015300020724      *
015400020724e   1C                   ENDSL
015500020212      *
015600020212      * Aggancio dati generali della tabella "numeratori"
015700020212     C                   clear                   TBEcod
015800020212     C                   move      *zeros        TBEke1
015900020212     C                   move      T02cod        TBEke1
016000020212     C                   clear                   TBEke2
016100020212     C                   clear                   TBElin
016200020212     C                   movel     KNSIF         TBEsif
016300020212     C     K05TBE01      chain     TNTBE01L
016400020212     C                   if        not %found(TNTBE01L)
016500020212     C                   clear                   TBEsif
016600020212     C     K05TBE01      chain     TNTBE01L
016700020212     C                   endif
016800020212     C                   if        %found(TNTBE01L)
016900020212     C                   movel     TNTBEds       xTNTBEds
017000020212     C                   else
017100020212     C                   clear                   xTNTBEds
017200020212     C                   endif
017300020212      *
017400020212     C                   ENDSR
017500020212
017600020212      *---------------------------------------------------------------*
017700020212      * GESV1  - Gestione videata selezione codice tabella            *
017800020212      *---------------------------------------------------------------*
017900020212     C     GESV1         BEGSR
018000020212      *
018100020212      * Inizializzazione videata
018200020212if  1C                   if        $CarV1 = *on
018300020212     C                   exsr      CARV1
018400020212     C                   movel     *off          $CarV1
018500020212e   1C                   endif
018600020212      *
018700020212      * Scrivo la testata
018800020212     C                   clear                   T1opz
018900020212     C                   write     TB24T1
019000020212      *
019100020212      * Se esistono errori sulla videata
019200020212      * emetto la write del formato a indicatori spenti per vedere
019300020212      * le eventuali decodifiche
019400020212if  1C                   if        *in99
019500020212     C                   movea     *in           WIN
019600020212     C                   movea     *zeros        *in(50)
019700020212     C                   write     TB24V1
019800020212     C                   movea     WIN           *in
019900020212e   1C                   endif
020000020212      *
020100020212     C                   exfmt     TB24V1
020200020212     C                   eval      *in99 = *off
020300020301     C                   clear                   V1MSG
020400020212      * F3=Fine
020500020212     C                   if        *inKC
020600020212cas 1C                   exsr      F03V1
020700020212     C                   goto      EndGesV1
020800020212e   1C                   endif
020900020212      *
021000020212      * Ricerche
021100020212     C                   clear                   XX
021200020212     C                   clear                   NMFL
021300020301     C     '?'           scan      V1CNU                                  20
021400020301if  1C                   if        *in20
021500020212     C                   add       1             XX
021600020212     C                   eval      NMFL(XX) = 'V1CNU     '
021700020212     C                   exsr      SEARCH
021800020212e   1C                   endif
021900020212      *
022000020301      * Controllo e decodifiche dati immessi a video
022100020301if  1C                   if        not *in99
022200020212     C                   exsr      CTRV1
022300020301e   1C                   endif
022400020212      *
022500020212      * Passaggio alla videata di dettaglio
022600020212if  1C                   if        not *in99
022700020212     C                   eval      $CarV2 = *on
022800020212     C                   eval      TipVid = '2'
022900020212e   1C                   endif
023000020212      *
023100020212     C     EndGesV1      ENDSR
023200020212
023300020212      *---------------------------------------------------------------*
023400020212      * CARV1  - Caricamento dati prima videata                       *
023500020212      *---------------------------------------------------------------*
023600020212     C     CARV1         BEGSR
023700020212      *
023800020212     C                   movea     *zeros        *in(50)
023900020212     C                   movea     '00000'       *in(01)
024000020212     C                   clear                   TB24V1
024100020212      *
024200020212     C                   move      '?'           V1CNU
024300020212      *
024400020212     C                   ENDSR
024500020212
024600020212      *---------------------------------------------------------------*
024700020212      * CTRV1  - Controllo e decodifica prima videata                 *
024800020212      *---------------------------------------------------------------*
024900020212     C     CTRV1         BEGSR
025000020212      *
025100020212     C                   movea     *zeros        *in(50)
025200020212     C                   clear                   V1msg
025300020212      *
025400020212     C                   clear                   V1CNUD
025500020212      * Codice numeratore obbligatorio
025600020212if  1C                   if        V1CNU = *blanks
025700020212     C                   seton                                        50  99
025800020212     C                   eval      V1msg = Msg(01)
025900020212     C                   goto      EndCtrV1
026000020212x   1C                   else
026100020212      * Controllo numericità del codice numeratore
026200020301     C     DigitN        check     V1CNU                                  20
026300020301if  2C                   if        *in20
026400020212     C                   seton                                        50  99
026500020212     C                   eval      V1msg = Msg(02)
026600020212     C                   goto      EndCtrV1
026700020212e   2C                   endif
026800020212      * Controllo/Decodifica codice numeratore
026900020212     C                   reset                   TIBS02DS
027000020212     C                   movel     KNSIF         T02sif
027100020212     C                   movel     V1CNU         T02ke1
027200020212     C                   call      'TIBS02R'
027300020212     C                   parm                    KPJBA
027400020212     C                   parm                    TIBS02DS
027500020212if  2C                   if        T02err = *blanks
027600020212     C                   movel     T02uni        DCNU
027700020212     C                   movel     §CNUdes       V1CNUD
027800020212x   2C                   else
027900020212     C                   clear                   DCNU
028000020212e   2C                   endif
028100020212e   1C                   endif
028200020212      *
028300020212     C     EndCtrV1      ENDSR
028400020212
028500020212      *---------------------------------------------------------------*
028600020212      * F03V1  - Tasto funzionale F03 -> Fine programma               *
028700020212      *---------------------------------------------------------------*
028800020212     C     F03V1         BEGSR
028900020212      *
029000020215     C                   movel     'S'           $Fine                          fine pgm
029100020212      *
029200020212     C                   ENDSR
029300020212
029400020212      *---------------------------------------------------------------*
029500020212      * SEARCH - Ricerche                                             *
029600020212      *---------------------------------------------------------------*
029700020212     C     SEARCH        BEGSR
029800020301      *
029900020301     C                   eval      *in99 = *on
030000020212      *
030100020212do  1C                   do        10            XX
030200020212      *
030300020212if  2C                   if        NMFL(XX) = *blanks
030400020212     C                   leave
030500020212e   2C                   endif
030600020212      *
030700020212sel 2C                   select
030800020212      * Codice numeratore
030900020212w   2C                   when      NMFL(XX) =  'V1CNU     '
031000020212     C                   reset                   TIBS02DS
031100020212     C                   movel     'R'           T02Mod
031200020212     C                   movel     KNSIF         T02Sif
031300020212     C                   call      'TIBS02R'
031400020212     C                   parm                    KPJBA
031500020212     C                   parm                    TIBS02DS
031600020212if  3C                   if        T02Err =  *blanks
031700020212if  4C                   if        T02ke1 <> *blanks
031800020212     C                   movel     T02ke1        V1CNU
031900020212     C                   movel     T02uni        DCNU
032000020212     C                   movel     §CNUDES       V1CNUD
032100020212e   4C                   endif
032200020212x   3C                   else
032300020212     C                   clear                   V1CNUD
032400020212e   3C                   endif
032500020212      *
032600020212e   2C                   endsl
032700020212      *
032800020212e   1C                   enddo
032900020212      *
033000020212     C                   ENDSR
033100020212
033200020212      *---------------------------------------------------------------*
033300020212      * GESV2  - Gestione videata dettaglio dati                      *
033400020212      *---------------------------------------------------------------*
033500020212     C     GESV2         BEGSR
033600020212      *
033700020212      * Inizializzazione videata
033800020212     C                   if        $CarV2 = *on
033900020212     C                   exsr      CarV2
034000020212     C                   move      *off          $CarV2
034100020212     C                   endif
034200020212      * Scrivo la testata
034300020212     C                   write     TB24T1
034400020212      *
034500020212      * Se esistono errori sulla videata
034600020212      * emetto la write del formato a indicatori spenti per vedere
034700020212      * le eventuali decodifiche
034800020212     C                   if        *in99
034900020212     C                   movea     *in           Win
035000020212     C                   movea     *zeros        *in(50)
035100020212     C                   write     TB24V2
035200020212     C                   movea     Win           *in
035300020212     C                   endif
035400020212      *
035500020212if  1C                   if        *in05
035600020212     C                   write     TB24V2
035700020212     C                   exfmt     PROTECT
035800020212x   1C                   else
035900020212     C                   exfmt     TB24V2
036000020212e   1C                   endif
036100020212     C                   eval      *in99 = *off
036200020301     C                   clear                   V2MSG
036300020301     C                   clear                   wTasto
036400020212      *
036500020212sel 1C                   select
036600020212      * F03=Fine
036700020212w   1C                   when      *inKC
036800020212     C                   exsr      F03V1
036900020212     C                   goto      EndGesV2
037000020212      * F12=Ritorno
037100020212w   1C                   when      *inKL
037200020212     C                   exsr      F12V2
037300020212     C                   goto      EndGesV2
037400020212e   1C                   endsl
037500020212      *
037600020212      * Controllo dati immessi a video
037700020212     C                   exsr      CTRV2
037800020212      *
037900020212      * Aggiornamento se non ci sono errori
038000020212if  1C                   if        not *in99
038100020212     C                             and (*inKF or *inKE or *inKQ)
038200020301sel 2C                   select
038300020301w   2C                   when      *inKE
038400020301     C                   eval      wTasto = '05'
038500020301w   2C                   when      *inKF
038600020301     C                   eval      wTasto = '06'
038700020301w   2C                   when      *inKQ
038800020301     C                   eval      wTasto = '16'
038900020301e   2C                   endsl
039000020226     C                   eval      $CarW1 = *on
039100020226     C                   eval      TipVid = 'A'
039200020212e   1C                   endif
039300020212      *
039400020212     C     EndGesV2      ENDSR
039500020212
039600020212      *---------------------------------------------------------------*
039700020212      * CARV2  - Caricamento dati seconda videata                     *
039800020212      *---------------------------------------------------------------*
039900020212     C     CARV2         BEGSR
040000020212      *
040100020212     C                   clear                   T1opz
040200020212     C                   movea     '00000'       *in(01)
040300020212      *
040400020212      * Aggancio la tabella, se trovo il codice sono in modifica
040500020212      * o ripristino (se record annullato), altrimenti in immissione
040600020212     C                   exsr      CHNTBE
040700020212      *
040800020212if  1C                   if        %found(TNTBE01L)
040900020212      *
041000020212      * MODIFICA/RIPRISTINO
041100020212     C                   movel     TBEuni        DCNU
041200020212if  2C                   if        TBEatb = *blanks
041300020212     C                   eval      *in02  = *on
041400020212     C                   eval      T1opz  = Opz(02)
041500020212x   2C                   else
041600020212     C                   eval      *in04  = *on
041700020212     C                   eval      T1opz  = Opz(06)
041800020212e   2C                   endif
041900020212      *
042000020212x   1C                   else
042100020212      *
042200020212      * IMMISSIONE
042300020212     c                   clear                   DCNU
042400020212     C                   eval      *in01  = *on
042500020212     C                   eval      T1opz  = Opz(01)
042600020212      *
042700020212e   1C                   endif
042800020212      *
042900020212     C                   move      V1CNU         V2CNU
043000020212     C                   eval      V2CNUD = §CNUdes
043100020212     C                   eval      V2EDP  = §CNUedp
043200020212     C                   eval      V2FAA  = §CNUfaa
043300020212     C                   eval      V2KDA  = §CNUkda
043400020212     C                   eval      V2FNU  = §CNUfnu
043500020226     C                   eval      V2P1N  = §CNUp1n
043600020212     C                   eval      V2PO1  = §CNUpo1
043700020226     C                   eval      V2P2N  = §CNUp2n
043800020212     C                   eval      V2PO2  = §CNUpo2
043900020212     C                   eval      V2RIN  = §CNUrin
044000020212     C                   eval      V2RFI  = §CNUrfi
044100020212     C                   eval      V2INC  = §CNUinc
044200020212     C                   eval      V2BIO  = §CNUbio
044300020212     C                   eval      V2CTN  = §CNUctn
044400020212     C                   eval      V2NCR  = §CNUncr
044500020308     C                   eval      V2SCA  = §CNUsca
044600020212     c
044700020212     C                   ENDSR
044800020212
044900020212      *---------------------------------------------------------------*
045000020212      * F12V2  - Tasto funzionale F12 -> Ritorno                      *
045100020212      *---------------------------------------------------------------*
045200020212     C     F12V2         BEGSR
045300020212      *
045400020212     C                   eval      TipVid = '1'
045500020212      *
045600020212     C                   ENDSR
045700020212
045800020212      *---------------------------------------------------------------*
045900020212      * CTRV2  - Controllo e decodifica seconda videata               *
046000020212      *---------------------------------------------------------------*
046100020212     C     CTRV2         BEGSR
046200020212      *
046300020212     C                   movea     *zeros        *in(50)
046400020212     C                   clear                   V2msg
046500020212      *
046600020212      * Non si fanno i controlli se richisto l'annullamento
046700020212     C     *inKQ         cabeq     *on           EndCtrV2
046800020212      *
046900020212sel 1C                   select
047000020313      *
047100020212      * Descrizione obbligatoria
047200020306w   1C                   when      V2cnud =  *blanks
047300020212     C                   seton                                        51  99
047400020212     C                   movel     Msg(03)       V2msg
047500020212      *
047600020212      * Numeratore definito "annuale" e "giornaliero"
047700020212w   1C                   when          V2faa = 'S'
047800020212     C                             and V2kda = 'S'
047900020212     C                   seton                                        535499
048000020212     C                   movel     Msg(04)       V2msg
048100020212      *
048200020212      * Range obbligatorio
048300020306     C                   when      V2rin  =  0   and   V2rfi  =  0
048400020212     C                   seton                                        616299
048500020212     C                   movel     Msg(06)       V2msg
048600020212      *
048700020212      * Inizio range errato
048800020313w   1C                   when      V2rin  <  1
048900020313     C                   seton                                        61  99
049000020313     C                   movel     Msg(07)       V2msg
049100020212      *
049200020212      * Fine range errato
049300020306     C                   when      V2rfi  <  V2rin + V2inc
049400020212     C                   seton                                        626499
049500020306     C                   movel     Msg(08)       V2msg
049600020212      *
049700020212      * Incremento progressivo obbligatorio
049800020306w   1C                   when      V2inc  =  0
049900020212     C                   seton                                        64  99
050000020306     C                   movel     Msg(09)       V2msg
050100020215      *
050200020215      * Incremento progressivo errato
050300020306w   1C                   when      V2inc  <  0
050400020215     C                   seton                                        64  99
050500020306     C                   movel     Msg(10)       V2msg
050600020212      *
050700020215e   1C                   endsl
050800020214     C     *in99         cabeq     *on           EndCtrV2
050900020214      *
051000020214      * P.O.1 errato o mancante
051100020306if  1C                   if        V2po1  <> 'TUT' and  V2po1  <> 'FIL'
051200020306     C                                             and  V2po1  <> *blanks
051300020214     C                   movel     V2po1         Wpo
051400020214     C                   movel     V2p1n         WpoN
051500020214     C                   exsr      CtrPO
051600020214if  2C                   if        $Err = *on
051700020214     C                   seton                                        56  99
051800020214     C                   movel     Msg(05)       V2msg
051900020214     C                   goto      EndCtrV2
052000020214e   2C                   endif
052100020214e   1C                   endif
052200020214      *
052300020214      * P.O.2 errato o mancante
052400020306if  1C                   if        V2po2  <> 'TUT' and  V2po2  <> 'FIL'
052500020306     C                                             and  V2po2  <> *blanks
052600020214     C                   movel     V2po2         Wpo
052700020214     C                   movel     V2p2n         WpoN
052800020214     C                   exsr      CtrPO
052900020214if  2C                   if        $Err = *on
053000020214     C                   seton                                        57  99
053100020214     C                   movel     Msg(05)       V2msg
053200020214     C                   goto      EndCtrV2
053300020214e   2C                   endif
053400020214e   1C                   endif
053500020212      *
053600020212     C     EndCtrV2      ENDSR
053700020212
053800020212      *---------------------------------------------------------------*
053900020212      * CTRPO  - Controllo Punto Operativo                            *
054000020212      *---------------------------------------------------------------*
054100020212     C     CTRPO         BEGSR
054200020212      *
054300020212     C                   movel     *off          $Err
054400020212      *
054500020301     C     DigitN        check     Wpo                                    20
054600020301if  1C                   if        *in20
054700020212     C                   eval      $Err = *on
054800020212     C                   goto      EndCtrPO
054900020212e   1C                   endif
055000020212      *
055100020214     C     WpoN          cabeq     'S'           EndCtrPO
055200020214      *
055300020212if  1C                   if        Wpo = *zeros
055400020212     C                   eval      $Err = *on
055500020212     C                   goto      EndCtrPO
055600020212e   1C                   endif
055700020212      *
055800020212     C                   move      Wpo           ORGfil
055900020212     C     ORGfil        chain     AZORG01L
056000020212if  1C                   if        not %found(AZORG01L)
056100020212     C                   eval      $Err = *on
056200020212     C                   goto      EndCtrPO
056300020212e   1C                   endif
056400020212      *
056500020212     C     EndCtrPO      ENDSR
056600020226
056700020226      *---------------------------------------------------------------*
056800020226      * GESW1  - Gestione videata dati relativi alla trasmissione     *
056900020226      *---------------------------------------------------------------*
057000020226     C     GESW1         BEGSR
057100020226      *
057200020226      * Inizializzazione videata
057300020226if  1C                   if        $CarW1 = *on
057400020226     C                   exsr      CARW1
057500020226     C                   movel     *off          $CarW1
057600020226e   1C                   endif
057700020226      *
057800020226if  1C                   if        *in05
057900020226     C                   write     TB24W1
058000020226     C                   exfmt     PROTECT
058100020226x   1C                   else
058200020226     C                   exfmt     TB24W1
058300020226e   1C                   endif
058400020226     C                   eval      *in99 = *off
058500020301     C                   clear                   W1MSG
058600020226      *
058700020226sel 1C                   select
058800020226      * F12=Ritorno
058900020226w   1C                   when      *inKL
059000020226     C                   exsr      F12W1
059100020226     C                   goto      EndGesW1
059200020226e   1C                   endsl
059300020226      *
059400020226      * Controllo dati immessi a video
059500020226     C                   exsr      CTRW1
059600020226      *
059700020226      * Aggiornamento se non ci sono errori
059800020301if  1C                   if        not *in99 and *inKF
059900020226     C                   exsr      AGGTBE
060000020226e   1C                   endif
060100020226      *
060200020226     C     EndGesW1      ENDSR
060300020313
060400020226      *---------------------------------------------------------------*
060500020226      * CARW1  - Caricamento dati window                              *
060600020226      *---------------------------------------------------------------*
060700020226     C     CARW1         BEGSR
060800020226      *
060900020226     C                   movea     *zeros        *in(50)
061000020226      *
061100020226sel 1C                   select
061200020226      *
061300020226      * F5=Ripristino
061400020226w   1C                   when      *inKE   and  *in04
061500020226     C                   eval      W1FTT = TBEftt
061600020226      *
061700020226      * F6=Conferma
061800020226w   1C                   when      *inKF
061900020226sel 2C                   select
062000020226      *   Immissione
062100020226w   2C                   when      *in01
062200020313     C                   eval      W1FTT = TBXftt
062300020226      *   Modifica / Ripristino
062400020226w   2C                   when      *in02   or    *in04
062500020313     C                   eval      W1FTT = TBEftt
062600020226e   2C                   endsl
062700020226      *
062800020226      * F16=Annullamento
062900020226w   1C                   when      *inKQ   and  not *in04
063000020313     C                   eval      W1FTT = TBEftt
063100020226      *
063200020226e   1C                   endsl
063300020226      *
063400020226      * Se NON immissione: visualizzo i dati relativi all'ultima
063500020226      *   trasmissione
063600020226if  1C                   if        not *in01
063700020226     C                   eval      W1FLT = TBEflt
063800020226     C                   eval      W1FTR = TBEftr
063900020226if  2C                   if        TBEdtr <> 0
064000020226     C                   clear                   WLBDAT
064100020226     C                   z-add     TBEdtr        G02inv
064200020226     C                   movel     '3'           G02err
064300020226     C                   call      'XSRDA8'
064400020226     C                   parm                    WLBDAT
064500020226     C                   z-add     G02dat        W1DTR
064600020226e   2C                   endif
064700020226e   1C                   endif
064800020226      *
064900020226     C                   ENDSR
065000020226
065100020226      *---------------------------------------------------------------*
065200020226      * CTRW1  - Controllo e decodifica window                        *
065300020226      *---------------------------------------------------------------*
065400020226     C     CTRW1         BEGSR
065500020226      *
065600020226     C                   movea     *zeros        *in(50)
065700020226      *
065800020226     C     EndCtrW1      ENDSR
065900020226
066000020226      *---------------------------------------------------------------*
066100020226      * F21W1  - Tasto funzionale F12 -> Ritorno                      *
066200020226      *---------------------------------------------------------------*
066300020226     C     F12W1         BEGSR
066400020226      *
066500020226     C                   eval      TipVid = '2'
066600020226      *
066700020226     C                   ENDSR
066800020212
066900020212      *---------------------------------------------------------------*
067000020212      * CHNTBE * Aggancio tabella                                     *
067100020212      *---------------------------------------------------------------*
067200020212     C     CHNTBE        BEGSR
067300020212      *
067400020212     C                   movel     T02cod        TBEcod
067500020212     C                   movel(p)  V1CNU         TBEke1
067600020212     C                   clear                   TBEke2
067700020212     C                   clear                   TBElin
067800020212     C                   movel     KNSIF         TBEsif
067900020212     C     K05TBE01      chain     TNTBE01L
068000020212      * Se non ho trovato il record con il sistema informativo
068100020212      * che ho in linea, lo abblenco
068200020212if  1C                   if        not %found(TNTBE01L)
068300020212     C                   clear                   TBEsif
068400020212     C     K05TBE01      chain     TNTBE01L
068500020212e   1C                   endif
068600020212      *
068700020212     C                   ENDSR
068800020212
068900020212      *---------------------------------------------------------------*
069000020212      * AGGTBE * Aggiornamento tabella                                *
069100020212      *---------------------------------------------------------------*
069200020212     C     AGGTBE        BEGSR
069300020212      *
069400020212sel 1C                   select
069500020212      *
069600020212      * F5=Ripristino
069700020301w   1C                   when      wTasto='05'  and  *in04
069800020212     C                   clear                   TBEatb
069900020212     C                   clear                   TBEftr
070000020212     C                   UPDATE    TNTBE000
070100020212      *
070200020212      * F6=Conferma
070300020301w   1C                   when      wTasto='06'
070400020212sel 2C                   select
070500020212      *   Immissione
070600020212w   2C                   when      *in01
070700020212     C                   exsr      RIETBE
070800020226     C                   clear                   TBEflt
070900020226     C                   clear                   TBEdtr
071000020212     C                   WRITE     TNTBE000                             22
071100020212      *   Modifica / Ripristino
071200020212w   2C                   when      *in02   or    *in04
071300020212     C                   exsr      RIETBE
071400020212     C                   UPDATE    TNTBE000
071500020212e   2C                   endsl
071600020212      *
071700020212      * F16=Annullamento
071800020301w   1C                   when      wTasto='16'  and  not *in04
071900020212     C                   movel     'A'           TBEatb
072000020212     C                   clear                   TBEftr
072100020212     C                   UPDATE    TNTBE000
072200020212      *
072300020212e   1C                   endsl
072400020212      *
072500020212      * Torno alla prima videata che carico come da inizio pgm
072600020212     C                   movel     '1'           TipVid
072700020212     C                   movel     *on           $CarV1
072800020226     C                   movel     *on           $CarV2
072900020226     C                   movel     *on           $CarW1
073000020212     C
073100020212     C                   ENDSR
073200020212
073300020212      *---------------------------------------------------------------*
073400020212      * Riempimento file tabella
073500020212      *---------------------------------------------------------------*
073600020212     C     RIETBE        BEGSR
073700020212      *
073800020212     C                   clear                   TBEatb
073900020212     C                   if        TBXsif <> *blanks
074000020212     C                   movel     KNSIF         TBEsif
074100020212     C                   else
074200020212     C                   clear                   TBEsif
074300020212     C                   endif
074400020212     C                   movel     TBXapl        TBEapl
074500020212     C                   movel     T02cod        TBEcod
074600020212     C                   movel     V2cnu         TBEke1
074700020212     C                   clear                   TBEke2
074800020212      *
074900020212     C                   clear                   DCNU
075000020212     C                   eval      §CNUdes = V2cnud
075100020212     C                   eval      §CNUedp = V2edp
075200020212     C                   eval      §CNUkda = V2kda
075300020212     C                   eval      §CNUfaa = V2faa
075400020212     C                   eval      §CNUfnu = V2fnu
075500020212     C                   eval      §CNUpo1 = V2po1
075600020212     C                   eval      §CNUpo2 = V2po2
075700020212     C                   eval      §CNUp1n = V2p1n
075800020212     C                   eval      §CNUp2n = V2p2n
075900020212     C                   eval      §CNUrin = V2rin
076000020212     C                   eval      §CNUrfi = V2rfi
076100020212     C                   eval      §CNUinc = V2inc
076200020212     C                   eval      §CNUbio = V2bio
076300020212     C                   eval      §CNUctn = V2ctn
076400020212     C                   eval      §CNUncr = V2ncr
076500020308     C                   eval      §CNUsca = V2sca
076600020212     C                   movel(p)  DCNU          TBEuni
076700020212      *
076800020226     C                   movel     W1FTT         TBEftt
076900020212     C                   z-add     TBXflt        TBEflt
077000020212     C                   clear                   TBEftr
077100020212     C
077200020212     C                   ENDSR
077300020212
077400020212** -OPZ-      *
077500020212  Inserimento
077600020212    Modifica
077700020212     Copia
077800020212  Annullamento
077900020212Visualizzazione
078000020212   ANNULLATO
078100020212** -MSG-                                                                     *
078200020313Immettere il codice numeratore                                                 01
078300020313Il codice numeratore deve essere numerico                                      02
078400020313Immettere la descrizione                                                       03
078500020313Se il numeratore è "annuale" non può avere la data in chiave (e viceversa)     04
078600020313Il punto operativo è errato                                                    05
078700020313Immettere il range                                                             06
078800020313Il valore iniziale del range è obbligatorio e deve essere maggiore di zero     07
078900020306Il valore finale deve essere superiore al valore iniziale di almeno 1 increm.  08
079000020313Immettere il numero di incremento progressivo                                  09
079100020313Il numero di incremento progressivo è errato                                   10
