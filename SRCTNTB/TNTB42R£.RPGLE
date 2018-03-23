000100020212      *---------------------------------------------------------------*
000200020912      * Gestione tabella "SCH" = Schedulatore                         *
000300020212      *---------------------------------------------------------------*
000400020212
000500020212     H DECEDIT('0,') DATEDIT(*DMY.)
000600020212
000700020212      *---------------------------------------------------------------*
000800020212      *   A R C H I V I                                               *
000900020212      *---------------------------------------------------------------*
001000020212      *
001100020212     FTNTBE01L  UF A E           K DISK
001200020913     FKFSIF01L  IF   E           K DISK
001300020918     FKPPRF01L  IF   E           K DISK
001400020212      *
001500020912     FTNTB28D   CF   E             WORKSTN
001600020212
001700020212      *---------------------------------------------------------------*
001800020212      *   S C H I E R E                                               *
001900020212      *---------------------------------------------------------------*
002000020212      *
002100020215     D Opz             s             15    dim(06) ctdata perrcd(1)             Decodifica OPZ
002200020212      *
002300021025     D Msg             s             78    dim(16) ctdata perrcd(1)             Messaggi video
002400020212      *
002500020215     D NmFl            s             10    dim(10)                              Campi per ricerca
002600020913      *
002700020913     D Ora             s              4  0 dim(15)                              Orari
002800020212
002900020212      *---------------------------------------------------------------*
003000020212      *   S T R U T T U R E   D A T I                                 *
003100020212      *---------------------------------------------------------------*
003200020212      *
003300020212      * Parametri
003400020212     D KPJBA         e ds
003500020212      *
003600020212      * Passaggio Parametri al pgm TIBS02R
003700020212     D TIBS02DS      e ds                  inz
003800020212     D  T02mod       e                     inz('C')
003900020912     D  T02cod       e                     inz('SCH')
004000020212      *
004100020912      * Tabella SCH = Schedulatore
004200020912     D DSCH          e ds                  inz
004300020212      *
004400020212      * Tracciato record file TNTBE00F
004500020212     D TNTBEds       e ds                  extname(TNTBE00F) inz
004600020212     D xTNTBEds      e ds                  extname(TNTBE00F) inz
004700020212     D                                     prefix(TBX:3)
004800020724
004900020724     D TIBS34DS      e ds                  inz
005000020724     D DDATIUTE      e ds                  inz
005100020724     D AZUTEDS       e ds                  inz extname(AZUTE00F)
005200020226
005300020226     D WLBDAT          ds                  inz
005400020226     D  G02DAT                 1      8  0
005500020226     D  G02INV                 9     16  0
005600020226     D  G02ERR                17     17
005700020226     D  G02TGI                18     22  0
005800021114
005900021114     d param01         ds
006000021114     d  p01sch                        4
006100021114     d  p01ord                        1
006200021114     d  p01sif                       10
006300021114     d  p01ke1                        4
006400021114     d  p01rit                        1
006500020212
006600020212      *---------------------------------------------------------------*
006700020212      *   V A R I A B I L I                                           *
006800020212      *---------------------------------------------------------------*
006900020212      *
007000020212     D XX              s              2  0 inz
007100020913     D yy              s              2  0 inz
007200020212     D $Err            s              1a   inz(*off)
007300020212     D $Fine           s              1a   inz(*off)
007400020913     D $CarV0          s              1a   inz(*on)
007500020913     D $CarV1          s              1a   inz(*off)
007600020212     D $CarV2          s              1a   inz(*off)
007700020226     D $CarW1          s              1a   inz(*off)
007800020913     D TipVid          s              1a   inz('0')
007900020212     D Win             s             99a   inz(*zeros)
008000020301     D wTasto          s              2a   inz(*zeros)
008100020913     d totora          s              5  0
008200020913     d wora            s              2  0
008300020913     d wminuti         s              2  0
008400020212
008500020212      *---------------------------------------------------------------*
008600020212      *   M A I N   L I N E                                           *
008700020212      *---------------------------------------------------------------*
008800020212      *  Riepilogo indicatori utilizzati:                             *
008900020212      *  --------------------------------                             *
009000020212      *  01 - Record inesistente (inserimento)                        *
009100020212      *  02 - Record esistente   (modifica)                           *
009200020212      *  04 - Record annullato   (ripristino)                         *
009300020301      *  20 - Comodo                                                  *
009400020212      *  22 - Errori in scrittura record (WRITE)                      *
009500020212      *---------------------------------------------------------------*
009600020212      *
009700020212      * Operazioni iniziali
009800020212     C                   exsr      RutInz
009900020212      *
010000020212      * Gestione video
010100020724     C     $Fine         downe     'S'
010200020913     C     TipVid        caseq     '0'           GesV0
010300020212     C     TipVid        caseq     '1'           GesV1
010400020212     C     TipVid        caseq     '2'           GesV2
010500020226     C     TipVid        caseq     'A'           GesW1
010600020212     C                   endcs
010700020212     C                   enddo
010800020212      *
010900020212     C                   movel     *on           *inLR
011000020212
011100020212      *---------------------------------------------------------------*
011200020212      * RutInz - Operazioni Iniziali                                  *
011300020212      *---------------------------------------------------------------*
011400020212     C     RutInz        BEGSR
011500020212      *
011600020212      * Ricezione parametri
011700020212     C     *entry        plist
011800020212     C                   parm                    KPJBA
011900020212      *
012000020212      * Definizioni chiavi di accesso
012100020212     C     K05TBE01      klist                                                  *tntbe01l
012200020212     C                   kfld                    TBEcod                         -tabella
012300020212     C                   kfld                    TBEke1                         -chiave uno
012400020212     C                   kfld                    TBEke2                         -chiave due
012500020212     C                   kfld                    TBElin                         -lingua
012600020212     C                   kfld                    TBEsif                         -s.informativo
012700020724      *
012800020724      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
012900020724     C     *dtaara       define    §azute        azuteds
013000020724     C     *dtaara       define    §datiute      ddatiute
013100020724      *
013200020724     C                   clear                   AzUteDs
013300020724     C                   clear                   DDatiUte
013400020724     C                   clear                   Tibs34Ds
013500020724     C                   in(E)     *dtaara
013600020724if  1C                   IF        %Error  or  RSUT = *blanks
013700020724     C                   call      'TIBS34R'
013800020724     C                   parm                    Tibs34Ds
013900020724     C                   in        *dtaara
014000020724e   1C                   ENDIF
014100020724      *-- Verifica errori e autorità profilo
014200020724sel 1C                   SELECT
014300020724      *-- controllo se ho errori nei dati utente
014400020724      *--   nel qual caso NON risulta un profilo abilitato
014500020724w   1C                   WHEN      DUTERR = 'E'
014600020724     C                   eval      $Fine  = 'S'
014700020724      *
014800020724      *-- CONTROLLO AUTORITA'
014900020724      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
015000020724      *-- se il chiamante non richiede autorità specifica verificare
015100020724      *--   quella del profilo
015200020724      *-- se il chiamante richiede autorità specifica verificarla,
015300020724      *--  se è blank verificare quella del profilo
015400020724      *
015500020724      * se UTEAUT = *BLANK non siamo in sede
015600020724w   1C                   WHEN      UTEAUT = *blank
015700020724      *
015800020724x   1c                   OTHER
015900020724      *
016000020724e   1C                   ENDSL
016100020913
016200020913     c                   EndSr
016300020913      *---------------------------------------------------------------*
016400020913      * GESV0  - Gestione videata richiesta S.I.                      *
016500020913      *---------------------------------------------------------------*
016600020913     C     GESV0         BEGSR
016700020913      *
016800020913      * Inizializzazione videata
016900020913if  1C                   if        $CarV0 = *on
017000020913     C                   exsr      CARV0
017100020913     C                   movel     *off          $CarV0
017200020913e   1C                   endif
017300020913      *
017400020913      * Scrivo la testata
017500020913     C                   clear                   T1opz
017600020913     C                   write     TB28T1
017700020913      *
017800020913      * Se esistono errori sulla videata
017900020913      * emetto la write del formato a indicatori spenti per vedere
018000020913      * le eventuali decodifiche
018100020913if  1C                   if        *in99
018200020913     C                   movea     *in           WIN
018300020913     C                   movea     *zeros        *in(50)
018400020913     C                   write     TB28V0
018500020913     C                   movea     WIN           *in
018600020913e   1C                   endif
018700020913      *
018800020913     C                   exfmt     TB28V0
018900020913     C                   eval      *in99 = *off
019000020913     C                   clear                   V0MSG
019100020913      * F3=Fine
019200020913     C                   if        *inKC
019300020913cas 1C                   exsr      F03V0
019400020913     C                   goto      EndGesV0
019500020913e   1C                   endif
019600020913      *
019700020913      * Controllo se immesso il s.i.
019800020913if  1C                   if        not *in99
019900020913     C                   exsr      CTRV0
020000020913e   1C                   endif
020100020212      *
020200020913      * Aggancio dati generali della tabella SCH'
020300020212     C                   clear                   TBEcod
020400020212     C                   move      *zeros        TBEke1
020500020212     C                   move      T02cod        TBEke1
020600020212     C                   clear                   TBEke2
020700020212     C                   clear                   TBElin
020800020913     C                   movel     V0Sif         TBEsif
020900020212     C     K05TBE01      chain     TNTBE01L
021000020212     C                   if        not %found(TNTBE01L)
021100020212     C                   clear                   TBEsif
021200020212     C     K05TBE01      chain     TNTBE01L
021300020212     C                   endif
021400020212     C                   if        %found(TNTBE01L)
021500020212     C                   movel     TNTBEds       xTNTBEds
021600020212     C                   else
021700020212     C                   clear                   xTNTBEds
021800020212     C                   endif
021900020913      *
022000020913      * Passaggio alla videata di dettaglio
022100020913if  1C                   if        not *in99
022200020913     C                   eval      $CarV1 = *on
022300020913     C                   eval      TipVid = '1'
022400020913e   1C                   endif
022500020212      *
022600020913     C     EndGesV0      ENDSR
022700020212
022800020913      *---------------------------------------------------------------*
022900020913      * CARV0  - Caricamento dati videata S.I.                        *
023000020913      *---------------------------------------------------------------*
023100020913     C     CARV0         BEGSR
023200020913      *
023300020913     C                   movea     *zeros        *in(50)
023400020913     C                   movea     '00000'       *in(01)
023500020913     C                   clear                   TB28V0
023600020913      *
023700020913     C                   move      Knsif         V0sif
023800020913      *
023900020913     C                   ENDSR
024000020913
024100020913      *---------------------------------------------------------------*
024200020913      * CTRV0  - Controllo videata s.i.                               *
024300020913      *---------------------------------------------------------------*
024400020913     C     CTRV0         BEGSR
024500020913      *
024600020913     C                   movea     *zeros        *in(50)
024700020913     C                   clear                   V0msg
024800020913      *
024900020913      * S.I. obbligatorio
025000020913if  1C                   if        V0SIF = *blanks
025100020913     C                   seton                                        50  99
025200020913     C                   eval      V0msg = Msg(01)
025300020913     C                   goto      EndCtrV0
025400020913e   1C                   endif
025500020913
025600020913      * Deve essere un s.i. valido
025700020913     c     v0sif         chain     kfsif01l
025800020913     c                   if        not %Found(kfsif01l)
025900020913     C                   seton                                        50  99
026000020913     C                   eval      V0msg = Msg(02)
026100020913     C                   goto      EndCtrV0
026200020913     c                   endif
026300020913      *
026400020913     C     EndCtrV0      ENDSR
026500020913
026600020913      *---------------------------------------------------------------*
026700020913      * F03V0  - Tasto funzionale F03 -> Fine programma               *
026800020913      *---------------------------------------------------------------*
026900020913     C     F03V0         BEGSR
027000020913      *
027100020913     C                   movel     'S'           $Fine                          fine pgm
027200020913      *
027300020913     C                   ENDSR
027400020212      *---------------------------------------------------------------*
027500020212      * GESV1  - Gestione videata selezione codice tabella            *
027600020212      *---------------------------------------------------------------*
027700020212     C     GESV1         BEGSR
027800020212      *
027900020212      * Inizializzazione videata
028000020212if  1C                   if        $CarV1 = *on
028100020212     C                   exsr      CARV1
028200020212     C                   movel     *off          $CarV1
028300020212e   1C                   endif
028400020913      * Scrivo la testata
028500020913     C                   clear                   T1opz
028600020913     C                   write     TB28T1
028700020913      *
028800020913      * Se esistono errori sulla videata
028900020913      * emetto la write del formato a indicatori spenti per vedere
029000020913      * le eventuali decodifiche
029100020913     C                   if        *in99
029200020913     C                   movea     *in           Win
029300020913     C                   movea     *zeros        *in(50)
029400020913     C                   write     TB28V1
029500020913     C                   movea     Win           *in
029600020913     C                   endif
029700020913      *
029800020913     C                   exfmt     TB28V1
029900020913     C                   eval      *in99 = *off
030000020913     C                   clear                   V1MSG
030100020913      *
030200020913sel 1C                   select
030300020913      * F03=Fine
030400020913w   1C                   when      *inKC
030500020913     C                   exsr      F03V0
030600020913     C                   goto      EndGesV1
030700020913      * F12=Ritorno
030800020913w   1C                   when      *inKL
030900020913     C                   exsr      F12V1
031000020913     C                   goto      EndGesV1
031100020913e   1C                   endsl
031200020212      *
031300020212      * Ricerche
031400020212     C                   clear                   XX
031500020212     C                   clear                   NMFL
031600020912     C     '?'           scan      V1AZN                                  20
031700020301if  1C                   if        *in20
031800020212     C                   add       1             XX
031900020912     C                   eval      NMFL(XX) = 'V1AZN     '
032000020212     C                   exsr      SEARCH
032100020212e   1C                   endif
032200020212      *
032300020301      * Controllo e decodifiche dati immessi a video
032400020301if  1C                   if        not *in99
032500020212     C                   exsr      CTRV1
032600020301e   1C                   endif
032700020212      *
032800020212      * Passaggio alla videata di dettaglio
032900020212if  1C                   if        not *in99
033000020212     C                   eval      $CarV2 = *on
033100020212     C                   eval      TipVid = '2'
033200020212e   1C                   endif
033300020212      *
033400020212     C     EndGesV1      ENDSR
033500020212
033600020212      *---------------------------------------------------------------*
033700020212      * CARV1  - Caricamento dati prima videata                       *
033800020212      *---------------------------------------------------------------*
033900020212     C     CARV1         BEGSR
034000020212      *
034100020913     C                   movea     *zeros        *in(50)
034200020913     C                   clear                   TB28V1
034300020913
034400020912     C                   move      '?'           V1AZN
034500020212      *
034600020212     C                   ENDSR
034700020212
034800020212      *---------------------------------------------------------------*
034900020212      * CTRV1  - Controllo e decodifica prima videata                 *
035000020212      *---------------------------------------------------------------*
035100020212     C     CTRV1         BEGSR
035200020212      *
035300020913     C                   movea     *zeros        *in(50)
035400020212     C                   clear                   V1msg
035500020212      *
035600020912     C                   clear                   V1AZND
035700020912      * Azione obbligatoria
035800020912if  1C                   if        V1AZN = *blanks
035900020913     C                   seton                                        51  99
036000020913     C                   eval      V1msg = Msg(03)
036100020212     C                   goto      EndCtrV1
036200020212x   1C                   else
036300020212      * Controllo/Decodifica codice numeratore
036400020212     C                   reset                   TIBS02DS
036500020913     C                   movel     TbxSif        T02sif
036600020912     C                   movel     V1AZN         T02ke1
036700020212     C                   call      'TIBS02R'
036800020212     C                   parm                    KPJBA
036900020212     C                   parm                    TIBS02DS
037000020212if  2C                   if        T02err = *blanks
037100020912     C                   movel     T02uni        DSCH
037200020912     C                   movel     d§SCHdes      V1AZND
037300020212x   2C                   else
037400020912     C                   clear                   DSCH
037500020212e   2C                   endif
037600020212e   1C                   endif
037700020212      *
037800020212     C     EndCtrV1      ENDSR
037900020913
038000020913      *---------------------------------------------------------------*
038100020913      * F12V1  - Tasto funzionale F12 -> Ritorno                      *
038200020913      *---------------------------------------------------------------*
038300020913     C     F12V1         BEGSR
038400020913      *
038500020913     C                   eval      TipVid = '0'
038600020913      *
038700020913     C                   ENDSR
038800020212
038900020212      *---------------------------------------------------------------*
039000020212      * SEARCH - Ricerche                                             *
039100020212      *---------------------------------------------------------------*
039200020212     C     SEARCH        BEGSR
039300020301      *
039400020301     C                   eval      *in99 = *on
039500020212      *
039600020212do  1C                   do        10            XX
039700020212      *
039800020212if  2C                   if        NMFL(XX) = *blanks
039900020212     C                   leave
040000020212e   2C                   endif
040100020212      *
040200020212sel 2C                   select
040300020912      * Azione
040400020912w   2C                   when      NMFL(XX) =  'V1AZN     '
040500021114     C                   reset                   Param01
040600021114     C                   movel     TbxSif        P01Sif
040700021114     c                   Movel     Param01       Kpjbu
040800021114     C                   call      'TNTB281R'
040900020212     C                   parm                    KPJBA
041000021114     c                   Movel     Kpjbu         Param01
041100021114if  4C                   if        P01ke1 <> *blanks
041200021114     C                   movel     P01ke1        V1AZN
041300020212x   3C                   else
041400020912     C                   clear                   V1AZND
041500020212e   3C                   endif
041600020212      *
041700020212e   2C                   endsl
041800020212      *
041900020212e   1C                   enddo
042000020212      *
042100020212     C                   ENDSR
042200020913
042300020212      *---------------------------------------------------------------*
042400020212      * GESV2  - Gestione videata dettaglio dati                      *
042500020212      *---------------------------------------------------------------*
042600020212     C     GESV2         BEGSR
042700020212      *
042800020212      * Inizializzazione videata
042900020212     C                   if        $CarV2 = *on
043000020212     C                   exsr      CarV2
043100020212     C                   move      *off          $CarV2
043200020212     C                   endif
043300020212      * Scrivo la testata
043400020912     C                   write     TB28T1
043500020212      *
043600020212      * Se esistono errori sulla videata
043700020212      * emetto la write del formato a indicatori spenti per vedere
043800020212      * le eventuali decodifiche
043900020212     C                   if        *in99
044000020212     C                   movea     *in           Win
044100020913     C                   movea     *zeros        *in(50)
044200020912     C                   write     TB28V2
044300020212     C                   movea     Win           *in
044400020212     C                   endif
044500020212      *
044600020212if  1C                   if        *in05
044700020912     C                   write     TB28V2
044800020212     C                   exfmt     PROTECT
044900020212x   1C                   else
045000020912     C                   exfmt     TB28V2
045100020212e   1C                   endif
045200020212     C                   eval      *in99 = *off
045300020301     C                   clear                   V2MSG
045400020301     C                   clear                   wTasto
045500020212      *
045600020212sel 1C                   select
045700020212      * F03=Fine
045800020212w   1C                   when      *inKC
045900020913     C                   exsr      F03V0
046000020212     C                   goto      EndGesV2
046100020212      * F12=Ritorno
046200020212w   1C                   when      *inKL
046300020913     C                   exsr      F12V2
046400020212     C                   goto      EndGesV2
046500020212e   1C                   endsl
046600020212      *
046700020212      * Controllo dati immessi a video
046800020212     C                   exsr      CTRV2
046900020212      *
047000020212      * Aggiornamento se non ci sono errori
047100020212if  1C                   if        not *in99
047200020212     C                             and (*inKF or *inKE or *inKQ)
047300020301sel 2C                   select
047400020301w   2C                   when      *inKE
047500020301     C                   eval      wTasto = '05'
047600020301w   2C                   when      *inKF
047700020301     C                   eval      wTasto = '06'
047800020301w   2C                   when      *inKQ
047900020301     C                   eval      wTasto = '16'
048000020301e   2C                   endsl
048100020226     C                   eval      $CarW1 = *on
048200020226     C                   eval      TipVid = 'A'
048300020212e   1C                   endif
048400020212      *
048500020212     C     EndGesV2      ENDSR
048600020212
048700020212      *---------------------------------------------------------------*
048800020212      * CARV2  - Caricamento dati seconda videata                     *
048900020212      *---------------------------------------------------------------*
049000020212     C     CARV2         BEGSR
049100020212      *
049200020212     C                   clear                   T1opz
049300020212     C                   movea     '00000'       *in(01)
049400020212      *
049500020212      * Aggancio la tabella, se trovo il codice sono in modifica
049600020212      * o ripristino (se record annullato), altrimenti in immissione
049700020212     C                   exsr      CHNTBE
049800020212      *
049900020212if  1C                   if        %found(TNTBE01L)
050000020212      *
050100020212      * MODIFICA/RIPRISTINO
050200020913     C                   movel     TBEuni        DSCH
050300020212if  2C                   if        TBEatb = *blanks
050400020212     C                   eval      *in02  = *on
050500020212     C                   eval      T1opz  = Opz(02)
050600020212x   2C                   else
050700020212     C                   eval      *in04  = *on
050800020212     C                   eval      T1opz  = Opz(06)
050900020212e   2C                   endif
051000020212      *
051100020212x   1C                   else
051200020212      *
051300020212      * IMMISSIONE
051400020913     c                   clear                   DSCH
051500020212     C                   eval      *in01  = *on
051600020212     C                   eval      T1opz  = Opz(01)
051700020212      *
051800020212e   1C                   endif
051900020212      *
052000020912     C                   move      V1AZN         V2AZN
052100021025     C                   eval      V2prio = d§SCHpri
052200021025     C                   eval      V2AZND = d§SCHdes
052300020912     C                   eval      V2ESE  = d§SCHese
052400020912     C                   eval      V2FRE  = d§SCHfre
052500020913     C                   eval      V2JOB  = d§SCHjob
052600020912     C                   eval      V2OR1  = D§SCHor1
052700020912     C                   eval      V2OR2  = D§SCHor2
052800020912     C                   eval      V2OR3  = D§SCHor3
052900020912     C                   eval      V2OR4  = D§SCHor4
053000020912     C                   eval      V2OR5  = D§SCHor5
053100020912     C                   eval      V2OR6  = D§SCHor6
053200020912     C                   eval      V2OR7  = D§SCHor7
053300020912     C                   eval      V2OR8  = D§SCHor8
053400020912     C                   eval      V2OR9  = D§SCHor9
053500020912     C                   eval      V2OR10 = D§SCHor10
053600020912     C                   eval      V2OR11 = D§SCHor11
053700020912     C                   eval      V2OR12 = D§SCHor12
053800020912     C                   eval      V2OR13 = D§SCHor13
053900020912     C                   eval      V2OR14 = D§SCHor14
054000020912     C                   eval      V2OR15 = D§SCHor15
054100020912     C                   eval      V2TPS  = D§SCHtps
054200020912     C                   eval      V2UTE  = D§SCHute
054300020212     c
054400020212     C                   ENDSR
054500020913
054600020913      *---------------------------------------------------------------*
054700020913      * F12V2  - Tasto funzionale F12 -> Ritorno                      *
054800020913      *---------------------------------------------------------------*
054900020913     C     F12V2         BEGSR
055000020913      *
055100020913     C                   eval      TipVid = '1'
055200021114     c                   Unlock    Tntbe01l
055300020913      *
055400020913     C                   ENDSR
055500020212
055600020212      *---------------------------------------------------------------*
055700020212      * CTRV2  - Controllo e decodifica seconda videata               *
055800020212      *---------------------------------------------------------------*
055900020212     C     CTRV2         BEGSR
056000020212      *
056100020913     C                   movea     *zeros        *in(50)
056200020212     C                   clear                   V2msg
056300020212      *
056400020212      * Non si fanno i controlli se richisto l'annullamento
056500020212     C     *inKQ         cabeq     *on           EndCtrV2
056600020913      *
056700020913      * Passo gli orari nella schiera
056800020913     c                   move      v2or1         ora(1)
056900020913     c                   move      v2or2         ora(2)
057000020913     c                   move      v2or3         ora(3)
057100020913     c                   move      v2or4         ora(4)
057200020913     c                   move      v2or5         ora(5)
057300020913     c                   move      v2or6         ora(6)
057400020913     c                   move      v2or7         ora(7)
057500020913     c                   move      v2or8         ora(8)
057600020913     c                   move      v2or9         ora(9)
057700020913     c                   move      v2or10        ora(10)
057800020913     c                   move      v2or11        ora(11)
057900020913     c                   move      v2or12        ora(12)
058000020913     c                   move      v2or13        ora(13)
058100020913     c                   move      v2or14        ora(14)
058200020913     c                   move      v2or15        ora(15)
058300020313      *
058400020212      * Descrizione obbligatoria
058500020913w   1C                   If        V2aznd =  *blanks
058600020913     C                   seton                                        52  99
058700020913     C                   movel     Msg(04)       V2msg
058800020913     C                   goto      EndctrV2
058900020913     c                   endif
059000020212      *
059100020912      * Flag esecuzione obbligatorio
059200020913w   1C                   If        V2ese = *blanks
059300020913     C                   seton                                        53  99
059400020913     C                   movel     Msg(05)       V2msg
059500020913     C                   goto      EndctrV2
059600020913     c                   endif
059700020913      * Non si può indicare esecuzione 'S' o 'E' in ambiente AS unico
059800020913w   1C                   If            (V2ese = 'S' or V2ese = 'E')
059900021028     c*                            and %subst(TbeSif:1:6) <> 'GAITRA'
060000021028     c*                            and %subst(TbeSif:1:6) <> 'SDITRA'
060100021028     c                             and %subst(v0sif:1:6) <> 'GAITRA'
060200021028     c                             and %subst(v0Sif:1:6) <> 'SDITRA'
060300020913     C                   seton                                        53  99
060400020913     C                   movel     Msg(06)       V2msg
060500020913     C                   goto      EndctrV2
060600020913     c                   endif
060700020212      *
060800020912      * Flag frequenza
060900020913w   1C                   If        V2fre  = *blanks
061000020913     C                   seton                                        54  99
061100020913     C                   movel     Msg(07)       V2msg
061200020913     C                   goto      EndctrV2
061300020913     c                   endif
061400021104     c* se la frequenza è settimanale obbligatoria la coda batch = KSNOTTESK
061500021025w   1C                   If        V2fre  = 'S'
061600021104     C                   eval      v2job = 'KSNOTTESK'
061700021025     c                   endif
061800020212      *
061900020912      * Flag tipo sottomissione
062000020913w   1C                   if             V2tps  = 'M'
062100020913     C                             and %subst(TbeSif:1:6) <> 'FILTRA'
062200020913     C                   seton                                        55  99
062300020913     C                   movel     Msg(08)       V2msg
062400020913     C                   goto      EndctrV2
062500020913     c                   endif
062600020215      *
062700020913      * Profilo obbligatorio
062800020913w   1C                   if        V2ute  = *blanks
062900020913     C                   seton                                        56  99
063000020913     C                   movel     Msg(09)       V2msg
063100020913     C                   goto      EndctrV2
063200020913     c                   endif
063300020918      * Deve esistere nel file profili del modulo base
063400020918     c     V2ute         Chain     Kpprf01l
063500020918     c                   if        not %Found(kpprf01l)
063600020918     c                   seton                                        56  99
063700020918     c                   movel     Msg(15)       V2msg
063800020918     c                   goto      EndCtrV2
063900020918     c                   endif
064000020913      * nel s.i. FILTRA è possibile solo EDPCEDXXX
064100020913w   1C                   if            v2ute <> 'EDPCEDXXX'
064200021028     c*                            and %subst(TbeSif:1:6) = 'FILTRA'
064300021028     c                             and %subst(v0Sif:1:6) = 'FILTRA'
064400020913     C                   seton                                        56  99
064500020913     C                   movel     Msg(10)       V2msg
064600020913     C                   goto      EndctrV2
064700020913     c                   endif
064800020913      * il profilo EDPCEDXXX è valido solo nel s.i. FILTRA
064900020913w   1C                   if            v2ute =  'EDPCEDXXX'
065000021028     c*                            and %subst(TbeSif:1:6) <> 'FILTRA'
065100021028     c                             and %subst(v0Sif:1:6) <> 'FILTRA'
065200020913     C                   seton                                        56  99
065300020913     C                   movel     Msg(11)       V2msg
065400020913     C                   goto      EndctrV2
065500020913     c                   endif
065600020913      * Per sottomissioni multiple è consentito solo l'utente EDPCEDXXX
065700020913w   1C                   if            v2ute  <> 'EDPCEDXXX'
065800020913     c                             and v2tps  = 'M'
065900020913     C                   seton                                        56  99
066000020913     C                   movel     Msg(12)       V2msg
066100020913     C                   goto      EndctrV2
066200020913     c                   endif
066300020913      *
066400021025      * Orari obbligatori solo x job giornalieri
066500020913     c                   xfoot     ora           totora
066600021025w   1c                   if        Totora = *zeros and
066700021025w   1c                             v2fre = 'G'
066800020913     C                   seton                                        60  99
066900020913     C                   movel     Msg(13)       V2msg
067000020913     C                   goto      EndctrV2
067100021025     c                   endif
067200021025w   1c                   if        Totora <> *zeros and
067300021025w   1c                             v2fre = 'S'
067400021025     C                   seton                                        60  99
067500021025     C                   movel     Msg(16)       V2msg
067600021025     C                   goto      EndctrV2
067700021025     c                   endif
067800020913      * controllo l'ora
067900020913     c                   do        15            xx
068000020913     c                   z-add     59            yy
068100020913     c                   movel     ora(xx)       Wora
068200020913     c                   move      ora(xx)       Wminuti
068300020913w   1C                   if        Wora  >= 24 or Wminuti >= 60
068400020913     C                   seton                                            99
068500020913     c                   add       xx            yy
068600020913     C                   eval      *in(yy) = *On
068700020913     C                   movel     Msg(14)       V2msg
068800020913     C                   goto      EndctrV2
068900020913e   1C                   endif
069000020913     c                   enddo
069100020212      *
069200020214     C     *in99         cabeq     *on           EndCtrV2
069300020212      *
069400020212     C     EndCtrV2      ENDSR
069500020212
069600020226      *---------------------------------------------------------------*
069700020226      * GESW1  - Gestione videata dati relativi alla trasmissione     *
069800020226      *---------------------------------------------------------------*
069900020226     C     GESW1         BEGSR
070000020226      *
070100020226      * Inizializzazione videata
070200020226if  1C                   if        $CarW1 = *on
070300020226     C                   exsr      CARW1
070400020226     C                   movel     *off          $CarW1
070500020226e   1C                   endif
070600020226      *
070700020226if  1C                   if        *in05
070800020912     C                   write     TB28W1
070900020226     C                   exfmt     PROTECT
071000020226x   1C                   else
071100020912     C                   exfmt     TB28W1
071200020226e   1C                   endif
071300020226     C                   eval      *in99 = *off
071400020301     C                   clear                   W1MSG
071500020226      *
071600020226sel 1C                   select
071700020226      * F12=Ritorno
071800020226w   1C                   when      *inKL
071900020226     C                   exsr      F12W1
072000020226     C                   goto      EndGesW1
072100020226e   1C                   endsl
072200020226      *
072300020226      * Controllo dati immessi a video
072400020226     C                   exsr      CTRW1
072500020226      *
072600020226      * Aggiornamento se non ci sono errori
072700020301if  1C                   if        not *in99 and *inKF
072800020226     C                   exsr      AGGTBE
072900020226e   1C                   endif
073000020226      *
073100020226     C     EndGesW1      ENDSR
073200020313
073300020226      *---------------------------------------------------------------*
073400020226      * CARW1  - Caricamento dati window                              *
073500020226      *---------------------------------------------------------------*
073600020226     C     CARW1         BEGSR
073700020226      *
073800020226     C                   movea     *zeros        *in(50)
073900020226      *
074000020226sel 1C                   select
074100020226      *
074200020226      * F5=Ripristino
074300020226w   1C                   when      *inKE   and  *in04
074400020226     C                   eval      W1FTT = TBEftt
074500020226      *
074600020226      * F6=Conferma
074700020226w   1C                   when      *inKF
074800020226sel 2C                   select
074900020226      *   Immissione
075000020226w   2C                   when      *in01
075100020313     C                   eval      W1FTT = TBXftt
075200020226      *   Modifica / Ripristino
075300020226w   2C                   when      *in02   or    *in04
075400020313     C                   eval      W1FTT = TBEftt
075500020226e   2C                   endsl
075600020226      *
075700020226      * F16=Annullamento
075800020226w   1C                   when      *inKQ   and  not *in04
075900020313     C                   eval      W1FTT = TBEftt
076000020226      *
076100020226e   1C                   endsl
076200020226      *
076300020226      * Se NON immissione: visualizzo i dati relativi all'ultima
076400020226      *   trasmissione
076500020226if  1C                   if        not *in01
076600020226     C                   eval      W1FLT = TBEflt
076700020226     C                   eval      W1FTR = TBEftr
076800020226if  2C                   if        TBEdtr <> 0
076900020226     C                   clear                   WLBDAT
077000020226     C                   z-add     TBEdtr        G02inv
077100020226     C                   movel     '3'           G02err
077200020226     C                   call      'XSRDA8'
077300020226     C                   parm                    WLBDAT
077400020226     C                   z-add     G02dat        W1DTR
077500020226e   2C                   endif
077600020226e   1C                   endif
077700020226      *
077800020226     C                   ENDSR
077900020226
078000020226      *---------------------------------------------------------------*
078100020226      * CTRW1  - Controllo e decodifica window                        *
078200020226      *---------------------------------------------------------------*
078300020226     C     CTRW1         BEGSR
078400020226      *
078500020226     C                   movea     *zeros        *in(50)
078600020226      *
078700020226     C     EndCtrW1      ENDSR
078800020226
078900020226      *---------------------------------------------------------------*
079000020226      * F21W1  - Tasto funzionale F12 -> Ritorno                      *
079100020226      *---------------------------------------------------------------*
079200020226     C     F12W1         BEGSR
079300020226      *
079400020226     C                   eval      TipVid = '2'
079500020226      *
079600020226     C                   ENDSR
079700020212
079800020212      *---------------------------------------------------------------*
079900020212      * CHNTBE * Aggancio tabella                                     *
080000020212      *---------------------------------------------------------------*
080100020212     C     CHNTBE        BEGSR
080200020212      *
080300020212     C                   movel     T02cod        TBEcod
080400020912     C                   movel(p)  V1AZN         TBEke1
080500020212     C                   clear                   TBEke2
080600020212     C                   clear                   TBElin
080700020913     C                   movel     tbxsif        TBEsif
080800020212     C     K05TBE01      chain     TNTBE01L
080900020212      * Se non ho trovato il record con il sistema informativo
081000020212      * che ho in linea, lo abblenco
081100020212if  1C                   if        not %found(TNTBE01L)
081200020212     C                   clear                   TBEsif
081300020212     C     K05TBE01      chain     TNTBE01L
081400020212e   1C                   endif
081500020212      *
081600020212     C                   ENDSR
081700020212
081800020212      *---------------------------------------------------------------*
081900020212      * AGGTBE * Aggiornamento tabella                                *
082000020212      *---------------------------------------------------------------*
082100020212     C     AGGTBE        BEGSR
082200020212      *
082300020212sel 1C                   select
082400020212      *
082500020212      * F5=Ripristino
082600020301w   1C                   when      wTasto='05'  and  *in04
082700020212     C                   clear                   TBEatb
082800020212     C                   clear                   TBEftr
082900020212     C                   UPDATE    TNTBE000
083000020212      *
083100020212      * F6=Conferma
083200020301w   1C                   when      wTasto='06'
083300020212sel 2C                   select
083400020212      *   Immissione
083500020212w   2C                   when      *in01
083600020212     C                   exsr      RIETBE
083700020226     C                   clear                   TBEflt
083800020226     C                   clear                   TBEdtr
083900020212     C                   WRITE     TNTBE000                             22
084000020212      *   Modifica / Ripristino
084100020212w   2C                   when      *in02   or    *in04
084200020212     C                   exsr      RIETBE
084300020212     C                   UPDATE    TNTBE000
084400020212e   2C                   endsl
084500020212      *
084600020212      * F16=Annullamento
084700020301w   1C                   when      wTasto='16'  and  not *in04
084800020212     C                   movel     'A'           TBEatb
084900020212     C                   clear                   TBEftr
085000020212     C                   UPDATE    TNTBE000
085100020212      *
085200020212e   1C                   endsl
085300020212      *
085400020212      * Torno alla prima videata che carico come da inizio pgm
085500020212     C                   movel     '1'           TipVid
085600020212     C                   movel     *on           $CarV1
085700020226     C                   movel     *on           $CarV2
085800020226     C                   movel     *on           $CarW1
085900020212     C
086000020212     C                   ENDSR
086100020212
086200020212      *---------------------------------------------------------------*
086300020212      * Riempimento file tabella
086400020212      *---------------------------------------------------------------*
086500020212     C     RIETBE        BEGSR
086600020212      *
086700020212     C                   clear                   TBEatb
086800020212     C                   if        TBXsif <> *blanks
086900020912     C                   movel     TBXsif        TBEsif
087000020212     C                   else
087100020212     C                   clear                   TBEsif
087200020212     C                   endif
087300020212     C                   movel     TBXapl        TBEapl
087400020212     C                   movel     T02cod        TBEcod
087500020913     C                   movel     V2azn         TBEke1
087600020212     C                   clear                   TBEke2
087700020212      *
087800020913     C                   clear                   DSCH
087900021025     C                   eval      d§SCHpri  =  V2prio
088000021025     C                   eval      d§SCHdes  =  V2AZND
088100020913     C                   eval      d§SCHese  =  V2ESE
088200020913     C                   eval      d§SCHfre  =  V2FRE
088300020913     C                   eval      d§SCHjob  =  V2JOB
088400020913     C                   eval      D§SCHor1  =  V2OR1
088500020913     C                   eval      D§SCHor2  =  V2OR2
088600020913     C                   eval      D§SCHor3  =  V2OR3
088700020913     C                   eval      D§SCHor4  =  V2OR4
088800020913     C                   eval      D§SCHor5  =  V2OR5
088900020913     C                   eval      D§SCHor6  =  V2OR6
089000020913     C                   eval      D§SCHor7  =  V2OR7
089100020913     C                   eval      D§SCHor8  =  V2OR8
089200020913     C                   eval      D§SCHor9  =  V2OR9
089300020913     C                   eval      D§SCHor10 =  V2OR10
089400020913     C                   eval      D§SCHor11 =  V2OR11
089500020913     C                   eval      D§SCHor12 =  V2OR12
089600020913     C                   eval      D§SCHor13 =  V2OR13
089700020913     C                   eval      D§SCHor14 =  V2OR14
089800020913     C                   eval      D§SCHor15 =  V2OR15
089900020913     C                   eval      D§SCHtps  =  V2TPS
090000020913     C                   eval      D§SCHute  =  V2UTE
090100020913     C                   movel(p)  DSCH          TBEuni
090200020212      *
090300020226     C                   movel     W1FTT         TBEftt
090400020212     C                   z-add     TBXflt        TBEflt
090500020212     C                   clear                   TBEftr
090600020212     C
090700020212     C                   ENDSR
090800020212
090900020212** -OPZ-      *
091000020212  Inserimento
091100020212    Modifica
091200020212     Copia
091300020212  Annullamento
091400020212Visualizzazione
091500020212   ANNULLATO
091600020212** -MSG-                                                                     *
091700020913Immettere il sistema informativo                                               01
091800020913Sistema informativo errato                                                     02
091900020913Immettere l'azione                                                             03
092000020913Immettere la descrizione                                                       04
092100020913Immettere il flag "da eseguire"                                                05
092200020913Azione da eseguire in sede possibile solo x s.i. GAITRA o SDITRA               06
092300020913Immettere la frequenza                                                         07
092400020913Tipo sottomissione multipla possibile solo x s.i. FILTRA                       08
092500020913Immettere l'utente                                                             09
092600020913Nel s.i. FILTRA l'utente deve essere EDPCEDXXX                                 10
092700020913Utente EDPCEDXXX valido solo x s.i. FILTRA                                     11
092800020913Per sottomissioni multiple l'utente deve essere EDPCEDXXX                      12
092900020913Immettere almeno un orario                                                     13
093000020913Orario errato                                                                  14
093100020918Utente errato - non esiste sul modulo base                                     15
093200021025Per lavori settimanali non bisogna impostare orari                             16
