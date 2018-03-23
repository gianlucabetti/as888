000100030805      *---------------------------------------------------------------*
000200030805      * Gestione tabella "OSR" = Serie O.R.M.                         *
000300030805      *---------------------------------------------------------------*
000400030805
000500030805     h decedit('0,') datedit(*dmy.)
000600030805
000700030805      *---------------------------------------------------------------*
000800030805      *   A R C H I V I                                               *
000900030805      *---------------------------------------------------------------*
001000030805      *
001100030805     fAZORG01L  if   e           k disk
001200030805     fTABEL00F  if   e           k disk
001300030805      *
001400030805     fTNTBE01L  uf a e           k disk
001500030805      *
001600030805     fTNTB30D   cf   e             workstn
001700030805
001800030805      *---------------------------------------------------------------*
001900030805      *   C O S T A N T I                                             *
002000030805      *---------------------------------------------------------------*
002100030805      *
002200030805     d DigitN          c                   const('0123456789')
002300030805
002400030805      *---------------------------------------------------------------*
002500030805      *   S C H I E R E                                               *
002600030805      *---------------------------------------------------------------*
002700030805      *
002800030806     d $Opz            s             15    dim(06) ctdata perrcd(1)             Decodifica OPZ
002900030806     d $Msg            s             78    dim(07) ctdata perrcd(1)             Messaggi video
003000030805     d $Fld            s             10    dim(10)                              Campi per ricerca
003100030805
003200030805      *---------------------------------------------------------------*
003300030805      *   S T R U T T U R E   D A T I                                 *
003400030805      *---------------------------------------------------------------*
003500030805      *
003600030805      * Parametri
003700030805     d KPJBA         e ds
003800030805      *
003900030805      * Passaggio Parametri al pgm TIBS02R
004000030805     d TIBS02DS      e ds                  inz
004100030805     d  T02mod       e                     inz('R')
004200030805     d  T02cod       e                     inz('OSR')
004300030805      *
004400030805      * Tabella OSR = Serie O.R.M.
004500030805     d dOSR          e ds                  inz
004600030805      *
004700030807      * Tabella 3A = Codici bolla
004800030807     d ds3A          e ds                  inz
004900030805      *
005000030805      * Parametri per interrogazione serie O.R.M.
005100030805     d TNTB301ds     e ds                  inz
005200030805     d  B30err       e                     inz(*off)
005300030805      *
005400030805      * Tracciato record file TNTBE00F
005500030805     d TNTBEds       e ds                  extname(TNTBE00F) inz
005600030805     d xTNTBEds      e ds                  extname(TNTBE00F) inz
005700030805     d                                     prefix(TBX:3)
005800030805      *
005900030805     d TIBS34DS      e ds                  inz
006000030805     d DDATIUTE      e ds                  inz
006100030805     d AZUTEDS       e ds                  extname(AZUTE00F) inz
006200030805      *
006300030805     d TIBS69DS      e ds                  inz
006400030805     d DS_ACO        e ds                  extname(CNACO00F) inz
006500030805     d DS_IND        e ds                  extname(CNIND00F) inz
006600030805     d DS_CLP        e ds                  extname(CNCLP00F) inz
006700030805     d DS_CLS        e ds                  extname(FNCLS00F) inz
006800030805      *
006900030805     d WLBDAT          ds                  inz
007000030805     d  G02DAT                 1      8  0 inz
007100030805     d  G02INV                 9     16  0 inz
007200030805     d  G02ERR                17     17    inz
007300030805     d  G02TGI                18     22  0 inz
007400030805      *
007500030805     d DSkeyOSR        ds             5    inz
007600030805     d  V1Cpoe
007700030805     d  V1Cnrs
007800030805      *
007900030805     d                sds
008000030805     d  VTCpgm           *proc
008100030805
008200030805      *---------------------------------------------------------------*
008300030805      *   V A R I A B I L I                                           *
008400030805      *---------------------------------------------------------------*
008500030805      *
008600030805     d XX              s              2  0 inz
008700030805     d $Err            s              1a   inz(*off)
008800030805     d $Fine           s              1a   inz(*off)
008900030805     d $CarV1          s              1a   inz(*on)
009000030805     d $CarV2          s              1a   inz(*off)
009100030805     d $CarW1          s              1a   inz(*off)
009200030805     d $TipVid         s              1a   inz('1')
009300030805     d $Tibs02         s              1a   inz(*off)
009400030805     d Win             s             99a   inz(*zeros)
009500030805     d wTasto          s              2a   inz(*zeros)
009600030805     d SAVpoe          s                   like(V1Cpoe) inz
009700030805     d CodUt           s              1  0 inz(1)
009800030805
009900030805      *---------------------------------------------------------------*
010000030805      *   M A I N   L I N E                                           *
010100030805      *---------------------------------------------------------------*
010200030805      *  Riepilogo indicatori utilizzati:                             *
010300030805      *  --------------------------------                             *
010400030805      *  01 - Record inesistente (inserimento)                        *
010500030805      *  02 - Record esistente   (modifica)                           *
010600030805      *  04 - Record annullato   (ripristino)                         *
010700030805      *  20 - Comodo                                                  *
010800030805      *  22 - Errori in scrittura record (WRITE)                      *
010900030805      *---------------------------------------------------------------*
011000030805      *
011100030805      * Operazioni iniziali
011200030805     c                   exsr      RutInz
011300030805      *
011400030805      * Gestione video
011500030805     c                   dow       $Fine = *off
011600030805     c     $TipVid       caseq     '1'           GesV1
011700030805     c     $TipVid       caseq     '2'           GesV2
011800030805     c     $TipVid       caseq     'A'           GesW1
011900030805     c                   endcs
012000030805     c                   enddo
012100030805      *
012200030805      * Fine
012300030805     c                   if        $Tibs02 = *on
012400030805     c                   clear                   TIBS02ds
012500030805     c                   movel     'C'           T02tla
012600030805     c                   call      'TIBS02R'
012700030805     c                   parm                    KPJBA
012800030805     c                   parm                    TIBS02ds
012900030805     c                   endif
013000030805      *
013100030805     c                   movel     *on           *inLR
013200030805
013300030805      *---------------------------------------------------------------*
013400030805      * RutInz - Operazioni Iniziali                                  *
013500030805      *---------------------------------------------------------------*
013600030805     c     RutInz        BEGSR
013700030805      *
013800030805      * Ricezione parametri
013900030805     c     *entry        plist
014000030805     c                   parm                    KPJBA
014100030805      *
014200030805      * Definizioni chiavi di accesso
014300030805     c     K05TBE01      klist                                                  *tntbe01l
014400030805     c                   kfld                    TBEcod                         -tabella
014500030805     c                   kfld                    TBEke1                         -chiave uno
014600030805     c                   kfld                    TBEke2                         -chiave due
014700030805     c                   kfld                    TBElin                         -lingua
014800030805     c                   kfld                    TBEsif                         -s.informativo
014900030805      *
015000030805     c     K03TAB00      klist
015100030805     c                   kfld                    CodUt
015200030805     c                   kfld                    TBLcod
015300030805     c                   kfld                    TBLkey
015400030805      *
015500030805      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
015600030805     c     *dtaara       define    §azute        azuteds
015700030805     c     *dtaara       define    §datiute      ddatiute
015800030805      *
015900030805     c                   clear                   AzUteDs
016000030805     c                   clear                   DDatiUte
016100030805     c                   clear                   Tibs34Ds
016200030805     c                   in(E)     *dtaara
016300030805if  1c                   if        %Error  or  RSUT = *blanks
016400030805     c                   call      'TIBS34R'
016500030805     c                   parm                    Tibs34Ds
016600030805     c                   in        *dtaara
016700030805e   1c                   endif
016800030805      *-- Verifica errori e autorità profilo
016900030805sel 1c                   SELECT
017000030805      *-- controllo se ho errori nei dati utente
017100030805      *--   nel qual caso NON risulta un profilo abilitato
017200030805w   1c                   WHEN      DUTerr = 'E'
017300030805     c                   eval      $Fine  = *on
017400030805      *
017500030805      *-- CONTROLLO AUTORITA'
017600030805      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
017700030805      *-- se il chiamante non richiede autorità specifica verificare
017800030805      *--   quella del profilo
017900030805      *-- se il chiamante richiede autorità specifica verificarla,
018000030805      *--  se è blank verificare quella del profilo
018100030805      *
018200030805      * se UTEAUT = *BLANK non siamo in sede
018300030805w   1c                   WHEN      UTEaut = *blank
018400030805      *
018500030805x   1c                   OTHER
018600030805      *
018700030805e   1c                   ENDSL
018800030805      *
018900030805      * Aggancio dati generali della tabella in esame
019000030805     c                   clear                   TBEcod
019100030805     c                   move      *zeros        TBEke1
019200030805     c                   move      T02cod        TBEke1
019300030805     c                   clear                   TBEke2
019400030805     c                   clear                   TBElin
019500030805     c                   movel     KNSIF         TBEsif
019600030805     c     K05TBE01      chain     TNTBE01L
019700030805     c                   if        not %found(TNTBE01L)
019800030805     c                   clear                   TBEsif
019900030805     c     K05TBE01      chain     TNTBE01L
020000030805     c                   endif
020100030805     c                   if        %found(TNTBE01L)
020200030805     c                   movel     TNTBEds       xTNTBEds
020300030805     c                   else
020400030805     c                   clear                   xTNTBEds
020500030805     c                   endif
020600030808      *
020700030808     c                   clear                   TB30V1
020800030808      *
020900030808      * Imposto il P.O. emissione ORM di default
021000030808     c                   clear                   V1Cpoe
021100030805      *
021200030805     c                   ENDSR
021300030805
021400030805      *---------------------------------------------------------------*
021500030805      * GESV1  - Gestione videata selezione codice tabella            *
021600030805      *---------------------------------------------------------------*
021700030805     c     GesV1         BEGSR
021800030805      *
021900030805      * Inizializzazione videata
022000030805if  1c                   if        $CarV1 = *on
022100030805     c                   exsr      CarV1
022200030805     c                   movel     *off          $CarV1
022300030805e   1c                   endif
022400030805      *
022500030805      * Scrivo la testata
022600030805     c                   clear                   T1opz
022700030805     c                   write     TB30T1
022800030805      *
022900030805      * Se esistono errori sulla videata
023000030805      * emetto la write del formato a indicatori spenti per vedere
023100030805      * le eventuali decodifiche
023200030805if  1c                   if        *in99
023300030805     c                   movea     *in           Win
023400030805     c                   movea     *zeros        *in(50)
023500030805     c                   write     TB30V1
023600030805     c                   movea     Win           *in
023700030805e   1c                   endif
023800030805      *
023900030805     c                   exfmt     TB30V1
024000030805     c                   eval      *in99 = *off
024100030805     c                   clear                   V1Dmsg
024200030805      * F3=Fine
024300030805if  1c                   if        *inKC
024400030805     c                   exsr      F03V1
024500030805     c                   goto      EndGesV1
024600030805e   1c                   endif
024700030805      *
024800030805      * Controllo dati immessi a video
024900030805     c                   exsr      CtrV1
025000030805     c     *in99         cabeq     *on           EndGesV1
025100030805      *
025200030805      * Passaggio alla videata di dettaglio
025300030805if  1c                   if        not *in99
025400030805     c                   eval      $CarV2 = *on
025500030805     c                   eval      $TipVid = '2'
025600030805e   1c                   endif
025700030805      *
025800030805     c     EndGesV1      ENDSR
025900030805
026000030805      *---------------------------------------------------------------*
026100030805      * CARV1  - Caricamento dati prima videata                       *
026200030805      *---------------------------------------------------------------*
026300030805     c     CarV1         BEGSR
026400030805      *
026500030805     c                   movea     *zeros        *in(50)
026600030805     c                   movea     '00000'       *in(01)
026700030805      *
026800030808      * Imposto il campo "numero serie" per l'interrogazione
026900030808      *   (ma lascio il cursore posizionato sul P.O.)
027000030808     c                   move (p)  '?'           V1Cnrs
027100030805      *
027200030805     c                   ENDSR
027300030805
027400030805      *---------------------------------------------------------------*
027500030805      * CTRV1  - Controllo e decodifica prima videata                 *
027600030805      *---------------------------------------------------------------*
027700030805     c     CtrV1         BEGSR
027800030805      *
027900030805     c                   movea     *zeros        *in(50)
028000030805     c                   clear                   V1Dmsg
028100030808     c                   clear                   xx
028200030808     c                   clear                   $Fld
028300030808      *
028400030808      * Controlli / Ricerche
028500030808      *
028600030808      * Controllo:
028700030808      * - P.O. emissione O.R.M. obbligatorio e obbligatoriamente valido
028800030808if  1c                   if        NOT  *inKJ
028900030808     c                   exsr      CtrPO
029000030808     c   99              goto      EndCtrV1
029100030808e   1c                   endif
029200030806      *
029300030808      * Ricerca:
029400030806      * - Numero Serie
029500030806     c     '?'           scan      V1Cnrs                                 20
029600030808if  1c                   if        *in20   or   *inKJ
029700030806     c                   clear                   V1Cnrs
029800030806     c                   add       1             XX
029900030808     c                   eval      $Fld(xx)  = 'V1CNRS    '
030000030806e   1c                   endif
030100030806if  1c                   if        xx > *zeros
030200030806     c                   exsr      Search
030300030806e   1c                   endif
030400030808      * Controllo:
030500030806      * - Numero serie
030600030808if  1c                   if           V1Cnrs = *blanks
030700030808     c                             or V1Cnrs = *zeros
030800030805     c                   seton                                        51  99
030900030808     c                   eval      V1Dmsg    = $Msg(03)
031000030805     c                   goto      EndCtrV1
031100030806x   1c                   else
031200030806     c     DigitN        check     V1Cnrs                                 20
031300030808if  2c                   if           *in20
031400030808     c                             or V1Cnrs = *all'9'
031500030806     c                   seton                                        52  99
031600030806     c                   movel     $Msg(04)      V1Dmsg
031700030806     c                   goto      EndCtrV1
031800030806e   2c                   endif
031900030806e   1c                   endif
032000030805      *
032100030805     c     EndCtrV1      ENDSR
032200030805
032300030805      *---------------------------------------------------------------*
032400030805      * F03V1  - Tasto funzionale F03 -> Fine programma               *
032500030805      *---------------------------------------------------------------*
032600030805     c     F03V1         BEGSR
032700030805      *
032800030805     c                   movel     *on           $Fine                          fine pgm
032900030805      *
033000030805     c                   ENDSR
033100030805
033200030805      *---------------------------------------------------------------*
033300030805      * CTRPO  - Controllo Punto Operativo                            *
033400030805      *---------------------------------------------------------------*
033500030805     c     CtrPO         BEGSR
033600030805      *
033700030805     c                   clear                   V1Dpoe
033800030805      *
033900030805if  1c                   if        V1Cpoe = *zeros
034000030805     c                   seton                                        50  99
034100030805     c                   eval      V1Dmsg = $Msg(01)
034200030805     c                   goto      EndCtrPO
034300030805e   1c                   endif
034400030805      *
034500030805     c     V1Cpoe        chain     AZORG01L
034600030805if  1c                   if        not %found(AZORG01L)
034700030805     c                             or  ORGfva =  '*'
034800030805     c                   seton                                        50  99
034900030805     c                   eval      V1Dmsg = $Msg(01)
035000030805     c                   goto      EndCtrPO
035100030805x   1c                   else
035200030805     c                   movel     V1Cpoe        SAVpoe
035300030805     c                   movel     ORGdes        V1Dpoe
035400030805e   1c                   endif
035500030805      *
035600030805     c     EndCtrPO      ENDSR
035700030805
035800030805      *---------------------------------------------------------------*
035900030805      * SEARCH - Ricerche                                             *
036000030805      *---------------------------------------------------------------*
036100030805     c     Search        BEGSR
036200030805      *
036300030805     c                   eval      *in99    =  *on
036400030805      *
036500030805do  1c                   DO        10            xx
036600030805      *
036700030805if  2c                   if        $Fld(xx) =  *blanks
036800030805     c                   leave
036900030805e   2c                   endif
037000030805      *
037100030805sel 2c                   SELECT
037200030805      * Numero Serie
037300030805w   2c                   WHEN      $Fld(xx) =  'V1CNRS    '
037400030805     c                   clear                   TNTB301ds
037500030806if  3c                   if        *inKJ  = *off
037600030805     c                   eval      B30opz = '1'
037700030806x   3c                   else
037800030806     c                   eval      B30opz = '2'
037900030806e   3c                   endif
038000030805     c                   eval      B30ke1 = DSkeyOSR
038100030805     c                   eval      B30sif = KNSIF
038200030805     c                   movel(p)  TNTB301ds     KPJBU
038300030805     c                   call      'TNTB301R'
038400030805     c                   parm                    KPJBA
038500030805     c                   movel     KPJBU         TNTB301ds
038600030806if  3c                   if            B30fxx <> 'F03'
038700030805     c                             and B30fxx <> 'F12'
038800030806     c                             and B30err =  *blanks
038900030805     c                   movel     B30ke1        DSkeyOSR
039000030806e   3c                   endif
039100030805      *
039200030805      * Codice cliente
039300030805w   2c                   WHEN      $Fld(xx) =  'V2CCLI    '
039400030805     c                   movel(p)  V2Drag        PAXdmt
039500030805     c                   call      'XALFA3BR'
039600030805     c                   parm      RSut          PAXdut           30
039700030805     c                   parm                    CODut
039800030805     c                   parm                    PAXdmt           48
039900030805     c                   parm      DUTkci        PAXccc            4 0
040000030805     c                   parm      9             PAXsta            1 0
040100030805     c                   parm      *blanks       PAXflr           90
040200030805     c                   parm      *blanks       PAXdit            3
040300030805     c                   parm      01            PAXnum            2 0
040400030805     c                   parm                    PAXkcm           80
040500030805     c                   parm                    PAXksm          140
040600030805     c                   parm                    PAXldm           60
040700030805if  3c                   if        PAXsta >= *zeros
040800030805     c                   movel     PAXksm        V2Ccli
040900030805e   3c                   endif
041000030805      *
041100030805      * Codice bolla da forzare in bollettazione
041200030805w   2c                   WHEN      $Fld(xx) =  'V2CCBO    '
041300030805     c                   call      'X§TABER'
041400030805     c                   parm                    CODut
041500030807     c                   parm      '3A'          X§TABcod          2
041600030805     c                   parm      *blanks       X§TABkey          8
041700030805     c                   parm      *blanks       X§TABdes         30
041800030805     c                   movel     X§TABkey      V2Ccbo
041900030805     c                   movel     X§TABdes      V2Dcbo
042000030805      *
042100030805e   2c                   ENDSL
042200030805      *
042300030805e   1c                   ENDDO
042400030805      *
042500030805     c                   ENDSR
042600030805
042700030805      *---------------------------------------------------------------*
042800030805      * GESV2  - Gestione videata dettaglio dati                      *
042900030805      *---------------------------------------------------------------*
043000030805     c     GesV2         BEGSR
043100030805      *
043200030805      * Inizializzazione videata
043300030805     c                   if        $CarV2 = *on
043400030805     c                   exsr      CarV2
043500030805     c                   move      *off          $CarV2
043600030805     c                   endif
043700030805      * Scrivo la testata
043800030805     c                   write     TB30T1
043900030805      *
044000030805      * Se esistono errori sulla videata
044100030805      * emetto la write del formato a indicatori spenti per vedere
044200030805      * le eventuali decodifiche
044300030805     c                   if        *in99
044400030805     c                   movea     *in           Win
044500030805     c                   movea     *zeros        *in(50)
044600030805     c                   write     TB30V2
044700030805     c                   movea     Win           *in
044800030805     c                   endif
044900030805      *
045000030805if  1c                   if        *in05
045100030805     c                   write     TB30V2
045200030805     c                   exfmt     PROTECT
045300030805x   1c                   else
045400030805     c                   exfmt     TB30V2
045500030805e   1c                   endif
045600030805     c                   eval      *in99 = *off
045700030805     c                   clear                   V1Dmsg
045800030805     c                   clear                   wTasto
045900030805      *
046000030805sel 1c                   select
046100030805      * F03=Fine
046200030805w   1c                   when      *inKC
046300030805     c                   exsr      F03V1
046400030805     c                   goto      EndGesV2
046500030805      * F12=Ritorno
046600030805w   1c                   when      *inKL
046700030805     c                   exsr      F12V2
046800030805     c                   goto      EndGesV2
046900030805e   1c                   endsl
047000030805      *
047100030805      * Controllo dati immessi a video
047200030805      * (non si fanno se richisto l'annullamento)
047300030805if  1c                   if        not *inKQ
047400030805     c                   exsr      CtrV2
047500030805e   1c                   endif
047600030805      *
047700030805      * Aggiornamento se non ci sono errori
047800030805if  1c                   if        not *in99
047900030805     c                             and (*inKF or *inKE or *inKQ)
048000030805sel 2c                   select
048100030805w   2c                   when      *inKE
048200030805     c                   eval      wTasto = '05'
048300030805w   2c                   when      *inKF
048400030805     c                   eval      wTasto = '06'
048500030805w   2c                   when      *inKQ
048600030805     c                   eval      wTasto = '16'
048700030805e   2c                   endsl
048800030805     c                   eval      $CarW1 = *on
048900030805     c                   eval      $TipVid = 'A'
049000030805e   1c                   endif
049100030805      *
049200030805     c     EndGesV2      ENDSR
049300030805
049400030805      *---------------------------------------------------------------*
049500030805      * CARV2  - Caricamento dati seconda videata                     *
049600030805      *---------------------------------------------------------------*
049700030805     c     CarV2         BEGSR
049800030805      *
049900030805     c                   clear                   T1opz
050000030805     c                   movea     '00000'       *in(01)
050100030805      *
050200030805      * Aggancio la tabella, se trovo il codice sono in modifica
050300030805      * o ripristino (se record annullato), altrimenti in immissione
050400030805     c                   exsr      ChnTBE
050500030805      *
050600030805if  1c                   if        %found(TNTBE01L)
050700030805      *
050800030805      * MODIFICA/RIPRISTINO
050900030805     c                   movel     TBEuni        dOSR
051000030805if  2c                   if        TBEatb = *blanks
051100030805     c                   eval      *in02  = *on
051200030805     c                   eval      T1opz  = $Opz(02)
051300030805x   2c                   else
051400030805     c                   eval      *in04  = *on
051500030805     c                   eval      T1opz  = $Opz(06)
051600030805e   2c                   endif
051700030805      *
051800030805x   1c                   else
051900030805      *
052000030805      * IMMISSIONE
052100030805     c                   clear                   dOSR
052200030805     c                   eval      *in01  = *on
052300030805     c                   eval      T1opz  = $Opz(01)
052400030805      *
052500030805e   1c                   endif
052600030805      *
052700030805     c                   eval      V2Cpoe = V1Cpoe
052800030805     c                   eval      V2Dpoe = V1Dpoe
052900030806     c                   move      V1Cnrs        V2Cnrs
053000030805     c                   move      D§OSRcli      V2Ccli
053100030805     c                   eval      V2Drag = D§OSRrag
053200030805     c                   eval      V2Ccbo = D§OSRcbo
053300030805     c                   eval      V2Dcbo = *blanks
053400030805     c                   eval      V2Cman = D§OSRman
053500030805     c                   eval      V2Ccpy = D§OSRcpy
053600030805     c                   eval      V2Cstp = D§OSRstp
053700030805     c                   eval      V2Cass = D§OSRass
053800031002     c                   eval      V2Csta = D§OSRsta
053900030805      *
054000030805     c                   ENDSR
054100030805
054200030805      *---------------------------------------------------------------*
054300030805      * F12V2  - Tasto funzionale F12 -> Ritorno                      *
054400030805      *---------------------------------------------------------------*
054500030805     c     F12V2         BEGSR
054600030805      *
054700030805     c                   eval      $TipVid = '1'
054800030805      *
054900030805     c                   ENDSR
055000030805
055100030805      *---------------------------------------------------------------*
055200030805      * CTRV2  - Controllo e decodifica seconda videata               *
055300030805      *---------------------------------------------------------------*
055400030805     c     CtrV2         BEGSR
055500030805      *
055600030805     c                   movea     *zeros        *in(50)
055700030805     c                   clear                   V1Dmsg
055800030805      *
055900030805      * Ricerche
056000030805     c                   clear                   xx
056100030805     c                   clear                   $Fld
056200030805      * - Codice cliente
056300030805     c     '?'           scan      V2Ccli                                 20
056400030805if  1c                   if        *in20
056500030805     c                   add       1             XX
056600030805     c                   eval      $Fld(xx) = 'V2CCLI    '
056700030805e   1c                   endif
056800030805      * - Codice bolla da forzare
056900030805     c     '?'           scan      V2Ccbo                                 20
057000030805if  1c                   if        *in20
057100030805     c                   add       1             XX
057200030805     c                   eval      $Fld(xx) = 'V2CCBO    '
057300030805e   1c                   endif
057400030805      *
057500030805if  1c                   if        xx > *zeros
057600030805     c                   exsr      Search
057700030805e   1c                   endif
057800030805      *
057900030805      * Controlli
058000030805      *
058100030805      * - Codice cliente obbligatorio e obbligatoriamente valido
058200030805     c                   clear                   V2Drag
058300030805if  1c                   IF           V2Ccli = *blanks
058400030805     c                             or V2Ccli = *zeros
058500030805     c                   seton                                        52  99
058600030806     c                   movel     $Msg(05)      V1Dmsg
058700030805     c                   goto      EndCtrV2
058800030805x   1c                   ELSE
058900030806     c     DigitN        check     V2Ccli                                 20
059000030806if  2c                   if        *in20
059100030806     c                   seton                                        52  99
059200030806     c                   movel     $Msg(06)      V1Dmsg
059300030806     c                   goto      EndCtrV2
059400030806e   2c                   endif
059500030805     c                   clear                   TIBS69ds
059600030805     c                   clear                   DS_ACO
059700030805     c                   clear                   DS_IND
059800030805     c                   clear                   DS_CLP
059900030805     c                   clear                   DS_CLS
060000030805     c                   eval      I69kcc = DUTkci
060100030805     c                   move      V2Ccli        I69kac
060200030805     c                   eval      I69sif = KNSIF
060300030805     c                   call      'TIBS69R'
060400030805     c                   parm                    TIBS69ds
060500030805     c                   parm                    DS_ACO
060600030805     c                   parm                    DS_IND
060700030805     c                   parm                    DS_CLP
060800030805     c                   parm                    DS_CLS
060900030805if  2c                   if        O69err <> *blanks
061000030805     c                   seton                                        52  99
061100030806     c                   movel     $Msg(06)      V1Dmsg
061200030805     c                   goto      EndCtrV2
061300030805e   2c                   endif
061400030805e   1c                   ENDIF
061500030805      *
061600030805      * Decodifica cliente
061700030805if  1c***                if        V2Drag = *blanks
061800030805     c                   movel     ACOrag        V2Drag
061900030805e   1c***                endif
062000030805      *
062100030805      * Codice bolla da forzare in bollettazione
062200030805     c                   clear                   V2Dcbo
062300030805if  1c                   if        V2Ccbo <> *blanks
062400030807     c                   clear                   ds3A
062500030807     c                   eval      TBLcod = '3A'
062600030805     c                   movel(p)  V2Ccbo        TBLkey
062700030805     c     K03TAB00      chain     TABEL
062800030805if  2c                   if        %found(TABEL00F)
062900030805     c                             and TBLflg = *blanks
063000030807     c                   movel     TBLuni        ds3A
063100030807     c                   movel     §3Ades        V2Dcbo
063200030805x   2c                   else
063300030805     c                   seton                                        54  99
063400030806     c                   movel     $Msg(07)      V1Dmsg
063500030805     c                   goto      EndCtrV2
063600030805e   2c                   endif
063700030805e   1c                   endif
063800030805      *
063900030805     c     EndCtrV2      ENDSR
064000030805
064100030805      *---------------------------------------------------------------*
064200030805      * GESW1  - Gestione videata dati relativi alla trasmissione     *
064300030805      *---------------------------------------------------------------*
064400030805     c     GesW1         BEGSR
064500030805      *
064600030805      * Inizializzazione videata
064700030805if  1c                   if        $CarW1 = *on
064800030805     c                   exsr      CarW1
064900030805     c                   movel     *off          $CarW1
065000030805e   1c                   endif
065100030805      *
065200030805if  1c                   if        *in05
065300030805     c                   write     TB30W1
065400030805     c                   exfmt     PROTECT
065500030805x   1c                   else
065600030805     c                   exfmt     TB30W1
065700030805e   1c                   endif
065800030805     c                   eval      *in99 = *off
065900030805     c                   clear                   W1MSG
066000030805      *
066100030805sel 1c                   select
066200030805      * F12=Ritorno
066300030805w   1c                   when      *inKL
066400030805     c                   exsr      F12W1
066500030805     c                   goto      EndGesW1
066600030805e   1c                   endsl
066700030805      *
066800030805      * Controllo dati immessi a video
066900030805     c                   exsr      CtrW1
067000030805      *
067100030805      * Aggiornamento se non ci sono errori
067200030805if  1c                   if        not *in99 and *inKF
067300030805     c                   exsr      AggTBE
067400030805e   1c                   endif
067500030805      *
067600030805     c     EndGesW1      ENDSR
067700030805
067800030805      *---------------------------------------------------------------*
067900030805      * CARW1  - Caricamento dati window                              *
068000030805      *---------------------------------------------------------------*
068100030805     c     CarW1         BEGSR
068200030805      *
068300030805     c                   movea     *zeros        *in(50)
068400030805      *
068500030805sel 1c                   select
068600030805      *
068700030805      * F5=Ripristino
068800030805w   1c                   when      *inKE   and  *in04
068900030805     c                   eval      W1FTT = TBEftt
069000030805      *
069100030805      * F6=Conferma
069200030805w   1c                   when      *inKF
069300030805sel 2c                   select
069400030805      *   Immissione
069500030805w   2c                   when      *in01
069600030805     c                   eval      W1FTT = TBXftt
069700030805      *   Modifica / Ripristino
069800030805w   2c                   when      *in02   or    *in04
069900030805     c                   eval      W1FTT = TBEftt
070000030805e   2c                   endsl
070100030805      *
070200030805      * F16=Annullamento
070300030805w   1c                   when      *inKQ   and  not *in04
070400030805     c                   eval      W1FTT = TBEftt
070500030805      *
070600030805e   1c                   endsl
070700030805      *
070800030805      * Se NON immissione: visualizzo i dati relativi all'ultima
070900030805      *   trasmissione
071000030805if  1c                   if        not *in01
071100030805     c                   eval      W1FLT = TBEflt
071200030805     c                   eval      W1FTR = TBEftr
071300030805if  2c                   if        TBEdtr <> 0
071400030805     c                   clear                   WLBDAT
071500030805     c                   z-add     TBEdtr        G02inv
071600030805     c                   movel     '3'           G02err
071700030805     c                   call      'XSRDA8'
071800030805     c                   parm                    WLBDAT
071900030805     c                   z-add     G02dat        W1DTR
072000030805e   2c                   endif
072100030805e   1c                   endif
072200030805      *
072300030805     c                   ENDSR
072400030805
072500030805      *---------------------------------------------------------------*
072600030805      * CTRW1  - Controllo e decodifica window                        *
072700030805      *---------------------------------------------------------------*
072800030805     c     CtrW1         BEGSR
072900030805      *
073000030805     c                   movea     *zeros        *in(50)
073100030805      *
073200030805     c     EndCtrW1      ENDSR
073300030805
073400030805      *---------------------------------------------------------------*
073500030805      * F21W1  - Tasto funzionale F12 -> Ritorno                      *
073600030805      *---------------------------------------------------------------*
073700030805     c     F12W1         BEGSR
073800030805      *
073900030805     c                   eval      $TipVid = '2'
074000030805      *
074100030805     c                   ENDSR
074200030805
074300030805      *---------------------------------------------------------------*
074400030805      * CHNTBE * Aggancio tabella                                     *
074500030805      *---------------------------------------------------------------*
074600030805     c     ChnTBE        BEGSR
074700030805      *
074800030805     c                   movel     T02cod        TBEcod
074900030805     c                   movel(p)  DSkeyOSR      TBEke1
075000030805     c                   clear                   TBEke2
075100030805     c                   clear                   TBElin
075200030805     c                   movel     KNSIF         TBEsif
075300030805     c     K05TBE01      chain     TNTBE01L
075400030805      * Se non ho trovato il record con il sistema informativo
075500030805      * che ho in linea, lo abblenco
075600030805if  1c                   if        not %found(TNTBE01L)
075700030805     c                   clear                   TBEsif
075800030805     c     K05TBE01      chain     TNTBE01L
075900030805e   1c                   endif
076000030805      *
076100030805     c                   ENDSR
076200030805
076300030805      *---------------------------------------------------------------*
076400030805      * AGGTBE * Aggiornamento tabella                                *
076500030805      *---------------------------------------------------------------*
076600030805     c     AggTBE        BEGSR
076700030805      *
076800030805sel 1c                   SELECT
076900030805      *
077000030805      * F5=Ripristino
077100030805w   1c                   WHEN      wTasto='05'  and  *in04
077200030805     c                   clear                   TBEatb
077300030805     c                   clear                   TBEftr
077400030805     c                   UPDATE    TNTBE000
077500030805      *
077600030805      * F6=Conferma
077700030805w   1c                   WHEN      wTasto='06'
077800030805     c                   exsr      RieTBE
077900030805sel 2c                   select
078000030805      *   Immissione
078100030805w   2c                   when      *in01
078200030805     c                   clear                   TBEflt
078300030805     c                   clear                   TBEdtr
078400030805     c                   WRITE     TNTBE000                             22
078500030805      *   Modifica / Ripristino
078600030805w   2c                   when      *in02   or    *in04
078700030805     c                   UPDATE    TNTBE000
078800030805e   2c                   endsl
078900030805      *
079000030805      * F16=Annullamento
079100030805w   1c                   WHEN      wTasto='16'  and  not *in04
079200030805     c                   movel     'A'           TBEatb
079300030805     c                   clear                   TBEftr
079400030805     c                   UPDATE    TNTBE000
079500030805      *
079600030805e   1c                   ENDSL
079700030805      *
079800030805      * Torno alla prima videata che carico come da inizio pgm
079900030805     c                   movel     '1'           $TipVid
080000030805     c                   movel     *on           $CarV1
080100030805     c                   movel     *on           $CarV2
080200030805     c
080300030805     c                   ENDSR
080400030805
080500030805      *---------------------------------------------------------------*
080600030805      * RIETBE * Riempimento dati tabella                             *
080700030805      *---------------------------------------------------------------*
080800030805     c     RieTBE        BEGSR
080900030805      *
081000030805     c                   clear                   TBEatb
081100030805     c                   if        TBXsif <> *blanks
081200030805     c                   movel     KNSIF         TBEsif
081300030805     c                   else
081400030805     c                   clear                   TBEsif
081500030805     c                   endif
081600030805     c                   movel     TBXapl        TBEapl
081700030805     c                   movel     T02cod        TBEcod
081800030805     c                   movel     DSkeyOSR      TBEke1
081900030805     c                   clear                   TBEke2
082000030805      *
082100030805     c                   clear                   dOSR
082200030805     c                   move      V2Ccli        D§OSRcli
082300030805     c                   eval      D§OSRrag = V2Drag
082400030805     c                   eval      D§OSRcbo = V2Ccbo
082500030805     c                   eval      D§OSRman = V2Cman
082600030805     c                   eval      D§OSRcpy = V2Ccpy
082700030805     c                   eval      D§OSRstp = V2Cstp
082800030805     c                   eval      D§OSRass = V2Cass
082900031002     c                   eval      D§OSRsta = V2Csta
083000030805     c                   movel(p)  dOSR          TBEuni
083100030805      *
083200030807     c                   movel     W1FTT         TBEftt
083300030807     c                   z-add     TBXflt        TBEflt
083400030805     c                   clear                   TBEftr
083500030805      *
083600030805     c                   ENDSR
083700030805
083800030806**   -$OPZ-   *
083900030805  Inserimento
084000030805    Modifica
084100030805     Copia
084200030805  Annullamento
084300030805Visualizzazione
084400030805   ANNULLATO
084500030806**   -$MSG-                                                                  *
084600030808Immettere il P.O. di emissione O.R.M.                                          01
084700030805P.O. di emissione O.R.M. inesistente o annullato                               02
084800030808Immettere un numero di serie O.R.M.                                            03
084900030806Numero di serie O.R.M. non valido                                              04
085000030806Immettere il codice cliente                                                    05
085100030806Codice cliente errato                                                          06
085200030807Codice bolla inesistente in tab. "3A"                                          07
