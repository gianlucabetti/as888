000100030805      *---------------------------------------------------------------*
000200031112      * Gestione tabella "MSC" = Vdl Pacchi                           *
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
001600031112     fTNTB33D   cf   e             workstn
001700030805
001800030805      *---------------------------------------------------------------*
001900030805      *   S C H I E R E                                               *
002000030805      *---------------------------------------------------------------*
002100030805      *
002200030806     d $Opz            s             15    dim(06) ctdata perrcd(1)             Decodifica OPZ
002300031112     d $Msg            s             78    dim(08) ctdata perrcd(1)             Messaggi video
002400030805     d $Fld            s             10    dim(10)                              Campi per ricerca
002500030805
002600030805      *---------------------------------------------------------------*
002700030805      *   S T R U T T U R E   D A T I                                 *
002800030805      *---------------------------------------------------------------*
002900030805      *
003000030805      * Parametri
003100030805     d KPJBA         e ds
003200030805      *
003300030805      * Passaggio Parametri al pgm TIBS02R
003400030805     d TIBS02DS      e ds                  inz
003500030805     d  T02mod       e                     inz('R')
003600031112     d  T02cod       e                     inz('MSC')
003700030805      *
003800031112      * Tabella MSC
003900031112     d dMSC          e ds                  inz
004000030805      *
004100030805      * Tracciato record file TNTBE00F
004200030805     d TNTBEds       e ds                  extname(TNTBE00F) inz
004300030805     d xTNTBEds      e ds                  extname(TNTBE00F) inz
004400030805     d                                     prefix(TBX:3)
004500030805      *
004600030805     d TIBS34DS      e ds                  inz
004700030805     d DDATIUTE      e ds                  inz
004800030805     d AZUTEDS       e ds                  extname(AZUTE00F) inz
004900030805      *
005000030805     d WLBDAT          ds                  inz
005100030805     d  G02DAT                 1      8  0 inz
005200030805     d  G02INV                 9     16  0 inz
005300030805     d  G02ERR                17     17    inz
005400030805     d  G02TGI                18     22  0 inz
005500030805      *
005600030805     d                sds
005700030805     d  VTCpgm           *proc
005800030805
005900030805      *---------------------------------------------------------------*
006000030805      *   V A R I A B I L I                                           *
006100030805      *---------------------------------------------------------------*
006200030805      *
006300030805     d XX              s              2  0 inz
006400030805     d $Err            s              1a   inz(*off)
006500030805     d $Fine           s              1a   inz(*off)
006600030805     d $CarV1          s              1a   inz(*on)
006700030805     d $CarV2          s              1a   inz(*off)
006800030805     d $CarW1          s              1a   inz(*off)
006900030805     d $TipVid         s              1a   inz('1')
007000030805     d $Tibs02         s              1a   inz(*off)
007100030805     d Win             s             99a   inz(*zeros)
007200030805     d wTasto          s              2a   inz(*zeros)
007300030805     d CodUt           s              1  0 inz(1)
007400031112     d kazorg          s                   like(orgfil)
007500030805
007600030805      *---------------------------------------------------------------*
007700030805      *   M A I N   L I N E                                           *
007800030805      *---------------------------------------------------------------*
007900030805      *  Riepilogo indicatori utilizzati:                             *
008000030805      *  --------------------------------                             *
008100030805      *  01 - Record inesistente (inserimento)                        *
008200030805      *  02 - Record esistente   (modifica)                           *
008300030805      *  04 - Record annullato   (ripristino)                         *
008400030805      *  20 - Comodo                                                  *
008500030805      *  22 - Errori in scrittura record (WRITE)                      *
008600030805      *---------------------------------------------------------------*
008700030805      *
008800030805      * Operazioni iniziali
008900030805     c                   exsr      RutInz
009000030805      *
009100030805      * Gestione video
009200030805     c                   dow       $Fine = *off
009300030805     c     $TipVid       caseq     '1'           GesV1
009400030805     c     $TipVid       caseq     '2'           GesV2
009500030805     c     $TipVid       caseq     'A'           GesW1
009600030805     c                   endcs
009700030805     c                   enddo
009800030805      *
009900030805      * Fine
010000030805     c                   if        $Tibs02 = *on
010100030805     c                   clear                   TIBS02ds
010200030805     c                   movel     'C'           T02tla
010300030805     c                   call      'TIBS02R'
010400030805     c                   parm                    KPJBA
010500030805     c                   parm                    TIBS02ds
010600030805     c                   endif
010700030805      *
010800030805     c                   movel     *on           *inLR
010900030805
011000030805      *---------------------------------------------------------------*
011100030805      * RutInz - Operazioni Iniziali                                  *
011200030805      *---------------------------------------------------------------*
011300030805     c     RutInz        BEGSR
011400030805      *
011500030805      * Ricezione parametri
011600030805     c     *entry        plist
011700030805     c                   parm                    KPJBA
011800030805      *
011900030805      * Definizioni chiavi di accesso
012000030805     c     K05TBE01      klist                                                  *tntbe01l
012100030805     c                   kfld                    TBEcod                         -tabella
012200030805     c                   kfld                    TBEke1                         -chiave uno
012300030805     c                   kfld                    TBEke2                         -chiave due
012400030805     c                   kfld                    TBElin                         -lingua
012500030805     c                   kfld                    TBEsif                         -s.informativo
012600030805      *
012700030805     c     K03TAB00      klist
012800030805     c                   kfld                    CodUt
012900030805     c                   kfld                    TBLcod
013000030805     c                   kfld                    TBLkey
013100030805      *
013200030805      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
013300030805     c     *dtaara       define    §azute        azuteds
013400030805     c     *dtaara       define    §datiute      ddatiute
013500030805      *
013600030805     c                   clear                   AzUteDs
013700030805     c                   clear                   DDatiUte
013800030805     c                   clear                   Tibs34Ds
013900030805     c                   in(E)     *dtaara
014000030805if  1c                   if        %Error  or  RSUT = *blanks
014100030805     c                   call      'TIBS34R'
014200030805     c                   parm                    Tibs34Ds
014300030805     c                   in        *dtaara
014400030805e   1c                   endif
014500030805      *-- Verifica errori e autorità profilo
014600030805sel 1c                   SELECT
014700030805      *-- controllo se ho errori nei dati utente
014800030805      *--   nel qual caso NON risulta un profilo abilitato
014900030805w   1c                   WHEN      DUTerr = 'E'
015000030805     c                   eval      $Fine  = *on
015100030805      *
015200030805      *-- CONTROLLO AUTORITA'
015300030805      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
015400030805      *-- se il chiamante non richiede autorità specifica verificare
015500030805      *--   quella del profilo
015600030805      *-- se il chiamante richiede autorità specifica verificarla,
015700030805      *--  se è blank verificare quella del profilo
015800030805      *
015900030805      * se UTEAUT = *BLANK non siamo in sede
016000030805w   1c                   WHEN      UTEaut = *blank
016100030805      *
016200030805x   1c                   OTHER
016300030805      *
016400030805e   1c                   ENDSL
016500030805      *
016600030805      * Aggancio dati generali della tabella in esame
016700030805     c                   clear                   TBEcod
016800030805     c                   move      *zeros        TBEke1
016900030805     c                   move      T02cod        TBEke1
017000030805     c                   clear                   TBEke2
017100030805     c                   clear                   TBElin
017200030805     c                   movel     KNSIF         TBEsif
017300030805     c     K05TBE01      chain     TNTBE01L
017400030805     c                   if        not %found(TNTBE01L)
017500030805     c                   clear                   TBEsif
017600030805     c     K05TBE01      chain     TNTBE01L
017700030805     c                   endif
017800030805     c                   if        %found(TNTBE01L)
017900030805     c                   movel     TNTBEds       xTNTBEds
018000030805     c                   else
018100030805     c                   clear                   xTNTBEds
018200030805     c                   endif
018300030808      *
018400031112     c                   clear                   TB33V1
018500030808      *
018600031112      * Imposto il P.O.
018700031112     c                   clear                   V1Cfgs
018800031112     c                   clear                   V1Cprg
018900030805      *
019000030805     c                   ENDSR
019100030805
019200030805      *---------------------------------------------------------------*
019300030805      * GESV1  - Gestione videata selezione codice tabella            *
019400030805      *---------------------------------------------------------------*
019500030805     c     GesV1         BEGSR
019600030805      *
019700030805      * Inizializzazione videata
019800030805if  1c                   if        $CarV1 = *on
019900030805     c                   exsr      CarV1
020000030805     c                   movel     *off          $CarV1
020100030805e   1c                   endif
020200030805      *
020300030805      * Scrivo la testata
020400030805     c                   clear                   T1opz
020500031112     c                   write     TB33T1
020600030805      *
020700030805      * Se esistono errori sulla videata
020800030805      * emetto la write del formato a indicatori spenti per vedere
020900030805      * le eventuali decodifiche
021000030805if  1c                   if        *in99
021100030805     c                   movea     *in           Win
021200030805     c                   movea     *zeros        *in(50)
021300031112     c                   write     TB33V1
021400030805     c                   movea     Win           *in
021500030805e   1c                   endif
021600030805      *
021700031112     c                   exfmt     TB33V1
021800030805     c                   eval      *in99 = *off
021900030805     c                   clear                   V1Dmsg
022000031112      * Ricerca
022100031112     c     '?'           scan      V1Cfgs                                 20
022200031112
022300031112     c                   select
022400030805      * F3=Fine
022500031112     c                   when      *inKC
022600030805     c                   exsr      F03V1
022700031112
022800031112     c                   when      *in20
022900031112     c                   exsr      Search
023000031112
023100030805      * Controllo dati immessi a video
023200031112     c                   other
023300030805     c                   exsr      CtrV1
023400030805      *
023500030805      * Passaggio alla videata di dettaglio
023600030805if  1c                   if        not *in99
023700030805     c                   eval      $CarV2 = *on
023800030805     c                   eval      $TipVid = '2'
023900030805e   1c                   endif
024000031112
024100031112     c                   endsl
024200031112
024300031112     c                   ENDSR
024400030805
024500030805      *---------------------------------------------------------------*
024600030805      * CARV1  - Caricamento dati prima videata                       *
024700030805      *---------------------------------------------------------------*
024800030805     c     CarV1         BEGSR
024900030805      *
025000030805     c                   movea     *zeros        *in(50)
025100030805     c                   movea     '00000'       *in(01)
025200030805      *
025300031112     c                   move (p)  '?'           V1Cfgs
025400031112     c                   clear                   v1cprg
025500030805      *
025600030805     c                   ENDSR
025700030805
025800030805      *---------------------------------------------------------------*
025900030805      * CTRV1  - Controllo e decodifica prima videata                 *
026000030805      *---------------------------------------------------------------*
026100030805     c     CtrV1         BEGSR
026200030805      *
026300030805     c                   movea     *zeros        *in(50)
026400030805     c                   clear                   V1Dmsg
026500031112     c                   clear                   v1dfgs
026600031112      *
026700031112      * - Punto Operativo obbligatorio
026800031112if  1c                   If        v1cfgs = *Blanks
026900031112     c                   seton                                        50  99
027000031112     c                   eval      V1Dmsg = $Msg(01)
027100031112     c                   goto      EndCtrV1
027200031112    1c                   Endif
027300031112
027400031112     c                   Movel     v1cfgs        kAzorg
027500031112     c                   exsr      CtrPO
027600031112     c                   if        *in99
027700031112     c                   eval      V1Dmsg = $Msg(02)
027800031112     c                   seton                                        50
027900031112     c                   goto      EndCtrV1
028000031112     c                   endif
028100031112
028200031112     c                   Movel     orgdes        v1dfgs
028300031112
028400031112      * - Progressivo obbligatorio
028500031112if  1c                   If        v1cprg = *zeros
028600031112     c                   seton                                        51  99
028700031112     c                   eval      V1Dmsg = $Msg(08)
028800031112     c                   goto      EndCtrV1
028900031112    1c                   Endif
029000030805      *
029100030805     c     EndCtrV1      ENDSR
029200030805
029300030805      *---------------------------------------------------------------*
029400030805      * F03V1  - Tasto funzionale F03 -> Fine programma               *
029500030805      *---------------------------------------------------------------*
029600030805     c     F03V1         BEGSR
029700030805      *
029800030805     c                   movel     *on           $Fine                          fine pgm
029900030805      *
030000030805     c                   ENDSR
030100030805
030200030805      *---------------------------------------------------------------*
030300030805      * CTRPO  - Controllo Punto Operativo                            *
030400030805      *---------------------------------------------------------------*
030500030805     c     CtrPO         BEGSR
030600030805      *
030700031112     c                   Clear                   orgdes
030800031112     c     kazorg        chain     AZORG01L
030900030805if  1c                   if        not %found(AZORG01L)
031000030805     c                             or  ORGfva =  '*'
031100031112     c                   seton                                            99
031200030805e   1c                   endif
031300030805      *
031400031112     c                   ENDSR
031500030805
031600030805      *---------------------------------------------------------------*
031700030805      * SEARCH - Ricerche                                             *
031800030805      *---------------------------------------------------------------*
031900030805     c     Search        BEGSR
032000031112
032100031112     C                   Reset                   Tibs02Ds
032200031112     c                   movel     KnSif         T02Sif
032300031112     C                   CALL      'TIBS02R'
032400031112     C                   PARM                    kpjba
032500031112     C                   PARM                    Tibs02Ds
032600031112     C                   if        T02Err =  *blanks
032700031112     C                   movel     T02ke1        V1cFgs
032800031112     C                   movel     T02ke2        V1cPrg
032900031112     c                   endif
033000030805      *
033100030805     c                   ENDSR
033200030805
033300030805      *---------------------------------------------------------------*
033400030805      * GESV2  - Gestione videata dettaglio dati                      *
033500030805      *---------------------------------------------------------------*
033600030805     c     GesV2         BEGSR
033700030805      *
033800030805      * Inizializzazione videata
033900030805     c                   if        $CarV2 = *on
034000030805     c                   exsr      CarV2
034100030805     c                   move      *off          $CarV2
034200030805     c                   endif
034300030805      * Scrivo la testata
034400031112     c                   write     TB33T1
034500030805      *
034600030805      * Se esistono errori sulla videata
034700030805      * emetto la write del formato a indicatori spenti per vedere
034800030805      * le eventuali decodifiche
034900030805     c                   if        *in99
035000030805     c                   movea     *in           Win
035100030805     c                   movea     *zeros        *in(50)
035200031112     c                   write     TB33V2
035300030805     c                   movea     Win           *in
035400030805     c                   endif
035500030805      *
035600030805if  1c                   if        *in05
035700031112     c                   write     TB33V2
035800030805     c                   exfmt     PROTECT
035900030805x   1c                   else
036000031112     c                   exfmt     TB33V2
036100030805e   1c                   endif
036200030805     c                   eval      *in99 = *off
036300030805     c                   clear                   V1Dmsg
036400030805     c                   clear                   wTasto
036500030805      *
036600030805sel 1c                   select
036700030805      * F03=Fine
036800030805w   1c                   when      *inKC
036900030805     c                   exsr      F03V1
037000030805     c                   goto      EndGesV2
037100030805      * F12=Ritorno
037200030805w   1c                   when      *inKL
037300030805     c                   exsr      F12V2
037400030805     c                   goto      EndGesV2
037500030805e   1c                   endsl
037600030805      *
037700030805      * Controllo dati immessi a video
037800030805      * (non si fanno se richisto l'annullamento)
037900030805if  1c                   if        not *inKQ
038000030805     c                   exsr      CtrV2
038100030805e   1c                   endif
038200030805      *
038300030805      * Aggiornamento se non ci sono errori
038400030805if  1c                   if        not *in99
038500030805     c                             and (*inKF or *inKE or *inKQ)
038600030805sel 2c                   select
038700030805w   2c                   when      *inKE
038800030805     c                   eval      wTasto = '05'
038900030805w   2c                   when      *inKF
039000030805     c                   eval      wTasto = '06'
039100030805w   2c                   when      *inKQ
039200030805     c                   eval      wTasto = '16'
039300030805e   2c                   endsl
039400030805     c                   eval      $CarW1 = *on
039500030805     c                   eval      $TipVid = 'A'
039600030805e   1c                   endif
039700030805      *
039800030805     c     EndGesV2      ENDSR
039900030805
040000030805      *---------------------------------------------------------------*
040100030805      * CARV2  - Caricamento dati seconda videata                     *
040200030805      *---------------------------------------------------------------*
040300030805     c     CarV2         BEGSR
040400030805      *
040500030805     c                   clear                   T1opz
040600030805     c                   movea     '00000'       *in(01)
040700030805      *
040800030805      * Aggancio la tabella, se trovo il codice sono in modifica
040900030805      * o ripristino (se record annullato), altrimenti in immissione
041000030805     c                   exsr      ChnTBE
041100030805      *
041200030805if  1c                   if        %found(TNTBE01L)
041300030805      *
041400030805      * MODIFICA/RIPRISTINO
041500031112     c                   movel     TBEuni        dmsc
041600030805if  2c                   if        TBEatb = *blanks
041700030805     c                   eval      *in02  = *on
041800030805     c                   eval      T1opz  = $Opz(02)
041900030805x   2c                   else
042000030805     c                   eval      *in04  = *on
042100030805     c                   eval      T1opz  = $Opz(06)
042200030805e   2c                   endif
042300030805      *
042400030805x   1c                   else
042500030805      *
042600030805      * IMMISSIONE
042700031112     c                   clear                   dmsc
042800030805     c                   eval      *in01  = *on
042900030805     c                   eval      T1opz  = $Opz(01)
043000030805      *
043100030805e   1c                   endif
043200030805      *
043300031112     c                   move      V1Cfgs        V2Cfgs
043400031112     c                   eval      V2Dfgs = V1Dfgs
043500031112     c                   eval      V2Cprg = V1Cprg
043600031112     c                   eval      V2Cdes = §mscdes
043700031112     c                   eval      V2Clnp = §msclnp
043800031112     c                   eval      V2Clna = §msclna
043900031112     c                   eval      V2Cnrs = §mscnrs
044000031112     c                   eval      V2Cnsc = §mscnsc
044100031112     c                   eval      V2Cznc = §mscznc
044200031112     c                   eval      V2Cpes = §mscpes
044300031112     c                   eval      V2Cvol = §mscvol
044400031112     c                   Movel     V2Clnp        kAzorg
044500031112     c                   exsr      CtrPO
044600031112     c                   Movel     orgdes        v2dlnp
044700031112     c                   Movel     V2Clna        kAzorg
044800031112     c                   exsr      CtrPO
044900031112     c                   Movel     orgdes        v2dlna
045000030805      *
045100030805     c                   ENDSR
045200030805
045300030805      *---------------------------------------------------------------*
045400030805      * F12V2  - Tasto funzionale F12 -> Ritorno                      *
045500030805      *---------------------------------------------------------------*
045600030805     c     F12V2         BEGSR
045700030805      *
045800030805     c                   eval      $TipVid = '1'
045900030805      *
046000030805     c                   ENDSR
046100030805
046200030805      *---------------------------------------------------------------*
046300030805      * CTRV2  - Controllo e decodifica seconda videata               *
046400030805      *---------------------------------------------------------------*
046500030805     c     CtrV2         BEGSR
046600030805      *
046700030805     c                   movea     *zeros        *in(50)
046800030805     c                   clear                   V1Dmsg
046900031112     c                   clear                   v2dlnp
047000031112     c                   clear                   v2dlna
047100031112
047200031112      * - Descrizione
047300031112if  1c                   IF           V2Cdes = *blanks
047400030805     c                   seton                                        52  99
047500031112     c                   movel     $Msg(03)      V1Dmsg
047600030805     c                   goto      EndCtrV2
047700030805e   1c                   ENDIF
047800031112
047900031112      * - Linea partenza
048000031112     c                   clear                   V2Dlnp
048100031112if  1c                   if        V2Clnp = *zeros
048200031112     c                   seton                                        53  99
048300031112     c                   movel     $Msg(04)      V1Dmsg
048400030805     c                   goto      EndCtrV2
048500031112e   1c                   endif
048600031112
048700031112     c                   Movel     V2Clnp        kAzorg
048800031112     c                   exsr      CtrPO
048900031112     c                   if        *in99
049000031112     c                   eval      V1Dmsg = $Msg(05)
049100031112     c                   seton                                        53
049200031112     c                   goto      EndCtrV2
049300031112     c                   endif
049400031112
049500031112     c                   Movel     orgdes        v2dlnp
049600031112
049700031112      * - Linea arrivo
049800031112     c                   clear                   V2Dlna
049900031112if  1c                   if        V2Clna = *zeros
050000031112     c                   seton                                        54  99
050100031112     c                   movel     $Msg(06)      V1Dmsg
050200031112     c                   goto      EndCtrV2
050300031112e   1c                   endif
050400031112
050500031112     c                   Movel     V2Clna        kAzorg
050600031112     c                   exsr      CtrPO
050700031112     c                   if        *in99
050800031112     c                   eval      V1Dmsg = $Msg(07)
050900031112     c                   seton                                        54
051000031112     c                   goto      EndCtrV2
051100031112     c                   endif
051200031112
051300031112     c                   Movel     orgdes        v2dlna
051400030805      *
051500030805     c     EndCtrV2      ENDSR
051600030805
051700030805      *---------------------------------------------------------------*
051800030805      * GESW1  - Gestione videata dati relativi alla trasmissione     *
051900030805      *---------------------------------------------------------------*
052000030805     c     GesW1         BEGSR
052100030805      *
052200030805      * Inizializzazione videata
052300030805if  1c                   if        $CarW1 = *on
052400030805     c                   exsr      CarW1
052500030805     c                   movel     *off          $CarW1
052600030805e   1c                   endif
052700030805      *
052800030805if  1c                   if        *in05
052900031112     c                   write     TB33W1
053000030805     c                   exfmt     PROTECT
053100030805x   1c                   else
053200031112     c                   exfmt     TB33W1
053300030805e   1c                   endif
053400030805     c                   eval      *in99 = *off
053500030805     c                   clear                   W1MSG
053600030805      *
053700030805sel 1c                   select
053800030805      * F12=Ritorno
053900030805w   1c                   when      *inKL
054000030805     c                   exsr      F12W1
054100030805     c                   goto      EndGesW1
054200030805e   1c                   endsl
054300030805      *
054400030805      * Controllo dati immessi a video
054500030805     c                   exsr      CtrW1
054600030805      *
054700030805      * Aggiornamento se non ci sono errori
054800030805if  1c                   if        not *in99 and *inKF
054900030805     c                   exsr      AggTBE
055000030805e   1c                   endif
055100030805      *
055200030805     c     EndGesW1      ENDSR
055300030805
055400030805      *---------------------------------------------------------------*
055500030805      * CARW1  - Caricamento dati window                              *
055600030805      *---------------------------------------------------------------*
055700030805     c     CarW1         BEGSR
055800030805      *
055900030805     c                   movea     *zeros        *in(50)
056000030805      *
056100030805sel 1c                   select
056200030805      *
056300030805      * F5=Ripristino
056400030805w   1c                   when      *inKE   and  *in04
056500030805     c                   eval      W1FTT = TBEftt
056600030805      *
056700030805      * F6=Conferma
056800030805w   1c                   when      *inKF
056900030805sel 2c                   select
057000030805      *   Immissione
057100030805w   2c                   when      *in01
057200030805     c                   eval      W1FTT = TBXftt
057300030805      *   Modifica / Ripristino
057400030805w   2c                   when      *in02   or    *in04
057500030805     c                   eval      W1FTT = TBEftt
057600030805e   2c                   endsl
057700030805      *
057800030805      * F16=Annullamento
057900030805w   1c                   when      *inKQ   and  not *in04
058000030805     c                   eval      W1FTT = TBEftt
058100030805      *
058200030805e   1c                   endsl
058300030805      *
058400030805      * Se NON immissione: visualizzo i dati relativi all'ultima
058500030805      *   trasmissione
058600030805if  1c                   if        not *in01
058700030805     c                   eval      W1FLT = TBEflt
058800030805     c                   eval      W1FTR = TBEftr
058900030805if  2c                   if        TBEdtr <> 0
059000030805     c                   clear                   WLBDAT
059100030805     c                   z-add     TBEdtr        G02inv
059200030805     c                   movel     '3'           G02err
059300030805     c                   call      'XSRDA8'
059400030805     c                   parm                    WLBDAT
059500030805     c                   z-add     G02dat        W1DTR
059600030805e   2c                   endif
059700030805e   1c                   endif
059800030805      *
059900030805     c                   ENDSR
060000030805
060100030805      *---------------------------------------------------------------*
060200030805      * CTRW1  - Controllo e decodifica window                        *
060300030805      *---------------------------------------------------------------*
060400030805     c     CtrW1         BEGSR
060500030805      *
060600030805     c                   movea     *zeros        *in(50)
060700030805      *
060800030805     c     EndCtrW1      ENDSR
060900030805
061000030805      *---------------------------------------------------------------*
061100030805      * F21W1  - Tasto funzionale F12 -> Ritorno                      *
061200030805      *---------------------------------------------------------------*
061300030805     c     F12W1         BEGSR
061400030805      *
061500030805     c                   eval      $TipVid = '2'
061600030805      *
061700030805     c                   ENDSR
061800030805
061900030805      *---------------------------------------------------------------*
062000030805      * CHNTBE * Aggancio tabella                                     *
062100030805      *---------------------------------------------------------------*
062200030805     c     ChnTBE        BEGSR
062300030805      *
062400030805     c                   movel     T02cod        TBEcod
062500031112     c                   movel(p)  v1cfgs        TBEke1
062600031112     c                   movel(p)  v1cprg        TBEke2
062700030805     c                   clear                   TBElin
062800030805     c                   movel     KNSIF         TBEsif
062900030805     c     K05TBE01      chain     TNTBE01L
063000030805      * Se non ho trovato il record con il sistema informativo
063100030805      * che ho in linea, lo abblenco
063200030805if  1c                   if        not %found(TNTBE01L)
063300030805     c                   clear                   TBEsif
063400030805     c     K05TBE01      chain     TNTBE01L
063500030805e   1c                   endif
063600030805      *
063700030805     c                   ENDSR
063800030805
063900030805      *---------------------------------------------------------------*
064000030805      * AGGTBE * Aggiornamento tabella                                *
064100030805      *---------------------------------------------------------------*
064200030805     c     AggTBE        BEGSR
064300030805      *
064400030805sel 1c                   SELECT
064500030805      *
064600030805      * F5=Ripristino
064700030805w   1c                   WHEN      wTasto='05'  and  *in04
064800030805     c                   clear                   TBEatb
064900030805     c                   clear                   TBEftr
065000030805     c                   UPDATE    TNTBE000
065100030805      *
065200030805      * F6=Conferma
065300030805w   1c                   WHEN      wTasto='06'
065400030805     c                   exsr      RieTBE
065500030805sel 2c                   select
065600030805      *   Immissione
065700030805w   2c                   when      *in01
065800030805     c                   clear                   TBEflt
065900030805     c                   clear                   TBEdtr
066000030805     c                   WRITE     TNTBE000                             22
066100030805      *   Modifica / Ripristino
066200030805w   2c                   when      *in02   or    *in04
066300030805     c                   UPDATE    TNTBE000
066400030805e   2c                   endsl
066500030805      *
066600030805      * F16=Annullamento
066700030805w   1c                   WHEN      wTasto='16'  and  not *in04
066800030805     c                   movel     'A'           TBEatb
066900030805     c                   clear                   TBEftr
067000030805     c                   UPDATE    TNTBE000
067100030805      *
067200030805e   1c                   ENDSL
067300030805      *
067400030805      * Torno alla prima videata che carico come da inizio pgm
067500030805     c                   movel     '1'           $TipVid
067600030805     c                   movel     *on           $CarV1
067700030805     c                   movel     *on           $CarV2
067800030805     c
067900030805     c                   ENDSR
068000030805
068100030805      *---------------------------------------------------------------*
068200030805      * RIETBE * Riempimento dati tabella                             *
068300030805      *---------------------------------------------------------------*
068400030805     c     RieTBE        BEGSR
068500030805      *
068600030805     c                   clear                   TBEatb
068700030805     c                   if        TBXsif <> *blanks
068800030805     c                   movel     KNSIF         TBEsif
068900030805     c                   else
069000030805     c                   clear                   TBEsif
069100030805     c                   endif
069200030805     c                   movel     TBXapl        TBEapl
069300030805     c                   movel     T02cod        TBEcod
069400031112     c                   movel     v1cfgs        TBEke1
069500031112     c                   movel     v1cprg        TBEke2
069600030805      *
069700031112     c                   clear                   dmsc
069800031112     c                   eval      §mscdes = V2Cdes
069900031112     c                   eval      §msclnp = V2Clnp
070000031112     c                   eval      §msclna = V2Clna
070100031112     c                   eval      §mscnrs = V2Cnrs
070200031112     c                   eval      §mscnsc = V2Cnsc
070300031112     c                   eval      §mscznc = V2Cznc
070400031112     c                   eval      §mscpes = V2Cpes
070500031112     c                   eval      §mscvol = V2Cvol
070600031112     c                   movel(p)  dmsc          TBEuni
070700030805      *
070800030807     c                   movel     W1FTT         TBEftt
070900030807     c                   z-add     TBXflt        TBEflt
071000030805     c                   clear                   TBEftr
071100030805      *
071200030805     c                   ENDSR
071300030805
071400030806**   -$OPZ-   *
071500030805  Inserimento
071600030805    Modifica
071700030805     Copia
071800030805  Annullamento
071900030805Visualizzazione
072000030805   ANNULLATO
072100030806**   -$MSG-                                                                  *
072200031112Immettere il Punto Operativo                                                   01
072300031112Punto operativo inesistente o annullato                                        02
072400031112Immettere la descrizione                                                       03
072500031112Immettere la linea di partenza                                                 04
072600031112Linea di partenza inesistente o annullato                                      05
072700031112Immettere la linea di arrivo                                                   06
072800031112Linea di arrivo inesistente o annullato                                        07
072900031112Immettere il Progressivo                                                       08
