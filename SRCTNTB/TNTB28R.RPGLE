000100101005     /*CMD ovrdbf file(TNTBESCK) tofile(*libl/TNTBE01L) ovrscope(*calllvl)
000200101005     /*PRM dbgview(*source)
000300101005     /*END dltovr file(TNTBESCK) lvl(*)
000400101005     /*END
000500020212      *---------------------------------------------------------------*
000600020912      * Gestione tabella "SCH" = Schedulatore                         *
000700020212      *---------------------------------------------------------------*
000800090423      *? N.B.?                                                        *
000900090423      * La compilazione richiede la seguente override:                *
001000090423      *?OVRDBF FILE(TNTBESCK) TOFILE(*LIBL/TNTBE01L)                 ?*
001100090423      *---------------------------------------------------------------*
001200020212
001300090320     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
001400020212
001500020212      *---------------------------------------------------------------*
001600020212      *   A R C H I V I                                               *
001700020212      *---------------------------------------------------------------*
001800020212      *
001900020212     FTNTBE01L  UF A E           K DISK
002000080721     FTNTBESCK  UF A E           K DISK    usropn prefix(k_)
002100080721     F                                     rename(TNTBE000:TNTBE00K)
002200020913     FKFSIF01L  IF   E           K DISK
002300020918     FKPPRF01L  IF   E           K DISK
002400090505     fkfazn11l  if   e           k disk
002500020212      *
002600020912     FTNTB28D   CF   E             WORKSTN
002700020212
002800020212      *---------------------------------------------------------------*
002900020212      *   S C H I E R E                                               *
003000020212      *---------------------------------------------------------------*
003100020212      *
003200020215     D Opz             s             15    dim(06) ctdata perrcd(1)             Decodifica OPZ
003300131128     D GIORSET         s              3    dim(7) ctdata perrcd(7)              Giorni della Sett.
003400060426     D Msg             s             78    dim(26) ctdata perrcd(1)             Messaggi video
003500020212      *
003600020215     D NmFl            s             10    dim(10)                              Campi per ricerca
003700020913     D Ora             s              4  0 dim(15)                              Orari
003800131128     D giaimm          s              3    dim(7)
003900131128     D numset          s              1  0 dim(7)
004000020212
004100020212      *---------------------------------------------------------------*
004200020212      *   S T R U T T U R E   D A T I                                 *
004300020212      *---------------------------------------------------------------*
004400090319
004500090319     d Status         sds
004600090319     d  SDSpgm           *proc
004700020212      *
004800020212      * Parametri
004900020212     D KPJBA         e ds
005000020212      *
005100020212      * Passaggio Parametri al pgm TIBS02R
005200020212     D TIBS02DS      e ds                  inz
005300020212     D  T02mod       e                     inz('C')
005400020912     D  T02cod       e                     inz('SCH')
005500020212      *
005600020912      * Tabella SCH = Schedulatore
005700020912     D DSCH          e ds                  inz
005800050222     D  alfschggm            122    123
005900020212      *
006000020212      * Tracciato record file TNTBE00F
006100020212     D TNTBEds       e ds                  extname(TNTBE00F) inz
006200020212     D xTNTBEds      e ds                  extname(TNTBE00F) inz
006300020212     D                                     prefix(TBX:3)
006400090320     D yTNTBEds      e ds                  extname(TNTBE00F) inz
006500090320     D                                     prefix(TBY:3)
006600020724
006700020724     D TIBS34DS      e ds                  inz
006800020724     D DDATIUTE      e ds                  inz
006900020724     D AZUTEDS       e ds                  inz extname(AZUTE00F)
007000080721      *
007100090319      * - Passaggio Parametri al pgm TRULKPJR
007200080721     D TRULKPJDS     e ds                  inz
007300090319      *
007400090319      * - Reperimento numeratore
007500090319     D TRUL33ds      e ds                  inz
007600020226
007700020226     D WLBDAT          ds                  inz
007800020226     D  G02DAT                 1      8  0
007900020226     D  G02INV                 9     16  0
008000020226     D  G02ERR                17     17
008100020226     D  G02TGI                18     22  0
008200021114
008300021114     d param01         ds
008400021114     d  p01sch                        4
008500021114     d  p01ord                        1
008600021114     d  p01sif                       10
008700101005     d  p01opz                        1
008800021114     d  p01ke1                        4
008900080721     d  p01ke2                       15
009000110603     d  p01cnd                       20
009100021114     d  p01rit                        1
009200161212     d  p01init                       1
009300110603      *
009400060426     d                 ds
009500060426     d  v2g1g                  1      3
009600060426     d  v2g2g                  4      6
009700060426     d  v2g3g                  7      9
009800060426     d  v2g4g                 10     12
009900060426     d  v2g5g                 13     15
010000060426     d  v2g6g                 16     18
010100060426     d  v2g7g                 19     21
010200060426     d GGGiorn                 1     21    dim(7)
010300020212
010400161212     d wp01init        s              1
010500020212      *---------------------------------------------------------------*
010600020212      *   V A R I A B I L I                                           *
010700020212      *---------------------------------------------------------------*
010800020212      *
010900060427     D CC              s              1  0 inz
011000060427     D XX              s              2  0 inz
011100020913     D yy              s              2  0 inz
011200020212     D $Err            s              1a   inz(*off)
011300020212     D $Fine           s              1a   inz(*off)
011400020913     D $CarV0          s              1a   inz(*on)
011500020913     D $CarV1          s              1a   inz(*off)
011600020212     D $CarV2          s              1a   inz(*off)
011700020226     D $CarW1          s              1a   inz(*off)
011800090423     D $NewKPJBU       s               n   inz
011900020913     D TipVid          s              1a   inz('0')
012000170119     D PrvVid          s              1a   inz(' ')
012100020212     D Win             s             99a   inz(*zeros)
012200020301     D wTasto          s              2a   inz(*zeros)
012300020913     d totora          s              5  0
012400020913     d wora            s              2  0
012500020913     d wminuti         s              2  0
012600110603      *
012700110603     d SavP01ord       s                   like(P01ord)  inz
012800110603     d SavP01cnd       s                   like(P01cnd)  inz
012900051107      *
013000051107      * Parametri x pgm. QCMDEXC
013100051107     d Qcmd            s            100    inz
013200051107     d Qlen            s             15  5 inz(100)
013300090320
013400090320       //--------------------------------------------------------------
013500090320       //?Definizione procedure usate.
013600090320       //--------------------------------------------------------------
013700090320
013800090320       // - Reperimento/Aggiornamento numeratori in GRU
013900090320      /copy gaitrasrc/srcProtoPr,TRUL33R
014000090320
014100090320       //--------------------------------------------------------------
014200090320       //?Definizione key-list.                                        ?
014300090320       //--------------------------------------------------------------
014400090320
014500090320     d keyTbeSCK_P   e ds                  extname(TNTBESCK : *key)
014600090320     d                                     prefix(k_)
014700090320     d                                     inz
014800020212
014900020212      *---------------------------------------------------------------*
015000020212      *   M A I N   L I N E                                           *
015100020212      *---------------------------------------------------------------*
015200020212      *  Riepilogo indicatori utilizzati:                             *
015300020212      *  --------------------------------                             *
015400020212      *  01 - Record inesistente (inserimento)                        *
015500020212      *  02 - Record esistente   (modifica)                           *
015600020212      *  04 - Record annullato   (ripristino)                         *
015700020301      *  20 - Comodo                                                  *
015800020212      *  22 - Errori in scrittura record (WRITE)                      *
015900020212      *---------------------------------------------------------------*
016000020212      *
016100020212      * Operazioni iniziali
016200020212     C                   exsr      RutInz
016300020212      *
016400020212      * Gestione video
016500020724     C     $Fine         downe     'S'
016600020913     C     TipVid        caseq     '0'           GesV0
016700020212     C     TipVid        caseq     '1'           GesV1
016800020212     C     TipVid        caseq     '2'           GesV2
016900110603     C     TipVid        caseq     '3'           GesV1N
017000020226     C     TipVid        caseq     'A'           GesW1
017100020212     C                   endcs
017200020212     C                   enddo
017300020212      *
017400080718     c*
017500080718     c* Elimino override al file tabelle come TNTBESCK
017600080718     c                   eval      Qcmd    =
017700080718     c                             'DLTOVR FILE(TNTBESCK) LVL(*)'
017800080718     c                   call(e)   'QCMDEXC'
017900080718     c                   parm                    Qcmd
018000080718     c                   parm                    Qlen
018100080718      *
018200080718      *
018300020212     C                   movel     *on           *inLR
018400020212
018500020212      *---------------------------------------------------------------*
018600020212      * RutInz - Operazioni Iniziali                                  *
018700020212      *---------------------------------------------------------------*
018800020212     C     RutInz        BEGSR
018900020212      *
019000020212      * Ricezione parametri
019100020212     C     *entry        plist
019200020212     C                   parm                    KPJBA
019300020212      *
019400020212      * Definizioni chiavi di accesso
019500020212     C     K05TBE01      klist                                                  *tntbe01l
019600020212     C                   kfld                    TBEcod                         -tabella
019700020212     C                   kfld                    TBEke1                         -chiave uno
019800020212     C                   kfld                    TBEke2                         -chiave due
019900020212     C                   kfld                    TBElin                         -lingua
020000020212     C                   kfld                    TBEsif                         -s.informativo
020100020724      *
020200020724      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
020300020724     C     *dtaara       define    §azute        azuteds
020400020724     C     *dtaara       define    §datiute      ddatiute
020500020724      *
020600020724     C                   clear                   AzUteDs
020700020724     C                   clear                   DDatiUte
020800020724     C                   clear                   Tibs34Ds
020900020724     C                   in(E)     *dtaara
021000020724if  1C                   IF        %Error  or  RSUT = *blanks
021100020724     C                   call      'TIBS34R'
021200020724     C                   parm                    Tibs34Ds
021300020724     C                   in        *dtaara
021400020724e   1C                   ENDIF
021500020724      *-- Verifica errori e autorità profilo
021600020724sel 1C                   SELECT
021700020724      *-- controllo se ho errori nei dati utente
021800020724      *--   nel qual caso NON risulta un profilo abilitato
021900020724w   1C                   WHEN      DUTERR = 'E'
022000020724     C                   eval      $Fine  = 'S'
022100020724      *
022200020724      *-- CONTROLLO AUTORITA'
022300020724      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
022400020724      *-- se il chiamante non richiede autorità specifica verificare
022500020724      *--   quella del profilo
022600020724      *-- se il chiamante richiede autorità specifica verificarla,
022700020724      *--  se è blank verificare quella del profilo
022800020724      *
022900020724      * se UTEAUT = *BLANK non siamo in sede
023000020724w   1C                   WHEN      UTEAUT = *blank
023100020724      *
023200020724x   1c                   OTHER
023300020724      *
023400020724e   1C                   ENDSL
023500090319      *
023600080718     c* Eseguo override al file tabelle come TNTBESCK
023700080718     c                   eval      Qcmd    =
023800080718     c                             'OVRDBF FILE(TNTBESCK) ' +
023900080718     c                             'TOFILE(*LIBL/TNTBE01L)'
024000080718     c                   call(e)   'QCMDEXC'
024100080718     c                   parm                    Qcmd
024200080718     c                   parm                    Qlen
024300080718     c*
024400080718     c* Quindi apro il file
024500080721     c                   open      TNTBESCK
024600090319      *
024700090319     c                   movel     SDSpgm        V1Tpgm
024800090319      *
024900020913     c                   EndSr
025000020913      *---------------------------------------------------------------*
025100020913      * GESV0  - Gestione videata richiesta S.I.                      *
025200020913      *---------------------------------------------------------------*
025300020913     C     GESV0         BEGSR
025400020913      *
025500020913      * Inizializzazione videata
025600020913if  1C                   if        $CarV0 = *on
025700020913     C                   exsr      CARV0
025800020913     C                   movel     *off          $CarV0
025900020913e   1C                   endif
026000020913      *
026100020913      * Scrivo la testata
026200020913     C                   clear                   T1opz
026300020913     C                   write     TB28T1
026400020913      *
026500020913      * Se esistono errori sulla videata
026600020913      * emetto la write del formato a indicatori spenti per vedere
026700020913      * le eventuali decodifiche
026800020913if  1C                   if        *in99
026900020913     C                   movea     *in           WIN
027000080721     C                   movea     *zeros        *in(40)
027100020913     C                   write     TB28V0
027200020913     C                   movea     WIN           *in
027300020913e   1C                   endif
027400020913      *
027500020913     C                   exfmt     TB28V0
027600020913     C                   eval      *in99 = *off
027700020913     C                   clear                   V0MSG
027800020913      * F3=Fine
027900020913     C                   if        *inKC
028000020913cas 1C                   exsr      F03V0
028100101005     C                   leavesr
028200020913e   1C                   endif
028300020913      *
028400020913      * Controllo se immesso il s.i.
028500020913if  1C                   if        not *in99
028600020913     C                   exsr      CTRV0
028700020913e   1C                   endif
028800020212      *
028900090320      * Aggancio dati generali della tabella SCH
029000020212     C                   clear                   TBEcod
029100020212     C                   move      *zeros        TBEke1
029200020212     C                   move      T02cod        TBEke1
029300020212     C                   clear                   TBEke2
029400020212     C                   clear                   TBElin
029500020913     C                   movel     V0Sif         TBEsif
029600131128     C     K05TBE01      chain(N)  TNTBE01L
029700020212     C                   if        not %found(TNTBE01L)
029800020212     C                   clear                   TBEsif
029900131128     C     K05TBE01      chain(N)  TNTBE01L
030000020212     C                   endif
030100020212     C                   if        %found(TNTBE01L)
030200020212     C                   movel     TNTBEds       xTNTBEds
030300020212     C                   else
030400020212     C                   clear                   xTNTBEds
030500020212     C                   endif
030600090320      *
030700090320      * Aggancio dati generali della tabella SCK
030800090320     C                   clear                   TBEcod
030900090320     C                   move      *zeros        TBEke1
031000090320     C                   move      'SCK'         TBEke1
031100090320     C                   clear                   TBEke2
031200090320     C                   clear                   TBElin
031300090320     C                   movel     V0Sif         TBEsif
031400131128     C     K05TBE01      chain(N)  TNTBE01L
031500090320     C                   if        not %found(TNTBE01L)
031600090320     C                   clear                   TBEsif
031700131128     C     K05TBE01      chain(N)  TNTBE01L
031800090320     C                   endif
031900090320     C                   if        %found(TNTBE01L)
032000090320     C                   movel     TNTBEds       yTNTBEds
032100090320     C                   else
032200090320     C                   clear                   yTNTBEds
032300090320     C                   endif
032400020913      *
032500020913      * Passaggio alla videata di dettaglio
032600020913if  1C                   if        not *in99
032700020913     C                   eval      $CarV1 = *on
032800170120     C                   eval      PrvVid = '0'
032900170119     C                   eval      TipVid = '1'
033000020913e   1C                   endif
033100020212      *
033200101005     C                   ENDSR
033300020212
033400020913      *---------------------------------------------------------------*
033500020913      * CARV0  - Caricamento dati videata S.I.                        *
033600020913      *---------------------------------------------------------------*
033700020913     C     CARV0         BEGSR
033800020913      *
033900020913     C                   movea     *zeros        *in(50)
034000020913     C                   movea     '00000'       *in(01)
034100161213     C                   eval      *in06 = *off
034200020913     C                   clear                   TB28V0
034300020913      *
034400020913     C                   move      Knsif         V0sif
034500020913      *
034600020913     C                   ENDSR
034700020913
034800020913      *---------------------------------------------------------------*
034900020913      * CTRV0  - Controllo videata s.i.                               *
035000020913      *---------------------------------------------------------------*
035100020913     C     CTRV0         BEGSR
035200020913      *
035300020913     C                   movea     *zeros        *in(50)
035400020913     C                   clear                   V0msg
035500020913      *
035600020913      * S.I. obbligatorio
035700020913if  1C                   if        V0SIF = *blanks
035800020913     C                   seton                                        50  99
035900020913     C                   eval      V0msg = Msg(01)
036000101005     C                   leavesr
036100020913e   1C                   endif
036200020913
036300020913      * Deve essere un s.i. valido
036400020913     c     v0sif         chain     kfsif01l
036500020913     c                   if        not %Found(kfsif01l)
036600020913     C                   seton                                        50  99
036700020913     C                   eval      V0msg = Msg(02)
036800101005     C                   leavesr
036900020913     c                   endif
037000020913      *
037100101005     C                   ENDSR
037200020913
037300020913      *---------------------------------------------------------------*
037400020913      * F03V0  - Tasto funzionale F03 -> Fine programma               *
037500020913      *---------------------------------------------------------------*
037600020913     C     F03V0         BEGSR
037700020913      *
037800020913     C                   movel     'S'           $Fine                          fine pgm
037900020913      *
038000020913     C                   ENDSR
038100110603
038200020212      *---------------------------------------------------------------*
038300020212      * GESV1  - Gestione videata selezione codice tabella            *
038400020212      *---------------------------------------------------------------*
038500020212     C     GESV1         BEGSR
038600020212      *
038700020212      * Inizializzazione videata
038800020212if  1C                   if        $CarV1 = *on
038900020212     C                   exsr      CARV1
039000020212     C                   movel     *off          $CarV1
039100020212e   1C                   endif
039200020913      * Scrivo la testata
039300020913     C                   clear                   T1opz
039400020913     C                   write     TB28T1
039500020913      *
039600020913      * Se esistono errori sulla videata
039700020913      * emetto la write del formato a indicatori spenti per vedere
039800020913      * le eventuali decodifiche
039900020913     C                   if        *in99
040000020913     C                   movea     *in           Win
040100020913     C                   movea     *zeros        *in(50)
040200020913     C                   write     TB28V1
040300020913     C                   movea     Win           *in
040400020913     C                   endif
040500020913      *
040600020913     C                   exfmt     TB28V1
040700020913     C                   eval      *in99 = *off
040800020913     C                   clear                   V1MSG
040900020913      *
041000020913sel 1C                   select
041100020913      * F03=Fine
041200020913w   1C                   when      *inKC
041300020913     C                   exsr      F03V0
041400101005     C                   leavesr
041500020913      * F12=Ritorno
041600020913w   1C                   when      *inKL
041700020913     C                   exsr      F12V1
041800101005     C                   leavesr
041900020913e   1C                   endsl
042000020212      *
042100020212      * Ricerche
042200020212     C                   clear                   XX
042300020212     C                   clear                   NMFL
042400020912     C     '?'           scan      V1AZN                                  20
042500020301if  1C                   if        *in20
042600020212     C                   add       1             XX
042700020912     C                   eval      NMFL(XX) = 'V1AZN     '
042800020212     C                   exsr      SEARCH
042900020212e   1C                   endif
043000020212      *
043100020301      * Controllo e decodifiche dati immessi a video
043200020301if  1C                   if        not *in99
043300020212     C                   exsr      CTRV1
043400020301e   1C                   endif
043500020212      *
043600020212      * Passaggio alla videata di dettaglio
043700020212if  1C                   if        not *in99
043800020212     C                   eval      $CarV2 = *on
043900170120      * per definire quale sarà il precedente devo sapere qual è l'attuale
044000170120      * visto che so già che andrò al '2'
044100170120     C                   select
044200170120     c* se ho scritto direttamente il codice azione
044300170120     c                   when      tipvid = '1'
044400170120     c* il video precedente è '1'
044500170120     C                   eval      PrvVid = '1'
044600170120     c* se sono andato in ricerca
044700170120     c                   when      tipvid = '3'
044800170120     c* il video precedente è '3'
044900170120     C                   eval      PrvVid = '3'
045000170120e   1C                   endsl
045100020212     C                   eval      TipVid = '2'
045200020212e   1C                   endif
045300020212      *
045400101005     C                   ENDSR
045500020212
045600020212      *---------------------------------------------------------------*
045700020212      * CARV1  - Caricamento dati prima videata                       *
045800020212      *---------------------------------------------------------------*
045900020212     C     CARV1         BEGSR
046000020212      *
046100020913     C                   movea     *zeros        *in(50)
046200020913     C                   clear                   TB28V1
046300020913
046400020912     C                   move      '?'           V1AZN
046500090320     C                   clear                   V1prg
046600040721     C                   move      V0sif         V1sif
046700110603      *
046800110603     c                   clear                   SavP01ord
046900110603     c                   clear                   SavP01cnd
047000020212      *
047100020212     C                   ENDSR
047200020212
047300020212      *---------------------------------------------------------------*
047400020212      * CTRV1  - Controllo e decodifica prima videata                 *
047500020212      *---------------------------------------------------------------*
047600020212     C     CTRV1         BEGSR
047700020212      *
047800020913     C                   movea     *zeros        *in(50)
047900020212     C                   clear                   V1msg
048000020212      *
048100020912     C                   clear                   V1AZND
048200020912      * Azione obbligatoria
048300020912if  1C                   if        V1AZN = *blanks
048400020913     C                   seton                                        51  99
048500020913     C                   eval      V1msg = Msg(03)
048600101005     C                   leavesr
048700020212x   1C                   else
048800020212      * Controllo/Decodifica codice numeratore
048900020212     C                   reset                   TIBS02DS
049000020913     C                   movel     TbxSif        T02sif
049100170120     C                   movel     V1AZN         T02ke1
049200090320     C                   move      V1prg         T02ke2
049300020212     C                   call      'TIBS02R'
049400020212     C                   parm                    KPJBA
049500020212     C                   parm                    TIBS02DS
049600020212if  2C                   if        T02err = *blanks
049700020912     C                   movel     T02uni        DSCH
049800020912     C                   movel     d§SCHdes      V1AZND
049900020212x   2C                   else
050000020912     C                   clear                   DSCH
050100020212e   2C                   endif
050200020212e   1C                   endif
050300020212      *
050400101005     C                   ENDSR
050500020913
050600020913      *---------------------------------------------------------------*
050700020913      * F12V1  - Tasto funzionale F12 -> Ritorno                      *
050800020913      *---------------------------------------------------------------*
050900020913     C     F12V1         BEGSR
051000020913      *
051100170120        // torno allo '0', precedente allo '0' non c'è niente
051200170120     C                   eval      PrvVid = '0'
051300020913     C                   eval      TipVid = '0'
051400020913      *
051500020913     C                   ENDSR
051600020212
051700020212      *---------------------------------------------------------------*
051800020212      * SEARCH - Ricerche                                             *
051900020212      *---------------------------------------------------------------*
052000020212     C     SEARCH        BEGSR
052100020301      *
052200101007     C*//                eval      *in99 = *on
052300020212      *
052400020212do  1C                   do        10            XX
052500020212      *
052600020212if  2C                   if        NMFL(XX) = *blanks
052700020212     C                   leave
052800020212e   2C                   endif
052900020212      *
053000020212sel 2C                   select
053100020912      * Azione
053200020912w   2C                   when      NMFL(XX) =  'V1AZN     '
053300021114     C                   reset                   Param01
053400021114     C                   movel     TbxSif        P01Sif
053500110603     c                   eval      P01ord = SavP01ord
053600110603     c                   eval      P01cnd = SavP01cnd
053700161212     c                   eval      p01init = wp01init
053800021114     c                   Movel     Param01       Kpjbu
053900021114     C                   call      'TNTB281R'
054000020212     C                   parm                    KPJBA
054100170120       // se sono entrato in ricerca è come fossi nel video '3'
054200170120       tipvid = '3';
054300021114     c                   Movel     Kpjbu         Param01
054400161212      * questo dato lo deve impostare questo pgm se passa dalla routine AGGTBE
054500161212     c                   clear                   p01init
054600161212     c                   clear                   wp01init
054700110603     c                   eval      SavP01ord = P01ord
054800110603     c                   eval      SavP01cnd = P01cnd
054900021114if  4C                   if        P01ke1 <> *blanks
055000021114     C                   movel     P01ke1        V1AZN
055100080721     C                   movel     P01ke2        V1PRG
055200020212x   3C                   else
055300020912     C                   clear                   V1AZND
055400101018     C                   eval      *in99 = *on
055500020212e   3C                   endif
055600110603     C                   select
055700110603     C                   when      P01rit = 'C'
055800170120        // torno allo '0', precedente allo '0' non c'è niente
055900170120     C                   eval      PrvVid = '0'
056000110603     C                   eval      TipVid = '0'
056100110603     C                   eval      $Fine = 'S'
056200110603     C                   leavesr
056300110603     C                   when      P01rit = 'L'
056400170120        // torno allo '1', precedente allo '1' c'è solo lo '0'
056500170120     C                   eval      PrvVid = '0'
056600110603     C                   eval      TipVid = '1'
056700110603     C                   iter
056800110603     C                   endsl
056900080721      *
057000020212e   2C                   endsl
057100020212      *
057200020212e   1C                   enddo
057300020212      *
057400020212     C                   ENDSR
057500110603
057600110603      /free
057700110603
057800110603       //--------------------------------------------------------------
057900110603       //?GESV1N - Gestione videata selezione codice tabella (RICERCA) ?
058000110603       //--------------------------------------------------------------
058100110603       BEGSR  GesV1N;
058200110603
058300110603         // -?Forzatura ricerca codice azione?
058400110603         clear  V1AZN;
058500110603         clear  V1PRG;
058600110603         clear  V1AZND;
058700110603         xx = 1;
058800110603         clear  NMFL;
058900110603         NMFL(XX) = 'V1AZN     ';
059000110603         exsr  SEARCH;
059100110603         select;
059200110603           when  P01rit <> *blank;
059300110603             leavesr;
059400110603           when  *in99;
059500110603             $CarV1 = *on;
059600170120        // torno allo '1', precedente allo '1' c'è solo lo '0'
059700170120             PrvVid = '0';
059800110603             TipVid = '1';
059900110603         endsl;
060000110603
060100110603         // -?Controllo e decodifica azione selezionata?
060200110603         exsr  CTRV1;
060300110603
060400110603         // -?Passaggio alla videata di dettaglio?
060500110603         if  not *in99;
060600110603           $CarV2 = *on;
060700170120        // come precedente c'è il video dove sono adesso (l''1')
060800170120      * per definire quale sarà il precedente devo sapere qual è l'attuale
060900170120      * visto che so già che andrò al '2'
061000170120     C                   select
061100170120     c* se ho scritto direttamente il codice azione
061200170120     c                   when      tipvid = '1'
061300170120     c* il video precedente è '1'
061400170120     C                   eval      PrvVid = '1'
061500170120     c* se sono andato in ricerca
061600170120     c                   when      tipvid = '3'
061700170120     c* il video precedente è '3'
061800170120     C                   eval      PrvVid = '3'
061900170120e   1C                   endsl
062000110603           TipVid = '2';
062100110603         endif;
062200110603
062300110603       ENDSR;
062400110603
062500110603      /end-free
062600110603
062700020212      *---------------------------------------------------------------*
062800020212      * GESV2  - Gestione videata dettaglio dati                      *
062900020212      *---------------------------------------------------------------*
063000020212     C     GESV2         BEGSR
063100020212      *
063200020212      * Inizializzazione videata
063300020212     C                   if        $CarV2 = *on
063400020212     C                   exsr      CarV2
063500020212     C                   move      *off          $CarV2
063600020212     C                   endif
063700020212      * Scrivo la testata
063800020912     C                   write     TB28T1
063900020212      *
064000020212      * Se esistono errori sulla videata
064100020212      * emetto la write del formato a indicatori spenti per vedere
064200020212      * le eventuali decodifiche
064300020212     C                   if        *in99
064400020212     C                   movea     *in           Win
064500020913     C                   movea     *zeros        *in(50)
064600020912     C                   write     TB28V2
064700020212     C                   movea     Win           *in
064800020212     C                   endif
064900020212      *
065000020212if  1C                   if        *in05
065100161213if  1C                             or *in06
065200020912     C                   write     TB28V2
065300020212     C                   exfmt     PROTECT
065400020212x   1C                   else
065500020912     C                   exfmt     TB28V2
065600020212e   1C                   endif
065700020212     C                   eval      *in99 = *off
065800020301     C                   clear                   V2MSG
065900020301     C                   clear                   wTasto
066000020212      *
066100020212sel 1C                   select
066200020212      * F03=Fine
066300020212w   1C                   when      *inKC
066400020913     C                   exsr      F03V0
066500101005     C                   leavesr
066600020212      * F12=Ritorno
066700020212w   1C                   when      *inKL
066800020913     C                   exsr      F12V2
066900101005     C                   leavesr
067000080718      * F07=Immagine KPJBU
067100080718w   1C                   when      *inKG
067200080721     C                   exsr      F07V2
067300020212e   1C                   endsl
067400020212      *
067500020212      * Controllo dati immessi a video
067600101005     C                   exsr      CTRV2
067700020212      *
067800020212      * Aggiornamento se non ci sono errori
067900020212if  1C                   if        not *in99
068000020212     C                             and (*inKF or *inKE or *inKQ)
068100020301sel 2C                   select
068200020301w   2C                   when      *inKE
068300020301     C                   eval      wTasto = '05'
068400020301w   2C                   when      *inKF
068500020301     C                   eval      wTasto = '06'
068600020301w   2C                   when      *inKQ
068700020301     C                   eval      wTasto = '16'
068800020301e   2C                   endsl
068900020226     C                   eval      $CarW1 = *on
069000170120        // da 'A' posso tornare solo a '2', io mi devo tenere a mente il precedente del '2'
069100170120        // che è statop impostato prima
069200020226     C                   eval      TipVid = 'A'
069300020212e   1C                   endif
069400020212      *
069500101005     C                   ENDSR
069600020212
069700020212      *---------------------------------------------------------------*
069800020212      * CARV2  - Caricamento dati seconda videata                     *
069900020212      *---------------------------------------------------------------*
070000020212     C     CARV2         BEGSR
070100020212      *
070200020212     C                   clear                   T1opz
070300020212     C                   movea     '00000'       *in(01)
070400161213     C                   eval      *in06 = *off
070500040721     C                   move      V0sif         V2sif
070600090319      *
070700090319     C                   clear                   trulkpjds
070800020212      *
070900020212      * Aggancio la tabella, se trovo il codice sono in modifica
071000020212      * o ripristino (se record annullato), altrimenti in immissione
071100020212     C                   exsr      CHNTBE
071200020212      *
071300101005sel 1c                   SELECT
071400101005      *
071500101005      * MODIFICA/RIPRISTINO
071600101005w   1c                   WHEN      %found(TNTBE01L)  and  P01opz <> '3'
071700161213w   1c                             and %found(TNTBE01L)  and  P01opz <> '4'
071800020913     C                   movel     TBEuni        DSCH
071900020212if  2C                   if        TBEatb = *blanks
072000020212     C                   eval      *in02  = *on
072100020212     C                   eval      T1opz  = Opz(02)
072200020212x   2C                   else
072300020212     C                   eval      *in04  = *on
072400020212     C                   eval      T1opz  = Opz(06)
072500020212e   2C                   endif
072600020212      *
072700101005      * COPIA
072800101005w   1c                   WHEN      %found(TNTBE01L)  and  P01opz = '3'
072900101005     c                   movel     TBEuni        DSCH
073000101005     C                   eval      *in01  = *on
073100101005     c                   eval      T1opz  = Opz(03)
073200161213      *
073300161213      * ANNULLAMENTO
073400161213w   1c                   WHEN      %found(TNTBE01L)  and  P01opz = '4'
073500161213     c                   movel     TBEuni        DSCH
073600161213     C                   eval      *in06  = *on
073700161213     c                   eval      T1opz  = Opz(04)
073800020212      *
073900101005      * IMMISSIONE
074000101005w   1C                   WHEN      NOT %found(TNTBE01L)
074100020913     c                   clear                   DSCH
074200020212     C                   eval      *in01  = *on
074300020212     C                   eval      T1opz  = Opz(01)
074400020212      *
074500101005e   1C                   ENDSL
074600101005      *
074700090505      * Aggancio il file delle azioni per recuperare la descrizione
074800090505     c     v1azn         chain     kfazn11l
074900090505     c                   if        not %found(kfazn11l)
075000090505     c                   clear                   kdsaz
075100090505     c                   endif
075200020212      *
075300020912     C                   move      V1AZN         V2AZN
075400101005     c                   IF        P01opz = '3'
075500101005     C                   clear                   V2prg
075600101005     C                   ELSE
075700080721     C                   move      V1PRG         V2PRG
075800101005     C                   ENDIF
075900021025     C                   eval      V2prio = d§SCHpri
076000021025     C                   eval      V2AZND = d§SCHdes
076100020912     C                   eval      V2ESE  = d§SCHese
076200060426     C**                 eval      V2FRE  = d§SCHfre
076300121008     C                   eval      V2Sosp = d§SCHSosp
076400121009     C* se l'azione è sospesa metto in reverse image la costante
076500121009     C                   SETOFF                                       21
076600121009     C                   IF        V2Sosp = 'S'
076700121009     C                   SETON                                        21
076800121009     C                   ENDIF
076900090505     c                   eval      v2dazn = kdsaz
077000060426     c                   clear                   v2gior
077100060426     c                   clear                   v2sett
077200060426     c                   clear                   v2mens
077300090319      *
077400060426     c                   select
077500060426     c                   when      d§schfre='G'
077600060426     c                   eval      v2gior='SI'
077700060426     c                   when      d§schfre='S'
077800060426     c                   eval      v2sett='SI'
077900060426     c                   when      d§schfre='M'
078000060426     c                   eval      v2mens='SI'
078100060426     c                   endsl
078200090319      *
078300050222     c                   if        alfschggm>=*zeros
078400050215     C                   eval      V2ggm  = d§SCHggm
078500050222     c                   else
078600050222     c                   clear                   v2ggm
078700050222     c                   endif
078800060427     c                   if        d§schg1s<>' '
078900060427     c                   movel     d§schg1s      cc
079000060427     C                   eval      V2g1s  = giorset(CC)
079100060427     c                   else
079200060427     c                   clear                   v2g1s
079300060427     c                   endif
079400060426     c* Per frequenza settimanale, se non impostato, imposto SAB
079500060426     c                   if        v2sett='SI' and v2g1s='   '
079600060426     c                   eval      v2g1s='SAB'
079700060426     c                   endif
079800090319      *
079900060427     c                   if        d§schg1g<>' '
080000060427     c                   movel     d§schg1g      cc
080100060427     C                   eval      V2g1g  = giorset(cc)
080200060427     c                   else
080300060427     c                   clear                   v2g1g
080400060427     c                   endif
080500090319      *
080600060427     c                   if        d§schg2g<>' '
080700060427     c                   movel     d§schg2g      cc
080800060427     C                   eval      V2g2g  = giorset(cc)
080900060427     c                   else
081000060427     c                   clear                   v2g2g
081100060427     c                   endif
081200090319      *
081300060427     c                   if        d§schg3g<>' '
081400060427     c                   movel     d§schg3g      cc
081500060427     C                   eval      V2g3g  = giorset(cc)
081600060427     c                   else
081700060427     c                   clear                   v2g3g
081800060427     c                   endif
081900060427     c                   if        d§schg4g<>' '
082000060427     c                   movel     d§schg4g      cc
082100060427     C                   eval      V2g4g  = giorset(cc)
082200060427     c                   else
082300060427     c                   clear                   v2g4g
082400060427     c                   endif
082500060427     c                   if        d§schg5g<>' '
082600060427     c                   movel     d§schg5g      cc
082700060427     C                   eval      V2g5g  = giorset(cc)
082800060427     c                   else
082900060427     c                   clear                   v2g5g
083000060427     c                   endif
083100060427     c                   if        d§schg6g<>' '
083200060427     c                   movel     d§schg6g      cc
083300060427     C                   eval      V2g6g  = giorset(cc)
083400060427     c                   else
083500060427     c                   clear                   v2g6g
083600060427     c                   endif
083700060427     c                   if        d§schg7g<>' '
083800060427     c                   movel     d§schg7g      cc
083900060427     C                   eval      V2g7g  = giorset(cc)
084000060427     c                   else
084100060427     c                   clear                   v2g7g
084200060427     c                   endif
084300060426     C                   eval      V2JOB  = d§SCHjob
084400020912     C                   eval      V2OR1  = D§SCHor1
084500060426     c                   if        v2or1>*zeros
084600060426     c                   seton                                        84
084700060426     c                   endif
084800020912     C                   eval      V2OR2  = D§SCHor2
084900060426     c                   if        v2or2>*zeros
085000060426     c                   seton                                        85
085100060426     c                   endif
085200020912     C                   eval      V2OR3  = D§SCHor3
085300060426     c                   if        v2or3>*zeros
085400060426     c                   seton                                        86
085500060426     c                   endif
085600020912     C                   eval      V2OR4  = D§SCHor4
085700060426     c                   if        v2or4>*zeros
085800060426     c                   seton                                        87
085900060426     c                   endif
086000020912     C                   eval      V2OR5  = D§SCHor5
086100060426     c                   if        v2or5>*zeros
086200060426     c                   seton                                        88
086300060426     c                   endif
086400020912     C                   eval      V2OR6  = D§SCHor6
086500060426     c                   if        v2or6>*zeros
086600060426     c                   seton                                        89
086700060426     c                   endif
086800020912     C                   eval      V2OR7  = D§SCHor7
086900060426     c                   if        v2or7>*zeros
087000060426     c                   seton                                        90
087100060426     c                   endif
087200020912     C                   eval      V2OR8  = D§SCHor8
087300060426     c                   if        v2or8>*zeros
087400060426     c                   seton                                        91
087500060426     c                   endif
087600020912     C                   eval      V2OR9  = D§SCHor9
087700060426     c                   if        v2or9>*zeros
087800060426     c                   seton                                        92
087900060426     c                   endif
088000020912     C                   eval      V2OR10 = D§SCHor10
088100060426     c                   if        v2or10>*zeros
088200060426     c                   seton                                        93
088300060426     c                   endif
088400020912     C                   eval      V2OR11 = D§SCHor11
088500060426     c                   if        v2or11>*zeros
088600060426     c                   seton                                        94
088700060426     c                   endif
088800020912     C                   eval      V2OR12 = D§SCHor12
088900060426     c                   if        v2or12>*zeros
089000060426     c                   seton                                        95
089100060426     c                   endif
089200020912     C                   eval      V2OR13 = D§SCHor13
089300060426     c                   if        v2or13>*zeros
089400060426     c                   seton                                        96
089500060426     c                   endif
089600020912     C                   eval      V2OR14 = D§SCHor14
089700060426     c                   if        v2or14>*zeros
089800060426     c                   seton                                        97
089900060426     c                   endif
090000020912     C                   eval      V2OR15 = D§SCHor15
090100060426     c                   if        v2or15>*zeros
090200060426     c                   seton                                        98
090300060426     c                   endif
090400020912     C                   eval      V2TPS  = D§SCHtps
090500020912     C                   eval      V2UTE  = D§SCHute
090600090320      /free
090700090320
090800090320         OkpjKPJBU = k_TBEuni;
090900090320
091000101005         // - Se ricevuto campo opzione "3" (da TNTB281R):
091100101005         //   è stata richiesta una "COPIA"
091200101005         *in43 = (P01opz = '3');
091300101005
091400090320         //// - Se V2PRG è valorizzato => è stata impostata l'immagine KPJBU
091500090320         //*in44 = (V2prg <> *blank);
091600090320         // - Se reperita la tab. "SCK" corrispondente
091700090320         //   => è stata impostata l'immagine KPJBU
091800090320         *in44 = (k_TBEuni <> *blanks);
091900090320
092000090320      /end-free
092100090320      *
092200020212     C                   ENDSR
092300080721
092400080721      *---------------------------------------------------------------*
092500080721      * F07V2  - Tasto funzionale F07 -> Immagine KPJBU               *
092600080721      *---------------------------------------------------------------*
092700080721     C     F07V2         BEGSR
092800080721      *
092900080721     C                   call      'TRULKPJR'
093000090319     C                   parm                    kpjba
093100080721     C                   parm                    trulkpjds
093200090319      *
093300090319     C                   eval      *in44 = (OkpjKPJBU <> *blanks)
093400080721      *
093500080721     C                   ENDSR
093600020913
093700020913      *---------------------------------------------------------------*
093800020913      * F12V2  - Tasto funzionale F12 -> Ritorno                      *
093900020913      *---------------------------------------------------------------*
094000020913     C     F12V2         BEGSR
094100020913      *
094200060427     c                   eval      $carv1=*on
094300170119     C***                eval      TipVid = '3'
094400170120       // torno dove mi sono salvato (o ricerca '3' o inserimento codice '1')
094500170119     C                   eval      TipVid = PrvVid
094600170120      * per definire quale sarà il precedente devo sapere dove tornerò
094700170120     C                   select
094800170120     c* se tornerò al codice azione
094900170120     c                   when      tipvid = '1'
095000170120     c* il video precedente è '0'
095100170120     C                   eval      PrvVid = '0'
095200170120     c* se tornerò alla ricerca
095300170120     c                   when      tipvid = '3'
095400170120     c* il video precedente è sempre la ricerca '3'
095500170120     C                   eval      PrvVid = '3'
095600161213      * se torno al pgm TNTB281R gli dico di non ricaricare il sfl
095700161213     C                   movel     'R'           wp01init
095800170120e   1C                   endsl
095900021114     c                   Unlock    Tntbe01l
096000020913      *
096100020913     C                   ENDSR
096200020212
096300020212      *---------------------------------------------------------------*
096400020212      * CTRV2  - Controllo e decodifica seconda videata               *
096500020212      *---------------------------------------------------------------*
096600020212     C     CTRV2         BEGSR
096700020212      *
096800020913     C                   movea     *zeros        *in(50)
096900020212     C                   clear                   V2msg
097000020212      *
097100020212      * Non si fanno i controlli se richisto l'annullamento
097200020212     C     *inKQ         cabeq     *on           EndCtrV2
097300101005      *
097400101005      * (Ri)Aggancio la tabella se sono in copia
097500101005      * (se è stato modificato il codice chiave)
097600101005if  1c                   IF        P01opz =  '3'
097700101005if  2c                   if        TBEke1 <> V2azn   or
097800101005     c                             TBEke2 <> V2prg
097900101005     c                   eval      V1azn  =  V2azn
098000101005     c                   eval      V1prg  =  V2prg
098100101005     c                   exsr      CHNTBE
098200101005e   2c                   endif
098300101005      * In COPIA NON può già esistere il record
098400101005if  2c                   if        %found(TNTBE01L)
098500101005     c                   seton                                        51  99
098600101005     c                   movel     Msg(26)       V2msg
098700101005     c                   leavesr
098800101005e   2c                   endif
098900101005e   1c                   ENDIF
099000020913      *
099100020913      * Passo gli orari nella schiera
099200020913     c                   move      v2or1         ora(1)
099300020913     c                   move      v2or2         ora(2)
099400020913     c                   move      v2or3         ora(3)
099500020913     c                   move      v2or4         ora(4)
099600020913     c                   move      v2or5         ora(5)
099700020913     c                   move      v2or6         ora(6)
099800020913     c                   move      v2or7         ora(7)
099900020913     c                   move      v2or8         ora(8)
100000020913     c                   move      v2or9         ora(9)
100100020913     c                   move      v2or10        ora(10)
100200020913     c                   move      v2or11        ora(11)
100300020913     c                   move      v2or12        ora(12)
100400020913     c                   move      v2or13        ora(13)
100500020913     c                   move      v2or14        ora(14)
100600020913     c                   move      v2or15        ora(15)
100700020313      *
100800020212      * Descrizione obbligatoria
100900051107if  1C                   If        V2aznd =  *blanks
101000020913     C                   seton                                        52  99
101100020913     C                   movel     Msg(04)       V2msg
101200051107     C                   leavesr
101300051107e   1c                   endif
101400020212      *
101500020912      * Flag esecuzione obbligatorio
101600051107if  1C                   If        V2ese = *blanks
101700020913     C                   seton                                        53  99
101800020913     C                   movel     Msg(05)       V2msg
101900051107     C                   leavesr
102000051107e   1c                   endif
102100040708      * Non si può indicare esecuzione 'S' in ambiente P.O. e viceversa
102200040708      *                     esecuzione 'P' in ambiente SEDE
102300051107if  1C                   If        (V2ese = 'S'
102400040721     c                             and %subst(V0SIF:1:6) <> 'GAITRA')
102500040708     c                             or
102600051107     C                             (V2ese = 'P'
102700040721     c                             and %subst(V0SIF:1:6) <> 'FILTRA')
102800020913     C                   seton                                        53  99
102900020913     C                   movel     Msg(06)       V2msg
103000040721     C                   eval      V2msg = %trim(V2msg) + ' ' + V0SIF
103100051107     C                   leavesr
103200051107e   1c                   endif
103300020212      *
103400020912      * Flag frequenza
103500060426if  1C***                If        V2fre  = *blanks
103600060426     c                   if        v2gior='  ' and v2sett='  ' and v2mens='  '
103700020913     C                   seton                                        54  99
103800020913     C                   movel     Msg(07)       V2msg
103900051107     C                   leavesr
104000051107e   1c                   endif
104100060426     c*
104200060426     c                   if        (v2gior<>'  ' and v2sett<>'  ')
104300060426     c                             or (v2gior<>'  ' and v2mens<>'  ')
104400060426     c                             or (v2mens<>'  ' and v2sett<>'  ')
104500060426     C                   movel     Msg(18)       V2msg
104600101005     c*
104700060426     c                   if        v2gior<>'  '
104800060426     c                   seton                                        5499
104900060426     c                   else
105000060426     c                   seton                                        5799
105100060426     c                   endif
105200101005     c*
105300060426     C                   leavesr
105400060426     c                   endif
105500060426     C
105600060426     c* Per frequenza giornaliera
105700060426     c                   if        v2gior='  '
105800060426     c                   clear                   pieno             1
105900060426     c                   do        7             gg
106000060426    3c                   if        ggGiorn(GG)<>'   '
106100060426     c                   eval      pieno='S'
106200060426     c                   endif
106300060426     c                   enddo
106400101005     c*
106500060426     c                   if        pieno='S'
106600060426     c                   seton                                        7699
106700060426     C                   movel     Msg(21)       V2msg
106800060426     C                   leavesr
106900060426e   1c                   endif
107000060426e   1c                   endif
107100101005     C*
107200060426    1c                   if        v2gior='SI'
107300060426     c                   clear                   giaimm
107400060426     c                   clear                   xx
107500060426    2c                   do        7             gg                3 0
107600060426     C                   Z-ADD     75            YY
107700060426    3c                   if        ggGiorn(GG)<>'   '
107800060426     c     ggGiorn(GG)   lookup    GIORSET                                30
107900060426    4C                   IF        NOT *IN30
108000060426     c                   add       GG            yy
108100060426     C                   eval      *in(yy) = *On
108200060426     c                   seton                                        99
108300060426     C                   movel     Msg(22)       V2msg
108400060426     C                   leavesr
108500060426e   4c                   endif
108600060426     c* Verifico se già immesso
108700060426     c     ggGiorn(gg)   lookup    giaimm                                 30
108800060426    4C                   IF        *IN30
108900060426     c                   add       GG            yy
109000060426     C                   eval      *in(yy) = *On
109100060426     c                   seton                                        99
109200060426     C                   movel     Msg(23)       V2msg
109300060426     C                   leavesr
109400060426   x4c                   else
109500060426     c                   add       1             xx
109600060426     c                   eval      giaimm(xx)=ggGiorn(GG)
109700060426e   4c                   endif
109800101005     c*
109900060426e   3c                   endif
110000060426e   2c                   endDO
110100060426e   1c                   endif
110200060426     c* Frequenza settimanale
110300060426     c                   if        v2sett='  ' and v2g1s<>*blanks
110400060426     c                   seton                                        8399
110500060426     C                   movel     Msg(24)       V2msg
110600060426     C                   leavesr
110700060426e   1c                   endif
110800101005     c*
110900060426    1c                   if        v2sett='SI'
111000060426     c     v2g1s         lookup    GIORSET                                30
111100060426    2C                   IF        NOT *IN30
111200060426     c                   seton                                        8399
111300060426     C                   movel     Msg(22)       V2msg
111400060426     C                   leavesr
111500060426e   2c                   endif
111600060426e   1c                   endif
111700101005     c*
111800060426     c* Frequenza Mensile
111900060426     c                   if        v2mens='SI'  and v2ggm=0
112000060426     c                   seton                                        7599
112100060426     C                   movel     Msg(19)       V2msg
112200060426     C                   leavesr
112300051107e   1c                   endif
112400060426     c                   if        v2mens='  '  and v2ggm>0
112500060426     c                   seton                                        7599
112600060426     C                   movel     Msg(20)       V2msg
112700060426     C                   leavesr
112800060426e   1c                   endif
112900060426    c
113000051107      *
113100051107      * Coda lavori forzata
113200051107if  1c                   if        V2job <> *blanks
113300051107     c                   eval      Qcmd    =  'CHKOBJ OBJ('
113400051107     c                                     +  %trimr(V2job)
113500051107     c                                     +  ') OBJTYPE(*JOBQ)'
113600051107     c                   call      'QCMDEXC'                            99
113700051107     c                   parm                    Qcmd
113800051107     c                   parm                    Qlen
113900051107if  2c                   if        *in99
114000051107     C                   seton                                        58  99
114100051107     C                   movel     Msg(17)       V2msg
114200051107     c                   leavesr
114300051107e   2c                   endif
114400051107e   1c                   endif
114500101005     c*
114600060426     c* La coda in notte la accetto solo per schedulazione Settimanale
114700091123     c* nel giorno di sabato o domenica
114800060426     c     'NOTTE'       scan      v2job                                  30
114900060428     c                   if        *in30
115000060428     c                   if        v2mens='SI'   or
115100091123     c                             (v2sett='SI' and v2g1s='SAB') or
115200091123     c                             (v2sett='SI' and v2g1s='DOM')
115300060428     c                   else
115400060426     C                   seton                                        58  99
115500060426     C                   movel     Msg(25)       V2msg
115600060426     C                   leavesr
115700060426e   1c                   endif
115800060428e   1c                   endif
115900020212      *
116000020912      * Flag tipo sottomissione
116100051107if  1C                   if             V2tps  = 'M'
116200111118     C                             and %subst(v0sif:1:6) <> 'FILTRA'
116300020913     C                   seton                                        55  99
116400020913     C                   movel     Msg(08)       V2msg
116500051107     C                   leavesr
116600051107e   1c                   endif
116700020215      *
116800020913      * Profilo obbligatorio
116900051107if  1C                   if        V2ute  = *blanks
117000020913     C                   seton                                        56  99
117100020913     C                   movel     Msg(09)       V2msg
117200051107     C                   leavesr
117300051107e   1c                   endif
117400020918      * Deve esistere nel file profili del modulo base
117500020918     c     V2ute         Chain     Kpprf01l
117600051107if  1c                   if        not %Found(kpprf01l)
117700020918     c                   seton                                        56  99
117800020918     c                   movel     Msg(15)       V2msg
117900051107     c                   leavesr
118000051107e   1c                   endif
118100020913      * nel s.i. FILTRA è possibile solo EDPCEDXXX
118200051107if  1C                   if            v2ute <> 'EDPCEDXXX'
118300021028     c                             and %subst(v0Sif:1:6) = 'FILTRA'
118400020913     C                   seton                                        56  99
118500020913     C                   movel     Msg(10)       V2msg
118600051107     C                   leavesr
118700051107e   1c                   endif
118800020913      * il profilo EDPCEDXXX è valido solo nel s.i. FILTRA
118900051107if  1C                   if            v2ute =  'EDPCEDXXX'
119000021028     c                             and %subst(v0Sif:1:6) <> 'FILTRA'
119100020913     C                   seton                                        56  99
119200020913     C                   movel     Msg(11)       V2msg
119300051107     C                   leavesr
119400051107e   1c                   endif
119500020913      * Per sottomissioni multiple è consentito solo l'utente EDPCEDXXX
119600051107if  1C                   if            v2ute  <> 'EDPCEDXXX'
119700020913     c                             and v2tps  = 'M'
119800020913     C                   seton                                        56  99
119900020913     C                   movel     Msg(12)       V2msg
120000051107     C                   leavesr
120100051107e   1c                   endif
120200020913      *
120300060426      * Orari obbligatori solo x job giornalieri/mensili
120400060426      *  o settimanali e coda frozata <> da ksnotte
120500020913     c                   xfoot     ora           totora
120600060426     c     'NOTTE'       scan      v2job                                  30
120700051107if  1c                   if        Totora = *zeros and
120800060426     c                             not *in30
120900020913     C                   seton                                        60  99
121000020913     C                   movel     Msg(13)       V2msg
121100051107     C                   leavesr
121200051107e   1c                   endif
121300051107if  1c                   if        Totora <> *zeros and
121400060426     c                             *in30
121500091123     c* In KNOTTESK è possibile indicare gli orari
121600091123     c                   if        v2job<>'KSNOTTESK'
121700021025     C                   seton                                        60  99
121800021025     C                   movel     Msg(16)       V2msg
121900051107     C                   leavesr
122000051107e   1c                   endif
122100091123e   1c                   endif
122200020913      * controllo l'ora
122300051107do  1c                   do        15            xx
122400020913     c                   z-add     59            yy
122500060426     c                   z-add     83            zz
122600020913     c                   movel     ora(xx)       Wora
122700020913     c                   move      ora(xx)       Wminuti
122800051107if  2C                   if        Wora  >= 24 or Wminuti >= 60
122900020913     C                   seton                                            99
123000020913     c                   add       xx            yy
123100060426     c                   add       xx            zz
123200020913     C                   eval      *in(yy) = *On
123300060426     C                   eval      *in(zz) = *On
123400020913     C                   movel     Msg(14)       V2msg
123500051107     C                   leavesr
123600060426   x2c                   else
123700060426    3c                   if        ora(xx)>0
123800060426     c                   add       xx            zz                3 0
123900060426     C                   eval      *in(zz) = *On
124000060426e   3C                   endif
124100060426e   2C                   endif
124200051107e   1c                   enddo
124300020212      *
124400020214     C     *in99         cabeq     *on           EndCtrV2
124500020212      *
124600020212     C     EndCtrV2      ENDSR
124700020212
124800020226      *---------------------------------------------------------------*
124900020226      * GESW1  - Gestione videata dati relativi alla trasmissione     *
125000020226      *---------------------------------------------------------------*
125100020226     C     GESW1         BEGSR
125200020226      *
125300020226      * Inizializzazione videata
125400020226if  1C                   if        $CarW1 = *on
125500020226     C                   exsr      CARW1
125600020226     C                   movel     *off          $CarW1
125700020226e   1C                   endif
125800020226      *
125900020226if  1C                   if        *in05
126000020912     C                   write     TB28W1
126100020226     C                   exfmt     PROTECT
126200020226x   1C                   else
126300020912     C                   exfmt     TB28W1
126400020226e   1C                   endif
126500020226     C                   eval      *in99 = *off
126600020301     C                   clear                   W1MSG
126700020226      *
126800020226sel 1C                   select
126900020226      * F12=Ritorno
127000020226w   1C                   when      *inKL
127100020226     C                   exsr      F12W1
127200101005     C                   leavesr
127300020226e   1C                   endsl
127400020226      *
127500020226      * Controllo dati immessi a video
127600020226     C                   exsr      CTRW1
127700020226      *
127800020226      * Aggiornamento se non ci sono errori
127900020301if  1C                   if        not *in99 and *inKF
128000020226     C                   exsr      AGGTBE
128100020226e   1C                   endif
128200020226      *
128300101005     C                   ENDSR
128400020313
128500020226      *---------------------------------------------------------------*
128600020226      * CARW1  - Caricamento dati window                              *
128700020226      *---------------------------------------------------------------*
128800020226     C     CARW1         BEGSR
128900020226      *
129000020226     C                   movea     *zeros        *in(50)
129100020226      *
129200020226sel 1C                   select
129300020226      *
129400020226      * F5=Ripristino
129500020226w   1C                   when      *inKE   and  *in04
129600020226     C                   eval      W1FTT = TBEftt
129700020226      *
129800020226      * F6=Conferma
129900020226w   1C                   when      *inKF
130000020226sel 2C                   select
130100020226      *   Immissione
130200020226w   2C                   when      *in01
130300020313     C                   eval      W1FTT = TBXftt
130400020226      *   Modifica / Ripristino
130500020226w   2C                   when      *in02   or    *in04
130600020313     C                   eval      W1FTT = TBEftt
130700020226e   2C                   endsl
130800020226      *
130900020226      * F16=Annullamento
131000020226w   1C                   when      *inKQ   and  not *in04
131100020313     C                   eval      W1FTT = TBEftt
131200020226      *
131300020226e   1C                   endsl
131400020226      *
131500020226      * Se NON immissione: visualizzo i dati relativi all'ultima
131600020226      *   trasmissione
131700020226if  1C                   if        not *in01
131800020226     C                   eval      W1FLT = TBEflt
131900020226     C                   eval      W1FTR = TBEftr
132000020226if  2C                   if        TBEdtr <> 0
132100020226     C                   clear                   WLBDAT
132200020226     C                   z-add     TBEdtr        G02inv
132300020226     C                   movel     '3'           G02err
132400020226     C                   call      'XSRDA8'
132500020226     C                   parm                    WLBDAT
132600020226     C                   z-add     G02dat        W1DTR
132700020226e   2C                   endif
132800020226e   1C                   endif
132900020226      *
133000020226     C                   ENDSR
133100020226
133200020226      *---------------------------------------------------------------*
133300020226      * CTRW1  - Controllo e decodifica window                        *
133400020226      *---------------------------------------------------------------*
133500020226     C     CTRW1         BEGSR
133600020226      *
133700020226     C                   movea     *zeros        *in(50)
133800020226      *
133900020226     C     EndCtrW1      ENDSR
134000020226
134100020226      *---------------------------------------------------------------*
134200020226      * F21W1  - Tasto funzionale F12 -> Ritorno                      *
134300020226      *---------------------------------------------------------------*
134400020226     C     F12W1         BEGSR
134500020226      *
134600170120     C                   eval      TipVid = '2'
134700170120       // il precedente resta quello memorizzato
134800170120     C*???               eval      PrvVid =
134900020226      *
135000020226     C                   ENDSR
135100020212
135200020212      *---------------------------------------------------------------*
135300020212      * CHNTBE * Aggancio tabella                                     *
135400020212      *---------------------------------------------------------------*
135500020212     C     CHNTBE        BEGSR
135600020212      *
135700020212     C                   movel     T02cod        TBEcod
135800020912     C                   movel(p)  V1AZN         TBEke1
135900090320     C                   movel     V1PRG         TBEke2
136000020212     C                   clear                   TBElin
136100020913     C                   movel     tbxsif        TBEsif
136200020212     C     K05TBE01      chain     TNTBE01L
136300020212      * Se non ho trovato il record con il sistema informativo
136400020212      * che ho in linea, lo abblenco
136500020212if  1C                   if        not %found(TNTBE01L)
136600020212     C                   clear                   TBEsif
136700020212     C     K05TBE01      chain     TNTBE01L
136800020212e   1C                   endif
136900090320      /free
137000090320         // Carico anche l'immagine della KPJBU (se già immessa)
137100090320         clear  k_TBEuni;
137200090320         IF  V1prg <> *blank;
137300090320           k_TBEcod = 'SCK';
137400090320           k_TBEke1 = V1prg;
137500090320           clear  k_TBEke2;
137600090320           clear  k_TBElin;
137700090320           k_TBEsif = TBYsif;
137800090320           chain  %kds(keyTBESCK_P)  TNTBESCK;
137900090320         ENDIF;
138000090320      /end-free
138100020212      *
138200020212     C                   ENDSR
138300020212
138400020212      *---------------------------------------------------------------*
138500020212      * AGGTBE * Aggiornamento tabella                                *
138600020212      *---------------------------------------------------------------*
138700020212     C     AGGTBE        BEGSR
138800090423      *
138900090423     c                   eval      $NewKpjbu = *off
139000020212      *
139100020212sel 1C                   select
139200020212      *
139300020212      * F5=Ripristino
139400020301w   1C                   when      wTasto='05'  and  *in04
139500020212     C                   clear                   TBEatb
139600020212     C                   clear                   TBEftr
139700020212     C                   UPDATE    TNTBE000
139800020212      *
139900020212      * F6=Conferma
140000020301w   1C                   when      wTasto='06'
140100080721     C*
140200090320      /free
140300090320         // - Aggiornamento immagine KPJBU
140400090320         select;
140500090320           // - Nessun parametro inserito/variato
140600110922           when  OkpjKPJBU =  *blank    and   *in01;
140700101005           when  OkpjKPJBU =  k_TBEuni  and   P01opz <> '3';
140800090320           // - Inserimento nuovi parametri
140900101005           when  OkpjKPJBU <> *blank   and   (V1prg  = *blank
141000101005                                        or    P01opz = '3');
141100090320             exsr  sr_RepNumSCK;
141200090320             if  *in99;
141300170120       // il precedente resta quello memorizzato
141400170120       //      PrvVid = TipVid;
141500170119               TipVid = '2';
141600090320               leavesr;
141700090320             endif;
141800090320             exsr  sr_RieTabSCK;
141900090320             write  TNTBE00K;
142000090423             $NewKPJBU = *on;
142100090320           // - Re-Inserimento parametri (precedentemente cancellati)
142200090320           when  OkpjKPJBU <> *blank   and   V1prg <> *blank   and
142300090320                 NOT %found(TNTBESCK);
142400090320             exsr  sr_RieTabSCK;
142500090320             write  TNTBE00K;
142600090320           // - Cancellazione parametri esistenti
142700090320           when  OkpjKPJBU = *blank   and   V1prg <> *blank;
142800090320             delete  TNTBE00K;
142900090320           // - Aggiornamento parametri
143000090320           other;
143100090320             k_TBEatb = *blank;
143200090320             k_TBEuni = OkpjKPJBU;
143300090320             clear  k_TBEftr;
143400090320             update  TNTBE00K;
143500090320         endsl;
143600090320       /end-free
143700090319      *
143800020212sel 2C                   select
143900020212      *   Immissione
144000020212w   2C                   when      *in01
144100020212     C                   exsr      RIETBE
144200020226     C                   clear                   TBEflt
144300020226     C                   clear                   TBEdtr
144400020212     C                   WRITE     TNTBE000                             22
144500020212      *   Modifica / Ripristino
144600020212w   2C                   when      *in02   or    *in04
144700020212     C                   exsr      RIETBE
144800020212     C                   UPDATE    TNTBE000
144900020212e   2C                   endsl
145000020212      *
145100020212      * F16=Annullamento
145200020301w   1C                   when      wTasto='16'  and  not *in04
145300090320      /free
145400090320             // - Cancellazione parametri
145500090320             if  V2prg <> *blank;
145600090320               k_TBEatb = 'A';
145700090320               clear  k_TBEftr;
145800090320               update  TNTBE00K;
145900090320             endif;
146000090320      /end-free
146100020212     C                   movel     'A'           TBEatb
146200020212     C                   clear                   TBEftr
146300020212     C                   UPDATE    TNTBE000
146400020212      *
146500020212e   1C                   endsl
146600090423      /free
146700090423         if  $NewKPJBU = *on;
146800090423           clear  TB28V3;
146900090423           V3ke2  = V2prg;
147000090423           exfmt  TB28V3;
147100090423         endif;
147200090423      /end-free
147300020212      *
147400020212      * Torno alla prima videata che carico come da inizio pgm
147500170119     C***                movel     '3'           TipVid
147600170119     C                   movel     PrvVid        TipVid
147700020212     C                   movel     *on           $CarV1
147800020226     C                   movel     *on           $CarV2
147900020226     C                   movel     *on           $CarW1
148000170120     c* se tornerò alla ricerca
148100170120     c                   if        tipvid = '3'
148200170120      * se torno al pgm TNTB281R gli dico di proseguire con la prossima scelta
148300170120     C                   movel     'N'           wp01init
148400170120e   1C                   endif
148500020212     C
148600020212     C                   ENDSR
148700020212
148800020212      *---------------------------------------------------------------*
148900020212      * Riempimento file tabella
149000020212      *---------------------------------------------------------------*
149100020212     C     RIETBE        BEGSR
149200020212      *
149300060427     C                   clear                   numset
149400060427     C                   clear                   TBEatb
149500020212     C                   if        TBXsif <> *blanks
149600020912     C                   movel     TBXsif        TBEsif
149700020212     C                   else
149800020212     C                   clear                   TBEsif
149900020212     C                   endif
150000020212     C                   movel     TBXapl        TBEapl
150100020212     C                   movel     T02cod        TBEcod
150200020913     C                   movel     V2azn         TBEke1
150300090320     C                   move      V2prg         TBEke2
150400020212      *
150500020913     C                   clear                   DSCH
150600021025     C                   eval      d§SCHpri  =  V2prio
150700021025     C                   eval      d§SCHdes  =  V2AZND
150800020913     C                   eval      d§SCHese  =  V2ESE
150900121008     C                   eval      d§SCHSosp =  V2Sosp
151000090319      *
151100060426     c                   select
151200060426     c                   when      v2gior='SI'
151300060426     c                   eval      d§schfre='G'
151400060426     c                   when      v2sett='SI'
151500060426     c                   eval      d§schfre='S'
151600060426     c                   when      v2mens='SI'
151700060426     c                   eval      d§schfre='M'
151800060426     c                   endsl
151900090319      *
152000090319      *
152100060426     C****               eval      d§SCHfre  =  V2FRE
152200050215     C                   eval      d§SCHggm  =  V2ggm
152300060427     c                   z-add     1             cc
152400060427     c     v2g1s         lookup    giorset(cc)                            30
152500060427     c   30              movel     cc            d§schg1s
152600060427     c  n30              clear                   d§schg1s
152700060427     c
152800090319      *
152900060426     c* Ordino i giorni della settimana per frequenza settimanale
153000060426     c                   z-add     1             xx
153100060426     c                   do        7             gg
153200060426     c                   if        gggiorn(GG)<>'   '
153300060426     c                   z-add     1             yy
153400060426     c     ggGiorn(GG)   lookup    giorset(YY)                            30
153500060426     c                   z-add     yy            numset(XX)
153600060426     c                   add       1             xx
153700060426     c                   endif
153800060426     c                   enddo
153900060426     c                   sorta     numset
154000090319      *
154100060426     C                   clear                   d§SCHg1g
154200060426     C                   clear                   d§SCHg2g
154300060426     C                   clear                   d§SCHg3g
154400060426     C                   clear                   d§SCHg4g
154500060426     C                   clear                   d§SCHg5g
154600060426     C                   clear                   d§SCHg6g
154700060426     C                   clear                   d§SCHg7g
154800060426     c                   do        7             xx
154900060426     c                   if        numset(XX)>0
155000060426     c                   if        d§schg1g='   '
155100060427     c                   movel     numset(XX)    d§schg1g
155200060426     c                   iter
155300060426     c                   endif
155400060426     c                   if        d§schg2g='   '
155500060427     c                   movel     numset(XX)    d§schg2g
155600060426     c                   iter
155700060426     c                   endif
155800060426     c                   if        d§schg3g='   '
155900060427     c                   movel     numset(XX)    d§schg3g
156000060426     c                   iter
156100060426     c                   endif
156200060426     c                   if        d§schg4g='   '
156300060427     c                   movel     numset(XX)    d§schg4g
156400060426     c                   iter
156500060426     c                   endif
156600060426     c                   if        d§schg5g='   '
156700060427     c                   movel     numset(XX)    d§schg5g
156800060426     c                   iter
156900060426     c                   endif
157000060426     c                   if        d§schg6g='   '
157100060427     c                   movel     numset(XX)    d§schg6g
157200060426     c                   iter
157300060426     c                   endif
157400060426     c                   if        d§schg7g='   '
157500060427     c                   movel     numset(XX)    d§schg7g
157600060426     c                   iter
157700060426     c                   endif
157800060426     c                   endif
157900060426     c                   enddo
158000090319      *
158100020913     C                   eval      d§SCHjob  =  V2JOB
158200020913     C                   eval      D§SCHor1  =  V2OR1
158300020913     C                   eval      D§SCHor2  =  V2OR2
158400020913     C                   eval      D§SCHor3  =  V2OR3
158500020913     C                   eval      D§SCHor4  =  V2OR4
158600020913     C                   eval      D§SCHor5  =  V2OR5
158700020913     C                   eval      D§SCHor6  =  V2OR6
158800020913     C                   eval      D§SCHor7  =  V2OR7
158900020913     C                   eval      D§SCHor8  =  V2OR8
159000020913     C                   eval      D§SCHor9  =  V2OR9
159100020913     C                   eval      D§SCHor10 =  V2OR10
159200020913     C                   eval      D§SCHor11 =  V2OR11
159300020913     C                   eval      D§SCHor12 =  V2OR12
159400020913     C                   eval      D§SCHor13 =  V2OR13
159500020913     C                   eval      D§SCHor14 =  V2OR14
159600020913     C                   eval      D§SCHor15 =  V2OR15
159700020913     C                   eval      D§SCHtps  =  V2TPS
159800020913     C                   eval      D§SCHute  =  V2UTE
159900020913     C                   movel(p)  DSCH          TBEuni
160000020212      *
160100020226     C                   movel     W1FTT         TBEftt
160200020212     C                   z-add     TBXflt        TBEflt
160300020212     C                   clear                   TBEftr
160400090319      *
160500020212     C                   ENDSR
160600090319
160700090320      /free
160800090320       //---------------------------------------------------------------
160900090320       // Reperimento progressivo per tabella "SCK"
161000090320       //---------------------------------------------------------------
161100090320       BEGSR  sr_RepNumSCK;
161200090320
161300090320         clear  TRUL33ds;
161400090320         I33tla = 'L';
161500090320         I33ope = *zero;
161600090320         I33cnu = 500;
161700090320         I33num = 1;
161800090320         KPJBU  = TRUL33ds;
161900090320
162000090320         trul33r ( KPJBA );
162100090320
162200090320         TRUL33ds = KPJBU;
162300090320         if  O33err <> *zero;
162400090320           *in99 = *on;
162500090320           V2msg = O33msg;
162600090320           leavesr;
162700090320         endif;
162800090320
162900090320         V2prg = %editc( O33nri : 'X' );
163000090320
163100090320       ENDSR;
163200090319
163300090320       //---------------------------------------------------------------
163400090320       // Riempimento tabella "SCK"
163500090320       //---------------------------------------------------------------
163600090320       BEGSR  sr_RieTabSCK;
163700090320
163800090320         clear  k_TBEatb;
163900090320         if  TBYsif <> *blank;
164000090320           k_TBEsif =  TBYsif;
164100090320         else;
164200090320           clear  k_TBEsif;
164300090320         endif;
164400090320         k_TBEapl = TBYapl;
164500090320         k_TBEcod = 'SCK';
164600101005         k_TBEke1 = V2prg;
164700090320         //k_TBEke2 = V2azn;
164800090320         clear  k_TBEke2;
164900090320         k_TBEuni = OkpjKPJBU;
165000090320
165100090320         k_TBEftt = W1FTT;
165200090320         k_TBEflt = TBYflt;
165300090320         clear  k_TBEftr;
165400090320
165500090320       ENDSR;
165600090320
165700090320      /end-free
165800090320
165900020212** -OPZ-      *
166000020212  Inserimento
166100020212    Modifica
166200020212     Copia
166300020212  Annullamento
166400020212Visualizzazione
166500020212   ANNULLATO
166600060426** GIORSET-  GIORNI DELLA settimana                                          *
166700060426LUNMARMERGIOVENSABDOM
166800020212** -MSG-                                                                     *
166900020913Immettere il sistema informativo                                               01
167000020913Sistema informativo errato                                                     02
167100020913Immettere l'azione                                                             03
167200020913Immettere la descrizione                                                       04
167300020913Immettere il flag "da eseguire"                                                05
167400040708Esecuzione non valida per il S.I.                                              06
167500020913Immettere la frequenza                                                         07
167600020913Tipo sottomissione multipla possibile solo x s.i. FILTRA                       08
167700020913Immettere l'utente                                                             09
167800020913Nel s.i. FILTRA l'utente deve essere EDPCEDXXX                                 10
167900020913Utente EDPCEDXXX valido solo x s.i. FILTRA                                     11
168000020913Per sottomissioni multiple l'utente deve essere EDPCEDXXX                      12
168100060426Immettere almeno 1 orario x sched.Giornaliera/Mensile/Settimanale non in Notte 13
168200020913Orario errato                                                                  14
168300020918Utente errato - non esiste sul modulo base                                     15
168400091123Per lavori Settimanali in NOTTE (no"SK) non bisogna impostare orari            16
168500051107Coda lavori inesistente                                                        17
168600060426Selezionare soltanto un tipo di frequenza                                      18
168700060426Per frequenza MENSILE, indicare il giorno del mese                             19
168800060426NON richiesta frequenza Mensile: eliminare il giorno del mese                  20
168900060426NON richiesta frequenza Giornaliera:eliminare relativi giorni della settimana  21
169000060426Giorno della settimana errato                                                  22
169100060426Giorno della settimana già immesso                                             23
169200060426NON richiesta frequenza Settimanale:eliminare relativo giorno della settimana  24
169300091123Elaborazioni in coda NOTTE valide solo x sched.Mensile o Settim.SABATO/DOMENIC 25
169400101005Azione già schedulata                                                          26
