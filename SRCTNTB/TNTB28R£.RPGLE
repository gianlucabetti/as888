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
009200110603      *
009300060426     d                 ds
009400060426     d  v2g1g                  1      3
009500060426     d  v2g2g                  4      6
009600060426     d  v2g3g                  7      9
009700060426     d  v2g4g                 10     12
009800060426     d  v2g5g                 13     15
009900060426     d  v2g6g                 16     18
010000060426     d  v2g7g                 19     21
010100060426     d GGGiorn                 1     21    dim(7)
010200020212
010300020212      *---------------------------------------------------------------*
010400020212      *   V A R I A B I L I                                           *
010500020212      *---------------------------------------------------------------*
010600020212      *
010700060427     D CC              s              1  0 inz
010800060427     D XX              s              2  0 inz
010900020913     D yy              s              2  0 inz
011000020212     D $Err            s              1a   inz(*off)
011100020212     D $Fine           s              1a   inz(*off)
011200020913     D $CarV0          s              1a   inz(*on)
011300020913     D $CarV1          s              1a   inz(*off)
011400020212     D $CarV2          s              1a   inz(*off)
011500020226     D $CarW1          s              1a   inz(*off)
011600090423     D $NewKPJBU       s               n   inz
011700020913     D TipVid          s              1a   inz('0')
011800020212     D Win             s             99a   inz(*zeros)
011900020301     D wTasto          s              2a   inz(*zeros)
012000020913     d totora          s              5  0
012100020913     d wora            s              2  0
012200020913     d wminuti         s              2  0
012300110603      *
012400110603     d SavP01ord       s                   like(P01ord)  inz
012500110603     d SavP01cnd       s                   like(P01cnd)  inz
012600051107      *
012700051107      * Parametri x pgm. QCMDEXC
012800051107     d Qcmd            s            100    inz
012900051107     d Qlen            s             15  5 inz(100)
013000090320
013100090320       //--------------------------------------------------------------
013200090320       //?Definizione procedure usate.
013300090320       //--------------------------------------------------------------
013400090320
013500090320       // - Reperimento/Aggiornamento numeratori in GRU
013600090320      /copy gaitrasrc/srcProtoPr,TRUL33R
013700090320
013800090320       //--------------------------------------------------------------
013900090320       //?Definizione key-list.                                        ?
014000090320       //--------------------------------------------------------------
014100090320
014200090320     d keyTbeSCK_P   e ds                  extname(TNTBESCK : *key)
014300090320     d                                     prefix(k_)
014400090320     d                                     inz
014500020212
014600020212      *---------------------------------------------------------------*
014700020212      *   M A I N   L I N E                                           *
014800020212      *---------------------------------------------------------------*
014900020212      *  Riepilogo indicatori utilizzati:                             *
015000020212      *  --------------------------------                             *
015100020212      *  01 - Record inesistente (inserimento)                        *
015200020212      *  02 - Record esistente   (modifica)                           *
015300020212      *  04 - Record annullato   (ripristino)                         *
015400020301      *  20 - Comodo                                                  *
015500020212      *  22 - Errori in scrittura record (WRITE)                      *
015600020212      *---------------------------------------------------------------*
015700020212      *
015800020212      * Operazioni iniziali
015900020212     C                   exsr      RutInz
016000020212      *
016100020212      * Gestione video
016200020724     C     $Fine         downe     'S'
016300020913     C     TipVid        caseq     '0'           GesV0
016400020212     C     TipVid        caseq     '1'           GesV1
016500020212     C     TipVid        caseq     '2'           GesV2
016600110603     C     TipVid        caseq     '3'           GesV1N
016700020226     C     TipVid        caseq     'A'           GesW1
016800020212     C                   endcs
016900020212     C                   enddo
017000020212      *
017100080718     c*
017200080718     c* Elimino override al file tabelle come TNTBESCK
017300080718     c                   eval      Qcmd    =
017400080718     c                             'DLTOVR FILE(TNTBESCK) LVL(*)'
017500080718     c                   call(e)   'QCMDEXC'
017600080718     c                   parm                    Qcmd
017700080718     c                   parm                    Qlen
017800080718      *
017900080718      *
018000020212     C                   movel     *on           *inLR
018100020212
018200020212      *---------------------------------------------------------------*
018300020212      * RutInz - Operazioni Iniziali                                  *
018400020212      *---------------------------------------------------------------*
018500020212     C     RutInz        BEGSR
018600020212      *
018700020212      * Ricezione parametri
018800020212     C     *entry        plist
018900020212     C                   parm                    KPJBA
019000020212      *
019100020212      * Definizioni chiavi di accesso
019200020212     C     K05TBE01      klist                                                  *tntbe01l
019300020212     C                   kfld                    TBEcod                         -tabella
019400020212     C                   kfld                    TBEke1                         -chiave uno
019500020212     C                   kfld                    TBEke2                         -chiave due
019600020212     C                   kfld                    TBElin                         -lingua
019700020212     C                   kfld                    TBEsif                         -s.informativo
019800020724      *
019900020724      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
020000020724     C     *dtaara       define    §azute        azuteds
020100020724     C     *dtaara       define    §datiute      ddatiute
020200020724      *
020300020724     C                   clear                   AzUteDs
020400020724     C                   clear                   DDatiUte
020500020724     C                   clear                   Tibs34Ds
020600020724     C                   in(E)     *dtaara
020700020724if  1C                   IF        %Error  or  RSUT = *blanks
020800020724     C                   call      'TIBS34R'
020900020724     C                   parm                    Tibs34Ds
021000020724     C                   in        *dtaara
021100020724e   1C                   ENDIF
021200020724      *-- Verifica errori e autorità profilo
021300020724sel 1C                   SELECT
021400020724      *-- controllo se ho errori nei dati utente
021500020724      *--   nel qual caso NON risulta un profilo abilitato
021600020724w   1C                   WHEN      DUTERR = 'E'
021700020724     C                   eval      $Fine  = 'S'
021800020724      *
021900020724      *-- CONTROLLO AUTORITA'
022000020724      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
022100020724      *-- se il chiamante non richiede autorità specifica verificare
022200020724      *--   quella del profilo
022300020724      *-- se il chiamante richiede autorità specifica verificarla,
022400020724      *--  se è blank verificare quella del profilo
022500020724      *
022600020724      * se UTEAUT = *BLANK non siamo in sede
022700020724w   1C                   WHEN      UTEAUT = *blank
022800020724      *
022900020724x   1c                   OTHER
023000020724      *
023100020724e   1C                   ENDSL
023200090319      *
023300080718     c* Eseguo override al file tabelle come TNTBESCK
023400080718     c                   eval      Qcmd    =
023500080718     c                             'OVRDBF FILE(TNTBESCK) ' +
023600080718     c                             'TOFILE(*LIBL/TNTBE01L)'
023700080718     c                   call(e)   'QCMDEXC'
023800080718     c                   parm                    Qcmd
023900080718     c                   parm                    Qlen
024000080718     c*
024100080718     c* Quindi apro il file
024200080721     c                   open      TNTBESCK
024300090319      *
024400090319     c                   movel     SDSpgm        V1Tpgm
024500090319      *
024600020913     c                   EndSr
024700020913      *---------------------------------------------------------------*
024800020913      * GESV0  - Gestione videata richiesta S.I.                      *
024900020913      *---------------------------------------------------------------*
025000020913     C     GESV0         BEGSR
025100020913      *
025200020913      * Inizializzazione videata
025300020913if  1C                   if        $CarV0 = *on
025400020913     C                   exsr      CARV0
025500020913     C                   movel     *off          $CarV0
025600020913e   1C                   endif
025700020913      *
025800020913      * Scrivo la testata
025900020913     C                   clear                   T1opz
026000020913     C                   write     TB28T1
026100020913      *
026200020913      * Se esistono errori sulla videata
026300020913      * emetto la write del formato a indicatori spenti per vedere
026400020913      * le eventuali decodifiche
026500020913if  1C                   if        *in99
026600020913     C                   movea     *in           WIN
026700080721     C                   movea     *zeros        *in(40)
026800020913     C                   write     TB28V0
026900020913     C                   movea     WIN           *in
027000020913e   1C                   endif
027100020913      *
027200020913     C                   exfmt     TB28V0
027300020913     C                   eval      *in99 = *off
027400020913     C                   clear                   V0MSG
027500020913      * F3=Fine
027600020913     C                   if        *inKC
027700020913cas 1C                   exsr      F03V0
027800101005     C                   leavesr
027900020913e   1C                   endif
028000020913      *
028100020913      * Controllo se immesso il s.i.
028200020913if  1C                   if        not *in99
028300020913     C                   exsr      CTRV0
028400020913e   1C                   endif
028500020212      *
028600090320      * Aggancio dati generali della tabella SCH
028700020212     C                   clear                   TBEcod
028800020212     C                   move      *zeros        TBEke1
028900020212     C                   move      T02cod        TBEke1
029000020212     C                   clear                   TBEke2
029100020212     C                   clear                   TBElin
029200020913     C                   movel     V0Sif         TBEsif
029300131128     C     K05TBE01      chain(N)  TNTBE01L
029400020212     C                   if        not %found(TNTBE01L)
029500020212     C                   clear                   TBEsif
029600131128     C     K05TBE01      chain(N)  TNTBE01L
029700020212     C                   endif
029800020212     C                   if        %found(TNTBE01L)
029900020212     C                   movel     TNTBEds       xTNTBEds
030000020212     C                   else
030100020212     C                   clear                   xTNTBEds
030200020212     C                   endif
030300090320      *
030400090320      * Aggancio dati generali della tabella SCK
030500090320     C                   clear                   TBEcod
030600090320     C                   move      *zeros        TBEke1
030700090320     C                   move      'SCK'         TBEke1
030800090320     C                   clear                   TBEke2
030900090320     C                   clear                   TBElin
031000090320     C                   movel     V0Sif         TBEsif
031100131128     C     K05TBE01      chain(N)  TNTBE01L
031200090320     C                   if        not %found(TNTBE01L)
031300090320     C                   clear                   TBEsif
031400131128     C     K05TBE01      chain(N)  TNTBE01L
031500090320     C                   endif
031600090320     C                   if        %found(TNTBE01L)
031700090320     C                   movel     TNTBEds       yTNTBEds
031800090320     C                   else
031900090320     C                   clear                   yTNTBEds
032000090320     C                   endif
032100020913      *
032200020913      * Passaggio alla videata di dettaglio
032300020913if  1C                   if        not *in99
032400020913     C                   eval      $CarV1 = *on
032500020913     C                   eval      TipVid = '1'
032600020913e   1C                   endif
032700020212      *
032800101005     C                   ENDSR
032900020212
033000020913      *---------------------------------------------------------------*
033100020913      * CARV0  - Caricamento dati videata S.I.                        *
033200020913      *---------------------------------------------------------------*
033300020913     C     CARV0         BEGSR
033400020913      *
033500020913     C                   movea     *zeros        *in(50)
033600020913     C                   movea     '00000'       *in(01)
033700020913     C                   clear                   TB28V0
033800020913      *
033900020913     C                   move      Knsif         V0sif
034000020913      *
034100020913     C                   ENDSR
034200020913
034300020913      *---------------------------------------------------------------*
034400020913      * CTRV0  - Controllo videata s.i.                               *
034500020913      *---------------------------------------------------------------*
034600020913     C     CTRV0         BEGSR
034700020913      *
034800020913     C                   movea     *zeros        *in(50)
034900020913     C                   clear                   V0msg
035000020913      *
035100020913      * S.I. obbligatorio
035200020913if  1C                   if        V0SIF = *blanks
035300020913     C                   seton                                        50  99
035400020913     C                   eval      V0msg = Msg(01)
035500101005     C                   leavesr
035600020913e   1C                   endif
035700020913
035800020913      * Deve essere un s.i. valido
035900020913     c     v0sif         chain     kfsif01l
036000020913     c                   if        not %Found(kfsif01l)
036100020913     C                   seton                                        50  99
036200020913     C                   eval      V0msg = Msg(02)
036300101005     C                   leavesr
036400020913     c                   endif
036500020913      *
036600101005     C                   ENDSR
036700020913
036800020913      *---------------------------------------------------------------*
036900020913      * F03V0  - Tasto funzionale F03 -> Fine programma               *
037000020913      *---------------------------------------------------------------*
037100020913     C     F03V0         BEGSR
037200020913      *
037300020913     C                   movel     'S'           $Fine                          fine pgm
037400020913      *
037500020913     C                   ENDSR
037600110603
037700020212      *---------------------------------------------------------------*
037800020212      * GESV1  - Gestione videata selezione codice tabella            *
037900020212      *---------------------------------------------------------------*
038000020212     C     GESV1         BEGSR
038100020212      *
038200020212      * Inizializzazione videata
038300020212if  1C                   if        $CarV1 = *on
038400020212     C                   exsr      CARV1
038500020212     C                   movel     *off          $CarV1
038600020212e   1C                   endif
038700020913      * Scrivo la testata
038800020913     C                   clear                   T1opz
038900020913     C                   write     TB28T1
039000020913      *
039100020913      * Se esistono errori sulla videata
039200020913      * emetto la write del formato a indicatori spenti per vedere
039300020913      * le eventuali decodifiche
039400020913     C                   if        *in99
039500020913     C                   movea     *in           Win
039600020913     C                   movea     *zeros        *in(50)
039700020913     C                   write     TB28V1
039800020913     C                   movea     Win           *in
039900020913     C                   endif
040000020913      *
040100020913     C                   exfmt     TB28V1
040200020913     C                   eval      *in99 = *off
040300020913     C                   clear                   V1MSG
040400020913      *
040500020913sel 1C                   select
040600020913      * F03=Fine
040700020913w   1C                   when      *inKC
040800020913     C                   exsr      F03V0
040900101005     C                   leavesr
041000020913      * F12=Ritorno
041100020913w   1C                   when      *inKL
041200020913     C                   exsr      F12V1
041300101005     C                   leavesr
041400020913e   1C                   endsl
041500020212      *
041600020212      * Ricerche
041700020212     C                   clear                   XX
041800020212     C                   clear                   NMFL
041900020912     C     '?'           scan      V1AZN                                  20
042000020301if  1C                   if        *in20
042100020212     C                   add       1             XX
042200020912     C                   eval      NMFL(XX) = 'V1AZN     '
042300020212     C                   exsr      SEARCH
042400020212e   1C                   endif
042500020212      *
042600020301      * Controllo e decodifiche dati immessi a video
042700020301if  1C                   if        not *in99
042800020212     C                   exsr      CTRV1
042900020301e   1C                   endif
043000020212      *
043100020212      * Passaggio alla videata di dettaglio
043200020212if  1C                   if        not *in99
043300020212     C                   eval      $CarV2 = *on
043400020212     C                   eval      TipVid = '2'
043500020212e   1C                   endif
043600020212      *
043700101005     C                   ENDSR
043800020212
043900020212      *---------------------------------------------------------------*
044000020212      * CARV1  - Caricamento dati prima videata                       *
044100020212      *---------------------------------------------------------------*
044200020212     C     CARV1         BEGSR
044300020212      *
044400020913     C                   movea     *zeros        *in(50)
044500020913     C                   clear                   TB28V1
044600020913
044700020912     C                   move      '?'           V1AZN
044800090320     C                   clear                   V1prg
044900040721     C                   move      V0sif         V1sif
045000110603      *
045100110603     c                   clear                   SavP01ord
045200110603     c                   clear                   SavP01cnd
045300020212      *
045400020212     C                   ENDSR
045500020212
045600020212      *---------------------------------------------------------------*
045700020212      * CTRV1  - Controllo e decodifica prima videata                 *
045800020212      *---------------------------------------------------------------*
045900020212     C     CTRV1         BEGSR
046000020212      *
046100020913     C                   movea     *zeros        *in(50)
046200020212     C                   clear                   V1msg
046300020212      *
046400020912     C                   clear                   V1AZND
046500020912      * Azione obbligatoria
046600020912if  1C                   if        V1AZN = *blanks
046700020913     C                   seton                                        51  99
046800020913     C                   eval      V1msg = Msg(03)
046900101005     C                   leavesr
047000020212x   1C                   else
047100020212      * Controllo/Decodifica codice numeratore
047200020212     C                   reset                   TIBS02DS
047300020913     C                   movel     TbxSif        T02sif
047400020912     C                   movel     V1AZN         T02ke1
047500090320     C                   move      V1prg         T02ke2
047600020212     C                   call      'TIBS02R'
047700020212     C                   parm                    KPJBA
047800020212     C                   parm                    TIBS02DS
047900020212if  2C                   if        T02err = *blanks
048000020912     C                   movel     T02uni        DSCH
048100020912     C                   movel     d§SCHdes      V1AZND
048200020212x   2C                   else
048300020912     C                   clear                   DSCH
048400020212e   2C                   endif
048500020212e   1C                   endif
048600020212      *
048700101005     C                   ENDSR
048800020913
048900020913      *---------------------------------------------------------------*
049000020913      * F12V1  - Tasto funzionale F12 -> Ritorno                      *
049100020913      *---------------------------------------------------------------*
049200020913     C     F12V1         BEGSR
049300020913      *
049400020913     C                   eval      TipVid = '0'
049500020913      *
049600020913     C                   ENDSR
049700020212
049800020212      *---------------------------------------------------------------*
049900020212      * SEARCH - Ricerche                                             *
050000020212      *---------------------------------------------------------------*
050100020212     C     SEARCH        BEGSR
050200020301      *
050300101007     C*//                eval      *in99 = *on
050400020212      *
050500020212do  1C                   do        10            XX
050600020212      *
050700020212if  2C                   if        NMFL(XX) = *blanks
050800020212     C                   leave
050900020212e   2C                   endif
051000020212      *
051100020212sel 2C                   select
051200020912      * Azione
051300020912w   2C                   when      NMFL(XX) =  'V1AZN     '
051400021114     C                   reset                   Param01
051500021114     C                   movel     TbxSif        P01Sif
051600110603     c                   eval      P01ord = SavP01ord
051700110603     c                   eval      P01cnd = SavP01cnd
051800021114     c                   Movel     Param01       Kpjbu
051900021114     C                   call      'TNTB281R'
052000020212     C                   parm                    KPJBA
052100021114     c                   Movel     Kpjbu         Param01
052200110603     c                   eval      SavP01ord = P01ord
052300110603     c                   eval      SavP01cnd = P01cnd
052400021114if  4C                   if        P01ke1 <> *blanks
052500021114     C                   movel     P01ke1        V1AZN
052600080721     C                   movel     P01ke2        V1PRG
052700020212x   3C                   else
052800020912     C                   clear                   V1AZND
052900101018     C                   eval      *in99 = *on
053000020212e   3C                   endif
053100110603     C                   select
053200110603     C                   when      P01rit = 'C'
053300110603     C                   eval      TipVid = '0'
053400110603     C                   eval      $Fine = 'S'
053500110603     C                   leavesr
053600110603     C                   when      P01rit = 'L'
053700110603     C                   eval      TipVid = '1'
053800110603     C                   iter
053900110603     C                   endsl
054000080721      *
054100020212e   2C                   endsl
054200020212      *
054300020212e   1C                   enddo
054400020212      *
054500020212     C                   ENDSR
054600110603
054700110603      /free
054800110603
054900110603       //--------------------------------------------------------------
055000110603       //?GESV1N - Gestione videata selezione codice tabella (RICERCA) ?
055100110603       //--------------------------------------------------------------
055200110603       BEGSR  GesV1N;
055300110603
055400110603         // -?Forzatura ricerca codice azione?
055500110603         clear  V1AZN;
055600110603         clear  V1PRG;
055700110603         clear  V1AZND;
055800110603         xx = 1;
055900110603         clear  NMFL;
056000110603         NMFL(XX) = 'V1AZN     ';
056100110603         exsr  SEARCH;
056200110603         select;
056300110603           when  P01rit <> *blank;
056400110603             leavesr;
056500110603           when  *in99;
056600110603             $CarV1 = *on;
056700110603             TipVid = '1';
056800110603         endsl;
056900110603
057000110603         // -?Controllo e decodifica azione selezionata?
057100110603         exsr  CTRV1;
057200110603
057300110603         // -?Passaggio alla videata di dettaglio?
057400110603         if  not *in99;
057500110603           $CarV2 = *on;
057600110603           TipVid = '2';
057700110603         endif;
057800110603
057900110603       ENDSR;
058000110603
058100110603      /end-free
058200110603
058300020212      *---------------------------------------------------------------*
058400020212      * GESV2  - Gestione videata dettaglio dati                      *
058500020212      *---------------------------------------------------------------*
058600020212     C     GESV2         BEGSR
058700020212      *
058800020212      * Inizializzazione videata
058900020212     C                   if        $CarV2 = *on
059000020212     C                   exsr      CarV2
059100020212     C                   move      *off          $CarV2
059200020212     C                   endif
059300020212      * Scrivo la testata
059400020912     C                   write     TB28T1
059500020212      *
059600020212      * Se esistono errori sulla videata
059700020212      * emetto la write del formato a indicatori spenti per vedere
059800020212      * le eventuali decodifiche
059900020212     C                   if        *in99
060000020212     C                   movea     *in           Win
060100020913     C                   movea     *zeros        *in(50)
060200020912     C                   write     TB28V2
060300020212     C                   movea     Win           *in
060400020212     C                   endif
060500020212      *
060600020212if  1C                   if        *in05
060700020912     C                   write     TB28V2
060800020212     C                   exfmt     PROTECT
060900020212x   1C                   else
061000020912     C                   exfmt     TB28V2
061100020212e   1C                   endif
061200020212     C                   eval      *in99 = *off
061300020301     C                   clear                   V2MSG
061400020301     C                   clear                   wTasto
061500020212      *
061600020212sel 1C                   select
061700020212      * F03=Fine
061800020212w   1C                   when      *inKC
061900020913     C                   exsr      F03V0
062000101005     C                   leavesr
062100020212      * F12=Ritorno
062200020212w   1C                   when      *inKL
062300020913     C                   exsr      F12V2
062400101005     C                   leavesr
062500080718      * F07=Immagine KPJBU
062600080718w   1C                   when      *inKG
062700080721     C                   exsr      F07V2
062800020212e   1C                   endsl
062900020212      *
063000020212      * Controllo dati immessi a video
063100101005     C                   exsr      CTRV2
063200020212      *
063300020212      * Aggiornamento se non ci sono errori
063400020212if  1C                   if        not *in99
063500020212     C                             and (*inKF or *inKE or *inKQ)
063600020301sel 2C                   select
063700020301w   2C                   when      *inKE
063800020301     C                   eval      wTasto = '05'
063900020301w   2C                   when      *inKF
064000020301     C                   eval      wTasto = '06'
064100020301w   2C                   when      *inKQ
064200020301     C                   eval      wTasto = '16'
064300020301e   2C                   endsl
064400020226     C                   eval      $CarW1 = *on
064500020226     C                   eval      TipVid = 'A'
064600020212e   1C                   endif
064700020212      *
064800101005     C                   ENDSR
064900020212
065000020212      *---------------------------------------------------------------*
065100020212      * CARV2  - Caricamento dati seconda videata                     *
065200020212      *---------------------------------------------------------------*
065300020212     C     CARV2         BEGSR
065400020212      *
065500020212     C                   clear                   T1opz
065600020212     C                   movea     '00000'       *in(01)
065700040721     C                   move      V0sif         V2sif
065800090319      *
065900090319     C                   clear                   trulkpjds
066000020212      *
066100020212      * Aggancio la tabella, se trovo il codice sono in modifica
066200020212      * o ripristino (se record annullato), altrimenti in immissione
066300020212     C                   exsr      CHNTBE
066400020212      *
066500101005sel 1c                   SELECT
066600101005      *
066700101005      * MODIFICA/RIPRISTINO
066800101005w   1c                   WHEN      %found(TNTBE01L)  and  P01opz <> '3'
066900020913     C                   movel     TBEuni        DSCH
067000020212if  2C                   if        TBEatb = *blanks
067100020212     C                   eval      *in02  = *on
067200020212     C                   eval      T1opz  = Opz(02)
067300020212x   2C                   else
067400020212     C                   eval      *in04  = *on
067500020212     C                   eval      T1opz  = Opz(06)
067600020212e   2C                   endif
067700020212      *
067800101005      * COPIA
067900101005w   1c                   WHEN      %found(TNTBE01L)  and  P01opz = '3'
068000101005     c                   movel     TBEuni        DSCH
068100101005     C                   eval      *in01  = *on
068200101005     c                   eval      T1opz  = Opz(03)
068300020212      *
068400101005      * IMMISSIONE
068500101005w   1C                   WHEN      NOT %found(TNTBE01L)
068600020913     c                   clear                   DSCH
068700020212     C                   eval      *in01  = *on
068800020212     C                   eval      T1opz  = Opz(01)
068900020212      *
069000101005e   1C                   ENDSL
069100101005      *
069200090505      * Aggancio il file delle azioni per recuperare la descrizione
069300090505     c     v1azn         chain     kfazn11l
069400090505     c                   if        not %found(kfazn11l)
069500090505     c                   clear                   kdsaz
069600090505     c                   endif
069700020212      *
069800020912     C                   move      V1AZN         V2AZN
069900101005     c                   IF        P01opz = '3'
070000101005     C                   clear                   V2prg
070100101005     C                   ELSE
070200080721     C                   move      V1PRG         V2PRG
070300101005     C                   ENDIF
070400021025     C                   eval      V2prio = d§SCHpri
070500021025     C                   eval      V2AZND = d§SCHdes
070600020912     C                   eval      V2ESE  = d§SCHese
070700060426     C**                 eval      V2FRE  = d§SCHfre
070800121008     C                   eval      V2Sosp = d§SCHSosp
070900121009     C* se l'azione è sospesa metto in reverse image la costante
071000121009     C                   SETOFF                                       21
071100121009     C                   IF        V2Sosp = 'S'
071200121009     C                   SETON                                        21
071300121009     C                   ENDIF
071400090505     c                   eval      v2dazn = kdsaz
071500060426     c                   clear                   v2gior
071600060426     c                   clear                   v2sett
071700060426     c                   clear                   v2mens
071800090319      *
071900060426     c                   select
072000060426     c                   when      d§schfre='G'
072100060426     c                   eval      v2gior='SI'
072200060426     c                   when      d§schfre='S'
072300060426     c                   eval      v2sett='SI'
072400060426     c                   when      d§schfre='M'
072500060426     c                   eval      v2mens='SI'
072600060426     c                   endsl
072700090319      *
072800050222     c                   if        alfschggm>=*zeros
072900050215     C                   eval      V2ggm  = d§SCHggm
073000050222     c                   else
073100050222     c                   clear                   v2ggm
073200050222     c                   endif
073300060427     c                   if        d§schg1s<>' '
073400060427     c                   movel     d§schg1s      cc
073500060427     C                   eval      V2g1s  = giorset(CC)
073600060427     c                   else
073700060427     c                   clear                   v2g1s
073800060427     c                   endif
073900060426     c* Per frequenza settimanale, se non impostato, imposto SAB
074000060426     c                   if        v2sett='SI' and v2g1s='   '
074100060426     c                   eval      v2g1s='SAB'
074200060426     c                   endif
074300090319      *
074400060427     c                   if        d§schg1g<>' '
074500060427     c                   movel     d§schg1g      cc
074600060427     C                   eval      V2g1g  = giorset(cc)
074700060427     c                   else
074800060427     c                   clear                   v2g1g
074900060427     c                   endif
075000090319      *
075100060427     c                   if        d§schg2g<>' '
075200060427     c                   movel     d§schg2g      cc
075300060427     C                   eval      V2g2g  = giorset(cc)
075400060427     c                   else
075500060427     c                   clear                   v2g2g
075600060427     c                   endif
075700090319      *
075800060427     c                   if        d§schg3g<>' '
075900060427     c                   movel     d§schg3g      cc
076000060427     C                   eval      V2g3g  = giorset(cc)
076100060427     c                   else
076200060427     c                   clear                   v2g3g
076300060427     c                   endif
076400060427     c                   if        d§schg4g<>' '
076500060427     c                   movel     d§schg4g      cc
076600060427     C                   eval      V2g4g  = giorset(cc)
076700060427     c                   else
076800060427     c                   clear                   v2g4g
076900060427     c                   endif
077000060427     c                   if        d§schg5g<>' '
077100060427     c                   movel     d§schg5g      cc
077200060427     C                   eval      V2g5g  = giorset(cc)
077300060427     c                   else
077400060427     c                   clear                   v2g5g
077500060427     c                   endif
077600060427     c                   if        d§schg6g<>' '
077700060427     c                   movel     d§schg6g      cc
077800060427     C                   eval      V2g6g  = giorset(cc)
077900060427     c                   else
078000060427     c                   clear                   v2g6g
078100060427     c                   endif
078200060427     c                   if        d§schg7g<>' '
078300060427     c                   movel     d§schg7g      cc
078400060427     C                   eval      V2g7g  = giorset(cc)
078500060427     c                   else
078600060427     c                   clear                   v2g7g
078700060427     c                   endif
078800060426     C                   eval      V2JOB  = d§SCHjob
078900020912     C                   eval      V2OR1  = D§SCHor1
079000060426     c                   if        v2or1>*zeros
079100060426     c                   seton                                        84
079200060426     c                   endif
079300020912     C                   eval      V2OR2  = D§SCHor2
079400060426     c                   if        v2or2>*zeros
079500060426     c                   seton                                        85
079600060426     c                   endif
079700020912     C                   eval      V2OR3  = D§SCHor3
079800060426     c                   if        v2or3>*zeros
079900060426     c                   seton                                        86
080000060426     c                   endif
080100020912     C                   eval      V2OR4  = D§SCHor4
080200060426     c                   if        v2or4>*zeros
080300060426     c                   seton                                        87
080400060426     c                   endif
080500020912     C                   eval      V2OR5  = D§SCHor5
080600060426     c                   if        v2or5>*zeros
080700060426     c                   seton                                        88
080800060426     c                   endif
080900020912     C                   eval      V2OR6  = D§SCHor6
081000060426     c                   if        v2or6>*zeros
081100060426     c                   seton                                        89
081200060426     c                   endif
081300020912     C                   eval      V2OR7  = D§SCHor7
081400060426     c                   if        v2or7>*zeros
081500060426     c                   seton                                        90
081600060426     c                   endif
081700020912     C                   eval      V2OR8  = D§SCHor8
081800060426     c                   if        v2or8>*zeros
081900060426     c                   seton                                        91
082000060426     c                   endif
082100020912     C                   eval      V2OR9  = D§SCHor9
082200060426     c                   if        v2or9>*zeros
082300060426     c                   seton                                        92
082400060426     c                   endif
082500020912     C                   eval      V2OR10 = D§SCHor10
082600060426     c                   if        v2or10>*zeros
082700060426     c                   seton                                        93
082800060426     c                   endif
082900020912     C                   eval      V2OR11 = D§SCHor11
083000060426     c                   if        v2or11>*zeros
083100060426     c                   seton                                        94
083200060426     c                   endif
083300020912     C                   eval      V2OR12 = D§SCHor12
083400060426     c                   if        v2or12>*zeros
083500060426     c                   seton                                        95
083600060426     c                   endif
083700020912     C                   eval      V2OR13 = D§SCHor13
083800060426     c                   if        v2or13>*zeros
083900060426     c                   seton                                        96
084000060426     c                   endif
084100020912     C                   eval      V2OR14 = D§SCHor14
084200060426     c                   if        v2or14>*zeros
084300060426     c                   seton                                        97
084400060426     c                   endif
084500020912     C                   eval      V2OR15 = D§SCHor15
084600060426     c                   if        v2or15>*zeros
084700060426     c                   seton                                        98
084800060426     c                   endif
084900020912     C                   eval      V2TPS  = D§SCHtps
085000020912     C                   eval      V2UTE  = D§SCHute
085100090320      /free
085200090320
085300090320         OkpjKPJBU = k_TBEuni;
085400090320
085500101005         // - Se ricevuto campo opzione "3" (da TNTB281R):
085600101005         //   è stata richiesta una "COPIA"
085700101005         *in43 = (P01opz = '3');
085800101005
085900090320         //// - Se V2PRG è valorizzato => è stata impostata l'immagine KPJBU
086000090320         //*in44 = (V2prg <> *blank);
086100090320         // - Se reperita la tab. "SCK" corrispondente
086200090320         //   => è stata impostata l'immagine KPJBU
086300090320         *in44 = (k_TBEuni <> *blanks);
086400090320
086500090320      /end-free
086600090320      *
086700020212     C                   ENDSR
086800080721
086900080721      *---------------------------------------------------------------*
087000080721      * F07V2  - Tasto funzionale F07 -> Immagine KPJBU               *
087100080721      *---------------------------------------------------------------*
087200080721     C     F07V2         BEGSR
087300080721      *
087400080721     C                   call      'TRULKPJR'
087500090319     C                   parm                    kpjba
087600080721     C                   parm                    trulkpjds
087700090319      *
087800090319     C                   eval      *in44 = (OkpjKPJBU <> *blanks)
087900080721      *
088000080721     C                   ENDSR
088100020913
088200020913      *---------------------------------------------------------------*
088300020913      * F12V2  - Tasto funzionale F12 -> Ritorno                      *
088400020913      *---------------------------------------------------------------*
088500020913     C     F12V2         BEGSR
088600020913      *
088700060427     c                   eval      $carv1=*on
088800110603     C                   eval      TipVid = '3'
088900021114     c                   Unlock    Tntbe01l
089000020913      *
089100020913     C                   ENDSR
089200020212
089300020212      *---------------------------------------------------------------*
089400020212      * CTRV2  - Controllo e decodifica seconda videata               *
089500020212      *---------------------------------------------------------------*
089600020212     C     CTRV2         BEGSR
089700020212      *
089800020913     C                   movea     *zeros        *in(50)
089900020212     C                   clear                   V2msg
090000020212      *
090100020212      * Non si fanno i controlli se richisto l'annullamento
090200020212     C     *inKQ         cabeq     *on           EndCtrV2
090300101005      *
090400101005      * (Ri)Aggancio la tabella se sono in copia
090500101005      * (se è stato modificato il codice chiave)
090600101005if  1c                   IF        P01opz =  '3'
090700101005if  2c                   if        TBEke1 <> V2azn   or
090800101005     c                             TBEke2 <> V2prg
090900101005     c                   eval      V1azn  =  V2azn
091000101005     c                   eval      V1prg  =  V2prg
091100101005     c                   exsr      CHNTBE
091200101005e   2c                   endif
091300101005      * In COPIA NON può già esistere il record
091400101005if  2c                   if        %found(TNTBE01L)
091500101005     c                   seton                                        51  99
091600101005     c                   movel     Msg(26)       V2msg
091700101005     c                   leavesr
091800101005e   2c                   endif
091900101005e   1c                   ENDIF
092000020913      *
092100020913      * Passo gli orari nella schiera
092200020913     c                   move      v2or1         ora(1)
092300020913     c                   move      v2or2         ora(2)
092400020913     c                   move      v2or3         ora(3)
092500020913     c                   move      v2or4         ora(4)
092600020913     c                   move      v2or5         ora(5)
092700020913     c                   move      v2or6         ora(6)
092800020913     c                   move      v2or7         ora(7)
092900020913     c                   move      v2or8         ora(8)
093000020913     c                   move      v2or9         ora(9)
093100020913     c                   move      v2or10        ora(10)
093200020913     c                   move      v2or11        ora(11)
093300020913     c                   move      v2or12        ora(12)
093400020913     c                   move      v2or13        ora(13)
093500020913     c                   move      v2or14        ora(14)
093600020913     c                   move      v2or15        ora(15)
093700020313      *
093800020212      * Descrizione obbligatoria
093900051107if  1C                   If        V2aznd =  *blanks
094000020913     C                   seton                                        52  99
094100020913     C                   movel     Msg(04)       V2msg
094200051107     C                   leavesr
094300051107e   1c                   endif
094400020212      *
094500020912      * Flag esecuzione obbligatorio
094600051107if  1C                   If        V2ese = *blanks
094700020913     C                   seton                                        53  99
094800020913     C                   movel     Msg(05)       V2msg
094900051107     C                   leavesr
095000051107e   1c                   endif
095100040708      * Non si può indicare esecuzione 'S' in ambiente P.O. e viceversa
095200040708      *                     esecuzione 'P' in ambiente SEDE
095300051107if  1C                   If        (V2ese = 'S'
095400040721     c                             and %subst(V0SIF:1:6) <> 'GAITRA')
095500040708     c                             or
095600051107     C                             (V2ese = 'P'
095700040721     c                             and %subst(V0SIF:1:6) <> 'FILTRA')
095800020913     C                   seton                                        53  99
095900020913     C                   movel     Msg(06)       V2msg
096000040721     C                   eval      V2msg = %trim(V2msg) + ' ' + V0SIF
096100051107     C                   leavesr
096200051107e   1c                   endif
096300020212      *
096400020912      * Flag frequenza
096500060426if  1C***                If        V2fre  = *blanks
096600060426     c                   if        v2gior='  ' and v2sett='  ' and v2mens='  '
096700020913     C                   seton                                        54  99
096800020913     C                   movel     Msg(07)       V2msg
096900051107     C                   leavesr
097000051107e   1c                   endif
097100060426     c*
097200060426     c                   if        (v2gior<>'  ' and v2sett<>'  ')
097300060426     c                             or (v2gior<>'  ' and v2mens<>'  ')
097400060426     c                             or (v2mens<>'  ' and v2sett<>'  ')
097500060426     C                   movel     Msg(18)       V2msg
097600101005     c*
097700060426     c                   if        v2gior<>'  '
097800060426     c                   seton                                        5499
097900060426     c                   else
098000060426     c                   seton                                        5799
098100060426     c                   endif
098200101005     c*
098300060426     C                   leavesr
098400060426     c                   endif
098500060426     C
098600060426     c* Per frequenza giornaliera
098700060426     c                   if        v2gior='  '
098800060426     c                   clear                   pieno             1
098900060426     c                   do        7             gg
099000060426    3c                   if        ggGiorn(GG)<>'   '
099100060426     c                   eval      pieno='S'
099200060426     c                   endif
099300060426     c                   enddo
099400101005     c*
099500060426     c                   if        pieno='S'
099600060426     c                   seton                                        7699
099700060426     C                   movel     Msg(21)       V2msg
099800060426     C                   leavesr
099900060426e   1c                   endif
100000060426e   1c                   endif
100100101005     C*
100200060426    1c                   if        v2gior='SI'
100300060426     c                   clear                   giaimm
100400060426     c                   clear                   xx
100500060426    2c                   do        7             gg                3 0
100600060426     C                   Z-ADD     75            YY
100700060426    3c                   if        ggGiorn(GG)<>'   '
100800060426     c     ggGiorn(GG)   lookup    GIORSET                                30
100900060426    4C                   IF        NOT *IN30
101000060426     c                   add       GG            yy
101100060426     C                   eval      *in(yy) = *On
101200060426     c                   seton                                        99
101300060426     C                   movel     Msg(22)       V2msg
101400060426     C                   leavesr
101500060426e   4c                   endif
101600060426     c* Verifico se già immesso
101700060426     c     ggGiorn(gg)   lookup    giaimm                                 30
101800060426    4C                   IF        *IN30
101900060426     c                   add       GG            yy
102000060426     C                   eval      *in(yy) = *On
102100060426     c                   seton                                        99
102200060426     C                   movel     Msg(23)       V2msg
102300060426     C                   leavesr
102400060426   x4c                   else
102500060426     c                   add       1             xx
102600060426     c                   eval      giaimm(xx)=ggGiorn(GG)
102700060426e   4c                   endif
102800101005     c*
102900060426e   3c                   endif
103000060426e   2c                   endDO
103100060426e   1c                   endif
103200060426     c* Frequenza settimanale
103300060426     c                   if        v2sett='  ' and v2g1s<>*blanks
103400060426     c                   seton                                        8399
103500060426     C                   movel     Msg(24)       V2msg
103600060426     C                   leavesr
103700060426e   1c                   endif
103800101005     c*
103900060426    1c                   if        v2sett='SI'
104000060426     c     v2g1s         lookup    GIORSET                                30
104100060426    2C                   IF        NOT *IN30
104200060426     c                   seton                                        8399
104300060426     C                   movel     Msg(22)       V2msg
104400060426     C                   leavesr
104500060426e   2c                   endif
104600060426e   1c                   endif
104700101005     c*
104800060426     c* Frequenza Mensile
104900060426     c                   if        v2mens='SI'  and v2ggm=0
105000060426     c                   seton                                        7599
105100060426     C                   movel     Msg(19)       V2msg
105200060426     C                   leavesr
105300051107e   1c                   endif
105400060426     c                   if        v2mens='  '  and v2ggm>0
105500060426     c                   seton                                        7599
105600060426     C                   movel     Msg(20)       V2msg
105700060426     C                   leavesr
105800060426e   1c                   endif
105900060426    c
106000051107      *
106100051107      * Coda lavori forzata
106200051107if  1c                   if        V2job <> *blanks
106300051107     c                   eval      Qcmd    =  'CHKOBJ OBJ('
106400051107     c                                     +  %trimr(V2job)
106500051107     c                                     +  ') OBJTYPE(*JOBQ)'
106600051107     c                   call      'QCMDEXC'                            99
106700051107     c                   parm                    Qcmd
106800051107     c                   parm                    Qlen
106900051107if  2c                   if        *in99
107000051107     C                   seton                                        58  99
107100051107     C                   movel     Msg(17)       V2msg
107200051107     c                   leavesr
107300051107e   2c                   endif
107400051107e   1c                   endif
107500101005     c*
107600060426     c* La coda in notte la accetto solo per schedulazione Settimanale
107700091123     c* nel giorno di sabato o domenica
107800060426     c     'NOTTE'       scan      v2job                                  30
107900060428     c                   if        *in30
108000060428     c                   if        v2mens='SI'   or
108100091123     c                             (v2sett='SI' and v2g1s='SAB') or
108200091123     c                             (v2sett='SI' and v2g1s='DOM')
108300060428     c                   else
108400060426     C                   seton                                        58  99
108500060426     C                   movel     Msg(25)       V2msg
108600060426     C                   leavesr
108700060426e   1c                   endif
108800060428e   1c                   endif
108900020212      *
109000020912      * Flag tipo sottomissione
109100051107if  1C                   if             V2tps  = 'M'
109200111118     C                             and %subst(v0sif:1:6) <> 'FILTRA'
109300020913     C                   seton                                        55  99
109400020913     C                   movel     Msg(08)       V2msg
109500051107     C                   leavesr
109600051107e   1c                   endif
109700020215      *
109800020913      * Profilo obbligatorio
109900051107if  1C                   if        V2ute  = *blanks
110000020913     C                   seton                                        56  99
110100020913     C                   movel     Msg(09)       V2msg
110200051107     C                   leavesr
110300051107e   1c                   endif
110400020918      * Deve esistere nel file profili del modulo base
110500020918     c     V2ute         Chain     Kpprf01l
110600051107if  1c                   if        not %Found(kpprf01l)
110700020918     c                   seton                                        56  99
110800020918     c                   movel     Msg(15)       V2msg
110900051107     c                   leavesr
111000051107e   1c                   endif
111100020913      * nel s.i. FILTRA è possibile solo EDPCEDXXX
111200051107if  1C                   if            v2ute <> 'EDPCEDXXX'
111300021028     c                             and %subst(v0Sif:1:6) = 'FILTRA'
111400020913     C                   seton                                        56  99
111500020913     C                   movel     Msg(10)       V2msg
111600051107     C                   leavesr
111700051107e   1c                   endif
111800020913      * il profilo EDPCEDXXX è valido solo nel s.i. FILTRA
111900051107if  1C                   if            v2ute =  'EDPCEDXXX'
112000021028     c                             and %subst(v0Sif:1:6) <> 'FILTRA'
112100020913     C                   seton                                        56  99
112200020913     C                   movel     Msg(11)       V2msg
112300051107     C                   leavesr
112400051107e   1c                   endif
112500020913      * Per sottomissioni multiple è consentito solo l'utente EDPCEDXXX
112600051107if  1C                   if            v2ute  <> 'EDPCEDXXX'
112700020913     c                             and v2tps  = 'M'
112800020913     C                   seton                                        56  99
112900020913     C                   movel     Msg(12)       V2msg
113000051107     C                   leavesr
113100051107e   1c                   endif
113200020913      *
113300060426      * Orari obbligatori solo x job giornalieri/mensili
113400060426      *  o settimanali e coda frozata <> da ksnotte
113500020913     c                   xfoot     ora           totora
113600060426     c     'NOTTE'       scan      v2job                                  30
113700051107if  1c                   if        Totora = *zeros and
113800060426     c                             not *in30
113900020913     C                   seton                                        60  99
114000020913     C                   movel     Msg(13)       V2msg
114100051107     C                   leavesr
114200051107e   1c                   endif
114300051107if  1c                   if        Totora <> *zeros and
114400060426     c                             *in30
114500091123     c* In KNOTTESK è possibile indicare gli orari
114600091123     c                   if        v2job<>'KSNOTTESK'
114700021025     C                   seton                                        60  99
114800021025     C                   movel     Msg(16)       V2msg
114900051107     C                   leavesr
115000051107e   1c                   endif
115100091123e   1c                   endif
115200020913      * controllo l'ora
115300051107do  1c                   do        15            xx
115400020913     c                   z-add     59            yy
115500060426     c                   z-add     83            zz
115600020913     c                   movel     ora(xx)       Wora
115700020913     c                   move      ora(xx)       Wminuti
115800051107if  2C                   if        Wora  >= 24 or Wminuti >= 60
115900020913     C                   seton                                            99
116000020913     c                   add       xx            yy
116100060426     c                   add       xx            zz
116200020913     C                   eval      *in(yy) = *On
116300060426     C                   eval      *in(zz) = *On
116400020913     C                   movel     Msg(14)       V2msg
116500051107     C                   leavesr
116600060426   x2c                   else
116700060426    3c                   if        ora(xx)>0
116800060426     c                   add       xx            zz                3 0
116900060426     C                   eval      *in(zz) = *On
117000060426e   3C                   endif
117100060426e   2C                   endif
117200051107e   1c                   enddo
117300020212      *
117400020214     C     *in99         cabeq     *on           EndCtrV2
117500020212      *
117600020212     C     EndCtrV2      ENDSR
117700020212
117800020226      *---------------------------------------------------------------*
117900020226      * GESW1  - Gestione videata dati relativi alla trasmissione     *
118000020226      *---------------------------------------------------------------*
118100020226     C     GESW1         BEGSR
118200020226      *
118300020226      * Inizializzazione videata
118400020226if  1C                   if        $CarW1 = *on
118500020226     C                   exsr      CARW1
118600020226     C                   movel     *off          $CarW1
118700020226e   1C                   endif
118800020226      *
118900020226if  1C                   if        *in05
119000020912     C                   write     TB28W1
119100020226     C                   exfmt     PROTECT
119200020226x   1C                   else
119300020912     C                   exfmt     TB28W1
119400020226e   1C                   endif
119500020226     C                   eval      *in99 = *off
119600020301     C                   clear                   W1MSG
119700020226      *
119800020226sel 1C                   select
119900020226      * F12=Ritorno
120000020226w   1C                   when      *inKL
120100020226     C                   exsr      F12W1
120200101005     C                   leavesr
120300020226e   1C                   endsl
120400020226      *
120500020226      * Controllo dati immessi a video
120600020226     C                   exsr      CTRW1
120700020226      *
120800020226      * Aggiornamento se non ci sono errori
120900020301if  1C                   if        not *in99 and *inKF
121000020226     C                   exsr      AGGTBE
121100020226e   1C                   endif
121200020226      *
121300101005     C                   ENDSR
121400020313
121500020226      *---------------------------------------------------------------*
121600020226      * CARW1  - Caricamento dati window                              *
121700020226      *---------------------------------------------------------------*
121800020226     C     CARW1         BEGSR
121900020226      *
122000020226     C                   movea     *zeros        *in(50)
122100020226      *
122200020226sel 1C                   select
122300020226      *
122400020226      * F5=Ripristino
122500020226w   1C                   when      *inKE   and  *in04
122600020226     C                   eval      W1FTT = TBEftt
122700020226      *
122800020226      * F6=Conferma
122900020226w   1C                   when      *inKF
123000020226sel 2C                   select
123100020226      *   Immissione
123200020226w   2C                   when      *in01
123300020313     C                   eval      W1FTT = TBXftt
123400020226      *   Modifica / Ripristino
123500020226w   2C                   when      *in02   or    *in04
123600020313     C                   eval      W1FTT = TBEftt
123700020226e   2C                   endsl
123800020226      *
123900020226      * F16=Annullamento
124000020226w   1C                   when      *inKQ   and  not *in04
124100020313     C                   eval      W1FTT = TBEftt
124200020226      *
124300020226e   1C                   endsl
124400020226      *
124500020226      * Se NON immissione: visualizzo i dati relativi all'ultima
124600020226      *   trasmissione
124700020226if  1C                   if        not *in01
124800020226     C                   eval      W1FLT = TBEflt
124900020226     C                   eval      W1FTR = TBEftr
125000020226if  2C                   if        TBEdtr <> 0
125100020226     C                   clear                   WLBDAT
125200020226     C                   z-add     TBEdtr        G02inv
125300020226     C                   movel     '3'           G02err
125400020226     C                   call      'XSRDA8'
125500020226     C                   parm                    WLBDAT
125600020226     C                   z-add     G02dat        W1DTR
125700020226e   2C                   endif
125800020226e   1C                   endif
125900020226      *
126000020226     C                   ENDSR
126100020226
126200020226      *---------------------------------------------------------------*
126300020226      * CTRW1  - Controllo e decodifica window                        *
126400020226      *---------------------------------------------------------------*
126500020226     C     CTRW1         BEGSR
126600020226      *
126700020226     C                   movea     *zeros        *in(50)
126800020226      *
126900020226     C     EndCtrW1      ENDSR
127000020226
127100020226      *---------------------------------------------------------------*
127200020226      * F21W1  - Tasto funzionale F12 -> Ritorno                      *
127300020226      *---------------------------------------------------------------*
127400020226     C     F12W1         BEGSR
127500020226      *
127600020226     C                   eval      TipVid = '2'
127700020226      *
127800020226     C                   ENDSR
127900020212
128000020212      *---------------------------------------------------------------*
128100020212      * CHNTBE * Aggancio tabella                                     *
128200020212      *---------------------------------------------------------------*
128300020212     C     CHNTBE        BEGSR
128400020212      *
128500020212     C                   movel     T02cod        TBEcod
128600020912     C                   movel(p)  V1AZN         TBEke1
128700090320     C                   movel     V1PRG         TBEke2
128800020212     C                   clear                   TBElin
128900020913     C                   movel     tbxsif        TBEsif
129000020212     C     K05TBE01      chain     TNTBE01L
129100020212      * Se non ho trovato il record con il sistema informativo
129200020212      * che ho in linea, lo abblenco
129300020212if  1C                   if        not %found(TNTBE01L)
129400020212     C                   clear                   TBEsif
129500020212     C     K05TBE01      chain     TNTBE01L
129600020212e   1C                   endif
129700090320      /free
129800090320         // Carico anche l'immagine della KPJBU (se già immessa)
129900090320         clear  k_TBEuni;
130000090320         IF  V1prg <> *blank;
130100090320           k_TBEcod = 'SCK';
130200090320           k_TBEke1 = V1prg;
130300090320           clear  k_TBEke2;
130400090320           clear  k_TBElin;
130500090320           k_TBEsif = TBYsif;
130600090320           chain  %kds(keyTBESCK_P)  TNTBESCK;
130700090320         ENDIF;
130800090320      /end-free
130900020212      *
131000020212     C                   ENDSR
131100020212
131200020212      *---------------------------------------------------------------*
131300020212      * AGGTBE * Aggiornamento tabella                                *
131400020212      *---------------------------------------------------------------*
131500020212     C     AGGTBE        BEGSR
131600090423      *
131700090423     c                   eval      $NewKpjbu = *off
131800020212      *
131900020212sel 1C                   select
132000020212      *
132100020212      * F5=Ripristino
132200020301w   1C                   when      wTasto='05'  and  *in04
132300020212     C                   clear                   TBEatb
132400020212     C                   clear                   TBEftr
132500020212     C                   UPDATE    TNTBE000
132600020212      *
132700020212      * F6=Conferma
132800020301w   1C                   when      wTasto='06'
132900080721     C*
133000090320      /free
133100090320         // - Aggiornamento immagine KPJBU
133200090320         select;
133300090320           // - Nessun parametro inserito/variato
133400110922           when  OkpjKPJBU =  *blank    and   *in01;
133500101005           when  OkpjKPJBU =  k_TBEuni  and   P01opz <> '3';
133600090320           // - Inserimento nuovi parametri
133700101005           when  OkpjKPJBU <> *blank   and   (V1prg  = *blank
133800101005                                        or    P01opz = '3');
133900090320             exsr  sr_RepNumSCK;
134000090320             if  *in99;
134100090320               TipVid = '2';
134200090320               leavesr;
134300090320             endif;
134400090320             exsr  sr_RieTabSCK;
134500090320             write  TNTBE00K;
134600090423             $NewKPJBU = *on;
134700090320           // - Re-Inserimento parametri (precedentemente cancellati)
134800090320           when  OkpjKPJBU <> *blank   and   V1prg <> *blank   and
134900090320                 NOT %found(TNTBESCK);
135000090320             exsr  sr_RieTabSCK;
135100090320             write  TNTBE00K;
135200090320           // - Cancellazione parametri esistenti
135300090320           when  OkpjKPJBU = *blank   and   V1prg <> *blank;
135400090320             delete  TNTBE00K;
135500090320           // - Aggiornamento parametri
135600090320           other;
135700090320             k_TBEatb = *blank;
135800090320             k_TBEuni = OkpjKPJBU;
135900090320             clear  k_TBEftr;
136000090320             update  TNTBE00K;
136100090320         endsl;
136200090320       /end-free
136300090319      *
136400020212sel 2C                   select
136500020212      *   Immissione
136600020212w   2C                   when      *in01
136700020212     C                   exsr      RIETBE
136800020226     C                   clear                   TBEflt
136900020226     C                   clear                   TBEdtr
137000020212     C                   WRITE     TNTBE000                             22
137100020212      *   Modifica / Ripristino
137200020212w   2C                   when      *in02   or    *in04
137300020212     C                   exsr      RIETBE
137400020212     C                   UPDATE    TNTBE000
137500020212e   2C                   endsl
137600020212      *
137700020212      * F16=Annullamento
137800020301w   1C                   when      wTasto='16'  and  not *in04
137900090320      /free
138000090320             // - Cancellazione parametri
138100090320             if  V2prg <> *blank;
138200090320               k_TBEatb = 'A';
138300090320               clear  k_TBEftr;
138400090320               update  TNTBE00K;
138500090320             endif;
138600090320      /end-free
138700020212     C                   movel     'A'           TBEatb
138800020212     C                   clear                   TBEftr
138900020212     C                   UPDATE    TNTBE000
139000020212      *
139100020212e   1C                   endsl
139200090423      /free
139300090423         if  $NewKPJBU = *on;
139400090423           clear  TB28V3;
139500090423           V3ke2  = V2prg;
139600090423           exfmt  TB28V3;
139700090423         endif;
139800090423      /end-free
139900020212      *
140000020212      * Torno alla prima videata che carico come da inizio pgm
140100110603     C                   movel     '3'           TipVid
140200020212     C                   movel     *on           $CarV1
140300020226     C                   movel     *on           $CarV2
140400020226     C                   movel     *on           $CarW1
140500020212     C
140600020212     C                   ENDSR
140700020212
140800020212      *---------------------------------------------------------------*
140900020212      * Riempimento file tabella
141000020212      *---------------------------------------------------------------*
141100020212     C     RIETBE        BEGSR
141200020212      *
141300060427     C                   clear                   numset
141400060427     C                   clear                   TBEatb
141500020212     C                   if        TBXsif <> *blanks
141600020912     C                   movel     TBXsif        TBEsif
141700020212     C                   else
141800020212     C                   clear                   TBEsif
141900020212     C                   endif
142000020212     C                   movel     TBXapl        TBEapl
142100020212     C                   movel     T02cod        TBEcod
142200020913     C                   movel     V2azn         TBEke1
142300090320     C                   move      V2prg         TBEke2
142400020212      *
142500020913     C                   clear                   DSCH
142600021025     C                   eval      d§SCHpri  =  V2prio
142700021025     C                   eval      d§SCHdes  =  V2AZND
142800020913     C                   eval      d§SCHese  =  V2ESE
142900121008     C                   eval      d§SCHSosp =  V2Sosp
143000090319      *
143100060426     c                   select
143200060426     c                   when      v2gior='SI'
143300060426     c                   eval      d§schfre='G'
143400060426     c                   when      v2sett='SI'
143500060426     c                   eval      d§schfre='S'
143600060426     c                   when      v2mens='SI'
143700060426     c                   eval      d§schfre='M'
143800060426     c                   endsl
143900090319      *
144000090319      *
144100060426     C****               eval      d§SCHfre  =  V2FRE
144200050215     C                   eval      d§SCHggm  =  V2ggm
144300060427     c                   z-add     1             cc
144400060427     c     v2g1s         lookup    giorset(cc)                            30
144500060427     c   30              movel     cc            d§schg1s
144600060427     c  n30              clear                   d§schg1s
144700060427     c
144800090319      *
144900060426     c* Ordino i giorni della settimana per frequenza settimanale
145000060426     c                   z-add     1             xx
145100060426     c                   do        7             gg
145200060426     c                   if        gggiorn(GG)<>'   '
145300060426     c                   z-add     1             yy
145400060426     c     ggGiorn(GG)   lookup    giorset(YY)                            30
145500060426     c                   z-add     yy            numset(XX)
145600060426     c                   add       1             xx
145700060426     c                   endif
145800060426     c                   enddo
145900060426     c                   sorta     numset
146000090319      *
146100060426     C                   clear                   d§SCHg1g
146200060426     C                   clear                   d§SCHg2g
146300060426     C                   clear                   d§SCHg3g
146400060426     C                   clear                   d§SCHg4g
146500060426     C                   clear                   d§SCHg5g
146600060426     C                   clear                   d§SCHg6g
146700060426     C                   clear                   d§SCHg7g
146800060426     c                   do        7             xx
146900060426     c                   if        numset(XX)>0
147000060426     c                   if        d§schg1g='   '
147100060427     c                   movel     numset(XX)    d§schg1g
147200060426     c                   iter
147300060426     c                   endif
147400060426     c                   if        d§schg2g='   '
147500060427     c                   movel     numset(XX)    d§schg2g
147600060426     c                   iter
147700060426     c                   endif
147800060426     c                   if        d§schg3g='   '
147900060427     c                   movel     numset(XX)    d§schg3g
148000060426     c                   iter
148100060426     c                   endif
148200060426     c                   if        d§schg4g='   '
148300060427     c                   movel     numset(XX)    d§schg4g
148400060426     c                   iter
148500060426     c                   endif
148600060426     c                   if        d§schg5g='   '
148700060427     c                   movel     numset(XX)    d§schg5g
148800060426     c                   iter
148900060426     c                   endif
149000060426     c                   if        d§schg6g='   '
149100060427     c                   movel     numset(XX)    d§schg6g
149200060426     c                   iter
149300060426     c                   endif
149400060426     c                   if        d§schg7g='   '
149500060427     c                   movel     numset(XX)    d§schg7g
149600060426     c                   iter
149700060426     c                   endif
149800060426     c                   endif
149900060426     c                   enddo
150000090319      *
150100020913     C                   eval      d§SCHjob  =  V2JOB
150200020913     C                   eval      D§SCHor1  =  V2OR1
150300020913     C                   eval      D§SCHor2  =  V2OR2
150400020913     C                   eval      D§SCHor3  =  V2OR3
150500020913     C                   eval      D§SCHor4  =  V2OR4
150600020913     C                   eval      D§SCHor5  =  V2OR5
150700020913     C                   eval      D§SCHor6  =  V2OR6
150800020913     C                   eval      D§SCHor7  =  V2OR7
150900020913     C                   eval      D§SCHor8  =  V2OR8
151000020913     C                   eval      D§SCHor9  =  V2OR9
151100020913     C                   eval      D§SCHor10 =  V2OR10
151200020913     C                   eval      D§SCHor11 =  V2OR11
151300020913     C                   eval      D§SCHor12 =  V2OR12
151400020913     C                   eval      D§SCHor13 =  V2OR13
151500020913     C                   eval      D§SCHor14 =  V2OR14
151600020913     C                   eval      D§SCHor15 =  V2OR15
151700020913     C                   eval      D§SCHtps  =  V2TPS
151800020913     C                   eval      D§SCHute  =  V2UTE
151900020913     C                   movel(p)  DSCH          TBEuni
152000020212      *
152100020226     C                   movel     W1FTT         TBEftt
152200020212     C                   z-add     TBXflt        TBEflt
152300020212     C                   clear                   TBEftr
152400090319      *
152500020212     C                   ENDSR
152600090319
152700090320      /free
152800090320       //---------------------------------------------------------------
152900090320       // Reperimento progressivo per tabella "SCK"
153000090320       //---------------------------------------------------------------
153100090320       BEGSR  sr_RepNumSCK;
153200090320
153300090320         clear  TRUL33ds;
153400090320         I33tla = 'L';
153500090320         I33ope = *zero;
153600090320         I33cnu = 500;
153700090320         I33num = 1;
153800090320         KPJBU  = TRUL33ds;
153900090320
154000090320         trul33r ( KPJBA );
154100090320
154200090320         TRUL33ds = KPJBU;
154300090320         if  O33err <> *zero;
154400090320           *in99 = *on;
154500090320           V2msg = O33msg;
154600090320           leavesr;
154700090320         endif;
154800090320
154900090320         V2prg = %editc( O33nri : 'X' );
155000090320
155100090320       ENDSR;
155200090319
155300090320       //---------------------------------------------------------------
155400090320       // Riempimento tabella "SCK"
155500090320       //---------------------------------------------------------------
155600090320       BEGSR  sr_RieTabSCK;
155700090320
155800090320         clear  k_TBEatb;
155900090320         if  TBYsif <> *blank;
156000090320           k_TBEsif =  TBYsif;
156100090320         else;
156200090320           clear  k_TBEsif;
156300090320         endif;
156400090320         k_TBEapl = TBYapl;
156500090320         k_TBEcod = 'SCK';
156600101005         k_TBEke1 = V2prg;
156700090320         //k_TBEke2 = V2azn;
156800090320         clear  k_TBEke2;
156900090320         k_TBEuni = OkpjKPJBU;
157000090320
157100090320         k_TBEftt = W1FTT;
157200090320         k_TBEflt = TBYflt;
157300090320         clear  k_TBEftr;
157400090320
157500090320       ENDSR;
157600090320
157700090320      /end-free
157800090320
157900020212** -OPZ-      *
158000020212  Inserimento
158100020212    Modifica
158200020212     Copia
158300020212  Annullamento
158400020212Visualizzazione
158500020212   ANNULLATO
158600060426** GIORSET-  GIORNI DELLA settimana                                          *
158700060426LUNMARMERGIOVENSABDOM
158800020212** -MSG-                                                                     *
158900020913Immettere il sistema informativo                                               01
159000020913Sistema informativo errato                                                     02
159100020913Immettere l'azione                                                             03
159200020913Immettere la descrizione                                                       04
159300020913Immettere il flag "da eseguire"                                                05
159400040708Esecuzione non valida per il S.I.                                              06
159500020913Immettere la frequenza                                                         07
159600020913Tipo sottomissione multipla possibile solo x s.i. FILTRA                       08
159700020913Immettere l'utente                                                             09
159800020913Nel s.i. FILTRA l'utente deve essere EDPCEDXXX                                 10
159900020913Utente EDPCEDXXX valido solo x s.i. FILTRA                                     11
160000020913Per sottomissioni multiple l'utente deve essere EDPCEDXXX                      12
160100060426Immettere almeno 1 orario x sched.Giornaliera/Mensile/Settimanale non in Notte 13
160200020913Orario errato                                                                  14
160300020918Utente errato - non esiste sul modulo base                                     15
160400091123Per lavori Settimanali in NOTTE (no"SK) non bisogna impostare orari            16
160500051107Coda lavori inesistente                                                        17
160600060426Selezionare soltanto un tipo di frequenza                                      18
160700060426Per frequenza MENSILE, indicare il giorno del mese                             19
160800060426NON richiesta frequenza Mensile: eliminare il giorno del mese                  20
160900060426NON richiesta frequenza Giornaliera:eliminare relativi giorni della settimana  21
161000060426Giorno della settimana errato                                                  22
161100060426Giorno della settimana già immesso                                             23
161200060426NON richiesta frequenza Settimanale:eliminare relativo giorno della settimana  24
161300091123Elaborazioni in coda NOTTE valide solo x sched.Mensile o Settim.SABATO/DOMENIC 25
161400101005Azione già schedulata                                                          26
