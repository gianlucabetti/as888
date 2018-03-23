000100070329      * FNLRSBR *-----------------------------------------------------*
000200070329      *?STAMPA LDV DI BACKUP (IN ARRIVO) - BATCH                     ?*
000300070329      *---------------------------------------------------------------*
000400070329
000500070329     h decedit('0,') datedit(*dmy/)
000600070329
000700070329      *---------------------------------------------------------------*
000800070329
000900070329     fFNARB54L  if   e           k disk
001000070330     fFIARG01L  if   e           k disk
001100070329      *
001200070329     fWFWLRS2L  Uf A e           k disk
001300070329
001400070329      *---------------------------------------------------------------*
001500070329
001600070329      *
001700070329      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - -?*
001800070329      *
001900070329      *
002000070329      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
002100070329      *
002200070329      *
002300070329      *?  S T R U T T U R E   D A T I   - - - - - - - - - - - - - - -?*
002400070329      *
002500070329      * - Parametri
002600070329     d KPJBA         e ds
002700070329     d FNLRSBds      e ds                  inz
002800070329      *
002900070329      * - Parametri x Controllo profilo utenti
003000070329     d TIBS34ds      e ds
003100070329      * - Ds di riferimento al file esterno AZUTE00F
003200070329     d AZUTEds       e ds                       extname(AZUTE00F)
003300070329      * - Ds per dati organigramma
003400070329     d dDatiUte      e ds
003500070329      *
003600070329      * - Parametri per FNLV55R (reperimento terminal arr/par/£6)
003700070329     d FNLV55ds      e ds                  inz
003800070329     d  D55tpt       e                     inz('6')
003900070329      *
004000070329      * - 143ª descrizione dell'organigramma
004100070329     d Og143         e ds                  inz
004200070330      *
004300070330      * - Parametri per TRUL06R (Caricamento £X)
004400070330     d TRUL06ds      e ds                  inz
004500070330     d  D06cod       e                     inz('£6')
004600070330     d  D06tla       e                     inz('L')
004700070330     d  $£6                    1     90  0 inz  dim(30)
004800070330      *
004900070330      * - Parametri per FIDG16R (verifica blocchi)
005000070330     d FIDG16ds      e ds                  inz
005100070330     d  D16telai     e                     inz('R')
005200070330     d  D16consi     e                     inz('S')
005300070329      *
005400070330      * - Parametri per FNLRS3R (stampa LdV in arrivo - NO DDT)
005500070329     d FNLRS1ds      e ds                  inz
005600070330     d  DS1ris       e                     inz('N')
005700070330     d  DS1zna       e                     inz(*hival)
005800070329      *
005900070329      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
006000070329      *
006100070330      * - Indici di schiera
006200070330     d xx              s              3  0 inz
006300070330      * - Parametri per pgm. XGIOSE1
006400070330     d Wggmmaa         s              6    inz
006500070330     d Wgioset         s              1    inz
006600070329      * - Variabili di comodo
006700070330     d wCount          s              9  0 inz
006800070330     d wData_6         s              6  0 inz
006900070330     d wData_Dmy       s               d   inz  datfmt(*dmy)
007000070413      *
007100070413     d wDCM            s                   inz  like(ARBdcm)
007200070329      *
007300070329      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
007400070329      *
007500070330      * - FNARB54L
007600070329     c     K02ARB54      klist
007700070330     c                   kfld                    $£6(xx)
007800070413     c                   kfld                    Wdcm
007900070330      * - FIARG01L
008000070330     c     K04ARG01      klist
008100070330     c                   kfld                    ARBaas
008200070330     c                   kfld                    ARBlnp
008300070330     c                   kfld                    ARBnrs
008400070330     c                   kfld                    ARBnsp
008500070330      * - WFWLRS2L
008600070330     c     K05WLRS2      klist
008700070330     c                   kfld                    WLRris
008800070330     c                   kfld                    ARBaas
008900070330     c                   kfld                    ARBlnp
009000070330     c                   kfld                    ARBnrs
009100070330     c                   kfld                    ARBnsp
009200070329
009300070329      *---------------------------------------------------------------*
009400070329      *? RIEPILOGO INDICATORI                                        ?*
009500070329      *---------------------------------------------------------------*
009600070329      * 10   - Comodo                                                 *
009700070329      *---------------------------------------------------------------*
009800070329
009900070329     c     *Entry        plist
010000070329     c                   parm                    KPJBA
010100070330      *
010200070330      * Operazioni iniziali
010300070330     c                   exsr      sr_Start
010400070329      *
010500070330      * Elaborazione (per ogni filiale della £6)
010600070330     c                   eval      xx       = 1
010700070330do  1c                   DOW       $£6(xx)  > *zeros
010800070330      *
010900070330      * - Registrazione in WFWLRS0F delle spedizioni da stampare
011000070330     c                   clear                   wCount
011100070329     c     K02ARB54      setll     FNARB000
011200070329     c     K02ARB54      reade     FNARB000
011300070330do  2c                   dow       NOT  %eof(FNARB54L)
011400070329     c                   exsr      Elab_FNARB
011500070329     c     K02ARB54      reade     FNARB000
011600070330e   2c                   enddo
011700070330      *
011800070330      * - Chiamata al pgm. FNLRS7R per la stampa (tramite FNLSB5R)
011900070330if  2c                   if        wCount   > *zeros
012000070330     c                   exsr      Call_FNLRS7
012100070330e   2c                   endif
012200070330      *
012300070330     c                   add       1             xx
012400070330e   1c                   ENDDO
012500070329      *
012600070329      * Fine
012700070330     c                   eval      *inLR    = *on
012800070329      *
012900070329      *---------------------------------------------------------------*
013000070329      *?Operazioni iniziali                                          ?*
013100070329      *---------------------------------------------------------------*
013200070330     c     sr_Start      BEGSR
013300070330      *
013400070330     c                   movel     KPJBU         FNLRSBds
013500070329      *
013600070329      * Reperimento dati job
013700070329     c     *dtaara       define    §azute        AZUTEds
013800070329     c     *dtaara       define    §datiute      dDATIUTE
013900070329      *
014000070329     c                   in(E)     *dtaara
014100070329     c                   IF        %ERROR or RSUT = *blanks
014200070329     c                   clear                   Tibs34Ds
014300070329     c                   call      'TIBS34R'
014400070329     c                   parm                    Tibs34Ds
014500070329     c                   in        *dtaara
014600070329     c                   ENDIF
014700070330      *
014800070330      * Impostazione data di stampa nel formato ggmmaa
014900070330     c     *iso          move      DSBdcm        wData_Dmy
015000070330      *
015100070330      * Reperimento del giorno della settimana da stampare
015200070330     c                   move      wData_Dmy     wData_6
015300070330     c                   move      wData_6       Wggmmaa
015400070330     c                   call      'XGIOSE1'
015500070330     c                   parm                    Wggmmaa           6
015600070330     c                   parm      *blanks       Wgioset           1
015700070330      *
015800070330      * Reperimento capofila (£6)
015900070330     c                   reset                   FNLV55ds
016000070330     c*** già così:      eval      D55tpt   = '6'
016100070330     c                   eval      D55lin   = DSBfgs
016200070330     c                   eval      D55drf   = DSBdcm
016300070330     c                   call      'FNLV55R'
016400070330     c                   parm                    FNLV55ds
016500070330if  1c                   if        D55err  <> *blanks
016600070330     c                   eval      D55tfa   = DSBfgs
016700070330e   1c                   endif
016800070330      *
016900070330      * Reperimento filiali della £6
017000070330     c                   reset                   TRUL06ds
017100070330     c                   movel(p)  DSBfgs        D06key
017200070330     c                   movel     TRUL06ds      KPJBU
017300070330     c                   call      'TRUL06R'
017400070330     c                   parm                    KPJBA
017500070330     c                   movel     KPJBU         TRUL06ds
017600070329      *
017700070329     c                   ENDSR
017800070329      *
017900070329      *---------------------------------------------------------------*
018000070329      *?Elaborazione singola spedizione                              ?*
018100070329      *---------------------------------------------------------------*
018200070329     c     Elab_FNARB    BEGSR
018300070329      *
018400070330      * Verifica se spedizione assegnabile
018500070330     c                   exsr      Call_FIDG16
018600070329      *
018700070330      * SE da stampare: scrive record nel file WFWLRS0F
018800070330      *  (per li richiamo del pgm. FNLRS7R)
018900070413if  1c                   if        D16conso = 'S'
019000070330     c                   exsr      Wrt_WFWLRS
019100070329e   1c                   endif
019200070329      *
019300070329     c                   ENDSR
019400070329      *
019500070329      *---------------------------------------------------------------*
019600070330      *?Richiamo del pgm FIDG16R x verifica se spedizione assegnabile?*
019700070329      *---------------------------------------------------------------*
019800070330     c     Call_FIDG16   BEGSR
019900070330      *
020000070330     c                   reset                   FIDG16ds
020100070330      *
020200070330     c                   eval      D16aasi  = ARBaas
020300070330     c                   eval      D16lnpi  = ARBlnp
020400070330     c                   eval      D16nrsi  = ARBnrs
020500070330     c                   eval      D16nspi  = ARBnsp
020600070330     c                   eval      D16dfvi  = DSBdcm
020700070830     c                   eval      D16dtai  = DSBdla
020800070830     c                   eval      D16orai  = DSBola
020900071210     c                   eval      D16ldcri = DSBdcrp
021000070330      *
021100070330     c                   movel(p)  FIDG16ds      KPJBU
021200070330     c                   call      'FIDG16R'
021300070330     c                   parm                    KPJBA
021400070330     c                   movel     KPJBU         FIDG16ds
021500070330      *
021600070330     c                   ENDSR
021700070330      *
021800070330      *---------------------------------------------------------------*
021900070330      *?Memorizzazione bolla nel work-file WFWLRS0F                  ?*
022000070330      *---------------------------------------------------------------*
022100070330     c     Wrt_WFWLRS    BEGSR
022200070330      *
022300070330     c     K05WLRS2      chain     WFWLRS00                             10
022400070330      *
022500070330      * SE record allocato: non si fa nulla!!!
022600070330if  1c                   if        *in10
022700070330     c                   leavesr
022800070330e   1c                   endif
022900070330      *
023000070330      * SE record nuovo: vanno impostati anche i campi chiave
023100070330if  1c                   IF        NOT  %found(WFWLRS2L)
023200070330     c                   clear                   WFWLRS00
023300070330     c*** già così:      clear                   WLRris
023400070330     c                   eval      WLRaas   = ARBaas
023500070330     c                   eval      WLRlnp   = ARBlnp
023600070330     c                   eval      WLRnrs   = ARBnrs
023700070330     c                   eval      WLRnsp   = ARBnsp
023800070330     c                   eval      WLRlna   = ARBlna
023900070330     c                   eval      WLRfgs   = DSBfgs
024000070330     c                   eval      WLRznc   = ARBznc
024100070330      * - stato giro, codice giro Tellus e sequenza consegna
024200070330     c     K04ARG01      chain     FIARG000
024300070330if  2c                   if        %found(FIARG01L)
024400070330     c                   eval      WLRtgi   = ARGtgi
024500070330     c                   eval      WLRcgi   = ARGcgi
024600070330     c                   eval      WLRsqg   = ARGsqg
024700070330e   2c                   endif
024800070330      * + Conteggio delle LdV da stampare (per LNA della £6)
024900070330     c                   add       1             wCount
025000070330e   1c                   ENDIF
025100070330      *
025200070330      * Impostazione flag abilitazione
025300070330if  1c                   if        ARBama   = *zeros
025400070330     c                   eval      WLRabl   = 1
025500070330x   1c                   else
025600070330     c                   clear                   WLRabl
025700070330e   1c                   endif
025800070330      *
025900070330      * Impostazione dell'asterisco nei seguenti casi:
026000070330     c                   clear                   WLRast
026100070330      * - assegnati non tassati
026200070330if  1c***                if             ARBaca  = *blanks
026300070330      * - c/assegni non abilitati
026400070330if  1c                   if             ARBacc  = *blanks
026500070330      * - appuntamento
026600070330     c                             or   ARBtc1  = 'A'
026700070330     c                             or   ARBtc2  = 'A'
026800070330      * - con richieste (data consegna richiesta > data LdV)
026900070330     c                             or   ARBtcr  = *blanks
027000070330     c                             and  ARBdcr  > DSBdcm
027100070330     c                             or   ARBtcr  = 'D'
027200070330     c                             and  ARBdcr >= DSBdcm
027300070330      * - turni di chiusura = data LdV
027400070330      *   (considero anche le mezze giornate;
027500070330      *    quindi: se chiuso tutte le mattine stampo sempre l'"*")
027600070330     c                             or   %subst(ARBgc1:1:1)  = *blanks
027700070330     c                             and  %subst(ARBgc1:2:1) <> *blanks
027800070330     c                             or   %subst(ARBgc2:1:1)  = *blanks
027900070330     c                             and  %subst(ARBgc2:2:1) <> *blanks
028000070330     c                             or   %subst(ARBgc2:1:1)  = Wgioset
028100070330     c                             or   %subst(ARBgc2:1:1)  = Wgioset
028200070330     c                   eval      WLRast   = '*'
028300070330e   1c                   endif
028400070330      *
028500070330      * Reimpostazione anche della ragione sociale del destinatario
028600070330     c                   eval      WLRrsd   = ARBrsd
028700070330      *
028800070330      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
028900070330      *
029000070330      * Scrittura/Aggiornamento record WrkF
029100070330if  1c                   if        NOT  %found(WFWLRS2L)
029200070330      *                  __________________
029300070330     c                   write     WFWLRS00
029400070330      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
029500070330x   1c                   else
029600070330      *                  __________________
029700070330     c                   update    WFWLRS00
029800070330      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
029900070330e   1c                   endif
030000070330      *
030100070330     c                   ENDSR
030200070330      *
030300070330      *---------------------------------------------------------------*
030400070330      *?Richiamo del pgm FNLRS7R x stampa LdV in arrivo con Tellus   ?*
030500070330      *---------------------------------------------------------------*
030600070330     c     Call_FNLRS7   BEGSR
030700070330      *
030800070330      * Richiamo pgm che elabora il work-file WFWLRS0F per richiamare
030900070330      *  la stampa LdV (SE scritto almeno un record nel WrkF)
031000070330     c                   reset                   FNLRS1ds
031100070330     c                   eval      DS1lna   = $£6(xx)
031200070330     c*** già così:      eval      DS1ris   = 'N'
031300070330     c***  "    "        eval      DS1pck   = *blanks
031400070330     c***  "    "        eval      DS1ddt   = *blanks
031500070330     c***  "    "        eval      DS1ass   = *blanks
031600070330     c***  "    "        eval      DS1zna   = *hival
031700070330     c                   eval      DS1fgs   = DSBfgs
031800070330     c                   eval      DS1D90psl    = DSBpsl
031900070330     c                   eval      DS1D90mdb    = DSBmdb
032000070330     c                   eval      DS1D90mdb4   = DSBmdb4
032100070330     c                   eval      DS1D90mdb5   = DSBmdb5
032200070330     c                   eval      DS1D90prb    = DSBprb
032300070330     c                   eval      DS1D90prb4   = DSBprb4
032400070330     c                   eval      DS1D90prb5   = DSBprb5
032500070330      *
032600070330     c                   movel(p)  FNLRS1ds      KPJBU
032700070330     c                   call      'FNLRS7R'
032800070330     c                   parm                    KPJBA
032900070330      *
033000070330     c                   ENDSR
