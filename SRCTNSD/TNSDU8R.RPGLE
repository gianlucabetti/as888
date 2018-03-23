000100020322     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020322      *------------------------------------------------------------------------*
000300020322      * STAMPA MOVIMENTI UNITA' EDP - RICHIESTA PARAMETRI
000400020322      *------------------------------------------------------------------------*
000500020322
000600020322      *------------------------------------------------------------------------*
000700020322      * DATA BASE
000800020322      *------------------------------------------------------------------------*
000900941115     FTNSDU8D   CF   E             WORKSTN INFDS(DEVDS1)
001000941115     F                                     SFILE(SDU8DVM:MSGULT)
001100020529     FAZORG21l  IF   E           K DISK
001200020529     fazorg01l  if   e           k disk
001300020322
001400020322      *------------------------------------------------------------------------*
001500020322      * SCHIERE
001600020322      *------------------------------------------------------------------------*
001700931201     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
001800940105     D CMD             S             80    DIM(5) CTDATA PERRCD(1)              *COMANDI
001900930806     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002000020322      * SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002100930803     D FFU             S              6    DIM(31)                              *FUNZIONE
002200930803     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002300930803     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002400020322      * TABELLA DESCRIZIONI A VIDEO
002500930802     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
002600020322      * TABELLA TASTI FUNZIONALI PREMUTI
002700930802     D KEYS            S              1    DIM(40)
002800020322
002900020322      *------------------------------------------------------------------------*
003000020322      * INPUT
003100020322      *------------------------------------------------------------------------*
003200020322      * RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003300930723     D DSFUN         E DS
003400930803     D  FUNRIG                 1     40
003500020322      * PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
003600930127     D DSMSG         E DS
003700020322      * DS CONTROLLA DATA (8)
003800971013     D WLBDA8          DS
003900971013     D  G08DAT                 1      8  0
004000971013     D  G08INV                 9     16  0
004100971013     D  G08ERR                17     17
004200971013     D  G08TGI                18     22  0
004300020322      * DS PER GESTIONE TASTI FUNZIONALI
004400930802     D DSKEYS        E DS
004500930803     D  KEYRIG                 1     40
004600020322      * RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
004700930806     D                SDS
004800930806     D  USER                 254    263
004900020322      * RIDEFINIZIONE INFDS DEL FILE VIDEO
005000000000     D DEVDS1          DS
005100000000     D  KEY                  369    369
005200930721     D  CURSOR               370    371
005300020322      * ARCHITETTURA
005400930922     D KPJBA         E DS
005500020322      * PARAMETRI DI LANCIO
005600931008     D PARAM           DS                  INZ
005700971013     D  PARORD                 1      1  0
005800971013     D  PARTIP                 2      3  0
005900971013     D  PARCOD                 4      8  0
006000971013     D  PARMAG                 9     11  0
006100971013     D  PARDTM                12     19  0
006200971013     D  PARDT2                20     27  0
006300971013     D  PARCAU                28     31
006400971013     D  DECTIP                32     61
006500971013     D  DECMAG                62     91
006600971013     D  DECDTM                82     99  0
006700971013     D  DECDT2               100    107  0
006800971013     D  DECCAU               108    127
006900020322      * PARAMETRI PER LANCIO ALTRI PGM DELLA PROCEDURA
007000931201     D DSUNIT        E DS
007100020322
007200020322     d tibs02ds      e ds
007300020709
007400020709     d ddatiute      e ds
007500020709     d azuteds       e ds                  extname(azute00f)
007600020709     d tibs34ds      e ds                  inz
007700020322
007800931201     C*------------------------------------------------------------------------*
007900971013     C* MAIN LINE
008000931201     C*------------------------------------------------------------------------*
008100931008     C*
008200000000     C* CICLO FINO A CHE:
008300930922DO  1C     $FINE         DOUEQ     '1'
008400000000     C*
008500931008     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
008600000000     C*
008700930721     C* CONTROLLA GESTIONE VIDEO
008800930922IF  2C     $GES          IFEQ      'V1'
008900930806     C                   EXSR      GESV1                                        *PARAMETRI
009000930922E   2C                   ENDIF
009100000000     C*
009200930922E   1C                   ENDDO
009300940105     C*
009400940105     C* SE E' STATO PREMUTO F03 IMPOSTO FLAG DELLA DS DI PROCEDURA
009500940105IF  1C     $F03          IFEQ      '1'
009600940105     C                   MOVEL     '1'           DSUF03
009700940105E   1C                   ENDIF
009800000000     C*
009900931018     C* FINE MAIN DEL PGM
010000940105     C                   MOVEL     DSUNIT        KPJBU
010100000000     C                   SETON                                        LR
010200931201     C*------------------------------------------------------------------------*
010300931201     C*?GESV1 - GESTIONE VIDEO PARAMETRI
010400931201     C*------------------------------------------------------------------------*
010500930922     C     GESV1         BEGSR
010600930922     C*
010700930922IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
010800930922     C                   EXSR      CARV1                                        *CARICA VIDEO
010900930922E   1C                   ENDIF
011000930922     C*
011100931018IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
011200931018     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
011300931018     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
011400931018E   1C                   ENDIF
011500931018     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
011600930922     C*
011700941115     C                   EXFMT     SDU8DV1                                      *PARAMETRI
011800930922     C*
011900930922     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
012000020717
012100020717      * Se Profilo non OK devo uscire dal pgm
012200020717     c                   If        *In76
012300020717     c                   Eval      $err = '1'
012400020717     c                   Eval      $fine = '1'
012500020717     c                   EndIf
012600930922     C*
012700930922IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
012800931008SL  2C                   SELECT
012900930922     C     KEY           WHENEQ    ENTER
013000930922     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
013100930922     C     KEY           WHENEQ    F6
013200930922     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
013300940105     C     KEY           WHENEQ    F22
013400940105     C                   EXSR      F22GES                                       *GESTIONE STAMPE
013500930922     C                   OTHER
013600930922     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
013700930922E   2C                   ENDSL
013800930922E   1C                   ENDIF
013900930922     C*
014000930922     C                   ENDSR
014100931201     C*------------------------------------------------------------------------*
014200931201     C*?CARV1 - CARICA VIDEO PARAMETRI
014300931201     C*------------------------------------------------------------------------*
014400930806     C     CARV1         BEGSR
014500930713     C*
014600930806     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
014700930713     C*
014800930804     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
014900930804     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
015000930713     C*
015100941115     C                   WRITE     SDU8DVT                                      *TESTATA
015200941115     C                   WRITE     SDU8DVF                                      *FUNZIONI
015300930713     C*
015400930713     C                   ENDSR
015500931201     C*------------------------------------------------------------------------*
015600931201     C*?ENTV1 - GESTIONE TASTO ENTER VIDEO 1 (PARAMETRI)
015700931201     C*------------------------------------------------------------------------*
015800930806     C     ENTV1         BEGSR
015900930806     C*
016000930806     C* CONTROLLA I CAMPI IMMESSI
016100930923     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
016200930804     C*
016300930804     C                   ENDSR
016400931201     C*------------------------------------------------------------------------*
016500931201     C*?F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
016600931201     C*------------------------------------------------------------------------*
016700930126     C     F06GES        BEGSR
016800930923     C*
016900930924     C* CONTROLLA I CAMPI IMMESSI
017000930924     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
017100930923     C*
017200930923     C* NON CI SONO ERRORI
017300930923IF  1C     $ERR          IFEQ      '0'
017400930923     C*
017500930923     C* LANCIO PROGRAMMA DI STAMPA
017600960201     C                   CLEAR                   PARAM
017700931202     C                   MOVEL     V1CORD        PARORD                         *TIPO ORDINAMENTO
017800931202     C                   MOVEL     V1CTIP        PARTIP                         *TIPO UNITA'
017900931213     C                   Z-ADD     V1CCOD        PARCOD                         *CODICE UNITA'
018000931202     C                   MOVEL     V1CMAG        PARMAG                         *MAGAZZINO
018100931213     C                   Z-ADD     DTMV1C        PARDTM                         *DATA MOVIM DAL A/M/
018200931213     C                   Z-ADD     DT2V1C        PARDT2                         *DATA MOVIM AL  A/M/
018300931213     C                   MOVEL     V1CCAU        PARCAU                         *CAUSALE MOVIMENTO
018400960201     C                   MOVEL     V1DTIP        DECTIP                         *DECODIFICA TIPO
018500960201     C                   MOVEL     V1DMAG        DECMAG                         *DECODIFICA MAGAZZIN
018600960201     C                   Z-ADD     V1CDTM        DECDTM                         *DATA MOVIM DAL G/M/
018700960201     C                   Z-ADD     V1CDT2        DECDT2                         *DATA MOVIM AL  G/M/
018800930923     C*
018900960202     C                   MOVEL     'SDU9'        KCOAZ
019000960202     C                   MOVEL     PARAM         KPJBU
019100960202     C                   CALL      'BCH10'
019200930923     C                   PARM                    KPJBA
019300930923     C*
019400940105     C                   MOVEL     MSG(12)       $MSG                           *STAMPA LANCIATA
019500940105     C                   EXSR      CARMSG
019600931215     C*
019700930923E   1C                   ENDIF
019800930127     C*
019900000000     C                   ENDSR
020000940105     C*------------------------------------------------------------------------*
020100940105     C*?F22GES -GEST. TASTO FUNZ. F22 ==> STAMPE
020200940105     C*------------------------------------------------------------------------*
020300940105     C     F22GES        BEGSR
020400940105     C*
020500940105     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
020600940105     C                   Z-ADD     80            LENGH            15 5
020700940105     C                   CALL      'QCAEXEC'
020800940105     C                   PARM                    CMD(1)
020900940105     C                   PARM                    LENGH
021000940105     C*
021100940105     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
021200940105     C*
021300940105     C                   ENDSR
021400931201     C*------------------------------------------------------------------------*
021500931201     C*?DECODI - CONTROLLA CAMPI E DECODIFICA
021600931201     C*------------------------------------------------------------------------*
021700930906     C     DECODI        BEGSR
021800930205     C*
021900931201     C* REIMPOSTO INDICATORI E VARIABILI DI CONTROLLO
022000931124     C                   SETOFF                                       606162
022100931201     C                   SETOFF                                       636465
022200931213     C                   SETOFF                                       66
022300930712     C*
022400931201     C* TIPO ORDINAMENTO
022500931202IF  1C     V1CORD        IFEQ      1
022600931202     C     V1CORD        OREQ      2
022700931202X   1C                   ELSE
022800930922     C                   MOVEL     '1'           $ERR
022900930922     C                   MOVEL     MSG(7)        $MSG
023000930922     C                   EXSR      CARMSG
023100931201     C                   SETON                                        60
023200931201E   1C                   ENDIF
023300931201     C*
023400931201     C* TIPO UNITA'
023500931201IF  1C     $ERR          IFEQ      '0'
023600931202IF  2C     V1CTIP        IFNE      *BLANKS                                      *SELEZIONATO
023700931201     C     V1CTIP        ANDNE     *ZEROS
023800931201     C                   MOVEL     V1CTIP        WTIP              2
023900931201     C                   MOVEL     *BLANKS       WDTIP            30
024000931201     C                   EXSR      CHKTIP
024100931202     C                   MOVEL     WTIP          V1CTIP
024200931202     C                   MOVEL     WDTIP         V1DTIP
024300931202     C*
024400931202IF  3C     $ERR          IFEQ      '1'                                          *ERRORE
024500931202     C                   MOVEL     MSG(8)        $MSG
024600931202     C                   EXSR      CARMSG
024700931202     C                   SETON                                        61
024800931202E   3C                   ENDIF
024900931202     C*
025000931202X   2C                   ELSE                                                   *NON SELEZIONATO
025100931201     C                   MOVEL     *ZEROS        V1CTIP
025200931201     C                   MOVEL     *BLANKS       V1DTIP
025300931201E   2C                   ENDIF
025400931201E   1C                   ENDIF
025500931213     C*
025600931213     C* CODICE UNITA'
025700931213IF  1C     $ERR          IFEQ      '0'
025800931213IF  2C     V1CCOD        IFGT      *ZEROS                                       *SELEZIONATO
025900931213IF  3C     V1CTIP        IFEQ      *BLANKS
026000931213     C     V1CTIP        OREQ      *ZEROS
026100931213     C                   MOVEL     '1'           $ERR
026200931213     C                   MOVEL     MSG(11)       $MSG
026300931213     C                   EXSR      CARMSG
026400931213     C                   SETON                                        61
026500931213E   3C                   ENDIF
026600931213E   2C                   ENDIF
026700931213E   1C                   ENDIF
026800931202     C*
026900931202     C* MAGAZZINO UBICAZIONE
027000931202IF  1C     $ERR          IFEQ      '0'
027100931202IF  2C     V1CMAG        IFNE      *BLANKS                                      *SELEZIONATO
027200931202     C     V1CMAG        ANDNE     *ZEROS
027300931202     C                   MOVEL     V1CMAG        WMAG              3
027400931202     C                   MOVEL     *BLANKS       WDMAG            30
027500931202     C                   EXSR      CHKMAG
027600931202     C                   MOVEL     WMAG          V1CMAG
027700931202     C                   MOVEL     WDMAG         V1DMAG
027800931202     C*
027900931202IF  3C     $ERR          IFEQ      '1'                                          *ERRORE
028000931202     C                   MOVEL     MSG(9)        $MSG
028100931202     C                   EXSR      CARMSG
028200931202     C                   SETON                                        62
028300931202E   3C                   ENDIF
028400931202     C*
028500931202X   2C                   ELSE                                                   *NON SELEZIOANTO
028600931202     C                   MOVEL     *ZEROS        V1CMAG
028700931202     C                   MOVEL     *BLANKS       V1DMAG
028800931202E   2C                   ENDIF
028900931202E   1C                   ENDIF
029000930923     C*
029100931202     C* DATA SCADENZA MANUTENZIONE DAL
029200971013     C                   Z-ADD     *ZEROS        DTMV1C                         *NON SELEZIONATA
029300931202IF  1C     $ERR          IFEQ      '0'
029400931213IF  2C     V1CDTM        IFNE      *ZEROS                                       *SELEZIONATA
029500971013     C                   Z-ADD     V1CDTM        G08DAT
029600971013     C                   Z-ADD     *ZEROS        G08INV
029700971013     C                   MOVEL     '0'           G08ERR
029800971013     C                   CALL      'XSRDA8'
029900971013     C                   PARM                    WLBDA8
030000971013     C                   Z-ADD     G08DAT        V1CDTM
030100971013     C                   Z-ADD     G08INV        DTMV1C            8 0
030200971013IF  3C     G08ERR        IFEQ      '1'                                          *ERRORE
030300931202     C                   MOVEL     '1'           $ERR
030400931202     C                   MOVEL     MSG(10)       $MSG
030500931202     C                   EXSR      CARMSG
030600931202     C                   SETON                                        63
030700931202E   3C                   ENDIF
030800931202E   2C                   ENDIF
030900931202E   1C                   ENDIF
031000931202     C*
031100931202     C* DATA SCADENZA MANUTENZIONE AL
031200971013     C                   Z-ADD     *ZEROS        DT2V1C                         *NON SELEZIONATA
031300931202IF  1C     $ERR          IFEQ      '0'
031400931213IF  2C     V1CDT2        IFNE      *ZEROS                                       *SELEZIONATA
031500971013     C                   Z-ADD     V1CDT2        G08DAT
031600971013     C                   Z-ADD     *ZEROS        G08INV
031700971013     C                   MOVEL     '0'           G08ERR
031800971013     C                   CALL      'XSRDA8'
031900971013     C                   PARM                    WLBDA8
032000971013     C                   Z-ADD     G08DAT        V1CDT2
032100971013     C                   Z-ADD     G08INV        DT2V1C            8 0
032200971013IF  3C     G08ERR        IFEQ      '1'                                          *ERRORE
032300931202     C                   MOVEL     '1'           $ERR
032400931202     C                   MOVEL     MSG(10)       $MSG
032500931202     C                   EXSR      CARMSG
032600931213     C                   SETON                                        64
032700931202E   3C                   ENDIF
032800931202E   2C                   ENDIF
032900931202E   1C                   ENDIF
033000931202     C*
033100931202     C* DATA AL NON IMMESSA: LA IMPOSTO UGUALA A DATA DAL
033200931202IF  1C     $ERR          IFEQ      '0'
033300931213IF  2C     V1CDTM        IFGT      *ZEROS
033400931213     C     V1CDT2        ANDEQ     *ZEROS
033500931213     C                   Z-ADD     V1CDTM        V1CDT2
033600931213     C                   Z-ADD     DTMV1C        DT2V1C
033700931202E   2C                   ENDIF
033800931202E   1C                   ENDIF
033900931202     C*
034000931202     C* DATA DAL/DATA AL
034100931202IF  1C     $ERR          IFEQ      '0'
034200931213IF  2C     V1CDTM        IFEQ      *ZEROS                                       *AL SOLO DE DAL
034300931213IF  3C     V1CDT2        IFNE      *ZEROS
034400931202     C                   MOVEL     '1'           $ERR
034500931202     C                   MOVEL     MSG(17)       $MSG
034600931202     C                   EXSR      CARMSG
034700931213     C                   SETON                                        64
034800931202E   3C                   ENDIF
034900931202     C*
035000931202X   2C                   ELSE                                                   *AL MAGGIORE DAL
035100931213IF  3C     V1CDT2        IFLT      V1CDTM
035200931202     C                   MOVEL     '1'           $ERR
035300931202     C                   MOVEL     MSG(16)       $MSG
035400931202     C                   EXSR      CARMSG
035500931213     C                   SETON                                        64
035600931202E   3C                   ENDIF
035700931202     C*
035800931202E   2C                   ENDIF
035900931202E   1C                   ENDIF
036000020322
036100020322      * DECODIFICA CAUSALE MOVIMENTO
036200020322IF  1C     $ERR          IFEQ      '0'
036300931217IF  1C     V1CCAU        IFGT      *BLANKS
036400020322     C                   MOVEL     *BLANKS       V1DCAU                         *DESCR.CAUSALE
036500020322     C                   MOVE      V1CCAU        WCAU
036600020322     C                   EXSR      CHKCAU                                       *CONTROLLA CAUSALE
036700020322     C     $ERR          IFEQ      '1'                                          --- 1 -->
036800020322     C                   MOVEL     '1'           $ERR
036900020322     C                   MOVEL     MSG(13)       $MSG
037000020322     C                   EXSR      CARMSG
037100020322     C                   SETON                                        66
037200020322     C                   ELSE                                                   <-- 1 -->
037300020322     C                   MOVE      WCAU          V1CCAU                         *COD.
037400020322     C                   MOVEL     WDCAU         V1DCAU                         *DESCR.
037500020322     C                   ENDIF                                                  <-- 1 ---
037600931217E   1C                   ENDIF
037700020322E   1C                   ENDIF
037800020322
037900931008     C                   ENDSR
038000020322      *------------------------------------------------------------------------*
038100020322      *?CHKTIP - CONTROLLA IL TIPO DI UNITA'
038200020322      *           INPUT:  WTIP  - VALORE IMMESSO A VIDEO
038300020322      *           OUTPUT  WDTIP - DECODIFICA TIPO UNITA'
038400020322      *------------------------------------------------------------------------*
038500931201     C     CHKTIP        BEGSR
038600020322
038700020322      * CONTROLLO SE E' STATO INSERITO '?'
038800020322     C     '?'           SCAN      WTIP                                   99
038900020322
039000020322      * E' STATO INSERITO: LANCIO IL PGM DI INTERROGAZIONE TABELLE
039100020322     c                   if        *in99                                        --- 1 -->
039200020322     c                   clear                   TIBS02DS
039300020322     c                   movel     'R'           t02mod
039400020322     c                   movel     knsif         t02sif
039500020322     c                   movel     'UNT'         t02cod
039600020322     c                   call      'TIBS02R'
039700020322     c                   parm                    KPJBA
039800020322     c                   parm                    TIBS02DS
039900020322     c                   movel     T02ke1        wtip
040000020322     c                   movel     t02uni        wdtip
040100931201     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA
040200020322     c                   endif                                                  <-- 1 ---
040300020322
040400020322      * NON E' STATO INSERITO: CONTROLLO CHE ESISTA IN TABELLA
040500020322     c                   clear                   TIBS02DS
040600020322     c                   movel     'C'           T02mod
040700020322     c                   movel     knsif         t02sif
040800020322     c                   movel     'UNT'         t02cod
040900020322     c                   movel(p)  wtip          T02ke1
041000020322     c                   call      'TIBS02R'
041100020322     c                   parm                    KPJBA
041200020322     c                   parm                    TIBS02DS
041300020322     c                   if        t02err <> *blanks
041400020322     c                   eval      $err = '1'
041500020322     c                   clear                   wdtip
041600020322     c                   else
041700020322     c                   movel     t02uni        wdtip
041800020322     c                   endif
041900020322
042000931201     C                   ENDSR
042100931201     C*------------------------------------------------------------------------*
042200931201     C*?CHKMAG - CONTROLLA IL MAGAZZINO UBICAZIONE
042300931202     C*           INPUT:  WMAG  - VALORE IMMESSO A VIDEO
042400931202     C*           OUTPUT  WDMAG - DECODIFICA MAGAZZINO UBIACAZIONE
042500931201     C*------------------------------------------------------------------------*
042600931201     C     CHKMAG        BEGSR
042700931201     C*
042800931201     C* CONTROLLO SE E' STATO INSERITO '?'
042900931201     C     '?'           SCAN      WMAG          A                 1 0    99
043000931201     C*
043100931202     C* E' STATO INSERITO; LANCIO IL PGM DI INTERROGAZIONE LINEE ARRIVO
043200931202IF  1C     *IN99         IFEQ      '1'
043300931202     C                   MOVEL     *BLANKS       §COD1
043400931201     C                   MOVE      *BLANKS       §DES1
043500941115     C                   CALL      'TNSD20R'
043600931202     C                   PARM                    §COD1             3
043700931201     C                   PARM                    §TIP              1
043800931201     C                   PARM                    §DES1            25
043900931202     C*
044000931202     C                   MOVEL     §COD1         WMAG
044100931202     C                   MOVEL     §DES1         WDMAG
044200931202     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA
044300931202     C*
044400931202     C* NON E' STATO INSERITO: CONTROLLO CHE ESISTA IN AZORG
044500931202X   1C                   ELSE
044600931201     C                   MOVE      WMAG          ORGFIL
044700020529     C     KEYORG        CHAIN     AZORG21l                           99
044800931202IF  2C     *IN99         IFEQ      '0'
044900931202     C     ORGFVA        ANDEQ     *BLANKS
045000931201     C                   MOVEL     ORGFIL        WMAG
045100931201     C                   MOVEL     ORGDES        WDMAG
045200931202X   2C                   ELSE
045300020529     c     keyorg        chain     azorg01l
045400020529     c                   if        %found(azorg01l) and orgfva = *blanks
045500020529     c                   movel     orgfil        wmag
045600020529     c                   movel     orgdes        wdmag
045700020529     c                   else
045800931201     C                   MOVEL     '1'           $ERR
045900931201     C                   MOVEL     *ALL'*'       WDMAG
046000020529     c                   endif
046100931202E   2C                   ENDIF
046200931202E   1C                   ENDIF
046300931201     C*
046400931201     C                   ENDSR
046500020322      *------------------------------------------------------------------------*
046600020322      *?CHKCAU - CONTROLLA LA CAUSALE MOVIMENTO
046700020322      *           INPUT:  WCAU  - VALORE IMMESSO A VIDEO
046800020322      *           OUTPUT  WDCAU - DECODIFICA CAUSALE MOVIMENTO
046900020322      *------------------------------------------------------------------------*
047000020322     C     CHKCAU        BEGSR
047100020322
047200020322      * CONTROLLO SE E' STATO INSERITO '?'
047300020322     C     '?'           SCAN      WCAU                                   99
047400020322
047500020322      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
047600020322     c                   if        *in99                                        --- 1 -->
047700020322     c                   clear                   TIBS02DS
047800020322     c                   movel     'R'           t02mod
047900020322     c                   movel     knsif         t02sif
048000020322     c                   movel     'UNM'         t02cod
048100020322     c                   call      'TIBS02R'
048200020322     c                   parm                    KPJBA
048300020322     c                   parm                    TIBS02DS
048400020322     c                   movel     T02ke1        wcau              4
048500020322     c                   movel     t02uni        wdcau            30
048600020322     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
048700020322     c                   endif                                                  <-- 1 ---
048800020322
048900020322      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
049000020322     c                   clear                   TIBS02DS
049100020322     c                   movel     'C'           T02mod
049200020322     c                   movel     knsif         t02sif
049300020322     c                   movel     'UNM'         t02cod
049400020322     c                   movel(p)  wcau          T02ke1
049500020322     c                   call      'TIBS02R'
049600020322     c                   parm                    KPJBA
049700020322     c                   parm                    TIBS02DS
049800020322     c                   if        t02err <> *blanks
049900020322     c                   eval      $err = '1'
050000020322     c                   clear                   wdcau
050100020322     c                   else
050200020322     c                   movel     t02uni        wdcau
050300020322     c                   endif
050400020322
050500020322     C                   ENDSR
050600931201     C*------------------------------------------------------------------------*
050700931201     C*?*INZSR - ROUTINE INIZIALE
050800931201     C*------------------------------------------------------------------------*
050900930713     C     *INZSR        BEGSR
051000000000     C*
051100000000     C* RICEVIMENTO PARAMETRI
051200000000     C     *ENTRY        PLIST
051300930922     C                   PARM                    KPJBA
051400940105     C                   MOVEL     KPJBU         DSUNIT
051500930310     C*
051600020529     C* CHIAVE LETTURA AZORG01L/azorg21l - COMPLETA
051700930310     C     KEYORG        KLIST
051800930712     C                   KFLD                    ORGFIL
051900930714     C*
052000930714     C* ESEGUE OPERAZIONI INIZIALI STANDARD
052100930921     C                   EXSR      INZSTD
052200931202     C*
052300931008     C* IMPOSTO CAMPI VIDEO RICHIESTA PARAMETRI
052400931202     C                   Z-ADD     1             V1CORD                         *ORDINAMENTO
052500930922     C*
052600930924     C* EMETTO FORMATO VIDEO
052700930924     C                   EXSR      CARV1                                        *EMISSIONE VIDEO
052800930923     C*
052900000000     C                   ENDSR
053000931201     C*------------------------------------------------------------------------*
053100931202     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
053200931202     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
053300931202     C*     LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
053400931202     C*     STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
053500931202     C*     NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
053600931202     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
053700931202     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
053800931201     C*------------------------------------------------------------------------*
053900930923     C* INZSTD - ROUTINE INIZIALE STANDARD
054000931201     C*------------------------------------------------------------------------*
054100930803     C     INZSTD        BEGSR
054200930714     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
054300930802     C                   CALL      'XKEYS'
054400930802     C                   PARM                    DSKEYS
054500930805     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
054600930714     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
054700930714     C     XMSGPA        PLIST
054800931213     C                   PARM                    DSMSMS
054900931213     C                   PARM                    DSMSPG
055000930714     C                   PARM                    DSMSKE
055100930714     C                   PARM                    DSMSER
055200930714     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
055300941115     C                   MOVEL     'TNSDU8R'     DSMSPG                         *NOME PGM CHIAMANTE
055400941115     C                   MOVEL     'TNSDU8R'     VMCQUE                         *CODA MESSAGGI DESTI
055500930806     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
055600930714     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
055700930805     C* INIZIALIZZO FLAG CONTROLLO PGM
055800930721     C                   MOVEL     '0'           $ERR              1            *ERRORE
055900930721     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
056000930806     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
056100930721     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
056200930721     C                   MOVEL     '0'           $FINE             1            *FINE PGM
056300930922     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
056400930923     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
056500930714     C* LANCIO ROUTINE PER VERIFICARE SE
056600930714     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
056700930714     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
056800020709
056900020709     c     *dtaara       define    §azute        azuteds
057000020709     c     *dtaara       define    §datiute      ddatiute
057100020709
057200020709     c                   in(E)     *dtaara
057300020709     c                   if        %Error or rsut = *blanks
057400020709     c                   clear                   tibs34ds
057500020709     c                   call      'TIBS34R'
057600020709     c                   parm                    Tibs34Ds
057700020709     c                   in        *dtaara
057800020709     c                   endif
057900020717
058000020717      * Controllo il profilo utente
058100020717     c                   ExSr      CtrUte
058200020717
058300930805     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
058400930721     C                   BITOFF    '01234567'    POSMI1            1
058500930721     C                   BITOFF    '01234567'    POSMI2            1
058600930721     C                   BITOFF    '01234567'    POSMA1            1
058700930721     C                   BITOFF    '01234567'    POSMA2            1
058800930721     C                   BITON     '34'          POSMI1            1            *18
058900930721     C                   BITON     '7'           POSMI2            1            *01
059000930721     C                   BITON     '34'          POSMA1            1            *18
059100930721     C                   BITON     '0'           POSMA2            1            *80
059200930721     C                   MOVE      POSMI2        POSMIN            2
059300930721     C                   MOVEL     POSMI1        POSMIN
059400930721     C                   MOVE      POSMA2        POSMAX            2
059500930721     C                   MOVEL     POSMA1        POSMAX
059600930714     C                   ENDSR
059700020717      *------------------------------------------------------------------------*
059800020717      *  CTRUTE - ROUTINE CONTROLLO UTENTE
059900020717      *------------------------------------------------------------------------*
060000020717     c     CtrUte        BegSr
060100020717
060200020717     c                   Eval      *In76 = *Off
060300020717
060400020717      * Controllo se il profilo è ok da tibs34
060500020717     c                   If        DutErr = 'E'
060600020717     c                   eval      $Msg = DutMsg
060700020717     c                   ExSr      CarMsg
060800020717     c                   eval      *In76 = *On
060900020717     c                   endIf
061000020717
061100020717      * Controllo se il profilo di sede
061200020717     c                   If        DutLpo <> 'S'
061300020717     c                   Eval      $Msg = Msg(14)
061400020717     c                   ExSr      CarMsg
061500020717     c                   Eval      *In76 = *On
061600020717     c                   EndIf
061700020717
061800020717     c                   endSr
061900931201     C*------------------------------------------------------------------------*
062000930923     C* FUNSTD - LANCIA SUBROUTINE STANDARD
062100931201     C*------------------------------------------------------------------------*
062200930804     C     FUNSTD        BEGSR
062300930804     C                   SELECT
062400930804     C     KEY           WHENEQ    F3
062500930804     C                   EXSR      F03STD
062600931115     C     KEY           WHENEQ    F12
062700931115     C                   EXSR      F12STD
062800930804     C     KEY           WHENEQ    F24
062900930804     C                   EXSR      F24STD
063000930804     C                   OTHER
063100930804     C                   MOVEL     MSG(2)        $MSG
063200930804     C                   EXSR      CARMSG
063300930804     C                   ENDSL
063400930804     C                   ENDSR
063500931201     C*------------------------------------------------------------------------*
063600930923     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
063700931201     C*------------------------------------------------------------------------*
063800930804     C     F03STD        BEGSR
063900930923     C*
064000940105     C* IMPOSTO FLAG DI PREMUTO TASTO F03
064100940105     C                   MOVEL     '1'           $F03
064200940105     C*
064300930804     C* IMPOSTO FLAG DI FINE PROGRAMMA
064400930927     C                   MOVEL     '1'           $FINE
064500930927     C*
064600930927     C                   ENDSR
064700931201     C*------------------------------------------------------------------------*
064800931115     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
064900931201     C*------------------------------------------------------------------------*
065000931115     C     F12STD        BEGSR
065100931115     C*
065200931115     C* IMPOSTO FLAG DI FINE PROGRAMMA
065300931115     C                   MOVEL     '1'           $FINE
065400931115     C*
065500931115     C                   ENDSR
065600931201     C*------------------------------------------------------------------------*
065700930923     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
065800931201     C*------------------------------------------------------------------------*
065900930804     C     F24STD        BEGSR
066000930923     C*
066100930804     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
066200930804     C                   EXSR      OUTFUN
066300930923     C*
066400930804     C                   ENDSR
066500931201     C*------------------------------------------------------------------------*
066600930923     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
066700930721     C*          CODA MESSAGGI DEL PGM
066800931201     C*------------------------------------------------------------------------*
066900930721     C     CARMSG        BEGSR
067000930723     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
067100930723     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
067200930721     C     MSGULT        IFEQ      0
067300930721     C                   MOVEL     '1'           *IN40
067400941115     C                   WRITE     SDU8DVK
067500930721     C                   MOVEL     '0'           *IN40
067600930805     C                   ENDIF
067700930721     C* INVIA MESSAGGIO ALLA CODA DEL PGM
067800930721     C                   MOVEL     $MSG          DSMSMS
067900930721     C                   CALL      'XMSG'        XMSGPA
068000930721     C     DSMSER        IFEQ      0
068100930806     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
068200930805     C                   ENDIF
068300930723     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
068400930923     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
068500941115     C                   WRITE     SDU8DVM                                      *SFL MESS
068600930723     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
068700930721     C                   MOVEL     *BLANKS       $MSG
068800930721     C                   ENDSR
068900931201     C*------------------------------------------------------------------------*
069000930923     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
069100931201     C*------------------------------------------------------------------------*
069200930721     C     OUTMSG        BEGSR
069300930721     C* VISUALIZZAZIONE MESSAGGIO
069400930721     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
069500941115     C                   WRITE     SDU8DVK                                      *CONTR MESS
069600930721     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
069700930721     C                   Z-ADD     *ZEROS        MSGULT
069800930721     C                   ENDSR
069900931201     C*------------------------------------------------------------------------*
070000930923     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
070100930803     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
070200930803     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
070300930803     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
070400930803     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
070500931201     C*------------------------------------------------------------------------*
070600930803     C     CARFUN        BEGSR
070700930803     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
070800930923     C     1             DO        31            I                 5 0          --- 1 -->
070900930803     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
071000930805     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
071100930805     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
071200020709     c                   If        DutLpo = 'S'
071300930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
071400930805     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
071500930803     C                   ELSE                                                   *INTERROG.
071600930803     C                   MOVEL     FUNASI        FAT(I)
071700930803     C                   ENDIF
071800930803     C                   ELSE                                                   <-- 2 --> *FILIALE
071900930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
072000930803     C                   MOVEL     FUNAFG        FAT(I)
072100930803     C                   ELSE                                                   *INTERROG.
072200930922     C                   MOVEL     FUNAFI        FAT(I)
072300930803     C                   ENDIF
072400930803     C                   ENDIF                                                  <-- 2 ---
072500930806     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
072600930806     C                   MOVEL     USER          WEDP              3            *PROFILO
072700930806     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
072800930806     C                   MOVEL     'S'           FAT(I)                         *ABILITO
072900930806     C                   ELSE
073000930806     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
073100930806     C                   ENDIF
073200930806     C                   ENDIF
073300930803     C                   ENDDO                                                  <-- 1 ---
073400930803     C                   ENDSR
073500931201     C*------------------------------------------------------------------------*
073600930923     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
073700930803     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
073800930806     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
073900930803     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
074000931201     C*------------------------------------------------------------------------*
074100930803     C     OUTFUN        BEGSR
074200930805     C     1             DO        8             I                              *PULIZIA FUNZIONI A
074300930805     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
074400930803     C                   ENDDO
074500930803     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
074600930803     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
074700930803     C     1             DO        31            I
074800930803     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
074900930803     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
075000930803     C                   ADD       1             FUNNAT
075100930803     C                   ENDIF
075200930803     C                   ENDDO
075300930805     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
075400930803     C     FUNNAT        IFGT      8
075500930803     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
075600930803     C                   ENDIF
075700930803     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
075800930805     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
075900930803     C                   Z-ADD     8             FUNVIS            5 0
076000930803     C     FAT(23)       IFEQ      'S'
076100930803     C                   SUB       1             FUNVIS
076200930803     C                   ENDIF
076300930803     C     FAT(24)       IFEQ      'S'
076400930803     C                   SUB       1             FUNVIS
076500930803     C                   ENDIF
076600930805     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
076700930923     C                   Z-ADD     *ZEROS        W                 5 0
076800930803     C                   ADD       1             FUNULT
076900930803     C     FUNULT        IFGT      31
077000930803     C                   Z-ADD     1             FUNULT
077100930805     C                   ENDIF
077200930805     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
077300930803     C     FUNULT        DO        31            I                              --- 1 -->
077400930803     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
077500930803     C     W             IFLT      FUNVIS                                       --- 2 -->
077600930803     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
077700930803     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
077800930803     C     FDE(I)        ANDNE     *BLANKS
077900930805     C     I             IFEQ      23                                           *TOLGO F23
078000930805     C     I             OREQ      24                                           *E F24
078100930803     C                   ELSE
078200930803     C                   ADD       1             W
078300930803     C                   MOVEL     FDE(I)        FUV(W)
078400930803     C                   ENDIF
078500930803     C                   ENDIF                                                  <-- 3 ---
078600930803     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
078700930803     C                   Z-ADD     I             FUNULT
078800930803     C                   ENDIF                                                  <-- 2 ---
078900930803     C                   ENDDO                                                  <-- 1 ---
079000930803     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
079100930806     C                   MOVEL     FUV(1)        VFD001
079200930806     C                   MOVEL     FUV(2)        VFD002
079300930806     C                   MOVEL     FUV(3)        VFD003
079400930806     C                   MOVEL     FUV(4)        VFD004
079500930806     C                   MOVEL     FUV(5)        VFD005
079600930806     C                   MOVEL     FUV(6)        VFD006
079700930803     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
079800930803     C     FAT(24)       ANDEQ     'N'
079900930806     C                   MOVEL     FUV(7)        VFD007
080000930806     C                   MOVEL     FUV(8)        VFD008
080100930803     C                   ELSE                                                   <-- 1 -->
080200930803     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
080300930803     C     FAT(24)       ANDEQ     'S'
080400930806     C                   MOVEL     FDE(23)       VFD007
080500930806     C                   MOVEL     FDE(24)       VFD008
080600930803     C                   ELSE                                                   <-- 2 -->
080700930806     C                   MOVEL     FUV(7)        VFD007
080800930803     C     FAT(23)       IFEQ      'S'
080900930806     C                   MOVEL     FDE(23)       VFD008
081000930803     C                   ELSE
081100930806     C                   MOVEL     FDE(24)       VFD008
081200930805     C                   ENDIF
081300930805     C                   ENDIF                                                  <-- 2 ---
081400930805     C                   ENDIF                                                  <-- 1 ---
081500930922     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
081600930803     C                   ENDSR
081700931201     C*------------------------------------------------------------------------*
081800930923     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
081900930803     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
082000930803     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
082100930803     C*                   - ERRORE SE FUNZIONE NON ATTIVA
082200931201     C*------------------------------------------------------------------------*
082300930803     C     CONFUN        BEGSR
082400930803     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
082500930803     C     1             DO        31            I                              --- 1 -->
082600930803     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
082700930805     C* CONTROLLO SE IL TASTO E' ATTIVO
082800930803     C     FAT(I)        IFNE      'S'                                          --- 3 -->
082900930804     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
083000930803     C                   EXSR      CARMSG
083100930803     C                   MOVEL     '1'           $ERR
083200930803     C                   ENDIF                                                  <-- 3 ---
083300930803     C                   ENDIF                                                  <-- 2 ---
083400930803     C                   ENDDO                                                  <-- 1 ---
083500930803     C                   ENDSR
083600931201     O*------------------------------------------------------------------------*
083700930802**       MSG - MESSAGGI
083800941115TNSDU8R- Tasto funzionale non valido.                                          1
083900941115TNSDU8R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
084000941115TNSDU8R- Opzione non valida.                                                   3
084100941115TNSDU8R- Opzione abilitata ma non gestita. Avvertire CED.                      4
084200941115TNSDU8R- Inizio elenco.                                                        5
084300941115TNSDU8R- Fine elenco.                                                          6
084400941115TNSDU8R- Scegliere uno degli ordinamenti possibili                             7
084500941115TNSDU8R- Tipo unità errato                                                     8
084600941115TNSDU8R- Magazzino errato                                                      9
084700941115TNSDU8R- Data movimento errata                                                10
084800941115TNSDU8R- Inserire anche il tipo unità se selezionato il codice                11
084900941115TNSDU8R- Stampa lanciata ...                                                  12
085000020322TNSDU8R- Causale inesistente                                                  13
085100020717TNSDU8R- Profilo non abilitato alla funzione                                  14
085200941115TNSDU8R-                                                                      15
085300941115TNSDU8R- Data AL inferiore a data DAL                                         16
085400941115TNSDU8R- Immettere la data AL solo se immessa la data DAL                     17
085500941115TNSDU8R-                                                                      18
085600941115TNSDU8R-                                                                      19
085700941115TNSDU8R-                                                                      20
085800940105** ======== SCHIERA: CMD  (COMANDI)                        ================
085900940105WRKSPLF
086000940105
086100940105
086200940105
086300940105
086400930806** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
086500930806F1    -                   -    NN     NN
086600930723F2    -                   -    NN     NN
086700930921F3    -F3=Fine            -    SS     SS
086800930723F4    -                   -    NN     NN
086900930723F5    -                   -    NN     NN
087000931026F6    -F6=Lancio          -    SS     SS
087100930723F7    -                   -    NN     NN
087200930723F8    -                   -    NN     NN
087300930723F9    -                   -    NN     NN
087400930723F10   -                   -    NN     NN
087500930723F11   -                   -    NN     NN
087600931115F12   -F12-Ritorno        -    SS     SS
087700930723F13   -                   -    NN     NN
087800930723F14   -                   -    NN     NN
087900930723F15   -                   -    NN     NN
088000930723F16   -                   -    NN     NN
088100930723F17   -                   -    NN     NN
088200930723F18   -                   -    NN     NN
088300930723F19   -                   -    NN     NN
088400930723F20   -                   -    NN     NN
088500930804F21   -                   -    NN     NN
088600941215F22   -F22=Gestione stampe-    SS     SS
088700930723F23   -                   -    NN     NN
088800930803F24   -F24=Altri tasti    -    NN     NN
088900930921ENTER -                   -    SS     SS
089000930802ROLLUP-                   -    NN     NN
089100930802ROLLDN-                   -    NN     NN
089200930802HELP  -                   -    NN     NN
089300930802PRINT -                   -    NN     NN
089400930802BSPACE-                   -    NN     NN
089500930802CLEAR -                   -    NN     NN
