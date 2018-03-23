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
003700020322      * PARAMETRI CHIAMATA ROUTINE XFIL (RITORNO FILIALE IN CUI SONO)
003800020709     D**!!! DSFIL         E DS
003900020322      * DS CONTROLLA DATA (8)
004000971013     D WLBDA8          DS
004100971013     D  G08DAT                 1      8  0
004200971013     D  G08INV                 9     16  0
004300971013     D  G08ERR                17     17
004400971013     D  G08TGI                18     22  0
004500020322      * DS PER GESTIONE TASTI FUNZIONALI
004600930802     D DSKEYS        E DS
004700930803     D  KEYRIG                 1     40
004800020322      * RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
004900930806     D                SDS
005000930806     D  USER                 254    263
005100020322      * RIDEFINIZIONE INFDS DEL FILE VIDEO
005200000000     D DEVDS1          DS
005300000000     D  KEY                  369    369
005400930721     D  CURSOR               370    371
005500020322      * ARCHITETTURA
005600930922     D KPJBA         E DS
005700020322      * PARAMETRI DI LANCIO
005800931008     D PARAM           DS                  INZ
005900971013     D  PARORD                 1      1  0
006000971013     D  PARTIP                 2      3  0
006100971013     D  PARCOD                 4      8  0
006200971013     D  PARMAG                 9     11  0
006300971013     D  PARDTM                12     19  0
006400971013     D  PARDT2                20     27  0
006500971013     D  PARCAU                28     31
006600971013     D  DECTIP                32     61
006700971013     D  DECMAG                62     91
006800971013     D  DECDTM                82     99  0
006900971013     D  DECDT2               100    107  0
007000971013     D  DECCAU               108    127
007100020322      * PARAMETRI PER LANCIO ALTRI PGM DELLA PROCEDURA
007200931201     D DSUNIT        E DS
007300020322
007400020322     d tibs02ds      e ds
007500020709
007600020709     d ddatiute      e ds
007700020709     d azuteds       e ds                  extname(azute00f)
007800020709     d tibs34ds      e ds                  inz
007900020322
008000931201     C*------------------------------------------------------------------------*
008100971013     C* MAIN LINE
008200931201     C*------------------------------------------------------------------------*
008300931008     C*
008400000000     C* CICLO FINO A CHE:
008500930922DO  1C     $FINE         DOUEQ     '1'
008600000000     C*
008700931008     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
008800000000     C*
008900930721     C* CONTROLLA GESTIONE VIDEO
009000930922IF  2C     $GES          IFEQ      'V1'
009100930806     C                   EXSR      GESV1                                        *PARAMETRI
009200930922E   2C                   ENDIF
009300000000     C*
009400930922E   1C                   ENDDO
009500940105     C*
009600940105     C* SE E' STATO PREMUTO F03 IMPOSTO FLAG DELLA DS DI PROCEDURA
009700940105IF  1C     $F03          IFEQ      '1'
009800940105     C                   MOVEL     '1'           DSUF03
009900940105E   1C                   ENDIF
010000000000     C*
010100931018     C* FINE MAIN DEL PGM
010200940105     C                   MOVEL     DSUNIT        KPJBU
010300000000     C                   SETON                                        LR
010400931201     C*------------------------------------------------------------------------*
010500931201     C*?GESV1 - GESTIONE VIDEO PARAMETRI
010600931201     C*------------------------------------------------------------------------*
010700930922     C     GESV1         BEGSR
010800930922     C*
010900930922IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
011000930922     C                   EXSR      CARV1                                        *CARICA VIDEO
011100930922E   1C                   ENDIF
011200930922     C*
011300931018IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
011400931018     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
011500931018     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
011600931018E   1C                   ENDIF
011700931018     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
011800930922     C*
011900941115     C                   EXFMT     SDU8DV1                                      *PARAMETRI
012000930922     C*
012100930922     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
012200930922     C*
012300930922IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
012400931008SL  2C                   SELECT
012500930922     C     KEY           WHENEQ    ENTER
012600930922     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
012700930922     C     KEY           WHENEQ    F6
012800930922     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
012900940105     C     KEY           WHENEQ    F22
013000940105     C                   EXSR      F22GES                                       *GESTIONE STAMPE
013100930922     C                   OTHER
013200930922     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
013300930922E   2C                   ENDSL
013400930922E   1C                   ENDIF
013500930922     C*
013600930922     C                   ENDSR
013700931201     C*------------------------------------------------------------------------*
013800931201     C*?CARV1 - CARICA VIDEO PARAMETRI
013900931201     C*------------------------------------------------------------------------*
014000930806     C     CARV1         BEGSR
014100930713     C*
014200930806     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
014300930713     C*
014400930804     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
014500930804     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
014600930713     C*
014700941115     C                   WRITE     SDU8DVT                                      *TESTATA
014800941115     C                   WRITE     SDU8DVF                                      *FUNZIONI
014900930713     C*
015000930713     C                   ENDSR
015100931201     C*------------------------------------------------------------------------*
015200931201     C*?ENTV1 - GESTIONE TASTO ENTER VIDEO 1 (PARAMETRI)
015300931201     C*------------------------------------------------------------------------*
015400930806     C     ENTV1         BEGSR
015500930806     C*
015600930806     C* CONTROLLA I CAMPI IMMESSI
015700930923     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
015800930804     C*
015900930804     C                   ENDSR
016000931201     C*------------------------------------------------------------------------*
016100931201     C*?F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
016200931201     C*------------------------------------------------------------------------*
016300930126     C     F06GES        BEGSR
016400930923     C*
016500930924     C* CONTROLLA I CAMPI IMMESSI
016600930924     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
016700930923     C*
016800930923     C* NON CI SONO ERRORI
016900930923IF  1C     $ERR          IFEQ      '0'
017000930923     C*
017100930923     C* LANCIO PROGRAMMA DI STAMPA
017200960201     C                   CLEAR                   PARAM
017300931202     C                   MOVEL     V1CORD        PARORD                         *TIPO ORDINAMENTO
017400931202     C                   MOVEL     V1CTIP        PARTIP                         *TIPO UNITA'
017500931213     C                   Z-ADD     V1CCOD        PARCOD                         *CODICE UNITA'
017600931202     C                   MOVEL     V1CMAG        PARMAG                         *MAGAZZINO
017700931213     C                   Z-ADD     DTMV1C        PARDTM                         *DATA MOVIM DAL A/M/
017800931213     C                   Z-ADD     DT2V1C        PARDT2                         *DATA MOVIM AL  A/M/
017900931213     C                   MOVEL     V1CCAU        PARCAU                         *CAUSALE MOVIMENTO
018000960201     C                   MOVEL     V1DTIP        DECTIP                         *DECODIFICA TIPO
018100960201     C                   MOVEL     V1DMAG        DECMAG                         *DECODIFICA MAGAZZIN
018200960201     C                   Z-ADD     V1CDTM        DECDTM                         *DATA MOVIM DAL G/M/
018300960201     C                   Z-ADD     V1CDT2        DECDT2                         *DATA MOVIM AL  G/M/
018400930923     C*
018500960202     C                   MOVEL     'SDU9'        KCOAZ
018600960202     C                   MOVEL     PARAM         KPJBU
018700960202     C                   CALL      'BCH10'
018800930923     C                   PARM                    KPJBA
018900930923     C*
019000940105     C                   MOVEL     MSG(12)       $MSG                           *STAMPA LANCIATA
019100940105     C                   EXSR      CARMSG
019200931215     C*
019300930923E   1C                   ENDIF
019400930127     C*
019500000000     C                   ENDSR
019600940105     C*------------------------------------------------------------------------*
019700940105     C*?F22GES -GEST. TASTO FUNZ. F22 ==> STAMPE
019800940105     C*------------------------------------------------------------------------*
019900940105     C     F22GES        BEGSR
020000940105     C*
020100940105     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
020200940105     C                   Z-ADD     80            LENGH            15 5
020300940105     C                   CALL      'QCAEXEC'
020400940105     C                   PARM                    CMD(1)
020500940105     C                   PARM                    LENGH
020600940105     C*
020700940105     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
020800940105     C*
020900940105     C                   ENDSR
021000931201     C*------------------------------------------------------------------------*
021100931201     C*?DECODI - CONTROLLA CAMPI E DECODIFICA
021200931201     C*------------------------------------------------------------------------*
021300930906     C     DECODI        BEGSR
021400930205     C*
021500931201     C* REIMPOSTO INDICATORI E VARIABILI DI CONTROLLO
021600931124     C                   SETOFF                                       606162
021700931201     C                   SETOFF                                       636465
021800931213     C                   SETOFF                                       66
021900930712     C*
022000931201     C* TIPO ORDINAMENTO
022100931202IF  1C     V1CORD        IFEQ      1
022200931202     C     V1CORD        OREQ      2
022300931202X   1C                   ELSE
022400930922     C                   MOVEL     '1'           $ERR
022500930922     C                   MOVEL     MSG(7)        $MSG
022600930922     C                   EXSR      CARMSG
022700931201     C                   SETON                                        60
022800931201E   1C                   ENDIF
022900931201     C*
023000931201     C* TIPO UNITA'
023100931201IF  1C     $ERR          IFEQ      '0'
023200931202IF  2C     V1CTIP        IFNE      *BLANKS                                      *SELEZIONATO
023300931201     C     V1CTIP        ANDNE     *ZEROS
023400931201     C                   MOVEL     V1CTIP        WTIP              2
023500931201     C                   MOVEL     *BLANKS       WDTIP            30
023600931201     C                   EXSR      CHKTIP
023700931202     C                   MOVEL     WTIP          V1CTIP
023800931202     C                   MOVEL     WDTIP         V1DTIP
023900931202     C*
024000931202IF  3C     $ERR          IFEQ      '1'                                          *ERRORE
024100931202     C                   MOVEL     MSG(8)        $MSG
024200931202     C                   EXSR      CARMSG
024300931202     C                   SETON                                        61
024400931202E   3C                   ENDIF
024500931202     C*
024600931202X   2C                   ELSE                                                   *NON SELEZIONATO
024700931201     C                   MOVEL     *ZEROS        V1CTIP
024800931201     C                   MOVEL     *BLANKS       V1DTIP
024900931201E   2C                   ENDIF
025000931201E   1C                   ENDIF
025100931213     C*
025200931213     C* CODICE UNITA'
025300931213IF  1C     $ERR          IFEQ      '0'
025400931213IF  2C     V1CCOD        IFGT      *ZEROS                                       *SELEZIONATO
025500931213IF  3C     V1CTIP        IFEQ      *BLANKS
025600931213     C     V1CTIP        OREQ      *ZEROS
025700931213     C                   MOVEL     '1'           $ERR
025800931213     C                   MOVEL     MSG(11)       $MSG
025900931213     C                   EXSR      CARMSG
026000931213     C                   SETON                                        61
026100931213E   3C                   ENDIF
026200931213E   2C                   ENDIF
026300931213E   1C                   ENDIF
026400931202     C*
026500931202     C* MAGAZZINO UBICAZIONE
026600931202IF  1C     $ERR          IFEQ      '0'
026700931202IF  2C     V1CMAG        IFNE      *BLANKS                                      *SELEZIONATO
026800931202     C     V1CMAG        ANDNE     *ZEROS
026900931202     C                   MOVEL     V1CMAG        WMAG              3
027000931202     C                   MOVEL     *BLANKS       WDMAG            30
027100931202     C                   EXSR      CHKMAG
027200931202     C                   MOVEL     WMAG          V1CMAG
027300931202     C                   MOVEL     WDMAG         V1DMAG
027400931202     C*
027500931202IF  3C     $ERR          IFEQ      '1'                                          *ERRORE
027600931202     C                   MOVEL     MSG(9)        $MSG
027700931202     C                   EXSR      CARMSG
027800931202     C                   SETON                                        62
027900931202E   3C                   ENDIF
028000931202     C*
028100931202X   2C                   ELSE                                                   *NON SELEZIOANTO
028200931202     C                   MOVEL     *ZEROS        V1CMAG
028300931202     C                   MOVEL     *BLANKS       V1DMAG
028400931202E   2C                   ENDIF
028500931202E   1C                   ENDIF
028600930923     C*
028700931202     C* DATA SCADENZA MANUTENZIONE DAL
028800971013     C                   Z-ADD     *ZEROS        DTMV1C                         *NON SELEZIONATA
028900931202IF  1C     $ERR          IFEQ      '0'
029000931213IF  2C     V1CDTM        IFNE      *ZEROS                                       *SELEZIONATA
029100971013     C                   Z-ADD     V1CDTM        G08DAT
029200971013     C                   Z-ADD     *ZEROS        G08INV
029300971013     C                   MOVEL     '0'           G08ERR
029400971013     C                   CALL      'XSRDA8'
029500971013     C                   PARM                    WLBDA8
029600971013     C                   Z-ADD     G08DAT        V1CDTM
029700971013     C                   Z-ADD     G08INV        DTMV1C            8 0
029800971013IF  3C     G08ERR        IFEQ      '1'                                          *ERRORE
029900931202     C                   MOVEL     '1'           $ERR
030000931202     C                   MOVEL     MSG(10)       $MSG
030100931202     C                   EXSR      CARMSG
030200931202     C                   SETON                                        63
030300931202E   3C                   ENDIF
030400931202E   2C                   ENDIF
030500931202E   1C                   ENDIF
030600931202     C*
030700931202     C* DATA SCADENZA MANUTENZIONE AL
030800971013     C                   Z-ADD     *ZEROS        DT2V1C                         *NON SELEZIONATA
030900931202IF  1C     $ERR          IFEQ      '0'
031000931213IF  2C     V1CDT2        IFNE      *ZEROS                                       *SELEZIONATA
031100971013     C                   Z-ADD     V1CDT2        G08DAT
031200971013     C                   Z-ADD     *ZEROS        G08INV
031300971013     C                   MOVEL     '0'           G08ERR
031400971013     C                   CALL      'XSRDA8'
031500971013     C                   PARM                    WLBDA8
031600971013     C                   Z-ADD     G08DAT        V1CDT2
031700971013     C                   Z-ADD     G08INV        DT2V1C            8 0
031800971013IF  3C     G08ERR        IFEQ      '1'                                          *ERRORE
031900931202     C                   MOVEL     '1'           $ERR
032000931202     C                   MOVEL     MSG(10)       $MSG
032100931202     C                   EXSR      CARMSG
032200931213     C                   SETON                                        64
032300931202E   3C                   ENDIF
032400931202E   2C                   ENDIF
032500931202E   1C                   ENDIF
032600931202     C*
032700931202     C* DATA AL NON IMMESSA: LA IMPOSTO UGUALA A DATA DAL
032800931202IF  1C     $ERR          IFEQ      '0'
032900931213IF  2C     V1CDTM        IFGT      *ZEROS
033000931213     C     V1CDT2        ANDEQ     *ZEROS
033100931213     C                   Z-ADD     V1CDTM        V1CDT2
033200931213     C                   Z-ADD     DTMV1C        DT2V1C
033300931202E   2C                   ENDIF
033400931202E   1C                   ENDIF
033500931202     C*
033600931202     C* DATA DAL/DATA AL
033700931202IF  1C     $ERR          IFEQ      '0'
033800931213IF  2C     V1CDTM        IFEQ      *ZEROS                                       *AL SOLO DE DAL
033900931213IF  3C     V1CDT2        IFNE      *ZEROS
034000931202     C                   MOVEL     '1'           $ERR
034100931202     C                   MOVEL     MSG(17)       $MSG
034200931202     C                   EXSR      CARMSG
034300931213     C                   SETON                                        64
034400931202E   3C                   ENDIF
034500931202     C*
034600931202X   2C                   ELSE                                                   *AL MAGGIORE DAL
034700931213IF  3C     V1CDT2        IFLT      V1CDTM
034800931202     C                   MOVEL     '1'           $ERR
034900931202     C                   MOVEL     MSG(16)       $MSG
035000931202     C                   EXSR      CARMSG
035100931213     C                   SETON                                        64
035200931202E   3C                   ENDIF
035300931202     C*
035400931202E   2C                   ENDIF
035500931202E   1C                   ENDIF
035600020322
035700020322      * DECODIFICA CAUSALE MOVIMENTO
035800020322IF  1C     $ERR          IFEQ      '0'
035900931217IF  1C     V1CCAU        IFGT      *BLANKS
036000020322     C                   MOVEL     *BLANKS       V1DCAU                         *DESCR.CAUSALE
036100020322     C                   MOVE      V1CCAU        WCAU
036200020322     C                   EXSR      CHKCAU                                       *CONTROLLA CAUSALE
036300020322     C     $ERR          IFEQ      '1'                                          --- 1 -->
036400020322     C                   MOVEL     '1'           $ERR
036500020322     C                   MOVEL     MSG(13)       $MSG
036600020322     C                   EXSR      CARMSG
036700020322     C                   SETON                                        66
036800020322     C                   ELSE                                                   <-- 1 -->
036900020322     C                   MOVE      WCAU          V1CCAU                         *COD.
037000020322     C                   MOVEL     WDCAU         V1DCAU                         *DESCR.
037100020322     C                   ENDIF                                                  <-- 1 ---
037200931217E   1C                   ENDIF
037300020322E   1C                   ENDIF
037400020322
037500931008     C                   ENDSR
037600020322      *------------------------------------------------------------------------*
037700020322      *?CHKTIP - CONTROLLA IL TIPO DI UNITA'
037800020322      *           INPUT:  WTIP  - VALORE IMMESSO A VIDEO
037900020322      *           OUTPUT  WDTIP - DECODIFICA TIPO UNITA'
038000020322      *------------------------------------------------------------------------*
038100931201     C     CHKTIP        BEGSR
038200020322
038300020322      * CONTROLLO SE E' STATO INSERITO '?'
038400020322     C     '?'           SCAN      WTIP                                   99
038500020322
038600020322      * E' STATO INSERITO: LANCIO IL PGM DI INTERROGAZIONE TABELLE
038700020322     c                   if        *in99                                        --- 1 -->
038800020322     c                   clear                   TIBS02DS
038900020322     c                   movel     'R'           t02mod
039000020322     c                   movel     knsif         t02sif
039100020322     c                   movel     'UNT'         t02cod
039200020322     c                   call      'TIBS02R'
039300020322     c                   parm                    KPJBA
039400020322     c                   parm                    TIBS02DS
039500020322     c                   movel     T02ke1        wtip
039600020322     c                   movel     t02uni        wdtip
039700931201     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA
039800020322     c                   endif                                                  <-- 1 ---
039900020322
040000020322      * NON E' STATO INSERITO: CONTROLLO CHE ESISTA IN TABELLA
040100020322     c                   clear                   TIBS02DS
040200020322     c                   movel     'C'           T02mod
040300020322     c                   movel     knsif         t02sif
040400020322     c                   movel     'UNT'         t02cod
040500020322     c                   movel(p)  wtip          T02ke1
040600020322     c                   call      'TIBS02R'
040700020322     c                   parm                    KPJBA
040800020322     c                   parm                    TIBS02DS
040900020322     c                   if        t02err <> *blanks
041000020322     c                   eval      $err = '1'
041100020322     c                   clear                   wdtip
041200020322     c                   else
041300020322     c                   movel     t02uni        wdtip
041400020322     c                   endif
041500020322
041600931201     C                   ENDSR
041700931201     C*------------------------------------------------------------------------*
041800931201     C*?CHKMAG - CONTROLLA IL MAGAZZINO UBICAZIONE
041900931202     C*           INPUT:  WMAG  - VALORE IMMESSO A VIDEO
042000931202     C*           OUTPUT  WDMAG - DECODIFICA MAGAZZINO UBIACAZIONE
042100931201     C*------------------------------------------------------------------------*
042200931201     C     CHKMAG        BEGSR
042300931201     C*
042400931201     C* CONTROLLO SE E' STATO INSERITO '?'
042500931201     C     '?'           SCAN      WMAG          A                 1 0    99
042600931201     C*
042700931202     C* E' STATO INSERITO; LANCIO IL PGM DI INTERROGAZIONE LINEE ARRIVO
042800931202IF  1C     *IN99         IFEQ      '1'
042900931202     C                   MOVEL     *BLANKS       §COD1
043000931201     C                   MOVE      *BLANKS       §DES1
043100941115     C                   CALL      'TNSD20R'
043200931202     C                   PARM                    §COD1             3
043300931201     C                   PARM                    §TIP              1
043400931201     C                   PARM                    §DES1            25
043500931202     C*
043600931202     C                   MOVEL     §COD1         WMAG
043700931202     C                   MOVEL     §DES1         WDMAG
043800931202     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA
043900931202     C*
044000931202     C* NON E' STATO INSERITO: CONTROLLO CHE ESISTA IN AZORG
044100931202X   1C                   ELSE
044200931201     C                   MOVE      WMAG          ORGFIL
044300020529     C     KEYORG        CHAIN     AZORG21l                           99
044400931202IF  2C     *IN99         IFEQ      '0'
044500931202     C     ORGFVA        ANDEQ     *BLANKS
044600931201     C                   MOVEL     ORGFIL        WMAG
044700931201     C                   MOVEL     ORGDES        WDMAG
044800931202X   2C                   ELSE
044900020529     c     keyorg        chain     azorg01l
045000020529     c                   if        %found(azorg01l) and orgfva = *blanks
045100020529     c                   movel     orgfil        wmag
045200020529     c                   movel     orgdes        wdmag
045300020529     c                   else
045400931201     C                   MOVEL     '1'           $ERR
045500931201     C                   MOVEL     *ALL'*'       WDMAG
045600020529     c                   endif
045700931202E   2C                   ENDIF
045800931202E   1C                   ENDIF
045900931201     C*
046000931201     C                   ENDSR
046100020322      *------------------------------------------------------------------------*
046200020322      *?CHKCAU - CONTROLLA LA CAUSALE MOVIMENTO
046300020322      *           INPUT:  WCAU  - VALORE IMMESSO A VIDEO
046400020322      *           OUTPUT  WDCAU - DECODIFICA CAUSALE MOVIMENTO
046500020322      *------------------------------------------------------------------------*
046600020322     C     CHKCAU        BEGSR
046700020322
046800020322      * CONTROLLO SE E' STATO INSERITO '?'
046900020322     C     '?'           SCAN      WCAU                                   99
047000020322
047100020322      * E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
047200020322     c                   if        *in99                                        --- 1 -->
047300020322     c                   clear                   TIBS02DS
047400020322     c                   movel     'R'           t02mod
047500020322     c                   movel     knsif         t02sif
047600020322     c                   movel     'UNM'         t02cod
047700020322     c                   call      'TIBS02R'
047800020322     c                   parm                    KPJBA
047900020322     c                   parm                    TIBS02DS
048000020322     c                   movel     T02ke1        wcau              4
048100020322     c                   movel     t02uni        wdcau            30
048200020322     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
048300020322     c                   endif                                                  <-- 1 ---
048400020322
048500020322      * NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
048600020322     c                   clear                   TIBS02DS
048700020322     c                   movel     'C'           T02mod
048800020322     c                   movel     knsif         t02sif
048900020322     c                   movel     'UNM'         t02cod
049000020322     c                   movel(p)  wcau          T02ke1
049100020322     c                   call      'TIBS02R'
049200020322     c                   parm                    KPJBA
049300020322     c                   parm                    TIBS02DS
049400020322     c                   if        t02err <> *blanks
049500020322     c                   eval      $err = '1'
049600020322     c                   clear                   wdcau
049700020322     c                   else
049800020322     c                   movel     t02uni        wdcau
049900020322     c                   endif
050000020322
050100020322     C                   ENDSR
050200931201     C*------------------------------------------------------------------------*
050300931201     C*?*INZSR - ROUTINE INIZIALE
050400931201     C*------------------------------------------------------------------------*
050500930713     C     *INZSR        BEGSR
050600000000     C*
050700000000     C* RICEVIMENTO PARAMETRI
050800000000     C     *ENTRY        PLIST
050900930922     C                   PARM                    KPJBA
051000940105     C                   MOVEL     KPJBU         DSUNIT
051100930310     C*
051200020529     C* CHIAVE LETTURA AZORG01L/azorg21l - COMPLETA
051300930310     C     KEYORG        KLIST
051400930712     C                   KFLD                    ORGFIL
051500930714     C*
051600930714     C* ESEGUE OPERAZIONI INIZIALI STANDARD
051700930921     C                   EXSR      INZSTD
051800931202     C*
051900931008     C* IMPOSTO CAMPI VIDEO RICHIESTA PARAMETRI
052000931202     C                   Z-ADD     1             V1CORD                         *ORDINAMENTO
052100930922     C*
052200930924     C* EMETTO FORMATO VIDEO
052300930924     C                   EXSR      CARV1                                        *EMISSIONE VIDEO
052400930923     C*
052500000000     C                   ENDSR
052600931201     C*------------------------------------------------------------------------*
052700931202     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
052800931202     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
052900931202     C*     LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
053000931202     C*     STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
053100931202     C*     NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
053200931202     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
053300931202     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
053400931201     C*------------------------------------------------------------------------*
053500930923     C* INZSTD - ROUTINE INIZIALE STANDARD
053600931201     C*------------------------------------------------------------------------*
053700930803     C     INZSTD        BEGSR
053800930714     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
053900930802     C                   CALL      'XKEYS'
054000930802     C                   PARM                    DSKEYS
054100930805     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
054200930714     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
054300930714     C     XMSGPA        PLIST
054400931213     C                   PARM                    DSMSMS
054500931213     C                   PARM                    DSMSPG
054600930714     C                   PARM                    DSMSKE
054700930714     C                   PARM                    DSMSER
054800930714     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
054900941115     C                   MOVEL     'TNSDU8R'     DSMSPG                         *NOME PGM CHIAMANTE
055000941115     C                   MOVEL     'TNSDU8R'     VMCQUE                         *CODA MESSAGGI DESTI
055100930806     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
055200930714     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
055300930805     C* INIZIALIZZO FLAG CONTROLLO PGM
055400930721     C                   MOVEL     '0'           $ERR              1            *ERRORE
055500930721     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
055600930806     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
055700930721     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
055800930721     C                   MOVEL     '0'           $FINE             1            *FINE PGM
055900930922     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
056000930923     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
056100930714     C* LANCIO ROUTINE PER VERIFICARE SE
056200930714     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
056300930714     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
056400020709     C**!!!              CALL      'XFIL'
056500020709     C**!!!              PARM                    DSFIL
056600020709
056700020709     c     *dtaara       define    §azute        azuteds
056800020709     c     *dtaara       define    §datiute      ddatiute
056900020709
057000020709     c                   in(E)     *dtaara
057100020709     c                   if        %Error or rsut = *blanks
057200020709     c                   clear                   tibs34ds
057300020709     c                   call      'TIBS34R'
057400020709     c                   parm                    Tibs34Ds
057500020709     c                   in        *dtaara
057600020709     c                   endif
057700930805     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
057800930721     C                   BITOFF    '01234567'    POSMI1            1
057900930721     C                   BITOFF    '01234567'    POSMI2            1
058000930721     C                   BITOFF    '01234567'    POSMA1            1
058100930721     C                   BITOFF    '01234567'    POSMA2            1
058200930721     C                   BITON     '34'          POSMI1            1            *18
058300930721     C                   BITON     '7'           POSMI2            1            *01
058400930721     C                   BITON     '34'          POSMA1            1            *18
058500930721     C                   BITON     '0'           POSMA2            1            *80
058600930721     C                   MOVE      POSMI2        POSMIN            2
058700930721     C                   MOVEL     POSMI1        POSMIN
058800930721     C                   MOVE      POSMA2        POSMAX            2
058900930721     C                   MOVEL     POSMA1        POSMAX
059000930714     C                   ENDSR
059100931201     C*------------------------------------------------------------------------*
059200930923     C* FUNSTD - LANCIA SUBROUTINE STANDARD
059300931201     C*------------------------------------------------------------------------*
059400930804     C     FUNSTD        BEGSR
059500930804     C                   SELECT
059600930804     C     KEY           WHENEQ    F3
059700930804     C                   EXSR      F03STD
059800931115     C     KEY           WHENEQ    F12
059900931115     C                   EXSR      F12STD
060000930804     C     KEY           WHENEQ    F24
060100930804     C                   EXSR      F24STD
060200930804     C                   OTHER
060300930804     C                   MOVEL     MSG(2)        $MSG
060400930804     C                   EXSR      CARMSG
060500930804     C                   ENDSL
060600930804     C                   ENDSR
060700931201     C*------------------------------------------------------------------------*
060800930923     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
060900931201     C*------------------------------------------------------------------------*
061000930804     C     F03STD        BEGSR
061100930923     C*
061200940105     C* IMPOSTO FLAG DI PREMUTO TASTO F03
061300940105     C                   MOVEL     '1'           $F03
061400940105     C*
061500930804     C* IMPOSTO FLAG DI FINE PROGRAMMA
061600930927     C                   MOVEL     '1'           $FINE
061700930927     C*
061800930927     C                   ENDSR
061900931201     C*------------------------------------------------------------------------*
062000931115     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
062100931201     C*------------------------------------------------------------------------*
062200931115     C     F12STD        BEGSR
062300931115     C*
062400931115     C* IMPOSTO FLAG DI FINE PROGRAMMA
062500931115     C                   MOVEL     '1'           $FINE
062600931115     C*
062700931115     C                   ENDSR
062800931201     C*------------------------------------------------------------------------*
062900930923     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
063000931201     C*------------------------------------------------------------------------*
063100930804     C     F24STD        BEGSR
063200930923     C*
063300930804     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
063400930804     C                   EXSR      OUTFUN
063500930923     C*
063600930804     C                   ENDSR
063700931201     C*------------------------------------------------------------------------*
063800930923     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
063900930721     C*          CODA MESSAGGI DEL PGM
064000931201     C*------------------------------------------------------------------------*
064100930721     C     CARMSG        BEGSR
064200930723     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
064300930723     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
064400930721     C     MSGULT        IFEQ      0
064500930721     C                   MOVEL     '1'           *IN40
064600941115     C                   WRITE     SDU8DVK
064700930721     C                   MOVEL     '0'           *IN40
064800930805     C                   ENDIF
064900930721     C* INVIA MESSAGGIO ALLA CODA DEL PGM
065000930721     C                   MOVEL     $MSG          DSMSMS
065100930721     C                   CALL      'XMSG'        XMSGPA
065200930721     C     DSMSER        IFEQ      0
065300930806     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
065400930805     C                   ENDIF
065500930723     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
065600930923     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
065700941115     C                   WRITE     SDU8DVM                                      *SFL MESS
065800930723     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
065900930721     C                   MOVEL     *BLANKS       $MSG
066000930721     C                   ENDSR
066100931201     C*------------------------------------------------------------------------*
066200930923     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
066300931201     C*------------------------------------------------------------------------*
066400930721     C     OUTMSG        BEGSR
066500930721     C* VISUALIZZAZIONE MESSAGGIO
066600930721     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
066700941115     C                   WRITE     SDU8DVK                                      *CONTR MESS
066800930721     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
066900930721     C                   Z-ADD     *ZEROS        MSGULT
067000930721     C                   ENDSR
067100931201     C*------------------------------------------------------------------------*
067200930923     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
067300930803     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
067400930803     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
067500930803     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
067600930803     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
067700931201     C*------------------------------------------------------------------------*
067800930803     C     CARFUN        BEGSR
067900930803     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
068000930923     C     1             DO        31            I                 5 0          --- 1 -->
068100930803     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
068200930805     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
068300930805     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
068400020709     C**!!!DSFITI        IFEQ      'S'                                          --- 2 --> *SEDE
068500020709     c                   If        DutLpo = 'S'
068600930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
068700930805     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
068800930803     C                   ELSE                                                   *INTERROG.
068900930803     C                   MOVEL     FUNASI        FAT(I)
069000930803     C                   ENDIF
069100930803     C                   ELSE                                                   <-- 2 --> *FILIALE
069200930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
069300930803     C                   MOVEL     FUNAFG        FAT(I)
069400930803     C                   ELSE                                                   *INTERROG.
069500930922     C                   MOVEL     FUNAFI        FAT(I)
069600930803     C                   ENDIF
069700930803     C                   ENDIF                                                  <-- 2 ---
069800930806     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
069900930806     C                   MOVEL     USER          WEDP              3            *PROFILO
070000930806     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
070100930806     C                   MOVEL     'S'           FAT(I)                         *ABILITO
070200930806     C                   ELSE
070300930806     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
070400930806     C                   ENDIF
070500930806     C                   ENDIF
070600930803     C                   ENDDO                                                  <-- 1 ---
070700930803     C                   ENDSR
070800931201     C*------------------------------------------------------------------------*
070900930923     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
071000930803     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
071100930806     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
071200930803     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
071300931201     C*------------------------------------------------------------------------*
071400930803     C     OUTFUN        BEGSR
071500930805     C     1             DO        8             I                              *PULIZIA FUNZIONI A
071600930805     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
071700930803     C                   ENDDO
071800930803     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
071900930803     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
072000930803     C     1             DO        31            I
072100930803     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
072200930803     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
072300930803     C                   ADD       1             FUNNAT
072400930803     C                   ENDIF
072500930803     C                   ENDDO
072600930805     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
072700930803     C     FUNNAT        IFGT      8
072800930803     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
072900930803     C                   ENDIF
073000930803     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
073100930805     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
073200930803     C                   Z-ADD     8             FUNVIS            5 0
073300930803     C     FAT(23)       IFEQ      'S'
073400930803     C                   SUB       1             FUNVIS
073500930803     C                   ENDIF
073600930803     C     FAT(24)       IFEQ      'S'
073700930803     C                   SUB       1             FUNVIS
073800930803     C                   ENDIF
073900930805     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
074000930923     C                   Z-ADD     *ZEROS        W                 5 0
074100930803     C                   ADD       1             FUNULT
074200930803     C     FUNULT        IFGT      31
074300930803     C                   Z-ADD     1             FUNULT
074400930805     C                   ENDIF
074500930805     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
074600930803     C     FUNULT        DO        31            I                              --- 1 -->
074700930803     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
074800930803     C     W             IFLT      FUNVIS                                       --- 2 -->
074900930803     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
075000930803     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
075100930803     C     FDE(I)        ANDNE     *BLANKS
075200930805     C     I             IFEQ      23                                           *TOLGO F23
075300930805     C     I             OREQ      24                                           *E F24
075400930803     C                   ELSE
075500930803     C                   ADD       1             W
075600930803     C                   MOVEL     FDE(I)        FUV(W)
075700930803     C                   ENDIF
075800930803     C                   ENDIF                                                  <-- 3 ---
075900930803     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
076000930803     C                   Z-ADD     I             FUNULT
076100930803     C                   ENDIF                                                  <-- 2 ---
076200930803     C                   ENDDO                                                  <-- 1 ---
076300930803     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
076400930806     C                   MOVEL     FUV(1)        VFD001
076500930806     C                   MOVEL     FUV(2)        VFD002
076600930806     C                   MOVEL     FUV(3)        VFD003
076700930806     C                   MOVEL     FUV(4)        VFD004
076800930806     C                   MOVEL     FUV(5)        VFD005
076900930806     C                   MOVEL     FUV(6)        VFD006
077000930803     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
077100930803     C     FAT(24)       ANDEQ     'N'
077200930806     C                   MOVEL     FUV(7)        VFD007
077300930806     C                   MOVEL     FUV(8)        VFD008
077400930803     C                   ELSE                                                   <-- 1 -->
077500930803     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
077600930803     C     FAT(24)       ANDEQ     'S'
077700930806     C                   MOVEL     FDE(23)       VFD007
077800930806     C                   MOVEL     FDE(24)       VFD008
077900930803     C                   ELSE                                                   <-- 2 -->
078000930806     C                   MOVEL     FUV(7)        VFD007
078100930803     C     FAT(23)       IFEQ      'S'
078200930806     C                   MOVEL     FDE(23)       VFD008
078300930803     C                   ELSE
078400930806     C                   MOVEL     FDE(24)       VFD008
078500930805     C                   ENDIF
078600930805     C                   ENDIF                                                  <-- 2 ---
078700930805     C                   ENDIF                                                  <-- 1 ---
078800930922     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
078900930803     C                   ENDSR
079000931201     C*------------------------------------------------------------------------*
079100930923     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
079200930803     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
079300930803     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
079400930803     C*                   - ERRORE SE FUNZIONE NON ATTIVA
079500931201     C*------------------------------------------------------------------------*
079600930803     C     CONFUN        BEGSR
079700930803     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
079800930803     C     1             DO        31            I                              --- 1 -->
079900930803     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
080000930805     C* CONTROLLO SE IL TASTO E' ATTIVO
080100930803     C     FAT(I)        IFNE      'S'                                          --- 3 -->
080200930804     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
080300930803     C                   EXSR      CARMSG
080400930803     C                   MOVEL     '1'           $ERR
080500930803     C                   ENDIF                                                  <-- 3 ---
080600930803     C                   ENDIF                                                  <-- 2 ---
080700930803     C                   ENDDO                                                  <-- 1 ---
080800930803     C                   ENDSR
080900931201     O*------------------------------------------------------------------------*
081000930802**       MSG - MESSAGGI
081100941115TNSDU8R- Tasto funzionale non valido.                                          1
081200941115TNSDU8R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
081300941115TNSDU8R- Opzione non valida.                                                   3
081400941115TNSDU8R- Opzione abilitata ma non gestita. Avvertire CED.                      4
081500941115TNSDU8R- Inizio elenco.                                                        5
081600941115TNSDU8R- Fine elenco.                                                          6
081700941115TNSDU8R- Scegliere uno degli ordinamenti possibili                             7
081800941115TNSDU8R- Tipo unità errato                                                     8
081900941115TNSDU8R- Magazzino errato                                                      9
082000941115TNSDU8R- Data movimento errata                                                10
082100941115TNSDU8R- Inserire anche il tipo unità se selezionato il codice                11
082200941115TNSDU8R- Stampa lanciata ...                                                  12
082300020322TNSDU8R- Causale inesistente                                                  13
082400941115TNSDU8R-                                                                      14
082500941115TNSDU8R-                                                                      15
082600941115TNSDU8R- Data AL inferiore a data DAL                                         16
082700941115TNSDU8R- Immettere la data AL solo se immessa la data DAL                     17
082800941115TNSDU8R-                                                                      18
082900941115TNSDU8R-                                                                      19
083000941115TNSDU8R-                                                                      20
083100940105** ======== SCHIERA: CMD  (COMANDI)                        ================
083200940105WRKSPLF
083300940105
083400940105
083500940105
083600940105
083700930806** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
083800930806F1    -                   -    NN     NN
083900930723F2    -                   -    NN     NN
084000930921F3    -F3=Fine            -    SS     SS
084100930723F4    -                   -    NN     NN
084200930723F5    -                   -    NN     NN
084300931026F6    -F6=Lancio          -    SS     SS
084400930723F7    -                   -    NN     NN
084500930723F8    -                   -    NN     NN
084600930723F9    -                   -    NN     NN
084700930723F10   -                   -    NN     NN
084800930723F11   -                   -    NN     NN
084900931115F12   -F12-Ritorno        -    SS     SS
085000930723F13   -                   -    NN     NN
085100930723F14   -                   -    NN     NN
085200930723F15   -                   -    NN     NN
085300930723F16   -                   -    NN     NN
085400930723F17   -                   -    NN     NN
085500930723F18   -                   -    NN     NN
085600930723F19   -                   -    NN     NN
085700930723F20   -                   -    NN     NN
085800930804F21   -                   -    NN     NN
085900941215F22   -F22=Gestione stampe-    SS     SS
086000930723F23   -                   -    NN     NN
086100930803F24   -F24=Altri tasti    -    NN     NN
086200930921ENTER -                   -    SS     SS
086300930802ROLLUP-                   -    NN     NN
086400930802ROLLDN-                   -    NN     NN
086500930802HELP  -                   -    NN     NN
086600930802PRINT -                   -    NN     NN
086700930802BSPACE-                   -    NN     NN
086800930802CLEAR -                   -    NN     NN
