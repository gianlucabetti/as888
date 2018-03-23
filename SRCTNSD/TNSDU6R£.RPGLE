000100020424     H DECEDIT('0,') DATEDIT(*ymd.) option(*nodebugio)
000200020322      *------------------------------------------------------------------------*
000300020322      * STAMPA UNITA' EDP - RICHIESTA PARAMETRI
000400020322      *------------------------------------------------------------------------*
000500020322
000600020322      *------------------------------------------------------------------------*
000700020322      * DATA BASE
000800020322      *------------------------------------------------------------------------*
000900941115     FTNSDU6D   CF   E             WORKSTN INFDS(DEVDS1)
001000941115     F                                     SFILE(SDU6DVM:MSGULT)
001100020529     FAZORG21l  IF   E           K DISK
001200020529     fazorg01l  if   e           k disk
001300020322
001400020322      *------------------------------------------------------------------------*
001500020322      * SCHIERE
001600020322      *------------------------------------------------------------------------*
001700020424     D MSG             S             78    DIM(30) CTDATA PERRCD(1)             *MESSAGGI
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
002800020523
002900020523     d  skpoabi        s              3  0 dim(500)
003000090904
003100090904       //--------------------------------------------------------------
003200090904       // COSTANTI
003300090904       //--------------------------------------------------------------
003400090904
003500090904     d c_Logistica     c                   const('LOG')
003600090904
003700020322      *------------------------------------------------------------------------*
003800020322      * INPUT
003900020322      *------------------------------------------------------------------------*
004000020424
004100020424     d wabi            s                   like(uteaut)
004200020517     d ii              s              3  0
004300020523     d wwmag           s              3  0
004400020424
004500020322      * RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004600930723     D DSFUN         E DS
004700930803     D  FUNRIG                 1     40
004800020322      * PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004900930127     D DSMSG         E DS
005000971010     D WLBDA8          DS
005100971010     D  G08DAT                 1      8  0
005200971010     D  G08INV                 9     16  0
005300971010     D  G08ERR                17     17
005400971010     D  G08TGI                18     22  0
005500020322      * DS PER GESTIONE TASTI FUNZIONALI
005600930802     D DSKEYS        E DS
005700930803     D  KEYRIG                 1     40
005800020322      * RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005900930806     D                SDS
006000930806     D  USER                 254    263
006100020322      * RIDEFINIZIONE INFDS DEL FILE VIDEO
006200000000     D DEVDS1          DS
006300000000     D  KEY                  369    369
006400930721     D  CURSOR               370    371
006500020322      * ARCHITETTURA
006600930922     D KPJBA         E DS
006700931008     D PARAM           DS                  INZ
006800971010     D  PARORD                 1      1  0
006900971010     D  PARTIP                 2      3  0
007000971010     D  PARMAG                 4      6  0
007100971010     D  PARDSM                 7     14  0
007200971010     D  PARDS2                15     22  0
007300971010     D  PARMAR                23     37
007400971010     D  PARMOD                38     52
007500971010     D  PARTFI                53     56
007600971010     D  PARSOD                57     57
007700971010     D  PARTEC                58     58
007800971010     D  DECTIP                59     88
007900971010     D  DECMAG                89    118
008000971010     D  DECDSM               119    126  0
008100971010     D  DECDS2               127    134  0
008200971010     D  PARLIF               135    135
008300971010     D  PARTOT               136    136
008400971010     D  PARFOS               137    137
008500020322      * CAMPO UNICO TIPI FILIALE
008600931202     D V1CTFI          DS                  INZ
008700931202     D  V1CTF1                 1      1                                         TIPO 1
008800931202     D  V1CTF2                 2      2                                         TIPO 2
008900931202     D  V1CTF3                 3      3                                         TIPO 3
009000931202     D  V1CTF4                 4      4                                         TIPO 4
009100020322      * PARAMETRI PER LANCIO ALTRI PGM DELLA PROCEDURA
009200931201     D DSUNIT        E DS
009300020322
009400020322     d tibs02ds      e ds
009500020424
009600020424     d ddatiute      e ds
009700020424     d azuteds       e ds                  extname(azute00f)
009800020424     d tibs34ds      e ds                  inz
009900020424     d dute01        e ds
010000020424     d dlat          e ds
010100020516     d trul31ds      e ds
010200020523     d  skpo                  10    759  0 dim(250)
010300090904      *
010400090904     d Og143         e ds                  inz
010500020322
010600931201     C*------------------------------------------------------------------------*
010700971010     C* MAIN LINE
010800931201     C*------------------------------------------------------------------------*
010900931008     C*
011000000000     C* CICLO FINO A CHE:
011100930922DO  1C     $FINE         DOUEQ     '1'
011200000000     C*
011300931008     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
011400000000     C*
011500930721     C* CONTROLLA GESTIONE VIDEO
011600930922IF  2C     $GES          IFEQ      'V1'
011700930806     C                   EXSR      GESV1                                        *PARAMETRI
011800930922E   2C                   ENDIF
011900000000     C*
012000930922E   1C                   ENDDO
012100940105     C*
012200940105     C* SE E' STATO PREMUTO F03 IMPOSTO FLAG DELLA DS DI PROCEDURA
012300940105IF  1C     $F03          IFEQ      '1'
012400940105     C                   MOVEL     '1'           DSUF03
012500940105E   1C                   ENDIF
012600000000     C*
012700931018     C* FINE MAIN DEL PGM
012800940105     C                   MOVEL     DSUNIT        KPJBU
012900000000     C                   SETON                                        LR
013000931201     C*------------------------------------------------------------------------*
013100931201     C*?GESV1 - GESTIONE VIDEO PARAMETRI
013200931201     C*------------------------------------------------------------------------*
013300930922     C     GESV1         BEGSR
013400930922     C*
013500930922IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
013600930922     C                   EXSR      CARV1                                        *CARICA VIDEO
013700930922E   1C                   ENDIF
013800020424
013900020424      * controllo il profilo utente
014000090904     c                   if        skPOabi(1) = *zeros
014100020424     c                   exsr      ctrute
014200090904     c                   endif
014300020424     c                   if        *in76
014400020424     c                   eval      $fine = *on
014500020424     c                   endif
014600930922     C*
014700931018IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
014800931018     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
014900931018     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
015000931018E   1C                   ENDIF
015100931018     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
015200930922     C*
015300941115     C                   EXFMT     SDU6DV1                                      *PARAMETRI
015400930922     C*
015500930922     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
015600930922     C*
015700930922IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
015800931008SL  2C                   SELECT
015900930922     C     KEY           WHENEQ    ENTER
016000930922     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
016100930922     C     KEY           WHENEQ    F6
016200930922     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
016300971010     C     KEY           WHENEQ    F22
016400940105     C                   EXSR      F22GES                                       *GESTIONE STAMPE
016500930922     C                   OTHER
016600930922     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
016700930922E   2C                   ENDSL
016800930922E   1C                   ENDIF
016900930922     C*
017000930922     C                   ENDSR
017100931201     C*------------------------------------------------------------------------*
017200931201     C*?CARV1 - CARICA VIDEO PARAMETRI
017300931201     C*------------------------------------------------------------------------*
017400930806     C     CARV1         BEGSR
017500930713     C*
017600930806     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
017700930713     C*
017800930804     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
017900930804     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
018000930713     C*
018100941115     C                   WRITE     SDU6DVT                                      *TESTATA
018200941115     C                   WRITE     SDU6DVF                                      *FUNZIONI
018300940105     C*
018400940105     C                   MOVEL     '0'           $TESVD                         *TESTATA RIEMESSA
018500930713     C*
018600930713     C                   ENDSR
018700931201     C*------------------------------------------------------------------------*
018800931201     C*?ENTV1 - GESTIONE TASTO ENTER VIDEO 1 (PARAMETRI)
018900931201     C*------------------------------------------------------------------------*
019000930806     C     ENTV1         BEGSR
019100930806     C*
019200930806     C* CONTROLLA I CAMPI IMMESSI
019300930923     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
019400930804     C*
019500930804     C                   ENDSR
019600931201     C*------------------------------------------------------------------------*
019700931201     C*?F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
019800931201     C*------------------------------------------------------------------------*
019900930126     C     F06GES        BEGSR
020000930923     C*
020100930924     C* CONTROLLA I CAMPI IMMESSI
020200930924     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
020300930923     C*
020400930923     C* NON CI SONO ERRORI
020500930923IF  1C     $ERR          IFEQ      '0'
020600930923     C*
020700930923     C* LANCIO PROGRAMMA DI STAMPA
020800931202     C                   MOVEL     V1CORD        PARORD                         *TIPO ORDINAMENTO
020900931202     C                   MOVEL     V1CTIP        PARTIP                         *TIPO UNITA'
021000931202     C                   MOVEL     V1CMAG        PARMAG                         *MAGAZZINO
021100931202     C                   Z-ADD     DSMV1C        PARDSM                         *SCAD MANUT DAL A/M/
021200931202     C                   Z-ADD     DS2V1C        PARDS2                         *SCAD MANUT AL  A/M/
021300931202     C                   MOVEL     V1CMAR        PARMAR                         *MARCA
021400931202     C                   MOVEL     V1CMOD        PARMOD                         *MODELLO
021500960930     C                   MOVEL     V1CTFI        PARTFI                         *TIPI PUNTI OPERATIV
021600931202     C                   MOVEL     V1CSOD        PARSOD                         *SINTETICA/DETTAGLIO
021700931202     C                   MOVEL     V1CTEC        PARTEC                         *DATI TECNICI
021800931206     C                   MOVEL     V1DTIP        DECTIP                         *DECODIFICA TIPO
021900931206     C                   MOVEL     V1DMAG        DECMAG                         *DECODIFICA MAGAZZIN
022000931203     C                   Z-ADD     V1CDSM        DECDSM                         *DATA SCAD DAL G/M/A
022100931203     C                   Z-ADD     V1CDS2        DECDS2                         *DATA SCAD AL  G/M/A
022200960930     C                   MOVEL     V1CLIF        PARLIF                         *LIVELLO P.OPERATIVI
022300950330     C                   MOVEL     V1CTOT        PARTOT                         *STAMPA TOTALI
022400960514     C                   MOVEL     V1CFOS        PARFOS                         *STAMPA PER FIL/SEDE
022500940104     C*
022600941115     C                   MOVEL     'SDU7'        KCOAZ
022700940104     C                   MOVEL     PARAM         KPJBU
022800090904     c                   if        KNMUS = *all'1'
022900090904     c                   call      'TNSDU7R'
023000090904     c                   parm                    KPJBA
023100090904     c                   else
023200020516     C                   CALL      'BCH10'
023300940104     C                   PARM                    KPJBA
023400090904     c                   endif
023500930923     C*
023600940201     C                   MOVEL     MSG(19)       $MSG                           *STAMPA LANCIATA ...
023700940105     C                   EXSR      CARMSG
023800931215     C*
023900930923E   1C                   ENDIF
024000930127     C*
024100000000     C                   ENDSR
024200940105     C*------------------------------------------------------------------------*
024300940105     C*?F22GES -GEST.TASTO FUNZ. F22 ==> STAMPE
024400940105     C*------------------------------------------------------------------------*
024500940105     C     F22GES        BEGSR
024600940105     C*
024700940105     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
024800940105     C                   Z-ADD     80            LENGH            15 5
024900940105     C                   CALL      'QCAEXEC'
025000940105     C                   PARM                    CMD(1)
025100940105     C                   PARM                    LENGH
025200940105     C*
025300940105     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
025400940105     C*
025500940105     C                   ENDSR
025600931201     C*------------------------------------------------------------------------*
025700931201     C*?DECODI - CONTROLLA CAMPI E DECODIFICA
025800931201     C*------------------------------------------------------------------------*
025900930906     C     DECODI        BEGSR
026000930205     C*
026100931201     C* REIMPOSTO INDICATORI E VARIABILI DI CONTROLLO
026200931124     C                   SETOFF                                       606162
026300931201     C                   SETOFF                                       636465
026400931201     C                   SETOFF                                       666768
026500931202     C                   SETOFF                                       697071
026600950330     C                   SETOFF                                       727374
026700960514     C                   SETOFF                                       75
026800930712     C*
026900931201     C* TIPO ORDINAMENTO
027000931202IF  1C     V1CORD        IFEQ      1
027100931202     C     V1CORD        OREQ      2
027200931202     C     V1CORD        OREQ      3
027300931202     C     V1CORD        OREQ      4
027400931202     C     V1CORD        OREQ      5
027500931202X   1C                   ELSE
027600930922     C                   MOVEL     '1'           $ERR
027700930922     C                   MOVEL     MSG(7)        $MSG
027800930922     C                   EXSR      CARMSG
027900931201     C                   SETON                                        60
028000931201E   1C                   ENDIF
028100020510
028200020510      * Controllo che l'ordinamento richiesto sia di tipo 3 se l'utente non ha abilitazione
028300020510      * 'AZ' cioè azienda
028400070202      * aggiungo anche l'ordinamento '2'
028500090904      * aggiungo anche utente Logistica
028600020510     c                   if        $err = *off and wabi <> 'AZ' and v1cord <> 3
028700070202     c                             and v1cord <> 2
028800090904     c                             and DUTntw <> c_Logistica
028900020510     c                   eval      $err = *on
029000020510     c                   eval      $msg = msg(25)
029100020510     c                   exsr      carmsg
029200020510     c                   eval      *in60 = *on
029300020510     c                   endif
029400020328
029500931201     C*
029600931201     C* TIPO UNITA'
029700931201IF  1C     $ERR          IFEQ      '0'
029800931202IF  2C     V1CTIP        IFNE      *BLANKS                                      *SELEZIONATO
029900931201     C     V1CTIP        ANDNE     *ZEROS
030000931201     C                   MOVEL     V1CTIP        WTIP              2
030100931201     C                   MOVEL     *BLANKS       WDTIP            30
030200931201     C                   EXSR      CHKTIP
030300931202     C                   MOVEL     WTIP          V1CTIP
030400931202     C                   MOVEL     WDTIP         V1DTIP
030500931202     C*
030600931202IF  3C     $ERR          IFEQ      '1'                                          *ERRORE
030700931202     C                   MOVEL     MSG(8)        $MSG
030800931202     C                   EXSR      CARMSG
030900931202     C                   SETON                                        61
031000931202E   3C                   ENDIF
031100931202     C*
031200931202X   2C                   ELSE                                                   *NON SELEZIONATO
031300931201     C                   MOVEL     *ZEROS        V1CTIP
031400931201     C                   MOVEL     *BLANKS       V1DTIP
031500931201E   2C                   ENDIF
031600931201E   1C                   ENDIF
031700931202     C*
031800931202     C* MAGAZZINO UBICAZIONE
031900931202IF  1C     $ERR          IFEQ      '0'
032000090907     c                   select
032100090907IF  2C     V1CMAG        whenNE    *BLANKS                                      *SELEZIONATO
032200931202     C     V1CMAG        ANDNE     *ZEROS
032300931202     C                   MOVEL     V1CMAG        WMAG              3
032400931202     C                   MOVEL     *BLANKS       WDMAG            30
032500931202     C                   EXSR      CHKMAG
032600931202     C                   MOVEL     WMAG          V1CMAG
032700931202     C                   MOVEL     WDMAG         V1DMAG
032800931202     C*
032900931202IF  3C     $ERR          IFEQ      '1'                                          *ERRORE
033000931202     C                   MOVEL     MSG(9)        $MSG
033100931202     C                   EXSR      CARMSG
033200931202     C                   SETON                                        62
033300931202E   3C                   ENDIF
033400090907      *
033500090907     c                   when      (V1Cmag = *blanks   or
033600090907     c                              V1Cmag = *zeros)   and
033700090907     c                             DUTntw = c_Logistica
033800090907     c                   eval      V1Cmag  = *zeros
033900090907     c                   eval      V1Dmag = 'Tutti i magazzini LOGISTICA   '
034000931202     C*
034100090907X   2C                   other                                                  *NON SELEZIOANTO
034200931202     C                   MOVEL     *ZEROS        V1CMAG
034300931202     C                   MOVEL     *BLANKS       V1DMAG
034400090907E   2C                   endsl
034500931202E   1C                   ENDIF
034600020513
034700020513      * Se ordinamento di tipo 3 se l'utente non ha abilitazione 'AZ' il magazzino è
034800020513      * obbligatorio
034900070202      * anche per il tipo ordinamento 2
035000090904      * ma NON per utente della Logistica
035100070202     c                   if        $err = *off and wabi <> 'AZ' and
035200090904     c                             (v1cord = 3 or v1cord = 2)
035300090904     c                             and DUTntw <> c_Logistica
035400020513     c                             and (v1cmag = *blanks or v1cmag = *zeros)
035500020513     c                   eval      $err = *on
035600020513     c                   eval      $msg = msg(27)
035700020513     c                   exsr      carmsg
035800020513     c                   eval      *in62 = *on
035900020513     c                   endif
036000020510
036100020510      * il magazzino deve essere congruente con le abilitazioni dell'utente
036200020510     c                   if        $err = *off and v1cmag <> *blanks
036300020513     c                             and v1cmag <> '951'
036400020523     c                   movel     v1cmag        wwmag
036500020523     c     wwmag         lookup    skpoabi                                99
036600020510     c                   if        not *in99
036700020510     c                   eval      $err = *on
036800020510     c                   eval      $msg = msg(26)
036900020510     c                   exsr      carmsg
037000020510     c                   eval      *in62 = *on
037100020510     c                   endif
037200020510     c                   endif
037300930923     C*
037400931202     C* DATA SCADENZA MANUTENZIONE DAL
037500971013     C                   Z-ADD     *ZEROS        DSMV1C                         *NON SELEZIONATA
037600931202IF  1C     $ERR          IFEQ      '0'
037700931202IF  2C     V1CDSM        IFNE      *ZEROS                                       *SELEZIONATA
037800971010     C                   Z-ADD     V1CDSM        G08DAT
037900971010     C                   Z-ADD     *ZEROS        G08INV
038000971010     C                   MOVEL     '0'           G08ERR
038100971010     C                   CALL      'XSRDA8'
038200971010     C                   PARM                    WLBDA8
038300971013     C                   Z-ADD     G08DAT        V1CDSM
038400971013     C                   Z-ADD     G08INV        DSMV1C            8 0
038500971010IF  3C     G08ERR        IFEQ      '1'                                          *ERRORE
038600931202     C                   MOVEL     '1'           $ERR
038700931202     C                   MOVEL     MSG(10)       $MSG
038800931202     C                   EXSR      CARMSG
038900931202     C                   SETON                                        63
039000931202E   3C                   ENDIF
039100931202E   2C                   ENDIF
039200931202E   1C                   ENDIF
039300931202     C*
039400931202     C* DATA SCADENZA MANUTENZIONE AL
039500971013     C                   Z-ADD     *ZEROS        DS2V1C                         *NON SELEZIONATA
039600931202IF  1C     $ERR          IFEQ      '0'
039700931202IF  2C     V1CDS2        IFNE      *ZEROS                                       *SELEZIONATA
039800971010     C                   Z-ADD     V1CDS2        G08DAT
039900971010     C                   Z-ADD     *ZEROS        G08INV
040000971010     C                   MOVEL     '0'           G08ERR
040100971010     C                   CALL      'XSRDA8'
040200971010     C                   PARM                    WLBDA8
040300971013     C                   Z-ADD     G08DAT        V1CDS2
040400971013     C                   Z-ADD     G08INV        DS2V1C            8 0
040500971010IF  3C     G08ERR        IFEQ      '1'                                          *ERRORE
040600931202     C                   MOVEL     '1'           $ERR
040700931202     C                   MOVEL     MSG(10)       $MSG
040800931202     C                   EXSR      CARMSG
040900931202     C                   SETON                                        72
041000931202E   3C                   ENDIF
041100931202E   2C                   ENDIF
041200931202E   1C                   ENDIF
041300931202     C*
041400931202     C* DATA AL NON IMMESSA: LA IMPOSTO UGUALA A DATA DAL
041500931202IF  1C     $ERR          IFEQ      '0'
041600931202IF  2C     V1CDSM        IFGT      *ZEROS
041700931202     C     V1CDS2        ANDEQ     *ZEROS
041800931202     C                   Z-ADD     V1CDSM        V1CDS2
041900931202     C                   Z-ADD     DSMV1C        DS2V1C
042000931202E   2C                   ENDIF
042100931202E   1C                   ENDIF
042200931202     C*
042300931202     C* DATA DAL/DATA AL
042400931202IF  1C     $ERR          IFEQ      '0'
042500931202IF  2C     V1CDSM        IFEQ      *ZEROS                                       *AL SOLO DE DAL
042600931202IF  3C     V1CDS2        IFNE      *ZEROS
042700931202     C                   MOVEL     '1'           $ERR
042800931202     C                   MOVEL     MSG(17)       $MSG
042900931202     C                   EXSR      CARMSG
043000931202     C                   SETON                                        72
043100931202E   3C                   ENDIF
043200931202     C*
043300931202X   2C                   ELSE                                                   *AL MAGGIORE DAL
043400931202IF  3C     V1CDS2        IFLT      V1CDSM
043500931202     C                   MOVEL     '1'           $ERR
043600931202     C                   MOVEL     MSG(16)       $MSG
043700931202     C                   EXSR      CARMSG
043800931202     C                   SETON                                        72
043900931202E   3C                   ENDIF
044000931202     C*
044100931202E   2C                   ENDIF
044200931202E   1C                   ENDIF
044300931202     C*
044400960930     C* TIPO PUNTO OPERATIVO
044500931202IF  1C     $ERR          IFEQ      '0'
044600931202IF  2C     V1CTFI        IFEQ      *BLANKS
044700931202     C     V1CTFI        OREQ      *ZEROS
044800931202     C                   MOVEL     '1'           $ERR
044900931202     C                   MOVEL     MSG(12)       $MSG
045000931202     C                   EXSR      CARMSG
045100931202     C                   SETON                                        666768
045200931202     C                   SETON                                        69
045300931202E   2C                   ENDIF
045400931202E   1C                   ENDIF
045500931202     C*
045600931202IF  1C     $ERR          IFEQ      '0'
045700960930IF  2C     V1CTF1        IFEQ      'F'
045800931202     C     V1CTF1        OREQ      'A'
045900931202     C     V1CTF1        OREQ      'V'
046000931202     C     V1CTF1        OREQ      'W'
046100931202     C     V1CTF1        OREQ      ' '
046200931202X   2C                   ELSE
046300931202     C                   MOVEL     '1'           $ERR
046400931202     C                   MOVEL     MSG(11)       $MSG
046500931202     C                   EXSR      CARMSG
046600931202     C                   SETON                                        66
046700931202E   2C                   ENDIF
046800931202E   1C                   ENDIF
046900931202     C*
047000931202IF  1C     $ERR          IFEQ      '0'
047100931202IF  2C     V1CTF2        IFEQ      'F'
047200931202     C     V1CTF2        OREQ      'A'
047300931202     C     V1CTF2        OREQ      'V'
047400931202     C     V1CTF2        OREQ      'W'
047500931202     C     V1CTF2        OREQ      ' '
047600931202X   2C                   ELSE
047700931202     C                   MOVEL     '1'           $ERR
047800931202     C                   MOVEL     MSG(11)       $MSG
047900931202     C                   EXSR      CARMSG
048000931202     C                   SETON                                        67
048100931202E   2C                   ENDIF
048200931202E   1C                   ENDIF
048300931202     C*
048400931202IF  1C     $ERR          IFEQ      '0'
048500931202IF  2C     V1CTF3        IFEQ      'F'
048600931202     C     V1CTF3        OREQ      'A'
048700931202     C     V1CTF3        OREQ      'V'
048800931202     C     V1CTF3        OREQ      'W'
048900931202     C     V1CTF3        OREQ      ' '
049000931202X   2C                   ELSE
049100931202     C                   MOVEL     '1'           $ERR
049200931202     C                   MOVEL     MSG(11)       $MSG
049300931202     C                   EXSR      CARMSG
049400931202     C                   SETON                                        68
049500931202E   2C                   ENDIF
049600931202E   1C                   ENDIF
049700931202     C*
049800931202IF  1C     $ERR          IFEQ      '0'
049900931202IF  2C     V1CTF4        IFEQ      'F'
050000931202     C     V1CTF4        OREQ      'A'
050100931202     C     V1CTF4        OREQ      'V'
050200931202     C     V1CTF4        OREQ      'W'
050300931202     C     V1CTF4        OREQ      ' '
050400931202X   2C                   ELSE
050500931202     C                   MOVEL     '1'           $ERR
050600931202     C                   MOVEL     MSG(11)       $MSG
050700931202     C                   EXSR      CARMSG
050800931202     C                   SETON                                        69
050900931202E   2C                   ENDIF
051000931202E   1C                   ENDIF
051100940128     C*
051200960930     C* LIVELLO PUNTO OPERATIVO
051300940128IF  1C     $ERR          IFEQ      '0'
051400940128IF  2C     V1CLIF        IFNE      '1'
051500940128     C     V1CLIF        ANDNE     '2'
051600940128     C     V1CLIF        ANDNE     'T'
051700940128     C                   MOVEL     '1'           $ERR
051800940128     C                   MOVEL     MSG(20)       $MSG
051900940128     C                   EXSR      CARMSG
052000940128     C                   SETON                                        73
052100940128E   2C                   ENDIF
052200940128E   1C                   ENDIF
052300931202     C*
052400931202     C* STAMPA SINTETICA O DETTAGLIATA
052500931202IF  1C     $ERR          IFEQ      '0'
052600931202IF  2C     V1CSOD        IFNE      'S'
052700931202     C     V1CSOD        ANDNE     'D'
052800931202     C                   MOVEL     '1'           $ERR
052900931202     C                   MOVEL     MSG(13)       $MSG
053000931202     C                   EXSR      CARMSG
053100931202     C                   SETON                                        70
053200931202E   2C                   ENDIF
053300931202E   1C                   ENDIF
053400931202     C*
053500931202     C* DATI TECNICI
053600931202IF  1C     $ERR          IFEQ      '0'
053700931202IF  2C     V1CTEC        IFNE      'S'
053800931202     C     V1CTEC        ANDNE     'N'
053900931202     C                   MOVEL     '1'           $ERR
054000931202     C                   MOVEL     MSG(15)       $MSG
054100931202     C                   EXSR      CARMSG
054200931202     C                   SETON                                        71
054300931202E   2C                   ENDIF
054400931202E   1C                   ENDIF
054500931207     C*
054600931207     C* DATI TECNICI/STAMPA SINTETICA O DETTAGLIATA
054700931207IF  1C     $ERR          IFEQ      '0'
054800931207IF  2C     V1CTEC        IFEQ      'S'
054900931207     C     V1CSOD        ANDEQ     'S'
055000931207     C                   MOVEL     '1'           $ERR
055100931207     C                   MOVEL     MSG(18)       $MSG
055200931207     C                   EXSR      CARMSG
055300931207     C                   SETON                                        71
055400931207E   2C                   ENDIF
055500931207E   1C                   ENDIF
055600931202     C*
055700960930     C* MAGAZZINO/PUNTO OPERATIVO
055800931202IF  1C     $ERR          IFEQ      '0'
055900931202IF  2C     V1CMAG        IFGT      *ZEROS
056000931202     C                   MOVE      V1CMAG        ORGFIL
056100020529     C     KEYORG        CHAIN     AZORG21l                           99
056200020529     c                   if        *in99
056300020529     c     keyorg        chain     azorg01l                           99
056400020529     c                   endif
056500100908     c* Se ORGFAG= "Y" come se fosse  "W" (file non in aggiornamento per cui posso sostituire...)
056600100908     c                   if        orgfag='Y'
056700100908     c                   eval      orgfag='W'
056800100908     c                   endif
056900931202     C     ORGFAG        SCAN      V1CTFI                                 99
057000931202IF  3C     *IN99         IFEQ      *OFF
057100931202     C                   MOVEL     '1'           $ERR
057200931202     C                   MOVEL     MSG(14)       $MSG
057300931202     C                   EXSR      CARMSG
057400931202     C                   SETON                                        62
057500931202E   3C                   ENDIF
057600931202E   2C                   ENDIF
057700931202E   1C                   ENDIF
057800950330     C*
057900950330     C* STAMPA TOTALI
058000950330IF  1C     $ERR          IFEQ      '0'
058100950330IF  2C     V1CTOT        IFEQ      'S'
058200950330     C     V1CTOT        OREQ      'N'
058300950330X   2C                   ELSE
058400950330     C                   MOVEL     '1'           $ERR
058500950330     C                   MOVEL     MSG(21)       $MSG
058600950330     C                   EXSR      CARMSG
058700950330     C                   SETON                                        74
058800950330E   2C                   ENDIF
058900950330E   1C                   ENDIF
059000960514     C*
059100960930     C* STAMPA PUNTO OPERATIVO / SEDE
059200960514IF  1C     $ERR          IFEQ      '0'
059300090907IF  2C     V1CFOS        IFEQ      'F'
059400960514     C     V1CFOS        OREQ      'S'
059500020709     c     DutLpo        Ifne      'S'
059600090907     C     V1CFOS        ANDNE     'F'                                          *SOLO PER FILIALE
059700960514     C                   MOVEL     '1'           $ERR
059800960514     C                   MOVEL     MSG(23)       $MSG
059900960514     C                   EXSR      CARMSG
060000960514     C                   SETON                                        75
060100960514E   3C                   ENDIF
060200960514X   2C                   ELSE
060300960514     C                   MOVEL     '1'           $ERR
060400960514     C                   MOVEL     MSG(22)       $MSG
060500960514     C                   EXSR      CARMSG
060600960514     C                   SETON                                        75
060700960514E   2C                   ENDIF
060800960514E   1C                   ENDIF
060900931202     C*
061000931008     C                   ENDSR
061100931201     C*------------------------------------------------------------------------*
061200931201     C*?CHKTIP - CONTROLLA IL TIPO DI UNITA'
061300931201     C*           INPUT:  WTIP  - VALORE IMMESSO A VIDEO
061400931201     C*           OUTPUT  WDTIP - DECODIFICA TIPO UNITA'
061500931201     C*------------------------------------------------------------------------*
061600931201     C     CHKTIP        BEGSR
061700020322
061800020322      * CONTROLLO SE E' STATO INSERITO '?'
061900020322     C     '?'           SCAN      WTIP                                   99
062000020322
062100020322      * E' STATO INSERITO: LANCIO IL PGM DI INTERROGAZIONE TABELLE
062200020322     c                   if        *in99                                        --- 1 -->
062300020322     c                   clear                   TIBS02DS
062400020322     c                   movel     'R'           t02mod
062500020322     c                   movel     knsif         t02sif
062600020322     c                   movel     'UNT'         t02cod
062700020322     c                   call      'TIBS02R'
062800020322     c                   parm                    KPJBA
062900020322     c                   parm                    TIBS02DS
063000020322     c                   movel     T02ke1        wtip
063100020322     c                   movel     t02uni        wdtip
063200020322     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
063300020322     c                   endif                                                  <-- 1 ---
063400020322
063500020322      * NON E' STATO INSERITO: CONTROLLO CHE ESISTA IN TABELLA
063600020322     c                   clear                   TIBS02DS
063700020322     c                   movel     'C'           T02mod
063800020322     c                   movel     knsif         t02sif
063900020322     c                   movel     'UNT'         t02cod
064000020322     c                   movel(p)  wtip          T02ke1
064100020322     c                   call      'TIBS02R'
064200020322     c                   parm                    KPJBA
064300020322     c                   parm                    TIBS02DS
064400020322     c                   if        t02err <> *blanks
064500020322     c                   eval      $err = '1'
064600020322     c                   clear                   wdtip
064700020322     c                   else
064800020322     c                   movel     t02uni        wdtip
064900020322     c                   endif
065000931201     C*
065100931201     C                   ENDSR
065200931201     C*------------------------------------------------------------------------*
065300931201     C*?CHKMAG - CONTROLLA IL MAGAZZINO UBICAZIONE
065400931202     C*           INPUT:  WMAG  - VALORE IMMESSO A VIDEO
065500931202     C*           OUTPUT  WDMAG - DECODIFICA MAGAZZINO UBIACAZIONE
065600931201     C*------------------------------------------------------------------------*
065700931201     C     CHKMAG        BEGSR
065800931201     C*
065900931201     C* CONTROLLO SE E' STATO INSERITO '?'
066000931201     C     '?'           SCAN      WMAG          A                 1 0    99
066100931201     C*
066200931202     C* E' STATO INSERITO; LANCIO IL PGM DI INTERROGAZIONE LINEE ARRIVO
066300931202IF  1C     *IN99         IFEQ      '1'
066400931202     C                   MOVEL     *BLANKS       §COD1
066500931201     C                   MOVE      *BLANKS       §DES1
066600941115     C                   CALL      'TNSD20R'
066700931202     C                   PARM                    §COD1             3
066800931201     C                   PARM                    §TIP              1
066900931201     C                   PARM                    §DES1            25
067000931202     C*
067100931202     C                   MOVEL     §COD1         WMAG
067200931202     C                   MOVEL     §DES1         WDMAG
067300931202     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA
067400931202     C*
067500931202     C* NON E' STATO INSERITO: CONTROLLO CHE ESISTA IN AZORG
067600931202X   1C                   ELSE
067700931201     C                   MOVE      WMAG          ORGFIL
067800020529     C     KEYORG        CHAIN     AZORG21l                           99
067900931202IF  2C     *IN99         IFEQ      '0'
068000931202     C     ORGFVA        ANDEQ     *BLANKS
068100931201     C                   MOVEL     ORGFIL        WMAG
068200931201     C                   MOVEL     ORGDES        WDMAG
068300931202X   2C                   ELSE
068400020529     c     keyorg        chain     azorg01l
068500020529     c                   if        %found(azorg01l) and orgfva = *blanks
068600020529     c                   movel     orgfil        wmag
068700020529     c                   movel     orgdes        wdmag
068800020529     c                   else
068900931201     C                   MOVEL     '1'           $ERR
069000931201     C                   MOVEL     *ALL'*'       WDMAG
069100020529     c                   endif
069200931202E   2C                   ENDIF
069300931202E   1C                   ENDIF
069400931201     C*
069500931201     C                   ENDSR
069600020424      *------------------------------------------------------------------------*
069700020424      *?CTRUTE - ROUTINE CONTROLLO UTENTE
069800020424      *------------------------------------------------------------------------*
069900020424     c     ctrute        begsr
070000020424
070100020424     c                   eval      *in76 = *off
070200020424
070300020424      * controllo se il profilo è abilitato
070400020424     c                   if        duterr = 'E'
070500020424     c                   eval      $msg = dutmsg
070600020424     c                   eval      *in76 = *on
070700020424     c                   exsr      carmsg
070800020424     c                   endif
070900020424      * controllo le abilitazionei del profilo
071000020510     c                   if        *in76 = *off
071100020424     c                   movel     utefaf        dute01
071200020424     c                   if        §uteiun <> *blanks
071300020424     c                   eval      wabi = §uteiun
071400020424     c                   else
071500020424     c                   eval      wabi = uteaut
071600020424     c                   endif
071700020424     c                   clear                   tibs02ds
071800020424     c                   movel     'C'           t02mod
071900020424     c                   movel     knsif         t02sif
072000020424     c                   movel     'LAT'         t02cod
072100020424     c                   movel(p)  wabi          t02ke1
072200020424     c                   call      'TIBS02R'
072300020424     c                   parm                    kpjba
072400020424     c                   parm                    tibs02ds
072500020424     c                   movel     t02uni        dlat
072600020424     c                   if        §latabi = 'S'
072700020424     c                   eval      $msg = msg(24)
072800020424     c                   exsr      carmsg
072900020424     c                   eval      *in76 = *on
073000020424     c                   endif
073100020424     c                   endif
073200020510
073300020510      * Utente ok richiamo pgm che ritorna i p.o. abilitati
073400020523     c                   if        *in76 = *off  and wabi <> 'AZ'
073500020516     c                   clear                   trul31ds
073600020516     c                   eval      i31abi = wabi
073700020516     c                   eval      i31cdi = dutdis
073800020516     c                   eval      i31car = dutare
073900020516     c                   eval      i31cpo = dutpou
074000020516     c                   call      'TRUL31R'
074100020516     c                   parm                    kpjba
074200020516     c                   parm                    trul31ds
074300020510     c                   endif
074400090904
074500090904      /free
074600090904
074700090904         clear  skPOabi;
074800090904         clear  ii;
074900090904
075000090904         SELECT;
075100090904
075200090904           // - Reperimento filiali gestibili per utente della LOGISTICA
075300090904           WHEN  DUTntw = c_Logistica;
075400090904
075500090904             setll  *loval  AZORG;
075600090904             read   AZORG;
075700090904
075800090904             DoW  Not  %eof(AZORG01L);
075900090904
076000090904               if  ORGfva = *blank;
076100090904
076200090904                 Og143 = ORGde3;
076300090904                 if  §OgNtw = DUTntw;
076400090904
076500090904                   ii += 1;
076600090904                   skPOabi(ii) = ORGfil;
076700090904
076800090904                 endif;
076900090904
077000090904               endif;
077100090904
077200090904               read   AZORG;
077300090904
077400090904             EndDo;
077500090904
077600090904      /end-free
077700020517
077800020529      * per abilitazione 'AZ' devo caricare tutti i p.o. di azorg21l
077900090904     c                   WHEN      wabi = 'AZ'
078000020529     c     *loval        setll     azorg21l
078100020517     c                   do        *hival
078200020529     c                   read      azorg21l
078300020517     c                   if        %eof
078400020517     c                   leave
078500020517     c                   endif
078600020517     c                   z-add     1             ii
078700020523     c     orgfil        lookup    skpoabi(ii)                            99
078800020517     c                   if        not *in99
078900020517     c                   z-add     1             ii
079000020517     c     *zeros        lookup    skpoabi(ii)                            99
079100020517     c                   if        *in99
079200020517     c                   movel     orgfil        skpoabi(ii)
079300020517     c                   endif
079400020517     c                   endif
079500020517     c                   enddo
079600020529      * e tutti i p.o. di azorg01l
079700020529     c     *loval        setll     azorg01l
079800020529     c                   do        *hival
079900020529     c                   read      azorg01l
080000020529     c                   if        %eof
080100020529     c                   leave
080200020529     c                   endif
080300020529     c                   z-add     1             ii
080400020529     c     orgfil        lookup    skpoabi(ii)                            99
080500020529     c                   if        not *in99
080600020529     c                   z-add     1             ii
080700020529     c     *zeros        lookup    skpoabi(ii)                            99
080800020529     c                   if        *in99
080900020529     c                   movel     orgfil        skpoabi(ii)
081000020529     c                   endif
081100020529     c                   endif
081200020529     c                   enddo
081300090904      *
081400020523      * se non è abilitazione 'AZ' passo la sk tornata dal TRUL31 nella sk di 500 elementi
081500090904     c                   OTHER
081600020523     c                   movea     skpo          skpoabi
081700020523     c                   z-add     1             ii
081800020523     c     951           lookup    skpoabi(ii)                            99
081900020523     c                   if        not *in99
082000020523     c                   z-add     1             ii
082100020523     c     *zeros        lookup    skpoabi(ii)                            99
082200020523     c                   if        *in99
082300021014     c                   movel     951           skpoabi(ii)
082400020523     c                   endif
082500020523     c                   endif
082600090904      *
082700090904     c                   ENDSL
082800020424
082900020424     c                   endsr
083000931201     C*------------------------------------------------------------------------*
083100931201     C*?*INZSR - ROUTINE INIZIALE
083200931201     C*------------------------------------------------------------------------*
083300930713     C     *INZSR        BEGSR
083400000000     C*
083500000000     C* RICEVIMENTO PARAMETRI
083600000000     C     *ENTRY        PLIST
083700930922     C                   PARM                    KPJBA
083800940105     C                   MOVEL     KPJBU         DSUNIT
083900020423
084000020423     c     *dtaara       define    §azute        azuteds
084100020423     c     *dtaara       define    §datiute      ddatiute
084200020423
084300020423     c                   in(E)     *dtaara
084400020424     c                   if        %Error or rsut = *blanks
084500020424     c                   clear                   tibs34ds
084600020423     c                   call      'TIBS34R'
084700020423     c                   parm                    Tibs34Ds
084800020423     c                   in        *dtaara
084900020424     c                   endif
085000020423
085100930310     C*
085200020529     C* CHIAVE LETTURA AZORG01L/azorg21l - COMPLETA
085300930310     C     KEYORG        KLIST
085400930712     C                   KFLD                    ORGFIL
085500930714     C*
085600930714     C* ESEGUE OPERAZIONI INIZIALI STANDARD
085700930921     C                   EXSR      INZSTD
085800931008     C*
085900931008     C* IMPOSTO CAMPI VIDEO RICHIESTA PARAMETRI
086000020709     c                   If        DutLpo = 'S'
086100931202     C                   Z-ADD     1             V1CORD                         *ORDINAMENTO
086200020326X   1C                   ELSE
086300020326     C                   Z-ADD     3             V1CORD                         *ORDINAMENTO
086400020326E   1C                   ENDIF
086500960930     C                   MOVEL     'F'           V1CTF1                         *TIPO PUNTO OPERATIV
086600931202     C                   MOVEL     'A'           V1CTF2                         *TIPO AGENZIA
086700931202     C                   MOVEL     'V'           V1CTF3                         *TIPO VARIE
086800931202     C                   MOVEL     'W'           V1CTF4                         *TIPO VARIE UNITA'
086900960930     C                   MOVEL     'T'           V1CLIF                         *LIVELLO P.OPERATIVI
087000020326     C                   MOVEL     'D'           V1CSOD                         *SINTETICA/DETTAGLIO
087100931202     C                   MOVEL     'N'           V1CTEC                         *DATI TECNICI
087200950330     C                   MOVEL     'N'           V1CTOT                         *STAMPA TOTALI
087300020709     c                   If        DutLpo = 'S'
087400960514     C                   MOVEL     'S'           V1CFOS                         *STAMPA PER SEDE
087500960514X   1C                   ELSE
087600090907     C                   MOVEL     'F'           V1CFOS                         *STAMPA PER FIL.
087700960514E   1C                   ENDIF
087800020328
087900930922     C*
088000930924     C* EMETTO FORMATO VIDEO
088100930924     C                   EXSR      CARV1                                        *EMISSIONE VIDEO
088200930923     C*
088300000000     C                   ENDSR
088400931201     C*------------------------------------------------------------------------*
088500931202     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
088600931202     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
088700931202     C*     LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
088800931202     C*     STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
088900931202     C*     NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
089000931202     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
089100931202     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
089200931201     C*------------------------------------------------------------------------*
089300930923     C* INZSTD - ROUTINE INIZIALE STANDARD
089400931201     C*------------------------------------------------------------------------*
089500930803     C     INZSTD        BEGSR
089600930714     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
089700930802     C                   CALL      'XKEYS'
089800930802     C                   PARM                    DSKEYS
089900930805     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
090000930714     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
090100930714     C     XMSGPA        PLIST
090200930714     C                   PARM                    DSMSMS
090300930714     C                   PARM                    DSMSPG
090400930714     C                   PARM                    DSMSKE
090500930714     C                   PARM                    DSMSER
090600930714     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
090700941115     C                   MOVEL     'TNSDU6R'     DSMSPG                         *NOME PGM CHIAMANTE
090800941115     C                   MOVEL     'TNSDU6R'     VMCQUE                         *CODA MESSAGGI DESTI
090900930806     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
091000930714     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
091100930805     C* INIZIALIZZO FLAG CONTROLLO PGM
091200930721     C                   MOVEL     '0'           $ERR              1            *ERRORE
091300930721     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
091400930806     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
091500930721     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
091600930721     C                   MOVEL     '0'           $FINE             1            *FINE PGM
091700930922     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
091800930923     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
091900930805     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
092000930721     C                   BITOFF    '01234567'    POSMI1            1
092100930721     C                   BITOFF    '01234567'    POSMI2            1
092200930721     C                   BITOFF    '01234567'    POSMA1            1
092300930721     C                   BITOFF    '01234567'    POSMA2            1
092400930721     C                   BITON     '34'          POSMI1            1            *18
092500930721     C                   BITON     '7'           POSMI2            1            *01
092600930721     C                   BITON     '34'          POSMA1            1            *18
092700930721     C                   BITON     '0'           POSMA2            1            *80
092800930721     C                   MOVE      POSMI2        POSMIN            2
092900930721     C                   MOVEL     POSMI1        POSMIN
093000930721     C                   MOVE      POSMA2        POSMAX            2
093100930721     C                   MOVEL     POSMA1        POSMAX
093200930714     C                   ENDSR
093300931201     C*------------------------------------------------------------------------*
093400930923     C* FUNSTD - LANCIA SUBROUTINE STANDARD
093500931201     C*------------------------------------------------------------------------*
093600930804     C     FUNSTD        BEGSR
093700930804     C                   SELECT
093800930804     C     KEY           WHENEQ    F3
093900930804     C                   EXSR      F03STD
094000931115     C     KEY           WHENEQ    F12
094100931115     C                   EXSR      F12STD
094200930804     C     KEY           WHENEQ    F24
094300930804     C                   EXSR      F24STD
094400930804     C                   OTHER
094500930804     C                   MOVEL     MSG(2)        $MSG
094600930804     C                   EXSR      CARMSG
094700930804     C                   ENDSL
094800930804     C                   ENDSR
094900931201     C*------------------------------------------------------------------------*
095000930923     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
095100931201     C*------------------------------------------------------------------------*
095200930804     C     F03STD        BEGSR
095300930923     C*
095400940105     C* IMPOSTO FLAG DI PREMUTO TASTO F03
095500940105     C                   MOVEL     '1'           $F03
095600940105     C*
095700930804     C* IMPOSTO FLAG DI FINE PROGRAMMA
095800930927     C                   MOVEL     '1'           $FINE
095900930927     C*
096000930927     C                   ENDSR
096100931201     C*------------------------------------------------------------------------*
096200931115     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
096300931201     C*------------------------------------------------------------------------*
096400931115     C     F12STD        BEGSR
096500931115     C*
096600931115     C* IMPOSTO FLAG DI FINE PROGRAMMA
096700931115     C                   MOVEL     '1'           $FINE
096800931115     C*
096900931115     C                   ENDSR
097000931201     C*------------------------------------------------------------------------*
097100930923     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
097200931201     C*------------------------------------------------------------------------*
097300930804     C     F24STD        BEGSR
097400930923     C*
097500930804     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
097600930804     C                   EXSR      OUTFUN
097700930923     C*
097800930804     C                   ENDSR
097900931201     C*------------------------------------------------------------------------*
098000930923     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
098100930721     C*          CODA MESSAGGI DEL PGM
098200931201     C*------------------------------------------------------------------------*
098300930721     C     CARMSG        BEGSR
098400930723     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
098500930723     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
098600930721     C     MSGULT        IFEQ      0
098700930721     C                   MOVEL     '1'           *IN40
098800941115     C                   WRITE     SDU6DVK
098900930721     C                   MOVEL     '0'           *IN40
099000930805     C                   ENDIF
099100930721     C* INVIA MESSAGGIO ALLA CODA DEL PGM
099200930721     C                   MOVEL     $MSG          DSMSMS
099300930721     C                   CALL      'XMSG'        XMSGPA
099400930721     C     DSMSER        IFEQ      0
099500930806     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
099600930805     C                   ENDIF
099700930723     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
099800930923     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
099900941115     C                   WRITE     SDU6DVM                                      *SFL MESS
100000930723     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
100100930721     C                   MOVEL     *BLANKS       $MSG
100200930721     C                   ENDSR
100300931201     C*------------------------------------------------------------------------*
100400930923     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
100500931201     C*------------------------------------------------------------------------*
100600930721     C     OUTMSG        BEGSR
100700930721     C* VISUALIZZAZIONE MESSAGGIO
100800930721     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
100900941115     C                   WRITE     SDU6DVK                                      *CONTR MESS
101000930721     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
101100930721     C                   Z-ADD     *ZEROS        MSGULT
101200930721     C                   ENDSR
101300931201     C*------------------------------------------------------------------------*
101400930923     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
101500930803     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
101600930803     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
101700930803     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
101800930803     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
101900931201     C*------------------------------------------------------------------------*
102000930803     C     CARFUN        BEGSR
102100930803     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
102200930923     C     1             DO        31            I                 5 0          --- 1 -->
102300930803     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
102400930805     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
102500930805     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
102600020709     C**!!!DSFITI        IFEQ      'S'                                          --- 2 --> *SEDE
102700020709     c                   If        DutLpo = 'S'
102800930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
102900930805     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
103000930803     C                   ELSE                                                   *INTERROG.
103100930803     C                   MOVEL     FUNASI        FAT(I)
103200930803     C                   ENDIF
103300930803     C                   ELSE                                                   <-- 2 --> *FILIALE
103400930923     C     $TGES         IFEQ      'G'                                          *GESTIONE
103500930803     C                   MOVEL     FUNAFG        FAT(I)
103600930803     C                   ELSE                                                   *INTERROG.
103700930922     C                   MOVEL     FUNAFI        FAT(I)
103800930803     C                   ENDIF
103900930803     C                   ENDIF                                                  <-- 2 ---
104000930806     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
104100930806     C                   MOVEL     USER          WEDP              3            *PROFILO
104200930806     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
104300930806     C                   MOVEL     'S'           FAT(I)                         *ABILITO
104400930806     C                   ELSE
104500930806     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
104600930806     C                   ENDIF
104700930806     C                   ENDIF
104800930803     C                   ENDDO                                                  <-- 1 ---
104900930803     C                   ENDSR
105000931201     C*------------------------------------------------------------------------*
105100930923     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
105200930803     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
105300930806     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
105400930803     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
105500931201     C*------------------------------------------------------------------------*
105600930803     C     OUTFUN        BEGSR
105700930805     C     1             DO        8             I                              *PULIZIA FUNZIONI A
105800930805     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
105900930803     C                   ENDDO
106000930803     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
106100930803     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
106200930803     C     1             DO        31            I
106300930803     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
106400930803     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
106500930803     C                   ADD       1             FUNNAT
106600930803     C                   ENDIF
106700930803     C                   ENDDO
106800930805     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
106900930803     C     FUNNAT        IFGT      8
107000930803     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
107100930803     C                   ENDIF
107200930803     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
107300930805     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
107400930803     C                   Z-ADD     8             FUNVIS            5 0
107500930803     C     FAT(23)       IFEQ      'S'
107600930803     C                   SUB       1             FUNVIS
107700930803     C                   ENDIF
107800930803     C     FAT(24)       IFEQ      'S'
107900930803     C                   SUB       1             FUNVIS
108000930803     C                   ENDIF
108100930805     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
108200930923     C                   Z-ADD     *ZEROS        W                 5 0
108300930803     C                   ADD       1             FUNULT
108400930803     C     FUNULT        IFGT      31
108500930803     C                   Z-ADD     1             FUNULT
108600930805     C                   ENDIF
108700930805     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
108800930803     C     FUNULT        DO        31            I                              --- 1 -->
108900930803     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
109000930803     C     W             IFLT      FUNVIS                                       --- 2 -->
109100930803     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
109200930803     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
109300930803     C     FDE(I)        ANDNE     *BLANKS
109400930805     C     I             IFEQ      23                                           *TOLGO F23
109500930805     C     I             OREQ      24                                           *E F24
109600930803     C                   ELSE
109700930803     C                   ADD       1             W
109800930803     C                   MOVEL     FDE(I)        FUV(W)
109900930803     C                   ENDIF
110000930803     C                   ENDIF                                                  <-- 3 ---
110100930803     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
110200930803     C                   Z-ADD     I             FUNULT
110300930803     C                   ENDIF                                                  <-- 2 ---
110400930803     C                   ENDDO                                                  <-- 1 ---
110500930803     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
110600930806     C                   MOVEL     FUV(1)        VFD001
110700930806     C                   MOVEL     FUV(2)        VFD002
110800930806     C                   MOVEL     FUV(3)        VFD003
110900930806     C                   MOVEL     FUV(4)        VFD004
111000930806     C                   MOVEL     FUV(5)        VFD005
111100930806     C                   MOVEL     FUV(6)        VFD006
111200930803     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
111300930803     C     FAT(24)       ANDEQ     'N'
111400930806     C                   MOVEL     FUV(7)        VFD007
111500930806     C                   MOVEL     FUV(8)        VFD008
111600930803     C                   ELSE                                                   <-- 1 -->
111700930803     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
111800930803     C     FAT(24)       ANDEQ     'S'
111900930806     C                   MOVEL     FDE(23)       VFD007
112000930806     C                   MOVEL     FDE(24)       VFD008
112100930803     C                   ELSE                                                   <-- 2 -->
112200930806     C                   MOVEL     FUV(7)        VFD007
112300930803     C     FAT(23)       IFEQ      'S'
112400930806     C                   MOVEL     FDE(23)       VFD008
112500930803     C                   ELSE
112600930806     C                   MOVEL     FDE(24)       VFD008
112700930805     C                   ENDIF
112800930805     C                   ENDIF                                                  <-- 2 ---
112900930805     C                   ENDIF                                                  <-- 1 ---
113000930922     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
113100930803     C                   ENDSR
113200931201     C*------------------------------------------------------------------------*
113300930923     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
113400930803     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
113500930803     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
113600930803     C*                   - ERRORE SE FUNZIONE NON ATTIVA
113700931201     C*------------------------------------------------------------------------*
113800930803     C     CONFUN        BEGSR
113900930803     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
114000930803     C     1             DO        31            I                              --- 1 -->
114100930803     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
114200930805     C* CONTROLLO SE IL TASTO E' ATTIVO
114300930803     C     FAT(I)        IFNE      'S'                                          --- 3 -->
114400930804     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
114500930803     C                   EXSR      CARMSG
114600930803     C                   MOVEL     '1'           $ERR
114700930803     C                   ENDIF                                                  <-- 3 ---
114800930803     C                   ENDIF                                                  <-- 2 ---
114900930803     C                   ENDDO                                                  <-- 1 ---
115000930803     C                   ENDSR
115100931201     O*------------------------------------------------------------------------*
115200930802**       MSG - MESSAGGI
115300941115TNSDU6R- Tasto funzionale non valido.                                          1
115400941115TNSDU6R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
115500941115TNSDU6R- Opzione non valida.                                                   3
115600941115TNSDU6R- Opzione abilitata ma non gestita. Avvertire CED.                      4
115700941115TNSDU6R- Inizio elenco.                                                        5
115800941115TNSDU6R- Fine elenco.                                                          6
115900941115TNSDU6R- Scegliere uno degli ordinamenti possibili                             7
116000941115TNSDU6R- Tipo unità errato                                                     8
116100941115TNSDU6R- Magazzino errato                                                      9
116200941115TNSDU6R- Data scadenza manutenzione errata                                    10
116300090907TNSDU6R- Tipo filiale errato                                                  11
116400090907TNSDU6R- Richiedere almeno un tipo filiale                                    12
116500941115TNSDU6R- 'S' per richiedere la stampa sintetica, 'D' per quella dettagliata   13
116600090907TNSDU6R- Il magazzino ubicazione non fa parte del tipo filiale selezionato    14
116700941115TNSDU6R- 'S' per stampare i dati tecnici, 'N' per non stamparli               15
116800941115TNSDU6R- Data AL inferiore a data DAL                                         16
116900941115TNSDU6R- Immettere la data AL solo se immessa la data DAL                     17
117000941115TNSDU6R- Se richiesta stampa sintetica i dati tecnici NON COMPARIRANNO MAI    18
117100941115TNSDU6R- Stampa lanciata ...                                                  19
117200941115TNSDU6R- Immettere 1 per stampare solo i 1° liv, 2 per i 2° liv, T per tutti  20
117300950330TNSDU6R- Indicare "S" per stampare i totali, "N" per non stamparli            21
117400090907TNSDU6R- Immettere P per stampare l'elenco per filiale, S per la sede         22
117500960514TNSDU6R- Impossibile richiedere la stampa per la sede                         23
117600020424TNSDU6R- Profilo non abilitato alla funzione                                  24
117700070202TNSDU6R- Profilo abilitato solo alla stampa con ordinamento di tipo "3" o "2" 25
117800020510TNSDU6R- Profilo non abilitato al magazzino richiesto                         26
117900020513TNSDU6R- Immettere il magazzino                                               27
118000940105** ======== SCHIERA: CMD  (COMANDI)                        ================
118100940105WRKSPLF
118200940105
118300940105
118400940105
118500940105
118600930806** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
118700930806F1    -                   -    NN     NN
118800930723F2    -                   -    NN     NN
118900930921F3    -F3=Fine            -    SS     SS
119000930723F4    -                   -    NN     NN
119100930723F5    -                   -    NN     NN
119200931026F6    -F6=Lancio          -    SS     SS
119300930723F7    -                   -    NN     NN
119400930723F8    -                   -    NN     NN
119500930723F9    -                   -    NN     NN
119600930723F10   -                   -    NN     NN
119700930723F11   -                   -    NN     NN
119800931115F12   -F12-Ritorno        -    SS     SS
119900930723F13   -                   -    NN     NN
120000930723F14   -                   -    NN     NN
120100930723F15   -                   -    NN     NN
120200930723F16   -                   -    NN     NN
120300930723F17   -                   -    NN     NN
120400930723F18   -                   -    NN     NN
120500930723F19   -                   -    NN     NN
120600930723F20   -                   -    NN     NN
120700930804F21   -                   -    NN     NN
120800941215F22   -F22=Gestione stampe-    SS     SS
120900930723F23   -                   -    NN     NN
121000930803F24   -F24=Altri tasti    -    NN     NN
121100930921ENTER -                   -    SS     SS
121200930802ROLLUP-                   -    NN     NN
121300930802ROLLDN-                   -    NN     NN
121400930802HELP  -                   -    NN     NN
121500930802PRINT -                   -    NN     NN
121600930802BSPACE-                   -    NN     NN
121700930802CLEAR -                   -    NN     NN
