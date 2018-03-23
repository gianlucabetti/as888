000100000000     H*------------------------------------------------------------------------*
000200991029     H* STATISTICA VOLUMI - PARAMETRI
000300080318     H* N.B questo pgm è stato copiato e opportunamente modificato da TIST18R
000400000000     H*------------------------------------------------------------------------*
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600000000     F*------------------------------------------------------------------------*
000700080320     F* DATA BASE
000800000000     F*------------------------------------------------------------------------*
000900080318     Ffnlsg1d   CF   E             WORKSTN INFDS(DEVDS1)
001000991229     F                                     SFILE(ST18DMS:MSGULT)
001100000000     FTABEL00F  IF   E           K DISK
001200000000     FAZORG01L  IF   E           K DISK
001300000000     D*------------------------------------------------------------------------*
001400000000     D* SCHIERE
001500000000     D*------------------------------------------------------------------------*
001600000000     D*-------------------
001700000000     D* MESSAGGI
001800000000     D*-------------------
001900090710     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
002000000000     D*-------------------
002100000000     D* COMANDI
002200000000     D*-------------------
002300000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002400000000     D*-------------------
002500000000     D* DESCRIZIONI TASTI FUNZIONALI
002600000000     D*-------------------
002700000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002800000000     D*-------------------
002900000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
003000000000     D*-------------------
003100000000     D FUV             S             19    DIM(24)
003200000000     D*------------------------------------------------------------------------*
003300000000     D* INPUT
003400000000     D*------------------------------------------------------------------------*
003500000000     D*-------------------
003600000000     D* RIDEFINIZIONE SCHIERA -FUD-
003700000000     D*-------------------
003800000000     D FUDDS           DS
003900000000     D*                -DESCRIZIONE PARZIALE
004000000000     D  FUDDE1                 1      1
004100000000     D*                -FUNZIONE
004200000000     D  FUDFUN                 2      3
004300000000     D*                -DESCRIZIONE PARZIALE
004400000000     D  FUDDE2                 4     19
004500000000     D*                -FILLER
004600000000     D  FUDFI0                20     34
004700000000     D*                -FLAG FUNZ.ATTIVA (S/N)
004800000000     D  FUDATT                35     35
004900000000     D*                -FILLER
005000000000     D  FUDFI1                36     80
005100000000     D*                -DESCRIZIONE COMPLETA
005200000000     D  FUDDES                 1     19
005300000000     D*                -RIDEFINIZIONE RIGA
005400000000     D  FUDRIG                 1     80
005500000000     D*-------------------
005600000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
005700000000     D*-------------------
005800000000     D DSMSG         E DS
005900000000     D*-------------------
006000020917     D* REPERIMENTO DATI UTENTE
006100020917     D*-------------------
006200020917     D TIBS34DS      E DS
006300020917     D DDATIUTE      E DS
006400020917     D AZUTEDS       E DS                  extname(AZUTE00F)
006500991019     D*-------------------
006600991019     D* DS PER REPERIMENTO PARAMETRI DI DFT
006700991019     D*-------------------
006800991019     D DSTP          E DS
006900080318
007000080318     d ds5a          e ds
007100000000     D*-------------------
007200000000     D* DS PARAMETRI DI LANCIO PGM
007300000000     D*-------------------
007400991103     D PARAM           DS                  INZ
007500000000     D  PARTLA                 1      1
007600080318     D**PARANN                 2      5  0
007700080318     D**PARMMI                 6      7  0
007800080318     D**PARMMF                 8      9  0
007900080318     d  pardal                 2      9  0
008000080318     D**PARSNR                10     10
008100991029     D  PARPID                27     31  2
008200080318     D**PARDIV                32     32
008300080318     D**PARARE                33     35  0
008400080318     D**PARFIL                36     38  0
008500080318     D  PARtfp                36     38  0
008600000000     D  PARDET                39     39
008700080318     D**PARTOT                40     40
008800991026     D  PARRG1                41     44  1
008900991026     D  PARRG2                45     48  1
009000080318     D**PARPIA                49     53  2
009100991029     D  PARPSD                54     58  2
009200080318     D**PARPSA                59     63  2
009300080318     D**PAROK                 65     66
009400991229     D  PARMIN                67     71  3
009500080318     D**PARPOS                72     72
009600080318     D**PARNT1                73     75
009700080318     D**PARNT2                76     78
009800080318     D**PARNT3                79     81
009900080318     D**PARNT4                82     84
010000080318     D**PARSAL                85     85
010100080318     d  paral                 86     93  0
010200090513     d  parord                75     75
010300090709     d  parpag                76     76
010400090709     d  parflp                77     79  0
010500000000     D*-------------------
010600000000     D* DS PER GESTIONE TASTI FUNZIONALI
010700000000     D*-------------------
010800000000     D DSKEY         E DS
010900000000     D DEVDS1          DS
011000000000     D  KEY                  369    369
011100000000     D*-------------------
011200000000     D* ARCHITETTURA
011300000000     D*-------------------
011400000000     D KPJBA         E DS
011500080318
011600080318     D WLBDAT          DS
011700080318     D  G02DAT                 1      8  0
011800080318     D  G02INV                 9     16  0
011900080318     D  G02ERR                17     17
012000080318     D  G02TGI                18     22  0
012100080318
012200080318     D WGIDAT          DS
012300080318     D  GIODAT                 1      8  0
012400080318     D  GIOINV                 9     16  0
012500080318     D  GIOTGI                17     21  0
012600080320     d fnlv55ds      e ds
012700080320     d og150         e ds
012800080320     d dataiso         s               d   datfmt(*iso)
012900080320     d dataeur         s               d   datfmt(*eur)
013000090709     d wtip            s              1
013100000000     C*------------------------------------------------------------------------*
013200000000     C* MAIN LINE
013300000000     C*------------------------------------------------------------------------*
013400000000     C*
013500000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
013600000000     C     ERRL00        IFEQ      '0'
013700000000     C*
013800000000     C* CICLO FINO A CHE:
013900000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
014000000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
014100000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
014200000000     C     F03L00        DOUEQ     '1'
014300000000     C     F12L00        OREQ      '1'
014400000000     C     FLFINE        OREQ      '1'
014500000000     C*
014600000000     C* EMETTE FORMATO VIDEO
014700000000     C                   EXSR      BUTFMR
014800000000     C*
014900000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
015000000000     C                   EXSR      FUNCON
015100000000     C*
015200000000     C                   END
015300000000     C*
015400000000     C                   END
015500000000     C*
015600000000     C                   SETON                                        LR
015700000000     C*------------------------------------------------------------------------*
015800000000     C* EMISSIONE FORMATO A VIDEO
015900000000     C*------------------------------------------------------------------------*
016000000000     C     BUTFMR        BEGSR
016100000000     C*
016200000000     C* WRITE RECORD TESTATA
016300991229     C                   WRITE     ST18D01
016400000000     C*
016500000000     C* VISUALIZZAZIONE MESSAGGIO
016600000000     C                   EXSR      SNDMSG
016700000000     C*
016800000000     C* WRITE RECORD FUNZIONI
016900991229     C                   WRITE     ST18D02
017000991019     C*
017100991019     C* EXFMT RECORD DETTAGLIO
017200991229     C                   EXFMT     ST18D03
017300991019     C*
017400000000     C* AZZERO FLAG DI ERRORE LIVELLO
017500000000     C                   MOVEL     '0'           ERRL00
017600000000     C*
017700000000     C* AZZERO FLAG DI ERRORE
017800991025     C                   SETOFF                                         7071
017900000000     C                   SETOFF                                       727377
018000050105     C                   SETOFF                                           76
018100000000     C                   SETOFF                                       787983
018200000000     C                   SETOFF                                       848586
018300950703     C                   SETOFF                                       818082
018400030910     C                   SETOFF                                       919293
018500030910     C                   SETOFF                                       94
018600000000     C*
018700000000     C                   ENDSR
018800000000     C*------------------------------------------------------------------------*
018900000000     C* SNDMSG - MANDA MESSAGGIO
019000000000     C*------------------------------------------------------------------------*
019100000000     C     SNDMSG        BEGSR
019200000000     C*
019300000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
019400000000     C                   MOVEL     '1'           *IN40
019500991229     C                   WRITE     ST18DMC
019600000000     C                   MOVEL     '0'           *IN40
019700000000     C*
019800000000     C* VISUALIZZAZIONE MESSAGGIO
019900000000     C                   Z-ADD     0             MSGULT            4 0
020000000000     C                   CALL      'XMSG'        XMSGPA
020100000000     C     DSMSER        IFEQ      0
020200000000     C                   MOVEL     DSMSKE        MSGKEY
020300000000     C                   END
020400000000     C                   Z-ADD     1             MSGULT
020500991229     C                   WRITE     ST18DMS
020600991229     C                   WRITE     ST18DMC
020700000000     C*
020800000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
020900000000     C                   MOVEL     *BLANKS       DSMSMS
021000000000     C*
021100000000     C                   ENDSR
021200000000     C*------------------------------------------------------------------------*
021300000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
021400000000     C*------------------------------------------------------------------------*
021500000000     C     ENTGES        BEGSR
021600000000     C*
021700000000     C* CONTROLLA I CAMPI IMMESSI
021800000000     C                   EXSR      DECODI
021900000000     C*
022000000000     C                   ENDSR
022100000000     C*------------------------------------------------------------------------*
022200000000     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
022300000000     C*------------------------------------------------------------------------*
022400000000     C     F01GES        BEGSR
022500000000     C                   ENDSR
022600000000     C*------------------------------------------------------------------------*
022700000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
022800000000     C*------------------------------------------------------------------------*
022900000000     C     F03GES        BEGSR
023000000000     C*
023100000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
023200000000     C                   MOVEL     '1'           F03L00
023300000000     C*
023400000000     C                   ENDSR
023500000000     C*------------------------------------------------------------------------*
023600000000     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
023700000000     C*------------------------------------------------------------------------*
023800991020     C     F06GES        BEGSR
023900991105     C*
024000991105     C                   EXSR      DECODI
024100991105     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
024200991105     C     ERRL00        IFEQ      '0'
024300991104     C*
024400080318     C* LANCIO IL PROGRAMMA DI STAMPA
024500080318     C                   MOVEL     'LSG2'        KCOAZ
024600991104     C                   EXSR      VIDPAR
024700991104     C                   MOVEL     PARAM         KPJBU
024800030910     C                   CALL      'BCH10'
024900080318 MOM C*                  CALL      'FNLSG2C'
025000991104     C                   PARM                    KPJBA
025100080319     c                   eval      flfine='1'
025200991104     C*
025300991105     C*
025400991105     C                   ENDIF
025500000000     C*
025600000000     C                   ENDSR
025700000000     C*------------------------------------------------------------------------*
025800000000     C* F07GES -GESTIONE TASTO FUNZIONALE F07 -
025900000000     C*------------------------------------------------------------------------*
026000000000     C     F07GES        BEGSR
026100000000     C                   ENDSR
026200000000     C*------------------------------------------------------------------------*
026300000000     C* F08GES -GESTIONE TASTO FUNZIONALE F08 -
026400000000     C*------------------------------------------------------------------------*
026500000000     C     F08GES        BEGSR
026600000000     C                   ENDSR
026700000000     C*------------------------------------------------------------------------*
026800000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
026900000000     C*------------------------------------------------------------------------*
027000000000     C     F12GES        BEGSR
027100000000     C*
027200000000     C* IMPOSTO FLAG DI LIVELLO
027300991103     C                   MOVEL     '1'           F12L00
027400000000     C*
027500000000     C                   ENDSR
027600110329     C*------------------------------------------------------------------------*
027700110329     C* F18GES -GESTIONE TASTO FUNZIONALE F18 - CONFERMA INTERATTIVA
027800110329     C*------------------------------------------------------------------------*
027900110329     C     F18GES        BEGSR
028000110329     C*
028100110329     C                   EXSR      DECODI
028200110329     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
028300110329     C     ERRL00        IFEQ      '0'
028400110329     C*
028500110329     C* LANCIO IL PROGRAMMA DI STAMPA
028600110329     C                   MOVEL     'LSG2'        KCOAZ
028700110329     C                   EXSR      VIDPAR
028800110329     C                   MOVEL     PARAM         KPJBU
028900110329     C                   CALL      'FNLSG2C'
029000110329     C                   PARM                    KPJBA
029100110329     c                   eval      flfine='1'
029200110329     C*
029300110329     C                   ENDIF
029400110329     C*
029500110329     C                   ENDSR
029600000000     C*------------------------------------------------------------------------*
029700000000     C* F21GES -GESTIONE TASTO FUNZIONALE F21 -
029800000000     C*------------------------------------------------------------------------*
029900000000     C     F21GES        BEGSR
030000000000     C                   ENDSR
030100000000     C*------------------------------------------------------------------------*
030200000000     C* F22GES -GESTIONE TASTO FUNZIONALE F22 - GEST.STAMPE
030300000000     C*------------------------------------------------------------------------*
030400000000     C     F22GES        BEGSR
030500000000     C*
030600991020     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
030700000000     C                   Z-ADD     80            LENGH            15 5
030800000000     C                   CALL      'QCAEXEC'
030900000000     C                   PARM                    CMD(1)
031000000000     C                   PARM                    LENGH
031100000000     C*
031200000000     C                   ENDSR
031300000000     C*------------------------------------------------------------------------*
031400991020     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
031500000000     C*------------------------------------------------------------------------*
031600000000     C     F24GES        BEGSR
031700000000     C*
031800000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
031900000000     C                   EXSR      FUNGES
032000000000     C*
032100000000     C                   ENDSR
032200000000     C*------------------------------------------------------------------------*
032300000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
032400000000     C*------------------------------------------------------------------------*
032500000000     C     HELGES        BEGSR
032600000000     C                   ENDSR
032700991020     C*------------------------------------------------------------------------*
032800991103     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
032900000000     C*------------------------------------------------------------------------*
033000000000     C     DECODI        BEGSR
033100080318     c                   clear                   pardal
033200080318     c                   clear                   paral
033300080318     C* CONTROLLO data spedizione dal
033400080318     C     ERRL00        IFEQ      '0'
033500080318     C     VIDdal        IFEQ      *ZEROS
033600080318     C                   MOVEL     '1'           ERRL00
033700080318     C                   MOVEL     MSG(3)        DSMSMS
033800080318     C                   SETON                                        78
033900080318     C                   ELSE
034000080318     C                   move      viddal        g02dat
034100080318     C                   movel     *blank        g02err
034200080318     C                   call      'XSRDA8'
034300080318     C                   parm                    wlbdat
034400080318    1C     g02err        ifeq      '1'
034500080318     C                   MOVEL     '1'           ERRL00
034600080318     C                   MOVEL     MSG(3)        DSMSMS
034700080318     C                   SETON                                        78
034800080318     c                   endif
034900080318     C                   END
035000080318     C                   END
035100080318     c*
035200080318     C* CONTROLLO data sepdizione al
035300080318     C     ERRL00        IFEQ      '0'
035400080318     C                   z-add     g02inv        pardal
035500080318     C                   z-add     G02dat        VIddal
035600080318     C     VIDal         IFEQ      *ZEROS
035700080318     C                   MOVEL     '1'           ERRL00
035800080318     C                   MOVEL     MSG(3)        DSMSMS
035900080318     C                   SETON                                        79
036000080318     C                   ELSE
036100080318     C                   move      vidal         g02dat
036200080318     C                   movel     *blank        g02err
036300080318     C                   call      'XSRDA8'
036400080318     C                   parm                    wlbdat
036500080318    1C     g02err        ifeq      '1'
036600080318     C                   MOVEL     '1'           ERRL00
036700080318     C                   MOVEL     MSG(3)        DSMSMS
036800080318     C                   SETON                                        79
036900080318     c                   endif
037000080318     C                   END
037100080318     C                   END
037200080318     c*
037300080318     c* errore se data dal > data al
037400080318     C     ERRL00        IFEQ      '0'
037500080318     C                   z-add     g02inv        paral
037600080318     C                   z-add     G02dat        VIdal
037700080318     c                   if        pardal>paral
037800080318     C                   MOVEL     '1'           ERRL00
037900080318     C                   MOVEL     MSG(4)        DSMSMS
038000080318     C                   SETON                                        79
038100080318     c                   endif
038200080318     c                   endif
038300080318     c* per il periodo dal/al non devono essere già trascorsi i giorni
038400080318     c* di pulizia
038500080318     C     ERRL00        IFEQ      '0'
038600080318      * Aggancio le due tabelle (5A ) per le date di pulizia archiv    i
038700080318     C                   MOVE      '5A'          ktbcod
038800080318     C                   MOVEL(P)  '1'           ktbkey
038900080318     c                   clear                   ds5a
039000080318     C     Keytab        CHAIN     TABEL00F
039100080318     c                   if        %found(tabel00f)
039200080318     C                   MOVEL     TBLUNI        DS5A
039300080318     c                   endif
039400080318     C                   MOVE      *BLANKS       $PGM
039500080318     C                   MOVEL     'FNLS61R'     $PGM
039600080318     C                   Z-ADD     §5ABLP        $DATPU            6 0
039700080318     C                   EXSR      TESTF
039800080318     C   50              seton                                        78
039900080318     C   50              MOVEL     '1'           ERRL00
040000080318     C   50              MOVEL     MSG(5)        DSMSMS
040100080320     c   50              eval      dsmsms=%trim(dsmsms) + ' '
040200080320     c                             + %editw(datpri:'  /  /    ')
040300080318     c                   endif
040400080318     C*
040500080318     c* Controllo terminal di partenza
040600080318     C                   MOVEL     *BLANKS       VDDtfp
040700080318     C     ERRL00        IFEQ      '0'
040800080318     c     vidtfp        ifeq      *blanks
040900080318     c     vidtfp        oreq      *zeros
041000080318     C                   MOVEL     '1'           ERRL00
041100080318     C                   MOVEL     MSG(14)       DSMSMS
041200080318     C                   SETON                                        80
041300080318     c                   else
041400080318     C                   MOVE      VIDtfp        WLIN
041500090709     c                   move      'T'           wtip
041600080318     C                   EXSR      CHKLIN
041700080318     C     ERRL00        IFEQ      '0'
041800080318     C                   MOVEL     WLIN          VIDtfp
041900080318     C                   MOVEL     WDLIN         VDDtfp
042000080318     C                   ELSE
042100080318     C                   SETON                                        80
042200080318     C                   END
042300080318     C                   END
042400080318     C                   END
042500090709     c* Controllo filiale di partenza
042600090709     C                   MOVEL     *BLANKS       VDDflp
042700090709     C     ERRL00        IFEQ      '0'
042800090709     c     vidflp        ifne      *blanks
042900090709     c     vidflp        andne     *zeros
043000090709     C                   MOVE      VIDflp        WLIN
043100090709     c                   move      'F'           wtip
043200090709     C                   EXSR      CHKLIN
043300090709     C     ERRL00        IFEQ      '0'
043400090709     C                   MOVEL     WLIN          VIDflp
043500090709     C                   MOVEL     WDLIN         VDDflp
043600090709     C                   ELSE
043700090709     C                   SETON                                        81
043800090709     C                   END
043900090709     C                   END
044000090709     C                   END
044100000000     C*
044200991029     C* CONTROLLO PERCENTUALI
044300000000     C     ERRL00        IFEQ      '0'
044400991103     C     VIDPID        IFLT      *ZEROS
044500991103     C     VIDPID        ORGT      100
044600000000     C                   MOVEL     '1'           ERRL00
044700991103     C                   MOVEL     MSG(15)       DSMSMS
044800000000     C                   SETON                                        86
044900000000     C                   END
045000000000     C                   END
045100991029     C*
045200991025     C     ERRL00        IFEQ      '0'
045300991103     C     VIDPSD        IFLT      *ZEROS
045400991103     C     VIDPSD        ORGT      100
045500991025     C                   MOVEL     '1'           ERRL00
045600991103     C                   MOVEL     MSG(15)       DSMSMS
045700991025     C                   SETON                                        87
045800991025     C                   END
045900991025     C                   END
046000991029     C*
046100991029     C* CONTROLLO RANGE INIZ/FIN %  RAPP. E SCOST VOL E COLLI VARI
046200991020     C     ERRL00        IFEQ      '0'
046300991020     C     VIDRG1        IFGT      VIDRG2
046400991020     C                   MOVEL     '1'           ERRL00
046500991020     C                   MOVEL     MSG(7)        DSMSMS
046600991020     C                   SETON                                        8485
046700991020     C                   END
046800991020     C                   END
046900000000     C*
047000000000     C                   ENDSR
047100000000     C*------------------------------------------------------------------------*
047200000000     C*  CHKLIN - CONTROLLA FILIALI E TERMINAL
047300000000     C*------------------------------------------------------------------------*
047400000000     C     CHKLIN        BEGSR
047500000000     C*
047600000000     C* CONTROLLO SE E' STATO INSERITO '?'
047700000000     C     '?'           SCAN      WLIN          A                 1 0    99
047800000000     C*
047900000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
048000000000IF  1C     *IN99         IFEQ      '1'
048100000000     C                   MOVEL     *BLANKS       §COD1             3
048200000000     C                   MOVE      *BLANKS       §DES1
048300971203     C                   MOVE      *BLANKS       §PRA
048400971203     C                   CALL      'TNSD24R'
048500000000     C                   PARM                    §COD1
048600000000     C                   PARM                    §TIP              1
048700000000     C                   PARM                    §DES1            25
048800971203     C                   PARM                    §PRA              3
048900000000     C                   MOVEL     §COD1         WLIN              3
049000000000     C                   MOVEL     §DES1         WDLIN            20
049100000000     C*
049200000000X   1C                   ELSE
049300000000     C*
049400000000     C* CARATTERI ALFABETICI NON VALIDI
049500000000IF  2C     WLIN          IFLT      *ZEROS
049600000000     C                   MOVEL     '1'           ERRL00
049700090710     c                   if        wtip='T'
049800000000     C                   MOVEL     MSG(14)       DSMSMS
049900090710     c                   else
050000090710     C                   MOVEL     MSG(24)       DSMSMS
050100090710     c                   endif
050200000000     C                   MOVEL     *ALL'*'       WDLIN
050300000000X   2C                   ELSE
050400000000     C*
050500000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
050600000000     C                   MOVE      WLIN          KORFIL
050700000000     C     KEYORG        CHAIN     AZORG                              99
050800000000IF  3C     *IN99         IFEQ      '0'
050900000000     C                   MOVEL     ORGDES        WDLIN
051000000000X   3C                   ELSE
051100000000     C                   MOVEL     '1'           ERRL00
051200090710     c                   if        wtip='T'
051300000000     C                   MOVEL     MSG(14)       DSMSMS
051400090710     c                   else
051500090710     C                   MOVEL     MSG(24)       DSMSMS
051600090710     c                   endif
051700000000     C                   MOVEL     *ALL'*'       WDLIN
051800000000E   3C                   END
051900000000IF  3C     ORGFAG        IFEQ      'V'
052000000000     C                   MOVEL     '1'           ERRL00
052100090710     c                   if        wtip='T'
052200000000     C                   MOVEL     MSG(14)       DSMSMS
052300090710     c                   else
052400090710     C                   MOVEL     MSG(14)       DSMSMS
052500090710     c                   endif
052600000000     C                   MOVEL     *ALL'*'       WDLIN
052700000000E   3C                   END
052800080320     c* deve essere un terminal di partenza con impianto
052900080320     c                   if        errl00='0'
053000080320     c                   clear                   fnlv55ds
053100080320     C                   MOVEL     'P'           D55TPT
053200080320     C                   MOVE      wlin          D55LIN
053300080321     C                   z-add     wdateu        d55drf
053400080320     C                   CALL      'FNLV55R'
053500080320     C                   PARM                    FNLV55DS
053600090709     c* Controllo terminal di partenza
053700090709    1c                   if        wtip='T'
053800080320     c                   if        d55err<>*blanks or
053900080320     c                             d55tfp<>orgfil
054000080320     C                   MOVEL     '1'           ERRL00
054100080320     C                   MOVEL     MSG(21)       DSMSMS
054200080320     c                   else
054300080320     C                   MOVEL     ORGDF0        OG150
054301120914     c                   testn                   §ogvol               31
054400120914     c***                if        §ogspv=*blanks
054401120914     c                   if        not *in31 or §ogvol=*zeros
054500080320     C                   MOVEL     '1'           ERRL00
054600080320     C                   MOVEL     MSG(22)       DSMSMS
054700080320     c                   endif
054800080320     c                   endif
054900090709   x1c                   else
055000090709     c* se controllo filiale  errore se non dell'area del terminal di
055100090709     c* partenza
055200090709    2c                   if        d55err<>*blanks or
055300090709     c                             d55tfp<>%dec(vidtfp:3:0)
055400090709     C                   MOVEL     '1'           ERRL00
055500090709     C                   MOVEL     MSG(23)       DSMSMS
055600090709     c
055700090709    2c                   endif
055800090709    1c                   endif
055900080320     c                   endif
056000000000E   2C                   END
056100000000E   1C                   ENDIF
056200000000     C*
056300000000     C                   ENDSR
056400000000     C*------------------------------------------------------------------------*
056500000000     C*  AZZVID - AZZERA O ABBLENCA O IMPOSTA TUTTI I CAMPI DEL FILE VIDEO
056600000000     C*------------------------------------------------------------------------*
056700000000     C     AZZVID        BEGSR
056800000000     C*
056900080318     C                   Z-ADD     *ZEROS        VIDdal
057000080318     C                   Z-ADD     *ZEROS        VIDal
057100080320     c* se utente isp99 o edp sproteggo il terminal di partenza
057200080320     c* altrimenti è sempre protetto
057300080320     c                   if        %subst(knmus:1:3)='EDP' or
057400080320     c                             %subst(knmus:1:5)='ISP99'
057500080320     c                   seton                                        74
057600080320     c                   endif
057700080318     c                   move      simfel        vidtfp
057800080319     C                   MOVE      VIDtfp        WLIN
057900080319     C                   EXSR      CHKLIN
058000080319     C                   MOVEL     WLIN          VIDtfp
058100080319     C                   MOVEL     WDLIN         VDDtfp
058200080320     C                   MOVEL     'D'           VICDET
058300090513     C                   MOVEL     '2'           VIdord
058400090709     c                   movel     'N'           vidpag
058500991020     C*-------                                                         --------*
058600991020     C* REP. VALORI LIMITE TABELLATI PER DEFAULT
058700991020     C*-------                                                         --------*
058800991020     C                   MOVEL     CODUT         KTBUTE
058900991020     C                   MOVEL     'TP'          KTBCOD
059000991020     C     KE2TAB        CHAIN     TABEL00F                           99
059100991020     C     *IN99         IFEQ      *ON
059200991020     C     TBLFLG        OREQ      '*'
059300991020     C* IMPOSTO CAMPI A VIDEO CON PARAMETRI GENERICI=1 e =100
059400991029     C                   Z-ADD     10            VIDPID
059500991029     C                   Z-ADD     10            VIDPSD
059600991029     C                   Z-ADD     -999          VIDRG1
059700991029     C                   Z-ADD     999           VIDRG2
059800991020     C                   ELSE
059900991020     C                   MOVEL     TBLUNI        DSTP
060000991020     C* IMPOSTO CAMPI A VIDEO CON PARAMETRI TABELLATI
060100991029     C                   Z-ADD     §TVDPD        VIDRG1
060200991026     C     VIDRG1        MULT      -1            VIDRG1
060300991029     C                   Z-ADD     §TVDPA        VIDRG2
060400991029     C                   Z-ADD     §TVCID        VIDPID
060500991029     C                   Z-ADD     §TVCSD        VIDPSD
060600991229     C                   Z-ADD     §TVMIN        VIDMIN
060700991029     C                   END
060800000000     C*
060900000000     C                   ENDSR
061000000000     C*------------------------------------------------------------------------*
061100991020     C*  VIDPAR - IMPOSTA I PARAMETRI DI LANCIO DAL VIDEO
061200000000     C*------------------------------------------------------------------------*
061300000000     C     VIDPAR        BEGSR
061400000000     C*
061500080318     C                   MOVE      VIDtfp        PARtfp
061600991029     C                   Z-ADD     VIDPID        PARPID
061700991029     C                   Z-ADD     VIDPSD        PARPSD
061800000000     C                   MOVEL     VICDET        PARDET
061900991020     C                   Z-ADD     VIDRG1        PARRG1
062000991020     C                   Z-ADD     VIDRG2        PARRG2
062100991229     C                   Z-ADD     VIDMIN        PARMIN
062200090513     C                   movel     Vidord        PARord
062300090709     C                   movel     Vidpag        PARpag
062400090709     C                   move      Vidflp        PARflp
062500000000     C*
062600000000     C                   ENDSR
062700080318      **************************************************************************
062800080318      * Viene rilevato l'ultimo giorno di utilizzo del pgm che si
062900080318      * occupa della pulizia archivio                                  o pgm);
063000080318      **************************************************************************
063100080318     C     TESTF         BEGSR
063200080318      *
063300080318     C                   CLEAR                   PRIDAT
063400080320     C                   CLEAR                   datpri            8 0
063500080320     c                   setoff                                       50
063600080318      *
063700080318      * REPERISCO DATA ULTIMO UTILIZZO PGM DI PULIZIA STATISTICHE ARRIVI
063800080318     C                   MOVE      *BLANKS       $DAT
063900080318     C                   MOVE      *BLANKS       $ERR
064000080318     C                   CALL      'TRUL49C'                            31
064100080318     C                   PARM                    $PGM             10
064200080318     C                   PARM                    $DAT              8
064300080318     C                   PARM                    $ERR              1
064400080318      *
064500080318      * PROGRAMMA IN USO DATA ULTIMO ULIZZO = DATA DEL GIORNO
064600080318      * idem se data ultimo utilizzo = 0
064700080318    1C     $ERR          IFEQ      '1'
064800080318     C     *IN31         OREQ      '1'
064900080318     c     $dat          oreq      '00000000'
065000080318     C                   MOVE      WDATEU        G02INV
065100080318   X1C                   ELSE
065200080318     C                   CLEAR                   G02INV
065300080318     C                   MOVE      $DAT          G02INV
065400080318    1C                   ENDIF
065500080318      *
065600080318      * SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI CHE TIENE DALLA
065700080318      * PULIZIA E TROVO LA PRIMA DATA AL DI SOTTO DELLA QUALE NON SI
065800080318      * POSSONO LANCIARE ELABORAZIONI PER MANCANZA DI DATI
065900080318      *
066000080318      * SE LA DATA E' UGUALE A ZERO NON ESEGUO LA SOTTRAZIONE
066100080318     C                   Z-ADD     *ZEROS        G02DAT
066200080318     C                   Z-ADD     *ZEROS        G02TGI
066300080318     C                   MOVEL     '3'           G02ERR
066400080318     C                   CALL      'XSRDA8'
066500080318     C                   PARM                    WLBDAT
066600080318      *
066700080318     C                   SUB       $DATPU        G02TGI
066800080318     C                   Z-ADD     *ZEROS        GIODAT
066900080318     C                   Z-ADD     *ZEROS        GIOINV
067000080318     C                   Z-ADD     G02TGI        GIOTGI
067100080318     C                   CALL      'XSRGI8'
067200080318     C                   PARM                    WGIDAT
067300080318      *
067400080318     C                   Z-ADD     GIOINV        PRIDAT            8 0
067500080318      *
067600080318      * DATA DAL MAGGIORE DATA CALCOLATA
067700080318     C     pardal        IFLE      PRIDAT
067800080318     C                   SETON                                        50
067900080320     c                   move      pridat        dataiso
068000080321     c     dataiso       adddur    1:*d          dataiso
068100080320     c                   move      dataiso       dataeur
068200080320     c                   move      dataeur       datpri
068300080318     C                   ENDIF
068400080318     c* se sono edp e sono il ambiente di prova non do errore
068500080318     c                   movel     knmus         winizio           3
068600080318     c                   if        winizio='EDP' and knsif='FILTRAPFP'
068700080318     C                   SETOFF                                       50
068800080318     C                   ENDIF
068900080318      *
069000080318     C                   ENDSR
069100020917     C*-------------------------------------------------------------------------
069200020917     C* REPERISCE I DATI UTENTE
069300020917     C*-------------------------------------------------------------------------
069400020917     C     REPDATIUTE    BEGSR
069500020917     C*
069600020917     C* INIZIALIZZA VARIABILI DI WRK
069700020917     C                   CLEAR                   TIBS34DS
069800020917     C                   CLEAR                   AZUTEDS
069900020917     C                   CLEAR                   DDATIUTE
070000020917     C*
070100020917     C     *DTAARA       DEFINE    §azute        azuteds
070200020917     C     *DTAARA       DEFINE    §datiute      ddatiute
070300020917     C                   IN(E)     *DTAARA
070400020917if  1C                   IF        %Error
070500020917     c                   EVAL      I34Tla = 'L'
070600020917     C                   CALL      'TIBS34R'
070700020917     C                   PARM                    Tibs34Ds
070800020917     C                   IN        *DTAARA
070900020917e   1C                   ENDIF
071000020917     C*
071100020917     C                   ENDSR
071200000000     C*------------------------------------------------------------------------*
071300980403     C* *INZSR - OPERAZIONI INIZIALI
071400000000     C*------------------------------------------------------------------------*
071500980403     C     *INZSR        BEGSR
071600000000     C*
071700000000     C* RICEVIMENTO PARAMETRI
071800000000     C     *ENTRY        PLIST
071900000000     C                   PARM                    KPJBA
072000000000     C*
072100000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
072200000000     C     KEYORG        KLIST
072300000000     C                   KFLD                    KORFIL
072400000000     C*
072500000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
072600000000     C     KEYTAB        KLIST
072700000000     C                   KFLD                    KTBUTE
072800000000     C                   KFLD                    KTBCOD
072900000000     C                   KFLD                    KTBKEY
073000980403     C*
073100980403     C* CHIAVE LETTURA TABEL00F - PARZIALE
073200980403     C     KE2TAB        KLIST
073300980403     C                   KFLD                    KTBUTE
073400980403     C                   KFLD                    KTBCOD
073500000000     C*
073600000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
073700000000     C     XMSGPA        PLIST
073800000000     C                   PARM                    DSMSMS
073900000000     C                   PARM                    DSMSPG
074000000000     C                   PARM                    DSMSKE
074100000000     C                   PARM                    DSMSER
074200000000     C*
074300000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
074400000000     C                   CALL      'XKEY'
074500000000     C                   PARM                    DSKEY
074600000000     C*
074700000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
074800000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
074900000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
075000000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
075100000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
075200000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
075300000000     C*
075400000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
075500000000     C*   -NOME PGM CHIAMANTE
075600000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
075700080318     C                   MOVEL     'FNLSG1R'     DSMSPG
075800080318     C                   MOVEL     'FNLSG1R'     MSGQUE
075900000000     C                   MOVEL     *BLANKS       MSGKEY
076000000000     C                   MOVEL     *BLANKS       DSMSMS
076100000000     C*
076200000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
076300000000     C                   Z-ADD     0             FUNGRU            5 0
076400110329     C*
076500110329      /free
076600110329         // -?Abilitazione F8=Esecuzione interattiva per utenti EDP*?
076700110329         If  %subst(knmus: 1 : 3) = 'EDP';
076800110329           FUDrig  = FUD(18);
076900110329           FUDatt  = 'S';
077000110329           FUD(18) = FUDrig;
077100110329         EndIf;
077200110329      /end-free
077300000000     C*
077400000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
077500000000     C                   EXSR      FUNGES
077600000000     C*
077700000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
077800000000     C                   MOVEL     '0'           F12L00            1
077900000000     C                   MOVEL     '0'           F03L00            1
078000000000     C                   MOVEL     '0'           FINL00            1
078100000000     C                   MOVEL     '0'           ERRL00            1
078200000000     C                   MOVEL     '0'           FLFINE            1
078300000000     C*
078400000000     C* LANCIO ROUTINE PER VERIFICARE SE
078500000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
078600000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
078700020917     C                   EXSR      REPDATIUTE
078800020917     C                   Z-ADD     1             CODUT             1 0
078900080321     c*
079000080321     C* RILEVO LA DATA DEL GIORNO E LA INVERTO
079100080321     C                   TIME                    WTIME            14 0
079200080321     C                   MOVE      WTIME         WDATE             8 0
079300080321     C                   Z-ADD     WDATE         G02DAT
079400080321     C                   Z-ADD     *ZERO         G02INV
079500080321     C                   MOVEL     *BLANKS       G02ERR
079600080321     C                   CALL      'XSRDA8'
079700080321     C                   PARM                    WLBDAT
079800080321     C                   Z-ADD     G02INV        WDATEU            8 0
079900000000     C*
080000000000     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
080100000000     C                   EXSR      AZZVID
080200000000     C*
080300000000     C                   ENDSR
080400000000     C*------------------------------------------------------------------------*
080500000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
080600000000     C*------------------------------------------------------------------------*
080700000000     C     FUNGES        BEGSR
080800000000     C*
080900000000     C* PULISCO CAMPI DI LAVORO
081000000000     C                   Z-ADD     0             W                 5 0
081100000000     C*
081200000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
081300000000     C* E CARICO TABELLA FUNZIONI ATTIVE
081400000000     C     1             DO        24            I
081500000000     C                   MOVEL     FUD(I)        FUDRIG
081600000000     C*
081700000000     C* PULIZIA FUNZIONI PRECEDENTI
081800000000     C                   MOVEL     *BLANKS       FUV(I)
081900000000     C*
082000000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
082100000000     C* (SALTA F24)
082200000000     C     FUDATT        IFEQ      'S'
082300000000     C     FUDFUN        ANDNE     '24'
082400000000     C                   ADD       1             W
082500000000     C                   MOVEL     FUDDES        FUV(W)
082600000000     C                   END
082700000000     C                   END
082800000000     C*
082900000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
083000000000     C* ALTRIMENTI DISATTIVO F24
083100000000     C     1             DO        24            I
083200000000     C                   MOVEL     FUD(I)        FUDRIG
083300000000     C*
083400000000     C     FUDFUN        IFEQ      '24'
083500000000     C     W             IFGT      6
083600000000     C                   MOVEL     'S'           FUDATT
083700000000     C                   ELSE
083800000000     C                   MOVEL     *BLANKS       FUDATT
083900000000     C                   END
084000000000     C                   MOVEL     FUDRIG        FUD(I)
084100000000     C                   END
084200000000     C                   END
084300000000     C*
084400000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
084500000000     C     W             DIV       6             I
084600000000     C                   MVR                     Z
084700000000     C*
084800000000     C     Z             IFNE      0
084900000000     C     I             ADD       1             Z
085000000000     C                   ELSE
085100000000     C                   Z-ADD     I             Z                 5 0
085200000000     C                   END
085300000000     C*
085400000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
085500000000     C                   ADD       1             FUNGRU
085600000000     C     FUNGRU        IFGT      Z
085700000000     C     FUNGRU        ORLT      1
085800000000     C                   Z-ADD     1             FUNGRU
085900000000     C                   END
086000000000     C*
086100000000     C* PRIMO GRUPPO DI 6 FUNZIONI
086200000000     C     FUNGRU        IFEQ      1
086300000000     C                   MOVEL     FUV(1)        FUN001
086400000000     C                   MOVEL     FUV(2)        FUN002
086500000000     C                   MOVEL     FUV(3)        FUN003
086600000000     C                   MOVEL     FUV(4)        FUN004
086700000000     C                   MOVEL     FUV(5)        FUN005
086800000000     C                   MOVEL     FUV(6)        FUN006
086900000000     C                   END
087000000000     C*
087100000000     C* SECONDO GRUPPO DI 6 FUNZIONI
087200000000     C     FUNGRU        IFEQ      2
087300000000     C                   MOVEL     FUV(7)        FUN001
087400000000     C                   MOVEL     FUV(8)        FUN002
087500000000     C                   MOVEL     FUV(9)        FUN003
087600000000     C                   MOVEL     FUV(10)       FUN004
087700000000     C                   MOVEL     FUV(11)       FUN005
087800000000     C                   MOVEL     FUV(12)       FUN006
087900000000     C                   END
088000000000     C*
088100000000     C* TERZO GRUPPO DI 6 FUNZIONI
088200000000     C     FUNGRU        IFEQ      3
088300000000     C                   MOVEL     FUV(13)       FUN001
088400000000     C                   MOVEL     FUV(14)       FUN002
088500000000     C                   MOVEL     FUV(15)       FUN003
088600000000     C                   MOVEL     FUV(16)       FUN004
088700000000     C                   MOVEL     FUV(17)       FUN005
088800000000     C                   MOVEL     FUV(18)       FUN006
088900000000     C                   END
089000000000     C*
089100000000     C* QUARTO GRUPPO DI 6 FUNZIONI
089200000000     C     FUNGRU        IFEQ      4
089300000000     C                   MOVEL     FUV(19)       FUN001
089400000000     C                   MOVEL     FUV(20)       FUN002
089500000000     C                   MOVEL     FUV(21)       FUN003
089600000000     C                   MOVEL     FUV(22)       FUN004
089700000000     C                   MOVEL     *BLANKS       FUN005
089800000000     C                   MOVEL     *BLANKS       FUN006
089900000000     C                   END
090000000000     C*
090100000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
090200000000     C                   MOVEL     *BLANKS       FUN007
090300000000     C                   MOVEL     *BLANKS       FUN008
090400000000     C*
090500000000     C     1             DO        24            I
090600000000     C                   MOVEL     FUD(I)        FUDRIG
090700000000     C     FUDFUN        IFEQ      '24'
090800000000     C     FUDATT        ANDEQ     'S'
090900000000     C                   MOVEL     FUDDES        FUN008
091000000000     C                   END
091100000000     C                   END
091200000000     C                   ENDSR
091300000000     C*------------------------------------------------------------------------*
091400000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
091500000000     C*------------------------------------------------------------------------*
091600000000     C     FUNCON        BEGSR
091700000000     C*
091800000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
091900000000     C                   EXSR      FUNVAR
092000000000     C*
092100000000     C* PULISCO CAMPI DI LAVORO
092200000000     C                   Z-ADD     0             I                 5 0
092300000000     C                   Z-ADD     0             W                 5 0
092400000000     C*
092500000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
092600000000     C     FUNKEY        IFNE      *BLANKS
092700000000     C     1             DO        24            I
092800000000     C                   MOVEL     FUD(I)        FUDRIG
092900000000     C*
093000000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
093100000000     C     FUNKEY        IFEQ      FUDFUN
093200000000     C     FUDATT        ANDEQ     'S'
093300000000     C                   Z-ADD     I             W
093400000000     C                   END
093500000000     C                   END
093600000000     C*
093700000000     C* FUNZIONE NON ATTIVA (ERRORE)
093800000000     C     W             IFEQ      0
093900000000     C*
094000000000     C* EMISSIONE MESSAGGIO DI ERRORE
094100000000     C                   MOVEL     MSG(1)        DSMSMS
094200000000     C*
094300000000     C                   END
094400000000     C                   END
094500000000     C*
094600000000     C* FUNZIONE ATTIVA (PROCEDI ...)
094700000000     C     W             IFNE      0
094800000000     C     FUNKEY        OREQ      *BLANKS
094900000000     C*
095000000000     C     FUNKEY        IFEQ      *BLANKS
095100000000     C                   EXSR      ENTGES
095200000000     C                   END
095300000000     C     FUNKEY        IFEQ      '01'
095400000000     C                   EXSR      F01GES
095500000000     C                   END
095600000000     C     FUNKEY        IFEQ      '03'
095700000000     C                   EXSR      F03GES
095800000000     C                   END
095900000000     C     FUNKEY        IFEQ      '06'
096000000000     C                   EXSR      F06GES
096100000000     C                   END
096200000000     C     FUNKEY        IFEQ      '07'
096300000000     C                   EXSR      F07GES
096400000000     C                   END
096500000000     C     FUNKEY        IFEQ      '08'
096600000000     C                   EXSR      F08GES
096700000000     C                   END
096800000000     C     FUNKEY        IFEQ      '12'
096900000000     C                   EXSR      F12GES
097000000000     C                   END
097100110329     C                   If        FUNkey = '18'
097200110329     C                   exsr      F18ges
097300110329     C                   EndIf
097400000000     C     FUNKEY        IFEQ      '21'
097500000000     C                   EXSR      F21GES
097600000000     C                   END
097700000000     C     FUNKEY        IFEQ      '22'
097800000000     C                   EXSR      F22GES
097900000000     C                   END
098000000000     C     FUNKEY        IFEQ      '24'
098100000000     C                   EXSR      F24GES
098200000000     C                   END
098300000000     C     FUNKEY        IFEQ      'HP'
098400000000     C                   EXSR      HELGES
098500000000     C                   END
098600000000     C*
098700000000     C                   END
098800000000     C*
098900000000     C                   ENDSR
099000000000     C*------------------------------------------------------------------------*
099100000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
099200000000     C*------------------------------------------------------------------------*
099300000000     C     FUNVAR        BEGSR
099400000000     C*
099500000000     C* INIZIALIZZA VARIABILE -FUNKEY-
099600000000     C*
099700000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
099800000000     C     KEY           IFEQ      ENTER
099900000000     C                   MOVEL     *BLANKS       FUNKEY
100000000000     C                   END
100100000000     C     KEY           IFEQ      F1
100200000000     C                   MOVEL     '01'          FUNKEY
100300000000     C                   END
100400000000     C     KEY           IFEQ      F2
100500000000     C                   MOVEL     '02'          FUNKEY
100600000000     C                   END
100700000000     C     KEY           IFEQ      F3
100800000000     C                   MOVEL     '03'          FUNKEY
100900000000     C                   END
101000000000     C     KEY           IFEQ      F4
101100000000     C                   MOVEL     '04'          FUNKEY
101200000000     C                   END
101300000000     C     KEY           IFEQ      F5
101400000000     C                   MOVEL     '05'          FUNKEY
101500000000     C                   END
101600991103     C     KEY           IFEQ      F6
101700000000     C                   MOVEL     '06'          FUNKEY
101800000000     C                   END
101900000000     C     KEY           IFEQ      F7
102000000000     C                   MOVEL     '07'          FUNKEY
102100000000     C                   END
102200000000     C     KEY           IFEQ      F8
102300000000     C                   MOVEL     '08'          FUNKEY
102400000000     C                   END
102500000000     C     KEY           IFEQ      F9
102600000000     C                   MOVEL     '09'          FUNKEY
102700000000     C                   END
102800000000     C     KEY           IFEQ      F10
102900000000     C                   MOVEL     '10'          FUNKEY
103000000000     C                   END
103100000000     C     KEY           IFEQ      F11
103200000000     C                   MOVEL     '11'          FUNKEY
103300000000     C                   END
103400000000     C     KEY           IFEQ      F12
103500000000     C                   MOVEL     '12'          FUNKEY
103600000000     C                   END
103700000000     C     KEY           IFEQ      F13
103800000000     C                   MOVEL     '13'          FUNKEY
103900000000     C                   END
104000000000     C     KEY           IFEQ      F14
104100000000     C                   MOVEL     '14'          FUNKEY
104200000000     C                   END
104300000000     C     KEY           IFEQ      F15
104400000000     C                   MOVEL     '15'          FUNKEY
104500000000     C                   END
104600000000     C     KEY           IFEQ      F16
104700000000     C                   MOVEL     '16'          FUNKEY
104800000000     C                   END
104900000000     C     KEY           IFEQ      F17
105000000000     C                   MOVEL     '17'          FUNKEY
105100000000     C                   END
105200000000     C     KEY           IFEQ      F18
105300000000     C                   MOVEL     '18'          FUNKEY
105400000000     C                   END
105500000000     C     KEY           IFEQ      F19
105600000000     C                   MOVEL     '19'          FUNKEY
105700000000     C                   END
105800000000     C     KEY           IFEQ      F20
105900000000     C                   MOVEL     '20'          FUNKEY
106000000000     C                   END
106100000000     C     KEY           IFEQ      F21
106200000000     C                   MOVEL     '21'          FUNKEY
106300000000     C                   END
106400000000     C     KEY           IFEQ      F22
106500000000     C                   MOVEL     '22'          FUNKEY
106600000000     C                   END
106700000000     C     KEY           IFEQ      F23
106800000000     C                   MOVEL     '23'          FUNKEY
106900000000     C                   END
107000000000     C     KEY           IFEQ      F24
107100000000     C                   MOVEL     '24'          FUNKEY
107200000000     C                   END
107300000000     C     KEY           IFEQ      ROLLUP
107400000000     C                   MOVEL     'RU'          FUNKEY
107500000000     C                   END
107600000000     C     KEY           IFEQ      HELP
107700000000     C                   MOVEL     'HP'          FUNKEY
107800000000     C                   END
107900000000     C                   ENDSR
108000000000     C*------------------------------------------------------------------------*
108100000000     C* NOTE PER COMPILAZIONE SCHIERE
108200000000     C*------------------------------------------------------------------------*
108300000000     C* -MSG-
108400000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
108500000000     C* (NON TOCCARE MSG,1)
108600000000     C*
108700000000     C* -CMD-
108800000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
108900000000     C*
109000000000     C* -FUD-
109100000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
109200000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
109300000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
109400000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
109500000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
109600000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
109700000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
109800000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
109900000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
110000000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
110100000000     C*
110200000000     C*------------------------------------------------------------------------*
110300991103** ======== SCHIERA: MSG  (MESSAGGI)                       ================
110400080318FNLSG1R- Tasto funzionale non gestito.                                         1
110500080318FNLSG1R- Percentuale non accettabile.                                          2
110600080318FNLSG1R- Data spedizione mancante o errata.                                    3
110700080318FNLSG1R- Data spedizione dal > Data spedzione al                               4
110800080320FNLSG1R- Periodo dal/al non elaborabile:dati disponibili dal                   5
110900080318FNLSG1R-                                                                       6
111000080318FNLSG1R- Range non valido, % iniziale > % finale.                              7
111100080318FNLSG1R- Immettere 'S' se si vogliono includere i volumi "reali" nella stampa. 8
111200080318FNLSG1R- Percentuale iniziale non accettabile.                         .       9
111300080318FNLSG1R- Percentuale finale non accettabile.                           io.    10
111400080318FNLSG1R-                                                                      11
111500080318FNLSG1R- Divisione non accettabile.                                           12
111600080318FNLSG1R- Area non accettabile.                                                13
111700080318FNLSG1R- Terminal di partenza errato o mancante                               14
111800080318FNLSG1R- Percentuale non accettabile, valori corretti da 0 a 100.             15
111900080318FNLSG1R- Data non valida.                                                     16
112000080318FNLSG1R- Parametro non accettabile se scelto lancio per saldi.                17
112100080318FNLSG1R- Parametro non accettabile se scelto lancio estemporaneo.             18
112200080318FNLSG1R- Tipo lancio non accettabile. Inserire 'S'.                           19
112300080318FNLSG1R- Non si possono lanciare le statistiche di gruppo in SDI !!!          20
112400080320FNLSG1R- La filiale non è un terminal di partenza:statistica non elaborabile  21
112500080320FNLSG1R- Non presente impianto rilevazione volume per il terminal indicato    22
112600090709FNLSG1R- Immettere una filiale di partenza dell'area del terminal indicato    23
112700090710FNLSG1R- Filiale di partenza errata                                           24
112800000000** ======== SCHIERA: CMD  (COMANDI)                        ================
112900000000WRKSPLF
113000000000
113100000000
113200000000
113300000000
113400000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
113500000000                   -        (S/N)=N                        funz.01¬
113600000000                   -        (S/N)=N                        funz.02¬
113700000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
113800000000                   -        (S/N)=N                        funz.04¬
113900000000                   -        (S/N)=N                        funz.05¬
114000000000F06=Lancio         -        (S/N)=S                        funz.06¬
114100000000                   -        (S/N)=N                        funz.07¬
114200000000                   -        (S/N)=N                        funz.08¬
114300000000                   -        (S/N)=N                        funz.09¬
114400000000                   -        (S/N)=N                        funz.10¬
114500000000                   -        (S/N)=N                        funz.11¬
114600991103F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
114700000000                   -        (S/N)=N                        funz.13¬
114800000000                   -        (S/N)=N                        funz.14¬
114900000000                   -        (S/N)=N                        funz.15¬
115000000000                   -        (S/N)=N                        funz.16¬
115100000000                   -        (S/N)=N                        funz.17¬
115200110329F18=Lancio interatt-        (S/N)=N  <== S-Utenti EDP*     funz.18¬
115300000000                   -        (S/N)=N                        funz.19¬
115400000000                   -        (S/N)=N                        funz.20¬
115500000000                   -        (S/N)=N                        funz.21¬
115600000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
115700000000                   -        (S/N)=N                        funz.23¬
115800991103F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
