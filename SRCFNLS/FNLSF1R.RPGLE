000100000000     H*------------------------------------------------------------------------*
000200991019     H* STATISTICA PESI - PARAMETRI
000300080317     H* N.B questo pgm è stato copiato e opportunamente modificato da TIST15R
000400000000     H*------------------------------------------------------------------------*
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600000000     F*------------------------------------------------------------------------*
000700000000     F* DATA BASE
000800000000     F*------------------------------------------------------------------------*
000900080317     FFNLSF1D   CF   E             WORKSTN INFDS(DEVDS1)
001000991229     F                                     SFILE(ST15DMS:MSGULT)
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
006100000000     D*-------------------
006200020917     D TIBS34DS      E DS
006300020917     D DDATIUTE      E DS
006400020917     D AZUTEDS       E DS                  extname(AZUTE00F)
006500991019     D*-------------------
006600991019     D* DS PER REPERIMENTO PARAMETRI DI DFT
006700991019     D*-------------------
006800991019     D DSTP          E DS
006900080317
007000080317     d ds5a          e ds
007100000000     D*-------------------
007200000000     D* DS PARAMETRI DI LANCIO PGM
007300000000     D*-------------------
007400950703     D PARAM           DS
007500000000     D  PARTLA                 1      1
007600080317     D**PARANN                 2      5  0
007700080317     D**PARMMI                 6      7  0
007800080317     D**PARMMF                 8      9  0
007900080317     d  pardal                 2      9  0
008000080317     D**PARSNR                10     10
008100991025     D  PARPEI                27     31  2
008200080317     D**PARDIV                32     32
008300080317     D**PARARE                33     35  0
008400080317     D**PARFIL                36     38  0
008500080317     D  PARtfp                36     38  0
008600000000     D  PARDET                39     39
008700080317     D**PARTOT                40     40
008800991026     D  PARRG1                41     44  1
008900991026     D  PARRG2                45     48  1
009000991026     D  PARTAR                49     53  3
009100991026     D  PARPES                54     58  2
009200991229     D  PARMIN                59     63  3
009300080317     D**PARPOS                64     64
009400080320     D**PARNEG                65     65
009500080317     D**PARSAL                66     66
009600080317     d  paral                 67     74  0
009700090508     d  parord                75     75
009800090706     d  parpag                76     76
009900090707     d  parflp                77     79  0
010000000000     D*-------------------
010100000000     D* DS PER GESTIONE TASTI FUNZIONALI
010200000000     D*-------------------
010300000000     D DSKEY         E DS
010400000000     D DEVDS1          DS
010500000000     D  KEY                  369    369
010600000000     D*-------------------
010700000000     D* ARCHITETTURA
010800000000     D*-------------------
010900000000     D KPJBA         E DS
011000080317
011100080317     D WLBDAT          DS
011200080317     D  G02DAT                 1      8  0
011300080317     D  G02INV                 9     16  0
011400080317     D  G02ERR                17     17
011500080317     D  G02TGI                18     22  0
011600080317
011700080317     D WGIDAT          DS
011800080317     D  GIODAT                 1      8  0
011900080317     D  GIOINV                 9     16  0
012000080317     D  GIOTGI                17     21  0
012100080320
012200080320     d fnlv55ds      e ds
012300080320     d og150         e ds
012400080320     d dataiso         s               d   datfmt(*iso)
012500080320     d dataeur         s               d   datfmt(*eur)
012600090708     d wtip            s              1
012700000000     C*------------------------------------------------------------------------*
012800000000     C* MAIN LINE
012900000000     C*------------------------------------------------------------------------*
013000000000     C*
013100000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
013200000000     C     ERRL00        IFEQ      '0'
013300000000     C*
013400000000     C* CICLO FINO A CHE:
013500000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
013600000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
013700000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
013800000000     C     F03L00        DOUEQ     '1'
013900000000     C     F12L00        OREQ      '1'
014000000000     C     FLFINE        OREQ      '1'
014100000000     C*
014200000000     C* EMETTE FORMATO VIDEO
014300000000     C                   EXSR      BUTFMR
014400000000     C*
014500000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
014600000000     C                   EXSR      FUNCON
014700000000     C*
014800000000     C                   END
014900000000     C*
015000000000     C                   END
015100000000     C*
015200000000     C                   SETON                                        LR
015300000000     C*------------------------------------------------------------------------*
015400000000     C* EMISSIONE FORMATO A VIDEO
015500000000     C*------------------------------------------------------------------------*
015600000000     C     BUTFMR        BEGSR
015700000000     C*
015800000000     C* WRITE RECORD TESTATA
015900991229     C                   WRITE     ST15D01
016000000000     C*
016100000000     C* VISUALIZZAZIONE MESSAGGIO
016200000000     C                   EXSR      SNDMSG
016300000000     C*
016400000000     C* WRITE RECORD FUNZIONI
016500991229     C                   WRITE     ST15D02
016600991019     C*
016700991019     C* EXFMT RECORD DETTAGLIO
016800991229     C                   EXFMT     ST15D03
016900991019     C*
017000000000     C* AZZERO FLAG DI ERRORE LIVELLO
017100000000     C                   MOVEL     '0'           ERRL00
017200000000     C*
017300000000     C* AZZERO FLAG DI ERRORE
017400991025     C                   SETOFF                                         7071
017500000000     C                   SETOFF                                       727377
017600050105     C                   SETOFF                                       76
017700000000     C                   SETOFF                                       787983
017800000000     C                   SETOFF                                       848586
017900950703     C                   SETOFF                                       818082
018000000000     C*
018100000000     C                   ENDSR
018200000000     C*------------------------------------------------------------------------*
018300000000     C* SNDMSG - MANDA MESSAGGIO
018400000000     C*------------------------------------------------------------------------*
018500000000     C     SNDMSG        BEGSR
018600000000     C*
018700000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
018800000000     C                   MOVEL     '1'           *IN40
018900991229     C                   WRITE     ST15DMC
019000000000     C                   MOVEL     '0'           *IN40
019100000000     C*
019200000000     C* VISUALIZZAZIONE MESSAGGIO
019300000000     C                   Z-ADD     0             MSGULT            4 0
019400000000     C                   CALL      'XMSG'        XMSGPA
019500000000     C     DSMSER        IFEQ      0
019600000000     C                   MOVEL     DSMSKE        MSGKEY
019700000000     C                   END
019800000000     C                   Z-ADD     1             MSGULT
019900991229     C                   WRITE     ST15DMS
020000991229     C                   WRITE     ST15DMC
020100000000     C*
020200000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
020300000000     C                   MOVEL     *BLANKS       DSMSMS
020400000000     C*
020500000000     C                   ENDSR
020600000000     C*------------------------------------------------------------------------*
020700000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
020800000000     C*------------------------------------------------------------------------*
020900000000     C     ENTGES        BEGSR
021000000000     C*
021100000000     C* CONTROLLA I CAMPI IMMESSI
021200000000     C                   EXSR      DECODI
021300000000     C*
021400000000     C                   ENDSR
021500000000     C*------------------------------------------------------------------------*
021600000000     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
021700000000     C*------------------------------------------------------------------------*
021800000000     C     F01GES        BEGSR
021900000000     C                   ENDSR
022000000000     C*------------------------------------------------------------------------*
022100000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
022200000000     C*------------------------------------------------------------------------*
022300000000     C     F03GES        BEGSR
022400000000     C*
022500000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
022600000000     C                   MOVEL     '1'           F03L00
022700000000     C*
022800000000     C                   ENDSR
022900000000     C*------------------------------------------------------------------------*
023000000000     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
023100000000     C*------------------------------------------------------------------------*
023200991020     C     F06GES        BEGSR
023300000000     C*
023400991105     C                   EXSR      DECODI
023500991105     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
023600991105     C     ERRL00        IFEQ      '0'
023700991105     C*
023800000000     C* LANCIO IL PROGRAMMA DI STAMPA
023900080317     C                   MOVEL     'LSF2'        KCOAZ
024000000000     C                   EXSR      VIDPAR
024100950703     C                   MOVEL     PARAM         KPJBU
024200000517     C                   CALL      'BCH10'
024300080317 MOM C*                    CALL 'FNLSF2C'
024400991021     C                   PARM                    KPJBA
024500080319     c                   eval      flfine='1'
024600991123     C*
024700991105     C                   ENDIF
024800991105     C*
024900000000     C                   ENDSR
025000000000     C*------------------------------------------------------------------------*
025100000000     C* F07GES -GESTIONE TASTO FUNZIONALE F07 -
025200000000     C*------------------------------------------------------------------------*
025300000000     C     F07GES        BEGSR
025400000000     C                   ENDSR
025500000000     C*------------------------------------------------------------------------*
025600000000     C* F08GES -GESTIONE TASTO FUNZIONALE F08 -
025700000000     C*------------------------------------------------------------------------*
025800000000     C     F08GES        BEGSR
025900000000     C                   ENDSR
026000000000     C*------------------------------------------------------------------------*
026100000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
026200000000     C*------------------------------------------------------------------------*
026300000000     C     F12GES        BEGSR
026400000000     C*
026500000000     C* IMPOSTO FLAG DI LIVELLO
026600000000     C                   MOVEL     '1'           F12L00
026700000000     C*
026800000000     C                   ENDSR
026900110329     C*------------------------------------------------------------------------*
027000110329     C* F18GES -GESTIONE TASTO FUNZIONALE F18 - CONFERMA INTERATTIVA
027100110329     C*------------------------------------------------------------------------*
027200110329     C     F18GES        BEGSR
027300110329     C*
027400110329     C                   EXSR      DECODI
027500110329     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
027600110329     C     ERRL00        IFEQ      '0'
027700110329     C*
027800110329     C* LANCIO IL PROGRAMMA DI STAMPA
027900110329     C                   MOVEL     'LSF2'        KCOAZ
028000110329     C                   EXSR      VIDPAR
028100110329     C                   MOVEL     PARAM         KPJBU
028200110329     C                   CALL      'FNLSF2C'
028300110329     C                   PARM                    KPJBA
028400110329     c                   eval      flfine='1'
028500110329     C*
028600110329     C                   ENDIF
028700110329     C*
028800110329     C                   ENDSR
028900000000     C*------------------------------------------------------------------------*
029000000000     C* F21GES -GESTIONE TASTO FUNZIONALE F21 -
029100000000     C*------------------------------------------------------------------------*
029200000000     C     F21GES        BEGSR
029300000000     C                   ENDSR
029400000000     C*------------------------------------------------------------------------*
029500000000     C* F22GES -GESTIONE TASTO FUNZIONALE F22 - GEST.STAMPE
029600000000     C*------------------------------------------------------------------------*
029700000000     C     F22GES        BEGSR
029800000000     C*
029900991020     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
030000000000     C                   Z-ADD     80            LENGH            15 5
030100000000     C                   CALL      'QCAEXEC'
030200000000     C                   PARM                    CMD(1)
030300000000     C                   PARM                    LENGH
030400000000     C*
030500000000     C                   ENDSR
030600000000     C*------------------------------------------------------------------------*
030700991020     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
030800000000     C*------------------------------------------------------------------------*
030900000000     C     F24GES        BEGSR
031000000000     C*
031100000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
031200000000     C                   EXSR      FUNGES
031300000000     C*
031400000000     C                   ENDSR
031500000000     C*------------------------------------------------------------------------*
031600000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
031700000000     C*------------------------------------------------------------------------*
031800000000     C     HELGES        BEGSR
031900000000     C                   ENDSR
032000991020     C*------------------------------------------------------------------------*
032100991105     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
032200000000     C*------------------------------------------------------------------------*
032300000000     C     DECODI        BEGSR
032400000000     C*
032500080317     c                   clear                   pardal
032600080317     c                   clear                   paral
032700080317     C* CONTROLLO data spedizione dal
032800991020     C     ERRL00        IFEQ      '0'
032900080317     C     VIDdal        IFEQ      *ZEROS
033000000000     C                   MOVEL     '1'           ERRL00
033100000000     C                   MOVEL     MSG(3)        DSMSMS
033200080317     C                   SETON                                        78
033300991020     C                   ELSE
033400080317     C                   move      viddal        g02dat
033500080317     C                   movel     *blank        g02err
033600080317     C                   call      'XSRDA8'
033700080317     C                   parm                    wlbdat
033800080317    1C     g02err        ifeq      '1'
033900000000     C                   MOVEL     '1'           ERRL00
034000000000     C                   MOVEL     MSG(3)        DSMSMS
034100080317     C                   SETON                                        78
034200080317     c                   endif
034300991020     C                   END
034400991020     C                   END
034500080317     c*
034600080317     C* CONTROLLO data sepdizione al
034700080317     C     ERRL00        IFEQ      '0'
034800080317     C                   z-add     g02inv        pardal
034900080317     C                   z-add     G02dat        VIddal
035000080317     C     VIDal         IFEQ      *ZEROS
035100080317     C                   MOVEL     '1'           ERRL00
035200080317     C                   MOVEL     MSG(3)        DSMSMS
035300080317     C                   SETON                                        79
035400080317     C                   ELSE
035500080317     C                   move      vidal         g02dat
035600080317     C                   movel     *blank        g02err
035700080317     C                   call      'XSRDA8'
035800080317     C                   parm                    wlbdat
035900080317    1C     g02err        ifeq      '1'
036000080317     C                   MOVEL     '1'           ERRL00
036100080317     C                   MOVEL     MSG(3)        DSMSMS
036200080317     C                   SETON                                        79
036300080317     c                   endif
036400080317     C                   END
036500080317     C                   END
036600080317     c*
036700080317     c* errore se data dal > data al
036800080317     C     ERRL00        IFEQ      '0'
036900080317     C                   z-add     g02inv        paral
037000080317     C                   z-add     G02dat        VIdal
037100080317     c                   if        pardal>paral
037200080317     C                   MOVEL     '1'           ERRL00
037300080317     C                   MOVEL     MSG(4)        DSMSMS
037400080317     C                   SETON                                        79
037500080317     c                   endif
037600080317     c                   endif
037700080317     c* per il periodo dal/al non devono essere già trascorsi i giorni
037800080317     c* di pulizia
037900080317     C     ERRL00        IFEQ      '0'
038000080317      * Aggancio le due tabelle (5A ) per le date di pulizia archiv    i
038100080317     C                   MOVE      '5A'          ktbcod
038200080317     C                   MOVEL(P)  '1'           ktbkey
038300080317     c                   clear                   ds5a
038400080317     C     Keytab        CHAIN     TABEL00F
038500080317     c                   if        %found(tabel00f)
038600080317     C                   MOVEL     TBLUNI        DS5A
038700080317     c                   endif
038800080317     C                   MOVE      *BLANKS       $PGM
038900080317     C                   MOVEL     'FNLS61R'     $PGM
039000080317     C                   Z-ADD     §5ABLP        $DATPU            6 0
039100080317     C                   EXSR      TESTF
039200080317     C   50              seton                                        78
039300080318     C   50              MOVEL     '1'           ERRL00
039400080318     C   50              MOVEL     MSG(5)        DSMSMS
039500080320     c   50              eval      dsmsms=%trim(dsmsms) + ' '
039600080320     c                             + %editw(datpri:'  /  /    ')
039700080317     c                   endif
039800080317     C*
039900080317     c* Controllo terminal di partenza
040000080317     C                   MOVEL     *BLANKS       VDDtfp
040100080317     C     ERRL00        IFEQ      '0'
040200080317     c     vidtfp        ifeq      *blanks
040300080317     c     vidtfp        oreq      *zeros
040400080317     C                   MOVEL     '1'           ERRL00
040500080317     C                   MOVEL     MSG(14)       DSMSMS
040600080317     C                   SETON                                        80
040700080317     c                   else
040800080317     C                   MOVE      VIDtfp        WLIN
040900090707     c                   move      'T'           wtip
041000080317     C                   EXSR      CHKLIN
041100080317     C     ERRL00        IFEQ      '0'
041200080317     C                   MOVEL     WLIN          VIDtfp
041300080317     C                   MOVEL     WDLIN         VDDtfp
041400080317     C                   ELSE
041500080317     C                   SETON                                        80
041600080317     C                   END
041700080317     C                   END
041800080317     C                   END
041900090707     c* Controllo filiale di partenza
042000090707     C                   MOVEL     *BLANKS       VDDflp
042100090707     C     ERRL00        IFEQ      '0'
042200090708     c     vidflp        ifne      *blanks
042300090708     c     vidflp        andne     *zeros
042400090707     C                   MOVE      VIDflp        WLIN
042500090707     c                   move      'F'           wtip
042600090707     C                   EXSR      CHKLIN
042700090707     C     ERRL00        IFEQ      '0'
042800090707     C                   MOVEL     WLIN          VIDflp
042900090707     C                   MOVEL     WDLIN         VDDflp
043000090707     C                   ELSE
043100090707     C                   SETON                                        81
043200090707     C                   END
043300090707     C                   END
043400090707     C                   END
043500000000     C*
043600991020     C* CONTROLLO PERCENTUALE
043700080317     C     ERRL00        IFEQ      '0'
043800080317     C     VIDPEI        IFLT      *ZEROS
043900080317     C     VIDPEI        ORGT      100
044000000000     C                   MOVEL     '1'           ERRL00
044100991104     C                   MOVEL     MSG(15)       DSMSMS
044200000000     C                   SETON                                        86
044300000000     C                   END
044400000000     C                   END
044500000000     C*
044600991025     C     ERRL00        IFEQ      '0'
044700991104     C     VIDPES        IFLT      *ZEROS
044800991104     C     VIDPES        ORGT      100
044900991025     C                   MOVEL     '1'           ERRL00
045000991104     C                   MOVEL     MSG(15)       DSMSMS
045100991025     C                   SETON                                        87
045200991025     C                   END
045300991025     C                   END
045400991020     C*
045500991020     C* CONTROLLO RANGE INIZ/FIN PERC. RAPPORTO PESI X ELAB.
045600991104     C     ERRL00        IFEQ      '0'
045700991104     C     VIDRG1        IFGT      VIDRG2
045800991104     C                   MOVEL     '1'           ERRL00
045900991104     C                   MOVEL     MSG(7)        DSMSMS
046000991104     C                   SETON                                        8485
046100991104     C                   END
046200991104     C                   END
046300000000     C*
046400000000     C                   ENDSR
046500000000     C*------------------------------------------------------------------------*
046600000000     C*  CHKLIN - CONTROLLA FILIALI E TERMINAL
046700000000     C*------------------------------------------------------------------------*
046800000000     C     CHKLIN        BEGSR
046900000000     C*
047000000000     C* CONTROLLO SE E' STATO INSERITO '?'
047100000000     C     '?'           SCAN      WLIN          A                 1 0    99
047200000000     C*
047300000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
047400000000IF  1C     *IN99         IFEQ      '1'
047500000000     C                   MOVEL     *BLANKS       §COD1             3
047600000000     C                   MOVE      *BLANKS       §DES1
047700971203     C                   MOVE      *BLANKS       §PRA
047800971203     C                   CALL      'TNSD24R'
047900000000     C                   PARM                    §COD1
048000000000     C                   PARM                    §TIP              1
048100000000     C                   PARM                    §DES1            25
048200971203     C                   PARM                    §PRA              3
048300000000     C                   MOVEL     §COD1         WLIN              3
048400000000     C                   MOVEL     §DES1         WDLIN            20
048500000000     C*
048600000000X   1C                   ELSE
048700000000     C*
048800000000     C* CARATTERI ALFABETICI NON VALIDI
048900000000IF  2C     WLIN          IFLT      *ZEROS
049000000000     C                   MOVEL     '1'           ERRL00
049100090710     c                   if        wtip='T'
049200000000     C                   MOVEL     MSG(14)       DSMSMS
049300090710     c                   else
049400090710     C                   MOVEL     MSG(24)       DSMSMS
049500090710     c                   endif
049600000000     C                   MOVEL     *ALL'*'       WDLIN
049700000000X   2C                   ELSE
049800000000     C*
049900000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
050000000000     C                   MOVE      WLIN          KORFIL
050100000000     C     KEYORG        CHAIN     AZORG                              99
050200000000IF  3C     *IN99         IFEQ      '0'
050300000000     C                   MOVEL     ORGDES        WDLIN
050400000000X   3C                   ELSE
050500000000     C                   MOVEL     '1'           ERRL00
050600090710     c                   if        wtip='T'
050700000000     C                   MOVEL     MSG(14)       DSMSMS
050800090710     c                   else
050900090710     C                   MOVEL     MSG(24)       DSMSMS
051000090710     c                   endif
051100000000     C                   MOVEL     *ALL'*'       WDLIN
051200000000E   3C                   END
051300000000IF  3C     ORGFAG        IFEQ      'V'
051400000000     C                   MOVEL     '1'           ERRL00
051500090710     c                   if        wtip='T'
051600000000     C                   MOVEL     MSG(14)       DSMSMS
051700090710     c                   else
051800090710     C                   MOVEL     MSG(24)       DSMSMS
051900090710     c                   endif
052000000000     C                   MOVEL     *ALL'*'       WDLIN
052100000000E   3C                   END
052200090707      *
052300080320     c                   if        errl00='0'
052400080320     c                   clear                   fnlv55ds
052500080320     C                   MOVEL     'P'           D55TPT
052600080320     C                   MOVE      wlin          D55LIN
052700080321     C                   z-add     wdateu        d55drf
052800080320     C                   CALL      'FNLV55R'
052900080320     C                   PARM                    FNLV55DS
053000090707     c* Controllo terminal di partenza
053100090708    1c                   if        wtip='T'
053200090707     c* deve essere un terminal di partenza con impianto
053300090708    2c                   if        d55err<>*blanks or
053400080320     c                             d55tfp<>orgfil
053500080320     C                   MOVEL     '1'           ERRL00
053600080320     C                   MOVEL     MSG(21)       DSMSMS
053700090708   x2c                   else
053800080320     C                   MOVEL     ORGDF0        OG150
053801120914     c                   testn                   §ogpes               31
053900120914    3c***                if        §ogspp=*blanks
053901120914    3c                   if        not *in31 or §ogpes=*zeros
054000080320     C                   MOVEL     '1'           ERRL00
054100080320     C                   MOVEL     MSG(22)       DSMSMS
054200090708    3c                   endif
054300090708    2c                   endif
054400090708   x1c                   else
054500090707     c* se controllo filiale  errore se non dell'area del terminal di
054600090707     c* partenza
054700090708    2c                   if        d55err<>*blanks or
054800090708     c                             d55tfp<>%dec(vidtfp:3:0)
054900090707     C                   MOVEL     '1'           ERRL00
055000090707     C                   MOVEL     MSG(23)       DSMSMS
055100090707     c
055200090708    2c                   endif
055300090708    1c                   endif
055400080320     c                   endif
055500080320     c
055600000000E   2C                   END
055700000000E   1C                   ENDIF
055800000000     C*
055900000000     C                   ENDSR
056000000000     C*------------------------------------------------------------------------*
056100000000     C*  AZZVID - AZZERA O ABBLENCA O IMPOSTA TUTTI I CAMPI DEL FILE VIDEO
056200000000     C*------------------------------------------------------------------------*
056300000000     C     AZZVID        BEGSR
056400000000     C*
056500080317     C                   Z-ADD     *ZEROS        VIDdal
056600080317     C                   Z-ADD     *ZEROS        VIDal
056700080320     c* se utente isp99 o edp sproteggo il terminal di partenza
056800080320     c* altrimenti è sempre protetto
056900080320     c                   if        %subst(knmus:1:3)='EDP' or
057000080320     c                             %subst(knmus:1:5)='ISP99'
057100080320     c                   seton                                        74
057200080320     c                   endif
057300080317     c                   move      simfel        vidtfp
057400080319     C                   MOVE      VIDtfp        WLIN
057500080319     C                   EXSR      CHKLIN
057600080319     C                   MOVEL     WLIN          VIDtfp
057700080319     C                   MOVEL     WDLIN         VDDtfp
057800080320     C                   MOVEL     'D'           VICDET
057900080320     C*                  MOVEL     'N'           VICNEG
058000090513     C                   MOVEL     '2'           VIdord
058100090706     c                   movel     'N'           vidpag
058200991020     C*-------                                                         --------*
058300991020     C* REP. VALORI LIMITE TABELLATI PER DEFAULT
058400991020     C*-------                                                         --------*
058500991020     C                   MOVEL     CODUT         KTBUTE
058600991020     C                   MOVEL     'TP'          KTBCOD
058700991020     C     KE2TAB        CHAIN     TABEL00F                           99
058800991020     C     *IN99         IFEQ      *ON
058900991020     C     TBLFLG        OREQ      '*'
059000991020     C* IMPOSTO CAMPI A VIDEO CON PARAMETRI GENERICI=1 e =100
059100991029     C                   Z-ADD     10            VIDPEI
059200991029     C                   Z-ADD     10            VIDPES
059300991029     C                   Z-ADD     -999          VIDRG1
059400991029     C                   Z-ADD     999           VIDRG2
059500991020     C                   ELSE
059600991020     C                   MOVEL     TBLUNI        DSTP
059700991020     C* IMPOSTO CAMPI A VIDEO CON PARAMETRI TABELLATI
059800991020     C                   Z-ADD     §TPDPD        VIDRG1
059900991026     C     VIDRG1        MULT      -1            VIDRG1
060000991026     C                   Z-ADD     §TPDPA        VIDRG2
060100991025     C                   Z-ADD     §TPCOI        VIDPEI
060200991025     C                   Z-ADD     §TPCOS        VIDPES
060300991229     C                   Z-ADD     §TPMIN        VIDMIN
060400991020     C                   Z-ADD     §TPTAR        PARTAR
060500000000     C                   END
060600000000     C*
060700000000     C                   ENDSR
060800000000     C*------------------------------------------------------------------------*
060900991020     C*  VIDPAR - IMPOSTA I PARAMETRI DI LANCIO DAL VIDEO
061000000000     C*------------------------------------------------------------------------*
061100000000     C     VIDPAR        BEGSR
061200000000     C*
061300080317     C                   MOVE      VIDtfp        PARtfp
061400991025     C                   Z-ADD     VIDPEI        PARPEI
061500991025     C                   Z-ADD     VIDPES        PARPES
061600000000     C                   MOVEL     VICDET        PARDET
061700991020     C                   Z-ADD     VIDRG1        PARRG1
061800991020     C                   Z-ADD     VIDRG2        PARRG2
061900991229     C                   Z-ADD     VIDMIN        PARMIN
062000090508     C                   movel     Vidord        PARord
062100090706     C                   movel     Vidpag        PARpag
062200090707     C                   move      Vidflp        PARflp
062300080320     C**                 MOVEL     VICNEG        PARNEG
062400991214     C*
062500000000     C                   ENDSR
062600080317      **************************************************************************
062700080317      * Viene rilevato l'ultimo giorno di utilizzo del pgm che si
062800080317      * occupa della pulizia archivio                                  o pgm);
062900080317      **************************************************************************
063000080317     C     TESTF         BEGSR
063100080317      *
063200080320     c                   setoff                                       50
063300080317     C                   CLEAR                   PRIDAT
063400080320     C                   CLEAR                   datpri            8 0
063500080317      *
063600080317      * REPERISCO DATA ULTIMO UTILIZZO PGM DI PULIZIA STATISTICHE ARRIVI
063700080317     C                   MOVE      *BLANKS       $DAT
063800080317     C                   MOVE      *BLANKS       $ERR
063900080317     C                   CALL      'TRUL49C'                            31
064000080317     C                   PARM                    $PGM             10
064100080317     C                   PARM                    $DAT              8
064200080317     C                   PARM                    $ERR              1
064300080317      *
064400080317      * PROGRAMMA IN USO DATA ULTIMO ULIZZO = DATA DEL GIORNO
064500080317      * idem se data ultimo utilizzo = 0
064600080317    1C     $ERR          IFEQ      '1'
064700080317     C     *IN31         OREQ      '1'
064800080317     c     $dat          oreq      '00000000'
064900080317     C                   MOVE      WDATEU        G02INV
065000080317   X1C                   ELSE
065100080317     C                   CLEAR                   G02INV
065200080317     C                   MOVE      $DAT          G02INV
065300080317    1C                   ENDIF
065400080317      *
065500080317      * SOTTRAGGO ALLA DATA DI ULTIMO UTILIZZO I GIORNI CHE TIENE DALLA
065600080317      * PULIZIA E TROVO LA PRIMA DATA AL DI SOTTO DELLA QUALE NON SI
065700080317      * POSSONO LANCIARE ELABORAZIONI PER MANCANZA DI DATI
065800080317      *
065900080317      * SE LA DATA E' UGUALE A ZERO NON ESEGUO LA SOTTRAZIONE
066000080317     C                   Z-ADD     *ZEROS        G02DAT
066100080317     C                   Z-ADD     *ZEROS        G02TGI
066200080317     C                   MOVEL     '3'           G02ERR
066300080317     C                   CALL      'XSRDA8'
066400080317     C                   PARM                    WLBDAT
066500080317      *
066600080317     C                   SUB       $DATPU        G02TGI
066700080317     C                   Z-ADD     *ZEROS        GIODAT
066800080317     C                   Z-ADD     *ZEROS        GIOINV
066900080317     C                   Z-ADD     G02TGI        GIOTGI
067000080317     C                   CALL      'XSRGI8'
067100080317     C                   PARM                    WGIDAT
067200080317      *
067300080317     C                   Z-ADD     GIOINV        PRIDAT            8 0
067400080317      *
067500080317      * DATA DAL MAGGIORE DATA CALCOLATA
067600080317     C     pardal        IFLE      PRIDAT
067700080317     C                   SETON                                        50
067800080320     c                   move      pridat        dataiso
067900080321     c     dataiso       adddur    1:*d          dataiso
068000080320     c                   move      dataiso       dataeur
068100080320     c                   move      dataeur       datpri
068200080317     C                   ENDIF
068300080317     c* se sono edp e sono il ambiente di prova non do errore
068400080317     c                   movel     knmus         winizio           3
068500080317     c                   if        winizio='EDP' and knsif='FILTRAPFP'
068600080317     C                   SETOFF                                       50
068700080317     C                   ENDIF
068800080317      *
068900080317     C                   ENDSR
069000020917     C*-------------------------------------------------------------------------
069100020917     C* REPERISCE I DATI UTENTE
069200020917     C*-------------------------------------------------------------------------
069300020917     C     REPDATIUTE    BEGSR
069400020917     C*
069500020917     C* INIZIALIZZA VARIABILI DI WRK
069600020917     C                   CLEAR                   TIBS34DS
069700020917     C                   CLEAR                   AZUTEDS
069800020917     C                   CLEAR                   DDATIUTE
069900020917     C*
070000020917     C     *DTAARA       DEFINE    §azute        azuteds
070100020917     C     *DTAARA       DEFINE    §datiute      ddatiute
070200020917     C                   IN(E)     *DTAARA
070300020917if  1C                   IF        %Error
070400020917     c                   EVAL      I34Tla = 'L'
070500020917     C                   CALL      'TIBS34R'
070600020917     C                   PARM                    Tibs34Ds
070700020917     C                   IN        *DTAARA
070800020917e   1C                   ENDIF
070900020917     C*
071000020917     C                   ENDSR
071100000000     C*------------------------------------------------------------------------*
071200980403     C* *INZSR - OPERAZIONI INIZIALI
071300000000     C*------------------------------------------------------------------------*
071400980403     C     *INZSR        BEGSR
071500000000     C*
071600000000     C* RICEVIMENTO PARAMETRI
071700000000     C     *ENTRY        PLIST
071800000000     C                   PARM                    KPJBA
071900000000     C*
072000000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
072100000000     C     KEYORG        KLIST
072200000000     C                   KFLD                    KORFIL
072300000000     C*
072400000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
072500000000     C     KEYTAB        KLIST
072600000000     C                   KFLD                    KTBUTE
072700000000     C                   KFLD                    KTBCOD
072800000000     C                   KFLD                    KTBKEY
072900980403     C*
073000980403     C* CHIAVE LETTURA TABEL00F - PARZIALE
073100980403     C     KE2TAB        KLIST
073200980403     C                   KFLD                    KTBUTE
073300980403     C                   KFLD                    KTBCOD
073400000000     C*
073500000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
073600000000     C     XMSGPA        PLIST
073700000000     C                   PARM                    DSMSMS
073800000000     C                   PARM                    DSMSPG
073900000000     C                   PARM                    DSMSKE
074000000000     C                   PARM                    DSMSER
074100000000     C*
074200000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
074300000000     C                   CALL      'XKEY'
074400000000     C                   PARM                    DSKEY
074500000000     C*
074600000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
074700000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
074800000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
074900000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
075000000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
075100000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
075200000000     C*
075300000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
075400000000     C*   -NOME PGM CHIAMANTE
075500000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
075600080317     C                   MOVEL     'FNLSF1R'     DSMSPG
075700080317     C                   MOVEL     'FNLSF1R'     MSGQUE
075800000000     C                   MOVEL     *BLANKS       MSGKEY
075900000000     C                   MOVEL     *BLANKS       DSMSMS
076000000000     C*
076100000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
076200000000     C                   Z-ADD     0             FUNGRU            5 0
076300110329     C*
076400110329      /free
076500110329         // -?Abilitazione F8=Esecuzione interattiva per utenti EDP*?
076600110329         If  %subst(knmus: 1 : 3) = 'EDP';
076700110329           FUDrig  = FUD(18);
076800110329           FUDatt  = 'S';
076900110329           FUD(18) = FUDrig;
077000110329         EndIf;
077100110329      /end-free
077200000000     C*
077300000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
077400000000     C                   EXSR      FUNGES
077500000000     C*
077600000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
077700000000     C                   MOVEL     '0'           F12L00            1
077800000000     C                   MOVEL     '0'           F03L00            1
077900000000     C                   MOVEL     '0'           FINL00            1
078000000000     C                   MOVEL     '0'           ERRL00            1
078100000000     C                   MOVEL     '0'           FLFINE            1
078200000000     C*
078300000000     C* LANCIO ROUTINE PER VERIFICARE SE
078400000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
078500000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
078600020917     C                   EXSR      REPDATIUTE
078700020917     C                   Z-ADD     1             CODUT             1 0
078800000000     C*
078900080317     C* RILEVO LA DATA DEL GIORNO E LA INVERTO
079000080317     C                   TIME                    WTIME            14 0
079100080317     C                   MOVE      WTIME         WDATE             8 0
079200080317     C                   Z-ADD     WDATE         G02DAT
079300080317     C                   Z-ADD     *ZERO         G02INV
079400080317     C                   MOVEL     *BLANKS       G02ERR
079500080317     C                   CALL      'XSRDA8'
079600080317     C                   PARM                    WLBDAT
079700080317     C                   Z-ADD     G02INV        WDATEU            8 0
079800080321     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
079900080321     C                   EXSR      AZZVID
080000080319      *
080100000000     C                   ENDSR
080200000000     C*------------------------------------------------------------------------*
080300000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
080400000000     C*------------------------------------------------------------------------*
080500000000     C     FUNGES        BEGSR
080600000000     C*
080700000000     C* PULISCO CAMPI DI LAVORO
080800000000     C                   Z-ADD     0             W                 5 0
080900000000     C*
081000000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
081100000000     C* E CARICO TABELLA FUNZIONI ATTIVE
081200000000     C     1             DO        24            I
081300000000     C                   MOVEL     FUD(I)        FUDRIG
081400000000     C*
081500000000     C* PULIZIA FUNZIONI PRECEDENTI
081600000000     C                   MOVEL     *BLANKS       FUV(I)
081700000000     C*
081800000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
081900000000     C* (SALTA F24)
082000000000     C     FUDATT        IFEQ      'S'
082100000000     C     FUDFUN        ANDNE     '24'
082200000000     C                   ADD       1             W
082300000000     C                   MOVEL     FUDDES        FUV(W)
082400000000     C                   END
082500000000     C                   END
082600000000     C*
082700000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
082800000000     C* ALTRIMENTI DISATTIVO F24
082900000000     C     1             DO        24            I
083000000000     C                   MOVEL     FUD(I)        FUDRIG
083100000000     C*
083200000000     C     FUDFUN        IFEQ      '24'
083300000000     C     W             IFGT      6
083400000000     C                   MOVEL     'S'           FUDATT
083500000000     C                   ELSE
083600000000     C                   MOVEL     *BLANKS       FUDATT
083700000000     C                   END
083800000000     C                   MOVEL     FUDRIG        FUD(I)
083900000000     C                   END
084000000000     C                   END
084100000000     C*
084200000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
084300000000     C     W             DIV       6             I
084400000000     C                   MVR                     Z
084500000000     C*
084600000000     C     Z             IFNE      0
084700000000     C     I             ADD       1             Z
084800000000     C                   ELSE
084900000000     C                   Z-ADD     I             Z                 5 0
085000000000     C                   END
085100000000     C*
085200000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
085300000000     C                   ADD       1             FUNGRU
085400000000     C     FUNGRU        IFGT      Z
085500000000     C     FUNGRU        ORLT      1
085600000000     C                   Z-ADD     1             FUNGRU
085700000000     C                   END
085800000000     C*
085900000000     C* PRIMO GRUPPO DI 6 FUNZIONI
086000000000     C     FUNGRU        IFEQ      1
086100000000     C                   MOVEL     FUV(1)        FUN001
086200000000     C                   MOVEL     FUV(2)        FUN002
086300000000     C                   MOVEL     FUV(3)        FUN003
086400000000     C                   MOVEL     FUV(4)        FUN004
086500000000     C                   MOVEL     FUV(5)        FUN005
086600000000     C                   MOVEL     FUV(6)        FUN006
086700000000     C                   END
086800000000     C*
086900000000     C* SECONDO GRUPPO DI 6 FUNZIONI
087000000000     C     FUNGRU        IFEQ      2
087100000000     C                   MOVEL     FUV(7)        FUN001
087200000000     C                   MOVEL     FUV(8)        FUN002
087300000000     C                   MOVEL     FUV(9)        FUN003
087400000000     C                   MOVEL     FUV(10)       FUN004
087500000000     C                   MOVEL     FUV(11)       FUN005
087600000000     C                   MOVEL     FUV(12)       FUN006
087700000000     C                   END
087800000000     C*
087900000000     C* TERZO GRUPPO DI 6 FUNZIONI
088000000000     C     FUNGRU        IFEQ      3
088100000000     C                   MOVEL     FUV(13)       FUN001
088200000000     C                   MOVEL     FUV(14)       FUN002
088300000000     C                   MOVEL     FUV(15)       FUN003
088400000000     C                   MOVEL     FUV(16)       FUN004
088500000000     C                   MOVEL     FUV(17)       FUN005
088600000000     C                   MOVEL     FUV(18)       FUN006
088700000000     C                   END
088800000000     C*
088900000000     C* QUARTO GRUPPO DI 6 FUNZIONI
089000000000     C     FUNGRU        IFEQ      4
089100000000     C                   MOVEL     FUV(19)       FUN001
089200000000     C                   MOVEL     FUV(20)       FUN002
089300000000     C                   MOVEL     FUV(21)       FUN003
089400000000     C                   MOVEL     FUV(22)       FUN004
089500000000     C                   MOVEL     *BLANKS       FUN005
089600000000     C                   MOVEL     *BLANKS       FUN006
089700000000     C                   END
089800000000     C*
089900000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
090000000000     C                   MOVEL     *BLANKS       FUN007
090100000000     C                   MOVEL     *BLANKS       FUN008
090200000000     C*
090300000000     C     1             DO        24            I
090400000000     C                   MOVEL     FUD(I)        FUDRIG
090500000000     C     FUDFUN        IFEQ      '24'
090600000000     C     FUDATT        ANDEQ     'S'
090700000000     C                   MOVEL     FUDDES        FUN008
090800000000     C                   END
090900000000     C                   END
091000000000     C                   ENDSR
091100000000     C*------------------------------------------------------------------------*
091200000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
091300000000     C*------------------------------------------------------------------------*
091400000000     C     FUNCON        BEGSR
091500000000     C*
091600000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
091700000000     C                   EXSR      FUNVAR
091800000000     C*
091900000000     C* PULISCO CAMPI DI LAVORO
092000000000     C                   Z-ADD     0             I                 5 0
092100000000     C                   Z-ADD     0             W                 5 0
092200000000     C*
092300000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
092400000000     C     FUNKEY        IFNE      *BLANKS
092500000000     C     1             DO        24            I
092600000000     C                   MOVEL     FUD(I)        FUDRIG
092700000000     C*
092800000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
092900000000     C     FUNKEY        IFEQ      FUDFUN
093000000000     C     FUDATT        ANDEQ     'S'
093100000000     C                   Z-ADD     I             W
093200000000     C                   END
093300000000     C                   END
093400000000     C*
093500000000     C* FUNZIONE NON ATTIVA (ERRORE)
093600000000     C     W             IFEQ      0
093700000000     C*
093800000000     C* EMISSIONE MESSAGGIO DI ERRORE
093900000000     C                   MOVEL     MSG(1)        DSMSMS
094000000000     C*
094100000000     C                   END
094200000000     C                   END
094300000000     C*
094400000000     C* FUNZIONE ATTIVA (PROCEDI ...)
094500000000     C     W             IFNE      0
094600000000     C     FUNKEY        OREQ      *BLANKS
094700000000     C*
094800000000     C     FUNKEY        IFEQ      *BLANKS
094900000000     C                   EXSR      ENTGES
095000000000     C                   END
095100000000     C     FUNKEY        IFEQ      '01'
095200000000     C                   EXSR      F01GES
095300000000     C                   END
095400000000     C     FUNKEY        IFEQ      '03'
095500000000     C                   EXSR      F03GES
095600000000     C                   END
095700000000     C     FUNKEY        IFEQ      '06'
095800000000     C                   EXSR      F06GES
095900000000     C                   END
096000000000     C     FUNKEY        IFEQ      '07'
096100000000     C                   EXSR      F07GES
096200000000     C                   END
096300000000     C     FUNKEY        IFEQ      '08'
096400000000     C                   EXSR      F08GES
096500000000     C                   END
096600000000     C     FUNKEY        IFEQ      '12'
096700000000     C                   EXSR      F12GES
096800000000     C                   END
096900110329     C                   If        FUNkey = '18'
097000110329     C                   exsr      F18ges
097100110329     C                   EndIf
097200000000     C     FUNKEY        IFEQ      '21'
097300000000     C                   EXSR      F21GES
097400000000     C                   END
097500000000     C     FUNKEY        IFEQ      '22'
097600000000     C                   EXSR      F22GES
097700000000     C                   END
097800000000     C     FUNKEY        IFEQ      '24'
097900000000     C                   EXSR      F24GES
098000000000     C                   END
098100000000     C     FUNKEY        IFEQ      'HP'
098200000000     C                   EXSR      HELGES
098300000000     C                   END
098400000000     C*
098500000000     C                   END
098600000000     C*
098700000000     C                   ENDSR
098800000000     C*------------------------------------------------------------------------*
098900000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
099000000000     C*------------------------------------------------------------------------*
099100000000     C     FUNVAR        BEGSR
099200000000     C*
099300000000     C* INIZIALIZZA VARIABILE -FUNKEY-
099400000000     C*
099500000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
099600000000     C     KEY           IFEQ      ENTER
099700000000     C                   MOVEL     *BLANKS       FUNKEY
099800000000     C                   END
099900000000     C     KEY           IFEQ      F1
100000000000     C                   MOVEL     '01'          FUNKEY
100100000000     C                   END
100200000000     C     KEY           IFEQ      F2
100300000000     C                   MOVEL     '02'          FUNKEY
100400000000     C                   END
100500000000     C     KEY           IFEQ      F3
100600000000     C                   MOVEL     '03'          FUNKEY
100700000000     C                   END
100800000000     C     KEY           IFEQ      F4
100900000000     C                   MOVEL     '04'          FUNKEY
101000000000     C                   END
101100000000     C     KEY           IFEQ      F5
101200000000     C                   MOVEL     '05'          FUNKEY
101300000000     C                   END
101400000000     C     KEY           IFEQ      F6
101500000000     C                   MOVEL     '06'          FUNKEY
101600000000     C                   END
101700000000     C     KEY           IFEQ      F7
101800000000     C                   MOVEL     '07'          FUNKEY
101900000000     C                   END
102000000000     C     KEY           IFEQ      F8
102100000000     C                   MOVEL     '08'          FUNKEY
102200000000     C                   END
102300000000     C     KEY           IFEQ      F9
102400000000     C                   MOVEL     '09'          FUNKEY
102500000000     C                   END
102600000000     C     KEY           IFEQ      F10
102700000000     C                   MOVEL     '10'          FUNKEY
102800000000     C                   END
102900000000     C     KEY           IFEQ      F11
103000000000     C                   MOVEL     '11'          FUNKEY
103100000000     C                   END
103200000000     C     KEY           IFEQ      F12
103300000000     C                   MOVEL     '12'          FUNKEY
103400000000     C                   END
103500000000     C     KEY           IFEQ      F13
103600000000     C                   MOVEL     '13'          FUNKEY
103700000000     C                   END
103800000000     C     KEY           IFEQ      F14
103900000000     C                   MOVEL     '14'          FUNKEY
104000000000     C                   END
104100000000     C     KEY           IFEQ      F15
104200000000     C                   MOVEL     '15'          FUNKEY
104300000000     C                   END
104400000000     C     KEY           IFEQ      F16
104500000000     C                   MOVEL     '16'          FUNKEY
104600000000     C                   END
104700000000     C     KEY           IFEQ      F17
104800000000     C                   MOVEL     '17'          FUNKEY
104900000000     C                   END
105000000000     C     KEY           IFEQ      F18
105100000000     C                   MOVEL     '18'          FUNKEY
105200000000     C                   END
105300000000     C     KEY           IFEQ      F19
105400000000     C                   MOVEL     '19'          FUNKEY
105500000000     C                   END
105600000000     C     KEY           IFEQ      F20
105700000000     C                   MOVEL     '20'          FUNKEY
105800000000     C                   END
105900000000     C     KEY           IFEQ      F21
106000000000     C                   MOVEL     '21'          FUNKEY
106100000000     C                   END
106200000000     C     KEY           IFEQ      F22
106300000000     C                   MOVEL     '22'          FUNKEY
106400000000     C                   END
106500000000     C     KEY           IFEQ      F23
106600000000     C                   MOVEL     '23'          FUNKEY
106700000000     C                   END
106800000000     C     KEY           IFEQ      F24
106900000000     C                   MOVEL     '24'          FUNKEY
107000000000     C                   END
107100000000     C     KEY           IFEQ      ROLLUP
107200000000     C                   MOVEL     'RU'          FUNKEY
107300000000     C                   END
107400000000     C     KEY           IFEQ      HELP
107500000000     C                   MOVEL     'HP'          FUNKEY
107600000000     C                   END
107700000000     C                   ENDSR
107800000000     C*------------------------------------------------------------------------*
107900000000     C* NOTE PER COMPILAZIONE SCHIERE
108000000000     C*------------------------------------------------------------------------*
108100000000     C* -MSG-
108200000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
108300000000     C* (NON TOCCARE MSG,1)
108400000000     C*
108500000000     C* -CMD-
108600000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
108700000000     C*
108800000000     C* -FUD-
108900000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
109000000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
109100000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
109200000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
109300000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
109400000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
109500000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
109600000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
109700000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
109800000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
109900000000     C*
110000000000     C*------------------------------------------------------------------------*
110100000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
110200080317FNLSF1R- Tasto funzionale non gestito.                                         1
110300080317FNLSF1R- Percentuale non accettabile.                                          2
110400080317FNLSF1R- Data spedizione mancante o errata.                                    3
110500080317FNLSF1R- Data spedizione dal > Data spedzione al                               4
110600080320FNLSF1R- Periodo dal/al non elaborabile:dati disponibili dal                   5
110700080317FNLSF1R-                                                                       6
110800080317FNLSF1R- Range non valido, % iniziale > % finale.                              7
110900080317FNLSF1R- Immettere 'S' se si vogliono includere i volumi "reali" nella stampa. 8
111000080317FNLSF1R- Percentuale iniziale non accettabile.                         .       9
111100080317FNLSF1R- Percentuale finale non accettabile.                           io.    10
111200080317FNLSF1R- Immettere 'S' per rigenerare il file e 'N' per non rigenerare.       11
111300080317FNLSF1R- Divisione non accettabile.                                           12
111400080317FNLSF1R- Area non accettabile.                                                13
111500080317FNLSF1R- Terminal di partenza errato o mancante                               14
111600080317FNLSF1R- Percentuale non accettabile, valori corretti da 0 a 100.             15
111700080317FNLSF1R- Data non valida.                                                     16
111800080317FNLSF1R- Parametro non accettabile se scelto lancio per saldi.                17
111900080317FNLSF1R- Parametro non accettabile se scelto lancio estemporaneo.             18
112000080317FNLSF1R- Tipo lancio non accettabile. Inserire 'S'.                           19
112100080317FNLSF1R- Non si possono lanciare le statistiche di gruppo in SDI !!!          20
112200080320FNLSF1R- La filiale non è un terminal di partenza:statistica non elaborabile  21
112300080320FNLSF1R- Non presente impianto rilevazione peso per il terminal indicato      22
112400090707FNLSF1R- Immettere una filiale di partenza dell'area del terminal indicato    23
112500090710FNLSF1R- Filiale di partenza errata                                           24
112600000000** ======== SCHIERA: CMD  (COMANDI)                        ================
112700000000WRKSPLF
112800000000
112900000000
113000000000
113100000000
113200000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
113300000000                   -        (S/N)=N                        funz.01¬
113400000000                   -        (S/N)=N                        funz.02¬
113500000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
113600000000                   -        (S/N)=N                        funz.04¬
113700000000                   -        (S/N)=N                        funz.05¬
113800000000F06=Lancio         -        (S/N)=S                        funz.06¬
113900000000                   -        (S/N)=N                        funz.07¬
114000000000                   -        (S/N)=N                        funz.08¬
114100000000                   -        (S/N)=N                        funz.09¬
114200000000                   -        (S/N)=N                        funz.10¬
114300000000                   -        (S/N)=N                        funz.11¬
114400000000F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
114500000000                   -        (S/N)=N                        funz.13¬
114600000000                   -        (S/N)=N                        funz.14¬
114700000000                   -        (S/N)=N                        funz.15¬
114800000000                   -        (S/N)=N                        funz.16¬
114900000000                   -        (S/N)=N                        funz.17¬
115000110329F18=Lancio interatt-        (S/N)=N  <== S-Utenti EDP*     funz.18¬
115100000000                   -        (S/N)=N                        funz.19¬
115200000000                   -        (S/N)=N                        funz.20¬
115300000000                   -        (S/N)=N                        funz.21¬
115400000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
115500000000                   -        (S/N)=N                        funz.23¬
115600000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
