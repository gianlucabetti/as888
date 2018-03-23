000100000000     H*------------------------------------------------------------------------*
000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300000000     F*------------------------------------------------------------------------*
000400000000     F* DATA BASE
000500000000     F*------------------------------------------------------------------------*
000600040416     FFNLG22D   CF   E             WORKSTN INFDS(DEVDS1)
000700040416     F                                     SFILE(FG22DMS:MSGULT)
000800000000     D*------------------------------------------------------------------------*
000900000000     D* SCHIERE
001000000000     D*------------------------------------------------------------------------*
001100000000     D*-------------------
001200000000     D* MESSAGGI
001300000000     D*-------------------
001400000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001500000000     D*-------------------
001600000000     D* COMANDI
001700000000     D*-------------------
001800000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
001900000000     D*-------------------
002000000000     D* DESCRIZIONI TASTI FUNZIONALI
002100000000     D*-------------------
002200000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002300000000     D*-------------------
002400000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
002500000000     D*-------------------
002600000000     D FUV             S             19    DIM(24)
002700000000     D*------------------------------------------------------------------------*
002800000000     D* INPUT
002900000000     D*------------------------------------------------------------------------*
003000000000     D* RIDEFINIZIONE SCHIERA -FUD-
003100040416     D*------------------------------------------------------------------------*
003200000000     D FUDDS           DS
003300000000     D*                -DESCRIZIONE PARZIALE
003400000000     D  FUDDE1                 1      1
003500000000     D*                -FUNZIONE
003600000000     D  FUDFUN                 2      3
003700000000     D*                -DESCRIZIONE PARZIALE
003800000000     D  FUDDE2                 4     19
003900000000     D*                -FILLER
004000000000     D  FUDFI0                20     34
004100000000     D*                -FLAG FUNZ.ATTIVA (S/N)
004200000000     D  FUDATT                35     35
004300000000     D*                -FILLER
004400000000     D  FUDFI1                36     80
004500000000     D*                -DESCRIZIONE COMPLETA
004600000000     D  FUDDES                 1     19
004700000000     D*                -RIDEFINIZIONE RIGA
004800000000     D  FUDRIG                 1     80
004900040416     D*------------------------------------------------------------------------*
005000000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
005100040416     D*------------------------------------------------------------------------*
005200000000     D DSMSG         E DS
005300000000     D*-------------------
005400020917     D* REPERIMENTO DATI UTENTE
005500000000     D*-------------------
005600020917     D TIBS34DS      E DS
005700020917     D DDATIUTE      E DS
005800020917     D AZUTEDS       E DS                  extname(AZUTE00F)
005900970401     D*----------------
006000000000     D* DS PARAMETRI DI LANCIO PGM
006100000000     D*-------------------
006200950703     D PARAM           DS
006300040416     D  PARFGS                 1      3  0
006400040416     D  PARDDA                 4     11  0
006500040416     D  PARADA                12     19  0
006600040416     D*-------------------
006700040416     D datacom         s               d   datfmt(*iso)
006800040416     D dataiso         s               d   datfmt(*iso)
006900040416     D dataeur         s               d   datfmt(*eur)
007000000000     D*-------------------
007100000000     D* DS PER GESTIONE TASTI FUNZIONALI
007200000000     D*-------------------
007300000000     D DSKEY         E DS
007400000000     D DEVDS1          DS
007500000000     D  KEY                  369    369
007600000000     D*-------------------
007700000000     D* ARCHITETTURA
007800000000     D*-------------------
007900000000     D KPJBA         E DS
008000000000     C*------------------------------------------------------------------------*
008100000000     C* MAIN LINE
008200000000     C*------------------------------------------------------------------------*
008300000000     C*
008400000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
008500000000     C     ERRL00        IFEQ      '0'
008600000000     C*
008700000000     C* CICLO FINO A CHE:
008800000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
008900000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
009000000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
009100000000     C     F03L00        DOUEQ     '1'
009200000000     C     F12L00        OREQ      '1'
009300000000     C     FLFINE        OREQ      '1'
009400000000     C*
009500000000     C* EMETTE FORMATO VIDEO
009600000000     C                   EXSR      BUTFMR
009700000000     C*
009800000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
009900000000     C                   EXSR      FUNCON
010000000000     C*
010100000000     C                   END
010200000000     C*
010300000000     C                   END
010400000000     C*
010500000000     C                   SETON                                        LR
010600000000     C*------------------------------------------------------------------------*
010700000000     C* EMISSIONE FORMATO A VIDEO
010800000000     C*------------------------------------------------------------------------*
010900000000     C     BUTFMR        BEGSR
011000000000     C*
011100000000     C* WRITE RECORD TESTATA
011200040416     C                   WRITE     FG22D01
011300000000     C*
011400000000     C* VISUALIZZAZIONE MESSAGGIO
011500000000     C                   EXSR      SNDMSG
011600000000     C*
011700000000     C* WRITE RECORD FUNZIONI
011800040416     C                   WRITE     FG22D02
011900991019     C*
012000991019     C* EXFMT RECORD DETTAGLIO
012100040416     C                   EXFMT     FG22D03
012200991019     C*
012300000000     C* AZZERO FLAG DI ERRORE LIVELLO
012400000000     C                   MOVEL     '0'           ERRL00
012500000000     C*
012600000000     C* AZZERO FLAG DI ERRORE
012700991025     C                   SETOFF                                         7071
012800000000     C                   SETOFF                                       727377
012900000000     C                   SETOFF                                       787983
013000000000     C                   SETOFF                                       848586
013100950703     C                   SETOFF                                       818082
013200000000     C*
013300000000     C                   ENDSR
013400000000     C*------------------------------------------------------------------------*
013500000000     C* SNDMSG - MANDA MESSAGGIO
013600000000     C*------------------------------------------------------------------------*
013700000000     C     SNDMSG        BEGSR
013800000000     C*
013900000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
014000000000     C                   MOVEL     '1'           *IN40
014100040416     C                   WRITE     FG22DMC
014200000000     C                   MOVEL     '0'           *IN40
014300000000     C*
014400000000     C* VISUALIZZAZIONE MESSAGGIO
014500000000     C                   Z-ADD     0             MSGULT            4 0
014600000000     C                   CALL      'XMSG'        XMSGPA
014700000000     C     DSMSER        IFEQ      0
014800000000     C                   MOVEL     DSMSKE        MSGKEY
014900000000     C                   END
015000000000     C                   Z-ADD     1             MSGULT
015100040416     C                   WRITE     FG22DMS
015200040416     C                   WRITE     FG22DMC
015300000000     C*
015400000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
015500000000     C                   MOVEL     *BLANKS       DSMSMS
015600000000     C*
015700000000     C                   ENDSR
015800000000     C*------------------------------------------------------------------------*
015900000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
016000000000     C*------------------------------------------------------------------------*
016100000000     C     ENTGES        BEGSR
016200000000     C*
016300000000     C* CONTROLLA I CAMPI IMMESSI
016400000000     C                   EXSR      DECODI
016500000000     C*
016600000000     C                   ENDSR
016700000000     C*------------------------------------------------------------------------*
016800000000     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
016900000000     C*------------------------------------------------------------------------*
017000000000     C     F01GES        BEGSR
017100000000     C                   ENDSR
017200000000     C*------------------------------------------------------------------------*
017300000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
017400000000     C*------------------------------------------------------------------------*
017500000000     C     F03GES        BEGSR
017600000000     C*
017700000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
017800000000     C                   MOVEL     '1'           F03L00
017900000000     C*
018000000000     C                   ENDSR
018100000000     C*------------------------------------------------------------------------*
018200000000     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
018300000000     C*------------------------------------------------------------------------*
018400991020     C     F06GES        BEGSR
018500000000     C*
018600991105     C                   EXSR      DECODI
018700991105     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
018800991105     C     ERRL00        IFEQ      '0'
018900991105     C*
019000000000     C* LANCIO IL PROGRAMMA DI STAMPA
019100040416     C                   MOVEL     'LG23'        KCOAZ
019200000000     C                   EXSR      VIDPAR
019300950703     C                   MOVEL     PARAM         KPJBU
019400040423     c                   if        knmus = *all'1'
019500040423 MOM c                   CALL      'FNLG23C'
019600040423     C                   PARM                    KPJBA
019700040423     c                   else
019800040312     C                   CALL      'BCH10'
019900991021     C                   PARM                    KPJBA
020000040423     c                   end
020100040312     C*
020200040312     C*  esce dopo aver sottomesso l'elaborazione
020300040312     C* IMPOSTO FLAG F03 DI LIVELLO PGM
020400040312     C                   MOVEL     '1'           F03L00
020500991123     C*
020600991105     C                   ENDIF
020700991105     C*
020800000000     C                   ENDSR
020900000000     C*------------------------------------------------------------------------*
021000000000     C* F07GES -GESTIONE TASTO FUNZIONALE F07 -
021100000000     C*------------------------------------------------------------------------*
021200000000     C     F07GES        BEGSR
021300000000     C                   ENDSR
021400000000     C*------------------------------------------------------------------------*
021500000000     C* F08GES -GESTIONE TASTO FUNZIONALE F08 -
021600000000     C*------------------------------------------------------------------------*
021700000000     C     F08GES        BEGSR
021800000000     C                   ENDSR
021900000000     C*------------------------------------------------------------------------*
022000000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
022100000000     C*------------------------------------------------------------------------*
022200000000     C     F12GES        BEGSR
022300000000     C*
022400000000     C* IMPOSTO FLAG DI LIVELLO
022500000000     C                   MOVEL     '1'           F12L00
022600000000     C*
022700000000     C                   ENDSR
022800000000     C*------------------------------------------------------------------------*
022900000000     C* F21GES -GESTIONE TASTO FUNZIONALE F21 -
023000000000     C*------------------------------------------------------------------------*
023100000000     C     F21GES        BEGSR
023200000000     C                   ENDSR
023300000000     C*------------------------------------------------------------------------*
023400000000     C* F22GES -GESTIONE TASTO FUNZIONALE F22 - GEST.STAMPE
023500000000     C*------------------------------------------------------------------------*
023600000000     C     F22GES        BEGSR
023700000000     C*
023800991020     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
023900000000     C                   Z-ADD     80            LENGH            15 5
024000000000     C                   CALL      'QCAEXEC'
024100000000     C                   PARM                    CMD(1)
024200000000     C                   PARM                    LENGH
024300000000     C*
024400000000     C                   ENDSR
024500000000     C*------------------------------------------------------------------------*
024600991020     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
024700000000     C*------------------------------------------------------------------------*
024800000000     C     F24GES        BEGSR
024900000000     C*
025000000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
025100000000     C                   EXSR      FUNGES
025200000000     C*
025300000000     C                   ENDSR
025400000000     C*------------------------------------------------------------------------*
025500000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
025600000000     C*------------------------------------------------------------------------*
025700000000     C     HELGES        BEGSR
025800000000     C                   ENDSR
025900991020     C*------------------------------------------------------------------------*
026000991105     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
026100000000     C*------------------------------------------------------------------------*
026200000000     C     DECODI        BEGSR
026300000000     C*
026400000000     C* CONTROLLO ANNO
026500991020     C     ERRL00        IFEQ      '0'
026600040416     C     VIDfgs        IFEQ      *ZEROS
026700000000     C                   MOVEL     '1'           ERRL00
026800040416     C                   MOVEL     MSG(2)        DSMSMS
026900000000     C                   SETON                                        70
027000991020     C                   ELSE
027100991020     C                   END
027200991020     C                   END
027300000000     C*
027400040416     C* controllo date da - a / range
027500040416     c* data da
027600040416     C     ERRL00        IFEQ      '0'
027700040416     c                   if        viddda <> 0
027800040416     c     *eur          test(d)                 viddda                 71
027900040416     c                   if        *in71
028000040416     c     *dmy          test(d)                 viddda                 71
028100040416     c  n71*dmy          move      viddda        dataeur
028200040416     c  n71              move      dataeur       viddda
028300040416     c                   end
028400040416     c                   else
028500040416     c                   seton                                        71
028600040416     c                   end
028700040416     C   71              MOVEL     '1'           ERRL00
028800040416     C   71              MOVEL     MSG(3)        DSMSMS
028900040416     c                   END
029000040416     c* data al
029100040416     C     ERRL00        IFEQ      '0'
029200040416     c                   if        vidada <> 0
029300040416     c     *eur          test(d)                 vidada                 72
029400040416     c                   if        *in72
029500040416     c     *dmy          test(d)                 vidada                 72
029600040416     c  n72*dmy          move      vidada        dataeur
029700040416     c  n72              move      dataeur       vidada
029800040416     c                   end
029900040416     c                   else
030000040416     c                   move      viddda        vidada
030100040416     c                   end
030200040416     C   72              MOVEL     '1'           ERRL00
030300040416     C   72              MOVEL     MSG(3)        DSMSMS
030400040416     c                   END
030500040416     c
030600040416     C     ERRL00        IFEQ      '0'
030700040416     c                   movel     viddda        dataeur
030800040416     c                   movel     dataeur       dataiso
030900040416     c                   movel     dataiso       wdatda            8 0
031000040416     c                   movel     vidada        dataeur
031100040416     c                   movel     dataeur       dataiso
031200040416     c                   movel     dataiso       wdataa            8 0
031300040416      *  Controllo limiti due date
031400040416     C     WDATDA        IFGT      WDATAA
031500040416     C                   SETON                                        7172
031600040416     C                   MOVEL     '1'           ERRL00
031700040416     C                   MOVEL     MSG(4)        DSMSMS
031800040416     C                   END
031900040416     C                   END
032000000000     C*
032100000000     C                   ENDSR
032200000000     C*------------------------------------------------------------------------*
032300000000     C*  AZZVID - AZZERA O ABBLENCA O IMPOSTA TUTTI I CAMPI DEL FILE VIDEO
032400000000     C*------------------------------------------------------------------------*
032500000000     C     AZZVID        BEGSR
032600000000     C*
032700040416     C                   Z-ADD     *ZEROS        VIDFGS
032800040416     C                   Z-ADD     *ZEROS        VIDDDA
032900040416     C                   Z-ADD     *ZEROS        VIDADA
033000000000     C*
033100040416     C                   move      *date         VIDdda
033200040416     C                   move      *date         VIDada
033300040312     C*
033400000000     C                   ENDSR
033500000000     C*------------------------------------------------------------------------*
033600991020     C*  VIDPAR - IMPOSTA I PARAMETRI DI LANCIO DAL VIDEO
033700000000     C*------------------------------------------------------------------------*
033800000000     C     VIDPAR        BEGSR
033900000000     C*
034000040416     C                   Z-ADD     vidFGS        PARfgs
034100040416     c                   move      wdatda        PARdda
034200040416     c                   move      wdataa        PARada
034300991214     C*
034400000000     C                   ENDSR
034500020917     C*-------------------------------------------------------------------------
034600020917     C* REPERISCE I DATI UTENTE
034700020917     C*-------------------------------------------------------------------------
034800020917     C     REPDATIUTE    BEGSR
034900020917     C*
035000020917     C* INIZIALIZZA VARIABILI DI WRK
035100020917     C                   CLEAR                   TIBS34DS
035200020917     C                   CLEAR                   AZUTEDS
035300020917     C                   CLEAR                   DDATIUTE
035400020917     C*
035500020917     C     *DTAARA       DEFINE    §azute        azuteds
035600020917     C     *DTAARA       DEFINE    §datiute      ddatiute
035700020917     C                   IN(E)     *DTAARA
035800020917if  1C                   IF        %Error
035900020917     c                   EVAL      I34Tla = 'L'
036000020917     C                   CALL      'TIBS34R'
036100020917     C                   PARM                    Tibs34Ds
036200020917     C                   IN        *DTAARA
036300020917e   1C                   ENDIF
036400020917     C*
036500020917     C                   ENDSR
036600000000     C*------------------------------------------------------------------------*
036700980403     C* *INZSR - OPERAZIONI INIZIALI
036800000000     C*------------------------------------------------------------------------*
036900980403     C     *INZSR        BEGSR
037000000000     C*
037100000000     C* RICEVIMENTO PARAMETRI
037200000000     C     *ENTRY        PLIST
037300000000     C                   PARM                    KPJBA
037400980403     C*
037500000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
037600000000     C     XMSGPA        PLIST
037700000000     C                   PARM                    DSMSMS
037800000000     C                   PARM                    DSMSPG
037900000000     C                   PARM                    DSMSKE
038000000000     C                   PARM                    DSMSER
038100000000     C*
038200000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
038300000000     C                   CALL      'XKEY'
038400000000     C                   PARM                    DSKEY
038500000000     C*
038600000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
038700000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
038800000000     C*
038900000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
039000000000     C*   -NOME PGM CHIAMANTE
039100000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
039200040416     C                   MOVEL     'FNLG22R'     DSMSPG
039300040416     C                   MOVEL     'FNLG22R'     MSGQUE
039400000000     C                   MOVEL     *BLANKS       MSGKEY
039500000000     C                   MOVEL     *BLANKS       DSMSMS
039600000000     C*
039700000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
039800000000     C                   Z-ADD     0             FUNGRU            5 0
039900000000     C*
040000000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
040100000000     C                   EXSR      FUNGES
040200000000     C*
040300000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
040400000000     C                   MOVEL     '0'           F12L00            1
040500000000     C                   MOVEL     '0'           F03L00            1
040600000000     C                   MOVEL     '0'           FINL00            1
040700000000     C                   MOVEL     '0'           ERRL00            1
040800000000     C                   MOVEL     '0'           FLFINE            1
040900000000     C*
041000000000     C* LANCIO ROUTINE PER VERIFICARE SE
041100000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
041200000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
041300020917     C                   EXSR      REPDATIUTE
041400020917     C                   Z-ADD     1             CODUT             1 0
041500000000     C*
041600000000     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
041700000000     C                   EXSR      AZZVID
041800000000     C*
041900000000     C                   ENDSR
042000000000     C*------------------------------------------------------------------------*
042100000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
042200000000     C*------------------------------------------------------------------------*
042300000000     C     FUNGES        BEGSR
042400000000     C*
042500000000     C* PULISCO CAMPI DI LAVORO
042600000000     C                   Z-ADD     0             W                 5 0
042700000000     C*
042800000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
042900000000     C* E CARICO TABELLA FUNZIONI ATTIVE
043000000000     C     1             DO        24            I
043100000000     C                   MOVEL     FUD(I)        FUDRIG
043200000000     C*
043300000000     C* PULIZIA FUNZIONI PRECEDENTI
043400000000     C                   MOVEL     *BLANKS       FUV(I)
043500000000     C*
043600000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
043700000000     C* (SALTA F24)
043800000000     C     FUDATT        IFEQ      'S'
043900000000     C     FUDFUN        ANDNE     '24'
044000000000     C                   ADD       1             W
044100000000     C                   MOVEL     FUDDES        FUV(W)
044200000000     C                   END
044300000000     C                   END
044400000000     C*
044500000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
044600000000     C* ALTRIMENTI DISATTIVO F24
044700000000     C     1             DO        24            I
044800000000     C                   MOVEL     FUD(I)        FUDRIG
044900000000     C*
045000000000     C     FUDFUN        IFEQ      '24'
045100000000     C     W             IFGT      6
045200000000     C                   MOVEL     'S'           FUDATT
045300000000     C                   ELSE
045400000000     C                   MOVEL     *BLANKS       FUDATT
045500000000     C                   END
045600000000     C                   MOVEL     FUDRIG        FUD(I)
045700000000     C                   END
045800000000     C                   END
045900000000     C*
046000000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
046100000000     C     W             DIV       6             I
046200000000     C                   MVR                     Z
046300000000     C*
046400000000     C     Z             IFNE      0
046500000000     C     I             ADD       1             Z
046600000000     C                   ELSE
046700000000     C                   Z-ADD     I             Z                 5 0
046800000000     C                   END
046900000000     C*
047000000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
047100000000     C                   ADD       1             FUNGRU
047200000000     C     FUNGRU        IFGT      Z
047300000000     C     FUNGRU        ORLT      1
047400000000     C                   Z-ADD     1             FUNGRU
047500000000     C                   END
047600000000     C*
047700000000     C* PRIMO GRUPPO DI 6 FUNZIONI
047800000000     C     FUNGRU        IFEQ      1
047900000000     C                   MOVEL     FUV(1)        FUN001
048000000000     C                   MOVEL     FUV(2)        FUN002
048100000000     C                   MOVEL     FUV(3)        FUN003
048200000000     C                   MOVEL     FUV(4)        FUN004
048300000000     C                   MOVEL     FUV(5)        FUN005
048400000000     C                   MOVEL     FUV(6)        FUN006
048500000000     C                   END
048600000000     C*
048700000000     C* SECONDO GRUPPO DI 6 FUNZIONI
048800000000     C     FUNGRU        IFEQ      2
048900000000     C                   MOVEL     FUV(7)        FUN001
049000000000     C                   MOVEL     FUV(8)        FUN002
049100000000     C                   MOVEL     FUV(9)        FUN003
049200000000     C                   MOVEL     FUV(10)       FUN004
049300000000     C                   MOVEL     FUV(11)       FUN005
049400000000     C                   MOVEL     FUV(12)       FUN006
049500000000     C                   END
049600000000     C*
049700000000     C* TERZO GRUPPO DI 6 FUNZIONI
049800000000     C     FUNGRU        IFEQ      3
049900000000     C                   MOVEL     FUV(13)       FUN001
050000000000     C                   MOVEL     FUV(14)       FUN002
050100000000     C                   MOVEL     FUV(15)       FUN003
050200000000     C                   MOVEL     FUV(16)       FUN004
050300000000     C                   MOVEL     FUV(17)       FUN005
050400000000     C                   MOVEL     FUV(18)       FUN006
050500000000     C                   END
050600000000     C*
050700000000     C* QUARTO GRUPPO DI 6 FUNZIONI
050800000000     C     FUNGRU        IFEQ      4
050900000000     C                   MOVEL     FUV(19)       FUN001
051000000000     C                   MOVEL     FUV(20)       FUN002
051100000000     C                   MOVEL     FUV(21)       FUN003
051200000000     C                   MOVEL     FUV(22)       FUN004
051300000000     C                   MOVEL     *BLANKS       FUN005
051400000000     C                   MOVEL     *BLANKS       FUN006
051500000000     C                   END
051600000000     C*
051700000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
051800000000     C                   MOVEL     *BLANKS       FUN007
051900000000     C                   MOVEL     *BLANKS       FUN008
052000000000     C*
052100000000     C     1             DO        24            I
052200000000     C                   MOVEL     FUD(I)        FUDRIG
052300000000     C     FUDFUN        IFEQ      '24'
052400000000     C     FUDATT        ANDEQ     'S'
052500000000     C                   MOVEL     FUDDES        FUN008
052600000000     C                   END
052700000000     C                   END
052800000000     C                   ENDSR
052900000000     C*------------------------------------------------------------------------*
053000000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
053100000000     C*------------------------------------------------------------------------*
053200000000     C     FUNCON        BEGSR
053300000000     C*
053400000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
053500000000     C                   EXSR      FUNVAR
053600000000     C*
053700000000     C* PULISCO CAMPI DI LAVORO
053800000000     C                   Z-ADD     0             I                 5 0
053900000000     C                   Z-ADD     0             W                 5 0
054000000000     C*
054100000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
054200000000     C     FUNKEY        IFNE      *BLANKS
054300000000     C     1             DO        24            I
054400000000     C                   MOVEL     FUD(I)        FUDRIG
054500000000     C*
054600000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
054700000000     C     FUNKEY        IFEQ      FUDFUN
054800000000     C     FUDATT        ANDEQ     'S'
054900000000     C                   Z-ADD     I             W
055000000000     C                   END
055100000000     C                   END
055200000000     C*
055300000000     C* FUNZIONE NON ATTIVA (ERRORE)
055400000000     C     W             IFEQ      0
055500000000     C*
055600000000     C* EMISSIONE MESSAGGIO DI ERRORE
055700000000     C                   MOVEL     MSG(1)        DSMSMS
055800000000     C*
055900000000     C                   END
056000000000     C                   END
056100000000     C*
056200000000     C* FUNZIONE ATTIVA (PROCEDI ...)
056300000000     C     W             IFNE      0
056400000000     C     FUNKEY        OREQ      *BLANKS
056500000000     C*
056600000000     C     FUNKEY        IFEQ      *BLANKS
056700000000     C                   EXSR      ENTGES
056800000000     C                   END
056900000000     C     FUNKEY        IFEQ      '01'
057000000000     C                   EXSR      F01GES
057100000000     C                   END
057200000000     C     FUNKEY        IFEQ      '03'
057300000000     C                   EXSR      F03GES
057400000000     C                   END
057500000000     C     FUNKEY        IFEQ      '06'
057600000000     C                   EXSR      F06GES
057700000000     C                   END
057800000000     C     FUNKEY        IFEQ      '07'
057900000000     C                   EXSR      F07GES
058000000000     C                   END
058100000000     C     FUNKEY        IFEQ      '08'
058200000000     C                   EXSR      F08GES
058300000000     C                   END
058400000000     C     FUNKEY        IFEQ      '12'
058500000000     C                   EXSR      F12GES
058600000000     C                   END
058700000000     C     FUNKEY        IFEQ      '21'
058800000000     C                   EXSR      F21GES
058900000000     C                   END
059000000000     C     FUNKEY        IFEQ      '22'
059100000000     C                   EXSR      F22GES
059200000000     C                   END
059300000000     C     FUNKEY        IFEQ      '24'
059400000000     C                   EXSR      F24GES
059500000000     C                   END
059600000000     C     FUNKEY        IFEQ      'HP'
059700000000     C                   EXSR      HELGES
059800000000     C                   END
059900000000     C*
060000000000     C                   END
060100000000     C*
060200000000     C                   ENDSR
060300000000     C*------------------------------------------------------------------------*
060400000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
060500000000     C*------------------------------------------------------------------------*
060600000000     C     FUNVAR        BEGSR
060700000000     C*
060800000000     C* INIZIALIZZA VARIABILE -FUNKEY-
060900000000     C*
061000000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
061100000000     C     KEY           IFEQ      ENTER
061200000000     C                   MOVEL     *BLANKS       FUNKEY
061300000000     C                   END
061400000000     C     KEY           IFEQ      F1
061500000000     C                   MOVEL     '01'          FUNKEY
061600000000     C                   END
061700000000     C     KEY           IFEQ      F2
061800000000     C                   MOVEL     '02'          FUNKEY
061900000000     C                   END
062000000000     C     KEY           IFEQ      F3
062100000000     C                   MOVEL     '03'          FUNKEY
062200000000     C                   END
062300000000     C     KEY           IFEQ      F4
062400000000     C                   MOVEL     '04'          FUNKEY
062500000000     C                   END
062600000000     C     KEY           IFEQ      F5
062700000000     C                   MOVEL     '05'          FUNKEY
062800000000     C                   END
062900000000     C     KEY           IFEQ      F6
063000000000     C                   MOVEL     '06'          FUNKEY
063100000000     C                   END
063200000000     C     KEY           IFEQ      F7
063300000000     C                   MOVEL     '07'          FUNKEY
063400000000     C                   END
063500000000     C     KEY           IFEQ      F8
063600000000     C                   MOVEL     '08'          FUNKEY
063700000000     C                   END
063800000000     C     KEY           IFEQ      F9
063900000000     C                   MOVEL     '09'          FUNKEY
064000000000     C                   END
064100000000     C     KEY           IFEQ      F10
064200000000     C                   MOVEL     '10'          FUNKEY
064300000000     C                   END
064400000000     C     KEY           IFEQ      F11
064500000000     C                   MOVEL     '11'          FUNKEY
064600000000     C                   END
064700000000     C     KEY           IFEQ      F12
064800000000     C                   MOVEL     '12'          FUNKEY
064900000000     C                   END
065000000000     C     KEY           IFEQ      F13
065100000000     C                   MOVEL     '13'          FUNKEY
065200000000     C                   END
065300000000     C     KEY           IFEQ      F14
065400000000     C                   MOVEL     '14'          FUNKEY
065500000000     C                   END
065600000000     C     KEY           IFEQ      F15
065700000000     C                   MOVEL     '15'          FUNKEY
065800000000     C                   END
065900000000     C     KEY           IFEQ      F16
066000000000     C                   MOVEL     '16'          FUNKEY
066100000000     C                   END
066200000000     C     KEY           IFEQ      F17
066300000000     C                   MOVEL     '17'          FUNKEY
066400000000     C                   END
066500000000     C     KEY           IFEQ      F18
066600000000     C                   MOVEL     '18'          FUNKEY
066700000000     C                   END
066800000000     C     KEY           IFEQ      F19
066900000000     C                   MOVEL     '19'          FUNKEY
067000000000     C                   END
067100000000     C     KEY           IFEQ      F20
067200000000     C                   MOVEL     '20'          FUNKEY
067300000000     C                   END
067400000000     C     KEY           IFEQ      F21
067500000000     C                   MOVEL     '21'          FUNKEY
067600000000     C                   END
067700000000     C     KEY           IFEQ      F22
067800000000     C                   MOVEL     '22'          FUNKEY
067900000000     C                   END
068000000000     C     KEY           IFEQ      F23
068100000000     C                   MOVEL     '23'          FUNKEY
068200000000     C                   END
068300000000     C     KEY           IFEQ      F24
068400000000     C                   MOVEL     '24'          FUNKEY
068500000000     C                   END
068600000000     C     KEY           IFEQ      ROLLUP
068700000000     C                   MOVEL     'RU'          FUNKEY
068800000000     C                   END
068900000000     C     KEY           IFEQ      HELP
069000000000     C                   MOVEL     'HP'          FUNKEY
069100000000     C                   END
069200000000     C                   ENDSR
069300000000     C*------------------------------------------------------------------------*
069400000000     C* NOTE PER COMPILAZIONE SCHIERE
069500000000     C*------------------------------------------------------------------------*
069600000000     C* -MSG-
069700000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
069800000000     C* (NON TOCCARE MSG,1)
069900000000     C*
070000000000     C* -CMD-
070100000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
070200000000     C*
070300000000     C* -FUD-
070400000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
070500000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
070600000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
070700000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
070800000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
070900000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
071000000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
071100000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
071200000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
071300000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
071400000000     C*
071500000000     C*------------------------------------------------------------------------*
071600000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
071700040416FNLG22R- Tasto funzionale non gestito.                                         1
071800040416FNLG22R- P.O. non valido.                                                      2
071900040416FNLG22R- Data non corretta.                                                    3
072000040416FNLG22R- Date non ordinate correttamente.                                      4
072100040416FNLG22R-                                                                       5
072200040416FNLG22R-                                                                       6
072300040416FNLG22R-                                                                       7
072400040416FNLG22R-                                                                       8
072500040416FNLG22R-                                                                       9
072600040416FNLG22R-                                                                      10
072700040416FNLG22R-                                                                      11
072800040416FNLG22R-                                                                      12
072900040416FNLG22R-                                                                      13
073000040416FNLG22R-                                                                      14
073100040416FNLG22R-                                                                      15
073200040416FNLG22R-                                                                      16
073300040416FNLG22R-                                                                      17
073400040416FNLG22R-                                                                      18
073500040416FNLG22R-                                                                      19
073600040416FNLG22R-                                                                      20
073700000000** ======== SCHIERA: CMD  (COMANDI)                        ================
073800000000WRKSPLF
073900000000
074000000000
074100000000
074200000000
074300000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
074400000000                   -        (S/N)=N                        funz.01¬
074500000000                   -        (S/N)=N                        funz.02¬
074600000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
074700000000                   -        (S/N)=N                        funz.04¬
074800000000                   -        (S/N)=N                        funz.05¬
074900000000F06=Lancio         -        (S/N)=S                        funz.06¬
075000000000                   -        (S/N)=N                        funz.07¬
075100000000                   -        (S/N)=N                        funz.08¬
075200000000                   -        (S/N)=N                        funz.09¬
075300000000                   -        (S/N)=N                        funz.10¬
075400000000                   -        (S/N)=N                        funz.11¬
075500040309F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
075600000000                   -        (S/N)=N                        funz.13¬
075700000000                   -        (S/N)=N                        funz.14¬
075800000000                   -        (S/N)=N                        funz.15¬
075900000000                   -        (S/N)=N                        funz.16¬
076000000000                   -        (S/N)=N                        funz.17¬
076100000000                   -        (S/N)=N                        funz.18¬
076200000000                   -        (S/N)=N                        funz.19¬
076300000000                   -        (S/N)=N                        funz.20¬
076400000000                   -        (S/N)=N                        funz.21¬
076500000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
076600000000                   -        (S/N)=N                        funz.23¬
076700000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
