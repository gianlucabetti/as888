000100970708     H*------------------------------------------------------------------------*
000200970708     H* TARIFFE DA CONTO ECONOMICO - CREAZIONE DETTAGLIO OFFERTA
000300970708     H*------------------------------------------------------------------------*
000400970708     H* QUESTO PROGRAMMA VIENE RICHIAMATO DA PGM CHE HANNO DAVANTI DEGLI OVRDBF
000500970708     H* CHE SOSTITUISCONO I FILE DELLE OFFERTE CON QUELLI DELLE TARIFFE.
000600970708     H* PER LEGGERE LE OFFERTE QUINDI OCCORRE USARE IL FILE DELLE TARIFFE
000700970725     H* 1° GIRO -> AGGIORNA LE COMPETENZE DEL WFILE CON IL VALORE DELLE STESSE
000800970725     H*            RAGGRUPPATO IN BASE AL TIPO DI RAPPRESENTAZIONE RICHIESTO;
000900970725     H* 2° GIRO -> NON AGGIORNA
001000970708     F*------------------------------------------------------------------------*
001100970708     H DECEDIT('0,') DATEDIT(*DMY.)
001200970708     F*------------------------------------------------------------------------*
001300970708     F* DATA BASE
001400970708     F*------------------------------------------------------------------------*
001500970708     FTABEL00F  IF   E           K DISK
001600011009     FTNTBE01L  IF   E           K DISK
001700010926     FTEETC01L  IF   E           K DISK
001800010926     FTEETS01L  IF   E           K DISK
001900991125     FTNTAM01L  IF   E           K DISK
002000990820     FTITAD04L  UF A E           K DISK
002100010926     FTEETW01L  UF   E           K DISK
002200970708     D*------------------------------------------------------------------------*
002300970708     D* SCHIERE
002400970708     D*------------------------------------------------------------------------*
002500970708     D*-------------------
002600970708     D* CODICI TASSAZIONE
002700970708     D*-------------------
002800050103     D CTS             S              2    DIM(600)                             *CODICI TASSAZIONE
002900050103     D COR             S              3    DIM(600)                             *ORDINAMENTO
003000050103     D RAP             S              2    DIM(600)                             *REGIONE
003100050103     D NAR             S              3    DIM(600)                             *NAZIONE
003200970708     D*-------------------
003300970708     D* DATI TIPO -SCAGLIONI-
003400970708     D*-------------------
003500970708     D SSGL            S             13  3 DIM(10)                              *SCAGLIONI
003600970708     D SPSC            S              7  1 DIM(10)                              *PESO
003700970708     D SCSC            S              7  1 DIM(10)                              *COLLI
003800970708     D SVSC            S              7  3 DIM(10)                              *VOLUME
003900011001     D SINL            S             13  3 DIM(10)                              *INOLTRO
004000970708     D SRPV            S              3  1 DIM(10)                              *RPV
004100970708     D*-------------------
004200970708     D* DATI SCAGLIONI     -10 SCAGLIONI-
004300970708     D*-------------------
004400970708     D*------------------------------------------------------------------------*
004500970708     D* INPUT
004600970708     D*------------------------------------------------------------------------*
004700970708     D*-------------------
004800970708     D* DATI SCAGLIONI
004900970708     D*-------------------
005000970708     D SDAT            DS                  OCCURS(10) INZ
005100050103     D  SCTS                   1   1200
005200050103     D                                     DIM(600)                             *CODICE TASSAZIONE
005300050103     D  SITR                1201   6600P 5
005400050103     D                                     DIM(600)                             *PORTO
005500050103     D  SINO                6601  12000P 5
005600050103     D                                     DIM(600)                             *INOLTRO
005700050103     D  SCOM               12001  17400P 5
005800050103     D                                     DIM(600)                             *COMPETENZA
005900970708     D*-------------------
006000970708     D* DS CODICI TASSAZIONE
006100970708     D*-------------------
006200970708     D DSCT          E DS
006300991125     D*-------------------
006400991125     D* FLAG OPERATIVI -TNTAM-
006500991125     D*-------------------
006600991125     D DSTA01        E DS
006700991125     D*-------------------
006800991125     D* CONVERSIONI IMPORTI
006900991125     D*-------------------
007000991125     D YEUR          E DS                  EXTNAME(YEURCODS)
007100970708     D*-------------------
007200970708     D* DS INFORMAZIONI PROGRAMMA
007300970708     D*-------------------
007400970708     D                SDS
007500980909     D  JOB                  244    253
007600980909     D  USER                 254    263
007700970708     D*-------------------
007800970708     D* DS DI PROCEDURA
007900970708     D*-------------------
008000970708     D DSTE00        E DS                  EXTNAME(TNTE00DS)
008100970708     D*-------------------
008200970708     D* ARCHITETTURA
008300970708     D*-------------------
008400970708     D KPJBA         E DS
008500011009     D*------------------
008600011009     D* DS LETTURA TABELLA "GED" TIPO RECORD "CE"
008700011009     D*------------------
008800011009     D DSGEDCE       E DS
008900970708     C*------------------------------------------------------------------------*
009000970708     C* MAIN LINE
009100970708     C*------------------------------------------------------------------------*
009200991125     C*
009300991125     C* CARICA LA TESTATA TARIFFA
009400991125     C                   EXSR      CARTAM
009500970708     C*
009600970708     C* CARICA I DATI TIPO -GENERALI-
009700970708     C                   EXSR      CARETC
009800970708     C*
009900970708     C* CARICA I DATI TIPO -SCAGLIONI-
010000970708     C                   EXSR      CARETS
010100970708     C*
010200970708     C* CANCELLA IL DETTAGLIO TARIFFARIO DELLA LINEA DI PARTENZA
010300970708     C                   EXSR      DELTAD
010400970725     C*
010500970725     C* AGGIORNA LA TARIFFA
010600970725     C                   EXSR      AGGTAR
010700970725     C*
010800970725     C* AGGIORNA IL WORKFILE
010900970725IF  1C     D00TUR        IFEQ      '1'                                          *SOLO 1° GIRO
011000970725     C                   EXSR      AGGETW
011100970725E   1C                   ENDIF
011200970725     C*
011300970725     C                   SETON                                        LR
011400970725     C*------------------------------------------------------------------------*
011500970725     C* AGGTAR - AGGIORNA LA TARIFFA
011600970725     C*------------------------------------------------------------------------*
011700970725     C     AGGTAR        BEGSR
011800970708     C*
011900970708     C* POSIZIONAMENTO E PRIMA LETTURA -FILE DI LAVORO-
012000970708     C                   EXSR      SETFIL
012100970708     C*
012200970708     C* CICLO FINO A FINE FILE
012300970708DO  1C     $FINE         DOWEQ     'N'
012400970708     C*
012500970708     C* OPERAZIONI PER NUOVO SCAGLIONE
012600970708     C                   EXSR      NEWSGL
012700970708     C*
012800970708     C* CICLO FINO A ROTTURA SCAGLIONE
012900970708DO  2C     $FINE         DOWEQ     'N'
013000970708     C     ETWSGL        ANDEQ     DEPSGL
013100970708     C*
013200970708     C* MEMORIZZA L'IMPORTO COMPETENZA SCAGLIONE PER CODICE TASSAZIONE
013300970708     C                   EXSR      MEMETW
013400970708     C*
013500970708     C* LETTURA SUCESSIVA -FILE DI LAVORO-
013600970708     C                   EXSR      LETFIL
013700970708E   2C                   ENDDO                                                  *FINE SCAGLIONE
013800970708     C*
013900970708     C* SCRIVE LA TARIFFA DELLO SCAGLIONE
014000970725     C                   EXSR      SCRTAR
014100970708E   1C                   ENDDO                                                  *FINE FILE
014200970708     C*
014300970725     C                   ENDSR
014400970708     C*------------------------------------------------------------------------*
014500970708     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA
014600970708     C*------------------------------------------------------------------------*
014700970708     C     SETFIL        BEGSR
014800970725     C*
014900970725     C* REIMPOSTA VARIABILI DI LAVORO
015000970725     C                   MOVEL     'N'           $FINE                          *FINE PGM
015100970708     C*
015200970708     C* POSIZIONAMENTO INIZIALE E PRIMA LETTURA
015300980909     C******************** MOVELUSER      KTWUSR
015400980909     C                   MOVEL     JOB           KTWUSR
015500980603     C                   MOVEL     D00DSF        KTWDSF
015600980603     C                   MOVEL     D00CTO        KTWCTO
015700970708     C                   Z-ADD     D00KSC        KTWKSC
015800970708     C                   Z-ADD     D00CTR        KTWCTR
015900970708     C                   Z-ADD     D00PRG        KTWPRG
016000010926     C     KEYETW        SETLL     TEETW01L                           99
016100970708     C*
016200970708     C* LETTURA RECORD IN BASE AL TIPO DI ORDINAMENTO RICHIESTO
016300970708IF  1C     *IN99         IFEQ      *OFF                                         *RECORD TROVATO
016400970708     C                   EXSR      LETFIL                                       *LETTURA RECORD
016500970708X   1C                   ELSE                                                   *FINE FILE
016600970708     C                   MOVEL     'S'           $FINE                          *FINE PGM
016700970708E   1C                   ENDIF
016800970708     C*
016900970708     C                   ENDSR
017000970708     C*------------------------------------------------------------------------*
017100970708     C* LETFIL - LETTURA SUCESSIVA RECORD
017200970708     C*------------------------------------------------------------------------*
017300970708     C     LETFIL        BEGSR
017400970708     C*
017500970708     C* LEGGE FINO A:
017600970708     C*      -FINE FILE
017700970708     C*      -TROVATO RECORD VALIDO
017800970708     C                   MOVEL     'N'           $RECOK
017900970708DO  1C     *IN99         DOWEQ     *OFF
018000970708     C     $RECOK        ANDEQ     'N'
018100010926     C     KEYETW        READE     TEETW01L                               99
018200970708     C*
018300970708IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
018400970708     C                   MOVEL     'S'           $FINE                          *FINE PGM
018500970708X   2C                   ELSE                                                   *RECORD TROVATO
018600970708     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
018700970708E   2C                   ENDIF
018800970708E   1C                   ENDDO
018900970708     C*
019000970725     C* RECORD VALIDO, MEMORIZZA CAMPI CHAIVE PER RILETTURA
019100970708     C                   MOVEL     ETWSGL        KSGL
019200970708     C                   MOVEL     ETWCTS        KCTS
019300970708     C                   MOVEL     ETWRDL        KRDL
019400970708     C*
019500970708     C                   ENDSR
019600970708     C*------------------------------------------------------------------------*
019700970708     C* CHKREC - CONTROLLA VALIDITA' RECORD
019800970708     C*------------------------------------------------------------------------*
019900970708     C     CHKREC        BEGSR
020000970708     C*
020100970708     C                   MOVEL     'S'           $RECOK                         *RECORD VALIDO
020200970708     C*
020300970708     C* RAGGRUPPAMENTO <> RAGGRUPPAMENTO PORTO, ESCLUDE
020400970708IF  1C     ETWRDL        IFNE      001
020500011023     C                   MOVEL     'N'           $RECOK                         *RECORD NON VALIDO
020600970708E   1C                   ENDIF
020700970708     C*
020800970708     C                   ENDSR
020900970708     C*------------------------------------------------------------------------*
021000970708     C* NEWSGL - OPERAZIONI PER NUOVO SCAGLIONE
021100970708     C*------------------------------------------------------------------------*
021200970708     C     NEWSGL        BEGSR
021300970708     C*
021400970708     C* MEMORIZZA IL NUOVO SCAGLIONE IN UN DEPOSITO
021500970708     C                   Z-ADD     ETWSGL        DEPSGL
021600970708     C*
021700970708     C* AZZERA LE VARIABILI DI LAVORO
021800970708     C                   Z-ADD     1             S                 5 0          *INDICE SCAGLIONE
021900970708     C                   SETOFF                                           28
022000970708     C     ETWSGL        LOOKUP    SSGL(S)                                28
022100970708IF  1C     *IN28         IFEQ      *ON
022200970708     C     S             OCCUR     SDAT
022300970708     C                   CLEAR                   SCTS                           *CODICI TASSAZIONE
022400970708     C                   CLEAR                   SITR                           *PORTO
022500970708     C                   CLEAR                   SINO                           *INOLTRO
022600970725     C                   CLEAR                   SCOM                           *COMPETENZA
022700970708E   1C                   ENDIF
022800970708     C*
022900970708     C* AZZERA LE VARIABILI DI LAVORO -CODICI TASSAZIONE-
023000970708     C                   Z-ADD     *ZEROS        C                 5 0          *INDICE C.TASSAZIONE
023100970708     C                   MOVEL     *BLANKS       DEPCTS                         *DEPOSITO CTS -%INC-
023200970708     C*
023300970708     C                   ENDSR
023400970708     C*------------------------------------------------------------------------*
023500970708     C* MEMETW -  MEMORIZZA L'IMPORTO COMPETENZA SCAGLIONE PER CODICE TASSAZIONE
023600970708     C*------------------------------------------------------------------------*
023700970708     C     MEMETW        BEGSR
023800970708     C*
023900970708     C* SI POSIZIONA SULLO SCAGLIONE DA MEMORIZZARE
024000970708     C     S             OCCUR     SDAT
024100970708     C*
024200970708     C* REPERISCE IL CODICE TASSAZIONE DA RAPPRESENTARE
024300970708IF  1C     ETWRCT        IFEQ      'T'                                          *ELEMENTARE
024400970708     C                   MOVEL     ETWCTS        WCTS
024500970708E   1C                   ENDIF
024600970708IF  1C     ETWRCT        IFEQ      'R'                                          *REGIONALE
024700970708     C                   MOVEL     ETWRAP        WCTS
024800970708E   1C                   ENDIF
024900970708IF  1C     ETWRCT        IFEQ      'N'                                          *NAZIONALE
025000970708     C                   MOVEL     ETWNAR        WCTS
025100970708E   1C                   ENDIF
025200970708     C*
025300970708     C* CALCOLA LA %INCIDENZA TOTALE SCAGLIONE DEL CTS DA RAPPRESENTARE
025400970708IF  1C     WCTS          IFNE      DEPCTS                                       *SE <> PRECEDENTE
025500970708     C                   MOVEL     WCTS          DEPCTS                         *DEPOSITI
025600970708     C                   MOVEL     ETWRCT        DEPRCT
025700970708     C                   Z-ADD     *ZEROS        TOTPPE                         *TOTALE %INCIDENZA
025800970708     C* PERCENTUALE
025900980909     C                   MOVEL     JOB           KTWUSR                         *CHIAVE LETTURA
026000980603     C                   MOVEL     D00DSF        KTWDSF
026100980603     C                   MOVEL     D00CTO        KTWCTO
026200970708     C                   Z-ADD     D00KSC        KTWKSC
026300970708     C                   Z-ADD     D00CTR        KTWCTR
026400970708     C                   Z-ADD     D00PRG        KTWPRG
026500970708     C                   Z-ADD     DEPSGL        KTWSGL
026600010926     C     KE2ETW        SETLL     TEETW01L                           98
026700010926     C     KE2ETW        READE(N)  TEETW01L                               98
026800970708DO  2C     *IN98         DOWEQ     *OFF
026900970708     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
027000970708IF  3C     $RECOK        IFEQ      'S'                                          *RECORD VALIDO
027100970708IF  4C     ETWRCT        IFEQ      DEPRCT                                       *STESSO RAGG.CTS
027200970708IF  5C     ETWRCT        IFEQ      'T'                                          *ELEMENTARE
027300970708     C                   MOVEL     ETWCTS        WCTS
027400970708E   5C                   ENDIF
027500970708IF  5C     ETWRCT        IFEQ      'R'                                          *REGIONALE
027600970708     C                   MOVEL     ETWRAP        WCTS
027700970708E   5C                   ENDIF
027800970708IF  5C     ETWRCT        IFEQ      'N'                                          *NAZIONALE
027900970708     C                   MOVEL     ETWNAR        WCTS
028000970708E   5C                   ENDIF
028100970708     C*
028200970708     C* MEMORIZZA IL TOTALE %INCIDENZA
028300970708IF  5C     WCTS          IFEQ      DEPCTS
028400970708     C     ETWPPE        ADD       TOTPPE        TOTPPE                         *TOTALE %INCIDENZA
028500970708E   5C                   ENDIF
028600970708E   4C                   ENDIF
028700970708E   3C                   ENDIF
028800010926     C     KE2ETW        READE(N)  TEETW01L                               98
028900970708E   2C                   ENDDO
029000970708     C*
029100970708     C* MEMORIZZA IL CODICE TASSAZIONE
029200970708     C                   Z-ADD     1             I
029300970708     C                   SETOFF                                           28
029400970708     C     DEPCTS        LOOKUP    SCTS(I)                                28
029500970708IF  2C     *IN28         IFEQ      *OFF                                         *MAI MEMORIZZATO
029600970708     C                   ADD       1             C
029700970708     C                   MOVEL     DEPCTS        SCTS(C)                        *CODICE TASSAZIONE
029800970708     C                   Z-ADD     C             I
029900970708E   2C                   ENDIF
030000970708E   1C                   ENDIF
030100970708     C*
030200970708     C* CALCOLA LA TARIFFA
030300970708     C                   Z-ADD     *ZEROS        WITR             17 5
030400970708     C                   Z-ADD     *ZEROS        WINO             17 5
030500970708     C                   EXSR      CALTAR
030600970708     C                   ADD       WITR          SITR(I)                        *PORTO
030700980908     C     ETCSIN        IFEQ      'N'                                          *NON INGLOBATO
030800980908     C                   Z-ADD     WINO          SINO(I)                        *INOLTRO
030900980908     C                   ELSE
031000970708     C                   ADD       WINO          SINO(I)                        *INOLTRO
031100980908     C                   ENDIF
031200970708     C*
031300970708     C                   ENDSR
031400970708     C*------------------------------------------------------------------------*
031500970708     C* CALTAR - CALCOLA LA TARIFFA
031600970708     C*------------------------------------------------------------------------*
031700970708     C     CALTAR        BEGSR
031800970708     C*
031900970708     C* RILEGGE IL REOCRD PER SPOSIZIONAMENTO
032000980909     C                   MOVEL     JOB           KTWUSR                         *CHIAVE LETTURA
032100980603     C                   MOVEL     D00DSF        KTWDSF
032200980603     C                   MOVEL     D00CTO        KTWCTO
032300970708     C                   Z-ADD     D00KSC        KTWKSC
032400970708     C                   Z-ADD     D00CTR        KTWCTR
032500970708     C                   Z-ADD     D00PRG        KTWPRG
032600970708     C                   Z-ADD     KSGL          KTWSGL
032700970708     C                   MOVEL     KCTS          KTWCTS
032800970708     C                   Z-ADD     KRDL          KTWRDL
032900010926     C     KE3ETW        CHAIN     TEETW01L                           99
033000970708     C*
033100970708     C* CALCOLA L'INOLTRO
033200970708IF  1C     ETCSIN        IFEQ      'N'                                          *NON INGLOBATO
033300970708     C                   Z-ADD     SINL(S)       WINO                           *INOLTRO
033400970708X   1C                   ELSE                                                   *INGLOBATO
033500970708IF  2C     SINL(S)       IFGT      *ZEROS
033600970708     C     SINL(S)       DIV       100           WINO             17 5
033700970708     C     WINO          MULT      ETCPPV        WINO
033800011023     C     D00TUR        IFEQ      '1'
033900980908     C                   ADD       WINO          ETWCOM                         *PORTO+INOLTRO
034000980908     C                   ENDIF
034100970708     C                   Z-ADD     *ZEROS        WINO                           *NO INOLTRO
034200970708E   2C                   ENDIF
034300970708E   1C                   ENDIF
034400970708     C*
034500970708     C* CALCOLA IL PORTO -IN BASE ALLA %INCIDENZA SGL/CTS-
034600980902IF  1C     ETWPPE        IFGT      *ZEROS
034700970708     C     ETWCOM        MULT      ETWPPE        WN247            24 7
034800970708     C     WN247         DIV       TOTPPE        WN175            17 5
034900970708     C                   Z-ADD     WN175         WITR
035000980902E   1C                   ENDIF
035100970725     C*
035200970708     C                   ENDSR
035300970708     C*------------------------------------------------------------------------*
035400970725     C* SCRTAR - SCRIVE LA TARIFFA DELLO SCAGLIONE
035500970708     C*------------------------------------------------------------------------*
035600970725     C     SCRTAR        BEGSR
035700970708     C*
035800970708     C* PER CIASCUN CODICE TASSAZIONE MEMORIZZATO
035900050103DO  1C     1             DO        600           C
036000970708IF  2C     SCTS(C)       IFNE      *BLANKS
036100990820     C                   CLEAR                   TITAD000
036200970708     C                   Z-ADD     D00KSC        TADKSC                         *NUMERO VISITA
036300970708     C                   Z-ADD     D00CTR        TADCTR                         *OFFERTA
036400970708     C                   Z-ADD     D00PRG        TADPRG                         *PROGRESSIVO
036500970708     C                   Z-ADD     D00LNP        TADLNP                         *LINEA DI PARTENZA
036600970708     C                   Z-ADD     DEPSGL        TADSGL                         *SCAGLIONE
036700970708     C                   Z-ADD     SRPV(S)       TADRPV                         *RAPPORTO PESO/VOLUM
036800970708     C                   MOVEL     SCTS(C)       TADCTS                         *CODICE TASSAZIONE
036900991125     C                   Z-ADD     SINO(C)       TADINO                         *INOLTRO
037000991125     C*
037100970708     C                   Z-ADD     1             I
037200970708     C                   SETOFF                                           28
037300970708     C     TADCTS        LOOKUP    CTS(I)                                 28
037400990820     C                   MOVE      COR(I)        TADORP                         *ORDINAMENTO (ALL Dx
037500970708     C*
037600970708     C* REPERISCE O CALCOLA IL VAORE DI RIFERIMENTO DELLA TARIFFA
037700970708     C                   Z-ADD     *ZEROS        PESTAS           13 3          *VALORE RIFERIMENTO
037800970708     C                   MOVEL     D00CTR        UNOCTR            1 0
037900970708IF  3C     UNOCTR        IFEQ      1                                            *TARIFFA A COLLI
038000970708     C                   Z-ADD     SCSC(S)       PESTAS                         *COLLI
038100970708X   3C                   ELSE                                                   *KG O QLI
038200970708     C                   EXSR      CALPT                                        *CALC. PESO TASSABIL
038300970708E   3C                   ENDIF
038400970708     C*
038500970708     C* SE VALORE > TARIFFA FINITA RENDE UNITARIO IL DETTAGLIO TARIFFARIO
038600970708IF  3C     PESTAS        IFGT      D00ATA                                       *VALORE > TAR FINITA
038700970708     C     SITR(C)       DIV       PESTAS        WN175                          *TARIFFA UNITARIA
038800970708IF  4C     UNOCTR        IFEQ      0                                            *TARIFFA A QLI
038900970708     C     WN175         MULT      100           TADITR                         *TRASFORMA TAR IN QL
039000970708X   4C                   ELSE                                                   *TARIFFA KG/COLLI
039100970708     C                   Z-ADD     WN175         TADITR
039200970708E   4C                   ENDIF
039300970708X   3C                   ELSE
039400970708     C                   Z-ADD     SITR(C)       TADITR                         *PORTO SECCO
039500970708E   3C                   ENDIF
039600011108     C*
039700011108     C* CONTROLLA SE CONVERTIRE ALCUNI CAMPI
039800011108IF  3C     TAMDIV        IFNE      WDIVCE
039900011108     C                   EXSR      CNVTAD
040000011108E   3C                   ENDIF
040100970708     C*
040200970708     C* ARROTONDA IL PORTO DELLA TARIFFA
040300011010     C                   Z-ADD     TADITR        $IMP             18 5
040400970708     C                   MOVEL     ETCFAR        $FDE              1
040500011108     C                   Z-ADD     SAVIAR        $ARR             18 5
040600970708     C                   EXSR      ARRIMP                                       *ARROTONDA IMPORTO
040700970708     C                   Z-ADD     $IMP          TADITR
040800970708     C* SCRIVE
040900011010     C     D00TUR        IFEQ      '2'
041000011023     C     TAMDIV        ANDEQ     'ITL'
041100011026     C                   EXSR      CNVINT
041200011010     C                   ENDIF
041300990820     C                   WRITE     TITAD000
041400970725     C*
041500970725     C* AGGIORNA IL CAMPO IN SCHIERA
041600011108     C                   Z-ADD     SITR(C)       SCOM(C)                        *COMPETENZA
041700011010     C                   Z-ADD     SITR(C)       $IMP             18 5
041800970725     C                   MOVEL     ETCFAR        $FDE              1
041900011010     C                   Z-ADD     ETCIAR        $ARR             18 5
042000970725     C                   EXSR      ARRIMP                                       *ARROTONDA IMPORTO
042100970725     C                   Z-ADD     $IMP          SCOM(C)
042200970725E   2C                   ENDIF
042300970708E   1C                   ENDDO                                                  *FINE CTS SCAGLIONE
042400970708     C*
042500970708     C                   ENDSR
042600011010     C*------------------------------------------------------------------------*
042700011010     C* CNVINT - TRASFORMA I CAMPI IN INTERO TRONCANDOLI
042800011010     C*------------------------------------------------------------------------*
042900011010     C     CNVINT        BEGSR
043000011010     C*
043100011010     C                   EVAL      TADINO = %int(TADINO)
043200011010     C                   EVAL      TADITR = %int(TADITR)
043300011010     C*
043400011010     C                   ENDSR
043500011023     C*------------------------------------------------------------------------*
043600011023     C* CNVTAD - CONVERTE I CAMPI DI TITAD IN DIVISA DELLA TARIFFA
043700011023     C*------------------------------------------------------------------------*
043800011023     C     CNVTAD        BEGSR
043900011023     C*
044000011026     C                   MOVEL     WDIVCE        WDVI              3
044100011023     C                   MOVEL     TAMDIV        WDVO              3
044200011023     C* IMPORTO TARIFFA
044300011023     C                   Z-ADD     TADITR        WIMI             30 9
044400011023     C                   EXSR      CNVEUR
044500011023     C                   Z-ADD     WIMO          TADITR
044600011023     C* TARIFFA INOLTRO
044700011023     C                   Z-ADD     TADINO        WIMI             30 9
044800011023     C                   EXSR      CNVEUR
044900011023     C                   Z-ADD     WIMO          TADINO
045000011023     C*
045100011023     C                   ENDSR
045200011023     C*------------------------------------------------------------------------*
045300011023     C* CNVETC - CONVERTE I CAMPI DI TEETC IN DIVISA DI CALCOLO DEL CE
045400011023     C*------------------------------------------------------------------------*
045500011023     C     CNVETC        BEGSR
045600011023     C*
045700011023     C                   MOVEL     TAMDIV        WDVI              3
045800011026     C                   MOVEL     WDIVCE        WDVO              3
045900011023     C* ARROTONDAMENTO
046000011108     C                   Z-ADD     ETCIAR        WIMI             30 9
046100011108     C                   EXSR      CNVEUR
046200011108     C                   Z-ADD     WIMO          ETCIAR
046300011023     C* CONTRASSEGNO
046400011023     C                   Z-ADD     ETCCAS        WIMI             30 9
046500011023     C                   EXSR      CNVEUR
046600011023     C                   Z-ADD     WIMO          ETCCAS
046700011023     C* ASSICURAZIONE
046800011023     C                   Z-ADD     ETCASC        WIMI             30 9
046900011023     C                   EXSR      CNVEUR
047000011023     C                   Z-ADD     WIMO          ETCASC
047100011023     C*
047200011023     C                   ENDSR
047300011023     C*------------------------------------------------------------------------*
047400011023     C* CNVETS - CONVERTE I CAMPI DI TEETS IN DIVISA DI CALCOLO DEL CE
047500011023     C*------------------------------------------------------------------------*
047600011023     C     CNVETS        BEGSR
047700011023     C*
047800011023     C                   MOVEL     TAMDIV        WDVI              3
047900011026     C                   MOVEL     WDIVCE        WDVO              3
048000011023     C* INOLTRO
048100011023     C                   Z-ADD     ETSINL        WIMI             30 9
048200011023     C                   EXSR      CNVEUR
048300011023     C                   Z-ADD     WIMO          ETSINL
048400011023     C*
048500011023     C                   ENDSR
048600991125     C*------------------------------------------------------------------------*
048700991125     C* CNVEUR - CONVERTE I CAMPI DA DIVIAS A LIRA
048800991125     C*------------------------------------------------------------------------*
048900991125     C     CNVEUR        BEGSR
049000991125     C*
049100991125     C* AZZERA VARIABILI DI LAVORO
049200991125     C                   Z-ADD     *ZEROS        WIMO             30 9          *IMPORTO CONVERTITO
049300011023     C                   MOVEL     *BLANKS       NDEC              1            *N. DEC IN OUTPUT
049400011023     C*
049500011026     C* IMPOSTO I DECIMALI DI OUTPUT A SECONDA DELLA DIVISA DI OUTPUT (NON + VERO: SEMPREE MAX DEC)
049600011026     C*                  IF        TAMDIV = 'ITL'
049700011026     C*                  EVAL      NDEC = '0'
049800011026     C*                  ELSE
049900011026     C*                  EVAL      NDEC = '3'
050000011026     C*                  ENDIF
050100011026     C                   EVAL      NDEC = '9'
050200991125     C*
050300991125IF  1C     WIMI          IFNE      *ZEROS
050400991125     C                   CLEAR                   YEUR
050500991125     C                   MOVEL     WDVI          YECDVI                         *DIVISA IMPORTO
050600991125     C                   Z-ADD     WIMI          YECIMI                         *IMPORTO DA CONVERTI
050700011023     C                   MOVEL     WDVO          YECDVO                         *DIVISA IMP CONVERTI
050800011023     C                   MOVEL     NDEC          YECDCO                         *N° DECIMALI
050900991125     C                   MOVEL     *BLANKS       YECTAR                         *ARROTONDAMENTO "H"
051000991125     C                   CALL      'YEURCO'
051100991125     C                   PARM                    YEUR
051200991125IF  2C     YECESI        IFEQ      ' '                                          *NO ERRORI
051300991125     C                   Z-ADD     YECIMO        WIMO                            -IMPORTO CONVERTITO
051400991125X   2C                   ELSE                                                   *SI ERRORI
051500991125     C                   Z-ADD     WIMI          WIMO                            -IMPOROTO DA CONVER
051600991125E   2C                   ENDIF
051700991125E   1C                   ENDIF
051800991125     C*
051900991125     C                   ENDSR
052000970725     C*------------------------------------------------------------------------*
052100970725     C* AGGETW - AGGIORNA IL WORKFILE
052200970725     C*------------------------------------------------------------------------*
052300970725     C     AGGETW        BEGSR
052400970725     C*
052500970725     C* POSIZIONAMENTO E PRIMA LETTURA -FILE DI LAVORO-
052600970725     C                   EXSR      SETFIL
052700970725     C*
052800970725     C* CICLO FINO A FINE FILE
052900970725DO  1C     $FINE         DOWEQ     'N'
053000970725     C*
053100970725     C* AGGIORNA RECORD
053200970725     C                   EXSR      SCRETW
053300970725     C*
053400970725     C* LETTURA SUCESSIVA -FILE DI LAVORO-
053500970725     C                   EXSR      LETFIL
053600970725E   1C                   ENDDO                                                  *FINE FILE
053700970725     C*
053800970725     C                   ENDSR
053900970725     C*------------------------------------------------------------------------*
054000970725     C* SCRETW - SCRIVE LA COMPETENZA DEL WORKFILE
054100970725     C*------------------------------------------------------------------------*
054200970725     C     SCRETW        BEGSR
054300970725     C*
054400970725     C* SI POSIZIONA SULLO SCAGLIONE DA AGGIORNARE
054500970725     C                   Z-ADD     1             S                              *INDICE SCAGLIONE
054600970725     C                   SETOFF                                           28
054700970725     C     ETWSGL        LOOKUP    SSGL(S)                                28
054800970725     C     S             OCCUR     SDAT
054900970725IF  1C     *IN28         IFEQ      *ON
055000970725     C*
055100970725     C* SI POSIZIONA SUL CODICE TASSAZIONE DA AGGIORNARE
055200970725IF  2C     ETWRCT        IFEQ      'T'                                          *ELEMENTARE
055300970725     C                   MOVEL     ETWCTS        WCTS
055400970725E   2C                   ENDIF
055500970725IF  2C     ETWRCT        IFEQ      'R'                                          *REGIONALE
055600970725     C                   MOVEL     ETWRAP        WCTS
055700970725E   2C                   ENDIF
055800970725IF  2C     ETWRCT        IFEQ      'N'                                          *NAZIONALE
055900970725     C                   MOVEL     ETWNAR        WCTS
056000970725E   2C                   ENDIF
056100970725     C                   Z-ADD     1             C                              *INDICE C.TASSAZIONE
056200970725     C                   SETOFF                                           28
056300970725     C     WCTS          LOOKUP    SCTS(C)                                28
056400970725IF  2C     *IN28         IFEQ      *ON
056500970725     C                   Z-ADD     SCOM(C)       ETWCOM                         *PORTO "PESATO"
056600970725     C                   EXCEPT    UPDETW
056700970725E   2C                   ENDIF
056800970725E   1C                   ENDIF
056900970725     C*
057000970725     C                   ENDSR
057100991125     C*------------------------------------------------------------------------*
057200991125     C* CARTAM - CARICA LA TESTATA TARIFFA
057300991125     C*------------------------------------------------------------------------*
057400991125     C     CARTAM        BEGSR
057500991125     C*
057600991125     C                   Z-ADD     D00KSC        KAMKSC
057700991125     C                   Z-ADD     D00CTR        KAMCTR
057800991125     C                   Z-ADD     D00PRG        KAMPRG
057900991125     C     KEYTAM        CHAIN     TNTAM01L                           99
058000991125IF  1C     *IN99         IFEQ      *OFF
058100991125     C                   MOVEL     TAMFLO        DSTA01
058200991125     C                   MOVEL     §TADIV        TAMDIV            3            *DIVISA TARIFFA
058300991125X   1C                   ELSE
058400991125     C                   MOVEL     *BLANKS       TAMDIV
058500991125E   1C                   ENDIF
058600991125     C*
058700991125     C                   ENDSR
058800970708     C*------------------------------------------------------------------------*
058900970708     C* CARETC - CARICA I DATI TIPO -GENERALI-
059000970708     C*------------------------------------------------------------------------*
059100970708     C     CARETC        BEGSR
059200970708     C*
059300980603     C                   MOVEL     D00DSF        KTCDSF
059400980603     C                   MOVEL     D00CTO        KTCCTO
059500970708     C                   Z-ADD     D00KSC        KTCKSC
059600970708     C                   Z-ADD     D00CTR        KTCCTR
059700970708     C                   Z-ADD     D00PRG        KTCPRG
059800970708     C                   Z-ADD     D00LNP        KTCLNP
059900011023     C     KEYETC        CHAIN     TEETC01L                           99
060000011108     C                   Z-ADD     ETCIAR        SAVIAR           30 9
060100011026     C     WDIVCE        IFNE      TAMDIV
060200011023     C                   EXSR      CNVETC
060300011023     C                   ENDIF
060400970708     C*
060500970708     C                   ENDSR
060600970708     C*------------------------------------------------------------------------*
060700970708     C* CARETS - CARICA I DATI TIPO -SCAGLIONI-
060800970708     C*------------------------------------------------------------------------*
060900970708     C     CARETS        BEGSR
061000970708     C*
061100970708     C                   Z-ADD     *ZEROS        I
061200980603     C                   MOVEL     D00DSF        KTSDSF
061300980603     C                   MOVEL     D00CTO        KTSCTO
061400970708     C                   Z-ADD     D00KSC        KTSKSC
061500970708     C                   Z-ADD     D00CTR        KTSCTR
061600970708     C                   Z-ADD     D00PRG        KTSPRG
061700970708     C                   Z-ADD     D00LNP        KTSLNP
061800010926     C     KEYETS        SETLL     TEETS01L                           99
061900011023     C     KEYETS        READE     TEETS01L                               99
062000970708DO  1C     *IN99         DOWEQ     *OFF
062100970708IF  2C     ETSATB        IFEQ      *BLANKS                                      *VALIDO
062200011026     C     D00DIV        IFNE      WDIVCE
062300011023     C                   MOVEL     D00DIV        WDVI
062400011023     C                   EXSR      CNVETS
062500011023     C                   ENDIF
062600011026     C                   ADD       1             I
062700970708     C                   Z-ADD     ETSSGL        SSGL(I)                        *SCAGLIONE
062800970708     C                   Z-ADD     ETSPSC        SPSC(I)                        *PESO
062900970708     C                   Z-ADD     ETSCSC        SCSC(I)                        *COLLI
063000970708     C                   Z-ADD     ETSVSC        SVSC(I)                        *VOLUME
063100970708     C                   Z-ADD     ETSINL        SINL(I)                        *INOLTRO
063200970708     C                   Z-ADD     ETSRPV        SRPV(I)                        *RAPPORTO PESO/VOLUM
063300970708E   2C                   ENDIF
063400010926     C     KEYETS        READE     TEETS01L                               99
063500970708E   1C                   ENDDO                                                  *FINE SCAGLIONI
063600970708     C*
063700970708     C                   ENDSR
063800970708     C*------------------------------------------------------------------------*
063900970708     C* DELTAD - CANCELLA IL DETTAGLIO TARIFFARIO
064000970708     C*------------------------------------------------------------------------*
064100970708     C     DELTAD        BEGSR
064200970708     C*
064300970708     C                   Z-ADD     D00KSC        KTDKSC
064400970708     C                   Z-ADD     D00CTR        KTDCTR
064500970708     C                   Z-ADD     D00PRG        KTDPRG
064600970708     C                   Z-ADD     D00LNP        KTDLNP
064700990820     C     KE2TAD        SETLL     TITAD04L
064800990820     C     KE2TAD        READE     TITAD04L                               99
064900970708DO  1C     *IN99         DOWEQ     *OFF
065000970708     C                   Z-ADD     1             I
065100970708     C                   SETOFF                                           28
065200970708     C     TADCTS        LOOKUP    CTS(I)                                 28
065300970708IF  3C     *IN28         IFEQ      *ON
065400970708IF  2C     D00FIE        IFEQ      'I'                                          *ITALIA
065500970708     C     NAR(I)        ANDEQ     'IT'                                         *SOLO CTS ITALIA
065600970708     C     D00FIE        OREQ      'E'                                          *ESTERO
065700990120     C     *IN28         OREQ      *OFF                                         *NON TROVATO
065800990820     C                   DELETE    TITAD000
065900970708E   2C                   ENDIF
066000970708E   3C                   ENDIF
066100990820     C     KE2TAD        READE     TITAD04L                               99
066200970708E   1C                   ENDDO
066300970708     C*
066400970708     C                   ENDSR
066500970708     C*------------------------------------------------------------------------*
066600970708     C* ARRIMP - ARROTONDA UN IMPORTO
066700970708     C*------------------------------------------------------------------------*
066800970708     C     ARRIMP        BEGSR
066900970708     C*---
067000970708     C* $IMP -> IMPORTO DA ARROTONDARE
067100970708     C* $FDE -> ' '=NESSUNO / E=ECCESSO / D=DIFETTO / A=PURO
067200970708     C* $ARR -> IMPORTO AL QUALE ARROTONDARE
067300970708     C*---
067400980910IF  0C     D00TUR        IFEQ      '2'                                          *ARROT.SOLO 2°GIRO
067500970708IF  1C     $IMP          IFGT      *ZEROS                                       *IMPORTO PIENO
067600970708IF  2C     $FDE          IFNE      ' '                                          *SI ARROTONDAMENTO
067700970708IF  3C     $FDE          IFEQ      'E'                                          *PER ECCESSO
067800970708     C     $FDE          OREQ      'D'                                          *PER DIFETTO
067900011010     C     $IMP          DIV       $ARR          COMODO           15 0
068000011108     C                   MVR                     RESTO            15 5
068100970708IF  4C     $FDE          IFEQ      'E'
068200970708IF  5C     RESTO         IFGT      *ZEROS
068300970708     C                   ADD       1             COMODO
068400970708E   5C                   ENDIF
068500970708E   4C                   ENDIF
068600970708     C     COMODO        MULT      $ARR          $IMP
068700970708X   3C                   ELSE                                                   *PURO
068800011108     C     $ARR          DIV       2             ARR2             18 5
068900011108     C     $IMP          SUB       ARR2          IMP2             18 5
069000970708IF  4C     IMP2          IFGT      *ZEROS
069100011108     C     IMP2          DIV       $ARR          COMODO
069200011108     C                   MVR                     RESTO
069300970708IF  5C     RESTO         IFGT      *ZEROS
069400970708     C                   ADD       1             COMODO
069500970708E   5C                   ENDIF
069600970708     C     COMODO        MULT      $ARR          $IMP
069700970708E   4C                   ENDIF
069800970708E   3C                   ENDIF
069900970708E   2C                   ENDIF
070000970708E   1C                   ENDIF
070100980910E   0C                   ENDIF
070200970708     C*
070300970708     C                   ENDSR
070400970708     C*------------------------------------------------------------------------*
070500970708     C* CALPT - CALCOLA IL PESO TASSABILE
070600970708     C*------------------------------------------------------------------------*
070700970708     C     CALPT         BEGSR
070800970708     C*
070900970708     C* IMPOSTA COME PESO QUELLO DEI DATI TIPO SCAGLIONE
071000970708     C                   Z-ADD     SPSC(S)       PESTAS           13 3          *KG O QLI
071100970708     C*
071200970708     C* CALCOLA IL PESO TASSABILE SE INDICATO RAPPORTO PESO/VOLUME
071300970708IF  1C     SRPV(S)       IFGT      *ZEROS
071400970708     C                   Z-ADD     *ZEROS        COMPRV            9 3
071500970708     C     SVSC(S)       MULT      SRPV(S)       COMPRV
071600970708     C     COMPRV        MULT      100           PESTAS                         *CALCOLO PESO TASS.
071700970708IF  2C     PESTAS        IFLT      SPSC(S)                                      *PESO TASS < REALE
071800970708     C                   Z-ADD     SPSC(S)       PESTAS                         *PESO TASS=PESO REAL
071900970708E   2C                   ENDIF
072000970708E   1C                   ENDIF
072100970708     C*
072200970708     C                   ENDSR
072300011009     C*------------------------------------------------------------------------*
072400011009     C* CARGEDCE - CARICAMENTO TABELLA "GED" TIPO RECORD "CE" E CONSIDERAZIONI SU DIVISE
072500011009     C*------------------------------------------------------------------------*
072600011009     C     CARGEDCE      BEGSR
072700011009     C*
072800011009     C                   CLEAR                   DSGEDCE
072900011026     C                   MOVEL     *blanks       WDIVCE            3
073000011009     C*
073100011009     C                   MOVEL(P)  'GED'         KTECOD
073200011009     C                   MOVEL(P)  'CE'          KTEKE1
073300011009     C     KEYTBE        CHAIN     TNTBE01L
073400011009     C                   IF        %found(TNTBE01L)
073500011009     C                   MOVEL     TBEUNI        DSGEDCE
073600011009     C                   ENDIF
073700011009     C*
073800011009     C                   IF        §GEDCEDAT > D00DTC
073900011026     C                   EVAL      WDIVCE = §GEDCEDIP
074000011026     C                   ELSE
074100011026     C                   EVAL      WDIVCE = §GEDCEDIV
074200011009     C                   ENDIF
074300011009     C*
074400011009     C                   ENDSR
074500970708     C*------------------------------------------------------------------------*
074600970708     C* *INZSR - OPERAZIONI INIZIALI
074700970708     C*------------------------------------------------------------------------*
074800970708     C     *INZSR        BEGSR
074900970708     C*--------------------
075000970708     C* RICEVIMENTO PARAMETRI
075100970708     C*--------------------
075200970708     C     *ENTRY        PLIST
075300970708     C                   PARM                    KPJBA
075400970708     C                   PARM                    DSTE00
075500970708     C*--------------------
075600970708     C* VARIABILI RIFERITE AL DATA BASE
075700970708     C*--------------------
075800970708     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
075900970708     C     *LIKE         DEFINE    TBLCOD        KTBCOD
076000010926     C     *LIKE         DEFINE    ETCDSF        KTCDSF                         *TEETC00F
076100980603     C     *LIKE         DEFINE    ETCCTO        KTCCTO
076200970708     C     *LIKE         DEFINE    ETCKSC        KTCKSC
076300970708     C     *LIKE         DEFINE    ETCCTR        KTCCTR
076400970708     C     *LIKE         DEFINE    ETCPRG        KTCPRG
076500970708     C     *LIKE         DEFINE    ETCLNP        KTCLNP
076600010926     C     *LIKE         DEFINE    ETSDSF        KTSDSF                         *TEETS00F
076700980603     C     *LIKE         DEFINE    ETSCTO        KTSCTO
076800970708     C     *LIKE         DEFINE    ETSKSC        KTSKSC
076900970708     C     *LIKE         DEFINE    ETSCTR        KTSCTR
077000970708     C     *LIKE         DEFINE    ETSPRG        KTSPRG
077100970708     C     *LIKE         DEFINE    ETSLNP        KTSLNP
077200990820     C     *LIKE         DEFINE    TADKSC        KTDKSC                         *TITAD00F
077300970708     C     *LIKE         DEFINE    TADCTR        KTDCTR
077400970708     C     *LIKE         DEFINE    TADPRG        KTDPRG
077500970708     C     *LIKE         DEFINE    TADLNP        KTDLNP
077600991125     C     *LIKE         DEFINE    TAMKSC        KAMKSC                         *TITAD00F
077700991125     C     *LIKE         DEFINE    TAMCTR        KAMCTR
077800991125     C     *LIKE         DEFINE    TAMPRG        KAMPRG
077900010926     C     *LIKE         DEFINE    ETWUSR        KTWUSR                         *TEETW00F
078000980603     C     *LIKE         DEFINE    ETWDSF        KTWDSF
078100980603     C     *LIKE         DEFINE    ETWCTO        KTWCTO
078200970708     C     *LIKE         DEFINE    ETWKSC        KTWKSC
078300970708     C     *LIKE         DEFINE    ETWCTR        KTWCTR
078400970708     C     *LIKE         DEFINE    ETWPRG        KTWPRG
078500970708     C     *LIKE         DEFINE    ETWSGL        KTWSGL
078600970708     C     *LIKE         DEFINE    ETWCTS        KTWCTS
078700970708     C     *LIKE         DEFINE    ETWRDL        KTWRDL
078800970708     C     *LIKE         DEFINE    ETWSGL        DEPSGL                         *DEPOSITI
078900970708     C     *LIKE         DEFINE    ETWCTS        DEPCTS
079000970708     C     *LIKE         DEFINE    ETWRCT        DEPRCT
079100970708     C     *LIKE         DEFINE    ETWCTS        WCTS                           *DI LAVORO
079200970708     C     *LIKE         DEFINE    ETWPPE        TOTPPE
079300970708     C     *LIKE         DEFINE    ETWSGL        KSGL                           *DEPOSITI CHAIVE
079400970708     C     *LIKE         DEFINE    ETWCTS        KCTS
079500970708     C     *LIKE         DEFINE    ETWRDL        KRDL
079600011009     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
079700011009     C     *LIKE         DEFINE    TBEKE1        KTEKE1
079800970708     C*--------------------
079900970708     C* CHIAVI DI LETTURA
080000970708     C*--------------------
080100970708     C* DI TABEL00F - PARZIALE
080200970708     C     KEYTAB        KLIST
080300970708     C                   KFLD                    KTBKUT                         *UTENTE
080400970708     C                   KFLD                    KTBCOD                         *TABELLA
080500010926     C* DI TEETC01L - COMPLETA
080600970708     C     KEYETC        KLIST
080700980603     C                   KFLD                    KTCDSF                         *DATI SEDE O FILIALE
080800980603     C                   KFLD                    KTCCTO                         *TARIFFA/OFFERTA
080900970708     C                   KFLD                    KTCKSC                         *CLIENTE/VISITA
081000970708     C                   KFLD                    KTCCTR                         *TARIFFA/OFFERTA
081100970708     C                   KFLD                    KTCPRG                         *PROGRESSIVO
081200970708     C                   KFLD                    KTCLNP                         *LINEA PARTENZA
081300010926     C* DI TEETS01L - PARZIALE
081400970708     C     KEYETS        KLIST
081500980603     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
081600970708     C                   KFLD                    KTSCTO                         *TARIFFA/OFFERTA
081700970708     C                   KFLD                    KTSKSC                         *CLIENTE/VISITA
081800970708     C                   KFLD                    KTSCTR                         *TARIFFA/OFFERTA
081900970708     C                   KFLD                    KTSPRG                         *PROGRESSIVO
082000970708     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
082100991125     C* DI TNTAM01L - COMPLETA
082200991125     C     KEYTAM        KLIST
082300991125     C                   KFLD                    KAMKSC                         *CLIENTE/VISITA
082400991125     C                   KFLD                    KAMCTR                         *TARIFFA/OFFERTA
082500991125     C                   KFLD                    KAMPRG                         *PROGRESSIVO
082600990820     C* DI TITAD04L - PARZIALE
082700970708     C     KE2TAD        KLIST
082800970708     C                   KFLD                    KTDKSC                         *CLIENTE/VISITA
082900970708     C                   KFLD                    KTDCTR                         *TARIFFA/OFFERTA
083000970708     C                   KFLD                    KTDPRG                         *PROGRESSIVO
083100970708     C                   KFLD                    KTDLNP                         *LINEA PARTENZA
083200010926     C* DI TEETW01L - PARZIALE
083300970708     C     KEYETW        KLIST
083400970708     C                   KFLD                    KTWUSR                         *UTENTE
083500980603     C                   KFLD                    KTWDSF                         *DATI SEDE O FILIALE
083600970708     C                   KFLD                    KTWCTO                         *TARIFFA/OFFERTA
083700970708     C                   KFLD                    KTWKSC                         *CLIENTE/VISITA
083800970708     C                   KFLD                    KTWCTR                         *TARIFFA/OFFERTA
083900970708     C                   KFLD                    KTWPRG                         *PROGRESSIVO
084000010926     C* DI TEETW01L - PARZIALE
084100970708     C     KE2ETW        KLIST
084200970708     C                   KFLD                    KTWUSR                         *UTENTE
084300980603     C                   KFLD                    KTWDSF                         *DATI SEDE O FILIALE
084400970708     C                   KFLD                    KTWCTO                         *TARIFFA/OFFERTA
084500970708     C                   KFLD                    KTWKSC                         *CLIENTE/VISITA
084600970708     C                   KFLD                    KTWCTR                         *TARIFFA/OFFERTA
084700970708     C                   KFLD                    KTWPRG                         *PROGRESSIVO
084800970708     C                   KFLD                    KTWSGL                         *SCAGLIONE
084900010926     C* DI TEETW01L - COMPLETA
085000970708     C     KE3ETW        KLIST
085100970708     C                   KFLD                    KTWUSR                         *UTENTE
085200980603     C                   KFLD                    KTWDSF                         *DATI SEDE O FILIALE
085300970708     C                   KFLD                    KTWCTO                         *TARIFFA/OFFERTA
085400970708     C                   KFLD                    KTWKSC                         *CLIENTE/VISITA
085500970708     C                   KFLD                    KTWCTR                         *TARIFFA/OFFERTA
085600970708     C                   KFLD                    KTWPRG                         *PROGRESSIVO
085700970708     C                   KFLD                    KTWSGL                         *SCAGLIONE
085800970708     C                   KFLD                    KTWCTS                         *CODICE TASSAZIONE
085900970708     C                   KFLD                    KTWRDL                         *RAGGRUPPAMENTO
086000011009     C* LETTURA TNTBE01L - PARZIALE
086100011009     C     KEYTBE        KLIST
086200011009     C                   KFLD                    KTECOD                         *UTENTE
086300011009     C                   KFLD                    KTEKE1                         *CODICE
086400970708     C*--------------------
086500970708     C* VARIABILI DI CONTROLLO
086600970708     C*--------------------
086700970708     C                   MOVEL     'N'           $FINE             1            *FINE PGM
086800970708     C                   MOVEL     'N'           $RECOK            1            *VALIDITA' RECORD
086900970708     C*
087000970708     C* CARICA LE TABELLE OCCORRENTI
087100970708     C                   EXSR      CARTAB
087200011009     C*
087300011009     C* RECUPERA DATI DA TEBELLA "GED" TIPO RECORD "CE"
087400011009     C                   EXSR      CARGEDCE
087500970708     C*
087600970708     C                   ENDSR
087700970708     C*------------------------------------------------------------------------*
087800970708     C* CARTAB - CARICA LE TABELLE OCCORRENTI
087900970708     C*------------------------------------------------------------------------*
088000970708     C     CARTAB        BEGSR
088100970708     C*--------------------
088200970708     C* CODICI TASSAZIONE
088300970708     C*--------------------
088400970708     C                   Z-ADD     *ZEROS        I                 5 0
088500020906     C                   Z-ADD     1             KTBKUT
088600970708     C                   MOVEL     'CT'          KTBCOD
088700970708     C     KEYTAB        SETLL     TABEL00F
088800970708     C     KEYTAB        READE     TABEL00F                               99
088900970708DO  1C     *IN99         DOWEQ     *OFF
089000970708IF  2C     TBLFLG        IFEQ      *BLANKS
089100970708     C                   MOVEL     TBLUNI        DSCT
089200970708IF  3C     §CTUTC        IFEQ      'S'                                          *DA CONSIDERARE
089300970708     C                   ADD       1             I
089400970708     C                   MOVEL     TBLKEY        CTS(I)
089500970708     C                   MOVEL     §CTCOR        COR(I)
089600970708     C                   MOVEL     §CTRAP        RAP(I)
089700970708IF  4C     §CTNAR        IFEQ      *BLANKS
089800970708     C                   MOVEL     'IT'          NAR(I)                         *NAZIONE ITALIA
089900970708X   4C                   ELSE
090000970708     C                   MOVEL     'EE'          NAR(I)                         *NAZIONE ITALIA
090100970708E   4C                   ENDIF
090200970708E   3C                   ENDIF
090300970708E   2C                   ENDIF
090400970708     C     KEYTAB        READE     TABEL00F                               99
090500970708E   1C                   ENDDO
090600970708     C*
090700970708     C                   ENDSR
090800970725     O*------------------------------------------------------------------------*
090900010926     OTEETW000  E            UPDETW
091000970725     O                       ETWCOM
