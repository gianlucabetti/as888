000100931123     H*------------------------------------------------------------------------*
000200950627     H* STATISTICA AFFIDABILITA' CONSEGNE - STAMPA                             *
000300931123     H*------------------------------------------------------------------------*
000400931122     H DECEDIT('0,') DATEDIT(*DMY.)
000500941031     F*------------------------------------------------------------------------*
000600941031     F* DATA BASE
000700941031     F*------------------------------------------------------------------------*
000800930623     FWFDLA01L  IF   E           K DISK
000900930208     FTABEL00F  IF   E           K DISK
001000930526     FAZORG01L  IF   E           K DISK
001100930622     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001200930622     F                                     FORMLEN(66)
001300930622     F                                     FORMOFL(60)
001400931123     L*------------------------------------------------------------------------*
001500941031     D*------------------------------------------------------------------------*
001600941031     D* SCHIERE
001700941031     D*------------------------------------------------------------------------*
001800941031     D*-------------------
001900930622     D* INTESTAZIONI STAMPA
002000941031     D*-------------------
002100950629     D TES             S             66    DIM(30) CTDATA PERRCD(1)             RIGHE TESTATA
002200950627     D INT             S             20    DIM(10) CTDATA PERRCD(1)             DECODIFICA INTESTAZI
002300941031     D*-------------------
002400930622     D* SCHIERA FILIALI
002500941031     D*-------------------
002600971124     D FIL             S              3  0 DIM(500)                             *CODICE FILIALE
002700971124     D DFIL            S             20    DIM(500)                             *DESCRIZIONE FILIALE
002800941202     D*-------------------
002900941202     D* TIPI SERVIZIO
003000941202     D*-------------------
003100941202     D TSP             S              1    DIM(5)                               *TIPI SERVIZIO
003200941202     D DTSP            S             15    DIM(5)                               *DESCRIZIONE SERVIZI
003300941202     D OTSP            S              1  0 DIM(5)                               *NUMERO OCUR DEL SER
003400941202     D*------------------
003500941202     D* SCHIERA MULTIPLA ---> - 5 TIPI SERVIZIO X 7 ORE DELEIVERY -
003600941202     D*------------------
003700941202     D*-------------------
003800941202     D* SCHIERE DI LAVORO
003900941202     D*-------------------
004000941202     D WTSP            S              8  0 DIM(6)                                 *SPEDIZIONI SERVIZ
004100941202     D WPER            S              6  2 DIM(6)                                 *PERCENTUALI SERVI
004200941031     D*------------------------------------------------------------------------*
004300941031     D* INPUT
004400941031     D*------------------------------------------------------------------------*
004500941202     D*------------------
004600941202     D* SPEDIZIONI  (8 X 6) = 48
004700941202     D*------------------
004800941202     D* DS MULTIPLA GENERALE  (5 TIPI SERVIZIO X 6 ORE DELIVERY)
004900941202     D*------------------
005000941202     D OGEN            DS                  OCCURS(5) INZ
005100941202     D  GEN                    1     48  0
005200941202     D                                     DIM(6)                                 *GENERALE
005300941202     D*------------------
005400941202     D* DS MULTIPLA DIVISIONE (5 TIPI SERVIZIO X 6 ORE DELIVERY)
005500941202     D*------------------
005600941202     D ODAN            DS                  OCCURS(5) INZ
005700941202     D  DAN                    1     48  0
005800941202     D                                     DIM(6)                                 *DIVISIONE
005900941202     D*------------------
006000941202     D* DS MULTIPLA AREA      (5 TIPI SERVIZIO X 6 ORE DELIVERY)
006100941202     D*------------------
006200941202     D OAAN            DS                  OCCURS(5) INZ
006300941202     D  AAN                    1     48  0
006400941202     D                                     DIM(6)                                 *AREA
006500941202     D*------------------
006600941202     D* DS MULTIPLA FILIALE   (5 TIPI SERVIZIO X 6 ORE DELIVERY)
006700941202     D*------------------
006800941202     D OFAN            DS                  OCCURS(5) INZ
006900941202     D  FAN                    1     48  0
007000941202     D                                     DIM(6)                                 *FILIALE
007100941202     D*------------------
007200941202     D* DS MULTIPLA ZONA      (5 TIPI SERVIZIO X 6 ORE DELIVERY)
007300941202     D*------------------
007400941202     D OZON            DS                  OCCURS(5) INZ
007500941202     D  ZON                    1     48  0
007600941202     D                                     DIM(6)                                 *ZONA
007700941202     D*------------------
007800941202     D* DS MULTIPLA C.A.P.    (5 TIPI SERVIZIO X 6 ORE DELIVERY)
007900941202     D*------------------
008000941202     D OCAN            DS                  OCCURS(5) INZ
008100941202     D  CAN                    1     48  0
008200941202     D                                     DIM(6)                                 *CAP
008300950627     D*-------------------
008400950627     D* ARCHITETTURA
008500950627     D*-------------------
008600950627     D KPJBA         E DS
008700941031     D*-------------------
008800020920     D* DS REPERIMENTO DATI UTENTE
008900020920     D*---------------------
009000020920     D TIBS34DS      E DS                                                       *Profili utente
009100020920     D DDATIUTE      E DS                                                       *Dati utente
009200020920     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009300950627     D*-------------------
009400950627     D* DS COMPOSIZIONE CAP+NAZ
009500950627     D*-------------------
009600950627     D                 DS
009700950627     D  WFANZD                 1      3
009800950627     D  WFACAD                 4     12
009900950627     D  WFACAP                 1     12
010000941031     D*-------------------
010100930208     D* DS PARAMETRI DI LANCIO PGM
010200941031     D*-------------------
010300950627     D PARAM           DS
010400941031     D  PARANN                 1      4  0
010500941031     D  PARMMI                 5      6  0
010600941031     D  PARMMF                 7      8  0
010700941031     D  PARSNC                 9      9
010800941031     D  PARSDC                10     10
010900941031     D  PARSNA                11     11
011000941031     D  PARSDA                12     12
011100941031     D  PARSNR                13     13
011200941031     D  PARDIV                14     14
011300941031     D  PARARE                15     17  0
011400941031     D  PARFLP                18     20  0
011500941031     D  PARTFP                21     23  0
011600941031     D  PARFLA                24     26  0
011700941031     D  PARTFA                27     29  0
011800941031     D  PARDTI                30     37  0
011900941031     D  PARDTF                38     45  0
012000941031     D  PARTLA                46     46
012100941031     D  PARSNP                47     47
012200950712     D  PARTOT                48     48
012300950712     D  PARIOE                49     49
012400970826     D  PARCFP                50     50
012500941031     C*------------------------------------------------------------------------*
012600941031     C* MAIN LINE
012700941031     C*------------------------------------------------------------------------*
012800000000     C*
012900930129     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
013000930208     C                   EXSR      SETFIL
013100930128     C*
013200930317     C* CICLO FINO A FINE FILE DI LAVORO
013300940120DO  1C     FINL00        DOWEQ     '0'
013400930623     C*
013500930623     C* OPERAZIONI PER NUOVA DIVISIONE
013600930623     C                   EXSR      NEWDIV
013700930317     C*
013800930622     C* CICLO FINO A ROTTURA DI DIVISIONE
013900940120DO  2C     FINL00        DOWEQ     '0'
014000930623     C     WFADIV        ANDEQ     DEPDIV
014100930623     C*
014200930623     C* OPERAZIONI PER NUOVA AREA
014300930623     C                   EXSR      NEWARE
014400930317     C*
014500930622     C* CICLO FINO A ROTTURA DI AREA
014600940120DO  3C     FINL00        DOWEQ     '0'
014700930623     C     WFADIV        ANDEQ     DEPDIV
014800930623     C     WFAARE        ANDEQ     DEPARE
014900930623     C*
015000930623     C* OPERAZIONI PER NUOVA FILIALE DI ARRIVO
015100930623     C                   EXSR      NEWFLA
015200930317     C*
015300930623     C* CICLO FINO A ROTTURA DI FILIALE DI ARRIVO
015400940120DO  4C     FINL00        DOWEQ     '0'
015500930623     C     WFADIV        ANDEQ     DEPDIV
015600930623     C     WFAARE        ANDEQ     DEPARE
015700930623     C     WFAFLA        ANDEQ     DEPFLA
015800930623     C*
015900930623     C* OPERAZIONI PER NUOVA ZONA ARRIVO
016000930623     C                   EXSR      NEWZNC
016100930317     C*
016200930623     C* CICLO FINO A ROTTURA DI ZONA ARRIVO
016300940120DO  5C     FINL00        DOWEQ     '0'
016400930623     C     WFADIV        ANDEQ     DEPDIV
016500930623     C     WFAARE        ANDEQ     DEPARE
016600930623     C     WFAFLA        ANDEQ     DEPFLA
016700930623     C     WFAZNC        ANDEQ     DEPZNC
016800930623     C*
016900930623     C* OPERAZIONI PER NUOVO CAP DESTINAZIONE
017000930623     C                   EXSR      NEWCAD
017100930622     C*
017200930623     C* CICLO FINO A ROTTURA DI CAP DESTINAZIONE
017300940120DO  6C     FINL00        DOWEQ     '0'
017400930623     C     WFADIV        ANDEQ     DEPDIV
017500930623     C     WFAARE        ANDEQ     DEPARE
017600930623     C     WFAFLA        ANDEQ     DEPFLA
017700930623     C     WFAZNC        ANDEQ     DEPZNC
017800950627     C     WFACAP        ANDEQ     DEPCAP
017900930318     C*
018000930622     C* MEMORIZZA LE SPEDIZIONI
018100930622     C                   EXSR      MEMSPE
018200000000     C*
018300930128     C* LEGGE IL RECORD SUCCESSIVO
018400930208     C                   EXSR      LETFIL
018500930128     C*
018600940120E   6C                   ENDDO
018700930318     C*
018800930623     C* STAMPA IL TOTALE CAP DI DESTINAZIONE
018900930623     C                   EXSR      STACAD
019000930318     C*
019100940120E   5C                   ENDDO
019200930622     C*
019300930623     C* STAMPA IL TOTALE ZONA ARRIVO
019400930623     C                   EXSR      STAZNC
019500930622     C*
019600940120E   4C                   ENDDO
019700930622     C*
019800930623     C* STAMPA IL TOTALE FILIALE DI ARRIVO
019900930623     C                   EXSR      STAFLA
020000930622     C*
020100940120E   3C                   ENDDO
020200930622     C*
020300940120     C* STAMPA IL TOTALE AREA SE RICHIESTA STAMPA TOTALI
020400940120IF  3C     PARTOT        IFEQ      'S'
020500930622     C                   EXSR      STAARE
020600940120E   3C                   ENDIF
020700930622     C*
020800940120E   2C                   ENDDO
020900930622     C*
021000940120     C* STAMPA IL TOTALE DIVISIONE SE RICHIESTA STAMPA TOTALI
021100940120IF  2C     PARTOT        IFEQ      'S'
021200930622     C                   EXSR      STADIV
021300940120E   2C                   ENDIF
021400930622     C*
021500940120E   1C                   ENDDO
021600940120     C*
021700940120     C* STAMPA IL TOTALE GENERALE SE RICHIESTA STAMPA TOTALI
021800940120IF  1C     PARTOT        IFEQ      'S'
021900930622     C                   EXSR      STAGEN
022000940120E   1C                   ENDIF
022100930622     C*
022200930622     C* STAMPA RIGA FINE STAMPA
022300930622     C                   EXCEPT    STAFIN
022400000000     C*
022500000000     C                   SETON                                        LR
022600931123     C*------------------------------------------------------------------------*
022700941031     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
022800931123     C*------------------------------------------------------------------------*
022900930208     C     SETFIL        BEGSR
023000930208     C*
023100930622     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
023200930623     C                   Z-ADD     PARANN        KWAANN
023300930623     C                   MOVEL     *BLANKS       KWADIV
023400930623     C                   Z-ADD     *ZEROS        KWAARE
023500930623     C                   Z-ADD     *ZEROS        KWAFLA
023600930623     C                   Z-ADD     *ZEROS        KWAZNC
023700950627     C                   MOVEL     *BLANKS       KWANZD
023800950627     C                   MOVEL     *BLANKS       KWACAD
023900930623     C                   Z-ADD     *ZEROS        KWAMES
024000941202     C                   MOVEL     *BLANKS       KWATSP
024100941202     C                   Z-ADD     *ZEROS        KWARAN
024200950627     C                   MOVEL     *BLANKS       KWAIOE
024300930623     C     KEYWFA        SETLL     WFDLA01L                           99        *FINE-ARCHIV
024400930623     C*
024500930623     C     *IN99         IFEQ      '1'
024600930623     C                   MOVEL     '1'           FINL00
024700930623     C                   ELSE
024800930623     C                   EXSR      LETFIL
024900930623     C                   END
025000930317     C*
025100930208     C                   ENDSR
025200931123     C*------------------------------------------------------------------------*
025300941031     C* LETFIL - LETTURA PROSSIMO RECORD
025400931123     C*------------------------------------------------------------------------*
025500930208     C     LETFIL        BEGSR
025600930623     C*
025700930623     C* LEGGO FINO A:
025800930628     C*     - FINE FILE O SUPERATO LIMITE FINALE
025900930623     C*     - TROVATO RECORD VALIDO
026000930623     C                   MOVEL     'N'           WRECOK
026100930628     C     FINL00        DOWEQ     '0'                                          --- 1 -->
026200930623     C     WRECOK        ANDEQ     'N'
026300930607     C*
026400930622     C* LETTURE SUCCESSIVE ALLA PRIMA
026500930623     C                   READ      WFDLA01L                               99
026600930624     C*
026700930628     C     *IN99         IFEQ      '1'                                          *FINE FILE
026800930628     C     WFAANN        ORNE      PARANN                                       *SUPERATO ANNO
026900930628     C                   MOVEL     '1'           FINL00
027000930628     C                   ELSE
027100930628     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
027200930624     C                   END
027300930623     C*
027400930623     C                   END                                                    <-- 1 ---
027500930208     C*
027600930208     C                   ENDSR
027700931123     C*------------------------------------------------------------------------*
027800941031     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
027900931123     C*------------------------------------------------------------------------*
028000930622     C     CHKREC        BEGSR
028100930622     C*
028200930622     C                   MOVEL     'S'           WRECOK            1
028300930622     C*
028400930622     C* SE NON RIENTRA NEI MESI RICHIESTI ESCLUDO IL RECORD
028500930623     C     WFAMES        IFGE      PARMMI
028600930623     C     WFAMES        ANDLE     PARMMF
028700930622     C                   ELSE
028800930622     C                   MOVEL     'N'           WRECOK
028900930622     C                   END
029000930623     C*
029100930623     C* SE DIVISIONE DEL RECORD <> DIVISIONE RICHIESTA, ESCLUDO
029200930623     C     PARDIV        IFNE      *BLANKS
029300930623     C     WFADIV        IFNE      PARDIV
029400930623     C                   MOVEL     'N'           WRECOK
029500930623     C                   END
029600930623     C                   END
029700930623     C*
029800930623     C* SE AREA DEL RECORD <> AREA RICHIESTA, ESCLUDO
029900930623     C     PARARE        IFNE      *ZEROS
030000930623     C     WFAARE        IFNE      PARARE
030100930623     C                   MOVEL     'N'           WRECOK
030200930623     C                   END
030300930623     C                   END
030400930623     C*
030500930623     C* SE FIL.ARR.  DEL RECORD <> FIL.ARR. RICHIESTA, ESCLUDO
030600930623     C     PARFLA        IFNE      *ZEROS
030700930623     C     WFAFLA        IFNE      PARFLA
030800930623     C                   MOVEL     'N'           WRECOK
030900930623     C                   END
031000930623     C                   END
031100930623     C*
031200930623     C* SE TERM.ARR. DEL RECORD <> TERM.ARR RICHIESTO, ESCLUDO
031300930623     C     PARTFA        IFNE      *ZEROS
031400930623     C     WFATFA        IFNE      PARTFA
031500930623     C                   MOVEL     'N'           WRECOK
031600930623     C                   END
031700930623     C                   END
031800950627     C*
031900950627     C* CONTROLLA IL TIPO DI DATI IN STATISTICA RICHIESTO
032000950627IF  1C     PARIOE        IFNE      *BLANKS                                      *SELEZIONE
032100950627IF  2C     PARIOE        IFNE      WFAIOE
032200950627     C                   MOVEL     'N'           WRECOK
032300950627E   2C                   ENDIF
032400950627X   1C                   ELSE                                                   *NESSUNA SELEZIONE
032500950627IF  2C     WFAIOE        IFEQ      'I'                                          *'I' + 'X' = TUTTO
032600950627     C     WFAIOE        OREQ      'X'
032700950627X   2C                   ELSE
032800950627     C                   MOVEL     'N'           WRECOK
032900950627E   2C                   ENDIF
033000950627E   1C                   ENDIF
033100930622     C*
033200930622     C                   ENDSR
033300931123     C*------------------------------------------------------------------------*
033400941031     C* MEMSPE - MEMORIZZA LE SPEDIZIONI
033500931123     C*------------------------------------------------------------------------*
033600930622     C     MEMSPE        BEGSR
033700941202     C*
033800941202     C* CERCO IN NUMERO DI OCUR DEL TIPO SERVIZIO LETTO
033900941202     C                   Z-ADD     1             I
034000941202     C                   SETOFF                                       28
034100941205     C     WFATSP        LOOKUP    TSP(I)                                 28
034200941202IF  1C     *IN28         IFEQ      *ON                                          *SOLO SE TROVATO
034300941202     C*
034400941202     C* NUMERO RANGE ORE DELIVERY
034500941202     C                   Z-ADD     WFARAN        R
034600941202     C*--------------------
034700941202     C* GENERALE
034800941202     C*--------------------
034900941202     C     OTSP(I)       OCCUR     OGEN
035000941202     C                   ADD       WFANSP        GEN(R)
035100941202     C*--------------------
035200941202     C* DIVISIONE
035300941202     C*--------------------
035400941202     C     OTSP(I)       OCCUR     ODAN
035500941202     C                   ADD       WFANSP        DAN(R)
035600941202     C*--------------------
035700941202     C* AREA
035800941202     C*--------------------
035900941202     C     OTSP(I)       OCCUR     OAAN
036000941202     C                   ADD       WFANSP        AAN(R)
036100941202     C*--------------------
036200941202     C* FILIALE
036300941202     C*--------------------
036400941202     C     OTSP(I)       OCCUR     OFAN
036500941202     C                   ADD       WFANSP        FAN(R)
036600941202     C*--------------------
036700941202     C* ZONA
036800941202     C*--------------------
036900941202     C     OTSP(I)       OCCUR     OZON
037000941202     C                   ADD       WFANSP        ZON(R)
037100941202     C*--------------------
037200941202     C* C.A.P.
037300941202     C*--------------------
037400941202     C     OTSP(I)       OCCUR     OCAN
037500941202     C                   ADD       WFANSP        CAN(R)
037600941202     C*
037700941202E   1C                   ENDIF
037800930317     C*
037900930317     C                   ENDSR
038000931123     C*------------------------------------------------------------------------*
038100941031     C* NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
038200931123     C*------------------------------------------------------------------------*
038300930623     C     NEWDIV        BEGSR
038400930623     C*
038500930623     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
038600930623     C                   MOVEL     WFADIV        DEPDIV
038700930623     C*
038800930623     C* AZZERA I TOTALI DELLA DIVISIONE
038900941205DO  1C     1             DO        WULTSP        R
039000941205     C     R             OCCUR     ODAN
039100941202     C                   CLEAR                   DAN
039200941205E   1C                   ENDDO
039300930623     C*
039400930623     C* DECODIFICA LA DIVISIONE
039500930623     C                   EXSR      DECDIV
039600930623     C*
039700931129     C* DESCRIZIONE INTESTAZIONE
039800930623     C                   MOVEL     TES(14)       STINT1           22
039900930623     C                   MOVEL     DESDIV        STINT2           20
040000930623     C*
040100930623     C                   ENDSR
040200931123     C*------------------------------------------------------------------------*
040300941031     C* NEWARE - OPERAZIONI PER NUOVA AREA
040400931123     C*------------------------------------------------------------------------*
040500930623     C     NEWARE        BEGSR
040600930623     C*
040700930623     C* MEMORIZZA LA NUOVA AREA IN UN DEPOSITO
040800930623     C                   Z-ADD     WFAARE        DEPARE
040900930623     C*
041000930623     C* AZZERA I TOTALI DELL'AREA
041100941205DO  1C     1             DO        WULTSP        R
041200941205     C     R             OCCUR     OAAN
041300941202     C                   CLEAR                   AAN
041400941205E   1C                   ENDDO
041500930623     C*
041600930623     C                   ENDSR
041700931123     C*------------------------------------------------------------------------*
041800941031     C* NEWFLA - OPERAZIONI PER NUOVA FILIALE DI ARRIVO
041900931123     C*------------------------------------------------------------------------*
042000930623     C     NEWFLA        BEGSR
042100930623     C*
042200931129     C* MEMORIZZA LA NUOVA FILIALE ARRIVO IN UN DEPOSITO
042300930623     C                   Z-ADD     WFAFLA        DEPFLA
042400930623     C*
042500931129     C* AZZERA I TOTALI DELLA FILIALE ARRIVO
042600941205DO  1C     1             DO        WULTSP        R
042700941205     C     R             OCCUR     OFAN
042800941202     C                   CLEAR                   FAN
042900941205E   1C                   ENDDO
043000931129     C*
043100931129     C* DECODIFICA LA FILIALE DI ARRIVO
043200931129     C                   MOVEL     DEPFLA        WFIL              3 0
043300931129     C                   EXSR      DECFIL
043400931129     C*
043500931129     C* SE RICHIESTA STAMPA DETTAGLIATA IMPOSTO L'INTESTAZIONE
043600931129IF  1C     PARSDA        IFEQ      'D'
043700950629     C*
043800950629     C* DESCRIZIONE TOTALE (DIVERSA FRA ITALIA/EXPORT  E  IMPORT)
043900950629IF  2C     PARIOE        IFEQ      'M'
044000950629     C                   MOVEL     TES(29)       STINT1
044100950629X   2C                   ELSE
044200931129     C                   MOVEL     TES(15)       STINT1
044300950629E   2C                   ENDIF
044400931129     C                   MOVEL     DESFIL        STINT2
044500931129E   1C                   ENDIF
044600930623     C*
044700931129     C* SE RICHIESTA STAMPA DETTAGLIATA O STAMPA TESTASTA: SALTO PAGINA
044800931129IF  1C     PARSDA        IFEQ      'D'
044900931129     C     FLGTES        OREQ      '1'
045000931129     C                   EXCEPT    TESTA                                        *STAMPO TESTATA
045100941209     C                   SETOFF                                       OF
045200931129     C                   MOVEL     '0'           FLGTES                         *REIMPOSTO FLAG
045300931129E   1C                   ENDIF
045400930623     C*
045500930623     C                   ENDSR
045600931123     C*------------------------------------------------------------------------*
045700941031     C* NEWZNC - OPERAZIONI PER NUOVA ZONA DI ARRIVO
045800931123     C*------------------------------------------------------------------------*
045900930623     C     NEWZNC        BEGSR
046000930623     C*
046100930623     C* MEMORIZZA IL NUOVA ZONA DI ARRIVO IN UN DEPOSITO
046200930623     C                   Z-ADD     WFAZNC        DEPZNC
046300930623     C*
046400930623     C* AZZERA I TOTALI DELLA ZONA DI ARRIVO
046500941205DO  1C     1             DO        WULTSP        R
046600941205     C     R             OCCUR     OZON
046700941202     C                   CLEAR                   ZON
046800941205E   1C                   ENDDO
046900930623     C*
047000930623     C                   ENDSR
047100931123     C*------------------------------------------------------------------------*
047200941031     C* NEWCAD - OPERAZIONI PER NUOVO CAP DI DESTINAZIONE
047300931123     C*------------------------------------------------------------------------*
047400930623     C     NEWCAD        BEGSR
047500930623     C*
047600930623     C* MEMORIZZA IL NUOVO CAP DESTINAZ. IN UN DEPOSITO
047700950627     C                   MOVEL     WFACAP        DEPCAP                         *NAZIONE + C.A.P.
047800930623     C*
047900930623     C* AZZERA I TOTALI DEL CAD DESTINAZIONE
048000941205DO  1C     1             DO        WULTSP        R
048100941205     C     R             OCCUR     OCAN
048200941202     C                   CLEAR                   CAN
048300941205E   1C                   ENDDO
048400930623     C*
048500930623     C                   ENDSR
048600931123     C*------------------------------------------------------------------------*
048700941031     C* STACAD - STAMPA TOTALI CAP DESTINAZIONE
048800931123     C*------------------------------------------------------------------------*
048900930623     C     STACAD        BEGSR
049000941202     C*
049100941202     C* AZZERO CAMPI DI LAVORO
049200941202     C                   Z-ADD     *ZEROS        WR                1 0          *NUMERO SERVIZIO
049300941205     C                   Z-ADD     *ZEROS        WTUTS             8 0          *SPED.DEL SERVIZIO
049400941202     C                   Z-ADD     *ZEROS        WTSP                           *SCHIERA ORE SERVIZI
049500941202     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
049600930622     C*
049700930622     C* SE RICHIESTA STAMPA DETTAGLIATA STAMPO
049800931129IF  1C     PARSDA        IFEQ      'D'
049900930622     C*
050000950704     C* DESCRIZIONE TOTALE
050100950629     C                   MOVEL(P)  TES(8)        STDES1           22
050200950627     C                   MOVE(P)   DEPCAP        STDES2           22
050300941202     C*
050400941202     C* IMPOSTO A NO L'INDICATORE DELLA RIBATTUTA
050500941202     C                   MOVEL     '0'           *IN01
050600941209     C*
050700941209     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
050800941209     C     *INOF         IFEQ      '1'
050900941209     C                   EXCEPT    TESTA
051000941209     C                   SETOFF                                       OF
051100941209     C                   END
051200941202     C*--------------------
051300941202     C* RIGA SERVIZIO
051400941202     C*--------------------
051500941202     C* PER CISCUN TIPO SERVIZIO ESISTENTE
051600941202     C                   Z-ADD     *ZEROS        PASCIC            1 0          *N°PASSAGGIO CICLO
051700941202DO  2C     1             DO        WULTSP        R
051800941202     C     R             OCCUR     OCAN
051900941202     C*
052000941202     C* SOLO SE MEMORIZZATO QC.
052100941202     C                   XFOOT     CAN           WSUM             15 0          *COMODO SOMMA SHIERA
052200941202IF  3C     WSUM          IFNE      *ZEROS
052300941202     C                   ADD       1             PASCIC
052400941202     C*
052500941202     C* DECODIFICHE IN STAMPA
052600941202     C     PASCIC        IFEQ      1
052700941202     C                   MOVEL(P)  STDES1        STDES            22            *TIPO DI TOTALE
052800941202     C                   ELSE
052900941202     C     PASCIC        IFEQ      2
053000941202     C                   MOVEL(P)  STDES2        STDES                          *TOTALE FIL/CAP/TERM
053100941202     C                   ELSE
053200941202     C     PASCIC        IFGT      2
053300941202     C                   MOVEL(P)  *BLANKS       STDES
053400941202     C                   ENDIF
053500941202     C                   ENDIF
053600941202     C                   ENDIF
053700941202     C*
053800941202     C                   Z-ADD     R             WR                             *CODICE SERVIZIO
053900941202     C                   XFOOT     CAN           WTUTS                          *SPEDIZ. DEL SEVIZIO
054000941202     C                   Z-ADD     CAN           WTSP                           *ORE DELIVERY SERVIZ
054100941202     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
054200941202     C                   EXSR      STASPE
054300941202E   3C                   ENDIF
054400941202     C*
054500941202E   2C                   ENDDO
054600941202     C*--------------------
054700941202     C* RIGA TOTALE
054800941202     C*--------------------
054900941202     C* SOMMA DEI TIPI SERVIZI DIVISI PER ORE DELIVERY
055000941202     C                   CLEAR                   WTSP
055100941202DO  2C     1             DO        WULTSP        R
055200941202     C     R             OCCUR     OCAN
055300941202     C                   ADD       CAN           WTSP                           *ORE DELIVERY SERVIZ
055400941202E   2C                   ENDDO
055500941202     C*
055600941202     C* SOLO SE MEMORIZZATO QC.
055700941202     C                   XFOOT     WTSP          WSUM                           *COMODO SOMMA SHIERA
055800941202IF  2C     WSUM          IFNE      *ZEROS
055900941202     C*
056000941202     C* DECODIFICHE IN STAMPA
056100941202     C     PASCIC        IFLT      2                                            *COMUNQUE UN SERVIZI
056200941202     C                   MOVEL(P)  STDES2        STDES                          *LO HA STAMPATO (>1)
056300941202     C                   ELSE
056400941202     C                   MOVEL(P)  *BLANKS       STDES
056500941202     C                   ENDIF
056600941202     C*
056700941202     C                   Z-ADD     0             WR                             *SERVIZIO "TOTALE"
056800941202     C                   XFOOT     WTSP          WTUTS                          *SPEDIZIONI TOTALI
056900941202     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
057000941202     C                   EXSR      STASPE
057100941202E   2C                   ENDIF
057200941202     C*
057300941202E   1C                   ENDIF
057400930208     C*
057500930208     C                   ENDSR
057600931123     C*------------------------------------------------------------------------*
057700941031     C* STAZNC - STAMPA TOTALI ZONA ARRIVO
057800931123     C*------------------------------------------------------------------------*
057900930623     C     STAZNC        BEGSR
058000941202     C*
058100941202     C* AZZERO CAMPI DI LAVORO
058200941202     C                   Z-ADD     *ZEROS        WR                1 0          *NUMERO SERVIZIO
058300941205     C                   Z-ADD     *ZEROS        WTUTS                          *SPED.DEL SERVIZIO
058400941202     C                   Z-ADD     *ZEROS        WTSP                           *SCHIERA ORE SERVIZI
058500941202     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
058600930622     C*
058700930622     C* SE RICHIESTA STAMPA DETTAGLIATA STAMPO
058800931129IF  1C     PARSDA        IFEQ      'D'
058900930622     C*
059000950704     C* DESCRIZIONE TOTALE
059100941205     C                   MOVEL(P)  TES(9)        STDES1
059200941206     C                   MOVE(P)   DEPZNC        STDES2
059300930623     C*
059400930623     C* IMPOSTO A SI LA RIBATTUTA
059500941206     C                   MOVEL     '1'           *IN10
059600941206     C*
059700941206     C* IMPOSTO A SI LA RIGA SI SEPARAZIONE
059800941206     C                   MOVEL     '0'           *IN01
059900941209     C*
060000941209     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
060100941209     C     *INOF         IFEQ      '1'
060200941209     C                   EXCEPT    TESTA
060300941209     C                   SETOFF                                       OF
060400941209     C                   END
060500941202     C*--------------------
060600941202     C* RIGA SERVIZIO
060700941202     C*--------------------
060800941202     C* PER CISCUN TIPO SERVIZIO ESISTENTE
060900941202     C                   Z-ADD     *ZEROS        PASCIC            1 0          *N°PASSAGGIO CICLO
061000941202DO  2C     1             DO        WULTSP        R
061100941202     C     R             OCCUR     OZON
061200941202     C*
061300941202     C* SOLO SE MEMORIZZATO QC.
061400941202     C                   XFOOT     ZON           WSUM             15 0          *COMODO SOMMA SHIERA
061500941202IF  3C     WSUM          IFNE      *ZEROS
061600941202     C                   ADD       1             PASCIC
061700941202     C*
061800941202     C* DECODIFICHE IN STAMPA
061900941202     C     PASCIC        IFEQ      1
062000941202     C                   MOVEL(P)  STDES1        STDES            22            *TIPO DI TOTALE
062100941202     C                   ELSE
062200941202     C     PASCIC        IFEQ      2
062300941202     C                   MOVEL(P)  STDES2        STDES                          *TOTALE FIL/CAP/TERM
062400941202     C                   ELSE
062500941202     C     PASCIC        IFGT      2
062600941202     C                   MOVEL(P)  *BLANKS       STDES
062700941202     C                   ENDIF
062800941202     C                   ENDIF
062900941202     C                   ENDIF
063000941202     C*
063100941202     C                   Z-ADD     R             WR                             *CODICE SERVIZIO
063200941202     C                   XFOOT     ZON           WTUTS                          *SPEDIZ. DEL SEVIZIO
063300941202     C                   Z-ADD     ZON           WTSP                           *ORE DELIVERY SERVIZ
063400941202     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
063500941202     C                   EXSR      STASPE
063600941202E   3C                   ENDIF
063700941202     C*
063800941202E   2C                   ENDDO
063900941202     C*--------------------
064000941202     C* RIGA TOTALE
064100941202     C*--------------------
064200941202     C* SOMMA DEI TIPI SERVIZI DIVISI PER ORE DELIVERY
064300941202     C                   CLEAR                   WTSP
064400941202DO  2C     1             DO        WULTSP        R
064500941202     C     R             OCCUR     OZON
064600941202     C                   ADD       ZON           WTSP                           *ORE DELIVERY SERVIZ
064700941202E   2C                   ENDDO
064800941202     C*
064900941202     C* SOLO SE MEMORIZZATO QC.
065000941202     C                   XFOOT     WTSP          WSUM                           *COMODO SOMMA SHIERA
065100941202IF  2C     WSUM          IFNE      *ZEROS
065200941202     C*
065300941202     C* DECODIFICHE IN STAMPA
065400941202     C     PASCIC        IFLT      2                                            *COMUNQUE UN SERVIZI
065500941202     C                   MOVEL(P)  STDES2        STDES                          *LO HA STAMPATO (>1)
065600941202     C                   ELSE
065700941202     C                   MOVEL(P)  *BLANKS       STDES
065800941202     C                   ENDIF
065900941202     C*
066000941202     C                   Z-ADD     0             WR                             *SERVIZIO "TOTALE"
066100941202     C                   XFOOT     WTSP          WTUTS                          *SPEDIZIONI TOTALI
066200941202     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
066300941202     C                   EXSR      STASPE
066400941202E   2C                   ENDIF
066500941202     C*
066600931129E   1C                   ENDIF
066700930622     C*
066800930622     C                   ENDSR
066900931123     C*------------------------------------------------------------------------*
067000941031     C* STAFLA - STAMPA TOTALI FILIALE ARRIVO
067100931123     C*------------------------------------------------------------------------*
067200930623     C     STAFLA        BEGSR
067300941202     C*
067400941202     C* AZZERO CAMPI DI LAVORO
067500941202     C                   Z-ADD     *ZEROS        WR                1 0          *NUMERO SERVIZIO
067600941205     C                   Z-ADD     *ZEROS        WTUTS                          *SPED.DEL SERVIZIO
067700941202     C                   Z-ADD     *ZEROS        WTSP                           *SCHIERA ORE SERVIZI
067800941202     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
067900930622     C*
068000950629     C* DESCRIZIONE TOTALE (DIVERSA FRA ITALIA/EXPORT  E  IMPORT)
068100950629IF  2C     PARIOE        IFEQ      'M'
068200950629     C                   MOVEL(P)  TES(28)       STDES1           22
068300950629X   2C                   ELSE
068400941205     C                   MOVEL(P)  TES(10)       STDES1
068500950629E   2C                   ENDIF
068600941206     C                   MOVE(P)   DESFIL        STDES2
068700930623     C*
068800930623     C* SE STAMPA DETTAGLIATA ==> RIBATTUTA=SI
068900930623     C* SE STAMPA SINTETICA   ==> RIBATTUTA=NO
069000941205IF  1C*----      PARSDA    IFEQ 'D'
069100941205     C*----                MOVEL'1'       *IN01
069200941205X   1C*----                ELSE
069300930623     C                   MOVEL     '0'           *IN01
069400941205E   1C*----                ENDIF
069500941209     C*
069600941209     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
069700941209     C     *INOF         IFEQ      '1'
069800941209     C                   EXCEPT    TESTA
069900941209     C                   SETOFF                                       OF
070000941209     C                   END
070100941202     C*--------------------
070200941202     C* RIGA SERVIZIO
070300941202     C*--------------------
070400941202     C* PER CISCUN TIPO SERVIZIO ESISTENTE
070500941202     C                   Z-ADD     *ZEROS        PASCIC            1 0          *N°PASSAGGIO CICLO
070600941202DO  1C     1             DO        WULTSP        R
070700941202     C     R             OCCUR     OFAN
070800941202     C*
070900941202     C* SOLO SE MEMORIZZATO QC.
071000941202     C                   XFOOT     FAN           WSUM             15 0          *COMODO SOMMA SHIERA
071100941202IF  2C     WSUM          IFNE      *ZEROS
071200941202     C                   ADD       1             PASCIC
071300941202     C*
071400941202     C* DECODIFICHE IN STAMPA
071500941202     C     PASCIC        IFEQ      1
071600941202     C                   MOVEL(P)  STDES1        STDES            22            *TIPO DI TOTALE
071700941202     C                   ELSE
071800941202     C     PASCIC        IFEQ      2
071900941202     C                   MOVEL(P)  STDES2        STDES                          *TOTALE FIL/CAP/TERM
072000941202     C                   ELSE
072100941202     C     PASCIC        IFGT      2
072200941202     C                   MOVEL(P)  *BLANKS       STDES
072300941202     C                   ENDIF
072400941202     C                   ENDIF
072500941202     C                   ENDIF
072600941202     C*
072700941202     C                   Z-ADD     R             WR                             *CODICE SERVIZIO
072800941205     C                   XFOOT     FAN           WTUTS                          *SPEDIZ. DEL SEVIZIO
072900941202     C                   Z-ADD     FAN           WTSP                           *ORE DELIVERY SERVIZ
073000941202     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
073100941202     C                   EXSR      STASPE
073200941202E   2C                   ENDIF
073300941202     C*
073400941202E   1C                   ENDDO
073500941202     C*--------------------
073600941202     C* RIGA TOTALE
073700941202     C*--------------------
073800941202     C* SOMMA DEI TIPI SERVIZI DIVISI PER ORE DELIVERY
073900941202     C                   CLEAR                   WTSP
074000941202DO  1C     1             DO        WULTSP        R
074100941202     C     R             OCCUR     OFAN
074200941202     C                   ADD       FAN           WTSP                           *ORE DELIVERY SERVIZ
074300941202E   1C                   ENDDO
074400941202     C*
074500941202     C* SOLO SE MEMORIZZATO QC.
074600941202     C                   XFOOT     WTSP          WSUM                           *COMODO SOMMA SHIERA
074700941202IF  1C     WSUM          IFNE      *ZEROS
074800941202     C*
074900941202     C* DECODIFICHE IN STAMPA
075000941202     C     PASCIC        IFLT      2                                            *COMUNQUE UN SERVIZI
075100941202     C                   MOVEL(P)  STDES2        STDES                          *LO HA STAMPATO (>1)
075200941202     C                   ELSE
075300941202     C                   MOVEL(P)  *BLANKS       STDES
075400941202     C                   ENDIF
075500941202     C*
075600941202     C                   Z-ADD     0             WR                             *SERVIZIO "TOTALE"
075700941202     C                   XFOOT     WTSP          WTUTS                          *SPEDIZIONI TOTALI
075800941202     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
075900941202     C                   EXSR      STASPE
076000941202E   1C                   ENDIF
076100930622     C*
076200930622     C                   ENDSR
076300931123     C*------------------------------------------------------------------------*
076400941031     C* STAARE - STAMPA TOTALI AREA
076500931123     C*------------------------------------------------------------------------*
076600930622     C     STAARE        BEGSR
076700941202     C*
076800941202     C* AZZERO CAMPI DI LAVORO
076900941202     C                   Z-ADD     *ZEROS        WR                1 0          *NUMERO SERVIZIO
077000941205     C                   Z-ADD     *ZEROS        WTUTS                          *SPED.DEL SERVIZIO
077100941202     C                   Z-ADD     *ZEROS        WTSP                           *SCHIERA ORE SERVIZI
077200941202     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
077300930622     C*
077400930622     C* DESCRIZIONE TOTALE
077500941205     C                   MOVEL(P)  TES(11)       STDES1
077600930622     C                   EXSR      DECARE
077700941206     C                   MOVE(P)   DESARE        STDES2
077800931122     C*
077900940114     C* STAMPO I TOTALI AREA A PAGINA NUOVA SOLO PER STAMPA IN DETTAGLIO
078000940114IF  1C     PARSDA        IFEQ      'D'
078100931129     C                   MOVEL     TES(14)       STINT1
078200931129     C                   MOVEL     DESDIV        STINT2
078300940114     C                   EXCEPT    TESTA                                        *TESTATA
078400941209     C                   SETOFF                                       OF
078500940114E   1C                   ENDIF
078600930623     C*
078700931123     C* IMPOSTO A NO LA RIBATTUTA
078800931123     C                   MOVEL     '0'           *IN01
078900941209     C*
079000941209     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
079100941209     C     *INOF         IFEQ      '1'
079200941209     C                   EXCEPT    TESTA
079300941209     C                   SETOFF                                       OF
079400941209     C                   END
079500941202     C*--------------------
079600941202     C* RIGA SERVIZIO
079700941202     C*--------------------
079800941202     C* PER CISCUN TIPO SERVIZIO ESISTENTE
079900941202     C                   Z-ADD     *ZEROS        PASCIC            1 0          *N°PASSAGGIO CICLO
080000941202DO  1C     1             DO        WULTSP        R
080100941202     C     R             OCCUR     OAAN
080200941202     C*
080300941202     C* SOLO SE MEMORIZZATO QC.
080400941202     C                   XFOOT     AAN           WSUM             15 0          *COMODO SOMMA SHIERA
080500941202IF  2C     WSUM          IFNE      *ZEROS
080600941202     C                   ADD       1             PASCIC
080700941202     C*
080800941202     C* DECODIFICHE IN STAMPA
080900941202     C     PASCIC        IFEQ      1
081000941202     C                   MOVEL(P)  STDES1        STDES            22            *TIPO DI TOTALE
081100941202     C                   ELSE
081200941202     C     PASCIC        IFEQ      2
081300941202     C                   MOVEL(P)  STDES2        STDES                          *TOTALE FIL/CAP/TERM
081400941202     C                   ELSE
081500941202     C     PASCIC        IFGT      2
081600941202     C                   MOVEL(P)  *BLANKS       STDES
081700941202     C                   ENDIF
081800941202     C                   ENDIF
081900941202     C                   ENDIF
082000941202     C*
082100941202     C                   Z-ADD     R             WR                             *CODICE SERVIZIO
082200941202     C                   XFOOT     AAN           WTUTS                          *SPEDIZ. DEL SEVIZIO
082300941202     C                   Z-ADD     AAN           WTSP                           *ORE DELIVERY SERVIZ
082400941202     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
082500941202     C                   EXSR      STASPE
082600941202E   2C                   ENDIF
082700941202     C*
082800941202E   1C                   ENDDO
082900941202     C*--------------------
083000941202     C* RIGA TOTALE
083100941202     C*--------------------
083200941202     C* SOMMA DEI TIPI SERVIZI DIVISI PER ORE DELIVERY
083300941202     C                   CLEAR                   WTSP
083400941202DO  1C     1             DO        WULTSP        R
083500941202     C     R             OCCUR     OAAN
083600941202     C                   ADD       AAN           WTSP                           *ORE DELIVERY SERVIZ
083700941202E   1C                   ENDDO
083800941202     C*
083900941202     C* SOLO SE MEMORIZZATO QC.
084000941202     C                   XFOOT     WTSP          WSUM                           *COMODO SOMMA SHIERA
084100941202IF  1C     WSUM          IFNE      *ZEROS
084200941202     C*
084300941202     C* DECODIFICHE IN STAMPA
084400941202     C     PASCIC        IFLT      2                                            *COMUNQUE UN SERVIZI
084500941202     C                   MOVEL(P)  STDES2        STDES                          *LO HA STAMPATO (>1)
084600941202     C                   ELSE
084700941202     C                   MOVEL(P)  *BLANKS       STDES
084800941202     C                   ENDIF
084900941202     C*
085000941202     C                   Z-ADD     0             WR                             *SERVIZIO "TOTALE"
085100941202     C                   XFOOT     WTSP          WTUTS                          *SPEDIZIONI TOTALI
085200941202     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
085300941202     C                   EXSR      STASPE
085400941205     C* SALTO PAG SUCESSIVO
085500941205     C                   MOVEL     '1'           FLGTES                         SALTO PAGINA SUCESSI
085600941202E   1C                   ENDIF
085700941202     C*
085800930622     C                   ENDSR
085900931123     C*------------------------------------------------------------------------*
086000941031     C* STADIV - STAMPA TOTALI DIVISIONE
086100931123     C*------------------------------------------------------------------------*
086200930622     C     STADIV        BEGSR
086300941202     C*
086400941202     C* AZZERO CAMPI DI LAVORO
086500941202     C                   Z-ADD     *ZEROS        WR                1 0          *NUMERO SERVIZIO
086600941205     C                   Z-ADD     *ZEROS        WTUTS                          *SPED.DEL SERVIZIO
086700941202     C                   Z-ADD     *ZEROS        WTSP                           *SCHIERA ORE SERVIZI
086800941202     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
086900930622     C*
087000930622     C* DESCRIZIONE TOTALE
087100941205     C                   MOVEL(P)  TES(12)       STDES1
087200941206     C                   MOVE(P)   DESDIV        STDES2
087300931122     C*
087400931122     C* STAMPO LA TESTATA PER AVERE I TOTALI DIVISIONE A PAGINA NUOVA
087500931129     C                   MOVEL     TES(14)       STINT1
087600931129     C                   MOVEL     DESDIV        STINT2
087700931122     C                   EXCEPT    TESTA
087800941209     C                   SETOFF                                       OF
087900930623     C*
088000931123     C* IMPOSTO A NO LA RIBATTUTA
088100931123     C                   MOVEL     '0'           *IN01
088200941209     C*
088300941209     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
088400941209     C     *INOF         IFEQ      '1'
088500941209     C                   EXCEPT    TESTA
088600941209     C                   SETOFF                                       OF
088700941209     C                   END
088800941202     C*--------------------
088900941202     C* RIGA SERVIZIO
089000941202     C*--------------------
089100941202     C* PER CISCUN TIPO SERVIZIO ESISTENTE
089200941202     C                   Z-ADD     *ZEROS        PASCIC            1 0          *N°PASSAGGIO CICLO
089300941202DO  1C     1             DO        WULTSP        R
089400941202     C     R             OCCUR     ODAN
089500941202     C*
089600941202     C* SOLO SE MEMORIZZATO QC.
089700941202     C                   XFOOT     DAN           WSUM             15 0          *COMODO SOMMA SHIERA
089800941202IF  2C     WSUM          IFNE      *ZEROS
089900941202     C                   ADD       1             PASCIC
090000941202     C*
090100941202     C* DECODIFICHE IN STAMPA
090200941202     C     PASCIC        IFEQ      1
090300941202     C                   MOVEL(P)  STDES1        STDES            22            *TIPO DI TOTALE
090400941202     C                   ELSE
090500941202     C     PASCIC        IFEQ      2
090600941202     C                   MOVEL(P)  STDES2        STDES                          *TOTALE FIL/CAP/TERM
090700941202     C                   ELSE
090800941202     C     PASCIC        IFGT      2
090900941202     C                   MOVEL(P)  *BLANKS       STDES
091000941202     C                   ENDIF
091100941202     C                   ENDIF
091200941202     C                   ENDIF
091300941202     C*
091400941202     C                   Z-ADD     R             WR                             *CODICE SERVIZIO
091500941202     C                   XFOOT     DAN           WTUTS                          *SPEDIZ. DEL SEVIZIO
091600941202     C                   Z-ADD     DAN           WTSP                           *ORE DELIVERY SERVIZ
091700941202     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
091800941202     C                   EXSR      STASPE
091900941202E   2C                   ENDIF
092000941202     C*
092100941202E   1C                   ENDDO
092200941202     C*--------------------
092300941202     C* RIGA TOTALE
092400941202     C*--------------------
092500941202     C* SOMMA DEI TIPI SERVIZI DIVISI PER ORE DELIVERY
092600941202     C                   CLEAR                   WTSP
092700941202DO  1C     1             DO        WULTSP        R
092800941202     C     R             OCCUR     ODAN
092900941202     C                   ADD       DAN           WTSP                           *ORE DELIVERY SERVIZ
093000941202E   1C                   ENDDO
093100941202     C*
093200941202     C* SOLO SE MEMORIZZATO QC.
093300941202     C                   XFOOT     WTSP          WSUM                           *COMODO SOMMA SHIERA
093400941202IF  1C     WSUM          IFNE      *ZEROS
093500941202     C*
093600941202     C* DECODIFICHE IN STAMPA
093700941202     C     PASCIC        IFLT      2                                            *COMUNQUE UN SERVIZI
093800941202     C                   MOVEL(P)  STDES2        STDES                          *LO HA STAMPATO (>1)
093900941202     C                   ELSE
094000941202     C                   MOVEL(P)  *BLANKS       STDES
094100941202     C                   ENDIF
094200941202     C*
094300941202     C                   Z-ADD     0             WR                             *SERVIZIO "TOTALE"
094400941202     C                   XFOOT     WTSP          WTUTS                          *SPEDIZIONI TOTALI
094500941202     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
094600941202     C                   EXSR      STASPE
094700941205     C* SALTO PAG SUCESSIVO
094800941205     C                   MOVEL     '1'           FLGTES                         SALTO PAGINA SUCESSI
094900941202E   1C                   ENDIF
095000941202     C*
095100930622     C                   ENDSR
095200931123     C*------------------------------------------------------------------------*
095300941031     C* STAGEN - STAMPA TOTALI GENERALI
095400931123     C*------------------------------------------------------------------------*
095500930622     C     STAGEN        BEGSR
095600941202     C*
095700941202     C* AZZERO CAMPI DI LAVORO
095800941202     C                   Z-ADD     *ZEROS        WR                1 0          *NUMERO SERVIZIO
095900941205     C                   Z-ADD     *ZEROS        WTUTS                          *SPED.DEL SERVIZIO
096000941202     C                   Z-ADD     *ZEROS        WTSP                           *SCHIERA ORE SERVIZI
096100941202     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
096200930622     C*
096300930622     C* DESCRIZIONE TOTALE
096400941205     C                   MOVEL(P)  TES(13)       STDES1
096500941205     C                   MOVEL(P)  *BLANKS       STDES2
096600931122     C*
096700931122     C* STAMPO LA TESTATA PER AVERE I TOTALI GENERALI A PAGINA NUOVA
096800931123     C                   MOVEL     *BLANKS       STINT1
096900931123     C                   MOVEL     *BLANKS       STINT2
097000931122     C                   EXCEPT    TESTA
097100941209     C                   SETOFF                                       OF
097200930623     C*
097300931123     C* IMPOSTO A NO LA RIBATTUTA
097400931123     C                   MOVEL     '0'           *IN01
097500941209     C*
097600941209     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
097700941209     C     *INOF         IFEQ      '1'
097800941209     C                   EXCEPT    TESTA
097900941209     C                   SETOFF                                       OF
098000941209     C                   END
098100941202     C*--------------------
098200941202     C* RIGA SERVIZIO
098300941202     C*--------------------
098400941202     C* PER CISCUN TIPO SERVIZIO ESISTENTE
098500941202     C                   Z-ADD     *ZEROS        PASCIC            1 0          *N°PASSAGGIO CICLO
098600941202DO  1C     1             DO        WULTSP        R
098700941202     C     R             OCCUR     OGEN
098800941202     C*
098900941202     C* SOLO SE MEMORIZZATO QC.
099000941202     C                   XFOOT     GEN           WSUM             15 0          *COMODO SOMMA SHIERA
099100941202IF  2C     WSUM          IFNE      *ZEROS
099200941202     C                   ADD       1             PASCIC
099300941202     C*
099400941202     C* DECODIFICHE IN STAMPA
099500941202     C     PASCIC        IFEQ      1
099600941202     C                   MOVEL(P)  STDES1        STDES            22            *TIPO DI TOTALE
099700941202     C                   ELSE
099800941202     C     PASCIC        IFEQ      2
099900941202     C                   MOVEL(P)  STDES2        STDES                          *TOTALE FIL/CAP/TERM
100000941202     C                   ELSE
100100941202     C     PASCIC        IFGT      2
100200941202     C                   MOVEL(P)  *BLANKS       STDES
100300941202     C                   ENDIF
100400941202     C                   ENDIF
100500941202     C                   ENDIF
100600941202     C*
100700941202     C                   Z-ADD     R             WR                             *CODICE SERVIZIO
100800941202     C                   XFOOT     GEN           WTUTS                          *SPEDIZ. DEL SEVIZIO
100900941202     C                   Z-ADD     GEN           WTSP                           *ORE DELIVERY SERVIZ
101000941202     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
101100941202     C                   EXSR      STASPE
101200941202E   2C                   ENDIF
101300941202     C*
101400941202E   1C                   ENDDO
101500941202     C*--------------------
101600941202     C* RIGA TOTALE
101700941202     C*--------------------
101800941202     C* SOMMA DEI TIPI SERVIZI DIVISI PER ORE DELIVERY
101900941202     C                   CLEAR                   WTSP
102000941202DO  1C     1             DO        WULTSP        R
102100941202     C     R             OCCUR     OGEN
102200941202     C                   ADD       GEN           WTSP                           *ORE DELIVERY SERVIZ
102300941202E   1C                   ENDDO
102400941202     C*
102500941202     C* SOLO SE MEMORIZZATO QC.
102600941202     C                   XFOOT     WTSP          WSUM                           *COMODO SOMMA SHIERA
102700941202IF  1C     WSUM          IFNE      *ZEROS
102800941202     C*
102900941202     C* DECODIFICHE IN STAMPA
103000941202     C     PASCIC        IFLT      2                                            *COMUNQUE UN SERVIZI
103100941202     C                   MOVEL(P)  STDES2        STDES                          *LO HA STAMPATO (>1)
103200941202     C                   ELSE
103300941202     C                   MOVEL(P)  *BLANKS       STDES
103400941202     C                   ENDIF
103500941202     C*
103600941202     C                   Z-ADD     0             WR                             *SERVIZIO "TOTALE"
103700941202     C                   XFOOT     WTSP          WTUTS                          *SPEDIZIONI TOTALI
103800941202     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
103900941202     C                   EXSR      STASPE
104000941202E   1C                   ENDIF
104100930622     C*
104200930622     C                   ENDSR
104300931123     C*------------------------------------------------------------------------*
104400941031     C* STASPE - STAMPA RIGHE SPEDIZIONI E PRECENTUALI
104500931123     C*------------------------------------------------------------------------*
104600930622     C     STASPE        BEGSR
104700941202     C*
104800941202     C                   Z-ADD     *ZEROS        WRKPER           17 2
104900941202     C*
105000941202     C* PER CISCUNA ORA DI DELIVERY, CALCOLA LA PERCENTUALE DI AFFIDABILITA'
105100941202     C* (N° SPED. SERVIZIO DELL'ORA X 100) : N° SPED. SERVIZIO DI TUTTE LE ORE
105200941202DO  1C     1             DO        6             I
105300941202     C     WTSP(I)       MULT      100           WRKPER
105400941202IF  2C     WRKPER        IFNE      *ZEROS
105500941202     C     WRKPER        DIV       WTUTS         WPER(I)
105600941202X   2C                   ELSE
105700941202     C                   Z-ADD     *ZEROS        WPER(I)
105800941202E   2C                   END
105900941202E   1C                   ENDDO
106000941202     C***
106100941202     C* DECODIFICHE IN STAMPA
106200941202     C***
106300941202     C                   MOVEL     *BLANKS       DESTSP           15            *DESCRIZ. SERVIZIO
106400941202IF  1C     WR            IFNE      0
106500941205     C                   MOVEL     DTSP(WR)      DESTSP
106600941202X   1C                   ELSE
106700941202     C                   MOVEL     'TOTALI'      DESTSP
106800941202E   1C                   ENDIF
106900941202     C*
107000941202     C* STAMPO RIGA SPEDIZIONI E RIGA PERCENTUALE
107100941202     C                   EXCEPT    RIGA
107200941206     C                   MOVEL     '0'           *IN10
107300941202     C*
107400941205     C* STAMPO RIGA DI SEPARAZIONE SE:
107500941205     C*             SE RICHIESTA (*IN01 OFF)
107600941205     C*             SE ARRIVATA A STAMPARE RIGA "TOTALE"
107700941202     C     *IN01         IFEQ      '0'
107800941205     C     WR            ANDEQ     0
107900941202     C                   EXCEPT    RIGSEP
108000941202     C                   END
108100941202     C*
108200941202     C                   ENDSR
108300931123     C*------------------------------------------------------------------------*
108400941031     C* DECDIV - DECODIFICA DIVISIONE
108500931123     C*------------------------------------------------------------------------*
108600930622     C     DECDIV        BEGSR
108700930514     C*
108800020920     C                   MOVEL     1             KTBUTE
108900930622     C                   MOVE      '17'          KTBCOD
109000930514     C                   MOVE      *BLANKS       KTBKEY
109100930622     C                   MOVEL     DEPDIV        KTBKEY
109200930514     C     KEYTAB        CHAIN     TABEL                              99
109300930514     C     *IN99         IFEQ      '1'
109400930514     C     TBLFLG        OREQ      '*'
109500930622     C                   MOVEL     *ALL'*'       DESDIV           20
109600930514     C                   ELSE
109700930622     C                   MOVEL     TBLUNI        DESDIV
109800930514     C                   END
109900930514     C*
110000930514     C                   ENDSR
110100931123     C*------------------------------------------------------------------------*
110200941031     C* DECARE - DECODIFICA AREA
110300931123     C*------------------------------------------------------------------------*
110400930622     C     DECARE        BEGSR
110500930622     C*
110600020920     C                   MOVEL     1             KTBUTE
110700930622     C                   MOVE      '05'          KTBCOD
110800930622     C                   MOVE      *BLANKS       KTBKEY
110900930622     C                   MOVEL     DEPARE        KTBKEY
111000930622     C     KEYTAB        CHAIN     TABEL                              99
111100930622     C     *IN99         IFEQ      '1'
111200930622     C     TBLFLG        OREQ      '*'
111300930622     C                   MOVEL     *ALL'*'       DESARE           20
111400930622     C                   ELSE
111500930622     C                   MOVEL     TBLUNI        DESARE
111600930622     C                   END
111700930622     C*
111800930622     C                   ENDSR
111900931123     C*------------------------------------------------------------------------*
112000941031     C* DECFIL - DECODIFICA FILIALE/TERMINAL
112100931123     C*------------------------------------------------------------------------*
112200930622     C     DECFIL        BEGSR
112300930622     C*
112400930622     C                   Z-ADD     1             I
112500930622     C     WFIL          LOOKUP    FIL(I)                                 99
112600930622     C     *IN99         IFEQ      '0'
112700930622     C                   MOVEL     *ALL'*'       DESFIL           20
112800930622     C                   ELSE
112900930622     C                   MOVEL     DFIL(I)       DESFIL
113000930622     C                   END
113100930622     C*
113200930622     C                   ENDSR
113300020920     C*--------------------------------------------------------------------------------------------*
113400020920     C* REPERISCE I DATI UTENTE
113500020920     C*--------------------------------------------------------------------------------------------*
113600020920     C     REPDATIUTE    BEGSR
113700020920     C*
113800020920     C* INIZIALIZZA VARIABILI DI WRK
113900020920     C                   CLEAR                   TIBS34DS
114000020920     C                   CLEAR                   AZUTEDS
114100020920     C                   CLEAR                   DDATIUTE
114200020920     C*
114300020920     C     *DTAARA       DEFINE    §azute        azuteds
114400020920     C     *DTAARA       DEFINE    §datiute      ddatiute
114500020920     C                   IN(E)     *DTAARA
114600020920if  1C                   IF        %Error
114700020920     c                   EVAL      I34Tla = 'L'
114800020920     C                   CALL      'TIBS34R'
114900020920     C                   PARM                    Tibs34Ds
115000020920     C                   IN        *DTAARA
115100020920e   1C                   ENDIF
115200020920     C*
115300020920     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
115400020920     C                   MOVEL(P)  RSUT          DSFIRS           20
115500020920     C*
115600020920     C                   ENDSR
115700931123     C*------------------------------------------------------------------------*
115800941031     C* *INZSR - ROUTINE INIZIALE
115900931123     C*------------------------------------------------------------------------*
116000941031     C     *INZSR        BEGSR
116100941031     C*--------------------
116200930129     C* PARAMETRI IN INPUT AL PGM
116300941031     C*--------------------
116400930514     C     *ENTRY        PLIST
116500930514     C                   PARM                    KPJBA
116600950626     C                   MOVEL     KPJBU         PARAM
116700941031     C*--------------------
116800941031     C* CAMPI RIFERITI AL DATA BASE
116900941031     C*--------------------
117000930208     C     *LIKE         DEFINE    TBLKUT        KTBUTE
117100930208     C     *LIKE         DEFINE    TBLCOD        KTBCOD
117200930208     C     *LIKE         DEFINE    TBLKEY        KTBKEY
117300930526     C     *LIKE         DEFINE    ORGFIL        KORFIL
117400930623     C     *LIKE         DEFINE    WFAANN        KWAANN
117500930623     C     *LIKE         DEFINE    WFADIV        KWADIV
117600930623     C     *LIKE         DEFINE    WFAARE        KWAARE
117700930623     C     *LIKE         DEFINE    WFAFLA        KWAFLA
117800950627     C     *LIKE         DEFINE    WFAZNC        KWAZNC
117900950627     C     *LIKE         DEFINE    WFANZD        KWANZD
118000950627     C     *LIKE         DEFINE    WFACAD        KWACAD
118100930623     C     *LIKE         DEFINE    WFAMES        KWAMES
118200941202     C     *LIKE         DEFINE    WFATSP        KWATSP
118300941202     C     *LIKE         DEFINE    WFARAN        KWARAN
118400950627     C     *LIKE         DEFINE    WFAIOE        KWAIOE
118500930623     C     *LIKE         DEFINE    WFADIV        DEPDIV
118600930623     C     *LIKE         DEFINE    WFAARE        DEPARE
118700930623     C     *LIKE         DEFINE    WFAFLA        DEPFLA
118800930623     C     *LIKE         DEFINE    WFAZNC        DEPZNC
118900950627     C     *LIKE         DEFINE    WFACAP        DEPCAP
119000941031     C*--------------------
119100941031     C* CHIAVI DI LETTURA
119200941031     C*--------------------
119300930208     C* CHIAVE LETTURA TABEL00F - COMPLETA
119400930208     C     KEYTAB        KLIST
119500930208     C                   KFLD                    KTBUTE
119600930208     C                   KFLD                    KTBCOD
119700930208     C                   KFLD                    KTBKEY
119800941202     C* CHIAVE LETTURA TABEL00F - PARZIALE
119900941202     C     KEYTA2        KLIST
120000941202     C                   KFLD                    KTBUTE
120100941202     C                   KFLD                    KTBCOD
120200930526     C*
120300930526     C* CHIAVE LETTURA AZORG01L - COMPLETA
120400930526     C     KEYORG        KLIST
120500930526     C                   KFLD                    KORFIL
120600930622     C*
120700930623     C* CHIAVE LETTURA WFDLA01L - COMPLETA
120800930623     C     KEYWFA        KLIST
120900930623     C                   KFLD                    KWAANN
121000930623     C                   KFLD                    KWADIV
121100930623     C                   KFLD                    KWAARE
121200930623     C                   KFLD                    KWAFLA
121300930623     C                   KFLD                    KWAZNC
121400950627     C                   KFLD                    KWANZD
121500930623     C                   KFLD                    KWACAD
121600930623     C                   KFLD                    KWAMES
121700941202     C                   KFLD                    KWATSP
121800941202     C                   KFLD                    KWARAN
121900950627     C                   KFLD                    KWAIOE
122000930208     C*
122100930208     C* LANCIO ROUTINE PER VERIFICARE SE
122200930208     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
122300930208     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
122400020920     C                   EXSR      REPDATIUTE
122500941031     C*--------------------
122600941031     C* CAMPI DI CONTROLLO
122700941031     C*--------------------
122800941031     C* FLAG TESTATA (1-STAMPA; 0-NON STAMPA)
122900941031     C                   MOVEL     '1'           FLGTES            1
123000941031     C                   MOVEL     '0'           FINL00            1            *FINE PGM
123100941031     C                   Z-ADD     UDATE         DATA              6 0          *DATA
123200941031     C                   TIME                    ORA               6 0          *ORA
123300941205     C     *LIKE         DEFINE    WFARAN        R                              *NUMERO RANGE
123400940117     C*
123500940117     C* IMPOSTO L'INTESTAZIONE 'DETTAGLIO' O 'SINTETICA'
123600940117IF  1C     PARSDA        IFEQ      'D'
123700940117     C                   MOVEL     INT(1)        DECPAR            9
123800940117X   1C                   ELSE
123900940117     C                   MOVEL     INT(2)        DECPAR
124000940117E   1C                   ENDIF
124100950627     C*
124200950627     C* IMPOSTA L'INTESTAZIONE 'ITALIA'/'IMPORT'/'EPORT'/TUTTO
124300950627SL  1C                   SELECT
124400950627     C     PARIOE        WHENEQ    'I'
124500950627     C                   MOVEL     INT(3)        DECIOE           15
124600950627     C     PARIOE        WHENEQ    'M'
124700950627     C                   MOVEL     INT(4)        DECIOE
124800950627     C     PARIOE        WHENEQ    'X'
124900950627     C                   MOVEL     INT(5)        DECIOE
125000950627     C     PARIOE        WHENEQ    *BLANKS
125100950628     C                   MOVEL     INT(6)        DECIOE
125200950627E   1C                   ENDSL
125300940120     C*
125400940120     C* FORZO STAMPA TOTALI AREA/DIV/GEN = 'S' SE CHIESTA LA STAMPA SINTETICA
125500940120IF  1C     PARSDA        IFEQ      'S'
125600940120     C                   MOVEL     'S'           PARTOT                         *TOTALI AREA/DIV/GEN
125700940120E   1C                   ENDIF
125800930622     C*
125900930622     C* MEMORIZZA LE DESCRIZIONI DELLE FILIALI E DEI TERMINAL
126000930622     C                   EXSR      CARORG
126100941202     C*
126200941202     C* CARICA TABELLE
126300941202     C                   EXSR      CARTAB
126400930623     C*
126500930623     C* SE SONO STATE INSERITE DELLE PARZIALIZZAZIONI IMPOSTO IL FLAG 02
126600930623     C* ALTO PER STAMPARE UNA RIGA IN TESTATA CON LE PARZIALIZZ.
126700930623     C     PARDIV        IFEQ      *BLANKS
126800930623     C     PARARE        ANDEQ     *ZEROS
126900930623     C     PARFLP        ANDEQ     *ZEROS
127000930623     C     PARTFP        ANDEQ     *ZEROS
127100930623     C     PARFLA        ANDEQ     *ZEROS
127200930623     C     PARTFA        ANDEQ     *ZEROS
127300930623     C                   SETOFF                                       02
127400930623     C                   ELSE
127500930623     C                   SETON                                        02
127600930623     C                   END
127700930624     C*
127800930624     C* SE LANCIO PER SALDI INTESTA CON ANNO/MESE (IND.03 OFF)
127900930624     C* SE LANCIO ESTEMPORANEO INETASTA CON DATA INIZIO E FINE (IND.03 ON)
128000930624     C     PARTLA        IFEQ      'S'
128100930624     C                   SETOFF                                       03
128200930624     C                   END
128300930624     C     PARTLA        IFEQ      'E'
128400930624     C                   SETON                                        03
128500930624     C                   END
128600000000     C*
128700000000     C                   ENDSR
128800931123     C*------------------------------------------------------------------------*
128900941031     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
129000931123     C*------------------------------------------------------------------------*
129100930622     C     CARORG        BEGSR
129200930622     C*
129300930622     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
129400930622     C                   Z-ADD     *ZEROS        KORFIL
129500930622     C     KEYORG        SETLL     AZORG01L                           99        *FINE-ARCHIV
129600930622     C  N99              READ      AZORG01L                               99
129700930622     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
129800930622     C     *IN99         DOWEQ     '0'                                          --- 1 -->
129900930622     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
130000930622     C     ORGFAG        IFNE      'V'
130100930622     C                   ADD       1             I
130200930622     C                   Z-ADD     ORGFIL        FIL(I)
130300930622     C                   MOVEL     ORGDES        DFIL(I)
130400930622     C                   END
130500930622     C                   END                                                    <-- 2 ---
130600930622     C                   READ      AZORG01L                               99
130700930622     C                   END                                                    <-- 1 ---
130800931123     C*
130900931123     C                   ENDSR
131000941202     C*------------------------------------------------------------------------*
131100941202     C* CARTAB - CARICA TABELLE
131200941202     C*------------------------------------------------------------------------*
131300941202     C     CARTAB        BEGSR
131400941202     C*--------------------
131500941202     C* TIPI SERVIZIO
131600941202     C*--------------------
131700941202     C                   Z-ADD     *ZEROS        I
131800020920     C                   MOVEL     1             KTBUTE
131900941202     C                   MOVEL     '5E'          KTBCOD
132000941202     C     KEYTA2        SETLL     TABEL00F
132100941202     C     KEYTA2        READE     TABEL00F                               99
132200941202DO  1C     *IN99         DOWEQ     *OFF
132300941202IF  2C     TBLFLG        IFEQ      *BLANKS
132400941202     C                   ADD       1             I
132500941202     C                   MOVEL     TBLKEY        TSP(I)                         *CODICE SERVIZIO
132600941202     C                   Z-ADD     I             OTSP(I)                        *NUMERO OCUR
132700941202     C                   MOVEL     TBLUNI        DTSP(I)
132800941202E   2C                   ENDIF
132900941202     C     KEYTA2        READE     TABEL00F                               99
133000941202E   1C                   ENDDO
133100941202     C*
133200941202     C                   Z-ADD     I             WULTSP            4 0          *ULTIMO SERV.CARICAT
133300941202     C*
133400941202     C                   ENDSR
133500931123     O*------------------------------------------------------------------------*
133600941031     O* STATISTICA AFFIDABILITA' CONSEGNA
133700941031     O*------------------------------------------------------------------------*
133800930622     OPRTF198   E            TESTA          1 02
133900020920     O                       DSFIRS              20
134000950627     O                                           40 'TNSS33R '
134100940117     O                                           90 '   STATISTICA  AFFID'
134200941202     O                                          110 'ABILITA" CONSEGNA   '
134300940120     O                       DECPAR             117
134400950627     O                       DECIOE             135
134500930622     O                                          150 'Data:'
134600930622     O                       DATA               159 '  /  /  '
134700930622     O                                          167 'Ora:'
134800930622     O                       ORA                178 '  .  .  '
134900930622     O                                          190 'Pagina:'
135000930622     O                       PAGE          Z    198
135100941031     O*------------------------
135200930624     O          E   N03      TESTA          1
135300930622     O                                           90 'ANNO:'
135400941031     O                       PARANN              95
135500930622     O                                          101 'MESI:'
135600930622     O                       PARMMI             104
135700930622     O                                          106 '/'
135800930622     O                       PARMMF             109
135900941031     O*------------------------
136000930624     O          E    03      TESTA          1
136100930624     O                                           90 'DAL:'
136200941031     O                       PARDTI             100 '  /  /    '
136300930624     O                                          105 'AL:'
136400941031     O                       PARDTF             115 '  /  /    '
136500941031     O*------------------------
136600930623     O          E    02      TESTA       1  1
136700930623     O                                           20 'PARZIALIZZAZIONI:   '
136800930623     O                                           40 '             DIVIS.:'
136900930623     O                       PARDIV              41
137000930624     O                                           61 '               AREA:'
137100930624     O                       PARARE        Z     64
137200961003     O                                           84 '          P.O.PART.:'
137300930624     O                       PARFLP        Z     87
137400930624     O                                          107 '         TERM.PART.:'
137500930624     O                       PARTFP        Z    110
137600961003     O                                          130 '           P.O.ARR.:'
137700930624     O                       PARFLA        Z    133
137800930624     O                                          153 '          TERM.ARR.:'
137900930624     O                       PARTFA        Z    156
138000941031     O*------------------------
138100930622     O          E            TESTA       1  1
138200930720     O                       TES(16)             66
138300930720     O                       TES(16)            132
138400930720     O                       TES(16)            198
138500941031     O*------------------------
138600930622     O          E            TESTA          1
138700930622     O                       TES(2)              66
138800930622     O                       TES(3)             132
138900930622     O                       TES(4)             198
139000950629     O                       STINT1              23
139100941031     O*------------------------
139200930622     O          E            TESTA          1
139300930622     O                       TES(5)              66
139400930622     O                       TES(6)             132
139500930622     O                       TES(7)             198
139600950629     O                       STINT2              23
139700941031     O*------------------------
139800930622     O          E            TESTA          1
139900930720     O                       TES(16)             66
140000930720     O                       TES(16)            132
140100930720     O                       TES(16)            198
140200941205     O*------------------------------------------------------------------------*
140300941205     O* PRIMA RIGA: SOLO SE RICHIESTA RIBATTUTA (INDIC.01)
140400941206     O          E    10      RIGA           0
140500950628     O                       STDES               23
140600941205     O                                            1 ' '
140700941205     O                                           24 ' '
140800941205     O                       DESTSP              40
140900941205     O                                           42 ' '
141000941205     O                       WTUTS         Z     51
141100941205     O                                           53 ' '
141200941205     O                       WTSP(1)       Z     62
141300941205     O                       WPER(1)       2     71
141400941205     O                                           73 ' '
141500941205     O                       WTSP(2)       Z     82
141600941205     O                       WPER(2)       2     91
141700941205     O                                           93 ' '
141800941205     O                       WTSP(3)       Z    102
141900941205     O                       WPER(3)       2    111
142000941205     O                                          113 ' '
142100941205     O                       WTSP(4)       Z    122
142200941205     O                       WPER(4)       2    131
142300941205     O                                          133 ' '
142400941205     O                       WTSP(5)       Z    142
142500941205     O                       WPER(5)       2    151
142600941205     O                                          153 ' '
142700941205     O                       WTSP(6)       Z    162
142800941205     O                       WPER(6)       2    171
142900941205     O                                          198 ' '
143000941205     O*------------------------
143100941205     O* PRIMA RIGA: STAMPATA SEMPRE
143200941205     O          E            RIGA           1
143300950628     O                       STDES               23
143400941205     O                                            1 '!'
143500941205     O                                           24 '!'
143600941205     O                       DESTSP              40
143700941205     O                                           42 '!'
143800941205     O                       WTUTS         Z     51
143900941205     O                                           53 '!'
144000941205     O                       WTSP(1)       Z     62
144100941205     O                       WPER(1)       2     71
144200941205     O                                           73 '!'
144300941205     O                       WTSP(2)       Z     82
144400941205     O                       WPER(2)       2     91
144500941205     O                                           93 '!'
144600941205     O                       WTSP(3)       Z    102
144700941205     O                       WPER(3)       2    111
144800941205     O                                          113 '!'
144900941205     O                       WTSP(4)       Z    122
145000941205     O                       WPER(4)       2    131
145100941205     O                                          133 '!'
145200941205     O                       WTSP(5)       Z    142
145300941205     O                       WPER(5)       2    151
145400941205     O                                          153 '!'
145500941205     O                       WTSP(6)       Z    162
145600941205     O                       WPER(6)       2    171
145700941205     O                                          198 '!'
145800931123     O*------------------------------------------------------------------------*
145900930622     O          E            RIGSEP         1
146000930622     O                       TES(1)              66
146100930622     O                       TES(1)             132
146200930622     O                       TES(1)             198
146300931123     O*------------------------------------------------------------------------*
146400941209     O          E            STAFIN           66
146500941205     O                                          110 ' F I N E   S T A M P A '
146600931123     O*------------------------------------------------------------------------*
146700950627** -TES-   (DESCRIZIONI  STAMPA)
146800941202------------------------------------------------------------------  1
146900941202!                      !     T I P O     !  N°SPED  !     ... OK .  2
147000941202..    !     ... 12 ...    !     ... 24 ...    !     ... 48 ...      3
147100941202!     ... 72 ...    !     ... >72 ...                            !  4
147200941202!                      ! S E R V I Z I O !  TOTALI  !    N°SPE      5
147300941202  %   !    N°SPE      %   !    N°SPE      %   !    N°SPE      %     6
147400941205!    N°SPE      %   !    N°SPE      %                            !  7
147500941205  TOT. CAP DI DESTINAZ                                              8
147600941205  TOT. ZONA CONSEGNA                                                9
147700961003  TOT. P.OPERATIVO ARR                                             10
147800941205  TOT. AREA                                                        11
147900941205  TOT. DIVISIONE                                                   12
148000941205  TOT. GENERALE                                                    13
148100941205  DIVISIONE                                                        14
148200961003  P.OPERATIVO ARRIVO                                               15
148300930720================================================================== 16
148400931122------------------------------------------------------------------ 17
148500930720                                                                   18
148600930720                                                                   19
148700930720                                                                   20
148800941202!                      ! XXXXXXXXXXXXXXX ! 99999999 ! 99999999  99  A
14890094120299,99 ! 99999999  9999,99 ! 99999999  9999,99 ! 99999999  9999,99   B
149000941202! 99999999  9999,99 ! 99999999  9999,99  !                          C
149100941202                                                                   24
149200950629(DESCRIZIONI CHE OCCORRONO ALL'IMPORT PERCHE INVERTE LNP CON LNA)  25
149300950704                                                                   26
149400950704                                                                   27
149500961003  TOT. P.OPERATIVO PAR                                        (10) 28
149600961003  P.OPERATIVO PARTENZA                                        (15) 29
149700950629                                                                   30
149800950627** -INT-   (INTESTAZIONE)
149900950627DETTAGLIO           1
150000950627SINTETICA           2
150100950627ITALIA              3
150200950627IMPORT              4
150300950627EXPORT              5
150400950629COMPLETA            6
150500950627                    7
150600950627                    8
150700950627                    9
150800950627                    10
