000100000000     H*------------------------------------------------------------------------*
000200950627     H* STATISTICA AFFIDABILITA' CLIENTE - STAMPA                              *
000300941205     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FWFDLC01L  IF   E           K DISK
000900000000     FTABEL00F  IF   E           K DISK
001000000000     FAZORG01L  IF   E           K DISK
001100000000     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001200000000     F                                     FORMLEN(66)
001300000000     F                                     FORMOFL(60)
001400000000     L*------------------------------------------------------------------------*
001500000000     D*------------------------------------------------------------------------*
001600000000     D* SCHIERE
001700000000     D*------------------------------------------------------------------------*
001800000000     D*-------------------
001900000000     D* INTESTAZIONI STAMPA
002000000000     D*-------------------
002100950629     D TES             S             66    DIM(30) CTDATA PERRCD(1)             RIGHE TESTATA
002200950627     D INT             S             20    DIM(10) CTDATA PERRCD(1)             DECODIFICHE INTESTAZ
002300000000     D*-------------------
002400000000     D* SCHIERA FILIALI
002500000000     D*-------------------
002600971124     D FIL             S              3  0 DIM(500)                             *CODICE FILIALE
002700971124     D DFIL            S             20    DIM(500)                             *DESCRIZIONE FILIALE
002800000000     D*-------------------
002900000000     D* TIPI SERVIZIO
003000000000     D*-------------------
003100000000     D TSP             S              1    DIM(5)                               *TIPI SERVIZIO
003200000000     D DTSP            S             15    DIM(5)                               *DESCRIZIONE SERVIZI
003300000000     D OTSP            S              1  0 DIM(5)                               *NUMERO OCUR DEL SER
003400000000     D*------------------
003500000000     D* SCHIERA MULTIPLA ---> - 5 TIPI SERVIZIO X 7 ORE DELEIVERY -
003600000000     D*------------------
003700000000     D*-------------------
003800000000     D* SCHIERE DI LAVORO
003900000000     D*-------------------
004000941202     D WTSP            S              8  0 DIM(6)                                 *SPEDIZIONI SERVIZ
004100000000     D WPER            S              6  2 DIM(6)                                 *PERCENTUALI SERVI
004200000000     D*------------------------------------------------------------------------*
004300000000     D* INPUT
004400000000     D*------------------------------------------------------------------------*
004500000000     D*------------------
004600941202     D* SPEDIZIONI  (8 X 6) = 48
004700000000     D*------------------
004800000000     D* DS MULTIPLA GENERALE  (5 TIPI SERVIZIO X 6 ORE DELIVERY)
004900000000     D*------------------
005000941205     D OGEN            DS                  OCCURS(5) INZ
005100941202     D  GEN                    1     48  0
005200941202     D                                     DIM(6)                                 *GENERALE
005300000000     D*------------------
005400000000     D* DS MULTIPLA DIVISIONE (5 TIPI SERVIZIO X 6 ORE DELIVERY)
005500000000     D*------------------
005600941205     D ODPN            DS                  OCCURS(5) INZ
005700941202     D  DPN                    1     48  0
005800941202     D                                     DIM(6)                                 *DIVISIONE
005900000000     D*------------------
006000000000     D* DS MULTIPLA AREA      (5 TIPI SERVIZIO X 6 ORE DELIVERY)
006100000000     D*------------------
006200941205     D OAPN            DS                  OCCURS(5) INZ
006300941202     D  APN                    1     48  0
006400941202     D                                     DIM(6)                                 *AREA
006500000000     D*------------------
006600000000     D* DS MULTIPLA FILIALE PARTENZA   (5 TIPI SERVIZIO X 6 ORE DELIVERY)
006700000000     D*------------------
006800941205     D OFPN            DS                  OCCURS(5) INZ
006900941202     D  FPN                    1     48  0
007000941202     D                                     DIM(6)                                 *FILIALE PARATENZA
007100000000     D*------------------
007200000000     D* DS MULTIPLA TERMINALE ARRIVO  (5 TIPI SERVIZIO X 6 ORE DELIVERY)
007300000000     D*------------------
007400941205     D OTAN            DS                  OCCURS(5) INZ
007500941202     D  TAN                    1     48  0
007600941202     D                                     DIM(6)                                 *TERMINAL ARRIVO
007700000000     D*------------------
007800000000     D* DS MULTIPLA FILIALE ARRIVO  (5 TIPI SERVIZIO X 6 ORE DELIVERY)
007900000000     D*------------------
008000941205     D OFAN            DS                  OCCURS(5) INZ
008100941202     D  FAN                    1     48  0
008200941202     D                                     DIM(6)                                 *FILIALE ARRIVO
008300000000     D*-------------------
008400020920     D* DS REPERIMENTO DATI UTENTE
008500020920     D*---------------------
008600020920     D TIBS34DS      E DS                                                       *Profili utente
008700020920     D DDATIUTE      E DS                                                       *Dati utente
008800020920     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008900000000     D*-------------------
009000000000     D* ARCHITETTURA
009100000000     D*-------------------
009200000000     D KPJBA         E DS
009300000000     D*-------------------
009400000000     D* DS PARAMETRI DI LANCIO PGM
009500000000     D*-------------------
009600950627     D PARAM           DS
009700000000     D  PARANN                 1      4  0
009800000000     D  PARMMI                 5      6  0
009900000000     D  PARMMF                 7      8  0
010000000000     D  PARSNC                 9      9
010100000000     D  PARSDC                10     10
010200000000     D  PARSNA                11     11
010300000000     D  PARSDA                12     12
010400000000     D  PARSNR                13     13
010500000000     D  PARDIV                14     14
010600000000     D  PARARE                15     17  0
010700000000     D  PARFLP                18     20  0
010800000000     D  PARTFP                21     23  0
010900000000     D  PARFLA                24     26  0
011000000000     D  PARTFA                27     29  0
011100000000     D  PARDTI                30     37  0
011200000000     D  PARDTF                38     45  0
011300000000     D  PARTLA                46     46
011400000000     D  PARSNP                47     47
011500950712     D  PARTOT                48     48
011600950712     D  PARIOE                49     49
011700970826     D  PARCFP                50     50
011800000000     C*------------------------------------------------------------------------*
011900000000     C* MAIN LINE
012000000000     C*------------------------------------------------------------------------*
012100000000     C*
012200000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
012300000000     C                   EXSR      SETFIL
012400000000     C*
012500000000     C* CICLO FINO A FINE FILE DI LAVORO
012600000000DO  1C     FINL00        DOWEQ     '0'
012700000000     C*
012800000000     C* OPERAZIONI PER NUOVA DIVISIONE
012900000000     C                   EXSR      NEWDIV
013000000000     C*
013100000000     C* CICLO FINO A ROTTURA DI DIVISIONE
013200000000DO  2C     FINL00        DOWEQ     '0'
013300000000     C     WFCDIV        ANDEQ     DEPDIV
013400000000     C*
013500000000     C* OPERAZIONI PER NUOVA AREA
013600000000     C                   EXSR      NEWARE
013700000000     C*
013800000000     C* CICLO FINO A ROTTURA DI AREA
013900000000DO  3C     FINL00        DOWEQ     '0'
014000000000     C     WFCDIV        ANDEQ     DEPDIV
014100000000     C     WFCARE        ANDEQ     DEPARE
014200000000     C*
014300000000     C* OPERAZIONI PER NUOVA FILIALE DI PARTENZA
014400000000     C                   EXSR      NEWFLP
014500000000     C*
014600000000     C* CICLO FINO A ROTTURA DI FILIALE DI PARTENZA
014700000000DO  4C     FINL00        DOWEQ     '0'
014800000000     C     WFCDIV        ANDEQ     DEPDIV
014900000000     C     WFCARE        ANDEQ     DEPARE
015000000000     C     WFCFLP        ANDEQ     DEPFLP
015100000000     C*
015200000000     C* OPERAZIONI PER NUOVO TERMINAL DI ARRIVO
015300000000     C                   EXSR      NEWTFA
015400000000     C*
015500000000     C* CICLO FINO A ROTTURA DI TERMINAL DI ARRIVO
015600000000DO  5C     FINL00        DOWEQ     '0'
015700000000     C     WFCDIV        ANDEQ     DEPDIV
015800000000     C     WFCARE        ANDEQ     DEPARE
015900000000     C     WFCFLP        ANDEQ     DEPFLP
016000000000     C     WFCTFA        ANDEQ     DEPTFA
016100000000     C*
016200000000     C* OPERAZIONI PER NUOVA FILIALE DI ARRIVO
016300000000     C                   EXSR      NEWFLA
016400000000     C*
016500000000     C* CICLO FINO A ROTTURA DI FILIALE DI ARRIVO
016600000000DO  6C     FINL00        DOWEQ     '0'
016700000000     C     WFCDIV        ANDEQ     DEPDIV
016800000000     C     WFCARE        ANDEQ     DEPARE
016900000000     C     WFCFLP        ANDEQ     DEPFLP
017000000000     C     WFCTFA        ANDEQ     DEPTFA
017100000000     C     WFCFLA        ANDEQ     DEPFLA
017200000000     C*
017300000000     C* MEMORIZZA LE SPEDIZIONI
017400000000     C                   EXSR      MEMSPE
017500000000     C*
017600000000     C* LEGGE IL RECORD SUCCESSIVO
017700000000     C                   EXSR      LETFIL
017800000000     C*
017900000000E   6C                   ENDDO
018000000000     C*
018100000000     C* STAMPA IL TOTALE FILIALE DI ARRIVO
018200000000     C                   EXSR      STAFLA
018300000000     C*
018400000000E   5C                   ENDDO
018500000000     C*
018600000000     C* STAMPA IL TOTALE TERMINAL DI ARRIVO
018700000000     C                   EXSR      STATFA
018800000000     C*
018900000000E   4C                   ENDDO
019000000000     C*
019100000000     C* STAMPA IL TOTALE FILIALE DI PARTENZA
019200000000     C                   EXSR      STAFLP
019300000000     C*
019400000000E   3C                   ENDDO
019500000000     C*
019600000000     C* STAMPA IL TOTALE AREA SE RICHIESTA STAMPA TOTALI
019700000000IF  3C     PARTOT        IFEQ      'S'
019800000000     C                   EXSR      STAARE
019900000000E   3C                   ENDIF
020000000000     C*
020100000000E   2C                   ENDDO
020200000000     C*
020300000000     C* STAMPA IL TOTALE DIVISIONE SE RICHIESTA STAMPA TOTALI
020400000000IF  2C     PARTOT        IFEQ      'S'
020500000000     C                   EXSR      STADIV
020600000000E   2C                   ENDIF
020700000000     C*
020800000000E   1C                   ENDDO
020900000000     C*
021000000000     C* STAMPA IL TOTALE GENERALE SE RICHIESTA STAMPA TOTALI
021100000000IF  1C     PARTOT        IFEQ      'S'
021200000000     C                   EXSR      STAGEN
021300000000E   1C                   ENDIF
021400000000     C*
021500000000     C* STAMPA RIGA FINE STAMPA
021600000000     C                   EXCEPT    STAFIN
021700000000     C*
021800000000     C                   SETON                                        LR
021900000000     C*------------------------------------------------------------------------*
022000000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
022100000000     C*------------------------------------------------------------------------*
022200000000     C     SETFIL        BEGSR
022300000000     C*
022400000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
022500000000     C                   Z-ADD     PARANN        KWCANN
022600000000     C                   MOVEL     *BLANKS       KWCDIV
022700000000     C                   Z-ADD     *ZEROS        KWCARE
022800000000     C                   Z-ADD     *ZEROS        KWCFLP
022900000000     C                   Z-ADD     *ZEROS        KWCTFA
023000000000     C                   Z-ADD     *ZEROS        KWCFLA
023100000000     C                   Z-ADD     *ZEROS        KWCMES
023200000000     C                   MOVEL     *BLANKS       KWCTSP
023300000000     C                   Z-ADD     *ZEROS        KWCRAN
023400950627     C                   MOVEL     *BLANKS       KWCIOE
023500000000     C     KEYWFC        SETLL     WFDLC01L                           99        *FINE-ARCHIV
023600000000     C*
023700000000     C     *IN99         IFEQ      '1'
023800000000     C                   MOVEL     '1'           FINL00
023900000000     C                   ELSE
024000000000     C                   EXSR      LETFIL
024100000000     C                   END
024200000000     C*
024300000000     C                   ENDSR
024400000000     C*------------------------------------------------------------------------*
024500000000     C* LETFIL - LETTURA PROSSIMO RECORD
024600000000     C*------------------------------------------------------------------------*
024700000000     C     LETFIL        BEGSR
024800000000     C*
024900000000     C* LEGGO FINO A:
025000000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
025100000000     C*     - TROVATO RECORD VALIDO
025200000000     C                   MOVEL     'N'           WRECOK
025300000000     C     FINL00        DOWEQ     '0'                                          --- 1 -->
025400000000     C     WRECOK        ANDEQ     'N'
025500000000     C*
025600000000     C* LETTURE SUCCESSIVE ALLA PRIMA
025700000000     C                   READ      WFDLC01L                               99
025800000000     C*
025900000000     C     *IN99         IFEQ      '1'                                          *FINA FILE
026000000000     C     WFCANN        ORNE      PARANN                                       *SUPERATO ANNO
026100000000     C                   MOVEL     '1'           FINL00
026200000000     C                   ELSE
026300000000     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
026400000000     C                   END
026500000000     C*
026600000000     C                   END                                                    <-- 1 ---
026700000000     C*
026800000000     C                   ENDSR
026900000000     C*------------------------------------------------------------------------*
027000000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
027100000000     C*------------------------------------------------------------------------*
027200000000     C     CHKREC        BEGSR
027300000000     C*
027400000000     C                   MOVEL     'S'           WRECOK            1
027500000000     C*
027600000000     C* SE NON RIENTRA NEI MESI RICHIESTI ESCLUDO IL RECORD
027700000000     C     WFCMES        IFGE      PARMMI
027800000000     C     WFCMES        ANDLE     PARMMF
027900000000     C                   ELSE
028000000000     C                   MOVEL     'N'           WRECOK
028100000000     C                   END
028200000000     C*
028300000000     C* SE DIVISIONE DEL RECORD <> DIVISIONE RICHIESTA, ESCLUDO
028400000000     C     PARDIV        IFNE      *BLANKS
028500000000     C     WFCDIV        IFNE      PARDIV
028600000000     C                   MOVEL     'N'           WRECOK
028700000000     C                   END
028800000000     C                   END
028900000000     C*
029000000000     C* SE AREA DEL RECORD <> AREA RICHIESTA, ESCLUDO
029100000000     C     PARARE        IFNE      *ZEROS
029200000000     C     WFCARE        IFNE      PARARE
029300000000     C                   MOVEL     'N'           WRECOK
029400000000     C                   END
029500000000     C                   END
029600000000     C*
029700000000     C* SE FIL.PART. DEL RECORD <> FIL.PART RICHIESTA, ESCLUDO
029800000000     C     PARFLP        IFNE      *ZEROS
029900000000     C     WFCFLP        IFNE      PARFLP
030000000000     C                   MOVEL     'N'           WRECOK
030100000000     C                   END
030200000000     C                   END
030300000000     C*
030400000000     C* SE TER.PART. DEL RECORD <> TER.PART RICHIESTO, ESCLUDO
030500000000     C     PARTFP        IFNE      *ZEROS
030600000000     C     WFCTFP        IFNE      PARTFP
030700000000     C                   MOVEL     'N'           WRECOK
030800000000     C                   END
030900000000     C                   END
031000000000     C*
031100000000     C* SE FIL.ARR.  DEL RECORD <> FIL.ARR. RICHIESTA, ESCLUDO
031200000000     C     PARFLA        IFNE      *ZEROS
031300000000     C     WFCFLA        IFNE      PARFLA
031400000000     C                   MOVEL     'N'           WRECOK
031500000000     C                   END
031600000000     C                   END
031700000000     C*
031800000000     C* SE TERM.ARR. DEL RECORD <> TERM.ARR RICHIESTO, ESCLUDO
031900000000     C     PARTFA        IFNE      *ZEROS
032000000000     C     WFCTFA        IFNE      PARTFA
032100000000     C                   MOVEL     'N'           WRECOK
032200000000     C                   END
032300000000     C                   END
032400950627     C*
032500950627     C* CONTROLLA IL TIPO DI DATI IN STATISTICA RICHIESTO
032600950627IF  1C     PARIOE        IFNE      *BLANKS                                      *SELEZIONE
032700950627IF  2C     PARIOE        IFNE      WFCIOE
032800950627     C                   MOVEL     'N'           WRECOK
032900950627E   2C                   ENDIF
033000950627X   1C                   ELSE                                                   *NESSUNA SELEZIONE
033100950627IF  2C     WFCIOE        IFEQ      'I'                                          *'I' + 'M' = TUTTO
033200950627     C     WFCIOE        OREQ      'M'
033300950627X   2C                   ELSE
033400950627     C                   MOVEL     'N'           WRECOK
033500950627E   2C                   ENDIF
033600950627E   1C                   ENDIF
033700000000     C*
033800000000     C                   ENDSR
033900000000     C*------------------------------------------------------------------------*
034000000000     C* MEMSPE - MEMORIZZA LE SPEDIZIONI
034100000000     C*------------------------------------------------------------------------*
034200000000     C     MEMSPE        BEGSR
034300000000     C*
034400000000     C* CERCO IN NUMEOR DI OCUR DEL TIPO SERVIZIO LETTO
034500000000     C                   Z-ADD     1             I
034600000000     C                   SETOFF                                       28
034700941205     C     WFCTSP        LOOKUP    TSP(I)                                 28
034800000000IF  1C     *IN28         IFEQ      *ON                                          *SOLO SE TROVATO
034900000000     C*
035000000000     C* NUMERO RANGE ORE DELIVERY
035100000000     C                   Z-ADD     WFCRAN        R
035200000000     C*--------------------
035300000000     C* GENERALE
035400000000     C*--------------------
035500000000     C     OTSP(I)       OCCUR     OGEN
035600000000     C                   ADD       WFCNSP        GEN(R)
035700000000     C*--------------------
035800000000     C* DIVISIONE
035900000000     C*--------------------
036000000000     C     OTSP(I)       OCCUR     ODPN
036100000000     C                   ADD       WFCNSP        DPN(R)
036200000000     C*--------------------
036300000000     C* AREA
036400000000     C*--------------------
036500000000     C     OTSP(I)       OCCUR     OAPN
036600000000     C                   ADD       WFCNSP        APN(R)
036700000000     C*--------------------
036800000000     C* FILIALE PARTENZA
036900000000     C*--------------------
037000000000     C     OTSP(I)       OCCUR     OFPN
037100000000     C                   ADD       WFCNSP        FPN(R)
037200000000     C*--------------------
037300000000     C* TERMINAL ARRIVO
037400000000     C*--------------------
037500000000     C     OTSP(I)       OCCUR     OTAN
037600000000     C                   ADD       WFCNSP        TAN(R)
037700000000     C*--------------------
037800000000     C* FILIALE ARRIVO
037900000000     C*--------------------
038000000000     C     OTSP(I)       OCCUR     OFAN
038100000000     C                   ADD       WFCNSP        FAN(R)
038200000000     C*
038300000000E   1C                   ENDIF
038400000000     C*
038500000000     C                   ENDSR
038600000000     C*------------------------------------------------------------------------*
038700000000     C* NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
038800000000     C*------------------------------------------------------------------------*
038900000000     C     NEWDIV        BEGSR
039000000000     C*
039100000000     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
039200000000     C                   MOVEL     WFCDIV        DEPDIV
039300000000     C*
039400000000     C* AZZERA I TOTALI DELLA DIVISIONE
039500941205DO  1C     1             DO        WULTSP        R
039600941205     C     R             OCCUR     ODPN
039700000000     C                   CLEAR                   DPN
039800941205E   1C                   ENDDO
039900000000     C*
040000000000     C* DECODIFICA LA DIVISIONE
040100000000     C                   EXSR      DECDIV
040200000000     C*
040300000000     C* DESCRIZIONE INTESTAZIONE
040400000000     C                   MOVEL     TES(14)       STINT1           22
040500941205     C                   MOVEL     DESDIV        STINT2           20
040600000000     C*
040700000000     C                   ENDSR
040800000000     C*------------------------------------------------------------------------*
040900000000     C* NEWARE - OPERAZIONI PER NUOVA AREA
041000000000     C*------------------------------------------------------------------------*
041100000000     C     NEWARE        BEGSR
041200000000     C*
041300000000     C* MEMORIZZA LA NUOVA AREA IN UN DEPOSITO
041400000000     C                   Z-ADD     WFCARE        DEPARE
041500000000     C*
041600000000     C* AZZERA I TOTALI DELL'AREA
041700941205DO  1C     1             DO        WULTSP        R
041800941205     C     R             OCCUR     OAPN
041900000000     C                   CLEAR                   APN
042000941205E   1C                   ENDDO
042100000000     C*
042200000000     C                   ENDSR
042300000000     C*------------------------------------------------------------------------*
042400000000     C* NEWFLP - OPERAZIONI PER NUOVA FILIALE DI PARTENZA
042500000000     C*------------------------------------------------------------------------*
042600000000     C     NEWFLP        BEGSR
042700000000     C*
042800000000     C* MEMORIZZA LA NUOVA FILIALE PARTENZA IN UN DEPOSITO
042900000000     C                   Z-ADD     WFCFLP        DEPFLP
043000000000     C*
043100000000     C* AZZERA I TOTALI DELLA FILIALE PARTENZA
043200941205DO  1C     1             DO        WULTSP        R
043300941205     C     R             OCCUR     OFPN
043400000000     C                   CLEAR                   FPN
043500941205E   1C                   ENDDO
043600000000     C*
043700000000     C* DECODIFICA LA FILIALE DI PARTENZA
043800000000     C                   MOVEL     DEPFLP        WFIL
043900000000     C                   EXSR      DECFIL
044000000000     C*
044100000000     C* SE RICHIESTA STAMPA DETTAGLIATA IMPOSTO L'INTESTAZIONE
044200000000IF  1C     PARSDC        IFEQ      'D'
044300950629     C*
044400950629     C* DESCRIZIONE INTESTAZIONE (DIVERSA PER ITALIA/IMPORT  E  EXPORT)
044500950629IF  2C     PARIOE        IFEQ      'X'                                          *EXPORT
044600950629     C                   MOVEL     TES(29)       STINT1
044700950629X   2C                   ELSE
044800000000     C                   MOVEL     TES(15)       STINT1
044900950629E   2C                   ENDIF
045000941205     C                   MOVEL     DESFIL        STINT2
045100000000E   1C                   ENDIF
045200000000     C*
045300000000     C* SE RICHIESTA STAMPA DETTAGLIATA O STAMPA TESTASTA: SALTO PAGINA
045400000000IF  1C     PARSDC        IFEQ      'D'
045500000000     C     FLGTES        OREQ      '1'
045600000000     C                   EXCEPT    TESTA                                        *STAMPO TESTATA
045700941209     C                   SETOFF                                       OF
045800000000     C                   MOVEL     '0'           FLGTES                         *REIMPOSTO FLAG
045900000000E   1C                   ENDIF
046000000000     C*
046100000000     C                   ENDSR
046200000000     C*------------------------------------------------------------------------*
046300000000     C* NEWTFA - OPERAZIONI PER NUOVO TERMINAL ARRIVO
046400000000     C*------------------------------------------------------------------------*
046500000000     C     NEWTFA        BEGSR
046600000000     C*
046700000000     C* MEMORIZZA IL NUOVO TERMINAL ARRIVO IN UN DEPOSITO
046800000000     C                   Z-ADD     WFCTFA        DEPTFA
046900000000     C*
047000000000     C* AZZERA I TOTALI DEL TERMINAL ARRIVO
047100941205DO  1C     1             DO        WULTSP        R
047200941205     C     R             OCCUR     OTAN
047300000000     C                   CLEAR                   TAN
047400941205E   1C                   ENDDO
047500000000     C*
047600000000     C                   ENDSR
047700000000     C*------------------------------------------------------------------------*
047800000000     C* NEWFLA - OPERAZIONI PER NUOVA FILIALE ARRIVO
047900000000     C*------------------------------------------------------------------------*
048000000000     C     NEWFLA        BEGSR
048100000000     C*
048200000000     C* MEMORIZZA IL NUOVO TERMINAL ARRIVO IN UN DEPOSITO
048300000000     C                   Z-ADD     WFCFLA        DEPFLA
048400000000     C*
048500000000     C* AZZERA I TOTALI DEL TERMINAL ARRIVO
048600941205DO  1C     1             DO        WULTSP        R
048700941205     C     R             OCCUR     OFAN
048800000000     C                   CLEAR                   FAN
048900941205E   1C                   ENDDO
049000000000     C*
049100000000     C                   ENDSR
049200000000     C*------------------------------------------------------------------------*
049300000000     C* STAFLA - STAMPA TOTALI FILIALE ARRIVO
049400000000     C*------------------------------------------------------------------------*
049500000000     C     STAFLA        BEGSR
049600000000     C*
049700000000     C* AZZERO CAMPI DI LAVORO
049800000000     C                   Z-ADD     *ZEROS        WR                1 0          *NUMERO SERVIZIO
049900941205     C                   Z-ADD     *ZEROS        WTUTS             8 0          *SPED.DEL SERVIZIO
050000000000     C                   Z-ADD     *ZEROS        WTSP                           *SCHIERA ORE SERVIZI
050100000000     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
050200000000     C*
050300000000     C* SE RICHIESTA STAMPA DETTAGLIATA STAMPO
050400000000IF  1C     PARSDC        IFEQ      'D'
050500000000     C*
050600000000     C* DECODIFICA FILIALE
050700000000     C                   MOVEL     DEPFLA        WFIL              3 0
050800000000     C                   EXSR      DECFIL
050900000000     C*
051000950629     C* DESCRIZIONE INTESTAZIONE (DIVERSA PER ITALIA/IMPORT  E  EXPORT)
051100950629IF  2C     PARIOE        IFEQ      'X'                                          *EXPORT
051200950629     C                   MOVEL(P)  TES(26)       STDES1           22
051300950629X   2C                   ELSE
051400950629     C                   MOVEL(P)  TES(8)        STDES1           22
051500950629E   2C                   ENDIF
051600941205     C                   MOVE(P)   DESFIL        STDES2           22
051700000000     C*
051800000000     C* IMPOSTO A NO L'INDICATORE DELLA RIBATTUTA
051900000000     C                   MOVEL     '0'           *IN01
052000941209     C*
052100941209     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
052200941209     C     *INOF         IFEQ      '1'
052300941209     C                   EXCEPT    TESTA
052400941209     C                   SETOFF                                       OF
052500941209     C                   END
052600000000     C*--------------------
052700000000     C* RIGA SERVIZIO
052800000000     C*--------------------
052900000000     C* PER CISCUN TIPO SERVIZIO ESISTENTE
053000000000     C                   Z-ADD     *ZEROS        PASCIC            1 0          *N°PASSAGGIO CICLO
053100000000DO  2C     1             DO        WULTSP        R
053200000000     C     R             OCCUR     OFAN
053300000000     C*
053400000000     C* SOLO SE MEMORIZZATO QC.
053500000000     C                   XFOOT     FAN           WSUM             15 0          *COMODO SOMMA SHIERA
053600000000IF  3C     WSUM          IFNE      *ZEROS
053700000000     C                   ADD       1             PASCIC
053800000000     C*
053900000000     C* DECODIFICHE IN STAMPA
054000000000     C     PASCIC        IFEQ      1
054100000000     C                   MOVEL(P)  STDES1        STDES            22            *TIPO DI TOTALE
054200000000     C                   ELSE
054300000000     C     PASCIC        IFEQ      2
054400000000     C                   MOVEL(P)  STDES2        STDES                          *TOTALE FIL/CAP/TERM
054500000000     C                   ELSE
054600000000     C     PASCIC        IFGT      2
054700000000     C                   MOVEL(P)  *BLANKS       STDES
054800000000     C                   ENDIF
054900000000     C                   ENDIF
055000000000     C                   ENDIF
055100000000     C*
055200000000     C                   Z-ADD     R             WR                             *CODICE SERVIZIO
055300000000     C                   XFOOT     FAN           WTUTS                          *SPEDIZ. DEL SEVIZIO
055400000000     C                   Z-ADD     FAN           WTSP                           *ORE DELIVERY SERVIZ
055500000000     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
055600000000     C                   EXSR      STASPE
055700000000E   3C                   ENDIF
055800000000     C*
055900000000E   2C                   ENDDO
056000000000     C*--------------------
056100000000     C* RIGA TOTALE
056200000000     C*--------------------
056300000000     C* SOMMA DEI TIPI SERVIZI DIVISI PER ORE DELIVERY
056400000000     C                   CLEAR                   WTSP
056500000000DO  2C     1             DO        WULTSP        R
056600000000     C     R             OCCUR     OFAN
056700000000     C                   ADD       FAN           WTSP                           *ORE DELIVERY SERVIZ
056800000000E   2C                   ENDDO
056900000000     C*
057000000000     C* SOLO SE MEMORIZZATO QC.
057100000000     C                   XFOOT     WTSP          WSUM                           *COMODO SOMMA SHIERA
057200000000IF  2C     WSUM          IFNE      *ZEROS
057300000000     C*
057400000000     C* DECODIFICHE IN STAMPA
057500000000     C     PASCIC        IFLT      2                                            *COMUNQUE UN SERVIZI
057600000000     C                   MOVEL(P)  STDES2        STDES                          *LO HA STAMPATO (>1)
057700000000     C                   ELSE
057800000000     C                   MOVEL(P)  *BLANKS       STDES
057900000000     C                   ENDIF
058000000000     C*
058100000000     C                   Z-ADD     0             WR                             *SERVIZIO "TOTALE"
058200000000     C                   XFOOT     WTSP          WTUTS                          *SPEDIZIONI TOTALI
058300000000     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
058400000000     C                   EXSR      STASPE
058500000000E   2C                   ENDIF
058600000000     C*
058700000000E   1C                   ENDIF
058800000000     C*
058900000000     C                   ENDSR
059000000000     C*------------------------------------------------------------------------*
059100000000     C* STATFA - STAMPA TOTALI TERMINAL ARRIVO
059200000000     C*------------------------------------------------------------------------*
059300000000     C     STATFA        BEGSR
059400941202     C*
059500941202     C* AZZERO CAMPI DI LAVORO
059600941202     C                   Z-ADD     *ZEROS        WR                             *CODICE SERVIZIO
059700941205     C                   Z-ADD     *ZEROS        WTUTS                          *SPED.DEL SERVIZIO
059800941202     C                   Z-ADD     *ZEROS        WTSP                           *SCHIERA ORE SERVIZI
059900941202     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
060000000000     C*
060100000000     C* SE RICHIESTA STAMPA DETTAGLIATA STAMPO
060200000000IF  1C     PARSDC        IFEQ      'D'
060300000000     C*
060400000000     C* DECODIFICA IL TERMINAL
060500000000     C                   MOVEL     DEPTFA        WFIL
060600000000     C                   EXSR      DECFIL
060700000000     C*
060800950629     C* DESCRIZIONE INTESTAZIONE (DIVERSA PER ITALIA/IMPORT  E  EXPORT)
060900950629IF  2C     PARIOE        IFEQ      'X'                                          *EXPORT
061000950629     C                   MOVEL(P)  TES(27)       STDES1           22
061100950629X   2C                   ELSE
061200950629     C                   MOVEL(P)  TES(9)        STDES1
061300950629E   2C                   ENDIF
061400941205     C                   MOVE(P)   DESFIL        STDES2
061500000000     C*
061600000000     C* IMPOSTO A NO L'INDICATORE DELLA RIBATTUTA
061700000000     C                   MOVEL     '0'           *IN01
061800941209     C*
061900941209     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
062000941209     C     *INOF         IFEQ      '1'
062100941209     C                   EXCEPT    TESTA
062200941209     C                   SETOFF                                       OF
062300941209     C                   END
062400000000     C*--------------------
062500000000     C* RIGA SERVIZIO
062600000000     C*--------------------
062700000000     C* PER CISCUN TIPO SERVIZIO ESISTENTE
062800000000     C                   Z-ADD     *ZEROS        PASCIC            1 0          *N°PASSAGGIO CICLO
062900000000DO  2C     1             DO        WULTSP        R
063000000000     C     R             OCCUR     OTAN
063100000000     C*
063200000000     C* SOLO SE MEMORIZZATO QC.
063300000000     C                   XFOOT     TAN           WSUM                           *COMODO SOMMA SHIERA
063400000000IF  3C     WSUM          IFNE      *ZEROS
063500000000     C                   ADD       1             PASCIC
063600000000     C*
063700000000     C* DECODIFICHE IN STAMPA
063800000000     C     PASCIC        IFEQ      1
063900000000     C                   MOVEL(P)  STDES1        STDES            22            *TIPO DI TOTALE
064000000000     C                   ELSE
064100000000     C     PASCIC        IFEQ      2
064200000000     C                   MOVEL(P)  STDES2        STDES                          *TOTALE FIL/CAP/TERM
064300000000     C                   ELSE
064400000000     C     PASCIC        IFGT      2
064500000000     C                   MOVEL(P)  *BLANKS       STDES
064600000000     C                   ENDIF
064700000000     C                   ENDIF
064800000000     C                   ENDIF
064900000000     C*
065000000000     C                   Z-ADD     R             WR                             *CODICE SERVIZIO
065100000000     C                   XFOOT     TAN           WTUTS                          *SPEDIZ. DEL SEVIZIO
065200000000     C                   Z-ADD     TAN           WTSP                           *ORE DELIVERY SERVIZ
065300000000     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
065400000000     C                   EXSR      STASPE
065500000000E   3C                   ENDIF
065600000000     C*
065700000000E   2C                   ENDDO
065800000000     C*--------------------
065900000000     C* RIGA TOTALE
066000000000     C*--------------------
066100000000     C* SOMMA DEI TIPI SERVIZI DIVISI PER ORE DELIVERY
066200000000     C                   CLEAR                   WTSP
066300000000DO  2C     1             DO        WULTSP        R
066400000000     C     R             OCCUR     OTAN
066500000000     C                   ADD       TAN           WTSP                           *ORE DELIVERY SERVIZ
066600000000E   2C                   ENDDO
066700000000     C*
066800000000     C* SOLO SE MEMORIZZATO QC.
066900000000     C                   XFOOT     WTSP          WSUM                           *COMODO SOMMA SHIERA
067000000000IF  2C     WSUM          IFNE      *ZEROS
067100000000     C*
067200000000     C* DECODIFICHE IN STAMPA
067300000000     C     PASCIC        IFLT      2                                            *COMUNQUE UN SERVIZI
067400000000     C                   MOVEL(P)  STDES2        STDES                          *LO HA STAMPATO (>1)
067500000000     C                   ELSE
067600000000     C                   MOVEL(P)  *BLANKS       STDES
067700000000     C                   ENDIF
067800000000     C*
067900000000     C                   Z-ADD     0             WR                             *SERVIZIO "TOTALE"
068000000000     C                   XFOOT     WTSP          WTUTS                          *SPEDIZIONI TOTALI
068100000000     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
068200000000     C                   EXSR      STASPE
068300000000E   2C                   ENDIF
068400000000     C*
068500000000E   1C                   ENDIF
068600000000     C*
068700000000     C                   ENDSR
068800000000     C*------------------------------------------------------------------------*
068900000000     C* STAFLP - STAMPA TOTALI FILIALE PARTENZA
069000000000     C*------------------------------------------------------------------------*
069100000000     C     STAFLP        BEGSR
069200000000     C*
069300000000     C* DECODIFICA LA FILIALE
069400000000     C                   MOVEL     DEPFLP        WFIL
069500000000     C                   EXSR      DECFIL
069600000000     C*
069700950629     C* DESCRIZIONE INTESTAZIONE (DIVERSA PER ITALIA/IMPORT  E  EXPORT)
069800950629IF  1C     PARIOE        IFEQ      'X'                                          *EXPORT
069900950629     C                   MOVEL(P)  TES(28)       STDES1           22
070000950629X   1C                   ELSE
070100941205     C                   MOVEL(P)  TES(10)       STDES1
070200950629E   1C                   ENDIF
070300941205     C                   MOVE(P)   DESFIL        STDES2
070400000000     C*
070500000000     C* AZZERO CAMPI DI LAVORO
070600000000     C                   Z-ADD     *ZEROS        WR                             *CODICE SERVIZIO
070700941205     C                   Z-ADD     *ZEROS        WTUTS                          *SPED.DEL SERVIZIO
070800000000     C                   Z-ADD     *ZEROS        WTSP                           *SCHIERA ORE SERVIZI
070900000000     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
071000000000     C*
071100000000     C* SE STAMPA DETTAGLIATA ==> RIBATTUTA=SI
071200000000     C* SE STAMPA SINTETICA   ==> RIBATTUTA=NO
071300941205     C*-----     PARSDC    IFEQ 'D'
071400941205     C*-----               MOVEL'1'       *IN01
071500941205     C*-----               ELSE
071600000000     C                   MOVEL     '0'           *IN01
071700941205     C*-----               END
071800941209     C*
071900941209     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
072000941209     C     *INOF         IFEQ      '1'
072100941209     C                   EXCEPT    TESTA
072200941209     C                   SETOFF                                       OF
072300941209     C                   END
072400000000     C*--------------------
072500000000     C* RIGA SERVIZIO
072600000000     C*--------------------
072700000000     C* PER CISCUN TIPO SERVIZIO ESISTENTE
072800000000     C                   Z-ADD     *ZEROS        PASCIC            1 0          *N°PASSAGGIO CICLO
072900000000DO  1C     1             DO        WULTSP        R
073000000000     C     R             OCCUR     OFPN
073100000000     C*
073200000000     C* SOLO SE MEMORIZZATO QC.
073300000000     C                   XFOOT     FPN           WSUM                           *COMODO SOMMA SHIERA
073400000000IF  2C     WSUM          IFNE      *ZEROS
073500000000     C                   ADD       1             PASCIC
073600000000     C*
073700000000     C* DECODIFICHE IN STAMPA
073800000000     C     PASCIC        IFEQ      1
073900000000     C                   MOVEL(P)  STDES1        STDES            22            *TIPO DI TOTALE
074000000000     C                   ELSE
074100000000     C     PASCIC        IFEQ      2
074200000000     C                   MOVEL(P)  STDES2        STDES                          *TOTALE FIL/CAP/TERM
074300000000     C                   ELSE
074400000000     C     PASCIC        IFGT      2
074500000000     C                   MOVEL(P)  *BLANKS       STDES
074600000000     C                   ENDIF
074700000000     C                   ENDIF
074800000000     C                   ENDIF
074900000000     C*
075000000000     C                   Z-ADD     R             WR                             *CODICE SERVIZIO
075100000000     C                   XFOOT     FPN           WTUTS                          *SPEDIZ. DEL SEVIZIO
075200000000     C                   Z-ADD     FPN           WTSP                           *ORE DELIVERY SERVIZ
075300000000     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
075400000000     C                   EXSR      STASPE
075500000000E   2C                   ENDIF
075600000000     C*
075700000000E   1C                   ENDDO
075800000000     C*--------------------
075900000000     C* RIGA TOTALE
076000000000     C*--------------------
076100000000     C* SOMMA DEI TIPI SERVIZI DIVISI PER ORE DELIVERY
076200000000     C                   CLEAR                   WTSP
076300000000DO  1C     1             DO        WULTSP        R
076400000000     C     R             OCCUR     OFPN
076500000000     C                   ADD       FPN           WTSP                           *ORE DELIVERY SERVIZ
076600000000E   1C                   ENDDO
076700000000     C*
076800000000     C* SOLO SE MEMORIZZATO QC.
076900000000     C                   XFOOT     WTSP          WSUM                           *COMODO SOMMA SHIERA
077000000000IF  1C     WSUM          IFNE      *ZEROS
077100000000     C*
077200000000     C* DECODIFICHE IN STAMPA
077300000000     C     PASCIC        IFLT      2                                            *COMUNQUE UN SERVIZI
077400000000     C                   MOVEL(P)  STDES2        STDES                          *LO HA STAMPATO (>1)
077500000000     C                   ELSE
077600000000     C                   MOVEL(P)  *BLANKS       STDES
077700000000     C                   ENDIF
077800000000     C*
077900000000     C                   Z-ADD     0             WR                             *SERVIZIO "TOTALE"
078000000000     C                   XFOOT     WTSP          WTUTS                          *SPEDIZIONI TOTALI
078100000000     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
078200000000     C                   EXSR      STASPE
078300000000E   1C                   ENDIF
078400000000     C*
078500000000     C                   ENDSR
078600000000     C*------------------------------------------------------------------------*
078700000000     C* STAARE - STAMPA TOTALI AREA
078800000000     C*------------------------------------------------------------------------*
078900000000     C     STAARE        BEGSR
079000000000     C*
079100000000     C* DESCRIZIONE TOTALE
079200941205     C                   MOVEL(P)  TES(11)       STDES1
079300000000     C                   EXSR      DECARE
079400941205     C                   MOVE(P)   DESARE        STDES2
079500000000     C*
079600000000     C* STAMPO I TOTALI AREA A PAGINA NUOVA SOLO PER STAMPA IN DETTAGLIO
079700000000IF  1C     PARSDC        IFEQ      'D'
079800000000     C                   MOVEL     TES(14)       STINT1
079900941205     C                   MOVEL     DESDIV        STINT2
080000000000     C                   EXCEPT    TESTA                                        *TESTATA
080100941209     C                   SETOFF                                       OF
080200000000E   1C                   ENDIF
080300000000     C*
080400000000     C* IMPOSTO A NO LA RIBATTUTA
080500000000     C                   MOVEL     '0'           *IN01
080600941209     C*
080700941209     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
080800941209     C     *INOF         IFEQ      '1'
080900941209     C                   EXCEPT    TESTA
081000941209     C                   SETOFF                                       OF
081100941209     C                   END
081200000000     C*
081300000000     C* AZZERO CAMPI DI LAVORO
081400000000     C                   Z-ADD     *ZEROS        WR                             *CODICE SERVIZIO
081500941205     C                   Z-ADD     *ZEROS        WTUTS                          *SPED.DEL SERVIZIO
081600000000     C                   Z-ADD     *ZEROS        WTSP                           *SCHIERA ORE SERVIZI
081700000000     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
081800000000     C*--------------------
081900000000     C* RIGA SERVIZIO
082000000000     C*--------------------
082100000000     C* PER CISCUN TIPO SERVIZIO ESISTENTE
082200000000     C                   Z-ADD     *ZEROS        PASCIC            1 0          *N°PASSAGGIO CICLO
082300000000DO  1C     1             DO        WULTSP        R
082400000000     C     R             OCCUR     OAPN
082500000000     C*
082600000000     C* SOLO SE MEMORIZZATO QC.
082700000000     C                   XFOOT     APN           WSUM                           *COMODO SOMMA SHIERA
082800000000IF  2C     WSUM          IFNE      *ZEROS
082900000000     C                   ADD       1             PASCIC
083000000000     C*
083100000000     C* DECODIFICHE IN STAMPA
083200000000     C     PASCIC        IFEQ      1
083300000000     C                   MOVEL(P)  STDES1        STDES            22            *TIPO DI TOTALE
083400000000     C                   ELSE
083500000000     C     PASCIC        IFEQ      2
083600000000     C                   MOVEL(P)  STDES2        STDES                          *TOTALE FIL/CAP/TERM
083700000000     C                   ELSE
083800000000     C     PASCIC        IFGT      2
083900000000     C                   MOVEL(P)  *BLANKS       STDES
084000000000     C                   ENDIF
084100000000     C                   ENDIF
084200000000     C                   ENDIF
084300000000     C*
084400000000     C                   Z-ADD     R             WR                             *CODICE SERVIZIO
084500000000     C                   XFOOT     APN           WTUTS                          *SPEDIZ. DEL SEVIZIO
084600000000     C                   Z-ADD     APN           WTSP                           *ORE DELIVERY SERVIZ
084700000000     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
084800000000     C                   EXSR      STASPE
084900000000E   2C                   ENDIF
085000000000     C*
085100000000E   1C                   ENDDO
085200000000     C*--------------------
085300000000     C* RIGA TOTALE
085400000000     C*--------------------
085500000000     C* SOMMA DEI TIPI SERVIZI DIVISI PER ORE DELIVERY
085600000000     C                   CLEAR                   WTSP
085700000000DO  1C     1             DO        WULTSP        R
085800000000     C     R             OCCUR     OAPN
085900000000     C                   ADD       APN           WTSP                           *ORE DELIVERY SERVIZ
086000000000E   1C                   ENDDO
086100000000     C*
086200000000     C* SOLO SE MEMORIZZATO QC.
086300000000     C                   XFOOT     WTSP          WSUM                           *COMODO SOMMA SHIERA
086400000000IF  1C     WSUM          IFNE      *ZEROS
086500000000     C*
086600000000     C* DECODIFICHE IN STAMPA
086700000000     C     PASCIC        IFLT      2                                            *COMUNQUE UN SERVIZI
086800000000     C                   MOVEL(P)  STDES2        STDES                          *LO HA STAMPATO (>1)
086900000000     C                   ELSE
087000000000     C                   MOVEL(P)  *BLANKS       STDES
087100000000     C                   ENDIF
087200000000     C*
087300000000     C                   Z-ADD     0             WR                             *SERVIZIO "TOTALE"
087400000000     C                   XFOOT     WTSP          WTUTS                          *SPEDIZIONI TOTALI
087500000000     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
087600000000     C                   EXSR      STASPE
087700941205     C* SALTO PAG SUCESSIVO
087800941205     C                   MOVEL     '1'           FLGTES                         SALTO PAGINA SUCESSI
087900000000E   1C                   ENDIF
088000000000     C*
088100000000     C                   ENDSR
088200000000     C*------------------------------------------------------------------------*
088300000000     C* STADIV - STAMPA TOTALI DIVISIONE
088400000000     C*------------------------------------------------------------------------*
088500000000     C     STADIV        BEGSR
088600000000     C*
088700000000     C* DESCRIZIONE TOTALE
088800941205     C                   MOVEL(P)  TES(12)       STDES1
088900000000     C*
089000000000     C* DECODIFICA LA DIVISIONE
089100000000     C                   EXSR      DECDIV
089200941205     C                   MOVE(P)   DESDIV        STDES2
089300000000     C*
089400000000     C* STAMPO LA TESTATA PER AVERE I TOTALI DIVISIONE A PAGINA NUOVA
089500000000     C                   MOVEL     TES(14)       STINT1
089600941205     C                   MOVEL     DESDIV        STINT2
089700000000     C                   EXCEPT    TESTA
089800941209     C                   SETOFF                                       OF
089900000000     C*
090000000000     C* IMPOSTO A NO LA RIBATTUTA
090100000000     C                   MOVEL     '0'           *IN01
090200941209     C*
090300941209     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
090400941209     C     *INOF         IFEQ      '1'
090500941209     C                   EXCEPT    TESTA
090600941209     C                   SETOFF                                       OF
090700941209     C                   END
090800000000     C*
090900000000     C* AZZERO CAMPI DI LAVORO
091000000000     C                   Z-ADD     *ZEROS        WR                             *CODICE SERVIZIO
091100941205     C                   Z-ADD     *ZEROS        WTUTS                          *SPED.DEL SERVIZIO
091200000000     C                   Z-ADD     *ZEROS        WTSP                           *SCHIERA ORE SERVIZI
091300000000     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
091400000000     C*--------------------
091500000000     C* RIGA SERVIZIO
091600000000     C*--------------------
091700000000     C* PER CISCUN TIPO SERVIZIO ESISTENTE
091800000000     C                   Z-ADD     *ZEROS        PASCIC            1 0          *N°PASSAGGIO CICLO
091900000000DO  1C     1             DO        WULTSP        R
092000000000     C     R             OCCUR     ODPN
092100000000     C*
092200000000     C* SOLO SE MEMORIZZATO QC.
092300000000     C                   XFOOT     DPN           WSUM                           *COMODO SOMMA SHIERA
092400000000IF  2C     WSUM          IFNE      *ZEROS
092500000000     C                   ADD       1             PASCIC
092600000000     C*
092700000000     C* DECODIFICHE IN STAMPA
092800000000     C     PASCIC        IFEQ      1
092900000000     C                   MOVEL(P)  STDES1        STDES            22            *TIPO DI TOTALE
093000000000     C                   ELSE
093100000000     C     PASCIC        IFEQ      2
093200000000     C                   MOVEL(P)  STDES2        STDES                          *TOTALE FIL/CAP/TERM
093300000000     C                   ELSE
093400000000     C     PASCIC        IFGT      2
093500000000     C                   MOVEL(P)  *BLANKS       STDES
093600000000     C                   ENDIF
093700000000     C                   ENDIF
093800000000     C                   ENDIF
093900000000     C*
094000000000     C                   Z-ADD     R             WR                             *CODICE SERVIZIO
094100941205     C                   XFOOT     DPN           WTUTS                          *SPEDIZ. DEL SEVIZIO
094200000000     C                   Z-ADD     DPN           WTSP                           *ORE DELIVERY SERVIZ
094300000000     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
094400000000     C                   EXSR      STASPE
094500000000E   2C                   ENDIF
094600000000     C*
094700000000E   1C                   ENDDO
094800000000     C*--------------------
094900000000     C* RIGA TOTALE
095000000000     C*--------------------
095100000000     C* SOMMA DEI TIPI SERVIZI DIVISI PER ORE DELIVERY
095200000000     C                   CLEAR                   WTSP
095300000000DO  1C     1             DO        WULTSP        R
095400000000     C     R             OCCUR     ODPN
095500000000     C                   ADD       DPN           WTSP                           *ORE DELIVERY SERVIZ
095600000000E   1C                   ENDDO
095700000000     C*
095800000000     C* SOLO SE MEMORIZZATO QC.
095900000000     C                   XFOOT     WTSP          WSUM                           *COMODO SOMMA SHIERA
096000000000IF  1C     WSUM          IFNE      *ZEROS
096100000000     C*
096200000000     C* DECODIFICHE IN STAMPA
096300000000     C     PASCIC        IFLT      2                                            *COMUNQUE UN SERVIZI
096400000000     C                   MOVEL(P)  STDES2        STDES                          *LO HA STAMPATO (>1)
096500000000     C                   ELSE
096600000000     C                   MOVEL(P)  *BLANKS       STDES
096700000000     C                   ENDIF
096800000000     C*
096900000000     C                   Z-ADD     0             WR                             *SERVIZIO "TOTALE"
097000000000     C                   XFOOT     WTSP          WTUTS                          *SPEDIZIONI TOTALI
097100000000     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
097200000000     C                   EXSR      STASPE
097300941205     C* SALTP PAG SUCESSIVO
097400941205     C                   MOVEL     '1'           FLGTES                         SALTO PAGINA SUCESSI
097500000000E   1C                   ENDIF
097600000000     C*
097700000000     C                   ENDSR
097800000000     C*------------------------------------------------------------------------*
097900000000     C* STAGEN - STAMPA TOTALI GENERALI
098000000000     C*------------------------------------------------------------------------*
098100000000     C     STAGEN        BEGSR
098200000000     C*
098300000000     C* DESCRIZIONE TOTALE
098400941205     C                   MOVEL(P)  TES(13)       STDES1
098500941205     C                   MOVEL     *BLANKS       STDES2
098600000000     C*
098700000000     C* STAMPO LA TESTATA PER AVERE I TOTALI GENERALI A PAGINA NUOVA
098800000000     C                   MOVEL     *BLANKS       STINT1
098900000000     C                   MOVEL     *BLANKS       STINT2
099000000000     C                   EXCEPT    TESTA
099100941209     C                   SETOFF                                       OF
099200000000     C*
099300000000     C* IMPOSTO A NO LA RIBATTUTA
099400000000     C                   MOVEL     '0'           *IN01
099500941209     C*
099600941209     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
099700941209     C     *INOF         IFEQ      '1'
099800941209     C                   EXCEPT    TESTA
099900941209     C                   SETOFF                                       OF
100000941209     C                   END
100100000000     C*
100200000000     C* AZZERO CAMPI DI LAVORO
100300000000     C                   Z-ADD     *ZEROS        WR                             *CODICE SERVIZIO
100400941205     C                   Z-ADD     *ZEROS        WTUTS                          *SPED.DEL SERVIZIO
100500000000     C                   Z-ADD     *ZEROS        WTSP                           *SCHIERA ORE SERVIZI
100600000000     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
100700000000     C*--------------------
100800000000     C* RIGA SERVIZIO
100900000000     C*--------------------
101000000000     C* PER CISCUN TIPO SERVIZIO ESISTENTE
101100000000     C                   Z-ADD     *ZEROS        PASCIC            1 0          *N°PASSAGGIO CICLO
101200000000DO  1C     1             DO        WULTSP        R
101300000000     C     R             OCCUR     OGEN
101400000000     C*
101500000000     C* SOLO SE MEMORIZZATO QC.
101600000000     C                   XFOOT     GEN           WSUM                           *COMODO SOMMA SHIERA
101700000000IF  2C     WSUM          IFNE      *ZEROS
101800000000     C                   ADD       1             PASCIC
101900000000     C*
102000000000     C* DECODIFICHE IN STAMPA
102100000000     C     PASCIC        IFEQ      1
102200000000     C                   MOVEL(P)  STDES1        STDES            22            *TIPO DI TOTALE
102300000000     C                   ELSE
102400000000     C     PASCIC        IFEQ      2
102500000000     C                   MOVEL(P)  STDES2        STDES                          *TOTALE FIL/CAP/TERM
102600000000     C                   ELSE
102700000000     C     PASCIC        IFGT      2
102800000000     C                   MOVEL(P)  *BLANKS       STDES
102900000000     C                   ENDIF
103000000000     C                   ENDIF
103100000000     C                   ENDIF
103200000000     C*
103300000000     C                   Z-ADD     R             WR                             *CODICE SERVIZIO
103400000000     C                   XFOOT     GEN           WTUTS                          *SPEDIZ. DEL SEVIZIO
103500000000     C                   Z-ADD     GEN           WTSP                           *ORE DELIVERY SERVIZ
103600000000     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
103700000000     C                   EXSR      STASPE
103800000000E   2C                   ENDIF
103900000000     C*
104000000000E   1C                   ENDDO
104100000000     C*--------------------
104200000000     C* RIGA TOTALE
104300000000     C*--------------------
104400000000     C* SOMMA DEI TIPI SERVIZI DIVISI PER ORE DELIVERY
104500000000     C                   CLEAR                   WTSP
104600000000DO  1C     1             DO        WULTSP        R
104700000000     C     R             OCCUR     OGEN
104800000000     C                   ADD       GEN           WTSP                           *ORE DELIVERY SERVIZ
104900000000E   1C                   ENDDO
105000000000     C*
105100000000     C* SOLO SE MEMORIZZATO QC.
105200000000     C                   XFOOT     WTSP          WSUM                           *COMODO SOMMA SHIERA
105300000000IF  1C     WSUM          IFNE      *ZEROS
105400000000     C*
105500000000     C* DECODIFICHE IN STAMPA
105600000000     C     PASCIC        IFLT      2                                            *COMUNQUE UN SERVIZI
105700000000     C                   MOVEL(P)  STDES2        STDES                          *LO HA STAMPATO (>1)
105800000000     C                   ELSE
105900000000     C                   MOVEL(P)  *BLANKS       STDES
106000000000     C                   ENDIF
106100000000     C*
106200000000     C                   Z-ADD     0             WR                             *SERVIZIO "TOTALE"
106300000000     C                   XFOOT     WTSP          WTUTS                          *SPEDIZIONI TOTALI
106400000000     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
106500000000     C                   EXSR      STASPE
106600000000E   1C                   ENDIF
106700000000     C*
106800000000     C                   ENDSR
106900000000     C*------------------------------------------------------------------------*
107000000000     C* STASPE - STAMPA RIGA SPEDIZIONI E PRECENTUALI
107100000000     C*------------------------------------------------------------------------*
107200000000     C     STASPE        BEGSR
107300000000     C*
107400000000     C                   Z-ADD     *ZEROS        WRKPER           17 2
107500000000     C*
107600000000     C* PER CISCUNA ORA DI DELIVERY, CALCOLA LA PERCENTUALE DI AFFIDABILITA'
107700000000     C* (N° SPED. SERVIZIO DELL'ORA X 100) : N° SPED. SERVIZIO DI TUTTE LE ORE
107800000000DO  1C     1             DO        6             I
107900000000     C     WTSP(I)       MULT      100           WRKPER
108000000000IF  2C     WRKPER        IFNE      *ZEROS
108100000000     C     WRKPER        DIV       WTUTS         WPER(I)
108200000000X   2C                   ELSE
108300000000     C                   Z-ADD     *ZEROS        WPER(I)
108400000000E   2C                   END
108500000000E   1C                   ENDDO
108600000000     C***
108700000000     C* DECODIFICHE IN STAMPA
108800000000     C***
108900000000     C                   MOVEL     *BLANKS       DESTSP           15            *DESCRIZ. SERVIZIO
109000000000IF  1C     WR            IFNE      0
109100941205     C                   MOVEL     DTSP(WR)      DESTSP
109200000000X   1C                   ELSE
109300000000     C                   MOVEL     'TOTALI'      DESTSP
109400000000E   1C                   ENDIF
109500000000     C*
109600000000     C* STAMPO RIGA SPEDIZIONI E RIGA PERCENTUALE
109700000000     C                   EXCEPT    RIGA
109800000000     C*
109900941205     C* STAMPO RIGA DI SEPARAZIONE:
110000941205     C*        SE RICHIESTA (*IN01 OFF)
110100941205     C*        SE ARRIVATA A STAMPARE LA RIGA "TOTALE"
110200000000     C     *IN01         IFEQ      '0'
110300941205     C     WR            ANDEQ     0
110400000000     C                   EXCEPT    RIGSEP
110500000000     C                   END
110600000000     C*
110700000000     C                   ENDSR
110800000000     C*------------------------------------------------------------------------*
110900000000     C* DECDIV - DECODIFICA DIVISIONE
111000000000     C*------------------------------------------------------------------------*
111100000000     C     DECDIV        BEGSR
111200000000     C*
111300020920     C                   MOVEL     1             KTBUTE
111400000000     C                   MOVE      '17'          KTBCOD
111500000000     C                   MOVE      *BLANKS       KTBKEY
111600000000     C                   MOVEL     DEPDIV        KTBKEY
111700000000     C     KEYTAB        CHAIN     TABEL                              99
111800000000     C     *IN99         IFEQ      '1'
111900000000     C     TBLFLG        OREQ      '*'
112000000000     C                   MOVEL     *ALL'*'       DESDIV           20
112100000000     C                   ELSE
112200000000     C                   MOVEL     TBLUNI        DESDIV
112300000000     C                   END
112400000000     C*
112500000000     C                   ENDSR
112600000000     C*------------------------------------------------------------------------*
112700000000     C* DECARE - DECODIFICA AREA
112800000000     C*------------------------------------------------------------------------*
112900000000     C     DECARE        BEGSR
113000000000     C*
113100020920     C                   MOVEL     1             KTBUTE
113200000000     C                   MOVE      '05'          KTBCOD
113300000000     C                   MOVE      *BLANKS       KTBKEY
113400000000     C                   MOVEL     DEPARE        KTBKEY
113500000000     C     KEYTAB        CHAIN     TABEL                              99
113600000000     C     *IN99         IFEQ      '1'
113700000000     C     TBLFLG        OREQ      '*'
113800000000     C                   MOVEL     *ALL'*'       DESARE           20
113900000000     C                   ELSE
114000000000     C                   MOVEL     TBLUNI        DESARE
114100000000     C                   END
114200000000     C*
114300000000     C                   ENDSR
114400000000     C*------------------------------------------------------------------------*
114500000000     C* DECFIL - DECODIFICA FILIALE/TERMINAL
114600000000     C*------------------------------------------------------------------------*
114700000000     C     DECFIL        BEGSR
114800000000     C*
114900000000     C                   Z-ADD     1             I
115000000000     C     WFIL          LOOKUP    FIL(I)                                 99
115100000000     C     *IN99         IFEQ      '0'
115200000000     C                   MOVEL     *ALL'*'       DESFIL           20
115300000000     C                   ELSE
115400000000     C                   MOVEL     DFIL(I)       DESFIL
115500000000     C                   END
115600000000     C*
115700000000     C                   ENDSR
115800020920     C*--------------------------------------------------------------------------------------------*
115900020920     C* REPERISCE I DATI UTENTE
116000020920     C*--------------------------------------------------------------------------------------------*
116100020920     C     REPDATIUTE    BEGSR
116200020920     C*
116300020920     C* INIZIALIZZA VARIABILI DI WRK
116400020920     C                   CLEAR                   TIBS34DS
116500020920     C                   CLEAR                   AZUTEDS
116600020920     C                   CLEAR                   DDATIUTE
116700020920     C*
116800020920     C     *DTAARA       DEFINE    §azute        azuteds
116900020920     C     *DTAARA       DEFINE    §datiute      ddatiute
117000020920     C                   IN(E)     *DTAARA
117100020920if  1C                   IF        %Error
117200020920     c                   EVAL      I34Tla = 'L'
117300020920     C                   CALL      'TIBS34R'
117400020920     C                   PARM                    Tibs34Ds
117500020920     C                   IN        *DTAARA
117600020920e   1C                   ENDIF
117700020920     C*
117800020920     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
117900020920     C                   MOVEL(P)  RSUT          DSFIRS           20
118000020920     C*
118100020920     C                   ENDSR
118200000000     C*------------------------------------------------------------------------*
118300000000     C* *INZSR - ROUTINE INIZIALE
118400000000     C*------------------------------------------------------------------------*
118500000000     C     *INZSR        BEGSR
118600000000     C*--------------------
118700000000     C* RICEVIMENTO PARAMETRI
118800000000     C*--------------------
118900000000     C     *ENTRY        PLIST
119000000000     C                   PARM                    KPJBA
119100950627     C                   MOVEL     KPJBU         PARAM
119200000000     C*--------------------
119300000000     C* CAMPI RIFERITI AL DATA BASE
119400000000     C*--------------------
119500000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
119600000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
119700000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
119800000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
119900000000     C     *LIKE         DEFINE    WFCANN        KWCANN
120000000000     C     *LIKE         DEFINE    WFCDIV        KWCDIV
120100000000     C     *LIKE         DEFINE    WFCARE        KWCARE
120200000000     C     *LIKE         DEFINE    WFCFLP        KWCFLP
120300000000     C     *LIKE         DEFINE    WFCTFA        KWCTFA
120400000000     C     *LIKE         DEFINE    WFCFLA        KWCFLA
120500000000     C     *LIKE         DEFINE    WFCMES        KWCMES
120600000000     C     *LIKE         DEFINE    WFCTSP        KWCTSP
120700000000     C     *LIKE         DEFINE    WFCRAN        KWCRAN
120800950627     C     *LIKE         DEFINE    WFCIOE        KWCIOE
120900000000     C     *LIKE         DEFINE    WFCDIV        DEPDIV
121000000000     C     *LIKE         DEFINE    WFCARE        DEPARE
121100000000     C     *LIKE         DEFINE    WFCFLP        DEPFLP
121200000000     C     *LIKE         DEFINE    WFCTFA        DEPTFA
121300000000     C     *LIKE         DEFINE    WFCFLA        DEPFLA
121400000000     C*--------------------
121500000000     C* CHIAVI DI LETTURA
121600000000     C*--------------------
121700000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
121800000000     C     KEYTAB        KLIST
121900000000     C                   KFLD                    KTBUTE
122000000000     C                   KFLD                    KTBCOD
122100000000     C                   KFLD                    KTBKEY
122200000000     C*
122300000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
122400000000     C     KEYTA2        KLIST
122500000000     C                   KFLD                    KTBUTE
122600000000     C                   KFLD                    KTBCOD
122700000000     C*
122800000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
122900000000     C     KEYORG        KLIST
123000000000     C                   KFLD                    KORFIL
123100000000     C*
123200000000     C* CHIAVE LETTURA WFDLC01L - COMPLETA
123300000000     C     KEYWFC        KLIST
123400000000     C                   KFLD                    KWCANN
123500000000     C                   KFLD                    KWCDIV
123600000000     C                   KFLD                    KWCARE
123700000000     C                   KFLD                    KWCFLP
123800000000     C                   KFLD                    KWCTFA
123900000000     C                   KFLD                    KWCFLA
124000000000     C                   KFLD                    KWCMES
124100000000     C                   KFLD                    KWCTSP
124200950627     C                   KFLD                    KWCRAN
124300950627     C                   KFLD                    KWCIOE
124400000000     C*--------------------
124500000000     C* CAMPI DI CONTROLLO
124600000000     C*--------------------
124700000000     C* FLAG TESTATA (1-STAMPA; 0-NON STAMPA)
124800000000     C                   MOVEL     '1'           FLGTES            1
124900000000     C                   MOVEL     '0'           FINL00            1            *FINE PGM
125000000000     C                   Z-ADD     UDATE         DATA              6 0          *DATA
125100000000     C                   TIME                    ORA               6 0          *ORA
125200000000     C     *LIKE         DEFINE    WFCRAN        R                              *NUMERO RANGE
125300000000     C*
125400000000     C* LANCIO ROUTINE PER VERIFICARE SE
125500000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
125600000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
125700020920     C                   EXSR      REPDATIUTE
125800000000     C*
125900950627     C* IMPOSTA L'INTESTAZIONE 'DETTAGLIO' O 'SINTETICA'
126000950627IF  1C     PARSDC        IFEQ      'D'
126100950627     C                   MOVEL     INT(1)        DECPAR            9
126200950627X   1C                   ELSE
126300950627     C                   MOVEL     INT(2)        DECPAR
126400950627E   1C                   ENDIF
126500950627     C*
126600950627     C* IMPOSTA L'INTESTAZIONE 'ITALIA'/'IMPORT'/'EPORT'/TUTTO
126700950627SL  1C                   SELECT
126800950627     C     PARIOE        WHENEQ    'I'
126900950627     C                   MOVEL     INT(3)        DECIOE           15
127000950627     C     PARIOE        WHENEQ    'M'
127100950627     C                   MOVEL     INT(4)        DECIOE
127200950627     C     PARIOE        WHENEQ    'X'
127300950627     C                   MOVEL     INT(5)        DECIOE
127400950627     C     PARIOE        WHENEQ    *BLANKS
127500950628     C                   MOVEL     INT(6)        DECIOE
127600950627E   1C                   ENDSL
127700950627     C*
127800000000     C* FORZO STAMPA TOTALI AREA/DIV/GEN = 'S' SE CHIESTA LA STAMPA SINTETICA
127900000000IF  1C     PARSDC        IFEQ      'S'
128000000000     C                   MOVEL     'S'           PARTOT                         *TOTALI AREA/DIV/GEN
128100000000E   1C                   ENDIF
128200000000     C*
128300000000     C* MEMORIZZA LE DESCRIZIONI DELLE FILIALI E DEI TERMINAL
128400000000     C                   EXSR      CARORG
128500000000     C*
128600000000     C* CARICA TABELLE
128700000000     C                   EXSR      CARTAB
128800000000     C*
128900000000     C* SE SONO STATE INSERITE DELLE PARZIALIZZAZIONI IMPOSTO IL FLAG 02 ON
129000000000     C* PER STAMPARE UNA RIGA IN TESTATA CON LE PARZIALIZZIONI
129100000000     C     PARDIV        IFEQ      *BLANKS
129200000000     C     PARARE        ANDEQ     *ZEROS
129300000000     C     PARFLP        ANDEQ     *ZEROS
129400000000     C     PARTFP        ANDEQ     *ZEROS
129500000000     C     PARFLA        ANDEQ     *ZEROS
129600000000     C     PARTFA        ANDEQ     *ZEROS
129700000000     C                   SETOFF                                       02
129800000000     C                   ELSE
129900000000     C                   SETON                                        02
130000000000     C                   END
130100000000     C*
130200000000     C* SE LANCIO PER SALDI INTESTA STAMPE CON ANNO/MESE (IND.03 OFF)
130300000000     C* SE LANCIO ESTEMPORANEO INTESTA CON DATA INIZIO E FINE (IND.03 ON)
130400000000     C     PARTLA        IFEQ      'S'
130500000000     C                   SETOFF                                       03
130600000000     C                   END
130700000000     C     PARTLA        IFEQ      'E'
130800000000     C                   SETON                                        03
130900000000     C                   END
131000000000     C*
131100000000     C                   ENDSR
131200000000     C*------------------------------------------------------------------------*
131300000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
131400000000     C*------------------------------------------------------------------------*
131500000000     C     CARORG        BEGSR
131600000000     C*
131700000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
131800000000     C                   Z-ADD     *ZEROS        KORFIL
131900000000     C     KEYORG        SETLL     AZORG01L                           99        *FINE-ARCHIV
132000000000     C  N99              READ      AZORG01L                               99
132100000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
132200000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
132300000000     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
132400000000     C     ORGFAG        IFNE      'V'
132500000000     C                   ADD       1             I
132600000000     C                   Z-ADD     ORGFIL        FIL(I)
132700000000     C                   MOVEL     ORGDES        DFIL(I)
132800000000     C                   END
132900000000     C                   END                                                    <-- 2 ---
133000000000     C                   READ      AZORG01L                               99
133100000000     C                   END                                                    <-- 1 ---
133200000000     C*
133300000000     C                   ENDSR
133400000000     C*------------------------------------------------------------------------*
133500000000     C* CARTAB - CARICA TABELLE
133600000000     C*------------------------------------------------------------------------*
133700000000     C     CARTAB        BEGSR
133800000000     C*--------------------
133900000000     C* TIPI SERVIZIO
134000000000     C*--------------------
134100000000     C                   Z-ADD     *ZEROS        I
134200020920     C                   MOVEL     1             KTBUTE
134300000000     C                   MOVEL     '5E'          KTBCOD
134400000000     C     KEYTA2        SETLL     TABEL00F
134500000000     C     KEYTA2        READE     TABEL00F                               99
134600000000DO  1C     *IN99         DOWEQ     *OFF
134700000000IF  2C     TBLFLG        IFEQ      *BLANKS
134800000000     C                   ADD       1             I
134900000000     C                   MOVEL     TBLKEY        TSP(I)                         *CODICE SERVIZIO
135000000000     C                   Z-ADD     I             OTSP(I)                        *NUMERO OCUR
135100000000     C                   MOVEL     TBLUNI        DTSP(I)
135200000000E   2C                   ENDIF
135300000000     C     KEYTA2        READE     TABEL00F                               99
135400000000E   1C                   ENDDO
135500000000     C*
135600000000     C                   Z-ADD     I             WULTSP            4 0          *ULTIMO SERV.CARICAT
135700000000     C*
135800000000     C                   ENDSR
135900000000     O*------------------------------------------------------------------------*
136000000000     O* STATISTICA AFFIDABILITA' CLIENTE
136100000000     O*------------------------------------------------------------------------*
136200000000     OPRTF198   E            TESTA          1 02
136300020920     O                       DSFIRS              20
136400950627     O                                           40 'TNSS32R '
136500000000     O                                           90 '   STATISTICA  AFFID'
136600000000     O                                          110 'ABILITA" CLIENTE    '
136700000000     O                       DECPAR             117
136800950627     O                       DECIOE             135
136900000000     O                                          150 'Data:'
137000000000     O                       DATA               159 '  /  /  '
137100000000     O                                          167 'Ora:'
137200000000     O                       ORA                178 '  .  .  '
137300000000     O                                          190 'Pagina:'
137400000000     O                       PAGE          Z    198
137500000000     O*------------------------
137600000000     O          E   N03      TESTA          1
137700000000     O                                           90 'ANNO:'
137800000000     O                       PARANN              95
137900000000     O                                          101 'MESI:'
138000000000     O                       PARMMI             104
138100000000     O                                          106 '/'
138200000000     O                       PARMMF             109
138300000000     O*------------------------
138400000000     O          E    03      TESTA          1
138500000000     O                                           90 'DAL:'
138600000000     O                       PARDTI             100 '  /  /    '
138700000000     O                                          105 'AL:'
138800000000     O                       PARDTF             115 '  /  /    '
138900000000     O*------------------------
139000000000     O          E    02      TESTA       1  1
139100000000     O                                           20 'PARZIALIZZAZIONI:   '
139200000000     O                                           40 '             DIVIS.:'
139300000000     O                       PARDIV              41
139400000000     O                                           61 '               AREA:'
139500000000     O                       PARARE        Z     64
139600961003     O                                           84 '          P.O.PART.:'
139700000000     O                       PARFLP        Z     87
139800000000     O                                          107 '         TERM.PART.:'
139900000000     O                       PARTFP        Z    110
140000961003     O                                          130 '           P.O.ARR.:'
140100000000     O                       PARFLA        Z    133
140200000000     O                                          153 '          TERM.ARR.:'
140300000000     O                       PARTFA        Z    156
140400000000     O*------------------------
140500000000     O          E            TESTA       1  1
140600000000     O                       TES(16)             66
140700000000     O                       TES(16)            132
140800000000     O                       TES(16)            198
140900000000     O*------------------------
141000000000     O          E            TESTA          1
141100000000     O                       TES(2)              66
141200000000     O                       TES(3)             132
141300000000     O                       TES(4)             198
141400950629     O                       STINT1              23
141500000000     O*------------------------
141600000000     O          E            TESTA          1
141700000000     O                       TES(5)              66
141800000000     O                       TES(6)             132
141900000000     O                       TES(7)             198
142000950629     O                       STINT2              23
142100000000     O*------------------------
142200000000     O          E            TESTA          1
142300000000     O                       TES(16)             66
142400000000     O                       TES(16)            132
142500000000     O                       TES(16)            198
142600000000     O*------------------------------------------------------------------------*
142700000000     O* PRIMA RIGA: SOLO SE RICHIESTA RIBATTUTA (INDIC.01)
142800000000     O          E    01      RIGA           0
142900950628     O                       STDES               23
143000941205     O                                            1 ' '
143100941205     O                                           24 ' '
143200000000     O                       DESTSP              40
143300941205     O                                           42 ' '
143400000000     O                       WTUTS         Z     51
143500941205     O                                           53 ' '
143600000000     O                       WTSP(1)       Z     62
143700000000     O                       WPER(1)       2     71
143800941205     O                                           73 ' '
143900000000     O                       WTSP(2)       Z     82
144000000000     O                       WPER(2)       2     91
144100941205     O                                           93 ' '
144200000000     O                       WTSP(3)       Z    102
144300000000     O                       WPER(3)       2    111
144400941205     O                                          113 ' '
144500000000     O                       WTSP(4)       Z    122
144600000000     O                       WPER(4)       2    131
144700941205     O                                          133 ' '
144800000000     O                       WTSP(5)       Z    142
144900000000     O                       WPER(5)       2    151
145000941205     O                                          153 ' '
145100000000     O                       WTSP(6)       Z    162
145200000000     O                       WPER(6)       2    171
145300941205     O                                          198 ' '
145400000000     O*------------------------
145500000000     O* PRIMA RIGA: STAMPATA SEMPRE
145600000000     O          E            RIGA           1
145700950628     O                       STDES               23
145800941205     O                                            1 '!'
145900941205     O                                           24 '!'
146000941205     O                       DESTSP              40
146100941205     O                                           42 '!'
146200941205     O                       WTUTS         Z     51
146300941205     O                                           53 '!'
146400941205     O                       WTSP(1)       Z     62
146500941205     O                       WPER(1)       2     71
146600941205     O                                           73 '!'
146700941205     O                       WTSP(2)       Z     82
146800941205     O                       WPER(2)       2     91
146900941205     O                                           93 '!'
147000941205     O                       WTSP(3)       Z    102
147100941205     O                       WPER(3)       2    111
147200941205     O                                          113 '!'
147300941205     O                       WTSP(4)       Z    122
147400941205     O                       WPER(4)       2    131
147500941205     O                                          133 '!'
147600941205     O                       WTSP(5)       Z    142
147700941205     O                       WPER(5)       2    151
147800941205     O                                          153 '!'
147900941205     O                       WTSP(6)       Z    162
148000941205     O                       WPER(6)       2    171
148100941205     O                                          198 '!'
148200000000     O*------------------------------------------------------------------------*
148300000000     O          E            RIGSEP         1
148400000000     O                       TES(1)              66
148500000000     O                       TES(1)             132
148600000000     O                       TES(1)             198
148700000000     O*------------------------------------------------------------------------*
148800941209     O          E            STAFIN           66
148900000000     O                                          110 ' F I N E   S T A M P A '
149000000000     O*------------------------------------------------------------------------*
149100950627** -TES-   (DESCRIZIONI  STAMPA)
149200000000------------------------------------------------------------------  1
149300000000!                      !     T I P O     !  N°SPED  !     ... OK .  2
149400000000..    !     ... 12 ...    !     ... 24 ...    !     ... 48 ...      3
149500000000!     ... 72 ...    !     ... >72 ...                            !  4
149600000000!                      ! S E R V I Z I O !  TOTALI  !    N°SPE      5
149700000000  %   !    N°SPE      %   !    N°SPE      %   !    N°SPE      %     6
149800941205!    N°SPE      %   !    N°SPE      %                            !  7
149900961003  TOT. P.OPERATIVO ARR                                              8
150000961003  TOT. TERMINAL ARRIVO                                              9
150100961003  TOT. P.OPERATIVO PAR                                              10
150200941205  TOT. AREA                                                         11
150300941205  TOT. DIVISIONE                                                    12
150400941205  TOT. GENERALE                                                     13
150500941205  DIVISIONE                                                         14
150600961003  P.OPERATIVO PARTENZA                                              15
150700941205==================================================================  16
150800941205------------------------------------------------------------------  17
150900950627                                                                    18
151000950627                                                                    19
151100950627                                                                    20
151200000000!                      ! XXXXXXXXXXXXXXX ! 99999999 ! 99999999  99  A
15130000000099,99 ! 99999999  9999,99 ! 99999999  9999,99 ! 99999999  9999,99   B
151400000000! 99999999  9999,99 ! 99999999  9999,99  !                          C
151500950627                                                                    24
151600950629(DESCRIZIONI CHE OCCORRONO ALLA EXPORT PERCHE'INVERTE LNA CON LNP)  25
151700980223  TOT. P.OPER. PARTENZA                                        ( 8) 26
151800950629  TOT. TERMINAL PARTENZA                                       ( 9) 27
151900980223  TOT. P.OPER. ARRIVO                                          (10) 28
152000980223  P.OPER. DI ARRIVO                                            (15) 29
15210095062930
152200950627** -INT-   (INTESTAZIONE)
152300950627DETTAGLIO           1
152400950627SINTETICA           2
152500950627ITALIA              3
152600950627IMPORT              4
152700950627EXPORT              5
152800950629COMPLETA            6
152900950627                    7
153000950627                    8
153100950627                    9
153200950627                    10
