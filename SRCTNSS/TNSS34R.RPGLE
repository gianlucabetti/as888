000100931122     H*------------------------------------------------------------------------*
000200950627     H* STATISTICA AFFIDABILITA' PARTENZA - STAMPA                             *
000300931122     H*------------------------------------------------------------------------*
000400931122     H DECEDIT('0,') DATEDIT(*DMY.)
000500941031     F*------------------------------------------------------------------------*
000600941031     F* DATA BASE
000700941031     F*------------------------------------------------------------------------*
000800931122     FWFDLP01L  IF   E           K DISK
000900930208     FTABEL00F  IF   E           K DISK
001000930526     FAZORG01L  IF   E           K DISK
001100930622     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001200930622     F                                     FORMLEN(66)
001300930622     F                                     FORMOFL(60)
001400931122     L*------------------------------------------------------------------------*
001500941031     D*------------------------------------------------------------------------*
001600941031     D* SCHIERE
001700941031     D*------------------------------------------------------------------------*
001800941031     D*-------------------
001900930622     D* INTESTAZIONI STAMPA
002000941031     D*-------------------
002100950629     D TES             S             66    DIM(30) CTDATA PERRCD(1)             RIGHE DI TESTATA
002200950627     D INT             S             20    DIM(10) CTDATA PERRCD(1)             DECODIFICHE INTESTAZ
002300941031     D*-------------------
002400930622     D* SCHIERA FILIALI
002500941031     D*-------------------
002600971124     D FIL             S              3  0 DIM(500)                             *CODICE FILIALE
002700971124     D DFIL            S             20    DIM(500)                             *DESCRIZIONE FILIALE
002800941205     D*-------------------
002900941205     D* TIPI SERVIZIO
003000941205     D*-------------------
003100941205     D TSP             S              1    DIM(5)                               *TIPI SERVIZIO
003200941205     D DTSP            S             15    DIM(5)                               *DESCRIZIONE SERVIZI
003300941205     D OTSP            S              1  0 DIM(5)                               *NUMERO OCUR DEL SER
003400941205     D*------------------
003500941205     D* SCHIERA MULTIPLA ---> - 5 TIPI SERVIZIO X 7 ORE DELEIVERY -
003600941205     D*------------------
003700941205     D*-------------------
003800941205     D* SCHIERE DI LAVORO
003900941205     D*-------------------
004000941205     D WTSP            S              8  0 DIM(6)                                 *SPEDIZIONI SERVIZ
004100941205     D WPER            S              6  2 DIM(6)                                 *PERCENTUALI SERVI
004200931122     D*------------------------------------------------------------------------*
004300941031     D* INPUT
004400941031     D*------------------------------------------------------------------------*
004500941205     D*------------------
004600941205     D* SPEDIZIONI  (8 X 6) = 48
004700941205     D*------------------
004800941205     D* DS MULTIPLA GENERALE  (5 TIPI SERVIZIO X 6 ORE DELIVERY)
004900941205     D*------------------
005000941205     D OGEN            DS                  OCCURS(5) INZ
005100941205     D  GEN                    1     48  0
005200941205     D                                     DIM(6)                                 *GENERALE
005300941205     D*------------------
005400941205     D* DS MULTIPLA DIVISIONE (5 TIPI SERVIZIO X 6 ORE DELIVERY)
005500941205     D*------------------
005600941205     D ODPN            DS                  OCCURS(5) INZ
005700941205     D  DPN                    1     48  0
005800941205     D                                     DIM(6)                                 *DIVISIONE
005900941205     D*------------------
006000941205     D* DS MULTIPLA AREA      (5 TIPI SERVIZIO X 6 ORE DELIVERY)
006100941205     D*------------------
006200941205     D OAPN            DS                  OCCURS(5) INZ
006300941205     D  APN                    1     48  0
006400941205     D                                     DIM(6)                                 *AREA
006500941205     D*------------------
006600941205     D* DS MULTIPLA FILIALE   (5 TIPI SERVIZIO X 6 ORE DELIVERY)
006700941205     D*------------------
006800941205     D OFPN            DS                  OCCURS(5) INZ
006900941205     D  FPN                    1     48  0
007000941205     D                                     DIM(6)                                 *FILIALE
007100941031     D*-------------------
007200020920     D* DS REPERIMENTO DATI UTENTE
007300020920     D*---------------------
007400020920     D TIBS34DS      E DS                                                       *Profili utente
007500020920     D DDATIUTE      E DS                                                       *Dati utente
007600020920     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007700941031     D*-------------------
007800941031     D* ARCHITETTURA
007900941031     D*-------------------
008000000000     D KPJBA         E DS
008100941031     D*-------------------
008200930208     D* DS PARAMETRI DI LANCIO PGM
008300941031     D*-------------------
008400950626     D PARAM           DS
008500941031     D  PARANN                 1      4  0
008600941031     D  PARMMI                 5      6  0
008700941031     D  PARMMF                 7      8  0
008800941031     D  PARSNC                 9      9
008900941031     D  PARSDC                10     10
009000941031     D  PARSNA                11     11
009100941031     D  PARSDA                12     12
009200941031     D  PARSNR                13     13
009300941031     D  PARDIV                14     14
009400941031     D  PARARE                15     17  0
009500941031     D  PARFLP                18     20  0
009600941031     D  PARTFP                21     23  0
009700941031     D  PARFLA                24     26  0
009800941031     D  PARTFA                27     29  0
009900941031     D  PARDTI                30     37  0
010000941031     D  PARDTF                38     45  0
010100941031     D  PARTLA                46     46
010200941031     D  PARSNP                47     47
010300950712     D  PARTOT                48     48
010400950712     D  PARIOE                49     49
010500970826     D  PARCFP                50     50
010600931122     C*------------------------------------------------------------------------*
010700941031     C* MAIN LINE
010800941031     C*------------------------------------------------------------------------*
010900000000     C*
011000930129     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
011100930208     C                   EXSR      SETFIL
011200930128     C*
011300930317     C* CICLO FINO A FINE FILE DI LAVORO
011400930208     C     FINL00        DOWEQ     '0'                                          --- 1 -->
011500930623     C*
011600930623     C* OPERAZIONI PER NUOVA DIVISIONE
011700930623     C                   EXSR      NEWDIV
011800930317     C*
011900930622     C* CICLO FINO A ROTTURA DI DIVISIONE
012000930317     C     FINL00        DOWEQ     '0'                                          --- 2 -->
012100931122     C     WFPDIV        ANDEQ     DEPDIV
012200930623     C*
012300930623     C* OPERAZIONI PER NUOVA AREA
012400930623     C                   EXSR      NEWARE
012500930317     C*
012600930622     C* CICLO FINO A ROTTURA DI AREA
012700930317     C     FINL00        DOWEQ     '0'                                          --- 3 -->
012800931122     C     WFPDIV        ANDEQ     DEPDIV
012900931122     C     WFPARE        ANDEQ     DEPARE
013000930623     C*
013100930623     C* OPERAZIONI PER NUOVA FILIALE DI PARTENZA
013200930623     C                   EXSR      NEWFLP
013300930317     C*
013400930622     C* CICLO FINO A ROTTURA DI FILIALE DI PARTENZA
013500930317     C     FINL00        DOWEQ     '0'                                          --- 4 -->
013600931122     C     WFPDIV        ANDEQ     DEPDIV
013700931122     C     WFPARE        ANDEQ     DEPARE
013800931122     C     WFPFLP        ANDEQ     DEPFLP
013900930318     C*
014000930622     C* MEMORIZZA LE SPEDIZIONI
014100930622     C                   EXSR      MEMSPE
014200000000     C*
014300930128     C* LEGGE IL RECORD SUCCESSIVO
014400930208     C                   EXSR      LETFIL
014500930128     C*
014600931122     C                   END                                                    <-- 4 ---
014700930622     C*
014800930622     C* STAMPA IL TOTALE FILIALE DI PARTENZA
014900930622     C                   EXSR      STAFLP
015000930622     C*
015100930317     C                   END                                                    <-- 3 ---
015200930622     C*
015300930622     C* STAMPA IL TOTALE AREA
015400930622     C                   EXSR      STAARE
015500930622     C*
015600930317     C                   END                                                    <-- 2 ---
015700930622     C*
015800930622     C* STAMPA IL TOTALE DIVISIONE
015900930622     C                   EXSR      STADIV
016000930622     C*
016100930317     C                   END                                                    <-- 1 ---
016200930622     C*
016300930622     C* STAMPA IL TOTALE GENERALE
016400930622     C                   EXSR      STAGEN
016500930622     C*
016600930622     C* STAMPA RIGA FINE STAMPA
016700930622     C                   EXCEPT    STAFIN
016800000000     C*
016900000000     C                   SETON                                        LR
017000931122     C*------------------------------------------------------------------------*
017100941031     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
017200931122     C*------------------------------------------------------------------------*
017300930208     C     SETFIL        BEGSR
017400930208     C*
017500930622     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
017600931122     C                   Z-ADD     PARANN        KWPANN
017700931122     C                   MOVEL     *BLANKS       KWPDIV
017800931122     C                   Z-ADD     *ZEROS        KWPARE
017900931122     C                   Z-ADD     *ZEROS        KWPFLP
018000931122     C                   Z-ADD     *ZEROS        KWPMES
018100941205     C                   MOVEL     *BLANKS       KWPTSP
018200941205     C                   Z-ADD     *ZEROS        KWPRAN
018300950627     C                   MOVEL     *BLANKS       KWPIOE
018400931122     C     KEYWFP        SETLL     WFDLP01L                           99        *FINE-ARCHIV
018500930623     C*
018600930623     C     *IN99         IFEQ      '1'
018700930623     C                   MOVEL     '1'           FINL00
018800930623     C                   ELSE
018900930623     C                   EXSR      LETFIL
019000930623     C                   END
019100930317     C*
019200930208     C                   ENDSR
019300931122     C*------------------------------------------------------------------------*
019400941031     C* LETFIL - LETTURA PROSSIMO RECORD
019500931122     C*------------------------------------------------------------------------*
019600930208     C     LETFIL        BEGSR
019700930623     C*
019800930623     C* LEGGO FINO A:
019900930628     C*     - FINE FILE O SUPERATO LIMITE FINALE
020000930623     C*     - TROVATO RECORD VALIDO
020100930623     C                   MOVEL     'N'           WRECOK
020200930628     C     FINL00        DOWEQ     '0'                                          --- 1 -->
020300930623     C     WRECOK        ANDEQ     'N'
020400930607     C*
020500930622     C* LETTURE SUCCESSIVE ALLA PRIMA
020600931122     C                   READ      WFDLP01L                               99
020700930624     C*
020800930628     C     *IN99         IFEQ      '1'                                          *FINA FILE
020900931122     C     WFPANN        ORNE      PARANN                                       *SUPERATO ANNO
021000930624     C                   MOVEL     '1'           FINL00
021100930628     C                   ELSE
021200930628     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
021300930628     C                   END
021400930623     C*
021500930623     C                   END                                                    <-- 1 ---
021600930208     C*
021700930208     C                   ENDSR
021800931122     C*------------------------------------------------------------------------*
021900941031     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
022000931122     C*------------------------------------------------------------------------*
022100930622     C     CHKREC        BEGSR
022200930622     C*
022300930622     C                   MOVEL     'S'           WRECOK            1
022400930622     C*
022500930622     C* SE NON RIENTRA NEI MESI RICHIESTI ESCLUDO IL RECORD
022600931122     C     WFPMES        IFGE      PARMMI
022700931122     C     WFPMES        ANDLE     PARMMF
022800930622     C                   ELSE
022900930622     C                   MOVEL     'N'           WRECOK
023000930622     C                   END
023100930623     C*
023200930623     C* SE DIVISIONE DEL RECORD <> DIVISIONE RICHIESTA, ESCLUDO
023300930623     C     PARDIV        IFNE      *BLANKS
023400931122     C     WFPDIV        IFNE      PARDIV
023500930623     C                   MOVEL     'N'           WRECOK
023600930623     C                   END
023700930623     C                   END
023800930623     C*
023900930623     C* SE AREA DEL RECORD <> AREA RICHIESTA, ESCLUDO
024000930623     C     PARARE        IFNE      *ZEROS
024100931122     C     WFPARE        IFNE      PARARE
024200930623     C                   MOVEL     'N'           WRECOK
024300930623     C                   END
024400930623     C                   END
024500930623     C*
024600930623     C* SE FIL.PART. DEL RECORD <> FIL.PART RICHIESTA, ESCLUDO
024700930623     C     PARFLP        IFNE      *ZEROS
024800931122     C     WFPFLP        IFNE      PARFLP
024900930623     C                   MOVEL     'N'           WRECOK
025000930623     C                   END
025100930623     C                   END
025200930623     C*
025300930623     C* SE TER.PART. DEL RECORD <> TER.PART RICHIESTO, ESCLUDO
025400930623     C     PARTFP        IFNE      *ZEROS
025500931122     C     WFPTFP        IFNE      PARTFP
025600930623     C                   MOVEL     'N'           WRECOK
025700930623     C                   END
025800930623     C                   END
025900950627     C*
026000950627     C* CONTROLLA IL TIPO DI DATI IN STATISTICA RICHIESTO
026100950627IF  1C     PARIOE        IFNE      *BLANKS                                      *SELEZIONE
026200950627IF  2C     PARIOE        IFNE      WFPIOE
026300950627     C                   MOVEL     'N'           WRECOK
026400950627E   2C                   ENDIF
026500950627X   1C                   ELSE                                                   *NESSUNA SELEZIONE
026600950627IF  2C     WFPIOE        IFEQ      'I'                                          *'I' + 'M' = TUTTO
026700950627     C     WFPIOE        OREQ      'M'
026800950627X   2C                   ELSE
026900950627     C                   MOVEL     'N'           WRECOK
027000950627E   2C                   ENDIF
027100950627E   1C                   ENDIF
027200930622     C*
027300930622     C                   ENDSR
027400931122     C*------------------------------------------------------------------------*
027500941031     C* MEMSPE - MEMORIZZA LE SPEDIZIONI
027600931122     C*------------------------------------------------------------------------*
027700941205     C     MEMSPE        BEGSR
027800941205     C*
027900941205     C* CERCO IN NUMERO DI OCUR DEL TIPO SERVIZIO LETTO
028000941205     C                   Z-ADD     1             I
028100941205     C                   SETOFF                                       28
028200941205     C     WFPTSP        LOOKUP    TSP(I)                                 28
028300941205IF  1C     *IN28         IFEQ      *ON                                          *SOLO SE TROVATO
028400941205     C*
028500941205     C* NUMERO RANGE ORE DELIVERY
028600941205     C                   Z-ADD     WFPRAN        R
028700941205     C*--------------------
028800941205     C* GENERALE
028900941205     C*--------------------
029000941205     C     OTSP(I)       OCCUR     OGEN
029100941205     C                   ADD       WFPNSP        GEN(R)
029200941205     C*--------------------
029300941205     C* DIVISIONE
029400941205     C*--------------------
029500941205     C     OTSP(I)       OCCUR     ODPN
029600941205     C                   ADD       WFPNSP        DPN(R)
029700941205     C*--------------------
029800941205     C* AREA
029900941205     C*--------------------
030000941205     C     OTSP(I)       OCCUR     OAPN
030100941205     C                   ADD       WFPNSP        APN(R)
030200941205     C*--------------------
030300941205     C* FILIALE
030400941205     C*--------------------
030500941205     C     OTSP(I)       OCCUR     OFPN
030600941205     C                   ADD       WFPNSP        FPN(R)
030700941205     C*
030800941205E   1C                   ENDIF
030900941205     C*
031000941205     C                   ENDSR
031100931122     C*------------------------------------------------------------------------*
031200941031     C* NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
031300931122     C*------------------------------------------------------------------------*
031400930623     C     NEWDIV        BEGSR
031500930623     C*
031600930623     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
031700931122     C                   MOVEL     WFPDIV        DEPDIV
031800941205     C*
031900941205     C* AZZERA I TOTALI DELLA DIVISIONE
032000941205DO  1C     1             DO        WULTSP        R
032100941205     C     R             OCCUR     ODPN
032200941205     C                   CLEAR                   DPN
032300941205E   1C                   ENDDO
032400930623     C*
032500930623     C* DECODIFICA LA DIVISIONE
032600930623     C                   EXSR      DECDIV
032700930623     C*
032800930628     C* DESCRIZIONE INTESTAZIONE
032900930623     C                   MOVEL     TES(14)       STINT1           22
033000930623     C*
033100930628     C* DESCRIZIONE DIVISIONE
033200930623     C                   MOVEL     DESDIV        STINT2           20
033300930623     C*
033400930623     C                   ENDSR
033500931122     C*------------------------------------------------------------------------*
033600941031     C* NEWARE - OPERAZIONI PER NUOVA AREA
033700931122     C*------------------------------------------------------------------------*
033800930623     C     NEWARE        BEGSR
033900930623     C*
034000930623     C* MEMORIZZA LA NUOVA AREA IN UN DEPOSITO
034100931122     C                   Z-ADD     WFPARE        DEPARE
034200930623     C*
034300930623     C* AZZERA I TOTALI DELL'AREA
034400941205DO  1C     1             DO        WULTSP        R
034500941205     C     R             OCCUR     OAPN
034600941205     C                   CLEAR                   APN
034700941205E   1C                   ENDDO
034800930623     C*
034900930623     C                   ENDSR
035000931122     C*------------------------------------------------------------------------*
035100941031     C* NEWFLP - OPERAZIONI PER NUOVA FILIALE DI PARTENZA
035200931122     C*------------------------------------------------------------------------*
035300930623     C     NEWFLP        BEGSR
035400930623     C*
035500930623     C* MEMORIZZA LA NUOVA FIL.PART. IN UN DEPOSITO
035600931122     C                   Z-ADD     WFPFLP        DEPFLP
035700930623     C*
035800931126     C* AZZERA I TOTALI DELLA FILIALE DI PARTENZA
035900941205DO  1C     1             DO        WULTSP        R
036000941205     C     R             OCCUR     OFPN
036100941205     C                   CLEAR                   FPN
036200941205E   1C                   ENDDO
036300930623     C*
036400931126     C* STAMPA TESTATA
036500931126     C     FLGTES        IFEQ      '1'                                          *SALTO PAGINA
036600931126     C                   EXCEPT    TESTA
036700941209     C                   SETOFF                                       OF
036800931126     C                   MOVEL     '0'           FLGTES                         *REIMPOSTO FLAG
036900931126E   1C                   ENDIF
037000930623     C*
037100930623     C                   ENDSR
037200931122     C*------------------------------------------------------------------------*
037300941031     C* STAFLP - STAMPA TOTALI FILIALE PARTENZA
037400931122     C*------------------------------------------------------------------------*
037500930622     C     STAFLP        BEGSR
037600930622     C*
037700950629     C* DESCRIZIONE TOTALE (DIVERSA FRA ITALIA/IMPORT  E  EXPORT)
037800950629IF  1C     PARIOE        IFEQ      'X'
037900950629     C                   MOVEL(P)  TES(26)       STDES1           22
038000950629X   1C                   ELSE
038100931123     C                   MOVEL     TES(10)       STDES1           22
038200950629E   2C                   ENDIF
038300950629     C*
038400941205     C* DECODIFICA LA FILIALE
038500941205     C                   MOVEL     DEPFLP        WFIL              3 0
038600941205     C                   EXSR      DECFIL
038700941209     C                   MOVE      DESFIL        STDES2           20
038800941209     C*
038900941209     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
039000941209     C     *INOF         IFEQ      '1'
039100941209     C                   EXCEPT    TESTA
039200941209     C                   SETOFF                                       OF
039300941209     C                   END
039400941205     C*
039500941205     C* AZZERO CAMPI DI LAVORO
039600941205     C                   Z-ADD     *ZEROS        WR                1 0          *NUMERO SERVIZIO
039700941205     C                   Z-ADD     *ZEROS        WTUTS             8 0          *SPED.DEL SERVIZIO
039800941205     C                   Z-ADD     *ZEROS        WTSP                           *SCHIERA ORE SERVIZI
039900941205     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
040000941205     C*--------------------
040100941205     C* RIGA SERVIZIO
040200941205     C*--------------------
040300941205     C* PER CISCUN TIPO SERVIZIO ESISTENTE
040400941205     C                   Z-ADD     *ZEROS        PASCIC            1 0          *N°PASSAGGIO CICLO
040500941205DO  1C     1             DO        WULTSP        R
040600941205     C     R             OCCUR     OFPN
040700941205     C*
040800941205     C* SOLO SE MEMORIZZATO QC.
040900941205     C                   XFOOT     FPN           WSUM             15 0          *COMODO SOMMA SHIERA
041000941205IF  2C     WSUM          IFNE      *ZEROS
041100941205     C                   ADD       1             PASCIC
041200941205     C*
041300941205     C* DECODIFICHE IN STAMPA
041400941205     C     PASCIC        IFEQ      1
041500941205     C                   MOVEL(P)  STDES1        STDES            22            *TIPO DI TOTALE
041600941205     C                   ELSE
041700941205     C     PASCIC        IFEQ      2
041800941205     C                   MOVEL(P)  STDES2        STDES                          *TOTALE FIL/CAP/TERM
041900941205     C                   ELSE
042000941205     C     PASCIC        IFGT      2
042100941205     C                   MOVEL(P)  *BLANKS       STDES
042200941205     C                   ENDIF
042300941205     C                   ENDIF
042400941205     C                   ENDIF
042500941205     C*
042600941205     C                   Z-ADD     R             WR                             *CODICE SERVIZIO
042700941205     C                   XFOOT     FPN           WTUTS                          *SPEDIZ. DEL SEVIZIO
042800941205     C                   Z-ADD     FPN           WTSP                           *ORE DELIVERY SERVIZ
042900941205     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
043000941205     C                   EXSR      STASPE
043100941205E   2C                   ENDIF
043200941205     C*
043300941205E   1C                   ENDDO
043400941205     C*--------------------
043500941205     C* RIGA TOTALE
043600941205     C*--------------------
043700941205     C* SOMMA DEI TIPI SERVIZI DIVISI PER ORE DELIVERY
043800941205     C                   CLEAR                   WTSP
043900941205DO  1C     1             DO        WULTSP        R
044000941205     C     R             OCCUR     OFPN
044100941205     C                   ADD       FPN           WTSP                           *ORE DELIVERY SERVIZ
044200941205E   1C                   ENDDO
044300941205     C*
044400941205     C* SOLO SE MEMORIZZATO QC.
044500941205     C                   XFOOT     WTSP          WSUM                           *COMODO SOMMA SHIERA
044600941205IF  1C     WSUM          IFNE      *ZEROS
044700941205     C*
044800941205     C* DECODIFICHE IN STAMPA
044900941205     C     PASCIC        IFLT      2                                            *COMUNQUE UN SERVIZI
045000941205     C                   MOVEL(P)  STDES2        STDES                          *LO HA STAMPATO (>1)
045100941205     C                   ELSE
045200941205     C                   MOVEL(P)  *BLANKS       STDES
045300941205     C                   ENDIF
045400941205     C*
045500941205     C                   Z-ADD     0             WR                             *SERVIZIO "TOTALE"
045600941205     C                   XFOOT     WTSP          WTUTS                          *SPEDIZIONI TOTALI
045700941205     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
045800941205     C                   EXSR      STASPE
045900941205E   1C                   ENDIF
046000930622     C*
046100930622     C                   ENDSR
046200931122     C*------------------------------------------------------------------------*
046300941031     C* STAARE - STAMPA TOTALI AREA
046400931122     C*------------------------------------------------------------------------*
046500930622     C     STAARE        BEGSR
046600930622     C*
046700930622     C* DESCRIZIONE TOTALE
046800930622     C                   MOVEL     TES(11)       STDES1
046900930622     C*
047000930622     C* DECODIFICA L'AREA
047100930622     C                   EXSR      DECARE
047200941206     C                   MOVE      DESARE        STDES2
047300931122     C*
047400931122     C* STAMPO LA TESTATA PER AVERE I TOTALI AREA A PAGINA NUOVA
047500940117     C*--------------------MOVELTES,14    STINT1
047600940117     C*--------------------MOVELDESDIV    STINT2
047700940117     C*--------------------EXCPTTESTA
047800941209     C*--------------------SETOF                     OF
047900930623     C*
048000940117     C* IMPOSTO A NO LA RIBATTUTA
048100940117     C                   MOVEL     '0'           *IN01
048200941209     C*
048300941209     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
048400941209     C     *INOF         IFEQ      '1'
048500941209     C                   EXCEPT    TESTA
048600941209     C                   SETOFF                                       OF
048700941209     C                   END
048800941205     C*
048900941205     C* AZZERO CAMPI DI LAVORO
049000941205     C                   Z-ADD     *ZEROS        WR                1 0          *NUMERO SERVIZIO
049100941205     C                   Z-ADD     *ZEROS        WTUTS                          *SPED.DEL SERVIZIO
049200941205     C                   Z-ADD     *ZEROS        WTSP                           *SCHIERA ORE SERVIZI
049300941205     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
049400941205     C*--------------------
049500941205     C* RIGA SERVIZIO
049600941205     C*--------------------
049700941205     C* PER CISCUN TIPO SERVIZIO ESISTENTE
049800941205     C                   Z-ADD     *ZEROS        PASCIC            1 0          *N°PASSAGGIO CICLO
049900941205DO  1C     1             DO        WULTSP        R
050000941205     C     R             OCCUR     OAPN
050100941205     C*
050200941205     C* SOLO SE MEMORIZZATO QC.
050300941205     C                   XFOOT     APN           WSUM             15 0          *COMODO SOMMA SHIERA
050400941205IF  2C     WSUM          IFNE      *ZEROS
050500941205     C                   ADD       1             PASCIC
050600941205     C*
050700941205     C* DECODIFICHE IN STAMPA
050800941205     C     PASCIC        IFEQ      1
050900941205     C                   MOVEL(P)  STDES1        STDES            22            *TIPO DI TOTALE
051000941205     C                   ELSE
051100941205     C     PASCIC        IFEQ      2
051200941205     C                   MOVEL(P)  STDES2        STDES                          *TOTALE FIL/CAP/TERM
051300941205     C                   ELSE
051400941205     C     PASCIC        IFGT      2
051500941205     C                   MOVEL(P)  *BLANKS       STDES
051600941205     C                   ENDIF
051700941205     C                   ENDIF
051800941205     C                   ENDIF
051900941205     C*
052000941205     C                   Z-ADD     R             WR                             *CODICE SERVIZIO
052100941205     C                   XFOOT     APN           WTUTS                          *SPEDIZ. DEL SEVIZIO
052200941205     C                   Z-ADD     APN           WTSP                           *ORE DELIVERY SERVIZ
052300941205     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
052400941205     C                   EXSR      STASPE
052500941205E   2C                   ENDIF
052600941205     C*
052700941205E   1C                   ENDDO
052800941205     C*--------------------
052900941205     C* RIGA TOTALE
053000941205     C*--------------------
053100941205     C* SOMMA DEI TIPI SERVIZI DIVISI PER ORE DELIVERY
053200941205     C                   CLEAR                   WTSP
053300941205DO  1C     1             DO        WULTSP        R
053400941205     C     R             OCCUR     OAPN
053500941205     C                   ADD       APN           WTSP                           *ORE DELIVERY SERVIZ
053600941205E   1C                   ENDDO
053700941205     C*
053800941205     C* SOLO SE MEMORIZZATO QC.
053900941205     C                   XFOOT     WTSP          WSUM                           *COMODO SOMMA SHIERA
054000941205IF  1C     WSUM          IFNE      *ZEROS
054100941205     C*
054200941205     C* DECODIFICHE IN STAMPA
054300941205     C     PASCIC        IFLT      2                                            *COMUNQUE UN SERVIZI
054400941205     C                   MOVEL(P)  STDES2        STDES                          *LO HA STAMPATO (>1)
054500941205     C                   ELSE
054600941205     C                   MOVEL(P)  *BLANKS       STDES
054700941205     C                   ENDIF
054800941205     C*
054900941205     C                   Z-ADD     0             WR                             *SERVIZIO "TOTALE"
055000941205     C                   XFOOT     WTSP          WTUTS                          *SPEDIZIONI TOTALI
055100941205     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
055200941205     C                   EXSR      STASPE
055300941205E   1C                   ENDIF
055400941205     C*
055500941205     C                   MOVEL     '1'           FLGTES                         *SALTO PAGINA SUCC.
055600930622     C*
055700930622     C                   ENDSR
055800931122     C*------------------------------------------------------------------------*
055900941031     C* STADIV - STAMPA TOTALI DIVISIONE
056000931122     C*------------------------------------------------------------------------*
056100930622     C     STADIV        BEGSR
056200930622     C*
056300930622     C* DESCRIZIONE TOTALE
056400930622     C                   MOVEL     TES(12)       STDES1
056500930622     C*
056600930622     C* DECODIFICA LA DIVISIONE
056700930623     C                   EXSR      DECDIV
056800941206     C                   MOVE      DESDIV        STDES2
056900931122     C*
057000931122     C* STAMPO LA TESTATA PER AVERE I TOTALI DIVISIONE A PAGINA NUOVA
057100931126     C                   MOVEL     TES(14)       STINT1
057200931126     C                   MOVEL     DESDIV        STINT2
057300931122     C                   EXCEPT    TESTA
057400941209     C                   SETOFF                                       OF
057500930623     C*
057600941205     C* IMPOSTO A NO LA RIBATTUTA
057700931123     C                   MOVEL     '0'           *IN01
057800941209     C*
057900941209     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
058000941209     C     *INOF         IFEQ      '1'
058100941209     C                   EXCEPT    TESTA
058200941209     C                   SETOFF                                       OF
058300941209     C                   END
058400941205     C*
058500941205     C* AZZERO CAMPI DI LAVORO
058600941205     C                   Z-ADD     *ZEROS        WR                1 0          *NUMERO SERVIZIO
058700941205     C                   Z-ADD     *ZEROS        WTUTS                          *SPED.DEL SERVIZIO
058800941205     C                   Z-ADD     *ZEROS        WTSP                           *SCHIERA ORE SERVIZI
058900941205     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
059000941205     C*--------------------
059100941205     C* RIGA SERVIZIO
059200941205     C*--------------------
059300941205     C* PER CISCUN TIPO SERVIZIO ESISTENTE
059400941205     C                   Z-ADD     *ZEROS        PASCIC            1 0          *N°PASSAGGIO CICLO
059500941205DO  1C     1             DO        WULTSP        R
059600941205     C     R             OCCUR     ODPN
059700941205     C*
059800941205     C* SOLO SE MEMORIZZATO QC.
059900941205     C                   XFOOT     DPN           WSUM             15 0          *COMODO SOMMA SHIERA
060000941205IF  2C     WSUM          IFNE      *ZEROS
060100941205     C                   ADD       1             PASCIC
060200941205     C*
060300941205     C* DECODIFICHE IN STAMPA
060400941205     C     PASCIC        IFEQ      1
060500941205     C                   MOVEL(P)  STDES1        STDES            22            *TIPO DI TOTALE
060600941205     C                   ELSE
060700941205     C     PASCIC        IFEQ      2
060800941205     C                   MOVEL(P)  STDES2        STDES                          *TOTALE FIL/CAP/TERM
060900941205     C                   ELSE
061000941205     C     PASCIC        IFGT      2
061100941205     C                   MOVEL(P)  *BLANKS       STDES
061200941205     C                   ENDIF
061300941205     C                   ENDIF
061400941205     C                   ENDIF
061500941205     C*
061600941205     C                   Z-ADD     R             WR                             *CODICE SERVIZIO
061700941205     C                   XFOOT     DPN           WTUTS                          *SPEDIZ. DEL SEVIZIO
061800941205     C                   Z-ADD     DPN           WTSP                           *ORE DELIVERY SERVIZ
061900941205     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
062000941205     C                   EXSR      STASPE
062100941205E   2C                   ENDIF
062200941205     C*
062300941205E   1C                   ENDDO
062400941205     C*--------------------
062500941205     C* RIGA TOTALE
062600941205     C*--------------------
062700941205     C* SOMMA DEI TIPI SERVIZI DIVISI PER ORE DELIVERY
062800941205     C                   CLEAR                   WTSP
062900941205DO  1C     1             DO        WULTSP        R
063000941205     C     R             OCCUR     ODPN
063100941205     C                   ADD       DPN           WTSP                           *ORE DELIVERY SERVIZ
063200941205E   1C                   ENDDO
063300941205     C*
063400941205     C* SOLO SE MEMORIZZATO QC.
063500941205     C                   XFOOT     WTSP          WSUM                           *COMODO SOMMA SHIERA
063600941205IF  1C     WSUM          IFNE      *ZEROS
063700941205     C*
063800941205     C* DECODIFICHE IN STAMPA
063900941205     C     PASCIC        IFLT      2                                            *COMUNQUE UN SERVIZI
064000941205     C                   MOVEL(P)  STDES2        STDES                          *LO HA STAMPATO (>1)
064100941205     C                   ELSE
064200941205     C                   MOVEL(P)  *BLANKS       STDES
064300941205     C                   ENDIF
064400941205     C*
064500941205     C                   Z-ADD     0             WR                             *SERVIZIO "TOTALE"
064600941205     C                   XFOOT     WTSP          WTUTS                          *SPEDIZIONI TOTALI
064700941205     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
064800941205     C                   EXSR      STASPE
064900941205     C* SALTO PAG SUCESSIVO
065000941205     C                   MOVEL     '1'           FLGTES                         *SALTO PAGINA
065100941205E   1C                   ENDIF
065200930622     C*
065300930622     C                   ENDSR
065400931122     C*------------------------------------------------------------------------*
065500941031     C* STAGEN - STAMPA TOTALI GENERALI
065600931122     C*------------------------------------------------------------------------*
065700930622     C     STAGEN        BEGSR
065800930622     C*
065900930622     C* DESCRIZIONE TOTALE
066000930622     C                   MOVEL     TES(13)       STDES1
066100930622     C                   MOVEL     *BLANKS       STDES2
066200931122     C*
066300931122     C* STAMPO LA TESTATA PER AVERE I TOTALI GENERALI A PAGINA NUOVA
066400931123     C                   MOVEL     *BLANKS       STINT1
066500931123     C                   MOVEL     *BLANKS       STINT2
066600931122     C                   EXCEPT    TESTA
066700941209     C                   SETOFF                                       OF
066800930623     C*
066900931123     C* IMPOSTO A NO LA RIBATTUTA
067000931123     C                   MOVEL     '0'           *IN01
067100941209     C*
067200941209     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
067300941209     C     *INOF         IFEQ      '1'
067400941209     C                   EXCEPT    TESTA
067500941209     C                   SETOFF                                       OF
067600941209     C                   END
067700941205     C*
067800941205     C* AZZERO CAMPI DI LAVORO
067900941205     C                   Z-ADD     *ZEROS        WR                1 0          *NUMERO SERVIZIO
068000941205     C                   Z-ADD     *ZEROS        WTUTS                          *SPED.DEL SERVIZIO
068100941205     C                   Z-ADD     *ZEROS        WTSP                           *SCHIERA ORE SERVIZI
068200941205     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
068300941205     C*--------------------
068400941205     C* RIGA SERVIZIO
068500941205     C*--------------------
068600941205     C* PER CISCUN TIPO SERVIZIO ESISTENTE
068700941205     C                   Z-ADD     *ZEROS        PASCIC            1 0          *N°PASSAGGIO CICLO
068800941205DO  1C     1             DO        WULTSP        R
068900941205     C     R             OCCUR     OGEN
069000941205     C*
069100941205     C* SOLO SE MEMORIZZATO QC.
069200941205     C                   XFOOT     GEN           WSUM             15 0          *COMODO SOMMA SHIERA
069300941205IF  2C     WSUM          IFNE      *ZEROS
069400941205     C                   ADD       1             PASCIC
069500941205     C*
069600941205     C* DECODIFICHE IN STAMPA
069700941205     C     PASCIC        IFEQ      1
069800941205     C                   MOVEL(P)  STDES1        STDES            22            *TIPO DI TOTALE
069900941205     C                   ELSE
070000941205     C     PASCIC        IFEQ      2
070100941205     C                   MOVEL(P)  STDES2        STDES                          *TOTALE FIL/CAP/TERM
070200941205     C                   ELSE
070300941205     C     PASCIC        IFGT      2
070400941205     C                   MOVEL(P)  *BLANKS       STDES
070500941205     C                   ENDIF
070600941205     C                   ENDIF
070700941205     C                   ENDIF
070800941205     C*
070900941205     C                   Z-ADD     R             WR                             *CODICE SERVIZIO
071000941205     C                   XFOOT     GEN           WTUTS                          *SPEDIZ. DEL SEVIZIO
071100941205     C                   Z-ADD     GEN           WTSP                           *ORE DELIVERY SERVIZ
071200941205     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
071300941205     C                   EXSR      STASPE
071400941205E   2C                   ENDIF
071500941205     C*
071600941205E   1C                   ENDDO
071700941205     C*--------------------
071800941205     C* RIGA TOTALE
071900941205     C*--------------------
072000941205     C* SOMMA DEI TIPI SERVIZI DIVISI PER ORE DELIVERY
072100941205     C                   CLEAR                   WTSP
072200941205DO  1C     1             DO        WULTSP        R
072300941205     C     R             OCCUR     OGEN
072400941205     C                   ADD       GEN           WTSP                           *ORE DELIVERY SERVIZ
072500941205E   1C                   ENDDO
072600941205     C*
072700941205     C* SOLO SE MEMORIZZATO QC.
072800941205     C                   XFOOT     WTSP          WSUM                           *COMODO SOMMA SHIERA
072900941205IF  1C     WSUM          IFNE      *ZEROS
073000941205     C*
073100941205     C* DECODIFICHE IN STAMPA
073200941205     C     PASCIC        IFLT      2                                            *COMUNQUE UN SERVIZI
073300941205     C                   MOVEL(P)  STDES2        STDES                          *LO HA STAMPATO (>1)
073400941205     C                   ELSE
073500941205     C                   MOVEL(P)  *BLANKS       STDES
073600941205     C                   ENDIF
073700941205     C*
073800941205     C                   Z-ADD     0             WR                             *SERVIZIO "TOTALE"
073900941205     C                   XFOOT     WTSP          WTUTS                          *SPEDIZIONI TOTALI
074000941205     C                   Z-ADD     *ZEROS        WPER                           *SCHIERA PERCENTUALI
074100941205     C                   EXSR      STASPE
074200930622     C*
074300941205E   1C                   ENDIF
074400941205     C*
074500930622     C                   ENDSR
074600941205     C*------------------------------------------------------------------------*
074700941205     C* STASPE - STAMPA RIGHE SPEDIZIONI E PRECENTUALI
074800941205     C*------------------------------------------------------------------------*
074900941205     C     STASPE        BEGSR
075000941205     C*
075100941205     C                   Z-ADD     *ZEROS        WRKPER           17 2
075200941205     C*
075300941205     C* PER CISCUNA ORA DI DELIVERY, CALCOLA LA PERCENTUALE DI AFFIDABILITA'
075400941205     C* (N° SPED. SERVIZIO DELL'ORA X 100) : N° SPED. SERVIZIO DI TUTTE LE ORE
075500941205DO  1C     1             DO        6             I
075600941205     C     WTSP(I)       MULT      100           WRKPER
075700941205IF  2C     WRKPER        IFNE      *ZEROS
075800941205     C     WRKPER        DIV       WTUTS         WPER(I)
075900941205X   2C                   ELSE
076000941205     C                   Z-ADD     *ZEROS        WPER(I)
076100941205E   2C                   END
076200941205E   1C                   ENDDO
076300941205     C***
076400941205     C* DECODIFICHE IN STAMPA
076500941205     C***
076600941205     C                   MOVEL     *BLANKS       DESTSP           15            *DESCRIZ. SERVIZIO
076700941205IF  1C     WR            IFNE      0
076800941205     C                   MOVEL     DTSP(WR)      DESTSP
076900941205X   1C                   ELSE
077000941205     C                   MOVEL     'TOTALI'      DESTSP
077100941205E   1C                   ENDIF
077200941205     C*
077300941205     C* STAMPO RIGA SPEDIZIONI E RIGA PERCENTUALE
077400941205     C                   EXCEPT    RIGA
077500941205     C*
077600941205     C* STAMPO RIGA DI SEPARAZIONE SE:
077700941205     C*             SE RICHIESTA (*IN01 OFF)
077800941205     C*             SE ARRIVATA A STAMPARE RIGA "TOTALE"
077900941205     C     *IN01         IFEQ      '0'
078000941205     C     WR            ANDEQ     0
078100941205     C                   EXCEPT    RIGSEP
078200941205     C                   END
078300941205     C*
078400941205     C                   ENDSR
078500931122     C*------------------------------------------------------------------------*
078600941031     C* DECDIV - DECODIFICA DIVISIONE
078700931122     C*------------------------------------------------------------------------*
078800930622     C     DECDIV        BEGSR
078900930514     C*
079000020920     C                   MOVEL     1             KTBUTE
079100930622     C                   MOVE      '17'          KTBCOD
079200930514     C                   MOVE      *BLANKS       KTBKEY
079300930622     C                   MOVEL     DEPDIV        KTBKEY
079400930514     C     KEYTAB        CHAIN     TABEL                              99
079500930514     C     *IN99         IFEQ      '1'
079600930514     C     TBLFLG        OREQ      '*'
079700930622     C                   MOVEL     *ALL'*'       DESDIV           20
079800930514     C                   ELSE
079900930622     C                   MOVEL     TBLUNI        DESDIV
080000930514     C                   END
080100930514     C*
080200930514     C                   ENDSR
080300931122     C*------------------------------------------------------------------------*
080400941031     C* DECARE - DECODIFICA AREA
080500931122     C*------------------------------------------------------------------------*
080600930622     C     DECARE        BEGSR
080700930622     C*
080800020920     C                   MOVEL     1             KTBUTE
080900930622     C                   MOVE      '05'          KTBCOD
081000930622     C                   MOVE      *BLANKS       KTBKEY
081100930622     C                   MOVEL     DEPARE        KTBKEY
081200930622     C     KEYTAB        CHAIN     TABEL                              99
081300930622     C     *IN99         IFEQ      '1'
081400930622     C     TBLFLG        OREQ      '*'
081500930622     C                   MOVEL     *ALL'*'       DESARE           20
081600930622     C                   ELSE
081700930622     C                   MOVEL     TBLUNI        DESARE
081800930622     C                   END
081900930622     C*
082000930622     C                   ENDSR
082100931122     C*------------------------------------------------------------------------*
082200941031     C* DECFIL - DECODIFICA FILIALE/TERMINAL
082300931122     C*------------------------------------------------------------------------*
082400930622     C     DECFIL        BEGSR
082500930622     C*
082600930622     C                   Z-ADD     1             I
082700930622     C     WFIL          LOOKUP    FIL(I)                                 99
082800930622     C     *IN99         IFEQ      '0'
082900930622     C                   MOVEL     *ALL'*'       DESFIL           20
083000930622     C                   ELSE
083100930622     C                   MOVEL     DFIL(I)       DESFIL
083200930622     C                   END
083300930622     C*
083400930622     C                   ENDSR
083500020920     C*--------------------------------------------------------------------------------------------*
083600020920     C* REPERISCE I DATI UTENTE
083700020920     C*--------------------------------------------------------------------------------------------*
083800020920     C     REPDATIUTE    BEGSR
083900020920     C*
084000020920     C* INIZIALIZZA VARIABILI DI WRK
084100020920     C                   CLEAR                   TIBS34DS
084200020920     C                   CLEAR                   AZUTEDS
084300020920     C                   CLEAR                   DDATIUTE
084400020920     C*
084500020920     C     *DTAARA       DEFINE    §azute        azuteds
084600020920     C     *DTAARA       DEFINE    §datiute      ddatiute
084700020920     C                   IN(E)     *DTAARA
084800020920if  1C                   IF        %Error
084900020920     c                   EVAL      I34Tla = 'L'
085000020920     C                   CALL      'TIBS34R'
085100020920     C                   PARM                    Tibs34Ds
085200020920     C                   IN        *DTAARA
085300020920e   1C                   ENDIF
085400020920     C*
085500020920     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
085600020920     C                   MOVEL(P)  RSUT          DSFIRS           20
085700020920     C*
085800020920     C                   ENDSR
085900931122     C*------------------------------------------------------------------------*
086000941031     C* *INZSR - ROUTINE INIZIALE
086100931122     C*------------------------------------------------------------------------*
086200941031     C     *INZSR        BEGSR
086300941031     C*--------------------
086400941031     C* RICEVIMENTO PARAMETRI
086500941031     C*--------------------
086600930514     C     *ENTRY        PLIST
086700930514     C                   PARM                    KPJBA
086800950626     C                   MOVEL     KPJBU         PARAM
086900941031     C*--------------------
087000941031     C* CAMPI RIFERITI AL DATA BASE
087100941031     C*--------------------
087200930208     C     *LIKE         DEFINE    TBLKUT        KTBUTE
087300930208     C     *LIKE         DEFINE    TBLCOD        KTBCOD
087400930208     C     *LIKE         DEFINE    TBLKEY        KTBKEY
087500930526     C     *LIKE         DEFINE    ORGFIL        KORFIL
087600931122     C     *LIKE         DEFINE    WFPANN        KWPANN
087700931122     C     *LIKE         DEFINE    WFPDIV        KWPDIV
087800931122     C     *LIKE         DEFINE    WFPARE        KWPARE
087900931122     C     *LIKE         DEFINE    WFPFLP        KWPFLP
088000931122     C     *LIKE         DEFINE    WFPMES        KWPMES
088100941205     C     *LIKE         DEFINE    WFPTSP        KWPTSP
088200941205     C     *LIKE         DEFINE    WFPRAN        KWPRAN
088300950627     C     *LIKE         DEFINE    WFPIOE        KWPIOE
088400931122     C     *LIKE         DEFINE    WFPDIV        DEPDIV
088500931122     C     *LIKE         DEFINE    WFPARE        DEPARE
088600931122     C     *LIKE         DEFINE    WFPFLP        DEPFLP
088700950627     C     *LIKE         DEFINE    WFPRAN        R                              *NUMERO RANGE
088800941031     C*--------------------
088900941031     C* CHIAVI DI LETTRUA
089000941031     C*--------------------
089100930208     C* CHIAVE LETTURA TABEL00F - COMPLETA
089200930208     C     KEYTAB        KLIST
089300930208     C                   KFLD                    KTBUTE
089400930208     C                   KFLD                    KTBCOD
089500930208     C                   KFLD                    KTBKEY
089600950627     C*
089700941205     C* CHIAVE LETTURA TABEL00F - PARZIALE
089800941205     C     KEYTA2        KLIST
089900941205     C                   KFLD                    KTBUTE
090000941205     C                   KFLD                    KTBCOD
090100930526     C*
090200930526     C* CHIAVE LETTURA AZORG01L - COMPLETA
090300930526     C     KEYORG        KLIST
090400930526     C                   KFLD                    KORFIL
090500930622     C*
090600931122     C* CHIAVE LETTURA WFDLP01L - COMPLETA
090700931122     C     KEYWFP        KLIST
090800931122     C                   KFLD                    KWPANN
090900931122     C                   KFLD                    KWPDIV
091000931122     C                   KFLD                    KWPARE
091100931122     C                   KFLD                    KWPFLP
091200931122     C                   KFLD                    KWPMES
091300941205     C                   KFLD                    KWPTSP
091400941205     C                   KFLD                    KWPRAN
091500950627     C                   KFLD                    KWPIOE
091600930208     C*
091700930208     C* LANCIO ROUTINE PER VERIFICARE SE
091800930208     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
091900930208     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
092000020920     C                   EXSR      REPDATIUTE
092100941031     C*--------------------
092200941031     C* CAMPI DI CONTROLLO
092300941031     C*--------------------
092400941031     C* FLAG TESTATA (1-STAMPA; 0-NON STAMPA)
092500941031     C                   MOVEL     '1'           FLGTES            1
092600930208     C                   MOVEL     '0'           FINL00            1
092700930622     C                   Z-ADD     UDATE         DATA              6 0
092800930622     C                   TIME                    ORA               6 0
092900950627     C*
093000950627     C* IMPOSTA L'INTESTAZIONE 'ITALIA'/'IMPORT'/'EPORT'/TUTTO
093100950627SL  1C                   SELECT
093200950627     C     PARIOE        WHENEQ    'I'
093300950627     C                   MOVEL     INT(3)        DECIOE           15
093400950627     C     PARIOE        WHENEQ    'M'
093500950627     C                   MOVEL     INT(4)        DECIOE
093600950627     C     PARIOE        WHENEQ    'X'
093700950627     C                   MOVEL     INT(5)        DECIOE
093800950627     C     PARIOE        WHENEQ    *BLANKS
093900950628     C                   MOVEL     INT(6)        DECIOE
094000950627E   1C                   ENDSL
094100930622     C*
094200930622     C* MEMORIZZA LE DESCRIZIONI DELLE FILIALI E DEI TERMINAL
094300930622     C                   EXSR      CARORG
094400941205     C*
094500941205     C* CARICA TABELLE
094600941205     C                   EXSR      CARTAB
094700930623     C*
094800930628     C* SE SONO STATE INSERITE DELLE PARZIALIZZAZIONI IMPOSTO IL FLAG 02 ON
094900931122     C* PER STAMPARE UNA RIGA IN TESTATA CON LE PARZIALIZZIONI
095000930623     C     PARDIV        IFEQ      *BLANKS
095100930623     C     PARARE        ANDEQ     *ZEROS
095200930623     C     PARFLP        ANDEQ     *ZEROS
095300931122     C     PARTFP        ANDEQ     *ZEROS
095400930623     C     PARFLA        ANDEQ     *ZEROS
095500930623     C     PARTFA        ANDEQ     *ZEROS
095600930623     C                   SETOFF                                       02
095700930623     C                   ELSE
095800930623     C                   SETON                                        02
095900930623     C                   END
096000930624     C*
096100930628     C* SE LANCIO PER SALDI INTESTA STAMPE CON ANNO/MESE (IND.03 OFF)
096200930628     C* SE LANCIO ESTEMPORANEO INTESTA CON DATA INIZIO E FINE (IND.03 ON)
096300930624     C     PARTLA        IFEQ      'S'
096400930624     C                   SETOFF                                       03
096500930624     C                   END
096600930624     C     PARTLA        IFEQ      'E'
096700930624     C                   SETON                                        03
096800930624     C                   END
096900931126     C*
097000000000     C                   ENDSR
097100931122     C*------------------------------------------------------------------------*
097200941031     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
097300931122     C*------------------------------------------------------------------------*
097400930622     C     CARORG        BEGSR
097500930622     C*
097600930622     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
097700930622     C                   Z-ADD     *ZEROS        KORFIL
097800930622     C     KEYORG        SETLL     AZORG01L                           99        *FINE-ARCHIV
097900930622     C  N99              READ      AZORG01L                               99
098000930622     C*
098100930622     C* CICLO FINO A FINE FILE
098200930622     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
098300930622     C     *IN99         DOWEQ     '0'                                          --- 1 -->
098400930622     C*
098500930622     C* ELIMINA RECORD ANNULLATI
098600930622     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
098700930622     C*
098800930622     C* ELIMINA VOCI VARIE
098900930622     C     ORGFAG        IFNE      'V'
099000930622     C                   ADD       1             I
099100930622     C                   Z-ADD     ORGFIL        FIL(I)
099200930622     C                   MOVEL     ORGDES        DFIL(I)
099300930622     C                   END
099400930622     C*
099500930622     C                   END                                                    <-- 2 ---
099600930622     C*
099700930622     C* LEGGE IL RECORD SUCCESSIVO
099800930622     C                   READ      AZORG01L                               99
099900930622     C*
100000930622     C                   END                                                    <-- 1 ---
100100930622     C*
100200931122     C                   ENDSR
100300941205     C*------------------------------------------------------------------------*
100400941205     C* CARTAB - CARICA TABELLE
100500941205     C*------------------------------------------------------------------------*
100600941205     C     CARTAB        BEGSR
100700941205     C*--------------------
100800941205     C* TIPI SERVIZIO
100900941205     C*--------------------
101000941205     C                   Z-ADD     *ZEROS        I
101100020920     C                   MOVEL     1             KTBUTE
101200941205     C                   MOVEL     '5E'          KTBCOD
101300941205     C     KEYTA2        SETLL     TABEL00F
101400941205     C     KEYTA2        READE     TABEL00F                               99
101500941205DO  1C     *IN99         DOWEQ     *OFF
101600941205IF  2C     TBLFLG        IFEQ      *BLANKS
101700941205     C                   ADD       1             I
101800941205     C                   MOVEL     TBLKEY        TSP(I)                         *CODICE SERVIZIO
101900941205     C                   Z-ADD     I             OTSP(I)                        *NUMERO OCUR
102000941205     C                   MOVEL     TBLUNI        DTSP(I)
102100941205E   2C                   ENDIF
102200941205     C     KEYTA2        READE     TABEL00F                               99
102300941205E   1C                   ENDDO
102400941205     C*
102500941205     C                   Z-ADD     I             WULTSP            4 0          *ULTIMO SERV.CARICAT
102600941205     C*
102700941205     C                   ENDSR
102800931122     O*------------------------------------------------------------------------*
102900941031     O* STATISTICA AFFIDABILITA' PARTENZA
103000941031     O*------------------------------------------------------------------------*
103100930622     OPRTF198   E            TESTA          1 02
103200020920     O                       DSFIRS              20
103300950627     O                                           40 'TNSS34R '
103400930624     O                                          100 '   STATISTICA  AFFID'
103500950627     O                                          120 'ABILITA" PARTENZE   '
103600950627     O                       DECIOE             135
103700930622     O                                          150 'Data:'
103800930622     O                       DATA               159 '  /  /  '
103900930622     O                                          167 'Ora:'
104000930622     O                       ORA                178 '  .  .  '
104100930622     O                                          190 'Pagina:'
104200930622     O                       PAGE          Z    198
104300941031     O*------------------------
104400930624     O          E   N03      TESTA          1
104500930622     O                                           90 'ANNO:'
104600941031     O                       PARANN              95
104700930622     O                                          101 'MESI:'
104800930622     O                       PARMMI             104
104900930622     O                                          106 '/'
105000930622     O                       PARMMF             109
105100941031     O*------------------------
105200930624     O          E    03      TESTA          1
105300930624     O                                           90 'DAL:'
105400941031     O                       PARDTI             100 '  /  /    '
105500930624     O                                          105 'AL:'
105600941031     O                       PARDTF             115 '  /  /    '
105700941031     O*------------------------
105800930623     O          E    02      TESTA       1  1
105900930623     O                                           20 'PARZIALIZZAZIONI:   '
106000930623     O                                           40 '             DIVIS.:'
106100930623     O                       PARDIV              41
106200930624     O                                           61 '               AREA:'
106300930624     O                       PARARE        Z     64
106400930624     O                                           84 '          FIL.PART.:'
106500930624     O                       PARFLP        Z     87
106600930624     O                                          107 '         TERM.PART.:'
106700930624     O                       PARTFP        Z    110
106800930624     O                                          130 '           FIL.ARR.:'
106900930624     O                       PARFLA        Z    133
107000930624     O                                          153 '          TERM.ARR.:'
107100930624     O                       PARTFA        Z    156
107200941031     O*------------------------
107300930622     O          E            TESTA       1  1
107400930720     O                       TES(16)             66
107500930720     O                       TES(16)            132
107600930720     O                       TES(16)            198
107700941031     O*------------------------
107800930622     O          E            TESTA          1
107900930622     O                       TES(2)              66
108000930622     O                       TES(3)             132
108100930622     O                       TES(4)             198
108200930623     O                       STINT1              23
108300941031     O*------------------------
108400930622     O          E            TESTA          1
108500930622     O                       TES(5)              66
108600930622     O                       TES(6)             132
108700930622     O                       TES(7)             198
108800930623     O                       STINT2              23
108900941031     O*------------------------
109000930622     O          E            TESTA          1
109100930720     O                       TES(16)             66
109200930720     O                       TES(16)            132
109300930720     O                       TES(16)            198
109400931122     O*------------------------------------------------------------------------*
109500930623     O* PRIMA RIGA: SOLO SE RICHIESTA RIBATTUTA (INDIC.01)
109600930623     O          E    01      RIGA           0
109700941206     O                       STDES               23
109800941205     O                                            1 ' '
109900941205     O                                           24 ' '
110000941205     O                       DESTSP              40
110100941205     O                                           42 ' '
110200941205     O                       WTUTS         Z     51
110300941205     O                                           53 ' '
110400941205     O                       WTSP(1)       Z     62
110500941205     O                       WPER(1)       2     71
110600941205     O                                           73 ' '
110700941205     O                       WTSP(2)       Z     82
110800941205     O                       WPER(2)       2     91
110900941205     O                                           93 ' '
111000941205     O                       WTSP(3)       Z    102
111100941205     O                       WPER(3)       2    111
111200941205     O                                          113 ' '
111300941205     O                       WTSP(4)       Z    122
111400941205     O                       WPER(4)       2    131
111500941205     O                                          133 ' '
111600941205     O                       WTSP(5)       Z    142
111700941205     O                       WPER(5)       2    151
111800941205     O                                          153 ' '
111900941205     O                       WTSP(6)       Z    162
112000941205     O                       WPER(6)       2    171
112100941205     O                                          198 ' '
112200941205     O*------------------------
112300941205     O* PRIMA RIGA: STAMPATA SEMPRE
112400941205     O          E            RIGA           1
112500941206     O                       STDES               23
112600941205     O                                            1 '!'
112700941205     O                                           24 '!'
112800941205     O                       DESTSP              40
112900941205     O                                           42 '!'
113000941205     O                       WTUTS         Z     51
113100941205     O                                           53 '!'
113200941205     O                       WTSP(1)       Z     62
113300941205     O                       WPER(1)       2     71
113400941205     O                                           73 '!'
113500941205     O                       WTSP(2)       Z     82
113600941205     O                       WPER(2)       2     91
113700941205     O                                           93 '!'
113800941205     O                       WTSP(3)       Z    102
113900941205     O                       WPER(3)       2    111
114000941205     O                                          113 '!'
114100941205     O                       WTSP(4)       Z    122
114200941205     O                       WPER(4)       2    131
114300941205     O                                          133 '!'
114400941205     O                       WTSP(5)       Z    142
114500941205     O                       WPER(5)       2    151
114600941205     O                                          153 '!'
114700941205     O                       WTSP(6)       Z    162
114800941205     O                       WPER(6)       2    171
114900941205     O                                          198 '!'
115000931122     O*------------------------------------------------------------------------*
115100930622     O          E            RIGSEP         1
115200930622     O                       TES(1)              66
115300930622     O                       TES(1)             132
115400930622     O                       TES(1)             198
115500931122     O*------------------------------------------------------------------------*
115600941209     O          E            STAFIN           66
115700941205     O                                          110 ' F I N E   S T A M P A '
115800931122     O*------------------------------------------------------------------------*
115900950627** -TES-   (DESCRIZIONI  STAMPA)
116000941205------------------------------------------------------------------  1
116100941205!                      !     T I P O     !  N°SPED  !     ... OK .  2
116200941205..    !     ... 12 ...    !     ... 24 ...    !     ... 48 ...      3
116300941205!     ... 72 ...    !     ... >72 ...                            !  4
116400941205!                      ! S E R V I Z I O !  TOTALI  !    N°SPE      5
116500941205  %   !    N°SPE      %   !    N°SPE      %   !    N°SPE      %     6
116600941205!    N°SPE      %   !    N°SPE      %                            !  7
116700941205                                                                    8
116800941205                                                                    9
116900961003 TOT. P.OPERATIVO PAR                                              10
117000941206 TOT. AREA                                                         11
117100941206 TOT. DIVISIONE                                                    12
117200941206 TOT. GENERALE                                                     13
117300941206 DIVISIONE                                                         14
117400961003 P.OPERATIVO PARTENZA                                              15
117500930720================================================================== 16
117600931122------------------------------------------------------------------ 17
117700930720                                                                   18
117800930720                                                                   19
117900930720                                                                   20
118000941205!                      ! XXXXXXXXXXXXXXX ! 99999999 ! 99999999  99  A
11810094120599,99 ! 99999999  9999,99 ! 99999999  9999,99 ! 99999999  9999,99   B
118200941205! 99999999  9999,99 ! 99999999  9999,99  !                          C
118300950629                                                                   24
118400950629(DESCRIZIONI CHE OCCORRONO ALLA EXPORT PERCHE'INVERTE LNA CON LNP) 25
118500961003 TOT. P.OPERATIVO ARR                                         (10) 26
118600950629                                                                   27
118700950629                                                                   28
118800961003 P.OPERATIVO ARRIVO                                           (15) 29
11890095062930
119000950627** -INT-   (INTESTAZIONE)
1191009506271
1192009506272
119300950627ITALIA              3
119400950627IMPORT              4
119500950627EXPORT              5
119600950629COMPLETA            6
119700950627                    7
119800950627                    8
119900950627                    9
120000950627                    10
