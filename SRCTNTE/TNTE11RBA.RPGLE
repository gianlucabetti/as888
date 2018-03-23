000100970306     H*------------------------------------------------------------------------*
000200970306     H*                   TARIFFE DA C/E - CALCOLA RICAVI                      *
000300970306     H*------------------------------------------------------------------------*
000400970306     H* PARTENDO DAI DATI TIPO / SCAGLIONI / CODICI TASSAZIONE DI UNA TARIFFA
000500970306     H* CALCOLA I RICAVI PER OGNI SCAGLIONE E PER OGNI CODICE TASSAZIONE E LI
000600970306     H* SCRIVE NEL FILE DI LAVORO DELLA PROCEDURA.
000700970306     H*---
000800970306     H* NOTA BENE:
000900970306     H*---
001000970306     H* OFFERTE ---> IL PGM HA DAVANTI DEGLI OVRDBF CHE SOSTITUISCONO LE OFFERTE
001100970306     H* CON LE TARIFFE. PER LEGGERE LE TARIFFE OCCORRE QUINDI LEGGERE LE OFFERTE
001200970306     H* TARIFFE ---> IL PGM NON HA DAVANTI NESSUN OVRDBF. LETTURE NORMALI
001300970306     H*---
001400970306     H*------------------------------------------------------------------------*
001500970306     H DECEDIT('0,') DATEDIT(*DMY.)
001600970306     F*------------------------------------------------------------------------*
001700970306     F* DATA BASE
001800970306     F*------------------------------------------------------------------------*
001900970306     F*------------
002000970306     F* DATI TIPO
002100970306     F*------------
002200010926     FTEETC01L  IF   E           K DISK
002300010926     FTEETS01L  IF   E           K DISK
002400011017     FTEETT01L  IF   E           K DISK
002500970306     F*------------
002600970306     F* WORKFILE
002700970306     F*------------
002800010926     FTEETW01L  UF A E           K DISK
002900010926     FTEETWR1L  UF A E           K DISK
003000970306     F*------------
003100970306     F* ORGANIGRAMMA
003200970306     F*------------
003300970306     FAZORG01L  IF   E           K DISK
003400970306     F*------------
003500011005     F* TABELLE (VECCHIE E NUOVE)
003600990820     F*------------
003700970306     FTABEL00F  IF   E           K DISK
003800011008     FTNTBE01L  IF   E           K DISK
003900011005     F*------------
004000970306     F* TARIFFE
004100970306     F*------------
004200970306     FTNTAM00L  IF   E           K DISK
004300990819     FTITAD00L  IF   E           K DISK
004400990819     FTITPT01L  IF   E           K DISK
004500990819     FTITPD02L  IF   E           K DISK
004600990819     FTITGC01L  IF   E           K DISK
004700970625     F*------------
004800970625     F* OFFERTE
004900970625     F*------------
005000041202     FTIOGC01L  IF   E           K DISK    usropn
005100990819     F                                     RENAME(TITGC000:TIOGC000)
005200970306     F*------------
005300970306     F* OFFERTE             -PER LEGGERE LA CARTELLO-
005400970306     F*------------
005500041202     FTNOFM00L  IF   E           K DISK    usropn
005600970306     F                                     RENAME(TNTAM000:TNOFM000)
005700041202     FTIOPT01L  IF   E           K DISK    usropn
005800990819     F                                     RENAME(TITPT000:TIOPT000)
005900041202     FTIOPD02L  IF   E           K DISK    usropn
006000990819     F                                     RENAME(TITPD000:TIOPD000)
006100970306     D*------------------------------------------------------------------------*
006200970306     D* SCHIERE
006300970306     D*------------------------------------------------------------------------*
006400970306     D*-------------------
006500970306     D* MESSAGGI
006600970306     D*-------------------
006700970306     D MSG             S             78    DIM(30) CTDATA PERRCD(1)
006800970306     D*-------------------
006900970306     D* ELEMENTI TASSAZIONE BOLLA
007000970306     D*-------------------
007100970306     D CC              S              1    DIM(100)                             *VOCE DA CALCOLARE S
007200970306     D*-------------------
007300040826     D* VOCI (INDICE = CODICE VOCE)
007400970306     D*-------------------
007500040826     D VV              S              3  0 DIM(200)                             *CODICE VOCE
007600040826     D VC              S              1    DIM(200)                             *VOCE DA CALCOLARE S
007700040826     D VP              S              9  5 DIM(200)                             *% DA CONSIDERARE
007800040826     D VI              S              1    DIM(200)                             *INGLOBA NEL PORTO S
007900040826     D VS              S              1  0 DIM(200)                             *SEQUENZA CALCOLO
008000040826     D VR              S              3  0 DIM(200)                             *RAGGRUPPAMENTO DELT
008100040826     D VIMP            S             21  8 DIM(200)                             *IMPORTO CALCOLATO V
008200040826     D VIM             S             21  8 DIM(200)                             *IMPORTO DA SCRIVERE
008300970306     D*------------------
008400970306     D* IMPORTO RAGGRUPPAMENTI DELTA
008500970306     D*------------------
008600011031     D IRDL            S             21  8 DIM(50)
008700970306     D*-------------------
008800970306     D* SCAGLIONI
008900970306     D*-------------------
009000011031     D SSGL            S             21  8 DIM(10)                              *SCAGLIONE SUPERIORE
009100970306     D SPSC            S              7  1 DIM(10)                              *PESO DI RIFERIMENTO
009200970414     D SCSC            S              7  1 DIM(10)                              *COLLI DI RIFERIMENT
009300970306     D SVSC            S              7  3 DIM(10)                              *VOLUME DI RIFERIMEN
009400011031     D SINL            S             21  8 DIM(10)                              *INOLTRO
009500970318     D SRPV            S              3  1 DIM(10)                              *RAPPORTO PESO/VOLUM
009600970414     D SSDE            S              1    DIM(10)                              *SEGNO DELTA
009700970414     D SPDE            S              6  3 DIM(10)                              *% DELTA
009800970306     D SPPE            S              4  1 DIM(10)                              *% INCIDENZA
009900970306     D*-------------------
010000970306     D* CODICI TASSAZIONE
010100970306     D*-------------------
010200050103     D CCTS            S              2    DIM(600)                             *CODICE
010300050103     D CRCT            S              1    DIM(600)                             *RAGGRUPPAMENTO
010400050103     D CPPE            S              4  1 DIM(600)                             *% INCIDENZA
010500050103     D CLNA            S              3  0 DIM(600)                             *LINEA ARRIVO
010600050103     D CFL1            S              1    DIM(600)                             *FLAG ITALIA/ESTERO
010700050103     D CTFA            S              3  0 DIM(600)                             *TERMINAL ARRIVO
010800050103     D CRAP            S              2    DIM(600)                             *REGIONE APPARTENENZ
010900050103     D CNAR            S              2    DIM(600)                             *NAZIONE APPARTENENZ
011000050103     D C§§5            S              1    DIM(600)                             *FLAG ISOLA/NONISOLA
011100970306     D*-------------------
011200970306     D* SCHIERE CALCOLO TARIFFE GIACENZA
011300970306     D*-------------------
011400970306     D TGS             S              3  0 DIM(3)                               *GG SOSTA
011500011031     D TTS             S             11  3 DIM(3)                               *TARIFFE SOSTA
011600970306     D SGG             S              3  0 DIM(3)                               *GG SOSTA
011700011031     D STU             S             21  8 DIM(3)                               *TARIFFA SOSTA UNITA
011800011031     D STT             S             21  8 DIM(3)                               *TARIFFA SOSTA TOTAL
011900970306     D*-------------------
012000970306     D* SCHIERE PER LA TASSAZIONE
012100970306     D*-------------------
012200970306     D IAC             S              5  1 DIM(12)                              *PUNTI ISTAT
012300970306     D DIA             S              8  0 DIM(12)                              *DATE SCATTI ISTAT
012400980707     D CTR             S              1    DIM(15)                              *TIPI TARIFFA
012500020104     D CP              S              1    DIM(50)                              *TARIFFE PARTICOLARI
012600970306     D TS              S              1    DIM(10)                              *TIPI SERVIZIO
012700050103     D CTS             S              2    DIM(600)                             *CODICI TASSAZIONE
012800970306     D*-------------------
012900970306     D* DS FORMATO RECORD TARIFFE TESTATA
013000970306     D*-------------------
013100970306     D TAMDS         E DS                  EXTNAME(TNTAM00F) INZ
013200970306     D*-------------------
013300970306     D* INFDS - INFORMAZIONI PROGRAMMA
013400970306     D*-------------------
013500970306     D                SDS
013600980909     D  JOB                  244    253
013700980909     D  USER                 254    263
013800970715     D*------------------
013900970715     D* DS "TISI95R" - CONTROLLO CAP
014000970715     D*------------------
014100970715     D DSSI95        E DS                  EXTNAME(TISI95DS)
014200970715     D*------------------
014300970715     D* DS "FNLV55R" - REPERIMENTO TERMINALs
014400970715     D*------------------
014500970715     D DSLV55        E DS                  EXTNAME(FNLV55DS)
014600970306     D*-------------------
014700970306     D* DS DI PROCEDURA
014800970306     D*-------------------
014900970306     D DSTE00        E DS                  EXTNAME(TNTE00DS)
015000991125     D*-------------------
015100991125     D* CONVERSIONI IMPORTI
015200991125     D*-------------------
015300991125     D YEUR          E DS                  EXTNAME(YEURCODS)
015400970306     D*-------------------
015500970306     D* ARCHITETTURA
015600970306     D*-------------------
015700970306     D KPJBA         E DS
015800970306     D*-------------------
015900970306     D* DS "XSRDA8" - CONTROLLO DATA (8)
016000970306     D*-------------------
016100970306     D WLBDA8          DS                  INZ
016200970306     D  G08DAT                 1      8  0
016300970306     D  G08INV                 9     16  0
016400970306     D  G08ERR                17     17
016500970306     D  G08TGI                18     22  0
016600021202     D*-------------------
016700021202     D* DS *PGM REPIMEMTO TARIFFE DI CARTELLO
016800021202     D*-------------------
016900021202     D TRUL27DS      E DS
017000970306     D*-------------------
017100970306     D* DS ISTAT -UNO-
017200970306     D*-------------------
017300970306     D DSQI1         E DS                  INZ
017400970306     D*-------------------
017500970306     D* DS ISTAT -DUE-
017600970306     D*-------------------
017700970306     D DSQI2         E DS                  INZ
017800970306     D*-------------------
017900970306     D* DS SIGLE VARIE
018000970306     D*-------------------
018100970306     D DS$2          E DS
018200970306     D*-------------------
018300980506     D* DS TIPI TARIFFA                     -15 TIPI TARIFFA-
018400970306     D*-------------------
018500980506     D DSTR          E DS                  OCCURS(15) INZ
018600040906     D*-------------------
018700040906     D* DS nazioni
018800040906     D*-------------------
018900040906     D DS15          E DS
019000970306     D*-------------------
019100050103     D* DS CODICI TASSAZIONE                -600 CODICI TASSAZIONE-
019200970306     D*-------------------
019300050103     D DSCT          E DS                  OCCURS(600) INZ
019400970306     D*-------------------
019500970306     D* DS TIPI SERVIZIO                    -10 TIPI SERVIZIO-
019600970306     D*-------------------
019700970306     D DS5E          E DS                  OCCURS(10) INZ
019800970306     D*-------------------
019900020104     D* DS CONSEGNE PARTICOLARI             -50 PARTICOLARITA'-
020000980706     D*-------------------
020100020104     D DS1P          E DS                  OCCURS(50) INZ
020200980706     D*-------------------
020300980706     D* FLAG OPERATIVI -TNTAM-
020400980706     D*-------------------
020500980706     D DSTA01        E DS
020600011005     D*------------------
020700011005     D* DS LETTURA TABELLA "GED" TIPO RECORD "CE"
020800011005     D*------------------
020900011005     D DSGEDCE       E DS
021000040716     D*------------------
021100040716     D* DS LETTURA TABELLA "GEI" TIPO RECORD "EUR" (limite risarcibile
021200040716     D*------------------
021300040716     D Dgei          E DS
021400970306     D*-------------------
021500020104     D* CONSEGNE PARTICOLARI DI CARTELLO    -50 PARTICOLARITA'-
021600970306     D*-------------------
021700020104     D CPIDS           DS                  OCCURS(50)
021800010926     D  TTPG8P                 1      1
021900011031     D  TARF8P                 2     22  8
022000011031     D  TARL8P                23     43  8
022100011031     D  TARO8P                44     64  8
022200011031     D  TRPV8P                65     67  1
022300011031     D  CP88                  68     68
022400011031     D  TCTS8P                69     70
022500011031     D  TCTE8P                71     72
022600011031     D  TAPL8P                73     73
022700970306     D*-------------------
022800970306     D* ASSICURAZIONE PER CONTO             -10 SCAGLIONI-
022900970306     D*-------------------
023000970306     D ACDS            DS                  OCCURS(10)
023100011031     D  ASGL                   1     21  8
023200011031     D  AITR                  22     42  8
023300011031     D  AMIN                  43     63  8
023400011031     D  AMAX                  64     84  8
023500011031     D  AAIN                  85     85
023600970306     D*-------------------
023700980707     D* DETTAGLIO TARIFFARIO CLIENTE        -11 SCAGLIONI-
023800970306     D*-------------------
023900980707     D TARDS           DS                  OCCURS(11)
024000990819     D  TLNP                   1      3  0
024100990819     D  TCTS                   4      5
024200990819     D  TNAZ                   6      8
024300990819     D  TCAP                   9     17
024400990819     D  TCTR                  18     20  0
024500011031     D  TSCG                  21     41  8
024600011031     D  TITR                  42     62  8
024700011031     D  TINO                  63     83  8
024800011031     D  TRPV                  84     86  1
024900011031     D  TMIN                  87    107  8
025000011031     D  TMAX                 108    128  8
025100001218     D*-------------------
025200001218     D* COMPOSIZIONI MESSAGGI
025300001218     D*-------------------
025400001218     D* MSG,26
025500001218     D                 DS
025600001218     D  DM26A                 76     77
025700001218     D  DMSG26                 1     78
025800020729     D*-------------------
025900020729     D* DS RIDEFINIZIONE DESCRIZIONE 143 ORGANIGRAMMA
026000020729     D*-------------------
026100020729     D OG143         E DS
026200020729     D*------------------
026300020729     D* DS PER LETTURA TABELLA "NTW"
026400020729     D*------------------
026500020729     D DNTW          E DS
026600970306     I*------------------------------------------------------------------------*
026700970306     I* INPUT
026800970306     I*------------------------------------------------------------------------*
026900970306     I*-------------------
027000970306     I* DS FORMATO RECORD TARIFFE GIACENZA
027100970306     I*-------------------
027200990819     ITITGC000
027300970306     I              TGCSG1                      TGS(1)
027400970306     I              TGCSG2                      TGS(2)
027500970306     I              TGCSG3                      TGS(3)
027600970306     I              TGCST1                      TTS(1)
027700970306     I              TGCST2                      TTS(2)
027800970306     I              TGCST3                      TTS(3)
027900970625     I*-------------------
028000970625     I* DS FORMATO RECORD TARIFFE GIACENZA
028100970625     I*-------------------
028200990819     ITIOGC000
028300970625     I              TGCSG1                      TGS(1)
028400970625     I              TGCSG2                      TGS(2)
028500970625     I              TGCSG3                      TGS(3)
028600970625     I              TGCST1                      TTS(1)
028700970625     I              TGCST2                      TTS(2)
028800970625     I              TGCST3                      TTS(3)
028900970306     C*------------------------------------------------------------------------*
029000970306     C* MAIN LINE
029100970306     C*------------------------------------------------------------------------*
029200970306     C*
029300970306     C* CANCELLA IL FILE DI LAVORO
029400970306IF  1C     WERR          IFEQ      'N'
029500970306     C                   EXSR      CANCWF
029600970306E   1C                   ENDIF
029700970306     C*
029800970306     C* CARICA LE TABELLE OCCORRENTI
029900970306IF  1C     WERR          IFEQ      'N'
030000970306     C                   EXSR      CARTAB
030100970306E   1C                   ENDIF
030200970715     C*
030300970306     C* IMPOSTA LA SEQUENZA DI CALCOLO DI TUTTE LE VOCI
030400970306IF  1C     WERR          IFEQ      'N'
030500970306     C                   EXSR      IMPSEQ
030600970306E   1C                   ENDIF
030700970306     C*
030800970306     C* CARICA DATI TIPO -GENERALI-
030900970306IF  1C     WERR          IFEQ      'N'
031000970306     C                   EXSR      CARETC
031100970306E   1C                   ENDIF
031200970306     C*
031300970306     C* CARICA DATI TIPO -SCAGLIONI-
031400970306IF  1C     WERR          IFEQ      'N'
031500970306     C                   EXSR      CARETS
031600970306E   1C                   ENDIF
031700970306     C*
031800970306     C* CARICA DATI TIPO -CODICI TASSAZIONE-
031900970306IF  1C     WERR          IFEQ      'N'
032000970306     C                   EXSR      CARETT
032100970306E   1C                   ENDIF
032200970306     C*
032300970306     C* DETERMINA LE VOCI DA CALCOLARE
032400970306IF  1C     WERR          IFEQ      'N'
032500970306     C                   EXSR      CHKVOC
032600970306E   1C                   ENDIF
032700970306     C*
032800970306     C* CARICA LA TARIFFA DEL CLIENTE -TESTATA-
032900970306IF  1C     WERR          IFEQ      'N'
033000970306     C                   EXSR      CARTAM
033100970306E   1C                   ENDIF
033200970306     C*
033300970306     C* CARICA LA TARIFFA GIACENZA DEL CLIENTE
033400970306IF  1C     WERR          IFEQ      'N'
033500970306IF  2C     VC(8)         IFEQ      'S'
033600970306     C     VC(18)        OREQ      'S'
033700970306     C     VC(19)        OREQ      'S'
033800970306     C     VC(20)        OREQ      'S'
033900970306     C                   MOVEL     '0'           $TGC
034000970306     C                   EXSR      CARTGC
034100970306E   2C                   ENDIF
034200970306E   1C                   ENDIF
034300970306     C*
034400970306     C* VALORIZZA LE VOCI DA CALCOLARE -FISSE-
034500970306IF  1C     WERR          IFEQ      'N'
034600970306     C                   EXSR      VALVO1
034700970306E   1C                   ENDIF
034800970306     C*
034900970306     C* CICLO PER TUTTI GLI SCAGLIONI ----------------------------------------->
035000970306DO  1C     1             DO        WULTSC        S                 6 0
035100970306IF  2C     SSGL(S)       IFNE      *ZEROS
035200970306     C*
035300970306     C* DETERMINA LE VOCI DA CALCOLARE
035400970306IF  3C     WERR          IFEQ      'N'
035500970306     C                   EXSR      CHKVOC
035600970306E   3C                   ENDIF
035700970306     C*
035800970306     C* VALORIZZA LE VOCI DA CALCOLARE -DIPENDENTI DA SCAGLIONI-
035900970306IF  3C     WERR          IFEQ      'N'
036000970306     C                   EXSR      VALVO2
036100970306E   3C                   ENDIF
036200970306     C*
036300970306     C* CICLO PER TUTTI I CODICI TASSAZIONE ----------------------------------->
036400970306DO  3C                   DO        WULTCT        T                 6 0
036500970306IF  4C     CCTS(T)       IFNE      *BLANKS
036600970306     C*
036700970306     C* DETERMINA LE VOCI DA CALCOLARE
036800970306IF  5C     WERR          IFEQ      'N'
036900970306     C                   EXSR      CHKVOC
037000970306E   5C                   ENDIF
037100970306     C*
037200970306     C* CARICA LA TARIFFA DEL CLIENTE -DETTAGLIO-
037300970306IF  5C     WERR          IFEQ      'N'
037400970306     C                   EXSR      CARTAD
037500970306E   5C                   ENDIF
037600970306     C*
037700970306     C* REPERISCE E CALCOLA LA QUANTITA' DA UTILIZZARE
037800970306IF  5C     WERR          IFEQ      'N'
037900970306     C                   EXSR      CALQTA
038000970306E   5C                   ENDIF
038100970306     C*
038200970306     C* VALORIZZA LE VOCI DA CALCOLARE -DIPENDENTI DA CODICI TASSAZIONE-
038300970306IF  5C     WERR          IFEQ      'N'
038400970306     C                   EXSR      VALVO3
038500970306E   5C                   ENDIF
038600970306     C*
038700970306     C* CALCOLA IMPORTI SULLE % INCIDENZA E I RAGGUPPAMENTI DELTA
038800970306IF  5C     WERR          IFEQ      'N'
038900970306     C                   EXSR      CALTOT
039000970306E   5C                   ENDIF
039100970306     C*
039200011017     C* SCRIVE IL FILE DI LAVORO TEETW
039300970306IF  5C     WERR          IFEQ      'N'
039400970306     C                   EXSR      SCRETW
039500970306E   5C                   ENDIF
039600970306     C*
039700010926     C* SCRIVE IL FILE DI LAVORO TEETWR SE LANCIO IN DEBUG
039800970306IF  5C     WERR          IFEQ      'N'
039900970306     C     D00DBG        ANDEQ     'S'
040000970306     C                   EXSR      SCREWR
040100970306E   5C                   ENDIF
040200970306E   4C                   ENDIF
040300970306E   3C                   ENDDO                                                  *FINE COD.TASSAZIONE
040400970306     C*
040500970306E   2C                   ENDIF
040600970306E   1C                   ENDDO                                                  *FINE SCAGLIONI
040700980702     C*
040800970715     C* OPERAZIONI FINALI
040900970715     C                   EXSR      FINSR
041000970306     C*
041100970306     C                   SETON                                        LR
041200970306     C*------------------------------------------------------------------------*
041300970306     C* IMPSEQ - IMPOSTA LA SEQUENZA DI CALCOLO DI TUTTE LE VOCI
041400970306     C*------------------------------------------------------------------------*
041500970306     C     IMPSEQ        BEGSR
041600970306     C*
041700970306     C* DETERMINA IL CODICE VOCE
041800970306     C                   EXSR      CODVOC
041900970306     C*
042000970306     C* DETERMINA LA SEQUENZA DELLE VOCI
042100970306     C                   EXSR      SEQVOC
042200970306     C*
042300970306     C* DETERMINA IL RAGGRUPPAMENTO DELTA DELLE VOCI
042400970306     C                   EXSR      RAGVOC
042500970306     C*
042600970306     C                   ENDSR
042700970306     C*------------------------------------------------------------------------*
042800970306     C* CODVOC - DETERMINA IL CODICE DELLE VOCI
042900970306     C*------------------------------------------------------------------------*
043000970306     C     CODVOC        BEGSR
043100970306     C*
043200970306     C                   Z-ADD     001           VV(1)                          *PORTO
043300970721     C                   Z-ADD     002           VV(2)                          *DIRITTO FISSO
043400970717     C                   Z-ADD     003           VV(3)                          *ADG X PORTO
043500970306     C                   Z-ADD     004           VV(4)                          *ISTAT X PORTO
043600970721     C                   Z-ADD     005           VV(5)                          *TCP
043700970306     C                   Z-ADD     006           VV(6)                          *INOLTRO
043800970717     C                   Z-ADD     007           VV(7)                          *ESPRESSO SU PORTO
043900970721     C                   Z-ADD     008           VV(8)                          *GIACENZA
044000970306     C                   Z-ADD     009           VV(9)                          *ASS.CONTO FRANCO
044100970306     C                   Z-ADD     010           VV(10)                         *ASS.RCV   FRANCO
044200970306     C                   Z-ADD     011           VV(11)                         *CONS. APPUNTAMENTO
044300970306     C                   Z-ADD     012           VV(12)                         *CONS. SUPERMERCATI
044400970306     C                   Z-ADD     013           VV(13)                         *CONS. AI PIANI
044500970306     C                   Z-ADD     014           VV(14)                         *CONS. FUORI MISURA
044600970721     C                   Z-ADD     015           VV(15)                         *C/ASSEGNO MITTENTE
044700970721     C                   Z-ADD     016           VV(16)                         *C/ASSEGNO BARTOLINI
044800970306     C                   Z-ADD     017           VV(17)                         *RITIRO
044900970721     C                   Z-ADD     018           VV(18)                         *GIACENZA
045000970721     C                   Z-ADD     019           VV(19)                         *GIACENZA
045100970721     C                   Z-ADD     020           VV(20)                         *GIACENZA
045200970717     C                   Z-ADD     021           VV(21)                         *ADG X INOLTRO
045300970717     C                   Z-ADD     022           VV(22)                         *ADG X DIR.FISSO
045400970717     C                   Z-ADD     023           VV(23)                         *ADG X ESPRESSO/PORT
045500970306     C                   Z-ADD     024           VV(24)                         *ISTAT X INOLTRO
045600970306     C                   Z-ADD     025           VV(25)                         *ISTAT X DIR.FISSO
045700970717     C                   Z-ADD     026           VV(26)                         *ISTAT X ESPRES/PORT
045800970306     C                   Z-ADD     027           VV(27)                         *ISTAT X ADG/PORTO
045900970306     C                   Z-ADD     028           VV(28)                         *ISTAT X ADG/D.FISSO
046000970306     C                   Z-ADD     029           VV(29)                         *ISTAT X ADG/INOLTRO
046100970717     C                   Z-ADD     030           VV(30)                         *ISTAT X ADG/ESP/POR
046200970306     C                   Z-ADD     031           VV(31)                         *ASS.CONTO ASSEGNATO
046300970306     C                   Z-ADD     032           VV(32)                         *ASS.RCV ASSEGNATO
046400970414     C                   Z-ADD     033           VV(33)                         *CONS. DDT
046500970721     C                   Z-ADD     034           VV(34)                         *ISTAT X ADG/ESP/INO
046600970414     C                   Z-ADD     035           VV(35)                         *CONS. LOC.DISAGIATE
046700970717     C                   Z-ADD     036           VV(36)                         *ESPRESSO SU INOLTRO
046800970717     C                   Z-ADD     037           VV(37)                         *ADG X ESPRES/INOLTR
046900970717     C                   Z-ADD     038           VV(38)                         *ISTAT X ESPRES/INOL
047000970721     C                   Z-ADD     039           VV(39)                         *ESPRESSO SU L.DISAG
047100970721     C                   Z-ADD     040           VV(40)                         *ADG X ESPRES/L.DISA
047200970721     C                   Z-ADD     041           VV(41)                         *ISTAT X ESPRES/L.DI
047300970721     C                   Z-ADD     042           VV(42)                         *ISTAT X ADG/ESP/LDI
047400971023     C                   Z-ADD     043           VV(43)                         *ADG X L.DISAGIATE
047500971023     C                   Z-ADD     044           VV(44)                         *ISTAT X L.DISAGIATE
047600971023     C                   Z-ADD     045           VV(45)                         *ISTAT X ADG/L.DISAG
047700001219     C                   Z-ADD     046           VV(46)                         *ADG SU TCP
047800001219     C                   Z-ADD     047           VV(47)                         *ADG SU GIACENZA VAR
047900001219     C                   Z-ADD     048           VV(48)                         *ADG SU ASSIC X CONT
048000001219     C                   Z-ADD     049           VV(49)                         *ADG SU ASSIC RCV  F
048100001219     C                   Z-ADD     050           VV(50)                         *ADG SU CONSEGNA APP
048200001219     C                   Z-ADD     051           VV(51)                         *ADG SU CONSEGNA SUP
048300001219     C                   Z-ADD     052           VV(52)                         *ADG SU CONSEGNA PIA
048400001219     C                   Z-ADD     053           VV(53)                         *ADG SU CONSEGNA FUO
048500001219     C                   Z-ADD     054           VV(54)                         *ADG SU C/ASSEGNO MI
048600001219     C                   Z-ADD     055           VV(55)                         *ADG SU C/ASSEGNO BA
048700001219     C                   Z-ADD     056           VV(56)                         *ADG SU RITIRO
048800001219     C                   Z-ADD     057           VV(57)                         *ADG SU GIACENZA CON
048900001219     C                   Z-ADD     058           VV(58)                         *ADG SU GIACENZA CON
049000001219     C                   Z-ADD     059           VV(59)                         *ADG SU GIACENZA SOS
049100001219     C                   Z-ADD     060           VV(60)                         *ADG SU ASSIC X CONT
049200001219     C                   Z-ADD     061           VV(61)                         *ADG SU ASSIC RCV AS
049300001219     C                   Z-ADD     062           VV(62)                         *ADG SU CONSEGNA DDT
049400001219     C                   Z-ADD     063           VV(63)                         *ISTAT SU ADG/TCP
049500001219     C                   Z-ADD     064           VV(64)                         *ISTAT SU ADG/GIAC.V
049600001219     C                   Z-ADD     065           VV(65)                         *ISTAT SU ADG/ASSXCO
049700001219     C                   Z-ADD     066           VV(66)                         *ISTAT SU ADG/ASS RC
049800001219     C                   Z-ADD     067           VV(67)                         *ISTAT SU ADG/CONSEG
049900001219     C                   Z-ADD     068           VV(68)                         *ISTAT SU ADG/CONSEG
050000001219     C                   Z-ADD     069           VV(69)                         *ISTAT SU ADG/CONSEG
050100001219     C                   Z-ADD     070           VV(70)                         *ISTAT SU ADG/CONSEG
050200001219     C                   Z-ADD     071           VV(71)                         *ISTAT SU ADG/CASSEG
050300001219     C                   Z-ADD     072           VV(72)                         *ISTAT SU ADG/CASSEG
050400001219     C                   Z-ADD     073           VV(73)                         *ISTAT SU ADG/RITIRO
050500001219     C                   Z-ADD     074           VV(74)                         *ISTAT SU ADG/GIACEN
050600001219     C                   Z-ADD     075           VV(75)                         *ISTAT SU ADG/GIACEN
050700001219     C                   Z-ADD     076           VV(76)                         *ISTAT SU ADG/GIACEN
050800001219     C                   Z-ADD     077           VV(77)                         *ISTAT SU ADG/ASSIC
050900001219     C                   Z-ADD     078           VV(78)                         *ISTAT SU ADG/ASSIC
051000001219     C                   Z-ADD     079           VV(79)                         *ISTAT SU ADG/CONSEG
051100011030     C                   Z-ADD     080           VV(80)                         *CONS.ISOLA
051200011030     C                   Z-ADD     081           VV(81)                         *ESPRESSO SU C.ISOLA
051300011030     C                   Z-ADD     082           VV(82)                         *ADG X ESPRES/C.ISOL
051400011030     C                   Z-ADD     083           VV(83)                         *ISTAT X ESPRES/C.IS
051500011030     C                   Z-ADD     084           VV(84)                         *ISTAT X ADG/ESP/CIS
051600011030     C                   Z-ADD     085           VV(85)                         *ADG X CONS.ISOLA
051700011030     C                   Z-ADD     086           VV(86)                         *ISTAT X CONS.ISOLA
051800011030     C                   Z-ADD     087           VV(87)                         *ISTAT X ADG/R.ISOLA
051900011030     C                   Z-ADD     088           VV(88)                         *RITIRO ISOLA
052000011030     C                   Z-ADD     089           VV(89)                         *ESPRESSO SU R.ISOLA
052100011030     C                   Z-ADD     090           VV(90)                         *ADG X ESPRES/R.ISOL
052200011030     C                   Z-ADD     091           VV(91)                         *ISTAT X ESPRES/R.IS
052300011030     C                   Z-ADD     092           VV(92)                         *ISTAT X ADG/ESP/RIS
052400011030     C                   Z-ADD     093           VV(93)                         *ADG X RITIRO ISOLA
052500011030     C                   Z-ADD     094           VV(94)                         *ISTAT X RITIROISOLA
052600011030     C                   Z-ADD     095           VV(95)                         *ISTAT X ADG/R.ISOLA
052700040826     C                   Z-ADD     096           VV(96)                         *ISTAT X ADG/R.ISOLA
052800040826     C                   Z-ADD     097           VV(97)                         *ISTAT X ADG/R.ISOLA
052900040826     C                   Z-ADD     098           VV(98)                         *ISTAT X ADG/R.ISOLA
053000040826     C                   Z-ADD     099           VV(99)                         *ISTAT X ADG/R.ISOLA
053100040826     C                   Z-ADD     100           VV(100)                        *ISTAT X ADG/R.ISOLA
053200040826     C                   Z-ADD     101           VV(101)                        *ISTAT X ADG/R.ISOLA
053300040826     C                   Z-ADD     102           VV(102)                        *ISTAT X ADG/R.ISOLA
053400040826     C                   Z-ADD     103           VV(103)                        *ISTAT X ADG/R.ISOLA
053500040826     C                   Z-ADD     104           VV(104)                        *ISTAT X ADG/R.ISOLA
053600971023     C*
053700040826     C                   Z-ADD     104           NVOCA             3 0          *N° VOCI DA CALCOLAR
053800970306     C*
053900970306     C                   ENDSR
054000970306     C*------------------------------------------------------------------------*
054100970306     C* SEQVOC - DETERMINA LA SEQUENZA DI CALCOLO DELLE VOCI
054200970306     C*------------------------------------------------------------------------*
054300970306     C     SEQVOC        BEGSR
054400970306     C*---
054500970306     C* VOCI FISSE = SEQUENZA UNO
054600970306     C*---
054700970306     C                   Z-ADD     1             VS(8)                          *GIACENZA
054800001222     C                   Z-ADD     1             VS(47)                         *ADG SU GIACENZA VAR
054900001222     C                   Z-ADD     1             VS(64)                         *ISTAT SU ADG/GIAC.V
055000970306     C*---
055100970306     C* VOCI DIPENDENTI DA SCAGLIONI = SEQUENZA DUE
055200970306     C*---
055300970306     C                   Z-ADD     2             VS(18)                         *GIACENZA
055400001227     C                   Z-ADD     2             VS(57)                         *ADG SU GIACENZA CON
055500001227     C                   Z-ADD     2             VS(74)                         *ISTAT SU ADG/GIACEN
055600970306     C                   Z-ADD     2             VS(19)                         *GIACENZA
055700001227     C                   Z-ADD     2             VS(58)                         *ADG SU GIACENZA CON
055800001227     C                   Z-ADD     2             VS(75)                         *ISTAT SU ADG/GIACEN
055900970306     C                   Z-ADD     2             VS(20)                         *GIACENZA
056000001227     C                   Z-ADD     2             VS(59)                         *ADG SU GIACENZA SOS
056100001227     C                   Z-ADD     2             VS(76)                         *ISTAT SU ADG/GIACEN
056200970306     C*---
056300970306     C* VOCI DIPENDENTI DA SCAGLIONI/CT/LNA/TERM ARRIVO = SEQUENZA TRE
056400970306     C*---
056500970306     C                   Z-ADD     3             VS(1)                          *PORTO
056600980702     C                   Z-ADD     3             VS(2)                          *DIRITTO FISSO
056700990819     C                   Z-ADD     3             VS(3)                          *A.D.G X PORTO
056800970306     C                   Z-ADD     3             VS(4)                          *ISTAT X PORTO
056900990819     C                   Z-ADD     3             VS(5)                          *T.C.P.
057000970306     C                   Z-ADD     3             VS(6)                          *INOLTRO
057100970717     C                   Z-ADD     3             VS(7)                          *ESPRESSO PORTO
057200970306     C                   Z-ADD     3             VS(9)                          *ASS.CONTO FRANCO
057300970306     C                   Z-ADD     3             VS(10)                         *ASSICURAZIONE R.C.V
057400970306     C                   Z-ADD     3             VS(11)                         *CONS. APPUNTAMENTO
057500970306     C                   Z-ADD     3             VS(12)                         *CONS. SUPERMERCATI
057600970306     C                   Z-ADD     3             VS(13)                         *CONS. AI PIANI
057700970306     C                   Z-ADD     3             VS(14)                         *CONS. FUORI MISURA
057800980702     C                   Z-ADD     3             VS(15)                         *C/ASSEGNO MITTENTE
057900980702     C                   Z-ADD     3             VS(16)                         *C/ASSEGNO BARTOLINI
058000970306     C                   Z-ADD     3             VS(17)                         *RITIRO
058100970306     C                   Z-ADD     3             VS(21)                         *A.D.G X INOLTRO
058200970306     C                   Z-ADD     3             VS(22)                         *A.D.G X DIR.FISSO
058300970717     C                   Z-ADD     3             VS(23)                         *ADG X ESPRESSO/PORT
058400970306     C                   Z-ADD     3             VS(24)                         *ISTAT X INOLTRO
058500970306     C                   Z-ADD     3             VS(25)                         *ISTAT X DIR.FISSO
058600970717     C                   Z-ADD     3             VS(26)                         *ISTAT X ESPRES/PORT
058700970306     C                   Z-ADD     3             VS(27)                         *ISTAT X ADG/PORTO
058800970306     C                   Z-ADD     3             VS(28)                         *ISTAT X ADG/D.FISSO
058900970306     C                   Z-ADD     3             VS(29)                         *ISTAT X ADG/INOLTRO
059000970717     C                   Z-ADD     3             VS(30)                         *ISTAT X ADG/ESP/POR
059100970306     C                   Z-ADD     3             VS(31)                         *ASS.CONTO ASSEGNATO
059200970306     C                   Z-ADD     3             VS(32)                         *ASS.RCV ASSEGNATO
059300970414     C                   Z-ADD     3             VS(33)                         *CONS. DDT
059400970721     C                   Z-ADD     3             VS(34)                         *ISTAT X ADG/ESP/INO
059500970414     C                   Z-ADD     3             VS(35)                         *CONS. LOC.DISAGIATE
059600970717     C                   Z-ADD     3             VS(36)                         *ESPRESSO INOLTRO
059700970717     C                   Z-ADD     3             VS(37)                         *ADG X ESPRESSO/INOL
059800970717     C                   Z-ADD     3             VS(38)                         *ISTAT X ESPRES/INOL
059900970721     C                   Z-ADD     3             VS(39)                         *ESPRESSO SU L.DISAG
060000970721     C                   Z-ADD     3             VS(40)                         *ADG X ESPRES/L.DISA
060100970721     C                   Z-ADD     3             VS(41)                         *ISTAT X ESPRES/L.DI
060200970721     C                   Z-ADD     3             VS(42)                         *ISTAT X ADG/ESP/LDI
060300971023     C                   Z-ADD     3             VS(43)                         *ADG X L.DISAGIATE
060400971023     C                   Z-ADD     3             VS(44)                         *ISTAT X L.DISAGIATE
060500971023     C                   Z-ADD     3             VS(45)                         *ISTAT X ADG/L.DISAG
060600001219     C                   Z-ADD     3             VS(46)                         *ADG SU TCP
060700001219     C                   Z-ADD     3             VS(48)                         *ADG SU ASSIC X CONT
060800001219     C                   Z-ADD     3             VS(49)                         *ADG SU ASSIC RCV  F
060900001219     C                   Z-ADD     3             VS(50)                         *ADG SU CONSEGNA APP
061000001219     C                   Z-ADD     3             VS(51)                         *ADG SU CONSEGNA SUP
061100001219     C                   Z-ADD     3             VS(52)                         *ADG SU CONSEGNA PIA
061200001219     C                   Z-ADD     3             VS(53)                         *ADG SU CONSEGNA FUO
061300001219     C                   Z-ADD     3             VS(54)                         *ADG SU C/ASSEGNO MI
061400001219     C                   Z-ADD     3             VS(55)                         *ADG SU C/ASSEGNO BA
061500001219     C                   Z-ADD     3             VS(56)                         *ADG SU RITIRO
061600001219     C                   Z-ADD     3             VS(60)                         *ADG SU ASSIC X CONT
061700001219     C                   Z-ADD     3             VS(61)                         *ADG SU ASSIC RCV AS
061800001219     C                   Z-ADD     3             VS(62)                         *ADG SU CONSEGNA DDT
061900001219     C                   Z-ADD     3             VS(63)                         *ISTAT SU ADG/TCP
062000001219     C                   Z-ADD     3             VS(65)                         *ISTAT SU ADG/ASSXCO
062100001219     C                   Z-ADD     3             VS(66)                         *ISTAT SU ADG/ASS RC
062200001219     C                   Z-ADD     3             VS(67)                         *ISTAT SU ADG/CONSEG
062300001219     C                   Z-ADD     3             VS(68)                         *ISTAT SU ADG/CONSEG
062400001219     C                   Z-ADD     3             VS(69)                         *ISTAT SU ADG/CONSEG
062500001219     C                   Z-ADD     3             VS(70)                         *ISTAT SU ADG/CONSEG
062600001219     C                   Z-ADD     3             VS(71)                         *ISTAT SU ADG/CASSEG
062700001219     C                   Z-ADD     3             VS(72)                         *ISTAT SU ADG/CASSEG
062800001219     C                   Z-ADD     3             VS(73)                         *ISTAT SU ADG/RITIRO
062900001219     C                   Z-ADD     3             VS(77)                         *ISTAT SU ADG/ASSIC
063000001219     C                   Z-ADD     3             VS(78)                         *ISTAT SU ADG/ASSIC
063100001219     C                   Z-ADD     3             VS(79)                         *ISTAT SU ADG/CONSEG
063200011030     C                   Z-ADD     3             VS(80)                         *CONS.ISOLA
063300011030     C                   Z-ADD     3             VS(81)                         *ESPRESSO SU C.ISOLA
063400011030     C                   Z-ADD     3             VS(82)                         *ADG X ESPRES/C.ISOL
063500011030     C                   Z-ADD     3             VS(83)                         *ISTAT X ESPRES/C.IS
063600011030     C                   Z-ADD     3             VS(84)                         *ISTAT X ADG/ESP/CIS
063700011030     C                   Z-ADD     3             VS(85)                         *ADG X CONS.ISOLA
063800011030     C                   Z-ADD     3             VS(86)                         *ISTAT X CONS.ISOLA
063900011030     C                   Z-ADD     3             VS(87)                         *ISTAT X ADG/C.ISOLA
064000011030     C                   Z-ADD     3             VS(88)                         *RITIRO ISOLA
064100011030     C                   Z-ADD     3             VS(89)                         *ESPRESSO SU R.ISOLA
064200011030     C                   Z-ADD     3             VS(90)                         *ADG X ESPRES/R.ISOL
064300011030     C                   Z-ADD     3             VS(91)                         *ISTAT X ESPRES/R.IS
064400011030     C                   Z-ADD     3             VS(92)                         *ISTAT X ADG/ESP/RIS
064500011030     C                   Z-ADD     3             VS(93)                         *ADG X RITIRO ISOLA
064600011030     C                   Z-ADD     3             VS(94)                         *ISTAT X RITIROISOLA
064700011030     C                   Z-ADD     3             VS(95)                         *ISTAT X ADG/R.ISOLA
064800040826     C                   Z-ADD     3             VS(96)                         *ISTAT X ADG/R.ISOLA
064900040826     C                   Z-ADD     3             VS(97)                         *ISTAT X ADG/R.ISOLA
065000040826     C                   Z-ADD     3             VS(98)                         *ISTAT X ADG/R.ISOLA
065100040826     C                   Z-ADD     3             VS(99)                         *ISTAT X ADG/R.ISOLA
065200040826     C                   Z-ADD     3             VS(100)                        *ISTAT X ADG/R.ISOLA
065300040826     C                   Z-ADD     3             VS(101)                        *ISTAT X ADG/R.ISOLA
065400040826     C                   Z-ADD     3             VS(102)                        *ISTAT X ADG/R.ISOLA
065500040826     C                   Z-ADD     3             VS(103)                        *ISTAT X ADG/R.ISOLA
065600040826     C                   Z-ADD     3             VS(104)                        *ISTAT X ADG/R.ISOLA
065700011030     C*
065800970306     C                   ENDSR
065900970306     C*------------------------------------------------------------------------*
066000970306     C* RAGVOC - DETERMINA IL RAGGRUPPAMENTO DELTA DELLE VOCI
066100970306     C*------------------------------------------------------------------------*
066200970306     C     RAGVOC        BEGSR
066300970717     C* PORTO
066400970717     C                   Z-ADD     001           VR(1)                          *PORTO
066500011031     C* RITIRO ISOLA (PORTO)
066600011030     C                   Z-ADD     001           VR(88)                         *RITIRO ISOLA
066700970717     C* EXTRA-PORTO
066800980702     C                   Z-ADD     002           VR(2)                          *DIRITTO FISSO
066900970717     C                   Z-ADD     002           VR(3)                          *ADG X PORTO
067000970717     C                   Z-ADD     002           VR(4)                          *ISTAT X PORTO
067100970717     C                   Z-ADD     002           VR(5)                          *TCP
067200970717     C                   Z-ADD     002           VR(6)                          *INOLTRO
067300970717     C                   Z-ADD     002           VR(17)                         *RITIRO
067400970717     C                   Z-ADD     002           VR(21)                         *ADG X INOLTRO
067500970717     C                   Z-ADD     002           VR(23)                         *ADG X ESPRES/PORTO
067600980702     C                   Z-ADD     002           VR(22)                         *ADG X DIR.FISSO
067700970717     C                   Z-ADD     002           VR(24)                         *ISTAT X INOLTRO
067800980702     C                   Z-ADD     002           VR(25)                         *ISTAT X DIR.FISSO
067900970717     C                   Z-ADD     002           VR(26)                         *ISTAT X ESPRES/PORT
068000970717     C                   Z-ADD     002           VR(27)                         *ISTAT X ADG/PORTO
068100970717     C                   Z-ADD     002           VR(28)                         *ISTAT X ADG/D.FISSO
068200970717     C                   Z-ADD     002           VR(29)                         *ISTAT X ADG/INOLTRO
068300970717     C                   Z-ADD     002           VR(30)                         *ISTAT X ADG/ESP/POR
068400970721     C                   Z-ADD     002           VR(34)                         *ISTAT X ADG/ESP/INO
068500970717     C                   Z-ADD     002           VR(37)                         *ADG X ESPRES/INOLT
068600970717     C                   Z-ADD     002           VR(38)                         *ISTAT X ESPRES/INOL
068700970721     C                   Z-ADD     002           VR(40)                         *ADG X ESPRES/L.DISA
068800970721     C                   Z-ADD     002           VR(41)                         *ISTAT X ESPRES/L.DI
068900970721     C                   Z-ADD     002           VR(42)                         *ISTAT X ADG/ESP/LDI
069000971023     C                   Z-ADD     002           VR(43)                         *ADG X L.DISAGIATE
069100971023     C                   Z-ADD     002           VR(44)                         *ISTAT X L.DISAGIATE
069200971023     C                   Z-ADD     002           VR(45)                         *ISTAT X ADG/L.DISAG
069300001219     C                   Z-ADD     002           VR(46)                         *ADG SU TCP
069400001219     C                   Z-ADD     002           VR(47)                         *ADG SU GIACENZA VAR
069500001219     C                   Z-ADD     002           VR(48)                         *ADG SU ASSIC X CONT
069600001219     C                   Z-ADD     002           VR(49)                         *ADG SU ASSIC RCV  F
069700001219     C                   Z-ADD     002           VR(50)                         *ADG SU CONSEGNA APP
069800001219     C                   Z-ADD     002           VR(51)                         *ADG SU CONSEGNA SUP
069900001219     C                   Z-ADD     002           VR(52)                         *ADG SU CONSEGNA PIA
070000001219     C                   Z-ADD     002           VR(53)                         *ADG SU CONSEGNA FUO
070100001219     C                   Z-ADD     002           VR(54)                         *ADG SU C/ASSEGNO MI
070200001219     C                   Z-ADD     002           VR(55)                         *ADG SU C/ASSEGNO BA
070300001219     C                   Z-ADD     002           VR(56)                         *ADG SU RITIRO
070400001219     C                   Z-ADD     002           VR(57)                         *ADG SU GIACENZA CON
070500001219     C                   Z-ADD     002           VR(58)                         *ADG SU GIACENZA CON
070600001219     C                   Z-ADD     002           VR(59)                         *ADG SU GIACENZA SOS
070700001219     C                   Z-ADD     002           VR(60)                         *ADG SU ASSIC X CONT
070800001219     C                   Z-ADD     002           VR(61)                         *ADG SU ASSIC RCV AS
070900001219     C                   Z-ADD     002           VR(62)                         *ADG SU CONSEGNA DDT
071000001219     C                   Z-ADD     002           VR(63)                         *ISTAT SU ADG/TCP
071100001219     C                   Z-ADD     002           VR(64)                         *ISTAT SU ADG/GIAC.V
071200001219     C                   Z-ADD     002           VR(65)                         *ISTAT SU ADG/ASSXCO
071300001219     C                   Z-ADD     002           VR(66)                         *ISTAT SU ADG/ASS RC
071400001219     C                   Z-ADD     002           VR(67)                         *ISTAT SU ADG/CONSEG
071500001219     C                   Z-ADD     002           VR(68)                         *ISTAT SU ADG/CONSEG
071600001219     C                   Z-ADD     002           VR(69)                         *ISTAT SU ADG/CONSEG
071700001219     C                   Z-ADD     002           VR(70)                         *ISTAT SU ADG/CONSEG
071800001219     C                   Z-ADD     002           VR(71)                         *ISTAT SU ADG/CASSEG
071900001219     C                   Z-ADD     002           VR(72)                         *ISTAT SU ADG/CASSEG
072000001219     C                   Z-ADD     002           VR(73)                         *ISTAT SU ADG/RITIRO
072100001219     C                   Z-ADD     002           VR(74)                         *ISTAT SU ADG/GIACEN
072200001219     C                   Z-ADD     002           VR(75)                         *ISTAT SU ADG/GIACEN
072300001219     C                   Z-ADD     002           VR(76)                         *ISTAT SU ADG/GIACEN
072400001219     C                   Z-ADD     002           VR(77)                         *ISTAT SU ADG/ASSIC
072500001219     C                   Z-ADD     002           VR(78)                         *ISTAT SU ADG/ASSIC
072600001219     C                   Z-ADD     002           VR(79)                         *ISTAT SU ADG/CONSEG
072700011030     C                   Z-ADD     002           VR(82)                         *ADG X ESPR/C.ISOLA
072800011030     C                   Z-ADD     002           VR(83)                         *ISTAT X ESPRES/C.IS
072900011030     C                   Z-ADD     002           VR(84)                         *ISTAT X ADG/ESP/CIS
073000011030     C                   Z-ADD     002           VR(85)                         *ADG X CONS.ISOLA
073100011030     C                   Z-ADD     002           VR(86)                         *ISTAT X CONS.ISOLA
073200011030     C                   Z-ADD     002           VR(87)                         *ISTAT X ADG/C.ISOLA
073300011031     C                   Z-ADD     002           VR(90)                         *ADG X ESPR/R.ISOLA
073400011031     C                   Z-ADD     002           VR(91)                         *ISTAT X ESPRES/R.IS
073500011031     C                   Z-ADD     002           VR(92)                         *ISTAT X ADG/ESP/RIS
073600011031     C                   Z-ADD     002           VR(93)                         *ADG X RITIRO ISOLA
073700011031     C                   Z-ADD     002           VR(94)                         *ISTAT X RITIROISOLA
073800011031     C                   Z-ADD     002           VR(95)                         *ISTAT X ADG/R.ISOLA
073900040826     C                   Z-ADD     002           VR(96)                         *CONS. LOC.DISAGIATE
074000040826     C                   Z-ADD     002           VR(97)                         *ISTAT X ADG/R.ISOLA
074100040826     C                   Z-ADD     002           VR(98)                         *ISTAT X ADG/R.ISOLA
074200040826     C                   Z-ADD     002           VR(99)                         *CONS. LOC.DISAGIATE
074300040826     C                   Z-ADD     002           VR(100)                        *ISTAT X ADG/R.ISOLA
074400040826     C                   Z-ADD     002           VR(101)                        *ISTAT X ADG/R.ISOLA
074500040826     C                   Z-ADD     002           VR(102)                        *ISTAT X ADG/R.ISOLA
074600040826     C                   Z-ADD     002           VR(103)                        *ISTAT X ADG/R.ISOLA
074700040826     C                   Z-ADD     002           VR(104)                        *ISTAT X ADG/R.ISOLA
074800970717     C* CONSEGNA AI PIANI
074900970717     C                   Z-ADD     003           VR(13)                         *CONS. AI PIANI
075000970717     C* CONSEGNA AI SUPERMERCATI
075100970717     C                   Z-ADD     004           VR(12)                         *CONS. SUPERMERCATI
075200970717     C* CONSEGNA SU APPUNTAMENTO
075300970717     C                   Z-ADD     005           VR(11)                         *CONS. APPUNTAMENTO
075400970717     C* CONSEGNA FUORI MISURA
075500970717     C                   Z-ADD     006           VR(14)                         *CONS. FUORI MISURA
075600970717     C* C/ASSEGNO
075700970717     C                   Z-ADD     007           VR(15)                         *C/ASSEGNO MITTENTE
075800970717     C                   Z-ADD     007           VR(16)                         *C/ASSEGNO BARTOLINI
075900970717     C* GIACENZA
076000970306     C                   Z-ADD     008           VR(8)                          *GIACENZA
076100970717     C                   Z-ADD     008           VR(18)                         *GIACENZA
076200970717     C                   Z-ADD     008           VR(19)                         *GIACENZA
076300970717     C                   Z-ADD     008           VR(20)                         *GIACENZA
076400970717     C* ASSICURAZIONE
076500970717     C                   Z-ADD     009           VR(9)                          *ASS.CONTO FRANCO
076600970717     C                   Z-ADD     009           VR(10)                         *ASS.RCV   FRANCO
076700970717     C                   Z-ADD     009           VR(31)                         *ASS.CONTO ASSEGNATO
076800970717     C                   Z-ADD     009           VR(32)                         *ASS.RCV ASSEGNATO
076900970717     C* ESPRESSO
077000970717     C                   Z-ADD     010           VR(7)                          *ESPRESSO PORTO
077100970717     C                   Z-ADD     010           VR(36)                         *ESPRESSO INOLTRO
077200970721     C                   Z-ADD     010           VR(39)                         *ESPRESSO L.DISAGIAT
077300011030     C                   Z-ADD     010           VR(81)                         *ESPRESSO CONS.ISOLA
077400011031     C                   Z-ADD     010           VR(89)                         *ESPRESSO RIT. ISOLA
077500970717     C* CONSEGNA DDT
077600970414     C                   Z-ADD     011           VR(33)                         *CONS. DDT
077700970717     C* CONSEGNA LOCALITA' DISAGIATA
077800970414     C                   Z-ADD     013           VR(35)                         *CONS. LOC.DISAGIATE
077900011030     C* CONSEGNA ISOLA
078000011030     C                   Z-ADD     016           VR(80)                         *CONS. ISOLA
078100970306     C*
078200970306     C                   ENDSR
078300970306     C*------------------------------------------------------------------------*
078400970306     C* CHKVOC - DETERMINA LE VOCI DA CALCOLARE
078500970306     C*------------------------------------------------------------------------*
078600970306     C     CHKVOC        BEGSR
078700970306     C*
078800970306     C* IMPOSTA A NO I FLAG "DA CALCOLARE" DI TUTTE LE VOCI
078900970306     C                   MOVEL     'N'           VC                             *VOCE DA CALCOLARE
079000970306     C                   Z-ADD     100           VP                             *% DA CONSIDERARE
079100970318     C                   MOVEL     'N'           VI                             *INGLOBA S/N
079200970306     C*
079300970306     C* IMPOSTA A SI I FLAG "DA CALCOLARE" DELLE VOCI SEMPRE PRESENTI
079400970306     C                   MOVEL     'S'           VC(1)                          *PORTO
079500970306     C                   MOVEL     'S'           VC(2)                          *DIRITTO FISSO
079600970306     C                   MOVEL     'S'           VC(3)                          *A.D.G X PORTO
079700970306     C                   MOVEL     'S'           VC(22)                         *A.D.G X DIR.FISSO
079800970306     C                   MOVEL     'S'           VC(4)                          *ISTAT X PORTO
079900970306     C                   MOVEL     'S'           VC(25)                         *ISTAT X DIR.FISSO
080000970306     C                   MOVEL     'S'           VC(27)                         *ISTAT X ADG/PORTO
080100970306     C                   MOVEL     'S'           VC(28)                         *ISTAT X ADG/D.FISSO
080200970306     C                   MOVEL     'S'           VC(5)                          *T.C.P.
080300001222     C                   MOVEL     'S'           VC(46)                         *ADG SU TCP
080400001222     C                   MOVEL     'S'           VC(63)                         *ISTAT SU ADG/TCP
080500970306     C*
080600970306     C* CONTROLLA SE ESISTE =======> INOLTRO
080700970306IF  1C     ETCPPV        IFGT      *ZEROS                                       *% SPED.IN PROVINCIA
080800970306     C                   MOVEL     'S'           VC(6)                          *INOLTRO
080900970306     C                   Z-ADD     ETCPPV        VP(6)                          *% DA CONSIDERARE
081000970314     C                   MOVEL     ETCSIN        VI(6)                          *INGLOBA S/N
081100970306     C                   MOVEL     'S'           VC(21)                         *A.D.G X INOLTRO
081200970306     C                   Z-ADD     ETCPPV        VP(21)                         *% DA CONSIDERARE
081300970306     C                   MOVEL     'S'           VC(24)                         *ISTAT X INOLTRO
081400970306     C                   Z-ADD     ETCPPV        VP(24)                         *% DA CONSIDERARE
081500971023     C                   MOVEL     'S'           VC(29)                         *ISTAT X ADG/INOLTRO
081600971023     C                   Z-ADD     ETCPPV        VP(29)                         *% DA CONSIDERARE
081700970306E   1C                   ENDIF
081800970306     C*
081900970717     C* CONTROLLA SE ESISTE =======> ESPRESSO PORTO
082000970306IF  1C     ETCESP        IFGT      *ZEROS
082100970717     C                   MOVEL     'S'           VC(7)                          *ESPRESSO PORTO
082200970306     C                   Z-ADD     ETCESP        VP(7)                          *% DA CONSIDERARE
082300970306     C                   MOVEL     ETCIES        VI(7)                          *INGLOBA S/N
082400970717     C                   MOVEL     'S'           VC(23)                         *ADG X ESP/PORTO
082500970306     C                   Z-ADD     ETCESP        VP(23)                         *% DA CONSIDERARE
082600970717     C                   MOVEL     'S'           VC(26)                         *ISTAT X ESP/PORTO
082700970306     C                   Z-ADD     ETCESP        VP(26)                         *% DA CONSIDERARE
082800970717     C                   MOVEL     'S'           VC(30)                         *ISTAT X ADG/ESP/POR
082900970717     C                   Z-ADD     ETCESP        VP(30)                         *% DA CONSIDERARE
083000970306E   1C                   ENDIF
083100970717     C*
083200970717     C* CONTROLLA SE ESISTE =======> ESPRESSO INOLTRO
083300970717IF  1C     ETCESP        IFGT      *ZEROS
083400970717IF  2C     ETCPPV        IFGT      *ZEROS
083500980928     C     ETCPPV        MULT      ETCESP        WN95             10 5
083600970717     C     WN95          DIV       100           WN95
083700970717IF  3C     WN95          IFGT      *ZEROS
083800970717     C                   MOVEL     'S'           VC(36)                         *ESPRESSO INOLTRO
083900970717     C                   Z-ADD     WN95          VP(36)                         *% DA CONSIDERARE
084000970717     C                   MOVEL     ETCIES        VI(36)                         *INGLOBA S/N
084100970717     C                   MOVEL     'S'           VC(37)                         *ADG X ESP/INOLTRO
084200970717     C                   Z-ADD     WN95          VP(37)                         *% DA CONSIDERARE
084300970717     C                   MOVEL     'S'           VC(38)                         *ISTAT X ESP/INOLTRO
084400970717     C                   Z-ADD     WN95          VP(38)                         *% DA CONSIDERARE
084500970721     C                   MOVEL     'S'           VC(34)                         *ISTAT X ADG/ESP/INO
084600970721     C                   Z-ADD     WN95          VP(34)                         *% DA CONSIDERARE
084700970717E   3C                   ENDIF
084800970717E   2C                   ENDIF
084900970717E   1C                   ENDIF
085000970721     C*
085100970721     C* CONTROLLA SE ESISTE =======> ESPRESSO LOCALITA' DISAGIATE
085200970721IF  1C     ETCESP        IFGT      *ZEROS
085300970721IF  2C     ETCPCL        IFGT      *ZEROS
085400980928     C     ETCPCL        MULT      ETCESP        WN95
085500970721     C     WN95          DIV       100           WN95
085600970721IF  3C     WN95          IFGT      *ZEROS
085700970721     C                   MOVEL     'S'           VC(39)                         *ESPRESSO L.DISAG.
085800970721     C                   Z-ADD     WN95          VP(39)                         *% DA CONSIDERARE
085900970721     C                   MOVEL     ETCIES        VI(39)                         *INGLOBA S/N
086000970721     C                   MOVEL     'S'           VC(40)                         *ADG X ESP/L.DISAG.
086100970721     C                   Z-ADD     WN95          VP(40)                         *% DA CONSIDERARE
086200970721     C                   MOVEL     'S'           VC(41)                         *ISTAT X ESP/L.DISAG
086300970721     C                   Z-ADD     WN95          VP(41)                         *% DA CONSIDERARE
086400970721     C                   MOVEL     'S'           VC(42)                         *ISTAT X ADG/ESP/LDI
086500970721     C                   Z-ADD     WN95          VP(42)                         *% DA CONSIDERARE
086600970721E   3C                   ENDIF
086700970721E   2C                   ENDIF
086800970721E   1C                   ENDIF
086900970306     C*
087000970306     C* CONTROLLA SE ESISTE =======> GIACENZA VARIE
087100970306IF  1C     ETCPGI        IFGT      *ZEROS
087200970306     C                   MOVEL     'S'           VC(8)                          *GIACENZA -FISSE-
087300970306     C                   Z-ADD     ETCPGI        VP(8)                          *% DA CONSIDERARE
087400970306     C                   MOVEL     ETCIGI        VI(8)                          *INGLOBA S/N
087500001222     C                   MOVEL     'S'           VC(47)                         *ADG SU GIACENZA VAR
087600001222     C                   Z-ADD     ETCPGI        VP(47)                         *% DA CONSIDERARE
087700001227     C                   MOVEL     'S'           VC(64)                         *ISTAT SU ADG/GIAC.V
087800001227     C                   Z-ADD     ETCPGI        VP(64)                         *% DA CONSIDERARE
087900970306     C                   MOVEL     'S'           VC(20)                         *GIACENZA -VARIABILI
088000970306     C                   Z-ADD     ETCPGI        VP(20)                         *% DA CONSIDERARE
088100970306     C                   MOVEL     ETCIGI        VI(20)                         *INGLOBA S/N
088200001227     C                   MOVEL     'S'           VC(59)                         *ADG SU GIACENZA SOS
088300001227     C                   Z-ADD     ETCPGI        VP(59)                         *% DA CONSIDERARE
088400001227     C                   MOVEL     'S'           VC(76)                         *ISTAT SU ADG/GIACEN
088500001227     C                   Z-ADD     ETCPGI        VP(76)                         *% DA CONSIDERARE
088600970306E   1C                   ENDIF
088700970306     C*
088800970306     C* CONTROLLA SE ESISTE =======> ASSICURAZIONE PER CONTO FRANCO
088900970306IF  1C     ETCPAC        IFGT      *ZEROS
089000970306IF  2C     ETCFRA        IFGT      *ZEROS
089100980928     C     ETCFRA        MULT      ETCPAC        WN95
089200970306     C     WN95          DIV       100           WN95
089300970306IF  3C     WN95          IFGT      *ZEROS
089400970306     C                   MOVEL     'S'           VC(9)                          *ASS.CONTO FRANCO
089500970306     C                   Z-ADD     WN95          VP(9)                          *% DA CONSIDERARE
089600970306     C                   MOVEL     ETCIAS        VI(9)                          *INGLOBA S/N
089700001227     C                   MOVEL     'S'           VC(48)                         *ADG SU ASSIC X CONT
089800001227     C                   Z-ADD     WN95          VP(48)                         *% DA CONSIDERARE
089900001227     C                   MOVEL     'S'           VC(65)                         *ISTAT SU ADG/ASSXCO
090000001227     C                   Z-ADD     WN95          VP(65)                         *% DA CONSIDERARE
090100970306E   3C                   ENDIF
090200970306E   2C                   ENDIF
090300970306E   1C                   ENDIF
090400970306     C*
090500970306     C* CONTROLLA SE ESISTE =======> ASSICURAZIONE PER CONTO ASSEGNATO
090600970306IF  1C     ETCPAC        IFGT      *ZEROS
090700970306IF  2C     ETCASS        IFGT      *ZEROS
090800980928     C     ETCASS        MULT      ETCPAC        WN95
090900970306     C     WN95          DIV       100           WN95
091000970306IF  3C     WN95          IFGT      *ZEROS
091100970306     C                   MOVEL     'S'           VC(31)                         *ASS.CONTO ASSEGNATO
091200970306     C                   Z-ADD     WN95          VP(31)                         *% DA CONSIDERARE
091300970306     C                   MOVEL     ETCIAS        VI(31)                         *INGLOBA S/N
091400001227     C                   MOVEL     'S'           VC(60)                         *ADG SU ASSIC X CONT
091500001227     C                   Z-ADD     WN95          VP(60)                         *% DA CONSIDERARE
091600001227     C                   MOVEL     'S'           VC(77)                         *ISTAT SU ADG/ASSIC
091700001227     C                   Z-ADD     WN95          VP(77)                         *% DA CONSIDERARE
091800970306E   3C                   ENDIF
091900970306E   2C                   ENDIF
092000970306E   1C                   ENDIF
092100970306     C*
092200970306     C* CONTROLLA SE ESISTE =======> ASSICURAZIONE RCV FRANCO
092300990621IF  1C     ETCPAR        IFGT      *ZEROS
092400990621IF  2C     ETCFRA        IFGT      *ZEROS
092500990621     C     ETCFRA        MULT      ETCPAR        WN95
092600990621     C     WN95          DIV       100           WN95
092700990621IF  3C     WN95          IFGT      *ZEROS
092800990621     C                   MOVEL     'S'           VC(10)                         *ASS.NE RCV FRANCO
092900990621     C                   Z-ADD     WN95          VP(10)                         *% DA CONSIDERARE
093000990621     C                   MOVEL     ETCIAS        VI(10)                         *INGLOBA S/N
093100001227     C                   MOVEL     'S'           VC(49)                         *ADG SU ASSIC RCV  F
093200001227     C                   Z-ADD     WN95          VP(49)                         *% DA CONSIDERARE
093300001227     C                   MOVEL     'S'           VC(66)                         *ISTAT SU ADG/ASS RC
093400001227     C                   Z-ADD     WN95          VP(66)                         *% DA CONSIDERARE
093500990621E   3C                   ENDIF
093600990621E   2C                   ENDIF
093700990621E   1C                   ENDIF
093800970306     C*
093900970306     C* CONTROLLA SE ESISTE =======> ASSICURAZIONE RCV ASSEGNATO
094000990621IF  1C     ETCPAR        IFGT      *ZEROS
094100990621IF  2C     ETCASS        IFGT      *ZEROS
094200990621     C     ETCASS        MULT      ETCPAR        WN95
094300990621     C     WN95          DIV       100           WN95
094400990621IF  3C     WN95          IFGT      *ZEROS
094500990621     C                   MOVEL     'S'           VC(32)                         *ASS.NE RCV ASSEGNAT
094600990621     C                   Z-ADD     WN95          VP(32)                         *% DA CONSIDERARE
094700990621     C                   MOVEL     ETCIAS        VI(32)                         *INGLOBA S/N
094800001227     C                   MOVEL     'S'           VC(61)                         *ADG SU ASSIC RCV  A
094900001227     C                   Z-ADD     WN95          VP(61)                         *% DA CONSIDERARE
095000001227     C                   MOVEL     'S'           VC(78)                         *ISTAT SU ADG/ASS RC
095100001227     C                   Z-ADD     WN95          VP(78)                         *% DA CONSIDERARE
095200990621E   3C                   ENDIF
095300990621E   2C                   ENDIF
095400990621E   1C                   ENDIF
095500970306     C*
095600970306     C* CONTROLLA SE ESISTE =======> CONSEGNA SU APPUNTAMENTO
095700970306IF  1C     ETCPCA        IFGT      *ZEROS
095800970306     C                   MOVEL     'S'           VC(11)                         *CONS. APPUNTAMENTO
095900970306     C                   Z-ADD     ETCPCA        VP(11)                         *% DA CONSIDERARE
096000970306     C                   MOVEL     ETCIAP        VI(11)                         *INGLOBA S/N
096100001227     C                   MOVEL     'S'           VC(50)                         *ADG SU CONSEGNA APP
096200001227     C                   Z-ADD     ETCPCA        VP(50)                         *% DA CONSIDERARE
096300001227     C                   MOVEL     'S'           VC(67)                         *ISTAT SU ADG/CONSEG
096400001227     C                   Z-ADD     ETCPCA        VP(67)                         *% DA CONSIDERARE
096500970306E   1C                   ENDIF
096600970306     C*
096700970306     C* CONTROLLA SE ESISTE =======> CONSEGNA AI SUPERMERCATI
096800970306IF  1C     ETCPCS        IFGT      *ZEROS
096900970306     C                   MOVEL     'S'           VC(12)                         *CONS. SUPERMERCATI
097000970306     C                   Z-ADD     ETCPCS        VP(12)                         *% DA CONSIDERARE
097100970306     C                   MOVEL     ETCISU        VI(12)                         *INGLOBA S/N
097200001227     C                   MOVEL     'S'           VC(51)                         *ADG SU CONSEGNA SUP
097300001227     C                   Z-ADD     ETCPCS        VP(51)                         *% DA CONSIDERARE
097400001227     C                   MOVEL     'S'           VC(68)                         *ISTAT SU ADG/CONSEG
097500001227     C                   Z-ADD     ETCPCS        VP(68)                         *% DA CONSIDERARE
097600970306E   1C                   ENDIF
097700970306     C*
097800970306     C* CONTROLLA SE ESISTE =======> CONSEGNA AI PIANI
097900970306IF  1C     ETCPCP        IFGT      *ZEROS
098000970306     C                   MOVEL     'S'           VC(13)                         *CONS. AI PIANI
098100970306     C                   Z-ADD     ETCPCP        VP(13)                         *% DA CONSIDERARE
098200970306     C                   MOVEL     ETCIPI        VI(13)                         *INGLOBA S/N
098300001227     C                   MOVEL     'S'           VC(52)                         *ADG SU CONSEGNA SUP
098400001227     C                   Z-ADD     ETCPCP        VP(52)                         *% DA CONSIDERARE
098500001227     C                   MOVEL     'S'           VC(69)                         *ISTAT SU ADG/CONSEG
098600001227     C                   Z-ADD     ETCPCP        VP(69)                         *% DA CONSIDERARE
098700970306E   1C                   ENDIF
098800040826     C* CONTROLLA SE ESISTE =======> CONSEGNA destinatario disagiato
098900040826IF  1C     ETCdes        IFGT      *ZEROS
099000040826     C                   MOVEL     'S'           VC(96)                         *CONS. AI PIANI
099100040826     C                   Z-ADD     ETCdes        VP(96)                         *% DA CONSIDERARE
099200040826     C                   MOVEL     'S'           VC(97)                         *ADG SU CONSEGNA SUP
099300040826     C                   Z-ADD     ETCdes        VP(97)                         *% DA CONSIDERARE
099400040826     C                   MOVEL     'S'           VC(98)                         *ISTAT SU ADG/CONSEG
099500040826     C                   Z-ADD     ETCdes        VP(98)                         *% DA CONSIDERARE
099600040826E   1C                   ENDIF
099700970306     C*
099800040826     C* CONTROLLA SE ESISTE =======> bancali BAN
099900040826      * Non si tassano i bancali per le bolle in porto assegnato(vedi TNSF20R)
100000040826IF  1C     ETCnba        IFGT      *ZEROS
100100040826IF  1C     ETCfra        andeq     100
100200040826     C                   MOVEL     'S'           VC(99)                         *CONS. AI PIANI
100300040826     C                   Z-ADD     ETCban        VP(99)                         *% DA CONSIDERARE
100400040826     C                   MOVEL     'S'           VC(100)                        *ADG SU CONSEGNA SUP
100500040826     C                   Z-ADD     ETCban        VP(100)                        *% DA CONSIDERARE
100600040826     C                   MOVEL     'S'           VC(101)                        *ISTAT SU ADG/CONSEG
100700040826     C                   Z-ADD     ETCban        VP(101)                        *% DA CONSIDERARE
100800040826E   1C                   ENDIF
100900040826     C*
101000970306     C* CONTROLLA SE ESISTE =======> CONSEGNA FUORI MISURA
101100970306IF  1C     ETCPCF        IFGT      *ZEROS
101200970306     C                   MOVEL     'S'           VC(14)                         *CONS. FUORI MISURA
101300970306     C                   Z-ADD     ETCPCF        VP(14)                         *% DA CONSIDERARE
101400970306     C                   MOVEL     ETCIFU        VI(14)                         *INGLOBA S/N
101500001227     C                   MOVEL     'S'           VC(53)                         *ADG SU CONSEGNA FUO
101600001227     C                   Z-ADD     ETCPCF        VP(53)                         *% DA CONSIDERARE
101700001227     C                   MOVEL     'S'           VC(70)                         *ISTAT SU ADG/CONSEG
101800001227     C                   Z-ADD     ETCPCF        VP(70)                         *% DA CONSIDERARE
101900970306E   1C                   ENDIF
102000970414     C*
102100970414     C* CONTROLLA SE ESISTE =======> CONSEGNA DDT
102200970414IF  1C     ETCPCD        IFGT      *ZEROS
102300970414     C                   MOVEL     'S'           VC(33)                         *CONS. DDT
102400970414     C                   Z-ADD     ETCPCD        VP(33)                         *% DA CONSIDERARE
102500970414     C                   MOVEL     ETCIDD        VI(33)                         *INGLOBA S/N
102600001227     C                   MOVEL     'S'           VC(62)                         *ADG SU CONSEGNA DDT
102700001227     C                   Z-ADD     ETCPCD        VP(62)                         *% DA CONSIDERARE
102800001227     C                   MOVEL     'S'           VC(79)                         *ISTAT SU ADG/CONSEG
102900001227     C                   Z-ADD     ETCPCD        VP(79)                         *% DA CONSIDERARE
103000970414E   1C                   ENDIF
103100970414     C*
103200970414     C* CONTROLLA SE ESISTE =======> CONSEGNA LOCALITA' DISAGIATE
103300970414IF  1C     ETCPCL        IFGT      *ZEROS
103400970414     C                   MOVEL     'S'           VC(35)                         *CONS. LOC.DISAGIATE
103500970414     C                   Z-ADD     ETCPCL        VP(35)                         *% DA CONSIDERARE
103600970414     C                   MOVEL     ETCILO        VI(35)                         *INGLOBA S/N
103700971023     C                   MOVEL     'S'           VC(43)                         *A.D.G X L.DISAG.
103800971023     C                   Z-ADD     ETCPCL        VP(43)                         *% DA CONSIDERARE
103900971023     C                   MOVEL     'S'           VC(44)                         *ISTAT X L.DISAG.
104000971023     C                   Z-ADD     ETCPCL        VP(44)                         *% DA CONSIDERARE
104100971023     C                   MOVEL     'S'           VC(45)                         *ISTAT X ADG/L.DISAG
104200971023     C                   Z-ADD     ETCPCL        VP(45)                         *% DA CONSIDERARE
104300970414E   1C                   ENDIF
104400970306     C*
104500970306     C* CONTROLLA SE ESISTE =======> CONTRASSEGNO MITTENTE
104600970306IF  1C     ETCPAM        IFGT      *ZEROS
104700970306     C                   MOVEL     'S'           VC(15)                         *C/ASSEGNO MITTENTE
104800970306     C                   Z-ADD     ETCPAM        VP(15)                         *% DA CONSIDERARE
104900970306     C                   MOVEL     ETCICA        VI(15)                         *INGLOBA S/N
105000001227     C                   MOVEL     'S'           VC(54)                         *ADG SU C/ASSEGNO MI
105100001227     C                   Z-ADD     ETCPAM        VP(54)                         *% DA CONSIDERARE
105200001227     C                   MOVEL     'S'           VC(71)                         *ISTAT SU ADG/CASSEG
105300001227     C                   Z-ADD     ETCPAM        VP(71)                         *% DA CONSIDERARE
105400970306E   1C                   ENDIF
105500970306     C*
105600970306     C* CONTROLLA SE ESISTE =======> CONTRASSEGNO BARTOLINI
105700970306IF  1C     ETCPAB        IFGT      *ZEROS
105800970306     C                   MOVEL     'S'           VC(16)                         *C/ASSEGNO BARTOLINI
105900970306     C                   Z-ADD     ETCPAB        VP(16)                         *% DA CONSIDERARE
106000970306     C                   MOVEL     ETCICA        VI(16)                         *INGLOBA S/N
106100001227     C                   MOVEL     'S'           VC(55)                         *ADG SU C/ASSEGNO BA
106200001227     C                   Z-ADD     ETCPAB        VP(55)                         *% DA CONSIDERARE
106300001227     C                   MOVEL     'S'           VC(72)                         *ISTAT SU ADG/CASSEG
106400001227     C                   Z-ADD     ETCPAB        VP(72)                         *% DA CONSIDERARE
106500970306E   1C                   ENDIF
106600970306     C*
106700970306     C* CONTROLLA SE ESISTE =======> RITIRO
106800970306IF  1C     T             IFGT      *ZEROS                                       *CICLO COD.TASSAZION
106900970306IF  2C     ETCASS        IFGT      *ZEROS
107000971204IF  3C     CFL1(T)       IFEQ      *BLANKS                                      *ARRIVO ITALIA
107100970306IF  4C     ETCPCM        IFNE      100
107200970306     C     100           SUB       ETCPCM        WN63              6 3
107300980928     C     WN63          MULT      ETCASS        WN95
107400970306     C     WN95          DIV       100           WN95
107500970306IF  5C     WN95          IFGT      *ZEROS
107600970306     C                   MOVEL     'S'           VC(17)                         *RITIRO
107700970306     C                   Z-ADD     WN95          VP(17)                         *% DA CONSIDERARE
107800001227     C                   MOVEL     'S'           VC(56)                         *ADG SU RITIRO
107900001227     C                   Z-ADD     WN95          VP(56)                         *% DA CONSIDERARE
108000001227     C                   MOVEL     'S'           VC(73)                         *ISTAT SU ADG/RITIRO
108100001227     C                   Z-ADD     WN95          VP(73)                         *% DA CONSIDERARE
108200970306E   5C                   ENDIF
108300970311E   4C                   ENDIF
108400970311X   3C                   ELSE                                                   *ARRIVO ESTERO
108500970306     C                   MOVEL     'S'           VC(17)                         *RITIRO
108600001227     C                   MOVEL     'S'           VC(56)                         *ADG SU RITIRO
108700001227     C                   MOVEL     'S'           VC(73)                         *ISTAT SU ADG/RITIRO
108800970306E   3C                   ENDIF
108900970306E   2C                   ENDIF
109000970306E   1C                   ENDIF
109100970306     C*
109200970306     C* CONTROLLA SE ESISTE =======> GIACENZA RICONSEGNA PROVINCIA (SOLO 50%)
109300970306IF  1C     ETCPGI        IFGT      *ZEROS
109400970306IF  2C     ETCPPV        IFGT      *ZEROS
109500980928     C     ETCPPV        MULT      ETCPGI        WN95
109600970306     C     WN95          DIV       100           WN95
109700970306     C     WN95          MULT      50            WN95
109800970306     C     WN95          DIV       100           WN95
109900970306IF  3C     WN95          IFGT      *ZEROS
110000970306     C                   MOVEL     'S'           VC(18)                         *GIACENZA
110100970306     C                   Z-ADD     WN95          VP(18)                         *% DA CONSIDERARE
110200970306     C                   MOVEL     ETCIGI        VI(18)                         *INGLOBA S/N
110300001227     C                   MOVEL     'S'           VC(57)                         *ADG SU GIACENZA CON
110400001227     C                   Z-ADD     WN95          VP(57)                         *% DA CONSIDERARE
110500001227     C                   MOVEL     'S'           VC(74)                         *ISTAT SU ADG/GIACEN
110600001227     C                   Z-ADD     WN95          VP(74)                         *% DA CONSIDERARE
110700970306E   3C                   ENDIF
110800970306E   2C                   ENDIF
110900970306E   1C                   ENDIF
111000970306     C*
111100970306     C* CONTROLLA SE ESISTE =======> GIACENZA RICONSEGNA CITTA' (SOLO 50%)
111200970306IF  1C     ETCPGI        IFGT      *ZEROS
111300970306IF  2C     ETCPPV        IFGT      *ZEROS
111400970306     C     100           SUB       ETCPPV        WN63              6 3
111500980928     C     WN63          MULT      ETCPGI        WN95
111600970306     C     WN95          DIV       100           WN95
111700970306     C     WN95          MULT      50            WN95
111800970306     C     WN95          DIV       100           WN95
111900970306IF  3C     WN95          IFGT      *ZEROS
112000970306     C                   MOVEL     'S'           VC(19)                         *GIACENZA
112100970306     C                   Z-ADD     WN95          VP(19)                         *% DA CONSIDERARE
112200970306     C                   MOVEL     ETCIGI        VI(19)                         *INGLOBA S/N
112300001227     C                   MOVEL     'S'           VC(58)                         *ADG SU GIACENZA CON
112400001227     C                   Z-ADD     WN95          VP(58)                         *% DA CONSIDERARE
112500001227     C                   MOVEL     'S'           VC(75)                         *ISTAT SU ADG/GIACEN
112600001227     C                   Z-ADD     WN95          VP(75)                         *% DA CONSIDERARE
112700970306E   3C                   ENDIF
112800970306E   2C                   ENDIF
112900970306E   1C                   ENDIF
113000011030     C*
113100011031     C* CONTROLLA SE ESISTE =======>
113200011031IF  1C     ETCCIS        IFGT      *ZEROS
113300011031IF  2C     T             IFGT      *ZEROS                                       *CICLO COD.TASSAZION
113400011031IF  3C     C§§5(T)       IFEQ      'S'                                          *CTS POTENZ.ISOLA
113500011031     C*                     =======> CONSEGNA ISOLA
113600011030     C                   MOVEL     'S'           VC(80)                         *CONSEGNA ISOLA
113700011030     C                   Z-ADD     ETCCIS        VP(80)                         *% DA CONSIDERARE
113800011030     C                   MOVEL     ETCISO        VI(80)                         *INGLOBA S/N
113900011030     C                   MOVEL     'S'           VC(85)                         *A.D.G X CONS.ISOLA
114000011030     C                   Z-ADD     ETCCIS        VP(85)                         *% DA CONSIDERARE
114100011030     C                   MOVEL     'S'           VC(86)                         *ISTAT X CONS.ISOLA
114200011030     C                   Z-ADD     ETCCIS        VP(86)                         *% DA CONSIDERARE
114300011030     C                   MOVEL     'S'           VC(87)                         *ISTAT X ADG/C.ISOLA
114400011030     C                   Z-ADD     ETCCIS        VP(87)                         *% DA CONSIDERARE
114500011031     C*                     =======> ESPRESSO CONSEGNA ISOLE
114600011031IF  4C     ETCESP        IFGT      *ZEROS
114700011031     C     ETCCIS        MULT      ETCESP        WN95
114800011031     C     WN95          DIV       100           WN95
114900011031IF  5C     WN95          IFGT      *ZEROS
115000011031     C                   MOVEL     'S'           VC(81)                         *ESPRESSO CONS.ISOLE
115100011031     C                   Z-ADD     WN95          VP(81)                         *% DA CONSIDERARE
115200011031     C                   MOVEL     ETCISO        VI(81)                         *INGLOBA S/N
115300011031     C                   MOVEL     'S'           VC(82)                         *ADG X ESP/C.ISOLE
115400011031     C                   Z-ADD     WN95          VP(82)                         *% DA CONSIDERARE
115500011031     C                   MOVEL     'S'           VC(83)                         *ISTAT X ESP/C.ISOLE
115600011031     C                   Z-ADD     WN95          VP(83)                         *% DA CONSIDERARE
115700011031     C                   MOVEL     'S'           VC(84)                         *ISTAT X ADG/ESP/CIS
115800011031     C                   Z-ADD     WN95          VP(84)                         *% DA CONSIDERARE
115900011031E   5C                   ENDIF
116000011031E   4C                   ENDIF
116100011031     C*
116200011030E   3C                   ENDIF
116300011030E   2C                   ENDIF
116400011030E   1C                   ENDIF
116500011030     C*
116600011031     C* CONTROLLA SE ESISTE =======>
116700011031IF  1C     ETCSAM        IFGT      *ZEROS
116800011031IF  2C     T             IFGT      *ZEROS                                       *CICLO COD.TASSAZION
116900011031IF  3C     D00PRI        IFEQ      CCTS(T)                                      *CTS DEL CLIENTE T/O
117000011031IF  4C     C§§5(T)       IFEQ      'S'                                          *CTS POTENZ.ISOLA
117100011031     C*                     =======> RITIRO ISOLA
117200011031     C                   MOVEL     'S'           VC(88)                         *RITIRO ISOLA
117300011031     C                   Z-ADD     ETCSAM        VP(88)                         *% DA CONSIDERARE
117400011031     C                   MOVEL     ETCINR        VI(88)                         *INGLOBA S/N
117500011031     C                   MOVEL     'S'           VC(93)                         *A.D.G X RIT.ISOLA
117600011031     C                   Z-ADD     ETCSAM        VP(93)                         *% DA CONSIDERARE
117700011031     C                   MOVEL     'S'           VC(94)                         *ISTAT X RIT.ISOLA
117800011031     C                   Z-ADD     ETCSAM        VP(94)                         *% DA CONSIDERARE
117900011031     C                   MOVEL     'S'           VC(95)                         *ISTAT X ADG/R.ISOLA
118000011031     C                   Z-ADD     ETCSAM        VP(95)                         *% DA CONSIDERARE
118100011031     C*                     =======> ESPRESSO RITIRO ISOLE
118200011031IF  5C     ETCESP        IFGT      *ZEROS
118300011031     C     ETCSAM        MULT      ETCESP        WN95
118400011031     C     WN95          DIV       100           WN95
118500011031IF  6C     WN95          IFGT      *ZEROS
118600011031     C                   MOVEL     'S'           VC(89)                         *ESPRESSO RIT.ISOLE
118700011031     C                   Z-ADD     WN95          VP(89)                         *% DA CONSIDERARE
118800011031     C                   MOVEL     ETCINR        VI(89)                         *INGLOBA S/N
118900011031     C                   MOVEL     'S'           VC(90)                         *ADG X ESP/RIT.ISOLE
119000011031     C                   Z-ADD     WN95          VP(90)                         *% DA CONSIDERARE
119100011031     C                   MOVEL     'S'           VC(91)                         *ISTAT X ESP/R.ISOLE
119200011031     C                   Z-ADD     WN95          VP(91)                         *% DA CONSIDERARE
119300011031     C                   MOVEL     'S'           VC(92)                         *ISTAT X ADG/ESP/RIS
119400011031     C                   Z-ADD     WN95          VP(92)                         *% DA CONSIDERARE
119500011031E   6C                   ENDIF
119600011031E   5C                   ENDIF
119700011031     C*
119800011031E   4C                   ENDIF
119900011031E   3C                   ENDIF
120000011031E   2C                   ENDIF
120100011030E   1C                   ENDIF
120200970306     C*
120300970306     C                   ENDSR
120400970306     C*------------------------------------------------------------------------*
120500970306     C* VALVO1 - VALORIZZA LE VOCI -FISSE-
120600970306     C*------------------------------------------------------------------------*
120700970306     C     VALVO1        BEGSR
120800970306     C*
120900970306     C* AZZERA GLI IMPORTI DELLE VOCI CON SEQUENZA UNO
121000970306DO  1C     1             DO        NVOCA         I
121100970306IF  2C     VS(I)         IFEQ      1
121200970306     C                   Z-ADD     *ZEROS        VIMP(I)                        *IMPORTO CALCOLATO
121300970306E   2C                   ENDIF
121400970306E   1C                   ENDDO
121500970306     C*
121600970306     C* CALCOLA LE SPESE DI GIACENZA VARIE
121700970306IF  1C     WERR          IFEQ      'N'
121800970306IF  2C     VC(8)         IFEQ      'S'                                          *VOCE DA CALCOLARE
121900970306     C     $TGC          ANDEQ     '1'                                          *TROVATA TAR.GIACENZ
122000970306     C                   EXSR      CALGCV
122100001227     C                   EXSR      CAL047
122200001227     C                   EXSR      CAL064
122300970306E   2C                   ENDIF
122400970306E   1C                   ENDIF
122500970306     C*
122600970306     C                   ENDSR
122700970306     C*------------------------------------------------------------------------*
122800970306     C* CARTGC - REPERISCE LA TARIFFA GIACENZA
122900970306     C*------------------------------------------------------------------------*
123000970306     C     CARTGC        BEGSR
123100970306     C*
123200970625     C* LEGGE LA TARIFFA GIACENZA DEL CLIENTE
123300970625     C                   Z-ADD     D00KSC        KGCKSC                         *CLIENTE
123400970625     C                   Z-ADD     D00CTR        KGCCTR                         *TARIFFA
123500970625     C                   Z-ADD     D00PRG        KGCPRG                         *PROGRESSIVO
123600011019     C                   MOVEL     D00DIV        WDIV
123700980703IF  1C     D00CTO        IFEQ      'O'                                          *IN OFFERTA
123800990819     C     KEYTGC        CHAIN     TIOGC01L                           99
123900980703X   1C                   ELSE                                                   *IN TARIFFA
124000990819     C     KEYTGC        CHAIN     TITGC01L                           99
124100970625E   1C                   ENDIF
124200970306     C*
124300970625     C* LEGGE LA TARIFFA DI CARTELLO SE NON TROVATA TARIFFA/OFFERTA CLIENTE
124400970625IF  1C     *IN99         IFEQ      *ON                                          *NON TROVATA
124500970625     C                   Z-ADD     CARKSC        KGCKSC
124600970625     C                   Z-ADD     CARCTR        KGCCTR
124700970625     C                   Z-ADD     CARPRG        KGCPRG
124800011023     C                   MOVEL     CARDIV        WDIV
124900011023     C     KEYTGC        CHAIN     TITGC01L                           99        *DA TARIFFA CARTELLO
125000970625E   1C                   ENDIF
125100970625     C* TARIFFA NON TROVATA
125200970625IF  1C     *IN99         IFEQ      *ON
125300970625     C                   MOVEL     MSG(23)       $MSG
125400970625     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
125500970625     C* TARIFFA TROVATA
125600970306X   1C                   ELSE
125700991125     C*
125800991125     C* CONTROLLA SE CONVERTIRE I CAMPI DI TITPD
125900011026IF  2C     WDIV          IFNE      WDIVCE
126000011026     C                   MOVEL     WDIV          WDVI              3
126100991125     C                   EXSR      CNVTGC
126200991125E   2C                   ENDIF
126300970625     C                   MOVEL     '1'           $TGC                           *TARIFFA TROVATA
126400970306E   1C                   ENDIF
126500970306     C*
126600970306     C                   ENDSR
126700970306     C*------------------------------------------------------------------------*
126800970306     C* CALGCV - CALCOLA LE SPESE DI GIACENZA VARIE
126900970306     C*------------------------------------------------------------------------*
127000970306     C     CALGCV        BEGSR
127100970306     C*
127200970306     C* REIMPOSTA LE VARIABILI DI LAVORO
127300011031     C                   Z-ADD     *ZEROS        TASSGV           21 8
127400011031     C                   Z-ADD     *ZEROS        TASSGD           21 8
127500970306     C*
127600970306     C* CALCOLA LE SPESE VARIE
127700970306IF  1C     TGCSGV        IFGT      *ZEROS                                       *IMMESSA TARIFFA
127800970306     C                   Z-ADD     TGCSGV        TASSGV
127900970306E   1C                   ENDIF
128000970306     C*
128100970306     C* CALCOLA LE SPESE DOSSIER
128200970306IF  1C     TGCSGD        IFGT      *ZEROS                                       *IMMESSA TARIFFA
128300970306     C     TGCSGD        MULT      ETCNRG        TASSGD
128400970306     C*
128500970306     C* AZZERA LE SPESE DOSSIER SE SI E' DENTRO AI GIORNI DI FRANCHIGIA
128600970306IF  2C     TGCFAF        IFEQ      '2'                                          *FRANCHIGIA
128700970306     C     ETCGGG        ANDLE     TGCSGF                                       *GG SOSTA < GG FRANC
128800970306     C                   Z-ADD     *ZEROS        TASSGD                         *NO SPESE DOSSIER
128900970306E   2C                   ENDIF
129000970306E   1C                   ENDIF
129100970306     C*
129200970314IF  1C     TASSGV        IFGT      *ZEROS
129300970314     C     TASSGD        ORGT      *ZEROS
129400970306     C                   Z-ADD     8             I
129500970306     C     TASSGV        ADD       TASSGD        VIMP(I)
129600970314E   1C                   ENDIF
129700970306     C*
129800970306     C                   ENDSR
129900001227     C*------------------------------------------------------------------------*
130000001227     C* CAL047 - CALCOLA ADG SU GIACENZA VARIE DOSSIER
130100001227     C*------------------------------------------------------------------------*
130200001227     C     CAL047        BEGSR
130300001227     C*
130400001227     C* REIMPOSTA LE VARIABILI DI LAVORO
130500011031     C                   Z-ADD     *ZEROS        TASADG           21 8
130600001227     C*
130700001227     C* CALCOLA L'A.D.G.
130800011031     C                   Z-ADD     VIMP(8)       WIMP             23 8
130900001227     C                   EXSR      ADGCAL
131000001227     C*
131100001227IF  1C     TASADG        IFGT      *ZEROS
131200001227     C                   Z-ADD     47            I
131300001227     C                   Z-ADD     TASADG        VIMP(I)
131400001227E   1C                   ENDIF
131500001227     C*
131600001227     C                   ENDSR
131700001227     C*------------------------------------------------------------------------*
131800001227     C* CAL064 - CALCOLA ISTAT SU ADG/GIAC.VARIE DOSSIER
131900001227     C*------------------------------------------------------------------------*
132000001227     C     CAL064        BEGSR
132100001227     C*
132200001227     C* REIMPOSTA LE VARIABILI DI LAVORO
132300011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
132400001227     C*
132500001227     C* CALCOLA L'ISTAT
132600011031     C                   Z-ADD     VIMP(47)      WIMP             23 8
132700001227     C                   EXSR      IACCAL
132800001227     C*
132900001227IF  1C     TASIAC        IFGT      *ZEROS
133000001227     C                   Z-ADD     64            I
133100001227     C                   Z-ADD     TASIAC        VIMP(I)
133200001227E   1C                   ENDIF
133300001227     C*
133400001227     C                   ENDSR
133500970306     C*------------------------------------------------------------------------*
133600970306     C* VALVO2 - VALORIZZA LE VOCI -DIPENDENTI DA SCAGLIONI-
133700970306     C*------------------------------------------------------------------------*
133800970306     C     VALVO2        BEGSR
133900970306     C*
134000970306     C* AZZERA GLI IMPORTI DELLE VOCI CON SEQUENZA DUE
134100970306DO  1C     1             DO        NVOCA         I
134200970306IF  2C     VS(I)         IFEQ      2
134300970306     C                   Z-ADD     *ZEROS        VIMP(I)                        *IMPORTO CALCOLATO
134400970306E   2C                   ENDIF
134500970306E   1C                   ENDDO
134600970306     C*
134700970306     C* CALCOLA LE SPESE DI GIACENZA DI RICONSEGNA PROVINCIA
134800970306IF  1C     WERR          IFEQ      'N'
134900970306IF  2C     VC(18)        IFEQ      'S'                                          *VOCE DA CALCOLARE
135000970306     C     $TGC          ANDEQ     '1'                                          *TROVATA TAR.GIACENZ
135100970306     C                   EXSR      CALGRP
135200001227     C                   EXSR      CAL057
135300001227     C                   EXSR      CAL074
135400970306E   2C                   ENDIF
135500970306E   1C                   ENDIF
135600970306     C*
135700970306     C* CALCOLA LE SPESE DI GIACENZA DI RICONSEGNA CITTA'
135800970306IF  1C     WERR          IFEQ      'N'
135900970306IF  2C     VC(19)        IFEQ      'S'                                          *VOCE DA CALCOLARE
136000970306     C     $TGC          ANDEQ     '1'                                          *TROVATA TAR.GIACENZ
136100970306     C                   EXSR      CALGRC
136200001227     C                   EXSR      CAL058
136300001227     C                   EXSR      CAL075
136400970306E   2C                   ENDIF
136500970306E   1C                   ENDIF
136600970306     C*
136700970306     C* CALCOLA LE SPESE DI GIACENZA DI SOSTA
136800970306IF  1C     WERR          IFEQ      'N'
136900970306IF  2C     VC(20)        IFEQ      'S'                                          *VOCE DA CALCOLARE
137000970306     C     $TGC          ANDEQ     '1'                                          *TROVATA TAR.GIACENZ
137100970306     C                   EXSR      CALGSO
137200001227     C                   EXSR      CAL059
137300001227     C                   EXSR      CAL076
137400970306E   2C                   ENDIF
137500970306E   1C                   ENDIF
137600970306     C*
137700970306     C                   ENDSR
137800970306     C*------------------------------------------------------------------------*
137900970306     C* CALTCP - CALCOLA T.C.P.
138000970306     C*------------------------------------------------------------------------*
138100970306     C     CALTCP        BEGSR
138200990819     C*
138300990819     C* REIMPOSTA LE VARIABILI DI LAVORO
138400990819     C                   MOVEL     'S'           WCAR                           *SI UTILIZZO TAR.CAR
138500011031     C                   Z-ADD     *ZEROS        TASTMP           21 8
138600990819     C*
138700990819     C* CALCOLA CONSEGNA PARTICOLARE
138800990819     C                   MOVEL     '7'           TASTC             1            *CODICE CONSEGNA
138900011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
139000990819     C                   EXSR      CALCP
139100990819     C                   Z-ADD     TASCP         TASTMP
139200970306     C*
139300970314IF  1C     TASTMP        IFGT      *ZEROS
139400970306     C                   Z-ADD     5             I
139500970306     C                   Z-ADD     TASTMP        VIMP(I)
139600970314E   1C                   ENDIF
139700990819     C*
139800970306     C                   ENDSR
139900001222     C*------------------------------------------------------------------------*
140000001222     C* CAL046 - CALCOLA ADG SU TCP
140100001222     C*------------------------------------------------------------------------*
140200001222     C     CAL046        BEGSR
140300001222     C*
140400001222     C* REIMPOSTA LE VARIABILI DI LAVORO
140500011031     C                   Z-ADD     *ZEROS        TASADG           21 8
140600001222     C*
140700001222     C* CALCOLA L'A.D.G.
140800011031     C                   Z-ADD     VIMP(5)       WIMP             23 8
140900001222     C                   EXSR      ADGCAL
141000001222     C*
141100001222IF  1C     TASADG        IFGT      *ZEROS
141200001222     C                   Z-ADD     46            I
141300001222     C                   Z-ADD     TASADG        VIMP(I)
141400001222E   1C                   ENDIF
141500001222     C*
141600001222     C                   ENDSR
141700001222     C*------------------------------------------------------------------------*
141800001222     C* CAL063 - CALCOLA ISTAT SU ADG/TCP
141900001222     C*------------------------------------------------------------------------*
142000001222     C     CAL063        BEGSR
142100001222     C*
142200001222     C* REIMPOSTA LE VARIABILI DI LAVORO
142300011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
142400001222     C*
142500001222     C* CALCOLA L'ISTAT
142600011031     C                   Z-ADD     VIMP(46)      WIMP             23 8
142700001222     C                   EXSR      IACCAL
142800001222     C*
142900001222IF  1C     TASIAC        IFGT      *ZEROS
143000001222     C                   Z-ADD     63            I
143100001222     C                   Z-ADD     TASIAC        VIMP(I)
143200001222E   1C                   ENDIF
143300001222     C*
143400001222     C                   ENDSR
143500040826     C*------------------------------------------------------------------------*
143600040826     C* CALdps - CALCOLA diritto di pesatura
143700040826     C*------------------------------------------------------------------------*
143800040826     C     CALdps        BEGSR
143900040826     C*
144000040826     C* REIMPOSTA LE VARIABILI DI LAVORO
144100040826     C                   MOVEL     'S'           WCAR                           *SI UTILIZZO TAR.CAR
144200040826     C                   Z-ADD     *ZEROS        TASdps           21 8
144300040826     C*
144400040826     C* CALCOLA CONSEGNA PARTICOLARE
144500040826     C                   MOVEL     '5'           TASTC             1            *CODICE CONSEGNA
144600040826     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
144700040826     C                   EXSR      CALCP
144800040826     C                   Z-ADD     TASCP         TASdps
144900040826     C*
145000040826IF  1C     TASdps        IFGT      *ZEROS
145100040826     C                   Z-ADD     102           I
145200040826     C                   Z-ADD     TASdps        VIMP(I)
145300040826E   1C                   ENDIF
145400040826     C*
145500040826     C                   ENDSR
145600040826     C*------------------------------------------------------------------------*
145700040826     C* CAL103 - CALCOLA ADG SU TCP
145800040826     C*------------------------------------------------------------------------*
145900040826     C     CAL103        BEGSR
146000040826     C*
146100040826     C* REIMPOSTA LE VARIABILI DI LAVORO
146200040826     C                   Z-ADD     *ZEROS        TASADG           21 8
146300040826     C*
146400040826     C* CALCOLA L'A.D.G.
146500040826     C                   Z-ADD     VIMP(102)     WIMP             23 8
146600040826     C                   EXSR      ADGCAL
146700040826     C*
146800040826IF  1C     TASADG        IFGT      *ZEROS
146900040826     C                   Z-ADD     103           I
147000040826     C                   Z-ADD     TASADG        VIMP(I)
147100040826E   1C                   ENDIF
147200040826     C*
147300040826     C                   ENDSR
147400040826     C*------------------------------------------------------------------------*
147500040826     C* CAL104 - CALCOLA ISTAT SU ADG/TCP
147600040826     C*------------------------------------------------------------------------*
147700040826     C     CAL104        BEGSR
147800040826     C*
147900040826     C* REIMPOSTA LE VARIABILI DI LAVORO
148000040826     C                   Z-ADD     *ZEROS        TASIAC           21 8
148100040826     C*
148200040826     C* CALCOLA L'ISTAT
148300040826     C                   Z-ADD     VIMP(103)     WIMP             23 8
148400040826     C                   EXSR      IACCAL
148500040826     C*
148600040826IF  1C     TASIAC        IFGT      *ZEROS
148700040826     C                   Z-ADD     104           I
148800040826     C                   Z-ADD     TASIAC        VIMP(I)
148900040826E   1C                   ENDIF
149000040826     C*
149100040826     C                   ENDSR
149200970306     C*------------------------------------------------------------------------*
149300970306     C* CALGRP - CALCOLA LE SPESE DI GIACENZA RICONSEGNA PROVINCIA
149400970306     C*------------------------------------------------------------------------*
149500970306     C     CALGRP        BEGSR
149600970306     C*
149700970306     C* REIMPOSTA LE VARIABILI DI LAVORO
149800011031     C                   Z-ADD     *ZEROS        TASSGP           21 8
149900970306     C*
150000970306     C* CALCOLA IL PESO TASSABILE -GIACENZA-
150100970306IF  1C     TGCSGP        IFGT      *ZEROS                                       *IMMESSA TARIFFA
150200970306     C                   Z-ADD     *ZEROS        §PESCA
150300970306     C                   EXSR      CALPTG
150400970306     C*
150500970306     C* CALCOLA LE SPESE DI GIACENZA
150600970306     C     TGCSGP        MULT      ETCNRG        TASSGR                         *SPESE X N°RIAPERTUR
150700970306     C     TASSGR        MULT      §PESCA        TASSGR                         *SPESE X PESO TASSAB
150800970306     C*
150900970306     C* AZZERA LE SPESE DI RICONSEGNA SE SI E' DENTRO AI GIORNI DI FRANCHIGIA
151000970306IF  2C     TGCFAF        IFEQ      '2'                                          *FRANCHIGIA
151100970306     C     ETCGGG        ANDLE     TGCSGF                                       *GG SOSTA < GG FRANC
151200970306     C                   Z-ADD     *ZEROS        TASSGP                         *NO SPESE RICONSEGNA
151300970306E   2C                   ENDIF
151400970306E   1C                   ENDIF
151500970306     C*
151600970314IF  1C     TASSGP        IFGT      *ZEROS
151700970306     C                   Z-ADD     18            I
151800970306     C                   Z-ADD     TASSGP        VIMP(I)
151900970314E   1C                   ENDIF
152000970306     C*
152100970306     C                   ENDSR
152200001227     C*------------------------------------------------------------------------*
152300001227     C* CAL057 - CALCOLA ADG SU GIACENZA RICONSEGNA PROVINCIA
152400001227     C*------------------------------------------------------------------------*
152500001227     C     CAL057        BEGSR
152600001227     C*
152700001227     C* REIMPOSTA LE VARIABILI DI LAVORO
152800011031     C                   Z-ADD     *ZEROS        TASADG           21 8
152900001227     C*
153000001227     C* CALCOLA L'A.D.G.
153100011031     C                   Z-ADD     VIMP(18)      WIMP             23 8
153200001227     C                   EXSR      ADGCAL
153300001227     C*
153400001227IF  1C     TASADG        IFGT      *ZEROS
153500001227     C                   Z-ADD     57            I
153600001227     C                   Z-ADD     TASADG        VIMP(I)
153700001227E   1C                   ENDIF
153800001227     C*
153900001227     C                   ENDSR
154000001227     C*------------------------------------------------------------------------*
154100001227     C* CAL074 - CALCOLA ISTAT SU ADG/GIACENZA RICONSEGNA PROVINCIA
154200001227     C*------------------------------------------------------------------------*
154300001227     C     CAL074        BEGSR
154400001227     C*
154500001227     C* REIMPOSTA LE VARIABILI DI LAVORO
154600011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
154700001227     C*
154800001227     C* CALCOLA L'ISTAT
154900011031     C                   Z-ADD     VIMP(57)      WIMP             23 8
155000001227     C                   EXSR      IACCAL
155100001227     C*
155200001227IF  1C     TASIAC        IFGT      *ZEROS
155300001227     C                   Z-ADD     74            I
155400001227     C                   Z-ADD     TASIAC        VIMP(I)
155500001227E   1C                   ENDIF
155600001227     C*
155700001227     C                   ENDSR
155800970306     C*------------------------------------------------------------------------*
155900970306     C* CALGRC - CALCOLA LE SPESE DI GIACENZA RICONSEGNA CITTA'
156000970306     C*------------------------------------------------------------------------*
156100970306     C     CALGRC        BEGSR
156200970306     C*
156300970306     C* REIMPOSTA LE VARIABILI DI LAVORO
156400011031     C                   Z-ADD     *ZEROS        TASSGR           21 8
156500970306     C*
156600970306     C* CALCOLA IL PESO TASSABILE -GIACENZA-
156700970306IF  1C     TGCSGR        IFGT      *ZEROS                                       *IMMESSA TARIFFA
156800970306     C                   Z-ADD     *ZEROS        §PESCA
156900970306     C                   EXSR      CALPTG
157000970306     C*
157100970306     C* CALCOLA LE SPESE DI GIACENZA
157200970306     C     TGCSGR        MULT      ETCNRG        TASSGR                         *SPESE X N°RIAPERTUR
157300970306     C     TASSGR        MULT      §PESCA        TASSGR                         *SPESE X PESO TASSAB
157400970306     C*
157500970306     C* AZZERA LE SPESE DI RICONSEGNA SE SI E' DENTRO AI GIORNI DI FRANCHIGIA
157600970306IF  2C     TGCFAF        IFEQ      '2'                                          *FRANCHIGIA
157700970306     C     ETCGGG        ANDLE     TGCSGF                                       *GG SOSTA < GG FRANC
157800970306     C                   Z-ADD     *ZEROS        TASSGR                         *NO SPESE RICONSEGNA
157900970306E   2C                   ENDIF
158000970306E   1C                   ENDIF
158100970306     C*
158200970314IF  1C     TASSGR        IFGT      *ZEROS
158300970306     C                   Z-ADD     19            I
158400970306     C                   Z-ADD     TASSGR        VIMP(I)
158500970314E   1C                   ENDIF
158600970306     C*
158700970306     C                   ENDSR
158800001227     C*------------------------------------------------------------------------*
158900001227     C* CAL058 - CALCOLA ADG SU GIACENZA RICONSEGNA CITTA'
159000001227     C*------------------------------------------------------------------------*
159100001227     C     CAL058        BEGSR
159200001227     C*
159300001227     C* REIMPOSTA LE VARIABILI DI LAVORO
159400011031     C                   Z-ADD     *ZEROS        TASADG           21 8
159500001227     C*
159600001227     C* CALCOLA L'A.D.G.
159700011031     C                   Z-ADD     VIMP(19)      WIMP             23 8
159800001227     C                   EXSR      ADGCAL
159900001227     C*
160000001227IF  1C     TASADG        IFGT      *ZEROS
160100001227     C                   Z-ADD     58            I
160200001227     C                   Z-ADD     TASADG        VIMP(I)
160300001227E   1C                   ENDIF
160400001227     C*
160500001227     C                   ENDSR
160600001227     C*------------------------------------------------------------------------*
160700001227     C* CAL075 - CALCOLA ISTAT SU ADG/GIACENZA RICONSEGNA CITTA'
160800001227     C*------------------------------------------------------------------------*
160900001227     C     CAL075        BEGSR
161000001227     C*
161100001227     C* REIMPOSTA LE VARIABILI DI LAVORO
161200011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
161300001227     C*
161400001227     C* CALCOLA L'ISTAT
161500011031     C                   Z-ADD     VIMP(58)      WIMP             23 8
161600001227     C                   EXSR      IACCAL
161700001227     C*
161800001227IF  1C     TASIAC        IFGT      *ZEROS
161900001227     C                   Z-ADD     75            I
162000001227     C                   Z-ADD     TASIAC        VIMP(I)
162100001227E   1C                   ENDIF
162200001227     C*
162300001227     C                   ENDSR
162400970306     C*------------------------------------------------------------------------*
162500970306     C* CALGSO - CALCOLA LE SPESE DI GIACENZA DI SOSTA
162600970306     C*------------------------------------------------------------------------*
162700970306     C     CALGSO        BEGSR
162800970306     C*
162900970306     C     *LIKE         DEFINE    ETCGGG        SAVGGG
163000970306     C     *LIKE         DEFINE    ETCGGG        GGGETC
163100970306     C*
163200970306     C* REIMPOSTA LE VARIABILI DI LAVORO
163300011031     C                   Z-ADD     *ZEROS        TASSGS           21 8
163400970306     C                   Z-ADD     *ZEROS        X                 1 0
163500970306     C                   Z-ADD     *ZEROS        XP                1 0
163600970306     C                   Z-ADD     *ZEROS        SGG                            *GG SOSTA
163700970306     C                   Z-ADD     *ZEROS        STU                            *TARIFFA UNITARIA
163800970306     C                   Z-ADD     *ZEROS        STT                            *TARIFFA TOTALE
163900970306     C*
164000970306     C                   Z-ADD     ETCGGG        SAVGGG
164100970306     C                   Z-ADD     ETCGGG        GGGETC
164200970306DO  1C     1             DO        3             X
164300970306IF  2C     SAVGGG        IFGT      TGS(X)
164400970306     C     SAVGGG        SUB       TGS(X)        GGGETC
164500970306IF  3C     X             IFEQ      1
164600970306     C                   Z-ADD     TGS(X)        SGG(X)
164700970306X   3C                   ELSE
164800970306     C     TGS(X)        SUB       TGS(XP)       SGG(X)
164900970306E   3C                   ENDIF
165000970306     C                   Z-ADD     TTS(X)        STU(X)
165100970306X   2C                   ELSE
165200970306     C                   Z-ADD     GGGETC        SGG(X)
165300970306     C                   Z-ADD     TTS(X)        STU(X)
165400970306     C                   Z-ADD     *ZEROS        SAVGGG
165500970306     C                   Z-ADD     *ZEROS        GGGETC
165600970306E   2C                   ENDIF
165700970306     C                   ADD       1             XP
165800970306E   1C                   ENDDO
165900970306     C*
166000970306     C* CALCOLA IL PESO TASSABILE -GIACENZA-
166100970306     C                   Z-ADD     *ZEROS        §PESCA
166200970306     C                   EXSR      CALPTG
166300970306     C*
166400970306     C* CALCOLA LE SPESE DI GIACENZA
166500970306     C     SGG           MULT      STU           STT
166600970306     C     STT           MULT      §PESCA        STT
166700970306     C                   XFOOT     STT           TASSGS
166800970306     C*
166900970306     C* APPLICA IL MINIMO TARIFFARIO SE SUPERATI I GIORNI DI FRANCHIGIA
167000970306IF  1C     TASSGS        IFLT      TGCSTM
167100970306     C     ETCGGG        ANDGT     TGCSGF
167200970306     C                   Z-ADD     TGCSTM        TASSGS
167300970306E   1C                   ENDIF
167400970306     C*
167500970306     C* AZZERA LE SPESE DI SOSTA SE SI E' DENTRO AI GIORNI DI FRANCHIGIA
167600970306IF  1C     TGCFAF        IFNE      *BLANKS                                      *FRANCHIGIA
167700970306     C     ETCGGG        ANDLE     TGCSGF                                       *GG SOSTA < GG FRANC
167800970306     C                   Z-ADD     *ZEROS        TASSGS                         *NO SPESE SOSTA
167900970306E   1C                   ENDIF
168000970306     C*
168100970314IF  1C     TASSGS        IFGT      *ZEROS
168200970306     C                   Z-ADD     20            I
168300970306     C                   Z-ADD     TASSGS        VIMP(I)
168400970314E   1C                   ENDIF
168500970306     C*
168600970306     C                   ENDSR
168700001227     C*------------------------------------------------------------------------*
168800001227     C* CAL059 - CALCOLA ADG SU GIACENZA SOSTA
168900001227     C*------------------------------------------------------------------------*
169000001227     C     CAL059        BEGSR
169100001227     C*
169200001227     C* REIMPOSTA LE VARIABILI DI LAVORO
169300011031     C                   Z-ADD     *ZEROS        TASADG           21 8
169400001227     C*
169500001227     C* CALCOLA L'A.D.G.
169600011031     C                   Z-ADD     VIMP(20)      WIMP             23 8
169700001227     C                   EXSR      ADGCAL
169800001227     C*
169900001227IF  1C     TASADG        IFGT      *ZEROS
170000001227     C                   Z-ADD     59            I
170100001227     C                   Z-ADD     TASADG        VIMP(I)
170200001227E   1C                   ENDIF
170300001227     C*
170400001227     C                   ENDSR
170500001227     C*------------------------------------------------------------------------*
170600001227     C* CAL076 - CALCOLA ISTAT SU ADG/GIACENZA SOSTA
170700001227     C*------------------------------------------------------------------------*
170800001227     C     CAL076        BEGSR
170900001227     C*
171000001227     C* REIMPOSTA LE VARIABILI DI LAVORO
171100011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
171200001227     C*
171300001227     C* CALCOLA L'ISTAT
171400011031     C                   Z-ADD     VIMP(59)      WIMP             23 8
171500001227     C                   EXSR      IACCAL
171600001227     C*
171700001227IF  1C     TASIAC        IFGT      *ZEROS
171800001227     C                   Z-ADD     76            I
171900001227     C                   Z-ADD     TASIAC        VIMP(I)
172000001227E   1C                   ENDIF
172100001227     C*
172200001227     C                   ENDSR
172300970306     C*------------------------------------------------------------------------*
172400970306     C* CALPTG - CALCOLA IL PESO TASSABILE -GIACENZA-
172500970306     C*------------------------------------------------------------------------*
172600970306     C     CALPTG        BEGSR
172700970306     C*
172800970306     C* REIMPOSTA LE VARIABILI DI LAVORO
172900970306     C                   Z-ADD     *ZEROS        §PESVO           13 3
173000970306     C                   Z-ADD     *ZEROS        §PES§            13 3
173100970306     C                   Z-ADD     *ZEROS        §PESCA            6 0
173200970306     C*
173300970725     C     TGCRPV        MULT      SVSC(S)       §PESVO
173400970306     C     §PESVO        MULT      100           §PESVO
173500970306IF  1C     §PESVO        IFGT      SPSC(S)
173600970306     C     §PESVO        DIV       100           §PES§
173700970306X   1C                   ELSE
173800970306     C     SPSC(S)       DIV       100           §PES§
173900970306E   1C                   ENDIF
174000970306     C     §PES§         ADD       0,99          §PESCA
174100970306     C*
174200970306     C                   ENDSR
174300970306     C*------------------------------------------------------------------------*
174400970306     C* CARTAD - CARICAMENTO TARIFFA CLIENTE -DETTAGLIO-
174500970306     C*------------------------------------------------------------------------*
174600970306     C     CARTAD        BEGSR
174700970306     C*
174800970306     C* REIMPOSTA LE VARIABILI DI LAVORO
174900980707DO  1C     1             DO        11            I
175000970306     C     I             OCCUR     TARDS
175100970306     C                   CLEAR                   TARDS
175200970306E   1C                   ENDDO
175300970306     C                   MOVEL     '0'           $TAD                           *TARIFFA TROVATA
175400970306     C*
175500970306     C* CERCA LA TARIFFA CLIENTE CON IL CODICE TASSAZIONE ELEMENTARE
175600970306     C                   Z-ADD     D00KSC        KTDKSC                         *CLIENTE
175700970306     C                   Z-ADD     D00PRG        KTDPRG                         *PROGRESSIVO
175800970306     C                   Z-ADD     D00LNP        KTDLNP                         *LINEA DI PARTENZA
175900990819     C                   MOVEL     CCTS(T)       KTDCTS                         *COD.TASS.ELEMENTARE
176000990824     C                   MOVEL     *BLANKS       KTDNAZ                         *NAZIONE
176100970306     C                   MOVEL     *BLANKS       KTDCAP                         *C.A.P.
176200970306     C                   Z-ADD     D00CTR        KTDCTR                         *CODICE TARIFFA
176300990819     C     KEYTAD        SETLL     TITAD00L                               98
176400970306IF  1C     *IN98         IFEQ      *ON                                          *TARIFFA TROVATA
176500970306     C                   MOVEL     '1'           $TAD
176600970306X   1C                   ELSE                                                   *TARIFFA NON TROVATA
176700970306     C*
176800970306     C* CERCA LA TARIFFA CLIENTE CON IL CODICE TASSAZIONE REGIONE
176900990803     C                   MOVEL     CRAP(T)       KTDCTS                         *COD.TASS.REGIONE
177000990819     C     KEYTAD        SETLL     TITAD00L                               98
177100990819IF  2C     *IN98         IFEQ      *ON                                          *TARIFFA TROVATA
177200970306     C                   MOVEL     '1'           $TAD
177300990819X   2C                   ELSE                                                   *TARIFFA NON TROVATA
177400970306     C*
177500970306     C* CERCA LA TARIFFA CLIENTE CON IL CODICE TASSAZIONE NAZIONE
177600990803     C                   MOVEL     CNAR(T)       KTDCTS                         *COD.TASS.NAZIONE
177700990819     C     KEYTAD        SETLL     TITAD00L                               98
177800990819IF  3C     *IN98         IFEQ      *ON                                          *TARIFFA TROVATA
177900970306     C                   MOVEL     '1'           $TAD
178000990819E   3C                   ENDIF
178100990819E   2C                   ENDIF
178200990819E   1C                   ENDIF
178300970306     C*
178400970306     C* SE TARIFFA NON TROVATA, ERRORE
178500970306IF  1C     $TAD          IFEQ      '0'
178600970306     C                   MOVEL     MSG(24)       $MSG
178700970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
178800970306     C*
178900970306     C* SE TARIFFA TROVATA, CARICA DETTAGLIO
179000970306X   1C                   ELSE
179100980707     C                   EXSR      CARDST
179200970306E   1C                   ENDIF
179300970306     C*
179400970306     C                   ENDSR
179500980707     C*------------------------------------------------------------------------*
179600980707     C* CARDST - CARICA CAMPI TARIFFA -DETTAGLIO-
179700980707     C*------------------------------------------------------------------------*
179800980707     C     CARDST        BEGSR
179900980707     C*
180000980707     C     *LIKE         DEFINE    TADRPV        RAPPV
180100980707     C                   DO        11            A                 5 0
180200980707     C     A             OCCUR     TARDS
180300980707     C                   CLEAR                   TARDS
180400980707     C                   ENDDO
180500980707     C                   Z-ADD     0             RAPPV
180600980707     C                   CLEAR                   WELAMT
180700980908     C                   Z-ADD     DMITAS        MINTAS                         *MIN.TASS. TNTAM
180800980707    1C                   DO        *HIVAL        A
180900990819     C**
181000980707     C** NON DEVO RILEGGERE PERCHE' DEVO CARICARE LO STESSO SCAGLIONE
181100980707    2C     WELAMT        IFNE      'S'
181200990819     C     KEYTAD        READE     TITAD000                               15
181300980707     C   15              GOTO      ENDDST
181400991125     C*
181500991125     C* CONTROLLA SE CONVERTIRE I CAMPI DI TNTAM
181600011026    3C     D00DIV        IFNE      WDIVCE
181700011019     C                   MOVEL     D00DIV        WDVI
181800991125     C                   EXSR      CNVTAD
181900991125    3C                   ENDIF
182000991125    2C                   ENDIF
182100980707     C** ELABORATO UNA VOLTA NON LO FACCIO PIU'
182200980707    2C     WELAMT        IFEQ      'S'
182300980707     C                   MOVEL     'E'           WELAMT            1
182400980707    2C                   ENDIF
182500980707     C**
182600980707     C     A             OCCUR     TARDS
182700980707     C* SE LO SCAGLIONE LETTO E' > DEL MINIMO TASSABILE CARICO
182800980707     C*  ANCHE LO SCAGLIONE DEL MINIMO CON TARIFFA=TARIFFA X MIN.TAS
182900980707     C**
183000980707     C                   Z-ADD     TADLNP        TLNP
183100980707     C                   MOVEL     TADCTS        TCTS
183200990819     C                   MOVEL     TADNAZ        TNAZ
183300990819     C                   MOVEL     TADCAP        TCAP
183400980707     C                   Z-ADD     TADCTR        TCTR
183500980707     C                   Z-ADD     TADRPV        TRPV
183600980707     C                   Z-ADD     TADMIN        TMIN
183700980707     C                   Z-ADD     TADMAX        TMAX
183800980707     C                   Z-ADD     TADRPV        RAPPV
183900991125     C*
184000980707     C** SE HO GIA' ELABORATO IL MINIMO NON LO FACCIO PIU'
184100980707    2C     WELAMT        IFNE      'E'
184200980707     C**
184300980707    3C     TADSGL        IFEQ      MINTAS
184400980707     C                   MOVEL     'E'           WELAMT
184500980707   X3C                   ELSE
184600980707     C** SE LO SCAGLIONE CHE LEGGO E' MAGGIORE DEL MINIMO TASSABILE
184700980707     C*  UTILIZZO PRIMA LA TARIFFA DELLO SCAGLIONE PER IL MINIMO T.
184800980707     C*  (MOLTIPLICANDOLA PER IL MINIMO TASSABILE)
184900980707     C*  POI LO UTILIZZO PER LO SCAGLIONE LETTO
185000980707    4C     TADSGL        IFGT      MINTAS
185100980707     C                   MOVEL     'S'           WELAMT
185200980707     C                   Z-ADD     MINTAS        TSCG
185300980707     C**
185400980707     C* SE NON C'E' APPLICAZIONE TARIFFA FINITA -- MOLTIPLICO
185500980707    5C     TAMATA        IFEQ      0
185600980707     C     TAMATA        ORLE      MINTAS
185700980707     C     TADSGL        ORGT      TAMATA
185800980707     C**
185900991125    6C     UNOCTR        IFEQ      4
186000980707     C     UNOCTR        OREQ      0
186100011031     C     MINTAS        DIV(H)    100           WMTAS            21 8
186200980707     C                   ELSE
186300980707     C                   Z-ADD     MINTAS        WMTAS
186400980707    6C                   ENDIF
186500980707     C* TARIFFA
186600010926     C     WMTAS         MULT(H)   TADITR        TITR
186700980707     C     WMTAS         MULT(H)   TADINO        TINO
186800980707     C                   ELSE
186900980707     C* C'E' APPLICAZIONE MAGGIORE --> UTILIZZO LA TARIFFA COSI' COM'E'
187000980707     C                   Z-ADD     TADITR        TITR
187100980707     C                   Z-ADD     TADINO        TINO
187200980707    5C                   ENDIF
187300980707    4C                   ENDIF
187400980707    3C                   ENDIF
187500980707    2C                   ENDIF
187600980707     C*
187700980707     C* SE NON HO CARICATO LO SCAGLIONE PER IL MINIMO TASSABILE
187800980707    2C     WELAMT        IFNE      'S'
187900980707     C                   Z-ADD     TADSGL        TSCG
188000980707     C                   Z-ADD     TADITR        TITR
188100980707     C                   Z-ADD     TADINO        TINO
188200980707    2C                   ENDIF
188300980707    1C                   ENDDO
188400991125     C*
188500980707     C     ENDDST        ENDSR
188600980707     C*------------------------------------------------------------------------*
188700980707     C* CALQTA - REPERISCE E CALCOLA LA QUANTITA' DA UTILIZZARE
188800980707     C*------------------------------------------------------------------------*
188900980707     C     CALQTA        BEGSR
189000980707     C*
189100980707     C* PESO TASSABILE E QUANTITA' DA FATTURARE
189200980707     C                   MOVEL     UNOCTR        WTPG              1
189300980707     C                   CLEAR                   WRPV
189400980707     C                   MOVEL     'TAD'         WDET              3
189500980707     C                   Z-ADD     TAMARL        WARL
189600980707     C                   Z-ADD     TAMARF        WARF
189700980707     C                   Z-ADD     TAMARO        WARO
189800980707     C                   EXSR      CALCOL
189900980707     C                   Z-ADD     ISOPES        PESTAS           13 3
190000980707     C*
190100980707     C                   ENDSR
190200980707     C*------------------------------------------------------------------------*
190300980707     C* CALCOL - CALCOLA RPV, ARROTONDAMENTI E PESO TASSABILE
190400980707     C*------------------------------------------------------------------------*
190500980707     C     CALCOL        BEGSR
190600980707     C*
190700980707     C     *LIKE         DEFINE    TAMARL        ARR
190800980707     C     *LIKE         DEFINE    TPTRPV        WRPV
190900980707     C     *LIKE         DEFINE    TPTARL        WARL
191000980707     C     *LIKE         DEFINE    TPTARF        WARF
191100980707     C     *LIKE         DEFINE    TPTARO        WARO
191200980707     C                   Z-ADD     0             ISOPES           13 3
191300980707SL  1C                   SELECT
191400980707     C* TARIFFA A QLI
191500980707     C     WTPG          WHENEQ    '0'
191600980707     C                   Z-ADD     SPSC(S)       ISOPES
191700980707     C* TARIFFA A COLLI
191800980707     C     WTPG          WHENEQ    '1'
191900980707     C                   Z-ADD     SCSC(S)       ISOPES
192000980707     C* TARIFFA A SPEDIZIONE
192100980707     C     WTPG          WHENEQ    '2'
192200980707     C                   Z-ADD     1             ISOPES
192300980707     C* TARIFFA A KG
192400980707     C     WTPG          WHENEQ    '3'
192500980707     C                   Z-ADD     SPSC(S)       ISOPES
192600980707E   1C                   ENDSL
192700980707     C** MINIMO TASSABILE STD
192800980707     C                   Z-ADD     0             MINTAS
192900980707     C**
193000980707     C                   Z-ADD     1             A
193100980707     C     WTPG          LOOKUP    CTR(A)                                 05
193200980707     C   05A             OCCUR     DSTR
193300980707     C   05              Z-ADD     §TRATA        MINTAS
193400980707     C**
193500980707     C** SOLO PER TAD (PER TPD NON MI SERVE)
193600980707    1C     WDET          IFEQ      'TAD'
193700980707     C                   CLEAR                   MINTAR
193800980707     C** UTILIZZO IL CAMPO CON I DECIMALI PER IL MINIMO TASSABILE
193900980707     C*  DA TABELLA TR. SE POI C'E' ANCHE IN TARIFFA USO QUELLO
194000980707     C   05              Z-ADD     §TRATA        MINTAR
194100980707    2C     TAMATA        IFGT      0
194200980707     C                   Z-ADD     TAMATA        MINTAR
194300980707    2C                   ENDIF
194400980707    1C                   ENDIF
194500980707     C**
194600980707     C** RPV SOLO PER TARIFFA 0 E 3
194700980707    1C     §TRRPV        IFEQ      'S'
194800980707     C** PER TAD PRENDO RPV DA RIGA DI DETTAGLIO CON PESO REALE
194900980707    2C     WDET          IFEQ      'TAD'
195000980707     C     SVSC(S)       ANDGT     0
195100980707    3C                   DO        11            A
195200980707     C     A             OCCUR     TARDS
195300980707    4C     TSCG          IFGE      ISOPES
195400980707     C                   GOTO      POITAS
195500980707    4C                   END
195600980707    3C                   END
195700980707     C** RICERCO RPV CON PESO REALE
195800980707    3C     TSCG          IFLT      ISOPES
195900980707     C                   MOVEL     MSG(25)       $MSG
196000980707     C                   EXSR      IMPERR
196100980707    3C                   END
196200980707     C     POITAS        TAG
196300980707     C                   Z-ADD     TRPV          WRPV
196400980707    2C                   ENDIF
196500980707     C**
196600980707    2C     SVSC(S)       IFNE      0
196700980707     C     WRPV          ANDNE     0
196800980707     C                   Z-ADD     0             COMPRV            9 3
196900980707     C     SVSC(S)       MULT      WRPV          COMPRV
197000980707     C     COMPRV        MULT      100           ISOPE2           13 3
197100980707    3C     ISOPE2        IFGT      ISOPES
197200980707     C                   Z-ADD     ISOPE2        ISOPES
197300980707    3C                   ENDIF
197400980707    2C                   ENDIF
197500980707    1C                   ENDIF
197600980707     C**
197700980707     C** ARROTONDAMENTI: LO FACCIO SEOCONDO QUANTO INDICATO IN DSTR
197800980707    0C     §TRARR        IFEQ      'S'
197900980707     C**
198000980707     C** ARROTONDAMENTO: TARIFFE 0 Q.LI - 1 - 3
198100980707     C     ISOPES        IFGE      MINTAS
198200980707     C                   Z-ADD     0             ARR
198300980707     C     ISOPES        IFGT      WARL
198400980707     C                   Z-ADD     WARO          ARR
198500980707     C                   ELSE
198600980707     C                   Z-ADD     WARF          ARR
198700980707     C                   END
198800980707     C**
198900011031     C                   Z-ADD     0             RESTO            21 8
199000011108     C                   Z-ADD     0             ISOPEX           21 0
199100980707     C     ARR           IFNE      0
199200980707     C     ISOPES        ANDNE     0
199300980707     C     ISOPES        DIV       ARR           ISOPEX
199400980707     C                   MVR                     RESTO
199500980707     C     RESTO         IFGT      0
199600980707     C     ISOPEX        MULT      ARR           ISOPES
199700980707     C                   ADD       ARR           ISOPES
199800980707     C                   ENDIF
199900980707     C                   ENDIF
200000980707     C                   ENDIF
200100980707     C**
200200980707    0C                   ENDIF
200300980707     C                   ENDSR
200400980707     C*------------------------------------------------------------------------*
200500980707     C* VALVO3 - VALORIZZA LE VOCI -DIPENDENTI DA CODICE TASSAZIONE-
200600980707     C*------------------------------------------------------------------------*
200700980707     C     VALVO3        BEGSR
200800970306     C*
200900970306     C* AZZERA GLI IMPORTI DELLE VOCI CON SEQUENZA TRE
201000970306DO  1C     1             DO        NVOCA         I
201100970306IF  2C     VS(I)         IFEQ      3
201200970306     C                   Z-ADD     *ZEROS        VIMP(I)                        *IMPORTO CALCOLATO
201300970306E   2C                   ENDIF
201400970306E   1C                   ENDDO
201500980702     C*---
201600980702     C* PORTO E INOLTRO
201700980702     C*---
201800980707IF  1C     WERR          IFEQ      'N'
201900980707IF  2C     VC(1)         IFEQ      'S'                                          *VOCE DA CALCOLARE
202000980707     C     VC(6)         OREQ      'S'                                          *VOCE DA CALCOLARE
202100980707     C                   EXSR      CALPEI
202200980707E   2C                   ENDIF
202300980707E   1C                   ENDIF
202400980702     C*---
202500980702     C* ALTRE PORTO
202600980702     C*---
202700980702     C* CALCOLA A.D.G. SUL PORTO
202800980702IF  1C     WERR          IFEQ      'N'
202900980702IF  2C     VC(3)         IFEQ      'S'                                          *VOCE DA CALCOLARE
203000980702     C                   EXSR      CALAGP
203100980702E   2C                   ENDIF
203200980702E   1C                   ENDIF
203300980702     C*
203400980702     C* CALCOLA ISTAT SUL PORTO
203500980702IF  1C     WERR          IFEQ      'N'
203600980702IF  2C     VC(4)         IFEQ      'S'                                          *VOCE DA CALCOLARE
203700980702     C                   EXSR      CALISP
203800980702E   2C                   ENDIF
203900980702E   1C                   ENDIF
204000980702     C*
204100980702     C* CALCOLA ISTAT SULL'A.D.G. DEL PORTO
204200980702IF  1C     WERR          IFEQ      'N'
204300980702IF  2C     VC(27)        IFEQ      'S'                                          *VOCE DA CALCOLARE
204400980702     C                   EXSR      CALIAP
204500980702E   2C                   ENDIF
204600980702E   1C                   ENDIF
204700980702     C*
204800980702     C* CALCOLA ESPRESSO SUL PORTO
204900980702IF  1C     WERR          IFEQ      'N'
205000980702IF  2C     VC(7)         IFEQ      'S'                                          *VOCE DA CALCOLARE
205100980702     C                   EXSR      CALESP
205200980702E   2C                   ENDIF
205300980702E   1C                   ENDIF
205400980702     C*
205500980702     C* CALCOLA A.D.G. SULL'ESPRESSO PORTO
205600980702IF  1C     WERR          IFEQ      'N'
205700980702IF  2C     VC(23)        IFEQ      'S'                                          *VOCE DA CALCOLARE
205800980702     C                   EXSR      CALAEP
205900980702E   2C                   ENDIF
206000980702E   1C                   ENDIF
206100980702     C*
206200980702     C* CALCOLA ISTAT SULL'ESPRESSO PORTO
206300980702IF  1C     WERR          IFEQ      'N'
206400980702IF  2C     VC(26)        IFEQ      'S'                                          *VOCE DA CALCOLARE
206500980702     C                   EXSR      CALIEP
206600980702E   2C                   ENDIF
206700980702E   1C                   ENDIF
206800980702     C*
206900980702     C* CALCOLA ISTAT SULL'A.D.G. DELL'ESPRESSO PORTO
207000980702IF  1C     WERR          IFEQ      'N'
207100980702IF  2C     VC(30)        IFEQ      'S'                                          *VOCE DA CALCOLARE
207200980702     C                   EXSR      CAIAEP
207300980702E   2C                   ENDIF
207400980702E   1C                   ENDIF
207500980702     C*---
207600980702     C* ALTRE INOLTRO
207700980702     C*---
207800980702     C* CALCOLA A.D.G. SULL'INOLTRO
207900980702IF  1C     WERR          IFEQ      'N'
208000980702IF  2C     VC(21)        IFEQ      'S'                                          *VOCE DA CALCOLARE
208100980702     C                   EXSR      CALAGI
208200980702E   2C                   ENDIF
208300980707E   1C                   ENDIF
208400980702     C*
208500980702     C* CALCOLA ISTAT SULL'INOLTRO
208600980702IF  1C     WERR          IFEQ      'N'
208700980702IF  2C     VC(24)        IFEQ      'S'                                          *VOCE DA CALCOLARE
208800980702     C                   EXSR      CALISI
208900980702E   2C                   ENDIF
209000980702E   1C                   ENDIF
209100980702     C*
209200980702     C* CALCOLA ISTAT SULL'A.D.G. DELL'INOLTRO
209300980702IF  1C     WERR          IFEQ      'N'
209400980702IF  2C     VC(29)        IFEQ      'S'                                          *VOCE DA CALCOLARE
209500980702     C                   EXSR      CALIAI
209600980702E   2C                   ENDIF
209700980702E   1C                   ENDIF
209800980702     C*
209900980702     C* CALCOLA ESPRESSO SULL'INOLTRO
210000980702IF  1C     WERR          IFEQ      'N'
210100980702IF  2C     VC(36)        IFEQ      'S'                                          *VOCE DA CALCOLARE
210200980702     C                   EXSR      CALESI
210300980702E   2C                   ENDIF
210400980702E   1C                   ENDIF
210500980702     C*
210600980702     C* CALCOLA A.D.G. SULL'ESPRESSO INOLTRO
210700980702IF  1C     WERR          IFEQ      'N'
210800980702IF  2C     VC(37)        IFEQ      'S'                                          *VOCE DA CALCOLARE
210900980702     C                   EXSR      CALAEI
211000980702E   2C                   ENDIF
211100980702E   1C                   ENDIF
211200980702     C*
211300980702     C* CALCOLA ISTAT SULL'ESPRESSO INOLTRO
211400980702IF  1C     WERR          IFEQ      'N'
211500980702IF  2C     VC(38)        IFEQ      'S'                                          *VOCE DA CALCOLARE
211600980702     C                   EXSR      CALIEI
211700980702E   2C                   ENDIF
211800980702E   1C                   ENDIF
211900980702     C*
212000980702     C* CALCOLA ISTAT SULL'A.D.G. DELL'ESPRESSO INOLTRO
212100980702IF  1C     WERR          IFEQ      'N'
212200980702IF  2C     VC(34)        IFEQ      'S'                                          *VOCE DA CALCOLARE
212300980702     C                   EXSR      CAIAEI
212400980702E   2C                   ENDIF
212500980702E   1C                   ENDIF
212600980702     C*---
212700980702     C* LOCALITA' DISAGIATE
212800980702     C*---
212900980702     C* CALCOLA CONSEGNA PARTICOLARE LOCALITA' DISAGIATE
213000980702IF  1C     WERR          IFEQ      'N'
213100980702IF  2C     VC(35)        IFEQ      'S'                                          *VOCE DA CALCOLARE
213200980702     C                   EXSR      CALCPL
213300980702E   2C                   ENDIF
213400980702E   1C                   ENDIF
213500980702     C*
213600980702     C* CALCOLA A.D.G. SULLE CONSEGNE LOCALITA' DISAGIATE
213700980702IF  1C     WERR          IFEQ      'N'
213800980702IF  2C     VC(43)        IFEQ      'S'                                          *VOCE DA CALCOLARE
213900980702     C                   EXSR      CALAGL
214000980702E   2C                   ENDIF
214100980702E   1C                   ENDIF
214200980702     C*
214300980702     C* CALCOLA ISTAT SULLE CONSEGNE LOCALITA' DISAGIATE
214400980702IF  1C     WERR          IFEQ      'N'
214500980702IF  2C     VC(44)        IFEQ      'S'                                          *VOCE DA CALCOLARE
214600980702     C                   EXSR      CALISL
214700980702E   2C                   ENDIF
214800980702E   1C                   ENDIF
214900980702     C*
215000980702     C* CALCOLA ISTAT SULL'A.D.G. DELLE CONSEGNE LOCALITA' DISAGIATE
215100980702IF  1C     WERR          IFEQ      'N'
215200980702IF  2C     VC(45)        IFEQ      'S'                                          *VOCE DA CALCOLARE
215300980702     C                   EXSR      CALIAL
215400980702E   2C                   ENDIF
215500980702E   1C                   ENDIF
215600980702     C*
215700980702     C* CALCOLA ESPRESSO SULLA CONSEGNA LOCALITA' DISAGIATE
215800980702IF  1C     WERR          IFEQ      'N'
215900980702IF  2C     VC(39)        IFEQ      'S'                                          *VOCE DA CALCOLARE
216000980702     C                   EXSR      CALESL
216100980702E   2C                   ENDIF
216200980702E   1C                   ENDIF
216300980702     C*
216400980702     C* CALCOLA A.D.G. SULL'ESPRESSO CONSEGNA LOCALITA' DISAGIATE
216500980702IF  1C     WERR          IFEQ      'N'
216600980702IF  2C     VC(40)        IFEQ      'S'                                          *VOCE DA CALCOLARE
216700980702     C                   EXSR      CALAEL
216800980702E   2C                   ENDIF
216900980702E   1C                   ENDIF
217000980702     C*
217100980702     C* CALCOLA ISTAT SULL'ESPRESSO CONSEGNA LOCALITA' DISAGIATE
217200980702IF  1C     WERR          IFEQ      'N'
217300980702IF  2C     VC(41)        IFEQ      'S'                                          *VOCE DA CALCOLARE
217400980702     C                   EXSR      CALIEL
217500980702E   2C                   ENDIF
217600980702E   1C                   ENDIF
217700980702     C*
217800980702     C* CALCOLA ISTAT SULL'A.D.G. DELL'ESPRESSO CONSEGNA LOCALITA' DISAGIATE
217900980702IF  1C     WERR          IFEQ      'N'
218000980702IF  2C     VC(42)        IFEQ      'S'                                          *VOCE DA CALCOLARE
218100980702     C                   EXSR      CAIAEL
218200980702E   2C                   ENDIF
218300980702E   1C                   ENDIF
218400011031     C*---
218500011031     C* RITIRO ISOLA
218600011031     C*---
218700011031     C* CALCOLA RITIRO ISOLA
218800011031IF  1C     WERR          IFEQ      'N'
218900011031IF  2C     VC(88)        IFEQ      'S'                                          *VOCE DA CALCOLARE
219000011031     C                   EXSR      CALSAM
219100011031E   2C                   ENDIF
219200011031E   1C                   ENDIF
219300011031     C*
219400011031     C* CALCOLA A.D.G. SUL RITIRO ISOLA
219500011031IF  1C     WERR          IFEQ      'N'
219600011031IF  2C     VC(93)        IFEQ      'S'                                          *VOCE DA CALCOLARE
219700011031     C                   EXSR      CALAGR
219800011031E   2C                   ENDIF
219900011031E   1C                   ENDIF
220000011031     C*
220100011031     C* CALCOLA ISTAT SUL RITIRO ISOLA
220200011031IF  1C     WERR          IFEQ      'N'
220300011031IF  2C     VC(94)        IFEQ      'S'                                          *VOCE DA CALCOLARE
220400011031     C                   EXSR      CALISR
220500011031E   2C                   ENDIF
220600011031E   1C                   ENDIF
220700011031     C*
220800011031     C* CALCOLA ISTAT SULL'A.D.G. DEL RITIRO ISOLA
220900011031IF  1C     WERR          IFEQ      'N'
221000011031IF  2C     VC(95)        IFEQ      'S'                                          *VOCE DA CALCOLARE
221100011031     C                   EXSR      CALIAR
221200011031E   2C                   ENDIF
221300011031E   1C                   ENDIF
221400011031     C*
221500011031     C* CALCOLA ESPRESSO SUL RITIRO ISOLA
221600011031IF  1C     WERR          IFEQ      'N'
221700011031IF  2C     VC(89)        IFEQ      'S'                                          *VOCE DA CALCOLARE
221800011031     C                   EXSR      CALESR
221900011031E   2C                   ENDIF
222000011031E   1C                   ENDIF
222100011031     C*
222200011031     C* CALCOLA A.D.G. SULL'ESPRESSO RITIRO ISOLA
222300011031IF  1C     WERR          IFEQ      'N'
222400011031IF  2C     VC(90)        IFEQ      'S'                                          *VOCE DA CALCOLARE
222500011031     C                   EXSR      CALAER
222600011031E   2C                   ENDIF
222700011031E   1C                   ENDIF
222800011031     C*
222900011031     C* CALCOLA ISTAT SULL'ESPRESSO RITIRO ISOLA
223000011031IF  1C     WERR          IFEQ      'N'
223100011031IF  2C     VC(91)        IFEQ      'S'                                          *VOCE DA CALCOLARE
223200011031     C                   EXSR      CALIER
223300011031E   2C                   ENDIF
223400011031E   1C                   ENDIF
223500011031     C*
223600011031     C* CALCOLA ISTAT SULL'A.D.G. DELL'ESPRESSO RITIRO ISOLA
223700011031IF  1C     WERR          IFEQ      'N'
223800011031IF  2C     VC(92)        IFEQ      'S'                                          *VOCE DA CALCOLARE
223900011031     C                   EXSR      CAIAER
224000011031E   2C                   ENDIF
224100011031E   1C                   ENDIF
224200011030     C*---
224300011030     C* CONSEGNA ISOLA
224400011030     C*---
224500011030     C* CALCOLA CONSEGNA PARTICOLARE ISLA
224600011030IF  1C     WERR          IFEQ      'N'
224700011030IF  2C     VC(80)        IFEQ      'S'                                          *VOCE DA CALCOLARE
224800011030     C                   EXSR      CALCIS
224900011030E   2C                   ENDIF
225000011030E   1C                   ENDIF
225100011030     C*
225200011030     C* CALCOLA A.D.G. SULLE CONSEGNE ISOLA
225300011030IF  1C     WERR          IFEQ      'N'
225400011030IF  2C     VC(85)        IFEQ      'S'                                          *VOCE DA CALCOLARE
225500011030     C                   EXSR      CALAGS
225600011030E   2C                   ENDIF
225700011030E   1C                   ENDIF
225800011030     C*
225900011030     C* CALCOLA ISTAT SULLE CONSEGNE ISOLA
226000011030IF  1C     WERR          IFEQ      'N'
226100011030IF  2C     VC(86)        IFEQ      'S'                                          *VOCE DA CALCOLARE
226200011030     C                   EXSR      CALISS
226300011030E   2C                   ENDIF
226400011030E   1C                   ENDIF
226500011030     C*
226600011030     C* CALCOLA ISTAT SULL'A.D.G. DELLE CONSEGNE ISOLA
226700011030IF  1C     WERR          IFEQ      'N'
226800011030IF  2C     VC(87)        IFEQ      'S'                                          *VOCE DA CALCOLARE
226900011030     C                   EXSR      CALIAS
227000011030E   2C                   ENDIF
227100011030E   1C                   ENDIF
227200011030     C*
227300011030     C* CALCOLA ESPRESSO SULLA CONSEGNA ISOLA
227400011030IF  1C     WERR          IFEQ      'N'
227500011030IF  2C     VC(81)        IFEQ      'S'                                          *VOCE DA CALCOLARE
227600011030     C                   EXSR      CALESS
227700011030E   2C                   ENDIF
227800011030E   1C                   ENDIF
227900011030     C*
228000011030     C* CALCOLA A.D.G. SULL'ESPRESSO CONSEGNA ISOLA
228100011030IF  1C     WERR          IFEQ      'N'
228200011030IF  2C     VC(82)        IFEQ      'S'                                          *VOCE DA CALCOLARE
228300011030     C                   EXSR      CALAES
228400011030E   2C                   ENDIF
228500011030E   1C                   ENDIF
228600011030     C*
228700011030     C* CALCOLA ISTAT SULL'ESPRESSO CONSEGNA ISOLA
228800011030IF  1C     WERR          IFEQ      'N'
228900011030IF  2C     VC(83)        IFEQ      'S'                                          *VOCE DA CALCOLARE
229000011030     C                   EXSR      CALIES
229100011030E   2C                   ENDIF
229200011030E   1C                   ENDIF
229300011030     C*
229400011030     C* CALCOLA ISTAT SULL'A.D.G. DELL'ESPRESSO CONSEGNA ISOLA
229500011030IF  1C     WERR          IFEQ      'N'
229600011030IF  2C     VC(84)        IFEQ      'S'                                          *VOCE DA CALCOLARE
229700011030     C                   EXSR      CAIAES
229800011030E   2C                   ENDIF
229900011030E   1C                   ENDIF
230000980702     C*---
230100011030     C* INOLTRO + LOCALITA' DISAGIATE + CONSEGNA ISOLA
230200980702     C*---
230300980702     C* RICALCOLA INOLTRO E VOCI CORRELATE RISPETTO AL FLAG APPICAZIONE TARIFFA INOLTRO
230400980702IF  1C     WERR          IFEQ      'N'
230500011030IF  2C     VC(35)        IFEQ      'S'                                          *L.DISAGIATA
230600011030     C     VC(80)        OREQ      'S'                                          *C.ISOLA
230700980702     C                   EXSR      CA3INL
230800980702E   2C                   ENDIF
230900980702E   1C                   ENDIF
231000980703     C*
231100980703     C* CALCOLA LA BASE PER IL CALCOLO DELLE TARIFFE CON % SUL TRASPORTO
231200980706     C                   EXSR      CALTBT
231300980702     C*---
231400980702     C* CONSEGNE PARTICOLARI
231500980702     C*---
231600970306     C* CALCOLA ASSICURAZIONE PER CONTO (FRANCO E ASSEGNATO)
231700970306IF  1C     WERR          IFEQ      'N'
231800970306IF  2C     VC(9)         IFEQ      'S'                                          *VOCE DA CALCOLARE
231900970306     C     VC(31)        OREQ      'S'                                          *VOCE DA CALCOLARE
232000970306     C                   EXSR      CALASC
232100001227     C                   EXSR      CAL048
232200001227     C                   EXSR      CAL065
232300001227     C                   EXSR      CAL060
232400001227     C                   EXSR      CAL077
232500970306E   2C                   ENDIF
232600970306E   1C                   ENDIF
232700970306     C*
232800970306     C* CALCOLA ASSICURAZIONE R.C.V. (FRANCO E ASSEGNATO)
232900970306IF  1C     WERR          IFEQ      'N'
233000970306IF  2C     VC(10)        IFEQ      'S'                                          *VOCE DA CALCOLARE
233100970306     C     VC(32)        OREQ      'S'                                          *VOCE DA CALCOLARE
233200970306     C                   EXSR      CALRCV
233300001227     C                   EXSR      CAL049
233400001227     C                   EXSR      CAL066
233500001227     C                   EXSR      CAL061
233600001227     C                   EXSR      CAL078
233700970306E   2C                   ENDIF
233800970306E   1C                   ENDIF
233900970306     C*
234000970306     C* CALCOLA CONSEGNA PARTICOLARE SU APPUNTAMENTO
234100970306IF  1C     WERR          IFEQ      'N'
234200970306IF  2C     VC(11)        IFEQ      'S'                                          *VOCE DA CALCOLARE
234300970306     C                   EXSR      CALCPA
234400001227     C                   EXSR      CAL050
234500001227     C                   EXSR      CAL067
234600970306E   2C                   ENDIF
234700970306E   1C                   ENDIF
234800970306     C*
234900970306     C* CALCOLA CONSEGNA PARTICOLARE AI SUPERMERCATI
235000970306IF  1C     WERR          IFEQ      'N'
235100970306IF  2C     VC(12)        IFEQ      'S'                                          *VOCE DA CALCOLARE
235200970306     C                   EXSR      CALCPS
235300001227     C                   EXSR      CAL051
235400001227     C                   EXSR      CAL068
235500970306E   2C                   ENDIF
235600970306E   1C                   ENDIF
235700970306     C*
235800970306     C* CALCOLA CONSEGNA PARTICOLARE AI PIANI
235900970306IF  1C     WERR          IFEQ      'N'
236000970306IF  2C     VC(13)        IFEQ      'S'                                          *VOCE DA CALCOLARE
236100970306     C                   EXSR      CALCPP
236200001227     C                   EXSR      CAL052
236300001227     C                   EXSR      CAL069
236400970306E   2C                   ENDIF
236500970306E   1C                   ENDIF
236600040826     C*
236700040826     C* CALCOLA CONSEGNA destinatario disagiato
236800040826IF  1C     WERR          IFEQ      'N'
236900040826IF  2C     VC(96)        IFEQ      'S'                                          *VOCE DA CALCOLARE
237000040826     C                   EXSR      CALdes
237100040826     C                   EXSR      CAL097
237200040826     C                   EXSR      CAL098
237300040826E   2C                   ENDIF
237400040826E   1C                   ENDIF
237500970306     C*
237600970306     C* CALCOLA CONSEGNA PARTICOLARE FUORI MISURA
237700970306IF  1C     WERR          IFEQ      'N'
237800970306IF  2C     VC(14)        IFEQ      'S'                                          *VOCE DA CALCOLARE
237900970306     C                   EXSR      CALCPF
238000001227     C                   EXSR      CAL053
238100001227     C                   EXSR      CAL070
238200970306E   2C                   ENDIF
238300970306E   1C                   ENDIF
238400970414     C*
238500970414     C* CALCOLA CONSEGNA PARTICOLARE DDT
238600970414IF  1C     WERR          IFEQ      'N'
238700970414IF  2C     VC(33)        IFEQ      'S'                                          *VOCE DA CALCOLARE
238800970414     C                   EXSR      CALCPD
238900001227     C                   EXSR      CAL062
239000001227     C                   EXSR      CAL079
239100970414E   2C                   ENDIF
239200970414E   1C                   ENDIF
239300980702     C*
239400980702     C* CALCOLA CONSEGNA PARTICOLARE RITIRO
239500980702IF  1C     WERR          IFEQ      'N'
239600980702IF  2C     VC(17)        IFEQ      'S'                                          *VOCE DA CALCOLARE
239700980702     C                   EXSR      CALRIT
239800001227     C                   EXSR      CAL056
239900001227     C                   EXSR      CAL073
240000980702E   2C                   ENDIF
240100980702E   1C                   ENDIF
240200980702     C*
240300980702     C* CALCOLA LA PROVVIGIONE ASSEGNO MITTENTE
240400980702IF  1C     WERR          IFEQ      'N'
240500980702IF  2C     VC(15)        IFEQ      'S'                                          *VOCE DA CALCOLARE
240600980702     C                   EXSR      CALPAM
240700001227     C                   EXSR      CAL054
240800001227     C                   EXSR      CAL071
240900980702E   2C                   ENDIF
241000980702E   1C                   ENDIF
241100980702     C*
241200980702     C* CALCOLA LA PROVVIGIONE ASSEGNO BARTOLINI
241300980702IF  1C     WERR          IFEQ      'N'
241400980702IF  2C     VC(16)        IFEQ      'S'                                          *VOCE DA CALCOLARE
241500980702     C                   EXSR      CALPAB
241600001227     C                   EXSR      CAL055
241700001227     C                   EXSR      CAL072
241800980702E   2C                   ENDIF
241900980702E   1C                   ENDIF
242000980702     C*
242100980702     C* CALCOLA IL DIRITTO FISSO
242200980702IF  1C     WERR          IFEQ      'N'
242300980702IF  2C     VC(2)         IFEQ      'S'                                          *VOCE DA CALCOLARE
242400980702     C                   EXSR      CALDIF
242500980702E   2C                   ENDIF
242600980702E   1C                   ENDIF
242700980702     C*
242800980702     C* CALCOLA A.D.G. SUL DIRITTO FISSO
242900980702IF  1C     WERR          IFEQ      'N'
243000980702IF  2C     VC(22)        IFEQ      'S'                                          *VOCE DA CALCOLARE
243100980702     C                   EXSR      CALAGD
243200980702E   2C                   ENDIF
243300980702E   1C                   ENDIF
243400980702     C*
243500980702     C* CALCOLA ISTAT SUL DIRITTO FISSO
243600980702IF  1C     WERR          IFEQ      'N'
243700980702IF  2C     VC(25)        IFEQ      'S'                                          *VOCE DA CALCOLARE
243800980702     C                   EXSR      CALISD
243900980702E   2C                   ENDIF
244000980702E   1C                   ENDIF
244100980702     C*
244200980702     C* CALCOLA ISTAT SULL'A.D.G. DEL DIRITTO FISSO
244300980702IF  1C     WERR          IFEQ      'N'
244400980702IF  2C     VC(28)        IFEQ      'S'                                          *VOCE DA CALCOLARE
244500980702     C                   EXSR      CALIAD
244600980702E   2C                   ENDIF
244700980702E   1C                   ENDIF
244800990819     C*
244900990819     C* CALCOLA T.C.P.
245000990819IF  1C     WERR          IFEQ      'N'
245100990819IF  2C     VC(5)         IFEQ      'S'                                          *VOCE DA CALCOLARE
245200990819     C                   EXSR      CALTCP
245300001222     C                   EXSR      CAL046
245400001227     C                   EXSR      CAL063
245500990819E   2C                   ENDIF
245600990819E   1C                   ENDIF
245700040826     C*
245800040826     C* CALCOLA bancali
245900040826IF  1C     WERR          IFEQ      'N'
246000040826IF  2C     VC(99)        IFEQ      'S'                                          *VOCE DA CALCOLARE
246100040826     C                   EXSR      CALban
246200040826     C                   EXSR      CAL100
246300040826     C                   EXSR      CAL101
246400040826E   2C                   ENDIF
246500040826E   1C                   ENDIF
246600040826     C*
246700040826     C* diritto di pesatura
246800040826IF  1C     WERR          IFEQ      'N'
246900040826IF  2C     §tatap        IFEQ      'S'                                          *VOCE DA CALCOLARE
247000040826     C                   EXSR      CALdps
247100040826     C                   EXSR      CAL103
247200040826     C                   EXSR      CAL104
247300040826E   2C                   ENDIF
247400040826E   1C                   ENDIF
247500970306     C*
247600970306     C                   ENDSR
247700980707     C*------------------------------------------------------------------------*
247800980707     C* CALPEI - CALCOLA PORTO E INOLTRO
247900980707     C*------------------------------------------------------------------------*
248000980707     C     CALPEI        BEGSR
248100980707     C*
248200011031     C                   Z-ADD     *ZEROS        TASPOR           21 8
248300011031     C                   Z-ADD     *ZEROS        TASINL           21 8
248400980707     C*****
248500980707     C** CALCOLO    P O R T O     ED    I N O L T R O
248600980707     C*****
248700980707     C                   DO        11            A
248800980707     C     A             OCCUR     TARDS
248900980707     C     TSCG          IFGE      PESTAS
249000980707     C                   GOTO      ENDTAS
249100980707     C                   END
249200980707     C                   END
249300980707     C** NON TROVATO SCAGLIONE ESATTO
249400980707     C     TSCG          IFLT      PESTAS
249500980707     C                   MOVEL     MSG(25)       $MSG
249600980707     C                   EXSR      IMPERR
249700980707     C                   GOTO      FINE
249800980707     C                   END
249900980707     C     ENDTAS        TAG
250000980707     C** MI SALVO LA POSIZIONE DELLO SCAGLIO TROVATO CHE MI
250100980707     C*  SERVE PER IL CALCOLO TARIFFA CON GARANTITO MAX SCAGLIONE PREC
250200980707     C     *LIKE         DEFINE    A             WA
250300980707     C                   Z-ADD     A             WA
250400980707     C**
250500980707     C* CALCOLO PER QUALE VALORE MOLTIPLICARE LA TARIFFA
250600980707     C                   Z-ADD     PESTAS        WPTAS3
250700980707     C                   EXSR      CALQLI
250800980707     C**
250900980707     C* MODO APPLICAZIONE TARIFFA
251000980707     C* CALCOLO NORMALE ANCHE PER SCALARE AL DI SOTTO
251100980707     C*  DELL'APPLICAZIONE TARIFFA FINITA E PER MAX SCAGLIONE PREC
251200980707     C     §TAF02        IFNE      'S'
251300980707     C     §TAF02        OREQ      'S'
251400980707     C     WATA          ANDNE     ' '
251500980707     C** MOLTIPLICAZIONE TARIFFA
251600980707     C                   EXSR      MULTAR
251700980707     C* APPLICAZIONE MINIMO E MASSIMO
251800980707     C                   EXSR      MINMAX
251900980707     C                   Z-ADD     WPOR          TASPOR
252000980707     C                   Z-ADD     WINL          TASINL
252100980707     C                   ENDIF
252200980707     C**
252300980707     C** S C A L A R E
252400980707     C     §TAF02        IFEQ      'S'
252500980707     C                   EXSR      SCALAR
252600980707     C                   ENDIF
252700980707     C**
252800980707     C** M A X   S C A G L I O N E   P R E C E D E N TE
252900980707     C     §TAF02        IFEQ      'G'
253000980707     C                   EXSR      MAXSCA
253100980707     C                   ENDIF
253200980707     C* PORTO
253300980707IF  1C     VC(1)         IFEQ      'S'                                          *VOCE DA CALCOLARE
253400980707IF  2C     TASPOR        IFGT      *ZEROS
253500980707     C                   Z-ADD     1             I
253600980707     C                   Z-ADD     TASPOR        VIMP(I)
253700980707E   2C                   ENDIF
253800980707E   1C                   ENDIF
253900980707     C* INOLTRO
254000980707IF  1C     VC(6)         IFEQ      'S'                                          *VOCE DA CALCOLARE
254100980707IF  2C     TASINL        IFGT      *ZEROS
254200980707     C                   Z-ADD     6             I
254300980707     C                   Z-ADD     TASINL        VIMP(I)
254400980707E   2C                   ENDIF
254500980707E   1C                   ENDIF
254600980707     C**
254700980707     C     FINE          TAG
254800980707     C                   ENDSR
254900980707     C*****************************************************************
255000980707     C** VALORE COL QUALE  MOLTIPLICARE LA TARIFFA
255100980707     C*****************************************************************
255200980707     C     CALQLI        BEGSR
255300980707     C                   Z-ADD     0             PESQLI           11 3
255400980707     C** SE LO SCAGLIONE SCELTO PER TASSARE E' INFERIORE AL MINIMO
255500980707     C** TASSABILE DA TARIFFA FORZO PESO TASSABILE = MINTAS
255600980707     C**
255700980707     C* WATA MI IDENTIFICA SE L'APPLICAZIONE TARIFFA FINITA E' DOVUTA
255800980707     C*  ALLO SCAGLIONE CONFRONTATO CON MINTAR (R)
255900980707     C*  O SE E' DOVUTA DAL PESO TASSABILE CONFRONTATO COL MINIMO
256000980707     C*  TASSABILE DI QUELLA TARIFFA (S) - differenza che ora non serve
256100980707     C*  + ma dal momento che c'e' la lascio
256200980707     C                   CLEAR                   WATA
256300980707    1C                   SELECT
256400980707     C     TSCG          WHENLE    MINTAR
256500980707     C                   Z-ADD     1             PESQLI
256600980707     C                   MOVEL     'R'           WATA              1
256700980707     C**
256800980707     C** SE PESO TASSABILE E' ANCORA INFERIORE
256900980707     C*  AL MINIMO TASSABILE DA TARIFFA IMPOSTO PESO TASS = 1
257000980707     C*  NON LO CONTROLLO PER LA TARIFFA A SCALARE PERCHE' ABBIAMO
257100980707     C*  SEMPRE INSERITO UNO SCAGLIONE=A MINIMO TASSABILE E
257200980707     C*  QUINDI E' GIA' CONTROLLATO NEL WHLE PRECEDENTE
257300980707     C     WPTAS3        WHENLE    MINTAS
257400980707     C     §TAF02        ANDNE     'S'
257500980707     C                   Z-ADD     1             PESQLI
257600980707     C                   MOVEL     'S'           WATA
257700980707   X1C                   OTHER
257800980707     C** CALCOLO : PESO IN Q.LI PER TARIFFA A PESO
257900980707     C** CALCOLO   VALORE/100   PER TARIFFA A VALORE (ESSENDO UNA %)
258000980707     C** PER LE ALTRE TARIFFE PESO TASSABILE INVARIATO
258100980707    2C     UNOCTR        IFEQ      0
258200980707     C     UNOCTR        OREQ      4
258300980707     C     WPTAS3        DIV       100           PESQLI
258400980707   X2C                   ELSE
258500980707     C                   Z-ADD     WPTAS3        PESQLI
258600980707    2C                   END
258700980707    1C                   ENDSL
258800980707     C                   ENDSR
258900980707     C*****************************************************************
259000980707     C* MOLTIPLICATO TARIFFA PER VALORE TROVATO (PESO TASSABILE)
259100980707     C*****************************************************************
259200980707     C     MULTAR        BEGSR
259300980707     C     *LIKE         DEFINE    TASPOR        WPOR
259400980707     C     *LIKE         DEFINE    TASINL        WINL
259500980707     C                   CLEAR                   WPOR
259600980707     C                   CLEAR                   WINL
259700980707     C**
259800980707     C     TITR          MULT(H)   PESQLI        WPOR                           ** PORTO
259900980707     C     TINO          MULT(H)   PESQLI        WINL                           ** INOLTRO
260000980707     C                   ENDSR
260100980707     C*****************************************************************
260200980707     C* CALCOLO TARIFFA A SCALARE
260300980707     C*****************************************************************
260400980707     C     SCALAR        BEGSR
260500980707     C**
260600980707     C     *LIKE         DEFINE    PESTAS        WPTAS1
260700980707     C     *LIKE         DEFINE    PESTAS        WDIFSC
260800980707     C     *LIKE         DEFINE    PESTAS        WPTAS3
260900980707     C     *LIKE         DEFINE    PESQLI        WPESQ
261000980707     C     *LIKE         DEFINE    PESQLI        WPESSV
261100980707     C     *LIKE         DEFINE    TITR          WTITR
261200980707     C     *LIKE         DEFINE    TITR          WSTITR
261300980707     C     *LIKE         DEFINE    TINO          WTINO
261400980707     C     *LIKE         DEFINE    TINO          WSTINO
261500980707     C     *LIKE         DEFINE    TSCG          WPRESC
261600980707     C**
261700980707     C                   CLEAR                   TASPOR
261800980707     C                   CLEAR                   TASINL
261900980707     C                   CLEAR                   WTROVA
262000980707     C                   CLEAR                   WPTAS3
262100980707     C                   CLEAR                   WDIFSC
262200980707     C                   CLEAR                   WPRESC
262300980707     C                   CLEAR                   WPESSV
262400980707     C                   CLEAR                   WPESQ
262500980707     C                   CLEAR                   WTITR
262600980707     C                   CLEAR                   WSTITR
262700980707     C                   CLEAR                   WTINO
262800980707     C                   CLEAR                   WSTINO
262900980707     C                   Z-ADD     PESTAS        WPTAS1
263000980707     C                   Z-ADD     1             A
263100980707     C**
263200980805    1C     A             DOWLE     11
263300980707     C     WTROVA        ANDEQ     ' '
263400980707     C     TSCG          ANDGT     0
263500980707     C     A             OCCUR     TARDS
263600980707     C**
263700980707     C* FACCIO LA DIFFERENZA TRA LO SCAGLIONE LETTO ED IL PRECEDENTE
263800980707     C     TSCG          SUB       WPRESC        WDIFSC
263900980707     C**
264000980707     C* SOTRAGGO DAL PESO TASSABILE RIMASTO LA PARTE DA TASSARE ORA
264100980707     C* IN WPTAS1 HO SEMPRE LA PARTE DI PESO TASSABILE ANCORA DA TASSAR
264200980707     C                   SUB       WDIFSC        WPTAS1
264300980707     C**
264400980707     C** SE IL VALORE E' DIVENTATO < DI 0 SIGNIFICA CHE HO TERMINATO LA
264500980707     C**  TASSAZIONE PER CUI USO SOLO LA PARTE RIMASTA DI WPTAS1
264600980707    2C     WPTAS1        IFLE      0
264700980707     C     WDIFSC        ADD       WPTAS1        WPTAS3
264800980707     C* PESO TASSABILE GLOBALE DELLA SPEDIZIONE
264900980707     C                   MOVEL     'S'           WTROVA            1
265000980707     C                   ELSE
265100980707     C**
265200980707     C* PARTE DI PESO DA TASSARE CON QUESTO SCAGLIONE
265300980707     C                   Z-ADD     WDIFSC        WPTAS3
265400980707     C* SALVO LO SCAGLIONE PRECEDENTE
265500980707     C                   Z-ADD     TSCG          WPRESC
265600980707    2C                   ENDIF
265700980707     C**
265800980707     C* IMPOSTO IL VALORE DA MOLTIPLICARE PER LA TARIFFA
265900980707     C                   EXSR      CALQLI
266000980707     C**
266100980707     C* SE LO SCAGLIONE E' APPLICAZIONE TARIFFA FINITA DEVO CONSIDERARE
266200980707     C*  SOLO L'ULTIMO SCAGLIONE COSI' E NON I PRECEDENTI
266300980707    2C     WATA          IFNE      ' '
266400980707     C     WTROVA        ANDEQ     ' '
266500980707     C* MI SALVO IL VALORE MA PER ORA NON LO MOLTIPLICO
266600980707     C                   Z-ADD     PESQLI        WPESQ
266700980707     C                   Z-ADD     TITR          WTITR
266800980707     C                   Z-ADD     TINO          WTINO
266900980707   X2C                   ELSE
267000980707     C**
267100980707     C* SE LO SCAGLIONE TROVATO E' L'ULTIMO MA COMUNQUE TARIFFA FINITA
267200980707     C* DEVO MOLTIPLICARE SOLO QUESTO --> AZZERO UN EVENTUALE PRECEDENT
267300980707     C*  CHE AVEVA LA TASSAZIONE TARIFFA FINITA
267400980707    3C     WTROVA        IFEQ      'S'
267500980707     C     WATA          ANDNE     ' '
267600980707     C                   CLEAR                   WPESQ
267700980707     C                   CLEAR                   WTITR
267800980707     C                   CLEAR                   WTINO
267900980707    3C                   ENDIF
268000980707     C*
268100980707     C* PRIMA DI MOLTIPLICARE LA TARIFFA TROVATA VEDO SE CE NE E'
268200980707     C*  UNA PRECEDENTE TARIFFA FINITA DA CONSIDERARE
268300980707    3C     WPESQ         IFGT      0
268400980707     C* SALVO PESO DA MOLTIPLICARE E TARIFFE DELLO SCAGLIONE
268500980707     C*  APPENA LETTO
268600980707     C                   Z-ADD     PESQLI        WPESSV
268700980707     C                   Z-ADD     TITR          WSTITR
268800980707     C                   Z-ADD     TINO          WSTINO
268900980707     C* IMPOSTO PESO DA MOLTIPLICARE E TARIFFE DELLO SCAGLIONE
269000980707     C*  PRECEDENTE PER IL CALCOLO
269100980707     C                   Z-ADD     WPESQ         PESQLI
269200980707     C                   Z-ADD     WTITR         TITR
269300980707     C                   Z-ADD     WTINO         TINO
269400980707     C                   EXSR      MULTAR
269500980707     C                   ADD       WPOR          TASPOR
269600980707     C                   ADD       WINL          TASINL
269700980707     C                   CLEAR                   WPESQ
269800980707     C                   CLEAR                   WTITR
269900980707     C                   CLEAR                   WTINO
270000980707     C* REIMPOSTO I DATI DELLO SCAGLIONE APPENA LETTO
270100980707     C                   Z-ADD     WPESSV        PESQLI
270200980707     C                   Z-ADD     WSTITR        TITR
270300980707     C                   Z-ADD     WSTINO        TINO
270400980707    3C                   ENDIF
270500980707     C**
270600980707     C* MOLTIPLICAZIONE TARIFFA
270700980707     C                   EXSR      MULTAR
270800980707     C                   ADD       WPOR          TASPOR
270900980707     C                   ADD       WINL          TASINL
271000980707    2C                   ENDIF
271100980707     C                   ADD       1             A
271200980707     C**
271300980707    1C                   ENDDO
271400980707     C**
271500980707     C* APPLICO MINIMO E MASSIMO
271600980707     C                   Z-ADD     TASPOR        WPOR
271700980707     C                   Z-ADD     TASINL        WINL
271800980707     C                   EXSR      MINMAX
271900980707     C                   Z-ADD     WPOR          TASPOR
272000980707     C                   Z-ADD     WINL          TASINL
272100980707     C                   ENDSR
272200980707     C*****************************************************************
272300980707     C* CALCOLO MAX SCAGLIONE PRECEDENTE
272400980707     C*****************************************************************
272500980707     C     MAXSCA        BEGSR
272600980707     C* SOLO SE LO SCAGLIONE APPLICATO NON ERA IL PRIMO
272700980707     C     WA            IFGT      1
272800980707     C* PER CONFRONTARLO CON IL VALORE NORMALE CALCOLATO
272900980707     C* IL CONFRONTO E' DATO DALLA SOMMA TRA PORT E INOLTRO
273000980707     C                   SUB       1             WA
273100980707     C     WA            OCCUR     TARDS
273200980707     C* CALCOLO PESO DA MOLTIPLICARE
273300980707     C                   Z-ADD     TSCG          WPTAS3
273400980707     C                   EXSR      CALQLI
273500980707     C** MOLTIPLICO LA TARIFFA
273600980707     C                   EXSR      MULTAR
273700980707     C** MINIMO E MASSIMO
273800980707     C                   EXSR      MINMAX
273900980707     C**
274000011018     C     TASPOR        ADD       TASINL        T0100            18 5
274100011018     C     WPOR          ADD       WINL          W0100            18 5
274200980707     C** PRENDO DELLE 2 LA TARIFFA PIU' ALTA
274300980707     C     W0100         IFGT      T0100
274400980707     C                   Z-ADD     WPOR          TASPOR
274500980707     C                   Z-ADD     WINL          TASINL
274600980707     C                   ENDIF
274700980707     C                   ENDIF
274800980707     C**
274900980707     C                   ENDSR
275000980707     C*****************************************************************
275100980707     C** MINIMO E MASSIMO DA APPLICARE -SE APPLICATO TUTTO NEL PORTO
275200980707     C*****************************************************************
275300980707     C     MINMAX        BEGSR
275400980707     C** NON ESISTE MINIMO E MASSIMO
275500980707     C     TMIN          IFEQ      0
275600980707     C     TMAX          ANDEQ     0
275700980707     C                   GOTO      DOPOMM
275800980707     C                   END
275900980707     C** MINIMO
276000011018     C     WPOR          ADD       WINL          WMAX             18 5
276100980707     C     TMIN          IFGT      WMAX
276200980707     C                   Z-ADD     TMIN          WPOR
276300980707     C                   Z-ADD     0             WINL
276400980707     C                   Z-ADD     TMIN          WMAX
276500980707     C                   END
276600980707     C** MASSIMO
276700980707     C     TMAX          IFGT      0
276800980707     C     WMAX          IFGT      TMAX
276900980707     C                   Z-ADD     TMAX          WPOR
277000980707     C                   Z-ADD     0             WINL
277100980707     C                   END
277200980707     C                   END
277300980707     C     DOPOMM        ENDSR
277400980706     C*------------------------------------------------------------------------*
277500980706     C* CALAGP - CALCOLA L'A.D.G. SUL PORTO
277600980706     C*------------------------------------------------------------------------*
277700980706     C     CALAGP        BEGSR
277800980706     C*
277900980706     C* REIMPOSTA LE VARIABILI DI LAVORO
278000011031     C                   Z-ADD     *ZEROS        TASADG           21 8
278100980706     C*
278200980706     C* CALCOLA L'A.D.G.
278300011031     C                   Z-ADD     TASPOR        WIMP             23 8
278400980706     C                   EXSR      ADGCAL
278500980706     C*
278600980706IF  1C     TASADG        IFGT      *ZEROS
278700980706     C                   Z-ADD     3             I
278800980706     C                   Z-ADD     TASADG        VIMP(I)
278900980706E   1C                   ENDIF
279000980706     C*
279100980706     C                   ENDSR
279200980706     C*------------------------------------------------------------------------*
279300980706     C* CALISP - CALCOLA L'ISTAT SUL PORTO
279400980706     C*------------------------------------------------------------------------*
279500980706     C     CALISP        BEGSR
279600980706     C*
279700980706     C* REIMPOSTA LE VARIABILI DI LAVORO
279800011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
279900980706     C*
280000980706     C* CALCOLA L'ISTAT
280100011031     C                   Z-ADD     TASPOR        WIMP             23 8
280200980706     C                   EXSR      IACCAL
280300980706     C*
280400980706IF  1C     TASIAC        IFGT      *ZEROS
280500980706     C                   Z-ADD     4             I
280600980706     C                   Z-ADD     TASIAC        VIMP(I)
280700980706E   1C                   ENDIF
280800980706     C*
280900980706     C                   ENDSR
281000980706     C*------------------------------------------------------------------------*
281100980706     C* CALIAP - CALCOLA L'ISTAT SULL'ADG DEL PORTO
281200980706     C*------------------------------------------------------------------------*
281300980706     C     CALIAP        BEGSR
281400980706     C*
281500980706     C* REIMPOSTA LE VARIABILI DI LAVORO
281600011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
281700980706     C*
281800980706     C* CALCOLA L'ISTAT
281900011031     C                   Z-ADD     VIMP(3)       WIMP             23 8
282000980706     C                   EXSR      IACCAL
282100980706     C*
282200980706IF  1C     TASIAC        IFGT      *ZEROS
282300980706     C                   Z-ADD     27            I
282400980706     C                   Z-ADD     TASIAC        VIMP(I)
282500980706E   1C                   ENDIF
282600980706     C*
282700980706     C                   ENDSR
282800970306     C*------------------------------------------------------------------------*
282900970717     C* CALESP - CALCOLA L'ESPRESSO PORTO
283000970306     C*------------------------------------------------------------------------*
283100970306     C     CALESP        BEGSR
283200970306     C*
283300970306     C* REIMPOSTA LE VARIABILI DI LAVORO
283400011031     C                   Z-ADD     *ZEROS        TASESP           21 8
283500970306     C*
283600970306     C* CALCOLA L'ESPRESSO
283700011031     C                   Z-ADD     TASPOR        IMPON            23 8
283800970306     C                   EXSR      CALTS
283900970306     C*
284000970314IF  1C     TASESP        IFGT      *ZEROS
284100970306     C                   Z-ADD     7             I
284200970306     C                   Z-ADD     TASESP        VIMP(I)
284300970314E   1C                   ENDIF
284400970306     C*
284500970306     C                   ENDSR
284600980706     C*------------------------------------------------------------------------*
284700980706     C* CALAEP - CALCOLA L'A.D.G. SULL'ESPRESSO PORTO
284800980706     C*------------------------------------------------------------------------*
284900980706     C     CALAEP        BEGSR
285000980706     C*
285100980706     C* REIMPOSTA LE VARIABILI DI LAVORO
285200011031     C                   Z-ADD     *ZEROS        TASADG           21 8
285300980706     C*
285400980706     C* CALCOLA L'A.D.G.
285500011031     C                   Z-ADD     VIMP(7)       WIMP             23 8
285600980706     C                   EXSR      ADGCAL
285700980706     C*
285800980706IF  1C     TASADG        IFGT      *ZEROS
285900980706     C                   Z-ADD     23            I
286000980706     C                   Z-ADD     TASADG        VIMP(I)
286100980706E   1C                   ENDIF
286200980706     C*
286300980706     C                   ENDSR
286400980706     C*------------------------------------------------------------------------*
286500980706     C* CALIEP - CALCOLA L'ISTAT SULL'ESPRESSO PORTO
286600980706     C*------------------------------------------------------------------------*
286700980706     C     CALIEP        BEGSR
286800980706     C*
286900980706     C* REIMPOSTA LE VARIABILI DI LAVORO
287000011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
287100980706     C*
287200980706     C* CALCOLA L'ISTAT
287300011031     C                   Z-ADD     VIMP(7)       WIMP             23 8
287400980706     C                   EXSR      IACCAL
287500980706     C*
287600980706IF  1C     TASIAC        IFGT      *ZEROS
287700980706     C                   Z-ADD     26            I
287800980706     C                   Z-ADD     TASIAC        VIMP(I)
287900980706E   1C                   ENDIF
288000980706     C*
288100980706     C                   ENDSR
288200980706     C*------------------------------------------------------------------------*
288300980706     C* CAIAEP - CALCOLA L'ISTAT SULL'ADG DELL'ESPRESSO PORTO
288400980706     C*------------------------------------------------------------------------*
288500980706     C     CAIAEP        BEGSR
288600980706     C*
288700980706     C* REIMPOSTA LE VARIABILI DI LAVORO
288800011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
288900980706     C*
289000980706     C* CALCOLA L'ISTAT
289100011031     C                   Z-ADD     VIMP(23)      WIMP             23 8
289200980706     C                   EXSR      IACCAL
289300980706     C*
289400980706IF  1C     TASIAC        IFGT      *ZEROS
289500980706     C                   Z-ADD     30            I
289600980706     C                   Z-ADD     TASIAC        VIMP(I)
289700980706E   1C                   ENDIF
289800980706     C*
289900980706     C                   ENDSR
290000980706     C*------------------------------------------------------------------------*
290100980706     C* CALAGI - CALCOLA L'A.D.G. SULL'INOLTRO
290200980706     C*------------------------------------------------------------------------*
290300980706     C     CALAGI        BEGSR
290400980706     C*
290500980706     C* REIMPOSTA LE VARIABILI DI LAVORO
290600011031     C                   Z-ADD     *ZEROS        TASADG           21 8
290700980706     C*
290800980706     C* CALCOLA L'A.D.G.
290900011031     C                   Z-ADD     TASINL        WIMP             23 8
291000980706     C                   EXSR      ADGCAL
291100980706     C*
291200980706IF  1C     TASADG        IFGT      *ZEROS
291300980706     C                   Z-ADD     21            I
291400980706     C                   Z-ADD     TASADG        VIMP(I)
291500980706E   1C                   ENDIF
291600980706     C*
291700980706     C                   ENDSR
291800980706     C*------------------------------------------------------------------------*
291900980706     C* CALISI - CALCOLA L'ISTAT SULL'INOLTRO
292000980706     C*------------------------------------------------------------------------*
292100980706     C     CALISI        BEGSR
292200980706     C*
292300980706     C* REIMPOSTA LE VARIABILI DI LAVORO
292400011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
292500980706     C*
292600980706     C* CALCOLA L'ISTAT
292700011031     C                   Z-ADD     TASINL        WIMP             23 8
292800980706     C                   EXSR      IACCAL
292900980706     C*
293000980706IF  1C     TASIAC        IFGT      *ZEROS
293100980706     C                   Z-ADD     24            I
293200980706     C                   Z-ADD     TASIAC        VIMP(I)
293300980706E   1C                   ENDIF
293400980706     C*
293500980706     C                   ENDSR
293600980706     C*------------------------------------------------------------------------*
293700980706     C* CALIAI - CALCOLA L'ISTAT SULL'ADG DELL'INOLTRO
293800980706     C*------------------------------------------------------------------------*
293900980706     C     CALIAI        BEGSR
294000980706     C*
294100980706     C* REIMPOSTA LE VARIABILI DI LAVORO
294200011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
294300980706     C*
294400980706     C* CALCOLA L'ISTAT
294500011031     C                   Z-ADD     VIMP(21)      WIMP             23 8
294600980706     C                   EXSR      IACCAL
294700980706     C*
294800980706IF  1C     TASIAC        IFGT      *ZEROS
294900980706     C                   Z-ADD     29            I
295000980706     C                   Z-ADD     TASIAC        VIMP(I)
295100980706E   1C                   ENDIF
295200980706     C*
295300980706     C                   ENDSR
295400970717     C*------------------------------------------------------------------------*
295500970717     C* CALESI - CALCOLA L'ESPRESSO INOLTRO
295600970717     C*------------------------------------------------------------------------*
295700970717     C     CALESI        BEGSR
295800970717     C*
295900970717     C* REIMPOSTA LE VARIABILI DI LAVORO
296000011031     C                   Z-ADD     *ZEROS        TASESP           21 8
296100970717     C*
296200970717     C* CALCOLA L'ESPRESSO
296300011031     C                   Z-ADD     TASINL        IMPON            23 8
296400970717     C                   EXSR      CALTS
296500970717     C*
296600970717IF  1C     TASESP        IFGT      *ZEROS
296700970717     C                   Z-ADD     36            I
296800970717     C                   Z-ADD     TASESP        VIMP(I)
296900970717E   1C                   ENDIF
297000970717     C*
297100970717     C                   ENDSR
297200980706     C*------------------------------------------------------------------------*
297300980706     C* CALAEI - CALCOLA L'A.D.G. SULL'ESPRESSO INOLTRO
297400980706     C*------------------------------------------------------------------------*
297500980706     C     CALAEI        BEGSR
297600980706     C*
297700980706     C* REIMPOSTA LE VARIABILI DI LAVORO
297800011031     C                   Z-ADD     *ZEROS        TASADG           21 8
297900980706     C*
298000980706     C* CALCOLA L'A.D.G.
298100011031     C                   Z-ADD     VIMP(36)      WIMP             23 8
298200980706     C                   EXSR      ADGCAL
298300980706     C*
298400980706IF  1C     TASADG        IFGT      *ZEROS
298500980706     C                   Z-ADD     37            I
298600980706     C                   Z-ADD     TASADG        VIMP(I)
298700980706E   1C                   ENDIF
298800980706     C*
298900980706     C                   ENDSR
299000980706     C*------------------------------------------------------------------------*
299100980706     C* CALIEI - CALCOLA L'ISTAT SULL'ESPRESSO INOLTRO
299200980706     C*------------------------------------------------------------------------*
299300980706     C     CALIEI        BEGSR
299400980706     C*
299500980706     C* REIMPOSTA LE VARIABILI DI LAVORO
299600011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
299700980706     C*
299800980706     C* CALCOLA L'ISTAT
299900011031     C                   Z-ADD     VIMP(36)      WIMP             23 8
300000980706     C                   EXSR      IACCAL
300100980706     C*
300200980706IF  1C     TASIAC        IFGT      *ZEROS
300300980706     C                   Z-ADD     38            I
300400980706     C                   Z-ADD     TASIAC        VIMP(I)
300500980706E   1C                   ENDIF
300600980706     C*
300700980706     C                   ENDSR
300800980706     C*------------------------------------------------------------------------*
300900980706     C* CAIAEI - CALCOLA L'ISTAT SULL'ADG DELL'ESPRESSO INOLTRO
301000980706     C*------------------------------------------------------------------------*
301100980706     C     CAIAEI        BEGSR
301200980706     C*
301300980706     C* REIMPOSTA LE VARIABILI DI LAVORO
301400011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
301500980706     C*
301600980706     C* CALCOLA L'ISTAT
301700011031     C                   Z-ADD     VIMP(37)      WIMP             23 8
301800980706     C                   EXSR      IACCAL
301900980706     C*
302000980706IF  1C     TASIAC        IFGT      *ZEROS
302100980706     C                   Z-ADD     39            I
302200980706     C                   Z-ADD     TASIAC        VIMP(I)
302300980706E   1C                   ENDIF
302400980706     C*
302500980706     C                   ENDSR
302600980706     C*------------------------------------------------------------------------*
302700980706     C* CALCPL - CALCOLA CONSEGNA PARTICOLARE LOCALITA' DISAGIATE
302800980706     C*------------------------------------------------------------------------*
302900980706     C     CALCPL        BEGSR
303000980706     C*
303100980706     C* REIMPOSTA LE VARIABILI DI LAVORO
303200980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
303300011031     C                   Z-ADD     *ZEROS        TASCPL           21 8
303400980706     C*
303500980706     C* CALCOLA CONSEGNA PARTICOLARE
303600980706     C                   MOVEL     'K'           TASTC             1            *CODICE CONSEGNA
303700011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
303800980706     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
303900980706     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
304000980706     C                   EXSR      CALCP
304100980706     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
304200980706     C                   Z-ADD     TASCP         TASCPL
304300980706     C*
304400980706IF  1C     TASCPL        IFGT      *ZEROS
304500980706     C                   Z-ADD     35            I
304600980706     C                   Z-ADD     TASCPL        VIMP(I)
304700980706E   1C                   ENDIF
304800980706     C*
304900980706     C* MEMORIZZA IL FLAG "APPLICAZIONE TARIFFA INOLTRO" DELLO SCAGLIONE DEL
305000980706     C* CODICE TASSAZIONE DEL DETTAGLIO TARIFFARIO
305100980706     C                   MOVEL     DEPAIN        CPLAIN
305200980706     C*
305300980706     C                   ENDSR
305400980706     C*------------------------------------------------------------------------*
305500980706     C* CALAGL - CALCOLA L'A.D.G. SULLE CONSEGNE LOCALITA' DISAGIATE
305600980706     C*------------------------------------------------------------------------*
305700980706     C     CALAGL        BEGSR
305800980706     C*
305900980706     C* REIMPOSTA LE VARIABILI DI LAVORO
306000011031     C                   Z-ADD     *ZEROS        TASADG           21 8
306100980706     C*
306200980706     C* CALCOLA L'A.D.G.
306300011031     C                   Z-ADD     TASCPL        WIMP             23 8
306400980706     C                   EXSR      ADGCAL
306500980706     C*
306600980706IF  1C     TASADG        IFGT      *ZEROS
306700980706     C                   Z-ADD     43            I
306800980706     C                   Z-ADD     TASADG        VIMP(I)
306900980706E   1C                   ENDIF
307000980706     C*
307100980706     C                   ENDSR
307200980706     C*------------------------------------------------------------------------*
307300980706     C* CALISL - CALCOLA L'ISTAT SULLE CONSEGNE LOCALITA' DISAGIATE
307400980706     C*------------------------------------------------------------------------*
307500980706     C     CALISL        BEGSR
307600980706     C*
307700980706     C* REIMPOSTA LE VARIABILI DI LAVORO
307800011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
307900980706     C*
308000980706     C* CALCOLA L'ISTAT
308100011031     C                   Z-ADD     TASCPL        WIMP             23 8
308200980706     C                   EXSR      IACCAL
308300980706     C*
308400980706IF  1C     TASIAC        IFGT      *ZEROS
308500980706     C                   Z-ADD     44            I
308600980706     C                   Z-ADD     TASIAC        VIMP(I)
308700980706E   1C                   ENDIF
308800980706     C*
308900980706     C                   ENDSR
309000980706     C*------------------------------------------------------------------------*
309100980706     C* CALIAL - CALCOLA L'ISTAT SULL'ADG DELLE CONEGNE LOCALITA' DISAGIATE
309200980706     C*------------------------------------------------------------------------*
309300980706     C     CALIAL        BEGSR
309400980706     C*
309500980706     C* REIMPOSTA LE VARIABILI DI LAVORO
309600011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
309700980706     C*
309800980706     C* CALCOLA L'ISTAT
309900011031     C                   Z-ADD     VIMP(43)      WIMP             23 8
310000980706     C                   EXSR      IACCAL
310100980706     C*
310200980706IF  1C     TASIAC        IFGT      *ZEROS
310300980706     C                   Z-ADD     45            I
310400980706     C                   Z-ADD     TASIAC        VIMP(I)
310500980706E   1C                   ENDIF
310600980706     C*
310700980706     C                   ENDSR
310800980706     C*------------------------------------------------------------------------*
310900980706     C* CALESL - CALCOLA L'ESPRESSO SULLA CONSEGNA LOCALITA' DISAGIATE
311000980706     C*------------------------------------------------------------------------*
311100980706     C     CALESL        BEGSR
311200980706     C*
311300980706     C* REIMPOSTA LE VARIABILI DI LAVORO
311400011031     C                   Z-ADD     *ZEROS        TASESP           21 8
311500980706     C*
311600980706     C* CALCOLA L'ESPRESSO
311700011031     C                   Z-ADD     VIMP(35)      IMPON            23 8
311800980706     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
311900980706     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
312000980706     C                   EXSR      CALTS
312100980706     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
312200980706     C*
312300980706IF  1C     TASESP        IFGT      *ZEROS
312400980706     C                   Z-ADD     39            I
312500980706     C                   Z-ADD     TASESP        VIMP(I)
312600980706E   1C                   ENDIF
312700980706     C*
312800980706     C                   ENDSR
312900980706     C*------------------------------------------------------------------------*
313000980706     C* CALAEL - CALCOLA L'A.D.G. SULL'ESPRESSO CONSEGNA LOCALITA' DISAGIATE
313100980706     C*------------------------------------------------------------------------*
313200980706     C     CALAEL        BEGSR
313300980706     C*
313400980706     C* REIMPOSTA LE VARIABILI DI LAVORO
313500011031     C                   Z-ADD     *ZEROS        TASADG           21 8
313600980706     C*
313700980706     C* CALCOLA L'A.D.G.
313800011031     C                   Z-ADD     VIMP(39)      WIMP             23 8
313900980706     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
314000980706     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
314100980706     C                   EXSR      ADGCAL
314200980706     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
314300980706     C*
314400980706IF  1C     TASADG        IFGT      *ZEROS
314500980706     C                   Z-ADD     40            I
314600980706     C                   Z-ADD     TASADG        VIMP(I)
314700980706E   1C                   ENDIF
314800980706     C*
314900980706     C                   ENDSR
315000980706     C*------------------------------------------------------------------------*
315100980706     C* CALIEL - CALCOLA L'ISTAT SULL'ESPRESSO CONSEGNA LOCALITA' DISAGIATE
315200980706     C*------------------------------------------------------------------------*
315300980706     C     CALIEL        BEGSR
315400980706     C*
315500980706     C* REIMPOSTA LE VARIABILI DI LAVORO
315600011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
315700980706     C*
315800980706     C* CALCOLA L'ISTAT
315900011031     C                   Z-ADD     VIMP(39)      WIMP             23 8
316000980706     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
316100980706     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
316200980706     C                   EXSR      IACCAL
316300980706     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
316400980706     C*
316500980706IF  1C     TASIAC        IFGT      *ZEROS
316600980706     C                   Z-ADD     41            I
316700980706     C                   Z-ADD     TASIAC        VIMP(I)
316800980706E   1C                   ENDIF
316900980706     C*
317000980706     C                   ENDSR
317100980706     C*------------------------------------------------------------------------*
317200980706     C* CAIAEL - CALCOLA L'ISTAT SULL'ADG DELL'ESPRESSO CONSEGNA L.DISAGIATE
317300980706     C*------------------------------------------------------------------------*
317400980706     C     CAIAEL        BEGSR
317500980706     C*
317600980706     C* REIMPOSTA LE VARIABILI DI LAVORO
317700011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
317800980706     C*
317900980706     C* CALCOLA L'ISTAT
318000011031     C                   Z-ADD     VIMP(40)      WIMP             23 8
318100980706     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
318200980706     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
318300980706     C                   EXSR      IACCAL
318400980706     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
318500980706     C*
318600980706IF  1C     TASIAC        IFGT      *ZEROS
318700980706     C                   Z-ADD     42            I
318800980706     C                   Z-ADD     TASIAC        VIMP(I)
318900980706E   1C                   ENDIF
319000980706     C*
319100980706     C                   ENDSR
319200011031     C*------------------------------------------------------------------------*
319300011031     C* CALSAM - CALCOLA RITIRO ISOLA
319400011031     C*------------------------------------------------------------------------*
319500011031     C     CALSAM        BEGSR
319600011031     C*
319700011031     C* REIMPOSTA LE VARIABILI DI LAVORO
319800011031     C                   MOVEL     'S'           WCAR                           *SI UTILIZZO TAR.CAR
319900011031     C                   Z-ADD     *ZEROS        TASSAM           21 8
320000011031     C*
320100011031     C* CALCOLA RITIRO COME CONSEGNA PARTICOLARE ISOLA
320200011031     C                   MOVEL     'I'           TASTC             1            *CODICE CONSEGNA
320300011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
320400011031     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
320500011031     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
320600011031     C                   EXSR      CALCP
320700011031     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
320800011031     C                   Z-ADD     TASCP         TASCIS
320900011031     C*
321000011031IF  1C     TASSAM        IFGT      *ZEROS
321100011031     C                   Z-ADD     88            I
321200011031     C                   Z-ADD     TASSAM        VIMP(I)
321300011031E   1C                   ENDIF
321400011031     C*
321500011031     C* MEMORIZZA IL FLAG "APPLICAZIONE TARIFFA INOLTRO" DELLO SCAGLIONE DEL
321600011031     C* CODICE TASSAZIONE DEL DETTAGLIO TARIFFARIO -NON SERVE, MA TENIAMOLO-
321700011031     C                   MOVEL     DEPAIN        SAMAIN
321800011031     C*
321900011031     C                   ENDSR
322000011031     C*------------------------------------------------------------------------*
322100011031     C* CALAGR - CALCOLA L'A.D.G. SUL RITIRO ISOLA
322200011031     C*------------------------------------------------------------------------*
322300011031     C     CALAGR        BEGSR
322400011031     C*
322500011031     C* REIMPOSTA LE VARIABILI DI LAVORO
322600011031     C                   Z-ADD     *ZEROS        TASADG           21 8
322700011031     C*
322800011031     C* CALCOLA L'A.D.G.
322900011031     C                   Z-ADD     TASSAM        WIMP             23 8
323000011031     C                   EXSR      ADGCAL
323100011031     C*
323200011031IF  1C     TASADG        IFGT      *ZEROS
323300011031     C                   Z-ADD     93            I
323400011031     C                   Z-ADD     TASADG        VIMP(I)
323500011031E   1C                   ENDIF
323600011031     C*
323700011031     C                   ENDSR
323800011031     C*------------------------------------------------------------------------*
323900011031     C* CALISR - CALCOLA L'ISTAT SUL RITIRO ISOLA
324000011031     C*------------------------------------------------------------------------*
324100011031     C     CALISR        BEGSR
324200011031     C*
324300011031     C* REIMPOSTA LE VARIABILI DI LAVORO
324400011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
324500011031     C*
324600011031     C* CALCOLA L'ISTAT
324700011031     C                   Z-ADD     TASSAM        WIMP             23 8
324800011031     C                   EXSR      IACCAL
324900011031     C*
325000011031IF  1C     TASIAC        IFGT      *ZEROS
325100011031     C                   Z-ADD     94            I
325200011031     C                   Z-ADD     TASIAC        VIMP(I)
325300011031E   1C                   ENDIF
325400011031     C*
325500011031     C                   ENDSR
325600011031     C*------------------------------------------------------------------------*
325700011031     C* CALIAR - CALCOLA L'ISTAT SULL'ADG DEL RITIRO ISOLA
325800011031     C*------------------------------------------------------------------------*
325900011031     C     CALIAR        BEGSR
326000011031     C*
326100011031     C* REIMPOSTA LE VARIABILI DI LAVORO
326200011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
326300011031     C*
326400011031     C* CALCOLA L'ISTAT
326500011031     C                   Z-ADD     VIMP(93)      WIMP             23 8
326600011031     C                   EXSR      IACCAL
326700011031     C*
326800011031IF  1C     TASIAC        IFGT      *ZEROS
326900011031     C                   Z-ADD     95            I
327000011031     C                   Z-ADD     TASIAC        VIMP(I)
327100011031E   1C                   ENDIF
327200011031     C*
327300011031     C                   ENDSR
327400011031     C*------------------------------------------------------------------------*
327500011031     C* CALESR - CALCOLA L'ESPRESSO SUL RITIRO ISOLA
327600011031     C*------------------------------------------------------------------------*
327700011031     C     CALESR        BEGSR
327800011031     C*
327900011031     C* REIMPOSTA LE VARIABILI DI LAVORO
328000011031     C                   Z-ADD     *ZEROS        TASESP           21 8
328100011031     C*
328200011031     C* CALCOLA L'ESPRESSO
328300011031     C                   Z-ADD     VIMP(88)      IMPON            23 8
328400011031     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
328500011031     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
328600011031     C                   EXSR      CALTS
328700011031     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
328800011031     C*
328900011031IF  1C     TASESP        IFGT      *ZEROS
329000011031     C                   Z-ADD     89            I
329100011031     C                   Z-ADD     TASESP        VIMP(I)
329200011031E   1C                   ENDIF
329300011031     C*
329400011031     C                   ENDSR
329500011031     C*------------------------------------------------------------------------*
329600011031     C* CALAER - CALCOLA L'A.D.G. SULL'ESPRESSO RITIRO ISOLA
329700011031     C*------------------------------------------------------------------------*
329800011031     C     CALAER        BEGSR
329900011031     C*
330000011031     C* REIMPOSTA LE VARIABILI DI LAVORO
330100011031     C                   Z-ADD     *ZEROS        TASADG           21 8
330200011031     C*
330300011031     C* CALCOLA L'A.D.G.
330400011031     C                   Z-ADD     VIMP(89)      WIMP             23 8
330500011031     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
330600011031     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
330700011031     C                   EXSR      ADGCAL
330800011031     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
330900011031     C*
331000011031IF  1C     TASADG        IFGT      *ZEROS
331100011031     C                   Z-ADD     90            I
331200011031     C                   Z-ADD     TASADG        VIMP(I)
331300011031E   1C                   ENDIF
331400011031     C*
331500011031     C                   ENDSR
331600011031     C*------------------------------------------------------------------------*
331700011031     C* CALIER - CALCOLA L'ISTAT SULL'ESPRESSO RITIRO ISOLA
331800011031     C*------------------------------------------------------------------------*
331900011031     C     CALIER        BEGSR
332000011031     C*
332100011031     C* REIMPOSTA LE VARIABILI DI LAVORO
332200011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
332300011031     C*
332400011031     C* CALCOLA L'ISTAT
332500011031     C                   Z-ADD     VIMP(89)      WIMP             23 8
332600011031     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
332700011031     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
332800011031     C                   EXSR      IACCAL
332900011031     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
333000011031     C*
333100011031IF  1C     TASIAC        IFGT      *ZEROS
333200011031     C                   Z-ADD     91            I
333300011031     C                   Z-ADD     TASIAC        VIMP(I)
333400011031E   1C                   ENDIF
333500011031     C*
333600011031     C                   ENDSR
333700011031     C*------------------------------------------------------------------------*
333800011031     C* CAIAER - CALCOLA L'ISTAT SULL'ADG DELL'ESPRESSO RITIRO ISOLA
333900011031     C*------------------------------------------------------------------------*
334000011031     C     CAIAER        BEGSR
334100011031     C*
334200011031     C* REIMPOSTA LE VARIABILI DI LAVORO
334300011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
334400011031     C*
334500011031     C* CALCOLA L'ISTAT
334600011031     C                   Z-ADD     VIMP(90)      WIMP             23 8
334700011031     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
334800011031     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
334900011031     C                   EXSR      IACCAL
335000011031     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
335100011031     C*
335200011031IF  1C     TASIAC        IFGT      *ZEROS
335300011031     C                   Z-ADD     92            I
335400011031     C                   Z-ADD     TASIAC        VIMP(I)
335500011031E   1C                   ENDIF
335600011031     C*
335700011031     C                   ENDSR
335800011030     C*------------------------------------------------------------------------*
335900011030     C* CALCIS - CALCOLA CONSEGNA PARTICOLARE ISOLA
336000011030     C*------------------------------------------------------------------------*
336100011030     C     CALCIS        BEGSR
336200011030     C*
336300011030     C* REIMPOSTA LE VARIABILI DI LAVORO
336400011030     C                   MOVEL     'S'           WCAR                           *SI UTILIZZO TAR.CAR
336500011031     C                   Z-ADD     *ZEROS        TASCIS           21 8
336600011030     C*
336700011030     C* CALCOLA CONSEGNA PARTICOLARE
336800011030     C                   MOVEL     'I'           TASTC             1            *CODICE CONSEGNA
336900011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
337000011030     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
337100011030     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
337200011030     C                   EXSR      CALCP
337300011030     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
337400011030     C                   Z-ADD     TASCP         TASCIS
337500011030     C*
337600011030IF  1C     TASCIS        IFGT      *ZEROS
337700011030     C                   Z-ADD     80            I
337800011030     C                   Z-ADD     TASCIS        VIMP(I)
337900011030E   1C                   ENDIF
338000011030     C*
338100011030     C* MEMORIZZA IL FLAG "APPLICAZIONE TARIFFA INOLTRO" DELLO SCAGLIONE DEL
338200011030     C* CODICE TASSAZIONE DEL DETTAGLIO TARIFFARIO
338300011030     C                   MOVEL     DEPAIN        CISAIN
338400011030     C*
338500011030     C                   ENDSR
338600011030     C*------------------------------------------------------------------------*
338700011030     C* CALAGS - CALCOLA L'A.D.G. SULLE CONSEGNE ISOLA
338800011030     C*------------------------------------------------------------------------*
338900011030     C     CALAGS        BEGSR
339000011030     C*
339100011030     C* REIMPOSTA LE VARIABILI DI LAVORO
339200011031     C                   Z-ADD     *ZEROS        TASADG           21 8
339300011030     C*
339400011030     C* CALCOLA L'A.D.G.
339500011031     C                   Z-ADD     TASCIS        WIMP             23 8
339600011030     C                   EXSR      ADGCAL
339700011030     C*
339800011030IF  1C     TASADG        IFGT      *ZEROS
339900011030     C                   Z-ADD     85            I
340000011030     C                   Z-ADD     TASADG        VIMP(I)
340100011030E   1C                   ENDIF
340200011030     C*
340300011030     C                   ENDSR
340400011030     C*------------------------------------------------------------------------*
340500011030     C* CALISS - CALCOLA L'ISTAT SULLE CONSEGNE ISOLA
340600011030     C*------------------------------------------------------------------------*
340700011030     C     CALISS        BEGSR
340800011030     C*
340900011030     C* REIMPOSTA LE VARIABILI DI LAVORO
341000011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
341100011030     C*
341200011030     C* CALCOLA L'ISTAT
341300011031     C                   Z-ADD     TASCIS        WIMP             23 8
341400011030     C                   EXSR      IACCAL
341500011030     C*
341600011030IF  1C     TASIAC        IFGT      *ZEROS
341700011030     C                   Z-ADD     86            I
341800011030     C                   Z-ADD     TASIAC        VIMP(I)
341900011030E   1C                   ENDIF
342000011030     C*
342100011030     C                   ENDSR
342200011030     C*------------------------------------------------------------------------*
342300011030     C* CALIAS - CALCOLA L'ISTAT SULL'ADG DELLE CONEGNE ISOLA
342400011030     C*------------------------------------------------------------------------*
342500011030     C     CALIAS        BEGSR
342600011030     C*
342700011030     C* REIMPOSTA LE VARIABILI DI LAVORO
342800011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
342900011030     C*
343000011030     C* CALCOLA L'ISTAT
343100011031     C                   Z-ADD     VIMP(85)      WIMP             23 8
343200011030     C                   EXSR      IACCAL
343300011030     C*
343400011030IF  1C     TASIAC        IFGT      *ZEROS
343500011030     C                   Z-ADD     87            I
343600011030     C                   Z-ADD     TASIAC        VIMP(I)
343700011030E   1C                   ENDIF
343800011030     C*
343900011030     C                   ENDSR
344000011030     C*------------------------------------------------------------------------*
344100011030     C* CALESS - CALCOLA L'ESPRESSO SULLA CONSEGNA ISOLA
344200011030     C*------------------------------------------------------------------------*
344300011030     C     CALESS        BEGSR
344400011030     C*
344500011030     C* REIMPOSTA LE VARIABILI DI LAVORO
344600011031     C                   Z-ADD     *ZEROS        TASESP           21 8
344700011030     C*
344800011030     C* CALCOLA L'ESPRESSO
344900011031     C                   Z-ADD     VIMP(80)      IMPON            23 8
345000011030     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
345100011030     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
345200011030     C                   EXSR      CALTS
345300011030     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
345400011030     C*
345500011030IF  1C     TASESP        IFGT      *ZEROS
345600011030     C                   Z-ADD     81            I
345700011030     C                   Z-ADD     TASESP        VIMP(I)
345800011030E   1C                   ENDIF
345900011030     C*
346000011030     C                   ENDSR
346100011030     C*------------------------------------------------------------------------*
346200011030     C* CALAES - CALCOLA L'A.D.G. SULL'ESPRESSO CONSEGNA ISOLA
346300011030     C*------------------------------------------------------------------------*
346400011030     C     CALAES        BEGSR
346500011030     C*
346600011030     C* REIMPOSTA LE VARIABILI DI LAVORO
346700011031     C                   Z-ADD     *ZEROS        TASADG           21 8
346800011030     C*
346900011030     C* CALCOLA L'A.D.G.
347000011031     C                   Z-ADD     VIMP(81)      WIMP             23 8
347100011030     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
347200011030     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
347300011030     C                   EXSR      ADGCAL
347400011030     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
347500011030     C*
347600011030IF  1C     TASADG        IFGT      *ZEROS
347700011030     C                   Z-ADD     82            I
347800011030     C                   Z-ADD     TASADG        VIMP(I)
347900011030E   1C                   ENDIF
348000011030     C*
348100011030     C                   ENDSR
348200011030     C*------------------------------------------------------------------------*
348300011030     C* CALIEL - CALCOLA L'ISTAT SULL'ESPRESSO CONSEGNA ISOLA
348400011030     C*------------------------------------------------------------------------*
348500011030     C     CALIES        BEGSR
348600011030     C*
348700011030     C* REIMPOSTA LE VARIABILI DI LAVORO
348800011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
348900011030     C*
349000011030     C* CALCOLA L'ISTAT
349100011031     C                   Z-ADD     VIMP(81)      WIMP             23 8
349200011030     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
349300011030     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
349400011030     C                   EXSR      IACCAL
349500011030     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
349600011030     C*
349700011030IF  1C     TASIAC        IFGT      *ZEROS
349800011030     C                   Z-ADD     83            I
349900011030     C                   Z-ADD     TASIAC        VIMP(I)
350000011030E   1C                   ENDIF
350100011030     C*
350200011030     C                   ENDSR
350300011030     C*------------------------------------------------------------------------*
350400011030     C* CAIAES - CALCOLA L'ISTAT SULL'ADG DELL'ESPRESSO CONSEGNA ISOLA
350500011030     C*------------------------------------------------------------------------*
350600011030     C     CAIAES        BEGSR
350700011030     C*
350800011030     C* REIMPOSTA LE VARIABILI DI LAVORO
350900011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
351000011030     C*
351100011030     C* CALCOLA L'ISTAT
351200011031     C                   Z-ADD     VIMP(82)      WIMP             23 8
351300011030     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
351400011030     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
351500011030     C                   EXSR      IACCAL
351600011030     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
351700011030     C*
351800011030IF  1C     TASIAC        IFGT      *ZEROS
351900011030     C                   Z-ADD     84            I
352000011030     C                   Z-ADD     TASIAC        VIMP(I)
352100011030E   1C                   ENDIF
352200011030     C*
352300011030     C                   ENDSR
352400980706     C*------------------------------------------------------------------------*
352500011030     C* CA3INL - RICALCOLA INOLTRO X LOCALITA' DISAGIATE / CONSEGNA ISOLA
352600980706     C*------------------------------------------------------------------------*
352700980706     C     CA3INL        BEGSR
352800980706     C*---
352900011030     C* FLAG='S' -> PAGA TARIFFA INOLTRO + TARIFFA LOCALITA' DISAGIATA  + ISOLA
353000011030     C* FLAG=' ' -> CANCELLA L'INOLTRO SE LA L.DISAGIATA o ISOLA E' DA PAGARE
353100011030     C*
353200011030     C* NOTE: In tassazione una bolla può essere O Località disagiata O Consegna isola.
353300011030     C*       Nel CAT l'azzeramento dell'inoltro scatta anche se SOLO UNA delle due
353400011030     C*       consegne prevede l'azzeramento e quindi può esistere l'incongruenza
353500011030     C*       che si abbia l'inoltro azzerato a fronte del fatto che UNA delle due consegne
353600011030     C*       non preveda l'azzeramento. (Mha!, avrete capito???)
353700011030     C*---
353800011030     c***
353900011030     c* LOCALITA' DISAGIATE / ISOLA
354000011030     c***
354100011030IF  1C     CPLAIN        IFEQ      *BLANKS
354200011030     C     CISAIN        OREQ      *BLANKS
354300980706     C*
354400980706     C* AZZERA LE VOCI INOLTRO
354500011030IF  2C     VIMP(35)      IFGT      *ZEROS                                       *L.DISAG.DA PAGARE
354600011030     C     VIMP(80)      ORGT      *ZEROS                                       *C.ISOLA DA PAGARE
354700980706DO  3C     1             DO        NVOCA         V                 6 0
354800980706IF  4C     VIMP(V)       IFGT      *ZEROS
354900980706IF  5C     V             IFEQ      006
355000980706     C     V             OREQ      021
355100980706     C     V             OREQ      024
355200980706     C     V             OREQ      029
355300980706     C     V             OREQ      034
355400980706     C     V             OREQ      036
355500980706     C     V             OREQ      037
355600980706     C     V             OREQ      038
355700980706     C                   Z-ADD     *ZEROS        VIMP(V)
355800980706E   5C                   ENDIF
355900980706E   4C                   ENDIF
356000980706E   3C                   ENDDO
356100980706E   2C                   ENDIF
356200980706E   1C                   ENDIF
356300980706     C*
356400980706     C                   ENDSR
356500980706     C*------------------------------------------------------------------------*
356600980706     C* CALTBT - CALCOLA LA BASE PER IL CALCOLO DELLE TARIFFE CON % SUL TRASPORTO
356700980706     C*------------------------------------------------------------------------*
356800980706     C     CALTBT        BEGSR
356900980706     C*
357000980706     C* REIMPOSTA LE VARIABILI DI LAVORO
357100011031     C                   Z-ADD     *ZEROS        TOTTRA           23 8
357200980706     C*
357300011030     C* PORTO + INOLTRO + LOCALITA' DISAGIATE + ISOLA + LORO ESPRESSI
357400980706     C     VIMP(1)       ADD       TOTTRA        TOTTRA                         *PORTO
357500980706     C     VIMP(6)       ADD       TOTTRA        TOTTRA                         *INOLTRO
357600980706     C     VIMP(35)      ADD       TOTTRA        TOTTRA                         *LOCALITA' DISAGIATE
357700980706     C     VIMP(7)       ADD       TOTTRA        TOTTRA                         *ESPRESSO PORTO
357800980706     C     VIMP(36)      ADD       TOTTRA        TOTTRA                         *ESPRESSO INOLTRO
357900980706     C     VIMP(39)      ADD       TOTTRA        TOTTRA                         *ESPRESSO L.DISAGIAT
358000011030     C     VIMP(80)      ADD       TOTTRA        TOTTRA                         *CONSEGNA ISOLA
358100011030     C     VIMP(81)      ADD       TOTTRA        TOTTRA                         *ESPRESSO C.ISOLA
358200980706     C*
358300980706     C                   ENDSR
358400970306     C*------------------------------------------------------------------------*
358500970306     C* CALTS  - CALCOLA TIPO SERVIZIO ESPRESSO
358600970306     C*------------------------------------------------------------------------*
358700980706     C     CALTS         BEGSR
358800970306     C*
358900980506     C* REIMPOSTA LE VARIABILI DI LAVORO
359000980506     C                   MOVEL     'S'           WCAR              1            *SI UTILIZZO TAR.CAR
359100020912     C                   MOVEL     'E'           WTSP              1              ESPRESSO='E'
359200980506     C*
359300970306     C* REPERISCE IL CODICE DELLA PARTICOLARITA' ESPRESSO
359400970306     C                   SETOFF                                           28
359500970306     C                   Z-ADD     1             I
359600980506     C     WTSP          LOOKUP    TS(I)                                  28
359700970306IF  1C     *IN28         IFEQ      *ON                                          *SERVIZIO ESISTENTE
359800980506     C     I             OCCUR     DS5E
359900980506IF  2C     §5EFTC        IFNE      *BLANKS                                      *SERVIZIO CON PARTIC
360000980506IF  3C     TAMTSP        IFEQ      WTSP
360100980804     C                   MOVEL     'N'           WCAR                           *NO UTILIZZO TAR.CAR
360200980506E   3C                   ENDIF
360300980506     C*
360400980506     C* CERCA LA CONSEGNA PARTICOLARE
360500980506     C                   MOVEL     §5EFTC        TASTC
360600980506     C                   EXSR      CERTPT
360700980506     C*
360800980506     C* CALCOLA RPV, ARROTONDAMENTI E PESO TASSABILE
360900980506IF  3C     $TPT          IFEQ      '1'                                          *TARIFFA TROVATA
361000980707     C                   MOVEL     TPTTPG        WTPG
361100980707     C                   MOVEL     TPTRPV        WRPV
361200980707     C                   CLEAR                   WDET
361300980707     C                   Z-ADD     TPTARL        WARL
361400980707     C                   Z-ADD     TPTARF        WARF
361500980707     C                   Z-ADD     TPTARO        WARO
361600980506     C                   EXSR      CALCOL
361700980506IF  4C     TPTTPG        IFEQ      'T'                                          *A TRASPORTO
361800980506     C                   Z-ADD     IMPON         ISOPES                         *PORTO
361900980506E   4C                   ENDIF
362000970306     C*
362100970306     C* CALCOLA LA TARIFFA ESPRESSO
362200980506     C                   Z-ADD     ISOPES        TPDPES                         *PESO
362300970306     C                   EXSR      CALACD
362400970306     C                   Z-ADD     VARIA         TASESP
362500970306E   3C                   ENDIF
362600970306E   2C                   ENDIF
362700970306E   1C                   ENDIF
362800970306     C*
362900970306     C                   ENDSR
363000970306     C*------------------------------------------------------------------------*
363100970306     C* CALAGD - CALCOLA L'A.D.G. SUL DIRITTO FISSO
363200970306     C*------------------------------------------------------------------------*
363300970306     C     CALAGD        BEGSR
363400970306     C*
363500970306     C* REIMPOSTA LE VARIABILI DI LAVORO
363600011031     C                   Z-ADD     *ZEROS        TASADG           21 8
363700970306     C*
363800970306     C* CALCOLA L'A.D.G.
363900011031     C                   Z-ADD     TASDIF        WIMP             23 8
364000970306     C                   EXSR      ADGCAL
364100970306     C*
364200970314IF  1C     TASADG        IFGT      *ZEROS
364300970306     C                   Z-ADD     22            I
364400970306     C                   Z-ADD     TASADG        VIMP(I)
364500970314E   1C                   ENDIF
364600970306     C*
364700970306     C                   ENDSR
364800970306     C*------------------------------------------------------------------------*
364900970306     C* ADGCAL - CALCOLA A.D.G. SU IMPORTO GENERICO
365000970306     C*------------------------------------------------------------------------*
365100970306     C     ADGCAL        BEGSR
365200970306     C*
365300970306IF  1C     TAMPAG        IFNE      *ZEROS                                       *INDICATA IN TARIFFA
365400011031     C     WIMP          MULT      TAMPAG        COMADG           23 8
365500970306IF  2C     COMADG        IFGT      *ZEROS
365600970306     C     COMADG        DIV(H)    100           TASADG
365700970306E   2C                   ENDIF
365800970306E   1C                   ENDIF
365900970306     C*
366000970306     C                   ENDSR
366100980706     C*------------------------------------------------------------------------*
366200980706     C* IACCAL - CALCOLA ISTAT SU IMPORTO GENERICO
366300980706     C*------------------------------------------------------------------------*
366400980706     C     IACCAL        BEGSR
366500980706     C*
366600980706     C* REIMPOSTA LE VARIABILI DI LAVORO
366700980706     C                   Z-ADD     *ZEROS        PI                3 0
366800980706     C                   Z-ADD     *ZEROS        PUNIAC            3 1
366900011031     C                   Z-ADD     *ZEROS        PERIAC           23 8
367000011031     C                   Z-ADD     *ZEROS        PERIAX           23 8
367100980706     C*
367200980706     C* CALCOLA PUNTI ISTAT DA APPLICARE
367300980706IF  1C     TAMFLI        IFGT      *ZEROS                                       *INDICATO IN TARIFFA
367400980706     C                   Z-ADD     TAMFLI        PI
367500980706     C                   SUB       1             PI
367600980706DO  2C     I             DO        12            I                              *SONO IN RELTA' MAX
367700980706IF  3C     PI            IFLE      8
367800980706     C                   ADD       1             PI
367900980706X   3C                   ELSE
368000980706     C                   LEAVE
368100980706E   3C                   ENDIF
368200980706IF  3C     D00DTC        IFGE      DIA(PI)
368300980706     C                   ADD       IAC(PI)       PUNIAC
368400980706X   3C                   ELSE
368500980706     C                   LEAVE
368600980706E   3C                   ENDIF
368700980706E   2C                   ENDDO
368800980706     C* CALCOLA L'ISTAT
368900980706     C     PUNIAC        MULT      TAMPRI        PERIAX
369000980706IF  2C     PERIAX        IFNE      *ZEROS
369100980706     C     PERIAX        DIV       100           PERIAC
369200980706E   2C                   ENDIF
369300980706IF  2C     PERIAC        IFNE      *ZEROS
369400980706     C     WIMP          MULT(H)   PERIAC        TASIAC
369500980706E   2C                   ENDIF
369600980706E   1C                   ENDIF
369700980706     C*
369800980706     C                   ENDSR
369900970306     C*------------------------------------------------------------------------*
370000970306     C* CALASC - CALCOLA ASSICURAZIONE PER CONTO
370100970306     C*------------------------------------------------------------------------*
370200970306     C     CALASC        BEGSR
370300970306     C*
370400970306     C* REIMPOSTA LE VARIABILI DI LAVORO
370500011031     C                   Z-ADD     *ZEROS        TASASF           21 8
370600011031     C                   Z-ADD     *ZEROS        TASASA           21 8
370700980506     C*---
370800980506     C* FRANCO
370900980506     C*---
371000980506     C                   MOVEL     'F'           WTPO
371100970306     C                   EXSR      CALVR
371200970306IF  1C     VC(9)         IFEQ      'S'                                          *VOCE DA CALCOLARE
371300970314     C     TASASF        ANDGT     *ZEROS
371400970306     C                   Z-ADD     9             I
371500970306     C                   Z-ADD     TASASF        VIMP(I)
371600970306E   1C                   ENDIF
371700980506     C*---
371800980506     C* ASSEGNATO
371900980506     C*---
372000980506     C                   MOVEL     'A'           WTPO
372100980506     C                   EXSR      CALVR
372200970306IF  1C     VC(31)        IFEQ      'S'                                          *VOCE DA CALCOLARE
372300970314     C     TASASA        ANDGT     *ZEROS
372400970306     C                   Z-ADD     31            I
372500970306     C                   Z-ADD     TASASA        VIMP(I)
372600970306E   1C                   ENDIF
372700970306     C*
372800970306     C                   ENDSR
372900970306     C*------------------------------------------------------------------------*
373000011023     C* CALVR - CALCOLA CONSEGNA PARTICOLARE ASSICURAZIONE PER CONTO
373100970306     C*------------------------------------------------------------------------*
373200970306     C     CALVR         BEGSR
373300970306     C*
373400980506     C* NB: L'IMPORTO DA ASSICURARE NEI DATI TIPO E' OBBLIGATORIO: TOGLIENDO LA
373500980506     C*     OBBLIGATORIETA' E' NECESSARIO GESTIRE IL MANCA TARIFFA PER MANDATI
373600980702     C*     A VALORE VARIABILE CON IMPORTO UGUALE A ZERO.
373700980506     C*
373800980506     C* REIMPOSTA VARIABILI DI LAVORO
373900011031     C                   Z-ADD     ETCASC        IASLIT           23 8
374000970306     C*
374100970306     C* SE CLIENTE 88 CALCOLA TARIFFA SE ESISTE IL VALORE MERCE NEI DATI TIPO
374200970306     C                   MOVEL     D00KSC        WA4               4 0
374300970306IF  1C     WA4           IFEQ      8888
374400970418     C     ETCASC        ANDGT     *ZEROS
374500970306     C     WA4           ORNE      8888
374600970306     C                   Z-ADD     1             I
374700970306     C                   SETOFF                                           28
374800970306     C                   MOVEL     'C'           KTPFTC
374900990819     C                   MOVEL     KTPFTC        A1
375000990819     C     A1            LOOKUP    CP(I)                                  28
375100970306IF  2C     *IN28         IFEQ      *ON                                          *C.PART.ASS.ESISTENT
375200970306     C     I             OCCUR     CPIDS                                        *TARIFFA DI CARTELLO
375300970306     C*
375400970306     C* RICERCA L'ASSICURAZIONE PER CONTO NELLE CONSEGNE PARTICOLARI DEL CLIENTE
375500970306     C                   Z-ADD     D00KSC        KTPKSC
375600970306     C                   Z-ADD     D00CTR        KTPCTR
375700970306     C                   Z-ADD     D00PRG        KTPPRG
375800011019     C                   MOVEL     D00DIV        WDIV
375900970306     C                   MOVEL     '0'           $TPT                           *TARIFFA NON TROVATA
376000990819     C     KEYTPT        SETLL     TITPT01L                               99
376100990819     C     KEYTPT        READE     TITPT01L                               99
376200970306DO  3C     *IN99         DOWEQ     *OFF
376300991125     C     $TPT          ANDEQ     '0'
376400991125     C*
376500991125     C* CONTROLLA SE CONVERTIRE I CAMPI DI TITPT
376600011026IF  4C     WDIV          IFNE      WDIVCE
376700011019     C                   MOVEL     WDIV          WDVI
376800991125     C                   EXSR      CNVTPT
376900991125E   4C                   ENDIF
377000991125     C*
377100970306IF  4C     TPTATB        IFEQ      *BLANKS
377200980506IF  5C     TPTAPL        IFEQ      *BLANKS                                      *APPLICAZIONE ENTRAM
377300980506     C     TPTAPL        OREQ      'A'                                          *APPLICAZIONE ARRIVO
377400980506     C     WTPO          ANDEQ     'A'                                          *ASSEGNATI O
377500980506     C     TPTAPL        OREQ      'P'                                          *APPLICAZIONE PARTEN
377600980506     C     WTPO          ANDEQ     'F'                                          *FRANCHI
377700970306     C                   MOVEL     '1'           $TPT                           *TARIFFA TROVATA
377800970314     C*
377900970314     C* USA IL VALORE MERCE IN TARIFFA SE C'E' ALTRIMENTI QUELLO DEI DATI TIPO
378000980506IF  6C     TPTVLM        IFGT      *ZEROS                                       *VALORE IN TARIFFA
378100980506IF  7C     TPTFVM        IFEQ      '1'                                          *TARIFFA A COLLI
378200980506     C     SCSC(S)       MULT      TPTVLM        IASLIT
378300980506E   7C                   ENDIF
378400980506IF  7C     TPTFVM        IFEQ      '2'                                          *TARIFFA A SPEDIZION
378500980506     C                   Z-ADD     TPTVLM        IASLIT
378600980506E   7C                   ENDIF
378700980506IF  7C     TPTFVM        IFEQ      '3'                                          *TARIFFA A PESO
378800980506     C     SPSC(S)       MULT      TPTVLM        IASLIT
378900980506E   7C                   ENDIF
379000980506X   6C                   ELSE                                                   *NO VALORE IN TARIFF
379100970314     C                   MOVEL     '2'           TPTFVM                         *TARIFFA A SPEDIZION
379200980506E   6C                   ENDIF
379300980506E   5C                   ENDIF
379400980506E   4C                   ENDIF
379500970306     C*
379600970306     C* LETTURA SUCESSIVA TARIFFE
379700970306IF  4C     $TPT          IFEQ      '0'                                          *TARIFFA NON TROVATA
379800990819     C     KEYTPT        READE     TITPT01L                               99
379900970306E   4C                   ENDIF
380000970306E   3C                   ENDDO                                                  *FINE TAR.PART.KSC
380100970306     C*
380200970306     C* TARIFFA ASSICURAZIONE NON TROVATA NELLE CONSEGNE PARTICOLARI DEL CLIENTE
380300970306     C* RICERCA NELLE CONSEGNE PARTICOLARI DI CARTELLO
380400970306IF  3C     $TPT          IFEQ      '0'                                          *TARIFFA KSC NO TROV
380500970306     C     CP88          ANDNE     'NO'
380600970306     C                   MOVEL     '1'           $TPT                           *TARIFFA TROVATA
380700970306     C                   Z-ADD     CARKSC        TPTKSC                         *CHIAVE CARTELLO
380800970306     C                   Z-ADD     CARCTR        TPTCTR
380900970306     C                   Z-ADD     CARPRG        TPTPRG
381000970306     C                   MOVEL     KTPFTC        TPTFTC
381100970306     C                   MOVEL     TTPG8P        TPTTPG                         *CAMPI CARTELLO
381200970306     C                   Z-ADD     TARF8P        TPTARF
381300970306     C                   Z-ADD     TARL8P        TPTARL
381400970306     C                   Z-ADD     TARO8P        TPTARO
381500970306     C                   Z-ADD     TRPV8P        TPTRPV
381600970306     C                   MOVEL     TAPL8P        TPTAPL
381700970306E   3C                   ENDIF
381800970306     C*
381900980506     C* CALCOLA RPV, ARROTONDAMENTI E PESO TASSABILE
382000970306IF  3C     $TPT          IFEQ      '1'                                          *TARIFFA TROVATA
382100980707     C                   MOVEL     TPTTPG        WTPG
382200980707     C                   MOVEL     TPTRPV        WRPV
382300980707     C                   CLEAR                   WDET
382400980707     C                   Z-ADD     TPTARL        WARL
382500980707     C                   Z-ADD     TPTARF        WARF
382600980707     C                   Z-ADD     TPTARO        WARO
382700980506     C                   EXSR      CALCOL
382800980506IF  4C     TPTTPG        IFEQ      'V'                                          *CALCOLO A VALORE
382900980506     C                   Z-ADD     IASLIT        ISOPES                         *IMPORTO DA ASSICURA
383000970306E   4C                   ENDIF
383100980703IF  4C     TPTTPG        IFEQ      'T'                                          *A TRASPORTO
383200980703     C                   Z-ADD     TOTTRA        ISOPES                         *TRASPORTO
383300980703E   4C                   ENDIF
383400970306     C*
383500970306     C* CALCOLA LA TARIFFA ASSICURAZIONE PER CONTO
383600011031     C                   Z-ADD     ISOPES        TPDPES           23 8          *PESO
383700970306     C                   EXSR      CALACD
383800980706     C                   EXSR      CALTRA                                       *CALCOLA % A TRASPOR
383900980506IF  4C     WTPO          IFEQ      'A'
384000980506     C                   Z-ADD     VARIA         TASASA                         *ASSIC.NE ASSEGNATO
384100980506X   4C                   ELSE
384200980506     C                   Z-ADD     VARIA         TASASF                         *ASSIC.NE FRANCO
384300970306E   4C                   ENDIF
384400970306E   3C                   ENDIF
384500970306E   2C                   ENDIF
384600970306E   1C                   ENDIF
384700970306     C*
384800970306     C                   ENDSR
384900001227     C*------------------------------------------------------------------------*
385000001227     C* CAL048 - CALCOLA ADG SU ASSIC X CONTO FRANCO
385100001227     C*------------------------------------------------------------------------*
385200001227     C     CAL048        BEGSR
385300001227     C*
385400001227     C* REIMPOSTA LE VARIABILI DI LAVORO
385500011031     C                   Z-ADD     *ZEROS        TASADG           21 8
385600001227     C*
385700001227     C* CALCOLA L'A.D.G.
385800011031     C                   Z-ADD     VIMP(9)       WIMP             23 8
385900001227     C                   EXSR      ADGCAL
386000001227     C*
386100001227IF  1C     TASADG        IFGT      *ZEROS
386200001227     C                   Z-ADD     48            I
386300001227     C                   Z-ADD     TASADG        VIMP(I)
386400001227E   1C                   ENDIF
386500001227     C*
386600001227     C                   ENDSR
386700001227     C*------------------------------------------------------------------------*
386800001227     C* CAL065 - CALCOLA ISTAT SU ADG/ASS X CONTO FRANCO
386900001227     C*------------------------------------------------------------------------*
387000001227     C     CAL065        BEGSR
387100001227     C*
387200001227     C* REIMPOSTA LE VARIABILI DI LAVORO
387300011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
387400001227     C*
387500001227     C* CALCOLA L'ISTAT
387600011031     C                   Z-ADD     VIMP(48)      WIMP             23 8
387700001227     C                   EXSR      IACCAL
387800001227     C*
387900001227IF  1C     TASIAC        IFGT      *ZEROS
388000001227     C                   Z-ADD     65            I
388100001227     C                   Z-ADD     TASIAC        VIMP(I)
388200001227E   1C                   ENDIF
388300001227     C*
388400001227     C                   ENDSR
388500001227     C*------------------------------------------------------------------------*
388600001227     C* CAL060 - CALCOLA ADG SU ASSIC X CONTO ASSEGNATO
388700001227     C*------------------------------------------------------------------------*
388800001227     C     CAL060        BEGSR
388900001227     C*
389000001227     C* REIMPOSTA LE VARIABILI DI LAVORO
389100011031     C                   Z-ADD     *ZEROS        TASADG           21 8
389200001227     C*
389300001227     C* CALCOLA L'A.D.G.
389400011031     C                   Z-ADD     VIMP(31)      WIMP             23 8
389500001227     C                   EXSR      ADGCAL
389600001227     C*
389700001227IF  1C     TASADG        IFGT      *ZEROS
389800001227     C                   Z-ADD     60            I
389900001227     C                   Z-ADD     TASADG        VIMP(I)
390000001227E   1C                   ENDIF
390100001227     C*
390200001227     C                   ENDSR
390300001227     C*------------------------------------------------------------------------*
390400001227     C* CAL077 - CALCOLA ISTAT SU ADG/ASS X CONTO ASSEGNATO
390500001227     C*------------------------------------------------------------------------*
390600001227     C     CAL077        BEGSR
390700001227     C*
390800001227     C* REIMPOSTA LE VARIABILI DI LAVORO
390900011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
391000001227     C*
391100001227     C* CALCOLA L'ISTAT
391200011031     C                   Z-ADD     VIMP(60)      WIMP             23 8
391300001227     C                   EXSR      IACCAL
391400001227     C*
391500001227IF  1C     TASIAC        IFGT      *ZEROS
391600001227     C                   Z-ADD     77            I
391700001227     C                   Z-ADD     TASIAC        VIMP(I)
391800001227E   1C                   ENDIF
391900001227     C*
392000001227     C                   ENDSR
392100970306     C*------------------------------------------------------------------------*
392200970306     C* CALRCV - CALCOLA ASSICURAZIONE R.C.V.
392300970306     C*------------------------------------------------------------------------*
392400970306     C     CALRCV        BEGSR
392500970306     C*
392600970306     C* REIMPOSTA LE VARIABILI DI LAVORO
392700011031     C                   Z-ADD     *ZEROS        TASRCF           21 8
392800011031     C                   Z-ADD     *ZEROS        TASRCA           21 8
392900980506     C*---
393000980506     C* FRANCO
393100980506     C*---
393200980506     C                   MOVEL     'F'           WTPO
393300970306     C                   EXSR      CALVE
393400970306IF  1C     VC(10)        IFEQ      'S'                                          *VOCE DA CALCOLARE
393500970314     C     TASRCF        ANDGT     *ZEROS
393600970306     C                   Z-ADD     10            I
393700970306     C                   Z-ADD     TASRCF        VIMP(I)
393800970306E   1C                   ENDIF
393900980506     C*---
394000980506     C* ASSEGNATO
394100980506     C*---
394200980707     C                   MOVEL     'A'           WTPO              1
394300980506     C                   EXSR      CALVE
394400970306IF  1C     VC(32)        IFEQ      'S'                                          *VOCE DA CALCOLARE
394500970314     C     TASRCA        ANDGT     *ZEROS
394600970306     C                   Z-ADD     32            I
394700970306     C                   Z-ADD     TASRCA        VIMP(I)
394800970306E   1C                   ENDIF
394900970306     C*
395000970306     C                   ENDSR
395100970306     C*------------------------------------------------------------------------*
395200970306     C* CALVE - CALCOLA CONSEGNA PARTICOLARE ASSICURAZIONE R.C.V.
395300970306     C*------------------------------------------------------------------------*
395400970306     C     CALVE         BEGSR
395500980804     C*
395600980804     C* REIMPOSTA LE VARIABILI DI LAVORO
395700980804     C                   MOVEL     'N'           WCAR                           *NO UTILIZZO TAR.CAR
395800980506     C*
395900980506     C* CERCA TARIFFA PARTICOLARE
396000980506     C                   MOVEL     'R'           TASTC             1            *CODICE CONSEGNA
396100980506     C                   EXSR      CERTPT
396200970306     C*
396300980506     C* CALCOLA RPV, ARROTONDAMENTI E PESO TASSABILE
396400980506IF  1C     $TPT          IFEQ      '1'                                          *TARIFFA TROVATA
396500980707     C                   MOVEL     TPTTPG        WTPG
396600980707     C                   MOVEL     TPTRPV        WRPV
396700980707     C                   CLEAR                   WDET
396800980707     C                   Z-ADD     TPTARL        WARL
396900980707     C                   Z-ADD     TPTARF        WARF
397000980707     C                   Z-ADD     TPTARO        WARO
397100980506     C                   EXSR      CALCOL
397200980506IF  2C     TPTTPG        IFEQ      'V'                                          *A VALORE
397300040716IF  3C     TPTVLM        IFGT      §gelrp
397400040716     C     TPTVLM        SUB       §gelrp        ISOPES
397500980506     C     ISOPES        MULT      SPSC(S)       ISOPES
397600980506E   3C                   ENDIF
397700980506E   2C                   ENDIF
397800980703IF  2C     TPTTPG        IFEQ      'T'                                          *A TRASPORTO
397900980703     C                   Z-ADD     TOTTRA        ISOPES                         *TRASPORTO
398000980703E   2C                   ENDIF
398100970306     C*
398200970306     C* CALCOLA LA TARIFFA ASSICURAZIONE R.C.V.
398300980506     C                   Z-ADD     ISOPES        TPDPES                         *PESO
398400970306     C                   EXSR      CALACD
398500980706     C                   EXSR      CALTRA                                       *CALCOLA % A TRASPOR
398600970306     C*---
398700970306     C* TIPO APPLICAZIONE TARIFFA:
398800980506     C* PARTENZA -> RICAVO SOLO PER FRANCHI
398900980506     C* ARRIVO   -> RICAVO SOLO PER ASSEGNATI
399000980506     C* BLANKS   -> RICAVO PER ENTRAMBI
399100970306     C*---
399200980506IF  2C     WTPO          IFEQ      'F'
399300980506IF  3C     TPTAPL        IFEQ      *BLANKS
399400980506     C     TPTAPL        OREQ      'P'
399500980506     C                   Z-ADD     VARIA         TASRCF                         *RICAVO FRANCO
399600980506E   3C                   ENDIF
399700980506X   2C                   ELSE
399800980506IF  3C     TPTAPL        IFEQ      *BLANKS
399900980506     C     TPTAPL        OREQ      'A'
400000980506     C                   Z-ADD     VARIA         TASRCA                         *RICAVO ASSEGNAO
400100980506E   3C                   ENDIF
400200980506E   2C                   ENDIF
400300980506E   1C                   ENDIF
400400970306     C*
400500970306     C                   ENDSR
400600001227     C*------------------------------------------------------------------------*
400700001227     C* CAL049 - CALCOLA ADG SU ASSIC RCV  FRANCO
400800001227     C*------------------------------------------------------------------------*
400900001227     C     CAL049        BEGSR
401000001227     C*
401100001227     C* REIMPOSTA LE VARIABILI DI LAVORO
401200011031     C                   Z-ADD     *ZEROS        TASADG           21 8
401300001227     C*
401400001227     C* CALCOLA L'A.D.G.
401500011031     C                   Z-ADD     VIMP(10)      WIMP             23 8
401600001227     C                   EXSR      ADGCAL
401700001227     C*
401800001227IF  1C     TASADG        IFGT      *ZEROS
401900001227     C                   Z-ADD     49            I
402000001227     C                   Z-ADD     TASADG        VIMP(I)
402100001227E   1C                   ENDIF
402200001227     C*
402300001227     C                   ENDSR
402400001227     C*------------------------------------------------------------------------*
402500001227     C* CAL066 - CALCOLA ISTAT SU ADG/ASS RCV  FRANCO
402600001227     C*------------------------------------------------------------------------*
402700001227     C     CAL066        BEGSR
402800001227     C*
402900001227     C* REIMPOSTA LE VARIABILI DI LAVORO
403000011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
403100001227     C*
403200001227     C* CALCOLA L'ISTAT
403300011031     C                   Z-ADD     VIMP(49)      WIMP             23 8
403400001227     C                   EXSR      IACCAL
403500001227     C*
403600001227IF  1C     TASIAC        IFGT      *ZEROS
403700001227     C                   Z-ADD     66            I
403800001227     C                   Z-ADD     TASIAC        VIMP(I)
403900001227E   1C                   ENDIF
404000001227     C*
404100001227     C                   ENDSR
404200001227     C*------------------------------------------------------------------------*
404300001227     C* CAL061 - CALCOLA ADG SU ASSIC RCV  ASSEGNATO
404400001227     C*------------------------------------------------------------------------*
404500001227     C     CAL061        BEGSR
404600001227     C*
404700001227     C* REIMPOSTA LE VARIABILI DI LAVORO
404800011031     C                   Z-ADD     *ZEROS        TASADG           21 8
404900001227     C*
405000001227     C* CALCOLA L'A.D.G.
405100011031     C                   Z-ADD     VIMP(32)      WIMP             23 8
405200001227     C                   EXSR      ADGCAL
405300001227     C*
405400001227IF  1C     TASADG        IFGT      *ZEROS
405500001227     C                   Z-ADD     61            I
405600001227     C                   Z-ADD     TASADG        VIMP(I)
405700001227E   1C                   ENDIF
405800001227     C*
405900001227     C                   ENDSR
406000001227     C*------------------------------------------------------------------------*
406100001227     C* CAL078 - CALCOLA ISTAT SU ADG/ASS RCV  ASSEGNATO
406200001227     C*------------------------------------------------------------------------*
406300001227     C     CAL078        BEGSR
406400001227     C*
406500001227     C* REIMPOSTA LE VARIABILI DI LAVORO
406600011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
406700001227     C*
406800001227     C* CALCOLA L'ISTAT
406900011031     C                   Z-ADD     VIMP(61)      WIMP             23 8
407000001227     C                   EXSR      IACCAL
407100001227     C*
407200001227IF  1C     TASIAC        IFGT      *ZEROS
407300001227     C                   Z-ADD     78            I
407400001227     C                   Z-ADD     TASIAC        VIMP(I)
407500001227E   1C                   ENDIF
407600001227     C*
407700001227     C                   ENDSR
407800970306     C*------------------------------------------------------------------------*
407900970306     C* CALCPA - CALCOLA CONSEGNA PARTICOLARE SU APPUNTAMENTO
408000970306     C*------------------------------------------------------------------------*
408100970306     C     CALCPA        BEGSR
408200970306     C*
408300970306     C* REIMPOSTA LE VARIABILI DI LAVORO
408400980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
408500011031     C                   Z-ADD     *ZEROS        TASCPA           21 8
408600970306     C*
408700970306     C* CALCOLA CONSEGNA PARTICOLARE
408800970306     C                   MOVEL     'A'           TASTC             1            *CODICE CONSEGNA
408900011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
409000970306     C                   EXSR      CALCP
409100970306     C                   Z-ADD     TASCP         TASCPA
409200970306     C*
409300970314IF  1C     TASCPA        IFGT      *ZEROS
409400970306     C                   Z-ADD     11            I
409500970306     C                   Z-ADD     TASCPA        VIMP(I)
409600970314E   1C                   ENDIF
409700970306     C*
409800970306     C                   ENDSR
409900001227     C*------------------------------------------------------------------------*
410000001227     C* CAL050 - CALCOLA ADG SU CONSEGNA APPUNTAMENTO
410100001227     C*------------------------------------------------------------------------*
410200001227     C     CAL050        BEGSR
410300001227     C*
410400001227     C* REIMPOSTA LE VARIABILI DI LAVORO
410500011031     C                   Z-ADD     *ZEROS        TASADG           21 8
410600001227     C*
410700001227     C* CALCOLA L'A.D.G.
410800011031     C                   Z-ADD     VIMP(11)      WIMP             23 8
410900001227     C                   EXSR      ADGCAL
411000001227     C*
411100001227IF  1C     TASADG        IFGT      *ZEROS
411200001227     C                   Z-ADD     50            I
411300001227     C                   Z-ADD     TASADG        VIMP(I)
411400001227E   1C                   ENDIF
411500001227     C*
411600001227     C                   ENDSR
411700001227     C*------------------------------------------------------------------------*
411800001227     C* CAL067 - CALCOLA ISTAT SU ADG/CONSEGNA APPUNTAMENTO
411900001227     C*------------------------------------------------------------------------*
412000001227     C     CAL067        BEGSR
412100001227     C*
412200001227     C* REIMPOSTA LE VARIABILI DI LAVORO
412300011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
412400001227     C*
412500001227     C* CALCOLA L'ISTAT
412600011031     C                   Z-ADD     VIMP(50)      WIMP             23 8
412700001227     C                   EXSR      IACCAL
412800001227     C*
412900001227IF  1C     TASIAC        IFGT      *ZEROS
413000001227     C                   Z-ADD     67            I
413100001227     C                   Z-ADD     TASIAC        VIMP(I)
413200001227E   1C                   ENDIF
413300001227     C*
413400001227     C                   ENDSR
413500970306     C*------------------------------------------------------------------------*
413600970306     C* CALCPS - CALCOLA CONSEGNA PARTICOLARE AI SUPERMERCATI
413700970306     C*------------------------------------------------------------------------*
413800970306     C     CALCPS        BEGSR
413900970306     C*
414000970306     C* REIMPOSTA LE VARIABILI DI LAVORO
414100980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
414200011031     C                   Z-ADD     *ZEROS        TASCPS           21 8
414300970306     C*
414400970306     C* CALCOLA CONSEGNA PARTICOLARE
414500970306     C                   MOVEL     'S'           TASTC             1            *CODICE CONSEGNA
414600011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
414700980804     C                   EXSR      CALCP
414800970306     C                   Z-ADD     TASCP         TASCPS
414900970306     C*
415000970314IF  1C     TASCPS        IFGT      *ZEROS
415100970306     C                   Z-ADD     12            I
415200970306     C                   Z-ADD     TASCPS        VIMP(I)
415300970314E   1C                   ENDIF
415400970306     C*
415500970306     C                   ENDSR
415600001227     C*------------------------------------------------------------------------*
415700001227     C* CAL051 - CALCOLA ADG SU CONSEGNA SUPERMERCATI
415800001227     C*------------------------------------------------------------------------*
415900001227     C     CAL051        BEGSR
416000001227     C*
416100001227     C* REIMPOSTA LE VARIABILI DI LAVORO
416200011031     C                   Z-ADD     *ZEROS        TASADG           21 8
416300001227     C*
416400001227     C* CALCOLA L'A.D.G.
416500011031     C                   Z-ADD     VIMP(12)      WIMP             23 8
416600001227     C                   EXSR      ADGCAL
416700001227     C*
416800001227IF  1C     TASADG        IFGT      *ZEROS
416900001227     C                   Z-ADD     51            I
417000001227     C                   Z-ADD     TASADG        VIMP(I)
417100001227E   1C                   ENDIF
417200001227     C*
417300001227     C                   ENDSR
417400001227     C*------------------------------------------------------------------------*
417500001227     C* CAL068 - CALCOLA ISTAT SU ADG/CONSEGNA SUPERMERCATI
417600001227     C*------------------------------------------------------------------------*
417700001227     C     CAL068        BEGSR
417800001227     C*
417900001227     C* REIMPOSTA LE VARIABILI DI LAVORO
418000011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
418100001227     C*
418200001227     C* CALCOLA L'ISTAT
418300011031     C                   Z-ADD     VIMP(51)      WIMP             23 8
418400001227     C                   EXSR      IACCAL
418500001227     C*
418600001227IF  1C     TASIAC        IFGT      *ZEROS
418700001227     C                   Z-ADD     68            I
418800001227     C                   Z-ADD     TASIAC        VIMP(I)
418900001227E   1C                   ENDIF
419000001227     C*
419100001227     C                   ENDSR
419200970306     C*------------------------------------------------------------------------*
419300970306     C* CALCPP - CALCOLA CONSEGNA PARTICOLARE AI PIANI
419400970306     C*------------------------------------------------------------------------*
419500970306     C     CALCPP        BEGSR
419600970306     C*
419700970306     C* REIMPOSTA LE VARIABILI DI LAVORO
419800980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
419900011031     C                   Z-ADD     *ZEROS        TASCPP           21 8
420000970306     C*
420100970306     C* CALCOLA CONSEGNA PARTICOLARE
420200970306     C                   MOVEL     'P'           TASTC             1            *CODICE CONSEGNA
420300011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
420400970306     C                   EXSR      CALCP
420500970306     C                   Z-ADD     TASCP         TASCPP
420600970306     C*
420700970314IF  1C     TASCPP        IFGT      *ZEROS
420800970306     C                   Z-ADD     13            I
420900970306     C                   Z-ADD     TASCPP        VIMP(I)
421000970314E   1C                   ENDIF
421100970306     C*
421200970306     C                   ENDSR
421300001227     C*------------------------------------------------------------------------*
421400001227     C* CAL052 - CALCOLA ADG SU CONSEGNA AI PIANI
421500001227     C*------------------------------------------------------------------------*
421600001227     C     CAL052        BEGSR
421700001227     C*
421800001227     C* REIMPOSTA LE VARIABILI DI LAVORO
421900011031     C                   Z-ADD     *ZEROS        TASADG           21 8
422000001227     C*
422100001227     C* CALCOLA L'A.D.G.
422200011031     C                   Z-ADD     VIMP(13)      WIMP             23 8
422300001227     C                   EXSR      ADGCAL
422400001227     C*
422500001227IF  1C     TASADG        IFGT      *ZEROS
422600001227     C                   Z-ADD     52            I
422700001227     C                   Z-ADD     TASADG        VIMP(I)
422800001227E   1C                   ENDIF
422900001227     C*
423000001227     C                   ENDSR
423100001227     C*------------------------------------------------------------------------*
423200001227     C* CAL069 - CALCOLA ISTAT SU ADG/CONSEGNA AI PIANI
423300001227     C*------------------------------------------------------------------------*
423400001227     C     CAL069        BEGSR
423500001227     C*
423600001227     C* REIMPOSTA LE VARIABILI DI LAVORO
423700011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
423800001227     C*
423900001227     C* CALCOLA L'ISTAT
424000011031     C                   Z-ADD     VIMP(52)      WIMP             23 8
424100001227     C                   EXSR      IACCAL
424200001227     C*
424300001227IF  1C     TASIAC        IFGT      *ZEROS
424400001227     C                   Z-ADD     69            I
424500001227     C                   Z-ADD     TASIAC        VIMP(I)
424600001227E   1C                   ENDIF
424700001227     C*
424800001227     C                   ENDSR
424900040826     C*------------------------------------------------------------------------*
425000040826     C* CALdes - CALCOLA CONSEGNA destinatario disagiato
425100040826     C*------------------------------------------------------------------------*
425200040826     C     CALdes        BEGSR
425300040826     C*
425400040826     C* REIMPOSTA LE VARIABILI DI LAVORO
425500040826     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
425600040826     C                   Z-ADD     *ZEROS        TASdes           21 8
425700040826     C*
425800040826     C* CALCOLA CONSEGNA PARTICOLARE
425900040826     C                   MOVEL     'X'           TASTC             1            *CODICE CONSEGNA
426000040826     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
426100040826     C                   EXSR      CALCP
426200040826     C                   Z-ADD     TASCP         TASdes
426300040826     C*
426400040826IF  1C     TASdes        IFGT      *ZEROS
426500040826     C                   Z-ADD     96            I
426600040826     C                   Z-ADD     TASdes        VIMP(I)
426700040826E   1C                   ENDIF
426800040826     C*
426900040826     C                   ENDSR
427000040826     C*------------------------------------------------------------------------*
427100040826     C* CAL097 - CALCOLA ADG SU CONSEGNA destinatario disagiato
427200040826     C*------------------------------------------------------------------------*
427300040826     C     CAL097        BEGSR
427400040826     C*
427500040826     C* REIMPOSTA LE VARIABILI DI LAVORO
427600040826     C                   Z-ADD     *ZEROS        TASADG
427700040826     C*
427800040826     C* CALCOLA L'A.D.G.
427900040826     C                   Z-ADD     VIMP(96)      WIMP
428000040826     C                   EXSR      ADGCAL
428100040826     C*
428200040826IF  1C     TASADG        IFGT      *ZEROS
428300040826     C                   Z-ADD     97            I
428400040826     C                   Z-ADD     TASADG        VIMP(I)
428500040826E   1C                   ENDIF
428600040826     C*
428700040826     C                   ENDSR
428800040826     C*------------------------------------------------------------------------*
428900040826     C* CAL098 - CALCOLA ISTAT SU ADG/CONSEGNA destinatario disagiato
429000040826     C*------------------------------------------------------------------------*
429100040826     C     CAL098        BEGSR
429200040826     C*
429300040826     C* REIMPOSTA LE VARIABILI DI LAVORO
429400040826     C                   Z-ADD     *ZEROS        TASIAC
429500040826     C*
429600040826     C* CALCOLA L'ISTAT
429700040826     C                   Z-ADD     VIMP(97)      WIMP
429800040826     C                   EXSR      IACCAL
429900040826     C*
430000040826IF  1C     TASIAC        IFGT      *ZEROS
430100040826     C                   Z-ADD     98            I
430200040826     C                   Z-ADD     TASIAC        VIMP(I)
430300040826E   1C                   ENDIF
430400040826     C*
430500040826     C                   ENDSR
430600040826     C*------------------------------------------------------------------------*
430700040826     C* CALban - CALCOLA bancali
430800040826     C*------------------------------------------------------------------------*
430900040826     C     CALban        BEGSR
431000040826     C*
431100040826     C* REIMPOSTA LE VARIABILI DI LAVORO
431200040826     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
431300040826     C                   Z-ADD     *ZEROS        TASban           21 8
431400040826     C*
431500040826     C* CALCOLA CONSEGNA PARTICOLARE
431600040826     C                   MOVEL     '='           TASTC             1            *CODICE CONSEGNA
431700040826     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
431800040826     C                   Z-ADD     ETCnba        TOTqta           21 8          *VALORE CONTRASS.
431900040826     C                   EXSR      CALCP
432000040826     C                   Z-ADD     *ZEROS        TOTqta
432100040826     C                   Z-ADD     TASCP         TASban
432200040826     C*
432300040826IF  1C     TASban        IFGT      *ZEROS
432400040826     C                   Z-ADD     99            I
432500040826     C                   Z-ADD     TASban        VIMP(I)
432600040826E   1C                   ENDIF
432700040826     C*
432800040826     C                   ENDSR
432900040826     C*------------------------------------------------------------------------*
433000040826     C* CAL100 - CALCOLA ADG SU bancali
433100040826     C*------------------------------------------------------------------------*
433200040826     C     CAL100        BEGSR
433300040826     C*
433400040826     C* REIMPOSTA LE VARIABILI DI LAVORO
433500040826     C                   Z-ADD     *ZEROS        TASADG
433600040826     C*
433700040826     C* CALCOLA L'A.D.G.
433800040826     C                   Z-ADD     VIMP(99)      WIMP
433900040826     C                   EXSR      ADGCAL
434000040826     C*
434100040826IF  1C     TASADG        IFGT      *ZEROS
434200040826     C                   Z-ADD     100           I
434300040826     C                   Z-ADD     TASADG        VIMP(I)
434400040826E   1C                   ENDIF
434500040826     C*
434600040826     C                   ENDSR
434700040826     C*------------------------------------------------------------------------*
434800040826     C* CAL101 - CALCOLA ISTAT SU ADG bancali
434900040826     C*------------------------------------------------------------------------*
435000040826     C     CAL101        BEGSR
435100040826     C*
435200040826     C* REIMPOSTA LE VARIABILI DI LAVORO
435300040826     C                   Z-ADD     *ZEROS        TASIAC
435400040826     C*
435500040826     C* CALCOLA L'ISTAT
435600040826     C                   Z-ADD     VIMP(100)     WIMP
435700040826     C                   EXSR      IACCAL
435800040826     C*
435900040826IF  1C     TASIAC        IFGT      *ZEROS
436000040826     C                   Z-ADD     101           I
436100040826     C                   Z-ADD     TASIAC        VIMP(I)
436200040826E   1C                   ENDIF
436300040826     C*
436400040826     C                   ENDSR
436500970306     C*------------------------------------------------------------------------*
436600970306     C* CALCPF - CALCOLA CONSEGNA PARTICOLARE FUORI MISURA
436700970306     C*------------------------------------------------------------------------*
436800970306     C     CALCPF        BEGSR
436900970306     C*
437000970306     C* REIMPOSTA LE VARIABILI DI LAVORO
437100980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
437200011031     C                   Z-ADD     *ZEROS        TASCPF           21 8
437300970306     C*
437400970306     C* CALCOLA CONSEGNA PARTICOLARE
437500970306     C                   MOVEL     'F'           TASTC             1            *CODICE CONSEGNA
437600011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
437700970306     C                   EXSR      CALCP
437800970306     C                   Z-ADD     TASCP         TASCPF
437900970306     C*
438000970314IF  1C     TASCPF        IFGT      *ZEROS
438100970306     C                   Z-ADD     14            I
438200970306     C                   Z-ADD     TASCPF        VIMP(I)
438300970314E   1C                   ENDIF
438400970306     C*
438500970306     C                   ENDSR
438600001227     C*------------------------------------------------------------------------*
438700001227     C* CAL053 - CALCOLA ADG SU CONSEGNA FUORI MISURA
438800001227     C*------------------------------------------------------------------------*
438900001227     C     CAL053        BEGSR
439000001227     C*
439100001227     C* REIMPOSTA LE VARIABILI DI LAVORO
439200011031     C                   Z-ADD     *ZEROS        TASADG           21 8
439300001227     C*
439400001227     C* CALCOLA L'A.D.G.
439500011031     C                   Z-ADD     VIMP(14)      WIMP             23 8
439600001227     C                   EXSR      ADGCAL
439700001227     C*
439800001227IF  1C     TASADG        IFGT      *ZEROS
439900001227     C                   Z-ADD     53            I
440000001227     C                   Z-ADD     TASADG        VIMP(I)
440100001227E   1C                   ENDIF
440200001227     C*
440300001227     C                   ENDSR
440400001227     C*------------------------------------------------------------------------*
440500001227     C* CAL070 - CALCOLA ISTAT SU ADG/CONSEGNA FUORI MISURA
440600001227     C*------------------------------------------------------------------------*
440700001227     C     CAL070        BEGSR
440800001227     C*
440900001227     C* REIMPOSTA LE VARIABILI DI LAVORO
441000011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
441100001227     C*
441200001227     C* CALCOLA L'ISTAT
441300011031     C                   Z-ADD     VIMP(53)      WIMP             23 8
441400001227     C                   EXSR      IACCAL
441500001227     C*
441600001227IF  1C     TASIAC        IFGT      *ZEROS
441700001227     C                   Z-ADD     70            I
441800001227     C                   Z-ADD     TASIAC        VIMP(I)
441900001227E   1C                   ENDIF
442000001227     C*
442100001227     C                   ENDSR
442200970414     C*------------------------------------------------------------------------*
442300970414     C* CALCPD - CALCOLA CONSEGNA PARTICOLARE DDT
442400970414     C*------------------------------------------------------------------------*
442500970414     C     CALCPD        BEGSR
442600970414     C*
442700970414     C* REIMPOSTA LE VARIABILI DI LAVORO
442800980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
442900011031     C                   Z-ADD     *ZEROS        TASCPD           21 8
443000970414     C*
443100970414     C* CALCOLA CONSEGNA PARTICOLARE
443200970414     C                   MOVEL     'B'           TASTC             1            *CODICE CONSEGNA
443300011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
443400970414     C                   EXSR      CALCP
443500970414     C                   Z-ADD     TASCP         TASCPD
443600970414     C*
443700970414IF  1C     TASCPD        IFGT      *ZEROS
443800970414     C                   Z-ADD     33            I
443900970414     C                   Z-ADD     TASCPD        VIMP(I)
444000970414E   1C                   ENDIF
444100970414     C*
444200970414     C                   ENDSR
444300001227     C*------------------------------------------------------------------------*
444400001227     C* CAL062 - CALCOLA ADG SU CONSEGNA DDT
444500001227     C*------------------------------------------------------------------------*
444600001227     C     CAL062        BEGSR
444700001227     C*
444800001227     C* REIMPOSTA LE VARIABILI DI LAVORO
444900011031     C                   Z-ADD     *ZEROS        TASADG           21 8
445000001227     C*
445100001227     C* CALCOLA L'A.D.G.
445200011031     C                   Z-ADD     VIMP(33)      WIMP             23 8
445300001227     C                   EXSR      ADGCAL
445400001227     C*
445500001227IF  1C     TASADG        IFGT      *ZEROS
445600001227     C                   Z-ADD     62            I
445700001227     C                   Z-ADD     TASADG        VIMP(I)
445800001227E   1C                   ENDIF
445900001227     C*
446000001227     C                   ENDSR
446100001227     C*------------------------------------------------------------------------*
446200001227     C* CAL079 - CALCOLA ISTAT SU ADG/CONSEGNA DDT
446300001227     C*------------------------------------------------------------------------*
446400001227     C     CAL079        BEGSR
446500001227     C*
446600001227     C* REIMPOSTA LE VARIABILI DI LAVORO
446700011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
446800001227     C*
446900001227     C* CALCOLA L'ISTAT
447000011031     C                   Z-ADD     VIMP(62)      WIMP             23 8
447100001227     C                   EXSR      IACCAL
447200001227     C*
447300001227IF  1C     TASIAC        IFGT      *ZEROS
447400001227     C                   Z-ADD     79            I
447500001227     C                   Z-ADD     TASIAC        VIMP(I)
447600001227E   1C                   ENDIF
447700001227     C*
447800001227     C                   ENDSR
447900980706     C*------------------------------------------------------------------------*
448000980706     C* CALRIT - CALCOLA IL RITIRO
448100980706     C*------------------------------------------------------------------------*
448200980706     C     CALRIT        BEGSR
448300980706     C*
448400980706     C* REIMPOSTA LE VARIABILI DI LAVORO
448500980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
448600011031     C                   Z-ADD     *ZEROS        TASRIT           21 8
448700980706     C*
448800980706     C* CALCOLA CONSEGNA PARTICOLARE
448900980706     C                   MOVEL     'U'           TASTC             1            *CODICE CONSEGNA
449000011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
449100980706     C*
449200980706     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
449300980706     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
449400980706     C                   EXSR      CALCP
449500980706     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
449600980706     C                   Z-ADD     TASCP         TASRIT
449700980706     C*
449800980706IF  1C     TASRIT        IFGT      *ZEROS
449900980706     C                   Z-ADD     17            I
450000980706     C                   Z-ADD     TASRIT        VIMP(I)
450100980706E   1C                   ENDIF
450200980706     C*
450300980706     C                   ENDSR
450400001227     C*------------------------------------------------------------------------*
450500001227     C* CAL056 - CALCOLA ADG SU RITIRO
450600001227     C*------------------------------------------------------------------------*
450700001227     C     CAL056        BEGSR
450800001227     C*
450900001227     C* REIMPOSTA LE VARIABILI DI LAVORO
451000011031     C                   Z-ADD     *ZEROS        TASADG           21 8
451100001227     C*
451200001227     C* CALCOLA L'A.D.G.
451300011031     C                   Z-ADD     VIMP(17)      WIMP             23 8
451400001227     C                   EXSR      ADGCAL
451500001227     C*
451600001227IF  1C     TASADG        IFGT      *ZEROS
451700001227     C                   Z-ADD     56            I
451800001227     C                   Z-ADD     TASADG        VIMP(I)
451900001227E   1C                   ENDIF
452000001227     C*
452100001227     C                   ENDSR
452200001227     C*------------------------------------------------------------------------*
452300001227     C* CAL073 - CALCOLA ISTAT SU ADG/RITIRO
452400001227     C*------------------------------------------------------------------------*
452500001227     C     CAL073        BEGSR
452600001227     C*
452700001227     C* REIMPOSTA LE VARIABILI DI LAVORO
452800011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
452900001227     C*
453000001227     C* CALCOLA L'ISTAT
453100011031     C                   Z-ADD     VIMP(56)      WIMP             23 8
453200001227     C                   EXSR      IACCAL
453300001227     C*
453400001227IF  1C     TASIAC        IFGT      *ZEROS
453500001227     C                   Z-ADD     73            I
453600001227     C                   Z-ADD     TASIAC        VIMP(I)
453700001227E   1C                   ENDIF
453800001227     C*
453900001227     C                   ENDSR
454000980706     C*------------------------------------------------------------------------*
454100980706     C* CALPAM - CALCOLA LA PROVVIGIONE ASSEGNO MITTENTE
454200980706     C*------------------------------------------------------------------------*
454300980706     C     CALPAM        BEGSR
454400980706     C*
454500980706     C* REIMPOSTA LE VARIABILI DI LAVORO
454600980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
454700011031     C                   Z-ADD     *ZEROS        TASPAM           21 8
454800980706     C*
454900980706     C* CALCOLA CONSEGNA PARTICOLARE
455000980706     C                   MOVEL     'W'           TASTC             1            *CODICE CONSEGNA
455100011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
455200011031     C                   Z-ADD     ETCCAS        TOTVAL           21 8          *VALORE CONTRASS.
455300980706     C                   EXSR      CALCP
455400980911     C                   Z-ADD     *ZEROS        TOTVAL
455500980706     C                   Z-ADD     TASCP         TASPAM
455600980706     C*
455700980706IF  1C     TASPAM        IFGT      *ZEROS
455800980706     C                   Z-ADD     15            I
455900980706     C                   Z-ADD     TASPAM        VIMP(I)
456000980706E   1C                   ENDIF
456100980706     C*
456200980706     C                   ENDSR
456300001227     C*------------------------------------------------------------------------*
456400001227     C* CAL054 - CALCOLA ADG SU C/ASSEGNO MITTENTE
456500001227     C*------------------------------------------------------------------------*
456600001227     C     CAL054        BEGSR
456700001227     C*
456800001227     C* REIMPOSTA LE VARIABILI DI LAVORO
456900011031     C                   Z-ADD     *ZEROS        TASADG           21 8
457000001227     C*
457100001227     C* CALCOLA L'A.D.G.
457200011031     C                   Z-ADD     VIMP(15)      WIMP             23 8
457300001227     C                   EXSR      ADGCAL
457400001227     C*
457500001227IF  1C     TASADG        IFGT      *ZEROS
457600001227     C                   Z-ADD     54            I
457700001227     C                   Z-ADD     TASADG        VIMP(I)
457800001227E   1C                   ENDIF
457900001227     C*
458000001227     C                   ENDSR
458100001227     C*------------------------------------------------------------------------*
458200001227     C* CAL071 - CALCOLA ISTAT SU ADG/CASSEGNO MITTENTE
458300001227     C*------------------------------------------------------------------------*
458400001227     C     CAL071        BEGSR
458500001227     C*
458600001227     C* REIMPOSTA LE VARIABILI DI LAVORO
458700011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
458800001227     C*
458900001227     C* CALCOLA L'ISTAT
459000011031     C                   Z-ADD     VIMP(54)      WIMP             23 8
459100001227     C                   EXSR      IACCAL
459200001227     C*
459300001227IF  1C     TASIAC        IFGT      *ZEROS
459400001227     C                   Z-ADD     71            I
459500001227     C                   Z-ADD     TASIAC        VIMP(I)
459600001227E   1C                   ENDIF
459700001227     C*
459800001227     C                   ENDSR
459900980706     C*------------------------------------------------------------------------*
460000980706     C* CALPAB - CALCOLA LA PROVVIGIONE ASSEGNO BARTOLINI
460100980706     C*------------------------------------------------------------------------*
460200980706     C     CALPAB        BEGSR
460300980706     C*
460400980706     C* REIMPOSTA LE VARIABILI DI LAVORO
460500980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
460600011031     C                   Z-ADD     *ZEROS        TASPAB           21 8
460700980706     C*
460800980706     C* CALCOLA CONSEGNA PARTICOLARE
460900980706     C                   MOVEL     'V'           TASTC             1            *CODICE CONSEGNA
461000011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
461100980911     C                   Z-ADD     ETCCAS        TOTVAL                         *VALORE CONTRASS.
461200980706     C                   EXSR      CALCP
461300980911     C                   Z-ADD     *ZEROS        TOTVAL
461400980706     C                   Z-ADD     TASCP         TASPAB
461500980706     C*
461600980706IF  1C     TASPAB        IFGT      *ZEROS
461700980706     C                   Z-ADD     16            I
461800980706     C                   Z-ADD     TASPAB        VIMP(I)
461900980706E   1C                   ENDIF
462000980706     C*
462100980706     C                   ENDSR
462200001227     C*------------------------------------------------------------------------*
462300001227     C* CAL055 - CALCOLA ADG SU C/ASSEGNO BARTOLINI
462400001227     C*------------------------------------------------------------------------*
462500001227     C     CAL055        BEGSR
462600001227     C*
462700001227     C* REIMPOSTA LE VARIABILI DI LAVORO
462800011031     C                   Z-ADD     *ZEROS        TASADG           21 8
462900001227     C*
463000001227     C* CALCOLA L'A.D.G.
463100011031     C                   Z-ADD     VIMP(16)      WIMP             23 8
463200001227     C                   EXSR      ADGCAL
463300001227     C*
463400001227IF  1C     TASADG        IFGT      *ZEROS
463500001227     C                   Z-ADD     55            I
463600001227     C                   Z-ADD     TASADG        VIMP(I)
463700001227E   1C                   ENDIF
463800001227     C*
463900001227     C                   ENDSR
464000001227     C*------------------------------------------------------------------------*
464100001227     C* CAL072 - CALCOLA ISTAT SU ADG/CASSEGNO BARTOLINI
464200001227     C*------------------------------------------------------------------------*
464300001227     C     CAL072        BEGSR
464400001227     C*
464500001227     C* REIMPOSTA LE VARIABILI DI LAVORO
464600011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
464700001227     C*
464800001227     C* CALCOLA L'ISTAT
464900011031     C                   Z-ADD     VIMP(55)      WIMP             23 8
465000001227     C                   EXSR      IACCAL
465100001227     C*
465200001227IF  1C     TASIAC        IFGT      *ZEROS
465300001227     C                   Z-ADD     72            I
465400001227     C                   Z-ADD     TASIAC        VIMP(I)
465500001227E   1C                   ENDIF
465600001227     C*
465700001227     C                   ENDSR
465800980706     C*------------------------------------------------------------------------*
465900980706     C* CALDIF - CALCOLA IL DIRITTO FISSO
466000980706     C*------------------------------------------------------------------------*
466100980706     C     CALDIF        BEGSR
466200980706     C*
466300980706     C* REIMPOSTA LE VARIABILI DI LAVORO
466400980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
466500011031     C                   Z-ADD     *ZEROS        TASDIF           21 8
466600980706     C*
466700980706     C* CALCOLA CONSEGNA PARTICOLARE
466800980706     C                   MOVEL     'H'           TASTC             1            *CODICE CONSEGNA
466900011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
467000980706     C                   EXSR      CALCP
467100980706     C                   Z-ADD     TASCP         TASDIF
467200980706     C*
467300980706IF  1C     TASDIF        IFGT      *ZEROS
467400980706     C                   Z-ADD     2             I
467500980706     C                   Z-ADD     TASDIF        VIMP(I)
467600980706E   1C                   ENDIF
467700980706     C*
467800980706     C                   ENDSR
467900980706     C*------------------------------------------------------------------------*
468000980706     C* CALISD - CALCOLA L'ISTAT SUL DIRITTO FISSO
468100980706     C*------------------------------------------------------------------------*
468200980706     C     CALISD        BEGSR
468300980706     C*
468400980706     C* REIMPOSTA LE VARIABILI DI LAVORO
468500011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
468600980706     C*
468700980706     C* CALCOLA L'ISTAT
468800011031     C                   Z-ADD     TASDIF        WIMP             23 8
468900980706     C                   EXSR      IACCAL
469000980706     C*
469100980706IF  1C     TASIAC        IFGT      *ZEROS
469200980706     C                   Z-ADD     25            I
469300980706     C                   Z-ADD     TASIAC        VIMP(I)
469400980706E   1C                   ENDIF
469500980706     C*
469600980706     C                   ENDSR
469700980706     C*------------------------------------------------------------------------*
469800980706     C* CALIAD - CALCOLA L'ISTAT SULL'ADG DEL DIRITTO FISSO
469900980706     C*------------------------------------------------------------------------*
470000980706     C     CALIAD        BEGSR
470100980706     C*
470200980706     C* REIMPOSTA LE VARIABILI DI LAVORO
470300011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
470400980706     C*
470500980706     C* CALCOLA L'ISTAT
470600011031     C                   Z-ADD     VIMP(22)      WIMP             23 8
470700980706     C                   EXSR      IACCAL
470800980706     C*
470900980706IF  1C     TASIAC        IFGT      *ZEROS
471000980706     C                   Z-ADD     28            I
471100980706     C                   Z-ADD     TASIAC        VIMP(I)
471200980706E   1C                   ENDIF
471300980706     C*
471400980706     C                   ENDSR
471500970306     C*------------------------------------------------------------------------*
471600970306     C* CALCP - CALCOLA CONSEGNA PARTICOLARE GENERICA
471700970306     C*------------------------------------------------------------------------*
471800970306     C     CALCP         BEGSR
471900970306     C*
472000980506     C* CERCA LA TARIFFA PARTICOLARE
472100980506     C                   EXSR      CERTPT
472200980706     C*
472300980706     C* CALCOLA RPV, ARROTONDAMENTI E PESO TASSABILE
472400980706IF  1C     $TPT          IFEQ      '1'                                          *TARIFFA TROVATA
472500980707     C                   MOVEL     TPTTPG        WTPG
472600980707     C                   MOVEL     TPTRPV        WRPV
472700980707     C                   CLEAR                   WDET
472800980707     C                   Z-ADD     TPTARL        WARL
472900980707     C                   Z-ADD     TPTARF        WARF
473000980707     C                   Z-ADD     TPTARO        WARO
473100980706     C                   EXSR      CALCOL
473200040826     c                   select
473300040826IF  2C                   when      tpttpg = 'T'                                 *A TRASPORTO
473400980706     C                   Z-ADD     TOTTRA        ISOPES                         *TRASPORTO
473500040826IF  2C                   when      tpttpg = 'V'                                 *A TRASPORTO
473600980911     C                   Z-ADD     TOTVAL        ISOPES                         *TRASPORTO
473700040826IF  2C                   when      tpttpg = '5'                                 *A TRASPORTO
473800040826     C                   Z-ADD     TOTqta        ISOPES                         *TRASPORTO
473900040826E   2C                   ENDsl
474000980706     C*
474100980706     C* CALCOLA LA CONSEGNA PARTICOLARE
474200970306     C                   Z-ADD     ISOPES        TPDPES                         *PESO
474300970306     C                   EXSR      CALACD
474400980706     C                   EXSR      CALTRA                                       *CALCOLA % A TRASPOR
474500970306     C                   Z-ADD     VARIA         TASCP
474600970721     C*
474700970721     C* MEMORIZZA IL FLAG "APPLICAZIONE TARIFFA INOLTRO" DELLO SCAGLIONE DEL
474800970721     C* CODICE TASSAZIONE DEL DETTAGLIO TARIFFARIO
474900970721     C                   MOVEL     AAIN          DEPAIN
475000980506E   1C                   ENDIF
475100970306     C*
475200970306     C                   ENDSR
475300980506     C*------------------------------------------------------------------------*
475400980506     C* CERTPT - CERCA TARIFFA PARTICOLARE
475500980506     C*------------------------------------------------------------------------*
475600980506     C     CERTPT        BEGSR
475700980506     C*
475800980506     C* REPERISCE IL CODICE PARTICOLARITA'
475900980506     C                   MOVEL     '0'           $TPT                           *TARIFFA NON TROVATA
476000980506     C                   MOVEL     TASTC         KTPFTC                         *CAMPO CHIAVE
476100980506     C                   Z-ADD     1             I
476200980506     C                   SETOFF                                           28
476300990819     C                   MOVEL     KTPFTC        A1                1
476400990819     C     A1            LOOKUP    CP(I)                                  28
476500980506IF  1C     *IN28         IFEQ      *ON                                          *CODICE ESISTENTE
476600980506     C     I             OCCUR     CPIDS                                        *TARIFFA DI CARTELLO
476700980506     C*
476800980506     C* RICERCA LA CONSEGNA PARTICOLARE NELLE TARIFFE DEL CLIENTE
476900980506     C                   Z-ADD     D00KSC        KTPKSC
477000980506     C                   Z-ADD     D00CTR        KTPCTR
477100980506     C                   Z-ADD     D00PRG        KTPPRG
477200990819     C     KEYTPT        SETLL     TITPT01L                               99
477300990819     C     KEYTPT        READE     TITPT01L                               99
477400980506DO  2C     *IN99         DOWEQ     *OFF
477500980506     C     $TPT          ANDEQ     '0'
477600991125     C*
477700991125     C* CONTROLLA SE CONVERTIRE I CAMPI DI TITPT
477800011026IF  3C     D00DIV        IFNE      WDIVCE
477900011019     C                   MOVEL     D00DIV        WDVI
478000991125     C                   EXSR      CNVTPT
478100991125E   3C                   ENDIF
478200991125     C*
478300980506IF  3C     TPTATB        IFEQ      *BLANKS
478400980506     C                   MOVEL     '1'           $TPT                           *TARIFFA TROVATA
478500980506E   3C                   ENDIF
478600980506     C*
478700980506     C* LETTURA SUCESSIVA TARIFFE
478800980506IF  3C     $TPT          IFEQ      '0'                                          *TARIFFA NON TROVATA
478900990819     C     KEYTPT        READE     TITPT01L                               99
479000980506E   3C                   ENDIF
479100980506E   2C                   ENDDO                                                  *FINE TAR.PART.KSC
479200980506     C*
479300980506     C* TARIFFA CONSEGNE PARTICOLARI NON TROVATA NELLE TARIFFE DEL CLIENTE
479400980506     C* RICERCA NELLE TARIFFE DI CARTELLO
479500980506IF  2C     $TPT          IFEQ      '0'                                          *TARIFFA KSC NO TROV
479600980506IF  3C     CP88          IFNE      'NO'                                         *C'E' TARIFFA CARTEL
479700980506     C     WCAR          ANDEQ     'S'                                          *SI UTILIZZO TAR.CAR
479800980506     C                   MOVEL     '1'           $TPT                           *TARIFFA TROVATA
479900980506     C                   Z-ADD     CARKSC        TPTKSC                         *CHIAVE CARTELLO
480000980506     C                   Z-ADD     CARCTR        TPTCTR
480100980506     C                   Z-ADD     CARPRG        TPTPRG
480200980506     C                   MOVEL     KTPFTC        TPTFTC
480300980506     C                   MOVEL     TTPG8P        TPTTPG                         *CAMPI CARTELLO
480400980506     C                   Z-ADD     TARF8P        TPTARF
480500980506     C                   Z-ADD     TARL8P        TPTARL
480600980506     C                   Z-ADD     TARO8P        TPTARO
480700980506     C                   Z-ADD     TRPV8P        TPTRPV
480800980506E   3C                   ENDIF
480900980506E   2C                   ENDIF
481000980506E   1C                   ENDIF
481100980506     C*
481200980506     C                   ENDSR
481300970306     C*------------------------------------------------------------------------*
481400970306     C* CALACD - CALCOLA TARIFFA PARTICOLARE GENERICA
481500970306     C*------------------------------------------------------------------------*
481600970306     C     CALACD        BEGSR
481700980706     C*
481800980706     C* AZZERA VARIABILI DI COMODO -SOLO DI QUESTO PGM-
481900980706     C                   MOVEL     *BLANKS       PTPG              1            *TIPO APPLICAZIONE
482000011031     C                   Z-ADD     *ZEROS        PITR             21 8          *IMPORTO TARIFFA
482100011031     C                   Z-ADD     *ZEROS        PVARI            23 8          *VARIA NON ARROTONDA
482200011031     C                   Z-ADD     *ZEROS        PVARIA           23 8          *VAIRA ARROTONDATA
482300970306     C*
482400970306     C* REIMPOSTA LE VARIABILI DI LAVORO
482500011031     C                   Z-ADD     *ZEROS        VARIA            23 8
482600970306DO  1C     1             DO        10            I
482700970306     C     I             OCCUR     ACDS
482800970306     C                   CLEAR                   ACDS
482900970306E   1C                   ENDDO
483000970306     C*
483100970306     C* IMPOSTA LA CHIAVE PER LA RICERCA SCAGLIONE NEL DETTAGLIO TARIFFARIO
483200970306     C                   Z-ADD     TPTKSC        KDPKSC                         *CLIENTE
483300970306     C                   Z-ADD     TPTCTR        KDPCTR                         *TARIFFA
483400970306     C                   Z-ADD     TPTPRG        KDPPRG                         *PROGRESSIVO
483500970306     C                   MOVEL     TPTFTC        KDPFTC                         *CONSEGNA PARTICOLAR
483600970306     C                   MOVEL     *BLANKS       KDPCTS                         *CODICE TASSAZIONE
483700970306     C*
483800970306     C* PER ALCUNE CONSEGNE PARTICOLARI SONO INDICATI NELLA TABELLA CONSEGNE
483900970306     C* PARTICOLARI I CODICI TASSAZIONE DA UTILIZZARE NELLA RICERCA TARIFFA
484000970306     C                   SETOFF                                           28
484100970306     C                   Z-ADD     1             I
484200970306     C     CCTS(T)       LOOKUP    CTS(I)                                 28
484300970306     C     I             OCCUR     DSCT
484400970306IF  1C     §CTNAR        IFNE      *BLANKS                                      *TARIFFA ESTERO
484500971204     C     DLPFL1        ORNE      *BLANKS                                      *ESTERO
484600970306IF  2C     TCTE8P        IFNE      *BLANKS                                      *SE INDICATO
484700970306     C                   MOVEL     TCTE8P        KDPCTS                         *CODICE 'EE' FISSO
484800970306E   2C                   ENDIF
484900970306X   1C                   ELSE                                                   *TARIFFA ITALIA
485000970306IF  2C     TCTS8P        IFNE      *BLANKS                                      *SE INDICATO
485100970306     C                   MOVEL     TCTS8P        KDPCTS                         *CODICE 'IT' FISSO
485200970306E   2C                   ENDIF
485300970306E   1C                   ENDIF
485400970306     C*
485500970306     C* CODICE TASSAZIONE NON FISSO, RICERCA QUAL'E' PER REPERIRE LA TARIFFA
485600980921     C                   SETOFF                                       98
485700980921IF  1C     KDPCTS        IFNE      *BLANKS
485800980921     C*
485900980921IF  2C     KDPKSC        IFEQ      CARKSC                                       *LEGGERE CARTELLO
486000980921     C     D00CTO        ANDEQ     'O'                                          *IN OFFERTA
486100990819     C     KEYTPD        SETLL     TIOPD02L                               98     DA OFFERTA
486200980921X   2C                   ELSE                                                   *LEGGERE CLIENTE
486300990819     C     KEYTPD        SETLL     TITPD02L                               98     DA TARIFFA
486400980921E   2C                   ENDIF
486500980921     C*
486600980921X   1C                   ELSE                                                   *KDPCTS = BLANKS
486700980921     C*
486800970306     C                   MOVEL     CCTS(T)       KDPCTS                         *CODCE TASSAZIONE
486900970306IF  2C     KDPKSC        IFEQ      CARKSC                                       *LEGGERE CARTELLO
487000970306     C     D00CTO        ANDEQ     'O'                                          *IN OFFERTA
487100990819     C     KEYTPD        SETLL     TIOPD02L                               98     DA OFFERTA
487200970306X   2C                   ELSE                                                   *LEGGERE CLIENTE
487300990819     C     KEYTPD        SETLL     TITPD02L                               98     DA TARIFFA
487400970306E   2C                   ENDIF
487500970306     C*
487600970306     C* CERCA LA TARIFFA CON IL CODICE TASSAZIONE REGIONE
487700980703IF  2C     *IN98         IFEQ      *OFF                                         *TARIFFA NON TROVATA
487800970306     C                   MOVEL     §CTRAP        KDPCTS                         *COD.TASS.REGIONE
487900970306IF  3C     KDPKSC        IFEQ      CARKSC                                       *LEGGERE CARTELLO
488000970306     C     D00CTO        ANDEQ     'O'                                          *IN OFFERTA
488100990819     C     KEYTPD        SETLL     TIOPD02L                               98     DA OFFERTA
488200970306X   3C                   ELSE                                                   *LEGGERE CLIENTE
488300990819     C     KEYTPD        SETLL     TITPD02L                               98     DA TARIFFA
488400970306E   3C                   ENDIF
488500970306     C*
488600970306     C* CERCA LA TARIFFA CON IL CODICE TASSAZIONE NAZIONE
488700000403IF  3C     *IN98         IFEQ      *OFF                                         *TARIFFA NON TROVATA
488800000403IF  4C     §CTNAR        IFEQ      *BLANKS
488900000403     C                   MOVEL     'IT'          KDPCTS                         *COD.TASS.NAZIONE
489000000403X   4C                   ELSE
489100000403     C                   MOVEL     'EE'          KDPCTS                         *COD.TASS.NAZIONE
489200000403E   4C                   ENDIF
489300000403IF  4C     KDPKSC        IFEQ      CARKSC                                       *LEGGERE CARTELLO
489400000403     C     D00CTO        ANDEQ     'O'                                          *IN OFFERTA
489500000403     C     KEYTPD        SETLL     TIOPD02L                               98     DA OFFERTA
489600000403X   4C                   ELSE                                                   *LEGGERE CLIENTE
489700000403     C     KEYTPD        SETLL     TITPD02L                               98     DA TARIFFA
489800000403E   4C                   ENDIF
489900000403E   3C                   ENDIF
490000000403IF  3C*---       *IN98     IFEQ *OFF                       *TARIFFA NON TROVATA
490100000403IF  4C*---       §CTNAR    IFEQ *BLANKS
490200000403IF  5C*---       DLPFL1    IFEQ *BLANKS
490300000403     C*---                 MOVEL'IT'      KDPCTS           *COD.TASS.NAZIONE
490400000403X   5C*---                 ELSE
490500000403     C*---                 MOVEL'EE'      KDPCTS           *COD.TASS.NAZIONE
490600000403E   5C*---                 ENDIF
490700000403IF  5C*---       KDPKSC    IFEQ CARKSC                     *LEGGERE CARTELLO
490800000403     C*---       D00CTO    ANDEQ'O'                        *IN OFFERTA
490900000403     C*---       KEYTPD    SETLLTIOPD02L                 98 DA OFFERTA
491000000403X   5C*---                 ELSE                            *LEGGERE CLIENTE
491100000403     C*---       KEYTPD    SETLLTITPD02L                 98 DA TARIFFA
491200000403E   5C*---                 ENDIF
491300000403E   4C*---                 ENDIF
491400000403E   3C*---                 ENDIF
491500970306E   2C                   ENDIF
491600970306E   1C                   ENDIF
491700000403     C*
491800970306     C* LEGGE LA TARIFFA
491900970306     C                   MOVEL     '0'           $TPD                           *TARIFFA NO ESISTENT
492000970306     C                   Z-ADD     *ZEROS        I
492100970306IF  1C     KDPKSC        IFEQ      CARKSC                                       *LEGGERE CARTELLO
492200970306     C     D00CTO        ANDEQ     'O'                                          *IN OFFERTA
492300990819     C     KEYTPD        READE     TIOPD02L                               98     DA OFFERTA
492400970306X   1C                   ELSE                                                   *LEGGERE CLIENTE
492500990819     C     KEYTPD        READE     TITPD02L                               98     DA TARIFFA
492600970306E   1C                   ENDIF
492700970306DO  1C     *IN98         DOWEQ     *OFF
492800991125     C*
492900991125     C* CONTROLLA SE CONVERTIRE I CAMPI DI TITPD
493000011023     C     KDPKSC        IFEQ      CARKSC                                       *LEGGERE CARTELLO
493100011023     C     D00CTO        ANDEQ     'O'                                          *IN OFFERTA
493200011023     C                   MOVEL     CARDIV        WDIV
493300011023     C                   ELSE
493400011019     C                   MOVEL     D00DIV        WDIV
493500011023     C                   ENDIF
493600011026IF  2C     WDIV          IFNE      WDIVCE
493700011019     C                   MOVEL     WDIV          WDVI
493800991125     C                   EXSR      CNVTPD
493900991125E   2C                   ENDIF
494000991125     C*
494100970306     C                   MOVEL     '1'           $TPD                           *TARIFFA ESISTENTE
494200970306     C                   ADD       1             I
494300970306     C     I             OCCUR     ACDS
494400970306     C                   Z-ADD     TPDSGL        ASGL
494500970306     C                   Z-ADD     TPDITR        AITR
494600970306     C                   Z-ADD     TPDMIN        AMIN
494700970306     C                   Z-ADD     TPDMAX        AMAX
494800970306     C                   MOVEL     TPDAIN        AAIN
494900970306IF  2C     KDPKSC        IFEQ      CARKSC                                       *LEGGERE CARTELLO
495000970306     C     D00CTO        ANDEQ     'O'                                          *IN OFFERTA
495100990819     C     KEYTPD        READE     TIOPD02L                               98     DA OFFERTA
495200970306X   2C                   ELSE                                                   *LEGGERE CLIENTE
495300990819     C     KEYTPD        READE     TITPD02L                               98     DA TARIFFA
495400970306E   2C                   ENDIF
495500970306E   1C                   ENDDO
495600970306     C*
495700970306     C* SE TARIFFA PARTICOLARE ESISTENTE, RICERCA LO SCAGLIONE
495800970306IF  1C     $TPD          IFEQ      '1'
495900970306DO  2C     1             DO        10            I
496000970306     C     I             OCCUR     ACDS
496100970306IF  3C     ASGL          IFGE      TPDPES                                       *SCAGLIONE TROVATO
496200970306     C                   LEAVE
496300970306E   3C                   ENDIF
496400970306E   2C                   ENDDO
496500970306     C*
496600970306     C* SCAGLIONE NON TROVATO, MANCA TARIFFA
496700970306IF  2C     ASGL          IFLT      TPDPES
496800001218     C                   MOVEL     MSG(26)       DMSG26
496900001218     C                   MOVEL     TPTFTC        DM26A
497000001218     C                   MOVEL     DMSG26        $MSG
497100970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
497200970306E   2C                   ENDIF
497300970306     C*
497400970306     C* CALCOLO DEL MINIMO TASSABILE
497500980703IF  2C     WERR          IFEQ      'N'                                          *NO ERRORE
497600980703IF  3C     ASGL          IFLE      MINTAS                                       *SGL<=MIN.TASS.STD.
497700970306     C                   Z-ADD     MINTAS        TPDPES                         *PESO=MIN.TASS.STD.
497800980703E   3C                   ENDIF
497900980703IF  3C     TPDPES        IFLE      MINTAS                                       *PESO<=MIN.TASS.STD.
498000970306     C                   Z-ADD     MINTAS        TPDPES                         *PESO=MIN.TASS.STD.
498100980703E   3C                   ENDIF
498200970306     C*
498300970306     C* CALCOLA IL PESO TASSABILE IN QUINTALI -PER TARIFFA A QUINTALI-
498400011031     C                   Z-ADD     *ZEROS        TPDQLI           23 8
498500980707IF  3C     TPTTPG        IFEQ      '0'
498600980707     C     TPTTPG        OREQ      '4'
498700970306     C     TPDPES        DIV       100           TPDQLI
498800980703X   3C                   ELSE
498900970306     C                   Z-ADD     TPDPES        TPDQLI
499000980703E   3C                   ENDIF
499100970306     C     AITR          MULT(H)   TPDQLI        VARIA
499200980703IF  3C     VARIA         IFGT      *ZEROS
499300980703IF  4C     TPTTPG        IFEQ      'V'                                          *A VALORE O
499400980506     C     TPTTPG        OREQ      'T'                                          *A TRASPORTO
499500970306     C     VARIA         DIV(H)    100           VARIA
499600980703E   4C                   ENDIF
499700980703E   3C                   ENDIF
499800980703E   2C                   ENDIF
499900980706     C*
500000980706     C* IMPOSTA VARIABILI DI COMODO -SOLO DI QUESTO PGM-
500100980706     C                   MOVEL     TPTTPG        PTPG                           *TIPO APPLICAZIONE
500200980706     C                   Z-ADD     TPDITR        PITR                           *IMPORTO TARIFFA
500300980706     C                   Z-ADD     VARIA         PVARI                          *VARIA NON ARROTONDA
500400970306     C* MINIMO TARIFFA
500500980703IF  2C     AMIN          IFGT      VARIA
500600970306     C                   Z-ADD     AMIN          VARIA
500700980703E   2C                   ENDIF
500800970306     C* MASSIMO TARIFFA
500900980703IF  2C     AMAX          IFGT      *ZEROS
501000980703IF  3C     AMAX          IFLT      VARIA
501100970306     C                   Z-ADD     AMAX          VARIA
501200980703E   3C                   ENDIF
501300980703E   2C                   ENDIF
501400980706     C*
501500980707     C* IMPOSTA VARIABILI DI COMODO -SOLO DI QUESTO PGM-
501600980706     C                   Z-ADD     VARIA         PVARIA                         *VARIA ARROTONDATA
501700970306E   1C                   ENDIF
501800970306     C*
501900970306     C                   ENDSR
502000980706     C*------------------------------------------------------------------------*
502100980706     C* CALTRA - CALCOLA CONSEGNA PARTICOLARE SUL TOTALE IMPORTO TRASPORTO
502200980706     C*------------------------------------------------------------------------*
502300980706     C     CALTRA        BEGSR
502400980706     C*---
502500980706     C* INPUT
502600980706     C* . PTPG ->   TIPO APPLICAZIONE
502700980706     C* . PITR ->   IMPORTO TARIFFA CALCOLATA
502800980706     C* . PVARI ->  VARIA CALCOLATA, MA NON ARROTONDATA
502900980706     C* . PVARIA -> VARIA CALCOLATA, E ARROTONDATA
503000980706     C*---
503100980706     C* REIMPOSTA LE VARIABILI DI LAVORO
503200011031     C                   Z-ADD     *ZEROS        TOTVAR           21 8          *VARIA
503300980706     C*
503400980706     C* SE TIPO APPLICAZIONE T' E HA CALCOLATO QUALCOSA
503500980706IF  1C     PTPG          IFEQ      'T'                                          *TARIFFA A TRASPORTO
503600980706     C     PITR          ANDGT     *ZEROS                                       *TARIFFA DA CALCOLAR
503700980706     C* PORTO
503800980706IF  2C     VIMP(1)       IFGT      *ZEROS
503900011031     C                   Z-ADD     VIMP(1)       IMPOR            21 8          *IMPORTO VOCE
504000980706     C                   Z-ADD     VP(1)         PERCEN            9 5          *% INCIDENZA
504100011031     C                   Z-ADD     *ZEROS        VOCE             21 8
504200980706     C                   EXSR      CALTR2
504300980706     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
504400980706E   2C                   ENDIF
504500980706     C* INOLTRO
504600980706IF  2C     VIMP(6)       IFGT      *ZEROS
504700011031     C                   Z-ADD     VIMP(6)       IMPOR            21 8          *IMPORTO VOCE
504800980706     C                   Z-ADD     VP(6)         PERCEN            9 5          *% INCIDENZA
504900011031     C                   Z-ADD     *ZEROS        VOCE             21 8
505000980706     C                   EXSR      CALTR2
505100980706     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
505200980706E   2C                   ENDIF
505300980706     C* LOCALITA' DISAGIATE
505400980706IF  2C     VIMP(35)      IFGT      *ZEROS
505500011031     C                   Z-ADD     VIMP(35)      IMPOR            21 8          *IMPORTO VOCE
505600980706     C                   Z-ADD     VP(35)        PERCEN            9 5          *% INCIDENZA
505700011031     C                   Z-ADD     *ZEROS        VOCE             21 8
505800980706     C                   EXSR      CALTR2
505900980706     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
506000980706E   2C                   ENDIF
506100980706     C* ESPRESSO PORTO
506200980706IF  2C     VIMP(7)       IFGT      *ZEROS
506300011031     C                   Z-ADD     VIMP(7)       IMPOR            21 8          *IMPORTO VOCE
506400980706     C                   Z-ADD     VP(7)         PERCEN            9 5          *% INCIDENZA
506500011031     C                   Z-ADD     *ZEROS        VOCE             21 8
506600980706     C                   EXSR      CALTR2
506700980706     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
506800980706E   2C                   ENDIF
506900980706     C* ESPRESSO INOLTRO
507000980706IF  2C     VIMP(36)      IFGT      *ZEROS
507100011031     C                   Z-ADD     VIMP(36)      IMPOR            21 8          *IMPORTO VOCE
507200980706     C                   Z-ADD     VP(36)        PERCEN            9 5          *% INCIDENZA
507300011031     C                   Z-ADD     *ZEROS        VOCE             21 8
507400980706     C                   EXSR      CALTR2
507500980706     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
507600980706E   2C                   ENDIF
507700980706     C* ESPRESSO LOCALITA' DISAGIATE
507800980706IF  2C     VIMP(39)      IFGT      *ZEROS
507900011031     C                   Z-ADD     VIMP(39)      IMPOR            21 8          *IMPORTO VOCE
508000980706     C                   Z-ADD     VP(39)        PERCEN            9 5          *% INCIDENZA
508100011031     C                   Z-ADD     *ZEROS        VOCE             21 8
508200980706     C                   EXSR      CALTR2
508300980706     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
508400980706E   2C                   ENDIF
508500011030     C* CONSEGNA ISOLA
508600011030IF  2C     VIMP(80)      IFGT      *ZEROS
508700011031     C                   Z-ADD     VIMP(80)      IMPOR            21 8          *IMPORTO VOCE
508800011030     C                   Z-ADD     VP(80)        PERCEN            9 5          *% INCIDENZA
508900011031     C                   Z-ADD     *ZEROS        VOCE             21 8
509000011030     C                   EXSR      CALTR2
509100011030     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
509200011030E   2C                   ENDIF
509300011030     C* ESPRESSO CONSEGNA ISOLA
509400011030IF  2C     VIMP(81)      IFGT      *ZEROS
509500011031     C                   Z-ADD     VIMP(81)      IMPOR            21 8          *IMPORTO VOCE
509600011030     C                   Z-ADD     VP(81)        PERCEN            9 5          *% INCIDENZA
509700011031     C                   Z-ADD     *ZEROS        VOCE             21 8
509800011030     C                   EXSR      CALTR2
509900011030     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
510000011030E   2C                   ENDIF
510100011030     C*
510200980706     C                   Z-ADD     TOTVAR        VARIA                          *VOCE TOTALE
510300980706E   1C                   ENDIF
510400980706     C*
510500980706     C                   ENDSR
510600980706     C*------------------------------------------------------------------------*
510700980706     C* CALTR2 - ESEGUE CALCOLI VOCE CALCOLATA SU UNA % DEL TRASPORTO
510800980706     C*------------------------------------------------------------------------*
510900980706     C     CALTR2        BEGSR
511000980706     C*---
511100980706     C* INPUT
511200980706     C* . PVARI  ->  VARIA CALCOLATA, MA NON ARROTONDATA
511300980706     C* . PVARIA -> VARIA CALCOLATA, E ARROTONDATA
511400980706     C* . IMPOR  -> IMPORTO VOCE
511500980706     C* . PERCEN -> % INCIDENZA
511600980706     C* OUTPUT
511700980706     C* . VOCE   -> VOCE CALCOLATA
511800980706     C*---
511900980706     C* ES. DA CALCOLARE VARIA = 10% DEL TRASPORTO
512000980706     C*   . VARIA CALCOLATA CON ARROTONDAMENTI = 2.000
512100980706     C*   . VARIA CALCOLATA SENZA ARROTONDAMENTI = 1.600
512200980706     C*   . PORTO = 10.000                         INCIDENZA 100%
512300980706     C*   . INOLTRO = 5.000                        INCIDENZA   5%
512400980706     C*   . LOCALITA' DISAGIATA = 1.000            INCIDENZA  20%
512500980706     C*   . ESPRESSO PORTO = 300                   INCIDENZA  15%
512600980706     C*   . ESPRESSO INOLTRO = 150                 INCIDENZA 15% SU  5% = 0,75%
512700980706     C*   . ESPRESSO LOCALITA' DISAGIATA = 30      INCIDENZA 15% SU 20% = 3%
512800011030     C*   . CONSEGNA ISOLA               = 2.500   INCIDENZA  30%
512900011030     C*   . ESPRESSO CONSEGNA ISOLA      = 30      INCIDENZA 15% SU 30% = 4,5%
513000980706     C* LA SOMMA DI TUTTE LE VOCI DA LA VARIA
513100011030     C* Esempio -->
513200980706     C*---
513300980706     C* % "T" DA CALCOLARE SULLA LOCALITA' DISAGIATA
513400980706     C* 10% DI 1.000 = 100
513500980706     C*---
513600011031     C     IMPOR         MULT      PITR          N11              23 8
513700011031     C     N11           DIV       100           VOCPT            23 8          *VOCE % SUL TRASPORT
513800980706     C*---
513900980706     C* VOCE TENUTO CONTO DELL'ARROTONDAMENTO -->
514000980706     C* 100 : 1.600 = X : 2.000
514100980706     C* X = 125
514200980706     C*---
514300980706     C     VOCPT         MULT      PVARIA        VOCPT
514400011031     C     VOCPT         DIV       PVARI         VOCPTA           25 8          *VOCE AL 100%
514500980706     C*---
514600980706     C* VOCE TENUTO CONTO DELLA SUA % DI INCIDENZA ->
514700980706     C* 20% DI 125 = 25
514800980706     C*---
514900011031     C     VOCPTA        MULT      PERCEN        N19              25 8
515000011031     C     N19           DIV(H)    100           VOCE             21 8          *VOCE ALLA %INCIDENZ
515100980706     C*
515200980706     C                   ENDSR
515300970306     C*------------------------------------------------------------------------*
515400970306     C* CALTOT - CALCOLA IMPORTI SULLE % INCIDENZA E I RAGGUPPAMENTI DELTA
515500970306     C*------------------------------------------------------------------------*
515600970306     C     CALTOT        BEGSR
515700970306     C*
515800970306     C* REIMPOSTA LE VARIABILI DI LAVORO
515900970311     C                   Z-ADD     *ZEROS        IRDL                           *RAGGRUPPAMENTI DELT
516000970311     C                   Z-ADD     *ZEROS        VIM                            *IMPORTI DA SCRIVERE
516100011031     C                   Z-ADD     *ZEROS        WVVOCE           25 8          *IMPORTO VOCE
516200970306     C*
516300970311     C* CICLO PER TUTTE LE VOCI DI TASSAZIONE
516400970306DO  1C     1             DO        NVOCA         V                 6 0
516500970311IF  2C     VIMP(V)       IFGT      *ZEROS
516600970306     C*
516700970311     C* APPLICA LA % DI INCIDENZA
516800970306     C                   Z-ADD     *ZEROS        WVVOCE
516900970306IF  3C     VP(V)         IFLT      100
517000970311     C     VIMP(V)       DIV       100           WVVOCE
517100970306     C     WVVOCE        MULT      VP(V)         VIM(V)
517200970311X   3C                   ELSE
517300970311     C                   Z-ADD     VIMP(V)       VIM(V)
517400970306E   3C                   ENDIF
517500970306     C*
517600970306     C* SE LA VOCE E' DA INGLOBARE NEL PORTO FORZA IL RAGGRUPPAMENTO PORTO
517700970306IF  3C     VI(V)         IFEQ      'S'
517800970306     C                   Z-ADD     001           VR(V)
517900970318E   3C                   ENDIF
518000970306     C*
518100970311     C* IMPOSTA IL RAGGRUPPAMENTO DELTA
518200970306     C                   Z-ADD     VR(V)         R                 6 0          *INDICE  RAGGRUPP.TO
518300970306     C                   ADD       VIM(V)        IRDL(R)                        *IMPORTO RAGGRUPP.TO
518400970306E   2C                   ENDIF
518500970306E   1C                   ENDDO                                                  *FINE VOCI
518600970306     C*
518700970306     C                   ENDSR
518800970306     C*------------------------------------------------------------------------*
518900970306     C* SCRETW - SCRIVE SU FILE DI LAVORO I RICAVI
519000970306     C*------------------------------------------------------------------------*
519100970306     C     SCRETW        BEGSR
519200970311     C*
519300970311     C* CICLO PER TUTTI I RAGGRUPPAMENTI DELTA
519400970306DO  1C     1             DO        50            R
519500970306IF  2C     IRDL(R)       IFGT      *ZEROS
519600970306     C     R             OREQ      1
519700980909     C******************** MOVELUSER      KTWUSR           *UTENTE
519800980909     C                   MOVEL     JOB           KTWUSR                         *UTENTE
519900980603     C                   MOVEL     D00DSF        KTWDSF                         *DATI SEDE O FILIALE
520000980603     C                   MOVEL     D00CTO        KTWCTO                         *TARIFFA O OFFERTA
520100970306     C                   Z-ADD     D00KSC        KTWKSC                         *CLIENTE O VISITA
520200970306     C                   Z-ADD     D00CTR        KTWCTR                         *TARIFFA
520300970306     C                   Z-ADD     D00PRG        KTWPRG                         *PROGRESSIVO
520400970306     C                   Z-ADD     SSGL(S)       KTWSGL                         *SCAGLIONE
520500970306     C                   MOVEL     CCTS(T)       KTWCTS                         *CODICE TASSAZIONE
520600970306     C                   Z-ADD     R             KTWRDL                         *RAGGRUP.DELTA
520700010926     C     KEYETW        CHAIN     TEETW01L                           99
520800970306     C* MODIFICA
520900970306IF  3C     *IN99         IFEQ      *OFF
521000970306     C                   ADD       IRDL(R)       ETWRIC                         *RICAVO
521100970317IF  4C     D00TLA        IFEQ      'D'                                          *LANCIO DELTA
521200970317     C                   Z-ADD     *ZEROS        ETWCOM                         *AZZERA COMPETENZE
521300970317E   4C                   ENDIF
521400010926     C                   UPDATE    TEETW000
521500970306     C* INSERIMENTO
521600970306X   3C                   ELSE
521700970306     C                   MOVEL     KTWUSR        ETWUSR                         *UTENTE
521800980603     C                   MOVEL     KTWDSF        ETWDSF                         *DATI SEDE O FILIALE
521900970306     C                   MOVEL     KTWCTO        ETWCTO                         *TARIFFA O OFFERTA
522000970306     C                   Z-ADD     KTWKSC        ETWKSC                         *CLIENTE O VISITA
522100970306     C                   Z-ADD     KTWCTR        ETWCTR                         *TARIFFA
522200970306     C                   Z-ADD     KTWPRG        ETWPRG                         *PROGRESSIVO
522300970306     C                   Z-ADD     KTWSGL        ETWSGL                         *SCAGLIONE
522400970306     C                   MOVEL     KTWCTS        ETWCTS                         *CODICE TASSAZIONE
522500970306     C                   Z-ADD     KTWRDL        ETWRDL                         *RAGGRUP.DELTA
522600970306     C                   Z-ADD     IRDL(R)       ETWRIC                         *RICAVO
522700011031     C     CPPE(T)       DIV       100           WPERC            21 8
522800970306     C     WPERC         MULT      SPPE(S)       ETWPPE                         *% INCIDENZA
522900970306     C                   MOVEL     CRCT(T)       ETWRCT                         *RAGGRUPP.COD.TASSAZ
523000970306     C                   MOVEL     CRAP(T)       ETWRAP                         *REGIONE CODICE
523100970306     C                   MOVEL     CNAR(T)       ETWNAR                         *NAZIONE CODICE
523200970306     C                   Z-ADD     *ZEROS        ETWCOM                         *COMPETENZE
523300010926     C                   WRITE     TEETW000
523400970306E   3C                   ENDIF
523500970306E   2C                   ENDIF
523600970306E   1C                   ENDDO                                                  *FINE RAGGRUPPAMENTI
523700970306     C*
523800970306     C                   ENDSR
523900970306     C*------------------------------------------------------------------------*
524000970306     C* SCREWR - SCRIVE SU FILE DI LAVORO IL DETTAGLIO DEI RICAVI -SE IN DEBUG-
524100970306     C*------------------------------------------------------------------------*
524200970306     C     SCREWR        BEGSR
524300970311     C*
524400970311     C* CICLO PER TUTTE LE VOCI DI TASSAZIONE
524500970306DO  1C     1             DO        NVOCA         V
524600970306IF  2C     VIM(V)        IFGT      *ZEROS
524700980909     C******************** MOVELUSER      EWRUSR           *UTENTE
524800980909     C                   MOVEL     JOB           EWRUSR                         *UTENTE
524900980603     C                   MOVEL     D00DSF        EWRDSF                         *DATI SEDE O FILIALE
525000970306     C                   MOVEL     D00CTO        EWRCTO                         *TARIFFA O OFFERTA
525100970306     C                   Z-ADD     D00KSC        EWRKSC                         *CLIENTE
525200970306     C                   Z-ADD     D00CTR        EWRCTR                         *TARIFFA
525300970306     C                   Z-ADD     D00PRG        EWRPRG                         *PROGRESSIVO
525400970306     C                   MOVEL     CCTS(T)       EWRCTS                         *CODICE TASSAZIONE
525500970306     C                   Z-ADD     SSGL(S)       EWRSGL                         *SCAGLIONE
525600970306     C                   Z-ADD     V             EWRVOC                         *VOCE
525700970306     C                   Z-ADD     VIM(V)        EWRIMP                         *IMPORTO VOCE
525800970306     C                   Z-ADD     VR(V)         EWRRDL                         *RAGGRUPPAMENTO DELT
525900010926     C                   WRITE     TEETWR00
526000970306E   2C                   ENDIF
526100970306E   1C                   ENDDO                                                  *FINE VOCI
526200970306     C*
526300970306     C                   ENDSR
526400970306     C*------------------------------------------------------------------------*
526500970306     C* CANCWF - CANCELLA IL FILE DI LAVORO
526600970306     C*------------------------------------------------------------------------*
526700970306     C     CANCWF        BEGSR
526800970317     C*
526900010926     C* CANCELLA I RECORD DELL'UTENTE NEL FILE TEETW00F
527000970317IF  1C     D00TLA        IFEQ      'D'                                          *LANCIO DELTA
527100980909     C******************** MOVELUSER      KTWUSR
527200980909     C                   MOVEL     JOB           KTWUSR
527300010926     C     KE2ETW        SETLL     TEETW01L
527400010926     C     KE2ETW        READE     TEETW01L                               99
527500970317DO  2C     *IN99         DOWEQ     *OFF
527600010926     C                   DELETE    TEETW000
527700010926     C     KE2ETW        READE     TEETW01L                               99
527800970317E   2C                   ENDDO
527900970317E   1C                   ENDIF
528000970306     C*
528100010926     C* CANCELLA I RECORD DELL'UTENTE NEL FILE TEETWR0F
528200980909     C******************** MOVELUSER      KWRUSR
528300980909     C                   MOVEL     JOB           KWRUSR
528400010926     C     KEYEWR        SETLL     TEETWR1L
528500010926     C     KEYEWR        READE     TEETWR1L                               99
528600970306DO  1C     *IN99         DOWEQ     *OFF
528700010926     C                   DELETE    TEETWR00
528800010926     C     KEYEWR        READE     TEETWR1L                               99
528900970306E   1C                   ENDDO
529000970306     C*
529100970306     C                   ENDSR
529200970306     C*------------------------------------------------------------------------*
529300970306     C* CARTAB - CARICA LE TABELLE
529400970306     C*------------------------------------------------------------------------*
529500970306     C     CARTAB        BEGSR
529600970306     C*
529700970306     C* CARICAMENTO DATI VARI TASSAZIONE -UNO-
529800970306     C                   EXSR      CARQT1
529900970306     C*
530000970306     C* CARICAMENTO ISTAT -UNO-
530100970306     C                   EXSR      CARQI1
530200970306     C*
530300970306     C* CARICAMENTO ISTAT -DUE-
530400970306     C                   EXSR      CARQI2
530500970306     C*
530600970306     C* CARICAMENTO TIPI TARIFFA
530700970306     C                   EXSR      CARTR
530800970306     C*
530900970306     C* CARICAMENTO SIGLE VARIE
531000970306     C                   EXSR      CAR$2
531100970306     C*
531200970306     C* CARICAMENTO CODICI TASSAZIONE
531300970306     C                   EXSR      CARCT
531400970306     C*
531500970306     C* CARICAMENTO TIPI SERVIZIO
531600970306     C                   EXSR      CAR5E
531700970306     C*
531800970306     C* CARICAMENTO TARIFFE PARTICOLARI -DI CARTELLO-
531900970306     C                   EXSR      CARPAR
532000970306     C*
532100970306     C                   ENDSR
532200970306     C*------------------------------------------------------------------------*
532300970306     C* CARQT1 - CARICAMENTO TABELLA DATI VARI DI TASSAZIONE -UNO-
532400970306     C*------------------------------------------------------------------------*
532500970306     C     CARQT1        BEGSR
532600021202     C*
532700021202     C                   CLEAR                   TRUL27DS
532800021202     C                   EVAL      I27TLA = 'L'
532900021202     C                   EVAL      I27DTA = DATCOR
533000021202     C                   CALL      'TRUL27R'
533100021202     C                   PARM                    TRUL27DS
533200021202     C                   IF        O27ERR = 'E'
533300021202     C                   MOVEL     MSG(11)       $MSG
533400021202     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
533500021202     C                   ENDIF
533600970306     C*
533700970306     C                   ENDSR
533800991125     C*------------------------------------------------------------------------*
533900970306     C* CARQI1 - CARICAMENTO ISTAT -UNO-
534000970306     C*------------------------------------------------------------------------*
534100970306     C     CARQI1        BEGSR
534200970306     C*
534300020906     C                   Z-ADD     1             KTBKUT
534400970306     C                   MOVEL     'QI'          KTBCOD
534500970306     C                   MOVEL(P)  '1'           KTBKEY
534600970306     C     KEYTAB        CHAIN     TABEL00F                           98
534700970306IF  1C     *IN98         IFEQ      *ON
534800970306     C                   MOVEL     MSG(13)       $MSG
534900970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
535000970306X   1C                   ELSE
535100970306     C                   MOVEL     TBLUNI        DSQI1
535200970306     C                   Z-ADD     §QID1T        DIA(1)                         *DATE SCATTO
535300970306     C                   Z-ADD     §QID2T        DIA(2)
535400970306     C                   Z-ADD     §QID3T        DIA(3)
535500970306     C                   Z-ADD     §QID4T        DIA(4)
535600970306     C                   Z-ADD     §QID5T        DIA(5)
535700970306     C                   Z-ADD     §QID6T        DIA(6)
535800970306     C                   Z-ADD     §QII1T        IAC(1)                         *PUNTI
535900970306     C                   Z-ADD     §QII2T        IAC(2)
536000970306     C                   Z-ADD     §QII3T        IAC(3)
536100970306     C                   Z-ADD     §QII4T        IAC(4)
536200970306     C                   Z-ADD     §QII5T        IAC(5)
536300970306     C                   Z-ADD     §QII6T        IAC(6)
536400970306E   1C                   ENDIF
536500970306     C*
536600970306     C                   ENDSR
536700970306     C*------------------------------------------------------------------------*
536800970306     C* CARQI2 - CARICAMENTO ISTAT -DUE-
536900970306     C*------------------------------------------------------------------------*
537000970306     C     CARQI2        BEGSR
537100970306     C*
537200020906     C                   Z-ADD     1             KTBKUT
537300970306     C                   MOVEL     'QI'          KTBCOD
537400970306     C                   MOVEL(P)  '2'           KTBKEY
537500970306     C     KEYTAB        CHAIN     TABEL00F                           98
537600970306IF  1C     *IN98         IFEQ      *ON
537700970306     C                   MOVEL     MSG(14)       $MSG
537800970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
537900970306X   1C                   ELSE
538000970306     C                   MOVEL     TBLUNI        DSQI2
538100970306     C                   Z-ADD     §QID7T        DIA(7)                         *DATE SCATTO
538200970306     C                   Z-ADD     §QID8T        DIA(8)
538300970306     C                   Z-ADD     §QID9T        DIA(9)
538400970306     C                   Z-ADD     §QID0T        DIA(10)
538500970306     C                   Z-ADD     §QID11        DIA(11)
538600970306     C                   Z-ADD     §QID12        DIA(12)
538700970306     C                   Z-ADD     §QII7T        IAC(7)                         *PUNTI
538800970306     C                   Z-ADD     §QII8T        IAC(8)
538900970306     C                   Z-ADD     §QII9T        IAC(9)
539000970306     C                   Z-ADD     §QII0T        IAC(10)
539100970306     C                   Z-ADD     §QII11        IAC(11)
539200970306     C                   Z-ADD     §QII12        IAC(12)
539300970306E   1C                   ENDIF
539400970306     C*
539500970306     C                   ENDSR
539600970306     C*------------------------------------------------------------------------*
539700970306     C* CARTR - CARICAMENTO TIPI TARIFFA
539800970306     C*------------------------------------------------------------------------*
539900970306     C     CARTR         BEGSR
540000970306     C*
540100980506DO  1C     1             DO        15            I
540200970306     C     I             OCCUR     DSTR
540300970306     C                   CLEAR                   DSTR                           *AZZERAMENTO
540400970306E   1C                   ENDDO
540500980707     C                   MOVEL     *BLANKS       CTR
540600970306     C*
540700970306     C                   Z-ADD     *ZEROS        I
540800020906     C                   Z-ADD     1             KTBKUT
540900970306     C                   MOVEL     'TR'          KTBCOD
541000970306     C     KE2TAB        SETLL     TABEL00F
541100970306     C     KE2TAB        READE     TABEL00F                               98
541200970306IF  1C     *IN98         IFEQ      *ON
541300970306     C                   MOVEL     MSG(15)       $MSG
541400970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
541500970306X   1C                   ELSE
541600970306DO  2C     *IN98         DOWEQ     *OFF
541700970306IF  3C     TBLKEY        IFNE      *BLANKS
541800970312IF  4C     TBLFLG        IFEQ      *BLANKS
541900970306     C                   ADD       1             I
542000970306     C                   MOVEL     TBLKEY        CTR(I)
542100970306     C     I             OCCUR     DSTR
542200970306     C                   MOVEL     TBLUNI        DSTR
542300970312E   4C                   ENDIF
542400970312E   3C                   ENDIF
542500970306     C     KE2TAB        READE     TABEL00F                               98
542600970306E   2C                   ENDDO
542700970306E   1C                   ENDIF
542800970306     C*
542900970306     C                   ENDSR
543000970306     C*------------------------------------------------------------------------*
543100970306     C* CAR$2 - CARICAMENTO SIGLE VARIE
543200970306     C*------------------------------------------------------------------------*
543300970306     C     CAR$2         BEGSR
543400970306     C*
543500970306     C                   CLEAR                   DS$2                           *AZZERAMENTO
543600970306     C*
543700020906     C                   Z-ADD     1             KTBKUT
543800970306     C                   MOVEL     '$2'          KTBCOD
543900970306     C                   MOVEL(P)  '1'           KTBKEY
544000970306     C     KEYTAB        CHAIN     TABEL00F                           98
544100970306IF  1C     *IN98         IFEQ      *ON
544200970306     C                   MOVEL     MSG(16)       $MSG
544300970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
544400970306X   1C                   ELSE
544500970306     C                   MOVEL     TBLUNI        DS$2
544600970306E   1C                   ENDIF
544700970306     C*
544800970306     C                   ENDSR
544900970306     C*------------------------------------------------------------------------*
545000970306     C* CARCT - CARICAMENTO CODICI TASSAZIONE
545100970306     C*------------------------------------------------------------------------*
545200970306     C     CARCT         BEGSR
545300970306     C*
545400050103DO  1C     1             DO        600           I
545500970306     C     I             OCCUR     DSCT
545600970306     C                   CLEAR                   DSCT                           *AZZERAMENTO
545700970306E   1C                   ENDDO
545800970306     C*
545900970306     C                   Z-ADD     *ZEROS        I
546000020906     C                   Z-ADD     1             KTBKUT
546100970306     C                   MOVEL     'CT'          KTBCOD
546200970306     C     KE2TAB        SETLL     TABEL00F
546300970306     C     KE2TAB        READE     TABEL00F                               98
546400970306IF  1C     *IN98         IFEQ      *ON
546500970306     C                   MOVEL     MSG(18)       $MSG
546600970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
546700970306X   1C                   ELSE
546800970306DO  2C     *IN98         DOWEQ     *OFF
546900970306IF  3C     TBLKEY        IFNE      *BLANKS
547000970312IF  4C     TBLFLG        IFEQ      *BLANKS
547100970306     C                   ADD       1             I
547200970306     C                   MOVEL     TBLKEY        CTS(I)
547300970306     C     I             OCCUR     DSCT
547400970306     C                   MOVEL     TBLUNI        DSCT
547500970312IF  5C     §CTUTC        IFEQ      'N'                                          *DA NON CONSIDERARE
547600970312     C                   CLEAR                   DSCT                           *PULISCE
547700970312     C     I             SUB       1             I                              *TORNA INDIETRO DI 1
547800970312E   5C                   ENDIF
547900970312E   4C                   ENDIF
548000970312E   3C                   ENDIF
548100970306     C     KE2TAB        READE     TABEL00F                               98
548200970306E   2C                   ENDDO
548300970306E   1C                   ENDIF
548400970306     C*
548500970306     C                   ENDSR
548600970306     C*------------------------------------------------------------------------*
548700970306     C* CAR5E - CARICAMENTO TIPI SERVIZIO
548800970306     C*------------------------------------------------------------------------*
548900970306     C     CAR5E         BEGSR
549000970306     C*
549100970306DO  1C     1             DO        10            I
549200970306     C     I             OCCUR     DS5E
549300970306     C                   CLEAR                   DS5E                           *AZZERAMENTO
549400970306E   1C                   ENDDO
549500970306     C*
549600970306     C                   Z-ADD     *ZEROS        I
549700020906     C                   Z-ADD     1             KTBKUT
549800970306     C                   MOVEL     '5E'          KTBCOD
549900970306     C     KE2TAB        SETLL     TABEL00F
550000970306     C     KE2TAB        READE     TABEL00F                               98
550100970306IF  1C     *IN98         IFEQ      *ON
550200970306     C                   MOVEL     MSG(19)       $MSG
550300970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
550400970306X   1C                   ELSE
550500970306DO  2C     *IN98         DOWEQ     *OFF
550600970306IF  3C     TBLKEY        IFNE      *BLANKS
550700970312IF  4C     TBLFLG        IFEQ      *BLANKS
550800970306     C                   ADD       1             I
550900970306     C                   MOVEL     TBLKEY        TS(I)
551000970306     C     I             OCCUR     DS5E
551100970306     C                   MOVEL     TBLUNI        DS5E
551200970312E   4C                   ENDIF
551300970312E   3C                   ENDIF
551400970306     C     KE2TAB        READE     TABEL00F                               98
551500970306E   2C                   ENDDO
551600970306E   1C                   ENDIF
551700970306     C*
551800970306     C                   ENDSR
551900970306     C*------------------------------------------------------------------------*
552000970306     C* CARPAR - CARICAMENTO TARIFFE PARTICOLARI -DI CARTELLO-
552100970306     C*------------------------------------------------------------------------*
552200970306     C     CARPAR        BEGSR
552300970306     C*---
552400970306     C* MEMORIZZA I DATI ASSICURAZIONE X CONTO / ISOLA / CONSEGNE PARTICOLARI
552500970306     C* DELLA TARIFFA DI CARTELLO IN VIGORE ALLA DATA DI FATTURAZIONE
552600970306     C*---
552700021202     C                   Z-ADD     O27KSC        KSC1                           *IMPOSTA CARTELLO
552800970306     C                   Z-ADD     *ZEROS        CTR1
552900011019     C                   EXSR      CERTAM                                       *CERCA TARIFFA
553000970306     C*
553100970306     C* MEMORIZZA LA CHIAVE DELLA TARIFFA DI CARTELLO IN VIGORE
553200970306IF  1C     WERR          IFEQ      'N'                                          *NO ERRORE
553300970306     C                   Z-ADD     TAMKSC        CARKSC
553400970306     C                   Z-ADD     TAMCTR        CARCTR
553500970306     C                   Z-ADD     TAMPRG        CARPRG
553600011019     C*
553700011023     C* RECUPERA LA DIVISA DELLA TARIFFA DI CARTELLO
553800011023     C                   CLEAR                   DSTA01
553900011023     C                   MOVEL     TAMFLO        DSTA01
554000011023     C                   MOVEL     §TADIV        CARDIV            3            *DIVISA TARIFFA
554100970306     C*
554200970306     C* CARICA LE TARIFFE PARTICOLARI DI CARTELLO
554300970306     C                   EXSR      CAR1P
554400970306X   1C                   ELSE                                                   *ERRORE
554500970306     C                   MOVEL     MSG(22)       $MSG
554600970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
554700970306E   1C                   ENDIF
554800970306     C*
554900970306     C                   ENDSR
555000970306     C*------------------------------------------------------------------------*
555100970306     C* CERTAM - CERCA UN TARIFFA
555200970306     C*------------------------------------------------------------------------*
555300970306     C     CERTAM        BEGSR
555400970306     C*
555500970306     C* REIMPOSTA LE VARIABILI DI LAVORO
555600970306     C                   Z-ADD     *ZEROS        DEPKSC                         *CAMPI DEPOSITO
555700970306     C                   Z-ADD     *ZEROS        DEPPRG
555800970306     C                   Z-ADD     *ZEROS        DEPCTR
555900970306     C                   MOVEL     '0'           $TAR              1            TARIFFA IN DECORRENZ
556000970306     C                   MOVEL     '0'           $SCA              1            TARIFFA SCADUTA
556100970306     C                   MOVEL     'N'           WERR                           *NO ERRORE
556200970306     C*
556300970306     C                   Z-ADD     KSC1          KTMKSC                         *CLIENTE
556400970306     C                   Z-ADD     CTR1          KTMCTR                         *TARIFFA
556500970306IF  1C     D00CTO        IFEQ      'O'                                          *OFFERTE
556600970306     C     KEYTAM        SETLL     TNOFM00L                               99
556700970306X   1C                   ELSE                                                   *TARIFFE
556800970306     C     KEYTAM        SETLL     TNTAM00L                               99
556900970306E   1C                   ENDIF
557000970306IF  1C     *IN99         IFEQ      *OFF                                         *NON TROVATA
557100970306     C                   MOVEL     'S'           WERR                           *MANCA TARIFFA
557200970306X   1C                   ELSE
557300970306IF  2C     D00CTO        IFEQ      'O'                                          *OFFERTE
557400970306     C     KEYTAM        READE     TNOFM00L                               99
557500970306X   2C                   ELSE                                                   *TARIFFE
557600970306     C     KEYTAM        READE     TNTAM00L                               99
557700970306E   2C                   ENDIF
557800970306DO  2C     *IN99         DOWEQ     *OFF                                         *FINE FILE
557900970306     C     $TAR          ANDEQ     '0'                                          *TROVATA TAR.IN DEC.
558000970306     C     WERR          ANDEQ     'N'                                          *NO MANCA TARIFFA
558100970306IF  3C     TAMATB        IFEQ      *BLANKS                                      *NO ANNULLATE
558200970306IF  4C     D00DTC        IFGE      TAMDDT                                       TARIFFA IN DECORRENZ
558300970306     C     D00DTC        ANDLE     TAMDST
558400970306     C                   MOVEL     '1'           $TAR                           *TAR.IN DEC.TROVATA
558500970306     C                   Z-ADD     TAMKSC        DEPKSC                         *CHIAVE DEPOSITATA
558600970306     C                   Z-ADD     TAMCTR        DEPCTR
558700970306     C                   Z-ADD     TAMPRG        DEPPRG
558800970306X   4C                   ELSE                                                   TARIFFA NON IN DECOR
558900970306IF  5C     D00DTC        IFLT      TAMDDT                                       *TARIFFA DA DECORRER
559000970306IF  6C     $SCA          IFEQ      '0'                                          *NON ESISTE TAR.SCAD
559100970306     C                   MOVEL     'S'           WERR                           *MANCA TARIFFA
559200970306E   6C                   ENDIF
559300970306E   5C                   ENDIF
559400970306IF  5C     D00DTC        IFGT      TAMDST                                       *TARIFFA SCADUTA
559500970306     C                   MOVEL     '1'           $SCA                           *TAR.SCADUTA TROVATA
559600970306     C                   Z-ADD     TAMKSC        DEPKSC                         *CHIAVE DEPOSITATA
559700970306     C                   Z-ADD     TAMCTR        DEPCTR
559800970306     C                   Z-ADD     TAMPRG        DEPPRG
559900970306E   5C                   ENDIF
560000970306E   4C                   ENDIF
560100970306E   3C                   ENDIF
560200970306IF  3C     D00CTO        IFEQ      'O'                                          *OFFERTE
560300970306     C     KEYTAM        READE     TNOFM00L                               99
560400970306X   3C                   ELSE                                                   *TARIFFE
560500970306     C     KEYTAM        READE     TNTAM00L                               99
560600970306E   3C                   ENDIF
560700970306E   2C                   ENDDO                                                  *FINE TARIFFE
560800970306E   1C                   ENDIF
560900970306     C                   Z-ADD     DEPKSC        TAMKSC                         *CHIAVE DEPOSITATA
561000970306     C                   Z-ADD     DEPCTR        TAMCTR
561100970306     C                   Z-ADD     DEPPRG        TAMPRG
561200970306     C*
561300970306     C                   ENDSR
561400970306     C*------------------------------------------------------------------------*
561500970306     C* CAR1P - CARICAMENTO TARIFFE PARTICOLARI DI CARTELLO
561600970306     C*------------------------------------------------------------------------*
561700970306     C     CAR1P         BEGSR
561800970306     C*
561900020104DO  1C     1             DO        50            I
562000970306     C     I             OCCUR     CPIDS
562100970306     C                   CLEAR                   CPIDS                          *AZZERAMENTO
562200970306     C     I             OCCUR     DS1P
562300970306     C                   CLEAR                   DS1P                           *AZZERAMENTO
562400970306E   1C                   ENDDO
562500970306     C                   MOVEL     *BLANKS       CP
562600970306     C*
562700970306     C* PER CIASCUNA TARIFFA PARTICOLARE IN TABELLA
562800970306     C                   Z-ADD     *ZEROS        I
562900020906     C                   Z-ADD     1             KTBKUT
563000970306     C                   MOVEL     '1P'          KTBCOD
563100970306     C     KE2TAB        SETLL     TABEL00F
563200970306     C     KE2TAB        READE     TABEL00F                               98
563300970306IF  1C     *IN98         IFEQ      *ON
563400970306     C                   MOVEL     MSG(20)       $MSG
563500970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
563600970306X   1C                   ELSE
563700970306DO  2C     *IN98         DOWEQ     *OFF
563800970306IF  3C     TBLKEY        IFNE      *BLANKS
563900970312IF  4C     TBLFLG        IFEQ      *BLANKS
564000970306     C                   MOVEL     TBLUNI        CAMPO            26
564100970306     C                   MOVE      CAMPO         WFCP              1
564200970312IF  5C     WFCP          IFEQ      'S'                                          *TARIFFE PART.DA FAT
564300970306     C                   ADD       1             I
564400970306     C                   MOVEL     TBLKEY        CP(I)
564500970306     C     I             OCCUR     DS1P
564600970306     C                   MOVEL     TBLUNI        DS1P
564700970306     C*
564800970306     C* CARICA LE TARIFFE PARTICOLARI DI CARTELLO
564900970306     C                   MOVEL     '0'           $TPT              1            *TAR.PARTIC.TROVATA
565000011023     C                   Z-ADD     TAMKSC        KTPKSC
565100970306     C                   Z-ADD     TAMCTR        KTPCTR
565200970306     C                   Z-ADD     TAMPRG        KTPPRG
565300970306     C                   MOVEL     CP(I)         KTPFTC
565400970311     C     I             OCCUR     CPIDS
565500970312IF  6C     D00CTO        IFEQ      'O'                                          *OFFERTE
565600990819     C     KEYTPT        SETLL     TIOPT01L                               99
565700990819     C     KEYTPT        READE     TIOPT01L                               99
565800970312X   6C                   ELSE                                                   *TARIFFE
565900990819     C     KEYTPT        SETLL     TITPT01L                               99
566000990819     C     KEYTPT        READE     TITPT01L                               99
566100970312E   6C                   ENDIF
566200970312DO  6C     *IN99         DOWEQ     *OFF                                         *FINE FILE
566300970306     C     $TPT          ANDEQ     '0'                                          *TROVATA TARIFFA
566400011023     C*
566500011023     C                   MOVEL     CARDIV        WDIV
566600991125     C*
566700991125     C* CONTROLLA SE CONVERTIRE I CAMPI DI TITPT
566800011026IF  7C     WDIV          IFNE      WDIVCE
566900011023     C                   MOVEL     WDIV          WDVI
567000011023     C                   EXSR      CNVTPT
567100011023E   7C                   ENDIF
567200991125     C*
567300970312IF  7C     TPTATB        IFEQ      *BLANKS
567400970306     C                   MOVEL     '1'           $TPT                           *TAR.PARTIC.TROVATA
567500970306     C                   Z-ADD     TPTRPV        TRPV8P
567600970306     C                   Z-ADD     TPTARF        TARF8P
567700970306     C                   Z-ADD     TPTARL        TARL8P
567800970306     C                   Z-ADD     TPTARO        TARO8P
567900970306     C                   MOVEL     TPTTPG        TTPG8P
568000970306     C                   MOVEL     TPTAPL        TAPL8P
568100970306     C                   MOVEL     §1PCTS        TCTS8P
568200970306     C                   MOVEL     §1PCTE        TCTE8P
568300970312E   7C                   ENDIF
568400970312IF  7C     D00CTO        IFEQ      'O'                                          *OFFERTE
568500990819     C     KEYTPT        READE     TIOPT01L                               99
568600970312X   7C                   ELSE                                                   *TARIFFE
568700990819     C     KEYTPT        READE     TITPT01L                               99
568800970312E   7C                   ENDIF
568900970312E   6C                   ENDDO                                                  *FINE TAR.PARTICOLAR
569000970312IF  6C     $TPT          IFEQ      '0'                                          *TARIFFA NON TROVATA
569100970306     C                   MOVEL     'NO'          CP88
569200970312E   6C                   ENDIF
569300970312E   5C                   ENDIF
569400970312E   4C                   ENDIF
569500970306E   3C                   ENDIF
569600970306     C     KE2TAB        READE     TABEL00F                               98
569700970306E   2C                   ENDDO
569800970306E   1C                   ENDIF
569900970306     C*
570000970306     C                   ENDSR
570100970715     C*------------------------------------------------------------------------*
570200970715     C* REPINP - RECUPERA INTRADAMENTO -PARTENZA-
570300970715     C*------------------------------------------------------------------------*
570400970715     C     REPINP        BEGSR
570500970715     C*
570600970715     C* TERMINAL PARTENZA
570700970715     C                   CLEAR                   DSLV55
570800970715     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
570900970715     C                   MOVEL     'P'           D55TPT                         *ECCEZIONE ARRIVO
571000970715     C                   Z-ADD     D00LNP        D55LIN                         *LINEA
571100970715     C                   Z-ADD     D00DTC        D55DRF                         *DATA RIFERIMENTO
571200970715     C                   CALL      'FNLV55R'
571300970715     C                   PARM                    DSLV55
571400970715IF  1C     D55ERR        IFNE      *BLANKS                                      *NON TROVATO
571500970715     C                   MOVEL     D55MSG        $MSG
571600970715     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
571700970715X   1C                   ELSE                                                   *TROVATO
571800970715     C                   Z-ADD     D55TFP        WTFP                           *TERMINAL PARTENZA
571900970715E   1C                   ENDIF
572000970715     C*
572100970715     C* CODICE TASSAZIONE PARTENZA
572200970715     C                   Z-ADD     D00LNP        KORFIL
572300970715     C     KEYORG        CHAIN     AZORG01L                           98
572400970715IF  1C     *IN98         IFEQ      *ON                                          *NON TROVATO
572500970715     C                   MOVEL     MSG(5)        $MSG
572600970715     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
572700970715X   1C                   ELSE                                                   *TROVATA
572800970721     C                   MOVEL     ORGCTS        WMCT                           *CTS MITTENTE
572900020729     C                   MOVEL     ORGDE3        OG143
573000020729     C*
573100020729     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
573200020729     C                   CLEAR                   DNTW
573300020729     C*
573400020729     C                   MOVEL(P)  'NTW'         KTECOD
573500020729     C                   MOVEL(P)  §OGNTW        KTEKE1
573600020729     C     KEYTBE        CHAIN     TNTBE01L
573700020729     C                   IF        %found(TNTBE01L)
573800020729     C                   MOVEL     TBEUNI        DNTW
573900020729     C                   ENDIF
574000020729     C*
574100020729     C                   MOVEL     §NTWFIE       DLPFL1                         *FLAG ITALIA/ESTERO
574200970715E   1C                   ENDIF
574300970715     C*
574400970715     C                   ENDSR
574500970306     C*------------------------------------------------------------------------*
574600970306     C* CARETC - CARICA I DATI TIPO -GENERALI-
574700970306     C*------------------------------------------------------------------------*
574800970306     C     CARETC        BEGSR
574900970306     C*
575000980603     C                   MOVEL     D00DSF        KTCDSF                         *DATI SEDE O FILIALE
575100970306     C                   MOVEL     D00CTO        KTCCTO                         *TARIFFA O OFFERTA
575200970306     C                   Z-ADD     D00KSC        KTCKSC                         *CLIENTE O VISITA
575300970306     C                   Z-ADD     D00CTR        KTCCTR                         *TARIFFA
575400970306     C                   Z-ADD     D00PRG        KTCPRG                         *PROGRESSIVO
575500970306     C                   Z-ADD     D00LNP        KTCLNP                         *LINEA DI PARTENZA
575600010926     C     KEYETC        CHAIN     TEETC01L                           99
575700970306IF  1C     *IN99         IFEQ      *ON                                          *NON TROVATO
575800970306     C     ETCATB        ORNE      *BLANKS                                      *NO ANNULLATI
575900970306     C                   MOVEL     MSG(1)        $MSG
576000970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
576100011008     C                   ELSE
576200011026     C     D00DIV        IFNE      WDIVCE
576300011019     C                   MOVEL     D00DIV        WDVI
576400011008     C                   EXSR      CNVETC
576500011008     C                   ENDIF
576600970306E   1C                   ENDIF
576700970306     C*
576800970306     C                   ENDSR
576900970306     C*------------------------------------------------------------------------*
577000970306     C* CARETS - CARICA I DATI TIPO -SCAGLIONI-
577100970306     C*------------------------------------------------------------------------*
577200970306     C     CARETS        BEGSR
577300970306     C*
577400970306     C                   MOVEL     '0'           $SGL              1            *SCAGLIONI ESISTENTI
577500970306     C                   Z-ADD     *ZEROS        I                 6 0
577600980603     C                   MOVEL     D00DSF        KTSDSF                         *DATI SEDE O FILIALE
577700970306     C                   MOVEL     D00CTO        KTSCTO                         *TARIFFA O OFFERTA
577800970306     C                   Z-ADD     D00KSC        KTSKSC                         *CLIENTE O VISITA
577900970306     C                   Z-ADD     D00CTR        KTSCTR                         *TARIFFA
578000970306     C                   Z-ADD     D00PRG        KTSPRG                         *PROGRESSIVO
578100970306     C                   Z-ADD     D00LNP        KTSLNP                         *LINEA DI PARTENZA
578200010926     C     KE2ETS        SETLL     TEETS01L
578300010926     C     KE2ETS        READE     TEETS01L                               99
578400970306DO  1C     *IN99         DOWEQ     *OFF
578500970306IF  2C     ETSATB        IFEQ      *BLANKS                                      *NO ANNULLATI
578600011026     C     D00DIV        IFNE      WDIVCE
578700011019     C                   MOVEL     D00DIV        WDVI
578800011008     C                   EXSR      CNVETS
578900011008     C                   ENDIF
579000970306     C                   MOVEL     '1'           $SGL
579100970306     C                   ADD       1             I
579200970306     C                   Z-ADD     ETSSGL        SSGL(I)                        *SCAGLIONE SUPERIORE
579300970306     C                   Z-ADD     ETSPSC        SPSC(I)                        *PESO RIFERIMENTO
579400970306     C                   Z-ADD     ETSCSC        SCSC(I)                        *COLLI RIFERIMENTO
579500970306     C                   Z-ADD     ETSVSC        SVSC(I)                        *VOLUME RIFERIMENTO
579600970318     C                   Z-ADD     ETSINL        SINL(I)                        *INOLTRO
579700970318     C                   Z-ADD     ETSRPV        SRPV(I)                        *RAPPORTO PESO/VOLUM
579800970306     C                   Z-ADD     ETSPPE        SPPE(I)                        *% INCIDENZA
579900970414     C                   MOVEL     ETSSDE        SSDE(I)                        *SEGNO DELTA
580000970414     C                   Z-ADD     ETSPDE        SPDE(I)                        *% DELTA
580100970306E   2C                   ENDIF
580200010926     C     KE2ETS        READE     TEETS01L                               99
580300970306E   1C                   ENDDO
580400970306     C                   Z-ADD     I             WULTSC            6 0
580500970306IF  1C     $SGL          IFEQ      '0'                                          *NON TROVATO
580600970306     C                   MOVEL     MSG(2)        $MSG
580700970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
580800970306E   1C                   ENDIF
580900970306     C*
581000970306     C                   ENDSR
581100970306     C*------------------------------------------------------------------------*
581200970306     C* CARETT - CARICA I DATI TIPO -CODICI TASSAZIONE-
581300970306     C*------------------------------------------------------------------------*
581400970306     C     CARETT        BEGSR
581500970306     C*
581600970306     C                   MOVEL     '0'           $CTS              1            *COD.TASS. ESISTENTI
581700970306     C                   Z-ADD     *ZEROS        I                 6 0
581800980603     C                   MOVEL     D00DSF        KTTDSF                         *DATI SEDE O FILIALE
581900970306     C                   MOVEL     D00CTO        KTTCTO                         *TARIFFA O OFFERTA
582000970306     C                   Z-ADD     D00KSC        KTTKSC                         *CLIENTE O VISITA
582100970306     C                   Z-ADD     D00CTR        KTTCTR                         *TARIFFA
582200970306     C                   Z-ADD     D00PRG        KTTPRG                         *PROGRESSIVO
582300970306     C                   Z-ADD     D00LNP        KTTLNP                         *LINEA DI PARTENZA
582400970306     C                   MOVEL     D00FIE        KTTFIE                         *ITALIA/ESTERO
582500040903     C                   MOVEL     D00dpd        KTTdpd                         *ITALIA/ESTERO
582600011017     C     KE2ETT        SETLL     TEETT01L
582700011017     C     KE2ETT        READE     TEETT01L                               99
582800970306DO  1C     *IN99         DOWEQ     *OFF
582900970306IF  2C     ETTRCT        IFNE      *BLANKS                                      *DA CALCOLARE
583000970306IF  3C     ETTATB        IFEQ      *BLANKS                                      *NO ANNULLATI
583100970306     C                   MOVEL     '1'           $CTS
583200970306     C                   ADD       1             I
583300970306     C                   MOVEL     ETTCTS        CCTS(I)                        *CODICE TASSAZIONE
583400970306     C                   MOVEL     ETTRCT        CRCT(I)                        *RAGGRUPPAMENTO
583500970306     C                   Z-ADD     ETTPPE        CPPE(I)                        *% INCIDENZA
583600970306     C*
583700970715     C* RECUPERA INSTRADAMENTO -ARRIVO-
583800020906     C                   Z-ADD     1             KTBKUT
583900970306     C                   MOVEL     'CT'          KTBCOD
584000970306     C                   MOVEL(P)  ETTCTS        KTBKEY
584100970306     C     KEYTAB        CHAIN     TABEL00F                           98
584200970306IF  4C     *IN98         IFEQ      *ON                                          *NON TROVATO
584300970306     C                   MOVEL     MSG(4)        $MSG
584400970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
584500970306X   4C                   ELSE                                                   *TROVATO
584600970306     C                   MOVEL     TBLUNI        DSCT
584700970306     C                   MOVEL     §CTRAP        CRAP(I)                        *REGIONE APPARTENENZ
584800970306     C     §CTNAR        IFEQ      *BLANKS
584900970306     C                   MOVEL     'IT'          CNAR(I)                        *NAZIONE APPARTENENZ
585000970306     C                   ELSE
585100970306     C                   MOVEL     'EE'          CNAR(I)
585200970306     C                   ENDIF
585300011030     C                   MOVEL     §CT§§5        C§§5(I)                        *FLAG ISOLA/NONISOLA
585400040906     C* RECUPERA la nazione
585500040906     c                   clear                   ds15
585600040906     C                   Z-ADD     1             KTBkut
585700040906     C                   MOVEL     '15'          KTBCOD
585800040906     C                   MOVEL     §ctnar        KTBKEY
585900040906     C     KEYTAB        CHAIN     TABEL00F                           99
586000040906     C     *IN99         IFEQ      *ON
586100040906     C                   MOVEL     MSG(6)        $msg                           *C.TASS. NON TROVATO
586200040906     C                   EXSR      impERR
586300040906     C                   Else
586400040906     C                   MOVEL     TBLUNI        DS15
586500040906     c                   end
586600040906     C* LINEA ARRIVO da cappario se non è fedex o DPD
586700040906     c* alrimenti dalla tabella 15
586800040906    5c                   if        d00dpd = ' '
586900970715     C                   CLEAR                   DSSI95
587000970715     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
587100970715     C                   MOVEL     '3'           I95TCN                         *CONTROLLO ESISTENZA
587200970715     C                   MOVEL     §CTNAR        I95NAR                         *NAZIONE
587300970715     C                   MOVEL     §CTCAP        I95CAP                         *CAP
587400970715     C                   Z-ADD     WTFP          I95TFP                         *TERMINAL PARTENZA
587500980602     C                   MOVEL     'S'           I95FRE                         *CONTROLLA ECCEZIONI
587600970715     C                   Z-ADD     D00DTC        I95DAT                         *DATA RIFERIMENTO
587700970715     C                   CALL      'TISI95R'
587800970715     C                   PARM                    DSSI95
587900040906IF  6C     O95ERR        IFNE      *BLANKS                                      *NON TROVATO
588000970715     C                   MOVEL     O95MSG        $MSG
588100970715     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
588200040906e   6C                   End                                                    *TROVATO
588300040906x   5c                   else
588400040906     c* dpd o fed
588500040906     c                   if        d00dpd = 'D'
588600040906     c                   z-add     §15lad        o95lna
588700040906     c                   else
588800040906     c                   z-add     §15laf        o95lna
588900040906     c                   end
589000040906e   5c                   end
589100970715     C                   Z-ADD     O95LNA        CLNA(I)                        *LINEA ARRIVO
589200971204     C* FLAG ITALIA/ESTERO
589300971204     C                   Z-ADD     CLNA(I)       KORFIL
589400971204     C     KEYORG        CHAIN     AZORG01L                           98
589500020729     C                   MOVEL     ORGDE3        OG143
589600040906IF  5C     *IN98         IFEQ      *OFF                                         *NON TROVATO
589700020729     C*
589800020729     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
589900020729     C                   CLEAR                   DNTW
590000020729     C*
590100020729     C                   MOVEL(P)  'NTW'         KTECOD
590200020729     C                   MOVEL(P)  §OGNTW        KTEKE1
590300020729     C     KEYTBE        CHAIN     TNTBE01L
590400020729     C                   IF        %found(TNTBE01L)
590500020729     C                   MOVEL     TBEUNI        DNTW
590600020729     C                   ENDIF
590700020729     C*
590800020729     C                   MOVEL     §NTWFIE       CFL1(I)                        *FLAG ITALIA/ESTERO
590900040906E   5C                   ENDIF
591000971204     C*
591100970715     C* TERMINAL ARRIVO
591200040906    5c                   if        o95lna <> 0
591300970715     C                   CLEAR                   DSLV55
591400970715     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
591500970715     C                   MOVEL     'A'           D55TPT                         *ECCEZIONE ARRIVO
591600970715     C                   Z-ADD     O95LNA        D55LIN                         *LINEA
591700970715     C                   Z-ADD     D00DTC        D55DRF                         *DATA RIFERIMENTO
591800970715     C                   Z-ADD     D00LNP        D55LNP                         *LINEA PARTENZA
591900970715     C                   CALL      'FNLV55R'
592000970715     C                   PARM                    DSLV55
592100040906IF  6C     D55ERR        IFNE      *BLANKS                                      *NON TROVATO
592200970715     C                   MOVEL     D55MSG        $MSG
592300970715     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
592400040906X   6C                   ELSE                                                   *TROVATO
592500970715     C                   Z-ADD     D55TFA        CTFA(I)                        *TERMINAL ARRIVO
592600040906E   6C                   ENDIF
592700040906E   5C                   ENDIF
592800970306E   4C                   ENDIF
592900970306E   3C                   ENDIF
593000970306E   2C                   ENDIF
593100011017     C     KE2ETT        READE     TEETT01L                               99
593200970306E   1C                   ENDDO                                                  *FINE COD.TASSAZIONE
593300970306     C                   Z-ADD     I             WULTCT            6 0
593400970306IF  1C     $CTS          IFEQ      '0'                                          *NON TROVATO
593500970306     C                   MOVEL     MSG(3)        $MSG
593600970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
593700970306E   1C                   ENDIF
593800970715     C*
593900970306     C                   ENDSR
594000970306     C*------------------------------------------------------------------------*
594100970306     C* CARTAM - CARICAMENTO TARIFFA CLIENTE -TESTATA-
594200970306     C*------------------------------------------------------------------------*
594300970306     C     CARTAM        BEGSR
594400970306     C*
594500970306     C                   Z-ADD     D00KSC        KTMKSC                         *CLIENTE
594600970306     C                   Z-ADD     D00CTR        KTMCTR                         *TARIFFA
594700970306     C                   Z-ADD     D00PRG        KTMPRG                         *PROGRESSIVO
594800970306     C     KEYTAM        CHAIN     TNTAM00L                           99        *ESISTE PER FORZA
594900040826     C                   MOVEL     TAMFLO        DSTA01
595000970306     C*
595100970306     C* APPLICAZIONE TARIFFA FINITA
595200011031     C                   Z-ADD     *ZEROS        MINTAS           23 8
595300011031     C                   Z-ADD     *ZEROS        MINTAR           23 8
595400970306     C                   MOVEL     D00CTR        UNOCTR            1 0          *TIPO TARIFFA
595500980707     C                   MOVEL     UNOCTR        WTPG              1
595600970306     C                   SETOFF                                           28
595700970306     C                   Z-ADD     1             I
595800980707     C     WTPG          LOOKUP    CTR(I)                                 28
595900970306IF  1C     *IN28         IFEQ      *ON
596000970306     C     I             OCCUR     DSTR
596100970306     C                   Z-ADD     §TRATA        MINTAS                         *MINIMO TASSABILE ST
596200970306E   1C                   ENDIF
596300980422     C                   Z-ADD     §TRATA        MINTAR
596400970306IF  1C     TAMATA        IFGT      *ZEROS                                       *APPLICAZ.TAR.FINITA
596500970306     C                   Z-ADD     TAMATA        MINTAR                         *MINIMO APPLICAZ. KS
596600970306E   1C                   ENDIF
596700970306     C*
596800980506     C* DEPOSITA I DATI DELLA DSTR DELLA STATA TARIFFA
596900980506     C                   Z-ADD     MINTAS        DMITAS                         *MINIMO TASSABILE ST
597000980506     C                   MOVEL     §TRARR        DTRARR                         *ARROTONDAM ABILE ST
597100980506     C                   MOVEL     §TRRPV        DTRRPV                         *RPV        ABILE ST
597200971022     C*
597300970306     C                   ENDSR
597400991125     C*------------------------------------------------------------------------*
597500011008     C* CNVTAD - CONVERTE I CAMPI DI TITAM IN DIVISA DI CALCOLO DEL CE
597600991125     C*------------------------------------------------------------------------*
597700991125     C     CNVTAD        BEGSR
597800991125     C* PORTO
597900991125     C                   Z-ADD     TADITR        WIMI             30 9
598000991125     C                   EXSR      CNVEUR
598100991125     C                   Z-ADD     WIMO          TADITR
598200991125     C* INOLTRO
598300991125     C                   Z-ADD     TADINO        WIMI             30 9
598400991125     C                   EXSR      CNVEUR
598500991125     C                   Z-ADD     WIMO          TADINO
598600991125     C*
598700991125     C                   ENDSR
598800991125     C*------------------------------------------------------------------------*
598900011008     C* CNVTPT - CONVERTE I CAMPI DI TITPT IN DIVISA DI CALCOLO DEL CE
599000991125     C*------------------------------------------------------------------------*
599100991125     C     CNVTPT        BEGSR
599200991125     C*
599300991125     C* ALCUNI CAMPI SONO DA CONVERTIRE SOLO SE CONTENGONO IMPORTI IN MONETA
599400991125IF  1C     TPTTPG        IFEQ      '4'                                          *QUANTITA'
599500991125     C     TPTTPG        OREQ      'T'                                          *TRASPORTO
599600991125     C     TPTTPG        OREQ      'V'                                          *VALORE
599700991125     C*
599800991125     C* ARROTONDAMENTO LIMITE
599900991125     C                   Z-ADD     TPTARL        WIMI             30 9
600000991125     C                   EXSR      CNVEUR
600100991125     C                   Z-ADD     WIMO          TPTARL
600200991125     C*
600300991125     C* ARROTONDAMENTO FINO LIMITE
600400991125     C                   Z-ADD     TPTARF        WIMI             30 9
600500991125     C                   EXSR      CNVEUR
600600991125     C                   Z-ADD     WIMO          TPTARF
600700991125     C*
600800991125     C* ARROTONDAMENTO OLTRE LIMITE
600900991125     C                   Z-ADD     TPTARO        WIMI             30 9
601000991125     C                   EXSR      CNVEUR
601100991125     C                   Z-ADD     WIMO          TPTARO
601200991125E   1C                   ENDIF
601300991125     C* VALORE MERCE
601400991125     C                   Z-ADD     TPTVLM        WIMI             30 9
601500991125     C                   EXSR      CNVEUR
601600991125     C                   Z-ADD     WIMO          TPTVLM
601700991125     C*
601800991125     C                   ENDSR
601900991125     C*------------------------------------------------------------------------*
602000011008     C* CNVTPD - CONVERTE I CAMPI DI TITPD IN DIVISA DI CALCOLO DEL CE
602100991125     C*------------------------------------------------------------------------*
602200991125     C     CNVTPD        BEGSR
602300991125     C*
602400991125     C* ALCUNI CAMPI SONO DA CONVERTIRE SOLO SE CONTENGONO IMPORTI IN MONETA
602500991125IF  1C     TPTTPG        IFEQ      '4'                                          *QUANTITA'
602600991125     C     TPTTPG        OREQ      'T'                                          *TRASPORTO
602700991125     C     TPTTPG        OREQ      'V'                                          *VALORE
602800991125     C* SCAGLIONE
602900991125     C                   Z-ADD     TPDSGL        N5                5 0
603000991125IF  2C     N5            IFNE      99999                                        *SCAGLIONE 99999=OLT
603100991125     C                   Z-ADD     TPDSGL        WIMI             30 9
603200991125     C                   EXSR      CNVEUR
603300991125     C                   Z-ADD     WIMO          TPDSGL
603400991125E   2C                   ENDIF
603500991125E   1C                   ENDIF
603600991125     C*
603700991125     C* ALCUNI CAMPI NON SONO DA CONVERTIRE PERCHE' RAPPRESENTANO PERCENTUALI
603800991125IF  1C     TPTTPG        IFNE      '4'                                          *QUANTITA'
603900991125     C     TPTTPG        ANDNE     'T'                                          *TRASPORTO
604000991125     C     TPTTPG        ANDNE     'V'                                          *VALORE
604100991125     C* TARIFFA
604200991125     C                   Z-ADD     TPDITR        WIMI             30 9
604300991125     C                   EXSR      CNVEUR
604400991125     C                   Z-ADD     WIMO          TPDITR
604500991125E   1C                   ENDIF
604600991125     C* MINIMO
604700991125     C                   Z-ADD     TPDMIN        WIMI             30 9
604800991125     C                   EXSR      CNVEUR
604900991125     C                   Z-ADD     WIMO          TPDMIN
605000991125     C* MASSIMO
605100991125     C                   Z-ADD     TPDMAX        WIMI             30 9
605200991125     C                   EXSR      CNVEUR
605300991125     C                   Z-ADD     WIMO          TPDMAX
605400991125     C*
605500991125     C                   ENDSR
605600991125     C*------------------------------------------------------------------------*
605700011008     C* CNVTGC - CONVERTE I CAMPI DI TITGC IN DIVISA DI CALCOLO DEL CE
605800991125     C*------------------------------------------------------------------------*
605900991125     C     CNVTGC        BEGSR
606000991125     C*
606100991125     C* SPESE VARIE
606200991125     C                   Z-ADD     TGCSGV        WIMI             30 9
606300991125     C                   EXSR      CNVEUR
606400991125     C                   Z-ADD     WIMO          TGCSGV
606500991125     C* SPESE RICONSEGNA CITTA'
606600991125     C                   Z-ADD     TGCSGR        WIMI             30 9
606700991125     C                   EXSR      CNVEUR
606800991125     C                   Z-ADD     WIMO          TGCSGR
606900991125     C* SPESE RICONSEGNA PROVINCIA
607000991125     C                   Z-ADD     TGCSGP        WIMI             30 9
607100991125     C                   EXSR      CNVEUR
607200991125     C                   Z-ADD     WIMO          TGCSGP
607300991125     C* SPESE DOSSIER
607400991125     C                   Z-ADD     TGCSGD        WIMI             30 9
607500991125     C                   EXSR      CNVEUR
607600991125     C                   Z-ADD     WIMO          TGCSGD
607700991125     C* 1° TARIFFA SOSTA
607800991125     C                   Z-ADD     TTS(1)        WIMI             30 9
607900991125     C                   EXSR      CNVEUR
608000991125     C                   Z-ADD     WIMO          TTS(1)
608100991125     C* 2° TARIFFA SOSTA
608200991125     C                   Z-ADD     TTS(2)        WIMI             30 9
608300991125     C                   EXSR      CNVEUR
608400991125     C                   Z-ADD     WIMO          TTS(2)
608500991125     C* 3° TARIFFA SOSTA
608600991125     C                   Z-ADD     TTS(3)        WIMI             30 9
608700991125     C                   EXSR      CNVEUR
608800991125     C                   Z-ADD     WIMO          TTS(3)
608900991125     C* TARIFFA MINIMA SOSTA
609000991125     C                   Z-ADD     TGCSTM        WIMI             30 9
609100991125     C                   EXSR      CNVEUR
609200991125     C                   Z-ADD     WIMO          TGCSTM
609300991125     C*
609400991125     C                   ENDSR
609500011008     C*------------------------------------------------------------------------*
609600011023     C* CNVETC - CONVERTE I CAMPI DI TEETC IN DIVISA DI CALCOLO DEL CE
609700011008     C*------------------------------------------------------------------------*
609800011008     C     CNVETC        BEGSR
609900011008     C* ARROTONDAMENTO
610000011008     C                   Z-ADD     ETCIAR        WIMI             30 9
610100011008     C                   EXSR      CNVEUR
610200011008     C                   Z-ADD     WIMO          ETCIAR
610300011008     C* CONTRASSEGNO
610400011008     C                   Z-ADD     ETCCAS        WIMI             30 9
610500011008     C                   EXSR      CNVEUR
610600011008     C                   Z-ADD     WIMO          ETCCAS
610700011008     C* ASSICURAZIONE
610800011008     C                   Z-ADD     ETCASC        WIMI             30 9
610900011008     C                   EXSR      CNVEUR
611000011008     C                   Z-ADD     WIMO          ETCASC
611100011008     C*
611200011008     C                   ENDSR
611300011008     C*------------------------------------------------------------------------*
611400011008     C* CNVETS - CONVERTE I CAMPI DI TEETS IN DIVISA DI CALCOLO DEL CE
611500011008     C*------------------------------------------------------------------------*
611600011008     C     CNVETS        BEGSR
611700011008     C* INOLTRO
611800011008     C                   Z-ADD     ETSINL        WIMI             30 9
611900011008     C                   EXSR      CNVEUR
612000011008     C                   Z-ADD     WIMO          ETSINL
612100011008     C*
612200011008     C                   ENDSR
612300991125     C*------------------------------------------------------------------------*
612400011008     C* CNVEUR - CONVERTE I CAMPI IN DIVISA DI CALCOLO DEL CE
612500991125     C*------------------------------------------------------------------------*
612600991125     C     CNVEUR        BEGSR
612700991125     C*
612800991125     C* AZZERA VARIABILI DI LAVORO
612900991125     C                   Z-ADD     *ZEROS        WIMO             30 9          *IMPORTO CONVERTITO
613000011008     C                   MOVEL     *BLANKS       NDEC              1            *N. DEC IN OUTPUT
613100011008     C*
613200011026     C* IMPOSTO I DECIMALI DI OUTPUT A SECONDA DELLA DIVISA DI OUTPUT (NON + VERO: SEMPREE MAX DEC)
613300011026     C*                  IF        §GEDCEDIV = 'ITL'
613400011026     C*                  EVAL      NDEC = '0'
613500011026     C*                  ELSE
613600011026     C*                  EVAL      NDEC = '3'
613700011026     C*                  ENDIF
613800011026     C                   EVAL      NDEC = '9'
613900991125     C*
614000991125IF  1C     WIMI          IFNE      *ZEROS
614100991125     C                   CLEAR                   YEUR
614200991125     C                   MOVEL     WDVI          YECDVI                         *DIVISA IMPORTO
614300991125     C                   Z-ADD     WIMI          YECIMI                         *IMPORTO DA CONVERTI
614400011026     C                   MOVEL     WDIVCE        YECDVO                         *DIVISA IMP CONVERTI
614500011008     C                   MOVEL     NDEC          YECDCO                         *N° DECIMALI
614600991125     C                   MOVEL     *BLANKS       YECTAR                         *ARROTONDAMENTO "H"
614700991125     C                   CALL      'YEURCO'
614800991125     C                   PARM                    YEUR
614900991125IF  2C     YECESI        IFEQ      ' '                                          *NO ERRORI
615000991125     C                   Z-ADD     YECIMO        WIMO                            -IMPORTO CONVERTITO
615100991125X   2C                   ELSE                                                   *SI ERRORI
615200991125     C                   Z-ADD     WIMI          WIMO                            -IMPOROTO DA CONVER
615300991125E   2C                   ENDIF
615400991125E   1C                   ENDIF
615500991125     C*
615600991125     C                   ENDSR
615700970306     C*------------------------------------------------------------------------*
615800970306     C* IMPERR - IMPOSTA ERRORE NELLA DS DI PROCEDURA
615900970306     C*------------------------------------------------------------------------*
616000970306     C     IMPERR        BEGSR
616100970306     C*
616200970306     C                   MOVEL     'S'           WERR                           *ERRORE
616300970306     C                   MOVEL     $MSG          D00MSG                         *MESSAGGIO PROCEDURA
616400970306     C                   MOVEL     '1'           D00ERR                         *ERRORE PROCEDURA
616500970306     C*
616600970306     C                   ENDSR
616700970715     C*------------------------------------------------------------------------*
616800970715     C* FINSR - OPERAZIONI FINALI
616900970715     C*------------------------------------------------------------------------*
617000970715     C     FINSR         BEGSR
617100970715     C*
617200970715     C* CHIUDE I PROGRAMMA DI UTILITA' RICHIAMATI
617300970715     C                   CLEAR                   DSSI95                         *CONTROLLA CAP
617400970715     C                   MOVEL     'C'           I95TLA
617500970715     C                   CALL      'TISI95R'
617600970715     C                   PARM                    DSSI95
617700970715     C*
617800970715     C                   CLEAR                   DSLV55                         *RECUPERA TERMINAL
617900970715     C                   MOVEL     'C'           D55TLA
618000970715     C                   CALL      'FNLV55R'
618100970715     C                   PARM                    DSLV55
618200970715     C*
618300970715     C                   ENDSR
618400011008     C*------------------------------------------------------------------------*
618500011008     C* CARGEDCE - CARICAMENTO TABELLA "GED" TIPO RECORD "CE" E CONSIDERAZIONI SU DIVISE
618600011008     C*------------------------------------------------------------------------*
618700011008     C     CARGEDCE      BEGSR
618800011008     C*
618900011008     C                   CLEAR                   DSGEDCE
619000011026     C                   MOVEL     *blanks       WDIVCE            3
619100011008     C*
619200011008     C                   MOVEL(P)  'GED'         KTECOD
619300011008     C                   MOVEL(P)  'CE'          KTEKE1
619400011008     C     KEYTBE        CHAIN     TNTBE01L
619500011008     C                   IF        %found(TNTBE01L)
619600011008     C                   MOVEL     TBEUNI        DSGEDCE
619700011008     C                   ENDIF
619800011008     C*
619900011008     C                   IF        §GEDCEDAT > D00DTC
620000011026     C                   EVAL      WDIVCE = §GEDCEDIP
620100011026     C                   ELSE
620200011026     C                   EVAL      WDIVCE = §GEDCEDIV
620300011008     C                   ENDIF
620400011008     C*
620500011008     C                   ENDSR
620600040716     C*------------------------------------------------------------------------*
620700040716     C* CARGEIEUR- CARICAMENTO TABELLA "GEI" TIPO RECORD "EUR"
620800040716     C*------------------------------------------------------------------------*
620900040716     C     CARGEIEUR     BEGSR
621000040716     C*
621100040716     C                   CLEAR                   DGEI
621200040716     C*
621300040716     C                   MOVEL(P)  'GEI'         KTECOD
621400040716     C                   MOVEL(P)  'EUR'         KTEKE1
621500040716     C     KEYTBE        CHAIN     TNTBE01L
621600040716     C                   IF        %found(TNTBE01L)
621700040716     C                   MOVEL     TBEUNI        DGEI
621800040716     C                   ENDIF
621900040716     C*
622000040716     C                   ENDSR
622100970306     C*------------------------------------------------------------------------*
622200970306     C* *INZSR - OPERAZIONI INIZIALI
622300970306     C*------------------------------------------------------------------------*
622400970306     C     *INZSR        BEGSR
622500970306     C*--------------------
622600970306     C* RICEVIMENTO PARAMETRI
622700970306     C*--------------------
622800970306     C     *ENTRY        PLIST
622900970306     C                   PARM                    KPJBA
623000970306     C                   PARM                    DSTE00
623100041202     c* apertura controllata di alcuni file xchè solo di filiale
623200041202     c                   open      tiogc01l                             95
623300041202     c                   open      tiopd02l                             95
623400041202     c                   open      tiopt01l                             95
623500041202     c                   open      tnofm00l                             95
623600970306     C*--------------------
623700970306     C* VARIABILI RIFERITE AL DATA BASE
623800970306     C*--------------------
623900010926     C     *LIKE         DEFINE    ETCDSF        KTCDSF                         *TEETC00F
624000980603     C     *LIKE         DEFINE    ETCCTO        KTCCTO
624100970306     C     *LIKE         DEFINE    ETCKSC        KTCKSC
624200970306     C     *LIKE         DEFINE    ETCCTR        KTCCTR
624300970306     C     *LIKE         DEFINE    ETCPRG        KTCPRG
624400970306     C     *LIKE         DEFINE    ETCLNP        KTCLNP
624500010926     C     *LIKE         DEFINE    ETSDSF        KTSDSF                         *TEETC00F
624600980603     C     *LIKE         DEFINE    ETSCTO        KTSCTO
624700970306     C     *LIKE         DEFINE    ETSKSC        KTSKSC
624800970306     C     *LIKE         DEFINE    ETSCTR        KTSCTR
624900970306     C     *LIKE         DEFINE    ETSPRG        KTSPRG
625000970306     C     *LIKE         DEFINE    ETSLNP        KTSLNP
625100970306     C     *LIKE         DEFINE    ETSSGL        KTSSGL
625200010926     C     *LIKE         DEFINE    ETTDSF        KTTDSF                         *TEETC00F
625300980603     C     *LIKE         DEFINE    ETTCTO        KTTCTO
625400970306     C     *LIKE         DEFINE    ETTKSC        KTTKSC
625500970306     C     *LIKE         DEFINE    ETTCTR        KTTCTR
625600970306     C     *LIKE         DEFINE    ETTPRG        KTTPRG
625700970306     C     *LIKE         DEFINE    ETTLNP        KTTLNP
625800970306     C     *LIKE         DEFINE    ETTFIE        KTTFIE
625900040903     C     *LIKE         DEFINE    ETTdpd        KTTdpd
626000970306     C     *LIKE         DEFINE    ETTCTS        KTTCTS
626100010926     C     *LIKE         DEFINE    ETWUSR        KTWUSR                         TEETW00F
626200980603     C     *LIKE         DEFINE    ETWDSF        KTWDSF
626300980603     C     *LIKE         DEFINE    ETWCTO        KTWCTO
626400970306     C     *LIKE         DEFINE    ETWKSC        KTWKSC
626500970306     C     *LIKE         DEFINE    ETWCTR        KTWCTR
626600970306     C     *LIKE         DEFINE    ETWPRG        KTWPRG
626700970306     C     *LIKE         DEFINE    ETWSGL        KTWSGL
626800970306     C     *LIKE         DEFINE    ETWCTS        KTWCTS
626900970306     C     *LIKE         DEFINE    ETWRDL        KTWRDL
627000010926     C     *LIKE         DEFINE    EWRUSR        KWRUSR                         TEETWR0F
627100970306     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
627200970306     C     *LIKE         DEFINE    TBLCOD        KTBCOD
627300970306     C     *LIKE         DEFINE    TBLKEY        KTBKEY
627400970306     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
627500970306     C     *LIKE         DEFINE    TAMKSC        KTMKSC                         *TNTAM00F
627600970306     C     *LIKE         DEFINE    TAMCTR        KTMCTR
627700970306     C     *LIKE         DEFINE    TAMPRG        KTMPRG
627800990819     C     *LIKE         DEFINE    TADKSC        KTDKSC                         *TITAD00F
627900970306     C     *LIKE         DEFINE    TADPRG        KTDPRG
628000970306     C     *LIKE         DEFINE    TADLNP        KTDLNP
628100970306     C     *LIKE         DEFINE    TADCTS        KTDCTS
628200990819     C     *LIKE         DEFINE    TADNAZ        KTDNAZ
628300990819     C     *LIKE         DEFINE    TADCAP        KTDCAP
628400970306     C     *LIKE         DEFINE    TADCTR        KTDCTR
628500990819     C     *LIKE         DEFINE    TPTKSC        KTPKSC                         *TITPT00F
628600970306     C     *LIKE         DEFINE    TPTCTR        KTPCTR
628700970306     C     *LIKE         DEFINE    TPTPRG        KTPPRG
628800970306     C     *LIKE         DEFINE    TPTFTC        KTPFTC
628900990819     C     *LIKE         DEFINE    TPDKSC        KDPKSC                         *TITPD00F
629000970306     C     *LIKE         DEFINE    TPDCTR        KDPCTR
629100970306     C     *LIKE         DEFINE    TPDPRG        KDPPRG
629200970306     C     *LIKE         DEFINE    TPDFTC        KDPFTC
629300970306     C     *LIKE         DEFINE    TPDCTS        KDPCTS
629400990819     C     *LIKE         DEFINE    TGCKSC        KGCKSC                         *TITGC00F
629500970306     C     *LIKE         DEFINE    TGCCTR        KGCCTR
629600970306     C     *LIKE         DEFINE    TGCPRG        KGCPRG
629700970306     C     *LIKE         DEFINE    TAMKSC        CARKSC                         *DI DEPOSITO
629800970306     C     *LIKE         DEFINE    TAMCTR        CARCTR
629900970306     C     *LIKE         DEFINE    TAMPRG        CARPRG
630000970306     C     *LIKE         DEFINE    TAMKSC        DEPKSC
630100970306     C     *LIKE         DEFINE    TAMCTR        DEPPRG
630200970306     C     *LIKE         DEFINE    TAMPRG        DEPCTR
630300970721     C     *LIKE         DEFINE    TPDAIN        DEPAIN
630400970721     C     *LIKE         DEFINE    TPDAIN        CPLAIN
630500011030     C     *LIKE         DEFINE    TPDAIN        CISAIN
630600011031     C     *LIKE         DEFINE    TPDAIN        SAMAIN
630700020729     C     *LIKE         DEFINE    §NTWFIE       DLPFL1
630800980506     C     *LIKE         DEFINE    §TRATA        DMITAS                                     ABILE ST
630900980506     C     *LIKE         DEFINE    §TRARR        DTRARR                                     ABILE ST
631000980506     C     *LIKE         DEFINE    §TRRPV        DTRRPV                                     ABILE ST
631100970306     C     *LIKE         DEFINE    ETTCTS        LCTS                           *DI LAVORO
631200970306     C     *LIKE         DEFINE    TAMKSC        KSC1
631300970306     C     *LIKE         DEFINE    TAMCTR        CTR1
631400970715     C     *LIKE         DEFINE    D55TFA        WTFA
631500970715     C     *LIKE         DEFINE    D55TFP        WTFP
631600970721     C     *LIKE         DEFINE    ORGCTS        WMCT
631700011008     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
631800011008     C     *LIKE         DEFINE    TBEKE1        KTEKE1
631900970306     C*--------------------
632000970306     C* CHIAVI DI LETTURA
632100970306     C*--------------------
632200010926     C* LETTURA TEETC01L - COMPLETA
632300970306     C     KEYETC        KLIST
632400980603     C                   KFLD                    KTCDSF                         *DATI SEDE O FILIALE
632500980603     C                   KFLD                    KTCCTO                         *TARIFFA O OFFERTA
632600970306     C                   KFLD                    KTCKSC                         *CLIENTE O VISITA
632700970306     C                   KFLD                    KTCCTR                         *TARIFFA
632800970306     C                   KFLD                    KTCPRG                         *PROGRESSIVO
632900970306     C                   KFLD                    KTCLNP                         *LINEA PARTENZA
633000010926     C* LETTURA TEETS01L - COMPLETA
633100970306     C     KEYETS        KLIST
633200980603     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
633300970306     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
633400970306     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
633500970306     C                   KFLD                    KTSCTR                         *TARIFFA
633600970306     C                   KFLD                    KTSPRG                         *PROGRESSIVO
633700970306     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
633800970306     C                   KFLD                    KTSSGL                         *SCAGLIONE
633900010926     C* LETTURA TEETS01L - PARZIALE
634000970306     C     KE2ETS        KLIST
634100980603     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
634200970306     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
634300970306     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
634400970306     C                   KFLD                    KTSCTR                         *TARIFFA
634500970306     C                   KFLD                    KTSPRG                         *PROGRESSIVO
634600970306     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
634700011017     C* LETTURA TEETT01L - COMPLETA
634800970306     C     KEYETT        KLIST
634900980603     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
635000970306     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
635100970306     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
635200970306     C                   KFLD                    KTTCTR                         *TARIFFA
635300970306     C                   KFLD                    KTTPRG                         *PROGRESSIVO
635400970306     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
635500970306     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
635600040903     C                   KFLD                    KTTdpd                         *ITALIA/ESTERO
635700970306     C                   KFLD                    KTTCTS                         *CODICE TASSAZIONE
635800011017     C* LETTURA TEETT01L - PARZIALE
635900970306     C     KE2ETT        KLIST
636000980603     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
636100970306     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
636200970306     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
636300970306     C                   KFLD                    KTTCTR                         *TARIFFA
636400970306     C                   KFLD                    KTTPRG                         *PROGRESSIVO
636500970306     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
636600970306     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
636700040903     C                   KFLD                    KTTdpd                         *ITALIA/ESTERO
636800010926     C* LETTURA TEETW01L - COMPLETA
636900970306     C     KEYETW        KLIST
637000970306     C                   KFLD                    KTWUSR                         UTENTE
637100980603     C                   KFLD                    KTWDSF                         *DATI SEDE O FILIALE
637200970306     C                   KFLD                    KTWCTO                         *TARIFFA O OFFERTA
637300970306     C                   KFLD                    KTWKSC                         *CLIENTE O VISITA
637400970306     C                   KFLD                    KTWCTR                         *TARIFFA
637500970306     C                   KFLD                    KTWPRG                         *PROGRESSIVO
637600970306     C                   KFLD                    KTWSGL                         *SCAGLIONE
637700970306     C                   KFLD                    KTWCTS                         *CODICE TASSAZIONE
637800970306     C                   KFLD                    KTWRDL                         *RAGGRUPPAMENTO DELT
637900010926     C* LETTURA TEETW01L - PARZIALE
638000970317     C     KE2ETW        KLIST
638100970317     C                   KFLD                    KTWUSR                         UTENTE
638200010926     C* LETTURA TEETWR1L - PARZIALE
638300970306     C     KEYEWR        KLIST
638400970306     C                   KFLD                    KWRUSR                         *UTENTE
638500970306     C* LETTURA TABEL00F - COMPLETA
638600970306     C     KEYTAB        KLIST
638700970306     C                   KFLD                    KTBKUT                         *UTENTE
638800970306     C                   KFLD                    KTBCOD                         *CODICE
638900970306     C                   KFLD                    KTBKEY                         *CHIAVE
639000970306     C* LETTURA TABEL00F - PARZIALE
639100970306     C     KE2TAB        KLIST
639200970306     C                   KFLD                    KTBKUT                         *UTENTE
639300970306     C                   KFLD                    KTBCOD                         *CODICE
639400970306     C* LETTURA AZORG01L - COMPLETA
639500970306     C     KEYORG        KLIST
639600970306     C                   KFLD                    KORFIL                         *FILIALE
639700970306     C* LETTURA TNTAM01L - PARZIALE
639800970306     C     KEYTAM        KLIST
639900970306     C                   KFLD                    KTMKSC                         *CLIENTE
640000970306     C                   KFLD                    KTMCTR                         *TARIFFA
640100970306     C* LETTURA TNTAM01L - COMPLETA
640200970306     C     KE2TAM        KLIST
640300970306     C                   KFLD                    KTMKSC                         *CLIENTE
640400970306     C                   KFLD                    KTMCTR                         *TARIFFA
640500970306     C                   KFLD                    KTMPRG                         *PROGRESSIVO
640600990819     C* LETTURA TITAD01L - PARZIALE
640700970306     C     KEYTAD        KLIST
640800970306     C                   KFLD                    KTDKSC                         *CLIENTE
640900970306     C                   KFLD                    KTDPRG                         *PROGRESSIVO
641000970306     C                   KFLD                    KTDLNP                         *LINEA DI PARTENZA
641100970306     C                   KFLD                    KTDCTS                         *CODICE TASSAZIONE
641200990819     C                   KFLD                    KTDNAZ                         *NAZIONE
641300990819     C                   KFLD                    KTDCAP                         *C.A.P.
641400970306     C                   KFLD                    KTDCTR                         *TARIFFA
641500990819     C* LETTURA TITPT01L - COMPLETA
641600970306     C     KEYTPT        KLIST
641700970306     C                   KFLD                    KTPKSC                         *CLIENTE
641800970306     C                   KFLD                    KTPCTR                         *TARIFFA
641900970306     C                   KFLD                    KTPPRG                         *PROGRESSIVO
642000970306     C                   KFLD                    KTPFTC                         *CONSEGNA PARTICOLAR
642100990819     C* LETTURA TITPD02L - PARZIALE
642200970306     C     KEYTPD        KLIST
642300970306     C                   KFLD                    KDPKSC                         *CLIENTE
642400970306     C                   KFLD                    KDPCTR                         *TARIFFA
642500970306     C                   KFLD                    KDPPRG                         *PROGRESSIVO
642600970306     C                   KFLD                    KDPFTC                         *CONSEGNA PARTICOLAR
642700970306     C                   KFLD                    KDPCTS                         *CODICE TASSAZIONE
642800990819     C* LETTURA TITGC01L - COMPLETA
642900970306     C     KEYTGC        KLIST
643000970306     C                   KFLD                    KGCKSC                         *CLIENTE
643100970306     C                   KFLD                    KGCCTR                         *TARIFFA
643200970306     C                   KFLD                    KGCPRG                         *PROGRESSIVO
643300011008     C* LETTURA TNTBE01L - PARZIALE
643400011008     C     KEYTBE        KLIST
643500011008     C                   KFLD                    KTECOD                         *UTENTE
643600011008     C                   KFLD                    KTEKE1                         *CODICE
643700970306     C*--------------------
643800970306     C* VARIABILI DI CONTROLLO
643900970306     C*--------------------
644000970306     C                   MOVEL     'N'           WERR              1            *ERRORE
644100970306     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGIO
644200970306     C                   MOVEL     '0'           $TGC              1            *TAR.GIACENZA  ESIST
644300970306     C                   MOVEL     '0'           $TAD              1            *TAR.DETTAGLIO ESIST
644400970306     C                   MOVEL     '0'           $TPT              1            *TAR.PART.TES. ESIST
644500970306     C                   MOVEL     '0'           $TPD              1            *TAR.PART.DET. ESIST
644600011023     C                   MOVEL     D00DIV        WDIV              3            *DIVISA CALCOLO CAT
644700970306     C*
644800970306     C* REPERISCE LA DATA CORRENTE
644900970306     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
645000970306     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
645100970306     C                   Z-ADD     WN8           G08DAT
645200970306     C                   Z-ADD     *ZEROS        G08INV
645300970306     C                   MOVEL     '0'           G08ERR
645400970306     C                   CALL      'XSRDA8'
645500970306     C                   PARM                    WLBDA8
645600970306     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
645700011008     C*
645800011008     C* RECUPERA DATI DA TEBELLA "GED" TIPO RECORD "CE"
645900011008     C                   EXSR      CARGEDCE
646000040716     C* RECUPERA DATI DA TEBELLA "GEI" TIPO RECORD "EUR"
646100040716     C                   EXSR      CARGEIEUR
646200971204     C*
646300971204     C* RECUPERA INSTRADAMENTO -PARTENZA-
646400971204     C                   EXSR      REPINP
646500970306     C*
646600970306     C                   ENDSR
646700970306     O*------------------------------------------------------------------------*
646800970306** MSG - MESSAGGI
646900970306TNTE11R- Dati tipo del cliente/tariffa non trovati                            1
647000970306TNTE11R- Scaglioni del cliente/tariffa non trovati                            2
647100970306TNTE11R- Codici tassazione del cliente/tariffa non trovati                    3
647200970306TNTE11R- Codice tassazione non trovato                                        4
647300970715TNTE11R- La linea di partenza della tariffa non è stata trovata               5
647400040906TNTE11R- Codice nazione non trovato
647500970715TNTE11R- 7
647600970306TNTE11R- 8
647700970306TNTE11R- 9
647800970306TNTE11R- 10
647900970306TNTE11R- Tabella QT1 non trovata. Avvertire il C.E.D.                         11
648000970306TNTE11R- Tabella QT2 non trovata. Avvertire il C.E.D.                         12
648100970306TNTE11R- Tabella QI1 non trovata. Avvertire il C.E.D.                         13
648200970306TNTE11R- Tabella QI2 non trovata. Avvertire il C.E.D.                         14
648300970306TNTE11R- Tabella TR  non trovata. Avvertire il C.E.D.                         15
648400970306TNTE11R- Tabella $2  non trovata. Avvertire il C.E.D.                         16
648500970306TNTE11R- Tabella CV  non trovata. Avvertire il C.E.D.                         17
648600970306TNTE11R- Tabella CT  non trovata. Avvertire il C.E.D.                         18
648700970306TNTE11R- Tabella 5E  non trovata. Avvertire il C.E.D.                         19
648800970306TNTE11R- Tabella 1P  non trovata. Avvertire il C.E.D.                         20
648900970306TNTE11R- 21
649000970306TNTE11R- MANCA TARIFFA. Tariffa di cartello inesistente.                      22
649100970306TNTE11R- MANCA TARIFFA. Tariffa giacenza non trovata.                         23
649200970306TNTE11R- MANCA TARIFFA. Tariffa dettaglio cliente non trovata.                24
649300970306TNTE11R- MANCA TARIFFA. Scaglione di peso in tariffa non trovato.             25
649400001218TNTE11R- MANCA TARIFFA. Scaglione di peso in t.particolare non trovato.   -  -26
649500970306TNTE11R- 27
649600970306TNTE11R- 28
649700970306TNTE11R- 29
649800970306TNTE11R- 30
