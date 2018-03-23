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
018600970306     D*-------------------
018700050103     D* DS CODICI TASSAZIONE                -600 CODICI TASSAZIONE-
018800970306     D*-------------------
018900050103     D DSCT          E DS                  OCCURS(600) INZ
019000970306     D*-------------------
019100970306     D* DS TIPI SERVIZIO                    -10 TIPI SERVIZIO-
019200970306     D*-------------------
019300970306     D DS5E          E DS                  OCCURS(10) INZ
019400970306     D*-------------------
019500020104     D* DS CONSEGNE PARTICOLARI             -50 PARTICOLARITA'-
019600980706     D*-------------------
019700020104     D DS1P          E DS                  OCCURS(50) INZ
019800980706     D*-------------------
019900980706     D* FLAG OPERATIVI -TNTAM-
020000980706     D*-------------------
020100980706     D DSTA01        E DS
020200011005     D*------------------
020300011005     D* DS LETTURA TABELLA "GED" TIPO RECORD "CE"
020400011005     D*------------------
020500011005     D DSGEDCE       E DS
020600040716     D*------------------
020700040716     D* DS LETTURA TABELLA "GEI" TIPO RECORD "EUR" (limite risarcibile
020800040716     D*------------------
020900040716     D Dgei          E DS
021000970306     D*-------------------
021100020104     D* CONSEGNE PARTICOLARI DI CARTELLO    -50 PARTICOLARITA'-
021200970306     D*-------------------
021300020104     D CPIDS           DS                  OCCURS(50)
021400010926     D  TTPG8P                 1      1
021500011031     D  TARF8P                 2     22  8
021600011031     D  TARL8P                23     43  8
021700011031     D  TARO8P                44     64  8
021800011031     D  TRPV8P                65     67  1
021900011031     D  CP88                  68     68
022000011031     D  TCTS8P                69     70
022100011031     D  TCTE8P                71     72
022200011031     D  TAPL8P                73     73
022300970306     D*-------------------
022400970306     D* ASSICURAZIONE PER CONTO             -10 SCAGLIONI-
022500970306     D*-------------------
022600970306     D ACDS            DS                  OCCURS(10)
022700011031     D  ASGL                   1     21  8
022800011031     D  AITR                  22     42  8
022900011031     D  AMIN                  43     63  8
023000011031     D  AMAX                  64     84  8
023100011031     D  AAIN                  85     85
023200970306     D*-------------------
023300980707     D* DETTAGLIO TARIFFARIO CLIENTE        -11 SCAGLIONI-
023400970306     D*-------------------
023500980707     D TARDS           DS                  OCCURS(11)
023600990819     D  TLNP                   1      3  0
023700990819     D  TCTS                   4      5
023800990819     D  TNAZ                   6      8
023900990819     D  TCAP                   9     17
024000990819     D  TCTR                  18     20  0
024100011031     D  TSCG                  21     41  8
024200011031     D  TITR                  42     62  8
024300011031     D  TINO                  63     83  8
024400011031     D  TRPV                  84     86  1
024500011031     D  TMIN                  87    107  8
024600011031     D  TMAX                 108    128  8
024700001218     D*-------------------
024800001218     D* COMPOSIZIONI MESSAGGI
024900001218     D*-------------------
025000001218     D* MSG,26
025100001218     D                 DS
025200001218     D  DM26A                 76     77
025300001218     D  DMSG26                 1     78
025400020729     D*-------------------
025500020729     D* DS RIDEFINIZIONE DESCRIZIONE 143 ORGANIGRAMMA
025600020729     D*-------------------
025700020729     D OG143         E DS
025800020729     D*------------------
025900020729     D* DS PER LETTURA TABELLA "NTW"
026000020729     D*------------------
026100020729     D DNTW          E DS
026200041213     d tasadgs         s                   like(tasadg)
026300970306     I*------------------------------------------------------------------------*
026400970306     I* INPUT
026500970306     I*------------------------------------------------------------------------*
026600970306     I*-------------------
026700970306     I* DS FORMATO RECORD TARIFFE GIACENZA
026800970306     I*-------------------
026900990819     ITITGC000
027000970306     I              TGCSG1                      TGS(1)
027100970306     I              TGCSG2                      TGS(2)
027200970306     I              TGCSG3                      TGS(3)
027300970306     I              TGCST1                      TTS(1)
027400970306     I              TGCST2                      TTS(2)
027500970306     I              TGCST3                      TTS(3)
027600970625     I*-------------------
027700970625     I* DS FORMATO RECORD TARIFFE GIACENZA
027800970625     I*-------------------
027900990819     ITIOGC000
028000970625     I              TGCSG1                      TGS(1)
028100970625     I              TGCSG2                      TGS(2)
028200970625     I              TGCSG3                      TGS(3)
028300970625     I              TGCST1                      TTS(1)
028400970625     I              TGCST2                      TTS(2)
028500970625     I              TGCST3                      TTS(3)
028600970306     C*------------------------------------------------------------------------*
028700970306     C* MAIN LINE
028800970306     C*------------------------------------------------------------------------*
028900970306     C*
029000970306     C* CANCELLA IL FILE DI LAVORO
029100970306IF  1C     WERR          IFEQ      'N'
029200970306     C                   EXSR      CANCWF
029300970306E   1C                   ENDIF
029400970306     C*
029500970306     C* CARICA LE TABELLE OCCORRENTI
029600970306IF  1C     WERR          IFEQ      'N'
029700970306     C                   EXSR      CARTAB
029800970306E   1C                   ENDIF
029900970715     C*
030000970306     C* IMPOSTA LA SEQUENZA DI CALCOLO DI TUTTE LE VOCI
030100970306IF  1C     WERR          IFEQ      'N'
030200970306     C                   EXSR      IMPSEQ
030300970306E   1C                   ENDIF
030400970306     C*
030500970306     C* CARICA DATI TIPO -GENERALI-
030600970306IF  1C     WERR          IFEQ      'N'
030700970306     C                   EXSR      CARETC
030800970306E   1C                   ENDIF
030900970306     C*
031000970306     C* CARICA DATI TIPO -SCAGLIONI-
031100970306IF  1C     WERR          IFEQ      'N'
031200970306     C                   EXSR      CARETS
031300970306E   1C                   ENDIF
031400970306     C*
031500970306     C* CARICA DATI TIPO -CODICI TASSAZIONE-
031600970306IF  1C     WERR          IFEQ      'N'
031700970306     C                   EXSR      CARETT
031800970306E   1C                   ENDIF
031900970306     C*
032000970306     C* DETERMINA LE VOCI DA CALCOLARE
032100970306IF  1C     WERR          IFEQ      'N'
032200970306     C                   EXSR      CHKVOC
032300970306E   1C                   ENDIF
032400970306     C*
032500970306     C* CARICA LA TARIFFA DEL CLIENTE -TESTATA-
032600970306IF  1C     WERR          IFEQ      'N'
032700970306     C                   EXSR      CARTAM
032800970306E   1C                   ENDIF
032900970306     C*
033000970306     C* CARICA LA TARIFFA GIACENZA DEL CLIENTE
033100970306IF  1C     WERR          IFEQ      'N'
033200970306IF  2C     VC(8)         IFEQ      'S'
033300970306     C     VC(18)        OREQ      'S'
033400970306     C     VC(19)        OREQ      'S'
033500970306     C     VC(20)        OREQ      'S'
033600970306     C                   MOVEL     '0'           $TGC
033700970306     C                   EXSR      CARTGC
033800970306E   2C                   ENDIF
033900970306E   1C                   ENDIF
034000970306     C*
034100970306     C* VALORIZZA LE VOCI DA CALCOLARE -FISSE-
034200970306IF  1C     WERR          IFEQ      'N'
034300970306     C                   EXSR      VALVO1
034400970306E   1C                   ENDIF
034500970306     C*
034600970306     C* CICLO PER TUTTI GLI SCAGLIONI ----------------------------------------->
034700970306DO  1C     1             DO        WULTSC        S                 6 0
034800970306IF  2C     SSGL(S)       IFNE      *ZEROS
034900970306     C*
035000970306     C* DETERMINA LE VOCI DA CALCOLARE
035100970306IF  3C     WERR          IFEQ      'N'
035200970306     C                   EXSR      CHKVOC
035300970306E   3C                   ENDIF
035400970306     C*
035500970306     C* VALORIZZA LE VOCI DA CALCOLARE -DIPENDENTI DA SCAGLIONI-
035600970306IF  3C     WERR          IFEQ      'N'
035700970306     C                   EXSR      VALVO2
035800970306E   3C                   ENDIF
035900970306     C*
036000970306     C* CICLO PER TUTTI I CODICI TASSAZIONE ----------------------------------->
036100970306DO  3C                   DO        WULTCT        T                 6 0
036200970306IF  4C     CCTS(T)       IFNE      *BLANKS
036300970306     C*
036400970306     C* DETERMINA LE VOCI DA CALCOLARE
036500970306IF  5C     WERR          IFEQ      'N'
036600970306     C                   EXSR      CHKVOC
036700970306E   5C                   ENDIF
036800970306     C*
036900970306     C* CARICA LA TARIFFA DEL CLIENTE -DETTAGLIO-
037000970306IF  5C     WERR          IFEQ      'N'
037100970306     C                   EXSR      CARTAD
037200970306E   5C                   ENDIF
037300970306     C*
037400970306     C* REPERISCE E CALCOLA LA QUANTITA' DA UTILIZZARE
037500970306IF  5C     WERR          IFEQ      'N'
037600970306     C                   EXSR      CALQTA
037700970306E   5C                   ENDIF
037800970306     C*
037900970306     C* VALORIZZA LE VOCI DA CALCOLARE -DIPENDENTI DA CODICI TASSAZIONE-
038000970306IF  5C     WERR          IFEQ      'N'
038100970306     C                   EXSR      VALVO3
038200970306E   5C                   ENDIF
038300970306     C*
038400970306     C* CALCOLA IMPORTI SULLE % INCIDENZA E I RAGGUPPAMENTI DELTA
038500970306IF  5C     WERR          IFEQ      'N'
038600970306     C                   EXSR      CALTOT
038700970306E   5C                   ENDIF
038800970306     C*
038900011017     C* SCRIVE IL FILE DI LAVORO TEETW
039000970306IF  5C     WERR          IFEQ      'N'
039100970306     C                   EXSR      SCRETW
039200970306E   5C                   ENDIF
039300970306     C*
039400010926     C* SCRIVE IL FILE DI LAVORO TEETWR SE LANCIO IN DEBUG
039500970306IF  5C     WERR          IFEQ      'N'
039600970306     C     D00DBG        ANDEQ     'S'
039700970306     C                   EXSR      SCREWR
039800970306E   5C                   ENDIF
039900970306E   4C                   ENDIF
040000970306E   3C                   ENDDO                                                  *FINE COD.TASSAZIONE
040100970306     C*
040200970306E   2C                   ENDIF
040300970306E   1C                   ENDDO                                                  *FINE SCAGLIONI
040400980702     C*
040500970715     C* OPERAZIONI FINALI
040600970715     C                   EXSR      FINSR
040700970306     C*
040800970306     C                   SETON                                        LR
040900970306     C*------------------------------------------------------------------------*
041000970306     C* IMPSEQ - IMPOSTA LA SEQUENZA DI CALCOLO DI TUTTE LE VOCI
041100970306     C*------------------------------------------------------------------------*
041200970306     C     IMPSEQ        BEGSR
041300970306     C*
041400970306     C* DETERMINA IL CODICE VOCE
041500970306     C                   EXSR      CODVOC
041600970306     C*
041700970306     C* DETERMINA LA SEQUENZA DELLE VOCI
041800970306     C                   EXSR      SEQVOC
041900970306     C*
042000970306     C* DETERMINA IL RAGGRUPPAMENTO DELTA DELLE VOCI
042100970306     C                   EXSR      RAGVOC
042200970306     C*
042300970306     C                   ENDSR
042400970306     C*------------------------------------------------------------------------*
042500970306     C* CODVOC - DETERMINA IL CODICE DELLE VOCI
042600970306     C*------------------------------------------------------------------------*
042700970306     C     CODVOC        BEGSR
042800970306     C*
042900970306     C                   Z-ADD     001           VV(1)                          *PORTO
043000970721     C                   Z-ADD     002           VV(2)                          *DIRITTO FISSO
043100970717     C                   Z-ADD     003           VV(3)                          *ADG X PORTO
043200970306     C                   Z-ADD     004           VV(4)                          *ISTAT X PORTO
043300970721     C                   Z-ADD     005           VV(5)                          *TCP
043400970306     C                   Z-ADD     006           VV(6)                          *INOLTRO
043500970717     C                   Z-ADD     007           VV(7)                          *ESPRESSO SU PORTO
043600970721     C                   Z-ADD     008           VV(8)                          *GIACENZA
043700970306     C                   Z-ADD     009           VV(9)                          *ASS.CONTO FRANCO
043800970306     C                   Z-ADD     010           VV(10)                         *ASS.RCV   FRANCO
043900970306     C                   Z-ADD     011           VV(11)                         *CONS. APPUNTAMENTO
044000970306     C                   Z-ADD     012           VV(12)                         *CONS. SUPERMERCATI
044100970306     C                   Z-ADD     013           VV(13)                         *CONS. AI PIANI
044200970306     C                   Z-ADD     014           VV(14)                         *CONS. FUORI MISURA
044300970721     C                   Z-ADD     015           VV(15)                         *C/ASSEGNO MITTENTE
044400970721     C                   Z-ADD     016           VV(16)                         *C/ASSEGNO BARTOLINI
044500970306     C                   Z-ADD     017           VV(17)                         *RITIRO
044600970721     C                   Z-ADD     018           VV(18)                         *GIACENZA
044700970721     C                   Z-ADD     019           VV(19)                         *GIACENZA
044800970721     C                   Z-ADD     020           VV(20)                         *GIACENZA
044900970717     C                   Z-ADD     021           VV(21)                         *ADG X INOLTRO
045000970717     C                   Z-ADD     022           VV(22)                         *ADG X DIR.FISSO
045100970717     C                   Z-ADD     023           VV(23)                         *ADG X ESPRESSO/PORT
045200970306     C                   Z-ADD     024           VV(24)                         *ISTAT X INOLTRO
045300970306     C                   Z-ADD     025           VV(25)                         *ISTAT X DIR.FISSO
045400970717     C                   Z-ADD     026           VV(26)                         *ISTAT X ESPRES/PORT
045500970306     C                   Z-ADD     027           VV(27)                         *ISTAT X ADG/PORTO
045600970306     C                   Z-ADD     028           VV(28)                         *ISTAT X ADG/D.FISSO
045700970306     C                   Z-ADD     029           VV(29)                         *ISTAT X ADG/INOLTRO
045800970717     C                   Z-ADD     030           VV(30)                         *ISTAT X ADG/ESP/POR
045900970306     C                   Z-ADD     031           VV(31)                         *ASS.CONTO ASSEGNATO
046000970306     C                   Z-ADD     032           VV(32)                         *ASS.RCV ASSEGNATO
046100970414     C                   Z-ADD     033           VV(33)                         *CONS. DDT
046200970721     C                   Z-ADD     034           VV(34)                         *ISTAT X ADG/ESP/INO
046300970414     C                   Z-ADD     035           VV(35)                         *CONS. LOC.DISAGIATE
046400970717     C                   Z-ADD     036           VV(36)                         *ESPRESSO SU INOLTRO
046500970717     C                   Z-ADD     037           VV(37)                         *ADG X ESPRES/INOLTR
046600970717     C                   Z-ADD     038           VV(38)                         *ISTAT X ESPRES/INOL
046700970721     C                   Z-ADD     039           VV(39)                         *ESPRESSO SU L.DISAG
046800970721     C                   Z-ADD     040           VV(40)                         *ADG X ESPRES/L.DISA
046900970721     C                   Z-ADD     041           VV(41)                         *ISTAT X ESPRES/L.DI
047000970721     C                   Z-ADD     042           VV(42)                         *ISTAT X ADG/ESP/LDI
047100971023     C                   Z-ADD     043           VV(43)                         *ADG X L.DISAGIATE
047200971023     C                   Z-ADD     044           VV(44)                         *ISTAT X L.DISAGIATE
047300971023     C                   Z-ADD     045           VV(45)                         *ISTAT X ADG/L.DISAG
047400001219     C                   Z-ADD     046           VV(46)                         *ADG SU TCP
047500001219     C                   Z-ADD     047           VV(47)                         *ADG SU GIACENZA VAR
047600001219     C                   Z-ADD     048           VV(48)                         *ADG SU ASSIC X CONT
047700001219     C                   Z-ADD     049           VV(49)                         *ADG SU ASSIC RCV  F
047800001219     C                   Z-ADD     050           VV(50)                         *ADG SU CONSEGNA APP
047900001219     C                   Z-ADD     051           VV(51)                         *ADG SU CONSEGNA SUP
048000001219     C                   Z-ADD     052           VV(52)                         *ADG SU CONSEGNA PIA
048100001219     C                   Z-ADD     053           VV(53)                         *ADG SU CONSEGNA FUO
048200001219     C                   Z-ADD     054           VV(54)                         *ADG SU C/ASSEGNO MI
048300001219     C                   Z-ADD     055           VV(55)                         *ADG SU C/ASSEGNO BA
048400001219     C                   Z-ADD     056           VV(56)                         *ADG SU RITIRO
048500001219     C                   Z-ADD     057           VV(57)                         *ADG SU GIACENZA CON
048600001219     C                   Z-ADD     058           VV(58)                         *ADG SU GIACENZA CON
048700001219     C                   Z-ADD     059           VV(59)                         *ADG SU GIACENZA SOS
048800001219     C                   Z-ADD     060           VV(60)                         *ADG SU ASSIC X CONT
048900001219     C                   Z-ADD     061           VV(61)                         *ADG SU ASSIC RCV AS
049000001219     C                   Z-ADD     062           VV(62)                         *ADG SU CONSEGNA DDT
049100001219     C                   Z-ADD     063           VV(63)                         *ISTAT SU ADG/TCP
049200001219     C                   Z-ADD     064           VV(64)                         *ISTAT SU ADG/GIAC.V
049300001219     C                   Z-ADD     065           VV(65)                         *ISTAT SU ADG/ASSXCO
049400001219     C                   Z-ADD     066           VV(66)                         *ISTAT SU ADG/ASS RC
049500001219     C                   Z-ADD     067           VV(67)                         *ISTAT SU ADG/CONSEG
049600001219     C                   Z-ADD     068           VV(68)                         *ISTAT SU ADG/CONSEG
049700001219     C                   Z-ADD     069           VV(69)                         *ISTAT SU ADG/CONSEG
049800001219     C                   Z-ADD     070           VV(70)                         *ISTAT SU ADG/CONSEG
049900001219     C                   Z-ADD     071           VV(71)                         *ISTAT SU ADG/CASSEG
050000001219     C                   Z-ADD     072           VV(72)                         *ISTAT SU ADG/CASSEG
050100001219     C                   Z-ADD     073           VV(73)                         *ISTAT SU ADG/RITIRO
050200001219     C                   Z-ADD     074           VV(74)                         *ISTAT SU ADG/GIACEN
050300001219     C                   Z-ADD     075           VV(75)                         *ISTAT SU ADG/GIACEN
050400001219     C                   Z-ADD     076           VV(76)                         *ISTAT SU ADG/GIACEN
050500001219     C                   Z-ADD     077           VV(77)                         *ISTAT SU ADG/ASSIC
050600001219     C                   Z-ADD     078           VV(78)                         *ISTAT SU ADG/ASSIC
050700001219     C                   Z-ADD     079           VV(79)                         *ISTAT SU ADG/CONSEG
050800011030     C                   Z-ADD     080           VV(80)                         *CONS.ISOLA
050900011030     C                   Z-ADD     081           VV(81)                         *ESPRESSO SU C.ISOLA
051000011030     C                   Z-ADD     082           VV(82)                         *ADG X ESPRES/C.ISOL
051100011030     C                   Z-ADD     083           VV(83)                         *ISTAT X ESPRES/C.IS
051200011030     C                   Z-ADD     084           VV(84)                         *ISTAT X ADG/ESP/CIS
051300011030     C                   Z-ADD     085           VV(85)                         *ADG X CONS.ISOLA
051400011030     C                   Z-ADD     086           VV(86)                         *ISTAT X CONS.ISOLA
051500011030     C                   Z-ADD     087           VV(87)                         *ISTAT X ADG/R.ISOLA
051600011030     C                   Z-ADD     088           VV(88)                         *RITIRO ISOLA
051700011030     C                   Z-ADD     089           VV(89)                         *ESPRESSO SU R.ISOLA
051800011030     C                   Z-ADD     090           VV(90)                         *ADG X ESPRES/R.ISOL
051900011030     C                   Z-ADD     091           VV(91)                         *ISTAT X ESPRES/R.IS
052000011030     C                   Z-ADD     092           VV(92)                         *ISTAT X ADG/ESP/RIS
052100011030     C                   Z-ADD     093           VV(93)                         *ADG X RITIRO ISOLA
052200011030     C                   Z-ADD     094           VV(94)                         *ISTAT X RITIROISOLA
052300011030     C                   Z-ADD     095           VV(95)                         *ISTAT X ADG/R.ISOLA
052400040826     C                   Z-ADD     096           VV(96)                         *ISTAT X ADG/R.ISOLA
052500040826     C                   Z-ADD     097           VV(97)                         *ISTAT X ADG/R.ISOLA
052600040826     C                   Z-ADD     098           VV(98)                         *ISTAT X ADG/R.ISOLA
052700040826     C                   Z-ADD     099           VV(99)                         *ISTAT X ADG/R.ISOLA
052800040826     C                   Z-ADD     100           VV(100)                        *ISTAT X ADG/R.ISOLA
052900040826     C                   Z-ADD     101           VV(101)                        *ISTAT X ADG/R.ISOLA
053000040826     C                   Z-ADD     102           VV(102)                        *ISTAT X ADG/R.ISOLA
053100040826     C                   Z-ADD     103           VV(103)                        *ISTAT X ADG/R.ISOLA
053200040826     C                   Z-ADD     104           VV(104)                        *ISTAT X ADG/R.ISOLA
053300060314     C                   Z-ADD     105           VV(105)                        *ISTAT X ADG/R.ISOLA
053400060314     C                   Z-ADD     106           VV(106)                        *ISTAT X ADG/R.ISOLA
053500060314     C                   Z-ADD     107           VV(107)                        *ISTAT X ADG/R.ISOLA
053600060314     C                   Z-ADD     108           VV(108)                        *ISTAT X ADG/R.ISOLA
053700060314     C                   Z-ADD     109           VV(109)                        *ISTAT X ADG/R.ISOLA
053800060314     C                   Z-ADD     110           VV(110)                        *ISTAT X ADG/R.ISOLA
053900971023     C*
054000060314     C                   Z-ADD     110           NVOCA             3 0          *N° VOCI DA CALCOLAR
054100970306     C*
054200970306     C                   ENDSR
054300970306     C*------------------------------------------------------------------------*
054400970306     C* SEQVOC - DETERMINA LA SEQUENZA DI CALCOLO DELLE VOCI
054500970306     C*------------------------------------------------------------------------*
054600970306     C     SEQVOC        BEGSR
054700970306     C*---
054800970306     C* VOCI FISSE = SEQUENZA UNO
054900970306     C*---
055000970306     C                   Z-ADD     1             VS(8)                          *GIACENZA
055100001222     C                   Z-ADD     1             VS(47)                         *ADG SU GIACENZA VAR
055200001222     C                   Z-ADD     1             VS(64)                         *ISTAT SU ADG/GIAC.V
055300970306     C*---
055400970306     C* VOCI DIPENDENTI DA SCAGLIONI = SEQUENZA DUE
055500970306     C*---
055600970306     C                   Z-ADD     2             VS(18)                         *GIACENZA
055700001227     C                   Z-ADD     2             VS(57)                         *ADG SU GIACENZA CON
055800001227     C                   Z-ADD     2             VS(74)                         *ISTAT SU ADG/GIACEN
055900970306     C                   Z-ADD     2             VS(19)                         *GIACENZA
056000001227     C                   Z-ADD     2             VS(58)                         *ADG SU GIACENZA CON
056100001227     C                   Z-ADD     2             VS(75)                         *ISTAT SU ADG/GIACEN
056200970306     C                   Z-ADD     2             VS(20)                         *GIACENZA
056300001227     C                   Z-ADD     2             VS(59)                         *ADG SU GIACENZA SOS
056400001227     C                   Z-ADD     2             VS(76)                         *ISTAT SU ADG/GIACEN
056500970306     C*---
056600970306     C* VOCI DIPENDENTI DA SCAGLIONI/CT/LNA/TERM ARRIVO = SEQUENZA TRE
056700970306     C*---
056800970306     C                   Z-ADD     3             VS(1)                          *PORTO
056900980702     C                   Z-ADD     3             VS(2)                          *DIRITTO FISSO
057000990819     C                   Z-ADD     3             VS(3)                          *A.D.G X PORTO
057100970306     C                   Z-ADD     3             VS(4)                          *ISTAT X PORTO
057200990819     C                   Z-ADD     3             VS(5)                          *T.C.P.
057300970306     C                   Z-ADD     3             VS(6)                          *INOLTRO
057400970717     C                   Z-ADD     3             VS(7)                          *ESPRESSO PORTO
057500970306     C                   Z-ADD     3             VS(9)                          *ASS.CONTO FRANCO
057600970306     C                   Z-ADD     3             VS(10)                         *ASSICURAZIONE R.C.V
057700970306     C                   Z-ADD     3             VS(11)                         *CONS. APPUNTAMENTO
057800970306     C                   Z-ADD     3             VS(12)                         *CONS. SUPERMERCATI
057900970306     C                   Z-ADD     3             VS(13)                         *CONS. AI PIANI
058000970306     C                   Z-ADD     3             VS(14)                         *CONS. FUORI MISURA
058100980702     C                   Z-ADD     3             VS(15)                         *C/ASSEGNO MITTENTE
058200980702     C                   Z-ADD     3             VS(16)                         *C/ASSEGNO BARTOLINI
058300970306     C                   Z-ADD     3             VS(17)                         *RITIRO
058400970306     C                   Z-ADD     3             VS(21)                         *A.D.G X INOLTRO
058500970306     C                   Z-ADD     3             VS(22)                         *A.D.G X DIR.FISSO
058600970717     C                   Z-ADD     3             VS(23)                         *ADG X ESPRESSO/PORT
058700970306     C                   Z-ADD     3             VS(24)                         *ISTAT X INOLTRO
058800970306     C                   Z-ADD     3             VS(25)                         *ISTAT X DIR.FISSO
058900970717     C                   Z-ADD     3             VS(26)                         *ISTAT X ESPRES/PORT
059000970306     C                   Z-ADD     3             VS(27)                         *ISTAT X ADG/PORTO
059100970306     C                   Z-ADD     3             VS(28)                         *ISTAT X ADG/D.FISSO
059200970306     C                   Z-ADD     3             VS(29)                         *ISTAT X ADG/INOLTRO
059300970717     C                   Z-ADD     3             VS(30)                         *ISTAT X ADG/ESP/POR
059400970306     C                   Z-ADD     3             VS(31)                         *ASS.CONTO ASSEGNATO
059500970306     C                   Z-ADD     3             VS(32)                         *ASS.RCV ASSEGNATO
059600970414     C                   Z-ADD     3             VS(33)                         *CONS. DDT
059700970721     C                   Z-ADD     3             VS(34)                         *ISTAT X ADG/ESP/INO
059800970414     C                   Z-ADD     3             VS(35)                         *CONS. LOC.DISAGIATE
059900970717     C                   Z-ADD     3             VS(36)                         *ESPRESSO INOLTRO
060000970717     C                   Z-ADD     3             VS(37)                         *ADG X ESPRESSO/INOL
060100970717     C                   Z-ADD     3             VS(38)                         *ISTAT X ESPRES/INOL
060200970721     C                   Z-ADD     3             VS(39)                         *ESPRESSO SU L.DISAG
060300970721     C                   Z-ADD     3             VS(40)                         *ADG X ESPRES/L.DISA
060400970721     C                   Z-ADD     3             VS(41)                         *ISTAT X ESPRES/L.DI
060500970721     C                   Z-ADD     3             VS(42)                         *ISTAT X ADG/ESP/LDI
060600971023     C                   Z-ADD     3             VS(43)                         *ADG X L.DISAGIATE
060700971023     C                   Z-ADD     3             VS(44)                         *ISTAT X L.DISAGIATE
060800971023     C                   Z-ADD     3             VS(45)                         *ISTAT X ADG/L.DISAG
060900001219     C                   Z-ADD     3             VS(46)                         *ADG SU TCP
061000001219     C                   Z-ADD     3             VS(48)                         *ADG SU ASSIC X CONT
061100001219     C                   Z-ADD     3             VS(49)                         *ADG SU ASSIC RCV  F
061200001219     C                   Z-ADD     3             VS(50)                         *ADG SU CONSEGNA APP
061300001219     C                   Z-ADD     3             VS(51)                         *ADG SU CONSEGNA SUP
061400001219     C                   Z-ADD     3             VS(52)                         *ADG SU CONSEGNA PIA
061500001219     C                   Z-ADD     3             VS(53)                         *ADG SU CONSEGNA FUO
061600001219     C                   Z-ADD     3             VS(54)                         *ADG SU C/ASSEGNO MI
061700001219     C                   Z-ADD     3             VS(55)                         *ADG SU C/ASSEGNO BA
061800001219     C                   Z-ADD     3             VS(56)                         *ADG SU RITIRO
061900001219     C                   Z-ADD     3             VS(60)                         *ADG SU ASSIC X CONT
062000001219     C                   Z-ADD     3             VS(61)                         *ADG SU ASSIC RCV AS
062100001219     C                   Z-ADD     3             VS(62)                         *ADG SU CONSEGNA DDT
062200001219     C                   Z-ADD     3             VS(63)                         *ISTAT SU ADG/TCP
062300001219     C                   Z-ADD     3             VS(65)                         *ISTAT SU ADG/ASSXCO
062400001219     C                   Z-ADD     3             VS(66)                         *ISTAT SU ADG/ASS RC
062500001219     C                   Z-ADD     3             VS(67)                         *ISTAT SU ADG/CONSEG
062600001219     C                   Z-ADD     3             VS(68)                         *ISTAT SU ADG/CONSEG
062700001219     C                   Z-ADD     3             VS(69)                         *ISTAT SU ADG/CONSEG
062800001219     C                   Z-ADD     3             VS(70)                         *ISTAT SU ADG/CONSEG
062900001219     C                   Z-ADD     3             VS(71)                         *ISTAT SU ADG/CASSEG
063000001219     C                   Z-ADD     3             VS(72)                         *ISTAT SU ADG/CASSEG
063100001219     C                   Z-ADD     3             VS(73)                         *ISTAT SU ADG/RITIRO
063200001219     C                   Z-ADD     3             VS(77)                         *ISTAT SU ADG/ASSIC
063300001219     C                   Z-ADD     3             VS(78)                         *ISTAT SU ADG/ASSIC
063400001219     C                   Z-ADD     3             VS(79)                         *ISTAT SU ADG/CONSEG
063500011030     C                   Z-ADD     3             VS(80)                         *CONS.ISOLA
063600011030     C                   Z-ADD     3             VS(81)                         *ESPRESSO SU C.ISOLA
063700011030     C                   Z-ADD     3             VS(82)                         *ADG X ESPRES/C.ISOL
063800011030     C                   Z-ADD     3             VS(83)                         *ISTAT X ESPRES/C.IS
063900011030     C                   Z-ADD     3             VS(84)                         *ISTAT X ADG/ESP/CIS
064000011030     C                   Z-ADD     3             VS(85)                         *ADG X CONS.ISOLA
064100011030     C                   Z-ADD     3             VS(86)                         *ISTAT X CONS.ISOLA
064200011030     C                   Z-ADD     3             VS(87)                         *ISTAT X ADG/C.ISOLA
064300011030     C                   Z-ADD     3             VS(88)                         *RITIRO ISOLA
064400011030     C                   Z-ADD     3             VS(89)                         *ESPRESSO SU R.ISOLA
064500011030     C                   Z-ADD     3             VS(90)                         *ADG X ESPRES/R.ISOL
064600011030     C                   Z-ADD     3             VS(91)                         *ISTAT X ESPRES/R.IS
064700011030     C                   Z-ADD     3             VS(92)                         *ISTAT X ADG/ESP/RIS
064800011030     C                   Z-ADD     3             VS(93)                         *ADG X RITIRO ISOLA
064900011030     C                   Z-ADD     3             VS(94)                         *ISTAT X RITIROISOLA
065000011030     C                   Z-ADD     3             VS(95)                         *ISTAT X ADG/R.ISOLA
065100040826     C                   Z-ADD     3             VS(96)                         *ISTAT X ADG/R.ISOLA
065200040826     C                   Z-ADD     3             VS(97)                         *ISTAT X ADG/R.ISOLA
065300040826     C                   Z-ADD     3             VS(98)                         *ISTAT X ADG/R.ISOLA
065400040826     C                   Z-ADD     3             VS(99)                         *ISTAT X ADG/R.ISOLA
065500040826     C                   Z-ADD     3             VS(100)                        *ISTAT X ADG/R.ISOLA
065600040826     C                   Z-ADD     3             VS(101)                        *ISTAT X ADG/R.ISOLA
065700040826     C                   Z-ADD     3             VS(102)                        *ISTAT X ADG/R.ISOLA
065800040826     C                   Z-ADD     3             VS(103)                        *ISTAT X ADG/R.ISOLA
065900040826     C                   Z-ADD     3             VS(104)                        *ISTAT X ADG/R.ISOLA
066000060314     C                   Z-ADD     3             VS(105)                        *ISTAT X ADG/R.ISOLA
066100060314     C                   Z-ADD     3             VS(106)                        *ISTAT X ADG/R.ISOLA
066200060314     C                   Z-ADD     3             VS(107)                        *ISTAT X ADG/R.ISOLA
066300060314     C                   Z-ADD     3             VS(108)                        *ISTAT X ADG/R.ISOLA
066400060314     C                   Z-ADD     3             VS(109)                        *ISTAT X ADG/R.ISOLA
066500060314     C                   Z-ADD     3             VS(110)                        *ISTAT X ADG/R.ISOLA
066600011030     C*
066700970306     C                   ENDSR
066800970306     C*------------------------------------------------------------------------*
066900970306     C* RAGVOC - DETERMINA IL RAGGRUPPAMENTO DELTA DELLE VOCI
067000970306     C*------------------------------------------------------------------------*
067100970306     C     RAGVOC        BEGSR
067200970717     C* PORTO
067300970717     C                   Z-ADD     001           VR(1)                          *PORTO
067400970717     C* EXTRA-PORTO
067500980702     C                   Z-ADD     002           VR(2)                          *DIRITTO FISSO
067600970717     C                   Z-ADD     002           VR(3)                          *ADG X PORTO
067700970717     C                   Z-ADD     002           VR(4)                          *ISTAT X PORTO
067800970717     C                   Z-ADD     002           VR(5)                          *TCP
067900970717     C                   Z-ADD     002           VR(6)                          *INOLTRO
068000970717     C                   Z-ADD     002           VR(17)                         *RITIRO
068100970717     C                   Z-ADD     002           VR(21)                         *ADG X INOLTRO
068200970717     C                   Z-ADD     002           VR(23)                         *ADG X ESPRES/PORTO
068300980702     C                   Z-ADD     002           VR(22)                         *ADG X DIR.FISSO
068400970717     C                   Z-ADD     002           VR(24)                         *ISTAT X INOLTRO
068500980702     C                   Z-ADD     002           VR(25)                         *ISTAT X DIR.FISSO
068600970717     C                   Z-ADD     002           VR(26)                         *ISTAT X ESPRES/PORT
068700970717     C                   Z-ADD     002           VR(27)                         *ISTAT X ADG/PORTO
068800970717     C                   Z-ADD     002           VR(28)                         *ISTAT X ADG/D.FISSO
068900970717     C                   Z-ADD     002           VR(29)                         *ISTAT X ADG/INOLTRO
069000970717     C                   Z-ADD     002           VR(30)                         *ISTAT X ADG/ESP/POR
069100970721     C                   Z-ADD     002           VR(34)                         *ISTAT X ADG/ESP/INO
069200970717     C                   Z-ADD     002           VR(37)                         *ADG X ESPRES/INOLT
069300970717     C                   Z-ADD     002           VR(38)                         *ISTAT X ESPRES/INOL
069400970721     C                   Z-ADD     002           VR(40)                         *ADG X ESPRES/L.DISA
069500970721     C                   Z-ADD     002           VR(41)                         *ISTAT X ESPRES/L.DI
069600970721     C                   Z-ADD     002           VR(42)                         *ISTAT X ADG/ESP/LDI
069700971023     C                   Z-ADD     002           VR(43)                         *ADG X L.DISAGIATE
069800971023     C                   Z-ADD     002           VR(44)                         *ISTAT X L.DISAGIATE
069900971023     C                   Z-ADD     002           VR(45)                         *ISTAT X ADG/L.DISAG
070000001219     C                   Z-ADD     002           VR(46)                         *ADG SU TCP
070100001219     C                   Z-ADD     002           VR(47)                         *ADG SU GIACENZA VAR
070200001219     C                   Z-ADD     002           VR(48)                         *ADG SU ASSIC X CONT
070300001219     C                   Z-ADD     002           VR(49)                         *ADG SU ASSIC RCV  F
070400001219     C                   Z-ADD     002           VR(50)                         *ADG SU CONSEGNA APP
070500001219     C                   Z-ADD     002           VR(51)                         *ADG SU CONSEGNA SUP
070600001219     C                   Z-ADD     002           VR(52)                         *ADG SU CONSEGNA PIA
070700001219     C                   Z-ADD     002           VR(53)                         *ADG SU CONSEGNA FUO
070800001219     C                   Z-ADD     002           VR(54)                         *ADG SU C/ASSEGNO MI
070900001219     C                   Z-ADD     002           VR(55)                         *ADG SU C/ASSEGNO BA
071000001219     C                   Z-ADD     002           VR(56)                         *ADG SU RITIRO
071100001219     C                   Z-ADD     002           VR(57)                         *ADG SU GIACENZA CON
071200001219     C                   Z-ADD     002           VR(58)                         *ADG SU GIACENZA CON
071300001219     C                   Z-ADD     002           VR(59)                         *ADG SU GIACENZA SOS
071400001219     C                   Z-ADD     002           VR(60)                         *ADG SU ASSIC X CONT
071500001219     C                   Z-ADD     002           VR(61)                         *ADG SU ASSIC RCV AS
071600001219     C                   Z-ADD     002           VR(62)                         *ADG SU CONSEGNA DDT
071700001219     C                   Z-ADD     002           VR(63)                         *ISTAT SU ADG/TCP
071800001219     C                   Z-ADD     002           VR(64)                         *ISTAT SU ADG/GIAC.V
071900001219     C                   Z-ADD     002           VR(65)                         *ISTAT SU ADG/ASSXCO
072000001219     C                   Z-ADD     002           VR(66)                         *ISTAT SU ADG/ASS RC
072100001219     C                   Z-ADD     002           VR(67)                         *ISTAT SU ADG/CONSEG
072200001219     C                   Z-ADD     002           VR(68)                         *ISTAT SU ADG/CONSEG
072300001219     C                   Z-ADD     002           VR(69)                         *ISTAT SU ADG/CONSEG
072400001219     C                   Z-ADD     002           VR(70)                         *ISTAT SU ADG/CONSEG
072500001219     C                   Z-ADD     002           VR(71)                         *ISTAT SU ADG/CASSEG
072600001219     C                   Z-ADD     002           VR(72)                         *ISTAT SU ADG/CASSEG
072700001219     C                   Z-ADD     002           VR(73)                         *ISTAT SU ADG/RITIRO
072800001219     C                   Z-ADD     002           VR(74)                         *ISTAT SU ADG/GIACEN
072900001219     C                   Z-ADD     002           VR(75)                         *ISTAT SU ADG/GIACEN
073000001219     C                   Z-ADD     002           VR(76)                         *ISTAT SU ADG/GIACEN
073100001219     C                   Z-ADD     002           VR(77)                         *ISTAT SU ADG/ASSIC
073200001219     C                   Z-ADD     002           VR(78)                         *ISTAT SU ADG/ASSIC
073300001219     C                   Z-ADD     002           VR(79)                         *ISTAT SU ADG/CONSEG
073400011030     C                   Z-ADD     002           VR(82)                         *ADG X ESPR/C.ISOLA
073500011030     C                   Z-ADD     002           VR(83)                         *ISTAT X ESPRES/C.IS
073600011030     C                   Z-ADD     002           VR(84)                         *ISTAT X ADG/ESP/CIS
073700011030     C                   Z-ADD     002           VR(85)                         *ADG X CONS.ISOLA
073800011030     C                   Z-ADD     002           VR(86)                         *ISTAT X CONS.ISOLA
073900011030     C                   Z-ADD     002           VR(87)                         *ISTAT X ADG/C.ISOLA
074000011031     C                   Z-ADD     002           VR(90)                         *ADG X ESPR/R.ISOLA
074100011031     C                   Z-ADD     002           VR(91)                         *ISTAT X ESPRES/R.IS
074200011031     C                   Z-ADD     002           VR(92)                         *ISTAT X ADG/ESP/RIS
074300011031     C                   Z-ADD     002           VR(93)                         *ADG X RITIRO ISOLA
074400011031     C                   Z-ADD     002           VR(94)                         *ISTAT X RITIROISOLA
074500011031     C                   Z-ADD     002           VR(95)                         *ISTAT X ADG/R.ISOLA
074600040826     C                   Z-ADD     002           VR(97)                         *ISTAT X ADG/R.ISOLA
074700040826     C                   Z-ADD     002           VR(98)                         *ISTAT X ADG/R.ISOLA
074800040826     C                   Z-ADD     002           VR(100)                        *ISTAT X ADG/R.ISOLA
074900040826     C                   Z-ADD     002           VR(101)                        *ISTAT X ADG/R.ISOLA
075000040826     C                   Z-ADD     002           VR(102)                        *ISTAT X ADG/R.ISOLA
075100040826     C                   Z-ADD     002           VR(103)                        *ISTAT X ADG/R.ISOLA
075200040826     C                   Z-ADD     002           VR(104)                        *ISTAT X ADG/R.ISOLA
075201070118     C                   Z-ADD     002           VR(106)                        *ISTAT X ADG/R.ISOLA
075202070118     C                   Z-ADD     002           VR(107)                        *ISTAT X ADG/R.ISOLA
075203070118     C                   Z-ADD     002           VR(109)                        *ISTAT X ADG/R.ISOLA
075204070118     C                   Z-ADD     002           VR(110)                        *ISTAT X ADG/R.ISOLA
075300970717     C* CONSEGNA AI PIANI
075400970717     C                   Z-ADD     003           VR(13)                         *CONS. AI PIANI
075500970717     C* CONSEGNA AI SUPERMERCATI
075600970717     C                   Z-ADD     004           VR(12)                         *CONS. SUPERMERCATI
075700970717     C* CONSEGNA SU APPUNTAMENTO
075800970717     C                   Z-ADD     005           VR(11)                         *CONS. APPUNTAMENTO
075900970717     C* CONSEGNA FUORI MISURA
076000970717     C                   Z-ADD     006           VR(14)                         *CONS. FUORI MISURA
076100970717     C* C/ASSEGNO
076200970717     C                   Z-ADD     007           VR(15)                         *C/ASSEGNO MITTENTE
076300970717     C                   Z-ADD     007           VR(16)                         *C/ASSEGNO BARTOLINI
076400970717     C* GIACENZA
076500970306     C                   Z-ADD     008           VR(8)                          *GIACENZA
076600970717     C                   Z-ADD     008           VR(18)                         *GIACENZA
076700970717     C                   Z-ADD     008           VR(19)                         *GIACENZA
076800970717     C                   Z-ADD     008           VR(20)                         *GIACENZA
076900970717     C* ASSICURAZIONE
077000970717     C                   Z-ADD     009           VR(9)                          *ASS.CONTO FRANCO
077100970717     C                   Z-ADD     009           VR(10)                         *ASS.RCV   FRANCO
077200970717     C                   Z-ADD     009           VR(31)                         *ASS.CONTO ASSEGNATO
077300970717     C                   Z-ADD     009           VR(32)                         *ASS.RCV ASSEGNATO
077400060314     C                   Z-ADD     009           VR(105)                        *ASS.RCV ASSEGNATO
077500060314     C                   Z-ADD     009           VR(108)                        *ASS.RCV ASSEGNATO
077600970717     C* ESPRESSO
077700970717     C                   Z-ADD     010           VR(7)                          *ESPRESSO PORTO
077800970717     C                   Z-ADD     010           VR(36)                         *ESPRESSO INOLTRO
077900970721     C                   Z-ADD     010           VR(39)                         *ESPRESSO L.DISAGIAT
078000011030     C                   Z-ADD     010           VR(81)                         *ESPRESSO CONS.ISOLA
078100011031     C                   Z-ADD     010           VR(89)                         *ESPRESSO RIT. ISOLA
078200970717     C* CONSEGNA DDT
078300970414     C                   Z-ADD     011           VR(33)                         *CONS. DDT
078400970717     C* CONSEGNA LOCALITA' DISAGIATA
078500970414     C                   Z-ADD     013           VR(35)                         *CONS. LOC.DISAGIATE
078600011030     C* CONSEGNA ISOLA
078700011030     C                   Z-ADD     016           VR(80)                         *CONS. ISOLA
078800050808     C* RITIRO ISOLA
078900050808     C                   Z-ADD     016           VR(88)                         *RITIRO ISOLA
079000050808     C* destinatario disagiato
079100050808     C                   Z-ADD     018           VR(96)                         *CONS. LOC.DISAGIATE
079200050808     C* bancali da rendere al cliente
079300050808     C                   Z-ADD     019           VR(99)                         *CONS. LOC.DISAGIATE
079400970306     C*
079500970306     C                   ENDSR
079600970306     C*------------------------------------------------------------------------*
079700970306     C* CHKVOC - DETERMINA LE VOCI DA CALCOLARE
079800970306     C*------------------------------------------------------------------------*
079900970306     C     CHKVOC        BEGSR
080000970306     C*
080100970306     C* IMPOSTA A NO I FLAG "DA CALCOLARE" DI TUTTE LE VOCI
080200970306     C                   MOVEL     'N'           VC                             *VOCE DA CALCOLARE
080300970306     C                   Z-ADD     100           VP                             *% DA CONSIDERARE
080400970318     C                   MOVEL     'N'           VI                             *INGLOBA S/N
080500970306     C*
080600970306     C* IMPOSTA A SI I FLAG "DA CALCOLARE" DELLE VOCI SEMPRE PRESENTI
080700970306     C                   MOVEL     'S'           VC(1)                          *PORTO
080800970306     C                   MOVEL     'S'           VC(2)                          *DIRITTO FISSO
080900970306     C                   MOVEL     'S'           VC(3)                          *A.D.G X PORTO
081000970306     C                   MOVEL     'S'           VC(22)                         *A.D.G X DIR.FISSO
081100970306     C                   MOVEL     'S'           VC(4)                          *ISTAT X PORTO
081200970306     C                   MOVEL     'S'           VC(25)                         *ISTAT X DIR.FISSO
081300970306     C                   MOVEL     'S'           VC(27)                         *ISTAT X ADG/PORTO
081400970306     C                   MOVEL     'S'           VC(28)                         *ISTAT X ADG/D.FISSO
081500970306     C                   MOVEL     'S'           VC(5)                          *T.C.P.
081600001222     C                   MOVEL     'S'           VC(46)                         *ADG SU TCP
081700001222     C                   MOVEL     'S'           VC(63)                         *ISTAT SU ADG/TCP
081800970306     C*
081900970306     C* CONTROLLA SE ESISTE =======> INOLTRO
082000970306IF  1C     ETCPPV        IFGT      *ZEROS                                       *% SPED.IN PROVINCIA
082100970306     C                   MOVEL     'S'           VC(6)                          *INOLTRO
082200970306     C                   Z-ADD     ETCPPV        VP(6)                          *% DA CONSIDERARE
082300970314     C                   MOVEL     ETCSIN        VI(6)                          *INGLOBA S/N
082400970306     C                   MOVEL     'S'           VC(21)                         *A.D.G X INOLTRO
082500970306     C                   Z-ADD     ETCPPV        VP(21)                         *% DA CONSIDERARE
082600970306     C                   MOVEL     'S'           VC(24)                         *ISTAT X INOLTRO
082700970306     C                   Z-ADD     ETCPPV        VP(24)                         *% DA CONSIDERARE
082800971023     C                   MOVEL     'S'           VC(29)                         *ISTAT X ADG/INOLTRO
082900971023     C                   Z-ADD     ETCPPV        VP(29)                         *% DA CONSIDERARE
083000970306E   1C                   ENDIF
083100970306     C*
083200970717     C* CONTROLLA SE ESISTE =======> ESPRESSO PORTO
083300970306IF  1C     ETCESP        IFGT      *ZEROS
083400970717     C                   MOVEL     'S'           VC(7)                          *ESPRESSO PORTO
083500970306     C                   Z-ADD     ETCESP        VP(7)                          *% DA CONSIDERARE
083600970306     C                   MOVEL     ETCIES        VI(7)                          *INGLOBA S/N
083700970717     C                   MOVEL     'S'           VC(23)                         *ADG X ESP/PORTO
083800970306     C                   Z-ADD     ETCESP        VP(23)                         *% DA CONSIDERARE
083900970717     C                   MOVEL     'S'           VC(26)                         *ISTAT X ESP/PORTO
084000970306     C                   Z-ADD     ETCESP        VP(26)                         *% DA CONSIDERARE
084100970717     C                   MOVEL     'S'           VC(30)                         *ISTAT X ADG/ESP/POR
084200970717     C                   Z-ADD     ETCESP        VP(30)                         *% DA CONSIDERARE
084300970306E   1C                   ENDIF
084400970717     C*
084500970717     C* CONTROLLA SE ESISTE =======> ESPRESSO INOLTRO
084600970717IF  1C     ETCESP        IFGT      *ZEROS
084700970717IF  2C     ETCPPV        IFGT      *ZEROS
084800980928     C     ETCPPV        MULT      ETCESP        WN95             10 5
084900970717     C     WN95          DIV       100           WN95
085000970717IF  3C     WN95          IFGT      *ZEROS
085100970717     C                   MOVEL     'S'           VC(36)                         *ESPRESSO INOLTRO
085200970717     C                   Z-ADD     WN95          VP(36)                         *% DA CONSIDERARE
085300970717     C                   MOVEL     ETCIES        VI(36)                         *INGLOBA S/N
085400970717     C                   MOVEL     'S'           VC(37)                         *ADG X ESP/INOLTRO
085500970717     C                   Z-ADD     WN95          VP(37)                         *% DA CONSIDERARE
085600970717     C                   MOVEL     'S'           VC(38)                         *ISTAT X ESP/INOLTRO
085700970717     C                   Z-ADD     WN95          VP(38)                         *% DA CONSIDERARE
085800970721     C                   MOVEL     'S'           VC(34)                         *ISTAT X ADG/ESP/INO
085900970721     C                   Z-ADD     WN95          VP(34)                         *% DA CONSIDERARE
086000970717E   3C                   ENDIF
086100970717E   2C                   ENDIF
086200970717E   1C                   ENDIF
086300970721     C*
086400970721     C* CONTROLLA SE ESISTE =======> ESPRESSO LOCALITA' DISAGIATE
086500970721IF  1C     ETCESP        IFGT      *ZEROS
086600970721IF  2C     ETCPCL        IFGT      *ZEROS
086700980928     C     ETCPCL        MULT      ETCESP        WN95
086800970721     C     WN95          DIV       100           WN95
086900970721IF  3C     WN95          IFGT      *ZEROS
087000970721     C                   MOVEL     'S'           VC(39)                         *ESPRESSO L.DISAG.
087100970721     C                   Z-ADD     WN95          VP(39)                         *% DA CONSIDERARE
087200970721     C                   MOVEL     ETCIES        VI(39)                         *INGLOBA S/N
087300970721     C                   MOVEL     'S'           VC(40)                         *ADG X ESP/L.DISAG.
087400970721     C                   Z-ADD     WN95          VP(40)                         *% DA CONSIDERARE
087500970721     C                   MOVEL     'S'           VC(41)                         *ISTAT X ESP/L.DISAG
087600970721     C                   Z-ADD     WN95          VP(41)                         *% DA CONSIDERARE
087700970721     C                   MOVEL     'S'           VC(42)                         *ISTAT X ADG/ESP/LDI
087800970721     C                   Z-ADD     WN95          VP(42)                         *% DA CONSIDERARE
087900970721E   3C                   ENDIF
088000970721E   2C                   ENDIF
088100970721E   1C                   ENDIF
088200970306     C*
088300970306     C* CONTROLLA SE ESISTE =======> GIACENZA VARIE
088400970306IF  1C     ETCPGI        IFGT      *ZEROS
088500970306     C                   MOVEL     'S'           VC(8)                          *GIACENZA -FISSE-
088600970306     C                   Z-ADD     ETCPGI        VP(8)                          *% DA CONSIDERARE
088700970306     C                   MOVEL     ETCIGI        VI(8)                          *INGLOBA S/N
088800001222     C                   MOVEL     'S'           VC(47)                         *ADG SU GIACENZA VAR
088900001222     C                   Z-ADD     ETCPGI        VP(47)                         *% DA CONSIDERARE
089000001227     C                   MOVEL     'S'           VC(64)                         *ISTAT SU ADG/GIAC.V
089100001227     C                   Z-ADD     ETCPGI        VP(64)                         *% DA CONSIDERARE
089200970306     C                   MOVEL     'S'           VC(20)                         *GIACENZA -VARIABILI
089300970306     C                   Z-ADD     ETCPGI        VP(20)                         *% DA CONSIDERARE
089400970306     C                   MOVEL     ETCIGI        VI(20)                         *INGLOBA S/N
089500001227     C                   MOVEL     'S'           VC(59)                         *ADG SU GIACENZA SOS
089600001227     C                   Z-ADD     ETCPGI        VP(59)                         *% DA CONSIDERARE
089700001227     C                   MOVEL     'S'           VC(76)                         *ISTAT SU ADG/GIACEN
089800001227     C                   Z-ADD     ETCPGI        VP(76)                         *% DA CONSIDERARE
089900970306E   1C                   ENDIF
090000970306     C*
090100060314     C* CONTROLLA SE ESISTE =======> ASSICURAZIONE AC PLUS
090200970306IF  1C     ETCPAC        IFGT      *ZEROS
090300970306IF  2C     ETCFRA        IFGT      *ZEROS
090400980928     C     ETCFRA        MULT      ETCPAC        WN95
090500970306     C     WN95          DIV       100           WN95
090600970306IF  3C     WN95          IFGT      *ZEROS
090700970306     C                   MOVEL     'S'           VC(9)                          *ASS.CONTO FRANCO
090800970306     C                   Z-ADD     WN95          VP(9)                          *% DA CONSIDERARE
090900970306     C                   MOVEL     ETCIAS        VI(9)                          *INGLOBA S/N
091000001227     C                   MOVEL     'S'           VC(48)                         *ADG SU ASSIC X CONT
091100001227     C                   Z-ADD     WN95          VP(48)                         *% DA CONSIDERARE
091200001227     C                   MOVEL     'S'           VC(65)                         *ISTAT SU ADG/ASSXCO
091300001227     C                   Z-ADD     WN95          VP(65)                         *% DA CONSIDERARE
091400970306E   3C                   ENDIF
091500970306E   2C                   ENDIF
091600970306E   1C                   ENDIF
091700970306     C*
091800060314     C* CONTROLLA SE ESISTE =======> ASSICURAZIONE AC PLUS
091900970306IF  1C     ETCPAC        IFGT      *ZEROS
092000970306IF  2C     ETCASS        IFGT      *ZEROS
092100980928     C     ETCASS        MULT      ETCPAC        WN95
092200970306     C     WN95          DIV       100           WN95
092300970306IF  3C     WN95          IFGT      *ZEROS
092400970306     C                   MOVEL     'S'           VC(31)                         *ASS.CONTO ASSEGNATO
092500970306     C                   Z-ADD     WN95          VP(31)                         *% DA CONSIDERARE
092600970306     C                   MOVEL     ETCIAS        VI(31)                         *INGLOBA S/N
092700001227     C                   MOVEL     'S'           VC(60)                         *ADG SU ASSIC X CONT
092800001227     C                   Z-ADD     WN95          VP(60)                         *% DA CONSIDERARE
092900001227     C                   MOVEL     'S'           VC(77)                         *ISTAT SU ADG/ASSIC
093000001227     C                   Z-ADD     WN95          VP(77)                         *% DA CONSIDERARE
093100970306E   3C                   ENDIF
093200970306E   2C                   ENDIF
093300970306E   1C                   ENDIF
093400060314     C* CONTROLLA SE ESISTE =======> ASSICURAZIONE AC BASE
093500060314IF  1C     ETCxx1        IFGT      *ZEROS
093600060314IF  2C     ETCFRA        IFGT      *ZEROS
093700060314     C     ETCFRA        MULT      ETCxx1        WN95
093800060314     C     WN95          DIV       100           WN95
093900060314IF  3C     WN95          IFGT      *ZEROS
094000060314     C                   MOVEL     'S'           VC(105)                        *ASS.CONTO FRANCO
094100060314     C                   Z-ADD     WN95          VP(105)                        *% DA CONSIDERARE
094200060314     C                   MOVEL     'N'           VI(105)                        *INGLOBA S/N
094300060314     C                   MOVEL     'S'           VC(106)                        *ADG SU ASSIC X CONT
094400060314     C                   Z-ADD     WN95          VP(106)                        *% DA CONSIDERARE
094500060314     C                   MOVEL     'S'           VC(107)                        *ISTAT SU ADG/ASSXCO
094600060314     C                   Z-ADD     WN95          VP(107)                        *% DA CONSIDERARE
094700060314E   3C                   ENDIF
094800060314E   2C                   ENDIF
094900060314E   1C                   ENDIF
095000060314     C*
095100060314     C* CONTROLLA SE ESISTE =======> ASSICURAZIONE AC BASE
095200060314IF  1C     ETCxx1        IFGT      *ZEROS
095300060314IF  2C     ETCASS        IFGT      *ZEROS
095400060314     C     ETCASS        MULT      ETCxx1        WN95
095500060314     C     WN95          DIV       100           WN95
095600060314IF  3C     WN95          IFGT      *ZEROS
095700060314     C                   MOVEL     'S'           VC(108)                        *ASS.CONTO ASSEGNATO
095800060314     C                   Z-ADD     WN95          VP(108)                        *% DA CONSIDERARE
095900060314     C                   MOVEL     'N'           VI(108)                        *INGLOBA S/N
096000060314     C                   MOVEL     'S'           VC(109)                        *ADG SU ASSIC X CONT
096100060314     C                   Z-ADD     WN95          VP(109)                        *% DA CONSIDERARE
096200060314     C                   MOVEL     'S'           VC(110)                        *ISTAT SU ADG/ASSIC
096300060314     C                   Z-ADD     WN95          VP(110)                        *% DA CONSIDERARE
096400060314E   3C                   ENDIF
096500060314E   2C                   ENDIF
096600060314E   1C                   ENDIF
096700970306     C*
096800970306     C* CONTROLLA SE ESISTE =======> ASSICURAZIONE RCV FRANCO
096900060314     c* tariffa particolare "R" ormai obsoleta
097000990621IF  1C     ETCPAR        IFGT      *ZEROS
097100990621IF  2C     ETCFRA        IFGT      *ZEROS
097200990621     C     ETCFRA        MULT      ETCPAR        WN95
097300990621     C     WN95          DIV       100           WN95
097400990621IF  3C     WN95          IFGT      *ZEROS
097500990621     C                   MOVEL     'S'           VC(10)                         *ASS.NE RCV FRANCO
097600990621     C                   Z-ADD     WN95          VP(10)                         *% DA CONSIDERARE
097700990621     C                   MOVEL     ETCIAS        VI(10)                         *INGLOBA S/N
097800001227     C                   MOVEL     'S'           VC(49)                         *ADG SU ASSIC RCV  F
097900001227     C                   Z-ADD     WN95          VP(49)                         *% DA CONSIDERARE
098000001227     C                   MOVEL     'S'           VC(66)                         *ISTAT SU ADG/ASS RC
098100001227     C                   Z-ADD     WN95          VP(66)                         *% DA CONSIDERARE
098200990621E   3C                   ENDIF
098300990621E   2C                   ENDIF
098400990621E   1C                   ENDIF
098500970306     C*
098600970306     C* CONTROLLA SE ESISTE =======> ASSICURAZIONE RCV ASSEGNATO
098700060314     c* tariffa particolare "R" ormai obsoleta
098800990621IF  1C     ETCPAR        IFGT      *ZEROS
098900990621IF  2C     ETCASS        IFGT      *ZEROS
099000990621     C     ETCASS        MULT      ETCPAR        WN95
099100990621     C     WN95          DIV       100           WN95
099200990621IF  3C     WN95          IFGT      *ZEROS
099300990621     C                   MOVEL     'S'           VC(32)                         *ASS.NE RCV ASSEGNAT
099400990621     C                   Z-ADD     WN95          VP(32)                         *% DA CONSIDERARE
099500990621     C                   MOVEL     ETCIAS        VI(32)                         *INGLOBA S/N
099600001227     C                   MOVEL     'S'           VC(61)                         *ADG SU ASSIC RCV  A
099700001227     C                   Z-ADD     WN95          VP(61)                         *% DA CONSIDERARE
099800001227     C                   MOVEL     'S'           VC(78)                         *ISTAT SU ADG/ASS RC
099900001227     C                   Z-ADD     WN95          VP(78)                         *% DA CONSIDERARE
100000990621E   3C                   ENDIF
100100990621E   2C                   ENDIF
100200990621E   1C                   ENDIF
100300970306     C*
100400970306     C* CONTROLLA SE ESISTE =======> CONSEGNA SU APPUNTAMENTO
100500970306IF  1C     ETCPCA        IFGT      *ZEROS
100600970306     C                   MOVEL     'S'           VC(11)                         *CONS. APPUNTAMENTO
100700970306     C                   Z-ADD     ETCPCA        VP(11)                         *% DA CONSIDERARE
100800970306     C                   MOVEL     ETCIAP        VI(11)                         *INGLOBA S/N
100900001227     C                   MOVEL     'S'           VC(50)                         *ADG SU CONSEGNA APP
101000001227     C                   Z-ADD     ETCPCA        VP(50)                         *% DA CONSIDERARE
101100001227     C                   MOVEL     'S'           VC(67)                         *ISTAT SU ADG/CONSEG
101200001227     C                   Z-ADD     ETCPCA        VP(67)                         *% DA CONSIDERARE
101300970306E   1C                   ENDIF
101400970306     C*
101500970306     C* CONTROLLA SE ESISTE =======> CONSEGNA AI SUPERMERCATI
101600970306IF  1C     ETCPCS        IFGT      *ZEROS
101700970306     C                   MOVEL     'S'           VC(12)                         *CONS. SUPERMERCATI
101800970306     C                   Z-ADD     ETCPCS        VP(12)                         *% DA CONSIDERARE
101900970306     C                   MOVEL     ETCISU        VI(12)                         *INGLOBA S/N
102000001227     C                   MOVEL     'S'           VC(51)                         *ADG SU CONSEGNA SUP
102100001227     C                   Z-ADD     ETCPCS        VP(51)                         *% DA CONSIDERARE
102200001227     C                   MOVEL     'S'           VC(68)                         *ISTAT SU ADG/CONSEG
102300001227     C                   Z-ADD     ETCPCS        VP(68)                         *% DA CONSIDERARE
102400970306E   1C                   ENDIF
102500970306     C*
102600970306     C* CONTROLLA SE ESISTE =======> CONSEGNA AI PIANI
102700970306IF  1C     ETCPCP        IFGT      *ZEROS
102800970306     C                   MOVEL     'S'           VC(13)                         *CONS. AI PIANI
102900970306     C                   Z-ADD     ETCPCP        VP(13)                         *% DA CONSIDERARE
103000970306     C                   MOVEL     ETCIPI        VI(13)                         *INGLOBA S/N
103100001227     C                   MOVEL     'S'           VC(52)                         *ADG SU CONSEGNA SUP
103200001227     C                   Z-ADD     ETCPCP        VP(52)                         *% DA CONSIDERARE
103300001227     C                   MOVEL     'S'           VC(69)                         *ISTAT SU ADG/CONSEG
103400001227     C                   Z-ADD     ETCPCP        VP(69)                         *% DA CONSIDERARE
103500970306E   1C                   ENDIF
103600040826     C* CONTROLLA SE ESISTE =======> CONSEGNA destinatario disagiato
103700040826IF  1C     ETCdes        IFGT      *ZEROS
103800040826     C                   MOVEL     'S'           VC(96)                         *CONS. AI PIANI
103900040826     C                   Z-ADD     ETCdes        VP(96)                         *% DA CONSIDERARE
104000040826     C                   MOVEL     'S'           VC(97)                         *ADG SU CONSEGNA SUP
104100040826     C                   Z-ADD     ETCdes        VP(97)                         *% DA CONSIDERARE
104200040826     C                   MOVEL     'S'           VC(98)                         *ISTAT SU ADG/CONSEG
104300040826     C                   Z-ADD     ETCdes        VP(98)                         *% DA CONSIDERARE
104400040826E   1C                   ENDIF
104500970306     C*
104600040826     C* CONTROLLA SE ESISTE =======> bancali BAN
104700040826      * Non si tassano i bancali per le bolle in porto assegnato(vedi TNSF20R)
104800040826IF  1C     ETCnba        IFGT      *ZEROS
104900040826IF  1C     ETCfra        andeq     100
105000040826     C                   MOVEL     'S'           VC(99)                         *CONS. AI PIANI
105100040826     C                   Z-ADD     ETCban        VP(99)                         *% DA CONSIDERARE
105200040826     C                   MOVEL     'S'           VC(100)                        *ADG SU CONSEGNA SUP
105300040826     C                   Z-ADD     ETCban        VP(100)                        *% DA CONSIDERARE
105400040826     C                   MOVEL     'S'           VC(101)                        *ISTAT SU ADG/CONSEG
105500040826     C                   Z-ADD     ETCban        VP(101)                        *% DA CONSIDERARE
105600040826E   1C                   ENDIF
105700040826     C*
105800970306     C* CONTROLLA SE ESISTE =======> CONSEGNA FUORI MISURA
105900970306IF  1C     ETCPCF        IFGT      *ZEROS
106000970306     C                   MOVEL     'S'           VC(14)                         *CONS. FUORI MISURA
106100970306     C                   Z-ADD     ETCPCF        VP(14)                         *% DA CONSIDERARE
106200970306     C                   MOVEL     ETCIFU        VI(14)                         *INGLOBA S/N
106300001227     C                   MOVEL     'S'           VC(53)                         *ADG SU CONSEGNA FUO
106400001227     C                   Z-ADD     ETCPCF        VP(53)                         *% DA CONSIDERARE
106500001227     C                   MOVEL     'S'           VC(70)                         *ISTAT SU ADG/CONSEG
106600001227     C                   Z-ADD     ETCPCF        VP(70)                         *% DA CONSIDERARE
106700970306E   1C                   ENDIF
106800970414     C*
106900970414     C* CONTROLLA SE ESISTE =======> CONSEGNA DDT
107000970414IF  1C     ETCPCD        IFGT      *ZEROS
107100970414     C                   MOVEL     'S'           VC(33)                         *CONS. DDT
107200970414     C                   Z-ADD     ETCPCD        VP(33)                         *% DA CONSIDERARE
107300970414     C                   MOVEL     ETCIDD        VI(33)                         *INGLOBA S/N
107400001227     C                   MOVEL     'S'           VC(62)                         *ADG SU CONSEGNA DDT
107500001227     C                   Z-ADD     ETCPCD        VP(62)                         *% DA CONSIDERARE
107600001227     C                   MOVEL     'S'           VC(79)                         *ISTAT SU ADG/CONSEG
107700001227     C                   Z-ADD     ETCPCD        VP(79)                         *% DA CONSIDERARE
107800970414E   1C                   ENDIF
107900970414     C*
108000970414     C* CONTROLLA SE ESISTE =======> CONSEGNA LOCALITA' DISAGIATE
108100970414IF  1C     ETCPCL        IFGT      *ZEROS
108200970414     C                   MOVEL     'S'           VC(35)                         *CONS. LOC.DISAGIATE
108300970414     C                   Z-ADD     ETCPCL        VP(35)                         *% DA CONSIDERARE
108400970414     C                   MOVEL     ETCILO        VI(35)                         *INGLOBA S/N
108500971023     C                   MOVEL     'S'           VC(43)                         *A.D.G X L.DISAG.
108600971023     C                   Z-ADD     ETCPCL        VP(43)                         *% DA CONSIDERARE
108700971023     C                   MOVEL     'S'           VC(44)                         *ISTAT X L.DISAG.
108800971023     C                   Z-ADD     ETCPCL        VP(44)                         *% DA CONSIDERARE
108900971023     C                   MOVEL     'S'           VC(45)                         *ISTAT X ADG/L.DISAG
109000971023     C                   Z-ADD     ETCPCL        VP(45)                         *% DA CONSIDERARE
109100970414E   1C                   ENDIF
109200970306     C*
109300970306     C* CONTROLLA SE ESISTE =======> CONTRASSEGNO MITTENTE
109400970306IF  1C     ETCPAM        IFGT      *ZEROS
109500970306     C                   MOVEL     'S'           VC(15)                         *C/ASSEGNO MITTENTE
109600970306     C                   Z-ADD     ETCPAM        VP(15)                         *% DA CONSIDERARE
109700970306     C                   MOVEL     ETCICA        VI(15)                         *INGLOBA S/N
109800001227     C                   MOVEL     'S'           VC(54)                         *ADG SU C/ASSEGNO MI
109900001227     C                   Z-ADD     ETCPAM        VP(54)                         *% DA CONSIDERARE
110000001227     C                   MOVEL     'S'           VC(71)                         *ISTAT SU ADG/CASSEG
110100001227     C                   Z-ADD     ETCPAM        VP(71)                         *% DA CONSIDERARE
110200970306E   1C                   ENDIF
110300970306     C*
110400970306     C* CONTROLLA SE ESISTE =======> CONTRASSEGNO BARTOLINI
110500970306IF  1C     ETCPAB        IFGT      *ZEROS
110600970306     C                   MOVEL     'S'           VC(16)                         *C/ASSEGNO BARTOLINI
110700970306     C                   Z-ADD     ETCPAB        VP(16)                         *% DA CONSIDERARE
110800970306     C                   MOVEL     ETCICA        VI(16)                         *INGLOBA S/N
110900001227     C                   MOVEL     'S'           VC(55)                         *ADG SU C/ASSEGNO BA
111000001227     C                   Z-ADD     ETCPAB        VP(55)                         *% DA CONSIDERARE
111100001227     C                   MOVEL     'S'           VC(72)                         *ISTAT SU ADG/CASSEG
111200001227     C                   Z-ADD     ETCPAB        VP(72)                         *% DA CONSIDERARE
111300970306E   1C                   ENDIF
111400970306     C*
111500970306     C* CONTROLLA SE ESISTE =======> RITIRO
111600970306IF  1C     T             IFGT      *ZEROS                                       *CICLO COD.TASSAZION
111700970306IF  2C     ETCASS        IFGT      *ZEROS
111800971204IF  3C     CFL1(T)       IFEQ      *BLANKS                                      *ARRIVO ITALIA
111900970306IF  4C     ETCPCM        IFNE      100
112000970306     C     100           SUB       ETCPCM        WN63              6 3
112100980928     C     WN63          MULT      ETCASS        WN95
112200970306     C     WN95          DIV       100           WN95
112300970306IF  5C     WN95          IFGT      *ZEROS
112400970306     C                   MOVEL     'S'           VC(17)                         *RITIRO
112500970306     C                   Z-ADD     WN95          VP(17)                         *% DA CONSIDERARE
112600001227     C                   MOVEL     'S'           VC(56)                         *ADG SU RITIRO
112700001227     C                   Z-ADD     WN95          VP(56)                         *% DA CONSIDERARE
112800001227     C                   MOVEL     'S'           VC(73)                         *ISTAT SU ADG/RITIRO
112900001227     C                   Z-ADD     WN95          VP(73)                         *% DA CONSIDERARE
113000970306E   5C                   ENDIF
113100970311E   4C                   ENDIF
113200970311X   3C                   ELSE                                                   *ARRIVO ESTERO
113300970306     C                   MOVEL     'S'           VC(17)                         *RITIRO
113400001227     C                   MOVEL     'S'           VC(56)                         *ADG SU RITIRO
113500001227     C                   MOVEL     'S'           VC(73)                         *ISTAT SU ADG/RITIRO
113600970306E   3C                   ENDIF
113700970306E   2C                   ENDIF
113800970306E   1C                   ENDIF
113900970306     C*
114000970306     C* CONTROLLA SE ESISTE =======> GIACENZA RICONSEGNA PROVINCIA (SOLO 50%)
114100970306IF  1C     ETCPGI        IFGT      *ZEROS
114200970306IF  2C     ETCPPV        IFGT      *ZEROS
114300980928     C     ETCPPV        MULT      ETCPGI        WN95
114400970306     C     WN95          DIV       100           WN95
114500970306     C     WN95          MULT      50            WN95
114600970306     C     WN95          DIV       100           WN95
114700970306IF  3C     WN95          IFGT      *ZEROS
114800970306     C                   MOVEL     'S'           VC(18)                         *GIACENZA
114900970306     C                   Z-ADD     WN95          VP(18)                         *% DA CONSIDERARE
115000970306     C                   MOVEL     ETCIGI        VI(18)                         *INGLOBA S/N
115100001227     C                   MOVEL     'S'           VC(57)                         *ADG SU GIACENZA CON
115200001227     C                   Z-ADD     WN95          VP(57)                         *% DA CONSIDERARE
115300001227     C                   MOVEL     'S'           VC(74)                         *ISTAT SU ADG/GIACEN
115400001227     C                   Z-ADD     WN95          VP(74)                         *% DA CONSIDERARE
115500970306E   3C                   ENDIF
115600970306E   2C                   ENDIF
115700970306E   1C                   ENDIF
115800970306     C*
115900970306     C* CONTROLLA SE ESISTE =======> GIACENZA RICONSEGNA CITTA' (SOLO 50%)
116000970306IF  1C     ETCPGI        IFGT      *ZEROS
116100970306IF  2C     ETCPPV        IFGT      *ZEROS
116200970306     C     100           SUB       ETCPPV        WN63              6 3
116300980928     C     WN63          MULT      ETCPGI        WN95
116400970306     C     WN95          DIV       100           WN95
116500970306     C     WN95          MULT      50            WN95
116600970306     C     WN95          DIV       100           WN95
116700970306IF  3C     WN95          IFGT      *ZEROS
116800970306     C                   MOVEL     'S'           VC(19)                         *GIACENZA
116900970306     C                   Z-ADD     WN95          VP(19)                         *% DA CONSIDERARE
117000970306     C                   MOVEL     ETCIGI        VI(19)                         *INGLOBA S/N
117100001227     C                   MOVEL     'S'           VC(58)                         *ADG SU GIACENZA CON
117200001227     C                   Z-ADD     WN95          VP(58)                         *% DA CONSIDERARE
117300001227     C                   MOVEL     'S'           VC(75)                         *ISTAT SU ADG/GIACEN
117400001227     C                   Z-ADD     WN95          VP(75)                         *% DA CONSIDERARE
117500970306E   3C                   ENDIF
117600970306E   2C                   ENDIF
117700970306E   1C                   ENDIF
117800011030     C*
117900011031     C* CONTROLLA SE ESISTE =======>
118000011031IF  1C     ETCCIS        IFGT      *ZEROS
118100011031IF  2C     T             IFGT      *ZEROS                                       *CICLO COD.TASSAZION
118200011031IF  3C     C§§5(T)       IFEQ      'S'                                          *CTS POTENZ.ISOLA
118300011031     C*                     =======> CONSEGNA ISOLA
118400011030     C                   MOVEL     'S'           VC(80)                         *CONSEGNA ISOLA
118500011030     C                   Z-ADD     ETCCIS        VP(80)                         *% DA CONSIDERARE
118600011030     C                   MOVEL     ETCISO        VI(80)                         *INGLOBA S/N
118700011030     C                   MOVEL     'S'           VC(85)                         *A.D.G X CONS.ISOLA
118800011030     C                   Z-ADD     ETCCIS        VP(85)                         *% DA CONSIDERARE
118900011030     C                   MOVEL     'S'           VC(86)                         *ISTAT X CONS.ISOLA
119000011030     C                   Z-ADD     ETCCIS        VP(86)                         *% DA CONSIDERARE
119100011030     C                   MOVEL     'S'           VC(87)                         *ISTAT X ADG/C.ISOLA
119200011030     C                   Z-ADD     ETCCIS        VP(87)                         *% DA CONSIDERARE
119300011031     C*                     =======> ESPRESSO CONSEGNA ISOLE
119400011031IF  4C     ETCESP        IFGT      *ZEROS
119500011031     C     ETCCIS        MULT      ETCESP        WN95
119600011031     C     WN95          DIV       100           WN95
119700011031IF  5C     WN95          IFGT      *ZEROS
119800011031     C                   MOVEL     'S'           VC(81)                         *ESPRESSO CONS.ISOLE
119900011031     C                   Z-ADD     WN95          VP(81)                         *% DA CONSIDERARE
120000011031     C                   MOVEL     ETCISO        VI(81)                         *INGLOBA S/N
120100011031     C                   MOVEL     'S'           VC(82)                         *ADG X ESP/C.ISOLE
120200011031     C                   Z-ADD     WN95          VP(82)                         *% DA CONSIDERARE
120300011031     C                   MOVEL     'S'           VC(83)                         *ISTAT X ESP/C.ISOLE
120400011031     C                   Z-ADD     WN95          VP(83)                         *% DA CONSIDERARE
120500011031     C                   MOVEL     'S'           VC(84)                         *ISTAT X ADG/ESP/CIS
120600011031     C                   Z-ADD     WN95          VP(84)                         *% DA CONSIDERARE
120700011031E   5C                   ENDIF
120800011031E   4C                   ENDIF
120900011031     C*
121000011030E   3C                   ENDIF
121100011030E   2C                   ENDIF
121200011030E   1C                   ENDIF
121300011030     C*
121400011031     C* CONTROLLA SE ESISTE =======>
121500011031IF  1C     ETCSAM        IFGT      *ZEROS
121600011031IF  2C     T             IFGT      *ZEROS                                       *CICLO COD.TASSAZION
121700011031IF  3C     D00PRI        IFEQ      CCTS(T)                                      *CTS DEL CLIENTE T/O
121800011031IF  4C     C§§5(T)       IFEQ      'S'                                          *CTS POTENZ.ISOLA
121900011031     C*                     =======> RITIRO ISOLA
122000011031     C                   MOVEL     'S'           VC(88)                         *RITIRO ISOLA
122100011031     C                   Z-ADD     ETCSAM        VP(88)                         *% DA CONSIDERARE
122200011031     C                   MOVEL     ETCINR        VI(88)                         *INGLOBA S/N
122300011031     C                   MOVEL     'S'           VC(93)                         *A.D.G X RIT.ISOLA
122400011031     C                   Z-ADD     ETCSAM        VP(93)                         *% DA CONSIDERARE
122500011031     C                   MOVEL     'S'           VC(94)                         *ISTAT X RIT.ISOLA
122600011031     C                   Z-ADD     ETCSAM        VP(94)                         *% DA CONSIDERARE
122700011031     C                   MOVEL     'S'           VC(95)                         *ISTAT X ADG/R.ISOLA
122800011031     C                   Z-ADD     ETCSAM        VP(95)                         *% DA CONSIDERARE
122900011031     C*                     =======> ESPRESSO RITIRO ISOLE
123000011031IF  5C     ETCESP        IFGT      *ZEROS
123100011031     C     ETCSAM        MULT      ETCESP        WN95
123200011031     C     WN95          DIV       100           WN95
123300011031IF  6C     WN95          IFGT      *ZEROS
123400011031     C                   MOVEL     'S'           VC(89)                         *ESPRESSO RIT.ISOLE
123500011031     C                   Z-ADD     WN95          VP(89)                         *% DA CONSIDERARE
123600011031     C                   MOVEL     ETCINR        VI(89)                         *INGLOBA S/N
123700011031     C                   MOVEL     'S'           VC(90)                         *ADG X ESP/RIT.ISOLE
123800011031     C                   Z-ADD     WN95          VP(90)                         *% DA CONSIDERARE
123900011031     C                   MOVEL     'S'           VC(91)                         *ISTAT X ESP/R.ISOLE
124000011031     C                   Z-ADD     WN95          VP(91)                         *% DA CONSIDERARE
124100011031     C                   MOVEL     'S'           VC(92)                         *ISTAT X ADG/ESP/RIS
124200011031     C                   Z-ADD     WN95          VP(92)                         *% DA CONSIDERARE
124300011031E   6C                   ENDIF
124400011031E   5C                   ENDIF
124500011031     C*
124600011031E   4C                   ENDIF
124700011031E   3C                   ENDIF
124800011031E   2C                   ENDIF
124900011030E   1C                   ENDIF
125000970306     C*
125100970306     C                   ENDSR
125200970306     C*------------------------------------------------------------------------*
125300970306     C* VALVO1 - VALORIZZA LE VOCI -FISSE-
125400970306     C*------------------------------------------------------------------------*
125500970306     C     VALVO1        BEGSR
125600970306     C*
125700970306     C* AZZERA GLI IMPORTI DELLE VOCI CON SEQUENZA UNO
125800970306DO  1C     1             DO        NVOCA         I
125900970306IF  2C     VS(I)         IFEQ      1
126000970306     C                   Z-ADD     *ZEROS        VIMP(I)                        *IMPORTO CALCOLATO
126100970306E   2C                   ENDIF
126200970306E   1C                   ENDDO
126300970306     C*
126400970306     C* CALCOLA LE SPESE DI GIACENZA VARIE
126500970306IF  1C     WERR          IFEQ      'N'
126600970306IF  2C     VC(8)         IFEQ      'S'                                          *VOCE DA CALCOLARE
126700970306     C     $TGC          ANDEQ     '1'                                          *TROVATA TAR.GIACENZ
126800970306     C                   EXSR      CALGCV
126900001227     C                   EXSR      CAL047
127000001227     C                   EXSR      CAL064
127100970306E   2C                   ENDIF
127200970306E   1C                   ENDIF
127300970306     C*
127400970306     C                   ENDSR
127500970306     C*------------------------------------------------------------------------*
127600970306     C* CARTGC - REPERISCE LA TARIFFA GIACENZA
127700970306     C*------------------------------------------------------------------------*
127800970306     C     CARTGC        BEGSR
127900970306     C*
128000970625     C* LEGGE LA TARIFFA GIACENZA DEL CLIENTE
128100970625     C                   Z-ADD     D00KSC        KGCKSC                         *CLIENTE
128200970625     C                   Z-ADD     D00CTR        KGCCTR                         *TARIFFA
128300970625     C                   Z-ADD     D00PRG        KGCPRG                         *PROGRESSIVO
128400011019     C                   MOVEL     D00DIV        WDIV
128500980703IF  1C     D00CTO        IFEQ      'O'                                          *IN OFFERTA
128600990819     C     KEYTGC        CHAIN     TIOGC01L                           99
128700980703X   1C                   ELSE                                                   *IN TARIFFA
128800990819     C     KEYTGC        CHAIN     TITGC01L                           99
128900970625E   1C                   ENDIF
129000970306     C*
129100970625     C* LEGGE LA TARIFFA DI CARTELLO SE NON TROVATA TARIFFA/OFFERTA CLIENTE
129200970625IF  1C     *IN99         IFEQ      *ON                                          *NON TROVATA
129300970625     C                   Z-ADD     CARKSC        KGCKSC
129400970625     C                   Z-ADD     CARCTR        KGCCTR
129500970625     C                   Z-ADD     CARPRG        KGCPRG
129600011023     C                   MOVEL     CARDIV        WDIV
129700011023     C     KEYTGC        CHAIN     TITGC01L                           99        *DA TARIFFA CARTELLO
129800970625E   1C                   ENDIF
129900970625     C* TARIFFA NON TROVATA
130000970625IF  1C     *IN99         IFEQ      *ON
130100970625     C                   MOVEL     MSG(23)       $MSG
130200970625     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
130300970625     C* TARIFFA TROVATA
130400970306X   1C                   ELSE
130500991125     C*
130600991125     C* CONTROLLA SE CONVERTIRE I CAMPI DI TITPD
130700011026IF  2C     WDIV          IFNE      WDIVCE
130800011026     C                   MOVEL     WDIV          WDVI              3
130900991125     C                   EXSR      CNVTGC
131000991125E   2C                   ENDIF
131100970625     C                   MOVEL     '1'           $TGC                           *TARIFFA TROVATA
131200970306E   1C                   ENDIF
131300970306     C*
131400970306     C                   ENDSR
131500970306     C*------------------------------------------------------------------------*
131600970306     C* CALGCV - CALCOLA LE SPESE DI GIACENZA VARIE
131700970306     C*------------------------------------------------------------------------*
131800970306     C     CALGCV        BEGSR
131900970306     C*
132000970306     C* REIMPOSTA LE VARIABILI DI LAVORO
132100011031     C                   Z-ADD     *ZEROS        TASSGV           21 8
132200011031     C                   Z-ADD     *ZEROS        TASSGD           21 8
132300970306     C*
132400970306     C* CALCOLA LE SPESE VARIE
132500970306IF  1C     TGCSGV        IFGT      *ZEROS                                       *IMMESSA TARIFFA
132600970306     C                   Z-ADD     TGCSGV        TASSGV
132700060125     c                   if        (tgcfaf = '2' or tgcfaf='3') and
132800060125     c                             etcggg <= tgcsgf
132900060124     C                   Z-ADD     *zeros        TASSGV
133000970306E   1C                   ENDIF
133100060124E   1C                   ENDIF
133200970306     C*
133300970306     C* CALCOLA LE SPESE DOSSIER
133400970306IF  1C     TGCSGD        IFGT      *ZEROS                                       *IMMESSA TARIFFA
133500970306     C     TGCSGD        MULT      ETCNRG        TASSGD
133600970306     C*
133700970306     C* AZZERA LE SPESE DOSSIER SE SI E' DENTRO AI GIORNI DI FRANCHIGIA
133800060125     c                   if        (tgcfaf = '2' or tgcfaf='3') and
133900060124     c                             etcggg <= tgcsgf
134000970306     C                   Z-ADD     *ZEROS        TASSGD                         *NO SPESE DOSSIER
134100060124E   2C                   ENDIF
134200970306E   1C                   ENDIF
134300970306     C*
134400970314IF  1C     TASSGV        IFGT      *ZEROS
134500970314     C     TASSGD        ORGT      *ZEROS
134600970306     C                   Z-ADD     8             I
134700970306     C     TASSGV        ADD       TASSGD        VIMP(I)
134800970314E   1C                   ENDIF
134900970306     C*
135000970306     C                   ENDSR
135100001227     C*------------------------------------------------------------------------*
135200001227     C* CAL047 - CALCOLA ADG SU GIACENZA VARIE DOSSIER
135300001227     C*------------------------------------------------------------------------*
135400001227     C     CAL047        BEGSR
135500001227     C*
135600001227     C* REIMPOSTA LE VARIABILI DI LAVORO
135700011031     C                   Z-ADD     *ZEROS        TASADG           21 8
135800001227     C*
135900001227     C* CALCOLA L'A.D.G.
136000011031     C                   Z-ADD     VIMP(8)       WIMP             23 8
136100001227     C                   EXSR      ADGCAL
136200001227     C*
136300001227IF  1C     TASADG        IFGT      *ZEROS
136400001227     C                   Z-ADD     47            I
136500001227     C                   Z-ADD     TASADG        VIMP(I)
136600001227E   1C                   ENDIF
136700001227     C*
136800001227     C                   ENDSR
136900001227     C*------------------------------------------------------------------------*
137000001227     C* CAL064 - CALCOLA ISTAT SU ADG/GIAC.VARIE DOSSIER
137100001227     C*------------------------------------------------------------------------*
137200001227     C     CAL064        BEGSR
137300001227     C*
137400001227     C* REIMPOSTA LE VARIABILI DI LAVORO
137500011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
137600001227     C*
137700001227     C* CALCOLA L'ISTAT
137800011031     C                   Z-ADD     VIMP(47)      WIMP             23 8
137900001227     C                   EXSR      IACCAL
138000001227     C*
138100001227IF  1C     TASIAC        IFGT      *ZEROS
138200001227     C                   Z-ADD     64            I
138300001227     C                   Z-ADD     TASIAC        VIMP(I)
138400001227E   1C                   ENDIF
138500001227     C*
138600001227     C                   ENDSR
138700970306     C*------------------------------------------------------------------------*
138800970306     C* VALVO2 - VALORIZZA LE VOCI -DIPENDENTI DA SCAGLIONI-
138900970306     C*------------------------------------------------------------------------*
139000970306     C     VALVO2        BEGSR
139100970306     C*
139200970306     C* AZZERA GLI IMPORTI DELLE VOCI CON SEQUENZA DUE
139300970306DO  1C     1             DO        NVOCA         I
139400970306IF  2C     VS(I)         IFEQ      2
139500970306     C                   Z-ADD     *ZEROS        VIMP(I)                        *IMPORTO CALCOLATO
139600970306E   2C                   ENDIF
139700970306E   1C                   ENDDO
139800970306     C*
139900970306     C* CALCOLA LE SPESE DI GIACENZA DI RICONSEGNA PROVINCIA
140000970306IF  1C     WERR          IFEQ      'N'
140100970306IF  2C     VC(18)        IFEQ      'S'                                          *VOCE DA CALCOLARE
140200970306     C     $TGC          ANDEQ     '1'                                          *TROVATA TAR.GIACENZ
140300970306     C                   EXSR      CALGRP
140400001227     C                   EXSR      CAL057
140500001227     C                   EXSR      CAL074
140600970306E   2C                   ENDIF
140700970306E   1C                   ENDIF
140800970306     C*
140900970306     C* CALCOLA LE SPESE DI GIACENZA DI RICONSEGNA CITTA'
141000970306IF  1C     WERR          IFEQ      'N'
141100970306IF  2C     VC(19)        IFEQ      'S'                                          *VOCE DA CALCOLARE
141200970306     C     $TGC          ANDEQ     '1'                                          *TROVATA TAR.GIACENZ
141300970306     C                   EXSR      CALGRC
141400001227     C                   EXSR      CAL058
141500001227     C                   EXSR      CAL075
141600970306E   2C                   ENDIF
141700970306E   1C                   ENDIF
141800970306     C*
141900970306     C* CALCOLA LE SPESE DI GIACENZA DI SOSTA
142000970306IF  1C     WERR          IFEQ      'N'
142100970306IF  2C     VC(20)        IFEQ      'S'                                          *VOCE DA CALCOLARE
142200970306     C     $TGC          ANDEQ     '1'                                          *TROVATA TAR.GIACENZ
142300970306     C                   EXSR      CALGSO
142400001227     C                   EXSR      CAL059
142500001227     C                   EXSR      CAL076
142600970306E   2C                   ENDIF
142700970306E   1C                   ENDIF
142800970306     C*
142900970306     C                   ENDSR
143000970306     C*------------------------------------------------------------------------*
143100970306     C* CALTCP - CALCOLA T.C.P.
143200970306     C*------------------------------------------------------------------------*
143300970306     C     CALTCP        BEGSR
143400990819     C*
143500990819     C* REIMPOSTA LE VARIABILI DI LAVORO
143600990819     C                   MOVEL     'S'           WCAR                           *SI UTILIZZO TAR.CAR
143700011031     C                   Z-ADD     *ZEROS        TASTMP           21 8
143800990819     C*
143900990819     C* CALCOLA CONSEGNA PARTICOLARE
144000990819     C                   MOVEL     '7'           TASTC             1            *CODICE CONSEGNA
144100011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
144200990819     C                   EXSR      CALCP
144300990819     C                   Z-ADD     TASCP         TASTMP
144400970306     C*
144500970314IF  1C     TASTMP        IFGT      *ZEROS
144600970306     C                   Z-ADD     5             I
144700970306     C                   Z-ADD     TASTMP        VIMP(I)
144800970314E   1C                   ENDIF
144900990819     C*
145000970306     C                   ENDSR
145100001222     C*------------------------------------------------------------------------*
145200001222     C* CAL046 - CALCOLA ADG SU TCP
145300001222     C*------------------------------------------------------------------------*
145400001222     C     CAL046        BEGSR
145500001222     C*
145600001222     C* REIMPOSTA LE VARIABILI DI LAVORO
145700011031     C                   Z-ADD     *ZEROS        TASADG           21 8
145800001222     C*
145900001222     C* CALCOLA L'A.D.G.
146000011031     C                   Z-ADD     VIMP(5)       WIMP             23 8
146100001222     C                   EXSR      ADGCAL
146200001222     C*
146300001222IF  1C     TASADG        IFGT      *ZEROS
146400001222     C                   Z-ADD     46            I
146500001222     C                   Z-ADD     TASADG        VIMP(I)
146600001222E   1C                   ENDIF
146700001222     C*
146800001222     C                   ENDSR
146900001222     C*------------------------------------------------------------------------*
147000001222     C* CAL063 - CALCOLA ISTAT SU ADG/TCP
147100001222     C*------------------------------------------------------------------------*
147200001222     C     CAL063        BEGSR
147300001222     C*
147400001222     C* REIMPOSTA LE VARIABILI DI LAVORO
147500011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
147600001222     C*
147700001222     C* CALCOLA L'ISTAT
147800011031     C                   Z-ADD     VIMP(46)      WIMP             23 8
147900001222     C                   EXSR      IACCAL
148000001222     C*
148100001222IF  1C     TASIAC        IFGT      *ZEROS
148200001222     C                   Z-ADD     63            I
148300001222     C                   Z-ADD     TASIAC        VIMP(I)
148400001222E   1C                   ENDIF
148500001222     C*
148600001222     C                   ENDSR
148700040826     C*------------------------------------------------------------------------*
148800040826     C* CALdps - CALCOLA diritto di pesatura
148900040826     C*------------------------------------------------------------------------*
149000040826     C     CALdps        BEGSR
149100040826     C*
149200040826     C* REIMPOSTA LE VARIABILI DI LAVORO
149300040826     C                   MOVEL     'S'           WCAR                           *SI UTILIZZO TAR.CAR
149400040826     C                   Z-ADD     *ZEROS        TASdps           21 8
149500040826     C*
149600040826     C* CALCOLA CONSEGNA PARTICOLARE
149700040826     C                   MOVEL     '5'           TASTC             1            *CODICE CONSEGNA
149800040826     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
149900040826     C                   EXSR      CALCP
150000040826     C                   Z-ADD     TASCP         TASdps
150100040826     C*
150200040826IF  1C     TASdps        IFGT      *ZEROS
150300040826     C                   Z-ADD     102           I
150400040826     C                   Z-ADD     TASdps        VIMP(I)
150500040826E   1C                   ENDIF
150600040826     C*
150700040826     C                   ENDSR
150800040826     C*------------------------------------------------------------------------*
150900040826     C* CAL103 - CALCOLA ADG SU TCP
151000040826     C*------------------------------------------------------------------------*
151100040826     C     CAL103        BEGSR
151200040826     C*
151300040826     C* REIMPOSTA LE VARIABILI DI LAVORO
151400040826     C                   Z-ADD     *ZEROS        TASADG           21 8
151500040826     C*
151600040826     C* CALCOLA L'A.D.G.
151700040826     C                   Z-ADD     VIMP(102)     WIMP             23 8
151800040826     C                   EXSR      ADGCAL
151900040826     C*
152000040826IF  1C     TASADG        IFGT      *ZEROS
152100040826     C                   Z-ADD     103           I
152200040826     C                   Z-ADD     TASADG        VIMP(I)
152300040826E   1C                   ENDIF
152400040826     C*
152500040826     C                   ENDSR
152600040826     C*------------------------------------------------------------------------*
152700040826     C* CAL104 - CALCOLA ISTAT SU ADG/TCP
152800040826     C*------------------------------------------------------------------------*
152900040826     C     CAL104        BEGSR
153000040826     C*
153100040826     C* REIMPOSTA LE VARIABILI DI LAVORO
153200040826     C                   Z-ADD     *ZEROS        TASIAC           21 8
153300040826     C*
153400040826     C* CALCOLA L'ISTAT
153500040826     C                   Z-ADD     VIMP(103)     WIMP             23 8
153600040826     C                   EXSR      IACCAL
153700040826     C*
153800040826IF  1C     TASIAC        IFGT      *ZEROS
153900040826     C                   Z-ADD     104           I
154000040826     C                   Z-ADD     TASIAC        VIMP(I)
154100040826E   1C                   ENDIF
154200040826     C*
154300040826     C                   ENDSR
154400970306     C*------------------------------------------------------------------------*
154500970306     C* CALGRP - CALCOLA LE SPESE DI GIACENZA RICONSEGNA PROVINCIA
154600970306     C*------------------------------------------------------------------------*
154700970306     C     CALGRP        BEGSR
154800970306     C*
154900970306     C* REIMPOSTA LE VARIABILI DI LAVORO
155000011031     C                   Z-ADD     *ZEROS        TASSGP           21 8
155100970306     C*
155200970306     C* CALCOLA IL PESO TASSABILE -GIACENZA-
155300970306IF  1C     TGCSGP        IFGT      *ZEROS                                       *IMMESSA TARIFFA
155400970306     C                   Z-ADD     *ZEROS        §PESCA
155500970306     C                   EXSR      CALPTG
155600970306     C*
155700970306     C* CALCOLA LE SPESE DI GIACENZA
155800970306     C     TGCSGP        MULT      ETCNRG        TASSGR                         *SPESE X N°RIAPERTUR
155900970306     C     TASSGR        MULT      §PESCA        TASSGR                         *SPESE X PESO TASSAB
156000970306     C*
156100970306     C* AZZERA LE SPESE DI RICONSEGNA SE SI E' DENTRO AI GIORNI DI FRANCHIGIA
156200970306IF  2C     TGCFAF        IFEQ      '2'                                          *FRANCHIGIA
156300970306     C     ETCGGG        ANDLE     TGCSGF                                       *GG SOSTA < GG FRANC
156400970306     C                   Z-ADD     *ZEROS        TASSGP                         *NO SPESE RICONSEGNA
156500970306E   2C                   ENDIF
156600970306E   1C                   ENDIF
156700970306     C*
156800970314IF  1C     TASSGP        IFGT      *ZEROS
156900970306     C                   Z-ADD     18            I
157000970306     C                   Z-ADD     TASSGP        VIMP(I)
157100970314E   1C                   ENDIF
157200970306     C*
157300970306     C                   ENDSR
157400001227     C*------------------------------------------------------------------------*
157500001227     C* CAL057 - CALCOLA ADG SU GIACENZA RICONSEGNA PROVINCIA
157600001227     C*------------------------------------------------------------------------*
157700001227     C     CAL057        BEGSR
157800001227     C*
157900001227     C* REIMPOSTA LE VARIABILI DI LAVORO
158000011031     C                   Z-ADD     *ZEROS        TASADG           21 8
158100001227     C*
158200001227     C* CALCOLA L'A.D.G.
158300011031     C                   Z-ADD     VIMP(18)      WIMP             23 8
158400001227     C                   EXSR      ADGCAL
158500001227     C*
158600001227IF  1C     TASADG        IFGT      *ZEROS
158700001227     C                   Z-ADD     57            I
158800001227     C                   Z-ADD     TASADG        VIMP(I)
158900001227E   1C                   ENDIF
159000001227     C*
159100001227     C                   ENDSR
159200001227     C*------------------------------------------------------------------------*
159300001227     C* CAL074 - CALCOLA ISTAT SU ADG/GIACENZA RICONSEGNA PROVINCIA
159400001227     C*------------------------------------------------------------------------*
159500001227     C     CAL074        BEGSR
159600001227     C*
159700001227     C* REIMPOSTA LE VARIABILI DI LAVORO
159800011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
159900001227     C*
160000001227     C* CALCOLA L'ISTAT
160100011031     C                   Z-ADD     VIMP(57)      WIMP             23 8
160200001227     C                   EXSR      IACCAL
160300001227     C*
160400001227IF  1C     TASIAC        IFGT      *ZEROS
160500001227     C                   Z-ADD     74            I
160600001227     C                   Z-ADD     TASIAC        VIMP(I)
160700001227E   1C                   ENDIF
160800001227     C*
160900001227     C                   ENDSR
161000970306     C*------------------------------------------------------------------------*
161100970306     C* CALGRC - CALCOLA LE SPESE DI GIACENZA RICONSEGNA CITTA'
161200970306     C*------------------------------------------------------------------------*
161300970306     C     CALGRC        BEGSR
161400970306     C*
161500970306     C* REIMPOSTA LE VARIABILI DI LAVORO
161600011031     C                   Z-ADD     *ZEROS        TASSGR           21 8
161700970306     C*
161800970306     C* CALCOLA IL PESO TASSABILE -GIACENZA-
161900970306IF  1C     TGCSGR        IFGT      *ZEROS                                       *IMMESSA TARIFFA
162000970306     C                   Z-ADD     *ZEROS        §PESCA
162100970306     C                   EXSR      CALPTG
162200970306     C*
162300970306     C* CALCOLA LE SPESE DI GIACENZA
162400970306     C     TGCSGR        MULT      ETCNRG        TASSGR                         *SPESE X N°RIAPERTUR
162500970306     C     TASSGR        MULT      §PESCA        TASSGR                         *SPESE X PESO TASSAB
162600970306     C*
162700970306     C* AZZERA LE SPESE DI RICONSEGNA SE SI E' DENTRO AI GIORNI DI FRANCHIGIA
162800970306IF  2C     TGCFAF        IFEQ      '2'                                          *FRANCHIGIA
162900970306     C     ETCGGG        ANDLE     TGCSGF                                       *GG SOSTA < GG FRANC
163000970306     C                   Z-ADD     *ZEROS        TASSGR                         *NO SPESE RICONSEGNA
163100970306E   2C                   ENDIF
163200970306E   1C                   ENDIF
163300970306     C*
163400970314IF  1C     TASSGR        IFGT      *ZEROS
163500970306     C                   Z-ADD     19            I
163600970306     C                   Z-ADD     TASSGR        VIMP(I)
163700970314E   1C                   ENDIF
163800970306     C*
163900970306     C                   ENDSR
164000001227     C*------------------------------------------------------------------------*
164100001227     C* CAL058 - CALCOLA ADG SU GIACENZA RICONSEGNA CITTA'
164200001227     C*------------------------------------------------------------------------*
164300001227     C     CAL058        BEGSR
164400001227     C*
164500001227     C* REIMPOSTA LE VARIABILI DI LAVORO
164600011031     C                   Z-ADD     *ZEROS        TASADG           21 8
164700001227     C*
164800001227     C* CALCOLA L'A.D.G.
164900011031     C                   Z-ADD     VIMP(19)      WIMP             23 8
165000001227     C                   EXSR      ADGCAL
165100001227     C*
165200001227IF  1C     TASADG        IFGT      *ZEROS
165300001227     C                   Z-ADD     58            I
165400001227     C                   Z-ADD     TASADG        VIMP(I)
165500001227E   1C                   ENDIF
165600001227     C*
165700001227     C                   ENDSR
165800001227     C*------------------------------------------------------------------------*
165900001227     C* CAL075 - CALCOLA ISTAT SU ADG/GIACENZA RICONSEGNA CITTA'
166000001227     C*------------------------------------------------------------------------*
166100001227     C     CAL075        BEGSR
166200001227     C*
166300001227     C* REIMPOSTA LE VARIABILI DI LAVORO
166400011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
166500001227     C*
166600001227     C* CALCOLA L'ISTAT
166700011031     C                   Z-ADD     VIMP(58)      WIMP             23 8
166800001227     C                   EXSR      IACCAL
166900001227     C*
167000001227IF  1C     TASIAC        IFGT      *ZEROS
167100001227     C                   Z-ADD     75            I
167200001227     C                   Z-ADD     TASIAC        VIMP(I)
167300001227E   1C                   ENDIF
167400001227     C*
167500001227     C                   ENDSR
167600970306     C*------------------------------------------------------------------------*
167700970306     C* CALGSO - CALCOLA LE SPESE DI GIACENZA DI SOSTA
167800970306     C*------------------------------------------------------------------------*
167900970306     C     CALGSO        BEGSR
168000970306     C*
168100970306     C     *LIKE         DEFINE    ETCGGG        SAVGGG
168200970306     C     *LIKE         DEFINE    ETCGGG        GGGETC
168300970306     C*
168400970306     C* REIMPOSTA LE VARIABILI DI LAVORO
168500011031     C                   Z-ADD     *ZEROS        TASSGS           21 8
168600970306     C                   Z-ADD     *ZEROS        X                 1 0
168700970306     C                   Z-ADD     *ZEROS        XP                1 0
168800970306     C                   Z-ADD     *ZEROS        SGG                            *GG SOSTA
168900970306     C                   Z-ADD     *ZEROS        STU                            *TARIFFA UNITARIA
169000970306     C                   Z-ADD     *ZEROS        STT                            *TARIFFA TOTALE
169100970306     C*
169200970306     C                   Z-ADD     ETCGGG        SAVGGG
169300970306     C                   Z-ADD     ETCGGG        GGGETC
169400970306DO  1C     1             DO        3             X
169500970306IF  2C     SAVGGG        IFGT      TGS(X)
169600970306     C     SAVGGG        SUB       TGS(X)        GGGETC
169700970306IF  3C     X             IFEQ      1
169800970306     C                   Z-ADD     TGS(X)        SGG(X)
169900970306X   3C                   ELSE
170000970306     C     TGS(X)        SUB       TGS(XP)       SGG(X)
170100970306E   3C                   ENDIF
170200970306     C                   Z-ADD     TTS(X)        STU(X)
170300970306X   2C                   ELSE
170400970306     C                   Z-ADD     GGGETC        SGG(X)
170500970306     C                   Z-ADD     TTS(X)        STU(X)
170600970306     C                   Z-ADD     *ZEROS        SAVGGG
170700970306     C                   Z-ADD     *ZEROS        GGGETC
170800970306E   2C                   ENDIF
170900970306     C                   ADD       1             XP
171000970306E   1C                   ENDDO
171100970306     C*
171200970306     C* CALCOLA IL PESO TASSABILE -GIACENZA-
171300970306     C                   Z-ADD     *ZEROS        §PESCA
171400970306     C                   EXSR      CALPTG
171500970306     C*
171600970306     C* CALCOLA LE SPESE DI GIACENZA
171700970306     C     SGG           MULT      STU           STT
171800970306     C     STT           MULT      §PESCA        STT
171900970306     C                   XFOOT     STT           TASSGS
172000970306     C*
172100970306     C* APPLICA IL MINIMO TARIFFARIO SE SUPERATI I GIORNI DI FRANCHIGIA
172200970306IF  1C     TASSGS        IFLT      TGCSTM
172300970306     C     ETCGGG        ANDGT     TGCSGF
172400970306     C                   Z-ADD     TGCSTM        TASSGS
172500970306E   1C                   ENDIF
172600970306     C*
172700970306     C* AZZERA LE SPESE DI SOSTA SE SI E' DENTRO AI GIORNI DI FRANCHIGIA
172800970306IF  1C     TGCFAF        IFNE      *BLANKS                                      *FRANCHIGIA
172900970306     C     ETCGGG        ANDLE     TGCSGF                                       *GG SOSTA < GG FRANC
173000970306     C                   Z-ADD     *ZEROS        TASSGS                         *NO SPESE SOSTA
173100970306E   1C                   ENDIF
173200970306     C*
173300970314IF  1C     TASSGS        IFGT      *ZEROS
173400970306     C                   Z-ADD     20            I
173500970306     C                   Z-ADD     TASSGS        VIMP(I)
173600970314E   1C                   ENDIF
173700970306     C*
173800970306     C                   ENDSR
173900001227     C*------------------------------------------------------------------------*
174000001227     C* CAL059 - CALCOLA ADG SU GIACENZA SOSTA
174100001227     C*------------------------------------------------------------------------*
174200001227     C     CAL059        BEGSR
174300001227     C*
174400001227     C* REIMPOSTA LE VARIABILI DI LAVORO
174500011031     C                   Z-ADD     *ZEROS        TASADG           21 8
174600001227     C*
174700001227     C* CALCOLA L'A.D.G.
174800011031     C                   Z-ADD     VIMP(20)      WIMP             23 8
174900001227     C                   EXSR      ADGCAL
175000001227     C*
175100001227IF  1C     TASADG        IFGT      *ZEROS
175200001227     C                   Z-ADD     59            I
175300001227     C                   Z-ADD     TASADG        VIMP(I)
175400001227E   1C                   ENDIF
175500001227     C*
175600001227     C                   ENDSR
175700001227     C*------------------------------------------------------------------------*
175800001227     C* CAL076 - CALCOLA ISTAT SU ADG/GIACENZA SOSTA
175900001227     C*------------------------------------------------------------------------*
176000001227     C     CAL076        BEGSR
176100001227     C*
176200001227     C* REIMPOSTA LE VARIABILI DI LAVORO
176300011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
176400001227     C*
176500001227     C* CALCOLA L'ISTAT
176600011031     C                   Z-ADD     VIMP(59)      WIMP             23 8
176700001227     C                   EXSR      IACCAL
176800001227     C*
176900001227IF  1C     TASIAC        IFGT      *ZEROS
177000001227     C                   Z-ADD     76            I
177100001227     C                   Z-ADD     TASIAC        VIMP(I)
177200001227E   1C                   ENDIF
177300001227     C*
177400001227     C                   ENDSR
177500970306     C*------------------------------------------------------------------------*
177600970306     C* CALPTG - CALCOLA IL PESO TASSABILE -GIACENZA-
177700970306     C*------------------------------------------------------------------------*
177800970306     C     CALPTG        BEGSR
177900970306     C*
178000970306     C* REIMPOSTA LE VARIABILI DI LAVORO
178100970306     C                   Z-ADD     *ZEROS        §PESVO           13 3
178200970306     C                   Z-ADD     *ZEROS        §PES§            13 3
178300970306     C                   Z-ADD     *ZEROS        §PESCA            6 0
178400970306     C*
178500970725     C     TGCRPV        MULT      SVSC(S)       §PESVO
178600970306     C     §PESVO        MULT      100           §PESVO
178700970306IF  1C     §PESVO        IFGT      SPSC(S)
178800970306     C     §PESVO        DIV       100           §PES§
178900970306X   1C                   ELSE
179000970306     C     SPSC(S)       DIV       100           §PES§
179100970306E   1C                   ENDIF
179200970306     C     §PES§         ADD       0,99          §PESCA
179300970306     C*
179400970306     C                   ENDSR
179500970306     C*------------------------------------------------------------------------*
179600970306     C* CARTAD - CARICAMENTO TARIFFA CLIENTE -DETTAGLIO-
179700970306     C*------------------------------------------------------------------------*
179800970306     C     CARTAD        BEGSR
179900970306     C*
180000970306     C* REIMPOSTA LE VARIABILI DI LAVORO
180100980707DO  1C     1             DO        11            I
180200970306     C     I             OCCUR     TARDS
180300970306     C                   CLEAR                   TARDS
180400970306E   1C                   ENDDO
180500970306     C                   MOVEL     '0'           $TAD                           *TARIFFA TROVATA
180600970306     C*
180700970306     C* CERCA LA TARIFFA CLIENTE CON IL CODICE TASSAZIONE ELEMENTARE
180800970306     C                   Z-ADD     D00KSC        KTDKSC                         *CLIENTE
180900970306     C                   Z-ADD     D00PRG        KTDPRG                         *PROGRESSIVO
181000970306     C                   Z-ADD     D00LNP        KTDLNP                         *LINEA DI PARTENZA
181100990819     C                   MOVEL     CCTS(T)       KTDCTS                         *COD.TASS.ELEMENTARE
181200990824     C                   MOVEL     *BLANKS       KTDNAZ                         *NAZIONE
181300970306     C                   MOVEL     *BLANKS       KTDCAP                         *C.A.P.
181400970306     C                   Z-ADD     D00CTR        KTDCTR                         *CODICE TARIFFA
181500990819     C     KEYTAD        SETLL     TITAD00L                               98
181600970306IF  1C     *IN98         IFEQ      *ON                                          *TARIFFA TROVATA
181700970306     C                   MOVEL     '1'           $TAD
181800970306X   1C                   ELSE                                                   *TARIFFA NON TROVATA
181900970306     C*
182000970306     C* CERCA LA TARIFFA CLIENTE CON IL CODICE TASSAZIONE REGIONE
182100990803     C                   MOVEL     CRAP(T)       KTDCTS                         *COD.TASS.REGIONE
182200990819     C     KEYTAD        SETLL     TITAD00L                               98
182300990819IF  2C     *IN98         IFEQ      *ON                                          *TARIFFA TROVATA
182400970306     C                   MOVEL     '1'           $TAD
182500990819X   2C                   ELSE                                                   *TARIFFA NON TROVATA
182600970306     C*
182700970306     C* CERCA LA TARIFFA CLIENTE CON IL CODICE TASSAZIONE NAZIONE
182800990803     C                   MOVEL     CNAR(T)       KTDCTS                         *COD.TASS.NAZIONE
182900990819     C     KEYTAD        SETLL     TITAD00L                               98
183000990819IF  3C     *IN98         IFEQ      *ON                                          *TARIFFA TROVATA
183100970306     C                   MOVEL     '1'           $TAD
183200990819E   3C                   ENDIF
183300990819E   2C                   ENDIF
183400990819E   1C                   ENDIF
183500970306     C*
183600970306     C* SE TARIFFA NON TROVATA, ERRORE
183700970306IF  1C     $TAD          IFEQ      '0'
183800970306     C                   MOVEL     MSG(24)       $MSG
183900970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
184000970306     C*
184100970306     C* SE TARIFFA TROVATA, CARICA DETTAGLIO
184200970306X   1C                   ELSE
184300980707     C                   EXSR      CARDST
184400970306E   1C                   ENDIF
184500970306     C*
184600970306     C                   ENDSR
184700980707     C*------------------------------------------------------------------------*
184800980707     C* CARDST - CARICA CAMPI TARIFFA -DETTAGLIO-
184900980707     C*------------------------------------------------------------------------*
185000980707     C     CARDST        BEGSR
185100980707     C*
185200980707     C     *LIKE         DEFINE    TADRPV        RAPPV
185300980707     C                   DO        11            A                 5 0
185400980707     C     A             OCCUR     TARDS
185500980707     C                   CLEAR                   TARDS
185600980707     C                   ENDDO
185700980707     C                   Z-ADD     0             RAPPV
185800980707     C                   CLEAR                   WELAMT
185900980908     C                   Z-ADD     DMITAS        MINTAS                         *MIN.TASS. TNTAM
186000980707    1C                   DO        *HIVAL        A
186100990819     C**
186200980707     C** NON DEVO RILEGGERE PERCHE' DEVO CARICARE LO STESSO SCAGLIONE
186300980707    2C     WELAMT        IFNE      'S'
186400990819     C     KEYTAD        READE     TITAD000                               15
186500980707     C   15              GOTO      ENDDST
186600991125     C*
186700991125     C* CONTROLLA SE CONVERTIRE I CAMPI DI TNTAM
186800011026    3C     D00DIV        IFNE      WDIVCE
186900011019     C                   MOVEL     D00DIV        WDVI
187000991125     C                   EXSR      CNVTAD
187100991125    3C                   ENDIF
187200991125    2C                   ENDIF
187300980707     C** ELABORATO UNA VOLTA NON LO FACCIO PIU'
187400980707    2C     WELAMT        IFEQ      'S'
187500980707     C                   MOVEL     'E'           WELAMT            1
187600980707    2C                   ENDIF
187700980707     C**
187800980707     C     A             OCCUR     TARDS
187900980707     C* SE LO SCAGLIONE LETTO E' > DEL MINIMO TASSABILE CARICO
188000980707     C*  ANCHE LO SCAGLIONE DEL MINIMO CON TARIFFA=TARIFFA X MIN.TAS
188100980707     C**
188200980707     C                   Z-ADD     TADLNP        TLNP
188300980707     C                   MOVEL     TADCTS        TCTS
188400990819     C                   MOVEL     TADNAZ        TNAZ
188500990819     C                   MOVEL     TADCAP        TCAP
188600980707     C                   Z-ADD     TADCTR        TCTR
188700980707     C                   Z-ADD     TADRPV        TRPV
188800980707     C                   Z-ADD     TADMIN        TMIN
188900980707     C                   Z-ADD     TADMAX        TMAX
189000980707     C                   Z-ADD     TADRPV        RAPPV
189100991125     C*
189200980707     C** SE HO GIA' ELABORATO IL MINIMO NON LO FACCIO PIU'
189300980707    2C     WELAMT        IFNE      'E'
189400980707     C**
189500980707    3C     TADSGL        IFEQ      MINTAS
189600980707     C                   MOVEL     'E'           WELAMT
189700980707   X3C                   ELSE
189800980707     C** SE LO SCAGLIONE CHE LEGGO E' MAGGIORE DEL MINIMO TASSABILE
189900980707     C*  UTILIZZO PRIMA LA TARIFFA DELLO SCAGLIONE PER IL MINIMO T.
190000980707     C*  (MOLTIPLICANDOLA PER IL MINIMO TASSABILE)
190100980707     C*  POI LO UTILIZZO PER LO SCAGLIONE LETTO
190200980707    4C     TADSGL        IFGT      MINTAS
190300980707     C                   MOVEL     'S'           WELAMT
190400980707     C                   Z-ADD     MINTAS        TSCG
190500980707     C**
190600980707     C* SE NON C'E' APPLICAZIONE TARIFFA FINITA -- MOLTIPLICO
190700980707    5C     TAMATA        IFEQ      0
190800980707     C     TAMATA        ORLE      MINTAS
190900980707     C     TADSGL        ORGT      TAMATA
191000980707     C**
191100991125    6C     UNOCTR        IFEQ      4
191200980707     C     UNOCTR        OREQ      0
191300011031     C     MINTAS        DIV(H)    100           WMTAS            21 8
191400980707     C                   ELSE
191500980707     C                   Z-ADD     MINTAS        WMTAS
191600980707    6C                   ENDIF
191700980707     C* TARIFFA
191800010926     C     WMTAS         MULT(H)   TADITR        TITR
191900980707     C     WMTAS         MULT(H)   TADINO        TINO
192000980707     C                   ELSE
192100980707     C* C'E' APPLICAZIONE MAGGIORE --> UTILIZZO LA TARIFFA COSI' COM'E'
192200980707     C                   Z-ADD     TADITR        TITR
192300980707     C                   Z-ADD     TADINO        TINO
192400980707    5C                   ENDIF
192500980707    4C                   ENDIF
192600980707    3C                   ENDIF
192700980707    2C                   ENDIF
192800980707     C*
192900980707     C* SE NON HO CARICATO LO SCAGLIONE PER IL MINIMO TASSABILE
193000980707    2C     WELAMT        IFNE      'S'
193100980707     C                   Z-ADD     TADSGL        TSCG
193200980707     C                   Z-ADD     TADITR        TITR
193300980707     C                   Z-ADD     TADINO        TINO
193400980707    2C                   ENDIF
193500980707    1C                   ENDDO
193600991125     C*
193700980707     C     ENDDST        ENDSR
193800980707     C*------------------------------------------------------------------------*
193900980707     C* CALQTA - REPERISCE E CALCOLA LA QUANTITA' DA UTILIZZARE
194000980707     C*------------------------------------------------------------------------*
194100980707     C     CALQTA        BEGSR
194200980707     C*
194300980707     C* PESO TASSABILE E QUANTITA' DA FATTURARE
194400980707     C                   MOVEL     UNOCTR        WTPG              1
194500980707     C                   CLEAR                   WRPV
194600980707     C                   MOVEL     'TAD'         WDET              3
194700980707     C                   Z-ADD     TAMARL        WARL
194800980707     C                   Z-ADD     TAMARF        WARF
194900980707     C                   Z-ADD     TAMARO        WARO
195000980707     C                   EXSR      CALCOL
195100980707     C                   Z-ADD     ISOPES        PESTAS           13 3
195200980707     C*
195300980707     C                   ENDSR
195400980707     C*------------------------------------------------------------------------*
195500980707     C* CALCOL - CALCOLA RPV, ARROTONDAMENTI E PESO TASSABILE
195600980707     C*------------------------------------------------------------------------*
195700980707     C     CALCOL        BEGSR
195800980707     C*
195900980707     C     *LIKE         DEFINE    TAMARL        ARR
196000980707     C     *LIKE         DEFINE    TPTRPV        WRPV
196100980707     C     *LIKE         DEFINE    TPTARL        WARL
196200980707     C     *LIKE         DEFINE    TPTARF        WARF
196300980707     C     *LIKE         DEFINE    TPTARO        WARO
196400980707     C                   Z-ADD     0             ISOPES           13 3
196500980707SL  1C                   SELECT
196600980707     C* TARIFFA A QLI
196700980707     C     WTPG          WHENEQ    '0'
196800980707     C                   Z-ADD     SPSC(S)       ISOPES
196900980707     C* TARIFFA A COLLI
197000980707     C     WTPG          WHENEQ    '1'
197100980707     C                   Z-ADD     SCSC(S)       ISOPES
197200980707     C* TARIFFA A SPEDIZIONE
197300980707     C     WTPG          WHENEQ    '2'
197400980707     C                   Z-ADD     1             ISOPES
197500980707     C* TARIFFA A KG
197600980707     C     WTPG          WHENEQ    '3'
197700980707     C                   Z-ADD     SPSC(S)       ISOPES
197800980707E   1C                   ENDSL
197900980707     C** MINIMO TASSABILE STD
198000980707     C                   Z-ADD     0             MINTAS
198100980707     C**
198200980707     C                   Z-ADD     1             A
198300980707     C     WTPG          LOOKUP    CTR(A)                                 05
198400980707     C   05A             OCCUR     DSTR
198500980707     C   05              Z-ADD     §TRATA        MINTAS
198600980707     C**
198700980707     C** SOLO PER TAD (PER TPD NON MI SERVE)
198800980707    1C     WDET          IFEQ      'TAD'
198900980707     C                   CLEAR                   MINTAR
199000980707     C** UTILIZZO IL CAMPO CON I DECIMALI PER IL MINIMO TASSABILE
199100980707     C*  DA TABELLA TR. SE POI C'E' ANCHE IN TARIFFA USO QUELLO
199200980707     C   05              Z-ADD     §TRATA        MINTAR
199300980707    2C     TAMATA        IFGT      0
199400980707     C                   Z-ADD     TAMATA        MINTAR
199500980707    2C                   ENDIF
199600980707    1C                   ENDIF
199700980707     C**
199800980707     C** RPV SOLO PER TARIFFA 0 E 3
199900980707    1C     §TRRPV        IFEQ      'S'
200000980707     C** PER TAD PRENDO RPV DA RIGA DI DETTAGLIO CON PESO REALE
200100980707    2C     WDET          IFEQ      'TAD'
200200980707     C     SVSC(S)       ANDGT     0
200300980707    3C                   DO        11            A
200400980707     C     A             OCCUR     TARDS
200500980707    4C     TSCG          IFGE      ISOPES
200600980707     C                   GOTO      POITAS
200700980707    4C                   END
200800980707    3C                   END
200900980707     C** RICERCO RPV CON PESO REALE
201000980707    3C     TSCG          IFLT      ISOPES
201100980707     C                   MOVEL     MSG(25)       $MSG
201200980707     C                   EXSR      IMPERR
201300980707    3C                   END
201400980707     C     POITAS        TAG
201500980707     C                   Z-ADD     TRPV          WRPV
201600980707    2C                   ENDIF
201700980707     C**
201800980707    2C     SVSC(S)       IFNE      0
201900980707     C     WRPV          ANDNE     0
202000980707     C                   Z-ADD     0             COMPRV            9 3
202100980707     C     SVSC(S)       MULT      WRPV          COMPRV
202200980707     C     COMPRV        MULT      100           ISOPE2           13 3
202300980707    3C     ISOPE2        IFGT      ISOPES
202400980707     C                   Z-ADD     ISOPE2        ISOPES
202500980707    3C                   ENDIF
202600980707    2C                   ENDIF
202700980707    1C                   ENDIF
202800980707     C**
202900980707     C** ARROTONDAMENTI: LO FACCIO SEOCONDO QUANTO INDICATO IN DSTR
203000980707    0C     §TRARR        IFEQ      'S'
203100980707     C**
203200980707     C** ARROTONDAMENTO: TARIFFE 0 Q.LI - 1 - 3
203300980707     C     ISOPES        IFGE      MINTAS
203400980707     C                   Z-ADD     0             ARR
203500980707     C     ISOPES        IFGT      WARL
203600980707     C                   Z-ADD     WARO          ARR
203700980707     C                   ELSE
203800980707     C                   Z-ADD     WARF          ARR
203900980707     C                   END
204000980707     C**
204100011031     C                   Z-ADD     0             RESTO            21 8
204200011108     C                   Z-ADD     0             ISOPEX           21 0
204300980707     C     ARR           IFNE      0
204400980707     C     ISOPES        ANDNE     0
204500980707     C     ISOPES        DIV       ARR           ISOPEX
204600980707     C                   MVR                     RESTO
204700980707     C     RESTO         IFGT      0
204800980707     C     ISOPEX        MULT      ARR           ISOPES
204900980707     C                   ADD       ARR           ISOPES
205000980707     C                   ENDIF
205100980707     C                   ENDIF
205200980707     C                   ENDIF
205300980707     C**
205400980707    0C                   ENDIF
205500980707     C                   ENDSR
205600980707     C*------------------------------------------------------------------------*
205700980707     C* VALVO3 - VALORIZZA LE VOCI -DIPENDENTI DA CODICE TASSAZIONE-
205800980707     C*------------------------------------------------------------------------*
205900980707     C     VALVO3        BEGSR
206000970306     C*
206100970306     C* AZZERA GLI IMPORTI DELLE VOCI CON SEQUENZA TRE
206200970306DO  1C     1             DO        NVOCA         I
206300970306IF  2C     VS(I)         IFEQ      3
206400970306     C                   Z-ADD     *ZEROS        VIMP(I)                        *IMPORTO CALCOLATO
206500970306E   2C                   ENDIF
206600970306E   1C                   ENDDO
206700980702     C*---
206800980702     C* PORTO E INOLTRO
206900980702     C*---
207000980707IF  1C     WERR          IFEQ      'N'
207100980707IF  2C     VC(1)         IFEQ      'S'                                          *VOCE DA CALCOLARE
207200980707     C     VC(6)         OREQ      'S'                                          *VOCE DA CALCOLARE
207300980707     C                   EXSR      CALPEI
207400980707E   2C                   ENDIF
207500980707E   1C                   ENDIF
207600980702     C*---
207700980702     C* ALTRE PORTO
207800980702     C*---
207900980702     C* CALCOLA A.D.G. SUL PORTO
208000980702IF  1C     WERR          IFEQ      'N'
208100980702IF  2C     VC(3)         IFEQ      'S'                                          *VOCE DA CALCOLARE
208200980702     C                   EXSR      CALAGP
208300980702E   2C                   ENDIF
208400980702E   1C                   ENDIF
208500980702     C*
208600980702     C* CALCOLA ISTAT SUL PORTO
208700980702IF  1C     WERR          IFEQ      'N'
208800980702IF  2C     VC(4)         IFEQ      'S'                                          *VOCE DA CALCOLARE
208900980702     C                   EXSR      CALISP
209000980702E   2C                   ENDIF
209100980702E   1C                   ENDIF
209200980702     C*
209300980702     C* CALCOLA ISTAT SULL'A.D.G. DEL PORTO
209400980702IF  1C     WERR          IFEQ      'N'
209500980702IF  2C     VC(27)        IFEQ      'S'                                          *VOCE DA CALCOLARE
209600980702     C                   EXSR      CALIAP
209700980702E   2C                   ENDIF
209800980702E   1C                   ENDIF
209900980702     C*
210000980702     C* CALCOLA ESPRESSO SUL PORTO
210100980702IF  1C     WERR          IFEQ      'N'
210200980702IF  2C     VC(7)         IFEQ      'S'                                          *VOCE DA CALCOLARE
210300980702     C                   EXSR      CALESP
210400980702E   2C                   ENDIF
210500980702E   1C                   ENDIF
210600980702     C*
210700980702     C* CALCOLA A.D.G. SULL'ESPRESSO PORTO
210800980702IF  1C     WERR          IFEQ      'N'
210900980702IF  2C     VC(23)        IFEQ      'S'                                          *VOCE DA CALCOLARE
211000980702     C                   EXSR      CALAEP
211100980702E   2C                   ENDIF
211200980702E   1C                   ENDIF
211300980702     C*
211400980702     C* CALCOLA ISTAT SULL'ESPRESSO PORTO
211500980702IF  1C     WERR          IFEQ      'N'
211600980702IF  2C     VC(26)        IFEQ      'S'                                          *VOCE DA CALCOLARE
211700980702     C                   EXSR      CALIEP
211800980702E   2C                   ENDIF
211900980702E   1C                   ENDIF
212000980702     C*
212100980702     C* CALCOLA ISTAT SULL'A.D.G. DELL'ESPRESSO PORTO
212200980702IF  1C     WERR          IFEQ      'N'
212300980702IF  2C     VC(30)        IFEQ      'S'                                          *VOCE DA CALCOLARE
212400980702     C                   EXSR      CAIAEP
212500980702E   2C                   ENDIF
212600980702E   1C                   ENDIF
212700980702     C*---
212800980702     C* ALTRE INOLTRO
212900980702     C*---
213000980702     C* CALCOLA A.D.G. SULL'INOLTRO
213100980702IF  1C     WERR          IFEQ      'N'
213200980702IF  2C     VC(21)        IFEQ      'S'                                          *VOCE DA CALCOLARE
213300980702     C                   EXSR      CALAGI
213400980702E   2C                   ENDIF
213500980707E   1C                   ENDIF
213600980702     C*
213700980702     C* CALCOLA ISTAT SULL'INOLTRO
213800980702IF  1C     WERR          IFEQ      'N'
213900980702IF  2C     VC(24)        IFEQ      'S'                                          *VOCE DA CALCOLARE
214000980702     C                   EXSR      CALISI
214100980702E   2C                   ENDIF
214200980702E   1C                   ENDIF
214300980702     C*
214400980702     C* CALCOLA ISTAT SULL'A.D.G. DELL'INOLTRO
214500980702IF  1C     WERR          IFEQ      'N'
214600980702IF  2C     VC(29)        IFEQ      'S'                                          *VOCE DA CALCOLARE
214700980702     C                   EXSR      CALIAI
214800980702E   2C                   ENDIF
214900980702E   1C                   ENDIF
215000980702     C*
215100980702     C* CALCOLA ESPRESSO SULL'INOLTRO
215200980702IF  1C     WERR          IFEQ      'N'
215300980702IF  2C     VC(36)        IFEQ      'S'                                          *VOCE DA CALCOLARE
215400980702     C                   EXSR      CALESI
215500980702E   2C                   ENDIF
215600980702E   1C                   ENDIF
215700980702     C*
215800980702     C* CALCOLA A.D.G. SULL'ESPRESSO INOLTRO
215900980702IF  1C     WERR          IFEQ      'N'
216000980702IF  2C     VC(37)        IFEQ      'S'                                          *VOCE DA CALCOLARE
216100980702     C                   EXSR      CALAEI
216200980702E   2C                   ENDIF
216300980702E   1C                   ENDIF
216400980702     C*
216500980702     C* CALCOLA ISTAT SULL'ESPRESSO INOLTRO
216600980702IF  1C     WERR          IFEQ      'N'
216700980702IF  2C     VC(38)        IFEQ      'S'                                          *VOCE DA CALCOLARE
216800980702     C                   EXSR      CALIEI
216900980702E   2C                   ENDIF
217000980702E   1C                   ENDIF
217100980702     C*
217200980702     C* CALCOLA ISTAT SULL'A.D.G. DELL'ESPRESSO INOLTRO
217300980702IF  1C     WERR          IFEQ      'N'
217400980702IF  2C     VC(34)        IFEQ      'S'                                          *VOCE DA CALCOLARE
217500980702     C                   EXSR      CAIAEI
217600980702E   2C                   ENDIF
217700980702E   1C                   ENDIF
217800980702     C*---
217900980702     C* LOCALITA' DISAGIATE
218000980702     C*---
218100980702     C* CALCOLA CONSEGNA PARTICOLARE LOCALITA' DISAGIATE
218200980702IF  1C     WERR          IFEQ      'N'
218300980702IF  2C     VC(35)        IFEQ      'S'                                          *VOCE DA CALCOLARE
218400980702     C                   EXSR      CALCPL
218500980702E   2C                   ENDIF
218600980702E   1C                   ENDIF
218700980702     C*
218800980702     C* CALCOLA A.D.G. SULLE CONSEGNE LOCALITA' DISAGIATE
218900980702IF  1C     WERR          IFEQ      'N'
219000980702IF  2C     VC(43)        IFEQ      'S'                                          *VOCE DA CALCOLARE
219100980702     C                   EXSR      CALAGL
219200980702E   2C                   ENDIF
219300980702E   1C                   ENDIF
219400980702     C*
219500980702     C* CALCOLA ISTAT SULLE CONSEGNE LOCALITA' DISAGIATE
219600980702IF  1C     WERR          IFEQ      'N'
219700980702IF  2C     VC(44)        IFEQ      'S'                                          *VOCE DA CALCOLARE
219800980702     C                   EXSR      CALISL
219900980702E   2C                   ENDIF
220000980702E   1C                   ENDIF
220100980702     C*
220200980702     C* CALCOLA ISTAT SULL'A.D.G. DELLE CONSEGNE LOCALITA' DISAGIATE
220300980702IF  1C     WERR          IFEQ      'N'
220400980702IF  2C     VC(45)        IFEQ      'S'                                          *VOCE DA CALCOLARE
220500980702     C                   EXSR      CALIAL
220600980702E   2C                   ENDIF
220700980702E   1C                   ENDIF
220800980702     C*
220900980702     C* CALCOLA ESPRESSO SULLA CONSEGNA LOCALITA' DISAGIATE
221000980702IF  1C     WERR          IFEQ      'N'
221100980702IF  2C     VC(39)        IFEQ      'S'                                          *VOCE DA CALCOLARE
221200980702     C                   EXSR      CALESL
221300980702E   2C                   ENDIF
221400980702E   1C                   ENDIF
221500980702     C*
221600980702     C* CALCOLA A.D.G. SULL'ESPRESSO CONSEGNA LOCALITA' DISAGIATE
221700980702IF  1C     WERR          IFEQ      'N'
221800980702IF  2C     VC(40)        IFEQ      'S'                                          *VOCE DA CALCOLARE
221900980702     C                   EXSR      CALAEL
222000980702E   2C                   ENDIF
222100980702E   1C                   ENDIF
222200980702     C*
222300980702     C* CALCOLA ISTAT SULL'ESPRESSO CONSEGNA LOCALITA' DISAGIATE
222400980702IF  1C     WERR          IFEQ      'N'
222500980702IF  2C     VC(41)        IFEQ      'S'                                          *VOCE DA CALCOLARE
222600980702     C                   EXSR      CALIEL
222700980702E   2C                   ENDIF
222800980702E   1C                   ENDIF
222900980702     C*
223000980702     C* CALCOLA ISTAT SULL'A.D.G. DELL'ESPRESSO CONSEGNA LOCALITA' DISAGIATE
223100980702IF  1C     WERR          IFEQ      'N'
223200980702IF  2C     VC(42)        IFEQ      'S'                                          *VOCE DA CALCOLARE
223300980702     C                   EXSR      CAIAEL
223400980702E   2C                   ENDIF
223500980702E   1C                   ENDIF
223600011031     C*---
223700011031     C* RITIRO ISOLA
223800011031     C*---
223900011031     C* CALCOLA RITIRO ISOLA
224000011031IF  1C     WERR          IFEQ      'N'
224100011031IF  2C     VC(88)        IFEQ      'S'                                          *VOCE DA CALCOLARE
224200011031     C                   EXSR      CALSAM
224300011031E   2C                   ENDIF
224400011031E   1C                   ENDIF
224500011031     C*
224600011031     C* CALCOLA A.D.G. SUL RITIRO ISOLA
224700011031IF  1C     WERR          IFEQ      'N'
224800011031IF  2C     VC(93)        IFEQ      'S'                                          *VOCE DA CALCOLARE
224900011031     C                   EXSR      CALAGR
225000011031E   2C                   ENDIF
225100011031E   1C                   ENDIF
225200011031     C*
225300011031     C* CALCOLA ISTAT SUL RITIRO ISOLA
225400011031IF  1C     WERR          IFEQ      'N'
225500011031IF  2C     VC(94)        IFEQ      'S'                                          *VOCE DA CALCOLARE
225600011031     C                   EXSR      CALISR
225700011031E   2C                   ENDIF
225800011031E   1C                   ENDIF
225900011031     C*
226000011031     C* CALCOLA ISTAT SULL'A.D.G. DEL RITIRO ISOLA
226100011031IF  1C     WERR          IFEQ      'N'
226200011031IF  2C     VC(95)        IFEQ      'S'                                          *VOCE DA CALCOLARE
226300011031     C                   EXSR      CALIAR
226400011031E   2C                   ENDIF
226500011031E   1C                   ENDIF
226600011031     C*
226700011031     C* CALCOLA ESPRESSO SUL RITIRO ISOLA
226800011031IF  1C     WERR          IFEQ      'N'
226900011031IF  2C     VC(89)        IFEQ      'S'                                          *VOCE DA CALCOLARE
227000011031     C                   EXSR      CALESR
227100011031E   2C                   ENDIF
227200011031E   1C                   ENDIF
227300011031     C*
227400011031     C* CALCOLA A.D.G. SULL'ESPRESSO RITIRO ISOLA
227500011031IF  1C     WERR          IFEQ      'N'
227600011031IF  2C     VC(90)        IFEQ      'S'                                          *VOCE DA CALCOLARE
227700011031     C                   EXSR      CALAER
227800011031E   2C                   ENDIF
227900011031E   1C                   ENDIF
228000011031     C*
228100011031     C* CALCOLA ISTAT SULL'ESPRESSO RITIRO ISOLA
228200011031IF  1C     WERR          IFEQ      'N'
228300011031IF  2C     VC(91)        IFEQ      'S'                                          *VOCE DA CALCOLARE
228400011031     C                   EXSR      CALIER
228500011031E   2C                   ENDIF
228600011031E   1C                   ENDIF
228700011031     C*
228800011031     C* CALCOLA ISTAT SULL'A.D.G. DELL'ESPRESSO RITIRO ISOLA
228900011031IF  1C     WERR          IFEQ      'N'
229000011031IF  2C     VC(92)        IFEQ      'S'                                          *VOCE DA CALCOLARE
229100011031     C                   EXSR      CAIAER
229200011031E   2C                   ENDIF
229300011031E   1C                   ENDIF
229400011030     C*---
229500011030     C* CONSEGNA ISOLA
229600011030     C*---
229700011030     C* CALCOLA CONSEGNA PARTICOLARE ISLA
229800011030IF  1C     WERR          IFEQ      'N'
229900011030IF  2C     VC(80)        IFEQ      'S'                                          *VOCE DA CALCOLARE
230000011030     C                   EXSR      CALCIS
230100011030E   2C                   ENDIF
230200011030E   1C                   ENDIF
230300011030     C*
230400011030     C* CALCOLA A.D.G. SULLE CONSEGNE ISOLA
230500011030IF  1C     WERR          IFEQ      'N'
230600011030IF  2C     VC(85)        IFEQ      'S'                                          *VOCE DA CALCOLARE
230700011030     C                   EXSR      CALAGS
230800011030E   2C                   ENDIF
230900011030E   1C                   ENDIF
231000011030     C*
231100011030     C* CALCOLA ISTAT SULLE CONSEGNE ISOLA
231200011030IF  1C     WERR          IFEQ      'N'
231300011030IF  2C     VC(86)        IFEQ      'S'                                          *VOCE DA CALCOLARE
231400011030     C                   EXSR      CALISS
231500011030E   2C                   ENDIF
231600011030E   1C                   ENDIF
231700011030     C*
231800011030     C* CALCOLA ISTAT SULL'A.D.G. DELLE CONSEGNE ISOLA
231900011030IF  1C     WERR          IFEQ      'N'
232000011030IF  2C     VC(87)        IFEQ      'S'                                          *VOCE DA CALCOLARE
232100011030     C                   EXSR      CALIAS
232200011030E   2C                   ENDIF
232300011030E   1C                   ENDIF
232400011030     C*
232500011030     C* CALCOLA ESPRESSO SULLA CONSEGNA ISOLA
232600011030IF  1C     WERR          IFEQ      'N'
232700011030IF  2C     VC(81)        IFEQ      'S'                                          *VOCE DA CALCOLARE
232800011030     C                   EXSR      CALESS
232900011030E   2C                   ENDIF
233000011030E   1C                   ENDIF
233100011030     C*
233200011030     C* CALCOLA A.D.G. SULL'ESPRESSO CONSEGNA ISOLA
233300011030IF  1C     WERR          IFEQ      'N'
233400011030IF  2C     VC(82)        IFEQ      'S'                                          *VOCE DA CALCOLARE
233500011030     C                   EXSR      CALAES
233600011030E   2C                   ENDIF
233700011030E   1C                   ENDIF
233800011030     C*
233900011030     C* CALCOLA ISTAT SULL'ESPRESSO CONSEGNA ISOLA
234000011030IF  1C     WERR          IFEQ      'N'
234100011030IF  2C     VC(83)        IFEQ      'S'                                          *VOCE DA CALCOLARE
234200011030     C                   EXSR      CALIES
234300011030E   2C                   ENDIF
234400011030E   1C                   ENDIF
234500011030     C*
234600011030     C* CALCOLA ISTAT SULL'A.D.G. DELL'ESPRESSO CONSEGNA ISOLA
234700011030IF  1C     WERR          IFEQ      'N'
234800011030IF  2C     VC(84)        IFEQ      'S'                                          *VOCE DA CALCOLARE
234900011030     C                   EXSR      CAIAES
235000011030E   2C                   ENDIF
235100011030E   1C                   ENDIF
235200980702     C*---
235300011030     C* INOLTRO + LOCALITA' DISAGIATE + CONSEGNA ISOLA
235400980702     C*---
235500980702     C* RICALCOLA INOLTRO E VOCI CORRELATE RISPETTO AL FLAG APPICAZIONE TARIFFA INOLTRO
235600980702IF  1C     WERR          IFEQ      'N'
235700011030IF  2C     VC(35)        IFEQ      'S'                                          *L.DISAGIATA
235800011030     C     VC(80)        OREQ      'S'                                          *C.ISOLA
235900980702     C                   EXSR      CA3INL
236000980702E   2C                   ENDIF
236100980702E   1C                   ENDIF
236200980703     C*
236300980703     C* CALCOLA LA BASE PER IL CALCOLO DELLE TARIFFE CON % SUL TRASPORTO
236400980706     C                   EXSR      CALTBT
236500980702     C*---
236600980702     C* CONSEGNE PARTICOLARI
236700980702     C*---
236800060314     C* CALCOLA ASSICURAZIONE ac plus (FRANCO E ASSEGNATO)
236900970306IF  1C     WERR          IFEQ      'N'
237000970306IF  2C     VC(9)         IFEQ      'S'                                          *VOCE DA CALCOLARE
237100970306     C     VC(31)        OREQ      'S'                                          *VOCE DA CALCOLARE
237200060315     C* REIMPOSTA VARIABILI DI LAVORO
237300060315     C                   Z-ADD     ETCASC        IASLIT           23 8
237400060315     C                   MOVEL     'C'           KTPFTC
237500970306     C                   EXSR      CALASC
237600001227     C                   EXSR      CAL048
237700001227     C                   EXSR      CAL065
237800001227     C                   EXSR      CAL060
237900001227     C                   EXSR      CAL077
238000970306E   2C                   ENDIF
238100970306E   1C                   ENDIF
238200060314     C* CALCOLA ASSICURAZIONE ac base (FRANCO E ASSEGNATO)
238300060315     c* no per cartello
238400060315     C                   MOVEL     D00KSC        WA4               4 0
238500060315     c                   if        wa4  <> 8888
238600060314IF  1C     WERR          IFEQ      'N'
238700060314IF  2C     VC(105)       IFEQ      'S'                                          *VOCE DA CALCOLARE
238800060314     C     VC(108)       OREQ      'S'                                          *VOCE DA CALCOLARE
238900060315     C                   Z-ADD     0             IASLIT           23 8
239000060315     C                   MOVEL     'd'           KTPFTC
239100060314     C                   EXSR      CALAcb
239200060314     C                   EXSR      CAL106
239300060314     C                   EXSR      CAL107
239400060314     C                   EXSR      CAL109
239500060314     C                   EXSR      CAL110
239600060314E   2C                   ENDIF
239700060314E   1C                   ENDIF
239800060315E   1C                   ENDIF
239900970306     C*
240000970306     C* CALCOLA ASSICURAZIONE R.C.V. (FRANCO E ASSEGNATO)
240100970306IF  1C     WERR          IFEQ      'N'
240200970306IF  2C     VC(10)        IFEQ      'S'                                          *VOCE DA CALCOLARE
240300970306     C     VC(32)        OREQ      'S'                                          *VOCE DA CALCOLARE
240400970306     C                   EXSR      CALRCV
240500001227     C                   EXSR      CAL049
240600001227     C                   EXSR      CAL066
240700001227     C                   EXSR      CAL061
240800001227     C                   EXSR      CAL078
240900970306E   2C                   ENDIF
241000970306E   1C                   ENDIF
241100970306     C*
241200970306     C* CALCOLA CONSEGNA PARTICOLARE SU APPUNTAMENTO
241300970306IF  1C     WERR          IFEQ      'N'
241400970306IF  2C     VC(11)        IFEQ      'S'                                          *VOCE DA CALCOLARE
241500970306     C                   EXSR      CALCPA
241600001227     C                   EXSR      CAL050
241700001227     C                   EXSR      CAL067
241800970306E   2C                   ENDIF
241900970306E   1C                   ENDIF
242000970306     C*
242100970306     C* CALCOLA CONSEGNA PARTICOLARE AI SUPERMERCATI
242200970306IF  1C     WERR          IFEQ      'N'
242300970306IF  2C     VC(12)        IFEQ      'S'                                          *VOCE DA CALCOLARE
242400970306     C                   EXSR      CALCPS
242500001227     C                   EXSR      CAL051
242600001227     C                   EXSR      CAL068
242700970306E   2C                   ENDIF
242800970306E   1C                   ENDIF
242900970306     C*
243000970306     C* CALCOLA CONSEGNA PARTICOLARE AI PIANI
243100970306IF  1C     WERR          IFEQ      'N'
243200970306IF  2C     VC(13)        IFEQ      'S'                                          *VOCE DA CALCOLARE
243300970306     C                   EXSR      CALCPP
243400001227     C                   EXSR      CAL052
243500001227     C                   EXSR      CAL069
243600970306E   2C                   ENDIF
243700970306E   1C                   ENDIF
243800040826     C*
243900040826     C* CALCOLA CONSEGNA destinatario disagiato
244000040826IF  1C     WERR          IFEQ      'N'
244100040826IF  2C     VC(96)        IFEQ      'S'                                          *VOCE DA CALCOLARE
244200040826     C                   EXSR      CALdes
244300040826     C                   EXSR      CAL097
244400040826     C                   EXSR      CAL098
244500040826E   2C                   ENDIF
244600040826E   1C                   ENDIF
244700970306     C*
244800970306     C* CALCOLA CONSEGNA PARTICOLARE FUORI MISURA
244900970306IF  1C     WERR          IFEQ      'N'
245000970306IF  2C     VC(14)        IFEQ      'S'                                          *VOCE DA CALCOLARE
245100970306     C                   EXSR      CALCPF
245200001227     C                   EXSR      CAL053
245300001227     C                   EXSR      CAL070
245400970306E   2C                   ENDIF
245500970306E   1C                   ENDIF
245600970414     C*
245700970414     C* CALCOLA CONSEGNA PARTICOLARE DDT
245800970414IF  1C     WERR          IFEQ      'N'
245900970414IF  2C     VC(33)        IFEQ      'S'                                          *VOCE DA CALCOLARE
246000970414     C                   EXSR      CALCPD
246100001227     C                   EXSR      CAL062
246200001227     C                   EXSR      CAL079
246300970414E   2C                   ENDIF
246400970414E   1C                   ENDIF
246500980702     C*
246600980702     C* CALCOLA CONSEGNA PARTICOLARE RITIRO
246700980702IF  1C     WERR          IFEQ      'N'
246800980702IF  2C     VC(17)        IFEQ      'S'                                          *VOCE DA CALCOLARE
246900980702     C                   EXSR      CALRIT
247000001227     C                   EXSR      CAL056
247100001227     C                   EXSR      CAL073
247200980702E   2C                   ENDIF
247300980702E   1C                   ENDIF
247400980702     C*
247500980702     C* CALCOLA LA PROVVIGIONE ASSEGNO MITTENTE
247600980702IF  1C     WERR          IFEQ      'N'
247700980702IF  2C     VC(15)        IFEQ      'S'                                          *VOCE DA CALCOLARE
247800980702     C                   EXSR      CALPAM
247900001227     C                   EXSR      CAL054
248000001227     C                   EXSR      CAL071
248100980702E   2C                   ENDIF
248200980702E   1C                   ENDIF
248300980702     C*
248400980702     C* CALCOLA LA PROVVIGIONE ASSEGNO BARTOLINI
248500980702IF  1C     WERR          IFEQ      'N'
248600980702IF  2C     VC(16)        IFEQ      'S'                                          *VOCE DA CALCOLARE
248700980702     C                   EXSR      CALPAB
248800001227     C                   EXSR      CAL055
248900001227     C                   EXSR      CAL072
249000980702E   2C                   ENDIF
249100980702E   1C                   ENDIF
249200980702     C*
249300980702     C* CALCOLA IL DIRITTO FISSO
249400980702IF  1C     WERR          IFEQ      'N'
249500980702IF  2C     VC(2)         IFEQ      'S'                                          *VOCE DA CALCOLARE
249600980702     C                   EXSR      CALDIF
249700980702E   2C                   ENDIF
249800980702E   1C                   ENDIF
249900980702     C*
250000980702     C* CALCOLA A.D.G. SUL DIRITTO FISSO
250100980702IF  1C     WERR          IFEQ      'N'
250200980702IF  2C     VC(22)        IFEQ      'S'                                          *VOCE DA CALCOLARE
250300980702     C                   EXSR      CALAGD
250400980702E   2C                   ENDIF
250500980702E   1C                   ENDIF
250600980702     C*
250700980702     C* CALCOLA ISTAT SUL DIRITTO FISSO
250800980702IF  1C     WERR          IFEQ      'N'
250900980702IF  2C     VC(25)        IFEQ      'S'                                          *VOCE DA CALCOLARE
251000980702     C                   EXSR      CALISD
251100980702E   2C                   ENDIF
251200980702E   1C                   ENDIF
251300980702     C*
251400980702     C* CALCOLA ISTAT SULL'A.D.G. DEL DIRITTO FISSO
251500980702IF  1C     WERR          IFEQ      'N'
251600980702IF  2C     VC(28)        IFEQ      'S'                                          *VOCE DA CALCOLARE
251700980702     C                   EXSR      CALIAD
251800980702E   2C                   ENDIF
251900980702E   1C                   ENDIF
252000990819     C*
252100990819     C* CALCOLA T.C.P.
252200990819IF  1C     WERR          IFEQ      'N'
252300990819IF  2C     VC(5)         IFEQ      'S'                                          *VOCE DA CALCOLARE
252400990819     C                   EXSR      CALTCP
252500001222     C                   EXSR      CAL046
252600001227     C                   EXSR      CAL063
252700990819E   2C                   ENDIF
252800990819E   1C                   ENDIF
252900040826     C*
253000040826     C* CALCOLA bancali
253100040826IF  1C     WERR          IFEQ      'N'
253200040826IF  2C     VC(99)        IFEQ      'S'                                          *VOCE DA CALCOLARE
253300040826     C                   EXSR      CALban
253400040826     C                   EXSR      CAL100
253500040826     C                   EXSR      CAL101
253600040826E   2C                   ENDIF
253700040826E   1C                   ENDIF
253800040826     C*
253900040826     C* diritto di pesatura
254000040826IF  1C     WERR          IFEQ      'N'
254100040826IF  2C     §tatap        IFEQ      'S'                                          *VOCE DA CALCOLARE
254200040826     C                   EXSR      CALdps
254300040826     C                   EXSR      CAL103
254400040826     C                   EXSR      CAL104
254500040826E   2C                   ENDIF
254600040826E   1C                   ENDIF
254700970306     C*
254800970306     C                   ENDSR
254900980707     C*------------------------------------------------------------------------*
255000980707     C* CALPEI - CALCOLA PORTO E INOLTRO
255100980707     C*------------------------------------------------------------------------*
255200980707     C     CALPEI        BEGSR
255300980707     C*
255400011031     C                   Z-ADD     *ZEROS        TASPOR           21 8
255500011031     C                   Z-ADD     *ZEROS        TASINL           21 8
255600980707     C*****
255700980707     C** CALCOLO    P O R T O     ED    I N O L T R O
255800980707     C*****
255900980707     C                   DO        11            A
256000980707     C     A             OCCUR     TARDS
256100980707     C     TSCG          IFGE      PESTAS
256200980707     C                   GOTO      ENDTAS
256300980707     C                   END
256400980707     C                   END
256500980707     C** NON TROVATO SCAGLIONE ESATTO
256600980707     C     TSCG          IFLT      PESTAS
256700980707     C                   MOVEL     MSG(25)       $MSG
256800980707     C                   EXSR      IMPERR
256900980707     C                   GOTO      FINE
257000980707     C                   END
257100980707     C     ENDTAS        TAG
257200980707     C** MI SALVO LA POSIZIONE DELLO SCAGLIO TROVATO CHE MI
257300980707     C*  SERVE PER IL CALCOLO TARIFFA CON GARANTITO MAX SCAGLIONE PREC
257400980707     C     *LIKE         DEFINE    A             WA
257500980707     C                   Z-ADD     A             WA
257600980707     C**
257700980707     C* CALCOLO PER QUALE VALORE MOLTIPLICARE LA TARIFFA
257800980707     C                   Z-ADD     PESTAS        WPTAS3
257900980707     C                   EXSR      CALQLI
258000980707     C**
258100980707     C* MODO APPLICAZIONE TARIFFA
258200980707     C* CALCOLO NORMALE ANCHE PER SCALARE AL DI SOTTO
258300980707     C*  DELL'APPLICAZIONE TARIFFA FINITA E PER MAX SCAGLIONE PREC
258400980707     C     §TAF02        IFNE      'S'
258500980707     C     §TAF02        OREQ      'S'
258600980707     C     WATA          ANDNE     ' '
258700980707     C** MOLTIPLICAZIONE TARIFFA
258800980707     C                   EXSR      MULTAR
258900980707     C* APPLICAZIONE MINIMO E MASSIMO
259000980707     C                   EXSR      MINMAX
259100980707     C                   Z-ADD     WPOR          TASPOR
259200980707     C                   Z-ADD     WINL          TASINL
259300980707     C                   ENDIF
259400980707     C**
259500980707     C** S C A L A R E
259600980707     C     §TAF02        IFEQ      'S'
259700980707     C                   EXSR      SCALAR
259800980707     C                   ENDIF
259900980707     C**
260000980707     C** M A X   S C A G L I O N E   P R E C E D E N TE
260100980707     C     §TAF02        IFEQ      'G'
260200980707     C                   EXSR      MAXSCA
260300980707     C                   ENDIF
260400980707     C* PORTO
260500980707IF  1C     VC(1)         IFEQ      'S'                                          *VOCE DA CALCOLARE
260600980707IF  2C     TASPOR        IFGT      *ZEROS
260700980707     C                   Z-ADD     1             I
260800980707     C                   Z-ADD     TASPOR        VIMP(I)
260900980707E   2C                   ENDIF
261000980707E   1C                   ENDIF
261100980707     C* INOLTRO
261200980707IF  1C     VC(6)         IFEQ      'S'                                          *VOCE DA CALCOLARE
261300980707IF  2C     TASINL        IFGT      *ZEROS
261400980707     C                   Z-ADD     6             I
261500980707     C                   Z-ADD     TASINL        VIMP(I)
261600980707E   2C                   ENDIF
261700980707E   1C                   ENDIF
261800980707     C**
261900980707     C     FINE          TAG
262000980707     C                   ENDSR
262100980707     C*****************************************************************
262200980707     C** VALORE COL QUALE  MOLTIPLICARE LA TARIFFA
262300980707     C*****************************************************************
262400980707     C     CALQLI        BEGSR
262500980707     C                   Z-ADD     0             PESQLI           11 3
262600980707     C** SE LO SCAGLIONE SCELTO PER TASSARE E' INFERIORE AL MINIMO
262700980707     C** TASSABILE DA TARIFFA FORZO PESO TASSABILE = MINTAS
262800980707     C**
262900980707     C* WATA MI IDENTIFICA SE L'APPLICAZIONE TARIFFA FINITA E' DOVUTA
263000980707     C*  ALLO SCAGLIONE CONFRONTATO CON MINTAR (R)
263100980707     C*  O SE E' DOVUTA DAL PESO TASSABILE CONFRONTATO COL MINIMO
263200980707     C*  TASSABILE DI QUELLA TARIFFA (S) - differenza che ora non serve
263300980707     C*  + ma dal momento che c'e' la lascio
263400980707     C                   CLEAR                   WATA
263500980707    1C                   SELECT
263600980707     C     TSCG          WHENLE    MINTAR
263700980707     C                   Z-ADD     1             PESQLI
263800980707     C                   MOVEL     'R'           WATA              1
263900980707     C**
264000980707     C** SE PESO TASSABILE E' ANCORA INFERIORE
264100980707     C*  AL MINIMO TASSABILE DA TARIFFA IMPOSTO PESO TASS = 1
264200980707     C*  NON LO CONTROLLO PER LA TARIFFA A SCALARE PERCHE' ABBIAMO
264300980707     C*  SEMPRE INSERITO UNO SCAGLIONE=A MINIMO TASSABILE E
264400980707     C*  QUINDI E' GIA' CONTROLLATO NEL WHLE PRECEDENTE
264500980707     C     WPTAS3        WHENLE    MINTAS
264600980707     C     §TAF02        ANDNE     'S'
264700980707     C                   Z-ADD     1             PESQLI
264800980707     C                   MOVEL     'S'           WATA
264900980707   X1C                   OTHER
265000980707     C** CALCOLO : PESO IN Q.LI PER TARIFFA A PESO
265100980707     C** CALCOLO   VALORE/100   PER TARIFFA A VALORE (ESSENDO UNA %)
265200980707     C** PER LE ALTRE TARIFFE PESO TASSABILE INVARIATO
265300980707    2C     UNOCTR        IFEQ      0
265400980707     C     UNOCTR        OREQ      4
265500980707     C     WPTAS3        DIV       100           PESQLI
265600980707   X2C                   ELSE
265700980707     C                   Z-ADD     WPTAS3        PESQLI
265800980707    2C                   END
265900980707    1C                   ENDSL
266000980707     C                   ENDSR
266100980707     C*****************************************************************
266200980707     C* MOLTIPLICATO TARIFFA PER VALORE TROVATO (PESO TASSABILE)
266300980707     C*****************************************************************
266400980707     C     MULTAR        BEGSR
266500980707     C     *LIKE         DEFINE    TASPOR        WPOR
266600980707     C     *LIKE         DEFINE    TASINL        WINL
266700980707     C                   CLEAR                   WPOR
266800980707     C                   CLEAR                   WINL
266900980707     C**
267000980707     C     TITR          MULT(H)   PESQLI        WPOR                           ** PORTO
267100980707     C     TINO          MULT(H)   PESQLI        WINL                           ** INOLTRO
267200980707     C                   ENDSR
267300980707     C*****************************************************************
267400980707     C* CALCOLO TARIFFA A SCALARE
267500980707     C*****************************************************************
267600980707     C     SCALAR        BEGSR
267700980707     C**
267800980707     C     *LIKE         DEFINE    PESTAS        WPTAS1
267900980707     C     *LIKE         DEFINE    PESTAS        WDIFSC
268000980707     C     *LIKE         DEFINE    PESTAS        WPTAS3
268100980707     C     *LIKE         DEFINE    PESQLI        WPESQ
268200980707     C     *LIKE         DEFINE    PESQLI        WPESSV
268300980707     C     *LIKE         DEFINE    TITR          WTITR
268400980707     C     *LIKE         DEFINE    TITR          WSTITR
268500980707     C     *LIKE         DEFINE    TINO          WTINO
268600980707     C     *LIKE         DEFINE    TINO          WSTINO
268700980707     C     *LIKE         DEFINE    TSCG          WPRESC
268800980707     C**
268900980707     C                   CLEAR                   TASPOR
269000980707     C                   CLEAR                   TASINL
269100980707     C                   CLEAR                   WTROVA
269200980707     C                   CLEAR                   WPTAS3
269300980707     C                   CLEAR                   WDIFSC
269400980707     C                   CLEAR                   WPRESC
269500980707     C                   CLEAR                   WPESSV
269600980707     C                   CLEAR                   WPESQ
269700980707     C                   CLEAR                   WTITR
269800980707     C                   CLEAR                   WSTITR
269900980707     C                   CLEAR                   WTINO
270000980707     C                   CLEAR                   WSTINO
270100980707     C                   Z-ADD     PESTAS        WPTAS1
270200980707     C                   Z-ADD     1             A
270300980707     C**
270400980805    1C     A             DOWLE     11
270500980707     C     WTROVA        ANDEQ     ' '
270600980707     C     TSCG          ANDGT     0
270700980707     C     A             OCCUR     TARDS
270800980707     C**
270900980707     C* FACCIO LA DIFFERENZA TRA LO SCAGLIONE LETTO ED IL PRECEDENTE
271000980707     C     TSCG          SUB       WPRESC        WDIFSC
271100980707     C**
271200980707     C* SOTRAGGO DAL PESO TASSABILE RIMASTO LA PARTE DA TASSARE ORA
271300980707     C* IN WPTAS1 HO SEMPRE LA PARTE DI PESO TASSABILE ANCORA DA TASSAR
271400980707     C                   SUB       WDIFSC        WPTAS1
271500980707     C**
271600980707     C** SE IL VALORE E' DIVENTATO < DI 0 SIGNIFICA CHE HO TERMINATO LA
271700980707     C**  TASSAZIONE PER CUI USO SOLO LA PARTE RIMASTA DI WPTAS1
271800980707    2C     WPTAS1        IFLE      0
271900980707     C     WDIFSC        ADD       WPTAS1        WPTAS3
272000980707     C* PESO TASSABILE GLOBALE DELLA SPEDIZIONE
272100980707     C                   MOVEL     'S'           WTROVA            1
272200980707     C                   ELSE
272300980707     C**
272400980707     C* PARTE DI PESO DA TASSARE CON QUESTO SCAGLIONE
272500980707     C                   Z-ADD     WDIFSC        WPTAS3
272600980707     C* SALVO LO SCAGLIONE PRECEDENTE
272700980707     C                   Z-ADD     TSCG          WPRESC
272800980707    2C                   ENDIF
272900980707     C**
273000980707     C* IMPOSTO IL VALORE DA MOLTIPLICARE PER LA TARIFFA
273100980707     C                   EXSR      CALQLI
273200980707     C**
273300980707     C* SE LO SCAGLIONE E' APPLICAZIONE TARIFFA FINITA DEVO CONSIDERARE
273400980707     C*  SOLO L'ULTIMO SCAGLIONE COSI' E NON I PRECEDENTI
273500980707    2C     WATA          IFNE      ' '
273600980707     C     WTROVA        ANDEQ     ' '
273700980707     C* MI SALVO IL VALORE MA PER ORA NON LO MOLTIPLICO
273800980707     C                   Z-ADD     PESQLI        WPESQ
273900980707     C                   Z-ADD     TITR          WTITR
274000980707     C                   Z-ADD     TINO          WTINO
274100980707   X2C                   ELSE
274200980707     C**
274300980707     C* SE LO SCAGLIONE TROVATO E' L'ULTIMO MA COMUNQUE TARIFFA FINITA
274400980707     C* DEVO MOLTIPLICARE SOLO QUESTO --> AZZERO UN EVENTUALE PRECEDENT
274500980707     C*  CHE AVEVA LA TASSAZIONE TARIFFA FINITA
274600980707    3C     WTROVA        IFEQ      'S'
274700980707     C     WATA          ANDNE     ' '
274800980707     C                   CLEAR                   WPESQ
274900980707     C                   CLEAR                   WTITR
275000980707     C                   CLEAR                   WTINO
275100980707    3C                   ENDIF
275200980707     C*
275300980707     C* PRIMA DI MOLTIPLICARE LA TARIFFA TROVATA VEDO SE CE NE E'
275400980707     C*  UNA PRECEDENTE TARIFFA FINITA DA CONSIDERARE
275500980707    3C     WPESQ         IFGT      0
275600980707     C* SALVO PESO DA MOLTIPLICARE E TARIFFE DELLO SCAGLIONE
275700980707     C*  APPENA LETTO
275800980707     C                   Z-ADD     PESQLI        WPESSV
275900980707     C                   Z-ADD     TITR          WSTITR
276000980707     C                   Z-ADD     TINO          WSTINO
276100980707     C* IMPOSTO PESO DA MOLTIPLICARE E TARIFFE DELLO SCAGLIONE
276200980707     C*  PRECEDENTE PER IL CALCOLO
276300980707     C                   Z-ADD     WPESQ         PESQLI
276400980707     C                   Z-ADD     WTITR         TITR
276500980707     C                   Z-ADD     WTINO         TINO
276600980707     C                   EXSR      MULTAR
276700980707     C                   ADD       WPOR          TASPOR
276800980707     C                   ADD       WINL          TASINL
276900980707     C                   CLEAR                   WPESQ
277000980707     C                   CLEAR                   WTITR
277100980707     C                   CLEAR                   WTINO
277200980707     C* REIMPOSTO I DATI DELLO SCAGLIONE APPENA LETTO
277300980707     C                   Z-ADD     WPESSV        PESQLI
277400980707     C                   Z-ADD     WSTITR        TITR
277500980707     C                   Z-ADD     WSTINO        TINO
277600980707    3C                   ENDIF
277700980707     C**
277800980707     C* MOLTIPLICAZIONE TARIFFA
277900980707     C                   EXSR      MULTAR
278000980707     C                   ADD       WPOR          TASPOR
278100980707     C                   ADD       WINL          TASINL
278200980707    2C                   ENDIF
278300980707     C                   ADD       1             A
278400980707     C**
278500980707    1C                   ENDDO
278600980707     C**
278700980707     C* APPLICO MINIMO E MASSIMO
278800980707     C                   Z-ADD     TASPOR        WPOR
278900980707     C                   Z-ADD     TASINL        WINL
279000980707     C                   EXSR      MINMAX
279100980707     C                   Z-ADD     WPOR          TASPOR
279200980707     C                   Z-ADD     WINL          TASINL
279300980707     C                   ENDSR
279400980707     C*****************************************************************
279500980707     C* CALCOLO MAX SCAGLIONE PRECEDENTE
279600980707     C*****************************************************************
279700980707     C     MAXSCA        BEGSR
279800980707     C* SOLO SE LO SCAGLIONE APPLICATO NON ERA IL PRIMO
279900980707     C     WA            IFGT      1
280000980707     C* PER CONFRONTARLO CON IL VALORE NORMALE CALCOLATO
280100980707     C* IL CONFRONTO E' DATO DALLA SOMMA TRA PORT E INOLTRO
280200980707     C                   SUB       1             WA
280300980707     C     WA            OCCUR     TARDS
280400980707     C* CALCOLO PESO DA MOLTIPLICARE
280500980707     C                   Z-ADD     TSCG          WPTAS3
280600980707     C                   EXSR      CALQLI
280700980707     C** MOLTIPLICO LA TARIFFA
280800980707     C                   EXSR      MULTAR
280900980707     C** MINIMO E MASSIMO
281000980707     C                   EXSR      MINMAX
281100980707     C**
281200011018     C     TASPOR        ADD       TASINL        T0100            18 5
281300011018     C     WPOR          ADD       WINL          W0100            18 5
281400980707     C** PRENDO DELLE 2 LA TARIFFA PIU' ALTA
281500980707     C     W0100         IFGT      T0100
281600980707     C                   Z-ADD     WPOR          TASPOR
281700980707     C                   Z-ADD     WINL          TASINL
281800980707     C                   ENDIF
281900980707     C                   ENDIF
282000980707     C**
282100980707     C                   ENDSR
282200980707     C*****************************************************************
282300980707     C** MINIMO E MASSIMO DA APPLICARE -SE APPLICATO TUTTO NEL PORTO
282400980707     C*****************************************************************
282500980707     C     MINMAX        BEGSR
282600980707     C** NON ESISTE MINIMO E MASSIMO
282700980707     C     TMIN          IFEQ      0
282800980707     C     TMAX          ANDEQ     0
282900980707     C                   GOTO      DOPOMM
283000980707     C                   END
283100980707     C** MINIMO
283200011018     C     WPOR          ADD       WINL          WMAX             18 5
283300980707     C     TMIN          IFGT      WMAX
283400980707     C                   Z-ADD     TMIN          WPOR
283500980707     C                   Z-ADD     0             WINL
283600980707     C                   Z-ADD     TMIN          WMAX
283700980707     C                   END
283800980707     C** MASSIMO
283900980707     C     TMAX          IFGT      0
284000980707     C     WMAX          IFGT      TMAX
284100980707     C                   Z-ADD     TMAX          WPOR
284200980707     C                   Z-ADD     0             WINL
284300980707     C                   END
284400980707     C                   END
284500980707     C     DOPOMM        ENDSR
284600980706     C*------------------------------------------------------------------------*
284700980706     C* CALAGP - CALCOLA L'A.D.G. SUL PORTO
284800980706     C*------------------------------------------------------------------------*
284900980706     C     CALAGP        BEGSR
285000980706     C*
285100980706     C* REIMPOSTA LE VARIABILI DI LAVORO
285200011031     C                   Z-ADD     *ZEROS        TASADG           21 8
285300980706     C*
285400980706     C* CALCOLA L'A.D.G.
285500011031     C                   Z-ADD     TASPOR        WIMP             23 8
285600980706     C                   EXSR      ADGCAL
285700980706     C*
285800980706IF  1C     TASADG        IFGT      *ZEROS
285900980706     C                   Z-ADD     3             I
286000980706     C                   Z-ADD     TASADG        VIMP(I)
286100980706E   1C                   ENDIF
286200980706     C*
286300980706     C                   ENDSR
286400980706     C*------------------------------------------------------------------------*
286500980706     C* CALISP - CALCOLA L'ISTAT SUL PORTO
286600980706     C*------------------------------------------------------------------------*
286700980706     C     CALISP        BEGSR
286800980706     C*
286900980706     C* REIMPOSTA LE VARIABILI DI LAVORO
287000011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
287100980706     C*
287200980706     C* CALCOLA L'ISTAT
287300011031     C                   Z-ADD     TASPOR        WIMP             23 8
287400980706     C                   EXSR      IACCAL
287500980706     C*
287600980706IF  1C     TASIAC        IFGT      *ZEROS
287700980706     C                   Z-ADD     4             I
287800980706     C                   Z-ADD     TASIAC        VIMP(I)
287900980706E   1C                   ENDIF
288000980706     C*
288100980706     C                   ENDSR
288200980706     C*------------------------------------------------------------------------*
288300980706     C* CALIAP - CALCOLA L'ISTAT SULL'ADG DEL PORTO
288400980706     C*------------------------------------------------------------------------*
288500980706     C     CALIAP        BEGSR
288600980706     C*
288700980706     C* REIMPOSTA LE VARIABILI DI LAVORO
288800011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
288900980706     C*
289000980706     C* CALCOLA L'ISTAT
289100011031     C                   Z-ADD     VIMP(3)       WIMP             23 8
289200980706     C                   EXSR      IACCAL
289300980706     C*
289400980706IF  1C     TASIAC        IFGT      *ZEROS
289500980706     C                   Z-ADD     27            I
289600980706     C                   Z-ADD     TASIAC        VIMP(I)
289700980706E   1C                   ENDIF
289800980706     C*
289900980706     C                   ENDSR
290000970306     C*------------------------------------------------------------------------*
290100970717     C* CALESP - CALCOLA L'ESPRESSO PORTO
290200970306     C*------------------------------------------------------------------------*
290300970306     C     CALESP        BEGSR
290400970306     C*
290500970306     C* REIMPOSTA LE VARIABILI DI LAVORO
290600011031     C                   Z-ADD     *ZEROS        TASESP           21 8
290700970306     C*
290800970306     C* CALCOLA L'ESPRESSO
290900011031     C                   Z-ADD     TASPOR        IMPON            23 8
291000970306     C                   EXSR      CALTS
291100970306     C*
291200970314IF  1C     TASESP        IFGT      *ZEROS
291300970306     C                   Z-ADD     7             I
291400970306     C                   Z-ADD     TASESP        VIMP(I)
291500970314E   1C                   ENDIF
291600970306     C*
291700970306     C                   ENDSR
291800980706     C*------------------------------------------------------------------------*
291900980706     C* CALAEP - CALCOLA L'A.D.G. SULL'ESPRESSO PORTO
292000980706     C*------------------------------------------------------------------------*
292100980706     C     CALAEP        BEGSR
292200980706     C*
292300980706     C* REIMPOSTA LE VARIABILI DI LAVORO
292400011031     C                   Z-ADD     *ZEROS        TASADG           21 8
292500980706     C*
292600980706     C* CALCOLA L'A.D.G.
292700011031     C                   Z-ADD     VIMP(7)       WIMP             23 8
292800980706     C                   EXSR      ADGCAL
292900980706     C*
293000980706IF  1C     TASADG        IFGT      *ZEROS
293100980706     C                   Z-ADD     23            I
293200980706     C                   Z-ADD     TASADG        VIMP(I)
293300980706E   1C                   ENDIF
293400980706     C*
293500980706     C                   ENDSR
293600980706     C*------------------------------------------------------------------------*
293700980706     C* CALIEP - CALCOLA L'ISTAT SULL'ESPRESSO PORTO
293800980706     C*------------------------------------------------------------------------*
293900980706     C     CALIEP        BEGSR
294000980706     C*
294100980706     C* REIMPOSTA LE VARIABILI DI LAVORO
294200011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
294300980706     C*
294400980706     C* CALCOLA L'ISTAT
294500011031     C                   Z-ADD     VIMP(7)       WIMP             23 8
294600980706     C                   EXSR      IACCAL
294700980706     C*
294800980706IF  1C     TASIAC        IFGT      *ZEROS
294900980706     C                   Z-ADD     26            I
295000980706     C                   Z-ADD     TASIAC        VIMP(I)
295100980706E   1C                   ENDIF
295200980706     C*
295300980706     C                   ENDSR
295400980706     C*------------------------------------------------------------------------*
295500980706     C* CAIAEP - CALCOLA L'ISTAT SULL'ADG DELL'ESPRESSO PORTO
295600980706     C*------------------------------------------------------------------------*
295700980706     C     CAIAEP        BEGSR
295800980706     C*
295900980706     C* REIMPOSTA LE VARIABILI DI LAVORO
296000011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
296100980706     C*
296200980706     C* CALCOLA L'ISTAT
296300011031     C                   Z-ADD     VIMP(23)      WIMP             23 8
296400980706     C                   EXSR      IACCAL
296500980706     C*
296600980706IF  1C     TASIAC        IFGT      *ZEROS
296700980706     C                   Z-ADD     30            I
296800980706     C                   Z-ADD     TASIAC        VIMP(I)
296900980706E   1C                   ENDIF
297000980706     C*
297100980706     C                   ENDSR
297200980706     C*------------------------------------------------------------------------*
297300980706     C* CALAGI - CALCOLA L'A.D.G. SULL'INOLTRO
297400980706     C*------------------------------------------------------------------------*
297500980706     C     CALAGI        BEGSR
297600980706     C*
297700980706     C* REIMPOSTA LE VARIABILI DI LAVORO
297800011031     C                   Z-ADD     *ZEROS        TASADG           21 8
297900980706     C*
298000980706     C* CALCOLA L'A.D.G.
298100011031     C                   Z-ADD     TASINL        WIMP             23 8
298200980706     C                   EXSR      ADGCAL
298300980706     C*
298400980706IF  1C     TASADG        IFGT      *ZEROS
298500980706     C                   Z-ADD     21            I
298600980706     C                   Z-ADD     TASADG        VIMP(I)
298700980706E   1C                   ENDIF
298800980706     C*
298900980706     C                   ENDSR
299000980706     C*------------------------------------------------------------------------*
299100980706     C* CALISI - CALCOLA L'ISTAT SULL'INOLTRO
299200980706     C*------------------------------------------------------------------------*
299300980706     C     CALISI        BEGSR
299400980706     C*
299500980706     C* REIMPOSTA LE VARIABILI DI LAVORO
299600011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
299700980706     C*
299800980706     C* CALCOLA L'ISTAT
299900011031     C                   Z-ADD     TASINL        WIMP             23 8
300000980706     C                   EXSR      IACCAL
300100980706     C*
300200980706IF  1C     TASIAC        IFGT      *ZEROS
300300980706     C                   Z-ADD     24            I
300400980706     C                   Z-ADD     TASIAC        VIMP(I)
300500980706E   1C                   ENDIF
300600980706     C*
300700980706     C                   ENDSR
300800980706     C*------------------------------------------------------------------------*
300900980706     C* CALIAI - CALCOLA L'ISTAT SULL'ADG DELL'INOLTRO
301000980706     C*------------------------------------------------------------------------*
301100980706     C     CALIAI        BEGSR
301200980706     C*
301300980706     C* REIMPOSTA LE VARIABILI DI LAVORO
301400011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
301500980706     C*
301600980706     C* CALCOLA L'ISTAT
301700011031     C                   Z-ADD     VIMP(21)      WIMP             23 8
301800980706     C                   EXSR      IACCAL
301900980706     C*
302000980706IF  1C     TASIAC        IFGT      *ZEROS
302100980706     C                   Z-ADD     29            I
302200980706     C                   Z-ADD     TASIAC        VIMP(I)
302300980706E   1C                   ENDIF
302400980706     C*
302500980706     C                   ENDSR
302600970717     C*------------------------------------------------------------------------*
302700970717     C* CALESI - CALCOLA L'ESPRESSO INOLTRO
302800970717     C*------------------------------------------------------------------------*
302900970717     C     CALESI        BEGSR
303000970717     C*
303100970717     C* REIMPOSTA LE VARIABILI DI LAVORO
303200011031     C                   Z-ADD     *ZEROS        TASESP           21 8
303300970717     C*
303400970717     C* CALCOLA L'ESPRESSO
303500011031     C                   Z-ADD     TASINL        IMPON            23 8
303600970717     C                   EXSR      CALTS
303700970717     C*
303800970717IF  1C     TASESP        IFGT      *ZEROS
303900970717     C                   Z-ADD     36            I
304000970717     C                   Z-ADD     TASESP        VIMP(I)
304100970717E   1C                   ENDIF
304200970717     C*
304300970717     C                   ENDSR
304400980706     C*------------------------------------------------------------------------*
304500980706     C* CALAEI - CALCOLA L'A.D.G. SULL'ESPRESSO INOLTRO
304600980706     C*------------------------------------------------------------------------*
304700980706     C     CALAEI        BEGSR
304800980706     C*
304900980706     C* REIMPOSTA LE VARIABILI DI LAVORO
305000011031     C                   Z-ADD     *ZEROS        TASADG           21 8
305100980706     C*
305200980706     C* CALCOLA L'A.D.G.
305300011031     C                   Z-ADD     VIMP(36)      WIMP             23 8
305400980706     C                   EXSR      ADGCAL
305500980706     C*
305600980706IF  1C     TASADG        IFGT      *ZEROS
305700980706     C                   Z-ADD     37            I
305800980706     C                   Z-ADD     TASADG        VIMP(I)
305900980706E   1C                   ENDIF
306000980706     C*
306100980706     C                   ENDSR
306200980706     C*------------------------------------------------------------------------*
306300980706     C* CALIEI - CALCOLA L'ISTAT SULL'ESPRESSO INOLTRO
306400980706     C*------------------------------------------------------------------------*
306500980706     C     CALIEI        BEGSR
306600980706     C*
306700980706     C* REIMPOSTA LE VARIABILI DI LAVORO
306800011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
306900980706     C*
307000980706     C* CALCOLA L'ISTAT
307100011031     C                   Z-ADD     VIMP(36)      WIMP             23 8
307200980706     C                   EXSR      IACCAL
307300980706     C*
307400980706IF  1C     TASIAC        IFGT      *ZEROS
307500980706     C                   Z-ADD     38            I
307600980706     C                   Z-ADD     TASIAC        VIMP(I)
307700980706E   1C                   ENDIF
307800980706     C*
307900980706     C                   ENDSR
308000980706     C*------------------------------------------------------------------------*
308100980706     C* CAIAEI - CALCOLA L'ISTAT SULL'ADG DELL'ESPRESSO INOLTRO
308200980706     C*------------------------------------------------------------------------*
308300980706     C     CAIAEI        BEGSR
308400980706     C*
308500980706     C* REIMPOSTA LE VARIABILI DI LAVORO
308600011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
308700980706     C*
308800980706     C* CALCOLA L'ISTAT
308900011031     C                   Z-ADD     VIMP(37)      WIMP             23 8
309000980706     C                   EXSR      IACCAL
309100980706     C*
309200980706IF  1C     TASIAC        IFGT      *ZEROS
309300980706     C                   Z-ADD     39            I
309400980706     C                   Z-ADD     TASIAC        VIMP(I)
309500980706E   1C                   ENDIF
309600980706     C*
309700980706     C                   ENDSR
309800980706     C*------------------------------------------------------------------------*
309900980706     C* CALCPL - CALCOLA CONSEGNA PARTICOLARE LOCALITA' DISAGIATE
310000980706     C*------------------------------------------------------------------------*
310100980706     C     CALCPL        BEGSR
310200980706     C*
310300980706     C* REIMPOSTA LE VARIABILI DI LAVORO
310400980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
310500011031     C                   Z-ADD     *ZEROS        TASCPL           21 8
310600980706     C*
310700980706     C* CALCOLA CONSEGNA PARTICOLARE
310800980706     C                   MOVEL     'K'           TASTC             1            *CODICE CONSEGNA
310900011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
311000980706     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
311100980706     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
311200980706     C                   EXSR      CALCP
311300980706     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
311400980706     C                   Z-ADD     TASCP         TASCPL
311500980706     C*
311600980706IF  1C     TASCPL        IFGT      *ZEROS
311700980706     C                   Z-ADD     35            I
311800980706     C                   Z-ADD     TASCPL        VIMP(I)
311900980706E   1C                   ENDIF
312000980706     C*
312100980706     C* MEMORIZZA IL FLAG "APPLICAZIONE TARIFFA INOLTRO" DELLO SCAGLIONE DEL
312200980706     C* CODICE TASSAZIONE DEL DETTAGLIO TARIFFARIO
312300980706     C                   MOVEL     DEPAIN        CPLAIN
312400980706     C*
312500980706     C                   ENDSR
312600980706     C*------------------------------------------------------------------------*
312700980706     C* CALAGL - CALCOLA L'A.D.G. SULLE CONSEGNE LOCALITA' DISAGIATE
312800980706     C*------------------------------------------------------------------------*
312900980706     C     CALAGL        BEGSR
313000980706     C*
313100980706     C* REIMPOSTA LE VARIABILI DI LAVORO
313200011031     C                   Z-ADD     *ZEROS        TASADG           21 8
313300980706     C*
313400980706     C* CALCOLA L'A.D.G.
313500011031     C                   Z-ADD     TASCPL        WIMP             23 8
313600980706     C                   EXSR      ADGCAL
313700980706     C*
313800980706IF  1C     TASADG        IFGT      *ZEROS
313900980706     C                   Z-ADD     43            I
314000980706     C                   Z-ADD     TASADG        VIMP(I)
314100980706E   1C                   ENDIF
314200980706     C*
314300980706     C                   ENDSR
314400980706     C*------------------------------------------------------------------------*
314500980706     C* CALISL - CALCOLA L'ISTAT SULLE CONSEGNE LOCALITA' DISAGIATE
314600980706     C*------------------------------------------------------------------------*
314700980706     C     CALISL        BEGSR
314800980706     C*
314900980706     C* REIMPOSTA LE VARIABILI DI LAVORO
315000011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
315100980706     C*
315200980706     C* CALCOLA L'ISTAT
315300011031     C                   Z-ADD     TASCPL        WIMP             23 8
315400980706     C                   EXSR      IACCAL
315500980706     C*
315600980706IF  1C     TASIAC        IFGT      *ZEROS
315700980706     C                   Z-ADD     44            I
315800980706     C                   Z-ADD     TASIAC        VIMP(I)
315900980706E   1C                   ENDIF
316000980706     C*
316100980706     C                   ENDSR
316200980706     C*------------------------------------------------------------------------*
316300980706     C* CALIAL - CALCOLA L'ISTAT SULL'ADG DELLE CONEGNE LOCALITA' DISAGIATE
316400980706     C*------------------------------------------------------------------------*
316500980706     C     CALIAL        BEGSR
316600980706     C*
316700980706     C* REIMPOSTA LE VARIABILI DI LAVORO
316800011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
316900980706     C*
317000980706     C* CALCOLA L'ISTAT
317100011031     C                   Z-ADD     VIMP(43)      WIMP             23 8
317200980706     C                   EXSR      IACCAL
317300980706     C*
317400980706IF  1C     TASIAC        IFGT      *ZEROS
317500980706     C                   Z-ADD     45            I
317600980706     C                   Z-ADD     TASIAC        VIMP(I)
317700980706E   1C                   ENDIF
317800980706     C*
317900980706     C                   ENDSR
318000980706     C*------------------------------------------------------------------------*
318100980706     C* CALESL - CALCOLA L'ESPRESSO SULLA CONSEGNA LOCALITA' DISAGIATE
318200980706     C*------------------------------------------------------------------------*
318300980706     C     CALESL        BEGSR
318400980706     C*
318500980706     C* REIMPOSTA LE VARIABILI DI LAVORO
318600011031     C                   Z-ADD     *ZEROS        TASESP           21 8
318700980706     C*
318800980706     C* CALCOLA L'ESPRESSO
318900011031     C                   Z-ADD     VIMP(35)      IMPON            23 8
319000980706     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
319100980706     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
319200980706     C                   EXSR      CALTS
319300980706     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
319400980706     C*
319500980706IF  1C     TASESP        IFGT      *ZEROS
319600980706     C                   Z-ADD     39            I
319700980706     C                   Z-ADD     TASESP        VIMP(I)
319800980706E   1C                   ENDIF
319900980706     C*
320000980706     C                   ENDSR
320100980706     C*------------------------------------------------------------------------*
320200980706     C* CALAEL - CALCOLA L'A.D.G. SULL'ESPRESSO CONSEGNA LOCALITA' DISAGIATE
320300980706     C*------------------------------------------------------------------------*
320400980706     C     CALAEL        BEGSR
320500980706     C*
320600980706     C* REIMPOSTA LE VARIABILI DI LAVORO
320700011031     C                   Z-ADD     *ZEROS        TASADG           21 8
320800980706     C*
320900980706     C* CALCOLA L'A.D.G.
321000011031     C                   Z-ADD     VIMP(39)      WIMP             23 8
321100980706     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
321200980706     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
321300980706     C                   EXSR      ADGCAL
321400980706     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
321500980706     C*
321600980706IF  1C     TASADG        IFGT      *ZEROS
321700980706     C                   Z-ADD     40            I
321800980706     C                   Z-ADD     TASADG        VIMP(I)
321900980706E   1C                   ENDIF
322000980706     C*
322100980706     C                   ENDSR
322200980706     C*------------------------------------------------------------------------*
322300980706     C* CALIEL - CALCOLA L'ISTAT SULL'ESPRESSO CONSEGNA LOCALITA' DISAGIATE
322400980706     C*------------------------------------------------------------------------*
322500980706     C     CALIEL        BEGSR
322600980706     C*
322700980706     C* REIMPOSTA LE VARIABILI DI LAVORO
322800011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
322900980706     C*
323000980706     C* CALCOLA L'ISTAT
323100011031     C                   Z-ADD     VIMP(39)      WIMP             23 8
323200980706     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
323300980706     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
323400980706     C                   EXSR      IACCAL
323500980706     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
323600980706     C*
323700980706IF  1C     TASIAC        IFGT      *ZEROS
323800980706     C                   Z-ADD     41            I
323900980706     C                   Z-ADD     TASIAC        VIMP(I)
324000980706E   1C                   ENDIF
324100980706     C*
324200980706     C                   ENDSR
324300980706     C*------------------------------------------------------------------------*
324400980706     C* CAIAEL - CALCOLA L'ISTAT SULL'ADG DELL'ESPRESSO CONSEGNA L.DISAGIATE
324500980706     C*------------------------------------------------------------------------*
324600980706     C     CAIAEL        BEGSR
324700980706     C*
324800980706     C* REIMPOSTA LE VARIABILI DI LAVORO
324900011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
325000980706     C*
325100980706     C* CALCOLA L'ISTAT
325200011031     C                   Z-ADD     VIMP(40)      WIMP             23 8
325300980706     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
325400980706     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
325500980706     C                   EXSR      IACCAL
325600980706     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
325700980706     C*
325800980706IF  1C     TASIAC        IFGT      *ZEROS
325900980706     C                   Z-ADD     42            I
326000980706     C                   Z-ADD     TASIAC        VIMP(I)
326100980706E   1C                   ENDIF
326200980706     C*
326300980706     C                   ENDSR
326400011031     C*------------------------------------------------------------------------*
326500011031     C* CALSAM - CALCOLA RITIRO ISOLA
326600011031     C*------------------------------------------------------------------------*
326700011031     C     CALSAM        BEGSR
326800011031     C*
326900011031     C* REIMPOSTA LE VARIABILI DI LAVORO
327000011031     C                   MOVEL     'S'           WCAR                           *SI UTILIZZO TAR.CAR
327100011031     C                   Z-ADD     *ZEROS        TASSAM           21 8
327200011031     C*
327300011031     C* CALCOLA RITIRO COME CONSEGNA PARTICOLARE ISOLA
327400011031     C                   MOVEL     'I'           TASTC             1            *CODICE CONSEGNA
327500011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
327600011031     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
327700011031     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
327800011031     C                   EXSR      CALCP
327900011031     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
328000050103     C                   Z-ADD     TASCP         TASSAM
328100011031     C*
328200011031IF  1C     TASSAM        IFGT      *ZEROS
328300011031     C                   Z-ADD     88            I
328400011031     C                   Z-ADD     TASSAM        VIMP(I)
328500011031E   1C                   ENDIF
328600011031     C*
328700011031     C* MEMORIZZA IL FLAG "APPLICAZIONE TARIFFA INOLTRO" DELLO SCAGLIONE DEL
328800011031     C* CODICE TASSAZIONE DEL DETTAGLIO TARIFFARIO -NON SERVE, MA TENIAMOLO-
328900011031     C                   MOVEL     DEPAIN        SAMAIN
329000011031     C*
329100011031     C                   ENDSR
329200011031     C*------------------------------------------------------------------------*
329300011031     C* CALAGR - CALCOLA L'A.D.G. SUL RITIRO ISOLA
329400011031     C*------------------------------------------------------------------------*
329500011031     C     CALAGR        BEGSR
329600011031     C*
329700011031     C* REIMPOSTA LE VARIABILI DI LAVORO
329800011031     C                   Z-ADD     *ZEROS        TASADG           21 8
329900011031     C*
330000011031     C* CALCOLA L'A.D.G.
330100011031     C                   Z-ADD     TASSAM        WIMP             23 8
330200011031     C                   EXSR      ADGCAL
330300011031     C*
330400011031IF  1C     TASADG        IFGT      *ZEROS
330500011031     C                   Z-ADD     93            I
330600011031     C                   Z-ADD     TASADG        VIMP(I)
330700011031E   1C                   ENDIF
330800011031     C*
330900011031     C                   ENDSR
331000011031     C*------------------------------------------------------------------------*
331100011031     C* CALISR - CALCOLA L'ISTAT SUL RITIRO ISOLA
331200011031     C*------------------------------------------------------------------------*
331300011031     C     CALISR        BEGSR
331400011031     C*
331500011031     C* REIMPOSTA LE VARIABILI DI LAVORO
331600011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
331700011031     C*
331800011031     C* CALCOLA L'ISTAT
331900011031     C                   Z-ADD     TASSAM        WIMP             23 8
332000011031     C                   EXSR      IACCAL
332100011031     C*
332200011031IF  1C     TASIAC        IFGT      *ZEROS
332300011031     C                   Z-ADD     94            I
332400011031     C                   Z-ADD     TASIAC        VIMP(I)
332500011031E   1C                   ENDIF
332600011031     C*
332700011031     C                   ENDSR
332800011031     C*------------------------------------------------------------------------*
332900011031     C* CALIAR - CALCOLA L'ISTAT SULL'ADG DEL RITIRO ISOLA
333000011031     C*------------------------------------------------------------------------*
333100011031     C     CALIAR        BEGSR
333200011031     C*
333300011031     C* REIMPOSTA LE VARIABILI DI LAVORO
333400011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
333500011031     C*
333600011031     C* CALCOLA L'ISTAT
333700011031     C                   Z-ADD     VIMP(93)      WIMP             23 8
333800011031     C                   EXSR      IACCAL
333900011031     C*
334000011031IF  1C     TASIAC        IFGT      *ZEROS
334100011031     C                   Z-ADD     95            I
334200011031     C                   Z-ADD     TASIAC        VIMP(I)
334300011031E   1C                   ENDIF
334400011031     C*
334500011031     C                   ENDSR
334600011031     C*------------------------------------------------------------------------*
334700011031     C* CALESR - CALCOLA L'ESPRESSO SUL RITIRO ISOLA
334800011031     C*------------------------------------------------------------------------*
334900011031     C     CALESR        BEGSR
335000011031     C*
335100011031     C* REIMPOSTA LE VARIABILI DI LAVORO
335200011031     C                   Z-ADD     *ZEROS        TASESP           21 8
335300011031     C*
335400011031     C* CALCOLA L'ESPRESSO
335500011031     C                   Z-ADD     VIMP(88)      IMPON            23 8
335600011031     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
335700011031     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
335800011031     C                   EXSR      CALTS
335900011031     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
336000011031     C*
336100011031IF  1C     TASESP        IFGT      *ZEROS
336200011031     C                   Z-ADD     89            I
336300011031     C                   Z-ADD     TASESP        VIMP(I)
336400011031E   1C                   ENDIF
336500011031     C*
336600011031     C                   ENDSR
336700011031     C*------------------------------------------------------------------------*
336800011031     C* CALAER - CALCOLA L'A.D.G. SULL'ESPRESSO RITIRO ISOLA
336900011031     C*------------------------------------------------------------------------*
337000011031     C     CALAER        BEGSR
337100011031     C*
337200011031     C* REIMPOSTA LE VARIABILI DI LAVORO
337300011031     C                   Z-ADD     *ZEROS        TASADG           21 8
337400011031     C*
337500011031     C* CALCOLA L'A.D.G.
337600011031     C                   Z-ADD     VIMP(89)      WIMP             23 8
337700011031     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
337800011031     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
337900011031     C                   EXSR      ADGCAL
338000011031     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
338100011031     C*
338200011031IF  1C     TASADG        IFGT      *ZEROS
338300011031     C                   Z-ADD     90            I
338400011031     C                   Z-ADD     TASADG        VIMP(I)
338500011031E   1C                   ENDIF
338600011031     C*
338700011031     C                   ENDSR
338800011031     C*------------------------------------------------------------------------*
338900011031     C* CALIER - CALCOLA L'ISTAT SULL'ESPRESSO RITIRO ISOLA
339000011031     C*------------------------------------------------------------------------*
339100011031     C     CALIER        BEGSR
339200011031     C*
339300011031     C* REIMPOSTA LE VARIABILI DI LAVORO
339400011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
339500011031     C*
339600011031     C* CALCOLA L'ISTAT
339700011031     C                   Z-ADD     VIMP(89)      WIMP             23 8
339800011031     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
339900011031     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
340000011031     C                   EXSR      IACCAL
340100011031     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
340200011031     C*
340300011031IF  1C     TASIAC        IFGT      *ZEROS
340400011031     C                   Z-ADD     91            I
340500011031     C                   Z-ADD     TASIAC        VIMP(I)
340600011031E   1C                   ENDIF
340700011031     C*
340800011031     C                   ENDSR
340900011031     C*------------------------------------------------------------------------*
341000011031     C* CAIAER - CALCOLA L'ISTAT SULL'ADG DELL'ESPRESSO RITIRO ISOLA
341100011031     C*------------------------------------------------------------------------*
341200011031     C     CAIAER        BEGSR
341300011031     C*
341400011031     C* REIMPOSTA LE VARIABILI DI LAVORO
341500011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
341600011031     C*
341700011031     C* CALCOLA L'ISTAT
341800011031     C                   Z-ADD     VIMP(90)      WIMP             23 8
341900011031     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
342000011031     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
342100011031     C                   EXSR      IACCAL
342200011031     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
342300011031     C*
342400011031IF  1C     TASIAC        IFGT      *ZEROS
342500011031     C                   Z-ADD     92            I
342600011031     C                   Z-ADD     TASIAC        VIMP(I)
342700011031E   1C                   ENDIF
342800011031     C*
342900011031     C                   ENDSR
343000011030     C*------------------------------------------------------------------------*
343100011030     C* CALCIS - CALCOLA CONSEGNA PARTICOLARE ISOLA
343200011030     C*------------------------------------------------------------------------*
343300011030     C     CALCIS        BEGSR
343400011030     C*
343500011030     C* REIMPOSTA LE VARIABILI DI LAVORO
343600011030     C                   MOVEL     'S'           WCAR                           *SI UTILIZZO TAR.CAR
343700011031     C                   Z-ADD     *ZEROS        TASCIS           21 8
343800011030     C*
343900011030     C* CALCOLA CONSEGNA PARTICOLARE
344000011030     C                   MOVEL     'I'           TASTC             1            *CODICE CONSEGNA
344100011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
344200011030     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
344300011030     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
344400011030     C                   EXSR      CALCP
344500011030     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
344600011030     C                   Z-ADD     TASCP         TASCIS
344700011030     C*
344800011030IF  1C     TASCIS        IFGT      *ZEROS
344900011030     C                   Z-ADD     80            I
345000011030     C                   Z-ADD     TASCIS        VIMP(I)
345100011030E   1C                   ENDIF
345200011030     C*
345300011030     C* MEMORIZZA IL FLAG "APPLICAZIONE TARIFFA INOLTRO" DELLO SCAGLIONE DEL
345400011030     C* CODICE TASSAZIONE DEL DETTAGLIO TARIFFARIO
345500011030     C                   MOVEL     DEPAIN        CISAIN
345600011030     C*
345700011030     C                   ENDSR
345800011030     C*------------------------------------------------------------------------*
345900011030     C* CALAGS - CALCOLA L'A.D.G. SULLE CONSEGNE ISOLA
346000011030     C*------------------------------------------------------------------------*
346100011030     C     CALAGS        BEGSR
346200011030     C*
346300011030     C* REIMPOSTA LE VARIABILI DI LAVORO
346400011031     C                   Z-ADD     *ZEROS        TASADG           21 8
346500011030     C*
346600011030     C* CALCOLA L'A.D.G.
346700011031     C                   Z-ADD     TASCIS        WIMP             23 8
346800011030     C                   EXSR      ADGCAL
346900011030     C*
347000011030IF  1C     TASADG        IFGT      *ZEROS
347100011030     C                   Z-ADD     85            I
347200011030     C                   Z-ADD     TASADG        VIMP(I)
347300011030E   1C                   ENDIF
347400011030     C*
347500011030     C                   ENDSR
347600011030     C*------------------------------------------------------------------------*
347700011030     C* CALISS - CALCOLA L'ISTAT SULLE CONSEGNE ISOLA
347800011030     C*------------------------------------------------------------------------*
347900011030     C     CALISS        BEGSR
348000011030     C*
348100011030     C* REIMPOSTA LE VARIABILI DI LAVORO
348200011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
348300011030     C*
348400011030     C* CALCOLA L'ISTAT
348500011031     C                   Z-ADD     TASCIS        WIMP             23 8
348600011030     C                   EXSR      IACCAL
348700011030     C*
348800011030IF  1C     TASIAC        IFGT      *ZEROS
348900011030     C                   Z-ADD     86            I
349000011030     C                   Z-ADD     TASIAC        VIMP(I)
349100011030E   1C                   ENDIF
349200011030     C*
349300011030     C                   ENDSR
349400011030     C*------------------------------------------------------------------------*
349500011030     C* CALIAS - CALCOLA L'ISTAT SULL'ADG DELLE CONEGNE ISOLA
349600011030     C*------------------------------------------------------------------------*
349700011030     C     CALIAS        BEGSR
349800011030     C*
349900011030     C* REIMPOSTA LE VARIABILI DI LAVORO
350000011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
350100011030     C*
350200011030     C* CALCOLA L'ISTAT
350300011031     C                   Z-ADD     VIMP(85)      WIMP             23 8
350400011030     C                   EXSR      IACCAL
350500011030     C*
350600011030IF  1C     TASIAC        IFGT      *ZEROS
350700011030     C                   Z-ADD     87            I
350800011030     C                   Z-ADD     TASIAC        VIMP(I)
350900011030E   1C                   ENDIF
351000011030     C*
351100011030     C                   ENDSR
351200011030     C*------------------------------------------------------------------------*
351300011030     C* CALESS - CALCOLA L'ESPRESSO SULLA CONSEGNA ISOLA
351400011030     C*------------------------------------------------------------------------*
351500011030     C     CALESS        BEGSR
351600011030     C*
351700011030     C* REIMPOSTA LE VARIABILI DI LAVORO
351800011031     C                   Z-ADD     *ZEROS        TASESP           21 8
351900011030     C*
352000011030     C* CALCOLA L'ESPRESSO
352100011031     C                   Z-ADD     VIMP(80)      IMPON            23 8
352200011030     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
352300011030     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
352400011030     C                   EXSR      CALTS
352500011030     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
352600011030     C*
352700011030IF  1C     TASESP        IFGT      *ZEROS
352800011030     C                   Z-ADD     81            I
352900011030     C                   Z-ADD     TASESP        VIMP(I)
353000011030E   1C                   ENDIF
353100011030     C*
353200011030     C                   ENDSR
353300011030     C*------------------------------------------------------------------------*
353400011030     C* CALAES - CALCOLA L'A.D.G. SULL'ESPRESSO CONSEGNA ISOLA
353500011030     C*------------------------------------------------------------------------*
353600011030     C     CALAES        BEGSR
353700011030     C*
353800011030     C* REIMPOSTA LE VARIABILI DI LAVORO
353900011031     C                   Z-ADD     *ZEROS        TASADG           21 8
354000011030     C*
354100011030     C* CALCOLA L'A.D.G.
354200011031     C                   Z-ADD     VIMP(81)      WIMP             23 8
354300011030     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
354400011030     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
354500011030     C                   EXSR      ADGCAL
354600011030     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
354700011030     C*
354800011030IF  1C     TASADG        IFGT      *ZEROS
354900011030     C                   Z-ADD     82            I
355000011030     C                   Z-ADD     TASADG        VIMP(I)
355100011030E   1C                   ENDIF
355200011030     C*
355300011030     C                   ENDSR
355400011030     C*------------------------------------------------------------------------*
355500011030     C* CALIEL - CALCOLA L'ISTAT SULL'ESPRESSO CONSEGNA ISOLA
355600011030     C*------------------------------------------------------------------------*
355700011030     C     CALIES        BEGSR
355800011030     C*
355900011030     C* REIMPOSTA LE VARIABILI DI LAVORO
356000011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
356100011030     C*
356200011030     C* CALCOLA L'ISTAT
356300011031     C                   Z-ADD     VIMP(81)      WIMP             23 8
356400011030     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
356500011030     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
356600011030     C                   EXSR      IACCAL
356700011030     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
356800011030     C*
356900011030IF  1C     TASIAC        IFGT      *ZEROS
357000011030     C                   Z-ADD     83            I
357100011030     C                   Z-ADD     TASIAC        VIMP(I)
357200011030E   1C                   ENDIF
357300011030     C*
357400011030     C                   ENDSR
357500011030     C*------------------------------------------------------------------------*
357600011030     C* CAIAES - CALCOLA L'ISTAT SULL'ADG DELL'ESPRESSO CONSEGNA ISOLA
357700011030     C*------------------------------------------------------------------------*
357800011030     C     CAIAES        BEGSR
357900011030     C*
358000011030     C* REIMPOSTA LE VARIABILI DI LAVORO
358100011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
358200011030     C*
358300011030     C* CALCOLA L'ISTAT
358400011031     C                   Z-ADD     VIMP(82)      WIMP             23 8
358500011030     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
358600011030     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
358700011030     C                   EXSR      IACCAL
358800011030     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
358900011030     C*
359000011030IF  1C     TASIAC        IFGT      *ZEROS
359100011030     C                   Z-ADD     84            I
359200011030     C                   Z-ADD     TASIAC        VIMP(I)
359300011030E   1C                   ENDIF
359400011030     C*
359500011030     C                   ENDSR
359600980706     C*------------------------------------------------------------------------*
359700011030     C* CA3INL - RICALCOLA INOLTRO X LOCALITA' DISAGIATE / CONSEGNA ISOLA
359800980706     C*------------------------------------------------------------------------*
359900980706     C     CA3INL        BEGSR
360000980706     C*---
360100011030     C* FLAG='S' -> PAGA TARIFFA INOLTRO + TARIFFA LOCALITA' DISAGIATA  + ISOLA
360200011030     C* FLAG=' ' -> CANCELLA L'INOLTRO SE LA L.DISAGIATA o ISOLA E' DA PAGARE
360300011030     C*
360400011030     C* NOTE: In tassazione una bolla può essere O Località disagiata O Consegna isola.
360500011030     C*       Nel CAT l'azzeramento dell'inoltro scatta anche se SOLO UNA delle due
360600011030     C*       consegne prevede l'azzeramento e quindi può esistere l'incongruenza
360700011030     C*       che si abbia l'inoltro azzerato a fronte del fatto che UNA delle due consegne
360800011030     C*       non preveda l'azzeramento. (Mha!, avrete capito???)
360900011030     C*---
361000011030     c***
361100011030     c* LOCALITA' DISAGIATE / ISOLA
361200011030     c***
361300011030IF  1C     CPLAIN        IFEQ      *BLANKS
361301060607IF  2C     VC(35)        andeq     'S'                                          *VOCE DA CALCOLARE
361400011030     C     CISAIN        OREQ      *BLANKS
361401060607IF  2C     VC(80)        andeq     'S'                                          *VOCE DA CALCOLARE
361500980706     C*
361600980706     C* AZZERA LE VOCI INOLTRO
361700011030IF  2C     VIMP(35)      IFGT      *ZEROS                                       *L.DISAG.DA PAGARE
361800011030     C     VIMP(80)      ORGT      *ZEROS                                       *C.ISOLA DA PAGARE
361900980706DO  3C     1             DO        NVOCA         V                 6 0
362000980706IF  4C     VIMP(V)       IFGT      *ZEROS
362100980706IF  5C     V             IFEQ      006
362200980706     C     V             OREQ      021
362300980706     C     V             OREQ      024
362400980706     C     V             OREQ      029
362500980706     C     V             OREQ      034
362600980706     C     V             OREQ      036
362700980706     C     V             OREQ      037
362800980706     C     V             OREQ      038
362900980706     C                   Z-ADD     *ZEROS        VIMP(V)
363000980706E   5C                   ENDIF
363100980706E   4C                   ENDIF
363200980706E   3C                   ENDDO
363300980706E   2C                   ENDIF
363400980706E   1C                   ENDIF
363500980706     C*
363600980706     C                   ENDSR
363700980706     C*------------------------------------------------------------------------*
363800980706     C* CALTBT - CALCOLA LA BASE PER IL CALCOLO DELLE TARIFFE CON % SUL TRASPORTO
363900980706     C*------------------------------------------------------------------------*
364000980706     C     CALTBT        BEGSR
364100980706     C*
364200980706     C* REIMPOSTA LE VARIABILI DI LAVORO
364300011031     C                   Z-ADD     *ZEROS        TOTTRA           23 8
364400980706     C*
364500011030     C* PORTO + INOLTRO + LOCALITA' DISAGIATE + ISOLA + LORO ESPRESSI
364600980706     C     VIMP(1)       ADD       TOTTRA        TOTTRA                         *PORTO
364700980706     C     VIMP(6)       ADD       TOTTRA        TOTTRA                         *INOLTRO
364800980706     C     VIMP(35)      ADD       TOTTRA        TOTTRA                         *LOCALITA' DISAGIATE
364900980706     C     VIMP(7)       ADD       TOTTRA        TOTTRA                         *ESPRESSO PORTO
365000980706     C     VIMP(36)      ADD       TOTTRA        TOTTRA                         *ESPRESSO INOLTRO
365100980706     C     VIMP(39)      ADD       TOTTRA        TOTTRA                         *ESPRESSO L.DISAGIAT
365200011030     C     VIMP(80)      ADD       TOTTRA        TOTTRA                         *CONSEGNA ISOLA
365300011030     C     VIMP(81)      ADD       TOTTRA        TOTTRA                         *ESPRESSO C.ISOLA
365400980706     C*
365500980706     C                   ENDSR
365600970306     C*------------------------------------------------------------------------*
365700970306     C* CALTS  - CALCOLA TIPO SERVIZIO ESPRESSO
365800970306     C*------------------------------------------------------------------------*
365900980706     C     CALTS         BEGSR
366000970306     C*
366100980506     C* REIMPOSTA LE VARIABILI DI LAVORO
366200980506     C                   MOVEL     'S'           WCAR              1            *SI UTILIZZO TAR.CAR
366300020912     C                   MOVEL     'E'           WTSP              1              ESPRESSO='E'
366400980506     C*
366500970306     C* REPERISCE IL CODICE DELLA PARTICOLARITA' ESPRESSO
366600970306     C                   SETOFF                                           28
366700970306     C                   Z-ADD     1             I
366800980506     C     WTSP          LOOKUP    TS(I)                                  28
366900970306IF  1C     *IN28         IFEQ      *ON                                          *SERVIZIO ESISTENTE
367000980506     C     I             OCCUR     DS5E
367100980506IF  2C     §5EFTC        IFNE      *BLANKS                                      *SERVIZIO CON PARTIC
367200980506IF  3C     TAMTSP        IFEQ      WTSP
367300980804     C                   MOVEL     'N'           WCAR                           *NO UTILIZZO TAR.CAR
367400980506E   3C                   ENDIF
367500980506     C*
367600980506     C* CERCA LA CONSEGNA PARTICOLARE
367700980506     C                   MOVEL     §5EFTC        TASTC
367800980506     C                   EXSR      CERTPT
367900980506     C*
368000980506     C* CALCOLA RPV, ARROTONDAMENTI E PESO TASSABILE
368100980506IF  3C     $TPT          IFEQ      '1'                                          *TARIFFA TROVATA
368200980707     C                   MOVEL     TPTTPG        WTPG
368300980707     C                   MOVEL     TPTRPV        WRPV
368400980707     C                   CLEAR                   WDET
368500980707     C                   Z-ADD     TPTARL        WARL
368600980707     C                   Z-ADD     TPTARF        WARF
368700980707     C                   Z-ADD     TPTARO        WARO
368800980506     C                   EXSR      CALCOL
368900980506IF  4C     TPTTPG        IFEQ      'T'                                          *A TRASPORTO
369000980506     C                   Z-ADD     IMPON         ISOPES                         *PORTO
369100980506E   4C                   ENDIF
369200970306     C*
369300970306     C* CALCOLA LA TARIFFA ESPRESSO
369400980506     C                   Z-ADD     ISOPES        TPDPES                         *PESO
369500970306     C                   EXSR      CALACD
369600970306     C                   Z-ADD     VARIA         TASESP
369700970306E   3C                   ENDIF
369800970306E   2C                   ENDIF
369900970306E   1C                   ENDIF
370000970306     C*
370100970306     C                   ENDSR
370200970306     C*------------------------------------------------------------------------*
370300970306     C* CALAGD - CALCOLA L'A.D.G. SUL DIRITTO FISSO
370400970306     C*------------------------------------------------------------------------*
370500970306     C     CALAGD        BEGSR
370600970306     C*
370700970306     C* REIMPOSTA LE VARIABILI DI LAVORO
370800011031     C                   Z-ADD     *ZEROS        TASADG           21 8
370900970306     C*
371000970306     C* CALCOLA L'A.D.G.
371100011031     C                   Z-ADD     TASDIF        WIMP             23 8
371200970306     C                   EXSR      ADGCAL
371300970306     C*
371400970314IF  1C     TASADG        IFGT      *ZEROS
371500970306     C                   Z-ADD     22            I
371600970306     C                   Z-ADD     TASADG        VIMP(I)
371700970314E   1C                   ENDIF
371800970306     C*
371900970306     C                   ENDSR
372000970306     C*------------------------------------------------------------------------*
372100970306     C* ADGCAL - CALCOLA A.D.G. SU IMPORTO GENERICO
372200970306     C*------------------------------------------------------------------------*
372300970306     C     ADGCAL        BEGSR
372400041213     C* anni precedenti
372500970306IF  1C     TAMPAG        IFNE      *ZEROS                                       *INDICATA IN TARIFFA
372600011031     C     WIMP          MULT      TAMPAG        COMADG           23 8
372700970306IF  2C     COMADG        IFGT      *ZEROS
372800970306     C     COMADG        DIV(H)    100           TASADG
372900970306E   2C                   ENDIF
373000970306E   1C                   ENDIF
373100041213     c                   z-add     tasadg        tasadgs
373200041213     C* anno in corso
373300041213IF  1C     TAMPpa        IFNE      *ZEROS                                       *INDICATA IN TARIFFA
373400041213     c                   eval      comadg = (wimp + tasadg)*tamppa
373500041213IF  2C     COMADG        IFGT      *ZEROS
373600041213     C     COMADG        DIV(H)    100           TASADG
373700041213E   2C                   ENDIF
373800041213     c                   add       tasadgs       tasadg
373900041213E   1C                   ENDIF
374000970306     C*
374100970306     C                   ENDSR
374200980706     C*------------------------------------------------------------------------*
374300980706     C* IACCAL - CALCOLA ISTAT SU IMPORTO GENERICO
374400980706     C*------------------------------------------------------------------------*
374500980706     C     IACCAL        BEGSR
374600980706     C*
374700980706     C* REIMPOSTA LE VARIABILI DI LAVORO
374800980706     C                   Z-ADD     *ZEROS        PI                3 0
374900980706     C                   Z-ADD     *ZEROS        PUNIAC            3 1
375000011031     C                   Z-ADD     *ZEROS        PERIAC           23 8
375100011031     C                   Z-ADD     *ZEROS        PERIAX           23 8
375200980706     C*
375300980706     C* CALCOLA PUNTI ISTAT DA APPLICARE
375400980706IF  1C     TAMFLI        IFGT      *ZEROS                                       *INDICATO IN TARIFFA
375500980706     C                   Z-ADD     TAMFLI        PI
375600980706     C                   SUB       1             PI
375700980706DO  2C     I             DO        12            I                              *SONO IN RELTA' MAX
375800980706IF  3C     PI            IFLE      8
375900980706     C                   ADD       1             PI
376000980706X   3C                   ELSE
376100980706     C                   LEAVE
376200980706E   3C                   ENDIF
376300980706IF  3C     D00DTC        IFGE      DIA(PI)
376301061018      * CALCOLO LA PERCENTUALE DI BASE DI CALCOLO
376302061018      * SE ESISTE GIÀ UNA PERCENTUALE CALCOLO LA PERENTUALE DELLA PERCENTYUALE E POI SOMMO TUTTO
376303061018      * ESEMPIO-....
376304061018      * 1° PUNTO PERCENTUALE ISTAT  2%
376305061018      * 2° PUNTO PERCENTUALE ISTAT  5%
376306061018      * PERCENTUALE ISTAT = 2 + (IL 5% DI 2) + 5 = 7,1
376307061018
376308061018     C                   IF        PUNIAC > 0
376309061018     C                   EVAL      PUNIAC = PUNIAC +((PUNIAC * IAC(PI)) / 100)
376310061018     C                   ENDIF
376400980706     C                   ADD       IAC(PI)       PUNIAC
376500980706X   3C                   ELSE
376600980706     C                   LEAVE
376700980706E   3C                   ENDIF
376800980706E   2C                   ENDDO
376900980706     C* CALCOLA L'ISTAT
377000980706     C     PUNIAC        MULT      TAMPRI        PERIAX
377100980706IF  2C     PERIAX        IFNE      *ZEROS
377200980706     C     PERIAX        DIV       100           PERIAC
377300980706E   2C                   ENDIF
377400980706IF  2C     PERIAC        IFNE      *ZEROS
377401061018     C                   DIV       100           PERIAC
377500980706     C     WIMP          MULT(H)   PERIAC        TASIAC
377600980706E   2C                   ENDIF
377700980706E   1C                   ENDIF
377800980706     C*
377900980706     C                   ENDSR
378000970306     C*------------------------------------------------------------------------*
378100060314     C* CALASC - CALCOLA ASSICURAZIONE ac plusTO
378200970306     C*------------------------------------------------------------------------*
378300970306     C     CALASC        BEGSR
378400970306     C*
378500970306     C* REIMPOSTA LE VARIABILI DI LAVORO
378600011031     C                   Z-ADD     *ZEROS        TASASF           21 8
378700011031     C                   Z-ADD     *ZEROS        TASASA           21 8
378800980506     C*---
378900980506     C* FRANCO
379000980506     C*---
379100980506     C                   MOVEL     'F'           WTPO
379200970306     C                   EXSR      CALVR
379300970306IF  1C     VC(9)         IFEQ      'S'                                          *VOCE DA CALCOLARE
379400970314     C     TASASF        ANDGT     *ZEROS
379500970306     C                   Z-ADD     9             I
379600970306     C                   Z-ADD     TASASF        VIMP(I)
379700970306E   1C                   ENDIF
379800980506     C*---
379900980506     C* ASSEGNATO
380000980506     C*---
380100980506     C                   MOVEL     'A'           WTPO
380200980506     C                   EXSR      CALVR
380300970306IF  1C     VC(31)        IFEQ      'S'                                          *VOCE DA CALCOLARE
380400970314     C     TASASA        ANDGT     *ZEROS
380500970306     C                   Z-ADD     31            I
380600970306     C                   Z-ADD     TASASA        VIMP(I)
380700970306E   1C                   ENDIF
380800970306     C*
380900970306     C                   ENDSR
381000060314     C*------------------------------------------------------------------------*
381100060314     C* CALAcb - CALCOLA ASSICURAZIONE ac base
381200060314     C*------------------------------------------------------------------------*
381300060315     C     CALAcb        BEGSR
381400060314     C*
381500060314     C* REIMPOSTA LE VARIABILI DI LAVORO
381600060314     C                   Z-ADD     *ZEROS        TASASF           21 8
381700060314     C                   Z-ADD     *ZEROS        TASASA           21 8
381800060314     C*---
381900060314     C* FRANCO
382000060314     C*---
382100060314     C                   MOVEL     'F'           WTPO
382200060314     C                   EXSR      CALVR
382300060314IF  1C     VC(105)       IFEQ      'S'                                          *VOCE DA CALCOLARE
382400060314     C     TASASF        ANDGT     *ZEROS
382500060314     C                   Z-ADD     105           I
382600060314     C                   Z-ADD     TASASF        VIMP(I)
382700060314E   1C                   ENDIF
382800060314     C*---
382900060314     C* ASSEGNATO
383000060314     C*---
383100060314     C                   MOVEL     'A'           WTPO
383200060314     C                   EXSR      CALVR
383300060314IF  1C     VC(108)       IFEQ      'S'                                          *VOCE DA CALCOLARE
383400060314     C     TASASA        ANDGT     *ZEROS
383500060314     C                   Z-ADD     108           I
383600060314     C                   Z-ADD     TASASA        VIMP(I)
383700060314E   1C                   ENDIF
383800060314     C*
383900060314     C                   ENDSR
384000970306     C*------------------------------------------------------------------------*
384100011023     C* CALVR - CALCOLA CONSEGNA PARTICOLARE ASSICURAZIONE PER CONTO
384200970306     C*------------------------------------------------------------------------*
384300970306     C     CALVR         BEGSR
384400970306     C*
384500980506     C* NB: L'IMPORTO DA ASSICURARE NEI DATI TIPO E' OBBLIGATORIO: TOGLIENDO LA
384600980506     C*     OBBLIGATORIETA' E' NECESSARIO GESTIRE IL MANCA TARIFFA PER MANDATI
384700980702     C*     A VALORE VARIABILE CON IMPORTO UGUALE A ZERO.
384800970306     C*
384900970306     C* SE CLIENTE 88 CALCOLA TARIFFA SE ESISTE IL VALORE MERCE NEI DATI TIPO
385000970306     C                   MOVEL     D00KSC        WA4               4 0
385100970306IF  1C     WA4           IFEQ      8888
385200970418     C     ETCASC        ANDGT     *ZEROS
385300970306     C     WA4           ORNE      8888
385400970306     C                   Z-ADD     1             I
385500970306     C                   SETOFF                                           28
385600990819     C                   MOVEL     KTPFTC        A1
385700990819     C     A1            LOOKUP    CP(I)                                  28
385800970306IF  2C     *IN28         IFEQ      *ON                                          *C.PART.ASS.ESISTENT
385900970306     C     I             OCCUR     CPIDS                                        *TARIFFA DI CARTELLO
386000970306     C*
386100970306     C* RICERCA L'ASSICURAZIONE PER CONTO NELLE CONSEGNE PARTICOLARI DEL CLIENTE
386200970306     C                   Z-ADD     D00KSC        KTPKSC
386300970306     C                   Z-ADD     D00CTR        KTPCTR
386400970306     C                   Z-ADD     D00PRG        KTPPRG
386500011019     C                   MOVEL     D00DIV        WDIV
386600970306     C                   MOVEL     '0'           $TPT                           *TARIFFA NON TROVATA
386700990819     C     KEYTPT        SETLL     TITPT01L                               99
386800990819     C     KEYTPT        READE     TITPT01L                               99
386900970306DO  3C     *IN99         DOWEQ     *OFF
387000991125     C     $TPT          ANDEQ     '0'
387100991125     C*
387200991125     C* CONTROLLA SE CONVERTIRE I CAMPI DI TITPT
387300011026IF  4C     WDIV          IFNE      WDIVCE
387400011019     C                   MOVEL     WDIV          WDVI
387500991125     C                   EXSR      CNVTPT
387600991125E   4C                   ENDIF
387700991125     C*
387800970306IF  4C     TPTATB        IFEQ      *BLANKS
387900980506IF  5C     TPTAPL        IFEQ      *BLANKS                                      *APPLICAZIONE ENTRAM
388000980506     C     TPTAPL        OREQ      'A'                                          *APPLICAZIONE ARRIVO
388100980506     C     WTPO          ANDEQ     'A'                                          *ASSEGNATI O
388200980506     C     TPTAPL        OREQ      'P'                                          *APPLICAZIONE PARTEN
388300980506     C     WTPO          ANDEQ     'F'                                          *FRANCHI
388400970306     C                   MOVEL     '1'           $TPT                           *TARIFFA TROVATA
388500970314     C*
388600970314     C* USA IL VALORE MERCE IN TARIFFA SE C'E' ALTRIMENTI QUELLO DEI DATI TIPO
388700980506IF  6C     TPTVLM        IFGT      *ZEROS                                       *VALORE IN TARIFFA
388800980506IF  7C     TPTFVM        IFEQ      '1'                                          *TARIFFA A COLLI
388900980506     C     SCSC(S)       MULT      TPTVLM        IASLIT
389000980506E   7C                   ENDIF
389100980506IF  7C     TPTFVM        IFEQ      '2'                                          *TARIFFA A SPEDIZION
389200980506     C                   Z-ADD     TPTVLM        IASLIT
389300980506E   7C                   ENDIF
389400980506IF  7C     TPTFVM        IFEQ      '3'                                          *TARIFFA A PESO
389500980506     C     SPSC(S)       MULT      TPTVLM        IASLIT
389600980506E   7C                   ENDIF
389700980506X   6C                   ELSE                                                   *NO VALORE IN TARIFF
389800970314     C                   MOVEL     '2'           TPTFVM                         *TARIFFA A SPEDIZION
389900980506E   6C                   ENDIF
390000980506E   5C                   ENDIF
390100980506E   4C                   ENDIF
390200970306     C*
390300970306     C* LETTURA SUCESSIVA TARIFFE
390400970306IF  4C     $TPT          IFEQ      '0'                                          *TARIFFA NON TROVATA
390500990819     C     KEYTPT        READE     TITPT01L                               99
390600970306E   4C                   ENDIF
390700970306E   3C                   ENDDO                                                  *FINE TAR.PART.KSC
390800970306     C*
390900970306     C* TARIFFA ASSICURAZIONE NON TROVATA NELLE CONSEGNE PARTICOLARI DEL CLIENTE
391000970306     C* RICERCA NELLE CONSEGNE PARTICOLARI DI CARTELLO
391100970306IF  3C     $TPT          IFEQ      '0'                                          *TARIFFA KSC NO TROV
391200970306     C     CP88          ANDNE     'NO'
391300970306     C                   MOVEL     '1'           $TPT                           *TARIFFA TROVATA
391400970306     C                   Z-ADD     CARKSC        TPTKSC                         *CHIAVE CARTELLO
391500970306     C                   Z-ADD     CARCTR        TPTCTR
391600970306     C                   Z-ADD     CARPRG        TPTPRG
391700970306     C                   MOVEL     KTPFTC        TPTFTC
391800970306     C                   MOVEL     TTPG8P        TPTTPG                         *CAMPI CARTELLO
391900970306     C                   Z-ADD     TARF8P        TPTARF
392000970306     C                   Z-ADD     TARL8P        TPTARL
392100970306     C                   Z-ADD     TARO8P        TPTARO
392200970306     C                   Z-ADD     TRPV8P        TPTRPV
392300970306     C                   MOVEL     TAPL8P        TPTAPL
392400970306E   3C                   ENDIF
392500970306     C*
392600980506     C* CALCOLA RPV, ARROTONDAMENTI E PESO TASSABILE
392700970306IF  3C     $TPT          IFEQ      '1'                                          *TARIFFA TROVATA
392800980707     C                   MOVEL     TPTTPG        WTPG
392900980707     C                   MOVEL     TPTRPV        WRPV
393000980707     C                   CLEAR                   WDET
393100980707     C                   Z-ADD     TPTARL        WARL
393200980707     C                   Z-ADD     TPTARF        WARF
393300980707     C                   Z-ADD     TPTARO        WARO
393400980506     C                   EXSR      CALCOL
393500980506IF  4C     TPTTPG        IFEQ      'V'                                          *CALCOLO A VALORE
393600980506     C                   Z-ADD     IASLIT        ISOPES                         *IMPORTO DA ASSICURA
393700970306E   4C                   ENDIF
393800980703IF  4C     TPTTPG        IFEQ      'T'                                          *A TRASPORTO
393900980703     C                   Z-ADD     TOTTRA        ISOPES                         *TRASPORTO
394000980703E   4C                   ENDIF
394100970306     C*
394200970306     C* CALCOLA LA TARIFFA ASSICURAZIONE PER CONTO
394300011031     C                   Z-ADD     ISOPES        TPDPES           23 8          *PESO
394400970306     C                   EXSR      CALACD
394500980706     C                   EXSR      CALTRA                                       *CALCOLA % A TRASPOR
394600980506IF  4C     WTPO          IFEQ      'A'
394700980506     C                   Z-ADD     VARIA         TASASA                         *ASSIC.NE ASSEGNATO
394800980506X   4C                   ELSE
394900980506     C                   Z-ADD     VARIA         TASASF                         *ASSIC.NE FRANCO
395000970306E   4C                   ENDIF
395100970306E   3C                   ENDIF
395200970306E   2C                   ENDIF
395300970306E   1C                   ENDIF
395400970306     C*
395500970306     C                   ENDSR
395600001227     C*------------------------------------------------------------------------*
395700060315     C* CAL048 - CALCOLA ADG SU ASSIC ac plus FRANCO
395800001227     C*------------------------------------------------------------------------*
395900001227     C     CAL048        BEGSR
396000001227     C*
396100001227     C* REIMPOSTA LE VARIABILI DI LAVORO
396200011031     C                   Z-ADD     *ZEROS        TASADG           21 8
396300001227     C*
396400001227     C* CALCOLA L'A.D.G.
396500011031     C                   Z-ADD     VIMP(9)       WIMP             23 8
396600001227     C                   EXSR      ADGCAL
396700001227     C*
396800001227IF  1C     TASADG        IFGT      *ZEROS
396900001227     C                   Z-ADD     48            I
397000001227     C                   Z-ADD     TASADG        VIMP(I)
397100001227E   1C                   ENDIF
397200001227     C*
397300001227     C                   ENDSR
397400001227     C*------------------------------------------------------------------------*
397500060315     C* CAL065 - CALCOLA ISTAT SU ADG/ASS ac plus FRANCO
397600001227     C*------------------------------------------------------------------------*
397700001227     C     CAL065        BEGSR
397800001227     C*
397900001227     C* REIMPOSTA LE VARIABILI DI LAVORO
398000011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
398100001227     C*
398200001227     C* CALCOLA L'ISTAT
398300011031     C                   Z-ADD     VIMP(48)      WIMP             23 8
398400001227     C                   EXSR      IACCAL
398500001227     C*
398600001227IF  1C     TASIAC        IFGT      *ZEROS
398700001227     C                   Z-ADD     65            I
398800001227     C                   Z-ADD     TASIAC        VIMP(I)
398900001227E   1C                   ENDIF
399000001227     C*
399100001227     C                   ENDSR
399200001227     C*------------------------------------------------------------------------*
399300060315     C* CAL060 - CALCOLA ADG SU ASSIC ac plus ASSEGNATO
399400001227     C*------------------------------------------------------------------------*
399500001227     C     CAL060        BEGSR
399600001227     C*
399700001227     C* REIMPOSTA LE VARIABILI DI LAVORO
399800011031     C                   Z-ADD     *ZEROS        TASADG           21 8
399900001227     C*
400000001227     C* CALCOLA L'A.D.G.
400100011031     C                   Z-ADD     VIMP(31)      WIMP             23 8
400200001227     C                   EXSR      ADGCAL
400300001227     C*
400400001227IF  1C     TASADG        IFGT      *ZEROS
400500001227     C                   Z-ADD     60            I
400600001227     C                   Z-ADD     TASADG        VIMP(I)
400700001227E   1C                   ENDIF
400800001227     C*
400900001227     C                   ENDSR
401000001227     C*------------------------------------------------------------------------*
401100060315     C* CAL077 - CALCOLA ISTAT SU ADG/ASS ac plus ASSEGNATO
401200001227     C*------------------------------------------------------------------------*
401300001227     C     CAL077        BEGSR
401400001227     C*
401500001227     C* REIMPOSTA LE VARIABILI DI LAVORO
401600011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
401700001227     C*
401800001227     C* CALCOLA L'ISTAT
401900011031     C                   Z-ADD     VIMP(60)      WIMP             23 8
402000001227     C                   EXSR      IACCAL
402100001227     C*
402200001227IF  1C     TASIAC        IFGT      *ZEROS
402300001227     C                   Z-ADD     77            I
402400001227     C                   Z-ADD     TASIAC        VIMP(I)
402500001227E   1C                   ENDIF
402600001227     C*
402700001227     C                   ENDSR
402800060315     C*------------------------------------------------------------------------*
402900060315     C* CAL106 - CALCOLA ADG SU ASSIC ac base FRANCO
403000060315     C*------------------------------------------------------------------------*
403100060315     C     CAL106        BEGSR
403200060315     C*
403300060315     C* REIMPOSTA LE VARIABILI DI LAVORO
403400060315     C                   Z-ADD     *ZEROS        TASADG           21 8
403500060315     C*
403600060315     C* CALCOLA L'A.D.G.
403700060315     C                   Z-ADD     VIMP(105)     WIMP             23 8
403800060315     C                   EXSR      ADGCAL
403900060315     C*
404000060315IF  1C     TASADG        IFGT      *ZEROS
404100060315     C                   Z-ADD     106           I
404200060315     C                   Z-ADD     TASADG        VIMP(I)
404300060315E   1C                   ENDIF
404400060315     C*
404500060315     C                   ENDSR
404600060315     C*------------------------------------------------------------------------*
404700060315     C* CAL107 - CALCOLA ISTAT SU ADG/ASS ac base FRANCO
404800060315     C*------------------------------------------------------------------------*
404900060315     C     CAL107        BEGSR
405000060315     C*
405100060315     C* REIMPOSTA LE VARIABILI DI LAVORO
405200060315     C                   Z-ADD     *ZEROS        TASIAC           21 8
405300060315     C*
405400060315     C* CALCOLA L'ISTAT
405500060315     C                   Z-ADD     VIMP(106)     WIMP             23 8
405600060315     C                   EXSR      IACCAL
405700060315     C*
405800060315IF  1C     TASIAC        IFGT      *ZEROS
405900060315     C                   Z-ADD     107           I
406000060315     C                   Z-ADD     TASIAC        VIMP(I)
406100060315E   1C                   ENDIF
406200060315     C*
406300060315     C                   ENDSR
406400060315     C*------------------------------------------------------------------------*
406500060315     C* CAL109 - CALCOLA ADG SU ASSIC ac base ASSEGNATO
406600060315     C*------------------------------------------------------------------------*
406700060315     C     CAL109        BEGSR
406800060315     C*
406900060315     C* REIMPOSTA LE VARIABILI DI LAVORO
407000060315     C                   Z-ADD     *ZEROS        TASADG           21 8
407100060315     C*
407200060315     C* CALCOLA L'A.D.G.
407300060315     C                   Z-ADD     VIMP(108)     WIMP             23 8
407400060315     C                   EXSR      ADGCAL
407500060315     C*
407600060315IF  1C     TASADG        IFGT      *ZEROS
407700060315     C                   Z-ADD     109           I
407800060315     C                   Z-ADD     TASADG        VIMP(I)
407900060315E   1C                   ENDIF
408000060315     C*
408100060315     C                   ENDSR
408200060315     C*------------------------------------------------------------------------*
408300060315     C* CAL110 - CALCOLA ISTAT SU ADG/ASS ac base ASSEGNATO
408400060315     C*------------------------------------------------------------------------*
408500060315     C     CAL110        BEGSR
408600060315     C*
408700060315     C* REIMPOSTA LE VARIABILI DI LAVORO
408800060315     C                   Z-ADD     *ZEROS        TASIAC           21 8
408900060315     C*
409000060315     C* CALCOLA L'ISTAT
409100060315     C                   Z-ADD     VIMP(109)     WIMP             23 8
409200060315     C                   EXSR      IACCAL
409300060315     C*
409400060315IF  1C     TASIAC        IFGT      *ZEROS
409500060315     C                   Z-ADD     110           I
409600060315     C                   Z-ADD     TASIAC        VIMP(I)
409700060315E   1C                   ENDIF
409800060315     C*
409900060315     C                   ENDSR
410000970306     C*------------------------------------------------------------------------*
410100970306     C* CALRCV - CALCOLA ASSICURAZIONE R.C.V.
410200970306     C*------------------------------------------------------------------------*
410300970306     C     CALRCV        BEGSR
410400970306     C*
410500970306     C* REIMPOSTA LE VARIABILI DI LAVORO
410600011031     C                   Z-ADD     *ZEROS        TASRCF           21 8
410700011031     C                   Z-ADD     *ZEROS        TASRCA           21 8
410800980506     C*---
410900980506     C* FRANCO
411000980506     C*---
411100980506     C                   MOVEL     'F'           WTPO
411200970306     C                   EXSR      CALVE
411300970306IF  1C     VC(10)        IFEQ      'S'                                          *VOCE DA CALCOLARE
411400970314     C     TASRCF        ANDGT     *ZEROS
411500970306     C                   Z-ADD     10            I
411600970306     C                   Z-ADD     TASRCF        VIMP(I)
411700970306E   1C                   ENDIF
411800980506     C*---
411900980506     C* ASSEGNATO
412000980506     C*---
412100980707     C                   MOVEL     'A'           WTPO              1
412200980506     C                   EXSR      CALVE
412300970306IF  1C     VC(32)        IFEQ      'S'                                          *VOCE DA CALCOLARE
412400970314     C     TASRCA        ANDGT     *ZEROS
412500970306     C                   Z-ADD     32            I
412600970306     C                   Z-ADD     TASRCA        VIMP(I)
412700970306E   1C                   ENDIF
412800970306     C*
412900970306     C                   ENDSR
413000970306     C*------------------------------------------------------------------------*
413100970306     C* CALVE - CALCOLA CONSEGNA PARTICOLARE ASSICURAZIONE R.C.V.
413200970306     C*------------------------------------------------------------------------*
413300970306     C     CALVE         BEGSR
413400980804     C*
413500980804     C* REIMPOSTA LE VARIABILI DI LAVORO
413600980804     C                   MOVEL     'N'           WCAR                           *NO UTILIZZO TAR.CAR
413700980506     C*
413800980506     C* CERCA TARIFFA PARTICOLARE
413900980506     C                   MOVEL     'R'           TASTC             1            *CODICE CONSEGNA
414000980506     C                   EXSR      CERTPT
414100970306     C*
414200980506     C* CALCOLA RPV, ARROTONDAMENTI E PESO TASSABILE
414300980506IF  1C     $TPT          IFEQ      '1'                                          *TARIFFA TROVATA
414400980707     C                   MOVEL     TPTTPG        WTPG
414500980707     C                   MOVEL     TPTRPV        WRPV
414600980707     C                   CLEAR                   WDET
414700980707     C                   Z-ADD     TPTARL        WARL
414800980707     C                   Z-ADD     TPTARF        WARF
414900980707     C                   Z-ADD     TPTARO        WARO
415000980506     C                   EXSR      CALCOL
415100980506IF  2C     TPTTPG        IFEQ      'V'                                          *A VALORE
415200040716IF  3C     TPTVLM        IFGT      §gelrp
415300040716     C     TPTVLM        SUB       §gelrp        ISOPES
415400980506     C     ISOPES        MULT      SPSC(S)       ISOPES
415500980506E   3C                   ENDIF
415600980506E   2C                   ENDIF
415700980703IF  2C     TPTTPG        IFEQ      'T'                                          *A TRASPORTO
415800980703     C                   Z-ADD     TOTTRA        ISOPES                         *TRASPORTO
415900980703E   2C                   ENDIF
416000970306     C*
416100970306     C* CALCOLA LA TARIFFA ASSICURAZIONE R.C.V.
416200980506     C                   Z-ADD     ISOPES        TPDPES                         *PESO
416300970306     C                   EXSR      CALACD
416400980706     C                   EXSR      CALTRA                                       *CALCOLA % A TRASPOR
416500970306     C*---
416600970306     C* TIPO APPLICAZIONE TARIFFA:
416700980506     C* PARTENZA -> RICAVO SOLO PER FRANCHI
416800980506     C* ARRIVO   -> RICAVO SOLO PER ASSEGNATI
416900980506     C* BLANKS   -> RICAVO PER ENTRAMBI
417000970306     C*---
417100980506IF  2C     WTPO          IFEQ      'F'
417200980506IF  3C     TPTAPL        IFEQ      *BLANKS
417300980506     C     TPTAPL        OREQ      'P'
417400980506     C                   Z-ADD     VARIA         TASRCF                         *RICAVO FRANCO
417500980506E   3C                   ENDIF
417600980506X   2C                   ELSE
417700980506IF  3C     TPTAPL        IFEQ      *BLANKS
417800980506     C     TPTAPL        OREQ      'A'
417900980506     C                   Z-ADD     VARIA         TASRCA                         *RICAVO ASSEGNAO
418000980506E   3C                   ENDIF
418100980506E   2C                   ENDIF
418200980506E   1C                   ENDIF
418300970306     C*
418400970306     C                   ENDSR
418500001227     C*------------------------------------------------------------------------*
418600001227     C* CAL049 - CALCOLA ADG SU ASSIC RCV  FRANCO
418700001227     C*------------------------------------------------------------------------*
418800001227     C     CAL049        BEGSR
418900001227     C*
419000001227     C* REIMPOSTA LE VARIABILI DI LAVORO
419100011031     C                   Z-ADD     *ZEROS        TASADG           21 8
419200001227     C*
419300001227     C* CALCOLA L'A.D.G.
419400011031     C                   Z-ADD     VIMP(10)      WIMP             23 8
419500001227     C                   EXSR      ADGCAL
419600001227     C*
419700001227IF  1C     TASADG        IFGT      *ZEROS
419800001227     C                   Z-ADD     49            I
419900001227     C                   Z-ADD     TASADG        VIMP(I)
420000001227E   1C                   ENDIF
420100001227     C*
420200001227     C                   ENDSR
420300001227     C*------------------------------------------------------------------------*
420400001227     C* CAL066 - CALCOLA ISTAT SU ADG/ASS RCV  FRANCO
420500001227     C*------------------------------------------------------------------------*
420600001227     C     CAL066        BEGSR
420700001227     C*
420800001227     C* REIMPOSTA LE VARIABILI DI LAVORO
420900011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
421000001227     C*
421100001227     C* CALCOLA L'ISTAT
421200011031     C                   Z-ADD     VIMP(49)      WIMP             23 8
421300001227     C                   EXSR      IACCAL
421400001227     C*
421500001227IF  1C     TASIAC        IFGT      *ZEROS
421600001227     C                   Z-ADD     66            I
421700001227     C                   Z-ADD     TASIAC        VIMP(I)
421800001227E   1C                   ENDIF
421900001227     C*
422000001227     C                   ENDSR
422100001227     C*------------------------------------------------------------------------*
422200001227     C* CAL061 - CALCOLA ADG SU ASSIC RCV  ASSEGNATO
422300001227     C*------------------------------------------------------------------------*
422400001227     C     CAL061        BEGSR
422500001227     C*
422600001227     C* REIMPOSTA LE VARIABILI DI LAVORO
422700011031     C                   Z-ADD     *ZEROS        TASADG           21 8
422800001227     C*
422900001227     C* CALCOLA L'A.D.G.
423000011031     C                   Z-ADD     VIMP(32)      WIMP             23 8
423100001227     C                   EXSR      ADGCAL
423200001227     C*
423300001227IF  1C     TASADG        IFGT      *ZEROS
423400001227     C                   Z-ADD     61            I
423500001227     C                   Z-ADD     TASADG        VIMP(I)
423600001227E   1C                   ENDIF
423700001227     C*
423800001227     C                   ENDSR
423900001227     C*------------------------------------------------------------------------*
424000001227     C* CAL078 - CALCOLA ISTAT SU ADG/ASS RCV  ASSEGNATO
424100001227     C*------------------------------------------------------------------------*
424200001227     C     CAL078        BEGSR
424300001227     C*
424400001227     C* REIMPOSTA LE VARIABILI DI LAVORO
424500011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
424600001227     C*
424700001227     C* CALCOLA L'ISTAT
424800011031     C                   Z-ADD     VIMP(61)      WIMP             23 8
424900001227     C                   EXSR      IACCAL
425000001227     C*
425100001227IF  1C     TASIAC        IFGT      *ZEROS
425200001227     C                   Z-ADD     78            I
425300001227     C                   Z-ADD     TASIAC        VIMP(I)
425400001227E   1C                   ENDIF
425500001227     C*
425600001227     C                   ENDSR
425700970306     C*------------------------------------------------------------------------*
425800970306     C* CALCPA - CALCOLA CONSEGNA PARTICOLARE SU APPUNTAMENTO
425900970306     C*------------------------------------------------------------------------*
426000970306     C     CALCPA        BEGSR
426100970306     C*
426200970306     C* REIMPOSTA LE VARIABILI DI LAVORO
426300980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
426400011031     C                   Z-ADD     *ZEROS        TASCPA           21 8
426500970306     C*
426600970306     C* CALCOLA CONSEGNA PARTICOLARE
426700970306     C                   MOVEL     'A'           TASTC             1            *CODICE CONSEGNA
426800011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
426900970306     C                   EXSR      CALCP
427000970306     C                   Z-ADD     TASCP         TASCPA
427100970306     C*
427200970314IF  1C     TASCPA        IFGT      *ZEROS
427300970306     C                   Z-ADD     11            I
427400970306     C                   Z-ADD     TASCPA        VIMP(I)
427500970314E   1C                   ENDIF
427600970306     C*
427700970306     C                   ENDSR
427800001227     C*------------------------------------------------------------------------*
427900001227     C* CAL050 - CALCOLA ADG SU CONSEGNA APPUNTAMENTO
428000001227     C*------------------------------------------------------------------------*
428100001227     C     CAL050        BEGSR
428200001227     C*
428300001227     C* REIMPOSTA LE VARIABILI DI LAVORO
428400011031     C                   Z-ADD     *ZEROS        TASADG           21 8
428500001227     C*
428600001227     C* CALCOLA L'A.D.G.
428700011031     C                   Z-ADD     VIMP(11)      WIMP             23 8
428800001227     C                   EXSR      ADGCAL
428900001227     C*
429000001227IF  1C     TASADG        IFGT      *ZEROS
429100001227     C                   Z-ADD     50            I
429200001227     C                   Z-ADD     TASADG        VIMP(I)
429300001227E   1C                   ENDIF
429400001227     C*
429500001227     C                   ENDSR
429600001227     C*------------------------------------------------------------------------*
429700001227     C* CAL067 - CALCOLA ISTAT SU ADG/CONSEGNA APPUNTAMENTO
429800001227     C*------------------------------------------------------------------------*
429900001227     C     CAL067        BEGSR
430000001227     C*
430100001227     C* REIMPOSTA LE VARIABILI DI LAVORO
430200011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
430300001227     C*
430400001227     C* CALCOLA L'ISTAT
430500011031     C                   Z-ADD     VIMP(50)      WIMP             23 8
430600001227     C                   EXSR      IACCAL
430700001227     C*
430800001227IF  1C     TASIAC        IFGT      *ZEROS
430900001227     C                   Z-ADD     67            I
431000001227     C                   Z-ADD     TASIAC        VIMP(I)
431100001227E   1C                   ENDIF
431200001227     C*
431300001227     C                   ENDSR
431400970306     C*------------------------------------------------------------------------*
431500970306     C* CALCPS - CALCOLA CONSEGNA PARTICOLARE AI SUPERMERCATI
431600970306     C*------------------------------------------------------------------------*
431700970306     C     CALCPS        BEGSR
431800970306     C*
431900970306     C* REIMPOSTA LE VARIABILI DI LAVORO
432000980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
432100011031     C                   Z-ADD     *ZEROS        TASCPS           21 8
432200970306     C*
432300970306     C* CALCOLA CONSEGNA PARTICOLARE
432400970306     C                   MOVEL     'S'           TASTC             1            *CODICE CONSEGNA
432500011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
432600980804     C                   EXSR      CALCP
432700970306     C                   Z-ADD     TASCP         TASCPS
432800970306     C*
432900970314IF  1C     TASCPS        IFGT      *ZEROS
433000970306     C                   Z-ADD     12            I
433100970306     C                   Z-ADD     TASCPS        VIMP(I)
433200970314E   1C                   ENDIF
433300970306     C*
433400970306     C                   ENDSR
433500001227     C*------------------------------------------------------------------------*
433600001227     C* CAL051 - CALCOLA ADG SU CONSEGNA SUPERMERCATI
433700001227     C*------------------------------------------------------------------------*
433800001227     C     CAL051        BEGSR
433900001227     C*
434000001227     C* REIMPOSTA LE VARIABILI DI LAVORO
434100011031     C                   Z-ADD     *ZEROS        TASADG           21 8
434200001227     C*
434300001227     C* CALCOLA L'A.D.G.
434400011031     C                   Z-ADD     VIMP(12)      WIMP             23 8
434500001227     C                   EXSR      ADGCAL
434600001227     C*
434700001227IF  1C     TASADG        IFGT      *ZEROS
434800001227     C                   Z-ADD     51            I
434900001227     C                   Z-ADD     TASADG        VIMP(I)
435000001227E   1C                   ENDIF
435100001227     C*
435200001227     C                   ENDSR
435300001227     C*------------------------------------------------------------------------*
435400001227     C* CAL068 - CALCOLA ISTAT SU ADG/CONSEGNA SUPERMERCATI
435500001227     C*------------------------------------------------------------------------*
435600001227     C     CAL068        BEGSR
435700001227     C*
435800001227     C* REIMPOSTA LE VARIABILI DI LAVORO
435900011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
436000001227     C*
436100001227     C* CALCOLA L'ISTAT
436200011031     C                   Z-ADD     VIMP(51)      WIMP             23 8
436300001227     C                   EXSR      IACCAL
436400001227     C*
436500001227IF  1C     TASIAC        IFGT      *ZEROS
436600001227     C                   Z-ADD     68            I
436700001227     C                   Z-ADD     TASIAC        VIMP(I)
436800001227E   1C                   ENDIF
436900001227     C*
437000001227     C                   ENDSR
437100970306     C*------------------------------------------------------------------------*
437200970306     C* CALCPP - CALCOLA CONSEGNA PARTICOLARE AI PIANI
437300970306     C*------------------------------------------------------------------------*
437400970306     C     CALCPP        BEGSR
437500970306     C*
437600970306     C* REIMPOSTA LE VARIABILI DI LAVORO
437700980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
437800011031     C                   Z-ADD     *ZEROS        TASCPP           21 8
437900970306     C*
438000970306     C* CALCOLA CONSEGNA PARTICOLARE
438100970306     C                   MOVEL     'P'           TASTC             1            *CODICE CONSEGNA
438200011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
438300970306     C                   EXSR      CALCP
438400970306     C                   Z-ADD     TASCP         TASCPP
438500970306     C*
438600970314IF  1C     TASCPP        IFGT      *ZEROS
438700970306     C                   Z-ADD     13            I
438800970306     C                   Z-ADD     TASCPP        VIMP(I)
438900970314E   1C                   ENDIF
439000970306     C*
439100970306     C                   ENDSR
439200001227     C*------------------------------------------------------------------------*
439300001227     C* CAL052 - CALCOLA ADG SU CONSEGNA AI PIANI
439400001227     C*------------------------------------------------------------------------*
439500001227     C     CAL052        BEGSR
439600001227     C*
439700001227     C* REIMPOSTA LE VARIABILI DI LAVORO
439800011031     C                   Z-ADD     *ZEROS        TASADG           21 8
439900001227     C*
440000001227     C* CALCOLA L'A.D.G.
440100011031     C                   Z-ADD     VIMP(13)      WIMP             23 8
440200001227     C                   EXSR      ADGCAL
440300001227     C*
440400001227IF  1C     TASADG        IFGT      *ZEROS
440500001227     C                   Z-ADD     52            I
440600001227     C                   Z-ADD     TASADG        VIMP(I)
440700001227E   1C                   ENDIF
440800001227     C*
440900001227     C                   ENDSR
441000001227     C*------------------------------------------------------------------------*
441100001227     C* CAL069 - CALCOLA ISTAT SU ADG/CONSEGNA AI PIANI
441200001227     C*------------------------------------------------------------------------*
441300001227     C     CAL069        BEGSR
441400001227     C*
441500001227     C* REIMPOSTA LE VARIABILI DI LAVORO
441600011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
441700001227     C*
441800001227     C* CALCOLA L'ISTAT
441900011031     C                   Z-ADD     VIMP(52)      WIMP             23 8
442000001227     C                   EXSR      IACCAL
442100001227     C*
442200001227IF  1C     TASIAC        IFGT      *ZEROS
442300001227     C                   Z-ADD     69            I
442400001227     C                   Z-ADD     TASIAC        VIMP(I)
442500001227E   1C                   ENDIF
442600001227     C*
442700001227     C                   ENDSR
442800040826     C*------------------------------------------------------------------------*
442900040826     C* CALdes - CALCOLA CONSEGNA destinatario disagiato
443000040826     C*------------------------------------------------------------------------*
443100040826     C     CALdes        BEGSR
443200040826     C*
443300040826     C* REIMPOSTA LE VARIABILI DI LAVORO
443400040826     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
443500040826     C                   Z-ADD     *ZEROS        TASdes           21 8
443600040826     C*
443700040826     C* CALCOLA CONSEGNA PARTICOLARE
443800040826     C                   MOVEL     'X'           TASTC             1            *CODICE CONSEGNA
443900040826     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
444000040826     C                   EXSR      CALCP
444100040826     C                   Z-ADD     TASCP         TASdes
444200040826     C*
444300040826IF  1C     TASdes        IFGT      *ZEROS
444400040826     C                   Z-ADD     96            I
444500040826     C                   Z-ADD     TASdes        VIMP(I)
444600040826E   1C                   ENDIF
444700040826     C*
444800040826     C                   ENDSR
444900040826     C*------------------------------------------------------------------------*
445000040826     C* CAL097 - CALCOLA ADG SU CONSEGNA destinatario disagiato
445100040826     C*------------------------------------------------------------------------*
445200040826     C     CAL097        BEGSR
445300040826     C*
445400040826     C* REIMPOSTA LE VARIABILI DI LAVORO
445500040826     C                   Z-ADD     *ZEROS        TASADG
445600040826     C*
445700040826     C* CALCOLA L'A.D.G.
445800040826     C                   Z-ADD     VIMP(96)      WIMP
445900040826     C                   EXSR      ADGCAL
446000040826     C*
446100040826IF  1C     TASADG        IFGT      *ZEROS
446200040826     C                   Z-ADD     97            I
446300040826     C                   Z-ADD     TASADG        VIMP(I)
446400040826E   1C                   ENDIF
446500040826     C*
446600040826     C                   ENDSR
446700040826     C*------------------------------------------------------------------------*
446800040826     C* CAL098 - CALCOLA ISTAT SU ADG/CONSEGNA destinatario disagiato
446900040826     C*------------------------------------------------------------------------*
447000040826     C     CAL098        BEGSR
447100040826     C*
447200040826     C* REIMPOSTA LE VARIABILI DI LAVORO
447300040826     C                   Z-ADD     *ZEROS        TASIAC
447400040826     C*
447500040826     C* CALCOLA L'ISTAT
447600040826     C                   Z-ADD     VIMP(97)      WIMP
447700040826     C                   EXSR      IACCAL
447800040826     C*
447900040826IF  1C     TASIAC        IFGT      *ZEROS
448000040826     C                   Z-ADD     98            I
448100040826     C                   Z-ADD     TASIAC        VIMP(I)
448200040826E   1C                   ENDIF
448300040826     C*
448400040826     C                   ENDSR
448500040826     C*------------------------------------------------------------------------*
448600040826     C* CALban - CALCOLA bancali
448700040826     C*------------------------------------------------------------------------*
448800040826     C     CALban        BEGSR
448900040826     C*
449000040826     C* REIMPOSTA LE VARIABILI DI LAVORO
449100040826     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
449200040826     C                   Z-ADD     *ZEROS        TASban           21 8
449300040826     C*
449400040826     C* CALCOLA CONSEGNA PARTICOLARE
449500040826     C                   MOVEL     '='           TASTC             1            *CODICE CONSEGNA
449600040826     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
449700040826     C                   Z-ADD     ETCnba        TOTqta           21 8          *VALORE CONTRASS.
449800040826     C                   EXSR      CALCP
449900040826     C                   Z-ADD     *ZEROS        TOTqta
450000040826     C                   Z-ADD     TASCP         TASban
450100040826     C*
450200040826IF  1C     TASban        IFGT      *ZEROS
450300040826     C                   Z-ADD     99            I
450400040826     C                   Z-ADD     TASban        VIMP(I)
450500040826E   1C                   ENDIF
450600040826     C*
450700040826     C                   ENDSR
450800040826     C*------------------------------------------------------------------------*
450900040826     C* CAL100 - CALCOLA ADG SU bancali
451000040826     C*------------------------------------------------------------------------*
451100040826     C     CAL100        BEGSR
451200040826     C*
451300040826     C* REIMPOSTA LE VARIABILI DI LAVORO
451400040826     C                   Z-ADD     *ZEROS        TASADG
451500040826     C*
451600040826     C* CALCOLA L'A.D.G.
451700040826     C                   Z-ADD     VIMP(99)      WIMP
451800040826     C                   EXSR      ADGCAL
451900040826     C*
452000040826IF  1C     TASADG        IFGT      *ZEROS
452100040826     C                   Z-ADD     100           I
452200040826     C                   Z-ADD     TASADG        VIMP(I)
452300040826E   1C                   ENDIF
452400040826     C*
452500040826     C                   ENDSR
452600040826     C*------------------------------------------------------------------------*
452700040826     C* CAL101 - CALCOLA ISTAT SU ADG bancali
452800040826     C*------------------------------------------------------------------------*
452900040826     C     CAL101        BEGSR
453000040826     C*
453100040826     C* REIMPOSTA LE VARIABILI DI LAVORO
453200040826     C                   Z-ADD     *ZEROS        TASIAC
453300040826     C*
453400040826     C* CALCOLA L'ISTAT
453500040826     C                   Z-ADD     VIMP(100)     WIMP
453600040826     C                   EXSR      IACCAL
453700040826     C*
453800040826IF  1C     TASIAC        IFGT      *ZEROS
453900040826     C                   Z-ADD     101           I
454000040826     C                   Z-ADD     TASIAC        VIMP(I)
454100040826E   1C                   ENDIF
454200040826     C*
454300040826     C                   ENDSR
454400970306     C*------------------------------------------------------------------------*
454500970306     C* CALCPF - CALCOLA CONSEGNA PARTICOLARE FUORI MISURA
454600970306     C*------------------------------------------------------------------------*
454700970306     C     CALCPF        BEGSR
454800970306     C*
454900970306     C* REIMPOSTA LE VARIABILI DI LAVORO
455000980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
455100011031     C                   Z-ADD     *ZEROS        TASCPF           21 8
455200970306     C*
455300970306     C* CALCOLA CONSEGNA PARTICOLARE
455400970306     C                   MOVEL     'F'           TASTC             1            *CODICE CONSEGNA
455500011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
455600970306     C                   EXSR      CALCP
455700970306     C                   Z-ADD     TASCP         TASCPF
455800970306     C*
455900970314IF  1C     TASCPF        IFGT      *ZEROS
456000970306     C                   Z-ADD     14            I
456100970306     C                   Z-ADD     TASCPF        VIMP(I)
456200970314E   1C                   ENDIF
456300970306     C*
456400970306     C                   ENDSR
456500001227     C*------------------------------------------------------------------------*
456600001227     C* CAL053 - CALCOLA ADG SU CONSEGNA FUORI MISURA
456700001227     C*------------------------------------------------------------------------*
456800001227     C     CAL053        BEGSR
456900001227     C*
457000001227     C* REIMPOSTA LE VARIABILI DI LAVORO
457100011031     C                   Z-ADD     *ZEROS        TASADG           21 8
457200001227     C*
457300001227     C* CALCOLA L'A.D.G.
457400011031     C                   Z-ADD     VIMP(14)      WIMP             23 8
457500001227     C                   EXSR      ADGCAL
457600001227     C*
457700001227IF  1C     TASADG        IFGT      *ZEROS
457800001227     C                   Z-ADD     53            I
457900001227     C                   Z-ADD     TASADG        VIMP(I)
458000001227E   1C                   ENDIF
458100001227     C*
458200001227     C                   ENDSR
458300001227     C*------------------------------------------------------------------------*
458400001227     C* CAL070 - CALCOLA ISTAT SU ADG/CONSEGNA FUORI MISURA
458500001227     C*------------------------------------------------------------------------*
458600001227     C     CAL070        BEGSR
458700001227     C*
458800001227     C* REIMPOSTA LE VARIABILI DI LAVORO
458900011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
459000001227     C*
459100001227     C* CALCOLA L'ISTAT
459200011031     C                   Z-ADD     VIMP(53)      WIMP             23 8
459300001227     C                   EXSR      IACCAL
459400001227     C*
459500001227IF  1C     TASIAC        IFGT      *ZEROS
459600001227     C                   Z-ADD     70            I
459700001227     C                   Z-ADD     TASIAC        VIMP(I)
459800001227E   1C                   ENDIF
459900001227     C*
460000001227     C                   ENDSR
460100970414     C*------------------------------------------------------------------------*
460200970414     C* CALCPD - CALCOLA CONSEGNA PARTICOLARE DDT
460300970414     C*------------------------------------------------------------------------*
460400970414     C     CALCPD        BEGSR
460500970414     C*
460600970414     C* REIMPOSTA LE VARIABILI DI LAVORO
460700980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
460800011031     C                   Z-ADD     *ZEROS        TASCPD           21 8
460900970414     C*
461000970414     C* CALCOLA CONSEGNA PARTICOLARE
461100970414     C                   MOVEL     'B'           TASTC             1            *CODICE CONSEGNA
461200011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
461300970414     C                   EXSR      CALCP
461400970414     C                   Z-ADD     TASCP         TASCPD
461500970414     C*
461600970414IF  1C     TASCPD        IFGT      *ZEROS
461700970414     C                   Z-ADD     33            I
461800970414     C                   Z-ADD     TASCPD        VIMP(I)
461900970414E   1C                   ENDIF
462000970414     C*
462100970414     C                   ENDSR
462200001227     C*------------------------------------------------------------------------*
462300001227     C* CAL062 - CALCOLA ADG SU CONSEGNA DDT
462400001227     C*------------------------------------------------------------------------*
462500001227     C     CAL062        BEGSR
462600001227     C*
462700001227     C* REIMPOSTA LE VARIABILI DI LAVORO
462800011031     C                   Z-ADD     *ZEROS        TASADG           21 8
462900001227     C*
463000001227     C* CALCOLA L'A.D.G.
463100011031     C                   Z-ADD     VIMP(33)      WIMP             23 8
463200001227     C                   EXSR      ADGCAL
463300001227     C*
463400001227IF  1C     TASADG        IFGT      *ZEROS
463500001227     C                   Z-ADD     62            I
463600001227     C                   Z-ADD     TASADG        VIMP(I)
463700001227E   1C                   ENDIF
463800001227     C*
463900001227     C                   ENDSR
464000001227     C*------------------------------------------------------------------------*
464100001227     C* CAL079 - CALCOLA ISTAT SU ADG/CONSEGNA DDT
464200001227     C*------------------------------------------------------------------------*
464300001227     C     CAL079        BEGSR
464400001227     C*
464500001227     C* REIMPOSTA LE VARIABILI DI LAVORO
464600011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
464700001227     C*
464800001227     C* CALCOLA L'ISTAT
464900011031     C                   Z-ADD     VIMP(62)      WIMP             23 8
465000001227     C                   EXSR      IACCAL
465100001227     C*
465200001227IF  1C     TASIAC        IFGT      *ZEROS
465300001227     C                   Z-ADD     79            I
465400001227     C                   Z-ADD     TASIAC        VIMP(I)
465500001227E   1C                   ENDIF
465600001227     C*
465700001227     C                   ENDSR
465800980706     C*------------------------------------------------------------------------*
465900980706     C* CALRIT - CALCOLA IL RITIRO
466000980706     C*------------------------------------------------------------------------*
466100980706     C     CALRIT        BEGSR
466200980706     C*
466300980706     C* REIMPOSTA LE VARIABILI DI LAVORO
466400980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
466500011031     C                   Z-ADD     *ZEROS        TASRIT           21 8
466600980706     C*
466700980706     C* CALCOLA CONSEGNA PARTICOLARE
466800980706     C                   MOVEL     'U'           TASTC             1            *CODICE CONSEGNA
466900011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
467000980706     C*
467100980706     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
467200980706     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
467300980706     C                   EXSR      CALCP
467400980706     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
467500980706     C                   Z-ADD     TASCP         TASRIT
467600980706     C*
467700980706IF  1C     TASRIT        IFGT      *ZEROS
467800980706     C                   Z-ADD     17            I
467900980706     C                   Z-ADD     TASRIT        VIMP(I)
468000980706E   1C                   ENDIF
468100980706     C*
468200980706     C                   ENDSR
468300001227     C*------------------------------------------------------------------------*
468400001227     C* CAL056 - CALCOLA ADG SU RITIRO
468500001227     C*------------------------------------------------------------------------*
468600001227     C     CAL056        BEGSR
468700001227     C*
468800001227     C* REIMPOSTA LE VARIABILI DI LAVORO
468900011031     C                   Z-ADD     *ZEROS        TASADG           21 8
469000001227     C*
469100001227     C* CALCOLA L'A.D.G.
469200011031     C                   Z-ADD     VIMP(17)      WIMP             23 8
469300001227     C                   EXSR      ADGCAL
469400001227     C*
469500001227IF  1C     TASADG        IFGT      *ZEROS
469600001227     C                   Z-ADD     56            I
469700001227     C                   Z-ADD     TASADG        VIMP(I)
469800001227E   1C                   ENDIF
469900001227     C*
470000001227     C                   ENDSR
470100001227     C*------------------------------------------------------------------------*
470200001227     C* CAL073 - CALCOLA ISTAT SU ADG/RITIRO
470300001227     C*------------------------------------------------------------------------*
470400001227     C     CAL073        BEGSR
470500001227     C*
470600001227     C* REIMPOSTA LE VARIABILI DI LAVORO
470700011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
470800001227     C*
470900001227     C* CALCOLA L'ISTAT
471000011031     C                   Z-ADD     VIMP(56)      WIMP             23 8
471100001227     C                   EXSR      IACCAL
471200001227     C*
471300001227IF  1C     TASIAC        IFGT      *ZEROS
471400001227     C                   Z-ADD     73            I
471500001227     C                   Z-ADD     TASIAC        VIMP(I)
471600001227E   1C                   ENDIF
471700001227     C*
471800001227     C                   ENDSR
471900980706     C*------------------------------------------------------------------------*
472000980706     C* CALPAM - CALCOLA LA PROVVIGIONE ASSEGNO MITTENTE
472100980706     C*------------------------------------------------------------------------*
472200980706     C     CALPAM        BEGSR
472300980706     C*
472400980706     C* REIMPOSTA LE VARIABILI DI LAVORO
472500980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
472600011031     C                   Z-ADD     *ZEROS        TASPAM           21 8
472700980706     C*
472800980706     C* CALCOLA CONSEGNA PARTICOLARE
472900980706     C                   MOVEL     'W'           TASTC             1            *CODICE CONSEGNA
473000011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
473100011031     C                   Z-ADD     ETCCAS        TOTVAL           21 8          *VALORE CONTRASS.
473200980706     C                   EXSR      CALCP
473300980911     C                   Z-ADD     *ZEROS        TOTVAL
473400980706     C                   Z-ADD     TASCP         TASPAM
473500980706     C*
473600980706IF  1C     TASPAM        IFGT      *ZEROS
473700980706     C                   Z-ADD     15            I
473800980706     C                   Z-ADD     TASPAM        VIMP(I)
473900980706E   1C                   ENDIF
474000980706     C*
474100980706     C                   ENDSR
474200001227     C*------------------------------------------------------------------------*
474300001227     C* CAL054 - CALCOLA ADG SU C/ASSEGNO MITTENTE
474400001227     C*------------------------------------------------------------------------*
474500001227     C     CAL054        BEGSR
474600001227     C*
474700001227     C* REIMPOSTA LE VARIABILI DI LAVORO
474800011031     C                   Z-ADD     *ZEROS        TASADG           21 8
474900001227     C*
475000001227     C* CALCOLA L'A.D.G.
475100011031     C                   Z-ADD     VIMP(15)      WIMP             23 8
475200001227     C                   EXSR      ADGCAL
475300001227     C*
475400001227IF  1C     TASADG        IFGT      *ZEROS
475500001227     C                   Z-ADD     54            I
475600001227     C                   Z-ADD     TASADG        VIMP(I)
475700001227E   1C                   ENDIF
475800001227     C*
475900001227     C                   ENDSR
476000001227     C*------------------------------------------------------------------------*
476100001227     C* CAL071 - CALCOLA ISTAT SU ADG/CASSEGNO MITTENTE
476200001227     C*------------------------------------------------------------------------*
476300001227     C     CAL071        BEGSR
476400001227     C*
476500001227     C* REIMPOSTA LE VARIABILI DI LAVORO
476600011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
476700001227     C*
476800001227     C* CALCOLA L'ISTAT
476900011031     C                   Z-ADD     VIMP(54)      WIMP             23 8
477000001227     C                   EXSR      IACCAL
477100001227     C*
477200001227IF  1C     TASIAC        IFGT      *ZEROS
477300001227     C                   Z-ADD     71            I
477400001227     C                   Z-ADD     TASIAC        VIMP(I)
477500001227E   1C                   ENDIF
477600001227     C*
477700001227     C                   ENDSR
477800980706     C*------------------------------------------------------------------------*
477900980706     C* CALPAB - CALCOLA LA PROVVIGIONE ASSEGNO BARTOLINI
478000980706     C*------------------------------------------------------------------------*
478100980706     C     CALPAB        BEGSR
478200980706     C*
478300980706     C* REIMPOSTA LE VARIABILI DI LAVORO
478400980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
478500011031     C                   Z-ADD     *ZEROS        TASPAB           21 8
478600980706     C*
478700980706     C* CALCOLA CONSEGNA PARTICOLARE
478800980706     C                   MOVEL     'V'           TASTC             1            *CODICE CONSEGNA
478900011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
479000980911     C                   Z-ADD     ETCCAS        TOTVAL                         *VALORE CONTRASS.
479100980706     C                   EXSR      CALCP
479200980911     C                   Z-ADD     *ZEROS        TOTVAL
479300980706     C                   Z-ADD     TASCP         TASPAB
479400980706     C*
479500980706IF  1C     TASPAB        IFGT      *ZEROS
479600980706     C                   Z-ADD     16            I
479700980706     C                   Z-ADD     TASPAB        VIMP(I)
479800980706E   1C                   ENDIF
479900980706     C*
480000980706     C                   ENDSR
480100001227     C*------------------------------------------------------------------------*
480200001227     C* CAL055 - CALCOLA ADG SU C/ASSEGNO BARTOLINI
480300001227     C*------------------------------------------------------------------------*
480400001227     C     CAL055        BEGSR
480500001227     C*
480600001227     C* REIMPOSTA LE VARIABILI DI LAVORO
480700011031     C                   Z-ADD     *ZEROS        TASADG           21 8
480800001227     C*
480900001227     C* CALCOLA L'A.D.G.
481000011031     C                   Z-ADD     VIMP(16)      WIMP             23 8
481100001227     C                   EXSR      ADGCAL
481200001227     C*
481300001227IF  1C     TASADG        IFGT      *ZEROS
481400001227     C                   Z-ADD     55            I
481500001227     C                   Z-ADD     TASADG        VIMP(I)
481600001227E   1C                   ENDIF
481700001227     C*
481800001227     C                   ENDSR
481900001227     C*------------------------------------------------------------------------*
482000001227     C* CAL072 - CALCOLA ISTAT SU ADG/CASSEGNO BARTOLINI
482100001227     C*------------------------------------------------------------------------*
482200001227     C     CAL072        BEGSR
482300001227     C*
482400001227     C* REIMPOSTA LE VARIABILI DI LAVORO
482500011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
482600001227     C*
482700001227     C* CALCOLA L'ISTAT
482800011031     C                   Z-ADD     VIMP(55)      WIMP             23 8
482900001227     C                   EXSR      IACCAL
483000001227     C*
483100001227IF  1C     TASIAC        IFGT      *ZEROS
483200001227     C                   Z-ADD     72            I
483300001227     C                   Z-ADD     TASIAC        VIMP(I)
483400001227E   1C                   ENDIF
483500001227     C*
483600001227     C                   ENDSR
483700980706     C*------------------------------------------------------------------------*
483800980706     C* CALDIF - CALCOLA IL DIRITTO FISSO
483900980706     C*------------------------------------------------------------------------*
484000980706     C     CALDIF        BEGSR
484100980706     C*
484200980706     C* REIMPOSTA LE VARIABILI DI LAVORO
484300980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
484400011031     C                   Z-ADD     *ZEROS        TASDIF           21 8
484500980706     C*
484600980706     C* CALCOLA CONSEGNA PARTICOLARE
484700980706     C                   MOVEL     'H'           TASTC             1            *CODICE CONSEGNA
484800011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
484900980706     C                   EXSR      CALCP
485000980706     C                   Z-ADD     TASCP         TASDIF
485100980706     C*
485200980706IF  1C     TASDIF        IFGT      *ZEROS
485300980706     C                   Z-ADD     2             I
485400980706     C                   Z-ADD     TASDIF        VIMP(I)
485500980706E   1C                   ENDIF
485600980706     C*
485700980706     C                   ENDSR
485800980706     C*------------------------------------------------------------------------*
485900980706     C* CALISD - CALCOLA L'ISTAT SUL DIRITTO FISSO
486000980706     C*------------------------------------------------------------------------*
486100980706     C     CALISD        BEGSR
486200980706     C*
486300980706     C* REIMPOSTA LE VARIABILI DI LAVORO
486400011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
486500980706     C*
486600980706     C* CALCOLA L'ISTAT
486700011031     C                   Z-ADD     TASDIF        WIMP             23 8
486800980706     C                   EXSR      IACCAL
486900980706     C*
487000980706IF  1C     TASIAC        IFGT      *ZEROS
487100980706     C                   Z-ADD     25            I
487200980706     C                   Z-ADD     TASIAC        VIMP(I)
487300980706E   1C                   ENDIF
487400980706     C*
487500980706     C                   ENDSR
487600980706     C*------------------------------------------------------------------------*
487700980706     C* CALIAD - CALCOLA L'ISTAT SULL'ADG DEL DIRITTO FISSO
487800980706     C*------------------------------------------------------------------------*
487900980706     C     CALIAD        BEGSR
488000980706     C*
488100980706     C* REIMPOSTA LE VARIABILI DI LAVORO
488200011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
488300980706     C*
488400980706     C* CALCOLA L'ISTAT
488500011031     C                   Z-ADD     VIMP(22)      WIMP             23 8
488600980706     C                   EXSR      IACCAL
488700980706     C*
488800980706IF  1C     TASIAC        IFGT      *ZEROS
488900980706     C                   Z-ADD     28            I
489000980706     C                   Z-ADD     TASIAC        VIMP(I)
489100980706E   1C                   ENDIF
489200980706     C*
489300980706     C                   ENDSR
489400970306     C*------------------------------------------------------------------------*
489500970306     C* CALCP - CALCOLA CONSEGNA PARTICOLARE GENERICA
489600970306     C*------------------------------------------------------------------------*
489700970306     C     CALCP         BEGSR
489800970306     C*
489900980506     C* CERCA LA TARIFFA PARTICOLARE
490000980506     C                   EXSR      CERTPT
490100980706     C*
490200980706     C* CALCOLA RPV, ARROTONDAMENTI E PESO TASSABILE
490300980706IF  1C     $TPT          IFEQ      '1'                                          *TARIFFA TROVATA
490400980707     C                   MOVEL     TPTTPG        WTPG
490500980707     C                   MOVEL     TPTRPV        WRPV
490600980707     C                   CLEAR                   WDET
490700980707     C                   Z-ADD     TPTARL        WARL
490800980707     C                   Z-ADD     TPTARF        WARF
490900980707     C                   Z-ADD     TPTARO        WARO
491000980706     C                   EXSR      CALCOL
491100040826     c                   select
491200040826IF  2C                   when      tpttpg = 'T'                                 *A TRASPORTO
491300980706     C                   Z-ADD     TOTTRA        ISOPES                         *TRASPORTO
491400040826IF  2C                   when      tpttpg = 'V'                                 *A TRASPORTO
491500980911     C                   Z-ADD     TOTVAL        ISOPES                         *TRASPORTO
491600040826IF  2C                   when      tpttpg = '5'                                 *A TRASPORTO
491700040826     C                   Z-ADD     TOTqta        ISOPES                         *TRASPORTO
491800040826E   2C                   ENDsl
491900980706     C*
492000980706     C* CALCOLA LA CONSEGNA PARTICOLARE
492100970306     C                   Z-ADD     ISOPES        TPDPES                         *PESO
492200970306     C                   EXSR      CALACD
492300980706     C                   EXSR      CALTRA                                       *CALCOLA % A TRASPOR
492400970306     C                   Z-ADD     VARIA         TASCP
492500970721     C*
492600970721     C* MEMORIZZA IL FLAG "APPLICAZIONE TARIFFA INOLTRO" DELLO SCAGLIONE DEL
492700970721     C* CODICE TASSAZIONE DEL DETTAGLIO TARIFFARIO
492800970721     C                   MOVEL     AAIN          DEPAIN
492900980506E   1C                   ENDIF
493000970306     C*
493100970306     C                   ENDSR
493200980506     C*------------------------------------------------------------------------*
493300980506     C* CERTPT - CERCA TARIFFA PARTICOLARE
493400980506     C*------------------------------------------------------------------------*
493500980506     C     CERTPT        BEGSR
493600980506     C*
493700980506     C* REPERISCE IL CODICE PARTICOLARITA'
493800980506     C                   MOVEL     '0'           $TPT                           *TARIFFA NON TROVATA
493900980506     C                   MOVEL     TASTC         KTPFTC                         *CAMPO CHIAVE
494000980506     C                   Z-ADD     1             I
494100980506     C                   SETOFF                                           28
494200990819     C                   MOVEL     KTPFTC        A1                1
494300990819     C     A1            LOOKUP    CP(I)                                  28
494400980506IF  1C     *IN28         IFEQ      *ON                                          *CODICE ESISTENTE
494500980506     C     I             OCCUR     CPIDS                                        *TARIFFA DI CARTELLO
494600980506     C*
494700980506     C* RICERCA LA CONSEGNA PARTICOLARE NELLE TARIFFE DEL CLIENTE
494800980506     C                   Z-ADD     D00KSC        KTPKSC
494900980506     C                   Z-ADD     D00CTR        KTPCTR
495000980506     C                   Z-ADD     D00PRG        KTPPRG
495100990819     C     KEYTPT        SETLL     TITPT01L                               99
495200990819     C     KEYTPT        READE     TITPT01L                               99
495300980506DO  2C     *IN99         DOWEQ     *OFF
495400980506     C     $TPT          ANDEQ     '0'
495500991125     C*
495600991125     C* CONTROLLA SE CONVERTIRE I CAMPI DI TITPT
495700011026IF  3C     D00DIV        IFNE      WDIVCE
495800011019     C                   MOVEL     D00DIV        WDVI
495900991125     C                   EXSR      CNVTPT
496000991125E   3C                   ENDIF
496100991125     C*
496200980506IF  3C     TPTATB        IFEQ      *BLANKS
496300980506     C                   MOVEL     '1'           $TPT                           *TARIFFA TROVATA
496400980506E   3C                   ENDIF
496500980506     C*
496600980506     C* LETTURA SUCESSIVA TARIFFE
496700980506IF  3C     $TPT          IFEQ      '0'                                          *TARIFFA NON TROVATA
496800990819     C     KEYTPT        READE     TITPT01L                               99
496900980506E   3C                   ENDIF
497000980506E   2C                   ENDDO                                                  *FINE TAR.PART.KSC
497100980506     C*
497200980506     C* TARIFFA CONSEGNE PARTICOLARI NON TROVATA NELLE TARIFFE DEL CLIENTE
497300980506     C* RICERCA NELLE TARIFFE DI CARTELLO
497400980506IF  2C     $TPT          IFEQ      '0'                                          *TARIFFA KSC NO TROV
497500980506IF  3C     CP88          IFNE      'NO'                                         *C'E' TARIFFA CARTEL
497600980506     C     WCAR          ANDEQ     'S'                                          *SI UTILIZZO TAR.CAR
497700980506     C                   MOVEL     '1'           $TPT                           *TARIFFA TROVATA
497800980506     C                   Z-ADD     CARKSC        TPTKSC                         *CHIAVE CARTELLO
497900980506     C                   Z-ADD     CARCTR        TPTCTR
498000980506     C                   Z-ADD     CARPRG        TPTPRG
498100980506     C                   MOVEL     KTPFTC        TPTFTC
498200980506     C                   MOVEL     TTPG8P        TPTTPG                         *CAMPI CARTELLO
498300980506     C                   Z-ADD     TARF8P        TPTARF
498400980506     C                   Z-ADD     TARL8P        TPTARL
498500980506     C                   Z-ADD     TARO8P        TPTARO
498600980506     C                   Z-ADD     TRPV8P        TPTRPV
498700980506E   3C                   ENDIF
498800980506E   2C                   ENDIF
498900980506E   1C                   ENDIF
499000980506     C*
499100980506     C                   ENDSR
499200970306     C*------------------------------------------------------------------------*
499300970306     C* CALACD - CALCOLA TARIFFA PARTICOLARE GENERICA
499400970306     C*------------------------------------------------------------------------*
499500970306     C     CALACD        BEGSR
499600980706     C*
499700980706     C* AZZERA VARIABILI DI COMODO -SOLO DI QUESTO PGM-
499800980706     C                   MOVEL     *BLANKS       PTPG              1            *TIPO APPLICAZIONE
499900011031     C                   Z-ADD     *ZEROS        PITR             21 8          *IMPORTO TARIFFA
500000011031     C                   Z-ADD     *ZEROS        PVARI            23 8          *VARIA NON ARROTONDA
500100011031     C                   Z-ADD     *ZEROS        PVARIA           23 8          *VAIRA ARROTONDATA
500200970306     C*
500300970306     C* REIMPOSTA LE VARIABILI DI LAVORO
500400011031     C                   Z-ADD     *ZEROS        VARIA            23 8
500500970306DO  1C     1             DO        10            I
500600970306     C     I             OCCUR     ACDS
500700970306     C                   CLEAR                   ACDS
500800970306E   1C                   ENDDO
500900970306     C*
501000970306     C* IMPOSTA LA CHIAVE PER LA RICERCA SCAGLIONE NEL DETTAGLIO TARIFFARIO
501100970306     C                   Z-ADD     TPTKSC        KDPKSC                         *CLIENTE
501200970306     C                   Z-ADD     TPTCTR        KDPCTR                         *TARIFFA
501300970306     C                   Z-ADD     TPTPRG        KDPPRG                         *PROGRESSIVO
501400970306     C                   MOVEL     TPTFTC        KDPFTC                         *CONSEGNA PARTICOLAR
501500970306     C                   MOVEL     *BLANKS       KDPCTS                         *CODICE TASSAZIONE
501600970306     C*
501700970306     C* PER ALCUNE CONSEGNE PARTICOLARI SONO INDICATI NELLA TABELLA CONSEGNE
501800970306     C* PARTICOLARI I CODICI TASSAZIONE DA UTILIZZARE NELLA RICERCA TARIFFA
501900970306     C                   SETOFF                                           28
502000970306     C                   Z-ADD     1             I
502100970306     C     CCTS(T)       LOOKUP    CTS(I)                                 28
502200970306     C     I             OCCUR     DSCT
502300970306IF  1C     §CTNAR        IFNE      *BLANKS                                      *TARIFFA ESTERO
502400971204     C     DLPFL1        ORNE      *BLANKS                                      *ESTERO
502500970306IF  2C     TCTE8P        IFNE      *BLANKS                                      *SE INDICATO
502600970306     C                   MOVEL     TCTE8P        KDPCTS                         *CODICE 'EE' FISSO
502700970306E   2C                   ENDIF
502800970306X   1C                   ELSE                                                   *TARIFFA ITALIA
502900970306IF  2C     TCTS8P        IFNE      *BLANKS                                      *SE INDICATO
503000970306     C                   MOVEL     TCTS8P        KDPCTS                         *CODICE 'IT' FISSO
503100970306E   2C                   ENDIF
503200970306E   1C                   ENDIF
503300970306     C*
503400970306     C* CODICE TASSAZIONE NON FISSO, RICERCA QUAL'E' PER REPERIRE LA TARIFFA
503500980921     C                   SETOFF                                       98
503600980921IF  1C     KDPCTS        IFNE      *BLANKS
503700980921     C*
503800980921IF  2C     KDPKSC        IFEQ      CARKSC                                       *LEGGERE CARTELLO
503900980921     C     D00CTO        ANDEQ     'O'                                          *IN OFFERTA
504000990819     C     KEYTPD        SETLL     TIOPD02L                               98     DA OFFERTA
504100980921X   2C                   ELSE                                                   *LEGGERE CLIENTE
504200990819     C     KEYTPD        SETLL     TITPD02L                               98     DA TARIFFA
504300980921E   2C                   ENDIF
504400980921     C*
504500980921X   1C                   ELSE                                                   *KDPCTS = BLANKS
504600980921     C*
504700970306     C                   MOVEL     CCTS(T)       KDPCTS                         *CODCE TASSAZIONE
504800970306IF  2C     KDPKSC        IFEQ      CARKSC                                       *LEGGERE CARTELLO
504900970306     C     D00CTO        ANDEQ     'O'                                          *IN OFFERTA
505000990819     C     KEYTPD        SETLL     TIOPD02L                               98     DA OFFERTA
505100970306X   2C                   ELSE                                                   *LEGGERE CLIENTE
505200990819     C     KEYTPD        SETLL     TITPD02L                               98     DA TARIFFA
505300970306E   2C                   ENDIF
505400970306     C*
505500970306     C* CERCA LA TARIFFA CON IL CODICE TASSAZIONE REGIONE
505600980703IF  2C     *IN98         IFEQ      *OFF                                         *TARIFFA NON TROVATA
505700970306     C                   MOVEL     §CTRAP        KDPCTS                         *COD.TASS.REGIONE
505800970306IF  3C     KDPKSC        IFEQ      CARKSC                                       *LEGGERE CARTELLO
505900970306     C     D00CTO        ANDEQ     'O'                                          *IN OFFERTA
506000990819     C     KEYTPD        SETLL     TIOPD02L                               98     DA OFFERTA
506100970306X   3C                   ELSE                                                   *LEGGERE CLIENTE
506200990819     C     KEYTPD        SETLL     TITPD02L                               98     DA TARIFFA
506300970306E   3C                   ENDIF
506400970306     C*
506500970306     C* CERCA LA TARIFFA CON IL CODICE TASSAZIONE NAZIONE
506600000403IF  3C     *IN98         IFEQ      *OFF                                         *TARIFFA NON TROVATA
506700000403IF  4C     §CTNAR        IFEQ      *BLANKS
506800000403     C                   MOVEL     'IT'          KDPCTS                         *COD.TASS.NAZIONE
506900000403X   4C                   ELSE
507000000403     C                   MOVEL     'EE'          KDPCTS                         *COD.TASS.NAZIONE
507100000403E   4C                   ENDIF
507200000403IF  4C     KDPKSC        IFEQ      CARKSC                                       *LEGGERE CARTELLO
507300000403     C     D00CTO        ANDEQ     'O'                                          *IN OFFERTA
507400000403     C     KEYTPD        SETLL     TIOPD02L                               98     DA OFFERTA
507500000403X   4C                   ELSE                                                   *LEGGERE CLIENTE
507600000403     C     KEYTPD        SETLL     TITPD02L                               98     DA TARIFFA
507700000403E   4C                   ENDIF
507800000403E   3C                   ENDIF
507900000403IF  3C*---       *IN98     IFEQ *OFF                       *TARIFFA NON TROVATA
508000000403IF  4C*---       §CTNAR    IFEQ *BLANKS
508100000403IF  5C*---       DLPFL1    IFEQ *BLANKS
508200000403     C*---                 MOVEL'IT'      KDPCTS           *COD.TASS.NAZIONE
508300000403X   5C*---                 ELSE
508400000403     C*---                 MOVEL'EE'      KDPCTS           *COD.TASS.NAZIONE
508500000403E   5C*---                 ENDIF
508600000403IF  5C*---       KDPKSC    IFEQ CARKSC                     *LEGGERE CARTELLO
508700000403     C*---       D00CTO    ANDEQ'O'                        *IN OFFERTA
508800000403     C*---       KEYTPD    SETLLTIOPD02L                 98 DA OFFERTA
508900000403X   5C*---                 ELSE                            *LEGGERE CLIENTE
509000000403     C*---       KEYTPD    SETLLTITPD02L                 98 DA TARIFFA
509100000403E   5C*---                 ENDIF
509200000403E   4C*---                 ENDIF
509300000403E   3C*---                 ENDIF
509400970306E   2C                   ENDIF
509500970306E   1C                   ENDIF
509600000403     C*
509700970306     C* LEGGE LA TARIFFA
509800970306     C                   MOVEL     '0'           $TPD                           *TARIFFA NO ESISTENT
509900970306     C                   Z-ADD     *ZEROS        I
510000970306IF  1C     KDPKSC        IFEQ      CARKSC                                       *LEGGERE CARTELLO
510100970306     C     D00CTO        ANDEQ     'O'                                          *IN OFFERTA
510200990819     C     KEYTPD        READE     TIOPD02L                               98     DA OFFERTA
510300970306X   1C                   ELSE                                                   *LEGGERE CLIENTE
510400990819     C     KEYTPD        READE     TITPD02L                               98     DA TARIFFA
510500970306E   1C                   ENDIF
510600970306DO  1C     *IN98         DOWEQ     *OFF
510700991125     C*
510800991125     C* CONTROLLA SE CONVERTIRE I CAMPI DI TITPD
510900011023     C     KDPKSC        IFEQ      CARKSC                                       *LEGGERE CARTELLO
511000011023     C     D00CTO        ANDEQ     'O'                                          *IN OFFERTA
511100011023     C                   MOVEL     CARDIV        WDIV
511200011023     C                   ELSE
511300011019     C                   MOVEL     D00DIV        WDIV
511400011023     C                   ENDIF
511500011026IF  2C     WDIV          IFNE      WDIVCE
511600011019     C                   MOVEL     WDIV          WDVI
511700991125     C                   EXSR      CNVTPD
511800991125E   2C                   ENDIF
511900991125     C*
512000970306     C                   MOVEL     '1'           $TPD                           *TARIFFA ESISTENTE
512100970306     C                   ADD       1             I
512200970306     C     I             OCCUR     ACDS
512300970306     C                   Z-ADD     TPDSGL        ASGL
512400970306     C                   Z-ADD     TPDITR        AITR
512500970306     C                   Z-ADD     TPDMIN        AMIN
512600970306     C                   Z-ADD     TPDMAX        AMAX
512700970306     C                   MOVEL     TPDAIN        AAIN
512800970306IF  2C     KDPKSC        IFEQ      CARKSC                                       *LEGGERE CARTELLO
512900970306     C     D00CTO        ANDEQ     'O'                                          *IN OFFERTA
513000990819     C     KEYTPD        READE     TIOPD02L                               98     DA OFFERTA
513100970306X   2C                   ELSE                                                   *LEGGERE CLIENTE
513200990819     C     KEYTPD        READE     TITPD02L                               98     DA TARIFFA
513300970306E   2C                   ENDIF
513400970306E   1C                   ENDDO
513500970306     C*
513600970306     C* SE TARIFFA PARTICOLARE ESISTENTE, RICERCA LO SCAGLIONE
513700970306IF  1C     $TPD          IFEQ      '1'
513800970306DO  2C     1             DO        10            I
513900970306     C     I             OCCUR     ACDS
514000970306IF  3C     ASGL          IFGE      TPDPES                                       *SCAGLIONE TROVATO
514100970306     C                   LEAVE
514200970306E   3C                   ENDIF
514300970306E   2C                   ENDDO
514400970306     C*
514500970306     C* SCAGLIONE NON TROVATO, MANCA TARIFFA
514600970306IF  2C     ASGL          IFLT      TPDPES
514700001218     C                   MOVEL     MSG(26)       DMSG26
514800001218     C                   MOVEL     TPTFTC        DM26A
514900001218     C                   MOVEL     DMSG26        $MSG
515000970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
515100970306E   2C                   ENDIF
515200970306     C*
515300970306     C* CALCOLO DEL MINIMO TASSABILE
515400980703IF  2C     WERR          IFEQ      'N'                                          *NO ERRORE
515500980703IF  3C     ASGL          IFLE      MINTAS                                       *SGL<=MIN.TASS.STD.
515600970306     C                   Z-ADD     MINTAS        TPDPES                         *PESO=MIN.TASS.STD.
515700980703E   3C                   ENDIF
515800980703IF  3C     TPDPES        IFLE      MINTAS                                       *PESO<=MIN.TASS.STD.
515900970306     C                   Z-ADD     MINTAS        TPDPES                         *PESO=MIN.TASS.STD.
516000980703E   3C                   ENDIF
516100970306     C*
516200970306     C* CALCOLA IL PESO TASSABILE IN QUINTALI -PER TARIFFA A QUINTALI-
516300011031     C                   Z-ADD     *ZEROS        TPDQLI           23 8
516400980707IF  3C     TPTTPG        IFEQ      '0'
516500980707     C     TPTTPG        OREQ      '4'
516600970306     C     TPDPES        DIV       100           TPDQLI
516700980703X   3C                   ELSE
516800970306     C                   Z-ADD     TPDPES        TPDQLI
516900980703E   3C                   ENDIF
517000970306     C     AITR          MULT(H)   TPDQLI        VARIA
517100980703IF  3C     VARIA         IFGT      *ZEROS
517200980703IF  4C     TPTTPG        IFEQ      'V'                                          *A VALORE O
517300980506     C     TPTTPG        OREQ      'T'                                          *A TRASPORTO
517400970306     C     VARIA         DIV(H)    100           VARIA
517500980703E   4C                   ENDIF
517600980703E   3C                   ENDIF
517700980703E   2C                   ENDIF
517800980706     C*
517900980706     C* IMPOSTA VARIABILI DI COMODO -SOLO DI QUESTO PGM-
518000980706     C                   MOVEL     TPTTPG        PTPG                           *TIPO APPLICAZIONE
518100980706     C                   Z-ADD     TPDITR        PITR                           *IMPORTO TARIFFA
518200980706     C                   Z-ADD     VARIA         PVARI                          *VARIA NON ARROTONDA
518300970306     C* MINIMO TARIFFA
518400980703IF  2C     AMIN          IFGT      VARIA
518500970306     C                   Z-ADD     AMIN          VARIA
518600980703E   2C                   ENDIF
518700970306     C* MASSIMO TARIFFA
518800980703IF  2C     AMAX          IFGT      *ZEROS
518900980703IF  3C     AMAX          IFLT      VARIA
519000970306     C                   Z-ADD     AMAX          VARIA
519100980703E   3C                   ENDIF
519200980703E   2C                   ENDIF
519300980706     C*
519400980707     C* IMPOSTA VARIABILI DI COMODO -SOLO DI QUESTO PGM-
519500980706     C                   Z-ADD     VARIA         PVARIA                         *VARIA ARROTONDATA
519600970306E   1C                   ENDIF
519700970306     C*
519800970306     C                   ENDSR
519900980706     C*------------------------------------------------------------------------*
520000980706     C* CALTRA - CALCOLA CONSEGNA PARTICOLARE SUL TOTALE IMPORTO TRASPORTO
520100980706     C*------------------------------------------------------------------------*
520200980706     C     CALTRA        BEGSR
520300980706     C*---
520400980706     C* INPUT
520500980706     C* . PTPG ->   TIPO APPLICAZIONE
520600980706     C* . PITR ->   IMPORTO TARIFFA CALCOLATA
520700980706     C* . PVARI ->  VARIA CALCOLATA, MA NON ARROTONDATA
520800980706     C* . PVARIA -> VARIA CALCOLATA, E ARROTONDATA
520900980706     C*---
521000980706     C* REIMPOSTA LE VARIABILI DI LAVORO
521100011031     C                   Z-ADD     *ZEROS        TOTVAR           21 8          *VARIA
521200980706     C*
521300980706     C* SE TIPO APPLICAZIONE T' E HA CALCOLATO QUALCOSA
521400980706IF  1C     PTPG          IFEQ      'T'                                          *TARIFFA A TRASPORTO
521500980706     C     PITR          ANDGT     *ZEROS                                       *TARIFFA DA CALCOLAR
521600980706     C* PORTO
521700980706IF  2C     VIMP(1)       IFGT      *ZEROS
521800011031     C                   Z-ADD     VIMP(1)       IMPOR            21 8          *IMPORTO VOCE
521900980706     C                   Z-ADD     VP(1)         PERCEN            9 5          *% INCIDENZA
522000011031     C                   Z-ADD     *ZEROS        VOCE             21 8
522100980706     C                   EXSR      CALTR2
522200980706     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
522300980706E   2C                   ENDIF
522400980706     C* INOLTRO
522500980706IF  2C     VIMP(6)       IFGT      *ZEROS
522600011031     C                   Z-ADD     VIMP(6)       IMPOR            21 8          *IMPORTO VOCE
522700980706     C                   Z-ADD     VP(6)         PERCEN            9 5          *% INCIDENZA
522800011031     C                   Z-ADD     *ZEROS        VOCE             21 8
522900980706     C                   EXSR      CALTR2
523000980706     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
523100980706E   2C                   ENDIF
523200980706     C* LOCALITA' DISAGIATE
523300980706IF  2C     VIMP(35)      IFGT      *ZEROS
523400011031     C                   Z-ADD     VIMP(35)      IMPOR            21 8          *IMPORTO VOCE
523500980706     C                   Z-ADD     VP(35)        PERCEN            9 5          *% INCIDENZA
523600011031     C                   Z-ADD     *ZEROS        VOCE             21 8
523700980706     C                   EXSR      CALTR2
523800980706     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
523900980706E   2C                   ENDIF
524000980706     C* ESPRESSO PORTO
524100980706IF  2C     VIMP(7)       IFGT      *ZEROS
524200011031     C                   Z-ADD     VIMP(7)       IMPOR            21 8          *IMPORTO VOCE
524300980706     C                   Z-ADD     VP(7)         PERCEN            9 5          *% INCIDENZA
524400011031     C                   Z-ADD     *ZEROS        VOCE             21 8
524500980706     C                   EXSR      CALTR2
524600980706     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
524700980706E   2C                   ENDIF
524800980706     C* ESPRESSO INOLTRO
524900980706IF  2C     VIMP(36)      IFGT      *ZEROS
525000011031     C                   Z-ADD     VIMP(36)      IMPOR            21 8          *IMPORTO VOCE
525100980706     C                   Z-ADD     VP(36)        PERCEN            9 5          *% INCIDENZA
525200011031     C                   Z-ADD     *ZEROS        VOCE             21 8
525300980706     C                   EXSR      CALTR2
525400980706     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
525500980706E   2C                   ENDIF
525600980706     C* ESPRESSO LOCALITA' DISAGIATE
525700980706IF  2C     VIMP(39)      IFGT      *ZEROS
525800011031     C                   Z-ADD     VIMP(39)      IMPOR            21 8          *IMPORTO VOCE
525900980706     C                   Z-ADD     VP(39)        PERCEN            9 5          *% INCIDENZA
526000011031     C                   Z-ADD     *ZEROS        VOCE             21 8
526100980706     C                   EXSR      CALTR2
526200980706     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
526300980706E   2C                   ENDIF
526400011030     C* CONSEGNA ISOLA
526500011030IF  2C     VIMP(80)      IFGT      *ZEROS
526600011031     C                   Z-ADD     VIMP(80)      IMPOR            21 8          *IMPORTO VOCE
526700011030     C                   Z-ADD     VP(80)        PERCEN            9 5          *% INCIDENZA
526800011031     C                   Z-ADD     *ZEROS        VOCE             21 8
526900011030     C                   EXSR      CALTR2
527000011030     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
527100011030E   2C                   ENDIF
527200011030     C* ESPRESSO CONSEGNA ISOLA
527300011030IF  2C     VIMP(81)      IFGT      *ZEROS
527400011031     C                   Z-ADD     VIMP(81)      IMPOR            21 8          *IMPORTO VOCE
527500011030     C                   Z-ADD     VP(81)        PERCEN            9 5          *% INCIDENZA
527600011031     C                   Z-ADD     *ZEROS        VOCE             21 8
527700011030     C                   EXSR      CALTR2
527800011030     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
527900011030E   2C                   ENDIF
528000011030     C*
528100980706     C                   Z-ADD     TOTVAR        VARIA                          *VOCE TOTALE
528200980706E   1C                   ENDIF
528300980706     C*
528400980706     C                   ENDSR
528500980706     C*------------------------------------------------------------------------*
528600980706     C* CALTR2 - ESEGUE CALCOLI VOCE CALCOLATA SU UNA % DEL TRASPORTO
528700980706     C*------------------------------------------------------------------------*
528800980706     C     CALTR2        BEGSR
528900980706     C*---
529000980706     C* INPUT
529100980706     C* . PVARI  ->  VARIA CALCOLATA, MA NON ARROTONDATA
529200980706     C* . PVARIA -> VARIA CALCOLATA, E ARROTONDATA
529300980706     C* . IMPOR  -> IMPORTO VOCE
529400980706     C* . PERCEN -> % INCIDENZA
529500980706     C* OUTPUT
529600980706     C* . VOCE   -> VOCE CALCOLATA
529700980706     C*---
529800980706     C* ES. DA CALCOLARE VARIA = 10% DEL TRASPORTO
529900980706     C*   . VARIA CALCOLATA CON ARROTONDAMENTI = 2.000
530000980706     C*   . VARIA CALCOLATA SENZA ARROTONDAMENTI = 1.600
530100980706     C*   . PORTO = 10.000                         INCIDENZA 100%
530200980706     C*   . INOLTRO = 5.000                        INCIDENZA   5%
530300980706     C*   . LOCALITA' DISAGIATA = 1.000            INCIDENZA  20%
530400980706     C*   . ESPRESSO PORTO = 300                   INCIDENZA  15%
530500980706     C*   . ESPRESSO INOLTRO = 150                 INCIDENZA 15% SU  5% = 0,75%
530600980706     C*   . ESPRESSO LOCALITA' DISAGIATA = 30      INCIDENZA 15% SU 20% = 3%
530700011030     C*   . CONSEGNA ISOLA               = 2.500   INCIDENZA  30%
530800011030     C*   . ESPRESSO CONSEGNA ISOLA      = 30      INCIDENZA 15% SU 30% = 4,5%
530900980706     C* LA SOMMA DI TUTTE LE VOCI DA LA VARIA
531000011030     C* Esempio -->
531100980706     C*---
531200980706     C* % "T" DA CALCOLARE SULLA LOCALITA' DISAGIATA
531300980706     C* 10% DI 1.000 = 100
531400980706     C*---
531500011031     C     IMPOR         MULT      PITR          N11              23 8
531600011031     C     N11           DIV       100           VOCPT            23 8          *VOCE % SUL TRASPORT
531700980706     C*---
531800980706     C* VOCE TENUTO CONTO DELL'ARROTONDAMENTO -->
531900980706     C* 100 : 1.600 = X : 2.000
532000980706     C* X = 125
532100980706     C*---
532200980706     C     VOCPT         MULT      PVARIA        VOCPT
532300011031     C     VOCPT         DIV       PVARI         VOCPTA           25 8          *VOCE AL 100%
532400980706     C*---
532500980706     C* VOCE TENUTO CONTO DELLA SUA % DI INCIDENZA ->
532600980706     C* 20% DI 125 = 25
532700980706     C*---
532800011031     C     VOCPTA        MULT      PERCEN        N19              25 8
532900011031     C     N19           DIV(H)    100           VOCE             21 8          *VOCE ALLA %INCIDENZ
533000980706     C*
533100980706     C                   ENDSR
533200970306     C*------------------------------------------------------------------------*
533300970306     C* CALTOT - CALCOLA IMPORTI SULLE % INCIDENZA E I RAGGUPPAMENTI DELTA
533400970306     C*------------------------------------------------------------------------*
533500970306     C     CALTOT        BEGSR
533600970306     C*
533700970306     C* REIMPOSTA LE VARIABILI DI LAVORO
533800970311     C                   Z-ADD     *ZEROS        IRDL                           *RAGGRUPPAMENTI DELT
533900970311     C                   Z-ADD     *ZEROS        VIM                            *IMPORTI DA SCRIVERE
534000011031     C                   Z-ADD     *ZEROS        WVVOCE           25 8          *IMPORTO VOCE
534100970306     C*
534200970311     C* CICLO PER TUTTE LE VOCI DI TASSAZIONE
534300970306DO  1C     1             DO        NVOCA         V                 6 0
534400970311IF  2C     VIMP(V)       IFGT      *ZEROS
534500970306     C*
534600970311     C* APPLICA LA % DI INCIDENZA
534700970306     C                   Z-ADD     *ZEROS        WVVOCE
534800970306IF  3C     VP(V)         IFLT      100
534900970311     C     VIMP(V)       DIV       100           WVVOCE
535000970306     C     WVVOCE        MULT      VP(V)         VIM(V)
535100970311X   3C                   ELSE
535200970311     C                   Z-ADD     VIMP(V)       VIM(V)
535300970306E   3C                   ENDIF
535400970306     C*
535500970306     C* SE LA VOCE E' DA INGLOBARE NEL PORTO FORZA IL RAGGRUPPAMENTO PORTO
535600970306IF  3C     VI(V)         IFEQ      'S'
535700970306     C                   Z-ADD     001           VR(V)
535800970318E   3C                   ENDIF
535900970306     C*
536000970311     C* IMPOSTA IL RAGGRUPPAMENTO DELTA
536100970306     C                   Z-ADD     VR(V)         R                 6 0          *INDICE  RAGGRUPP.TO
536200970306     C                   ADD       VIM(V)        IRDL(R)                        *IMPORTO RAGGRUPP.TO
536300970306E   2C                   ENDIF
536400970306E   1C                   ENDDO                                                  *FINE VOCI
536500970306     C*
536600970306     C                   ENDSR
536700970306     C*------------------------------------------------------------------------*
536800970306     C* SCRETW - SCRIVE SU FILE DI LAVORO I RICAVI
536900970306     C*------------------------------------------------------------------------*
537000970306     C     SCRETW        BEGSR
537100970311     C*
537200970311     C* CICLO PER TUTTI I RAGGRUPPAMENTI DELTA
537300970306DO  1C     1             DO        50            R
537400970306IF  2C     IRDL(R)       IFGT      *ZEROS
537500970306     C     R             OREQ      1
537600980909     C******************** MOVELUSER      KTWUSR           *UTENTE
537700980909     C                   MOVEL     JOB           KTWUSR                         *UTENTE
537800980603     C                   MOVEL     D00DSF        KTWDSF                         *DATI SEDE O FILIALE
537900980603     C                   MOVEL     D00CTO        KTWCTO                         *TARIFFA O OFFERTA
538000970306     C                   Z-ADD     D00KSC        KTWKSC                         *CLIENTE O VISITA
538100970306     C                   Z-ADD     D00CTR        KTWCTR                         *TARIFFA
538200970306     C                   Z-ADD     D00PRG        KTWPRG                         *PROGRESSIVO
538300970306     C                   Z-ADD     SSGL(S)       KTWSGL                         *SCAGLIONE
538400970306     C                   MOVEL     CCTS(T)       KTWCTS                         *CODICE TASSAZIONE
538500970306     C                   Z-ADD     R             KTWRDL                         *RAGGRUP.DELTA
538600010926     C     KEYETW        CHAIN     TEETW01L                           99
538700970306     C* MODIFICA
538800970306IF  3C     *IN99         IFEQ      *OFF
538900970306     C                   ADD       IRDL(R)       ETWRIC                         *RICAVO
539000970317IF  4C     D00TLA        IFEQ      'D'                                          *LANCIO DELTA
539100970317     C                   Z-ADD     *ZEROS        ETWCOM                         *AZZERA COMPETENZE
539200970317E   4C                   ENDIF
539300010926     C                   UPDATE    TEETW000
539400970306     C* INSERIMENTO
539500970306X   3C                   ELSE
539600970306     C                   MOVEL     KTWUSR        ETWUSR                         *UTENTE
539700980603     C                   MOVEL     KTWDSF        ETWDSF                         *DATI SEDE O FILIALE
539800970306     C                   MOVEL     KTWCTO        ETWCTO                         *TARIFFA O OFFERTA
539900970306     C                   Z-ADD     KTWKSC        ETWKSC                         *CLIENTE O VISITA
540000970306     C                   Z-ADD     KTWCTR        ETWCTR                         *TARIFFA
540100970306     C                   Z-ADD     KTWPRG        ETWPRG                         *PROGRESSIVO
540200970306     C                   Z-ADD     KTWSGL        ETWSGL                         *SCAGLIONE
540300970306     C                   MOVEL     KTWCTS        ETWCTS                         *CODICE TASSAZIONE
540400970306     C                   Z-ADD     KTWRDL        ETWRDL                         *RAGGRUP.DELTA
540500970306     C                   Z-ADD     IRDL(R)       ETWRIC                         *RICAVO
540600011031     C     CPPE(T)       DIV       100           WPERC            21 8
540700970306     C     WPERC         MULT      SPPE(S)       ETWPPE                         *% INCIDENZA
540800970306     C                   MOVEL     CRCT(T)       ETWRCT                         *RAGGRUPP.COD.TASSAZ
540900970306     C                   MOVEL     CRAP(T)       ETWRAP                         *REGIONE CODICE
541000970306     C                   MOVEL     CNAR(T)       ETWNAR                         *NAZIONE CODICE
541100970306     C                   Z-ADD     *ZEROS        ETWCOM                         *COMPETENZE
541200010926     C                   WRITE     TEETW000
541300970306E   3C                   ENDIF
541400970306E   2C                   ENDIF
541500970306E   1C                   ENDDO                                                  *FINE RAGGRUPPAMENTI
541600970306     C*
541700970306     C                   ENDSR
541800970306     C*------------------------------------------------------------------------*
541900970306     C* SCREWR - SCRIVE SU FILE DI LAVORO IL DETTAGLIO DEI RICAVI -SE IN DEBUG-
542000970306     C*------------------------------------------------------------------------*
542100970306     C     SCREWR        BEGSR
542200970311     C*
542300970311     C* CICLO PER TUTTE LE VOCI DI TASSAZIONE
542400970306DO  1C     1             DO        NVOCA         V
542500970306IF  2C     VIM(V)        IFGT      *ZEROS
542600980909     C******************** MOVELUSER      EWRUSR           *UTENTE
542700980909     C                   MOVEL     JOB           EWRUSR                         *UTENTE
542800980603     C                   MOVEL     D00DSF        EWRDSF                         *DATI SEDE O FILIALE
542900970306     C                   MOVEL     D00CTO        EWRCTO                         *TARIFFA O OFFERTA
543000970306     C                   Z-ADD     D00KSC        EWRKSC                         *CLIENTE
543100970306     C                   Z-ADD     D00CTR        EWRCTR                         *TARIFFA
543200970306     C                   Z-ADD     D00PRG        EWRPRG                         *PROGRESSIVO
543300970306     C                   MOVEL     CCTS(T)       EWRCTS                         *CODICE TASSAZIONE
543400970306     C                   Z-ADD     SSGL(S)       EWRSGL                         *SCAGLIONE
543500970306     C                   Z-ADD     V             EWRVOC                         *VOCE
543600970306     C                   Z-ADD     VIM(V)        EWRIMP                         *IMPORTO VOCE
543700970306     C                   Z-ADD     VR(V)         EWRRDL                         *RAGGRUPPAMENTO DELT
543800010926     C                   WRITE     TEETWR00
543900970306E   2C                   ENDIF
544000970306E   1C                   ENDDO                                                  *FINE VOCI
544100970306     C*
544200970306     C                   ENDSR
544300970306     C*------------------------------------------------------------------------*
544400970306     C* CANCWF - CANCELLA IL FILE DI LAVORO
544500970306     C*------------------------------------------------------------------------*
544600970306     C     CANCWF        BEGSR
544700970317     C*
544800010926     C* CANCELLA I RECORD DELL'UTENTE NEL FILE TEETW00F
544900970317IF  1C     D00TLA        IFEQ      'D'                                          *LANCIO DELTA
545000980909     C******************** MOVELUSER      KTWUSR
545100980909     C                   MOVEL     JOB           KTWUSR
545200010926     C     KE2ETW        SETLL     TEETW01L
545300010926     C     KE2ETW        READE     TEETW01L                               99
545400970317DO  2C     *IN99         DOWEQ     *OFF
545500010926     C                   DELETE    TEETW000
545600010926     C     KE2ETW        READE     TEETW01L                               99
545700970317E   2C                   ENDDO
545800970317E   1C                   ENDIF
545900970306     C*
546000010926     C* CANCELLA I RECORD DELL'UTENTE NEL FILE TEETWR0F
546100980909     C******************** MOVELUSER      KWRUSR
546200980909     C                   MOVEL     JOB           KWRUSR
546300010926     C     KEYEWR        SETLL     TEETWR1L
546400010926     C     KEYEWR        READE     TEETWR1L                               99
546500970306DO  1C     *IN99         DOWEQ     *OFF
546600010926     C                   DELETE    TEETWR00
546700010926     C     KEYEWR        READE     TEETWR1L                               99
546800970306E   1C                   ENDDO
546900970306     C*
547000970306     C                   ENDSR
547100970306     C*------------------------------------------------------------------------*
547200970306     C* CARTAB - CARICA LE TABELLE
547300970306     C*------------------------------------------------------------------------*
547400970306     C     CARTAB        BEGSR
547500970306     C*
547600970306     C* CARICAMENTO DATI VARI TASSAZIONE -UNO-
547700970306     C                   EXSR      CARQT1
547800970306     C*
547900970306     C* CARICAMENTO ISTAT -UNO-
548000970306     C                   EXSR      CARQI1
548100970306     C*
548200970306     C* CARICAMENTO ISTAT -DUE-
548300970306     C                   EXSR      CARQI2
548400970306     C*
548500970306     C* CARICAMENTO TIPI TARIFFA
548600970306     C                   EXSR      CARTR
548700970306     C*
548800970306     C* CARICAMENTO SIGLE VARIE
548900970306     C                   EXSR      CAR$2
549000970306     C*
549100970306     C* CARICAMENTO CODICI TASSAZIONE
549200970306     C                   EXSR      CARCT
549300970306     C*
549400970306     C* CARICAMENTO TIPI SERVIZIO
549500970306     C                   EXSR      CAR5E
549600970306     C*
549700970306     C* CARICAMENTO TARIFFE PARTICOLARI -DI CARTELLO-
549800970306     C                   EXSR      CARPAR
549900970306     C*
550000970306     C                   ENDSR
550100970306     C*------------------------------------------------------------------------*
550200970306     C* CARQT1 - CARICAMENTO TABELLA DATI VARI DI TASSAZIONE -UNO-
550300970306     C*------------------------------------------------------------------------*
550400970306     C     CARQT1        BEGSR
550500021202     C*
550600021202     C                   CLEAR                   TRUL27DS
550700021202     C                   EVAL      I27TLA = 'L'
550800021202     C                   EVAL      I27DTA = DATCOR
550900021202     C                   CALL      'TRUL27R'
551000021202     C                   PARM                    TRUL27DS
551100021202     C                   IF        O27ERR = 'E'
551200021202     C                   MOVEL     MSG(11)       $MSG
551300021202     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
551400021202     C                   ENDIF
551500970306     C*
551600970306     C                   ENDSR
551700991125     C*------------------------------------------------------------------------*
551800970306     C* CARQI1 - CARICAMENTO ISTAT -UNO-
551900970306     C*------------------------------------------------------------------------*
552000970306     C     CARQI1        BEGSR
552100970306     C*
552200020906     C                   Z-ADD     1             KTBKUT
552300970306     C                   MOVEL     'QI'          KTBCOD
552400970306     C                   MOVEL(P)  '1'           KTBKEY
552500970306     C     KEYTAB        CHAIN     TABEL00F                           98
552600970306IF  1C     *IN98         IFEQ      *ON
552700970306     C                   MOVEL     MSG(13)       $MSG
552800970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
552900970306X   1C                   ELSE
553000970306     C                   MOVEL     TBLUNI        DSQI1
553100970306     C                   Z-ADD     §QID1T        DIA(1)                         *DATE SCATTO
553200970306     C                   Z-ADD     §QID2T        DIA(2)
553300970306     C                   Z-ADD     §QID3T        DIA(3)
553400970306     C                   Z-ADD     §QID4T        DIA(4)
553500970306     C                   Z-ADD     §QID5T        DIA(5)
553600970306     C                   Z-ADD     §QID6T        DIA(6)
553700970306     C                   Z-ADD     §QII1T        IAC(1)                         *PUNTI
553800970306     C                   Z-ADD     §QII2T        IAC(2)
553900970306     C                   Z-ADD     §QII3T        IAC(3)
554000970306     C                   Z-ADD     §QII4T        IAC(4)
554100970306     C                   Z-ADD     §QII5T        IAC(5)
554200970306     C                   Z-ADD     §QII6T        IAC(6)
554300970306E   1C                   ENDIF
554400970306     C*
554500970306     C                   ENDSR
554600970306     C*------------------------------------------------------------------------*
554700970306     C* CARQI2 - CARICAMENTO ISTAT -DUE-
554800970306     C*------------------------------------------------------------------------*
554900970306     C     CARQI2        BEGSR
555000970306     C*
555100020906     C                   Z-ADD     1             KTBKUT
555200970306     C                   MOVEL     'QI'          KTBCOD
555300970306     C                   MOVEL(P)  '2'           KTBKEY
555400970306     C     KEYTAB        CHAIN     TABEL00F                           98
555500970306IF  1C     *IN98         IFEQ      *ON
555600970306     C                   MOVEL     MSG(14)       $MSG
555700970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
555800970306X   1C                   ELSE
555900970306     C                   MOVEL     TBLUNI        DSQI2
556000970306     C                   Z-ADD     §QID7T        DIA(7)                         *DATE SCATTO
556100970306     C                   Z-ADD     §QID8T        DIA(8)
556200970306     C                   Z-ADD     §QID9T        DIA(9)
556300970306     C                   Z-ADD     §QID0T        DIA(10)
556400970306     C                   Z-ADD     §QID11        DIA(11)
556500970306     C                   Z-ADD     §QID12        DIA(12)
556600970306     C                   Z-ADD     §QII7T        IAC(7)                         *PUNTI
556700970306     C                   Z-ADD     §QII8T        IAC(8)
556800970306     C                   Z-ADD     §QII9T        IAC(9)
556900970306     C                   Z-ADD     §QII0T        IAC(10)
557000970306     C                   Z-ADD     §QII11        IAC(11)
557100970306     C                   Z-ADD     §QII12        IAC(12)
557200970306E   1C                   ENDIF
557300970306     C*
557400970306     C                   ENDSR
557500970306     C*------------------------------------------------------------------------*
557600970306     C* CARTR - CARICAMENTO TIPI TARIFFA
557700970306     C*------------------------------------------------------------------------*
557800970306     C     CARTR         BEGSR
557900970306     C*
558000980506DO  1C     1             DO        15            I
558100970306     C     I             OCCUR     DSTR
558200970306     C                   CLEAR                   DSTR                           *AZZERAMENTO
558300970306E   1C                   ENDDO
558400980707     C                   MOVEL     *BLANKS       CTR
558500970306     C*
558600970306     C                   Z-ADD     *ZEROS        I
558700020906     C                   Z-ADD     1             KTBKUT
558800970306     C                   MOVEL     'TR'          KTBCOD
558900970306     C     KE2TAB        SETLL     TABEL00F
559000970306     C     KE2TAB        READE     TABEL00F                               98
559100970306IF  1C     *IN98         IFEQ      *ON
559200970306     C                   MOVEL     MSG(15)       $MSG
559300970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
559400970306X   1C                   ELSE
559500970306DO  2C     *IN98         DOWEQ     *OFF
559600970306IF  3C     TBLKEY        IFNE      *BLANKS
559700970312IF  4C     TBLFLG        IFEQ      *BLANKS
559800970306     C                   ADD       1             I
559900970306     C                   MOVEL     TBLKEY        CTR(I)
560000970306     C     I             OCCUR     DSTR
560100970306     C                   MOVEL     TBLUNI        DSTR
560200970312E   4C                   ENDIF
560300970312E   3C                   ENDIF
560400970306     C     KE2TAB        READE     TABEL00F                               98
560500970306E   2C                   ENDDO
560600970306E   1C                   ENDIF
560700970306     C*
560800970306     C                   ENDSR
560900970306     C*------------------------------------------------------------------------*
561000970306     C* CAR$2 - CARICAMENTO SIGLE VARIE
561100970306     C*------------------------------------------------------------------------*
561200970306     C     CAR$2         BEGSR
561300970306     C*
561400970306     C                   CLEAR                   DS$2                           *AZZERAMENTO
561500970306     C*
561600020906     C                   Z-ADD     1             KTBKUT
561700970306     C                   MOVEL     '$2'          KTBCOD
561800970306     C                   MOVEL(P)  '1'           KTBKEY
561900970306     C     KEYTAB        CHAIN     TABEL00F                           98
562000970306IF  1C     *IN98         IFEQ      *ON
562100970306     C                   MOVEL     MSG(16)       $MSG
562200970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
562300970306X   1C                   ELSE
562400970306     C                   MOVEL     TBLUNI        DS$2
562500970306E   1C                   ENDIF
562600970306     C*
562700970306     C                   ENDSR
562800970306     C*------------------------------------------------------------------------*
562900970306     C* CARCT - CARICAMENTO CODICI TASSAZIONE
563000970306     C*------------------------------------------------------------------------*
563100970306     C     CARCT         BEGSR
563200970306     C*
563300050103DO  1C     1             DO        600           I
563400970306     C     I             OCCUR     DSCT
563500970306     C                   CLEAR                   DSCT                           *AZZERAMENTO
563600970306E   1C                   ENDDO
563700970306     C*
563800970306     C                   Z-ADD     *ZEROS        I
563900020906     C                   Z-ADD     1             KTBKUT
564000970306     C                   MOVEL     'CT'          KTBCOD
564100970306     C     KE2TAB        SETLL     TABEL00F
564200970306     C     KE2TAB        READE     TABEL00F                               98
564300970306IF  1C     *IN98         IFEQ      *ON
564400970306     C                   MOVEL     MSG(18)       $MSG
564500970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
564600970306X   1C                   ELSE
564700970306DO  2C     *IN98         DOWEQ     *OFF
564800970306IF  3C     TBLKEY        IFNE      *BLANKS
564900970312IF  4C     TBLFLG        IFEQ      *BLANKS
565000970306     C                   ADD       1             I
565100970306     C                   MOVEL     TBLKEY        CTS(I)
565200970306     C     I             OCCUR     DSCT
565300970306     C                   MOVEL     TBLUNI        DSCT
565400970312IF  5C     §CTUTC        IFEQ      'N'                                          *DA NON CONSIDERARE
565500970312     C                   CLEAR                   DSCT                           *PULISCE
565600970312     C     I             SUB       1             I                              *TORNA INDIETRO DI 1
565700970312E   5C                   ENDIF
565800970312E   4C                   ENDIF
565900970312E   3C                   ENDIF
566000970306     C     KE2TAB        READE     TABEL00F                               98
566100970306E   2C                   ENDDO
566200970306E   1C                   ENDIF
566300970306     C*
566400970306     C                   ENDSR
566500970306     C*------------------------------------------------------------------------*
566600970306     C* CAR5E - CARICAMENTO TIPI SERVIZIO
566700970306     C*------------------------------------------------------------------------*
566800970306     C     CAR5E         BEGSR
566900970306     C*
567000970306DO  1C     1             DO        10            I
567100970306     C     I             OCCUR     DS5E
567200970306     C                   CLEAR                   DS5E                           *AZZERAMENTO
567300970306E   1C                   ENDDO
567400970306     C*
567500970306     C                   Z-ADD     *ZEROS        I
567600020906     C                   Z-ADD     1             KTBKUT
567700970306     C                   MOVEL     '5E'          KTBCOD
567800970306     C     KE2TAB        SETLL     TABEL00F
567900970306     C     KE2TAB        READE     TABEL00F                               98
568000970306IF  1C     *IN98         IFEQ      *ON
568100970306     C                   MOVEL     MSG(19)       $MSG
568200970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
568300970306X   1C                   ELSE
568400970306DO  2C     *IN98         DOWEQ     *OFF
568500970306IF  3C     TBLKEY        IFNE      *BLANKS
568600970312IF  4C     TBLFLG        IFEQ      *BLANKS
568700970306     C                   ADD       1             I
568800970306     C                   MOVEL     TBLKEY        TS(I)
568900970306     C     I             OCCUR     DS5E
569000970306     C                   MOVEL     TBLUNI        DS5E
569100970312E   4C                   ENDIF
569200970312E   3C                   ENDIF
569300970306     C     KE2TAB        READE     TABEL00F                               98
569400970306E   2C                   ENDDO
569500970306E   1C                   ENDIF
569600970306     C*
569700970306     C                   ENDSR
569800970306     C*------------------------------------------------------------------------*
569900970306     C* CARPAR - CARICAMENTO TARIFFE PARTICOLARI -DI CARTELLO-
570000970306     C*------------------------------------------------------------------------*
570100970306     C     CARPAR        BEGSR
570200970306     C*---
570300970306     C* MEMORIZZA I DATI ASSICURAZIONE X CONTO / ISOLA / CONSEGNE PARTICOLARI
570400970306     C* DELLA TARIFFA DI CARTELLO IN VIGORE ALLA DATA DI FATTURAZIONE
570500970306     C*---
570600021202     C                   Z-ADD     O27KSC        KSC1                           *IMPOSTA CARTELLO
570700970306     C                   Z-ADD     *ZEROS        CTR1
570800011019     C                   EXSR      CERTAM                                       *CERCA TARIFFA
570900970306     C*
571000970306     C* MEMORIZZA LA CHIAVE DELLA TARIFFA DI CARTELLO IN VIGORE
571100970306IF  1C     WERR          IFEQ      'N'                                          *NO ERRORE
571200970306     C                   Z-ADD     TAMKSC        CARKSC
571300970306     C                   Z-ADD     TAMCTR        CARCTR
571400970306     C                   Z-ADD     TAMPRG        CARPRG
571500011019     C*
571600011023     C* RECUPERA LA DIVISA DELLA TARIFFA DI CARTELLO
571700011023     C                   CLEAR                   DSTA01
571800011023     C                   MOVEL     TAMFLO        DSTA01
571900011023     C                   MOVEL     §TADIV        CARDIV            3            *DIVISA TARIFFA
572000970306     C*
572100970306     C* CARICA LE TARIFFE PARTICOLARI DI CARTELLO
572200970306     C                   EXSR      CAR1P
572300970306X   1C                   ELSE                                                   *ERRORE
572400970306     C                   MOVEL     MSG(22)       $MSG
572500970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
572600970306E   1C                   ENDIF
572700970306     C*
572800970306     C                   ENDSR
572900970306     C*------------------------------------------------------------------------*
573000970306     C* CERTAM - CERCA UN TARIFFA
573100970306     C*------------------------------------------------------------------------*
573200970306     C     CERTAM        BEGSR
573300970306     C*
573400970306     C* REIMPOSTA LE VARIABILI DI LAVORO
573500970306     C                   Z-ADD     *ZEROS        DEPKSC                         *CAMPI DEPOSITO
573600970306     C                   Z-ADD     *ZEROS        DEPPRG
573700970306     C                   Z-ADD     *ZEROS        DEPCTR
573800970306     C                   MOVEL     '0'           $TAR              1            TARIFFA IN DECORRENZ
573900970306     C                   MOVEL     '0'           $SCA              1            TARIFFA SCADUTA
574000970306     C                   MOVEL     'N'           WERR                           *NO ERRORE
574100970306     C*
574200970306     C                   Z-ADD     KSC1          KTMKSC                         *CLIENTE
574300970306     C                   Z-ADD     CTR1          KTMCTR                         *TARIFFA
574400970306IF  1C     D00CTO        IFEQ      'O'                                          *OFFERTE
574500970306     C     KEYTAM        SETLL     TNOFM00L                               99
574600970306X   1C                   ELSE                                                   *TARIFFE
574700970306     C     KEYTAM        SETLL     TNTAM00L                               99
574800970306E   1C                   ENDIF
574900970306IF  1C     *IN99         IFEQ      *OFF                                         *NON TROVATA
575000970306     C                   MOVEL     'S'           WERR                           *MANCA TARIFFA
575100970306X   1C                   ELSE
575200970306IF  2C     D00CTO        IFEQ      'O'                                          *OFFERTE
575300970306     C     KEYTAM        READE     TNOFM00L                               99
575400970306X   2C                   ELSE                                                   *TARIFFE
575500970306     C     KEYTAM        READE     TNTAM00L                               99
575600970306E   2C                   ENDIF
575700970306DO  2C     *IN99         DOWEQ     *OFF                                         *FINE FILE
575800970306     C     $TAR          ANDEQ     '0'                                          *TROVATA TAR.IN DEC.
575900970306     C     WERR          ANDEQ     'N'                                          *NO MANCA TARIFFA
576000970306IF  3C     TAMATB        IFEQ      *BLANKS                                      *NO ANNULLATE
576100970306IF  4C     D00DTC        IFGE      TAMDDT                                       TARIFFA IN DECORRENZ
576200970306     C     D00DTC        ANDLE     TAMDST
576300970306     C                   MOVEL     '1'           $TAR                           *TAR.IN DEC.TROVATA
576400970306     C                   Z-ADD     TAMKSC        DEPKSC                         *CHIAVE DEPOSITATA
576500970306     C                   Z-ADD     TAMCTR        DEPCTR
576600970306     C                   Z-ADD     TAMPRG        DEPPRG
576700970306X   4C                   ELSE                                                   TARIFFA NON IN DECOR
576800970306IF  5C     D00DTC        IFLT      TAMDDT                                       *TARIFFA DA DECORRER
576900970306IF  6C     $SCA          IFEQ      '0'                                          *NON ESISTE TAR.SCAD
577000970306     C                   MOVEL     'S'           WERR                           *MANCA TARIFFA
577100970306E   6C                   ENDIF
577200970306E   5C                   ENDIF
577300970306IF  5C     D00DTC        IFGT      TAMDST                                       *TARIFFA SCADUTA
577400970306     C                   MOVEL     '1'           $SCA                           *TAR.SCADUTA TROVATA
577500970306     C                   Z-ADD     TAMKSC        DEPKSC                         *CHIAVE DEPOSITATA
577600970306     C                   Z-ADD     TAMCTR        DEPCTR
577700970306     C                   Z-ADD     TAMPRG        DEPPRG
577800970306E   5C                   ENDIF
577900970306E   4C                   ENDIF
578000970306E   3C                   ENDIF
578100970306IF  3C     D00CTO        IFEQ      'O'                                          *OFFERTE
578200970306     C     KEYTAM        READE     TNOFM00L                               99
578300970306X   3C                   ELSE                                                   *TARIFFE
578400970306     C     KEYTAM        READE     TNTAM00L                               99
578500970306E   3C                   ENDIF
578600970306E   2C                   ENDDO                                                  *FINE TARIFFE
578700970306E   1C                   ENDIF
578800970306     C                   Z-ADD     DEPKSC        TAMKSC                         *CHIAVE DEPOSITATA
578900970306     C                   Z-ADD     DEPCTR        TAMCTR
579000970306     C                   Z-ADD     DEPPRG        TAMPRG
579100970306     C*
579200970306     C                   ENDSR
579300970306     C*------------------------------------------------------------------------*
579400970306     C* CAR1P - CARICAMENTO TARIFFE PARTICOLARI DI CARTELLO
579500970306     C*------------------------------------------------------------------------*
579600970306     C     CAR1P         BEGSR
579700970306     C*
579800020104DO  1C     1             DO        50            I
579900970306     C     I             OCCUR     CPIDS
580000970306     C                   CLEAR                   CPIDS                          *AZZERAMENTO
580100970306     C     I             OCCUR     DS1P
580200970306     C                   CLEAR                   DS1P                           *AZZERAMENTO
580300970306E   1C                   ENDDO
580400970306     C                   MOVEL     *BLANKS       CP
580500970306     C*
580600970306     C* PER CIASCUNA TARIFFA PARTICOLARE IN TABELLA
580700970306     C                   Z-ADD     *ZEROS        I
580800020906     C                   Z-ADD     1             KTBKUT
580900970306     C                   MOVEL     '1P'          KTBCOD
581000970306     C     KE2TAB        SETLL     TABEL00F
581100970306     C     KE2TAB        READE     TABEL00F                               98
581200970306IF  1C     *IN98         IFEQ      *ON
581300970306     C                   MOVEL     MSG(20)       $MSG
581400970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
581500970306X   1C                   ELSE
581600970306DO  2C     *IN98         DOWEQ     *OFF
581700970306IF  3C     TBLKEY        IFNE      *BLANKS
581800970312IF  4C     TBLFLG        IFEQ      *BLANKS
581900970306     C                   MOVEL     TBLUNI        CAMPO            26
582000970306     C                   MOVE      CAMPO         WFCP              1
582100970312IF  5C     WFCP          IFEQ      'S'                                          *TARIFFE PART.DA FAT
582200970306     C                   ADD       1             I
582300970306     C                   MOVEL     TBLKEY        CP(I)
582400970306     C     I             OCCUR     DS1P
582500970306     C                   MOVEL     TBLUNI        DS1P
582600970306     C*
582700970306     C* CARICA LE TARIFFE PARTICOLARI DI CARTELLO
582800970306     C                   MOVEL     '0'           $TPT              1            *TAR.PARTIC.TROVATA
582900011023     C                   Z-ADD     TAMKSC        KTPKSC
583000970306     C                   Z-ADD     TAMCTR        KTPCTR
583100970306     C                   Z-ADD     TAMPRG        KTPPRG
583200970306     C                   MOVEL     CP(I)         KTPFTC
583300970311     C     I             OCCUR     CPIDS
583400970312IF  6C     D00CTO        IFEQ      'O'                                          *OFFERTE
583500990819     C     KEYTPT        SETLL     TIOPT01L                               99
583600990819     C     KEYTPT        READE     TIOPT01L                               99
583700970312X   6C                   ELSE                                                   *TARIFFE
583800990819     C     KEYTPT        SETLL     TITPT01L                               99
583900990819     C     KEYTPT        READE     TITPT01L                               99
584000970312E   6C                   ENDIF
584100970312DO  6C     *IN99         DOWEQ     *OFF                                         *FINE FILE
584200970306     C     $TPT          ANDEQ     '0'                                          *TROVATA TARIFFA
584300011023     C*
584400011023     C                   MOVEL     CARDIV        WDIV
584500991125     C*
584600991125     C* CONTROLLA SE CONVERTIRE I CAMPI DI TITPT
584700011026IF  7C     WDIV          IFNE      WDIVCE
584800011023     C                   MOVEL     WDIV          WDVI
584900011023     C                   EXSR      CNVTPT
585000011023E   7C                   ENDIF
585100991125     C*
585200970312IF  7C     TPTATB        IFEQ      *BLANKS
585300970306     C                   MOVEL     '1'           $TPT                           *TAR.PARTIC.TROVATA
585400970306     C                   Z-ADD     TPTRPV        TRPV8P
585500970306     C                   Z-ADD     TPTARF        TARF8P
585600970306     C                   Z-ADD     TPTARL        TARL8P
585700970306     C                   Z-ADD     TPTARO        TARO8P
585800970306     C                   MOVEL     TPTTPG        TTPG8P
585900970306     C                   MOVEL     TPTAPL        TAPL8P
586000970306     C                   MOVEL     §1PCTS        TCTS8P
586100970306     C                   MOVEL     §1PCTE        TCTE8P
586200970312E   7C                   ENDIF
586300970312IF  7C     D00CTO        IFEQ      'O'                                          *OFFERTE
586400990819     C     KEYTPT        READE     TIOPT01L                               99
586500970312X   7C                   ELSE                                                   *TARIFFE
586600990819     C     KEYTPT        READE     TITPT01L                               99
586700970312E   7C                   ENDIF
586800970312E   6C                   ENDDO                                                  *FINE TAR.PARTICOLAR
586900970312IF  6C     $TPT          IFEQ      '0'                                          *TARIFFA NON TROVATA
587000970306     C                   MOVEL     'NO'          CP88
587100970312E   6C                   ENDIF
587200970312E   5C                   ENDIF
587300970312E   4C                   ENDIF
587400970306E   3C                   ENDIF
587500970306     C     KE2TAB        READE     TABEL00F                               98
587600970306E   2C                   ENDDO
587700970306E   1C                   ENDIF
587800970306     C*
587900970306     C                   ENDSR
588000970715     C*------------------------------------------------------------------------*
588100970715     C* REPINP - RECUPERA INTRADAMENTO -PARTENZA-
588200970715     C*------------------------------------------------------------------------*
588300970715     C     REPINP        BEGSR
588400970715     C*
588500970715     C* TERMINAL PARTENZA
588600970715     C                   CLEAR                   DSLV55
588700970715     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
588800970715     C                   MOVEL     'P'           D55TPT                         *ECCEZIONE ARRIVO
588900970715     C                   Z-ADD     D00LNP        D55LIN                         *LINEA
589000970715     C                   Z-ADD     D00DTC        D55DRF                         *DATA RIFERIMENTO
589100970715     C                   CALL      'FNLV55R'
589200970715     C                   PARM                    DSLV55
589300970715IF  1C     D55ERR        IFNE      *BLANKS                                      *NON TROVATO
589400970715     C                   MOVEL     D55MSG        $MSG
589500970715     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
589600970715X   1C                   ELSE                                                   *TROVATO
589700970715     C                   Z-ADD     D55TFP        WTFP                           *TERMINAL PARTENZA
589800970715E   1C                   ENDIF
589900970715     C*
590000970715     C* CODICE TASSAZIONE PARTENZA
590100970715     C                   Z-ADD     D00LNP        KORFIL
590200970715     C     KEYORG        CHAIN     AZORG01L                           98
590300970715IF  1C     *IN98         IFEQ      *ON                                          *NON TROVATO
590400970715     C                   MOVEL     MSG(5)        $MSG
590500970715     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
590600970715X   1C                   ELSE                                                   *TROVATA
590700970721     C                   MOVEL     ORGCTS        WMCT                           *CTS MITTENTE
590800020729     C                   MOVEL     ORGDE3        OG143
590900020729     C*
591000020729     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
591100020729     C                   CLEAR                   DNTW
591200020729     C*
591300020729     C                   MOVEL(P)  'NTW'         KTECOD
591400020729     C                   MOVEL(P)  §OGNTW        KTEKE1
591500020729     C     KEYTBE        CHAIN     TNTBE01L
591600020729     C                   IF        %found(TNTBE01L)
591700020729     C                   MOVEL     TBEUNI        DNTW
591800020729     C                   ENDIF
591900020729     C*
592000020729     C                   MOVEL     §NTWFIE       DLPFL1                         *FLAG ITALIA/ESTERO
592100970715E   1C                   ENDIF
592200970715     C*
592300970715     C                   ENDSR
592400970306     C*------------------------------------------------------------------------*
592500970306     C* CARETC - CARICA I DATI TIPO -GENERALI-
592600970306     C*------------------------------------------------------------------------*
592700970306     C     CARETC        BEGSR
592800970306     C*
592900980603     C                   MOVEL     D00DSF        KTCDSF                         *DATI SEDE O FILIALE
593000970306     C                   MOVEL     D00CTO        KTCCTO                         *TARIFFA O OFFERTA
593100970306     C                   Z-ADD     D00KSC        KTCKSC                         *CLIENTE O VISITA
593200970306     C                   Z-ADD     D00CTR        KTCCTR                         *TARIFFA
593300970306     C                   Z-ADD     D00PRG        KTCPRG                         *PROGRESSIVO
593400970306     C                   Z-ADD     D00LNP        KTCLNP                         *LINEA DI PARTENZA
593500010926     C     KEYETC        CHAIN     TEETC01L                           99
593600970306IF  1C     *IN99         IFEQ      *ON                                          *NON TROVATO
593700970306     C     ETCATB        ORNE      *BLANKS                                      *NO ANNULLATI
593800970306     C                   MOVEL     MSG(1)        $MSG
593900970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
594000011008     C                   ELSE
594100011026     C     D00DIV        IFNE      WDIVCE
594200011019     C                   MOVEL     D00DIV        WDVI
594300011008     C                   EXSR      CNVETC
594400011008     C                   ENDIF
594500970306E   1C                   ENDIF
594600970306     C*
594700970306     C                   ENDSR
594800970306     C*------------------------------------------------------------------------*
594900970306     C* CARETS - CARICA I DATI TIPO -SCAGLIONI-
595000970306     C*------------------------------------------------------------------------*
595100970306     C     CARETS        BEGSR
595200970306     C*
595300970306     C                   MOVEL     '0'           $SGL              1            *SCAGLIONI ESISTENTI
595400970306     C                   Z-ADD     *ZEROS        I                 6 0
595500980603     C                   MOVEL     D00DSF        KTSDSF                         *DATI SEDE O FILIALE
595600970306     C                   MOVEL     D00CTO        KTSCTO                         *TARIFFA O OFFERTA
595700970306     C                   Z-ADD     D00KSC        KTSKSC                         *CLIENTE O VISITA
595800970306     C                   Z-ADD     D00CTR        KTSCTR                         *TARIFFA
595900970306     C                   Z-ADD     D00PRG        KTSPRG                         *PROGRESSIVO
596000970306     C                   Z-ADD     D00LNP        KTSLNP                         *LINEA DI PARTENZA
596100010926     C     KE2ETS        SETLL     TEETS01L
596200010926     C     KE2ETS        READE     TEETS01L                               99
596300970306DO  1C     *IN99         DOWEQ     *OFF
596400970306IF  2C     ETSATB        IFEQ      *BLANKS                                      *NO ANNULLATI
596500011026     C     D00DIV        IFNE      WDIVCE
596600011019     C                   MOVEL     D00DIV        WDVI
596700011008     C                   EXSR      CNVETS
596800011008     C                   ENDIF
596900970306     C                   MOVEL     '1'           $SGL
597000970306     C                   ADD       1             I
597100970306     C                   Z-ADD     ETSSGL        SSGL(I)                        *SCAGLIONE SUPERIORE
597200970306     C                   Z-ADD     ETSPSC        SPSC(I)                        *PESO RIFERIMENTO
597300970306     C                   Z-ADD     ETSCSC        SCSC(I)                        *COLLI RIFERIMENTO
597400970306     C                   Z-ADD     ETSVSC        SVSC(I)                        *VOLUME RIFERIMENTO
597500970318     C                   Z-ADD     ETSINL        SINL(I)                        *INOLTRO
597600970318     C                   Z-ADD     ETSRPV        SRPV(I)                        *RAPPORTO PESO/VOLUM
597700970306     C                   Z-ADD     ETSPPE        SPPE(I)                        *% INCIDENZA
597800970414     C                   MOVEL     ETSSDE        SSDE(I)                        *SEGNO DELTA
597900970414     C                   Z-ADD     ETSPDE        SPDE(I)                        *% DELTA
598000970306E   2C                   ENDIF
598100010926     C     KE2ETS        READE     TEETS01L                               99
598200970306E   1C                   ENDDO
598300970306     C                   Z-ADD     I             WULTSC            6 0
598400970306IF  1C     $SGL          IFEQ      '0'                                          *NON TROVATO
598500970306     C                   MOVEL     MSG(2)        $MSG
598600970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
598700970306E   1C                   ENDIF
598800970306     C*
598900970306     C                   ENDSR
599000970306     C*------------------------------------------------------------------------*
599100970306     C* CARETT - CARICA I DATI TIPO -CODICI TASSAZIONE-
599200970306     C*------------------------------------------------------------------------*
599300970306     C     CARETT        BEGSR
599400970306     C*
599500970306     C                   MOVEL     '0'           $CTS              1            *COD.TASS. ESISTENTI
599600970306     C                   Z-ADD     *ZEROS        I                 6 0
599700980603     C                   MOVEL     D00DSF        KTTDSF                         *DATI SEDE O FILIALE
599800970306     C                   MOVEL     D00CTO        KTTCTO                         *TARIFFA O OFFERTA
599900970306     C                   Z-ADD     D00KSC        KTTKSC                         *CLIENTE O VISITA
600000970306     C                   Z-ADD     D00CTR        KTTCTR                         *TARIFFA
600100970306     C                   Z-ADD     D00PRG        KTTPRG                         *PROGRESSIVO
600200970306     C                   Z-ADD     D00LNP        KTTLNP                         *LINEA DI PARTENZA
600300970306     C                   MOVEL     D00FIE        KTTFIE                         *ITALIA/ESTERO
600400011017     C     KE2ETT        SETLL     TEETT01L
600500011017     C     KE2ETT        READE     TEETT01L                               99
600600970306DO  1C     *IN99         DOWEQ     *OFF
600700970306IF  2C     ETTRCT        IFNE      *BLANKS                                      *DA CALCOLARE
600800970306IF  3C     ETTATB        IFEQ      *BLANKS                                      *NO ANNULLATI
600900970306     C                   MOVEL     '1'           $CTS
601000970306     C                   ADD       1             I
601100970306     C                   MOVEL     ETTCTS        CCTS(I)                        *CODICE TASSAZIONE
601200970306     C                   MOVEL     ETTRCT        CRCT(I)                        *RAGGRUPPAMENTO
601300970306     C                   Z-ADD     ETTPPE        CPPE(I)                        *% INCIDENZA
601400970306     C*
601500970715     C* RECUPERA INSTRADAMENTO -ARRIVO-
601600020906     C                   Z-ADD     1             KTBKUT
601700970306     C                   MOVEL     'CT'          KTBCOD
601800970306     C                   MOVEL(P)  ETTCTS        KTBKEY
601900970306     C     KEYTAB        CHAIN     TABEL00F                           98
602000970306IF  4C     *IN98         IFEQ      *ON                                          *NON TROVATO
602100970306     C                   MOVEL     MSG(4)        $MSG
602200970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
602300970306X   4C                   ELSE                                                   *TROVATO
602400970306     C                   MOVEL     TBLUNI        DSCT
602500970306     C                   MOVEL     §CTRAP        CRAP(I)                        *REGIONE APPARTENENZ
602600970306     C     §CTNAR        IFEQ      *BLANKS
602700970306     C                   MOVEL     'IT'          CNAR(I)                        *NAZIONE APPARTENENZ
602800970306     C                   ELSE
602900970306     C                   MOVEL     'EE'          CNAR(I)
603000970306     C                   ENDIF
603100011030     C                   MOVEL     §CT§§5        C§§5(I)                        *FLAG ISOLA/NONISOLA
603200970715     C* LINEA ARRIVO
603300970715     C                   CLEAR                   DSSI95
603400970715     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
603500970715     C                   MOVEL     '3'           I95TCN                         *CONTROLLO ESISTENZA
603600970715     C                   MOVEL     §CTNAR        I95NAR                         *NAZIONE
603700970715     C                   MOVEL     §CTCAP        I95CAP                         *CAP
603800970715     C                   Z-ADD     WTFP          I95TFP                         *TERMINAL PARTENZA
603900980602     C                   MOVEL     'S'           I95FRE                         *CONTROLLA ECCEZIONI
604000970715     C                   Z-ADD     D00DTC        I95DAT                         *DATA RIFERIMENTO
604100970715     C                   CALL      'TISI95R'
604200970715     C                   PARM                    DSSI95
604300970715IF  5C     O95ERR        IFNE      *BLANKS                                      *NON TROVATO
604400970715     C                   MOVEL     O95MSG        $MSG
604500970715     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
604600970715X   5C                   ELSE                                                   *TROVATO
604700970715     C                   Z-ADD     O95LNA        CLNA(I)                        *LINEA ARRIVO
604800971204     C* FLAG ITALIA/ESTERO
604900971204     C                   Z-ADD     CLNA(I)       KORFIL
605000971204     C     KEYORG        CHAIN     AZORG01L                           98
605100020729     C                   MOVEL     ORGDE3        OG143
605200971204IF  6C     *IN98         IFEQ      *OFF                                         *NON TROVATO
605300020729     C*
605400020729     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
605500020729     C                   CLEAR                   DNTW
605600020729     C*
605700020729     C                   MOVEL(P)  'NTW'         KTECOD
605800020729     C                   MOVEL(P)  §OGNTW        KTEKE1
605900020729     C     KEYTBE        CHAIN     TNTBE01L
606000020729     C                   IF        %found(TNTBE01L)
606100020729     C                   MOVEL     TBEUNI        DNTW
606200020729     C                   ENDIF
606300020729     C*
606400020729     C                   MOVEL     §NTWFIE       CFL1(I)                        *FLAG ITALIA/ESTERO
606500971204E   6C                   ENDIF
606600971204     C*
606700970715     C* TERMINAL ARRIVO
606800970715     C                   CLEAR                   DSLV55
606900970715     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
607000970715     C                   MOVEL     'A'           D55TPT                         *ECCEZIONE ARRIVO
607100970715     C                   Z-ADD     O95LNA        D55LIN                         *LINEA
607200970715     C                   Z-ADD     D00DTC        D55DRF                         *DATA RIFERIMENTO
607300970715     C                   Z-ADD     D00LNP        D55LNP                         *LINEA PARTENZA
607400970715     C                   CALL      'FNLV55R'
607500970715     C                   PARM                    DSLV55
607600970715IF  6C     D55ERR        IFNE      *BLANKS                                      *NON TROVATO
607700970715     C                   MOVEL     D55MSG        $MSG
607800970715     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
607900970715X   6C                   ELSE                                                   *TROVATO
608000970715     C                   Z-ADD     D55TFA        CTFA(I)                        *TERMINAL ARRIVO
608100970715E   6C                   ENDIF
608200970715E   5C                   ENDIF
608300970306E   4C                   ENDIF
608400970306E   3C                   ENDIF
608500970306E   2C                   ENDIF
608600011017     C     KE2ETT        READE     TEETT01L                               99
608700970306E   1C                   ENDDO                                                  *FINE COD.TASSAZIONE
608800970306     C                   Z-ADD     I             WULTCT            6 0
608900970306IF  1C     $CTS          IFEQ      '0'                                          *NON TROVATO
609000970306     C                   MOVEL     MSG(3)        $MSG
609100970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
609200970306E   1C                   ENDIF
609300970715     C*
609400970306     C                   ENDSR
609500970306     C*------------------------------------------------------------------------*
609600970306     C* CARTAM - CARICAMENTO TARIFFA CLIENTE -TESTATA-
609700970306     C*------------------------------------------------------------------------*
609800970306     C     CARTAM        BEGSR
609900970306     C*
610000970306     C                   Z-ADD     D00KSC        KTMKSC                         *CLIENTE
610100970306     C                   Z-ADD     D00CTR        KTMCTR                         *TARIFFA
610200970306     C                   Z-ADD     D00PRG        KTMPRG                         *PROGRESSIVO
610300970306     C     KEYTAM        CHAIN     TNTAM00L                           99        *ESISTE PER FORZA
610400040826     C                   MOVEL     TAMFLO        DSTA01
610500970306     C*
610600970306     C* APPLICAZIONE TARIFFA FINITA
610700011031     C                   Z-ADD     *ZEROS        MINTAS           23 8
610800011031     C                   Z-ADD     *ZEROS        MINTAR           23 8
610900970306     C                   MOVEL     D00CTR        UNOCTR            1 0          *TIPO TARIFFA
611000980707     C                   MOVEL     UNOCTR        WTPG              1
611100970306     C                   SETOFF                                           28
611200970306     C                   Z-ADD     1             I
611300980707     C     WTPG          LOOKUP    CTR(I)                                 28
611400970306IF  1C     *IN28         IFEQ      *ON
611500970306     C     I             OCCUR     DSTR
611600970306     C                   Z-ADD     §TRATA        MINTAS                         *MINIMO TASSABILE ST
611700970306E   1C                   ENDIF
611800980422     C                   Z-ADD     §TRATA        MINTAR
611900970306IF  1C     TAMATA        IFGT      *ZEROS                                       *APPLICAZ.TAR.FINITA
612000970306     C                   Z-ADD     TAMATA        MINTAR                         *MINIMO APPLICAZ. KS
612100970306E   1C                   ENDIF
612200970306     C*
612300980506     C* DEPOSITA I DATI DELLA DSTR DELLA STATA TARIFFA
612400980506     C                   Z-ADD     MINTAS        DMITAS                         *MINIMO TASSABILE ST
612500980506     C                   MOVEL     §TRARR        DTRARR                         *ARROTONDAM ABILE ST
612600980506     C                   MOVEL     §TRRPV        DTRRPV                         *RPV        ABILE ST
612700971022     C*
612800970306     C                   ENDSR
612900991125     C*------------------------------------------------------------------------*
613000011008     C* CNVTAD - CONVERTE I CAMPI DI TITAM IN DIVISA DI CALCOLO DEL CE
613100991125     C*------------------------------------------------------------------------*
613200991125     C     CNVTAD        BEGSR
613300991125     C* PORTO
613400991125     C                   Z-ADD     TADITR        WIMI             30 9
613500991125     C                   EXSR      CNVEUR
613600991125     C                   Z-ADD     WIMO          TADITR
613700991125     C* INOLTRO
613800991125     C                   Z-ADD     TADINO        WIMI             30 9
613900991125     C                   EXSR      CNVEUR
614000991125     C                   Z-ADD     WIMO          TADINO
614100991125     C*
614200991125     C                   ENDSR
614300991125     C*------------------------------------------------------------------------*
614400011008     C* CNVTPT - CONVERTE I CAMPI DI TITPT IN DIVISA DI CALCOLO DEL CE
614500991125     C*------------------------------------------------------------------------*
614600991125     C     CNVTPT        BEGSR
614700991125     C*
614800991125     C* ALCUNI CAMPI SONO DA CONVERTIRE SOLO SE CONTENGONO IMPORTI IN MONETA
614900991125IF  1C     TPTTPG        IFEQ      '4'                                          *QUANTITA'
615000991125     C     TPTTPG        OREQ      'T'                                          *TRASPORTO
615100991125     C     TPTTPG        OREQ      'V'                                          *VALORE
615200991125     C*
615300991125     C* ARROTONDAMENTO LIMITE
615400991125     C                   Z-ADD     TPTARL        WIMI             30 9
615500991125     C                   EXSR      CNVEUR
615600991125     C                   Z-ADD     WIMO          TPTARL
615700991125     C*
615800991125     C* ARROTONDAMENTO FINO LIMITE
615900991125     C                   Z-ADD     TPTARF        WIMI             30 9
616000991125     C                   EXSR      CNVEUR
616100991125     C                   Z-ADD     WIMO          TPTARF
616200991125     C*
616300991125     C* ARROTONDAMENTO OLTRE LIMITE
616400991125     C                   Z-ADD     TPTARO        WIMI             30 9
616500991125     C                   EXSR      CNVEUR
616600991125     C                   Z-ADD     WIMO          TPTARO
616700991125E   1C                   ENDIF
616800991125     C* VALORE MERCE
616900991125     C                   Z-ADD     TPTVLM        WIMI             30 9
617000991125     C                   EXSR      CNVEUR
617100991125     C                   Z-ADD     WIMO          TPTVLM
617200991125     C*
617300991125     C                   ENDSR
617400991125     C*------------------------------------------------------------------------*
617500011008     C* CNVTPD - CONVERTE I CAMPI DI TITPD IN DIVISA DI CALCOLO DEL CE
617600991125     C*------------------------------------------------------------------------*
617700991125     C     CNVTPD        BEGSR
617800991125     C*
617900991125     C* ALCUNI CAMPI SONO DA CONVERTIRE SOLO SE CONTENGONO IMPORTI IN MONETA
618000991125IF  1C     TPTTPG        IFEQ      '4'                                          *QUANTITA'
618100991125     C     TPTTPG        OREQ      'T'                                          *TRASPORTO
618200991125     C     TPTTPG        OREQ      'V'                                          *VALORE
618300991125     C* SCAGLIONE
618400991125     C                   Z-ADD     TPDSGL        N5                5 0
618500991125IF  2C     N5            IFNE      99999                                        *SCAGLIONE 99999=OLT
618600991125     C                   Z-ADD     TPDSGL        WIMI             30 9
618700991125     C                   EXSR      CNVEUR
618800991125     C                   Z-ADD     WIMO          TPDSGL
618900991125E   2C                   ENDIF
619000991125E   1C                   ENDIF
619100991125     C*
619200991125     C* ALCUNI CAMPI NON SONO DA CONVERTIRE PERCHE' RAPPRESENTANO PERCENTUALI
619300991125IF  1C     TPTTPG        IFNE      '4'                                          *QUANTITA'
619400991125     C     TPTTPG        ANDNE     'T'                                          *TRASPORTO
619500991125     C     TPTTPG        ANDNE     'V'                                          *VALORE
619600991125     C* TARIFFA
619700991125     C                   Z-ADD     TPDITR        WIMI             30 9
619800991125     C                   EXSR      CNVEUR
619900991125     C                   Z-ADD     WIMO          TPDITR
620000991125E   1C                   ENDIF
620100991125     C* MINIMO
620200991125     C                   Z-ADD     TPDMIN        WIMI             30 9
620300991125     C                   EXSR      CNVEUR
620400991125     C                   Z-ADD     WIMO          TPDMIN
620500991125     C* MASSIMO
620600991125     C                   Z-ADD     TPDMAX        WIMI             30 9
620700991125     C                   EXSR      CNVEUR
620800991125     C                   Z-ADD     WIMO          TPDMAX
620900991125     C*
621000991125     C                   ENDSR
621100991125     C*------------------------------------------------------------------------*
621200011008     C* CNVTGC - CONVERTE I CAMPI DI TITGC IN DIVISA DI CALCOLO DEL CE
621300991125     C*------------------------------------------------------------------------*
621400991125     C     CNVTGC        BEGSR
621500991125     C*
621600991125     C* SPESE VARIE
621700991125     C                   Z-ADD     TGCSGV        WIMI             30 9
621800991125     C                   EXSR      CNVEUR
621900991125     C                   Z-ADD     WIMO          TGCSGV
622000991125     C* SPESE RICONSEGNA CITTA'
622100991125     C                   Z-ADD     TGCSGR        WIMI             30 9
622200991125     C                   EXSR      CNVEUR
622300991125     C                   Z-ADD     WIMO          TGCSGR
622400991125     C* SPESE RICONSEGNA PROVINCIA
622500991125     C                   Z-ADD     TGCSGP        WIMI             30 9
622600991125     C                   EXSR      CNVEUR
622700991125     C                   Z-ADD     WIMO          TGCSGP
622800991125     C* SPESE DOSSIER
622900991125     C                   Z-ADD     TGCSGD        WIMI             30 9
623000991125     C                   EXSR      CNVEUR
623100991125     C                   Z-ADD     WIMO          TGCSGD
623200991125     C* 1° TARIFFA SOSTA
623300991125     C                   Z-ADD     TTS(1)        WIMI             30 9
623400991125     C                   EXSR      CNVEUR
623500991125     C                   Z-ADD     WIMO          TTS(1)
623600991125     C* 2° TARIFFA SOSTA
623700991125     C                   Z-ADD     TTS(2)        WIMI             30 9
623800991125     C                   EXSR      CNVEUR
623900991125     C                   Z-ADD     WIMO          TTS(2)
624000991125     C* 3° TARIFFA SOSTA
624100991125     C                   Z-ADD     TTS(3)        WIMI             30 9
624200991125     C                   EXSR      CNVEUR
624300991125     C                   Z-ADD     WIMO          TTS(3)
624400991125     C* TARIFFA MINIMA SOSTA
624500991125     C                   Z-ADD     TGCSTM        WIMI             30 9
624600991125     C                   EXSR      CNVEUR
624700991125     C                   Z-ADD     WIMO          TGCSTM
624800991125     C*
624900991125     C                   ENDSR
625000011008     C*------------------------------------------------------------------------*
625100011023     C* CNVETC - CONVERTE I CAMPI DI TEETC IN DIVISA DI CALCOLO DEL CE
625200011008     C*------------------------------------------------------------------------*
625300011008     C     CNVETC        BEGSR
625400011008     C* ARROTONDAMENTO
625500011008     C                   Z-ADD     ETCIAR        WIMI             30 9
625600011008     C                   EXSR      CNVEUR
625700011008     C                   Z-ADD     WIMO          ETCIAR
625800011008     C* CONTRASSEGNO
625900011008     C                   Z-ADD     ETCCAS        WIMI             30 9
626000011008     C                   EXSR      CNVEUR
626100011008     C                   Z-ADD     WIMO          ETCCAS
626200011008     C* ASSICURAZIONE
626300011008     C                   Z-ADD     ETCASC        WIMI             30 9
626400011008     C                   EXSR      CNVEUR
626500011008     C                   Z-ADD     WIMO          ETCASC
626600011008     C*
626700011008     C                   ENDSR
626800011008     C*------------------------------------------------------------------------*
626900011008     C* CNVETS - CONVERTE I CAMPI DI TEETS IN DIVISA DI CALCOLO DEL CE
627000011008     C*------------------------------------------------------------------------*
627100011008     C     CNVETS        BEGSR
627200011008     C* INOLTRO
627300011008     C                   Z-ADD     ETSINL        WIMI             30 9
627400011008     C                   EXSR      CNVEUR
627500011008     C                   Z-ADD     WIMO          ETSINL
627600011008     C*
627700011008     C                   ENDSR
627800991125     C*------------------------------------------------------------------------*
627900011008     C* CNVEUR - CONVERTE I CAMPI IN DIVISA DI CALCOLO DEL CE
628000991125     C*------------------------------------------------------------------------*
628100991125     C     CNVEUR        BEGSR
628200991125     C*
628300991125     C* AZZERA VARIABILI DI LAVORO
628400991125     C                   Z-ADD     *ZEROS        WIMO             30 9          *IMPORTO CONVERTITO
628500011008     C                   MOVEL     *BLANKS       NDEC              1            *N. DEC IN OUTPUT
628600011008     C*
628700011026     C* IMPOSTO I DECIMALI DI OUTPUT A SECONDA DELLA DIVISA DI OUTPUT (NON + VERO: SEMPREE MAX DEC)
628800011026     C*                  IF        §GEDCEDIV = 'ITL'
628900011026     C*                  EVAL      NDEC = '0'
629000011026     C*                  ELSE
629100011026     C*                  EVAL      NDEC = '3'
629200011026     C*                  ENDIF
629300011026     C                   EVAL      NDEC = '9'
629400991125     C*
629500991125IF  1C     WIMI          IFNE      *ZEROS
629600991125     C                   CLEAR                   YEUR
629700991125     C                   MOVEL     WDVI          YECDVI                         *DIVISA IMPORTO
629800991125     C                   Z-ADD     WIMI          YECIMI                         *IMPORTO DA CONVERTI
629900011026     C                   MOVEL     WDIVCE        YECDVO                         *DIVISA IMP CONVERTI
630000011008     C                   MOVEL     NDEC          YECDCO                         *N° DECIMALI
630100991125     C                   MOVEL     *BLANKS       YECTAR                         *ARROTONDAMENTO "H"
630200991125     C                   CALL      'YEURCO'
630300991125     C                   PARM                    YEUR
630400991125IF  2C     YECESI        IFEQ      ' '                                          *NO ERRORI
630500991125     C                   Z-ADD     YECIMO        WIMO                            -IMPORTO CONVERTITO
630600991125X   2C                   ELSE                                                   *SI ERRORI
630700991125     C                   Z-ADD     WIMI          WIMO                            -IMPOROTO DA CONVER
630800991125E   2C                   ENDIF
630900991125E   1C                   ENDIF
631000991125     C*
631100991125     C                   ENDSR
631200970306     C*------------------------------------------------------------------------*
631300970306     C* IMPERR - IMPOSTA ERRORE NELLA DS DI PROCEDURA
631400970306     C*------------------------------------------------------------------------*
631500970306     C     IMPERR        BEGSR
631600970306     C*
631700970306     C                   MOVEL     'S'           WERR                           *ERRORE
631800970306     C                   MOVEL     $MSG          D00MSG                         *MESSAGGIO PROCEDURA
631900970306     C                   MOVEL     '1'           D00ERR                         *ERRORE PROCEDURA
632000970306     C*
632100970306     C                   ENDSR
632200970715     C*------------------------------------------------------------------------*
632300970715     C* FINSR - OPERAZIONI FINALI
632400970715     C*------------------------------------------------------------------------*
632500970715     C     FINSR         BEGSR
632600970715     C*
632700970715     C* CHIUDE I PROGRAMMA DI UTILITA' RICHIAMATI
632800970715     C                   CLEAR                   DSSI95                         *CONTROLLA CAP
632900970715     C                   MOVEL     'C'           I95TLA
633000970715     C                   CALL      'TISI95R'
633100970715     C                   PARM                    DSSI95
633200970715     C*
633300970715     C                   CLEAR                   DSLV55                         *RECUPERA TERMINAL
633400970715     C                   MOVEL     'C'           D55TLA
633500970715     C                   CALL      'FNLV55R'
633600970715     C                   PARM                    DSLV55
633700970715     C*
633800970715     C                   ENDSR
633900011008     C*------------------------------------------------------------------------*
634000011008     C* CARGEDCE - CARICAMENTO TABELLA "GED" TIPO RECORD "CE" E CONSIDERAZIONI SU DIVISE
634100011008     C*------------------------------------------------------------------------*
634200011008     C     CARGEDCE      BEGSR
634300011008     C*
634400011008     C                   CLEAR                   DSGEDCE
634500011026     C                   MOVEL     *blanks       WDIVCE            3
634600011008     C*
634700011008     C                   MOVEL(P)  'GED'         KTECOD
634800011008     C                   MOVEL(P)  'CE'          KTEKE1
634900011008     C     KEYTBE        CHAIN     TNTBE01L
635000011008     C                   IF        %found(TNTBE01L)
635100011008     C                   MOVEL     TBEUNI        DSGEDCE
635200011008     C                   ENDIF
635300011008     C*
635400011008     C                   IF        §GEDCEDAT > D00DTC
635500011026     C                   EVAL      WDIVCE = §GEDCEDIP
635600011026     C                   ELSE
635700011026     C                   EVAL      WDIVCE = §GEDCEDIV
635800011008     C                   ENDIF
635900011008     C*
636000011008     C                   ENDSR
636100040716     C*------------------------------------------------------------------------*
636200040716     C* CARGEIEUR- CARICAMENTO TABELLA "GEI" TIPO RECORD "EUR"
636300040716     C*------------------------------------------------------------------------*
636400040716     C     CARGEIEUR     BEGSR
636500040716     C*
636600040716     C                   CLEAR                   DGEI
636700040716     C*
636800040716     C                   MOVEL(P)  'GEI'         KTECOD
636900040716     C                   MOVEL(P)  'EUR'         KTEKE1
637000040716     C     KEYTBE        CHAIN     TNTBE01L
637100040716     C                   IF        %found(TNTBE01L)
637200040716     C                   MOVEL     TBEUNI        DGEI
637300040716     C                   ENDIF
637400040716     C*
637500040716     C                   ENDSR
637600970306     C*------------------------------------------------------------------------*
637700970306     C* *INZSR - OPERAZIONI INIZIALI
637800970306     C*------------------------------------------------------------------------*
637900970306     C     *INZSR        BEGSR
638000970306     C*--------------------
638100970306     C* RICEVIMENTO PARAMETRI
638200970306     C*--------------------
638300970306     C     *ENTRY        PLIST
638400970306     C                   PARM                    KPJBA
638500970306     C                   PARM                    DSTE00
638600041202     c* apertura controllata di alcuni file xchè solo di filiale
638700041202     c                   open      tiogc01l                             95
638800041202     c                   open      tiopd02l                             95
638900041202     c                   open      tiopt01l                             95
639000041202     c                   open      tnofm00l                             95
639100970306     C*--------------------
639200970306     C* VARIABILI RIFERITE AL DATA BASE
639300970306     C*--------------------
639400010926     C     *LIKE         DEFINE    ETCDSF        KTCDSF                         *TEETC00F
639500980603     C     *LIKE         DEFINE    ETCCTO        KTCCTO
639600970306     C     *LIKE         DEFINE    ETCKSC        KTCKSC
639700970306     C     *LIKE         DEFINE    ETCCTR        KTCCTR
639800970306     C     *LIKE         DEFINE    ETCPRG        KTCPRG
639900970306     C     *LIKE         DEFINE    ETCLNP        KTCLNP
640000010926     C     *LIKE         DEFINE    ETSDSF        KTSDSF                         *TEETC00F
640100980603     C     *LIKE         DEFINE    ETSCTO        KTSCTO
640200970306     C     *LIKE         DEFINE    ETSKSC        KTSKSC
640300970306     C     *LIKE         DEFINE    ETSCTR        KTSCTR
640400970306     C     *LIKE         DEFINE    ETSPRG        KTSPRG
640500970306     C     *LIKE         DEFINE    ETSLNP        KTSLNP
640600970306     C     *LIKE         DEFINE    ETSSGL        KTSSGL
640700010926     C     *LIKE         DEFINE    ETTDSF        KTTDSF                         *TEETC00F
640800980603     C     *LIKE         DEFINE    ETTCTO        KTTCTO
640900970306     C     *LIKE         DEFINE    ETTKSC        KTTKSC
641000970306     C     *LIKE         DEFINE    ETTCTR        KTTCTR
641100970306     C     *LIKE         DEFINE    ETTPRG        KTTPRG
641200970306     C     *LIKE         DEFINE    ETTLNP        KTTLNP
641300970306     C     *LIKE         DEFINE    ETTFIE        KTTFIE
641400970306     C     *LIKE         DEFINE    ETTCTS        KTTCTS
641500010926     C     *LIKE         DEFINE    ETWUSR        KTWUSR                         TEETW00F
641600980603     C     *LIKE         DEFINE    ETWDSF        KTWDSF
641700980603     C     *LIKE         DEFINE    ETWCTO        KTWCTO
641800970306     C     *LIKE         DEFINE    ETWKSC        KTWKSC
641900970306     C     *LIKE         DEFINE    ETWCTR        KTWCTR
642000970306     C     *LIKE         DEFINE    ETWPRG        KTWPRG
642100970306     C     *LIKE         DEFINE    ETWSGL        KTWSGL
642200970306     C     *LIKE         DEFINE    ETWCTS        KTWCTS
642300970306     C     *LIKE         DEFINE    ETWRDL        KTWRDL
642400010926     C     *LIKE         DEFINE    EWRUSR        KWRUSR                         TEETWR0F
642500970306     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
642600970306     C     *LIKE         DEFINE    TBLCOD        KTBCOD
642700970306     C     *LIKE         DEFINE    TBLKEY        KTBKEY
642800970306     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
642900970306     C     *LIKE         DEFINE    TAMKSC        KTMKSC                         *TNTAM00F
643000970306     C     *LIKE         DEFINE    TAMCTR        KTMCTR
643100970306     C     *LIKE         DEFINE    TAMPRG        KTMPRG
643200990819     C     *LIKE         DEFINE    TADKSC        KTDKSC                         *TITAD00F
643300970306     C     *LIKE         DEFINE    TADPRG        KTDPRG
643400970306     C     *LIKE         DEFINE    TADLNP        KTDLNP
643500970306     C     *LIKE         DEFINE    TADCTS        KTDCTS
643600990819     C     *LIKE         DEFINE    TADNAZ        KTDNAZ
643700990819     C     *LIKE         DEFINE    TADCAP        KTDCAP
643800970306     C     *LIKE         DEFINE    TADCTR        KTDCTR
643900990819     C     *LIKE         DEFINE    TPTKSC        KTPKSC                         *TITPT00F
644000970306     C     *LIKE         DEFINE    TPTCTR        KTPCTR
644100970306     C     *LIKE         DEFINE    TPTPRG        KTPPRG
644200970306     C     *LIKE         DEFINE    TPTFTC        KTPFTC
644300990819     C     *LIKE         DEFINE    TPDKSC        KDPKSC                         *TITPD00F
644400970306     C     *LIKE         DEFINE    TPDCTR        KDPCTR
644500970306     C     *LIKE         DEFINE    TPDPRG        KDPPRG
644600970306     C     *LIKE         DEFINE    TPDFTC        KDPFTC
644700970306     C     *LIKE         DEFINE    TPDCTS        KDPCTS
644800990819     C     *LIKE         DEFINE    TGCKSC        KGCKSC                         *TITGC00F
644900970306     C     *LIKE         DEFINE    TGCCTR        KGCCTR
645000970306     C     *LIKE         DEFINE    TGCPRG        KGCPRG
645100970306     C     *LIKE         DEFINE    TAMKSC        CARKSC                         *DI DEPOSITO
645200970306     C     *LIKE         DEFINE    TAMCTR        CARCTR
645300970306     C     *LIKE         DEFINE    TAMPRG        CARPRG
645400970306     C     *LIKE         DEFINE    TAMKSC        DEPKSC
645500970306     C     *LIKE         DEFINE    TAMCTR        DEPPRG
645600970306     C     *LIKE         DEFINE    TAMPRG        DEPCTR
645700970721     C     *LIKE         DEFINE    TPDAIN        DEPAIN
645800970721     C     *LIKE         DEFINE    TPDAIN        CPLAIN
645900011030     C     *LIKE         DEFINE    TPDAIN        CISAIN
646000011031     C     *LIKE         DEFINE    TPDAIN        SAMAIN
646100020729     C     *LIKE         DEFINE    §NTWFIE       DLPFL1
646200980506     C     *LIKE         DEFINE    §TRATA        DMITAS                                     ABILE ST
646300980506     C     *LIKE         DEFINE    §TRARR        DTRARR                                     ABILE ST
646400980506     C     *LIKE         DEFINE    §TRRPV        DTRRPV                                     ABILE ST
646500970306     C     *LIKE         DEFINE    ETTCTS        LCTS                           *DI LAVORO
646600970306     C     *LIKE         DEFINE    TAMKSC        KSC1
646700970306     C     *LIKE         DEFINE    TAMCTR        CTR1
646800970715     C     *LIKE         DEFINE    D55TFA        WTFA
646900970715     C     *LIKE         DEFINE    D55TFP        WTFP
647000970721     C     *LIKE         DEFINE    ORGCTS        WMCT
647100011008     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
647200011008     C     *LIKE         DEFINE    TBEKE1        KTEKE1
647300970306     C*--------------------
647400970306     C* CHIAVI DI LETTURA
647500970306     C*--------------------
647600010926     C* LETTURA TEETC01L - COMPLETA
647700970306     C     KEYETC        KLIST
647800980603     C                   KFLD                    KTCDSF                         *DATI SEDE O FILIALE
647900980603     C                   KFLD                    KTCCTO                         *TARIFFA O OFFERTA
648000970306     C                   KFLD                    KTCKSC                         *CLIENTE O VISITA
648100970306     C                   KFLD                    KTCCTR                         *TARIFFA
648200970306     C                   KFLD                    KTCPRG                         *PROGRESSIVO
648300970306     C                   KFLD                    KTCLNP                         *LINEA PARTENZA
648400010926     C* LETTURA TEETS01L - COMPLETA
648500970306     C     KEYETS        KLIST
648600980603     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
648700970306     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
648800970306     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
648900970306     C                   KFLD                    KTSCTR                         *TARIFFA
649000970306     C                   KFLD                    KTSPRG                         *PROGRESSIVO
649100970306     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
649200970306     C                   KFLD                    KTSSGL                         *SCAGLIONE
649300010926     C* LETTURA TEETS01L - PARZIALE
649400970306     C     KE2ETS        KLIST
649500980603     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
649600970306     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
649700970306     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
649800970306     C                   KFLD                    KTSCTR                         *TARIFFA
649900970306     C                   KFLD                    KTSPRG                         *PROGRESSIVO
650000970306     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
650100011017     C* LETTURA TEETT01L - COMPLETA
650200970306     C     KEYETT        KLIST
650300980603     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
650400970306     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
650500970306     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
650600970306     C                   KFLD                    KTTCTR                         *TARIFFA
650700970306     C                   KFLD                    KTTPRG                         *PROGRESSIVO
650800970306     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
650900970306     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
651000970306     C                   KFLD                    KTTCTS                         *CODICE TASSAZIONE
651100011017     C* LETTURA TEETT01L - PARZIALE
651200970306     C     KE2ETT        KLIST
651300980603     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
651400970306     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
651500970306     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
651600970306     C                   KFLD                    KTTCTR                         *TARIFFA
651700970306     C                   KFLD                    KTTPRG                         *PROGRESSIVO
651800970306     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
651900970306     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
652000010926     C* LETTURA TEETW01L - COMPLETA
652100970306     C     KEYETW        KLIST
652200970306     C                   KFLD                    KTWUSR                         UTENTE
652300980603     C                   KFLD                    KTWDSF                         *DATI SEDE O FILIALE
652400970306     C                   KFLD                    KTWCTO                         *TARIFFA O OFFERTA
652500970306     C                   KFLD                    KTWKSC                         *CLIENTE O VISITA
652600970306     C                   KFLD                    KTWCTR                         *TARIFFA
652700970306     C                   KFLD                    KTWPRG                         *PROGRESSIVO
652800970306     C                   KFLD                    KTWSGL                         *SCAGLIONE
652900970306     C                   KFLD                    KTWCTS                         *CODICE TASSAZIONE
653000970306     C                   KFLD                    KTWRDL                         *RAGGRUPPAMENTO DELT
653100010926     C* LETTURA TEETW01L - PARZIALE
653200970317     C     KE2ETW        KLIST
653300970317     C                   KFLD                    KTWUSR                         UTENTE
653400010926     C* LETTURA TEETWR1L - PARZIALE
653500970306     C     KEYEWR        KLIST
653600970306     C                   KFLD                    KWRUSR                         *UTENTE
653700970306     C* LETTURA TABEL00F - COMPLETA
653800970306     C     KEYTAB        KLIST
653900970306     C                   KFLD                    KTBKUT                         *UTENTE
654000970306     C                   KFLD                    KTBCOD                         *CODICE
654100970306     C                   KFLD                    KTBKEY                         *CHIAVE
654200970306     C* LETTURA TABEL00F - PARZIALE
654300970306     C     KE2TAB        KLIST
654400970306     C                   KFLD                    KTBKUT                         *UTENTE
654500970306     C                   KFLD                    KTBCOD                         *CODICE
654600970306     C* LETTURA AZORG01L - COMPLETA
654700970306     C     KEYORG        KLIST
654800970306     C                   KFLD                    KORFIL                         *FILIALE
654900970306     C* LETTURA TNTAM01L - PARZIALE
655000970306     C     KEYTAM        KLIST
655100970306     C                   KFLD                    KTMKSC                         *CLIENTE
655200970306     C                   KFLD                    KTMCTR                         *TARIFFA
655300970306     C* LETTURA TNTAM01L - COMPLETA
655400970306     C     KE2TAM        KLIST
655500970306     C                   KFLD                    KTMKSC                         *CLIENTE
655600970306     C                   KFLD                    KTMCTR                         *TARIFFA
655700970306     C                   KFLD                    KTMPRG                         *PROGRESSIVO
655800990819     C* LETTURA TITAD01L - PARZIALE
655900970306     C     KEYTAD        KLIST
656000970306     C                   KFLD                    KTDKSC                         *CLIENTE
656100970306     C                   KFLD                    KTDPRG                         *PROGRESSIVO
656200970306     C                   KFLD                    KTDLNP                         *LINEA DI PARTENZA
656300970306     C                   KFLD                    KTDCTS                         *CODICE TASSAZIONE
656400990819     C                   KFLD                    KTDNAZ                         *NAZIONE
656500990819     C                   KFLD                    KTDCAP                         *C.A.P.
656600970306     C                   KFLD                    KTDCTR                         *TARIFFA
656700990819     C* LETTURA TITPT01L - COMPLETA
656800970306     C     KEYTPT        KLIST
656900970306     C                   KFLD                    KTPKSC                         *CLIENTE
657000970306     C                   KFLD                    KTPCTR                         *TARIFFA
657100970306     C                   KFLD                    KTPPRG                         *PROGRESSIVO
657200970306     C                   KFLD                    KTPFTC                         *CONSEGNA PARTICOLAR
657300990819     C* LETTURA TITPD02L - PARZIALE
657400970306     C     KEYTPD        KLIST
657500970306     C                   KFLD                    KDPKSC                         *CLIENTE
657600970306     C                   KFLD                    KDPCTR                         *TARIFFA
657700970306     C                   KFLD                    KDPPRG                         *PROGRESSIVO
657800970306     C                   KFLD                    KDPFTC                         *CONSEGNA PARTICOLAR
657900970306     C                   KFLD                    KDPCTS                         *CODICE TASSAZIONE
658000990819     C* LETTURA TITGC01L - COMPLETA
658100970306     C     KEYTGC        KLIST
658200970306     C                   KFLD                    KGCKSC                         *CLIENTE
658300970306     C                   KFLD                    KGCCTR                         *TARIFFA
658400970306     C                   KFLD                    KGCPRG                         *PROGRESSIVO
658500011008     C* LETTURA TNTBE01L - PARZIALE
658600011008     C     KEYTBE        KLIST
658700011008     C                   KFLD                    KTECOD                         *UTENTE
658800011008     C                   KFLD                    KTEKE1                         *CODICE
658900970306     C*--------------------
659000970306     C* VARIABILI DI CONTROLLO
659100970306     C*--------------------
659200970306     C                   MOVEL     'N'           WERR              1            *ERRORE
659300970306     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGIO
659400970306     C                   MOVEL     '0'           $TGC              1            *TAR.GIACENZA  ESIST
659500970306     C                   MOVEL     '0'           $TAD              1            *TAR.DETTAGLIO ESIST
659600970306     C                   MOVEL     '0'           $TPT              1            *TAR.PART.TES. ESIST
659700970306     C                   MOVEL     '0'           $TPD              1            *TAR.PART.DET. ESIST
659800011023     C                   MOVEL     D00DIV        WDIV              3            *DIVISA CALCOLO CAT
659900970306     C*
660000970306     C* REPERISCE LA DATA CORRENTE
660100970306     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
660200970306     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
660300970306     C                   Z-ADD     WN8           G08DAT
660400970306     C                   Z-ADD     *ZEROS        G08INV
660500970306     C                   MOVEL     '0'           G08ERR
660600970306     C                   CALL      'XSRDA8'
660700970306     C                   PARM                    WLBDA8
660800970306     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
660900011008     C*
661000011008     C* RECUPERA DATI DA TEBELLA "GED" TIPO RECORD "CE"
661100011008     C                   EXSR      CARGEDCE
661200040716     C* RECUPERA DATI DA TEBELLA "GEI" TIPO RECORD "EUR"
661300040716     C                   EXSR      CARGEIEUR
661400971204     C*
661500971204     C* RECUPERA INSTRADAMENTO -PARTENZA-
661600971204     C                   EXSR      REPINP
661700970306     C*
661800970306     C                   ENDSR
661900970306     O*------------------------------------------------------------------------*
662000970306** MSG - MESSAGGI
662100970306TNTE11R- Dati tipo del cliente/tariffa non trovati                            1
662200970306TNTE11R- Scaglioni del cliente/tariffa non trovati                            2
662300970306TNTE11R- Codici tassazione del cliente/tariffa non trovati                    3
662400970306TNTE11R- Codice tassazione non trovato                                        4
662500970715TNTE11R- La linea di partenza della tariffa non è stata trovata               5
662600970715TNTE11R- 6
662700970715TNTE11R- 7
662800970306TNTE11R- 8
662900970306TNTE11R- 9
663000970306TNTE11R- 10
663100970306TNTE11R- Tabella QT1 non trovata. Avvertire il C.E.D.                         11
663200970306TNTE11R- Tabella QT2 non trovata. Avvertire il C.E.D.                         12
663300970306TNTE11R- Tabella QI1 non trovata. Avvertire il C.E.D.                         13
663400970306TNTE11R- Tabella QI2 non trovata. Avvertire il C.E.D.                         14
663500970306TNTE11R- Tabella TR  non trovata. Avvertire il C.E.D.                         15
663600970306TNTE11R- Tabella $2  non trovata. Avvertire il C.E.D.                         16
663700970306TNTE11R- Tabella CV  non trovata. Avvertire il C.E.D.                         17
663800970306TNTE11R- Tabella CT  non trovata. Avvertire il C.E.D.                         18
663900970306TNTE11R- Tabella 5E  non trovata. Avvertire il C.E.D.                         19
664000970306TNTE11R- Tabella 1P  non trovata. Avvertire il C.E.D.                         20
664100970306TNTE11R- 21
664200970306TNTE11R- MANCA TARIFFA. Tariffa di cartello inesistente.                      22
664300970306TNTE11R- MANCA TARIFFA. Tariffa giacenza non trovata.                         23
664400970306TNTE11R- MANCA TARIFFA. Tariffa dettaglio cliente non trovata.                24
664500970306TNTE11R- MANCA TARIFFA. Scaglione di peso in tariffa non trovato.             25
664600001218TNTE11R- MANCA TARIFFA. Scaglione di peso in t.particolare non trovato.   -  -26
664700970306TNTE11R- 27
664800970306TNTE11R- 28
664900970306TNTE11R- 29
665000970306TNTE11R- 30
