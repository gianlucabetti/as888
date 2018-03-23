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
012800101018     D*-------------------
012900101018     D* DS pgm calcolo ISTAT
013000101018     D*-------------------
013100101018     d TrulISTds     e ds
013200970306     D*-------------------
013300970306     D* DS FORMATO RECORD TARIFFE TESTATA
013400970306     D*-------------------
013500970306     D TAMDS         E DS                  EXTNAME(TNTAM00F) INZ
013600970306     D*-------------------
013700970306     D* INFDS - INFORMAZIONI PROGRAMMA
013800970306     D*-------------------
013900970306     D                SDS
014000980909     D  JOB                  244    253
014100980909     D  USER                 254    263
014200970715     D*------------------
014300970715     D* DS "TISI95R" - CONTROLLO CAP
014400970715     D*------------------
014500970715     D DSSI95        E DS                  EXTNAME(TISI95DS)
014600970715     D*------------------
014700970715     D* DS "FNLV55R" - REPERIMENTO TERMINALs
014800970715     D*------------------
014900970715     D DSLV55        E DS                  EXTNAME(FNLV55DS)
015000970306     D*-------------------
015100970306     D* DS DI PROCEDURA
015200970306     D*-------------------
015300970306     D DSTE00        E DS                  EXTNAME(TNTE00DS)
015400991125     D*-------------------
015500991125     D* CONVERSIONI IMPORTI
015600991125     D*-------------------
015700991125     D YEUR          E DS                  EXTNAME(YEURCODS)
015800970306     D*-------------------
015900970306     D* ARCHITETTURA
016000970306     D*-------------------
016100970306     D KPJBA         E DS
016200970306     D*-------------------
016300970306     D* DS "XSRDA8" - CONTROLLO DATA (8)
016400970306     D*-------------------
016500970306     D WLBDA8          DS                  INZ
016600970306     D  G08DAT                 1      8  0
016700970306     D  G08INV                 9     16  0
016800970306     D  G08ERR                17     17
016900970306     D  G08TGI                18     22  0
017000021202     D*-------------------
017100021202     D* DS *PGM REPIMEMTO TARIFFE DI CARTELLO
017200021202     D*-------------------
017300021202     D TRUL27DS      E DS
017400970306     D*-------------------
017500970306     D* DS SIGLE VARIE
017600970306     D*-------------------
017700970306     D DS$2          E DS
017800970306     D*-------------------
017900980506     D* DS TIPI TARIFFA                     -15 TIPI TARIFFA-
018000970306     D*-------------------
018100980506     D DSTR          E DS                  OCCURS(15) INZ
018200970306     D*-------------------
018300050103     D* DS CODICI TASSAZIONE                -600 CODICI TASSAZIONE-
018400970306     D*-------------------
018500050103     D DSCT          E DS                  OCCURS(600) INZ
018600970306     D*-------------------
018700970306     D* DS TIPI SERVIZIO                    -10 TIPI SERVIZIO-
018800970306     D*-------------------
018900970306     D DS5E          E DS                  OCCURS(10) INZ
019000970306     D*-------------------
019100020104     D* DS CONSEGNE PARTICOLARI             -50 PARTICOLARITA'-
019200980706     D*-------------------
019300020104     D DS1P          E DS                  OCCURS(50) INZ
019400980706     D*-------------------
019500980706     D* FLAG OPERATIVI -TNTAM-
019600980706     D*-------------------
019700980706     D DSTA01        E DS
019800011005     D*------------------
019900011005     D* DS LETTURA TABELLA "GED" TIPO RECORD "CE"
020000011005     D*------------------
020100011005     D DSGEDCE       E DS
020200040716     D*------------------
020300040716     D* DS LETTURA TABELLA "GEI" TIPO RECORD "EUR" (limite risarcibile
020400040716     D*------------------
020500040716     D Dgei          E DS
020600970306     D*-------------------
020700020104     D* CONSEGNE PARTICOLARI DI CARTELLO    -50 PARTICOLARITA'-
020800970306     D*-------------------
020900020104     D CPIDS           DS                  OCCURS(50)
021000010926     D  TTPG8P                 1      1
021100011031     D  TARF8P                 2     22  8
021200011031     D  TARL8P                23     43  8
021300011031     D  TARO8P                44     64  8
021400011031     D  TRPV8P                65     67  1
021500011031     D  CP88                  68     68
021600011031     D  TCTS8P                69     70
021700011031     D  TCTE8P                71     72
021800011031     D  TAPL8P                73     73
021900970306     D*-------------------
022000970306     D* ASSICURAZIONE PER CONTO             -10 SCAGLIONI-
022100970306     D*-------------------
022200970306     D ACDS            DS                  OCCURS(10)
022300011031     D  ASGL                   1     21  8
022400011031     D  AITR                  22     42  8
022500011031     D  AMIN                  43     63  8
022600011031     D  AMAX                  64     84  8
022700011031     D  AAIN                  85     85
022800970306     D*-------------------
022900980707     D* DETTAGLIO TARIFFARIO CLIENTE        -11 SCAGLIONI-
023000970306     D*-------------------
023100980707     D TARDS           DS                  OCCURS(11)
023200990819     D  TLNP                   1      3  0
023300990819     D  TCTS                   4      5
023400990819     D  TNAZ                   6      8
023500990819     D  TCAP                   9     17
023600990819     D  TCTR                  18     20  0
023700011031     D  TSCG                  21     41  8
023800011031     D  TITR                  42     62  8
023900011031     D  TINO                  63     83  8
024000011031     D  TRPV                  84     86  1
024100011031     D  TMIN                  87    107  8
024200011031     D  TMAX                 108    128  8
024300001218     D*-------------------
024400001218     D* COMPOSIZIONI MESSAGGI
024500001218     D*-------------------
024600001218     D* MSG,26
024700001218     D                 DS
024800001218     D  DM26A                 76     77
024900001218     D  DMSG26                 1     78
025000020729     D*-------------------
025100020729     D* DS RIDEFINIZIONE DESCRIZIONE 143 ORGANIGRAMMA
025200020729     D*-------------------
025300020729     D OG143         E DS
025400020729     D*------------------
025500020729     D* DS PER LETTURA TABELLA "NTW"
025600020729     D*------------------
025700020729     D DNTW          E DS
025800041213     d tasadgs         s                   like(tasadg)
025900091026     d tpg             s                   like(tpttpg)
026000970306     I*------------------------------------------------------------------------*
026100970306     I* INPUT
026200970306     I*------------------------------------------------------------------------*
026300970306     I*-------------------
026400970306     I* DS FORMATO RECORD TARIFFE GIACENZA
026500970306     I*-------------------
026600990819     ITITGC000
026700970306     I              TGCSG1                      TGS(1)
026800970306     I              TGCSG2                      TGS(2)
026900970306     I              TGCSG3                      TGS(3)
027000970306     I              TGCST1                      TTS(1)
027100970306     I              TGCST2                      TTS(2)
027200970306     I              TGCST3                      TTS(3)
027300970625     I*-------------------
027400970625     I* DS FORMATO RECORD TARIFFE GIACENZA
027500970625     I*-------------------
027600990819     ITIOGC000
027700970625     I              TGCSG1                      TGS(1)
027800970625     I              TGCSG2                      TGS(2)
027900970625     I              TGCSG3                      TGS(3)
028000970625     I              TGCST1                      TTS(1)
028100970625     I              TGCST2                      TTS(2)
028200970625     I              TGCST3                      TTS(3)
028300970306     C*------------------------------------------------------------------------*
028400970306     C* MAIN LINE
028500970306     C*------------------------------------------------------------------------*
028600970306     C*
028700970306     C* CANCELLA IL FILE DI LAVORO
028800970306IF  1C     WERR          IFEQ      'N'
028900970306     C                   EXSR      CANCWF
029000970306E   1C                   ENDIF
029100970306     C*
029200970306     C* CARICA LE TABELLE OCCORRENTI
029300970306IF  1C     WERR          IFEQ      'N'
029400970306     C                   EXSR      CARTAB
029500970306E   1C                   ENDIF
029600970715     C*
029700970306     C* IMPOSTA LA SEQUENZA DI CALCOLO DI TUTTE LE VOCI
029800970306IF  1C     WERR          IFEQ      'N'
029900970306     C                   EXSR      IMPSEQ
030000970306E   1C                   ENDIF
030100970306     C*
030200970306     C* CARICA DATI TIPO -GENERALI-
030300970306IF  1C     WERR          IFEQ      'N'
030400970306     C                   EXSR      CARETC
030500970306E   1C                   ENDIF
030600970306     C*
030700970306     C* CARICA DATI TIPO -SCAGLIONI-
030800970306IF  1C     WERR          IFEQ      'N'
030900970306     C                   EXSR      CARETS
031000970306E   1C                   ENDIF
031100970306     C*
031200970306     C* CARICA DATI TIPO -CODICI TASSAZIONE-
031300970306IF  1C     WERR          IFEQ      'N'
031400970306     C                   EXSR      CARETT
031500970306E   1C                   ENDIF
031600970306     C*
031700970306     C* DETERMINA LE VOCI DA CALCOLARE
031800970306IF  1C     WERR          IFEQ      'N'
031900970306     C                   EXSR      CHKVOC
032000970306E   1C                   ENDIF
032100970306     C*
032200970306     C* CARICA LA TARIFFA DEL CLIENTE -TESTATA-
032300970306IF  1C     WERR          IFEQ      'N'
032400970306     C                   EXSR      CARTAM
032500970306E   1C                   ENDIF
032600970306     C*
032700970306     C* CARICA LA TARIFFA GIACENZA DEL CLIENTE
032800970306IF  1C     WERR          IFEQ      'N'
032900970306IF  2C     VC(8)         IFEQ      'S'
033000970306     C     VC(18)        OREQ      'S'
033100970306     C     VC(19)        OREQ      'S'
033200970306     C     VC(20)        OREQ      'S'
033300970306     C                   MOVEL     '0'           $TGC
033400970306     C                   EXSR      CARTGC
033500970306E   2C                   ENDIF
033600970306E   1C                   ENDIF
033700970306     C*
033800970306     C* VALORIZZA LE VOCI DA CALCOLARE -FISSE-
033900970306IF  1C     WERR          IFEQ      'N'
034000970306     C                   EXSR      VALVO1
034100970306E   1C                   ENDIF
034200970306     C*
034300970306     C* CICLO PER TUTTI GLI SCAGLIONI ----------------------------------------->
034400970306DO  1C     1             DO        WULTSC        S                 6 0
034500970306IF  2C     SSGL(S)       IFNE      *ZEROS
034600970306     C*
034700970306     C* DETERMINA LE VOCI DA CALCOLARE
034800970306IF  3C     WERR          IFEQ      'N'
034900970306     C                   EXSR      CHKVOC
035000970306E   3C                   ENDIF
035100970306     C*
035200970306     C* VALORIZZA LE VOCI DA CALCOLARE -DIPENDENTI DA SCAGLIONI-
035300970306IF  3C     WERR          IFEQ      'N'
035400970306     C                   EXSR      VALVO2
035500970306E   3C                   ENDIF
035600970306     C*
035700970306     C* CICLO PER TUTTI I CODICI TASSAZIONE ----------------------------------->
035800970306DO  3C                   DO        WULTCT        T                 6 0
035900970306IF  4C     CCTS(T)       IFNE      *BLANKS
036000970306     C*
036100970306     C* DETERMINA LE VOCI DA CALCOLARE
036200970306IF  5C     WERR          IFEQ      'N'
036300970306     C                   EXSR      CHKVOC
036400970306E   5C                   ENDIF
036500970306     C*
036600970306     C* CARICA LA TARIFFA DEL CLIENTE -DETTAGLIO-
036700970306IF  5C     WERR          IFEQ      'N'
036800970306     C                   EXSR      CARTAD
036900970306E   5C                   ENDIF
037000970306     C*
037100970306     C* REPERISCE E CALCOLA LA QUANTITA' DA UTILIZZARE
037200970306IF  5C     WERR          IFEQ      'N'
037300970306     C                   EXSR      CALQTA
037400970306E   5C                   ENDIF
037500970306     C*
037600970306     C* VALORIZZA LE VOCI DA CALCOLARE -DIPENDENTI DA CODICI TASSAZIONE-
037700970306IF  5C     WERR          IFEQ      'N'
037800970306     C                   EXSR      VALVO3
037900970306E   5C                   ENDIF
038000970306     C*
038100970306     C* CALCOLA IMPORTI SULLE % INCIDENZA E I RAGGUPPAMENTI DELTA
038200970306IF  5C     WERR          IFEQ      'N'
038300970306     C                   EXSR      CALTOT
038400970306E   5C                   ENDIF
038500970306     C*
038600011017     C* SCRIVE IL FILE DI LAVORO TEETW
038700970306IF  5C     WERR          IFEQ      'N'
038800970306     C                   EXSR      SCRETW
038900970306E   5C                   ENDIF
039000970306     C*
039100010926     C* SCRIVE IL FILE DI LAVORO TEETWR SE LANCIO IN DEBUG
039200970306IF  5C     WERR          IFEQ      'N'
039300970306     C     D00DBG        ANDEQ     'S'
039400970306     C                   EXSR      SCREWR
039500970306E   5C                   ENDIF
039600970306E   4C                   ENDIF
039700970306E   3C                   ENDDO                                                  *FINE COD.TASSAZIONE
039800970306     C*
039900970306E   2C                   ENDIF
040000970306E   1C                   ENDDO                                                  *FINE SCAGLIONI
040100980702     C*
040200970715     C* OPERAZIONI FINALI
040300970715     C                   EXSR      FINSR
040400970306     C*
040500970306     C                   SETON                                        LR
040600970306     C*------------------------------------------------------------------------*
040700970306     C* IMPSEQ - IMPOSTA LA SEQUENZA DI CALCOLO DI TUTTE LE VOCI
040800970306     C*------------------------------------------------------------------------*
040900970306     C     IMPSEQ        BEGSR
041000970306     C*
041100970306     C* DETERMINA IL CODICE VOCE
041200970306     C                   EXSR      CODVOC
041300970306     C*
041400970306     C* DETERMINA LA SEQUENZA DELLE VOCI
041500970306     C                   EXSR      SEQVOC
041600970306     C*
041700970306     C* DETERMINA IL RAGGRUPPAMENTO DELTA DELLE VOCI
041800970306     C                   EXSR      RAGVOC
041900970306     C*
042000970306     C                   ENDSR
042100970306     C*------------------------------------------------------------------------*
042200970306     C* CODVOC - DETERMINA IL CODICE DELLE VOCI
042300970306     C*------------------------------------------------------------------------*
042400970306     C     CODVOC        BEGSR
042500970306     C*
042600970306     C                   Z-ADD     001           VV(1)                          *PORTO
042700970721     C                   Z-ADD     002           VV(2)                          *DIRITTO FISSO
042800970717     C                   Z-ADD     003           VV(3)                          *ADG X PORTO
042900970306     C                   Z-ADD     004           VV(4)                          *ISTAT X PORTO
043000970721     C                   Z-ADD     005           VV(5)                          *TCP
043100970306     C                   Z-ADD     006           VV(6)                          *INOLTRO
043200970717     C                   Z-ADD     007           VV(7)                          *ESPRESSO SU PORTO
043300970721     C                   Z-ADD     008           VV(8)                          *GIACENZA
043400970306     C                   Z-ADD     009           VV(9)                          *ASS.CONTO FRANCO
043500970306     C                   Z-ADD     010           VV(10)                         *ASS.RCV   FRANCO
043600970306     C                   Z-ADD     011           VV(11)                         *CONS. APPUNTAMENTO
043700970306     C                   Z-ADD     012           VV(12)                         *CONS. SUPERMERCATI
043800970306     C                   Z-ADD     013           VV(13)                         *CONS. AI PIANI
043900970306     C                   Z-ADD     014           VV(14)                         *CONS. FUORI MISURA
044000970721     C                   Z-ADD     015           VV(15)                         *C/ASSEGNO MITTENTE
044100970721     C                   Z-ADD     016           VV(16)                         *C/ASSEGNO BARTOLINI
044200970306     C                   Z-ADD     017           VV(17)                         *RITIRO
044300970721     C                   Z-ADD     018           VV(18)                         *GIACENZA
044400970721     C                   Z-ADD     019           VV(19)                         *GIACENZA
044500970721     C                   Z-ADD     020           VV(20)                         *GIACENZA
044600970717     C                   Z-ADD     021           VV(21)                         *ADG X INOLTRO
044700970717     C                   Z-ADD     022           VV(22)                         *ADG X DIR.FISSO
044800970717     C                   Z-ADD     023           VV(23)                         *ADG X ESPRESSO/PORT
044900970306     C                   Z-ADD     024           VV(24)                         *ISTAT X INOLTRO
045000970306     C                   Z-ADD     025           VV(25)                         *ISTAT X DIR.FISSO
045100970717     C                   Z-ADD     026           VV(26)                         *ISTAT X ESPRES/PORT
045200970306     C                   Z-ADD     027           VV(27)                         *ISTAT X ADG/PORTO
045300970306     C                   Z-ADD     028           VV(28)                         *ISTAT X ADG/D.FISSO
045400970306     C                   Z-ADD     029           VV(29)                         *ISTAT X ADG/INOLTRO
045500970717     C                   Z-ADD     030           VV(30)                         *ISTAT X ADG/ESP/POR
045600970306     C                   Z-ADD     031           VV(31)                         *ASS.CONTO ASSEGNATO
045700970306     C                   Z-ADD     032           VV(32)                         *ASS.RCV ASSEGNATO
045800970414     C                   Z-ADD     033           VV(33)                         *CONS. DDT
045900970721     C                   Z-ADD     034           VV(34)                         *ISTAT X ADG/ESP/INO
046000970414     C                   Z-ADD     035           VV(35)                         *CONS. LOC.DISAGIATE
046100970717     C                   Z-ADD     036           VV(36)                         *ESPRESSO SU INOLTRO
046200970717     C                   Z-ADD     037           VV(37)                         *ADG X ESPRES/INOLTR
046300970717     C                   Z-ADD     038           VV(38)                         *ISTAT X ESPRES/INOL
046400970721     C                   Z-ADD     039           VV(39)                         *ESPRESSO SU L.DISAG
046500970721     C                   Z-ADD     040           VV(40)                         *ADG X ESPRES/L.DISA
046600970721     C                   Z-ADD     041           VV(41)                         *ISTAT X ESPRES/L.DI
046700970721     C                   Z-ADD     042           VV(42)                         *ISTAT X ADG/ESP/LDI
046800971023     C                   Z-ADD     043           VV(43)                         *ADG X L.DISAGIATE
046900971023     C                   Z-ADD     044           VV(44)                         *ISTAT X L.DISAGIATE
047000971023     C                   Z-ADD     045           VV(45)                         *ISTAT X ADG/L.DISAG
047100001219     C                   Z-ADD     046           VV(46)                         *ADG SU TCP
047200001219     C                   Z-ADD     047           VV(47)                         *ADG SU GIACENZA VAR
047300001219     C                   Z-ADD     048           VV(48)                         *ADG SU ASSIC X CONT
047400001219     C                   Z-ADD     049           VV(49)                         *ADG SU ASSIC RCV  F
047500001219     C                   Z-ADD     050           VV(50)                         *ADG SU CONSEGNA APP
047600001219     C                   Z-ADD     051           VV(51)                         *ADG SU CONSEGNA SUP
047700001219     C                   Z-ADD     052           VV(52)                         *ADG SU CONSEGNA PIA
047800001219     C                   Z-ADD     053           VV(53)                         *ADG SU CONSEGNA FUO
047900001219     C                   Z-ADD     054           VV(54)                         *ADG SU C/ASSEGNO MI
048000001219     C                   Z-ADD     055           VV(55)                         *ADG SU C/ASSEGNO BA
048100001219     C                   Z-ADD     056           VV(56)                         *ADG SU RITIRO
048200001219     C                   Z-ADD     057           VV(57)                         *ADG SU GIACENZA CON
048300001219     C                   Z-ADD     058           VV(58)                         *ADG SU GIACENZA CON
048400001219     C                   Z-ADD     059           VV(59)                         *ADG SU GIACENZA SOS
048500001219     C                   Z-ADD     060           VV(60)                         *ADG SU ASSIC X CONT
048600001219     C                   Z-ADD     061           VV(61)                         *ADG SU ASSIC RCV AS
048700001219     C                   Z-ADD     062           VV(62)                         *ADG SU CONSEGNA DDT
048800001219     C                   Z-ADD     063           VV(63)                         *ISTAT SU ADG/TCP
048900001219     C                   Z-ADD     064           VV(64)                         *ISTAT SU ADG/GIAC.V
049000001219     C                   Z-ADD     065           VV(65)                         *ISTAT SU ADG/ASSXCO
049100001219     C                   Z-ADD     066           VV(66)                         *ISTAT SU ADG/ASS RC
049200001219     C                   Z-ADD     067           VV(67)                         *ISTAT SU ADG/CONSEG
049300001219     C                   Z-ADD     068           VV(68)                         *ISTAT SU ADG/CONSEG
049400001219     C                   Z-ADD     069           VV(69)                         *ISTAT SU ADG/CONSEG
049500001219     C                   Z-ADD     070           VV(70)                         *ISTAT SU ADG/CONSEG
049600001219     C                   Z-ADD     071           VV(71)                         *ISTAT SU ADG/CASSEG
049700001219     C                   Z-ADD     072           VV(72)                         *ISTAT SU ADG/CASSEG
049800001219     C                   Z-ADD     073           VV(73)                         *ISTAT SU ADG/RITIRO
049900001219     C                   Z-ADD     074           VV(74)                         *ISTAT SU ADG/GIACEN
050000001219     C                   Z-ADD     075           VV(75)                         *ISTAT SU ADG/GIACEN
050100001219     C                   Z-ADD     076           VV(76)                         *ISTAT SU ADG/GIACEN
050200001219     C                   Z-ADD     077           VV(77)                         *ISTAT SU ADG/ASSIC
050300001219     C                   Z-ADD     078           VV(78)                         *ISTAT SU ADG/ASSIC
050400001219     C                   Z-ADD     079           VV(79)                         *ISTAT SU ADG/CONSEG
050500011030     C                   Z-ADD     080           VV(80)                         *CONS.ISOLA
050600011030     C                   Z-ADD     081           VV(81)                         *ESPRESSO SU C.ISOLA
050700011030     C                   Z-ADD     082           VV(82)                         *ADG X ESPRES/C.ISOL
050800011030     C                   Z-ADD     083           VV(83)                         *ISTAT X ESPRES/C.IS
050900011030     C                   Z-ADD     084           VV(84)                         *ISTAT X ADG/ESP/CIS
051000011030     C                   Z-ADD     085           VV(85)                         *ADG X CONS.ISOLA
051100011030     C                   Z-ADD     086           VV(86)                         *ISTAT X CONS.ISOLA
051200011030     C                   Z-ADD     087           VV(87)                         *ISTAT X ADG/R.ISOLA
051300011030     C                   Z-ADD     088           VV(88)                         *RITIRO ISOLA
051400011030     C                   Z-ADD     089           VV(89)                         *ESPRESSO SU R.ISOLA
051500011030     C                   Z-ADD     090           VV(90)                         *ADG X ESPRES/R.ISOL
051600011030     C                   Z-ADD     091           VV(91)                         *ISTAT X ESPRES/R.IS
051700011030     C                   Z-ADD     092           VV(92)                         *ISTAT X ADG/ESP/RIS
051800011030     C                   Z-ADD     093           VV(93)                         *ADG X RITIRO ISOLA
051900011030     C                   Z-ADD     094           VV(94)                         *ISTAT X RITIROISOLA
052000011030     C                   Z-ADD     095           VV(95)                         *ISTAT X ADG/R.ISOLA
052100040826     C                   Z-ADD     096           VV(96)                         *ISTAT X ADG/R.ISOLA
052200040826     C                   Z-ADD     097           VV(97)                         *ISTAT X ADG/R.ISOLA
052300040826     C                   Z-ADD     098           VV(98)                         *ISTAT X ADG/R.ISOLA
052400040826     C                   Z-ADD     099           VV(99)                         *ISTAT X ADG/R.ISOLA
052500040826     C                   Z-ADD     100           VV(100)                        *ISTAT X ADG/R.ISOLA
052600040826     C                   Z-ADD     101           VV(101)                        *ISTAT X ADG/R.ISOLA
052700040826     C                   Z-ADD     102           VV(102)                        *ISTAT X ADG/R.ISOLA
052800040826     C                   Z-ADD     103           VV(103)                        *ISTAT X ADG/R.ISOLA
052900040826     C                   Z-ADD     104           VV(104)                        *ISTAT X ADG/R.ISOLA
053000060314     C                   Z-ADD     105           VV(105)                        *ISTAT X ADG/R.ISOLA
053100060314     C                   Z-ADD     106           VV(106)                        *ISTAT X ADG/R.ISOLA
053200060314     C                   Z-ADD     107           VV(107)                        *ISTAT X ADG/R.ISOLA
053300060314     C                   Z-ADD     108           VV(108)                        *ISTAT X ADG/R.ISOLA
053400060314     C                   Z-ADD     109           VV(109)                        *ISTAT X ADG/R.ISOLA
053500060314     C                   Z-ADD     110           VV(110)                        *ISTAT X ADG/R.ISOLA
053600971023     C*
053700060314     C                   Z-ADD     110           NVOCA             3 0          *N° VOCI DA CALCOLAR
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
065700060314     C                   Z-ADD     3             VS(105)                        *ISTAT X ADG/R.ISOLA
065800060314     C                   Z-ADD     3             VS(106)                        *ISTAT X ADG/R.ISOLA
065900060314     C                   Z-ADD     3             VS(107)                        *ISTAT X ADG/R.ISOLA
066000060314     C                   Z-ADD     3             VS(108)                        *ISTAT X ADG/R.ISOLA
066100060314     C                   Z-ADD     3             VS(109)                        *ISTAT X ADG/R.ISOLA
066200060314     C                   Z-ADD     3             VS(110)                        *ISTAT X ADG/R.ISOLA
066300011030     C*
066400970306     C                   ENDSR
066500970306     C*------------------------------------------------------------------------*
066600970306     C* RAGVOC - DETERMINA IL RAGGRUPPAMENTO DELTA DELLE VOCI
066700970306     C*------------------------------------------------------------------------*
066800970306     C     RAGVOC        BEGSR
066900970717     C* PORTO
067000970717     C                   Z-ADD     001           VR(1)                          *PORTO
067100970717     C* EXTRA-PORTO
067200980702     C                   Z-ADD     002           VR(2)                          *DIRITTO FISSO
067300970717     C                   Z-ADD     002           VR(3)                          *ADG X PORTO
067400970717     C                   Z-ADD     002           VR(4)                          *ISTAT X PORTO
067500970717     C                   Z-ADD     002           VR(5)                          *TCP
067600970717     C                   Z-ADD     002           VR(6)                          *INOLTRO
067700970717     C                   Z-ADD     002           VR(17)                         *RITIRO
067800970717     C                   Z-ADD     002           VR(21)                         *ADG X INOLTRO
067900970717     C                   Z-ADD     002           VR(23)                         *ADG X ESPRES/PORTO
068000980702     C                   Z-ADD     002           VR(22)                         *ADG X DIR.FISSO
068100970717     C                   Z-ADD     002           VR(24)                         *ISTAT X INOLTRO
068200980702     C                   Z-ADD     002           VR(25)                         *ISTAT X DIR.FISSO
068300970717     C                   Z-ADD     002           VR(26)                         *ISTAT X ESPRES/PORT
068400970717     C                   Z-ADD     002           VR(27)                         *ISTAT X ADG/PORTO
068500970717     C                   Z-ADD     002           VR(28)                         *ISTAT X ADG/D.FISSO
068600970717     C                   Z-ADD     002           VR(29)                         *ISTAT X ADG/INOLTRO
068700970717     C                   Z-ADD     002           VR(30)                         *ISTAT X ADG/ESP/POR
068800970721     C                   Z-ADD     002           VR(34)                         *ISTAT X ADG/ESP/INO
068900970717     C                   Z-ADD     002           VR(37)                         *ADG X ESPRES/INOLT
069000970717     C                   Z-ADD     002           VR(38)                         *ISTAT X ESPRES/INOL
069100970721     C                   Z-ADD     002           VR(40)                         *ADG X ESPRES/L.DISA
069200970721     C                   Z-ADD     002           VR(41)                         *ISTAT X ESPRES/L.DI
069300970721     C                   Z-ADD     002           VR(42)                         *ISTAT X ADG/ESP/LDI
069400971023     C                   Z-ADD     002           VR(43)                         *ADG X L.DISAGIATE
069500971023     C                   Z-ADD     002           VR(44)                         *ISTAT X L.DISAGIATE
069600971023     C                   Z-ADD     002           VR(45)                         *ISTAT X ADG/L.DISAG
069700001219     C                   Z-ADD     002           VR(46)                         *ADG SU TCP
069800001219     C                   Z-ADD     002           VR(47)                         *ADG SU GIACENZA VAR
069900001219     C                   Z-ADD     002           VR(48)                         *ADG SU ASSIC X CONT
070000001219     C                   Z-ADD     002           VR(49)                         *ADG SU ASSIC RCV  F
070100001219     C                   Z-ADD     002           VR(50)                         *ADG SU CONSEGNA APP
070200001219     C                   Z-ADD     002           VR(51)                         *ADG SU CONSEGNA SUP
070300001219     C                   Z-ADD     002           VR(52)                         *ADG SU CONSEGNA PIA
070400001219     C                   Z-ADD     002           VR(53)                         *ADG SU CONSEGNA FUO
070500001219     C                   Z-ADD     002           VR(54)                         *ADG SU C/ASSEGNO MI
070600001219     C                   Z-ADD     002           VR(55)                         *ADG SU C/ASSEGNO BA
070700001219     C                   Z-ADD     002           VR(56)                         *ADG SU RITIRO
070800001219     C                   Z-ADD     002           VR(57)                         *ADG SU GIACENZA CON
070900001219     C                   Z-ADD     002           VR(58)                         *ADG SU GIACENZA CON
071000001219     C                   Z-ADD     002           VR(59)                         *ADG SU GIACENZA SOS
071100001219     C                   Z-ADD     002           VR(60)                         *ADG SU ASSIC X CONT
071200001219     C                   Z-ADD     002           VR(61)                         *ADG SU ASSIC RCV AS
071300001219     C                   Z-ADD     002           VR(62)                         *ADG SU CONSEGNA DDT
071400001219     C                   Z-ADD     002           VR(63)                         *ISTAT SU ADG/TCP
071500001219     C                   Z-ADD     002           VR(64)                         *ISTAT SU ADG/GIAC.V
071600001219     C                   Z-ADD     002           VR(65)                         *ISTAT SU ADG/ASSXCO
071700001219     C                   Z-ADD     002           VR(66)                         *ISTAT SU ADG/ASS RC
071800001219     C                   Z-ADD     002           VR(67)                         *ISTAT SU ADG/CONSEG
071900001219     C                   Z-ADD     002           VR(68)                         *ISTAT SU ADG/CONSEG
072000001219     C                   Z-ADD     002           VR(69)                         *ISTAT SU ADG/CONSEG
072100001219     C                   Z-ADD     002           VR(70)                         *ISTAT SU ADG/CONSEG
072200001219     C                   Z-ADD     002           VR(71)                         *ISTAT SU ADG/CASSEG
072300001219     C                   Z-ADD     002           VR(72)                         *ISTAT SU ADG/CASSEG
072400001219     C                   Z-ADD     002           VR(73)                         *ISTAT SU ADG/RITIRO
072500001219     C                   Z-ADD     002           VR(74)                         *ISTAT SU ADG/GIACEN
072600001219     C                   Z-ADD     002           VR(75)                         *ISTAT SU ADG/GIACEN
072700001219     C                   Z-ADD     002           VR(76)                         *ISTAT SU ADG/GIACEN
072800001219     C                   Z-ADD     002           VR(77)                         *ISTAT SU ADG/ASSIC
072900001219     C                   Z-ADD     002           VR(78)                         *ISTAT SU ADG/ASSIC
073000001219     C                   Z-ADD     002           VR(79)                         *ISTAT SU ADG/CONSEG
073100011030     C                   Z-ADD     002           VR(82)                         *ADG X ESPR/C.ISOLA
073200011030     C                   Z-ADD     002           VR(83)                         *ISTAT X ESPRES/C.IS
073300011030     C                   Z-ADD     002           VR(84)                         *ISTAT X ADG/ESP/CIS
073400011030     C                   Z-ADD     002           VR(85)                         *ADG X CONS.ISOLA
073500011030     C                   Z-ADD     002           VR(86)                         *ISTAT X CONS.ISOLA
073600011030     C                   Z-ADD     002           VR(87)                         *ISTAT X ADG/C.ISOLA
073700011031     C                   Z-ADD     002           VR(90)                         *ADG X ESPR/R.ISOLA
073800011031     C                   Z-ADD     002           VR(91)                         *ISTAT X ESPRES/R.IS
073900011031     C                   Z-ADD     002           VR(92)                         *ISTAT X ADG/ESP/RIS
074000011031     C                   Z-ADD     002           VR(93)                         *ADG X RITIRO ISOLA
074100011031     C                   Z-ADD     002           VR(94)                         *ISTAT X RITIROISOLA
074200011031     C                   Z-ADD     002           VR(95)                         *ISTAT X ADG/R.ISOLA
074300040826     C                   Z-ADD     002           VR(97)                         *ISTAT X ADG/R.ISOLA
074400040826     C                   Z-ADD     002           VR(98)                         *ISTAT X ADG/R.ISOLA
074500040826     C                   Z-ADD     002           VR(100)                        *ISTAT X ADG/R.ISOLA
074600040826     C                   Z-ADD     002           VR(101)                        *ISTAT X ADG/R.ISOLA
074700040826     C                   Z-ADD     002           VR(102)                        *ISTAT X ADG/R.ISOLA
074800040826     C                   Z-ADD     002           VR(103)                        *ISTAT X ADG/R.ISOLA
074900040826     C                   Z-ADD     002           VR(104)                        *ISTAT X ADG/R.ISOLA
075000070118     C                   Z-ADD     002           VR(106)                        *ISTAT X ADG/R.ISOLA
075100070118     C                   Z-ADD     002           VR(107)                        *ISTAT X ADG/R.ISOLA
075200070118     C                   Z-ADD     002           VR(109)                        *ISTAT X ADG/R.ISOLA
075300070118     C                   Z-ADD     002           VR(110)                        *ISTAT X ADG/R.ISOLA
075400970717     C* CONSEGNA AI PIANI
075500970717     C                   Z-ADD     003           VR(13)                         *CONS. AI PIANI
075600970717     C* CONSEGNA AI SUPERMERCATI
075700970717     C                   Z-ADD     004           VR(12)                         *CONS. SUPERMERCATI
075800970717     C* CONSEGNA SU APPUNTAMENTO
075900970717     C                   Z-ADD     005           VR(11)                         *CONS. APPUNTAMENTO
076000970717     C* CONSEGNA FUORI MISURA
076100970717     C                   Z-ADD     006           VR(14)                         *CONS. FUORI MISURA
076200970717     C* C/ASSEGNO
076300970717     C                   Z-ADD     007           VR(15)                         *C/ASSEGNO MITTENTE
076400970717     C                   Z-ADD     007           VR(16)                         *C/ASSEGNO BARTOLINI
076500970717     C* GIACENZA
076600970306     C                   Z-ADD     008           VR(8)                          *GIACENZA
076700970717     C                   Z-ADD     008           VR(18)                         *GIACENZA
076800970717     C                   Z-ADD     008           VR(19)                         *GIACENZA
076900970717     C                   Z-ADD     008           VR(20)                         *GIACENZA
077000970717     C* ASSICURAZIONE
077100970717     C                   Z-ADD     009           VR(9)                          *ASS.CONTO FRANCO
077200970717     C                   Z-ADD     009           VR(10)                         *ASS.RCV   FRANCO
077300970717     C                   Z-ADD     009           VR(31)                         *ASS.CONTO ASSEGNATO
077400970717     C                   Z-ADD     009           VR(32)                         *ASS.RCV ASSEGNATO
077500060314     C                   Z-ADD     009           VR(105)                        *ASS.RCV ASSEGNATO
077600060314     C                   Z-ADD     009           VR(108)                        *ASS.RCV ASSEGNATO
077700970717     C* ESPRESSO
077800970717     C                   Z-ADD     010           VR(7)                          *ESPRESSO PORTO
077900970717     C                   Z-ADD     010           VR(36)                         *ESPRESSO INOLTRO
078000970721     C                   Z-ADD     010           VR(39)                         *ESPRESSO L.DISAGIAT
078100011030     C                   Z-ADD     010           VR(81)                         *ESPRESSO CONS.ISOLA
078200011031     C                   Z-ADD     010           VR(89)                         *ESPRESSO RIT. ISOLA
078300970717     C* CONSEGNA DDT
078400970414     C                   Z-ADD     011           VR(33)                         *CONS. DDT
078500970717     C* CONSEGNA LOCALITA' DISAGIATA
078600970414     C                   Z-ADD     013           VR(35)                         *CONS. LOC.DISAGIATE
078700011030     C* CONSEGNA ISOLA
078800011030     C                   Z-ADD     016           VR(80)                         *CONS. ISOLA
078900050808     C* RITIRO ISOLA
079000050808     C                   Z-ADD     016           VR(88)                         *RITIRO ISOLA
079100050808     C* destinatario disagiato
079200050808     C                   Z-ADD     018           VR(96)                         *CONS. LOC.DISAGIATE
079300050808     C* bancali da rendere al cliente
079400050808     C                   Z-ADD     019           VR(99)                         *CONS. LOC.DISAGIATE
079500970306     C*
079600970306     C                   ENDSR
079700970306     C*------------------------------------------------------------------------*
079800970306     C* CHKVOC - DETERMINA LE VOCI DA CALCOLARE
079900970306     C*------------------------------------------------------------------------*
080000970306     C     CHKVOC        BEGSR
080100970306     C*
080200970306     C* IMPOSTA A NO I FLAG "DA CALCOLARE" DI TUTTE LE VOCI
080300970306     C                   MOVEL     'N'           VC                             *VOCE DA CALCOLARE
080400970306     C                   Z-ADD     100           VP                             *% DA CONSIDERARE
080500970318     C                   MOVEL     'N'           VI                             *INGLOBA S/N
080600970306     C*
080700970306     C* IMPOSTA A SI I FLAG "DA CALCOLARE" DELLE VOCI SEMPRE PRESENTI
080800970306     C                   MOVEL     'S'           VC(1)                          *PORTO
080900970306     C                   MOVEL     'S'           VC(2)                          *DIRITTO FISSO
081000970306     C                   MOVEL     'S'           VC(3)                          *A.D.G X PORTO
081100970306     C                   MOVEL     'S'           VC(22)                         *A.D.G X DIR.FISSO
081200970306     C                   MOVEL     'S'           VC(4)                          *ISTAT X PORTO
081300970306     C                   MOVEL     'S'           VC(25)                         *ISTAT X DIR.FISSO
081400970306     C                   MOVEL     'S'           VC(27)                         *ISTAT X ADG/PORTO
081500970306     C                   MOVEL     'S'           VC(28)                         *ISTAT X ADG/D.FISSO
081600970306     C                   MOVEL     'S'           VC(5)                          *T.C.P.
081700001222     C                   MOVEL     'S'           VC(46)                         *ADG SU TCP
081800001222     C                   MOVEL     'S'           VC(63)                         *ISTAT SU ADG/TCP
081900970306     C*
082000970306     C* CONTROLLA SE ESISTE =======> INOLTRO
082100970306IF  1C     ETCPPV        IFGT      *ZEROS                                       *% SPED.IN PROVINCIA
082200970306     C                   MOVEL     'S'           VC(6)                          *INOLTRO
082300970306     C                   Z-ADD     ETCPPV        VP(6)                          *% DA CONSIDERARE
082400970314     C                   MOVEL     ETCSIN        VI(6)                          *INGLOBA S/N
082500970306     C                   MOVEL     'S'           VC(21)                         *A.D.G X INOLTRO
082600970306     C                   Z-ADD     ETCPPV        VP(21)                         *% DA CONSIDERARE
082700970306     C                   MOVEL     'S'           VC(24)                         *ISTAT X INOLTRO
082800970306     C                   Z-ADD     ETCPPV        VP(24)                         *% DA CONSIDERARE
082900971023     C                   MOVEL     'S'           VC(29)                         *ISTAT X ADG/INOLTRO
083000971023     C                   Z-ADD     ETCPPV        VP(29)                         *% DA CONSIDERARE
083100970306E   1C                   ENDIF
083200970306     C*
083300970717     C* CONTROLLA SE ESISTE =======> ESPRESSO PORTO
083400970306IF  1C     ETCESP        IFGT      *ZEROS
083500970717     C                   MOVEL     'S'           VC(7)                          *ESPRESSO PORTO
083600970306     C                   Z-ADD     ETCESP        VP(7)                          *% DA CONSIDERARE
083700970306     C                   MOVEL     ETCIES        VI(7)                          *INGLOBA S/N
083800970717     C                   MOVEL     'S'           VC(23)                         *ADG X ESP/PORTO
083900970306     C                   Z-ADD     ETCESP        VP(23)                         *% DA CONSIDERARE
084000970717     C                   MOVEL     'S'           VC(26)                         *ISTAT X ESP/PORTO
084100970306     C                   Z-ADD     ETCESP        VP(26)                         *% DA CONSIDERARE
084200970717     C                   MOVEL     'S'           VC(30)                         *ISTAT X ADG/ESP/POR
084300970717     C                   Z-ADD     ETCESP        VP(30)                         *% DA CONSIDERARE
084400970306E   1C                   ENDIF
084500970717     C*
084600970717     C* CONTROLLA SE ESISTE =======> ESPRESSO INOLTRO
084700970717IF  1C     ETCESP        IFGT      *ZEROS
084800970717IF  2C     ETCPPV        IFGT      *ZEROS
084900980928     C     ETCPPV        MULT      ETCESP        WN95             10 5
085000970717     C     WN95          DIV       100           WN95
085100970717IF  3C     WN95          IFGT      *ZEROS
085200970717     C                   MOVEL     'S'           VC(36)                         *ESPRESSO INOLTRO
085300970717     C                   Z-ADD     WN95          VP(36)                         *% DA CONSIDERARE
085400970717     C                   MOVEL     ETCIES        VI(36)                         *INGLOBA S/N
085500970717     C                   MOVEL     'S'           VC(37)                         *ADG X ESP/INOLTRO
085600970717     C                   Z-ADD     WN95          VP(37)                         *% DA CONSIDERARE
085700970717     C                   MOVEL     'S'           VC(38)                         *ISTAT X ESP/INOLTRO
085800970717     C                   Z-ADD     WN95          VP(38)                         *% DA CONSIDERARE
085900970721     C                   MOVEL     'S'           VC(34)                         *ISTAT X ADG/ESP/INO
086000970721     C                   Z-ADD     WN95          VP(34)                         *% DA CONSIDERARE
086100970717E   3C                   ENDIF
086200970717E   2C                   ENDIF
086300970717E   1C                   ENDIF
086400970721     C*
086500970721     C* CONTROLLA SE ESISTE =======> ESPRESSO LOCALITA' DISAGIATE
086600970721IF  1C     ETCESP        IFGT      *ZEROS
086700970721IF  2C     ETCPCL        IFGT      *ZEROS
086800980928     C     ETCPCL        MULT      ETCESP        WN95
086900970721     C     WN95          DIV       100           WN95
087000970721IF  3C     WN95          IFGT      *ZEROS
087100970721     C                   MOVEL     'S'           VC(39)                         *ESPRESSO L.DISAG.
087200970721     C                   Z-ADD     WN95          VP(39)                         *% DA CONSIDERARE
087300970721     C                   MOVEL     ETCIES        VI(39)                         *INGLOBA S/N
087400970721     C                   MOVEL     'S'           VC(40)                         *ADG X ESP/L.DISAG.
087500970721     C                   Z-ADD     WN95          VP(40)                         *% DA CONSIDERARE
087600970721     C                   MOVEL     'S'           VC(41)                         *ISTAT X ESP/L.DISAG
087700970721     C                   Z-ADD     WN95          VP(41)                         *% DA CONSIDERARE
087800970721     C                   MOVEL     'S'           VC(42)                         *ISTAT X ADG/ESP/LDI
087900970721     C                   Z-ADD     WN95          VP(42)                         *% DA CONSIDERARE
088000970721E   3C                   ENDIF
088100970721E   2C                   ENDIF
088200970721E   1C                   ENDIF
088300970306     C*
088400970306     C* CONTROLLA SE ESISTE =======> GIACENZA VARIE
088500970306IF  1C     ETCPGI        IFGT      *ZEROS
088600970306     C                   MOVEL     'S'           VC(8)                          *GIACENZA -FISSE-
088700970306     C                   Z-ADD     ETCPGI        VP(8)                          *% DA CONSIDERARE
088800970306     C                   MOVEL     ETCIGI        VI(8)                          *INGLOBA S/N
088900001222     C                   MOVEL     'S'           VC(47)                         *ADG SU GIACENZA VAR
089000001222     C                   Z-ADD     ETCPGI        VP(47)                         *% DA CONSIDERARE
089100001227     C                   MOVEL     'S'           VC(64)                         *ISTAT SU ADG/GIAC.V
089200001227     C                   Z-ADD     ETCPGI        VP(64)                         *% DA CONSIDERARE
089300970306     C                   MOVEL     'S'           VC(20)                         *GIACENZA -VARIABILI
089400970306     C                   Z-ADD     ETCPGI        VP(20)                         *% DA CONSIDERARE
089500970306     C                   MOVEL     ETCIGI        VI(20)                         *INGLOBA S/N
089600001227     C                   MOVEL     'S'           VC(59)                         *ADG SU GIACENZA SOS
089700001227     C                   Z-ADD     ETCPGI        VP(59)                         *% DA CONSIDERARE
089800001227     C                   MOVEL     'S'           VC(76)                         *ISTAT SU ADG/GIACEN
089900001227     C                   Z-ADD     ETCPGI        VP(76)                         *% DA CONSIDERARE
090000970306E   1C                   ENDIF
090100970306     C*
090200060314     C* CONTROLLA SE ESISTE =======> ASSICURAZIONE AC PLUS
090300970306IF  1C     ETCPAC        IFGT      *ZEROS
090400970306IF  2C     ETCFRA        IFGT      *ZEROS
090500980928     C     ETCFRA        MULT      ETCPAC        WN95
090600970306     C     WN95          DIV       100           WN95
090700970306IF  3C     WN95          IFGT      *ZEROS
090800970306     C                   MOVEL     'S'           VC(9)                          *ASS.CONTO FRANCO
090900970306     C                   Z-ADD     WN95          VP(9)                          *% DA CONSIDERARE
091000970306     C                   MOVEL     ETCIAS        VI(9)                          *INGLOBA S/N
091100001227     C                   MOVEL     'S'           VC(48)                         *ADG SU ASSIC X CONT
091200001227     C                   Z-ADD     WN95          VP(48)                         *% DA CONSIDERARE
091300001227     C                   MOVEL     'S'           VC(65)                         *ISTAT SU ADG/ASSXCO
091400001227     C                   Z-ADD     WN95          VP(65)                         *% DA CONSIDERARE
091500970306E   3C                   ENDIF
091600970306E   2C                   ENDIF
091700970306E   1C                   ENDIF
091800970306     C*
091900060314     C* CONTROLLA SE ESISTE =======> ASSICURAZIONE AC PLUS
092000970306IF  1C     ETCPAC        IFGT      *ZEROS
092100970306IF  2C     ETCASS        IFGT      *ZEROS
092200980928     C     ETCASS        MULT      ETCPAC        WN95
092300970306     C     WN95          DIV       100           WN95
092400970306IF  3C     WN95          IFGT      *ZEROS
092500970306     C                   MOVEL     'S'           VC(31)                         *ASS.CONTO ASSEGNATO
092600970306     C                   Z-ADD     WN95          VP(31)                         *% DA CONSIDERARE
092700970306     C                   MOVEL     ETCIAS        VI(31)                         *INGLOBA S/N
092800001227     C                   MOVEL     'S'           VC(60)                         *ADG SU ASSIC X CONT
092900001227     C                   Z-ADD     WN95          VP(60)                         *% DA CONSIDERARE
093000001227     C                   MOVEL     'S'           VC(77)                         *ISTAT SU ADG/ASSIC
093100001227     C                   Z-ADD     WN95          VP(77)                         *% DA CONSIDERARE
093200970306E   3C                   ENDIF
093300970306E   2C                   ENDIF
093400970306E   1C                   ENDIF
093500060314     C* CONTROLLA SE ESISTE =======> ASSICURAZIONE AC BASE
093600060314IF  1C     ETCxx1        IFGT      *ZEROS
093700060314IF  2C     ETCFRA        IFGT      *ZEROS
093800060314     C     ETCFRA        MULT      ETCxx1        WN95
093900060314     C     WN95          DIV       100           WN95
094000060314IF  3C     WN95          IFGT      *ZEROS
094100060314     C                   MOVEL     'S'           VC(105)                        *ASS.CONTO FRANCO
094200060314     C                   Z-ADD     WN95          VP(105)                        *% DA CONSIDERARE
094300060314     C                   MOVEL     'N'           VI(105)                        *INGLOBA S/N
094400060314     C                   MOVEL     'S'           VC(106)                        *ADG SU ASSIC X CONT
094500060314     C                   Z-ADD     WN95          VP(106)                        *% DA CONSIDERARE
094600060314     C                   MOVEL     'S'           VC(107)                        *ISTAT SU ADG/ASSXCO
094700060314     C                   Z-ADD     WN95          VP(107)                        *% DA CONSIDERARE
094800060314E   3C                   ENDIF
094900060314E   2C                   ENDIF
095000060314E   1C                   ENDIF
095100060314     C*
095200060314     C* CONTROLLA SE ESISTE =======> ASSICURAZIONE AC BASE
095300060314IF  1C     ETCxx1        IFGT      *ZEROS
095400060314IF  2C     ETCASS        IFGT      *ZEROS
095500060314     C     ETCASS        MULT      ETCxx1        WN95
095600060314     C     WN95          DIV       100           WN95
095700060314IF  3C     WN95          IFGT      *ZEROS
095800060314     C                   MOVEL     'S'           VC(108)                        *ASS.CONTO ASSEGNATO
095900060314     C                   Z-ADD     WN95          VP(108)                        *% DA CONSIDERARE
096000060314     C                   MOVEL     'N'           VI(108)                        *INGLOBA S/N
096100060314     C                   MOVEL     'S'           VC(109)                        *ADG SU ASSIC X CONT
096200060314     C                   Z-ADD     WN95          VP(109)                        *% DA CONSIDERARE
096300060314     C                   MOVEL     'S'           VC(110)                        *ISTAT SU ADG/ASSIC
096400060314     C                   Z-ADD     WN95          VP(110)                        *% DA CONSIDERARE
096500060314E   3C                   ENDIF
096600060314E   2C                   ENDIF
096700060314E   1C                   ENDIF
096800970306     C*
096900970306     C* CONTROLLA SE ESISTE =======> ASSICURAZIONE RCV FRANCO
097000060314     c* tariffa particolare "R" ormai obsoleta
097100990621IF  1C     ETCPAR        IFGT      *ZEROS
097200990621IF  2C     ETCFRA        IFGT      *ZEROS
097300990621     C     ETCFRA        MULT      ETCPAR        WN95
097400990621     C     WN95          DIV       100           WN95
097500990621IF  3C     WN95          IFGT      *ZEROS
097600990621     C                   MOVEL     'S'           VC(10)                         *ASS.NE RCV FRANCO
097700990621     C                   Z-ADD     WN95          VP(10)                         *% DA CONSIDERARE
097800990621     C                   MOVEL     ETCIAS        VI(10)                         *INGLOBA S/N
097900001227     C                   MOVEL     'S'           VC(49)                         *ADG SU ASSIC RCV  F
098000001227     C                   Z-ADD     WN95          VP(49)                         *% DA CONSIDERARE
098100001227     C                   MOVEL     'S'           VC(66)                         *ISTAT SU ADG/ASS RC
098200001227     C                   Z-ADD     WN95          VP(66)                         *% DA CONSIDERARE
098300990621E   3C                   ENDIF
098400990621E   2C                   ENDIF
098500990621E   1C                   ENDIF
098600970306     C*
098700970306     C* CONTROLLA SE ESISTE =======> ASSICURAZIONE RCV ASSEGNATO
098800060314     c* tariffa particolare "R" ormai obsoleta
098900990621IF  1C     ETCPAR        IFGT      *ZEROS
099000990621IF  2C     ETCASS        IFGT      *ZEROS
099100990621     C     ETCASS        MULT      ETCPAR        WN95
099200990621     C     WN95          DIV       100           WN95
099300990621IF  3C     WN95          IFGT      *ZEROS
099400990621     C                   MOVEL     'S'           VC(32)                         *ASS.NE RCV ASSEGNAT
099500990621     C                   Z-ADD     WN95          VP(32)                         *% DA CONSIDERARE
099600990621     C                   MOVEL     ETCIAS        VI(32)                         *INGLOBA S/N
099700001227     C                   MOVEL     'S'           VC(61)                         *ADG SU ASSIC RCV  A
099800001227     C                   Z-ADD     WN95          VP(61)                         *% DA CONSIDERARE
099900001227     C                   MOVEL     'S'           VC(78)                         *ISTAT SU ADG/ASS RC
100000001227     C                   Z-ADD     WN95          VP(78)                         *% DA CONSIDERARE
100100990621E   3C                   ENDIF
100200990621E   2C                   ENDIF
100300990621E   1C                   ENDIF
100400970306     C*
100500970306     C* CONTROLLA SE ESISTE =======> CONSEGNA SU APPUNTAMENTO
100600970306IF  1C     ETCPCA        IFGT      *ZEROS
100700970306     C                   MOVEL     'S'           VC(11)                         *CONS. APPUNTAMENTO
100800970306     C                   Z-ADD     ETCPCA        VP(11)                         *% DA CONSIDERARE
100900970306     C                   MOVEL     ETCIAP        VI(11)                         *INGLOBA S/N
101000001227     C                   MOVEL     'S'           VC(50)                         *ADG SU CONSEGNA APP
101100001227     C                   Z-ADD     ETCPCA        VP(50)                         *% DA CONSIDERARE
101200001227     C                   MOVEL     'S'           VC(67)                         *ISTAT SU ADG/CONSEG
101300001227     C                   Z-ADD     ETCPCA        VP(67)                         *% DA CONSIDERARE
101400970306E   1C                   ENDIF
101500970306     C*
101600970306     C* CONTROLLA SE ESISTE =======> CONSEGNA AI SUPERMERCATI
101700970306IF  1C     ETCPCS        IFGT      *ZEROS
101800970306     C                   MOVEL     'S'           VC(12)                         *CONS. SUPERMERCATI
101900970306     C                   Z-ADD     ETCPCS        VP(12)                         *% DA CONSIDERARE
102000970306     C                   MOVEL     ETCISU        VI(12)                         *INGLOBA S/N
102100001227     C                   MOVEL     'S'           VC(51)                         *ADG SU CONSEGNA SUP
102200001227     C                   Z-ADD     ETCPCS        VP(51)                         *% DA CONSIDERARE
102300001227     C                   MOVEL     'S'           VC(68)                         *ISTAT SU ADG/CONSEG
102400001227     C                   Z-ADD     ETCPCS        VP(68)                         *% DA CONSIDERARE
102500970306E   1C                   ENDIF
102600970306     C*
102700970306     C* CONTROLLA SE ESISTE =======> CONSEGNA AI PIANI
102800970306IF  1C     ETCPCP        IFGT      *ZEROS
102900970306     C                   MOVEL     'S'           VC(13)                         *CONS. AI PIANI
103000970306     C                   Z-ADD     ETCPCP        VP(13)                         *% DA CONSIDERARE
103100970306     C                   MOVEL     ETCIPI        VI(13)                         *INGLOBA S/N
103200001227     C                   MOVEL     'S'           VC(52)                         *ADG SU CONSEGNA SUP
103300001227     C                   Z-ADD     ETCPCP        VP(52)                         *% DA CONSIDERARE
103400001227     C                   MOVEL     'S'           VC(69)                         *ISTAT SU ADG/CONSEG
103500001227     C                   Z-ADD     ETCPCP        VP(69)                         *% DA CONSIDERARE
103600970306E   1C                   ENDIF
103700040826     C* CONTROLLA SE ESISTE =======> CONSEGNA destinatario disagiato
103800040826IF  1C     ETCdes        IFGT      *ZEROS
103900040826     C                   MOVEL     'S'           VC(96)                         *CONS. AI PIANI
104000040826     C                   Z-ADD     ETCdes        VP(96)                         *% DA CONSIDERARE
104100040826     C                   MOVEL     'S'           VC(97)                         *ADG SU CONSEGNA SUP
104200040826     C                   Z-ADD     ETCdes        VP(97)                         *% DA CONSIDERARE
104300040826     C                   MOVEL     'S'           VC(98)                         *ISTAT SU ADG/CONSEG
104400040826     C                   Z-ADD     ETCdes        VP(98)                         *% DA CONSIDERARE
104500040826E   1C                   ENDIF
104600970306     C*
104700040826     C* CONTROLLA SE ESISTE =======> bancali BAN
104800040826      * Non si tassano i bancali per le bolle in porto assegnato(vedi TNSF20R)
104900040826IF  1C     ETCnba        IFGT      *ZEROS
105000040826IF  1C     ETCfra        andeq     100
105100040826     C                   MOVEL     'S'           VC(99)                         *CONS. AI PIANI
105200040826     C                   Z-ADD     ETCban        VP(99)                         *% DA CONSIDERARE
105300040826     C                   MOVEL     'S'           VC(100)                        *ADG SU CONSEGNA SUP
105400040826     C                   Z-ADD     ETCban        VP(100)                        *% DA CONSIDERARE
105500040826     C                   MOVEL     'S'           VC(101)                        *ISTAT SU ADG/CONSEG
105600040826     C                   Z-ADD     ETCban        VP(101)                        *% DA CONSIDERARE
105700040826E   1C                   ENDIF
105800040826     C*
105900970306     C* CONTROLLA SE ESISTE =======> CONSEGNA FUORI MISURA
106000970306IF  1C     ETCPCF        IFGT      *ZEROS
106100970306     C                   MOVEL     'S'           VC(14)                         *CONS. FUORI MISURA
106200970306     C                   Z-ADD     ETCPCF        VP(14)                         *% DA CONSIDERARE
106300970306     C                   MOVEL     ETCIFU        VI(14)                         *INGLOBA S/N
106400001227     C                   MOVEL     'S'           VC(53)                         *ADG SU CONSEGNA FUO
106500001227     C                   Z-ADD     ETCPCF        VP(53)                         *% DA CONSIDERARE
106600001227     C                   MOVEL     'S'           VC(70)                         *ISTAT SU ADG/CONSEG
106700001227     C                   Z-ADD     ETCPCF        VP(70)                         *% DA CONSIDERARE
106800970306E   1C                   ENDIF
106900970414     C*
107000970414     C* CONTROLLA SE ESISTE =======> CONSEGNA DDT
107100970414IF  1C     ETCPCD        IFGT      *ZEROS
107200970414     C                   MOVEL     'S'           VC(33)                         *CONS. DDT
107300970414     C                   Z-ADD     ETCPCD        VP(33)                         *% DA CONSIDERARE
107400970414     C                   MOVEL     ETCIDD        VI(33)                         *INGLOBA S/N
107500001227     C                   MOVEL     'S'           VC(62)                         *ADG SU CONSEGNA DDT
107600001227     C                   Z-ADD     ETCPCD        VP(62)                         *% DA CONSIDERARE
107700001227     C                   MOVEL     'S'           VC(79)                         *ISTAT SU ADG/CONSEG
107800001227     C                   Z-ADD     ETCPCD        VP(79)                         *% DA CONSIDERARE
107900970414E   1C                   ENDIF
108000970414     C*
108100970414     C* CONTROLLA SE ESISTE =======> CONSEGNA LOCALITA' DISAGIATE
108200970414IF  1C     ETCPCL        IFGT      *ZEROS
108300970414     C                   MOVEL     'S'           VC(35)                         *CONS. LOC.DISAGIATE
108400970414     C                   Z-ADD     ETCPCL        VP(35)                         *% DA CONSIDERARE
108500970414     C                   MOVEL     ETCILO        VI(35)                         *INGLOBA S/N
108600971023     C                   MOVEL     'S'           VC(43)                         *A.D.G X L.DISAG.
108700971023     C                   Z-ADD     ETCPCL        VP(43)                         *% DA CONSIDERARE
108800971023     C                   MOVEL     'S'           VC(44)                         *ISTAT X L.DISAG.
108900971023     C                   Z-ADD     ETCPCL        VP(44)                         *% DA CONSIDERARE
109000971023     C                   MOVEL     'S'           VC(45)                         *ISTAT X ADG/L.DISAG
109100971023     C                   Z-ADD     ETCPCL        VP(45)                         *% DA CONSIDERARE
109200970414E   1C                   ENDIF
109300970306     C*
109400970306     C* CONTROLLA SE ESISTE =======> CONTRASSEGNO MITTENTE
109500970306IF  1C     ETCPAM        IFGT      *ZEROS
109600970306     C                   MOVEL     'S'           VC(15)                         *C/ASSEGNO MITTENTE
109700970306     C                   Z-ADD     ETCPAM        VP(15)                         *% DA CONSIDERARE
109800970306     C                   MOVEL     ETCICA        VI(15)                         *INGLOBA S/N
109900001227     C                   MOVEL     'S'           VC(54)                         *ADG SU C/ASSEGNO MI
110000001227     C                   Z-ADD     ETCPAM        VP(54)                         *% DA CONSIDERARE
110100001227     C                   MOVEL     'S'           VC(71)                         *ISTAT SU ADG/CASSEG
110200001227     C                   Z-ADD     ETCPAM        VP(71)                         *% DA CONSIDERARE
110300970306E   1C                   ENDIF
110400970306     C*
110500970306     C* CONTROLLA SE ESISTE =======> CONTRASSEGNO BARTOLINI
110600970306IF  1C     ETCPAB        IFGT      *ZEROS
110700970306     C                   MOVEL     'S'           VC(16)                         *C/ASSEGNO BARTOLINI
110800970306     C                   Z-ADD     ETCPAB        VP(16)                         *% DA CONSIDERARE
110900970306     C                   MOVEL     ETCICA        VI(16)                         *INGLOBA S/N
111000001227     C                   MOVEL     'S'           VC(55)                         *ADG SU C/ASSEGNO BA
111100001227     C                   Z-ADD     ETCPAB        VP(55)                         *% DA CONSIDERARE
111200001227     C                   MOVEL     'S'           VC(72)                         *ISTAT SU ADG/CASSEG
111300001227     C                   Z-ADD     ETCPAB        VP(72)                         *% DA CONSIDERARE
111400970306E   1C                   ENDIF
111500970306     C*
111600970306     C* CONTROLLA SE ESISTE =======> RITIRO
111700970306IF  1C     T             IFGT      *ZEROS                                       *CICLO COD.TASSAZION
111800970306IF  2C     ETCASS        IFGT      *ZEROS
111900971204IF  3C     CFL1(T)       IFEQ      *BLANKS                                      *ARRIVO ITALIA
112000970306IF  4C     ETCPCM        IFNE      100
112100970306     C     100           SUB       ETCPCM        WN63              6 3
112200980928     C     WN63          MULT      ETCASS        WN95
112300970306     C     WN95          DIV       100           WN95
112400970306IF  5C     WN95          IFGT      *ZEROS
112500970306     C                   MOVEL     'S'           VC(17)                         *RITIRO
112600970306     C                   Z-ADD     WN95          VP(17)                         *% DA CONSIDERARE
112700001227     C                   MOVEL     'S'           VC(56)                         *ADG SU RITIRO
112800001227     C                   Z-ADD     WN95          VP(56)                         *% DA CONSIDERARE
112900001227     C                   MOVEL     'S'           VC(73)                         *ISTAT SU ADG/RITIRO
113000001227     C                   Z-ADD     WN95          VP(73)                         *% DA CONSIDERARE
113100970306E   5C                   ENDIF
113200970311E   4C                   ENDIF
113300970311X   3C                   ELSE                                                   *ARRIVO ESTERO
113400970306     C                   MOVEL     'S'           VC(17)                         *RITIRO
113500001227     C                   MOVEL     'S'           VC(56)                         *ADG SU RITIRO
113600001227     C                   MOVEL     'S'           VC(73)                         *ISTAT SU ADG/RITIRO
113700970306E   3C                   ENDIF
113800970306E   2C                   ENDIF
113900970306E   1C                   ENDIF
114000970306     C*
114100970306     C* CONTROLLA SE ESISTE =======> GIACENZA RICONSEGNA PROVINCIA (SOLO 50%)
114200970306IF  1C     ETCPGI        IFGT      *ZEROS
114300970306IF  2C     ETCPPV        IFGT      *ZEROS
114400980928     C     ETCPPV        MULT      ETCPGI        WN95
114500970306     C     WN95          DIV       100           WN95
114600970306     C     WN95          MULT      50            WN95
114700970306     C     WN95          DIV       100           WN95
114800970306IF  3C     WN95          IFGT      *ZEROS
114900970306     C                   MOVEL     'S'           VC(18)                         *GIACENZA
115000970306     C                   Z-ADD     WN95          VP(18)                         *% DA CONSIDERARE
115100970306     C                   MOVEL     ETCIGI        VI(18)                         *INGLOBA S/N
115200001227     C                   MOVEL     'S'           VC(57)                         *ADG SU GIACENZA CON
115300001227     C                   Z-ADD     WN95          VP(57)                         *% DA CONSIDERARE
115400001227     C                   MOVEL     'S'           VC(74)                         *ISTAT SU ADG/GIACEN
115500001227     C                   Z-ADD     WN95          VP(74)                         *% DA CONSIDERARE
115600970306E   3C                   ENDIF
115700970306E   2C                   ENDIF
115800970306E   1C                   ENDIF
115900970306     C*
116000970306     C* CONTROLLA SE ESISTE =======> GIACENZA RICONSEGNA CITTA' (SOLO 50%)
116100970306IF  1C     ETCPGI        IFGT      *ZEROS
116200970306IF  2C     ETCPPV        IFGT      *ZEROS
116300970306     C     100           SUB       ETCPPV        WN63              6 3
116400980928     C     WN63          MULT      ETCPGI        WN95
116500970306     C     WN95          DIV       100           WN95
116600970306     C     WN95          MULT      50            WN95
116700970306     C     WN95          DIV       100           WN95
116800970306IF  3C     WN95          IFGT      *ZEROS
116900970306     C                   MOVEL     'S'           VC(19)                         *GIACENZA
117000970306     C                   Z-ADD     WN95          VP(19)                         *% DA CONSIDERARE
117100970306     C                   MOVEL     ETCIGI        VI(19)                         *INGLOBA S/N
117200001227     C                   MOVEL     'S'           VC(58)                         *ADG SU GIACENZA CON
117300001227     C                   Z-ADD     WN95          VP(58)                         *% DA CONSIDERARE
117400001227     C                   MOVEL     'S'           VC(75)                         *ISTAT SU ADG/GIACEN
117500001227     C                   Z-ADD     WN95          VP(75)                         *% DA CONSIDERARE
117600970306E   3C                   ENDIF
117700970306E   2C                   ENDIF
117800970306E   1C                   ENDIF
117900011030     C*
118000011031     C* CONTROLLA SE ESISTE =======>
118100011031IF  1C     ETCCIS        IFGT      *ZEROS
118200011031IF  2C     T             IFGT      *ZEROS                                       *CICLO COD.TASSAZION
118300011031IF  3C     C§§5(T)       IFEQ      'S'                                          *CTS POTENZ.ISOLA
118400011031     C*                     =======> CONSEGNA ISOLA
118500011030     C                   MOVEL     'S'           VC(80)                         *CONSEGNA ISOLA
118600011030     C                   Z-ADD     ETCCIS        VP(80)                         *% DA CONSIDERARE
118700011030     C                   MOVEL     ETCISO        VI(80)                         *INGLOBA S/N
118800011030     C                   MOVEL     'S'           VC(85)                         *A.D.G X CONS.ISOLA
118900011030     C                   Z-ADD     ETCCIS        VP(85)                         *% DA CONSIDERARE
119000011030     C                   MOVEL     'S'           VC(86)                         *ISTAT X CONS.ISOLA
119100011030     C                   Z-ADD     ETCCIS        VP(86)                         *% DA CONSIDERARE
119200011030     C                   MOVEL     'S'           VC(87)                         *ISTAT X ADG/C.ISOLA
119300011030     C                   Z-ADD     ETCCIS        VP(87)                         *% DA CONSIDERARE
119400011031     C*                     =======> ESPRESSO CONSEGNA ISOLE
119500011031IF  4C     ETCESP        IFGT      *ZEROS
119600011031     C     ETCCIS        MULT      ETCESP        WN95
119700011031     C     WN95          DIV       100           WN95
119800011031IF  5C     WN95          IFGT      *ZEROS
119900011031     C                   MOVEL     'S'           VC(81)                         *ESPRESSO CONS.ISOLE
120000011031     C                   Z-ADD     WN95          VP(81)                         *% DA CONSIDERARE
120100011031     C                   MOVEL     ETCISO        VI(81)                         *INGLOBA S/N
120200011031     C                   MOVEL     'S'           VC(82)                         *ADG X ESP/C.ISOLE
120300011031     C                   Z-ADD     WN95          VP(82)                         *% DA CONSIDERARE
120400011031     C                   MOVEL     'S'           VC(83)                         *ISTAT X ESP/C.ISOLE
120500011031     C                   Z-ADD     WN95          VP(83)                         *% DA CONSIDERARE
120600011031     C                   MOVEL     'S'           VC(84)                         *ISTAT X ADG/ESP/CIS
120700011031     C                   Z-ADD     WN95          VP(84)                         *% DA CONSIDERARE
120800011031E   5C                   ENDIF
120900011031E   4C                   ENDIF
121000011031     C*
121100011030E   3C                   ENDIF
121200011030E   2C                   ENDIF
121300011030E   1C                   ENDIF
121400011030     C*
121500011031     C* CONTROLLA SE ESISTE =======>
121600011031IF  1C     ETCSAM        IFGT      *ZEROS
121700011031IF  2C     T             IFGT      *ZEROS                                       *CICLO COD.TASSAZION
121800011031IF  3C     D00PRI        IFEQ      CCTS(T)                                      *CTS DEL CLIENTE T/O
121900011031IF  4C     C§§5(T)       IFEQ      'S'                                          *CTS POTENZ.ISOLA
122000011031     C*                     =======> RITIRO ISOLA
122100011031     C                   MOVEL     'S'           VC(88)                         *RITIRO ISOLA
122200011031     C                   Z-ADD     ETCSAM        VP(88)                         *% DA CONSIDERARE
122300011031     C                   MOVEL     ETCINR        VI(88)                         *INGLOBA S/N
122400011031     C                   MOVEL     'S'           VC(93)                         *A.D.G X RIT.ISOLA
122500011031     C                   Z-ADD     ETCSAM        VP(93)                         *% DA CONSIDERARE
122600011031     C                   MOVEL     'S'           VC(94)                         *ISTAT X RIT.ISOLA
122700011031     C                   Z-ADD     ETCSAM        VP(94)                         *% DA CONSIDERARE
122800011031     C                   MOVEL     'S'           VC(95)                         *ISTAT X ADG/R.ISOLA
122900011031     C                   Z-ADD     ETCSAM        VP(95)                         *% DA CONSIDERARE
123000011031     C*                     =======> ESPRESSO RITIRO ISOLE
123100011031IF  5C     ETCESP        IFGT      *ZEROS
123200011031     C     ETCSAM        MULT      ETCESP        WN95
123300011031     C     WN95          DIV       100           WN95
123400011031IF  6C     WN95          IFGT      *ZEROS
123500011031     C                   MOVEL     'S'           VC(89)                         *ESPRESSO RIT.ISOLE
123600011031     C                   Z-ADD     WN95          VP(89)                         *% DA CONSIDERARE
123700011031     C                   MOVEL     ETCINR        VI(89)                         *INGLOBA S/N
123800011031     C                   MOVEL     'S'           VC(90)                         *ADG X ESP/RIT.ISOLE
123900011031     C                   Z-ADD     WN95          VP(90)                         *% DA CONSIDERARE
124000011031     C                   MOVEL     'S'           VC(91)                         *ISTAT X ESP/R.ISOLE
124100011031     C                   Z-ADD     WN95          VP(91)                         *% DA CONSIDERARE
124200011031     C                   MOVEL     'S'           VC(92)                         *ISTAT X ADG/ESP/RIS
124300011031     C                   Z-ADD     WN95          VP(92)                         *% DA CONSIDERARE
124400011031E   6C                   ENDIF
124500011031E   5C                   ENDIF
124600011031     C*
124700011031E   4C                   ENDIF
124800011031E   3C                   ENDIF
124900011031E   2C                   ENDIF
125000011030E   1C                   ENDIF
125100970306     C*
125200970306     C                   ENDSR
125300970306     C*------------------------------------------------------------------------*
125400970306     C* VALVO1 - VALORIZZA LE VOCI -FISSE-
125500970306     C*------------------------------------------------------------------------*
125600970306     C     VALVO1        BEGSR
125700970306     C*
125800970306     C* AZZERA GLI IMPORTI DELLE VOCI CON SEQUENZA UNO
125900970306DO  1C     1             DO        NVOCA         I
126000970306IF  2C     VS(I)         IFEQ      1
126100970306     C                   Z-ADD     *ZEROS        VIMP(I)                        *IMPORTO CALCOLATO
126200970306E   2C                   ENDIF
126300970306E   1C                   ENDDO
126400970306     C*
126500970306     C* CALCOLA LE SPESE DI GIACENZA VARIE
126600970306IF  1C     WERR          IFEQ      'N'
126700970306IF  2C     VC(8)         IFEQ      'S'                                          *VOCE DA CALCOLARE
126800970306     C     $TGC          ANDEQ     '1'                                          *TROVATA TAR.GIACENZ
126900970306     C                   EXSR      CALGCV
127000001227     C                   EXSR      CAL047
127100001227     C                   EXSR      CAL064
127200970306E   2C                   ENDIF
127300970306E   1C                   ENDIF
127400970306     C*
127500970306     C                   ENDSR
127600970306     C*------------------------------------------------------------------------*
127700970306     C* CARTGC - REPERISCE LA TARIFFA GIACENZA
127800970306     C*------------------------------------------------------------------------*
127900970306     C     CARTGC        BEGSR
128000970306     C*
128100970625     C* LEGGE LA TARIFFA GIACENZA DEL CLIENTE
128200970625     C                   Z-ADD     D00KSC        KGCKSC                         *CLIENTE
128300970625     C                   Z-ADD     D00CTR        KGCCTR                         *TARIFFA
128400970625     C                   Z-ADD     D00PRG        KGCPRG                         *PROGRESSIVO
128500011019     C                   MOVEL     D00DIV        WDIV
128600091123     c                   if        %subst(d00op0: 1: 1)='O'
128700990819     C     KEYTGC        CHAIN     TIOGC01L                           99
128800980703X   1C                   ELSE                                                   *IN TARIFFA
128900990819     C     KEYTGC        CHAIN     TITGC01L                           99
129000970625E   1C                   ENDIF
129100970306     C*
129200970625     C* LEGGE LA TARIFFA DI CARTELLO SE NON TROVATA TARIFFA/OFFERTA CLIENTE
129300970625IF  1C     *IN99         IFEQ      *ON                                          *NON TROVATA
129400970625     C                   Z-ADD     CARKSC        KGCKSC
129500970625     C                   Z-ADD     CARCTR        KGCCTR
129600970625     C                   Z-ADD     CARPRG        KGCPRG
129700011023     C                   MOVEL     CARDIV        WDIV
129800011023     C     KEYTGC        CHAIN     TITGC01L                           99        *DA TARIFFA CARTELLO
129900970625E   1C                   ENDIF
130000970625     C* TARIFFA NON TROVATA
130100970625IF  1C     *IN99         IFEQ      *ON
130200970625     C                   MOVEL     MSG(23)       $MSG
130300970625     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
130400970625     C* TARIFFA TROVATA
130500970306X   1C                   ELSE
130600991125     C*
130700991125     C* CONTROLLA SE CONVERTIRE I CAMPI DI TITPD
130800011026IF  2C     WDIV          IFNE      WDIVCE
130900011026     C                   MOVEL     WDIV          WDVI              3
131000991125     C                   EXSR      CNVTGC
131100991125E   2C                   ENDIF
131200970625     C                   MOVEL     '1'           $TGC                           *TARIFFA TROVATA
131300970306E   1C                   ENDIF
131400970306     C*
131500970306     C                   ENDSR
131600970306     C*------------------------------------------------------------------------*
131700970306     C* CALGCV - CALCOLA LE SPESE DI GIACENZA VARIE
131800970306     C*------------------------------------------------------------------------*
131900970306     C     CALGCV        BEGSR
132000970306     C*
132100970306     C* REIMPOSTA LE VARIABILI DI LAVORO
132200011031     C                   Z-ADD     *ZEROS        TASSGV           21 8
132300011031     C                   Z-ADD     *ZEROS        TASSGD           21 8
132400970306     C*
132500970306     C* CALCOLA LE SPESE VARIE
132600970306IF  1C     TGCSGV        IFGT      *ZEROS                                       *IMMESSA TARIFFA
132700970306     C                   Z-ADD     TGCSGV        TASSGV
132800060125     c                   if        (tgcfaf = '2' or tgcfaf='3') and
132900060125     c                             etcggg <= tgcsgf
133000060124     C                   Z-ADD     *zeros        TASSGV
133100970306E   1C                   ENDIF
133200060124E   1C                   ENDIF
133300970306     C*
133400970306     C* CALCOLA LE SPESE DOSSIER
133500970306IF  1C     TGCSGD        IFGT      *ZEROS                                       *IMMESSA TARIFFA
133600970306     C     TGCSGD        MULT      ETCNRG        TASSGD
133700970306     C*
133800970306     C* AZZERA LE SPESE DOSSIER SE SI E' DENTRO AI GIORNI DI FRANCHIGIA
133900060125     c                   if        (tgcfaf = '2' or tgcfaf='3') and
134000060124     c                             etcggg <= tgcsgf
134100970306     C                   Z-ADD     *ZEROS        TASSGD                         *NO SPESE DOSSIER
134200060124E   2C                   ENDIF
134300970306E   1C                   ENDIF
134400970306     C*
134500970314IF  1C     TASSGV        IFGT      *ZEROS
134600970314     C     TASSGD        ORGT      *ZEROS
134700970306     C                   Z-ADD     8             I
134800970306     C     TASSGV        ADD       TASSGD        VIMP(I)
134900970314E   1C                   ENDIF
135000970306     C*
135100970306     C                   ENDSR
135200001227     C*------------------------------------------------------------------------*
135300001227     C* CAL047 - CALCOLA ADG SU GIACENZA VARIE DOSSIER
135400001227     C*------------------------------------------------------------------------*
135500001227     C     CAL047        BEGSR
135600001227     C*
135700001227     C* REIMPOSTA LE VARIABILI DI LAVORO
135800011031     C                   Z-ADD     *ZEROS        TASADG           21 8
135900001227     C*
136000001227     C* CALCOLA L'A.D.G.
136100011031     C                   Z-ADD     VIMP(8)       WIMP             23 8
136200001227     C                   EXSR      ADGCAL
136300001227     C*
136400001227IF  1C     TASADG        IFGT      *ZEROS
136500001227     C                   Z-ADD     47            I
136600001227     C                   Z-ADD     TASADG        VIMP(I)
136700001227E   1C                   ENDIF
136800001227     C*
136900001227     C                   ENDSR
137000001227     C*------------------------------------------------------------------------*
137100001227     C* CAL064 - CALCOLA ISTAT SU ADG/GIAC.VARIE DOSSIER
137200001227     C*------------------------------------------------------------------------*
137300001227     C     CAL064        BEGSR
137400001227     C*
137500001227     C* REIMPOSTA LE VARIABILI DI LAVORO
137600011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
137700001227     C*
137800001227     C* CALCOLA L'ISTAT
137900011031     C                   Z-ADD     VIMP(47)      WIMP             23 8
138000001227     C                   EXSR      IACCAL
138100001227     C*
138200001227IF  1C     TASIAC        IFGT      *ZEROS
138300001227     C                   Z-ADD     64            I
138400001227     C                   Z-ADD     TASIAC        VIMP(I)
138500001227E   1C                   ENDIF
138600001227     C*
138700001227     C                   ENDSR
138800970306     C*------------------------------------------------------------------------*
138900970306     C* VALVO2 - VALORIZZA LE VOCI -DIPENDENTI DA SCAGLIONI-
139000970306     C*------------------------------------------------------------------------*
139100970306     C     VALVO2        BEGSR
139200970306     C*
139300970306     C* AZZERA GLI IMPORTI DELLE VOCI CON SEQUENZA DUE
139400970306DO  1C     1             DO        NVOCA         I
139500970306IF  2C     VS(I)         IFEQ      2
139600970306     C                   Z-ADD     *ZEROS        VIMP(I)                        *IMPORTO CALCOLATO
139700970306E   2C                   ENDIF
139800970306E   1C                   ENDDO
139900970306     C*
140000970306     C* CALCOLA LE SPESE DI GIACENZA DI RICONSEGNA PROVINCIA
140100970306IF  1C     WERR          IFEQ      'N'
140200970306IF  2C     VC(18)        IFEQ      'S'                                          *VOCE DA CALCOLARE
140300970306     C     $TGC          ANDEQ     '1'                                          *TROVATA TAR.GIACENZ
140400970306     C                   EXSR      CALGRP
140500001227     C                   EXSR      CAL057
140600001227     C                   EXSR      CAL074
140700970306E   2C                   ENDIF
140800970306E   1C                   ENDIF
140900970306     C*
141000970306     C* CALCOLA LE SPESE DI GIACENZA DI RICONSEGNA CITTA'
141100970306IF  1C     WERR          IFEQ      'N'
141200970306IF  2C     VC(19)        IFEQ      'S'                                          *VOCE DA CALCOLARE
141300970306     C     $TGC          ANDEQ     '1'                                          *TROVATA TAR.GIACENZ
141400970306     C                   EXSR      CALGRC
141500001227     C                   EXSR      CAL058
141600001227     C                   EXSR      CAL075
141700970306E   2C                   ENDIF
141800970306E   1C                   ENDIF
141900970306     C*
142000970306     C* CALCOLA LE SPESE DI GIACENZA DI SOSTA
142100970306IF  1C     WERR          IFEQ      'N'
142200970306IF  2C     VC(20)        IFEQ      'S'                                          *VOCE DA CALCOLARE
142300970306     C     $TGC          ANDEQ     '1'                                          *TROVATA TAR.GIACENZ
142400970306     C                   EXSR      CALGSO
142500001227     C                   EXSR      CAL059
142600001227     C                   EXSR      CAL076
142700970306E   2C                   ENDIF
142800970306E   1C                   ENDIF
142900970306     C*
143000970306     C                   ENDSR
143100970306     C*------------------------------------------------------------------------*
143200970306     C* CALTCP - CALCOLA T.C.P.
143300970306     C*------------------------------------------------------------------------*
143400970306     C     CALTCP        BEGSR
143500990819     C*
143600990819     C* REIMPOSTA LE VARIABILI DI LAVORO
143700990819     C                   MOVEL     'S'           WCAR                           *SI UTILIZZO TAR.CAR
143800011031     C                   Z-ADD     *ZEROS        TASTMP           21 8
143900990819     C*
144000990819     C* CALCOLA CONSEGNA PARTICOLARE
144100990819     C                   MOVEL     '7'           TASTC             1            *CODICE CONSEGNA
144200011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
144300990819     C                   EXSR      CALCP
144400990819     C                   Z-ADD     TASCP         TASTMP
144500970306     C*
144600970314IF  1C     TASTMP        IFGT      *ZEROS
144700970306     C                   Z-ADD     5             I
144800970306     C                   Z-ADD     TASTMP        VIMP(I)
144900970314E   1C                   ENDIF
145000990819     C*
145100970306     C                   ENDSR
145200001222     C*------------------------------------------------------------------------*
145300001222     C* CAL046 - CALCOLA ADG SU TCP
145400001222     C*------------------------------------------------------------------------*
145500001222     C     CAL046        BEGSR
145600001222     C*
145700001222     C* REIMPOSTA LE VARIABILI DI LAVORO
145800011031     C                   Z-ADD     *ZEROS        TASADG           21 8
145900001222     C*
146000001222     C* CALCOLA L'A.D.G.
146100011031     C                   Z-ADD     VIMP(5)       WIMP             23 8
146200001222     C                   EXSR      ADGCAL
146300001222     C*
146400001222IF  1C     TASADG        IFGT      *ZEROS
146500001222     C                   Z-ADD     46            I
146600001222     C                   Z-ADD     TASADG        VIMP(I)
146700001222E   1C                   ENDIF
146800001222     C*
146900001222     C                   ENDSR
147000001222     C*------------------------------------------------------------------------*
147100001222     C* CAL063 - CALCOLA ISTAT SU ADG/TCP
147200001222     C*------------------------------------------------------------------------*
147300001222     C     CAL063        BEGSR
147400001222     C*
147500001222     C* REIMPOSTA LE VARIABILI DI LAVORO
147600011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
147700001222     C*
147800001222     C* CALCOLA L'ISTAT
147900011031     C                   Z-ADD     VIMP(46)      WIMP             23 8
148000001222     C                   EXSR      IACCAL
148100001222     C*
148200001222IF  1C     TASIAC        IFGT      *ZEROS
148300001222     C                   Z-ADD     63            I
148400001222     C                   Z-ADD     TASIAC        VIMP(I)
148500001222E   1C                   ENDIF
148600001222     C*
148700001222     C                   ENDSR
148800040826     C*------------------------------------------------------------------------*
148900040826     C* CALdps - CALCOLA diritto di pesatura
149000040826     C*------------------------------------------------------------------------*
149100040826     C     CALdps        BEGSR
149200040826     C*
149300040826     C* REIMPOSTA LE VARIABILI DI LAVORO
149400040826     C                   MOVEL     'S'           WCAR                           *SI UTILIZZO TAR.CAR
149500040826     C                   Z-ADD     *ZEROS        TASdps           21 8
149600040826     C*
149700040826     C* CALCOLA CONSEGNA PARTICOLARE
149800040826     C                   MOVEL     '5'           TASTC             1            *CODICE CONSEGNA
149900040826     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
150000040826     C                   EXSR      CALCP
150100040826     C                   Z-ADD     TASCP         TASdps
150200040826     C*
150300040826IF  1C     TASdps        IFGT      *ZEROS
150400040826     C                   Z-ADD     102           I
150500040826     C                   Z-ADD     TASdps        VIMP(I)
150600040826E   1C                   ENDIF
150700040826     C*
150800040826     C                   ENDSR
150900040826     C*------------------------------------------------------------------------*
151000040826     C* CAL103 - CALCOLA ADG SU TCP
151100040826     C*------------------------------------------------------------------------*
151200040826     C     CAL103        BEGSR
151300040826     C*
151400040826     C* REIMPOSTA LE VARIABILI DI LAVORO
151500040826     C                   Z-ADD     *ZEROS        TASADG           21 8
151600040826     C*
151700040826     C* CALCOLA L'A.D.G.
151800040826     C                   Z-ADD     VIMP(102)     WIMP             23 8
151900040826     C                   EXSR      ADGCAL
152000040826     C*
152100040826IF  1C     TASADG        IFGT      *ZEROS
152200040826     C                   Z-ADD     103           I
152300040826     C                   Z-ADD     TASADG        VIMP(I)
152400040826E   1C                   ENDIF
152500040826     C*
152600040826     C                   ENDSR
152700040826     C*------------------------------------------------------------------------*
152800040826     C* CAL104 - CALCOLA ISTAT SU ADG/TCP
152900040826     C*------------------------------------------------------------------------*
153000040826     C     CAL104        BEGSR
153100040826     C*
153200040826     C* REIMPOSTA LE VARIABILI DI LAVORO
153300040826     C                   Z-ADD     *ZEROS        TASIAC           21 8
153400040826     C*
153500040826     C* CALCOLA L'ISTAT
153600040826     C                   Z-ADD     VIMP(103)     WIMP             23 8
153700040826     C                   EXSR      IACCAL
153800040826     C*
153900040826IF  1C     TASIAC        IFGT      *ZEROS
154000040826     C                   Z-ADD     104           I
154100040826     C                   Z-ADD     TASIAC        VIMP(I)
154200040826E   1C                   ENDIF
154300040826     C*
154400040826     C                   ENDSR
154500970306     C*------------------------------------------------------------------------*
154600970306     C* CALGRP - CALCOLA LE SPESE DI GIACENZA RICONSEGNA PROVINCIA
154700970306     C*------------------------------------------------------------------------*
154800970306     C     CALGRP        BEGSR
154900970306     C*
155000970306     C* REIMPOSTA LE VARIABILI DI LAVORO
155100011031     C                   Z-ADD     *ZEROS        TASSGP           21 8
155200970306     C*
155300970306     C* CALCOLA IL PESO TASSABILE -GIACENZA-
155400970306IF  1C     TGCSGP        IFGT      *ZEROS                                       *IMMESSA TARIFFA
155500970306     C                   Z-ADD     *ZEROS        §PESCA
155600970306     C                   EXSR      CALPTG
155700970306     C*
155800970306     C* CALCOLA LE SPESE DI GIACENZA
155900970306     C     TGCSGP        MULT      ETCNRG        TASSGR                         *SPESE X N°RIAPERTUR
156000970306     C     TASSGR        MULT      §PESCA        TASSGR                         *SPESE X PESO TASSAB
156100970306     C*
156200970306     C* AZZERA LE SPESE DI RICONSEGNA SE SI E' DENTRO AI GIORNI DI FRANCHIGIA
156300970306IF  2C     TGCFAF        IFEQ      '2'                                          *FRANCHIGIA
156400970306     C     ETCGGG        ANDLE     TGCSGF                                       *GG SOSTA < GG FRANC
156500970306     C                   Z-ADD     *ZEROS        TASSGP                         *NO SPESE RICONSEGNA
156600970306E   2C                   ENDIF
156700970306E   1C                   ENDIF
156800970306     C*
156900970314IF  1C     TASSGP        IFGT      *ZEROS
157000970306     C                   Z-ADD     18            I
157100970306     C                   Z-ADD     TASSGP        VIMP(I)
157200970314E   1C                   ENDIF
157300970306     C*
157400970306     C                   ENDSR
157500001227     C*------------------------------------------------------------------------*
157600001227     C* CAL057 - CALCOLA ADG SU GIACENZA RICONSEGNA PROVINCIA
157700001227     C*------------------------------------------------------------------------*
157800001227     C     CAL057        BEGSR
157900001227     C*
158000001227     C* REIMPOSTA LE VARIABILI DI LAVORO
158100011031     C                   Z-ADD     *ZEROS        TASADG           21 8
158200001227     C*
158300001227     C* CALCOLA L'A.D.G.
158400011031     C                   Z-ADD     VIMP(18)      WIMP             23 8
158500001227     C                   EXSR      ADGCAL
158600001227     C*
158700001227IF  1C     TASADG        IFGT      *ZEROS
158800001227     C                   Z-ADD     57            I
158900001227     C                   Z-ADD     TASADG        VIMP(I)
159000001227E   1C                   ENDIF
159100001227     C*
159200001227     C                   ENDSR
159300001227     C*------------------------------------------------------------------------*
159400001227     C* CAL074 - CALCOLA ISTAT SU ADG/GIACENZA RICONSEGNA PROVINCIA
159500001227     C*------------------------------------------------------------------------*
159600001227     C     CAL074        BEGSR
159700001227     C*
159800001227     C* REIMPOSTA LE VARIABILI DI LAVORO
159900011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
160000001227     C*
160100001227     C* CALCOLA L'ISTAT
160200011031     C                   Z-ADD     VIMP(57)      WIMP             23 8
160300001227     C                   EXSR      IACCAL
160400001227     C*
160500001227IF  1C     TASIAC        IFGT      *ZEROS
160600001227     C                   Z-ADD     74            I
160700001227     C                   Z-ADD     TASIAC        VIMP(I)
160800001227E   1C                   ENDIF
160900001227     C*
161000001227     C                   ENDSR
161100970306     C*------------------------------------------------------------------------*
161200970306     C* CALGRC - CALCOLA LE SPESE DI GIACENZA RICONSEGNA CITTA'
161300970306     C*------------------------------------------------------------------------*
161400970306     C     CALGRC        BEGSR
161500970306     C*
161600970306     C* REIMPOSTA LE VARIABILI DI LAVORO
161700011031     C                   Z-ADD     *ZEROS        TASSGR           21 8
161800970306     C*
161900970306     C* CALCOLA IL PESO TASSABILE -GIACENZA-
162000970306IF  1C     TGCSGR        IFGT      *ZEROS                                       *IMMESSA TARIFFA
162100970306     C                   Z-ADD     *ZEROS        §PESCA
162200970306     C                   EXSR      CALPTG
162300970306     C*
162400970306     C* CALCOLA LE SPESE DI GIACENZA
162500970306     C     TGCSGR        MULT      ETCNRG        TASSGR                         *SPESE X N°RIAPERTUR
162600970306     C     TASSGR        MULT      §PESCA        TASSGR                         *SPESE X PESO TASSAB
162700970306     C*
162800970306     C* AZZERA LE SPESE DI RICONSEGNA SE SI E' DENTRO AI GIORNI DI FRANCHIGIA
162900970306IF  2C     TGCFAF        IFEQ      '2'                                          *FRANCHIGIA
163000970306     C     ETCGGG        ANDLE     TGCSGF                                       *GG SOSTA < GG FRANC
163100970306     C                   Z-ADD     *ZEROS        TASSGR                         *NO SPESE RICONSEGNA
163200970306E   2C                   ENDIF
163300970306E   1C                   ENDIF
163400970306     C*
163500970314IF  1C     TASSGR        IFGT      *ZEROS
163600970306     C                   Z-ADD     19            I
163700970306     C                   Z-ADD     TASSGR        VIMP(I)
163800970314E   1C                   ENDIF
163900970306     C*
164000970306     C                   ENDSR
164100001227     C*------------------------------------------------------------------------*
164200001227     C* CAL058 - CALCOLA ADG SU GIACENZA RICONSEGNA CITTA'
164300001227     C*------------------------------------------------------------------------*
164400001227     C     CAL058        BEGSR
164500001227     C*
164600001227     C* REIMPOSTA LE VARIABILI DI LAVORO
164700011031     C                   Z-ADD     *ZEROS        TASADG           21 8
164800001227     C*
164900001227     C* CALCOLA L'A.D.G.
165000011031     C                   Z-ADD     VIMP(19)      WIMP             23 8
165100001227     C                   EXSR      ADGCAL
165200001227     C*
165300001227IF  1C     TASADG        IFGT      *ZEROS
165400001227     C                   Z-ADD     58            I
165500001227     C                   Z-ADD     TASADG        VIMP(I)
165600001227E   1C                   ENDIF
165700001227     C*
165800001227     C                   ENDSR
165900001227     C*------------------------------------------------------------------------*
166000001227     C* CAL075 - CALCOLA ISTAT SU ADG/GIACENZA RICONSEGNA CITTA'
166100001227     C*------------------------------------------------------------------------*
166200001227     C     CAL075        BEGSR
166300001227     C*
166400001227     C* REIMPOSTA LE VARIABILI DI LAVORO
166500011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
166600001227     C*
166700001227     C* CALCOLA L'ISTAT
166800011031     C                   Z-ADD     VIMP(58)      WIMP             23 8
166900001227     C                   EXSR      IACCAL
167000001227     C*
167100001227IF  1C     TASIAC        IFGT      *ZEROS
167200001227     C                   Z-ADD     75            I
167300001227     C                   Z-ADD     TASIAC        VIMP(I)
167400001227E   1C                   ENDIF
167500001227     C*
167600001227     C                   ENDSR
167700970306     C*------------------------------------------------------------------------*
167800970306     C* CALGSO - CALCOLA LE SPESE DI GIACENZA DI SOSTA
167900970306     C*------------------------------------------------------------------------*
168000970306     C     CALGSO        BEGSR
168100970306     C*
168200970306     C     *LIKE         DEFINE    ETCGGG        SAVGGG
168300970306     C     *LIKE         DEFINE    ETCGGG        GGGETC
168400970306     C*
168500970306     C* REIMPOSTA LE VARIABILI DI LAVORO
168600011031     C                   Z-ADD     *ZEROS        TASSGS           21 8
168700970306     C                   Z-ADD     *ZEROS        X                 1 0
168800970306     C                   Z-ADD     *ZEROS        XP                1 0
168900970306     C                   Z-ADD     *ZEROS        SGG                            *GG SOSTA
169000970306     C                   Z-ADD     *ZEROS        STU                            *TARIFFA UNITARIA
169100970306     C                   Z-ADD     *ZEROS        STT                            *TARIFFA TOTALE
169200970306     C*
169300970306     C                   Z-ADD     ETCGGG        SAVGGG
169400970306     C                   Z-ADD     ETCGGG        GGGETC
169500970306DO  1C     1             DO        3             X
169600970306IF  2C     SAVGGG        IFGT      TGS(X)
169700970306     C     SAVGGG        SUB       TGS(X)        GGGETC
169800970306IF  3C     X             IFEQ      1
169900970306     C                   Z-ADD     TGS(X)        SGG(X)
170000970306X   3C                   ELSE
170100970306     C     TGS(X)        SUB       TGS(XP)       SGG(X)
170200970306E   3C                   ENDIF
170300970306     C                   Z-ADD     TTS(X)        STU(X)
170400970306X   2C                   ELSE
170500970306     C                   Z-ADD     GGGETC        SGG(X)
170600970306     C                   Z-ADD     TTS(X)        STU(X)
170700970306     C                   Z-ADD     *ZEROS        SAVGGG
170800970306     C                   Z-ADD     *ZEROS        GGGETC
170900970306E   2C                   ENDIF
171000970306     C                   ADD       1             XP
171100970306E   1C                   ENDDO
171200970306     C*
171300970306     C* CALCOLA IL PESO TASSABILE -GIACENZA-
171400970306     C                   Z-ADD     *ZEROS        §PESCA
171500970306     C                   EXSR      CALPTG
171600970306     C*
171700970306     C* CALCOLA LE SPESE DI GIACENZA
171800970306     C     SGG           MULT      STU           STT
171900970306     C     STT           MULT      §PESCA        STT
172000970306     C                   XFOOT     STT           TASSGS
172100970306     C*
172200970306     C* APPLICA IL MINIMO TARIFFARIO SE SUPERATI I GIORNI DI FRANCHIGIA
172300970306IF  1C     TASSGS        IFLT      TGCSTM
172400970306     C     ETCGGG        ANDGT     TGCSGF
172500970306     C                   Z-ADD     TGCSTM        TASSGS
172600970306E   1C                   ENDIF
172700970306     C*
172800970306     C* AZZERA LE SPESE DI SOSTA SE SI E' DENTRO AI GIORNI DI FRANCHIGIA
172900970306IF  1C     TGCFAF        IFNE      *BLANKS                                      *FRANCHIGIA
173000970306     C     ETCGGG        ANDLE     TGCSGF                                       *GG SOSTA < GG FRANC
173100970306     C                   Z-ADD     *ZEROS        TASSGS                         *NO SPESE SOSTA
173200970306E   1C                   ENDIF
173300970306     C*
173400970314IF  1C     TASSGS        IFGT      *ZEROS
173500970306     C                   Z-ADD     20            I
173600970306     C                   Z-ADD     TASSGS        VIMP(I)
173700970314E   1C                   ENDIF
173800970306     C*
173900970306     C                   ENDSR
174000001227     C*------------------------------------------------------------------------*
174100001227     C* CAL059 - CALCOLA ADG SU GIACENZA SOSTA
174200001227     C*------------------------------------------------------------------------*
174300001227     C     CAL059        BEGSR
174400001227     C*
174500001227     C* REIMPOSTA LE VARIABILI DI LAVORO
174600011031     C                   Z-ADD     *ZEROS        TASADG           21 8
174700001227     C*
174800001227     C* CALCOLA L'A.D.G.
174900011031     C                   Z-ADD     VIMP(20)      WIMP             23 8
175000001227     C                   EXSR      ADGCAL
175100001227     C*
175200001227IF  1C     TASADG        IFGT      *ZEROS
175300001227     C                   Z-ADD     59            I
175400001227     C                   Z-ADD     TASADG        VIMP(I)
175500001227E   1C                   ENDIF
175600001227     C*
175700001227     C                   ENDSR
175800001227     C*------------------------------------------------------------------------*
175900001227     C* CAL076 - CALCOLA ISTAT SU ADG/GIACENZA SOSTA
176000001227     C*------------------------------------------------------------------------*
176100001227     C     CAL076        BEGSR
176200001227     C*
176300001227     C* REIMPOSTA LE VARIABILI DI LAVORO
176400011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
176500001227     C*
176600001227     C* CALCOLA L'ISTAT
176700011031     C                   Z-ADD     VIMP(59)      WIMP             23 8
176800001227     C                   EXSR      IACCAL
176900001227     C*
177000001227IF  1C     TASIAC        IFGT      *ZEROS
177100001227     C                   Z-ADD     76            I
177200001227     C                   Z-ADD     TASIAC        VIMP(I)
177300001227E   1C                   ENDIF
177400001227     C*
177500001227     C                   ENDSR
177600970306     C*------------------------------------------------------------------------*
177700970306     C* CALPTG - CALCOLA IL PESO TASSABILE -GIACENZA-
177800970306     C*------------------------------------------------------------------------*
177900970306     C     CALPTG        BEGSR
178000970306     C*
178100970306     C* REIMPOSTA LE VARIABILI DI LAVORO
178200970306     C                   Z-ADD     *ZEROS        §PESVO           13 3
178300970306     C                   Z-ADD     *ZEROS        §PES§            13 3
178400970306     C                   Z-ADD     *ZEROS        §PESCA            6 0
178500970306     C*
178600970725     C     TGCRPV        MULT      SVSC(S)       §PESVO
178700970306     C     §PESVO        MULT      100           §PESVO
178800970306IF  1C     §PESVO        IFGT      SPSC(S)
178900970306     C     §PESVO        DIV       100           §PES§
179000970306X   1C                   ELSE
179100970306     C     SPSC(S)       DIV       100           §PES§
179200970306E   1C                   ENDIF
179300970306     C     §PES§         ADD       0,99          §PESCA
179400970306     C*
179500970306     C                   ENDSR
179600970306     C*------------------------------------------------------------------------*
179700970306     C* CARTAD - CARICAMENTO TARIFFA CLIENTE -DETTAGLIO-
179800970306     C*------------------------------------------------------------------------*
179900970306     C     CARTAD        BEGSR
180000970306     C*
180100970306     C* REIMPOSTA LE VARIABILI DI LAVORO
180200980707DO  1C     1             DO        11            I
180300970306     C     I             OCCUR     TARDS
180400970306     C                   CLEAR                   TARDS
180500970306E   1C                   ENDDO
180600970306     C                   MOVEL     '0'           $TAD                           *TARIFFA TROVATA
180700970306     C*
180800970306     C* CERCA LA TARIFFA CLIENTE CON IL CODICE TASSAZIONE ELEMENTARE
180900970306     C                   Z-ADD     D00KSC        KTDKSC                         *CLIENTE
181000970306     C                   Z-ADD     D00PRG        KTDPRG                         *PROGRESSIVO
181100970306     C                   Z-ADD     D00LNP        KTDLNP                         *LINEA DI PARTENZA
181200990819     C                   MOVEL     CCTS(T)       KTDCTS                         *COD.TASS.ELEMENTARE
181300990824     C                   MOVEL     *BLANKS       KTDNAZ                         *NAZIONE
181400970306     C                   MOVEL     *BLANKS       KTDCAP                         *C.A.P.
181500970306     C                   Z-ADD     D00CTR        KTDCTR                         *CODICE TARIFFA
181600990819     C     KEYTAD        SETLL     TITAD00L                               98
181700970306IF  1C     *IN98         IFEQ      *ON                                          *TARIFFA TROVATA
181800970306     C                   MOVEL     '1'           $TAD
181900970306X   1C                   ELSE                                                   *TARIFFA NON TROVATA
182000970306     C*
182100970306     C* CERCA LA TARIFFA CLIENTE CON IL CODICE TASSAZIONE REGIONE
182200990803     C                   MOVEL     CRAP(T)       KTDCTS                         *COD.TASS.REGIONE
182300990819     C     KEYTAD        SETLL     TITAD00L                               98
182400990819IF  2C     *IN98         IFEQ      *ON                                          *TARIFFA TROVATA
182500970306     C                   MOVEL     '1'           $TAD
182600990819X   2C                   ELSE                                                   *TARIFFA NON TROVATA
182700970306     C*
182800970306     C* CERCA LA TARIFFA CLIENTE CON IL CODICE TASSAZIONE NAZIONE
182900990803     C                   MOVEL     CNAR(T)       KTDCTS                         *COD.TASS.NAZIONE
183000990819     C     KEYTAD        SETLL     TITAD00L                               98
183100990819IF  3C     *IN98         IFEQ      *ON                                          *TARIFFA TROVATA
183200970306     C                   MOVEL     '1'           $TAD
183300990819E   3C                   ENDIF
183400990819E   2C                   ENDIF
183500990819E   1C                   ENDIF
183600970306     C*
183700970306     C* SE TARIFFA NON TROVATA, ERRORE
183800970306IF  1C     $TAD          IFEQ      '0'
183900970306     C                   MOVEL     MSG(24)       $MSG
184000970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
184100970306     C*
184200970306     C* SE TARIFFA TROVATA, CARICA DETTAGLIO
184300970306X   1C                   ELSE
184400980707     C                   EXSR      CARDST
184500970306E   1C                   ENDIF
184600970306     C*
184700970306     C                   ENDSR
184800980707     C*------------------------------------------------------------------------*
184900980707     C* CARDST - CARICA CAMPI TARIFFA -DETTAGLIO-
185000980707     C*------------------------------------------------------------------------*
185100980707     C     CARDST        BEGSR
185200980707     C*
185300980707     C     *LIKE         DEFINE    TADRPV        RAPPV
185400980707     C                   DO        11            A                 5 0
185500980707     C     A             OCCUR     TARDS
185600980707     C                   CLEAR                   TARDS
185700980707     C                   ENDDO
185800980707     C                   Z-ADD     0             RAPPV
185900980707     C                   CLEAR                   WELAMT
186000980908     C                   Z-ADD     DMITAS        MINTAS                         *MIN.TASS. TNTAM
186100980707    1C                   DO        *HIVAL        A
186200990819     C**
186300980707     C** NON DEVO RILEGGERE PERCHE' DEVO CARICARE LO STESSO SCAGLIONE
186400980707    2C     WELAMT        IFNE      'S'
186500990819     C     KEYTAD        READE     TITAD000                               15
186600980707     C   15              GOTO      ENDDST
186700991125     C*
186800991125     C* CONTROLLA SE CONVERTIRE I CAMPI DI TNTAM
186900011026    3C     D00DIV        IFNE      WDIVCE
187000011019     C                   MOVEL     D00DIV        WDVI
187100991125     C                   EXSR      CNVTAD
187200991125    3C                   ENDIF
187300991125    2C                   ENDIF
187400980707     C** ELABORATO UNA VOLTA NON LO FACCIO PIU'
187500980707    2C     WELAMT        IFEQ      'S'
187600980707     C                   MOVEL     'E'           WELAMT            1
187700980707    2C                   ENDIF
187800980707     C**
187900980707     C     A             OCCUR     TARDS
188000980707     C* SE LO SCAGLIONE LETTO E' > DEL MINIMO TASSABILE CARICO
188100980707     C*  ANCHE LO SCAGLIONE DEL MINIMO CON TARIFFA=TARIFFA X MIN.TAS
188200980707     C**
188300980707     C                   Z-ADD     TADLNP        TLNP
188400980707     C                   MOVEL     TADCTS        TCTS
188500990819     C                   MOVEL     TADNAZ        TNAZ
188600990819     C                   MOVEL     TADCAP        TCAP
188700980707     C                   Z-ADD     TADCTR        TCTR
188800980707     C                   Z-ADD     TADRPV        TRPV
188900980707     C                   Z-ADD     TADMIN        TMIN
189000980707     C                   Z-ADD     TADMAX        TMAX
189100980707     C                   Z-ADD     TADRPV        RAPPV
189200991125     C*
189300980707     C** SE HO GIA' ELABORATO IL MINIMO NON LO FACCIO PIU'
189400980707    2C     WELAMT        IFNE      'E'
189500980707     C**
189600980707    3C     TADSGL        IFEQ      MINTAS
189700980707     C                   MOVEL     'E'           WELAMT
189800980707   X3C                   ELSE
189900980707     C** SE LO SCAGLIONE CHE LEGGO E' MAGGIORE DEL MINIMO TASSABILE
190000980707     C*  UTILIZZO PRIMA LA TARIFFA DELLO SCAGLIONE PER IL MINIMO T.
190100980707     C*  (MOLTIPLICANDOLA PER IL MINIMO TASSABILE)
190200980707     C*  POI LO UTILIZZO PER LO SCAGLIONE LETTO
190300980707    4C     TADSGL        IFGT      MINTAS
190400980707     C                   MOVEL     'S'           WELAMT
190500980707     C                   Z-ADD     MINTAS        TSCG
190600980707     C**
190700980707     C* SE NON C'E' APPLICAZIONE TARIFFA FINITA -- MOLTIPLICO
190800980707    5C     TAMATA        IFEQ      0
190900980707     C     TAMATA        ORLE      MINTAS
191000980707     C     TADSGL        ORGT      TAMATA
191100980707     C**
191200991125    6C     UNOCTR        IFEQ      4
191300980707     C     UNOCTR        OREQ      0
191400011031     C     MINTAS        DIV(H)    100           WMTAS            21 8
191500980707     C                   ELSE
191600980707     C                   Z-ADD     MINTAS        WMTAS
191700980707    6C                   ENDIF
191800980707     C* TARIFFA
191900010926     C     WMTAS         MULT(H)   TADITR        TITR
192000980707     C     WMTAS         MULT(H)   TADINO        TINO
192100980707     C                   ELSE
192200980707     C* C'E' APPLICAZIONE MAGGIORE --> UTILIZZO LA TARIFFA COSI' COM'E'
192300980707     C                   Z-ADD     TADITR        TITR
192400980707     C                   Z-ADD     TADINO        TINO
192500980707    5C                   ENDIF
192600980707    4C                   ENDIF
192700980707    3C                   ENDIF
192800980707    2C                   ENDIF
192900980707     C*
193000980707     C* SE NON HO CARICATO LO SCAGLIONE PER IL MINIMO TASSABILE
193100980707    2C     WELAMT        IFNE      'S'
193200980707     C                   Z-ADD     TADSGL        TSCG
193300980707     C                   Z-ADD     TADITR        TITR
193400980707     C                   Z-ADD     TADINO        TINO
193500980707    2C                   ENDIF
193600980707    1C                   ENDDO
193700991125     C*
193800980707     C     ENDDST        ENDSR
193900980707     C*------------------------------------------------------------------------*
194000980707     C* CALQTA - REPERISCE E CALCOLA LA QUANTITA' DA UTILIZZARE
194100980707     C*------------------------------------------------------------------------*
194200980707     C     CALQTA        BEGSR
194300980707     C*
194400980707     C* PESO TASSABILE E QUANTITA' DA FATTURARE
194500980707     C                   MOVEL     UNOCTR        WTPG              1
194600980707     C                   CLEAR                   WRPV
194700980707     C                   MOVEL     'TAD'         WDET              3
194800980707     C                   Z-ADD     TAMARL        WARL
194900980707     C                   Z-ADD     TAMARF        WARF
195000980707     C                   Z-ADD     TAMARO        WARO
195100980707     C                   EXSR      CALCOL
195200980707     C                   Z-ADD     ISOPES        PESTAS           13 3
195300980707     C*
195400980707     C                   ENDSR
195500980707     C*------------------------------------------------------------------------*
195600980707     C* CALCOL - CALCOLA RPV, ARROTONDAMENTI E PESO TASSABILE
195700980707     C*------------------------------------------------------------------------*
195800980707     C     CALCOL        BEGSR
195900980707     C*
196000980707     C     *LIKE         DEFINE    TAMARL        ARR
196100980707     C     *LIKE         DEFINE    TPTRPV        WRPV
196200980707     C     *LIKE         DEFINE    TPTARL        WARL
196300980707     C     *LIKE         DEFINE    TPTARF        WARF
196400980707     C     *LIKE         DEFINE    TPTARO        WARO
196500980707     C                   Z-ADD     0             ISOPES           13 3
196600980707SL  1C                   SELECT
196700980707     C* TARIFFA A QLI
196800980707     C     WTPG          WHENEQ    '0'
196900980707     C                   Z-ADD     SPSC(S)       ISOPES
197000980707     C* TARIFFA A COLLI
197100980707     C     WTPG          WHENEQ    '1'
197200980707     C                   Z-ADD     SCSC(S)       ISOPES
197300980707     C* TARIFFA A SPEDIZIONE
197400980707     C     WTPG          WHENEQ    '2'
197500980707     C                   Z-ADD     1             ISOPES
197600980707     C* TARIFFA A KG
197700980707     C     WTPG          WHENEQ    '3'
197800980707     C                   Z-ADD     SPSC(S)       ISOPES
197900980707E   1C                   ENDSL
198000980707     C** MINIMO TASSABILE STD
198100980707     C                   Z-ADD     0             MINTAS
198200980707     C**
198300980707     C                   Z-ADD     1             A
198400980707     C     WTPG          LOOKUP    CTR(A)                                 05
198500980707     C   05A             OCCUR     DSTR
198600980707     C   05              Z-ADD     §TRATA        MINTAS
198700980707     C**
198800980707     C** SOLO PER TAD (PER TPD NON MI SERVE)
198900980707    1C     WDET          IFEQ      'TAD'
199000980707     C                   CLEAR                   MINTAR
199100980707     C** UTILIZZO IL CAMPO CON I DECIMALI PER IL MINIMO TASSABILE
199200980707     C*  DA TABELLA TR. SE POI C'E' ANCHE IN TARIFFA USO QUELLO
199300980707     C   05              Z-ADD     §TRATA        MINTAR
199400980707    2C     TAMATA        IFGT      0
199500980707     C                   Z-ADD     TAMATA        MINTAR
199600980707    2C                   ENDIF
199700980707    1C                   ENDIF
199800980707     C**
199900980707     C** RPV SOLO PER TARIFFA 0 E 3
200000980707    1C     §TRRPV        IFEQ      'S'
200100980707     C** PER TAD PRENDO RPV DA RIGA DI DETTAGLIO CON PESO REALE
200200980707    2C     WDET          IFEQ      'TAD'
200300980707     C     SVSC(S)       ANDGT     0
200400980707    3C                   DO        11            A
200500980707     C     A             OCCUR     TARDS
200600980707    4C     TSCG          IFGE      ISOPES
200700980707     C                   GOTO      POITAS
200800980707    4C                   END
200900980707    3C                   END
201000980707     C** RICERCO RPV CON PESO REALE
201100980707    3C     TSCG          IFLT      ISOPES
201200980707     C                   MOVEL     MSG(25)       $MSG
201300980707     C                   EXSR      IMPERR
201400980707    3C                   END
201500980707     C     POITAS        TAG
201600980707     C                   Z-ADD     TRPV          WRPV
201700980707    2C                   ENDIF
201800980707     C**
201900980707    2C     SVSC(S)       IFNE      0
202000980707     C     WRPV          ANDNE     0
202100980707     C                   Z-ADD     0             COMPRV            9 3
202200980707     C     SVSC(S)       MULT      WRPV          COMPRV
202300980707     C     COMPRV        MULT      100           ISOPE2           13 3
202400980707    3C     ISOPE2        IFGT      ISOPES
202500980707     C                   Z-ADD     ISOPE2        ISOPES
202600980707    3C                   ENDIF
202700980707    2C                   ENDIF
202800980707    1C                   ENDIF
202900980707     C**
203000980707     C** ARROTONDAMENTI: LO FACCIO SEOCONDO QUANTO INDICATO IN DSTR
203100980707    0C     §TRARR        IFEQ      'S'
203200980707     C**
203300980707     C** ARROTONDAMENTO: TARIFFE 0 Q.LI - 1 - 3
203400980707     C     ISOPES        IFGE      MINTAS
203500980707     C                   Z-ADD     0             ARR
203600980707     C     ISOPES        IFGT      WARL
203700980707     C                   Z-ADD     WARO          ARR
203800980707     C                   ELSE
203900980707     C                   Z-ADD     WARF          ARR
204000980707     C                   END
204100980707     C**
204200011031     C                   Z-ADD     0             RESTO            21 8
204300011108     C                   Z-ADD     0             ISOPEX           21 0
204400980707     C     ARR           IFNE      0
204500980707     C     ISOPES        ANDNE     0
204600980707     C     ISOPES        DIV       ARR           ISOPEX
204700980707     C                   MVR                     RESTO
204800980707     C     RESTO         IFGT      0
204900980707     C     ISOPEX        MULT      ARR           ISOPES
205000980707     C                   ADD       ARR           ISOPES
205100980707     C                   ENDIF
205200980707     C                   ENDIF
205300980707     C                   ENDIF
205400980707     C**
205500980707    0C                   ENDIF
205600980707     C                   ENDSR
205700980707     C*------------------------------------------------------------------------*
205800980707     C* VALVO3 - VALORIZZA LE VOCI -DIPENDENTI DA CODICE TASSAZIONE-
205900980707     C*------------------------------------------------------------------------*
206000980707     C     VALVO3        BEGSR
206100970306     C*
206200970306     C* AZZERA GLI IMPORTI DELLE VOCI CON SEQUENZA TRE
206300970306DO  1C     1             DO        NVOCA         I
206400970306IF  2C     VS(I)         IFEQ      3
206500970306     C                   Z-ADD     *ZEROS        VIMP(I)                        *IMPORTO CALCOLATO
206600970306E   2C                   ENDIF
206700970306E   1C                   ENDDO
206800980702     C*---
206900980702     C* PORTO E INOLTRO
207000980702     C*---
207100980707IF  1C     WERR          IFEQ      'N'
207200980707IF  2C     VC(1)         IFEQ      'S'                                          *VOCE DA CALCOLARE
207300980707     C     VC(6)         OREQ      'S'                                          *VOCE DA CALCOLARE
207400980707     C                   EXSR      CALPEI
207500980707E   2C                   ENDIF
207600980707E   1C                   ENDIF
207700980702     C*---
207800980702     C* ALTRE PORTO
207900980702     C*---
208000980702     C* CALCOLA A.D.G. SUL PORTO
208100980702IF  1C     WERR          IFEQ      'N'
208200980702IF  2C     VC(3)         IFEQ      'S'                                          *VOCE DA CALCOLARE
208300980702     C                   EXSR      CALAGP
208400980702E   2C                   ENDIF
208500980702E   1C                   ENDIF
208600980702     C*
208700980702     C* CALCOLA ISTAT SUL PORTO
208800980702IF  1C     WERR          IFEQ      'N'
208900980702IF  2C     VC(4)         IFEQ      'S'                                          *VOCE DA CALCOLARE
209000980702     C                   EXSR      CALISP
209100980702E   2C                   ENDIF
209200980702E   1C                   ENDIF
209300980702     C*
209400980702     C* CALCOLA ISTAT SULL'A.D.G. DEL PORTO
209500980702IF  1C     WERR          IFEQ      'N'
209600980702IF  2C     VC(27)        IFEQ      'S'                                          *VOCE DA CALCOLARE
209700980702     C                   EXSR      CALIAP
209800980702E   2C                   ENDIF
209900980702E   1C                   ENDIF
210000980702     C*
210100980702     C*---
210200980702     C* ALTRE INOLTRO
210300980702     C*---
210400980702     C* CALCOLA A.D.G. SULL'INOLTRO
210500980702IF  1C     WERR          IFEQ      'N'
210600980702IF  2C     VC(21)        IFEQ      'S'                                          *VOCE DA CALCOLARE
210700980702     C                   EXSR      CALAGI
210800980702E   2C                   ENDIF
210900980707E   1C                   ENDIF
211000980702     C*
211100980702     C* CALCOLA ISTAT SULL'INOLTRO
211200980702IF  1C     WERR          IFEQ      'N'
211300980702IF  2C     VC(24)        IFEQ      'S'                                          *VOCE DA CALCOLARE
211400980702     C                   EXSR      CALISI
211500980702E   2C                   ENDIF
211600980702E   1C                   ENDIF
211700980702     C*
211800980702     C* CALCOLA ISTAT SULL'A.D.G. DELL'INOLTRO
211900980702IF  1C     WERR          IFEQ      'N'
212000980702IF  2C     VC(29)        IFEQ      'S'                                          *VOCE DA CALCOLARE
212100980702     C                   EXSR      CALIAI
212200980702E   2C                   ENDIF
212300980702E   1C                   ENDIF
212400980702     C*---
212500980702     C* LOCALITA' DISAGIATE
212600980702     C*---
212700980702     C* CALCOLA CONSEGNA PARTICOLARE LOCALITA' DISAGIATE
212800980702IF  1C     WERR          IFEQ      'N'
212900980702IF  2C     VC(35)        IFEQ      'S'                                          *VOCE DA CALCOLARE
213000980702     C                   EXSR      CALCPL
213100980702E   2C                   ENDIF
213200980702E   1C                   ENDIF
213300980702     C*
213400980702     C* CALCOLA A.D.G. SULLE CONSEGNE LOCALITA' DISAGIATE
213500980702IF  1C     WERR          IFEQ      'N'
213600980702IF  2C     VC(43)        IFEQ      'S'                                          *VOCE DA CALCOLARE
213700980702     C                   EXSR      CALAGL
213800980702E   2C                   ENDIF
213900980702E   1C                   ENDIF
214000980702     C*
214100980702     C* CALCOLA ISTAT SULLE CONSEGNE LOCALITA' DISAGIATE
214200980702IF  1C     WERR          IFEQ      'N'
214300980702IF  2C     VC(44)        IFEQ      'S'                                          *VOCE DA CALCOLARE
214400980702     C                   EXSR      CALISL
214500980702E   2C                   ENDIF
214600980702E   1C                   ENDIF
214700980702     C*
214800980702     C* CALCOLA ISTAT SULL'A.D.G. DELLE CONSEGNE LOCALITA' DISAGIATE
214900980702IF  1C     WERR          IFEQ      'N'
215000980702IF  2C     VC(45)        IFEQ      'S'                                          *VOCE DA CALCOLARE
215100980702     C                   EXSR      CALIAL
215200980702E   2C                   ENDIF
215300980702E   1C                   ENDIF
215400011031     C*---
215500011031     C* RITIRO ISOLA
215600011031     C*---
215700011031     C* CALCOLA RITIRO ISOLA
215800011031IF  1C     WERR          IFEQ      'N'
215900011031IF  2C     VC(88)        IFEQ      'S'                                          *VOCE DA CALCOLARE
216000011031     C                   EXSR      CALSAM
216100011031E   2C                   ENDIF
216200011031E   1C                   ENDIF
216300011031     C*
216400011031     C* CALCOLA A.D.G. SUL RITIRO ISOLA
216500011031IF  1C     WERR          IFEQ      'N'
216600011031IF  2C     VC(93)        IFEQ      'S'                                          *VOCE DA CALCOLARE
216700011031     C                   EXSR      CALAGR
216800011031E   2C                   ENDIF
216900011031E   1C                   ENDIF
217000011031     C*
217100011031     C* CALCOLA ISTAT SUL RITIRO ISOLA
217200011031IF  1C     WERR          IFEQ      'N'
217300011031IF  2C     VC(94)        IFEQ      'S'                                          *VOCE DA CALCOLARE
217400011031     C                   EXSR      CALISR
217500011031E   2C                   ENDIF
217600011031E   1C                   ENDIF
217700011031     C*
217800011031     C* CALCOLA ISTAT SULL'A.D.G. DEL RITIRO ISOLA
217900011031IF  1C     WERR          IFEQ      'N'
218000011031IF  2C     VC(95)        IFEQ      'S'                                          *VOCE DA CALCOLARE
218100011031     C                   EXSR      CALIAR
218200011031E   2C                   ENDIF
218300011031E   1C                   ENDIF
218400011030     C*---
218500011030     C* CONSEGNA ISOLA
218600011030     C*---
218700091023     C* CALCOLA CONSEGNA PARTICOLARE ISOLA
218800011030IF  1C     WERR          IFEQ      'N'
218900011030IF  2C     VC(80)        IFEQ      'S'                                          *VOCE DA CALCOLARE
219000011030     C                   EXSR      CALCIS
219100011030E   2C                   ENDIF
219200011030E   1C                   ENDIF
219300011030     C*
219400011030     C* CALCOLA A.D.G. SULLE CONSEGNE ISOLA
219500011030IF  1C     WERR          IFEQ      'N'
219600011030IF  2C     VC(85)        IFEQ      'S'                                          *VOCE DA CALCOLARE
219700011030     C                   EXSR      CALAGS
219800011030E   2C                   ENDIF
219900011030E   1C                   ENDIF
220000011030     C*
220100011030     C* CALCOLA ISTAT SULLE CONSEGNE ISOLA
220200011030IF  1C     WERR          IFEQ      'N'
220300011030IF  2C     VC(86)        IFEQ      'S'                                          *VOCE DA CALCOLARE
220400011030     C                   EXSR      CALISS
220500011030E   2C                   ENDIF
220600011030E   1C                   ENDIF
220700011030     C*
220800011030     C* CALCOLA ISTAT SULL'A.D.G. DELLE CONSEGNE ISOLA
220900011030IF  1C     WERR          IFEQ      'N'
221000011030IF  2C     VC(87)        IFEQ      'S'                                          *VOCE DA CALCOLARE
221100011030     C                   EXSR      CALIAS
221200011030E   2C                   ENDIF
221300011030E   1C                   ENDIF
221400980702     C*---
221500011030     C* INOLTRO + LOCALITA' DISAGIATE + CONSEGNA ISOLA
221600980702     C*---
221700980702     C* RICALCOLA INOLTRO E VOCI CORRELATE RISPETTO AL FLAG APPICAZIONE TARIFFA INOLTRO
221800980702IF  1C     WERR          IFEQ      'N'
221900011030IF  2C     VC(35)        IFEQ      'S'                                          *L.DISAGIATA
222000011030     C     VC(80)        OREQ      'S'                                          *C.ISOLA
222100980702     C                   EXSR      CA3INL
222200980702E   2C                   ENDIF
222300980702E   1C                   ENDIF
222400091023     c* sommo porto + inoltro + disagiate + cons.isola + ritiro isola x
222500091023     C* calcolare espresso una volta sola e poi riproporziono
222600091023     c* l'espresso trovato in base ai valori delle voci originali
222700091023     C* CALCOLA ESPRESSO
222800091023IF  1C     WERR          IFEQ      'N'
222900091023IF  2C     VC(7)         IFEQ      'S'                                          *VOCE DA CALCOLARE
223000091023     C                   EXSR      CALESP
223100091023     c                   exsr      calimpesp
223200091023E   2C                   ENDIF
223300091023E   1C                   ENDIF
223400091023     C*
223500091023     C* CALCOLA A.D.G. SULL'ESPRESSO PORTO
223600091023IF  1C     WERR          IFEQ      'N'
223700091023IF  2C     VC(23)        IFEQ      'S'                                          *VOCE DA CALCOLARE
223800091023     C                   EXSR      CALAEP
223900091023E   2C                   ENDIF
224000091023E   1C                   ENDIF
224100091023     C*
224200091023     C* CALCOLA ISTAT SULL'ESPRESSO PORTO
224300091023IF  1C     WERR          IFEQ      'N'
224400091023IF  2C     VC(26)        IFEQ      'S'                                          *VOCE DA CALCOLARE
224500091023     C                   EXSR      CALIEP
224600091023E   2C                   ENDIF
224700091023E   1C                   ENDIF
224800091023     C*
224900091023     C* CALCOLA ISTAT SULL'A.D.G. DELL'ESPRESSO PORTO
225000091023IF  1C     WERR          IFEQ      'N'
225100091023IF  2C     VC(30)        IFEQ      'S'                                          *VOCE DA CALCOLARE
225200091023     C                   EXSR      CAIAEP
225300091023E   2C                   ENDIF
225400091023E   1C                   ENDIF
225500091023     C*
225600091023     C* CALCOLA ESPRESSO SULL'INOLTRO
225700091023     c* non lo deve + fare l'espresso va calcolato una volta sola sulla
225800091023     c* somma del porto, isola, inoltr, diasgiate, ritiro isola
225900091023IF  1C*    WERR          IFEQ      'N'
226000091023IF  2C*    VC(36)        IFEQ      'S'                                          *VOCE DA CALCOLARE
226100091023     C*                  EXSR      CALESI
226200091023E   2C*                  ENDIF
226300091023E   1C*                  ENDIF
226400091023     C*
226500091023     C* CALCOLA A.D.G. SULL'ESPRESSO INOLTRO
226600091023IF  1C     WERR          IFEQ      'N'
226700091023IF  2C     VC(37)        IFEQ      'S'                                          *VOCE DA CALCOLARE
226800091023     C                   EXSR      CALAEI
226900091023E   2C                   ENDIF
227000091023E   1C                   ENDIF
227100091023     C*
227200091023     C* CALCOLA ISTAT SULL'ESPRESSO INOLTRO
227300091023IF  1C     WERR          IFEQ      'N'
227400091023IF  2C     VC(38)        IFEQ      'S'                                          *VOCE DA CALCOLARE
227500091023     C                   EXSR      CALIEI
227600091023E   2C                   ENDIF
227700091023E   1C                   ENDIF
227800091023     C*
227900091023     C* CALCOLA ISTAT SULL'A.D.G. DELL'ESPRESSO INOLTRO
228000091023IF  1C     WERR          IFEQ      'N'
228100091023IF  2C     VC(34)        IFEQ      'S'                                          *VOCE DA CALCOLARE
228200091023     C                   EXSR      CAIAEI
228300091023E   2C                   ENDIF
228400091023E   1C                   ENDIF
228500091023     C*
228600091023     C* CALCOLA ESPRESSO SULLA CONSEGNA LOCALITA' DISAGIATE
228700091023     c* non lo deve + fare l'espresso va calcolato una volta sola sulla
228800091023     c* somma del porto, isola, inoltr, diasgiate, ritiro isola
228900091023IF  1C*    WERR          IFEQ      'N'
229000091023IF  2C*    VC(39)        IFEQ      'S'                                          *VOCE DA CALCOLARE
229100091023     C*                  EXSR      CALESL
229200091023E   2C*                  ENDIF
229300091023E   1C*                  ENDIF
229400091023     C*
229500091023     C* CALCOLA A.D.G. SULL'ESPRESSO CONSEGNA LOCALITA' DISAGIATE
229600091023IF  1C     WERR          IFEQ      'N'
229700091023IF  2C     VC(40)        IFEQ      'S'                                          *VOCE DA CALCOLARE
229800091023     C                   EXSR      CALAEL
229900091023E   2C                   ENDIF
230000091023E   1C                   ENDIF
230100091023     C*
230200091023     C* CALCOLA ISTAT SULL'ESPRESSO CONSEGNA LOCALITA' DISAGIATE
230300091023IF  1C     WERR          IFEQ      'N'
230400091023IF  2C     VC(41)        IFEQ      'S'                                          *VOCE DA CALCOLARE
230500091023     C                   EXSR      CALIEL
230600091023E   2C                   ENDIF
230700091023E   1C                   ENDIF
230800091023     C*
230900091023     C* CALCOLA ISTAT SULL'A.D.G. DELL'ESPRESSO CONSEGNA LOCALITA' DISAGIATE
231000091023IF  1C     WERR          IFEQ      'N'
231100091023IF  2C     VC(42)        IFEQ      'S'                                          *VOCE DA CALCOLARE
231200091023     C                   EXSR      CAIAEL
231300091023E   2C                   ENDIF
231400091023E   1C                   ENDIF
231500091023     C*
231600091023     C* CALCOLA ESPRESSO SUL RITIRO ISOLA
231700091023     c* non lo deve + fare l'espresso va calcolato una volta sola sulla
231800091023     c* somma del porto, isola, inoltr, diasgiate, ritiro isola
231900091023IF  1C*    WERR          IFEQ      'N'
232000091023IF  2C*    VC(89)        IFEQ      'S'                                          *VOCE DA CALCOLARE
232100091023     C*                  EXSR      CALESR
232200091023E   2C*                  ENDIF
232300091023E   1C*                  ENDIF
232400091023     C*
232500091023     C* CALCOLA A.D.G. SULL'ESPRESSO RITIRO ISOLA
232600091023IF  1C     WERR          IFEQ      'N'
232700091023IF  2C     VC(90)        IFEQ      'S'                                          *VOCE DA CALCOLARE
232800091023     C                   EXSR      CALAER
232900091023E   2C                   ENDIF
233000091023E   1C                   ENDIF
233100091023     C*
233200091023     C* CALCOLA ISTAT SULL'ESPRESSO RITIRO ISOLA
233300091023IF  1C     WERR          IFEQ      'N'
233400091023IF  2C     VC(91)        IFEQ      'S'                                          *VOCE DA CALCOLARE
233500091023     C                   EXSR      CALIER
233600091023E   2C                   ENDIF
233700091023E   1C                   ENDIF
233800091023     C*
233900091023     C* CALCOLA ISTAT SULL'A.D.G. DELL'ESPRESSO RITIRO ISOLA
234000091023IF  1C     WERR          IFEQ      'N'
234100091023IF  2C     VC(92)        IFEQ      'S'                                          *VOCE DA CALCOLARE
234200091023     C                   EXSR      CAIAER
234300091023E   2C                   ENDIF
234400091023E   1C                   ENDIF
234500091023     C*
234600091023     C* CALCOLA ESPRESSO SULLA CONSEGNA ISOLA
234700091023     c* non lo deve + fare l'espresso va calcolato una volta sola sulla
234800091023     c* somma del porto, isola, inoltr, diasgiate, ritiro isola
234900091023IF  1C*    WERR          IFEQ      'N'
235000091023IF  2C*    VC(81)        IFEQ      'S'                                          *VOCE DA CALCOLARE
235100091023     C*                  EXSR      CALESS
235200091023E   2C*                  ENDIF
235300091023E   1C*                  ENDIF
235400091023     C*
235500091023     C* CALCOLA A.D.G. SULL'ESPRESSO CONSEGNA ISOLA
235600091023IF  1C     WERR          IFEQ      'N'
235700091023IF  2C     VC(82)        IFEQ      'S'                                          *VOCE DA CALCOLARE
235800091023     C                   EXSR      CALAES
235900091023E   2C                   ENDIF
236000091023E   1C                   ENDIF
236100091023     C*
236200091023     C* CALCOLA ISTAT SULL'ESPRESSO CONSEGNA ISOLA
236300091023IF  1C     WERR          IFEQ      'N'
236400091023IF  2C     VC(83)        IFEQ      'S'                                          *VOCE DA CALCOLARE
236500091023     C                   EXSR      CALIES
236600091023E   2C                   ENDIF
236700091023E   1C                   ENDIF
236800091023     C*
236900091023     C* CALCOLA ISTAT SULL'A.D.G. DELL'ESPRESSO CONSEGNA ISOLA
237000091023IF  1C     WERR          IFEQ      'N'
237100091023IF  2C     VC(84)        IFEQ      'S'                                          *VOCE DA CALCOLARE
237200091023     C                   EXSR      CAIAES
237300091023E   2C                   ENDIF
237400091023E   1C                   ENDIF
237500091023     C*
237600980703     C* CALCOLA LA BASE PER IL CALCOLO DELLE TARIFFE CON % SUL TRASPORTO
237700980706     C                   EXSR      CALTBT
237800980702     C*---
237900980702     C* CONSEGNE PARTICOLARI
238000980702     C*---
238100060314     C* CALCOLA ASSICURAZIONE ac plus (FRANCO E ASSEGNATO)
238200970306IF  1C     WERR          IFEQ      'N'
238300970306IF  2C     VC(9)         IFEQ      'S'                                          *VOCE DA CALCOLARE
238400970306     C     VC(31)        OREQ      'S'                                          *VOCE DA CALCOLARE
238500060315     C* REIMPOSTA VARIABILI DI LAVORO
238600060315     C                   Z-ADD     ETCASC        IASLIT           23 8
238700060315     C                   MOVEL     'C'           KTPFTC
238800970306     C                   EXSR      CALASC
238900001227     C                   EXSR      CAL048
239000001227     C                   EXSR      CAL065
239100001227     C                   EXSR      CAL060
239200001227     C                   EXSR      CAL077
239300970306E   2C                   ENDIF
239400970306E   1C                   ENDIF
239500060314     C* CALCOLA ASSICURAZIONE ac base (FRANCO E ASSEGNATO)
239600060315     c* no per cartello
239700060315     C                   MOVEL     D00KSC        WA4               4 0
239800060315     c                   if        wa4  <> 8888
239900060314IF  1C     WERR          IFEQ      'N'
240000060314IF  2C     VC(105)       IFEQ      'S'                                          *VOCE DA CALCOLARE
240100060314     C     VC(108)       OREQ      'S'                                          *VOCE DA CALCOLARE
240200060315     C                   Z-ADD     0             IASLIT           23 8
240300060315     C                   MOVEL     'd'           KTPFTC
240400060314     C                   EXSR      CALAcb
240500060314     C                   EXSR      CAL106
240600060314     C                   EXSR      CAL107
240700060314     C                   EXSR      CAL109
240800060314     C                   EXSR      CAL110
240900060314E   2C                   ENDIF
241000060314E   1C                   ENDIF
241100060315E   1C                   ENDIF
241200970306     C*
241300970306     C* CALCOLA ASSICURAZIONE R.C.V. (FRANCO E ASSEGNATO)
241400970306IF  1C     WERR          IFEQ      'N'
241500970306IF  2C     VC(10)        IFEQ      'S'                                          *VOCE DA CALCOLARE
241600970306     C     VC(32)        OREQ      'S'                                          *VOCE DA CALCOLARE
241700970306     C                   EXSR      CALRCV
241800001227     C                   EXSR      CAL049
241900001227     C                   EXSR      CAL066
242000001227     C                   EXSR      CAL061
242100001227     C                   EXSR      CAL078
242200970306E   2C                   ENDIF
242300970306E   1C                   ENDIF
242400970306     C*
242500970306     C* CALCOLA CONSEGNA PARTICOLARE SU APPUNTAMENTO
242600970306IF  1C     WERR          IFEQ      'N'
242700970306IF  2C     VC(11)        IFEQ      'S'                                          *VOCE DA CALCOLARE
242800970306     C                   EXSR      CALCPA
242900001227     C                   EXSR      CAL050
243000001227     C                   EXSR      CAL067
243100970306E   2C                   ENDIF
243200970306E   1C                   ENDIF
243300970306     C*
243400970306     C* CALCOLA CONSEGNA PARTICOLARE AI SUPERMERCATI
243500970306IF  1C     WERR          IFEQ      'N'
243600970306IF  2C     VC(12)        IFEQ      'S'                                          *VOCE DA CALCOLARE
243700970306     C                   EXSR      CALCPS
243800001227     C                   EXSR      CAL051
243900001227     C                   EXSR      CAL068
244000970306E   2C                   ENDIF
244100970306E   1C                   ENDIF
244200970306     C*
244300970306     C* CALCOLA CONSEGNA PARTICOLARE AI PIANI
244400970306IF  1C     WERR          IFEQ      'N'
244500970306IF  2C     VC(13)        IFEQ      'S'                                          *VOCE DA CALCOLARE
244600970306     C                   EXSR      CALCPP
244700001227     C                   EXSR      CAL052
244800001227     C                   EXSR      CAL069
244900970306E   2C                   ENDIF
245000970306E   1C                   ENDIF
245100040826     C*
245200040826     C* CALCOLA CONSEGNA destinatario disagiato
245300040826IF  1C     WERR          IFEQ      'N'
245400040826IF  2C     VC(96)        IFEQ      'S'                                          *VOCE DA CALCOLARE
245500040826     C                   EXSR      CALdes
245600040826     C                   EXSR      CAL097
245700040826     C                   EXSR      CAL098
245800040826E   2C                   ENDIF
245900040826E   1C                   ENDIF
246000970306     C*
246100970306     C* CALCOLA CONSEGNA PARTICOLARE FUORI MISURA
246200970306IF  1C     WERR          IFEQ      'N'
246300970306IF  2C     VC(14)        IFEQ      'S'                                          *VOCE DA CALCOLARE
246400970306     C                   EXSR      CALCPF
246500001227     C                   EXSR      CAL053
246600001227     C                   EXSR      CAL070
246700970306E   2C                   ENDIF
246800970306E   1C                   ENDIF
246900970414     C*
247000970414     C* CALCOLA CONSEGNA PARTICOLARE DDT
247100970414IF  1C     WERR          IFEQ      'N'
247200970414IF  2C     VC(33)        IFEQ      'S'                                          *VOCE DA CALCOLARE
247300970414     C                   EXSR      CALCPD
247400001227     C                   EXSR      CAL062
247500001227     C                   EXSR      CAL079
247600970414E   2C                   ENDIF
247700970414E   1C                   ENDIF
247800980702     C*
247900980702     C* CALCOLA CONSEGNA PARTICOLARE RITIRO
248000980702IF  1C     WERR          IFEQ      'N'
248100980702IF  2C     VC(17)        IFEQ      'S'                                          *VOCE DA CALCOLARE
248200980702     C                   EXSR      CALRIT
248300001227     C                   EXSR      CAL056
248400001227     C                   EXSR      CAL073
248500980702E   2C                   ENDIF
248600980702E   1C                   ENDIF
248700980702     C*
248800980702     C* CALCOLA LA PROVVIGIONE ASSEGNO MITTENTE
248900980702IF  1C     WERR          IFEQ      'N'
249000980702IF  2C     VC(15)        IFEQ      'S'                                          *VOCE DA CALCOLARE
249100980702     C                   EXSR      CALPAM
249200001227     C                   EXSR      CAL054
249300001227     C                   EXSR      CAL071
249400980702E   2C                   ENDIF
249500980702E   1C                   ENDIF
249600980702     C*
249700980702     C* CALCOLA LA PROVVIGIONE ASSEGNO BARTOLINI
249800980702IF  1C     WERR          IFEQ      'N'
249900980702IF  2C     VC(16)        IFEQ      'S'                                          *VOCE DA CALCOLARE
250000980702     C                   EXSR      CALPAB
250100001227     C                   EXSR      CAL055
250200001227     C                   EXSR      CAL072
250300980702E   2C                   ENDIF
250400980702E   1C                   ENDIF
250500980702     C*
250600980702     C* CALCOLA IL DIRITTO FISSO
250700980702IF  1C     WERR          IFEQ      'N'
250800980702IF  2C     VC(2)         IFEQ      'S'                                          *VOCE DA CALCOLARE
250900980702     C                   EXSR      CALDIF
251000980702E   2C                   ENDIF
251100980702E   1C                   ENDIF
251200980702     C*
251300980702     C* CALCOLA A.D.G. SUL DIRITTO FISSO
251400980702IF  1C     WERR          IFEQ      'N'
251500980702IF  2C     VC(22)        IFEQ      'S'                                          *VOCE DA CALCOLARE
251600980702     C                   EXSR      CALAGD
251700980702E   2C                   ENDIF
251800980702E   1C                   ENDIF
251900980702     C*
252000980702     C* CALCOLA ISTAT SUL DIRITTO FISSO
252100980702IF  1C     WERR          IFEQ      'N'
252200980702IF  2C     VC(25)        IFEQ      'S'                                          *VOCE DA CALCOLARE
252300980702     C                   EXSR      CALISD
252400980702E   2C                   ENDIF
252500980702E   1C                   ENDIF
252600980702     C*
252700980702     C* CALCOLA ISTAT SULL'A.D.G. DEL DIRITTO FISSO
252800980702IF  1C     WERR          IFEQ      'N'
252900980702IF  2C     VC(28)        IFEQ      'S'                                          *VOCE DA CALCOLARE
253000980702     C                   EXSR      CALIAD
253100980702E   2C                   ENDIF
253200980702E   1C                   ENDIF
253300990819     C*
253400990819     C* CALCOLA T.C.P.
253500990819IF  1C     WERR          IFEQ      'N'
253600990819IF  2C     VC(5)         IFEQ      'S'                                          *VOCE DA CALCOLARE
253700990819     C                   EXSR      CALTCP
253800001222     C                   EXSR      CAL046
253900001227     C                   EXSR      CAL063
254000990819E   2C                   ENDIF
254100990819E   1C                   ENDIF
254200040826     C*
254300040826     C* CALCOLA bancali
254400040826IF  1C     WERR          IFEQ      'N'
254500040826IF  2C     VC(99)        IFEQ      'S'                                          *VOCE DA CALCOLARE
254600040826     C                   EXSR      CALban
254700040826     C                   EXSR      CAL100
254800040826     C                   EXSR      CAL101
254900040826E   2C                   ENDIF
255000040826E   1C                   ENDIF
255100040826     C*
255200040826     C* diritto di pesatura
255300040826IF  1C     WERR          IFEQ      'N'
255400040826IF  2C     §tatap        IFEQ      'S'                                          *VOCE DA CALCOLARE
255500040826     C                   EXSR      CALdps
255600040826     C                   EXSR      CAL103
255700040826     C                   EXSR      CAL104
255800040826E   2C                   ENDIF
255900040826E   1C                   ENDIF
256000970306     C*
256100970306     C                   ENDSR
256200980707     C*------------------------------------------------------------------------*
256300091023     C* calimpesp - riproporziona l'importo dell'espresso in base
256400091023     c* ai valori delle voci originali
256500980707     C*------------------------------------------------------------------------*
256600091023     C     calimpesp     BEGSR
256700091026     c* ripartisco solo se applicata tpttpg = 'T'
256800091026     c                   if        tpg ='T'
256900091023     c* porto
257000091023     c                   if        vimp(1) <> 0
257100091023     c     tasesp        mult      vimp(1)       vimp(7)
257200091023     c                   div       impon         vimp(7)
257300091023     c                   else
257400091023     c                   clear                   vimp(7)
257500091023     c                   end
257600091023     c* inoltro
257700091023     c                   if        vimp(6) <> 0
257800091023     c     tasesp        mult      vimp(6)       vimp(36)
257900091023     c                   div       impon         vimp(36)
258000091023     c                   else
258100091023     c                   clear                   vimp(36)
258200091023     c                   end
258300091023     c* disagiate
258400091023     c                   if        vimp(35) <> 0
258500091023     c     tasesp        mult      vimp(35)      vimp(39)
258600091023     c                   div       impon         vimp(39)
258700091023     c                   else
258800091023     c                   clear                   vimp(39)
258900091023     c                   end
259000091023     c* consegna isole
259100091023     c                   if        vimp(80) <> 0
259200091023     c     tasesp        mult      vimp(80)      vimp(81)
259300091023     c                   div       impon         vimp(81)
259400091023     c                   else
259500091023     c                   clear                   vimp(81)
259600091023     c                   end
259700091023     c* ritiro isole
259800091023     c                   if        vimp(88) <> 0
259900091023     c     tasesp        mult      vimp(88)      vimp(89)
260000091023     c                   div       impon         vimp(89)
260100091023     c                   else
260200091023     c                   clear                   vimp(89)
260300091023     c                   end
260400091026     c*
260500091026     c                   else
260600091026     c                   eval      vimp(7) = tasesp
260700091026     c                   clear                   vimp(89)
260800091026     c                   clear                   vimp(81)
260900091026     c                   clear                   vimp(39)
261000091026     c                   clear                   vimp(36)
261100091026     c                   end
261200091023     c*
261300091023     C                   ENDSR
261400091023     C*------------------------------------------------------------------------*
261500091023     C* CALPEI - CALCOLA PORTO E INOLTRO
261600091023     C*------------------------------------------------------------------------*
261700091023     C     CALPEI        BEGSR
261800980707     C*
261900011031     C                   Z-ADD     *ZEROS        TASPOR           21 8
262000011031     C                   Z-ADD     *ZEROS        TASINL           21 8
262100980707     C*****
262200980707     C** CALCOLO    P O R T O     ED    I N O L T R O
262300980707     C*****
262400980707     C                   DO        11            A
262500980707     C     A             OCCUR     TARDS
262600980707     C     TSCG          IFGE      PESTAS
262700980707     C                   GOTO      ENDTAS
262800980707     C                   END
262900980707     C                   END
263000980707     C** NON TROVATO SCAGLIONE ESATTO
263100980707     C     TSCG          IFLT      PESTAS
263200980707     C                   MOVEL     MSG(25)       $MSG
263300980707     C                   EXSR      IMPERR
263400980707     C                   GOTO      FINE
263500980707     C                   END
263600980707     C     ENDTAS        TAG
263700980707     C** MI SALVO LA POSIZIONE DELLO SCAGLIO TROVATO CHE MI
263800980707     C*  SERVE PER IL CALCOLO TARIFFA CON GARANTITO MAX SCAGLIONE PREC
263900980707     C     *LIKE         DEFINE    A             WA
264000980707     C                   Z-ADD     A             WA
264100980707     C**
264200980707     C* CALCOLO PER QUALE VALORE MOLTIPLICARE LA TARIFFA
264300980707     C                   Z-ADD     PESTAS        WPTAS3
264400980707     C                   EXSR      CALQLI
264500980707     C**
264600980707     C* MODO APPLICAZIONE TARIFFA
264700980707     C* CALCOLO NORMALE ANCHE PER SCALARE AL DI SOTTO
264800980707     C*  DELL'APPLICAZIONE TARIFFA FINITA E PER MAX SCAGLIONE PREC
264900980707     C     §TAF02        IFNE      'S'
265000980707     C     §TAF02        OREQ      'S'
265100980707     C     WATA          ANDNE     ' '
265200980707     C** MOLTIPLICAZIONE TARIFFA
265300980707     C                   EXSR      MULTAR
265400980707     C* APPLICAZIONE MINIMO E MASSIMO
265500980707     C                   EXSR      MINMAX
265600980707     C                   Z-ADD     WPOR          TASPOR
265700980707     C                   Z-ADD     WINL          TASINL
265800980707     C                   ENDIF
265900980707     C**
266000980707     C** S C A L A R E
266100980707     C     §TAF02        IFEQ      'S'
266200980707     C                   EXSR      SCALAR
266300980707     C                   ENDIF
266400980707     C**
266500980707     C** M A X   S C A G L I O N E   P R E C E D E N TE
266600980707     C     §TAF02        IFEQ      'G'
266700980707     C                   EXSR      MAXSCA
266800980707     C                   ENDIF
266900980707     C* PORTO
267000980707IF  1C     VC(1)         IFEQ      'S'                                          *VOCE DA CALCOLARE
267100980707IF  2C     TASPOR        IFGT      *ZEROS
267200980707     C                   Z-ADD     1             I
267300980707     C                   Z-ADD     TASPOR        VIMP(I)
267400980707E   2C                   ENDIF
267500980707E   1C                   ENDIF
267600980707     C* INOLTRO
267700980707IF  1C     VC(6)         IFEQ      'S'                                          *VOCE DA CALCOLARE
267800980707IF  2C     TASINL        IFGT      *ZEROS
267900980707     C                   Z-ADD     6             I
268000980707     C                   Z-ADD     TASINL        VIMP(I)
268100980707E   2C                   ENDIF
268200980707E   1C                   ENDIF
268300980707     C**
268400980707     C     FINE          TAG
268500980707     C                   ENDSR
268600980707     C*****************************************************************
268700980707     C** VALORE COL QUALE  MOLTIPLICARE LA TARIFFA
268800980707     C*****************************************************************
268900980707     C     CALQLI        BEGSR
269000980707     C                   Z-ADD     0             PESQLI           11 3
269100980707     C** SE LO SCAGLIONE SCELTO PER TASSARE E' INFERIORE AL MINIMO
269200980707     C** TASSABILE DA TARIFFA FORZO PESO TASSABILE = MINTAS
269300980707     C**
269400980707     C* WATA MI IDENTIFICA SE L'APPLICAZIONE TARIFFA FINITA E' DOVUTA
269500980707     C*  ALLO SCAGLIONE CONFRONTATO CON MINTAR (R)
269600980707     C*  O SE E' DOVUTA DAL PESO TASSABILE CONFRONTATO COL MINIMO
269700980707     C*  TASSABILE DI QUELLA TARIFFA (S) - differenza che ora non serve
269800980707     C*  + ma dal momento che c'e' la lascio
269900980707     C                   CLEAR                   WATA
270000980707    1C                   SELECT
270100980707     C     TSCG          WHENLE    MINTAR
270200980707     C                   Z-ADD     1             PESQLI
270300980707     C                   MOVEL     'R'           WATA              1
270400980707     C**
270500980707     C** SE PESO TASSABILE E' ANCORA INFERIORE
270600980707     C*  AL MINIMO TASSABILE DA TARIFFA IMPOSTO PESO TASS = 1
270700980707     C*  NON LO CONTROLLO PER LA TARIFFA A SCALARE PERCHE' ABBIAMO
270800980707     C*  SEMPRE INSERITO UNO SCAGLIONE=A MINIMO TASSABILE E
270900980707     C*  QUINDI E' GIA' CONTROLLATO NEL WHLE PRECEDENTE
271000980707     C     WPTAS3        WHENLE    MINTAS
271100980707     C     §TAF02        ANDNE     'S'
271200980707     C                   Z-ADD     1             PESQLI
271300980707     C                   MOVEL     'S'           WATA
271400980707   X1C                   OTHER
271500980707     C** CALCOLO : PESO IN Q.LI PER TARIFFA A PESO
271600980707     C** CALCOLO   VALORE/100   PER TARIFFA A VALORE (ESSENDO UNA %)
271700980707     C** PER LE ALTRE TARIFFE PESO TASSABILE INVARIATO
271800980707    2C     UNOCTR        IFEQ      0
271900980707     C     UNOCTR        OREQ      4
272000980707     C     WPTAS3        DIV       100           PESQLI
272100980707   X2C                   ELSE
272200980707     C                   Z-ADD     WPTAS3        PESQLI
272300980707    2C                   END
272400980707    1C                   ENDSL
272500980707     C                   ENDSR
272600980707     C*****************************************************************
272700980707     C* MOLTIPLICATO TARIFFA PER VALORE TROVATO (PESO TASSABILE)
272800980707     C*****************************************************************
272900980707     C     MULTAR        BEGSR
273000980707     C     *LIKE         DEFINE    TASPOR        WPOR
273100980707     C     *LIKE         DEFINE    TASINL        WINL
273200980707     C                   CLEAR                   WPOR
273300980707     C                   CLEAR                   WINL
273400980707     C**
273500980707     C     TITR          MULT(H)   PESQLI        WPOR                           ** PORTO
273600980707     C     TINO          MULT(H)   PESQLI        WINL                           ** INOLTRO
273700980707     C                   ENDSR
273800980707     C*****************************************************************
273900980707     C* CALCOLO TARIFFA A SCALARE
274000980707     C*****************************************************************
274100980707     C     SCALAR        BEGSR
274200980707     C**
274300980707     C     *LIKE         DEFINE    PESTAS        WPTAS1
274400980707     C     *LIKE         DEFINE    PESTAS        WDIFSC
274500980707     C     *LIKE         DEFINE    PESTAS        WPTAS3
274600980707     C     *LIKE         DEFINE    PESQLI        WPESQ
274700980707     C     *LIKE         DEFINE    PESQLI        WPESSV
274800980707     C     *LIKE         DEFINE    TITR          WTITR
274900980707     C     *LIKE         DEFINE    TITR          WSTITR
275000980707     C     *LIKE         DEFINE    TINO          WTINO
275100980707     C     *LIKE         DEFINE    TINO          WSTINO
275200980707     C     *LIKE         DEFINE    TSCG          WPRESC
275300980707     C**
275400980707     C                   CLEAR                   TASPOR
275500980707     C                   CLEAR                   TASINL
275600980707     C                   CLEAR                   WTROVA
275700980707     C                   CLEAR                   WPTAS3
275800980707     C                   CLEAR                   WDIFSC
275900980707     C                   CLEAR                   WPRESC
276000980707     C                   CLEAR                   WPESSV
276100980707     C                   CLEAR                   WPESQ
276200980707     C                   CLEAR                   WTITR
276300980707     C                   CLEAR                   WSTITR
276400980707     C                   CLEAR                   WTINO
276500980707     C                   CLEAR                   WSTINO
276600980707     C                   Z-ADD     PESTAS        WPTAS1
276700980707     C                   Z-ADD     1             A
276800980707     C**
276900980805    1C     A             DOWLE     11
277000980707     C     WTROVA        ANDEQ     ' '
277100980707     C     TSCG          ANDGT     0
277200980707     C     A             OCCUR     TARDS
277300980707     C**
277400980707     C* FACCIO LA DIFFERENZA TRA LO SCAGLIONE LETTO ED IL PRECEDENTE
277500980707     C     TSCG          SUB       WPRESC        WDIFSC
277600980707     C**
277700980707     C* SOTRAGGO DAL PESO TASSABILE RIMASTO LA PARTE DA TASSARE ORA
277800980707     C* IN WPTAS1 HO SEMPRE LA PARTE DI PESO TASSABILE ANCORA DA TASSAR
277900980707     C                   SUB       WDIFSC        WPTAS1
278000980707     C**
278100980707     C** SE IL VALORE E' DIVENTATO < DI 0 SIGNIFICA CHE HO TERMINATO LA
278200980707     C**  TASSAZIONE PER CUI USO SOLO LA PARTE RIMASTA DI WPTAS1
278300980707    2C     WPTAS1        IFLE      0
278400980707     C     WDIFSC        ADD       WPTAS1        WPTAS3
278500980707     C* PESO TASSABILE GLOBALE DELLA SPEDIZIONE
278600980707     C                   MOVEL     'S'           WTROVA            1
278700980707     C                   ELSE
278800980707     C**
278900980707     C* PARTE DI PESO DA TASSARE CON QUESTO SCAGLIONE
279000980707     C                   Z-ADD     WDIFSC        WPTAS3
279100980707     C* SALVO LO SCAGLIONE PRECEDENTE
279200980707     C                   Z-ADD     TSCG          WPRESC
279300980707    2C                   ENDIF
279400980707     C**
279500980707     C* IMPOSTO IL VALORE DA MOLTIPLICARE PER LA TARIFFA
279600980707     C                   EXSR      CALQLI
279700980707     C**
279800980707     C* SE LO SCAGLIONE E' APPLICAZIONE TARIFFA FINITA DEVO CONSIDERARE
279900980707     C*  SOLO L'ULTIMO SCAGLIONE COSI' E NON I PRECEDENTI
280000980707    2C     WATA          IFNE      ' '
280100980707     C     WTROVA        ANDEQ     ' '
280200980707     C* MI SALVO IL VALORE MA PER ORA NON LO MOLTIPLICO
280300980707     C                   Z-ADD     PESQLI        WPESQ
280400980707     C                   Z-ADD     TITR          WTITR
280500980707     C                   Z-ADD     TINO          WTINO
280600980707   X2C                   ELSE
280700980707     C**
280800980707     C* SE LO SCAGLIONE TROVATO E' L'ULTIMO MA COMUNQUE TARIFFA FINITA
280900980707     C* DEVO MOLTIPLICARE SOLO QUESTO --> AZZERO UN EVENTUALE PRECEDENT
281000980707     C*  CHE AVEVA LA TASSAZIONE TARIFFA FINITA
281100980707    3C     WTROVA        IFEQ      'S'
281200980707     C     WATA          ANDNE     ' '
281300980707     C                   CLEAR                   WPESQ
281400980707     C                   CLEAR                   WTITR
281500980707     C                   CLEAR                   WTINO
281600980707    3C                   ENDIF
281700980707     C*
281800980707     C* PRIMA DI MOLTIPLICARE LA TARIFFA TROVATA VEDO SE CE NE E'
281900980707     C*  UNA PRECEDENTE TARIFFA FINITA DA CONSIDERARE
282000980707    3C     WPESQ         IFGT      0
282100980707     C* SALVO PESO DA MOLTIPLICARE E TARIFFE DELLO SCAGLIONE
282200980707     C*  APPENA LETTO
282300980707     C                   Z-ADD     PESQLI        WPESSV
282400980707     C                   Z-ADD     TITR          WSTITR
282500980707     C                   Z-ADD     TINO          WSTINO
282600980707     C* IMPOSTO PESO DA MOLTIPLICARE E TARIFFE DELLO SCAGLIONE
282700980707     C*  PRECEDENTE PER IL CALCOLO
282800980707     C                   Z-ADD     WPESQ         PESQLI
282900980707     C                   Z-ADD     WTITR         TITR
283000980707     C                   Z-ADD     WTINO         TINO
283100980707     C                   EXSR      MULTAR
283200980707     C                   ADD       WPOR          TASPOR
283300980707     C                   ADD       WINL          TASINL
283400980707     C                   CLEAR                   WPESQ
283500980707     C                   CLEAR                   WTITR
283600980707     C                   CLEAR                   WTINO
283700980707     C* REIMPOSTO I DATI DELLO SCAGLIONE APPENA LETTO
283800980707     C                   Z-ADD     WPESSV        PESQLI
283900980707     C                   Z-ADD     WSTITR        TITR
284000980707     C                   Z-ADD     WSTINO        TINO
284100980707    3C                   ENDIF
284200980707     C**
284300980707     C* MOLTIPLICAZIONE TARIFFA
284400980707     C                   EXSR      MULTAR
284500980707     C                   ADD       WPOR          TASPOR
284600980707     C                   ADD       WINL          TASINL
284700980707    2C                   ENDIF
284800980707     C                   ADD       1             A
284900980707     C**
285000980707    1C                   ENDDO
285100980707     C**
285200980707     C* APPLICO MINIMO E MASSIMO
285300980707     C                   Z-ADD     TASPOR        WPOR
285400980707     C                   Z-ADD     TASINL        WINL
285500980707     C                   EXSR      MINMAX
285600980707     C                   Z-ADD     WPOR          TASPOR
285700980707     C                   Z-ADD     WINL          TASINL
285800980707     C                   ENDSR
285900980707     C*****************************************************************
286000980707     C* CALCOLO MAX SCAGLIONE PRECEDENTE
286100980707     C*****************************************************************
286200980707     C     MAXSCA        BEGSR
286300980707     C* SOLO SE LO SCAGLIONE APPLICATO NON ERA IL PRIMO
286400980707     C     WA            IFGT      1
286500980707     C* PER CONFRONTARLO CON IL VALORE NORMALE CALCOLATO
286600980707     C* IL CONFRONTO E' DATO DALLA SOMMA TRA PORT E INOLTRO
286700980707     C                   SUB       1             WA
286800980707     C     WA            OCCUR     TARDS
286900980707     C* CALCOLO PESO DA MOLTIPLICARE
287000980707     C                   Z-ADD     TSCG          WPTAS3
287100980707     C                   EXSR      CALQLI
287200980707     C** MOLTIPLICO LA TARIFFA
287300980707     C                   EXSR      MULTAR
287400980707     C** MINIMO E MASSIMO
287500980707     C                   EXSR      MINMAX
287600980707     C**
287700011018     C     TASPOR        ADD       TASINL        T0100            18 5
287800011018     C     WPOR          ADD       WINL          W0100            18 5
287900980707     C** PRENDO DELLE 2 LA TARIFFA PIU' ALTA
288000980707     C     W0100         IFGT      T0100
288100980707     C                   Z-ADD     WPOR          TASPOR
288200980707     C                   Z-ADD     WINL          TASINL
288300980707     C                   ENDIF
288400980707     C                   ENDIF
288500980707     C**
288600980707     C                   ENDSR
288700980707     C*****************************************************************
288800980707     C** MINIMO E MASSIMO DA APPLICARE -SE APPLICATO TUTTO NEL PORTO
288900980707     C*****************************************************************
289000980707     C     MINMAX        BEGSR
289100980707     C** NON ESISTE MINIMO E MASSIMO
289200980707     C     TMIN          IFEQ      0
289300980707     C     TMAX          ANDEQ     0
289400980707     C                   GOTO      DOPOMM
289500980707     C                   END
289600980707     C** MINIMO
289700011018     C     WPOR          ADD       WINL          WMAX             18 5
289800980707     C     TMIN          IFGT      WMAX
289900980707     C                   Z-ADD     TMIN          WPOR
290000980707     C                   Z-ADD     0             WINL
290100980707     C                   Z-ADD     TMIN          WMAX
290200980707     C                   END
290300980707     C** MASSIMO
290400980707     C     TMAX          IFGT      0
290500980707     C     WMAX          IFGT      TMAX
290600980707     C                   Z-ADD     TMAX          WPOR
290700980707     C                   Z-ADD     0             WINL
290800980707     C                   END
290900980707     C                   END
291000980707     C     DOPOMM        ENDSR
291100980706     C*------------------------------------------------------------------------*
291200980706     C* CALAGP - CALCOLA L'A.D.G. SUL PORTO
291300980706     C*------------------------------------------------------------------------*
291400980706     C     CALAGP        BEGSR
291500980706     C*
291600980706     C* REIMPOSTA LE VARIABILI DI LAVORO
291700011031     C                   Z-ADD     *ZEROS        TASADG           21 8
291800980706     C*
291900980706     C* CALCOLA L'A.D.G.
292000011031     C                   Z-ADD     TASPOR        WIMP             23 8
292100980706     C                   EXSR      ADGCAL
292200980706     C*
292300980706IF  1C     TASADG        IFGT      *ZEROS
292400980706     C                   Z-ADD     3             I
292500980706     C                   Z-ADD     TASADG        VIMP(I)
292600980706E   1C                   ENDIF
292700980706     C*
292800980706     C                   ENDSR
292900980706     C*------------------------------------------------------------------------*
293000980706     C* CALISP - CALCOLA L'ISTAT SUL PORTO
293100980706     C*------------------------------------------------------------------------*
293200980706     C     CALISP        BEGSR
293300980706     C*
293400980706     C* REIMPOSTA LE VARIABILI DI LAVORO
293500011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
293600980706     C*
293700980706     C* CALCOLA L'ISTAT
293800011031     C                   Z-ADD     TASPOR        WIMP             23 8
293900980706     C                   EXSR      IACCAL
294000980706     C*
294100980706IF  1C     TASIAC        IFGT      *ZEROS
294200980706     C                   Z-ADD     4             I
294300980706     C                   Z-ADD     TASIAC        VIMP(I)
294400980706E   1C                   ENDIF
294500980706     C*
294600980706     C                   ENDSR
294700980706     C*------------------------------------------------------------------------*
294800980706     C* CALIAP - CALCOLA L'ISTAT SULL'ADG DEL PORTO
294900980706     C*------------------------------------------------------------------------*
295000980706     C     CALIAP        BEGSR
295100980706     C*
295200980706     C* REIMPOSTA LE VARIABILI DI LAVORO
295300011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
295400980706     C*
295500980706     C* CALCOLA L'ISTAT
295600011031     C                   Z-ADD     VIMP(3)       WIMP             23 8
295700980706     C                   EXSR      IACCAL
295800980706     C*
295900980706IF  1C     TASIAC        IFGT      *ZEROS
296000980706     C                   Z-ADD     27            I
296100980706     C                   Z-ADD     TASIAC        VIMP(I)
296200980706E   1C                   ENDIF
296300980706     C*
296400980706     C                   ENDSR
296500970306     C*------------------------------------------------------------------------*
296600970717     C* CALESP - CALCOLA L'ESPRESSO PORTO
296700970306     C*------------------------------------------------------------------------*
296800970306     C     CALESP        BEGSR
296900970306     C*
297000970306     C* REIMPOSTA LE VARIABILI DI LAVORO
297100011031     C                   Z-ADD     *ZEROS        TASESP           21 8
297200970306     C*
297300091023     C* CALCOLA L'ESPRESSO sul porto + inoltro + disagiate + ritiro
297400091023     c* isola + consegna isola
297500091023     C*                  Z-ADD     TASPOR        IMPON            23 8
297600091023     c                   eval      impon = vimp(1)+ vimp(6)+
297700091023     c                             vimp(35) + vimp(80) + vimp(88)
297800091023     c*
297900970306     C                   EXSR      CALTS
298000970306     C*
298100970314IF  1C     TASESP        IFGT      *ZEROS
298200091023     C*                  Z-ADD     7             I
298300091023     C*                  Z-ADD     TASESP        VIMP(I)
298400970314E   1C                   ENDIF
298500970306     C*
298600970306     C                   ENDSR
298700980706     C*------------------------------------------------------------------------*
298800980706     C* CALAEP - CALCOLA L'A.D.G. SULL'ESPRESSO PORTO
298900980706     C*------------------------------------------------------------------------*
299000980706     C     CALAEP        BEGSR
299100980706     C*
299200980706     C* REIMPOSTA LE VARIABILI DI LAVORO
299300011031     C                   Z-ADD     *ZEROS        TASADG           21 8
299400980706     C*
299500980706     C* CALCOLA L'A.D.G.
299600011031     C                   Z-ADD     VIMP(7)       WIMP             23 8
299700980706     C                   EXSR      ADGCAL
299800980706     C*
299900980706IF  1C     TASADG        IFGT      *ZEROS
300000980706     C                   Z-ADD     23            I
300100980706     C                   Z-ADD     TASADG        VIMP(I)
300200980706E   1C                   ENDIF
300300980706     C*
300400980706     C                   ENDSR
300500980706     C*------------------------------------------------------------------------*
300600980706     C* CALIEP - CALCOLA L'ISTAT SULL'ESPRESSO PORTO
300700980706     C*------------------------------------------------------------------------*
300800980706     C     CALIEP        BEGSR
300900980706     C*
301000980706     C* REIMPOSTA LE VARIABILI DI LAVORO
301100011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
301200980706     C*
301300980706     C* CALCOLA L'ISTAT
301400011031     C                   Z-ADD     VIMP(7)       WIMP             23 8
301500980706     C                   EXSR      IACCAL
301600980706     C*
301700980706IF  1C     TASIAC        IFGT      *ZEROS
301800980706     C                   Z-ADD     26            I
301900980706     C                   Z-ADD     TASIAC        VIMP(I)
302000980706E   1C                   ENDIF
302100980706     C*
302200980706     C                   ENDSR
302300980706     C*------------------------------------------------------------------------*
302400980706     C* CAIAEP - CALCOLA L'ISTAT SULL'ADG DELL'ESPRESSO PORTO
302500980706     C*------------------------------------------------------------------------*
302600980706     C     CAIAEP        BEGSR
302700980706     C*
302800980706     C* REIMPOSTA LE VARIABILI DI LAVORO
302900011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
303000980706     C*
303100980706     C* CALCOLA L'ISTAT
303200011031     C                   Z-ADD     VIMP(23)      WIMP             23 8
303300980706     C                   EXSR      IACCAL
303400980706     C*
303500980706IF  1C     TASIAC        IFGT      *ZEROS
303600980706     C                   Z-ADD     30            I
303700980706     C                   Z-ADD     TASIAC        VIMP(I)
303800980706E   1C                   ENDIF
303900980706     C*
304000980706     C                   ENDSR
304100980706     C*------------------------------------------------------------------------*
304200980706     C* CALAGI - CALCOLA L'A.D.G. SULL'INOLTRO
304300980706     C*------------------------------------------------------------------------*
304400980706     C     CALAGI        BEGSR
304500980706     C*
304600980706     C* REIMPOSTA LE VARIABILI DI LAVORO
304700011031     C                   Z-ADD     *ZEROS        TASADG           21 8
304800980706     C*
304900980706     C* CALCOLA L'A.D.G.
305000011031     C                   Z-ADD     TASINL        WIMP             23 8
305100980706     C                   EXSR      ADGCAL
305200980706     C*
305300980706IF  1C     TASADG        IFGT      *ZEROS
305400980706     C                   Z-ADD     21            I
305500980706     C                   Z-ADD     TASADG        VIMP(I)
305600980706E   1C                   ENDIF
305700980706     C*
305800980706     C                   ENDSR
305900980706     C*------------------------------------------------------------------------*
306000980706     C* CALISI - CALCOLA L'ISTAT SULL'INOLTRO
306100980706     C*------------------------------------------------------------------------*
306200980706     C     CALISI        BEGSR
306300980706     C*
306400980706     C* REIMPOSTA LE VARIABILI DI LAVORO
306500011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
306600980706     C*
306700980706     C* CALCOLA L'ISTAT
306800011031     C                   Z-ADD     TASINL        WIMP             23 8
306900980706     C                   EXSR      IACCAL
307000980706     C*
307100980706IF  1C     TASIAC        IFGT      *ZEROS
307200980706     C                   Z-ADD     24            I
307300980706     C                   Z-ADD     TASIAC        VIMP(I)
307400980706E   1C                   ENDIF
307500980706     C*
307600980706     C                   ENDSR
307700980706     C*------------------------------------------------------------------------*
307800980706     C* CALIAI - CALCOLA L'ISTAT SULL'ADG DELL'INOLTRO
307900980706     C*------------------------------------------------------------------------*
308000980706     C     CALIAI        BEGSR
308100980706     C*
308200980706     C* REIMPOSTA LE VARIABILI DI LAVORO
308300011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
308400980706     C*
308500980706     C* CALCOLA L'ISTAT
308600011031     C                   Z-ADD     VIMP(21)      WIMP             23 8
308700980706     C                   EXSR      IACCAL
308800980706     C*
308900980706IF  1C     TASIAC        IFGT      *ZEROS
309000980706     C                   Z-ADD     29            I
309100980706     C                   Z-ADD     TASIAC        VIMP(I)
309200980706E   1C                   ENDIF
309300980706     C*
309400980706     C                   ENDSR
309500970717     C*------------------------------------------------------------------------*
309600970717     C* CALESI - CALCOLA L'ESPRESSO INOLTRO
309700970717     C*------------------------------------------------------------------------*
309800970717     C     CALESI        BEGSR
309900970717     C*
310000970717     C* REIMPOSTA LE VARIABILI DI LAVORO
310100011031     C                   Z-ADD     *ZEROS        TASESP           21 8
310200970717     C*
310300970717     C* CALCOLA L'ESPRESSO
310400011031     C                   Z-ADD     TASINL        IMPON            23 8
310500970717     C                   EXSR      CALTS
310600970717     C*
310700970717IF  1C     TASESP        IFGT      *ZEROS
310800970717     C                   Z-ADD     36            I
310900970717     C                   Z-ADD     TASESP        VIMP(I)
311000970717E   1C                   ENDIF
311100970717     C*
311200970717     C                   ENDSR
311300980706     C*------------------------------------------------------------------------*
311400980706     C* CALAEI - CALCOLA L'A.D.G. SULL'ESPRESSO INOLTRO
311500980706     C*------------------------------------------------------------------------*
311600980706     C     CALAEI        BEGSR
311700980706     C*
311800980706     C* REIMPOSTA LE VARIABILI DI LAVORO
311900011031     C                   Z-ADD     *ZEROS        TASADG           21 8
312000980706     C*
312100980706     C* CALCOLA L'A.D.G.
312200011031     C                   Z-ADD     VIMP(36)      WIMP             23 8
312300980706     C                   EXSR      ADGCAL
312400980706     C*
312500980706IF  1C     TASADG        IFGT      *ZEROS
312600980706     C                   Z-ADD     37            I
312700980706     C                   Z-ADD     TASADG        VIMP(I)
312800980706E   1C                   ENDIF
312900980706     C*
313000980706     C                   ENDSR
313100980706     C*------------------------------------------------------------------------*
313200980706     C* CALIEI - CALCOLA L'ISTAT SULL'ESPRESSO INOLTRO
313300980706     C*------------------------------------------------------------------------*
313400980706     C     CALIEI        BEGSR
313500980706     C*
313600980706     C* REIMPOSTA LE VARIABILI DI LAVORO
313700011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
313800980706     C*
313900980706     C* CALCOLA L'ISTAT
314000011031     C                   Z-ADD     VIMP(36)      WIMP             23 8
314100980706     C                   EXSR      IACCAL
314200980706     C*
314300980706IF  1C     TASIAC        IFGT      *ZEROS
314400980706     C                   Z-ADD     38            I
314500980706     C                   Z-ADD     TASIAC        VIMP(I)
314600980706E   1C                   ENDIF
314700980706     C*
314800980706     C                   ENDSR
314900980706     C*------------------------------------------------------------------------*
315000980706     C* CAIAEI - CALCOLA L'ISTAT SULL'ADG DELL'ESPRESSO INOLTRO
315100980706     C*------------------------------------------------------------------------*
315200980706     C     CAIAEI        BEGSR
315300980706     C*
315400980706     C* REIMPOSTA LE VARIABILI DI LAVORO
315500011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
315600980706     C*
315700980706     C* CALCOLA L'ISTAT
315800011031     C                   Z-ADD     VIMP(37)      WIMP             23 8
315900980706     C                   EXSR      IACCAL
316000980706     C*
316100980706IF  1C     TASIAC        IFGT      *ZEROS
316200980706     C                   Z-ADD     39            I
316300980706     C                   Z-ADD     TASIAC        VIMP(I)
316400980706E   1C                   ENDIF
316500980706     C*
316600980706     C                   ENDSR
316700980706     C*------------------------------------------------------------------------*
316800980706     C* CALCPL - CALCOLA CONSEGNA PARTICOLARE LOCALITA' DISAGIATE
316900980706     C*------------------------------------------------------------------------*
317000980706     C     CALCPL        BEGSR
317100980706     C*
317200980706     C* REIMPOSTA LE VARIABILI DI LAVORO
317300980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
317400011031     C                   Z-ADD     *ZEROS        TASCPL           21 8
317500980706     C*
317600980706     C* CALCOLA CONSEGNA PARTICOLARE
317700980706     C                   MOVEL     'K'           TASTC             1            *CODICE CONSEGNA
317800011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
317900980706     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
318000980706     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
318100980706     C                   EXSR      CALCP
318200980706     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
318300980706     C                   Z-ADD     TASCP         TASCPL
318400980706     C*
318500980706IF  1C     TASCPL        IFGT      *ZEROS
318600980706     C                   Z-ADD     35            I
318700980706     C                   Z-ADD     TASCPL        VIMP(I)
318800980706E   1C                   ENDIF
318900980706     C*
319000980706     C* MEMORIZZA IL FLAG "APPLICAZIONE TARIFFA INOLTRO" DELLO SCAGLIONE DEL
319100980706     C* CODICE TASSAZIONE DEL DETTAGLIO TARIFFARIO
319200980706     C                   MOVEL     DEPAIN        CPLAIN
319300980706     C*
319400980706     C                   ENDSR
319500980706     C*------------------------------------------------------------------------*
319600980706     C* CALAGL - CALCOLA L'A.D.G. SULLE CONSEGNE LOCALITA' DISAGIATE
319700980706     C*------------------------------------------------------------------------*
319800980706     C     CALAGL        BEGSR
319900980706     C*
320000980706     C* REIMPOSTA LE VARIABILI DI LAVORO
320100011031     C                   Z-ADD     *ZEROS        TASADG           21 8
320200980706     C*
320300980706     C* CALCOLA L'A.D.G.
320400011031     C                   Z-ADD     TASCPL        WIMP             23 8
320500980706     C                   EXSR      ADGCAL
320600980706     C*
320700980706IF  1C     TASADG        IFGT      *ZEROS
320800980706     C                   Z-ADD     43            I
320900980706     C                   Z-ADD     TASADG        VIMP(I)
321000980706E   1C                   ENDIF
321100980706     C*
321200980706     C                   ENDSR
321300980706     C*------------------------------------------------------------------------*
321400980706     C* CALISL - CALCOLA L'ISTAT SULLE CONSEGNE LOCALITA' DISAGIATE
321500980706     C*------------------------------------------------------------------------*
321600980706     C     CALISL        BEGSR
321700980706     C*
321800980706     C* REIMPOSTA LE VARIABILI DI LAVORO
321900011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
322000980706     C*
322100980706     C* CALCOLA L'ISTAT
322200011031     C                   Z-ADD     TASCPL        WIMP             23 8
322300980706     C                   EXSR      IACCAL
322400980706     C*
322500980706IF  1C     TASIAC        IFGT      *ZEROS
322600980706     C                   Z-ADD     44            I
322700980706     C                   Z-ADD     TASIAC        VIMP(I)
322800980706E   1C                   ENDIF
322900980706     C*
323000980706     C                   ENDSR
323100980706     C*------------------------------------------------------------------------*
323200980706     C* CALIAL - CALCOLA L'ISTAT SULL'ADG DELLE CONEGNE LOCALITA' DISAGIATE
323300980706     C*------------------------------------------------------------------------*
323400980706     C     CALIAL        BEGSR
323500980706     C*
323600980706     C* REIMPOSTA LE VARIABILI DI LAVORO
323700011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
323800980706     C*
323900980706     C* CALCOLA L'ISTAT
324000011031     C                   Z-ADD     VIMP(43)      WIMP             23 8
324100980706     C                   EXSR      IACCAL
324200980706     C*
324300980706IF  1C     TASIAC        IFGT      *ZEROS
324400980706     C                   Z-ADD     45            I
324500980706     C                   Z-ADD     TASIAC        VIMP(I)
324600980706E   1C                   ENDIF
324700980706     C*
324800980706     C                   ENDSR
324900980706     C*------------------------------------------------------------------------*
325000980706     C* CALESL - CALCOLA L'ESPRESSO SULLA CONSEGNA LOCALITA' DISAGIATE
325100980706     C*------------------------------------------------------------------------*
325200980706     C     CALESL        BEGSR
325300980706     C*
325400980706     C* REIMPOSTA LE VARIABILI DI LAVORO
325500011031     C                   Z-ADD     *ZEROS        TASESP           21 8
325600980706     C*
325700980706     C* CALCOLA L'ESPRESSO
325800011031     C                   Z-ADD     VIMP(35)      IMPON            23 8
325900980706     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
326000980706     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
326100980706     C                   EXSR      CALTS
326200980706     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
326300980706     C*
326400980706IF  1C     TASESP        IFGT      *ZEROS
326500980706     C                   Z-ADD     39            I
326600980706     C                   Z-ADD     TASESP        VIMP(I)
326700980706E   1C                   ENDIF
326800980706     C*
326900980706     C                   ENDSR
327000980706     C*------------------------------------------------------------------------*
327100980706     C* CALAEL - CALCOLA L'A.D.G. SULL'ESPRESSO CONSEGNA LOCALITA' DISAGIATE
327200980706     C*------------------------------------------------------------------------*
327300980706     C     CALAEL        BEGSR
327400980706     C*
327500980706     C* REIMPOSTA LE VARIABILI DI LAVORO
327600011031     C                   Z-ADD     *ZEROS        TASADG           21 8
327700980706     C*
327800980706     C* CALCOLA L'A.D.G.
327900011031     C                   Z-ADD     VIMP(39)      WIMP             23 8
328000980706     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
328100980706     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
328200980706     C                   EXSR      ADGCAL
328300980706     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
328400980706     C*
328500980706IF  1C     TASADG        IFGT      *ZEROS
328600980706     C                   Z-ADD     40            I
328700980706     C                   Z-ADD     TASADG        VIMP(I)
328800980706E   1C                   ENDIF
328900980706     C*
329000980706     C                   ENDSR
329100980706     C*------------------------------------------------------------------------*
329200980706     C* CALIEL - CALCOLA L'ISTAT SULL'ESPRESSO CONSEGNA LOCALITA' DISAGIATE
329300980706     C*------------------------------------------------------------------------*
329400980706     C     CALIEL        BEGSR
329500980706     C*
329600980706     C* REIMPOSTA LE VARIABILI DI LAVORO
329700011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
329800980706     C*
329900980706     C* CALCOLA L'ISTAT
330000011031     C                   Z-ADD     VIMP(39)      WIMP             23 8
330100980706     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
330200980706     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
330300980706     C                   EXSR      IACCAL
330400980706     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
330500980706     C*
330600980706IF  1C     TASIAC        IFGT      *ZEROS
330700980706     C                   Z-ADD     41            I
330800980706     C                   Z-ADD     TASIAC        VIMP(I)
330900980706E   1C                   ENDIF
331000980706     C*
331100980706     C                   ENDSR
331200980706     C*------------------------------------------------------------------------*
331300980706     C* CAIAEL - CALCOLA L'ISTAT SULL'ADG DELL'ESPRESSO CONSEGNA L.DISAGIATE
331400980706     C*------------------------------------------------------------------------*
331500980706     C     CAIAEL        BEGSR
331600980706     C*
331700980706     C* REIMPOSTA LE VARIABILI DI LAVORO
331800011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
331900980706     C*
332000980706     C* CALCOLA L'ISTAT
332100011031     C                   Z-ADD     VIMP(40)      WIMP             23 8
332200980706     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
332300980706     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
332400980706     C                   EXSR      IACCAL
332500980706     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
332600980706     C*
332700980706IF  1C     TASIAC        IFGT      *ZEROS
332800980706     C                   Z-ADD     42            I
332900980706     C                   Z-ADD     TASIAC        VIMP(I)
333000980706E   1C                   ENDIF
333100980706     C*
333200980706     C                   ENDSR
333300011031     C*------------------------------------------------------------------------*
333400011031     C* CALSAM - CALCOLA RITIRO ISOLA
333500011031     C*------------------------------------------------------------------------*
333600011031     C     CALSAM        BEGSR
333700011031     C*
333800011031     C* REIMPOSTA LE VARIABILI DI LAVORO
333900011031     C                   MOVEL     'S'           WCAR                           *SI UTILIZZO TAR.CAR
334000011031     C                   Z-ADD     *ZEROS        TASSAM           21 8
334100011031     C*
334200011031     C* CALCOLA RITIRO COME CONSEGNA PARTICOLARE ISOLA
334300011031     C                   MOVEL     'I'           TASTC             1            *CODICE CONSEGNA
334400011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
334500011031     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
334600011031     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
334700011031     C                   EXSR      CALCP
334800011031     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
334900050103     C                   Z-ADD     TASCP         TASSAM
335000011031     C*
335100011031IF  1C     TASSAM        IFGT      *ZEROS
335200011031     C                   Z-ADD     88            I
335300011031     C                   Z-ADD     TASSAM        VIMP(I)
335400011031E   1C                   ENDIF
335500011031     C*
335600011031     C* MEMORIZZA IL FLAG "APPLICAZIONE TARIFFA INOLTRO" DELLO SCAGLIONE DEL
335700011031     C* CODICE TASSAZIONE DEL DETTAGLIO TARIFFARIO -NON SERVE, MA TENIAMOLO-
335800011031     C                   MOVEL     DEPAIN        SAMAIN
335900011031     C*
336000011031     C                   ENDSR
336100011031     C*------------------------------------------------------------------------*
336200011031     C* CALAGR - CALCOLA L'A.D.G. SUL RITIRO ISOLA
336300011031     C*------------------------------------------------------------------------*
336400011031     C     CALAGR        BEGSR
336500011031     C*
336600011031     C* REIMPOSTA LE VARIABILI DI LAVORO
336700011031     C                   Z-ADD     *ZEROS        TASADG           21 8
336800011031     C*
336900011031     C* CALCOLA L'A.D.G.
337000011031     C                   Z-ADD     TASSAM        WIMP             23 8
337100011031     C                   EXSR      ADGCAL
337200011031     C*
337300011031IF  1C     TASADG        IFGT      *ZEROS
337400011031     C                   Z-ADD     93            I
337500011031     C                   Z-ADD     TASADG        VIMP(I)
337600011031E   1C                   ENDIF
337700011031     C*
337800011031     C                   ENDSR
337900011031     C*------------------------------------------------------------------------*
338000011031     C* CALISR - CALCOLA L'ISTAT SUL RITIRO ISOLA
338100011031     C*------------------------------------------------------------------------*
338200011031     C     CALISR        BEGSR
338300011031     C*
338400011031     C* REIMPOSTA LE VARIABILI DI LAVORO
338500011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
338600011031     C*
338700011031     C* CALCOLA L'ISTAT
338800011031     C                   Z-ADD     TASSAM        WIMP             23 8
338900011031     C                   EXSR      IACCAL
339000011031     C*
339100011031IF  1C     TASIAC        IFGT      *ZEROS
339200011031     C                   Z-ADD     94            I
339300011031     C                   Z-ADD     TASIAC        VIMP(I)
339400011031E   1C                   ENDIF
339500011031     C*
339600011031     C                   ENDSR
339700011031     C*------------------------------------------------------------------------*
339800011031     C* CALIAR - CALCOLA L'ISTAT SULL'ADG DEL RITIRO ISOLA
339900011031     C*------------------------------------------------------------------------*
340000011031     C     CALIAR        BEGSR
340100011031     C*
340200011031     C* REIMPOSTA LE VARIABILI DI LAVORO
340300011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
340400011031     C*
340500011031     C* CALCOLA L'ISTAT
340600011031     C                   Z-ADD     VIMP(93)      WIMP             23 8
340700011031     C                   EXSR      IACCAL
340800011031     C*
340900011031IF  1C     TASIAC        IFGT      *ZEROS
341000011031     C                   Z-ADD     95            I
341100011031     C                   Z-ADD     TASIAC        VIMP(I)
341200011031E   1C                   ENDIF
341300011031     C*
341400011031     C                   ENDSR
341500011031     C*------------------------------------------------------------------------*
341600011031     C* CALESR - CALCOLA L'ESPRESSO SUL RITIRO ISOLA
341700011031     C*------------------------------------------------------------------------*
341800011031     C     CALESR        BEGSR
341900011031     C*
342000011031     C* REIMPOSTA LE VARIABILI DI LAVORO
342100011031     C                   Z-ADD     *ZEROS        TASESP           21 8
342200011031     C*
342300011031     C* CALCOLA L'ESPRESSO
342400011031     C                   Z-ADD     VIMP(88)      IMPON            23 8
342500011031     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
342600011031     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
342700011031     C                   EXSR      CALTS
342800011031     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
342900011031     C*
343000011031IF  1C     TASESP        IFGT      *ZEROS
343100011031     C                   Z-ADD     89            I
343200011031     C                   Z-ADD     TASESP        VIMP(I)
343300011031E   1C                   ENDIF
343400011031     C*
343500011031     C                   ENDSR
343600011031     C*------------------------------------------------------------------------*
343700011031     C* CALAER - CALCOLA L'A.D.G. SULL'ESPRESSO RITIRO ISOLA
343800011031     C*------------------------------------------------------------------------*
343900011031     C     CALAER        BEGSR
344000011031     C*
344100011031     C* REIMPOSTA LE VARIABILI DI LAVORO
344200011031     C                   Z-ADD     *ZEROS        TASADG           21 8
344300011031     C*
344400011031     C* CALCOLA L'A.D.G.
344500011031     C                   Z-ADD     VIMP(89)      WIMP             23 8
344600011031     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
344700011031     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
344800011031     C                   EXSR      ADGCAL
344900011031     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
345000011031     C*
345100011031IF  1C     TASADG        IFGT      *ZEROS
345200011031     C                   Z-ADD     90            I
345300011031     C                   Z-ADD     TASADG        VIMP(I)
345400011031E   1C                   ENDIF
345500011031     C*
345600011031     C                   ENDSR
345700011031     C*------------------------------------------------------------------------*
345800011031     C* CALIER - CALCOLA L'ISTAT SULL'ESPRESSO RITIRO ISOLA
345900011031     C*------------------------------------------------------------------------*
346000011031     C     CALIER        BEGSR
346100011031     C*
346200011031     C* REIMPOSTA LE VARIABILI DI LAVORO
346300011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
346400011031     C*
346500011031     C* CALCOLA L'ISTAT
346600011031     C                   Z-ADD     VIMP(89)      WIMP             23 8
346700011031     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
346800011031     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
346900011031     C                   EXSR      IACCAL
347000011031     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
347100011031     C*
347200011031IF  1C     TASIAC        IFGT      *ZEROS
347300011031     C                   Z-ADD     91            I
347400011031     C                   Z-ADD     TASIAC        VIMP(I)
347500011031E   1C                   ENDIF
347600011031     C*
347700011031     C                   ENDSR
347800011031     C*------------------------------------------------------------------------*
347900011031     C* CAIAER - CALCOLA L'ISTAT SULL'ADG DELL'ESPRESSO RITIRO ISOLA
348000011031     C*------------------------------------------------------------------------*
348100011031     C     CAIAER        BEGSR
348200011031     C*
348300011031     C* REIMPOSTA LE VARIABILI DI LAVORO
348400011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
348500011031     C*
348600011031     C* CALCOLA L'ISTAT
348700011031     C                   Z-ADD     VIMP(90)      WIMP             23 8
348800011031     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
348900011031     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
349000011031     C                   EXSR      IACCAL
349100011031     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
349200011031     C*
349300011031IF  1C     TASIAC        IFGT      *ZEROS
349400011031     C                   Z-ADD     92            I
349500011031     C                   Z-ADD     TASIAC        VIMP(I)
349600011031E   1C                   ENDIF
349700011031     C*
349800011031     C                   ENDSR
349900011030     C*------------------------------------------------------------------------*
350000011030     C* CALCIS - CALCOLA CONSEGNA PARTICOLARE ISOLA
350100011030     C*------------------------------------------------------------------------*
350200011030     C     CALCIS        BEGSR
350300011030     C*
350400011030     C* REIMPOSTA LE VARIABILI DI LAVORO
350500011030     C                   MOVEL     'S'           WCAR                           *SI UTILIZZO TAR.CAR
350600011031     C                   Z-ADD     *ZEROS        TASCIS           21 8
350700011030     C*
350800011030     C* CALCOLA CONSEGNA PARTICOLARE
350900011030     C                   MOVEL     'I'           TASTC             1            *CODICE CONSEGNA
351000011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
351100011030     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
351200011030     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
351300011030     C                   EXSR      CALCP
351400011030     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
351500011030     C                   Z-ADD     TASCP         TASCIS
351600011030     C*
351700011030IF  1C     TASCIS        IFGT      *ZEROS
351800011030     C                   Z-ADD     80            I
351900011030     C                   Z-ADD     TASCIS        VIMP(I)
352000011030E   1C                   ENDIF
352100011030     C*
352200011030     C* MEMORIZZA IL FLAG "APPLICAZIONE TARIFFA INOLTRO" DELLO SCAGLIONE DEL
352300011030     C* CODICE TASSAZIONE DEL DETTAGLIO TARIFFARIO
352400011030     C                   MOVEL     DEPAIN        CISAIN
352500011030     C*
352600011030     C                   ENDSR
352700011030     C*------------------------------------------------------------------------*
352800011030     C* CALAGS - CALCOLA L'A.D.G. SULLE CONSEGNE ISOLA
352900011030     C*------------------------------------------------------------------------*
353000011030     C     CALAGS        BEGSR
353100011030     C*
353200011030     C* REIMPOSTA LE VARIABILI DI LAVORO
353300011031     C                   Z-ADD     *ZEROS        TASADG           21 8
353400011030     C*
353500011030     C* CALCOLA L'A.D.G.
353600011031     C                   Z-ADD     TASCIS        WIMP             23 8
353700011030     C                   EXSR      ADGCAL
353800011030     C*
353900011030IF  1C     TASADG        IFGT      *ZEROS
354000011030     C                   Z-ADD     85            I
354100011030     C                   Z-ADD     TASADG        VIMP(I)
354200011030E   1C                   ENDIF
354300011030     C*
354400011030     C                   ENDSR
354500011030     C*------------------------------------------------------------------------*
354600011030     C* CALISS - CALCOLA L'ISTAT SULLE CONSEGNE ISOLA
354700011030     C*------------------------------------------------------------------------*
354800011030     C     CALISS        BEGSR
354900011030     C*
355000011030     C* REIMPOSTA LE VARIABILI DI LAVORO
355100011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
355200011030     C*
355300011030     C* CALCOLA L'ISTAT
355400011031     C                   Z-ADD     TASCIS        WIMP             23 8
355500011030     C                   EXSR      IACCAL
355600011030     C*
355700011030IF  1C     TASIAC        IFGT      *ZEROS
355800011030     C                   Z-ADD     86            I
355900011030     C                   Z-ADD     TASIAC        VIMP(I)
356000011030E   1C                   ENDIF
356100011030     C*
356200011030     C                   ENDSR
356300011030     C*------------------------------------------------------------------------*
356400011030     C* CALIAS - CALCOLA L'ISTAT SULL'ADG DELLE CONEGNE ISOLA
356500011030     C*------------------------------------------------------------------------*
356600011030     C     CALIAS        BEGSR
356700011030     C*
356800011030     C* REIMPOSTA LE VARIABILI DI LAVORO
356900011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
357000011030     C*
357100011030     C* CALCOLA L'ISTAT
357200011031     C                   Z-ADD     VIMP(85)      WIMP             23 8
357300011030     C                   EXSR      IACCAL
357400011030     C*
357500011030IF  1C     TASIAC        IFGT      *ZEROS
357600011030     C                   Z-ADD     87            I
357700011030     C                   Z-ADD     TASIAC        VIMP(I)
357800011030E   1C                   ENDIF
357900011030     C*
358000011030     C                   ENDSR
358100011030     C*------------------------------------------------------------------------*
358200011030     C* CALESS - CALCOLA L'ESPRESSO SULLA CONSEGNA ISOLA
358300011030     C*------------------------------------------------------------------------*
358400011030     C     CALESS        BEGSR
358500011030     C*
358600011030     C* REIMPOSTA LE VARIABILI DI LAVORO
358700011031     C                   Z-ADD     *ZEROS        TASESP           21 8
358800011030     C*
358900011030     C* CALCOLA L'ESPRESSO
359000011031     C                   Z-ADD     VIMP(80)      IMPON            23 8
359100011030     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
359200011030     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
359300011030     C                   EXSR      CALTS
359400011030     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
359500011030     C*
359600011030IF  1C     TASESP        IFGT      *ZEROS
359700011030     C                   Z-ADD     81            I
359800011030     C                   Z-ADD     TASESP        VIMP(I)
359900011030E   1C                   ENDIF
360000011030     C*
360100011030     C                   ENDSR
360200011030     C*------------------------------------------------------------------------*
360300011030     C* CALAES - CALCOLA L'A.D.G. SULL'ESPRESSO CONSEGNA ISOLA
360400011030     C*------------------------------------------------------------------------*
360500011030     C     CALAES        BEGSR
360600011030     C*
360700011030     C* REIMPOSTA LE VARIABILI DI LAVORO
360800011031     C                   Z-ADD     *ZEROS        TASADG           21 8
360900011030     C*
361000011030     C* CALCOLA L'A.D.G.
361100011031     C                   Z-ADD     VIMP(81)      WIMP             23 8
361200011030     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
361300011030     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
361400011030     C                   EXSR      ADGCAL
361500011030     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
361600011030     C*
361700011030IF  1C     TASADG        IFGT      *ZEROS
361800011030     C                   Z-ADD     82            I
361900011030     C                   Z-ADD     TASADG        VIMP(I)
362000011030E   1C                   ENDIF
362100011030     C*
362200011030     C                   ENDSR
362300011030     C*------------------------------------------------------------------------*
362400011030     C* CALIEL - CALCOLA L'ISTAT SULL'ESPRESSO CONSEGNA ISOLA
362500011030     C*------------------------------------------------------------------------*
362600011030     C     CALIES        BEGSR
362700011030     C*
362800011030     C* REIMPOSTA LE VARIABILI DI LAVORO
362900011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
363000011030     C*
363100011030     C* CALCOLA L'ISTAT
363200011031     C                   Z-ADD     VIMP(81)      WIMP             23 8
363300011030     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
363400011030     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
363500011030     C                   EXSR      IACCAL
363600011030     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
363700011030     C*
363800011030IF  1C     TASIAC        IFGT      *ZEROS
363900011030     C                   Z-ADD     83            I
364000011030     C                   Z-ADD     TASIAC        VIMP(I)
364100011030E   1C                   ENDIF
364200011030     C*
364300011030     C                   ENDSR
364400011030     C*------------------------------------------------------------------------*
364500011030     C* CAIAES - CALCOLA L'ISTAT SULL'ADG DELL'ESPRESSO CONSEGNA ISOLA
364600011030     C*------------------------------------------------------------------------*
364700011030     C     CAIAES        BEGSR
364800011030     C*
364900011030     C* REIMPOSTA LE VARIABILI DI LAVORO
365000011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
365100011030     C*
365200011030     C* CALCOLA L'ISTAT
365300011031     C                   Z-ADD     VIMP(82)      WIMP             23 8
365400011030     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
365500011030     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
365600011030     C                   EXSR      IACCAL
365700011030     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
365800011030     C*
365900011030IF  1C     TASIAC        IFGT      *ZEROS
366000011030     C                   Z-ADD     84            I
366100011030     C                   Z-ADD     TASIAC        VIMP(I)
366200011030E   1C                   ENDIF
366300011030     C*
366400011030     C                   ENDSR
366500980706     C*------------------------------------------------------------------------*
366600011030     C* CA3INL - RICALCOLA INOLTRO X LOCALITA' DISAGIATE / CONSEGNA ISOLA
366700980706     C*------------------------------------------------------------------------*
366800980706     C     CA3INL        BEGSR
366900980706     C*---
367000011030     C* FLAG='S' -> PAGA TARIFFA INOLTRO + TARIFFA LOCALITA' DISAGIATA  + ISOLA
367100011030     C* FLAG=' ' -> CANCELLA L'INOLTRO SE LA L.DISAGIATA o ISOLA E' DA PAGARE
367200011030     C*
367300011030     C* NOTE: In tassazione una bolla può essere O Località disagiata O Consegna isola.
367400011030     C*       Nel CAT l'azzeramento dell'inoltro scatta anche se SOLO UNA delle due
367500011030     C*       consegne prevede l'azzeramento e quindi può esistere l'incongruenza
367600011030     C*       che si abbia l'inoltro azzerato a fronte del fatto che UNA delle due consegne
367700011030     C*       non preveda l'azzeramento. (Mha!, avrete capito???)
367800011030     C*---
367900011030     c***
368000011030     c* LOCALITA' DISAGIATE / ISOLA
368100011030     c***
368200011030IF  1C     CPLAIN        IFEQ      *BLANKS
368300060607IF  2C     VC(35)        andeq     'S'                                          *VOCE DA CALCOLARE
368400011030     C     CISAIN        OREQ      *BLANKS
368500060607IF  2C     VC(80)        andeq     'S'                                          *VOCE DA CALCOLARE
368600980706     C*
368700980706     C* AZZERA LE VOCI INOLTRO
368800011030IF  2C     VIMP(35)      IFGT      *ZEROS                                       *L.DISAG.DA PAGARE
368900011030     C     VIMP(80)      ORGT      *ZEROS                                       *C.ISOLA DA PAGARE
369000980706DO  3C     1             DO        NVOCA         V                 6 0
369100980706IF  4C     VIMP(V)       IFGT      *ZEROS
369200980706IF  5C     V             IFEQ      006
369300980706     C     V             OREQ      021
369400980706     C     V             OREQ      024
369500980706     C     V             OREQ      029
369600980706     C     V             OREQ      034
369700980706     C     V             OREQ      036
369800980706     C     V             OREQ      037
369900980706     C     V             OREQ      038
370000980706     C                   Z-ADD     *ZEROS        VIMP(V)
370100980706E   5C                   ENDIF
370200980706E   4C                   ENDIF
370300980706E   3C                   ENDDO
370400980706E   2C                   ENDIF
370500980706E   1C                   ENDIF
370600980706     C*
370700980706     C                   ENDSR
370800980706     C*------------------------------------------------------------------------*
370900980706     C* CALTBT - CALCOLA LA BASE PER IL CALCOLO DELLE TARIFFE CON % SUL TRASPORTO
371000980706     C*------------------------------------------------------------------------*
371100980706     C     CALTBT        BEGSR
371200980706     C*
371300980706     C* REIMPOSTA LE VARIABILI DI LAVORO
371400011031     C                   Z-ADD     *ZEROS        TOTTRA           23 8
371500980706     C*
371600011030     C* PORTO + INOLTRO + LOCALITA' DISAGIATE + ISOLA + LORO ESPRESSI
371700980706     C     VIMP(1)       ADD       TOTTRA        TOTTRA                         *PORTO
371800980706     C     VIMP(6)       ADD       TOTTRA        TOTTRA                         *INOLTRO
371900980706     C     VIMP(35)      ADD       TOTTRA        TOTTRA                         *LOCALITA' DISAGIATE
372000980706     C     VIMP(7)       ADD       TOTTRA        TOTTRA                         *ESPRESSO PORTO
372100980706     C     VIMP(36)      ADD       TOTTRA        TOTTRA                         *ESPRESSO INOLTRO
372200980706     C     VIMP(39)      ADD       TOTTRA        TOTTRA                         *ESPRESSO L.DISAGIAT
372300011030     C     VIMP(80)      ADD       TOTTRA        TOTTRA                         *CONSEGNA ISOLA
372400011030     C     VIMP(81)      ADD       TOTTRA        TOTTRA                         *ESPRESSO C.ISOLA
372500980706     C*
372600980706     C                   ENDSR
372700970306     C*------------------------------------------------------------------------*
372800970306     C* CALTS  - CALCOLA TIPO SERVIZIO ESPRESSO
372900970306     C*------------------------------------------------------------------------*
373000980706     C     CALTS         BEGSR
373100970306     C*
373200980506     C* REIMPOSTA LE VARIABILI DI LAVORO
373300980506     C                   MOVEL     'S'           WCAR              1            *SI UTILIZZO TAR.CAR
373400020912     C                   MOVEL     'E'           WTSP              1              ESPRESSO='E'
373500980506     C*
373600970306     C* REPERISCE IL CODICE DELLA PARTICOLARITA' ESPRESSO
373700970306     C                   SETOFF                                           28
373800970306     C                   Z-ADD     1             I
373900980506     C     WTSP          LOOKUP    TS(I)                                  28
374000970306IF  1C     *IN28         IFEQ      *ON                                          *SERVIZIO ESISTENTE
374100980506     C     I             OCCUR     DS5E
374200980506IF  2C     §5EFTC        IFNE      *BLANKS                                      *SERVIZIO CON PARTIC
374300980506IF  3C     TAMTSP        IFEQ      WTSP
374400980804     C                   MOVEL     'N'           WCAR                           *NO UTILIZZO TAR.CAR
374500980506E   3C                   ENDIF
374600980506     C*
374700980506     C* CERCA LA CONSEGNA PARTICOLARE
374800980506     C                   MOVEL     §5EFTC        TASTC
374900980506     C                   EXSR      CERTPT
375000980506     C*
375100980506     C* CALCOLA RPV, ARROTONDAMENTI E PESO TASSABILE
375200980506IF  3C     $TPT          IFEQ      '1'                                          *TARIFFA TROVATA
375300980707     C                   MOVEL     TPTTPG        WTPG
375400980707     C                   MOVEL     TPTRPV        WRPV
375500980707     C                   CLEAR                   WDET
375600980707     C                   Z-ADD     TPTARL        WARL
375700980707     C                   Z-ADD     TPTARF        WARF
375800980707     C                   Z-ADD     TPTARO        WARO
375900980506     C                   EXSR      CALCOL
376000980506IF  4C     TPTTPG        IFEQ      'T'                                          *A TRASPORTO
376100980506     C                   Z-ADD     IMPON         ISOPES                         *PORTO
376200091026     c                   eval      tpg = 'T'
376300980506E   4C                   ENDIF
376400970306     C*
376500970306     C* CALCOLA LA TARIFFA ESPRESSO
376600980506     C                   Z-ADD     ISOPES        TPDPES                         *PESO
376700970306     C                   EXSR      CALACD
376800970306     C                   Z-ADD     VARIA         TASESP
376900970306E   3C                   ENDIF
377000970306E   2C                   ENDIF
377100970306E   1C                   ENDIF
377200970306     C*
377300970306     C                   ENDSR
377400970306     C*------------------------------------------------------------------------*
377500970306     C* CALAGD - CALCOLA L'A.D.G. SUL DIRITTO FISSO
377600970306     C*------------------------------------------------------------------------*
377700970306     C     CALAGD        BEGSR
377800970306     C*
377900970306     C* REIMPOSTA LE VARIABILI DI LAVORO
378000011031     C                   Z-ADD     *ZEROS        TASADG           21 8
378100970306     C*
378200970306     C* CALCOLA L'A.D.G.
378300011031     C                   Z-ADD     TASDIF        WIMP             23 8
378400970306     C                   EXSR      ADGCAL
378500970306     C*
378600970314IF  1C     TASADG        IFGT      *ZEROS
378700970306     C                   Z-ADD     22            I
378800970306     C                   Z-ADD     TASADG        VIMP(I)
378900970314E   1C                   ENDIF
379000970306     C*
379100970306     C                   ENDSR
379200970306     C*------------------------------------------------------------------------*
379300970306     C* ADGCAL - CALCOLA A.D.G. SU IMPORTO GENERICO
379400970306     C*------------------------------------------------------------------------*
379500970306     C     ADGCAL        BEGSR
379600041213     C* anni precedenti
379700970306IF  1C     TAMPAG        IFNE      *ZEROS                                       *INDICATA IN TARIFFA
379800011031     C     WIMP          MULT      TAMPAG        COMADG           23 8
379900970306IF  2C     COMADG        IFGT      *ZEROS
380000970306     C     COMADG        DIV(H)    100           TASADG
380100970306E   2C                   ENDIF
380200970306E   1C                   ENDIF
380300041213     c                   z-add     tasadg        tasadgs
380400041213     C* anno in corso
380500041213IF  1C     TAMPpa        IFNE      *ZEROS                                       *INDICATA IN TARIFFA
380600041213     c                   eval      comadg = (wimp + tasadg)*tamppa
380700041213IF  2C     COMADG        IFGT      *ZEROS
380800041213     C     COMADG        DIV(H)    100           TASADG
380900041213E   2C                   ENDIF
381000041213     c                   add       tasadgs       tasadg
381100041213E   1C                   ENDIF
381200970306     C*
381300970306     C                   ENDSR
381400980706     C*------------------------------------------------------------------------*
381500980706     C* IACCAL - CALCOLA ISTAT SU IMPORTO GENERICO
381600980706     C*------------------------------------------------------------------------*
381700980706     C     IACCAL        BEGSR
381800980706     C*
381900980706     C* REIMPOSTA LE VARIABILI DI LAVORO
382000101018     C                   Z-ADD     *ZEROS        PUNIAC            7 4
382100101018     C                   Z-ADD     *ZEROS        PERIAC           23 8
382200101018     C                   Z-ADD     *ZEROS        PERIAX           23 8
382300980706     C*
382400061018      * CALCOLO LA PERCENTUALE DI BASE DI CALCOLO
382500061018      * SE ESISTE GIÀ UNA PERCENTUALE CALCOLO LA PERENTUALE DELLA PERCENTYUALE E POI SOMMO TUTTO
382600061018      * ESEMPIO-....
382700061018      * 1° PUNTO PERCENTUALE ISTAT  2%
382800061018      * 2° PUNTO PERCENTUALE ISTAT  5%
382900061018      * PERCENTUALE ISTAT = 2 + (IL 5% DI 2) + 5 = 7,1
383000101018     c                   clear                   trulistds
383100101018     c                   eval      IISTsca = tamrct
383200101018     c                   eval      IISTdsp = d00dtc
383300101018     c                   call      'TRULISTR'
383400101018     c                   parm                    trulistds
383500101018     c                   IF        OISTerr = *blanks
383600101018     c                   eval      puniac = OISTiac
383700101018     c                   ENDIF
383800980706     C* CALCOLA L'ISTAT
383900980706     C     PUNIAC        MULT      TAMPRI        PERIAX
384000980706IF  2C     PERIAX        IFNE      *ZEROS
384100980706     C     PERIAX        DIV       100           PERIAC
384200980706E   2C                   ENDIF
384300980706IF  2C     PERIAC        IFNE      *ZEROS
384400061018     C                   DIV       100           PERIAC
384500980706     C     WIMP          MULT(H)   PERIAC        TASIAC
384600980706E   2C                   ENDIF
384700980706     C*
384800980706     C                   ENDSR
384900970306     C*------------------------------------------------------------------------*
385000060314     C* CALASC - CALCOLA ASSICURAZIONE ac plusTO
385100970306     C*------------------------------------------------------------------------*
385200970306     C     CALASC        BEGSR
385300970306     C*
385400970306     C* REIMPOSTA LE VARIABILI DI LAVORO
385500011031     C                   Z-ADD     *ZEROS        TASASF           21 8
385600011031     C                   Z-ADD     *ZEROS        TASASA           21 8
385700980506     C*---
385800980506     C* FRANCO
385900980506     C*---
386000980506     C                   MOVEL     'F'           WTPO
386100970306     C                   EXSR      CALVR
386200970306IF  1C     VC(9)         IFEQ      'S'                                          *VOCE DA CALCOLARE
386300970314     C     TASASF        ANDGT     *ZEROS
386400970306     C                   Z-ADD     9             I
386500970306     C                   Z-ADD     TASASF        VIMP(I)
386600970306E   1C                   ENDIF
386700980506     C*---
386800980506     C* ASSEGNATO
386900980506     C*---
387000980506     C                   MOVEL     'A'           WTPO
387100980506     C                   EXSR      CALVR
387200970306IF  1C     VC(31)        IFEQ      'S'                                          *VOCE DA CALCOLARE
387300970314     C     TASASA        ANDGT     *ZEROS
387400970306     C                   Z-ADD     31            I
387500970306     C                   Z-ADD     TASASA        VIMP(I)
387600970306E   1C                   ENDIF
387700970306     C*
387800970306     C                   ENDSR
387900060314     C*------------------------------------------------------------------------*
388000060314     C* CALAcb - CALCOLA ASSICURAZIONE ac base
388100060314     C*------------------------------------------------------------------------*
388200060315     C     CALAcb        BEGSR
388300060314     C*
388400060314     C* REIMPOSTA LE VARIABILI DI LAVORO
388500060314     C                   Z-ADD     *ZEROS        TASASF           21 8
388600060314     C                   Z-ADD     *ZEROS        TASASA           21 8
388700060314     C*---
388800060314     C* FRANCO
388900060314     C*---
389000060314     C                   MOVEL     'F'           WTPO
389100060314     C                   EXSR      CALVR
389200060314IF  1C     VC(105)       IFEQ      'S'                                          *VOCE DA CALCOLARE
389300060314     C     TASASF        ANDGT     *ZEROS
389400060314     C                   Z-ADD     105           I
389500060314     C                   Z-ADD     TASASF        VIMP(I)
389600060314E   1C                   ENDIF
389700060314     C*---
389800060314     C* ASSEGNATO
389900060314     C*---
390000060314     C                   MOVEL     'A'           WTPO
390100060314     C                   EXSR      CALVR
390200060314IF  1C     VC(108)       IFEQ      'S'                                          *VOCE DA CALCOLARE
390300060314     C     TASASA        ANDGT     *ZEROS
390400060314     C                   Z-ADD     108           I
390500060314     C                   Z-ADD     TASASA        VIMP(I)
390600060314E   1C                   ENDIF
390700060314     C*
390800060314     C                   ENDSR
390900970306     C*------------------------------------------------------------------------*
391000011023     C* CALVR - CALCOLA CONSEGNA PARTICOLARE ASSICURAZIONE PER CONTO
391100970306     C*------------------------------------------------------------------------*
391200970306     C     CALVR         BEGSR
391300970306     C*
391400980506     C* NB: L'IMPORTO DA ASSICURARE NEI DATI TIPO E' OBBLIGATORIO: TOGLIENDO LA
391500980506     C*     OBBLIGATORIETA' E' NECESSARIO GESTIRE IL MANCA TARIFFA PER MANDATI
391600980702     C*     A VALORE VARIABILE CON IMPORTO UGUALE A ZERO.
391700970306     C*
391800970306     C* SE CLIENTE 88 CALCOLA TARIFFA SE ESISTE IL VALORE MERCE NEI DATI TIPO
391900970306     C                   MOVEL     D00KSC        WA4               4 0
392000970306IF  1C     WA4           IFEQ      8888
392100970418     C     ETCASC        ANDGT     *ZEROS
392200970306     C     WA4           ORNE      8888
392300970306     C                   Z-ADD     1             I
392400970306     C                   SETOFF                                           28
392500990819     C                   MOVEL     KTPFTC        A1
392600990819     C     A1            LOOKUP    CP(I)                                  28
392700970306IF  2C     *IN28         IFEQ      *ON                                          *C.PART.ASS.ESISTENT
392800970306     C     I             OCCUR     CPIDS                                        *TARIFFA DI CARTELLO
392900970306     C*
393000970306     C* RICERCA L'ASSICURAZIONE PER CONTO NELLE CONSEGNE PARTICOLARI DEL CLIENTE
393100970306     C                   Z-ADD     D00KSC        KTPKSC
393200970306     C                   Z-ADD     D00CTR        KTPCTR
393300970306     C                   Z-ADD     D00PRG        KTPPRG
393400011019     C                   MOVEL     D00DIV        WDIV
393500970306     C                   MOVEL     '0'           $TPT                           *TARIFFA NON TROVATA
393600990819     C     KEYTPT        SETLL     TITPT01L                               99
393700990819     C     KEYTPT        READE     TITPT01L                               99
393800970306DO  3C     *IN99         DOWEQ     *OFF
393900991125     C     $TPT          ANDEQ     '0'
394000991125     C*
394100991125     C* CONTROLLA SE CONVERTIRE I CAMPI DI TITPT
394200011026IF  4C     WDIV          IFNE      WDIVCE
394300011019     C                   MOVEL     WDIV          WDVI
394400991125     C                   EXSR      CNVTPT
394500991125E   4C                   ENDIF
394600991125     C*
394700970306IF  4C     TPTATB        IFEQ      *BLANKS
394800980506IF  5C     TPTAPL        IFEQ      *BLANKS                                      *APPLICAZIONE ENTRAM
394900980506     C     TPTAPL        OREQ      'A'                                          *APPLICAZIONE ARRIVO
395000980506     C     WTPO          ANDEQ     'A'                                          *ASSEGNATI O
395100980506     C     TPTAPL        OREQ      'P'                                          *APPLICAZIONE PARTEN
395200980506     C     WTPO          ANDEQ     'F'                                          *FRANCHI
395300970306     C                   MOVEL     '1'           $TPT                           *TARIFFA TROVATA
395400970314     C*
395500970314     C* USA IL VALORE MERCE IN TARIFFA SE C'E' ALTRIMENTI QUELLO DEI DATI TIPO
395600980506IF  6C     TPTVLM        IFGT      *ZEROS                                       *VALORE IN TARIFFA
395700980506IF  7C     TPTFVM        IFEQ      '1'                                          *TARIFFA A COLLI
395800980506     C     SCSC(S)       MULT      TPTVLM        IASLIT
395900980506E   7C                   ENDIF
396000980506IF  7C     TPTFVM        IFEQ      '2'                                          *TARIFFA A SPEDIZION
396100980506     C                   Z-ADD     TPTVLM        IASLIT
396200980506E   7C                   ENDIF
396300980506IF  7C     TPTFVM        IFEQ      '3'                                          *TARIFFA A PESO
396400980506     C     SPSC(S)       MULT      TPTVLM        IASLIT
396500980506E   7C                   ENDIF
396600980506X   6C                   ELSE                                                   *NO VALORE IN TARIFF
396700970314     C                   MOVEL     '2'           TPTFVM                         *TARIFFA A SPEDIZION
396800980506E   6C                   ENDIF
396900980506E   5C                   ENDIF
397000980506E   4C                   ENDIF
397100970306     C*
397200970306     C* LETTURA SUCESSIVA TARIFFE
397300970306IF  4C     $TPT          IFEQ      '0'                                          *TARIFFA NON TROVATA
397400990819     C     KEYTPT        READE     TITPT01L                               99
397500970306E   4C                   ENDIF
397600970306E   3C                   ENDDO                                                  *FINE TAR.PART.KSC
397700970306     C*
397800970306     C* TARIFFA ASSICURAZIONE NON TROVATA NELLE CONSEGNE PARTICOLARI DEL CLIENTE
397900970306     C* RICERCA NELLE CONSEGNE PARTICOLARI DI CARTELLO
398000970306IF  3C     $TPT          IFEQ      '0'                                          *TARIFFA KSC NO TROV
398100970306     C     CP88          ANDNE     'NO'
398200970306     C                   MOVEL     '1'           $TPT                           *TARIFFA TROVATA
398300970306     C                   Z-ADD     CARKSC        TPTKSC                         *CHIAVE CARTELLO
398400970306     C                   Z-ADD     CARCTR        TPTCTR
398500970306     C                   Z-ADD     CARPRG        TPTPRG
398600970306     C                   MOVEL     KTPFTC        TPTFTC
398700970306     C                   MOVEL     TTPG8P        TPTTPG                         *CAMPI CARTELLO
398800970306     C                   Z-ADD     TARF8P        TPTARF
398900970306     C                   Z-ADD     TARL8P        TPTARL
399000970306     C                   Z-ADD     TARO8P        TPTARO
399100970306     C                   Z-ADD     TRPV8P        TPTRPV
399200970306     C                   MOVEL     TAPL8P        TPTAPL
399300970306E   3C                   ENDIF
399400970306     C*
399500980506     C* CALCOLA RPV, ARROTONDAMENTI E PESO TASSABILE
399600970306IF  3C     $TPT          IFEQ      '1'                                          *TARIFFA TROVATA
399700980707     C                   MOVEL     TPTTPG        WTPG
399800980707     C                   MOVEL     TPTRPV        WRPV
399900980707     C                   CLEAR                   WDET
400000980707     C                   Z-ADD     TPTARL        WARL
400100980707     C                   Z-ADD     TPTARF        WARF
400200980707     C                   Z-ADD     TPTARO        WARO
400300980506     C                   EXSR      CALCOL
400400980506IF  4C     TPTTPG        IFEQ      'V'                                          *CALCOLO A VALORE
400500980506     C                   Z-ADD     IASLIT        ISOPES                         *IMPORTO DA ASSICURA
400600970306E   4C                   ENDIF
400700980703IF  4C     TPTTPG        IFEQ      'T'                                          *A TRASPORTO
400800980703     C                   Z-ADD     TOTTRA        ISOPES                         *TRASPORTO
400900980703E   4C                   ENDIF
401000970306     C*
401100970306     C* CALCOLA LA TARIFFA ASSICURAZIONE PER CONTO
401200011031     C                   Z-ADD     ISOPES        TPDPES           23 8          *PESO
401300970306     C                   EXSR      CALACD
401400980706     C                   EXSR      CALTRA                                       *CALCOLA % A TRASPOR
401500980506IF  4C     WTPO          IFEQ      'A'
401600980506     C                   Z-ADD     VARIA         TASASA                         *ASSIC.NE ASSEGNATO
401700980506X   4C                   ELSE
401800980506     C                   Z-ADD     VARIA         TASASF                         *ASSIC.NE FRANCO
401900970306E   4C                   ENDIF
402000970306E   3C                   ENDIF
402100970306E   2C                   ENDIF
402200970306E   1C                   ENDIF
402300970306     C*
402400970306     C                   ENDSR
402500001227     C*------------------------------------------------------------------------*
402600060315     C* CAL048 - CALCOLA ADG SU ASSIC ac plus FRANCO
402700001227     C*------------------------------------------------------------------------*
402800001227     C     CAL048        BEGSR
402900001227     C*
403000001227     C* REIMPOSTA LE VARIABILI DI LAVORO
403100011031     C                   Z-ADD     *ZEROS        TASADG           21 8
403200001227     C*
403300001227     C* CALCOLA L'A.D.G.
403400011031     C                   Z-ADD     VIMP(9)       WIMP             23 8
403500001227     C                   EXSR      ADGCAL
403600001227     C*
403700001227IF  1C     TASADG        IFGT      *ZEROS
403800001227     C                   Z-ADD     48            I
403900001227     C                   Z-ADD     TASADG        VIMP(I)
404000001227E   1C                   ENDIF
404100001227     C*
404200001227     C                   ENDSR
404300001227     C*------------------------------------------------------------------------*
404400060315     C* CAL065 - CALCOLA ISTAT SU ADG/ASS ac plus FRANCO
404500001227     C*------------------------------------------------------------------------*
404600001227     C     CAL065        BEGSR
404700001227     C*
404800001227     C* REIMPOSTA LE VARIABILI DI LAVORO
404900011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
405000001227     C*
405100001227     C* CALCOLA L'ISTAT
405200011031     C                   Z-ADD     VIMP(48)      WIMP             23 8
405300001227     C                   EXSR      IACCAL
405400001227     C*
405500001227IF  1C     TASIAC        IFGT      *ZEROS
405600001227     C                   Z-ADD     65            I
405700001227     C                   Z-ADD     TASIAC        VIMP(I)
405800001227E   1C                   ENDIF
405900001227     C*
406000001227     C                   ENDSR
406100001227     C*------------------------------------------------------------------------*
406200060315     C* CAL060 - CALCOLA ADG SU ASSIC ac plus ASSEGNATO
406300001227     C*------------------------------------------------------------------------*
406400001227     C     CAL060        BEGSR
406500001227     C*
406600001227     C* REIMPOSTA LE VARIABILI DI LAVORO
406700011031     C                   Z-ADD     *ZEROS        TASADG           21 8
406800001227     C*
406900001227     C* CALCOLA L'A.D.G.
407000011031     C                   Z-ADD     VIMP(31)      WIMP             23 8
407100001227     C                   EXSR      ADGCAL
407200001227     C*
407300001227IF  1C     TASADG        IFGT      *ZEROS
407400001227     C                   Z-ADD     60            I
407500001227     C                   Z-ADD     TASADG        VIMP(I)
407600001227E   1C                   ENDIF
407700001227     C*
407800001227     C                   ENDSR
407900001227     C*------------------------------------------------------------------------*
408000060315     C* CAL077 - CALCOLA ISTAT SU ADG/ASS ac plus ASSEGNATO
408100001227     C*------------------------------------------------------------------------*
408200001227     C     CAL077        BEGSR
408300001227     C*
408400001227     C* REIMPOSTA LE VARIABILI DI LAVORO
408500011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
408600001227     C*
408700001227     C* CALCOLA L'ISTAT
408800011031     C                   Z-ADD     VIMP(60)      WIMP             23 8
408900001227     C                   EXSR      IACCAL
409000001227     C*
409100001227IF  1C     TASIAC        IFGT      *ZEROS
409200001227     C                   Z-ADD     77            I
409300001227     C                   Z-ADD     TASIAC        VIMP(I)
409400001227E   1C                   ENDIF
409500001227     C*
409600001227     C                   ENDSR
409700060315     C*------------------------------------------------------------------------*
409800060315     C* CAL106 - CALCOLA ADG SU ASSIC ac base FRANCO
409900060315     C*------------------------------------------------------------------------*
410000060315     C     CAL106        BEGSR
410100060315     C*
410200060315     C* REIMPOSTA LE VARIABILI DI LAVORO
410300060315     C                   Z-ADD     *ZEROS        TASADG           21 8
410400060315     C*
410500060315     C* CALCOLA L'A.D.G.
410600060315     C                   Z-ADD     VIMP(105)     WIMP             23 8
410700060315     C                   EXSR      ADGCAL
410800060315     C*
410900060315IF  1C     TASADG        IFGT      *ZEROS
411000060315     C                   Z-ADD     106           I
411100060315     C                   Z-ADD     TASADG        VIMP(I)
411200060315E   1C                   ENDIF
411300060315     C*
411400060315     C                   ENDSR
411500060315     C*------------------------------------------------------------------------*
411600060315     C* CAL107 - CALCOLA ISTAT SU ADG/ASS ac base FRANCO
411700060315     C*------------------------------------------------------------------------*
411800060315     C     CAL107        BEGSR
411900060315     C*
412000060315     C* REIMPOSTA LE VARIABILI DI LAVORO
412100060315     C                   Z-ADD     *ZEROS        TASIAC           21 8
412200060315     C*
412300060315     C* CALCOLA L'ISTAT
412400060315     C                   Z-ADD     VIMP(106)     WIMP             23 8
412500060315     C                   EXSR      IACCAL
412600060315     C*
412700060315IF  1C     TASIAC        IFGT      *ZEROS
412800060315     C                   Z-ADD     107           I
412900060315     C                   Z-ADD     TASIAC        VIMP(I)
413000060315E   1C                   ENDIF
413100060315     C*
413200060315     C                   ENDSR
413300060315     C*------------------------------------------------------------------------*
413400060315     C* CAL109 - CALCOLA ADG SU ASSIC ac base ASSEGNATO
413500060315     C*------------------------------------------------------------------------*
413600060315     C     CAL109        BEGSR
413700060315     C*
413800060315     C* REIMPOSTA LE VARIABILI DI LAVORO
413900060315     C                   Z-ADD     *ZEROS        TASADG           21 8
414000060315     C*
414100060315     C* CALCOLA L'A.D.G.
414200060315     C                   Z-ADD     VIMP(108)     WIMP             23 8
414300060315     C                   EXSR      ADGCAL
414400060315     C*
414500060315IF  1C     TASADG        IFGT      *ZEROS
414600060315     C                   Z-ADD     109           I
414700060315     C                   Z-ADD     TASADG        VIMP(I)
414800060315E   1C                   ENDIF
414900060315     C*
415000060315     C                   ENDSR
415100060315     C*------------------------------------------------------------------------*
415200060315     C* CAL110 - CALCOLA ISTAT SU ADG/ASS ac base ASSEGNATO
415300060315     C*------------------------------------------------------------------------*
415400060315     C     CAL110        BEGSR
415500060315     C*
415600060315     C* REIMPOSTA LE VARIABILI DI LAVORO
415700060315     C                   Z-ADD     *ZEROS        TASIAC           21 8
415800060315     C*
415900060315     C* CALCOLA L'ISTAT
416000060315     C                   Z-ADD     VIMP(109)     WIMP             23 8
416100060315     C                   EXSR      IACCAL
416200060315     C*
416300060315IF  1C     TASIAC        IFGT      *ZEROS
416400060315     C                   Z-ADD     110           I
416500060315     C                   Z-ADD     TASIAC        VIMP(I)
416600060315E   1C                   ENDIF
416700060315     C*
416800060315     C                   ENDSR
416900970306     C*------------------------------------------------------------------------*
417000970306     C* CALRCV - CALCOLA ASSICURAZIONE R.C.V.
417100970306     C*------------------------------------------------------------------------*
417200970306     C     CALRCV        BEGSR
417300970306     C*
417400970306     C* REIMPOSTA LE VARIABILI DI LAVORO
417500011031     C                   Z-ADD     *ZEROS        TASRCF           21 8
417600011031     C                   Z-ADD     *ZEROS        TASRCA           21 8
417700980506     C*---
417800980506     C* FRANCO
417900980506     C*---
418000980506     C                   MOVEL     'F'           WTPO
418100970306     C                   EXSR      CALVE
418200970306IF  1C     VC(10)        IFEQ      'S'                                          *VOCE DA CALCOLARE
418300970314     C     TASRCF        ANDGT     *ZEROS
418400970306     C                   Z-ADD     10            I
418500970306     C                   Z-ADD     TASRCF        VIMP(I)
418600970306E   1C                   ENDIF
418700980506     C*---
418800980506     C* ASSEGNATO
418900980506     C*---
419000980707     C                   MOVEL     'A'           WTPO              1
419100980506     C                   EXSR      CALVE
419200970306IF  1C     VC(32)        IFEQ      'S'                                          *VOCE DA CALCOLARE
419300970314     C     TASRCA        ANDGT     *ZEROS
419400970306     C                   Z-ADD     32            I
419500970306     C                   Z-ADD     TASRCA        VIMP(I)
419600970306E   1C                   ENDIF
419700970306     C*
419800970306     C                   ENDSR
419900970306     C*------------------------------------------------------------------------*
420000970306     C* CALVE - CALCOLA CONSEGNA PARTICOLARE ASSICURAZIONE R.C.V.
420100970306     C*------------------------------------------------------------------------*
420200970306     C     CALVE         BEGSR
420300980804     C*
420400980804     C* REIMPOSTA LE VARIABILI DI LAVORO
420500980804     C                   MOVEL     'N'           WCAR                           *NO UTILIZZO TAR.CAR
420600980506     C*
420700980506     C* CERCA TARIFFA PARTICOLARE
420800980506     C                   MOVEL     'R'           TASTC             1            *CODICE CONSEGNA
420900980506     C                   EXSR      CERTPT
421000970306     C*
421100980506     C* CALCOLA RPV, ARROTONDAMENTI E PESO TASSABILE
421200980506IF  1C     $TPT          IFEQ      '1'                                          *TARIFFA TROVATA
421300980707     C                   MOVEL     TPTTPG        WTPG
421400980707     C                   MOVEL     TPTRPV        WRPV
421500980707     C                   CLEAR                   WDET
421600980707     C                   Z-ADD     TPTARL        WARL
421700980707     C                   Z-ADD     TPTARF        WARF
421800980707     C                   Z-ADD     TPTARO        WARO
421900980506     C                   EXSR      CALCOL
422000980506IF  2C     TPTTPG        IFEQ      'V'                                          *A VALORE
422100040716IF  3C     TPTVLM        IFGT      §gelrp
422200040716     C     TPTVLM        SUB       §gelrp        ISOPES
422300980506     C     ISOPES        MULT      SPSC(S)       ISOPES
422400980506E   3C                   ENDIF
422500980506E   2C                   ENDIF
422600980703IF  2C     TPTTPG        IFEQ      'T'                                          *A TRASPORTO
422700980703     C                   Z-ADD     TOTTRA        ISOPES                         *TRASPORTO
422800980703E   2C                   ENDIF
422900970306     C*
423000970306     C* CALCOLA LA TARIFFA ASSICURAZIONE R.C.V.
423100980506     C                   Z-ADD     ISOPES        TPDPES                         *PESO
423200970306     C                   EXSR      CALACD
423300980706     C                   EXSR      CALTRA                                       *CALCOLA % A TRASPOR
423400970306     C*---
423500970306     C* TIPO APPLICAZIONE TARIFFA:
423600980506     C* PARTENZA -> RICAVO SOLO PER FRANCHI
423700980506     C* ARRIVO   -> RICAVO SOLO PER ASSEGNATI
423800980506     C* BLANKS   -> RICAVO PER ENTRAMBI
423900970306     C*---
424000980506IF  2C     WTPO          IFEQ      'F'
424100980506IF  3C     TPTAPL        IFEQ      *BLANKS
424200980506     C     TPTAPL        OREQ      'P'
424300980506     C                   Z-ADD     VARIA         TASRCF                         *RICAVO FRANCO
424400980506E   3C                   ENDIF
424500980506X   2C                   ELSE
424600980506IF  3C     TPTAPL        IFEQ      *BLANKS
424700980506     C     TPTAPL        OREQ      'A'
424800980506     C                   Z-ADD     VARIA         TASRCA                         *RICAVO ASSEGNAO
424900980506E   3C                   ENDIF
425000980506E   2C                   ENDIF
425100980506E   1C                   ENDIF
425200970306     C*
425300970306     C                   ENDSR
425400001227     C*------------------------------------------------------------------------*
425500001227     C* CAL049 - CALCOLA ADG SU ASSIC RCV  FRANCO
425600001227     C*------------------------------------------------------------------------*
425700001227     C     CAL049        BEGSR
425800001227     C*
425900001227     C* REIMPOSTA LE VARIABILI DI LAVORO
426000011031     C                   Z-ADD     *ZEROS        TASADG           21 8
426100001227     C*
426200001227     C* CALCOLA L'A.D.G.
426300011031     C                   Z-ADD     VIMP(10)      WIMP             23 8
426400001227     C                   EXSR      ADGCAL
426500001227     C*
426600001227IF  1C     TASADG        IFGT      *ZEROS
426700001227     C                   Z-ADD     49            I
426800001227     C                   Z-ADD     TASADG        VIMP(I)
426900001227E   1C                   ENDIF
427000001227     C*
427100001227     C                   ENDSR
427200001227     C*------------------------------------------------------------------------*
427300001227     C* CAL066 - CALCOLA ISTAT SU ADG/ASS RCV  FRANCO
427400001227     C*------------------------------------------------------------------------*
427500001227     C     CAL066        BEGSR
427600001227     C*
427700001227     C* REIMPOSTA LE VARIABILI DI LAVORO
427800011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
427900001227     C*
428000001227     C* CALCOLA L'ISTAT
428100011031     C                   Z-ADD     VIMP(49)      WIMP             23 8
428200001227     C                   EXSR      IACCAL
428300001227     C*
428400001227IF  1C     TASIAC        IFGT      *ZEROS
428500001227     C                   Z-ADD     66            I
428600001227     C                   Z-ADD     TASIAC        VIMP(I)
428700001227E   1C                   ENDIF
428800001227     C*
428900001227     C                   ENDSR
429000001227     C*------------------------------------------------------------------------*
429100001227     C* CAL061 - CALCOLA ADG SU ASSIC RCV  ASSEGNATO
429200001227     C*------------------------------------------------------------------------*
429300001227     C     CAL061        BEGSR
429400001227     C*
429500001227     C* REIMPOSTA LE VARIABILI DI LAVORO
429600011031     C                   Z-ADD     *ZEROS        TASADG           21 8
429700001227     C*
429800001227     C* CALCOLA L'A.D.G.
429900011031     C                   Z-ADD     VIMP(32)      WIMP             23 8
430000001227     C                   EXSR      ADGCAL
430100001227     C*
430200001227IF  1C     TASADG        IFGT      *ZEROS
430300001227     C                   Z-ADD     61            I
430400001227     C                   Z-ADD     TASADG        VIMP(I)
430500001227E   1C                   ENDIF
430600001227     C*
430700001227     C                   ENDSR
430800001227     C*------------------------------------------------------------------------*
430900001227     C* CAL078 - CALCOLA ISTAT SU ADG/ASS RCV  ASSEGNATO
431000001227     C*------------------------------------------------------------------------*
431100001227     C     CAL078        BEGSR
431200001227     C*
431300001227     C* REIMPOSTA LE VARIABILI DI LAVORO
431400011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
431500001227     C*
431600001227     C* CALCOLA L'ISTAT
431700011031     C                   Z-ADD     VIMP(61)      WIMP             23 8
431800001227     C                   EXSR      IACCAL
431900001227     C*
432000001227IF  1C     TASIAC        IFGT      *ZEROS
432100001227     C                   Z-ADD     78            I
432200001227     C                   Z-ADD     TASIAC        VIMP(I)
432300001227E   1C                   ENDIF
432400001227     C*
432500001227     C                   ENDSR
432600970306     C*------------------------------------------------------------------------*
432700970306     C* CALCPA - CALCOLA CONSEGNA PARTICOLARE SU APPUNTAMENTO
432800970306     C*------------------------------------------------------------------------*
432900970306     C     CALCPA        BEGSR
433000970306     C*
433100970306     C* REIMPOSTA LE VARIABILI DI LAVORO
433200980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
433300011031     C                   Z-ADD     *ZEROS        TASCPA           21 8
433400970306     C*
433500970306     C* CALCOLA CONSEGNA PARTICOLARE
433600970306     C                   MOVEL     'A'           TASTC             1            *CODICE CONSEGNA
433700011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
433800970306     C                   EXSR      CALCP
433900970306     C                   Z-ADD     TASCP         TASCPA
434000970306     C*
434100970314IF  1C     TASCPA        IFGT      *ZEROS
434200970306     C                   Z-ADD     11            I
434300970306     C                   Z-ADD     TASCPA        VIMP(I)
434400970314E   1C                   ENDIF
434500970306     C*
434600970306     C                   ENDSR
434700001227     C*------------------------------------------------------------------------*
434800001227     C* CAL050 - CALCOLA ADG SU CONSEGNA APPUNTAMENTO
434900001227     C*------------------------------------------------------------------------*
435000001227     C     CAL050        BEGSR
435100001227     C*
435200001227     C* REIMPOSTA LE VARIABILI DI LAVORO
435300011031     C                   Z-ADD     *ZEROS        TASADG           21 8
435400001227     C*
435500001227     C* CALCOLA L'A.D.G.
435600011031     C                   Z-ADD     VIMP(11)      WIMP             23 8
435700001227     C                   EXSR      ADGCAL
435800001227     C*
435900001227IF  1C     TASADG        IFGT      *ZEROS
436000001227     C                   Z-ADD     50            I
436100001227     C                   Z-ADD     TASADG        VIMP(I)
436200001227E   1C                   ENDIF
436300001227     C*
436400001227     C                   ENDSR
436500001227     C*------------------------------------------------------------------------*
436600001227     C* CAL067 - CALCOLA ISTAT SU ADG/CONSEGNA APPUNTAMENTO
436700001227     C*------------------------------------------------------------------------*
436800001227     C     CAL067        BEGSR
436900001227     C*
437000001227     C* REIMPOSTA LE VARIABILI DI LAVORO
437100011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
437200001227     C*
437300001227     C* CALCOLA L'ISTAT
437400011031     C                   Z-ADD     VIMP(50)      WIMP             23 8
437500001227     C                   EXSR      IACCAL
437600001227     C*
437700001227IF  1C     TASIAC        IFGT      *ZEROS
437800001227     C                   Z-ADD     67            I
437900001227     C                   Z-ADD     TASIAC        VIMP(I)
438000001227E   1C                   ENDIF
438100001227     C*
438200001227     C                   ENDSR
438300970306     C*------------------------------------------------------------------------*
438400970306     C* CALCPS - CALCOLA CONSEGNA PARTICOLARE AI SUPERMERCATI
438500970306     C*------------------------------------------------------------------------*
438600970306     C     CALCPS        BEGSR
438700970306     C*
438800970306     C* REIMPOSTA LE VARIABILI DI LAVORO
438900980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
439000011031     C                   Z-ADD     *ZEROS        TASCPS           21 8
439100970306     C*
439200970306     C* CALCOLA CONSEGNA PARTICOLARE
439300970306     C                   MOVEL     'S'           TASTC             1            *CODICE CONSEGNA
439400011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
439500980804     C                   EXSR      CALCP
439600970306     C                   Z-ADD     TASCP         TASCPS
439700970306     C*
439800970314IF  1C     TASCPS        IFGT      *ZEROS
439900970306     C                   Z-ADD     12            I
440000970306     C                   Z-ADD     TASCPS        VIMP(I)
440100970314E   1C                   ENDIF
440200970306     C*
440300970306     C                   ENDSR
440400001227     C*------------------------------------------------------------------------*
440500001227     C* CAL051 - CALCOLA ADG SU CONSEGNA SUPERMERCATI
440600001227     C*------------------------------------------------------------------------*
440700001227     C     CAL051        BEGSR
440800001227     C*
440900001227     C* REIMPOSTA LE VARIABILI DI LAVORO
441000011031     C                   Z-ADD     *ZEROS        TASADG           21 8
441100001227     C*
441200001227     C* CALCOLA L'A.D.G.
441300011031     C                   Z-ADD     VIMP(12)      WIMP             23 8
441400001227     C                   EXSR      ADGCAL
441500001227     C*
441600001227IF  1C     TASADG        IFGT      *ZEROS
441700001227     C                   Z-ADD     51            I
441800001227     C                   Z-ADD     TASADG        VIMP(I)
441900001227E   1C                   ENDIF
442000001227     C*
442100001227     C                   ENDSR
442200001227     C*------------------------------------------------------------------------*
442300001227     C* CAL068 - CALCOLA ISTAT SU ADG/CONSEGNA SUPERMERCATI
442400001227     C*------------------------------------------------------------------------*
442500001227     C     CAL068        BEGSR
442600001227     C*
442700001227     C* REIMPOSTA LE VARIABILI DI LAVORO
442800011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
442900001227     C*
443000001227     C* CALCOLA L'ISTAT
443100011031     C                   Z-ADD     VIMP(51)      WIMP             23 8
443200001227     C                   EXSR      IACCAL
443300001227     C*
443400001227IF  1C     TASIAC        IFGT      *ZEROS
443500001227     C                   Z-ADD     68            I
443600001227     C                   Z-ADD     TASIAC        VIMP(I)
443700001227E   1C                   ENDIF
443800001227     C*
443900001227     C                   ENDSR
444000970306     C*------------------------------------------------------------------------*
444100970306     C* CALCPP - CALCOLA CONSEGNA PARTICOLARE AI PIANI
444200970306     C*------------------------------------------------------------------------*
444300970306     C     CALCPP        BEGSR
444400970306     C*
444500970306     C* REIMPOSTA LE VARIABILI DI LAVORO
444600980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
444700011031     C                   Z-ADD     *ZEROS        TASCPP           21 8
444800970306     C*
444900970306     C* CALCOLA CONSEGNA PARTICOLARE
445000970306     C                   MOVEL     'P'           TASTC             1            *CODICE CONSEGNA
445100011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
445200970306     C                   EXSR      CALCP
445300970306     C                   Z-ADD     TASCP         TASCPP
445400970306     C*
445500970314IF  1C     TASCPP        IFGT      *ZEROS
445600970306     C                   Z-ADD     13            I
445700970306     C                   Z-ADD     TASCPP        VIMP(I)
445800970314E   1C                   ENDIF
445900970306     C*
446000970306     C                   ENDSR
446100001227     C*------------------------------------------------------------------------*
446200001227     C* CAL052 - CALCOLA ADG SU CONSEGNA AI PIANI
446300001227     C*------------------------------------------------------------------------*
446400001227     C     CAL052        BEGSR
446500001227     C*
446600001227     C* REIMPOSTA LE VARIABILI DI LAVORO
446700011031     C                   Z-ADD     *ZEROS        TASADG           21 8
446800001227     C*
446900001227     C* CALCOLA L'A.D.G.
447000011031     C                   Z-ADD     VIMP(13)      WIMP             23 8
447100001227     C                   EXSR      ADGCAL
447200001227     C*
447300001227IF  1C     TASADG        IFGT      *ZEROS
447400001227     C                   Z-ADD     52            I
447500001227     C                   Z-ADD     TASADG        VIMP(I)
447600001227E   1C                   ENDIF
447700001227     C*
447800001227     C                   ENDSR
447900001227     C*------------------------------------------------------------------------*
448000001227     C* CAL069 - CALCOLA ISTAT SU ADG/CONSEGNA AI PIANI
448100001227     C*------------------------------------------------------------------------*
448200001227     C     CAL069        BEGSR
448300001227     C*
448400001227     C* REIMPOSTA LE VARIABILI DI LAVORO
448500011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
448600001227     C*
448700001227     C* CALCOLA L'ISTAT
448800011031     C                   Z-ADD     VIMP(52)      WIMP             23 8
448900001227     C                   EXSR      IACCAL
449000001227     C*
449100001227IF  1C     TASIAC        IFGT      *ZEROS
449200001227     C                   Z-ADD     69            I
449300001227     C                   Z-ADD     TASIAC        VIMP(I)
449400001227E   1C                   ENDIF
449500001227     C*
449600001227     C                   ENDSR
449700040826     C*------------------------------------------------------------------------*
449800040826     C* CALdes - CALCOLA CONSEGNA destinatario disagiato
449900040826     C*------------------------------------------------------------------------*
450000040826     C     CALdes        BEGSR
450100040826     C*
450200040826     C* REIMPOSTA LE VARIABILI DI LAVORO
450300040826     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
450400040826     C                   Z-ADD     *ZEROS        TASdes           21 8
450500040826     C*
450600040826     C* CALCOLA CONSEGNA PARTICOLARE
450700040826     C                   MOVEL     'X'           TASTC             1            *CODICE CONSEGNA
450800040826     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
450900040826     C                   EXSR      CALCP
451000040826     C                   Z-ADD     TASCP         TASdes
451100040826     C*
451200040826IF  1C     TASdes        IFGT      *ZEROS
451300040826     C                   Z-ADD     96            I
451400040826     C                   Z-ADD     TASdes        VIMP(I)
451500040826E   1C                   ENDIF
451600040826     C*
451700040826     C                   ENDSR
451800040826     C*------------------------------------------------------------------------*
451900040826     C* CAL097 - CALCOLA ADG SU CONSEGNA destinatario disagiato
452000040826     C*------------------------------------------------------------------------*
452100040826     C     CAL097        BEGSR
452200040826     C*
452300040826     C* REIMPOSTA LE VARIABILI DI LAVORO
452400040826     C                   Z-ADD     *ZEROS        TASADG
452500040826     C*
452600040826     C* CALCOLA L'A.D.G.
452700040826     C                   Z-ADD     VIMP(96)      WIMP
452800040826     C                   EXSR      ADGCAL
452900040826     C*
453000040826IF  1C     TASADG        IFGT      *ZEROS
453100040826     C                   Z-ADD     97            I
453200040826     C                   Z-ADD     TASADG        VIMP(I)
453300040826E   1C                   ENDIF
453400040826     C*
453500040826     C                   ENDSR
453600040826     C*------------------------------------------------------------------------*
453700040826     C* CAL098 - CALCOLA ISTAT SU ADG/CONSEGNA destinatario disagiato
453800040826     C*------------------------------------------------------------------------*
453900040826     C     CAL098        BEGSR
454000040826     C*
454100040826     C* REIMPOSTA LE VARIABILI DI LAVORO
454200040826     C                   Z-ADD     *ZEROS        TASIAC
454300040826     C*
454400040826     C* CALCOLA L'ISTAT
454500040826     C                   Z-ADD     VIMP(97)      WIMP
454600040826     C                   EXSR      IACCAL
454700040826     C*
454800040826IF  1C     TASIAC        IFGT      *ZEROS
454900040826     C                   Z-ADD     98            I
455000040826     C                   Z-ADD     TASIAC        VIMP(I)
455100040826E   1C                   ENDIF
455200040826     C*
455300040826     C                   ENDSR
455400040826     C*------------------------------------------------------------------------*
455500040826     C* CALban - CALCOLA bancali
455600040826     C*------------------------------------------------------------------------*
455700040826     C     CALban        BEGSR
455800040826     C*
455900040826     C* REIMPOSTA LE VARIABILI DI LAVORO
456000040826     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
456100040826     C                   Z-ADD     *ZEROS        TASban           21 8
456200040826     C*
456300040826     C* CALCOLA CONSEGNA PARTICOLARE
456400040826     C                   MOVEL     '='           TASTC             1            *CODICE CONSEGNA
456500040826     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
456600040826     C                   Z-ADD     ETCnba        TOTqta           21 8          *VALORE CONTRASS.
456700040826     C                   EXSR      CALCP
456800040826     C                   Z-ADD     *ZEROS        TOTqta
456900040826     C                   Z-ADD     TASCP         TASban
457000040826     C*
457100040826IF  1C     TASban        IFGT      *ZEROS
457200040826     C                   Z-ADD     99            I
457300040826     C                   Z-ADD     TASban        VIMP(I)
457400040826E   1C                   ENDIF
457500040826     C*
457600040826     C                   ENDSR
457700040826     C*------------------------------------------------------------------------*
457800040826     C* CAL100 - CALCOLA ADG SU bancali
457900040826     C*------------------------------------------------------------------------*
458000040826     C     CAL100        BEGSR
458100040826     C*
458200040826     C* REIMPOSTA LE VARIABILI DI LAVORO
458300040826     C                   Z-ADD     *ZEROS        TASADG
458400040826     C*
458500040826     C* CALCOLA L'A.D.G.
458600040826     C                   Z-ADD     VIMP(99)      WIMP
458700040826     C                   EXSR      ADGCAL
458800040826     C*
458900040826IF  1C     TASADG        IFGT      *ZEROS
459000040826     C                   Z-ADD     100           I
459100040826     C                   Z-ADD     TASADG        VIMP(I)
459200040826E   1C                   ENDIF
459300040826     C*
459400040826     C                   ENDSR
459500040826     C*------------------------------------------------------------------------*
459600040826     C* CAL101 - CALCOLA ISTAT SU ADG bancali
459700040826     C*------------------------------------------------------------------------*
459800040826     C     CAL101        BEGSR
459900040826     C*
460000040826     C* REIMPOSTA LE VARIABILI DI LAVORO
460100040826     C                   Z-ADD     *ZEROS        TASIAC
460200040826     C*
460300040826     C* CALCOLA L'ISTAT
460400040826     C                   Z-ADD     VIMP(100)     WIMP
460500040826     C                   EXSR      IACCAL
460600040826     C*
460700040826IF  1C     TASIAC        IFGT      *ZEROS
460800040826     C                   Z-ADD     101           I
460900040826     C                   Z-ADD     TASIAC        VIMP(I)
461000040826E   1C                   ENDIF
461100040826     C*
461200040826     C                   ENDSR
461300970306     C*------------------------------------------------------------------------*
461400970306     C* CALCPF - CALCOLA CONSEGNA PARTICOLARE FUORI MISURA
461500970306     C*------------------------------------------------------------------------*
461600970306     C     CALCPF        BEGSR
461700970306     C*
461800970306     C* REIMPOSTA LE VARIABILI DI LAVORO
461900980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
462000011031     C                   Z-ADD     *ZEROS        TASCPF           21 8
462100970306     C*
462200970306     C* CALCOLA CONSEGNA PARTICOLARE
462300970306     C                   MOVEL     'F'           TASTC             1            *CODICE CONSEGNA
462400011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
462500970306     C                   EXSR      CALCP
462600970306     C                   Z-ADD     TASCP         TASCPF
462700970306     C*
462800970314IF  1C     TASCPF        IFGT      *ZEROS
462900970306     C                   Z-ADD     14            I
463000970306     C                   Z-ADD     TASCPF        VIMP(I)
463100970314E   1C                   ENDIF
463200970306     C*
463300970306     C                   ENDSR
463400001227     C*------------------------------------------------------------------------*
463500001227     C* CAL053 - CALCOLA ADG SU CONSEGNA FUORI MISURA
463600001227     C*------------------------------------------------------------------------*
463700001227     C     CAL053        BEGSR
463800001227     C*
463900001227     C* REIMPOSTA LE VARIABILI DI LAVORO
464000011031     C                   Z-ADD     *ZEROS        TASADG           21 8
464100001227     C*
464200001227     C* CALCOLA L'A.D.G.
464300011031     C                   Z-ADD     VIMP(14)      WIMP             23 8
464400001227     C                   EXSR      ADGCAL
464500001227     C*
464600001227IF  1C     TASADG        IFGT      *ZEROS
464700001227     C                   Z-ADD     53            I
464800001227     C                   Z-ADD     TASADG        VIMP(I)
464900001227E   1C                   ENDIF
465000001227     C*
465100001227     C                   ENDSR
465200001227     C*------------------------------------------------------------------------*
465300001227     C* CAL070 - CALCOLA ISTAT SU ADG/CONSEGNA FUORI MISURA
465400001227     C*------------------------------------------------------------------------*
465500001227     C     CAL070        BEGSR
465600001227     C*
465700001227     C* REIMPOSTA LE VARIABILI DI LAVORO
465800011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
465900001227     C*
466000001227     C* CALCOLA L'ISTAT
466100011031     C                   Z-ADD     VIMP(53)      WIMP             23 8
466200001227     C                   EXSR      IACCAL
466300001227     C*
466400001227IF  1C     TASIAC        IFGT      *ZEROS
466500001227     C                   Z-ADD     70            I
466600001227     C                   Z-ADD     TASIAC        VIMP(I)
466700001227E   1C                   ENDIF
466800001227     C*
466900001227     C                   ENDSR
467000970414     C*------------------------------------------------------------------------*
467100970414     C* CALCPD - CALCOLA CONSEGNA PARTICOLARE DDT
467200970414     C*------------------------------------------------------------------------*
467300970414     C     CALCPD        BEGSR
467400970414     C*
467500970414     C* REIMPOSTA LE VARIABILI DI LAVORO
467600980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
467700011031     C                   Z-ADD     *ZEROS        TASCPD           21 8
467800970414     C*
467900970414     C* CALCOLA CONSEGNA PARTICOLARE
468000970414     C                   MOVEL     'B'           TASTC             1            *CODICE CONSEGNA
468100011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
468200970414     C                   EXSR      CALCP
468300970414     C                   Z-ADD     TASCP         TASCPD
468400970414     C*
468500970414IF  1C     TASCPD        IFGT      *ZEROS
468600970414     C                   Z-ADD     33            I
468700970414     C                   Z-ADD     TASCPD        VIMP(I)
468800970414E   1C                   ENDIF
468900970414     C*
469000970414     C                   ENDSR
469100001227     C*------------------------------------------------------------------------*
469200001227     C* CAL062 - CALCOLA ADG SU CONSEGNA DDT
469300001227     C*------------------------------------------------------------------------*
469400001227     C     CAL062        BEGSR
469500001227     C*
469600001227     C* REIMPOSTA LE VARIABILI DI LAVORO
469700011031     C                   Z-ADD     *ZEROS        TASADG           21 8
469800001227     C*
469900001227     C* CALCOLA L'A.D.G.
470000011031     C                   Z-ADD     VIMP(33)      WIMP             23 8
470100001227     C                   EXSR      ADGCAL
470200001227     C*
470300001227IF  1C     TASADG        IFGT      *ZEROS
470400001227     C                   Z-ADD     62            I
470500001227     C                   Z-ADD     TASADG        VIMP(I)
470600001227E   1C                   ENDIF
470700001227     C*
470800001227     C                   ENDSR
470900001227     C*------------------------------------------------------------------------*
471000001227     C* CAL079 - CALCOLA ISTAT SU ADG/CONSEGNA DDT
471100001227     C*------------------------------------------------------------------------*
471200001227     C     CAL079        BEGSR
471300001227     C*
471400001227     C* REIMPOSTA LE VARIABILI DI LAVORO
471500011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
471600001227     C*
471700001227     C* CALCOLA L'ISTAT
471800011031     C                   Z-ADD     VIMP(62)      WIMP             23 8
471900001227     C                   EXSR      IACCAL
472000001227     C*
472100001227IF  1C     TASIAC        IFGT      *ZEROS
472200001227     C                   Z-ADD     79            I
472300001227     C                   Z-ADD     TASIAC        VIMP(I)
472400001227E   1C                   ENDIF
472500001227     C*
472600001227     C                   ENDSR
472700980706     C*------------------------------------------------------------------------*
472800980706     C* CALRIT - CALCOLA IL RITIRO
472900980706     C*------------------------------------------------------------------------*
473000980706     C     CALRIT        BEGSR
473100980706     C*
473200980706     C* REIMPOSTA LE VARIABILI DI LAVORO
473300980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
473400011031     C                   Z-ADD     *ZEROS        TASRIT           21 8
473500980706     C*
473600980706     C* CALCOLA CONSEGNA PARTICOLARE
473700980706     C                   MOVEL     'U'           TASTC             1            *CODICE CONSEGNA
473800011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
473900980706     C*
474000980706     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
474100980706     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
474200980706     C                   EXSR      CALCP
474300980706     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
474400980706     C                   Z-ADD     TASCP         TASRIT
474500980706     C*
474600980706IF  1C     TASRIT        IFGT      *ZEROS
474700980706     C                   Z-ADD     17            I
474800980706     C                   Z-ADD     TASRIT        VIMP(I)
474900980706E   1C                   ENDIF
475000980706     C*
475100980706     C                   ENDSR
475200001227     C*------------------------------------------------------------------------*
475300001227     C* CAL056 - CALCOLA ADG SU RITIRO
475400001227     C*------------------------------------------------------------------------*
475500001227     C     CAL056        BEGSR
475600001227     C*
475700001227     C* REIMPOSTA LE VARIABILI DI LAVORO
475800011031     C                   Z-ADD     *ZEROS        TASADG           21 8
475900001227     C*
476000001227     C* CALCOLA L'A.D.G.
476100011031     C                   Z-ADD     VIMP(17)      WIMP             23 8
476200001227     C                   EXSR      ADGCAL
476300001227     C*
476400001227IF  1C     TASADG        IFGT      *ZEROS
476500001227     C                   Z-ADD     56            I
476600001227     C                   Z-ADD     TASADG        VIMP(I)
476700001227E   1C                   ENDIF
476800001227     C*
476900001227     C                   ENDSR
477000001227     C*------------------------------------------------------------------------*
477100001227     C* CAL073 - CALCOLA ISTAT SU ADG/RITIRO
477200001227     C*------------------------------------------------------------------------*
477300001227     C     CAL073        BEGSR
477400001227     C*
477500001227     C* REIMPOSTA LE VARIABILI DI LAVORO
477600011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
477700001227     C*
477800001227     C* CALCOLA L'ISTAT
477900011031     C                   Z-ADD     VIMP(56)      WIMP             23 8
478000001227     C                   EXSR      IACCAL
478100001227     C*
478200001227IF  1C     TASIAC        IFGT      *ZEROS
478300001227     C                   Z-ADD     73            I
478400001227     C                   Z-ADD     TASIAC        VIMP(I)
478500001227E   1C                   ENDIF
478600001227     C*
478700001227     C                   ENDSR
478800980706     C*------------------------------------------------------------------------*
478900980706     C* CALPAM - CALCOLA LA PROVVIGIONE ASSEGNO MITTENTE
479000980706     C*------------------------------------------------------------------------*
479100980706     C     CALPAM        BEGSR
479200980706     C*
479300980706     C* REIMPOSTA LE VARIABILI DI LAVORO
479400980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
479500011031     C                   Z-ADD     *ZEROS        TASPAM           21 8
479600980706     C*
479700980706     C* CALCOLA CONSEGNA PARTICOLARE
479800980706     C                   MOVEL     'W'           TASTC             1            *CODICE CONSEGNA
479900011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
480000011031     C                   Z-ADD     ETCCAS        TOTVAL           21 8          *VALORE CONTRASS.
480100980706     C                   EXSR      CALCP
480200980911     C                   Z-ADD     *ZEROS        TOTVAL
480300980706     C                   Z-ADD     TASCP         TASPAM
480400980706     C*
480500980706IF  1C     TASPAM        IFGT      *ZEROS
480600980706     C                   Z-ADD     15            I
480700980706     C                   Z-ADD     TASPAM        VIMP(I)
480800980706E   1C                   ENDIF
480900980706     C*
481000980706     C                   ENDSR
481100001227     C*------------------------------------------------------------------------*
481200001227     C* CAL054 - CALCOLA ADG SU C/ASSEGNO MITTENTE
481300001227     C*------------------------------------------------------------------------*
481400001227     C     CAL054        BEGSR
481500001227     C*
481600001227     C* REIMPOSTA LE VARIABILI DI LAVORO
481700011031     C                   Z-ADD     *ZEROS        TASADG           21 8
481800001227     C*
481900001227     C* CALCOLA L'A.D.G.
482000011031     C                   Z-ADD     VIMP(15)      WIMP             23 8
482100001227     C                   EXSR      ADGCAL
482200001227     C*
482300001227IF  1C     TASADG        IFGT      *ZEROS
482400001227     C                   Z-ADD     54            I
482500001227     C                   Z-ADD     TASADG        VIMP(I)
482600001227E   1C                   ENDIF
482700001227     C*
482800001227     C                   ENDSR
482900001227     C*------------------------------------------------------------------------*
483000001227     C* CAL071 - CALCOLA ISTAT SU ADG/CASSEGNO MITTENTE
483100001227     C*------------------------------------------------------------------------*
483200001227     C     CAL071        BEGSR
483300001227     C*
483400001227     C* REIMPOSTA LE VARIABILI DI LAVORO
483500011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
483600001227     C*
483700001227     C* CALCOLA L'ISTAT
483800011031     C                   Z-ADD     VIMP(54)      WIMP             23 8
483900001227     C                   EXSR      IACCAL
484000001227     C*
484100001227IF  1C     TASIAC        IFGT      *ZEROS
484200001227     C                   Z-ADD     71            I
484300001227     C                   Z-ADD     TASIAC        VIMP(I)
484400001227E   1C                   ENDIF
484500001227     C*
484600001227     C                   ENDSR
484700980706     C*------------------------------------------------------------------------*
484800980706     C* CALPAB - CALCOLA LA PROVVIGIONE ASSEGNO BARTOLINI
484900980706     C*------------------------------------------------------------------------*
485000980706     C     CALPAB        BEGSR
485100980706     C*
485200980706     C* REIMPOSTA LE VARIABILI DI LAVORO
485300980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
485400011031     C                   Z-ADD     *ZEROS        TASPAB           21 8
485500980706     C*
485600980706     C* CALCOLA CONSEGNA PARTICOLARE
485700980706     C                   MOVEL     'V'           TASTC             1            *CODICE CONSEGNA
485800011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
485900980911     C                   Z-ADD     ETCCAS        TOTVAL                         *VALORE CONTRASS.
486000980706     C                   EXSR      CALCP
486100980911     C                   Z-ADD     *ZEROS        TOTVAL
486200980706     C                   Z-ADD     TASCP         TASPAB
486300980706     C*
486400980706IF  1C     TASPAB        IFGT      *ZEROS
486500980706     C                   Z-ADD     16            I
486600980706     C                   Z-ADD     TASPAB        VIMP(I)
486700980706E   1C                   ENDIF
486800980706     C*
486900980706     C                   ENDSR
487000001227     C*------------------------------------------------------------------------*
487100001227     C* CAL055 - CALCOLA ADG SU C/ASSEGNO BARTOLINI
487200001227     C*------------------------------------------------------------------------*
487300001227     C     CAL055        BEGSR
487400001227     C*
487500001227     C* REIMPOSTA LE VARIABILI DI LAVORO
487600011031     C                   Z-ADD     *ZEROS        TASADG           21 8
487700001227     C*
487800001227     C* CALCOLA L'A.D.G.
487900011031     C                   Z-ADD     VIMP(16)      WIMP             23 8
488000001227     C                   EXSR      ADGCAL
488100001227     C*
488200001227IF  1C     TASADG        IFGT      *ZEROS
488300001227     C                   Z-ADD     55            I
488400001227     C                   Z-ADD     TASADG        VIMP(I)
488500001227E   1C                   ENDIF
488600001227     C*
488700001227     C                   ENDSR
488800001227     C*------------------------------------------------------------------------*
488900001227     C* CAL072 - CALCOLA ISTAT SU ADG/CASSEGNO BARTOLINI
489000001227     C*------------------------------------------------------------------------*
489100001227     C     CAL072        BEGSR
489200001227     C*
489300001227     C* REIMPOSTA LE VARIABILI DI LAVORO
489400011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
489500001227     C*
489600001227     C* CALCOLA L'ISTAT
489700011031     C                   Z-ADD     VIMP(55)      WIMP             23 8
489800001227     C                   EXSR      IACCAL
489900001227     C*
490000001227IF  1C     TASIAC        IFGT      *ZEROS
490100001227     C                   Z-ADD     72            I
490200001227     C                   Z-ADD     TASIAC        VIMP(I)
490300001227E   1C                   ENDIF
490400001227     C*
490500001227     C                   ENDSR
490600980706     C*------------------------------------------------------------------------*
490700980706     C* CALDIF - CALCOLA IL DIRITTO FISSO
490800980706     C*------------------------------------------------------------------------*
490900980706     C     CALDIF        BEGSR
491000980706     C*
491100980706     C* REIMPOSTA LE VARIABILI DI LAVORO
491200980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
491300011031     C                   Z-ADD     *ZEROS        TASDIF           21 8
491400980706     C*
491500980706     C* CALCOLA CONSEGNA PARTICOLARE
491600980706     C                   MOVEL     'H'           TASTC             1            *CODICE CONSEGNA
491700011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
491800980706     C                   EXSR      CALCP
491900980706     C                   Z-ADD     TASCP         TASDIF
492000980706     C*
492100980706IF  1C     TASDIF        IFGT      *ZEROS
492200980706     C                   Z-ADD     2             I
492300980706     C                   Z-ADD     TASDIF        VIMP(I)
492400980706E   1C                   ENDIF
492500980706     C*
492600980706     C                   ENDSR
492700980706     C*------------------------------------------------------------------------*
492800980706     C* CALISD - CALCOLA L'ISTAT SUL DIRITTO FISSO
492900980706     C*------------------------------------------------------------------------*
493000980706     C     CALISD        BEGSR
493100980706     C*
493200980706     C* REIMPOSTA LE VARIABILI DI LAVORO
493300011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
493400980706     C*
493500980706     C* CALCOLA L'ISTAT
493600011031     C                   Z-ADD     TASDIF        WIMP             23 8
493700980706     C                   EXSR      IACCAL
493800980706     C*
493900980706IF  1C     TASIAC        IFGT      *ZEROS
494000980706     C                   Z-ADD     25            I
494100980706     C                   Z-ADD     TASIAC        VIMP(I)
494200980706E   1C                   ENDIF
494300980706     C*
494400980706     C                   ENDSR
494500980706     C*------------------------------------------------------------------------*
494600980706     C* CALIAD - CALCOLA L'ISTAT SULL'ADG DEL DIRITTO FISSO
494700980706     C*------------------------------------------------------------------------*
494800980706     C     CALIAD        BEGSR
494900980706     C*
495000980706     C* REIMPOSTA LE VARIABILI DI LAVORO
495100011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
495200980706     C*
495300980706     C* CALCOLA L'ISTAT
495400011031     C                   Z-ADD     VIMP(22)      WIMP             23 8
495500980706     C                   EXSR      IACCAL
495600980706     C*
495700980706IF  1C     TASIAC        IFGT      *ZEROS
495800980706     C                   Z-ADD     28            I
495900980706     C                   Z-ADD     TASIAC        VIMP(I)
496000980706E   1C                   ENDIF
496100980706     C*
496200980706     C                   ENDSR
496300970306     C*------------------------------------------------------------------------*
496400970306     C* CALCP - CALCOLA CONSEGNA PARTICOLARE GENERICA
496500970306     C*------------------------------------------------------------------------*
496600970306     C     CALCP         BEGSR
496700970306     C*
496800980506     C* CERCA LA TARIFFA PARTICOLARE
496900980506     C                   EXSR      CERTPT
497000980706     C*
497100980706     C* CALCOLA RPV, ARROTONDAMENTI E PESO TASSABILE
497200980706IF  1C     $TPT          IFEQ      '1'                                          *TARIFFA TROVATA
497300980707     C                   MOVEL     TPTTPG        WTPG
497400980707     C                   MOVEL     TPTRPV        WRPV
497500980707     C                   CLEAR                   WDET
497600980707     C                   Z-ADD     TPTARL        WARL
497700980707     C                   Z-ADD     TPTARF        WARF
497800980707     C                   Z-ADD     TPTARO        WARO
497900980706     C                   EXSR      CALCOL
498000040826     c                   select
498100040826IF  2C                   when      tpttpg = 'T'                                 *A TRASPORTO
498200980706     C                   Z-ADD     TOTTRA        ISOPES                         *TRASPORTO
498300040826IF  2C                   when      tpttpg = 'V'                                 *A TRASPORTO
498400980911     C                   Z-ADD     TOTVAL        ISOPES                         *TRASPORTO
498500040826IF  2C                   when      tpttpg = '5'                                 *A TRASPORTO
498600040826     C                   Z-ADD     TOTqta        ISOPES                         *TRASPORTO
498700040826E   2C                   ENDsl
498800980706     C*
498900980706     C* CALCOLA LA CONSEGNA PARTICOLARE
499000970306     C                   Z-ADD     ISOPES        TPDPES                         *PESO
499100970306     C                   EXSR      CALACD
499200980706     C                   EXSR      CALTRA                                       *CALCOLA % A TRASPOR
499300970306     C                   Z-ADD     VARIA         TASCP
499400970721     C*
499500970721     C* MEMORIZZA IL FLAG "APPLICAZIONE TARIFFA INOLTRO" DELLO SCAGLIONE DEL
499600970721     C* CODICE TASSAZIONE DEL DETTAGLIO TARIFFARIO
499700970721     C                   MOVEL     AAIN          DEPAIN
499800980506E   1C                   ENDIF
499900970306     C*
500000970306     C                   ENDSR
500100980506     C*------------------------------------------------------------------------*
500200980506     C* CERTPT - CERCA TARIFFA PARTICOLARE
500300980506     C*------------------------------------------------------------------------*
500400980506     C     CERTPT        BEGSR
500500980506     C*
500600980506     C* REPERISCE IL CODICE PARTICOLARITA'
500700980506     C                   MOVEL     '0'           $TPT                           *TARIFFA NON TROVATA
500800980506     C                   MOVEL     TASTC         KTPFTC                         *CAMPO CHIAVE
500900980506     C                   Z-ADD     1             I
501000980506     C                   SETOFF                                           28
501100990819     C                   MOVEL     KTPFTC        A1                1
501200990819     C     A1            LOOKUP    CP(I)                                  28
501300980506IF  1C     *IN28         IFEQ      *ON                                          *CODICE ESISTENTE
501400980506     C     I             OCCUR     CPIDS                                        *TARIFFA DI CARTELLO
501500980506     C*
501600980506     C* RICERCA LA CONSEGNA PARTICOLARE NELLE TARIFFE DEL CLIENTE
501700980506     C                   Z-ADD     D00KSC        KTPKSC
501800980506     C                   Z-ADD     D00CTR        KTPCTR
501900980506     C                   Z-ADD     D00PRG        KTPPRG
502000990819     C     KEYTPT        SETLL     TITPT01L                               99
502100990819     C     KEYTPT        READE     TITPT01L                               99
502200980506DO  2C     *IN99         DOWEQ     *OFF
502300980506     C     $TPT          ANDEQ     '0'
502400991125     C*
502500991125     C* CONTROLLA SE CONVERTIRE I CAMPI DI TITPT
502600011026IF  3C     D00DIV        IFNE      WDIVCE
502700011019     C                   MOVEL     D00DIV        WDVI
502800991125     C                   EXSR      CNVTPT
502900991125E   3C                   ENDIF
503000991125     C*
503100980506IF  3C     TPTATB        IFEQ      *BLANKS
503200980506     C                   MOVEL     '1'           $TPT                           *TARIFFA TROVATA
503300980506E   3C                   ENDIF
503400980506     C*
503500980506     C* LETTURA SUCESSIVA TARIFFE
503600980506IF  3C     $TPT          IFEQ      '0'                                          *TARIFFA NON TROVATA
503700990819     C     KEYTPT        READE     TITPT01L                               99
503800980506E   3C                   ENDIF
503900980506E   2C                   ENDDO                                                  *FINE TAR.PART.KSC
504000980506     C*
504100980506     C* TARIFFA CONSEGNE PARTICOLARI NON TROVATA NELLE TARIFFE DEL CLIENTE
504200980506     C* RICERCA NELLE TARIFFE DI CARTELLO
504300980506IF  2C     $TPT          IFEQ      '0'                                          *TARIFFA KSC NO TROV
504400980506IF  3C     CP88          IFNE      'NO'                                         *C'E' TARIFFA CARTEL
504500980506     C     WCAR          ANDEQ     'S'                                          *SI UTILIZZO TAR.CAR
504600980506     C                   MOVEL     '1'           $TPT                           *TARIFFA TROVATA
504700980506     C                   Z-ADD     CARKSC        TPTKSC                         *CHIAVE CARTELLO
504800980506     C                   Z-ADD     CARCTR        TPTCTR
504900980506     C                   Z-ADD     CARPRG        TPTPRG
505000980506     C                   MOVEL     KTPFTC        TPTFTC
505100980506     C                   MOVEL     TTPG8P        TPTTPG                         *CAMPI CARTELLO
505200980506     C                   Z-ADD     TARF8P        TPTARF
505300980506     C                   Z-ADD     TARL8P        TPTARL
505400980506     C                   Z-ADD     TARO8P        TPTARO
505500980506     C                   Z-ADD     TRPV8P        TPTRPV
505600980506E   3C                   ENDIF
505700980506E   2C                   ENDIF
505800980506E   1C                   ENDIF
505900980506     C*
506000980506     C                   ENDSR
506100970306     C*------------------------------------------------------------------------*
506200970306     C* CALACD - CALCOLA TARIFFA PARTICOLARE GENERICA
506300970306     C*------------------------------------------------------------------------*
506400970306     C     CALACD        BEGSR
506500980706     C*
506600980706     C* AZZERA VARIABILI DI COMODO -SOLO DI QUESTO PGM-
506700980706     C                   MOVEL     *BLANKS       PTPG              1            *TIPO APPLICAZIONE
506800011031     C                   Z-ADD     *ZEROS        PITR             21 8          *IMPORTO TARIFFA
506900011031     C                   Z-ADD     *ZEROS        PVARI            23 8          *VARIA NON ARROTONDA
507000011031     C                   Z-ADD     *ZEROS        PVARIA           23 8          *VAIRA ARROTONDATA
507100970306     C*
507200970306     C* REIMPOSTA LE VARIABILI DI LAVORO
507300011031     C                   Z-ADD     *ZEROS        VARIA            23 8
507400970306DO  1C     1             DO        10            I
507500970306     C     I             OCCUR     ACDS
507600970306     C                   CLEAR                   ACDS
507700970306E   1C                   ENDDO
507800970306     C*
507900970306     C* IMPOSTA LA CHIAVE PER LA RICERCA SCAGLIONE NEL DETTAGLIO TARIFFARIO
508000970306     C                   Z-ADD     TPTKSC        KDPKSC                         *CLIENTE
508100970306     C                   Z-ADD     TPTCTR        KDPCTR                         *TARIFFA
508200970306     C                   Z-ADD     TPTPRG        KDPPRG                         *PROGRESSIVO
508300970306     C                   MOVEL     TPTFTC        KDPFTC                         *CONSEGNA PARTICOLAR
508400970306     C                   MOVEL     *BLANKS       KDPCTS                         *CODICE TASSAZIONE
508500970306     C*
508600970306     C* PER ALCUNE CONSEGNE PARTICOLARI SONO INDICATI NELLA TABELLA CONSEGNE
508700970306     C* PARTICOLARI I CODICI TASSAZIONE DA UTILIZZARE NELLA RICERCA TARIFFA
508800970306     C                   SETOFF                                           28
508900970306     C                   Z-ADD     1             I
509000970306     C     CCTS(T)       LOOKUP    CTS(I)                                 28
509100970306     C     I             OCCUR     DSCT
509200970306IF  1C     §CTNAR        IFNE      *BLANKS                                      *TARIFFA ESTERO
509300971204     C     DLPFL1        ORNE      *BLANKS                                      *ESTERO
509400970306IF  2C     TCTE8P        IFNE      *BLANKS                                      *SE INDICATO
509500970306     C                   MOVEL     TCTE8P        KDPCTS                         *CODICE 'EE' FISSO
509600970306E   2C                   ENDIF
509700970306X   1C                   ELSE                                                   *TARIFFA ITALIA
509800970306IF  2C     TCTS8P        IFNE      *BLANKS                                      *SE INDICATO
509900970306     C                   MOVEL     TCTS8P        KDPCTS                         *CODICE 'IT' FISSO
510000970306E   2C                   ENDIF
510100970306E   1C                   ENDIF
510200970306     C*
510300970306     C* CODICE TASSAZIONE NON FISSO, RICERCA QUAL'E' PER REPERIRE LA TARIFFA
510400980921     C                   SETOFF                                       98
510500980921IF  1C     KDPCTS        IFNE      *BLANKS
510600980921     C*
510700091123     C                   IF        KDPKSC = CARKSC  and                         *LEGGERE CARTELLO
510800091123     c                             %subst(d00op0: 1: 1)='O'
510900990819     C     KEYTPD        SETLL     TIOPD02L                               98     DA OFFERTA
511000980921X   2C                   ELSE                                                   *LEGGERE CLIENTE
511100990819     C     KEYTPD        SETLL     TITPD02L                               98     DA TARIFFA
511200980921E   2C                   ENDIF
511300980921     C*
511400980921X   1C                   ELSE                                                   *KDPCTS = BLANKS
511500980921     C*
511600970306     C                   MOVEL     CCTS(T)       KDPCTS                         *CODCE TASSAZIONE
511700091123     C                   IF        KDPKSC = CARKSC  and                         *LEGGERE CARTELLO
511800091123     c                             %subst(d00op0: 1: 1)='O'
511900990819     C     KEYTPD        SETLL     TIOPD02L                               98     DA OFFERTA
512000970306X   2C                   ELSE                                                   *LEGGERE CLIENTE
512100990819     C     KEYTPD        SETLL     TITPD02L                               98     DA TARIFFA
512200970306E   2C                   ENDIF
512300970306     C*
512400970306     C* CERCA LA TARIFFA CON IL CODICE TASSAZIONE REGIONE
512500980703IF  2C     *IN98         IFEQ      *OFF                                         *TARIFFA NON TROVATA
512600970306     C                   MOVEL     §CTRAP        KDPCTS                         *COD.TASS.REGIONE
512700091123     C                   IF        KDPKSC = CARKSC  and                         *LEGGERE CARTELLO
512800091123     c                             %subst(d00op0: 1: 1)='O'
512900990819     C     KEYTPD        SETLL     TIOPD02L                               98     DA OFFERTA
513000970306X   3C                   ELSE                                                   *LEGGERE CLIENTE
513100990819     C     KEYTPD        SETLL     TITPD02L                               98     DA TARIFFA
513200970306E   3C                   ENDIF
513300970306     C*
513400970306     C* CERCA LA TARIFFA CON IL CODICE TASSAZIONE NAZIONE
513500000403IF  3C     *IN98         IFEQ      *OFF                                         *TARIFFA NON TROVATA
513600000403IF  4C     §CTNAR        IFEQ      *BLANKS
513700000403     C                   MOVEL     'IT'          KDPCTS                         *COD.TASS.NAZIONE
513800000403X   4C                   ELSE
513900000403     C                   MOVEL     'EE'          KDPCTS                         *COD.TASS.NAZIONE
514000000403E   4C                   ENDIF
514100091123     C                   IF        KDPKSC = CARKSC  and                         *LEGGERE CARTELLO
514200091123     c                             %subst(d00op0: 1: 1)='O'
514300000403     C     KEYTPD        SETLL     TIOPD02L                               98     DA OFFERTA
514400000403X   4C                   ELSE                                                   *LEGGERE CLIENTE
514500000403     C     KEYTPD        SETLL     TITPD02L                               98     DA TARIFFA
514600000403E   4C                   ENDIF
514700000403E   3C                   ENDIF
514800970306E   2C                   ENDIF
514900970306E   1C                   ENDIF
515000000403     C*
515100970306     C* LEGGE LA TARIFFA
515200970306     C                   MOVEL     '0'           $TPD                           *TARIFFA NO ESISTENT
515300970306     C                   Z-ADD     *ZEROS        I
515400091123     C                   IF        KDPKSC = CARKSC  and                         *LEGGERE CARTELLO
515500091123     c                             %subst(d00op0: 1: 1)='O'
515600990819     C     KEYTPD        READE     TIOPD02L                               98     DA OFFERTA
515700970306X   1C                   ELSE                                                   *LEGGERE CLIENTE
515800990819     C     KEYTPD        READE     TITPD02L                               98     DA TARIFFA
515900970306E   1C                   ENDIF
516000970306DO  1C     *IN98         DOWEQ     *OFF
516100991125     C*
516200991125     C* CONTROLLA SE CONVERTIRE I CAMPI DI TITPD
516300091123     C                   IF        KDPKSC = CARKSC  and                         *LEGGERE CARTELLO
516400091123     c                             %subst(d00op0: 1: 1)='O'
516500011023     C                   MOVEL     CARDIV        WDIV
516600011023     C                   ELSE
516700011019     C                   MOVEL     D00DIV        WDIV
516800011023     C                   ENDIF
516900011026IF  2C     WDIV          IFNE      WDIVCE
517000011019     C                   MOVEL     WDIV          WDVI
517100991125     C                   EXSR      CNVTPD
517200991125E   2C                   ENDIF
517300991125     C*
517400970306     C                   MOVEL     '1'           $TPD                           *TARIFFA ESISTENTE
517500970306     C                   ADD       1             I
517600970306     C     I             OCCUR     ACDS
517700970306     C                   Z-ADD     TPDSGL        ASGL
517800970306     C                   Z-ADD     TPDITR        AITR
517900970306     C                   Z-ADD     TPDMIN        AMIN
518000970306     C                   Z-ADD     TPDMAX        AMAX
518100970306     C                   MOVEL     TPDAIN        AAIN
518200091123     C                   IF        KDPKSC = CARKSC  and                         *LEGGERE CARTELLO
518300091123     c                             %subst(d00op0: 1: 1)='O'
518400990819     C     KEYTPD        READE     TIOPD02L                               98     DA OFFERTA
518500970306X   2C                   ELSE                                                   *LEGGERE CLIENTE
518600990819     C     KEYTPD        READE     TITPD02L                               98     DA TARIFFA
518700970306E   2C                   ENDIF
518800970306E   1C                   ENDDO
518900970306     C*
519000970306     C* SE TARIFFA PARTICOLARE ESISTENTE, RICERCA LO SCAGLIONE
519100970306IF  1C     $TPD          IFEQ      '1'
519200970306DO  2C     1             DO        10            I
519300970306     C     I             OCCUR     ACDS
519400970306IF  3C     ASGL          IFGE      TPDPES                                       *SCAGLIONE TROVATO
519500970306     C                   LEAVE
519600970306E   3C                   ENDIF
519700970306E   2C                   ENDDO
519800970306     C*
519900970306     C* SCAGLIONE NON TROVATO, MANCA TARIFFA
520000970306IF  2C     ASGL          IFLT      TPDPES
520100001218     C                   MOVEL     MSG(26)       DMSG26
520200001218     C                   MOVEL     TPTFTC        DM26A
520300001218     C                   MOVEL     DMSG26        $MSG
520400970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
520500970306E   2C                   ENDIF
520600970306     C*
520700970306     C* CALCOLO DEL MINIMO TASSABILE
520800980703IF  2C     WERR          IFEQ      'N'                                          *NO ERRORE
520900980703IF  3C     ASGL          IFLE      MINTAS                                       *SGL<=MIN.TASS.STD.
521000970306     C                   Z-ADD     MINTAS        TPDPES                         *PESO=MIN.TASS.STD.
521100980703E   3C                   ENDIF
521200980703IF  3C     TPDPES        IFLE      MINTAS                                       *PESO<=MIN.TASS.STD.
521300970306     C                   Z-ADD     MINTAS        TPDPES                         *PESO=MIN.TASS.STD.
521400980703E   3C                   ENDIF
521500970306     C*
521600970306     C* CALCOLA IL PESO TASSABILE IN QUINTALI -PER TARIFFA A QUINTALI-
521700011031     C                   Z-ADD     *ZEROS        TPDQLI           23 8
521800980707IF  3C     TPTTPG        IFEQ      '0'
521900980707     C     TPTTPG        OREQ      '4'
522000970306     C     TPDPES        DIV       100           TPDQLI
522100980703X   3C                   ELSE
522200970306     C                   Z-ADD     TPDPES        TPDQLI
522300980703E   3C                   ENDIF
522400970306     C     AITR          MULT(H)   TPDQLI        VARIA
522500980703IF  3C     VARIA         IFGT      *ZEROS
522600980703IF  4C     TPTTPG        IFEQ      'V'                                          *A VALORE O
522700980506     C     TPTTPG        OREQ      'T'                                          *A TRASPORTO
522800970306     C     VARIA         DIV(H)    100           VARIA
522900980703E   4C                   ENDIF
523000980703E   3C                   ENDIF
523100980703E   2C                   ENDIF
523200980706     C*
523300980706     C* IMPOSTA VARIABILI DI COMODO -SOLO DI QUESTO PGM-
523400980706     C                   MOVEL     TPTTPG        PTPG                           *TIPO APPLICAZIONE
523500980706     C                   Z-ADD     TPDITR        PITR                           *IMPORTO TARIFFA
523600980706     C                   Z-ADD     VARIA         PVARI                          *VARIA NON ARROTONDA
523700970306     C* MINIMO TARIFFA
523800980703IF  2C     AMIN          IFGT      VARIA
523900970306     C                   Z-ADD     AMIN          VARIA
524000091026     c                   clear                   tpg
524100980703E   2C                   ENDIF
524200970306     C* MASSIMO TARIFFA
524300980703IF  2C     AMAX          IFGT      *ZEROS
524400980703IF  3C     AMAX          IFLT      VARIA
524500970306     C                   Z-ADD     AMAX          VARIA
524600091026     c                   clear                   tpg
524700980703E   3C                   ENDIF
524800980703E   2C                   ENDIF
524900980706     C*
525000980707     C* IMPOSTA VARIABILI DI COMODO -SOLO DI QUESTO PGM-
525100980706     C                   Z-ADD     VARIA         PVARIA                         *VARIA ARROTONDATA
525200970306E   1C                   ENDIF
525300970306     C*
525400970306     C                   ENDSR
525500980706     C*------------------------------------------------------------------------*
525600980706     C* CALTRA - CALCOLA CONSEGNA PARTICOLARE SUL TOTALE IMPORTO TRASPORTO
525700980706     C*------------------------------------------------------------------------*
525800980706     C     CALTRA        BEGSR
525900980706     C*---
526000980706     C* INPUT
526100980706     C* . PTPG ->   TIPO APPLICAZIONE
526200980706     C* . PITR ->   IMPORTO TARIFFA CALCOLATA
526300980706     C* . PVARI ->  VARIA CALCOLATA, MA NON ARROTONDATA
526400980706     C* . PVARIA -> VARIA CALCOLATA, E ARROTONDATA
526500980706     C*---
526600980706     C* REIMPOSTA LE VARIABILI DI LAVORO
526700011031     C                   Z-ADD     *ZEROS        TOTVAR           21 8          *VARIA
526800980706     C*
526900980706     C* SE TIPO APPLICAZIONE T' E HA CALCOLATO QUALCOSA
527000980706IF  1C     PTPG          IFEQ      'T'                                          *TARIFFA A TRASPORTO
527100980706     C     PITR          ANDGT     *ZEROS                                       *TARIFFA DA CALCOLAR
527200111006     C     PVARI         ANDGT     *ZEROS                                       *TARIFFA DA CALCOLAR
527300980706     C* PORTO
527400980706IF  2C     VIMP(1)       IFGT      *ZEROS
527500011031     C                   Z-ADD     VIMP(1)       IMPOR            21 8          *IMPORTO VOCE
527600980706     C                   Z-ADD     VP(1)         PERCEN            9 5          *% INCIDENZA
527700011031     C                   Z-ADD     *ZEROS        VOCE             21 8
527800980706     C                   EXSR      CALTR2
527900980706     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
528000980706E   2C                   ENDIF
528100980706     C* INOLTRO
528200980706IF  2C     VIMP(6)       IFGT      *ZEROS
528300011031     C                   Z-ADD     VIMP(6)       IMPOR            21 8          *IMPORTO VOCE
528400980706     C                   Z-ADD     VP(6)         PERCEN            9 5          *% INCIDENZA
528500011031     C                   Z-ADD     *ZEROS        VOCE             21 8
528600980706     C                   EXSR      CALTR2
528700980706     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
528800980706E   2C                   ENDIF
528900980706     C* LOCALITA' DISAGIATE
529000980706IF  2C     VIMP(35)      IFGT      *ZEROS
529100011031     C                   Z-ADD     VIMP(35)      IMPOR            21 8          *IMPORTO VOCE
529200980706     C                   Z-ADD     VP(35)        PERCEN            9 5          *% INCIDENZA
529300011031     C                   Z-ADD     *ZEROS        VOCE             21 8
529400980706     C                   EXSR      CALTR2
529500980706     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
529600980706E   2C                   ENDIF
529700980706     C* ESPRESSO PORTO
529800980706IF  2C     VIMP(7)       IFGT      *ZEROS
529900011031     C                   Z-ADD     VIMP(7)       IMPOR            21 8          *IMPORTO VOCE
530000980706     C                   Z-ADD     VP(7)         PERCEN            9 5          *% INCIDENZA
530100011031     C                   Z-ADD     *ZEROS        VOCE             21 8
530200980706     C                   EXSR      CALTR2
530300980706     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
530400980706E   2C                   ENDIF
530500980706     C* ESPRESSO INOLTRO
530600980706IF  2C     VIMP(36)      IFGT      *ZEROS
530700011031     C                   Z-ADD     VIMP(36)      IMPOR            21 8          *IMPORTO VOCE
530800980706     C                   Z-ADD     VP(36)        PERCEN            9 5          *% INCIDENZA
530900011031     C                   Z-ADD     *ZEROS        VOCE             21 8
531000980706     C                   EXSR      CALTR2
531100980706     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
531200980706E   2C                   ENDIF
531300980706     C* ESPRESSO LOCALITA' DISAGIATE
531400980706IF  2C     VIMP(39)      IFGT      *ZEROS
531500011031     C                   Z-ADD     VIMP(39)      IMPOR            21 8          *IMPORTO VOCE
531600980706     C                   Z-ADD     VP(39)        PERCEN            9 5          *% INCIDENZA
531700011031     C                   Z-ADD     *ZEROS        VOCE             21 8
531800980706     C                   EXSR      CALTR2
531900980706     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
532000980706E   2C                   ENDIF
532100011030     C* CONSEGNA ISOLA
532200011030IF  2C     VIMP(80)      IFGT      *ZEROS
532300011031     C                   Z-ADD     VIMP(80)      IMPOR            21 8          *IMPORTO VOCE
532400011030     C                   Z-ADD     VP(80)        PERCEN            9 5          *% INCIDENZA
532500011031     C                   Z-ADD     *ZEROS        VOCE             21 8
532600011030     C                   EXSR      CALTR2
532700011030     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
532800011030E   2C                   ENDIF
532900011030     C* ESPRESSO CONSEGNA ISOLA
533000011030IF  2C     VIMP(81)      IFGT      *ZEROS
533100011031     C                   Z-ADD     VIMP(81)      IMPOR            21 8          *IMPORTO VOCE
533200011030     C                   Z-ADD     VP(81)        PERCEN            9 5          *% INCIDENZA
533300011031     C                   Z-ADD     *ZEROS        VOCE             21 8
533400011030     C                   EXSR      CALTR2
533500011030     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
533600011030E   2C                   ENDIF
533700011030     C*
533800980706     C                   Z-ADD     TOTVAR        VARIA                          *VOCE TOTALE
533900980706E   1C                   ENDIF
534000980706     C*
534100980706     C                   ENDSR
534200980706     C*------------------------------------------------------------------------*
534300980706     C* CALTR2 - ESEGUE CALCOLI VOCE CALCOLATA SU UNA % DEL TRASPORTO
534400980706     C*------------------------------------------------------------------------*
534500980706     C     CALTR2        BEGSR
534600980706     C*---
534700980706     C* INPUT
534800980706     C* . PVARI  ->  VARIA CALCOLATA, MA NON ARROTONDATA
534900980706     C* . PVARIA -> VARIA CALCOLATA, E ARROTONDATA
535000980706     C* . IMPOR  -> IMPORTO VOCE
535100980706     C* . PERCEN -> % INCIDENZA
535200980706     C* OUTPUT
535300980706     C* . VOCE   -> VOCE CALCOLATA
535400980706     C*---
535500980706     C* ES. DA CALCOLARE VARIA = 10% DEL TRASPORTO
535600980706     C*   . VARIA CALCOLATA CON ARROTONDAMENTI = 2.000
535700980706     C*   . VARIA CALCOLATA SENZA ARROTONDAMENTI = 1.600
535800980706     C*   . PORTO = 10.000                         INCIDENZA 100%
535900980706     C*   . INOLTRO = 5.000                        INCIDENZA   5%
536000980706     C*   . LOCALITA' DISAGIATA = 1.000            INCIDENZA  20%
536100980706     C*   . ESPRESSO PORTO = 300                   INCIDENZA  15%
536200980706     C*   . ESPRESSO INOLTRO = 150                 INCIDENZA 15% SU  5% = 0,75%
536300980706     C*   . ESPRESSO LOCALITA' DISAGIATA = 30      INCIDENZA 15% SU 20% = 3%
536400011030     C*   . CONSEGNA ISOLA               = 2.500   INCIDENZA  30%
536500011030     C*   . ESPRESSO CONSEGNA ISOLA      = 30      INCIDENZA 15% SU 30% = 4,5%
536600980706     C* LA SOMMA DI TUTTE LE VOCI DA LA VARIA
536700011030     C* Esempio -->
536800980706     C*---
536900980706     C* % "T" DA CALCOLARE SULLA LOCALITA' DISAGIATA
537000980706     C* 10% DI 1.000 = 100
537100980706     C*---
537200011031     C     IMPOR         MULT      PITR          N11              23 8
537300011031     C     N11           DIV       100           VOCPT            23 8          *VOCE % SUL TRASPORT
537400980706     C*---
537500980706     C* VOCE TENUTO CONTO DELL'ARROTONDAMENTO -->
537600980706     C* 100 : 1.600 = X : 2.000
537700980706     C* X = 125
537800980706     C*---
537900980706     C     VOCPT         MULT      PVARIA        VOCPT
538000011031     C     VOCPT         DIV       PVARI         VOCPTA           25 8          *VOCE AL 100%
538100980706     C*---
538200980706     C* VOCE TENUTO CONTO DELLA SUA % DI INCIDENZA ->
538300980706     C* 20% DI 125 = 25
538400980706     C*---
538500011031     C     VOCPTA        MULT      PERCEN        N19              25 8
538600011031     C     N19           DIV(H)    100           VOCE             21 8          *VOCE ALLA %INCIDENZ
538700980706     C*
538800980706     C                   ENDSR
538900970306     C*------------------------------------------------------------------------*
539000970306     C* CALTOT - CALCOLA IMPORTI SULLE % INCIDENZA E I RAGGUPPAMENTI DELTA
539100970306     C*------------------------------------------------------------------------*
539200970306     C     CALTOT        BEGSR
539300970306     C*
539400970306     C* REIMPOSTA LE VARIABILI DI LAVORO
539500970311     C                   Z-ADD     *ZEROS        IRDL                           *RAGGRUPPAMENTI DELT
539600970311     C                   Z-ADD     *ZEROS        VIM                            *IMPORTI DA SCRIVERE
539700011031     C                   Z-ADD     *ZEROS        WVVOCE           25 8          *IMPORTO VOCE
539800970306     C*
539900970311     C* CICLO PER TUTTE LE VOCI DI TASSAZIONE
540000970306DO  1C     1             DO        NVOCA         V                 6 0
540100970311IF  2C     VIMP(V)       IFGT      *ZEROS
540200970306     C*
540300970311     C* APPLICA LA % DI INCIDENZA
540400970306     C                   Z-ADD     *ZEROS        WVVOCE
540500970306IF  3C     VP(V)         IFLT      100
540600970311     C     VIMP(V)       DIV       100           WVVOCE
540700970306     C     WVVOCE        MULT      VP(V)         VIM(V)
540800970311X   3C                   ELSE
540900970311     C                   Z-ADD     VIMP(V)       VIM(V)
541000970306E   3C                   ENDIF
541100970306     C*
541200970306     C* SE LA VOCE E' DA INGLOBARE NEL PORTO FORZA IL RAGGRUPPAMENTO PORTO
541300970306IF  3C     VI(V)         IFEQ      'S'
541400970306     C                   Z-ADD     001           VR(V)
541500970318E   3C                   ENDIF
541600970306     C*
541700970311     C* IMPOSTA IL RAGGRUPPAMENTO DELTA
541800970306     C                   Z-ADD     VR(V)         R                 6 0          *INDICE  RAGGRUPP.TO
541900970306     C                   ADD       VIM(V)        IRDL(R)                        *IMPORTO RAGGRUPP.TO
542000970306E   2C                   ENDIF
542100970306E   1C                   ENDDO                                                  *FINE VOCI
542200970306     C*
542300970306     C                   ENDSR
542400970306     C*------------------------------------------------------------------------*
542500970306     C* SCRETW - SCRIVE SU FILE DI LAVORO I RICAVI
542600970306     C*------------------------------------------------------------------------*
542700970306     C     SCRETW        BEGSR
542800970311     C*
542900970311     C* CICLO PER TUTTI I RAGGRUPPAMENTI DELTA
543000970306DO  1C     1             DO        50            R
543100970306IF  2C     IRDL(R)       IFGT      *ZEROS
543200970306     C     R             OREQ      1
543300980909     C******************** MOVELUSER      KTWUSR           *UTENTE
543400980909     C                   MOVEL     JOB           KTWUSR                         *UTENTE
543500091123     C                   MOVEL     'S'           KTWDSF                         *DATI SEDE O FILIALE
543600980603     C                   MOVEL     D00CTO        KTWCTO                         *TARIFFA O OFFERTA
543700970306     C                   Z-ADD     D00KSC        KTWKSC                         *CLIENTE O VISITA
543800970306     C                   Z-ADD     D00CTR        KTWCTR                         *TARIFFA
543900970306     C                   Z-ADD     D00PRG        KTWPRG                         *PROGRESSIVO
544000970306     C                   Z-ADD     SSGL(S)       KTWSGL                         *SCAGLIONE
544100970306     C                   MOVEL     CCTS(T)       KTWCTS                         *CODICE TASSAZIONE
544200970306     C                   Z-ADD     R             KTWRDL                         *RAGGRUP.DELTA
544300010926     C     KEYETW        CHAIN     TEETW01L                           99
544400970306     C* MODIFICA
544500970306IF  3C     *IN99         IFEQ      *OFF
544600970306     C                   ADD       IRDL(R)       ETWRIC                         *RICAVO
544700970317IF  4C     D00TLA        IFEQ      'D'                                          *LANCIO DELTA
544800970317     C                   Z-ADD     *ZEROS        ETWCOM                         *AZZERA COMPETENZE
544900970317E   4C                   ENDIF
545000010926     C                   UPDATE    TEETW000
545100970306     C* INSERIMENTO
545200970306X   3C                   ELSE
545300970306     C                   MOVEL     KTWUSR        ETWUSR                         *UTENTE
545400980603     C                   MOVEL     KTWDSF        ETWDSF                         *DATI SEDE O FILIALE
545500970306     C                   MOVEL     KTWCTO        ETWCTO                         *TARIFFA O OFFERTA
545600970306     C                   Z-ADD     KTWKSC        ETWKSC                         *CLIENTE O VISITA
545700970306     C                   Z-ADD     KTWCTR        ETWCTR                         *TARIFFA
545800970306     C                   Z-ADD     KTWPRG        ETWPRG                         *PROGRESSIVO
545900970306     C                   Z-ADD     KTWSGL        ETWSGL                         *SCAGLIONE
546000970306     C                   MOVEL     KTWCTS        ETWCTS                         *CODICE TASSAZIONE
546100970306     C                   Z-ADD     KTWRDL        ETWRDL                         *RAGGRUP.DELTA
546200970306     C                   Z-ADD     IRDL(R)       ETWRIC                         *RICAVO
546300011031     C     CPPE(T)       DIV       100           WPERC            21 8
546400970306     C     WPERC         MULT      SPPE(S)       ETWPPE                         *% INCIDENZA
546500970306     C                   MOVEL     CRCT(T)       ETWRCT                         *RAGGRUPP.COD.TASSAZ
546600970306     C                   MOVEL     CRAP(T)       ETWRAP                         *REGIONE CODICE
546700970306     C                   MOVEL     CNAR(T)       ETWNAR                         *NAZIONE CODICE
546800970306     C                   Z-ADD     *ZEROS        ETWCOM                         *COMPETENZE
546900010926     C                   WRITE     TEETW000
547000970306E   3C                   ENDIF
547100970306E   2C                   ENDIF
547200970306E   1C                   ENDDO                                                  *FINE RAGGRUPPAMENTI
547300970306     C*
547400970306     C                   ENDSR
547500970306     C*------------------------------------------------------------------------*
547600970306     C* SCREWR - SCRIVE SU FILE DI LAVORO IL DETTAGLIO DEI RICAVI -SE IN DEBUG-
547700970306     C*------------------------------------------------------------------------*
547800970306     C     SCREWR        BEGSR
547900970311     C*
548000970311     C* CICLO PER TUTTE LE VOCI DI TASSAZIONE
548100970306DO  1C     1             DO        NVOCA         V
548200970306IF  2C     VIM(V)        IFGT      *ZEROS
548300980909     C******************** MOVELUSER      EWRUSR           *UTENTE
548400980909     C                   MOVEL     JOB           EWRUSR                         *UTENTE
548500091123     C                   MOVEL     'S'           EWRDSF                         *DATI SEDE O FILIALE
548600970306     C                   MOVEL     D00CTO        EWRCTO                         *TARIFFA O OFFERTA
548700970306     C                   Z-ADD     D00KSC        EWRKSC                         *CLIENTE
548800970306     C                   Z-ADD     D00CTR        EWRCTR                         *TARIFFA
548900970306     C                   Z-ADD     D00PRG        EWRPRG                         *PROGRESSIVO
549000970306     C                   MOVEL     CCTS(T)       EWRCTS                         *CODICE TASSAZIONE
549100970306     C                   Z-ADD     SSGL(S)       EWRSGL                         *SCAGLIONE
549200970306     C                   Z-ADD     V             EWRVOC                         *VOCE
549300970306     C                   Z-ADD     VIM(V)        EWRIMP                         *IMPORTO VOCE
549400970306     C                   Z-ADD     VR(V)         EWRRDL                         *RAGGRUPPAMENTO DELT
549500010926     C                   WRITE     TEETWR00
549600970306E   2C                   ENDIF
549700970306E   1C                   ENDDO                                                  *FINE VOCI
549800970306     C*
549900970306     C                   ENDSR
550000970306     C*------------------------------------------------------------------------*
550100970306     C* CANCWF - CANCELLA IL FILE DI LAVORO
550200970306     C*------------------------------------------------------------------------*
550300970306     C     CANCWF        BEGSR
550400970317     C*
550500010926     C* CANCELLA I RECORD DELL'UTENTE NEL FILE TEETW00F
550600970317IF  1C     D00TLA        IFEQ      'D'                                          *LANCIO DELTA
550700980909     C******************** MOVELUSER      KTWUSR
550800980909     C                   MOVEL     JOB           KTWUSR
550900010926     C     KE2ETW        SETLL     TEETW01L
551000010926     C     KE2ETW        READE     TEETW01L                               99
551100970317DO  2C     *IN99         DOWEQ     *OFF
551200010926     C                   DELETE    TEETW000
551300010926     C     KE2ETW        READE     TEETW01L                               99
551400970317E   2C                   ENDDO
551500970317E   1C                   ENDIF
551600970306     C*
551700010926     C* CANCELLA I RECORD DELL'UTENTE NEL FILE TEETWR0F
551800980909     C******************** MOVELUSER      KWRUSR
551900980909     C                   MOVEL     JOB           KWRUSR
552000010926     C     KEYEWR        SETLL     TEETWR1L
552100010926     C     KEYEWR        READE     TEETWR1L                               99
552200970306DO  1C     *IN99         DOWEQ     *OFF
552300010926     C                   DELETE    TEETWR00
552400010926     C     KEYEWR        READE     TEETWR1L                               99
552500970306E   1C                   ENDDO
552600970306     C*
552700970306     C                   ENDSR
552800970306     C*------------------------------------------------------------------------*
552900970306     C* CARTAB - CARICA LE TABELLE
553000970306     C*------------------------------------------------------------------------*
553100970306     C     CARTAB        BEGSR
553200970306     C*
553300970306     C* CARICAMENTO DATI VARI TASSAZIONE -UNO-
553400970306     C                   EXSR      CARQT1
553500970306     C*
553600970306     C* CARICAMENTO TIPI TARIFFA
553700970306     C                   EXSR      CARTR
553800970306     C*
553900970306     C* CARICAMENTO SIGLE VARIE
554000970306     C                   EXSR      CAR$2
554100970306     C*
554200970306     C* CARICAMENTO CODICI TASSAZIONE
554300970306     C                   EXSR      CARCT
554400970306     C*
554500970306     C* CARICAMENTO TIPI SERVIZIO
554600970306     C                   EXSR      CAR5E
554700970306     C*
554800970306     C* CARICAMENTO TARIFFE PARTICOLARI -DI CARTELLO-
554900970306     C                   EXSR      CARPAR
555000970306     C*
555100970306     C                   ENDSR
555200970306     C*------------------------------------------------------------------------*
555300970306     C* CARQT1 - CARICAMENTO TABELLA DATI VARI DI TASSAZIONE -UNO-
555400970306     C*------------------------------------------------------------------------*
555500970306     C     CARQT1        BEGSR
555600021202     C*
555700021202     C                   CLEAR                   TRUL27DS
555800021202     C                   EVAL      I27TLA = 'L'
555900021202     C                   EVAL      I27DTA = DATCOR
556000021202     C                   CALL      'TRUL27R'
556100021202     C                   PARM                    TRUL27DS
556200021202     C                   IF        O27ERR = 'E'
556300021202     C                   MOVEL     MSG(11)       $MSG
556400021202     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
556500021202     C                   ENDIF
556600970306     C*
556700970306     C                   ENDSR
556800970306     C*------------------------------------------------------------------------*
556900970306     C* CARTR - CARICAMENTO TIPI TARIFFA
557000970306     C*------------------------------------------------------------------------*
557100970306     C     CARTR         BEGSR
557200970306     C*
557300980506DO  1C     1             DO        15            I
557400970306     C     I             OCCUR     DSTR
557500970306     C                   CLEAR                   DSTR                           *AZZERAMENTO
557600970306E   1C                   ENDDO
557700980707     C                   MOVEL     *BLANKS       CTR
557800970306     C*
557900970306     C                   Z-ADD     *ZEROS        I
558000020906     C                   Z-ADD     1             KTBKUT
558100970306     C                   MOVEL     'TR'          KTBCOD
558200970306     C     KE2TAB        SETLL     TABEL00F
558300970306     C     KE2TAB        READE     TABEL00F                               98
558400970306IF  1C     *IN98         IFEQ      *ON
558500970306     C                   MOVEL     MSG(15)       $MSG
558600970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
558700970306X   1C                   ELSE
558800970306DO  2C     *IN98         DOWEQ     *OFF
558900970306IF  3C     TBLKEY        IFNE      *BLANKS
559000970312IF  4C     TBLFLG        IFEQ      *BLANKS
559100970306     C                   ADD       1             I
559200970306     C                   MOVEL     TBLKEY        CTR(I)
559300970306     C     I             OCCUR     DSTR
559400970306     C                   MOVEL     TBLUNI        DSTR
559500970312E   4C                   ENDIF
559600970312E   3C                   ENDIF
559700970306     C     KE2TAB        READE     TABEL00F                               98
559800970306E   2C                   ENDDO
559900970306E   1C                   ENDIF
560000970306     C*
560100970306     C                   ENDSR
560200970306     C*------------------------------------------------------------------------*
560300970306     C* CAR$2 - CARICAMENTO SIGLE VARIE
560400970306     C*------------------------------------------------------------------------*
560500970306     C     CAR$2         BEGSR
560600970306     C*
560700970306     C                   CLEAR                   DS$2                           *AZZERAMENTO
560800970306     C*
560900020906     C                   Z-ADD     1             KTBKUT
561000970306     C                   MOVEL     '$2'          KTBCOD
561100970306     C                   MOVEL(P)  '1'           KTBKEY
561200970306     C     KEYTAB        CHAIN     TABEL00F                           98
561300970306IF  1C     *IN98         IFEQ      *ON
561400970306     C                   MOVEL     MSG(16)       $MSG
561500970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
561600970306X   1C                   ELSE
561700970306     C                   MOVEL     TBLUNI        DS$2
561800970306E   1C                   ENDIF
561900970306     C*
562000970306     C                   ENDSR
562100970306     C*------------------------------------------------------------------------*
562200970306     C* CARCT - CARICAMENTO CODICI TASSAZIONE
562300970306     C*------------------------------------------------------------------------*
562400970306     C     CARCT         BEGSR
562500970306     C*
562600050103DO  1C     1             DO        600           I
562700970306     C     I             OCCUR     DSCT
562800970306     C                   CLEAR                   DSCT                           *AZZERAMENTO
562900970306E   1C                   ENDDO
563000970306     C*
563100970306     C                   Z-ADD     *ZEROS        I
563200020906     C                   Z-ADD     1             KTBKUT
563300970306     C                   MOVEL     'CT'          KTBCOD
563400970306     C     KE2TAB        SETLL     TABEL00F
563500970306     C     KE2TAB        READE     TABEL00F                               98
563600970306IF  1C     *IN98         IFEQ      *ON
563700970306     C                   MOVEL     MSG(18)       $MSG
563800970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
563900970306X   1C                   ELSE
564000970306DO  2C     *IN98         DOWEQ     *OFF
564100970306IF  3C     TBLKEY        IFNE      *BLANKS
564200970312IF  4C     TBLFLG        IFEQ      *BLANKS
564300970306     C                   ADD       1             I
564400970306     C                   MOVEL     TBLKEY        CTS(I)
564500970306     C     I             OCCUR     DSCT
564600970306     C                   MOVEL     TBLUNI        DSCT
564700970312IF  5C     §CTUTC        IFEQ      'N'                                          *DA NON CONSIDERARE
564800970312     C                   CLEAR                   DSCT                           *PULISCE
564900970312     C     I             SUB       1             I                              *TORNA INDIETRO DI 1
565000970312E   5C                   ENDIF
565100970312E   4C                   ENDIF
565200970312E   3C                   ENDIF
565300970306     C     KE2TAB        READE     TABEL00F                               98
565400970306E   2C                   ENDDO
565500970306E   1C                   ENDIF
565600970306     C*
565700970306     C                   ENDSR
565800970306     C*------------------------------------------------------------------------*
565900970306     C* CAR5E - CARICAMENTO TIPI SERVIZIO
566000970306     C*------------------------------------------------------------------------*
566100970306     C     CAR5E         BEGSR
566200970306     C*
566300970306DO  1C     1             DO        10            I
566400970306     C     I             OCCUR     DS5E
566500970306     C                   CLEAR                   DS5E                           *AZZERAMENTO
566600970306E   1C                   ENDDO
566700970306     C*
566800970306     C                   Z-ADD     *ZEROS        I
566900020906     C                   Z-ADD     1             KTBKUT
567000970306     C                   MOVEL     '5E'          KTBCOD
567100970306     C     KE2TAB        SETLL     TABEL00F
567200970306     C     KE2TAB        READE     TABEL00F                               98
567300970306IF  1C     *IN98         IFEQ      *ON
567400970306     C                   MOVEL     MSG(19)       $MSG
567500970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
567600970306X   1C                   ELSE
567700970306DO  2C     *IN98         DOWEQ     *OFF
567800970306IF  3C     TBLKEY        IFNE      *BLANKS
567900970312IF  4C     TBLFLG        IFEQ      *BLANKS
568000970306     C                   ADD       1             I
568100970306     C                   MOVEL     TBLKEY        TS(I)
568200970306     C     I             OCCUR     DS5E
568300970306     C                   MOVEL     TBLUNI        DS5E
568400970312E   4C                   ENDIF
568500970312E   3C                   ENDIF
568600970306     C     KE2TAB        READE     TABEL00F                               98
568700970306E   2C                   ENDDO
568800970306E   1C                   ENDIF
568900970306     C*
569000970306     C                   ENDSR
569100970306     C*------------------------------------------------------------------------*
569200970306     C* CARPAR - CARICAMENTO TARIFFE PARTICOLARI -DI CARTELLO-
569300970306     C*------------------------------------------------------------------------*
569400970306     C     CARPAR        BEGSR
569500970306     C*---
569600970306     C* MEMORIZZA I DATI ASSICURAZIONE X CONTO / ISOLA / CONSEGNE PARTICOLARI
569700970306     C* DELLA TARIFFA DI CARTELLO IN VIGORE ALLA DATA DI FATTURAZIONE
569800970306     C*---
569900021202     C                   Z-ADD     O27KSC        KSC1                           *IMPOSTA CARTELLO
570000970306     C                   Z-ADD     *ZEROS        CTR1
570100011019     C                   EXSR      CERTAM                                       *CERCA TARIFFA
570200970306     C*
570300970306     C* MEMORIZZA LA CHIAVE DELLA TARIFFA DI CARTELLO IN VIGORE
570400970306IF  1C     WERR          IFEQ      'N'                                          *NO ERRORE
570500970306     C                   Z-ADD     TAMKSC        CARKSC
570600970306     C                   Z-ADD     TAMCTR        CARCTR
570700970306     C                   Z-ADD     TAMPRG        CARPRG
570800011019     C*
570900011023     C* RECUPERA LA DIVISA DELLA TARIFFA DI CARTELLO
571000011023     C                   CLEAR                   DSTA01
571100011023     C                   MOVEL     TAMFLO        DSTA01
571200011023     C                   MOVEL     §TADIV        CARDIV            3            *DIVISA TARIFFA
571300970306     C*
571400970306     C* CARICA LE TARIFFE PARTICOLARI DI CARTELLO
571500970306     C                   EXSR      CAR1P
571600970306X   1C                   ELSE                                                   *ERRORE
571700970306     C                   MOVEL     MSG(22)       $MSG
571800970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
571900970306E   1C                   ENDIF
572000970306     C*
572100970306     C                   ENDSR
572200970306     C*------------------------------------------------------------------------*
572300970306     C* CERTAM - CERCA UN TARIFFA
572400970306     C*------------------------------------------------------------------------*
572500970306     C     CERTAM        BEGSR
572600970306     C*
572700970306     C* REIMPOSTA LE VARIABILI DI LAVORO
572800970306     C                   Z-ADD     *ZEROS        DEPKSC                         *CAMPI DEPOSITO
572900970306     C                   Z-ADD     *ZEROS        DEPPRG
573000970306     C                   Z-ADD     *ZEROS        DEPCTR
573100970306     C                   MOVEL     '0'           $TAR              1            TARIFFA IN DECORRENZ
573200970306     C                   MOVEL     '0'           $SCA              1            TARIFFA SCADUTA
573300970306     C                   MOVEL     'N'           WERR                           *NO ERRORE
573400970306     C*
573500970306     C                   Z-ADD     KSC1          KTMKSC                         *CLIENTE
573600970306     C                   Z-ADD     CTR1          KTMCTR                         *TARIFFA
573700091123     c                   if        %subst(d00op0: 1: 1)='O'
573800970306     C     KEYTAM        SETLL     TNOFM00L                               99
573900970306X   1C                   ELSE                                                   *TARIFFE
574000970306     C     KEYTAM        SETLL     TNTAM00L                               99
574100970306E   1C                   ENDIF
574200970306IF  1C     *IN99         IFEQ      *OFF                                         *NON TROVATA
574300970306     C                   MOVEL     'S'           WERR                           *MANCA TARIFFA
574400970306X   1C                   ELSE
574500091123     c                   if        %subst(d00op0: 1: 1)='O'
574600970306     C     KEYTAM        READE     TNOFM00L                               99
574700970306X   2C                   ELSE                                                   *TARIFFE
574800970306     C     KEYTAM        READE     TNTAM00L                               99
574900970306E   2C                   ENDIF
575000970306DO  2C     *IN99         DOWEQ     *OFF                                         *FINE FILE
575100970306     C     $TAR          ANDEQ     '0'                                          *TROVATA TAR.IN DEC.
575200970306     C     WERR          ANDEQ     'N'                                          *NO MANCA TARIFFA
575300970306IF  3C     TAMATB        IFEQ      *BLANKS                                      *NO ANNULLATE
575400970306IF  4C     D00DTC        IFGE      TAMDDT                                       TARIFFA IN DECORRENZ
575500970306     C     D00DTC        ANDLE     TAMDST
575600970306     C                   MOVEL     '1'           $TAR                           *TAR.IN DEC.TROVATA
575700970306     C                   Z-ADD     TAMKSC        DEPKSC                         *CHIAVE DEPOSITATA
575800970306     C                   Z-ADD     TAMCTR        DEPCTR
575900970306     C                   Z-ADD     TAMPRG        DEPPRG
576000970306X   4C                   ELSE                                                   TARIFFA NON IN DECOR
576100970306IF  5C     D00DTC        IFLT      TAMDDT                                       *TARIFFA DA DECORRER
576200970306IF  6C     $SCA          IFEQ      '0'                                          *NON ESISTE TAR.SCAD
576300970306     C                   MOVEL     'S'           WERR                           *MANCA TARIFFA
576400970306E   6C                   ENDIF
576500970306E   5C                   ENDIF
576600970306IF  5C     D00DTC        IFGT      TAMDST                                       *TARIFFA SCADUTA
576700970306     C                   MOVEL     '1'           $SCA                           *TAR.SCADUTA TROVATA
576800970306     C                   Z-ADD     TAMKSC        DEPKSC                         *CHIAVE DEPOSITATA
576900970306     C                   Z-ADD     TAMCTR        DEPCTR
577000970306     C                   Z-ADD     TAMPRG        DEPPRG
577100970306E   5C                   ENDIF
577200970306E   4C                   ENDIF
577300970306E   3C                   ENDIF
577400091123     c                   if        %subst(d00op0: 1: 1)='O'
577500970306     C     KEYTAM        READE     TNOFM00L                               99
577600970306X   3C                   ELSE                                                   *TARIFFE
577700970306     C     KEYTAM        READE     TNTAM00L                               99
577800970306E   3C                   ENDIF
577900970306E   2C                   ENDDO                                                  *FINE TARIFFE
578000970306E   1C                   ENDIF
578100970306     C                   Z-ADD     DEPKSC        TAMKSC                         *CHIAVE DEPOSITATA
578200970306     C                   Z-ADD     DEPCTR        TAMCTR
578300970306     C                   Z-ADD     DEPPRG        TAMPRG
578400970306     C*
578500970306     C                   ENDSR
578600970306     C*------------------------------------------------------------------------*
578700970306     C* CAR1P - CARICAMENTO TARIFFE PARTICOLARI DI CARTELLO
578800970306     C*------------------------------------------------------------------------*
578900970306     C     CAR1P         BEGSR
579000970306     C*
579100020104DO  1C     1             DO        50            I
579200970306     C     I             OCCUR     CPIDS
579300970306     C                   CLEAR                   CPIDS                          *AZZERAMENTO
579400970306     C     I             OCCUR     DS1P
579500970306     C                   CLEAR                   DS1P                           *AZZERAMENTO
579600970306E   1C                   ENDDO
579700970306     C                   MOVEL     *BLANKS       CP
579800970306     C*
579900970306     C* PER CIASCUNA TARIFFA PARTICOLARE IN TABELLA
580000970306     C                   Z-ADD     *ZEROS        I
580100020906     C                   Z-ADD     1             KTBKUT
580200970306     C                   MOVEL     '1P'          KTBCOD
580300970306     C     KE2TAB        SETLL     TABEL00F
580400970306     C     KE2TAB        READE     TABEL00F                               98
580500970306IF  1C     *IN98         IFEQ      *ON
580600970306     C                   MOVEL     MSG(20)       $MSG
580700970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
580800970306X   1C                   ELSE
580900970306DO  2C     *IN98         DOWEQ     *OFF
581000970306IF  3C     TBLKEY        IFNE      *BLANKS
581100970312IF  4C     TBLFLG        IFEQ      *BLANKS
581200970306     C                   MOVEL     TBLUNI        CAMPO            26
581300970306     C                   MOVE      CAMPO         WFCP              1
581400970312IF  5C     WFCP          IFEQ      'S'                                          *TARIFFE PART.DA FAT
581500970306     C                   ADD       1             I
581600970306     C                   MOVEL     TBLKEY        CP(I)
581700970306     C     I             OCCUR     DS1P
581800970306     C                   MOVEL     TBLUNI        DS1P
581900970306     C*
582000970306     C* CARICA LE TARIFFE PARTICOLARI DI CARTELLO
582100970306     C                   MOVEL     '0'           $TPT              1            *TAR.PARTIC.TROVATA
582200011023     C                   Z-ADD     TAMKSC        KTPKSC
582300970306     C                   Z-ADD     TAMCTR        KTPCTR
582400970306     C                   Z-ADD     TAMPRG        KTPPRG
582500970306     C                   MOVEL     CP(I)         KTPFTC
582600970311     C     I             OCCUR     CPIDS
582700091123     c                   if        %subst(d00op0: 1: 1)='O'
582800990819     C     KEYTPT        SETLL     TIOPT01L                               99
582900990819     C     KEYTPT        READE     TIOPT01L                               99
583000970312X   6C                   ELSE                                                   *TARIFFE
583100990819     C     KEYTPT        SETLL     TITPT01L                               99
583200990819     C     KEYTPT        READE     TITPT01L                               99
583300970312E   6C                   ENDIF
583400970312DO  6C     *IN99         DOWEQ     *OFF                                         *FINE FILE
583500970306     C     $TPT          ANDEQ     '0'                                          *TROVATA TARIFFA
583600011023     C*
583700011023     C                   MOVEL     CARDIV        WDIV
583800991125     C*
583900991125     C* CONTROLLA SE CONVERTIRE I CAMPI DI TITPT
584000011026IF  7C     WDIV          IFNE      WDIVCE
584100011023     C                   MOVEL     WDIV          WDVI
584200011023     C                   EXSR      CNVTPT
584300011023E   7C                   ENDIF
584400991125     C*
584500970312IF  7C     TPTATB        IFEQ      *BLANKS
584600970306     C                   MOVEL     '1'           $TPT                           *TAR.PARTIC.TROVATA
584700970306     C                   Z-ADD     TPTRPV        TRPV8P
584800970306     C                   Z-ADD     TPTARF        TARF8P
584900970306     C                   Z-ADD     TPTARL        TARL8P
585000970306     C                   Z-ADD     TPTARO        TARO8P
585100970306     C                   MOVEL     TPTTPG        TTPG8P
585200970306     C                   MOVEL     TPTAPL        TAPL8P
585300970306     C                   MOVEL     §1PCTS        TCTS8P
585400970306     C                   MOVEL     §1PCTE        TCTE8P
585500970312E   7C                   ENDIF
585600091123     c                   if        %subst(d00op0: 1: 1)='O'
585700990819     C     KEYTPT        READE     TIOPT01L                               99
585800970312X   7C                   ELSE                                                   *TARIFFE
585900990819     C     KEYTPT        READE     TITPT01L                               99
586000970312E   7C                   ENDIF
586100970312E   6C                   ENDDO                                                  *FINE TAR.PARTICOLAR
586200970312IF  6C     $TPT          IFEQ      '0'                                          *TARIFFA NON TROVATA
586300970306     C                   MOVEL     'NO'          CP88
586400970312E   6C                   ENDIF
586500970312E   5C                   ENDIF
586600970312E   4C                   ENDIF
586700970306E   3C                   ENDIF
586800970306     C     KE2TAB        READE     TABEL00F                               98
586900970306E   2C                   ENDDO
587000970306E   1C                   ENDIF
587100970306     C*
587200970306     C                   ENDSR
587300970715     C*------------------------------------------------------------------------*
587400970715     C* REPINP - RECUPERA INTRADAMENTO -PARTENZA-
587500970715     C*------------------------------------------------------------------------*
587600970715     C     REPINP        BEGSR
587700970715     C*
587800970715     C* TERMINAL PARTENZA
587900970715     C                   CLEAR                   DSLV55
588000970715     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
588100970715     C                   MOVEL     'P'           D55TPT                         *ECCEZIONE ARRIVO
588200970715     C                   Z-ADD     D00LNP        D55LIN                         *LINEA
588300970715     C                   Z-ADD     D00DTC        D55DRF                         *DATA RIFERIMENTO
588400970715     C                   CALL      'FNLV55R'
588500970715     C                   PARM                    DSLV55
588600970715IF  1C     D55ERR        IFNE      *BLANKS                                      *NON TROVATO
588700970715     C                   MOVEL     D55MSG        $MSG
588800970715     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
588900970715X   1C                   ELSE                                                   *TROVATO
589000970715     C                   Z-ADD     D55TFP        WTFP                           *TERMINAL PARTENZA
589100970715E   1C                   ENDIF
589200970715     C*
589300970715     C* CODICE TASSAZIONE PARTENZA
589400970715     C                   Z-ADD     D00LNP        KORFIL
589500970715     C     KEYORG        CHAIN     AZORG01L                           98
589600970715IF  1C     *IN98         IFEQ      *ON                                          *NON TROVATO
589700970715     C                   MOVEL     MSG(5)        $MSG
589800970715     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
589900970715X   1C                   ELSE                                                   *TROVATA
590000970721     C                   MOVEL     ORGCTS        WMCT                           *CTS MITTENTE
590100020729     C                   MOVEL     ORGDE3        OG143
590200020729     C*
590300020729     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
590400020729     C                   CLEAR                   DNTW
590500020729     C*
590600020729     C                   MOVEL(P)  'NTW'         KTECOD
590700020729     C                   MOVEL(P)  §OGNTW        KTEKE1
590800020729     C     KEYTBE        CHAIN     TNTBE01L
590900020729     C                   IF        %found(TNTBE01L)
591000020729     C                   MOVEL     TBEUNI        DNTW
591100020729     C                   ENDIF
591200020729     C*
591300020729     C                   MOVEL     §NTWFIE       DLPFL1                         *FLAG ITALIA/ESTERO
591400970715E   1C                   ENDIF
591500970715     C*
591600970715     C                   ENDSR
591700970306     C*------------------------------------------------------------------------*
591800970306     C* CARETC - CARICA I DATI TIPO -GENERALI-
591900970306     C*------------------------------------------------------------------------*
592000970306     C     CARETC        BEGSR
592100970306     C*
592200091123     C                   MOVEL     'S'           KTCDSF                         *DATI SEDE O FILIALE
592300970306     C                   MOVEL     D00CTO        KTCCTO                         *TARIFFA O OFFERTA
592400970306     C                   Z-ADD     D00KSC        KTCKSC                         *CLIENTE O VISITA
592500970306     C                   Z-ADD     D00CTR        KTCCTR                         *TARIFFA
592600970306     C                   Z-ADD     D00PRG        KTCPRG                         *PROGRESSIVO
592700970306     C                   Z-ADD     D00LNP        KTCLNP                         *LINEA DI PARTENZA
592800010926     C     KEYETC        CHAIN     TEETC01L                           99
592900970306IF  1C     *IN99         IFEQ      *ON                                          *NON TROVATO
593000970306     C     ETCATB        ORNE      *BLANKS                                      *NO ANNULLATI
593100970306     C                   MOVEL     MSG(1)        $MSG
593200970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
593300011008     C                   ELSE
593400011026     C     D00DIV        IFNE      WDIVCE
593500011019     C                   MOVEL     D00DIV        WDVI
593600011008     C                   EXSR      CNVETC
593700011008     C                   ENDIF
593800970306E   1C                   ENDIF
593900970306     C*
594000970306     C                   ENDSR
594100970306     C*------------------------------------------------------------------------*
594200970306     C* CARETS - CARICA I DATI TIPO -SCAGLIONI-
594300970306     C*------------------------------------------------------------------------*
594400970306     C     CARETS        BEGSR
594500970306     C*
594600970306     C                   MOVEL     '0'           $SGL              1            *SCAGLIONI ESISTENTI
594700970306     C                   Z-ADD     *ZEROS        I                 6 0
594800091123     C                   MOVEL     'S'           KTSDSF                         *DATI SEDE O FILIALE
594900970306     C                   MOVEL     D00CTO        KTSCTO                         *TARIFFA O OFFERTA
595000970306     C                   Z-ADD     D00KSC        KTSKSC                         *CLIENTE O VISITA
595100970306     C                   Z-ADD     D00CTR        KTSCTR                         *TARIFFA
595200970306     C                   Z-ADD     D00PRG        KTSPRG                         *PROGRESSIVO
595300970306     C                   Z-ADD     D00LNP        KTSLNP                         *LINEA DI PARTENZA
595400010926     C     KE2ETS        SETLL     TEETS01L
595500010926     C     KE2ETS        READE     TEETS01L                               99
595600970306DO  1C     *IN99         DOWEQ     *OFF
595700970306IF  2C     ETSATB        IFEQ      *BLANKS                                      *NO ANNULLATI
595800011026     C     D00DIV        IFNE      WDIVCE
595900011019     C                   MOVEL     D00DIV        WDVI
596000011008     C                   EXSR      CNVETS
596100011008     C                   ENDIF
596200970306     C                   MOVEL     '1'           $SGL
596300970306     C                   ADD       1             I
596400970306     C                   Z-ADD     ETSSGL        SSGL(I)                        *SCAGLIONE SUPERIORE
596500970306     C                   Z-ADD     ETSPSC        SPSC(I)                        *PESO RIFERIMENTO
596600970306     C                   Z-ADD     ETSCSC        SCSC(I)                        *COLLI RIFERIMENTO
596700970306     C                   Z-ADD     ETSVSC        SVSC(I)                        *VOLUME RIFERIMENTO
596800970318     C                   Z-ADD     ETSINL        SINL(I)                        *INOLTRO
596900970318     C                   Z-ADD     ETSRPV        SRPV(I)                        *RAPPORTO PESO/VOLUM
597000970306     C                   Z-ADD     ETSPPE        SPPE(I)                        *% INCIDENZA
597100970414     C                   MOVEL     ETSSDE        SSDE(I)                        *SEGNO DELTA
597200970414     C                   Z-ADD     ETSPDE        SPDE(I)                        *% DELTA
597300970306E   2C                   ENDIF
597400010926     C     KE2ETS        READE     TEETS01L                               99
597500970306E   1C                   ENDDO
597600970306     C                   Z-ADD     I             WULTSC            6 0
597700970306IF  1C     $SGL          IFEQ      '0'                                          *NON TROVATO
597800970306     C                   MOVEL     MSG(2)        $MSG
597900970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
598000970306E   1C                   ENDIF
598100970306     C*
598200970306     C                   ENDSR
598300970306     C*------------------------------------------------------------------------*
598400970306     C* CARETT - CARICA I DATI TIPO -CODICI TASSAZIONE-
598500970306     C*------------------------------------------------------------------------*
598600970306     C     CARETT        BEGSR
598700970306     C*
598800970306     C                   MOVEL     '0'           $CTS              1            *COD.TASS. ESISTENTI
598900970306     C                   Z-ADD     *ZEROS        I                 6 0
599000091123     C                   MOVEL     'S'           KTTDSF                         *DATI SEDE O FILIALE
599100970306     C                   MOVEL     D00CTO        KTTCTO                         *TARIFFA O OFFERTA
599200970306     C                   Z-ADD     D00KSC        KTTKSC                         *CLIENTE O VISITA
599300970306     C                   Z-ADD     D00CTR        KTTCTR                         *TARIFFA
599400970306     C                   Z-ADD     D00PRG        KTTPRG                         *PROGRESSIVO
599500970306     C                   Z-ADD     D00LNP        KTTLNP                         *LINEA DI PARTENZA
599600970306     C                   MOVEL     D00FIE        KTTFIE                         *ITALIA/ESTERO
599700011017     C     KE2ETT        SETLL     TEETT01L
599800011017     C     KE2ETT        READE     TEETT01L                               99
599900970306DO  1C     *IN99         DOWEQ     *OFF
600000970306IF  2C     ETTRCT        IFNE      *BLANKS                                      *DA CALCOLARE
600100970306IF  3C     ETTATB        IFEQ      *BLANKS                                      *NO ANNULLATI
600200970306     C                   MOVEL     '1'           $CTS
600300970306     C                   ADD       1             I
600400970306     C                   MOVEL     ETTCTS        CCTS(I)                        *CODICE TASSAZIONE
600500970306     C                   MOVEL     ETTRCT        CRCT(I)                        *RAGGRUPPAMENTO
600600970306     C                   Z-ADD     ETTPPE        CPPE(I)                        *% INCIDENZA
600700970306     C*
600800970715     C* RECUPERA INSTRADAMENTO -ARRIVO-
600900020906     C                   Z-ADD     1             KTBKUT
601000970306     C                   MOVEL     'CT'          KTBCOD
601100970306     C                   MOVEL(P)  ETTCTS        KTBKEY
601200970306     C     KEYTAB        CHAIN     TABEL00F                           98
601300970306IF  4C     *IN98         IFEQ      *ON                                          *NON TROVATO
601400970306     C                   MOVEL     MSG(4)        $MSG
601500970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
601600970306X   4C                   ELSE                                                   *TROVATO
601700970306     C                   MOVEL     TBLUNI        DSCT
601800970306     C                   MOVEL     §CTRAP        CRAP(I)                        *REGIONE APPARTENENZ
601900970306     C     §CTNAR        IFEQ      *BLANKS
602000970306     C                   MOVEL     'IT'          CNAR(I)                        *NAZIONE APPARTENENZ
602100970306     C                   ELSE
602200970306     C                   MOVEL     'EE'          CNAR(I)
602300970306     C                   ENDIF
602400011030     C                   MOVEL     §CT§§5        C§§5(I)                        *FLAG ISOLA/NONISOLA
602500970715     C* LINEA ARRIVO
602600970715     C                   CLEAR                   DSSI95
602700970715     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
602800970715     C                   MOVEL     '3'           I95TCN                         *CONTROLLO ESISTENZA
602900970715     C                   MOVEL     §CTNAR        I95NAR                         *NAZIONE
603000970715     C                   MOVEL     §CTCAP        I95CAP                         *CAP
603100970715     C                   Z-ADD     WTFP          I95TFP                         *TERMINAL PARTENZA
603200980602     C                   MOVEL     'S'           I95FRE                         *CONTROLLA ECCEZIONI
603300970715     C                   Z-ADD     D00DTC        I95DAT                         *DATA RIFERIMENTO
603400970715     C                   CALL      'TISI95R'
603500970715     C                   PARM                    DSSI95
603600970715IF  5C     O95ERR        IFNE      *BLANKS                                      *NON TROVATO
603700970715     C                   MOVEL     O95MSG        $MSG
603800970715     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
603900970715X   5C                   ELSE                                                   *TROVATO
604000970715     C                   Z-ADD     O95LNA        CLNA(I)                        *LINEA ARRIVO
604100971204     C* FLAG ITALIA/ESTERO
604200971204     C                   Z-ADD     CLNA(I)       KORFIL
604300971204     C     KEYORG        CHAIN     AZORG01L                           98
604400020729     C                   MOVEL     ORGDE3        OG143
604500971204IF  6C     *IN98         IFEQ      *OFF                                         *NON TROVATO
604600020729     C*
604700020729     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
604800020729     C                   CLEAR                   DNTW
604900020729     C*
605000020729     C                   MOVEL(P)  'NTW'         KTECOD
605100020729     C                   MOVEL(P)  §OGNTW        KTEKE1
605200020729     C     KEYTBE        CHAIN     TNTBE01L
605300020729     C                   IF        %found(TNTBE01L)
605400020729     C                   MOVEL     TBEUNI        DNTW
605500020729     C                   ENDIF
605600020729     C*
605700020729     C                   MOVEL     §NTWFIE       CFL1(I)                        *FLAG ITALIA/ESTERO
605800971204E   6C                   ENDIF
605900971204     C*
606000970715     C* TERMINAL ARRIVO
606100970715     C                   CLEAR                   DSLV55
606200970715     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
606300970715     C                   MOVEL     'A'           D55TPT                         *ECCEZIONE ARRIVO
606400970715     C                   Z-ADD     O95LNA        D55LIN                         *LINEA
606500970715     C                   Z-ADD     D00DTC        D55DRF                         *DATA RIFERIMENTO
606600970715     C                   Z-ADD     D00LNP        D55LNP                         *LINEA PARTENZA
606700970715     C                   CALL      'FNLV55R'
606800970715     C                   PARM                    DSLV55
606900970715IF  6C     D55ERR        IFNE      *BLANKS                                      *NON TROVATO
607000970715     C                   MOVEL     D55MSG        $MSG
607100970715     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
607200970715X   6C                   ELSE                                                   *TROVATO
607300970715     C                   Z-ADD     D55TFA        CTFA(I)                        *TERMINAL ARRIVO
607400970715E   6C                   ENDIF
607500970715E   5C                   ENDIF
607600970306E   4C                   ENDIF
607700970306E   3C                   ENDIF
607800970306E   2C                   ENDIF
607900011017     C     KE2ETT        READE     TEETT01L                               99
608000970306E   1C                   ENDDO                                                  *FINE COD.TASSAZIONE
608100970306     C                   Z-ADD     I             WULTCT            6 0
608200970306IF  1C     $CTS          IFEQ      '0'                                          *NON TROVATO
608300970306     C                   MOVEL     MSG(3)        $MSG
608400970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
608500970306E   1C                   ENDIF
608600970715     C*
608700970306     C                   ENDSR
608800970306     C*------------------------------------------------------------------------*
608900970306     C* CARTAM - CARICAMENTO TARIFFA CLIENTE -TESTATA-
609000970306     C*------------------------------------------------------------------------*
609100970306     C     CARTAM        BEGSR
609200970306     C*
609300970306     C                   Z-ADD     D00KSC        KTMKSC                         *CLIENTE
609400970306     C                   Z-ADD     D00CTR        KTMCTR                         *TARIFFA
609500970306     C                   Z-ADD     D00PRG        KTMPRG                         *PROGRESSIVO
609600091201     C     KE2TAM        CHAIN     TNTAM00L                           99        *ESISTE PER FORZA
609700040826     C                   MOVEL     TAMFLO        DSTA01
609800970306     C*
609900970306     C* APPLICAZIONE TARIFFA FINITA
610000011031     C                   Z-ADD     *ZEROS        MINTAS           23 8
610100011031     C                   Z-ADD     *ZEROS        MINTAR           23 8
610200970306     C                   MOVEL     D00CTR        UNOCTR            1 0          *TIPO TARIFFA
610300980707     C                   MOVEL     UNOCTR        WTPG              1
610400970306     C                   SETOFF                                           28
610500970306     C                   Z-ADD     1             I
610600980707     C     WTPG          LOOKUP    CTR(I)                                 28
610700970306IF  1C     *IN28         IFEQ      *ON
610800970306     C     I             OCCUR     DSTR
610900970306     C                   Z-ADD     §TRATA        MINTAS                         *MINIMO TASSABILE ST
611000970306E   1C                   ENDIF
611100980422     C                   Z-ADD     §TRATA        MINTAR
611200970306IF  1C     TAMATA        IFGT      *ZEROS                                       *APPLICAZ.TAR.FINITA
611300970306     C                   Z-ADD     TAMATA        MINTAR                         *MINIMO APPLICAZ. KS
611400970306E   1C                   ENDIF
611500970306     C*
611600980506     C* DEPOSITA I DATI DELLA DSTR DELLA STATA TARIFFA
611700980506     C                   Z-ADD     MINTAS        DMITAS                         *MINIMO TASSABILE ST
611800980506     C                   MOVEL     §TRARR        DTRARR                         *ARROTONDAM ABILE ST
611900980506     C                   MOVEL     §TRRPV        DTRRPV                         *RPV        ABILE ST
612000971022     C*
612100970306     C                   ENDSR
612200991125     C*------------------------------------------------------------------------*
612300011008     C* CNVTAD - CONVERTE I CAMPI DI TITAM IN DIVISA DI CALCOLO DEL CE
612400991125     C*------------------------------------------------------------------------*
612500991125     C     CNVTAD        BEGSR
612600991125     C* PORTO
612700991125     C                   Z-ADD     TADITR        WIMI             30 9
612800991125     C                   EXSR      CNVEUR
612900991125     C                   Z-ADD     WIMO          TADITR
613000991125     C* INOLTRO
613100991125     C                   Z-ADD     TADINO        WIMI             30 9
613200991125     C                   EXSR      CNVEUR
613300991125     C                   Z-ADD     WIMO          TADINO
613400991125     C*
613500991125     C                   ENDSR
613600991125     C*------------------------------------------------------------------------*
613700011008     C* CNVTPT - CONVERTE I CAMPI DI TITPT IN DIVISA DI CALCOLO DEL CE
613800991125     C*------------------------------------------------------------------------*
613900991125     C     CNVTPT        BEGSR
614000991125     C*
614100991125     C* ALCUNI CAMPI SONO DA CONVERTIRE SOLO SE CONTENGONO IMPORTI IN MONETA
614200991125IF  1C     TPTTPG        IFEQ      '4'                                          *QUANTITA'
614300991125     C     TPTTPG        OREQ      'T'                                          *TRASPORTO
614400991125     C     TPTTPG        OREQ      'V'                                          *VALORE
614500991125     C*
614600991125     C* ARROTONDAMENTO LIMITE
614700991125     C                   Z-ADD     TPTARL        WIMI             30 9
614800991125     C                   EXSR      CNVEUR
614900991125     C                   Z-ADD     WIMO          TPTARL
615000991125     C*
615100991125     C* ARROTONDAMENTO FINO LIMITE
615200991125     C                   Z-ADD     TPTARF        WIMI             30 9
615300991125     C                   EXSR      CNVEUR
615400991125     C                   Z-ADD     WIMO          TPTARF
615500991125     C*
615600991125     C* ARROTONDAMENTO OLTRE LIMITE
615700991125     C                   Z-ADD     TPTARO        WIMI             30 9
615800991125     C                   EXSR      CNVEUR
615900991125     C                   Z-ADD     WIMO          TPTARO
616000991125E   1C                   ENDIF
616100991125     C* VALORE MERCE
616200991125     C                   Z-ADD     TPTVLM        WIMI             30 9
616300991125     C                   EXSR      CNVEUR
616400991125     C                   Z-ADD     WIMO          TPTVLM
616500991125     C*
616600991125     C                   ENDSR
616700991125     C*------------------------------------------------------------------------*
616800011008     C* CNVTPD - CONVERTE I CAMPI DI TITPD IN DIVISA DI CALCOLO DEL CE
616900991125     C*------------------------------------------------------------------------*
617000991125     C     CNVTPD        BEGSR
617100991125     C*
617200991125     C* ALCUNI CAMPI SONO DA CONVERTIRE SOLO SE CONTENGONO IMPORTI IN MONETA
617300991125IF  1C     TPTTPG        IFEQ      '4'                                          *QUANTITA'
617400991125     C     TPTTPG        OREQ      'T'                                          *TRASPORTO
617500991125     C     TPTTPG        OREQ      'V'                                          *VALORE
617600991125     C* SCAGLIONE
617700991125     C                   Z-ADD     TPDSGL        N5                5 0
617800991125IF  2C     N5            IFNE      99999                                        *SCAGLIONE 99999=OLT
617900991125     C                   Z-ADD     TPDSGL        WIMI             30 9
618000991125     C                   EXSR      CNVEUR
618100991125     C                   Z-ADD     WIMO          TPDSGL
618200991125E   2C                   ENDIF
618300991125E   1C                   ENDIF
618400991125     C*
618500991125     C* ALCUNI CAMPI NON SONO DA CONVERTIRE PERCHE' RAPPRESENTANO PERCENTUALI
618600991125IF  1C     TPTTPG        IFNE      '4'                                          *QUANTITA'
618700991125     C     TPTTPG        ANDNE     'T'                                          *TRASPORTO
618800991125     C     TPTTPG        ANDNE     'V'                                          *VALORE
618900991125     C* TARIFFA
619000991125     C                   Z-ADD     TPDITR        WIMI             30 9
619100991125     C                   EXSR      CNVEUR
619200991125     C                   Z-ADD     WIMO          TPDITR
619300991125E   1C                   ENDIF
619400991125     C* MINIMO
619500991125     C                   Z-ADD     TPDMIN        WIMI             30 9
619600991125     C                   EXSR      CNVEUR
619700991125     C                   Z-ADD     WIMO          TPDMIN
619800991125     C* MASSIMO
619900991125     C                   Z-ADD     TPDMAX        WIMI             30 9
620000991125     C                   EXSR      CNVEUR
620100991125     C                   Z-ADD     WIMO          TPDMAX
620200991125     C*
620300991125     C                   ENDSR
620400991125     C*------------------------------------------------------------------------*
620500011008     C* CNVTGC - CONVERTE I CAMPI DI TITGC IN DIVISA DI CALCOLO DEL CE
620600991125     C*------------------------------------------------------------------------*
620700991125     C     CNVTGC        BEGSR
620800991125     C*
620900991125     C* SPESE VARIE
621000991125     C                   Z-ADD     TGCSGV        WIMI             30 9
621100991125     C                   EXSR      CNVEUR
621200991125     C                   Z-ADD     WIMO          TGCSGV
621300991125     C* SPESE RICONSEGNA CITTA'
621400991125     C                   Z-ADD     TGCSGR        WIMI             30 9
621500991125     C                   EXSR      CNVEUR
621600991125     C                   Z-ADD     WIMO          TGCSGR
621700991125     C* SPESE RICONSEGNA PROVINCIA
621800991125     C                   Z-ADD     TGCSGP        WIMI             30 9
621900991125     C                   EXSR      CNVEUR
622000991125     C                   Z-ADD     WIMO          TGCSGP
622100991125     C* SPESE DOSSIER
622200991125     C                   Z-ADD     TGCSGD        WIMI             30 9
622300991125     C                   EXSR      CNVEUR
622400991125     C                   Z-ADD     WIMO          TGCSGD
622500991125     C* 1° TARIFFA SOSTA
622600991125     C                   Z-ADD     TTS(1)        WIMI             30 9
622700991125     C                   EXSR      CNVEUR
622800991125     C                   Z-ADD     WIMO          TTS(1)
622900991125     C* 2° TARIFFA SOSTA
623000991125     C                   Z-ADD     TTS(2)        WIMI             30 9
623100991125     C                   EXSR      CNVEUR
623200991125     C                   Z-ADD     WIMO          TTS(2)
623300991125     C* 3° TARIFFA SOSTA
623400991125     C                   Z-ADD     TTS(3)        WIMI             30 9
623500991125     C                   EXSR      CNVEUR
623600991125     C                   Z-ADD     WIMO          TTS(3)
623700991125     C* TARIFFA MINIMA SOSTA
623800991125     C                   Z-ADD     TGCSTM        WIMI             30 9
623900991125     C                   EXSR      CNVEUR
624000991125     C                   Z-ADD     WIMO          TGCSTM
624100991125     C*
624200991125     C                   ENDSR
624300011008     C*------------------------------------------------------------------------*
624400011023     C* CNVETC - CONVERTE I CAMPI DI TEETC IN DIVISA DI CALCOLO DEL CE
624500011008     C*------------------------------------------------------------------------*
624600011008     C     CNVETC        BEGSR
624700011008     C* ARROTONDAMENTO
624800011008     C                   Z-ADD     ETCIAR        WIMI             30 9
624900011008     C                   EXSR      CNVEUR
625000011008     C                   Z-ADD     WIMO          ETCIAR
625100011008     C* CONTRASSEGNO
625200011008     C                   Z-ADD     ETCCAS        WIMI             30 9
625300011008     C                   EXSR      CNVEUR
625400011008     C                   Z-ADD     WIMO          ETCCAS
625500011008     C* ASSICURAZIONE
625600011008     C                   Z-ADD     ETCASC        WIMI             30 9
625700011008     C                   EXSR      CNVEUR
625800011008     C                   Z-ADD     WIMO          ETCASC
625900011008     C*
626000011008     C                   ENDSR
626100011008     C*------------------------------------------------------------------------*
626200011008     C* CNVETS - CONVERTE I CAMPI DI TEETS IN DIVISA DI CALCOLO DEL CE
626300011008     C*------------------------------------------------------------------------*
626400011008     C     CNVETS        BEGSR
626500011008     C* INOLTRO
626600011008     C                   Z-ADD     ETSINL        WIMI             30 9
626700011008     C                   EXSR      CNVEUR
626800011008     C                   Z-ADD     WIMO          ETSINL
626900011008     C*
627000011008     C                   ENDSR
627100991125     C*------------------------------------------------------------------------*
627200011008     C* CNVEUR - CONVERTE I CAMPI IN DIVISA DI CALCOLO DEL CE
627300991125     C*------------------------------------------------------------------------*
627400991125     C     CNVEUR        BEGSR
627500991125     C*
627600991125     C* AZZERA VARIABILI DI LAVORO
627700991125     C                   Z-ADD     *ZEROS        WIMO             30 9          *IMPORTO CONVERTITO
627800011008     C                   MOVEL     *BLANKS       NDEC              1            *N. DEC IN OUTPUT
627900011008     C*
628000011026     C* IMPOSTO I DECIMALI DI OUTPUT A SECONDA DELLA DIVISA DI OUTPUT (NON + VERO: SEMPREE MAX DEC)
628100011026     C*                  IF        §GEDCEDIV = 'ITL'
628200011026     C*                  EVAL      NDEC = '0'
628300011026     C*                  ELSE
628400011026     C*                  EVAL      NDEC = '3'
628500011026     C*                  ENDIF
628600011026     C                   EVAL      NDEC = '9'
628700991125     C*
628800991125IF  1C     WIMI          IFNE      *ZEROS
628900991125     C                   CLEAR                   YEUR
629000991125     C                   MOVEL     WDVI          YECDVI                         *DIVISA IMPORTO
629100991125     C                   Z-ADD     WIMI          YECIMI                         *IMPORTO DA CONVERTI
629200011026     C                   MOVEL     WDIVCE        YECDVO                         *DIVISA IMP CONVERTI
629300011008     C                   MOVEL     NDEC          YECDCO                         *N° DECIMALI
629400991125     C                   MOVEL     *BLANKS       YECTAR                         *ARROTONDAMENTO "H"
629500991125     C                   CALL      'YEURCO'
629600991125     C                   PARM                    YEUR
629700991125IF  2C     YECESI        IFEQ      ' '                                          *NO ERRORI
629800991125     C                   Z-ADD     YECIMO        WIMO                            -IMPORTO CONVERTITO
629900991125X   2C                   ELSE                                                   *SI ERRORI
630000991125     C                   Z-ADD     WIMI          WIMO                            -IMPOROTO DA CONVER
630100991125E   2C                   ENDIF
630200991125E   1C                   ENDIF
630300991125     C*
630400991125     C                   ENDSR
630500970306     C*------------------------------------------------------------------------*
630600970306     C* IMPERR - IMPOSTA ERRORE NELLA DS DI PROCEDURA
630700970306     C*------------------------------------------------------------------------*
630800970306     C     IMPERR        BEGSR
630900970306     C*
631000970306     C                   MOVEL     'S'           WERR                           *ERRORE
631100970306     C                   MOVEL     $MSG          D00MSG                         *MESSAGGIO PROCEDURA
631200970306     C                   MOVEL     '1'           D00ERR                         *ERRORE PROCEDURA
631300970306     C*
631400970306     C                   ENDSR
631500970715     C*------------------------------------------------------------------------*
631600970715     C* FINSR - OPERAZIONI FINALI
631700970715     C*------------------------------------------------------------------------*
631800970715     C     FINSR         BEGSR
631900970715     C*
632000970715     C* CHIUDE I PROGRAMMA DI UTILITA' RICHIAMATI
632100970715     C                   CLEAR                   DSSI95                         *CONTROLLA CAP
632200970715     C                   MOVEL     'C'           I95TLA
632300970715     C                   CALL      'TISI95R'
632400970715     C                   PARM                    DSSI95
632500970715     C*
632600970715     C                   CLEAR                   DSLV55                         *RECUPERA TERMINAL
632700970715     C                   MOVEL     'C'           D55TLA
632800970715     C                   CALL      'FNLV55R'
632900970715     C                   PARM                    DSLV55
633000970715     C*
633100970715     C                   ENDSR
633200011008     C*------------------------------------------------------------------------*
633300011008     C* CARGEDCE - CARICAMENTO TABELLA "GED" TIPO RECORD "CE" E CONSIDERAZIONI SU DIVISE
633400011008     C*------------------------------------------------------------------------*
633500011008     C     CARGEDCE      BEGSR
633600011008     C*
633700011008     C                   CLEAR                   DSGEDCE
633800011026     C                   MOVEL     *blanks       WDIVCE            3
633900011008     C*
634000011008     C                   MOVEL(P)  'GED'         KTECOD
634100011008     C                   MOVEL(P)  'CE'          KTEKE1
634200011008     C     KEYTBE        CHAIN     TNTBE01L
634300011008     C                   IF        %found(TNTBE01L)
634400011008     C                   MOVEL     TBEUNI        DSGEDCE
634500011008     C                   ENDIF
634600011008     C*
634700011008     C                   IF        §GEDCEDAT > D00DTC
634800011026     C                   EVAL      WDIVCE = §GEDCEDIP
634900011026     C                   ELSE
635000011026     C                   EVAL      WDIVCE = §GEDCEDIV
635100011008     C                   ENDIF
635200011008     C*
635300011008     C                   ENDSR
635400040716     C*------------------------------------------------------------------------*
635500040716     C* CARGEIEUR- CARICAMENTO TABELLA "GEI" TIPO RECORD "EUR"
635600040716     C*------------------------------------------------------------------------*
635700040716     C     CARGEIEUR     BEGSR
635800040716     C*
635900040716     C                   CLEAR                   DGEI
636000040716     C*
636100040716     C                   MOVEL(P)  'GEI'         KTECOD
636200040716     C                   MOVEL(P)  'EUR'         KTEKE1
636300040716     C     KEYTBE        CHAIN     TNTBE01L
636400040716     C                   IF        %found(TNTBE01L)
636500040716     C                   MOVEL     TBEUNI        DGEI
636600040716     C                   ENDIF
636700040716     C*
636800040716     C                   ENDSR
636900970306     C*------------------------------------------------------------------------*
637000970306     C* *INZSR - OPERAZIONI INIZIALI
637100970306     C*------------------------------------------------------------------------*
637200970306     C     *INZSR        BEGSR
637300970306     C*--------------------
637400970306     C* RICEVIMENTO PARAMETRI
637500970306     C*--------------------
637600970306     C     *ENTRY        PLIST
637700970306     C                   PARM                    KPJBA
637800970306     C                   PARM                    DSTE00
637900041202     c* apertura controllata di alcuni file xchè solo di filiale
638000041202     c                   open      tiogc01l                             95
638100041202     c                   open      tiopd02l                             95
638200041202     c                   open      tiopt01l                             95
638300041202     c                   open      tnofm00l                             95
638400970306     C*--------------------
638500970306     C* VARIABILI RIFERITE AL DATA BASE
638600970306     C*--------------------
638700010926     C     *LIKE         DEFINE    ETCDSF        KTCDSF                         *TEETC00F
638800980603     C     *LIKE         DEFINE    ETCCTO        KTCCTO
638900970306     C     *LIKE         DEFINE    ETCKSC        KTCKSC
639000970306     C     *LIKE         DEFINE    ETCCTR        KTCCTR
639100970306     C     *LIKE         DEFINE    ETCPRG        KTCPRG
639200970306     C     *LIKE         DEFINE    ETCLNP        KTCLNP
639300010926     C     *LIKE         DEFINE    ETSDSF        KTSDSF                         *TEETC00F
639400980603     C     *LIKE         DEFINE    ETSCTO        KTSCTO
639500970306     C     *LIKE         DEFINE    ETSKSC        KTSKSC
639600970306     C     *LIKE         DEFINE    ETSCTR        KTSCTR
639700970306     C     *LIKE         DEFINE    ETSPRG        KTSPRG
639800970306     C     *LIKE         DEFINE    ETSLNP        KTSLNP
639900970306     C     *LIKE         DEFINE    ETSSGL        KTSSGL
640000010926     C     *LIKE         DEFINE    ETTDSF        KTTDSF                         *TEETC00F
640100980603     C     *LIKE         DEFINE    ETTCTO        KTTCTO
640200970306     C     *LIKE         DEFINE    ETTKSC        KTTKSC
640300970306     C     *LIKE         DEFINE    ETTCTR        KTTCTR
640400970306     C     *LIKE         DEFINE    ETTPRG        KTTPRG
640500970306     C     *LIKE         DEFINE    ETTLNP        KTTLNP
640600970306     C     *LIKE         DEFINE    ETTFIE        KTTFIE
640700970306     C     *LIKE         DEFINE    ETTCTS        KTTCTS
640800010926     C     *LIKE         DEFINE    ETWUSR        KTWUSR                         TEETW00F
640900980603     C     *LIKE         DEFINE    ETWDSF        KTWDSF
641000980603     C     *LIKE         DEFINE    ETWCTO        KTWCTO
641100970306     C     *LIKE         DEFINE    ETWKSC        KTWKSC
641200970306     C     *LIKE         DEFINE    ETWCTR        KTWCTR
641300970306     C     *LIKE         DEFINE    ETWPRG        KTWPRG
641400970306     C     *LIKE         DEFINE    ETWSGL        KTWSGL
641500970306     C     *LIKE         DEFINE    ETWCTS        KTWCTS
641600970306     C     *LIKE         DEFINE    ETWRDL        KTWRDL
641700010926     C     *LIKE         DEFINE    EWRUSR        KWRUSR                         TEETWR0F
641800970306     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
641900970306     C     *LIKE         DEFINE    TBLCOD        KTBCOD
642000970306     C     *LIKE         DEFINE    TBLKEY        KTBKEY
642100970306     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
642200970306     C     *LIKE         DEFINE    TAMKSC        KTMKSC                         *TNTAM00F
642300970306     C     *LIKE         DEFINE    TAMCTR        KTMCTR
642400970306     C     *LIKE         DEFINE    TAMPRG        KTMPRG
642500990819     C     *LIKE         DEFINE    TADKSC        KTDKSC                         *TITAD00F
642600970306     C     *LIKE         DEFINE    TADPRG        KTDPRG
642700970306     C     *LIKE         DEFINE    TADLNP        KTDLNP
642800970306     C     *LIKE         DEFINE    TADCTS        KTDCTS
642900990819     C     *LIKE         DEFINE    TADNAZ        KTDNAZ
643000990819     C     *LIKE         DEFINE    TADCAP        KTDCAP
643100970306     C     *LIKE         DEFINE    TADCTR        KTDCTR
643200990819     C     *LIKE         DEFINE    TPTKSC        KTPKSC                         *TITPT00F
643300970306     C     *LIKE         DEFINE    TPTCTR        KTPCTR
643400970306     C     *LIKE         DEFINE    TPTPRG        KTPPRG
643500970306     C     *LIKE         DEFINE    TPTFTC        KTPFTC
643600990819     C     *LIKE         DEFINE    TPDKSC        KDPKSC                         *TITPD00F
643700970306     C     *LIKE         DEFINE    TPDCTR        KDPCTR
643800970306     C     *LIKE         DEFINE    TPDPRG        KDPPRG
643900970306     C     *LIKE         DEFINE    TPDFTC        KDPFTC
644000970306     C     *LIKE         DEFINE    TPDCTS        KDPCTS
644100990819     C     *LIKE         DEFINE    TGCKSC        KGCKSC                         *TITGC00F
644200970306     C     *LIKE         DEFINE    TGCCTR        KGCCTR
644300970306     C     *LIKE         DEFINE    TGCPRG        KGCPRG
644400970306     C     *LIKE         DEFINE    TAMKSC        CARKSC                         *DI DEPOSITO
644500970306     C     *LIKE         DEFINE    TAMCTR        CARCTR
644600970306     C     *LIKE         DEFINE    TAMPRG        CARPRG
644700970306     C     *LIKE         DEFINE    TAMKSC        DEPKSC
644800970306     C     *LIKE         DEFINE    TAMCTR        DEPPRG
644900970306     C     *LIKE         DEFINE    TAMPRG        DEPCTR
645000970721     C     *LIKE         DEFINE    TPDAIN        DEPAIN
645100970721     C     *LIKE         DEFINE    TPDAIN        CPLAIN
645200011030     C     *LIKE         DEFINE    TPDAIN        CISAIN
645300011031     C     *LIKE         DEFINE    TPDAIN        SAMAIN
645400020729     C     *LIKE         DEFINE    §NTWFIE       DLPFL1
645500980506     C     *LIKE         DEFINE    §TRATA        DMITAS                                     ABILE ST
645600980506     C     *LIKE         DEFINE    §TRARR        DTRARR                                     ABILE ST
645700980506     C     *LIKE         DEFINE    §TRRPV        DTRRPV                                     ABILE ST
645800970306     C     *LIKE         DEFINE    ETTCTS        LCTS                           *DI LAVORO
645900970306     C     *LIKE         DEFINE    TAMKSC        KSC1
646000970306     C     *LIKE         DEFINE    TAMCTR        CTR1
646100970715     C     *LIKE         DEFINE    D55TFA        WTFA
646200970715     C     *LIKE         DEFINE    D55TFP        WTFP
646300970721     C     *LIKE         DEFINE    ORGCTS        WMCT
646400011008     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
646500011008     C     *LIKE         DEFINE    TBEKE1        KTEKE1
646600970306     C*--------------------
646700970306     C* CHIAVI DI LETTURA
646800970306     C*--------------------
646900010926     C* LETTURA TEETC01L - COMPLETA
647000970306     C     KEYETC        KLIST
647100980603     C                   KFLD                    KTCDSF                         *DATI SEDE O FILIALE
647200980603     C                   KFLD                    KTCCTO                         *TARIFFA O OFFERTA
647300970306     C                   KFLD                    KTCKSC                         *CLIENTE O VISITA
647400970306     C                   KFLD                    KTCCTR                         *TARIFFA
647500970306     C                   KFLD                    KTCPRG                         *PROGRESSIVO
647600970306     C                   KFLD                    KTCLNP                         *LINEA PARTENZA
647700010926     C* LETTURA TEETS01L - COMPLETA
647800970306     C     KEYETS        KLIST
647900980603     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
648000970306     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
648100970306     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
648200970306     C                   KFLD                    KTSCTR                         *TARIFFA
648300970306     C                   KFLD                    KTSPRG                         *PROGRESSIVO
648400970306     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
648500970306     C                   KFLD                    KTSSGL                         *SCAGLIONE
648600010926     C* LETTURA TEETS01L - PARZIALE
648700970306     C     KE2ETS        KLIST
648800980603     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
648900970306     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
649000970306     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
649100970306     C                   KFLD                    KTSCTR                         *TARIFFA
649200970306     C                   KFLD                    KTSPRG                         *PROGRESSIVO
649300970306     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
649400011017     C* LETTURA TEETT01L - COMPLETA
649500970306     C     KEYETT        KLIST
649600980603     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
649700970306     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
649800970306     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
649900970306     C                   KFLD                    KTTCTR                         *TARIFFA
650000970306     C                   KFLD                    KTTPRG                         *PROGRESSIVO
650100970306     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
650200970306     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
650300970306     C                   KFLD                    KTTCTS                         *CODICE TASSAZIONE
650400011017     C* LETTURA TEETT01L - PARZIALE
650500970306     C     KE2ETT        KLIST
650600980603     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
650700970306     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
650800970306     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
650900970306     C                   KFLD                    KTTCTR                         *TARIFFA
651000970306     C                   KFLD                    KTTPRG                         *PROGRESSIVO
651100970306     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
651200970306     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
651300010926     C* LETTURA TEETW01L - COMPLETA
651400970306     C     KEYETW        KLIST
651500970306     C                   KFLD                    KTWUSR                         UTENTE
651600980603     C                   KFLD                    KTWDSF                         *DATI SEDE O FILIALE
651700970306     C                   KFLD                    KTWCTO                         *TARIFFA O OFFERTA
651800970306     C                   KFLD                    KTWKSC                         *CLIENTE O VISITA
651900970306     C                   KFLD                    KTWCTR                         *TARIFFA
652000970306     C                   KFLD                    KTWPRG                         *PROGRESSIVO
652100970306     C                   KFLD                    KTWSGL                         *SCAGLIONE
652200970306     C                   KFLD                    KTWCTS                         *CODICE TASSAZIONE
652300970306     C                   KFLD                    KTWRDL                         *RAGGRUPPAMENTO DELT
652400010926     C* LETTURA TEETW01L - PARZIALE
652500970317     C     KE2ETW        KLIST
652600970317     C                   KFLD                    KTWUSR                         UTENTE
652700010926     C* LETTURA TEETWR1L - PARZIALE
652800970306     C     KEYEWR        KLIST
652900970306     C                   KFLD                    KWRUSR                         *UTENTE
653000970306     C* LETTURA TABEL00F - COMPLETA
653100970306     C     KEYTAB        KLIST
653200970306     C                   KFLD                    KTBKUT                         *UTENTE
653300970306     C                   KFLD                    KTBCOD                         *CODICE
653400970306     C                   KFLD                    KTBKEY                         *CHIAVE
653500970306     C* LETTURA TABEL00F - PARZIALE
653600970306     C     KE2TAB        KLIST
653700970306     C                   KFLD                    KTBKUT                         *UTENTE
653800970306     C                   KFLD                    KTBCOD                         *CODICE
653900970306     C* LETTURA AZORG01L - COMPLETA
654000970306     C     KEYORG        KLIST
654100970306     C                   KFLD                    KORFIL                         *FILIALE
654200970306     C* LETTURA TNTAM01L - PARZIALE
654300970306     C     KEYTAM        KLIST
654400970306     C                   KFLD                    KTMKSC                         *CLIENTE
654500970306     C                   KFLD                    KTMCTR                         *TARIFFA
654600970306     C* LETTURA TNTAM01L - COMPLETA
654700970306     C     KE2TAM        KLIST
654800970306     C                   KFLD                    KTMKSC                         *CLIENTE
654900970306     C                   KFLD                    KTMCTR                         *TARIFFA
655000970306     C                   KFLD                    KTMPRG                         *PROGRESSIVO
655100990819     C* LETTURA TITAD01L - PARZIALE
655200970306     C     KEYTAD        KLIST
655300970306     C                   KFLD                    KTDKSC                         *CLIENTE
655400970306     C                   KFLD                    KTDPRG                         *PROGRESSIVO
655500970306     C                   KFLD                    KTDLNP                         *LINEA DI PARTENZA
655600970306     C                   KFLD                    KTDCTS                         *CODICE TASSAZIONE
655700990819     C                   KFLD                    KTDNAZ                         *NAZIONE
655800990819     C                   KFLD                    KTDCAP                         *C.A.P.
655900970306     C                   KFLD                    KTDCTR                         *TARIFFA
656000990819     C* LETTURA TITPT01L - COMPLETA
656100970306     C     KEYTPT        KLIST
656200970306     C                   KFLD                    KTPKSC                         *CLIENTE
656300970306     C                   KFLD                    KTPCTR                         *TARIFFA
656400970306     C                   KFLD                    KTPPRG                         *PROGRESSIVO
656500970306     C                   KFLD                    KTPFTC                         *CONSEGNA PARTICOLAR
656600990819     C* LETTURA TITPD02L - PARZIALE
656700970306     C     KEYTPD        KLIST
656800970306     C                   KFLD                    KDPKSC                         *CLIENTE
656900970306     C                   KFLD                    KDPCTR                         *TARIFFA
657000970306     C                   KFLD                    KDPPRG                         *PROGRESSIVO
657100970306     C                   KFLD                    KDPFTC                         *CONSEGNA PARTICOLAR
657200970306     C                   KFLD                    KDPCTS                         *CODICE TASSAZIONE
657300990819     C* LETTURA TITGC01L - COMPLETA
657400970306     C     KEYTGC        KLIST
657500970306     C                   KFLD                    KGCKSC                         *CLIENTE
657600970306     C                   KFLD                    KGCCTR                         *TARIFFA
657700970306     C                   KFLD                    KGCPRG                         *PROGRESSIVO
657800011008     C* LETTURA TNTBE01L - PARZIALE
657900011008     C     KEYTBE        KLIST
658000011008     C                   KFLD                    KTECOD                         *UTENTE
658100011008     C                   KFLD                    KTEKE1                         *CODICE
658200970306     C*--------------------
658300970306     C* VARIABILI DI CONTROLLO
658400970306     C*--------------------
658500970306     C                   MOVEL     'N'           WERR              1            *ERRORE
658600970306     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGIO
658700970306     C                   MOVEL     '0'           $TGC              1            *TAR.GIACENZA  ESIST
658800970306     C                   MOVEL     '0'           $TAD              1            *TAR.DETTAGLIO ESIST
658900970306     C                   MOVEL     '0'           $TPT              1            *TAR.PART.TES. ESIST
659000970306     C                   MOVEL     '0'           $TPD              1            *TAR.PART.DET. ESIST
659100011023     C                   MOVEL     D00DIV        WDIV              3            *DIVISA CALCOLO CAT
659200970306     C*
659300970306     C* REPERISCE LA DATA CORRENTE
659400970306     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
659500970306     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
659600970306     C                   Z-ADD     WN8           G08DAT
659700970306     C                   Z-ADD     *ZEROS        G08INV
659800970306     C                   MOVEL     '0'           G08ERR
659900970306     C                   CALL      'XSRDA8'
660000970306     C                   PARM                    WLBDA8
660100970306     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
660200011008     C*
660300011008     C* RECUPERA DATI DA TEBELLA "GED" TIPO RECORD "CE"
660400011008     C                   EXSR      CARGEDCE
660500040716     C* RECUPERA DATI DA TEBELLA "GEI" TIPO RECORD "EUR"
660600040716     C                   EXSR      CARGEIEUR
660700971204     C*
660800971204     C* RECUPERA INSTRADAMENTO -PARTENZA-
660900971204     C                   EXSR      REPINP
661000970306     C*
661100970306     C                   ENDSR
661200970306     O*------------------------------------------------------------------------*
661300970306** MSG - MESSAGGI
661400970306TNTE11R- Dati tipo del cliente/tariffa non trovati                            1
661500970306TNTE11R- Scaglioni del cliente/tariffa non trovati                            2
661600970306TNTE11R- Codici tassazione del cliente/tariffa non trovati                    3
661700970306TNTE11R- Codice tassazione non trovato                                        4
661800970715TNTE11R- La linea di partenza della tariffa non è stata trovata               5
661900970715TNTE11R- 6
662000970715TNTE11R- 7
662100970306TNTE11R- 8
662200970306TNTE11R- 9
662300970306TNTE11R- 10
662400970306TNTE11R- Tabella QT1 non trovata. Avvertire il C.E.D.                         11
662500970306TNTE11R- Tabella QT2 non trovata. Avvertire il C.E.D.                         12
662600101018???????????? % ISTAT non trovata. Avvertire il C.E.D.                         13
662700101018                                                                              14
662800970306TNTE11R- Tabella TR  non trovata. Avvertire il C.E.D.                         15
662900970306TNTE11R- Tabella $2  non trovata. Avvertire il C.E.D.                         16
663000970306TNTE11R- Tabella CV  non trovata. Avvertire il C.E.D.                         17
663100970306TNTE11R- Tabella CT  non trovata. Avvertire il C.E.D.                         18
663200970306TNTE11R- Tabella 5E  non trovata. Avvertire il C.E.D.                         19
663300970306TNTE11R- Tabella 1P  non trovata. Avvertire il C.E.D.                         20
663400970306TNTE11R- 21
663500970306TNTE11R- MANCA TARIFFA. Tariffa di cartello inesistente.                      22
663600970306TNTE11R- MANCA TARIFFA. Tariffa giacenza non trovata.                         23
663700970306TNTE11R- MANCA TARIFFA. Tariffa dettaglio cliente non trovata.                24
663800970306TNTE11R- MANCA TARIFFA. Scaglione di peso in tariffa non trovato.             25
663900001218TNTE11R- MANCA TARIFFA. Scaglione di peso in t.particolare non trovato.   -  -26
664000970306TNTE11R- 27
664100970306TNTE11R- 28
664200970306TNTE11R- 29
664300970306TNTE11R- 30
