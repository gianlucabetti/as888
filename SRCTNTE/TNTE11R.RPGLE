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
014700151120     D Detcflo       E DS
014800151120     D*------------------
014900970715     D* DS "FNLV55R" - REPERIMENTO TERMINALs
015000970715     D*------------------
015100970715     D DSLV55        E DS                  EXTNAME(FNLV55DS)
015200970306     D*-------------------
015300970306     D* DS DI PROCEDURA
015400970306     D*-------------------
015500970306     D DSTE00        E DS                  EXTNAME(TNTE00DS)
015600991125     D*-------------------
015700991125     D* CONVERSIONI IMPORTI
015800991125     D*-------------------
015900991125     D YEUR          E DS                  EXTNAME(YEURCODS)
016000970306     D*-------------------
016100970306     D* ARCHITETTURA
016200970306     D*-------------------
016300970306     D KPJBA         E DS
016400970306     D*-------------------
016500970306     D* DS "XSRDA8" - CONTROLLO DATA (8)
016600970306     D*-------------------
016700970306     D WLBDA8          DS                  INZ
016800970306     D  G08DAT                 1      8  0
016900970306     D  G08INV                 9     16  0
017000970306     D  G08ERR                17     17
017100970306     D  G08TGI                18     22  0
017200021202     D*-------------------
017300021202     D* DS *PGM REPIMEMTO TARIFFE DI CARTELLO
017400021202     D*-------------------
017500160310     D TRULC7DS      E DS
017600970306     D*-------------------
017700970306     D* DS SIGLE VARIE
017800970306     D*-------------------
017900970306     D DS$2          E DS
018000970306     D*-------------------
018100980506     D* DS TIPI TARIFFA                     -15 TIPI TARIFFA-
018200970306     D*-------------------
018300980506     D DSTR          E DS                  OCCURS(15) INZ
018400970306     D*-------------------
018500050103     D* DS CODICI TASSAZIONE                -600 CODICI TASSAZIONE-
018600970306     D*-------------------
018700050103     D DSCT          E DS                  OCCURS(600) INZ
018800970306     D*-------------------
018900970306     D* DS TIPI SERVIZIO                    -10 TIPI SERVIZIO-
019000970306     D*-------------------
019100970306     D DS5E          E DS                  OCCURS(10) INZ
019200970306     D*-------------------
019300020104     D* DS CONSEGNE PARTICOLARI             -50 PARTICOLARITA'-
019400980706     D*-------------------
019500020104     D DS1P          E DS                  OCCURS(50) INZ
019600980706     D*-------------------
019700980706     D* FLAG OPERATIVI -TNTAM-
019800980706     D*-------------------
019900980706     D DSTA01        E DS
020000011005     D*------------------
020100011005     D* DS LETTURA TABELLA "GED" TIPO RECORD "CE"
020200011005     D*------------------
020300011005     D DSGEDCE       E DS
020400040716     D*------------------
020500040716     D* DS LETTURA TABELLA "GEI" TIPO RECORD "EUR" (limite risarcibile
020600040716     D*------------------
020700040716     D Dgei          E DS
020800970306     D*-------------------
020900020104     D* CONSEGNE PARTICOLARI DI CARTELLO    -50 PARTICOLARITA'-
021000970306     D*-------------------
021100020104     D CPIDS           DS                  OCCURS(50)
021200010926     D  TTPG8P                 1      1
021300011031     D  TARF8P                 2     22  8
021400011031     D  TARL8P                23     43  8
021500011031     D  TARO8P                44     64  8
021600011031     D  TRPV8P                65     67  1
021700011031     D  CP88                  68     68
021800011031     D  TCTS8P                69     70
021900011031     D  TCTE8P                71     72
022000011031     D  TAPL8P                73     73
022100970306     D*-------------------
022200970306     D* ASSICURAZIONE PER CONTO             -10 SCAGLIONI-
022300970306     D*-------------------
022400970306     D ACDS            DS                  OCCURS(10)
022500011031     D  ASGL                   1     21  8
022600011031     D  AITR                  22     42  8
022700011031     D  AMIN                  43     63  8
022800011031     D  AMAX                  64     84  8
022900011031     D  AAIN                  85     85
023000970306     D*-------------------
023100980707     D* DETTAGLIO TARIFFARIO CLIENTE        -11 SCAGLIONI-
023200970306     D*-------------------
023300980707     D TARDS           DS                  OCCURS(11)
023400990819     D  TLNP                   1      3  0
023500990819     D  TCTS                   4      5
023600990819     D  TNAZ                   6      8
023700990819     D  TCAP                   9     17
023800990819     D  TCTR                  18     20  0
023900011031     D  TSCG                  21     41  8
024000011031     D  TITR                  42     62  8
024100011031     D  TINO                  63     83  8
024200011031     D  TRPV                  84     86  1
024300011031     D  TMIN                  87    107  8
024400011031     D  TMAX                 108    128  8
024500001218     D*-------------------
024600001218     D* COMPOSIZIONI MESSAGGI
024700001218     D*-------------------
024800001218     D* MSG,26
024900001218     D                 DS
025000001218     D  DM26A                 76     77
025100001218     D  DMSG26                 1     78
025200020729     D*-------------------
025300020729     D* DS RIDEFINIZIONE DESCRIZIONE 143 ORGANIGRAMMA
025400020729     D*-------------------
025500020729     D OG143         E DS
025600020729     D*------------------
025700020729     D* DS PER LETTURA TABELLA "NTW"
025800020729     D*------------------
025900020729     D DNTW          E DS
026000041213     d tasadgs         s                   like(tasadg)
026100091026     d tpg             s                   like(tpttpg)
026200970306     I*------------------------------------------------------------------------*
026300970306     I* INPUT
026400970306     I*------------------------------------------------------------------------*
026500970306     I*-------------------
026600970306     I* DS FORMATO RECORD TARIFFE GIACENZA
026700970306     I*-------------------
026800990819     ITITGC000
026900970306     I              TGCSG1                      TGS(1)
027000970306     I              TGCSG2                      TGS(2)
027100970306     I              TGCSG3                      TGS(3)
027200970306     I              TGCST1                      TTS(1)
027300970306     I              TGCST2                      TTS(2)
027400970306     I              TGCST3                      TTS(3)
027500970625     I*-------------------
027600970625     I* DS FORMATO RECORD TARIFFE GIACENZA
027700970625     I*-------------------
027800990819     ITIOGC000
027900970625     I              TGCSG1                      TGS(1)
028000970625     I              TGCSG2                      TGS(2)
028100970625     I              TGCSG3                      TGS(3)
028200970625     I              TGCST1                      TTS(1)
028300970625     I              TGCST2                      TTS(2)
028400970625     I              TGCST3                      TTS(3)
028500970306     C*------------------------------------------------------------------------*
028600970306     C* MAIN LINE
028700970306     C*------------------------------------------------------------------------*
028800970306     C*
028900970306     C* CANCELLA IL FILE DI LAVORO
029000970306IF  1C     WERR          IFEQ      'N'
029100970306     C                   EXSR      CANCWF
029200970306E   1C                   ENDIF
029300970306     C*
029400970306     C* CARICA LE TABELLE OCCORRENTI
029500970306IF  1C     WERR          IFEQ      'N'
029600970306     C                   EXSR      CARTAB
029700970306E   1C                   ENDIF
029800970715     C*
029900970306     C* IMPOSTA LA SEQUENZA DI CALCOLO DI TUTTE LE VOCI
030000970306IF  1C     WERR          IFEQ      'N'
030100970306     C                   EXSR      IMPSEQ
030200970306E   1C                   ENDIF
030300970306     C*
030400970306     C* CARICA DATI TIPO -GENERALI-
030500970306IF  1C     WERR          IFEQ      'N'
030600970306     C                   EXSR      CARETC
030700970306E   1C                   ENDIF
030800970306     C*
030900970306     C* CARICA DATI TIPO -SCAGLIONI-
031000970306IF  1C     WERR          IFEQ      'N'
031100970306     C                   EXSR      CARETS
031200970306E   1C                   ENDIF
031300970306     C*
031400970306     C* CARICA DATI TIPO -CODICI TASSAZIONE-
031500970306IF  1C     WERR          IFEQ      'N'
031600970306     C                   EXSR      CARETT
031700970306E   1C                   ENDIF
031800970306     C*
031900970306     C* DETERMINA LE VOCI DA CALCOLARE
032000970306IF  1C     WERR          IFEQ      'N'
032100970306     C                   EXSR      CHKVOC
032200970306E   1C                   ENDIF
032300970306     C*
032400970306     C* CARICA LA TARIFFA DEL CLIENTE -TESTATA-
032500970306IF  1C     WERR          IFEQ      'N'
032600970306     C                   EXSR      CARTAM
032700970306E   1C                   ENDIF
032800970306     C*
032900970306     C* CARICA LA TARIFFA GIACENZA DEL CLIENTE
033000970306IF  1C     WERR          IFEQ      'N'
033100970306IF  2C     VC(8)         IFEQ      'S'
033200970306     C     VC(18)        OREQ      'S'
033300970306     C     VC(19)        OREQ      'S'
033400970306     C     VC(20)        OREQ      'S'
033500970306     C                   MOVEL     '0'           $TGC
033600970306     C                   EXSR      CARTGC
033700970306E   2C                   ENDIF
033800970306E   1C                   ENDIF
033900970306     C*
034000970306     C* VALORIZZA LE VOCI DA CALCOLARE -FISSE-
034100970306IF  1C     WERR          IFEQ      'N'
034200970306     C                   EXSR      VALVO1
034300970306E   1C                   ENDIF
034400970306     C*
034500970306     C* CICLO PER TUTTI GLI SCAGLIONI ----------------------------------------->
034600970306DO  1C     1             DO        WULTSC        S                 6 0
034700970306IF  2C     SSGL(S)       IFNE      *ZEROS
034800970306     C*
034900970306     C* DETERMINA LE VOCI DA CALCOLARE
035000970306IF  3C     WERR          IFEQ      'N'
035100970306     C                   EXSR      CHKVOC
035200970306E   3C                   ENDIF
035300970306     C*
035400970306     C* VALORIZZA LE VOCI DA CALCOLARE -DIPENDENTI DA SCAGLIONI-
035500970306IF  3C     WERR          IFEQ      'N'
035600970306     C                   EXSR      VALVO2
035700970306E   3C                   ENDIF
035800970306     C*
035900970306     C* CICLO PER TUTTI I CODICI TASSAZIONE ----------------------------------->
036000970306DO  3C                   DO        WULTCT        T                 6 0
036100970306IF  4C     CCTS(T)       IFNE      *BLANKS
036200970306     C*
036300970306     C* DETERMINA LE VOCI DA CALCOLARE
036400970306IF  5C     WERR          IFEQ      'N'
036500970306     C                   EXSR      CHKVOC
036600970306E   5C                   ENDIF
036700970306     C*
036800970306     C* CARICA LA TARIFFA DEL CLIENTE -DETTAGLIO-
036900970306IF  5C     WERR          IFEQ      'N'
037000970306     C                   EXSR      CARTAD
037100970306E   5C                   ENDIF
037200970306     C*
037300970306     C* REPERISCE E CALCOLA LA QUANTITA' DA UTILIZZARE
037400970306IF  5C     WERR          IFEQ      'N'
037500970306     C                   EXSR      CALQTA
037600970306E   5C                   ENDIF
037700970306     C*
037800970306     C* VALORIZZA LE VOCI DA CALCOLARE -DIPENDENTI DA CODICI TASSAZIONE-
037900970306IF  5C     WERR          IFEQ      'N'
038000970306     C                   EXSR      VALVO3
038100970306E   5C                   ENDIF
038200970306     C*
038300970306     C* CALCOLA IMPORTI SULLE % INCIDENZA E I RAGGUPPAMENTI DELTA
038400970306IF  5C     WERR          IFEQ      'N'
038500970306     C                   EXSR      CALTOT
038600970306E   5C                   ENDIF
038700970306     C*
038800011017     C* SCRIVE IL FILE DI LAVORO TEETW
038900970306IF  5C     WERR          IFEQ      'N'
039000970306     C                   EXSR      SCRETW
039100970306E   5C                   ENDIF
039200970306     C*
039300010926     C* SCRIVE IL FILE DI LAVORO TEETWR SE LANCIO IN DEBUG
039400970306IF  5C     WERR          IFEQ      'N'
039500970306     C     D00DBG        ANDEQ     'S'
039600970306     C                   EXSR      SCREWR
039700970306E   5C                   ENDIF
039800970306E   4C                   ENDIF
039900970306E   3C                   ENDDO                                                  *FINE COD.TASSAZIONE
040000970306     C*
040100970306E   2C                   ENDIF
040200970306E   1C                   ENDDO                                                  *FINE SCAGLIONI
040300980702     C*
040400970715     C* OPERAZIONI FINALI
040500970715     C                   EXSR      FINSR
040600970306     C*
040700970306     C                   SETON                                        LR
040800970306     C*------------------------------------------------------------------------*
040900970306     C* IMPSEQ - IMPOSTA LA SEQUENZA DI CALCOLO DI TUTTE LE VOCI
041000970306     C*------------------------------------------------------------------------*
041100970306     C     IMPSEQ        BEGSR
041200970306     C*
041300970306     C* DETERMINA IL CODICE VOCE
041400970306     C                   EXSR      CODVOC
041500970306     C*
041600970306     C* DETERMINA LA SEQUENZA DELLE VOCI
041700970306     C                   EXSR      SEQVOC
041800970306     C*
041900970306     C* DETERMINA IL RAGGRUPPAMENTO DELTA DELLE VOCI
042000970306     C                   EXSR      RAGVOC
042100970306     C*
042200970306     C                   ENDSR
042300970306     C*------------------------------------------------------------------------*
042400970306     C* CODVOC - DETERMINA IL CODICE DELLE VOCI
042500970306     C*------------------------------------------------------------------------*
042600970306     C     CODVOC        BEGSR
042700970306     C*
042800970306     C                   Z-ADD     001           VV(1)                          *PORTO
042900970721     C                   Z-ADD     002           VV(2)                          *DIRITTO FISSO
043000970717     C                   Z-ADD     003           VV(3)                          *ADG X PORTO
043100970306     C                   Z-ADD     004           VV(4)                          *ISTAT X PORTO
043200970721     C                   Z-ADD     005           VV(5)                          *TCP
043300970306     C                   Z-ADD     006           VV(6)                          *INOLTRO
043400970717     C                   Z-ADD     007           VV(7)                          *ESPRESSO SU PORTO
043500970721     C                   Z-ADD     008           VV(8)                          *GIACENZA
043600970306     C                   Z-ADD     009           VV(9)                          *ASS.CONTO FRANCO
043700970306     C                   Z-ADD     010           VV(10)                         *ASS.RCV   FRANCO
043800970306     C                   Z-ADD     011           VV(11)                         *CONS. APPUNTAMENTO
043900970306     C                   Z-ADD     012           VV(12)                         *CONS. SUPERMERCATI
044000970306     C                   Z-ADD     013           VV(13)                         *CONS. AI PIANI
044100970306     C                   Z-ADD     014           VV(14)                         *CONS. FUORI MISURA
044200970721     C                   Z-ADD     015           VV(15)                         *C/ASSEGNO MITTENTE
044300970721     C                   Z-ADD     016           VV(16)                         *C/ASSEGNO BARTOLINI
044400970306     C                   Z-ADD     017           VV(17)                         *RITIRO
044500970721     C                   Z-ADD     018           VV(18)                         *GIACENZA
044600970721     C                   Z-ADD     019           VV(19)                         *GIACENZA
044700970721     C                   Z-ADD     020           VV(20)                         *GIACENZA
044800970717     C                   Z-ADD     021           VV(21)                         *ADG X INOLTRO
044900970717     C                   Z-ADD     022           VV(22)                         *ADG X DIR.FISSO
045000970717     C                   Z-ADD     023           VV(23)                         *ADG X ESPRESSO/PORT
045100970306     C                   Z-ADD     024           VV(24)                         *ISTAT X INOLTRO
045200970306     C                   Z-ADD     025           VV(25)                         *ISTAT X DIR.FISSO
045300970717     C                   Z-ADD     026           VV(26)                         *ISTAT X ESPRES/PORT
045400970306     C                   Z-ADD     027           VV(27)                         *ISTAT X ADG/PORTO
045500970306     C                   Z-ADD     028           VV(28)                         *ISTAT X ADG/D.FISSO
045600970306     C                   Z-ADD     029           VV(29)                         *ISTAT X ADG/INOLTRO
045700970717     C                   Z-ADD     030           VV(30)                         *ISTAT X ADG/ESP/POR
045800970306     C                   Z-ADD     031           VV(31)                         *ASS.CONTO ASSEGNATO
045900970306     C                   Z-ADD     032           VV(32)                         *ASS.RCV ASSEGNATO
046000970414     C                   Z-ADD     033           VV(33)                         *CONS. DDT
046100970721     C                   Z-ADD     034           VV(34)                         *ISTAT X ADG/ESP/INO
046200970414     C                   Z-ADD     035           VV(35)                         *CONS. LOC.DISAGIATE
046300970717     C                   Z-ADD     036           VV(36)                         *ESPRESSO SU INOLTRO
046400970717     C                   Z-ADD     037           VV(37)                         *ADG X ESPRES/INOLTR
046500970717     C                   Z-ADD     038           VV(38)                         *ISTAT X ESPRES/INOL
046600970721     C                   Z-ADD     039           VV(39)                         *ESPRESSO SU L.DISAG
046700970721     C                   Z-ADD     040           VV(40)                         *ADG X ESPRES/L.DISA
046800970721     C                   Z-ADD     041           VV(41)                         *ISTAT X ESPRES/L.DI
046900970721     C                   Z-ADD     042           VV(42)                         *ISTAT X ADG/ESP/LDI
047000971023     C                   Z-ADD     043           VV(43)                         *ADG X L.DISAGIATE
047100971023     C                   Z-ADD     044           VV(44)                         *ISTAT X L.DISAGIATE
047200971023     C                   Z-ADD     045           VV(45)                         *ISTAT X ADG/L.DISAG
047300001219     C                   Z-ADD     046           VV(46)                         *ADG SU TCP
047400001219     C                   Z-ADD     047           VV(47)                         *ADG SU GIACENZA VAR
047500001219     C                   Z-ADD     048           VV(48)                         *ADG SU ASSIC X CONT
047600001219     C                   Z-ADD     049           VV(49)                         *ADG SU ASSIC RCV  F
047700001219     C                   Z-ADD     050           VV(50)                         *ADG SU CONSEGNA APP
047800001219     C                   Z-ADD     051           VV(51)                         *ADG SU CONSEGNA SUP
047900001219     C                   Z-ADD     052           VV(52)                         *ADG SU CONSEGNA PIA
048000001219     C                   Z-ADD     053           VV(53)                         *ADG SU CONSEGNA FUO
048100001219     C                   Z-ADD     054           VV(54)                         *ADG SU C/ASSEGNO MI
048200001219     C                   Z-ADD     055           VV(55)                         *ADG SU C/ASSEGNO BA
048300001219     C                   Z-ADD     056           VV(56)                         *ADG SU RITIRO
048400001219     C                   Z-ADD     057           VV(57)                         *ADG SU GIACENZA CON
048500001219     C                   Z-ADD     058           VV(58)                         *ADG SU GIACENZA CON
048600001219     C                   Z-ADD     059           VV(59)                         *ADG SU GIACENZA SOS
048700001219     C                   Z-ADD     060           VV(60)                         *ADG SU ASSIC X CONT
048800001219     C                   Z-ADD     061           VV(61)                         *ADG SU ASSIC RCV AS
048900001219     C                   Z-ADD     062           VV(62)                         *ADG SU CONSEGNA DDT
049000001219     C                   Z-ADD     063           VV(63)                         *ISTAT SU ADG/TCP
049100001219     C                   Z-ADD     064           VV(64)                         *ISTAT SU ADG/GIAC.V
049200001219     C                   Z-ADD     065           VV(65)                         *ISTAT SU ADG/ASSXCO
049300001219     C                   Z-ADD     066           VV(66)                         *ISTAT SU ADG/ASS RC
049400001219     C                   Z-ADD     067           VV(67)                         *ISTAT SU ADG/CONSEG
049500001219     C                   Z-ADD     068           VV(68)                         *ISTAT SU ADG/CONSEG
049600001219     C                   Z-ADD     069           VV(69)                         *ISTAT SU ADG/CONSEG
049700001219     C                   Z-ADD     070           VV(70)                         *ISTAT SU ADG/CONSEG
049800001219     C                   Z-ADD     071           VV(71)                         *ISTAT SU ADG/CASSEG
049900001219     C                   Z-ADD     072           VV(72)                         *ISTAT SU ADG/CASSEG
050000001219     C                   Z-ADD     073           VV(73)                         *ISTAT SU ADG/RITIRO
050100001219     C                   Z-ADD     074           VV(74)                         *ISTAT SU ADG/GIACEN
050200001219     C                   Z-ADD     075           VV(75)                         *ISTAT SU ADG/GIACEN
050300001219     C                   Z-ADD     076           VV(76)                         *ISTAT SU ADG/GIACEN
050400001219     C                   Z-ADD     077           VV(77)                         *ISTAT SU ADG/ASSIC
050500001219     C                   Z-ADD     078           VV(78)                         *ISTAT SU ADG/ASSIC
050600001219     C                   Z-ADD     079           VV(79)                         *ISTAT SU ADG/CONSEG
050700011030     C                   Z-ADD     080           VV(80)                         *CONS.ISOLA
050800011030     C                   Z-ADD     081           VV(81)                         *ESPRESSO SU C.ISOLA
050900011030     C                   Z-ADD     082           VV(82)                         *ADG X ESPRES/C.ISOL
051000011030     C                   Z-ADD     083           VV(83)                         *ISTAT X ESPRES/C.IS
051100011030     C                   Z-ADD     084           VV(84)                         *ISTAT X ADG/ESP/CIS
051200011030     C                   Z-ADD     085           VV(85)                         *ADG X CONS.ISOLA
051300011030     C                   Z-ADD     086           VV(86)                         *ISTAT X CONS.ISOLA
051400011030     C                   Z-ADD     087           VV(87)                         *ISTAT X ADG/R.ISOLA
051500011030     C                   Z-ADD     088           VV(88)                         *RITIRO ISOLA
051600011030     C                   Z-ADD     089           VV(89)                         *ESPRESSO SU R.ISOLA
051700011030     C                   Z-ADD     090           VV(90)                         *ADG X ESPRES/R.ISOL
051800011030     C                   Z-ADD     091           VV(91)                         *ISTAT X ESPRES/R.IS
051900011030     C                   Z-ADD     092           VV(92)                         *ISTAT X ADG/ESP/RIS
052000011030     C                   Z-ADD     093           VV(93)                         *ADG X RITIRO ISOLA
052100011030     C                   Z-ADD     094           VV(94)                         *ISTAT X RITIROISOLA
052200011030     C                   Z-ADD     095           VV(95)                         *ISTAT X ADG/R.ISOLA
052300040826     C                   Z-ADD     096           VV(96)                         *ISTAT X ADG/R.ISOLA
052400040826     C                   Z-ADD     097           VV(97)                         *ISTAT X ADG/R.ISOLA
052500040826     C                   Z-ADD     098           VV(98)                         *ISTAT X ADG/R.ISOLA
052600040826     C                   Z-ADD     099           VV(99)                         *ISTAT X ADG/R.ISOLA
052700040826     C                   Z-ADD     100           VV(100)                        *ISTAT X ADG/R.ISOLA
052800040826     C                   Z-ADD     101           VV(101)                        *ISTAT X ADG/R.ISOLA
052900040826     C                   Z-ADD     102           VV(102)                        *ISTAT X ADG/R.ISOLA
053000040826     C                   Z-ADD     103           VV(103)                        *ISTAT X ADG/R.ISOLA
053100040826     C                   Z-ADD     104           VV(104)                        *ISTAT X ADG/R.ISOLA
053200060314     C                   Z-ADD     105           VV(105)                        *ISTAT X ADG/R.ISOLA
053300060314     C                   Z-ADD     106           VV(106)                        *ISTAT X ADG/R.ISOLA
053400060314     C                   Z-ADD     107           VV(107)                        *ISTAT X ADG/R.ISOLA
053500060314     C                   Z-ADD     108           VV(108)                        *ISTAT X ADG/R.ISOLA
053600060314     C                   Z-ADD     109           VV(109)                        *ISTAT X ADG/R.ISOLA
053700060314     C                   Z-ADD     110           VV(110)                        *ISTAT X ADG/R.ISOLA
053800151120     C                   Z-ADD     111           VV(111)                        *ISTAT X ADG/R.ISOLA
053900151120     C                   Z-ADD     112           VV(112)                        *ISTAT X ADG/R.ISOLA
054000151120     C                   Z-ADD     113           VV(113)                        *ISTAT X ADG/R.ISOLA
054100151120     C                   Z-ADD     114           VV(114)                        *ISTAT X ADG/R.ISOLA
054200151120     C                   Z-ADD     115           VV(115)                        *ISTAT X ADG/R.ISOLA
054300151120     C                   Z-ADD     116           VV(116)                        *ISTAT X ADG/R.ISOLA
054400971023     C*
054500151120     C                   Z-ADD     116           NVOCA             3 0          *N° VOCI DA CALCOLAR
054600970306     C*
054700970306     C                   ENDSR
054800970306     C*------------------------------------------------------------------------*
054900970306     C* SEQVOC - DETERMINA LA SEQUENZA DI CALCOLO DELLE VOCI
055000970306     C*------------------------------------------------------------------------*
055100970306     C     SEQVOC        BEGSR
055200970306     C*---
055300970306     C* VOCI FISSE = SEQUENZA UNO
055400970306     C*---
055500970306     C                   Z-ADD     1             VS(8)                          *GIACENZA
055600001222     C                   Z-ADD     1             VS(47)                         *ADG SU GIACENZA VAR
055700001222     C                   Z-ADD     1             VS(64)                         *ISTAT SU ADG/GIAC.V
055800970306     C*---
055900970306     C* VOCI DIPENDENTI DA SCAGLIONI = SEQUENZA DUE
056000970306     C*---
056100970306     C                   Z-ADD     2             VS(18)                         *GIACENZA
056200001227     C                   Z-ADD     2             VS(57)                         *ADG SU GIACENZA CON
056300001227     C                   Z-ADD     2             VS(74)                         *ISTAT SU ADG/GIACEN
056400970306     C                   Z-ADD     2             VS(19)                         *GIACENZA
056500001227     C                   Z-ADD     2             VS(58)                         *ADG SU GIACENZA CON
056600001227     C                   Z-ADD     2             VS(75)                         *ISTAT SU ADG/GIACEN
056700970306     C                   Z-ADD     2             VS(20)                         *GIACENZA
056800001227     C                   Z-ADD     2             VS(59)                         *ADG SU GIACENZA SOS
056900001227     C                   Z-ADD     2             VS(76)                         *ISTAT SU ADG/GIACEN
057000970306     C*---
057100970306     C* VOCI DIPENDENTI DA SCAGLIONI/CT/LNA/TERM ARRIVO = SEQUENZA TRE
057200970306     C*---
057300970306     C                   Z-ADD     3             VS(1)                          *PORTO
057400980702     C                   Z-ADD     3             VS(2)                          *DIRITTO FISSO
057500990819     C                   Z-ADD     3             VS(3)                          *A.D.G X PORTO
057600970306     C                   Z-ADD     3             VS(4)                          *ISTAT X PORTO
057700990819     C                   Z-ADD     3             VS(5)                          *T.C.P.
057800970306     C                   Z-ADD     3             VS(6)                          *INOLTRO
057900970717     C                   Z-ADD     3             VS(7)                          *ESPRESSO PORTO
058000970306     C                   Z-ADD     3             VS(9)                          *ASS.CONTO FRANCO
058100970306     C                   Z-ADD     3             VS(10)                         *ASSICURAZIONE R.C.V
058200970306     C                   Z-ADD     3             VS(11)                         *CONS. APPUNTAMENTO
058300970306     C                   Z-ADD     3             VS(12)                         *CONS. SUPERMERCATI
058400970306     C                   Z-ADD     3             VS(13)                         *CONS. AI PIANI
058500970306     C                   Z-ADD     3             VS(14)                         *CONS. FUORI MISURA
058600980702     C                   Z-ADD     3             VS(15)                         *C/ASSEGNO MITTENTE
058700980702     C                   Z-ADD     3             VS(16)                         *C/ASSEGNO BARTOLINI
058800970306     C                   Z-ADD     3             VS(17)                         *RITIRO
058900970306     C                   Z-ADD     3             VS(21)                         *A.D.G X INOLTRO
059000970306     C                   Z-ADD     3             VS(22)                         *A.D.G X DIR.FISSO
059100970717     C                   Z-ADD     3             VS(23)                         *ADG X ESPRESSO/PORT
059200970306     C                   Z-ADD     3             VS(24)                         *ISTAT X INOLTRO
059300970306     C                   Z-ADD     3             VS(25)                         *ISTAT X DIR.FISSO
059400970717     C                   Z-ADD     3             VS(26)                         *ISTAT X ESPRES/PORT
059500970306     C                   Z-ADD     3             VS(27)                         *ISTAT X ADG/PORTO
059600970306     C                   Z-ADD     3             VS(28)                         *ISTAT X ADG/D.FISSO
059700970306     C                   Z-ADD     3             VS(29)                         *ISTAT X ADG/INOLTRO
059800970717     C                   Z-ADD     3             VS(30)                         *ISTAT X ADG/ESP/POR
059900970306     C                   Z-ADD     3             VS(31)                         *ASS.CONTO ASSEGNATO
060000970306     C                   Z-ADD     3             VS(32)                         *ASS.RCV ASSEGNATO
060100970414     C                   Z-ADD     3             VS(33)                         *CONS. DDT
060200970721     C                   Z-ADD     3             VS(34)                         *ISTAT X ADG/ESP/INO
060300970414     C                   Z-ADD     3             VS(35)                         *CONS. LOC.DISAGIATE
060400970717     C                   Z-ADD     3             VS(36)                         *ESPRESSO INOLTRO
060500970717     C                   Z-ADD     3             VS(37)                         *ADG X ESPRESSO/INOL
060600970717     C                   Z-ADD     3             VS(38)                         *ISTAT X ESPRES/INOL
060700970721     C                   Z-ADD     3             VS(39)                         *ESPRESSO SU L.DISAG
060800970721     C                   Z-ADD     3             VS(40)                         *ADG X ESPRES/L.DISA
060900970721     C                   Z-ADD     3             VS(41)                         *ISTAT X ESPRES/L.DI
061000970721     C                   Z-ADD     3             VS(42)                         *ISTAT X ADG/ESP/LDI
061100971023     C                   Z-ADD     3             VS(43)                         *ADG X L.DISAGIATE
061200971023     C                   Z-ADD     3             VS(44)                         *ISTAT X L.DISAGIATE
061300971023     C                   Z-ADD     3             VS(45)                         *ISTAT X ADG/L.DISAG
061400001219     C                   Z-ADD     3             VS(46)                         *ADG SU TCP
061500001219     C                   Z-ADD     3             VS(48)                         *ADG SU ASSIC X CONT
061600001219     C                   Z-ADD     3             VS(49)                         *ADG SU ASSIC RCV  F
061700001219     C                   Z-ADD     3             VS(50)                         *ADG SU CONSEGNA APP
061800001219     C                   Z-ADD     3             VS(51)                         *ADG SU CONSEGNA SUP
061900001219     C                   Z-ADD     3             VS(52)                         *ADG SU CONSEGNA PIA
062000001219     C                   Z-ADD     3             VS(53)                         *ADG SU CONSEGNA FUO
062100001219     C                   Z-ADD     3             VS(54)                         *ADG SU C/ASSEGNO MI
062200001219     C                   Z-ADD     3             VS(55)                         *ADG SU C/ASSEGNO BA
062300001219     C                   Z-ADD     3             VS(56)                         *ADG SU RITIRO
062400001219     C                   Z-ADD     3             VS(60)                         *ADG SU ASSIC X CONT
062500001219     C                   Z-ADD     3             VS(61)                         *ADG SU ASSIC RCV AS
062600001219     C                   Z-ADD     3             VS(62)                         *ADG SU CONSEGNA DDT
062700001219     C                   Z-ADD     3             VS(63)                         *ISTAT SU ADG/TCP
062800001219     C                   Z-ADD     3             VS(65)                         *ISTAT SU ADG/ASSXCO
062900001219     C                   Z-ADD     3             VS(66)                         *ISTAT SU ADG/ASS RC
063000001219     C                   Z-ADD     3             VS(67)                         *ISTAT SU ADG/CONSEG
063100001219     C                   Z-ADD     3             VS(68)                         *ISTAT SU ADG/CONSEG
063200001219     C                   Z-ADD     3             VS(69)                         *ISTAT SU ADG/CONSEG
063300001219     C                   Z-ADD     3             VS(70)                         *ISTAT SU ADG/CONSEG
063400001219     C                   Z-ADD     3             VS(71)                         *ISTAT SU ADG/CASSEG
063500001219     C                   Z-ADD     3             VS(72)                         *ISTAT SU ADG/CASSEG
063600001219     C                   Z-ADD     3             VS(73)                         *ISTAT SU ADG/RITIRO
063700001219     C                   Z-ADD     3             VS(77)                         *ISTAT SU ADG/ASSIC
063800001219     C                   Z-ADD     3             VS(78)                         *ISTAT SU ADG/ASSIC
063900001219     C                   Z-ADD     3             VS(79)                         *ISTAT SU ADG/CONSEG
064000011030     C                   Z-ADD     3             VS(80)                         *CONS.ISOLA
064100011030     C                   Z-ADD     3             VS(81)                         *ESPRESSO SU C.ISOLA
064200011030     C                   Z-ADD     3             VS(82)                         *ADG X ESPRES/C.ISOL
064300011030     C                   Z-ADD     3             VS(83)                         *ISTAT X ESPRES/C.IS
064400011030     C                   Z-ADD     3             VS(84)                         *ISTAT X ADG/ESP/CIS
064500011030     C                   Z-ADD     3             VS(85)                         *ADG X CONS.ISOLA
064600011030     C                   Z-ADD     3             VS(86)                         *ISTAT X CONS.ISOLA
064700011030     C                   Z-ADD     3             VS(87)                         *ISTAT X ADG/C.ISOLA
064800011030     C                   Z-ADD     3             VS(88)                         *RITIRO ISOLA
064900011030     C                   Z-ADD     3             VS(89)                         *ESPRESSO SU R.ISOLA
065000011030     C                   Z-ADD     3             VS(90)                         *ADG X ESPRES/R.ISOL
065100011030     C                   Z-ADD     3             VS(91)                         *ISTAT X ESPRES/R.IS
065200011030     C                   Z-ADD     3             VS(92)                         *ISTAT X ADG/ESP/RIS
065300011030     C                   Z-ADD     3             VS(93)                         *ADG X RITIRO ISOLA
065400011030     C                   Z-ADD     3             VS(94)                         *ISTAT X RITIROISOLA
065500011030     C                   Z-ADD     3             VS(95)                         *ISTAT X ADG/R.ISOLA
065600040826     C                   Z-ADD     3             VS(96)                         *ISTAT X ADG/R.ISOLA
065700040826     C                   Z-ADD     3             VS(97)                         *ISTAT X ADG/R.ISOLA
065800040826     C                   Z-ADD     3             VS(98)                         *ISTAT X ADG/R.ISOLA
065900040826     C                   Z-ADD     3             VS(99)                         *ISTAT X ADG/R.ISOLA
066000040826     C                   Z-ADD     3             VS(100)                        *ISTAT X ADG/R.ISOLA
066100040826     C                   Z-ADD     3             VS(101)                        *ISTAT X ADG/R.ISOLA
066200040826     C                   Z-ADD     3             VS(102)                        *ISTAT X ADG/R.ISOLA
066300040826     C                   Z-ADD     3             VS(103)                        *ISTAT X ADG/R.ISOLA
066400040826     C                   Z-ADD     3             VS(104)                        *ISTAT X ADG/R.ISOLA
066500060314     C                   Z-ADD     3             VS(105)                        *ISTAT X ADG/R.ISOLA
066600060314     C                   Z-ADD     3             VS(106)                        *ISTAT X ADG/R.ISOLA
066700060314     C                   Z-ADD     3             VS(107)                        *ISTAT X ADG/R.ISOLA
066800060314     C                   Z-ADD     3             VS(108)                        *ISTAT X ADG/R.ISOLA
066900060314     C                   Z-ADD     3             VS(109)                        *ISTAT X ADG/R.ISOLA
067000060314     C                   Z-ADD     3             VS(110)                        *ISTAT X ADG/R.ISOLA
067100151120     C                   Z-ADD     3             VS(111)                        *ISTAT X ADG/R.ISOLA
067200151120     C                   Z-ADD     3             VS(112)                        *ISTAT X ADG/R.ISOLA
067300151120     C                   Z-ADD     3             VS(113)                        *ISTAT X ADG/R.ISOLA
067400151120     C                   Z-ADD     3             VS(114)                        *ISTAT X ADG/R.ISOLA
067500151120     C                   Z-ADD     3             VS(115)                        *ISTAT X ADG/R.ISOLA
067600151120     C                   Z-ADD     3             VS(116)                        *ISTAT X ADG/R.ISOLA
067700011030     C*
067800970306     C                   ENDSR
067900970306     C*------------------------------------------------------------------------*
068000970306     C* RAGVOC - DETERMINA IL RAGGRUPPAMENTO DELTA DELLE VOCI
068100970306     C*------------------------------------------------------------------------*
068200970306     C     RAGVOC        BEGSR
068300970717     C* PORTO
068400970717     C                   Z-ADD     001           VR(1)                          *PORTO
068500970717     C* EXTRA-PORTO
068600980702     C                   Z-ADD     002           VR(2)                          *DIRITTO FISSO
068700970717     C                   Z-ADD     002           VR(3)                          *ADG X PORTO
068800970717     C                   Z-ADD     002           VR(4)                          *ISTAT X PORTO
068900970717     C                   Z-ADD     002           VR(5)                          *TCP
069000970717     C                   Z-ADD     002           VR(6)                          *INOLTRO
069100970717     C                   Z-ADD     002           VR(17)                         *RITIRO
069200970717     C                   Z-ADD     002           VR(21)                         *ADG X INOLTRO
069300970717     C                   Z-ADD     002           VR(23)                         *ADG X ESPRES/PORTO
069400980702     C                   Z-ADD     002           VR(22)                         *ADG X DIR.FISSO
069500970717     C                   Z-ADD     002           VR(24)                         *ISTAT X INOLTRO
069600980702     C                   Z-ADD     002           VR(25)                         *ISTAT X DIR.FISSO
069700970717     C                   Z-ADD     002           VR(26)                         *ISTAT X ESPRES/PORT
069800970717     C                   Z-ADD     002           VR(27)                         *ISTAT X ADG/PORTO
069900970717     C                   Z-ADD     002           VR(28)                         *ISTAT X ADG/D.FISSO
070000970717     C                   Z-ADD     002           VR(29)                         *ISTAT X ADG/INOLTRO
070100970717     C                   Z-ADD     002           VR(30)                         *ISTAT X ADG/ESP/POR
070200970721     C                   Z-ADD     002           VR(34)                         *ISTAT X ADG/ESP/INO
070300970717     C                   Z-ADD     002           VR(37)                         *ADG X ESPRES/INOLT
070400970717     C                   Z-ADD     002           VR(38)                         *ISTAT X ESPRES/INOL
070500970721     C                   Z-ADD     002           VR(40)                         *ADG X ESPRES/L.DISA
070600970721     C                   Z-ADD     002           VR(41)                         *ISTAT X ESPRES/L.DI
070700970721     C                   Z-ADD     002           VR(42)                         *ISTAT X ADG/ESP/LDI
070800971023     C                   Z-ADD     002           VR(43)                         *ADG X L.DISAGIATE
070900971023     C                   Z-ADD     002           VR(44)                         *ISTAT X L.DISAGIATE
071000971023     C                   Z-ADD     002           VR(45)                         *ISTAT X ADG/L.DISAG
071100001219     C                   Z-ADD     002           VR(46)                         *ADG SU TCP
071200001219     C                   Z-ADD     002           VR(47)                         *ADG SU GIACENZA VAR
071300001219     C                   Z-ADD     002           VR(48)                         *ADG SU ASSIC X CONT
071400001219     C                   Z-ADD     002           VR(49)                         *ADG SU ASSIC RCV  F
071500001219     C                   Z-ADD     002           VR(50)                         *ADG SU CONSEGNA APP
071600001219     C                   Z-ADD     002           VR(51)                         *ADG SU CONSEGNA SUP
071700001219     C                   Z-ADD     002           VR(52)                         *ADG SU CONSEGNA PIA
071800001219     C                   Z-ADD     002           VR(53)                         *ADG SU CONSEGNA FUO
071900001219     C                   Z-ADD     002           VR(54)                         *ADG SU C/ASSEGNO MI
072000001219     C                   Z-ADD     002           VR(55)                         *ADG SU C/ASSEGNO BA
072100001219     C                   Z-ADD     002           VR(56)                         *ADG SU RITIRO
072200001219     C                   Z-ADD     002           VR(57)                         *ADG SU GIACENZA CON
072300001219     C                   Z-ADD     002           VR(58)                         *ADG SU GIACENZA CON
072400001219     C                   Z-ADD     002           VR(59)                         *ADG SU GIACENZA SOS
072500001219     C                   Z-ADD     002           VR(60)                         *ADG SU ASSIC X CONT
072600001219     C                   Z-ADD     002           VR(61)                         *ADG SU ASSIC RCV AS
072700001219     C                   Z-ADD     002           VR(62)                         *ADG SU CONSEGNA DDT
072800001219     C                   Z-ADD     002           VR(63)                         *ISTAT SU ADG/TCP
072900001219     C                   Z-ADD     002           VR(64)                         *ISTAT SU ADG/GIAC.V
073000001219     C                   Z-ADD     002           VR(65)                         *ISTAT SU ADG/ASSXCO
073100001219     C                   Z-ADD     002           VR(66)                         *ISTAT SU ADG/ASS RC
073200001219     C                   Z-ADD     002           VR(67)                         *ISTAT SU ADG/CONSEG
073300001219     C                   Z-ADD     002           VR(68)                         *ISTAT SU ADG/CONSEG
073400001219     C                   Z-ADD     002           VR(69)                         *ISTAT SU ADG/CONSEG
073500001219     C                   Z-ADD     002           VR(70)                         *ISTAT SU ADG/CONSEG
073600001219     C                   Z-ADD     002           VR(71)                         *ISTAT SU ADG/CASSEG
073700001219     C                   Z-ADD     002           VR(72)                         *ISTAT SU ADG/CASSEG
073800001219     C                   Z-ADD     002           VR(73)                         *ISTAT SU ADG/RITIRO
073900001219     C                   Z-ADD     002           VR(74)                         *ISTAT SU ADG/GIACEN
074000001219     C                   Z-ADD     002           VR(75)                         *ISTAT SU ADG/GIACEN
074100001219     C                   Z-ADD     002           VR(76)                         *ISTAT SU ADG/GIACEN
074200001219     C                   Z-ADD     002           VR(77)                         *ISTAT SU ADG/ASSIC
074300001219     C                   Z-ADD     002           VR(78)                         *ISTAT SU ADG/ASSIC
074400001219     C                   Z-ADD     002           VR(79)                         *ISTAT SU ADG/CONSEG
074500011030     C                   Z-ADD     002           VR(82)                         *ADG X ESPR/C.ISOLA
074600011030     C                   Z-ADD     002           VR(83)                         *ISTAT X ESPRES/C.IS
074700011030     C                   Z-ADD     002           VR(84)                         *ISTAT X ADG/ESP/CIS
074800011030     C                   Z-ADD     002           VR(85)                         *ADG X CONS.ISOLA
074900011030     C                   Z-ADD     002           VR(86)                         *ISTAT X CONS.ISOLA
075000011030     C                   Z-ADD     002           VR(87)                         *ISTAT X ADG/C.ISOLA
075100011031     C                   Z-ADD     002           VR(90)                         *ADG X ESPR/R.ISOLA
075200011031     C                   Z-ADD     002           VR(91)                         *ISTAT X ESPRES/R.IS
075300011031     C                   Z-ADD     002           VR(92)                         *ISTAT X ADG/ESP/RIS
075400011031     C                   Z-ADD     002           VR(93)                         *ADG X RITIRO ISOLA
075500011031     C                   Z-ADD     002           VR(94)                         *ISTAT X RITIROISOLA
075600011031     C                   Z-ADD     002           VR(95)                         *ISTAT X ADG/R.ISOLA
075700040826     C                   Z-ADD     002           VR(97)                         *ISTAT X ADG/R.ISOLA
075800040826     C                   Z-ADD     002           VR(98)                         *ISTAT X ADG/R.ISOLA
075900040826     C                   Z-ADD     002           VR(100)                        *ISTAT X ADG/R.ISOLA
076000040826     C                   Z-ADD     002           VR(101)                        *ISTAT X ADG/R.ISOLA
076100040826     C                   Z-ADD     002           VR(102)                        *ISTAT X ADG/R.ISOLA
076200040826     C                   Z-ADD     002           VR(103)                        *ISTAT X ADG/R.ISOLA
076300040826     C                   Z-ADD     002           VR(104)                        *ISTAT X ADG/R.ISOLA
076400070118     C                   Z-ADD     002           VR(106)                        *ISTAT X ADG/R.ISOLA
076500070118     C                   Z-ADD     002           VR(107)                        *ISTAT X ADG/R.ISOLA
076600070118     C                   Z-ADD     002           VR(109)                        *ISTAT X ADG/R.ISOLA
076700070118     C                   Z-ADD     002           VR(110)                        *ISTAT X ADG/R.ISOLA
076800151120     C                   Z-ADD     002           VR(111)                        *ISTAT X ADG/R.ISOLA
076900160121     C                   Z-ADD     002           VR(112)                        *ISTAT X ADG/R.ISOLA
077000160121     C                   Z-ADD     002           VR(113)                        *ISTAT X ADG/R.ISOLA
077100160121     C                   Z-ADD     002           VR(115)                        *ISTAT X ADG/R.ISOLA
077200160121     C                   Z-ADD     002           VR(116)                        *ISTAT X ADG/R.ISOLA
077300151120     C* CONSEGNA AI PIANI
077400151120     C                   Z-ADD     003           VR(13)                         *CONS. AI PIANI
077500970717     C* CONSEGNA AI SUPERMERCATI
077600970717     C                   Z-ADD     004           VR(12)                         *CONS. SUPERMERCATI
077700970717     C* CONSEGNA SU APPUNTAMENTO
077800970717     C                   Z-ADD     005           VR(11)                         *CONS. APPUNTAMENTO
077900970717     C* CONSEGNA FUORI MISURA
078000970717     C                   Z-ADD     006           VR(14)                         *CONS. FUORI MISURA
078100970717     C* C/ASSEGNO
078200970717     C                   Z-ADD     007           VR(15)                         *C/ASSEGNO MITTENTE
078300970717     C                   Z-ADD     007           VR(16)                         *C/ASSEGNO BARTOLINI
078400970717     C* GIACENZA
078500970306     C                   Z-ADD     008           VR(8)                          *GIACENZA
078600970717     C                   Z-ADD     008           VR(18)                         *GIACENZA
078700970717     C                   Z-ADD     008           VR(19)                         *GIACENZA
078800970717     C                   Z-ADD     008           VR(20)                         *GIACENZA
078900970717     C* ASSICURAZIONE
079000970717     C                   Z-ADD     009           VR(9)                          *ASS.CONTO FRANCO
079100970717     C                   Z-ADD     009           VR(10)                         *ASS.RCV   FRANCO
079200970717     C                   Z-ADD     009           VR(31)                         *ASS.CONTO ASSEGNATO
079300970717     C                   Z-ADD     009           VR(32)                         *ASS.RCV ASSEGNATO
079400060314     C                   Z-ADD     009           VR(105)                        *ASS.RCV ASSEGNATO
079500060314     C                   Z-ADD     009           VR(108)                        *ASS.RCV ASSEGNATO
079600970717     C* ESPRESSO
079700970717     C                   Z-ADD     010           VR(7)                          *ESPRESSO PORTO
079800970717     C                   Z-ADD     010           VR(36)                         *ESPRESSO INOLTRO
079900970721     C                   Z-ADD     010           VR(39)                         *ESPRESSO L.DISAGIAT
080000011030     C                   Z-ADD     010           VR(81)                         *ESPRESSO CONS.ISOLA
080100011031     C                   Z-ADD     010           VR(89)                         *ESPRESSO RIT. ISOLA
080200970717     C* CONSEGNA DDT
080300970414     C                   Z-ADD     011           VR(33)                         *CONS. DDT
080400970717     C* CONSEGNA LOCALITA' DISAGIATA
080500970414     C                   Z-ADD     013           VR(35)                         *CONS. LOC.DISAGIATE
080600011030     C* CONSEGNA ISOLA
080700011030     C                   Z-ADD     016           VR(80)                         *CONS. ISOLA
080800050808     C* RITIRO ISOLA
080900050808     C                   Z-ADD     016           VR(88)                         *RITIRO ISOLA
081000050808     C* destinatario disagiato
081100050808     C                   Z-ADD     018           VR(96)                         *CONS. LOC.DISAGIATE
081200050808     C* bancali da rendere al cliente
081300050808     C                   Z-ADD     019           VR(99)                         *CONS. LOC.DISAGIATE
081400151120     C* CONSEGNA packing list
081500151120     C                   Z-ADD     021           VR(114)                        *CONS. AI PIANI
081600970306     C*
081700970306     C                   ENDSR
081800970306     C*------------------------------------------------------------------------*
081900970306     C* CHKVOC - DETERMINA LE VOCI DA CALCOLARE
082000970306     C*------------------------------------------------------------------------*
082100970306     C     CHKVOC        BEGSR
082200970306     C*
082300970306     C* IMPOSTA A NO I FLAG "DA CALCOLARE" DI TUTTE LE VOCI
082400970306     C                   MOVEL     'N'           VC                             *VOCE DA CALCOLARE
082500970306     C                   Z-ADD     100           VP                             *% DA CONSIDERARE
082600970318     C                   MOVEL     'N'           VI                             *INGLOBA S/N
082700970306     C*
082800970306     C* IMPOSTA A SI I FLAG "DA CALCOLARE" DELLE VOCI SEMPRE PRESENTI
082900970306     C                   MOVEL     'S'           VC(1)                          *PORTO
083000970306     C                   MOVEL     'S'           VC(2)                          *DIRITTO FISSO
083100970306     C                   MOVEL     'S'           VC(3)                          *A.D.G X PORTO
083200970306     C                   MOVEL     'S'           VC(22)                         *A.D.G X DIR.FISSO
083300970306     C                   MOVEL     'S'           VC(4)                          *ISTAT X PORTO
083400970306     C                   MOVEL     'S'           VC(25)                         *ISTAT X DIR.FISSO
083500970306     C                   MOVEL     'S'           VC(27)                         *ISTAT X ADG/PORTO
083600970306     C                   MOVEL     'S'           VC(28)                         *ISTAT X ADG/D.FISSO
083700970306     C                   MOVEL     'S'           VC(5)                          *T.C.P.
083800001222     C                   MOVEL     'S'           VC(46)                         *ADG SU TCP
083900001222     C                   MOVEL     'S'           VC(63)                         *ISTAT SU ADG/TCP
084000970306     C*
084100970306     C* CONTROLLA SE ESISTE =======> INOLTRO
084200970306IF  1C     ETCPPV        IFGT      *ZEROS                                       *% SPED.IN PROVINCIA
084300970306     C                   MOVEL     'S'           VC(6)                          *INOLTRO
084400970306     C                   Z-ADD     ETCPPV        VP(6)                          *% DA CONSIDERARE
084500970314     C                   MOVEL     ETCSIN        VI(6)                          *INGLOBA S/N
084600970306     C                   MOVEL     'S'           VC(21)                         *A.D.G X INOLTRO
084700970306     C                   Z-ADD     ETCPPV        VP(21)                         *% DA CONSIDERARE
084800970306     C                   MOVEL     'S'           VC(24)                         *ISTAT X INOLTRO
084900970306     C                   Z-ADD     ETCPPV        VP(24)                         *% DA CONSIDERARE
085000971023     C                   MOVEL     'S'           VC(29)                         *ISTAT X ADG/INOLTRO
085100971023     C                   Z-ADD     ETCPPV        VP(29)                         *% DA CONSIDERARE
085200970306E   1C                   ENDIF
085300970306     C*
085400970717     C* CONTROLLA SE ESISTE =======> ESPRESSO PORTO
085500970306IF  1C     ETCESP        IFGT      *ZEROS
085600970717     C                   MOVEL     'S'           VC(7)                          *ESPRESSO PORTO
085700970306     C                   Z-ADD     ETCESP        VP(7)                          *% DA CONSIDERARE
085800970306     C                   MOVEL     ETCIES        VI(7)                          *INGLOBA S/N
085900970717     C                   MOVEL     'S'           VC(23)                         *ADG X ESP/PORTO
086000970306     C                   Z-ADD     ETCESP        VP(23)                         *% DA CONSIDERARE
086100970717     C                   MOVEL     'S'           VC(26)                         *ISTAT X ESP/PORTO
086200970306     C                   Z-ADD     ETCESP        VP(26)                         *% DA CONSIDERARE
086300970717     C                   MOVEL     'S'           VC(30)                         *ISTAT X ADG/ESP/POR
086400970717     C                   Z-ADD     ETCESP        VP(30)                         *% DA CONSIDERARE
086500970306E   1C                   ENDIF
086600970717     C*
086700970717     C* CONTROLLA SE ESISTE =======> ESPRESSO INOLTRO
086800970717IF  1C     ETCESP        IFGT      *ZEROS
086900970717IF  2C     ETCPPV        IFGT      *ZEROS
087000980928     C     ETCPPV        MULT      ETCESP        WN95             10 5
087100970717     C     WN95          DIV       100           WN95
087200970717IF  3C     WN95          IFGT      *ZEROS
087300970717     C                   MOVEL     'S'           VC(36)                         *ESPRESSO INOLTRO
087400970717     C                   Z-ADD     WN95          VP(36)                         *% DA CONSIDERARE
087500970717     C                   MOVEL     ETCIES        VI(36)                         *INGLOBA S/N
087600970717     C                   MOVEL     'S'           VC(37)                         *ADG X ESP/INOLTRO
087700970717     C                   Z-ADD     WN95          VP(37)                         *% DA CONSIDERARE
087800970717     C                   MOVEL     'S'           VC(38)                         *ISTAT X ESP/INOLTRO
087900970717     C                   Z-ADD     WN95          VP(38)                         *% DA CONSIDERARE
088000970721     C                   MOVEL     'S'           VC(34)                         *ISTAT X ADG/ESP/INO
088100970721     C                   Z-ADD     WN95          VP(34)                         *% DA CONSIDERARE
088200970717E   3C                   ENDIF
088300970717E   2C                   ENDIF
088400970717E   1C                   ENDIF
088500970721     C*
088600970721     C* CONTROLLA SE ESISTE =======> ESPRESSO LOCALITA' DISAGIATE
088700970721IF  1C     ETCESP        IFGT      *ZEROS
088800970721IF  2C     ETCPCL        IFGT      *ZEROS
088900980928     C     ETCPCL        MULT      ETCESP        WN95
089000970721     C     WN95          DIV       100           WN95
089100970721IF  3C     WN95          IFGT      *ZEROS
089200970721     C                   MOVEL     'S'           VC(39)                         *ESPRESSO L.DISAG.
089300970721     C                   Z-ADD     WN95          VP(39)                         *% DA CONSIDERARE
089400970721     C                   MOVEL     ETCIES        VI(39)                         *INGLOBA S/N
089500970721     C                   MOVEL     'S'           VC(40)                         *ADG X ESP/L.DISAG.
089600970721     C                   Z-ADD     WN95          VP(40)                         *% DA CONSIDERARE
089700970721     C                   MOVEL     'S'           VC(41)                         *ISTAT X ESP/L.DISAG
089800970721     C                   Z-ADD     WN95          VP(41)                         *% DA CONSIDERARE
089900970721     C                   MOVEL     'S'           VC(42)                         *ISTAT X ADG/ESP/LDI
090000970721     C                   Z-ADD     WN95          VP(42)                         *% DA CONSIDERARE
090100970721E   3C                   ENDIF
090200970721E   2C                   ENDIF
090300970721E   1C                   ENDIF
090400970306     C*
090500970306     C* CONTROLLA SE ESISTE =======> GIACENZA VARIE
090600970306IF  1C     ETCPGI        IFGT      *ZEROS
090700970306     C                   MOVEL     'S'           VC(8)                          *GIACENZA -FISSE-
090800970306     C                   Z-ADD     ETCPGI        VP(8)                          *% DA CONSIDERARE
090900970306     C                   MOVEL     ETCIGI        VI(8)                          *INGLOBA S/N
091000001222     C                   MOVEL     'S'           VC(47)                         *ADG SU GIACENZA VAR
091100001222     C                   Z-ADD     ETCPGI        VP(47)                         *% DA CONSIDERARE
091200001227     C                   MOVEL     'S'           VC(64)                         *ISTAT SU ADG/GIAC.V
091300001227     C                   Z-ADD     ETCPGI        VP(64)                         *% DA CONSIDERARE
091400970306     C                   MOVEL     'S'           VC(20)                         *GIACENZA -VARIABILI
091500970306     C                   Z-ADD     ETCPGI        VP(20)                         *% DA CONSIDERARE
091600970306     C                   MOVEL     ETCIGI        VI(20)                         *INGLOBA S/N
091700001227     C                   MOVEL     'S'           VC(59)                         *ADG SU GIACENZA SOS
091800001227     C                   Z-ADD     ETCPGI        VP(59)                         *% DA CONSIDERARE
091900001227     C                   MOVEL     'S'           VC(76)                         *ISTAT SU ADG/GIACEN
092000001227     C                   Z-ADD     ETCPGI        VP(76)                         *% DA CONSIDERARE
092100970306E   1C                   ENDIF
092200970306     C*
092300060314     C* CONTROLLA SE ESISTE =======> ASSICURAZIONE AC PLUS
092400970306IF  1C     ETCPAC        IFGT      *ZEROS
092500970306IF  2C     ETCFRA        IFGT      *ZEROS
092600980928     C     ETCFRA        MULT      ETCPAC        WN95
092700970306     C     WN95          DIV       100           WN95
092800970306IF  3C     WN95          IFGT      *ZEROS
092900970306     C                   MOVEL     'S'           VC(9)                          *ASS.CONTO FRANCO
093000970306     C                   Z-ADD     WN95          VP(9)                          *% DA CONSIDERARE
093100970306     C                   MOVEL     ETCIAS        VI(9)                          *INGLOBA S/N
093200001227     C                   MOVEL     'S'           VC(48)                         *ADG SU ASSIC X CONT
093300001227     C                   Z-ADD     WN95          VP(48)                         *% DA CONSIDERARE
093400001227     C                   MOVEL     'S'           VC(65)                         *ISTAT SU ADG/ASSXCO
093500001227     C                   Z-ADD     WN95          VP(65)                         *% DA CONSIDERARE
093600970306E   3C                   ENDIF
093700970306E   2C                   ENDIF
093800970306E   1C                   ENDIF
093900970306     C*
094000060314     C* CONTROLLA SE ESISTE =======> ASSICURAZIONE AC PLUS
094100970306IF  1C     ETCPAC        IFGT      *ZEROS
094200970306IF  2C     ETCASS        IFGT      *ZEROS
094300980928     C     ETCASS        MULT      ETCPAC        WN95
094400970306     C     WN95          DIV       100           WN95
094500970306IF  3C     WN95          IFGT      *ZEROS
094600970306     C                   MOVEL     'S'           VC(31)                         *ASS.CONTO ASSEGNATO
094700970306     C                   Z-ADD     WN95          VP(31)                         *% DA CONSIDERARE
094800970306     C                   MOVEL     ETCIAS        VI(31)                         *INGLOBA S/N
094900001227     C                   MOVEL     'S'           VC(60)                         *ADG SU ASSIC X CONT
095000001227     C                   Z-ADD     WN95          VP(60)                         *% DA CONSIDERARE
095100001227     C                   MOVEL     'S'           VC(77)                         *ISTAT SU ADG/ASSIC
095200001227     C                   Z-ADD     WN95          VP(77)                         *% DA CONSIDERARE
095300970306E   3C                   ENDIF
095400970306E   2C                   ENDIF
095500970306E   1C                   ENDIF
095600060314     C* CONTROLLA SE ESISTE =======> ASSICURAZIONE AC BASE
095700060314IF  1C     ETCxx1        IFGT      *ZEROS
095800060314IF  2C     ETCFRA        IFGT      *ZEROS
095900060314     C     ETCFRA        MULT      ETCxx1        WN95
096000060314     C     WN95          DIV       100           WN95
096100060314IF  3C     WN95          IFGT      *ZEROS
096200060314     C                   MOVEL     'S'           VC(105)                        *ASS.CONTO FRANCO
096300060314     C                   Z-ADD     WN95          VP(105)                        *% DA CONSIDERARE
096400060314     C                   MOVEL     'N'           VI(105)                        *INGLOBA S/N
096500060314     C                   MOVEL     'S'           VC(106)                        *ADG SU ASSIC X CONT
096600060314     C                   Z-ADD     WN95          VP(106)                        *% DA CONSIDERARE
096700060314     C                   MOVEL     'S'           VC(107)                        *ISTAT SU ADG/ASSXCO
096800060314     C                   Z-ADD     WN95          VP(107)                        *% DA CONSIDERARE
096900060314E   3C                   ENDIF
097000060314E   2C                   ENDIF
097100060314E   1C                   ENDIF
097200060314     C*
097300060314     C* CONTROLLA SE ESISTE =======> ASSICURAZIONE AC BASE
097400060314IF  1C     ETCxx1        IFGT      *ZEROS
097500060314IF  2C     ETCASS        IFGT      *ZEROS
097600060314     C     ETCASS        MULT      ETCxx1        WN95
097700060314     C     WN95          DIV       100           WN95
097800060314IF  3C     WN95          IFGT      *ZEROS
097900060314     C                   MOVEL     'S'           VC(108)                        *ASS.CONTO ASSEGNATO
098000060314     C                   Z-ADD     WN95          VP(108)                        *% DA CONSIDERARE
098100060314     C                   MOVEL     'N'           VI(108)                        *INGLOBA S/N
098200060314     C                   MOVEL     'S'           VC(109)                        *ADG SU ASSIC X CONT
098300060314     C                   Z-ADD     WN95          VP(109)                        *% DA CONSIDERARE
098400060314     C                   MOVEL     'S'           VC(110)                        *ISTAT SU ADG/ASSIC
098500060314     C                   Z-ADD     WN95          VP(110)                        *% DA CONSIDERARE
098600060314E   3C                   ENDIF
098700060314E   2C                   ENDIF
098800060314E   1C                   ENDIF
098900970306     C*
099000970306     C* CONTROLLA SE ESISTE =======> ASSICURAZIONE RCV FRANCO
099100060314     c* tariffa particolare "R" ormai obsoleta
099200990621IF  1C     ETCPAR        IFGT      *ZEROS
099300990621IF  2C     ETCFRA        IFGT      *ZEROS
099400990621     C     ETCFRA        MULT      ETCPAR        WN95
099500990621     C     WN95          DIV       100           WN95
099600990621IF  3C     WN95          IFGT      *ZEROS
099700990621     C                   MOVEL     'S'           VC(10)                         *ASS.NE RCV FRANCO
099800990621     C                   Z-ADD     WN95          VP(10)                         *% DA CONSIDERARE
099900990621     C                   MOVEL     ETCIAS        VI(10)                         *INGLOBA S/N
100000001227     C                   MOVEL     'S'           VC(49)                         *ADG SU ASSIC RCV  F
100100001227     C                   Z-ADD     WN95          VP(49)                         *% DA CONSIDERARE
100200001227     C                   MOVEL     'S'           VC(66)                         *ISTAT SU ADG/ASS RC
100300001227     C                   Z-ADD     WN95          VP(66)                         *% DA CONSIDERARE
100400990621E   3C                   ENDIF
100500990621E   2C                   ENDIF
100600990621E   1C                   ENDIF
100700970306     C*
100800970306     C* CONTROLLA SE ESISTE =======> ASSICURAZIONE RCV ASSEGNATO
100900060314     c* tariffa particolare "R" ormai obsoleta
101000990621IF  1C     ETCPAR        IFGT      *ZEROS
101100990621IF  2C     ETCASS        IFGT      *ZEROS
101200990621     C     ETCASS        MULT      ETCPAR        WN95
101300990621     C     WN95          DIV       100           WN95
101400990621IF  3C     WN95          IFGT      *ZEROS
101500990621     C                   MOVEL     'S'           VC(32)                         *ASS.NE RCV ASSEGNAT
101600990621     C                   Z-ADD     WN95          VP(32)                         *% DA CONSIDERARE
101700990621     C                   MOVEL     ETCIAS        VI(32)                         *INGLOBA S/N
101800001227     C                   MOVEL     'S'           VC(61)                         *ADG SU ASSIC RCV  A
101900001227     C                   Z-ADD     WN95          VP(61)                         *% DA CONSIDERARE
102000001227     C                   MOVEL     'S'           VC(78)                         *ISTAT SU ADG/ASS RC
102100001227     C                   Z-ADD     WN95          VP(78)                         *% DA CONSIDERARE
102200990621E   3C                   ENDIF
102300990621E   2C                   ENDIF
102400990621E   1C                   ENDIF
102500970306     C*
102600970306     C* CONTROLLA SE ESISTE =======> CONSEGNA SU APPUNTAMENTO
102700970306IF  1C     ETCPCA        IFGT      *ZEROS
102800970306     C                   MOVEL     'S'           VC(11)                         *CONS. APPUNTAMENTO
102900970306     C                   Z-ADD     ETCPCA        VP(11)                         *% DA CONSIDERARE
103000970306     C                   MOVEL     ETCIAP        VI(11)                         *INGLOBA S/N
103100001227     C                   MOVEL     'S'           VC(50)                         *ADG SU CONSEGNA APP
103200001227     C                   Z-ADD     ETCPCA        VP(50)                         *% DA CONSIDERARE
103300001227     C                   MOVEL     'S'           VC(67)                         *ISTAT SU ADG/CONSEG
103400001227     C                   Z-ADD     ETCPCA        VP(67)                         *% DA CONSIDERARE
103500970306E   1C                   ENDIF
103600970306     C*
103700970306     C* CONTROLLA SE ESISTE =======> CONSEGNA AI SUPERMERCATI
103800970306IF  1C     ETCPCS        IFGT      *ZEROS
103900970306     C                   MOVEL     'S'           VC(12)                         *CONS. SUPERMERCATI
104000970306     C                   Z-ADD     ETCPCS        VP(12)                         *% DA CONSIDERARE
104100970306     C                   MOVEL     ETCISU        VI(12)                         *INGLOBA S/N
104200001227     C                   MOVEL     'S'           VC(51)                         *ADG SU CONSEGNA SUP
104300001227     C                   Z-ADD     ETCPCS        VP(51)                         *% DA CONSIDERARE
104400001227     C                   MOVEL     'S'           VC(68)                         *ISTAT SU ADG/CONSEG
104500001227     C                   Z-ADD     ETCPCS        VP(68)                         *% DA CONSIDERARE
104600970306E   1C                   ENDIF
104700970306     C*
104800970306     C* CONTROLLA SE ESISTE =======> CONSEGNA AI PIANI
104900970306IF  1C     ETCPCP        IFGT      *ZEROS
105000970306     C                   MOVEL     'S'           VC(13)                         *CONS. AI PIANI
105100970306     C                   Z-ADD     ETCPCP        VP(13)                         *% DA CONSIDERARE
105200151120     C                                                                          *INGLOBA S/N
105300001227     C                   MOVEL     'S'           VC(52)                         *ADG SU CONSEGNA SUP
105400001227     C                   Z-ADD     ETCPCP        VP(52)                         *% DA CONSIDERARE
105500001227     C                   MOVEL     'S'           VC(69)                         *ISTAT SU ADG/CONSEG
105600001227     C                   Z-ADD     ETCPCP        VP(69)                         *% DA CONSIDERARE
105700970306E   1C                   ENDIF
105800151120     C* CONTROLLA SE ESISTE =======> packing list
105900151120IF  1C     ETCPkl        IFeq      'S'
106000151209     c     d00dtc        andge     20160101
106100151120     C                   MOVEL     'S'           VC(114)                        *CONS. AI PIANI
106200151120     C                   Z-ADD     100           VP(114)                        *% DA CONSIDERARE
106300151120     C                   MOVEL     ETCiva        VI(114)                        *INGLOBA S/N
106400151120     C                   MOVEL     'S'           VC(115)                        *ADG SU CONSEGNA SUP
106500151120     C                   Z-ADD     100           VP(115)                        *% DA CONSIDERARE
106600151120     C                   MOVEL     'S'           VC(116)                        *ISTAT SU ADG/CONSEG
106700151120     C                   Z-ADD     100           VP(116)                        *% DA CONSIDERARE
106800151120E   1C                   ENDIF
106900151120     C* CONTROLLA SE ESISTE =======> ritiro telefonico
107000151120IF  1C     §ETCrtm       IFeq      'S'
107100151209     c     d00dtc        andge     20160101
107200151120     C                   MOVEL     'S'           VC(111)                        *CONS. AI PIANI
107301170410     C                   eval(h)   VP(111) = 100/ETCSPR
107400151120     C                   MOVEL     §ETCirp       VI(111)                        *INGLOBA S/N
107500151120     C                   MOVEL     'S'           VC(112)                        *ADG SU CONSEGNA SUP
107601170410     C                   eval(h)   VP(112) = 100/ETCSPR
107700151120     C                   MOVEL     'S'           VC(113)                        *ISTAT SU ADG/CONSEG
107801170410     C                   eval(h)   VP(113) = 100/ETCSPR
107900151120E   1C                   ENDIF
108000040826     C* CONTROLLA SE ESISTE =======> CONSEGNA destinatario disagiato
108100040826IF  1C     ETCdes        IFGT      *ZEROS
108200040826     C                   MOVEL     'S'           VC(96)                         *CONS. AI PIANI
108300040826     C                   Z-ADD     ETCdes        VP(96)                         *% DA CONSIDERARE
108400040826     C                   MOVEL     'S'           VC(97)                         *ADG SU CONSEGNA SUP
108500040826     C                   Z-ADD     ETCdes        VP(97)                         *% DA CONSIDERARE
108600040826     C                   MOVEL     'S'           VC(98)                         *ISTAT SU ADG/CONSEG
108700040826     C                   Z-ADD     ETCdes        VP(98)                         *% DA CONSIDERARE
108800040826E   1C                   ENDIF
108900970306     C*
109000040826     C* CONTROLLA SE ESISTE =======> bancali BAN
109100040826      * Non si tassano i bancali per le bolle in porto assegnato(vedi TNSF20R)
109200040826IF  1C     ETCnba        IFGT      *ZEROS
109300040826IF  1C     ETCfra        andeq     100
109400040826     C                   MOVEL     'S'           VC(99)                         *CONS. AI PIANI
109500040826     C                   Z-ADD     ETCban        VP(99)                         *% DA CONSIDERARE
109600040826     C                   MOVEL     'S'           VC(100)                        *ADG SU CONSEGNA SUP
109700040826     C                   Z-ADD     ETCban        VP(100)                        *% DA CONSIDERARE
109800040826     C                   MOVEL     'S'           VC(101)                        *ISTAT SU ADG/CONSEG
109900040826     C                   Z-ADD     ETCban        VP(101)                        *% DA CONSIDERARE
110000040826E   1C                   ENDIF
110100040826     C*
110200970306     C* CONTROLLA SE ESISTE =======> CONSEGNA FUORI MISURA
110300970306IF  1C     ETCPCF        IFGT      *ZEROS
110400970306     C                   MOVEL     'S'           VC(14)                         *CONS. FUORI MISURA
110500970306     C                   Z-ADD     ETCPCF        VP(14)                         *% DA CONSIDERARE
110600970306     C                   MOVEL     ETCIFU        VI(14)                         *INGLOBA S/N
110700001227     C                   MOVEL     'S'           VC(53)                         *ADG SU CONSEGNA FUO
110800001227     C                   Z-ADD     ETCPCF        VP(53)                         *% DA CONSIDERARE
110900001227     C                   MOVEL     'S'           VC(70)                         *ISTAT SU ADG/CONSEG
111000001227     C                   Z-ADD     ETCPCF        VP(70)                         *% DA CONSIDERARE
111100970306E   1C                   ENDIF
111200970414     C*
111300970414     C* CONTROLLA SE ESISTE =======> CONSEGNA DDT
111400970414IF  1C     ETCPCD        IFGT      *ZEROS
111500970414     C                   MOVEL     'S'           VC(33)                         *CONS. DDT
111600970414     C                   Z-ADD     ETCPCD        VP(33)                         *% DA CONSIDERARE
111700970414     C                   MOVEL     ETCIDD        VI(33)                         *INGLOBA S/N
111800001227     C                   MOVEL     'S'           VC(62)                         *ADG SU CONSEGNA DDT
111900001227     C                   Z-ADD     ETCPCD        VP(62)                         *% DA CONSIDERARE
112000001227     C                   MOVEL     'S'           VC(79)                         *ISTAT SU ADG/CONSEG
112100001227     C                   Z-ADD     ETCPCD        VP(79)                         *% DA CONSIDERARE
112200970414E   1C                   ENDIF
112300970414     C*
112400970414     C* CONTROLLA SE ESISTE =======> CONSEGNA LOCALITA' DISAGIATE
112500970414IF  1C     ETCPCL        IFGT      *ZEROS
112600970414     C                   MOVEL     'S'           VC(35)                         *CONS. LOC.DISAGIATE
112700970414     C                   Z-ADD     ETCPCL        VP(35)                         *% DA CONSIDERARE
112800970414     C                   MOVEL     ETCILO        VI(35)                         *INGLOBA S/N
112900971023     C                   MOVEL     'S'           VC(43)                         *A.D.G X L.DISAG.
113000971023     C                   Z-ADD     ETCPCL        VP(43)                         *% DA CONSIDERARE
113100971023     C                   MOVEL     'S'           VC(44)                         *ISTAT X L.DISAG.
113200971023     C                   Z-ADD     ETCPCL        VP(44)                         *% DA CONSIDERARE
113300971023     C                   MOVEL     'S'           VC(45)                         *ISTAT X ADG/L.DISAG
113400971023     C                   Z-ADD     ETCPCL        VP(45)                         *% DA CONSIDERARE
113500970414E   1C                   ENDIF
113600970306     C*
113700970306     C* CONTROLLA SE ESISTE =======> CONTRASSEGNO MITTENTE
113800970306IF  1C     ETCPAM        IFGT      *ZEROS
113900970306     C                   MOVEL     'S'           VC(15)                         *C/ASSEGNO MITTENTE
114000970306     C                   Z-ADD     ETCPAM        VP(15)                         *% DA CONSIDERARE
114100970306     C                   MOVEL     ETCICA        VI(15)                         *INGLOBA S/N
114200001227     C                   MOVEL     'S'           VC(54)                         *ADG SU C/ASSEGNO MI
114300001227     C                   Z-ADD     ETCPAM        VP(54)                         *% DA CONSIDERARE
114400001227     C                   MOVEL     'S'           VC(71)                         *ISTAT SU ADG/CASSEG
114500001227     C                   Z-ADD     ETCPAM        VP(71)                         *% DA CONSIDERARE
114600970306E   1C                   ENDIF
114700970306     C*
114800970306     C* CONTROLLA SE ESISTE =======> CONTRASSEGNO BARTOLINI
114900970306IF  1C     ETCPAB        IFGT      *ZEROS
115000970306     C                   MOVEL     'S'           VC(16)                         *C/ASSEGNO BARTOLINI
115100970306     C                   Z-ADD     ETCPAB        VP(16)                         *% DA CONSIDERARE
115200970306     C                   MOVEL     ETCICA        VI(16)                         *INGLOBA S/N
115300001227     C                   MOVEL     'S'           VC(55)                         *ADG SU C/ASSEGNO BA
115400001227     C                   Z-ADD     ETCPAB        VP(55)                         *% DA CONSIDERARE
115500001227     C                   MOVEL     'S'           VC(72)                         *ISTAT SU ADG/CASSEG
115600001227     C                   Z-ADD     ETCPAB        VP(72)                         *% DA CONSIDERARE
115700970306E   1C                   ENDIF
115800970306     C*
115900970306     C* CONTROLLA SE ESISTE =======> RITIRO
116000970306IF  1C     T             IFGT      *ZEROS                                       *CICLO COD.TASSAZION
116100970306IF  2C     ETCASS        IFGT      *ZEROS
116200971204IF  3C     CFL1(T)       IFEQ      *BLANKS                                      *ARRIVO ITALIA
116300970306IF  4C     ETCPCM        IFNE      100
116400970306     C     100           SUB       ETCPCM        WN63              6 3
116500980928     C     WN63          MULT      ETCASS        WN95
116600970306     C     WN95          DIV       100           WN95
116700970306IF  5C     WN95          IFGT      *ZEROS
116800970306     C                   MOVEL     'S'           VC(17)                         *RITIRO
116900970306     C                   Z-ADD     WN95          VP(17)                         *% DA CONSIDERARE
117000001227     C                   MOVEL     'S'           VC(56)                         *ADG SU RITIRO
117100001227     C                   Z-ADD     WN95          VP(56)                         *% DA CONSIDERARE
117200001227     C                   MOVEL     'S'           VC(73)                         *ISTAT SU ADG/RITIRO
117300001227     C                   Z-ADD     WN95          VP(73)                         *% DA CONSIDERARE
117400970306E   5C                   ENDIF
117500970311E   4C                   ENDIF
117600970311X   3C                   ELSE                                                   *ARRIVO ESTERO
117700970306     C                   MOVEL     'S'           VC(17)                         *RITIRO
117800001227     C                   MOVEL     'S'           VC(56)                         *ADG SU RITIRO
117900001227     C                   MOVEL     'S'           VC(73)                         *ISTAT SU ADG/RITIRO
118000970306E   3C                   ENDIF
118100970306E   2C                   ENDIF
118200970306E   1C                   ENDIF
118300970306     C*
118400970306     C* CONTROLLA SE ESISTE =======> GIACENZA RICONSEGNA PROVINCIA (SOLO 50%)
118500970306IF  1C     ETCPGI        IFGT      *ZEROS
118600970306IF  2C     ETCPPV        IFGT      *ZEROS
118700980928     C     ETCPPV        MULT      ETCPGI        WN95
118800970306     C     WN95          DIV       100           WN95
118900970306     C     WN95          MULT      50            WN95
119000970306     C     WN95          DIV       100           WN95
119100970306IF  3C     WN95          IFGT      *ZEROS
119200970306     C                   MOVEL     'S'           VC(18)                         *GIACENZA
119300970306     C                   Z-ADD     WN95          VP(18)                         *% DA CONSIDERARE
119400970306     C                   MOVEL     ETCIGI        VI(18)                         *INGLOBA S/N
119500001227     C                   MOVEL     'S'           VC(57)                         *ADG SU GIACENZA CON
119600001227     C                   Z-ADD     WN95          VP(57)                         *% DA CONSIDERARE
119700001227     C                   MOVEL     'S'           VC(74)                         *ISTAT SU ADG/GIACEN
119800001227     C                   Z-ADD     WN95          VP(74)                         *% DA CONSIDERARE
119900970306E   3C                   ENDIF
120000970306E   2C                   ENDIF
120100970306E   1C                   ENDIF
120200970306     C*
120300970306     C* CONTROLLA SE ESISTE =======> GIACENZA RICONSEGNA CITTA' (SOLO 50%)
120400970306IF  1C     ETCPGI        IFGT      *ZEROS
120500970306IF  2C     ETCPPV        IFGT      *ZEROS
120600970306     C     100           SUB       ETCPPV        WN63              6 3
120700980928     C     WN63          MULT      ETCPGI        WN95
120800970306     C     WN95          DIV       100           WN95
120900970306     C     WN95          MULT      50            WN95
121000970306     C     WN95          DIV       100           WN95
121100970306IF  3C     WN95          IFGT      *ZEROS
121200970306     C                   MOVEL     'S'           VC(19)                         *GIACENZA
121300970306     C                   Z-ADD     WN95          VP(19)                         *% DA CONSIDERARE
121400970306     C                   MOVEL     ETCIGI        VI(19)                         *INGLOBA S/N
121500001227     C                   MOVEL     'S'           VC(58)                         *ADG SU GIACENZA CON
121600001227     C                   Z-ADD     WN95          VP(58)                         *% DA CONSIDERARE
121700001227     C                   MOVEL     'S'           VC(75)                         *ISTAT SU ADG/GIACEN
121800001227     C                   Z-ADD     WN95          VP(75)                         *% DA CONSIDERARE
121900970306E   3C                   ENDIF
122000970306E   2C                   ENDIF
122100970306E   1C                   ENDIF
122200011030     C*
122300011031     C* CONTROLLA SE ESISTE =======>
122400011031IF  1C     ETCCIS        IFGT      *ZEROS
122500011031IF  2C     T             IFGT      *ZEROS                                       *CICLO COD.TASSAZION
122600011031IF  3C     C§§5(T)       IFEQ      'S'                                          *CTS POTENZ.ISOLA
122700011031     C*                     =======> CONSEGNA ISOLA
122800011030     C                   MOVEL     'S'           VC(80)                         *CONSEGNA ISOLA
122900011030     C                   Z-ADD     ETCCIS        VP(80)                         *% DA CONSIDERARE
123000011030     C                   MOVEL     ETCISO        VI(80)                         *INGLOBA S/N
123100011030     C                   MOVEL     'S'           VC(85)                         *A.D.G X CONS.ISOLA
123200011030     C                   Z-ADD     ETCCIS        VP(85)                         *% DA CONSIDERARE
123300011030     C                   MOVEL     'S'           VC(86)                         *ISTAT X CONS.ISOLA
123400011030     C                   Z-ADD     ETCCIS        VP(86)                         *% DA CONSIDERARE
123500011030     C                   MOVEL     'S'           VC(87)                         *ISTAT X ADG/C.ISOLA
123600011030     C                   Z-ADD     ETCCIS        VP(87)                         *% DA CONSIDERARE
123700011031     C*                     =======> ESPRESSO CONSEGNA ISOLE
123800011031IF  4C     ETCESP        IFGT      *ZEROS
123900011031     C     ETCCIS        MULT      ETCESP        WN95
124000011031     C     WN95          DIV       100           WN95
124100011031IF  5C     WN95          IFGT      *ZEROS
124200011031     C                   MOVEL     'S'           VC(81)                         *ESPRESSO CONS.ISOLE
124300011031     C                   Z-ADD     WN95          VP(81)                         *% DA CONSIDERARE
124400011031     C                   MOVEL     ETCISO        VI(81)                         *INGLOBA S/N
124500011031     C                   MOVEL     'S'           VC(82)                         *ADG X ESP/C.ISOLE
124600011031     C                   Z-ADD     WN95          VP(82)                         *% DA CONSIDERARE
124700011031     C                   MOVEL     'S'           VC(83)                         *ISTAT X ESP/C.ISOLE
124800011031     C                   Z-ADD     WN95          VP(83)                         *% DA CONSIDERARE
124900011031     C                   MOVEL     'S'           VC(84)                         *ISTAT X ADG/ESP/CIS
125000011031     C                   Z-ADD     WN95          VP(84)                         *% DA CONSIDERARE
125100011031E   5C                   ENDIF
125200011031E   4C                   ENDIF
125300011031     C*
125400011030E   3C                   ENDIF
125500011030E   2C                   ENDIF
125600011030E   1C                   ENDIF
125700011030     C*
125800011031     C* CONTROLLA SE ESISTE =======>
125900011031IF  1C     ETCSAM        IFGT      *ZEROS
126000011031IF  2C     T             IFGT      *ZEROS                                       *CICLO COD.TASSAZION
126100011031IF  3C     D00PRI        IFEQ      CCTS(T)                                      *CTS DEL CLIENTE T/O
126200011031IF  4C     C§§5(T)       IFEQ      'S'                                          *CTS POTENZ.ISOLA
126300011031     C*                     =======> RITIRO ISOLA
126400011031     C                   MOVEL     'S'           VC(88)                         *RITIRO ISOLA
126500011031     C                   Z-ADD     ETCSAM        VP(88)                         *% DA CONSIDERARE
126600011031     C                   MOVEL     ETCINR        VI(88)                         *INGLOBA S/N
126700011031     C                   MOVEL     'S'           VC(93)                         *A.D.G X RIT.ISOLA
126800011031     C                   Z-ADD     ETCSAM        VP(93)                         *% DA CONSIDERARE
126900011031     C                   MOVEL     'S'           VC(94)                         *ISTAT X RIT.ISOLA
127000011031     C                   Z-ADD     ETCSAM        VP(94)                         *% DA CONSIDERARE
127100011031     C                   MOVEL     'S'           VC(95)                         *ISTAT X ADG/R.ISOLA
127200011031     C                   Z-ADD     ETCSAM        VP(95)                         *% DA CONSIDERARE
127300011031     C*                     =======> ESPRESSO RITIRO ISOLE
127400011031IF  5C     ETCESP        IFGT      *ZEROS
127500011031     C     ETCSAM        MULT      ETCESP        WN95
127600011031     C     WN95          DIV       100           WN95
127700011031IF  6C     WN95          IFGT      *ZEROS
127800011031     C                   MOVEL     'S'           VC(89)                         *ESPRESSO RIT.ISOLE
127900011031     C                   Z-ADD     WN95          VP(89)                         *% DA CONSIDERARE
128000011031     C                   MOVEL     ETCINR        VI(89)                         *INGLOBA S/N
128100011031     C                   MOVEL     'S'           VC(90)                         *ADG X ESP/RIT.ISOLE
128200011031     C                   Z-ADD     WN95          VP(90)                         *% DA CONSIDERARE
128300011031     C                   MOVEL     'S'           VC(91)                         *ISTAT X ESP/R.ISOLE
128400011031     C                   Z-ADD     WN95          VP(91)                         *% DA CONSIDERARE
128500011031     C                   MOVEL     'S'           VC(92)                         *ISTAT X ADG/ESP/RIS
128600011031     C                   Z-ADD     WN95          VP(92)                         *% DA CONSIDERARE
128700011031E   6C                   ENDIF
128800011031E   5C                   ENDIF
128900011031     C*
129000011031E   4C                   ENDIF
129100011031E   3C                   ENDIF
129200011031E   2C                   ENDIF
129300011030E   1C                   ENDIF
129400970306     C*
129500970306     C                   ENDSR
129600970306     C*------------------------------------------------------------------------*
129700970306     C* VALVO1 - VALORIZZA LE VOCI -FISSE-
129800970306     C*------------------------------------------------------------------------*
129900970306     C     VALVO1        BEGSR
130000970306     C*
130100970306     C* AZZERA GLI IMPORTI DELLE VOCI CON SEQUENZA UNO
130200970306DO  1C     1             DO        NVOCA         I
130300970306IF  2C     VS(I)         IFEQ      1
130400970306     C                   Z-ADD     *ZEROS        VIMP(I)                        *IMPORTO CALCOLATO
130500970306E   2C                   ENDIF
130600970306E   1C                   ENDDO
130700970306     C*
130800970306     C* CALCOLA LE SPESE DI GIACENZA VARIE
130900970306IF  1C     WERR          IFEQ      'N'
131000970306IF  2C     VC(8)         IFEQ      'S'                                          *VOCE DA CALCOLARE
131100970306     C     $TGC          ANDEQ     '1'                                          *TROVATA TAR.GIACENZ
131200970306     C                   EXSR      CALGCV
131300001227     C                   EXSR      CAL047
131400001227     C                   EXSR      CAL064
131500970306E   2C                   ENDIF
131600970306E   1C                   ENDIF
131700970306     C*
131800970306     C                   ENDSR
131900970306     C*------------------------------------------------------------------------*
132000970306     C* CARTGC - REPERISCE LA TARIFFA GIACENZA
132100970306     C*------------------------------------------------------------------------*
132200970306     C     CARTGC        BEGSR
132300970306     C*
132400970625     C* LEGGE LA TARIFFA GIACENZA DEL CLIENTE
132500970625     C                   Z-ADD     D00KSC        KGCKSC                         *CLIENTE
132600970625     C                   Z-ADD     D00CTR        KGCCTR                         *TARIFFA
132700970625     C                   Z-ADD     D00PRG        KGCPRG                         *PROGRESSIVO
132800011019     C                   MOVEL     D00DIV        WDIV
132900091123     c                   if        %subst(d00op0: 1: 1)='O'
133000990819     C     KEYTGC        CHAIN     TIOGC01L                           99
133100980703X   1C                   ELSE                                                   *IN TARIFFA
133200990819     C     KEYTGC        CHAIN     TITGC01L                           99
133300970625E   1C                   ENDIF
133400970306     C*
133500970625     C* LEGGE LA TARIFFA DI CARTELLO SE NON TROVATA TARIFFA/OFFERTA CLIENTE
133600970625IF  1C     *IN99         IFEQ      *ON                                          *NON TROVATA
133700970625     C                   Z-ADD     CARKSC        KGCKSC
133800970625     C                   Z-ADD     CARCTR        KGCCTR
133900970625     C                   Z-ADD     CARPRG        KGCPRG
134000011023     C                   MOVEL     CARDIV        WDIV
134100011023     C     KEYTGC        CHAIN     TITGC01L                           99        *DA TARIFFA CARTELLO
134200970625E   1C                   ENDIF
134300970625     C* TARIFFA NON TROVATA
134400970625IF  1C     *IN99         IFEQ      *ON
134500970625     C                   MOVEL     MSG(23)       $MSG
134600970625     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
134700970625     C* TARIFFA TROVATA
134800970306X   1C                   ELSE
134900991125     C*
135000991125     C* CONTROLLA SE CONVERTIRE I CAMPI DI TITPD
135100011026IF  2C     WDIV          IFNE      WDIVCE
135200011026     C                   MOVEL     WDIV          WDVI              3
135300991125     C                   EXSR      CNVTGC
135400991125E   2C                   ENDIF
135500970625     C                   MOVEL     '1'           $TGC                           *TARIFFA TROVATA
135600970306E   1C                   ENDIF
135700970306     C*
135800970306     C                   ENDSR
135900970306     C*------------------------------------------------------------------------*
136000970306     C* CALGCV - CALCOLA LE SPESE DI GIACENZA VARIE
136100970306     C*------------------------------------------------------------------------*
136200970306     C     CALGCV        BEGSR
136300970306     C*
136400970306     C* REIMPOSTA LE VARIABILI DI LAVORO
136500011031     C                   Z-ADD     *ZEROS        TASSGV           21 8
136600011031     C                   Z-ADD     *ZEROS        TASSGD           21 8
136700970306     C*
136800970306     C* CALCOLA LE SPESE VARIE
136900970306IF  1C     TGCSGV        IFGT      *ZEROS                                       *IMMESSA TARIFFA
137000970306     C                   Z-ADD     TGCSGV        TASSGV
137100060125     c                   if        (tgcfaf = '2' or tgcfaf='3') and
137200060125     c                             etcggg <= tgcsgf
137300060124     C                   Z-ADD     *zeros        TASSGV
137400970306E   1C                   ENDIF
137500060124E   1C                   ENDIF
137600970306     C*
137700970306     C* CALCOLA LE SPESE DOSSIER
137800970306IF  1C     TGCSGD        IFGT      *ZEROS                                       *IMMESSA TARIFFA
137900970306     C     TGCSGD        MULT      ETCNRG        TASSGD
138000970306     C*
138100970306     C* AZZERA LE SPESE DOSSIER SE SI E' DENTRO AI GIORNI DI FRANCHIGIA
138200060125     c                   if        (tgcfaf = '2' or tgcfaf='3') and
138300060124     c                             etcggg <= tgcsgf
138400970306     C                   Z-ADD     *ZEROS        TASSGD                         *NO SPESE DOSSIER
138500060124E   2C                   ENDIF
138600970306E   1C                   ENDIF
138700970306     C*
138800970314IF  1C     TASSGV        IFGT      *ZEROS
138900970314     C     TASSGD        ORGT      *ZEROS
139000970306     C                   Z-ADD     8             I
139100970306     C     TASSGV        ADD       TASSGD        VIMP(I)
139200970314E   1C                   ENDIF
139300970306     C*
139400970306     C                   ENDSR
139500001227     C*------------------------------------------------------------------------*
139600001227     C* CAL047 - CALCOLA ADG SU GIACENZA VARIE DOSSIER
139700001227     C*------------------------------------------------------------------------*
139800001227     C     CAL047        BEGSR
139900001227     C*
140000001227     C* REIMPOSTA LE VARIABILI DI LAVORO
140100011031     C                   Z-ADD     *ZEROS        TASADG           21 8
140200001227     C*
140300001227     C* CALCOLA L'A.D.G.
140400011031     C                   Z-ADD     VIMP(8)       WIMP             23 8
140500001227     C                   EXSR      ADGCAL
140600001227     C*
140700001227IF  1C     TASADG        IFGT      *ZEROS
140800001227     C                   Z-ADD     47            I
140900001227     C                   Z-ADD     TASADG        VIMP(I)
141000001227E   1C                   ENDIF
141100001227     C*
141200001227     C                   ENDSR
141300001227     C*------------------------------------------------------------------------*
141400001227     C* CAL064 - CALCOLA ISTAT SU ADG/GIAC.VARIE DOSSIER
141500001227     C*------------------------------------------------------------------------*
141600001227     C     CAL064        BEGSR
141700001227     C*
141800001227     C* REIMPOSTA LE VARIABILI DI LAVORO
141900011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
142000001227     C*
142100001227     C* CALCOLA L'ISTAT
142200011031     C                   Z-ADD     VIMP(47)      WIMP             23 8
142300001227     C                   EXSR      IACCAL
142400001227     C*
142500001227IF  1C     TASIAC        IFGT      *ZEROS
142600001227     C                   Z-ADD     64            I
142700001227     C                   Z-ADD     TASIAC        VIMP(I)
142800001227E   1C                   ENDIF
142900001227     C*
143000001227     C                   ENDSR
143100970306     C*------------------------------------------------------------------------*
143200970306     C* VALVO2 - VALORIZZA LE VOCI -DIPENDENTI DA SCAGLIONI-
143300970306     C*------------------------------------------------------------------------*
143400970306     C     VALVO2        BEGSR
143500970306     C*
143600970306     C* AZZERA GLI IMPORTI DELLE VOCI CON SEQUENZA DUE
143700970306DO  1C     1             DO        NVOCA         I
143800970306IF  2C     VS(I)         IFEQ      2
143900970306     C                   Z-ADD     *ZEROS        VIMP(I)                        *IMPORTO CALCOLATO
144000970306E   2C                   ENDIF
144100970306E   1C                   ENDDO
144200970306     C*
144300970306     C* CALCOLA LE SPESE DI GIACENZA DI RICONSEGNA PROVINCIA
144400970306IF  1C     WERR          IFEQ      'N'
144500970306IF  2C     VC(18)        IFEQ      'S'                                          *VOCE DA CALCOLARE
144600970306     C     $TGC          ANDEQ     '1'                                          *TROVATA TAR.GIACENZ
144700970306     C                   EXSR      CALGRP
144800001227     C                   EXSR      CAL057
144900001227     C                   EXSR      CAL074
145000970306E   2C                   ENDIF
145100970306E   1C                   ENDIF
145200970306     C*
145300970306     C* CALCOLA LE SPESE DI GIACENZA DI RICONSEGNA CITTA'
145400970306IF  1C     WERR          IFEQ      'N'
145500970306IF  2C     VC(19)        IFEQ      'S'                                          *VOCE DA CALCOLARE
145600970306     C     $TGC          ANDEQ     '1'                                          *TROVATA TAR.GIACENZ
145700970306     C                   EXSR      CALGRC
145800001227     C                   EXSR      CAL058
145900001227     C                   EXSR      CAL075
146000970306E   2C                   ENDIF
146100970306E   1C                   ENDIF
146200970306     C*
146300970306     C* CALCOLA LE SPESE DI GIACENZA DI SOSTA
146400970306IF  1C     WERR          IFEQ      'N'
146500970306IF  2C     VC(20)        IFEQ      'S'                                          *VOCE DA CALCOLARE
146600970306     C     $TGC          ANDEQ     '1'                                          *TROVATA TAR.GIACENZ
146700970306     C                   EXSR      CALGSO
146800001227     C                   EXSR      CAL059
146900001227     C                   EXSR      CAL076
147000970306E   2C                   ENDIF
147100970306E   1C                   ENDIF
147200970306     C*
147300970306     C                   ENDSR
147400970306     C*------------------------------------------------------------------------*
147500970306     C* CALTCP - CALCOLA T.C.P.
147600970306     C*------------------------------------------------------------------------*
147700970306     C     CALTCP        BEGSR
147800990819     C*
147900990819     C* REIMPOSTA LE VARIABILI DI LAVORO
148000990819     C                   MOVEL     'S'           WCAR                           *SI UTILIZZO TAR.CAR
148100011031     C                   Z-ADD     *ZEROS        TASTMP           21 8
148200990819     C*
148300990819     C* CALCOLA CONSEGNA PARTICOLARE
148400990819     C                   MOVEL     '7'           TASTC             1            *CODICE CONSEGNA
148500011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
148600990819     C                   EXSR      CALCP
148700990819     C                   Z-ADD     TASCP         TASTMP
148800970306     C*
148900970314IF  1C     TASTMP        IFGT      *ZEROS
149000970306     C                   Z-ADD     5             I
149100970306     C                   Z-ADD     TASTMP        VIMP(I)
149200970314E   1C                   ENDIF
149300990819     C*
149400970306     C                   ENDSR
149500001222     C*------------------------------------------------------------------------*
149600001222     C* CAL046 - CALCOLA ADG SU TCP
149700001222     C*------------------------------------------------------------------------*
149800001222     C     CAL046        BEGSR
149900001222     C*
150000001222     C* REIMPOSTA LE VARIABILI DI LAVORO
150100011031     C                   Z-ADD     *ZEROS        TASADG           21 8
150200001222     C*
150300001222     C* CALCOLA L'A.D.G.
150400011031     C                   Z-ADD     VIMP(5)       WIMP             23 8
150500001222     C                   EXSR      ADGCAL
150600001222     C*
150700001222IF  1C     TASADG        IFGT      *ZEROS
150800001222     C                   Z-ADD     46            I
150900001222     C                   Z-ADD     TASADG        VIMP(I)
151000001222E   1C                   ENDIF
151100001222     C*
151200001222     C                   ENDSR
151300001222     C*------------------------------------------------------------------------*
151400001222     C* CAL063 - CALCOLA ISTAT SU ADG/TCP
151500001222     C*------------------------------------------------------------------------*
151600001222     C     CAL063        BEGSR
151700001222     C*
151800001222     C* REIMPOSTA LE VARIABILI DI LAVORO
151900011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
152000001222     C*
152100001222     C* CALCOLA L'ISTAT
152200011031     C                   Z-ADD     VIMP(46)      WIMP             23 8
152300001222     C                   EXSR      IACCAL
152400001222     C*
152500001222IF  1C     TASIAC        IFGT      *ZEROS
152600001222     C                   Z-ADD     63            I
152700001222     C                   Z-ADD     TASIAC        VIMP(I)
152800001222E   1C                   ENDIF
152900001222     C*
153000001222     C                   ENDSR
153100040826     C*------------------------------------------------------------------------*
153200040826     C* CALdps - CALCOLA diritto di pesatura
153300040826     C*------------------------------------------------------------------------*
153400040826     C     CALdps        BEGSR
153500040826     C*
153600040826     C* REIMPOSTA LE VARIABILI DI LAVORO
153700040826     C                   MOVEL     'S'           WCAR                           *SI UTILIZZO TAR.CAR
153800040826     C                   Z-ADD     *ZEROS        TASdps           21 8
153900040826     C*
154000040826     C* CALCOLA CONSEGNA PARTICOLARE
154100040826     C                   MOVEL     '5'           TASTC             1            *CODICE CONSEGNA
154200040826     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
154300040826     C                   EXSR      CALCP
154400040826     C                   Z-ADD     TASCP         TASdps
154500040826     C*
154600040826IF  1C     TASdps        IFGT      *ZEROS
154700040826     C                   Z-ADD     102           I
154800040826     C                   Z-ADD     TASdps        VIMP(I)
154900040826E   1C                   ENDIF
155000040826     C*
155100040826     C                   ENDSR
155200040826     C*------------------------------------------------------------------------*
155300040826     C* CAL103 - CALCOLA ADG SU TCP
155400040826     C*------------------------------------------------------------------------*
155500040826     C     CAL103        BEGSR
155600040826     C*
155700040826     C* REIMPOSTA LE VARIABILI DI LAVORO
155800040826     C                   Z-ADD     *ZEROS        TASADG           21 8
155900040826     C*
156000040826     C* CALCOLA L'A.D.G.
156100040826     C                   Z-ADD     VIMP(102)     WIMP             23 8
156200040826     C                   EXSR      ADGCAL
156300040826     C*
156400040826IF  1C     TASADG        IFGT      *ZEROS
156500040826     C                   Z-ADD     103           I
156600040826     C                   Z-ADD     TASADG        VIMP(I)
156700040826E   1C                   ENDIF
156800040826     C*
156900040826     C                   ENDSR
157000040826     C*------------------------------------------------------------------------*
157100040826     C* CAL104 - CALCOLA ISTAT SU ADG/TCP
157200040826     C*------------------------------------------------------------------------*
157300040826     C     CAL104        BEGSR
157400040826     C*
157500040826     C* REIMPOSTA LE VARIABILI DI LAVORO
157600040826     C                   Z-ADD     *ZEROS        TASIAC           21 8
157700040826     C*
157800040826     C* CALCOLA L'ISTAT
157900040826     C                   Z-ADD     VIMP(103)     WIMP             23 8
158000040826     C                   EXSR      IACCAL
158100040826     C*
158200040826IF  1C     TASIAC        IFGT      *ZEROS
158300040826     C                   Z-ADD     104           I
158400040826     C                   Z-ADD     TASIAC        VIMP(I)
158500040826E   1C                   ENDIF
158600040826     C*
158700040826     C                   ENDSR
158800970306     C*------------------------------------------------------------------------*
158900970306     C* CALGRP - CALCOLA LE SPESE DI GIACENZA RICONSEGNA PROVINCIA
159000970306     C*------------------------------------------------------------------------*
159100970306     C     CALGRP        BEGSR
159200970306     C*
159300970306     C* REIMPOSTA LE VARIABILI DI LAVORO
159400011031     C                   Z-ADD     *ZEROS        TASSGP           21 8
159500970306     C*
159600970306     C* CALCOLA IL PESO TASSABILE -GIACENZA-
159700970306IF  1C     TGCSGP        IFGT      *ZEROS                                       *IMMESSA TARIFFA
159800970306     C                   Z-ADD     *ZEROS        §PESCA
159900970306     C                   EXSR      CALPTG
160000970306     C*
160100970306     C* CALCOLA LE SPESE DI GIACENZA
160200970306     C     TGCSGP        MULT      ETCNRG        TASSGR                         *SPESE X N°RIAPERTUR
160300970306     C     TASSGR        MULT      §PESCA        TASSGR                         *SPESE X PESO TASSAB
160400970306     C*
160500970306     C* AZZERA LE SPESE DI RICONSEGNA SE SI E' DENTRO AI GIORNI DI FRANCHIGIA
160600970306IF  2C     TGCFAF        IFEQ      '2'                                          *FRANCHIGIA
160700970306     C     ETCGGG        ANDLE     TGCSGF                                       *GG SOSTA < GG FRANC
160800970306     C                   Z-ADD     *ZEROS        TASSGP                         *NO SPESE RICONSEGNA
160900970306E   2C                   ENDIF
161000970306E   1C                   ENDIF
161100970306     C*
161200970314IF  1C     TASSGP        IFGT      *ZEROS
161300970306     C                   Z-ADD     18            I
161400970306     C                   Z-ADD     TASSGP        VIMP(I)
161500970314E   1C                   ENDIF
161600970306     C*
161700970306     C                   ENDSR
161800001227     C*------------------------------------------------------------------------*
161900001227     C* CAL057 - CALCOLA ADG SU GIACENZA RICONSEGNA PROVINCIA
162000001227     C*------------------------------------------------------------------------*
162100001227     C     CAL057        BEGSR
162200001227     C*
162300001227     C* REIMPOSTA LE VARIABILI DI LAVORO
162400011031     C                   Z-ADD     *ZEROS        TASADG           21 8
162500001227     C*
162600001227     C* CALCOLA L'A.D.G.
162700011031     C                   Z-ADD     VIMP(18)      WIMP             23 8
162800001227     C                   EXSR      ADGCAL
162900001227     C*
163000001227IF  1C     TASADG        IFGT      *ZEROS
163100001227     C                   Z-ADD     57            I
163200001227     C                   Z-ADD     TASADG        VIMP(I)
163300001227E   1C                   ENDIF
163400001227     C*
163500001227     C                   ENDSR
163600001227     C*------------------------------------------------------------------------*
163700001227     C* CAL074 - CALCOLA ISTAT SU ADG/GIACENZA RICONSEGNA PROVINCIA
163800001227     C*------------------------------------------------------------------------*
163900001227     C     CAL074        BEGSR
164000001227     C*
164100001227     C* REIMPOSTA LE VARIABILI DI LAVORO
164200011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
164300001227     C*
164400001227     C* CALCOLA L'ISTAT
164500011031     C                   Z-ADD     VIMP(57)      WIMP             23 8
164600001227     C                   EXSR      IACCAL
164700001227     C*
164800001227IF  1C     TASIAC        IFGT      *ZEROS
164900001227     C                   Z-ADD     74            I
165000001227     C                   Z-ADD     TASIAC        VIMP(I)
165100001227E   1C                   ENDIF
165200001227     C*
165300001227     C                   ENDSR
165400970306     C*------------------------------------------------------------------------*
165500970306     C* CALGRC - CALCOLA LE SPESE DI GIACENZA RICONSEGNA CITTA'
165600970306     C*------------------------------------------------------------------------*
165700970306     C     CALGRC        BEGSR
165800970306     C*
165900970306     C* REIMPOSTA LE VARIABILI DI LAVORO
166000011031     C                   Z-ADD     *ZEROS        TASSGR           21 8
166100970306     C*
166200970306     C* CALCOLA IL PESO TASSABILE -GIACENZA-
166300970306IF  1C     TGCSGR        IFGT      *ZEROS                                       *IMMESSA TARIFFA
166400970306     C                   Z-ADD     *ZEROS        §PESCA
166500970306     C                   EXSR      CALPTG
166600970306     C*
166700970306     C* CALCOLA LE SPESE DI GIACENZA
166800970306     C     TGCSGR        MULT      ETCNRG        TASSGR                         *SPESE X N°RIAPERTUR
166900970306     C     TASSGR        MULT      §PESCA        TASSGR                         *SPESE X PESO TASSAB
167000970306     C*
167100970306     C* AZZERA LE SPESE DI RICONSEGNA SE SI E' DENTRO AI GIORNI DI FRANCHIGIA
167200970306IF  2C     TGCFAF        IFEQ      '2'                                          *FRANCHIGIA
167300970306     C     ETCGGG        ANDLE     TGCSGF                                       *GG SOSTA < GG FRANC
167400970306     C                   Z-ADD     *ZEROS        TASSGR                         *NO SPESE RICONSEGNA
167500970306E   2C                   ENDIF
167600970306E   1C                   ENDIF
167700970306     C*
167800970314IF  1C     TASSGR        IFGT      *ZEROS
167900970306     C                   Z-ADD     19            I
168000970306     C                   Z-ADD     TASSGR        VIMP(I)
168100970314E   1C                   ENDIF
168200970306     C*
168300970306     C                   ENDSR
168400001227     C*------------------------------------------------------------------------*
168500001227     C* CAL058 - CALCOLA ADG SU GIACENZA RICONSEGNA CITTA'
168600001227     C*------------------------------------------------------------------------*
168700001227     C     CAL058        BEGSR
168800001227     C*
168900001227     C* REIMPOSTA LE VARIABILI DI LAVORO
169000011031     C                   Z-ADD     *ZEROS        TASADG           21 8
169100001227     C*
169200001227     C* CALCOLA L'A.D.G.
169300011031     C                   Z-ADD     VIMP(19)      WIMP             23 8
169400001227     C                   EXSR      ADGCAL
169500001227     C*
169600001227IF  1C     TASADG        IFGT      *ZEROS
169700001227     C                   Z-ADD     58            I
169800001227     C                   Z-ADD     TASADG        VIMP(I)
169900001227E   1C                   ENDIF
170000001227     C*
170100001227     C                   ENDSR
170200001227     C*------------------------------------------------------------------------*
170300001227     C* CAL075 - CALCOLA ISTAT SU ADG/GIACENZA RICONSEGNA CITTA'
170400001227     C*------------------------------------------------------------------------*
170500001227     C     CAL075        BEGSR
170600001227     C*
170700001227     C* REIMPOSTA LE VARIABILI DI LAVORO
170800011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
170900001227     C*
171000001227     C* CALCOLA L'ISTAT
171100011031     C                   Z-ADD     VIMP(58)      WIMP             23 8
171200001227     C                   EXSR      IACCAL
171300001227     C*
171400001227IF  1C     TASIAC        IFGT      *ZEROS
171500001227     C                   Z-ADD     75            I
171600001227     C                   Z-ADD     TASIAC        VIMP(I)
171700001227E   1C                   ENDIF
171800001227     C*
171900001227     C                   ENDSR
172000970306     C*------------------------------------------------------------------------*
172100970306     C* CALGSO - CALCOLA LE SPESE DI GIACENZA DI SOSTA
172200970306     C*------------------------------------------------------------------------*
172300970306     C     CALGSO        BEGSR
172400970306     C*
172500970306     C     *LIKE         DEFINE    ETCGGG        SAVGGG
172600970306     C     *LIKE         DEFINE    ETCGGG        GGGETC
172700970306     C*
172800970306     C* REIMPOSTA LE VARIABILI DI LAVORO
172900011031     C                   Z-ADD     *ZEROS        TASSGS           21 8
173000970306     C                   Z-ADD     *ZEROS        X                 1 0
173100970306     C                   Z-ADD     *ZEROS        XP                1 0
173200970306     C                   Z-ADD     *ZEROS        SGG                            *GG SOSTA
173300970306     C                   Z-ADD     *ZEROS        STU                            *TARIFFA UNITARIA
173400970306     C                   Z-ADD     *ZEROS        STT                            *TARIFFA TOTALE
173500970306     C*
173600970306     C                   Z-ADD     ETCGGG        SAVGGG
173700970306     C                   Z-ADD     ETCGGG        GGGETC
173800970306DO  1C     1             DO        3             X
173900970306IF  2C     SAVGGG        IFGT      TGS(X)
174000970306     C     SAVGGG        SUB       TGS(X)        GGGETC
174100970306IF  3C     X             IFEQ      1
174200970306     C                   Z-ADD     TGS(X)        SGG(X)
174300970306X   3C                   ELSE
174400970306     C     TGS(X)        SUB       TGS(XP)       SGG(X)
174500970306E   3C                   ENDIF
174600970306     C                   Z-ADD     TTS(X)        STU(X)
174700970306X   2C                   ELSE
174800970306     C                   Z-ADD     GGGETC        SGG(X)
174900970306     C                   Z-ADD     TTS(X)        STU(X)
175000970306     C                   Z-ADD     *ZEROS        SAVGGG
175100970306     C                   Z-ADD     *ZEROS        GGGETC
175200970306E   2C                   ENDIF
175300970306     C                   ADD       1             XP
175400970306E   1C                   ENDDO
175500970306     C*
175600970306     C* CALCOLA IL PESO TASSABILE -GIACENZA-
175700970306     C                   Z-ADD     *ZEROS        §PESCA
175800970306     C                   EXSR      CALPTG
175900970306     C*
176000970306     C* CALCOLA LE SPESE DI GIACENZA
176100970306     C     SGG           MULT      STU           STT
176200970306     C     STT           MULT      §PESCA        STT
176300970306     C                   XFOOT     STT           TASSGS
176400970306     C*
176500970306     C* APPLICA IL MINIMO TARIFFARIO SE SUPERATI I GIORNI DI FRANCHIGIA
176600970306IF  1C     TASSGS        IFLT      TGCSTM
176700970306     C     ETCGGG        ANDGT     TGCSGF
176800970306     C                   Z-ADD     TGCSTM        TASSGS
176900970306E   1C                   ENDIF
177000970306     C*
177100970306     C* AZZERA LE SPESE DI SOSTA SE SI E' DENTRO AI GIORNI DI FRANCHIGIA
177200970306IF  1C     TGCFAF        IFNE      *BLANKS                                      *FRANCHIGIA
177300970306     C     ETCGGG        ANDLE     TGCSGF                                       *GG SOSTA < GG FRANC
177400970306     C                   Z-ADD     *ZEROS        TASSGS                         *NO SPESE SOSTA
177500970306E   1C                   ENDIF
177600970306     C*
177700970314IF  1C     TASSGS        IFGT      *ZEROS
177800970306     C                   Z-ADD     20            I
177900970306     C                   Z-ADD     TASSGS        VIMP(I)
178000970314E   1C                   ENDIF
178100970306     C*
178200970306     C                   ENDSR
178300001227     C*------------------------------------------------------------------------*
178400001227     C* CAL059 - CALCOLA ADG SU GIACENZA SOSTA
178500001227     C*------------------------------------------------------------------------*
178600001227     C     CAL059        BEGSR
178700001227     C*
178800001227     C* REIMPOSTA LE VARIABILI DI LAVORO
178900011031     C                   Z-ADD     *ZEROS        TASADG           21 8
179000001227     C*
179100001227     C* CALCOLA L'A.D.G.
179200011031     C                   Z-ADD     VIMP(20)      WIMP             23 8
179300001227     C                   EXSR      ADGCAL
179400001227     C*
179500001227IF  1C     TASADG        IFGT      *ZEROS
179600001227     C                   Z-ADD     59            I
179700001227     C                   Z-ADD     TASADG        VIMP(I)
179800001227E   1C                   ENDIF
179900001227     C*
180000001227     C                   ENDSR
180100001227     C*------------------------------------------------------------------------*
180200001227     C* CAL076 - CALCOLA ISTAT SU ADG/GIACENZA SOSTA
180300001227     C*------------------------------------------------------------------------*
180400001227     C     CAL076        BEGSR
180500001227     C*
180600001227     C* REIMPOSTA LE VARIABILI DI LAVORO
180700011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
180800001227     C*
180900001227     C* CALCOLA L'ISTAT
181000011031     C                   Z-ADD     VIMP(59)      WIMP             23 8
181100001227     C                   EXSR      IACCAL
181200001227     C*
181300001227IF  1C     TASIAC        IFGT      *ZEROS
181400001227     C                   Z-ADD     76            I
181500001227     C                   Z-ADD     TASIAC        VIMP(I)
181600001227E   1C                   ENDIF
181700001227     C*
181800001227     C                   ENDSR
181900970306     C*------------------------------------------------------------------------*
182000970306     C* CALPTG - CALCOLA IL PESO TASSABILE -GIACENZA-
182100970306     C*------------------------------------------------------------------------*
182200970306     C     CALPTG        BEGSR
182300970306     C*
182400970306     C* REIMPOSTA LE VARIABILI DI LAVORO
182500970306     C                   Z-ADD     *ZEROS        §PESVO           13 3
182600970306     C                   Z-ADD     *ZEROS        §PES§            13 3
182700970306     C                   Z-ADD     *ZEROS        §PESCA            6 0
182800970306     C*
182900970725     C     TGCRPV        MULT      SVSC(S)       §PESVO
183000970306     C     §PESVO        MULT      100           §PESVO
183100970306IF  1C     §PESVO        IFGT      SPSC(S)
183200970306     C     §PESVO        DIV       100           §PES§
183300970306X   1C                   ELSE
183400970306     C     SPSC(S)       DIV       100           §PES§
183500970306E   1C                   ENDIF
183600970306     C     §PES§         ADD       0,99          §PESCA
183700970306     C*
183800970306     C                   ENDSR
183900970306     C*------------------------------------------------------------------------*
184000970306     C* CARTAD - CARICAMENTO TARIFFA CLIENTE -DETTAGLIO-
184100970306     C*------------------------------------------------------------------------*
184200970306     C     CARTAD        BEGSR
184300970306     C*
184400970306     C* REIMPOSTA LE VARIABILI DI LAVORO
184500980707DO  1C     1             DO        11            I
184600970306     C     I             OCCUR     TARDS
184700970306     C                   CLEAR                   TARDS
184800970306E   1C                   ENDDO
184900970306     C                   MOVEL     '0'           $TAD                           *TARIFFA TROVATA
185000970306     C*
185100970306     C* CERCA LA TARIFFA CLIENTE CON IL CODICE TASSAZIONE ELEMENTARE
185200970306     C                   Z-ADD     D00KSC        KTDKSC                         *CLIENTE
185300970306     C                   Z-ADD     D00PRG        KTDPRG                         *PROGRESSIVO
185400970306     C                   Z-ADD     D00LNP        KTDLNP                         *LINEA DI PARTENZA
185500990819     C                   MOVEL     CCTS(T)       KTDCTS                         *COD.TASS.ELEMENTARE
185600990824     C                   MOVEL     *BLANKS       KTDNAZ                         *NAZIONE
185700970306     C                   MOVEL     *BLANKS       KTDCAP                         *C.A.P.
185800970306     C                   Z-ADD     D00CTR        KTDCTR                         *CODICE TARIFFA
185900990819     C     KEYTAD        SETLL     TITAD00L                               98
186000970306IF  1C     *IN98         IFEQ      *ON                                          *TARIFFA TROVATA
186100970306     C                   MOVEL     '1'           $TAD
186200970306X   1C                   ELSE                                                   *TARIFFA NON TROVATA
186300970306     C*
186400970306     C* CERCA LA TARIFFA CLIENTE CON IL CODICE TASSAZIONE REGIONE
186500990803     C                   MOVEL     CRAP(T)       KTDCTS                         *COD.TASS.REGIONE
186600990819     C     KEYTAD        SETLL     TITAD00L                               98
186700990819IF  2C     *IN98         IFEQ      *ON                                          *TARIFFA TROVATA
186800970306     C                   MOVEL     '1'           $TAD
186900990819X   2C                   ELSE                                                   *TARIFFA NON TROVATA
187000970306     C*
187100970306     C* CERCA LA TARIFFA CLIENTE CON IL CODICE TASSAZIONE NAZIONE
187200990803     C                   MOVEL     CNAR(T)       KTDCTS                         *COD.TASS.NAZIONE
187300990819     C     KEYTAD        SETLL     TITAD00L                               98
187400990819IF  3C     *IN98         IFEQ      *ON                                          *TARIFFA TROVATA
187500970306     C                   MOVEL     '1'           $TAD
187600990819E   3C                   ENDIF
187700990819E   2C                   ENDIF
187800990819E   1C                   ENDIF
187900970306     C*
188000970306     C* SE TARIFFA NON TROVATA, ERRORE
188100970306IF  1C     $TAD          IFEQ      '0'
188200970306     C                   MOVEL     MSG(24)       $MSG
188300970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
188400970306     C*
188500970306     C* SE TARIFFA TROVATA, CARICA DETTAGLIO
188600970306X   1C                   ELSE
188700980707     C                   EXSR      CARDST
188800970306E   1C                   ENDIF
188900970306     C*
189000970306     C                   ENDSR
189100980707     C*------------------------------------------------------------------------*
189200980707     C* CARDST - CARICA CAMPI TARIFFA -DETTAGLIO-
189300980707     C*------------------------------------------------------------------------*
189400980707     C     CARDST        BEGSR
189500980707     C*
189600980707     C     *LIKE         DEFINE    TADRPV        RAPPV
189700980707     C                   DO        11            A                 5 0
189800980707     C     A             OCCUR     TARDS
189900980707     C                   CLEAR                   TARDS
190000980707     C                   ENDDO
190100980707     C                   Z-ADD     0             RAPPV
190200980707     C                   CLEAR                   WELAMT
190300980908     C                   Z-ADD     DMITAS        MINTAS                         *MIN.TASS. TNTAM
190400980707    1C                   DO        *HIVAL        A
190500990819     C**
190600980707     C** NON DEVO RILEGGERE PERCHE' DEVO CARICARE LO STESSO SCAGLIONE
190700980707    2C     WELAMT        IFNE      'S'
190800990819     C     KEYTAD        READE     TITAD000                               15
190900980707     C   15              GOTO      ENDDST
191000991125     C*
191100991125     C* CONTROLLA SE CONVERTIRE I CAMPI DI TNTAM
191200011026    3C     D00DIV        IFNE      WDIVCE
191300011019     C                   MOVEL     D00DIV        WDVI
191400991125     C                   EXSR      CNVTAD
191500991125    3C                   ENDIF
191600991125    2C                   ENDIF
191700980707     C** ELABORATO UNA VOLTA NON LO FACCIO PIU'
191800980707    2C     WELAMT        IFEQ      'S'
191900980707     C                   MOVEL     'E'           WELAMT            1
192000980707    2C                   ENDIF
192100980707     C**
192200980707     C     A             OCCUR     TARDS
192300980707     C* SE LO SCAGLIONE LETTO E' > DEL MINIMO TASSABILE CARICO
192400980707     C*  ANCHE LO SCAGLIONE DEL MINIMO CON TARIFFA=TARIFFA X MIN.TAS
192500980707     C**
192600980707     C                   Z-ADD     TADLNP        TLNP
192700980707     C                   MOVEL     TADCTS        TCTS
192800990819     C                   MOVEL     TADNAZ        TNAZ
192900990819     C                   MOVEL     TADCAP        TCAP
193000980707     C                   Z-ADD     TADCTR        TCTR
193100980707     C                   Z-ADD     TADRPV        TRPV
193200980707     C                   Z-ADD     TADMIN        TMIN
193300980707     C                   Z-ADD     TADMAX        TMAX
193400980707     C                   Z-ADD     TADRPV        RAPPV
193500991125     C*
193600980707     C** SE HO GIA' ELABORATO IL MINIMO NON LO FACCIO PIU'
193700980707    2C     WELAMT        IFNE      'E'
193800980707     C**
193900980707    3C     TADSGL        IFEQ      MINTAS
194000980707     C                   MOVEL     'E'           WELAMT
194100980707   X3C                   ELSE
194200980707     C** SE LO SCAGLIONE CHE LEGGO E' MAGGIORE DEL MINIMO TASSABILE
194300980707     C*  UTILIZZO PRIMA LA TARIFFA DELLO SCAGLIONE PER IL MINIMO T.
194400980707     C*  (MOLTIPLICANDOLA PER IL MINIMO TASSABILE)
194500980707     C*  POI LO UTILIZZO PER LO SCAGLIONE LETTO
194600980707    4C     TADSGL        IFGT      MINTAS
194700980707     C                   MOVEL     'S'           WELAMT
194800980707     C                   Z-ADD     MINTAS        TSCG
194900980707     C**
195000980707     C* SE NON C'E' APPLICAZIONE TARIFFA FINITA -- MOLTIPLICO
195100980707    5C     TAMATA        IFEQ      0
195200980707     C     TAMATA        ORLE      MINTAS
195300980707     C     TADSGL        ORGT      TAMATA
195400980707     C**
195500991125    6C     UNOCTR        IFEQ      4
195600980707     C     UNOCTR        OREQ      0
195700011031     C     MINTAS        DIV(H)    100           WMTAS            21 8
195800980707     C                   ELSE
195900980707     C                   Z-ADD     MINTAS        WMTAS
196000980707    6C                   ENDIF
196100980707     C* TARIFFA
196200010926     C     WMTAS         MULT(H)   TADITR        TITR
196300980707     C     WMTAS         MULT(H)   TADINO        TINO
196400980707     C                   ELSE
196500980707     C* C'E' APPLICAZIONE MAGGIORE --> UTILIZZO LA TARIFFA COSI' COM'E'
196600980707     C                   Z-ADD     TADITR        TITR
196700980707     C                   Z-ADD     TADINO        TINO
196800980707    5C                   ENDIF
196900980707    4C                   ENDIF
197000980707    3C                   ENDIF
197100980707    2C                   ENDIF
197200980707     C*
197300980707     C* SE NON HO CARICATO LO SCAGLIONE PER IL MINIMO TASSABILE
197400980707    2C     WELAMT        IFNE      'S'
197500980707     C                   Z-ADD     TADSGL        TSCG
197600980707     C                   Z-ADD     TADITR        TITR
197700980707     C                   Z-ADD     TADINO        TINO
197800980707    2C                   ENDIF
197900980707    1C                   ENDDO
198000991125     C*
198100980707     C     ENDDST        ENDSR
198200980707     C*------------------------------------------------------------------------*
198300980707     C* CALQTA - REPERISCE E CALCOLA LA QUANTITA' DA UTILIZZARE
198400980707     C*------------------------------------------------------------------------*
198500980707     C     CALQTA        BEGSR
198600980707     C*
198700980707     C* PESO TASSABILE E QUANTITA' DA FATTURARE
198800980707     C                   MOVEL     UNOCTR        WTPG              1
198900980707     C                   CLEAR                   WRPV
199000980707     C                   MOVEL     'TAD'         WDET              3
199100980707     C                   Z-ADD     TAMARL        WARL
199200980707     C                   Z-ADD     TAMARF        WARF
199300980707     C                   Z-ADD     TAMARO        WARO
199400980707     C                   EXSR      CALCOL
199500980707     C                   Z-ADD     ISOPES        PESTAS           13 3
199600980707     C*
199700980707     C                   ENDSR
199800980707     C*------------------------------------------------------------------------*
199900980707     C* CALCOL - CALCOLA RPV, ARROTONDAMENTI E PESO TASSABILE
200000980707     C*------------------------------------------------------------------------*
200100980707     C     CALCOL        BEGSR
200200980707     C*
200300980707     C     *LIKE         DEFINE    TAMARL        ARR
200400980707     C     *LIKE         DEFINE    TPTRPV        WRPV
200500980707     C     *LIKE         DEFINE    TPTARL        WARL
200600980707     C     *LIKE         DEFINE    TPTARF        WARF
200700980707     C     *LIKE         DEFINE    TPTARO        WARO
200800980707     C                   Z-ADD     0             ISOPES           13 3
200900980707SL  1C                   SELECT
201000980707     C* TARIFFA A QLI
201100980707     C     WTPG          WHENEQ    '0'
201200980707     C                   Z-ADD     SPSC(S)       ISOPES
201300980707     C* TARIFFA A COLLI
201400980707     C     WTPG          WHENEQ    '1'
201500980707     C                   Z-ADD     SCSC(S)       ISOPES
201600980707     C* TARIFFA A SPEDIZIONE
201700980707     C     WTPG          WHENEQ    '2'
201800980707     C                   Z-ADD     1             ISOPES
201900980707     C* TARIFFA A KG
202000980707     C     WTPG          WHENEQ    '3'
202100980707     C                   Z-ADD     SPSC(S)       ISOPES
202200980707E   1C                   ENDSL
202300980707     C** MINIMO TASSABILE STD
202400980707     C                   Z-ADD     0             MINTAS
202500980707     C**
202600980707     C                   Z-ADD     1             A
202700980707     C     WTPG          LOOKUP    CTR(A)                                 05
202800980707     C   05A             OCCUR     DSTR
202900980707     C   05              Z-ADD     §TRATA        MINTAS
203000980707     C**
203100980707     C** SOLO PER TAD (PER TPD NON MI SERVE)
203200980707    1C     WDET          IFEQ      'TAD'
203300980707     C                   CLEAR                   MINTAR
203400980707     C** UTILIZZO IL CAMPO CON I DECIMALI PER IL MINIMO TASSABILE
203500980707     C*  DA TABELLA TR. SE POI C'E' ANCHE IN TARIFFA USO QUELLO
203600980707     C   05              Z-ADD     §TRATA        MINTAR
203700980707    2C     TAMATA        IFGT      0
203800980707     C                   Z-ADD     TAMATA        MINTAR
203900980707    2C                   ENDIF
204000980707    1C                   ENDIF
204100980707     C**
204200980707     C** RPV SOLO PER TARIFFA 0 E 3
204300980707    1C     §TRRPV        IFEQ      'S'
204400980707     C** PER TAD PRENDO RPV DA RIGA DI DETTAGLIO CON PESO REALE
204500980707    2C     WDET          IFEQ      'TAD'
204600980707     C     SVSC(S)       ANDGT     0
204700980707    3C                   DO        11            A
204800980707     C     A             OCCUR     TARDS
204900980707    4C     TSCG          IFGE      ISOPES
205000980707     C                   GOTO      POITAS
205100980707    4C                   END
205200980707    3C                   END
205300980707     C** RICERCO RPV CON PESO REALE
205400980707    3C     TSCG          IFLT      ISOPES
205500980707     C                   MOVEL     MSG(25)       $MSG
205600980707     C                   EXSR      IMPERR
205700980707    3C                   END
205800980707     C     POITAS        TAG
205900980707     C                   Z-ADD     TRPV          WRPV
206000980707    2C                   ENDIF
206100980707     C**
206200980707    2C     SVSC(S)       IFNE      0
206300980707     C     WRPV          ANDNE     0
206400980707     C                   Z-ADD     0             COMPRV            9 3
206500980707     C     SVSC(S)       MULT      WRPV          COMPRV
206600980707     C     COMPRV        MULT      100           ISOPE2           13 3
206700980707    3C     ISOPE2        IFGT      ISOPES
206800980707     C                   Z-ADD     ISOPE2        ISOPES
206900980707    3C                   ENDIF
207000980707    2C                   ENDIF
207100980707    1C                   ENDIF
207200980707     C**
207300980707     C** ARROTONDAMENTI: LO FACCIO SEOCONDO QUANTO INDICATO IN DSTR
207400980707    0C     §TRARR        IFEQ      'S'
207500980707     C**
207600980707     C** ARROTONDAMENTO: TARIFFE 0 Q.LI - 1 - 3
207700980707     C     ISOPES        IFGE      MINTAS
207800980707     C                   Z-ADD     0             ARR
207900980707     C     ISOPES        IFGT      WARL
208000980707     C                   Z-ADD     WARO          ARR
208100980707     C                   ELSE
208200980707     C                   Z-ADD     WARF          ARR
208300980707     C                   END
208400980707     C**
208500011031     C                   Z-ADD     0             RESTO            21 8
208600011108     C                   Z-ADD     0             ISOPEX           21 0
208700980707     C     ARR           IFNE      0
208800980707     C     ISOPES        ANDNE     0
208900980707     C     ISOPES        DIV       ARR           ISOPEX
209000980707     C                   MVR                     RESTO
209100980707     C     RESTO         IFGT      0
209200980707     C     ISOPEX        MULT      ARR           ISOPES
209300980707     C                   ADD       ARR           ISOPES
209400980707     C                   ENDIF
209500980707     C                   ENDIF
209600980707     C                   ENDIF
209700980707     C**
209800980707    0C                   ENDIF
209900980707     C                   ENDSR
210000980707     C*------------------------------------------------------------------------*
210100980707     C* VALVO3 - VALORIZZA LE VOCI -DIPENDENTI DA CODICE TASSAZIONE-
210200980707     C*------------------------------------------------------------------------*
210300980707     C     VALVO3        BEGSR
210400970306     C*
210500970306     C* AZZERA GLI IMPORTI DELLE VOCI CON SEQUENZA TRE
210600970306DO  1C     1             DO        NVOCA         I
210700970306IF  2C     VS(I)         IFEQ      3
210800970306     C                   Z-ADD     *ZEROS        VIMP(I)                        *IMPORTO CALCOLATO
210900970306E   2C                   ENDIF
211000970306E   1C                   ENDDO
211100980702     C*---
211200980702     C* PORTO E INOLTRO
211300980702     C*---
211400980707IF  1C     WERR          IFEQ      'N'
211500980707IF  2C     VC(1)         IFEQ      'S'                                          *VOCE DA CALCOLARE
211600980707     C     VC(6)         OREQ      'S'                                          *VOCE DA CALCOLARE
211700980707     C                   EXSR      CALPEI
211800980707E   2C                   ENDIF
211900980707E   1C                   ENDIF
212000980702     C*---
212100980702     C* ALTRE PORTO
212200980702     C*---
212300980702     C* CALCOLA A.D.G. SUL PORTO
212400980702IF  1C     WERR          IFEQ      'N'
212500980702IF  2C     VC(3)         IFEQ      'S'                                          *VOCE DA CALCOLARE
212600980702     C                   EXSR      CALAGP
212700980702E   2C                   ENDIF
212800980702E   1C                   ENDIF
212900980702     C*
213000980702     C* CALCOLA ISTAT SUL PORTO
213100980702IF  1C     WERR          IFEQ      'N'
213200980702IF  2C     VC(4)         IFEQ      'S'                                          *VOCE DA CALCOLARE
213300980702     C                   EXSR      CALISP
213400980702E   2C                   ENDIF
213500980702E   1C                   ENDIF
213600980702     C*
213700980702     C* CALCOLA ISTAT SULL'A.D.G. DEL PORTO
213800980702IF  1C     WERR          IFEQ      'N'
213900980702IF  2C     VC(27)        IFEQ      'S'                                          *VOCE DA CALCOLARE
214000980702     C                   EXSR      CALIAP
214100980702E   2C                   ENDIF
214200980702E   1C                   ENDIF
214300980702     C*
214400980702     C*---
214500980702     C* ALTRE INOLTRO
214600980702     C*---
214700980702     C* CALCOLA A.D.G. SULL'INOLTRO
214800980702IF  1C     WERR          IFEQ      'N'
214900980702IF  2C     VC(21)        IFEQ      'S'                                          *VOCE DA CALCOLARE
215000980702     C                   EXSR      CALAGI
215100980702E   2C                   ENDIF
215200980707E   1C                   ENDIF
215300980702     C*
215400980702     C* CALCOLA ISTAT SULL'INOLTRO
215500980702IF  1C     WERR          IFEQ      'N'
215600980702IF  2C     VC(24)        IFEQ      'S'                                          *VOCE DA CALCOLARE
215700980702     C                   EXSR      CALISI
215800980702E   2C                   ENDIF
215900980702E   1C                   ENDIF
216000980702     C*
216100980702     C* CALCOLA ISTAT SULL'A.D.G. DELL'INOLTRO
216200980702IF  1C     WERR          IFEQ      'N'
216300980702IF  2C     VC(29)        IFEQ      'S'                                          *VOCE DA CALCOLARE
216400980702     C                   EXSR      CALIAI
216500980702E   2C                   ENDIF
216600980702E   1C                   ENDIF
216700980702     C*---
216800980702     C* LOCALITA' DISAGIATE
216900980702     C*---
217000980702     C* CALCOLA CONSEGNA PARTICOLARE LOCALITA' DISAGIATE
217100980702IF  1C     WERR          IFEQ      'N'
217200980702IF  2C     VC(35)        IFEQ      'S'                                          *VOCE DA CALCOLARE
217300980702     C                   EXSR      CALCPL
217400980702E   2C                   ENDIF
217500980702E   1C                   ENDIF
217600980702     C*
217700980702     C* CALCOLA A.D.G. SULLE CONSEGNE LOCALITA' DISAGIATE
217800980702IF  1C     WERR          IFEQ      'N'
217900980702IF  2C     VC(43)        IFEQ      'S'                                          *VOCE DA CALCOLARE
218000980702     C                   EXSR      CALAGL
218100980702E   2C                   ENDIF
218200980702E   1C                   ENDIF
218300980702     C*
218400980702     C* CALCOLA ISTAT SULLE CONSEGNE LOCALITA' DISAGIATE
218500980702IF  1C     WERR          IFEQ      'N'
218600980702IF  2C     VC(44)        IFEQ      'S'                                          *VOCE DA CALCOLARE
218700980702     C                   EXSR      CALISL
218800980702E   2C                   ENDIF
218900980702E   1C                   ENDIF
219000980702     C*
219100980702     C* CALCOLA ISTAT SULL'A.D.G. DELLE CONSEGNE LOCALITA' DISAGIATE
219200980702IF  1C     WERR          IFEQ      'N'
219300980702IF  2C     VC(45)        IFEQ      'S'                                          *VOCE DA CALCOLARE
219400980702     C                   EXSR      CALIAL
219500980702E   2C                   ENDIF
219600980702E   1C                   ENDIF
219700011031     C*---
219800011031     C* RITIRO ISOLA
219900011031     C*---
220000011031     C* CALCOLA RITIRO ISOLA
220100011031IF  1C     WERR          IFEQ      'N'
220200011031IF  2C     VC(88)        IFEQ      'S'                                          *VOCE DA CALCOLARE
220300011031     C                   EXSR      CALSAM
220400011031E   2C                   ENDIF
220500011031E   1C                   ENDIF
220600011031     C*
220700011031     C* CALCOLA A.D.G. SUL RITIRO ISOLA
220800011031IF  1C     WERR          IFEQ      'N'
220900011031IF  2C     VC(93)        IFEQ      'S'                                          *VOCE DA CALCOLARE
221000011031     C                   EXSR      CALAGR
221100011031E   2C                   ENDIF
221200011031E   1C                   ENDIF
221300011031     C*
221400011031     C* CALCOLA ISTAT SUL RITIRO ISOLA
221500011031IF  1C     WERR          IFEQ      'N'
221600011031IF  2C     VC(94)        IFEQ      'S'                                          *VOCE DA CALCOLARE
221700011031     C                   EXSR      CALISR
221800011031E   2C                   ENDIF
221900011031E   1C                   ENDIF
222000011031     C*
222100011031     C* CALCOLA ISTAT SULL'A.D.G. DEL RITIRO ISOLA
222200011031IF  1C     WERR          IFEQ      'N'
222300011031IF  2C     VC(95)        IFEQ      'S'                                          *VOCE DA CALCOLARE
222400011031     C                   EXSR      CALIAR
222500011031E   2C                   ENDIF
222600011031E   1C                   ENDIF
222700011030     C*---
222800011030     C* CONSEGNA ISOLA
222900011030     C*---
223000091023     C* CALCOLA CONSEGNA PARTICOLARE ISOLA
223100011030IF  1C     WERR          IFEQ      'N'
223200011030IF  2C     VC(80)        IFEQ      'S'                                          *VOCE DA CALCOLARE
223300011030     C                   EXSR      CALCIS
223400011030E   2C                   ENDIF
223500011030E   1C                   ENDIF
223600011030     C*
223700011030     C* CALCOLA A.D.G. SULLE CONSEGNE ISOLA
223800011030IF  1C     WERR          IFEQ      'N'
223900011030IF  2C     VC(85)        IFEQ      'S'                                          *VOCE DA CALCOLARE
224000011030     C                   EXSR      CALAGS
224100011030E   2C                   ENDIF
224200011030E   1C                   ENDIF
224300011030     C*
224400011030     C* CALCOLA ISTAT SULLE CONSEGNE ISOLA
224500011030IF  1C     WERR          IFEQ      'N'
224600011030IF  2C     VC(86)        IFEQ      'S'                                          *VOCE DA CALCOLARE
224700011030     C                   EXSR      CALISS
224800011030E   2C                   ENDIF
224900011030E   1C                   ENDIF
225000011030     C*
225100011030     C* CALCOLA ISTAT SULL'A.D.G. DELLE CONSEGNE ISOLA
225200011030IF  1C     WERR          IFEQ      'N'
225300011030IF  2C     VC(87)        IFEQ      'S'                                          *VOCE DA CALCOLARE
225400011030     C                   EXSR      CALIAS
225500011030E   2C                   ENDIF
225600011030E   1C                   ENDIF
225700980702     C*---
225800011030     C* INOLTRO + LOCALITA' DISAGIATE + CONSEGNA ISOLA
225900980702     C*---
226000980702     C* RICALCOLA INOLTRO E VOCI CORRELATE RISPETTO AL FLAG APPICAZIONE TARIFFA INOLTRO
226100980702IF  1C     WERR          IFEQ      'N'
226200011030IF  2C     VC(35)        IFEQ      'S'                                          *L.DISAGIATA
226300011030     C     VC(80)        OREQ      'S'                                          *C.ISOLA
226400980702     C                   EXSR      CA3INL
226500980702E   2C                   ENDIF
226600980702E   1C                   ENDIF
226700091023     c* sommo porto + inoltro + disagiate + cons.isola + ritiro isola x
226800091023     C* calcolare espresso una volta sola e poi riproporziono
226900091023     c* l'espresso trovato in base ai valori delle voci originali
227000091023     C* CALCOLA ESPRESSO
227100091023IF  1C     WERR          IFEQ      'N'
227200091023IF  2C     VC(7)         IFEQ      'S'                                          *VOCE DA CALCOLARE
227300091023     C                   EXSR      CALESP
227400091023     c                   exsr      calimpesp
227500091023E   2C                   ENDIF
227600091023E   1C                   ENDIF
227700091023     C*
227800091023     C* CALCOLA A.D.G. SULL'ESPRESSO PORTO
227900091023IF  1C     WERR          IFEQ      'N'
228000091023IF  2C     VC(23)        IFEQ      'S'                                          *VOCE DA CALCOLARE
228100091023     C                   EXSR      CALAEP
228200091023E   2C                   ENDIF
228300091023E   1C                   ENDIF
228400091023     C*
228500091023     C* CALCOLA ISTAT SULL'ESPRESSO PORTO
228600091023IF  1C     WERR          IFEQ      'N'
228700091023IF  2C     VC(26)        IFEQ      'S'                                          *VOCE DA CALCOLARE
228800091023     C                   EXSR      CALIEP
228900091023E   2C                   ENDIF
229000091023E   1C                   ENDIF
229100091023     C*
229200091023     C* CALCOLA ISTAT SULL'A.D.G. DELL'ESPRESSO PORTO
229300091023IF  1C     WERR          IFEQ      'N'
229400091023IF  2C     VC(30)        IFEQ      'S'                                          *VOCE DA CALCOLARE
229500091023     C                   EXSR      CAIAEP
229600091023E   2C                   ENDIF
229700091023E   1C                   ENDIF
229800091023     C*
229900091023     C* CALCOLA ESPRESSO SULL'INOLTRO
230000091023     c* non lo deve + fare l'espresso va calcolato una volta sola sulla
230100091023     c* somma del porto, isola, inoltr, diasgiate, ritiro isola
230200091023IF  1C*    WERR          IFEQ      'N'
230300091023IF  2C*    VC(36)        IFEQ      'S'                                          *VOCE DA CALCOLARE
230400091023     C*                  EXSR      CALESI
230500091023E   2C*                  ENDIF
230600091023E   1C*                  ENDIF
230700091023     C*
230800091023     C* CALCOLA A.D.G. SULL'ESPRESSO INOLTRO
230900091023IF  1C     WERR          IFEQ      'N'
231000091023IF  2C     VC(37)        IFEQ      'S'                                          *VOCE DA CALCOLARE
231100091023     C                   EXSR      CALAEI
231200091023E   2C                   ENDIF
231300091023E   1C                   ENDIF
231400091023     C*
231500091023     C* CALCOLA ISTAT SULL'ESPRESSO INOLTRO
231600091023IF  1C     WERR          IFEQ      'N'
231700091023IF  2C     VC(38)        IFEQ      'S'                                          *VOCE DA CALCOLARE
231800091023     C                   EXSR      CALIEI
231900091023E   2C                   ENDIF
232000091023E   1C                   ENDIF
232100091023     C*
232200091023     C* CALCOLA ISTAT SULL'A.D.G. DELL'ESPRESSO INOLTRO
232300091023IF  1C     WERR          IFEQ      'N'
232400091023IF  2C     VC(34)        IFEQ      'S'                                          *VOCE DA CALCOLARE
232500091023     C                   EXSR      CAIAEI
232600091023E   2C                   ENDIF
232700091023E   1C                   ENDIF
232800091023     C*
232900091023     C* CALCOLA ESPRESSO SULLA CONSEGNA LOCALITA' DISAGIATE
233000091023     c* non lo deve + fare l'espresso va calcolato una volta sola sulla
233100091023     c* somma del porto, isola, inoltr, diasgiate, ritiro isola
233200091023IF  1C*    WERR          IFEQ      'N'
233300091023IF  2C*    VC(39)        IFEQ      'S'                                          *VOCE DA CALCOLARE
233400091023     C*                  EXSR      CALESL
233500091023E   2C*                  ENDIF
233600091023E   1C*                  ENDIF
233700091023     C*
233800091023     C* CALCOLA A.D.G. SULL'ESPRESSO CONSEGNA LOCALITA' DISAGIATE
233900091023IF  1C     WERR          IFEQ      'N'
234000091023IF  2C     VC(40)        IFEQ      'S'                                          *VOCE DA CALCOLARE
234100091023     C                   EXSR      CALAEL
234200091023E   2C                   ENDIF
234300091023E   1C                   ENDIF
234400091023     C*
234500091023     C* CALCOLA ISTAT SULL'ESPRESSO CONSEGNA LOCALITA' DISAGIATE
234600091023IF  1C     WERR          IFEQ      'N'
234700091023IF  2C     VC(41)        IFEQ      'S'                                          *VOCE DA CALCOLARE
234800091023     C                   EXSR      CALIEL
234900091023E   2C                   ENDIF
235000091023E   1C                   ENDIF
235100091023     C*
235200091023     C* CALCOLA ISTAT SULL'A.D.G. DELL'ESPRESSO CONSEGNA LOCALITA' DISAGIATE
235300091023IF  1C     WERR          IFEQ      'N'
235400091023IF  2C     VC(42)        IFEQ      'S'                                          *VOCE DA CALCOLARE
235500091023     C                   EXSR      CAIAEL
235600091023E   2C                   ENDIF
235700091023E   1C                   ENDIF
235800091023     C*
235900091023     C* CALCOLA ESPRESSO SUL RITIRO ISOLA
236000091023     c* non lo deve + fare l'espresso va calcolato una volta sola sulla
236100091023     c* somma del porto, isola, inoltr, diasgiate, ritiro isola
236200091023IF  1C*    WERR          IFEQ      'N'
236300091023IF  2C*    VC(89)        IFEQ      'S'                                          *VOCE DA CALCOLARE
236400091023     C*                  EXSR      CALESR
236500091023E   2C*                  ENDIF
236600091023E   1C*                  ENDIF
236700091023     C*
236800091023     C* CALCOLA A.D.G. SULL'ESPRESSO RITIRO ISOLA
236900091023IF  1C     WERR          IFEQ      'N'
237000091023IF  2C     VC(90)        IFEQ      'S'                                          *VOCE DA CALCOLARE
237100091023     C                   EXSR      CALAER
237200091023E   2C                   ENDIF
237300091023E   1C                   ENDIF
237400091023     C*
237500091023     C* CALCOLA ISTAT SULL'ESPRESSO RITIRO ISOLA
237600091023IF  1C     WERR          IFEQ      'N'
237700091023IF  2C     VC(91)        IFEQ      'S'                                          *VOCE DA CALCOLARE
237800091023     C                   EXSR      CALIER
237900091023E   2C                   ENDIF
238000091023E   1C                   ENDIF
238100091023     C*
238200091023     C* CALCOLA ISTAT SULL'A.D.G. DELL'ESPRESSO RITIRO ISOLA
238300091023IF  1C     WERR          IFEQ      'N'
238400091023IF  2C     VC(92)        IFEQ      'S'                                          *VOCE DA CALCOLARE
238500091023     C                   EXSR      CAIAER
238600091023E   2C                   ENDIF
238700091023E   1C                   ENDIF
238800091023     C*
238900091023     C* CALCOLA ESPRESSO SULLA CONSEGNA ISOLA
239000091023     c* non lo deve + fare l'espresso va calcolato una volta sola sulla
239100091023     c* somma del porto, isola, inoltr, diasgiate, ritiro isola
239200091023IF  1C*    WERR          IFEQ      'N'
239300091023IF  2C*    VC(81)        IFEQ      'S'                                          *VOCE DA CALCOLARE
239400091023     C*                  EXSR      CALESS
239500091023E   2C*                  ENDIF
239600091023E   1C*                  ENDIF
239700091023     C*
239800091023     C* CALCOLA A.D.G. SULL'ESPRESSO CONSEGNA ISOLA
239900091023IF  1C     WERR          IFEQ      'N'
240000091023IF  2C     VC(82)        IFEQ      'S'                                          *VOCE DA CALCOLARE
240100091023     C                   EXSR      CALAES
240200091023E   2C                   ENDIF
240300091023E   1C                   ENDIF
240400091023     C*
240500091023     C* CALCOLA ISTAT SULL'ESPRESSO CONSEGNA ISOLA
240600091023IF  1C     WERR          IFEQ      'N'
240700091023IF  2C     VC(83)        IFEQ      'S'                                          *VOCE DA CALCOLARE
240800091023     C                   EXSR      CALIES
240900091023E   2C                   ENDIF
241000091023E   1C                   ENDIF
241100091023     C*
241200091023     C* CALCOLA ISTAT SULL'A.D.G. DELL'ESPRESSO CONSEGNA ISOLA
241300091023IF  1C     WERR          IFEQ      'N'
241400091023IF  2C     VC(84)        IFEQ      'S'                                          *VOCE DA CALCOLARE
241500091023     C                   EXSR      CAIAES
241600091023E   2C                   ENDIF
241700091023E   1C                   ENDIF
241800091023     C*
241900980703     C* CALCOLA LA BASE PER IL CALCOLO DELLE TARIFFE CON % SUL TRASPORTO
242000980706     C                   EXSR      CALTBT
242100980702     C*---
242200980702     C* CONSEGNE PARTICOLARI
242300980702     C*---
242400060314     C* CALCOLA ASSICURAZIONE ac plus (FRANCO E ASSEGNATO)
242500970306IF  1C     WERR          IFEQ      'N'
242600970306IF  2C     VC(9)         IFEQ      'S'                                          *VOCE DA CALCOLARE
242700970306     C     VC(31)        OREQ      'S'                                          *VOCE DA CALCOLARE
242800060315     C* REIMPOSTA VARIABILI DI LAVORO
242900060315     C                   Z-ADD     ETCASC        IASLIT           23 8
243000060315     C                   MOVEL     'C'           KTPFTC
243100970306     C                   EXSR      CALASC
243200001227     C                   EXSR      CAL048
243300001227     C                   EXSR      CAL065
243400001227     C                   EXSR      CAL060
243500001227     C                   EXSR      CAL077
243600970306E   2C                   ENDIF
243700970306E   1C                   ENDIF
243800060314     C* CALCOLA ASSICURAZIONE ac base (FRANCO E ASSEGNATO)
243900060315     c* no per cartello
244000060315     C                   MOVEL     D00KSC        WA4               4 0
244100060315     c                   if        wa4  <> 8888
244200060314IF  1C     WERR          IFEQ      'N'
244300060314IF  2C     VC(105)       IFEQ      'S'                                          *VOCE DA CALCOLARE
244400060314     C     VC(108)       OREQ      'S'                                          *VOCE DA CALCOLARE
244500060315     C                   Z-ADD     0             IASLIT           23 8
244600060315     C                   MOVEL     'd'           KTPFTC
244700060314     C                   EXSR      CALAcb
244800060314     C                   EXSR      CAL106
244900060314     C                   EXSR      CAL107
245000060314     C                   EXSR      CAL109
245100060314     C                   EXSR      CAL110
245200060314E   2C                   ENDIF
245300060314E   1C                   ENDIF
245400060315E   1C                   ENDIF
245500970306     C*
245600970306     C* CALCOLA ASSICURAZIONE R.C.V. (FRANCO E ASSEGNATO)
245700970306IF  1C     WERR          IFEQ      'N'
245800970306IF  2C     VC(10)        IFEQ      'S'                                          *VOCE DA CALCOLARE
245900970306     C     VC(32)        OREQ      'S'                                          *VOCE DA CALCOLARE
246000970306     C                   EXSR      CALRCV
246100001227     C                   EXSR      CAL049
246200001227     C                   EXSR      CAL066
246300001227     C                   EXSR      CAL061
246400001227     C                   EXSR      CAL078
246500970306E   2C                   ENDIF
246600970306E   1C                   ENDIF
246700970306     C*
246800970306     C* CALCOLA CONSEGNA PARTICOLARE SU APPUNTAMENTO
246900970306IF  1C     WERR          IFEQ      'N'
247000970306IF  2C     VC(11)        IFEQ      'S'                                          *VOCE DA CALCOLARE
247100970306     C                   EXSR      CALCPA
247200001227     C                   EXSR      CAL050
247300001227     C                   EXSR      CAL067
247400970306E   2C                   ENDIF
247500970306E   1C                   ENDIF
247600970306     C*
247700970306     C* CALCOLA CONSEGNA PARTICOLARE AI SUPERMERCATI
247800970306IF  1C     WERR          IFEQ      'N'
247900970306IF  2C     VC(12)        IFEQ      'S'                                          *VOCE DA CALCOLARE
248000970306     C                   EXSR      CALCPS
248100001227     C                   EXSR      CAL051
248200001227     C                   EXSR      CAL068
248300970306E   2C                   ENDIF
248400970306E   1C                   ENDIF
248500970306     C*
248600970306     C* CALCOLA CONSEGNA PARTICOLARE AI PIANI
248700970306IF  1C     WERR          IFEQ      'N'
248800970306IF  2C     VC(13)        IFEQ      'S'                                          *VOCE DA CALCOLARE
248900970306     C                   EXSR      CALCPP
249000001227     C                   EXSR      CAL052
249100001227     C                   EXSR      CAL069
249200970306E   2C                   ENDIF
249300970306E   1C                   ENDIF
249400151120     C*
249500151120     C* CALCOLA packing list
249600151120IF  1C     WERR          IFEQ      'N'
249700151120IF  2C     VC(114)       IFEQ      'S'                                          *VOCE DA CALCOLARE
249800151120     C                   EXSR      CALpkl
249900151120     C                   EXSR      CAL115
250000151120     C                   EXSR      CAL116
250100151120E   2C                   ENDIF
250200151120E   1C                   ENDIF
250300151120     C*
250400151120     C* CALCOLA ritiro telefonico
250500151120IF  1C     WERR          IFEQ      'N'
250600151120IF  2C     VC(111)       IFEQ      'S'                                          *VOCE DA CALCOLARE
250700151120     C                   EXSR      CALrtm
250800151120     C                   EXSR      CAL112
250900151120     C                   EXSR      CAL113
251000151120E   2C                   ENDIF
251100151120E   1C                   ENDIF
251200040826     C*
251300040826     C* CALCOLA CONSEGNA destinatario disagiato
251400040826IF  1C     WERR          IFEQ      'N'
251500040826IF  2C     VC(96)        IFEQ      'S'                                          *VOCE DA CALCOLARE
251600040826     C                   EXSR      CALdes
251700040826     C                   EXSR      CAL097
251800040826     C                   EXSR      CAL098
251900040826E   2C                   ENDIF
252000040826E   1C                   ENDIF
252100970306     C*
252200970306     C* CALCOLA CONSEGNA PARTICOLARE FUORI MISURA
252300970306IF  1C     WERR          IFEQ      'N'
252400970306IF  2C     VC(14)        IFEQ      'S'                                          *VOCE DA CALCOLARE
252500970306     C                   EXSR      CALCPF
252600001227     C                   EXSR      CAL053
252700001227     C                   EXSR      CAL070
252800970306E   2C                   ENDIF
252900970306E   1C                   ENDIF
253000970414     C*
253100970414     C* CALCOLA CONSEGNA PARTICOLARE DDT
253200970414IF  1C     WERR          IFEQ      'N'
253300970414IF  2C     VC(33)        IFEQ      'S'                                          *VOCE DA CALCOLARE
253400970414     C                   EXSR      CALCPD
253500001227     C                   EXSR      CAL062
253600001227     C                   EXSR      CAL079
253700970414E   2C                   ENDIF
253800970414E   1C                   ENDIF
253900980702     C*
254000980702     C* CALCOLA CONSEGNA PARTICOLARE RITIRO
254100980702IF  1C     WERR          IFEQ      'N'
254200980702IF  2C     VC(17)        IFEQ      'S'                                          *VOCE DA CALCOLARE
254300980702     C                   EXSR      CALRIT
254400001227     C                   EXSR      CAL056
254500001227     C                   EXSR      CAL073
254600980702E   2C                   ENDIF
254700980702E   1C                   ENDIF
254800980702     C*
254900980702     C* CALCOLA LA PROVVIGIONE ASSEGNO MITTENTE
255000980702IF  1C     WERR          IFEQ      'N'
255100980702IF  2C     VC(15)        IFEQ      'S'                                          *VOCE DA CALCOLARE
255200980702     C                   EXSR      CALPAM
255300001227     C                   EXSR      CAL054
255400001227     C                   EXSR      CAL071
255500980702E   2C                   ENDIF
255600980702E   1C                   ENDIF
255700980702     C*
255800980702     C* CALCOLA LA PROVVIGIONE ASSEGNO BARTOLINI
255900980702IF  1C     WERR          IFEQ      'N'
256000980702IF  2C     VC(16)        IFEQ      'S'                                          *VOCE DA CALCOLARE
256100980702     C                   EXSR      CALPAB
256200001227     C                   EXSR      CAL055
256300001227     C                   EXSR      CAL072
256400980702E   2C                   ENDIF
256500980702E   1C                   ENDIF
256600980702     C*
256700980702     C* CALCOLA IL DIRITTO FISSO
256800980702IF  1C     WERR          IFEQ      'N'
256900980702IF  2C     VC(2)         IFEQ      'S'                                          *VOCE DA CALCOLARE
257000980702     C                   EXSR      CALDIF
257100980702E   2C                   ENDIF
257200980702E   1C                   ENDIF
257300980702     C*
257400980702     C* CALCOLA A.D.G. SUL DIRITTO FISSO
257500980702IF  1C     WERR          IFEQ      'N'
257600980702IF  2C     VC(22)        IFEQ      'S'                                          *VOCE DA CALCOLARE
257700980702     C                   EXSR      CALAGD
257800980702E   2C                   ENDIF
257900980702E   1C                   ENDIF
258000980702     C*
258100980702     C* CALCOLA ISTAT SUL DIRITTO FISSO
258200980702IF  1C     WERR          IFEQ      'N'
258300980702IF  2C     VC(25)        IFEQ      'S'                                          *VOCE DA CALCOLARE
258400980702     C                   EXSR      CALISD
258500980702E   2C                   ENDIF
258600980702E   1C                   ENDIF
258700980702     C*
258800980702     C* CALCOLA ISTAT SULL'A.D.G. DEL DIRITTO FISSO
258900980702IF  1C     WERR          IFEQ      'N'
259000980702IF  2C     VC(28)        IFEQ      'S'                                          *VOCE DA CALCOLARE
259100980702     C                   EXSR      CALIAD
259200980702E   2C                   ENDIF
259300980702E   1C                   ENDIF
259400990819     C*
259500990819     C* CALCOLA T.C.P.
259600990819IF  1C     WERR          IFEQ      'N'
259700990819IF  2C     VC(5)         IFEQ      'S'                                          *VOCE DA CALCOLARE
259800990819     C                   EXSR      CALTCP
259900001222     C                   EXSR      CAL046
260000001227     C                   EXSR      CAL063
260100990819E   2C                   ENDIF
260200990819E   1C                   ENDIF
260300040826     C*
260400040826     C* CALCOLA bancali
260500040826IF  1C     WERR          IFEQ      'N'
260600040826IF  2C     VC(99)        IFEQ      'S'                                          *VOCE DA CALCOLARE
260700040826     C                   EXSR      CALban
260800040826     C                   EXSR      CAL100
260900040826     C                   EXSR      CAL101
261000040826E   2C                   ENDIF
261100040826E   1C                   ENDIF
261200040826     C*
261300040826     C* diritto di pesatura
261400040826IF  1C     WERR          IFEQ      'N'
261500040826IF  2C     §tatap        IFEQ      'S'                                          *VOCE DA CALCOLARE
261600040826     C                   EXSR      CALdps
261700040826     C                   EXSR      CAL103
261800040826     C                   EXSR      CAL104
261900040826E   2C                   ENDIF
262000040826E   1C                   ENDIF
262100970306     C*
262200970306     C                   ENDSR
262300980707     C*------------------------------------------------------------------------*
262400091023     C* calimpesp - riproporziona l'importo dell'espresso in base
262500091023     c* ai valori delle voci originali
262600980707     C*------------------------------------------------------------------------*
262700091023     C     calimpesp     BEGSR
262800091026     c* ripartisco solo se applicata tpttpg = 'T'
262900091026     c                   if        tpg ='T'
263000091023     c* porto
263100091023     c                   if        vimp(1) <> 0
263200091023     c     tasesp        mult      vimp(1)       vimp(7)
263300091023     c                   div       impon         vimp(7)
263400091023     c                   else
263500091023     c                   clear                   vimp(7)
263600091023     c                   end
263700091023     c* inoltro
263800091023     c                   if        vimp(6) <> 0
263900091023     c     tasesp        mult      vimp(6)       vimp(36)
264000091023     c                   div       impon         vimp(36)
264100091023     c                   else
264200091023     c                   clear                   vimp(36)
264300091023     c                   end
264400091023     c* disagiate
264500091023     c                   if        vimp(35) <> 0
264600091023     c     tasesp        mult      vimp(35)      vimp(39)
264700091023     c                   div       impon         vimp(39)
264800091023     c                   else
264900091023     c                   clear                   vimp(39)
265000091023     c                   end
265100091023     c* consegna isole
265200091023     c                   if        vimp(80) <> 0
265300091023     c     tasesp        mult      vimp(80)      vimp(81)
265400091023     c                   div       impon         vimp(81)
265500091023     c                   else
265600091023     c                   clear                   vimp(81)
265700091023     c                   end
265800091023     c* ritiro isole
265900091023     c                   if        vimp(88) <> 0
266000091023     c     tasesp        mult      vimp(88)      vimp(89)
266100091023     c                   div       impon         vimp(89)
266200091023     c                   else
266300091023     c                   clear                   vimp(89)
266400091023     c                   end
266500091026     c*
266600091026     c                   else
266700091026     c                   eval      vimp(7) = tasesp
266800091026     c                   clear                   vimp(89)
266900091026     c                   clear                   vimp(81)
267000091026     c                   clear                   vimp(39)
267100091026     c                   clear                   vimp(36)
267200091026     c                   end
267300091023     c*
267400091023     C                   ENDSR
267500091023     C*------------------------------------------------------------------------*
267600091023     C* CALPEI - CALCOLA PORTO E INOLTRO
267700091023     C*------------------------------------------------------------------------*
267800091023     C     CALPEI        BEGSR
267900980707     C*
268000011031     C                   Z-ADD     *ZEROS        TASPOR           21 8
268100011031     C                   Z-ADD     *ZEROS        TASINL           21 8
268200980707     C*****
268300980707     C** CALCOLO    P O R T O     ED    I N O L T R O
268400980707     C*****
268500980707     C                   DO        11            A
268600980707     C     A             OCCUR     TARDS
268700980707     C     TSCG          IFGE      PESTAS
268800980707     C                   GOTO      ENDTAS
268900980707     C                   END
269000980707     C                   END
269100980707     C** NON TROVATO SCAGLIONE ESATTO
269200980707     C     TSCG          IFLT      PESTAS
269300980707     C                   MOVEL     MSG(25)       $MSG
269400980707     C                   EXSR      IMPERR
269500980707     C                   GOTO      FINE
269600980707     C                   END
269700980707     C     ENDTAS        TAG
269800980707     C** MI SALVO LA POSIZIONE DELLO SCAGLIO TROVATO CHE MI
269900980707     C*  SERVE PER IL CALCOLO TARIFFA CON GARANTITO MAX SCAGLIONE PREC
270000980707     C     *LIKE         DEFINE    A             WA
270100980707     C                   Z-ADD     A             WA
270200980707     C**
270300980707     C* CALCOLO PER QUALE VALORE MOLTIPLICARE LA TARIFFA
270400980707     C                   Z-ADD     PESTAS        WPTAS3
270500980707     C                   EXSR      CALQLI
270600980707     C**
270700980707     C* MODO APPLICAZIONE TARIFFA
270800980707     C* CALCOLO NORMALE ANCHE PER SCALARE AL DI SOTTO
270900980707     C*  DELL'APPLICAZIONE TARIFFA FINITA E PER MAX SCAGLIONE PREC
271000980707     C     §TAF02        IFNE      'S'
271100980707     C     §TAF02        OREQ      'S'
271200980707     C     WATA          ANDNE     ' '
271300980707     C** MOLTIPLICAZIONE TARIFFA
271400980707     C                   EXSR      MULTAR
271500980707     C* APPLICAZIONE MINIMO E MASSIMO
271600980707     C                   EXSR      MINMAX
271700980707     C                   Z-ADD     WPOR          TASPOR
271800980707     C                   Z-ADD     WINL          TASINL
271900980707     C                   ENDIF
272000980707     C**
272100980707     C** S C A L A R E
272200980707     C     §TAF02        IFEQ      'S'
272300980707     C                   EXSR      SCALAR
272400980707     C                   ENDIF
272500980707     C**
272600980707     C** M A X   S C A G L I O N E   P R E C E D E N TE
272700980707     C     §TAF02        IFEQ      'G'
272800980707     C                   EXSR      MAXSCA
272900980707     C                   ENDIF
273000980707     C* PORTO
273100980707IF  1C     VC(1)         IFEQ      'S'                                          *VOCE DA CALCOLARE
273200980707IF  2C     TASPOR        IFGT      *ZEROS
273300980707     C                   Z-ADD     1             I
273400980707     C                   Z-ADD     TASPOR        VIMP(I)
273500980707E   2C                   ENDIF
273600980707E   1C                   ENDIF
273700980707     C* INOLTRO
273800980707IF  1C     VC(6)         IFEQ      'S'                                          *VOCE DA CALCOLARE
273900980707IF  2C     TASINL        IFGT      *ZEROS
274000980707     C                   Z-ADD     6             I
274100980707     C                   Z-ADD     TASINL        VIMP(I)
274200980707E   2C                   ENDIF
274300980707E   1C                   ENDIF
274400980707     C**
274500980707     C     FINE          TAG
274600980707     C                   ENDSR
274700980707     C*****************************************************************
274800980707     C** VALORE COL QUALE  MOLTIPLICARE LA TARIFFA
274900980707     C*****************************************************************
275000980707     C     CALQLI        BEGSR
275100980707     C                   Z-ADD     0             PESQLI           11 3
275200980707     C** SE LO SCAGLIONE SCELTO PER TASSARE E' INFERIORE AL MINIMO
275300980707     C** TASSABILE DA TARIFFA FORZO PESO TASSABILE = MINTAS
275400980707     C**
275500980707     C* WATA MI IDENTIFICA SE L'APPLICAZIONE TARIFFA FINITA E' DOVUTA
275600980707     C*  ALLO SCAGLIONE CONFRONTATO CON MINTAR (R)
275700980707     C*  O SE E' DOVUTA DAL PESO TASSABILE CONFRONTATO COL MINIMO
275800980707     C*  TASSABILE DI QUELLA TARIFFA (S) - differenza che ora non serve
275900980707     C*  + ma dal momento che c'e' la lascio
276000980707     C                   CLEAR                   WATA
276100980707    1C                   SELECT
276200980707     C     TSCG          WHENLE    MINTAR
276300980707     C                   Z-ADD     1             PESQLI
276400980707     C                   MOVEL     'R'           WATA              1
276500980707     C**
276600980707     C** SE PESO TASSABILE E' ANCORA INFERIORE
276700980707     C*  AL MINIMO TASSABILE DA TARIFFA IMPOSTO PESO TASS = 1
276800980707     C*  NON LO CONTROLLO PER LA TARIFFA A SCALARE PERCHE' ABBIAMO
276900980707     C*  SEMPRE INSERITO UNO SCAGLIONE=A MINIMO TASSABILE E
277000980707     C*  QUINDI E' GIA' CONTROLLATO NEL WHLE PRECEDENTE
277100980707     C     WPTAS3        WHENLE    MINTAS
277200980707     C     §TAF02        ANDNE     'S'
277300980707     C                   Z-ADD     1             PESQLI
277400980707     C                   MOVEL     'S'           WATA
277500980707   X1C                   OTHER
277600980707     C** CALCOLO : PESO IN Q.LI PER TARIFFA A PESO
277700980707     C** CALCOLO   VALORE/100   PER TARIFFA A VALORE (ESSENDO UNA %)
277800980707     C** PER LE ALTRE TARIFFE PESO TASSABILE INVARIATO
277900980707    2C     UNOCTR        IFEQ      0
278000980707     C     UNOCTR        OREQ      4
278100980707     C     WPTAS3        DIV       100           PESQLI
278200980707   X2C                   ELSE
278300980707     C                   Z-ADD     WPTAS3        PESQLI
278400980707    2C                   END
278500980707    1C                   ENDSL
278600980707     C                   ENDSR
278700980707     C*****************************************************************
278800980707     C* MOLTIPLICATO TARIFFA PER VALORE TROVATO (PESO TASSABILE)
278900980707     C*****************************************************************
279000980707     C     MULTAR        BEGSR
279100980707     C     *LIKE         DEFINE    TASPOR        WPOR
279200980707     C     *LIKE         DEFINE    TASINL        WINL
279300980707     C                   CLEAR                   WPOR
279400980707     C                   CLEAR                   WINL
279500980707     C**
279600980707     C     TITR          MULT(H)   PESQLI        WPOR                           ** PORTO
279700980707     C     TINO          MULT(H)   PESQLI        WINL                           ** INOLTRO
279800980707     C                   ENDSR
279900980707     C*****************************************************************
280000980707     C* CALCOLO TARIFFA A SCALARE
280100980707     C*****************************************************************
280200980707     C     SCALAR        BEGSR
280300980707     C**
280400980707     C     *LIKE         DEFINE    PESTAS        WPTAS1
280500980707     C     *LIKE         DEFINE    PESTAS        WDIFSC
280600980707     C     *LIKE         DEFINE    PESTAS        WPTAS3
280700980707     C     *LIKE         DEFINE    PESQLI        WPESQ
280800980707     C     *LIKE         DEFINE    PESQLI        WPESSV
280900980707     C     *LIKE         DEFINE    TITR          WTITR
281000980707     C     *LIKE         DEFINE    TITR          WSTITR
281100980707     C     *LIKE         DEFINE    TINO          WTINO
281200980707     C     *LIKE         DEFINE    TINO          WSTINO
281300980707     C     *LIKE         DEFINE    TSCG          WPRESC
281400980707     C**
281500980707     C                   CLEAR                   TASPOR
281600980707     C                   CLEAR                   TASINL
281700980707     C                   CLEAR                   WTROVA
281800980707     C                   CLEAR                   WPTAS3
281900980707     C                   CLEAR                   WDIFSC
282000980707     C                   CLEAR                   WPRESC
282100980707     C                   CLEAR                   WPESSV
282200980707     C                   CLEAR                   WPESQ
282300980707     C                   CLEAR                   WTITR
282400980707     C                   CLEAR                   WSTITR
282500980707     C                   CLEAR                   WTINO
282600980707     C                   CLEAR                   WSTINO
282700980707     C                   Z-ADD     PESTAS        WPTAS1
282800980707     C                   Z-ADD     1             A
282900980707     C**
283000980805    1C     A             DOWLE     11
283100980707     C     WTROVA        ANDEQ     ' '
283200980707     C     TSCG          ANDGT     0
283300980707     C     A             OCCUR     TARDS
283400980707     C**
283500980707     C* FACCIO LA DIFFERENZA TRA LO SCAGLIONE LETTO ED IL PRECEDENTE
283600980707     C     TSCG          SUB       WPRESC        WDIFSC
283700980707     C**
283800980707     C* SOTRAGGO DAL PESO TASSABILE RIMASTO LA PARTE DA TASSARE ORA
283900980707     C* IN WPTAS1 HO SEMPRE LA PARTE DI PESO TASSABILE ANCORA DA TASSAR
284000980707     C                   SUB       WDIFSC        WPTAS1
284100980707     C**
284200980707     C** SE IL VALORE E' DIVENTATO < DI 0 SIGNIFICA CHE HO TERMINATO LA
284300980707     C**  TASSAZIONE PER CUI USO SOLO LA PARTE RIMASTA DI WPTAS1
284400980707    2C     WPTAS1        IFLE      0
284500980707     C     WDIFSC        ADD       WPTAS1        WPTAS3
284600980707     C* PESO TASSABILE GLOBALE DELLA SPEDIZIONE
284700980707     C                   MOVEL     'S'           WTROVA            1
284800980707     C                   ELSE
284900980707     C**
285000980707     C* PARTE DI PESO DA TASSARE CON QUESTO SCAGLIONE
285100980707     C                   Z-ADD     WDIFSC        WPTAS3
285200980707     C* SALVO LO SCAGLIONE PRECEDENTE
285300980707     C                   Z-ADD     TSCG          WPRESC
285400980707    2C                   ENDIF
285500980707     C**
285600980707     C* IMPOSTO IL VALORE DA MOLTIPLICARE PER LA TARIFFA
285700980707     C                   EXSR      CALQLI
285800980707     C**
285900980707     C* SE LO SCAGLIONE E' APPLICAZIONE TARIFFA FINITA DEVO CONSIDERARE
286000980707     C*  SOLO L'ULTIMO SCAGLIONE COSI' E NON I PRECEDENTI
286100980707    2C     WATA          IFNE      ' '
286200980707     C     WTROVA        ANDEQ     ' '
286300980707     C* MI SALVO IL VALORE MA PER ORA NON LO MOLTIPLICO
286400980707     C                   Z-ADD     PESQLI        WPESQ
286500980707     C                   Z-ADD     TITR          WTITR
286600980707     C                   Z-ADD     TINO          WTINO
286700980707   X2C                   ELSE
286800980707     C**
286900980707     C* SE LO SCAGLIONE TROVATO E' L'ULTIMO MA COMUNQUE TARIFFA FINITA
287000980707     C* DEVO MOLTIPLICARE SOLO QUESTO --> AZZERO UN EVENTUALE PRECEDENT
287100980707     C*  CHE AVEVA LA TASSAZIONE TARIFFA FINITA
287200980707    3C     WTROVA        IFEQ      'S'
287300980707     C     WATA          ANDNE     ' '
287400980707     C                   CLEAR                   WPESQ
287500980707     C                   CLEAR                   WTITR
287600980707     C                   CLEAR                   WTINO
287700980707    3C                   ENDIF
287800980707     C*
287900980707     C* PRIMA DI MOLTIPLICARE LA TARIFFA TROVATA VEDO SE CE NE E'
288000980707     C*  UNA PRECEDENTE TARIFFA FINITA DA CONSIDERARE
288100980707    3C     WPESQ         IFGT      0
288200980707     C* SALVO PESO DA MOLTIPLICARE E TARIFFE DELLO SCAGLIONE
288300980707     C*  APPENA LETTO
288400980707     C                   Z-ADD     PESQLI        WPESSV
288500980707     C                   Z-ADD     TITR          WSTITR
288600980707     C                   Z-ADD     TINO          WSTINO
288700980707     C* IMPOSTO PESO DA MOLTIPLICARE E TARIFFE DELLO SCAGLIONE
288800980707     C*  PRECEDENTE PER IL CALCOLO
288900980707     C                   Z-ADD     WPESQ         PESQLI
289000980707     C                   Z-ADD     WTITR         TITR
289100980707     C                   Z-ADD     WTINO         TINO
289200980707     C                   EXSR      MULTAR
289300980707     C                   ADD       WPOR          TASPOR
289400980707     C                   ADD       WINL          TASINL
289500980707     C                   CLEAR                   WPESQ
289600980707     C                   CLEAR                   WTITR
289700980707     C                   CLEAR                   WTINO
289800980707     C* REIMPOSTO I DATI DELLO SCAGLIONE APPENA LETTO
289900980707     C                   Z-ADD     WPESSV        PESQLI
290000980707     C                   Z-ADD     WSTITR        TITR
290100980707     C                   Z-ADD     WSTINO        TINO
290200980707    3C                   ENDIF
290300980707     C**
290400980707     C* MOLTIPLICAZIONE TARIFFA
290500980707     C                   EXSR      MULTAR
290600980707     C                   ADD       WPOR          TASPOR
290700980707     C                   ADD       WINL          TASINL
290800980707    2C                   ENDIF
290900980707     C                   ADD       1             A
291000980707     C**
291100980707    1C                   ENDDO
291200980707     C**
291300980707     C* APPLICO MINIMO E MASSIMO
291400980707     C                   Z-ADD     TASPOR        WPOR
291500980707     C                   Z-ADD     TASINL        WINL
291600980707     C                   EXSR      MINMAX
291700980707     C                   Z-ADD     WPOR          TASPOR
291800980707     C                   Z-ADD     WINL          TASINL
291900980707     C                   ENDSR
292000980707     C*****************************************************************
292100980707     C* CALCOLO MAX SCAGLIONE PRECEDENTE
292200980707     C*****************************************************************
292300980707     C     MAXSCA        BEGSR
292400980707     C* SOLO SE LO SCAGLIONE APPLICATO NON ERA IL PRIMO
292500980707     C     WA            IFGT      1
292600980707     C* PER CONFRONTARLO CON IL VALORE NORMALE CALCOLATO
292700980707     C* IL CONFRONTO E' DATO DALLA SOMMA TRA PORT E INOLTRO
292800980707     C                   SUB       1             WA
292900980707     C     WA            OCCUR     TARDS
293000980707     C* CALCOLO PESO DA MOLTIPLICARE
293100980707     C                   Z-ADD     TSCG          WPTAS3
293200980707     C                   EXSR      CALQLI
293300980707     C** MOLTIPLICO LA TARIFFA
293400980707     C                   EXSR      MULTAR
293500980707     C** MINIMO E MASSIMO
293600980707     C                   EXSR      MINMAX
293700980707     C**
293800011018     C     TASPOR        ADD       TASINL        T0100            18 5
293900011018     C     WPOR          ADD       WINL          W0100            18 5
294000980707     C** PRENDO DELLE 2 LA TARIFFA PIU' ALTA
294100980707     C     W0100         IFGT      T0100
294200980707     C                   Z-ADD     WPOR          TASPOR
294300980707     C                   Z-ADD     WINL          TASINL
294400980707     C                   ENDIF
294500980707     C                   ENDIF
294600980707     C**
294700980707     C                   ENDSR
294800980707     C*****************************************************************
294900980707     C** MINIMO E MASSIMO DA APPLICARE -SE APPLICATO TUTTO NEL PORTO
295000980707     C*****************************************************************
295100980707     C     MINMAX        BEGSR
295200980707     C** NON ESISTE MINIMO E MASSIMO
295300980707     C     TMIN          IFEQ      0
295400980707     C     TMAX          ANDEQ     0
295500980707     C                   GOTO      DOPOMM
295600980707     C                   END
295700980707     C** MINIMO
295800011018     C     WPOR          ADD       WINL          WMAX             18 5
295900980707     C     TMIN          IFGT      WMAX
296000980707     C                   Z-ADD     TMIN          WPOR
296100980707     C                   Z-ADD     0             WINL
296200980707     C                   Z-ADD     TMIN          WMAX
296300980707     C                   END
296400980707     C** MASSIMO
296500980707     C     TMAX          IFGT      0
296600980707     C     WMAX          IFGT      TMAX
296700980707     C                   Z-ADD     TMAX          WPOR
296800980707     C                   Z-ADD     0             WINL
296900980707     C                   END
297000980707     C                   END
297100980707     C     DOPOMM        ENDSR
297200980706     C*------------------------------------------------------------------------*
297300980706     C* CALAGP - CALCOLA L'A.D.G. SUL PORTO
297400980706     C*------------------------------------------------------------------------*
297500980706     C     CALAGP        BEGSR
297600980706     C*
297700980706     C* REIMPOSTA LE VARIABILI DI LAVORO
297800011031     C                   Z-ADD     *ZEROS        TASADG           21 8
297900980706     C*
298000980706     C* CALCOLA L'A.D.G.
298100011031     C                   Z-ADD     TASPOR        WIMP             23 8
298200980706     C                   EXSR      ADGCAL
298300980706     C*
298400980706IF  1C     TASADG        IFGT      *ZEROS
298500980706     C                   Z-ADD     3             I
298600980706     C                   Z-ADD     TASADG        VIMP(I)
298700980706E   1C                   ENDIF
298800980706     C*
298900980706     C                   ENDSR
299000980706     C*------------------------------------------------------------------------*
299100980706     C* CALISP - CALCOLA L'ISTAT SUL PORTO
299200980706     C*------------------------------------------------------------------------*
299300980706     C     CALISP        BEGSR
299400980706     C*
299500980706     C* REIMPOSTA LE VARIABILI DI LAVORO
299600011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
299700980706     C*
299800980706     C* CALCOLA L'ISTAT
299900011031     C                   Z-ADD     TASPOR        WIMP             23 8
300000980706     C                   EXSR      IACCAL
300100980706     C*
300200980706IF  1C     TASIAC        IFGT      *ZEROS
300300980706     C                   Z-ADD     4             I
300400980706     C                   Z-ADD     TASIAC        VIMP(I)
300500980706E   1C                   ENDIF
300600980706     C*
300700980706     C                   ENDSR
300800980706     C*------------------------------------------------------------------------*
300900980706     C* CALIAP - CALCOLA L'ISTAT SULL'ADG DEL PORTO
301000980706     C*------------------------------------------------------------------------*
301100980706     C     CALIAP        BEGSR
301200980706     C*
301300980706     C* REIMPOSTA LE VARIABILI DI LAVORO
301400011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
301500980706     C*
301600980706     C* CALCOLA L'ISTAT
301700011031     C                   Z-ADD     VIMP(3)       WIMP             23 8
301800980706     C                   EXSR      IACCAL
301900980706     C*
302000980706IF  1C     TASIAC        IFGT      *ZEROS
302100980706     C                   Z-ADD     27            I
302200980706     C                   Z-ADD     TASIAC        VIMP(I)
302300980706E   1C                   ENDIF
302400980706     C*
302500980706     C                   ENDSR
302600970306     C*------------------------------------------------------------------------*
302700970717     C* CALESP - CALCOLA L'ESPRESSO PORTO
302800970306     C*------------------------------------------------------------------------*
302900970306     C     CALESP        BEGSR
303000970306     C*
303100970306     C* REIMPOSTA LE VARIABILI DI LAVORO
303200011031     C                   Z-ADD     *ZEROS        TASESP           21 8
303300970306     C*
303400091023     C* CALCOLA L'ESPRESSO sul porto + inoltro + disagiate + ritiro
303500091023     c* isola + consegna isola
303600091023     C*                  Z-ADD     TASPOR        IMPON            23 8
303700091023     c                   eval      impon = vimp(1)+ vimp(6)+
303800091023     c                             vimp(35) + vimp(80) + vimp(88)
303900091023     c*
304000970306     C                   EXSR      CALTS
304100970306     C*
304200970314IF  1C     TASESP        IFGT      *ZEROS
304300091023     C*                  Z-ADD     7             I
304400091023     C*                  Z-ADD     TASESP        VIMP(I)
304500970314E   1C                   ENDIF
304600970306     C*
304700970306     C                   ENDSR
304800980706     C*------------------------------------------------------------------------*
304900980706     C* CALAEP - CALCOLA L'A.D.G. SULL'ESPRESSO PORTO
305000980706     C*------------------------------------------------------------------------*
305100980706     C     CALAEP        BEGSR
305200980706     C*
305300980706     C* REIMPOSTA LE VARIABILI DI LAVORO
305400011031     C                   Z-ADD     *ZEROS        TASADG           21 8
305500980706     C*
305600980706     C* CALCOLA L'A.D.G.
305700011031     C                   Z-ADD     VIMP(7)       WIMP             23 8
305800980706     C                   EXSR      ADGCAL
305900980706     C*
306000980706IF  1C     TASADG        IFGT      *ZEROS
306100980706     C                   Z-ADD     23            I
306200980706     C                   Z-ADD     TASADG        VIMP(I)
306300980706E   1C                   ENDIF
306400980706     C*
306500980706     C                   ENDSR
306600980706     C*------------------------------------------------------------------------*
306700980706     C* CALIEP - CALCOLA L'ISTAT SULL'ESPRESSO PORTO
306800980706     C*------------------------------------------------------------------------*
306900980706     C     CALIEP        BEGSR
307000980706     C*
307100980706     C* REIMPOSTA LE VARIABILI DI LAVORO
307200011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
307300980706     C*
307400980706     C* CALCOLA L'ISTAT
307500011031     C                   Z-ADD     VIMP(7)       WIMP             23 8
307600980706     C                   EXSR      IACCAL
307700980706     C*
307800980706IF  1C     TASIAC        IFGT      *ZEROS
307900980706     C                   Z-ADD     26            I
308000980706     C                   Z-ADD     TASIAC        VIMP(I)
308100980706E   1C                   ENDIF
308200980706     C*
308300980706     C                   ENDSR
308400980706     C*------------------------------------------------------------------------*
308500980706     C* CAIAEP - CALCOLA L'ISTAT SULL'ADG DELL'ESPRESSO PORTO
308600980706     C*------------------------------------------------------------------------*
308700980706     C     CAIAEP        BEGSR
308800980706     C*
308900980706     C* REIMPOSTA LE VARIABILI DI LAVORO
309000011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
309100980706     C*
309200980706     C* CALCOLA L'ISTAT
309300011031     C                   Z-ADD     VIMP(23)      WIMP             23 8
309400980706     C                   EXSR      IACCAL
309500980706     C*
309600980706IF  1C     TASIAC        IFGT      *ZEROS
309700980706     C                   Z-ADD     30            I
309800980706     C                   Z-ADD     TASIAC        VIMP(I)
309900980706E   1C                   ENDIF
310000980706     C*
310100980706     C                   ENDSR
310200980706     C*------------------------------------------------------------------------*
310300980706     C* CALAGI - CALCOLA L'A.D.G. SULL'INOLTRO
310400980706     C*------------------------------------------------------------------------*
310500980706     C     CALAGI        BEGSR
310600980706     C*
310700980706     C* REIMPOSTA LE VARIABILI DI LAVORO
310800011031     C                   Z-ADD     *ZEROS        TASADG           21 8
310900980706     C*
311000980706     C* CALCOLA L'A.D.G.
311100011031     C                   Z-ADD     TASINL        WIMP             23 8
311200980706     C                   EXSR      ADGCAL
311300980706     C*
311400980706IF  1C     TASADG        IFGT      *ZEROS
311500980706     C                   Z-ADD     21            I
311600980706     C                   Z-ADD     TASADG        VIMP(I)
311700980706E   1C                   ENDIF
311800980706     C*
311900980706     C                   ENDSR
312000980706     C*------------------------------------------------------------------------*
312100980706     C* CALISI - CALCOLA L'ISTAT SULL'INOLTRO
312200980706     C*------------------------------------------------------------------------*
312300980706     C     CALISI        BEGSR
312400980706     C*
312500980706     C* REIMPOSTA LE VARIABILI DI LAVORO
312600011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
312700980706     C*
312800980706     C* CALCOLA L'ISTAT
312900011031     C                   Z-ADD     TASINL        WIMP             23 8
313000980706     C                   EXSR      IACCAL
313100980706     C*
313200980706IF  1C     TASIAC        IFGT      *ZEROS
313300980706     C                   Z-ADD     24            I
313400980706     C                   Z-ADD     TASIAC        VIMP(I)
313500980706E   1C                   ENDIF
313600980706     C*
313700980706     C                   ENDSR
313800980706     C*------------------------------------------------------------------------*
313900980706     C* CALIAI - CALCOLA L'ISTAT SULL'ADG DELL'INOLTRO
314000980706     C*------------------------------------------------------------------------*
314100980706     C     CALIAI        BEGSR
314200980706     C*
314300980706     C* REIMPOSTA LE VARIABILI DI LAVORO
314400011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
314500980706     C*
314600980706     C* CALCOLA L'ISTAT
314700011031     C                   Z-ADD     VIMP(21)      WIMP             23 8
314800980706     C                   EXSR      IACCAL
314900980706     C*
315000980706IF  1C     TASIAC        IFGT      *ZEROS
315100980706     C                   Z-ADD     29            I
315200980706     C                   Z-ADD     TASIAC        VIMP(I)
315300980706E   1C                   ENDIF
315400980706     C*
315500980706     C                   ENDSR
315600970717     C*------------------------------------------------------------------------*
315700970717     C* CALESI - CALCOLA L'ESPRESSO INOLTRO
315800970717     C*------------------------------------------------------------------------*
315900970717     C     CALESI        BEGSR
316000970717     C*
316100970717     C* REIMPOSTA LE VARIABILI DI LAVORO
316200011031     C                   Z-ADD     *ZEROS        TASESP           21 8
316300970717     C*
316400970717     C* CALCOLA L'ESPRESSO
316500011031     C                   Z-ADD     TASINL        IMPON            23 8
316600970717     C                   EXSR      CALTS
316700970717     C*
316800970717IF  1C     TASESP        IFGT      *ZEROS
316900970717     C                   Z-ADD     36            I
317000970717     C                   Z-ADD     TASESP        VIMP(I)
317100970717E   1C                   ENDIF
317200970717     C*
317300970717     C                   ENDSR
317400980706     C*------------------------------------------------------------------------*
317500980706     C* CALAEI - CALCOLA L'A.D.G. SULL'ESPRESSO INOLTRO
317600980706     C*------------------------------------------------------------------------*
317700980706     C     CALAEI        BEGSR
317800980706     C*
317900980706     C* REIMPOSTA LE VARIABILI DI LAVORO
318000011031     C                   Z-ADD     *ZEROS        TASADG           21 8
318100980706     C*
318200980706     C* CALCOLA L'A.D.G.
318300011031     C                   Z-ADD     VIMP(36)      WIMP             23 8
318400980706     C                   EXSR      ADGCAL
318500980706     C*
318600980706IF  1C     TASADG        IFGT      *ZEROS
318700980706     C                   Z-ADD     37            I
318800980706     C                   Z-ADD     TASADG        VIMP(I)
318900980706E   1C                   ENDIF
319000980706     C*
319100980706     C                   ENDSR
319200980706     C*------------------------------------------------------------------------*
319300980706     C* CALIEI - CALCOLA L'ISTAT SULL'ESPRESSO INOLTRO
319400980706     C*------------------------------------------------------------------------*
319500980706     C     CALIEI        BEGSR
319600980706     C*
319700980706     C* REIMPOSTA LE VARIABILI DI LAVORO
319800011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
319900980706     C*
320000980706     C* CALCOLA L'ISTAT
320100011031     C                   Z-ADD     VIMP(36)      WIMP             23 8
320200980706     C                   EXSR      IACCAL
320300980706     C*
320400980706IF  1C     TASIAC        IFGT      *ZEROS
320500980706     C                   Z-ADD     38            I
320600980706     C                   Z-ADD     TASIAC        VIMP(I)
320700980706E   1C                   ENDIF
320800980706     C*
320900980706     C                   ENDSR
321000980706     C*------------------------------------------------------------------------*
321100980706     C* CAIAEI - CALCOLA L'ISTAT SULL'ADG DELL'ESPRESSO INOLTRO
321200980706     C*------------------------------------------------------------------------*
321300980706     C     CAIAEI        BEGSR
321400980706     C*
321500980706     C* REIMPOSTA LE VARIABILI DI LAVORO
321600011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
321700980706     C*
321800980706     C* CALCOLA L'ISTAT
321900011031     C                   Z-ADD     VIMP(37)      WIMP             23 8
322000980706     C                   EXSR      IACCAL
322100980706     C*
322200980706IF  1C     TASIAC        IFGT      *ZEROS
322300980706     C                   Z-ADD     39            I
322400980706     C                   Z-ADD     TASIAC        VIMP(I)
322500980706E   1C                   ENDIF
322600980706     C*
322700980706     C                   ENDSR
322800980706     C*------------------------------------------------------------------------*
322900980706     C* CALCPL - CALCOLA CONSEGNA PARTICOLARE LOCALITA' DISAGIATE
323000980706     C*------------------------------------------------------------------------*
323100980706     C     CALCPL        BEGSR
323200980706     C*
323300980706     C* REIMPOSTA LE VARIABILI DI LAVORO
323400980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
323500011031     C                   Z-ADD     *ZEROS        TASCPL           21 8
323600980706     C*
323700980706     C* CALCOLA CONSEGNA PARTICOLARE
323800980706     C                   MOVEL     'K'           TASTC             1            *CODICE CONSEGNA
323900011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
324000980706     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
324100980706     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
324200980706     C                   EXSR      CALCP
324300980706     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
324400980706     C                   Z-ADD     TASCP         TASCPL
324500980706     C*
324600980706IF  1C     TASCPL        IFGT      *ZEROS
324700980706     C                   Z-ADD     35            I
324800980706     C                   Z-ADD     TASCPL        VIMP(I)
324900980706E   1C                   ENDIF
325000980706     C*
325100980706     C* MEMORIZZA IL FLAG "APPLICAZIONE TARIFFA INOLTRO" DELLO SCAGLIONE DEL
325200980706     C* CODICE TASSAZIONE DEL DETTAGLIO TARIFFARIO
325300980706     C                   MOVEL     DEPAIN        CPLAIN
325400980706     C*
325500980706     C                   ENDSR
325600980706     C*------------------------------------------------------------------------*
325700980706     C* CALAGL - CALCOLA L'A.D.G. SULLE CONSEGNE LOCALITA' DISAGIATE
325800980706     C*------------------------------------------------------------------------*
325900980706     C     CALAGL        BEGSR
326000980706     C*
326100980706     C* REIMPOSTA LE VARIABILI DI LAVORO
326200011031     C                   Z-ADD     *ZEROS        TASADG           21 8
326300980706     C*
326400980706     C* CALCOLA L'A.D.G.
326500011031     C                   Z-ADD     TASCPL        WIMP             23 8
326600980706     C                   EXSR      ADGCAL
326700980706     C*
326800980706IF  1C     TASADG        IFGT      *ZEROS
326900980706     C                   Z-ADD     43            I
327000980706     C                   Z-ADD     TASADG        VIMP(I)
327100980706E   1C                   ENDIF
327200980706     C*
327300980706     C                   ENDSR
327400980706     C*------------------------------------------------------------------------*
327500980706     C* CALISL - CALCOLA L'ISTAT SULLE CONSEGNE LOCALITA' DISAGIATE
327600980706     C*------------------------------------------------------------------------*
327700980706     C     CALISL        BEGSR
327800980706     C*
327900980706     C* REIMPOSTA LE VARIABILI DI LAVORO
328000011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
328100980706     C*
328200980706     C* CALCOLA L'ISTAT
328300011031     C                   Z-ADD     TASCPL        WIMP             23 8
328400980706     C                   EXSR      IACCAL
328500980706     C*
328600980706IF  1C     TASIAC        IFGT      *ZEROS
328700980706     C                   Z-ADD     44            I
328800980706     C                   Z-ADD     TASIAC        VIMP(I)
328900980706E   1C                   ENDIF
329000980706     C*
329100980706     C                   ENDSR
329200980706     C*------------------------------------------------------------------------*
329300980706     C* CALIAL - CALCOLA L'ISTAT SULL'ADG DELLE CONEGNE LOCALITA' DISAGIATE
329400980706     C*------------------------------------------------------------------------*
329500980706     C     CALIAL        BEGSR
329600980706     C*
329700980706     C* REIMPOSTA LE VARIABILI DI LAVORO
329800011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
329900980706     C*
330000980706     C* CALCOLA L'ISTAT
330100011031     C                   Z-ADD     VIMP(43)      WIMP             23 8
330200980706     C                   EXSR      IACCAL
330300980706     C*
330400980706IF  1C     TASIAC        IFGT      *ZEROS
330500980706     C                   Z-ADD     45            I
330600980706     C                   Z-ADD     TASIAC        VIMP(I)
330700980706E   1C                   ENDIF
330800980706     C*
330900980706     C                   ENDSR
331000980706     C*------------------------------------------------------------------------*
331100980706     C* CALESL - CALCOLA L'ESPRESSO SULLA CONSEGNA LOCALITA' DISAGIATE
331200980706     C*------------------------------------------------------------------------*
331300980706     C     CALESL        BEGSR
331400980706     C*
331500980706     C* REIMPOSTA LE VARIABILI DI LAVORO
331600011031     C                   Z-ADD     *ZEROS        TASESP           21 8
331700980706     C*
331800980706     C* CALCOLA L'ESPRESSO
331900011031     C                   Z-ADD     VIMP(35)      IMPON            23 8
332000980706     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
332100980706     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
332200980706     C                   EXSR      CALTS
332300980706     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
332400980706     C*
332500980706IF  1C     TASESP        IFGT      *ZEROS
332600980706     C                   Z-ADD     39            I
332700980706     C                   Z-ADD     TASESP        VIMP(I)
332800980706E   1C                   ENDIF
332900980706     C*
333000980706     C                   ENDSR
333100980706     C*------------------------------------------------------------------------*
333200980706     C* CALAEL - CALCOLA L'A.D.G. SULL'ESPRESSO CONSEGNA LOCALITA' DISAGIATE
333300980706     C*------------------------------------------------------------------------*
333400980706     C     CALAEL        BEGSR
333500980706     C*
333600980706     C* REIMPOSTA LE VARIABILI DI LAVORO
333700011031     C                   Z-ADD     *ZEROS        TASADG           21 8
333800980706     C*
333900980706     C* CALCOLA L'A.D.G.
334000011031     C                   Z-ADD     VIMP(39)      WIMP             23 8
334100980706     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
334200980706     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
334300980706     C                   EXSR      ADGCAL
334400980706     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
334500980706     C*
334600980706IF  1C     TASADG        IFGT      *ZEROS
334700980706     C                   Z-ADD     40            I
334800980706     C                   Z-ADD     TASADG        VIMP(I)
334900980706E   1C                   ENDIF
335000980706     C*
335100980706     C                   ENDSR
335200980706     C*------------------------------------------------------------------------*
335300980706     C* CALIEL - CALCOLA L'ISTAT SULL'ESPRESSO CONSEGNA LOCALITA' DISAGIATE
335400980706     C*------------------------------------------------------------------------*
335500980706     C     CALIEL        BEGSR
335600980706     C*
335700980706     C* REIMPOSTA LE VARIABILI DI LAVORO
335800011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
335900980706     C*
336000980706     C* CALCOLA L'ISTAT
336100011031     C                   Z-ADD     VIMP(39)      WIMP             23 8
336200980706     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
336300980706     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
336400980706     C                   EXSR      IACCAL
336500980706     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
336600980706     C*
336700980706IF  1C     TASIAC        IFGT      *ZEROS
336800980706     C                   Z-ADD     41            I
336900980706     C                   Z-ADD     TASIAC        VIMP(I)
337000980706E   1C                   ENDIF
337100980706     C*
337200980706     C                   ENDSR
337300980706     C*------------------------------------------------------------------------*
337400980706     C* CAIAEL - CALCOLA L'ISTAT SULL'ADG DELL'ESPRESSO CONSEGNA L.DISAGIATE
337500980706     C*------------------------------------------------------------------------*
337600980706     C     CAIAEL        BEGSR
337700980706     C*
337800980706     C* REIMPOSTA LE VARIABILI DI LAVORO
337900011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
338000980706     C*
338100980706     C* CALCOLA L'ISTAT
338200011031     C                   Z-ADD     VIMP(40)      WIMP             23 8
338300980706     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
338400980706     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
338500980706     C                   EXSR      IACCAL
338600980706     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
338700980706     C*
338800980706IF  1C     TASIAC        IFGT      *ZEROS
338900980706     C                   Z-ADD     42            I
339000980706     C                   Z-ADD     TASIAC        VIMP(I)
339100980706E   1C                   ENDIF
339200980706     C*
339300980706     C                   ENDSR
339400011031     C*------------------------------------------------------------------------*
339500011031     C* CALSAM - CALCOLA RITIRO ISOLA
339600011031     C*------------------------------------------------------------------------*
339700011031     C     CALSAM        BEGSR
339800011031     C*
339900011031     C* REIMPOSTA LE VARIABILI DI LAVORO
340000011031     C                   MOVEL     'S'           WCAR                           *SI UTILIZZO TAR.CAR
340100011031     C                   Z-ADD     *ZEROS        TASSAM           21 8
340200011031     C*
340300011031     C* CALCOLA RITIRO COME CONSEGNA PARTICOLARE ISOLA
340400011031     C                   MOVEL     'I'           TASTC             1            *CODICE CONSEGNA
340500011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
340600011031     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
340700011031     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
340800011031     C                   EXSR      CALCP
340900011031     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
341000050103     C                   Z-ADD     TASCP         TASSAM
341100011031     C*
341200011031IF  1C     TASSAM        IFGT      *ZEROS
341300011031     C                   Z-ADD     88            I
341400011031     C                   Z-ADD     TASSAM        VIMP(I)
341500011031E   1C                   ENDIF
341600011031     C*
341700011031     C* MEMORIZZA IL FLAG "APPLICAZIONE TARIFFA INOLTRO" DELLO SCAGLIONE DEL
341800011031     C* CODICE TASSAZIONE DEL DETTAGLIO TARIFFARIO -NON SERVE, MA TENIAMOLO-
341900011031     C                   MOVEL     DEPAIN        SAMAIN
342000011031     C*
342100011031     C                   ENDSR
342200011031     C*------------------------------------------------------------------------*
342300011031     C* CALAGR - CALCOLA L'A.D.G. SUL RITIRO ISOLA
342400011031     C*------------------------------------------------------------------------*
342500011031     C     CALAGR        BEGSR
342600011031     C*
342700011031     C* REIMPOSTA LE VARIABILI DI LAVORO
342800011031     C                   Z-ADD     *ZEROS        TASADG           21 8
342900011031     C*
343000011031     C* CALCOLA L'A.D.G.
343100011031     C                   Z-ADD     TASSAM        WIMP             23 8
343200011031     C                   EXSR      ADGCAL
343300011031     C*
343400011031IF  1C     TASADG        IFGT      *ZEROS
343500011031     C                   Z-ADD     93            I
343600011031     C                   Z-ADD     TASADG        VIMP(I)
343700011031E   1C                   ENDIF
343800011031     C*
343900011031     C                   ENDSR
344000011031     C*------------------------------------------------------------------------*
344100011031     C* CALISR - CALCOLA L'ISTAT SUL RITIRO ISOLA
344200011031     C*------------------------------------------------------------------------*
344300011031     C     CALISR        BEGSR
344400011031     C*
344500011031     C* REIMPOSTA LE VARIABILI DI LAVORO
344600011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
344700011031     C*
344800011031     C* CALCOLA L'ISTAT
344900011031     C                   Z-ADD     TASSAM        WIMP             23 8
345000011031     C                   EXSR      IACCAL
345100011031     C*
345200011031IF  1C     TASIAC        IFGT      *ZEROS
345300011031     C                   Z-ADD     94            I
345400011031     C                   Z-ADD     TASIAC        VIMP(I)
345500011031E   1C                   ENDIF
345600011031     C*
345700011031     C                   ENDSR
345800011031     C*------------------------------------------------------------------------*
345900011031     C* CALIAR - CALCOLA L'ISTAT SULL'ADG DEL RITIRO ISOLA
346000011031     C*------------------------------------------------------------------------*
346100011031     C     CALIAR        BEGSR
346200011031     C*
346300011031     C* REIMPOSTA LE VARIABILI DI LAVORO
346400011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
346500011031     C*
346600011031     C* CALCOLA L'ISTAT
346700011031     C                   Z-ADD     VIMP(93)      WIMP             23 8
346800011031     C                   EXSR      IACCAL
346900011031     C*
347000011031IF  1C     TASIAC        IFGT      *ZEROS
347100011031     C                   Z-ADD     95            I
347200011031     C                   Z-ADD     TASIAC        VIMP(I)
347300011031E   1C                   ENDIF
347400011031     C*
347500011031     C                   ENDSR
347600011031     C*------------------------------------------------------------------------*
347700011031     C* CALESR - CALCOLA L'ESPRESSO SUL RITIRO ISOLA
347800011031     C*------------------------------------------------------------------------*
347900011031     C     CALESR        BEGSR
348000011031     C*
348100011031     C* REIMPOSTA LE VARIABILI DI LAVORO
348200011031     C                   Z-ADD     *ZEROS        TASESP           21 8
348300011031     C*
348400011031     C* CALCOLA L'ESPRESSO
348500011031     C                   Z-ADD     VIMP(88)      IMPON            23 8
348600011031     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
348700011031     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
348800011031     C                   EXSR      CALTS
348900011031     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
349000011031     C*
349100011031IF  1C     TASESP        IFGT      *ZEROS
349200011031     C                   Z-ADD     89            I
349300011031     C                   Z-ADD     TASESP        VIMP(I)
349400011031E   1C                   ENDIF
349500011031     C*
349600011031     C                   ENDSR
349700011031     C*------------------------------------------------------------------------*
349800011031     C* CALAER - CALCOLA L'A.D.G. SULL'ESPRESSO RITIRO ISOLA
349900011031     C*------------------------------------------------------------------------*
350000011031     C     CALAER        BEGSR
350100011031     C*
350200011031     C* REIMPOSTA LE VARIABILI DI LAVORO
350300011031     C                   Z-ADD     *ZEROS        TASADG           21 8
350400011031     C*
350500011031     C* CALCOLA L'A.D.G.
350600011031     C                   Z-ADD     VIMP(89)      WIMP             23 8
350700011031     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
350800011031     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
350900011031     C                   EXSR      ADGCAL
351000011031     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
351100011031     C*
351200011031IF  1C     TASADG        IFGT      *ZEROS
351300011031     C                   Z-ADD     90            I
351400011031     C                   Z-ADD     TASADG        VIMP(I)
351500011031E   1C                   ENDIF
351600011031     C*
351700011031     C                   ENDSR
351800011031     C*------------------------------------------------------------------------*
351900011031     C* CALIER - CALCOLA L'ISTAT SULL'ESPRESSO RITIRO ISOLA
352000011031     C*------------------------------------------------------------------------*
352100011031     C     CALIER        BEGSR
352200011031     C*
352300011031     C* REIMPOSTA LE VARIABILI DI LAVORO
352400011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
352500011031     C*
352600011031     C* CALCOLA L'ISTAT
352700011031     C                   Z-ADD     VIMP(89)      WIMP             23 8
352800011031     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
352900011031     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
353000011031     C                   EXSR      IACCAL
353100011031     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
353200011031     C*
353300011031IF  1C     TASIAC        IFGT      *ZEROS
353400011031     C                   Z-ADD     91            I
353500011031     C                   Z-ADD     TASIAC        VIMP(I)
353600011031E   1C                   ENDIF
353700011031     C*
353800011031     C                   ENDSR
353900011031     C*------------------------------------------------------------------------*
354000011031     C* CAIAER - CALCOLA L'ISTAT SULL'ADG DELL'ESPRESSO RITIRO ISOLA
354100011031     C*------------------------------------------------------------------------*
354200011031     C     CAIAER        BEGSR
354300011031     C*
354400011031     C* REIMPOSTA LE VARIABILI DI LAVORO
354500011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
354600011031     C*
354700011031     C* CALCOLA L'ISTAT
354800011031     C                   Z-ADD     VIMP(90)      WIMP             23 8
354900011031     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
355000011031     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
355100011031     C                   EXSR      IACCAL
355200011031     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
355300011031     C*
355400011031IF  1C     TASIAC        IFGT      *ZEROS
355500011031     C                   Z-ADD     92            I
355600011031     C                   Z-ADD     TASIAC        VIMP(I)
355700011031E   1C                   ENDIF
355800011031     C*
355900011031     C                   ENDSR
356000011030     C*------------------------------------------------------------------------*
356100011030     C* CALCIS - CALCOLA CONSEGNA PARTICOLARE ISOLA
356200011030     C*------------------------------------------------------------------------*
356300011030     C     CALCIS        BEGSR
356400011030     C*
356500011030     C* REIMPOSTA LE VARIABILI DI LAVORO
356600011030     C                   MOVEL     'S'           WCAR                           *SI UTILIZZO TAR.CAR
356700011031     C                   Z-ADD     *ZEROS        TASCIS           21 8
356800011030     C*
356900011030     C* CALCOLA CONSEGNA PARTICOLARE
357000011030     C                   MOVEL     'I'           TASTC             1            *CODICE CONSEGNA
357100011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
357200011030     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
357300011030     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
357400011030     C                   EXSR      CALCP
357500011030     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
357600011030     C                   Z-ADD     TASCP         TASCIS
357700011030     C*
357800011030IF  1C     TASCIS        IFGT      *ZEROS
357900011030     C                   Z-ADD     80            I
358000011030     C                   Z-ADD     TASCIS        VIMP(I)
358100011030E   1C                   ENDIF
358200011030     C*
358300011030     C* MEMORIZZA IL FLAG "APPLICAZIONE TARIFFA INOLTRO" DELLO SCAGLIONE DEL
358400011030     C* CODICE TASSAZIONE DEL DETTAGLIO TARIFFARIO
358500011030     C                   MOVEL     DEPAIN        CISAIN
358600011030     C*
358700011030     C                   ENDSR
358800011030     C*------------------------------------------------------------------------*
358900011030     C* CALAGS - CALCOLA L'A.D.G. SULLE CONSEGNE ISOLA
359000011030     C*------------------------------------------------------------------------*
359100011030     C     CALAGS        BEGSR
359200011030     C*
359300011030     C* REIMPOSTA LE VARIABILI DI LAVORO
359400011031     C                   Z-ADD     *ZEROS        TASADG           21 8
359500011030     C*
359600011030     C* CALCOLA L'A.D.G.
359700011031     C                   Z-ADD     TASCIS        WIMP             23 8
359800011030     C                   EXSR      ADGCAL
359900011030     C*
360000011030IF  1C     TASADG        IFGT      *ZEROS
360100011030     C                   Z-ADD     85            I
360200011030     C                   Z-ADD     TASADG        VIMP(I)
360300011030E   1C                   ENDIF
360400011030     C*
360500011030     C                   ENDSR
360600011030     C*------------------------------------------------------------------------*
360700011030     C* CALISS - CALCOLA L'ISTAT SULLE CONSEGNE ISOLA
360800011030     C*------------------------------------------------------------------------*
360900011030     C     CALISS        BEGSR
361000011030     C*
361100011030     C* REIMPOSTA LE VARIABILI DI LAVORO
361200011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
361300011030     C*
361400011030     C* CALCOLA L'ISTAT
361500011031     C                   Z-ADD     TASCIS        WIMP             23 8
361600011030     C                   EXSR      IACCAL
361700011030     C*
361800011030IF  1C     TASIAC        IFGT      *ZEROS
361900011030     C                   Z-ADD     86            I
362000011030     C                   Z-ADD     TASIAC        VIMP(I)
362100011030E   1C                   ENDIF
362200011030     C*
362300011030     C                   ENDSR
362400011030     C*------------------------------------------------------------------------*
362500011030     C* CALIAS - CALCOLA L'ISTAT SULL'ADG DELLE CONEGNE ISOLA
362600011030     C*------------------------------------------------------------------------*
362700011030     C     CALIAS        BEGSR
362800011030     C*
362900011030     C* REIMPOSTA LE VARIABILI DI LAVORO
363000011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
363100011030     C*
363200011030     C* CALCOLA L'ISTAT
363300011031     C                   Z-ADD     VIMP(85)      WIMP             23 8
363400011030     C                   EXSR      IACCAL
363500011030     C*
363600011030IF  1C     TASIAC        IFGT      *ZEROS
363700011030     C                   Z-ADD     87            I
363800011030     C                   Z-ADD     TASIAC        VIMP(I)
363900011030E   1C                   ENDIF
364000011030     C*
364100011030     C                   ENDSR
364200011030     C*------------------------------------------------------------------------*
364300011030     C* CALESS - CALCOLA L'ESPRESSO SULLA CONSEGNA ISOLA
364400011030     C*------------------------------------------------------------------------*
364500011030     C     CALESS        BEGSR
364600011030     C*
364700011030     C* REIMPOSTA LE VARIABILI DI LAVORO
364800011031     C                   Z-ADD     *ZEROS        TASESP           21 8
364900011030     C*
365000011030     C* CALCOLA L'ESPRESSO
365100011031     C                   Z-ADD     VIMP(80)      IMPON            23 8
365200011030     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
365300011030     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
365400011030     C                   EXSR      CALTS
365500011030     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
365600011030     C*
365700011030IF  1C     TASESP        IFGT      *ZEROS
365800011030     C                   Z-ADD     81            I
365900011030     C                   Z-ADD     TASESP        VIMP(I)
366000011030E   1C                   ENDIF
366100011030     C*
366200011030     C                   ENDSR
366300011030     C*------------------------------------------------------------------------*
366400011030     C* CALAES - CALCOLA L'A.D.G. SULL'ESPRESSO CONSEGNA ISOLA
366500011030     C*------------------------------------------------------------------------*
366600011030     C     CALAES        BEGSR
366700011030     C*
366800011030     C* REIMPOSTA LE VARIABILI DI LAVORO
366900011031     C                   Z-ADD     *ZEROS        TASADG           21 8
367000011030     C*
367100011030     C* CALCOLA L'A.D.G.
367200011031     C                   Z-ADD     VIMP(81)      WIMP             23 8
367300011030     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
367400011030     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
367500011030     C                   EXSR      ADGCAL
367600011030     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
367700011030     C*
367800011030IF  1C     TASADG        IFGT      *ZEROS
367900011030     C                   Z-ADD     82            I
368000011030     C                   Z-ADD     TASADG        VIMP(I)
368100011030E   1C                   ENDIF
368200011030     C*
368300011030     C                   ENDSR
368400011030     C*------------------------------------------------------------------------*
368500011030     C* CALIEL - CALCOLA L'ISTAT SULL'ESPRESSO CONSEGNA ISOLA
368600011030     C*------------------------------------------------------------------------*
368700011030     C     CALIES        BEGSR
368800011030     C*
368900011030     C* REIMPOSTA LE VARIABILI DI LAVORO
369000011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
369100011030     C*
369200011030     C* CALCOLA L'ISTAT
369300011031     C                   Z-ADD     VIMP(81)      WIMP             23 8
369400011030     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
369500011030     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
369600011030     C                   EXSR      IACCAL
369700011030     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
369800011030     C*
369900011030IF  1C     TASIAC        IFGT      *ZEROS
370000011030     C                   Z-ADD     83            I
370100011030     C                   Z-ADD     TASIAC        VIMP(I)
370200011030E   1C                   ENDIF
370300011030     C*
370400011030     C                   ENDSR
370500011030     C*------------------------------------------------------------------------*
370600011030     C* CAIAES - CALCOLA L'ISTAT SULL'ADG DELL'ESPRESSO CONSEGNA ISOLA
370700011030     C*------------------------------------------------------------------------*
370800011030     C     CAIAES        BEGSR
370900011030     C*
371000011030     C* REIMPOSTA LE VARIABILI DI LAVORO
371100011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
371200011030     C*
371300011030     C* CALCOLA L'ISTAT
371400011031     C                   Z-ADD     VIMP(82)      WIMP             23 8
371500011030     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
371600011030     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
371700011030     C                   EXSR      IACCAL
371800011030     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
371900011030     C*
372000011030IF  1C     TASIAC        IFGT      *ZEROS
372100011030     C                   Z-ADD     84            I
372200011030     C                   Z-ADD     TASIAC        VIMP(I)
372300011030E   1C                   ENDIF
372400011030     C*
372500011030     C                   ENDSR
372600980706     C*------------------------------------------------------------------------*
372700011030     C* CA3INL - RICALCOLA INOLTRO X LOCALITA' DISAGIATE / CONSEGNA ISOLA
372800980706     C*------------------------------------------------------------------------*
372900980706     C     CA3INL        BEGSR
373000980706     C*---
373100011030     C* FLAG='S' -> PAGA TARIFFA INOLTRO + TARIFFA LOCALITA' DISAGIATA  + ISOLA
373200011030     C* FLAG=' ' -> CANCELLA L'INOLTRO SE LA L.DISAGIATA o ISOLA E' DA PAGARE
373300011030     C*
373400011030     C* NOTE: In tassazione una bolla può essere O Località disagiata O Consegna isola.
373500011030     C*       Nel CAT l'azzeramento dell'inoltro scatta anche se SOLO UNA delle due
373600011030     C*       consegne prevede l'azzeramento e quindi può esistere l'incongruenza
373700011030     C*       che si abbia l'inoltro azzerato a fronte del fatto che UNA delle due consegne
373800011030     C*       non preveda l'azzeramento. (Mha!, avrete capito???)
373900011030     C*---
374000011030     c***
374100011030     c* LOCALITA' DISAGIATE / ISOLA
374200011030     c***
374300011030IF  1C     CPLAIN        IFEQ      *BLANKS
374400060607IF  2C     VC(35)        andeq     'S'                                          *VOCE DA CALCOLARE
374500011030     C     CISAIN        OREQ      *BLANKS
374600060607IF  2C     VC(80)        andeq     'S'                                          *VOCE DA CALCOLARE
374700980706     C*
374800980706     C* AZZERA LE VOCI INOLTRO
374900011030IF  2C     VIMP(35)      IFGT      *ZEROS                                       *L.DISAG.DA PAGARE
375000011030     C     VIMP(80)      ORGT      *ZEROS                                       *C.ISOLA DA PAGARE
375100980706DO  3C     1             DO        NVOCA         V                 6 0
375200980706IF  4C     VIMP(V)       IFGT      *ZEROS
375300980706IF  5C     V             IFEQ      006
375400980706     C     V             OREQ      021
375500980706     C     V             OREQ      024
375600980706     C     V             OREQ      029
375700980706     C     V             OREQ      034
375800980706     C     V             OREQ      036
375900980706     C     V             OREQ      037
376000980706     C     V             OREQ      038
376100980706     C                   Z-ADD     *ZEROS        VIMP(V)
376200980706E   5C                   ENDIF
376300980706E   4C                   ENDIF
376400980706E   3C                   ENDDO
376500980706E   2C                   ENDIF
376600980706E   1C                   ENDIF
376700980706     C*
376800980706     C                   ENDSR
376900980706     C*------------------------------------------------------------------------*
377000980706     C* CALTBT - CALCOLA LA BASE PER IL CALCOLO DELLE TARIFFE CON % SUL TRASPORTO
377100980706     C*------------------------------------------------------------------------*
377200980706     C     CALTBT        BEGSR
377300980706     C*
377400980706     C* REIMPOSTA LE VARIABILI DI LAVORO
377500011031     C                   Z-ADD     *ZEROS        TOTTRA           23 8
377600980706     C*
377700011030     C* PORTO + INOLTRO + LOCALITA' DISAGIATE + ISOLA + LORO ESPRESSI
377800980706     C     VIMP(1)       ADD       TOTTRA        TOTTRA                         *PORTO
377900980706     C     VIMP(6)       ADD       TOTTRA        TOTTRA                         *INOLTRO
378000980706     C     VIMP(35)      ADD       TOTTRA        TOTTRA                         *LOCALITA' DISAGIATE
378100980706     C     VIMP(7)       ADD       TOTTRA        TOTTRA                         *ESPRESSO PORTO
378200980706     C     VIMP(36)      ADD       TOTTRA        TOTTRA                         *ESPRESSO INOLTRO
378300980706     C     VIMP(39)      ADD       TOTTRA        TOTTRA                         *ESPRESSO L.DISAGIAT
378400011030     C     VIMP(80)      ADD       TOTTRA        TOTTRA                         *CONSEGNA ISOLA
378500011030     C     VIMP(81)      ADD       TOTTRA        TOTTRA                         *ESPRESSO C.ISOLA
378600980706     C*
378700980706     C                   ENDSR
378800970306     C*------------------------------------------------------------------------*
378900970306     C* CALTS  - CALCOLA TIPO SERVIZIO ESPRESSO
379000970306     C*------------------------------------------------------------------------*
379100980706     C     CALTS         BEGSR
379200970306     C*
379300980506     C* REIMPOSTA LE VARIABILI DI LAVORO
379400980506     C                   MOVEL     'S'           WCAR              1            *SI UTILIZZO TAR.CAR
379500020912     C                   MOVEL     'E'           WTSP              1              ESPRESSO='E'
379600980506     C*
379700970306     C* REPERISCE IL CODICE DELLA PARTICOLARITA' ESPRESSO
379800970306     C                   SETOFF                                           28
379900970306     C                   Z-ADD     1             I
380000980506     C     WTSP          LOOKUP    TS(I)                                  28
380100970306IF  1C     *IN28         IFEQ      *ON                                          *SERVIZIO ESISTENTE
380200980506     C     I             OCCUR     DS5E
380300980506IF  2C     §5EFTC        IFNE      *BLANKS                                      *SERVIZIO CON PARTIC
380400980506IF  3C     TAMTSP        IFEQ      WTSP
380500980804     C                   MOVEL     'N'           WCAR                           *NO UTILIZZO TAR.CAR
380600980506E   3C                   ENDIF
380700980506     C*
380800980506     C* CERCA LA CONSEGNA PARTICOLARE
380900980506     C                   MOVEL     §5EFTC        TASTC
381000980506     C                   EXSR      CERTPT
381100980506     C*
381200980506     C* CALCOLA RPV, ARROTONDAMENTI E PESO TASSABILE
381300980506IF  3C     $TPT          IFEQ      '1'                                          *TARIFFA TROVATA
381400980707     C                   MOVEL     TPTTPG        WTPG
381500980707     C                   MOVEL     TPTRPV        WRPV
381600980707     C                   CLEAR                   WDET
381700980707     C                   Z-ADD     TPTARL        WARL
381800980707     C                   Z-ADD     TPTARF        WARF
381900980707     C                   Z-ADD     TPTARO        WARO
382000980506     C                   EXSR      CALCOL
382100980506IF  4C     TPTTPG        IFEQ      'T'                                          *A TRASPORTO
382200980506     C                   Z-ADD     IMPON         ISOPES                         *PORTO
382300091026     c                   eval      tpg = 'T'
382400980506E   4C                   ENDIF
382500970306     C*
382600970306     C* CALCOLA LA TARIFFA ESPRESSO
382700980506     C                   Z-ADD     ISOPES        TPDPES                         *PESO
382800970306     C                   EXSR      CALACD
382900970306     C                   Z-ADD     VARIA         TASESP
383000970306E   3C                   ENDIF
383100970306E   2C                   ENDIF
383200970306E   1C                   ENDIF
383300970306     C*
383400970306     C                   ENDSR
383500970306     C*------------------------------------------------------------------------*
383600970306     C* CALAGD - CALCOLA L'A.D.G. SUL DIRITTO FISSO
383700970306     C*------------------------------------------------------------------------*
383800970306     C     CALAGD        BEGSR
383900970306     C*
384000970306     C* REIMPOSTA LE VARIABILI DI LAVORO
384100011031     C                   Z-ADD     *ZEROS        TASADG           21 8
384200970306     C*
384300970306     C* CALCOLA L'A.D.G.
384400011031     C                   Z-ADD     TASDIF        WIMP             23 8
384500970306     C                   EXSR      ADGCAL
384600970306     C*
384700970314IF  1C     TASADG        IFGT      *ZEROS
384800970306     C                   Z-ADD     22            I
384900970306     C                   Z-ADD     TASADG        VIMP(I)
385000970314E   1C                   ENDIF
385100970306     C*
385200970306     C                   ENDSR
385300970306     C*------------------------------------------------------------------------*
385400970306     C* ADGCAL - CALCOLA A.D.G. SU IMPORTO GENERICO
385500970306     C*------------------------------------------------------------------------*
385600970306     C     ADGCAL        BEGSR
385700041213     C* anni precedenti
385800970306IF  1C     TAMPAG        IFNE      *ZEROS                                       *INDICATA IN TARIFFA
385900011031     C     WIMP          MULT      TAMPAG        COMADG           23 8
386000970306IF  2C     COMADG        IFGT      *ZEROS
386100970306     C     COMADG        DIV(H)    100           TASADG
386200970306E   2C                   ENDIF
386300970306E   1C                   ENDIF
386400041213     c                   z-add     tasadg        tasadgs
386500041213     C* anno in corso
386600041213IF  1C     TAMPpa        IFNE      *ZEROS                                       *INDICATA IN TARIFFA
386700041213     c                   eval      comadg = (wimp + tasadg)*tamppa
386800041213IF  2C     COMADG        IFGT      *ZEROS
386900041213     C     COMADG        DIV(H)    100           TASADG
387000041213E   2C                   ENDIF
387100041213     c                   add       tasadgs       tasadg
387200041213E   1C                   ENDIF
387300970306     C*
387400970306     C                   ENDSR
387500980706     C*------------------------------------------------------------------------*
387600980706     C* IACCAL - CALCOLA ISTAT SU IMPORTO GENERICO
387700980706     C*------------------------------------------------------------------------*
387800980706     C     IACCAL        BEGSR
387900980706     C*
388000980706     C* REIMPOSTA LE VARIABILI DI LAVORO
388100101018     C                   Z-ADD     *ZEROS        PUNIAC            7 4
388200101018     C                   Z-ADD     *ZEROS        PERIAC           23 8
388300101018     C                   Z-ADD     *ZEROS        PERIAX           23 8
388400980706     C*
388500061018      * CALCOLO LA PERCENTUALE DI BASE DI CALCOLO
388600061018      * SE ESISTE GIÀ UNA PERCENTUALE CALCOLO LA PERENTUALE DELLA PERCENTYUALE E POI SOMMO TUTTO
388700061018      * ESEMPIO-....
388800061018      * 1° PUNTO PERCENTUALE ISTAT  2%
388900061018      * 2° PUNTO PERCENTUALE ISTAT  5%
389000061018      * PERCENTUALE ISTAT = 2 + (IL 5% DI 2) + 5 = 7,1
389100101018     c                   clear                   trulistds
389200101018     c                   eval      IISTsca = tamrct
389300101018     c                   eval      IISTdsp = d00dtc
389400101018     c                   call      'TRULISTR'
389500101018     c                   parm                    trulistds
389600101018     c                   IF        OISTerr = *blanks
389700101018     c                   eval      puniac = OISTiac
389800101018     c                   ENDIF
389900980706     C* CALCOLA L'ISTAT
390000980706     C     PUNIAC        MULT      TAMPRI        PERIAX
390100980706IF  2C     PERIAX        IFNE      *ZEROS
390200980706     C     PERIAX        DIV       100           PERIAC
390300980706E   2C                   ENDIF
390400980706IF  2C     PERIAC        IFNE      *ZEROS
390500061018     C                   DIV       100           PERIAC
390600980706     C     WIMP          MULT(H)   PERIAC        TASIAC
390700980706E   2C                   ENDIF
390800980706     C*
390900980706     C                   ENDSR
391000970306     C*------------------------------------------------------------------------*
391100060314     C* CALASC - CALCOLA ASSICURAZIONE ac plusTO
391200970306     C*------------------------------------------------------------------------*
391300970306     C     CALASC        BEGSR
391400970306     C*
391500970306     C* REIMPOSTA LE VARIABILI DI LAVORO
391600011031     C                   Z-ADD     *ZEROS        TASASF           21 8
391700011031     C                   Z-ADD     *ZEROS        TASASA           21 8
391800980506     C*---
391900980506     C* FRANCO
392000980506     C*---
392100980506     C                   MOVEL     'F'           WTPO
392200970306     C                   EXSR      CALVR
392300970306IF  1C     VC(9)         IFEQ      'S'                                          *VOCE DA CALCOLARE
392400970314     C     TASASF        ANDGT     *ZEROS
392500970306     C                   Z-ADD     9             I
392600970306     C                   Z-ADD     TASASF        VIMP(I)
392700970306E   1C                   ENDIF
392800980506     C*---
392900980506     C* ASSEGNATO
393000980506     C*---
393100980506     C                   MOVEL     'A'           WTPO
393200980506     C                   EXSR      CALVR
393300970306IF  1C     VC(31)        IFEQ      'S'                                          *VOCE DA CALCOLARE
393400970314     C     TASASA        ANDGT     *ZEROS
393500970306     C                   Z-ADD     31            I
393600970306     C                   Z-ADD     TASASA        VIMP(I)
393700970306E   1C                   ENDIF
393800970306     C*
393900970306     C                   ENDSR
394000060314     C*------------------------------------------------------------------------*
394100060314     C* CALAcb - CALCOLA ASSICURAZIONE ac base
394200060314     C*------------------------------------------------------------------------*
394300060315     C     CALAcb        BEGSR
394400060314     C*
394500060314     C* REIMPOSTA LE VARIABILI DI LAVORO
394600060314     C                   Z-ADD     *ZEROS        TASASF           21 8
394700060314     C                   Z-ADD     *ZEROS        TASASA           21 8
394800060314     C*---
394900060314     C* FRANCO
395000060314     C*---
395100060314     C                   MOVEL     'F'           WTPO
395200060314     C                   EXSR      CALVR
395300060314IF  1C     VC(105)       IFEQ      'S'                                          *VOCE DA CALCOLARE
395400060314     C     TASASF        ANDGT     *ZEROS
395500060314     C                   Z-ADD     105           I
395600060314     C                   Z-ADD     TASASF        VIMP(I)
395700060314E   1C                   ENDIF
395800060314     C*---
395900060314     C* ASSEGNATO
396000060314     C*---
396100060314     C                   MOVEL     'A'           WTPO
396200060314     C                   EXSR      CALVR
396300060314IF  1C     VC(108)       IFEQ      'S'                                          *VOCE DA CALCOLARE
396400060314     C     TASASA        ANDGT     *ZEROS
396500060314     C                   Z-ADD     108           I
396600060314     C                   Z-ADD     TASASA        VIMP(I)
396700060314E   1C                   ENDIF
396800060314     C*
396900060314     C                   ENDSR
397000970306     C*------------------------------------------------------------------------*
397100011023     C* CALVR - CALCOLA CONSEGNA PARTICOLARE ASSICURAZIONE PER CONTO
397200970306     C*------------------------------------------------------------------------*
397300970306     C     CALVR         BEGSR
397400970306     C*
397500980506     C* NB: L'IMPORTO DA ASSICURARE NEI DATI TIPO E' OBBLIGATORIO: TOGLIENDO LA
397600980506     C*     OBBLIGATORIETA' E' NECESSARIO GESTIRE IL MANCA TARIFFA PER MANDATI
397700980702     C*     A VALORE VARIABILE CON IMPORTO UGUALE A ZERO.
397800970306     C*
397900970306     C* SE CLIENTE 88 CALCOLA TARIFFA SE ESISTE IL VALORE MERCE NEI DATI TIPO
398000970306     C                   MOVEL     D00KSC        WA4               4 0
398100970306IF  1C     WA4           IFEQ      8888
398200970418     C     ETCASC        ANDGT     *ZEROS
398300970306     C     WA4           ORNE      8888
398400970306     C                   Z-ADD     1             I
398500970306     C                   SETOFF                                           28
398600990819     C                   MOVEL     KTPFTC        A1
398700990819     C     A1            LOOKUP    CP(I)                                  28
398800970306IF  2C     *IN28         IFEQ      *ON                                          *C.PART.ASS.ESISTENT
398900970306     C     I             OCCUR     CPIDS                                        *TARIFFA DI CARTELLO
399000970306     C*
399100970306     C* RICERCA L'ASSICURAZIONE PER CONTO NELLE CONSEGNE PARTICOLARI DEL CLIENTE
399200970306     C                   Z-ADD     D00KSC        KTPKSC
399300970306     C                   Z-ADD     D00CTR        KTPCTR
399400970306     C                   Z-ADD     D00PRG        KTPPRG
399500011019     C                   MOVEL     D00DIV        WDIV
399600970306     C                   MOVEL     '0'           $TPT                           *TARIFFA NON TROVATA
399700990819     C     KEYTPT        SETLL     TITPT01L                               99
399800990819     C     KEYTPT        READE     TITPT01L                               99
399900970306DO  3C     *IN99         DOWEQ     *OFF
400000991125     C     $TPT          ANDEQ     '0'
400100991125     C*
400200991125     C* CONTROLLA SE CONVERTIRE I CAMPI DI TITPT
400300011026IF  4C     WDIV          IFNE      WDIVCE
400400011019     C                   MOVEL     WDIV          WDVI
400500991125     C                   EXSR      CNVTPT
400600991125E   4C                   ENDIF
400700991125     C*
400800970306IF  4C     TPTATB        IFEQ      *BLANKS
400900980506IF  5C     TPTAPL        IFEQ      *BLANKS                                      *APPLICAZIONE ENTRAM
401000980506     C     TPTAPL        OREQ      'A'                                          *APPLICAZIONE ARRIVO
401100980506     C     WTPO          ANDEQ     'A'                                          *ASSEGNATI O
401200980506     C     TPTAPL        OREQ      'P'                                          *APPLICAZIONE PARTEN
401300980506     C     WTPO          ANDEQ     'F'                                          *FRANCHI
401400970306     C                   MOVEL     '1'           $TPT                           *TARIFFA TROVATA
401500970314     C*
401600970314     C* USA IL VALORE MERCE IN TARIFFA SE C'E' ALTRIMENTI QUELLO DEI DATI TIPO
401700980506IF  6C     TPTVLM        IFGT      *ZEROS                                       *VALORE IN TARIFFA
401800980506IF  7C     TPTFVM        IFEQ      '1'                                          *TARIFFA A COLLI
401900980506     C     SCSC(S)       MULT      TPTVLM        IASLIT
402000980506E   7C                   ENDIF
402100980506IF  7C     TPTFVM        IFEQ      '2'                                          *TARIFFA A SPEDIZION
402200980506     C                   Z-ADD     TPTVLM        IASLIT
402300980506E   7C                   ENDIF
402400980506IF  7C     TPTFVM        IFEQ      '3'                                          *TARIFFA A PESO
402500980506     C     SPSC(S)       MULT      TPTVLM        IASLIT
402600980506E   7C                   ENDIF
402700980506X   6C                   ELSE                                                   *NO VALORE IN TARIFF
402800970314     C                   MOVEL     '2'           TPTFVM                         *TARIFFA A SPEDIZION
402900980506E   6C                   ENDIF
403000980506E   5C                   ENDIF
403100980506E   4C                   ENDIF
403200970306     C*
403300970306     C* LETTURA SUCESSIVA TARIFFE
403400970306IF  4C     $TPT          IFEQ      '0'                                          *TARIFFA NON TROVATA
403500990819     C     KEYTPT        READE     TITPT01L                               99
403600970306E   4C                   ENDIF
403700970306E   3C                   ENDDO                                                  *FINE TAR.PART.KSC
403800970306     C*
403900970306     C* TARIFFA ASSICURAZIONE NON TROVATA NELLE CONSEGNE PARTICOLARI DEL CLIENTE
404000970306     C* RICERCA NELLE CONSEGNE PARTICOLARI DI CARTELLO
404100970306IF  3C     $TPT          IFEQ      '0'                                          *TARIFFA KSC NO TROV
404200970306     C     CP88          ANDNE     'NO'
404300970306     C                   MOVEL     '1'           $TPT                           *TARIFFA TROVATA
404400970306     C                   Z-ADD     CARKSC        TPTKSC                         *CHIAVE CARTELLO
404500970306     C                   Z-ADD     CARCTR        TPTCTR
404600970306     C                   Z-ADD     CARPRG        TPTPRG
404700970306     C                   MOVEL     KTPFTC        TPTFTC
404800970306     C                   MOVEL     TTPG8P        TPTTPG                         *CAMPI CARTELLO
404900970306     C                   Z-ADD     TARF8P        TPTARF
405000970306     C                   Z-ADD     TARL8P        TPTARL
405100970306     C                   Z-ADD     TARO8P        TPTARO
405200970306     C                   Z-ADD     TRPV8P        TPTRPV
405300970306     C                   MOVEL     TAPL8P        TPTAPL
405400970306E   3C                   ENDIF
405500970306     C*
405600980506     C* CALCOLA RPV, ARROTONDAMENTI E PESO TASSABILE
405700970306IF  3C     $TPT          IFEQ      '1'                                          *TARIFFA TROVATA
405800980707     C                   MOVEL     TPTTPG        WTPG
405900980707     C                   MOVEL     TPTRPV        WRPV
406000980707     C                   CLEAR                   WDET
406100980707     C                   Z-ADD     TPTARL        WARL
406200980707     C                   Z-ADD     TPTARF        WARF
406300980707     C                   Z-ADD     TPTARO        WARO
406400980506     C                   EXSR      CALCOL
406500980506IF  4C     TPTTPG        IFEQ      'V'                                          *CALCOLO A VALORE
406600980506     C                   Z-ADD     IASLIT        ISOPES                         *IMPORTO DA ASSICURA
406700970306E   4C                   ENDIF
406800980703IF  4C     TPTTPG        IFEQ      'T'                                          *A TRASPORTO
406900980703     C                   Z-ADD     TOTTRA        ISOPES                         *TRASPORTO
407000980703E   4C                   ENDIF
407100970306     C*
407200970306     C* CALCOLA LA TARIFFA ASSICURAZIONE PER CONTO
407300011031     C                   Z-ADD     ISOPES        TPDPES           23 8          *PESO
407400970306     C                   EXSR      CALACD
407500980706     C                   EXSR      CALTRA                                       *CALCOLA % A TRASPOR
407600980506IF  4C     WTPO          IFEQ      'A'
407700980506     C                   Z-ADD     VARIA         TASASA                         *ASSIC.NE ASSEGNATO
407800980506X   4C                   ELSE
407900980506     C                   Z-ADD     VARIA         TASASF                         *ASSIC.NE FRANCO
408000970306E   4C                   ENDIF
408100970306E   3C                   ENDIF
408200970306E   2C                   ENDIF
408300970306E   1C                   ENDIF
408400970306     C*
408500970306     C                   ENDSR
408600001227     C*------------------------------------------------------------------------*
408700060315     C* CAL048 - CALCOLA ADG SU ASSIC ac plus FRANCO
408800001227     C*------------------------------------------------------------------------*
408900001227     C     CAL048        BEGSR
409000001227     C*
409100001227     C* REIMPOSTA LE VARIABILI DI LAVORO
409200011031     C                   Z-ADD     *ZEROS        TASADG           21 8
409300001227     C*
409400001227     C* CALCOLA L'A.D.G.
409500011031     C                   Z-ADD     VIMP(9)       WIMP             23 8
409600001227     C                   EXSR      ADGCAL
409700001227     C*
409800001227IF  1C     TASADG        IFGT      *ZEROS
409900001227     C                   Z-ADD     48            I
410000001227     C                   Z-ADD     TASADG        VIMP(I)
410100001227E   1C                   ENDIF
410200001227     C*
410300001227     C                   ENDSR
410400001227     C*------------------------------------------------------------------------*
410500060315     C* CAL065 - CALCOLA ISTAT SU ADG/ASS ac plus FRANCO
410600001227     C*------------------------------------------------------------------------*
410700001227     C     CAL065        BEGSR
410800001227     C*
410900001227     C* REIMPOSTA LE VARIABILI DI LAVORO
411000011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
411100001227     C*
411200001227     C* CALCOLA L'ISTAT
411300011031     C                   Z-ADD     VIMP(48)      WIMP             23 8
411400001227     C                   EXSR      IACCAL
411500001227     C*
411600001227IF  1C     TASIAC        IFGT      *ZEROS
411700001227     C                   Z-ADD     65            I
411800001227     C                   Z-ADD     TASIAC        VIMP(I)
411900001227E   1C                   ENDIF
412000001227     C*
412100001227     C                   ENDSR
412200001227     C*------------------------------------------------------------------------*
412300060315     C* CAL060 - CALCOLA ADG SU ASSIC ac plus ASSEGNATO
412400001227     C*------------------------------------------------------------------------*
412500001227     C     CAL060        BEGSR
412600001227     C*
412700001227     C* REIMPOSTA LE VARIABILI DI LAVORO
412800011031     C                   Z-ADD     *ZEROS        TASADG           21 8
412900001227     C*
413000001227     C* CALCOLA L'A.D.G.
413100011031     C                   Z-ADD     VIMP(31)      WIMP             23 8
413200001227     C                   EXSR      ADGCAL
413300001227     C*
413400001227IF  1C     TASADG        IFGT      *ZEROS
413500001227     C                   Z-ADD     60            I
413600001227     C                   Z-ADD     TASADG        VIMP(I)
413700001227E   1C                   ENDIF
413800001227     C*
413900001227     C                   ENDSR
414000001227     C*------------------------------------------------------------------------*
414100060315     C* CAL077 - CALCOLA ISTAT SU ADG/ASS ac plus ASSEGNATO
414200001227     C*------------------------------------------------------------------------*
414300001227     C     CAL077        BEGSR
414400001227     C*
414500001227     C* REIMPOSTA LE VARIABILI DI LAVORO
414600011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
414700001227     C*
414800001227     C* CALCOLA L'ISTAT
414900011031     C                   Z-ADD     VIMP(60)      WIMP             23 8
415000001227     C                   EXSR      IACCAL
415100001227     C*
415200001227IF  1C     TASIAC        IFGT      *ZEROS
415300001227     C                   Z-ADD     77            I
415400001227     C                   Z-ADD     TASIAC        VIMP(I)
415500001227E   1C                   ENDIF
415600001227     C*
415700001227     C                   ENDSR
415800060315     C*------------------------------------------------------------------------*
415900060315     C* CAL106 - CALCOLA ADG SU ASSIC ac base FRANCO
416000060315     C*------------------------------------------------------------------------*
416100060315     C     CAL106        BEGSR
416200060315     C*
416300060315     C* REIMPOSTA LE VARIABILI DI LAVORO
416400060315     C                   Z-ADD     *ZEROS        TASADG           21 8
416500060315     C*
416600060315     C* CALCOLA L'A.D.G.
416700060315     C                   Z-ADD     VIMP(105)     WIMP             23 8
416800060315     C                   EXSR      ADGCAL
416900060315     C*
417000060315IF  1C     TASADG        IFGT      *ZEROS
417100060315     C                   Z-ADD     106           I
417200060315     C                   Z-ADD     TASADG        VIMP(I)
417300060315E   1C                   ENDIF
417400060315     C*
417500060315     C                   ENDSR
417600060315     C*------------------------------------------------------------------------*
417700060315     C* CAL107 - CALCOLA ISTAT SU ADG/ASS ac base FRANCO
417800060315     C*------------------------------------------------------------------------*
417900060315     C     CAL107        BEGSR
418000060315     C*
418100060315     C* REIMPOSTA LE VARIABILI DI LAVORO
418200060315     C                   Z-ADD     *ZEROS        TASIAC           21 8
418300060315     C*
418400060315     C* CALCOLA L'ISTAT
418500060315     C                   Z-ADD     VIMP(106)     WIMP             23 8
418600060315     C                   EXSR      IACCAL
418700060315     C*
418800060315IF  1C     TASIAC        IFGT      *ZEROS
418900060315     C                   Z-ADD     107           I
419000060315     C                   Z-ADD     TASIAC        VIMP(I)
419100060315E   1C                   ENDIF
419200060315     C*
419300060315     C                   ENDSR
419400060315     C*------------------------------------------------------------------------*
419500060315     C* CAL109 - CALCOLA ADG SU ASSIC ac base ASSEGNATO
419600060315     C*------------------------------------------------------------------------*
419700060315     C     CAL109        BEGSR
419800060315     C*
419900060315     C* REIMPOSTA LE VARIABILI DI LAVORO
420000060315     C                   Z-ADD     *ZEROS        TASADG           21 8
420100060315     C*
420200060315     C* CALCOLA L'A.D.G.
420300060315     C                   Z-ADD     VIMP(108)     WIMP             23 8
420400060315     C                   EXSR      ADGCAL
420500060315     C*
420600060315IF  1C     TASADG        IFGT      *ZEROS
420700060315     C                   Z-ADD     109           I
420800060315     C                   Z-ADD     TASADG        VIMP(I)
420900060315E   1C                   ENDIF
421000060315     C*
421100060315     C                   ENDSR
421200060315     C*------------------------------------------------------------------------*
421300060315     C* CAL110 - CALCOLA ISTAT SU ADG/ASS ac base ASSEGNATO
421400060315     C*------------------------------------------------------------------------*
421500060315     C     CAL110        BEGSR
421600060315     C*
421700060315     C* REIMPOSTA LE VARIABILI DI LAVORO
421800060315     C                   Z-ADD     *ZEROS        TASIAC           21 8
421900060315     C*
422000060315     C* CALCOLA L'ISTAT
422100060315     C                   Z-ADD     VIMP(109)     WIMP             23 8
422200060315     C                   EXSR      IACCAL
422300060315     C*
422400060315IF  1C     TASIAC        IFGT      *ZEROS
422500060315     C                   Z-ADD     110           I
422600060315     C                   Z-ADD     TASIAC        VIMP(I)
422700060315E   1C                   ENDIF
422800060315     C*
422900060315     C                   ENDSR
423000970306     C*------------------------------------------------------------------------*
423100970306     C* CALRCV - CALCOLA ASSICURAZIONE R.C.V.
423200970306     C*------------------------------------------------------------------------*
423300970306     C     CALRCV        BEGSR
423400970306     C*
423500970306     C* REIMPOSTA LE VARIABILI DI LAVORO
423600011031     C                   Z-ADD     *ZEROS        TASRCF           21 8
423700011031     C                   Z-ADD     *ZEROS        TASRCA           21 8
423800980506     C*---
423900980506     C* FRANCO
424000980506     C*---
424100980506     C                   MOVEL     'F'           WTPO
424200970306     C                   EXSR      CALVE
424300970306IF  1C     VC(10)        IFEQ      'S'                                          *VOCE DA CALCOLARE
424400970314     C     TASRCF        ANDGT     *ZEROS
424500970306     C                   Z-ADD     10            I
424600970306     C                   Z-ADD     TASRCF        VIMP(I)
424700970306E   1C                   ENDIF
424800980506     C*---
424900980506     C* ASSEGNATO
425000980506     C*---
425100980707     C                   MOVEL     'A'           WTPO              1
425200980506     C                   EXSR      CALVE
425300970306IF  1C     VC(32)        IFEQ      'S'                                          *VOCE DA CALCOLARE
425400970314     C     TASRCA        ANDGT     *ZEROS
425500970306     C                   Z-ADD     32            I
425600970306     C                   Z-ADD     TASRCA        VIMP(I)
425700970306E   1C                   ENDIF
425800970306     C*
425900970306     C                   ENDSR
426000970306     C*------------------------------------------------------------------------*
426100970306     C* CALVE - CALCOLA CONSEGNA PARTICOLARE ASSICURAZIONE R.C.V.
426200970306     C*------------------------------------------------------------------------*
426300970306     C     CALVE         BEGSR
426400980804     C*
426500980804     C* REIMPOSTA LE VARIABILI DI LAVORO
426600980804     C                   MOVEL     'N'           WCAR                           *NO UTILIZZO TAR.CAR
426700980506     C*
426800980506     C* CERCA TARIFFA PARTICOLARE
426900980506     C                   MOVEL     'R'           TASTC             1            *CODICE CONSEGNA
427000980506     C                   EXSR      CERTPT
427100970306     C*
427200980506     C* CALCOLA RPV, ARROTONDAMENTI E PESO TASSABILE
427300980506IF  1C     $TPT          IFEQ      '1'                                          *TARIFFA TROVATA
427400980707     C                   MOVEL     TPTTPG        WTPG
427500980707     C                   MOVEL     TPTRPV        WRPV
427600980707     C                   CLEAR                   WDET
427700980707     C                   Z-ADD     TPTARL        WARL
427800980707     C                   Z-ADD     TPTARF        WARF
427900980707     C                   Z-ADD     TPTARO        WARO
428000980506     C                   EXSR      CALCOL
428100980506IF  2C     TPTTPG        IFEQ      'V'                                          *A VALORE
428200040716IF  3C     TPTVLM        IFGT      §gelrp
428300040716     C     TPTVLM        SUB       §gelrp        ISOPES
428400980506     C     ISOPES        MULT      SPSC(S)       ISOPES
428500980506E   3C                   ENDIF
428600980506E   2C                   ENDIF
428700980703IF  2C     TPTTPG        IFEQ      'T'                                          *A TRASPORTO
428800980703     C                   Z-ADD     TOTTRA        ISOPES                         *TRASPORTO
428900980703E   2C                   ENDIF
429000970306     C*
429100970306     C* CALCOLA LA TARIFFA ASSICURAZIONE R.C.V.
429200980506     C                   Z-ADD     ISOPES        TPDPES                         *PESO
429300970306     C                   EXSR      CALACD
429400980706     C                   EXSR      CALTRA                                       *CALCOLA % A TRASPOR
429500970306     C*---
429600970306     C* TIPO APPLICAZIONE TARIFFA:
429700980506     C* PARTENZA -> RICAVO SOLO PER FRANCHI
429800980506     C* ARRIVO   -> RICAVO SOLO PER ASSEGNATI
429900980506     C* BLANKS   -> RICAVO PER ENTRAMBI
430000970306     C*---
430100980506IF  2C     WTPO          IFEQ      'F'
430200980506IF  3C     TPTAPL        IFEQ      *BLANKS
430300980506     C     TPTAPL        OREQ      'P'
430400980506     C                   Z-ADD     VARIA         TASRCF                         *RICAVO FRANCO
430500980506E   3C                   ENDIF
430600980506X   2C                   ELSE
430700980506IF  3C     TPTAPL        IFEQ      *BLANKS
430800980506     C     TPTAPL        OREQ      'A'
430900980506     C                   Z-ADD     VARIA         TASRCA                         *RICAVO ASSEGNAO
431000980506E   3C                   ENDIF
431100980506E   2C                   ENDIF
431200980506E   1C                   ENDIF
431300970306     C*
431400970306     C                   ENDSR
431500001227     C*------------------------------------------------------------------------*
431600001227     C* CAL049 - CALCOLA ADG SU ASSIC RCV  FRANCO
431700001227     C*------------------------------------------------------------------------*
431800001227     C     CAL049        BEGSR
431900001227     C*
432000001227     C* REIMPOSTA LE VARIABILI DI LAVORO
432100011031     C                   Z-ADD     *ZEROS        TASADG           21 8
432200001227     C*
432300001227     C* CALCOLA L'A.D.G.
432400011031     C                   Z-ADD     VIMP(10)      WIMP             23 8
432500001227     C                   EXSR      ADGCAL
432600001227     C*
432700001227IF  1C     TASADG        IFGT      *ZEROS
432800001227     C                   Z-ADD     49            I
432900001227     C                   Z-ADD     TASADG        VIMP(I)
433000001227E   1C                   ENDIF
433100001227     C*
433200001227     C                   ENDSR
433300001227     C*------------------------------------------------------------------------*
433400001227     C* CAL066 - CALCOLA ISTAT SU ADG/ASS RCV  FRANCO
433500001227     C*------------------------------------------------------------------------*
433600001227     C     CAL066        BEGSR
433700001227     C*
433800001227     C* REIMPOSTA LE VARIABILI DI LAVORO
433900011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
434000001227     C*
434100001227     C* CALCOLA L'ISTAT
434200011031     C                   Z-ADD     VIMP(49)      WIMP             23 8
434300001227     C                   EXSR      IACCAL
434400001227     C*
434500001227IF  1C     TASIAC        IFGT      *ZEROS
434600001227     C                   Z-ADD     66            I
434700001227     C                   Z-ADD     TASIAC        VIMP(I)
434800001227E   1C                   ENDIF
434900001227     C*
435000001227     C                   ENDSR
435100001227     C*------------------------------------------------------------------------*
435200001227     C* CAL061 - CALCOLA ADG SU ASSIC RCV  ASSEGNATO
435300001227     C*------------------------------------------------------------------------*
435400001227     C     CAL061        BEGSR
435500001227     C*
435600001227     C* REIMPOSTA LE VARIABILI DI LAVORO
435700011031     C                   Z-ADD     *ZEROS        TASADG           21 8
435800001227     C*
435900001227     C* CALCOLA L'A.D.G.
436000011031     C                   Z-ADD     VIMP(32)      WIMP             23 8
436100001227     C                   EXSR      ADGCAL
436200001227     C*
436300001227IF  1C     TASADG        IFGT      *ZEROS
436400001227     C                   Z-ADD     61            I
436500001227     C                   Z-ADD     TASADG        VIMP(I)
436600001227E   1C                   ENDIF
436700001227     C*
436800001227     C                   ENDSR
436900001227     C*------------------------------------------------------------------------*
437000001227     C* CAL078 - CALCOLA ISTAT SU ADG/ASS RCV  ASSEGNATO
437100001227     C*------------------------------------------------------------------------*
437200001227     C     CAL078        BEGSR
437300001227     C*
437400001227     C* REIMPOSTA LE VARIABILI DI LAVORO
437500011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
437600001227     C*
437700001227     C* CALCOLA L'ISTAT
437800011031     C                   Z-ADD     VIMP(61)      WIMP             23 8
437900001227     C                   EXSR      IACCAL
438000001227     C*
438100001227IF  1C     TASIAC        IFGT      *ZEROS
438200001227     C                   Z-ADD     78            I
438300001227     C                   Z-ADD     TASIAC        VIMP(I)
438400001227E   1C                   ENDIF
438500001227     C*
438600001227     C                   ENDSR
438700970306     C*------------------------------------------------------------------------*
438800970306     C* CALCPA - CALCOLA CONSEGNA PARTICOLARE SU APPUNTAMENTO
438900970306     C*------------------------------------------------------------------------*
439000970306     C     CALCPA        BEGSR
439100970306     C*
439200970306     C* REIMPOSTA LE VARIABILI DI LAVORO
439300980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
439400011031     C                   Z-ADD     *ZEROS        TASCPA           21 8
439500970306     C*
439600970306     C* CALCOLA CONSEGNA PARTICOLARE
439700970306     C                   MOVEL     'A'           TASTC             1            *CODICE CONSEGNA
439800011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
439900970306     C                   EXSR      CALCP
440000970306     C                   Z-ADD     TASCP         TASCPA
440100970306     C*
440200970314IF  1C     TASCPA        IFGT      *ZEROS
440300970306     C                   Z-ADD     11            I
440400970306     C                   Z-ADD     TASCPA        VIMP(I)
440500970314E   1C                   ENDIF
440600970306     C*
440700970306     C                   ENDSR
440800001227     C*------------------------------------------------------------------------*
440900001227     C* CAL050 - CALCOLA ADG SU CONSEGNA APPUNTAMENTO
441000001227     C*------------------------------------------------------------------------*
441100001227     C     CAL050        BEGSR
441200001227     C*
441300001227     C* REIMPOSTA LE VARIABILI DI LAVORO
441400011031     C                   Z-ADD     *ZEROS        TASADG           21 8
441500001227     C*
441600001227     C* CALCOLA L'A.D.G.
441700011031     C                   Z-ADD     VIMP(11)      WIMP             23 8
441800001227     C                   EXSR      ADGCAL
441900001227     C*
442000001227IF  1C     TASADG        IFGT      *ZEROS
442100001227     C                   Z-ADD     50            I
442200001227     C                   Z-ADD     TASADG        VIMP(I)
442300001227E   1C                   ENDIF
442400001227     C*
442500001227     C                   ENDSR
442600001227     C*------------------------------------------------------------------------*
442700001227     C* CAL067 - CALCOLA ISTAT SU ADG/CONSEGNA APPUNTAMENTO
442800001227     C*------------------------------------------------------------------------*
442900001227     C     CAL067        BEGSR
443000001227     C*
443100001227     C* REIMPOSTA LE VARIABILI DI LAVORO
443200011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
443300001227     C*
443400001227     C* CALCOLA L'ISTAT
443500011031     C                   Z-ADD     VIMP(50)      WIMP             23 8
443600001227     C                   EXSR      IACCAL
443700001227     C*
443800001227IF  1C     TASIAC        IFGT      *ZEROS
443900001227     C                   Z-ADD     67            I
444000001227     C                   Z-ADD     TASIAC        VIMP(I)
444100001227E   1C                   ENDIF
444200001227     C*
444300001227     C                   ENDSR
444400970306     C*------------------------------------------------------------------------*
444500970306     C* CALCPS - CALCOLA CONSEGNA PARTICOLARE AI SUPERMERCATI
444600970306     C*------------------------------------------------------------------------*
444700970306     C     CALCPS        BEGSR
444800970306     C*
444900970306     C* REIMPOSTA LE VARIABILI DI LAVORO
445000980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
445100011031     C                   Z-ADD     *ZEROS        TASCPS           21 8
445200970306     C*
445300970306     C* CALCOLA CONSEGNA PARTICOLARE
445400970306     C                   MOVEL     'S'           TASTC             1            *CODICE CONSEGNA
445500011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
445600980804     C                   EXSR      CALCP
445700970306     C                   Z-ADD     TASCP         TASCPS
445800970306     C*
445900970314IF  1C     TASCPS        IFGT      *ZEROS
446000970306     C                   Z-ADD     12            I
446100970306     C                   Z-ADD     TASCPS        VIMP(I)
446200970314E   1C                   ENDIF
446300970306     C*
446400970306     C                   ENDSR
446500001227     C*------------------------------------------------------------------------*
446600001227     C* CAL051 - CALCOLA ADG SU CONSEGNA SUPERMERCATI
446700001227     C*------------------------------------------------------------------------*
446800001227     C     CAL051        BEGSR
446900001227     C*
447000001227     C* REIMPOSTA LE VARIABILI DI LAVORO
447100011031     C                   Z-ADD     *ZEROS        TASADG           21 8
447200001227     C*
447300001227     C* CALCOLA L'A.D.G.
447400011031     C                   Z-ADD     VIMP(12)      WIMP             23 8
447500001227     C                   EXSR      ADGCAL
447600001227     C*
447700001227IF  1C     TASADG        IFGT      *ZEROS
447800001227     C                   Z-ADD     51            I
447900001227     C                   Z-ADD     TASADG        VIMP(I)
448000001227E   1C                   ENDIF
448100001227     C*
448200001227     C                   ENDSR
448300001227     C*------------------------------------------------------------------------*
448400001227     C* CAL068 - CALCOLA ISTAT SU ADG/CONSEGNA SUPERMERCATI
448500001227     C*------------------------------------------------------------------------*
448600001227     C     CAL068        BEGSR
448700001227     C*
448800001227     C* REIMPOSTA LE VARIABILI DI LAVORO
448900011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
449000001227     C*
449100001227     C* CALCOLA L'ISTAT
449200011031     C                   Z-ADD     VIMP(51)      WIMP             23 8
449300001227     C                   EXSR      IACCAL
449400001227     C*
449500001227IF  1C     TASIAC        IFGT      *ZEROS
449600001227     C                   Z-ADD     68            I
449700001227     C                   Z-ADD     TASIAC        VIMP(I)
449800001227E   1C                   ENDIF
449900001227     C*
450000001227     C                   ENDSR
450100970306     C*------------------------------------------------------------------------*
450200970306     C* CALCPP - CALCOLA CONSEGNA PARTICOLARE AI PIANI
450300970306     C*------------------------------------------------------------------------*
450400970306     C     CALCPP        BEGSR
450500970306     C*
450600970306     C* REIMPOSTA LE VARIABILI DI LAVORO
450700980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
450800011031     C                   Z-ADD     *ZEROS        TASCPP           21 8
450900970306     C*
451000970306     C* CALCOLA CONSEGNA PARTICOLARE
451100970306     C                   MOVEL     'P'           TASTC             1            *CODICE CONSEGNA
451200011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
451300970306     C                   EXSR      CALCP
451400970306     C                   Z-ADD     TASCP         TASCPP
451500970306     C*
451600970314IF  1C     TASCPP        IFGT      *ZEROS
451700970306     C                   Z-ADD     13            I
451800970306     C                   Z-ADD     TASCPP        VIMP(I)
451900970314E   1C                   ENDIF
452000970306     C*
452100970306     C                   ENDSR
452200001227     C*------------------------------------------------------------------------*
452300001227     C* CAL052 - CALCOLA ADG SU CONSEGNA AI PIANI
452400001227     C*------------------------------------------------------------------------*
452500001227     C     CAL052        BEGSR
452600001227     C*
452700001227     C* REIMPOSTA LE VARIABILI DI LAVORO
452800011031     C                   Z-ADD     *ZEROS        TASADG           21 8
452900001227     C*
453000001227     C* CALCOLA L'A.D.G.
453100011031     C                   Z-ADD     VIMP(13)      WIMP             23 8
453200001227     C                   EXSR      ADGCAL
453300001227     C*
453400001227IF  1C     TASADG        IFGT      *ZEROS
453500001227     C                   Z-ADD     52            I
453600001227     C                   Z-ADD     TASADG        VIMP(I)
453700001227E   1C                   ENDIF
453800001227     C*
453900001227     C                   ENDSR
454000001227     C*------------------------------------------------------------------------*
454100001227     C* CAL069 - CALCOLA ISTAT SU ADG/CONSEGNA AI PIANI
454200001227     C*------------------------------------------------------------------------*
454300001227     C     CAL069        BEGSR
454400001227     C*
454500001227     C* REIMPOSTA LE VARIABILI DI LAVORO
454600011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
454700001227     C*
454800001227     C* CALCOLA L'ISTAT
454900011031     C                   Z-ADD     VIMP(52)      WIMP             23 8
455000001227     C                   EXSR      IACCAL
455100001227     C*
455200001227IF  1C     TASIAC        IFGT      *ZEROS
455300001227     C                   Z-ADD     69            I
455400001227     C                   Z-ADD     TASIAC        VIMP(I)
455500001227E   1C                   ENDIF
455600001227     C*
455700001227     C                   ENDSR
455800151120     C*------------------------------------------------------------------------*
455900151120     C* CALpkl - CALCOLA packing list
456000151120     C*------------------------------------------------------------------------*
456100151120     C     CALpkl        BEGSR
456200151120     C*
456300151120     C* REIMPOSTA LE VARIABILI DI LAVORO
456400151120     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
456500151120     C                   Z-ADD     *ZEROS        TASpkl           21 8
456600151120     C*
456700151120     C* CALCOLA CONSEGNA PARTICOLARE
456800151120     C                   MOVEL     'k'           TASTC             1            *CODICE CONSEGNA
456900151120     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
457000151120     C                   EXSR      CALCP
457100151120     C                   Z-ADD     TASCP         TASpkl
457200151120     C*
457300151120IF  1C     TASpkl        IFGT      *ZEROS
457400151120     C                   Z-ADD     114           I
457500151120     C                   Z-ADD     TASpkl        VIMP(I)
457600151120E   1C                   ENDIF
457700151120     C*
457800151120     C                   ENDSR
457900151120     C*------------------------------------------------------------------------*
458000151120     C* CAL115 - CALCOLA ADG SU packing list
458100151120     C*------------------------------------------------------------------------*
458200151120     C     CAL115        BEGSR
458300151120     C*
458400151120     C* REIMPOSTA LE VARIABILI DI LAVORO
458500151120     C                   Z-ADD     *ZEROS        TASADG           21 8
458600151120     C*
458700151120     C* CALCOLA L'A.D.G.
458800151120     C                   Z-ADD     VIMP(114)     WIMP             23 8
458900151120     C                   EXSR      ADGCAL
459000151120     C*
459100151120IF  1C     TASADG        IFGT      *ZEROS
459200151120     C                   Z-ADD     115           I
459300151120     C                   Z-ADD     TASADG        VIMP(I)
459400151120E   1C                   ENDIF
459500151120     C*
459600151120     C                   ENDSR
459700151120     C*------------------------------------------------------------------------*
459800151120     C* CAL069 - CALCOLA ISTAT SU ADG/packing list
459900151120     C*------------------------------------------------------------------------*
460000151120     C     CAL116        BEGSR
460100151120     C*
460200151120     C* REIMPOSTA LE VARIABILI DI LAVORO
460300151120     C                   Z-ADD     *ZEROS        TASIAC           21 8
460400151120     C*
460500151120     C* CALCOLA L'ISTAT
460600151120     C                   Z-ADD     VIMP(115)     WIMP             23 8
460700151120     C                   EXSR      IACCAL
460800151120     C*
460900151120IF  1C     TASIAC        IFGT      *ZEROS
461000151120     C                   Z-ADD     116           I
461100151120     C                   Z-ADD     TASIAC        VIMP(I)
461200151120E   1C                   ENDIF
461300151120     C*
461400151120     C                   ENDSR
461500151120     C*------------------------------------------------------------------------*
461600151120     C* CALrtm - CALCOLA ritiro telefonico
461700151120     C*------------------------------------------------------------------------*
461800151120     C     CALrtm        BEGSR
461900151120     C*
462000151120     C* REIMPOSTA LE VARIABILI DI LAVORO
462100151120     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
462200151120     C                   Z-ADD     *ZEROS        TASrtm           21 8
462300151120     C*
462400151120     C* CALCOLA CONSEGNA PARTICOLARE
462500151120     C                   MOVEL     't'           TASTC             1            *CODICE CONSEGNA
462600151120     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
462700151120     C                   EXSR      CALCP
462800151120     C                   Z-ADD     TASCP         TASrtm
462900151120     C*
463000151120IF  1C     TASrtm        IFGT      *ZEROS
463100151120     C                   Z-ADD     111           I
463200151120     C                   Z-ADD     TASrtm        VIMP(I)
463300151120E   1C                   ENDIF
463400151120     C*
463500151120     C                   ENDSR
463600151120     C*------------------------------------------------------------------------*
463700151120     C* CAL112 - CALCOLA ADG SU ritiro telefonico
463800151120     C*------------------------------------------------------------------------*
463900151120     C     CAL112        BEGSR
464000151120     C*
464100151120     C* REIMPOSTA LE VARIABILI DI LAVORO
464200151120     C                   Z-ADD     *ZEROS        TASADG           21 8
464300151120     C*
464400151120     C* CALCOLA L'A.D.G.
464500151120     C                   Z-ADD     VIMP(111)     WIMP             23 8
464600151120     C                   EXSR      ADGCAL
464700151120     C*
464800151120IF  1C     TASADG        IFGT      *ZEROS
464900151120     C                   Z-ADD     112           I
465000151120     C                   Z-ADD     TASADG        VIMP(I)
465100151120E   1C                   ENDIF
465200151120     C*
465300151120     C                   ENDSR
465400151120     C*------------------------------------------------------------------------*
465500151120     C* CAL113 - CALCOLA ISTAT SU ADG/ritiro telefonico
465600151120     C*------------------------------------------------------------------------*
465700151120     C     CAL113        BEGSR
465800151120     C*
465900151120     C* REIMPOSTA LE VARIABILI DI LAVORO
466000151120     C                   Z-ADD     *ZEROS        TASIAC           21 8
466100151120     C*
466200151120     C* CALCOLA L'ISTAT
466300151120     C                   Z-ADD     VIMP(112)     WIMP             23 8
466400151120     C                   EXSR      IACCAL
466500151120     C*
466600151120IF  1C     TASIAC        IFGT      *ZEROS
466700151120     C                   Z-ADD     113           I
466800151120     C                   Z-ADD     TASIAC        VIMP(I)
466900151120E   1C                   ENDIF
467000151120     C*
467100151120     C                   ENDSR
467200040826     C*------------------------------------------------------------------------*
467300040826     C* CALdes - CALCOLA CONSEGNA destinatario disagiato
467400040826     C*------------------------------------------------------------------------*
467500040826     C     CALdes        BEGSR
467600040826     C*
467700040826     C* REIMPOSTA LE VARIABILI DI LAVORO
467800040826     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
467900040826     C                   Z-ADD     *ZEROS        TASdes           21 8
468000040826     C*
468100040826     C* CALCOLA CONSEGNA PARTICOLARE
468200040826     C                   MOVEL     'X'           TASTC             1            *CODICE CONSEGNA
468300040826     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
468400040826     C                   EXSR      CALCP
468500040826     C                   Z-ADD     TASCP         TASdes
468600040826     C*
468700040826IF  1C     TASdes        IFGT      *ZEROS
468800040826     C                   Z-ADD     96            I
468900040826     C                   Z-ADD     TASdes        VIMP(I)
469000040826E   1C                   ENDIF
469100040826     C*
469200040826     C                   ENDSR
469300040826     C*------------------------------------------------------------------------*
469400040826     C* CAL097 - CALCOLA ADG SU CONSEGNA destinatario disagiato
469500040826     C*------------------------------------------------------------------------*
469600040826     C     CAL097        BEGSR
469700040826     C*
469800040826     C* REIMPOSTA LE VARIABILI DI LAVORO
469900040826     C                   Z-ADD     *ZEROS        TASADG
470000040826     C*
470100040826     C* CALCOLA L'A.D.G.
470200040826     C                   Z-ADD     VIMP(96)      WIMP
470300040826     C                   EXSR      ADGCAL
470400040826     C*
470500040826IF  1C     TASADG        IFGT      *ZEROS
470600040826     C                   Z-ADD     97            I
470700040826     C                   Z-ADD     TASADG        VIMP(I)
470800040826E   1C                   ENDIF
470900040826     C*
471000040826     C                   ENDSR
471100040826     C*------------------------------------------------------------------------*
471200040826     C* CAL098 - CALCOLA ISTAT SU ADG/CONSEGNA destinatario disagiato
471300040826     C*------------------------------------------------------------------------*
471400040826     C     CAL098        BEGSR
471500040826     C*
471600040826     C* REIMPOSTA LE VARIABILI DI LAVORO
471700040826     C                   Z-ADD     *ZEROS        TASIAC
471800040826     C*
471900040826     C* CALCOLA L'ISTAT
472000040826     C                   Z-ADD     VIMP(97)      WIMP
472100040826     C                   EXSR      IACCAL
472200040826     C*
472300040826IF  1C     TASIAC        IFGT      *ZEROS
472400040826     C                   Z-ADD     98            I
472500040826     C                   Z-ADD     TASIAC        VIMP(I)
472600040826E   1C                   ENDIF
472700040826     C*
472800040826     C                   ENDSR
472900040826     C*------------------------------------------------------------------------*
473000040826     C* CALban - CALCOLA bancali
473100040826     C*------------------------------------------------------------------------*
473200040826     C     CALban        BEGSR
473300040826     C*
473400040826     C* REIMPOSTA LE VARIABILI DI LAVORO
473500040826     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
473600040826     C                   Z-ADD     *ZEROS        TASban           21 8
473700040826     C*
473800040826     C* CALCOLA CONSEGNA PARTICOLARE
473900040826     C                   MOVEL     '='           TASTC             1            *CODICE CONSEGNA
474000040826     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
474100040826     C                   Z-ADD     ETCnba        TOTqta           21 8          *VALORE CONTRASS.
474200040826     C                   EXSR      CALCP
474300040826     C                   Z-ADD     *ZEROS        TOTqta
474400040826     C                   Z-ADD     TASCP         TASban
474500040826     C*
474600040826IF  1C     TASban        IFGT      *ZEROS
474700040826     C                   Z-ADD     99            I
474800040826     C                   Z-ADD     TASban        VIMP(I)
474900040826E   1C                   ENDIF
475000040826     C*
475100040826     C                   ENDSR
475200040826     C*------------------------------------------------------------------------*
475300040826     C* CAL100 - CALCOLA ADG SU bancali
475400040826     C*------------------------------------------------------------------------*
475500040826     C     CAL100        BEGSR
475600040826     C*
475700040826     C* REIMPOSTA LE VARIABILI DI LAVORO
475800040826     C                   Z-ADD     *ZEROS        TASADG
475900040826     C*
476000040826     C* CALCOLA L'A.D.G.
476100040826     C                   Z-ADD     VIMP(99)      WIMP
476200040826     C                   EXSR      ADGCAL
476300040826     C*
476400040826IF  1C     TASADG        IFGT      *ZEROS
476500040826     C                   Z-ADD     100           I
476600040826     C                   Z-ADD     TASADG        VIMP(I)
476700040826E   1C                   ENDIF
476800040826     C*
476900040826     C                   ENDSR
477000040826     C*------------------------------------------------------------------------*
477100040826     C* CAL101 - CALCOLA ISTAT SU ADG bancali
477200040826     C*------------------------------------------------------------------------*
477300040826     C     CAL101        BEGSR
477400040826     C*
477500040826     C* REIMPOSTA LE VARIABILI DI LAVORO
477600040826     C                   Z-ADD     *ZEROS        TASIAC
477700040826     C*
477800040826     C* CALCOLA L'ISTAT
477900040826     C                   Z-ADD     VIMP(100)     WIMP
478000040826     C                   EXSR      IACCAL
478100040826     C*
478200040826IF  1C     TASIAC        IFGT      *ZEROS
478300040826     C                   Z-ADD     101           I
478400040826     C                   Z-ADD     TASIAC        VIMP(I)
478500040826E   1C                   ENDIF
478600040826     C*
478700040826     C                   ENDSR
478800970306     C*------------------------------------------------------------------------*
478900970306     C* CALCPF - CALCOLA CONSEGNA PARTICOLARE FUORI MISURA
479000970306     C*------------------------------------------------------------------------*
479100970306     C     CALCPF        BEGSR
479200970306     C*
479300970306     C* REIMPOSTA LE VARIABILI DI LAVORO
479400980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
479500011031     C                   Z-ADD     *ZEROS        TASCPF           21 8
479600970306     C*
479700970306     C* CALCOLA CONSEGNA PARTICOLARE
479800970306     C                   MOVEL     'F'           TASTC             1            *CODICE CONSEGNA
479900011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
480000970306     C                   EXSR      CALCP
480100970306     C                   Z-ADD     TASCP         TASCPF
480200970306     C*
480300970314IF  1C     TASCPF        IFGT      *ZEROS
480400970306     C                   Z-ADD     14            I
480500970306     C                   Z-ADD     TASCPF        VIMP(I)
480600970314E   1C                   ENDIF
480700970306     C*
480800970306     C                   ENDSR
480900001227     C*------------------------------------------------------------------------*
481000001227     C* CAL053 - CALCOLA ADG SU CONSEGNA FUORI MISURA
481100001227     C*------------------------------------------------------------------------*
481200001227     C     CAL053        BEGSR
481300001227     C*
481400001227     C* REIMPOSTA LE VARIABILI DI LAVORO
481500011031     C                   Z-ADD     *ZEROS        TASADG           21 8
481600001227     C*
481700001227     C* CALCOLA L'A.D.G.
481800011031     C                   Z-ADD     VIMP(14)      WIMP             23 8
481900001227     C                   EXSR      ADGCAL
482000001227     C*
482100001227IF  1C     TASADG        IFGT      *ZEROS
482200001227     C                   Z-ADD     53            I
482300001227     C                   Z-ADD     TASADG        VIMP(I)
482400001227E   1C                   ENDIF
482500001227     C*
482600001227     C                   ENDSR
482700001227     C*------------------------------------------------------------------------*
482800001227     C* CAL070 - CALCOLA ISTAT SU ADG/CONSEGNA FUORI MISURA
482900001227     C*------------------------------------------------------------------------*
483000001227     C     CAL070        BEGSR
483100001227     C*
483200001227     C* REIMPOSTA LE VARIABILI DI LAVORO
483300011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
483400001227     C*
483500001227     C* CALCOLA L'ISTAT
483600011031     C                   Z-ADD     VIMP(53)      WIMP             23 8
483700001227     C                   EXSR      IACCAL
483800001227     C*
483900001227IF  1C     TASIAC        IFGT      *ZEROS
484000001227     C                   Z-ADD     70            I
484100001227     C                   Z-ADD     TASIAC        VIMP(I)
484200001227E   1C                   ENDIF
484300001227     C*
484400001227     C                   ENDSR
484500970414     C*------------------------------------------------------------------------*
484600970414     C* CALCPD - CALCOLA CONSEGNA PARTICOLARE DDT
484700970414     C*------------------------------------------------------------------------*
484800970414     C     CALCPD        BEGSR
484900970414     C*
485000970414     C* REIMPOSTA LE VARIABILI DI LAVORO
485100980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
485200011031     C                   Z-ADD     *ZEROS        TASCPD           21 8
485300970414     C*
485400970414     C* CALCOLA CONSEGNA PARTICOLARE
485500970414     C                   MOVEL     'B'           TASTC             1            *CODICE CONSEGNA
485600011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
485700970414     C                   EXSR      CALCP
485800970414     C                   Z-ADD     TASCP         TASCPD
485900970414     C*
486000970414IF  1C     TASCPD        IFGT      *ZEROS
486100970414     C                   Z-ADD     33            I
486200970414     C                   Z-ADD     TASCPD        VIMP(I)
486300970414E   1C                   ENDIF
486400970414     C*
486500970414     C                   ENDSR
486600001227     C*------------------------------------------------------------------------*
486700001227     C* CAL062 - CALCOLA ADG SU CONSEGNA DDT
486800001227     C*------------------------------------------------------------------------*
486900001227     C     CAL062        BEGSR
487000001227     C*
487100001227     C* REIMPOSTA LE VARIABILI DI LAVORO
487200011031     C                   Z-ADD     *ZEROS        TASADG           21 8
487300001227     C*
487400001227     C* CALCOLA L'A.D.G.
487500011031     C                   Z-ADD     VIMP(33)      WIMP             23 8
487600001227     C                   EXSR      ADGCAL
487700001227     C*
487800001227IF  1C     TASADG        IFGT      *ZEROS
487900001227     C                   Z-ADD     62            I
488000001227     C                   Z-ADD     TASADG        VIMP(I)
488100001227E   1C                   ENDIF
488200001227     C*
488300001227     C                   ENDSR
488400001227     C*------------------------------------------------------------------------*
488500001227     C* CAL079 - CALCOLA ISTAT SU ADG/CONSEGNA DDT
488600001227     C*------------------------------------------------------------------------*
488700001227     C     CAL079        BEGSR
488800001227     C*
488900001227     C* REIMPOSTA LE VARIABILI DI LAVORO
489000011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
489100001227     C*
489200001227     C* CALCOLA L'ISTAT
489300011031     C                   Z-ADD     VIMP(62)      WIMP             23 8
489400001227     C                   EXSR      IACCAL
489500001227     C*
489600001227IF  1C     TASIAC        IFGT      *ZEROS
489700001227     C                   Z-ADD     79            I
489800001227     C                   Z-ADD     TASIAC        VIMP(I)
489900001227E   1C                   ENDIF
490000001227     C*
490100001227     C                   ENDSR
490200980706     C*------------------------------------------------------------------------*
490300980706     C* CALRIT - CALCOLA IL RITIRO
490400980706     C*------------------------------------------------------------------------*
490500980706     C     CALRIT        BEGSR
490600980706     C*
490700980706     C* REIMPOSTA LE VARIABILI DI LAVORO
490800980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
490900011031     C                   Z-ADD     *ZEROS        TASRIT           21 8
491000980706     C*
491100980706     C* CALCOLA CONSEGNA PARTICOLARE
491200980706     C                   MOVEL     'U'           TASTC             1            *CODICE CONSEGNA
491300011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
491400980706     C*
491500980706     C                   MOVEL     CCTS(T)       LCTS                           *DEPOSITO COD.TASS.
491600980706     C                   MOVEL     WMCT          CCTS(T)                        *COD.TASS.MITTENTE
491700980706     C                   EXSR      CALCP
491800980706     C                   MOVEL     LCTS          CCTS(T)                        *DEPOSITO COD.TASS.
491900980706     C                   Z-ADD     TASCP         TASRIT
492000980706     C*
492100980706IF  1C     TASRIT        IFGT      *ZEROS
492200980706     C                   Z-ADD     17            I
492300980706     C                   Z-ADD     TASRIT        VIMP(I)
492400980706E   1C                   ENDIF
492500980706     C*
492600980706     C                   ENDSR
492700001227     C*------------------------------------------------------------------------*
492800001227     C* CAL056 - CALCOLA ADG SU RITIRO
492900001227     C*------------------------------------------------------------------------*
493000001227     C     CAL056        BEGSR
493100001227     C*
493200001227     C* REIMPOSTA LE VARIABILI DI LAVORO
493300011031     C                   Z-ADD     *ZEROS        TASADG           21 8
493400001227     C*
493500001227     C* CALCOLA L'A.D.G.
493600011031     C                   Z-ADD     VIMP(17)      WIMP             23 8
493700001227     C                   EXSR      ADGCAL
493800001227     C*
493900001227IF  1C     TASADG        IFGT      *ZEROS
494000001227     C                   Z-ADD     56            I
494100001227     C                   Z-ADD     TASADG        VIMP(I)
494200001227E   1C                   ENDIF
494300001227     C*
494400001227     C                   ENDSR
494500001227     C*------------------------------------------------------------------------*
494600001227     C* CAL073 - CALCOLA ISTAT SU ADG/RITIRO
494700001227     C*------------------------------------------------------------------------*
494800001227     C     CAL073        BEGSR
494900001227     C*
495000001227     C* REIMPOSTA LE VARIABILI DI LAVORO
495100011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
495200001227     C*
495300001227     C* CALCOLA L'ISTAT
495400011031     C                   Z-ADD     VIMP(56)      WIMP             23 8
495500001227     C                   EXSR      IACCAL
495600001227     C*
495700001227IF  1C     TASIAC        IFGT      *ZEROS
495800001227     C                   Z-ADD     73            I
495900001227     C                   Z-ADD     TASIAC        VIMP(I)
496000001227E   1C                   ENDIF
496100001227     C*
496200001227     C                   ENDSR
496300980706     C*------------------------------------------------------------------------*
496400980706     C* CALPAM - CALCOLA LA PROVVIGIONE ASSEGNO MITTENTE
496500980706     C*------------------------------------------------------------------------*
496600980706     C     CALPAM        BEGSR
496700980706     C*
496800980706     C* REIMPOSTA LE VARIABILI DI LAVORO
496900980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
497000011031     C                   Z-ADD     *ZEROS        TASPAM           21 8
497100980706     C*
497200980706     C* CALCOLA CONSEGNA PARTICOLARE
497300980706     C                   MOVEL     'W'           TASTC             1            *CODICE CONSEGNA
497400011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
497500011031     C                   Z-ADD     ETCCAS        TOTVAL           21 8          *VALORE CONTRASS.
497600980706     C                   EXSR      CALCP
497700980911     C                   Z-ADD     *ZEROS        TOTVAL
497800980706     C                   Z-ADD     TASCP         TASPAM
497900980706     C*
498000980706IF  1C     TASPAM        IFGT      *ZEROS
498100980706     C                   Z-ADD     15            I
498200980706     C                   Z-ADD     TASPAM        VIMP(I)
498300980706E   1C                   ENDIF
498400980706     C*
498500980706     C                   ENDSR
498600001227     C*------------------------------------------------------------------------*
498700001227     C* CAL054 - CALCOLA ADG SU C/ASSEGNO MITTENTE
498800001227     C*------------------------------------------------------------------------*
498900001227     C     CAL054        BEGSR
499000001227     C*
499100001227     C* REIMPOSTA LE VARIABILI DI LAVORO
499200011031     C                   Z-ADD     *ZEROS        TASADG           21 8
499300001227     C*
499400001227     C* CALCOLA L'A.D.G.
499500011031     C                   Z-ADD     VIMP(15)      WIMP             23 8
499600001227     C                   EXSR      ADGCAL
499700001227     C*
499800001227IF  1C     TASADG        IFGT      *ZEROS
499900001227     C                   Z-ADD     54            I
500000001227     C                   Z-ADD     TASADG        VIMP(I)
500100001227E   1C                   ENDIF
500200001227     C*
500300001227     C                   ENDSR
500400001227     C*------------------------------------------------------------------------*
500500001227     C* CAL071 - CALCOLA ISTAT SU ADG/CASSEGNO MITTENTE
500600001227     C*------------------------------------------------------------------------*
500700001227     C     CAL071        BEGSR
500800001227     C*
500900001227     C* REIMPOSTA LE VARIABILI DI LAVORO
501000011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
501100001227     C*
501200001227     C* CALCOLA L'ISTAT
501300011031     C                   Z-ADD     VIMP(54)      WIMP             23 8
501400001227     C                   EXSR      IACCAL
501500001227     C*
501600001227IF  1C     TASIAC        IFGT      *ZEROS
501700001227     C                   Z-ADD     71            I
501800001227     C                   Z-ADD     TASIAC        VIMP(I)
501900001227E   1C                   ENDIF
502000001227     C*
502100001227     C                   ENDSR
502200980706     C*------------------------------------------------------------------------*
502300980706     C* CALPAB - CALCOLA LA PROVVIGIONE ASSEGNO BARTOLINI
502400980706     C*------------------------------------------------------------------------*
502500980706     C     CALPAB        BEGSR
502600980706     C*
502700980706     C* REIMPOSTA LE VARIABILI DI LAVORO
502800980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
502900011031     C                   Z-ADD     *ZEROS        TASPAB           21 8
503000980706     C*
503100980706     C* CALCOLA CONSEGNA PARTICOLARE
503200980706     C                   MOVEL     'V'           TASTC             1            *CODICE CONSEGNA
503300011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
503400980911     C                   Z-ADD     ETCCAS        TOTVAL                         *VALORE CONTRASS.
503500980706     C                   EXSR      CALCP
503600980911     C                   Z-ADD     *ZEROS        TOTVAL
503700980706     C                   Z-ADD     TASCP         TASPAB
503800980706     C*
503900980706IF  1C     TASPAB        IFGT      *ZEROS
504000980706     C                   Z-ADD     16            I
504100980706     C                   Z-ADD     TASPAB        VIMP(I)
504200980706E   1C                   ENDIF
504300980706     C*
504400980706     C                   ENDSR
504500001227     C*------------------------------------------------------------------------*
504600001227     C* CAL055 - CALCOLA ADG SU C/ASSEGNO BARTOLINI
504700001227     C*------------------------------------------------------------------------*
504800001227     C     CAL055        BEGSR
504900001227     C*
505000001227     C* REIMPOSTA LE VARIABILI DI LAVORO
505100011031     C                   Z-ADD     *ZEROS        TASADG           21 8
505200001227     C*
505300001227     C* CALCOLA L'A.D.G.
505400011031     C                   Z-ADD     VIMP(16)      WIMP             23 8
505500001227     C                   EXSR      ADGCAL
505600001227     C*
505700001227IF  1C     TASADG        IFGT      *ZEROS
505800001227     C                   Z-ADD     55            I
505900001227     C                   Z-ADD     TASADG        VIMP(I)
506000001227E   1C                   ENDIF
506100001227     C*
506200001227     C                   ENDSR
506300001227     C*------------------------------------------------------------------------*
506400001227     C* CAL072 - CALCOLA ISTAT SU ADG/CASSEGNO BARTOLINI
506500001227     C*------------------------------------------------------------------------*
506600001227     C     CAL072        BEGSR
506700001227     C*
506800001227     C* REIMPOSTA LE VARIABILI DI LAVORO
506900011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
507000001227     C*
507100001227     C* CALCOLA L'ISTAT
507200011031     C                   Z-ADD     VIMP(55)      WIMP             23 8
507300001227     C                   EXSR      IACCAL
507400001227     C*
507500001227IF  1C     TASIAC        IFGT      *ZEROS
507600001227     C                   Z-ADD     72            I
507700001227     C                   Z-ADD     TASIAC        VIMP(I)
507800001227E   1C                   ENDIF
507900001227     C*
508000001227     C                   ENDSR
508100980706     C*------------------------------------------------------------------------*
508200980706     C* CALDIF - CALCOLA IL DIRITTO FISSO
508300980706     C*------------------------------------------------------------------------*
508400980706     C     CALDIF        BEGSR
508500980706     C*
508600980706     C* REIMPOSTA LE VARIABILI DI LAVORO
508700980804     C                   MOVEL     'S'           WCAR                           *NO UTILIZZO TAR.CAR
508800011031     C                   Z-ADD     *ZEROS        TASDIF           21 8
508900980706     C*
509000980706     C* CALCOLA CONSEGNA PARTICOLARE
509100980706     C                   MOVEL     'H'           TASTC             1            *CODICE CONSEGNA
509200011031     C                   Z-ADD     *ZEROS        TASCP            21 8          *IMPORTO CONSEGNA
509300980706     C                   EXSR      CALCP
509400980706     C                   Z-ADD     TASCP         TASDIF
509500980706     C*
509600980706IF  1C     TASDIF        IFGT      *ZEROS
509700980706     C                   Z-ADD     2             I
509800980706     C                   Z-ADD     TASDIF        VIMP(I)
509900980706E   1C                   ENDIF
510000980706     C*
510100980706     C                   ENDSR
510200980706     C*------------------------------------------------------------------------*
510300980706     C* CALISD - CALCOLA L'ISTAT SUL DIRITTO FISSO
510400980706     C*------------------------------------------------------------------------*
510500980706     C     CALISD        BEGSR
510600980706     C*
510700980706     C* REIMPOSTA LE VARIABILI DI LAVORO
510800011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
510900980706     C*
511000980706     C* CALCOLA L'ISTAT
511100011031     C                   Z-ADD     TASDIF        WIMP             23 8
511200980706     C                   EXSR      IACCAL
511300980706     C*
511400980706IF  1C     TASIAC        IFGT      *ZEROS
511500980706     C                   Z-ADD     25            I
511600980706     C                   Z-ADD     TASIAC        VIMP(I)
511700980706E   1C                   ENDIF
511800980706     C*
511900980706     C                   ENDSR
512000980706     C*------------------------------------------------------------------------*
512100980706     C* CALIAD - CALCOLA L'ISTAT SULL'ADG DEL DIRITTO FISSO
512200980706     C*------------------------------------------------------------------------*
512300980706     C     CALIAD        BEGSR
512400980706     C*
512500980706     C* REIMPOSTA LE VARIABILI DI LAVORO
512600011031     C                   Z-ADD     *ZEROS        TASIAC           21 8
512700980706     C*
512800980706     C* CALCOLA L'ISTAT
512900011031     C                   Z-ADD     VIMP(22)      WIMP             23 8
513000980706     C                   EXSR      IACCAL
513100980706     C*
513200980706IF  1C     TASIAC        IFGT      *ZEROS
513300980706     C                   Z-ADD     28            I
513400980706     C                   Z-ADD     TASIAC        VIMP(I)
513500980706E   1C                   ENDIF
513600980706     C*
513700980706     C                   ENDSR
513800970306     C*------------------------------------------------------------------------*
513900970306     C* CALCP - CALCOLA CONSEGNA PARTICOLARE GENERICA
514000970306     C*------------------------------------------------------------------------*
514100970306     C     CALCP         BEGSR
514200970306     C*
514300980506     C* CERCA LA TARIFFA PARTICOLARE
514400980506     C                   EXSR      CERTPT
514500980706     C*
514600980706     C* CALCOLA RPV, ARROTONDAMENTI E PESO TASSABILE
514700980706IF  1C     $TPT          IFEQ      '1'                                          *TARIFFA TROVATA
514800980707     C                   MOVEL     TPTTPG        WTPG
514900980707     C                   MOVEL     TPTRPV        WRPV
515000980707     C                   CLEAR                   WDET
515100980707     C                   Z-ADD     TPTARL        WARL
515200980707     C                   Z-ADD     TPTARF        WARF
515300980707     C                   Z-ADD     TPTARO        WARO
515400980706     C                   EXSR      CALCOL
515500040826     c                   select
515600040826IF  2C                   when      tpttpg = 'T'                                 *A TRASPORTO
515700980706     C                   Z-ADD     TOTTRA        ISOPES                         *TRASPORTO
515800040826IF  2C                   when      tpttpg = 'V'                                 *A TRASPORTO
515900980911     C                   Z-ADD     TOTVAL        ISOPES                         *TRASPORTO
516000040826IF  2C                   when      tpttpg = '5'                                 *A TRASPORTO
516100040826     C                   Z-ADD     TOTqta        ISOPES                         *TRASPORTO
516200040826E   2C                   ENDsl
516300980706     C*
516400980706     C* CALCOLA LA CONSEGNA PARTICOLARE
516500970306     C                   Z-ADD     ISOPES        TPDPES                         *PESO
516600970306     C                   EXSR      CALACD
516700980706     C                   EXSR      CALTRA                                       *CALCOLA % A TRASPOR
516800970306     C                   Z-ADD     VARIA         TASCP
516900970721     C*
517000970721     C* MEMORIZZA IL FLAG "APPLICAZIONE TARIFFA INOLTRO" DELLO SCAGLIONE DEL
517100970721     C* CODICE TASSAZIONE DEL DETTAGLIO TARIFFARIO
517200970721     C                   MOVEL     AAIN          DEPAIN
517300980506E   1C                   ENDIF
517400970306     C*
517500970306     C                   ENDSR
517600980506     C*------------------------------------------------------------------------*
517700980506     C* CERTPT - CERCA TARIFFA PARTICOLARE
517800980506     C*------------------------------------------------------------------------*
517900980506     C     CERTPT        BEGSR
518000980506     C*
518100980506     C* REPERISCE IL CODICE PARTICOLARITA'
518200980506     C                   MOVEL     '0'           $TPT                           *TARIFFA NON TROVATA
518300980506     C                   MOVEL     TASTC         KTPFTC                         *CAMPO CHIAVE
518400980506     C                   Z-ADD     1             I
518500980506     C                   SETOFF                                           28
518600990819     C                   MOVEL     KTPFTC        A1                1
518700990819     C     A1            LOOKUP    CP(I)                                  28
518800980506IF  1C     *IN28         IFEQ      *ON                                          *CODICE ESISTENTE
518900980506     C     I             OCCUR     CPIDS                                        *TARIFFA DI CARTELLO
519000980506     C*
519100980506     C* RICERCA LA CONSEGNA PARTICOLARE NELLE TARIFFE DEL CLIENTE
519200980506     C                   Z-ADD     D00KSC        KTPKSC
519300980506     C                   Z-ADD     D00CTR        KTPCTR
519400980506     C                   Z-ADD     D00PRG        KTPPRG
519500990819     C     KEYTPT        SETLL     TITPT01L                               99
519600990819     C     KEYTPT        READE     TITPT01L                               99
519700980506DO  2C     *IN99         DOWEQ     *OFF
519800980506     C     $TPT          ANDEQ     '0'
519900991125     C*
520000991125     C* CONTROLLA SE CONVERTIRE I CAMPI DI TITPT
520100011026IF  3C     D00DIV        IFNE      WDIVCE
520200011019     C                   MOVEL     D00DIV        WDVI
520300991125     C                   EXSR      CNVTPT
520400991125E   3C                   ENDIF
520500991125     C*
520600980506IF  3C     TPTATB        IFEQ      *BLANKS
520700980506     C                   MOVEL     '1'           $TPT                           *TARIFFA TROVATA
520800980506E   3C                   ENDIF
520900980506     C*
521000980506     C* LETTURA SUCESSIVA TARIFFE
521100980506IF  3C     $TPT          IFEQ      '0'                                          *TARIFFA NON TROVATA
521200990819     C     KEYTPT        READE     TITPT01L                               99
521300980506E   3C                   ENDIF
521400980506E   2C                   ENDDO                                                  *FINE TAR.PART.KSC
521500980506     C*
521600980506     C* TARIFFA CONSEGNE PARTICOLARI NON TROVATA NELLE TARIFFE DEL CLIENTE
521700980506     C* RICERCA NELLE TARIFFE DI CARTELLO
521800980506IF  2C     $TPT          IFEQ      '0'                                          *TARIFFA KSC NO TROV
521900980506IF  3C     CP88          IFNE      'NO'                                         *C'E' TARIFFA CARTEL
522000980506     C     WCAR          ANDEQ     'S'                                          *SI UTILIZZO TAR.CAR
522100980506     C                   MOVEL     '1'           $TPT                           *TARIFFA TROVATA
522200980506     C                   Z-ADD     CARKSC        TPTKSC                         *CHIAVE CARTELLO
522300980506     C                   Z-ADD     CARCTR        TPTCTR
522400980506     C                   Z-ADD     CARPRG        TPTPRG
522500980506     C                   MOVEL     KTPFTC        TPTFTC
522600980506     C                   MOVEL     TTPG8P        TPTTPG                         *CAMPI CARTELLO
522700980506     C                   Z-ADD     TARF8P        TPTARF
522800980506     C                   Z-ADD     TARL8P        TPTARL
522900980506     C                   Z-ADD     TARO8P        TPTARO
523000980506     C                   Z-ADD     TRPV8P        TPTRPV
523100980506E   3C                   ENDIF
523200980506E   2C                   ENDIF
523300980506E   1C                   ENDIF
523400980506     C*
523500980506     C                   ENDSR
523600970306     C*------------------------------------------------------------------------*
523700970306     C* CALACD - CALCOLA TARIFFA PARTICOLARE GENERICA
523800970306     C*------------------------------------------------------------------------*
523900970306     C     CALACD        BEGSR
524000980706     C*
524100980706     C* AZZERA VARIABILI DI COMODO -SOLO DI QUESTO PGM-
524200980706     C                   MOVEL     *BLANKS       PTPG              1            *TIPO APPLICAZIONE
524300011031     C                   Z-ADD     *ZEROS        PITR             21 8          *IMPORTO TARIFFA
524400011031     C                   Z-ADD     *ZEROS        PVARI            23 8          *VARIA NON ARROTONDA
524500011031     C                   Z-ADD     *ZEROS        PVARIA           23 8          *VAIRA ARROTONDATA
524600970306     C*
524700970306     C* REIMPOSTA LE VARIABILI DI LAVORO
524800011031     C                   Z-ADD     *ZEROS        VARIA            23 8
524900970306DO  1C     1             DO        10            I
525000970306     C     I             OCCUR     ACDS
525100970306     C                   CLEAR                   ACDS
525200970306E   1C                   ENDDO
525300970306     C*
525400970306     C* IMPOSTA LA CHIAVE PER LA RICERCA SCAGLIONE NEL DETTAGLIO TARIFFARIO
525500970306     C                   Z-ADD     TPTKSC        KDPKSC                         *CLIENTE
525600970306     C                   Z-ADD     TPTCTR        KDPCTR                         *TARIFFA
525700970306     C                   Z-ADD     TPTPRG        KDPPRG                         *PROGRESSIVO
525800970306     C                   MOVEL     TPTFTC        KDPFTC                         *CONSEGNA PARTICOLAR
525900970306     C                   MOVEL     *BLANKS       KDPCTS                         *CODICE TASSAZIONE
526000970306     C*
526100970306     C* PER ALCUNE CONSEGNE PARTICOLARI SONO INDICATI NELLA TABELLA CONSEGNE
526200970306     C* PARTICOLARI I CODICI TASSAZIONE DA UTILIZZARE NELLA RICERCA TARIFFA
526300970306     C                   SETOFF                                           28
526400970306     C                   Z-ADD     1             I
526500970306     C     CCTS(T)       LOOKUP    CTS(I)                                 28
526600970306     C     I             OCCUR     DSCT
526700970306IF  1C     §CTNAR        IFNE      *BLANKS                                      *TARIFFA ESTERO
526800971204     C     DLPFL1        ORNE      *BLANKS                                      *ESTERO
526900970306IF  2C     TCTE8P        IFNE      *BLANKS                                      *SE INDICATO
527000970306     C                   MOVEL     TCTE8P        KDPCTS                         *CODICE 'EE' FISSO
527100970306E   2C                   ENDIF
527200970306X   1C                   ELSE                                                   *TARIFFA ITALIA
527300970306IF  2C     TCTS8P        IFNE      *BLANKS                                      *SE INDICATO
527400970306     C                   MOVEL     TCTS8P        KDPCTS                         *CODICE 'IT' FISSO
527500970306E   2C                   ENDIF
527600970306E   1C                   ENDIF
527700970306     C*
527800970306     C* CODICE TASSAZIONE NON FISSO, RICERCA QUAL'E' PER REPERIRE LA TARIFFA
527900980921     C                   SETOFF                                       98
528000980921IF  1C     KDPCTS        IFNE      *BLANKS
528100980921     C*
528200091123     C                   IF        KDPKSC = CARKSC  and                         *LEGGERE CARTELLO
528300091123     c                             %subst(d00op0: 1: 1)='O'
528400990819     C     KEYTPD        SETLL     TIOPD02L                               98     DA OFFERTA
528500980921X   2C                   ELSE                                                   *LEGGERE CLIENTE
528600990819     C     KEYTPD        SETLL     TITPD02L                               98     DA TARIFFA
528700980921E   2C                   ENDIF
528800980921     C*
528900980921X   1C                   ELSE                                                   *KDPCTS = BLANKS
529000980921     C*
529100970306     C                   MOVEL     CCTS(T)       KDPCTS                         *CODCE TASSAZIONE
529200091123     C                   IF        KDPKSC = CARKSC  and                         *LEGGERE CARTELLO
529300091123     c                             %subst(d00op0: 1: 1)='O'
529400990819     C     KEYTPD        SETLL     TIOPD02L                               98     DA OFFERTA
529500970306X   2C                   ELSE                                                   *LEGGERE CLIENTE
529600990819     C     KEYTPD        SETLL     TITPD02L                               98     DA TARIFFA
529700970306E   2C                   ENDIF
529800970306     C*
529900970306     C* CERCA LA TARIFFA CON IL CODICE TASSAZIONE REGIONE
530000980703IF  2C     *IN98         IFEQ      *OFF                                         *TARIFFA NON TROVATA
530100970306     C                   MOVEL     §CTRAP        KDPCTS                         *COD.TASS.REGIONE
530200091123     C                   IF        KDPKSC = CARKSC  and                         *LEGGERE CARTELLO
530300091123     c                             %subst(d00op0: 1: 1)='O'
530400990819     C     KEYTPD        SETLL     TIOPD02L                               98     DA OFFERTA
530500970306X   3C                   ELSE                                                   *LEGGERE CLIENTE
530600990819     C     KEYTPD        SETLL     TITPD02L                               98     DA TARIFFA
530700970306E   3C                   ENDIF
530800970306     C*
530900970306     C* CERCA LA TARIFFA CON IL CODICE TASSAZIONE NAZIONE
531000000403IF  3C     *IN98         IFEQ      *OFF                                         *TARIFFA NON TROVATA
531100000403IF  4C     §CTNAR        IFEQ      *BLANKS
531200000403     C                   MOVEL     'IT'          KDPCTS                         *COD.TASS.NAZIONE
531300000403X   4C                   ELSE
531400000403     C                   MOVEL     'EE'          KDPCTS                         *COD.TASS.NAZIONE
531500000403E   4C                   ENDIF
531600091123     C                   IF        KDPKSC = CARKSC  and                         *LEGGERE CARTELLO
531700091123     c                             %subst(d00op0: 1: 1)='O'
531800000403     C     KEYTPD        SETLL     TIOPD02L                               98     DA OFFERTA
531900000403X   4C                   ELSE                                                   *LEGGERE CLIENTE
532000000403     C     KEYTPD        SETLL     TITPD02L                               98     DA TARIFFA
532100000403E   4C                   ENDIF
532200000403E   3C                   ENDIF
532300970306E   2C                   ENDIF
532400970306E   1C                   ENDIF
532500000403     C*
532600970306     C* LEGGE LA TARIFFA
532700970306     C                   MOVEL     '0'           $TPD                           *TARIFFA NO ESISTENT
532800970306     C                   Z-ADD     *ZEROS        I
532900091123     C                   IF        KDPKSC = CARKSC  and                         *LEGGERE CARTELLO
533000091123     c                             %subst(d00op0: 1: 1)='O'
533100990819     C     KEYTPD        READE     TIOPD02L                               98     DA OFFERTA
533200970306X   1C                   ELSE                                                   *LEGGERE CLIENTE
533300990819     C     KEYTPD        READE     TITPD02L                               98     DA TARIFFA
533400970306E   1C                   ENDIF
533500970306DO  1C     *IN98         DOWEQ     *OFF
533600991125     C*
533700991125     C* CONTROLLA SE CONVERTIRE I CAMPI DI TITPD
533800091123     C                   IF        KDPKSC = CARKSC  and                         *LEGGERE CARTELLO
533900091123     c                             %subst(d00op0: 1: 1)='O'
534000011023     C                   MOVEL     CARDIV        WDIV
534100011023     C                   ELSE
534200011019     C                   MOVEL     D00DIV        WDIV
534300011023     C                   ENDIF
534400011026IF  2C     WDIV          IFNE      WDIVCE
534500011019     C                   MOVEL     WDIV          WDVI
534600991125     C                   EXSR      CNVTPD
534700991125E   2C                   ENDIF
534800991125     C*
534900970306     C                   MOVEL     '1'           $TPD                           *TARIFFA ESISTENTE
535000970306     C                   ADD       1             I
535100970306     C     I             OCCUR     ACDS
535200970306     C                   Z-ADD     TPDSGL        ASGL
535300970306     C                   Z-ADD     TPDITR        AITR
535400970306     C                   Z-ADD     TPDMIN        AMIN
535500970306     C                   Z-ADD     TPDMAX        AMAX
535600970306     C                   MOVEL     TPDAIN        AAIN
535700091123     C                   IF        KDPKSC = CARKSC  and                         *LEGGERE CARTELLO
535800091123     c                             %subst(d00op0: 1: 1)='O'
535900990819     C     KEYTPD        READE     TIOPD02L                               98     DA OFFERTA
536000970306X   2C                   ELSE                                                   *LEGGERE CLIENTE
536100990819     C     KEYTPD        READE     TITPD02L                               98     DA TARIFFA
536200970306E   2C                   ENDIF
536300970306E   1C                   ENDDO
536400970306     C*
536500970306     C* SE TARIFFA PARTICOLARE ESISTENTE, RICERCA LO SCAGLIONE
536600970306IF  1C     $TPD          IFEQ      '1'
536700970306DO  2C     1             DO        10            I
536800970306     C     I             OCCUR     ACDS
536900970306IF  3C     ASGL          IFGE      TPDPES                                       *SCAGLIONE TROVATO
537000970306     C                   LEAVE
537100970306E   3C                   ENDIF
537200970306E   2C                   ENDDO
537300970306     C*
537400970306     C* SCAGLIONE NON TROVATO, MANCA TARIFFA
537500970306IF  2C     ASGL          IFLT      TPDPES
537600001218     C                   MOVEL     MSG(26)       DMSG26
537700001218     C                   MOVEL     TPTFTC        DM26A
537800001218     C                   MOVEL     DMSG26        $MSG
537900970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
538000970306E   2C                   ENDIF
538100970306     C*
538200970306     C* CALCOLO DEL MINIMO TASSABILE
538300980703IF  2C     WERR          IFEQ      'N'                                          *NO ERRORE
538400980703IF  3C     ASGL          IFLE      MINTAS                                       *SGL<=MIN.TASS.STD.
538500970306     C                   Z-ADD     MINTAS        TPDPES                         *PESO=MIN.TASS.STD.
538600980703E   3C                   ENDIF
538700980703IF  3C     TPDPES        IFLE      MINTAS                                       *PESO<=MIN.TASS.STD.
538800970306     C                   Z-ADD     MINTAS        TPDPES                         *PESO=MIN.TASS.STD.
538900980703E   3C                   ENDIF
539000970306     C*
539100970306     C* CALCOLA IL PESO TASSABILE IN QUINTALI -PER TARIFFA A QUINTALI-
539200011031     C                   Z-ADD     *ZEROS        TPDQLI           23 8
539300980707IF  3C     TPTTPG        IFEQ      '0'
539400980707     C     TPTTPG        OREQ      '4'
539500970306     C     TPDPES        DIV       100           TPDQLI
539600980703X   3C                   ELSE
539700970306     C                   Z-ADD     TPDPES        TPDQLI
539800980703E   3C                   ENDIF
539900970306     C     AITR          MULT(H)   TPDQLI        VARIA
540000980703IF  3C     VARIA         IFGT      *ZEROS
540100980703IF  4C     TPTTPG        IFEQ      'V'                                          *A VALORE O
540200980506     C     TPTTPG        OREQ      'T'                                          *A TRASPORTO
540300970306     C     VARIA         DIV(H)    100           VARIA
540400980703E   4C                   ENDIF
540500980703E   3C                   ENDIF
540600980703E   2C                   ENDIF
540700980706     C*
540800980706     C* IMPOSTA VARIABILI DI COMODO -SOLO DI QUESTO PGM-
540900980706     C                   MOVEL     TPTTPG        PTPG                           *TIPO APPLICAZIONE
541000980706     C                   Z-ADD     TPDITR        PITR                           *IMPORTO TARIFFA
541100980706     C                   Z-ADD     VARIA         PVARI                          *VARIA NON ARROTONDA
541200970306     C* MINIMO TARIFFA
541300980703IF  2C     AMIN          IFGT      VARIA
541400970306     C                   Z-ADD     AMIN          VARIA
541500091026     c                   clear                   tpg
541600980703E   2C                   ENDIF
541700970306     C* MASSIMO TARIFFA
541800980703IF  2C     AMAX          IFGT      *ZEROS
541900980703IF  3C     AMAX          IFLT      VARIA
542000970306     C                   Z-ADD     AMAX          VARIA
542100091026     c                   clear                   tpg
542200980703E   3C                   ENDIF
542300980703E   2C                   ENDIF
542400980706     C*
542500980707     C* IMPOSTA VARIABILI DI COMODO -SOLO DI QUESTO PGM-
542600980706     C                   Z-ADD     VARIA         PVARIA                         *VARIA ARROTONDATA
542700970306E   1C                   ENDIF
542800970306     C*
542900970306     C                   ENDSR
543000980706     C*------------------------------------------------------------------------*
543100980706     C* CALTRA - CALCOLA CONSEGNA PARTICOLARE SUL TOTALE IMPORTO TRASPORTO
543200980706     C*------------------------------------------------------------------------*
543300980706     C     CALTRA        BEGSR
543400980706     C*---
543500980706     C* INPUT
543600980706     C* . PTPG ->   TIPO APPLICAZIONE
543700980706     C* . PITR ->   IMPORTO TARIFFA CALCOLATA
543800980706     C* . PVARI ->  VARIA CALCOLATA, MA NON ARROTONDATA
543900980706     C* . PVARIA -> VARIA CALCOLATA, E ARROTONDATA
544000980706     C*---
544100980706     C* REIMPOSTA LE VARIABILI DI LAVORO
544200011031     C                   Z-ADD     *ZEROS        TOTVAR           21 8          *VARIA
544300980706     C*
544400980706     C* SE TIPO APPLICAZIONE T' E HA CALCOLATO QUALCOSA
544500980706IF  1C     PTPG          IFEQ      'T'                                          *TARIFFA A TRASPORTO
544600980706     C     PITR          ANDGT     *ZEROS                                       *TARIFFA DA CALCOLAR
544700111006     C     PVARI         ANDGT     *ZEROS                                       *TARIFFA DA CALCOLAR
544800980706     C* PORTO
544900980706IF  2C     VIMP(1)       IFGT      *ZEROS
545000011031     C                   Z-ADD     VIMP(1)       IMPOR            21 8          *IMPORTO VOCE
545100980706     C                   Z-ADD     VP(1)         PERCEN            9 5          *% INCIDENZA
545200011031     C                   Z-ADD     *ZEROS        VOCE             21 8
545300980706     C                   EXSR      CALTR2
545400980706     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
545500980706E   2C                   ENDIF
545600980706     C* INOLTRO
545700980706IF  2C     VIMP(6)       IFGT      *ZEROS
545800011031     C                   Z-ADD     VIMP(6)       IMPOR            21 8          *IMPORTO VOCE
545900980706     C                   Z-ADD     VP(6)         PERCEN            9 5          *% INCIDENZA
546000011031     C                   Z-ADD     *ZEROS        VOCE             21 8
546100980706     C                   EXSR      CALTR2
546200980706     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
546300980706E   2C                   ENDIF
546400980706     C* LOCALITA' DISAGIATE
546500980706IF  2C     VIMP(35)      IFGT      *ZEROS
546600011031     C                   Z-ADD     VIMP(35)      IMPOR            21 8          *IMPORTO VOCE
546700980706     C                   Z-ADD     VP(35)        PERCEN            9 5          *% INCIDENZA
546800011031     C                   Z-ADD     *ZEROS        VOCE             21 8
546900980706     C                   EXSR      CALTR2
547000980706     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
547100980706E   2C                   ENDIF
547200980706     C* ESPRESSO PORTO
547300980706IF  2C     VIMP(7)       IFGT      *ZEROS
547400011031     C                   Z-ADD     VIMP(7)       IMPOR            21 8          *IMPORTO VOCE
547500980706     C                   Z-ADD     VP(7)         PERCEN            9 5          *% INCIDENZA
547600011031     C                   Z-ADD     *ZEROS        VOCE             21 8
547700980706     C                   EXSR      CALTR2
547800980706     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
547900980706E   2C                   ENDIF
548000980706     C* ESPRESSO INOLTRO
548100980706IF  2C     VIMP(36)      IFGT      *ZEROS
548200011031     C                   Z-ADD     VIMP(36)      IMPOR            21 8          *IMPORTO VOCE
548300980706     C                   Z-ADD     VP(36)        PERCEN            9 5          *% INCIDENZA
548400011031     C                   Z-ADD     *ZEROS        VOCE             21 8
548500980706     C                   EXSR      CALTR2
548600980706     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
548700980706E   2C                   ENDIF
548800980706     C* ESPRESSO LOCALITA' DISAGIATE
548900980706IF  2C     VIMP(39)      IFGT      *ZEROS
549000011031     C                   Z-ADD     VIMP(39)      IMPOR            21 8          *IMPORTO VOCE
549100980706     C                   Z-ADD     VP(39)        PERCEN            9 5          *% INCIDENZA
549200011031     C                   Z-ADD     *ZEROS        VOCE             21 8
549300980706     C                   EXSR      CALTR2
549400980706     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
549500980706E   2C                   ENDIF
549600011030     C* CONSEGNA ISOLA
549700011030IF  2C     VIMP(80)      IFGT      *ZEROS
549800011031     C                   Z-ADD     VIMP(80)      IMPOR            21 8          *IMPORTO VOCE
549900011030     C                   Z-ADD     VP(80)        PERCEN            9 5          *% INCIDENZA
550000011031     C                   Z-ADD     *ZEROS        VOCE             21 8
550100011030     C                   EXSR      CALTR2
550200011030     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
550300011030E   2C                   ENDIF
550400011030     C* ESPRESSO CONSEGNA ISOLA
550500011030IF  2C     VIMP(81)      IFGT      *ZEROS
550600011031     C                   Z-ADD     VIMP(81)      IMPOR            21 8          *IMPORTO VOCE
550700011030     C                   Z-ADD     VP(81)        PERCEN            9 5          *% INCIDENZA
550800011031     C                   Z-ADD     *ZEROS        VOCE             21 8
550900011030     C                   EXSR      CALTR2
551000011030     C                   ADD       VOCE          TOTVAR                         *VOCE CALCOLATA
551100011030E   2C                   ENDIF
551200011030     C*
551300980706     C                   Z-ADD     TOTVAR        VARIA                          *VOCE TOTALE
551400980706E   1C                   ENDIF
551500980706     C*
551600980706     C                   ENDSR
551700980706     C*------------------------------------------------------------------------*
551800980706     C* CALTR2 - ESEGUE CALCOLI VOCE CALCOLATA SU UNA % DEL TRASPORTO
551900980706     C*------------------------------------------------------------------------*
552000980706     C     CALTR2        BEGSR
552100980706     C*---
552200980706     C* INPUT
552300980706     C* . PVARI  ->  VARIA CALCOLATA, MA NON ARROTONDATA
552400980706     C* . PVARIA -> VARIA CALCOLATA, E ARROTONDATA
552500980706     C* . IMPOR  -> IMPORTO VOCE
552600980706     C* . PERCEN -> % INCIDENZA
552700980706     C* OUTPUT
552800980706     C* . VOCE   -> VOCE CALCOLATA
552900980706     C*---
553000980706     C* ES. DA CALCOLARE VARIA = 10% DEL TRASPORTO
553100980706     C*   . VARIA CALCOLATA CON ARROTONDAMENTI = 2.000
553200980706     C*   . VARIA CALCOLATA SENZA ARROTONDAMENTI = 1.600
553300980706     C*   . PORTO = 10.000                         INCIDENZA 100%
553400980706     C*   . INOLTRO = 5.000                        INCIDENZA   5%
553500980706     C*   . LOCALITA' DISAGIATA = 1.000            INCIDENZA  20%
553600980706     C*   . ESPRESSO PORTO = 300                   INCIDENZA  15%
553700980706     C*   . ESPRESSO INOLTRO = 150                 INCIDENZA 15% SU  5% = 0,75%
553800980706     C*   . ESPRESSO LOCALITA' DISAGIATA = 30      INCIDENZA 15% SU 20% = 3%
553900011030     C*   . CONSEGNA ISOLA               = 2.500   INCIDENZA  30%
554000011030     C*   . ESPRESSO CONSEGNA ISOLA      = 30      INCIDENZA 15% SU 30% = 4,5%
554100980706     C* LA SOMMA DI TUTTE LE VOCI DA LA VARIA
554200011030     C* Esempio -->
554300980706     C*---
554400980706     C* % "T" DA CALCOLARE SULLA LOCALITA' DISAGIATA
554500980706     C* 10% DI 1.000 = 100
554600980706     C*---
554700011031     C     IMPOR         MULT      PITR          N11              23 8
554800011031     C     N11           DIV       100           VOCPT            23 8          *VOCE % SUL TRASPORT
554900980706     C*---
555000980706     C* VOCE TENUTO CONTO DELL'ARROTONDAMENTO -->
555100980706     C* 100 : 1.600 = X : 2.000
555200980706     C* X = 125
555300980706     C*---
555400980706     C     VOCPT         MULT      PVARIA        VOCPT
555500011031     C     VOCPT         DIV       PVARI         VOCPTA           25 8          *VOCE AL 100%
555600980706     C*---
555700980706     C* VOCE TENUTO CONTO DELLA SUA % DI INCIDENZA ->
555800980706     C* 20% DI 125 = 25
555900980706     C*---
556000011031     C     VOCPTA        MULT      PERCEN        N19              25 8
556100011031     C     N19           DIV(H)    100           VOCE             21 8          *VOCE ALLA %INCIDENZ
556200980706     C*
556300980706     C                   ENDSR
556400970306     C*------------------------------------------------------------------------*
556500970306     C* CALTOT - CALCOLA IMPORTI SULLE % INCIDENZA E I RAGGUPPAMENTI DELTA
556600970306     C*------------------------------------------------------------------------*
556700970306     C     CALTOT        BEGSR
556800970306     C*
556900970306     C* REIMPOSTA LE VARIABILI DI LAVORO
557000970311     C                   Z-ADD     *ZEROS        IRDL                           *RAGGRUPPAMENTI DELT
557100970311     C                   Z-ADD     *ZEROS        VIM                            *IMPORTI DA SCRIVERE
557200011031     C                   Z-ADD     *ZEROS        WVVOCE           25 8          *IMPORTO VOCE
557300970306     C*
557400970311     C* CICLO PER TUTTE LE VOCI DI TASSAZIONE
557500970306DO  1C     1             DO        NVOCA         V                 6 0
557600970311IF  2C     VIMP(V)       IFGT      *ZEROS
557700970306     C*
557800970311     C* APPLICA LA % DI INCIDENZA
557900970306     C                   Z-ADD     *ZEROS        WVVOCE
558000970306IF  3C     VP(V)         IFLT      100
558100970311     C     VIMP(V)       DIV       100           WVVOCE
558200970306     C     WVVOCE        MULT      VP(V)         VIM(V)
558300970311X   3C                   ELSE
558400970311     C                   Z-ADD     VIMP(V)       VIM(V)
558500970306E   3C                   ENDIF
558600970306     C*
558700970306     C* SE LA VOCE E' DA INGLOBARE NEL PORTO FORZA IL RAGGRUPPAMENTO PORTO
558800970306IF  3C     VI(V)         IFEQ      'S'
558900970306     C                   Z-ADD     001           VR(V)
559000970318E   3C                   ENDIF
559100970306     C*
559200970311     C* IMPOSTA IL RAGGRUPPAMENTO DELTA
559300970306     C                   Z-ADD     VR(V)         R                 6 0          *INDICE  RAGGRUPP.TO
559400970306     C                   ADD       VIM(V)        IRDL(R)                        *IMPORTO RAGGRUPP.TO
559500970306E   2C                   ENDIF
559600970306E   1C                   ENDDO                                                  *FINE VOCI
559700970306     C*
559800970306     C                   ENDSR
559900970306     C*------------------------------------------------------------------------*
560000970306     C* SCRETW - SCRIVE SU FILE DI LAVORO I RICAVI
560100970306     C*------------------------------------------------------------------------*
560200970306     C     SCRETW        BEGSR
560300970311     C*
560400970311     C* CICLO PER TUTTI I RAGGRUPPAMENTI DELTA
560500970306DO  1C     1             DO        50            R
560600970306IF  2C     IRDL(R)       IFGT      *ZEROS
560700970306     C     R             OREQ      1
560800980909     C******************** MOVELUSER      KTWUSR           *UTENTE
560900980909     C                   MOVEL     JOB           KTWUSR                         *UTENTE
561000091123     C                   MOVEL     'S'           KTWDSF                         *DATI SEDE O FILIALE
561100980603     C                   MOVEL     D00CTO        KTWCTO                         *TARIFFA O OFFERTA
561200970306     C                   Z-ADD     D00KSC        KTWKSC                         *CLIENTE O VISITA
561300970306     C                   Z-ADD     D00CTR        KTWCTR                         *TARIFFA
561400970306     C                   Z-ADD     D00PRG        KTWPRG                         *PROGRESSIVO
561500970306     C                   Z-ADD     SSGL(S)       KTWSGL                         *SCAGLIONE
561600970306     C                   MOVEL     CCTS(T)       KTWCTS                         *CODICE TASSAZIONE
561700970306     C                   Z-ADD     R             KTWRDL                         *RAGGRUP.DELTA
561800010926     C     KEYETW        CHAIN     TEETW01L                           99
561900970306     C* MODIFICA
562000970306IF  3C     *IN99         IFEQ      *OFF
562100970306     C                   ADD       IRDL(R)       ETWRIC                         *RICAVO
562200970317IF  4C     D00TLA        IFEQ      'D'                                          *LANCIO DELTA
562300970317     C                   Z-ADD     *ZEROS        ETWCOM                         *AZZERA COMPETENZE
562400970317E   4C                   ENDIF
562500010926     C                   UPDATE    TEETW000
562600970306     C* INSERIMENTO
562700970306X   3C                   ELSE
562800970306     C                   MOVEL     KTWUSR        ETWUSR                         *UTENTE
562900980603     C                   MOVEL     KTWDSF        ETWDSF                         *DATI SEDE O FILIALE
563000970306     C                   MOVEL     KTWCTO        ETWCTO                         *TARIFFA O OFFERTA
563100970306     C                   Z-ADD     KTWKSC        ETWKSC                         *CLIENTE O VISITA
563200970306     C                   Z-ADD     KTWCTR        ETWCTR                         *TARIFFA
563300970306     C                   Z-ADD     KTWPRG        ETWPRG                         *PROGRESSIVO
563400970306     C                   Z-ADD     KTWSGL        ETWSGL                         *SCAGLIONE
563500970306     C                   MOVEL     KTWCTS        ETWCTS                         *CODICE TASSAZIONE
563600970306     C                   Z-ADD     KTWRDL        ETWRDL                         *RAGGRUP.DELTA
563700970306     C                   Z-ADD     IRDL(R)       ETWRIC                         *RICAVO
563800011031     C     CPPE(T)       DIV       100           WPERC            21 8
563900970306     C     WPERC         MULT      SPPE(S)       ETWPPE                         *% INCIDENZA
564000970306     C                   MOVEL     CRCT(T)       ETWRCT                         *RAGGRUPP.COD.TASSAZ
564100970306     C                   MOVEL     CRAP(T)       ETWRAP                         *REGIONE CODICE
564200970306     C                   MOVEL     CNAR(T)       ETWNAR                         *NAZIONE CODICE
564300970306     C                   Z-ADD     *ZEROS        ETWCOM                         *COMPETENZE
564400010926     C                   WRITE     TEETW000
564500970306E   3C                   ENDIF
564600970306E   2C                   ENDIF
564700970306E   1C                   ENDDO                                                  *FINE RAGGRUPPAMENTI
564800970306     C*
564900970306     C                   ENDSR
565000970306     C*------------------------------------------------------------------------*
565100970306     C* SCREWR - SCRIVE SU FILE DI LAVORO IL DETTAGLIO DEI RICAVI -SE IN DEBUG-
565200970306     C*------------------------------------------------------------------------*
565300970306     C     SCREWR        BEGSR
565400970311     C*
565500970311     C* CICLO PER TUTTE LE VOCI DI TASSAZIONE
565600970306DO  1C     1             DO        NVOCA         V
565700970306IF  2C     VIM(V)        IFGT      *ZEROS
565800980909     C******************** MOVELUSER      EWRUSR           *UTENTE
565900980909     C                   MOVEL     JOB           EWRUSR                         *UTENTE
566000091123     C                   MOVEL     'S'           EWRDSF                         *DATI SEDE O FILIALE
566100970306     C                   MOVEL     D00CTO        EWRCTO                         *TARIFFA O OFFERTA
566200970306     C                   Z-ADD     D00KSC        EWRKSC                         *CLIENTE
566300970306     C                   Z-ADD     D00CTR        EWRCTR                         *TARIFFA
566400970306     C                   Z-ADD     D00PRG        EWRPRG                         *PROGRESSIVO
566500970306     C                   MOVEL     CCTS(T)       EWRCTS                         *CODICE TASSAZIONE
566600970306     C                   Z-ADD     SSGL(S)       EWRSGL                         *SCAGLIONE
566700970306     C                   Z-ADD     V             EWRVOC                         *VOCE
566800970306     C                   Z-ADD     VIM(V)        EWRIMP                         *IMPORTO VOCE
566900970306     C                   Z-ADD     VR(V)         EWRRDL                         *RAGGRUPPAMENTO DELT
567000010926     C                   WRITE     TEETWR00
567100970306E   2C                   ENDIF
567200970306E   1C                   ENDDO                                                  *FINE VOCI
567300970306     C*
567400970306     C                   ENDSR
567500970306     C*------------------------------------------------------------------------*
567600970306     C* CANCWF - CANCELLA IL FILE DI LAVORO
567700970306     C*------------------------------------------------------------------------*
567800970306     C     CANCWF        BEGSR
567900970317     C*
568000010926     C* CANCELLA I RECORD DELL'UTENTE NEL FILE TEETW00F
568100970317IF  1C     D00TLA        IFEQ      'D'                                          *LANCIO DELTA
568200980909     C******************** MOVELUSER      KTWUSR
568300980909     C                   MOVEL     JOB           KTWUSR
568400010926     C     KE2ETW        SETLL     TEETW01L
568500010926     C     KE2ETW        READE     TEETW01L                               99
568600970317DO  2C     *IN99         DOWEQ     *OFF
568700010926     C                   DELETE    TEETW000
568800010926     C     KE2ETW        READE     TEETW01L                               99
568900970317E   2C                   ENDDO
569000970317E   1C                   ENDIF
569100970306     C*
569200010926     C* CANCELLA I RECORD DELL'UTENTE NEL FILE TEETWR0F
569300980909     C******************** MOVELUSER      KWRUSR
569400980909     C                   MOVEL     JOB           KWRUSR
569500010926     C     KEYEWR        SETLL     TEETWR1L
569600010926     C     KEYEWR        READE     TEETWR1L                               99
569700970306DO  1C     *IN99         DOWEQ     *OFF
569800010926     C                   DELETE    TEETWR00
569900010926     C     KEYEWR        READE     TEETWR1L                               99
570000970306E   1C                   ENDDO
570100970306     C*
570200970306     C                   ENDSR
570300970306     C*------------------------------------------------------------------------*
570400970306     C* CARTAB - CARICA LE TABELLE
570500970306     C*------------------------------------------------------------------------*
570600970306     C     CARTAB        BEGSR
570700970306     C*
570800970306     C* CARICAMENTO DATI VARI TASSAZIONE -UNO-
570900970306     C                   EXSR      CARQT1
571000970306     C*
571100970306     C* CARICAMENTO TIPI TARIFFA
571200970306     C                   EXSR      CARTR
571300970306     C*
571400970306     C* CARICAMENTO SIGLE VARIE
571500970306     C                   EXSR      CAR$2
571600970306     C*
571700970306     C* CARICAMENTO CODICI TASSAZIONE
571800970306     C                   EXSR      CARCT
571900970306     C*
572000970306     C* CARICAMENTO TIPI SERVIZIO
572100970306     C                   EXSR      CAR5E
572200970306     C*
572300970306     C* CARICAMENTO TARIFFE PARTICOLARI -DI CARTELLO-
572400970306     C                   EXSR      CARPAR
572500970306     C*
572600970306     C                   ENDSR
572700970306     C*------------------------------------------------------------------------*
572800970306     C* CARQT1 - CARICAMENTO TABELLA DATI VARI DI TASSAZIONE -UNO-
572900970306     C*------------------------------------------------------------------------*
573000970306     C     CARQT1        BEGSR
573100021202     C*
573200160310     C                   CLEAR                   TRULC7DS
573300160310     C                   EVAL      IC7TLA = 'L'
573400160310     c                   eval      ic7tfie='I'
573500160310     C                   CALL      'TRULC7R'
573600160310     C                   PARM                    TRULC7DS
573700160310     C                   IF        OC7ERR = 'E'
573800160310     C                   MOVEL     MSG(11)       $MSG
573900021202     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
574000021202     C                   ENDIF
574100970306     C*
574200970306     C                   ENDSR
574300970306     C*------------------------------------------------------------------------*
574400970306     C* CARTR - CARICAMENTO TIPI TARIFFA
574500970306     C*------------------------------------------------------------------------*
574600970306     C     CARTR         BEGSR
574700970306     C*
574800980506DO  1C     1             DO        15            I
574900970306     C     I             OCCUR     DSTR
575000970306     C                   CLEAR                   DSTR                           *AZZERAMENTO
575100970306E   1C                   ENDDO
575200980707     C                   MOVEL     *BLANKS       CTR
575300970306     C*
575400970306     C                   Z-ADD     *ZEROS        I
575500020906     C                   Z-ADD     1             KTBKUT
575600970306     C                   MOVEL     'TR'          KTBCOD
575700970306     C     KE2TAB        SETLL     TABEL00F
575800970306     C     KE2TAB        READE     TABEL00F                               98
575900970306IF  1C     *IN98         IFEQ      *ON
576000970306     C                   MOVEL     MSG(15)       $MSG
576100970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
576200970306X   1C                   ELSE
576300970306DO  2C     *IN98         DOWEQ     *OFF
576400970306IF  3C     TBLKEY        IFNE      *BLANKS
576500970312IF  4C     TBLFLG        IFEQ      *BLANKS
576600970306     C                   ADD       1             I
576700970306     C                   MOVEL     TBLKEY        CTR(I)
576800970306     C     I             OCCUR     DSTR
576900970306     C                   MOVEL     TBLUNI        DSTR
577000970312E   4C                   ENDIF
577100970312E   3C                   ENDIF
577200970306     C     KE2TAB        READE     TABEL00F                               98
577300970306E   2C                   ENDDO
577400970306E   1C                   ENDIF
577500970306     C*
577600970306     C                   ENDSR
577700970306     C*------------------------------------------------------------------------*
577800970306     C* CAR$2 - CARICAMENTO SIGLE VARIE
577900970306     C*------------------------------------------------------------------------*
578000970306     C     CAR$2         BEGSR
578100970306     C*
578200970306     C                   CLEAR                   DS$2                           *AZZERAMENTO
578300970306     C*
578400020906     C                   Z-ADD     1             KTBKUT
578500970306     C                   MOVEL     '$2'          KTBCOD
578600970306     C                   MOVEL(P)  '1'           KTBKEY
578700970306     C     KEYTAB        CHAIN     TABEL00F                           98
578800970306IF  1C     *IN98         IFEQ      *ON
578900970306     C                   MOVEL     MSG(16)       $MSG
579000970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
579100970306X   1C                   ELSE
579200970306     C                   MOVEL     TBLUNI        DS$2
579300970306E   1C                   ENDIF
579400970306     C*
579500970306     C                   ENDSR
579600970306     C*------------------------------------------------------------------------*
579700970306     C* CARCT - CARICAMENTO CODICI TASSAZIONE
579800970306     C*------------------------------------------------------------------------*
579900970306     C     CARCT         BEGSR
580000970306     C*
580100050103DO  1C     1             DO        600           I
580200970306     C     I             OCCUR     DSCT
580300970306     C                   CLEAR                   DSCT                           *AZZERAMENTO
580400970306E   1C                   ENDDO
580500970306     C*
580600970306     C                   Z-ADD     *ZEROS        I
580700020906     C                   Z-ADD     1             KTBKUT
580800970306     C                   MOVEL     'CT'          KTBCOD
580900970306     C     KE2TAB        SETLL     TABEL00F
581000970306     C     KE2TAB        READE     TABEL00F                               98
581100970306IF  1C     *IN98         IFEQ      *ON
581200970306     C                   MOVEL     MSG(18)       $MSG
581300970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
581400970306X   1C                   ELSE
581500970306DO  2C     *IN98         DOWEQ     *OFF
581600970306IF  3C     TBLKEY        IFNE      *BLANKS
581700970312IF  4C     TBLFLG        IFEQ      *BLANKS
581800970306     C                   ADD       1             I
581900970306     C                   MOVEL     TBLKEY        CTS(I)
582000970306     C     I             OCCUR     DSCT
582100970306     C                   MOVEL     TBLUNI        DSCT
582200970312IF  5C     §CTUTC        IFEQ      'N'                                          *DA NON CONSIDERARE
582300970312     C                   CLEAR                   DSCT                           *PULISCE
582400970312     C     I             SUB       1             I                              *TORNA INDIETRO DI 1
582500970312E   5C                   ENDIF
582600970312E   4C                   ENDIF
582700970312E   3C                   ENDIF
582800970306     C     KE2TAB        READE     TABEL00F                               98
582900970306E   2C                   ENDDO
583000970306E   1C                   ENDIF
583100970306     C*
583200970306     C                   ENDSR
583300970306     C*------------------------------------------------------------------------*
583400970306     C* CAR5E - CARICAMENTO TIPI SERVIZIO
583500970306     C*------------------------------------------------------------------------*
583600970306     C     CAR5E         BEGSR
583700970306     C*
583800970306DO  1C     1             DO        10            I
583900970306     C     I             OCCUR     DS5E
584000970306     C                   CLEAR                   DS5E                           *AZZERAMENTO
584100970306E   1C                   ENDDO
584200970306     C*
584300970306     C                   Z-ADD     *ZEROS        I
584400020906     C                   Z-ADD     1             KTBKUT
584500970306     C                   MOVEL     '5E'          KTBCOD
584600970306     C     KE2TAB        SETLL     TABEL00F
584700970306     C     KE2TAB        READE     TABEL00F                               98
584800970306IF  1C     *IN98         IFEQ      *ON
584900970306     C                   MOVEL     MSG(19)       $MSG
585000970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
585100970306X   1C                   ELSE
585200970306DO  2C     *IN98         DOWEQ     *OFF
585300970306IF  3C     TBLKEY        IFNE      *BLANKS
585400970312IF  4C     TBLFLG        IFEQ      *BLANKS
585500970306     C                   ADD       1             I
585600970306     C                   MOVEL     TBLKEY        TS(I)
585700970306     C     I             OCCUR     DS5E
585800970306     C                   MOVEL     TBLUNI        DS5E
585900970312E   4C                   ENDIF
586000970312E   3C                   ENDIF
586100970306     C     KE2TAB        READE     TABEL00F                               98
586200970306E   2C                   ENDDO
586300970306E   1C                   ENDIF
586400970306     C*
586500970306     C                   ENDSR
586600970306     C*------------------------------------------------------------------------*
586700970306     C* CARPAR - CARICAMENTO TARIFFE PARTICOLARI -DI CARTELLO-
586800970306     C*------------------------------------------------------------------------*
586900970306     C     CARPAR        BEGSR
587000970306     C*---
587100970306     C* MEMORIZZA I DATI ASSICURAZIONE X CONTO / ISOLA / CONSEGNE PARTICOLARI
587200970306     C* DELLA TARIFFA DI CARTELLO IN VIGORE ALLA DATA DI FATTURAZIONE
587300970306     C*---
587400160310     C                   Z-ADD     OC7KSCc       KSC1                           *IMPOSTA CARTELLO
587500160310     C                   Z-ADD     oc7ctrc       CTR1
587600160310     C***                Z-ADD     *ZEROS        CTR1
587700011019     C                   EXSR      CERTAM                                       *CERCA TARIFFA
587800970306     C*
587900970306     C* MEMORIZZA LA CHIAVE DELLA TARIFFA DI CARTELLO IN VIGORE
588000970306IF  1C     WERR          IFEQ      'N'                                          *NO ERRORE
588100970306     C                   Z-ADD     TAMKSC        CARKSC
588200970306     C                   Z-ADD     TAMCTR        CARCTR
588300970306     C                   Z-ADD     TAMPRG        CARPRG
588400011019     C*
588500011023     C* RECUPERA LA DIVISA DELLA TARIFFA DI CARTELLO
588600011023     C                   CLEAR                   DSTA01
588700011023     C                   MOVEL     TAMFLO        DSTA01
588800011023     C                   MOVEL     §TADIV        CARDIV            3            *DIVISA TARIFFA
588900970306     C*
589000970306     C* CARICA LE TARIFFE PARTICOLARI DI CARTELLO
589100970306     C                   EXSR      CAR1P
589200970306X   1C                   ELSE                                                   *ERRORE
589300970306     C                   MOVEL     MSG(22)       $MSG
589400970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
589500970306E   1C                   ENDIF
589600970306     C*
589700970306     C                   ENDSR
589800970306     C*------------------------------------------------------------------------*
589900970306     C* CERTAM - CERCA UN TARIFFA
590000970306     C*------------------------------------------------------------------------*
590100970306     C     CERTAM        BEGSR
590200970306     C*
590300970306     C* REIMPOSTA LE VARIABILI DI LAVORO
590400970306     C                   Z-ADD     *ZEROS        DEPKSC                         *CAMPI DEPOSITO
590500970306     C                   Z-ADD     *ZEROS        DEPPRG
590600970306     C                   Z-ADD     *ZEROS        DEPCTR
590700970306     C                   MOVEL     '0'           $TAR              1            TARIFFA IN DECORRENZ
590800970306     C                   MOVEL     '0'           $SCA              1            TARIFFA SCADUTA
590900970306     C                   MOVEL     'N'           WERR                           *NO ERRORE
591000970306     C*
591100970306     C                   Z-ADD     KSC1          KTMKSC                         *CLIENTE
591200970306     C                   Z-ADD     CTR1          KTMCTR                         *TARIFFA
591300091123     c                   if        %subst(d00op0: 1: 1)='O'
591400970306     C     KEYTAM        SETLL     TNOFM00L                               99
591500970306X   1C                   ELSE                                                   *TARIFFE
591600970306     C     KEYTAM        SETLL     TNTAM00L                               99
591700970306E   1C                   ENDIF
591800970306IF  1C     *IN99         IFEQ      *OFF                                         *NON TROVATA
591900970306     C                   MOVEL     'S'           WERR                           *MANCA TARIFFA
592000970306X   1C                   ELSE
592100091123     c                   if        %subst(d00op0: 1: 1)='O'
592200970306     C     KEYTAM        READE     TNOFM00L                               99
592300970306X   2C                   ELSE                                                   *TARIFFE
592400970306     C     KEYTAM        READE     TNTAM00L                               99
592500970306E   2C                   ENDIF
592600970306DO  2C     *IN99         DOWEQ     *OFF                                         *FINE FILE
592700970306     C     $TAR          ANDEQ     '0'                                          *TROVATA TAR.IN DEC.
592800970306     C     WERR          ANDEQ     'N'                                          *NO MANCA TARIFFA
592900970306IF  3C     TAMATB        IFEQ      *BLANKS                                      *NO ANNULLATE
593000970306IF  4C     D00DTC        IFGE      TAMDDT                                       TARIFFA IN DECORRENZ
593100970306     C     D00DTC        ANDLE     TAMDST
593200970306     C                   MOVEL     '1'           $TAR                           *TAR.IN DEC.TROVATA
593300970306     C                   Z-ADD     TAMKSC        DEPKSC                         *CHIAVE DEPOSITATA
593400970306     C                   Z-ADD     TAMCTR        DEPCTR
593500970306     C                   Z-ADD     TAMPRG        DEPPRG
593600970306X   4C                   ELSE                                                   TARIFFA NON IN DECOR
593700970306IF  5C     D00DTC        IFLT      TAMDDT                                       *TARIFFA DA DECORRER
593800970306IF  6C     $SCA          IFEQ      '0'                                          *NON ESISTE TAR.SCAD
593900970306     C                   MOVEL     'S'           WERR                           *MANCA TARIFFA
594000970306E   6C                   ENDIF
594100970306E   5C                   ENDIF
594200970306IF  5C     D00DTC        IFGT      TAMDST                                       *TARIFFA SCADUTA
594300970306     C                   MOVEL     '1'           $SCA                           *TAR.SCADUTA TROVATA
594400970306     C                   Z-ADD     TAMKSC        DEPKSC                         *CHIAVE DEPOSITATA
594500970306     C                   Z-ADD     TAMCTR        DEPCTR
594600970306     C                   Z-ADD     TAMPRG        DEPPRG
594700970306E   5C                   ENDIF
594800970306E   4C                   ENDIF
594900970306E   3C                   ENDIF
595000091123     c                   if        %subst(d00op0: 1: 1)='O'
595100970306     C     KEYTAM        READE     TNOFM00L                               99
595200970306X   3C                   ELSE                                                   *TARIFFE
595300970306     C     KEYTAM        READE     TNTAM00L                               99
595400970306E   3C                   ENDIF
595500970306E   2C                   ENDDO                                                  *FINE TARIFFE
595600970306E   1C                   ENDIF
595700970306     C                   Z-ADD     DEPKSC        TAMKSC                         *CHIAVE DEPOSITATA
595800970306     C                   Z-ADD     DEPCTR        TAMCTR
595900970306     C                   Z-ADD     DEPPRG        TAMPRG
596000970306     C*
596100970306     C                   ENDSR
596200970306     C*------------------------------------------------------------------------*
596300970306     C* CAR1P - CARICAMENTO TARIFFE PARTICOLARI DI CARTELLO
596400970306     C*------------------------------------------------------------------------*
596500970306     C     CAR1P         BEGSR
596600970306     C*
596700020104DO  1C     1             DO        50            I
596800970306     C     I             OCCUR     CPIDS
596900970306     C                   CLEAR                   CPIDS                          *AZZERAMENTO
597000970306     C     I             OCCUR     DS1P
597100970306     C                   CLEAR                   DS1P                           *AZZERAMENTO
597200970306E   1C                   ENDDO
597300970306     C                   MOVEL     *BLANKS       CP
597400970306     C*
597500970306     C* PER CIASCUNA TARIFFA PARTICOLARE IN TABELLA
597600970306     C                   Z-ADD     *ZEROS        I
597700020906     C                   Z-ADD     1             KTBKUT
597800970306     C                   MOVEL     '1P'          KTBCOD
597900970306     C     KE2TAB        SETLL     TABEL00F
598000970306     C     KE2TAB        READE     TABEL00F                               98
598100970306IF  1C     *IN98         IFEQ      *ON
598200970306     C                   MOVEL     MSG(20)       $MSG
598300970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
598400970306X   1C                   ELSE
598500970306DO  2C     *IN98         DOWEQ     *OFF
598600970306IF  3C     TBLKEY        IFNE      *BLANKS
598700970312IF  4C     TBLFLG        IFEQ      *BLANKS
598800970306     C                   MOVEL     TBLUNI        CAMPO            26
598900970306     C                   MOVE      CAMPO         WFCP              1
599000970312IF  5C     WFCP          IFEQ      'S'                                          *TARIFFE PART.DA FAT
599100970306     C                   ADD       1             I
599200970306     C                   MOVEL     TBLKEY        CP(I)
599300970306     C     I             OCCUR     DS1P
599400970306     C                   MOVEL     TBLUNI        DS1P
599500970306     C*
599600970306     C* CARICA LE TARIFFE PARTICOLARI DI CARTELLO
599700970306     C                   MOVEL     '0'           $TPT              1            *TAR.PARTIC.TROVATA
599800011023     C                   Z-ADD     TAMKSC        KTPKSC
599900970306     C                   Z-ADD     TAMCTR        KTPCTR
600000970306     C                   Z-ADD     TAMPRG        KTPPRG
600100970306     C                   MOVEL     CP(I)         KTPFTC
600200970311     C     I             OCCUR     CPIDS
600300091123     c                   if        %subst(d00op0: 1: 1)='O'
600400990819     C     KEYTPT        SETLL     TIOPT01L                               99
600500990819     C     KEYTPT        READE     TIOPT01L                               99
600600970312X   6C                   ELSE                                                   *TARIFFE
600700990819     C     KEYTPT        SETLL     TITPT01L                               99
600800990819     C     KEYTPT        READE     TITPT01L                               99
600900970312E   6C                   ENDIF
601000970312DO  6C     *IN99         DOWEQ     *OFF                                         *FINE FILE
601100970306     C     $TPT          ANDEQ     '0'                                          *TROVATA TARIFFA
601200011023     C*
601300011023     C                   MOVEL     CARDIV        WDIV
601400991125     C*
601500991125     C* CONTROLLA SE CONVERTIRE I CAMPI DI TITPT
601600011026IF  7C     WDIV          IFNE      WDIVCE
601700011023     C                   MOVEL     WDIV          WDVI
601800011023     C                   EXSR      CNVTPT
601900011023E   7C                   ENDIF
602000991125     C*
602100970312IF  7C     TPTATB        IFEQ      *BLANKS
602200970306     C                   MOVEL     '1'           $TPT                           *TAR.PARTIC.TROVATA
602300970306     C                   Z-ADD     TPTRPV        TRPV8P
602400970306     C                   Z-ADD     TPTARF        TARF8P
602500970306     C                   Z-ADD     TPTARL        TARL8P
602600970306     C                   Z-ADD     TPTARO        TARO8P
602700970306     C                   MOVEL     TPTTPG        TTPG8P
602800970306     C                   MOVEL     TPTAPL        TAPL8P
602900970306     C                   MOVEL     §1PCTS        TCTS8P
603000970306     C                   MOVEL     §1PCTE        TCTE8P
603100970312E   7C                   ENDIF
603200091123     c                   if        %subst(d00op0: 1: 1)='O'
603300990819     C     KEYTPT        READE     TIOPT01L                               99
603400970312X   7C                   ELSE                                                   *TARIFFE
603500990819     C     KEYTPT        READE     TITPT01L                               99
603600970312E   7C                   ENDIF
603700970312E   6C                   ENDDO                                                  *FINE TAR.PARTICOLAR
603800970312IF  6C     $TPT          IFEQ      '0'                                          *TARIFFA NON TROVATA
603900970306     C                   MOVEL     'NO'          CP88
604000970312E   6C                   ENDIF
604100970312E   5C                   ENDIF
604200970312E   4C                   ENDIF
604300970306E   3C                   ENDIF
604400970306     C     KE2TAB        READE     TABEL00F                               98
604500970306E   2C                   ENDDO
604600970306E   1C                   ENDIF
604700970306     C*
604800970306     C                   ENDSR
604900970715     C*------------------------------------------------------------------------*
605000970715     C* REPINP - RECUPERA INTRADAMENTO -PARTENZA-
605100970715     C*------------------------------------------------------------------------*
605200970715     C     REPINP        BEGSR
605300970715     C*
605400970715     C* TERMINAL PARTENZA
605500970715     C                   CLEAR                   DSLV55
605600970715     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
605700970715     C                   MOVEL     'P'           D55TPT                         *ECCEZIONE ARRIVO
605800970715     C                   Z-ADD     D00LNP        D55LIN                         *LINEA
605900970715     C                   Z-ADD     D00DTC        D55DRF                         *DATA RIFERIMENTO
606000970715     C                   CALL      'FNLV55R'
606100970715     C                   PARM                    DSLV55
606200970715IF  1C     D55ERR        IFNE      *BLANKS                                      *NON TROVATO
606300970715     C                   MOVEL     D55MSG        $MSG
606400970715     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
606500970715X   1C                   ELSE                                                   *TROVATO
606600970715     C                   Z-ADD     D55TFP        WTFP                           *TERMINAL PARTENZA
606700970715E   1C                   ENDIF
606800970715     C*
606900970715     C* CODICE TASSAZIONE PARTENZA
607000970715     C                   Z-ADD     D00LNP        KORFIL
607100970715     C     KEYORG        CHAIN     AZORG01L                           98
607200970715IF  1C     *IN98         IFEQ      *ON                                          *NON TROVATO
607300970715     C                   MOVEL     MSG(5)        $MSG
607400970715     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
607500970715X   1C                   ELSE                                                   *TROVATA
607600970721     C                   MOVEL     ORGCTS        WMCT                           *CTS MITTENTE
607700020729     C                   MOVEL     ORGDE3        OG143
607800020729     C*
607900020729     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
608000020729     C                   CLEAR                   DNTW
608100020729     C*
608200020729     C                   MOVEL(P)  'NTW'         KTECOD
608300020729     C                   MOVEL(P)  §OGNTW        KTEKE1
608400020729     C     KEYTBE        CHAIN     TNTBE01L
608500020729     C                   IF        %found(TNTBE01L)
608600020729     C                   MOVEL     TBEUNI        DNTW
608700020729     C                   ENDIF
608800020729     C*
608900020729     C                   MOVEL     §NTWFIE       DLPFL1                         *FLAG ITALIA/ESTERO
609000970715E   1C                   ENDIF
609100970715     C*
609200970715     C                   ENDSR
609300970306     C*------------------------------------------------------------------------*
609400970306     C* CARETC - CARICA I DATI TIPO -GENERALI-
609500970306     C*------------------------------------------------------------------------*
609600970306     C     CARETC        BEGSR
609700970306     C*
609800091123     C                   MOVEL     'S'           KTCDSF                         *DATI SEDE O FILIALE
609900970306     C                   MOVEL     D00CTO        KTCCTO                         *TARIFFA O OFFERTA
610000970306     C                   Z-ADD     D00KSC        KTCKSC                         *CLIENTE O VISITA
610100970306     C                   Z-ADD     D00CTR        KTCCTR                         *TARIFFA
610200970306     C                   Z-ADD     D00PRG        KTCPRG                         *PROGRESSIVO
610300970306     C                   Z-ADD     D00LNP        KTCLNP                         *LINEA DI PARTENZA
610400151120     c                   clear                   detcflo
610500010926     C     KEYETC        CHAIN     TEETC01L                           99
610600970306IF  1C     *IN99         IFEQ      *ON                                          *NON TROVATO
610700970306     C     ETCATB        ORNE      *BLANKS                                      *NO ANNULLATI
610800970306     C                   MOVEL     MSG(1)        $MSG
610900970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
611000011008     C                   ELSE
611100151120     c                   movel     etcflo        detcflo
611200011026     C     D00DIV        IFNE      WDIVCE
611300011019     C                   MOVEL     D00DIV        WDVI
611400011008     C                   EXSR      CNVETC
611500011008     C                   ENDIF
611600970306E   1C                   ENDIF
611700970306     C*
611800970306     C                   ENDSR
611900970306     C*------------------------------------------------------------------------*
612000970306     C* CARETS - CARICA I DATI TIPO -SCAGLIONI-
612100970306     C*------------------------------------------------------------------------*
612200970306     C     CARETS        BEGSR
612300970306     C*
612400970306     C                   MOVEL     '0'           $SGL              1            *SCAGLIONI ESISTENTI
612500970306     C                   Z-ADD     *ZEROS        I                 6 0
612600091123     C                   MOVEL     'S'           KTSDSF                         *DATI SEDE O FILIALE
612700970306     C                   MOVEL     D00CTO        KTSCTO                         *TARIFFA O OFFERTA
612800970306     C                   Z-ADD     D00KSC        KTSKSC                         *CLIENTE O VISITA
612900970306     C                   Z-ADD     D00CTR        KTSCTR                         *TARIFFA
613000970306     C                   Z-ADD     D00PRG        KTSPRG                         *PROGRESSIVO
613100970306     C                   Z-ADD     D00LNP        KTSLNP                         *LINEA DI PARTENZA
613200010926     C     KE2ETS        SETLL     TEETS01L
613300010926     C     KE2ETS        READE     TEETS01L                               99
613400970306DO  1C     *IN99         DOWEQ     *OFF
613500970306IF  2C     ETSATB        IFEQ      *BLANKS                                      *NO ANNULLATI
613600011026     C     D00DIV        IFNE      WDIVCE
613700011019     C                   MOVEL     D00DIV        WDVI
613800011008     C                   EXSR      CNVETS
613900011008     C                   ENDIF
614000970306     C                   MOVEL     '1'           $SGL
614100970306     C                   ADD       1             I
614200970306     C                   Z-ADD     ETSSGL        SSGL(I)                        *SCAGLIONE SUPERIORE
614300970306     C                   Z-ADD     ETSPSC        SPSC(I)                        *PESO RIFERIMENTO
614400970306     C                   Z-ADD     ETSCSC        SCSC(I)                        *COLLI RIFERIMENTO
614500970306     C                   Z-ADD     ETSVSC        SVSC(I)                        *VOLUME RIFERIMENTO
614600970318     C                   Z-ADD     ETSINL        SINL(I)                        *INOLTRO
614700970318     C                   Z-ADD     ETSRPV        SRPV(I)                        *RAPPORTO PESO/VOLUM
614800970306     C                   Z-ADD     ETSPPE        SPPE(I)                        *% INCIDENZA
614900970414     C                   MOVEL     ETSSDE        SSDE(I)                        *SEGNO DELTA
615000970414     C                   Z-ADD     ETSPDE        SPDE(I)                        *% DELTA
615100970306E   2C                   ENDIF
615200010926     C     KE2ETS        READE     TEETS01L                               99
615300970306E   1C                   ENDDO
615400970306     C                   Z-ADD     I             WULTSC            6 0
615500970306IF  1C     $SGL          IFEQ      '0'                                          *NON TROVATO
615600970306     C                   MOVEL     MSG(2)        $MSG
615700970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
615800970306E   1C                   ENDIF
615900970306     C*
616000970306     C                   ENDSR
616100970306     C*------------------------------------------------------------------------*
616200970306     C* CARETT - CARICA I DATI TIPO -CODICI TASSAZIONE-
616300970306     C*------------------------------------------------------------------------*
616400970306     C     CARETT        BEGSR
616500970306     C*
616600970306     C                   MOVEL     '0'           $CTS              1            *COD.TASS. ESISTENTI
616700970306     C                   Z-ADD     *ZEROS        I                 6 0
616800091123     C                   MOVEL     'S'           KTTDSF                         *DATI SEDE O FILIALE
616900970306     C                   MOVEL     D00CTO        KTTCTO                         *TARIFFA O OFFERTA
617000970306     C                   Z-ADD     D00KSC        KTTKSC                         *CLIENTE O VISITA
617100970306     C                   Z-ADD     D00CTR        KTTCTR                         *TARIFFA
617200970306     C                   Z-ADD     D00PRG        KTTPRG                         *PROGRESSIVO
617300970306     C                   Z-ADD     D00LNP        KTTLNP                         *LINEA DI PARTENZA
617400970306     C                   MOVEL     D00FIE        KTTFIE                         *ITALIA/ESTERO
617500011017     C     KE2ETT        SETLL     TEETT01L
617600011017     C     KE2ETT        READE     TEETT01L                               99
617700970306DO  1C     *IN99         DOWEQ     *OFF
617800970306IF  2C     ETTRCT        IFNE      *BLANKS                                      *DA CALCOLARE
617900970306IF  3C     ETTATB        IFEQ      *BLANKS                                      *NO ANNULLATI
618000970306     C                   MOVEL     '1'           $CTS
618100970306     C                   ADD       1             I
618200970306     C                   MOVEL     ETTCTS        CCTS(I)                        *CODICE TASSAZIONE
618300970306     C                   MOVEL     ETTRCT        CRCT(I)                        *RAGGRUPPAMENTO
618400970306     C                   Z-ADD     ETTPPE        CPPE(I)                        *% INCIDENZA
618500970306     C*
618600970715     C* RECUPERA INSTRADAMENTO -ARRIVO-
618700020906     C                   Z-ADD     1             KTBKUT
618800970306     C                   MOVEL     'CT'          KTBCOD
618900970306     C                   MOVEL(P)  ETTCTS        KTBKEY
619000970306     C     KEYTAB        CHAIN     TABEL00F                           98
619100970306IF  4C     *IN98         IFEQ      *ON                                          *NON TROVATO
619200970306     C                   MOVEL     MSG(4)        $MSG
619300970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
619400970306X   4C                   ELSE                                                   *TROVATO
619500970306     C                   MOVEL     TBLUNI        DSCT
619600970306     C                   MOVEL     §CTRAP        CRAP(I)                        *REGIONE APPARTENENZ
619700970306     C     §CTNAR        IFEQ      *BLANKS
619800970306     C                   MOVEL     'IT'          CNAR(I)                        *NAZIONE APPARTENENZ
619900970306     C                   ELSE
620000970306     C                   MOVEL     'EE'          CNAR(I)
620100970306     C                   ENDIF
620200011030     C                   MOVEL     §CT§§5        C§§5(I)                        *FLAG ISOLA/NONISOLA
620300970715     C* LINEA ARRIVO
620400970715     C                   CLEAR                   DSSI95
620500970715     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
620600970715     C                   MOVEL     '3'           I95TCN                         *CONTROLLO ESISTENZA
620700970715     C                   MOVEL     §CTNAR        I95NAR                         *NAZIONE
620800970715     C                   MOVEL     §CTCAP        I95CAP                         *CAP
620900970715     C                   Z-ADD     WTFP          I95TFP                         *TERMINAL PARTENZA
621000980602     C                   MOVEL     'S'           I95FRE                         *CONTROLLA ECCEZIONI
621100970715     C                   Z-ADD     D00DTC        I95DAT                         *DATA RIFERIMENTO
621200970715     C                   CALL      'TISI95R'
621300970715     C                   PARM                    DSSI95
621400970715IF  5C     O95ERR        IFNE      *BLANKS                                      *NON TROVATO
621500970715     C                   MOVEL     O95MSG        $MSG
621600970715     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
621700970715X   5C                   ELSE                                                   *TROVATO
621800970715     C                   Z-ADD     O95LNA        CLNA(I)                        *LINEA ARRIVO
621900971204     C* FLAG ITALIA/ESTERO
622000971204     C                   Z-ADD     CLNA(I)       KORFIL
622100971204     C     KEYORG        CHAIN     AZORG01L                           98
622200020729     C                   MOVEL     ORGDE3        OG143
622300971204IF  6C     *IN98         IFEQ      *OFF                                         *NON TROVATO
622400020729     C*
622500020729     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
622600020729     C                   CLEAR                   DNTW
622700020729     C*
622800020729     C                   MOVEL(P)  'NTW'         KTECOD
622900020729     C                   MOVEL(P)  §OGNTW        KTEKE1
623000020729     C     KEYTBE        CHAIN     TNTBE01L
623100020729     C                   IF        %found(TNTBE01L)
623200020729     C                   MOVEL     TBEUNI        DNTW
623300020729     C                   ENDIF
623400020729     C*
623500020729     C                   MOVEL     §NTWFIE       CFL1(I)                        *FLAG ITALIA/ESTERO
623600971204E   6C                   ENDIF
623700971204     C*
623800970715     C* TERMINAL ARRIVO
623900970715     C                   CLEAR                   DSLV55
624000970715     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
624100970715     C                   MOVEL     'A'           D55TPT                         *ECCEZIONE ARRIVO
624200970715     C                   Z-ADD     O95LNA        D55LIN                         *LINEA
624300970715     C                   Z-ADD     D00DTC        D55DRF                         *DATA RIFERIMENTO
624400970715     C                   Z-ADD     D00LNP        D55LNP                         *LINEA PARTENZA
624500970715     C                   CALL      'FNLV55R'
624600970715     C                   PARM                    DSLV55
624700970715IF  6C     D55ERR        IFNE      *BLANKS                                      *NON TROVATO
624800970715     C                   MOVEL     D55MSG        $MSG
624900970715     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
625000970715X   6C                   ELSE                                                   *TROVATO
625100970715     C                   Z-ADD     D55TFA        CTFA(I)                        *TERMINAL ARRIVO
625200970715E   6C                   ENDIF
625300970715E   5C                   ENDIF
625400970306E   4C                   ENDIF
625500970306E   3C                   ENDIF
625600970306E   2C                   ENDIF
625700011017     C     KE2ETT        READE     TEETT01L                               99
625800970306E   1C                   ENDDO                                                  *FINE COD.TASSAZIONE
625900970306     C                   Z-ADD     I             WULTCT            6 0
626000970306IF  1C     $CTS          IFEQ      '0'                                          *NON TROVATO
626100970306     C                   MOVEL     MSG(3)        $MSG
626200970306     C                   EXSR      IMPERR                                       *IMPOSTA ERRORE
626300970306E   1C                   ENDIF
626400970715     C*
626500970306     C                   ENDSR
626600970306     C*------------------------------------------------------------------------*
626700970306     C* CARTAM - CARICAMENTO TARIFFA CLIENTE -TESTATA-
626800970306     C*------------------------------------------------------------------------*
626900970306     C     CARTAM        BEGSR
627000970306     C*
627100970306     C                   Z-ADD     D00KSC        KTMKSC                         *CLIENTE
627200970306     C                   Z-ADD     D00CTR        KTMCTR                         *TARIFFA
627300970306     C                   Z-ADD     D00PRG        KTMPRG                         *PROGRESSIVO
627400091201     C     KE2TAM        CHAIN     TNTAM00L                           99        *ESISTE PER FORZA
627500040826     C                   MOVEL     TAMFLO        DSTA01
627600970306     C*
627700970306     C* APPLICAZIONE TARIFFA FINITA
627800011031     C                   Z-ADD     *ZEROS        MINTAS           23 8
627900011031     C                   Z-ADD     *ZEROS        MINTAR           23 8
628000970306     C                   MOVEL     D00CTR        UNOCTR            1 0          *TIPO TARIFFA
628100980707     C                   MOVEL     UNOCTR        WTPG              1
628200970306     C                   SETOFF                                           28
628300970306     C                   Z-ADD     1             I
628400980707     C     WTPG          LOOKUP    CTR(I)                                 28
628500970306IF  1C     *IN28         IFEQ      *ON
628600970306     C     I             OCCUR     DSTR
628700970306     C                   Z-ADD     §TRATA        MINTAS                         *MINIMO TASSABILE ST
628800970306E   1C                   ENDIF
628900980422     C                   Z-ADD     §TRATA        MINTAR
629000970306IF  1C     TAMATA        IFGT      *ZEROS                                       *APPLICAZ.TAR.FINITA
629100970306     C                   Z-ADD     TAMATA        MINTAR                         *MINIMO APPLICAZ. KS
629200970306E   1C                   ENDIF
629300970306     C*
629400980506     C* DEPOSITA I DATI DELLA DSTR DELLA STATA TARIFFA
629500980506     C                   Z-ADD     MINTAS        DMITAS                         *MINIMO TASSABILE ST
629600980506     C                   MOVEL     §TRARR        DTRARR                         *ARROTONDAM ABILE ST
629700980506     C                   MOVEL     §TRRPV        DTRRPV                         *RPV        ABILE ST
629800971022     C*
629900970306     C                   ENDSR
630000991125     C*------------------------------------------------------------------------*
630100011008     C* CNVTAD - CONVERTE I CAMPI DI TITAM IN DIVISA DI CALCOLO DEL CE
630200991125     C*------------------------------------------------------------------------*
630300991125     C     CNVTAD        BEGSR
630400991125     C* PORTO
630500991125     C                   Z-ADD     TADITR        WIMI             30 9
630600991125     C                   EXSR      CNVEUR
630700991125     C                   Z-ADD     WIMO          TADITR
630800991125     C* INOLTRO
630900991125     C                   Z-ADD     TADINO        WIMI             30 9
631000991125     C                   EXSR      CNVEUR
631100991125     C                   Z-ADD     WIMO          TADINO
631200991125     C*
631300991125     C                   ENDSR
631400991125     C*------------------------------------------------------------------------*
631500011008     C* CNVTPT - CONVERTE I CAMPI DI TITPT IN DIVISA DI CALCOLO DEL CE
631600991125     C*------------------------------------------------------------------------*
631700991125     C     CNVTPT        BEGSR
631800991125     C*
631900991125     C* ALCUNI CAMPI SONO DA CONVERTIRE SOLO SE CONTENGONO IMPORTI IN MONETA
632000991125IF  1C     TPTTPG        IFEQ      '4'                                          *QUANTITA'
632100991125     C     TPTTPG        OREQ      'T'                                          *TRASPORTO
632200991125     C     TPTTPG        OREQ      'V'                                          *VALORE
632300991125     C*
632400991125     C* ARROTONDAMENTO LIMITE
632500991125     C                   Z-ADD     TPTARL        WIMI             30 9
632600991125     C                   EXSR      CNVEUR
632700991125     C                   Z-ADD     WIMO          TPTARL
632800991125     C*
632900991125     C* ARROTONDAMENTO FINO LIMITE
633000991125     C                   Z-ADD     TPTARF        WIMI             30 9
633100991125     C                   EXSR      CNVEUR
633200991125     C                   Z-ADD     WIMO          TPTARF
633300991125     C*
633400991125     C* ARROTONDAMENTO OLTRE LIMITE
633500991125     C                   Z-ADD     TPTARO        WIMI             30 9
633600991125     C                   EXSR      CNVEUR
633700991125     C                   Z-ADD     WIMO          TPTARO
633800991125E   1C                   ENDIF
633900991125     C* VALORE MERCE
634000991125     C                   Z-ADD     TPTVLM        WIMI             30 9
634100991125     C                   EXSR      CNVEUR
634200991125     C                   Z-ADD     WIMO          TPTVLM
634300991125     C*
634400991125     C                   ENDSR
634500991125     C*------------------------------------------------------------------------*
634600011008     C* CNVTPD - CONVERTE I CAMPI DI TITPD IN DIVISA DI CALCOLO DEL CE
634700991125     C*------------------------------------------------------------------------*
634800991125     C     CNVTPD        BEGSR
634900991125     C*
635000991125     C* ALCUNI CAMPI SONO DA CONVERTIRE SOLO SE CONTENGONO IMPORTI IN MONETA
635100991125IF  1C     TPTTPG        IFEQ      '4'                                          *QUANTITA'
635200991125     C     TPTTPG        OREQ      'T'                                          *TRASPORTO
635300991125     C     TPTTPG        OREQ      'V'                                          *VALORE
635400991125     C* SCAGLIONE
635500991125     C                   Z-ADD     TPDSGL        N5                5 0
635600991125IF  2C     N5            IFNE      99999                                        *SCAGLIONE 99999=OLT
635700991125     C                   Z-ADD     TPDSGL        WIMI             30 9
635800991125     C                   EXSR      CNVEUR
635900991125     C                   Z-ADD     WIMO          TPDSGL
636000991125E   2C                   ENDIF
636100991125E   1C                   ENDIF
636200991125     C*
636300991125     C* ALCUNI CAMPI NON SONO DA CONVERTIRE PERCHE' RAPPRESENTANO PERCENTUALI
636400991125IF  1C     TPTTPG        IFNE      '4'                                          *QUANTITA'
636500991125     C     TPTTPG        ANDNE     'T'                                          *TRASPORTO
636600991125     C     TPTTPG        ANDNE     'V'                                          *VALORE
636700991125     C* TARIFFA
636800991125     C                   Z-ADD     TPDITR        WIMI             30 9
636900991125     C                   EXSR      CNVEUR
637000991125     C                   Z-ADD     WIMO          TPDITR
637100991125E   1C                   ENDIF
637200991125     C* MINIMO
637300991125     C                   Z-ADD     TPDMIN        WIMI             30 9
637400991125     C                   EXSR      CNVEUR
637500991125     C                   Z-ADD     WIMO          TPDMIN
637600991125     C* MASSIMO
637700991125     C                   Z-ADD     TPDMAX        WIMI             30 9
637800991125     C                   EXSR      CNVEUR
637900991125     C                   Z-ADD     WIMO          TPDMAX
638000991125     C*
638100991125     C                   ENDSR
638200991125     C*------------------------------------------------------------------------*
638300011008     C* CNVTGC - CONVERTE I CAMPI DI TITGC IN DIVISA DI CALCOLO DEL CE
638400991125     C*------------------------------------------------------------------------*
638500991125     C     CNVTGC        BEGSR
638600991125     C*
638700991125     C* SPESE VARIE
638800991125     C                   Z-ADD     TGCSGV        WIMI             30 9
638900991125     C                   EXSR      CNVEUR
639000991125     C                   Z-ADD     WIMO          TGCSGV
639100991125     C* SPESE RICONSEGNA CITTA'
639200991125     C                   Z-ADD     TGCSGR        WIMI             30 9
639300991125     C                   EXSR      CNVEUR
639400991125     C                   Z-ADD     WIMO          TGCSGR
639500991125     C* SPESE RICONSEGNA PROVINCIA
639600991125     C                   Z-ADD     TGCSGP        WIMI             30 9
639700991125     C                   EXSR      CNVEUR
639800991125     C                   Z-ADD     WIMO          TGCSGP
639900991125     C* SPESE DOSSIER
640000991125     C                   Z-ADD     TGCSGD        WIMI             30 9
640100991125     C                   EXSR      CNVEUR
640200991125     C                   Z-ADD     WIMO          TGCSGD
640300991125     C* 1° TARIFFA SOSTA
640400991125     C                   Z-ADD     TTS(1)        WIMI             30 9
640500991125     C                   EXSR      CNVEUR
640600991125     C                   Z-ADD     WIMO          TTS(1)
640700991125     C* 2° TARIFFA SOSTA
640800991125     C                   Z-ADD     TTS(2)        WIMI             30 9
640900991125     C                   EXSR      CNVEUR
641000991125     C                   Z-ADD     WIMO          TTS(2)
641100991125     C* 3° TARIFFA SOSTA
641200991125     C                   Z-ADD     TTS(3)        WIMI             30 9
641300991125     C                   EXSR      CNVEUR
641400991125     C                   Z-ADD     WIMO          TTS(3)
641500991125     C* TARIFFA MINIMA SOSTA
641600991125     C                   Z-ADD     TGCSTM        WIMI             30 9
641700991125     C                   EXSR      CNVEUR
641800991125     C                   Z-ADD     WIMO          TGCSTM
641900991125     C*
642000991125     C                   ENDSR
642100011008     C*------------------------------------------------------------------------*
642200011023     C* CNVETC - CONVERTE I CAMPI DI TEETC IN DIVISA DI CALCOLO DEL CE
642300011008     C*------------------------------------------------------------------------*
642400011008     C     CNVETC        BEGSR
642500011008     C* ARROTONDAMENTO
642600011008     C                   Z-ADD     ETCIAR        WIMI             30 9
642700011008     C                   EXSR      CNVEUR
642800011008     C                   Z-ADD     WIMO          ETCIAR
642900011008     C* CONTRASSEGNO
643000011008     C                   Z-ADD     ETCCAS        WIMI             30 9
643100011008     C                   EXSR      CNVEUR
643200011008     C                   Z-ADD     WIMO          ETCCAS
643300011008     C* ASSICURAZIONE
643400011008     C                   Z-ADD     ETCASC        WIMI             30 9
643500011008     C                   EXSR      CNVEUR
643600011008     C                   Z-ADD     WIMO          ETCASC
643700011008     C*
643800011008     C                   ENDSR
643900011008     C*------------------------------------------------------------------------*
644000011008     C* CNVETS - CONVERTE I CAMPI DI TEETS IN DIVISA DI CALCOLO DEL CE
644100011008     C*------------------------------------------------------------------------*
644200011008     C     CNVETS        BEGSR
644300011008     C* INOLTRO
644400011008     C                   Z-ADD     ETSINL        WIMI             30 9
644500011008     C                   EXSR      CNVEUR
644600011008     C                   Z-ADD     WIMO          ETSINL
644700011008     C*
644800011008     C                   ENDSR
644900991125     C*------------------------------------------------------------------------*
645000011008     C* CNVEUR - CONVERTE I CAMPI IN DIVISA DI CALCOLO DEL CE
645100991125     C*------------------------------------------------------------------------*
645200991125     C     CNVEUR        BEGSR
645300991125     C*
645400991125     C* AZZERA VARIABILI DI LAVORO
645500991125     C                   Z-ADD     *ZEROS        WIMO             30 9          *IMPORTO CONVERTITO
645600011008     C                   MOVEL     *BLANKS       NDEC              1            *N. DEC IN OUTPUT
645700011008     C*
645800011026     C* IMPOSTO I DECIMALI DI OUTPUT A SECONDA DELLA DIVISA DI OUTPUT (NON + VERO: SEMPREE MAX DEC)
645900011026     C*                  IF        §GEDCEDIV = 'ITL'
646000011026     C*                  EVAL      NDEC = '0'
646100011026     C*                  ELSE
646200011026     C*                  EVAL      NDEC = '3'
646300011026     C*                  ENDIF
646400011026     C                   EVAL      NDEC = '9'
646500991125     C*
646600991125IF  1C     WIMI          IFNE      *ZEROS
646700991125     C                   CLEAR                   YEUR
646800991125     C                   MOVEL     WDVI          YECDVI                         *DIVISA IMPORTO
646900991125     C                   Z-ADD     WIMI          YECIMI                         *IMPORTO DA CONVERTI
647000011026     C                   MOVEL     WDIVCE        YECDVO                         *DIVISA IMP CONVERTI
647100011008     C                   MOVEL     NDEC          YECDCO                         *N° DECIMALI
647200991125     C                   MOVEL     *BLANKS       YECTAR                         *ARROTONDAMENTO "H"
647300991125     C                   CALL      'YEURCO'
647400991125     C                   PARM                    YEUR
647500991125IF  2C     YECESI        IFEQ      ' '                                          *NO ERRORI
647600991125     C                   Z-ADD     YECIMO        WIMO                            -IMPORTO CONVERTITO
647700991125X   2C                   ELSE                                                   *SI ERRORI
647800991125     C                   Z-ADD     WIMI          WIMO                            -IMPOROTO DA CONVER
647900991125E   2C                   ENDIF
648000991125E   1C                   ENDIF
648100991125     C*
648200991125     C                   ENDSR
648300970306     C*------------------------------------------------------------------------*
648400970306     C* IMPERR - IMPOSTA ERRORE NELLA DS DI PROCEDURA
648500970306     C*------------------------------------------------------------------------*
648600970306     C     IMPERR        BEGSR
648700970306     C*
648800970306     C                   MOVEL     'S'           WERR                           *ERRORE
648900970306     C                   MOVEL     $MSG          D00MSG                         *MESSAGGIO PROCEDURA
649000970306     C                   MOVEL     '1'           D00ERR                         *ERRORE PROCEDURA
649100970306     C*
649200970306     C                   ENDSR
649300970715     C*------------------------------------------------------------------------*
649400970715     C* FINSR - OPERAZIONI FINALI
649500970715     C*------------------------------------------------------------------------*
649600970715     C     FINSR         BEGSR
649700970715     C*
649800970715     C* CHIUDE I PROGRAMMA DI UTILITA' RICHIAMATI
649900970715     C                   CLEAR                   DSSI95                         *CONTROLLA CAP
650000970715     C                   MOVEL     'C'           I95TLA
650100970715     C                   CALL      'TISI95R'
650200970715     C                   PARM                    DSSI95
650300970715     C*
650400970715     C                   CLEAR                   DSLV55                         *RECUPERA TERMINAL
650500970715     C                   MOVEL     'C'           D55TLA
650600970715     C                   CALL      'FNLV55R'
650700970715     C                   PARM                    DSLV55
650800970715     C*
650900970715     C                   ENDSR
651000011008     C*------------------------------------------------------------------------*
651100011008     C* CARGEDCE - CARICAMENTO TABELLA "GED" TIPO RECORD "CE" E CONSIDERAZIONI SU DIVISE
651200011008     C*------------------------------------------------------------------------*
651300011008     C     CARGEDCE      BEGSR
651400011008     C*
651500011008     C                   CLEAR                   DSGEDCE
651600011026     C                   MOVEL     *blanks       WDIVCE            3
651700011008     C*
651800011008     C                   MOVEL(P)  'GED'         KTECOD
651900011008     C                   MOVEL(P)  'CE'          KTEKE1
652000011008     C     KEYTBE        CHAIN     TNTBE01L
652100011008     C                   IF        %found(TNTBE01L)
652200011008     C                   MOVEL     TBEUNI        DSGEDCE
652300011008     C                   ENDIF
652400011008     C*
652500011008     C                   IF        §GEDCEDAT > D00DTC
652600011026     C                   EVAL      WDIVCE = §GEDCEDIP
652700011026     C                   ELSE
652800011026     C                   EVAL      WDIVCE = §GEDCEDIV
652900011008     C                   ENDIF
653000011008     C*
653100011008     C                   ENDSR
653200040716     C*------------------------------------------------------------------------*
653300040716     C* CARGEIEUR- CARICAMENTO TABELLA "GEI" TIPO RECORD "EUR"
653400040716     C*------------------------------------------------------------------------*
653500040716     C     CARGEIEUR     BEGSR
653600040716     C*
653700040716     C                   CLEAR                   DGEI
653800040716     C*
653900040716     C                   MOVEL(P)  'GEI'         KTECOD
654000040716     C                   MOVEL(P)  'EUR'         KTEKE1
654100040716     C     KEYTBE        CHAIN     TNTBE01L
654200040716     C                   IF        %found(TNTBE01L)
654300040716     C                   MOVEL     TBEUNI        DGEI
654400040716     C                   ENDIF
654500040716     C*
654600040716     C                   ENDSR
654700970306     C*------------------------------------------------------------------------*
654800970306     C* *INZSR - OPERAZIONI INIZIALI
654900970306     C*------------------------------------------------------------------------*
655000970306     C     *INZSR        BEGSR
655100970306     C*--------------------
655200970306     C* RICEVIMENTO PARAMETRI
655300970306     C*--------------------
655400970306     C     *ENTRY        PLIST
655500970306     C                   PARM                    KPJBA
655600970306     C                   PARM                    DSTE00
655700041202     c* apertura controllata di alcuni file xchè solo di filiale
655800041202     c                   open      tiogc01l                             95
655900041202     c                   open      tiopd02l                             95
656000041202     c                   open      tiopt01l                             95
656100041202     c                   open      tnofm00l                             95
656200970306     C*--------------------
656300970306     C* VARIABILI RIFERITE AL DATA BASE
656400970306     C*--------------------
656500010926     C     *LIKE         DEFINE    ETCDSF        KTCDSF                         *TEETC00F
656600980603     C     *LIKE         DEFINE    ETCCTO        KTCCTO
656700970306     C     *LIKE         DEFINE    ETCKSC        KTCKSC
656800970306     C     *LIKE         DEFINE    ETCCTR        KTCCTR
656900970306     C     *LIKE         DEFINE    ETCPRG        KTCPRG
657000970306     C     *LIKE         DEFINE    ETCLNP        KTCLNP
657100010926     C     *LIKE         DEFINE    ETSDSF        KTSDSF                         *TEETC00F
657200980603     C     *LIKE         DEFINE    ETSCTO        KTSCTO
657300970306     C     *LIKE         DEFINE    ETSKSC        KTSKSC
657400970306     C     *LIKE         DEFINE    ETSCTR        KTSCTR
657500970306     C     *LIKE         DEFINE    ETSPRG        KTSPRG
657600970306     C     *LIKE         DEFINE    ETSLNP        KTSLNP
657700970306     C     *LIKE         DEFINE    ETSSGL        KTSSGL
657800010926     C     *LIKE         DEFINE    ETTDSF        KTTDSF                         *TEETC00F
657900980603     C     *LIKE         DEFINE    ETTCTO        KTTCTO
658000970306     C     *LIKE         DEFINE    ETTKSC        KTTKSC
658100970306     C     *LIKE         DEFINE    ETTCTR        KTTCTR
658200970306     C     *LIKE         DEFINE    ETTPRG        KTTPRG
658300970306     C     *LIKE         DEFINE    ETTLNP        KTTLNP
658400970306     C     *LIKE         DEFINE    ETTFIE        KTTFIE
658500970306     C     *LIKE         DEFINE    ETTCTS        KTTCTS
658600010926     C     *LIKE         DEFINE    ETWUSR        KTWUSR                         TEETW00F
658700980603     C     *LIKE         DEFINE    ETWDSF        KTWDSF
658800980603     C     *LIKE         DEFINE    ETWCTO        KTWCTO
658900970306     C     *LIKE         DEFINE    ETWKSC        KTWKSC
659000970306     C     *LIKE         DEFINE    ETWCTR        KTWCTR
659100970306     C     *LIKE         DEFINE    ETWPRG        KTWPRG
659200970306     C     *LIKE         DEFINE    ETWSGL        KTWSGL
659300970306     C     *LIKE         DEFINE    ETWCTS        KTWCTS
659400970306     C     *LIKE         DEFINE    ETWRDL        KTWRDL
659500010926     C     *LIKE         DEFINE    EWRUSR        KWRUSR                         TEETWR0F
659600970306     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
659700970306     C     *LIKE         DEFINE    TBLCOD        KTBCOD
659800970306     C     *LIKE         DEFINE    TBLKEY        KTBKEY
659900970306     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
660000970306     C     *LIKE         DEFINE    TAMKSC        KTMKSC                         *TNTAM00F
660100970306     C     *LIKE         DEFINE    TAMCTR        KTMCTR
660200970306     C     *LIKE         DEFINE    TAMPRG        KTMPRG
660300990819     C     *LIKE         DEFINE    TADKSC        KTDKSC                         *TITAD00F
660400970306     C     *LIKE         DEFINE    TADPRG        KTDPRG
660500970306     C     *LIKE         DEFINE    TADLNP        KTDLNP
660600970306     C     *LIKE         DEFINE    TADCTS        KTDCTS
660700990819     C     *LIKE         DEFINE    TADNAZ        KTDNAZ
660800990819     C     *LIKE         DEFINE    TADCAP        KTDCAP
660900970306     C     *LIKE         DEFINE    TADCTR        KTDCTR
661000990819     C     *LIKE         DEFINE    TPTKSC        KTPKSC                         *TITPT00F
661100970306     C     *LIKE         DEFINE    TPTCTR        KTPCTR
661200970306     C     *LIKE         DEFINE    TPTPRG        KTPPRG
661300970306     C     *LIKE         DEFINE    TPTFTC        KTPFTC
661400990819     C     *LIKE         DEFINE    TPDKSC        KDPKSC                         *TITPD00F
661500970306     C     *LIKE         DEFINE    TPDCTR        KDPCTR
661600970306     C     *LIKE         DEFINE    TPDPRG        KDPPRG
661700970306     C     *LIKE         DEFINE    TPDFTC        KDPFTC
661800970306     C     *LIKE         DEFINE    TPDCTS        KDPCTS
661900990819     C     *LIKE         DEFINE    TGCKSC        KGCKSC                         *TITGC00F
662000970306     C     *LIKE         DEFINE    TGCCTR        KGCCTR
662100970306     C     *LIKE         DEFINE    TGCPRG        KGCPRG
662200970306     C     *LIKE         DEFINE    TAMKSC        CARKSC                         *DI DEPOSITO
662300970306     C     *LIKE         DEFINE    TAMCTR        CARCTR
662400970306     C     *LIKE         DEFINE    TAMPRG        CARPRG
662500970306     C     *LIKE         DEFINE    TAMKSC        DEPKSC
662600970306     C     *LIKE         DEFINE    TAMCTR        DEPPRG
662700970306     C     *LIKE         DEFINE    TAMPRG        DEPCTR
662800970721     C     *LIKE         DEFINE    TPDAIN        DEPAIN
662900970721     C     *LIKE         DEFINE    TPDAIN        CPLAIN
663000011030     C     *LIKE         DEFINE    TPDAIN        CISAIN
663100011031     C     *LIKE         DEFINE    TPDAIN        SAMAIN
663200020729     C     *LIKE         DEFINE    §NTWFIE       DLPFL1
663300980506     C     *LIKE         DEFINE    §TRATA        DMITAS                                     ABILE ST
663400980506     C     *LIKE         DEFINE    §TRARR        DTRARR                                     ABILE ST
663500980506     C     *LIKE         DEFINE    §TRRPV        DTRRPV                                     ABILE ST
663600970306     C     *LIKE         DEFINE    ETTCTS        LCTS                           *DI LAVORO
663700970306     C     *LIKE         DEFINE    TAMKSC        KSC1
663800970306     C     *LIKE         DEFINE    TAMCTR        CTR1
663900970715     C     *LIKE         DEFINE    D55TFA        WTFA
664000970715     C     *LIKE         DEFINE    D55TFP        WTFP
664100970721     C     *LIKE         DEFINE    ORGCTS        WMCT
664200011008     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
664300011008     C     *LIKE         DEFINE    TBEKE1        KTEKE1
664400970306     C*--------------------
664500970306     C* CHIAVI DI LETTURA
664600970306     C*--------------------
664700010926     C* LETTURA TEETC01L - COMPLETA
664800970306     C     KEYETC        KLIST
664900980603     C                   KFLD                    KTCDSF                         *DATI SEDE O FILIALE
665000980603     C                   KFLD                    KTCCTO                         *TARIFFA O OFFERTA
665100970306     C                   KFLD                    KTCKSC                         *CLIENTE O VISITA
665200970306     C                   KFLD                    KTCCTR                         *TARIFFA
665300970306     C                   KFLD                    KTCPRG                         *PROGRESSIVO
665400970306     C                   KFLD                    KTCLNP                         *LINEA PARTENZA
665500010926     C* LETTURA TEETS01L - COMPLETA
665600970306     C     KEYETS        KLIST
665700980603     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
665800970306     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
665900970306     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
666000970306     C                   KFLD                    KTSCTR                         *TARIFFA
666100970306     C                   KFLD                    KTSPRG                         *PROGRESSIVO
666200970306     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
666300970306     C                   KFLD                    KTSSGL                         *SCAGLIONE
666400010926     C* LETTURA TEETS01L - PARZIALE
666500970306     C     KE2ETS        KLIST
666600980603     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
666700970306     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
666800970306     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
666900970306     C                   KFLD                    KTSCTR                         *TARIFFA
667000970306     C                   KFLD                    KTSPRG                         *PROGRESSIVO
667100970306     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
667200011017     C* LETTURA TEETT01L - COMPLETA
667300970306     C     KEYETT        KLIST
667400980603     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
667500970306     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
667600970306     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
667700970306     C                   KFLD                    KTTCTR                         *TARIFFA
667800970306     C                   KFLD                    KTTPRG                         *PROGRESSIVO
667900970306     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
668000970306     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
668100970306     C                   KFLD                    KTTCTS                         *CODICE TASSAZIONE
668200011017     C* LETTURA TEETT01L - PARZIALE
668300970306     C     KE2ETT        KLIST
668400980603     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
668500970306     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
668600970306     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
668700970306     C                   KFLD                    KTTCTR                         *TARIFFA
668800970306     C                   KFLD                    KTTPRG                         *PROGRESSIVO
668900970306     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
669000970306     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
669100010926     C* LETTURA TEETW01L - COMPLETA
669200970306     C     KEYETW        KLIST
669300970306     C                   KFLD                    KTWUSR                         UTENTE
669400980603     C                   KFLD                    KTWDSF                         *DATI SEDE O FILIALE
669500970306     C                   KFLD                    KTWCTO                         *TARIFFA O OFFERTA
669600970306     C                   KFLD                    KTWKSC                         *CLIENTE O VISITA
669700970306     C                   KFLD                    KTWCTR                         *TARIFFA
669800970306     C                   KFLD                    KTWPRG                         *PROGRESSIVO
669900970306     C                   KFLD                    KTWSGL                         *SCAGLIONE
670000970306     C                   KFLD                    KTWCTS                         *CODICE TASSAZIONE
670100970306     C                   KFLD                    KTWRDL                         *RAGGRUPPAMENTO DELT
670200010926     C* LETTURA TEETW01L - PARZIALE
670300970317     C     KE2ETW        KLIST
670400970317     C                   KFLD                    KTWUSR                         UTENTE
670500010926     C* LETTURA TEETWR1L - PARZIALE
670600970306     C     KEYEWR        KLIST
670700970306     C                   KFLD                    KWRUSR                         *UTENTE
670800970306     C* LETTURA TABEL00F - COMPLETA
670900970306     C     KEYTAB        KLIST
671000970306     C                   KFLD                    KTBKUT                         *UTENTE
671100970306     C                   KFLD                    KTBCOD                         *CODICE
671200970306     C                   KFLD                    KTBKEY                         *CHIAVE
671300970306     C* LETTURA TABEL00F - PARZIALE
671400970306     C     KE2TAB        KLIST
671500970306     C                   KFLD                    KTBKUT                         *UTENTE
671600970306     C                   KFLD                    KTBCOD                         *CODICE
671700970306     C* LETTURA AZORG01L - COMPLETA
671800970306     C     KEYORG        KLIST
671900970306     C                   KFLD                    KORFIL                         *FILIALE
672000970306     C* LETTURA TNTAM01L - PARZIALE
672100970306     C     KEYTAM        KLIST
672200970306     C                   KFLD                    KTMKSC                         *CLIENTE
672300970306     C                   KFLD                    KTMCTR                         *TARIFFA
672400970306     C* LETTURA TNTAM01L - COMPLETA
672500970306     C     KE2TAM        KLIST
672600970306     C                   KFLD                    KTMKSC                         *CLIENTE
672700970306     C                   KFLD                    KTMCTR                         *TARIFFA
672800970306     C                   KFLD                    KTMPRG                         *PROGRESSIVO
672900990819     C* LETTURA TITAD01L - PARZIALE
673000970306     C     KEYTAD        KLIST
673100970306     C                   KFLD                    KTDKSC                         *CLIENTE
673200970306     C                   KFLD                    KTDPRG                         *PROGRESSIVO
673300970306     C                   KFLD                    KTDLNP                         *LINEA DI PARTENZA
673400970306     C                   KFLD                    KTDCTS                         *CODICE TASSAZIONE
673500990819     C                   KFLD                    KTDNAZ                         *NAZIONE
673600990819     C                   KFLD                    KTDCAP                         *C.A.P.
673700970306     C                   KFLD                    KTDCTR                         *TARIFFA
673800990819     C* LETTURA TITPT01L - COMPLETA
673900970306     C     KEYTPT        KLIST
674000970306     C                   KFLD                    KTPKSC                         *CLIENTE
674100970306     C                   KFLD                    KTPCTR                         *TARIFFA
674200970306     C                   KFLD                    KTPPRG                         *PROGRESSIVO
674300970306     C                   KFLD                    KTPFTC                         *CONSEGNA PARTICOLAR
674400990819     C* LETTURA TITPD02L - PARZIALE
674500970306     C     KEYTPD        KLIST
674600970306     C                   KFLD                    KDPKSC                         *CLIENTE
674700970306     C                   KFLD                    KDPCTR                         *TARIFFA
674800970306     C                   KFLD                    KDPPRG                         *PROGRESSIVO
674900970306     C                   KFLD                    KDPFTC                         *CONSEGNA PARTICOLAR
675000970306     C                   KFLD                    KDPCTS                         *CODICE TASSAZIONE
675100990819     C* LETTURA TITGC01L - COMPLETA
675200970306     C     KEYTGC        KLIST
675300970306     C                   KFLD                    KGCKSC                         *CLIENTE
675400970306     C                   KFLD                    KGCCTR                         *TARIFFA
675500970306     C                   KFLD                    KGCPRG                         *PROGRESSIVO
675600011008     C* LETTURA TNTBE01L - PARZIALE
675700011008     C     KEYTBE        KLIST
675800011008     C                   KFLD                    KTECOD                         *UTENTE
675900011008     C                   KFLD                    KTEKE1                         *CODICE
676000970306     C*--------------------
676100970306     C* VARIABILI DI CONTROLLO
676200970306     C*--------------------
676300970306     C                   MOVEL     'N'           WERR              1            *ERRORE
676400970306     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGIO
676500970306     C                   MOVEL     '0'           $TGC              1            *TAR.GIACENZA  ESIST
676600970306     C                   MOVEL     '0'           $TAD              1            *TAR.DETTAGLIO ESIST
676700970306     C                   MOVEL     '0'           $TPT              1            *TAR.PART.TES. ESIST
676800970306     C                   MOVEL     '0'           $TPD              1            *TAR.PART.DET. ESIST
676900011023     C                   MOVEL     D00DIV        WDIV              3            *DIVISA CALCOLO CAT
677000970306     C*
677100970306     C* REPERISCE LA DATA CORRENTE
677200970306     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
677300970306     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
677400970306     C                   Z-ADD     WN8           G08DAT
677500970306     C                   Z-ADD     *ZEROS        G08INV
677600970306     C                   MOVEL     '0'           G08ERR
677700970306     C                   CALL      'XSRDA8'
677800970306     C                   PARM                    WLBDA8
677900970306     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
678000011008     C*
678100011008     C* RECUPERA DATI DA TEBELLA "GED" TIPO RECORD "CE"
678200011008     C                   EXSR      CARGEDCE
678300040716     C* RECUPERA DATI DA TEBELLA "GEI" TIPO RECORD "EUR"
678400040716     C                   EXSR      CARGEIEUR
678500971204     C*
678600971204     C* RECUPERA INSTRADAMENTO -PARTENZA-
678700971204     C                   EXSR      REPINP
678800970306     C*
678900970306     C                   ENDSR
679000970306     O*------------------------------------------------------------------------*
679100970306** MSG - MESSAGGI
679200970306TNTE11R- Dati tipo del cliente/tariffa non trovati                            1
679300970306TNTE11R- Scaglioni del cliente/tariffa non trovati                            2
679400970306TNTE11R- Codici tassazione del cliente/tariffa non trovati                    3
679500970306TNTE11R- Codice tassazione non trovato                                        4
679600970715TNTE11R- La linea di partenza della tariffa non è stata trovata               5
679700970715TNTE11R- 6
679800970715TNTE11R- 7
679900970306TNTE11R- 8
680000970306TNTE11R- 9
680100970306TNTE11R- 10
680200160310TNTE11R- Non trovata tariffa di cartello. telefonare CED                      11
680300970306TNTE11R- Tabella QT2 non trovata. Avvertire il C.E.D.                         12
680400101018???????????? % ISTAT non trovata. Avvertire il C.E.D.                         13
680500101018                                                                              14
680600970306TNTE11R- Tabella TR  non trovata. Avvertire il C.E.D.                         15
680700970306TNTE11R- Tabella $2  non trovata. Avvertire il C.E.D.                         16
680800970306TNTE11R- Tabella CV  non trovata. Avvertire il C.E.D.                         17
680900970306TNTE11R- Tabella CT  non trovata. Avvertire il C.E.D.                         18
681000970306TNTE11R- Tabella 5E  non trovata. Avvertire il C.E.D.                         19
681100970306TNTE11R- Tabella 1P  non trovata. Avvertire il C.E.D.                         20
681200970306TNTE11R- 21
681300970306TNTE11R- MANCA TARIFFA. Tariffa di cartello inesistente.                      22
681400970306TNTE11R- MANCA TARIFFA. Tariffa giacenza non trovata.                         23
681500970306TNTE11R- MANCA TARIFFA. Tariffa dettaglio cliente non trovata.                24
681600970306TNTE11R- MANCA TARIFFA. Scaglione di peso in tariffa non trovato.             25
681700001218TNTE11R- MANCA TARIFFA. Scaglione di peso in t.particolare non trovato.   -  -26
681800970306TNTE11R- 27
681900970306TNTE11R- 28
682000970306TNTE11R- 29
682100970306TNTE11R- 30
