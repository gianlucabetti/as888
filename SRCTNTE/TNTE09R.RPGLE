000100970117     H*------------------------------------------------------------------------*
000200970117     H* AGGIORNA I CODICI TASSAZIONE DALLA TARIFFA CLIENTE                     *
000300970117     H*------------------------------------------------------------------------*
000400970117     H DECEDIT('0,') DATEDIT(*DMY.)
000500970117     F*------------------------------------------------------------------------*
000600970117     F* DATA BASE
000700970117     F*------------------------------------------------------------------------*
000800971204     FAZORG01L  IF   E           K DISK
000900970117     FTABEL00F  IF   E           K DISK
001000990819     FTITAD04L  IF   E           K DISK
001100011017     FTEETT01L  UF A E           K DISK
001200020729     FTNTBE01L  IF   E           K DISK
001300970117     D*------------------------------------------------------------------------*
001400970117     D* SCHIERE
001500970117     D*------------------------------------------------------------------------*
001600970130     D*-------------------
001700970130     D* MESSAGGI
001800970130     D*-------------------
001900970130     D MSG             S             78    DIM(10) CTDATA PERRCD(1)
002000970117     D*-------------------
002100970117     D* CODICI TASSAZIONE
002200970117     D*-------------------
002300970117     D*---
002400970117     D* TABELLA CT
002500970117     D*---
002600050103     D CTS             S              2    DIM(600)                             *CODICE
002700050103     D RAP             S              2    DIM(600)                             *CODICE REGIONE
002800050103     D NAR             S              2    DIM(600)                             *CODICE NAZIONE -> P
002900050103     D TCT             S              1    DIM(600)                             *TIPO (E/R/N)   -> P
003000970117     D*---
003100970117     D* TARIFFE CLIENTE
003200970117     D*---
003300970117     D* DI LAVORO
003400050103     D WCTS            S              2    DIM(600)                             *CODICI
003500050103     D WRAP            S              2    DIM(600)                             *CODICE REGIONE
003600050103     D WNAR            S              2    DIM(600)                             *CODICE NAZIONE -> P
003700050103     D WTCT            S              1    DIM(600)                             *TIPO (E/R/N)   -> P
003800970117     D* DELLA TARIFFA
003900050103     D TCTS            S              2    DIM(600)                             *CODICI -ELEMENTARI-
004000050103     D TRCT            S              1    DIM(600)                             *RAGGRUPPAEMNTO CODI
004100970130     D* DEI TATI TIPO
004200050103     D ECTS            S              2    DIM(600)                             *CODICI -ELEMENTARI-
004300050103     D ERCT            S              1    DIM(600)                             *RAGGRUPPAEMNTO CODI
004400970117     D*------------------------------------------------------------------------*
004500970117     D* INPUT
004600970117     D*------------------------------------------------------------------------*
004700970117     D*-------------------
004800970117     D* DS CODICI TASSAZIONE
004900970117     D*-------------------
005000970117     D DSCT          E DS
005100970117     D*-------------------
005200970117     D* DS PASSAGGIO PARAMETRI
005300970117     D*-------------------
005400970117     D DSTE00        E DS                  EXTNAME(TNTE00DS)
005500970117     D*------------------
005600970117     D* DS "XSRDA8" - CONTROLLA DATA (8)
005700970117     D*------------------
005800970117     D WLBDA8          DS
005900970117     D  G08DAT                 1      8  0
006000970117     D  G08INV                 9     16  0
006100970117     D  G08ERR                17     17
006200970117     D  G08TGI                18     22  0
006300970117     D*-------------------
006400970117     D* ARCHITETTURA
006500970117     D*-------------------
006600970117     D KPJBA         E DS
006700020729     D*-------------------
006800020729     D* DS RIDEFINIZIONE DESCRIZIONE 143 ORGANIGRAMMA
006900020729     D*-------------------
007000020729     D OG143         E DS
007100020729     D*------------------
007200020729     D* DS PER LETTURA TABELLA "NTW"
007300020729     D*------------------
007400020729     D DNTW          E DS
007500970117     C*------------------------------------------------------------------------*
007600970117     C* MAIN LINE
007700970117     C*------------------------------------------------------------------------*
007800970117     C*
007900970117     C* ERRORI IN ENTRATA, ESCE DAL PGM
008000970117IF  1C     $FINE         IFEQ      'N'
008100970117     C*
008200970117     C* POSIZIONAMENTO E PRIMA LETTURA
008300970117     C                   EXSR      SETFIL
008400970117     C*
008500970117     C* CICLO FINO A FINE ARCHIVIO
008600970117DO  2C     $FINE         DOWEQ     'N'
008700970117     C*
008800970117     C* AGGIORNA DATI TIPO -CODICI TASSAZIONE-
008900970117     C                   EXSR      AGGETT
009000970117     C*
009100970117     C* LETTURA SUCESSIVA
009200970117     C                   EXSR      LETFIL
009300970117E   2C                   ENDDO                                                  *FINE COD.TASSAZIONE
009400970117E   1C                   ENDIF
009500970117     C*
009600970117     C                   SETON                                        LR
009700970117     C*------------------------------------------------------------------------*
009800970117     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA      -CODICI TASSAZIONE TARIFFA-
009900970117     C*------------------------------------------------------------------------*
010000970117     C     SETFIL        BEGSR
010100970117     C*
010200970117     C                   Z-ADD     *ZEROS        I
010300970117     C                   EXSR      LETFIL                                       *LETTURA RECORD
010400970117     C*
010500970117     C                   ENDSR
010600970117     C*------------------------------------------------------------------------*
010700970117     C* LETFIL - LETTURA PROSSIMO RECORD             -CODICI TASSAZIONE TARIFFA-
010800970117     C*------------------------------------------------------------------------*
010900970117     C     LETFIL        BEGSR
011000970117     C*
011100970117     C* LETTURA FINO A:
011200970117     C*  - FINE FILE
011300970117     C*  - TROVATO RECORD VALIDO
011400970117     C                   MOVEL     'N'           WRECOK
011500970117DO  1C     $FINE         DOWEQ     'N'
011600970117     C     WRECOK        ANDEQ     'N'
011700970117     C*
011800970117     C                   ADD       1             I                              *LETTURA
011900970117     C*
012000050103IF  2C     I             IFGT      600                                          *FINE ARCHIVIO
012100970117     C     TCTS(I)       OREQ      *BLANKS
012200970117     C                   MOVEL     'S'           $FINE                          *FINE PGM
012300970117X   2C                   ELSE
012400970117     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
012500970117E   2C                   ENDIF
012600970117E   1C                   ENDDO
012700970117     C*
012800970117     C                   ENDSR
012900970117     C*------------------------------------------------------------------------*
013000970117     C* CHKREC - CONTROLLA VALIDITA' RECORD          -CODICI TASSAZIONE TARIFFA-
013100970117     C*------------------------------------------------------------------------*
013200970117     C     CHKREC        BEGSR
013300970117     C*
013400970117     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
013500970117     C*
013600970117     C                   ENDSR
013700970117     C*------------------------------------------------------------------------*
013800970117     C* AGGETT - AGGIORNAMENTO DATI TIPO -CODICI TASSAZIONE-
013900970117     C*------------------------------------------------------------------------*
014000970117     C     AGGETT        BEGSR
014100970117     C*
014200970117     C* LEGGE IL RECORD IN ARCHIVIO
014300091123     C                   MOVEL     'S'           KTTDSF
014400980603     C                   MOVEL     D00CTO        KTTCTO
014500970117     C                   Z-ADD     D00KSC        KTTKSC
014600970117     C                   Z-ADD     D00CTR        KTTCTR
014700970117     C                   Z-ADD     D00PRG        KTTPRG
014800970117     C                   Z-ADD     D00LNP        KTTLNP
014900970117     C                   MOVEL     D00FIE        KTTFIE
015000970117     C                   MOVEL     TCTS(I)       KTTCTS
015100011017     C     KEYETT        CHAIN     TEETT01L                           99
015200970117     C* INSERIMENTO
015300970117IF  1C     *IN99         IFEQ      *ON
015400970117     C                   EXSR      IMPETT                                       *IMPOSTA I CAMPI
015500011017     C                   WRITE     TEETT000
015600011017     C                   UNLOCK    TEETT01L
015700970117X   1C                   ELSE
015800970117     C* VARIAZIONE
015900970117IF  2C     ETTRCT        IFNE      TRCT(I)                                      *SE MODIFICATO QC.
016000970117     C                   EXSR      IMPETT                                       *IMPOSTA I CAMPI
016100011017     C                   UPDATE    TEETT000
016200970117E   2C                   ENDIF
016300970117E   1C                   ENDIF
016400970117     C*
016500970117     C                   ENDSR
016600970117     C*------------------------------------------------------------------------*
016700970117     C* IMPETT - IMPOSTA I CAMPI DEL RECORD DATI TIPO -CODICI TASSAZIONE-
016800970117     C*------------------------------------------------------------------------*
016900970117     C     IMPETT        BEGSR
017000970117     C*
017100970117IF  1C     *IN99         IFEQ      *ON                                          *INSERIMENTO
017200980603     C                   MOVEL     KTTDSF        ETTDSF
017300970117     C                   MOVEL     KTTCTO        ETTCTO
017400970117     C                   Z-ADD     KTTKSC        ETTKSC
017500970117     C                   Z-ADD     KTTCTR        ETTCTR
017600970117     C                   Z-ADD     KTTPRG        ETTPRG
017700970117     C                   Z-ADD     KTTLNP        ETTLNP
017800970117     C                   MOVEL     KTTFIE        ETTFIE
017900970117     C                   MOVEL     KTTCTS        ETTCTS
018000970117     C                   Z-ADD     DATCOR        ETTDIR
018100970117     C                   Z-ADD     *ZEROS        ETTDUV
018200970117     C                   Z-ADD     *ZEROS        ETTPPE
018300970117X   1C                   ELSE                                                   *VARIAZIONE
018400970117     C                   Z-ADD     DATCOR        ETTDUV
018500970117E   1C                   ENDIF
018600970117     C*
018700970117     C                   MOVEL     TRCT(I)       ETTRCT                         *RAGGRUPPAMENTO
018800970117IF  1C     TRCT(I)       IFEQ      *BLANKS
018900970117     C                   Z-ADD     *ZEROS        ETTPPE
019000970117E   1C                   ENDIF
019100970117     C*
019200970130     C                   MOVEL     *BLANKS       ETTATB
019300970130     C                   MOVEL     *BLANKS       ETTFTR
019400970117     C                   Z-ADD     *ZEROS        ETTDTR
019500970117     C*
019600970117     C                   ENDSR
019700970117     C*------------------------------------------------------------------------*
019800970117     C* CARTAD - CARICA I CODICI TASSAZIONE DALLA TARIFFA
019900970117     C*------------------------------------------------------------------------*
020000970117     C     CARTAD        BEGSR
020100970117     C*
020200970117     C* MEMORIZZA I CODICI DELLA LINEA DI PARTENZA RICHIESTA
020300970117     C                   Z-ADD     *ZEROS        I
020400970117     C                   Z-ADD     D00KSC        KTDKSC
020500970117     C                   Z-ADD     D00CTR        KTDCTR
020600970117     C                   Z-ADD     D00PRG        KTDPRG
020700970117     C                   Z-ADD     D00LNP        KTDLNP
020800990819     C     KEYTAD        SETLL     TITAD04L                               99
020900990819     C     KEYTAD        READE     TITAD04L                               99
021000970117DO  1C     *IN99         DOWEQ     *OFF
021100970117IF  2C     TADATB        IFEQ      *BLANKS                                      *NON ANNULLATI
021200970130     C                   SETOFF                                           98
021300970117     C                   Z-ADD     1             J                 4 0
021400970117     C     TADCTS        LOOKUP    CTS(J)                                 98
021500970117IF  3C     *IN98         IFEQ      *ON                                          *TROVATO
021600970117     C                   SETOFF                                           98
021700970117     C                   Z-ADD     1             K                 4 0
021800970117     C     TADCTS        LOOKUP    WCTS(K)                                98
021900970117IF  4C     *IN98         IFEQ      *OFF                                         *MAI MEMORIZZATO
022000970117     C                   ADD       1             I
022100970117     C                   MOVEL     TADCTS        WCTS(I)                        *CODICE
022200970117     C                   MOVEL     RAP(J)        WRAP(I)                        *REGIONE
022300970117     C                   MOVEL     NAR(J)        WNAR(I)                        *NAZIONE
022400970117     C                   MOVEL     TCT(J)        WTCT(I)                        *TIPO
022500970117E   4C                   ENDIF
022600970117E   3C                   ENDIF
022700970117E   2C                   ENDIF
022800990819     C     KEYTAD        READE     TITAD04L                               99
022900970117E   1C                   ENDDO
023000970117     C*
023100970117     C* TRASFORMA I CODICI CARICATI IN CODICI ELEMENTARI
023200970117     C                   EXSR      RAPSVI
023300970130     C*---
023400970130     C* SE INTERROGAZIONE I CODICI TASSAZIONE NON DEVONO ESSERE AGGIORNATI E
023500970130     C* QUINDI PROSEGUE L'ELABORAZIONE SOLO SE L'ARCHIVIO E' GIA' CORRETTO
023600970130     C*---
023700970130IF  1C     $TGES         IFEQ      'I'
023800011017     C                   EXSR      CARETT                                       *CARICA CODICI-TEETT
023900970130     C                   EXSR      CHKETT                                       *CONTROLLA CODICI
024000970130IF  2C     WERR          IFEQ      '1'                                          *ERRORE
024100970130     C                   MOVEL     '1'           D00ERR
024200970130     C                   MOVEL     MSG(1)        D00MSG
024300970130E   2C                   ENDIF
024400970130     C                   MOVEL     'S'           $FINE                          *USCITA PGM -SEMPRE-
024500970130E   1C                   ENDIF
024600970117     C*
024700970117     C                   ENDSR
024800970117     C*------------------------------------------------------------------------*
024900970117     C* RAPSVI - TRASFORMA I CODICI DELLA TARIFFA IN CODICI ELEMENTARI
025000970117     C*------------------------------------------------------------------------*
025100970117     C     RAPSVI        BEGSR
025200970117     C*
025300970117     C* 1蚣IRO-> CARICA TUTTI I CODICI ELEMENTARI COME -NON CALCOLARE-
025400970117     C                   Z-ADD     *ZEROS        J                 4 0
025500050103DO  1C     1             DO        600           I
025600970117IF  2C     CTS(I)        IFNE      *BLANKS
025700970117IF  3C     TCT(I)        IFEQ      'E'                                          *SOLO ELEMENTARI
025800970117     C                   ADD       1             J
025900970117     C                   MOVEL     CTS(I)        TCTS(J)                        *CODICE
026000970117     C                   MOVEL     *BLANKS       TRCT(J)                        *RAGGRUPPAMENTO
026100970117E   3C                   ENDIF
026200970117E   2C                   ENDIF
026300970117E   1C                   ENDDO                                                  *FINE CODICI BASE
026400970117     C*
026500970117     C* 2蚣IRO-> TRASFORMA I -NON CALCOLARE- IN -CALCOLARE IN NAZIONE-
026600050103DO  1C     1             DO        600           I
026700970117IF  2C     WCTS(I)       IFNE      *BLANKS
026800970117IF  3C     WTCT(I)       IFEQ      'N'                                          *SOLO NAZIONE
026900050103DO  4C     1             DO        600           J
027000970117IF  5C     TCTS(J)       IFNE      *BLANKS
027100970117     C                   SETOFF                                           98
027200970117     C                   Z-ADD     1             K                 4 0
027300970117     C     TCTS(J)       LOOKUP    CTS(K)                                 98
027400970117IF  6C     WNAR(I)       IFEQ      NAR(K)
027500970117     C                   MOVEL     'N'           TRCT(J)
027600970117E   6C                   ENDIF
027700970117E   5C                   ENDIF
027800970117E   4C                   ENDDO                                                  *FINE COD.EL.TARIFFA
027900970117E   3C                   ENDIF
028000970117E   2C                   ENDIF
028100970117E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
028200970117     C*
028300970117     C* 3蚣IRO-> TRASFORMA I -CALCOLARE IN NAZIONE- IN -CALCOLARE IN REGIONE-
028400050103DO  1C     1             DO        600           I
028500970117IF  2C     WCTS(I)       IFNE      *BLANKS
028600970117IF  3C     WTCT(I)       IFEQ      'R'                                          *SOLO REGIONE
028700050103DO  4C     1             DO        600           J
028800970117IF  5C     TCTS(J)       IFNE      *BLANKS
028900970117     C                   SETOFF                                           98
029000970117     C                   Z-ADD     1             K                 4 0
029100970117     C     TCTS(J)       LOOKUP    CTS(K)                                 98
029200970117IF  6C     WRAP(I)       IFEQ      RAP(K)
029300970117     C                   MOVEL     'R'           TRCT(J)
029400970117E   6C                   ENDIF
029500970117E   5C                   ENDIF
029600970117E   4C                   ENDDO                                                  *FINE COD.EL.TARIFFA
029700970117E   3C                   ENDIF
029800970117E   2C                   ENDIF
029900970117E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
030000970117     C*
030100970117     C* 4蚣IRO-> TRASFORMA I -CALCOLARE IN REGIONE- IN -CALCOLARE-
030200050103DO  1C     1             DO        600           I
030300970117IF  2C     WCTS(I)       IFNE      *BLANKS
030400970117IF  3C     WTCT(I)       IFEQ      'E'                                          *SOLO ELEMENTARI
030500050103DO  4C     1             DO        600           J
030600970117IF  5C     TCTS(J)       IFNE      *BLANKS
030700970117IF  6C     WCTS(I)       IFEQ      TCTS(J)
030800970117     C                   MOVEL     'T'           TRCT(J)
030900970117E   6C                   ENDIF
031000970117E   5C                   ENDIF
031100970117E   4C                   ENDDO                                                  *FINE COD.EL.TARIFFA
031200970117E   3C                   ENDIF
031300970117E   2C                   ENDIF
031400970117E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
031500970117     C*
031600970117     C                   ENDSR
031700970130     C*------------------------------------------------------------------------*
031800970130     C* CARETT - CARICA I CODICI TASSAZIONE DI DATI TIPO
031900970130     C*------------------------------------------------------------------------*
032000970130     C     CARETT        BEGSR
032100970130     C*
032200970130     C* AZZERA LE SCHIERE DI MEMORIZZAZIONE
032300970130     C                   CLEAR                   ECTS
032400970130     C                   CLEAR                   ERCT
032500970130     C                   Z-ADD     *ZEROS        I
032600970130     C*
032700091123     C                   MOVEL     'S'           KTTDSF
032800970130     C                   MOVEL     D00CTO        KTTCTO
032900970130     C                   Z-ADD     D00KSC        KTTKSC
033000970130     C                   Z-ADD     D00CTR        KTTCTR
033100970130     C                   Z-ADD     D00PRG        KTTPRG
033200970130     C                   Z-ADD     D00LNP        KTTLNP
033300970130     C                   MOVEL     D00FIE        KTTFIE
033400011017     C     KE2ETT        SETLL     TEETT01L
033500011017     C     KE2ETT        READE(N)  TEETT01L                               99
033600970130DO  1C     *IN99         DOWEQ     *OFF
033700970130IF  2C     ETTATB        IFEQ      *BLANKS
033800970130     C                   ADD       1             I
033900970130     C                   MOVEL     ETTCTS        ECTS(I)
034000970130     C                   MOVEL     ETTRCT        ERCT(I)
034100970130E   2C                   ENDIF
034200011017     C     KE2ETT        READE(N)  TEETT01L                               99
034300970130E   1C                   ENDDO
034400970130     C*
034500970130     C                   ENDSR
034600970130     C*------------------------------------------------------------------------*
034700970130     C* CHKETT - CONTROLLA I CODICI TASSAZIONE TIPO CON QUELLI DELLA TARIFFA
034800970130     C*------------------------------------------------------------------------*
034900970130     C     CHKETT        BEGSR
035000970130     C*
035100970130     C* REIMPOSTA VARIABILI DI LAVORO
035200970130     C                   MOVEL     '0'           WERR
035300970130     C*
035400970130     C* CONTROLLA I CODICI DEL DETTAGLIO TARIFFARIO CON QUELLI DEI DATI TIPO
035500050103DO  1C     1             DO        600           I
035600970130IF  2C     TCTS(I)       IFNE      *BLANKS
035700141001IF  2C     TRCT(I)       andne     *BLANKS
035800970130     C                   SETOFF                                           98
035900970130     C                   Z-ADD     1             J
036000970130     C     TCTS(I)       LOOKUP    ECTS(J)                                98
036100970130     C*
036200970130     C* CODICE TASSAZIONE -DATI TIPO- NON TROVATO, ERRORE
036300970130IF  3C     *IN98         IFEQ      *OFF
036400970130     C                   MOVEL     '1'           WERR                           *ERRORE
036500970130     C                   LEAVE
036600970130X   3C                   ELSE
036700970130     C*
036800970130     C* RAGGRUPPAMENTO CODICE DIVERSO, ERRORE
036900970130IF  4C     TRCT(I)       IFNE      ERCT(J)
037000970130     C                   MOVEL     '1'           WERR                           *ERRORE
037100970130     C                   LEAVE
037200970130E   4C                   ENDIF
037300970130E   3C                   ENDIF
037400970130     C*
037500970130E   2C                   ENDIF
037600970130E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
037700970130     C*
037800970130     C                   ENDSR
037900970117     C*------------------------------------------------------------------------*
038000970117     C* CARTAB - CARICA LE TABELLE OCCORRENTI
038100970117     C*------------------------------------------------------------------------*
038200970117     C     CARTAB        BEGSR
038300970117     C*--------------------
038400970117     C* CODICI TASSAZIONE
038500970117     C*--------------------
038600970117     C                   Z-ADD     *ZEROS        I                 4 0
038700020906     C                   Z-ADD     1             KTBKUT
038800970117     C                   MOVEL     'CT'          KTBCOD
038900970117     C     KE2TAB        SETLL     TABEL00F
039000970117     C     KE2TAB        READE     TABEL00F                               99
039100970117DO  1C     *IN99         DOWEQ     *OFF
039200970117IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NON ANNULLATI
039300970312     C                   MOVEL     TBLUNI        DSCT
039400970312IF  3C     低TUTC        IFEQ      'S'                                          *DA CONSIDERARE
039500971204IF  4C     DLPFL1        IFEQ      *BLANKS                                      *SE PARTENZA ITALIA
039600970117     C     D00FIE        ANDEQ     'I'                                          *SE TARIFFA ITALIA
039700970117     C     低TNAR        ANDEQ     *BLANKS                                      *CODICE ITALIA
039800971204     C     DLPFL1        OREQ      *BLANKS                                      *SE PARTENZA ITALIA
039900970117     C     D00FIE        ANDEQ     'E'                                          *SE TARIFFA ESTERO
040000970117     C     低TNAR        ANDNE     *BLANKS                                      *CODICE ESTERO
040100971204     C     DLPFL1        ORNE      *BLANKS                                      *SE PARTENZA ESTERO
040200970117     C     D00FIE        ANDEQ     'E'                                          *SE TARIFFA ESTERO
040300970117     C     低TNAR        ANDEQ     *BLANKS                                      *CODICE ITALIA
040400971204     C     DLPFL1        ORNE      *BLANKS                                      *SE PARTENZA ESTERO
040500970117     C     D00FIE        ANDEQ     'E'                                          *SE TARIFFA ESTERO
040600970117     C     低TNAR        ANDNE     *BLANKS                                      *CODICE ESTERO
040700970117     C                   ADD       1             I
040800970117     C                   MOVEL     TBLKEY        CTS(I)                         *CODICE
040900970117     C                   MOVEL     低TRAP        RAP(I)                         *REGIONE
041000970117     C                   MOVE      低TCOR        WA1               1            *TIPO CODICE
041100970312IF  5C     WA1           IFEQ      '9'
041200970312IF  6C     CTS(I)        IFEQ      'IT'
041300970117     C     CTS(I)        OREQ      'EE'
041400970117     C                   MOVEL     'N'           TCT(I)                         *TIPO "NAZIONE"
041500970312X   6C                   ELSE
041600970117     C                   MOVEL     'R'           TCT(I)                         *TIPO "REGIONE"
041700970312E   6C                   ENDIF
041800970312X   5C                   ELSE
041900970117     C                   MOVEL     'E'           TCT(I)                         *TIPO "ELEMENTARE"
042000970312E   5C                   ENDIF
042100970312IF  5C     低TNAR        IFEQ      *BLANKS
042200970117     C                   MOVEL     'IT'          NAR(I)                         *NAZIONE ITALIA
042300970312X   5C                   ELSE
042400970117     C                   MOVEL     'EE'          NAR(I)                         *NAZIONE ESTERO
042500970312E   5C                   ENDIF
042600970312E   4C                   ENDIF
042700970117E   3C                   ENDIF
042800970117E   2C                   ENDIF
042900970117     C     KE2TAB        READE     TABEL00F                               99
043000970117E   1C                   ENDDO
043100970117     C*
043200970117     C                   ENDSR
043300970117     C*------------------------------------------------------------------------*
043400970117     C* *INZSR - OPERAZIONI INIZIALI
043500970117     C*------------------------------------------------------------------------*
043600970117     C     *INZSR        BEGSR
043700970117     C*--------------------
043800970117     C* RICEVIMENTO PARAMETRI
043900970117     C*--------------------
044000970117     C     *ENTRY        PLIST
044100970117     C                   PARM                    KPJBA
044200970117     C                   PARM                    DSTE00
044300970117     C*--------------------
044400970117     C* VARIABILI RIFERITE AL DATABASE
044500970117     C*--------------------
044600971204     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
044700970117     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
044800970117     C     *LIKE         DEFINE    TBLCOD        KTBCOD
044900970117     C     *LIKE         DEFINE    TBLKEY        KTBKEY
045000970117     C     *LIKE         DEFINE    TADKSC        KTDKSC                         *TNTAD00F
045100970117     C     *LIKE         DEFINE    TADCTR        KTDCTR
045200970117     C     *LIKE         DEFINE    TADPRG        KTDPRG
045300970117     C     *LIKE         DEFINE    TADLNP        KTDLNP
045400011017     C     *LIKE         DEFINE    ETTDSF        KTTDSF                         *TEETT00F
045500980603     C     *LIKE         DEFINE    ETTCTO        KTTCTO
045600970117     C     *LIKE         DEFINE    ETTKSC        KTTKSC
045700970117     C     *LIKE         DEFINE    ETTCTR        KTTCTR
045800970117     C     *LIKE         DEFINE    ETTPRG        KTTPRG
045900970117     C     *LIKE         DEFINE    ETTLNP        KTTLNP
046000970117     C     *LIKE         DEFINE    ETTFIE        KTTFIE
046100970117     C     *LIKE         DEFINE    ETTCTS        KTTCTS
046200020729     C     *LIKE         DEFINE    冷TWFIE       DLPFL1                         *DEPOSITI
046300020729     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
046400020729     C     *LIKE         DEFINE    TBEKE1        KTEKE1
046500970117     C*--------------------
046600970117     C* CHIAVI DI LETTURA
046700970117     C*--------------------
046800971204     C* LETTURA AZORG01L - COMPLETA
046900971204     C     KEYORG        KLIST
047000971204     C                   KFLD                    KORFIL                         *FILIALE
047100970117     C* LETTURA TABEL00F - PARZIALE
047200970117     C     KE2TAB        KLIST
047300970117     C                   KFLD                    KTBKUT                         *UTENTE
047400970117     C                   KFLD                    KTBCOD                         *TABELLA
047500011017     C* LETTURA TEETT01L - COMPLETA
047600970117     C     KEYETT        KLIST
047700980603     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
047800980603     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
047900970117     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
048000970117     C                   KFLD                    KTTCTR                         *TARIFFA
048100970117     C                   KFLD                    KTTPRG                         *PROGRESSIVO
048200970117     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
048300970117     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
048400970117     C                   KFLD                    KTTCTS                         *CODICE TASSAZIONE
048500011017     C* LETTURA TEETT01L - PARZIALE
048600970130     C     KE2ETT        KLIST
048700980603     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
048800970130     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
048900970130     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
049000970130     C                   KFLD                    KTTCTR                         *TARIFFA
049100970130     C                   KFLD                    KTTPRG                         *PROGRESSIVO
049200970130     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
049300970130     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
049400970117     C* LETTURA TNTAD04L - PARZIALE
049500970117     C     KEYTAD        KLIST
049600970117     C                   KFLD                    KTDKSC                         *CLIENTE
049700970117     C                   KFLD                    KTDCTR                         *TARIFFA
049800970117     C                   KFLD                    KTDPRG                         *PROGRESSIVO
049900970117     C                   KFLD                    KTDLNP                         *LINEA PARTENZA
050000020729     C* LETTURA TNTBE01L - PARZIALE
050100020729     C     KEYTBE        KLIST
050200020729     C                   KFLD                    KTECOD                         *UTENTE
050300020729     C                   KFLD                    KTEKE1                         *CODICE
050400970117     C*--------------------
050500970117     C* VARIABILI DI CONTROLLO
050600970117     C*--------------------
050700970117     C                   MOVEL     'N'           $FINE             1            *FINE PROGRAMMA
050800970117     C                   MOVEL     '0'           WERR              1            *ERRORE
050900970117     C                   MOVEL     'S'           WRECOK            1            *VALIDITA'RECORD
051000970130     C                   MOVEL     ' '           $TGES             1            *GESTIONE (I/G)
051100970117     C*
051200970117     C* REPERISCE LA DATA CORRENTE
051300970117     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
051400970117     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
051500970117     C                   Z-ADD     WN8           G08DAT
051600970117     C                   Z-ADD     *ZEROS        G08INV
051700970117     C                   MOVEL     '0'           G08ERR
051800970117     C                   CALL      'XSRDA8'
051900970117     C                   PARM                    WLBDA8
052000970117     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
052100970130     C*
052200970130     C* 02->GESTIONE, 05->INTERROGAZIONE
052300970130     C                   MOVE      D00OP0        WA2               2
052400970130IF  1C     WA2           IFEQ      '02'
052500970130     C                   MOVEL     'G'           $TGES                          *GESTIONE
052600970130X   1C                   ELSE
052700970130     C                   MOVEL     'I'           $TGES                          *INTERROGAZIONE
052800970130E   1C                   ENDIF
052900971204     C*
053000971204     C* CONTROLLA ESISTENZA CODICE IN ARCHIVIO
053100971204     C                   Z-ADD     D00LNP        KORFIL
053200971204     C     KEYORG        CHAIN     AZORG01L                           99
053300971204IF  1C     *IN99         IFEQ      *OFF
053400971204     C     ORGFVA        ANDEQ     *BLANKS
053500971204     C     ORGFAG        ANDNE     'V'
053600020729     C                   MOVEL     ORGDE3        OG143
053700020729     C*
053800020729     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
053900020729     C                   CLEAR                   DNTW
054000020729     C*
054100020729     C                   MOVEL(P)  'NTW'         KTECOD
054200020729     C                   MOVEL(P)  別GNTW        KTEKE1
054300020729     C     KEYTBE        CHAIN     TNTBE01L
054400020729     C                   IF        %found(TNTBE01L)
054500020729     C                   MOVEL     TBEUNI        DNTW
054600020729     C                   ENDIF
054700020729     C*
054800020729     C                   MOVEL     冷TWFIE       DLPFL1
054900971204E   1C                   ENDIF
055000970117     C*
055100970117     C* CARICA LE TABELLE OCCORRENTI
055200970117     C                   EXSR      CARTAB
055300970117     C*
055400970117     C* CARICA I CODICI TASSAZIONE DALLE TARIFFE CLIENTE
055500970117     C                   EXSR      CARTAD
055600970117     C*
055700970117     C                   ENDSR
055800970130     O*------------------------------------------------------------------------*
055900970130** -MSG- MESSAGGI
056000970130TNTE09R- Codici tassazione errati. Entrare dalla gestione tariffe/offerte      1
056100970130TNTE09R- 2
056200970130TNTE09R- 3
056300970130TNTE09R- 4
056400970130TNTE09R- 5
056500970130TNTE09R- 6
056600970130TNTE09R- 7
056700970130TNTE09R- 8
056800970130TNTE09R- 9
056900970130TNTE09R- 10
