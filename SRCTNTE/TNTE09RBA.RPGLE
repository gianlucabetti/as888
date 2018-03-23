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
004400040901     D*---
004500040901     D* tabella nazioni per eex/fed/dpd
004600040901     D*---
004700050103     D naz             S                   DIM(600) inz like(§ctnar)            *CODICE
004800050103     D dfe             S              1    DIM(600) inz                         *REGIONE
004900970117     D*------------------------------------------------------------------------*
005000970117     D* INPUT
005100970117     D*------------------------------------------------------------------------*
005200040901     D*-------------------
005300040901     D* DS nazioni
005400040901     D*-------------------
005500040901     D DS15          E DS
005600970117     D*-------------------
005700970117     D* DS CODICI TASSAZIONE
005800970117     D*-------------------
005900970117     D DSCT          E DS
006000970117     D*-------------------
006100970117     D* DS PASSAGGIO PARAMETRI
006200970117     D*-------------------
006300970117     D DSTE00        E DS                  EXTNAME(TNTE00DS)
006400970117     D*------------------
006500970117     D* DS "XSRDA8" - CONTROLLA DATA (8)
006600970117     D*------------------
006700970117     D WLBDA8          DS
006800970117     D  G08DAT                 1      8  0
006900970117     D  G08INV                 9     16  0
007000970117     D  G08ERR                17     17
007100970117     D  G08TGI                18     22  0
007200970117     D*-------------------
007300970117     D* ARCHITETTURA
007400970117     D*-------------------
007500970117     D KPJBA         E DS
007600020729     D*-------------------
007700020729     D* DS RIDEFINIZIONE DESCRIZIONE 143 ORGANIGRAMMA
007800020729     D*-------------------
007900020729     D OG143         E DS
008000020729     D*------------------
008100020729     D* DS PER LETTURA TABELLA "NTW"
008200020729     D*------------------
008300020729     D DNTW          E DS
008400970117     C*------------------------------------------------------------------------*
008500970117     C* MAIN LINE
008600970117     C*------------------------------------------------------------------------*
008700970117     C*
008800970117     C* ERRORI IN ENTRATA, ESCE DAL PGM
008900970117IF  1C     $FINE         IFEQ      'N'
009000970117     C*
009100970117     C* POSIZIONAMENTO E PRIMA LETTURA
009200970117     C                   EXSR      SETFIL
009300970117     C*
009400970117     C* CICLO FINO A FINE ARCHIVIO
009500970117DO  2C     $FINE         DOWEQ     'N'
009600970117     C*
009700970117     C* AGGIORNA DATI TIPO -CODICI TASSAZIONE-
009800970117     C                   EXSR      AGGETT
009900970117     C*
010000970117     C* LETTURA SUCESSIVA
010100970117     C                   EXSR      LETFIL
010200970117E   2C                   ENDDO                                                  *FINE COD.TASSAZIONE
010300970117E   1C                   ENDIF
010400970117     C*
010500970117     C                   SETON                                        LR
010600970117     C*------------------------------------------------------------------------*
010700970117     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA      -CODICI TASSAZIONE TARIFFA-
010800970117     C*------------------------------------------------------------------------*
010900970117     C     SETFIL        BEGSR
011000970117     C*
011100970117     C                   Z-ADD     *ZEROS        I
011200970117     C                   EXSR      LETFIL                                       *LETTURA RECORD
011300970117     C*
011400970117     C                   ENDSR
011500970117     C*------------------------------------------------------------------------*
011600970117     C* LETFIL - LETTURA PROSSIMO RECORD             -CODICI TASSAZIONE TARIFFA-
011700970117     C*------------------------------------------------------------------------*
011800970117     C     LETFIL        BEGSR
011900970117     C*
012000970117     C* LETTURA FINO A:
012100970117     C*  - FINE FILE
012200970117     C*  - TROVATO RECORD VALIDO
012300970117     C                   MOVEL     'N'           WRECOK
012400970117DO  1C     $FINE         DOWEQ     'N'
012500970117     C     WRECOK        ANDEQ     'N'
012600970117     C*
012700970117     C                   ADD       1             I                              *LETTURA
012800970117     C*
012900050103IF  2C     I             IFGT      600                                          *FINE ARCHIVIO
013000970117     C     TCTS(I)       OREQ      *BLANKS
013100970117     C                   MOVEL     'S'           $FINE                          *FINE PGM
013200970117X   2C                   ELSE
013300970117     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
013400970117E   2C                   ENDIF
013500970117E   1C                   ENDDO
013600970117     C*
013700970117     C                   ENDSR
013800970117     C*------------------------------------------------------------------------*
013900970117     C* CHKREC - CONTROLLA VALIDITA' RECORD          -CODICI TASSAZIONE TARIFFA-
014000970117     C*------------------------------------------------------------------------*
014100970117     C     CHKREC        BEGSR
014200970117     C*
014300970117     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
014400970117     C*
014500970117     C                   ENDSR
014600970117     C*------------------------------------------------------------------------*
014700970117     C* AGGETT - AGGIORNAMENTO DATI TIPO -CODICI TASSAZIONE-
014800970117     C*------------------------------------------------------------------------*
014900970117     C     AGGETT        BEGSR
015000970117     C*
015100970117     C* LEGGE IL RECORD IN ARCHIVIO
015200980603     C                   MOVEL     D00DSF        KTTDSF
015300980603     C                   MOVEL     D00CTO        KTTCTO
015400970117     C                   Z-ADD     D00KSC        KTTKSC
015500970117     C                   Z-ADD     D00CTR        KTTCTR
015600970117     C                   Z-ADD     D00PRG        KTTPRG
015700970117     C                   Z-ADD     D00LNP        KTTLNP
015800970117     C                   MOVEL     D00FIE        KTTFIE
015900040831     C                   MOVEL     D00dpd        KTTdpd
016000970117     C                   MOVEL     TCTS(I)       KTTCTS
016100011017     C     KEYETT        CHAIN     TEETT01L                           99
016200970117     C* INSERIMENTO
016300970117IF  1C     *IN99         IFEQ      *ON
016400970117     C                   EXSR      IMPETT                                       *IMPOSTA I CAMPI
016500011017     C                   WRITE     TEETT000
016600011017     C                   UNLOCK    TEETT01L
016700970117X   1C                   ELSE
016800970117     C* VARIAZIONE
016900970117IF  2C     ETTRCT        IFNE      TRCT(I)                                      *SE MODIFICATO QC.
017000970117     C                   EXSR      IMPETT                                       *IMPOSTA I CAMPI
017100011017     C                   UPDATE    TEETT000
017200970117E   2C                   ENDIF
017300970117E   1C                   ENDIF
017400970117     C*
017500970117     C                   ENDSR
017600970117     C*------------------------------------------------------------------------*
017700970117     C* IMPETT - IMPOSTA I CAMPI DEL RECORD DATI TIPO -CODICI TASSAZIONE-
017800970117     C*------------------------------------------------------------------------*
017900970117     C     IMPETT        BEGSR
018000970117     C*
018100970117IF  1C     *IN99         IFEQ      *ON                                          *INSERIMENTO
018200980603     C                   MOVEL     KTTDSF        ETTDSF
018300970117     C                   MOVEL     KTTCTO        ETTCTO
018400970117     C                   Z-ADD     KTTKSC        ETTKSC
018500970117     C                   Z-ADD     KTTCTR        ETTCTR
018600970117     C                   Z-ADD     KTTPRG        ETTPRG
018700970117     C                   Z-ADD     KTTLNP        ETTLNP
018800970117     C                   MOVEL     KTTFIE        ETTFIE
018900040831     C                   MOVEL     KTTdpd        ETTdpd
019000970117     C                   MOVEL     KTTCTS        ETTCTS
019100970117     C                   Z-ADD     DATCOR        ETTDIR
019200970117     C                   Z-ADD     *ZEROS        ETTDUV
019300970117     C                   Z-ADD     *ZEROS        ETTPPE
019400970117X   1C                   ELSE                                                   *VARIAZIONE
019500970117     C                   Z-ADD     DATCOR        ETTDUV
019600970117E   1C                   ENDIF
019700970117     C*
019800970117     C                   MOVEL     TRCT(I)       ETTRCT                         *RAGGRUPPAMENTO
019900970117IF  1C     TRCT(I)       IFEQ      *BLANKS
020000970117     C                   Z-ADD     *ZEROS        ETTPPE
020100970117E   1C                   ENDIF
020200970117     C*
020300970130     C                   MOVEL     *BLANKS       ETTATB
020400970130     C                   MOVEL     *BLANKS       ETTFTR
020500970117     C                   Z-ADD     *ZEROS        ETTDTR
020600970117     C*
020700970117     C                   ENDSR
020800970117     C*------------------------------------------------------------------------*
020900970117     C* CARTAD - CARICA I CODICI TASSAZIONE DALLA TARIFFA
021000970117     C*------------------------------------------------------------------------*
021100970117     C     CARTAD        BEGSR
021200970117     C*
021300970117     C* MEMORIZZA I CODICI DELLA LINEA DI PARTENZA RICHIESTA
021400970117     C                   Z-ADD     *ZEROS        I
021500970117     C                   Z-ADD     D00KSC        KTDKSC
021600970117     C                   Z-ADD     D00CTR        KTDCTR
021700970117     C                   Z-ADD     D00PRG        KTDPRG
021800970117     C                   Z-ADD     D00LNP        KTDLNP
021900990819     C     KEYTAD        SETLL     TITAD04L                               99
022000990819     C     KEYTAD        READE     TITAD04L                               99
022100970117DO  1C     *IN99         DOWEQ     *OFF
022200970117IF  2C     TADATB        IFEQ      *BLANKS                                      *NON ANNULLATI
022300970130     C                   SETOFF                                           98
022400970117     C                   Z-ADD     1             J                 4 0
022500970117     C     TADCTS        LOOKUP    CTS(J)                                 98
022600970117IF  3C     *IN98         IFEQ      *ON                                          *TROVATO
022700970117     C                   SETOFF                                           98
022800970117     C                   Z-ADD     1             K                 4 0
022900970117     C     TADCTS        LOOKUP    WCTS(K)                                98
023000970117IF  4C     *IN98         IFEQ      *OFF                                         *MAI MEMORIZZATO
023100970117     C                   ADD       1             I
023200970117     C                   MOVEL     TADCTS        WCTS(I)                        *CODICE
023300970117     C                   MOVEL     RAP(J)        WRAP(I)                        *REGIONE
023400970117     C                   MOVEL     NAR(J)        WNAR(I)                        *NAZIONE
023500970117     C                   MOVEL     TCT(J)        WTCT(I)                        *TIPO
023600970117E   4C                   ENDIF
023700970117E   3C                   ENDIF
023800970117E   2C                   ENDIF
023900990819     C     KEYTAD        READE     TITAD04L                               99
024000970117E   1C                   ENDDO
024100970117     C*
024200970117     C* TRASFORMA I CODICI CARICATI IN CODICI ELEMENTARI
024300970117     C                   EXSR      RAPSVI
024400970130     C*---
024500970130     C* SE INTERROGAZIONE I CODICI TASSAZIONE NON DEVONO ESSERE AGGIORNATI E
024600970130     C* QUINDI PROSEGUE L'ELABORAZIONE SOLO SE L'ARCHIVIO E' GIA' CORRETTO
024700970130     C*---
024800970130IF  1C     $TGES         IFEQ      'I'
024900011017     C                   EXSR      CARETT                                       *CARICA CODICI-TEETT
025000970130     C                   EXSR      CHKETT                                       *CONTROLLA CODICI
025100970130IF  2C     WERR          IFEQ      '1'                                          *ERRORE
025200970130     C                   MOVEL     '1'           D00ERR
025300970130     C                   MOVEL     MSG(1)        D00MSG
025400970130E   2C                   ENDIF
025500970130     C                   MOVEL     'S'           $FINE                          *USCITA PGM -SEMPRE-
025600970130E   1C                   ENDIF
025700970117     C*
025800970117     C                   ENDSR
025900970117     C*------------------------------------------------------------------------*
026000970117     C* RAPSVI - TRASFORMA I CODICI DELLA TARIFFA IN CODICI ELEMENTARI
026100970117     C*------------------------------------------------------------------------*
026200970117     C     RAPSVI        BEGSR
026300970117     C*
026400970117     C* 1°GIRO-> CARICA TUTTI I CODICI ELEMENTARI COME -NON CALCOLARE-
026500970117     C                   Z-ADD     *ZEROS        J                 4 0
026600050103DO  1C     1             DO        600           I
026700970117IF  2C     CTS(I)        IFNE      *BLANKS
026800970117IF  3C     TCT(I)        IFEQ      'E'                                          *SOLO ELEMENTARI
026900970117     C                   ADD       1             J
027000970117     C                   MOVEL     CTS(I)        TCTS(J)                        *CODICE
027100970117     C                   MOVEL     *BLANKS       TRCT(J)                        *RAGGRUPPAMENTO
027200970117E   3C                   ENDIF
027300970117E   2C                   ENDIF
027400970117E   1C                   ENDDO                                                  *FINE CODICI BASE
027500970117     C*
027600970117     C* 2°GIRO-> TRASFORMA I -NON CALCOLARE- IN -CALCOLARE IN NAZIONE-
027700050103DO  1C     1             DO        600           I
027800970117IF  2C     WCTS(I)       IFNE      *BLANKS
027900970117IF  3C     WTCT(I)       IFEQ      'N'                                          *SOLO NAZIONE
028000050103DO  4C     1             DO        600           J
028100970117IF  5C     TCTS(J)       IFNE      *BLANKS
028200970117     C                   SETOFF                                           98
028300970117     C                   Z-ADD     1             K                 4 0
028400970117     C     TCTS(J)       LOOKUP    CTS(K)                                 98
028500970117IF  6C     WNAR(I)       IFEQ      NAR(K)
028600970117     C                   MOVEL     'N'           TRCT(J)
028700970117E   6C                   ENDIF
028800970117E   5C                   ENDIF
028900970117E   4C                   ENDDO                                                  *FINE COD.EL.TARIFFA
029000970117E   3C                   ENDIF
029100970117E   2C                   ENDIF
029200970117E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
029300970117     C*
029400970117     C* 3°GIRO-> TRASFORMA I -CALCOLARE IN NAZIONE- IN -CALCOLARE IN REGIONE-
029500050103DO  1C     1             DO        600           I
029600970117IF  2C     WCTS(I)       IFNE      *BLANKS
029700970117IF  3C     WTCT(I)       IFEQ      'R'                                          *SOLO REGIONE
029800050103DO  4C     1             DO        600           J
029900970117IF  5C     TCTS(J)       IFNE      *BLANKS
030000970117     C                   SETOFF                                           98
030100970117     C                   Z-ADD     1             K                 4 0
030200970117     C     TCTS(J)       LOOKUP    CTS(K)                                 98
030300970117IF  6C     WRAP(I)       IFEQ      RAP(K)
030400970117     C                   MOVEL     'R'           TRCT(J)
030500970117E   6C                   ENDIF
030600970117E   5C                   ENDIF
030700970117E   4C                   ENDDO                                                  *FINE COD.EL.TARIFFA
030800970117E   3C                   ENDIF
030900970117E   2C                   ENDIF
031000970117E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
031100970117     C*
031200970117     C* 4°GIRO-> TRASFORMA I -CALCOLARE IN REGIONE- IN -CALCOLARE-
031300050103DO  1C     1             DO        600           I
031400970117IF  2C     WCTS(I)       IFNE      *BLANKS
031500970117IF  3C     WTCT(I)       IFEQ      'E'                                          *SOLO ELEMENTARI
031600050103DO  4C     1             DO        600           J
031700970117IF  5C     TCTS(J)       IFNE      *BLANKS
031800970117IF  6C     WCTS(I)       IFEQ      TCTS(J)
031900970117     C                   MOVEL     'T'           TRCT(J)
032000970117E   6C                   ENDIF
032100970117E   5C                   ENDIF
032200970117E   4C                   ENDDO                                                  *FINE COD.EL.TARIFFA
032300970117E   3C                   ENDIF
032400970117E   2C                   ENDIF
032500970117E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
032600970117     C*
032700970117     C                   ENDSR
032800970130     C*------------------------------------------------------------------------*
032900970130     C* CARETT - CARICA I CODICI TASSAZIONE DI DATI TIPO
033000970130     C*------------------------------------------------------------------------*
033100970130     C     CARETT        BEGSR
033200970130     C*
033300970130     C* AZZERA LE SCHIERE DI MEMORIZZAZIONE
033400970130     C                   CLEAR                   ECTS
033500970130     C                   CLEAR                   ERCT
033600970130     C                   Z-ADD     *ZEROS        I
033700970130     C*
033800980603     C                   MOVEL     D00DSF        KTTDSF
033900970130     C                   MOVEL     D00CTO        KTTCTO
034000970130     C                   Z-ADD     D00KSC        KTTKSC
034100970130     C                   Z-ADD     D00CTR        KTTCTR
034200970130     C                   Z-ADD     D00PRG        KTTPRG
034300970130     C                   Z-ADD     D00LNP        KTTLNP
034400970130     C                   MOVEL     D00FIE        KTTFIE
034500040831     C                   MOVEL     D00dpd        KTTdpd
034600011017     C     KE2ETT        SETLL     TEETT01L
034700011017     C     KE2ETT        READE(N)  TEETT01L                               99
034800970130DO  1C     *IN99         DOWEQ     *OFF
034900970130IF  2C     ETTATB        IFEQ      *BLANKS
035000970130     C                   ADD       1             I
035100970130     C                   MOVEL     ETTCTS        ECTS(I)
035200970130     C                   MOVEL     ETTRCT        ERCT(I)
035300970130E   2C                   ENDIF
035400011017     C     KE2ETT        READE(N)  TEETT01L                               99
035500970130E   1C                   ENDDO
035600970130     C*
035700970130     C                   ENDSR
035800970130     C*------------------------------------------------------------------------*
035900970130     C* CHKETT - CONTROLLA I CODICI TASSAZIONE TIPO CON QUELLI DELLA TARIFFA
036000970130     C*------------------------------------------------------------------------*
036100970130     C     CHKETT        BEGSR
036200970130     C*
036300970130     C* REIMPOSTA VARIABILI DI LAVORO
036400970130     C                   MOVEL     '0'           WERR
036500970130     C*
036600970130     C* CONTROLLA I CODICI DEL DETTAGLIO TARIFFARIO CON QUELLI DEI DATI TIPO
036700050103DO  1C     1             DO        600           I
036800970130IF  2C     TCTS(I)       IFNE      *BLANKS
036900970130     C                   SETOFF                                           98
037000970130     C                   Z-ADD     1             J
037100970130     C     TCTS(I)       LOOKUP    ECTS(J)                                98
037200970130     C*
037300970130     C* CODICE TASSAZIONE -DATI TIPO- NON TROVATO, ERRORE
037400970130IF  3C     *IN98         IFEQ      *OFF
037500970130     C                   MOVEL     '1'           WERR                           *ERRORE
037600970130     C                   LEAVE
037700970130X   3C                   ELSE
037800970130     C*
037900970130     C* RAGGRUPPAMENTO CODICE DIVERSO, ERRORE
038000970130IF  4C     TRCT(I)       IFNE      ERCT(J)
038100970130     C                   MOVEL     '1'           WERR                           *ERRORE
038200970130     C                   LEAVE
038300970130E   4C                   ENDIF
038400970130E   3C                   ENDIF
038500970130     C*
038600970130E   2C                   ENDIF
038700970130E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
038800970130     C*
038900970130     C                   ENDSR
039000970117     C*------------------------------------------------------------------------*
039100970117     C* CARTAB - CARICA LE TABELLE OCCORRENTI
039200970117     C*------------------------------------------------------------------------*
039300970117     C     CARTAB        BEGSR
039400040901     c* carico nazioni gestite dai network
039500040901     c                   if        d00fie = 'E' or d00dpd <> ' '
039600040901     c                   exsr      srnaz
039700040901     c                   end
039800970117     C*--------------------
039900970117     C* CODICI TASSAZIONE
040000970117     C*--------------------
040100970117     C                   Z-ADD     *ZEROS        I                 4 0
040200020906     C                   Z-ADD     1             KTBKUT
040300970117     C                   MOVEL     'CT'          KTBCOD
040400970117     C     KE2TAB        SETLL     TABEL00F
040500040901DO  1C                   DO        *hival
040600040901     C     KE2TAB        READE     TABEL00F
040700040901DO  1C                   if        %eof(tabel00f)
040800040901     c                   leave
040900040901     c                   end
041000040901IF  2C     TBLFLG        IFne      *BLANKS                                      *NON ANNULLATI
041100040901     c                   iter
041200040901     c                   end
041300970312     C                   MOVEL     TBLUNI        DSCT
041400040901     c* utilizzo in tariffa
041500040901IF  3C     §CTUTC        IFne      'S'                                          *DA CONSIDERARE
041600040901     c                   iter
041700040901     c                   end
041800040902     c* per fed solo tariffe fed e viceversa
041900040902IF  3C     §CTUTf        IFeq      'S'                                          *DA CONSIDERARE
042000040902IF  3C     d00dpd        andne     'F'                                          *DA CONSIDERARE
042100040902     c                   iter
042200040902     c                   end
042300040902IF  3C     §CTUTf        IFne      'S'                                          *DA CONSIDERARE
042400040902IF  3C     d00dpd        andeq     'F'                                          *DA CONSIDERARE
042500040902     c                   iter
042600040902     c                   end
042700040902     c* per dpd no cod tassazione EE
042800040902     c                   if        tblkey = 'EE' and d00dpd = 'D'
042900040902     c                   iter
043000040902     c                   end
043100040901     c* verifico tab. delle nazioni x vedere se paese servito dal network
043200040901     c                   if        d00fie = 'E' or d00dpd <> ' '
043300040901     c     §ctnar        lookup    naz                                    99
043400040901     c                   if        not *in99
043500040901     c                   iter
043600040901     c                   end
043700040901     c                   end
043800971204IF  4C     DLPFL1        IFEQ      *BLANKS                                      *SE PARTENZA ITALIA
043900970117     C     D00FIE        ANDEQ     'I'                                          *SE TARIFFA ITALIA
044000970117     C     §CTNAR        ANDEQ     *BLANKS                                      *CODICE ITALIA
044100040831     C     d00dpd        ANDeq     ' '                                          *CODICI ITALIA
044200040901     C     D00FIE        orEQ      'I'                                          *TARIFFA ITALIA
044300040831     C     d00dpd        ANDeq     'D'                                          *CODICI ITALIA
044400040831     C     §CTNAR        ANDne     *BLANKS                                      *CODICI ITALIA
044500971204     C     DLPFL1        OREQ      *BLANKS                                      *SE PARTENZA ITALIA
044600970117     C     D00FIE        ANDEQ     'E'                                          *SE TARIFFA ESTERO
044700970117     C     §CTNAR        ANDNE     *BLANKS                                      *CODICE ESTERO
044800971204     C     DLPFL1        ORNE      *BLANKS                                      *SE PARTENZA ESTERO
044900970117     C     D00FIE        ANDEQ     'E'                                          *SE TARIFFA ESTERO
045000970117     C     §CTNAR        ANDEQ     *BLANKS                                      *CODICE ITALIA
045100971204     C     DLPFL1        ORNE      *BLANKS                                      *SE PARTENZA ESTERO
045200970117     C     D00FIE        ANDEQ     'E'                                          *SE TARIFFA ESTERO
045300970117     C     §CTNAR        ANDNE     *BLANKS                                      *CODICE ESTERO
045400970117     C                   ADD       1             I
045500970117     C                   MOVEL     TBLKEY        CTS(I)                         *CODICE
045600970117     C                   MOVEL     §CTRAP        RAP(I)                         *REGIONE
045700970117     C                   MOVE      §CTCOR        WA1               1            *TIPO CODICE
045800970312IF  5C     WA1           IFEQ      '9'
045900970312IF  6C     CTS(I)        IFEQ      'IT'
046000970117     C     CTS(I)        OREQ      'EE'
046100970117     C                   MOVEL     'N'           TCT(I)                         *TIPO "NAZIONE"
046200970312X   6C                   ELSE
046300970117     C                   MOVEL     'R'           TCT(I)                         *TIPO "REGIONE"
046400040903     c* per fedex carico sia come regione che come codice elementare
046500040903     c                   if        d00dpd = 'F'
046600040903     C                   MOVEL     'E'           TCT(I)                         *TIPO "REGIONE"
046700040903     c                   end
046800970312E   6C                   ENDIF
046900970312X   5C                   ELSE
047000970117     C                   MOVEL     'E'           TCT(I)                         *TIPO "ELEMENTARE"
047100970312E   5C                   ENDIF
047200970312IF  5C     §CTNAR        IFEQ      *BLANKS
047300970117     C                   MOVEL     'IT'          NAR(I)                         *NAZIONE ITALIA
047400970312X   5C                   ELSE
047500970117     C                   MOVEL     'EE'          NAR(I)                         *NAZIONE ESTERO
047600970117E   3C                   ENDIF
047700970117E   2C                   ENDIF
047800970117E   1C                   ENDDO
047900970117     C*
048000970117     C                   ENDSR
048100040901     C*------------------------------------------------------------------------*
048200040901     C* leggo tabella delle nazioni x vedere se paese servito dal network
048300040901     C*------------------------------------------------------------------------*
048400040901     C     srnaz         BEGSR
048500040901     c                   clear                   naz
048600040901     c                   clear                   dfe
048700040901     c                   clear                   nz                3 0
048800040901     C                   Z-ADD     1             KTBKUT
048900040901     C                   MOVEL     '15'          KTBCOD
049000040901     C     KE2TAB        SETLL     TABEL00F
049100040901     c                   do        *hival
049200040901     C     KE2TAB        READE     TABEL00F
049300040901DO  1C                   if        %eof(tabel00f)
049400040901     c                   leave
049500040901     c                   end
049600040901IF  2C     TBLFLG        IFne      *BLANKS                                      *NON ANNULLATI
049700040901     c                   iter
049800040901     c                   end
049900040901     C                   MOVEL     TBLUNI        DS15
050000040901     c* EEX
050100040901     c                   if        d00fie = 'E' and d00dpd = *blanks
050200040901     c                             and §15eex = 'S'
050300040901     c                   add       1             nz
050400040902     c                   movel     'E'           dfe(nz)
050500040901     c                   movel     tblkey        naz(nz)
050600040901     c                   iter
050700040901     c                   end
050800040901     c* FED
050900040901     c                   if        d00fie = 'E' and d00dpd = 'F'
051000040901     c                             and §15laf <> 0
051100040901     c                   add       1             nz
051200040902     c                   movel     'F'           dfe(nz)
051300040901     c                   movel     tblkey        naz(nz)
051400040901     c                   iter
051500040901     c                   end
051600040901     c* FED
051700040901     c                   if        d00fie = 'I' and d00dpd = 'D'
051800040901     c                             and §15lad <> 0
051900040901     c                   add       1             nz
052000040902     c                   movel     'D'           dfe(nz)
052100040901     c                   movel     tblkey        naz(nz)
052200040901     c                   iter
052300040901     c                   end
052400040901     c                   enddo
052500040901     C                   ENDSR
052600970117     C*------------------------------------------------------------------------*
052700970117     C* *INZSR - OPERAZIONI INIZIALI
052800970117     C*------------------------------------------------------------------------*
052900970117     C     *INZSR        BEGSR
053000970117     C*--------------------
053100970117     C* RICEVIMENTO PARAMETRI
053200970117     C*--------------------
053300970117     C     *ENTRY        PLIST
053400970117     C                   PARM                    KPJBA
053500970117     C                   PARM                    DSTE00
053600970117     C*--------------------
053700970117     C* VARIABILI RIFERITE AL DATABASE
053800970117     C*--------------------
053900971204     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
054000970117     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
054100970117     C     *LIKE         DEFINE    TBLCOD        KTBCOD
054200970117     C     *LIKE         DEFINE    TBLKEY        KTBKEY
054300970117     C     *LIKE         DEFINE    TADKSC        KTDKSC                         *TNTAD00F
054400970117     C     *LIKE         DEFINE    TADCTR        KTDCTR
054500970117     C     *LIKE         DEFINE    TADPRG        KTDPRG
054600970117     C     *LIKE         DEFINE    TADLNP        KTDLNP
054700011017     C     *LIKE         DEFINE    ETTDSF        KTTDSF                         *TEETT00F
054800980603     C     *LIKE         DEFINE    ETTCTO        KTTCTO
054900970117     C     *LIKE         DEFINE    ETTKSC        KTTKSC
055000970117     C     *LIKE         DEFINE    ETTCTR        KTTCTR
055100970117     C     *LIKE         DEFINE    ETTPRG        KTTPRG
055200970117     C     *LIKE         DEFINE    ETTLNP        KTTLNP
055300970117     C     *LIKE         DEFINE    ETTFIE        KTTFIE
055400040831     C     *LIKE         DEFINE    ETTdpd        KTTdpd
055500970117     C     *LIKE         DEFINE    ETTCTS        KTTCTS
055600020729     C     *LIKE         DEFINE    §NTWFIE       DLPFL1                         *DEPOSITI
055700020729     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
055800020729     C     *LIKE         DEFINE    TBEKE1        KTEKE1
055900970117     C*--------------------
056000970117     C* CHIAVI DI LETTURA
056100970117     C*--------------------
056200971204     C* LETTURA AZORG01L - COMPLETA
056300971204     C     KEYORG        KLIST
056400971204     C                   KFLD                    KORFIL                         *FILIALE
056500970117     C* LETTURA TABEL00F - PARZIALE
056600970117     C     KE2TAB        KLIST
056700970117     C                   KFLD                    KTBKUT                         *UTENTE
056800970117     C                   KFLD                    KTBCOD                         *TABELLA
056900011017     C* LETTURA TEETT01L - COMPLETA
057000970117     C     KEYETT        KLIST
057100980603     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
057200980603     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
057300970117     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
057400970117     C                   KFLD                    KTTCTR                         *TARIFFA
057500970117     C                   KFLD                    KTTPRG                         *PROGRESSIVO
057600970117     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
057700970117     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
057800040831     C                   KFLD                    KTTdpd                         *ITALIA/ESTERO
057900970117     C                   KFLD                    KTTCTS                         *CODICE TASSAZIONE
058000011017     C* LETTURA TEETT01L - PARZIALE
058100970130     C     KE2ETT        KLIST
058200980603     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
058300970130     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
058400970130     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
058500970130     C                   KFLD                    KTTCTR                         *TARIFFA
058600970130     C                   KFLD                    KTTPRG                         *PROGRESSIVO
058700970130     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
058800970130     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
058900040831     C                   KFLD                    KTTdpd                         *ITALIA/ESTERO
059000970117     C* LETTURA TNTAD04L - PARZIALE
059100970117     C     KEYTAD        KLIST
059200970117     C                   KFLD                    KTDKSC                         *CLIENTE
059300970117     C                   KFLD                    KTDCTR                         *TARIFFA
059400970117     C                   KFLD                    KTDPRG                         *PROGRESSIVO
059500970117     C                   KFLD                    KTDLNP                         *LINEA PARTENZA
059600020729     C* LETTURA TNTBE01L - PARZIALE
059700020729     C     KEYTBE        KLIST
059800020729     C                   KFLD                    KTECOD                         *UTENTE
059900020729     C                   KFLD                    KTEKE1                         *CODICE
060000970117     C*--------------------
060100970117     C* VARIABILI DI CONTROLLO
060200970117     C*--------------------
060300970117     C                   MOVEL     'N'           $FINE             1            *FINE PROGRAMMA
060400970117     C                   MOVEL     '0'           WERR              1            *ERRORE
060500970117     C                   MOVEL     'S'           WRECOK            1            *VALIDITA'RECORD
060600970130     C                   MOVEL     ' '           $TGES             1            *GESTIONE (I/G)
060700970117     C*
060800970117     C* REPERISCE LA DATA CORRENTE
060900970117     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
061000970117     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
061100970117     C                   Z-ADD     WN8           G08DAT
061200970117     C                   Z-ADD     *ZEROS        G08INV
061300970117     C                   MOVEL     '0'           G08ERR
061400970117     C                   CALL      'XSRDA8'
061500970117     C                   PARM                    WLBDA8
061600970117     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
061700970130     C*
061800970130     C* 02->GESTIONE, 05->INTERROGAZIONE
061900970130     C                   MOVE      D00OP0        WA2               2
062000970130IF  1C     WA2           IFEQ      '02'
062100970130     C                   MOVEL     'G'           $TGES                          *GESTIONE
062200970130X   1C                   ELSE
062300970130     C                   MOVEL     'I'           $TGES                          *INTERROGAZIONE
062400970130E   1C                   ENDIF
062500971204     C*
062600971204     C* CONTROLLA ESISTENZA CODICE IN ARCHIVIO
062700971204     C                   Z-ADD     D00LNP        KORFIL
062800971204     C     KEYORG        CHAIN     AZORG01L                           99
062900971204IF  1C     *IN99         IFEQ      *OFF
063000971204     C     ORGFVA        ANDEQ     *BLANKS
063100971204     C     ORGFAG        ANDNE     'V'
063200020729     C                   MOVEL     ORGDE3        OG143
063300020729     C*
063400020729     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
063500020729     C                   CLEAR                   DNTW
063600020729     C*
063700020729     C                   MOVEL(P)  'NTW'         KTECOD
063800020729     C                   MOVEL(P)  §OGNTW        KTEKE1
063900020729     C     KEYTBE        CHAIN     TNTBE01L
064000020729     C                   IF        %found(TNTBE01L)
064100020729     C                   MOVEL     TBEUNI        DNTW
064200020729     C                   ENDIF
064300020729     C*
064400020729     C                   MOVEL     §NTWFIE       DLPFL1
064500971204E   1C                   ENDIF
064600970117     C*
064700970117     C* CARICA LE TABELLE OCCORRENTI
064800970117     C                   EXSR      CARTAB
064900970117     C*
065000970117     C* CARICA I CODICI TASSAZIONE DALLE TARIFFE CLIENTE
065100970117     C                   EXSR      CARTAD
065200970117     C*
065300970117     C                   ENDSR
065400970130     O*------------------------------------------------------------------------*
065500970130** -MSG- MESSAGGI
065600970130TNTE09R- Codici tassazione errati. Entrare dalla gestione tariffe/offerte      1
065700970130TNTE09R- 2
065800970130TNTE09R- 3
065900970130TNTE09R- 4
066000970130TNTE09R- 5
066100970130TNTE09R- 6
066200970130TNTE09R- 7
066300970130TNTE09R- 8
066400970130TNTE09R- 9
066500970130TNTE09R- 10
