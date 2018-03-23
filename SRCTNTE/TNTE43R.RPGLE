000100961227     H*------------------------------------------------------------------------*
000200961227     H* TARIFFE DA CONTO ECONOMICO - STAMPA DATI TIPO - ESEGUE                 *
000300961227     H*------------------------------------------------------------------------*
000400961227     H DECEDIT('0,') DATEDIT(*DMY.)
000500961227     F*------------------------------------------------------------------------*
000600961227     F* DATA BASE
000700961227     F*------------------------------------------------------------------------*
000800020730     FTNTBE01L  IF   E           K DISK
000900011002     FTEETC01L  IF   E           K DISK
001000011002     FTEETS01L  IF   E           K DISK
001100060830     FTEETS11L  IF   E           K DISK    RENAME(TEETS000:TEETS100)
001200011002     FTEETT01L  IF   E           K DISK
001300970512     FAZORG01L  IF   E           K DISK
001400970512     FCNACO00F  IF   E           K DISK
001500970512     FTABEL00F  IF   E           K DISK
001600970512     FTNTAM01L  IF   E           K DISK
001700990820     FTITAD04L  IF   E           K DISK
001800041202     FTFACO00F  IF   E           K DISK    usropn
001900970512     F                                     RENAME(CNACO000:TFACO000)
002100041202     FTNOFM01L  IF   E           K DISK    usropn
002200961227     F                                     RENAME(TNTAM000:TNOFM000)
002300041202     FTIOFD01L  IF   E           K DISK    usropn
002400990820     F                                     RENAME(TITAD000:TIOFD000)
002500961227     FTNTE41P   O    E             PRINTER OFLIND(*IN01)
002600961227     D*------------------------------------------------------------------------*
002700961227     D* SCHIERE
002800961227     D*------------------------------------------------------------------------*
002900961227     D*------------------
003000961227     D* DECODIFICHE
003100961227     D*------------------
003200961227     D DDEC            S             30    DIM(20) CTDATA PERRCD(1)
003300961227     D*------------------
003400961227     D* ORGANIGRAMMA
003500961227     D*------------------
003600971124     D ORG             S              3  0 DIM(500)                             *CODICE
003700971124     D DORG            S             20    DIM(500)                             *DESCRIZIONE
003800971204     D FL1             S              1    DIM(500)                             *FLAG ITALIA/ESTERO
003900970512     D*------------------
004000970512     D* SELEZIONI
004100970512     D*------------------
004200971124     D SORG            S              3  0 DIM(500)                             *FILIALI
004300961227     D*-------------------
004400961227     D* TIPI TARIFFA
004500961227     D*-------------------
004600030127     D TTR             S              1    DIM(15)                              *CODICE
004700030127     D DTTR            S             20    DIM(15)                              *DESCRIZIONE
004800961227     D*-------------------
004900961227     D* CODICI TASSAZIONE
005000961227     D*-------------------
005100050103     D CTS             S              2    DIM(600)                             *CODICE
005200050103     D TCT             S              1    DIM(600)                             *TIPO
005300050103     D DCTS            S             20    DIM(600)                             *DESCRIZIONE
005400050103     D COR             S              3    DIM(600)                             *ORDINAMENTO
005500050103     D FCR             S              1    DIM(600)                             *FLAG CAPOLUOGO
005600050103     D RAP             S              2    DIM(600)                             *REGIONE
005700050103     D NAR             S              2    DIM(600)                             *NAZIONE
005800970418     D* DI RAPPRESENTAZIONE
005900050103     D OCTS            S              2    DIM(600)                             *CODICE
006000050103     D ORCT            S              1    DIM(600)                             *RAGGRUPPAMENTO
006100050103     D OCOR            S              3    DIM(600)                             *ORDINAMENTO
006200050103     D OTCT            S              1    DIM(600)                             *TIPO
006300050103     D OPPE            S              4  1 DIM(600)                             *%INCIDENZA
006400050103     D OFTR            S              1    DIM(600)                             *TIPO
006500961227     D*------------------
006600961227     D* TABELLE CAPICONTO
006700961227     D*------------------
006800961227     D RI              S              4  0 DIM(4)                               *TAB RAG. CI
006900961227     D*------------------------------------------------------------------------*
007000961227     D* INPUT
007100961227     D*------------------------------------------------------------------------*
007101091125     D tnte04ds      E DS
007102151202     D detcflo       E DS
007200961227     D*-------------------
007300961227     D* DS TIPI TARIFFA
007400961227     D*-------------------
007500961227     D DSTR          E DS
007600090611     D* DS TIPI servizio
007700090611     D*-------------------
007800090611     D DS5e          E DS
007900961227     D*-------------------
008000961227     D* DS CODICI TASSAZIONE
008100961227     D*-------------------
008200961227     D DSCT          E DS
008300961227     D*------------------
008400020830     D* DS "X§PARUT" - RICERCA PARAMETRI UTENTE
008500961227     D*------------------
008600961227     D CARCNT          DS
008700961227     D  FL12CN                 1      2
008800961227     D  FL34CN                 3      4
008900961227     D  TIPCNT                 5      6
009000020906     D PARUT         E DS                  EXTNAME(UT§DSE0F)
009100961227     D  FLU                   66     97
009200961227     D                                     DIM(32)                              FLAG PAR.UTENT
009300961227     D  TCU                  398    697
009400961227     D                                     DIM(50)                              *TAB FLG 8  TP.CN
009500961227     D  KCU                  698    847P 0
009600961227     D                                     DIM(50)                              *TAB CAPICONTO
009700961227     D                                     PACKEVEN
009800961227     D TCUDS           DS
009900961227     D  E1                     1      1
010000961227     D  E3                     3      3
010100961227     D  E2                     2      2
010200961227     D  E4                     4      4
010300961227     D  E56                    5      6
010400961227     D*------------------
010500961227     D* DS "XSRDA8" - CONTROLLA DATA (8)
010600961227     D*------------------
010700961227     D WLBDA8          DS
010800961227     D  G08DAT                 1      8  0
010900961227     D  G08INV                 9     16  0
011000961227     D  G08ERR                17     17
011100961227     D  G08TGI                18     22  0
011200961227     D*------------------
011300961227     D* DS SCOMPOSIZIONE DESCRIZIONE VIDEO
011400961227     D*------------------
011500961227     D* DDEC,3
011600961227     D                 DS
011700961227     D  D03RCT                14     30
011800961227     D  DDEC03                 1     30
011900961227     D*-------------------
012000961227     D* DS DI PROCEDURA
012100961227     D*-------------------
012200961227     D DSTE00        E DS                  EXTNAME(TNTE00DS)
012300961227     D*------------------
012400961227     D* ARCHITETTURA
012500961227     D*------------------
012600961227     D KPJBA         E DS
012700961227     D  PROFIL                 1     10
012800040804     D*------------------
012900040804     D* DS PARAMETRI DI LANCIO
013000040804     D*------------------
013100040804     D PARAM           DS
013200040804     D  PARKSC                 1      7  0
013300040804     D  PARFPA                 8     10  0
013400040804     D  PARFAR                11     13  0
013500040804     D  PARFAP                14     16  0
013600040804     D  PARDS1                17     24  0
013700040804     D  PARAA1                17     20  0
013800040804     D  PARMG1                21     24  0
013900040804     D  PARDS2                25     32  0
014000040804     D  PARTAR                33     35
014100040804     D  PARCTO                36     36
014200040804     D  PARKOV                37     43  0
014300040804     D  PARCAR                37     41  0
014400040804     D  PARCTR                44     46  0
014500040804     D  PARTTR                44     44
014600040804     D  PARPRG                47     49  0
014700040804     D  PARLNP                50     52  0
014800040804     D  PARFIE                53     53
014900040804     D  PARSIM                54     54
015000040804     D  PARELA                55     55
015100040804     D  PARDIV                56     58
015200040804     D  PARUNI                59     59
015300040804     D  PARsta                60     60
015400040804     D  PARint                61     61
015401091130     D  PARsie                62     62
015402091130     D  PARdsf                63     63
015500020730     D*-------------------
015600020730     D* DS RIDEFINIZIONE DESCRIZIONE 143 ORGANIGRAMMA
015700020730     D*-------------------
015800020730     D OG143         E DS
015900020730     D*------------------
016000020730     D* DS PER LETTURA TABELLA "NTW"
016100020730     D*------------------
016200020730     D DNTW          E DS
016300961227     C*------------------------------------------------------------------------*
016400961227     C* MAIN LINE
016500961227     C*------------------------------------------------------------------------*
016600961227     C*
016700961227     C* STAMPA I PARAMETRI DI LANCIO
016800961227     C                   EXSR      STMPAR
016900961227     C*
017000961227     C* POSIZIONAMENTO E PRIMA LETTURA -DATI TIPO GENERALI-
017100961227     C                   EXSR      SETFIL
017200961227     C*
017300961227     C* CICLO FINO A FINE FILE
017400961227DO  1C     $FINE         DOWEQ     '0'
017500970512     C*
017600970512     C* OPERAZIONE PER NUOVA LINEA DI PARTENZA
017700970512     C                   EXSR      NEWLNP
017800970512     C*
017900970512     C* CICLO FINO A ROTTURA LINEA DI PARTENZA
018000970512DO  2C     $FINE         DOWEQ     '0'
018100970512     C     DEPLNP        ANDEQ     ETCLNP
018200961227     C*
018300961227     C* STAMPA TUTTI I DATI TIPO
018400961227     C                   EXSR      STMETX
018500961227     C*
018600961227     C* LETTURA SUCESSIVA -DATI TIPO GENERALI-
018700961227     C                   EXSR      LETFIL
018800970512E   2C                   ENDDO                                                  *ROTTURA L.PARTENZA
018900970512E   1C                   ENDDO                                                  *FINE FILE
019000961227     C*
019100961227     C* STAMPA RIGA DI FINE PROSPETTO
019200961227     C                   WRITE     TE41FI
019300961227     C*
019400961227     C                   SETON                                        LR
019500961227     C*------------------------------------------------------------------------*
019600961227     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA -DATI TIPO GENERALI-
019700961227     C*------------------------------------------------------------------------*
019800961227     C     SETFIL        BEGSR
019900961227     C*
020000091124     C                   MOVEL     'S'           KTCDSF                         *DATI SEDE O FILIALE
020100980603     C                   MOVEL     D00CTO        KTCCTO                         *TARIFFA O OFFERTA
020200970117     C                   Z-ADD     D00KSC        KTCKSC                         *CLIENTE O VISITA
020300961227     C                   Z-ADD     D00CTR        KTCCTR                         *TARIFFA
020400961227     C                   Z-ADD     D00PRG        KTCPRG                         *PROGRESSIVO
020500970512     C                   Z-ADD     SORG(1)       KTCLNP                         *LINEA DI PARTENZA
020600011002     C     KEYETC        SETLL     TEETC01L                           99
020700961227IF  1C     *IN99         IFEQ      *ON
020800961227     C                   MOVEL     '1'           $FINE                          *FINE PGM
020900961227X   1C                   ELSE
021000961227     C                   EXSR      LETFIL                                       *LETTURA RECORD
021100961227E   1C                   ENDIF
021200961227     C*
021300961227     C                   ENDSR
021400961227     C*------------------------------------------------------------------------*
021500961227     C* LETFIL - LETTURA SUCESSIVA -DATI TIPO GENERALI-
021600961227     C*------------------------------------------------------------------------*
021700961227     C     LETFIL        BEGSR
021800961227     C*
021900961227     C* LETTURA FINO A:
022000961227     C*  - FINE FILE
022100961227     C*  - TROVATO RECORD VALIDO
022200961227     C                   MOVEL     'N'           WRECOK
022300961227DO  1C     $FINE         DOWEQ     '0'
022400961227     C     WRECOK        ANDEQ     'N'
022500011002     C     KE2ETC        READE     TEETC01L                               99
022600961227IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
022700961227     C                   MOVEL     '1'           $FINE
022800961227X   2C                   ELSE
022900961227     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
023000961227E   2C                   ENDIF
023100961227E   1C                   ENDDO
023200961227     C*
023300961227     C                   ENDSR
023400961227     C*------------------------------------------------------------------------*
023500961227     C* CHKREC - CONTROLLA VALIDITA' RECORD -DATI TIPO GENERALI-
023600961227     C*------------------------------------------------------------------------*
023700961227     C     CHKREC        BEGSR
023800961227     C*
023900961227     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
024000970117     C*
024100970117     C* RECORD ANNULLATO, ESCLUDE
024200970117IF  1C     ETCATB        IFNE      *BLANKS
024300970117     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
024400970117E   1C                   ENDIF
024500961227     C*
024600970512     C* LINEA DI PARTENZA <> DA QUELLA/E RICHIESTA/E, ESCLUDE
024700970512     C                   Z-ADD     1             I
024800970512     C                   SETOFF                                           28
024900970512     C     ETCLNP        LOOKUP    SORG(I)                                28
025000970512IF  1C     *IN28         IFEQ      *OFF                                         *NON SELEZIONATA
025100961227     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
025200961227E   1C                   ENDIF
025300961227     C*
025400961227     C                   ENDSR
025500970512     C*------------------------------------------------------------------------*
025600970512     C* NEWLNP - OPERAZIONI PER NUOVA LINEA DI PARTENZA
025700970512     C*------------------------------------------------------------------------*
025800970512     C     NEWLNP        BEGSR
025900970512     C*
026000970512     C* REIMPOSTA LE VARIABILI DI LAVORO
026100970512     C                   CLEAR                   OCTS
026200970512     C                   CLEAR                   OTCT
026300970512     C                   CLEAR                   OCOR
026400970512     C                   CLEAR                   ORCT
026500970512     C                   CLEAR                   OPPE
026600040805     C                   CLEAR                   OFTR
026700971204     C*
026800971204     C* MEMORIZZA IL NUOVO CODICE IN UN DEPOSITO
026900971204     C                   Z-ADD     ETCLNP        DEPLNP
027000971204     C*
027100971204     C* REPERISCE DATI FILIALE
027200971204     C                   Z-ADD     1             I
027300971204     C                   SETOFF                                           98
027400971204     C     DEPLNP        LOOKUP    ORG(I)                                 98
027500971204IF  1C     *IN98         IFEQ      *ON
027600971204     C                   MOVEL     FL1(I)        DLPFL1
027700971204X   1C                   ELSE
027800971204     C                   MOVEL     *BLANKS       DLPFL1
027900971204E   1C                   ENDIF
028000970512     C*
028100970512     C* CARICA I CODICI TASSAZIONE SECONDO LA RAPPRESENTAZIONE RICHIESTA
028200970512IF  1C     D00SCT        IFEQ      ' '                                          *DA TARIFFA
028300970512     C                   Z-ADD     *ZEROS        I
028400970512     C                   Z-ADD     D00KSC        KTDKSC
028500970512     C                   Z-ADD     D00CTR        KTDCTR
028600970512     C                   Z-ADD     D00PRG        KTDPRG
028700970512     C                   Z-ADD     ETCLNP        KTDLNP
028800970512IF  2C     D00CTO        IFEQ      'T'                                          *TARIFFA
028900990820     C     KEYTAD        SETLL     TITAD04L                               99
029000990820     C     KEYTAD        READE     TITAD04L                               99
029100970512X   2C                   ELSE                                                   *OFFERTA
029200990820     C     KEYTAD        SETLL     TIOFD01L                               99
029300990820     C     KEYTAD        READE     TIOFD01L                               99
029400970512E   2C                   ENDIF
029500970512DO  2C     *IN99         DOWEQ     *OFF
029600970512IF  3C     TADATB        IFEQ      *BLANKS                                      *NON ANNULLATI
029700970512     C                   SETOFF                                           98
029800970512     C                   Z-ADD     1             J                 4 0
029900970512     C     TADCTS        LOOKUP    CTS(J)                                 98
030000970512IF  4C     *IN98         IFEQ      *ON                                          *TROVATO
030100971204IF  5C     DLPFL1        IFEQ      *BLANKS                                      *SE PARTENZA ITALIA
030200970512     C     D00SIE        ANDEQ     'I'                                          *SE TARIFFA ITALIA
030300970512     C     NAR(J)        ANDEQ     'IT'                                         *CODICE ITALIA
030400971204     C     DLPFL1        OREQ      *BLANKS                                      *SE PARTENZA ITALIA
030500970512     C     D00SIE        ANDEQ     'E'                                          *SE TARIFFA ESTERO
030600970512     C     NAR(J)        ANDNE     'IT'                                         *CODICE ESTERO
030700971204     C     DLPFL1        ORNE      *BLANKS                                      *SE PARTENZA ESTERO
030800970512     C     D00SIE        ANDEQ     'E'                                          *SE TARIFFA ESTERO
030900970512     C     NAR(J)        ANDEQ     'IT'                                         *CODICE ITALIA
031000971204     C     DLPFL1        ORNE      *BLANKS                                      *SE PARTENZA ESTERO
031100970512     C     D00SIE        ANDEQ     'E'                                          *SE TARIFFA ESTERO
031200970512     C     NAR(J)        ANDNE     'IT'                                         *CODICE ESTERO
031300970512     C                   SETOFF                                           98
031400970512     C                   Z-ADD     1             K                 4 0
031500970512     C     TADCTS        LOOKUP    OCTS(K)                                98
031600970512IF  6C     *IN98         IFEQ      *OFF                                         *MAI MEMORIZZATO
031700970512     C                   ADD       1             I
031800970512     C                   MOVEL     TADCTS        OCTS(I)                        *CODICE
031900970512     C                   MOVEL     TCT(J)        OTCT(I)                        *TIPO
032000970512     C                   MOVEL     COR(J)        OCOR(I)                        *ORDINAMENTO
032100970512     C                   MOVEL     *BLANKS       ORCT(I)                        *RAGGRUPPAMENTO
032200970512     C                   Z-ADD     *ZEROS        OPPE(I)                        *% INCIDENZA
032300970512E   6C                   ENDIF
032400970512E   5C                   ENDIF
032500970512E   4C                   ENDIF
032600970512E   3C                   ENDIF
032700970512IF  3C     D00CTO        IFEQ      'T'                                          *TARIFFA
032800990820     C     KEYTAD        READE     TITAD04L                               99
032900970512X   3C                   ELSE                                                   *OFFERTA
033000990820     C     KEYTAD        READE     TIOFD01L                               99
033100970512E   3C                   ENDIF
033200970512E   2C                   ENDDO
033300970512E   1C                   ENDIF
033400970512     C*
033500970512     C* PER REGIONE / IN DETTAGLIO
033600970512IF  1C     D00SCT        IFEQ      'R'                                          *PER REGIONE
033700970512     C     D00SCT        OREQ      'T'                                          *IN DETTAGLIO
033800970512     C                   Z-ADD     *ZEROS        I
033900050103DO  2C     1             DO        600           J
034000970512IF  3C     CTS(J)        IFNE      *BLANKS
034100971204IF  4C     DLPFL1        IFEQ      *BLANKS                                      *SE PARTENZA ITALIA
034200970512     C     D00SIE        ANDEQ     'I'                                          *SE TARIFFA ITALIA
034300970512     C     NAR(J)        ANDEQ     'IT'                                         *CODICE ITALIA
034400971204     C     DLPFL1        OREQ      *BLANKS                                      *SE PARTENZA ITALIA
034500970512     C     D00SIE        ANDEQ     'E'                                          *SE TARIFFA ESTERO
034600970512     C     NAR(J)        ANDNE     'IT'                                         *CODICE ESTERO
034700971204     C     DLPFL1        ORNE      *BLANKS                                      *SE PARTENZA ESTERO
034800970512     C     D00SIE        ANDEQ     'E'                                          *SE TARIFFA ESTERO
034900970512     C     NAR(J)        ANDEQ     'IT'                                         *CODICE ITALIA
035000971204     C     DLPFL1        ORNE      *BLANKS                                      *SE PARTENZA ESTERO
035100970512     C     D00SIE        ANDEQ     'E'                                          *SE TARIFFA ESTERO
035200970512     C     NAR(J)        ANDNE     'IT'                                         *CODICE ESTERO
035300970512IF  5C     D00SCT        IFEQ      'R'                                          *PER REGIONE
035400970512     C     TCT(J)        ANDEQ     'R'                                          *CTS REGIONALI
035500970512     C     D00SCT        OREQ      'T'                                          *IN DETTAGLIO
035600970512     C     TCT(J)        ANDEQ     'E'                                          *CTS ELEMENTARI
035700970512     C                   ADD       1             I
035800970512     C                   MOVEL     CTS(J)        OCTS(I)                        *CODICE
035900970512     C                   MOVEL     TCT(J)        OTCT(I)                        *TIPO
036000970512     C                   MOVEL     COR(J)        OCOR(I)                        *ORDINAMENTO
036100970512     C                   MOVEL     *BLANKS       ORCT(I)                        *RAGGRUPPAMENTO
036200970512     C                   Z-ADD     *ZEROS        OPPE(I)                        *% INCIDENZA
036300970512E   5C                   ENDIF
036400970512E   4C                   ENDIF
036500970512E   3C                   ENDIF
036600970512E   2C                   ENDDO
036700970512E   1C                   ENDIF
036800970512     C*
036900970512     C                   ENDSR
037000961227     C*------------------------------------------------------------------------*
037100961227     C* STMETX - STAMPA TUTTI I DATI TIPO
037200961227     C*------------------------------------------------------------------------*
037300961227     C     STMETX        BEGSR
037400961227     C*---
037500961227     C* DATI TIPO -GENERALI-
037600961227     C*---
037700961227     C                   EXSR      STMINC                                       *STAMPA INTESTAZIONE
037800961227     C                   EXSR      STMETC                                       *STAMPA IL RECORD
037900961227     C                   WRITE     TE41R2                                       *RIGA DI SEPARAZIONE
038000961227     C*---
038100961227     C* DATI TIPO -SCAGLIONI-
038200961227     C*---
038300961227     C                   EXSR      STMINS                                       *STAMPA INTESTAZIONE
038400980603     C                   MOVEL     ETCDSF        KTSDSF                         *DATI SEDE O FILIALE
038500970117     C                   MOVEL     ETCCTO        KTSCTO                         *TARIFFA O OFFERTA
038600970117     C                   Z-ADD     ETCKSC        KTSKSC                         *CLIENTE O VISITA
038700961227     C                   Z-ADD     ETCCTR        KTSCTR                         *TARIFFA
038800961227     C                   Z-ADD     ETCPRG        KTSPRG                         *PROGRESSIVO
038900961227     C                   Z-ADD     ETCLNP        KTSLNP                         *LINEA DI PARTENZA
039000060830C*   C* SCAGLIONI RICAVI
039100060830     C                   SETOFF                                       44
039200011002     C     KEYETS        SETLL     TEETS01L
039300011002     C     KEYETS        READE     TEETS01L                               98
039400961227DO  1C     *IN98         DOWEQ     *OFF
039500970117IF  2C     ETSATB        IFEQ      *BLANKS
039600961227     C                   EXSR      STMETS                                       *STAMPA IL RECORD
039700970117E   2C                   ENDIF
039800011002     C     KEYETS        READE     TEETS01L                               98
039900961227E   1C                   ENDDO
040000060830C*   C* SCAGLIONI COMPETENZE
040100060830     C     KEYETS        SETLL     TEETS11L
040200060830     C     KEYETS        READE     TEETS11L                               98
040300060830DO  1C     *IN98         IFEQ      *OFF
040400060830IF  2C     ETSATB        ANDEQ     *BLANKS
040500060830     C                   SETON                                        44
040600060830DO  1C     *IN01         IFEQ      *OFF
040700060830     C                   WRITE     TE41ISB
040800060830     C                   END
040900060830     C                   END
041000060830DO  1C     *IN98         DOWEQ     *OFF
041100060830IF  2C     ETSATB        IFEQ      *BLANKS
041200060830     C                   EXSR      STMETS                                       *STAMPA IL RECORD
041300060830E   2C                   ENDIF
041400060830     C     KEYETS        READE     TEETS11L                               98
041500060830E   1C                   ENDDO
041600961227     C*---
041700961227     C* DATI TIPO -CODICI TASSAZIONE-
041800961227     C*---
041900961227     C                   WRITE     TE41R2                                       *RIGA DI SEPARAZIONE
042000961227     C                   EXSR      STAETT
042100961227     C                   WRITE     TE41R1                                       *RIGA DI SEPARAZIONE
042200961227     C*
042300961227     C                   ENDSR
042400961227     C*------------------------------------------------------------------------*
042500961227     C* STMINC - STAMPA INTESTAZIONE DATI TIPO -GENERALI-
042600961227     C*------------------------------------------------------------------------*
042700961227     C     STMINC        BEGSR
042800961227     C*
042801091201     c                   movel     'PROGRESSIVO:'indprg
042802091124     c                   select
042900091124IF  1C                   when      d00cto = 'O'
042901091201     c                   if        d00dsf = *blanks
042902091201     C                   MOVEL     'VISITA '     INDCTO
042903091201     c                   else
042904091201     C                   MOVEL     'TRATT. '     INDCTO
042905091201     c                   movel     'VERSIONE...:'indprg
042906091201     c                   end
043100091124X   1C                   when      d00cto = 'T'
043200091201     C                   MOVEL     'CLIENTE'     INDCTO
043300091125E   1C                   ENDsl
043400060828     c                   if        kcdaz = 'TE3N'
043500970117     C                   Z-ADD     PACKSC        INCKSC
043600970117     C                   MOVEL     PADKSC        INCRAG
043700040804     c                   else
043800040804     C                   Z-ADD     PACKov        INCKSC
043900040804     C                   MOVEL     PADKov        INCRAG
044000040804     c                   end
044100961227     C                   Z-ADD     PACCTR        INCCTR
044200961227     C                   MOVEL     PADCTR        INDCTR
044300961227     C                   Z-ADD     PACPRG        INCPRG
044400961227     C                   Z-ADD     ETCLNP        INCLNP
044500011003     C                   MOVEL     PACDIV        INCDIV
044600961227     C                   Z-ADD     1             I
044700961227     C                   SETOFF                                           98
044800040806IF  2C                   IF        inclnp = 888 or inclnp = 889
044900040806IF  2C     inclnp        IFEQ      888
045000040806     C                   MOVEL     DDEC(9)       INDLNP
045100040806E   2C                   ENDIF
045200040806IF  2C     inclnp        IFEQ      889
045300040806     C                   MOVEL     DDEC(10)      INDLNP
045400040806E   2C                   ENDIF
045500040806     c                   else
045600961227     C     INCLNP        LOOKUP    ORG(I)                                 98
045700961227IF  1C     *IN98         IFEQ      *ON
045800961227     C                   MOVEL     DORG(I)       INDLNP
045900961227X   1C                   ELSE
046000961227     C                   MOVEL     *ALL'*'       INDLNP
046100961227E   1C                   ENDIF
046200040806E   1C                   ENDIF
046300961227     C*
046400961227     C                   WRITE     TE41TE                                       *TESTATA
046500961227     C                   WRITE     TE41IN                                       *INTESTAZIONE
046600961227     C                   WRITE     TE41R1                                       *RIGA DI SEPARAZIONE
046700961227     C                   SETOFF                                       01
046800961227     C*
046900961227     C                   ENDSR
047000961227     C*------------------------------------------------------------------------*
047100961227     C* STMETC - STAMPA I DATI TIPO -GENERALI-
047200961227     C*------------------------------------------------------------------------*
047300961227     C     STMETC        BEGSR
047400961227     C*
047401151202     c                   movel     etcflo        detcflo
047500961227     C                   MOVEL     ETCCLV        DC1CLV
047600961227     C                   MOVEL     ETCBOL        DC1BOL
047700961227     C                   Z-ADD     ETCGGP        DC1GGP
047800961227     C                   Z-ADD     ETCSPR        DC1SPR
047900970417     C*---                 Z-ADDETCSPC    DC1SPC
048000970108     C                   Z-ADD     ETCPMC        DC1PMC
048100970108     C                   Z-ADD     ETCVMC        DC1VMC
048200961227     C                   Z-ADD     ETCFRA        DC1FRA
048300961227     C                   Z-ADD     ETCASS        DC1ASS
048400961227     C                   Z-ADD     ETCPCV        DC1PCV
048500961227     C                   Z-ADD     ETCPCM        DC1PCM
048600961227     C                   Z-ADD     ETCPFD        DC1PFD
048700961227     C                   Z-ADD     ETCPCA        DC1PCA
048800961227     C                   MOVEL     ETCIAP        DC1IAP
048900961227     C                   Z-ADD     ETCPCP        DC1PCP
049000961227     C                   MOVEL     ETCIPI        DC1IPI
049100961227     C                   Z-ADD     ETCPCS        DC1PCS
049200961227     C                   MOVEL     ETCISU        DC1ISU
049300961227     C                   Z-ADD     ETCPCF        DC1PCF
049400961227     C                   MOVEL     ETCIFU        DC1IFU
049500970418     C                   Z-ADD     ETCPCD        DC1PCD
049600970418     C                   MOVEL     ETCIDD        DC1IDD
049700970418     C                   Z-ADD     ETCPCC        DC1PCC
049800970418     C                   MOVEL     ETCICE        DC1ICE
049900970418     C                   Z-ADD     ETCPCL        DC1PCL
050000970418     C                   MOVEL     ETCILO        DC1ILO
050100031204     C                   Z-ADD     ETCCIS        DC1CIS
050200031204     C                   MOVEL     ETCISO        DC1ISO
050300031204     C                   Z-ADD     ETCSAM        DC1SAM
050400031204     C                   MOVEL     ETCINR        DC1INR
050500021104     C*
050600021104     C                   IF        ETCDKC = 'S'
050700021104     C                   EVAL      DC1DKC = 'SI'
050800021104     C                   ELSE
050900021104     C                   EVAL      DC1DKC = 'NO'
051000021104     C                   ENDIF
051100021104     C                   IF        ETCIX1 = *blanks
051200021104     C                   MOVE(P)   'AUTOTRASP.'  DC1IX1
051300021104     C                   ENDIF
051400021104     C                   IF        ETCIX1 = 'C'
051500021104     C                   MOVE(P)   'COOPERATIVA' DC1IX1
051600021104     C                   ENDIF
051700021104     C                   IF        ETCIX1 = 'K'
051800021104     C                   MOVE(P)   'CLIENTE'     DC1IX1
051900021104     C                   ENDIF
052000021104     C                   IF        ETCIX2 = *blanks
052100021104     C                   MOVE(P)   'AUTOTRASP.'  DC1IX2
052200021104     C                   ENDIF
052300021104     C                   IF        ETCIX2 = 'K'
052400021104     C                   MOVE(P)   'COOPERATIVA' DC1IX2
052500021104     C                   ENDIF
052600040805     C                   MOVEL     ETCFAR        DC1FAR
052700040805     C                   Z-ADD     ETCIAR        DC1IAR
052800040805     C                   MOVEL     ETCDCV        DC1DCV
053000050808     C                   MOVEL     ETCpkl        DC1pkl
053001151202     C                   MOVEL     §ETCrtm       DC1rtm
053100040805     C                   Z-ADD     ETCval        DC1val
053200040805     C                   Z-ADD     ETCdes        DC1des
053300050810     C                   MOVEL     ETCIDE        DC1IDE
053400021104     C*
053500021104     C                   WRITE     TE41IC1                                      *DESCRIZIONE CAMPI-1
053600021104     C                   WRITE     TE41DC1                                      *CAMPI -1-
053700021104     C                   WRITE     TE41IC2                                      *DESCRIZIONE CAMPI-2
053800021104     C                   WRITE     TE41DC2                                      *CAMPI -2-
053900961227     C*
054000961227     C                   Z-ADD     ETCPPV        DC2PPV
054100961227     C                   MOVEL     ETCSIN        DC2SIN
054200961227     C                   Z-ADD     ETCESP        DC2ESP
054300961227     C                   MOVEL     ETCIES        DC2IES
054400961227     C                   Z-ADD     ETCPAB        DC2PAB
054500961227     C                   Z-ADD     ETCPAM        DC2PAM
054600961227     C                   Z-ADD     ETCCAS        DC2CAS
054700961227     C                   MOVEL     ETCICA        DC2ICA
054800961227     C                   Z-ADD     ETCPAR        DC2PAR
054900060315     C                   Z-ADD     ETCxx1        DC2xx1
055000961227     C                   Z-ADD     ETCPAC        DC2PAC
055100961227     C                   Z-ADD     ETCASC        DC2ASC
055200961227     C                   MOVEL     ETCIAS        DC2IAS
055300961227     C                   Z-ADD     ETCPGI        DC2PGI
055400961227     C                   Z-ADD     ETCGGG        DC2GGG
055500970512     C*---                 Z-ADDETCNRG    DC2NRG
055600961227     C                   MOVEL     ETCIGI        DC2IGI
055700040804     C                   Z-ADD     ETCNba        DC2Nba
055800040824     C                   Z-ADD     ETCban        DC2ban
055900050810     C                   MOVEL     ETCIBA        DC2IBA
056000040824     C                   Z-ADD     ETCbnb        DC2bnb
056100040811     C                   Z-ADD     ETCNbn        DC2nbn
056200961227     C*
056300021104     C                   WRITE     TE41IC3                                      *DESCRIZIONE CAMPI-3
056400021104     C                   WRITE     TE41DC3                                      *CAMPI -3-
056500961227     C*
056600961227     C                   ENDSR
056700961227     C*------------------------------------------------------------------------*
056800961227     C* STMINS - STAMPA INTESTAZIONE DATI TIPO -SCAGLIONI-
056900961227     C*------------------------------------------------------------------------*
057000961227     C     STMINS        BEGSR
057100961227     C*
057200961227IF  1C     *IN01         IFEQ      *ON                                          *FINE PAGINA
057300961227     C                   WRITE     TE41TE                                       *TESTATA
057400961227     C                   WRITE     TE41IN                                       *INTESTAZIONE
057500961227     C                   SETOFF                                       01
057600961227E   1C                   ENDIF
057700961227     C*
057800961227     C                   MOVEL     ETCCTR        WA1
057900961227IF  1C     WA1           IFEQ      '1'
058000961227     C                   MOVEL     'COLLI'       ISDSGL
058100961227X   1C                   ELSE
058200961227     C                   MOVEL     ' KG  '       ISDSGL
058300961227E   1C                   ENDIF
058400060830     C                   SETOFF                                       44
058500060830     C                   WRITE     TE41ISB                                      *DESCRIZIONE CAMPI
058600060830     C                   WRITE     TE41IS                                       *DESCRIZIONE CAMPI
058700961227     C*
058800961227     C                   ENDSR
058900961227     C*------------------------------------------------------------------------*
059000961227     C* STMETS - STAMPA I DATI TIPO -SCAGLIONI-
059100961227     C*------------------------------------------------------------------------*
059200961227     C     STMETS        BEGSR
059300961227     C*
059400961227IF  1C     *IN01         IFEQ      *ON                                          *FINE PAGINA
059500961227     C                   WRITE     TE41TE                                       *TESTATA
059600961227     C                   WRITE     TE41IN                                       *INTESTAZIONE
059700060830     C                   WRITE     TE41ISB                                      *DESCRIZIONE CAMPI
059800060830     C                   WRITE     TE41IS                                       *DESCRIZIONE CAMPI
059900961227     C                   SETOFF                                       01
060000961227E   1C                   ENDIF
060100961227     C*
060200961227     C                   Z-ADD     ETSSGL        DSCSGL
060300961227     C                   Z-ADD     ETSPSC        DSCPSC
060400961227     C                   Z-ADD     ETSCSC        DSCCSC
060500961227     C                   Z-ADD     ETSVSC        DSCVSC
060600970318     C                   Z-ADD     ETSINL        DSCINL
060700970318     C                   Z-ADD     ETSRPV        DSCRPV
060800970415     C                   MOVEL     ETSSDE        DSCSDE
060900970415     C                   Z-ADD     ETSPDE        DSCPDE
061000970415     C                   Z-ADD     ETSPPE        DSCPPE
061100040805     C* HO UTILIZZO IL CAMPO FTR X MEMORIZZARE SE IL VALORE È STATO DETTO
061200040805     C* OPPURE SE HO AVUTO DELLE SPEDIZIONI. AD OGNI AGGIORNAMENTO
061300040805     C* DEL RECORD VERRÀ PULITO E QUINDI PERDO IL CONTENUTO ORIGINALE
061400040805     C* MA È GIUSTO COSÌ
061500040805     C                   MOVEL     *BLANKS       DSDDED
061600040805     C                   IF        ETSFTR = '*'
061700040805     C                   EVAL      DSDDED ='NESSUNA SPEDIZIONE: VALORE DEDOTTO'
061800040805     C                   END
061900961227     C*
062000961227     C                   WRITE     TE41DS                                       *CAMPI
062100961227     C*
062200961227     C                   ENDSR
062300961227     C*------------------------------------------------------------------------*
062400961227     C* STAETT - IMPOSTA LA STAMPA DEI DATI TIPO -CODICI TASSAZIONE-
062500961227     C*------------------------------------------------------------------------*
062600961227     C     STAETT        BEGSR
062700961227     C*
062800970418     C* MEMORIZZA I CODICI TASSAZIONE IN SCHIERE
062900961227     C                   EXSR      STMINT                                       *STAMPA INTESTAZIONE
063000091124     C                   MOVEL     'S'           KTTDSF                         *DATI SEDE O FILIALE
063100970117     C                   MOVEL     ETCCTO        KTTCTO                         *TARIFFA O OFFERTA
063200970117     C                   Z-ADD     ETCKSC        KTTKSC                         *CLIENTE O VISITA
063300961227     C                   Z-ADD     ETCCTR        KTTCTR                         *TARIFFA
063400961227     C                   Z-ADD     ETCPRG        KTTPRG                         *PROGRESSIVO
063500961227     C                   Z-ADD     ETCLNP        KTTLNP                         *LINEA DI PARTENZA
063600970418     C                   MOVEL     D00SIE        KTTFIE                         *TARIFFA ITA/EST
063700011002     C     KEYETT        SETLL     TEETT01L
063800011002     C     KEYETT        READE     TEETT01L                               98
063900961227DO  1C     *IN98         DOWEQ     *OFF
064000961227IF  2C     ETTRCT        IFNE      *BLANKS                                      *DA CALCOLARE
064100970117IF  3C     ETTATB        IFEQ      *BLANKS
064200970418     C*
064300970418     C* REPERISCE I DATI DEL CODICE TASSAZIONE DA MEMORIZZARE
064400970418     C                   SETOFF                                           28
064500961227     C                   Z-ADD     1             J                 4 0
064600970418     C     ETTCTS        LOOKUP    CTS(J)                                 28
064700970418IF  4C     *IN28         IFEQ      *ON                                          *TROVATO
064800970418     C*
064900970418     C* CERCA IL CODICE COME ELEMENTARE
065000970418     C                   SETOFF                                           27
065100970418     C                   Z-ADD     1             I                 4 0
065200970418     C     ETTCTS        LOOKUP    OCTS(I)                                27
065300970418IF  5C     *IN27         IFEQ      *OFF                                         *NON TROVATO
065400970418     C*
065500970418     C* CERCA IL CODICE COME REGIONALE, SE NON TROVATO
065600970418     C                   SETOFF                                           27
065700970418     C                   Z-ADD     1             I                 4 0
065800970418     C     RAP(J)        LOOKUP    OCTS(I)                                27
065900970418IF  6C     *IN27         IFEQ      *OFF                                         *NON TROVATO
066000970418     C*
066100970418     C* CERCA IL CODICE COME NAZIONALE, SE NON TROVATO
066200970418     C                   SETOFF                                           27
066300970418     C                   Z-ADD     1             I                 4 0
066400970418     C     NAR(J)        LOOKUP    OCTS(I)                                27
066500970418E   6C                   ENDIF
066600970418E   5C                   ENDIF
066700970418     C*
066800970418     C* CODICE RECUPERATO, MEMORIZZA I DATI
066900970418IF  5C     *IN27         IFEQ      *ON
067000970418     C                   ADD       ETTPPE        OPPE(I)                        *% INCIDENZA
067100040805     C                   MOVEL     ETTFTR        OFTR(I)                        *RAGGRUPP.VERO
067200970418IF  6C     TCT(J)        IFEQ      'E'                                          *ELEMENTARE
067300970421IF  7C     D00SCT        IFEQ      'T'                                          *RAPPR.COD.TASS.DETT
067400040805     C                   MOVEL     ETTRCT        ORCT(I)                        *RAGGRUPP.VERO
067500970421E   7C                   ENDIF
067600970421E   6C                   ENDIF
067700970421E   5C                   ENDIF
067800970418E   4C                   ENDIF
067900970418E   3C                   ENDIF
068000961227E   2C                   ENDIF
068100011002     C     KEYETT        READE     TEETT01L                               98
068200961227E   1C                   ENDDO
068300961227     C*
068400961227     C* ORDINA I CODICI TASSAZIONE MEMORIZZATI
068500961227     C                   EXSR      ORDCTS
068600961227     C*
068700961227     C* STAMPA I CODICI TASSAZIONE MEMORIZZATI
068800050103DO  1C     1             DO        600           I
068900970418IF  2C     OCTS(I)       IFNE      *BLANKS
069000970508     C     OPPE(I)       ANDGT     *ZEROS
069100961227     C                   EXSR      STMETT                                       *STAMPA IL RECORD
069200961227E   2C                   ENDIF
069300961227E   1C                   ENDDO
069400961227     C*
069500961227     C                   ENDSR
069600961227     C*------------------------------------------------------------------------*
069700961227     C* STMINT - STAMPA INTESTAZIONE DATI TIPO -CODICI TASSAZIONE-
069800961227     C*------------------------------------------------------------------------*
069900961227     C     STMINT        BEGSR
070000961227     C*
070100961227IF  1C     *IN01         IFEQ      *ON                                          *FINE PAGINA
070200961227     C                   WRITE     TE41TE                                       *TESTATA
070300961227     C                   WRITE     TE41IN                                       *INTESTAZIONE
070400961227     C                   SETOFF                                       01
070500961227E   1C                   ENDIF
070600961227     C*
070700040805IF  1C     D00FIE        IFEQ      'I'
070800961227     C                   MOVEL     'ITALIA'      I2DFIE
070900961227X   1C                   ELSE
071000961227     C                   MOVEL     'ESTERO'      I2DFIE
071100961227E   1C                   ENDIF
071200961227     C                   WRITE     TE41I2                                       *ALTRA INTESTAZIONE
071300961227     C                   WRITE     TE41IT                                       *DESCRIZIONE CAMPI
071400961227     C*
071500961227     C                   ENDSR
071600961227     C*------------------------------------------------------------------------*
071700961227     C* ORDCTS - ORDINA I CODICI TASSAZIONE MEMORIZZATI
071800961227     C*------------------------------------------------------------------------*
071900961227     C     ORDCTS        BEGSR
072000961227     C*
072100050103DO  1C     1             DO        600           I
072200970418IF  2C     OCTS(I)       IFGT      *BLANKS                                      *ELEMENTO PIENO
072300961227     C     1             ADD       I             J
072400050103DO  3C     J             DO        600           J
072500970418IF  4C     OCTS(J)       IFGT      *BLANKS                                      *ELEMENTO PIENO
072600970418IF  5C     OCOR(I)       IFGT      OCOR(J)
072700970418     C                   MOVEL     OCTS(I)       PCTS                           *CODICE
072800970418     C                   MOVEL     OCTS(J)       OCTS(I)
072900970418     C                   MOVEL     PCTS          OCTS(J)
073000970418     C                   MOVEL     ORCT(I)       PRCT                           *RAGGRUPPAMENTO
073100970418     C                   MOVEL     ORCT(J)       ORCT(I)
073200970418     C                   MOVEL     PRCT          ORCT(J)
073300970418     C                   MOVEL     OPPE(I)       PPPE                           *% INCIDENZA
073400970418     C                   MOVEL     OPPE(J)       OPPE(I)
073500970418     C                   MOVEL     PPPE          OPPE(J)
073600040806     C                   MOVEL     Oftr(I)       Pftr                           *% INCIDENZA
073700040806     C                   MOVEL     Oftr(J)       Oftr(I)
073800040806     C                   MOVEL     Pftr          Oftr(J)
073900970418     C                   MOVEL     OTCT(I)       PTCT              1            *TIPO
074000970418     C                   MOVEL     OTCT(J)       OTCT(I)
074100970418     C                   MOVEL     PTCT          OTCT(J)
074200970418     C                   MOVEL     OCOR(I)       PCOR                           *ORDINAMENTO
074300970418     C                   MOVEL     OCOR(J)       OCOR(I)
074400970418     C                   MOVEL     PCOR          OCOR(J)
074500961227E   5C                   ENDIF
074600961227E   4C                   ENDIF
074700961227E   3C                   ENDDO                                                  *FINE CICLO UNO
074800961227E   2C                   ENDIF
074900961227E   1C                   ENDDO                                                  *FINE CICLO DUE
075000961227     C*
075100961227     C                   ENDSR
075200961227     C*------------------------------------------------------------------------*
075300961227     C* STMETT - STAMPA I DATI TIPO -CODICI TASSAZIONE-
075400961227     C*------------------------------------------------------------------------*
075500961227     C     STMETT        BEGSR
075600961227     C*
075700961227IF  1C     *IN01         IFEQ      *ON                                          *FINE PAGINA
075800970418     C                   WRITE     TE41TE                                       *TESTATA
075900961227     C                   WRITE     TE41IN                                       *INTESTAZIONE
076000961227     C                   WRITE     TE41I2                                       *ALTRA INTESTAZIONE
076100961227     C                   WRITE     TE41IT                                       *DESCRIZIONE CAMPI
076200961227     C                   SETOFF                                       01
076300961227E   1C                   ENDIF
076400961227     C* CAMPI
076500970418     C                   MOVEL     OCTS(I)       DTCCTS
076600970418     C                   Z-ADD     OPPE(I)       DTCPPE
076700040805     C* HO UTILIZZO IL CAMPO FTR X MEMORIZZARE SE IL VALORE È STATO DETTO
076800040805     C* OPPURE SE HO AVUTO DELLE SPEDIZIONI. AD OGNI AGGIORNAMENTO
076900040805     C* DEL RECORD VERRÀ PULITO E QUINDI PERDO IL CONTENUTO ORIGINALE
077000040805     C* MA È GIUSTO COSÌ
077100040805     C                   MOVEL     *BLANKS       DTDDED
077200040805     C                   IF        OFTR(I) = '*'
077300040805     C                   EVAL      DTDDED ='NESSUNA SPEDIZIONE: VALORE DEDOTTO'
077400040805     C                   END
077500970418     C* DECODIFICHE
077600970421     C                   MOVEL     *BLANKS       DTDCTS
077700970421     C                   MOVEL     *BLANKS       DTCRCT
077800970421     C                   MOVEL     *BLANKS       DTDRCT
077900970421     C                   MOVEL     *BLANKS       DTCFCR
078000970421     C                   SETOFF                                           98
078100970421     C                   Z-ADD     1             J
078200970421     C     DTCCTS        LOOKUP    CTS(J)                                 98
078300970421IF  1C     *IN98         IFEQ      *ON
078400970421     C                   MOVEL     DCTS(J)       DTDCTS                         *DESCR.COD.TASSAZION
078500970421     C                   MOVEL     RAP(J)        LRAP                           *REGIONE
078600970421     C                   MOVEL     NAR(J)        LNAR                           *NAZIONE
078700970421     C*
078800970421IF  2C     D00SCT        IFEQ      'T'
078900970421     C                   MOVEL     ORCT(I)       DTCRCT                         *RAGGRUPPAMENTO
079000961227     C                   MOVEL     FCR(J)        DTCFCR                         *FLAG CAPOLUOGO
079100970421IF  3C     DTCRCT        IFEQ      *BLANKS                                      *NON CALCOLARE
079200961227     C                   MOVEL     DDEC(1)       DTDRCT                         *DESCR.RAGGRUPPAMENT
079300970421E   3C                   ENDIF
079400970421IF  3C     DTCRCT        IFEQ      'T'                                          *CALCOLARE
079500961227     C                   MOVEL     DDEC(2)       DTDRCT                         *DESCR.RAGGRUPPAMENT
079600970421E   3C                   ENDIF
079700970421IF  3C     DTCRCT        IFEQ      'R'                                          *CALCOLARE IN REGION
079800961227     C                   SETOFF                                           98
079900961227     C                   Z-ADD     1             J
080000961227     C     LRAP          LOOKUP    CTS(J)                                 98
080100970421IF  4C     *IN98         IFEQ      *ON
080200961227     C                   MOVEL     DDEC(3)       DDEC03
080300961227     C                   MOVEL     DCTS(J)       D03RCT
080400961227     C                   MOVEL     DDEC03        DTDRCT                         *DESCR.RAGGRUPPAMENT
080500970421E   4C                   ENDIF
080600970421E   3C                   ENDIF
080700970421IF  3C     DTCRCT        IFEQ      'N'                                          *CALCOLARE IN NAZION
080800961227     C                   SETOFF                                           98
080900961227     C                   Z-ADD     1             J
081000961227     C     LNAR          LOOKUP    CTS(J)                                 98
081100970421IF  4C     *IN98         IFEQ      *ON
081200961227     C                   MOVEL     DDEC(3)       DDEC03
081300961227     C                   MOVEL     DCTS(J)       D03RCT
081400961227     C                   MOVEL     DDEC03        DTDRCT                         *DESCR.RAGGRUPPAMENT
081500970421E   4C                   ENDIF
081600970421E   3C                   ENDIF
081700970421E   2C                   ENDIF
081800970421E   1C                   ENDIF
081900961227     C*
082000961227     C                   WRITE     TE41DT                                       *CAMPI
082100961227     C*
082200961227     C                   ENDSR
082300961227     C*------------------------------------------------------------------------*
082400961227     C* STMPAR - STAMPA I PARAMETRI DI LANCIO
082500961227     C*------------------------------------------------------------------------*
082600961227     C     STMPAR        BEGSR
082601091125     c                   if        d00dsf = ' '
082602091201     c                   eval      padctod= 'TARIFFA O OFFERTA ...........:'
082603091201     c                   eval      padkovd= 'CLIENTE O VISITA ............:'
082604091201     c                   eval      padkscd= 'CLIENTE O VISITA ............:'
082605091201     c                   eval      padprgd= 'PROGRESSIVO .................:'
082606091125     c                   else
082607091201     c                   eval      padctod= 'TARIFFA O OFFERTA ...........:'
082608091201     c                   eval      padkovd= 'CLIENTE O TRATTATIVA ........:'
082609100420     c                   eval      padkscd= 'CLIENTE O TRATTATIVA ........:'
082610091201     c                   eval      padprgd= 'VERSIONE ....................:'
082611091125     c                   end
082700040804     C                   SELECT
082800040804     C* STAMPA DATI TIPO
082900060828     C                   WHEN      KCDAZ='TE3N'
083000040804     C                   MOVEL     'TNTE43R'     TECPGM
083100040804     C                   MOVEL     DDEC(20)      TEDLAN
083200040804     C*
083400040804     C                   Z-ADD     D00KSC        PACKSC
083500040804     C                   Z-ADD     D00CTR        PACCTR
083600040804     C                   Z-ADD     D00PRG        PACPRG
083700040804     C                   Z-ADD     D00LNP        PACLNP
083800040804     C                   MOVEL     D00SIE        PACSIE
083900040804     C                   MOVEL     D00SCT        PACSCT
084000040804     C                   MOVEL     D00DIV        PACDIV
084100040804     C* DECODIFICHE
084200040804     C                   MOVEL     ACORAG        PADKSC
084201091201IF  1C     D00CTO        IFEQ      'T'
084202091201     C                   MOVEL     'TARIFFA'     PADCTO
084203091201X   1C                   ELSE
084204091201     C                   MOVEL     'OFFERTA'     PADCTO
084206091201E   1C                   ENDIF
084800040804     C*
084900040804     C                   Z-ADD     1             I
085000040804     C                   SETOFF                                           98
085100040804     C                   MOVEL     D00CTR        WA1               1
085200040804     C     WA1           LOOKUP    TTR(I)                                 98
085300040804IF  1C     *IN98         IFEQ      *ON
085400040804     C                   MOVEL     DTTR(I)       PADCTR
085500040804X   1C                   ELSE
085600040804     C                   MOVEL     *ALL'*'       PADCTR
085700040804E   1C                   ENDIF
085800040804     C*
085900040804IF  1C     D00LNP        IFNE      999
086000040804     C                   Z-ADD     1             I
086100040804     C                   SETOFF                                           98
086200040804     C     D00LNP        LOOKUP    ORG(I)                                 98
086300040804IF  2C     *IN98         IFEQ      *ON
086400040804     C                   MOVEL     DORG(I)       PADLNP
086500040804X   2C                   ELSE
086600040804     C                   MOVEL     *ALL'*'       PADLNP
086700040804E   2C                   ENDIF
086800040804X   1C                   ELSE
086900040804     C                   MOVEL(P)  'TUTTE'       PADLNP
087000040804E   1C                   ENDIF
087100040804     C*
087200040804IF  1C     D00SIE        IFEQ      'I'
087300040804     C                   MOVEL     'ITALIA'      PADSIE
087400040804X   1C                   ELSE
087500040804     C                   MOVEL     'ESTERO'      PADSIE
087600040804E   1C                   ENDIF
088300040804     C*
088400040804IF  1C     D00SCT        IFEQ      ' '
088500040804     C                   MOVEL     DDEC(6)       PADSCT
088600040804E   1C                   ENDIF
088700040804IF  1C     D00SCT        IFEQ      'R'
088800040804     C                   MOVEL     DDEC(7)       PADSCT
088900040804E   1C                   ENDIF
089000040804IF  1C     D00SCT        IFEQ      'T'
089100040804     C                   MOVEL     DDEC(8)       PADSCT
089200040804E   1C                   ENDIF
089300040804     C*
089400040804     C* STAMPA LA PAGINA DEI PARAMETRI SE LANCIATA DA MENU'
089500040804IF  1C     D00OP0        IFEQ      *BLANKS                                      *LANCIO DA FILTRO
089600040804     C                   WRITE     TE41TE                                       *TESTATA
089700040804     C                   WRITE     TE41P43                                      *PARAMETRI
089800040804E   1C                   ENDIF
089900040804     C* CREAZIONE DATI TIPO
090000060828     C                   WHEN      KCDAZ='TE6N'
090100040804     C                   MOVEL     'TNTE41R'     TECPGM
090200040804     C                   MOVEL     DDEC(5)       TEDLAN
090300040804     C*
090400040804     C                   Z-ADD     PARKSC        PACKSC
090500040804     C                   Z-ADD     PARFPA        PACFPA
090600040804     C                   Z-ADD     PARFAR        PACFAR
090700040804     C                   Z-ADD     PARFAP        PACFAP
090800040804IF  1C     PARDS1        IFGT      *ZEROS
090900040804     C                   Z-ADD     PARDS1        G08INV
091000040804     C                   Z-ADD     *ZEROS        G08DAT
091100040804     C                   MOVEL     '3'           G08ERR
091200040804     C                   CALL      'XSRDA8'
091300040804     C                   PARM                    WLBDA8
091400040804     C                   Z-ADD     G08DAT        PACDS1
091500040804E   1C                   ENDIF
091600040804IF  1C     PARDS2        IFGT      *ZEROS
091700040804     C                   Z-ADD     PARDS2        G08INV
091800040804     C                   Z-ADD     *ZEROS        G08DAT
091900040804     C                   MOVEL     '3'           G08ERR
092000040804     C                   CALL      'XSRDA8'
092100040804     C                   PARM                    WLBDA8
092200040804     C                   Z-ADD     G08DAT        PACDS2
092300040804E   1C                   ENDIF
092400040804     C                   MOVEL     PARTAR        PACTAR
092500040804     C                   Z-ADD     PARKOV        PACKOV
092600040804     C                   Z-ADD     PARCTR        PACCTR
092700040804     C                   Z-ADD     PARPRG        PACPRG
092800040804     C                   Z-ADD     PARLNP        PACLNP
092900040804     C                   MOVEL     PARFIE        PACFIE
093000040804     C                   MOVEL     PARELA        PACELA
093100040804     C                   MOVEL     PARDIV        PACDIV
093200040804IF  1C     PARSIM        IFEQ      'S'
093300040804     C                   MOVEL     'SI'          PADSIM
093400040804X   1C                   ELSE
093500040804     C                   MOVEL     'NO'          PADSIM
093600040804E   1C                   ENDIF
093601091201     C* DECODIFICHE
093602091201IF  1C     PARCTO        IFEQ      'T'
093603091201     C                   MOVEL     'TARIFFA'     PADCTO
093604091201X   1C                   ELSE
093605091201     C                   MOVEL     'OFFERTA'     PADCTO
093607091201E   1C                   ENDIF
094300040804     C*
094400040804     C     PARUNI        IFEQ      'S'
094500040804     C                   EVAL      PADUNI = '(UNIFICANTE)'
094600040804     C                   ENDIF
094700040804     C*
094800040804IF  1C     PARCTO        IFEQ      'T'
094900040804     C                   MOVEL     ACORAG        PADKOV
095000040804E   1C                   ENDIF
095100040804     C*
095200040804IF  1C     PARFPA        IFGT      *ZEROS
095300040804     C                   Z-ADD     1             I                 4 0
095400040804     C                   SETOFF                                           99
095500040804     C     PARFPA        LOOKUP    ORG(I)                                 99
095600040804IF  2C     *IN99         IFEQ      *ON
095700040804     C                   MOVEL     DORG(I)       PADFPA
095800040804X   2C                   ELSE
095900040804     C                   MOVEL     *ALL'*'       PADFPA
096000040804E   2C                   ENDIF
096100040804E   1C                   ENDIF
096200040804     C*
096300040804IF  1C     PARFAR        IFGT      *ZEROS
096400040804     C                   Z-ADD     1             I                 4 0
096500040804     C                   SETOFF                                           99
096600040804     C     PARFAR        LOOKUP    ORG(I)                                 99
096700040804IF  2C     *IN99         IFEQ      *ON
096800040804     C                   MOVEL     DORG(I)       PADFAR
096900040804X   2C                   ELSE
097000040804     C                   MOVEL     *ALL'*'       PADFAR
097100040804E   2C                   ENDIF
097200040804E   1C                   ENDIF
097300040804     C*
097400040804IF  1C     PARFAP        IFGT      *ZEROS
097500040804     C                   Z-ADD     1             I                 4 0
097600040804     C                   SETOFF                                           99
097700040804     C     PARFAP        LOOKUP    ORG(I)                                 99
097800040804IF  2C     *IN99         IFEQ      *ON
097900040804     C                   MOVEL     DORG(I)       PADFAP
098000040804X   2C                   ELSE
098100040804     C                   MOVEL     *ALL'*'       PADFAP
098200040804E   2C                   ENDIF
098300040804E   1C                   ENDIF
098400040804     C*
098500040804     C                   Z-ADD     1             I                 4 0
098600040804     C                   SETOFF                                           99
098700040804     C     PARTTR        LOOKUP    TTR(I)                                 99
098800040804IF  1C     *IN99         IFEQ      *ON
098900040804     C                   MOVEL     DTTR(I)       PADCTR
099000040804X   1C                   ELSE
099100040804     C                   MOVEL     *ALL'*'       PADCTR
099200040804E   1C                   ENDIF
099300040804     C*
099400040804IF  1C     PARLNP        IFNE      999
099500040804     C                   Z-ADD     1             I                 4 0
099600040804     C                   SETOFF                                           99
099700040804     C     PARLNP        LOOKUP    ORG(I)                                 99
099800040804IF  2C     *IN99         IFEQ      *ON
099900040804     C                   MOVEL     DORG(I)       PADLNP
100000040804X   2C                   ELSE
100100040804     C                   MOVEL     *ALL'*'       PADLNP
100200040804E   2C                   ENDIF
100300040804X   1C                   ELSE
100400040804     C                   MOVEL     'TUTTE'       PADLNP
100500040804E   1C                   ENDIF
100600040804     C*
100700040804IF  1C     PARFIE        IFNE      *BLANKS
100800040804IF  2C     PARFIE        IFEQ      'I'
100900040804     C                   MOVEL     'ITALIA'      PADFIE
101000040804X   2C                   ELSE
101100040804     C                   MOVEL     'ESTERO'      PADFIE
101200040804E   2C                   ENDIF
101300040804X   1C                   ELSE
101400040804     C                   MOVEL     'ENTRAMBE'    PADFIE
101500040804E   1C                   ENDIF
101600040805     c                   if        *in88
101700040804     C                   WRITE     TE41TE                                       *TESTATA
101800040804     C                   WRITE     TE41P41                                      *PARAMETRI
101900040805     c                   end
102000040804     C                   ENDSL
102100961227     C*
102200961227     C                   ENDSR
102300961227     C*------------------------------------------------------------------------*
102400961227     C* CARTAB - CARICAMENTO TABELLE
102500961227     C*------------------------------------------------------------------------*
102600961227     C     CARTAB        BEGSR
102700961227     C*--------------------
102800961227     C* ORGANIGRAMMA
102900961227     C*--------------------
103000961227     C                   Z-ADD     *ZEROS        I                 4 0
103100961227     C     *LOVAL        SETLL     AZORG01L
103200961227     C                   READ      AZORG01L                               99
103300961227DO  1C     *IN99         DOWEQ     *OFF
103400961227IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATE
103500961227     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
103600961227     C                   ADD       1             I
103700961227     C                   Z-ADD     ORGFIL        ORG(I)                         *FILIALE
103800961227     C                   MOVEL     ORGDES        DORG(I)                        *DESCRIZIONE
103900020730     C                   MOVEL     ORGDE3        OG143
104000020730     C*
104100020730     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
104200020730     C                   CLEAR                   DNTW
104300020730     C*
104400020730     C                   MOVEL(P)  'NTW'         KTECOD
104500020730     C                   MOVEL(P)  §OGNTW        KTEKE1
104600020730     C     KEYTBE        CHAIN     TNTBE01L
104700020730     C                   IF        %found(TNTBE01L)
104800020730     C                   MOVEL     TBEUNI        DNTW
104900020730     C                   ENDIF
105000020730     C*
105100020730     C                   MOVEL     §NTWFIE       FL1(I)                         *FLAG ITALIA/ESTERO
105200970512     C* SELEZIONI
105300970512IF  3C     D00LNP        IFEQ      999                                          *TUTTE
105400970512     C                   Z-ADD     ORGFIL        SORG(I)                        *FILIALI SELEZIONATE
105500981014X   3C                   ELSE
105600981014IF  4C     D00LNP        IFEQ      888                                          *TUTTE
105700981014     C                   Z-ADD     888           SORG(1)                        *FILIALI SELEZIONATE
105800981014     C                   LEAVE
105900981014X   4C                   ELSE
106000981014IF  5C     D00LNP        IFEQ      889                                          *TUTTE
106100981014     C                   Z-ADD     889           SORG(1)                        *FILIALI SELEZIONATE
106200981014     C                   LEAVE
106300981014X   5C                   ELSE
106400981014IF  6C     D00LNP        IFEQ      ORGFIL                                       *UNA FILIALE
106500970512     C                   Z-ADD     ORGFIL        SORG(1)                        *FILIALE SELEZIONATA
106600981014     C                   LEAVE
106700981014E   6C                   ENDIF
106800981014E   5C                   ENDIF
106900981014E   4C                   ENDIF
107000970512E   3C                   ENDIF
107100961227E   2C                   ENDIF
107200961227     C                   READ      AZORG01L                               99
107300961227E   1C                   ENDDO
107400961227     C*--------------------
107500961227     C* TIPI TARIFFA
107600961227     C*--------------------
107700961227     C                   Z-ADD     *ZEROS        I                 4 0
107800020906     C                   Z-ADD     1             KTBKUT
107900961227     C                   MOVEL     'TR'          KTBCOD
108000961227     C     KE2TAB        SETLL     TABEL00F
108100961227     C     KE2TAB        READE     TABEL00F                               99
108200961227DO  1C     *IN99         DOWEQ     *OFF
108300961227IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
108400961227     C                   ADD       1             I
108500961227     C                   MOVEL     TBLUNI        DSTR
108600961227     C                   MOVEL     TBLKEY        TTR(I)                         *CODICE
108700961227     C                   MOVEL     §TRDES        DTTR(I)                        *DESCRIZIONE
108800961227E   2C                   ENDIF
108900961227     C     KE2TAB        READE     TABEL00F                               99
109000961227E   1C                   ENDDO
109100090611     C*--------------------
109200090611     C* TIPo servizio 'E'
109300090611     C*--------------------
109400090611     C                   Z-ADD     *ZEROS        I                 4 0
109500090611     C                   Z-ADD     1             KTBKUT
109600090611     C                   MOVEL     '5E'          KTBCOD
109700090611     C                   MOVEL(p)  'E'           KTBkey
109800090611     C     KE3TAB        chain     TABEL00F
109900090611DO  1C                   if        %found(tabel00f)
110000090611     C                   MOVEL     TBLUNI        DS5e
110100090611     C                   eval      costes = '%'+ §5ed05 + '/ING'
110200090611E   2C                   endif
110300961227     C*--------------------
110400961227     C* CODICI TASSAZIONE
110500961227     C*--------------------
110600961227     C                   Z-ADD     *ZEROS        I                 4 0
110700020906     C                   Z-ADD     1             KTBKUT
110800961227     C                   MOVEL     'CT'          KTBCOD
110900961227     C     KE2TAB        SETLL     TABEL00F
111000961227     C     KE2TAB        READE     TABEL00F                               99
111100961227DO  1C     *IN99         DOWEQ     *OFF
111200961227IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NON ANNULLATI
111300970312     C                   MOVEL     TBLUNI        DSCT
111400970312IF  3C     §CTUTC        IFEQ      'S'                                          *DA CONSIDERARE
111500961227     C                   ADD       1             I
111600961227     C                   MOVEL     TBLKEY        CTS(I)                         *CODICE
111700961227     C                   MOVEL     §CTDES        DCTS(I)                        *DESCRIZIONE
111800961227     C                   MOVEL     §CTFCR        FCR(I)                         *FLAG CAPOLUOGO
111900961227     C                   MOVEL     §CTCOR        COR(I)                         *ORDINAMENTO
112000961227     C                   MOVEL     §CTRAP        RAP(I)                         *REGIONE
112100970418     C                   MOVE      §CTCOR        WA1                            *TIPO CODICE
112200970512IF  4C     WA1           IFEQ      '9'
112300970512IF  5C     CTS(I)        IFEQ      'IT'
112400970418     C     CTS(I)        OREQ      'EE'
112500970418     C                   MOVEL     'N'           TCT(I)                         *TIPO "NAZIONE"
112600970512X   5C                   ELSE
112700970418     C                   MOVEL     'R'           TCT(I)                         *TIPO "REGIONE"
112800970512E   5C                   ENDIF
112900970512X   4C                   ELSE
113000970418     C                   MOVEL     'E'           TCT(I)                         *TIPO "ELEMENTARE"
113100970512E   4C                   ENDIF
113200970512IF  4C     §CTNAR        IFEQ      *BLANKS
113300961227     C                   MOVEL     'IT'          NAR(I)                         *NAZIONE ITALIA
113400970512X   4C                   ELSE
113500961227     C                   MOVEL     'EE'          NAR(I)                         *NAZIONE ESTERO
113600970512E   4C                   ENDIF
113700970312E   3C                   ENDIF
113800970312E   2C                   ENDIF
113900961227     C     KE2TAB        READE     TABEL00F                               99
114000961227E   1C                   ENDDO
114100961227     C*--------------------
114200961227     C* LEGGE LA TARIFFA / OFFERTA
114300961227     C*--------------------
114400961227     C                   Z-ADD     D00KSC        KTMKSC
114500961227     C                   Z-ADD     D00CTR        KTMCTR
114600961227     C                   Z-ADD     D00PRG        KTMPRG
114700970418IF  1C     D00CTO        IFEQ      'T'                                          *TARIFFA
114800961227     C     KEYTAM        CHAIN     TNTAM01L                           99
114900970418X   1C                   ELSE                                                   *OFFERTA
115000961227     C     KEYTAM        CHAIN     TNOFM01L                           99
115100970418E   1C                   ENDIF
115200970418     C*--------------------
115300970418     C* LEGGE IL CLIENTE
115400970418     C*--------------------
115500020906     C                   Z-ADD     1             KACKUT                         *ANAGRAFICA
115600970512     C                   Z-ADD     KCCCL         KACKCC
115700970512     C                   Z-ADD     D00KSC        KACKSC
115800970512IF  1C     D00CTO        IFEQ      'T'                                          *TARIFFA
115900970512     C     KEYACO        CHAIN     CNACO00F                           99
116000970512X   1C                   ELSE                                                   *OFFERTA
116100970512     C     KEYACO        CHAIN     TFACO00F                           99
116200970512IF  2C     *IN99         IFEQ      *ON                                          *NON TROVATO
116501091124     c                   clear                   tnte04ds
116502091124     C                   movel     D00dsf        d04dsf
116503091124     C                   Z-ADD     D00KSC        d04nrv
116505091124     c                   call      'TNTE04R'
116506091124     c                   parm                    kpjba
116507091126     c                   parm                    tnte04ds
116509091124     C                   Z-ADD     d04KSC        KacKSC                         *CLIENTE DELLA VISIT
116600970512     C     KEYACO        CHAIN     CNACO00F                           99
116700970512E   2C                   ENDIF
116800970512E   1C                   ENDIF
116900961227     C*
117000961227     C                   ENDSR
117100961227     C*------------------------------------------------------------------------*
117200961227     C* CARCC  - CARICA I CAPICONTO DI CONTABILITA'
117300961227     C*------------------------------------------------------------------------*
117400961227     C     CARCC         BEGSR
117500961227     C*
117600961227     C* RECUPERA I DATI DELL'UTENTE
117700020906     C                   Z-ADD     1             CODUT
117800020830     C                   CALL      'X§PARUT'
117900961227     C                   PARM                    PARUT
118000961227     C*---
118100961227     C* A) --> CARICAMENTO  C.CN  CENTRO DI COSTO
118200961227     C* B) --> CARICAMENTO  C.CN  I.V.A.
118300961227     C* C) --> CARICAMENTO  C/CN FINANZIARIA
118400961227     C*---
118500961227     C                   Z-ADD     1             Z
118600961227DO  1C                   DO        50            X2                2 0
118700961227     C                   MOVE      TCU(X2)       CARCNT
118800961227IF  2C     TIPCNT        IFEQ      'CI'
118900961227     C                   MOVE      KCU(X2)       KCCCI             4 0           *CONTAB. INDUSTRIAL
119000961227E   2C                   ENDIF
119100961227IF  2C     FL34CN        IFEQ      '01'
119200961227     C     TIPCNT        ANDEQ     'CG'
119300961227     C                   Z-ADD     KCU(X2)       KCCCL             4 0
119400961227E   2C                   ENDIF                                                   *CLIENTI ITALIA
119500961227IF  2C     FL34CN        IFEQ      '02'
119600961227     C     TIPCNT        ANDEQ     'CG'
119700961227     C                   Z-ADD     KCU(X2)       KCCFO             4 0
119800961227E   2C                   ENDIF                                                   *FORNITORI ITALIA
119900961227IF  2C     TIPCNT        IFEQ      'RI'
120000961227     C                   Z-ADD     KCU(X2)       RI(Z)
120100961227     C                   ADD       1             Z                 3 0           * ?!?
120200961227E   2C                   ENDIF
120300961227E   1C                   ENDDO
120400040804     c                   movel     ragut         dsfirs
120500961227     C*
120600961227     C                   ENDSR
120700961227     C*------------------------------------------------------------------------*
120800961227     C* *INZSR - OPERAZIONI INIZIALI
120900961227     C*------------------------------------------------------------------------*
121000961227     C     *INZSR        BEGSR
121100961227     C*--------------------
121200961227     C* RICEVIMENTO PARAMETRI
121300961227     C*--------------------
121400961227     C     *ENTRY        PLIST
121500961227     C                   PARM                    KPJBA
121600040804     C                   PARM                    PARAM
121700041202     c* apertura controllata di alcuni file xchè solo di filiale
121800041202     c                   open      tfaco00f                             95
121900041202     c                   open      tiofd01l                             95
122100041202     c                   open      tnofm01l                             95
122200040805     c* 88 mi serve per stampare i parametri di lancio
122300040805     c* se mi ha chiamato la stampa avrò 1 parm e quindi li devo stamapare
122400040805     c* se mi ha chiamato la creaz. dati tipo avrò 2 parm e quindi li
122500040805     c* devo stampare solo la prima volta
122600040804     C                   IF        %PARMS = 1
122700040805     C                   seton                                        88
122800040805     c                   else
122801091130     c                   movel     pardsf        d00dsf
122900040805     c                   if        parint = ' '
123000040805     C                   seton                                        88
123100040805     c                   else
123200040805     C                   setoff                                       88
123300040805     c                   end
123400040804     C                   END
123500961227     C                   MOVEL     KPJBU         DSTE00
123600961227     C*--------------------
123700961227     C* VARIABILI RIFERITE AL DATABASE
123800961227     C*--------------------
123900961227     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
124000970418     C     *LIKE         DEFINE    TBLCOD        KTBCOD
124100961227     C     *LIKE         DEFINE    TBLKEY        KTBKEY
124200970418     C     *LIKE         DEFINE    ACOKUT        KACKUT                         *CNACO00F
124300970418     C     *LIKE         DEFINE    ACOKCC        KACKCC
124400970418     C     *LIKE         DEFINE    ACOKSC        KACKSC
124600961227     C     *LIKE         DEFINE    TAMKSC        KTMKSC                         *TNTAM00F
124700961227     C     *LIKE         DEFINE    TAMCTR        KTMCTR
124800961227     C     *LIKE         DEFINE    TAMPRG        KTMPRG
124900990820     C     *LIKE         DEFINE    TADKSC        KTDKSC                         *TITAD00F
125000970418     C     *LIKE         DEFINE    TADCTR        KTDCTR
125100970418     C     *LIKE         DEFINE    TADPRG        KTDPRG
125200970418     C     *LIKE         DEFINE    TADLNP        KTDLNP
125300011002     C     *LIKE         DEFINE    ETCDSF        KTCDSF                         *TEETC00F
125400980603     C     *LIKE         DEFINE    ETCCTO        KTCCTO
125500970117     C     *LIKE         DEFINE    ETCKSC        KTCKSC
125600961227     C     *LIKE         DEFINE    ETCCTR        KTCCTR
125700961227     C     *LIKE         DEFINE    ETCPRG        KTCPRG
125800961227     C     *LIKE         DEFINE    ETCLNP        KTCLNP
125900011002     C     *LIKE         DEFINE    ETSDSF        KTSDSF                         *TEETS00F
126000980603     C     *LIKE         DEFINE    ETSCTO        KTSCTO
126100970117     C     *LIKE         DEFINE    ETSKSC        KTSKSC
126200961227     C     *LIKE         DEFINE    ETSCTR        KTSCTR
126300961227     C     *LIKE         DEFINE    ETSPRG        KTSPRG
126400961227     C     *LIKE         DEFINE    ETSLNP        KTSLNP
126500011002     C     *LIKE         DEFINE    ETTDSF        KTTDSF                         *TEETT00F
126600980603     C     *LIKE         DEFINE    ETTCTO        KTTCTO
126700970117     C     *LIKE         DEFINE    ETTKSC        KTTKSC
126800961227     C     *LIKE         DEFINE    ETTCTR        KTTCTR
126900961227     C     *LIKE         DEFINE    ETTPRG        KTTPRG
127000961227     C     *LIKE         DEFINE    ETTLNP        KTTLNP
127100961227     C     *LIKE         DEFINE    ETTFIE        KTTFIE
127200970512     C     *LIKE         DEFINE    ETCLNP        DEPLNP                         *DI DEPOSITO
127300020730     C     *LIKE         DEFINE    §NTWFIE       DLPFL1
127400961227     C     *LIKE         DEFINE    ETTCTS        PCTS                           *DI PARCHEGGIO
127500961227     C     *LIKE         DEFINE    ETTRCT        PRCT
127600961227     C     *LIKE         DEFINE    ETTPPE        PPPE
127700040806     C     *LIKE         DEFINE    ETTftr        Pftr
127800961227     C     *LIKE         DEFINE    §CTCOR        PCOR
127900961227     C     *LIKE         DEFINE    ETTCTS        LRAP                           *DI LAVORO
128000961227     C     *LIKE         DEFINE    ETTCTS        LNAR
128100020730     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
128200020730     C     *LIKE         DEFINE    TBEKE1        KTEKE1
128300961227     C*--------------------
128400961227     C* CHAIVI DI LETTURA
128500961227     C*--------------------
128600961227     C* LETTURA TABEL00F - PARZIALE
128700961227     C     KE2TAB        KLIST
128800961227     C                   KFLD                    KTBKUT                         *CODICE UTENTE
128900961227     C                   KFLD                    KTBCOD                         *CODICE TABELLA
129000090611     C     KE3TAB        KLIST
129100090611     C                   KFLD                    KTBKUT                         *CODICE UTENTE
129200090611     C                   KFLD                    KTBCOD                         *CODICE TABELLA
129300090611     C                   KFLD                    KTBkey                         *CODICE TABELLA
129400970418     C* LETTURA CNACO00F - COMPETA
129500970418     C     KEYACO        KLIST
129600970418     C                   KFLD                    KACKUT                         *CODICE UTENTE
129700970418     C                   KFLD                    KACKCC                         *CODICE CAPOCONTO
129800970418     C                   KFLD                    KACKSC                         *CODICE CLIENTE
130200961227     C* LETTURA TNTAM01L - COMPLETA
130300961227     C     KEYTAM        KLIST
130400961227     C                   KFLD                    KTMKSC                         *CLIENTE
130500961227     C                   KFLD                    KTMCTR                         *CODICE TARIFFA
130600961227     C                   KFLD                    KTMPRG                         *PROGRESSIVO
130700990820     C* LETTURA TITAD04L - PARZIALE
130800970418     C     KEYTAD        KLIST
130900970418     C                   KFLD                    KTDKSC                         *CLIENTE
131000970418     C                   KFLD                    KTDCTR                         *TARIFFA
131100970418     C                   KFLD                    KTDPRG                         *PROGRESSIVO
131200970418     C                   KFLD                    KTDLNP                         *LINEA PARTENZA
131300011002     C* LETTURA TEETC01L - COMPLETA
131400961227     C     KEYETC        KLIST
131500980603     C                   KFLD                    KTCDSF                         *DATI SEDE O FILIALE
131600980603     C                   KFLD                    KTCCTO                         *TARIFFA O OFFERTA
131700970117     C                   KFLD                    KTCKSC                         *CLIENTE O VISITA
131800961227     C                   KFLD                    KTCCTR                         *TARIFFA
131900961227     C                   KFLD                    KTCPRG                         *PROGRESSIVO
132000961227     C                   KFLD                    KTCLNP                         *LINEA PARTENZA
132100011002     C* LETTURA TEETC01L - PARZIALE
132200961227     C     KE2ETC        KLIST
132300980603     C                   KFLD                    KTCDSF                         *DATI SEDE O FILIALE
132400980603     C                   KFLD                    KTCCTO                         *TARIFFA O OFFERTA
132500970117     C                   KFLD                    KTCKSC                         *CLIENTE O VISITA
132600961227     C                   KFLD                    KTCCTR                         *TARIFFA
132700961227     C                   KFLD                    KTCPRG                         *PROGRESSIVO
132800011002     C* LETTURA TEETS01L - PARZIALE
132900961227     C     KEYETS        KLIST
133000980603     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
133100980603     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
133200970117     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
133300961227     C                   KFLD                    KTSCTR                         *TARIFFA
133400961227     C                   KFLD                    KTSPRG                         *PROGRESSIVO
133500961227     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
133600011002     C* LETTURA TEETT01L - PARZIALE
133700961227     C     KEYETT        KLIST
133800980603     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
133900970117     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
134000970117     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
134100961227     C                   KFLD                    KTTCTR                         *TARIFFA
134200961227     C                   KFLD                    KTTPRG                         *PROGRESSIVO
134300961227     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
134400961227     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
134500020730     C* LETTURA TNTBE01L - PARZIALE
134600020730     C     KEYTBE        KLIST
134700020730     C                   KFLD                    KTECOD                         *UTENTE
134800020730     C                   KFLD                    KTEKE1                         *CODICE
134900961227     C*--------------------
135000961227     C* VARIABILI DI CONTROLLO
135100961227     C*--------------------
135200961227     C                   MOVEL     '0'           $FINE             1            *FINE PGM
135300961227     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
135400961227     C                   MOVEL     '1'           *IN01                          *OVERFLOW TNTE41P
135500961227     C*
135600961227     C* REPERISCE LA DATA CORRENTE
135700961227     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
135800961227     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
135900961227     C                   Z-ADD     WN8           G08DAT
136000961227     C                   Z-ADD     *ZEROS        G08INV
136100961227     C                   MOVEL     '0'           G08ERR
136200961227     C                   CALL      'XSRDA8'
136300961227     C                   PARM                    WLBDA8
136400961227     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
136500961227     C*
136600961227     C* CARICA CAPOCONTI DI CONTABILITA'
136700961227     C                   EXSR      CARCC
136800961227     C*
136900961227     C* CARICA TABELLE OCCORRENTI
137000961227     C                   EXSR      CARTAB
137100961227     C*
137200961227     C                   ENDSR
137300961227     O*------------------------------------------------------------------------*
137400961227** -DDEC-
137500961227NON CALCOLARE                 1
137600961227CALCOLARE                     2
137700961227CALCOLARE IN XXXXXXXXXXXXXXXXX3
137800040804CALCOLARE IN NAZIONE          4
137900040804 **  CREAZIONE DATI  TIPO  ** 5
138000970417COME DA TARIFFA               6
138100970417PER REGIONE                   7
138200970417IN DETTAGLIO                  8
138300040806TUTTA L'ITALIA                9
138400040806TUTTO L'ESTERO               10
13850096122711
13860096122712
13870097041713
13880097041714
13890096122715
13900096122716
13910096122717
13920096122718
13930096122719
139400961227***   STAMPA  DATI  TIPO   ***20
