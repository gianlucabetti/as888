000100970117     H*------------------------------------------------------------------------*
000200970117     H* TARIFFE DA CONTO ECONOMICO - RICHIESTA DATI TIPO GENERALI              *
000300970117     H*------------------------------------------------------------------------*
000400970117     H DATEDIT(*YMD)
000500970117     F*------------------------------------------------------------------------*
000600970117     F* DATA BASE
000700970117     F*------------------------------------------------------------------------*
000800970117     FTNTE01D   CF   E             WORKSTN
000900970117     F*------------
001000970117     F* TABELLE
001100970117     F*------------
001200970117     FTABEL00F  IF   E           K DISK
001300011011     FTNTBE01L  IF   E           K DISK
001400060830     FTeets11L  IF   E           K DISK
001500970117     F*------------
001600970117     F* ORGANIGRAMMA
001700970117     F*------------
001800970117     FAZORG01L  IF   E           K DISK
001900970117     F*------------
002000970117     F* PIANO DEI CONTI
002100970117     F*------------
002200970117     FCNACO00F  IF   E           K DISK
002300970414     FCNCLP00F  IF   E           K DISK
002400011030     FCNIND00F  IF   E           K DISK
002500041202     FTFACO00F  IF   E           K DISK    usropn
002600970602     F                                     RENAME(CNACO000:TFACO000)
002700041202     FTFCLP00F  IF   E           K DISK    usropn
002800970414     F                                     RENAME(CNCLP000:TFCLP000)
002900041202     FTFIND00F  IF   E           K DISK    usropn
003000011030     F                                     RENAME(CNIND000:TFIND000)
003100970117     F*------------
003200970117     F* TARIFFE CLIENTE
003300970117     F*------------
003400970117     FTNTAM01L  IF   E           K DISK
003500041202     FTNOFM01L  IF   E           K DISK    usropn
003600970117     F                                     RENAME(TNTAM000:TNOFM000)
003700990818     FTITAD04L  IF   E           K DISK
003800011001     FTEETC01L  UF A E           K DISK
003900970117     D*------------------------------------------------------------------------*
004000970117     D* SCHIERE
004100970117     D*------------------------------------------------------------------------*
004200970117     D*-------------------
004300970117     D* DECODIFICHE VARIE
004400970117     D*-------------------
004500980818     D DDEC            S             20    DIM(10) CTDATA PERRCD(1)
004600970117     D*-------------------
004700970117     D* DESCRIZIONE VIDEO IN GESTIONE
004800970117     D*-------------------
004900970117     D DVID            S             20    DIM(5) CTDATA PERRCD(1)
005000970117     D*-------------------
005100970117     D* DESCRIZIONE OPERAZIONE
005200970117     D*-------------------
005300970117     D DOPE            S             15    DIM(5) CTDATA PERRCD(1)
005400970117     D*-------------------
005500970117     D* MESSAGGI VIDEO
005600970117     D*-------------------
005700151119     D MSG             S             78    DIM(86) CTDATA PERRCD(1)
005800970117     D*-------------------
005900970117     D* TIPO SERVIZIO
006000970117     D*-------------------
006100970117     D TSP             S              1    DIM(10)                              *CODICE
006200970117     D DTSP            S             20    DIM(10)                              *DESCRIZIONE
006300970117     D*-------------------
006400970117     D* TIPI TARIFFA
006500970117     D*-------------------
006600970117     D TTR             S              1    DIM(10)                              *CODICE
006700970117     D DTTR            S             20    DIM(10)                              *DESCRIZIONE
006800970117     D*-------------------
006900970117     D* DI LAVORO
007000970117     D*-------------------
007100970117     D TSGL            S             13  3 DIM(10)                              *SCAGLIONI IN TARIFF
007200970117     D*------------------
007300970117     D* TABELLE CAPICONTO
007400970117     D*------------------
007500970117     D RI              S              4  0 DIM(4)                               *TAB RAG. CI
007600970117     D*------------------------------------------------------------------------*
007700970117     D* INPUT
007800970117     D*------------------------------------------------------------------------*
007900150227     D*------------------
008000150227     D* DS RIDEFINIZIONE TABELLA "TBA"
008100150227     D*------------------
008200150227     D DTBA          E DS
008300150227     d comtba          s                   like(§etctba)
008400150227     D*-------------------
008500150227     D Detcflo       E DS
008600970117     D*-------------------
008700970117     D* DS TIPI SERVIZIO
008800970117     D*-------------------
008900970117     D DS5E          E DS
009000970117     D*-------------------
009100970117     D* DS TIPI TARIFFA
009200970117     D*-------------------
009300970117     D DSTR          E DS
009400970117     D*-------------------
009500020923     D* DS REPERIMENTO DATI UTENTE
009600970117     D*-------------------
009700091103     D Tnte04DS      E DS                                                       *Profili utente
009800091103     D TIBS34DS      E DS                                                       *Profili utente
009900020923     D DDATIUTE      E DS                                                       *Dati utente
010000020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
010100970117     D*-------------------
010200970117     D* DS PASSAGGIO PARAMETRI
010300970117     D*-------------------
010400980430     D DSTE00        E DS                  EXTNAME(TNTE00DS)
010500980508     D*------------------
010600980430     D* DS RICERCA DATI TIPO
010700980508     D*------------------
010800980430     D DSTE30        E DS                  EXTNAME(TNTE30DS)
010900021202     D*------------------
011000021202     D* DS REPERIMENTO DIVISA TARIFFE
011100021202     D*------------------
011200021202     D DSTA01        E DS
011300021202     D*-------------------
011400021202     D* DS *PGM REPIMEMTO TARIFFE DI CARTELLO
011500021202     D*-------------------
011600160229     D TRULC7DS      E DS
011700970117     D*------------------
011800970117     D* DS "XSRDA8" - CONTROLLA DATA (8)
011900970117     D*------------------
012000970117     D WLBDA8          DS
012100970117     D  G08DAT                 1      8  0
012200970117     D  G08INV                 9     16  0
012300970117     D  G08ERR                17     17
012400970117     D  G08TGI                18     22  0
012500970117     D*-------------------
012600970117     D* ARCHITETTURA
012700970117     D*-------------------
012800970117     D KPJBA         E DS
012900970117     D*------------------
013000970117     D* DS SCOMPOSIZIONE MESSAGGIO DI ERRORE
013100970117     D*------------------
013200970117     D* MSG,62
013300970117     D                 DS
013400970117     D  ERRLNP                30     32
013500970117     D  ERRCTS                34     35
013600970117     D  ERRM62                 1     78
013700011011     D*------------------
013800011011     D* DS LETTURA TABELLA "GED" TIPO RECORD "CE"
013900011011     D*------------------
014000011011     D DSGEDCE       E DS
014100011008     D*------------------
014200011008     D* VARIABILI DI WRK
014300011008     D*------------------
014400011008     D  NUMDEC         S              9  9 INZ(*zeros)
014500040824     D  com100         S                   like(v4csban)
014600050225     D  ok             S              1    inz(*off)
014700090611     D  costs          S             25
014800011030     D*-------------------
014900011030     D* DS TABELLA "CT"
015000011030     D*-------------------
015100011030     D DSCT          E DS
015200020729     D*-------------------
015300020729     D* DS RIDEFINIZIONE DESCRIZIONE 143 ORGANIGRAMMA
015400020729     D*-------------------
015500020729     D OG143         E DS
015600020729     D*------------------
015700020729     D* DS PER LETTURA TABELLA "NTW"
015800020729     D*------------------
015900020729     D DNTW          E DS
016000970117     C*------------------------------------------------------------------------*
016100970117     C* MAIN LINE
016200970117     C*------------------------------------------------------------------------*
016300970117     C*
016400970117     C* OPERAZIONI INIZIALI
016500970117     C                   EXSR      RUTINZ
016600970117     C*
016700970117     C* ERRORI IN ENTRATA, ESCE DAL PGM
016800970117IF  1C     $FINE         IFNE      'S'
016900970117     C*
017000970117     C* IMPOSTAZIONI PRIMA VIDEATA
017100970117     C                   EXSR      IMPVD1
017200970117     C*
017300970117     C* CICLO FINO A RICHIESTA DI FINE ELABORAZIONE
017400970117DO  2C     $FINE         DOUEQ     'S'
017500970117     C     TIPVID        CASEQ     '1'           GESVD1                         *1° VIDEO
017600970117     C     TIPVID        CASEQ     '2'           GESVD2                         *2° VIDEO
017700970117     C     TIPVID        CASEQ     '3'           GESVD3                         *3° VIDEO
017800970117     C     TIPVID        CASEQ     '4'           GESVD4                         *4° VIDEO
017900970117     C     TIPVID        CASEQ     '5'           GESVD5                         *5° VIDEO
018000970117     C                   ENDCS
018100970117E   2C                   ENDDO
018200970117E   1C                   ENDIF
018300970117     C*
018400970117     C                   SETON                                        LR
018500970117     C*------------------------------------------------------------------------*
018600970117     C* IMPVD1 - IMPOSTAZIONI PRIMA VIDEATA
018700970117     C*------------------------------------------------------------------------*
018800970117     C     IMPVD1        BEGSR
018900970117     C*
019000050225     C                   eval      ok = *off                                     MESSAGGIO
019100970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI
019200970117     C                   EXSR      SETVD1
019300970117     C                   MOVEL     '1'           TIPVID                         *GESTIONE VIDEO
019400970117     C*
019500970117     C                   ENDSR
019600970117     C*------------------------------------------------------------------------*
019700970117     C* SETVD1 - IMPOSTAZIONI INDICATORI PRIMA VIDEATA
019800970117     C*------------------------------------------------------------------------*
019900970117     C     SETVD1        BEGSR
020000060825     c*
020100980429     C                   SETOFF                                       181920     ERRORI
020200980429     C                   SETOFF                                       21
020300970117     C                   SETOFF                                       90         GENERICO DI ERRORE
020400970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
020500970117     C*
020600970117     C                   ENDSR
020700970117     C*------------------------------------------------------------------------*
020800970117     C* GESVD1 - GESTIONE PRIMA VIDEATA
020900970117     C*------------------------------------------------------------------------*
021000970117     C     GESVD1        BEGSR
021100970117     C*
021200970117     C* EMISSIONE VIDEATA
021300970117     C                   WRITE     TE01D00                                      *TESTATA
021400970117     C                   EXFMT     TE01D01
021500970117     C                   EXSR      SETVD1
021600970117     C* F03-FINE
021700970117IF  1C     *INKC         IFEQ      *ON
021800970117     C                   EXSR      F03GES
021900970117     C                   GOTO      FINVD1
022000970117E   1C                   ENDIF
022100970117     C* F12-RITORNO
022200970117IF  1C     *INKL         IFEQ      *ON
022300970117     C                   EXSR      F12GES
022400970117     C                   GOTO      FINVD1
022500970117E   1C                   ENDIF
022600970117     C* CONTROLLA VIDEO
022700970117     C                   EXSR      CTRVD1
022800970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
022900970117     C                   GOTO      FINVD1
023000970117E   1C                   ENDIF
023100970117     C*
023200041105     C* LEGGE IL RECORD DA MANUTENZIONARE
023300041105     C                   EXSR      LETREC
023400970128     C*
023500970128     C* SE DATI TIPO INESISTENTI, PROPONE FINESTRA F05=CARICA DA ALTRA TARIFFA
023600091120     C                   MOVEL     'S'           KTCDSF                         *DATI SEDE/FILIALE
023700970128     C                   MOVEL     D00CTO        KTCCTO                         *TARIFFA O OFFERTA
023800970128     C                   Z-ADD     D00KSC        KTCKSC                         *CLIENTE P VISITA
023900970128     C                   Z-ADD     D00CTR        KTCCTR                         *TARIFFA
024000970128     C                   Z-ADD     D00PRG        KTCPRG                         *PROGRESSIVO
024100970128     C                   Z-ADD     V1CLNP        KTCLNP                         *LINEA DI PARTENZA
024200011001     C     KEYETC        SETLL     TEETC01L                               99
024300970128IF  1C     *IN99         IFEQ      *OFF
024400970128     C                   WRITE     TE01D00                                      *TESTATA
024500970128     C                   WRITE     TE01D06                                      *MESSAGGIO SF VUOTO
024600970128     C                   EXSR      F05GES
024700970128E   1C                   ENDIF
024800970117     C*
024900970117     C* EMETTE VIDEATA SUCCESSIVA
025000970117     C                   EXSR      IMPVD2
025100970117     C*
025200970117     C     FINVD1        ENDSR
025300970117     C*------------------------------------------------------------------------*
025400970117     C* CTRVD1 - CONTROLLA PRIMA VIDEATA
025500970117     C*------------------------------------------------------------------------*
025600970117     C     CTRVD1        BEGSR
025700970117     C*--------------------
025800970117     C* TARIFFA ITALIA/ESTERO
025900970117     C*--------------------
026000970117IF  1C     V1CFIE        IFNE      'I'
026100970117     C     V1CFIE        ANDNE     'E'
026200970117     C                   SETON                                        1990
026300970117     C                   MOVEL     MSG(1)        $MSG
026400970117     C                   GOTO      FINCT1
026500970117X   1C                   ELSE
026600970117     C                   MOVEL     V1CFIE        V0CFIE
026700970117IF  2C     V1CFIE        IFEQ      'I'
026800970117     C                   MOVEL     DDEC(1)       V1DFIE
026900970117     C                   MOVEL     DDEC(1)       V0DFIE
027000970117E   2C                   ENDIF
027100970117IF  2C     V1CFIE        IFEQ      'E'
027200970117     C                   MOVEL     DDEC(2)       V1DFIE
027300970117     C                   MOVEL     DDEC(2)       V0DFIE
027400970117E   2C                   ENDIF
027500970117E   1C                   ENDIF
027600970117     C*--------------------
027700970117     C* LINEA DI PARTENZA
027800970117     C*--------------------
027900970117     C                   MOVEL     V1CLNP        WLNP
028000970117     C                   MOVEL     *BLANKS       WDLNP
028100971204     C                   MOVEL     *BLANKS       WFL1
028200970117     C                   MOVEL     '0'           WERR
028300970117     C                   EXSR      CHKLNP
028400970117     C                   MOVEL     WDLNP         V1DLNP
028500980818IF  1C     WERR          IFEQ      '1'
028600970117     C                   SETON                                        2090
028700970117     C                   MOVEL     MSG(3)        $MSG
028800970117     C                   GOTO      FINCT1
028900970117E   1C                   ENDIF
029000971204IF  1C     WFL1          IFNE      *BLANKS                                      *ESTERO
029100970211     C                   SETON                                        2090
029200970211     C                   MOVEL     MSG(15)       $MSG
029300970211     C                   GOTO      FINCT1
029400970211E   1C                   ENDIF
029500970117IF  1C     V1CFIE        IFEQ      'I'
029600971204     C     WFL1          ANDNE     *BLANKS                                      *ESTERO
029700970117     C                   SETON                                        2090
029800970117     C                   MOVEL     MSG(16)       $MSG
029900970117     C                   GOTO      FINCT1
030000970117E   1C                   ENDIF
030100970314     C*--------------------
030200970314     C* DATA RIFERIMENTO CALCOLO
030300970314     C*--------------------
030400970317     C     *LIKE         DEFINE    V1CDTC        DTCV1C
030500970314     C                   Z-ADD     V1CDTC        G08DAT
030600970314     C                   Z-ADD     *ZEROS        G08INV
030700970314     C                   MOVEL     '0'           G08ERR
030800970314     C                   CALL      'XSRDA8'
030900970314     C                   PARM                    WLBDA8
031000970314     C                   Z-ADD     G08DAT        V1CDTC
031100970318     C                   Z-ADD     G08INV        DTCV1C
031200970314IF  1C     G08ERR        IFEQ      '1'
031300970314     C                   SETON                                        2190
031400970314     C                   MOVEL     MSG(47)       $MSG
031500970314     C                   GOTO      FINCT1
031600970314E   1C                   ENDIF
031700970117     C*--------------------
031800970117     C* DATI TIPO INESISTENTI
031900970117     C*--------------------
032000970117IF  1C     $TGES         IFEQ      'I'                                          *INTERROGAZIONE
032100091120     C                   MOVEL     'S'           KTCDSF                         *DATI SEDE/FILIALE
032200970117     C                   MOVEL     D00CTO        KTCCTO                         *TARIFFA O OFFERTA
032300980429     C                   Z-ADD     D00KSC        KTCKSC                         *CLIENTE O VISITA
032400970117     C                   Z-ADD     D00CTR        KTCCTR                         *TARIFFA
032500970117     C                   Z-ADD     D00PRG        KTCPRG                         *PROGRESSIVO
032600970117     C                   Z-ADD     V1CLNP        KTCLNP                         *LINEA DI PARTENZA
032700011001     C     KEYETC        SETLL     TEETC01L                               99
032800970117IF  2C     *IN99         IFEQ      *OFF                                         *NON TROVATO
032900970117     C                   SETON                                        2090
033000970117     C                   MOVEL     MSG(2)        $MSG
033100970117     C                   GOTO      FINCT1
033200970117E   2C                   ENDIF
033300970117E   1C                   ENDIF
033400970117     C*--------------------
033500970117     C* SCAGLIONI IN TARIFFA
033600970117     C*--------------------
033700970117     C* CARICA I PRIMI SCAGLIONI DEL DETTAGLIO TARIFFA DI UN CODICE TASSAZIONE
033800970117IF  1C     D00TLA        IFEQ      'D'                                          *DA CALCOLO DELTA
033900970117     C     *LIKE         DEFINE    TADORP        WORP
034000970117     C                   MOVEL     '0'           $TAD                           *ESISTENZA DETTAGLIO
034100970117     C                   CLEAR                   TSGL
034200970117     C                   Z-ADD     *ZEROS        I
034300970117     C                   Z-ADD     V0CKSC        KTDKSC
034400970117     C                   Z-ADD     V0CCTR        KTDCTR
034500970117     C                   Z-ADD     V0CPRG        KTDPRG
034600970117     C                   Z-ADD     V1CLNP        KTDLNP
034700990818     C     KEYTAD        SETLL     TITAD04L
034800990818     C     KEYTAD        READE     TITAD04L                               99
034900970117DO  2C     *IN99         DOWEQ     *OFF
035000970117     C                   MOVEL     TADORP        WORP
035100970117DO  3C     *IN99         DOWEQ     *OFF
035200970117     C     TADORP        ANDEQ     WORP
035300970117IF  4C     TADATB        IFEQ      *BLANKS                                      *NO ANNULLATI
035400970117     C                   MOVEL     '1'           $TAD                           *DETTAGLIO ESISTENTE
035500970117     C                   ADD       1             I
035600970117IF  5C     I             IFLE      10
035700970117     C                   Z-ADD     TADSGL        TSGL(I)
035800970117E   5C                   ENDIF
035900970117E   4C                   ENDIF
036000990818     C     KEYTAD        READE     TITAD04L                               99
036100970117E   3C                   ENDDO                                                  *FINE COD.TASSAZIONE
036200970117     C                   LEAVE
036300970117E   2C                   ENDDO                                                  *FINE DETTAGLIO
036400970117     C*---
036500970117     C* DETTAGLIO PER LA LINEA DI PARTENZA INESISTENTE, ERRORE E USCITA
036600970117     C*---
036700970117IF  2C     $TAD          IFEQ      '0'
036800970117     C                   SETON                                        2090
036900970117     C                   MOVEL     MSG(64)       $MSG
037000970117     C                   GOTO      FINCT1
037100970117E   2C                   ENDIF
037200970117     C* CONTROLLA SCAGLIONI
037300970117     C                   Z-ADD     V0CKSC        KTDKSC
037400970117     C                   Z-ADD     V0CCTR        KTDCTR
037500970117     C                   Z-ADD     V0CPRG        KTDPRG
037600970117     C                   Z-ADD     V1CLNP        KTDLNP
037700990818     C     KEYTAD        SETLL     TITAD04L
037800990818     C     KEYTAD        READE     TITAD04L                               99
037900970117DO  2C     *IN99         DOWEQ     *OFF
038000970117     C                   MOVEL     TADORP        WORP
038100970117     C                   Z-ADD     *ZEROS        I
038200970117DO  3C     *IN99         DOWEQ     *OFF
038300970117     C     TADORP        ANDEQ     WORP
038400970117IF  4C     TADATB        IFEQ      *BLANKS                                      *NO ANNULLATI
038500080616     C*---
038600080616     C* SCAGLIONI CON C.A.P., ERRORE E USCITA
038700080616     C*---
038800080616IF  5C     TADCAP        IFNE      *BLANKS
038900080616     C                   SETON                                        90
039000080616     C                   MOVEL     MSG(63)       D00MSG
039100080616     C                   MOVEL     '1'           D00ERR
039200080616     C                   MOVEL     'S'           $FINE                          *USCITA PGM
039300080616     C                   GOTO      FINCT1
039400080616E   5C                   ENDIF
039500080616     C*---
039600970117     C                   ADD       1             I
039700970117     C*---
039800970117     C* SCAGLIONI MAGGIORI DI 10, ERRORE E USCITA
039900970117     C*---
040000970117IF  5C     I             IFGT      10
040100970117     C                   SETON                                        90
040200970117     C                   MOVEL     MSG(61)       D00MSG
040300970117     C                   MOVEL     '1'           D00ERR
040400970117     C                   MOVEL     'S'           $FINE                          *USCITA PGM
040500970117     C                   GOTO      FINCT1
040600970117E   5C                   ENDIF
040700970117     C*---
040800970117     C* SCAGLIONI DIVERSI PER CODICE TASSAZIONE, ERRORE E USCITA
040900970117     C*---
041000970117IF  5C     TADSGL        IFNE      TSGL(I)
041100970117     C                   SETON                                        90
041200970117     C                   MOVEL     MSG(62)       ERRM62
041300970117     C                   MOVEL     TADLNP        ERRLNP
041400970117     C                   MOVEL     TADCTS        ERRCTS
041500970117     C                   MOVEL     ERRM62        D00MSG
041600970117     C                   MOVEL     '1'           D00ERR
041700970117     C                   MOVEL     'S'           $FINE                          *USCITA PGM
041800970117     C                   GOTO      FINCT1
041900970117E   5C                   ENDIF
042000970117E   4C                   ENDIF
042100990818     C     KEYTAD        READE     TITAD04L                               99
042200970117E   3C                   ENDDO                                                  *FINE COD.TASSAZIONE
042300970117E   2C                   ENDDO                                                  *FINE DETTAGLIO
042400970117E   1C                   ENDIF
042500970117     C*
042600970117     C     FINCT1        ENDSR
042700970117     C*------------------------------------------------------------------------*
042800970117     C* LETREC - LEGGE IL RECORD DA MANUTENZIONARE
042900970117     C*------------------------------------------------------------------------*
043000970117     C     LETREC        BEGSR
043100970117     C*
043200970117     C                   MOVEL     '0'           $REC
043300041105     c                   clear                   dataf             8 0
043400970117     C*
043500091120     C                   MOVEL     'S'           KTCDSF                         *DATI SEDE/FILIALE
043600970117     C                   MOVEL     D00CTO        KTCCTO
043700970117     C                   Z-ADD     V0CKSC        KTCKSC
043800970117     C                   Z-ADD     V0CCTR        KTCCTR
043900970117     C                   Z-ADD     V0CPRG        KTCPRG
044000970117     C                   Z-ADD     V1CLNP        KTCLNP
044100011001     C     KEYETC        CHAIN(N)  TEETC01L                           99
044200970117IF  1C     *IN99         IFEQ      *ON
044300970117     C                   MOVEL     'I'           $REC                           *DA INSERIRE
044400970117     C                   MOVEL     DOPE(1)       V0COPE                         *INSERIMENTO
044500970117X   1C                   ELSE
044600970117     C                   MOVEL     'M'           $REC                           *DA MODIFICARE
044700970117     C                   MOVEL     DOPE(2)       V0COPE                         *MANUTENZIONE
044800970117E   1C                   ENDIF
044900970117IF  1C     $TGES         IFEQ      'I'                                          *INTERROGAZIONE
045000970117     C                   MOVEL     *BLANKS       V0COPE                         *INSERIMENTO
045100970117E   1C                   ENDIF
045200051019     C* 89 indicatore a video per gestire "#" accanto alla domanda
045300050808     C* ingloba al porto. Se vengono attivate maggioraz. su voci con
045400050808     c* raggruppamento 001 la domanda vale solo per i ricavi, mentre
045500050808     c* per i costi no. Quindi attenzione se si aggiungono domande o
045600050808     c* si modificano voci già attive controllare i raggruppamenti!!!
045700050808     c     v1cfie        comp      'I'                                    89
045800050808     C* RIEMPE O AZZERA I CAMPI A VIDEO (DI TUTTI I FORMATI)
045900970117IF  1C     $REC          IFEQ      'I'
046000970117     C                   EXSR      AZZREC                                       *AZZERA
046100970117E   1C                   ENDIF
046200970117IF  1C     $REC          IFEQ      'M'
046300970117     C                   EXSR      IMPREC                                       *IMPOSTA
046400970117E   1C                   ENDIF
046500970117     C*
046600970117     C                   ENDSR
046700041105     C*------------------------------------------------------------------------*
046800041105     C* LETRECS- LEGGE IL RECORD analogo di SEDE
046900041105     C*------------------------------------------------------------------------*
047000041105     C     LETRECs       BEGSR
047100041105     C*
047200041105     c                   clear                   datas             8 0
047300041108     c                   clear                   comctr            1
047400041105     C                   MOVEL     'S'           KTCDSF                         *DATI SEDE/FILIALE
047500041105     C                   MOVEL     D00CTO        KTCCTO
047600041105     C                   Z-ADD     V0CKSC        KTCKSC
047700041105     C                   Z-ADD     V0CCTR        KTCCTR
047800041105     C                   Z-ADD     V0CPRG        KTCPRG
047900041105     C                   Z-ADD     V1CLNP        KTCLNP
048000041105     C     KEYETC        CHAIN(N)  TEETC01L
048100041105     c                   if        %found(teetc01l)
048200041105     c                   if        etcduv > etcdir
048300041105     c                   eval      datas = etcduv
048400041105     c                   else
048500041105     c                   eval      datas = etcdir
048600041105     c                   end
048700041105     c                   end
048800041105     C*
048900041105     C                   ENDSR
049000970117     C*------------------------------------------------------------------------*
049100970117     C* AZZREC - AZZERA I CAMPI A VIDEO -DI TUTTI I FORMATI-
049200970117     C*------------------------------------------------------------------------*
049300970117     C     AZZREC        BEGSR
049400970117     C*
049500980504     C                   MOVEL     *BLANKS       V2CDCV
049600980504     C                   MOVEL     *BLANKS       V2CFAR
049700970317     C                   Z-ADD     *ZEROS        V2CIAR
049800970117     C                   MOVEL     *BLANKS       V2CCLV
049900970117     C                   MOVEL     *BLANKS       V2CBOL
050000021031     C                   MOVEL     *BLANKS       V2CDKC
050100021031     C                   MOVEL     *BLANKS       V2CIX1
050200021031     C                   MOVEL     *BLANKS       V2CIX2
050300970117     C                   Z-ADD     *ZEROS        V2CGGP
050400970117     C                   Z-ADD     *ZEROS        V2CSPR
050500970117     C                   Z-ADD     *ZEROS        V2CPMC
050600970117     C                   Z-ADD     *ZEROS        V2CVMC
050700970117     C                   Z-ADD     *ZEROS        V2CFRA
050800970117     C                   Z-ADD     *ZEROS        V2CASS
050900970117     C                   Z-ADD     *ZEROS        V2CPCV
051000970117     C                   Z-ADD     *ZEROS        V2CPCM
051100970117     C                   Z-ADD     *ZEROS        V2CPFD
051200040730     C                   Z-ADD     *ZEROS        V2Cval
051300151119     C                   movel     ' '           v2crtm
051400151119     C                   movel     'N'           v2cirp
051500970117     C                   Z-ADD     *ZEROS        V3CPPV                         *TERZA VIDEATA
051600970117     C                   MOVEL     'N'           V3CSIN
051700970117     C                   Z-ADD     *ZEROS        V3CESP
051800970117     C                   MOVEL     'N'           V3CIES
051900970117     C                   Z-ADD     *ZEROS        V3CPGI
052000970424     C                   Z-ADD     *ZEROS        V3CGGG
052100970117     C                   MOVEL     'N'           V3CIGI
052200150227     C                   Z-ADD     *zeros        V4Ctba                         *QUARTA VIDEATA
052300150227     c                   clear                   v4dtba
052400150227     C                   Z-ADD     *ZEROS        V4CPAB                         *QUARTA VIDEATA
052500970117     C                   Z-ADD     *ZEROS        V4CPAM
052600970117     C                   MOVEL     'N'           V4CICA
052700970117     C                   Z-ADD     *ZEROS        V4CCAS
052800011009     C                   MOVEL     D00DIV        V4CDIC
052900970117     C                   Z-ADD     *ZEROS        V4CPAR
053000970117     C                   Z-ADD     *ZEROS        V4CPAC
053100060314     C                   Z-ADD     *ZEROS        V4Cacb
053200970117     C                   MOVEL     'N'           V4CIAS
053300970117     C                   Z-ADD     *ZEROS        V4CASC
053400011009     C                   MOVEL     D00DIV        V4CDIA
053500040811     C                   Z-ADD     *ZEROS        V4Csbnb
053600040824     C                   Z-ADD     *ZEROS        V4Csban
053700050802     C                   MOVEL     'N'           V4CIba
053800040811     C                   Z-ADD     *ZEROS        V4Cnbn
053900040802     C                   Z-ADD     *ZEROS        V4Cnba
054000151119     C                   movel     ' '           v5cpkl
054100151119     C                   movel     'N'           v5cipk
054200970117     C                   Z-ADD     *ZEROS        V5CPCA                         *QUINTA VIDEATA
054300970117     C                   MOVEL     'N'           V5CIAP
054400970117     C                   Z-ADD     *ZEROS        V5CPCP
054500970117     C                   MOVEL     'N'           V5CIPI
054600970117     C                   Z-ADD     *ZEROS        V5CPCS
054700970117     C                   MOVEL     'N'           V5CISU
054800970117     C                   Z-ADD     *ZEROS        V5CPCF
054900970117     C                   MOVEL     'N'           V5CIFU
055000970414     C                   Z-ADD     *ZEROS        V5CPCD
055100970414     C                   MOVEL     'N'           V5CIDD
055200970414     C                   Z-ADD     *ZEROS        V5CPCC
055300970414     C                   MOVEL     'N'           V5CICE
055400970414     C                   Z-ADD     *ZEROS        V5CPCL
055500970414     C                   MOVEL     'N'           V5CILO
055600011009     C                   Z-ADD     *ZEROS        V5CCIS
055700011009     C                   MOVEL     'N'           V5CISO
055800011009     C                   Z-ADD     *ZEROS        V5CSAM
055900011009     C                   MOVEL     'N'           V5CINR
056000040802     C                   Z-ADD     *ZEROS        V5Cdes
056100050808     C                   MOVEL     'N'           V5CIDE
056200970117     C*
056300970117     C                   ENDSR
056400970117     C*------------------------------------------------------------------------*
056500970117     C* IMPREC - IMPOSTA I CAMPI A VIDEO -DI TUTTI I FORMATI-
056600970117     C*------------------------------------------------------------------------*
056700970117     C     IMPREC        BEGSR
056800970117     C*
056900150227     C                   MOVEL     ETCflo        detcflo
057000150227     C                   MOVEL     ETCDCV        V2CDCV
057100980504     C                   MOVEL     ETCFAR        V2CFAR
057200970314     C                   Z-ADD     ETCIAR        V2CIAR
057300970117     C                   MOVEL     ETCCLV        V2CCLV
057400970117     C                   MOVEL     ETCBOL        V2CBOL
057500021031     C                   MOVEL     ETCDKC        V2CDKC
057600021031     C                   MOVEL     ETCIX1        V2CIX1
057700021031     C                   MOVEL     ETCIX2        V2CIX2
057800970117     C                   Z-ADD     ETCGGP        V2CGGP
057900970117     C                   Z-ADD     ETCSPR        V2CSPR
058000970117     C                   Z-ADD     ETCPMC        V2CPMC
058100970117     C                   Z-ADD     ETCVMC        V2CVMC
058200970117     C                   Z-ADD     ETCFRA        V2CFRA
058300970117     C                   Z-ADD     ETCASS        V2CASS
058400970117     C                   Z-ADD     ETCPCV        V2CPCV
058500970117     C                   Z-ADD     ETCPCM        V2CPCM
058600970117     C                   Z-ADD     ETCPFD        V2CPFD
058700040730     C                   Z-ADD     ETCval        V2Cval
058800151119     C                   movel     §ETCrtm       v2crtm
058900151119     C                   movel     §ETCirp       v2cirp
059000970117     C                   Z-ADD     ETCPPV        V3CPPV                         *TERZA VIDEATA
059100970117     C                   MOVEL     ETCSIN        V3CSIN
059200970117     C                   Z-ADD     ETCESP        V3CESP
059300970117     C                   MOVEL     ETCIES        V3CIES
059400970117     C                   Z-ADD     ETCPGI        V3CPGI
059500970117     C                   Z-ADD     ETCGGG        V3CGGG
059600021031     C                   Z-ADD     ETCNRG        V3CNRG
059700970117     C                   MOVEL     ETCIGI        V3CIGI
059800150227     c                   if        §etctba <> ' '
059900150227     c                   MOVE      §ETCtba       V4ctba
060000150302     c                   MOVE      §ETCtba       comtba
060100150305     c                   exsr      cnttba
060200150227     c                   else
060300150227     c                   clear                   v4ctba
060400150227     c                   clear                   comtba
060500150227     c                   end
060600970117     C                   Z-ADD     ETCPAB        V4CPAB                         *QUARTA VIDEATA
060700970117     C                   Z-ADD     ETCPAM        V4CPAM
060800970117     C                   MOVEL     ETCICA        V4CICA
060900970117     C                   Z-ADD     ETCCAS        V4CCAS
061000011009     C                   MOVEL     D00DIV        V4CDIC
061100060313     C                   Z-ADD     ETCxx1        V4Cacb
061200060313     C                   Z-ADD     ETCPAR        V4CPAR
061300970117     C                   Z-ADD     ETCPAC        V4CPAC
061400970117     C                   MOVEL     ETCIAS        V4CIAS
061500970117     C                   Z-ADD     ETCASC        V4CASC
061600011009     C                   MOVEL     D00DIV        V4CDIA
061700040811     C                   Z-ADD     ETCbnb        V4Csbnb
061800040824     C                   Z-ADD     ETCban        V4Csban
061900050802     C                   MOVEL     ETCIba        V4CIba
062000040811     C                   Z-ADD     ETCnbn        V4Cnbn
062100040802     C                   Z-ADD     ETCnba        V4Cnba
062200151119     C                   movel     ETCPkl        v5cpkl
062300151119     C                   movel     ETCiva        v5cipk
062400970117     C                   Z-ADD     ETCPCA        V5CPCA                         *QUINTA VIDEATA
062500970117     C                   MOVEL     ETCIAP        V5CIAP
062600970117     C                   Z-ADD     ETCPCP        V5CPCP
062700970117     C                   MOVEL     ETCIPI        V5CIPI
062800970117     C                   Z-ADD     ETCPCS        V5CPCS
062900970117     C                   MOVEL     ETCISU        V5CISU
063000970117     C                   Z-ADD     ETCPCF        V5CPCF
063100970117     C                   MOVEL     ETCIFU        V5CIFU
063200970414     C                   Z-ADD     ETCPCD        V5CPCD
063300970414     C                   MOVEL     ETCIDD        V5CIDD
063400970414     C                   Z-ADD     ETCPCC        V5CPCC
063500970414     C                   MOVEL     ETCICE        V5CICE
063600970414     C                   Z-ADD     ETCPCL        V5CPCL
063700970414     C                   MOVEL     ETCILO        V5CILO
063800011009     C                   Z-ADD     ETCCIS        V5CCIS
063900011009     C                   MOVEL     ETCISO        V5CISO
064000011009     C                   Z-ADD     ETCSAM        V5CSAM
064100011009     C                   MOVEL     ETCINR        V5CINR
064200040802     C                   Z-ADD     ETCdes        V5Cdes
064300050808     C                   MOVEL     ETCIDE        V5CIDE
064400041105     c                   if        etcduv > etcdir
064500041105     c                   eval      dataf = etcduv
064600041105     c                   else
064700041105     c                   eval      dataf = etcdir
064800041105     c                   end
064900970117     C*
065000970117     C                   ENDSR
065100970117     C*------------------------------------------------------------------------*
065200970117     C* IMPVD2 - IMPOSTAZIONE SECONDA VIDEATA
065300970117     C*------------------------------------------------------------------------*
065400970117     C     IMPVD2        BEGSR
065500970117     C*
065600970117     C* REIMPOSTA INDICATORI
065700970117     C                   EXSR      SETVD2
065800970117     C                   MOVEL     '2'           TIPVID
065900970117     C*
066000970117     C                   Z-ADD     V1CLNP        V2CLNP
066100970117     C                   MOVEL     V1DLNP        V2DLNP
066200970314     C                   Z-ADD     V1CDTC        V2CDTC
066300970117     C*
066400970117     C                   WRITE     TE01D00                                      *TESTATA
066500970117     C*
066600970117     C                   ENDSR
066700970117     C*------------------------------------------------------------------------*
066800970117     C* SETVD2 - IMPOSTAZIONI INDICATORI SECONDA VIDEATA
066900970117     C*------------------------------------------------------------------------*
067000970117     C     SETVD2        BEGSR
067100970117     C*
067200970117     C                   SETOFF                                       222324     ERRORI
067300970117     C                   SETOFF                                       252627
067400970117     C                   SETOFF                                       282930
067500970117     C                   SETOFF                                       313233
067600980504     C                   SETOFF                                       343566
067700021104     C                   SETOFF                                       677581
067800041108     C                   SETOFF                                       828397
067900050808     C                   SETOFF                                       1385
068000970117     C                   SETOFF                                       90         GENERICO DI ERRORE
068100970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
068200970117IF  1C     D00TLA        IFEQ      'D'                                          *CALCOLO DELTA
068300970117     C                   SETON                                        02        *ND ALCUNI CAMPI
068400970117X   1C                   ELSE                                                   *CALCOLO DELTA
068500970117     C                   SETOFF                                       02        *NESSUNA ND
068600970117E   1C                   ENDIF
068700970117     C*
068800970117     C                   ENDSR
068900970117     C*------------------------------------------------------------------------*
069000970117     C* GESVD2 - GESTIONE SECONDA VIDEATA
069100970117     C*------------------------------------------------------------------------*
069200970117     C     GESVD2        BEGSR
069300970117     C*
069400970117     C* EMISSIONE VIDEATA
069500970117     C                   WRITE     TE01D00
069600970117     C                   EXFMT     TE01D02
069700970117     C                   EXSR      SETVD2
069800970117     C* F03-FINE
069900970117IF  1C     *INKC         IFEQ      *ON
070000970117     C                   EXSR      F03GES
070100970117     C                   GOTO      FINVD2
070200970117E   1C                   ENDIF
070300970117     C* F05-CARICA DA ALTRA TARIFFA
070400970117IF  1C     *INKE         IFEQ      *ON
070500970117     C                   EXSR      F05GES
070600970117     C                   GOTO      FINVD2
070700970117E   1C                   ENDIF
070800970117     C* CONTROLLA VIDEO
070900970117     C                   EXSR      CTRVD2
071000970117IF  1C     *IN90         IFEQ      *ON
071100970117     C                   GOTO      FINVD2
071200970117E   1C                   ENDIF
071300970117     C* F12-RITORNO
071400970117IF  1C     *INKL         IFEQ      *ON
071500970117     C                   EXSR      IMPVD1
071600970117     C                   GOTO      FINVD2
071700970117E   1C                   ENDIF
071800970117     C* F06-AGGIORNA E PROSEGUE
071900970117IF  1C     *INKF         IFEQ      *ON
072000970117     C                   EXSR      F06GES
072100970117     C                   GOTO      FINVD2
072200970117E   1C                   ENDIF
072300970117     C*
072400970117     C* EMETTE VIDEATA SUCCESSIVA
072500970117     C                   EXSR      IMPVD3
072600970117     C*
072700970117     C     FINVD2        ENDSR
072800970117     C*------------------------------------------------------------------------*
072900970117     C* CTRVD2 - CONTROLLA SECONDA VIDEATA
073000970117     C*------------------------------------------------------------------------*
073100970117     C     CTRVD2        BEGSR
073200041105     C*--------------------
073300041105     C* DESCRIZIONE
073400041105     C*--------------------
073500980504IF  1C     V2CDCV        IFEQ      *BLANKS
073600980504     C                   SETON                                        7590
073700980504     C                   MOVEL     MSG(44)       $MSG
073800980504     C                   GOTO      FINCT2
073900980504E   1C                   ENDIF
074000970314     C*--------------------
074100970314     C* TIPO E IMPORTO ARROTONDAMENTO
074200970314     C*--------------------
074300970317IF  1C     V2CFAR        IFNE      *BLANKS
074400970314     C     V2CFAR        ANDNE     'E'
074500970314     C     V2CFAR        ANDNE     'D'
074600970314     C     V2CFAR        ANDNE     'A'
074700970314     C                   SETON                                        6690
074800970314     C                   MOVEL     MSG(48)       $MSG
074900970314     C                   GOTO      FINCT2
075000970317E   1C                   ENDIF
075100970314     C*
075200970317IF  1C     V2CFAR        IFNE      *BLANKS
075300970318     C     V2CIAR        ANDGT     *ZEROS
075400970318     C     V2CFAR        OREQ      *BLANKS
075500011008     C     V2CIAR        ANDEQ     *ZEROS
075600011008     C* Se trattasi di divisa ITL controllo che non abbiano inserito decimali
075700011008     C     D00DIV        IFEQ      'ITL'
075800011008     C                   Z-ADD     V2CIAR        NUMDEC
075900011008     C     NUMDEC        IFGT      *ZEROS
076000011008     C                   SETON                                        6790
076100011008     C                   MOVEL     MSG(52)       $MSG
076200011008     C                   GOTO      FINCT2
076300011008     C                   ENDIF
076400011008     C                   ENDIF
076500011010     C* Se trattasi di divisa EUR controllo che siano valori ben determinati
076600011010     C     D00DIV        IFEQ      'EUR'
076700011010     C     V2CIAR        IFGT      *ZEROS
076800011030     C     V2CIAR        IFEQ      0,10
076900011030     C     V2CIAR        OREQ      0,25
077000011010     C     V2CIAR        OREQ      0,50
077100011010     C     V2CIAR        OREQ      0,75
077200011010     C     V2CIAR        OREQ      1,00
077300011010     C                   ELSE
077400011010     C                   SETON                                        6790
077500011010     C                   MOVEL     MSG(55)       $MSG
077600011010     C                   GOTO      FINCT2
077700011010     C                   ENDIF
077800011010     C                   ENDIF
077900011010     C                   ENDIF
078000011010     C*
078100970318X   1C                   ELSE
078200970314     C                   SETON                                        666790
078300970314     C                   MOVEL     MSG(49)       $MSG
078400970314     C                   GOTO      FINCT2
078500970314E   1C                   ENDIF
078600970117     C*--------------------
078700970117     C* QUALITA' CLIENTE
078800970117     C*--------------------
078900980521     C                   MOVEL     '0'           WERR
079000980521     C                   MOVEL     V2CCLV        WCLV
079100980521     C                   EXSR      CHKCLV
079200980521     C                   MOVEL     WCLV          V2CCLV
079300980521IF  1C     WERR          IFEQ      '1'
079400970117     C                   SETON                                        2290
079500970117     C                   MOVEL     MSG(5)        $MSG
079600970117     C                   GOTO      FINCT2
079700980521E   1C                   ENDIF
079800970117     C*--------------------
079900970117     C* PASSAGGIO DATI IN BOLLETTAZIONE
080000970117     C*--------------------
080100970117IF  1C     V2CBOL        IFEQ      'A'
080200970117     C     V2CBOL        OREQ      'B'
080300970117     C     V2CBOL        OREQ      *BLANKS
080400970117X   1C                   ELSE
080500970117     C                   SETON                                        2390
080600970117     C                   MOVEL     MSG(6)        $MSG
080700970117     C                   GOTO      FINCT2
080800970117E   1C                   ENDIF
080900021104     C*--------------------
081000021104     C* PROCEDURA DISCK "C"
081100021104     C*--------------------
081200021104IF  1C     V2CDKC        IFEQ      'S'
081300021104     C     V2CDKC        OREQ      *BLANKS
081400021104X   1C                   ELSE
081500021104     C                   SETON                                        8190
081600021104     C                   MOVEL     MSG(57)       $MSG
081700021104     C                   GOTO      FINCT2
081800021104E   1C                   ENDIF
081900021104     C*--------------------
082000021104     C* PASSAGGIO DATI IN BOLLETTAZIONE (SOL. "B") / PROCEDURA DISCK "C"
082100021104     C*--------------------
082200021104     C                   IF        V2CBOL='B' AND V2CDKC='S'
082300021104     C                   SETON                                        238190
082400021104     C                   MOVEL     MSG(58)       $MSG
082500021104     C                   GOTO      FINCT2
082600021104     C                   ENDIF
082700021031     C*--------------------
082800021104     C* PASSAGGIO DATI IN BOLLETTAZIONE (SOL. "B") / CHI ETICHETTA
082900021031     C*--------------------
083000021104     C                   IF        V2CBOL='B' AND V2CIX1<>'K'
083100021031     C                   SETON                                        238290
083200021104     C                   MOVEL     MSG(67)       $MSG
083300021031     C                   GOTO      FINCT2
083400021031     C                   ENDIF
083500021031     C*--------------------
083600021031     C* CHI ETICHETTA
083700021031     C*--------------------
083800021031IF  1C     V2CIX1        IFEQ      'K'
083900021031     C     V2CIX1        OREQ      'C'
084000021031     C     V2CIX1        OREQ      *BLANKS
084100021031X   1C                   ELSE
084200021031     C                   SETON                                        8290
084300021104     C                   MOVEL     MSG(57)       $MSG
084400021031     C                   GOTO      FINCT2
084500021031E   1C                   ENDIF
084600021031     C*--------------------
084700021031     C* PROCEDURA DISCK "C" / CHI ETICHETTA
084800021031     C*--------------------
084900021104     C                   IF        V2CBOL <> 'B'
085000021104     C                   IF        V2CDKC='S'
085100021104     C                   IF        V2CIX1=' ' OR  V2CIX1='K'
085200021104     C                   ELSE
085300021031     C                   SETON                                        818290
085400021104     C                   MOVEL     MSG(68)       $MSG
085500021031     C                   GOTO      FINCT2
085600021031     C                   ENDIF
085700021104     C                   ENDIF
085800021104     C* Eseguo doppio test x diversificare il messaggio d errore a video
085900021104     C                   IF        V2CDKC=*blanks
086000021104     C                   IF        V2CIX1=' ' OR V2CIX1='C'
086100021104     C                   ELSE
086200021104     C                   SETON                                        818290
086300021104     C                   MOVEL     MSG(69)       $MSG
086400021104     C                   GOTO      FINCT2
086500021104     C                   ENDIF
086600021104     C                   ENDIF
086700021104     C                   ENDIF
086800021031     C*--------------------
086900021031     C* CHI SCARICA
087000021031     C*--------------------
087100021031IF  1C     V2CIX2        IFEQ      'C'
087200021031     C     V2CIX2        OREQ      *BLANKS
087300021031X   1C                   ELSE
087400021031     C                   SETON                                        8390
087500021104     C                   MOVEL     MSG(57)       $MSG
087600021031     C                   GOTO      FINCT2
087700021031E   1C                   ENDIF
087800970117     C*--------------------
087900970117     C* SPEDIZIONI/RITIRO
088000970117     C*--------------------
088100970117IF  1C     V2CSPR        IFEQ      *ZEROS
088200970117     C                   SETON                                        2890
088300970117     C                   MOVEL     MSG(10)       $MSG
088400970117     C                   GOTO      FINCT2
088500970117E   1C                   ENDIF
088600970117     C*--------------------
088700970117     C* PESO MEDIO PER COLLO
088800970117     C*--------------------
088900970117IF  1C     V2CPMC        IFEQ      *ZEROS
089000970117     C                   SETON                                        2790
089100970117     C                   MOVEL     MSG(7)        $MSG
089200970117     C                   GOTO      FINCT2
089300970117E   1C                   ENDIF
089400970117     C*--------------------
089500970117     C* VOLUME MEDIO PER COLLO
089600970117     C*--------------------
089700970117IF  1C     V2CVMC        IFEQ      *ZEROS
089800970117     C                   SETON                                        3290
089900970117     C                   MOVEL     MSG(8)        $MSG
090000970117     C                   GOTO      FINCT2
090100970117E   1C                   ENDIF
090200970117     C*--------------------
090300970117     C* % FRANCO
090400970117     C*--------------------
090500980803IF  1C     V2CFRA        IFGT      *ZEROS
090600980803     C     V2CFRA        ANDNE     100
090700980803     C                   SETON                                        3090
090800980803     C                   MOVEL     MSG(50)       $MSG
090900980803     C                   GOTO      FINCT2
091000980803E   1C                   ENDIF
091100970117     C*--------------------
091200970117     C* % ASSEGNATO
091300970117     C*--------------------
091400980803IF  1C     V2CASS        IFGT      *ZEROS
091500980803     C     V2CASS        ANDNE     100
091600980803     C                   SETON                                        3190
091700980803     C                   MOVEL     MSG(50)       $MSG
091800980803     C                   GOTO      FINCT2
091900980803E   1C                   ENDIF
092000970117     C*--------------------
092100980803     C* % FRANCO E % ASSEGNATO
092200970117     C*--------------------
092300980803IF  1C     V2CFRA        IFEQ      *ZEROS
092400980803     C     V2CASS        ANDEQ     *ZEROS
092500980803     C     V2CFRA        ORGT      *ZEROS
092600980803     C     V2CASS        ANDGT     *ZEROS
092700980803     C                   SETON                                        303190
092800980803     C                   MOVEL     MSG(51)       $MSG
092900980803     C                   GOTO      FINCT2
093000980803E   1C                   ENDIF
093100970117     C*--------------------
093200970117     C* % CONSEGNA AI PRIVATI
093300970117     C*--------------------
093400970117IF  1C     V2CPCV        IFGT      100
093500970117     C                   SETON                                        3390
093600970117     C                   MOVEL     MSG(27)       $MSG
093700970117     C                   GOTO      FINCT2
093800970117E   1C                   ENDIF
093900970117     C*--------------------
094000011009     C* % MERCE A MAGAZZINO
094100970117     C*--------------------
094200970117IF  1C     V2CPCM        IFGT      100
094300970117     C                   SETON                                        3490
094400970117     C                   MOVEL     MSG(27)       $MSG
094500970117     C                   GOTO      FINCT2
094600131121     c                   else
094700131121     c     v5csam        add       v2cpcm        wn7
094800131121IF  1C     WN7           IFGT      100
094900131121     C                   SETON                                        3490
095000131121     C                   MOVEL     MSG(82)       $MSG
095100131121     C                   GOTO      FINCT2
095200131121E   1C                   ENDIF
095300970117E   1C                   ENDIF
095400970117     C*--------------------
095500970117     C* % CONSEGNA FERMO DEPOSITO
095600970117     C*--------------------
095700970117IF  1C     V2CPFD        IFGT      100
095800970117     C                   SETON                                        3590
095900970117     C                   MOVEL     MSG(27)       $MSG
096000970117     C                   GOTO      FINCT2
096100131120E   1C                   ELSE
096200131120     C* FERMO DEPOSITO + CONSEGNA ISOLA DEVE ESSERE <= 100
096300131120IF  1C     V2CPFD        ADD       V5CCIS        WN7
096400131120IF  1C     WN7           IFGT      100
096500131120     C                   SETON                                        3590
096600131120     C                   MOVEL     MSG(81)       $MSG
096700131120     C                   GOTO      FINCT2
096800131120E   1C                   ENDIF
096900131121E   1C                   ENDIF
097000040730     C*--------------------
097100040730     C* % merce di valore
097200040730     C*--------------------
097300040730IF  1C     V2Cval        IFGT      100
097400040730     C                   SETON                                        1390
097500040730     C                   MOVEL     MSG(27)       $MSG
097600040730     C                   GOTO      FINCT2
097700040730E   1C                   ENDIF
097800050808     C*--------------------
097900151119     C* ritiro telefonico / mail
098000050808     C*--------------------
098100151119IF  1C     V2Crtm        IFne      'S'
098200151119IF  1C     V2Crtm        andne     ' '
098300050808     C                   SETON                                        8590
098400151119     C                   MOVEL     MSG(85)       $MSG
098500050808     C                   GOTO      FINCT2
098600050808E   1C                   ENDIF
098700151119IF  1C     V2Crtm        IFeq      'S'
098800151119IF  1C     V2Cirp        IFne      'S'
098900151119IF  1C     V2Cirp        andne     'N'
099000151119     C                   SETON                                        8690
099100151119     C                   MOVEL     MSG(86)       $MSG
099200151119     C                   GOTO      FINCT2
099300151119E   1C                   ENDIF
099400151119E   1C                   ENDIF
099500970117     C*
099600970117     C     FINCT2        ENDSR
099700980521     C*------------------------------------------------------------------------*
099800980521     C* CHKCLV - CONTROLLA LA QUALITA' CLIENTE
099900980521     C*------------------------------------------------------------------------*
100000980521     C     CHKCLV        BEGSR
100100980521     C*
100200980521     C     *LIKE         DEFINE    V2CCLV        WCLV
100300980521     C*
100400980521     C* RICHIESTO ELENCO CODICI
100500980521     C     '?'           SCAN      WCLV                                   99
100600980521IF  1C     *IN99         IFEQ      *ON
100700020923     C                   Z-ADD     1             §KUT
100800980521     C                   MOVEL     'IC'          §COD
100900980521     C                   MOVEL     *BLANKS       §KEY
101000980521     C                   MOVEL     *BLANKS       §DES
101100980521     C                   CALL      'X§TABER'
101200980521     C                   PARM                    §KUT              1 0
101300980521     C                   PARM                    §COD              2
101400980521     C                   PARM                    §KEY              8
101500980521     C                   PARM                    §DES             30
101600980521     C                   MOVEL     §KEY          WCLV
101700980521X   1C                   ELSE
101800980521     C*
101900980521     C* CONTROLLA ESISTENZA CODICE IN ARCHIVIO
102000020923     C                   Z-ADD     1             KTBKUT
102100980521     C                   MOVEL     'IC'          KTBCOD
102200980521     C                   MOVEL(P)  WCLV          KTBKEY
102300980521     C     KEYTAB        CHAIN     TABEL00F                           99
102400980521IF  2C     *IN99         IFEQ      '1'
102500980521     C     TBLFLG        ORNE      *BLANKS
102600980521     C                   MOVEL     '1'           WERR
102700980521E   2C                   ENDIF
102800980521E   1C                   ENDIF
102900980521     C*
103000980521     C                   ENDSR
103100970117     C*------------------------------------------------------------------------*
103200970117     C* IMPVD3 - IMPOSTAZIONE TERZA VIDEATA
103300970117     C*------------------------------------------------------------------------*
103400970117     C     IMPVD3        BEGSR
103500970117     C*
103600970117     C* REIMPOSTA INDICATORI
103700970117     C                   EXSR      SETVD3
103800970117     C                   MOVEL     '3'           TIPVID
103900970117     C*
104000970117     C                   Z-ADD     V1CLNP        V3CLNP
104100970117     C                   MOVEL     V1DLNP        V3DLNP
104200970314     C                   Z-ADD     V1CDTC        V3CDTC
104300970117     C*
104400970117     C                   WRITE     TE01D00                                      *TESTATA
104500970117     C*
104600970117     C                   ENDSR
104700970117     C*------------------------------------------------------------------------*
104800970117     C* SETVD3 - IMPOSTAZIONI INDICATORI TERZA VIDEATA
104900970117     C*------------------------------------------------------------------------*
105000970117     C     SETVD3        BEGSR
105100970117     C*
105200970317     C                   SETOFF                                       5759       ERRORI
105300970117     C                   SETOFF                                       4445
105400970117     C                   SETOFF                                       545556
105500970117     C                   SETOFF                                       60
105600970117     C                   SETOFF                                       90         GENERICO DI ERRORE
105700970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
105800970117     C*
105900970117     C                   ENDSR
106000970117     C*------------------------------------------------------------------------*
106100970117     C* GESVD3 - GESTIONE TERZA VIDEATA
106200970117     C*------------------------------------------------------------------------*
106300970117     C     GESVD3        BEGSR
106400970117     C*
106500970117     C* EMISSIONE VIDEATA
106600970117     C                   WRITE     TE01D00                                      *TESTATA
106700970117     C                   EXFMT     TE01D03
106800970117     C                   EXSR      SETVD3
106900970117     C* F03-FINE
107000970117IF  1C     *INKC         IFEQ      *ON
107100970117     C                   EXSR      F03GES
107200970117     C                   GOTO      FINVD3
107300970117E   1C                   ENDIF
107400970117     C* CONTROLLA VIDEO
107500970117     C                   EXSR      CTRVD3
107600970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
107700970117     C                   GOTO      FINVD3
107800970117E   1C                   ENDIF
107900970117     C* F12-RITORNO
108000970117IF  1C     *INKL         IFEQ      *ON
108100970117     C                   EXSR      IMPVD2
108200970117     C                   GOTO      FINVD3
108300970117E   1C                   ENDIF
108400970117     C*
108500970117     C* EMETTE VIDEATA SUCCESSIVA
108600970117     C                   EXSR      IMPVD4
108700970117     C*
108800970117     C     FINVD3        ENDSR
108900970117     C*------------------------------------------------------------------------*
109000970117     C* CTRVD3 - CONTROLLA TERZA VIDEATA
109100970117     C*------------------------------------------------------------------------*
109200970117     C     CTRVD3        BEGSR
109300970117     C*---
109400970117     C* INOLTRO
109500970117     C*---
109600970117     C*--------------------
109700970117     C* % SPEDIZIONI IN PROVINCIA
109800970117     C*--------------------
109900970117IF  1C     V3CPPV        IFGT      100
110000970117     C                   SETON                                        5790
110100970117     C                   MOVEL     MSG(27)       $MSG
110200970117     C                   GOTO      FINCT3
110300970117E   1C                   ENDIF
110400970117     C*--------------------
110500970317     C* INGLOBA INOLTRO
110600970117     C*--------------------
110700970314IF  1C     V3CSIN        IFEQ      'S'
110800970314     C     V3CSIN        OREQ      'N'
110900970314X   1C                   ELSE
111000970314     C                   SETON                                        5990
111100970314     C                   MOVEL     MSG(40)       $MSG
111200970314     C                   GOTO      FINCT3
111300970314E   1C                   ENDIF
111400970314     C*
111500970314IF  1C     V3CPPV        IFEQ      *ZEROS
111600970314     C     V3CSIN        ANDEQ     'S'
111700970314     C                   SETON                                        575990
111800970314     C                   MOVEL     MSG(41)       $MSG
111900970314     C                   GOTO      FINCT3
112000970314E   1C                   ENDIF
112100970117     C*---
112200970117     C* ESPRESSO
112300970117     C*---
112400970117     C*--------------------
112500970117     C* % ESPRESSI
112600970117     C*--------------------
112700970117IF  1C     V3CESP        IFGT      100
112800970117     C                   SETON                                        4490
112900970117     C                   MOVEL     MSG(27)       $MSG
113000970117     C                   GOTO      FINCT3
113100970117E   1C                   ENDIF
113200970117     C*
113300970117IF  1C     V0CTSP        IFEQ      'E'
113400970117     C     V3CESP        ANDLT     100
113500970117     C                   SETON                                        4490
113600970117     C                   MOVEL     MSG(28)       $MSG
113700090611     c                   movel     costs         $msg
113800970117     C                   GOTO      FINCT3
113900970117E   1C                   ENDIF
114000970117     C*--------------------
114100970117     C* INGLOBA ESPRESSO
114200970117     C*--------------------
114300970117IF  1C     V3CIES        IFEQ      'S'
114400970117     C     V3CIES        OREQ      'N'
114500970117X   1C                   ELSE
114600970117     C                   SETON                                        4590
114700970117     C                   MOVEL     MSG(30)       $MSG
114800970117     C                   GOTO      FINCT3
114900970117E   1C                   ENDIF
115000970117     C*
115100970117IF  1C     V0CTSP        IFEQ      'E'
115200970117     C     V3CIES        ANDNE     'S'
115300970117     C                   SETON                                        4590
115400970117     C                   MOVEL     MSG(29)       $MSG
115500090611     c                   movel     costs         $msg
115600970117     C                   GOTO      FINCT3
115700970117E   1C                   ENDIF
115800970117     C*
115900970117IF  1C     V3CESP        IFEQ      *ZEROS
116000970117     C     V3CIES        ANDEQ     'S'
116100970117     C                   SETON                                        444590
116200970117     C                   MOVEL     MSG(31)       $MSG
116300970117     C                   GOTO      FINCT3
116400970117E   1C                   ENDIF
116500970117     C*---
116600970117     C* GIACENZA
116700970117     C*---
116800970117IF  1C     V3CPGI        IFGT      100
116900970117     C                   SETON                                        5490
117000970117     C                   MOVEL     MSG(27)       $MSG
117100970117     C                   GOTO      FINCT3
117200970117E   1C                   ENDIF
117300970117     C*--------------------
117400970117     C* GIORNI MEDI DI SOSTA
117500970117     C*--------------------
117600970117IF  1C     V3CPGI        IFGT      *ZEROS
117700970117IF  2C     V3CGGG        IFEQ      *ZEROS
117800970117     C                   SETON                                        5590
117900970117     C                   MOVEL     MSG(36)       $MSG
118000970117     C                   GOTO      FINCT3
118100970117E   2C                   ENDIF
118200970117X   1C                   ELSE
118300970117IF  2C     V3CGGG        IFGT      *ZEROS
118400970117     C                   SETON                                        5590
118500970117     C                   MOVEL     MSG(37)       $MSG
118600970117     C                   GOTO      FINCT3
118700970117E   2C                   ENDIF
118800970117E   1C                   ENDIF
118900970117     C*--------------------
119000970117     C* RIAPERTURE GIACENZA
119100970117     C*--------------------
119200970424IF  1C     V3CPGI        IFGT      *ZEROS                                       *SI %GIACENZE
119300970424     C                   Z-ADD     1             V3CNRG                         *FISSO UNA RIAPERTUR
119400970424X   1C                   ELSE                                                   *NO %GIACENZE
119500970424     C                   Z-ADD     *ZEROS        V3CNRG                         *NO RIAPERTURE
119600970424E   1C                   ENDIF
119700970424     C*--------------------
119800970117     C* INGLOBA GIACENZA
119900970117     C*--------------------
120000970117IF  1C     V3CIGI        IFEQ      'S'
120100970117     C     V3CIGI        OREQ      'N'
120200970117X   1C                   ELSE
120300970117     C                   SETON                                        5690
120400970117     C                   MOVEL     MSG(34)       $MSG
120500970117     C                   GOTO      FINCT3
120600970117E   1C                   ENDIF
120700970117     C*
120800970117IF  1C     V3CIGI        IFEQ      'S'
120900970117     C     V3CPGI        ANDEQ     *ZEROS
121000970117     C                   SETON                                        545690
121100970117     C                   MOVEL     MSG(35)       $MSG
121200970117     C                   GOTO      FINCT3
121300970117E   1C                   ENDIF
121400970117     C*
121500970117     C     FINCT3        ENDSR
121600970117     C*------------------------------------------------------------------------*
121700970117     C* IMPVD4 - IMPOSTAZIONE QUARTA VIDEATA
121800970117     C*------------------------------------------------------------------------*
121900970117     C     IMPVD4        BEGSR
122000970117     C*
122100970117     C* REIMPOSTA INDICATORI
122200970117     C                   EXSR      SETVD4
122300970117     C                   MOVEL     '4'           TIPVID
122400970117     C*
122500970117     C                   Z-ADD     V1CLNP        V4CLNP
122600970117     C                   MOVEL     V1DLNP        V4DLNP
122700970314     C                   Z-ADD     V1CDTC        V4CDTC
122800970117     C*
122900970117     C                   WRITE     TE01D00                                      *TESTATA
123000970117     C*
123100970117     C                   ENDSR
123200970117     C*------------------------------------------------------------------------*
123300970117     C* SETVD4 - IMPOSTAZIONI INDICATORI QUARTA VIDEATA
123400970117     C*------------------------------------------------------------------------*
123500970117     C     SETVD4        BEGSR
123600970117     C*
123700970117     C                   SETOFF                                       464748     ERRORI
123800040812     C                   SETOFF                                       491215
123900970117     C                   SETOFF                                       505152
124000040824     C                   SETOFF                                       531617
124100150227     C                   SETOFF                                       885818
124200970117     C                   SETOFF                                       90         GENERICO DI ERRORE
124300970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
124400970117     C*
124500970117     C                   ENDSR
124600970117     C*------------------------------------------------------------------------*
124700970117     C* GESVD4 - GESTIONE QUARTA VIDEATA
124800970117     C*------------------------------------------------------------------------*
124900970117     C     GESVD4        BEGSR
125000970117     C*
125100970117     C* EMISSIONE VIDEATA
125200970117     C                   WRITE     TE01D00                                      *TESTATA
125300970117     C                   EXFMT     TE01D04
125400970117     C                   EXSR      SETVD4
125500970117     C* F03-FINE
125600970117IF  1C     *INKC         IFEQ      *ON
125700970117     C                   EXSR      F03GES
125800970117     C                   GOTO      FINVD4
125900970117E   1C                   ENDIF
126000970117     C* CONTROLLA VIDEO
126100970117     C                   EXSR      CTRVD4
126200970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
126300970117     C                   GOTO      FINVD4
126400970117E   1C                   ENDIF
126500970117     C* F12-RITORNO
126600970117IF  1C     *INKL         IFEQ      *ON
126700970117     C                   EXSR      IMPVD3
126800970117     C                   GOTO      FINVD4
126900970117E   1C                   ENDIF
127000970117     C*
127100970117     C* EMETTE VIDEATA SUCCESSIVA
127200970117     C                   EXSR      IMPVD5
127300970117     C*
127400970117     C     FINVD4        ENDSR
127500970117     C*------------------------------------------------------------------------*
127600970117     C* CTRVD4 - CONTROLLA QUARTA VIDEATA
127700970117     C*------------------------------------------------------------------------*
127800970117     C     CTRVD4        BEGSR
127900970117     C*---
128000970117     C* CONTRASSEGNO
128100970117     C*---
128200970117     C*--------------------
128300970117     C* % CONTRASSEGNI BARTOLINI
128400970117     C*--------------------
128500970117IF  1C     V4CPAB        IFGT      100
128600970117     C                   SETON                                        4690
128700970117     C                   MOVEL     MSG(27)       $MSG
128800970117     C                   GOTO      FINCT4
128900970117E   1C                   ENDIF
129000970117     C*--------------------
129100970117     C* % CONTRASSEGNI MITTENTE
129200970117     C*--------------------
129300970117IF  1C     V4CPAM        IFGT      100
129400970117     C                   SETON                                        4790
129500970117     C                   MOVEL     MSG(27)       $MSG
129600970117     C                   GOTO      FINCT4
129700970117E   1C                   ENDIF
129800970117     C*--------------------
129900970117     C* % CONTRASSEGNI BARTOLINI + MITTENTE
130000970117     C*--------------------
130100970117     C     V4CPAB        ADD       V4CPAM        WN7               7 3
130200970117IF  1C     WN7           IFGT      100
130300970117     C                   SETON                                        464790
130400970117     C                   MOVEL     MSG(19)       $MSG
130500970117     C                   GOTO      FINCT4
130600970117E   1C                   ENDIF
130700970117     C*--------------------
130800970117     C* INGLOBA CONTRASSEGNO
130900970117     C*--------------------
131000970117IF  1C     V4CICA        IFEQ      'S'
131100970117     C     V4CICA        OREQ      'N'
131200970117X   1C                   ELSE
131300970117     C                   SETON                                        4890
131400970117     C                   MOVEL     MSG(20)       $MSG
131500970117     C                   GOTO      FINCT4
131600970117E   1C                   ENDIF
131700970117     C*
131800970117IF  1C     V4CICA        IFEQ      'S'
131900970117     C     V4CPAB        ANDEQ     *ZEROS
132000970117     C     V4CPAM        ANDEQ     *ZEROS
132100970117     C                   SETON                                        464748
132200970117     C                   SETON                                        90
132300970117     C                   MOVEL     MSG(21)       $MSG
132400970117     C                   GOTO      FINCT4
132500970117E   1C                   ENDIF
132600970117     C*--------------------
132700970117     C* VALORE MEDIO C/ASSEGNO
132800970117     C*--------------------
132900970117IF  1C     V4CPAB        IFGT      *ZEROS
133000970117     C     V4CPAM        ORGT      *ZEROS
133100970117IF  2C     V4CCAS        IFEQ      *ZEROS
133200970117     C                   SETON                                        4990
133300970117     C                   MOVEL     MSG(22)       $MSG
133400970117     C                   GOTO      FINCT4
133500970117E   2C                   ENDIF
133600011008     C* Se trattasi di divisa ITL controllo che non abbiano inserito decimali
133700011008     C     D00DIV        IFEQ      'ITL'
133800011008     C                   Z-ADD     V4CCAS        NUMDEC
133900011008     C     NUMDEC        IFGT      *ZEROS
134000011008     C                   SETON                                        4990
134100011008     C                   MOVEL     MSG(52)       $MSG
134200011008     C                   GOTO      FINCT4
134300011008     C                   ENDIF
134400011008     C                   ENDIF
134500011008     C*
134600970117X   1C                   ELSE
134700970117IF  2C     V4CCAS        IFGT      *ZEROS
134800970117     C                   SETON                                        4990
134900970117     C                   MOVEL     MSG(32)       $MSG
135000970117     C                   GOTO      FINCT4
135100970117E   2C                   ENDIF
135200970117E   1C                   ENDIF
135300970117     C*---
135400970117     C* ASSICURAZIONE
135500970117     C*---
135600970117     C*--------------------
135700970117     C* % ASSICURAZIONE R.C.V.
135800970117     C*--------------------
135900970117IF  1C     V4CPAR        IFGT      100
136000970117     C                   SETON                                        5090
136100970117     C                   MOVEL     MSG(27)       $MSG
136200970117     C                   GOTO      FINCT4
136300970117E   1C                   ENDIF
136400060313     c* rcv o ac base
136500060313IF  1C     V4CPAR        IFGT      0
136600060313IF  1C     V4Cacb        andgt     0
136700060313     C                   SETON                                        509058
136800060313     C                   MOVEL     MSG(79)       $MSG
136900060313     C                   GOTO      FINCT4
137000060313E   1C                   ENDIF
137100060313     C*--------------------
137200060313     C* % ASSICURAZIONE AC BASE
137300060313     C*--------------------
137400060313IF  1C     V4Cacb        IFGT      100
137500060313     C                   SETON                                        5890
137600060313     C                   MOVEL     MSG(27)       $MSG
137700060313     C                   GOTO      FINCT4
137800060313E   1C                   ENDIF
137900970117     C*--------------------
138000060313     C* % ASSICURAZIONE AC PLUS
138100970117     C*--------------------
138200970117IF  1C     V4CPAC        IFGT      100
138300970117     C                   SETON                                        5190
138400970117     C                   MOVEL     MSG(27)       $MSG
138500970117     C                   GOTO      FINCT4
138600970117E   1C                   ENDIF
138700970117     C*--------------------
138800970117     C* % ASSICURAZIONE R.C.V. + ASSICURAZIONE PER CONTO
138900970117     C*--------------------
139000060313     c                   if        (v4cpar + v4cpac + v4cacb) <> 100
139100970117     C                   SETON                                        505190
139200060313     C                   SETON                                        58
139300970117     C                   MOVEL     MSG(23)       $MSG
139400970117     C                   GOTO      FINCT4
139500970117E   1C                   ENDIF
139600970117     C*--------------------
139700970117     C* INGLOBA ASSICURAZIONE
139800970117     C*--------------------
139900970117IF  1C     V4CIAS        IFEQ      'S'
140000970117     C     V4CIAS        OREQ      'N'
140100970117X   1C                   ELSE
140200970117     C                   SETON                                        5290
140300970117     C                   MOVEL     MSG(24)       $MSG
140400970117     C                   GOTO      FINCT4
140500970117E   1C                   ENDIF
140600970117     C*
140700970117IF  1C     V4CIAS        IFEQ      'S'
140800970117     C     V4CPAR        ANDEQ     *ZEROS
140900970117     C     V4CPAC        ANDEQ     *ZEROS
141000970117     C                   SETON                                        505152
141100970117     C                   SETON                                        90
141200970117     C                   MOVEL     MSG(25)       $MSG
141300970117     C                   GOTO      FINCT4
141400970117E   1C                   ENDIF
141500970117     C*--------------------
141600970117     C* VALORE MEDIO IMPORTO DA ASSICURARE
141700970117     C*--------------------
141800970117IF  1C     V4CPAC        IFGT      *ZEROS
141900970117IF  2C     V4CASC        IFEQ      *ZEROS
142000970117     C                   SETON                                        5390
142100970117     C                   MOVEL     MSG(26)       $MSG
142200970117     C                   GOTO      FINCT4
142300970117E   2C                   ENDIF
142400011008     C* Se trattasi di divisa ITL controllo che non abbiano inserito decimali
142500011008     C     D00DIV        IFEQ      'ITL'
142600011008     C                   Z-ADD     V4CASC        NUMDEC
142700011008     C     NUMDEC        IFGT      *ZEROS
142800011008     C                   SETON                                        5390
142900011008     C                   MOVEL     MSG(52)       $MSG
143000011008     C                   GOTO      FINCT4
143100011008     C                   ENDIF
143200011008     C                   ENDIF
143300011008     C*
143400970117X   1C                   ELSE
143500970117IF  2C     V4CASC        IFGT      *ZEROS
143600970117     C                   SETON                                        5390
143700970117     C                   MOVEL     MSG(33)       $MSG
143800970117     C                   GOTO      FINCT4
143900970117E   2C                   ENDIF
144000970117E   1C                   ENDIF
144100040802     C*---
144200040802     C* BANCALI
144300040802     C*---
144400040824X    c* se %sped. oltre 20 colli  > 100 errore
144500040811IF  1C     V4Csbnb       IFGT      100
144600040802     C                   SETON                                        1290
144700040802     C                   MOVEL     MSG(27)       $MSG
144800040802     C                   GOTO      FINCT4
144900040802E   1C                   ENDIF
145000040824X    c* se %sped. oltre 20 colli  obbligo anche il nr. bancali medi
145100040811IF  1C                   IF        (V4Csbnb > 0 and V4Cnbn = 0) or
145200040811     C                             (V4Csbnb = 0 and V4Cnbn <>0)
145300040811     C                   SETON                                        129016
145400040811     C                   MOVEL     MSG(73)       $MSG
145500040811     C                   GOTO      FINCT4
145600040811E   1C                   ENDIF
145700040824X    c* se %sped. bancali Bartolini > 100 errore
145800040824IF  1C     V4Csban       IFGT      100
145900040824     C                   SETON                                        1790
146000040824     C                   MOVEL     MSG(27)       $MSG
146100040824     C                   GOTO      FINCT4
146200040824E   1C                   ENDIF
146300040824X    c* se %sped. bancali Bartolini obbligo anche il nr. bancali medi
146400040824IF  1C                   IF        (V4Csban > 0 and V4Cnba = 0) or
146500040824     C                             (V4Csban = 0 and V4Cnba <>0)
146600040824     C                   SETON                                        179015
146700040824     C                   MOVEL     MSG(73)       $MSG
146800040824     C                   GOTO      FINCT4
146900040824E   1C                   ENDIF
147000150227IF  1C                   IF        (V4Csban > 0 and V4Ctba = 0)
147100150227     C                   SETON                                        189017
147200150227     C                   MOVEL     MSG(84)       $MSG
147300150227     C                   GOTO      FINCT4
147400150227E   1C                   ENDIF
147500150227     c                   clear                   v4dtba
147600150227IF  1C                   IF        V4Ctba <> 0
147700150227     c                   exsr      cnttba
147800150227E   1C                   ENDIF
147900050802     C*--------------------
148000050802     C* INGLOBA bancali ban
148100050802     C*--------------------
148200050802IF  1C     V4CIba        IFEQ      'S'
148300050802     C     V4CIba        OREQ      'N'
148400050802X   1C                   ELSE
148500050808     C                   SETON                                        8890
148600050802     C                   MOVEL     MSG(76)       $MSG
148700050802     C                   GOTO      FINCT4
148800050802E   1C                   ENDIF
148900050802     C*
149000050802IF  1C     V4CIba        IFEQ      'S'
149100050802     C     V4Cnba        ANDEQ     *ZEROS
149200050808     C                   SETON                                        151788
149300050802     C                   SETON                                        90
149400050802     C                   MOVEL     MSG(77)       $MSG
149500050802     C                   GOTO      FINCT4
149600050802E   1C                   ENDIF
149700040824     c* se esistono solo spedizione imbancalate dalla coop (BNB) non
149800040824     c* è possibile che esisti la gestione dei bancali Bartolini (BAN)
149900040824     c     v4csbnb       add       v4csban       com100
150000040824     C                   if        com100 > 100
150100040824     C                   SETON                                        121790
150200040824     C                   MOVEL     MSG(71)       $MSG
150300040824     C                   GOTO      FINCT4
150400040824E   2C                   ENDIF
150500040802     C*
150600040802     c*
150700970117     C     FINCT4        ENDSR
150800970117     C*------------------------------------------------------------------------*
150900970117     C* IMPVD5 - IMPOSTAZIONE QUINTA VIDEATA
151000970117     C*------------------------------------------------------------------------*
151100970117     C     IMPVD5        BEGSR
151200970117     C*
151300970117     C* REIMPOSTA INDICATORI
151400970117     C                   EXSR      SETVD5
151500970117     C                   MOVEL     '5'           TIPVID
151600970117     C*
151700970117     C                   Z-ADD     V1CLNP        V5CLNP
151800970117     C                   MOVEL     V1DLNP        V5DLNP
151900970314     C                   Z-ADD     V1CDTC        V5CDTC
152000970117     C*
152100970117     C                   WRITE     TE01D00                                      *TESTATA
152200970117     C*
152300970117     C                   ENDSR
152400970117     C*------------------------------------------------------------------------*
152500970117     C* SETVD5 - IMPOSTAZIONI INDICATORI QUINTA VIDEATA
152600970117     C*------------------------------------------------------------------------*
152700970117     C     SETVD5        BEGSR
152800970117     C*
152900970117     C                   SETOFF                                       363738     ERRORI
153000970117     C                   SETOFF                                       394041
153100970414     C                   SETOFF                                       424368
153200970414     C                   SETOFF                                       697071
153300011009     C                   SETOFF                                       727377
153400011009     C                   SETOFF                                       787980
153500050805     C                   SETOFF                                       1486
153600970117     C                   SETOFF                                       90         GENERICO DI ERRORE
153700970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
153800970117     C*
153900970117     C                   ENDSR
154000970117     C*------------------------------------------------------------------------*
154100970117     C* GESVD5 - GESTIONE QUINTA VIDEATA
154200970117     C*------------------------------------------------------------------------*
154300970117     C     GESVD5        BEGSR
154400970117     C*
154500970117     C* EMISSIONE VIDEATA
154600970117     C                   WRITE     TE01D00                                      *TESTATA
154700970117     C                   EXFMT     TE01D05
154800970117     C                   EXSR      SETVD5
154900970117     C* F03-FINE
155000970117IF  1C     *INKC         IFEQ      *ON
155100970117     C                   EXSR      F03GES
155200970117     C                   GOTO      FINVD5
155300970117E   1C                   ENDIF
155400970117     C* CONTROLLA VIDEO
155500970117     C                   EXSR      CTRVD5
155600970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
155700970117     C                   GOTO      FINVD5
155800970117E   1C                   ENDIF
155900970117     C* F12-RITORNO
156000970117IF  1C     *INKL         IFEQ      *ON
156100970117     C                   EXSR      IMPVD4
156200970117     C                   GOTO      FINVD5
156300970117E   1C                   ENDIF
156400970117     C*
156500970117     C* RITORNA ALLA SECONDA VIDEATA
156600970117     C                   EXSR      IMPVD2
156700970117     C*
156800970117     C     FINVD5        ENDSR
156900970117     C*------------------------------------------------------------------------*
157000970117     C* CTRVD5 - CONTROLLA QUINTA VIDEATA
157100970117     C*------------------------------------------------------------------------*
157200970117     C     CTRVD5        BEGSR
157300970414     C*---
157400970414     C* CONSEGNE PARTICOLARI
157500970414     C*---
157600970117     C*--------------------
157700970414     C* % SU APPUNTAMENTO
157800970117     C*--------------------
157900970117IF  1C     V5CPCA        IFGT      100
158000970117     C                   SETON                                        3690
158100970117     C                   MOVEL     MSG(27)       $MSG
158200970117     C                   GOTO      FINCT5
158300131121     c                   else
158400131121     c     v5cpca        add       v5cpcs        wn7
158500131121IF  1C     WN7           IFGT      100
158600131121     C                   SETON                                        3690
158700131121     C                   MOVEL     MSG(83)       $MSG
158800131121     C                   GOTO      FINCT5
158900131121E   1C                   ENDIF
159000970117E   1C                   ENDIF
159100970117     C*--------------------
159200970414     C* INGLOBA AL PORTO
159300970117     C*--------------------
159400970117IF  1C     V5CIAP        IFEQ      'S'
159500970117     C     V5CIAP        OREQ      'N'
159600970117X   1C                   ELSE
159700970117     C                   SETON                                        3790
159800970117     C                   MOVEL     MSG(17)       $MSG
159900970117     C                   GOTO      FINCT5
160000970117E   1C                   ENDIF
160100970117     C*
160200970117IF  1C     V5CPCA        IFEQ      *ZEROS
160300970117     C     V5CIAP        ANDEQ     'S'
160400970117     C                   SETON                                        363790
160500970117     C                   MOVEL     MSG(18)       $MSG
160600970117     C                   GOTO      FINCT5
160700970117E   1C                   ENDIF
160800970117     C*--------------------
160900970414     C* % AI PIANI
161000970117     C*--------------------
161100970117IF  1C     V5CPCP        IFGT      100
161200970117     C                   SETON                                        3890
161300970117     C                   MOVEL     MSG(27)       $MSG
161400970117     C                   GOTO      FINCT5
161500970117E   1C                   ENDIF
161600970414     C*--------------------
161700970414     C* INGLOBA AL PORTO
161800970414     C*--------------------
161900970117IF  1C     V5CIPI        IFEQ      'S'
162000970117     C     V5CIPI        OREQ      'N'
162100970117X   1C                   ELSE
162200970117     C                   SETON                                        3990
162300970117     C                   MOVEL     MSG(17)       $MSG
162400970117     C                   GOTO      FINCT5
162500970117E   1C                   ENDIF
162600970117     C*
162700970117IF  1C     V5CPCP        IFEQ      *ZEROS
162800970117     C     V5CIPI        ANDEQ     'S'
162900970117     C                   SETON                                        383990
163000970117     C                   MOVEL     MSG(18)       $MSG
163100970117     C                   GOTO      FINCT5
163200970117E   1C                   ENDIF
163300970117     C*--------------------
163400970414     C* % AI SUPERMERCATI
163500970117     C*--------------------
163600970117IF  1C     V5CPCS        IFGT      100
163700970117     C                   SETON                                        4090
163800970117     C                   MOVEL     MSG(27)       $MSG
163900970117     C                   GOTO      FINCT5
164000131121     c                   else
164100131121     c     v5cpca        add       v5cpcs        wn7
164200131121IF  1C     WN7           IFGT      100
164300131121     C                   SETON                                        4090
164400131121     C                   MOVEL     MSG(83)       $MSG
164500131121     C                   GOTO      FINCT5
164600131121E   1C                   ENDIF
164700970117E   1C                   ENDIF
164800970414     C*--------------------
164900970414     C* INGLOBA AL PORTO
165000970414     C*--------------------
165100970117IF  1C     V5CISU        IFEQ      'S'
165200970117     C     V5CISU        OREQ      'N'
165300970117X   1C                   ELSE
165400970117     C                   SETON                                        4190
165500970117     C                   MOVEL     MSG(17)       $MSG
165600970117     C                   GOTO      FINCT5
165700970117E   1C                   ENDIF
165800970117     C*
165900970117IF  1C     V5CPCS        IFEQ      *ZEROS
166000970117     C     V5CISU        ANDEQ     'S'
166100970117     C                   SETON                                        404190
166200970117     C                   MOVEL     MSG(18)       $MSG
166300970117     C                   GOTO      FINCT5
166400970117E   1C                   ENDIF
166500970117     C*--------------------
166600970414     C* % FUORI MISURA
166700970117     C*--------------------
166800970117IF  1C     V5CPCF        IFGT      100
166900970117     C                   SETON                                        4290
167000970117     C                   MOVEL     MSG(27)       $MSG
167100970117     C                   GOTO      FINCT5
167200970117E   1C                   ENDIF
167300970414     C*--------------------
167400970414     C* INGLOBA AL PORTO
167500970414     C*--------------------
167600970117IF  1C     V5CIFU        IFEQ      'S'
167700970117     C     V5CIFU        OREQ      'N'
167800970117X   1C                   ELSE
167900970117     C                   SETON                                        4390
168000970117     C                   MOVEL     MSG(17)       $MSG
168100970117     C                   GOTO      FINCT5
168200970117E   1C                   ENDIF
168300970117     C*
168400970117IF  1C     V5CPCF        IFEQ      *ZEROS
168500970117     C     V5CIFU        ANDEQ     'S'
168600970117     C                   SETON                                        424390
168700970117     C                   MOVEL     MSG(18)       $MSG
168800970117     C                   GOTO      FINCT5
168900970117E   1C                   ENDIF
169000970414     C*--------------------
169100970414     C* % DDT
169200970414     C*--------------------
169300970414IF  1C     V5CPCD        IFGT      100
169400970414     C                   SETON                                        6890
169500970414     C                   MOVEL     MSG(27)       $MSG
169600970414     C                   GOTO      FINCT5
169700970414E   1C                   ENDIF
169800970414     C*--------------------
169900970414     C* INGLOBA AL PORTO
170000970414     C*--------------------
170100970414IF  1C     V5CIDD        IFEQ      'S'
170200970414     C     V5CIDD        OREQ      'N'
170300970414X   1C                   ELSE
170400970414     C                   SETON                                        6990
170500970414     C                   MOVEL     MSG(17)       $MSG
170600970414     C                   GOTO      FINCT5
170700970414E   1C                   ENDIF
170800970414     C*
170900970414IF  1C     V5CPCD        IFEQ      *ZEROS
171000970414     C     V5CIDD        ANDEQ     'S'
171100970414     C                   SETON                                        686990
171200970414     C                   MOVEL     MSG(18)       $MSG
171300970414     C                   GOTO      FINCT5
171400970414E   1C                   ENDIF
171500970414     C*--------------------
171600970414     C* % CENTRI STORICI
171700970414     C*--------------------
171800970414IF  1C     V5CPCC        IFGT      100
171900970414     C                   SETON                                        7090
172000970414     C                   MOVEL     MSG(27)       $MSG
172100970414     C                   GOTO      FINCT5
172200970414E   1C                   ENDIF
172300970414     C*--------------------
172400970414     C* INGLOBA AL PORTO
172500970414     C*--------------------
172600970414IF  1C     V5CICE        IFEQ      'S'
172700970414     C     V5CICE        OREQ      'N'
172800970414X   1C                   ELSE
172900970414     C                   SETON                                        7190
173000970414     C                   MOVEL     MSG(17)       $MSG
173100970414     C                   GOTO      FINCT5
173200970414E   1C                   ENDIF
173300970414     C*
173400970414IF  1C     V5CPCC        IFEQ      *ZEROS
173500970414     C     V5CICE        ANDEQ     'S'
173600970414     C                   SETON                                        707190
173700970414     C                   MOVEL     MSG(18)       $MSG
173800970414     C                   GOTO      FINCT5
173900970414E   1C                   ENDIF
174000970414     C*--------------------
174100970414     C* % LOCALITA' DISAGIATE
174200970414     C*--------------------
174300970414IF  1C     V5CPCL        IFGT      100
174400970414     C                   SETON                                        7290
174500970414     C                   MOVEL     MSG(27)       $MSG
174600970414     C                   GOTO      FINCT5
174700970414E   1C                   ENDIF
174800970414     C*--------------------
174900970414     C* INGLOBA AL PORTO
175000970414     C*--------------------
175100970414IF  1C     V5CILO        IFEQ      'S'
175200970414     C     V5CILO        OREQ      'N'
175300970414X   1C                   ELSE
175400970414     C                   SETON                                        7390
175500970414     C                   MOVEL     MSG(17)       $MSG
175600970414     C                   GOTO      FINCT5
175700970414E   1C                   ENDIF
175800970414     C*
175900970414IF  1C     V5CPCL        IFEQ      *ZEROS
176000970414     C     V5CILO        ANDEQ     'S'
176100970414     C                   SETON                                        727390
176200970414     C                   MOVEL     MSG(18)       $MSG
176300970414     C                   GOTO      FINCT5
176400970414E   1C                   ENDIF
176500011009     C*--------------------
176600011009     C* % CONS.  ISOLE
176700011009     C*--------------------
176800011009IF  1C     V5CCIS        IFGT      100
176900011009     C                   SETON                                        7790
177000011009     C                   MOVEL     MSG(27)       $MSG
177100011009     C                   GOTO      FINCT5
177200131120     c                   else
177300131120     C* FERMO DEPOSITO + CONSEGNA ISOLA DEVE ESSERE <= 100
177400131120IF  1C     V2CPFD        ADD       V5CCIS        WN7
177500131120IF  1C     WN7           IFGT      100
177600131121     C                   SETON                                        7790
177700131120     C                   MOVEL     MSG(81)       $MSG
177800131121     C                   GOTO      FINCT5
177900011009E   1C                   ENDIF
178000131121E   1C                   ENDIF
178100011009     C*--------------------
178200011009     C* INGLOBA AL PORTO
178300011009     C*--------------------
178400011009IF  1C     V5CISO        IFEQ      'S'
178500011009     C     V5CISO        OREQ      'N'
178600011009X   1C                   ELSE
178700011009     C                   SETON                                        7890
178800011009     C                   MOVEL     MSG(17)       $MSG
178900011009     C                   GOTO      FINCT5
179000011009E   1C                   ENDIF
179100011009     C*
179200011009IF  1C     V5CCIS        IFEQ      *ZEROS
179300011009     C     V5CISO        ANDEQ     'S'
179400011009     C                   SETON                                        777890
179500011009     C                   MOVEL     MSG(18)       $MSG
179600011009     C                   GOTO      FINCT5
179700011009E   1C                   ENDIF
179800011009     C*--------------------
179900011009     C* % RITIRI ISOLE
180000011009     C*--------------------
180100011031     C* Se ricbiesto il ritiro isola, controlla se il codice tassaizione del cliente della tariffa /
180200011031     c* offerta sia un "POSSIBILE ISOLA" (da tabella)
180300011009IF  1C     V5CSAM        IFGT      100
180400011009     C                   SETON                                        7990
180500011009     C                   MOVEL     MSG(27)       $MSG
180600011009     C                   GOTO      FINCT5
180700011031X   1C                   ELSE
180800011031IF  2C     V5CSAM        IFGT      *ZEROS
180900020923     C                   Z-ADD     1             KTBKUT
181000011030     C                   MOVEL     'CT'          KTBCOD
181100011030     C                   MOVEL(P)  WRKCTS        KTBKEY
181200011030     C     KEYTAB        CHAIN     TABEL00F                           99
181300011031     C                   MOVEL     TBLUNI        DSCT
181400011031IF  3c     WRKCTS        IFEQ      *BLANKS                                      *CLIENTE POTENZIALE
181500011031     C     *IN99         OREQ      *ON                                          *NON TROVATO
181600011031     C     TBLFLG        ORNE      *BLANKS                                      *ANULLATO
181700011031     C     §CT§§5        ORNE      'S'                                          *CODICE NON ISOLA
181800011030     C                   SETON                                        7990
181900011030     C                   MOVEL     MSG(56)       $MSG
182000011030     C                   GOTO      FINCT5
182100011031E   3C                   ENDIF
182200131121     c     v5csam        add       v2cpcm        wn7
182300131121IF  1C     WN7           IFGT      100
182400131121     C                   SETON                                        7990
182500131121     C                   MOVEL     MSG(82)       $MSG
182600131121     C                   GOTO      FINCT5
182700131121E   1C                   ENDIF
182800011031E   2C                   ENDIF
182900011009E   1C                   ENDIF
183000011009     C*--------------------
183100011009     C* INGLOBA AL PORTO
183200011009     C*--------------------
183300011009IF  1C     V5CINR        IFEQ      'S'
183400011009     C     V5CINR        OREQ      'N'
183500011009X   1C                   ELSE
183600011009     C                   SETON                                        8090
183700011009     C                   MOVEL     MSG(54)       $MSG
183800011009     C                   GOTO      FINCT5
183900011009E   1C                   ENDIF
184000011009     C*
184100011009IF  1C     V5CSAM        IFEQ      *ZEROS
184200011009     C     V5CINR        ANDEQ     'S'
184300011009     C                   SETON                                        798090
184400011009     C                   MOVEL     MSG(18)       $MSG
184500011009     C                   GOTO      FINCT5
184600011009E   1C                   ENDIF
184700040802     C*--------------------
184800040802     C* % sped destinatario disagiato
184900040802     C*--------------------
185000040802IF  1C     V5Cdes        IFGT      100
185100040802     C                   SETON                                        1490
185200040802     C                   MOVEL     MSG(27)       $MSG
185300040802     C                   GOTO      FINCT5
185400040802E   1C                   ENDIF
185500050805     C*--------------------
185600050805     C* INGLOBA AL PORTO
185700050805     C*--------------------
185800050805IF  1C     v5cide        IFEQ      'S'
185900050805     C     v5cide        OREQ      'N'
186000050805X   1C                   ELSE
186100050805     C                   SETON                                        8690
186200050805     C                   MOVEL     MSG(72)       $MSG
186300050805     C                   GOTO      FINCT5
186400050805E   1C                   ENDIF
186500050805     C*
186600050805IF  1C     v5cdes        IFEQ      *ZEROS
186700050805     C     v5cide        ANDEQ     'S'
186800050805     C                   SETON                                        148690
186900050805     C                   MOVEL     MSG(78)       $MSG
187000050805     C                   GOTO      FINCT5
187100050805E   1C                   ENDIF
187200151119     C*--------------------
187300151119     C* packing list
187400151119     C*--------------------
187500151119IF  1C     V5Cpkl        IFne      ' '
187600151119IF  1C     V5Cpkl        andne     'S'
187700151119     C                   SETON                                        7090
187800151119     C                   MOVEL     MSG(75)       $MSG
187900151119     C                   GOTO      FINCT5
188000151119E   1C                   ENDIF
188100151119IF  1C     V5Cpkl        IFeq      'S'
188200151119IF  1C     DTCV1C        andge     20160101
188300151119IF  1C     V5Cipk        IFne      'S'
188400151119IF  1C     V5Cipk        andne     'N'
188500151119     C                   SETON                                        7190
188600151119     C                   MOVEL     MSG(86)       $MSG
188700151119     C                   GOTO      FINCT5
188800151119E   1C                   ENDIF
188900151119E   1C                   ENDIF
189000970117     C*
189100970117     C     FINCT5        ENDSR
189200970117     C*------------------------------------------------------------------------*
189300970117     C* F03GES - GESTIONE TASTO FUNZIONALE F03 ==> FINE
189400970117     C*------------------------------------------------------------------------*
189500970117     C     F03GES        BEGSR
189600970117     C*
189700970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
189800970117     C                   MOVEL     '1'           D00F03                         *F03 IN PROCEDURA
189900970117     C*
190000970117     C                   ENDSR
190100970117     C*------------------------------------------------------------------------*
190200970117     C* F12GES - GESTIONE TASTO FUNZIONALE F12 ==> RITORNO
190300970117     C*------------------------------------------------------------------------*
190400970117     C     F12GES        BEGSR
190500970117     C*
190600970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
190700970117     C                   MOVEL     '1'           D00F12                         *F12 IN PROCEDURA
190800970117     C*
190900970117     C                   ENDSR
191000970117     C*------------------------------------------------------------------------*
191100970117     C* F05GES - GESTIONE TASTO FUNZIONALE F05 ==> CARICA DA ALTRA TARIFFA
191200970117     C*------------------------------------------------------------------------*
191300970117     C     F05GES        BEGSR
191400970117     C*
191500970117     C* GESTIONE VIDETA
191600970117     C                   EXSR      IMPWD2
191700970117DO  1C     $FINV         DOUEQ     'S'
191800970117     C                   EXSR      GESWD2                                       *GESTIONE FINESTRA
191900970117E   1C                   ENDDO
192000970117     C*
192100970117     C* CARICA A VIDEO I DATI TIPO DELLA TARIFFA RICHIESTA
192200970117IF  1C     $CONF         IFEQ      'S'                                          *SE CONFERMATO
192300970117     C                   EXSR      MEMETC                                       *MEMORIZZA
192400970117     C                   EXSR      IMPREC                                       *CARICA
192500970117E   1C                   ENDIF
192600970117     C*
192700970117     C                   ENDSR
192800970117     C*------------------------------------------------------------------------*
192900970117     C* IMPWD2 - IMPOSTAZIONI SECONDA FINESTRA
193000970117     C*------------------------------------------------------------------------*
193100970117     C     IMPWD2        BEGSR
193200970117     C*
193300091103     c                   if        d00dsf = ' '
193400091103     c                   eval      d2wcto = 'Tariffa/Offerta ............:'
193500091103     c                   eval      s2wcto = '(T/O)'
193600091103     c                   eval      d2wksc = 'Cliente o Visita ...........:'
193700091120     c                   eval      d2wprg = 'Progressivo ................:'
193800091103     c                   eval      d2wtesta = 'Caricamento dati tipo da altra -
193900091103     c                             Tariffa/Offerta'
194000091103     c                   else
194100091201     c                   eval      d2wcto = 'Tariffa/Offerta ............:'
194200091130     c                   eval      s2wcto = '(T/O)'
194300091103     c                   eval      d2wksc = 'Cliente o Trattativa .......:'
194400091120     c                   eval      d2wprg = 'Versione ...................:'
194500091103     c                   eval      d2wtesta = 'Caricamento dati tipo da altra -
194600091103     c                             Tariffa/Trattativa'
194700091103     c                   end
194800970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI
194900970117     C                   MOVEL     'N'           $FINV                          *FINE FINESTRA
195000970117     C                   MOVEL     'N'           $CONF                          *CONFERMA CARICAMENT
195100980429     C                   MOVEL     D0DCTO        V2WCTO
195200970319     C                   Z-ADD     D0DKSC        V2WKSC
195300970319     C                   Z-ADD     D0DCTR        V2WCTR
195400970319     C                   Z-ADD     D0DPRG        V2WPRG
195500970319     C                   Z-ADD     D0DLNP        V2WLNP
195600011003     C                   MOVEL     D0DDIV        V2WDIV
195700970117     C                   EXSR      SETWD2
195800970117     C*
195900970117     C                   ENDSR
196000970117     C*------------------------------------------------------------------------*
196100970117     C* SETWD2 - IMPOSTAZIONI INDICATORI SECONDA FINESTRA
196200970117     C*------------------------------------------------------------------------*
196300970117     C     SETWD2        BEGSR
196400970117     C*
196500970117     C                   SETOFF                                       616263     ERRORI
196600980429     C                   SETOFF                                       646574
196700011003     C                   SETOFF                                       76
196800970117     C                   SETOFF                                       90         GENERICO DI ERRORE
196900970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
197000970117     C*
197100970117     C                   ENDSR
197200970117     C*------------------------------------------------------------------------*
197300970117     C* GESWD2 - GESTIONE SECONDA FINESTRA
197400970117     C*------------------------------------------------------------------------*
197500970117     C     GESWD2        BEGSR
197600970117     C*
197700970117     C* EMISSIONE VIDEATA
197800970117     C                   EXFMT     TE01W02
197900970117     C                   EXSR      SETWD2
198000970117     C* F12-RITORNO
198100970117IF  1C     *INKL         IFEQ      *ON
198200970117     C                   MOVEL     'N'           $CONF                          *CONFERMA NO
198300970117     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
198400970117     C                   GOTO      FINWD2
198500970117E   1C                   ENDIF
198600980430     C* F11-RICERCA
198700980430IF  1C     *INKK         IFEQ      *ON
198800980430     C                   EXSR      GESRIC
198900060825     C*                  GOTO      FINWD2
199000980430E   1C                   ENDIF
199100970117     C* CONTROLLA VIDEO
199200970117     C                   EXSR      CTRWD2
199300970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
199400970117     C                   GOTO      FINWD2
199500970117E   1C                   ENDIF
199600970117     C* F06-CONFERMA
199700970117IF  1C     *INKF         IFEQ      *ON
199800970117     C                   MOVEL     'S'           $CONF                          *CONFERMA SI
199900970117     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
200000970117     C                   GOTO      FINWD2
200100970117E   1C                   ENDIF
200200970117     C*
200300970117     C     FINWD2        ENDSR
200400980430     C*------------------------------------------------------------------------*
200500980430     C* GESRIC - GESTIONE RICERCA DATI TIPO
200600980430     C*------------------------------------------------------------------------*
200700980430     C     GESRIC        BEGSR
200800980430     C*
200900980430     C                   CLEAR                   DSTE30
201000980430     C                   MOVEL     'C01'         D30OP0
201100980708     C                   MOVEL     'C'           D30ETX
201200980804     C                   Z-ADD     D00KSC        D30KSC
201300011011     C                   MOVEL     D00DIV        D30DIV
201400091123     C                   MOVEL     D00Dsf        D30dsf
201500980430     C                   CALL      'TNTE32R'
201600980430     C                   PARM                    KPJBA
201700980430     C                   PARM                    DSTE30
201800980430IF  1C     D30ERR        IFEQ      '1'                                          *ERRORE
201900980430     C                   SETON                                        90
202000980504     C                   MOVEL     D30ERR        $MSG                           *COMUNQUE MESSAGGIO
202100980430X   1C                   ELSE                                                   *NO ERRORE
202200980430IF  2C     D30F03        IFEQ      '1'                                          *NON SCELTA
202300980430     C     D30F12        OREQ      '1'
202400980430X   2C                   ELSE                                                   *SCELTA
202500980430     C                   MOVEL     'S'           $CONF                          *CONFERMA SI
202600980430     C                   MOVEL     D30CTO        V2WCTO
202700980430     C                   Z-ADD     D30KSC        V2WKSC
202800980430     C                   Z-ADD     D30CTR        V2WCTR
202900980430     C                   Z-ADD     D30PRG        V2WPRG
203000980430     C                   Z-ADD     D30LNP        V2WLNP
203100011003     C                   MOVEL     D30DIV        V2WDIV
203200980430E   2C                   ENDIF
203300980430E   1C                   ENDIF
203400980430     C                   ENDSR
203500970117     C*------------------------------------------------------------------------*
203600970117     C* CTRWD2 - CONTROLLA SECONDA FINESTRA
203700970117     C*------------------------------------------------------------------------*
203800970117     C     CTRWD2        BEGSR
203900970117     C*--------------------
204000091103     C* TARIFFA / OFFERTA  / TRATTATIVA
204100970117     C*--------------------
204200091103     c                   if        (d00dsf = ' '
204300091103IF  1C                             and v2wcto <> 'T' and
204400091103IF  1C                             v2wcto <> 'O') or
204500091103     c                             (d00dsf = 'X'
204600091103IF  1C                             and v2wcto <> 'T' and
204700091130IF  1C                             v2wcto <> 'O')
204800091103     C                   SETON                                        90
204900091103     C                   MOVEL     MSG(4)        $MSG
205000091103     C                   GOTO      FINCW2
205100091103E   1C                   ENDIF
205200011026     C*
205300011026     C* IMPOSTAZIONE APPLICAZIONE TARIFFA FINITA IN DS
205400011026     C                   Z-ADD     V2WKSC        KTMKSC                         *CLIENTE
205500011026     C                   Z-ADD     V2WCTR        KTMCTR                         *TARIFFA
205600011026     C                   Z-ADD     V2WPRG        KTMPRG                         *PROGRESSIVO
205700020715     C*
205800020715     C* SE SONO IN MANUTENZIONE OFFERTE INVERTO I CRITERI DI LETTURA RISPETTO ALLA SCELTA INDICATA
205900020715     C* A VIDEO X GESTIRE LE OVRDBF PRESENTI
206000091103     c                   if        %subst(d00op0: 1: 1)='O'
206100020715     C                   IF        V2WCTO = 'O'
206200020715     C     KEYTAM        CHAIN     TNTAM01L                           84
206300020715     C                   ELSE
206400020715     C     KEYTAM        CHAIN     TNOFM01L                           84
206500020715     C                   ENDIF
206600020715     C                   ELSE
206700020715     C                   IF        V2WCTO = 'T'
206800011210     C     KEYTAM        CHAIN     TNTAM01L                           84
206900011210     C                   ELSE
207000011210     C     KEYTAM        CHAIN     TNOFM01L                           84
207100011210     C                   ENDIF
207200020715     C                   ENDIF
207300011210     C                   IF        not *in84
207400011026     C                   MOVEL     TAMFLO        DSTA01
207500011026     C                   MOVEL     §TADIV        V2WDIV
207600011210     C                   ELSE
207700011210     C                   SETON                                        616263
207800011210     C                   SETON                                        646574
207900011210     C                   SETON                                        76
208000011210     C                   SETON                                        90
208100011210     C                   MOVEL     MSG(59)       $MSG
208200011210     C                   GOTO      FINCW2
208300011026E   2C                   ENDIF
208400970117     C*--------------------
208500970117     C* LINEA DI PARTENZA
208600970117     C*--------------------
208700970117     C                   MOVEL     V2WLNP        WLNP
208800970117     C                   MOVEL     *BLANKS       WDLNP
208900970117     C                   MOVEL     '0'           WERR
209000970117     C                   EXSR      CHKLNP
209100980818IF  1C     WERR          IFEQ      '1'
209200980818IF  2C     WLNP          IFEQ      888
209300980818     C     WLNP          OREQ      889
209400980818     C                   MOVEL     '0'           WERR
209500980818     C                   MOVEL     DDEC(10)      WDLNP
209600980818X   2C                   ELSE
209700970117     C                   SETON                                        6590
209800970117     C                   MOVEL     MSG(3)        $MSG
209900970117     C                   GOTO      FINCW2
210000980818E   2C                   ENDIF
210100980818E   1C                   ENDIF
210200970117     C*--------------------
210300970117     C* DATI TIPO ESISTENTI
210400970117     C*--------------------
210500091120     C                   MOVEL     'S'           KTCDSF                         *DATI SEDE/FILIALE
210600970117     C                   MOVEL     V2WCTO        KTCCTO                         *TARIFFA O OFFERTA
210700970117     C                   Z-ADD     V2WKSC        KTCKSC                         *CLIENTE P VISITA
210800970117     C                   Z-ADD     V2WCTR        KTCCTR                         *TARIFFA
210900970117     C                   Z-ADD     V2WPRG        KTCPRG                         *PROGRESSIVO
211000970117     C                   Z-ADD     V2WLNP        KTCLNP                         *LINEA DI PARTENZA
211100011001     C     KEYETC        SETLL     TEETC01L                               99
211200970117IF  1C     *IN99         IFEQ      *OFF                                         *NON TROVATO
211300970117     C                   SETON                                        616263
211400980429     C                   SETON                                        646574
211500011003     C                   SETON                                        76
211600980429     C                   SETON                                        90
211700970117     C                   MOVEL     MSG(9)        $MSG
211800970117     C                   GOTO      FINCW2
211900970117E   1C                   ENDIF
212000970117     C*
212100970319     C* DEPOSITA NEI CAMPI DELLA DS LA TARIFFA RICHIESTA IN COPIA
212200970319IF  1C     WERR          IFEQ      '0'                                          *NO ERRORE
212300091120     C                   MOVEL     'S'           D0DDSF
212400980429     C                   MOVEL     V2WCTO        D0DCTO
212500970319     C                   Z-ADD     V2WKSC        D0DKSC
212600970319     C                   Z-ADD     V2WCTR        D0DCTR
212700970319     C                   Z-ADD     V2WPRG        D0DPRG
212800970319     C                   Z-ADD     V2WLNP        D0DLNP
212900011003     C                   MOVEL     V2WDIV        D0DDIV
213000970319E   1C                   ENDIF
213100970319     C*
213200970117     C     FINCW2        ENDSR
213300970117     C*------------------------------------------------------------------------*
213400970117     C* MEMETC - MEMORIZZA I DATI TIPO GENERALI DELLA TARIFFA RICHIESTA
213500970117     C*------------------------------------------------------------------------*
213600970117     C     MEMETC        BEGSR
213700970117     C*
213800970117     C* LEGGE I DATI TIPO DELLA TARIFFA RICHIESTA
213900091120     C                   MOVEL     'S'           KTCDSF                         *DATI SEDE/FILIALE
214000970117     C                   MOVEL     V2WCTO        KTCCTO                         *TARIFFA O OFFERTA
214100970117     C                   Z-ADD     V2WKSC        KTCKSC                         *CLIENTE O VISITA
214200970117     C                   Z-ADD     V2WCTR        KTCCTR                         *TARIFFA
214300970117     C                   Z-ADD     V2WPRG        KTCPRG                         *PROGRESSIVO
214400970117     C                   Z-ADD     V2WLNP        KTCLNP                         *LINEA DI PARTENZA
214500011001     C     KEYETC        CHAIN(N)  TEETC01L                           99
214600970117     C*
214700980430     C                   ENDSR
214800970117     C*------------------------------------------------------------------------*
214900970117     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> AGGIORNA E PROSEGUE
215000970117     C*------------------------------------------------------------------------*
215100970117     C     F06GES        BEGSR
215200970725     C*
215300970725     C* IMPOSTA VARIABILI DI LAVORO
215400970725     C                   MOVE      '1'           WTUR                           *N°GIRO SCRITTURA TA
215500970725     C*
215600970117     C* AGGIORNA L'ARCHIVIO
215700970117     C                   MOVEL     '0'           WERR
215800970117IF  1C     $TGES         IFEQ      'G'                                          *SE GESTIONE
215900970117     C                   EXSR      AGGETC                                       *AGGIORNA
216000970117E   1C                   ENDIF
216100970117IF  1C     WERR          IFEQ      '0'                                          *NO ERRORI
216200011001     C                   MOVEL     'S'           D00ETC                         *S=TEETC CONTROLLATO
216300970117     C*---
216400970117     C* LANCIA LA GESTIONE DEGLI SCAGLIONI
216500970117     C*---
216600970117     C                   EXSR      IMPLAN                                       *IMPOSTA CAMPI
216700011001     C                   MOVEL     'N'           D00ETS                         *S=TEETS CONTROLLATO
216800970117     C                   MOVEL     'TNTE02R'     LPGM              8            *PGM DA LANCIARE
216900970117     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
217000970130IF  2C     D00ETS        IFEQ      'S'                                          *LANCIO OK
217100060830     c* 92 on ho gli scaglioni anche per le competenze
217200061026     c                   if        etcbnb <> 0
217300060830     c     keyetc        setll     teets11l                               92
217400061030     c                   else
217500061030     c                   setoff                                       92
217600061026     c                   end
217700970117     C*---
217800970117     C* LANCIA LA SCRITTURA DEI CODICI TASSAZIONE DAL DETTAGLIO TARIFFA
217900970117     C*---
218000970130IF  3C     D00TLA        IFEQ      'D'                                          D E L T A
218100970117     C                   EXSR      IMPLAN                                       *IMPOSTA CAMPI
218200060830     C                   MOVEL     'TNTE09R'     LPGM                           *PGM DA LANCIARE
218300970117     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
218400970130E   3C                   ENDIF
218500970117     C*---
218600970117     C* LANCIA LA GESTIONE DEI CODICI TASSAZIONE
218700970117     C*---
218800970130IF  3C     D00ERR        IFEQ      '0'                                          *LANCIO OK
218900970117     C                   EXSR      IMPLAN                                       *IMPOSTA CAMPI
219000011001     C                   MOVEL     'N'           D00ETT                         *S=TEETT CONTROLLATO
219100060830     C                   MOVEL     'TNTE03R'     LPGM                           *PGM DA LANCIARE
219200970117     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
219300970317IF  4C     D00ETT        IFEQ      'S'                                          *LANCIO OK
219400970317     C***
219500970317     C* D00TLA='P' --> CALCOLO PORTO
219600970317     C***
219700970317IF  5C     D00TLA        IFEQ      'P'
219800970117     C*---
219900970117     C* LANCIA IL CALCOLO DELLE COMPETENZE
220000970117     C*---
220100970313     C                   EXSR      IMPLAN                                       *IMPOSTA CAMPI
220200060830     c* scaglioni competenze
220300060830     c                   if        *in92
220400060830     C                   MOVEL     'TNTE10C'     LPGM                           *PGM DA LANCIARE
220500060830     C                   MOVE      'TNTE10R   '  kpjbu                          *PGM DA LANCIARE
220600060830     c                   else
220700060830     c* scaglioni ricavi
220800060830     C                   MOVEL     'TNTE10R'     LPGM                           *PGM DA LANCIARE
220900060830     c                   end
221000970313     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
221100970317IF  6C     D00ERR        IFEQ      '0'                                          *LANCIO OK
221200970117     C*---
221300970117     C* LANCIA LA SCRITTURA DEL DETTAGLIO TARIFFA
221400970117     C*---
221500970117     C                   EXSR      IMPLAN                                       *IMPOSTA CAMPI
221600060830     c* scaglioni competenze
221700060830     c                   if        *in92
221800060830     C                   MOVEL     'TNTE10C'     LPGM                           *PGM DA LANCIARE
221900060830     C                   MOVE      'TNTE15R   '  kpjbu                          *PGM DA LANCIARE
222000060830     c                   else
222100060830     c* scaglioni ricavi
222200060830     C                   MOVEL     'TNTE15R'     LPGM                           *PGM DA LANCIARE
222300060830     c                   end
222400970117     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
222500970506IF  7C     D00ERR        IFEQ      '0'                                          *LANCIO OK
222600970506     C*---
222700970506     C* LANCIA IL CALCOLO DEI RICAVI
222800970506     C*---
222900970506     C                   EXSR      IMPLAN                                       *IMPOSTA CAMPI
223000060830     C                   MOVEL     'TNTE11C'     LPGM                           *PGM DA LANCIARE
223100970506     C                   EXSR      LANPGM
223200970619IF  8C     D00ERR        IFEQ      '0'                                          *LANCIO OK
223300970506     C*---
223400970506     C* LANCIA LA RISCRITTURA DELLE COMPETENZE IN BASE AL DELTA RICHIESTO
223500970506     C*---
223600970506     C                   EXSR      IMPLAN                                       *IMPOSTA CAMPI
223700060830     c* scaglioni competenze
223800060830     c                   if        *in92
223900060830     C                   MOVEL     'TNTE10C'     LPGM                           *PGM DA LANCIARE
224000060830     C                   MOVE      'TNTE16R   '  kpjbu                          *PGM DA LANCIARE
224100060830     c                   else
224200060830     c* scaglioni ricavi
224300060830     C                   MOVEL     'TNTE16R'     LPGM                           *PGM DA LANCIARE
224400060830     c                   end
224500970506     C                   EXSR      LANPGM
224600970506IF  9C     D00ERR        IFEQ      '0'                                          *LANCIO OK
224700970506     C*---
224800970506     C* LANCIA LA SCRITTURA DEL DETTAGLIO TARIFFA
224900970506     C*---
225000970725     C                   MOVE      '2'           WTUR                           *N°GIRO SCRITTURA TA
225100970506     C                   EXSR      IMPLAN                                       *IMPOSTA CAMPI
225200060830     c* scaglioni competenze
225300060830     c                   if        *in92
225400060830     C                   MOVEL     'TNTE10C'     LPGM                           *PGM DA LANCIARE
225500060830     C                   MOVE      'TNTE15R   '  kpjbu                          *PGM DA LANCIARE
225600060830     c                   else
225700060830     c* scaglioni ricavi
225800060830     C                   MOVEL     'TNTE15R'     LPGM                           *PGM DA LANCIARE
225900060830     c                   end
226000970506     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
226100970506     C                   MOVEL     'S'           $FINE                          *FINE PROCEDURA
226200970506E   9C                   ENDIF
226300970506E   8C                   ENDIF
226400970506E   7C                   ENDIF
226500970506E   6C                   ENDIF
226600970317E   5C                   ENDIF
226700970117     C***
226800970117     C* D00TLA='D' --> CALCOLO DELTA
226900970117     C***
227000970317IF  5C     D00TLA        IFEQ      'D'
227100970117     C*---
227200970117     C* LANCIA IL CALCOLO DEI RICAVI
227300970117     C*---
227400970306     C                   EXSR      IMPLAN                                       *IMPOSTA CAMPI
227500060830     C                   MOVEL     'TNTE11C'     LPGM                           *PGM DA LANCIARE
227600970306     C                   EXSR      LANPGM
227700970317IF  6C     D00ERR        IFEQ      '0'                                          *LANCIO OK
227800970317     C*---
227900970317     C* LANCIA IL CALCOLO DELLE COMPETENZE
228000970317     C*---
228100970317     C                   EXSR      IMPLAN                                       *IMPOSTA CAMPI
228200060830     c* scaglioni competenze
228300060830     c                   if        *in92
228400060830     C                   MOVEL     'TNTE10C'     LPGM                           *PGM DA LANCIARE
228500060830     C                   MOVE      'TNTE10R   '  kpjbu                          *PGM DA LANCIARE
228600060830     c                   else
228700060830     c* scaglioni ricavi
228800060830     C                   MOVEL     'TNTE10R'     LPGM                           *PGM DA LANCIARE
228900060830     c                   end
229000970317     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
229100970317IF  7C     D00ERR        IFEQ      '0'                                          *LANCIO OK
229200970317     C*---
229300970317     C* LANCIA LA VISUALIZZAZIONE DEL DELTA
229400970317     C*---
229500970117     C                   EXSR      IMPLAN                                       *IMPOSTA CAMPI
229600060830     C                   MOVEL     'TNTE20R'     LPGM                           *PGM DA LANCIARE
229700970117     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
229800970317E   7C                   ENDIF
229900970117E   6C                   ENDIF
230000970117E   5C                   ENDIF
230100970117E   4C                   ENDIF
230200970117E   3C                   ENDIF
230300970117E   2C                   ENDIF
230400970117E   1C                   ENDIF
230500970117     C*
230600970117     C                   ENDSR
230700970117     C*------------------------------------------------------------------------*
230800011001     C* AGGETC - AGGIORNAMENTO ARCHIVIO -TEETC-
230900970117     C*------------------------------------------------------------------------*
231000970117     C     AGGETC        BEGSR
231100970117     C*
231200970117     C* RILEGGE IL RECORD DA AGGIORNARE
231300091120     C                   MOVEL     'S'           KTCDSF
231400970117     C                   MOVEL     D00CTO        KTCCTO
231500970117     C                   Z-ADD     V0CKSC        KTCKSC
231600970117     C                   Z-ADD     V0CCTR        KTCCTR
231700970117     C                   Z-ADD     V0CPRG        KTCPRG
231800970117     C                   Z-ADD     V1CLNP        KTCLNP
231900011001     C     KEYETC        CHAIN     TEETC01L                           9998
232000970117IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
232100970117     C                   SETON                                        90
232200970117     C                   MOVEL     MSG(70)       $MSG
232300970117     C                   MOVEL     '1'           WERR
232400970117X   1C                   ELSE                                                   *NON ALLOCATO
232500970117     C* INSERIMENTO
232600970117IF  2C     *IN99         IFEQ      *ON
232700970117     C                   EXSR      IMPETC                                       *IMPOSTA I CAMPI
232800011001     C                   WRITE     TEETC000
232900970117     C                   MOVEL     DOPE(2)       V0COPE                         *MANUTENZ.IN TESTATA
233000970117     C* VARIAZIONE
233100970117X   2C                   ELSE
233200150305     c                   MOVE      v4ctba        comtba
233300980504IF  3C     V2CDCV        IFNE      ETCDCV                                       *SE MODIFICATO QC.
233400980504     C     V2CFAR        ORNE      ETCFAR
233500970314     C     V2CIAR        ORNE      ETCIAR
233600970314     C     V2CCLV        ORNE      ETCCLV
233700970117     C     V2CBOL        ORNE      ETCBOL
233800021125     C     V2CDKC        ORNE      ETCDKC
233900021125     C     V2CIX1        ORNE      ETCIX1
234000021125     C     V2CIX2        ORNE      ETCIX2
234100970117     C     V2CGGP        ORNE      ETCGGP
234200970117     C     V2CSPR        ORNE      ETCSPR
234300970117     C     V2CPMC        ORNE      ETCPMC
234400970117     C     V2CVMC        ORNE      ETCVMC
234500970117     C     V2CFRA        ORNE      ETCFRA
234600970117     C     V2CASS        ORNE      ETCASS
234700970117     C     V2CPCV        ORNE      ETCPCV
234800970117     C     V2CPCM        ORNE      ETCPCM
234900970117     C     V2CPFD        ORNE      ETCPFD
235000040730     C     V2Cval        ORNE      ETCval
235100151119     C     V2Crtm        ORNE      §ETCrtm
235200160210     C     V2Cirp        ORNE      §etcirp
235300970117     C     V3CPPV        ORNE      ETCPPV
235400970117     C     V3CSIN        ORNE      ETCSIN
235500970117     C     V3CESP        ORNE      ETCESP
235600970117     C     V3CIES        ORNE      ETCIES
235700970117     C     V3CPGI        ORNE      ETCPGI
235800970117     C     V3CGGG        ORNE      ETCGGG
235900970117     C     V3CNRG        ORNE      ETCNRG
236000970117     C     V3CIGI        ORNE      ETCIGI
236100970117     C     V4CPAB        ORNE      ETCPAB
236200970117     C     V4CPAM        ORNE      ETCPAM
236300970117     C     V4CICA        ORNE      ETCICA
236400970117     C     V4CCAS        ORNE      ETCCAS
236500970117     C     V4CPAR        ORNE      ETCPAR
236600060313     C     V4Cacb        ORNE      ETCxx1
236700970117     C     V4CPAC        ORNE      ETCPAC
236800970117     C     V4CIAS        ORNE      ETCIAS
236900970117     C     V4CASC        ORNE      ETCASC
237000040811     C     V4Csbnb       ORNE      ETCbnb
237100040824     C     V4Csban       ORNE      ETCban
237200150302     C     comtba        ORNE      §etctba
237300050802     C     V4Ciba        ORNE      ETCiba
237400040811     C     V4Cnbn        ORNE      ETCnbn
237500040811     C     V4Cnba        ORNE      ETCnba
237600970117     C     V5CPCA        ORNE      ETCPCA
237700970117     C     V5CIAP        ORNE      ETCIAP
237800970117     C     V5CPCP        ORNE      ETCPCP
237900970117     C     V5CIPI        ORNE      ETCIPI
238000970117     C     V5CPCS        ORNE      ETCPCS
238100970117     C     V5CISU        ORNE      ETCISU
238200970117     C     V5CPCF        ORNE      ETCPCF
238300970117     C     V5CIFU        ORNE      ETCIFU
238400970414     C     V5CPCD        ORNE      ETCPCD
238500970414     C     V5CIDD        ORNE      ETCIDD
238600970414     C     V5CPCC        ORNE      ETCPCC
238700970414     C     V5CICE        ORNE      ETCICE
238800151119     C     v5cpkl        ORNE      ETCPkl
238900970414     C     V5CPCL        ORNE      ETCPCL
239000970414     C     V5CILO        ORNE      ETCILO
239100011017     C     V5CCIS        ORNE      ETCCIS
239200011017     C     V5CISO        ORNE      ETCISO
239300011017     C     V5CSAM        ORNE      ETCSAM
239400011017     C     V5CINR        ORNE      ETCINR
239500040802     C     V5Cdes        ORNE      ETCdes
239600050808     C     V5CIDE        ORNE      ETCIDE
239700970117     C                   EXSR      IMPETC                                       *IMPOSTA I CAMPI
239800011001     C                   UPDATE    TEETC000
239900970117E   3C                   ENDIF
240000970117E   2C                   ENDIF
240100970117E   1C                   ENDIF
240200011001     C                   UNLOCK    TEETC01L
240300970117     C*
240400970117     C                   ENDSR
240500970117     C*------------------------------------------------------------------------*
240600011001     C* IMPETC - IMPOSTA I CAMPI DEL RECORD DA AGGIORNARE -TEETC-
240700970117     C*------------------------------------------------------------------------*
240800970117     C     IMPETC        BEGSR
240900970117     C*
241000970117IF  1C     *IN99         IFEQ      *ON                                          *INSERIMENTO
241100091120     C                   MOVEL     'S'           ETCDSF
241200980429     C                   MOVEL     KTCCTO        ETCCTO
241300970117     C                   Z-ADD     KTCKSC        ETCKSC
241400970117     C                   Z-ADD     KTCCTR        ETCCTR
241500970117     C                   Z-ADD     KTCPRG        ETCPRG
241600970117     C                   Z-ADD     KTCLNP        ETCLNP
241700970117     C                   Z-ADD     DATCOR        ETCDIR
241800970117     C                   Z-ADD     *ZEROS        ETCDUV
241900970117     C                   MOVEL     *BLANKS       ETCFTR
242000970117     C                   Z-ADD     *ZEROS        ETCDTR
242100970117X   1C                   ELSE                                                   *VARIAZIONE
242200970117     C                   MOVEL     *BLANKS       ETCFTR
242300970117     C                   Z-ADD     DATCOR        ETCDUV
242400970117E   1C                   ENDIF
242500980504     C                   MOVEL     V2CDCV        ETCDCV
242600980504     C                   MOVEL     V2CFAR        ETCFAR
242700970314     C                   Z-ADD     V2CIAR        ETCIAR
242800970117     C                   MOVEL     V2CCLV        ETCCLV
242900970117     C                   MOVEL     V2CBOL        ETCBOL
243000021125     C                   MOVEL     V2CDKC        ETCDKC
243100021125     C                   MOVEL     V2CIX1        ETCIX1
243200021125     C                   MOVEL     V2CIX2        ETCIX2
243300970117     C                   Z-ADD     V2CGGP        ETCGGP
243400970117     C                   Z-ADD     V2CSPR        ETCSPR
243500970117     C                   Z-ADD     V2CPMC        ETCPMC
243600970117     C                   Z-ADD     V2CVMC        ETCVMC
243700970117     C                   Z-ADD     V2CFRA        ETCFRA
243800970117     C                   Z-ADD     V2CASS        ETCASS
243900970117     C                   Z-ADD     V2CPCV        ETCPCV
244000970117     C                   Z-ADD     V2CPCM        ETCPCM
244100970117     C                   Z-ADD     V2CPFD        ETCPFD
244200040730     C                   Z-ADD     V2Cval        ETCval
244300151119     C                   movel     v2crtm        §ETCrtm
244400151119     C                   movel     v2cirp        §ETCirp
244500970117     C                   Z-ADD     V3CPPV        ETCPPV                         *TERZA VIDEATA
244600970117     C                   MOVEL     V3CSIN        ETCSIN
244700970117     C                   Z-ADD     V3CESP        ETCESP
244800970117     C                   MOVEL     V3CIES        ETCIES
244900970117     C                   Z-ADD     V3CPGI        ETCPGI
245000970117     C                   Z-ADD     V3CGGG        ETCGGG
245100970117     C                   Z-ADD     V3CNRG        ETCNRG
245200970117     C                   MOVEL     V3CIGI        ETCIGI
245300970117     C                   Z-ADD     V4CPAB        ETCPAB                         *QUARTA VIDEATA
245400970117     C                   Z-ADD     V4CPAM        ETCPAM
245500970117     C                   MOVEL     V4CICA        ETCICA
245600970117     C                   Z-ADD     V4CCAS        ETCCAS
245700970117     C                   Z-ADD     V4CPAR        ETCPAR
245800060313     C                   Z-ADD     V4Cacb        ETCxx1
245900970117     C                   Z-ADD     V4CPAC        ETCPAC
246000970117     C                   MOVEL     V4CIAS        ETCIAS
246100970117     C                   Z-ADD     V4CASC        ETCASC
246200040811     C                   Z-ADD     V4Csbnb       ETCbnb
246300040824     C                   Z-ADD     V4Csban       ETCban
246400150227     c                   if        v4ctba <> 0
246500150227     C                   move      V4Ctba        §ETCtba                        *QUARTA VIDEATA
246600150227     c                   end
246700050802     C                   MOVEL     V4CIba        ETCIba
246800040811     C                   Z-ADD     V4Cnbn        ETCnbn
246900040811     C                   Z-ADD     V4Cnba        ETCnba
247000151119     C                   movel     v5cpkl        ETCPkl
247100151119     C                   movel     v5cipk        ETCiva
247200970117     C                   Z-ADD     V5CPCA        ETCPCA                         *QUINTA VIDEATA
247300970117     C                   MOVEL     V5CIAP        ETCIAP
247400970117     C                   Z-ADD     V5CPCP        ETCPCP
247500970117     C                   MOVEL     V5CIPI        ETCIPI
247600970117     C                   Z-ADD     V5CPCS        ETCPCS
247700970117     C                   MOVEL     V5CISU        ETCISU
247800970117     C                   Z-ADD     V5CPCF        ETCPCF
247900970117     C                   MOVEL     V5CIFU        ETCIFU
248000970414     C                   Z-ADD     V5CPCD        ETCPCD
248100970414     C                   MOVEL     V5CIDD        ETCIDD
248200970414     C                   Z-ADD     V5CPCC        ETCPCC
248300970414     C                   MOVEL     V5CICE        ETCICE
248400970414     C                   Z-ADD     V5CPCL        ETCPCL
248500970414     C                   MOVEL     V5CILO        ETCILO
248600011017     C                   Z-ADD     V5CCIS        ETCCIS
248700011017     C                   MOVEL     V5CISO        ETCISO
248800011017     C                   Z-ADD     V5CSAM        ETCSAM
248900011017     C                   MOVEL     V5CINR        ETCINR
249000040802     C                   Z-ADD     V5Cdes        ETCdes
249100050808     C                   MOVEL     V5CIDE        ETCIDE
249200011017     C                   MOVEL     *BLANKS       ETCATB
249300151119     C                   movel     detcflo       ETCflo                         *QUARTA VIDEATA
249400970117     C*
249500970117     C                   ENDSR
249600970117     C*------------------------------------------------------------------------*
249700970117     C* IMPLAN - IMPOSTA I DATI DI LANCIO PROGRAMMA
249800970117     C*------------------------------------------------------------------------*
249900970117     C     IMPLAN        BEGSR
250000970117     C*
250100970117     C                   MOVEL     'F06'         D00OP1                         *LIVELLO PROGRAMMA
250200970117     C                   MOVEL     '0'           D00F03                         *F03-FINE PGM
250300970117     C                   MOVEL     '0'           D00F12                         *F12-RITORNO
250400970117     C                   MOVEL     '0'           D00ERR                         *ERRORE
250500970117     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO ERRORE
250600970117     C                   Z-ADD     V0CKSC        D00KSC                         *CLIENTE
250700970117     C                   Z-ADD     V0CCTR        D00CTR                         *TARIFFA
250800970117     C                   Z-ADD     V0CPRG        D00PRG                         *PROGRESSIVO
250900970117     C                   MOVEL     V0CTSP        D00TSP                         *TIPO SERVIZIO
251000970117     C                   MOVEL     V1CFIE        D00FIE                         *ITALIA/ESTERO
251100970117     C                   Z-ADD     V1CLNP        D00LNP                         *LINEA DI PARTENZA
251200970314     C                   Z-ADD     DTCV1C        D00DTC                         *DATA DI RIFERIMENTO
251300970725     C                   MOVEL     WTUR          D00TUR                         *N°GIRO SCRITTURA TA
251400011003     C                   MOVEL     V0CDIV        D00DIV                         *DIVISA TARIFFA/OFFE
251500970117     C*
251600970117     C                   ENDSR
251700970117     C*------------------------------------------------------------------------*
251800970117     C* LANPGM - LANCIA UN PROGRAMMA
251900970117     C*------------------------------------------------------------------------*
252000970117     C     LANPGM        BEGSR
252100970117     C*
252200970117     C                   CALL      LPGM                                 99
252300970117     C                   PARM                    KPJBA
252400970117     C                   PARM                    DSTE00
252500970117     C                   MOVEL     D00MSG        $MSG                           *MESSAGGIO DI RITORN
252600970117IF  1C     D00F03        IFEQ      '1'
252700970117     C                   MOVEL     'S'           $FINE                          *FINE PGM
252800970117E   1C                   ENDIF
252900970117IF  1C     D00ERR        IFEQ      '1'
253000970117     C                   SETON                                        90
253100970117E   1C                   ENDIF
253200970117     C*
253300970117     C                   ENDSR
253400970117     C*------------------------------------------------------------------------*
253500970117     C* CHKLNP - CONTROLLA LA LINEA DI PARTENZA
253600970117     C*------------------------------------------------------------------------*
253700970117     C     CHKLNP        BEGSR
253800970117     C*
253900970117     C     *LIKE         DEFINE    V1CLNP        WLNP
254000020729     C     *LIKE         DEFINE    §NTWFIE       WFL1
254100970117     C     *LIKE         DEFINE    V1DLNP        WDLNP
254200970117     C*
254300970117     C* CONTROLLA ESISTENZA CODICE IN ARCHIVIO
254400970117     C                   MOVEL     WLNP          KORFIL
254500970117     C     KEYORG        CHAIN     AZORG01L                           99
254600970117IF  1C     *IN99         IFEQ      *OFF
254700970117     C     ORGFVA        ANDEQ     *BLANKS
254800970117     C     ORGFAG        ANDNE     'V'
254900970117     C                   MOVEL     ORGDES        WDLNP
255000020729     C                   MOVEL     ORGDE3        OG143
255100020729     C*
255200020729     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
255300020729     C                   CLEAR                   DNTW
255400020729     C*
255500020729     C                   MOVEL(P)  'NTW'         KTECOD
255600020729     C                   MOVEL(P)  §OGNTW        KTEKE1
255700020729     C     KEYTBE        CHAIN     TNTBE01L
255800020729     C                   IF        %found(TNTBE01L)
255900020729     C                   MOVEL     TBEUNI        DNTW
256000020729     C                   ENDIF
256100020729     C*
256200020729     C                   MOVEL     §NTWFIE       WFL1
256300970117X   1C                   ELSE
256400980818     C                   MOVEL     '1'           WERR
256500980818E   1C                   ENDIF
256600970117     C*
256700970117     C                   ENDSR
256800970117     C*------------------------------------------------------------------------*
256900970117     C* IMPVD0 - IMPOSTAZIONE TESTATA
257000970117     C*------------------------------------------------------------------------*
257100970117     C     IMPVD0        BEGSR
257200970117     C*
257300970117     C                   MOVEL     DVID(1)       V0CVID                         *GESTIONE VIDEO
257400970117     C                   MOVEL     *BLANKS       V0COPE                         *OPERAZIONE
257500970117IF  1C     D00TLA        IFEQ      'P'
257600970117     C                   MOVEL     DVID(2)       V0CTLA                         *LIVELLO PROCEDURA
257700970117X   1C                   ELSE
257800970117     C                   MOVEL     DVID(3)       V0CTLA                         *LIVELLO PROCEDURA
257900970117E   1C                   ENDIF
258000091120     c*
258100091120     C                   MOVEL     'Prg.'        d0cprg                         *TARIFFA O OFFERTA
258200091103     c                   if        %subst(d00op0: 1: 1) = 'O'
258300091120     c                   if        d00dsf =' '
258400091120     C                   MOVEL     'Visita '     d0cKsc                         *TARIFFA O OFFERTA
258500970117X   1C                   ELSE
258600091120     C                   MOVEL     'Tratt. '     d0cksc                         *TARIFFA O OFFERTA
258700091120     C                   MOVEL     'Ver.'        d0cprg                         *TARIFFA O OFFERTA
258800091120E   1C                   ENDIF
258900091120X   1C                   ELSE
259000091120     C                   MOVEL     'Cliente'     d0cksc                         *TARIFFA O OFFERTA
259100970117E   1C                   ENDIF
259200970117IF  1C     $TGES         IFEQ      'I'                                          *INTERROGAZIONE
259300970117     C                   SETON                                        10        *SI PROTEZIONE CAMPI
259400970117     C                   MOVEL     DOPE(3)       V0CGES                         *INTERROGAZIONE
259500970117X   1C                   ELSE                                                   *GESTIONE
259600970117     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
259700970117     C                   MOVEL     DOPE(4)       V0CGES                         *GESTIONE
259800970117E   1C                   ENDIF
259900970117     C* CAMPI
260000970117     C                   Z-ADD     D00KSC        V0CKSC
260100970117     C                   Z-ADD     D00CTR        V0CCTR
260200970117     C                   Z-ADD     D00PRG        V0CPRG
260300970117     C                   MOVEL     D00FIE        V0CFIE
260400011003     C                   MOVEL     D00DIV        V0CDIV
260500970117IF  1C     D00FIE        IFEQ      *BLANKS                                      *NE ITALIA/NE ESTERO
260600970117     C                   SETON                                        01        *CHIEDERE TAR.I/E
260700970117X   1C                   ELSE                                                   *ITALIA O ESTERO
260800970117     C                   SETOFF                                       01        *NON CHIEDERE TAR.I/
260900970117     C                   MOVEL     D00FIE        V1CFIE
261000970117E   1C                   ENDIF
261100970117     C                   MOVEL     D00TSP        V0CTSP
261200970117     C                   Z-ADD     D00LNP        V1CLNP
261300970318     C                   Z-ADD     D00DTC        G08INV
261400970318     C                   Z-ADD     *ZEROS        G08DAT
261500970318     C                   MOVEL     '3'           G08ERR
261600970318     C                   CALL      'XSRDA8'
261700970318     C                   PARM                    WLBDA8
261800970318     C                   Z-ADD     G08DAT        V1CDTC                         *DATA RIFERIM.CALCOL
261900980429     C*
262000970117     C* DECODIFICHE
262100970117     C                   MOVEL     *BLANKS       V0DKSC
262200970117IF  1C     D00KSC        IFGT      *ZEROS
262300020923     C                   Z-ADD     1             KACKUT
262400020923     C                   Z-ADD     DUTKCI        KACKCC
262500970117     C                   Z-ADD     D00KSC        KACKSC
262600091103IF  2C                   IF        %subst(d00op0: 1: 1) ='T'                    *TARIFFA
262700970117     C     KEYACO        CHAIN     CNACO00F                           99
262800970414X   2C                   ELSE                                                   *OFFERTA
262900970414     C     KEYACO        CHAIN     TFACO00F                           99
263000970414E   2C                   ENDIF
263100970117IF  2C     *IN99         IFEQ      *OFF
263200970117     C                   MOVEL     ACORAG        V0DKSC                         *CLIENTE
263300970117E   2C                   ENDIF
263400970117E   1C                   ENDIF
263500970117     C*
263600970117     C                   MOVEL     *BLANKS       V0DCTR
263700970117IF  1C     D00KSC        IFGT      *ZEROS
263800970117     C                   MOVEL     D00CTR        WA1               1
263900970117     C                   SETOFF                                           99
264000970117     C                   Z-ADD     1             I
264100970117     C     WA1           LOOKUP    TTR(I)                                 99
264200970117IF  2C     *IN99         IFEQ      *ON
264300970117     C                   MOVEL     DTTR(I)       V0DCTR                         *TIPO TARIFFA
264400970117E   2C                   ENDIF
264500970117E   1C                   ENDIF
264600970117     C*
264700970117     C                   MOVEL     *BLANKS       V0DTSP
264800970117IF  1C     D00TSP        IFNE      *BLANKS
264900970117     C                   SETOFF                                           99
265000970117     C                   Z-ADD     1             I
265100970117     C     D00TSP        LOOKUP    TSP(I)                                 99
265200970117IF  2C     *IN99         IFEQ      *ON
265300970117     C                   MOVEL     DTSP(I)       V0DTSP                         *TIPO SERVIZIO
265400970117E   2C                   ENDIF
265500970117E   1C                   ENDIF
265600980429     C*
265700980429     C                   MOVEL     *BLANKS       V0DFIE
265800980429IF  1C     D00FIE        IFEQ      'I'
265900980429     C                   MOVEL     DDEC(1)       V0DFIE                         *FLAG ITALIA/ESTERO
266000980429E   1C                   ENDIF
266100980429IF  1C     D00FIE        IFEQ      'E'
266200980429     C                   MOVEL     DDEC(2)       V0DFIE                         *FLAG ITALIA/ESTERO
266300980429E   1C                   ENDIF
266400970117     C*
266500970117     C                   MOVEL     *BLANKS       V1DLNP
266600970117IF  1C     D00LNP        IFGT      *ZEROS
266700970117     C                   Z-ADD     D00LNP        KORFIL
266800970117     C     KEYORG        CHAIN     AZORG01L                           99
266900970117IF  2C     *IN99         IFEQ      *OFF
267000970117     C     ORGFVA        ANDEQ     *BLANKS
267100970117     C     ORGFAG        ANDNE     'V'
267200970117     C                   MOVEL     ORGDES        V1DLNP
267300970117E   2C                   ENDIF
267400970117E   1C                   ENDIF
267500970117     C*
267600970117     C                   ENDSR
267700970117     C*------------------------------------------------------------------------*
267800011011     C* CTRENT - CONTROLLI INIZIALI DI ENTRATA
267900970117     C*------------------------------------------------------------------------*
268000970117     C     CTRENT        BEGSR
268100970117     C*
268200970117     C* TIPO TARIFFA NON IN GESTIONE, ERRORE
268300970117     C                   MOVEL     D00CTR        WA1               1
268400970117     C                   SETOFF                                           99
268500970117     C     WA1           LOOKUP    TTR                                    99
268600970117IF  1C     *IN99         IFEQ      *OFF
268700970117     C                   MOVEL     MSG(60)       D00MSG
268800970117     C                   MOVEL     '1'           D00ERR
268900970117     C                   MOVEL     'S'           $FINE                          *USCITA PGM
269000970117E   1C                   ENDIF
269100970117     C*
269200011011     C     D00TLA        IFEQ      'P'
269300011011     C     D00DIV        ANDEQ     'ITL'
269400011011     C     DATCOR        ANDGE     §GEDCEDAT
269500011011     C                   MOVEL     MSG(65)       D00MSG
269600011011     C                   MOVEL     '1'           D00ERR
269700011011     C                   MOVEL     'S'           $FINE                          *USCITA PGM
269800011011     C                   ENDIF
269900020213     C*
270000020213     C     D00DPD        IFEQ      'F'
270100020213     C                   MOVEL     MSG(66)       D00MSG
270200020213     C                   MOVEL     '1'           D00ERR
270300020213     C                   MOVEL     'S'           $FINE                          *USCITA PGM
270400020213     C                   ENDIF
270500011011     C*
270600970117     C                   ENDSR
270700150227     C*------------------------------------------------------------------------*
270800150227     C* CNTCOD - CONTROLLA PRESENZA DEL tipo bancale                   "
270900150227     C*------------------------------------------------------------------------*
271000150227     C     CNTtba        BEGSR
271100150227     C*
271200150227     C                   CLEAR                   Dtba
271300150227     C                   CLEAR                   v4dtba
271400150227     C*
271500150227     C                   MOVEL(P)  'TBA'         KTECOD
271600150227     C                   MOVEL(P)  v4ctba        KTEKE1
271700150227     C     KEYTBE        CHAIN     TNTBE01L
271800150227     C                   IF        %found(TNTBE01L)
271900150227     C                   MOVEL     TBEUNI        Dtba
272000150227     c                   movel     §tbadesc      v4dtba
272100150227     c                   else
272200150227     c                   seton                                        1890
272300150227     C                   ENDIF
272400150227     C*
272500150227     C                   ENDSR
272600011011     C*------------------------------------------------------------------------*
272700011011     C* CARGEDCE - CARICAMENTO TABELLA "GED" TIPO RECORD "CE" E CONSIDERAZIONI SU DIVISE
272800011011     C*------------------------------------------------------------------------*
272900011011     C     CARGEDCE      BEGSR
273000011011     C*
273100011011     C                   CLEAR                   DSGEDCE
273200011026     C                   MOVEL     *blanks       WDIVCE            3
273300011011     C*
273400011011     C                   MOVEL(P)  'GED'         KTECOD
273500011011     C                   MOVEL(P)  'CE'          KTEKE1
273600011011     C     KEYTBE        CHAIN     TNTBE01L
273700011011     C                   IF        %found(TNTBE01L)
273800011011     C                   MOVEL     TBEUNI        DSGEDCE
273900011011     C                   ENDIF
274000011011     C*
274100011011     C                   IF        §GEDCEDAT > D00DTC
274200011026     C                   EVAL      WDIVCE = §GEDCEDIP
274300011026     C                   ELSE
274400011026     C                   EVAL      WDIVCE = §GEDCEDIV
274500011011     C                   ENDIF
274600011011     C*
274700011011     C                   ENDSR
274800970117     C*------------------------------------------------------------------------*
274900970117     C* CARTAB - CARICA LE TABELLE OCCORRENTI
275000970117     C*------------------------------------------------------------------------*
275100970117     C     CARTAB        BEGSR
275200970414     C*
275300970414     C* IMPOSTAZIONE APPLICAZIONE TARIFFA FINITA IN DS
275400970401     C                   Z-ADD     D00KSC        KTMKSC                         *CLIENTE
275500970401     C                   Z-ADD     D00CTR        KTMCTR                         *TARIFFA
275600970401     C                   Z-ADD     D00PRG        KTMPRG                         *PROGRESSIVO
275700970401     C     KEYTAM        CHAIN     TNTAM01L                           99        *ESISTE PER FORZA
275800970401     C                   Z-ADD     TAMATA        D00ATA                         *APPL.TAR.FINITA
275900970401IF  1C     D00ATA        IFEQ      *ZEROS
276000020923     C                   Z-ADD     1             KTBKUT
276100970401     C                   MOVEL     'TR'          KTBCOD
276200970401     C                   MOVEL     D00CTR        UNOCTR            1 0          *TIPO TARIFFA
276300970401     C                   MOVEL     UNOCTR        KTBKEY
276400970401     C     KEYTAB        CHAIN     TABEL00F                           99
276500970401IF  2C     *IN99         IFEQ      *OFF
276600970401     C                   MOVEL     TBLUNI        DSTR
276700970401     C                   Z-ADD     §TRATA        D00ATA                         *MIN.APPL.TAR.FINITA
276800970401E   2C                   ENDIF
276900970401E   1C                   ENDIF
277000970117     C*--------------------
277100970117     C* TIPI SERVIZIO
277200970117     C*--------------------
277300970117     C                   Z-ADD     *ZEROS        I                 4 0
277400020923     C                   Z-ADD     1             KTBKUT
277500970117     C                   MOVEL     '5E'          KTBCOD
277600970117     C     KE2TAB        SETLL     TABEL00F
277700970117     C     KE2TAB        READE     TABEL00F                               99
277800970117DO  1C     *IN99         DOWEQ     *OFF
277900970117IF  2C     TBLFLG        IFEQ      *BLANKS
278000970117     C                   ADD       1             I
278100970117     C                   MOVEL     TBLUNI        DS5E
278200970117     C                   MOVEL     TBLKEY        TSP(I)                         *CODICE
278300970117     C                   MOVEL     §5EDES        DTSP(I)                        *DESCRIZIONE
278400090611      *imposta costanti a video per tipo servizio 'E'
278500090611     c                   if        tsp(i) = 'E'
278600090611     c                   movel     §5ed08        costserv
278700090611     c                   eval      costserv2 = '%sped in ' + costserv +
278800090611     c                             '............:'
278900090611     c                   eval      costs = 'TNTE01R- Tariffa '+ costserv
279000090611     c                   endif
279100970117E   2C                   ENDIF
279200970117     C     KE2TAB        READE     TABEL00F                               99
279300970117E   1C                   ENDDO
279400970117     C*--------------------
279500970117     C* TIPI TARIFFA
279600970117     C*--------------------
279700970117     C                   Z-ADD     *ZEROS        I                 4 0
279800020923     C                   Z-ADD     1             KTBKUT
279900970117     C                   MOVEL     'TR'          KTBCOD
280000970117     C     KE2TAB        SETLL     TABEL00F
280100970117     C     KE2TAB        READE     TABEL00F                               99
280200970117DO  1C     *IN99         DOWEQ     *OFF
280300970117IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
280400970117     C                   MOVEL     TBLUNI        DSTR
280500970117IF  3C     §TRUTE        IFEQ      'S'                                          *IN GESTIONE
280600970117     C                   ADD       1             I
280700970117     C                   MOVEL     TBLKEY        TTR(I)                         *CODICE
280800970117     C                   MOVEL     §TRDE3        DTTR(I)                        *DESCRIZIONE
280900970117E   3C                   ENDIF
281000970117E   2C                   ENDIF
281100970117     C     KE2TAB        READE     TABEL00F                               99
281200970117E   1C                   ENDDO
281300970117     C*--------------------
281400970117     C* TARIFFA DI CARTELLO
281500970117     C*--------------------
281600970117     C* OFFERTE -> IL PGM HA DAVANTI DEGLI OVRDBF CHE SOSTITUISCONO LE OFFERTE
281700970117     C* CON LE TARIFFE. PER LEGGERE LE TARIFFE OCCORRE LEGGERE LE OFFERTE
281800970117     C* TARIFFE -> IL PGM NON HA DAVANTI NESSUN OVRDBF
281900021202     C                   Z-ADD     *ZEROS        CARKSC
282000021202     C                   Z-ADD     *ZEROS        CARCTR
282100021202     C                   Z-ADD     *ZEROS        CARPRG
282200021202     C                   MOVEL     *BLANKS       CARDIV
282300160229     C                   CLEAR                   TRULC7DS
282400160229     C                   EVAL      IC7TLA = 'L'
282500160229     c                   eval      ic7tfie='I'
282600160229     C                   EVAL      IC7DTA = DATCOR
282700160229     C                   CALL      'TRULC7R'
282800160229     C                   PARM                    TRULC7DS
282900021202     C*
283000160229IF  1C                   IF        Oc7ERR <> 'E'
283100160229     C                   Z-ADD     oc7kscc       CARKSC
283200160229     C                   Z-ADD     oc7ctrc       CARCTR
283300160229     C                   movel     oc7prgc       CARprg
283400160229     c* chain in tntam per prendere la divisa
283500160229IF  2C                   IF        %subst(d00op0:1 :1)='O'
283600160229     C     Kcart         chain     TNOFM01L
283700160229     c                   else
283800160229     C     Kcart         chain     TNTAM01L
283900160229     c                   endif
284000160229     c
284100160229     c                   if        %found
284200160229     C                   MOVEL     TAMFLO        DSTA01
284300160229     C                   MOVEL     §TADIV        CARDIV
284400160229     c                   endif
284500160229     c                   endif
284600160229     c***
284700160229     C***                Z-ADD     O27KSC        KTMKSC
284800160229IF  2C***                IF        %subst(d00op0:1 :1)='O'
284900160229     C***  KTMKSC        SETLL     TNOFM01L
285000160229     C***  KTMKSC        READE     TNOFM01L                               99
285100160229X   2C***                ELSE
285200160229     C***  KTMKSC        SETLL     TNTAM01L
285300160229     C***  KTMKSC        READE     TNTAM01L                               99
285400160229E   2C***                ENDIF
285500160229DO  2C***  *IN99         DOWEQ     *OFF
285600160229IF  3C***  TAMATB        IFNE      'A'                                          *NO ANNULLATE
285700160229     C***  TAMBAP        ANDNE     'B'                                          *NO BLOCCATE
285800160229IF  4C***  DATCOR        IFGE      TAMDDT                                       *IN DECORRENZA
285900160229     C***  DATCOR        ANDLE     TAMDST
286000160229     C***                Z-ADD     TAMKSC        CARKSC
286100160229     C***                Z-ADD     TAMCTR        CARCTR
286200160229     C***                Z-ADD     TAMPRG        CARPRG
286300160229     C***                MOVEL     TAMFLO        DSTA01
286400160229     C***                MOVEL     §TADIV        CARDIV
286500160229     C***                LEAVE
286600160229E   4C***                ENDIF
286700160229E   3C***                ENDIF
286800160229IF  3C***                IF        %subst(d00op0: 1: 1)= 'O'
286900160229     C***  KTMKSC        READE     TNOFM01L                               99
287000160229X   3C***                ELSE
287100160229     C***  KTMKSC        READE     TNTAM01L                               99
287200160229E   3C***                ENDIF
287300160229E   2C***                ENDDO
287400160229E   1C***                ENDIF
287500021202     C*
287600970117     C                   ENDSR
287700020923     C*--------------------------------------------------------------------------------------------*
287800020923     C* REPERISCE I DATI UTENTE
287900020923     C*--------------------------------------------------------------------------------------------*
288000020923     C     REPDATIUTE    BEGSR
288100020923     C*
288200020923     C* INIZIALIZZA VARIABILI DI WRK
288300020923     C                   CLEAR                   TIBS34DS
288400020923     C                   CLEAR                   AZUTEDS
288500020923     C                   CLEAR                   DDATIUTE
288600020923     C*
288700020923     C     *DTAARA       DEFINE    §azute        azuteds
288800020923     C     *DTAARA       DEFINE    §datiute      ddatiute
288900020923     C                   IN(E)     *DTAARA
289000020923if  1C                   IF        %Error
289100020923     c                   EVAL      I34Tla = 'L'
289200020923     C                   CALL      'TIBS34R'
289300020923     C                   PARM                    Tibs34Ds
289400020923     C                   IN        *DTAARA
289500020923e   1C                   ENDIF
289600020923     C*
289700020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
289800020923     C                   MOVEL(P)  RSUT          DSFIRS           20
289900020923     C*
290000020923     C                   ENDSR
290100970117     C*------------------------------------------------------------------------*
290200970117     C* RUTINZ - OPERAZIONI INIZIALI
290300970117     C*------------------------------------------------------------------------*
290400970117     C     RUTINZ        BEGSR
290500970117     C*--------------------
290600970117     C* RICEVIMENTO PARAMETRI
290700970117     C*--------------------
290800970117     C     *ENTRY        PLIST
290900970117     C                   PARM                    KPJBA
291000970117     C                   PARM                    DSTE00
291100041202     c* apertura controllata di alcuni file xchè solo di filiale
291200041202     c                   open      tfaco00f                             95
291300041202     c                   open      tfclp00f                             95
291400041202     c                   open      tfind00f                             95
291500041202     c                   open      tnofm01l                             95
291600970117     C*--------------------
291700970117     C* VARIABILI RIFERITE AL DATABASE
291800970117     C*--------------------
291900970117     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
292000970117     C     *LIKE         DEFINE    TBLCOD        KTBCOD
292100970117     C     *LIKE         DEFINE    TBLKEY        KTBKEY
292200970117     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
292300970117     C     *LIKE         DEFINE    ACOKUT        KACKUT                         *CNAC000F
292400970117     C     *LIKE         DEFINE    ACOKCC        KACKCC
292500970117     C     *LIKE         DEFINE    ACOKSC        KACKSC
292600970414     C     *LIKE         DEFINE    CLPKUT        KCLKUT                         *CNCLP00F
292700970414     C     *LIKE         DEFINE    CLPKCC        KCLKCC
292800970414     C     *LIKE         DEFINE    CLPKSC        KCLKSC
292900091103     C     *LIKE         DEFINE    d04NRV        KVINRV                         *TNVIS00F
293000990818     C     *LIKE         DEFINE    TADKSC        KTDKSC                         *TITAD00F
293100970117     C     *LIKE         DEFINE    TADCTR        KTDCTR
293200970117     C     *LIKE         DEFINE    TADPRG        KTDPRG
293300970117     C     *LIKE         DEFINE    TADLNP        KTDLNP
293400011001     C     *LIKE         DEFINE    ETCDSF        KTCDSF                         *TEETC00F
293500980429     C     *LIKE         DEFINE    ETCCTO        KTCCTO
293600970117     C     *LIKE         DEFINE    ETCKSC        KTCKSC
293700970117     C     *LIKE         DEFINE    ETCCTR        KTCCTR
293800970117     C     *LIKE         DEFINE    ETCPRG        KTCPRG
293900970117     C     *LIKE         DEFINE    ETCLNP        KTCLNP
294000970117     C     *LIKE         DEFINE    TAMKSC        CARKSC
294100970117     C     *LIKE         DEFINE    TAMCTR        CARCTR
294200970117     C     *LIKE         DEFINE    TAMPRG        CARPRG
294300970401     C     *LIKE         DEFINE    TAMKSC        KTMKSC                         *TNTAM00F
294400970401     C     *LIKE         DEFINE    TAMCTR        KTMCTR
294500970401     C     *LIKE         DEFINE    TAMPRG        KTMPRG
294600011003     C     *LIKE         DEFINE    §TADIV        CARDIV
294700970725     C     *LIKE         DEFINE    D00TUR        WTUR                           *LAVORO
294800011011     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
294900011011     C     *LIKE         DEFINE    TBEKE1        KTEKE1
295000970117     C*--------------------
295100970117     C* CHIAVI DI LETTURA
295200970117     C*--------------------
295300970117     C* LETTURA TABEL00F - COMPLETA
295400970117     C     KEYTAB        KLIST
295500970117     C                   KFLD                    KTBKUT                         *UTENTE
295600970117     C                   KFLD                    KTBCOD                         *TABELLA
295700970117     C                   KFLD                    KTBKEY                         *CODICE
295800970117     C* LETTURA TABEL00F - PARZIALE
295900970117     C     KE2TAB        KLIST
296000970117     C                   KFLD                    KTBKUT                         *UTENTE
296100970117     C                   KFLD                    KTBCOD                         *TABELLA
296200970117     C* LETTURA AZORG01L - COMPLETA
296300970117     C     KEYORG        KLIST
296400970117     C                   KFLD                    KORFIL                         *FILIALE
296500970414     C* LETTURA CNACO00F - COMPLETA
296600970117     C     KEYACO        KLIST
296700970117     C                   KFLD                    KACKUT                         *CODICE UTENTE
296800970117     C                   KFLD                    KACKCC                         *CODICE CAPOCONTO
296900970117     C                   KFLD                    KACKSC                         *CODICE CLIENTE
297000970414     C* LETTURA CNCLP00F - COMPLETA
297100970414     C     KEYCLP        KLIST
297200970414     C                   KFLD                    KCLKUT                         *CODICE UTENTE
297300970414     C                   KFLD                    KCLKCC                         *CODICE CAPOCONTO
297400970414     C                   KFLD                    KCLKSC                         *CODICE CLIENTE
297500011001     C* LETTURA TEETC01L - COMPLETA
297600970117     C     KEYETC        KLIST
297700980429     C                   KFLD                    KTCDSF                         *DATI SEDE/FILIALE
297800980429     C                   KFLD                    KTCCTO                         *TARIFFA O OFFERTA
297900970117     C                   KFLD                    KTCKSC                         *CLIENTE
298000970117     C                   KFLD                    KTCCTR                         *TARIFFA
298100970117     C                   KFLD                    KTCPRG                         *PROGRESSIVO
298200970117     C                   KFLD                    KTCLNP                         *LINEA PARTENZA
298300990818     C* LETTURA TITAD04L - PARZIALE
298400970117     C     KEYTAD        KLIST
298500970117     C                   KFLD                    KTDKSC                         *CLIENTE
298600970117     C                   KFLD                    KTDCTR                         *TARIFFA
298700970117     C                   KFLD                    KTDPRG                         *PROGRESSIVO
298800970117     C                   KFLD                    KTDLNP                         *LINEA PARTENZA
298900990818     C* LETTURA TITAD04L - PARZIALE
299000970204     C     KE2TAD        KLIST
299100970204     C                   KFLD                    KTDKSC                         *CLIENTE
299200970204     C                   KFLD                    KTDCTR                         *TARIFFA
299300970204     C                   KFLD                    KTDPRG                         *PROGRESSIVO
299400970401     C* LETTURA TNTAM01L - COMPLETA
299500970401     C     KEYTAM        KLIST
299600970401     C                   KFLD                    KTMKSC                         *CLIENTE
299700970401     C                   KFLD                    KTMCTR                         *TARIFFA
299800970401     C                   KFLD                    KTMPRG                         *PROGRESSIVO
299900160229     C     Kcart         KLIST
300000160229     C                   KFLD                    carKSC                         *CLIENTE
300100160229     C                   KFLD                    carCTR                         *TARIFFA
300200160229     C                   KFLD                    carPRG                         *PROGRESSIVO
300300011011     C* LETTURA TNTBE01L - PARZIALE
300400011011     C     KEYTBE        KLIST
300500011011     C                   KFLD                    KTECOD                         *UTENTE
300600011011     C                   KFLD                    KTEKE1                         *CODICE
300700970117     C*--------------------
300800970117     C* VARIABILI NON RIFERITE AL DATABASE
300900970117     C*--------------------
301000970117     C                   MOVEL     'N'           $FINE             1            *FINE PROGRAMMA
301100970117     C                   MOVEL     '0'           $REC              1            *STATO RECORD
301200970117     C                   MOVEL     '0'           WERR              1            *ERRORE
301300970117     C                   MOVEL     '1'           TIPVID            1            *VIDEO DA GESTIRE
301400990818     C                   MOVEL     '0'           $TAD              1            *ESISTENZA TITAD
301500970117     C                   MOVEL     'N'           $CONF             1            *CONFERMA CARICAMENT
301600970117     C                   MOVEL     'N'           $FINV             1            *FINE VIDEATA
301700970117     C                   MOVEL     ' '           $TGES             1            *GESTIONE (I/G)
301800970117     C*
301900970117     C* REPERISCE I DATI DELL'UTENTE
302000020923     C                   EXSR      REPDATIUTE
302100970117     C*
302200970117     C* REPERISCE LA DATA CORRENTE
302300970117     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
302400970117     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
302500970117     C                   Z-ADD     WN8           G08DAT
302600970117     C                   Z-ADD     *ZEROS        G08INV
302700970117     C                   MOVEL     '0'           G08ERR
302800970117     C                   CALL      'XSRDA8'
302900970117     C                   PARM                    WLBDA8
303000970117     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
303100970117     C*
303200970117     C* 02->GESTIONE, 05->INTERROGAZIONE
303300970117     C                   MOVE      D00OP0        WA2               2
303400970117IF  1C     WA2           IFEQ      '02'
303500970117     C                   MOVEL     'G'           $TGES                          *GESTIONE
303600970117X   1C                   ELSE
303700970117     C                   MOVEL     'I'           $TGES                          *INTERROGAZIONE
303800970117E   1C                   ENDIF
303900970129     C*
304000970602     C* SE PROFILO EDP IMPOSTA IL LANCIO DEL PROGRAMMA IN DEBUG = 'SI'
304100970507     C                   MOVEL     'N'           D00DBG
304200970620     C                   MOVEL     KNMUS         WUTE              3
304300970620IF  1C     WUTE          IFEQ      'EDP'
304400970910     C                   MOVEL     'S'           D00DBG
304500970602E   1C                   ENDIF
304600970117     C*
304700970117     C* CARICA LE TABELLE OCCORRENTI
304800011011     C                   EXSR      CARTAB
304900011011     C*
305000011011     C* CARICA LE NUOVE TABELLE OCCORRENTI
305100011011     C                   EXSR      CARGEDCE
305200970117     C*
305300970117     C* CONTROLLI INIZIALI DI ENTRATA
305400970117     C                   EXSR      CTRENT
305500970117     C*
305600970117     C* IMPOSTA LA TESTATA
305700970319     C                   EXSR      IMPVD0
305800970414     C*
305900970414     C* PROPONE LA LINEA DI PARTENZA DEL COMMERCIALE DEL CLIENTE
306000011031     C                   MOVEL     *BLANKS       WRKCTS            2
306100020923     C                   Z-ADD     1             KCLKUT
306200020923     C                   Z-ADD     DUTKCI        KCLKCC
306300970414     C                   Z-ADD     D00KSC        KCLKSC
306400091103IF  1C                   IF        %subst(d00op0: 1:1)='T'                      *TARIFFA
306500970414     C     KEYCLP        CHAIN     CNCLP00F                           99
306600011030     C     KEYCLP        CHAIN     CNIND00F                           99
306700970414X   1C                   ELSE                                                   *OFFERTA
306800970414     C     KEYCLP        CHAIN     TFCLP00F                           99
306900011030     C     KEYCLP        CHAIN     TFIND00F                           99
307000970414IF  2C     *IN99         IFEQ      *ON
307100091103     c                   clear                   tnte04ds
307200091103     C                   movel     D00dsf        d04dsf
307300091103     C                   Z-ADD     D00KSC        d04nrv
307400091103     c                   call      'TNTE04R'
307500091103     c                   parm                    kpjba
307600091126     c                   parm                    tnte04ds
307700091103     C                   Z-ADD     d04KSC        KCLKSC                         *CLIENTE DELLA VISIT
307800091120     c                   movel     d04rag        v0dksc
307900970414     C     KEYCLP        CHAIN     CNCLP00F                           99
308000011031     C     KEYCLP        CHAIN     CNIND00F                           99
308100970414E   2C                   ENDIF
308200970414E   1C                   ENDIF
308300970414IF  1C     *IN99         IFEQ      *OFF
308400970414     C                   MOVEL     CLPAGE        V1CLNP
308500011030     C                   MOVEL     INDCAE        WRKCTS
308600970414E   1C                   ENDIF
308700011031     C* Imposto il codice tassaizione del cliente tariffa / offerta (se visita di un potenziale
308800011031     C* il cliente può essere vuoto e quindi il codice tassazione = *blanks
308900011030     C                   EVAL      D00PRI = WRKCTS
309000970319     C*
309100970319     C* IMPOSTA NELLA FINESTRA CARICA DA ALTRA TARIFFA IL CODICE DELLA CARTELLO
309200091120     C                   MOVEL     'S'           D0DDSF
309300970414     C                   MOVEL     'T'           D0DCTO
309400970414     C                   Z-ADD     CARKSC        D0DKSC
309500970414     C                   Z-ADD     CARCTR        D0DCTR
309600970414     C                   Z-ADD     CARPRG        D0DPRG
309700970414     C                   Z-ADD     V1CLNP        D0DLNP
309800011003     C                   MOVEL     CARDIV        D0DDIV
309900970117     C*
310000970117     C                   ENDSR
310100970117     O*------------------------------------------------------------------------*
310200970117** -DDEC-
310300970117ITALIA              1
310400970117ESTERO              2
310500980429SEDE                3
310600980429FILIALE             4
3107009808185
3108009808186
3109009808187
3110009808188
3111009808189
311200980818TUTTE LE FILIALI    10
311300970117** -DVID-
311400970117***   GENERALI   ***1
311500970117PORTO               2
311600970117DELTA               3
3117009701174
3118009701175
311900970117** -DOPE-
312000970117* INSERIMENTO *1
312100970117* VARIAZIONE * 2
312200970117INTERROGAZIONE 3
312300970117 * GESTIONE *  4
3124009701175
312500970619** -MSG-
312600970117TNTE01R- Immettere I per elaborare la tariffa italia, E per quella estero     1
312700970117TNTE01R- Dati tipo richiesti non trovati                                      2
312800970117TNTE01R- Filiale errata                                                       3
312900091123TNTE01R- Il valore scelto non è corretto                                      4
313000970117TNTE01R- Qualità cliente errata. Immettere uno dei valori possibili           5
313100970117TNTE01R- Passaggio dati errato. Immettere uno dei valori possibili            6
313200970117TNTE01R- Immettere il peso medio del collo                                    7
313300970117TNTE01R- Immettere il volume medio del collo                                  8
313400970117TNTE01R- Non esistono i dati tipo per la tariffa/visita richiesta             9
313500970117TNTE01R- Immettere il numero di spedizioni medie per ritiro                   10
313600970117TNTE01R- Immettere il numero di spedizioni medie per consegna                 11
313700980803TNTE01R- Immettere la % delle spedizioni in franco e/o assegnato              12
313800970117TNTE01R- La % fra franchi e assegnati non può essere maggiore di 100          13
313900970117TNTE01R- La % fra franchi e assegnati deve essere uguale a 100                14
314000970211TNTE01R- Impossibile calcolare il porto/delta per un punto operativo estero   15
314100970117TNTE01R- Tariffa italia: la linea di partenza non può essere estera           16
314200970117TNTE01R- Inserire S per inglobare la consegna al porto, N per non inglobarla  17
314300970117TNTE01R- Per inglobare la consegna inserire la percentuale                    18
314400970117TNTE01R- La % fra c/assegni Bartolini e mittente non può essere > di 100      19
314500970117TNTE01R- Inserire S per inglobare il c/assegno al porto, N per non inglobarlo 20
314600970117TNTE01R- Per inglobare il c/assegno inserire almeno una percentuale           21
314700970117TNTE01R- Inserire il valore medio del contrassegno                            22
314800060315TNTE01R- La % fra assicurazione R.C.V., AC BASE e AC PLUS deve essere = 100   23
314900970117TNTE01R- Inserire S per inglobare l'assicurazione al porto, N per non farlo   24
315000970117TNTE01R- Per inglobare l'assicurazione inserire almeno una percentuale        25
315100970117TNTE01R- Inserire il valore medio dell'importo da assicurare                  26
315200970117TNTE01R- La percentuale non può essere maggiore di 100                        27
315300970117TNTE01R- Tariffa espresso. Inserire 100 nella % di spedizioni di questo tipo  28
315400970117TNTE01R- Tariffa espresso. L'inglobo nel porto è obbligatorio                 29
315500970117TNTE01R- Inserire S per inglobare l'espresso al porto, N per non inglobarlo   30
315600970117TNTE01R- Per inglobare l'espresso inserire la percentuale                     31
315700970117TNTE01R- Non inserire il valore medio contrassegno                            32
315800970117TNTE01R- Non inserire il valore medio importo da assicurare                   33
315900970117TNTE01R- Inserire S per inglobare la giacenza al porto, N per non inglobarla  34
316000970117TNTE01R- Per inglobare la giacenza inserire la percentuale                    35
316100970117TNTE01R- Inserire i giorni medi di sosta                                      36
316200970117TNTE01R- Non inserire i giorni medi di sosta                                  37
316300980429TNTE01R- Immettere S per gestire i dati creati da sede, F x quelli di filiale 38
316400980429TNTE01R- "S" per caricare i dati creati da sede, "F" per quelli di filiale    39
316500970314TNTE01R- Inserire S per inglobare l'inoltro al porto, N per non inglobarlo    40
316600970314TNTE01R- Per inglobare l'inoltro inserire la percentuale                      41
316700970117TNTE01R- Segno errato                                                         42
316800970314TNTE01R- Se inserito il segno immettere anche la % di scostamento e viceversa 43
316900980504TNTE01R- Descrizione obbligatoria                                             44
317000970117TNTE01R- Inserire il numero delle riaperture giacenza                         45
317100970117TNTE01R- Non inserire il numero delle riaperture giacenza                     46
317200970314TNTE01R- Data riferimento calcolo errata                                      47
317300970314TNTE01R- Tipo arrotondamento errato                                           48
317400970314TNTE01R- Se inserito tipo arrotondamento inserire anche l'importo e viceversa 49
317500980803TNTE01R- La percentuale può essere solo uguale a 100                          50
317600980803TNTE01R- Inserire il tipo porto: 100% di spedizioni o franco o assegnato      51
317700011008TNTE01R- Impossibile inserire importi con decimali quando la divisa è ITL.    52
317800011009TNTE01R- Se si indica la percentuale è obbligatorio inglobare al porto.       53
317900011009TNTE01R- Inserire S per inglobare il ritiro al porto, N per non inglobarlo.   54
318000011108TNTE01R- I valori consentiti sono 0,10 0,25 0,50 0,75 1,00                    55
318100011031TNTE01R- Impossibile impostare valore, il cliente non risiede in CAP con isole56
318200021104TNTE01R- Immettere uno dei valori possibili                                   57
318300021104TNTE01R- Impossibile indicare Procedura Disk "C" x soluzione "B".             58
318400011210TNTE01R- ERRORE. Tariffa/Offerta inesistente                                  59
318500970117TNTE01R- ERRORE. Tipo tariffa non gestibile                                   60
318600970117TNTE01R- ERRORE. In tariffa esistono più di 10 scaglioni                      61
318700970117TNTE01R- ERRORE. Il cod.tass XXX-XX ha scaglioni diversi dal primo cod.tass.  62
318800970117TNTE01R- ERRORE. In tariffa esistono scaglioni con indicazione del C.A.P.     63
318900970117TNTE01R- ERRORE. In tariffa non esistono righe dettaglio x la linea richiesta 64
319000011011TNTE01R- ERRORE. Divisa attualmente non accettabile                           65
319100020213TNTE01R- ERRORE. Utilizzo del CAT impossibile per tariffe/offerte FedEX       66
319200021104TNTE01R- Per soluzione "B" etichetta solo il cliente                          67
319300021104TNTE01R- Per procedura Disk "C" etichettano l'Autotrasp. o il cliente         68
319400021104TNTE01R- Per non procedura Disk "C" etichettano l'Autotrasp. o la Coop        69
319500970117TNTE01R- Record momentaneamente allocato, riprovare più tardi                 70
319600040802TNTE01R- La % fra bancali Bartolini e oltre 20 colli non può essere > di 100  71
319700050805TNTE01R- Inserire S per inglobare destinatario disagiato, N per non iglobarlo 72
319800040824TNTE01R- Se immessa %sped ... obbligatorio nr. medio bancali e viceversa      73
319900050225I dati di SEDE sono + recenti di quelli di FILIALE! Ricordarsi d'aggiornarli  74
320000050808TNTE01R- Inserire S oppure N per packing list                                 75
320100050802TNTE01R- Inserire S per inglobare bancali da rendere, N per non iglobarli     76
320200050808TNTE01R- Per inglobare i bancali da rendere inserire percentuale e il numero  77
320300050805TNTE01R- Per inglobare il destinatario disagiato inserire la percentuale      78
320400060313TNTE01R- Non è possibile inserire entrambi i tipi di assicurazione            79
320500060825Il codice tariffa scelto incongruente con quello originale                    80
320600131121TNTE01R- La % di CONSEGNA ISOLA e FERMO DEPOSITO non può essere > di 100      81
320700131121TNTE01R- La % di RITIRO ISOLA e MERCE PORTATA A MAG. non può essere > di 100  82
320800131121TNTE01R- La % di su APPUNTAMENTO e ai SUPERMARCATI non può essere > di 100    83
320900150227TNTE01R- Se immessa %sped ... obbligatorio il tipo bancali                    84
321000151119TNTE01R- Inserire S oppure N per ritiro telefonico / mail                     85
321100151119TNTE01R- Inserire S oppure N per inglobare al porto                           86
