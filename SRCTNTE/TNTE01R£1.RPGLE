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
007801150227     D*------------------
007802150227     D* DS RIDEFINIZIONE TABELLA "TBA"
007803150227     D*------------------
007804150227     D DTBA          E DS
007805150227     d comtba          s                   like(§etctba)
007806150227     D*-------------------
007807150227     D Detcflo       E DS
007900970117     D*-------------------
008000970117     D* DS TIPI SERVIZIO
008100970117     D*-------------------
008200970117     D DS5E          E DS
008300970117     D*-------------------
008400970117     D* DS TIPI TARIFFA
008500970117     D*-------------------
008600970117     D DSTR          E DS
008700970117     D*-------------------
008800020923     D* DS REPERIMENTO DATI UTENTE
008900970117     D*-------------------
009000091103     D Tnte04DS      E DS                                                       *Profili utente
009100091103     D TIBS34DS      E DS                                                       *Profili utente
009200020923     D DDATIUTE      E DS                                                       *Dati utente
009300020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009400970117     D*-------------------
009500970117     D* DS PASSAGGIO PARAMETRI
009600970117     D*-------------------
009700980430     D DSTE00        E DS                  EXTNAME(TNTE00DS)
009800980508     D*------------------
009900980430     D* DS RICERCA DATI TIPO
010000980508     D*------------------
010100980430     D DSTE30        E DS                  EXTNAME(TNTE30DS)
010200021202     D*------------------
010300021202     D* DS REPERIMENTO DIVISA TARIFFE
010400021202     D*------------------
010500021202     D DSTA01        E DS
010600021202     D*-------------------
010700021202     D* DS *PGM REPIMEMTO TARIFFE DI CARTELLO
010800021202     D*-------------------
010900021202     D TRUL27DS      E DS
011000970117     D*------------------
011100970117     D* DS "XSRDA8" - CONTROLLA DATA (8)
011200970117     D*------------------
011300970117     D WLBDA8          DS
011400970117     D  G08DAT                 1      8  0
011500970117     D  G08INV                 9     16  0
011600970117     D  G08ERR                17     17
011700970117     D  G08TGI                18     22  0
011800970117     D*-------------------
011900970117     D* ARCHITETTURA
012000970117     D*-------------------
012100970117     D KPJBA         E DS
012200970117     D*------------------
012300970117     D* DS SCOMPOSIZIONE MESSAGGIO DI ERRORE
012400970117     D*------------------
012500970117     D* MSG,62
012600970117     D                 DS
012700970117     D  ERRLNP                30     32
012800970117     D  ERRCTS                34     35
012900970117     D  ERRM62                 1     78
013000011011     D*------------------
013100011011     D* DS LETTURA TABELLA "GED" TIPO RECORD "CE"
013200011011     D*------------------
013300011011     D DSGEDCE       E DS
013400011008     D*------------------
013500011008     D* VARIABILI DI WRK
013600011008     D*------------------
013700011008     D  NUMDEC         S              9  9 INZ(*zeros)
013800040824     D  com100         S                   like(v4csban)
013900050225     D  ok             S              1    inz(*off)
014000090611     D  costs          S             25
014100011030     D*-------------------
014200011030     D* DS TABELLA "CT"
014300011030     D*-------------------
014400011030     D DSCT          E DS
014500020729     D*-------------------
014600020729     D* DS RIDEFINIZIONE DESCRIZIONE 143 ORGANIGRAMMA
014700020729     D*-------------------
014800020729     D OG143         E DS
014900020729     D*------------------
015000020729     D* DS PER LETTURA TABELLA "NTW"
015100020729     D*------------------
015200020729     D DNTW          E DS
015300970117     C*------------------------------------------------------------------------*
015400970117     C* MAIN LINE
015500970117     C*------------------------------------------------------------------------*
015600970117     C*
015700970117     C* OPERAZIONI INIZIALI
015800970117     C                   EXSR      RUTINZ
015900970117     C*
016000970117     C* ERRORI IN ENTRATA, ESCE DAL PGM
016100970117IF  1C     $FINE         IFNE      'S'
016200970117     C*
016300970117     C* IMPOSTAZIONI PRIMA VIDEATA
016400970117     C                   EXSR      IMPVD1
016500970117     C*
016600970117     C* CICLO FINO A RICHIESTA DI FINE ELABORAZIONE
016700970117DO  2C     $FINE         DOUEQ     'S'
016800970117     C     TIPVID        CASEQ     '1'           GESVD1                         *1° VIDEO
016900970117     C     TIPVID        CASEQ     '2'           GESVD2                         *2° VIDEO
017000970117     C     TIPVID        CASEQ     '3'           GESVD3                         *3° VIDEO
017100970117     C     TIPVID        CASEQ     '4'           GESVD4                         *4° VIDEO
017200970117     C     TIPVID        CASEQ     '5'           GESVD5                         *5° VIDEO
017300970117     C                   ENDCS
017400970117E   2C                   ENDDO
017500970117E   1C                   ENDIF
017600970117     C*
017700970117     C                   SETON                                        LR
017800970117     C*------------------------------------------------------------------------*
017900970117     C* IMPVD1 - IMPOSTAZIONI PRIMA VIDEATA
018000970117     C*------------------------------------------------------------------------*
018100970117     C     IMPVD1        BEGSR
018200970117     C*
018300050225     C                   eval      ok = *off                                     MESSAGGIO
018400970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI
018500970117     C                   EXSR      SETVD1
018600970117     C                   MOVEL     '1'           TIPVID                         *GESTIONE VIDEO
018700970117     C*
018800970117     C                   ENDSR
018900970117     C*------------------------------------------------------------------------*
019000970117     C* SETVD1 - IMPOSTAZIONI INDICATORI PRIMA VIDEATA
019100970117     C*------------------------------------------------------------------------*
019200970117     C     SETVD1        BEGSR
019300060825     c*
019400980429     C                   SETOFF                                       181920     ERRORI
019500980429     C                   SETOFF                                       21
019600970117     C                   SETOFF                                       90         GENERICO DI ERRORE
019700970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
019800970117     C*
019900970117     C                   ENDSR
020000970117     C*------------------------------------------------------------------------*
020100970117     C* GESVD1 - GESTIONE PRIMA VIDEATA
020200970117     C*------------------------------------------------------------------------*
020300970117     C     GESVD1        BEGSR
020400970117     C*
020500970117     C* EMISSIONE VIDEATA
020600970117     C                   WRITE     TE01D00                                      *TESTATA
020700970117     C                   EXFMT     TE01D01
020800970117     C                   EXSR      SETVD1
020900970117     C* F03-FINE
021000970117IF  1C     *INKC         IFEQ      *ON
021100970117     C                   EXSR      F03GES
021200970117     C                   GOTO      FINVD1
021300970117E   1C                   ENDIF
021400970117     C* F12-RITORNO
021500970117IF  1C     *INKL         IFEQ      *ON
021600970117     C                   EXSR      F12GES
021700970117     C                   GOTO      FINVD1
021800970117E   1C                   ENDIF
021900970117     C* CONTROLLA VIDEO
022000970117     C                   EXSR      CTRVD1
022100970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
022200970117     C                   GOTO      FINVD1
022300970117E   1C                   ENDIF
022400970117     C*
022500041105     C* LEGGE IL RECORD DA MANUTENZIONARE
022600041105     C                   EXSR      LETREC
022700970128     C*
022800970128     C* SE DATI TIPO INESISTENTI, PROPONE FINESTRA F05=CARICA DA ALTRA TARIFFA
022900091120     C                   MOVEL     'S'           KTCDSF                         *DATI SEDE/FILIALE
023000970128     C                   MOVEL     D00CTO        KTCCTO                         *TARIFFA O OFFERTA
023100970128     C                   Z-ADD     D00KSC        KTCKSC                         *CLIENTE P VISITA
023200970128     C                   Z-ADD     D00CTR        KTCCTR                         *TARIFFA
023300970128     C                   Z-ADD     D00PRG        KTCPRG                         *PROGRESSIVO
023400970128     C                   Z-ADD     V1CLNP        KTCLNP                         *LINEA DI PARTENZA
023500011001     C     KEYETC        SETLL     TEETC01L                               99
023600970128IF  1C     *IN99         IFEQ      *OFF
023700970128     C                   WRITE     TE01D00                                      *TESTATA
023800970128     C                   WRITE     TE01D06                                      *MESSAGGIO SF VUOTO
023900970128     C                   EXSR      F05GES
024000970128E   1C                   ENDIF
024100970117     C*
024200970117     C* EMETTE VIDEATA SUCCESSIVA
024300970117     C                   EXSR      IMPVD2
024400970117     C*
024500970117     C     FINVD1        ENDSR
024600970117     C*------------------------------------------------------------------------*
024700970117     C* CTRVD1 - CONTROLLA PRIMA VIDEATA
024800970117     C*------------------------------------------------------------------------*
024900970117     C     CTRVD1        BEGSR
025000970117     C*--------------------
025100970117     C* TARIFFA ITALIA/ESTERO
025200970117     C*--------------------
025300970117IF  1C     V1CFIE        IFNE      'I'
025400970117     C     V1CFIE        ANDNE     'E'
025500970117     C                   SETON                                        1990
025600970117     C                   MOVEL     MSG(1)        $MSG
025700970117     C                   GOTO      FINCT1
025800970117X   1C                   ELSE
025900970117     C                   MOVEL     V1CFIE        V0CFIE
026000970117IF  2C     V1CFIE        IFEQ      'I'
026100970117     C                   MOVEL     DDEC(1)       V1DFIE
026200970117     C                   MOVEL     DDEC(1)       V0DFIE
026300970117E   2C                   ENDIF
026400970117IF  2C     V1CFIE        IFEQ      'E'
026500970117     C                   MOVEL     DDEC(2)       V1DFIE
026600970117     C                   MOVEL     DDEC(2)       V0DFIE
026700970117E   2C                   ENDIF
026800970117E   1C                   ENDIF
026900970117     C*--------------------
027000970117     C* LINEA DI PARTENZA
027100970117     C*--------------------
027200970117     C                   MOVEL     V1CLNP        WLNP
027300970117     C                   MOVEL     *BLANKS       WDLNP
027400971204     C                   MOVEL     *BLANKS       WFL1
027500970117     C                   MOVEL     '0'           WERR
027600970117     C                   EXSR      CHKLNP
027700970117     C                   MOVEL     WDLNP         V1DLNP
027800980818IF  1C     WERR          IFEQ      '1'
027900970117     C                   SETON                                        2090
028000970117     C                   MOVEL     MSG(3)        $MSG
028100970117     C                   GOTO      FINCT1
028200970117E   1C                   ENDIF
028300971204IF  1C     WFL1          IFNE      *BLANKS                                      *ESTERO
028400970211     C                   SETON                                        2090
028500970211     C                   MOVEL     MSG(15)       $MSG
028600970211     C                   GOTO      FINCT1
028700970211E   1C                   ENDIF
028800970117IF  1C     V1CFIE        IFEQ      'I'
028900971204     C     WFL1          ANDNE     *BLANKS                                      *ESTERO
029000970117     C                   SETON                                        2090
029100970117     C                   MOVEL     MSG(16)       $MSG
029200970117     C                   GOTO      FINCT1
029300970117E   1C                   ENDIF
029400970314     C*--------------------
029500970314     C* DATA RIFERIMENTO CALCOLO
029600970314     C*--------------------
029700970317     C     *LIKE         DEFINE    V1CDTC        DTCV1C
029800970314     C                   Z-ADD     V1CDTC        G08DAT
029900970314     C                   Z-ADD     *ZEROS        G08INV
030000970314     C                   MOVEL     '0'           G08ERR
030100970314     C                   CALL      'XSRDA8'
030200970314     C                   PARM                    WLBDA8
030300970314     C                   Z-ADD     G08DAT        V1CDTC
030400970318     C                   Z-ADD     G08INV        DTCV1C
030500970314IF  1C     G08ERR        IFEQ      '1'
030600970314     C                   SETON                                        2190
030700970314     C                   MOVEL     MSG(47)       $MSG
030800970314     C                   GOTO      FINCT1
030900970314E   1C                   ENDIF
031000970117     C*--------------------
031100970117     C* DATI TIPO INESISTENTI
031200970117     C*--------------------
031300970117IF  1C     $TGES         IFEQ      'I'                                          *INTERROGAZIONE
031400091120     C                   MOVEL     'S'           KTCDSF                         *DATI SEDE/FILIALE
031500970117     C                   MOVEL     D00CTO        KTCCTO                         *TARIFFA O OFFERTA
031600980429     C                   Z-ADD     D00KSC        KTCKSC                         *CLIENTE O VISITA
031700970117     C                   Z-ADD     D00CTR        KTCCTR                         *TARIFFA
031800970117     C                   Z-ADD     D00PRG        KTCPRG                         *PROGRESSIVO
031900970117     C                   Z-ADD     V1CLNP        KTCLNP                         *LINEA DI PARTENZA
032000011001     C     KEYETC        SETLL     TEETC01L                               99
032100970117IF  2C     *IN99         IFEQ      *OFF                                         *NON TROVATO
032200970117     C                   SETON                                        2090
032300970117     C                   MOVEL     MSG(2)        $MSG
032400970117     C                   GOTO      FINCT1
032500970117E   2C                   ENDIF
032600970117E   1C                   ENDIF
032700970117     C*--------------------
032800970117     C* SCAGLIONI IN TARIFFA
032900970117     C*--------------------
033000970117     C* CARICA I PRIMI SCAGLIONI DEL DETTAGLIO TARIFFA DI UN CODICE TASSAZIONE
033100970117IF  1C     D00TLA        IFEQ      'D'                                          *DA CALCOLO DELTA
033200970117     C     *LIKE         DEFINE    TADORP        WORP
033300970117     C                   MOVEL     '0'           $TAD                           *ESISTENZA DETTAGLIO
033400970117     C                   CLEAR                   TSGL
033500970117     C                   Z-ADD     *ZEROS        I
033600970117     C                   Z-ADD     V0CKSC        KTDKSC
033700970117     C                   Z-ADD     V0CCTR        KTDCTR
033800970117     C                   Z-ADD     V0CPRG        KTDPRG
033900970117     C                   Z-ADD     V1CLNP        KTDLNP
034000990818     C     KEYTAD        SETLL     TITAD04L
034100990818     C     KEYTAD        READE     TITAD04L                               99
034200970117DO  2C     *IN99         DOWEQ     *OFF
034300970117     C                   MOVEL     TADORP        WORP
034400970117DO  3C     *IN99         DOWEQ     *OFF
034500970117     C     TADORP        ANDEQ     WORP
034600970117IF  4C     TADATB        IFEQ      *BLANKS                                      *NO ANNULLATI
034700970117     C                   MOVEL     '1'           $TAD                           *DETTAGLIO ESISTENTE
034800970117     C                   ADD       1             I
034900970117IF  5C     I             IFLE      10
035000970117     C                   Z-ADD     TADSGL        TSGL(I)
035100970117E   5C                   ENDIF
035200970117E   4C                   ENDIF
035300990818     C     KEYTAD        READE     TITAD04L                               99
035400970117E   3C                   ENDDO                                                  *FINE COD.TASSAZIONE
035500970117     C                   LEAVE
035600970117E   2C                   ENDDO                                                  *FINE DETTAGLIO
035700970117     C*---
035800970117     C* DETTAGLIO PER LA LINEA DI PARTENZA INESISTENTE, ERRORE E USCITA
035900970117     C*---
036000970117IF  2C     $TAD          IFEQ      '0'
036100970117     C                   SETON                                        2090
036200970117     C                   MOVEL     MSG(64)       $MSG
036300970117     C                   GOTO      FINCT1
036400970117E   2C                   ENDIF
036500970117     C* CONTROLLA SCAGLIONI
036600970117     C                   Z-ADD     V0CKSC        KTDKSC
036700970117     C                   Z-ADD     V0CCTR        KTDCTR
036800970117     C                   Z-ADD     V0CPRG        KTDPRG
036900970117     C                   Z-ADD     V1CLNP        KTDLNP
037000990818     C     KEYTAD        SETLL     TITAD04L
037100990818     C     KEYTAD        READE     TITAD04L                               99
037200970117DO  2C     *IN99         DOWEQ     *OFF
037300970117     C                   MOVEL     TADORP        WORP
037400970117     C                   Z-ADD     *ZEROS        I
037500970117DO  3C     *IN99         DOWEQ     *OFF
037600970117     C     TADORP        ANDEQ     WORP
037700970117IF  4C     TADATB        IFEQ      *BLANKS                                      *NO ANNULLATI
037800080616     C*---
037900080616     C* SCAGLIONI CON C.A.P., ERRORE E USCITA
038000080616     C*---
038100080616IF  5C     TADCAP        IFNE      *BLANKS
038200080616     C                   SETON                                        90
038300080616     C                   MOVEL     MSG(63)       D00MSG
038400080616     C                   MOVEL     '1'           D00ERR
038500080616     C                   MOVEL     'S'           $FINE                          *USCITA PGM
038600080616     C                   GOTO      FINCT1
038700080616E   5C                   ENDIF
038800080616     C*---
038900970117     C                   ADD       1             I
039000970117     C*---
039100970117     C* SCAGLIONI MAGGIORI DI 10, ERRORE E USCITA
039200970117     C*---
039300970117IF  5C     I             IFGT      10
039400970117     C                   SETON                                        90
039500970117     C                   MOVEL     MSG(61)       D00MSG
039600970117     C                   MOVEL     '1'           D00ERR
039700970117     C                   MOVEL     'S'           $FINE                          *USCITA PGM
039800970117     C                   GOTO      FINCT1
039900970117E   5C                   ENDIF
040000970117     C*---
040100970117     C* SCAGLIONI DIVERSI PER CODICE TASSAZIONE, ERRORE E USCITA
040200970117     C*---
040300970117IF  5C     TADSGL        IFNE      TSGL(I)
040400970117     C                   SETON                                        90
040500970117     C                   MOVEL     MSG(62)       ERRM62
040600970117     C                   MOVEL     TADLNP        ERRLNP
040700970117     C                   MOVEL     TADCTS        ERRCTS
040800970117     C                   MOVEL     ERRM62        D00MSG
040900970117     C                   MOVEL     '1'           D00ERR
041000970117     C                   MOVEL     'S'           $FINE                          *USCITA PGM
041100970117     C                   GOTO      FINCT1
041200970117E   5C                   ENDIF
041300970117E   4C                   ENDIF
041400990818     C     KEYTAD        READE     TITAD04L                               99
041500970117E   3C                   ENDDO                                                  *FINE COD.TASSAZIONE
041600970117E   2C                   ENDDO                                                  *FINE DETTAGLIO
041700970117E   1C                   ENDIF
041800970117     C*
041900970117     C     FINCT1        ENDSR
042000970117     C*------------------------------------------------------------------------*
042100970117     C* LETREC - LEGGE IL RECORD DA MANUTENZIONARE
042200970117     C*------------------------------------------------------------------------*
042300970117     C     LETREC        BEGSR
042400970117     C*
042500970117     C                   MOVEL     '0'           $REC
042600041105     c                   clear                   dataf             8 0
042700970117     C*
042800091120     C                   MOVEL     'S'           KTCDSF                         *DATI SEDE/FILIALE
042900970117     C                   MOVEL     D00CTO        KTCCTO
043000970117     C                   Z-ADD     V0CKSC        KTCKSC
043100970117     C                   Z-ADD     V0CCTR        KTCCTR
043200970117     C                   Z-ADD     V0CPRG        KTCPRG
043300970117     C                   Z-ADD     V1CLNP        KTCLNP
043400011001     C     KEYETC        CHAIN(N)  TEETC01L                           99
043500970117IF  1C     *IN99         IFEQ      *ON
043600970117     C                   MOVEL     'I'           $REC                           *DA INSERIRE
043700970117     C                   MOVEL     DOPE(1)       V0COPE                         *INSERIMENTO
043800970117X   1C                   ELSE
043900970117     C                   MOVEL     'M'           $REC                           *DA MODIFICARE
044000970117     C                   MOVEL     DOPE(2)       V0COPE                         *MANUTENZIONE
044100970117E   1C                   ENDIF
044200970117IF  1C     $TGES         IFEQ      'I'                                          *INTERROGAZIONE
044300970117     C                   MOVEL     *BLANKS       V0COPE                         *INSERIMENTO
044400970117E   1C                   ENDIF
044500051019     C* 89 indicatore a video per gestire "#" accanto alla domanda
044600050808     C* ingloba al porto. Se vengono attivate maggioraz. su voci con
044700050808     c* raggruppamento 001 la domanda vale solo per i ricavi, mentre
044800050808     c* per i costi no. Quindi attenzione se si aggiungono domande o
044900050808     c* si modificano voci già attive controllare i raggruppamenti!!!
045000050808     c     v1cfie        comp      'I'                                    89
045100050808     C* RIEMPE O AZZERA I CAMPI A VIDEO (DI TUTTI I FORMATI)
045200970117IF  1C     $REC          IFEQ      'I'
045300970117     C                   EXSR      AZZREC                                       *AZZERA
045400970117E   1C                   ENDIF
045500970117IF  1C     $REC          IFEQ      'M'
045600970117     C                   EXSR      IMPREC                                       *IMPOSTA
045700970117E   1C                   ENDIF
045800970117     C*
045900970117     C                   ENDSR
046000041105     C*------------------------------------------------------------------------*
046100041105     C* LETRECS- LEGGE IL RECORD analogo di SEDE
046200041105     C*------------------------------------------------------------------------*
046300041105     C     LETRECs       BEGSR
046400041105     C*
046500041105     c                   clear                   datas             8 0
046600041108     c                   clear                   comctr            1
046700041105     C                   MOVEL     'S'           KTCDSF                         *DATI SEDE/FILIALE
046800041105     C                   MOVEL     D00CTO        KTCCTO
046900041105     C                   Z-ADD     V0CKSC        KTCKSC
047000041105     C                   Z-ADD     V0CCTR        KTCCTR
047100041105     C                   Z-ADD     V0CPRG        KTCPRG
047200041105     C                   Z-ADD     V1CLNP        KTCLNP
047300041105     C     KEYETC        CHAIN(N)  TEETC01L
047400041105     c                   if        %found(teetc01l)
047500041105     c                   if        etcduv > etcdir
047600041105     c                   eval      datas = etcduv
047700041105     c                   else
047800041105     c                   eval      datas = etcdir
047900041105     c                   end
048000041105     c                   end
048100041105     C*
048200041105     C                   ENDSR
048300970117     C*------------------------------------------------------------------------*
048400970117     C* AZZREC - AZZERA I CAMPI A VIDEO -DI TUTTI I FORMATI-
048500970117     C*------------------------------------------------------------------------*
048600970117     C     AZZREC        BEGSR
048700970117     C*
048800980504     C                   MOVEL     *BLANKS       V2CDCV
048900980504     C                   MOVEL     *BLANKS       V2CFAR
049000970317     C                   Z-ADD     *ZEROS        V2CIAR
049100970117     C                   MOVEL     *BLANKS       V2CCLV
049200970117     C                   MOVEL     *BLANKS       V2CBOL
049300021031     C                   MOVEL     *BLANKS       V2CDKC
049400021031     C                   MOVEL     *BLANKS       V2CIX1
049500021031     C                   MOVEL     *BLANKS       V2CIX2
049600970117     C                   Z-ADD     *ZEROS        V2CGGP
049700970117     C                   Z-ADD     *ZEROS        V2CSPR
049800970117     C                   Z-ADD     *ZEROS        V2CPMC
049900970117     C                   Z-ADD     *ZEROS        V2CVMC
050000970117     C                   Z-ADD     *ZEROS        V2CFRA
050100970117     C                   Z-ADD     *ZEROS        V2CASS
050200970117     C                   Z-ADD     *ZEROS        V2CPCV
050300970117     C                   Z-ADD     *ZEROS        V2CPCM
050400970117     C                   Z-ADD     *ZEROS        V2CPFD
050500040730     C                   Z-ADD     *ZEROS        V2Cval
050501151119     C                   movel     ' '           v2crtm
050502151119     C                   movel     'N'           v2cirp
050700970117     C                   Z-ADD     *ZEROS        V3CPPV                         *TERZA VIDEATA
050800970117     C                   MOVEL     'N'           V3CSIN
050900970117     C                   Z-ADD     *ZEROS        V3CESP
051000970117     C                   MOVEL     'N'           V3CIES
051100970117     C                   Z-ADD     *ZEROS        V3CPGI
051200970424     C                   Z-ADD     *ZEROS        V3CGGG
051300970117     C                   MOVEL     'N'           V3CIGI
051400150227     C                   Z-ADD     *zeros        V4Ctba                         *QUARTA VIDEATA
051401150227     c                   clear                   v4dtba
051402150227     C                   Z-ADD     *ZEROS        V4CPAB                         *QUARTA VIDEATA
051500970117     C                   Z-ADD     *ZEROS        V4CPAM
051600970117     C                   MOVEL     'N'           V4CICA
051700970117     C                   Z-ADD     *ZEROS        V4CCAS
051800011009     C                   MOVEL     D00DIV        V4CDIC
051900970117     C                   Z-ADD     *ZEROS        V4CPAR
052000970117     C                   Z-ADD     *ZEROS        V4CPAC
052100060314     C                   Z-ADD     *ZEROS        V4Cacb
052200970117     C                   MOVEL     'N'           V4CIAS
052300970117     C                   Z-ADD     *ZEROS        V4CASC
052400011009     C                   MOVEL     D00DIV        V4CDIA
052500040811     C                   Z-ADD     *ZEROS        V4Csbnb
052600040824     C                   Z-ADD     *ZEROS        V4Csban
052700050802     C                   MOVEL     'N'           V4CIba
052800040811     C                   Z-ADD     *ZEROS        V4Cnbn
052900040802     C                   Z-ADD     *ZEROS        V4Cnba
052901151119     C                   movel     ' '           v5cpkl
052902151119     C                   movel     'N'           v5cipk
053000970117     C                   Z-ADD     *ZEROS        V5CPCA                         *QUINTA VIDEATA
053100970117     C                   MOVEL     'N'           V5CIAP
053200970117     C                   Z-ADD     *ZEROS        V5CPCP
053300970117     C                   MOVEL     'N'           V5CIPI
053400970117     C                   Z-ADD     *ZEROS        V5CPCS
053500970117     C                   MOVEL     'N'           V5CISU
053600970117     C                   Z-ADD     *ZEROS        V5CPCF
053700970117     C                   MOVEL     'N'           V5CIFU
053800970414     C                   Z-ADD     *ZEROS        V5CPCD
053900970414     C                   MOVEL     'N'           V5CIDD
054000970414     C                   Z-ADD     *ZEROS        V5CPCC
054100970414     C                   MOVEL     'N'           V5CICE
054200970414     C                   Z-ADD     *ZEROS        V5CPCL
054300970414     C                   MOVEL     'N'           V5CILO
054400011009     C                   Z-ADD     *ZEROS        V5CCIS
054500011009     C                   MOVEL     'N'           V5CISO
054600011009     C                   Z-ADD     *ZEROS        V5CSAM
054700011009     C                   MOVEL     'N'           V5CINR
054800040802     C                   Z-ADD     *ZEROS        V5Cdes
054900050808     C                   MOVEL     'N'           V5CIDE
055000970117     C*
055100970117     C                   ENDSR
055200970117     C*------------------------------------------------------------------------*
055300970117     C* IMPREC - IMPOSTA I CAMPI A VIDEO -DI TUTTI I FORMATI-
055400970117     C*------------------------------------------------------------------------*
055500970117     C     IMPREC        BEGSR
055600970117     C*
055700150227     C                   MOVEL     ETCflo        detcflo
055702150227     C                   MOVEL     ETCDCV        V2CDCV
055800980504     C                   MOVEL     ETCFAR        V2CFAR
055900970314     C                   Z-ADD     ETCIAR        V2CIAR
056000970117     C                   MOVEL     ETCCLV        V2CCLV
056100970117     C                   MOVEL     ETCBOL        V2CBOL
056200021031     C                   MOVEL     ETCDKC        V2CDKC
056300021031     C                   MOVEL     ETCIX1        V2CIX1
056400021031     C                   MOVEL     ETCIX2        V2CIX2
056500970117     C                   Z-ADD     ETCGGP        V2CGGP
056600970117     C                   Z-ADD     ETCSPR        V2CSPR
056700970117     C                   Z-ADD     ETCPMC        V2CPMC
056800970117     C                   Z-ADD     ETCVMC        V2CVMC
056900970117     C                   Z-ADD     ETCFRA        V2CFRA
057000970117     C                   Z-ADD     ETCASS        V2CASS
057100970117     C                   Z-ADD     ETCPCV        V2CPCV
057200970117     C                   Z-ADD     ETCPCM        V2CPCM
057300970117     C                   Z-ADD     ETCPFD        V2CPFD
057400040730     C                   Z-ADD     ETCval        V2Cval
057401151119     C                   movel     §ETCrtm       v2crtm
057404151119     C                   movel     §ETCirp       v2cirp
057600970117     C                   Z-ADD     ETCPPV        V3CPPV                         *TERZA VIDEATA
057700970117     C                   MOVEL     ETCSIN        V3CSIN
057800970117     C                   Z-ADD     ETCESP        V3CESP
057900970117     C                   MOVEL     ETCIES        V3CIES
058000970117     C                   Z-ADD     ETCPGI        V3CPGI
058100970117     C                   Z-ADD     ETCGGG        V3CGGG
058200021031     C                   Z-ADD     ETCNRG        V3CNRG
058300970117     C                   MOVEL     ETCIGI        V3CIGI
058301150227     c                   if        §etctba <> ' '
058302150227     c                   MOVE      §ETCtba       V4ctba
058303150302     c                   MOVE      §ETCtba       comtba
058305150305     c                   exsr      cnttba
058307150227     c                   else
058308150227     c                   clear                   v4ctba
058309150227     c                   clear                   comtba
058310150227     c                   end
058400970117     C                   Z-ADD     ETCPAB        V4CPAB                         *QUARTA VIDEATA
058500970117     C                   Z-ADD     ETCPAM        V4CPAM
058600970117     C                   MOVEL     ETCICA        V4CICA
058700970117     C                   Z-ADD     ETCCAS        V4CCAS
058800011009     C                   MOVEL     D00DIV        V4CDIC
058900060313     C                   Z-ADD     ETCxx1        V4Cacb
059000060313     C                   Z-ADD     ETCPAR        V4CPAR
059100970117     C                   Z-ADD     ETCPAC        V4CPAC
059200970117     C                   MOVEL     ETCIAS        V4CIAS
059300970117     C                   Z-ADD     ETCASC        V4CASC
059400011009     C                   MOVEL     D00DIV        V4CDIA
059500040811     C                   Z-ADD     ETCbnb        V4Csbnb
059600040824     C                   Z-ADD     ETCban        V4Csban
059700050802     C                   MOVEL     ETCIba        V4CIba
059800040811     C                   Z-ADD     ETCnbn        V4Cnbn
059900040802     C                   Z-ADD     ETCnba        V4Cnba
059901151119     C                   movel     ETCPkl        v5cpkl
059902151119     C                   movel     ETCiva        v5cipk
060000970117     C                   Z-ADD     ETCPCA        V5CPCA                         *QUINTA VIDEATA
060100970117     C                   MOVEL     ETCIAP        V5CIAP
060200970117     C                   Z-ADD     ETCPCP        V5CPCP
060300970117     C                   MOVEL     ETCIPI        V5CIPI
060400970117     C                   Z-ADD     ETCPCS        V5CPCS
060500970117     C                   MOVEL     ETCISU        V5CISU
060600970117     C                   Z-ADD     ETCPCF        V5CPCF
060700970117     C                   MOVEL     ETCIFU        V5CIFU
060800970414     C                   Z-ADD     ETCPCD        V5CPCD
060900970414     C                   MOVEL     ETCIDD        V5CIDD
061000970414     C                   Z-ADD     ETCPCC        V5CPCC
061100970414     C                   MOVEL     ETCICE        V5CICE
061200970414     C                   Z-ADD     ETCPCL        V5CPCL
061300970414     C                   MOVEL     ETCILO        V5CILO
061400011009     C                   Z-ADD     ETCCIS        V5CCIS
061500011009     C                   MOVEL     ETCISO        V5CISO
061600011009     C                   Z-ADD     ETCSAM        V5CSAM
061700011009     C                   MOVEL     ETCINR        V5CINR
061800040802     C                   Z-ADD     ETCdes        V5Cdes
061900050808     C                   MOVEL     ETCIDE        V5CIDE
062000041105     c                   if        etcduv > etcdir
062100041105     c                   eval      dataf = etcduv
062200041105     c                   else
062300041105     c                   eval      dataf = etcdir
062400041105     c                   end
062500970117     C*
062600970117     C                   ENDSR
062700970117     C*------------------------------------------------------------------------*
062800970117     C* IMPVD2 - IMPOSTAZIONE SECONDA VIDEATA
062900970117     C*------------------------------------------------------------------------*
063000970117     C     IMPVD2        BEGSR
063100970117     C*
063200970117     C* REIMPOSTA INDICATORI
063300970117     C                   EXSR      SETVD2
063400970117     C                   MOVEL     '2'           TIPVID
063500970117     C*
063600970117     C                   Z-ADD     V1CLNP        V2CLNP
063700970117     C                   MOVEL     V1DLNP        V2DLNP
063800970314     C                   Z-ADD     V1CDTC        V2CDTC
063900970117     C*
064000970117     C                   WRITE     TE01D00                                      *TESTATA
064100970117     C*
064200970117     C                   ENDSR
064300970117     C*------------------------------------------------------------------------*
064400970117     C* SETVD2 - IMPOSTAZIONI INDICATORI SECONDA VIDEATA
064500970117     C*------------------------------------------------------------------------*
064600970117     C     SETVD2        BEGSR
064700970117     C*
064800970117     C                   SETOFF                                       222324     ERRORI
064900970117     C                   SETOFF                                       252627
065000970117     C                   SETOFF                                       282930
065100970117     C                   SETOFF                                       313233
065200980504     C                   SETOFF                                       343566
065300021104     C                   SETOFF                                       677581
065400041108     C                   SETOFF                                       828397
065500050808     C                   SETOFF                                       1385
065600970117     C                   SETOFF                                       90         GENERICO DI ERRORE
065700970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
065800970117IF  1C     D00TLA        IFEQ      'D'                                          *CALCOLO DELTA
065900970117     C                   SETON                                        02        *ND ALCUNI CAMPI
066000970117X   1C                   ELSE                                                   *CALCOLO DELTA
066100970117     C                   SETOFF                                       02        *NESSUNA ND
066200970117E   1C                   ENDIF
066300970117     C*
066400970117     C                   ENDSR
066500970117     C*------------------------------------------------------------------------*
066600970117     C* GESVD2 - GESTIONE SECONDA VIDEATA
066700970117     C*------------------------------------------------------------------------*
066800970117     C     GESVD2        BEGSR
066900970117     C*
067000970117     C* EMISSIONE VIDEATA
067100970117     C                   WRITE     TE01D00
067200970117     C                   EXFMT     TE01D02
067300970117     C                   EXSR      SETVD2
067400970117     C* F03-FINE
067500970117IF  1C     *INKC         IFEQ      *ON
067600970117     C                   EXSR      F03GES
067700970117     C                   GOTO      FINVD2
067800970117E   1C                   ENDIF
067900970117     C* F05-CARICA DA ALTRA TARIFFA
068000970117IF  1C     *INKE         IFEQ      *ON
068100970117     C                   EXSR      F05GES
068200970117     C                   GOTO      FINVD2
068300970117E   1C                   ENDIF
068400970117     C* CONTROLLA VIDEO
068500970117     C                   EXSR      CTRVD2
068600970117IF  1C     *IN90         IFEQ      *ON
068700970117     C                   GOTO      FINVD2
068800970117E   1C                   ENDIF
068900970117     C* F12-RITORNO
069000970117IF  1C     *INKL         IFEQ      *ON
069100970117     C                   EXSR      IMPVD1
069200970117     C                   GOTO      FINVD2
069300970117E   1C                   ENDIF
069400970117     C* F06-AGGIORNA E PROSEGUE
069500970117IF  1C     *INKF         IFEQ      *ON
069600970117     C                   EXSR      F06GES
069700970117     C                   GOTO      FINVD2
069800970117E   1C                   ENDIF
069900970117     C*
070000970117     C* EMETTE VIDEATA SUCCESSIVA
070100970117     C                   EXSR      IMPVD3
070200970117     C*
070300970117     C     FINVD2        ENDSR
070400970117     C*------------------------------------------------------------------------*
070500970117     C* CTRVD2 - CONTROLLA SECONDA VIDEATA
070600970117     C*------------------------------------------------------------------------*
070700970117     C     CTRVD2        BEGSR
070800041105     C*--------------------
070900041105     C* DESCRIZIONE
071000041105     C*--------------------
071100980504IF  1C     V2CDCV        IFEQ      *BLANKS
071200980504     C                   SETON                                        7590
071300980504     C                   MOVEL     MSG(44)       $MSG
071400980504     C                   GOTO      FINCT2
071500980504E   1C                   ENDIF
071600970314     C*--------------------
071700970314     C* TIPO E IMPORTO ARROTONDAMENTO
071800970314     C*--------------------
071900970317IF  1C     V2CFAR        IFNE      *BLANKS
072000970314     C     V2CFAR        ANDNE     'E'
072100970314     C     V2CFAR        ANDNE     'D'
072200970314     C     V2CFAR        ANDNE     'A'
072300970314     C                   SETON                                        6690
072400970314     C                   MOVEL     MSG(48)       $MSG
072500970314     C                   GOTO      FINCT2
072600970317E   1C                   ENDIF
072700970314     C*
072800970317IF  1C     V2CFAR        IFNE      *BLANKS
072900970318     C     V2CIAR        ANDGT     *ZEROS
073000970318     C     V2CFAR        OREQ      *BLANKS
073100011008     C     V2CIAR        ANDEQ     *ZEROS
073200011008     C* Se trattasi di divisa ITL controllo che non abbiano inserito decimali
073300011008     C     D00DIV        IFEQ      'ITL'
073400011008     C                   Z-ADD     V2CIAR        NUMDEC
073500011008     C     NUMDEC        IFGT      *ZEROS
073600011008     C                   SETON                                        6790
073700011008     C                   MOVEL     MSG(52)       $MSG
073800011008     C                   GOTO      FINCT2
073900011008     C                   ENDIF
074000011008     C                   ENDIF
074100011010     C* Se trattasi di divisa EUR controllo che siano valori ben determinati
074200011010     C     D00DIV        IFEQ      'EUR'
074300011010     C     V2CIAR        IFGT      *ZEROS
074400011030     C     V2CIAR        IFEQ      0,10
074500011030     C     V2CIAR        OREQ      0,25
074600011010     C     V2CIAR        OREQ      0,50
074700011010     C     V2CIAR        OREQ      0,75
074800011010     C     V2CIAR        OREQ      1,00
074900011010     C                   ELSE
075000011010     C                   SETON                                        6790
075100011010     C                   MOVEL     MSG(55)       $MSG
075200011010     C                   GOTO      FINCT2
075300011010     C                   ENDIF
075400011010     C                   ENDIF
075500011010     C                   ENDIF
075600011010     C*
075700970318X   1C                   ELSE
075800970314     C                   SETON                                        666790
075900970314     C                   MOVEL     MSG(49)       $MSG
076000970314     C                   GOTO      FINCT2
076100970314E   1C                   ENDIF
076200970117     C*--------------------
076300970117     C* QUALITA' CLIENTE
076400970117     C*--------------------
076500980521     C                   MOVEL     '0'           WERR
076600980521     C                   MOVEL     V2CCLV        WCLV
076700980521     C                   EXSR      CHKCLV
076800980521     C                   MOVEL     WCLV          V2CCLV
076900980521IF  1C     WERR          IFEQ      '1'
077000970117     C                   SETON                                        2290
077100970117     C                   MOVEL     MSG(5)        $MSG
077200970117     C                   GOTO      FINCT2
077300980521E   1C                   ENDIF
077400970117     C*--------------------
077500970117     C* PASSAGGIO DATI IN BOLLETTAZIONE
077600970117     C*--------------------
077700970117IF  1C     V2CBOL        IFEQ      'A'
077800970117     C     V2CBOL        OREQ      'B'
077900970117     C     V2CBOL        OREQ      *BLANKS
078000970117X   1C                   ELSE
078100970117     C                   SETON                                        2390
078200970117     C                   MOVEL     MSG(6)        $MSG
078300970117     C                   GOTO      FINCT2
078400970117E   1C                   ENDIF
078500021104     C*--------------------
078600021104     C* PROCEDURA DISCK "C"
078700021104     C*--------------------
078800021104IF  1C     V2CDKC        IFEQ      'S'
078900021104     C     V2CDKC        OREQ      *BLANKS
079000021104X   1C                   ELSE
079100021104     C                   SETON                                        8190
079200021104     C                   MOVEL     MSG(57)       $MSG
079300021104     C                   GOTO      FINCT2
079400021104E   1C                   ENDIF
079500021104     C*--------------------
079600021104     C* PASSAGGIO DATI IN BOLLETTAZIONE (SOL. "B") / PROCEDURA DISCK "C"
079700021104     C*--------------------
079800021104     C                   IF        V2CBOL='B' AND V2CDKC='S'
079900021104     C                   SETON                                        238190
080000021104     C                   MOVEL     MSG(58)       $MSG
080100021104     C                   GOTO      FINCT2
080200021104     C                   ENDIF
080300021031     C*--------------------
080400021104     C* PASSAGGIO DATI IN BOLLETTAZIONE (SOL. "B") / CHI ETICHETTA
080500021031     C*--------------------
080600021104     C                   IF        V2CBOL='B' AND V2CIX1<>'K'
080700021031     C                   SETON                                        238290
080800021104     C                   MOVEL     MSG(67)       $MSG
080900021031     C                   GOTO      FINCT2
081000021031     C                   ENDIF
081100021031     C*--------------------
081200021031     C* CHI ETICHETTA
081300021031     C*--------------------
081400021031IF  1C     V2CIX1        IFEQ      'K'
081500021031     C     V2CIX1        OREQ      'C'
081600021031     C     V2CIX1        OREQ      *BLANKS
081700021031X   1C                   ELSE
081800021031     C                   SETON                                        8290
081900021104     C                   MOVEL     MSG(57)       $MSG
082000021031     C                   GOTO      FINCT2
082100021031E   1C                   ENDIF
082200021031     C*--------------------
082300021031     C* PROCEDURA DISCK "C" / CHI ETICHETTA
082400021031     C*--------------------
082500021104     C                   IF        V2CBOL <> 'B'
082600021104     C                   IF        V2CDKC='S'
082700021104     C                   IF        V2CIX1=' ' OR  V2CIX1='K'
082800021104     C                   ELSE
082900021031     C                   SETON                                        818290
083000021104     C                   MOVEL     MSG(68)       $MSG
083100021031     C                   GOTO      FINCT2
083200021031     C                   ENDIF
083300021104     C                   ENDIF
083400021104     C* Eseguo doppio test x diversificare il messaggio d errore a video
083500021104     C                   IF        V2CDKC=*blanks
083600021104     C                   IF        V2CIX1=' ' OR V2CIX1='C'
083700021104     C                   ELSE
083800021104     C                   SETON                                        818290
083900021104     C                   MOVEL     MSG(69)       $MSG
084000021104     C                   GOTO      FINCT2
084100021104     C                   ENDIF
084200021104     C                   ENDIF
084300021104     C                   ENDIF
084400021031     C*--------------------
084500021031     C* CHI SCARICA
084600021031     C*--------------------
084700021031IF  1C     V2CIX2        IFEQ      'C'
084800021031     C     V2CIX2        OREQ      *BLANKS
084900021031X   1C                   ELSE
085000021031     C                   SETON                                        8390
085100021104     C                   MOVEL     MSG(57)       $MSG
085200021031     C                   GOTO      FINCT2
085300021031E   1C                   ENDIF
085400970117     C*--------------------
085500970117     C* SPEDIZIONI/RITIRO
085600970117     C*--------------------
085700970117IF  1C     V2CSPR        IFEQ      *ZEROS
085800970117     C                   SETON                                        2890
085900970117     C                   MOVEL     MSG(10)       $MSG
086000970117     C                   GOTO      FINCT2
086100970117E   1C                   ENDIF
086200970117     C*--------------------
086300970117     C* PESO MEDIO PER COLLO
086400970117     C*--------------------
086500970117IF  1C     V2CPMC        IFEQ      *ZEROS
086600970117     C                   SETON                                        2790
086700970117     C                   MOVEL     MSG(7)        $MSG
086800970117     C                   GOTO      FINCT2
086900970117E   1C                   ENDIF
087000970117     C*--------------------
087100970117     C* VOLUME MEDIO PER COLLO
087200970117     C*--------------------
087300970117IF  1C     V2CVMC        IFEQ      *ZEROS
087400970117     C                   SETON                                        3290
087500970117     C                   MOVEL     MSG(8)        $MSG
087600970117     C                   GOTO      FINCT2
087700970117E   1C                   ENDIF
087800970117     C*--------------------
087900970117     C* % FRANCO
088000970117     C*--------------------
088100980803IF  1C     V2CFRA        IFGT      *ZEROS
088200980803     C     V2CFRA        ANDNE     100
088300980803     C                   SETON                                        3090
088400980803     C                   MOVEL     MSG(50)       $MSG
088500980803     C                   GOTO      FINCT2
088600980803E   1C                   ENDIF
088700970117     C*--------------------
088800970117     C* % ASSEGNATO
088900970117     C*--------------------
089000980803IF  1C     V2CASS        IFGT      *ZEROS
089100980803     C     V2CASS        ANDNE     100
089200980803     C                   SETON                                        3190
089300980803     C                   MOVEL     MSG(50)       $MSG
089400980803     C                   GOTO      FINCT2
089500980803E   1C                   ENDIF
089600970117     C*--------------------
089700980803     C* % FRANCO E % ASSEGNATO
089800970117     C*--------------------
089900980803IF  1C     V2CFRA        IFEQ      *ZEROS
090000980803     C     V2CASS        ANDEQ     *ZEROS
090100980803     C     V2CFRA        ORGT      *ZEROS
090200980803     C     V2CASS        ANDGT     *ZEROS
090300980803     C                   SETON                                        303190
090400980803     C                   MOVEL     MSG(51)       $MSG
090500980803     C                   GOTO      FINCT2
090600980803E   1C                   ENDIF
090700970117     C*--------------------
090800970117     C* % CONSEGNA AI PRIVATI
090900970117     C*--------------------
091000970117IF  1C     V2CPCV        IFGT      100
091100970117     C                   SETON                                        3390
091200970117     C                   MOVEL     MSG(27)       $MSG
091300970117     C                   GOTO      FINCT2
091400970117E   1C                   ENDIF
091500970117     C*--------------------
091600011009     C* % MERCE A MAGAZZINO
091700970117     C*--------------------
091800970117IF  1C     V2CPCM        IFGT      100
091900970117     C                   SETON                                        3490
092000970117     C                   MOVEL     MSG(27)       $MSG
092100970117     C                   GOTO      FINCT2
092200131121     c                   else
092300131121     c     v5csam        add       v2cpcm        wn7
092400131121IF  1C     WN7           IFGT      100
092500131121     C                   SETON                                        3490
092600131121     C                   MOVEL     MSG(82)       $MSG
092700131121     C                   GOTO      FINCT2
092800131121E   1C                   ENDIF
092900970117E   1C                   ENDIF
093000970117     C*--------------------
093100970117     C* % CONSEGNA FERMO DEPOSITO
093200970117     C*--------------------
093300970117IF  1C     V2CPFD        IFGT      100
093400970117     C                   SETON                                        3590
093500970117     C                   MOVEL     MSG(27)       $MSG
093600970117     C                   GOTO      FINCT2
093700131120E   1C                   ELSE
093800131120     C* FERMO DEPOSITO + CONSEGNA ISOLA DEVE ESSERE <= 100
093900131120IF  1C     V2CPFD        ADD       V5CCIS        WN7
094000131120IF  1C     WN7           IFGT      100
094100131120     C                   SETON                                        3590
094200131120     C                   MOVEL     MSG(81)       $MSG
094300131120     C                   GOTO      FINCT2
094400131120E   1C                   ENDIF
094500131121E   1C                   ENDIF
094600040730     C*--------------------
094700040730     C* % merce di valore
094800040730     C*--------------------
094900040730IF  1C     V2Cval        IFGT      100
095000040730     C                   SETON                                        1390
095100040730     C                   MOVEL     MSG(27)       $MSG
095200040730     C                   GOTO      FINCT2
095300040730E   1C                   ENDIF
095400050808     C*--------------------
095500151119     C* ritiro telefonico / mail
095600050808     C*--------------------
095700151119IF  1C     V2Crtm        IFne      'S'
095800151119IF  1C     V2Crtm        andne     ' '
095900050808     C                   SETON                                        8590
096000151119     C                   MOVEL     MSG(85)       $MSG
096100050808     C                   GOTO      FINCT2
096200050808E   1C                   ENDIF
096201151119IF  1C     V2Crtm        IFeq      'S'
096202151119IF  1C     V2Cirp        IFne      'S'
096203151119IF  1C     V2Cirp        andne     'N'
096204151119     C                   SETON                                        8690
096205151119     C                   MOVEL     MSG(86)       $MSG
096206151119     C                   GOTO      FINCT2
096207151119E   1C                   ENDIF
096208151119E   1C                   ENDIF
096300970117     C*
096400970117     C     FINCT2        ENDSR
096500980521     C*------------------------------------------------------------------------*
096600980521     C* CHKCLV - CONTROLLA LA QUALITA' CLIENTE
096700980521     C*------------------------------------------------------------------------*
096800980521     C     CHKCLV        BEGSR
096900980521     C*
097000980521     C     *LIKE         DEFINE    V2CCLV        WCLV
097100980521     C*
097200980521     C* RICHIESTO ELENCO CODICI
097300980521     C     '?'           SCAN      WCLV                                   99
097400980521IF  1C     *IN99         IFEQ      *ON
097500020923     C                   Z-ADD     1             §KUT
097600980521     C                   MOVEL     'IC'          §COD
097700980521     C                   MOVEL     *BLANKS       §KEY
097800980521     C                   MOVEL     *BLANKS       §DES
097900980521     C                   CALL      'X§TABER'
098000980521     C                   PARM                    §KUT              1 0
098100980521     C                   PARM                    §COD              2
098200980521     C                   PARM                    §KEY              8
098300980521     C                   PARM                    §DES             30
098400980521     C                   MOVEL     §KEY          WCLV
098500980521X   1C                   ELSE
098600980521     C*
098700980521     C* CONTROLLA ESISTENZA CODICE IN ARCHIVIO
098800020923     C                   Z-ADD     1             KTBKUT
098900980521     C                   MOVEL     'IC'          KTBCOD
099000980521     C                   MOVEL(P)  WCLV          KTBKEY
099100980521     C     KEYTAB        CHAIN     TABEL00F                           99
099200980521IF  2C     *IN99         IFEQ      '1'
099300980521     C     TBLFLG        ORNE      *BLANKS
099400980521     C                   MOVEL     '1'           WERR
099500980521E   2C                   ENDIF
099600980521E   1C                   ENDIF
099700980521     C*
099800980521     C                   ENDSR
099900970117     C*------------------------------------------------------------------------*
100000970117     C* IMPVD3 - IMPOSTAZIONE TERZA VIDEATA
100100970117     C*------------------------------------------------------------------------*
100200970117     C     IMPVD3        BEGSR
100300970117     C*
100400970117     C* REIMPOSTA INDICATORI
100500970117     C                   EXSR      SETVD3
100600970117     C                   MOVEL     '3'           TIPVID
100700970117     C*
100800970117     C                   Z-ADD     V1CLNP        V3CLNP
100900970117     C                   MOVEL     V1DLNP        V3DLNP
101000970314     C                   Z-ADD     V1CDTC        V3CDTC
101100970117     C*
101200970117     C                   WRITE     TE01D00                                      *TESTATA
101300970117     C*
101400970117     C                   ENDSR
101500970117     C*------------------------------------------------------------------------*
101600970117     C* SETVD3 - IMPOSTAZIONI INDICATORI TERZA VIDEATA
101700970117     C*------------------------------------------------------------------------*
101800970117     C     SETVD3        BEGSR
101900970117     C*
102000970317     C                   SETOFF                                       5759       ERRORI
102100970117     C                   SETOFF                                       4445
102200970117     C                   SETOFF                                       545556
102300970117     C                   SETOFF                                       60
102400970117     C                   SETOFF                                       90         GENERICO DI ERRORE
102500970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
102600970117     C*
102700970117     C                   ENDSR
102800970117     C*------------------------------------------------------------------------*
102900970117     C* GESVD3 - GESTIONE TERZA VIDEATA
103000970117     C*------------------------------------------------------------------------*
103100970117     C     GESVD3        BEGSR
103200970117     C*
103300970117     C* EMISSIONE VIDEATA
103400970117     C                   WRITE     TE01D00                                      *TESTATA
103500970117     C                   EXFMT     TE01D03
103600970117     C                   EXSR      SETVD3
103700970117     C* F03-FINE
103800970117IF  1C     *INKC         IFEQ      *ON
103900970117     C                   EXSR      F03GES
104000970117     C                   GOTO      FINVD3
104100970117E   1C                   ENDIF
104200970117     C* CONTROLLA VIDEO
104300970117     C                   EXSR      CTRVD3
104400970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
104500970117     C                   GOTO      FINVD3
104600970117E   1C                   ENDIF
104700970117     C* F12-RITORNO
104800970117IF  1C     *INKL         IFEQ      *ON
104900970117     C                   EXSR      IMPVD2
105000970117     C                   GOTO      FINVD3
105100970117E   1C                   ENDIF
105200970117     C*
105300970117     C* EMETTE VIDEATA SUCCESSIVA
105400970117     C                   EXSR      IMPVD4
105500970117     C*
105600970117     C     FINVD3        ENDSR
105700970117     C*------------------------------------------------------------------------*
105800970117     C* CTRVD3 - CONTROLLA TERZA VIDEATA
105900970117     C*------------------------------------------------------------------------*
106000970117     C     CTRVD3        BEGSR
106100970117     C*---
106200970117     C* INOLTRO
106300970117     C*---
106400970117     C*--------------------
106500970117     C* % SPEDIZIONI IN PROVINCIA
106600970117     C*--------------------
106700970117IF  1C     V3CPPV        IFGT      100
106800970117     C                   SETON                                        5790
106900970117     C                   MOVEL     MSG(27)       $MSG
107000970117     C                   GOTO      FINCT3
107100970117E   1C                   ENDIF
107200970117     C*--------------------
107300970317     C* INGLOBA INOLTRO
107400970117     C*--------------------
107500970314IF  1C     V3CSIN        IFEQ      'S'
107600970314     C     V3CSIN        OREQ      'N'
107700970314X   1C                   ELSE
107800970314     C                   SETON                                        5990
107900970314     C                   MOVEL     MSG(40)       $MSG
108000970314     C                   GOTO      FINCT3
108100970314E   1C                   ENDIF
108200970314     C*
108300970314IF  1C     V3CPPV        IFEQ      *ZEROS
108400970314     C     V3CSIN        ANDEQ     'S'
108500970314     C                   SETON                                        575990
108600970314     C                   MOVEL     MSG(41)       $MSG
108700970314     C                   GOTO      FINCT3
108800970314E   1C                   ENDIF
108900970117     C*---
109000970117     C* ESPRESSO
109100970117     C*---
109200970117     C*--------------------
109300970117     C* % ESPRESSI
109400970117     C*--------------------
109500970117IF  1C     V3CESP        IFGT      100
109600970117     C                   SETON                                        4490
109700970117     C                   MOVEL     MSG(27)       $MSG
109800970117     C                   GOTO      FINCT3
109900970117E   1C                   ENDIF
110000970117     C*
110100970117IF  1C     V0CTSP        IFEQ      'E'
110200970117     C     V3CESP        ANDLT     100
110300970117     C                   SETON                                        4490
110400970117     C                   MOVEL     MSG(28)       $MSG
110500090611     c                   movel     costs         $msg
110600970117     C                   GOTO      FINCT3
110700970117E   1C                   ENDIF
110800970117     C*--------------------
110900970117     C* INGLOBA ESPRESSO
111000970117     C*--------------------
111100970117IF  1C     V3CIES        IFEQ      'S'
111200970117     C     V3CIES        OREQ      'N'
111300970117X   1C                   ELSE
111400970117     C                   SETON                                        4590
111500970117     C                   MOVEL     MSG(30)       $MSG
111600970117     C                   GOTO      FINCT3
111700970117E   1C                   ENDIF
111800970117     C*
111900970117IF  1C     V0CTSP        IFEQ      'E'
112000970117     C     V3CIES        ANDNE     'S'
112100970117     C                   SETON                                        4590
112200970117     C                   MOVEL     MSG(29)       $MSG
112300090611     c                   movel     costs         $msg
112400970117     C                   GOTO      FINCT3
112500970117E   1C                   ENDIF
112600970117     C*
112700970117IF  1C     V3CESP        IFEQ      *ZEROS
112800970117     C     V3CIES        ANDEQ     'S'
112900970117     C                   SETON                                        444590
113000970117     C                   MOVEL     MSG(31)       $MSG
113100970117     C                   GOTO      FINCT3
113200970117E   1C                   ENDIF
113300970117     C*---
113400970117     C* GIACENZA
113500970117     C*---
113600970117IF  1C     V3CPGI        IFGT      100
113700970117     C                   SETON                                        5490
113800970117     C                   MOVEL     MSG(27)       $MSG
113900970117     C                   GOTO      FINCT3
114000970117E   1C                   ENDIF
114100970117     C*--------------------
114200970117     C* GIORNI MEDI DI SOSTA
114300970117     C*--------------------
114400970117IF  1C     V3CPGI        IFGT      *ZEROS
114500970117IF  2C     V3CGGG        IFEQ      *ZEROS
114600970117     C                   SETON                                        5590
114700970117     C                   MOVEL     MSG(36)       $MSG
114800970117     C                   GOTO      FINCT3
114900970117E   2C                   ENDIF
115000970117X   1C                   ELSE
115100970117IF  2C     V3CGGG        IFGT      *ZEROS
115200970117     C                   SETON                                        5590
115300970117     C                   MOVEL     MSG(37)       $MSG
115400970117     C                   GOTO      FINCT3
115500970117E   2C                   ENDIF
115600970117E   1C                   ENDIF
115700970117     C*--------------------
115800970117     C* RIAPERTURE GIACENZA
115900970117     C*--------------------
116000970424IF  1C     V3CPGI        IFGT      *ZEROS                                       *SI %GIACENZE
116100970424     C                   Z-ADD     1             V3CNRG                         *FISSO UNA RIAPERTUR
116200970424X   1C                   ELSE                                                   *NO %GIACENZE
116300970424     C                   Z-ADD     *ZEROS        V3CNRG                         *NO RIAPERTURE
116400970424E   1C                   ENDIF
116500970424     C*--------------------
116600970117     C* INGLOBA GIACENZA
116700970117     C*--------------------
116800970117IF  1C     V3CIGI        IFEQ      'S'
116900970117     C     V3CIGI        OREQ      'N'
117000970117X   1C                   ELSE
117100970117     C                   SETON                                        5690
117200970117     C                   MOVEL     MSG(34)       $MSG
117300970117     C                   GOTO      FINCT3
117400970117E   1C                   ENDIF
117500970117     C*
117600970117IF  1C     V3CIGI        IFEQ      'S'
117700970117     C     V3CPGI        ANDEQ     *ZEROS
117800970117     C                   SETON                                        545690
117900970117     C                   MOVEL     MSG(35)       $MSG
118000970117     C                   GOTO      FINCT3
118100970117E   1C                   ENDIF
118200970117     C*
118300970117     C     FINCT3        ENDSR
118400970117     C*------------------------------------------------------------------------*
118500970117     C* IMPVD4 - IMPOSTAZIONE QUARTA VIDEATA
118600970117     C*------------------------------------------------------------------------*
118700970117     C     IMPVD4        BEGSR
118800970117     C*
118900970117     C* REIMPOSTA INDICATORI
119000970117     C                   EXSR      SETVD4
119100970117     C                   MOVEL     '4'           TIPVID
119200970117     C*
119300970117     C                   Z-ADD     V1CLNP        V4CLNP
119400970117     C                   MOVEL     V1DLNP        V4DLNP
119500970314     C                   Z-ADD     V1CDTC        V4CDTC
119600970117     C*
119700970117     C                   WRITE     TE01D00                                      *TESTATA
119800970117     C*
119900970117     C                   ENDSR
120000970117     C*------------------------------------------------------------------------*
120100970117     C* SETVD4 - IMPOSTAZIONI INDICATORI QUARTA VIDEATA
120200970117     C*------------------------------------------------------------------------*
120300970117     C     SETVD4        BEGSR
120400970117     C*
120500970117     C                   SETOFF                                       464748     ERRORI
120600040812     C                   SETOFF                                       491215
120700970117     C                   SETOFF                                       505152
120800040824     C                   SETOFF                                       531617
120900150227     C                   SETOFF                                       885818
121000970117     C                   SETOFF                                       90         GENERICO DI ERRORE
121100970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
121200970117     C*
121300970117     C                   ENDSR
121400970117     C*------------------------------------------------------------------------*
121500970117     C* GESVD4 - GESTIONE QUARTA VIDEATA
121600970117     C*------------------------------------------------------------------------*
121700970117     C     GESVD4        BEGSR
121800970117     C*
121900970117     C* EMISSIONE VIDEATA
122000970117     C                   WRITE     TE01D00                                      *TESTATA
122100970117     C                   EXFMT     TE01D04
122200970117     C                   EXSR      SETVD4
122300970117     C* F03-FINE
122400970117IF  1C     *INKC         IFEQ      *ON
122500970117     C                   EXSR      F03GES
122600970117     C                   GOTO      FINVD4
122700970117E   1C                   ENDIF
122800970117     C* CONTROLLA VIDEO
122900970117     C                   EXSR      CTRVD4
123000970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
123100970117     C                   GOTO      FINVD4
123200970117E   1C                   ENDIF
123300970117     C* F12-RITORNO
123400970117IF  1C     *INKL         IFEQ      *ON
123500970117     C                   EXSR      IMPVD3
123600970117     C                   GOTO      FINVD4
123700970117E   1C                   ENDIF
123800970117     C*
123900970117     C* EMETTE VIDEATA SUCCESSIVA
124000970117     C                   EXSR      IMPVD5
124100970117     C*
124200970117     C     FINVD4        ENDSR
124300970117     C*------------------------------------------------------------------------*
124400970117     C* CTRVD4 - CONTROLLA QUARTA VIDEATA
124500970117     C*------------------------------------------------------------------------*
124600970117     C     CTRVD4        BEGSR
124700970117     C*---
124800970117     C* CONTRASSEGNO
124900970117     C*---
125000970117     C*--------------------
125100970117     C* % CONTRASSEGNI BARTOLINI
125200970117     C*--------------------
125300970117IF  1C     V4CPAB        IFGT      100
125400970117     C                   SETON                                        4690
125500970117     C                   MOVEL     MSG(27)       $MSG
125600970117     C                   GOTO      FINCT4
125700970117E   1C                   ENDIF
125800970117     C*--------------------
125900970117     C* % CONTRASSEGNI MITTENTE
126000970117     C*--------------------
126100970117IF  1C     V4CPAM        IFGT      100
126200970117     C                   SETON                                        4790
126300970117     C                   MOVEL     MSG(27)       $MSG
126400970117     C                   GOTO      FINCT4
126500970117E   1C                   ENDIF
126600970117     C*--------------------
126700970117     C* % CONTRASSEGNI BARTOLINI + MITTENTE
126800970117     C*--------------------
126900970117     C     V4CPAB        ADD       V4CPAM        WN7               7 3
127000970117IF  1C     WN7           IFGT      100
127100970117     C                   SETON                                        464790
127200970117     C                   MOVEL     MSG(19)       $MSG
127300970117     C                   GOTO      FINCT4
127400970117E   1C                   ENDIF
127500970117     C*--------------------
127600970117     C* INGLOBA CONTRASSEGNO
127700970117     C*--------------------
127800970117IF  1C     V4CICA        IFEQ      'S'
127900970117     C     V4CICA        OREQ      'N'
128000970117X   1C                   ELSE
128100970117     C                   SETON                                        4890
128200970117     C                   MOVEL     MSG(20)       $MSG
128300970117     C                   GOTO      FINCT4
128400970117E   1C                   ENDIF
128500970117     C*
128600970117IF  1C     V4CICA        IFEQ      'S'
128700970117     C     V4CPAB        ANDEQ     *ZEROS
128800970117     C     V4CPAM        ANDEQ     *ZEROS
128900970117     C                   SETON                                        464748
129000970117     C                   SETON                                        90
129100970117     C                   MOVEL     MSG(21)       $MSG
129200970117     C                   GOTO      FINCT4
129300970117E   1C                   ENDIF
129400970117     C*--------------------
129500970117     C* VALORE MEDIO C/ASSEGNO
129600970117     C*--------------------
129700970117IF  1C     V4CPAB        IFGT      *ZEROS
129800970117     C     V4CPAM        ORGT      *ZEROS
129900970117IF  2C     V4CCAS        IFEQ      *ZEROS
130000970117     C                   SETON                                        4990
130100970117     C                   MOVEL     MSG(22)       $MSG
130200970117     C                   GOTO      FINCT4
130300970117E   2C                   ENDIF
130400011008     C* Se trattasi di divisa ITL controllo che non abbiano inserito decimali
130500011008     C     D00DIV        IFEQ      'ITL'
130600011008     C                   Z-ADD     V4CCAS        NUMDEC
130700011008     C     NUMDEC        IFGT      *ZEROS
130800011008     C                   SETON                                        4990
130900011008     C                   MOVEL     MSG(52)       $MSG
131000011008     C                   GOTO      FINCT4
131100011008     C                   ENDIF
131200011008     C                   ENDIF
131300011008     C*
131400970117X   1C                   ELSE
131500970117IF  2C     V4CCAS        IFGT      *ZEROS
131600970117     C                   SETON                                        4990
131700970117     C                   MOVEL     MSG(32)       $MSG
131800970117     C                   GOTO      FINCT4
131900970117E   2C                   ENDIF
132000970117E   1C                   ENDIF
132100970117     C*---
132200970117     C* ASSICURAZIONE
132300970117     C*---
132400970117     C*--------------------
132500970117     C* % ASSICURAZIONE R.C.V.
132600970117     C*--------------------
132700970117IF  1C     V4CPAR        IFGT      100
132800970117     C                   SETON                                        5090
132900970117     C                   MOVEL     MSG(27)       $MSG
133000970117     C                   GOTO      FINCT4
133100970117E   1C                   ENDIF
133200060313     c* rcv o ac base
133300060313IF  1C     V4CPAR        IFGT      0
133400060313IF  1C     V4Cacb        andgt     0
133500060313     C                   SETON                                        509058
133600060313     C                   MOVEL     MSG(79)       $MSG
133700060313     C                   GOTO      FINCT4
133800060313E   1C                   ENDIF
133900060313     C*--------------------
134000060313     C* % ASSICURAZIONE AC BASE
134100060313     C*--------------------
134200060313IF  1C     V4Cacb        IFGT      100
134300060313     C                   SETON                                        5890
134400060313     C                   MOVEL     MSG(27)       $MSG
134500060313     C                   GOTO      FINCT4
134600060313E   1C                   ENDIF
134700970117     C*--------------------
134800060313     C* % ASSICURAZIONE AC PLUS
134900970117     C*--------------------
135000970117IF  1C     V4CPAC        IFGT      100
135100970117     C                   SETON                                        5190
135200970117     C                   MOVEL     MSG(27)       $MSG
135300970117     C                   GOTO      FINCT4
135400970117E   1C                   ENDIF
135500970117     C*--------------------
135600970117     C* % ASSICURAZIONE R.C.V. + ASSICURAZIONE PER CONTO
135700970117     C*--------------------
135800060313     c                   if        (v4cpar + v4cpac + v4cacb) <> 100
135900970117     C                   SETON                                        505190
136000060313     C                   SETON                                        58
136100970117     C                   MOVEL     MSG(23)       $MSG
136200970117     C                   GOTO      FINCT4
136300970117E   1C                   ENDIF
136400970117     C*--------------------
136500970117     C* INGLOBA ASSICURAZIONE
136600970117     C*--------------------
136700970117IF  1C     V4CIAS        IFEQ      'S'
136800970117     C     V4CIAS        OREQ      'N'
136900970117X   1C                   ELSE
137000970117     C                   SETON                                        5290
137100970117     C                   MOVEL     MSG(24)       $MSG
137200970117     C                   GOTO      FINCT4
137300970117E   1C                   ENDIF
137400970117     C*
137500970117IF  1C     V4CIAS        IFEQ      'S'
137600970117     C     V4CPAR        ANDEQ     *ZEROS
137700970117     C     V4CPAC        ANDEQ     *ZEROS
137800970117     C                   SETON                                        505152
137900970117     C                   SETON                                        90
138000970117     C                   MOVEL     MSG(25)       $MSG
138100970117     C                   GOTO      FINCT4
138200970117E   1C                   ENDIF
138300970117     C*--------------------
138400970117     C* VALORE MEDIO IMPORTO DA ASSICURARE
138500970117     C*--------------------
138600970117IF  1C     V4CPAC        IFGT      *ZEROS
138700970117IF  2C     V4CASC        IFEQ      *ZEROS
138800970117     C                   SETON                                        5390
138900970117     C                   MOVEL     MSG(26)       $MSG
139000970117     C                   GOTO      FINCT4
139100970117E   2C                   ENDIF
139200011008     C* Se trattasi di divisa ITL controllo che non abbiano inserito decimali
139300011008     C     D00DIV        IFEQ      'ITL'
139400011008     C                   Z-ADD     V4CASC        NUMDEC
139500011008     C     NUMDEC        IFGT      *ZEROS
139600011008     C                   SETON                                        5390
139700011008     C                   MOVEL     MSG(52)       $MSG
139800011008     C                   GOTO      FINCT4
139900011008     C                   ENDIF
140000011008     C                   ENDIF
140100011008     C*
140200970117X   1C                   ELSE
140300970117IF  2C     V4CASC        IFGT      *ZEROS
140400970117     C                   SETON                                        5390
140500970117     C                   MOVEL     MSG(33)       $MSG
140600970117     C                   GOTO      FINCT4
140700970117E   2C                   ENDIF
140800970117E   1C                   ENDIF
140900040802     C*---
141000040802     C* BANCALI
141100040802     C*---
141200040824X    c* se %sped. oltre 20 colli  > 100 errore
141300040811IF  1C     V4Csbnb       IFGT      100
141400040802     C                   SETON                                        1290
141500040802     C                   MOVEL     MSG(27)       $MSG
141600040802     C                   GOTO      FINCT4
141700040802E   1C                   ENDIF
141800040824X    c* se %sped. oltre 20 colli  obbligo anche il nr. bancali medi
141900040811IF  1C                   IF        (V4Csbnb > 0 and V4Cnbn = 0) or
142000040811     C                             (V4Csbnb = 0 and V4Cnbn <>0)
142100040811     C                   SETON                                        129016
142200040811     C                   MOVEL     MSG(73)       $MSG
142300040811     C                   GOTO      FINCT4
142400040811E   1C                   ENDIF
142500040824X    c* se %sped. bancali Bartolini > 100 errore
142600040824IF  1C     V4Csban       IFGT      100
142700040824     C                   SETON                                        1790
142800040824     C                   MOVEL     MSG(27)       $MSG
142900040824     C                   GOTO      FINCT4
143000040824E   1C                   ENDIF
143100040824X    c* se %sped. bancali Bartolini obbligo anche il nr. bancali medi
143200040824IF  1C                   IF        (V4Csban > 0 and V4Cnba = 0) or
143300040824     C                             (V4Csban = 0 and V4Cnba <>0)
143400040824     C                   SETON                                        179015
143500040824     C                   MOVEL     MSG(73)       $MSG
143600040824     C                   GOTO      FINCT4
143700040824E   1C                   ENDIF
143701150227IF  1C                   IF        (V4Csban > 0 and V4Ctba = 0)
143703150227     C                   SETON                                        189017
143704150227     C                   MOVEL     MSG(84)       $MSG
143705150227     C                   GOTO      FINCT4
143706150227E   1C                   ENDIF
143707150227     c                   clear                   v4dtba
143708150227IF  1C                   IF        V4Ctba <> 0
143709150227     c                   exsr      cnttba
143710150227E   1C                   ENDIF
143800050802     C*--------------------
143900050802     C* INGLOBA bancali ban
144000050802     C*--------------------
144100050802IF  1C     V4CIba        IFEQ      'S'
144200050802     C     V4CIba        OREQ      'N'
144300050802X   1C                   ELSE
144400050808     C                   SETON                                        8890
144500050802     C                   MOVEL     MSG(76)       $MSG
144600050802     C                   GOTO      FINCT4
144700050802E   1C                   ENDIF
144800050802     C*
144900050802IF  1C     V4CIba        IFEQ      'S'
145000050802     C     V4Cnba        ANDEQ     *ZEROS
145100050808     C                   SETON                                        151788
145200050802     C                   SETON                                        90
145300050802     C                   MOVEL     MSG(77)       $MSG
145400050802     C                   GOTO      FINCT4
145500050802E   1C                   ENDIF
145600040824     c* se esistono solo spedizione imbancalate dalla coop (BNB) non
145700040824     c* è possibile che esisti la gestione dei bancali Bartolini (BAN)
145800040824     c     v4csbnb       add       v4csban       com100
145900040824     C                   if        com100 > 100
146000040824     C                   SETON                                        121790
146100040824     C                   MOVEL     MSG(71)       $MSG
146200040824     C                   GOTO      FINCT4
146300040824E   2C                   ENDIF
146400040802     C*
146500040802     c*
146600970117     C     FINCT4        ENDSR
146700970117     C*------------------------------------------------------------------------*
146800970117     C* IMPVD5 - IMPOSTAZIONE QUINTA VIDEATA
146900970117     C*------------------------------------------------------------------------*
147000970117     C     IMPVD5        BEGSR
147100970117     C*
147200970117     C* REIMPOSTA INDICATORI
147300970117     C                   EXSR      SETVD5
147400970117     C                   MOVEL     '5'           TIPVID
147500970117     C*
147600970117     C                   Z-ADD     V1CLNP        V5CLNP
147700970117     C                   MOVEL     V1DLNP        V5DLNP
147800970314     C                   Z-ADD     V1CDTC        V5CDTC
147900970117     C*
148000970117     C                   WRITE     TE01D00                                      *TESTATA
148100970117     C*
148200970117     C                   ENDSR
148300970117     C*------------------------------------------------------------------------*
148400970117     C* SETVD5 - IMPOSTAZIONI INDICATORI QUINTA VIDEATA
148500970117     C*------------------------------------------------------------------------*
148600970117     C     SETVD5        BEGSR
148700970117     C*
148800970117     C                   SETOFF                                       363738     ERRORI
148900970117     C                   SETOFF                                       394041
149000970414     C                   SETOFF                                       424368
149100970414     C                   SETOFF                                       697071
149200011009     C                   SETOFF                                       727377
149300011009     C                   SETOFF                                       787980
149400050805     C                   SETOFF                                       1486
149500970117     C                   SETOFF                                       90         GENERICO DI ERRORE
149600970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
149700970117     C*
149800970117     C                   ENDSR
149900970117     C*------------------------------------------------------------------------*
150000970117     C* GESVD5 - GESTIONE QUINTA VIDEATA
150100970117     C*------------------------------------------------------------------------*
150200970117     C     GESVD5        BEGSR
150300970117     C*
150400970117     C* EMISSIONE VIDEATA
150500970117     C                   WRITE     TE01D00                                      *TESTATA
150600970117     C                   EXFMT     TE01D05
150700970117     C                   EXSR      SETVD5
150800970117     C* F03-FINE
150900970117IF  1C     *INKC         IFEQ      *ON
151000970117     C                   EXSR      F03GES
151100970117     C                   GOTO      FINVD5
151200970117E   1C                   ENDIF
151300970117     C* CONTROLLA VIDEO
151400970117     C                   EXSR      CTRVD5
151500970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
151600970117     C                   GOTO      FINVD5
151700970117E   1C                   ENDIF
151800970117     C* F12-RITORNO
151900970117IF  1C     *INKL         IFEQ      *ON
152000970117     C                   EXSR      IMPVD4
152100970117     C                   GOTO      FINVD5
152200970117E   1C                   ENDIF
152300970117     C*
152400970117     C* RITORNA ALLA SECONDA VIDEATA
152500970117     C                   EXSR      IMPVD2
152600970117     C*
152700970117     C     FINVD5        ENDSR
152800970117     C*------------------------------------------------------------------------*
152900970117     C* CTRVD5 - CONTROLLA QUINTA VIDEATA
153000970117     C*------------------------------------------------------------------------*
153100970117     C     CTRVD5        BEGSR
153200970414     C*---
153300970414     C* CONSEGNE PARTICOLARI
153400970414     C*---
153500970117     C*--------------------
153600970414     C* % SU APPUNTAMENTO
153700970117     C*--------------------
153800970117IF  1C     V5CPCA        IFGT      100
153900970117     C                   SETON                                        3690
154000970117     C                   MOVEL     MSG(27)       $MSG
154100970117     C                   GOTO      FINCT5
154200131121     c                   else
154300131121     c     v5cpca        add       v5cpcs        wn7
154400131121IF  1C     WN7           IFGT      100
154500131121     C                   SETON                                        3690
154600131121     C                   MOVEL     MSG(83)       $MSG
154700131121     C                   GOTO      FINCT5
154800131121E   1C                   ENDIF
154900970117E   1C                   ENDIF
155000970117     C*--------------------
155100970414     C* INGLOBA AL PORTO
155200970117     C*--------------------
155300970117IF  1C     V5CIAP        IFEQ      'S'
155400970117     C     V5CIAP        OREQ      'N'
155500970117X   1C                   ELSE
155600970117     C                   SETON                                        3790
155700970117     C                   MOVEL     MSG(17)       $MSG
155800970117     C                   GOTO      FINCT5
155900970117E   1C                   ENDIF
156000970117     C*
156100970117IF  1C     V5CPCA        IFEQ      *ZEROS
156200970117     C     V5CIAP        ANDEQ     'S'
156300970117     C                   SETON                                        363790
156400970117     C                   MOVEL     MSG(18)       $MSG
156500970117     C                   GOTO      FINCT5
156600970117E   1C                   ENDIF
156700970117     C*--------------------
156800970414     C* % AI PIANI
156900970117     C*--------------------
157000970117IF  1C     V5CPCP        IFGT      100
157100970117     C                   SETON                                        3890
157200970117     C                   MOVEL     MSG(27)       $MSG
157300970117     C                   GOTO      FINCT5
157400970117E   1C                   ENDIF
157500970414     C*--------------------
157600970414     C* INGLOBA AL PORTO
157700970414     C*--------------------
157800970117IF  1C     V5CIPI        IFEQ      'S'
157900970117     C     V5CIPI        OREQ      'N'
158000970117X   1C                   ELSE
158100970117     C                   SETON                                        3990
158200970117     C                   MOVEL     MSG(17)       $MSG
158300970117     C                   GOTO      FINCT5
158400970117E   1C                   ENDIF
158500970117     C*
158600970117IF  1C     V5CPCP        IFEQ      *ZEROS
158700970117     C     V5CIPI        ANDEQ     'S'
158800970117     C                   SETON                                        383990
158900970117     C                   MOVEL     MSG(18)       $MSG
159000970117     C                   GOTO      FINCT5
159100970117E   1C                   ENDIF
159200970117     C*--------------------
159300970414     C* % AI SUPERMERCATI
159400970117     C*--------------------
159500970117IF  1C     V5CPCS        IFGT      100
159600970117     C                   SETON                                        4090
159700970117     C                   MOVEL     MSG(27)       $MSG
159800970117     C                   GOTO      FINCT5
159900131121     c                   else
160000131121     c     v5cpca        add       v5cpcs        wn7
160100131121IF  1C     WN7           IFGT      100
160200131121     C                   SETON                                        4090
160300131121     C                   MOVEL     MSG(83)       $MSG
160400131121     C                   GOTO      FINCT5
160500131121E   1C                   ENDIF
160600970117E   1C                   ENDIF
160700970414     C*--------------------
160800970414     C* INGLOBA AL PORTO
160900970414     C*--------------------
161000970117IF  1C     V5CISU        IFEQ      'S'
161100970117     C     V5CISU        OREQ      'N'
161200970117X   1C                   ELSE
161300970117     C                   SETON                                        4190
161400970117     C                   MOVEL     MSG(17)       $MSG
161500970117     C                   GOTO      FINCT5
161600970117E   1C                   ENDIF
161700970117     C*
161800970117IF  1C     V5CPCS        IFEQ      *ZEROS
161900970117     C     V5CISU        ANDEQ     'S'
162000970117     C                   SETON                                        404190
162100970117     C                   MOVEL     MSG(18)       $MSG
162200970117     C                   GOTO      FINCT5
162300970117E   1C                   ENDIF
162400970117     C*--------------------
162500970414     C* % FUORI MISURA
162600970117     C*--------------------
162700970117IF  1C     V5CPCF        IFGT      100
162800970117     C                   SETON                                        4290
162900970117     C                   MOVEL     MSG(27)       $MSG
163000970117     C                   GOTO      FINCT5
163100970117E   1C                   ENDIF
163200970414     C*--------------------
163300970414     C* INGLOBA AL PORTO
163400970414     C*--------------------
163500970117IF  1C     V5CIFU        IFEQ      'S'
163600970117     C     V5CIFU        OREQ      'N'
163700970117X   1C                   ELSE
163800970117     C                   SETON                                        4390
163900970117     C                   MOVEL     MSG(17)       $MSG
164000970117     C                   GOTO      FINCT5
164100970117E   1C                   ENDIF
164200970117     C*
164300970117IF  1C     V5CPCF        IFEQ      *ZEROS
164400970117     C     V5CIFU        ANDEQ     'S'
164500970117     C                   SETON                                        424390
164600970117     C                   MOVEL     MSG(18)       $MSG
164700970117     C                   GOTO      FINCT5
164800970117E   1C                   ENDIF
164900970414     C*--------------------
165000970414     C* % DDT
165100970414     C*--------------------
165200970414IF  1C     V5CPCD        IFGT      100
165300970414     C                   SETON                                        6890
165400970414     C                   MOVEL     MSG(27)       $MSG
165500970414     C                   GOTO      FINCT5
165600970414E   1C                   ENDIF
165700970414     C*--------------------
165800970414     C* INGLOBA AL PORTO
165900970414     C*--------------------
166000970414IF  1C     V5CIDD        IFEQ      'S'
166100970414     C     V5CIDD        OREQ      'N'
166200970414X   1C                   ELSE
166300970414     C                   SETON                                        6990
166400970414     C                   MOVEL     MSG(17)       $MSG
166500970414     C                   GOTO      FINCT5
166600970414E   1C                   ENDIF
166700970414     C*
166800970414IF  1C     V5CPCD        IFEQ      *ZEROS
166900970414     C     V5CIDD        ANDEQ     'S'
167000970414     C                   SETON                                        686990
167100970414     C                   MOVEL     MSG(18)       $MSG
167200970414     C                   GOTO      FINCT5
167300970414E   1C                   ENDIF
167400970414     C*--------------------
167500970414     C* % CENTRI STORICI
167600970414     C*--------------------
167700970414IF  1C     V5CPCC        IFGT      100
167800970414     C                   SETON                                        7090
167900970414     C                   MOVEL     MSG(27)       $MSG
168000970414     C                   GOTO      FINCT5
168100970414E   1C                   ENDIF
168200970414     C*--------------------
168300970414     C* INGLOBA AL PORTO
168400970414     C*--------------------
168500970414IF  1C     V5CICE        IFEQ      'S'
168600970414     C     V5CICE        OREQ      'N'
168700970414X   1C                   ELSE
168800970414     C                   SETON                                        7190
168900970414     C                   MOVEL     MSG(17)       $MSG
169000970414     C                   GOTO      FINCT5
169100970414E   1C                   ENDIF
169200970414     C*
169300970414IF  1C     V5CPCC        IFEQ      *ZEROS
169400970414     C     V5CICE        ANDEQ     'S'
169500970414     C                   SETON                                        707190
169600970414     C                   MOVEL     MSG(18)       $MSG
169700970414     C                   GOTO      FINCT5
169800970414E   1C                   ENDIF
169900970414     C*--------------------
170000970414     C* % LOCALITA' DISAGIATE
170100970414     C*--------------------
170200970414IF  1C     V5CPCL        IFGT      100
170300970414     C                   SETON                                        7290
170400970414     C                   MOVEL     MSG(27)       $MSG
170500970414     C                   GOTO      FINCT5
170600970414E   1C                   ENDIF
170700970414     C*--------------------
170800970414     C* INGLOBA AL PORTO
170900970414     C*--------------------
171000970414IF  1C     V5CILO        IFEQ      'S'
171100970414     C     V5CILO        OREQ      'N'
171200970414X   1C                   ELSE
171300970414     C                   SETON                                        7390
171400970414     C                   MOVEL     MSG(17)       $MSG
171500970414     C                   GOTO      FINCT5
171600970414E   1C                   ENDIF
171700970414     C*
171800970414IF  1C     V5CPCL        IFEQ      *ZEROS
171900970414     C     V5CILO        ANDEQ     'S'
172000970414     C                   SETON                                        727390
172100970414     C                   MOVEL     MSG(18)       $MSG
172200970414     C                   GOTO      FINCT5
172300970414E   1C                   ENDIF
172400011009     C*--------------------
172500011009     C* % CONS.  ISOLE
172600011009     C*--------------------
172700011009IF  1C     V5CCIS        IFGT      100
172800011009     C                   SETON                                        7790
172900011009     C                   MOVEL     MSG(27)       $MSG
173000011009     C                   GOTO      FINCT5
173100131120     c                   else
173200131120     C* FERMO DEPOSITO + CONSEGNA ISOLA DEVE ESSERE <= 100
173300131120IF  1C     V2CPFD        ADD       V5CCIS        WN7
173400131120IF  1C     WN7           IFGT      100
173500131121     C                   SETON                                        7790
173600131120     C                   MOVEL     MSG(81)       $MSG
173700131121     C                   GOTO      FINCT5
173800011009E   1C                   ENDIF
173900131121E   1C                   ENDIF
174000011009     C*--------------------
174100011009     C* INGLOBA AL PORTO
174200011009     C*--------------------
174300011009IF  1C     V5CISO        IFEQ      'S'
174400011009     C     V5CISO        OREQ      'N'
174500011009X   1C                   ELSE
174600011009     C                   SETON                                        7890
174700011009     C                   MOVEL     MSG(17)       $MSG
174800011009     C                   GOTO      FINCT5
174900011009E   1C                   ENDIF
175000011009     C*
175100011009IF  1C     V5CCIS        IFEQ      *ZEROS
175200011009     C     V5CISO        ANDEQ     'S'
175300011009     C                   SETON                                        777890
175400011009     C                   MOVEL     MSG(18)       $MSG
175500011009     C                   GOTO      FINCT5
175600011009E   1C                   ENDIF
175700011009     C*--------------------
175800011009     C* % RITIRI ISOLE
175900011009     C*--------------------
176000011031     C* Se ricbiesto il ritiro isola, controlla se il codice tassaizione del cliente della tariffa /
176100011031     c* offerta sia un "POSSIBILE ISOLA" (da tabella)
176200011009IF  1C     V5CSAM        IFGT      100
176300011009     C                   SETON                                        7990
176400011009     C                   MOVEL     MSG(27)       $MSG
176500011009     C                   GOTO      FINCT5
176600011031X   1C                   ELSE
176700011031IF  2C     V5CSAM        IFGT      *ZEROS
176800020923     C                   Z-ADD     1             KTBKUT
176900011030     C                   MOVEL     'CT'          KTBCOD
177000011030     C                   MOVEL(P)  WRKCTS        KTBKEY
177100011030     C     KEYTAB        CHAIN     TABEL00F                           99
177200011031     C                   MOVEL     TBLUNI        DSCT
177300011031IF  3c     WRKCTS        IFEQ      *BLANKS                                      *CLIENTE POTENZIALE
177400011031     C     *IN99         OREQ      *ON                                          *NON TROVATO
177500011031     C     TBLFLG        ORNE      *BLANKS                                      *ANULLATO
177600011031     C     §CT§§5        ORNE      'S'                                          *CODICE NON ISOLA
177700011030     C                   SETON                                        7990
177800011030     C                   MOVEL     MSG(56)       $MSG
177900011030     C                   GOTO      FINCT5
178000011031E   3C                   ENDIF
178100131121     c     v5csam        add       v2cpcm        wn7
178200131121IF  1C     WN7           IFGT      100
178300131121     C                   SETON                                        7990
178400131121     C                   MOVEL     MSG(82)       $MSG
178500131121     C                   GOTO      FINCT5
178600131121E   1C                   ENDIF
178700011031E   2C                   ENDIF
178800011009E   1C                   ENDIF
178900011009     C*--------------------
179000011009     C* INGLOBA AL PORTO
179100011009     C*--------------------
179200011009IF  1C     V5CINR        IFEQ      'S'
179300011009     C     V5CINR        OREQ      'N'
179400011009X   1C                   ELSE
179500011009     C                   SETON                                        8090
179600011009     C                   MOVEL     MSG(54)       $MSG
179700011009     C                   GOTO      FINCT5
179800011009E   1C                   ENDIF
179900011009     C*
180000011009IF  1C     V5CSAM        IFEQ      *ZEROS
180100011009     C     V5CINR        ANDEQ     'S'
180200011009     C                   SETON                                        798090
180300011009     C                   MOVEL     MSG(18)       $MSG
180400011009     C                   GOTO      FINCT5
180500011009E   1C                   ENDIF
180600040802     C*--------------------
180700040802     C* % sped destinatario disagiato
180800040802     C*--------------------
180900040802IF  1C     V5Cdes        IFGT      100
181000040802     C                   SETON                                        1490
181100040802     C                   MOVEL     MSG(27)       $MSG
181200040802     C                   GOTO      FINCT5
181300040802E   1C                   ENDIF
181400050805     C*--------------------
181500050805     C* INGLOBA AL PORTO
181600050805     C*--------------------
181700050805IF  1C     v5cide        IFEQ      'S'
181800050805     C     v5cide        OREQ      'N'
181900050805X   1C                   ELSE
182000050805     C                   SETON                                        8690
182100050805     C                   MOVEL     MSG(72)       $MSG
182200050805     C                   GOTO      FINCT5
182300050805E   1C                   ENDIF
182400050805     C*
182500050805IF  1C     v5cdes        IFEQ      *ZEROS
182600050805     C     v5cide        ANDEQ     'S'
182700050805     C                   SETON                                        148690
182800050805     C                   MOVEL     MSG(78)       $MSG
182900050805     C                   GOTO      FINCT5
183000050805E   1C                   ENDIF
183001151119     C*--------------------
183002151119     C* packing list
183003151119     C*--------------------
183004151119IF  1C     V5Cpkl        IFne      ' '
183005151119IF  1C     V5Cpkl        andne     'S'
183006151119     C                   SETON                                        7090
183007151119     C                   MOVEL     MSG(75)       $MSG
183008151119     C                   GOTO      FINCT5
183009151119E   1C                   ENDIF
183010151119IF  1C     V5Cpkl        IFeq      'S'
183011151119IF  1C     DTCV1C        andge     20160101
183012151119IF  1C     V5Cipk        IFne      'S'
183013151119IF  1C     V5Cipk        andne     'N'
183014151119     C                   SETON                                        7190
183015151119     C                   MOVEL     MSG(86)       $MSG
183016151119     C                   GOTO      FINCT5
183017151119E   1C                   ENDIF
183018151119E   1C                   ENDIF
183100970117     C*
183200970117     C     FINCT5        ENDSR
183300970117     C*------------------------------------------------------------------------*
183400970117     C* F03GES - GESTIONE TASTO FUNZIONALE F03 ==> FINE
183500970117     C*------------------------------------------------------------------------*
183600970117     C     F03GES        BEGSR
183700970117     C*
183800970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
183900970117     C                   MOVEL     '1'           D00F03                         *F03 IN PROCEDURA
184000970117     C*
184100970117     C                   ENDSR
184200970117     C*------------------------------------------------------------------------*
184300970117     C* F12GES - GESTIONE TASTO FUNZIONALE F12 ==> RITORNO
184400970117     C*------------------------------------------------------------------------*
184500970117     C     F12GES        BEGSR
184600970117     C*
184700970117     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
184800970117     C                   MOVEL     '1'           D00F12                         *F12 IN PROCEDURA
184900970117     C*
185000970117     C                   ENDSR
185100970117     C*------------------------------------------------------------------------*
185200970117     C* F05GES - GESTIONE TASTO FUNZIONALE F05 ==> CARICA DA ALTRA TARIFFA
185300970117     C*------------------------------------------------------------------------*
185400970117     C     F05GES        BEGSR
185500970117     C*
185600970117     C* GESTIONE VIDETA
185700970117     C                   EXSR      IMPWD2
185800970117DO  1C     $FINV         DOUEQ     'S'
185900970117     C                   EXSR      GESWD2                                       *GESTIONE FINESTRA
186000970117E   1C                   ENDDO
186100970117     C*
186200970117     C* CARICA A VIDEO I DATI TIPO DELLA TARIFFA RICHIESTA
186300970117IF  1C     $CONF         IFEQ      'S'                                          *SE CONFERMATO
186400970117     C                   EXSR      MEMETC                                       *MEMORIZZA
186500970117     C                   EXSR      IMPREC                                       *CARICA
186600970117E   1C                   ENDIF
186700970117     C*
186800970117     C                   ENDSR
186900970117     C*------------------------------------------------------------------------*
187000970117     C* IMPWD2 - IMPOSTAZIONI SECONDA FINESTRA
187100970117     C*------------------------------------------------------------------------*
187200970117     C     IMPWD2        BEGSR
187300970117     C*
187400091103     c                   if        d00dsf = ' '
187500091103     c                   eval      d2wcto = 'Tariffa/Offerta ............:'
187600091103     c                   eval      s2wcto = '(T/O)'
187700091103     c                   eval      d2wksc = 'Cliente o Visita ...........:'
187800091120     c                   eval      d2wprg = 'Progressivo ................:'
187900091103     c                   eval      d2wtesta = 'Caricamento dati tipo da altra -
188000091103     c                             Tariffa/Offerta'
188100091103     c                   else
188200091201     c                   eval      d2wcto = 'Tariffa/Offerta ............:'
188300091130     c                   eval      s2wcto = '(T/O)'
188400091103     c                   eval      d2wksc = 'Cliente o Trattativa .......:'
188500091120     c                   eval      d2wprg = 'Versione ...................:'
188600091103     c                   eval      d2wtesta = 'Caricamento dati tipo da altra -
188700091103     c                             Tariffa/Trattativa'
188800091103     c                   end
188900970117     C* REIMPOSTA FORMATO VIDEO E INDICATORI
189000970117     C                   MOVEL     'N'           $FINV                          *FINE FINESTRA
189100970117     C                   MOVEL     'N'           $CONF                          *CONFERMA CARICAMENT
189200980429     C                   MOVEL     D0DCTO        V2WCTO
189300970319     C                   Z-ADD     D0DKSC        V2WKSC
189400970319     C                   Z-ADD     D0DCTR        V2WCTR
189500970319     C                   Z-ADD     D0DPRG        V2WPRG
189600970319     C                   Z-ADD     D0DLNP        V2WLNP
189700011003     C                   MOVEL     D0DDIV        V2WDIV
189800970117     C                   EXSR      SETWD2
189900970117     C*
190000970117     C                   ENDSR
190100970117     C*------------------------------------------------------------------------*
190200970117     C* SETWD2 - IMPOSTAZIONI INDICATORI SECONDA FINESTRA
190300970117     C*------------------------------------------------------------------------*
190400970117     C     SETWD2        BEGSR
190500970117     C*
190600970117     C                   SETOFF                                       616263     ERRORI
190700980429     C                   SETOFF                                       646574
190800011003     C                   SETOFF                                       76
190900970117     C                   SETOFF                                       90         GENERICO DI ERRORE
191000970117     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
191100970117     C*
191200970117     C                   ENDSR
191300970117     C*------------------------------------------------------------------------*
191400970117     C* GESWD2 - GESTIONE SECONDA FINESTRA
191500970117     C*------------------------------------------------------------------------*
191600970117     C     GESWD2        BEGSR
191700970117     C*
191800970117     C* EMISSIONE VIDEATA
191900970117     C                   EXFMT     TE01W02
192000970117     C                   EXSR      SETWD2
192100970117     C* F12-RITORNO
192200970117IF  1C     *INKL         IFEQ      *ON
192300970117     C                   MOVEL     'N'           $CONF                          *CONFERMA NO
192400970117     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
192500970117     C                   GOTO      FINWD2
192600970117E   1C                   ENDIF
192700980430     C* F11-RICERCA
192800980430IF  1C     *INKK         IFEQ      *ON
192900980430     C                   EXSR      GESRIC
193000060825     C*                  GOTO      FINWD2
193100980430E   1C                   ENDIF
193200970117     C* CONTROLLA VIDEO
193300970117     C                   EXSR      CTRWD2
193400970117IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
193500970117     C                   GOTO      FINWD2
193600970117E   1C                   ENDIF
193700970117     C* F06-CONFERMA
193800970117IF  1C     *INKF         IFEQ      *ON
193900970117     C                   MOVEL     'S'           $CONF                          *CONFERMA SI
194000970117     C                   MOVEL     'S'           $FINV                          *FINE FINESTRA
194100970117     C                   GOTO      FINWD2
194200970117E   1C                   ENDIF
194300970117     C*
194400970117     C     FINWD2        ENDSR
194500980430     C*------------------------------------------------------------------------*
194600980430     C* GESRIC - GESTIONE RICERCA DATI TIPO
194700980430     C*------------------------------------------------------------------------*
194800980430     C     GESRIC        BEGSR
194900980430     C*
195000980430     C                   CLEAR                   DSTE30
195100980430     C                   MOVEL     'C01'         D30OP0
195200980708     C                   MOVEL     'C'           D30ETX
195300980804     C                   Z-ADD     D00KSC        D30KSC
195400011011     C                   MOVEL     D00DIV        D30DIV
195500091123     C                   MOVEL     D00Dsf        D30dsf
195600980430     C                   CALL      'TNTE32R'
195700980430     C                   PARM                    KPJBA
195800980430     C                   PARM                    DSTE30
195900980430IF  1C     D30ERR        IFEQ      '1'                                          *ERRORE
196000980430     C                   SETON                                        90
196100980504     C                   MOVEL     D30ERR        $MSG                           *COMUNQUE MESSAGGIO
196200980430X   1C                   ELSE                                                   *NO ERRORE
196300980430IF  2C     D30F03        IFEQ      '1'                                          *NON SCELTA
196400980430     C     D30F12        OREQ      '1'
196500980430X   2C                   ELSE                                                   *SCELTA
196600980430     C                   MOVEL     'S'           $CONF                          *CONFERMA SI
196700980430     C                   MOVEL     D30CTO        V2WCTO
196800980430     C                   Z-ADD     D30KSC        V2WKSC
196900980430     C                   Z-ADD     D30CTR        V2WCTR
197000980430     C                   Z-ADD     D30PRG        V2WPRG
197100980430     C                   Z-ADD     D30LNP        V2WLNP
197200011003     C                   MOVEL     D30DIV        V2WDIV
197300980430E   2C                   ENDIF
197400980430E   1C                   ENDIF
197500980430     C                   ENDSR
197600970117     C*------------------------------------------------------------------------*
197700970117     C* CTRWD2 - CONTROLLA SECONDA FINESTRA
197800970117     C*------------------------------------------------------------------------*
197900970117     C     CTRWD2        BEGSR
198000970117     C*--------------------
198100091103     C* TARIFFA / OFFERTA  / TRATTATIVA
198200970117     C*--------------------
198300091103     c                   if        (d00dsf = ' '
198400091103IF  1C                             and v2wcto <> 'T' and
198500091103IF  1C                             v2wcto <> 'O') or
198600091103     c                             (d00dsf = 'X'
198700091103IF  1C                             and v2wcto <> 'T' and
198800091130IF  1C                             v2wcto <> 'O')
198900091103     C                   SETON                                        90
199000091103     C                   MOVEL     MSG(4)        $MSG
199100091103     C                   GOTO      FINCW2
199200091103E   1C                   ENDIF
199300011026     C*
199400011026     C* IMPOSTAZIONE APPLICAZIONE TARIFFA FINITA IN DS
199500011026     C                   Z-ADD     V2WKSC        KTMKSC                         *CLIENTE
199600011026     C                   Z-ADD     V2WCTR        KTMCTR                         *TARIFFA
199700011026     C                   Z-ADD     V2WPRG        KTMPRG                         *PROGRESSIVO
199800020715     C*
199900020715     C* SE SONO IN MANUTENZIONE OFFERTE INVERTO I CRITERI DI LETTURA RISPETTO ALLA SCELTA INDICATA
200000020715     C* A VIDEO X GESTIRE LE OVRDBF PRESENTI
200100091103     c                   if        %subst(d00op0: 1: 1)='O'
200200020715     C                   IF        V2WCTO = 'O'
200300020715     C     KEYTAM        CHAIN     TNTAM01L                           84
200400020715     C                   ELSE
200500020715     C     KEYTAM        CHAIN     TNOFM01L                           84
200600020715     C                   ENDIF
200700020715     C                   ELSE
200800020715     C                   IF        V2WCTO = 'T'
200900011210     C     KEYTAM        CHAIN     TNTAM01L                           84
201000011210     C                   ELSE
201100011210     C     KEYTAM        CHAIN     TNOFM01L                           84
201200011210     C                   ENDIF
201300020715     C                   ENDIF
201400011210     C                   IF        not *in84
201500011026     C                   MOVEL     TAMFLO        DSTA01
201600011026     C                   MOVEL     §TADIV        V2WDIV
201700011210     C                   ELSE
201800011210     C                   SETON                                        616263
201900011210     C                   SETON                                        646574
202000011210     C                   SETON                                        76
202100011210     C                   SETON                                        90
202200011210     C                   MOVEL     MSG(59)       $MSG
202300011210     C                   GOTO      FINCW2
202400011026E   2C                   ENDIF
202500970117     C*--------------------
202600970117     C* LINEA DI PARTENZA
202700970117     C*--------------------
202800970117     C                   MOVEL     V2WLNP        WLNP
202900970117     C                   MOVEL     *BLANKS       WDLNP
203000970117     C                   MOVEL     '0'           WERR
203100970117     C                   EXSR      CHKLNP
203200980818IF  1C     WERR          IFEQ      '1'
203300980818IF  2C     WLNP          IFEQ      888
203400980818     C     WLNP          OREQ      889
203500980818     C                   MOVEL     '0'           WERR
203600980818     C                   MOVEL     DDEC(10)      WDLNP
203700980818X   2C                   ELSE
203800970117     C                   SETON                                        6590
203900970117     C                   MOVEL     MSG(3)        $MSG
204000970117     C                   GOTO      FINCW2
204100980818E   2C                   ENDIF
204200980818E   1C                   ENDIF
204300970117     C*--------------------
204400970117     C* DATI TIPO ESISTENTI
204500970117     C*--------------------
204600091120     C                   MOVEL     'S'           KTCDSF                         *DATI SEDE/FILIALE
204700970117     C                   MOVEL     V2WCTO        KTCCTO                         *TARIFFA O OFFERTA
204800970117     C                   Z-ADD     V2WKSC        KTCKSC                         *CLIENTE P VISITA
204900970117     C                   Z-ADD     V2WCTR        KTCCTR                         *TARIFFA
205000970117     C                   Z-ADD     V2WPRG        KTCPRG                         *PROGRESSIVO
205100970117     C                   Z-ADD     V2WLNP        KTCLNP                         *LINEA DI PARTENZA
205200011001     C     KEYETC        SETLL     TEETC01L                               99
205300970117IF  1C     *IN99         IFEQ      *OFF                                         *NON TROVATO
205400970117     C                   SETON                                        616263
205500980429     C                   SETON                                        646574
205600011003     C                   SETON                                        76
205700980429     C                   SETON                                        90
205800970117     C                   MOVEL     MSG(9)        $MSG
205900970117     C                   GOTO      FINCW2
206000970117E   1C                   ENDIF
206100970117     C*
206200970319     C* DEPOSITA NEI CAMPI DELLA DS LA TARIFFA RICHIESTA IN COPIA
206300970319IF  1C     WERR          IFEQ      '0'                                          *NO ERRORE
206400091120     C                   MOVEL     'S'           D0DDSF
206500980429     C                   MOVEL     V2WCTO        D0DCTO
206600970319     C                   Z-ADD     V2WKSC        D0DKSC
206700970319     C                   Z-ADD     V2WCTR        D0DCTR
206800970319     C                   Z-ADD     V2WPRG        D0DPRG
206900970319     C                   Z-ADD     V2WLNP        D0DLNP
207000011003     C                   MOVEL     V2WDIV        D0DDIV
207100970319E   1C                   ENDIF
207200970319     C*
207300970117     C     FINCW2        ENDSR
207400970117     C*------------------------------------------------------------------------*
207500970117     C* MEMETC - MEMORIZZA I DATI TIPO GENERALI DELLA TARIFFA RICHIESTA
207600970117     C*------------------------------------------------------------------------*
207700970117     C     MEMETC        BEGSR
207800970117     C*
207900970117     C* LEGGE I DATI TIPO DELLA TARIFFA RICHIESTA
208000091120     C                   MOVEL     'S'           KTCDSF                         *DATI SEDE/FILIALE
208100970117     C                   MOVEL     V2WCTO        KTCCTO                         *TARIFFA O OFFERTA
208200970117     C                   Z-ADD     V2WKSC        KTCKSC                         *CLIENTE O VISITA
208300970117     C                   Z-ADD     V2WCTR        KTCCTR                         *TARIFFA
208400970117     C                   Z-ADD     V2WPRG        KTCPRG                         *PROGRESSIVO
208500970117     C                   Z-ADD     V2WLNP        KTCLNP                         *LINEA DI PARTENZA
208600011001     C     KEYETC        CHAIN(N)  TEETC01L                           99
208700970117     C*
208800980430     C                   ENDSR
208900970117     C*------------------------------------------------------------------------*
209000970117     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> AGGIORNA E PROSEGUE
209100970117     C*------------------------------------------------------------------------*
209200970117     C     F06GES        BEGSR
209300970725     C*
209400970725     C* IMPOSTA VARIABILI DI LAVORO
209500970725     C                   MOVE      '1'           WTUR                           *N°GIRO SCRITTURA TA
209600970725     C*
209700970117     C* AGGIORNA L'ARCHIVIO
209800970117     C                   MOVEL     '0'           WERR
209900970117IF  1C     $TGES         IFEQ      'G'                                          *SE GESTIONE
210000970117     C                   EXSR      AGGETC                                       *AGGIORNA
210100970117E   1C                   ENDIF
210200970117IF  1C     WERR          IFEQ      '0'                                          *NO ERRORI
210300011001     C                   MOVEL     'S'           D00ETC                         *S=TEETC CONTROLLATO
210400970117     C*---
210500970117     C* LANCIA LA GESTIONE DEGLI SCAGLIONI
210600970117     C*---
210700970117     C                   EXSR      IMPLAN                                       *IMPOSTA CAMPI
210800011001     C                   MOVEL     'N'           D00ETS                         *S=TEETS CONTROLLATO
210900970117     C                   MOVEL     'TNTE02R'     LPGM              8            *PGM DA LANCIARE
211000970117     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
211100970130IF  2C     D00ETS        IFEQ      'S'                                          *LANCIO OK
211200060830     c* 92 on ho gli scaglioni anche per le competenze
211300061026     c                   if        etcbnb <> 0
211400060830     c     keyetc        setll     teets11l                               92
211500061030     c                   else
211600061030     c                   setoff                                       92
211700061026     c                   end
211800970117     C*---
211900970117     C* LANCIA LA SCRITTURA DEI CODICI TASSAZIONE DAL DETTAGLIO TARIFFA
212000970117     C*---
212100970130IF  3C     D00TLA        IFEQ      'D'                                          D E L T A
212200970117     C                   EXSR      IMPLAN                                       *IMPOSTA CAMPI
212300060830     C                   MOVEL     'TNTE09R'     LPGM                           *PGM DA LANCIARE
212400970117     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
212500970130E   3C                   ENDIF
212600970117     C*---
212700970117     C* LANCIA LA GESTIONE DEI CODICI TASSAZIONE
212800970117     C*---
212900970130IF  3C     D00ERR        IFEQ      '0'                                          *LANCIO OK
213000970117     C                   EXSR      IMPLAN                                       *IMPOSTA CAMPI
213100011001     C                   MOVEL     'N'           D00ETT                         *S=TEETT CONTROLLATO
213200060830     C                   MOVEL     'TNTE03R'     LPGM                           *PGM DA LANCIARE
213300970117     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
213400970317IF  4C     D00ETT        IFEQ      'S'                                          *LANCIO OK
213500970317     C***
213600970317     C* D00TLA='P' --> CALCOLO PORTO
213700970317     C***
213800970317IF  5C     D00TLA        IFEQ      'P'
213900970117     C*---
214000970117     C* LANCIA IL CALCOLO DELLE COMPETENZE
214100970117     C*---
214200970313     C                   EXSR      IMPLAN                                       *IMPOSTA CAMPI
214300060830     c* scaglioni competenze
214400060830     c                   if        *in92
214500060830     C                   MOVEL     'TNTE10C'     LPGM                           *PGM DA LANCIARE
214600060830     C                   MOVE      'TNTE10R   '  kpjbu                          *PGM DA LANCIARE
214700060830     c                   else
214800060830     c* scaglioni ricavi
214900060830     C                   MOVEL     'TNTE10R'     LPGM                           *PGM DA LANCIARE
215000060830     c                   end
215100970313     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
215200970317IF  6C     D00ERR        IFEQ      '0'                                          *LANCIO OK
215300970117     C*---
215400970117     C* LANCIA LA SCRITTURA DEL DETTAGLIO TARIFFA
215500970117     C*---
215600970117     C                   EXSR      IMPLAN                                       *IMPOSTA CAMPI
215700060830     c* scaglioni competenze
215800060830     c                   if        *in92
215900060830     C                   MOVEL     'TNTE10C'     LPGM                           *PGM DA LANCIARE
216000060830     C                   MOVE      'TNTE15R   '  kpjbu                          *PGM DA LANCIARE
216100060830     c                   else
216200060830     c* scaglioni ricavi
216300060830     C                   MOVEL     'TNTE15R'     LPGM                           *PGM DA LANCIARE
216400060830     c                   end
216500970117     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
216600970506IF  7C     D00ERR        IFEQ      '0'                                          *LANCIO OK
216700970506     C*---
216800970506     C* LANCIA IL CALCOLO DEI RICAVI
216900970506     C*---
217000970506     C                   EXSR      IMPLAN                                       *IMPOSTA CAMPI
217100060830     C                   MOVEL     'TNTE11C'     LPGM                           *PGM DA LANCIARE
217200970506     C                   EXSR      LANPGM
217300970619IF  8C     D00ERR        IFEQ      '0'                                          *LANCIO OK
217400970506     C*---
217500970506     C* LANCIA LA RISCRITTURA DELLE COMPETENZE IN BASE AL DELTA RICHIESTO
217600970506     C*---
217700970506     C                   EXSR      IMPLAN                                       *IMPOSTA CAMPI
217800060830     c* scaglioni competenze
217900060830     c                   if        *in92
218000060830     C                   MOVEL     'TNTE10C'     LPGM                           *PGM DA LANCIARE
218100060830     C                   MOVE      'TNTE16R   '  kpjbu                          *PGM DA LANCIARE
218200060830     c                   else
218300060830     c* scaglioni ricavi
218400060830     C                   MOVEL     'TNTE16R'     LPGM                           *PGM DA LANCIARE
218500060830     c                   end
218600970506     C                   EXSR      LANPGM
218700970506IF  9C     D00ERR        IFEQ      '0'                                          *LANCIO OK
218800970506     C*---
218900970506     C* LANCIA LA SCRITTURA DEL DETTAGLIO TARIFFA
219000970506     C*---
219100970725     C                   MOVE      '2'           WTUR                           *N°GIRO SCRITTURA TA
219200970506     C                   EXSR      IMPLAN                                       *IMPOSTA CAMPI
219300060830     c* scaglioni competenze
219400060830     c                   if        *in92
219500060830     C                   MOVEL     'TNTE10C'     LPGM                           *PGM DA LANCIARE
219600060830     C                   MOVE      'TNTE15R   '  kpjbu                          *PGM DA LANCIARE
219700060830     c                   else
219800060830     c* scaglioni ricavi
219900060830     C                   MOVEL     'TNTE15R'     LPGM                           *PGM DA LANCIARE
220000060830     c                   end
220100970506     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
220200970506     C                   MOVEL     'S'           $FINE                          *FINE PROCEDURA
220300970506E   9C                   ENDIF
220400970506E   8C                   ENDIF
220500970506E   7C                   ENDIF
220600970506E   6C                   ENDIF
220700970317E   5C                   ENDIF
220800970117     C***
220900970117     C* D00TLA='D' --> CALCOLO DELTA
221000970117     C***
221100970317IF  5C     D00TLA        IFEQ      'D'
221200970117     C*---
221300970117     C* LANCIA IL CALCOLO DEI RICAVI
221400970117     C*---
221500970306     C                   EXSR      IMPLAN                                       *IMPOSTA CAMPI
221600060830     C                   MOVEL     'TNTE11C'     LPGM                           *PGM DA LANCIARE
221700970306     C                   EXSR      LANPGM
221800970317IF  6C     D00ERR        IFEQ      '0'                                          *LANCIO OK
221900970317     C*---
222000970317     C* LANCIA IL CALCOLO DELLE COMPETENZE
222100970317     C*---
222200970317     C                   EXSR      IMPLAN                                       *IMPOSTA CAMPI
222300060830     c* scaglioni competenze
222400060830     c                   if        *in92
222500060830     C                   MOVEL     'TNTE10C'     LPGM                           *PGM DA LANCIARE
222600060830     C                   MOVE      'TNTE10R   '  kpjbu                          *PGM DA LANCIARE
222700060830     c                   else
222800060830     c* scaglioni ricavi
222900060830     C                   MOVEL     'TNTE10R'     LPGM                           *PGM DA LANCIARE
223000060830     c                   end
223100970317     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
223200970317IF  7C     D00ERR        IFEQ      '0'                                          *LANCIO OK
223300970317     C*---
223400970317     C* LANCIA LA VISUALIZZAZIONE DEL DELTA
223500970317     C*---
223600970117     C                   EXSR      IMPLAN                                       *IMPOSTA CAMPI
223700060830     C                   MOVEL     'TNTE20R'     LPGM                           *PGM DA LANCIARE
223800970117     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
223900970317E   7C                   ENDIF
224000970117E   6C                   ENDIF
224100970117E   5C                   ENDIF
224200970117E   4C                   ENDIF
224300970117E   3C                   ENDIF
224400970117E   2C                   ENDIF
224500970117E   1C                   ENDIF
224600970117     C*
224700970117     C                   ENDSR
224800970117     C*------------------------------------------------------------------------*
224900011001     C* AGGETC - AGGIORNAMENTO ARCHIVIO -TEETC-
225000970117     C*------------------------------------------------------------------------*
225100970117     C     AGGETC        BEGSR
225200970117     C*
225300970117     C* RILEGGE IL RECORD DA AGGIORNARE
225400091120     C                   MOVEL     'S'           KTCDSF
225500970117     C                   MOVEL     D00CTO        KTCCTO
225600970117     C                   Z-ADD     V0CKSC        KTCKSC
225700970117     C                   Z-ADD     V0CCTR        KTCCTR
225800970117     C                   Z-ADD     V0CPRG        KTCPRG
225900970117     C                   Z-ADD     V1CLNP        KTCLNP
226000011001     C     KEYETC        CHAIN     TEETC01L                           9998
226100970117IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
226200970117     C                   SETON                                        90
226300970117     C                   MOVEL     MSG(70)       $MSG
226400970117     C                   MOVEL     '1'           WERR
226500970117X   1C                   ELSE                                                   *NON ALLOCATO
226600970117     C* INSERIMENTO
226700970117IF  2C     *IN99         IFEQ      *ON
226800970117     C                   EXSR      IMPETC                                       *IMPOSTA I CAMPI
226900011001     C                   WRITE     TEETC000
227000970117     C                   MOVEL     DOPE(2)       V0COPE                         *MANUTENZ.IN TESTATA
227100970117     C* VARIAZIONE
227200970117X   2C                   ELSE
227201150305     c                   MOVE      v4ctba        comtba
227300980504IF  3C     V2CDCV        IFNE      ETCDCV                                       *SE MODIFICATO QC.
227400980504     C     V2CFAR        ORNE      ETCFAR
227500970314     C     V2CIAR        ORNE      ETCIAR
227600970314     C     V2CCLV        ORNE      ETCCLV
227700970117     C     V2CBOL        ORNE      ETCBOL
227800021125     C     V2CDKC        ORNE      ETCDKC
227900021125     C     V2CIX1        ORNE      ETCIX1
228000021125     C     V2CIX2        ORNE      ETCIX2
228100970117     C     V2CGGP        ORNE      ETCGGP
228200970117     C     V2CSPR        ORNE      ETCSPR
228300970117     C     V2CPMC        ORNE      ETCPMC
228400970117     C     V2CVMC        ORNE      ETCVMC
228500970117     C     V2CFRA        ORNE      ETCFRA
228600970117     C     V2CASS        ORNE      ETCASS
228700970117     C     V2CPCV        ORNE      ETCPCV
228800970117     C     V2CPCM        ORNE      ETCPCM
228900970117     C     V2CPFD        ORNE      ETCPFD
229000040730     C     V2Cval        ORNE      ETCval
229001151119     C     V2Crtm        ORNE      §ETCrtm
229100970117     C     V3CPPV        ORNE      ETCPPV
229200970117     C     V3CSIN        ORNE      ETCSIN
229300970117     C     V3CESP        ORNE      ETCESP
229400970117     C     V3CIES        ORNE      ETCIES
229500970117     C     V3CPGI        ORNE      ETCPGI
229600970117     C     V3CGGG        ORNE      ETCGGG
229700970117     C     V3CNRG        ORNE      ETCNRG
229800970117     C     V3CIGI        ORNE      ETCIGI
229900970117     C     V4CPAB        ORNE      ETCPAB
230000970117     C     V4CPAM        ORNE      ETCPAM
230100970117     C     V4CICA        ORNE      ETCICA
230200970117     C     V4CCAS        ORNE      ETCCAS
230300970117     C     V4CPAR        ORNE      ETCPAR
230400060313     C     V4Cacb        ORNE      ETCxx1
230500970117     C     V4CPAC        ORNE      ETCPAC
230600970117     C     V4CIAS        ORNE      ETCIAS
230700970117     C     V4CASC        ORNE      ETCASC
230800040811     C     V4Csbnb       ORNE      ETCbnb
230900040824     C     V4Csban       ORNE      ETCban
230901150302     C     comtba        ORNE      §etctba
231000050802     C     V4Ciba        ORNE      ETCiba
231100040811     C     V4Cnbn        ORNE      ETCnbn
231200040811     C     V4Cnba        ORNE      ETCnba
231300970117     C     V5CPCA        ORNE      ETCPCA
231400970117     C     V5CIAP        ORNE      ETCIAP
231500970117     C     V5CPCP        ORNE      ETCPCP
231600970117     C     V5CIPI        ORNE      ETCIPI
231700970117     C     V5CPCS        ORNE      ETCPCS
231800970117     C     V5CISU        ORNE      ETCISU
231900970117     C     V5CPCF        ORNE      ETCPCF
232000970117     C     V5CIFU        ORNE      ETCIFU
232100970414     C     V5CPCD        ORNE      ETCPCD
232200970414     C     V5CIDD        ORNE      ETCIDD
232300970414     C     V5CPCC        ORNE      ETCPCC
232400970414     C     V5CICE        ORNE      ETCICE
232500151119     C     v5cpkl        ORNE      ETCPkl
232600970414     C     V5CPCL        ORNE      ETCPCL
232700970414     C     V5CILO        ORNE      ETCILO
232800011017     C     V5CCIS        ORNE      ETCCIS
232900011017     C     V5CISO        ORNE      ETCISO
233000011017     C     V5CSAM        ORNE      ETCSAM
233100011017     C     V5CINR        ORNE      ETCINR
233200040802     C     V5Cdes        ORNE      ETCdes
233300050808     C     V5CIDE        ORNE      ETCIDE
233400970117     C                   EXSR      IMPETC                                       *IMPOSTA I CAMPI
233500011001     C                   UPDATE    TEETC000
233600970117E   3C                   ENDIF
233700970117E   2C                   ENDIF
233800970117E   1C                   ENDIF
233900011001     C                   UNLOCK    TEETC01L
234000970117     C*
234100970117     C                   ENDSR
234200970117     C*------------------------------------------------------------------------*
234300011001     C* IMPETC - IMPOSTA I CAMPI DEL RECORD DA AGGIORNARE -TEETC-
234400970117     C*------------------------------------------------------------------------*
234500970117     C     IMPETC        BEGSR
234600970117     C*
234700970117IF  1C     *IN99         IFEQ      *ON                                          *INSERIMENTO
234800091120     C                   MOVEL     'S'           ETCDSF
234900980429     C                   MOVEL     KTCCTO        ETCCTO
235000970117     C                   Z-ADD     KTCKSC        ETCKSC
235100970117     C                   Z-ADD     KTCCTR        ETCCTR
235200970117     C                   Z-ADD     KTCPRG        ETCPRG
235300970117     C                   Z-ADD     KTCLNP        ETCLNP
235400970117     C                   Z-ADD     DATCOR        ETCDIR
235500970117     C                   Z-ADD     *ZEROS        ETCDUV
235600970117     C                   MOVEL     *BLANKS       ETCFTR
235700970117     C                   Z-ADD     *ZEROS        ETCDTR
235800970117X   1C                   ELSE                                                   *VARIAZIONE
235900970117     C                   MOVEL     *BLANKS       ETCFTR
236000970117     C                   Z-ADD     DATCOR        ETCDUV
236100970117E   1C                   ENDIF
236200980504     C                   MOVEL     V2CDCV        ETCDCV
236300980504     C                   MOVEL     V2CFAR        ETCFAR
236400970314     C                   Z-ADD     V2CIAR        ETCIAR
236500970117     C                   MOVEL     V2CCLV        ETCCLV
236600970117     C                   MOVEL     V2CBOL        ETCBOL
236700021125     C                   MOVEL     V2CDKC        ETCDKC
236800021125     C                   MOVEL     V2CIX1        ETCIX1
236900021125     C                   MOVEL     V2CIX2        ETCIX2
237000970117     C                   Z-ADD     V2CGGP        ETCGGP
237100970117     C                   Z-ADD     V2CSPR        ETCSPR
237200970117     C                   Z-ADD     V2CPMC        ETCPMC
237300970117     C                   Z-ADD     V2CVMC        ETCVMC
237400970117     C                   Z-ADD     V2CFRA        ETCFRA
237500970117     C                   Z-ADD     V2CASS        ETCASS
237600970117     C                   Z-ADD     V2CPCV        ETCPCV
237700970117     C                   Z-ADD     V2CPCM        ETCPCM
237800970117     C                   Z-ADD     V2CPFD        ETCPFD
237900040730     C                   Z-ADD     V2Cval        ETCval
237901151119     c                   if        v2crtm <> ' '
238000151119     C                   movel     v2crtm        §ETCrtm
238001151119     C                   movel     v2cirp        §ETCirp
238002151119     c                   end
238100970117     C                   Z-ADD     V3CPPV        ETCPPV                         *TERZA VIDEATA
238200970117     C                   MOVEL     V3CSIN        ETCSIN
238300970117     C                   Z-ADD     V3CESP        ETCESP
238400970117     C                   MOVEL     V3CIES        ETCIES
238500970117     C                   Z-ADD     V3CPGI        ETCPGI
238600970117     C                   Z-ADD     V3CGGG        ETCGGG
238700970117     C                   Z-ADD     V3CNRG        ETCNRG
238800970117     C                   MOVEL     V3CIGI        ETCIGI
238900970117     C                   Z-ADD     V4CPAB        ETCPAB                         *QUARTA VIDEATA
239000970117     C                   Z-ADD     V4CPAM        ETCPAM
239100970117     C                   MOVEL     V4CICA        ETCICA
239200970117     C                   Z-ADD     V4CCAS        ETCCAS
239300970117     C                   Z-ADD     V4CPAR        ETCPAR
239400060313     C                   Z-ADD     V4Cacb        ETCxx1
239500970117     C                   Z-ADD     V4CPAC        ETCPAC
239600970117     C                   MOVEL     V4CIAS        ETCIAS
239700970117     C                   Z-ADD     V4CASC        ETCASC
239800040811     C                   Z-ADD     V4Csbnb       ETCbnb
239900040824     C                   Z-ADD     V4Csban       ETCban
239901150227     c                   if        v4ctba <> 0
239902150227     C                   move      V4Ctba        §ETCtba                        *QUARTA VIDEATA
239904150227     c                   end
240000050802     C                   MOVEL     V4CIba        ETCIba
240100040811     C                   Z-ADD     V4Cnbn        ETCnbn
240200040811     C                   Z-ADD     V4Cnba        ETCnba
240201151119     C                   movel     v5cpkl        ETCPkl
240202151119     C                   movel     v5cipk        ETCiva
240300970117     C                   Z-ADD     V5CPCA        ETCPCA                         *QUINTA VIDEATA
240400970117     C                   MOVEL     V5CIAP        ETCIAP
240500970117     C                   Z-ADD     V5CPCP        ETCPCP
240600970117     C                   MOVEL     V5CIPI        ETCIPI
240700970117     C                   Z-ADD     V5CPCS        ETCPCS
240800970117     C                   MOVEL     V5CISU        ETCISU
240900970117     C                   Z-ADD     V5CPCF        ETCPCF
241000970117     C                   MOVEL     V5CIFU        ETCIFU
241100970414     C                   Z-ADD     V5CPCD        ETCPCD
241200970414     C                   MOVEL     V5CIDD        ETCIDD
241300970414     C                   Z-ADD     V5CPCC        ETCPCC
241400970414     C                   MOVEL     V5CICE        ETCICE
241500970414     C                   Z-ADD     V5CPCL        ETCPCL
241600970414     C                   MOVEL     V5CILO        ETCILO
241700011017     C                   Z-ADD     V5CCIS        ETCCIS
241800011017     C                   MOVEL     V5CISO        ETCISO
241900011017     C                   Z-ADD     V5CSAM        ETCSAM
242000011017     C                   MOVEL     V5CINR        ETCINR
242100040802     C                   Z-ADD     V5Cdes        ETCdes
242200050808     C                   MOVEL     V5CIDE        ETCIDE
242300011017     C                   MOVEL     *BLANKS       ETCATB
242301151119     C                   movel     detcflo       ETCflo                         *QUARTA VIDEATA
242400970117     C*
242500970117     C                   ENDSR
242600970117     C*------------------------------------------------------------------------*
242700970117     C* IMPLAN - IMPOSTA I DATI DI LANCIO PROGRAMMA
242800970117     C*------------------------------------------------------------------------*
242900970117     C     IMPLAN        BEGSR
243000970117     C*
243100970117     C                   MOVEL     'F06'         D00OP1                         *LIVELLO PROGRAMMA
243200970117     C                   MOVEL     '0'           D00F03                         *F03-FINE PGM
243300970117     C                   MOVEL     '0'           D00F12                         *F12-RITORNO
243400970117     C                   MOVEL     '0'           D00ERR                         *ERRORE
243500970117     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO ERRORE
243600970117     C                   Z-ADD     V0CKSC        D00KSC                         *CLIENTE
243700970117     C                   Z-ADD     V0CCTR        D00CTR                         *TARIFFA
243800970117     C                   Z-ADD     V0CPRG        D00PRG                         *PROGRESSIVO
243900970117     C                   MOVEL     V0CTSP        D00TSP                         *TIPO SERVIZIO
244000970117     C                   MOVEL     V1CFIE        D00FIE                         *ITALIA/ESTERO
244100970117     C                   Z-ADD     V1CLNP        D00LNP                         *LINEA DI PARTENZA
244200970314     C                   Z-ADD     DTCV1C        D00DTC                         *DATA DI RIFERIMENTO
244300970725     C                   MOVEL     WTUR          D00TUR                         *N°GIRO SCRITTURA TA
244400011003     C                   MOVEL     V0CDIV        D00DIV                         *DIVISA TARIFFA/OFFE
244500970117     C*
244600970117     C                   ENDSR
244700970117     C*------------------------------------------------------------------------*
244800970117     C* LANPGM - LANCIA UN PROGRAMMA
244900970117     C*------------------------------------------------------------------------*
245000970117     C     LANPGM        BEGSR
245100970117     C*
245200970117     C                   CALL      LPGM                                 99
245300970117     C                   PARM                    KPJBA
245400970117     C                   PARM                    DSTE00
245500970117     C                   MOVEL     D00MSG        $MSG                           *MESSAGGIO DI RITORN
245600970117IF  1C     D00F03        IFEQ      '1'
245700970117     C                   MOVEL     'S'           $FINE                          *FINE PGM
245800970117E   1C                   ENDIF
245900970117IF  1C     D00ERR        IFEQ      '1'
246000970117     C                   SETON                                        90
246100970117E   1C                   ENDIF
246200970117     C*
246300970117     C                   ENDSR
246400970117     C*------------------------------------------------------------------------*
246500970117     C* CHKLNP - CONTROLLA LA LINEA DI PARTENZA
246600970117     C*------------------------------------------------------------------------*
246700970117     C     CHKLNP        BEGSR
246800970117     C*
246900970117     C     *LIKE         DEFINE    V1CLNP        WLNP
247000020729     C     *LIKE         DEFINE    §NTWFIE       WFL1
247100970117     C     *LIKE         DEFINE    V1DLNP        WDLNP
247200970117     C*
247300970117     C* CONTROLLA ESISTENZA CODICE IN ARCHIVIO
247400970117     C                   MOVEL     WLNP          KORFIL
247500970117     C     KEYORG        CHAIN     AZORG01L                           99
247600970117IF  1C     *IN99         IFEQ      *OFF
247700970117     C     ORGFVA        ANDEQ     *BLANKS
247800970117     C     ORGFAG        ANDNE     'V'
247900970117     C                   MOVEL     ORGDES        WDLNP
248000020729     C                   MOVEL     ORGDE3        OG143
248100020729     C*
248200020729     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
248300020729     C                   CLEAR                   DNTW
248400020729     C*
248500020729     C                   MOVEL(P)  'NTW'         KTECOD
248600020729     C                   MOVEL(P)  §OGNTW        KTEKE1
248700020729     C     KEYTBE        CHAIN     TNTBE01L
248800020729     C                   IF        %found(TNTBE01L)
248900020729     C                   MOVEL     TBEUNI        DNTW
249000020729     C                   ENDIF
249100020729     C*
249200020729     C                   MOVEL     §NTWFIE       WFL1
249300970117X   1C                   ELSE
249400980818     C                   MOVEL     '1'           WERR
249500980818E   1C                   ENDIF
249600970117     C*
249700970117     C                   ENDSR
249800970117     C*------------------------------------------------------------------------*
249900970117     C* IMPVD0 - IMPOSTAZIONE TESTATA
250000970117     C*------------------------------------------------------------------------*
250100970117     C     IMPVD0        BEGSR
250200970117     C*
250300970117     C                   MOVEL     DVID(1)       V0CVID                         *GESTIONE VIDEO
250400970117     C                   MOVEL     *BLANKS       V0COPE                         *OPERAZIONE
250500970117IF  1C     D00TLA        IFEQ      'P'
250600970117     C                   MOVEL     DVID(2)       V0CTLA                         *LIVELLO PROCEDURA
250700970117X   1C                   ELSE
250800970117     C                   MOVEL     DVID(3)       V0CTLA                         *LIVELLO PROCEDURA
250900970117E   1C                   ENDIF
251000091120     c*
251100091120     C                   MOVEL     'Prg.'        d0cprg                         *TARIFFA O OFFERTA
251200091103     c                   if        %subst(d00op0: 1: 1) = 'O'
251300091120     c                   if        d00dsf =' '
251400091120     C                   MOVEL     'Visita '     d0cKsc                         *TARIFFA O OFFERTA
251500970117X   1C                   ELSE
251600091120     C                   MOVEL     'Tratt. '     d0cksc                         *TARIFFA O OFFERTA
251700091120     C                   MOVEL     'Ver.'        d0cprg                         *TARIFFA O OFFERTA
251800091120E   1C                   ENDIF
251900091120X   1C                   ELSE
252000091120     C                   MOVEL     'Cliente'     d0cksc                         *TARIFFA O OFFERTA
252100970117E   1C                   ENDIF
252200970117IF  1C     $TGES         IFEQ      'I'                                          *INTERROGAZIONE
252300970117     C                   SETON                                        10        *SI PROTEZIONE CAMPI
252400970117     C                   MOVEL     DOPE(3)       V0CGES                         *INTERROGAZIONE
252500970117X   1C                   ELSE                                                   *GESTIONE
252600970117     C                   SETOFF                                       10        *NO PROTEZIONE CAMPI
252700970117     C                   MOVEL     DOPE(4)       V0CGES                         *GESTIONE
252800970117E   1C                   ENDIF
252900970117     C* CAMPI
253000970117     C                   Z-ADD     D00KSC        V0CKSC
253100970117     C                   Z-ADD     D00CTR        V0CCTR
253200970117     C                   Z-ADD     D00PRG        V0CPRG
253300970117     C                   MOVEL     D00FIE        V0CFIE
253400011003     C                   MOVEL     D00DIV        V0CDIV
253500970117IF  1C     D00FIE        IFEQ      *BLANKS                                      *NE ITALIA/NE ESTERO
253600970117     C                   SETON                                        01        *CHIEDERE TAR.I/E
253700970117X   1C                   ELSE                                                   *ITALIA O ESTERO
253800970117     C                   SETOFF                                       01        *NON CHIEDERE TAR.I/
253900970117     C                   MOVEL     D00FIE        V1CFIE
254000970117E   1C                   ENDIF
254100970117     C                   MOVEL     D00TSP        V0CTSP
254200970117     C                   Z-ADD     D00LNP        V1CLNP
254300970318     C                   Z-ADD     D00DTC        G08INV
254400970318     C                   Z-ADD     *ZEROS        G08DAT
254500970318     C                   MOVEL     '3'           G08ERR
254600970318     C                   CALL      'XSRDA8'
254700970318     C                   PARM                    WLBDA8
254800970318     C                   Z-ADD     G08DAT        V1CDTC                         *DATA RIFERIM.CALCOL
254900980429     C*
255000970117     C* DECODIFICHE
255100970117     C                   MOVEL     *BLANKS       V0DKSC
255200970117IF  1C     D00KSC        IFGT      *ZEROS
255300020923     C                   Z-ADD     1             KACKUT
255400020923     C                   Z-ADD     DUTKCI        KACKCC
255500970117     C                   Z-ADD     D00KSC        KACKSC
255600091103IF  2C                   IF        %subst(d00op0: 1: 1) ='T'                    *TARIFFA
255700970117     C     KEYACO        CHAIN     CNACO00F                           99
255800970414X   2C                   ELSE                                                   *OFFERTA
255900970414     C     KEYACO        CHAIN     TFACO00F                           99
256000970414E   2C                   ENDIF
256100970117IF  2C     *IN99         IFEQ      *OFF
256200970117     C                   MOVEL     ACORAG        V0DKSC                         *CLIENTE
256300970117E   2C                   ENDIF
256400970117E   1C                   ENDIF
256500970117     C*
256600970117     C                   MOVEL     *BLANKS       V0DCTR
256700970117IF  1C     D00KSC        IFGT      *ZEROS
256800970117     C                   MOVEL     D00CTR        WA1               1
256900970117     C                   SETOFF                                           99
257000970117     C                   Z-ADD     1             I
257100970117     C     WA1           LOOKUP    TTR(I)                                 99
257200970117IF  2C     *IN99         IFEQ      *ON
257300970117     C                   MOVEL     DTTR(I)       V0DCTR                         *TIPO TARIFFA
257400970117E   2C                   ENDIF
257500970117E   1C                   ENDIF
257600970117     C*
257700970117     C                   MOVEL     *BLANKS       V0DTSP
257800970117IF  1C     D00TSP        IFNE      *BLANKS
257900970117     C                   SETOFF                                           99
258000970117     C                   Z-ADD     1             I
258100970117     C     D00TSP        LOOKUP    TSP(I)                                 99
258200970117IF  2C     *IN99         IFEQ      *ON
258300970117     C                   MOVEL     DTSP(I)       V0DTSP                         *TIPO SERVIZIO
258400970117E   2C                   ENDIF
258500970117E   1C                   ENDIF
258600980429     C*
258700980429     C                   MOVEL     *BLANKS       V0DFIE
258800980429IF  1C     D00FIE        IFEQ      'I'
258900980429     C                   MOVEL     DDEC(1)       V0DFIE                         *FLAG ITALIA/ESTERO
259000980429E   1C                   ENDIF
259100980429IF  1C     D00FIE        IFEQ      'E'
259200980429     C                   MOVEL     DDEC(2)       V0DFIE                         *FLAG ITALIA/ESTERO
259300980429E   1C                   ENDIF
259400970117     C*
259500970117     C                   MOVEL     *BLANKS       V1DLNP
259600970117IF  1C     D00LNP        IFGT      *ZEROS
259700970117     C                   Z-ADD     D00LNP        KORFIL
259800970117     C     KEYORG        CHAIN     AZORG01L                           99
259900970117IF  2C     *IN99         IFEQ      *OFF
260000970117     C     ORGFVA        ANDEQ     *BLANKS
260100970117     C     ORGFAG        ANDNE     'V'
260200970117     C                   MOVEL     ORGDES        V1DLNP
260300970117E   2C                   ENDIF
260400970117E   1C                   ENDIF
260500970117     C*
260600970117     C                   ENDSR
260700970117     C*------------------------------------------------------------------------*
260800011011     C* CTRENT - CONTROLLI INIZIALI DI ENTRATA
260900970117     C*------------------------------------------------------------------------*
261000970117     C     CTRENT        BEGSR
261100970117     C*
261200970117     C* TIPO TARIFFA NON IN GESTIONE, ERRORE
261300970117     C                   MOVEL     D00CTR        WA1               1
261400970117     C                   SETOFF                                           99
261500970117     C     WA1           LOOKUP    TTR                                    99
261600970117IF  1C     *IN99         IFEQ      *OFF
261700970117     C                   MOVEL     MSG(60)       D00MSG
261800970117     C                   MOVEL     '1'           D00ERR
261900970117     C                   MOVEL     'S'           $FINE                          *USCITA PGM
262000970117E   1C                   ENDIF
262100970117     C*
262200011011     C     D00TLA        IFEQ      'P'
262300011011     C     D00DIV        ANDEQ     'ITL'
262400011011     C     DATCOR        ANDGE     §GEDCEDAT
262500011011     C                   MOVEL     MSG(65)       D00MSG
262600011011     C                   MOVEL     '1'           D00ERR
262700011011     C                   MOVEL     'S'           $FINE                          *USCITA PGM
262800011011     C                   ENDIF
262900020213     C*
263000020213     C     D00DPD        IFEQ      'F'
263100020213     C                   MOVEL     MSG(66)       D00MSG
263200020213     C                   MOVEL     '1'           D00ERR
263300020213     C                   MOVEL     'S'           $FINE                          *USCITA PGM
263400020213     C                   ENDIF
263500011011     C*
263600970117     C                   ENDSR
263601150227     C*------------------------------------------------------------------------*
263602150227     C* CNTCOD - CONTROLLA PRESENZA DEL tipo bancale                   "
263603150227     C*------------------------------------------------------------------------*
263604150227     C     CNTtba        BEGSR
263607150227     C*
263608150227     C                   CLEAR                   Dtba
263609150227     C                   CLEAR                   v4dtba
263610150227     C*
263611150227     C                   MOVEL(P)  'TBA'         KTECOD
263612150227     C                   MOVEL(P)  v4ctba        KTEKE1
263613150227     C     KEYTBE        CHAIN     TNTBE01L
263614150227     C                   IF        %found(TNTBE01L)
263615150227     C                   MOVEL     TBEUNI        Dtba
263616150227     c                   movel     §tbadesc      v4dtba
263617150227     c                   else
263618150227     c                   seton                                        1890
263619150227     C                   ENDIF
263634150227     C*
263635150227     C                   ENDSR
263700011011     C*------------------------------------------------------------------------*
263800011011     C* CARGEDCE - CARICAMENTO TABELLA "GED" TIPO RECORD "CE" E CONSIDERAZIONI SU DIVISE
263900011011     C*------------------------------------------------------------------------*
264000011011     C     CARGEDCE      BEGSR
264100011011     C*
264200011011     C                   CLEAR                   DSGEDCE
264300011026     C                   MOVEL     *blanks       WDIVCE            3
264400011011     C*
264500011011     C                   MOVEL(P)  'GED'         KTECOD
264600011011     C                   MOVEL(P)  'CE'          KTEKE1
264700011011     C     KEYTBE        CHAIN     TNTBE01L
264800011011     C                   IF        %found(TNTBE01L)
264900011011     C                   MOVEL     TBEUNI        DSGEDCE
265000011011     C                   ENDIF
265100011011     C*
265200011011     C                   IF        §GEDCEDAT > D00DTC
265300011026     C                   EVAL      WDIVCE = §GEDCEDIP
265400011026     C                   ELSE
265500011026     C                   EVAL      WDIVCE = §GEDCEDIV
265600011011     C                   ENDIF
265700011011     C*
265800011011     C                   ENDSR
265900970117     C*------------------------------------------------------------------------*
266000970117     C* CARTAB - CARICA LE TABELLE OCCORRENTI
266100970117     C*------------------------------------------------------------------------*
266200970117     C     CARTAB        BEGSR
266300970414     C*
266400970414     C* IMPOSTAZIONE APPLICAZIONE TARIFFA FINITA IN DS
266500970401     C                   Z-ADD     D00KSC        KTMKSC                         *CLIENTE
266600970401     C                   Z-ADD     D00CTR        KTMCTR                         *TARIFFA
266700970401     C                   Z-ADD     D00PRG        KTMPRG                         *PROGRESSIVO
266800970401     C     KEYTAM        CHAIN     TNTAM01L                           99        *ESISTE PER FORZA
266900970401     C                   Z-ADD     TAMATA        D00ATA                         *APPL.TAR.FINITA
267000970401IF  1C     D00ATA        IFEQ      *ZEROS
267100020923     C                   Z-ADD     1             KTBKUT
267200970401     C                   MOVEL     'TR'          KTBCOD
267300970401     C                   MOVEL     D00CTR        UNOCTR            1 0          *TIPO TARIFFA
267400970401     C                   MOVEL     UNOCTR        KTBKEY
267500970401     C     KEYTAB        CHAIN     TABEL00F                           99
267600970401IF  2C     *IN99         IFEQ      *OFF
267700970401     C                   MOVEL     TBLUNI        DSTR
267800970401     C                   Z-ADD     §TRATA        D00ATA                         *MIN.APPL.TAR.FINITA
267900970401E   2C                   ENDIF
268000970401E   1C                   ENDIF
268100970117     C*--------------------
268200970117     C* TIPI SERVIZIO
268300970117     C*--------------------
268400970117     C                   Z-ADD     *ZEROS        I                 4 0
268500020923     C                   Z-ADD     1             KTBKUT
268600970117     C                   MOVEL     '5E'          KTBCOD
268700970117     C     KE2TAB        SETLL     TABEL00F
268800970117     C     KE2TAB        READE     TABEL00F                               99
268900970117DO  1C     *IN99         DOWEQ     *OFF
269000970117IF  2C     TBLFLG        IFEQ      *BLANKS
269100970117     C                   ADD       1             I
269200970117     C                   MOVEL     TBLUNI        DS5E
269300970117     C                   MOVEL     TBLKEY        TSP(I)                         *CODICE
269400970117     C                   MOVEL     §5EDES        DTSP(I)                        *DESCRIZIONE
269500090611      *imposta costanti a video per tipo servizio 'E'
269600090611     c                   if        tsp(i) = 'E'
269700090611     c                   movel     §5ed08        costserv
269800090611     c                   eval      costserv2 = '%sped in ' + costserv +
269900090611     c                             '............:'
270000090611     c                   eval      costs = 'TNTE01R- Tariffa '+ costserv
270100090611     c                   endif
270200970117E   2C                   ENDIF
270300970117     C     KE2TAB        READE     TABEL00F                               99
270400970117E   1C                   ENDDO
270500970117     C*--------------------
270600970117     C* TIPI TARIFFA
270700970117     C*--------------------
270800970117     C                   Z-ADD     *ZEROS        I                 4 0
270900020923     C                   Z-ADD     1             KTBKUT
271000970117     C                   MOVEL     'TR'          KTBCOD
271100970117     C     KE2TAB        SETLL     TABEL00F
271200970117     C     KE2TAB        READE     TABEL00F                               99
271300970117DO  1C     *IN99         DOWEQ     *OFF
271400970117IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
271500970117     C                   MOVEL     TBLUNI        DSTR
271600970117IF  3C     §TRUTE        IFEQ      'S'                                          *IN GESTIONE
271700970117     C                   ADD       1             I
271800970117     C                   MOVEL     TBLKEY        TTR(I)                         *CODICE
271900970117     C                   MOVEL     §TRDE3        DTTR(I)                        *DESCRIZIONE
272000970117E   3C                   ENDIF
272100970117E   2C                   ENDIF
272200970117     C     KE2TAB        READE     TABEL00F                               99
272300970117E   1C                   ENDDO
272400970117     C*--------------------
272500970117     C* TARIFFA DI CARTELLO
272600970117     C*--------------------
272700970117     C* OFFERTE -> IL PGM HA DAVANTI DEGLI OVRDBF CHE SOSTITUISCONO LE OFFERTE
272800970117     C* CON LE TARIFFE. PER LEGGERE LE TARIFFE OCCORRE LEGGERE LE OFFERTE
272900970117     C* TARIFFE -> IL PGM NON HA DAVANTI NESSUN OVRDBF
273000021202     C                   Z-ADD     *ZEROS        CARKSC
273100021202     C                   Z-ADD     *ZEROS        CARCTR
273200021202     C                   Z-ADD     *ZEROS        CARPRG
273300021202     C                   MOVEL     *BLANKS       CARDIV
273400021202     C                   CLEAR                   TRUL27DS
273500021202     C                   EVAL      I27TLA = 'L'
273600021202     C                   EVAL      I27DTA = DATCOR
273700021202     C                   CALL      'TRUL27R'
273800021202     C                   PARM                    TRUL27DS
273900021202     C*
274000021202IF  1C                   IF        O27ERR <> 'E'
274100021202     C                   Z-ADD     O27KSC        KTMKSC
274200091103IF  2C                   IF        %subst(d00op0:1 :1)='O'
274300970117     C     KTMKSC        SETLL     TNOFM01L
274400970117     C     KTMKSC        READE     TNOFM01L                               99
274500970117X   2C                   ELSE
274600970117     C     KTMKSC        SETLL     TNTAM01L
274700970117     C     KTMKSC        READE     TNTAM01L                               99
274800970117E   2C                   ENDIF
274900970117DO  2C     *IN99         DOWEQ     *OFF
275000970117IF  3C     TAMATB        IFNE      'A'                                          *NO ANNULLATE
275100970117     C     TAMBAP        ANDNE     'B'                                          *NO BLOCCATE
275200970117IF  4C     DATCOR        IFGE      TAMDDT                                       *IN DECORRENZA
275300970117     C     DATCOR        ANDLE     TAMDST
275400970117     C                   Z-ADD     TAMKSC        CARKSC
275500970117     C                   Z-ADD     TAMCTR        CARCTR
275600970117     C                   Z-ADD     TAMPRG        CARPRG
275700011003     C                   MOVEL     TAMFLO        DSTA01
275800011003     C                   MOVEL     §TADIV        CARDIV
275900970117     C                   LEAVE
276000970117E   4C                   ENDIF
276100970117E   3C                   ENDIF
276200091103IF  3C                   IF        %subst(d00op0: 1: 1)= 'O'
276300970117     C     KTMKSC        READE     TNOFM01L                               99
276400970117X   3C                   ELSE
276500970117     C     KTMKSC        READE     TNTAM01L                               99
276600970117E   3C                   ENDIF
276700970117E   2C                   ENDDO
276800970117E   1C                   ENDIF
276900021202     C*
277000970117     C                   ENDSR
277100020923     C*--------------------------------------------------------------------------------------------*
277200020923     C* REPERISCE I DATI UTENTE
277300020923     C*--------------------------------------------------------------------------------------------*
277400020923     C     REPDATIUTE    BEGSR
277500020923     C*
277600020923     C* INIZIALIZZA VARIABILI DI WRK
277700020923     C                   CLEAR                   TIBS34DS
277800020923     C                   CLEAR                   AZUTEDS
277900020923     C                   CLEAR                   DDATIUTE
278000020923     C*
278100020923     C     *DTAARA       DEFINE    §azute        azuteds
278200020923     C     *DTAARA       DEFINE    §datiute      ddatiute
278300020923     C                   IN(E)     *DTAARA
278400020923if  1C                   IF        %Error
278500020923     c                   EVAL      I34Tla = 'L'
278600020923     C                   CALL      'TIBS34R'
278700020923     C                   PARM                    Tibs34Ds
278800020923     C                   IN        *DTAARA
278900020923e   1C                   ENDIF
279000020923     C*
279100020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
279200020923     C                   MOVEL(P)  RSUT          DSFIRS           20
279300020923     C*
279400020923     C                   ENDSR
279500970117     C*------------------------------------------------------------------------*
279600970117     C* RUTINZ - OPERAZIONI INIZIALI
279700970117     C*------------------------------------------------------------------------*
279800970117     C     RUTINZ        BEGSR
279900970117     C*--------------------
280000970117     C* RICEVIMENTO PARAMETRI
280100970117     C*--------------------
280200970117     C     *ENTRY        PLIST
280300970117     C                   PARM                    KPJBA
280400970117     C                   PARM                    DSTE00
280500041202     c* apertura controllata di alcuni file xchè solo di filiale
280600041202     c                   open      tfaco00f                             95
280700041202     c                   open      tfclp00f                             95
280800041202     c                   open      tfind00f                             95
280900041202     c                   open      tnofm01l                             95
281000970117     C*--------------------
281100970117     C* VARIABILI RIFERITE AL DATABASE
281200970117     C*--------------------
281300970117     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
281400970117     C     *LIKE         DEFINE    TBLCOD        KTBCOD
281500970117     C     *LIKE         DEFINE    TBLKEY        KTBKEY
281600970117     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
281700970117     C     *LIKE         DEFINE    ACOKUT        KACKUT                         *CNAC000F
281800970117     C     *LIKE         DEFINE    ACOKCC        KACKCC
281900970117     C     *LIKE         DEFINE    ACOKSC        KACKSC
282000970414     C     *LIKE         DEFINE    CLPKUT        KCLKUT                         *CNCLP00F
282100970414     C     *LIKE         DEFINE    CLPKCC        KCLKCC
282200970414     C     *LIKE         DEFINE    CLPKSC        KCLKSC
282300091103     C     *LIKE         DEFINE    d04NRV        KVINRV                         *TNVIS00F
282400990818     C     *LIKE         DEFINE    TADKSC        KTDKSC                         *TITAD00F
282500970117     C     *LIKE         DEFINE    TADCTR        KTDCTR
282600970117     C     *LIKE         DEFINE    TADPRG        KTDPRG
282700970117     C     *LIKE         DEFINE    TADLNP        KTDLNP
282800011001     C     *LIKE         DEFINE    ETCDSF        KTCDSF                         *TEETC00F
282900980429     C     *LIKE         DEFINE    ETCCTO        KTCCTO
283000970117     C     *LIKE         DEFINE    ETCKSC        KTCKSC
283100970117     C     *LIKE         DEFINE    ETCCTR        KTCCTR
283200970117     C     *LIKE         DEFINE    ETCPRG        KTCPRG
283300970117     C     *LIKE         DEFINE    ETCLNP        KTCLNP
283400970117     C     *LIKE         DEFINE    TAMKSC        CARKSC
283500970117     C     *LIKE         DEFINE    TAMCTR        CARCTR
283600970117     C     *LIKE         DEFINE    TAMPRG        CARPRG
283700970401     C     *LIKE         DEFINE    TAMKSC        KTMKSC                         *TNTAM00F
283800970401     C     *LIKE         DEFINE    TAMCTR        KTMCTR
283900970401     C     *LIKE         DEFINE    TAMPRG        KTMPRG
284000011003     C     *LIKE         DEFINE    §TADIV        CARDIV
284100970725     C     *LIKE         DEFINE    D00TUR        WTUR                           *LAVORO
284200011011     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
284300011011     C     *LIKE         DEFINE    TBEKE1        KTEKE1
284400970117     C*--------------------
284500970117     C* CHIAVI DI LETTURA
284600970117     C*--------------------
284700970117     C* LETTURA TABEL00F - COMPLETA
284800970117     C     KEYTAB        KLIST
284900970117     C                   KFLD                    KTBKUT                         *UTENTE
285000970117     C                   KFLD                    KTBCOD                         *TABELLA
285100970117     C                   KFLD                    KTBKEY                         *CODICE
285200970117     C* LETTURA TABEL00F - PARZIALE
285300970117     C     KE2TAB        KLIST
285400970117     C                   KFLD                    KTBKUT                         *UTENTE
285500970117     C                   KFLD                    KTBCOD                         *TABELLA
285600970117     C* LETTURA AZORG01L - COMPLETA
285700970117     C     KEYORG        KLIST
285800970117     C                   KFLD                    KORFIL                         *FILIALE
285900970414     C* LETTURA CNACO00F - COMPLETA
286000970117     C     KEYACO        KLIST
286100970117     C                   KFLD                    KACKUT                         *CODICE UTENTE
286200970117     C                   KFLD                    KACKCC                         *CODICE CAPOCONTO
286300970117     C                   KFLD                    KACKSC                         *CODICE CLIENTE
286400970414     C* LETTURA CNCLP00F - COMPLETA
286500970414     C     KEYCLP        KLIST
286600970414     C                   KFLD                    KCLKUT                         *CODICE UTENTE
286700970414     C                   KFLD                    KCLKCC                         *CODICE CAPOCONTO
286800970414     C                   KFLD                    KCLKSC                         *CODICE CLIENTE
286900011001     C* LETTURA TEETC01L - COMPLETA
287000970117     C     KEYETC        KLIST
287100980429     C                   KFLD                    KTCDSF                         *DATI SEDE/FILIALE
287200980429     C                   KFLD                    KTCCTO                         *TARIFFA O OFFERTA
287300970117     C                   KFLD                    KTCKSC                         *CLIENTE
287400970117     C                   KFLD                    KTCCTR                         *TARIFFA
287500970117     C                   KFLD                    KTCPRG                         *PROGRESSIVO
287600970117     C                   KFLD                    KTCLNP                         *LINEA PARTENZA
287700990818     C* LETTURA TITAD04L - PARZIALE
287800970117     C     KEYTAD        KLIST
287900970117     C                   KFLD                    KTDKSC                         *CLIENTE
288000970117     C                   KFLD                    KTDCTR                         *TARIFFA
288100970117     C                   KFLD                    KTDPRG                         *PROGRESSIVO
288200970117     C                   KFLD                    KTDLNP                         *LINEA PARTENZA
288300990818     C* LETTURA TITAD04L - PARZIALE
288400970204     C     KE2TAD        KLIST
288500970204     C                   KFLD                    KTDKSC                         *CLIENTE
288600970204     C                   KFLD                    KTDCTR                         *TARIFFA
288700970204     C                   KFLD                    KTDPRG                         *PROGRESSIVO
288800970401     C* LETTURA TNTAM01L - COMPLETA
288900970401     C     KEYTAM        KLIST
289000970401     C                   KFLD                    KTMKSC                         *CLIENTE
289100970401     C                   KFLD                    KTMCTR                         *TARIFFA
289200970401     C                   KFLD                    KTMPRG                         *PROGRESSIVO
289300011011     C* LETTURA TNTBE01L - PARZIALE
289400011011     C     KEYTBE        KLIST
289500011011     C                   KFLD                    KTECOD                         *UTENTE
289600011011     C                   KFLD                    KTEKE1                         *CODICE
289700970117     C*--------------------
289800970117     C* VARIABILI NON RIFERITE AL DATABASE
289900970117     C*--------------------
290000970117     C                   MOVEL     'N'           $FINE             1            *FINE PROGRAMMA
290100970117     C                   MOVEL     '0'           $REC              1            *STATO RECORD
290200970117     C                   MOVEL     '0'           WERR              1            *ERRORE
290300970117     C                   MOVEL     '1'           TIPVID            1            *VIDEO DA GESTIRE
290400990818     C                   MOVEL     '0'           $TAD              1            *ESISTENZA TITAD
290500970117     C                   MOVEL     'N'           $CONF             1            *CONFERMA CARICAMENT
290600970117     C                   MOVEL     'N'           $FINV             1            *FINE VIDEATA
290700970117     C                   MOVEL     ' '           $TGES             1            *GESTIONE (I/G)
290800970117     C*
290900970117     C* REPERISCE I DATI DELL'UTENTE
291000020923     C                   EXSR      REPDATIUTE
291100970117     C*
291200970117     C* REPERISCE LA DATA CORRENTE
291300970117     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
291400970117     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
291500970117     C                   Z-ADD     WN8           G08DAT
291600970117     C                   Z-ADD     *ZEROS        G08INV
291700970117     C                   MOVEL     '0'           G08ERR
291800970117     C                   CALL      'XSRDA8'
291900970117     C                   PARM                    WLBDA8
292000970117     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
292100970117     C*
292200970117     C* 02->GESTIONE, 05->INTERROGAZIONE
292300970117     C                   MOVE      D00OP0        WA2               2
292400970117IF  1C     WA2           IFEQ      '02'
292500970117     C                   MOVEL     'G'           $TGES                          *GESTIONE
292600970117X   1C                   ELSE
292700970117     C                   MOVEL     'I'           $TGES                          *INTERROGAZIONE
292800970117E   1C                   ENDIF
292900970129     C*
293000970602     C* SE PROFILO EDP IMPOSTA IL LANCIO DEL PROGRAMMA IN DEBUG = 'SI'
293100970507     C                   MOVEL     'N'           D00DBG
293200970620     C                   MOVEL     KNMUS         WUTE              3
293300970620IF  1C     WUTE          IFEQ      'EDP'
293400970910     C                   MOVEL     'S'           D00DBG
293500970602E   1C                   ENDIF
293600970117     C*
293700970117     C* CARICA LE TABELLE OCCORRENTI
293800011011     C                   EXSR      CARTAB
293900011011     C*
294000011011     C* CARICA LE NUOVE TABELLE OCCORRENTI
294100011011     C                   EXSR      CARGEDCE
294200970117     C*
294300970117     C* CONTROLLI INIZIALI DI ENTRATA
294400970117     C                   EXSR      CTRENT
294500970117     C*
294600970117     C* IMPOSTA LA TESTATA
294700970319     C                   EXSR      IMPVD0
294800970414     C*
294900970414     C* PROPONE LA LINEA DI PARTENZA DEL COMMERCIALE DEL CLIENTE
295000011031     C                   MOVEL     *BLANKS       WRKCTS            2
295100020923     C                   Z-ADD     1             KCLKUT
295200020923     C                   Z-ADD     DUTKCI        KCLKCC
295300970414     C                   Z-ADD     D00KSC        KCLKSC
295400091103IF  1C                   IF        %subst(d00op0: 1:1)='T'                      *TARIFFA
295500970414     C     KEYCLP        CHAIN     CNCLP00F                           99
295600011030     C     KEYCLP        CHAIN     CNIND00F                           99
295700970414X   1C                   ELSE                                                   *OFFERTA
295800970414     C     KEYCLP        CHAIN     TFCLP00F                           99
295900011030     C     KEYCLP        CHAIN     TFIND00F                           99
296000970414IF  2C     *IN99         IFEQ      *ON
296100091103     c                   clear                   tnte04ds
296200091103     C                   movel     D00dsf        d04dsf
296300091103     C                   Z-ADD     D00KSC        d04nrv
296400091103     c                   call      'TNTE04R'
296500091103     c                   parm                    kpjba
296600091126     c                   parm                    tnte04ds
296700091103     C                   Z-ADD     d04KSC        KCLKSC                         *CLIENTE DELLA VISIT
296800091120     c                   movel     d04rag        v0dksc
296900970414     C     KEYCLP        CHAIN     CNCLP00F                           99
297000011031     C     KEYCLP        CHAIN     CNIND00F                           99
297100970414E   2C                   ENDIF
297200970414E   1C                   ENDIF
297300970414IF  1C     *IN99         IFEQ      *OFF
297400970414     C                   MOVEL     CLPAGE        V1CLNP
297500011030     C                   MOVEL     INDCAE        WRKCTS
297600970414E   1C                   ENDIF
297700011031     C* Imposto il codice tassaizione del cliente tariffa / offerta (se visita di un potenziale
297800011031     C* il cliente può essere vuoto e quindi il codice tassazione = *blanks
297900011030     C                   EVAL      D00PRI = WRKCTS
298000970319     C*
298100970319     C* IMPOSTA NELLA FINESTRA CARICA DA ALTRA TARIFFA IL CODICE DELLA CARTELLO
298200091120     C                   MOVEL     'S'           D0DDSF
298300970414     C                   MOVEL     'T'           D0DCTO
298400970414     C                   Z-ADD     CARKSC        D0DKSC
298500970414     C                   Z-ADD     CARCTR        D0DCTR
298600970414     C                   Z-ADD     CARPRG        D0DPRG
298700970414     C                   Z-ADD     V1CLNP        D0DLNP
298800011003     C                   MOVEL     CARDIV        D0DDIV
298900970117     C*
299000970117     C                   ENDSR
299100970117     O*------------------------------------------------------------------------*
299200970117** -DDEC-
299300970117ITALIA              1
299400970117ESTERO              2
299500980429SEDE                3
299600980429FILIALE             4
2997009808185
2998009808186
2999009808187
3000009808188
3001009808189
300200980818TUTTE LE FILIALI    10
300300970117** -DVID-
300400970117***   GENERALI   ***1
300500970117PORTO               2
300600970117DELTA               3
3007009701174
3008009701175
300900970117** -DOPE-
301000970117* INSERIMENTO *1
301100970117* VARIAZIONE * 2
301200970117INTERROGAZIONE 3
301300970117 * GESTIONE *  4
3014009701175
301500970619** -MSG-
301600970117TNTE01R- Immettere I per elaborare la tariffa italia, E per quella estero     1
301700970117TNTE01R- Dati tipo richiesti non trovati                                      2
301800970117TNTE01R- Filiale errata                                                       3
301900091123TNTE01R- Il valore scelto non è corretto                                      4
302000970117TNTE01R- Qualità cliente errata. Immettere uno dei valori possibili           5
302100970117TNTE01R- Passaggio dati errato. Immettere uno dei valori possibili            6
302200970117TNTE01R- Immettere il peso medio del collo                                    7
302300970117TNTE01R- Immettere il volume medio del collo                                  8
302400970117TNTE01R- Non esistono i dati tipo per la tariffa/visita richiesta             9
302500970117TNTE01R- Immettere il numero di spedizioni medie per ritiro                   10
302600970117TNTE01R- Immettere il numero di spedizioni medie per consegna                 11
302700980803TNTE01R- Immettere la % delle spedizioni in franco e/o assegnato              12
302800970117TNTE01R- La % fra franchi e assegnati non può essere maggiore di 100          13
302900970117TNTE01R- La % fra franchi e assegnati deve essere uguale a 100                14
303000970211TNTE01R- Impossibile calcolare il porto/delta per un punto operativo estero   15
303100970117TNTE01R- Tariffa italia: la linea di partenza non può essere estera           16
303200970117TNTE01R- Inserire S per inglobare la consegna al porto, N per non inglobarla  17
303300970117TNTE01R- Per inglobare la consegna inserire la percentuale                    18
303400970117TNTE01R- La % fra c/assegni Bartolini e mittente non può essere > di 100      19
303500970117TNTE01R- Inserire S per inglobare il c/assegno al porto, N per non inglobarlo 20
303600970117TNTE01R- Per inglobare il c/assegno inserire almeno una percentuale           21
303700970117TNTE01R- Inserire il valore medio del contrassegno                            22
303800060315TNTE01R- La % fra assicurazione R.C.V., AC BASE e AC PLUS deve essere = 100   23
303900970117TNTE01R- Inserire S per inglobare l'assicurazione al porto, N per non farlo   24
304000970117TNTE01R- Per inglobare l'assicurazione inserire almeno una percentuale        25
304100970117TNTE01R- Inserire il valore medio dell'importo da assicurare                  26
304200970117TNTE01R- La percentuale non può essere maggiore di 100                        27
304300970117TNTE01R- Tariffa espresso. Inserire 100 nella % di spedizioni di questo tipo  28
304400970117TNTE01R- Tariffa espresso. L'inglobo nel porto è obbligatorio                 29
304500970117TNTE01R- Inserire S per inglobare l'espresso al porto, N per non inglobarlo   30
304600970117TNTE01R- Per inglobare l'espresso inserire la percentuale                     31
304700970117TNTE01R- Non inserire il valore medio contrassegno                            32
304800970117TNTE01R- Non inserire il valore medio importo da assicurare                   33
304900970117TNTE01R- Inserire S per inglobare la giacenza al porto, N per non inglobarla  34
305000970117TNTE01R- Per inglobare la giacenza inserire la percentuale                    35
305100970117TNTE01R- Inserire i giorni medi di sosta                                      36
305200970117TNTE01R- Non inserire i giorni medi di sosta                                  37
305300980429TNTE01R- Immettere S per gestire i dati creati da sede, F x quelli di filiale 38
305400980429TNTE01R- "S" per caricare i dati creati da sede, "F" per quelli di filiale    39
305500970314TNTE01R- Inserire S per inglobare l'inoltro al porto, N per non inglobarlo    40
305600970314TNTE01R- Per inglobare l'inoltro inserire la percentuale                      41
305700970117TNTE01R- Segno errato                                                         42
305800970314TNTE01R- Se inserito il segno immettere anche la % di scostamento e viceversa 43
305900980504TNTE01R- Descrizione obbligatoria                                             44
306000970117TNTE01R- Inserire il numero delle riaperture giacenza                         45
306100970117TNTE01R- Non inserire il numero delle riaperture giacenza                     46
306200970314TNTE01R- Data riferimento calcolo errata                                      47
306300970314TNTE01R- Tipo arrotondamento errato                                           48
306400970314TNTE01R- Se inserito tipo arrotondamento inserire anche l'importo e viceversa 49
306500980803TNTE01R- La percentuale può essere solo uguale a 100                          50
306600980803TNTE01R- Inserire il tipo porto: 100% di spedizioni o franco o assegnato      51
306700011008TNTE01R- Impossibile inserire importi con decimali quando la divisa è ITL.    52
306800011009TNTE01R- Se si indica la percentuale è obbligatorio inglobare al porto.       53
306900011009TNTE01R- Inserire S per inglobare il ritiro al porto, N per non inglobarlo.   54
307000011108TNTE01R- I valori consentiti sono 0,10 0,25 0,50 0,75 1,00                    55
307100011031TNTE01R- Impossibile impostare valore, il cliente non risiede in CAP con isole56
307200021104TNTE01R- Immettere uno dei valori possibili                                   57
307300021104TNTE01R- Impossibile indicare Procedura Disk "C" x soluzione "B".             58
307400011210TNTE01R- ERRORE. Tariffa/Offerta inesistente                                  59
307500970117TNTE01R- ERRORE. Tipo tariffa non gestibile                                   60
307600970117TNTE01R- ERRORE. In tariffa esistono più di 10 scaglioni                      61
307700970117TNTE01R- ERRORE. Il cod.tass XXX-XX ha scaglioni diversi dal primo cod.tass.  62
307800970117TNTE01R- ERRORE. In tariffa esistono scaglioni con indicazione del C.A.P.     63
307900970117TNTE01R- ERRORE. In tariffa non esistono righe dettaglio x la linea richiesta 64
308000011011TNTE01R- ERRORE. Divisa attualmente non accettabile                           65
308100020213TNTE01R- ERRORE. Utilizzo del CAT impossibile per tariffe/offerte FedEX       66
308200021104TNTE01R- Per soluzione "B" etichetta solo il cliente                          67
308300021104TNTE01R- Per procedura Disk "C" etichettano l'Autotrasp. o il cliente         68
308400021104TNTE01R- Per non procedura Disk "C" etichettano l'Autotrasp. o la Coop        69
308500970117TNTE01R- Record momentaneamente allocato, riprovare più tardi                 70
308600040802TNTE01R- La % fra bancali Bartolini e oltre 20 colli non può essere > di 100  71
308700050805TNTE01R- Inserire S per inglobare destinatario disagiato, N per non iglobarlo 72
308800040824TNTE01R- Se immessa %sped ... obbligatorio nr. medio bancali e viceversa      73
308900050225I dati di SEDE sono + recenti di quelli di FILIALE! Ricordarsi d'aggiornarli  74
309000050808TNTE01R- Inserire S oppure N per packing list                                 75
309100050802TNTE01R- Inserire S per inglobare bancali da rendere, N per non iglobarli     76
309200050808TNTE01R- Per inglobare i bancali da rendere inserire percentuale e il numero  77
309300050805TNTE01R- Per inglobare il destinatario disagiato inserire la percentuale      78
309400060313TNTE01R- Non è possibile inserire entrambi i tipi di assicurazione            79
309500060825Il codice tariffa scelto incongruente con quello originale                    80
309600131121TNTE01R- La % di CONSEGNA ISOLA e FERMO DEPOSITO non può essere > di 100      81
309700131121TNTE01R- La % di RITIRO ISOLA e MERCE PORTATA A MAG. non può essere > di 100  82
309800131121TNTE01R- La % di su APPUNTAMENTO e ai SUPERMARCATI non può essere > di 100    83
309900150227TNTE01R- Se immessa %sped ... obbligatorio il tipo bancali                    84
310000151119TNTE01R- Inserire S oppure N per ritiro telefonico / mail                     85
310100151119TNTE01R- Inserire S oppure N per inglobare al porto                           86
