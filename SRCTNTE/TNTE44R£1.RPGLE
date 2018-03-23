000100980806     H*------------------------------------------------------------------------*
000200980806     H* TARIFFE DA CONTO ECONOMICO - CREAZIONE DATI TIPO - FILTRO              *
000300980806     H*------------------------------------------------------------------------*
000400980806     H DATEDIT(*YMD)
000500980806     F*------------------------------------------------------------------------*
000600980806     F* DATA BASE
000700980806     F*------------------------------------------------------------------------*
000800980806     FTNTE44D   CF   E             WORKSTN
000900980806     FTABEL00F  IF   E           K DISK
001000011029     FTNTBE01L  IF   E           K DISK
001100980806     FAZORG01L  IF   E           K DISK
001200980806     FCNACO00F  IF   E           K DISK
001201060807     FCNclp00F  IF   E           K DISK
001300980806     FTNTAM01L  IF   E           K DISK
001500060804     FTNOFM01L  IF   E           K DISK
001600980806     F                                     RENAME(TNTAM000:TNOFM000)
001700980806     D*------------------------------------------------------------------------*
001800980806     D* SCHIERE
001900980806     D*------------------------------------------------------------------------*
002000980806     D*-------------------
002100980806     D* MESSAGGI VIDEO
002200980806     D*-------------------
002300980810     D MSG             S             78    DIM(30) CTDATA PERRCD(1)
002400980806     D*-------------------
002500980806     D* TIPI TARIFFA
002600980806     D*-------------------
002700980806     D TTR             S              1    DIM(10)                              *CODICE
002800980806     D DTTR            S             20    DIM(10)                              *DESCRIZIONE
002900980806     D*-------------------
003000980806     D* TABELLE "XCLIR"- RICERCA CLIENTI
003100980806     D*-------------------
003200980806     D KSC             S              7  0 DIM(30)                              *NUMERICO
003300980806     D KSA             S              4    DIM(30)                              *ALFANUMERICO
003400980806     D*------------------
003500980806     D* TABELLE CAPICONTO
003600980806     D*------------------
003700980806     D RI              S              4  0 DIM(4)                               *TAB RAG. CI
003800020228     D*-------------------
003900020228     D* SCHIERA FILIALI FEDEX
004000020228     D*-------------------
004100020228     D FILFED          S              3    DIM(999)
004200980806     D*------------------------------------------------------------------------*
004300980806     D* INPUT
004400980806     D*------------------------------------------------------------------------*
004500040723     d tibs55ds      E DS
004600980806     D*-------------------
004700980806     D* DS TIPI TARIFFA
004800980806     D*-------------------
004900980806     D DSTR          E DS
004901060807     d dLat          e ds
004902060807     d ds01          e ds
004903060807     d dute01        e ds
004904060807     d tibs02ds      e ds
004905060807     d wabi            s                   like(UteAut)
004906060807     d wcmm            s                   LIKE(clpage)
004907060920     d dtaiso          s               d
005000011011     D*-------------------
005100011011     D* DS REPERIMENTO DIVISA TARIFFE/OFFERTE
005200011011     D*-------------------
005300011011     D DSTA01        E DS
005400980806     D*-------------------
005500980806     D* DS GIORNI DI PULIZIA ARCHIVI
005600980806     D*-------------------
005700980806     D DS5A          E DS
005800980828     D*-------------------
005900980828     D* DS AVANZAMENTO TRASFERIMENTI
006000980828     D*-------------------
006100980828     D DS$3          E DS
006200020924     D*-------------------
006300020924     D* DS REPERIMENTO DATI UTENTE
006400020924     D*-------------------
006500020924     D TIBS34DS      E DS                                                       *Profili utente
006600020924     D DDATIUTE      E DS                                                       *Dati utente
006700020924     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006701060807     d Trul31ds      e ds
006702060807     D  S31                   10    759  0 dim(250)
006801060807      * P.O. gestibili dall'utente
006802060807     d l1              s              3  0 dim(250) inz(*Zeros)
007900980806     D*------------------
008000980806     D* DS "XSRDA8" - CONTROLLA DATA (8)
008100980806     D*------------------
008200980806     D WLBDA8          DS
008300980806     D  G08DAT                 1      8  0
008400980806     D  G08INV                 9     16  0
008500980806     D  G08ERR                17     17
008600980806     D  G08TGI                18     22  0
008700980806     D*------------------
008800980806     D* DS "XSRGI8" - RICREA DATA(8) DA TOTALI GIORNI
008900980806     D*------------------
009000980806     D WGIDA8          DS
009100980806     D  GIODAT                 1      8  0
009200980806     D  GIOINV                 9     16  0
009300980806     D  GIOTGI                17     21  0
009400980806     D*------------------
009500980806     D* DS "XCLIR"- RICERCA CLIENTI
009600980806     D*------------------
009700980806     D DSXC          E DS                  EXTNAME(XCLIRDS)
009800980806     D                 DS
009900980806     D  DSKSN                  1      4P 0
010000980806     D  DSKSA                  1      4
010100980806     D*------------------
010200980806     D* DS PARAMETRI DI LANCIO
010300980806     D*------------------
010400980806     D PARAM           DS                  INZ
010500980806     D  PARKSC                 1      7  0
010600980806     D  PARFPA                 8     10  0
010700980806     D  PARFAR                11     13  0
010800980806     D  PARFAP                14     16  0
010900980806     D  PARDS1                17     24  0
011000980806     D  PARDS2                25     32  0
011100980806     D  PARTAR                33     35
011200980806     D  PARCTO                36     36
011300980806     D  PARKOV                37     43  0
011400980806     D  PARCTR                44     46  0
011500980806     D  PARPRG                47     49  0
011600980806     D  PARLNP                50     52  0
011700980806     D  PARFIE                53     53
011800980806     D  PARSIM                54     54
011900040723     D  PARELA                55     55
012000040723     D  PARDIV                56     58
012100040723     D  PARUNI                59     59
012200040805     D  PARsta                60     60
012300040805     D  PARint                61     61
012400040805     D  PARsie                62     62
012401091130     D  PARdsf                63     63
012500980806     D*-------------------
012600980806     D* ARCHITETTURA
012700980806     D*-------------------
012800980806     D KPJBA         E DS
012801091125     D tnte04ds      E DS
012900011029     D*------------------
013000011029     D* DS LETTURA TABELLA "GED" TIPO RECORD "CE"
013100011029     D*------------------
013200011029     D DSGEDCE       E DS
013300020228     D*-------------------
013400020228     D* DS ESTERNA X DETERMINAZIONE NETWORK FILIALE DA ORGANIGRAMMA
013500020228     D*-------------------
013600020228     D OG143         E DS
013700020730     D*------------------
013800020730     D* DS PER LETTURA TABELLA "NTW"
013900020730     D*------------------
014000020730     D DNTW          E DS
014100021029     D*-------------------
014200021029     D* DS REPERIMENTO/CONTROLLO PADRE/FIGLI
014300021029     D*-------------------
014400021029     D TIBS10DS      E DS
014500040726     D*
014600040726     D KNSIFS          S                   LIKE(KNSIF)
014700980806     C*------------------------------------------------------------------------*
014800980806     C* MAIN LINE
014900980806     C*------------------------------------------------------------------------*
015000980806     C*
015100980806     C* IMPOSTAZIONI PRIMA VIDEATA
015200980806     C                   EXSR      IMPVD1
015300980806     C*
015400980806     C* CICLO FINO A RICHIESTA DI FINE ELABORAZIONE
015500980806DO  1C     $FINE         DOUEQ     'S'
015600980806     C     TIPVID        CASEQ     '1'           GESVD1                         *1° VIDEO
015700980806     C                   ENDCS
015800980806E   1C                   ENDDO
015900980806     C*
016000980806     C                   SETON                                        LR
016100980806     C*------------------------------------------------------------------------*
016200980806     C* IMPVD1 - IMPOSTAZIONI PRIMA VIDEATA
016300980806     C*------------------------------------------------------------------------*
016400980806     C     IMPVD1        BEGSR
016500980806     C*
016600980806     C* REIMPOSTA FORMATO VIDEO E INDICATORI
016700980806     C                   EXSR      SETVD1
016800980806     C                   MOVEL     '1'           TIPVID                         *GESTIONE VIDEO
016900021031     C                   MOVEL     'N'           V1CUNI
017000980806     C*
017100980806     C                   ENDSR
017200980806     C*------------------------------------------------------------------------*
017300980806     C* SETVD1 - IMPOSTAZIONI INDICATORI PRIMA VIDEATA
017400980806     C*------------------------------------------------------------------------*
017500980806     C     SETVD1        BEGSR
017600980806     C*
017700980806     C                   SETOFF                                       202122     ERRORI
017800980806     C                   SETOFF                                       232425
017900980806     C                   SETOFF                                       262728
018000980806     C                   SETOFF                                       293031
018100980810     C                   SETOFF                                       323334
018200021031     C                   SETOFF                                       9035       GENERICO DI ERRORE
018300980806     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
018301091125     c                   if        comdsf = ' '
018302091125     c                   eval      d2wcto = 'Tariffa o Offerta ............:'
018303091125     c                   eval      s2wcto = '(T/O)'
018304091125     c                   eval      d2wksc = 'Cliente o Visita .............:'
018305091125     c                   eval      d2wprg = 'Progressivo ..................:'
018306091125     c                   else
018307091201     c                   eval      d2wcto = 'Tariffa o Offerta ............:'
018308091130     c                   eval      s2wcto = '(T/O)'
018309091125     c                   eval      d2wksc = 'Cliente o Trattativa .........:'
018310091125     c                   eval      d2wprg = 'Versione .....................:'
018311091125     c                   end
018400980806     C*
018500980806     C                   ENDSR
018600980806     C*------------------------------------------------------------------------*
018700980806     C* GESVD1 - GESTIONE PRIMA VIDEATA
018800980806     C*------------------------------------------------------------------------*
018900980806     C     GESVD1        BEGSR
019000980806     C*
019100980806     C* EMISSIONE VIDEATA
019200980806IF  1C     $TESVD        IFEQ      '1'
019300980806     C                   WRITE     TE44D00                                      *TESTATA
019400980806     C                   MOVEL     '0'           $TESVD
019500980806E   1C                   ENDIF
019600980806     C                   EXFMT     TE44D01
019700980806     C                   EXSR      SETVD1
019800980806     C* F03-FINE
019900980806IF  1C     *INKC         IFEQ      *ON
020000980806     C                   EXSR      F03GES
020100980806     C                   GOTO      FINVD1
020200980806E   1C                   ENDIF
020300980806     C* ENTER-CONTROLLA VIDEO
020400980806     C                   EXSR      CTRVD1
020500980806IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
020600980806     C                   GOTO      FINVD1
020700980806E   1C                   ENDIF
020800980806     C* F06-CONFERMA
020900980806IF  1C     *INKF         IFEQ      *ON
021000980806     C                   EXSR      F06GES
021100980806     C                   GOTO      FINVD1
021200980806E   1C                   ENDIF
021300980806     C*
021400980806     C     FINVD1        ENDSR
021500980806     C*------------------------------------------------------------------------*
021600980806     C* CTRVD1 - CONTROLLA PRIMA VIDEATA
021700980806     C*------------------------------------------------------------------------*
021800980806     C     CTRVD1        BEGSR
021900980806     C*
022000980806     C* REIMPOSTA LE VARIABILI DI LAVORO
022100980806     C                   MOVEL     '0'           WERR
022200980810     C***
022300980810     C* LANCIO
022400980810     C***
022500980810     C*--------------------
022600980810     C* DATI SEDE O FILIALE
022700980810     C*--------------------
022800980811IF  1C     V1CELA        IFNE      'S'
022900980811     C     V1CELA        ANDNE     'F'
023000980810     C                   SETON                                        3490
023100980810     C                   MOVEL     MSG(18)       $MSG
023200980810     C                   GOTO      FINCT1
023300980810E   1C                   ENDIF
023400980810     C*--------------------
023500980810     C* DATI SEDE/FILIALE + SIMULAZIONE LANCIO
023600980810     C*--------------------
023700040806IF  1C*    V1CELA        IFEQ      'S'
023800040806     C*    dutlpo        andne     'S'
023900040806IF  2C*    V1CSIM        IFEQ      'S'
024000040806     C*                  SETON                                        243490
024100040806     C*                  MOVEL     MSG(21)       $MSG
024200040806     C*                  GOTO      FINCT1
024300040806E   2C*                  ENDIF
024400040806E   1C*                  ENDIF
024500980828     C* IMPOSTAZIONE DATE
024600980828IF  1C     V1CDS1        IFEQ      *ZEROS
024700980828     C     V1CDS2        ANDEQ     *ZEROS
024800980828     C                   EXSR      DATVID
024900980828E   1C                   ENDIF
025000980810     C*--------------------
025100980810     C* SIMULAZIONE LANCIO
025200980810     C*--------------------
025300980810IF  1C     V1CSIM        IFNE      'S'
025400980810     C     V1CSIM        ANDNE     'N'
025500040805     C     V1CSIM        ANDNE     'T'
025600980810     C                   SETON                                        2490
025700980810     C                   MOVEL     MSG(7)        $MSG
025800980810     C                   GOTO      FINCT1
025900980810E   1C                   ENDIF
026000980806     C***
026100980806     C* BOLLE DA ELABORARE
026200980806     C***
026300980810     C*--------------------
026400980810     C* DATI SEDE/FILIALE + CLIENTE
026500980810     C*--------------------
026600980811IF  1C     V1CELA        IFEQ      'S'                                          *DATI DA SEDE
026700040723IF  1C     dutlpo        andne     'S'                                          *DATI DA SEDE
026800980810IF  2C     V1CKSC        IFEQ      *BLANKS                                      *NON SELEZIONATO
026900980810     C     V1CKSC        OREQ      *ZEROS
027000980819     C                   SETON                                        2090
027100980810     C                   MOVEL     MSG(20)       $MSG
027200980810     C                   GOTO      FINCT1
027300980810E   2C                   ENDIF
027400980810E   1C                   ENDIF
027500980810     C*--------------------
027600980810     C* CLIENTE
027700980810     C*--------------------
027800980810     C                   Z-ADD     *ZEROS        KSCV1C
027900980810     C                   MOVEL     *BLANKS       V1DKSC
028000980806IF  1C     V1CKSC        IFNE      *BLANKS                                      *SELEZIONATO
028100980806     C     V1CKSC        ANDNE     *ZEROS
028200980806     C                   MOVEL     V1CKSC        WKSC
028300980806     C                   MOVEL     *BLANKS       WDKSC
028400980806     C                   EXSR      CHKKSC
028500980806     C                   MOVEL     WKSC          V1CKSC
028600980806     C                   MOVEL     WDKSC         V1DKSC
028700980806     C                   MOVEL     WKSC          KSCV1C
028800980806IF  2C     WERR          IFEQ      '1'
028900980806     C                   SETON                                        2090
029000980806     C                   MOVEL     MSG(1)        $MSG
029100980806     C                   GOTO      FINCT1
029200980806E   2C                   ENDIF
029300980810E   1C                   ENDIF
029400021029     C*--------------------
029500021029     C* CLIENTE PADRE/FIGLIO
029600021029     C*--------------------
029700021029IF  1C     V1CUNI        IFEQ      'S'
029800021029IF  2C     V1CKSC        IFNE      *BLANKS                                      *SELEZIONATO
029900021029     C     V1CKSC        ANDGT     *ZEROS
030000021029     C                   CLEAR                   TIBS10DS
030100021029     C                   EVAL      D10DRF = DATCOR                              *data riferimento
030200021029     C                   EVAL      D10TLE = 'ST'                                *tipo legame
030300021029     C                   EVAL      D10PAF = 'F'                                 *cerca il padre
030400021029     C                   MOVE(P)   V1CKSC        D10COD                         *cod cliente da tst
030500021029     C                   CALL      'TIBS10R'
030600021029     C                   PARM                    TIBS10DS
030700021029     C*
030800021029     C                   IF        D10ERR <> *blanks OR                         *No unif. o padre
030900021029     C                             D10COP =  *zeros
031000021029     C                   SETON                                        209035
031100021029     C                   MOVEL     MSG(22)       $MSG
031200021029     C                   GOTO      FINCT1
031300021029     C                   ENDIF
031400021029E   2C                   ENDIF
031500021029E   1C                   ENDIF
031600980810     C*--------------------
031700980810     C* DATI SEDE/FILIALE + DATA SPEDIZIONE DAL/AL
031800980810     C*--------------------
031900980811IF  1C     V1CELA        IFEQ      'S'                                          *DATI DA SEDE
032000980810IF  2C     V1CDS1        IFEQ      *ZEROS                                       *NON SELEZIONATO
032100980810     C     V1CDS2        OREQ      *ZEROS
032200980819     C                   SETON                                        2290
032300980810     C                   MOVEL     MSG(20)       $MSG
032400980810     C                   GOTO      FINCT1
032500980810E   2C                   ENDIF
032600980810E   1C                   ENDIF
032700980810     C*--------------------
032800980810     C* DATA SPEDIZIONE DAL
032900980810     C*--------------------
033000980810     C                   Z-ADD     *ZEROS        DS1V1C
033100980810IF  1C     V1CDS1        IFGT      *ZEROS                                       *SELEZIONATA
033200980810     C                   Z-ADD     V1CDS1        WDA8
033300980810     C                   Z-ADD     *ZEROS        DA8W
033400980810     C                   EXSR      CHKDAT
033500980810     C                   Z-ADD     WDA8          V1CDS1
033600980810     C                   Z-ADD     DA8W          DS1V1C
033700980810IF  2C     WERR          IFEQ      '1'
033800980810     C                   SETON                                        2290
033900980810     C                   MOVEL     MSG(3)        $MSG
034000980810     C                   GOTO      FINCT1
034100980810E   2C                   ENDIF
034200980810IF  2C     WERR          IFEQ      '0'
034600980828     C                   Z-ADD     DA2INI        WINI              8 0
034800980828IF  3C     DS1V1C        IFLT      WINI
034900980810     C                   SETON                                        2290
035000980810     C                   MOVEL     MSG(16)       $MSG
035100980810     C                   GOTO      FINCT1
035200980810E   3C                   ENDIF
035300980810E   2C                   ENDIF
035400980810E   1C                   ENDIF
035500980810     C*--------------------
035600980810     C* DATA SPEDIZIONE AL
035700980810     C*--------------------
035800980810     C                   Z-ADD     *ZEROS        DS2V1C
035900980810IF  1C     V1CDS2        IFGT      *ZEROS                                       *SELEZIONATA
036000980810     C                   Z-ADD     V1CDS2        WDA8
036100980810     C                   Z-ADD     *ZEROS        DA8W
036200980810     C                   EXSR      CHKDAT
036300980810     C                   Z-ADD     WDA8          V1CDS2
036400980810     C                   Z-ADD     DA8W          DS2V1C
036500980810IF  2C     WERR          IFEQ      '1'
036600980810     C                   SETON                                        2390
036700980810     C                   MOVEL     MSG(3)        $MSG
036800980810     C                   GOTO      FINCT1
036900980810E   2C                   ENDIF
037000980810IF  2C     WERR          IFEQ      '0'
037400980828     C                   Z-ADD     DA2FIN        WFIN              8 0
037600980828IF  3C     DS2V1C        IFGT      WFIN
037700980810     C                   SETON                                        2390
038100980828     C                   MOVEL     MSG(23)       $MSG
038300980810     C                   GOTO      FINCT1
038400980810E   3C                   ENDIF
038500980810E   2C                   ENDIF
038600980810E   1C                   ENDIF
038700980810     C*--------------------
038800980810     C* DATA SPEDIZIONE DAL/AL
038900980810     C*--------------------
039000980810IF  1C     V1CDS1        IFGT      *ZEROS
039100980810     C     V1CDS2        ANDEQ     *ZEROS
039200980810     C     V1CDS2        ORGT      *ZEROS
039300980810     C     V1CDS1        ANDEQ     *ZEROS
039400980810     C                   SETON                                        222390
039500980810     C                   MOVEL     MSG(4)        $MSG
039600980810     C                   GOTO      FINCT1
039700980810E   1C                   ENDIF
039800040830IF  1C     DS2V1C        IFLT      DS1V1C
039900980810     C                   SETON                                        222390
040000980810     C                   MOVEL     MSG(5)        $MSG
040100980810     C                   GOTO      FINCT1
040200980810E   1C                   ENDIF
040300980806     C*--------------------
040400980806     C* LINEA DI PARTENZA
040500980806     C*--------------------
040600980806     C                   Z-ADD     *ZEROS        FPAV1C
040700980806     C                   MOVEL     *BLANKS       V1DFPA
040800980806IF  1C     V1CFPA        IFGT      *BLANKS                                      *SELEZIONATA
040900980806     C                   MOVEL     V1CFPA        WFIL
041000980806     C                   MOVEL     *BLANKS       WDFIL
041100980806     C                   MOVEL     *BLANKS       WFL1
041200980806     C                   EXSR      CHKFIL
041300040723     c                   if        dutlpo <> 'S'
041400021106     C                   EXSR      CHKFPA
041500040723     c                   end
041600980806     C                   MOVEL     WFIL          V1CFPA
041700980806     C                   MOVEL     WDFIL         V1DFPA
041800980806     C                   MOVEL     WFIL          FPAV1C
041900980806IF  2C     WERR          IFEQ      '1'
042000980806     C                   SETON                                        2190
042100980806     C                   MOVEL     MSG(2)        $MSG
042200980806     C                   GOTO      FINCT1
042300020228     C                   ELSE
042400020228     C* VERIFICO SE LNA E' FILIALE FEDEX
042500020228     C                   IF        WFIL > *ZEROS  OR
042600020228     C                             WFIL > *BLANKS
042700020228     C     WFIL          LOOKUP    FILFED                                 55
042800020228     C                   IF        *IN55 = *ON
042900020228     C                   SETON                                        2190
043000020228     C                   MOVEL     MSG(25)       $MSG
043100020228     C                   GOTO      FINCT1
043200020228     C                   ENDIF
043300020228     C                   ENDIF
043400020228     C*
043500980806E   2C                   ENDIF
043600980806E   1C                   ENDIF
043700980806     C*--------------------
043800980806     C* LINEA DI ARRIVO
043900980806     C*--------------------
044000980806     C                   Z-ADD     *ZEROS        FARV1C
044100980806     C                   MOVEL     *BLANKS       V1DFAR
044200980806IF  1C     V1CFAR        IFGT      *BLANKS                                      *SELEZIONATA
044300980806     C                   MOVEL     V1CFAR        WFIL
044400980806     C                   MOVEL     *BLANKS       WDFIL
044500980806     C                   MOVEL     *BLANKS       WFL1
044600980806     C                   EXSR      CHKFIL
044700980806     C                   MOVEL     WFIL          V1CFAR
044800980806     C                   MOVEL     WDFIL         V1DFAR
044900980806     C                   MOVEL     WFIL          FARV1C
045000980806IF  2C     WERR          IFEQ      '1'
045100980806     C                   SETON                                        3290
045200980806     C                   MOVEL     MSG(2)        $MSG
045300980806     C                   GOTO      FINCT1
045400020228     C                   ELSE
045500020228     C* VERIFICO SE LNA E' FILIALE FEDEX
045600020228     C                   IF        WFIL > *ZEROS  OR
045700020228     C                             WFIL > *BLANKS
045800020228     C     WFIL          LOOKUP    FILFED                                 55
045900020228     C                   IF        *IN55 = *ON
046000020313     C                   SETON                                        3290
046100020228     C                   MOVEL     MSG(25)       $MSG
046200020228     C                   GOTO      FINCT1
046300020228     C                   ENDIF
046400020228     C                   ENDIF
046500020228     C*
046600980806E   2C                   ENDIF
046700980806E   1C                   ENDIF
046800980806     C*--------------------
046900980806     C* LINEA DI ARRIVO O DI PARTENZA
047000980806     C*--------------------
047100980806     C                   Z-ADD     *ZEROS        FAPV1C
047200980806     C                   MOVEL     *BLANKS       V1DFAP
047300980806IF  1C     V1CFAP        IFGT      *BLANKS                                      *SELEZIONATA
047400980806     C                   MOVEL     V1CFAP        WFIL
047500980806     C                   MOVEL     *BLANKS       WDFIL
047600980806     C                   MOVEL     *BLANKS       WFL1
047700980806     C                   EXSR      CHKFIL
047800980806     C                   MOVEL     WFIL          V1CFAP
047900980806     C                   MOVEL     WDFIL         V1DFAP
048000980806     C                   MOVEL     WFIL          FAPV1C
048100980806IF  2C     WERR          IFEQ      '1'
048200980806     C                   SETON                                        3390
048300980806     C                   MOVEL     MSG(2)        $MSG
048400980806     C                   GOTO      FINCT1
048500020228     C                   ELSE
048600020228     C* VERIFICO SE LNA E' FILIALE FEDEX
048700020228     C                   IF        WFIL > *ZEROS  OR
048800020228     C                             WFIL > *BLANKS
048900020228     C     WFIL          LOOKUP    FILFED                                 55
049000020228     C                   IF        *IN55 = *ON
049100020313     C                   SETON                                        3390
049200020228     C                   MOVEL     MSG(25)       $MSG
049300020228     C                   GOTO      FINCT1
049400020228     C                   ENDIF
049500020228     C                   ENDIF
049600020228     C*
049700980806E   2C                   ENDIF
049800980806E   1C                   ENDIF
049900020228     C*
050000980806IF  1C     V1CFAP        IFGT      *BLANKS
050100980806IF  2C     V1CFPA        IFGT      *BLANKS
050200980806     C     V1CFPA        ORGT      *BLANKS
050300980806     C                   SETON                                        3390
050400980806     C                   MOVEL     MSG(15)       $MSG
050500980806     C                   GOTO      FINCT1
050600980806E   2C                   ENDIF
050700980806E   1C                   ENDIF
050800980806     C*--------------------
050900980806     C* CODICE TARIFFA
051000980806     C*--------------------
051100980806IF  1C     V1CTAR        IFNE      *BLANKS                                      *SELEZIONATA
051200980806IF  2C     V1CTAR        IFLT      *ZEROS
051300980806     C                   SETON                                        2590
051400980806     C                   MOVEL     MSG(14)       $MSG
051500980806     C                   GOTO      FINCT1
051600980806E   2C                   ENDIF
051700980806E   1C                   ENDIF
051800980806     C*--------------------
051900980806     C* CLIENTE + FILIALE/I DATA SPEDIZIONE + CODICE TARIFFA
052000980806     C*--------------------
052100980806IF  1C     V1CKSC        IFGT      *ZEROS
052200980806     C     V1CFPA        ORGT      *ZEROS
052300980806     C     V1CFAR        ORGT      *ZEROS
052400980806     C     V1CFAP        ORGT      *ZEROS
052500980806     C     V1CDS1        ORGT      *ZEROS
052600980806     C     V1CTAR        ORNE      *BLANKS
052700980806X   1C                   ELSE
052800980806     C                   SETON                                        202122
052900980806     C                   SETON                                        232532
053000980806     C                   SETON                                        3390
053100980806     C                   MOVEL     MSG(6)        $MSG
053200980806     C                   GOTO      FINCT1
053300980806E   1C                   ENDIF
053400980806     C***
053500980806     C* CODICE TARIFFA / OFFERTA
053600980806     C***
053700980806     C*--------------------
053800980806     C* TARIFFA O OFFERTA
053900980806     C*--------------------
053901091125     c                   if        (comdsf = ' '
053902091125IF  1C                             and v1ccto <> 'T' and
053903091125IF  1C                             v1ccto <> 'O') or
053904091125     c                             (comdsf = 'X'
053905091125IF  1C                             and v1ccto <> 'T' and
053906091130IF  1C                             v1ccto <> 'O')
053907091201     C                   SETON                                        9031
053908091125     C                   MOVEL     MSG(8)        $MSG
053909091125     C                   GOTO      FINCt1
053910091125E   1C                   ENDIF
055700980806     C*--------------------
055800980806     C* CODICE TARIFFA
055900980806     C*--------------------
056000980806     C                   MOVEL     V1CCTR        WN1               1 0
056100980806     C                   MOVEL     WN1           WA1               1
056200980806     C                   SETOFF                                           98
056300980806     C                   Z-ADD     1             I
056400980806     C     WA1           LOOKUP    TTR(I)                                 98
056500980806IF  1C     *IN98         IFEQ      *OFF                                         *NON TROVATA
056600980806     C                   SETON                                        2790
056700980806     C                   MOVEL     MSG(9)        $MSG
056800980806     C                   GOTO      FINCT1
056900980806E   1C                   ENDIF
057000980806     C*--------------------
057100980806     C* CLIENTE O VISITA
057200980806     C*--------------------
057300980806IF  1C     V1CCTO        IFEQ      'T'                                          T A R I F F A
057400980806     C                   Z-ADD     V1CKOV        KTMKSC
057500980806     C                   Z-ADD     V1CCTR        KTMCTR
057600980806     C                   Z-ADD     V1CPRG        KTMPRG
057700980806     C     KEYTAM        CHAIN     TNTAM01L                           99
057800980806IF  2C     *IN99         IFEQ      *ON                                          *INESISTENTE
057900981012     C     V1CKOV        OREQ      CARKSC                                       *CREAZIONE CARTELLO
058000981012     C     V1CELA        ANDEQ     'S'                                          *IN SEDE
058100980806     C                   SETON                                        262728
058200980806     C                   SETON                                        90
058300980806     C                   MOVEL     MSG(10)       $MSG
058400980806     C                   GOTO      FINCT1
058500011011     C                   ELSE
058600020228     C* VERIFICO SE TARIFFA E' FEDEX
058700020228     C                   MOVEL     TAMFLO        DSTA01
058800020228     C                   IF        §TAFED = 'S'
058900020228     C                   SETON                                        262728
059000020228     C                   SETON                                        90
059100020228     C                   MOVEL     MSG(26)       $MSG
059200020228     C                   GOTO      FINCT1
059300020313     C                   ENDIF
059400020314     C* VERIFICO SE LINEA DEL CLIENTE O VISITA SIA IN GESTIONE (£1)
059500020314     C* SE TRATTASI DI TARIFFA DI CARTELLO VA CMQ BENE
059600040723     c                   if        dutlpo <> 'S'
060000020314     C                   MOVEL     V1CKOV        DEPFIL            3 0
060100020314     C     DEPFIL        LOOKUP    L1                                     55
060200020313     C                   IF        *IN55 = *OFF
060300020313     C                   SETON                                        2690
060400020313     C                   MOVEL     MSG(27)       $MSG
060500020313     C                   GOTO      FINCT1
060700020313     C                   ENDIF
060800040723     C                   ENDIF
060900020228     C*
061000011011     C                   MOVEL     §TADIV        V1CDIV
061100980806E   2C                   ENDIF
061200980806X   1C                   ELSE                                                   O F F E R T A
061300980806     C                   Z-ADD     V1CKOV        KFMKSC
061400980806     C                   Z-ADD     V1CCTR        KFMCTR
061500980806     C                   Z-ADD     V1CPRG        KFMPRG
061600980806     C     KEYOFM        CHAIN     TNOFM01L                           99
061700980806IF  2C     *IN99         IFEQ      *ON                                          *INESISTENTE
061800980806     C                   SETON                                        262728
061900980806     C                   SETON                                        90
062000980806     C                   MOVEL     MSG(11)       $MSG
062100980806     C                   GOTO      FINCT1
062200011029     C                   ELSE
062300011029     C                   MOVEL     TAMFLO        DSTA01
062400011029     C                   MOVEL     §TADIV        V1CDIV
062500020314E   2C                   ENDIF
062600020314     C*
062601091125     c                   clear                   tnte04ds
062602091125     C                   movel     comdsf        d04dsf
062603091125     C                   Z-ADD     v1ckov        d04nrv
062605091125     c                   call      'TNTE04R'
062606091125     c                   parm                    kpjba
062607091126     c                   parm                    tnte04ds
062800091125     C                   IF        d04err = ' '
062900091125     C                   IF        d04KSC > *zeros
063000091125     C                   MOVEL     d04KSC        DEPFIL
063100020314     C                   ELSE
063200091125     C                   MOVEL     d04CMM        DEPFIL
063300020314     C                   ENDIF
063400020314     C     DEPFIL        LOOKUP    L1                                     55
063500020314     C                   IF        *IN55 = *OFF
063600020314     C                   SETON                                        262728
063700020314     C                   SETON                                        90
063800020314     C                   MOVEL     MSG(27)       $MSG
063900020314     C                   GOTO      FINCT1
064001060807     C                   ENDIF
064100020314     C                   ELSE
064200020314     C                   SETON                                        262728
064300020314     C                   SETON                                        90
064400020314     C                   MOVEL     MSG(27)       $MSG
064500020314     C                   GOTO      FINCT1
064600020314     C                   ENDIF
064700020314     C*
064800980806E   1C                   ENDIF
064900980806     C*--------------------
065000980806     C* LINEA DI PARTENZA
065100980806     C*--------------------
065200980810     C                   MOVEL     *BLANKS       V1DLNP
065300040723IF  1C     V1CLNP        IFNE      999                                          *SELEZIONE PER SEDE
065400980806     C                   MOVEL     V1CLNP        WFIL
065500980806     C                   MOVEL     *BLANKS       WDFIL
065600980806     C                   MOVEL     *BLANKS       WFL1
065700980806     C                   EXSR      CHKFIL
065800980806     C                   MOVEL     WFIL          V1CLNP
065900040723     C                   MOVEL     WDFIL         V1DLNP
066000980806     C                   MOVEL     WFL1          DV1FL1
066100980806IF  2C     WERR          IFEQ      '1'
066200980806     C                   SETON                                        2990
066300980806     C                   MOVEL     MSG(2)        $MSG
066400980806     C                   GOTO      FINCT1
066500020228     C                   ELSE
066600020228     C* VERIFICO SE LNA E' FILIALE FEDEX
066700020228     C                   IF        WFIL > *ZEROS  OR
066800020228     C                             WFIL > *BLANKS
066900020228     C     WFIL          LOOKUP    FILFED                                 55
067000020228     C                   IF        *IN55 = *ON
067100020228     C                   SETON                                        2990
067200020228     C                   MOVEL     MSG(25)       $MSG
067300020228     C                   GOTO      FINCT1
067400020228     C                   ENDIF
067500020228     C                   ENDIF
067600020228     C*
067700980806E   2C                   ENDIF
067800040723X   1C                   ELSE
067900060912     c*                  if        dutlpo = 'S'
068000060912     C*                  MOVEL(P)  'TUTTE'       V1DLNP
068100060912     c*                  else
068200040723     C                   SETON                                        2990
068300040723     C                   MOVEL     MSG(2)        $MSG
068400040723     C                   GOTO      FINCT1
068500060912     c*                  end
068600040723E   1C                   ENDIF
068700980806     C*--------------------
068800980806     C* TARIFFA ITALIA/ESTERO
068900980806     C*--------------------
069000980806IF  1C     V1CFIE        IFNE      'I'
069100980806     C     V1CFIE        ANDNE     'E'
069200980806     C     V1CFIE        ANDNE     ' '
069300980806     C                   SETON                                        3090
069400980806     C                   MOVEL     MSG(12)       $MSG
069500980806     C                   GOTO      FINCT1
069600980806E   1C                   ENDIF
069700980806     C*--------------------
069800980806     C* LINEA DI PARTENZA + TARIFFA ITALIA/ESTERO
069900980806     C*--------------------
070000980806IF  1C     V1CFIE        IFEQ      'I'
070100980806     C     DV1FL1        ANDNE     *BLANKS
070200980806     C                   SETON                                        293090
070300980806     C                   MOVEL     MSG(13)       $MSG
070400980806     C                   GOTO      FINCT1
070500980806E   1C                   ENDIF
070600011029     C*--------------------
070700011029     C* CONTROLLO VALIDITA' DIVISA
070800011029     C*--------------------
070900011029     C     V1CDIV        IFEQ      'ITL'
071000011029     C     DATCOR        ANDGE     §GEDCEDAT
071100011029     C                   SETON                                        262728
071200011029     C                   SETON                                        90
071300011029     C                   MOVEL     MSG(24)       $MSG
071400011029     C                   GOTO      FINCT1
071500011029     C                   ENDIF
071600980806     C*
071700980806     C     FINCT1        ENDSR
071800980828     C*------------------------------------------------------------------------*
071900980828     C* DATVID - IMPOSTA LE DATE SPEDIZIONE A VIDEO
072000980828     C*------------------------------------------------------------------------*
072100980828     C     DATVID        BEGSR
073700980828     C                   Z-ADD     DA2INI        G08INV                         *DATA INIZIALE
073800980828     C                   Z-ADD     *ZEROS        G08DAT
073900980828     C                   MOVEL     '3'           G08ERR
074000980828     C                   CALL      'XSRDA8'
074100980828     C                   PARM                    WLBDA8
074200980828     C                   Z-ADD     G08DAT        V1CDS1
074300980828     C                   Z-ADD     DA2FIN        G08INV                         *DATA FINALE
074400980828     C                   Z-ADD     *ZEROS        G08DAT
074500980828     C                   MOVEL     '3'           G08ERR
074600980828     C                   CALL      'XSRDA8'
074700980828     C                   PARM                    WLBDA8
074800980828     C                   Z-ADD     G08DAT        V1CDS2
075000980828     C*
075100980828     C                   ENDSR
075101060807     C*------------------------------------------------------------------------*
075102060807     C* srcom  - CONTROLLA anche il po del commerciale
075103060807     C*------------------------------------------------------------------------*
075104060807     C     srcom         BEGSR
075105060807     c*
075107060807     C                   Z-ADD     DUTKCI        KACKCC
075108060807     C                   MOVEL     WKSC          KACKSC
075109060807     c     keyaco        chain     cnclp000
075110060807    3c                   if        not %found(cnclp00f)
075111060807     c                   clear                   wporagru          3 0
075112060807     c                   else
075113060807     c                   eval      wcmm=clpage
075114060807     c                   exsr      cercmm
075115060807     c                   movel     §01rgf        wporagru
075117060807     C     Wporagru      LOOKUP    l1                                     55
075118060808    2C     *IN55         IFEQ      *OFF
075119060807     c                   eval      werr = '1'
075120091201     C                   ELSE
075121091201     c                   clear                   werr
075122060807    3C                   ENDIF
075123091201    3c                   endif
075124060807     c                   endsr
075125060807     C*------------------------------------------------------------------------*
075126060807     C*--- RICERCA codice commerciale su tabella ---------------------*
075127060807     C*------------------------------------------------------------------------*
075128060807     C     CERCMM        BEGSR
075130060807     C                   Z-ADD     1             KTBKUT
075131060807     C                   MOVEL     '01'          KTBCOD
075132060807     C                   MOVEL(P)  wcmm          KTBKEY
075133060807     C     Keytab        CHAIN     TABEL00f
075134060807     c                   if        not %found(tabel00f)
075135060807     C                   clear                   TBLUNI
075136060807     C                   clear                   ds01
075137060807     c                   else
075138060807     C                   movel     tbluni        ds01
075139060807     c                   end
075140060807     C                   ENDSR
075200980806     C*------------------------------------------------------------------------*
075300980806     C* CHKKSC - CONTROLLA IL CLIENTE
075400980806     C*------------------------------------------------------------------------*
075500980806     C     CHKKSC        BEGSR
075600980806     C*
075700980806     C     *LIKE         DEFINE    V1CKSC        WKSC
075800980806     C     *LIKE         DEFINE    V1DKSC        WDKSC
075900980806     C*
076000980806     C* RICHIESTO ELENCO CODICI
076100980806     C     '?'           SCAN      WKSC                                   98
076200980806IF  1C     *IN98         IFEQ      *ON
076300980806     C                   CLEAR                   DSXC                           *CLIENTI
076400020924     C                   Z-ADD     DUTKCI        DXCCAP                         *CAPOCONTO
076401060912     c                   eval      dxcaut = wabi
076500980806     C                   CALL      'XCLIR'
076600980806     C                   PARM                    DSXC
076700980806     C* SCOMPATTA
076800980806     C                   MOVEA     DXCKSC        KSA                            *DS INPUT -> SCH ALF
076900980806DO  2C     1             DO        30            J                 4 0
077000980806     C                   MOVE      KSA(J)        DSKSA                          *SCH ALF -> DS ALF
077100980806     C                   Z-ADD     DSKSN         KSC(J)                         *DS NUM -> SCH NUM
077200980806E   2C                   ENDDO
077300980806     C                   MOVEL     KSC(1)        WKSC
077400980806     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
077500980806E   1C                   ENDIF
077600980806     C*
077700980806     C* CODICE CLIENTE NUMERICO
077800980806IF  1C     WKSC          IFLT      *ZEROS
077900980806     C                   MOVEL     '1'           WERR
078000980806X   1C                   ELSE
078100980806     C*
078200980806     C* CONTROLLA ESISTENZA CODICE IN ARCHIVIO
078300020924     C                   Z-ADD     1             KACKUT
078400020924     C                   Z-ADD     DUTKCI        KACKCC
078500980806     C                   MOVEL     WKSC          KACKSC
078600980806     C     KEYACO        CHAIN     CNACO00F                           99
078700980806IF  2C     *IN99         IFEQ      *ON
078800980806     C                   MOVEL     '1'           WERR
078900980806X   2C                   ELSE
079000980806     C                   MOVEL     ACORAG        WDKSC
079100980806E   2C                   ENDIF
079200980806E   1C                   ENDIF
079300021031     C*
079400021031     C* VERIFICA SE LA LINEA DEL CLIENTE RICHIESTO E' IN GESTIONE
079500021031     C                   MOVEL     WKSC          DEPFIL
079600021031     C     DEPFIL        LOOKUP    L1                                     55
079700021031     C                   IF        *IN55 = *OFF
079800021031     C                   MOVEL     '1'           WERR
079900021031     C                   ENDIF
079901060807     c* verifico anche il po commerciale
079902060807     c                   if        werr = '1'
079903060807     c                   exsr      srcom
079904060807     c                   end
080000980806     C*
080100980806     C                   ENDSR
080200980806     C*------------------------------------------------------------------------*
080300980806     C* CHKFIL - CONTROLLA LA FILIALE
080400980806     C*------------------------------------------------------------------------*
080500980806     C     CHKFIL        BEGSR
080600980806     C*
080700980806     C     *LIKE         DEFINE    V1CFPA        WFIL
080800020730     C     *LIKE         DEFINE    §NTWFIE       WFL1
080900980806     C     *LIKE         DEFINE    V1DFPA        WDFIL
081000980806     C*
081100980806     C* RICHIESTO ELENCO CODICI
081200980806     C     '?'           SCAN      WFIL                                   98
081300980806IF  1C     *IN98         IFEQ      *ON
081400980806     C                   MOVEL     *BLANKS       §OFIL
081500980806     C                   MOVEL     *BLANKS       §OTIP
081600980806     C                   MOVEL     *BLANKS       §ODES
081700980806     C                   MOVEL     *BLANKS       §OPRA
081800980806     C                   CALL      'TNSD24R'
081900980806     C                   PARM                    §OFIL             3
082000980806     C                   PARM                    §OTIP             1
082100980806     C                   PARM                    §ODES            25
082200980806     C                   PARM                    §OPRA             3
082300980806     C                   MOVEL     §OFIL         WFIL                           *CODICE
082400980806     C                   MOVEL     §ODES         WDFIL                          *DESCRIZIONE
082500980806     C                   MOVE      '1'           $TESVD                         *RIEMETTERE TESTATA
082600980806E   1C                   ENDIF
082700980806     C*
082800980806     C* CODICE FILIALE NUMERICO
082900980806IF  1C     WFIL          IFLT      *ZEROS
083000980806     C                   MOVEL     '1'           WERR
083100980806X   1C                   ELSE
083200980806     C*
083300980806     C* CONTROLLA ESISTENZA CODICE IN ARCHIVIO
083400980806     C                   MOVEL     WFIL          KORFIL
083500980806     C     KEYORG        CHAIN     AZORG01L                           98
083600980806IF  2C     *IN98         IFEQ      *ON                                          *NON TROVATA
083700980806     C     ORGFVA        ORNE      *BLANKS                                      *NO ANNULLATI
083800980806     C                   MOVEL     '1'           WERR
083900980806X   2C                   ELSE
084000980806     C                   MOVEL     ORGDES        WDFIL                          *DESCRIZIONE
084100020730     C                   MOVEL     ORGDE3        OG143
084200020730     C*
084300020730     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
084400020730     C                   CLEAR                   DNTW
084500020730     C*
084600020730     C                   MOVEL(P)  'NTW'         KTECOD
084700020730     C                   MOVEL(P)  §OGNTW        KTEKE1
084800020730     C     KEYTBE        CHAIN     TNTBE01L
084900020730     C                   IF        %found(TNTBE01L)
085000020730     C                   MOVEL     TBEUNI        DNTW
085100020730     C                   ENDIF
085200020730     C*
085300020730     C                   MOVEL     §NTWFIE       WFL1                           *FLAG ITALIA/ESTERO
085400980806E   2C                   ENDIF
085500980806E   1C                   ENDIF
085600980806     C*
085700980806     C                   ENDSR
085800021106     C*------------------------------------------------------------------------*
085900021106     C* CHKFPA - ALTRI CONTROLLI SULLA FILIALE DI PARTENZA
086000021106     C*------------------------------------------------------------------------*
086100021106     C     CHKFPA        BEGSR
086200021106     C*
086300021106     C                   MOVE(P)   V1CFPA        WNUMFIL           3 0
086400021106     C*
086500021106     C* SE INDICATO ANCHE IL CLIENTE => VERIFICA CHE LA LINEA DI PARTENZA SIA DELL'ELABORATORE
086600060807     C*                  IF        V1CKSC > *ZEROS
086700060807     C*    WNUMFIL       LOOKUP    SKI                                    55
086800060807     C*                  IF        *IN55 = *OFF
086900060807     C*                  MOVEL     '1'           WERR
087000060807     C*                  ENDIF
087100021106     C*
087200021106     C* SE NON INDICATO IL CLIENTE => VERIFICA CHE LA LINEA DI PARTENZA SIA IN GESTIONE
087300060807     C*                  ELSE
087400021106     C     WNUMFIL       LOOKUP    L1                                     55
087500021106     C                   IF        *IN55 = *OFF
087600021106     C                   MOVEL     '1'           WERR
087700021106     C                   ENDIF
087800060807     C*                  ENDIF
087900021106     C*
088000021106     C                   ENDSR
088100980806     C*------------------------------------------------------------------------*
088200980806     C* CHKDAT - CONTROLLA LA DATA
088300980806     C*------------------------------------------------------------------------*
088400980806     C     CHKDAT        BEGSR
088500980806     C*
088600980806     C     *LIKE         DEFINE    V1CDS1        WDA8
088700980806     C     *LIKE         DEFINE    V1CDS1        DA8W
088800980806     C*
088900980806     C                   Z-ADD     WDA8          G08DAT
089000980806     C                   Z-ADD     *ZEROS        G08INV
089100980806     C                   MOVEL     '0'           G08ERR
089200980806     C                   CALL      'XSRDA8'
089300980806     C                   PARM                    WLBDA8
089400980806     C                   Z-ADD     G08DAT        WDA8
089500980806IF  1C     G08ERR        IFEQ      '1'
089600980806     C                   MOVEL     '1'           WERR
089700980806X   1C                   ELSE
089800980806     C                   Z-ADD     G08INV        DA8W
089900980806E   1C                   ENDIF
090000980806     C*
090100980806     C                   ENDSR
090200980806     C*------------------------------------------------------------------------*
090300980806     C* F03GES - GESTIONE TASTO FUNZIONALE F03 ==> FINE
090400980806     C*------------------------------------------------------------------------*
090500980806     C     F03GES        BEGSR
090600980806     C*
090700980806     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
090800980810     C*
090900980806     C                   ENDSR
091000980806     C*------------------------------------------------------------------------*
091100980806     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA
091200980806     C*------------------------------------------------------------------------*
091300040723     c     F06GES        BEGSR
091400040723     C* Richiamo TIBS55R per conoscere S.Informativi
091500040723     C* in cui immettere il lavoro
091600040723     C                   CLEAR                   TIBS55ds
091700040723     C                   MOVEL     'L'           I50TLA
091800040723     C                   MOVEL     dutpou        I50PPO
091900040723     C                   MOVEL     '046'         I50APO
092000040723     C                   CALL      'TIBS55R'
092100040723     C                   PARM                    TIBS55ds
092200040726     C* MI SALVO IL S.I. ORIGINALE XCHÈ SE NO NON LO REIMPOSTA
092300040726     c                   eval      knsifS = KNSIF
092400980806     C*
092500040723     C* LANCIA IL PROGRAMMA
092600980806     C                   Z-ADD     KSCV1C        PARKSC
092700980806     C                   Z-ADD     FPAV1C        PARFPA
092800980806     C                   Z-ADD     FARV1C        PARFAR
092900980806     C                   Z-ADD     FAPV1C        PARFAP
093000980806     C                   Z-ADD     DS1V1C        PARDS1
093100980806     C                   Z-ADD     DS2V1C        PARDS2
093200980806     C                   MOVEL     V1CTAR        PARTAR
093300980806     C                   MOVEL     V1CCTO        PARCTO
093400980806     C                   Z-ADD     V1CKOV        PARKOV
093500980806     C                   Z-ADD     V1CCTR        PARCTR
093600980806     C                   Z-ADD     V1CPRG        PARPRG
093700980806     C                   Z-ADD     V1CLNP        PARLNP
093800980806     C                   MOVEL     V1CFIE        PARFIE
093801091130     C                   MOVEL     comdsf        PARdsf
093900040805     c* se simulazione solo aggiornamento imposto il flag si stampa = ' '
094000040805     c* altrimenti se richiesto simulazione o agg.+stampa imposto S
094100040805     c                   if        v1csim <> 'N'
094200040805     c                   movel     'S'           parsta
094300040805     c                   end
094400040805     c* se simulazione = stampa + agg. imposto il flag di simulazione = N
094500040805     c* per aggiornare
094600040805     c                   if        v1csim = 'T'
094700040805     c                   movel     'N'           parsim
094800040805     c                   else
094900980810     C                   MOVEL     V1CSIM        PARSIM
095000040805     c                   end
095100011011     C                   MOVEL     V1CDIV        PARDIV
095200021029     C                   MOVEL     V1CUNI        PARUNI
095300040726     C                   MOVEL     v1cela        PARELA
095400040806     C                   MOVEL     TAMFIE        PARSIE
095800040726     c                   eval      knsif = O50ASI
095900060808     C                   MOVEL     'TE6N'        KCOAZ
096200980806     C                   MOVEL     PARAM         KPJBU
096201060830     C                   MOVE      'TNTE41C   '  KPJBU
096500051102     c                   movel(p)  'BCH10'       Wpgm             10
096600051102     c                   call      Wpgm
096700980806     C                   PARM                    KPJBA
096800040726     c                   eval      knsif = KNSIFS
096900980806     C*
097000980806     C                   ENDSR
097100011029     C*------------------------------------------------------------------------*
097200011029     C* CARGEDCE - CARICAMENTO TABELLA "GED" TIPO RECORD "CE" E CONSIDERAZIONI SU DIVISE
097300011029     C*------------------------------------------------------------------------*
097400011029     C     CARGEDCE      BEGSR
097500011029     C*
097600011029     C                   CLEAR                   DSGEDCE
097700011029     C                   MOVEL     *blanks       WDIVCE            3
097800011029     C*
097900011029     C                   MOVEL(P)  'GED'         KTECOD
098000011029     C                   MOVEL(P)  'CE'          KTEKE1
098100011029     C     KEYTBE        CHAIN     TNTBE01L
098200011029     C                   IF        %found(TNTBE01L)
098300011029     C                   MOVEL     TBEUNI        DSGEDCE
098400011029     C                   ENDIF
098500011029     C*
098600011029     C                   IF        §GEDCEDAT > DATCOR
098700011029     C                   EVAL      WDIVCE = §GEDCEDIP
098800011029     C                   ELSE
098900011029     C                   EVAL      WDIVCE = §GEDCEDIV
099000011029     C                   ENDIF
099100011029     C*
099200011029     C                   ENDSR
099300980806     C*------------------------------------------------------------------------*
099400980806     C* CARTAB - CARICA LE TABELLE OCCORRENTI
099500980806     C*------------------------------------------------------------------------*
099600980806     C     CARTAB        BEGSR
099700980806     C*--------------------
099800980806     C* TIPI TARIFFA
099900980806     C*--------------------
100000980806     C                   Z-ADD     *ZEROS        I                 4 0
100100020924     C                   Z-ADD     1             KTBKUT
100200980806     C                   MOVEL     'TR'          KTBCOD
100300980806     C     KE2TAB        SETLL     TABEL00F
100400980806     C     KE2TAB        READE     TABEL00F                               99
100500980806DO  1C     *IN99         DOWEQ     *OFF
100600980806IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
100700980806     C                   MOVEL     TBLUNI        DSTR
100800980806IF  3C     §TRUTE        IFEQ      'S'                                          *IN GESTIONE
100900980806     C                   ADD       1             I
101000980806     C                   MOVEL     TBLKEY        TTR(I)                         *CODICE
101100980806     C                   MOVEL     §TRDE3        DTTR(I)                        *DESCRIZIONE
101200980806E   3C                   ENDIF
101300980806E   2C                   ENDIF
101400980806     C     KE2TAB        READE     TABEL00F                               99
101500980806E   1C                   ENDDO
101600981012     C*--------------------
101700980806     C* GIORNI DI PULIZIA ARCHIVI
101800980806     C*--------------------
101900980806     C                   Z-ADD     *ZEROS        I                 4 0
102000020924     C                   Z-ADD     1             KTBKUT
102100980806     C                   MOVEL     '5A'          KTBCOD
102200000622     C                   MOVEL(P)  '1'           KTBKEY
102300000622     C     KEYTAB        CHAIN     TABEL00F                           99
102400980806IF  1C     *IN99         IFEQ      *OFF
102500980806     C     TBLFLG        ANDEQ     *BLANKS
102600980806     C                   MOVEL     TBLUNI        DS5A
102700980806X   1C                   ELSE
102800980806     C                   CLEAR                   DS5A
102900980806E   1C                   ENDIF
103000020228     C*
103100020228     C                   Z-ADD     *ZEROS        K                 3 0
103200020228     C*
103300020228     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
103400020228     C                   Z-ADD     *ZEROS        KORFIL
103500020228     C     KEYORG        SETLL     AZORG01L                           99        *FINE-ARCHIV
103600020228     C  N99              READ      AZORG01L                               99
103700020228     C     *IN99         DOWEQ     '0'                                          --- 1 -->
103800020228     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
103900020228     C     ORGFAG        ANDNE     'V'
104000020228     C                   MOVEL     ORGDE3        OG143
104100020228     C* VERIFICO SE FILIALE FEDEX
104200020228     C                   IF        §OGNTW = 'FED'
104300020228     C                   ADD       1             K
104400020228     C                   MOVEL     ORGFIL        FILFED(K)
104500020228     C                   ENDIF
104600020228     C                   ENDIF
104700020228     C                   READ      AZORG01L                               99
104800020228     C                   ENDDO
109000980806     C*
109100980806     C                   ENDSR
109200980806     C*------------------------------------------------------------------------*
109300980806     C* IMPVD0 - IMPOSTAZIONE TESTATA
109400980806     C*------------------------------------------------------------------------*
109500980806     C     IMPVD0        BEGSR
109600980806     C*
109700980806     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
109800980806     C*
109900980806     C                   ENDSR
110000980806     C*------------------------------------------------------------------------*
110100980806     C* IMPDAT - IMPOSTA LE DATE DI LAVORO
110200980806     C*------------------------------------------------------------------------*
110300980806     C     IMPDAT        BEGSR
115800980828     C*---
115900980828     C* S E D E ->  DATA FINALE   = DATA ULTIMA PRESENZA IN TNTAS10F -TRASFERIMENTO 10>0P-
116000980828     C*             DATA INIZIALE = DATA PRIMA  PRESENZA IN TNTAS10F -TRASFERIMENTO 00>10-
116100980828     C*---
116101060807     C                   Z-ADD     *ZEROS        DATCOR            8 0          *DATE IN AA/M/G
116102060807     C                   TIME                    WN14             14 0          *ORA(6) + DATA(8)
116103060807     C                   MOVE      WN14          WN8               8 0          *DATA(8)
116104060807     C                   Z-ADD     WN8           G08DAT
116105060807     C                   Z-ADD     *ZEROS        G08INV
116106060807     C                   MOVEL     *BLANKS       G08ERR
116107060807     C                   CALL      'XSRDA8'
116108060807     C                   PARM                    WLBDA8
116109060807     C                   Z-ADD     G08INV        DATCOR                         *DATA CORRENTE AA/G/
116110060920     c                   move      datcor        dtaiso
116111060920     c                   subdur    1:*d          dtaiso
116112060807     c*
116200980828     C                   Z-ADD     *ZEROS        DA2INI            8 0          *DATE IN AA/M/G
116300980828     C                   Z-ADD     *ZEROS        DA2FIN            8 0
116400980828     C                   CLEAR                   DS$3
116500020924     C                   Z-ADD     1             KTBKUT
116600980828     C                   MOVEL     '$3'          KTBCOD
116700980828     C     KE2TAB        CHAIN     TABEL00F                           99
116800980828IF  1C     *IN99         IFEQ      *OFF
116900980828     C                   MOVEL     TBLUNI        DS$3
117000060920     C                   MOVEL     §$3D10        DA2INI
117001060920     c* fine oggi - 1
117002060920     C                   MOVE      dtaiso        DA2FIN
117100060920     C*                  MOVEL     §$3D00        DA2FIN
117200980828E   1C                   ENDIF
117300980806     C*
117400980806     C                   ENDSR
117500020924     C*--------------------------------------------------------------------------------------------*
117600020924     C* REPERISCE I DATI UTENTE
117700020924     C*--------------------------------------------------------------------------------------------*
117800020924     C     REPDATIUTE    BEGSR
117900020924     C*
118000020924     C* INIZIALIZZA VARIABILI DI WRK
118100020924     C                   CLEAR                   TIBS34DS
118200020924     C                   CLEAR                   AZUTEDS
118300020924     C                   CLEAR                   DDATIUTE
118400020924     C*
118500020924     C     *DTAARA       DEFINE    §azute        azuteds
118600020924     C     *DTAARA       DEFINE    §datiute      ddatiute
118700020924     C                   IN(E)     *DTAARA
118800020924if  1C                   IF        %Error
118900020924     c                   EVAL      I34Tla = 'L'
119000020924     C                   CALL      'TIBS34R'
119100020924     C                   PARM                    Tibs34Ds
119200020924     C                   IN        *DTAARA
119300020924e   1C                   ENDIF
119400020924     C*
119500020924     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
119600020924     C                   MOVEL(P)  RSUT          DSFIRS           20
119601060807
119602060807     c                   Clear                   wabi
119603060807     c                   Clear                   dLat
119604060807      * Verifica errori e autorità profilo
119605060807s   1c                   Select
119606060807      * se ho errori nei dati utente esco dal pgm
119607060807w   1c                   When      DutErr = 'E'
119608060807     c                   return
119609060807      * se non c'è l'abilitazione
119610060807      * --> se 1° livello, abilitazioni al terminal
119611060807      *     se 2° livello, abilitazioni al punto operativo
119612060807      *     se sede è impossibile ma se capita mando a fine il pgm
119613060807w   1c                   When      UteAut = *Blanks
119614060807if  2c                   If        DutLpo = '1'
119615060807     c                   Eval      wabi   = 'TP'
119616060807e   2c                   EndIf
119617060807if  2c                   If        DutLpo = '2'
119618060807     c                   Eval      wabi   = 'PO'
119619060807e   2c                   EndIf
119623060807      * carica le abilitazioni del profilo
119624060807x   1c                   Other
119625060807     c                   Movel     UteFaf        Dute01
119626060807     c                   If        §UteGtc <> *Blanks
119627060807     c                   Eval      wabi = §UteGtc
119628060807     c                   Else
119629060807     c                   Eval      wabi = UteAut
119630060807     c                   EndIf
119631060807e   1c                   EndSl
119632060807
119633060807      * controllo se ok l'abilitazione dell'utente
119634060807     c                   Clear                   Tibs02ds
119635060807     c                   Eval      T02Mod = 'C'
119636060807     c                   Eval      T02Sif = knsif
119637060807     c                   Eval      T02Cod = 'LAT'
119638060807     c                   Movel(p)  wabi          T02Ke1
119639060807     c                   Call      'TIBS02R'
119640060807     c                   Parm                    kpjba
119641060807     c                   Parm                    Tibs02ds
119642060807if  1c                   If        T02Err = *Blanks
119643060807     c                   Eval      dLat = T02Uni
119644060807e   1c                   EndIf
119673060807      * Reperimento dei P.O. gestibili dall'utente
119674060807     c                   clear                   TRUL31DS
119675060807     c                   eval      I31abi = wabi
119676060807     c                   eval      I31cdi = DUTdis
119677060807     c                   eval      I31car = DUTare
119678060807     c                   eval      I31cpo = DUTpou
119679060807     c                   call      'TRUL31R'
119680060807     c                   parm                    KPJBA
119681060807     c                   parm                    TRUL31DS
119682060807if  2c                   if        O31pog > *zeros
119683060807     c                   movea     s31           l1
119688060807e   2c                   endif
119700020924     C*
119800020924     C                   ENDSR
119900980806     C*------------------------------------------------------------------------*
120000980806     C* *INZSR - OPERAZIONI INIZIALI
120100980806     C*------------------------------------------------------------------------*
120200980806     C     *INZSR        BEGSR
120300980806     C*--------------------
120400980806     C* RICEVIMENTO PARAMETRI
120500980806     C*--------------------
120600980806     C     *ENTRY        PLIST
120700980806     C                   PARM                    KPJBA
120701091125     C                   movel     KPJBU         comdsf            1
120800980806     C*--------------------
120900980806     C* VARIABILI RIFERITE AL DATABASE
121000980806     C*--------------------
121100980806     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
121200980806     C     *LIKE         DEFINE    TBLCOD        KTBCOD
121300980806     C     *LIKE         DEFINE    TBLKEY        KTBKEY
121400980806     C     *LIKE         DEFINE    ACOKUT        KACKUT                         *CNAC000F
121500980806     C     *LIKE         DEFINE    ACOKCC        KACKCC
121600980806     C     *LIKE         DEFINE    ACOKSC        KACKSC
121700980806     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
121800980806     C     *LIKE         DEFINE    TAMKSC        KTMKSC                         *TNTAM00F
121900980806     C     *LIKE         DEFINE    TAMCTR        KTMCTR
122000980806     C     *LIKE         DEFINE    TAMPRG        KTMPRG
122100980806     C     *LIKE         DEFINE    TAMKSC        KFMKSC                         *TNOFM00F
122200980806     C     *LIKE         DEFINE    TAMCTR        KFMCTR
122300980806     C     *LIKE         DEFINE    TAMPRG        KFMPRG
122400980806     C     *LIKE         DEFINE    ACOKSC        KSCV1C                         *DEPOSITI VIDEO
122500980806     C     *LIKE         DEFINE    ORGFIL        FPAV1C
122600980806     C     *LIKE         DEFINE    ORGFIL        FARV1C
122700980806     C     *LIKE         DEFINE    ORGFIL        FAPV1C
122800980806     C     *LIKE         DEFINE    V1CDS1        DS1V1C
122900980806     C     *LIKE         DEFINE    V1CDS2        DS2V1C
123000020730     C     *LIKE         DEFINE    §NTWFIE       DV1FL1
123100981012     C     *LIKE         DEFINE    TAMKSC        CARKSC
123200011029     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
123300011029     C     *LIKE         DEFINE    TBEKE1        KTEKE1
123400020924     C     *LIKE         DEFINE    KPJBU         DEPKPJBU                       *depositi
123500980806     C*--------------------
123600980806     C* CHIAVI DI LETTURA
123700980806     C*--------------------
123800980806     C* LETTURA TABEL00F - COMPLETA
123900980806     C     KEYTAB        KLIST
124000980806     C                   KFLD                    KTBKUT                         *UTENTE
124100980806     C                   KFLD                    KTBCOD                         *TABELLA
124200980806     C                   KFLD                    KTBKEY                         *CODICE
124300980806     C* LETTURA TABEL00F - PARZIALE
124400980806     C     KE2TAB        KLIST
124500980806     C                   KFLD                    KTBKUT                         *UTENTE
124600980806     C                   KFLD                    KTBCOD                         *TABELLA
124700011029     C* LETTURA TNTBE01L - PARZIALE
124800011029     C     KEYTBE        KLIST
124900011029     C                   KFLD                    KTECOD                         *UTENTE
125000011029     C                   KFLD                    KTEKE1                         *CODICE
125100980806     C* LETTURA CNACO00F - COMPLETA
125200980806     C     KEYACO        KLIST
125300980806     C                   KFLD                    KACKUT                         *CODICE UTENTE
125400980806     C                   KFLD                    KACKCC                         *CODICE CAPOCONTO
125500980806     C                   KFLD                    KACKSC                         *CODICE CLIENTE
125600980806     C* LETTURA AZORG01L - COMPLETA
125700980806     C     KEYORG        KLIST
125800980806     C                   KFLD                    KORFIL                         *FILIALE
125900980806     C* LETTURA TNTAM01L - COMPLETA
126000980806     C     KEYTAM        KLIST
126100980806     C                   KFLD                    KTMKSC                         *CLIENTE
126200980806     C                   KFLD                    KTMCTR                         *TARIFFA
126300980806     C                   KFLD                    KTMPRG                         *PROGRESSIVO
126400980806     C* LETTURA TNOFM01L - COMPLETA
126500980806     C     KEYOFM        KLIST
126600980806     C                   KFLD                    KFMKSC                         *CLIENTE
126700980806     C                   KFLD                    KFMCTR                         *TARIFFA
126800980806     C                   KFLD                    KFMPRG                         *PROGRESSIVO
126900980806     C*--------------------
127000980806     C* VARIABILI NON RIFERITE AL DATABASE
127100980806     C*--------------------
127200980806     C                   MOVEL     'N'           $FINE             1            *FINE PROGRAMMA
127300980806     C                   MOVEL     '0'           $REC              1            *STATO RECORD
127400980806     C                   MOVEL     '0'           WERR              1            *ERRORE
127500980806     C                   MOVEL     *BLANKS       WMSG             78            *MESSAGGIO
127600980806     C                   MOVEL     '1'           $TESVD            1            *RIEMETTERE TESTATA
127700980806     C                   MOVEL     '1'           TIPVID            1            *VIDEO DA GESTIRE
127800980806     C*
127900980806     C* REPERISCE I DATI DELL'UTENTE
128000020924     C                   EXSR      REPDATIUTE
128001060804     C*
129101060804     c                   movel     'S'           v1cela            1
129200980806     C*
129300980806     C* CONTROLLA L'ENTRATA
129400980806     C                   MOVEL     KPJBU         D00KPJ            3            *LANCIO AZIONE KPJ
129500980806     C*
129600980806     C* CARICA LE TABELLE OCCORRENTI
129700980806     C                   EXSR      CARTAB
129800011029     C*
129900011029     C* CARICA LE NUOVE TABELLE OCCORRENTI
130000011029     C                   EXSR      CARGEDCE
130100980806     C*
130200980806     C* IMPOSTA LE DATE DI LAVORO
130300980806     C                   EXSR      IMPDAT
130400980806     C*
130500980806     C* IMPOSTAZIONI TESTATA
130600980806     C                   EXSR      IMPVD0
130700980806     C*
130800980806     C                   ENDSR
130900980806     O*------------------------------------------------------------------------*
131000980810** -MSG-
131100021031TNTE44R- Codice cliente inesistente o non in gestione.                        1
131200021106TNTE44R- Linea errata, non ingestione o non presente sull'elaboratore         2
131300980806TNTE44R- Data errata                                                          3
131400980806TNTE44R- Lasciare vuote o immettere entrambe le date                          4
131500980806TNTE44R- La data AL deve essere maggiore o uguale della data DAL              5
131600980806TNTE44R- Immettere almeno una selezione di bolle da elaborare                 6
131700980806TNTE44R- Immettere 'S' per simulare il lancio, 'N' per aggiornare l'archivio  7
131800091125TNTE44R- Il valore scelto non è corretto                                      8
131900980806TNTE44R- Tipo tariffa errato o non in gestione                                9
132000981012TNTE44R- Codice tariffa inesistente o non in gestione                         10
132100980806TNTE44R- Codice offerta inesistente                                           11
132200021031TNTE44R- 'I' per scrivere la tariffa italia, 'E' per scriverla come estero    12
132300980806TNTE44R- La tariffa italia non può avere filiali di partenza estere           13
132400980806TNTE44R- Codice tariffa errato.                                               14
132500980806TNTE44R- Per selezionare il P.O. lasciare vuote le altre selezioni            15
132600980806TNTE44R- La data DAL è inferiore alla data ultima di esistenza bolle          16
132700980806TNTE44R- La data AL non può essere maggiore della data corrente               17
132800980810TNTE44R- 'S' per scrivere la tariffa utilizzando i dati in sede, oppure 'F'   18
132900980810TNTE44R- Se lancio per sede è possibile creare i dati tipo solo come tariffa  19
133000060807TNTE44R- Cliente e date obbligatori                                           20
133100980810TNTE44R- Se lancio per sede è impossibile simulare la creazione               21
133200021029TNTE44R- Selezioni incongruenti, il cliente selezionato non è un unificante.  22
133300980828TNTE44R- La data AL non può essere maggiore della data ultima esistenza bolle 23
133400011029TNTE44R- ERRORE. Divisa attualmente non accettabile                           24
133500020228TNTE44R- Impossibile indicare linea FEDEX                                     25
133600020228TNTE44R- Impossibile indicare tariffa FEDEX                                   26
133700020313TNTE44R- Impossibile indicare filiale non in gestione                         27
133800980810TNTE44R- 28
133900980810TNTE44R- 29
134000980810TNTE44R- 30
