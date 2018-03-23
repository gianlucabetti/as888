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
001300060807     FCNclp00F  IF   E           K DISK
001400140611     Fazcmm01l  IF   E           K DISK
001500980806     FTNTAM01L  IF   E           K DISK
001600060804     FTNOFM01L  IF   E           K DISK
001700980806     F                                     RENAME(TNTAM000:TNOFM000)
001800980806     D*------------------------------------------------------------------------*
001900980806     D* SCHIERE
002000980806     D*------------------------------------------------------------------------*
002100980806     D*-------------------
002200980806     D* MESSAGGI VIDEO
002300980806     D*-------------------
002400980810     D MSG             S             78    DIM(30) CTDATA PERRCD(1)
002500980806     D*-------------------
002600980806     D* TIPI TARIFFA
002700980806     D*-------------------
002800980806     D TTR             S              1    DIM(10)                              *CODICE
002900980806     D DTTR            S             20    DIM(10)                              *DESCRIZIONE
003000980806     D*-------------------
003100980806     D* TABELLE "XCLIR"- RICERCA CLIENTI
003200980806     D*-------------------
003300980806     D KSC             S              7  0 DIM(30)                              *NUMERICO
003400980806     D KSA             S              4    DIM(30)                              *ALFANUMERICO
003500980806     D*------------------
003600980806     D* TABELLE CAPICONTO
003700980806     D*------------------
003800980806     D RI              S              4  0 DIM(4)                               *TAB RAG. CI
003900020228     D*-------------------
004000020228     D* SCHIERA FILIALI FEDEX
004100020228     D*-------------------
004200020228     D FILFED          S              3    DIM(999)
004300980806     D*------------------------------------------------------------------------*
004400980806     D* INPUT
004500980806     D*------------------------------------------------------------------------*
004600040723     d tibs55ds      E DS
004700980806     D*-------------------
004800980806     D* DS TIPI TARIFFA
004900980806     D*-------------------
005000980806     D DSTR          E DS
005100060807     d dLat          e ds
005200060807     d dute01        e ds
005300060807     d tibs02ds      e ds
005400060807     d wabi            s                   like(UteAut)
005500060920     d dtaiso          s               d
005600011011     D*-------------------
005700011011     D* DS REPERIMENTO DIVISA TARIFFE/OFFERTE
005800011011     D*-------------------
005900011011     D DSTA01        E DS
006000980806     D*-------------------
006100980806     D* DS GIORNI DI PULIZIA ARCHIVI
006200980806     D*-------------------
006300980806     D DS5A          E DS
006400980828     D*-------------------
006500980828     D* DS AVANZAMENTO TRASFERIMENTI
006600980828     D*-------------------
006700980828     D DS$3          E DS
006800020924     D*-------------------
006900020924     D* DS REPERIMENTO DATI UTENTE
007000020924     D*-------------------
007100020924     D TIBS34DS      E DS                                                       *Profili utente
007200020924     D DDATIUTE      E DS                                                       *Dati utente
007300020924     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007400060807     d Trul31ds      e ds
007500060807     D  S31                   10    759  0 dim(250)
007600060807      * P.O. gestibili dall'utente
007700060807     d l1              s              3  0 dim(250) inz(*Zeros)
007800980806     D*------------------
007900980806     D* DS "XSRDA8" - CONTROLLA DATA (8)
008000980806     D*------------------
008100980806     D WLBDA8          DS
008200980806     D  G08DAT                 1      8  0
008300980806     D  G08INV                 9     16  0
008400980806     D  G08ERR                17     17
008500980806     D  G08TGI                18     22  0
008600980806     D*------------------
008700980806     D* DS "XSRGI8" - RICREA DATA(8) DA TOTALI GIORNI
008800980806     D*------------------
008900980806     D WGIDA8          DS
009000980806     D  GIODAT                 1      8  0
009100980806     D  GIOINV                 9     16  0
009200980806     D  GIOTGI                17     21  0
009300980806     D*------------------
009400980806     D* DS "XCLIR"- RICERCA CLIENTI
009500980806     D*------------------
009600980806     D DSXC          E DS                  EXTNAME(XCLIRDS)
009700980806     D                 DS
009800980806     D  DSKSN                  1      4P 0
009900980806     D  DSKSA                  1      4
010000980806     D*------------------
010100980806     D* DS PARAMETRI DI LANCIO
010200980806     D*------------------
010300980806     D PARAM           DS                  INZ
010400980806     D  PARKSC                 1      7  0
010500980806     D  PARFPA                 8     10  0
010600980806     D  PARFAR                11     13  0
010700980806     D  PARFAP                14     16  0
010800980806     D  PARDS1                17     24  0
010900980806     D  PARDS2                25     32  0
011000980806     D  PARTAR                33     35
011100980806     D  PARCTO                36     36
011200980806     D  PARKOV                37     43  0
011300980806     D  PARCTR                44     46  0
011400980806     D  PARPRG                47     49  0
011500980806     D  PARLNP                50     52  0
011600980806     D  PARFIE                53     53
011700980806     D  PARSIM                54     54
011800040723     D  PARELA                55     55
011900040723     D  PARDIV                56     58
012000040723     D  PARUNI                59     59
012100040805     D  PARsta                60     60
012200040805     D  PARint                61     61
012300040805     D  PARsie                62     62
012400091130     D  PARdsf                63     63
012500980806     D*-------------------
012600980806     D* ARCHITETTURA
012700980806     D*-------------------
012800980806     D KPJBA         E DS
012900091125     D tnte04ds      E DS
013000011029     D*------------------
013100011029     D* DS LETTURA TABELLA "GED" TIPO RECORD "CE"
013200011029     D*------------------
013300011029     D DSGEDCE       E DS
013400020228     D*-------------------
013500020228     D* DS ESTERNA X DETERMINAZIONE NETWORK FILIALE DA ORGANIGRAMMA
013600020228     D*-------------------
013700020228     D OG143         E DS
013800020730     D*------------------
013900020730     D* DS PER LETTURA TABELLA "NTW"
014000020730     D*------------------
014100020730     D DNTW          E DS
014200021029     D*-------------------
014300021029     D* DS REPERIMENTO/CONTROLLO PADRE/FIGLI
014400021029     D*-------------------
014500021029     D TIBS10DS      E DS
014600040726     D*
014700040726     D KNSIFS          S                   LIKE(KNSIF)
014800980806     C*------------------------------------------------------------------------*
014900980806     C* MAIN LINE
015000980806     C*------------------------------------------------------------------------*
015100980806     C*
015200980806     C* IMPOSTAZIONI PRIMA VIDEATA
015300980806     C                   EXSR      IMPVD1
015400980806     C*
015500980806     C* CICLO FINO A RICHIESTA DI FINE ELABORAZIONE
015600980806DO  1C     $FINE         DOUEQ     'S'
015700980806     C     TIPVID        CASEQ     '1'           GESVD1                         *1° VIDEO
015800980806     C                   ENDCS
015900980806E   1C                   ENDDO
016000980806     C*
016100980806     C                   SETON                                        LR
016200980806     C*------------------------------------------------------------------------*
016300980806     C* IMPVD1 - IMPOSTAZIONI PRIMA VIDEATA
016400980806     C*------------------------------------------------------------------------*
016500980806     C     IMPVD1        BEGSR
016600980806     C*
016700980806     C* REIMPOSTA FORMATO VIDEO E INDICATORI
016800980806     C                   EXSR      SETVD1
016900980806     C                   MOVEL     '1'           TIPVID                         *GESTIONE VIDEO
017000021031     C                   MOVEL     'N'           V1CUNI
017100980806     C*
017200980806     C                   ENDSR
017300980806     C*------------------------------------------------------------------------*
017400980806     C* SETVD1 - IMPOSTAZIONI INDICATORI PRIMA VIDEATA
017500980806     C*------------------------------------------------------------------------*
017600980806     C     SETVD1        BEGSR
017700980806     C*
017800980806     C                   SETOFF                                       202122     ERRORI
017900980806     C                   SETOFF                                       232425
018000980806     C                   SETOFF                                       262728
018100980806     C                   SETOFF                                       293031
018200980810     C                   SETOFF                                       323334
018300021031     C                   SETOFF                                       9035       GENERICO DI ERRORE
018400980806     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
018500091125     c                   if        comdsf = ' '
018600091125     c                   eval      d2wcto = 'Tariffa o Offerta ............:'
018700091125     c                   eval      s2wcto = '(T/O)'
018800091125     c                   eval      d2wksc = 'Cliente o Visita .............:'
018900091125     c                   eval      d2wprg = 'Progressivo ..................:'
019000091125     c                   else
019100091201     c                   eval      d2wcto = 'Tariffa o Offerta ............:'
019200091130     c                   eval      s2wcto = '(T/O)'
019300091125     c                   eval      d2wksc = 'Cliente o Trattativa .........:'
019400091125     c                   eval      d2wprg = 'Versione .....................:'
019500091125     c                   end
019600980806     C*
019700980806     C                   ENDSR
019800980806     C*------------------------------------------------------------------------*
019900980806     C* GESVD1 - GESTIONE PRIMA VIDEATA
020000980806     C*------------------------------------------------------------------------*
020100980806     C     GESVD1        BEGSR
020200980806     C*
020300980806     C* EMISSIONE VIDEATA
020400980806IF  1C     $TESVD        IFEQ      '1'
020500980806     C                   WRITE     TE44D00                                      *TESTATA
020600980806     C                   MOVEL     '0'           $TESVD
020700980806E   1C                   ENDIF
020800980806     C                   EXFMT     TE44D01
020900980806     C                   EXSR      SETVD1
021000980806     C* F03-FINE
021100980806IF  1C     *INKC         IFEQ      *ON
021200980806     C                   EXSR      F03GES
021300980806     C                   GOTO      FINVD1
021400980806E   1C                   ENDIF
021500980806     C* ENTER-CONTROLLA VIDEO
021600980806     C                   EXSR      CTRVD1
021700980806IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
021800980806     C                   GOTO      FINVD1
021900980806E   1C                   ENDIF
022000980806     C* F06-CONFERMA
022100980806IF  1C     *INKF         IFEQ      *ON
022200980806     C                   EXSR      F06GES
022300980806     C                   GOTO      FINVD1
022400980806E   1C                   ENDIF
022500980806     C*
022600980806     C     FINVD1        ENDSR
022700980806     C*------------------------------------------------------------------------*
022800980806     C* CTRVD1 - CONTROLLA PRIMA VIDEATA
022900980806     C*------------------------------------------------------------------------*
023000980806     C     CTRVD1        BEGSR
023100980806     C*
023200980806     C* REIMPOSTA LE VARIABILI DI LAVORO
023300980806     C                   MOVEL     '0'           WERR
023400980810     C***
023500980810     C* LANCIO
023600980810     C***
023700980810     C*--------------------
023800980810     C* DATI SEDE O FILIALE
023900980810     C*--------------------
024000980811IF  1C     V1CELA        IFNE      'S'
024100980811     C     V1CELA        ANDNE     'F'
024200980810     C                   SETON                                        3490
024300980810     C                   MOVEL     MSG(18)       $MSG
024400980810     C                   GOTO      FINCT1
024500980810E   1C                   ENDIF
024600980810     C*--------------------
024700980810     C* DATI SEDE/FILIALE + SIMULAZIONE LANCIO
024800980810     C*--------------------
024900040806IF  1C*    V1CELA        IFEQ      'S'
025000040806     C*    dutlpo        andne     'S'
025100040806IF  2C*    V1CSIM        IFEQ      'S'
025200040806     C*                  SETON                                        243490
025300040806     C*                  MOVEL     MSG(21)       $MSG
025400040806     C*                  GOTO      FINCT1
025500040806E   2C*                  ENDIF
025600040806E   1C*                  ENDIF
025700980828     C* IMPOSTAZIONE DATE
025800980828IF  1C     V1CDS1        IFEQ      *ZEROS
025900980828     C     V1CDS2        ANDEQ     *ZEROS
026000980828     C                   EXSR      DATVID
026100980828E   1C                   ENDIF
026200980810     C*--------------------
026300980810     C* SIMULAZIONE LANCIO
026400980810     C*--------------------
026500980810IF  1C     V1CSIM        IFNE      'S'
026600980810     C     V1CSIM        ANDNE     'N'
026700040805     C     V1CSIM        ANDNE     'T'
026800980810     C                   SETON                                        2490
026900980810     C                   MOVEL     MSG(7)        $MSG
027000980810     C                   GOTO      FINCT1
027100980810E   1C                   ENDIF
027200980806     C***
027300980806     C* BOLLE DA ELABORARE
027400980806     C***
027500980810     C*--------------------
027600980810     C* DATI SEDE/FILIALE + CLIENTE
027700980810     C*--------------------
027800980811IF  1C     V1CELA        IFEQ      'S'                                          *DATI DA SEDE
027900040723IF  1C     dutlpo        andne     'S'                                          *DATI DA SEDE
028000980810IF  2C     V1CKSC        IFEQ      *BLANKS                                      *NON SELEZIONATO
028100980810     C     V1CKSC        OREQ      *ZEROS
028200980819     C                   SETON                                        2090
028300980810     C                   MOVEL     MSG(20)       $MSG
028400980810     C                   GOTO      FINCT1
028500980810E   2C                   ENDIF
028600980810E   1C                   ENDIF
028700980810     C*--------------------
028800980810     C* CLIENTE
028900980810     C*--------------------
029000980810     C                   Z-ADD     *ZEROS        KSCV1C
029100980810     C                   MOVEL     *BLANKS       V1DKSC
029200980806IF  1C     V1CKSC        IFNE      *BLANKS                                      *SELEZIONATO
029300980806     C     V1CKSC        ANDNE     *ZEROS
029400980806     C                   MOVEL     V1CKSC        WKSC
029500980806     C                   MOVEL     *BLANKS       WDKSC
029600980806     C                   EXSR      CHKKSC
029700980806     C                   MOVEL     WKSC          V1CKSC
029800980806     C                   MOVEL     WDKSC         V1DKSC
029900980806     C                   MOVEL     WKSC          KSCV1C
030000980806IF  2C     WERR          IFEQ      '1'
030100980806     C                   SETON                                        2090
030200980806     C                   MOVEL     MSG(1)        $MSG
030300980806     C                   GOTO      FINCT1
030400980806E   2C                   ENDIF
030500980810E   1C                   ENDIF
030600021029     C*--------------------
030700021029     C* CLIENTE PADRE/FIGLIO
030800021029     C*--------------------
030900021029IF  1C     V1CUNI        IFEQ      'S'
031000021029IF  2C     V1CKSC        IFNE      *BLANKS                                      *SELEZIONATO
031100021029     C     V1CKSC        ANDGT     *ZEROS
031200021029     C                   CLEAR                   TIBS10DS
031300021029     C                   EVAL      D10DRF = DATCOR                              *data riferimento
031400021029     C                   EVAL      D10TLE = 'ST'                                *tipo legame
031500021029     C                   EVAL      D10PAF = 'F'                                 *cerca il padre
031600021029     C                   MOVE(P)   V1CKSC        D10COD                         *cod cliente da tst
031700021029     C                   CALL      'TIBS10R'
031800021029     C                   PARM                    TIBS10DS
031900021029     C*
032000021029     C                   IF        D10ERR <> *blanks OR                         *No unif. o padre
032100021029     C                             D10COP =  *zeros
032200021029     C                   SETON                                        209035
032300021029     C                   MOVEL     MSG(22)       $MSG
032400021029     C                   GOTO      FINCT1
032500021029     C                   ENDIF
032600021029E   2C                   ENDIF
032700021029E   1C                   ENDIF
032800980810     C*--------------------
032900980810     C* DATI SEDE/FILIALE + DATA SPEDIZIONE DAL/AL
033000980810     C*--------------------
033100980811IF  1C     V1CELA        IFEQ      'S'                                          *DATI DA SEDE
033200980810IF  2C     V1CDS1        IFEQ      *ZEROS                                       *NON SELEZIONATO
033300980810     C     V1CDS2        OREQ      *ZEROS
033400980819     C                   SETON                                        2290
033500980810     C                   MOVEL     MSG(20)       $MSG
033600980810     C                   GOTO      FINCT1
033700980810E   2C                   ENDIF
033800980810E   1C                   ENDIF
033900980810     C*--------------------
034000980810     C* DATA SPEDIZIONE DAL
034100980810     C*--------------------
034200980810     C                   Z-ADD     *ZEROS        DS1V1C
034300980810IF  1C     V1CDS1        IFGT      *ZEROS                                       *SELEZIONATA
034400980810     C                   Z-ADD     V1CDS1        WDA8
034500980810     C                   Z-ADD     *ZEROS        DA8W
034600980810     C                   EXSR      CHKDAT
034700980810     C                   Z-ADD     WDA8          V1CDS1
034800980810     C                   Z-ADD     DA8W          DS1V1C
034900980810IF  2C     WERR          IFEQ      '1'
035000980810     C                   SETON                                        2290
035100980810     C                   MOVEL     MSG(3)        $MSG
035200980810     C                   GOTO      FINCT1
035300980810E   2C                   ENDIF
035400980810IF  2C     WERR          IFEQ      '0'
035500980828     C                   Z-ADD     DA2INI        WINI              8 0
035600980828IF  3C     DS1V1C        IFLT      WINI
035700980810     C                   SETON                                        2290
035800980810     C                   MOVEL     MSG(16)       $MSG
035900980810     C                   GOTO      FINCT1
036000980810E   3C                   ENDIF
036100980810E   2C                   ENDIF
036200980810E   1C                   ENDIF
036300980810     C*--------------------
036400980810     C* DATA SPEDIZIONE AL
036500980810     C*--------------------
036600980810     C                   Z-ADD     *ZEROS        DS2V1C
036700980810IF  1C     V1CDS2        IFGT      *ZEROS                                       *SELEZIONATA
036800980810     C                   Z-ADD     V1CDS2        WDA8
036900980810     C                   Z-ADD     *ZEROS        DA8W
037000980810     C                   EXSR      CHKDAT
037100980810     C                   Z-ADD     WDA8          V1CDS2
037200980810     C                   Z-ADD     DA8W          DS2V1C
037300980810IF  2C     WERR          IFEQ      '1'
037400980810     C                   SETON                                        2390
037500980810     C                   MOVEL     MSG(3)        $MSG
037600980810     C                   GOTO      FINCT1
037700980810E   2C                   ENDIF
037800980810IF  2C     WERR          IFEQ      '0'
037900980828     C                   Z-ADD     DA2FIN        WFIN              8 0
038000980828IF  3C     DS2V1C        IFGT      WFIN
038100980810     C                   SETON                                        2390
038200980828     C                   MOVEL     MSG(23)       $MSG
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
054000091125     c                   if        (comdsf = ' '
054100091125IF  1C                             and v1ccto <> 'T' and
054200091125IF  1C                             v1ccto <> 'O') or
054300091125     c                             (comdsf = 'X'
054400091125IF  1C                             and v1ccto <> 'T' and
054500091130IF  1C                             v1ccto <> 'O')
054600091201     C                   SETON                                        9031
054700091125     C                   MOVEL     MSG(8)        $MSG
054800091125     C                   GOTO      FINCt1
054900091125E   1C                   ENDIF
055000980806     C*--------------------
055100980806     C* CODICE TARIFFA
055200980806     C*--------------------
055300980806     C                   MOVEL     V1CCTR        WN1               1 0
055400980806     C                   MOVEL     WN1           WA1               1
055500980806     C                   SETOFF                                           98
055600980806     C                   Z-ADD     1             I
055700980806     C     WA1           LOOKUP    TTR(I)                                 98
055800980806IF  1C     *IN98         IFEQ      *OFF                                         *NON TROVATA
055900980806     C                   SETON                                        2790
056000980806     C                   MOVEL     MSG(9)        $MSG
056100980806     C                   GOTO      FINCT1
056200980806E   1C                   ENDIF
056300980806     C*--------------------
056400980806     C* CLIENTE O VISITA
056500980806     C*--------------------
056600980806IF  1C     V1CCTO        IFEQ      'T'                                          T A R I F F A
056700980806     C                   Z-ADD     V1CKOV        KTMKSC
056800980806     C                   Z-ADD     V1CCTR        KTMCTR
056900980806     C                   Z-ADD     V1CPRG        KTMPRG
057000980806     C     KEYTAM        CHAIN     TNTAM01L                           99
057100980806IF  2C     *IN99         IFEQ      *ON                                          *INESISTENTE
057200981012     C     V1CKOV        OREQ      CARKSC                                       *CREAZIONE CARTELLO
057300981012     C     V1CELA        ANDEQ     'S'                                          *IN SEDE
057400980806     C                   SETON                                        262728
057500980806     C                   SETON                                        90
057600980806     C                   MOVEL     MSG(10)       $MSG
057700980806     C                   GOTO      FINCT1
057800011011     C                   ELSE
057900020228     C* VERIFICO SE TARIFFA E' FEDEX
058000020228     C                   MOVEL     TAMFLO        DSTA01
058100020228     C                   IF        §TAFED = 'S'
058200020228     C                   SETON                                        262728
058300020228     C                   SETON                                        90
058400020228     C                   MOVEL     MSG(26)       $MSG
058500020228     C                   GOTO      FINCT1
058600020313     C                   ENDIF
058700020314     C* VERIFICO SE LINEA DEL CLIENTE O VISITA SIA IN GESTIONE (£1)
058800020314     C* SE TRATTASI DI TARIFFA DI CARTELLO VA CMQ BENE
058900040723     c                   if        dutlpo <> 'S'
059000020314     C                   MOVEL     V1CKOV        DEPFIL            3 0
059100020314     C     DEPFIL        LOOKUP    L1                                     55
059200020313     C                   IF        *IN55 = *OFF
059300020313     C                   SETON                                        2690
059400020313     C                   MOVEL     MSG(27)       $MSG
059500020313     C                   GOTO      FINCT1
059600020313     C                   ENDIF
059700040723     C                   ENDIF
059800020228     C*
059900011011     C                   MOVEL     §TADIV        V1CDIV
060000980806E   2C                   ENDIF
060100980806X   1C                   ELSE                                                   O F F E R T A
060200980806     C                   Z-ADD     V1CKOV        KFMKSC
060300980806     C                   Z-ADD     V1CCTR        KFMCTR
060400980806     C                   Z-ADD     V1CPRG        KFMPRG
060500980806     C     KEYOFM        CHAIN     TNOFM01L                           99
060600980806IF  2C     *IN99         IFEQ      *ON                                          *INESISTENTE
060700980806     C                   SETON                                        262728
060800980806     C                   SETON                                        90
060900980806     C                   MOVEL     MSG(11)       $MSG
061000980806     C                   GOTO      FINCT1
061100011029     C                   ELSE
061200011029     C                   MOVEL     TAMFLO        DSTA01
061300011029     C                   MOVEL     §TADIV        V1CDIV
061400020314E   2C                   ENDIF
061500020314     C*
061600091125     c                   clear                   tnte04ds
061700091125     C                   movel     comdsf        d04dsf
061800091125     C                   Z-ADD     v1ckov        d04nrv
061900091125     c                   call      'TNTE04R'
062000091125     c                   parm                    kpjba
062100091126     c                   parm                    tnte04ds
062200091125     C                   IF        d04err = ' '
062300091125     C                   IF        d04KSC > *zeros
062400091125     C                   MOVEL     d04KSC        DEPFIL
062500020314     C                   ELSE
062600091125     C                   MOVEL     d04CMM        DEPFIL
062700020314     C                   ENDIF
062800020314     C     DEPFIL        LOOKUP    L1                                     55
062900020314     C                   IF        *IN55 = *OFF
063000020314     C                   SETON                                        262728
063100020314     C                   SETON                                        90
063200020314     C                   MOVEL     MSG(27)       $MSG
063300020314     C                   GOTO      FINCT1
063400060807     C                   ENDIF
063500020314     C                   ELSE
063600020314     C                   SETON                                        262728
063700020314     C                   SETON                                        90
063800020314     C                   MOVEL     MSG(27)       $MSG
063900020314     C                   GOTO      FINCT1
064000020314     C                   ENDIF
064100020314     C*
064200980806E   1C                   ENDIF
064300980806     C*--------------------
064400980806     C* LINEA DI PARTENZA
064500980806     C*--------------------
064600980810     C                   MOVEL     *BLANKS       V1DLNP
064700040723IF  1C     V1CLNP        IFNE      999                                          *SELEZIONE PER SEDE
064800980806     C                   MOVEL     V1CLNP        WFIL
064900980806     C                   MOVEL     *BLANKS       WDFIL
065000980806     C                   MOVEL     *BLANKS       WFL1
065100980806     C                   EXSR      CHKFIL
065200980806     C                   MOVEL     WFIL          V1CLNP
065300040723     C                   MOVEL     WDFIL         V1DLNP
065400980806     C                   MOVEL     WFL1          DV1FL1
065500980806IF  2C     WERR          IFEQ      '1'
065600980806     C                   SETON                                        2990
065700980806     C                   MOVEL     MSG(2)        $MSG
065800980806     C                   GOTO      FINCT1
065900020228     C                   ELSE
066000020228     C* VERIFICO SE LNA E' FILIALE FEDEX
066100020228     C                   IF        WFIL > *ZEROS  OR
066200020228     C                             WFIL > *BLANKS
066300020228     C     WFIL          LOOKUP    FILFED                                 55
066400020228     C                   IF        *IN55 = *ON
066500020228     C                   SETON                                        2990
066600020228     C                   MOVEL     MSG(25)       $MSG
066700020228     C                   GOTO      FINCT1
066800020228     C                   ENDIF
066900020228     C                   ENDIF
067000020228     C*
067100980806E   2C                   ENDIF
067200040723X   1C                   ELSE
067300060912     c*                  if        dutlpo = 'S'
067400060912     C*                  MOVEL(P)  'TUTTE'       V1DLNP
067500060912     c*                  else
067600040723     C                   SETON                                        2990
067700040723     C                   MOVEL     MSG(2)        $MSG
067800040723     C                   GOTO      FINCT1
067900060912     c*                  end
068000040723E   1C                   ENDIF
068100980806     C*--------------------
068200980806     C* TARIFFA ITALIA/ESTERO
068300980806     C*--------------------
068400980806IF  1C     V1CFIE        IFNE      'I'
068500980806     C     V1CFIE        ANDNE     'E'
068600980806     C     V1CFIE        ANDNE     ' '
068700980806     C                   SETON                                        3090
068800980806     C                   MOVEL     MSG(12)       $MSG
068900980806     C                   GOTO      FINCT1
069000980806E   1C                   ENDIF
069100980806     C*--------------------
069200980806     C* LINEA DI PARTENZA + TARIFFA ITALIA/ESTERO
069300980806     C*--------------------
069400980806IF  1C     V1CFIE        IFEQ      'I'
069500980806     C     DV1FL1        ANDNE     *BLANKS
069600980806     C                   SETON                                        293090
069700980806     C                   MOVEL     MSG(13)       $MSG
069800980806     C                   GOTO      FINCT1
069900980806E   1C                   ENDIF
070000011029     C*--------------------
070100011029     C* CONTROLLO VALIDITA' DIVISA
070200011029     C*--------------------
070300011029     C     V1CDIV        IFEQ      'ITL'
070400011029     C     DATCOR        ANDGE     §GEDCEDAT
070500011029     C                   SETON                                        262728
070600011029     C                   SETON                                        90
070700011029     C                   MOVEL     MSG(24)       $MSG
070800011029     C                   GOTO      FINCT1
070900011029     C                   ENDIF
071000980806     C*
071100980806     C     FINCT1        ENDSR
071200980828     C*------------------------------------------------------------------------*
071300980828     C* DATVID - IMPOSTA LE DATE SPEDIZIONE A VIDEO
071400980828     C*------------------------------------------------------------------------*
071500980828     C     DATVID        BEGSR
071600980828     C                   Z-ADD     DA2INI        G08INV                         *DATA INIZIALE
071700980828     C                   Z-ADD     *ZEROS        G08DAT
071800980828     C                   MOVEL     '3'           G08ERR
071900980828     C                   CALL      'XSRDA8'
072000980828     C                   PARM                    WLBDA8
072100980828     C                   Z-ADD     G08DAT        V1CDS1
072200980828     C                   Z-ADD     DA2FIN        G08INV                         *DATA FINALE
072300980828     C                   Z-ADD     *ZEROS        G08DAT
072400980828     C                   MOVEL     '3'           G08ERR
072500980828     C                   CALL      'XSRDA8'
072600980828     C                   PARM                    WLBDA8
072700980828     C                   Z-ADD     G08DAT        V1CDS2
072800980828     C*
072900980828     C                   ENDSR
073000060807     C*------------------------------------------------------------------------*
073100060807     C* srcom  - CONTROLLA anche il po del commerciale
073200060807     C*------------------------------------------------------------------------*
073300060807     C     srcom         BEGSR
073400060807     c*
073500060807     C                   Z-ADD     DUTKCI        KACKCC
073600060807     C                   MOVEL     WKSC          KACKSC
073700060807     c     keyaco        chain     cnclp000
073800060807    3c                   if        not %found(cnclp00f)
073900060807     c                   clear                   wporagru          3 0
074000060807     c                   else
074100140611     C     clpage        CHAIN     azcmm01l
074200140611     c                   if        not %found(azcmm01l)
074300140611     c                   eval      werr = '1'
074400140611     c                   leavesr
074500140611     c                   end
074600140611     c                   movel     cmmuni        wporagru
074700140611     C     wporagru      LOOKUP    l1                                     55
074800060808    2C     *IN55         IFEQ      *OFF
074900060807     c                   eval      werr = '1'
075000091201     C                   ELSE
075100091201     c                   clear                   werr
075200060807    3C                   ENDIF
075300091201    3c                   endif
075400060807     c                   endsr
075500980806     C*------------------------------------------------------------------------*
075600980806     C* CHKKSC - CONTROLLA IL CLIENTE
075700980806     C*------------------------------------------------------------------------*
075800980806     C     CHKKSC        BEGSR
075900980806     C*
076000980806     C     *LIKE         DEFINE    V1CKSC        WKSC
076100980806     C     *LIKE         DEFINE    V1DKSC        WDKSC
076200980806     C*
076300980806     C* RICHIESTO ELENCO CODICI
076400980806     C     '?'           SCAN      WKSC                                   98
076500980806IF  1C     *IN98         IFEQ      *ON
076600980806     C                   CLEAR                   DSXC                           *CLIENTI
076700020924     C                   Z-ADD     DUTKCI        DXCCAP                         *CAPOCONTO
076800060912     c                   eval      dxcaut = wabi
076900980806     C                   CALL      'XCLIR'
077000980806     C                   PARM                    DSXC
077100980806     C* SCOMPATTA
077200980806     C                   MOVEA     DXCKSC        KSA                            *DS INPUT -> SCH ALF
077300980806DO  2C     1             DO        30            J                 4 0
077400980806     C                   MOVE      KSA(J)        DSKSA                          *SCH ALF -> DS ALF
077500980806     C                   Z-ADD     DSKSN         KSC(J)                         *DS NUM -> SCH NUM
077600980806E   2C                   ENDDO
077700980806     C                   MOVEL     KSC(1)        WKSC
077800980806     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
077900980806E   1C                   ENDIF
078000980806     C*
078100980806     C* CODICE CLIENTE NUMERICO
078200980806IF  1C     WKSC          IFLT      *ZEROS
078300980806     C                   MOVEL     '1'           WERR
078400980806X   1C                   ELSE
078500980806     C*
078600980806     C* CONTROLLA ESISTENZA CODICE IN ARCHIVIO
078700020924     C                   Z-ADD     1             KACKUT
078800020924     C                   Z-ADD     DUTKCI        KACKCC
078900980806     C                   MOVEL     WKSC          KACKSC
079000980806     C     KEYACO        CHAIN     CNACO00F                           99
079100980806IF  2C     *IN99         IFEQ      *ON
079200980806     C                   MOVEL     '1'           WERR
079300980806X   2C                   ELSE
079400980806     C                   MOVEL     ACORAG        WDKSC
079500980806E   2C                   ENDIF
079600980806E   1C                   ENDIF
079700021031     C*
079800021031     C* VERIFICA SE LA LINEA DEL CLIENTE RICHIESTO E' IN GESTIONE
079900021031     C                   MOVEL     WKSC          DEPFIL
080000021031     C     DEPFIL        LOOKUP    L1                                     55
080100021031     C                   IF        *IN55 = *OFF
080200021031     C                   MOVEL     '1'           WERR
080300021031     C                   ENDIF
080400060807     c* verifico anche il po commerciale
080500060807     c                   if        werr = '1'
080600060807     c                   exsr      srcom
080700060807     c                   end
080800980806     C*
080900980806     C                   ENDSR
081000980806     C*------------------------------------------------------------------------*
081100980806     C* CHKFIL - CONTROLLA LA FILIALE
081200980806     C*------------------------------------------------------------------------*
081300980806     C     CHKFIL        BEGSR
081400980806     C*
081500980806     C     *LIKE         DEFINE    V1CFPA        WFIL
081600020730     C     *LIKE         DEFINE    §NTWFIE       WFL1
081700980806     C     *LIKE         DEFINE    V1DFPA        WDFIL
081800980806     C*
081900980806     C* RICHIESTO ELENCO CODICI
082000980806     C     '?'           SCAN      WFIL                                   98
082100980806IF  1C     *IN98         IFEQ      *ON
082200980806     C                   MOVEL     *BLANKS       §OFIL
082300980806     C                   MOVEL     *BLANKS       §OTIP
082400980806     C                   MOVEL     *BLANKS       §ODES
082500980806     C                   MOVEL     *BLANKS       §OPRA
082600980806     C                   CALL      'TNSD24R'
082700980806     C                   PARM                    §OFIL             3
082800980806     C                   PARM                    §OTIP             1
082900980806     C                   PARM                    §ODES            25
083000980806     C                   PARM                    §OPRA             3
083100980806     C                   MOVEL     §OFIL         WFIL                           *CODICE
083200980806     C                   MOVEL     §ODES         WDFIL                          *DESCRIZIONE
083300980806     C                   MOVE      '1'           $TESVD                         *RIEMETTERE TESTATA
083400980806E   1C                   ENDIF
083500980806     C*
083600980806     C* CODICE FILIALE NUMERICO
083700980806IF  1C     WFIL          IFLT      *ZEROS
083800980806     C                   MOVEL     '1'           WERR
083900980806X   1C                   ELSE
084000980806     C*
084100980806     C* CONTROLLA ESISTENZA CODICE IN ARCHIVIO
084200980806     C                   MOVEL     WFIL          KORFIL
084300980806     C     KEYORG        CHAIN     AZORG01L                           98
084400980806IF  2C     *IN98         IFEQ      *ON                                          *NON TROVATA
084500980806     C     ORGFVA        ORNE      *BLANKS                                      *NO ANNULLATI
084600980806     C                   MOVEL     '1'           WERR
084700980806X   2C                   ELSE
084800980806     C                   MOVEL     ORGDES        WDFIL                          *DESCRIZIONE
084900020730     C                   MOVEL     ORGDE3        OG143
085000020730     C*
085100020730     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
085200020730     C                   CLEAR                   DNTW
085300020730     C*
085400020730     C                   MOVEL(P)  'NTW'         KTECOD
085500020730     C                   MOVEL(P)  §OGNTW        KTEKE1
085600020730     C     KEYTBE        CHAIN     TNTBE01L
085700020730     C                   IF        %found(TNTBE01L)
085800020730     C                   MOVEL     TBEUNI        DNTW
085900020730     C                   ENDIF
086000020730     C*
086100020730     C                   MOVEL     §NTWFIE       WFL1                           *FLAG ITALIA/ESTERO
086200980806E   2C                   ENDIF
086300980806E   1C                   ENDIF
086400980806     C*
086500980806     C                   ENDSR
086600021106     C*------------------------------------------------------------------------*
086700021106     C* CHKFPA - ALTRI CONTROLLI SULLA FILIALE DI PARTENZA
086800021106     C*------------------------------------------------------------------------*
086900021106     C     CHKFPA        BEGSR
087000021106     C*
087100021106     C                   MOVE(P)   V1CFPA        WNUMFIL           3 0
087200021106     C*
087300021106     C* SE INDICATO ANCHE IL CLIENTE => VERIFICA CHE LA LINEA DI PARTENZA SIA DELL'ELABORATORE
087400060807     C*                  IF        V1CKSC > *ZEROS
087500060807     C*    WNUMFIL       LOOKUP    SKI                                    55
087600060807     C*                  IF        *IN55 = *OFF
087700060807     C*                  MOVEL     '1'           WERR
087800060807     C*                  ENDIF
087900021106     C*
088000021106     C* SE NON INDICATO IL CLIENTE => VERIFICA CHE LA LINEA DI PARTENZA SIA IN GESTIONE
088100060807     C*                  ELSE
088200021106     C     WNUMFIL       LOOKUP    L1                                     55
088300021106     C                   IF        *IN55 = *OFF
088400021106     C                   MOVEL     '1'           WERR
088500021106     C                   ENDIF
088600060807     C*                  ENDIF
088700021106     C*
088800021106     C                   ENDSR
088900980806     C*------------------------------------------------------------------------*
089000980806     C* CHKDAT - CONTROLLA LA DATA
089100980806     C*------------------------------------------------------------------------*
089200980806     C     CHKDAT        BEGSR
089300980806     C*
089400980806     C     *LIKE         DEFINE    V1CDS1        WDA8
089500980806     C     *LIKE         DEFINE    V1CDS1        DA8W
089600980806     C*
089700980806     C                   Z-ADD     WDA8          G08DAT
089800980806     C                   Z-ADD     *ZEROS        G08INV
089900980806     C                   MOVEL     '0'           G08ERR
090000980806     C                   CALL      'XSRDA8'
090100980806     C                   PARM                    WLBDA8
090200980806     C                   Z-ADD     G08DAT        WDA8
090300980806IF  1C     G08ERR        IFEQ      '1'
090400980806     C                   MOVEL     '1'           WERR
090500980806X   1C                   ELSE
090600980806     C                   Z-ADD     G08INV        DA8W
090700980806E   1C                   ENDIF
090800980806     C*
090900980806     C                   ENDSR
091000980806     C*------------------------------------------------------------------------*
091100980806     C* F03GES - GESTIONE TASTO FUNZIONALE F03 ==> FINE
091200980806     C*------------------------------------------------------------------------*
091300980806     C     F03GES        BEGSR
091400980806     C*
091500980806     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
091600980810     C*
091700980806     C                   ENDSR
091800980806     C*------------------------------------------------------------------------*
091900980806     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA
092000980806     C*------------------------------------------------------------------------*
092100040723     c     F06GES        BEGSR
092200040723     C* Richiamo TIBS55R per conoscere S.Informativi
092300040723     C* in cui immettere il lavoro
092400040723     C                   CLEAR                   TIBS55ds
092500040723     C                   MOVEL     'L'           I50TLA
092600040723     C                   MOVEL     dutpou        I50PPO
092700040723     C                   MOVEL     '046'         I50APO
092800040723     C                   CALL      'TIBS55R'
092900040723     C                   PARM                    TIBS55ds
093000040726     C* MI SALVO IL S.I. ORIGINALE XCHÈ SE NO NON LO REIMPOSTA
093100040726     c                   eval      knsifS = KNSIF
093200980806     C*
093300040723     C* LANCIA IL PROGRAMMA
093400980806     C                   Z-ADD     KSCV1C        PARKSC
093500980806     C                   Z-ADD     FPAV1C        PARFPA
093600980806     C                   Z-ADD     FARV1C        PARFAR
093700980806     C                   Z-ADD     FAPV1C        PARFAP
093800980806     C                   Z-ADD     DS1V1C        PARDS1
093900980806     C                   Z-ADD     DS2V1C        PARDS2
094000980806     C                   MOVEL     V1CTAR        PARTAR
094100980806     C                   MOVEL     V1CCTO        PARCTO
094200980806     C                   Z-ADD     V1CKOV        PARKOV
094300980806     C                   Z-ADD     V1CCTR        PARCTR
094400980806     C                   Z-ADD     V1CPRG        PARPRG
094500980806     C                   Z-ADD     V1CLNP        PARLNP
094600980806     C                   MOVEL     V1CFIE        PARFIE
094700091130     C                   MOVEL     comdsf        PARdsf
094800040805     c* se simulazione solo aggiornamento imposto il flag si stampa = ' '
094900040805     c* altrimenti se richiesto simulazione o agg.+stampa imposto S
095000040805     c                   if        v1csim <> 'N'
095100040805     c                   movel     'S'           parsta
095200040805     c                   end
095300040805     c* se simulazione = stampa + agg. imposto il flag di simulazione = N
095400040805     c* per aggiornare
095500040805     c                   if        v1csim = 'T'
095600040805     c                   movel     'N'           parsim
095700040805     c                   else
095800980810     C                   MOVEL     V1CSIM        PARSIM
095900040805     c                   end
096000011011     C                   MOVEL     V1CDIV        PARDIV
096100021029     C                   MOVEL     V1CUNI        PARUNI
096200040726     C                   MOVEL     v1cela        PARELA
096300040806     C                   MOVEL     TAMFIE        PARSIE
096400040726     c                   eval      knsif = O50ASI
096500060808     C                   MOVEL     'TE6N'        KCOAZ
096600980806     C                   MOVEL     PARAM         KPJBU
096700060830     C                   MOVE      'TNTE41C   '  KPJBU
096800051102     c                   movel(p)  'BCH10'       Wpgm             10
096900051102     c                   call      Wpgm
097000980806     C                   PARM                    KPJBA
097100040726     c                   eval      knsif = KNSIFS
097200980806     C*
097300980806     C                   ENDSR
097400011029     C*------------------------------------------------------------------------*
097500011029     C* CARGEDCE - CARICAMENTO TABELLA "GED" TIPO RECORD "CE" E CONSIDERAZIONI SU DIVISE
097600011029     C*------------------------------------------------------------------------*
097700011029     C     CARGEDCE      BEGSR
097800011029     C*
097900011029     C                   CLEAR                   DSGEDCE
098000011029     C                   MOVEL     *blanks       WDIVCE            3
098100011029     C*
098200011029     C                   MOVEL(P)  'GED'         KTECOD
098300011029     C                   MOVEL(P)  'CE'          KTEKE1
098400011029     C     KEYTBE        CHAIN     TNTBE01L
098500011029     C                   IF        %found(TNTBE01L)
098600011029     C                   MOVEL     TBEUNI        DSGEDCE
098700011029     C                   ENDIF
098800011029     C*
098900011029     C                   IF        §GEDCEDAT > DATCOR
099000011029     C                   EVAL      WDIVCE = §GEDCEDIP
099100011029     C                   ELSE
099200011029     C                   EVAL      WDIVCE = §GEDCEDIV
099300011029     C                   ENDIF
099400011029     C*
099500011029     C                   ENDSR
099600980806     C*------------------------------------------------------------------------*
099700980806     C* CARTAB - CARICA LE TABELLE OCCORRENTI
099800980806     C*------------------------------------------------------------------------*
099900980806     C     CARTAB        BEGSR
100000980806     C*--------------------
100100980806     C* TIPI TARIFFA
100200980806     C*--------------------
100300980806     C                   Z-ADD     *ZEROS        I                 4 0
100400020924     C                   Z-ADD     1             KTBKUT
100500980806     C                   MOVEL     'TR'          KTBCOD
100600980806     C     KE2TAB        SETLL     TABEL00F
100700980806     C     KE2TAB        READE     TABEL00F                               99
100800980806DO  1C     *IN99         DOWEQ     *OFF
100900980806IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
101000980806     C                   MOVEL     TBLUNI        DSTR
101100980806IF  3C     §TRUTE        IFEQ      'S'                                          *IN GESTIONE
101200980806     C                   ADD       1             I
101300980806     C                   MOVEL     TBLKEY        TTR(I)                         *CODICE
101400980806     C                   MOVEL     §TRDE3        DTTR(I)                        *DESCRIZIONE
101500980806E   3C                   ENDIF
101600980806E   2C                   ENDIF
101700980806     C     KE2TAB        READE     TABEL00F                               99
101800980806E   1C                   ENDDO
101900981012     C*--------------------
102000980806     C* GIORNI DI PULIZIA ARCHIVI
102100980806     C*--------------------
102200980806     C                   Z-ADD     *ZEROS        I                 4 0
102300020924     C                   Z-ADD     1             KTBKUT
102400980806     C                   MOVEL     '5A'          KTBCOD
102500000622     C                   MOVEL(P)  '1'           KTBKEY
102600000622     C     KEYTAB        CHAIN     TABEL00F                           99
102700980806IF  1C     *IN99         IFEQ      *OFF
102800980806     C     TBLFLG        ANDEQ     *BLANKS
102900980806     C                   MOVEL     TBLUNI        DS5A
103000980806X   1C                   ELSE
103100980806     C                   CLEAR                   DS5A
103200980806E   1C                   ENDIF
103300020228     C*
103400020228     C                   Z-ADD     *ZEROS        K                 3 0
103500020228     C*
103600020228     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
103700020228     C                   Z-ADD     *ZEROS        KORFIL
103800020228     C     KEYORG        SETLL     AZORG01L                           99        *FINE-ARCHIV
103900020228     C  N99              READ      AZORG01L                               99
104000020228     C     *IN99         DOWEQ     '0'                                          --- 1 -->
104100020228     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
104200020228     C     ORGFAG        ANDNE     'V'
104300020228     C                   MOVEL     ORGDE3        OG143
104400020228     C* VERIFICO SE FILIALE FEDEX
104500020228     C                   IF        §OGNTW = 'FED'
104600020228     C                   ADD       1             K
104700020228     C                   MOVEL     ORGFIL        FILFED(K)
104800020228     C                   ENDIF
104900020228     C                   ENDIF
105000020228     C                   READ      AZORG01L                               99
105100020228     C                   ENDDO
105200980806     C*
105300980806     C                   ENDSR
105400980806     C*------------------------------------------------------------------------*
105500980806     C* IMPVD0 - IMPOSTAZIONE TESTATA
105600980806     C*------------------------------------------------------------------------*
105700980806     C     IMPVD0        BEGSR
105800980806     C*
105900980806     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
106000980806     C*
106100980806     C                   ENDSR
106200980806     C*------------------------------------------------------------------------*
106300980806     C* IMPDAT - IMPOSTA LE DATE DI LAVORO
106400980806     C*------------------------------------------------------------------------*
106500980806     C     IMPDAT        BEGSR
106600980828     C*---
106700980828     C* S E D E ->  DATA FINALE   = DATA ULTIMA PRESENZA IN TNTAS10F -TRASFERIMENTO 10>0P-
106800980828     C*             DATA INIZIALE = DATA PRIMA  PRESENZA IN TNTAS10F -TRASFERIMENTO 00>10-
106900980828     C*---
107000060807     C                   Z-ADD     *ZEROS        DATCOR            8 0          *DATE IN AA/M/G
107100060807     C                   TIME                    WN14             14 0          *ORA(6) + DATA(8)
107200060807     C                   MOVE      WN14          WN8               8 0          *DATA(8)
107300060807     C                   Z-ADD     WN8           G08DAT
107400060807     C                   Z-ADD     *ZEROS        G08INV
107500060807     C                   MOVEL     *BLANKS       G08ERR
107600060807     C                   CALL      'XSRDA8'
107700060807     C                   PARM                    WLBDA8
107800060807     C                   Z-ADD     G08INV        DATCOR                         *DATA CORRENTE AA/G/
107900060920     c                   move      datcor        dtaiso
108000060920     c                   subdur    1:*d          dtaiso
108100060807     c*
108200980828     C                   Z-ADD     *ZEROS        DA2INI            8 0          *DATE IN AA/M/G
108300980828     C                   Z-ADD     *ZEROS        DA2FIN            8 0
108400980828     C                   CLEAR                   DS$3
108500020924     C                   Z-ADD     1             KTBKUT
108600980828     C                   MOVEL     '$3'          KTBCOD
108700980828     C     KE2TAB        CHAIN     TABEL00F                           99
108800980828IF  1C     *IN99         IFEQ      *OFF
108900980828     C                   MOVEL     TBLUNI        DS$3
109000060920     C                   MOVEL     §$3D10        DA2INI
109100060920     c* fine oggi - 1
109200060920     C                   MOVE      dtaiso        DA2FIN
109300060920     C*                  MOVEL     §$3D00        DA2FIN
109400980828E   1C                   ENDIF
109500980806     C*
109600980806     C                   ENDSR
109700020924     C*--------------------------------------------------------------------------------------------*
109800020924     C* REPERISCE I DATI UTENTE
109900020924     C*--------------------------------------------------------------------------------------------*
110000020924     C     REPDATIUTE    BEGSR
110100020924     C*
110200020924     C* INIZIALIZZA VARIABILI DI WRK
110300020924     C                   CLEAR                   TIBS34DS
110400020924     C                   CLEAR                   AZUTEDS
110500020924     C                   CLEAR                   DDATIUTE
110600020924     C*
110700020924     C     *DTAARA       DEFINE    §azute        azuteds
110800020924     C     *DTAARA       DEFINE    §datiute      ddatiute
110900020924     C                   IN(E)     *DTAARA
111000020924if  1C                   IF        %Error
111100020924     c                   EVAL      I34Tla = 'L'
111200020924     C                   CALL      'TIBS34R'
111300020924     C                   PARM                    Tibs34Ds
111400020924     C                   IN        *DTAARA
111500020924e   1C                   ENDIF
111600020924     C*
111700020924     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
111800020924     C                   MOVEL(P)  RSUT          DSFIRS           20
111900060807
112000060807     c                   Clear                   wabi
112100060807     c                   Clear                   dLat
112200060807      * Verifica errori e autorità profilo
112300060807s   1c                   Select
112400060807      * se ho errori nei dati utente esco dal pgm
112500060807w   1c                   When      DutErr = 'E'
112600060807     c                   return
112700060807      * se non c'è l'abilitazione
112800060807      * --> se 1° livello, abilitazioni al terminal
112900060807      *     se 2° livello, abilitazioni al punto operativo
113000060807      *     se sede è impossibile ma se capita mando a fine il pgm
113100060807w   1c                   When      UteAut = *Blanks
113200060807if  2c                   If        DutLpo = '1'
113300060807     c                   Eval      wabi   = 'TP'
113400060807e   2c                   EndIf
113500060807if  2c                   If        DutLpo = '2'
113600060807     c                   Eval      wabi   = 'PO'
113700060807e   2c                   EndIf
113800060807      * carica le abilitazioni del profilo
113900060807x   1c                   Other
114000060807     c                   Movel     UteFaf        Dute01
114100060807     c                   If        §UteGtc <> *Blanks
114200060807     c                   Eval      wabi = §UteGtc
114300060807     c                   Else
114400060807     c                   Eval      wabi = UteAut
114500060807     c                   EndIf
114600060807e   1c                   EndSl
114700060807
114800060807      * controllo se ok l'abilitazione dell'utente
114900060807     c                   Clear                   Tibs02ds
115000060807     c                   Eval      T02Mod = 'C'
115100060807     c                   Eval      T02Sif = knsif
115200060807     c                   Eval      T02Cod = 'LAT'
115300060807     c                   Movel(p)  wabi          T02Ke1
115400060807     c                   Call      'TIBS02R'
115500060807     c                   Parm                    kpjba
115600060807     c                   Parm                    Tibs02ds
115700060807if  1c                   If        T02Err = *Blanks
115800060807     c                   Eval      dLat = T02Uni
115900060807e   1c                   EndIf
116000060807      * Reperimento dei P.O. gestibili dall'utente
116100060807     c                   clear                   TRUL31DS
116200060807     c                   eval      I31abi = wabi
116300060807     c                   eval      I31cdi = DUTdis
116400060807     c                   eval      I31car = DUTare
116500060807     c                   eval      I31cpo = DUTpou
116600060807     c                   call      'TRUL31R'
116700060807     c                   parm                    KPJBA
116800060807     c                   parm                    TRUL31DS
116900060807if  2c                   if        O31pog > *zeros
117000060807     c                   movea     s31           l1
117100060807e   2c                   endif
117200020924     C*
117300020924     C                   ENDSR
117400980806     C*------------------------------------------------------------------------*
117500980806     C* *INZSR - OPERAZIONI INIZIALI
117600980806     C*------------------------------------------------------------------------*
117700980806     C     *INZSR        BEGSR
117800980806     C*--------------------
117900980806     C* RICEVIMENTO PARAMETRI
118000980806     C*--------------------
118100980806     C     *ENTRY        PLIST
118200980806     C                   PARM                    KPJBA
118300091125     C                   movel     KPJBU         comdsf            1
118400980806     C*--------------------
118500980806     C* VARIABILI RIFERITE AL DATABASE
118600980806     C*--------------------
118700980806     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
118800980806     C     *LIKE         DEFINE    TBLCOD        KTBCOD
118900980806     C     *LIKE         DEFINE    TBLKEY        KTBKEY
119000980806     C     *LIKE         DEFINE    ACOKUT        KACKUT                         *CNAC000F
119100980806     C     *LIKE         DEFINE    ACOKCC        KACKCC
119200980806     C     *LIKE         DEFINE    ACOKSC        KACKSC
119300980806     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
119400980806     C     *LIKE         DEFINE    TAMKSC        KTMKSC                         *TNTAM00F
119500980806     C     *LIKE         DEFINE    TAMCTR        KTMCTR
119600980806     C     *LIKE         DEFINE    TAMPRG        KTMPRG
119700980806     C     *LIKE         DEFINE    TAMKSC        KFMKSC                         *TNOFM00F
119800980806     C     *LIKE         DEFINE    TAMCTR        KFMCTR
119900980806     C     *LIKE         DEFINE    TAMPRG        KFMPRG
120000980806     C     *LIKE         DEFINE    ACOKSC        KSCV1C                         *DEPOSITI VIDEO
120100980806     C     *LIKE         DEFINE    ORGFIL        FPAV1C
120200980806     C     *LIKE         DEFINE    ORGFIL        FARV1C
120300980806     C     *LIKE         DEFINE    ORGFIL        FAPV1C
120400980806     C     *LIKE         DEFINE    V1CDS1        DS1V1C
120500980806     C     *LIKE         DEFINE    V1CDS2        DS2V1C
120600020730     C     *LIKE         DEFINE    §NTWFIE       DV1FL1
120700981012     C     *LIKE         DEFINE    TAMKSC        CARKSC
120800011029     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
120900011029     C     *LIKE         DEFINE    TBEKE1        KTEKE1
121000020924     C     *LIKE         DEFINE    KPJBU         DEPKPJBU                       *depositi
121100980806     C*--------------------
121200980806     C* CHIAVI DI LETTURA
121300980806     C*--------------------
121400980806     C* LETTURA TABEL00F - COMPLETA
121500980806     C     KEYTAB        KLIST
121600980806     C                   KFLD                    KTBKUT                         *UTENTE
121700980806     C                   KFLD                    KTBCOD                         *TABELLA
121800980806     C                   KFLD                    KTBKEY                         *CODICE
121900980806     C* LETTURA TABEL00F - PARZIALE
122000980806     C     KE2TAB        KLIST
122100980806     C                   KFLD                    KTBKUT                         *UTENTE
122200980806     C                   KFLD                    KTBCOD                         *TABELLA
122300011029     C* LETTURA TNTBE01L - PARZIALE
122400011029     C     KEYTBE        KLIST
122500011029     C                   KFLD                    KTECOD                         *UTENTE
122600011029     C                   KFLD                    KTEKE1                         *CODICE
122700980806     C* LETTURA CNACO00F - COMPLETA
122800980806     C     KEYACO        KLIST
122900980806     C                   KFLD                    KACKUT                         *CODICE UTENTE
123000980806     C                   KFLD                    KACKCC                         *CODICE CAPOCONTO
123100980806     C                   KFLD                    KACKSC                         *CODICE CLIENTE
123200980806     C* LETTURA AZORG01L - COMPLETA
123300980806     C     KEYORG        KLIST
123400980806     C                   KFLD                    KORFIL                         *FILIALE
123500980806     C* LETTURA TNTAM01L - COMPLETA
123600980806     C     KEYTAM        KLIST
123700980806     C                   KFLD                    KTMKSC                         *CLIENTE
123800980806     C                   KFLD                    KTMCTR                         *TARIFFA
123900980806     C                   KFLD                    KTMPRG                         *PROGRESSIVO
124000980806     C* LETTURA TNOFM01L - COMPLETA
124100980806     C     KEYOFM        KLIST
124200980806     C                   KFLD                    KFMKSC                         *CLIENTE
124300980806     C                   KFLD                    KFMCTR                         *TARIFFA
124400980806     C                   KFLD                    KFMPRG                         *PROGRESSIVO
124500980806     C*--------------------
124600980806     C* VARIABILI NON RIFERITE AL DATABASE
124700980806     C*--------------------
124800980806     C                   MOVEL     'N'           $FINE             1            *FINE PROGRAMMA
124900980806     C                   MOVEL     '0'           $REC              1            *STATO RECORD
125000980806     C                   MOVEL     '0'           WERR              1            *ERRORE
125100980806     C                   MOVEL     *BLANKS       WMSG             78            *MESSAGGIO
125200980806     C                   MOVEL     '1'           $TESVD            1            *RIEMETTERE TESTATA
125300980806     C                   MOVEL     '1'           TIPVID            1            *VIDEO DA GESTIRE
125400980806     C*
125500980806     C* REPERISCE I DATI DELL'UTENTE
125600020924     C                   EXSR      REPDATIUTE
125700060804     C*
125800060804     c                   movel     'S'           v1cela            1
125900980806     C*
126000980806     C* CONTROLLA L'ENTRATA
126100980806     C                   MOVEL     KPJBU         D00KPJ            3            *LANCIO AZIONE KPJ
126200980806     C*
126300980806     C* CARICA LE TABELLE OCCORRENTI
126400980806     C                   EXSR      CARTAB
126500011029     C*
126600011029     C* CARICA LE NUOVE TABELLE OCCORRENTI
126700011029     C                   EXSR      CARGEDCE
126800980806     C*
126900980806     C* IMPOSTA LE DATE DI LAVORO
127000980806     C                   EXSR      IMPDAT
127100980806     C*
127200980806     C* IMPOSTAZIONI TESTATA
127300980806     C                   EXSR      IMPVD0
127400980806     C*
127500980806     C                   ENDSR
127600980806     O*------------------------------------------------------------------------*
127700980810** -MSG-
127800021031TNTE44R- Codice cliente inesistente o non in gestione.                        1
127900021106TNTE44R- Linea errata, non ingestione o non presente sull'elaboratore         2
128000980806TNTE44R- Data errata                                                          3
128100980806TNTE44R- Lasciare vuote o immettere entrambe le date                          4
128200980806TNTE44R- La data AL deve essere maggiore o uguale della data DAL              5
128300980806TNTE44R- Immettere almeno una selezione di bolle da elaborare                 6
128400980806TNTE44R- Immettere 'S' per simulare il lancio, 'N' per aggiornare l'archivio  7
128500091125TNTE44R- Il valore scelto non è corretto                                      8
128600980806TNTE44R- Tipo tariffa errato o non in gestione                                9
128700981012TNTE44R- Codice tariffa inesistente o non in gestione                         10
128800980806TNTE44R- Codice offerta inesistente                                           11
128900021031TNTE44R- 'I' per scrivere la tariffa italia, 'E' per scriverla come estero    12
129000980806TNTE44R- La tariffa italia non può avere filiali di partenza estere           13
129100980806TNTE44R- Codice tariffa errato.                                               14
129200980806TNTE44R- Per selezionare il P.O. lasciare vuote le altre selezioni            15
129300980806TNTE44R- La data DAL è inferiore alla data ultima di esistenza bolle          16
129400980806TNTE44R- La data AL non può essere maggiore della data corrente               17
129500980810TNTE44R- 'S' per scrivere la tariffa utilizzando i dati in sede, oppure 'F'   18
129600980810TNTE44R- Se lancio per sede è possibile creare i dati tipo solo come tariffa  19
129700060807TNTE44R- Cliente e date obbligatori                                           20
129800980810TNTE44R- Se lancio per sede è impossibile simulare la creazione               21
129900021029TNTE44R- Selezioni incongruenti, il cliente selezionato non è un unificante.  22
130000980828TNTE44R- La data AL non può essere maggiore della data ultima esistenza bolle 23
130100011029TNTE44R- ERRORE. Divisa attualmente non accettabile                           24
130200020228TNTE44R- Impossibile indicare linea FEDEX                                     25
130300020228TNTE44R- Impossibile indicare tariffa FEDEX                                   26
130400020313TNTE44R- Impossibile indicare filiale non in gestione                         27
130500980810TNTE44R- 28
130600980810TNTE44R- 29
130700980810TNTE44R- 30
