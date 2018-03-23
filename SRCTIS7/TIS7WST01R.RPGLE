000100970526     H*------------------------------------------------------------------------*
000200030929     H* STATISTICA MANUTENZIONE BOLLE
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800120927     Fcnaco00f  IF   E           K DISK
000900121019     fWFWST02l  IF   e           k disk    usropn
001000120927     FTIS7WST01dcf   e             WORKSTN INFDS(DEVDS1)
001100120927     F                                     SFILE(S7WST01DMS:MSGULT)
001200970526     D*------------------------------------------------------------------------*
001300970526     D* SCHIERE
001400970526     D*------------------------------------------------------------------------*
001500970526     D* MESSAGGI
001600970526     D*------------
001700000807     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
001800970526     D*------------
001900970526     D* DESCRIZIONI TASTI FUNZIONALI
002000970526     D*------------
002100970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002200970526     D*------------
002300970526     D* FUNZIONI DA CARICARE A VIDEO
002400970526     D*------------
002500970526     D FUV             S             19    DIM(24)
002600970526     D*------------------------------------------------------------------------*
002700970526     D* INPUT
002800970526     D*------------------------------------------------------------------------*
002900970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003000970526     D*------------
003100970526     D FUDDS           DS
003200970526     D  FUDDE1                 1      1
003300970526     D  FUDFUN                 2      3
003400970526     D  FUDDE2                 4     19
003500970526     D  FUDFI0                20     34
003600970526     D  FUDATT                35     35
003700970526     D  FUDFI1                36     80
003800970526     D  FUDDES                 1     19
003900970526     D  FUDRIG                 1     80
004000970526     D*------------
004100970526     D* DS GESTIONE TASTI FUNZIONALI
004200970526     D*------------
004300970526     D DSKEY         E DS
004400970526     D DEVDS1          DS
004500970526     D  KEY                  369    369
004600970526     D*------------
004700970526     D* DS "XMSG" - GESTIONE MESSAGGI
004800970526     D*------------
004900970526     D DSMSG         E DS
005000970528     D*------------------
005100970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
005200970528     D*------------------
005300970528     D WLBDA8          DS                  INZ
005400970528     D  G08DAT                 1      8  0
005500970528     D  G08INV                 9     16  0
005600970528     D  G08ERR                17     17
005700970528     D  G08TGI                18     22  0
005800970526     D*------------
005900970526     D* ARCHITETTURA
006000970526     D*------------
006100970526     D KPJBA         E DS
006200020923     D*-------------------
006300020923     D* DS REPERIMENTO DATI UTENTE
006400020923     D*-------------------
006500020923     D TIBS34DS      E DS                                                       *Profili utente
006600020923     D DDATIUTE      E DS                                                       *Dati utente
006700020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006800120927     D* DS REPERIMENTO PADRE/FIGLI
006900120927     D TIBS10DS      E DS
007000120928     D*-------------------
007100120928
007200121003     d Status         sds
007300121003     d  SDSpgm           *proc
007400121003     d  SDSprm           *parms
007500121003     d  SDSuser              254    263
007600121003
007700120928     d TIS7WST02R      pr                  extpgm('TIS7WST02R')
007800121016     d  Arch                               likeds(kpjba)
007900121024
008000121024     d wErrore         s              3s 0
008100121024     d TIS7WST03R      pr                  extpgm('TIS7WST02R')
008200121024     d  Errore                             like(wErrore)
008300121001     D*
008400121003     D*------------------
008500121003     D* DS X PASSAGGIO PARAMETRI
008600121003     D*------------------
008700121003     D PARAM           DS                  INZ
008800121003     d    wNrChia                     7s 0
008900121003     d    wDAD                        8s 0
009000121003     d    wDAA                        8s 0
009100121003     d    wCCM                        7s 0
009200121016     d    wSU                         1
009300121003     d    wVerTrk                     1
009400121003     d    wChkXml                     1
009500121003     d    wTipExe                     4
009600121003     d    wRmn                       15s 0
009700121003     d    wRma                       15
009800121003     d    wAAS                        4s 0
009900121003     d    wLNP                        3s 0
010000121003     d    wNRS                        2s 0
010100121003     d    wNSP                        7s 0
010200121019     d    wIdJob                     50    varying
010300121029     d    wNRS1                       2s 0
010400120928
010500970526     C*------------------------------------------------------------------------*
010600000303     C* MAIN LINES
010700970526     C*------------------------------------------------------------------------*
010800970526     C*
010900120927     C* metto i default su D03
011000120927     C                   EXSR      InzD03
011100120927     C*
011200120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011300991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011400970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011500970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011600970526     C*
011700970526     C* EMETTE FORMATO VIDEO
011800970526     C                   EXSR      BUTFMR
011900970526     C*
012000970526     C* GESTIONE TASTI FUNZIONALI
012100970526     C                   EXSR      FUNCON
012200970526E   2C                   ENDDO
012300970526     C*
012400970526     C* FINE PGM
012500970526     C                   SETON                                        LR
012600970526     C*------------------------------------------------------------------------*
012700970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
012800970526     C*------------------------------------------------------------------------*
012900970526     C     BUTFMR        BEGSR
013000970526     C*
013100970526     C* EMETTE LA TESTATA
013200120927     C                   WRITE     S7WST01D01
013300970526     C*
013400970526     C* SPEDISCE EVENTUALE MESSAGGIO
013500970526     C                   EXSR      SNDMSG
013600970526     C*
013700970526     C* EMETTE LE FUNZIONI
013800120927     C                   WRITE     S7WST01D02
013900970526     C*
014000970526     C* EMETTE IL VIDEO
014100120927     C                   EXFMT     S7WST01D03
014200970526     C*
014300991214     C* REIMPOSTA FLAG DI ERRORE
014400991214     C                   MOVEL     '0'           ERRL00
014500970526     C*
014600970526     C* REIMPOSTA INDICATORI DI ERRORE
014700020611     C                   SETOFF                                       555556
014800970526     C*
014900970526     C                   ENDSR
015000970526     C*------------------------------------------------------------------------*
015100120927     C* InzD03 - Inizializzo D03
015200970526     C*------------------------------------------------------------------------*
015300120927     C     InzD03        BEGSR
015400970526     C*
015500120927     C                   EVAL      VidVerTrk = 'W'
015600120927     C                   EVAL      VidChkXml = 'N'
015700120927     C                   EVAL      VidSU     = 'S'
015800120927     C*
015900120927     C                   ENDSR
016000120927     C*------------------------------------------------------------------------*
016100120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
016200120927     C*------------------------------------------------------------------------*
016300120927     C     SNDMSG        BEGSR
016400120927     C*
016500970526     C* PULISCE IL SFILE MESSAGGI
016600970526     C                   MOVEL     '1'           *IN40
016700120927     C                   WRITE     S7WST01DMC
016800970526     C                   MOVEL     '0'           *IN40
016900970526     C*
017000970526     C* VISUALIZZA IL MESSAGGIO
017100970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
017200970526     C                   CALL      'XMSG'        XMSGPA
017300020611     C     DSMSER        IFEQ      *ZEROS
017400970526     C                   MOVEL     DSMSKE        MSGKEY
017500020611     C                   ENDIF
017600970526     C                   Z-ADD     1             MSGULT
017700120927     C                   WRITE     S7WST01DMS
017800120927     C                   WRITE     S7WST01DMC
017900970526     C*
018000970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
018100970526     C                   MOVEL     *BLANKS       DSMSMS
018200970526     C*
018300970526     C                   ENDSR
018400970526     C*------------------------------------------------------------------------*
018500970526     C* ENTGES - GESTIONE ENTER -> INVIO
018600970526     C*------------------------------------------------------------------------*
018700970526     C     ENTGES        BEGSR
018800970526     C*
018900970526     C* CONTROLLA I CAMPI IMMESSI
019000970526     C                   EXSR      DECODI
019100970526     C*
019200970526     C                   ENDSR
019300970526     C*------------------------------------------------------------------------*
019400970526     C* F03GES - GESTIONE F03 -> FINE
019500970526     C*------------------------------------------------------------------------*
019600970526     C     F03GES        BEGSR
019700970526     C*
019800970526     C* IMPOSTA "PREMUTO F03"
019900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
020000970526     C*
020100970526     C                   ENDSR
020200970526     C*------------------------------------------------------------------------*
020300991214     C* F06GES - GESTIONE F06 -> CONFERMA
020400970526     C*------------------------------------------------------------------------*
020500970526     C     F06GES        BEGSR
020600970526     C*
020700991214     C* CONTROLLA I CAMPI IMMESSI
020800991214     C                   EXSR      DECODI
020900020611     C*
021000991214     C* ESEGUE ROUTINE DI CONFERMA
021100020611     C     ERRL00        IFEQ      '0'
021200991214     C                   EXSR      ESEGUI
021300031001     C* IMPOSTA FINE PROGRAMMA
021400031001     C                   MOVEL     '1'           FINL00
021500991214     C                   ENDIF
021600970526     C*
021700991214     C                   ENDSR
021800121024     C*------------------------------------------------------------------------*
021900121024     C* F07RIP - GESTIONE F07 -> Ripristina l'esistente
022000121024     C*------------------------------------------------------------------------*
022100121024     C     F07Rip        BEGSR
022200121024     C*
022300121024     C                   EVAL      wErrore = 0
022400121024     C*
022500121024     C                   CALL      'TIS7WST03R'
022600121024     C                   PARM                    wErrore
022700121024     C*
022800121024     C*se c'è stato un errore lo segnalo, come segnalo se è andato tutto bene
022900121024     C                   IF        wErrore <> 0
023000121024     C                   EVAL      ERRL00 = '1'
023100121024     C                   MOVEL     MSG(23)       DSMSMS
023200121024     C                   ELSE
023300121024     C                   MOVEL     MSG(24)       DSMSMS
023400121024     C                   ENDIF
023500121024     C*
023600121024     C                   ENDSR
023700970526     C*------------------------------------------------------------------------*
023800970526     C* F12GES - GESTIONE F12 -> RITORNO
023900970526     C*------------------------------------------------------------------------*
024000970526     C     F12GES        BEGSR
024100970526     C*
024200970526     C* IMPOSTA "PREMUTO F12"
024300970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
024400970526     C*
024500970526     C                   ENDSR
024600970526     C*------------------------------------------------------------------------*
024700970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
024800970526     C*------------------------------------------------------------------------*
024900970526     C     F24GES        BEGSR
025000970526     C*
025100970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
025200970526     C*
025300970526     C                   ENDSR
025400970526     C*------------------------------------------------------------------------*
025500991214     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
025600970526     C*------------------------------------------------------------------------*
025700991214     C     ESEGUI        BEGSR
025800991214     C*
025900121001     C                   EVAL      wNrChia  =    VidNrChia
026000121001     C                   EVAL      wDAD     =    VidDAD
026100121001     C                   EVAL      wDAA     =    VidDAA
026200121001     C                   EVAL      wCCM     =    VidCCM
026300121016     C                   EVAL      wSU      =    VidSU
026400121001     C                   EVAL      wVerTrk  =    VidVerTrk
026500121001     C                   EVAL      wChkXml  =    VidChkXml
026600121001     C                   EVAL      wTipExe  =    VidTipExe
026700121001     C                   EVAL      wRmn     =    VidRmn
026800121001     C                   EVAL      wRma     =    VidRma
026900121001     C                   EVAL      wAAS     =    VidAAS
027000121001     C                   EVAL      wLNP     =    VidLNP
027100121001     C                   EVAL      wNRS     =    VidNRS
027200121001     C                   EVAL      wNSP     =    VidNSP
027300121029     C                   EVAL      wNRS1    =    VidNRS1
027400121016     C                   MOVEL     PARAM         KPJBU
027500121003
027600121019     C* se richiesto pulisco il work-file
027700121019     C                   IF        VidClr = 'S'
027800121019     C                   CLOSE     WFWST02L
027900121019     C                   EVAL      depcmd = 'CLRPFM WFWST00F'
028000121019     C                   Z-ADD     15            LENGH            15 5
028100121019     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
028200121019     C                   PARM                    depcmd          100
028300121019     C                   PARM                    LENGH
028400121019     C                   ENDIF
028500121019
028600121016     C                   IF        VidElab = 'I'
028700121003     C* chiamo in interattivo
028800121001     C                   CALL      'TIS7WST02R'
028900121016     C                   PARM                    KPJBA
029000121003     C                   ELSE
029100121016     C* sottometto
029200121016     C                   MOVEL     'S7W2'        KCOAZ
029300121016     C                   CALL      'BCH10'
029400121016     C                   PARM                    KPJBA
029500121003     C                   ENDIF
029600991214     C*
029700970526     C                   ENDSR
029800970526     C*------------------------------------------------------------------------*
029900991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
030000970526     C*------------------------------------------------------------------------*
030100970526     C     DECODI        BEGSR
030200000306     C*
030300121019     C* apro il file delle estrazioni se chiuso
030400121019     C                   IF        not %open(WFWST02L)
030500121019     C                   OPEN      WFWST02L
030600121019     C                   ENDIF
030700121019     C*
030800120927     C                   SETOFF                                       505152
030900120927     C                   SETOFF                                       535455
031000120927     C                   SETOFF                                       565758
031100121019     C                   SETOFF                                       596061
031200120927     C*
031300121019     C* Id di lancio inesistente (test inutile se pulirò il file)
031400120927     C     ERRL00        IFEQ      '0'
031500121023     C                   EVAL      wIdJob   =    VidIdJob
031600121019     C                   IF        VIDIdJob <> *blank
031700121019     C                             and VidClr <> 'S'
031800121019     C                   EVAL      WFWIdJob   =    VidIdJob
031900121019     C     VidIdJob      CHAIN(N)  WFWST02L
032000121019     C                   IF        %found
032100120927     C                   EVAL      ERRL00 = '1'
032200121019     C                   SETON                                        60
032300121019     C                   MOVEL     MSG(07)       DSMSMS
032400121023     C                   EVAL      wIdJob   =    *blank
032500120927     C                   ENDIF
032600120927     C                   ENDIF
032700121019     C                   ENDIF
032800120927     C*
032900120927     C* DATA INIZIO formalmente valida
033000120927     C     ERRL00        IFEQ      '0'
033100120927     C                   Z-ADD     VIDDAD        WDA8
033200120927     C                   Z-ADD     *ZEROS        DA8W
033300120927     C                   EXSR      CHKDA8
033400120927     C     ERRL00        IFEQ      '1'
033500120927     C                   SETON                                        54
033600120927     C                   MOVEL     MSG(14)       DSMSMS
033700120927     C                   ENDIF
033800120927     C                   ENDIF
033900120927     C*
034000120927     C* DATA FINE formalmente valida
034100020611     C     ERRL00        IFEQ      '0'
034200030929     C                   Z-ADD     VIDDAA        WDA8
034300020611     C                   Z-ADD     *ZEROS        DA8W
034400970528     C                   EXSR      CHKDA8
034500020611     C     ERRL00        IFEQ      '1'
034600991214     C                   SETON                                        55
034700970528     C                   MOVEL     MSG(14)       DSMSMS
034800020611     C                   ENDIF
034900020611     C                   ENDIF
035000020611     C*
035100120927     C* Fine > Inizio
035200020611     C     ERRL00        IFEQ      '0'
035300120927     C     VIDDAA        IFLT      VIDDAD
035400020611     C                   SETON                                        5455
035500970528     C                   MOVEL     MSG(15)       DSMSMS
035600970528     C                   MOVEL     '1'           ERRL00
035700020611     C                   ENDIF
035800020611     C                   ENDIF
035900000306     C*
036000120927     C* Ctrl i campi obbligatori
036100120927     C     ERRL00        IFEQ      '0'
036200120927     C                   SELECT
036300121019     C* cod.cliente
036400121019     C                   WHEN      VIDCCM = 0
036500121019     C                   MOVEL     '1'           ERRL00
036600121019     C                   SETON                                        5159
036700121019     C                   MOVEL     MSG(6)        DSMSMS
036800121019     C                   MOVEL     *BLANKS       VDDCCM
036900121019     C* id lancio
037000121019     C                   WHEN      VIDIdJob = *blank
037100121019     C                   MOVEL     '1'           ERRL00
037200121019     C                   SETON                                        60
037300121019     C                   MOVEL     MSG(6)        DSMSMS
037400120927     C* flag singolo/unificante
037500120927     C                   WHEN      VIDSU  = *blank
037600120927     C                   MOVEL     '1'           ERRL00
037700120927     C                   SETON                                        53
037800120927     C                   MOVEL     MSG(6)        DSMSMS
037900120927     C* versione tracking
038000120927     C                   WHEN      VIDVERTRK = *blank
038100120927     C                   MOVEL     '1'           ERRL00
038200120927     C                   SETON                                        56
038300120927     C                   MOVEL     MSG(6)        DSMSMS
038400120927     C* flag verifica esito xml
038500120927     C                   WHEN      VIDCHKXML = *blank
038600120927     C                   MOVEL     '1'           ERRL00
038700120927     C                   SETON                                        57
038800120927     C                   MOVEL     MSG(6)        DSMSMS
038900120927     C* tipo interrogazione
039000120927     C                   WHEN      VIDTIPEXE = *blank
039100120927     C                   MOVEL     '1'           ERRL00
039200120927     C                   SETON                                        58
039300120927     C                   MOVEL     MSG(6)        DSMSMS
039400120927     C                   ENDSL
039500120927     C                   ENDIF
039600121001     C*
039700121001     C* Cod. cliente mittente esistente su CNACO
039800121001     C     ERRL00        IFEQ      '0'
039900120927     C                   Z-ADD     1             ACOkut
040000120927     C                   Z-ADD     DUTKCI        ACOkcc
040100120927     C                   Z-ADD     VIDCCM        ACOksc
040200120927     C     K03ACO00      CHAIN     cnaco00f
040300120927     C                   IF        %found(cnaco00f)
040400120927     C                   MOVEL     ACOrag        VDDCCM
040500120927     C                   ELSE
040600120927     C                   MOVEL     '1'           ERRL00
040700120927     C                   SETON                                        51
040800120927     C                   MOVEL     MSG(3)        DSMSMS
040900120927     C                   MOVEL     *BLANKS       VDDCCM
041000120927     C                   ENDIF
041100120927     C                   ENDIF
041200120927     C*
041300120927     C* Ctrl cod. cliente sia PADRE se richiesto U=Unificante
0414001209281    C     ERRL00        IFEQ      '0'
0415001209282    C                   IF        VIDSU = 'U'
041600120927     C                   CLEAR                   TIBS10DS
041700120927     C                   EVAL      D10DRF = %dec(%Date() : *ISO)                *data rif. = oggi
041800120927     C                   EVAL      D10TLE = 'WW'                                *tipo legame
041900120927     C                   EVAL      D10PAF = 'F'                                 *verifica se padre
042000120927     C                   EVAL      D10COD = VIDCCM
042100120927     C                   CALL      'TIBS10R'
042200120927     C                   PARM                    TIBS10DS
042300120927     C* se non è PADRE, controllo se è FIGLIO o stand alone
0424001209283    C                   IF        D10ERR <> *BLANKS
042500120927     C                   CLEAR                   TIBS10DS
042600120927     C                   EVAL      D10DRF = %dec(%Date() : *ISO)                *data rif. = oggi
042700120927     C                   EVAL      D10TLE = 'WW'                                *tipo legame
042800120927     C                   EVAL      D10PAF = 'P'                                 *verifica se figlio
042900121001     C                   EVAL      D10COD = VIDCCM
043000120927     C                   CALL      'TIBS10R'
043100120927     C                   PARM                    TIBS10DS
043200120927     C* se non è né PADRE né FIGLIO, allora è uno stand alone
0433001209284    C                   IF        D10ERR <> *BLANKS
043400120927     C* in questo caso non do errore ma forzo il flag Singolo/Unificante a Singolo
043500120927     C                   EVAL      VIDSU = 'S'
043600120927     C* se invece è FIGLIO, do errore e forzo il cod. cliente col padre ricevuto
0437001209284x   C                   ELSE
043800120927     C                   MOVEL     '1'           ERRL00
043900120927     C                   SETON                                        51
044000120927     C                   MOVEL     MSG(4)        DSMSMS
044100121001     C                   EVAL      VIDCCM = D10COP
0442001209284e   C                   ENDIF
0443001210163x   C* ho indicato un padre e avrei ottenuto i suoi figl
044400120928     C                   ELSE
044500121016     C***                EVAL      wSKC = D10SKC
0446001209283e   C                   ENDIF
044700121016     C* se richiesta l'operazione per il singolo CCM non faccio nulla, ci pensa il chiamato
0448001209282x   C                   ELSE
0449001209282e   C                   ENDIF
0450001209281e   C                   ENDIF
045100120927     C*
045200120927     C* Ctrl che il valore della spedizione abbia un senso compiuto
045300120927     C* (se qualcosa è valorizzato, ci devono essere anno/linea/spedizione)
045400120927     C     ERRL00        IFEQ      '0'
045500120927     C                   IF        (VIDAAS <> 0 or VIDLNP <> 0 or VIDNRS <> 0 or
045600120927     C                              VIDNSP <> 0) and
045700120927     C                             (VIDAAS = 0 or VIDLNP = 0 or VIDNSP = 0)
045800120927     C                   MOVEL     '1'           ERRL00
045900120927     C                   SETON                                        52
046000120927     C                   MOVEL     MSG(5)        DSMSMS
046100120927     C                   ENDIF
046200120927     C                   ENDIF
046300120927     C*
046400970528     C                   ENDSR
046500970528     C*------------------------------------------------------------------------*
046600970528     C* CHKDA8 - CONTROLLA LA DATA (8)
046700970528     C*------------------------------------------------------------------------*
046800970528     C     CHKDA8        BEGSR
046900970528     C*
047000120927     C* le date sono tutte in formato yyyyMMdd
047100030929     C     *LIKE         DEFINE    VIDDAD        WDA8
047200030929     C     *LIKE         DEFINE    VIDDAA        DA8W
047300970528     C*
047400120927     C                   Z-ADD     WDA8          G08INV
047500120927     C                   Z-ADD     *ZEROS        G08DAT
047600120927     C                   MOVEL     '3'           G08ERR
047700970528     C                   CALL      'XSRDA8'
047800970528     C                   PARM                    WLBDA8
047900970528     C                   Z-ADD     G08DAT        WDA8
048000970528     C                   Z-ADD     G08INV        DA8W
048100020611     C     G08ERR        IFEQ      '1'
048200970528     C                   MOVEL     '1'           ERRL00
048300020611     C                   ENDIF
048400970528     C*
048500970528     C                   ENDSR
048600970526     C*------------------------------------------------------------------------*
048700970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
048800970526     C*------------------------------------------------------------------------*
048900970526     C     AZZVID        BEGSR
049000970526     C* CAMPI
049100030929     C                   Z-ADD     *ZEROS        VIDDAD
049200030929     C                   Z-ADD     *ZEROS        VIDDAA
049300970526     C*
049400970526     C                   ENDSR
049500970526     C*------------------------------------------------------------------------*
049600020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
049700970526     C*------------------------------------------------------------------------*
049800970526     C     IMPFUN        BEGSR
049900970526     C*
050000970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
050100970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
050200970526     C                   EXSR      FUNGES
050300970526     C*
050400970526     C                   ENDSR
050500020923     C*--------------------------------------------------------------------------------------------*
050600020923     C* REPERISCE I DATI UTENTE
050700020923     C*--------------------------------------------------------------------------------------------*
050800020923     C     REPDATIUTE    BEGSR
050900020923     C*
051000020923     C* INIZIALIZZA VARIABILI DI WRK
051100020923     C                   CLEAR                   TIBS34DS
051200020923     C                   CLEAR                   AZUTEDS
051300020923     C                   CLEAR                   DDATIUTE
051400020923     C*
051500020923     C     *DTAARA       DEFINE    §azute        azuteds
051600020923     C     *DTAARA       DEFINE    §datiute      ddatiute
051700020923     C                   IN(E)     *DTAARA
051800020923if  1C                   IF        %Error
051900020923     c                   EVAL      I34Tla = 'L'
052000020923     C                   CALL      'TIBS34R'
052100020923     C                   PARM                    Tibs34Ds
052200020923     C                   IN        *DTAARA
052300020923e   1C                   ENDIF
052400020923     C*
052500020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
052600020923     C                   MOVEL(P)  RSUT          DSFIRS           20
052700020923     C*
052800020923     C                   ENDSR
052900970526     C*------------------------------------------------------------------------*
053000970526     C* *INZSR - OPERAZIONI INIZIALI
053100970526     C*------------------------------------------------------------------------*
053200970526     C     *INZSR        BEGSR
053300991217     C*
053400991217     C     *ENTRY        PLIST
053500991217     C                   PARM                    KPJBA
053600120927     C*
053700120927     C* LETTURA cnaco00f -
053800120927     C     K03ACO00      KLIST
053900120927     C                   KFLD                    ACOkut
054000120927     C                   KFLD                    ACOkcc
054100120927     C                   KFLD                    ACOksc
054200020923     C*
054300020923     C* REPERISCE I DATI DELL'UTENTE
054400020923     C                   EXSR      REPDATIUTE
054500970526     C*
054600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
054700970526     C     XMSGPA        PLIST
054800970526     C                   PARM                    DSMSMS
054900970526     C                   PARM                    DSMSPG
055000970526     C                   PARM                    DSMSKE
055100970526     C                   PARM                    DSMSER
055200970526     C*
055300970526     C* IMPOSTA LA DS -TASTI PREMUTI-
055400970526     C                   CALL      'XKEY'
055500970526     C                   PARM                    DSKEY
055600970526     C*---
055700970526     C* VARIABILI NON RIFERITE AL DATA BASE
055800970526     C*---
055900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
056000991214     C                   MOVEL     *BLANKS       DEPOP1            3
056100970526     C*---
056200970526     C* VARIABILI DI CONTROLLO
056300970526     C*---
056400970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
056500970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
056600970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
056700970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
056800970526     C*
056900970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
057000120927     C                   MOVEL     'TIS7WST01R'  DSMSPG
057100120927     C                   MOVEL     'TIS7WST01R'  MSGQUE
057200970526     C                   MOVEL     *BLANKS       MSGKEY
057300970526     C                   MOVEL     *BLANKS       DSMSMS
057400970528     C*
057500970528     C* CALCOLA LA DATA CORRENTE
057600970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
057700970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
057800970528     C                   Z-ADD     WN8           G08DAT
057900970528     C                   Z-ADD     *ZEROS        G08INV
058000970528     C                   MOVEL     '0'           G08ERR
058100970528     C                   CALL      'XSRDA8'
058200970528     C                   PARM                    WLBDA8
058300970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
058400000310     C*
058500000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
058600000310     C                   EXSR      AZZVID
058700970526     C*
058800970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
058900970526     C                   EXSR      IMPFUN
059000970526     C*
059100970526     C                   ENDSR
059200970526     C*------------------------------------------------------------------------*
059300970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
059400970526     C*------------------------------------------------------------------------*
059500970526     C     FUNGES        BEGSR
059600970526     C*
059700970526     C* PULISCO CAMPI DI LAVORO
059800970526     C                   Z-ADD     0             W                 5 0
059900970526     C*
060000970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
060100970526     C* E CARICO TABELLA FUNZIONI ATTIVE
060200970526     C     1             DO        24            I
060300970526     C                   MOVEL     FUD(I)        FUDRIG
060400970526     C*
060500970526     C* PULIZIA FUNZIONI PRECEDENTI
060600970526     C                   MOVEL     *BLANKS       FUV(I)
060700970526     C*
060800970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
060900970526     C* (SALTA F24)
061000970526     C     FUDATT        IFEQ      'S'
061100970526     C     FUDFUN        ANDNE     '24'
061200970526     C                   ADD       1             W
061300970526     C                   MOVEL     FUDDES        FUV(W)
061400970526     C                   END
061500970526     C                   END                                                    DO WHILE
061600970526     C*
061700970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
061800970526     C* ALTRIMENTI DISATTIVO F24
061900970526     C     1             DO        24            I
062000970526     C                   MOVEL     FUD(I)        FUDRIG
062100970526     C*
062200970526     C     FUDFUN        IFEQ      '24'
062300970526     C     W             IFGT      6
062400970526     C                   MOVEL     'S'           FUDATT
062500970526     C                   ELSE
062600970526     C                   MOVEL     *BLANKS       FUDATT
062700970526     C                   END                                                    W GT 8
062800970526     C                   MOVEL     FUDRIG        FUD(I)
062900970526     C                   END
063000970526     C                   END                                                    DO WHILE
063100970526     C*
063200970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
063300970526     C     W             DIV       6             I
063400970526     C                   MVR                     Z
063500970526     C*
063600970526     C     Z             IFNE      0
063700970526     C     I             ADD       1             Z
063800970526     C                   ELSE
063900970526     C                   Z-ADD     I             Z                 5 0
064000970526     C                   END
064100970526     C*
064200970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
064300970526     C                   ADD       1             FUNGRU
064400970526     C     FUNGRU        IFGT      Z
064500970526     C     FUNGRU        ORLT      1
064600970526     C                   Z-ADD     1             FUNGRU
064700970526     C                   END
064800970526     C*
064900970526     C* PRIMO GRUPPO DI 6 FUNZIONI
065000970526     C     FUNGRU        IFEQ      1
065100970526     C                   MOVEL     FUV(1)        FUN001
065200970526     C                   MOVEL     FUV(2)        FUN002
065300970526     C                   MOVEL     FUV(3)        FUN003
065400970526     C                   MOVEL     FUV(4)        FUN004
065500970526     C                   MOVEL     FUV(5)        FUN005
065600970526     C                   MOVEL     FUV(6)        FUN006
065700970526     C                   END
065800970526     C*
065900970526     C* SECONDO GRUPPO DI 6 FUNZIONI
066000970526     C     FUNGRU        IFEQ      2
066100970526     C                   MOVEL     FUV(7)        FUN001
066200970526     C                   MOVEL     FUV(8)        FUN002
066300970526     C                   MOVEL     FUV(9)        FUN003
066400970526     C                   MOVEL     FUV(10)       FUN004
066500970526     C                   MOVEL     FUV(11)       FUN005
066600970526     C                   MOVEL     FUV(12)       FUN006
066700970526     C                   END
066800970526     C*
066900970526     C* TERZO GRUPPO DI 6 FUNZIONI
067000970526     C     FUNGRU        IFEQ      3
067100970526     C                   MOVEL     FUV(13)       FUN001
067200970526     C                   MOVEL     FUV(14)       FUN002
067300970526     C                   MOVEL     FUV(15)       FUN003
067400970526     C                   MOVEL     FUV(16)       FUN004
067500970526     C                   MOVEL     FUV(17)       FUN005
067600970526     C                   MOVEL     FUV(18)       FUN006
067700970526     C                   END
067800970526     C*
067900970526     C* QUARTO GRUPPO DI 6 FUNZIONI
068000970526     C     FUNGRU        IFEQ      4
068100970526     C                   MOVEL     FUV(19)       FUN001
068200970526     C                   MOVEL     FUV(20)       FUN002
068300970526     C                   MOVEL     FUV(21)       FUN003
068400970526     C                   MOVEL     FUV(22)       FUN004
068500970526     C                   MOVEL     *BLANKS       FUN005
068600970526     C                   MOVEL     *BLANKS       FUN006
068700970526     C                   END
068800970526     C*
068900970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
069000970526     C                   MOVEL     *BLANKS       FUN007
069100970526     C                   MOVEL     *BLANKS       FUN008
069200970526     C*
069300970526     C     1             DO        24            I
069400970526     C                   MOVEL     FUD(I)        FUDRIG
069500970526     C     FUDFUN        IFEQ      '24'
069600970526     C     FUDATT        ANDEQ     'S'
069700970526     C                   MOVEL     FUDDES        FUN008
069800970526     C                   END
069900970526     C                   END                                                    DO
070000970526     C*
070100970526     C                   ENDSR
070200970526     C*------------------------------------------------------------------------*
070300970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
070400970526     C*------------------------------------------------------------------------*
070500970526     C     FUNCON        BEGSR
070600970526     C*
070700970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
070800970526     C                   EXSR      FUNVAR
070900970526     C*
071000970526     C* PULISCO CAMPI DI LAVORO
071100970526     C                   Z-ADD     0             I                 5 0
071200970526     C                   Z-ADD     0             W                 5 0
071300970526     C*
071400970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
071500970526     C     FUNKEY        IFNE      *BLANKS
071600970526     C     1             DO        24            I
071700970526     C                   MOVEL     FUD(I)        FUDRIG
071800970526     C*
071900970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
072000970526     C     FUNKEY        IFEQ      FUDFUN
072100970526     C     FUDATT        ANDEQ     'S'
072200970526     C                   Z-ADD     I             W
072300970526     C                   END
072400970526     C                   END                                                    DO WHILE
072500970526     C*
072600970526     C* FUNZIONE NON ATTIVA (ERRORE)
072700970526     C     W             IFEQ      0
072800970526     C*
072900970526     C* EMISSIONE MESSAGGIO DI ERRORE
073000970526     C                   MOVEL     MSG(1)        DSMSMS
073100970526     C*
073200970526     C                   END                                                    W=0
073300970526     C                   END                                                    FUNKEY NE *BLANKS
073400970526     C*
073500970526     C* FUNZIONE ATTIVA (PROCEDI ...)
073600970526     C     W             IFNE      0
073700970526     C     FUNKEY        OREQ      *BLANKS
073800970526     C     FUNKEY        IFEQ      *BLANKS
073900970526     C                   EXSR      ENTGES
074000970526     C                   END
074100970526     C     FUNKEY        IFEQ      '03'
074200970526     C                   EXSR      F03GES
074300970526     C                   END
074400970526     C     FUNKEY        IFEQ      '06'
074500970526     C                   EXSR      F06GES
074600970526     C                   END
074700121024     C     FUNKEY        IFEQ      '07'
074800121024     C                   EXSR      F07RIP
074900121024     C                   END
075000970526     C     FUNKEY        IFEQ      '12'
075100970526     C                   EXSR      F12GES
075200970526     C                   END
075300970526     C     FUNKEY        IFEQ      '24'
075400970526     C                   EXSR      F24GES
075500970526     C                   END
075600970526     C                   END
075700970526     C*
075800970526     C                   ENDSR
075900970526     C*------------------------------------------------------------------------*
076000970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
076100970526     C*------------------------------------------------------------------------*
076200970526     C     FUNVAR        BEGSR
076300970526     C*
076400970526     C* INIZIALIZZA VARIABILE -FUNKEY-
076500970526     C*
076600970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
076700970526     C     KEY           IFEQ      ENTER
076800970526     C                   MOVEL     *BLANKS       FUNKEY
076900970526     C                   END
077000970526     C     KEY           IFEQ      F1
077100970526     C                   MOVEL     '01'          FUNKEY
077200970526     C                   END
077300970526     C     KEY           IFEQ      F2
077400970526     C                   MOVEL     '02'          FUNKEY
077500970526     C                   END
077600970526     C     KEY           IFEQ      F3
077700970526     C                   MOVEL     '03'          FUNKEY
077800970526     C                   END
077900970526     C     KEY           IFEQ      F4
078000970526     C                   MOVEL     '04'          FUNKEY
078100970526     C                   END
078200970526     C     KEY           IFEQ      F5
078300970526     C                   MOVEL     '05'          FUNKEY
078400970526     C                   END
078500970526     C     KEY           IFEQ      F6
078600970526     C                   MOVEL     '06'          FUNKEY
078700970526     C                   END
078800970526     C     KEY           IFEQ      F7
078900970526     C                   MOVEL     '07'          FUNKEY
079000970526     C                   END
079100970526     C     KEY           IFEQ      F8
079200970526     C                   MOVEL     '08'          FUNKEY
079300970526     C                   END
079400970526     C     KEY           IFEQ      F9
079500970526     C                   MOVEL     '09'          FUNKEY
079600970526     C                   END
079700970526     C     KEY           IFEQ      F10
079800970526     C                   MOVEL     '10'          FUNKEY
079900970526     C                   END
080000970526     C     KEY           IFEQ      F11
080100970526     C                   MOVEL     '11'          FUNKEY
080200970526     C                   END
080300970526     C     KEY           IFEQ      F12
080400970526     C                   MOVEL     '12'          FUNKEY
080500970526     C                   END
080600970526     C     KEY           IFEQ      F13
080700970526     C                   MOVEL     '13'          FUNKEY
080800970526     C                   END
080900970526     C     KEY           IFEQ      F14
081000970526     C                   MOVEL     '14'          FUNKEY
081100970526     C                   END
081200970526     C     KEY           IFEQ      F15
081300970526     C                   MOVEL     '15'          FUNKEY
081400970526     C                   END
081500970526     C     KEY           IFEQ      F16
081600970526     C                   MOVEL     '16'          FUNKEY
081700970526     C                   END
081800970526     C     KEY           IFEQ      F17
081900970526     C                   MOVEL     '17'          FUNKEY
082000970526     C                   END
082100970526     C     KEY           IFEQ      F18
082200970526     C                   MOVEL     '18'          FUNKEY
082300970526     C                   END
082400970526     C     KEY           IFEQ      F19
082500970526     C                   MOVEL     '19'          FUNKEY
082600970526     C                   END
082700970526     C     KEY           IFEQ      F20
082800970526     C                   MOVEL     '20'          FUNKEY
082900970526     C                   END
083000970526     C     KEY           IFEQ      F21
083100970526     C                   MOVEL     '21'          FUNKEY
083200970526     C                   END
083300970526     C     KEY           IFEQ      F22
083400970526     C                   MOVEL     '22'          FUNKEY
083500970526     C                   END
083600970526     C     KEY           IFEQ      F23
083700970526     C                   MOVEL     '23'          FUNKEY
083800970526     C                   END
083900970526     C     KEY           IFEQ      F24
084000970526     C                   MOVEL     '24'          FUNKEY
084100970526     C                   END
084200970526     C     KEY           IFEQ      ROLLUP
084300970526     C                   MOVEL     'RU'          FUNKEY
084400970526     C                   END
084500970526     C     KEY           IFEQ      HELP
084600970526     C                   MOVEL     'HP'          FUNKEY
084700970526     C                   END
084800970526     C                   ENDSR
084900970526     C*------------------------------------------------------------------------*
085000970526     C* NOTE COSTRUZIONE SCHIERE
085100970526     C*------------------------------------------------------------------------*
085200970526     C*
085300970526     C* -MSG-MESSAGGI
085400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
085500970526     C* (NON TOCCARE MSG,1 E MSG,2)
085600970526     C*
085700970526     C* -CMD-COMANDI
085800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
085900970526     C*
086000970526     C* -OPD-OPZIONI
086100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
086200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
086300970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
086400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
086500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
086600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
086700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
086800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
086900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
087000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
087100970526     C*
087200970526     C* -FUD-FUNZIONI
087300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
087400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
087500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
087600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
087700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
087800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
087900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
088000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
088100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
088200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
088300970526     O*------------------------------------------------------------------------*
088400970526** MSG - MESSAGGI
088500120927Tasto funzionale non gestito                                                   1
088600120927Numero chiamate obbligatorio                                                   2
088700120927Codice cliente non esiste su CNACO00F                                          3
088800120927Cliente unificante non è "padre" ma "figlio": forzato col relativo "padre"     4
088900120927Se indicato "Solo spedizione" devono essere presenti anno/linea/spedizione     5
089000120927Campo obbligatorio                                                             6
089100121019Id di lancio già utilizzata                                                    7
089200120927                                                                               8
089300120927                                                                               9
089400120927                                                                               10
089500120927                                                                               11
089600120927                                                                               12
089700120927                                                                               13
089800120927Data errata                                                                    14
089900120927La data inizio deve essere maggiore della data fine                            15
090000120927                                                                               16
090100120927                                                                               17
090200120927                                                                               18
090300120927                                                                               19
090400120927                                                                               20
090500120927Scelta obbligatoria                                                            21
090600120927                                                                               22
090700121024Errore nella sfleggatura                                                       23
090800121024Funzione di sfleggatura riuscita                                               24
090900970526** FUD - FUNZIONI
091000970526                   -        (S/N)=N                        funz.01¬
091100970526                   -        (S/N)=N                        funz.02¬
091200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
091300970526                   -        (S/N)=N                        funz.04¬
091400970526                   -        (S/N)=N                        funz.05¬
091500991214F06=Conferma       -        (S/N)=S                        funz.06¬
091600121024F07=Sflegga dati   -        (S/N)=S                        funz.07¬
091700970527                   -        (S/N)=N                        funz.08¬
091800970527                   -        (S/N)=N                        funz.09¬
091900970527                   -        (S/N)=N                        funz.10¬
092000970526                   -        (S/N)=N                        funz.11¬
092100970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
092200970526                   -        (S/N)=N                        funz.13¬
092300970526                   -        (S/N)=N                        funz.14¬
092400970526                   -        (S/N)=N                        funz.15¬
092500970526                   -        (S/N)=N                        funz.16¬
092600970526                   -        (S/N)=N                        funz.17¬
092700970526                   -        (S/N)=N                        funz.18¬
092800970526                   -        (S/N)=N                        funz.19¬
092900970526                   -        (S/N)=N                        funz.20¬
093000970526                   -        (S/N)=N                        funz.21¬
093100970526                   -        (S/N)=N                        funz.22¬
093200970526                   -        (S/N)=N                        funz.23¬
093300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
