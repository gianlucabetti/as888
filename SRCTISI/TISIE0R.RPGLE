000100000000     H*------------------------------------------------------------------------*
000200000104     H* IMPORTAZIONE CAPPARIO DPD
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800060426     FTISIE0D   CF   E             WORKSTN INFDS(DEVDS1)
000900060426     F                                     SFILE(SIE0DMS:MSGULT)
001000060428     FDPCVE01L  UF A E           K DISK    USROPN
001100060426     FDPCVE02L  IF   E           K DISK    USROPN
001200060426     F                                     RENAME(dpcve000:dpcve2)
001300060427     FWFDPCALF  IF   E           K DISK
001400060427     FWFDPCCCF  IF   E           K DISK
001500060427     FWFDPCCUF  IF   E           K DISK
001600060427     FWFDPCDPF  IF   E           K DISK
001700060427     FWFDPCDYF  IF   E           K DISK
001800060427     FWFDPCROF  IF   E           K DISK
001900060427     FWFDPCSCF  IF   E           K DISK
002000000000     D*------------------------------------------------------------------------*
002100000000     D* SCHIERE
002200000000     D*------------------------------------------------------------------------*
002300060427     D predde          s              8  0 dim(200)
002400060427     D predsc          s              8  0 dim(200)
002500000000     D*-------------------
002600000000     D* MESSAGGI
002700000000     D*-------------------
002800000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002900000000     D*-------------------
003000000000     D* DESCRIZIONI TASTI FUNZIONALI
003100000000     D*-------------------
003200000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003300000000     D*-------------------
003400000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
003500000000     D*-------------------
003600000000     D FUV             S             19    DIM(24)
003700000000     D*------------------------------------------------------------------------*
003800000000     D* INPUT
003900000000     D*------------------------------------------------------------------------*
004000000000     D*-------------------
004100000000     D* RIDEFINIZIONE SCHIERA -FUD-
004200000000     D*-------------------
004300000000     D FUDDS           DS
004400000000     D*                -DESCRIZIONE PARZIALE
004500000000     D  FUDDE1                 1      1
004600000000     D*                -FUNZIONE
004700000000     D  FUDFUN                 2      3
004800000000     D*                -DESCRIZIONE PARZIALE
004900000000     D  FUDDE2                 4     19
005000000000     D*                -FILLER
005100000000     D  FUDFI0                20     34
005200000000     D*                -FLAG FUNZ.ATTIVA (S/N)
005300000000     D  FUDATT                35     35
005400000000     D*                -FILLER
005500000000     D  FUDFI1                36     80
005600000000     D*                -DESCRIZIONE COMPLETA
005700000000     D  FUDDES                 1     19
005800000000     D*                -RIDEFINIZIONE RIGA
005900000000     D  FUDRIG                 1     80
006000000000     D*-------------------
006100000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
006200000000     D*-------------------
006300000000     D DSMSG         E DS
006400000000     D*-------------------
006500020919     D* DS REPERIMENTO DATI UTENTE
006600020919     D*------------
006700020919     D TIBS34DS      E DS                                                       *Profili utente
006800020919     D DDATIUTE      E DS                                                       *Dati utente
006900020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007000000104     D*--------------------
007100000104     D* DS "XSRDAT" - CONTROLLO DATA (8)
007200000104     D*--------------------
007300020802     DWLBDA8           DS                  INZ
007400000104     D  G08DAT                 1      8  0
007500000104     D  G08INV                 9     16  0
007600000104     D  G08ERR                17     17
007700000104     D  G08TGI                18     22  0
007800970401     D*----------------
007900000000     D* DS PARAMETRI DI LANCIO PGM
008000000000     D*-------------------
008100060427     D PARAM           DS                  INZ
008200060608     D  PARVER                        5  0
008300060608     D  PARICCC                       1
008400060608     D  PARICAL                       1
008500060608     D  PARICDY                       1
008600060608     D  PARICDP                       1
008700060608     D  PARICSC                       1
008800060608     D  PARICRO                       1
008900060608     D  PARICLO                       1
009000060608     D  PARICCU                       1
009100080424     D  PARVERDDE                     8  0
009200080424     D  PARSTRBOL                     8  0
009300000000     D*-------------------
009400000000     D* DS PER GESTIONE TASTI FUNZIONALI
009500000000     D*-------------------
009600000000     D DSKEY         E DS
009700000000     D DEVDS1          DS
009800000000     D  KEY                  369    369
009900000000     D*-------------------
010000000000     D* ARCHITETTURA
010100000000     D*-------------------
010200000000     D KPJBA         E DS
010300060426     D*-------------------
010400060426     D* VARIABILI DI WRK
010500060426     D*-------------------
010600060426     D wVerAllow       s              8    inz
010700060426     D wVerCountry     s              8    inz
010800060426     D wVerCustoms     s              8    inz
010900060426     D wVerDeny        s              8    inz
011000060426     D wVerDepots      s              8    inz
011100060426     D wVerRoutes      s              8    inz
011200060426     D wVerService     s              8    inz
011300060426     D wExpAllow       s              8    inz
011400060426     D wExpCountry     s              8    inz
011500060426     D wExpCustoms     s              8    inz
011600060426     D wExpDeny        s              8    inz
011700060426     D wExpDepots      s              8    inz
011800060426     D wExpRoutes      s              8    inz
011900060426     D wExpService     s              8    inz
012000120625     D wUpdRoutes      s              8    inz
012100000000     C*------------------------------------------------------------------------*
012200000000     C* MAIN LINE
012300000000     C*------------------------------------------------------------------------*
012400000000     C*
012500000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
012600000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
012700000000     C*
012800000000     C* CICLO FINO A CHE:
012900000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
013000000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
013100000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
013200000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
013300000000     C     F12L00        OREQ      '1'
013400000000     C     FLFINE        OREQ      '1'
013500060428     C*
013600010418     C* PROME CONSIDERAZIONI
013700010418     C                   EXSR      EXEINZ
013800000000     C*
013900000000     C* EMETTE FORMATO VIDEO
014000000000     C                   EXSR      BUTFMR
014100000000     C*
014200000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
014300000000     C                   EXSR      FUNCON
014400000000     C*
014500060428     C                   ENDDO                                                  <-- 2 --
014600000000     C*
014700060428     C                   ENDIF                                                  <-- 1 --
014800000000     C*
014900000000     C                   SETON                                        LR
015000010418     C*------------------------------------------------------------------------*
015100010418     C* PRIME CONSIDERAZIONI
015200010418     C*------------------------------------------------------------------------*
015300010418     C     EXEINZ        BEGSR
015400060608     C*
015500060608     C* APRO IL FILE VERSIONI
015600060608     C                   IF        not %open(DPCVE01L)
015700060608     C                   OPEN      DPCVE01L
015800060608     C                   ENDIF
015900010418     C*
016000010418     C* REPERISCO L'ULTIMA VERSIONE DEL CAPPARIO DPD X PROPORRE LA NUOVA A VIDEO
016100060426     C     *HIVAL        SETGT     DPCVE01L
016200060426     C                   READP(e)  DPCVE01L
016300060426     C                   IF        not %eof(DPCVE01L)
016400060426     C                   EVAL      VIDVER = CVEVER + 1
016500010418     C                   ENDIF
016600060608     C*
016700060608     C* CHIUDO IL FILE VERSIONI
016800060608     C                   IF        %open(DPCVE01L)
016900060608     C                   CLOSE     DPCVE01L
017000060608     C                   ENDIF
017100010418     C*
017200010418     C                   ENDSR
017300000000     C*------------------------------------------------------------------------*
017400000000     C* EMISSIONE FORMATO A VIDEO
017500000000     C*------------------------------------------------------------------------*
017600000000     C     BUTFMR        BEGSR
017700000000     C*
017800000000     C* WRITE RECORD TESTATA
017900060426     C                   WRITE     SIE0D01
018000000000     C*
018100000000     C* VISUALIZZAZIONE MESSAGGIO
018200000000     C                   EXSR      SNDMSG
018300000000     C*
018400000000     C* WRITE RECORD FUNZIONI
018500060426     C                   WRITE     SIE0D02
018600991019     C*
018700991019     C* EXFMT RECORD DETTAGLIO
018800060426     C                   EXFMT     SIE0D03
018900991019     C*
019000000000     C* AZZERO FLAG DI ERRORE LIVELLO
019100000000     C                   MOVEL     '0'           ERRL00
019200000000     C*
019300000000     C* AZZERO FLAG DI ERRORE
019400060427     C                   SETOFF                                       707172
019500060427     C                   SETOFF                                       7380
019600000000     C*
019700000000     C                   ENDSR
019800000000     C*------------------------------------------------------------------------*
019900000000     C* SNDMSG - MANDA MESSAGGIO
020000000000     C*------------------------------------------------------------------------*
020100000000     C     SNDMSG        BEGSR
020200000000     C*
020300000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
020400000000     C                   MOVEL     '1'           *IN40
020500060426     C                   WRITE     SIE0DMC
020600000000     C                   MOVEL     '0'           *IN40
020700000000     C*
020800000000     C* VISUALIZZAZIONE MESSAGGIO
020900000000     C                   Z-ADD     0             MSGULT            4 0
021000000000     C                   CALL      'XMSG'        XMSGPA
021100000000     C     DSMSER        IFEQ      0
021200000000     C                   MOVEL     DSMSKE        MSGKEY
021300000000     C                   END
021400000000     C                   Z-ADD     1             MSGULT
021500060426     C                   WRITE     SIE0DMS                                      SFL MESS
021600060426     C                   WRITE     SIE0DMC                                      CONTR MESS
021700000000     C*
021800000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
021900000000     C                   MOVEL     *BLANKS       DSMSMS
022000000000     C*
022100000000     C                   ENDSR
022200000000     C*------------------------------------------------------------------------*
022300000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
022400000000     C*------------------------------------------------------------------------*
022500000000     C     ENTGES        BEGSR
022600000000     C*
022700000000     C* CONTROLLA I CAMPI IMMESSI
022800000000     C                   EXSR      DECODI
022900000000     C*
023000000000     C                   ENDSR
023100000000     C*------------------------------------------------------------------------*
023200000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
023300000000     C*------------------------------------------------------------------------*
023400000000     C     F03GES        BEGSR
023500060428     C*
023600060428     C* CHIUDO IL FILE VERSIONI
023700060608     C                   IF        %open(DPCVE01L)
023800060428     C                   CLOSE     DPCVE01L
023900060608     C                   ENDIF
024000000000     C*
024100000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
024200000000     C                   MOVEL     '1'           F03L00
024300000000     C*
024400000000     C                   ENDSR
024500000000     C*------------------------------------------------------------------------*
024600000000     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
024700000000     C*------------------------------------------------------------------------*
024800991020     C     F06GES        BEGSR
024900991105     C*
025000991105     C                   EXSR      DECODI
025100060427     C*
025200991105     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
025300991105     C     ERRL00        IFEQ      '0'
025400060608     C*
025500060608     C                   EXSR      F10GES
025600000104     C*
025700060426     C* INSERISCO LA VERSIONE NEL FILE DPCVE01L
025800000104     C     *IN80         IFEQ      *OFF
025900060427     C                   EXSR      INSVER
026000060608     C*
026100060608     C* CHIUDO IL FILE VERSIONI
026200060608     C                   IF        %open(DPCVE01L)
026300060608     C                   CLOSE     DPCVE01L
026400060608     C                   ENDIF
026500060427     C*
026600060427     C* SOTTOMETTO IL PROGRAMMA DI ELABORAZIONE ARCHIVI IMPORTATI
026700060427     C                   MOVEL     'SIE1'        KCOAZ
026800060427     C                   Z-ADD     VIDVER        PARVER
026900060608     C                   MOVEL     VIDICCC       PARICCC
027000060608     C                   MOVEL     VIDICAL       PARICAL
027100060608     C                   MOVEL     VIDICDY       PARICDY
027200060608     C                   MOVEL     VIDICDP       PARICDP
027300060608     C                   MOVEL     VIDICSC       PARICSC
027400060608     C                   MOVEL     VIDICRO       PARICRO
027500060608     C                   MOVEL     VIDICLO       PARICLO
027600060608     C                   MOVEL     VIDICCU       PARICCU
027700080424     C                   Z-ADD     DATADEC       PARVERDDE
027800080424     C                   EVAL      PARSTRBOL = predde(k-1)
027900060427     C                   MOVEL     PARAM         KPJBU
028000080325     C***                CALL      'BCH10'
028100080325 MOM C                   CALL      'TISIE1R'
028200060427     C                   PARM                    KPJBA
028300000104     C                   ENDIF
028400991105     C*
028500991105     C                   ENDIF
028600000000     C*
028700000000     C                   ENDSR
028800060608     C*------------------------------------------------------------------------*
028900060608     C* F10GES -GESTIONE TASTO FUNZIONALE F10 - EXE CLRPFM
029000060608     C*------------------------------------------------------------------------*
029100060608     C     F10GES        BEGSR
029200060608     C*
029300060608     C                   MOVEL     *blanks       command          80
029400060608     C                   Z-ADD     80            lenght           15 5
029500060608     C*
029600060608     C                   IF        VIDCCVE = 'X'
029700060608     C                   EVAL      command = 'CLRPFM  FILE(DPCVE00F)'
029800060608     C                   CALL      'QCMDEXC'
029900060608     C                   PARM                    command
030000060608     C                   PARM                    lenght
030100060608     C                   ENDIF
030200060608     C*
030300060608     C                   IF        VIDCCCC = 'X'
030400060608     C                   EVAL      command = 'CLRPFM  FILE(DPCCC00F)'
030500060608     C                   CALL      'QCMDEXC'
030600060608     C                   PARM                    command
030700060608     C                   PARM                    lenght
030800060608     C                   ENDIF
030900060608     C*
031000060608     C                   IF        VIDCCAL = 'X'
031100060608     C                   EVAL      command = 'CLRPFM  FILE(DPCAL00F)'
031200060608     C                   CALL      'QCMDEXC'
031300060608     C                   PARM                    command
031400060608     C                   PARM                    lenght
031500060608     C                   ENDIF
031600060608     C*
031700060608     C                   IF        VIDCCDY = 'X'
031800060608     C                   EVAL      command = 'CLRPFM  FILE(DPCDY00F)'
031900060608     C                   CALL      'QCMDEXC'
032000060608     C                   PARM                    command
032100060608     C                   PARM                    lenght
032200060608     C                   ENDIF
032300060608     C*
032400060608     C                   IF        VIDCCDP = 'X'
032500060608     C                   EVAL      command = 'CLRPFM  FILE(DPCDP00F)'
032600060608     C                   CALL      'QCMDEXC'
032700060608     C                   PARM                    command
032800060608     C                   PARM                    lenght
032900060608     C                   ENDIF
033000060608     C*
033100060608     C                   IF        VIDCCSC = 'X'
033200060608     C                   EVAL      command = 'CLRPFM  FILE(DPCSC00F)'
033300060608     C                   CALL      'QCMDEXC'
033400060608     C                   PARM                    command
033500060608     C                   PARM                    lenght
033600060608     C                   ENDIF
033700060608     C*
033800060608     C                   IF        VIDCCRO = 'X'
033900060608     C                   EVAL      command = 'CLRPFM  FILE(DPCRO00F)'
034000060608     C                   CALL      'QCMDEXC'
034100060608     C                   PARM                    command
034200060608     C                   PARM                    lenght
034300060608     C                   ENDIF
034400060608     C*
034500060608     C                   IF        VIDCCLO = 'X'
034600060608     C                   EVAL      command = 'CLRPFM  FILE(DPCLO00F)'
034700060608     C                   CALL      'QCMDEXC'
034800060608     C                   PARM                    command
034900060608     C                   PARM                    lenght
035000060608     C                   ENDIF
035100060608     C*
035200060608     C                   IF        VIDCCCU = 'X'
035300060608     C                   EVAL      command = 'CLRPFM  FILE(DPCCU00F)'
035400060608     C                   CALL      'QCMDEXC'
035500060608     C                   PARM                    command
035600060608     C                   PARM                    lenght
035700060608     C                   ENDIF
035800060608     C*
035900060608     C                   EXSR      ENTGES
036000060608     C*
036100060608     C                   ENDSR
036200000000     C*------------------------------------------------------------------------*
036300000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
036400000000     C*------------------------------------------------------------------------*
036500000000     C     F12GES        BEGSR
036600060428     C*
036700060428     C* CHIUDO IL FILE VERSIONI
036800060608     C                   IF        %open(DPCVE01L)
036900060428     C                   CLOSE     DPCVE01L
037000060608     C                   ENDIF
037100000000     C*
037200000000     C* IMPOSTO FLAG DI LIVELLO
037300991103     C                   MOVEL     '1'           F12L00
037400000000     C*
037500000000     C                   ENDSR
037600000000     C*------------------------------------------------------------------------*
037700991020     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
037800000000     C*------------------------------------------------------------------------*
037900000000     C     F24GES        BEGSR
038000000000     C*
038100000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
038200000000     C                   EXSR      FUNGES
038300000000     C*
038400000000     C                   ENDSR
038500991020     C*------------------------------------------------------------------------*
038600000104     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
038700000000     C*------------------------------------------------------------------------*
038800000000     C     DECODI        BEGSR
038900060426     C*
039000060426     C* CONTROLLO CONGRUENZA VERSIONI E SCADENZA TUTTI FILE DA ACQUISIRE
039100060426     C     ERRL00        IFEQ      '0'
039200060426     C                   EXSR      REPVERALW
039300060426     C                   EXSR      REPVERCOU
039400060426     C                   EXSR      REPVERCUS
039500060426     C                   EXSR      REPVERDEN
039600060426     C                   EXSR      REPVERDEP
039700060426     C                   EXSR      REPVERROU
039800060426     C                   EXSR      REPVERSER
039900060426     C* Se nn sono TUTTE identiche segnala errore
040000060426     C                   MOVEL     'N'           wFileOk           1
040100060426     C                   IF        wVerAllow   = wVerCountry AND
040200060426     C                             wVerCountry = wVerCustoms AND
040300060426     C                             wVerCustoms = wVerDeny    AND
040400060426     C                             wVerDeny    = wVerDepots  AND
040500060426     C                             wVerDepots  = wVerRoutes  AND
040600060426     C                             wVerRoutes  = wVerService
040700060426     C                   IF        wExpAllow   = wExpCountry AND
040800060426     C                             wExpCountry = wExpCustoms AND
040900060426     C                             wExpCustoms = wExpDeny    AND
041000060426     C                             wExpDeny    = wExpDepots  AND
041100060426     C                             wExpDepots  = wExpRoutes  AND
041200060426     C                             wExpRoutes  = wExpService
041300060426     C                   MOVEL     'S'           wFileOk
041400060426     C                   ENDIF
041500060426     C                   ENDIF
041600060426     C* Se qualcosa "nn torna" emetto errore a video
041700060427     C                   IF        wFileOk <> 'S'
041800060426     C                   SETON                                        70
041900060426     C                   MOVEL     '1'           ERRL00
042000060426     C                   MOVEL     MSG(8)        DSMSMS
042100060427     C                   ENDIF
042200060426     C                   ENDIF
042300060426     C*
042400060426     C* CONTROLLO VERSIONE CAPPARIO
042500060426     C     ERRL00        IFEQ      '0'
042600060426     C                   EXSR      CHKVER
042700060426     C                   ENDIF
042800000000     C*
042900060426     C* I CONTROLLI SEGUENTI SONO SUBORDINATI ALL'INSERIMENTO IN "VERSIONI"
043000000104     C     *IN80         IFEQ      *OFF
043100000104     C*
043200000104     C* CONTROLLO DATA DECORRENZA
043300991020     C     ERRL00        IFEQ      '0'
043400010418     C                   Z-ADD     VIDDAI        G08DAT
043500000105     C                   Z-ADD     *ZEROS        G08INV
043600000105     C                   MOVEL     '0'           G08ERR
043700000104     C                   CALL      'XSRDA8'
043800000104     C                   PARM                    WLBDA8
043900010418     C                   Z-ADD     G08DAT        VIDDAI
044000000104     C     G08ERR        IFEQ      '1'
044100000104     C                   MOVEL     '1'           ERRL00
044200000104     C                   MOVEL     MSG(3)        DSMSMS
044300000104     C                   SETON                                        71
044400000107     C                   ELSE
044500000107     C                   Z-ADD     G08INV        DATADEC           8 0
044600000104     C                   ENDIF
044700000104     C                   ENDIF
044800000104     C*
044900000104     C* CONTROLLO DATA SCADENZA
045000000104     C     ERRL00        IFEQ      '0'
045100010418     C                   Z-ADD     VIDDAF        G08DAT
045200000105     C                   Z-ADD     *ZEROS        G08INV
045300000105     C                   MOVEL     '0'           G08ERR
045400000104     C                   CALL      'XSRDA8'
045500000104     C                   PARM                    WLBDA8
045600010418     C                   Z-ADD     G08DAT        VIDDAF
045700000104     C     G08ERR        IFEQ      '1'
045800000104     C                   MOVEL     '1'           ERRL00
045900000104     C                   MOVEL     MSG(3)        DSMSMS
046000000104     C                   SETON                                        72
046100000107     C                   ELSE
046200000107     C                   Z-ADD     G08INV        DATASCA           8 0
046300000104     C                   ENDIF
046400000104     C                   ENDIF
046500000112     C*
046600000112     C* CONTROLLI X CORRETTA SERIALIZZAZIONE VERSIONI
046700000112     C     ERRL00        IFEQ      '0'
046800000112     C                   EXSR      CHKSRL
046900000112     C                   ENDIF
047000000112     C*
047100000104     C                   ENDIF
047200000104     C*
047300000000     C                   ENDSR
047400000112     C*------------------------------------------------------------------------*
047500000112     C*  CHKSRL - CONTROLLA LE DATE DECORRENZA/SCADENZA VERS. X CORRETTA SERIAL.
047600000112     C*------------------------------------------------------------------------*
047700000112     C     CHKSRL        BEGSR
047800060622     C*
047900060622     C                   SETOFF                                       1112
048000000112     C*
048100000113     C                   MOVEL     '0'           PROCEDI           1
048200000112     C                   Z-ADD     1             k                 4 0
048300000112     C* APRO IL FILE LOGICO
048400060608     C                   IF        not %open(DPCVE02L)
048500060426     C                   OPEN      DPCVE02L
048600060608     C                   ENDIF
048700060426     C* MI POSIZIONO ALL'INIZIO PER SCORRERLO TUTTO IN ORDINE DI "DATA DECORRENZA"
048800060426     C     *loval        SETLL     DPCVE02L                           97
048900000112     C     *IN97         IFEQ      *OFF
049000060426     C                   READ      DPCVE02L                               97
049100000112     C     *IN97         DOWEQ     *OFF
049200000112     C* CARICO IN SCHIERA LE DATE DECORRENZE/SCADENZA
049300060426     C                   Z-ADD     CVEDDE        predde(k)
049400060426     C                   Z-ADD     CVEDSC        predsc(k)
049500000112     C                   ADD       1             k
049600060426     C                   READ      DPCVE02L                               97
049700000112     C                   ENDDO
049800000112     C                   ELSE
049900000112     C* VI E' UN ERRORE SUL FILE VERSIONI
050000000113     C                   MOVEL     '1'           PROCEDI
050100000113     C                   MOVEL     MSG(2)        DSMSMS
050200000112     C                   ENDIF
050300000112     C*
050400000112     C     ERRL00        IFNE      '1'
050500000113     C     PROCEDI       ANDNE     '1'
050600000112     C*
050700000112     C* INIZIO LE VERIFICHE COI SEGUENTI CRITERI
050800000112     C* 1) "DATA DA" deve essere: > "data scadenza" precedente    AND
050900000112     C*                           < "data decorrenza" successiva
051000000112     C*
051100000112     C* se alla fine del ciclo di lettura il test #1 non è verificato
051200000112     C* "DATA DA"    deve essere:
051300000112     C* 2)                        > "data scadenza" ultima letta
051400000112     C*
051500000112     C* se #1 e #2 non verificato => ERRORE,
051600000112     C*                altrimenti => se verificato #1, segue il #3
051700000112     C*                altrimenti => se verificato #2, segue il #4
051800000112     C*
051900000112     C* 3) "DATA A"  deve essere: > "DATA DA"                     AND
052000000112     C*                           < "data decorrenza" successiva
052100000112     C*
052200000112     C* 4) "DATA A"  deve essere: > "DATA DA"
052300000112     C*
052400000112     C                   Z-ADD     1             p                 4 0
052500000112     C                   Z-ADD     *zeros        d                 4 0
052600060622     C     p             DOWLT     k
052700000112     C                   EVAL      d=(p+1)
052800000112     C* TESTO ENTRAMBE LE DATE "DA"
052900000112     C     predde(p)     IFNE      *zeros
053000000112     C     predde(d)     IFNE      *zeros
053100000112     C     DATADEC       IFGT      predsc(p)
053200000112     C     DATADEC       ANDLT     predde(d)
053300000112     C                   SETON                                        11
053400000112     C                   Z-ADD     predde(p)     savdde            8 0
053500000112     C                   Z-ADD     predsc(d)     savdsc            8 0
053600000112     C                   LEAVE
053700000112     C                   ENDIF
053800000112     C                   ELSE
053900000112     C     DATADEC       IFGT      predsc(p)
054000000112     C                   SETON                                        12
054100000112     C                   Z-ADD     predde(p)     savdde
054200000112     C                   Z-ADD     predsc(p)     savdsc
054300000112     C                   LEAVE
054400000112     C                   ENDIF
054500000112     C                   ENDIF
054600000112     C                   ELSE
054700000112     C                   LEAVE
054800000112     C                   ENDIF
054900000112     C                   ADD       1             p
055000000112     C                   ENDDO
055100000112     C*
055200000112     C     *IN11         IFEQ      *OFF
055300000112     C     *IN12         ANDEQ     *OFF
055400000112     C* LA DATA DECORENZEA NON E' COMPATIBILE CON QUELLE GIA' PRESENTI
055500000112     C                   MOVEL     '1'           ERRL00
055600000112     C                   MOVEL     MSG(6)        DSMSMS
055700000112     C                   ELSE
055800000112     C     *IN11         IFEQ      *ON
055900000112     C     DATASCA       IFGT      DATADEC
056000000112     C     DATASCA       ANDLT     savdde
056100000112     C                   SETON                                        13
056200000112     C                   ELSE
056300000112     C                   MOVEL     '1'           ERRL00
056400000112     C                   MOVEL     MSG(7)        DSMSMS
056500000112     C                   ENDIF
056600000112     C                   ELSE
056700000112     C     *IN12         IFEQ      *ON
056800000112     C     DATASCA       IFGT      DATADEC
056900000112     C                   SETON                                        14
057000000112     C                   ELSE
057100000112     C                   MOVEL     '1'           ERRL00
057200000112     C                   MOVEL     MSG(4)        DSMSMS
057300000112     C                   ENDIF
057400000112     C                   ENDIF
057500000112     C                   ENDIF
057600000112     C                   ENDIF
057700000112     C*
057800000112     C                   ENDIF
057900000112     C*
058000000112     C* CHIUDO IL FILE LOGICO
058100060608     C                   IF        %open(DPCVE02L)
058200060426     C                   CLOSE     DPCVE02L
058300060608     C                   ENDIF
058400000112     C*
058500000112     C                   ENDSR
058600000104     C*------------------------------------------------------------------------*
058700000104     C*  CHKVER - CONTROLLA PRESENZA O MENO DELLA VERSIONE DIGITATA (IN INPUT)
058800000104     C*------------------------------------------------------------------------*
058900000104     C     CHKVER        BEGSR
059000000104     C*
059100000105     C     VIDVER        IFEQ      *ZEROS
059200000105     C                   MOVEL     '1'           ERRL00
059300000105     C                   ELSE
059400060608     C*
059500060608     C* APRO IL FILE VERSIONI
059600060608     C                   IF        not %open(DPCVE01L)
059700060608     C                   OPEN      DPCVE01L
059800060608     C                   ENDIF
059900000105     C*
060000060426     C     VIDVER        CHAIN     DPCVE01L                           99
060100000104     C     *IN99         IFEQ      *ON
060200000104     C                   EXSR      REPVER
060300000104     C                   SETOFF                                       80
060400000104     C                   MOVEL     MSG(2)        DSMSMS
060500000104     C                   ELSE
060600000104     C                   SETON                                        80
060700000104     C                   ENDIF
060800060608     C*
060900060608     C* CHIUDO IL FILE LOGICO
061000060608     C                   IF        %open(DPCVE01L)
061100060608     C                   CLOSE     DPCVE01L
061200060608     C                   ENDIF
061300000104     C*
061400000105     C                   ENDIF
061500000105     C*
061600000104     C                   ENDSR
061700000104     C*------------------------------------------------------------------------*
061800000104     C*  INSVER - INSERISCE LA VERSIONE DIGITATA (IN INPUT)
061900000104     C*------------------------------------------------------------------------*
062000060608     C     INSVER        BEGSR
062100060608     C*
062200060608     C* APRO IL FILE VERSIONI
062300060608     C                   IF        not %open(DPCVE01L)
062400060608     C                   OPEN      DPCVE01L
062500060608     C                   ENDIF
062600000104     C*
062700060426     C* RIEMPIO BUFFER RECORD DI DPCVE01L
062800060426     C                   Z-ADD     VIDVER        CVEVER
062900060426     C                   Z-ADD     DATADEC       CVEDDE
063000060426     C                   Z-ADD     DATASCA       CVEDSC
063100060426     C                   MOVEL     VIDDES        CVEDES
063200060428     C                   EVAL      CVEVERD = %subst(wVerRoutes:5:2) + '/' +
063300060428     C                                       %subst(wVerRoutes:1:4)
063400060426     C                   MOVEL     *BLANKS       CVEATB
063500000104     C*
063600000104     C* SCRITTURA RECORD
063700060426     C                   WRITE     DPCVE000
063800060608     C*
063900060608     C* CHIUDO IL FILE LOGICO
064000060608     C                   IF        %open(DPCVE01L)
064100060608     C                   CLOSE     DPCVE01L
064200060608     C                   ENDIF
064300000104     C*
064400000104     C                   ENDSR
064500000104     C*------------------------------------------------------------------------*
064600060426     C*  REPVER - REPERISCE DATI RELATIVI A VERSIONE E SCADENZA ARCHIVI IMPORTATI
064700000104     C*------------------------------------------------------------------------*
064800060426     C     REPVER        BEGSR
064900000104     C*
065000130225     C                   EVAL      VIDDES =  'Ver:' + wVerRoutes +
065100130225     C                                       ' Fr:' + wUpdRoutes +
065200130225     C                                       ' To:' + wExpRoutes
065300000104     C*
065400000104     C                   ENDSR
065500060426     C*------------------------------------------------------------------------*
065600060426     C*  REPVERALW - REPERISCE DATI DI INTESTAZIONE ARCHIVIO "ALLOW"
065700060426     C*------------------------------------------------------------------------*
065800060426     C     REPVERALW     BEGSR
065900060426     C*
066000060426     C* Scorro il file fino a reperire le 2 keyword relative a Versione e Scadenza
066100060426     C     *START        SETLL     WFDPCALF
066200060426     C                   READ      WFDPCALF
066300060426     C                   DOW       not %eof(WFDPCALF)
066400060426     C                   IF        %subst(%trim(WDCALDTA):1:8) = '#Version'
066500060426     C                   EVAL      wVerAllow = %subst(%trim(WDCALDTA):11:8)
066600060426     C                   ENDIF
066700060426     C                   IF        %subst(%trim(WDCALDTA):1:11) = '#Expiration'
066800060426     C                   EVAL      wExpAllow = %subst(%trim(WDCALDTA):14:8)
066900060426     C                   ENDIF
067000060426     C                   IF        wVerAllow<>*blanks AND wExpAllow<>*blanks
067100060426     C                   LEAVE
067200060426     C                   ENDIF
067300060426     C                   READ      WFDPCALF
067400060426     C                   ENDDO
067500060426     C*
067600060426     C                   ENDSR
067700060426     C*------------------------------------------------------------------------*
067800060426     C*  REPVERCOU - REPERISCE DATI DI INTESTAZIONE ARCHIVIO "COUNTRY"
067900060426     C*------------------------------------------------------------------------*
068000060426     C     REPVERCOU     BEGSR
068100060426     C*
068200060426     C* Scorro il file fino a reperire le 2 keyword relative a Versione e Scadenza
068300060426     C     *START        SETLL     WFDPCCCF
068400060426     C                   READ      WFDPCCCF
068500060426     C                   DOW       not %eof(WFDPCCCF)
068600060426     C                   IF        %subst(%trim(WDCCCDTA):1:8) = '#Version'
068700060426     C                   EVAL      wVerCountry = %subst(%trim(WDCCCDTA):11:8)
068800060426     C                   ENDIF
068900060426     C                   IF        %subst(%trim(WDCCCDTA):1:11) = '#Expiration'
069000060426     C                   EVAL      wExpCountry = %subst(%trim(WDCCCDTA):14:8)
069100060426     C                   ENDIF
069200060426     C                   IF        wVerCountry<>*blanks AND wExpCountry<>*blanks
069300060426     C                   LEAVE
069400060426     C                   ENDIF
069500060426     C                   READ      WFDPCCCF
069600060426     C                   ENDDO
069700060426     C*
069800060426     C                   ENDSR
069900060426     C*------------------------------------------------------------------------*
070000060426     C*  REPVERCUS - REPERISCE DATI DI INTESTAZIONE ARCHIVIO "CUSTOMS"
070100060426     C*------------------------------------------------------------------------*
070200060426     C     REPVERCUS     BEGSR
070300060426     C*
070400060426     C* Scorro il file fino a reperire le 2 keyword relative a Versione e Scadenza
070500060426     C     *START        SETLL     WFDPCCUF
070600060426     C                   READ      WFDPCCUF
070700060426     C                   DOW       not %eof(WFDPCCUF)
070800060426     C                   IF        %subst(%trim(WDCCUDTA):1:8) = '#Version'
070900060426     C                   EVAL      wVerCustoms = %subst(%trim(WDCCUDTA):11:8)
071000060426     C                   ENDIF
071100060426     C                   IF        %subst(%trim(WDCCUDTA):1:11) = '#Expiration'
071200060426     C                   EVAL      wExpCustoms = %subst(%trim(WDCCUDTA):14:8)
071300060426     C                   ENDIF
071400060426     C                   IF        wVerCustoms<>*blanks AND wExpCustoms<>*blanks
071500060426     C                   LEAVE
071600060426     C                   ENDIF
071700060426     C                   READ      WFDPCCUF
071800060426     C                   ENDDO
071900060426     C*
072000060426     C                   ENDSR
072100060426     C*------------------------------------------------------------------------*
072200060426     C*  REPVERDEN - REPERISCE DATI DI INTESTAZIONE ARCHIVIO "DENY"
072300060426     C*------------------------------------------------------------------------*
072400060426     C     REPVERDEN     BEGSR
072500060426     C*
072600060426     C* Scorro il file fino a reperire le 2 keyword relative a Versione e Scadenza
072700060426     C     *START        SETLL     WFDPCDYF
072800060426     C                   READ      WFDPCDYF
072900060426     C                   DOW       not %eof(WFDPCDYF)
073000060426     C                   IF        %subst(%trim(WDCDYDTA):1:8) = '#Version'
073100060426     C                   EVAL      wVerDeny   = %subst(%trim(WDCDYDTA):11:8)
073200060426     C                   ENDIF
073300060426     C                   IF        %subst(%trim(WDCDYDTA):1:11) = '#Expiration'
073400060426     C                   EVAL      wExpDeny   = %subst(%trim(WDCDYDTA):14:8)
073500060426     C                   ENDIF
073600060426     C                   IF        wVerDeny<>*blanks AND wExpDeny<>*blanks
073700060426     C                   LEAVE
073800060426     C                   ENDIF
073900060426     C                   READ      WFDPCDYF
074000060426     C                   ENDDO
074100060426     C*
074200060426     C                   ENDSR
074300060426     C*------------------------------------------------------------------------*
074400060426     C*  REPVERDEP - REPERISCE DATI DI INTESTAZIONE ARCHIVIO "DEPOTS"
074500060426     C*------------------------------------------------------------------------*
074600060426     C     REPVERDEP     BEGSR
074700060426     C*
074800060426     C* Scorro il file fino a reperire le 2 keyword relative a Versione e Scadenza
074900060426     C     *START        SETLL     WFDPCDPF
075000060426     C                   READ      WFDPCDPF
075100060426     C                   DOW       not %eof(WFDPCDPF)
075200060426     C                   IF        %subst(%trim(WDCDPDTA):1:8) = '#Version'
075300060426     C                   EVAL      wVerDepots = %subst(%trim(WDCDPDTA):11:8)
075400060426     C                   ENDIF
075500060426     C                   IF        %subst(%trim(WDCDPDTA):1:11) = '#Expiration'
075600060426     C                   EVAL      wExpDepots = %subst(%trim(WDCDPDTA):14:8)
075700060426     C                   ENDIF
075800060426     C                   IF        wVerDepots<>*blanks AND wExpDepots<>*blanks
075900060426     C                   LEAVE
076000060426     C                   ENDIF
076100060426     C                   READ      WFDPCDPF
076200060426     C                   ENDDO
076300060426     C*
076400060426     C                   ENDSR
076500060426     C*------------------------------------------------------------------------*
076600060426     C*  REPVERROU - REPERISCE DATI DI INTESTAZIONE ARCHIVIO "ROUTES"
076700060426     C*------------------------------------------------------------------------*
076800060426     C     REPVERROU     BEGSR
076900060426     C*
077000060426     C* Scorro il file fino a reperire le 2 keyword relative a Versione e Scadenza
077100060426     C     *START        SETLL     WFDPCROF
077200060426     C                   READ      WFDPCROF
077300060426     C                   DOW       not %eof(WFDPCROF)
077400060426     C                   IF        %subst(%trim(WDCRODTA):1:8) = '#Version'
077500060426     C                   EVAL      wVerRoutes = %subst(%trim(WDCRODTA):11:8)
077600060426     C                   ENDIF
077700120625     C                   IF        %subst(%trim(WDCRODTA):1:7) = '#Update'
077800120625     C                   EVAL      wUpdRoutes = %subst(%trim(WDCRODTA):10:8)
077900120625     C                   ENDIF
078000060426     C                   IF        %subst(%trim(WDCRODTA):1:11) = '#Expiration'
078100060426     C                   EVAL      wExpRoutes = %subst(%trim(WDCRODTA):14:8)
078200060426     C                   ENDIF
078300120625     C                   IF        wVerRoutes<>*blanks AND
078400120625     C                             wUpdRoutes<>*blanks AND
078500120625     C                             wExpRoutes<>*blanks
078600060426     C                   LEAVE
078700060426     C                   ENDIF
078800060426     C                   READ      WFDPCROF
078900060426     C                   ENDDO
079000060426     C*
079100060426     C                   ENDSR
079200060426     C*------------------------------------------------------------------------*
079300060426     C*  REPVERSER - REPERISCE DATI DI INTESTAZIONE ARCHIVIO "SERVICE"
079400060426     C*------------------------------------------------------------------------*
079500060426     C     REPVERSER     BEGSR
079600060426     C*
079700060426     C* Scorro il file fino a reperire le 2 keyword relative a Versione e Scadenza
079800060426     C     *START        SETLL     WFDPCSCF
079900060426     C                   READ      WFDPCSCF
080000060426     C                   DOW       not %eof(WFDPCSCF)
080100060426     C                   IF        %subst(%trim(WDCSCDTA):1:8) = '#Version'
080200060426     C                   EVAL      wVerService = %subst(%trim(WDCSCDTA):11:8)
080300060426     C                   ENDIF
080400060426     C                   IF        %subst(%trim(WDCSCDTA):1:11) = '#Expiration'
080500060427     C                   EVAL      wExpService = %subst(%trim(WDCSCDTA):14:8)
080600060426     C                   ENDIF
080700060426     C                   IF        wVerService<>*blanks AND wExpService<>*blanks
080800060426     C                   LEAVE
080900060426     C                   ENDIF
081000060426     C                   READ      WFDPCSCF
081100060426     C                   ENDDO
081200060426     C*
081300060426     C                   ENDSR
081400000000     C*------------------------------------------------------------------------*
081500000000     C*  AZZVID - AZZERA O ABBLENCA O IMPOSTA TUTTI I CAMPI DEL FILE VIDEO
081600000000     C*------------------------------------------------------------------------*
081700000000     C     AZZVID        BEGSR
081800000000     C*
081900000104     C                   Z-ADD     *ZEROS        VIDVER
082000000104     C                   Z-ADD     *ZEROS        VIDDAI
082100000104     C                   Z-ADD     *ZEROS        VIDDAF
082200000104     C                   MOVEL     *BLANKS       VIDDES
082300060608     C                   MOVEL     *BLANKS       VIDCCCC
082400060608     C                   MOVEL     *BLANKS       VIDCCAL
082500060608     C                   MOVEL     *BLANKS       VIDCCDY
082600060608     C                   MOVEL     *BLANKS       VIDCCDP
082700060608     C                   MOVEL     *BLANKS       VIDCCSC
082800060608     C                   MOVEL     *BLANKS       VIDCCRO
082900060608     C                   MOVEL     *BLANKS       VIDCCLO
083000060608     C                   MOVEL     *BLANKS       VIDCCCU
083100060608     C                   MOVEL     'X'           VIDICCC
083200060608     C                   MOVEL     'X'           VIDICAL
083300060608     C                   MOVEL     'X'           VIDICDY
083400060608     C                   MOVEL     'X'           VIDICDP
083500060608     C                   MOVEL     'X'           VIDICSC
083600060608     C                   MOVEL     'X'           VIDICRO
083700060608     C                   MOVEL     'X'           VIDICLO
083800060608     C                   MOVEL     'X'           VIDICCU
083900000000     C*
084000000104     C                   ENDSR
084100020919     C*--------------------------------------------------------------------------------------------*
084200020919     C* REPERISCE I DATI UTENTE
084300020919     C*--------------------------------------------------------------------------------------------*
084400020919     C     REPDATIUTE    BEGSR
084500020919     C*
084600020919     C* INIZIALIZZA VARIABILI DI WRK
084700020919     C                   CLEAR                   TIBS34DS
084800020919     C                   CLEAR                   AZUTEDS
084900020919     C                   CLEAR                   DDATIUTE
085000020919     C*
085100020919     C     *DTAARA       DEFINE    §azute        azuteds
085200020919     C     *DTAARA       DEFINE    §datiute      ddatiute
085300020919     C                   IN(E)     *DTAARA
085400020919if  1C                   IF        %Error
085500020919     c                   EVAL      I34Tla = 'L'
085600020919     C                   CALL      'TIBS34R'
085700020919     C                   PARM                    Tibs34Ds
085800020919     C                   IN        *DTAARA
085900020919e   1C                   ENDIF
086000020919     C*
086100020919     C                   ENDSR
086200000000     C*------------------------------------------------------------------------*
086300980403     C* *INZSR - OPERAZIONI INIZIALI
086400000000     C*------------------------------------------------------------------------*
086500980403     C     *INZSR        BEGSR
086600000000     C*
086700000000     C* RICEVIMENTO PARAMETRI
086800000000     C     *ENTRY        PLIST
086900000000     C                   PARM                    KPJBA
087000000104     C*
087100060427     C* AZZERO INIZIALE INDICATORI
087200060427     C                   SETOFF                                       707172
087300060427     C                   SETOFF                                       7380
087400000000     C*
087500000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
087600000000     C     XMSGPA        PLIST
087700000000     C                   PARM                    DSMSMS
087800000000     C                   PARM                    DSMSPG
087900000000     C                   PARM                    DSMSKE
088000000000     C                   PARM                    DSMSER
088100000000     C*
088200000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
088300000000     C                   CALL      'XKEY'
088400000000     C                   PARM                    DSKEY
088500000000     C*
088600000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
088700000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
088800000000     C*
088900000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
089000000000     C*   -NOME PGM CHIAMANTE
089100000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
089200060426     C                   MOVEL     'TISIE0R'     DSMSPG
089300060426     C                   MOVEL     'TISIE0R'     MSGQUE
089400000000     C                   MOVEL     *BLANKS       MSGKEY
089500000000     C                   MOVEL     *BLANKS       DSMSMS
089600000000     C*
089700000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
089800000000     C                   Z-ADD     0             FUNGRU            5 0
089900000000     C*
090000000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
090100000000     C                   EXSR      FUNGES
090200000000     C*
090300000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
090400000000     C                   MOVEL     '0'           F12L00            1
090500000000     C                   MOVEL     '0'           F03L00            1
090600000000     C                   MOVEL     '0'           FINL00            1
090700000000     C                   MOVEL     '0'           ERRL00            1
090800000000     C                   MOVEL     '0'           FLFINE            1
090900000000     C*
091000000000     C* LANCIO ROUTINE PER VERIFICARE SE
091100000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
091200000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
091300020919     C                   EXSR      REPDATIUTE
091400000000     C*
091500000000     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
091600000000     C                   EXSR      AZZVID
091700000000     C*
091800000000     C                   ENDSR
091900000000     C*------------------------------------------------------------------------*
092000000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
092100000000     C*------------------------------------------------------------------------*
092200000000     C     FUNGES        BEGSR
092300000000     C*
092400000000     C* PULISCO CAMPI DI LAVORO
092500000000     C                   Z-ADD     0             W                 5 0
092600000000     C*
092700000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
092800000000     C* E CARICO TABELLA FUNZIONI ATTIVE
092900000000     C     1             DO        24            I
093000000000     C                   MOVEL     FUD(I)        FUDRIG
093100000000     C*
093200000000     C* PULIZIA FUNZIONI PRECEDENTI
093300000000     C                   MOVEL     *BLANKS       FUV(I)
093400000000     C*
093500000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
093600000000     C* (SALTA F24)
093700000000     C     FUDATT        IFEQ      'S'
093800000000     C     FUDFUN        ANDNE     '24'
093900000000     C                   ADD       1             W
094000000000     C                   MOVEL     FUDDES        FUV(W)
094100000000     C                   END
094200000000     C                   END                                                    DO WHILE
094300000000     C*
094400000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
094500000000     C* ALTRIMENTI DISATTIVO F24
094600000000     C     1             DO        24            I
094700000000     C                   MOVEL     FUD(I)        FUDRIG
094800000000     C*
094900000000     C     FUDFUN        IFEQ      '24'
095000000000     C     W             IFGT      6
095100000000     C                   MOVEL     'S'           FUDATT
095200000000     C                   ELSE
095300000000     C                   MOVEL     *BLANKS       FUDATT
095400000000     C                   END                                                    W GT 8
095500000000     C                   MOVEL     FUDRIG        FUD(I)
095600000000     C                   END
095700000000     C                   END                                                    DO WHILE
095800000000     C*
095900000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
096000000000     C     W             DIV       6             I
096100000000     C                   MVR                     Z
096200000000     C*
096300000000     C     Z             IFNE      0
096400000000     C     I             ADD       1             Z
096500000000     C                   ELSE
096600000000     C                   Z-ADD     I             Z                 5 0
096700000000     C                   END
096800000000     C*
096900000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
097000000000     C                   ADD       1             FUNGRU
097100000000     C     FUNGRU        IFGT      Z
097200000000     C     FUNGRU        ORLT      1
097300000000     C                   Z-ADD     1             FUNGRU
097400000000     C                   END
097500000000     C*
097600000000     C* PRIMO GRUPPO DI 6 FUNZIONI
097700000000     C     FUNGRU        IFEQ      1
097800000000     C                   MOVEL     FUV(1)        FUN001
097900000000     C                   MOVEL     FUV(2)        FUN002
098000000000     C                   MOVEL     FUV(3)        FUN003
098100000000     C                   MOVEL     FUV(4)        FUN004
098200000000     C                   MOVEL     FUV(5)        FUN005
098300000000     C                   MOVEL     FUV(6)        FUN006
098400000000     C                   END
098500000000     C*
098600000000     C* SECONDO GRUPPO DI 6 FUNZIONI
098700000000     C     FUNGRU        IFEQ      2
098800000000     C                   MOVEL     FUV(7)        FUN001
098900000000     C                   MOVEL     FUV(8)        FUN002
099000000000     C                   MOVEL     FUV(9)        FUN003
099100000000     C                   MOVEL     FUV(10)       FUN004
099200000000     C                   MOVEL     FUV(11)       FUN005
099300000000     C                   MOVEL     FUV(12)       FUN006
099400000000     C                   END
099500000000     C*
099600000000     C* TERZO GRUPPO DI 6 FUNZIONI
099700000000     C     FUNGRU        IFEQ      3
099800000000     C                   MOVEL     FUV(13)       FUN001
099900000000     C                   MOVEL     FUV(14)       FUN002
100000000000     C                   MOVEL     FUV(15)       FUN003
100100000000     C                   MOVEL     FUV(16)       FUN004
100200000000     C                   MOVEL     FUV(17)       FUN005
100300000000     C                   MOVEL     FUV(18)       FUN006
100400000000     C                   END
100500000000     C*
100600000000     C* QUARTO GRUPPO DI 6 FUNZIONI
100700000000     C     FUNGRU        IFEQ      4
100800000000     C                   MOVEL     FUV(19)       FUN001
100900000000     C                   MOVEL     FUV(20)       FUN002
101000000000     C                   MOVEL     FUV(21)       FUN003
101100000000     C                   MOVEL     FUV(22)       FUN004
101200000000     C                   MOVEL     *BLANKS       FUN005
101300000000     C                   MOVEL     *BLANKS       FUN006
101400000000     C                   END
101500000000     C*
101600000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
101700000000     C                   MOVEL     *BLANKS       FUN007
101800000000     C                   MOVEL     *BLANKS       FUN008
101900000000     C*
102000000000     C     1             DO        24            I
102100000000     C                   MOVEL     FUD(I)        FUDRIG
102200000000     C     FUDFUN        IFEQ      '24'
102300000000     C     FUDATT        ANDEQ     'S'
102400000000     C                   MOVEL     FUDDES        FUN008
102500000000     C                   END
102600000000     C                   END                                                    DO
102700000000     C                   ENDSR
102800000000     C*------------------------------------------------------------------------*
102900000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
103000000000     C*------------------------------------------------------------------------*
103100000000     C     FUNCON        BEGSR
103200000000     C*
103300000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
103400000000     C                   EXSR      FUNVAR
103500000000     C*
103600000000     C* PULISCO CAMPI DI LAVORO
103700000000     C                   Z-ADD     0             I                 5 0
103800000000     C                   Z-ADD     0             W                 5 0
103900000000     C*
104000000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
104100000000     C     FUNKEY        IFNE      *BLANKS
104200000000     C     1             DO        24            I
104300000000     C                   MOVEL     FUD(I)        FUDRIG
104400000000     C*
104500000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
104600000000     C     FUNKEY        IFEQ      FUDFUN
104700000000     C     FUDATT        ANDEQ     'S'
104800000000     C                   Z-ADD     I             W
104900000000     C                   END
105000000000     C                   END                                                    DO WHILE
105100000000     C*
105200000000     C* FUNZIONE NON ATTIVA (ERRORE)
105300000000     C     W             IFEQ      0
105400000000     C*
105500000000     C* EMISSIONE MESSAGGIO DI ERRORE
105600000000     C                   MOVEL     MSG(1)        DSMSMS
105700000000     C*
105800000000     C                   END                                                    W=0
105900000000     C                   END                                                    FUNKEY NE *BLANKS
106000000000     C*
106100000000     C* FUNZIONE ATTIVA (PROCEDI ...)
106200000000     C     W             IFNE      0
106300000000     C     FUNKEY        OREQ      *BLANKS
106400000000     C*
106500000000     C     FUNKEY        IFEQ      *BLANKS
106600000000     C                   EXSR      ENTGES
106700000000     C                   END
106800000000     C     FUNKEY        IFEQ      '01'
106900000000     C                   END
107000000000     C     FUNKEY        IFEQ      '03'
107100000000     C                   EXSR      F03GES
107200000000     C                   END
107300000000     C     FUNKEY        IFEQ      '06'
107400000000     C                   EXSR      F06GES
107500000000     C                   END
107600000000     C     FUNKEY        IFEQ      '07'
107700000000     C                   END
107800000000     C     FUNKEY        IFEQ      '08'
107900000000     C                   END
108000060608     C     FUNKEY        IFEQ      '10'
108100060608     C                   EXSR      F10GES
108200060608     C                   END
108300000000     C     FUNKEY        IFEQ      '12'
108400000000     C                   EXSR      F12GES
108500000000     C                   END
108600000000     C     FUNKEY        IFEQ      '21'
108700000000     C                   END
108800000000     C     FUNKEY        IFEQ      '22'
108900000000     C                   END
109000000000     C     FUNKEY        IFEQ      '24'
109100000000     C                   EXSR      F24GES
109200000000     C                   END
109300000000     C     FUNKEY        IFEQ      'HP'
109400000000     C                   END
109500000000     C*
109600000000     C                   END
109700000000     C*
109800000000     C                   ENDSR
109900000000     C*------------------------------------------------------------------------*
110000000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
110100000000     C*------------------------------------------------------------------------*
110200000000     C     FUNVAR        BEGSR
110300000000     C*
110400000000     C* INIZIALIZZA VARIABILE -FUNKEY-
110500000000     C*
110600000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
110700000000     C     KEY           IFEQ      ENTER
110800000000     C                   MOVEL     *BLANKS       FUNKEY
110900000000     C                   END
111000000000     C     KEY           IFEQ      F1
111100000000     C                   MOVEL     '01'          FUNKEY
111200000000     C                   END
111300000000     C     KEY           IFEQ      F2
111400000000     C                   MOVEL     '02'          FUNKEY
111500000000     C                   END
111600000000     C     KEY           IFEQ      F3
111700000000     C                   MOVEL     '03'          FUNKEY
111800000000     C                   END
111900000000     C     KEY           IFEQ      F4
112000000000     C                   MOVEL     '04'          FUNKEY
112100000000     C                   END
112200000000     C     KEY           IFEQ      F5
112300000000     C                   MOVEL     '05'          FUNKEY
112400000000     C                   END
112500991103     C     KEY           IFEQ      F6
112600000000     C                   MOVEL     '06'          FUNKEY
112700000000     C                   END
112800000000     C     KEY           IFEQ      F7
112900000000     C                   MOVEL     '07'          FUNKEY
113000000000     C                   END
113100000000     C     KEY           IFEQ      F8
113200000000     C                   MOVEL     '08'          FUNKEY
113300000000     C                   END
113400000000     C     KEY           IFEQ      F9
113500000000     C                   MOVEL     '09'          FUNKEY
113600000000     C                   END
113700000000     C     KEY           IFEQ      F10
113800000000     C                   MOVEL     '10'          FUNKEY
113900000000     C                   END
114000000000     C     KEY           IFEQ      F11
114100000000     C                   MOVEL     '11'          FUNKEY
114200000000     C                   END
114300000000     C     KEY           IFEQ      F12
114400000000     C                   MOVEL     '12'          FUNKEY
114500000000     C                   END
114600000000     C     KEY           IFEQ      F13
114700000000     C                   MOVEL     '13'          FUNKEY
114800000000     C                   END
114900000000     C     KEY           IFEQ      F14
115000000000     C                   MOVEL     '14'          FUNKEY
115100000000     C                   END
115200000000     C     KEY           IFEQ      F15
115300000000     C                   MOVEL     '15'          FUNKEY
115400000000     C                   END
115500000000     C     KEY           IFEQ      F16
115600000000     C                   MOVEL     '16'          FUNKEY
115700000000     C                   END
115800000000     C     KEY           IFEQ      F17
115900000000     C                   MOVEL     '17'          FUNKEY
116000000000     C                   END
116100000000     C     KEY           IFEQ      F18
116200000000     C                   MOVEL     '18'          FUNKEY
116300000000     C                   END
116400000000     C     KEY           IFEQ      F19
116500000000     C                   MOVEL     '19'          FUNKEY
116600000000     C                   END
116700000000     C     KEY           IFEQ      F20
116800000000     C                   MOVEL     '20'          FUNKEY
116900000000     C                   END
117000000000     C     KEY           IFEQ      F21
117100000000     C                   MOVEL     '21'          FUNKEY
117200000000     C                   END
117300000000     C     KEY           IFEQ      F22
117400000000     C                   MOVEL     '22'          FUNKEY
117500000000     C                   END
117600000000     C     KEY           IFEQ      F23
117700000000     C                   MOVEL     '23'          FUNKEY
117800000000     C                   END
117900000000     C     KEY           IFEQ      F24
118000000000     C                   MOVEL     '24'          FUNKEY
118100000000     C                   END
118200000000     C     KEY           IFEQ      ROLLUP
118300000000     C                   MOVEL     'RU'          FUNKEY
118400000000     C                   END
118500000000     C     KEY           IFEQ      HELP
118600000000     C                   MOVEL     'HP'          FUNKEY
118700000000     C                   END
118800000000     C                   ENDSR
118900000000     C*------------------------------------------------------------------------*
119000000000     C* NOTE PER COMPILAZIONE SCHIERE
119100000000     C*------------------------------------------------------------------------*
119200000000     C* -MSG-
119300000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
119400000000     C* (NON TOCCARE MSG,1)
119500000000     C*
119600000000     C* -CMD-
119700000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
119800000000     C*
119900000000     C* -FUD-
120000000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
120100000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
120200000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
120300000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
120400000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
120500000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
120600000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
120700000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
120800000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
120900000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
121000000000     C*
121100000000     C*------------------------------------------------------------------------*
121200991103** ======== SCHIERA: MSG  (MESSAGGI)                       ================
121300060426TISIE0R- Tasto funzionale non gestito.                                         1
121400060426TISIE0R- Versione Cappario non presente, F6=Procedi - F3=Fine.                 2
121500060426TISIE0R- Data non valida.                                                      3
121600060426TISIE0R- La data di decorrenza deve essere inferiore alla data di scadenza.    4
121700060426TISIE0R-                                                                       5
121800060426TISIE0R- La data decorrenza non è compatibile con quelle già presenti.         6
121900060426TISIE0R- La data scadenza non è compatibile con quelle già presenti.           7
122000060426TISIE0R- ATTENZIONE versioni e/o scadenze non uguali su tutti i file!!!        8
122100060426TISIE0R-                                                                       9
122200060426TISIE0R-                                                                      10
122300060426TISIE0R-                                                                      11
122400060426TISIE0R-                                                                      12
122500060426TISIE0R-                                                                      13
122600060426TISIE0R-                                                                      14
122700060426TISIE0R-                                                                      15
122800060426TISIE0R-                                                                      16
122900060426TISIE0R-                                                                      17
123000060426TISIE0R-                                                                      18
123100060426TISIE0R-                                                                      19
123200060426TISIE0R-                                                                      20
123300000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
123400000000                   -        (S/N)=N                        funz.01¬
123500000000                   -        (S/N)=N                        funz.02¬
123600000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
123700000000                   -        (S/N)=N                        funz.04¬
123800000000                   -        (S/N)=N                        funz.05¬
123900060608F06=EXE Import     -        (S/N)=S                        funz.06¬
124000060608                   -        (S/N)=N                        funz.07¬
124100060608                   -        (S/N)=N                        funz.08¬
124200000000                   -        (S/N)=N                        funz.09¬
124300060608F10=EXE CLRPFM     -        (S/N)=S                        funz.10¬
124400000000                   -        (S/N)=N                        funz.11¬
124500991103F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
124600000000                   -        (S/N)=N                        funz.13¬
124700000000                   -        (S/N)=N                        funz.14¬
124800000000                   -        (S/N)=N                        funz.15¬
124900000000                   -        (S/N)=N                        funz.16¬
125000000000                   -        (S/N)=N                        funz.17¬
125100000000                   -        (S/N)=N                        funz.18¬
125200000000                   -        (S/N)=N                        funz.19¬
125300000000                   -        (S/N)=N                        funz.20¬
125400000000                   -        (S/N)=N                        funz.21¬
125500000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
125600000000                   -        (S/N)=N                        funz.23¬
125700991103F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
