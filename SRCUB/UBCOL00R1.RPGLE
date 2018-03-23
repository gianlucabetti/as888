000100970526     H*------------------------------------------------------------------------*
000200140114     H* Lancio Diagnostica formati TIT per posizione campi chiave
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800160217     FUBCOL00D1 cf   e             WORKSTN INFDS(DEVDS1)
000900160217     F                                     SFILE(COL00DMS:MSGULT)
001000970526     D*------------------------------------------------------------------------*
001100970526     D* SCHIERE
001200970526     D*------------------------------------------------------------------------*
001300970526     D* MESSAGGI
001400970526     D*------------
001500000807     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
001600970526     D*------------
001700970526     D* DESCRIZIONI TASTI FUNZIONALI
001800970526     D*------------
001900970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002000970526     D*------------
002100970526     D* FUNZIONI DA CARICARE A VIDEO
002200970526     D*------------
002300970526     D FUV             S             19    DIM(24)
002400970526     D*------------------------------------------------------------------------*
002500970526     D* INPUT
002600970526     D*------------------------------------------------------------------------*
002700970526     D* RIDEFINIZIONE FUNZIONI VIDEO
002800970526     D*------------
002900970526     D FUDDS           DS
003000970526     D  FUDDE1                 1      1
003100970526     D  FUDFUN                 2      3
003200970526     D  FUDDE2                 4     19
003300970526     D  FUDFI0                20     34
003400970526     D  FUDATT                35     35
003500970526     D  FUDFI1                36     80
003600970526     D  FUDDES                 1     19
003700970526     D  FUDRIG                 1     80
003800970526     D*------------
003900970526     D* DS GESTIONE TASTI FUNZIONALI
004000970526     D*------------
004100970526     D DSKEY         E DS
004200970526     D DEVDS1          DS
004300970526     D  KEY                  369    369
004400970526     D*------------
004500970526     D* DS "XMSG" - GESTIONE MESSAGGI
004600970526     D*------------
004700970526     D DSMSG         E DS
004800970528     D*------------------
004900970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
005000970528     D*------------------
005100970528     D WLBDA8          DS                  INZ
005200970528     D  G08DAT                 1      8  0
005300970528     D  G08INV                 9     16  0
005400970528     D  G08ERR                17     17
005500970528     D  G08TGI                18     22  0
005600970526     D*------------
005700970526     D* ARCHITETTURA
005800970526     D*------------
005900970526     D KPJBA         E DS
006000020923     D*-------------------
006100020923     D* DS REPERIMENTO DATI UTENTE
006200020923     D*-------------------
006300020923     D TIBS34DS      E DS                                                       *Profili utente
006400020923     D DDATIUTE      E DS                                                       *Dati utente
006500020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006600120927     D* DS REPERIMENTO PADRE/FIGLI
006700120927     D TIBS10DS      E DS
006800120928     D*-------------------
006900120928
007000121003     d Status         sds
007100121003     d  SDSpgm           *proc
007200121003     d  SDSprm           *parms
007300121003     d  SDSuser              254    263
007400130116
007500130116     d ora             s               t
007600140117     d wX              s              2s 0
007700150407     d wVAB            s               n
007800150407     d wVAO            s               n
007900160223
008000160223       //--------------------------------------------------------------
008100160223       // Definizione prototipi procedure.
008200160223       //--------------------------------------------------------------
008300160223
008400160223     d/copy gaitrasrc/srcProtoPi,UBCOL00R
008500120928
008600970526     C*------------------------------------------------------------------------*
008700000303     C* MAIN LINES
008800970526     C*------------------------------------------------------------------------*
008900970526     C*
009000120927     C* metto i default su D03
009100120927     C                   EXSR      InzD03
009200120927     C*
009300120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009400991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
009500970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
009600970526     C     FINL00        OREQ      '1'                                          *FINE PGM
009700970526     C*
009800970526     C* EMETTE FORMATO VIDEO
009900970526     C                   EXSR      BUTFMR
010000970526     C*
010100970526     C* GESTIONE TASTI FUNZIONALI
010200970526     C                   EXSR      FUNCON
010300970526E   2C                   ENDDO
010400970526     C*
010500970526     C* FINE PGM
010600970526     C                   SETON                                        LR
010700970526     C*------------------------------------------------------------------------*
010800970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
010900970526     C*------------------------------------------------------------------------*
011000970526     C     BUTFMR        BEGSR
011100970526     C*
011200970526     C* EMETTE LA TESTATA
011300160217     C                   WRITE     COL00D01
011400970526     C*
011500970526     C* SPEDISCE EVENTUALE MESSAGGIO
011600970526     C                   EXSR      SNDMSG
011700970526     C*
011800970526     C* EMETTE LE FUNZIONI
011900160217     C                   WRITE     COL00D02
012000970526     C*
012100970526     C* EMETTE IL VIDEO
012200160217     C                   EXFMT     COL00D03
012300970526     C*
012400991214     C* REIMPOSTA FLAG DI ERRORE
012500991214     C                   MOVEL     '0'           ERRL00
012600970526     C*
012700970526     C* REIMPOSTA INDICATORI DI ERRORE
012800020611     C                   SETOFF                                       555556
012900970526     C*
013000970526     C                   ENDSR
013100970526     C*------------------------------------------------------------------------*
013200120927     C* InzD03 - Inizializzo D03
013300970526     C*------------------------------------------------------------------------*
013400120927     C     InzD03        BEGSR
013500120927     C*
013600130108     C*
013700120927     C                   ENDSR
013800120927     C*------------------------------------------------------------------------*
013900120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
014000120927     C*------------------------------------------------------------------------*
014100120927     C     SNDMSG        BEGSR
014200120927     C*
014300970526     C* PULISCE IL SFILE MESSAGGI
014400970526     C                   MOVEL     '1'           *IN40
014500160217     C                   WRITE     COL00DMC
014600970526     C                   MOVEL     '0'           *IN40
014700970526     C*
014800970526     C* VISUALIZZA IL MESSAGGIO
014900970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
015000970526     C                   CALL      'XMSG'        XMSGPA
015100020611     C     DSMSER        IFEQ      *ZEROS
015200970526     C                   MOVEL     DSMSKE        MSGKEY
015300020611     C                   ENDIF
015400970526     C                   Z-ADD     1             MSGULT
015500160217     C                   WRITE     COL00DMS
015600160217     C                   WRITE     COL00DMC
015700970526     C*
015800970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
015900970526     C                   MOVEL     *BLANKS       DSMSMS
016000970526     C*
016100970526     C                   ENDSR
016200970526     C*------------------------------------------------------------------------*
016300970526     C* ENTGES - GESTIONE ENTER -> INVIO
016400970526     C*------------------------------------------------------------------------*
016500970526     C     ENTGES        BEGSR
016600970526     C*
016700970526     C* CONTROLLA I CAMPI IMMESSI
016800970526     C                   EXSR      DECODI
016900970526     C*
017000970526     C                   ENDSR
017100970526     C*------------------------------------------------------------------------*
017200970526     C* F03GES - GESTIONE F03 -> FINE
017300970526     C*------------------------------------------------------------------------*
017400970526     C     F03GES        BEGSR
017500970526     C*
017600970526     C* IMPOSTA "PREMUTO F03"
017700970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
017800970526     C*
017900970526     C                   ENDSR
018000970526     C*------------------------------------------------------------------------*
018100991214     C* F06GES - GESTIONE F06 -> CONFERMA
018200970526     C*------------------------------------------------------------------------*
018300970526     C     F06GES        BEGSR
018400970526     C*
018500991214     C* CONTROLLA I CAMPI IMMESSI
018600991214     C                   EXSR      DECODI
018700020611     C*
018800991214     C* ESEGUE ROUTINE DI CONFERMA
018900020611     C     ERRL00        IFEQ      '0'
019000991214     C                   EXSR      ESEGUI
019100130110     C* il messaggio di simulazione conclusa lo do sempre, a prescindere dagli esiti
019200121109     C                   MOVEL     MSG(20)       DSMSMS
019300121109     C* non esco mai dal pgm
019400121109     C***                MOVEL     '1'           FINL00
019500121108     C                   ENDIF
019600970526     C*
019700991214     C                   ENDSR
019800970526     C*------------------------------------------------------------------------*
019900130109     C* F07GES - GESTIONE F07 -> STAMPA
020000970526     C*------------------------------------------------------------------------*
020100130109     C     F07GES        BEGSR
020200970526     C*
020300140818     C                   EXSR      F06GES
020400130110     C*
020500970526     C                   ENDSR
020600130109     C*------------------------------------------------------------------------*
020700130109     C* F12GES - GESTIONE F12 -> RITORNO
020800130109     C*------------------------------------------------------------------------*
020900130109     C     F12GES        BEGSR
021000130109     C*
021100130109     C* IMPOSTA "PREMUTO F12"
021200130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021300130109     C*
021400130109     C                   ENDSR
021500970526     C*------------------------------------------------------------------------*
021600970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
021700970526     C*------------------------------------------------------------------------*
021800970526     C     F24GES        BEGSR
021900970526     C*
022000970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
022100970526     C*
022200970526     C                   ENDSR
022300970526     C*------------------------------------------------------------------------*
022400140116     C* ESEGUI - OPERAZIONI DI LANCIO PGM
022500970526     C*------------------------------------------------------------------------*
022600991214     C     ESEGUI        BEGSR
022700140120     C*
022800160217     C* richiamo il driver
022900160223     C                   EVAL      UBCOLOpz     = d03Opz
023000160223     C                   EVAL      UBCOLFlgOpe  = d03FlgOpe
023100160223     C                   EVAL      UBCOLTLa     = d03TLa
023200160223     C                   EVAL      UBCOLAAS     = d03AAS
023300160223     C                   EVAL      UBCOLLNP     = d03LNP
023400160223     C                   EVAL      UBCOLNRS     = d03NRS
023500160223     C                   EVAL      UBCOLNSP     = d03NSP
023600160223     c                   clear                   UBCOLErr
023700121108     C* chiamo
023800160217     C                   CALL      'UBCOL00R'
023900160223     c                   parm                    UBCOLOpz
024000160223     c                   parm                    UBCOLFlgOpe
024100160223     c                   parm                    UBCOLTLa
024200160223     c                   parm                    UBCOLAAS
024300160223     c                   parm                    UBCOLLNP
024400160223     c                   parm                    UBCOLNRS
024500160223     c                   parm                    UBCOLNSP
024600160223     c                   parm                    UBCOLNCL
024700160223     c                   parm                    UBCOLTIP
024800160223     c                   parm                    UBCOLDCM
024900160223     C                   PARM                    UBCOLHMC
025000160223     C                   PARM                    UBCOLSkBRT
025100160223     C                   PARM                    UBCOLSkCLI
025200160223     c                   parm                    UBCOLSkDCM
025300160223     c                   parm                    UBCOLSkHMC
025400160223     c                   parm                    UBCOLSkDFS
025500160223     c                   parm                    UBCOLSkHMS
025600160223     c                   parm                    UBCOLSkVUC
025700160223     c                   parm                    UBCOLSkPUC
025800160223     c                   parm                    UBCOLErr
025900121108     C*
026000160223     c                   EVAL      D03Err   = UBCOLErr
026100160217     C* valorizzo l'output solo se l'esito è OK
026200160217     c                   IF        D03Err=0
026300160223     C                   eval      d03NCL =    UBCOLNCL
026400160223     C                   eval      d03TIP =    UBCOLTIP
026500160223     C                   eval      d03DCM =    UBCOLDCM
026600160223     C                   eval      d03HMC =    UBCOLHMC
026700160223     C                   eval      D03SKBRT_1 =  UBCOLSkBRT(1)
026800160223     C                   eval      D03SKCLI_1 =  UBCOLSkCLI(1)
026900160223     c                   eval      D03SKDCM_1 =  UBCOLSkDCM(1)
027000160223     c                   eval      D03SKHMC_1 =  UBCOLSkHMC(1)
027100160223     c                   eval      D03SKDFS_1 =  UBCOLSkDFS(1)
027200160223     c                   eval      D03SKHMS_1 =  UBCOLSkHMS(1)
027300160223     c                   eval      D03SKVUC_1 =  UBCOLSkVUC(1)
027400160223     c                   eval      D03SKPUC_1 =  UBCOLSkPUC(1)
027500160223     C                   eval      D03SKBRT_2 =  UBCOLSkBRT(2)
027600160223     C                   eval      D03SKCLI_2 =  UBCOLSkCLI(2)
027700160223     c                   eval      D03SKDCM_2 =  UBCOLSkDCM(2)
027800160223     c                   eval      D03SKHMC_2 =  UBCOLSkHMC(2)
027900160223     c                   eval      D03SKDFS_2 =  UBCOLSkDFS(2)
028000160223     c                   eval      D03SKHMS_2 =  UBCOLSkHMS(2)
028100160223     c                   eval      D03SKVUC_2 =  UBCOLSkVUC(2)
028200160223     c                   eval      D03SKPUC_2 =  UBCOLSkPUC(2)
028300160223     C                   eval      D03SKBRT_3 =  UBCOLSkBRT(3)
028400160223     C                   eval      D03SKCLI_3 =  UBCOLSkCLI(3)
028500160223     c                   eval      D03SKDCM_3 =  UBCOLSkDCM(3)
028600160223     c                   eval      D03SKHMC_3 =  UBCOLSkHMC(3)
028700160223     c                   eval      D03SKDFS_3 =  UBCOLSkDFS(3)
028800160223     c                   eval      D03SKHMS_3 =  UBCOLSkHMS(3)
028900160223     c                   eval      D03SKVUC_3 =  UBCOLSkVUC(3)
029000160223     c                   eval      D03SKPUC_3 =  UBCOLSkPUC(3)
029100160223     C                   eval      D03SKBRT_4 =  UBCOLSkBRT(4)
029200160223     C                   eval      D03SKCLI_4 =  UBCOLSkCLI(4)
029300160223     c                   eval      D03SKDCM_4 =  UBCOLSkDCM(4)
029400160223     c                   eval      D03SKHMC_4 =  UBCOLSkHMC(4)
029500160223     c                   eval      D03SKDFS_4 =  UBCOLSkDFS(4)
029600160223     c                   eval      D03SKHMS_4 =  UBCOLSkHMS(4)
029700160223     c                   eval      D03SKVUC_4 =  UBCOLSkVUC(4)
029800160223     c                   eval      D03SKPUC_4 =  UBCOLSkPUC(4)
029900160223     C                   eval      D03SKBRT_5 =  UBCOLSkBRT(5)
030000160223     C                   eval      D03SKCLI_5 =  UBCOLSkCLI(5)
030100160223     c                   eval      D03SKDCM_5 =  UBCOLSkDCM(5)
030200160223     c                   eval      D03SKHMC_5 =  UBCOLSkHMC(5)
030300160223     c                   eval      D03SKDFS_5 =  UBCOLSkDFS(5)
030400160223     c                   eval      D03SKHMS_5 =  UBCOLSkHMS(5)
030500160223     c                   eval      D03SKVUC_5 =  UBCOLSkVUC(5)
030600160223     c                   eval      D03SKPUC_5 =  UBCOLSkPUC(5)
030700160223     C                   eval      D03SKBRT_6 =  UBCOLSkBRT(6)
030800160223     C                   eval      D03SKCLI_6 =  UBCOLSkCLI(6)
030900160223     c                   eval      D03SKDCM_6 =  UBCOLSkDCM(6)
031000160223     c                   eval      D03SKHMC_6 =  UBCOLSkHMC(6)
031100160223     c                   eval      D03SKDFS_6 =  UBCOLSkDFS(6)
031200160223     c                   eval      D03SKHMS_6 =  UBCOLSkHMS(6)
031300160223     c                   eval      D03SKVUC_6 =  UBCOLSkVUC(6)
031400160223     c                   eval      D03SKPUC_6 =  UBCOLSkPUC(6)
031500160223     C                   eval      D03SKBRT_7 =  UBCOLSkBRT(7)
031600160223     C                   eval      D03SKCLI_7 =  UBCOLSkCLI(7)
031700160223     c                   eval      D03SKDCM_7 =  UBCOLSkDCM(7)
031800160223     c                   eval      D03SKHMC_7 =  UBCOLSkHMC(7)
031900160223     c                   eval      D03SKDFS_7 =  UBCOLSkDFS(7)
032000160223     c                   eval      D03SKHMS_7 =  UBCOLSkHMS(7)
032100160223     c                   eval      D03SKVUC_7 =  UBCOLSkVUC(7)
032200160223     c                   eval      D03SKPUC_7 =  UBCOLSkPUC(7)
032300160223     C                   eval      D03SKBRT_8 =  UBCOLSkBRT(8)
032400160223     C                   eval      D03SKCLI_8 =  UBCOLSkCLI(8)
032500160223     c                   eval      D03SKDCM_8 =  UBCOLSkDCM(8)
032600160223     c                   eval      D03SKHMC_8 =  UBCOLSkHMC(8)
032700160223     c                   eval      D03SKDFS_8 =  UBCOLSkDFS(8)
032800160223     c                   eval      D03SKHMS_8 =  UBCOLSkHMS(8)
032900160223     c                   eval      D03SKVUC_8 =  UBCOLSkVUC(8)
033000160223     c                   eval      D03SKPUC_8 =  UBCOLSkPUC(8)
033100160223     C                   eval      D03SKBRT_9 =  UBCOLSkBRT(9)
033200160223     C                   eval      D03SKCLI_9 =  UBCOLSkCLI(9)
033300160223     c                   eval      D03SKDCM_9 =  UBCOLSkDCM(9)
033400160223     c                   eval      D03SKHMC_9 =  UBCOLSkHMC(9)
033500160223     c                   eval      D03SKDFS_9 =  UBCOLSkDFS(9)
033600160223     c                   eval      D03SKHMS_9 =  UBCOLSkHMS(9)
033700160223     c                   eval      D03SKVUC_9 =  UBCOLSkVUC(9)
033800160223     c                   eval      D03SKPUC_9 =  UBCOLSkPUC(9)
033900160223     C                   eval      D03SKBRT_0 =  UBCOLSkBRT(10)
034000160223     C                   eval      D03SKCLI_0 =  UBCOLSkCLI(10)
034100160223     c                   eval      D03SKDCM_0 =  UBCOLSkDCM(10)
034200160223     c                   eval      D03SKHMC_0 =  UBCOLSkHMC(10)
034300160223     c                   eval      D03SKDFS_0 =  UBCOLSkDFS(10)
034400160223     c                   eval      D03SKHMS_0 =  UBCOLSkHMS(10)
034500160223     c                   eval      D03SKVUC_0 =  UBCOLSkVUC(10)
034600160223     c                   eval      D03SKPUC_0 =  UBCOLSkPUC(10)
034700160223     C                   ELSE
034800160223     C                   eval      d03NCL =    0
034900160223     C                   eval      d03TIP =    *blank
035000160223     C                   eval      d03DCM =    0
035100160223     C                   eval      d03HMC =    0
035200160223     C                   eval      D03SKBRT_1 =  *blank
035300160223     C                   eval      D03SKCLI_1 =  *blank
035400160223     c                   eval      D03SKDCM_1 =  0
035500160223     c                   eval      D03SKHMC_1 =  0
035600160223     c                   eval      D03SKDFS_1 =  0
035700160223     c                   eval      D03SKHMS_1 =  0
035800160223     c                   eval      D03SKVUC_1 =  0
035900160223     c                   eval      D03SKPUC_1 =  0
036000160223     C                   eval      D03SKBRT_2 =  *blank
036100160223     C                   eval      D03SKCLI_2 =  *blank
036200160223     c                   eval      D03SKDCM_2 =  0
036300160223     c                   eval      D03SKHMC_2 =  0
036400160223     c                   eval      D03SKDFS_2 =  0
036500160223     c                   eval      D03SKHMS_2 =  0
036600160223     c                   eval      D03SKVUC_2 =  0
036700160223     c                   eval      D03SKPUC_2 =  0
036800160223     C                   eval      D03SKBRT_3 =  *blank
036900160223     C                   eval      D03SKCLI_3 =  *blank
037000160223     c                   eval      D03SKDCM_3 =  0
037100160223     c                   eval      D03SKHMC_3 =  0
037200160223     c                   eval      D03SKDFS_3 =  0
037300160223     c                   eval      D03SKHMS_3 =  0
037400160223     c                   eval      D03SKVUC_3 =  0
037500160223     c                   eval      D03SKPUC_3 =  0
037600160223     C                   eval      D03SKBRT_4 =  *blank
037700160223     C                   eval      D03SKCLI_4 =  *blank
037800160223     c                   eval      D03SKDCM_4 =  0
037900160223     c                   eval      D03SKHMC_4 =  0
038000160223     c                   eval      D03SKDFS_4 =  0
038100160223     c                   eval      D03SKHMS_4 =  0
038200160223     c                   eval      D03SKVUC_4 =  0
038300160223     c                   eval      D03SKPUC_4 =  0
038400160223     C                   eval      D03SKBRT_5 =  *blank
038500160223     C                   eval      D03SKCLI_5 =  *blank
038600160223     c                   eval      D03SKDCM_5 =  0
038700160223     c                   eval      D03SKHMC_5 =  0
038800160223     c                   eval      D03SKDFS_5 =  0
038900160223     c                   eval      D03SKHMS_5 =  0
039000160223     c                   eval      D03SKVUC_5 =  0
039100160223     c                   eval      D03SKPUC_5 =  0
039200160223     C                   eval      D03SKBRT_6 =  *blank
039300160223     C                   eval      D03SKCLI_6 =  *blank
039400160223     c                   eval      D03SKDCM_6 =  0
039500160223     c                   eval      D03SKHMC_6 =  0
039600160223     c                   eval      D03SKDFS_6 =  0
039700160223     c                   eval      D03SKHMS_6 =  0
039800160223     c                   eval      D03SKVUC_6 =  0
039900160223     c                   eval      D03SKPUC_6 =  0
040000160223     C                   eval      D03SKBRT_7 =  *blank
040100160223     C                   eval      D03SKCLI_7 =  *blank
040200160223     c                   eval      D03SKDCM_7 =  0
040300160223     c                   eval      D03SKHMC_7 =  0
040400160223     c                   eval      D03SKDFS_7 =  0
040500160223     c                   eval      D03SKHMS_7 =  0
040600160223     c                   eval      D03SKVUC_7 =  0
040700160223     c                   eval      D03SKPUC_7 =  0
040800160223     C                   eval      D03SKBRT_8 =  *blank
040900160223     C                   eval      D03SKCLI_8 =  *blank
041000160223     c                   eval      D03SKDCM_8 =  0
041100160223     c                   eval      D03SKHMC_8 =  0
041200160223     c                   eval      D03SKDFS_8 =  0
041300160223     c                   eval      D03SKHMS_8 =  0
041400160223     c                   eval      D03SKVUC_8 =  0
041500160223     c                   eval      D03SKPUC_8 =  0
041600160223     C                   eval      D03SKBRT_9 =  *blank
041700160223     C                   eval      D03SKCLI_9 =  *blank
041800160223     c                   eval      D03SKDCM_9 =  0
041900160223     c                   eval      D03SKHMC_9 =  0
042000160223     c                   eval      D03SKDFS_9 =  0
042100160223     c                   eval      D03SKHMS_9 =  0
042200160223     c                   eval      D03SKVUC_9 =  0
042300160223     c                   eval      D03SKPUC_9 =  0
042400160223     C                   eval      D03SKBRT_0 =  *blank
042500160223     C                   eval      D03SKCLI_0 =  *blank
042600160223     c                   eval      D03SKDCM_0 =  0
042700160223     c                   eval      D03SKHMC_0 =  0
042800160223     c                   eval      D03SKDFS_0 =  0
042900160223     c                   eval      D03SKHMS_0 =  0
043000160223     c                   eval      D03SKVUC_0 =  0
043100160223     c                   eval      D03SKPUC_0 =  0
043200160223     C                   ENDIF
043300991214     C*
043400970526     C                   ENDSR
043500970526     C*------------------------------------------------------------------------*
043600991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
043700970526     C*------------------------------------------------------------------------*
043800970526     C     DECODI        BEGSR
043900121019     C*
044000160217     C                   SETOFF                                       505152
044100160223     C                   clear                   UBCOLNCL
044200160223     C                   clear                   UBCOLTIP
044300160223     C                   clear                   UBCOLDCM
044400160223     C                   clear                   UBCOLHMC
044500160223     C                   clear                   UBCOLSkBRT
044600160223     C                   clear                   UBCOLSkCLI
044700160223     c                   clear                   UBCOLSkDCM
044800160223     c                   clear                   UBCOLSkHMC
044900160223     c                   clear                   UBCOLSkDFS
045000160223     c                   clear                   UBCOLSkHMS
045100160223     c                   clear                   UBCOLSkVUC
045200160223     c                   clear                   UBCOLSkPUC
045300120927     C*
045400970528     C                   ENDSR
045500970526     C*------------------------------------------------------------------------*
045600970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
045700970526     C*------------------------------------------------------------------------*
045800970526     C     AZZVID        BEGSR
045900970526     C* CAMPI
046000970526     C*
046100970526     C                   ENDSR
046200970526     C*------------------------------------------------------------------------*
046300020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
046400970526     C*------------------------------------------------------------------------*
046500970526     C     IMPFUN        BEGSR
046600970526     C*
046700970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
046800970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
046900970526     C                   EXSR      FUNGES
047000970526     C*
047100970526     C                   ENDSR
047200020923     C*--------------------------------------------------------------------------------------------*
047300020923     C* REPERISCE I DATI UTENTE
047400020923     C*--------------------------------------------------------------------------------------------*
047500020923     C     REPDATIUTE    BEGSR
047600020923     C*
047700020923     C* INIZIALIZZA VARIABILI DI WRK
047800020923     C                   CLEAR                   TIBS34DS
047900020923     C                   CLEAR                   AZUTEDS
048000020923     C                   CLEAR                   DDATIUTE
048100020923     C*
048200020923     C     *DTAARA       DEFINE    §azute        azuteds
048300020923     C     *DTAARA       DEFINE    §datiute      ddatiute
048400020923     C                   IN(E)     *DTAARA
048500020923if  1C                   IF        %Error
048600020923     c                   EVAL      I34Tla = 'L'
048700020923     C                   CALL      'TIBS34R'
048800020923     C                   PARM                    Tibs34Ds
048900020923     C                   IN        *DTAARA
049000020923e   1C                   ENDIF
049100020923     C*
049200020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
049300020923     C                   MOVEL(P)  RSUT          DSFIRS           20
049400020923     C*
049500020923     C                   ENDSR
049600970526     C*------------------------------------------------------------------------*
049700970526     C* *INZSR - OPERAZIONI INIZIALI
049800970526     C*------------------------------------------------------------------------*
049900970526     C     *INZSR        BEGSR
050000991217     C*
050100991217     C     *ENTRY        PLIST
050200991217     C                   PARM                    KPJBA
050300020923     C*
050400020923     C* REPERISCE I DATI DELL'UTENTE
050500020923     C                   EXSR      REPDATIUTE
050600970526     C*
050700970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
050800970526     C     XMSGPA        PLIST
050900970526     C                   PARM                    DSMSMS
051000970526     C                   PARM                    DSMSPG
051100970526     C                   PARM                    DSMSKE
051200970526     C                   PARM                    DSMSER
051300970526     C*
051400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
051500970526     C                   CALL      'XKEY'
051600970526     C                   PARM                    DSKEY
051700970526     C*---
051800970526     C* VARIABILI NON RIFERITE AL DATA BASE
051900970526     C*---
052000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
052100991214     C                   MOVEL     *BLANKS       DEPOP1            3
052200970526     C*---
052300970526     C* VARIABILI DI CONTROLLO
052400970526     C*---
052500970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
052600970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
052700970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
052800970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
052900970526     C*
053000970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
053100160217     C                   MOVEL     'UBCOL00R1'   DSMSPG
053200160217     C                   MOVEL     'UBCOL00R1'   MSGQUE
053300970526     C                   MOVEL     *BLANKS       MSGKEY
053400970526     C                   MOVEL     *BLANKS       DSMSMS
053500970528     C*
053600970528     C* CALCOLA LA DATA CORRENTE
053700970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
053800970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
053900970528     C                   Z-ADD     WN8           G08DAT
054000970528     C                   Z-ADD     *ZEROS        G08INV
054100970528     C                   MOVEL     '0'           G08ERR
054200970528     C                   CALL      'XSRDA8'
054300970528     C                   PARM                    WLBDA8
054400970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
054500000310     C*
054600000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
054700000310     C                   EXSR      AZZVID
054800970526     C*
054900970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
055000970526     C                   EXSR      IMPFUN
055100970526     C*
055200970526     C                   ENDSR
055300970526     C*------------------------------------------------------------------------*
055400970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
055500970526     C*------------------------------------------------------------------------*
055600970526     C     FUNGES        BEGSR
055700970526     C*
055800970526     C* PULISCO CAMPI DI LAVORO
055900970526     C                   Z-ADD     0             W                 5 0
056000970526     C*
056100970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
056200970526     C* E CARICO TABELLA FUNZIONI ATTIVE
056300970526     C     1             DO        24            I
056400970526     C                   MOVEL     FUD(I)        FUDRIG
056500970526     C*
056600970526     C* PULIZIA FUNZIONI PRECEDENTI
056700970526     C                   MOVEL     *BLANKS       FUV(I)
056800970526     C*
056900970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
057000970526     C* (SALTA F24)
057100970526     C     FUDATT        IFEQ      'S'
057200970526     C     FUDFUN        ANDNE     '24'
057300970526     C                   ADD       1             W
057400970526     C                   MOVEL     FUDDES        FUV(W)
057500970526     C                   END
057600970526     C                   END                                                    DO WHILE
057700970526     C*
057800970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
057900970526     C* ALTRIMENTI DISATTIVO F24
058000970526     C     1             DO        24            I
058100970526     C                   MOVEL     FUD(I)        FUDRIG
058200970526     C*
058300970526     C     FUDFUN        IFEQ      '24'
058400970526     C     W             IFGT      6
058500970526     C                   MOVEL     'S'           FUDATT
058600970526     C                   ELSE
058700970526     C                   MOVEL     *BLANKS       FUDATT
058800970526     C                   END                                                    W GT 8
058900970526     C                   MOVEL     FUDRIG        FUD(I)
059000970526     C                   END
059100970526     C                   END                                                    DO WHILE
059200970526     C*
059300970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
059400970526     C     W             DIV       6             I
059500970526     C                   MVR                     Z
059600970526     C*
059700970526     C     Z             IFNE      0
059800970526     C     I             ADD       1             Z
059900970526     C                   ELSE
060000970526     C                   Z-ADD     I             Z                 5 0
060100970526     C                   END
060200970526     C*
060300970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
060400970526     C                   ADD       1             FUNGRU
060500970526     C     FUNGRU        IFGT      Z
060600970526     C     FUNGRU        ORLT      1
060700970526     C                   Z-ADD     1             FUNGRU
060800970526     C                   END
060900970526     C*
061000970526     C* PRIMO GRUPPO DI 6 FUNZIONI
061100970526     C     FUNGRU        IFEQ      1
061200970526     C                   MOVEL     FUV(1)        FUN001
061300970526     C                   MOVEL     FUV(2)        FUN002
061400970526     C                   MOVEL     FUV(3)        FUN003
061500970526     C                   MOVEL     FUV(4)        FUN004
061600970526     C                   MOVEL     FUV(5)        FUN005
061700970526     C                   MOVEL     FUV(6)        FUN006
061800970526     C                   END
061900970526     C*
062000970526     C* SECONDO GRUPPO DI 6 FUNZIONI
062100970526     C     FUNGRU        IFEQ      2
062200970526     C                   MOVEL     FUV(7)        FUN001
062300970526     C                   MOVEL     FUV(8)        FUN002
062400970526     C                   MOVEL     FUV(9)        FUN003
062500970526     C                   MOVEL     FUV(10)       FUN004
062600970526     C                   MOVEL     FUV(11)       FUN005
062700970526     C                   MOVEL     FUV(12)       FUN006
062800970526     C                   END
062900970526     C*
063000970526     C* TERZO GRUPPO DI 6 FUNZIONI
063100970526     C     FUNGRU        IFEQ      3
063200970526     C                   MOVEL     FUV(13)       FUN001
063300970526     C                   MOVEL     FUV(14)       FUN002
063400970526     C                   MOVEL     FUV(15)       FUN003
063500970526     C                   MOVEL     FUV(16)       FUN004
063600970526     C                   MOVEL     FUV(17)       FUN005
063700970526     C                   MOVEL     FUV(18)       FUN006
063800970526     C                   END
063900970526     C*
064000970526     C* QUARTO GRUPPO DI 6 FUNZIONI
064100970526     C     FUNGRU        IFEQ      4
064200970526     C                   MOVEL     FUV(19)       FUN001
064300970526     C                   MOVEL     FUV(20)       FUN002
064400970526     C                   MOVEL     FUV(21)       FUN003
064500970526     C                   MOVEL     FUV(22)       FUN004
064600970526     C                   MOVEL     *BLANKS       FUN005
064700970526     C                   MOVEL     *BLANKS       FUN006
064800970526     C                   END
064900970526     C*
065000970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
065100970526     C                   MOVEL     *BLANKS       FUN007
065200970526     C                   MOVEL     *BLANKS       FUN008
065300970526     C*
065400970526     C     1             DO        24            I
065500970526     C                   MOVEL     FUD(I)        FUDRIG
065600970526     C     FUDFUN        IFEQ      '24'
065700970526     C     FUDATT        ANDEQ     'S'
065800970526     C                   MOVEL     FUDDES        FUN008
065900970526     C                   END
066000970526     C                   END                                                    DO
066100970526     C*
066200970526     C                   ENDSR
066300970526     C*------------------------------------------------------------------------*
066400970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
066500970526     C*------------------------------------------------------------------------*
066600970526     C     FUNCON        BEGSR
066700970526     C*
066800970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
066900970526     C                   EXSR      FUNVAR
067000970526     C*
067100970526     C* PULISCO CAMPI DI LAVORO
067200970526     C                   Z-ADD     0             I                 5 0
067300970526     C                   Z-ADD     0             W                 5 0
067400970526     C*
067500970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
067600970526     C     FUNKEY        IFNE      *BLANKS
067700970526     C     1             DO        24            I
067800970526     C                   MOVEL     FUD(I)        FUDRIG
067900970526     C*
068000970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
068100970526     C     FUNKEY        IFEQ      FUDFUN
068200970526     C     FUDATT        ANDEQ     'S'
068300970526     C                   Z-ADD     I             W
068400970526     C                   END
068500970526     C                   END                                                    DO WHILE
068600970526     C*
068700970526     C* FUNZIONE NON ATTIVA (ERRORE)
068800970526     C     W             IFEQ      0
068900970526     C*
069000970526     C* EMISSIONE MESSAGGIO DI ERRORE
069100970526     C                   MOVEL     MSG(1)        DSMSMS
069200970526     C*
069300970526     C                   END                                                    W=0
069400970526     C                   END                                                    FUNKEY NE *BLANKS
069500970526     C*
069600970526     C* FUNZIONE ATTIVA (PROCEDI ...)
069700970526     C     W             IFNE      0
069800970526     C     FUNKEY        OREQ      *BLANKS
069900970526     C     FUNKEY        IFEQ      *BLANKS
070000970526     C                   EXSR      ENTGES
070100970526     C                   END
070200970526     C     FUNKEY        IFEQ      '03'
070300970526     C                   EXSR      F03GES
070400970526     C                   END
070500970526     C     FUNKEY        IFEQ      '06'
070600970526     C                   EXSR      F06GES
070700970526     C                   END
070800130109     C     FUNKEY        IFEQ      '07'
070900130109     C                   EXSR      F07GES
071000130109     C                   END
071100970526     C     FUNKEY        IFEQ      '12'
071200970526     C                   EXSR      F12GES
071300970526     C                   END
071400970526     C     FUNKEY        IFEQ      '24'
071500970526     C                   EXSR      F24GES
071600970526     C                   END
071700970526     C                   END
071800970526     C*
071900970526     C                   ENDSR
072000970526     C*------------------------------------------------------------------------*
072100970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
072200970526     C*------------------------------------------------------------------------*
072300970526     C     FUNVAR        BEGSR
072400970526     C*
072500970526     C* INIZIALIZZA VARIABILE -FUNKEY-
072600970526     C*
072700970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
072800970526     C     KEY           IFEQ      ENTER
072900970526     C                   MOVEL     *BLANKS       FUNKEY
073000970526     C                   END
073100970526     C     KEY           IFEQ      F1
073200970526     C                   MOVEL     '01'          FUNKEY
073300970526     C                   END
073400970526     C     KEY           IFEQ      F2
073500970526     C                   MOVEL     '02'          FUNKEY
073600970526     C                   END
073700970526     C     KEY           IFEQ      F3
073800970526     C                   MOVEL     '03'          FUNKEY
073900970526     C                   END
074000970526     C     KEY           IFEQ      F4
074100970526     C                   MOVEL     '04'          FUNKEY
074200970526     C                   END
074300970526     C     KEY           IFEQ      F5
074400970526     C                   MOVEL     '05'          FUNKEY
074500970526     C                   END
074600970526     C     KEY           IFEQ      F6
074700970526     C                   MOVEL     '06'          FUNKEY
074800970526     C                   END
074900970526     C     KEY           IFEQ      F7
075000970526     C                   MOVEL     '07'          FUNKEY
075100970526     C                   END
075200970526     C     KEY           IFEQ      F8
075300970526     C                   MOVEL     '08'          FUNKEY
075400970526     C                   END
075500970526     C     KEY           IFEQ      F9
075600970526     C                   MOVEL     '09'          FUNKEY
075700970526     C                   END
075800970526     C     KEY           IFEQ      F10
075900970526     C                   MOVEL     '10'          FUNKEY
076000970526     C                   END
076100970526     C     KEY           IFEQ      F11
076200970526     C                   MOVEL     '11'          FUNKEY
076300970526     C                   END
076400970526     C     KEY           IFEQ      F12
076500970526     C                   MOVEL     '12'          FUNKEY
076600970526     C                   END
076700970526     C     KEY           IFEQ      F13
076800970526     C                   MOVEL     '13'          FUNKEY
076900970526     C                   END
077000970526     C     KEY           IFEQ      F14
077100970526     C                   MOVEL     '14'          FUNKEY
077200970526     C                   END
077300970526     C     KEY           IFEQ      F15
077400970526     C                   MOVEL     '15'          FUNKEY
077500970526     C                   END
077600970526     C     KEY           IFEQ      F16
077700970526     C                   MOVEL     '16'          FUNKEY
077800970526     C                   END
077900970526     C     KEY           IFEQ      F17
078000970526     C                   MOVEL     '17'          FUNKEY
078100970526     C                   END
078200970526     C     KEY           IFEQ      F18
078300970526     C                   MOVEL     '18'          FUNKEY
078400970526     C                   END
078500970526     C     KEY           IFEQ      F19
078600970526     C                   MOVEL     '19'          FUNKEY
078700970526     C                   END
078800970526     C     KEY           IFEQ      F20
078900970526     C                   MOVEL     '20'          FUNKEY
079000970526     C                   END
079100970526     C     KEY           IFEQ      F21
079200970526     C                   MOVEL     '21'          FUNKEY
079300970526     C                   END
079400970526     C     KEY           IFEQ      F22
079500970526     C                   MOVEL     '22'          FUNKEY
079600970526     C                   END
079700970526     C     KEY           IFEQ      F23
079800970526     C                   MOVEL     '23'          FUNKEY
079900970526     C                   END
080000970526     C     KEY           IFEQ      F24
080100970526     C                   MOVEL     '24'          FUNKEY
080200970526     C                   END
080300970526     C     KEY           IFEQ      ROLLUP
080400970526     C                   MOVEL     'RU'          FUNKEY
080500970526     C                   END
080600970526     C     KEY           IFEQ      HELP
080700970526     C                   MOVEL     'HP'          FUNKEY
080800970526     C                   END
080900970526     C                   ENDSR
081000970526     C*------------------------------------------------------------------------*
081100970526     C* NOTE COSTRUZIONE SCHIERE
081200970526     C*------------------------------------------------------------------------*
081300970526     C*
081400970526     C* -MSG-MESSAGGI
081500970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
081600970526     C* (NON TOCCARE MSG,1 E MSG,2)
081700970526     C*
081800970526     C* -CMD-COMANDI
081900970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
082000970526     C*
082100970526     C* -OPD-OPZIONI
082200970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
082300970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
082400970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
082500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
082600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
082700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
082800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
082900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
083000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
083100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
083200970526     C*
083300970526     C* -FUD-FUNZIONI
083400970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
083500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
083600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
083700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
083800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
083900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
084000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
084100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
084200970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
084300970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
084400970526** MSG - MESSAGGI
084500120927Tasto funzionale non gestito                                                   1
084600160217                                                                               2
084700160217                                                                               3
084800160217                                                                               4
084900160217                                                                               5
085000160217                                                                               6
085100160217                                                                               7
085200130108                                                                               8
085300120927                                                                               9
085400120927                                                                               10
085500120927                                                                               11
085600120927                                                                               12
085700120927                                                                               13
085800120927Data errata                                                                    14
085900140116                                                                               15
086000120927                                                                               16
086100120927                                                                               17
086200120927                                                                               18
086300120927                                                                               19
086400160217Esecuzione  conclusa - controllare l'esito                                     20
086500160217                                                                               21
086600160217                                                                               22
086700121108                                                                               23
086800121108                                                                               24
086900970526** FUD - FUNZIONI
087000970526                   -        (S/N)=N                        funz.01¬
087100970526                   -        (S/N)=N                        funz.02¬
087200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
087300970526                   -        (S/N)=N                        funz.04¬
087400970526                   -        (S/N)=N                        funz.05¬
087500991214F06=Conferma       -        (S/N)=S                        funz.06¬
087600160217                   -        (S/N)=S                        funz.07¬
087700970527                   -        (S/N)=N                        funz.08¬
087800970527                   -        (S/N)=N                        funz.09¬
087900970527                   -        (S/N)=N                        funz.10¬
088000970526                   -        (S/N)=N                        funz.11¬
088100970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
088200970526                   -        (S/N)=N                        funz.13¬
088300970526                   -        (S/N)=N                        funz.14¬
088400970526                   -        (S/N)=N                        funz.15¬
088500970526                   -        (S/N)=N                        funz.16¬
088600970526                   -        (S/N)=N                        funz.17¬
088700970526                   -        (S/N)=N                        funz.18¬
088800970526                   -        (S/N)=N                        funz.19¬
088900970526                   -        (S/N)=N                        funz.20¬
089000970526                   -        (S/N)=N                        funz.21¬
089100970526                   -        (S/N)=N                        funz.22¬
089200970526                   -        (S/N)=N                        funz.23¬
089300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
