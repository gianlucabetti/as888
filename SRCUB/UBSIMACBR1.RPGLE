000100970526     H*------------------------------------------------------------------------*
000200130110     H* Simulazione accorpamento bolle
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800120927     Fcnaco00f  IF   E           K DISK
000900130108     FUBSIMACBD1cf   e             WORKSTN INFDS(DEVDS1)
001000130108     F                                     SFILE(SIMACBDMS:MSGULT)
001100130109     Fqsysprt   O    f  132        PRINTER usropn
001200130109     F                                     oflind(*inoa)
001300970526     D*------------------------------------------------------------------------*
001400970526     D* SCHIERE
001500970526     D*------------------------------------------------------------------------*
001600970526     D* MESSAGGI
001700970526     D*------------
001800000807     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
001900970526     D*------------
002000970526     D* DESCRIZIONI TASTI FUNZIONALI
002100970526     D*------------
002200970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002300970526     D*------------
002400970526     D* FUNZIONI DA CARICARE A VIDEO
002500970526     D*------------
002600970526     D FUV             S             19    DIM(24)
002700970526     D*------------------------------------------------------------------------*
002800970526     D* INPUT
002900970526     D*------------------------------------------------------------------------*
003000970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003100970526     D*------------
003200970526     D FUDDS           DS
003300970526     D  FUDDE1                 1      1
003400970526     D  FUDFUN                 2      3
003500970526     D  FUDDE2                 4     19
003600970526     D  FUDFI0                20     34
003700970526     D  FUDATT                35     35
003800970526     D  FUDFI1                36     80
003900970526     D  FUDDES                 1     19
004000970526     D  FUDRIG                 1     80
004100970526     D*------------
004200970526     D* DS GESTIONE TASTI FUNZIONALI
004300970526     D*------------
004400970526     D DSKEY         E DS
004500970526     D DEVDS1          DS
004600970526     D  KEY                  369    369
004700970526     D*------------
004800970526     D* DS "XMSG" - GESTIONE MESSAGGI
004900970526     D*------------
005000970526     D DSMSG         E DS
005100970528     D*------------------
005200970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
005300970528     D*------------------
005400970528     D WLBDA8          DS                  INZ
005500970528     D  G08DAT                 1      8  0
005600970528     D  G08INV                 9     16  0
005700970528     D  G08ERR                17     17
005800970528     D  G08TGI                18     22  0
005900970526     D*------------
006000970526     D* ARCHITETTURA
006100970526     D*------------
006200970526     D KPJBA         E DS
006300020923     D*-------------------
006400020923     D* DS REPERIMENTO DATI UTENTE
006500020923     D*-------------------
006600020923     D TIBS34DS      E DS                                                       *Profili utente
006700020923     D DDATIUTE      E DS                                                       *Dati utente
006800020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006900120927     D* DS REPERIMENTO PADRE/FIGLI
007000120927     D TIBS10DS      E DS
007100120928     D*-------------------
007200120928
007300121003     d Status         sds
007400121003     d  SDSpgm           *proc
007500121003     d  SDSprm           *parms
007600121003     d  SDSuser              254    263
007700121003
007800130123     d pIn_CCM1        s              7s 0
007900130123     d pIn_CCM2        s              7s 0
008000130123     d pIn_CCM3        s              7s 0
008100130123     d pIn_CCM4        s              7s 0
008200130123     d pIn_CCM5        s              7s 0
008300130108     d pIn_DA          s              8s 0
008400130108     d pIn_A           s              8s 0
008500130108     d pIn_BSE         s              1
008600130110     d pOut_Esito0     s              9b 0
008700130110     d pOut_Esito1     s              9b 0
008800130110     d pOut_Esito2     s              9b 0
008900130108     d pOut_0NRSPE     s              8s 0
009000130108     d pOut_0MDCOL     s              7s 2
009100130108     d pOut_0MDPES     s              9s 3
009200130108     d pOut_0MDVOL     s              5s 3
009300130108     d pOut_1NRSPE     s              8s 0
009400130108     d pOut_1MDCOL     s              7s 2
009500130108     d pOut_1MDPES     s              9s 3
009600130108     d pOut_1MDVOL     s              5s 3
009700130108     d pOut_2NRSPE     s              8s 0
009800130108     d pOut_2MDCOL     s              7s 2
009900130108     d pOut_2MDPES     s              9s 3
010000130108     d pOut_2MDVOL     s              5s 3
010100130108     d UBSIMACBR       pr                  extpgm('UBSIMACBR')
010200130123     d pIn_CCM1                       7s 0
010300130123     d pIn_CCM2                       7s 0
010400130123     d pIn_CCM3                       7s 0
010500130123     d pIn_CCM4                       7s 0
010600130123     d pIn_CCM5                       7s 0
010700130108     d pIn_DA                         8s 0
010800130108     d pIn_A                          8s 0
010900130108     d pIn_BSE                        1
011000130110     d pOut_Esito0                    9b 0
011100130110     d pOut_Esito1                    9b 0
011200130110     d pOut_Esito2                    9b 0
011300130108     d pOut_0NRSPE                    8s 0
011400130108     d pOut_0MDCOL                    7s 2
011500130108     d pOut_0MDPES                    9s 3
011600130108     d pOut_0MDVOL                    5s 3
011700130108     d pOut_1NRSPE                    8s 0
011800130108     d pOut_1MDCOL                    7s 2
011900130108     d pOut_1MDPES                    9s 3
012000130108     d pOut_1MDVOL                    5s 3
012100130108     d pOut_2NRSPE                    8s 0
012200130108     d pOut_2MDCOL                    7s 2
012300130108     d pOut_2MDPES                    9s 3
012400130108     d pOut_2MDVOL                    5s 3
012500130116
012600130116     d ora             s               t
012700120928
012800970526     C*------------------------------------------------------------------------*
012900000303     C* MAIN LINES
013000970526     C*------------------------------------------------------------------------*
013100970526     C*
013200120927     C* metto i default su D03
013300120927     C                   EXSR      InzD03
013400120927     C*
013500120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
013600991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
013700970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
013800970526     C     FINL00        OREQ      '1'                                          *FINE PGM
013900970526     C*
014000970526     C* EMETTE FORMATO VIDEO
014100970526     C                   EXSR      BUTFMR
014200970526     C*
014300970526     C* GESTIONE TASTI FUNZIONALI
014400970526     C                   EXSR      FUNCON
014500970526E   2C                   ENDDO
014600970526     C*
014700970526     C* FINE PGM
014800970526     C                   SETON                                        LR
014900970526     C*------------------------------------------------------------------------*
015000970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
015100970526     C*------------------------------------------------------------------------*
015200970526     C     BUTFMR        BEGSR
015300970526     C*
015400970526     C* EMETTE LA TESTATA
015500130108     C                   WRITE     SIMACBD01
015600970526     C*
015700970526     C* SPEDISCE EVENTUALE MESSAGGIO
015800970526     C                   EXSR      SNDMSG
015900970526     C*
016000970526     C* EMETTE LE FUNZIONI
016100130108     C                   WRITE     SIMACBD02
016200970526     C*
016300970526     C* EMETTE IL VIDEO
016400130108     C                   EXFMT     SIMACBD03
016500970526     C*
016600991214     C* REIMPOSTA FLAG DI ERRORE
016700991214     C                   MOVEL     '0'           ERRL00
016800970526     C*
016900970526     C* REIMPOSTA INDICATORI DI ERRORE
017000020611     C                   SETOFF                                       555556
017100970526     C*
017200970526     C                   ENDSR
017300970526     C*------------------------------------------------------------------------*
017400120927     C* InzD03 - Inizializzo D03
017500970526     C*------------------------------------------------------------------------*
017600120927     C     InzD03        BEGSR
017700120927     C*
017800130108     C*
017900120927     C                   ENDSR
018000120927     C*------------------------------------------------------------------------*
018100120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
018200120927     C*------------------------------------------------------------------------*
018300120927     C     SNDMSG        BEGSR
018400120927     C*
018500970526     C* PULISCE IL SFILE MESSAGGI
018600970526     C                   MOVEL     '1'           *IN40
018700130108     C                   WRITE     SIMACBDMC
018800970526     C                   MOVEL     '0'           *IN40
018900970526     C*
019000970526     C* VISUALIZZA IL MESSAGGIO
019100970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
019200970526     C                   CALL      'XMSG'        XMSGPA
019300020611     C     DSMSER        IFEQ      *ZEROS
019400970526     C                   MOVEL     DSMSKE        MSGKEY
019500020611     C                   ENDIF
019600970526     C                   Z-ADD     1             MSGULT
019700130108     C                   WRITE     SIMACBDMS
019800130108     C                   WRITE     SIMACBDMC
019900970526     C*
020000970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
020100970526     C                   MOVEL     *BLANKS       DSMSMS
020200970526     C*
020300970526     C                   ENDSR
020400970526     C*------------------------------------------------------------------------*
020500970526     C* ENTGES - GESTIONE ENTER -> INVIO
020600970526     C*------------------------------------------------------------------------*
020700970526     C     ENTGES        BEGSR
020800970526     C*
020900970526     C* CONTROLLA I CAMPI IMMESSI
021000970526     C                   EXSR      DECODI
021100970526     C*
021200970526     C                   ENDSR
021300970526     C*------------------------------------------------------------------------*
021400970526     C* F03GES - GESTIONE F03 -> FINE
021500970526     C*------------------------------------------------------------------------*
021600970526     C     F03GES        BEGSR
021700970526     C*
021800970526     C* IMPOSTA "PREMUTO F03"
021900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
022000970526     C*
022100970526     C                   ENDSR
022200970526     C*------------------------------------------------------------------------*
022300991214     C* F06GES - GESTIONE F06 -> CONFERMA
022400970526     C*------------------------------------------------------------------------*
022500970526     C     F06GES        BEGSR
022600970526     C*
022700991214     C* CONTROLLA I CAMPI IMMESSI
022800991214     C                   EXSR      DECODI
022900020611     C*
023000991214     C* ESEGUE ROUTINE DI CONFERMA
023100020611     C     ERRL00        IFEQ      '0'
023200991214     C                   EXSR      ESEGUI
023300130110     C* il messaggio di simulazione conclusa lo do sempre, a prescindere dagli esiti
023400121109     C                   MOVEL     MSG(20)       DSMSMS
023500121109     C* non esco mai dal pgm
023600121109     C***                MOVEL     '1'           FINL00
023700121108     C                   ENDIF
023800970526     C*
023900991214     C                   ENDSR
024000970526     C*------------------------------------------------------------------------*
024100130109     C* F07GES - GESTIONE F07 -> STAMPA
024200970526     C*------------------------------------------------------------------------*
024300130109     C     F07GES        BEGSR
024400970526     C*
024500130110     C* eseguo la conferma
024600130110     C                   EXSR      F06Ges
024700130110     C* la stampa la faccio a prescindere dagli esiti, solo se non ci sono errori a video
024800130110     C     ERRL00        IFEQ      '0'
024900130110     C*
025000130109     C                   if        not %open(qsysprt)
025100130109     C                   open      qsysprt
025200130116     C                   eval      ora = %time()
025300130116     C                   IF        VIDBSE = *blank
025400130116     C                   EVAL      VIDBSE = 'N'
025500130116     C                   ENDIF
025600130109     C                   except    testdett
025700130116     C                   IF        VIDBSE = 'N'
025800130116     C                   EVAL      VIDBSE = *blank
025900130116     C                   ENDIF
026000130109     C                   endif
026100130109     C*
026200130109     C                   if        %open(qsysprt)
026300130109     C                   except    findett
026400130109     C                   endif
026500130109     C*
026600130109     C                   if        %open(qsysprt)
026700130109     C                   close     qsysprt
026800130109     C                   endif
026900970526     C*
027000130110     C                   MOVEL     MSG(22)       DSMSMS
027100130110     C                   ENDIF
027200130110     C*
027300970526     C                   ENDSR
027400130109     C*------------------------------------------------------------------------*
027500130109     C* F12GES - GESTIONE F12 -> RITORNO
027600130109     C*------------------------------------------------------------------------*
027700130109     C     F12GES        BEGSR
027800130109     C*
027900130109     C* IMPOSTA "PREMUTO F12"
028000130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
028100130109     C*
028200130109     C                   ENDSR
028300970526     C*------------------------------------------------------------------------*
028400970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
028500970526     C*------------------------------------------------------------------------*
028600970526     C     F24GES        BEGSR
028700970526     C*
028800970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
028900970526     C*
029000970526     C                   ENDSR
029100970526     C*------------------------------------------------------------------------*
029200991214     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
029300970526     C*------------------------------------------------------------------------*
029400991214     C     ESEGUI        BEGSR
029500121108     C*
029600130123     C                   EVAL      pIn_CCM1    = VIDCCM1
029700130123     C                   EVAL      pIn_CCM2    = VIDCCM2
029800130123     C                   EVAL      pIn_CCM3    = VIDCCM3
029900130123     C                   EVAL      pIn_CCM4    = VIDCCM4
030000130123     C                   EVAL      pIn_CCM5    = VIDCCM5
030100130108     C                   EVAL      pIn_BSE     = VIDBSE
030200130108     c                   EVAL      pIn_Da      = %dec(%date(VIDDa:*eur):*iso)
030300130108     c                   EVAL      pIn_A       = %dec(%date(VIDA:*eur):*iso)
030400121108     C* chiamo
030500130108     C                   CALL      'UBSIMACBR'
030600130123     C                   PARM                    pIn_CCM1
030700130123     C                   PARM                    pIn_CCM2
030800130123     C                   PARM                    pIn_CCM3
030900130123     C                   PARM                    pIn_CCM4
031000130123     C                   PARM                    pIn_CCM5
031100130108     C                   PARM                    pIn_Da
031200130108     C                   PARM                    pIn_A
031300130108     c                   PARM                    pIn_BSE
031400130110     c                   parm                    pOut_Esito0
031500130110     c                   parm                    pOut_Esito1
031600130110     c                   parm                    pOut_Esito2
031700130108     c                   parm                    pOut_0NRSPE
031800130108     c                   parm                    pOut_0MDCOL
031900130108     c                   parm                    pOut_0MDPES
032000130108     c                   parm                    pOut_0MDVOL
032100130108     c                   parm                    pOut_1NRSPE
032200130108     c                   parm                    pOut_1MDCOL
032300130108     c                   parm                    pOut_1MDPES
032400130108     c                   parm                    pOut_1MDVOL
032500130108     c                   parm                    pOut_2NRSPE
032600130108     c                   parm                    pOut_2MDCOL
032700130108     c                   parm                    pOut_2MDPES
032800130108     c                   parm                    pOut_2MDVOL
032900121108     C*
033000130116     c                   CLEAR                   VIDEsito0
033100130116     c                   IF        pOut_Esito0 < 0 and
033200130116     c                             pOut_Esito0 <> -305
033300130116     c                   EVAL      VIDEsito0  = 'ERR'
033400130116     c                   ENDIF
033500130116     c                   CLEAR                   VIDEsito1
033600130116     c                   IF        pOut_Esito1 < 0 and
033700130116     c                             pOut_Esito1 <> -305
033800130116     c                   EVAL      VIDEsito1  = 'ERR'
033900130116     c                   ENDIF
034000130116     c                   CLEAR                   VIDEsito2
034100130116     c                   IF        pOut_Esito2 < 0 and
034200130116     c                             pOut_Esito2 <> -305
034300130116     c                   EVAL      VIDEsito2  = 'ERR'
034400130116     c                   ENDIF
034500130108     c                   EVAL      VID0NRSPE  = pOut_0NRSPE
034600130108     c                   EVAL      VID0MDCOL  = pOut_0MDCOL
034700130108     c                   EVAL      VID0MDPES  = pOut_0MDPES
034800130108     c                   EVAL      VID0MDVOL  = pOut_0MDVOL
034900130108     c                   EVAL      VID1NRSPE  = pOut_1NRSPE
035000130108     c                   EVAL      VID1MDCOL  = pOut_1MDCOL
035100130108     c                   EVAL      VID1MDPES  = pOut_1MDPES
035200130108     c                   EVAL      VID1MDVOL  = pOut_1MDVOL
035300130108     c                   EVAL      VID2NRSPE  = pOut_2NRSPE
035400130108     c                   EVAL      VID2MDCOL  = pOut_2MDCOL
035500130108     c                   EVAL      VID2MDPES  = pOut_2MDPES
035600130108     c                   EVAL      VID2MDVOL  = pOut_2MDVOL
035700991214     C*
035800970526     C                   ENDSR
035900970526     C*------------------------------------------------------------------------*
036000991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
036100970526     C*------------------------------------------------------------------------*
036200970526     C     DECODI        BEGSR
036300121019     C*
036400120927     C                   SETOFF                                       505152
036500120927     C                   SETOFF                                       535455
036600130123     C                   SETOFF                                       56
036700130123     C                   MOVEL     *BLANKS       VDDCCM1
036800130123     C                   MOVEL     *BLANKS       VDDCCM2
036900130123     C                   MOVEL     *BLANKS       VDDCCM3
037000130123     C                   MOVEL     *BLANKS       VDDCCM4
037100130123     C                   MOVEL     *BLANKS       VDDCCM5
037200000306     C*
037300120927     C* Ctrl i campi obbligatori
037400120927     C     ERRL00        IFEQ      '0'
037500120927     C                   SELECT
037600130123     C* almeno un cod.cliente
037700130123     C                   WHEN      VIDCCM1 = 0 and
037800130123     C                             VIDCCM2 = 0 and
037900130123     C                             VIDCCM3 = 0 and
038000130123     C                             VIDCCM4 = 0 and
038100130123     C                             VIDCCM5 = 0
038200121108     C                   MOVEL     '1'           ERRL00
038300130123     C                   SETON                                        515253
038400130123     C                   SETON                                        5455
038500121108     C                   MOVEL     MSG(6)        DSMSMS
038600120927     C                   ENDSL
038700120927     C                   ENDIF
038800121001     C*
038900130123     C* Cod. cliente 1 mittente esistente su CNACO
039000121001     C     ERRL00        IFEQ      '0'
039100130123     C                   IF        VIDCCM1 > 0
039200120927     C                   Z-ADD     1             ACOkut
039300120927     C                   Z-ADD     DUTKCI        ACOkcc
039400130123     C                   Z-ADD     VIDCCM1       ACOksc
039500120927     C     K03ACO00      CHAIN     cnaco00f
039600120927     C                   IF        %found(cnaco00f)
039700130123     C                   MOVEL     ACOrag        VDDCCM1
039800120927     C                   ELSE
039900120927     C                   MOVEL     '1'           ERRL00
040000130108     C                   SETON                                        51
040100120927     C                   MOVEL     MSG(3)        DSMSMS
040200130123     C                   MOVEL     *BLANKS       VDDCCM1
040300120927     C                   ENDIF
040400130123     C                   ENDIF
040500120927     C                   ENDIF
040600130123     C*
040700130123     C* Cod. cliente 2 mittente esistente su CNACO
040800130123     C     ERRL00        IFEQ      '0'
040900130123     C                   IF        VIDCCM2 > 0
041000130123     C                   Z-ADD     1             ACOkut
041100130123     C                   Z-ADD     DUTKCI        ACOkcc
041200130123     C                   Z-ADD     VIDCCM2       ACOksc
041300130123     C     K03ACO00      CHAIN     cnaco00f
041400130123     C                   IF        %found(cnaco00f)
041500130123     C                   MOVEL     ACOrag        VDDCCM2
041600130123     C                   ELSE
041700130123     C                   MOVEL     '1'           ERRL00
041800130123     C                   SETON                                        52
041900130123     C                   MOVEL     MSG(3)        DSMSMS
042000130123     C                   MOVEL     *BLANKS       VDDCCM2
042100130123     C                   ENDIF
042200130123     C                   ENDIF
042300130123     C                   ENDIF
042400130123     C*
042500130123     C* Cod. cliente 3 mittente esistente su CNACO
042600130123     C     ERRL00        IFEQ      '0'
042700130123     C                   IF        VIDCCM3 > 0
042800130123     C                   Z-ADD     1             ACOkut
042900130123     C                   Z-ADD     DUTKCI        ACOkcc
043000130123     C                   Z-ADD     VIDCCM3       ACOksc
043100130123     C     K03ACO00      CHAIN     cnaco00f
043200130123     C                   IF        %found(cnaco00f)
043300130123     C                   MOVEL     ACOrag        VDDCCM3
043400130123     C                   ELSE
043500130123     C                   MOVEL     '1'           ERRL00
043600130123     C                   SETON                                        53
043700130123     C                   MOVEL     MSG(3)        DSMSMS
043800130123     C                   MOVEL     *BLANKS       VDDCCM3
043900130123     C                   ENDIF
044000130123     C                   ENDIF
044100130123     C                   ENDIF
044200130123     C*
044300130123     C* Cod. cliente 4 mittente esistente su CNACO
044400130123     C     ERRL00        IFEQ      '0'
044500130123     C                   IF        VIDCCM4 > 0
044600130123     C                   Z-ADD     1             ACOkut
044700130123     C                   Z-ADD     DUTKCI        ACOkcc
044800130123     C                   Z-ADD     VIDCCM4       ACOksc
044900130123     C     K03ACO00      CHAIN     cnaco00f
045000130123     C                   IF        %found(cnaco00f)
045100130123     C                   MOVEL     ACOrag        VDDCCM4
045200130123     C                   ELSE
045300130123     C                   MOVEL     '1'           ERRL00
045400130123     C                   SETON                                        54
045500130123     C                   MOVEL     MSG(3)        DSMSMS
045600130123     C                   MOVEL     *BLANKS       VDDCCM4
045700130123     C                   ENDIF
045800130123     C                   ENDIF
045900130123     C                   ENDIF
046000130123     C*
046100130123     C* Cod. cliente 5 mittente esistente su CNACO
046200130123     C     ERRL00        IFEQ      '0'
046300130123     C                   IF        VIDCCM5 > 0
046400130123     C                   Z-ADD     1             ACOkut
046500130123     C                   Z-ADD     DUTKCI        ACOkcc
046600130123     C                   Z-ADD     VIDCCM5       ACOksc
046700130123     C     K03ACO00      CHAIN     cnaco00f
046800130123     C                   IF        %found(cnaco00f)
046900130123     C                   MOVEL     ACOrag        VDDCCM5
047000130123     C                   ELSE
047100130123     C                   MOVEL     '1'           ERRL00
047200130123     C                   SETON                                        55
047300130123     C                   MOVEL     MSG(3)        DSMSMS
047400130123     C                   MOVEL     *BLANKS       VDDCCM5
047500130123     C                   ENDIF
047600130123     C                   ENDIF
047700130123     C                   ENDIF
047800130108     C*
047900130108     C* Data DA formalmente valida
048000130108     C     ERRL00        IFEQ      '0'
048100130108     C     *eur          TEST(D)                 VIDDA                  50
048200130108     C                   IF        *in50 = *on
048300130108     C                   MOVEL     '1'           ERRL00
048400130108     C                   MOVEL     MSG(14)       DSMSMS
048500130108     C                   ELSE
048600130108     c                   EVAL      pIn_Da      = %dec(%date(VIDDa:*eur):*iso)
048700130108     C                   ENDIF
048800130108     C                   ENDIF
048900130108     C*
049000130108     C* Data A  formalmente valida
049100130108     C     ERRL00        IFEQ      '0'
049200130123     C     *eur          TEST(D)                 VIDA                   56
049300130123     C                   IF        *in56 = *on
049400130108     C                   MOVEL     '1'           ERRL00
049500130108     C                   MOVEL     MSG(14)       DSMSMS
049600130108     C                   ELSE
049700130108     c                   EVAL      pIn_A       = %dec(%date(VIDA:*eur):*iso)
049800130108     C                   ENDIF
049900130108     C                   ENDIF
050000130108     C*
050100130108     C* Data A >= Data Da
050200130108     C     ERRL00        IFEQ      '0'
050300130108     C                   IF        pIn_A < pIn_Da
050400130123     C                   SETON                                        5056
050500130108     C                   MOVEL     '1'           ERRL00
050600130108     C                   MOVEL     MSG(15)       DSMSMS
050700130108     C                   ENDIF
050800130108     C                   ENDIF
050900120927     C*
051000970528     C                   ENDSR
051100970526     C*------------------------------------------------------------------------*
051200970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
051300970526     C*------------------------------------------------------------------------*
051400970526     C     AZZVID        BEGSR
051500970526     C* CAMPI
051600970526     C*
051700970526     C                   ENDSR
051800970526     C*------------------------------------------------------------------------*
051900020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
052000970526     C*------------------------------------------------------------------------*
052100970526     C     IMPFUN        BEGSR
052200970526     C*
052300970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
052400970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
052500970526     C                   EXSR      FUNGES
052600970526     C*
052700970526     C                   ENDSR
052800020923     C*--------------------------------------------------------------------------------------------*
052900020923     C* REPERISCE I DATI UTENTE
053000020923     C*--------------------------------------------------------------------------------------------*
053100020923     C     REPDATIUTE    BEGSR
053200020923     C*
053300020923     C* INIZIALIZZA VARIABILI DI WRK
053400020923     C                   CLEAR                   TIBS34DS
053500020923     C                   CLEAR                   AZUTEDS
053600020923     C                   CLEAR                   DDATIUTE
053700020923     C*
053800020923     C     *DTAARA       DEFINE    §azute        azuteds
053900020923     C     *DTAARA       DEFINE    §datiute      ddatiute
054000020923     C                   IN(E)     *DTAARA
054100020923if  1C                   IF        %Error
054200020923     c                   EVAL      I34Tla = 'L'
054300020923     C                   CALL      'TIBS34R'
054400020923     C                   PARM                    Tibs34Ds
054500020923     C                   IN        *DTAARA
054600020923e   1C                   ENDIF
054700020923     C*
054800020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
054900020923     C                   MOVEL(P)  RSUT          DSFIRS           20
055000020923     C*
055100020923     C                   ENDSR
055200970526     C*------------------------------------------------------------------------*
055300970526     C* *INZSR - OPERAZIONI INIZIALI
055400970526     C*------------------------------------------------------------------------*
055500970526     C     *INZSR        BEGSR
055600991217     C*
055700991217     C     *ENTRY        PLIST
055800991217     C                   PARM                    KPJBA
055900120927     C*
056000120927     C* LETTURA cnaco00f -
056100120927     C     K03ACO00      KLIST
056200120927     C                   KFLD                    ACOkut
056300120927     C                   KFLD                    ACOkcc
056400120927     C                   KFLD                    ACOksc
056500020923     C*
056600020923     C* REPERISCE I DATI DELL'UTENTE
056700020923     C                   EXSR      REPDATIUTE
056800970526     C*
056900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
057000970526     C     XMSGPA        PLIST
057100970526     C                   PARM                    DSMSMS
057200970526     C                   PARM                    DSMSPG
057300970526     C                   PARM                    DSMSKE
057400970526     C                   PARM                    DSMSER
057500970526     C*
057600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
057700970526     C                   CALL      'XKEY'
057800970526     C                   PARM                    DSKEY
057900970526     C*---
058000970526     C* VARIABILI NON RIFERITE AL DATA BASE
058100970526     C*---
058200970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
058300991214     C                   MOVEL     *BLANKS       DEPOP1            3
058400970526     C*---
058500970526     C* VARIABILI DI CONTROLLO
058600970526     C*---
058700970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
058800970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
058900970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
059000970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
059100970526     C*
059200970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
059300130108     C                   MOVEL     'UBSIMACBR'   DSMSPG
059400130108     C                   MOVEL     'UBSIMACBR'   MSGQUE
059500970526     C                   MOVEL     *BLANKS       MSGKEY
059600970526     C                   MOVEL     *BLANKS       DSMSMS
059700970528     C*
059800970528     C* CALCOLA LA DATA CORRENTE
059900970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
060000970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
060100970528     C                   Z-ADD     WN8           G08DAT
060200970528     C                   Z-ADD     *ZEROS        G08INV
060300970528     C                   MOVEL     '0'           G08ERR
060400970528     C                   CALL      'XSRDA8'
060500970528     C                   PARM                    WLBDA8
060600970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
060700000310     C*
060800000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
060900000310     C                   EXSR      AZZVID
061000970526     C*
061100970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
061200970526     C                   EXSR      IMPFUN
061300970526     C*
061400970526     C                   ENDSR
061500970526     C*------------------------------------------------------------------------*
061600970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
061700970526     C*------------------------------------------------------------------------*
061800970526     C     FUNGES        BEGSR
061900970526     C*
062000970526     C* PULISCO CAMPI DI LAVORO
062100970526     C                   Z-ADD     0             W                 5 0
062200970526     C*
062300970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
062400970526     C* E CARICO TABELLA FUNZIONI ATTIVE
062500970526     C     1             DO        24            I
062600970526     C                   MOVEL     FUD(I)        FUDRIG
062700970526     C*
062800970526     C* PULIZIA FUNZIONI PRECEDENTI
062900970526     C                   MOVEL     *BLANKS       FUV(I)
063000970526     C*
063100970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
063200970526     C* (SALTA F24)
063300970526     C     FUDATT        IFEQ      'S'
063400970526     C     FUDFUN        ANDNE     '24'
063500970526     C                   ADD       1             W
063600970526     C                   MOVEL     FUDDES        FUV(W)
063700970526     C                   END
063800970526     C                   END                                                    DO WHILE
063900970526     C*
064000970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
064100970526     C* ALTRIMENTI DISATTIVO F24
064200970526     C     1             DO        24            I
064300970526     C                   MOVEL     FUD(I)        FUDRIG
064400970526     C*
064500970526     C     FUDFUN        IFEQ      '24'
064600970526     C     W             IFGT      6
064700970526     C                   MOVEL     'S'           FUDATT
064800970526     C                   ELSE
064900970526     C                   MOVEL     *BLANKS       FUDATT
065000970526     C                   END                                                    W GT 8
065100970526     C                   MOVEL     FUDRIG        FUD(I)
065200970526     C                   END
065300970526     C                   END                                                    DO WHILE
065400970526     C*
065500970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
065600970526     C     W             DIV       6             I
065700970526     C                   MVR                     Z
065800970526     C*
065900970526     C     Z             IFNE      0
066000970526     C     I             ADD       1             Z
066100970526     C                   ELSE
066200970526     C                   Z-ADD     I             Z                 5 0
066300970526     C                   END
066400970526     C*
066500970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
066600970526     C                   ADD       1             FUNGRU
066700970526     C     FUNGRU        IFGT      Z
066800970526     C     FUNGRU        ORLT      1
066900970526     C                   Z-ADD     1             FUNGRU
067000970526     C                   END
067100970526     C*
067200970526     C* PRIMO GRUPPO DI 6 FUNZIONI
067300970526     C     FUNGRU        IFEQ      1
067400970526     C                   MOVEL     FUV(1)        FUN001
067500970526     C                   MOVEL     FUV(2)        FUN002
067600970526     C                   MOVEL     FUV(3)        FUN003
067700970526     C                   MOVEL     FUV(4)        FUN004
067800970526     C                   MOVEL     FUV(5)        FUN005
067900970526     C                   MOVEL     FUV(6)        FUN006
068000970526     C                   END
068100970526     C*
068200970526     C* SECONDO GRUPPO DI 6 FUNZIONI
068300970526     C     FUNGRU        IFEQ      2
068400970526     C                   MOVEL     FUV(7)        FUN001
068500970526     C                   MOVEL     FUV(8)        FUN002
068600970526     C                   MOVEL     FUV(9)        FUN003
068700970526     C                   MOVEL     FUV(10)       FUN004
068800970526     C                   MOVEL     FUV(11)       FUN005
068900970526     C                   MOVEL     FUV(12)       FUN006
069000970526     C                   END
069100970526     C*
069200970526     C* TERZO GRUPPO DI 6 FUNZIONI
069300970526     C     FUNGRU        IFEQ      3
069400970526     C                   MOVEL     FUV(13)       FUN001
069500970526     C                   MOVEL     FUV(14)       FUN002
069600970526     C                   MOVEL     FUV(15)       FUN003
069700970526     C                   MOVEL     FUV(16)       FUN004
069800970526     C                   MOVEL     FUV(17)       FUN005
069900970526     C                   MOVEL     FUV(18)       FUN006
070000970526     C                   END
070100970526     C*
070200970526     C* QUARTO GRUPPO DI 6 FUNZIONI
070300970526     C     FUNGRU        IFEQ      4
070400970526     C                   MOVEL     FUV(19)       FUN001
070500970526     C                   MOVEL     FUV(20)       FUN002
070600970526     C                   MOVEL     FUV(21)       FUN003
070700970526     C                   MOVEL     FUV(22)       FUN004
070800970526     C                   MOVEL     *BLANKS       FUN005
070900970526     C                   MOVEL     *BLANKS       FUN006
071000970526     C                   END
071100970526     C*
071200970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
071300970526     C                   MOVEL     *BLANKS       FUN007
071400970526     C                   MOVEL     *BLANKS       FUN008
071500970526     C*
071600970526     C     1             DO        24            I
071700970526     C                   MOVEL     FUD(I)        FUDRIG
071800970526     C     FUDFUN        IFEQ      '24'
071900970526     C     FUDATT        ANDEQ     'S'
072000970526     C                   MOVEL     FUDDES        FUN008
072100970526     C                   END
072200970526     C                   END                                                    DO
072300970526     C*
072400970526     C                   ENDSR
072500970526     C*------------------------------------------------------------------------*
072600970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
072700970526     C*------------------------------------------------------------------------*
072800970526     C     FUNCON        BEGSR
072900970526     C*
073000970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
073100970526     C                   EXSR      FUNVAR
073200970526     C*
073300970526     C* PULISCO CAMPI DI LAVORO
073400970526     C                   Z-ADD     0             I                 5 0
073500970526     C                   Z-ADD     0             W                 5 0
073600970526     C*
073700970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
073800970526     C     FUNKEY        IFNE      *BLANKS
073900970526     C     1             DO        24            I
074000970526     C                   MOVEL     FUD(I)        FUDRIG
074100970526     C*
074200970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
074300970526     C     FUNKEY        IFEQ      FUDFUN
074400970526     C     FUDATT        ANDEQ     'S'
074500970526     C                   Z-ADD     I             W
074600970526     C                   END
074700970526     C                   END                                                    DO WHILE
074800970526     C*
074900970526     C* FUNZIONE NON ATTIVA (ERRORE)
075000970526     C     W             IFEQ      0
075100970526     C*
075200970526     C* EMISSIONE MESSAGGIO DI ERRORE
075300970526     C                   MOVEL     MSG(1)        DSMSMS
075400970526     C*
075500970526     C                   END                                                    W=0
075600970526     C                   END                                                    FUNKEY NE *BLANKS
075700970526     C*
075800970526     C* FUNZIONE ATTIVA (PROCEDI ...)
075900970526     C     W             IFNE      0
076000970526     C     FUNKEY        OREQ      *BLANKS
076100970526     C     FUNKEY        IFEQ      *BLANKS
076200970526     C                   EXSR      ENTGES
076300970526     C                   END
076400970526     C     FUNKEY        IFEQ      '03'
076500970526     C                   EXSR      F03GES
076600970526     C                   END
076700970526     C     FUNKEY        IFEQ      '06'
076800970526     C                   EXSR      F06GES
076900970526     C                   END
077000130109     C     FUNKEY        IFEQ      '07'
077100130109     C                   EXSR      F07GES
077200130109     C                   END
077300970526     C     FUNKEY        IFEQ      '12'
077400970526     C                   EXSR      F12GES
077500970526     C                   END
077600970526     C     FUNKEY        IFEQ      '24'
077700970526     C                   EXSR      F24GES
077800970526     C                   END
077900970526     C                   END
078000970526     C*
078100970526     C                   ENDSR
078200970526     C*------------------------------------------------------------------------*
078300970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
078400970526     C*------------------------------------------------------------------------*
078500970526     C     FUNVAR        BEGSR
078600970526     C*
078700970526     C* INIZIALIZZA VARIABILE -FUNKEY-
078800970526     C*
078900970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
079000970526     C     KEY           IFEQ      ENTER
079100970526     C                   MOVEL     *BLANKS       FUNKEY
079200970526     C                   END
079300970526     C     KEY           IFEQ      F1
079400970526     C                   MOVEL     '01'          FUNKEY
079500970526     C                   END
079600970526     C     KEY           IFEQ      F2
079700970526     C                   MOVEL     '02'          FUNKEY
079800970526     C                   END
079900970526     C     KEY           IFEQ      F3
080000970526     C                   MOVEL     '03'          FUNKEY
080100970526     C                   END
080200970526     C     KEY           IFEQ      F4
080300970526     C                   MOVEL     '04'          FUNKEY
080400970526     C                   END
080500970526     C     KEY           IFEQ      F5
080600970526     C                   MOVEL     '05'          FUNKEY
080700970526     C                   END
080800970526     C     KEY           IFEQ      F6
080900970526     C                   MOVEL     '06'          FUNKEY
081000970526     C                   END
081100970526     C     KEY           IFEQ      F7
081200970526     C                   MOVEL     '07'          FUNKEY
081300970526     C                   END
081400970526     C     KEY           IFEQ      F8
081500970526     C                   MOVEL     '08'          FUNKEY
081600970526     C                   END
081700970526     C     KEY           IFEQ      F9
081800970526     C                   MOVEL     '09'          FUNKEY
081900970526     C                   END
082000970526     C     KEY           IFEQ      F10
082100970526     C                   MOVEL     '10'          FUNKEY
082200970526     C                   END
082300970526     C     KEY           IFEQ      F11
082400970526     C                   MOVEL     '11'          FUNKEY
082500970526     C                   END
082600970526     C     KEY           IFEQ      F12
082700970526     C                   MOVEL     '12'          FUNKEY
082800970526     C                   END
082900970526     C     KEY           IFEQ      F13
083000970526     C                   MOVEL     '13'          FUNKEY
083100970526     C                   END
083200970526     C     KEY           IFEQ      F14
083300970526     C                   MOVEL     '14'          FUNKEY
083400970526     C                   END
083500970526     C     KEY           IFEQ      F15
083600970526     C                   MOVEL     '15'          FUNKEY
083700970526     C                   END
083800970526     C     KEY           IFEQ      F16
083900970526     C                   MOVEL     '16'          FUNKEY
084000970526     C                   END
084100970526     C     KEY           IFEQ      F17
084200970526     C                   MOVEL     '17'          FUNKEY
084300970526     C                   END
084400970526     C     KEY           IFEQ      F18
084500970526     C                   MOVEL     '18'          FUNKEY
084600970526     C                   END
084700970526     C     KEY           IFEQ      F19
084800970526     C                   MOVEL     '19'          FUNKEY
084900970526     C                   END
085000970526     C     KEY           IFEQ      F20
085100970526     C                   MOVEL     '20'          FUNKEY
085200970526     C                   END
085300970526     C     KEY           IFEQ      F21
085400970526     C                   MOVEL     '21'          FUNKEY
085500970526     C                   END
085600970526     C     KEY           IFEQ      F22
085700970526     C                   MOVEL     '22'          FUNKEY
085800970526     C                   END
085900970526     C     KEY           IFEQ      F23
086000970526     C                   MOVEL     '23'          FUNKEY
086100970526     C                   END
086200970526     C     KEY           IFEQ      F24
086300970526     C                   MOVEL     '24'          FUNKEY
086400970526     C                   END
086500970526     C     KEY           IFEQ      ROLLUP
086600970526     C                   MOVEL     'RU'          FUNKEY
086700970526     C                   END
086800970526     C     KEY           IFEQ      HELP
086900970526     C                   MOVEL     'HP'          FUNKEY
087000970526     C                   END
087100970526     C                   ENDSR
087200970526     C*------------------------------------------------------------------------*
087300970526     C* NOTE COSTRUZIONE SCHIERE
087400970526     C*------------------------------------------------------------------------*
087500970526     C*
087600970526     C* -MSG-MESSAGGI
087700970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
087800970526     C* (NON TOCCARE MSG,1 E MSG,2)
087900970526     C*
088000970526     C* -CMD-COMANDI
088100970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
088200970526     C*
088300970526     C* -OPD-OPZIONI
088400970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
088500970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
088600970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
088700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
088800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
088900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
089000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
089100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
089200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
089300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
089400970526     C*
089500970526     C* -FUD-FUNZIONI
089600970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
089700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
089800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
089900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
090000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
090100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
090200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
090300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
090400970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
090500970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
090600970526     O*------------------------------------------------------------------------*
090700130110     Oqsysprt   E            testdett          2
090800130116     O                       KNSIF
090900130116     O                       SDSUSER             +2
091000130116     O                                           72 '* Simulazione accorpamento'
091100130116     O                                           +1 'bolle *'
091200130110     O                       G08DAT        Y    130
091300130116     O          E            testdett    1
091400130116     O                       SDSPGM
091500130116     O                       ORA                130
091600130116     O          E            testdett    4
091700130123     O                                              'Clienti'
091800130123     O                       VIDCCM1       Z     +1
091900130123     O                       VDDCCM1             +2
092000130123     O          E            testdett    1
092100130123     O                       VIDCCM2       Z     15
092200130123     O                       VDDCCM2             +2
092300130123     O          E            testdett    1
092400130123     O                       VIDCCM3       Z     15
092500130123     O                       VDDCCM3             +2
092600130123     O          E            testdett    1
092700130123     O                       VIDCCM4       Z     15
092800130123     O                       VDDCCM4             +2
092900130123     O          E            testdett    1
093000130123     O                       VIDCCM5       Z     15
093100130123     O                       VDDCCM5             +2
093200130123     O          E            testdett    1
093300130123     O                                              'Periodo'
093400130110     O                       VIDDA         Y     +1
093500130110     O                                           +1 '-'
093600130110     O                       VIDA          Y     +1
093700130110     O          E            testdett    1
093800130110     O                                              'Solo bolle con serie ='
093900130110     O                       VIDBSE              +1
094000130110     O          E            testdett    3
094100130117     O                                           47 'Nr. spedizioni'
094200130117     O                                           68 'Media colli/sped.'
094300130117     O                                           92 'Media peso kg./sped.'
094400130117     O                                          117 'Media volume m3/sped.'
094500130117     O                                          126 'Esito'
094600130109     O          E            findett     2
094700130117     O                                              'Rilevamento dati reali'
094800130117     O                       VID0NRSPE     1     45
094900130117     O                       VID0MDCOL     1     63
095000130117     O                       VID0MDPES     1     87
095100130117     O                       VID0MDVOL     1    112
095200130117     O                       VIDESITO0          126
095300130109     O          E            findett     2
095400130117     O                                              '1a sim. (NO accorpamento C'
095500130117     O                                              '/ASS)'
095600130117     O                       VID1NRSPE     1     45
095700130117     O                       VID1MDCOL     1     63
095800130117     O                       VID1MDPES     1     87
095900130117     O                       VID1MDVOL     1    112
096000130117     O                       VIDESITO1          126
096100130109     O          E            findett     2
096200130117     O                                              '2a sim. (SI accorpamento C'
096300130117     O                                              '/ASS)'
096400130117     O                       VID2NRSPE     1     45
096500130117     O                       VID2MDCOL     1     63
096600130117     O                       VID2MDPES     1     87
096700130117     O                       VID2MDVOL     1    112
096800130117     O                       VIDESITO2          126
096900970526** MSG - MESSAGGI
097000120927Tasto funzionale non gestito                                                   1
097100120927Numero chiamate obbligatorio                                                   2
097200120927Codice cliente non esiste su CNACO00F                                          3
097300130108                                                                               4
097400130108                                                                               5
097500130108Campo obbligatorio                                                             6
097600130108                                                                               7
097700130108                                                                               8
097800120927                                                                               9
097900120927                                                                               10
098000120927                                                                               11
098100120927                                                                               12
098200120927                                                                               13
098300120927Data errata                                                                    14
098400130108La data inizio deve essere > o = della data fine                               15
098500120927                                                                               16
098600120927                                                                               17
098700120927                                                                               18
098800120927                                                                               19
098900130110Simulazione conclusa - controllare gli esiti                                   20
099000120927Scelta obbligatoria                                                            21
099100130110Eseguita la stampa                                                             22
099200121108                                                                               23
099300121108                                                                               24
099400970526** FUD - FUNZIONI
099500970526                   -        (S/N)=N                        funz.01¬
099600970526                   -        (S/N)=N                        funz.02¬
099700970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
099800970526                   -        (S/N)=N                        funz.04¬
099900970526                   -        (S/N)=N                        funz.05¬
100000991214F06=Conferma       -        (S/N)=S                        funz.06¬
100100130110F07=Conf.+Stampa   -        (S/N)=S                        funz.07¬
100200970527                   -        (S/N)=N                        funz.08¬
100300970527                   -        (S/N)=N                        funz.09¬
100400970527                   -        (S/N)=N                        funz.10¬
100500970526                   -        (S/N)=N                        funz.11¬
100600970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
100700970526                   -        (S/N)=N                        funz.13¬
100800970526                   -        (S/N)=N                        funz.14¬
100900970526                   -        (S/N)=N                        funz.15¬
101000970526                   -        (S/N)=N                        funz.16¬
101100970526                   -        (S/N)=N                        funz.17¬
101200970526                   -        (S/N)=N                        funz.18¬
101300970526                   -        (S/N)=N                        funz.19¬
101400970526                   -        (S/N)=N                        funz.20¬
101500970526                   -        (S/N)=N                        funz.21¬
101600970526                   -        (S/N)=N                        funz.22¬
101700970526                   -        (S/N)=N                        funz.23¬
101800970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
