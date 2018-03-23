000100970526     H*------------------------------------------------------------------------*
000200991025     H* Gestione tipi file internet
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800991025     Ftis721d   cf   e             WORKSTN INFDS(DEVDS1)
000900991025     F                                     SFILE(s721DMS:MSGULT)
001000970527     F*------------
001100991025     Ftivtf01L  UF A E           K DISK
001200970526     D*------------------------------------------------------------------------*
001300970526     D* SCHIERE
001400970526     D*------------------------------------------------------------------------*
001500970526     D*------------
001600970526     D* DECODIFICHE
001700970526     D*------------
001800991014     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
001900970526     D*------------
002000970526     D* MESSAGGI
002100970526     D*------------
002200970527     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002300970526     D*------------
002400970526     D* DESCRIZIONI TASTI FUNZIONALI
002500970526     D*------------
002600970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002700970526     D*------------
002800970526     D* FUNZIONI DA CARICARE A VIDEO
002900970526     D*------------
003000970526     D FUV             S             19    DIM(24)
003100970526     D*------------------------------------------------------------------------*
003200970526     D* INPUT
003300970526     D*------------------------------------------------------------------------*
003400970526     D*------------
003500970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003600970526     D*------------
003700970526     D FUDDS           DS
003800970526     D  FUDDE1                 1      1
003900970526     D  FUDFUN                 2      3
004000970526     D  FUDDE2                 4     19
004100970526     D  FUDFI0                20     34
004200970526     D  FUDATT                35     35
004300970526     D  FUDFI1                36     80
004400970526     D  FUDDES                 1     19
004500970526     D  FUDRIG                 1     80
004600970526     D*------------
004700970526     D* DS GESTIONE TASTI FUNZIONALI
004800970526     D*------------
004900970526     D DSKEY         E DS
005000970526     D DEVDS1          DS
005100970526     D  KEY                  369    369
005200970526     D*------------
005300970526     D* DS "XMSG" - GESTIONE MESSAGGI
005400970526     D*------------
005500970526     D DSMSG         E DS
005600970526     D*------------
005700020906     D* TABELLE
005800020906     D*------------
005900991108     D TIBS02DS      E DS
006000970528     D*------------------
006100970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
006200970528     D*------------------
006300970528     D WLBDA8          DS                  INZ
006400970528     D  G08DAT                 1      8  0
006500970528     D  G08INV                 9     16  0
006600970528     D  G08ERR                17     17
006700970528     D  G08TGI                18     22  0
006800970526     D*------------
006900970526     D* DS DI PROCEDURA
007000970526     D*------------
007100991025     D tis720        E DS                  EXTNAME(tis720ds)
007200101013     D dvtfppi       E DS
007300970526     D*------------
007400970526     D* ARCHITETTURA
007500970526     D*------------
007600970526     D KPJBA         E DS
007700970526     C*------------------------------------------------------------------------*
007800970526     C* MAIN LINES
007900970526     C*------------------------------------------------------------------------*
008000970526     C*
008100970526     C* NON CI SONO ERRORI IN ENTRATA
008200970526IF  1C     ERRL00        IFEQ      '0'
008300970526     C*
008400970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
008500970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
008600970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
008700970526     C     FINL00        OREQ      '1'                                          *FINE PGM
008800970526     C*
008900970526     C* EMETTE FORMATO VIDEO
009000970526     C                   EXSR      BUTFMR
009100970526     C*
009200970526     C* GESTIONE TASTI FUNZIONALI
009300970526     C                   EXSR      FUNCON
009400970526E   2C                   ENDDO
009500970526     C*
009600970526     C* CI SONO ERRORI IN ENTRATA
009700970527X   1C                   ELSE
009800991025     C                   MOVEL     '1'           S20err
009900991025     C                   MOVEL     DSMSMS        S20msg
010000970526E   1C                   ENDIF
010100970526     C* FINE PGM
010200970526     C                   SETON                                        LR
010300970526     C*------------------------------------------------------------------------*
010400970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
010500970526     C*------------------------------------------------------------------------*
010600970526     C     BUTFMR        BEGSR
010700970526     C*
010800970526     C* EMETTE LA TESTATA
010900991025     C                   WRITE     s721D01
011000970526     C*
011100970526     C* SPEDISCE EVENTUALE MESSAGGIO
011200970526     C                   EXSR      SNDMSG
011300970526     C*
011400970526     C* EMETTE LE FUNZIONI
011500991025     C                   WRITE     s721D02
011600970526     C*
011700970526     C* EMETTE IL VIDEO
011800991025     C                   EXFMT     s721D03
011900970526     C*
012000970526     C* REIMPOSTA FLAG DI ERRORE
012100970526     C                   MOVEL     '0'           ERRL00
012200970526     C*
012300970526     C* REIMPOSTA INDICATORI DI ERRORE
012400970526     C                   SETOFF                                         5859
012500970526     C                   SETOFF                                       606162
012600970526     C                   SETOFF                                       636465
012700970526     C                   SETOFF                                       666768
012800970526     C                   SETOFF                                       697071
012900970526     C                   SETOFF                                       727374
013000970526     C                   SETOFF                                       757677
013100970526     C                   SETOFF                                       787980
013200970526     C                   SETOFF                                       818283
013300970526     C                   SETOFF                                       848586
013400970526     C                   SETOFF                                       878889
013500001120     C                   SETOFF                                       555657
013600970526     C*
013700970526     C                   ENDSR
013800970526     C*------------------------------------------------------------------------*
013900970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
014000970526     C*------------------------------------------------------------------------*
014100970526     C     SNDMSG        BEGSR
014200970526     C*
014300970526     C* PULISCE IL SFILE MESSAGGI
014400970526     C                   MOVEL     '1'           *IN40
014500991025     C                   WRITE     s721DMC
014600970526     C                   MOVEL     '0'           *IN40
014700970526     C*
014800970526     C* VISUALIZZA IL MESSAGGIO
014900970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
015000970526     C                   CALL      'XMSG'        XMSGPA
015100970526IF  1C     DSMSER        IFEQ      *ZEROS
015200970526     C                   MOVEL     DSMSKE        MSGKEY
015300970526E   1C                   ENDIF
015400970526     C                   Z-ADD     1             MSGULT
015500991025     C                   WRITE     s721DMS
015600991025     C                   WRITE     s721DMC
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
017000970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
017100970526IF  1C     DEPOP1        IFEQ      'O05'
017200970526     C                   MOVEL     '1'           FINL00
017300970526E   1C                   ENDIF
017400970526     C*
017500970526     C                   ENDSR
017600970526     C*------------------------------------------------------------------------*
017700970526     C* F03GES - GESTIONE F03 -> FINE
017800970526     C*------------------------------------------------------------------------*
017900970526     C     F03GES        BEGSR
018000970526     C*
018100970526     C* IMPOSTA "PREMUTO F03"
018200970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018300991025     C                   MOVEL     '1'           S20F03                         *DI PROCEDURA
018400970526     C*
018500970526     C                   ENDSR
018600970526     C*------------------------------------------------------------------------*
018700970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
018800970526     C*------------------------------------------------------------------------*
018900970526     C     F06GES        BEGSR
019000970526     C*
019100970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
019200970526IF  1C     DEPOP1        IFEQ      'F06'
019300970526     C                   EXSR      INSERI
019400970526E   1C                   ENDIF
019500970526IF  1C     DEPOP1        IFEQ      'O02'
019600970526     C                   EXSR      MODIFI
019700970526E   1C                   ENDIF
019800991014IF  1C     DEPOP1        IFEQ      'O03'
019900991014     C                   EXSR      copia
020000991014E   1C                   ENDIF
020100991104IF  1C*    DEPOP1        IFEQ      'O04'
020200991104     C*                  EXSR      ANNULL
020300991104E   1C*                  ENDIF
020400970526IF  1C     DEPOP1        IFEQ      'O07'
020500970526     C                   EXSR      RIPRIS
020600970526E   1C                   ENDIF
020700970526     C*
020800970526     C                   ENDSR
020900970526     C*------------------------------------------------------------------------*
021000970526     C* F12GES - GESTIONE F12 -> RITORNO
021100970526     C*------------------------------------------------------------------------*
021200970526     C     F12GES        BEGSR
021300970526     C*
021400970526     C* IMPOSTA "PREMUTO F12"
021500970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021600991025     C                   MOVEL     '1'           S20F12                         *DI PROCEDURA
021700970526     C*
021800970526     C                   ENDSR
021900970526     C*------------------------------------------------------------------------*
022000970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022100970526     C*------------------------------------------------------------------------*
022200970526     C     F24GES        BEGSR
022300970526     C*
022400970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
022500970526     C*
022600970526     C                   ENDSR
022700970526     C*------------------------------------------------------------------------*
022800970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
022900970526     C*------------------------------------------------------------------------*
023000970526     C     INSERI        BEGSR
023100970526     C*
023200970526     C* CONTROLLA I CAMPI IMMESSI
023300970526     C                   EXSR      DECODI
023400970526     C*
023500970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
023600970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
023700991025     C                   movel     VIDTip        vtftip
023800991026     c                   exsr      impvtf
023900991025     C                   WRITE     tivtf000
024000991025     C                   MOVEL     MSG(2)        S20MSG                         *MESSAGGIO
024100970602     C*
024200970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
024300970602     C                   EXSR      AZZVID
024400970526E   1C                   ENDIF
024500970526     C*
024600970526     C                   ENDSR
024700970526     C*------------------------------------------------------------------------*
024800970526     C* MODIFI - OPERAZIONI DI MODIFICA
024900970526     C*------------------------------------------------------------------------*
025000970526     C     MODIFI        BEGSR
025100970526     C*
025200970526     C* CONTROLLA I CAMPI IMMESSI
025300970526     C                   EXSR      DECODI
025400970526     C*
025500970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
025600970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025700991025     C                   EXSR      LETvtf
025800970526     C*
025900970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
026000970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026100991025     C                   EXSR      IMPvtf                                       *IMPOSTA I CAMPI
026200991025     C                   UPDATE    tivtf000
026300991025     C                   MOVEL     MSG(3)        S20MSG
026400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
026500970526E   2C                   ENDIF
026600970526E   1C                   ENDIF
026700970526     C*
026800970526     C                   ENDSR
026900991014     C*------------------------------------------------------------------------*
027000991014     C* copia  - OPERAZIONI DI copia
027100991014     C*------------------------------------------------------------------------*
027200991014     C     copia         BEGSR
027300991014     C*
027400991014     C* CONTROLLA I CAMPI IMMESSI
027500991014     C                   EXSR      DECODI
027600991014     C*
027700991014     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
027800991014IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
027900991025     C                   movel     VIDTip        vtftip
028000991025     C                   EXSR      IMPvtf                                       *IMPOSTA I CAMPI
028100991025     C                   write     tivtf000
028200991025     C                   MOVEL     MSG(3)        S20MSG
028300991014     C                   MOVEL     '1'           FINL00                         *FINE PGM
028400991014E   2C                   ENDIF
028500991014     C*
028600991014     C                   ENDSR
028700970526     C*------------------------------------------------------------------------*
028800970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
028900970526     C*------------------------------------------------------------------------*
029000991104     C*    ANNULL        BEGSR
029100991025     C*
029200991025     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
029300991104     C*                  EXSR      LETvtf
029400991025     C*
029500991025     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
029600991104IF  1C*    ERRL00        IFEQ      '0'                                          *NO ERRORI
029700991104     C*                  DELETE    tivtf000                                     *CANCELLA
029800991104     C*                  MOVEL     MSG(4)        s20MSG                         *MESSAGGIO
029900991104     C*                  MOVEL     '1'           FINL00                         *FINE PGM
030000991104E   1C*                  ENDIF
030100970526     C*
030200991104     C*                  ENDSR
030300970526     C*------------------------------------------------------------------------*
030400970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
030500970526     C*------------------------------------------------------------------------*
030600970526     C     RIPRIS        BEGSR
030700970526     C                   ENDSR
030800970526     C*------------------------------------------------------------------------*
030900991025     C* LETvtf - LEGGE IL RECORD PERL'AGGIORNAMENTO
031000970526     C*------------------------------------------------------------------------*
031100991025     C     LETvtf        BEGSR
031200970526     C*
031300991025     C                   MOVEL     S20tip        KCEtip
031400991025     C     KEYvtf        CHAIN     tivtf01L                           9998
031500970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
031600970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
031700991025     C                   MOVEL     MSG(7)        S20MSG                         *MESSAGGIO
031800970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
031900970526E   1C                   ENDIF
032000970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
032100970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
032200991025     C                   MOVEL     MSG(6)        S20MSG                         *MESSAGGIO
032300970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
032400970526E   1C                   ENDIF
032500970526     C*
032600970526     C                   ENDSR
032700970526     C*------------------------------------------------------------------------*
032800970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
032900970526     C*------------------------------------------------------------------------*
033000970526     C     DECODI        BEGSR
033100991008     C*--------------------
033200991008     C* tipo legame
033300991008     C*--------------------
033400001120     C     ERRL00        IFEQ      '0'
033500001120     C     VIDtip        IFne      *blank
033600991025     c                   exsr      cnttip
033700001120     C                   ELSE
033800991008     C                   MOVEL     '1'           ERRL00
033900991011     C                   SETON                                        66
034000991011     C                   MOVEL     MSG(12)       DSMSMS
034100001120     C                   ENDIF
034200001120     C                   ENDIF
034300991027     C*--------------------
034400991027     C* descrizione
034500991027     C*--------------------
034600001120     C     ERRL00        IFEQ      '0'
034700001120     C     VIDdes        IFne      *blank
034800001120     C                   ELSE
034900991027     C                   MOVEL     '1'           ERRL00
035000991027     C                   SETON                                        67
035100991027     C                   MOVEL     MSG(9)        DSMSMS
035200001120     C                   ENDIF
035300001120     C                   ENDIF
035400001120     C*--------------------
035500991027     C* Up -Dwn
035600991027     C*--------------------
035700001120     C     ERRL00        IFEQ      '0'
035800001120     C     VIDud         IFne      *blank
035900001120     C                   ELSE
036000991027     C                   MOVEL     '1'           ERRL00
036100991027     C                   SETON                                        68
036200991027     C                   MOVEL     MSG(9)        DSMSMS
036300001120     C                   ENDIF
036400001120     C                   ENDIF
036500001120     C*--------------------
036600001120     C* Controlla flag up/ down
036700001120     C*--------------------
036800001120     C     ERRL00        IFEQ      '0'
036900001120     C     VIDsnd        IFne      *blank
037000991108     c                   if        vidud = 'U'
037100991108     C                   MOVEL     '1'           ERRL00
037200991108     C                   SETON                                        64
037300991108     C                   MOVEL     MSG(13)       DSMSMS
037400991108     c                   end
037500001120     C                   ELSE
037600991108     c                   if        vidud = 'D'
037700991108     C                   MOVEL     '1'           ERRL00
037800991108     C                   SETON                                        64
037900991108     C                   MOVEL     MSG(12)       DSMSMS
038000001120     C                   ENDIF
038100001120     C                   ENDIF
038200001120     C                   ENDIF
038300001120     C*--------------------
038400001120     C* Programma traduttore
038500991027     C*--------------------
038600001120     C     ERRL00        IFEQ      '0'
038700001120     C     VIDpgt        IFNE      *blank
038800010315     C     VIDpgt        IFNE      '*'
038900001120     C                   CALL(e)   'TIS721C'
039000050112     C                   PARM      *blanks       LIBRERIA         10
039100001120     C                   PARM      VIDpgt        OBJECT           10
039200001120     C                   PARM      '*PGM   '     TYPE              7
039300001120     C                   PARM      '0'           ESITO             1
039400001120     C                   IF        ESITO = '1'
039500001120     C                   MOVEL     '1'           ERRL00
039600001120     C                   SETON                                        69
039700001120     C                   MOVEL     MSG(14)       DSMSMS
039800001120     C                   ENDIF
039900010315     C                   ENDIF
040000001120     C                   ELSE
040100001120     C                   MOVEL     '1'           ERRL00
040200001120     C                   SETON                                        69
040300001120     C                   MOVEL     MSG(9)        DSMSMS
040400001120     C                   ENDIF
040500001120     C                   ENDIF
040600001120     C*--------------------
040700001120     C* Programma post-traduzione
040800001120     C*--------------------
040900001120     C     ERRL00        IFEQ      '0'
041000001120     C     VIDpgi        IFNE      *blank
041100010315     C     VIDpgi        IFNE      '*'
041200001120     C                   CALL(e)   'TIS721C'
041300050112     C                   PARM      *blanks       LIBRERIA         10
041400001120     C                   PARM      VIDpgi        OBJECT           10
041500001120     C                   PARM      '*PGM   '     TYPE              7
041600001120     C                   PARM      '0'           ESITO             1
041700001120     C                   IF        ESITO = '1'
041800001120     C                   MOVEL     '1'           ERRL00
041900001120     C                   SETON                                        55
042000001120     C                   MOVEL     MSG(14)       DSMSMS
042100001120     C                   ENDIF
042200010315     C                   ENDIF
042300001120     C                   ENDIF
042400001120     C                   ENDIF
042500001120     C*--------------------
042600001120     C* File da tradurre
042700991027     C*--------------------
042800001120     C     ERRL00        IFEQ      '0'
042900001120     C     VIDfid        IFNE      *blank
043000010315     C     VIDfid        IFNE      '*'
043100001120     C                   CALL(e)   'TIS721C'
043200050112     C                   PARM      *blanks       LIBRERIA         10
043300001120     C                   PARM      VIDfid        OBJECT           10
043400001120     C                   PARM      '*FILE  '     TYPE              7
043500001120     C                   PARM      '0'           ESITO             1
043600001120     C                   IF        ESITO = '1'
043700001120     C                   MOVEL     '1'           ERRL00
043800001120     C                   SETON                                        65
043900001120     C                   MOVEL     MSG(15)       DSMSMS
044000001120     C                   ENDIF
044100010315     C                   ENDIF
044200001120     C                   ELSE
044300001120     C                   IF        VIDud = 'D'
044400991027     C                   MOVEL     '1'           ERRL00
044500991027     C                   SETON                                        65
044600991027     C                   MOVEL     MSG(9)        DSMSMS
044700001120     C                   ENDIF
044800001120     C                   ENDIF
044900001120     C                   ENDIF
045000001120     C*--------------------
045100001120     C* File storico
045200001120     C*--------------------
045300001120     C     ERRL00        IFEQ      '0'
045400001120     C     VIDfis        IFNE      *blank
045500010315     C     VIDfis        IFNE      '*'
045600010315     C                   CALL(e)   'TIS721C'
045700050112     C                   PARM      *blanks       LIBRERIA         10
045800001120     C                   PARM      VIDfis        OBJECT           10
045900001120     C                   PARM      '*FILE  '     TYPE              7
046000001120     C                   PARM      '0'           ESITO             1
046100001120     C                   IF        ESITO = '1'
046200001120     C                   MOVEL     '1'           ERRL00
046300001120     C                   SETON                                        56
046400001120     C                   MOVEL     MSG(15)       DSMSMS
046500001120     C                   ENDIF
046600010315     C                   ENDIF
046700001120     C                   ENDIF
046800001120     C                   ENDIF
046900001120     C*--------------------
047000001120     C* File tradotto
047100001120     C*--------------------
047200001120     C     ERRL00        IFEQ      '0'
047300001120     C     VIDfit        IFNE      *blank
047400010315     C     VIDfit        IFNE      '*'
047500001120     C                   CALL(e)   'TIS721C'
047600050112     C                   PARM      *blanks       LIBRERIA         10
047700001120     C                   PARM      VIDfit        OBJECT           10
047800001120     C                   PARM      '*FILE  '     TYPE              7
047900001120     C                   PARM      '0'           ESITO             1
048000001120     C                   IF        ESITO = '1'
048100001120     C                   MOVEL     '1'           ERRL00
048200001120     C                   SETON                                        57
048300001120     C                   MOVEL     MSG(15)       DSMSMS
048400001120     C                   ENDIF
048500010315     C                   ENDIF
048600001120     C                   ENDIF
048700001120     C                   ENDIF
048800060616     C*--------------------
048900060616     C* File Da tradurre / File Storico / Programma Post-Traduzione
049000060616     C*--------------------
049100060804     C*    ERRL00        IFEQ      '0'
049200060804     C*                  IF        (VIDfid='*' AND VIDfis='*' AND VIDpgi='*') OR
049300060804     C*                            (VIDfid<>'*' AND VIDfis<>'*' AND VIDpgi<>'*')
049400060804     C*                  ELSE
049500060804     C*                  MOVEL     '1'           ERRL00
049600060804     C*                  SETON                                        555665
049700060804     C*                  MOVEL     MSG(16)       DSMSMS
049800060804     C*                  ENDIF
049900060804     C*                  ENDIF
050000001120     C*
050100070918     C*--------------------
050200070918     C* Flag "No membro su file tradotto" => controllo che sia indicato solo x UPLOADs
050300070918     C*--------------------
050400070918     C     ERRL00        IFEQ      '0'
050500070918     C                   IF        VIDMBRT <> *blanks AND
050600070918     C                             VIDud   <> 'U'
050700070918     C                   MOVEL     '1'           ERRL00
050800070918     C                   SETON                                        58
050900070918     C                   MOVEL     MSG(17)       DSMSMS
051000070918     C                   ENDIF
051100070918     C                   ENDIF
051200001120     C                   ENDSR
051300991008     C*------------------------------------------------------------------------*
051400991108     C* Cnttip - CONTROLLA codice tipo file
051500991008     C*------------------------------------------------------------------------*
051600001120     C     cnttip        begsr
051700001120     C*
051800001120     C                   if        depop1 = 'F06' or
051900001120     C                             depop1 = 'O03'
052000001120     C     vidtip        setll     tivtf01l                               98
052100001120     C                   if        *in98 = '1'
052200001120     C                   move      '1'           errl00
052300001120     C                   seton                                        66
052400001120     C                   movel     MSG(10)       DSMSMS
052500001120     C                   endif
052600001120     C                   endif
052700001120     C                   if        errl00 = '0'
052800991108     C     '?'           SCAN      vidtip                                 30
052900001120     C*  Ricerca
053000001120     C                   eval      t02tla = 'L'
053100001120     C                   if        *IN30 = *ON
053200001120     C                   movel     'R'           t02mod
053300001120     C                   else
053400001120     C                   movel     'C'           t02mod
053500001120     C                   movel(p)  vidtip        t02ke1
053600001120     C                   endif
053700001120     C                   movel     'ITF'         t02cod
053800001120     C                   movel     knsif         t02sif
053900001120     C                   call      'TIBS02R'
054000001120     C                   parm                    KPJBA
054100001120     C                   parm                    TIBS02DS
054200001120     C
054300001120     C*
054400001120     C                   if        T02err = *blanks
054500991108     C                   movel     T02ke1        vidtip
054600991108     C                   movel     T02uni        vddtip
054700001120     C                   else
054800001120     C                   movel     '1'           errl00
054900991108     C                   movel     *blank        vddtip
055000001120     C                   seton                                        66
055100001120     C                   movel     msg(11)       DSMSMS
055200001120     C                   endif
055300001120     C                   endif
055400001120     C*
055500001120     C                   ENDSR
055600970526     C*------------------------------------------------------------------------*
055700970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
055800970526     C*------------------------------------------------------------------------*
055900970526     C     IREVID        BEGSR
056000970526     C* CAMPI
056100991025     C                   MOVEL     vtftip        VIDtip
056200991025     C                   MOVEL     vtfdes        VIDdes
056300991025     C                   MOVEL     vtfud         VIDud
056400991025     C                   MOVEL     vtfpgt        VIDpgt
056500991025     C                   MOVEL     vtfppt        VIDppt
056600991025     C                   MOVEL     vtfpgi        VIDpgi
056700101013     C                   MOVEL     vtfppi        dvtfppi
056800101013     C                   MOVEL     D§VTFPPIMF    VIDppimf
056900101013     C                   MOVEL     D§VTFPPIMS    VIDppims
057000101013     C                   MOVEL     D§VTFPPIMZ    VIDppimz
057100991025     C                   MOVEL     vtfsnd        VIDsnd
057200991025     C                   MOVEL     vtffid        VIDfid
057300991025     C                   MOVEL     vtffis        VIDfis
057400991025     C                   MOVEL     vtffit        VIDfit
057500991025     C                   MOVEL     vtfctl        VIDctl
057600011127     C                   MOVEL     vtffi1        VIDtab
057700051013     C                   EVAL      VIDMBRT = %subst(vtffi2:1:1)
057800970527     C*
057900970526     C                   ENDSR
058000970526     C*------------------------------------------------------------------------*
058100970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
058200970526     C*------------------------------------------------------------------------*
058300970526     C     AZZVID        BEGSR
058400970526     C* CAMPI
058500991025     C                   MOVEL     *blank        VIDtip
058600991025     C                   MOVEL     *blank        VIDdes
058700991025     C                   MOVEL     *blank        VIDud
058800991025     C                   MOVEL     *blank        VIDpgt
058900991025     C                   MOVEL     *blank        VIDppt
059000991025     C                   MOVEL     *blank        VIDpgi
059100101013     C                   MOVEL     *blank        VIDppimf
059200101013     C                   MOVEL     *blank        VIDppims
059300101013     C                   MOVEL     *blank        VIDppimz
059400991025     C                   MOVEL     *blank        VIDsnd
059500991025     C                   MOVEL     *blank        VIDfid
059600991025     C                   MOVEL     *blank        VIDfis
059700991025     C                   MOVEL     *blank        VIDfit
059800991025     C                   MOVEL     *blank        VIDctl
059900011127     C                   MOVEL     *blank        VIDtab
060000051013     C                   MOVEL     *blank        VIDMBRT
060100970526     C*
060200970526     C                   ENDSR
060300970526     C*------------------------------------------------------------------------*
060400991025     C* IMPvtf - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
060500970526     C*------------------------------------------------------------------------*
060600991025     C     IMPvtf        BEGSR
060700970526     C*
060800991026     C                   movel     VIDdes        vtfdes
060900991026     C                   movel     VIDud         vtfud
061000991026     C                   movel     VIDpgt        vtfpgt
061100991026     C                   movel     VIDppt        vtfppt
061200991026     C                   movel     VIDpgi        vtfpgi
061300101013     C                   movel     VIDppimf      D§VTFPPIMF
061400101013     C                   movel     VIDppims      D§VTFPPIMS
061500101013     C                   movel     VIDppimz      D§VTFPPIMZ
061600101013     C                   movel     dvtfppi       vtfppi
061700991026     C                   movel     VIDsnd        vtfsnd
061800991026     C                   movel     VIDfid        vtffid
061900991026     C                   movel     VIDfis        vtffis
062000991026     C                   movel     VIDfit        vtffit
062100991026     C                   movel     VIDctl        vtfctl
062200011127     C                   movel     VIDtab        vtffi1
062300051013     C                   eval      %subst(vtffi2:1:1) = VIDmbrt
062400970526     C*
062500970526     C                   ENDSR
062600970526     C*------------------------------------------------------------------------*
062700970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
062800970526     C*------------------------------------------------------------------------*
062900970526     C     IMPFUN        BEGSR
063000970526     C*
063100970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
063200970526IF  1C     DEPOP1        IFEQ      'O05'
063300970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
063400970526     C                   MOVEL     'N'           FUDATT
063500970526     C                   MOVEL     FUDRIG        FUD(6)
063600970526E   1C                   ENDIF
063700970526     C*
063800970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
063900970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
064000970526     C                   EXSR      FUNGES
064100970526     C*
064200970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
064300970526     C*---
064400970526     C* 90 -> PROTEZIONE CHIAVE
064500970526     C* 91 -> PROTEZIONE TOTALE
064600970526     C*---
064700970528IF  1C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
064800970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
064900970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
065000970526     C                   SETOFF                                       91
065100970526E   2C                   ENDIF
065200970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
065300991014IF  2C     DEPOP1        oreq      'O01'                                        *MODIFICA
065400970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
065500970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
065600970526E   2C                   ENDIF
065700970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
065800970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
065900970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
066000970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
066100970526     C                   SETON                                        91
066200970526E   2C                   ENDIF
066300970526X   1C                   ELSE                                                   *I N T E R R O G A
066400970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
066500970526     C                   SETON                                        91
066600970526E   1C                   ENDIF
066700970526     C*
066800970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
066900970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
067000991025     C                   MOVEL     S20tip        KCEtip
067100991025     C     KEYvtf        CHAIN     tivtf01L                           9998
067200970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
067300970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
067400970526     C                   MOVEL     MSG(6)        DSMSMS
067500970526X   2C                   ELSE
067600970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
067700980128IF  3C     DEPOP1        IFEQ      'O05'
067800991014IF  3C     DEPOP1        oreq      'O02'
067900980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
068000980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
068100980128E   3C                   ENDIF
068200970526E   2C                   ENDIF
068300970526X   1C                   ELSE                                                   *INSERIMENTO
068400970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
068500970526E   1C                   ENDIF
068600970526     C*
068700970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
068800970526IF  1C     DEPOP1        IFEQ      'O02'
068900970526     C                   MOVEL     DEC(1)        VIDFUN
069000970526E   1C                   ENDIF
069100991014IF  1C     DEPOP1        IFEQ      'O03'
069200991014     C                   MOVEL     DEC(6)        VIDFUN
069300991014E   1C                   ENDIF
069400970526IF  1C     DEPOP1        IFEQ      'O04'
069500970526     C                   MOVEL     DEC(2)        VIDFUN
069600970526E   1C                   ENDIF
069700970526IF  1C     DEPOP1        IFEQ      'O05'
069800970526     C                   MOVEL     DEC(3)        VIDFUN
069900970526E   1C                   ENDIF
070000970526IF  1C     DEPOP1        IFEQ      'O07'
070100970526     C                   MOVEL     DEC(4)        VIDFUN
070200970526E   1C                   ENDIF
070300970526IF  1C     DEPOP1        IFEQ      'F06'
070400970526     C                   MOVEL     DEC(5)        VIDFUN
070500970526E   1C                   ENDIF
070600970526     C*
070700970526     C                   ENDSR
070800970526     C*------------------------------------------------------------------------*
070900970526     C* *INZSR - OPERAZIONI INIZIALI
071000970526     C*------------------------------------------------------------------------*
071100970526     C     *INZSR        BEGSR
071200970526     C*---
071300970526     C* RICEVIMENTO PARAMETRI
071400970526     C*---
071500970526     C     *ENTRY        PLIST
071600970612     C                   PARM                    KPJBA
071700991028     C                   movel     kpjbu         tis720
071800970526     C*
071900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
072000970526     C     XMSGPA        PLIST
072100970526     C                   PARM                    DSMSMS
072200970526     C                   PARM                    DSMSPG
072300970526     C                   PARM                    DSMSKE
072400970526     C                   PARM                    DSMSER
072500970526     C*
072600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
072700970526     C                   CALL      'XKEY'
072800970526     C                   PARM                    DSKEY
072900970526     C*---
073000970526     C* VARIABILI NON RIFERITE AL DATA BASE
073100970526     C*---
073200970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
073300991025     C     *LIKE         DEFINE    S20OP0        DEPOP0
073400991025     C     *LIKE         DEFINE    S20OP1        DEPOP1
073500991025     C     *LIKE         DEFINE    S20OP0        WOP0
073600991025     C     *LIKE         DEFINE    S20OP1        WOP1
073700970526     C*---
073800970526     C* VARIABILI DI CONTROLLO
073900970526     C*---
074000970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
074100970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
074200970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
074300970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
074400970526     C*
074500970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
074600991025     C                   MOVEL     'TIS721R'     DSMSPG
074700991025     C                   MOVEL     'TIS721R'     MSGQUE
074800970526     C                   MOVEL     *BLANKS       MSGKEY
074900970526     C                   MOVEL     *BLANKS       DSMSMS
075000970526     C*---
075100970526     C* VARIABILI RIFERITE AL DATA BASE
075200970526     C*---
075300991025     C     *LIKE         DEFINE    vtftip        KCEtip
075400970526     C*---
075500970526     C* CHIAVI DI LETTURA
075600970526     C*---
075700991025     C* LETTURA tivtf01L - COMPLETA
075800991025     C     KEYvtf        KLIST
075900991025     C                   KFLD                    KCEtip                         *PARTENZA/ARRIVO
076000970528     C*
076100970528     C* CALCOLA LA DATA CORRENTE
076200970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
076300970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
076400970528     C                   Z-ADD     WN8           G08DAT
076500970528     C                   Z-ADD     *ZEROS        G08INV
076600970528     C                   MOVEL     '0'           G08ERR
076700970528     C                   CALL      'XSRDA8'
076800970528     C                   PARM                    WLBDA8
076900970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
077000970526     C*
077100970526     C* DEPOSITA LE OPZIONI DI ENTRATA
077200991025     C                   MOVEL     S20OP0        DEPOP0                         *LIVELLO PROCEDURA
077300991025     C                   MOVEL     S20OP1        DEPOP1                         *LIVELLO PROGRAMMA
077400970526     C*
077500970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
077600970526     C                   EXSR      IMPFUN
077700970526     C*
077800970526     C                   ENDSR
077900970526     C*------------------------------------------------------------------------*
078000970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
078100970526     C*------------------------------------------------------------------------*
078200970526     C     FUNGES        BEGSR
078300970526     C*
078400970526     C* PULISCO CAMPI DI LAVORO
078500970526     C                   Z-ADD     0             W                 5 0
078600970526     C*
078700970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
078800970526     C* E CARICO TABELLA FUNZIONI ATTIVE
078900970526     C     1             DO        24            I
079000970526     C                   MOVEL     FUD(I)        FUDRIG
079100970526     C*
079200970526     C* PULIZIA FUNZIONI PRECEDENTI
079300970526     C                   MOVEL     *BLANKS       FUV(I)
079400970526     C*
079500970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
079600970526     C* (SALTA F24)
079700970526     C     FUDATT        IFEQ      'S'
079800970526     C     FUDFUN        ANDNE     '24'
079900970526     C                   ADD       1             W
080000970526     C                   MOVEL     FUDDES        FUV(W)
080100970526     C                   END
080200970526     C                   END                                                    DO WHILE
080300970526     C*
080400970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
080500970526     C* ALTRIMENTI DISATTIVO F24
080600970526     C     1             DO        24            I
080700970526     C                   MOVEL     FUD(I)        FUDRIG
080800970526     C*
080900970526     C     FUDFUN        IFEQ      '24'
081000970526     C     W             IFGT      6
081100970526     C                   MOVEL     'S'           FUDATT
081200970526     C                   ELSE
081300970526     C                   MOVEL     *BLANKS       FUDATT
081400970526     C                   END                                                    W GT 8
081500970526     C                   MOVEL     FUDRIG        FUD(I)
081600970526     C                   END
081700970526     C                   END                                                    DO WHILE
081800970526     C*
081900970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
082000970526     C     W             DIV       6             I
082100970526     C                   MVR                     Z
082200970526     C*
082300970526     C     Z             IFNE      0
082400970526     C     I             ADD       1             Z
082500970526     C                   ELSE
082600970526     C                   Z-ADD     I             Z                 5 0
082700970526     C                   END
082800970526     C*
082900970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
083000970526     C                   ADD       1             FUNGRU
083100970526     C     FUNGRU        IFGT      Z
083200970526     C     FUNGRU        ORLT      1
083300970526     C                   Z-ADD     1             FUNGRU
083400970526     C                   END
083500970526     C*
083600970526     C* PRIMO GRUPPO DI 6 FUNZIONI
083700970526     C     FUNGRU        IFEQ      1
083800970526     C                   MOVEL     FUV(1)        FUN001
083900970526     C                   MOVEL     FUV(2)        FUN002
084000970526     C                   MOVEL     FUV(3)        FUN003
084100970526     C                   MOVEL     FUV(4)        FUN004
084200970526     C                   MOVEL     FUV(5)        FUN005
084300970526     C                   MOVEL     FUV(6)        FUN006
084400970526     C                   END
084500970526     C*
084600970526     C* SECONDO GRUPPO DI 6 FUNZIONI
084700970526     C     FUNGRU        IFEQ      2
084800970526     C                   MOVEL     FUV(7)        FUN001
084900970526     C                   MOVEL     FUV(8)        FUN002
085000970526     C                   MOVEL     FUV(9)        FUN003
085100970526     C                   MOVEL     FUV(10)       FUN004
085200970526     C                   MOVEL     FUV(11)       FUN005
085300970526     C                   MOVEL     FUV(12)       FUN006
085400970526     C                   END
085500970526     C*
085600970526     C* TERZO GRUPPO DI 6 FUNZIONI
085700970526     C     FUNGRU        IFEQ      3
085800970526     C                   MOVEL     FUV(13)       FUN001
085900970526     C                   MOVEL     FUV(14)       FUN002
086000970526     C                   MOVEL     FUV(15)       FUN003
086100970526     C                   MOVEL     FUV(16)       FUN004
086200970526     C                   MOVEL     FUV(17)       FUN005
086300970526     C                   MOVEL     FUV(18)       FUN006
086400970526     C                   END
086500970526     C*
086600970526     C* QUARTO GRUPPO DI 6 FUNZIONI
086700970526     C     FUNGRU        IFEQ      4
086800970526     C                   MOVEL     FUV(19)       FUN001
086900970526     C                   MOVEL     FUV(20)       FUN002
087000970526     C                   MOVEL     FUV(21)       FUN003
087100970526     C                   MOVEL     FUV(22)       FUN004
087200970526     C                   MOVEL     *BLANKS       FUN005
087300970526     C                   MOVEL     *BLANKS       FUN006
087400970526     C                   END
087500970526     C*
087600970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
087700970526     C                   MOVEL     *BLANKS       FUN007
087800970526     C                   MOVEL     *BLANKS       FUN008
087900970526     C*
088000970526     C     1             DO        24            I
088100970526     C                   MOVEL     FUD(I)        FUDRIG
088200970526     C     FUDFUN        IFEQ      '24'
088300970526     C     FUDATT        ANDEQ     'S'
088400970526     C                   MOVEL     FUDDES        FUN008
088500970526     C                   END
088600970526     C                   END                                                    DO
088700970526     C*
088800970526     C                   ENDSR
088900970526     C*------------------------------------------------------------------------*
089000970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
089100970526     C*------------------------------------------------------------------------*
089200970526     C     FUNCON        BEGSR
089300970526     C*
089400970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
089500970526     C                   EXSR      FUNVAR
089600970526     C*
089700970526     C* PULISCO CAMPI DI LAVORO
089800970526     C                   Z-ADD     0             I                 5 0
089900970526     C                   Z-ADD     0             W                 5 0
090000970526     C*
090100970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
090200970526     C     FUNKEY        IFNE      *BLANKS
090300970526     C     1             DO        24            I
090400970526     C                   MOVEL     FUD(I)        FUDRIG
090500970526     C*
090600970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
090700970526     C     FUNKEY        IFEQ      FUDFUN
090800970526     C     FUDATT        ANDEQ     'S'
090900970526     C                   Z-ADD     I             W
091000970526     C                   END
091100970526     C                   END                                                    DO WHILE
091200970526     C*
091300970526     C* FUNZIONE NON ATTIVA (ERRORE)
091400970526     C     W             IFEQ      0
091500970526     C*
091600970526     C* EMISSIONE MESSAGGIO DI ERRORE
091700970526     C                   MOVEL     MSG(1)        DSMSMS
091800970526     C*
091900970526     C                   END                                                    W=0
092000970526     C                   END                                                    FUNKEY NE *BLANKS
092100970526     C*
092200970526     C* FUNZIONE ATTIVA (PROCEDI ...)
092300970526     C     W             IFNE      0
092400970526     C     FUNKEY        OREQ      *BLANKS
092500970526     C     FUNKEY        IFEQ      *BLANKS
092600970526     C                   EXSR      ENTGES
092700970526     C                   END
092800970526     C     FUNKEY        IFEQ      '03'
092900970526     C                   EXSR      F03GES
093000970526     C                   END
093100970526     C     FUNKEY        IFEQ      '06'
093200970526     C                   EXSR      F06GES
093300970526     C                   END
093400970526     C     FUNKEY        IFEQ      '12'
093500970526     C                   EXSR      F12GES
093600970526     C                   END
093700970526     C     FUNKEY        IFEQ      '24'
093800970526     C                   EXSR      F24GES
093900970526     C                   END
094000970526     C                   END
094100970526     C*
094200970526     C                   ENDSR
094300970526     C*------------------------------------------------------------------------*
094400970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
094500970526     C*------------------------------------------------------------------------*
094600970526     C     FUNVAR        BEGSR
094700970526     C*
094800970526     C* INIZIALIZZA VARIABILE -FUNKEY-
094900970526     C*
095000970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
095100970526     C     KEY           IFEQ      ENTER
095200970526     C                   MOVEL     *BLANKS       FUNKEY
095300970526     C                   END
095400970526     C     KEY           IFEQ      F1
095500970526     C                   MOVEL     '01'          FUNKEY
095600970526     C                   END
095700970526     C     KEY           IFEQ      F2
095800970526     C                   MOVEL     '02'          FUNKEY
095900970526     C                   END
096000970526     C     KEY           IFEQ      F3
096100970526     C                   MOVEL     '03'          FUNKEY
096200970526     C                   END
096300970526     C     KEY           IFEQ      F4
096400970526     C                   MOVEL     '04'          FUNKEY
096500970526     C                   END
096600970526     C     KEY           IFEQ      F5
096700970526     C                   MOVEL     '05'          FUNKEY
096800970526     C                   END
096900970526     C     KEY           IFEQ      F6
097000970526     C                   MOVEL     '06'          FUNKEY
097100970526     C                   END
097200970526     C     KEY           IFEQ      F7
097300970526     C                   MOVEL     '07'          FUNKEY
097400970526     C                   END
097500970526     C     KEY           IFEQ      F8
097600970526     C                   MOVEL     '08'          FUNKEY
097700970526     C                   END
097800970526     C     KEY           IFEQ      F9
097900970526     C                   MOVEL     '09'          FUNKEY
098000970526     C                   END
098100970526     C     KEY           IFEQ      F10
098200970526     C                   MOVEL     '10'          FUNKEY
098300970526     C                   END
098400970526     C     KEY           IFEQ      F11
098500970526     C                   MOVEL     '11'          FUNKEY
098600970526     C                   END
098700970526     C     KEY           IFEQ      F12
098800970526     C                   MOVEL     '12'          FUNKEY
098900970526     C                   END
099000970526     C     KEY           IFEQ      F13
099100970526     C                   MOVEL     '13'          FUNKEY
099200970526     C                   END
099300970526     C     KEY           IFEQ      F14
099400970526     C                   MOVEL     '14'          FUNKEY
099500970526     C                   END
099600970526     C     KEY           IFEQ      F15
099700970526     C                   MOVEL     '15'          FUNKEY
099800970526     C                   END
099900970526     C     KEY           IFEQ      F16
100000970526     C                   MOVEL     '16'          FUNKEY
100100970526     C                   END
100200970526     C     KEY           IFEQ      F17
100300970526     C                   MOVEL     '17'          FUNKEY
100400970526     C                   END
100500970526     C     KEY           IFEQ      F18
100600970526     C                   MOVEL     '18'          FUNKEY
100700970526     C                   END
100800970526     C     KEY           IFEQ      F19
100900970526     C                   MOVEL     '19'          FUNKEY
101000970526     C                   END
101100970526     C     KEY           IFEQ      F20
101200970526     C                   MOVEL     '20'          FUNKEY
101300970526     C                   END
101400970526     C     KEY           IFEQ      F21
101500970526     C                   MOVEL     '21'          FUNKEY
101600970526     C                   END
101700970526     C     KEY           IFEQ      F22
101800970526     C                   MOVEL     '22'          FUNKEY
101900970526     C                   END
102000970526     C     KEY           IFEQ      F23
102100970526     C                   MOVEL     '23'          FUNKEY
102200970526     C                   END
102300970526     C     KEY           IFEQ      F24
102400970526     C                   MOVEL     '24'          FUNKEY
102500970526     C                   END
102600970526     C     KEY           IFEQ      ROLLUP
102700970526     C                   MOVEL     'RU'          FUNKEY
102800970526     C                   END
102900970526     C     KEY           IFEQ      HELP
103000970526     C                   MOVEL     'HP'          FUNKEY
103100970526     C                   END
103200970526     C                   ENDSR
103300970526     C*------------------------------------------------------------------------*
103400970526     C* NOTE COSTRUZIONE SCHIERE
103500970526     C*------------------------------------------------------------------------*
103600970526     C*
103700970526     C* -MSG-MESSAGGI
103800970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
103900970526     C* (NON TOCCARE MSG,1 E MSG,2)
104000970526     C*
104100970526     C* -CMD-COMANDI
104200970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
104300970526     C*
104400970526     C* -OPD-OPZIONI
104500970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
104600970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
104700970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
104800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
104900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
105000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
105100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
105200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
105300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
105400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
105500970526     C*
105600970526     C* -FUD-FUNZIONI
105700970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
105800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
105900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
106000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
106100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
106200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
106300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
106400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
106500970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
106600970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
106700970526     O*------------------------------------------------------------------------*
106800970526** DEC - DECODIFICHE
106900970526    MODIFICA                                                      1
107000970526  ANNULLAMENTO                                                    2
107100970526VISUALIZZAZIONE                                                   3
107200970526   RIPRISTINO                                                     4
107300970526  INSERIMENTO                                                     5
107400991014     COPIA                                                        6
107500970526** MSG - MESSAGGI
107600991025TIS721R- Tasto funzionale non gestito                                          1
107700991025TIS721R- Inserimento effettuato                                                2
107800991025TIS721R- Modifica effettuata                                                   3
107900991025TIS721R- Annullamento effettuato                                               4
108000991025TIS721R- Ripristino effettuato                                                 5
108100991025TIS721R- Richiesta funzione per record inesistente                             6
108200991025TIS721R- Record momentaneamente allocato: riprovare più tardi                  7
108300991025TIS721R- Richiesta funzione per record esistente                               8
108400991027TIS721R- Il campo è obbligatorio                                               9
108500991025TIS721R- Il codice tipo file è già presente in lista                           10
108600991108TIS721R- Codice tipo file non presente in tabella                              11
108700991108TIS721R- Immettere il valore I oppure D se il file è DOWNLOAD                  12
108800991108TIS721R- Lasciare il campo a blank se il file è di UPLOAD                      13
108900001120TIS721R- Oggetto inesistente, verificare e correggere.                         14
109000001120TIS721R- File inesistente, verificare e correggere.                            15
109100060616TIS721R- Valori incompatibili, correggere!                                     16
109200070918TIS721R- Valorizzazione possibile solo x operazioni di UPLOAD                  17
109300991025TIS721R-                                                                       18
109400991025TIS721R-                                                                       19
109500991025TIS721R-                                                                       20
109600970526** FUD - FUNZIONI
109700970526                   -        (S/N)=N                        funz.01¬
109800970526                   -        (S/N)=N                        funz.02¬
109900970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
110000970526                   -        (S/N)=N                        funz.04¬
110100970526                   -        (S/N)=N                        funz.05¬
110200970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
110300970527                   -        (S/N)=N                        funz.07¬
110400970527                   -        (S/N)=N                        funz.08¬
110500970527                   -        (S/N)=N                        funz.09¬
110600970527                   -        (S/N)=N                        funz.10¬
110700970526                   -        (S/N)=N                        funz.11¬
110800970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
110900970526                   -        (S/N)=N                        funz.13¬
111000970526                   -        (S/N)=N                        funz.14¬
111100970526                   -        (S/N)=N                        funz.15¬
111200970526                   -        (S/N)=N                        funz.16¬
111300970526                   -        (S/N)=N                        funz.17¬
111400970526                   -        (S/N)=N                        funz.18¬
111500970526                   -        (S/N)=N                        funz.19¬
111600970526                   -        (S/N)=N                        funz.20¬
111700970526                   -        (S/N)=N                        funz.21¬
111800970526                   -        (S/N)=N                        funz.22¬
111900970526                   -        (S/N)=N                        funz.23¬
112000970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
