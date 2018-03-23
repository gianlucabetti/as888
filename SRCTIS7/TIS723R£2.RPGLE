000100970526     H*------------------------------------------------------------------------*
000200991025     H* Gestione tipi file internet
000300970526     H*------------------------------------------------------------------------*
000400101026     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000500970526     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900991026     Ftis723d   cf   e             WORKSTN INFDS(DEVDS1)
001000991026     F                                     SFILE(s723DMS:MSGULT)
001100970527     F*------------
001200991027     Ftivir03L  UF A E           K DISK
001300080603     Ftivire1L  UF A E           K DISK
001400991027     Ftivtf01l  if   e           k disk
001500991027     Fazorg01l  if   e           k disk
001600000626     Ftntbe01l  if   e           k disk
001700000727     Ftianv01l  if   e           k disk
001800970526     D*------------------------------------------------------------------------*
001900970526     D* SCHIERE
002000970526     D*------------------------------------------------------------------------*
002100970526     D*------------
002200970526     D* DECODIFICHE
002300970526     D*------------
002400991014     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
002500970526     D*------------
002600970526     D* MESSAGGI
002700970526     D*------------
002800101026     D MSG             S             78    DIM(30) CTDATA PERRCD(1)
002900970526     D*------------
003000970526     D* DESCRIZIONI TASTI FUNZIONALI
003100970526     D*------------
003200970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003300970526     D*------------
003400970526     D* FUNZIONI DA CARICARE A VIDEO
003500970526     D*------------
003600970526     D FUV             S             19    DIM(24)
003700970526     D*------------------------------------------------------------------------*
003800970526     D* INPUT
003900970526     D*------------------------------------------------------------------------*
004000970526     D*------------
004100970526     D* RIDEFINIZIONE FUNZIONI VIDEO
004200970526     D*------------
004300970526     D FUDDS           DS
004400970526     D  FUDDE1                 1      1
004500970526     D  FUDFUN                 2      3
004600970526     D  FUDDE2                 4     19
004700970526     D  FUDFI0                20     34
004800970526     D  FUDATT                35     35
004900970526     D  FUDFI1                36     80
005000970526     D  FUDDES                 1     19
005100970526     D  FUDRIG                 1     80
005200970526     D*------------
005300970526     D* DS GESTIONE TASTI FUNZIONALI
005400970526     D*------------
005500970526     D DSKEY         E DS
005600970526     D DEVDS1          DS
005700970526     D  KEY                  369    369
005800970526     D*------------
005900970526     D* DS "XMSG" - GESTIONE MESSAGGI
006000970526     D*------------
006100970526     D DSMSG         E DS
006200970526     D*------------
006300020906     D* TABELLE
006400020906     D*------------
006500991108     D TIBS02DS      E DS
006600970528     D*------------------
006700970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
006800970528     D*------------------
006900970528     D WLBDA8          DS                  INZ
007000970528     D  G08DAT                 1      8  0
007100970528     D  G08INV                 9     16  0
007200970528     D  G08ERR                17     17
007300970528     D  G08TGI                18     22  0
007400020218     D*------------
007500020218     D* DS "ITF" - TABELLA SCAMBIO TIPI FILE
007600020218     D*------------
007700020218     D DITF          E DS
007800970526     D*------------
007900970526     D* DS DI PROCEDURA
008000970526     D*------------
008100991025     D tis720        E DS                  EXTNAME(tis720ds)
008200030526     D DVIR01        E DS
008300030603     D DISV          E DS
008400040816     D DIFT          E DS
008500101013     D DVTFPPI       E DS
008600970526     D*------------
008700970526     D* ARCHITETTURA
008800970526     D*------------
008900970526     D KPJBA         E DS
009000080603     D*------------
009100080603     D* ESTENSIONE ANAGRAFICA CLIENTI/TIPI FILE
009200080603     D*------------
009300080603     D DVIRE01       E DS
009400040816     D*------------
009500040816     D* DS REPERIMENTO DATI CLIENTI
009600040816     D*------------
009700040816     DBS69DS         e ds                  extname(TIBS69DS)
009800040816     DACODS          e ds                  extname(cnaco00f)
009900040816     DINDDS          e ds                  extname(cnind00f)
010000040816     DCLPDS          e ds                  extname(cnclp00f)
010100040816     DCLSDS          e ds                  extname(fncls00f)
010200081020
010300081020     D*------------
010400081020     D* DS x richiamo *pgm utilità d controllo indirizzi e-mail
010500081020     D*------------
010600081020     D DSEMAIL       E DS
010700040816     D*------------
010800040816     D* VARIABILI D WRK
010900040816     D*------------
011000040816     D ksc             s              7  0 dim(30)
011100040816     D ksa             s              4    dim(30)
011200101026     D isvftpeml       s              2    dim(100)
011300040816     D skTipF          s              2    dim(500)
011400040816     D skPgt           s             10    dim(500)
011500040816     D skFileT         s             10    dim(500)
011600100311     D skMembroT       s             10    dim(500)
011700040816     D j               s              3  0 inz
011800040816     D wVIDPGT         s                   like(VIDPGT)
011900040816     D wVIDFIT         s                   like(VIDFIT)
012000101026     D*-------------------
012100101026     D* DEFINIZIONI PROCEDURE ESTERNE
012200101026     D*-------------------
012300101026     D/COPY GAITRASRC/SRCPROTOPI,UBBVSQL01
012400101026     D/COPY GAITRASRC/SRCPROTOPR,UBBVSQL01
012500101027     D/COPY GAITRASRC/SRCPROTOPI,UBBVSKT01
012600101027     D/COPY GAITRASRC/SRCPROTOPR,UBBVSKT01
012700970526     C*------------------------------------------------------------------------*
012800970526     C* MAIN LINES
012900970526     C*------------------------------------------------------------------------*
013000970526     C*
013100970526     C* NON CI SONO ERRORI IN ENTRATA
013200970526IF  1C     ERRL00        IFEQ      '0'
013300970526     C*
013400970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
013500970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
013600970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
013700970526     C     FINL00        OREQ      '1'                                          *FINE PGM
013800970526     C*
013900970526     C* EMETTE FORMATO VIDEO
014000970526     C                   EXSR      BUTFMR
014100970526     C*
014200970526     C* GESTIONE TASTI FUNZIONALI
014300970526     C                   EXSR      FUNCON
014400970526E   2C                   ENDDO
014500970526     C*
014600970526     C* CI SONO ERRORI IN ENTRATA
014700970527X   1C                   ELSE
014800991025     C                   MOVEL     '1'           S20err
014900991025     C                   MOVEL     DSMSMS        S20msg
015000970526E   1C                   ENDIF
015100970526     C* FINE PGM
015200991210     c                   exsr      finsr
015300101026     C*
015400101026     C* Chiudo la connessione
015500101026     C                   exsr      clsCONN
015600101026     C*
015700970526     C                   SETON                                        LR
015800970526     C*------------------------------------------------------------------------*
015900970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
016000970526     C*------------------------------------------------------------------------*
016100970526     C     BUTFMR        BEGSR
016200970526     C*
016300970526     C* EMETTE LA TESTATA
016400991026     C                   WRITE     s723D01
016500970526     C*
016600970526     C* SPEDISCE EVENTUALE MESSAGGIO
016700970526     C                   EXSR      SNDMSG
016800970526     C*
016900970526     C* EMETTE LE FUNZIONI
017000991026     C                   WRITE     s723D02
017100970526     C*
017200970526     C* EMETTE IL VIDEO
017300991026     C                   EXFMT     s723D03
017400970526     C*
017500970526     C* REIMPOSTA FLAG DI ERRORE
017600970526     C                   MOVEL     '0'           ERRL00
017700970526     C*
017800970526     C* REIMPOSTA INDICATORI DI ERRORE
017900970526     C                   SETOFF                                         5859
018000970526     C                   SETOFF                                       606162
018100970526     C                   SETOFF                                       636465
018200970526     C                   SETOFF                                       666768
018300970526     C                   SETOFF                                       697071
018400970526     C                   SETOFF                                       727374
018500970526     C                   SETOFF                                       787980
018600970526     C                   SETOFF                                       818283
018700970526     C                   SETOFF                                       848586
018800970526     C                   SETOFF                                       878889
018900001120     C                   SETOFF                                       525354
019000001120     C                   SETOFF                                       555657
019100970526     C*
019200970526     C                   ENDSR
019300970526     C*------------------------------------------------------------------------*
019400970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
019500970526     C*------------------------------------------------------------------------*
019600970526     C     SNDMSG        BEGSR
019700970526     C*
019800970526     C* PULISCE IL SFILE MESSAGGI
019900970526     C                   MOVEL     '1'           *IN40
020000991026     C                   WRITE     s723DMC
020100970526     C                   MOVEL     '0'           *IN40
020200970526     C*
020300970526     C* VISUALIZZA IL MESSAGGIO
020400970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
020500970526     C                   CALL      'XMSG'        XMSGPA
020600970526IF  1C     DSMSER        IFEQ      *ZEROS
020700970526     C                   MOVEL     DSMSKE        MSGKEY
020800970526E   1C                   ENDIF
020900970526     C                   Z-ADD     1             MSGULT
021000991026     C                   WRITE     s723DMS
021100991026     C                   WRITE     s723DMC
021200970526     C*
021300970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
021400970526     C                   MOVEL     *BLANKS       DSMSMS
021500970526     C*
021600970526     C                   ENDSR
021700970526     C*------------------------------------------------------------------------*
021800970526     C* ENTGES - GESTIONE ENTER -> INVIO
021900970526     C*------------------------------------------------------------------------*
022000970526     C     ENTGES        BEGSR
022100970526     C*
022200970526     C* CONTROLLA I CAMPI IMMESSI
022300970526     C                   EXSR      DECODI
022400970526     C*
022500970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
022600970526IF  1C     DEPOP1        IFEQ      'O05'
022700970526     C                   MOVEL     '1'           FINL00
022800970526E   1C                   ENDIF
022900970526     C*
023000970526     C                   ENDSR
023100031229     C*------------------------------------------------------------------------*
023200031229     C* F01GES - GESTIONE F01 -> VISUALIZZA REGOLE PARAMETRI TRADUTTORE
023300031229     C*------------------------------------------------------------------------*
023400031229     C     F01GES        BEGSR
023500031229     C*
023600031229     C* Passo o il traduttore standard relativo al tipo file corrente o se inserito il
023700031229     C* traduttore specifico
023800031229     C                   IF        VIDPGT <> *blanks
023900031229     C                   EVAL      wPGT = VIDPGT
024000031229     C                   ELSE
024100031229     C                   EVAL      wPGT = VIDPGTD
024200031229     C                   ENDIF
024300031229     C*
024400031229     C* Visualizzo le regole relative
024500031229     C                   CALL      'TIS723C'
024600031229     C                   PARM                    wPGT             10
024700040107     C                   PARM                    wERR              1
024800040107     C*
024900040107     C* Verifico se trovato membro con descrizoine dettaglio parametri traduttore
025000040107     C                   IF        wERR = '1'
025100040107     C                   MOVEL     MSG(23)       DSMSMS
025200040107     C                   ENDIF
025300031229     C*
025400031229     C                   ENDSR
025500970526     C*------------------------------------------------------------------------*
025600970526     C* F03GES - GESTIONE F03 -> FINE
025700970526     C*------------------------------------------------------------------------*
025800970526     C     F03GES        BEGSR
025900970526     C*
026000970526     C* IMPOSTA "PREMUTO F03"
026100970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
026200991025     C                   MOVEL     '1'           S20F03                         *DI PROCEDURA
026300970526     C*
026400970526     C                   ENDSR
026500970526     C*------------------------------------------------------------------------*
026600970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
026700970526     C*------------------------------------------------------------------------*
026800970526     C     F06GES        BEGSR
026900970526     C*
027000970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
027100970526IF  1C     DEPOP1        IFEQ      'F06'
027200970526     C                   EXSR      INSERI
027300970526E   1C                   ENDIF
027400970526IF  1C     DEPOP1        IFEQ      'O02'
027500970526     C                   EXSR      MODIFI
027600970526E   1C                   ENDIF
027700991014IF  1C     DEPOP1        IFEQ      'O03'
027800011129     C                   EXSR      COPIA
027900991014E   1C                   ENDIF
028000991104IF  1C*    DEPOP1        IFEQ      'O04'
028100991104     C*                  EXSR      ANNULL
028200991104E   1C*                  ENDIF
028300970526IF  1C     DEPOP1        IFEQ      'O07'
028400970526     C                   EXSR      RIPRIS
028500970526E   1C                   ENDIF
028600080603     C*
028700080603     C* Gestione estensione anagrafica clienti/tipi file => TRC 01
028800080603     C                   CLEAR                   DVIRE01
028900080603     C                   EVAL      KCEdti  = virDTI
029000080603     C                   EVAL      vireTRC = '01'
029100080603     C     KEYvire1_C    CHAIN     tivire1l
029200080603     C                   IF        %found(tivire1l)
029300080603     C                   IF        vidEML <> *blanks
029400080603     C                   EVAL      §vire01INV = 'S'
029500080603     C                   EVAL      §vire01EML = vidEML
029600080603     C                   EVAL      vireFLO = DVIRE01
029700080603     C                   UPDATE    tivire00
029800080603     C                   ELSE
029900080603     C                   DELETE    tivire00
030000080603     C                   ENDIF
030100080603     C                   ELSE
030200080603     C                   IF        vidEML <> *blanks
030300080603     C                   EVAL      vireKSC = virKSC
030400080603     C                   EVAL      vireTIP = virTIP
030500080603     C                   EVAL      vireDTI = virDTI
030600080603     C                   EVAL      vireTRC = '01'
030700080603     C                   EVAL      §vire01INV = 'S'
030800080603     C                   EVAL      §vire01EML = vidEML
030900080603     C                   EVAL      vireFLO = DVIRE01
031000080603     C                   WRITE     tivire00
031100080603     C                   ENDIF
031200080603     C                   ENDIF
031300970526     C*
031400011129     C* Dopo aver effettuato le operazioni richieste apro la tabela correlata al tipo file.
031500011129     C                   IF        errl00 = '0'                                 *NO ERRORI
031600011129     C                   CALL      'TIS70UR'
031700011129     C                   PARM                    KPJBA
031800011129     C                   PARM                    VIDTIP
031900011129     C                   PARM                    VIDKSC
032000011129     C                   PARM      *blanks       flgope            1
032100011129     C                   ENDIF
032200011129     C*
032300970526     C                   ENDSR
032400970526     C*------------------------------------------------------------------------*
032500970526     C* F12GES - GESTIONE F12 -> RITORNO
032600970526     C*------------------------------------------------------------------------*
032700970526     C     F12GES        BEGSR
032800970526     C*
032900970526     C* IMPOSTA "PREMUTO F12"
033000970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
033100991025     C                   MOVEL     '1'           S20F12                         *DI PROCEDURA
033200970526     C*
033300970526     C                   ENDSR
033400970526     C*------------------------------------------------------------------------*
033500970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
033600970526     C*------------------------------------------------------------------------*
033700970526     C     F24GES        BEGSR
033800970526     C*
033900970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
034000970526     C*
034100970526     C                   ENDSR
034200970526     C*------------------------------------------------------------------------*
034300970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
034400970526     C*------------------------------------------------------------------------*
034500970526     C     INSERI        BEGSR
034600970526     C*
034700970526     C* CONTROLLA I CAMPI IMMESSI
034800970526     C                   EXSR      DECODI
034900970526     C*
035000970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
035100991026IF  1C                   if        errl00 = '0'                                 *NO ERRORI
035200991026     C                   movel     VIDTip        virtip
035300991026     C                   move      VIDksc        virksc
035400991026     C                   z-add     VIDseq        virseq
035500991026     C                   Z-ADD     viddti        G08dat
035600991026     C                   Z-ADD     *ZEROS        G08inv
035700991026     C                   MOVEL     '0'           G08ERR
035800991026     C                   CALL      'XSRDA8'
035900991026     C                   PARM                    WLBDA8
036000991026     C                   Z-ADD     G08inv        virdti
036100991026     c                   exsr      impvir
036200991026     C                   WRITE     tivir000
036300991025     C                   MOVEL     MSG(2)        S20MSG                         *MESSAGGIO
036400970602     C*
036500970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
036600020228     C****               EXSR      AZZVID
036700970526E   1C                   ENDIF
036800970526     C*
036900970526     C                   ENDSR
037000970526     C*------------------------------------------------------------------------*
037100970526     C* MODIFI - OPERAZIONI DI MODIFICA
037200970526     C*------------------------------------------------------------------------*
037300970526     C     MODIFI        BEGSR
037400970526     C*
037500970526     C* CONTROLLA I CAMPI IMMESSI
037600970526     C                   EXSR      DECODI
037700970526     C*
037800970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
037900970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
038000991026     C                   EXSR      LETvir
038100970526     C*
038200970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
038300970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
038400991026     C                   EXSR      IMPvir                                       *IMPOSTA I CAMPI
038500991026     C                   UPDATE    tivir000
038600101027     C                   EVAL      S20MSG = MSG(3)
038700101027     C                   EVAL      DSMSMS = MSG(3)
038800101027     C*
038900101027     C* Se trattasi d tipo servizio "idoneo" modifico anche i dati utente lato "BARTVAS"
039000101027     C                   IF        *IN80
039100101027      /FREE
039200101027
039300101027         if *in30 = *off;
039400101027           pInMaskChk  = VIDPPIMC;
039500101027         else;
039600101027           pInMaskChk  = pOutCliMskChk;
039700101027         endif;
039800101027
039900101027         if *in31 = *off;
040000101027           pInMaskFile = VIDPPIMF;
040100101027         else;
040200101027           pInMaskFile = pOutCliMskFil;
040300101027         endif;
040400101027
040500101027         if *in32 = *off;
040600101027           pInMaskSem  = VIDPPIMS;
040700101027         else;
040800101027           pInMaskSem  = pOutCliMskSem;
040900101027         endif;
041000101027
041100101027         if *in33 = *off;
041200101027           pInMaskZip  = VIDPPIMZ;
041300101027         else;
041400101027           pInMaskZip  = pOutCliMskZip;
041500101027         endif;
041600101027
041700101027
041800101027         if UBBVSQL01_Upd01UnqCliTipFile(
041900101027                         pInConn
042000101027                       : VIDKSC
042100101027                       : VIDTIP
042200101027                       : pInMaskChk
042300101027                       : pInMaskFile
042400101027                       : pInMaskSem
042500101027                       : pInMaskZip) = 0;
042600101027         else;
042700101027              S20MSG = MSG(29);
042800101027              DSMSMS = MSG(29);
042900101027         endif;
043000101027
043100101027      /END-FREE
043200101027     C
043300101027     C*
043400101027     C* Quindi (già che c sono ricarico anche il job
043500101027     C                   callp(e)  UBBVSKT01_ReloadCliTipF(
043600101027     C                                           VIDKSC
043700101027     C                                          :VIDTIP
043800101027     C                                          :pOutCliProc)
043900101027     C                   ENDIF
044000101027     C*
044100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
044200970526E   2C                   ENDIF
044300970526E   1C                   ENDIF
044400970526     C*
044500970526     C                   ENDSR
044600991014     C*------------------------------------------------------------------------*
044700991014     C* copia  - OPERAZIONI DI copia
044800991014     C*------------------------------------------------------------------------*
044900991014     C     copia         BEGSR
045000991014     C*
045100991014     C* CONTROLLA I CAMPI IMMESSI
045200991014     C                   EXSR      DECODI
045300991014     C*
045400991014     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
045500991014IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
045600991026     C                   movel     VIDTip        virtip
045700991110     C                   move      VIDksc        virksc
045800991110     C                   z-add     VIDseq        virseq
045900991110     C                   Z-ADD     viddti        G08dat
046000991110     C                   Z-ADD     *ZEROS        G08inv
046100991110     C                   MOVEL     '0'           G08ERR
046200991110     C                   CALL      'XSRDA8'
046300991110     C                   PARM                    WLBDA8
046400991110     C                   Z-ADD     G08inv        virdti
046500991026     C                   EXSR      IMPvir                                       *IMPOSTA I CAMPI
046600991026     C                   write     tivir000
046700991025     C                   MOVEL     MSG(3)        S20MSG
046800991014     C                   MOVEL     '1'           FINL00                         *FINE PGM
046900991014E   2C                   ENDIF
047000991014     C*
047100991014     C                   ENDSR
047200970526     C*------------------------------------------------------------------------*
047300970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
047400970526     C*------------------------------------------------------------------------*
047500991104     C*    ANNULL        BEGSR
047600991025     C*
047700991025     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
047800991104     C*                  EXSR      LETvir
047900991025     C*
048000991025     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
048100991104IF  1C*    ERRL00        IFEQ      '0'                                          *NO ERRORI
048200991104     C*                  DELETE    tivir000                                     *CANCELLA
048300991104     C*                  MOVEL     MSG(4)        s20MSG                         *MESSAGGIO
048400991104     C*                  MOVEL     '1'           FINL00                         *FINE PGM
048500991104E   1C*                  ENDIF
048600970526     C*
048700991104     C*                  ENDSR
048800970526     C*------------------------------------------------------------------------*
048900970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
049000970526     C*------------------------------------------------------------------------*
049100970526     C     RIPRIS        BEGSR
049200970526     C                   ENDSR
049300970526     C*------------------------------------------------------------------------*
049400991026     C* LETvir - LEGGE IL RECORD PERL'AGGIORNAMENTO
049500970526     C*------------------------------------------------------------------------*
049600991026     C     LETvir        BEGSR
049700970526     C*
049800991025     C                   MOVEL     S20tip        KCEtip
049900991026     C                   MOVEL     S20ksc        KCEksc
050000000420     C     KEYvi2        CHAIN     tivir03L                           9998
050100970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
050200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
050300991025     C                   MOVEL     MSG(7)        S20MSG                         *MESSAGGIO
050400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
050500970526E   1C                   ENDIF
050600970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
050700970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
050800991025     C                   MOVEL     MSG(6)        S20MSG                         *MESSAGGIO
050900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
051000970526E   1C                   ENDIF
051100970526     C*
051200970526     C                   ENDSR
051300970526     C*------------------------------------------------------------------------*
051400970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
051500970526     C*------------------------------------------------------------------------*
051600970526     C     DECODI        BEGSR
051700001120     C*
051800001120     C*----------------------
051900001120     C* Controllo tipo file
052000001120     C*----------------------
052100001120     C     ERRL00        IFEQ      '0'
052200001120     C     VIDtip        IFne      *blank
052300001120     C                   exsr      cnttip
052400001120     C                   ELSE
052500991008     C                   MOVEL     '1'           ERRL00
052600991011     C                   SETON                                        66
052700991027     C                   MOVEL     MSG(12)       DSMSMS
052800001120     C                   ENDIF
052900001120     C                   ENDIF
053000001120     C*----------------------
053100001120     C* Controllo del codice
053200001120     C*----------------------
053300001120     C     ERRL00        IFEQ      '0'
053400001120     C     VIDksc        IFne      *blank
053500001120     C     VIDksc        andne     *zeros
053600001120     C                   exsr      cntksc
053700001120     C                   ELSE
053800991027     C                   MOVEL     '1'           ERRL00
053900991027     C                   SETON                                        65
054000991027     C                   MOVEL     MSG(12)       DSMSMS
054100001120     C                   ENDIF
054200001120     C                   ENDIF
054300101026     C*
054400101026     C* Verifico se x il servizio a video è richiesta l'indicazione della password FTP
054500101026     C     ERRL00        IFEQ      '0'
054600101026     C     VIDisv        lookup    isvftpeml                              80
054700101026 xxx C***                setoff                                       80
054800101026     C                   ENDIF
054900001120     C*----------------------
055000001120     C* Controllo della sequenza
055100001120     C*----------------------
055200001120     C     ERRL00        IFEQ      '0'
055300001120     C     VIDseq        IFne      0
055400001120     C                   ELSE
055500991027     C                   MOVEL     '1'           ERRL00
055600991027     C                   SETON                                        67
055700991027     C                   MOVEL     MSG(12)       DSMSMS
055800001120     C                   ENDIF
055900001120     C                   ENDIF
056000001120     C*----------------------
056100001120     C* Tipo servizio
056200001120     C*----------------------
056300001120     C     ERRL00        IFEQ      '0'
056400001120     C     VIDisv        IFne      *blank
056500001120     C                   exsr      cntisv
056600040319     C* Verifico la compatibilità del servizio rispetto al tipo-file
056700040319     C                   IF        VTFUD <> §ISVVERS
056800040319     C                   MOVEL     '1'           ERRL00
056900040319     C                   SETON                                        6670
057000040319     C                   MOVEL     MSG(24)       DSMSMS
057100040319     C                   ENDIF
057200001120     C                   ELSE
057300000626     C                   MOVEL     '1'           ERRL00
057400000626     C                   SETON                                        70
057500000626     C                   MOVEL     MSG(12)       DSMSMS
057600001120     C                   ENDIF
057700001120     C                   ENDIF
057800100614     C*--------------------
057900100614     C* Tipo membro d default...  solo x anagrafiche d download (ovviamente no in visualizz.)
058000100614     C*--------------------
058100100614     C     ERRL00        IFEQ      '0'
058200100614     C                   IF        DEPOP1 = 'F06' OR
058300100614     C                             DEPOP1 = 'O03'
058400100614     C                   IF        §ISVPROC = 'F' AND
058500100614     C                             §ISVVERS = 'D'
058600100614     C                   IF        VIDtmbd = *blanks AND
058700100614     C                             vtfud   = 'D'
058800100614     C                   MOVEL     'M'           VIDtmbd
058900100614     C                   ENDIF
059000100614     C                   ENDIF
059100100614     C                   ENDIF
059200100614     C                   ENDIF
059300030603     C*----------------------
059400030603     C* Tipo membro
059500030603     C*----------------------
059600030603     C     ERRL00        IFEQ      '0'
059700030603     C     VIDtmbd       IFNE      *blank
059800030603     C                   IF        §ISVPROC = 'F' AND
059900030603     C                             §ISVVERS = 'D'
060000030603     C                   ELSE
060100030603     C                   MOVEL     '1'           ERRL00
060200030603     C                   SETON                                        61
060300030603     C                   MOVEL     MSG(22)       DSMSMS
060400030603     C                   ENDIF
060500030603     C                   ENDIF
060600030603     C                   ENDIF
060700001120     C*----------------------
060800001120     C* Controllo della descrizione
060900001120     C*----------------------
061000001120     C     ERRL00        IFEQ      '0'
061100001120     C     VIDdes        IFne      *blanks
061200001120     C                   ELSE
061300991122     C*                  MOVEL     '1'           ERRL00
061400991122     C*                  SETON                                        62
061500991122     C*                  MOVEL     MSG(12)       DSMSMS
061600991122     C                   MOVEL     VDDTIP        VIDDES
061700001120     C                   ENDIF
061800001120     C                   ENDIF
061900991026     C*--------------------
062000991026     C* DATA inizio
062100991026     C*--------------------
062200991026     C     *LIKE         DEFINE    VIDDti        DtiVID
062300001120     C     ERRL00        IFEQ      '0'
062400991026     C                   Z-ADD     VIDDti        wda8
062500991026     C                   Z-ADD     *ZEROS        da8w
062600991026     C                   EXSR      CHKDA8
062700991026     C                   Z-ADD     WDA8          viddti
062800991026     C                   Z-ADD     DA8W          dtivid
062900001120     C     ERRL00        IFEQ      '1'
063000991026     C                   SETON                                        63
063100991026     C                   MOVEL     MSG(14)       dsmsms
063200001120     C                   ENDIF
063300001120     C                   ENDIF
063400991026     C* INSERIMENTO -> RECORD INESISTENTE
063500001120     C     ERRL00        IFEQ      '0'
063600001120     C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
063700001120     C     DEPOP1        orEQ      'O03'                                        *INSERIMENTO
063800991026     C                   move      vidksc        KCEksc
063900000420     C                   move      vidtip        KCEtip
064000000420     C                   z-add     dtivid        KCEdti
064100000420     C     KEYvi2        CHAIN(N)  tivir03L                           99
064200001120     C     *IN99         IFEQ      *OFF
064300991026     C                   MOVEL     '1'           ERRL00
064400991026     C                   MOVEL     MSG(8)        DSMSMS
064500991026     C                   SETON                                        676566
064600991026     C                   SETON                                        63
064700001120     C                   ENDIF
064800001120     C                   ENDIF
064900001120     C                   ENDIF
065000001120     C*--------------------
065100991026     C* DATA SCADENZA
065200991026     C*--------------------
065300991026     C     *LIKE         DEFINE    VIDDtf        DtfVID
065400001120     C     ERRL00        IFEQ      '0'
065500991026     C                   Z-ADD     VIDDtf        WDA8
065600991026     C                   Z-ADD     *ZEROS        DA8W
065700991026     C                   EXSR      CHKDA8
065800991026     C                   Z-ADD     WDA8          VIDDtf
065900991026     C                   Z-ADD     DA8W          DtfVID
066000001120     C     ERRL00        IFEQ      '1'
066100991026     C                   SETON                                        68
066200991026     C                   MOVEL     MSG(14)       DSMSMS
066300001120     C                   ENDIF
066400001120     C                   ENDIF
066500991026     C* SCADENZA < DECORRENZA
066600001120     C     ERRL00        IFEQ      '0'
066700001120     C     DtfVID        IFLT      DtiVID
066800991026     C                   SETON                                        68
066900991026     C                   MOVEL     MSG(15)       DSMSMS
067000991026     C                   MOVEL     '1'           ERRL00
067100001120     C                   ENDIF
067200001120     C                   ENDIF
067300991026     C* SCADENZA >= DATA CORRENTE
067400001120     C     ERRL00        IFEQ      '0'
067500001120     C     DEPOP1        IFEQ      'O02'
067600991026     C     DEPOP1        OREQ      'O07'
067700991026     C     DEPOP1        OREQ      'F06'
067800001120     C     DtfVID        IFLT      DATCOR
067900001120     C                   SETON                                        68
068000991026     C                   MOVEL     MSG(16)       DSMSMS
068100991026     C                   MOVEL     '1'           ERRL00
068200001120     C                   ENDIF
068300001120     C                   ENDIF
068400001120     C                   ENDIF
068500991026     C*---
068600991026     C* DECORRENZA + SCADENZA
068700991026     C*---
068800991026     C* ACCAVALLAMENTO DATI
068900001120     C     ERRL00        IFEQ      '0'
069000991026     C                   MOVEL     VIDtip        KCEtip
069100991026     C                   MOVEL     vidksc        KCEksc
069200991026     C     KEyvir        SETLL     tivir03L
069300991026     C     KEyvir        READE(N)  tivir03L                               99
069400001120     C     *IN99         DOWEQ     *OFF
069500001120     C     DEPOP1        IFNE      'F06'                                        *NON INSERIMENTO
069600991026     C     DtiVID        ANDNE     virdti                                       *RECORD IN LINEA
069700991026     C     DEPOP1        OREQ      'F06'                                        *INSERIMENTO
069800001120     C     DtiVID        IFGE      virdti                                       *DECORRENZA
069900991026     C     DtiVID        ANDLE     virdtf
070000991026     C     DtfVID        ORGE      virdti                                       *SCADENZA
070100991026     C     DtfVID        ANDLE     virdtf
070200991026     C     virdti        ORGE      DtiVID                                       *DECORRENZA
070300991026     C     virdtf        ANDLE     DtfVID
070400991026     C                   SETON                                        666765
070500991026     C                   SETON                                        6368
070600991026     C                   MOVEL     MSG(17)       DSMSMS
070700991026     C                   MOVEL     '1'           ERRL00
070800991026     C                   LEAVE
070900001120     C                   ENDIF
071000001120     C                   ENDIF
071100991026     C     KEyvir        READE(N)  tivir03L                               99
071200001120     C                   ENDDO
071300001120     C                   ENDIF
071400001120     C*--------------------
071500001120     C* Controllo filiale
071600001120     C*--------------------
071700001120     C     ERRL00        IFEQ      '0'
071800001120     C     VIDpoi        IFne      *blank
071900001120     C     VIDpoi        andne     *zeros
072000001120     C                   exsr      cntpoi
072100001120     C                   ELSE
072200000724     C     vtfud         IFNE      'D'
072300991027     C                   MOVEL     '1'           ERRL00
072400000724     C                   MOVEL     MSG(12)       DSMSMS
072500991027     C                   SETON                                        69
072600991122     C                   ENDIF
072700001120     C                   ENDIF
072800001120     C                   ENDIF
072900001120     C*--------------------
073000011016     C* Programma traduttore
073100001120     C*--------------------
073200001120     C     ERRL00        IFEQ      '0'
073300001120     C     VIDpgt        IFNE      *blank
073400001120     C                   CALL(e)   'TIS721C'
073500050112     C                   PARM      *blanks       LIBRERIA         10
073600001120     C                   PARM      VIDpgt        OBJECT           10
073700001120     C                   PARM      '*PGM   '     TYPE              7
073800001120     C                   PARM      '0'           ESITO             1
073900001120     C                   IF        ESITO = '1'
074000001120     C                   MOVEL     '1'           ERRL00
074100001120     C                   SETON                                        54
074200001120     C                   MOVEL     MSG(19)       DSMSMS
074300001120     C                   ENDIF
074400001120     C                   ENDIF
074500001120     C                   ENDIF
074600001120     C*--------------------
074700001120     C* Programma post-traduzione
074800001120     C*--------------------
074900001120     C     ERRL00        IFEQ      '0'
075000001120     C     VIDpgi        IFNE      *blank
075100001120     C                   CALL(e)   'TIS721C'
075200050112     C                   PARM      *blanks       LIBRERIA         10
075300001120     C                   PARM      VIDpgi        OBJECT           10
075400001120     C                   PARM      '*PGM   '     TYPE              7
075500001120     C                   PARM      '0'           ESITO             1
075600001120     C                   IF        ESITO = '1'
075700001120     C                   MOVEL     '1'           ERRL00
075800001120     C                   SETON                                        55
075900001120     C                   MOVEL     MSG(19)       DSMSMS
076000001120     C                   ENDIF
076100001120     C                   ENDIF
076200001120     C                   ENDIF
076300001120     C*--------------------
076400001120     C* File da tradurre
076500001120     C*--------------------
076600001120     C     ERRL00        IFEQ      '0'
076700001120     C     VIDfid        IFNE      *blank
076800001120     C                   CALL(e)   'TIS721C'
076900050112     C                   PARM      *blanks       LIBRERIA         10
077000001120     C                   PARM      VIDfid        OBJECT           10
077100001120     C                   PARM      '*FILE  '     TYPE              7
077200001120     C                   PARM      '0'           ESITO             1
077300001120     C                   IF        ESITO = '1'
077400001120     C                   MOVEL     '1'           ERRL00
077500001120     C                   SETON                                        53
077600001120     C                   MOVEL     MSG(20)       DSMSMS
077700001120     C                   ENDIF
077800001120     C                   ENDIF
077900001120     C                   ENDIF
078000001120     C*--------------------
078100001120     C* File storico
078200001120     C*--------------------
078300001120     C     ERRL00        IFEQ      '0'
078400001120     C     VIDfis        IFNE      *blank
078500001120     C                   CALL(e)   'TIS721C'
078600050112     C                   PARM      *blanks       LIBRERIA         10
078700001120     C                   PARM      VIDfis        OBJECT           10
078800001120     C                   PARM      '*FILE  '     TYPE              7
078900001120     C                   PARM      '0'           ESITO             1
079000001120     C                   IF        ESITO = '1'
079100001120     C                   MOVEL     '1'           ERRL00
079200001120     C                   SETON                                        56
079300001120     C                   MOVEL     MSG(20)       DSMSMS
079400001120     C                   ENDIF
079500001120     C                   ENDIF
079600001120     C                   ENDIF
079700001120     C*--------------------
079800001120     C* File tradotto
079900001120     C*--------------------
080000001120     C     ERRL00        IFEQ      '0'
080100001120     C     VIDfit        IFNE      *blank
080200001120     C                   CALL(e)   'TIS721C'
080300050112     C                   PARM      VIDfi3        LIBRERIA         10
080400001120     C                   PARM      VIDfit        OBJECT           10
080500001120     C                   PARM      '*FILE  '     TYPE              7
080600001120     C                   PARM      '0'           ESITO             1
080700001120     C                   IF        ESITO = '1'
080800001120     C                   MOVEL     '1'           ERRL00
080900001120     C                   SETON                                        57
081000001120     C                   MOVEL     MSG(20)       DSMSMS
081100001120     C                   ENDIF
081200001120     C                   ENDIF
081300001120     C                   ENDIF
081400001120     C*--------------------
081500001120     C* Libreria file tradotto
081600001120     C*--------------------
081700001120     C     ERRL00        IFEQ      '0'
081800001120     C     VIDfi3        IFNE      *blank
081900001120     C                   CALL(e)   'TIS721C'
082000050112     C                   PARM      *blanks       LIBRERIA         10
082100001120     C                   PARM      VIDfi3        OBJECT           10
082200001120     C                   PARM      '*LIB   '     TYPE              7
082300001120     C                   PARM      '0'           ESITO             1
082400001120     C                   IF        ESITO = '1'
082500001120     C                   MOVEL     '1'           ERRL00
082600001120     C                   SETON                                        52
082700001120     C                   MOVEL     MSG(21)       DSMSMS
082800001120     C                   ENDIF
082900001120     C                   ENDIF
083000001120     C                   ENDIF
083100040816     C*--------------------
083200100311     C* Controllo corrispondenza Tipo File / Programma Traduttore / File Tradotto / Tipo membro Trad
083300040816     C*--------------------
083400040816     C     ERRL00        IFEQ      '0'
083500040816     C     VIDfit        IFne      *blank
083600040816     C     VIDpgt        ORne      *blank
083700040816     C                   EVAL      TBEcod = 'IFT'
083800040816     C     KEYtbe1_P     SETLL     tntbe01l
083900040816     C                   IF        %equal(tntbe01l)
084000040816     C* Inizializzo indici e schiere
084100040816     C                   Z-ADD     *zeros        j
084200040816     C                   CLEAR                   skTipF
084300040816     C                   CLEAR                   skPgt
084400040816     C                   CLEAR                   skFileT
084500100311     C                   CLEAR                   skMembroT
084600040816     C* Carico le schiere da tabella 'IFT'
084700040816     C     KEYtbe1_P     READE     tntbe01l
084800040816     C                   DOW       not %eof(tntbe01l)
084900040816     C                   IF        TBEatb = *blanks
085000040816     C                   EVAL      DIFT = TBEuni
085100040816     C                   ADD       1             j
085200100311     C                   EVAL      skTipF(j)    = TBEke1
085300100311     C                   EVAL      skPgt(j)     = §IFTPGT
085400100311     C                   EVAL      skFileT(j)   = §IFTFILE
085500100311     C                   EVAL      skMembroT(j) = §IFTTMBD
085600040816     C                   ENDIF
085700040816     C     KEYtbe1_P     READE     tntbe01l
085800040816     C                   ENDDO
085900040816     C* Imposto cosa testare
086000040816     C                   IF        VIDPGT <> *blanks
086100040816     C                   EVAL      wVIDPGT = VIDPGT
086200040816     C                   ELSE
086300040816     C                   EVAL      wVIDPGT = VIDPGTD
086400040816     C                   ENDIF
086500040816     C                   IF        VIDFIT <> *blanks
086600040816     C                   EVAL      wVIDFIT = VIDFIT
086700040816     C                   ELSE
086800040816     C                   EVAL      wVIDFIT = VIDFITD
086900040816     C                   ENDIF
087000040816     C* Testo se il traduttore indicato è tra quelli da controllare (in tabella 'IFT')
087100040823     C                   MOVEL     '0'           wFlgIFTok         1
087200100223     C                   SETOFF                                       16
087300040816     C     wVIDPGT       LOOKUP    skPgt                                  15
087400040816     C                   IF        %found
087500040816     C* Testo se vi è corrispondenza
087600040816     C                   EVAL      j = 1
087700040816     C                   DOW       j <= %elem(skTipF)
087800100223     C                   IF        skTipF(j)  = VIDTIP  AND
087900100223     C                             skPgt(j)   = wVIDPGT
088000100223     C                   SETON                                        16
088100100311     C*
088200100223     C                   IF        skFileT(j) = wVIDFIT
088300040816     C                   MOVEL     '1'           wFlgIFTok
088400100311     C*
088500100311     C                   IF        DEPOP1 = 'O02' OR
088600100311     C                             DEPOP1 = 'O03' OR
088700100311     C                             DEPOP1 = 'F06'
088800100312     C                   IF        VIDtmbd      =  *blanks AND
088900100312     C                             skMembroT(j) <> *blanks
089000100311     C                   EVAL      VIDtmbd = skMembroT(j)
089100100311     C                   SETON                                        61
089200100311     C                   ENDIF
089300100311     C                   ENDIF
089400100311     C*
089500040816     C                   LEAVE
089600040816     C                   ENDIF
089700100311     C*
089800100223     C                   ENDIF
089900040816     C                   ADD       1             j
090000040816     C                   ENDDO
090100040816     C* Se nn trovata esatta corrispondenza => errore
090200100223     C                   IF        *IN16 and wFlgIFTok = '0'
090300040816     C                   MOVEL     '1'           ERRL00
090400040816     C                   MOVEL     MSG(25)       DSMSMS
090500040816     C                   SETON                                        665457
090600040816     C                   ENDIF
090700040816     C                   ENDIF
090800040816     C                   ENDIF
090900040816     C                   ENDIF
091000040823     C                   ENDIF
091100051026     C*--------------------
091200051026     C* Controllo particolare x anagrafiche d download: se si sta cambiando il traduttore
091300051026     C* verifico prima che nn vi siano attualmente record d log a stato zero
091400051026     C*--------------------
091500051026     C     ERRL00        IFEQ      '0'
091600051026     C     vtfud         IFEQ      'D'
091700051026     C     VIDpgt        IFNE      virPGT
091800051026     C     VIDpgi        ORNE      virPGI
091900051026     C                   CALL      'TIS723R1'
092000051026     C                   PARM                    VIDKSC
092100051026     C                   PARM                    VIDTIP
092200051026     C                   PARM                    VTFUD
092300051026     C                   PARM      *zeros        wRecZero          9 0
092400051026     C                   IF        wRecZero > *zeros
092500051026     C                   MOVEL     '1'           ERRL00
092600051026     C                   MOVEL     MSG(26)       DSMSMS
092700051026     C                   SETON                                        5455
092800051026     C                   ENDIF
092900051026     C                   ENDIF
093000051026     C                   ENDIF
093100051026     C                   ENDIF
093200060616     C*--------------------
093300060616     C* File Da tradurre / File Storico / Programma Post-Traduzione
093400060616     C*--------------------
093500060616     C     ERRL00        IFEQ      '0'
093600060616     C                   IF        (VIDfid='*' AND VIDfis='*' AND VIDpgi='*') OR
093700060616     C                             (VIDfid<>'*' AND VIDfis<>'*' AND VIDpgi<>'*')
093800060616     C                   ELSE
093900060616     C                   MOVEL     '1'           ERRL00
094000060616     C                   SETON                                        535556
094100060616     C                   MOVEL     MSG(27)       DSMSMS
094200060616     C                   ENDIF
094300060616     C                   ENDIF
094400081020     C*--------------------
094500081020     C* Mail x eventuale feed-back richiesto
094600081020     C*--------------------
094700081020     C     ERRL00        IFEQ      '0'
094800081020     C                   CLEAR                   DSEMAIL
094900081020     C                   EVAL      §EMLINDI = VIDEML
095000081020     C                   CALL      'XEMAIL'
095100081020     C                   PARM                    DSEMAIL
095200081020     C                   IF        §EMLERRO = '1'
095300081020     C                   MOVEL     '1'           ERRL00
095400081020     C                   SETON                                        64
095500081020     C                   MOVEL     §EMLMSGO      DSMSMS
095600081020     C                   ENDIF
095700081020     C                   ENDIF
095800100311     C*--------------------
095900100311     C* Dati solo x upload no in download
096000100311     C*--------------------
096100100311     C     ERRL00        IFEQ      '0'
096200100311     C                   IF        vtfud   = 'D'
096300100311     C                   MOVEL     *blanks       VIDpoi
096400100311     C                   MOVEL     *blanks       VIDfi2
096500100311     C                   ENDIF
096600100311     C                   ENDIF
096700001120     C*
096800001120     C                   ENDSR
096900000626     C*------------------------------------------------------------------------*
097000000626     C* Cntisv - CONTROLLA codice tipo legame
097100000626     C*------------------------------------------------------------------------*
097200001120     C     cntisv        begsr
097300001120     C                   clear                   TIBS02DS
097400001120     C*
097500101027     C     '?'           SCAN      Vidisv                                 28
097600001120     C*  Ricerca
097700001120     C                   eval      t02tla = 'L'
097800101027     C                   IF        *IN28 = *ON
097900001120     C                   movel     'R'           t02mod
098000001120     C                   else
098100001120     C                   movel     'C'           t02mod
098200001120     C                   movel(p)  vidisv        t02ke1
098300001120     C                   end
098400000626     C                   movel     'ISV'         t02cod
098500001120     C                   movel     knsif         t02sif
098600001120     C                   CALL      'TIBS02R'
098700001120     C                   parm                    KPJBA
098800001120     C                   parm                    TIBS02DS
098900001120     C
099000001120     C*
099100001120     C                   IF        T02err = *blanks
099200000626     C                   movel     T02ke1        Vidisv
099300000626     C                   movel     T02uni        vddisv
099400030603     C                   movel(P)  T02uni        DISV
099500001120     C                   else
099600001120     C                   movel     '1'           errl00
099700001120     C                   seton                                        70
099800001120     C                   MOVEL     MSG(18)       DSMSMS
099900001120     C                   ENDIF
100000001120     C
100100001120     C                   endsr
100200991027     C*------------------------------------------------------------------------*
100300991027     C* cnttip - Controlla esistenza tipo file su tivt00f
100400991027     C*------------------------------------------------------------------------*
100500001120     C     cnttip        begsr
100600001120     C                   clear                   TIBS02DS
100700000626     C*
100800101027     C     '?'           SCAN      vidtip                                 28
100900001120     C*  Ricerca
101000001120     C                   eval      t02tla = 'L'
101100101027     C                   IF        *IN28 = *ON
101200001120     C                   movel     'R'           t02mod
101300001120     C                   else
101400001120     C                   movel     'C'           t02mod
101500001120     C                   movel(p)  vidtip        t02ke1
101600001120     C                   end
101700991108     C                   movel     'ITF'         t02cod
101800001120     C                   movel     knsif         t02sif
101900991108     C                   CALL      'TIBS02R'
102000001120     C                   parm                    KPJBA
102100001120     C                   parm                    TIBS02DS
102200001120     C
102300001120     C*
102400001120     C                   IF        T02err = *blanks
102500991108     C                   movel     T02ke1        vidtip
102600020218     C                   movel     T02uni        DITF
102700020218     C                   movel     §ITFDESC      vddtip
102800021003     C* Sostituisco con il dato proveniente dalla tabella solo se nn già indicato
102900021003     C* qualcosa nelle regole specifiche x il cliente/tipo file.
103000021014     C                   if        vidfit = *blanks
103100021014     C                   movel     §ITFLIBD      vidfi2
103200021014     C                   endif
103300001120     C                   else
103400001120     C                   movel     '1'           errl00
103500991108     C                   movel     *blank        vddtip
103600001120     C                   seton                                        66
103700991108     C                   MOVEL     msg(11)       DSMSMS
103800991108     C                   ENDIF
103900001120     C                   if        errl00 = '0'
104000001120     C     vidtip        chain     tivtf01l                           97
104100001120     C                   if        not *in97
104200001120     C                   movel     vtfpgt        vidpgtd
104300001120     C                   movel     vtfpgi        vidpgid
104400001120     C                   movel     vtffid        vidfidd
104500001120     C                   movel     vtffis        vidfisd
104600001120     C                   movel     vtffit        vidfitd
104700101013     C                   movel     vtfppi        DVTFPPI
104800101026     C                   if        DEPOP1 = 'F06' OR
104900101026     C                             DEPOP1 = 'O03'
105000101026     C                   select
105100101026     C                   when      vtfUD = 'U'
105200101026     C                   movel     D§VTFPPIMF    vidppiMC
105300101026     C                   when      vtfUD = 'D'
105400101026     C                   movel     D§VTFPPIMF    vidppiMF
105500101026     C                   endsl
105600101026     C                   movel     D§VTFPPIMS    vidppiMS
105700101026     C                   movel     D§VTFPPIMZ    vidppiMZ
105800101026     C                   endif
105900100308     C                   if        vidfi4d <> *blanks and vidfi4d <> '*'
106000001120     C                   eval      vidfi4d = %subst(vidfitd:1:5) + '00F'
106100100308     C                   endif
106200001120     C                   else
106300001120     C                   movel     *blank        vidpgtd
106400001120     C                   movel     *blank        vidpgid
106500001120     C                   movel     *blank        vidfidd
106600001120     C                   movel     *blank        vidfisd
106700001120     C                   movel     *blank        vidfitd
106800001120     C                   movel     *blank        vidfi4d
106900001120     C                   endif
107000001120     C                   endif
107100001120     C*
107200001120     C                   endsr
107300991027     C*------------------------------------------------------------------------*
107400040901     C* cntksc - Controlla esistenza codice
107500991027     C*------------------------------------------------------------------------*
107600001120     C     cntksc        begsr
107700010913     C*
107800010913     C     vidksc        chain     tianv01l
107900010913     C                   if        %found
108000010913     C                   movel     ANVDES        vddksc
108100010913     C                   else
108200001120     C                   move      vidksc        wcli              7 0
108300991210     C                   CLEAR                   BS69DS
108400991210     C                   CLEAR                   ACODS
108500991210     C                   MOVEL     KNSIF         I69SIF
108600991210     C                   Z-ADD     WCLI          I69KAC
108700991210     C                   CALL      'TIBS69R'
108800991210     C                   PARM                    BS69DS
108900991210     C                   PARM                    ACODS
109000991210     C                   PARM                    INDDS
109100991210     C                   PARM                    CLPDS
109200991210     C                   PARM                    CLSDS
109300991210     C     O69ERR        IFNE      '1'
109400991210     C                   MOVEL     ACORAG        vddksc
109500991210     C                   ELSE
109600991210     C                   MOVEL     *ALL'*'       vddksc
109700010913     C                   MOVE      '1'           errl00
109800010913     C                   MOVEL     msg(11)       dsmsms
109900010913     C                   ENDIF
110000000727     C                   ENDIF
110100010913     C*
110200010913     C                   ENDSR
110300991027     C*------------------------------------------------------------------------*
110400040901     C* cntpoi - Controlla codice filiale
110500991027     C*------------------------------------------------------------------------*
110600001120     C     cntpoi        begsr
110700001120     C*
110800010529     C                   if        vidpoi <> '046' and
110900020809     C                             vidpoi <> '999' and
111000020902     C                             vidpoi <> '500' and
111100020902     C                             vidpoi <> '789'
111200001120     C*
111300001120     C                   move      *blank        vddpoi
111400001120     C     '?'           scan      vidpoi                                 97
111500001120     C                   if        *in97
111600001120     C                   movel     *blank        cod1
111700001120     C                   movel     *blank        des1
111800001120     C                   movel     *blank        pra1
111900001120     C                   call      'TNSD24R'
112000001120     C                   parm                    cod1              3
112100001120     C                   parm                    tip1              1
112200001120     C                   parm                    des1             25
112300001120     C                   parm                    pra1              3
112400001120     C                   movel     cod1          vidpoi
112500001120     C                   endif
112600001120     C                   move      vidpoi        cod1n             3 0
112700001120     C     cod1n         chain     azorg01l                           98
112800001120     C                   if        not *in98 and orgfag <>'V'
112900001120     C                   movel     orgdes        vddpoi
113000001120     C                   else
113100001120     C                   seton                                        69
113200001120     C                   move      '1'           errl00
113300001120     C                   movel     msg(13)       dsmsms
113400001120     C                   endif
113500001120     C*
113600001120     C                   else
113700001120     C                   movel     'SEDE BOLOGNA'vddpoi
113800001120     C                   endif
113900001120     C*
114000001120     C                   endsr
114100991210     C*------------------------------------------------------------------------*
114200991210     C* FINSR - OPERAZIONI FINALI
114300991210     C*------------------------------------------------------------------------*
114400991210     C     FINSR         BEGSR
114500991210     C*
114600991210     C* CHIUDE I FILE DEI PGM CHIAMATI
114700991210     C                   CLEAR                   bs69ds
114800991210     C                   CLEAR                   acods
114900991210     C                   CLEAR                   indds
115000991210     C                   CLEAR                   clpds
115100991210     C                   CLEAR                   clsds
115200991210     C                   MOVEL     'C'           i69tla
115300991210     C                   MOVEL     knsif         i69sif
115400991210     C                   CALL      'TIBS69R'
115500991210     C                   PARM                    bs69ds
115600991210     C                   PARM                    acods
115700991210     C                   PARM                    indds
115800991210     C                   PARM                    clpds
115900991210     C                   PARM                    clsds
116000991210     C*
116100991210     C                   ENDSR
116200991026     C*------------------------------------------------------------------------*
116300991026     C* CHKDA8 - CONTROLLA LA DATA (8)
116400991026     C*------------------------------------------------------------------------*
116500991026     C     CHKDA8        BEGSR
116600991026     C*
116700991026     C     *LIKE         DEFINE    VIDDti        WDA8
116800991026     C     *LIKE         DEFINE    VIDDti        DA8W
116900991026     C*
117000991026     C                   Z-ADD     WDA8          G08DAT
117100991026     C                   Z-ADD     *ZEROS        G08INV
117200991026     C                   MOVEL     '0'           G08ERR
117300991026     C                   CALL      'XSRDA8'
117400991026     C                   PARM                    WLBDA8
117500991026     C                   Z-ADD     G08DAT        WDA8
117600991026     C                   Z-ADD     G08INV        DA8W
117700991026IF  1C     G08ERR        IFEQ      '1'
117800991026     C                   MOVEL     '1'           ERRL00
117900991026E   1C                   ENDIF
118000991026     C*
118100991026     C                   ENDSR
118200970526     C*------------------------------------------------------------------------*
118300970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
118400970526     C*------------------------------------------------------------------------*
118500970526     C     IREVID        BEGSR
118600080604     C*
118700080604     C* REPERISCO INFO DA ESTENSIONE ANAGRAFICA CLIENTI/TIPI FILE => TRC 01
118800080604     C                   IF        §vire01INV = 'S'
118900080604     C                   EVAL      VIDEML = §vire01EML
119000080604     C                   ENDIF
119100970526     C* CAMPI
119200991026     C                   MOVEL     virtip        VIDtip
119300020218     C                   MOVE      virksc        vidksc
119400020218     C                   Z-ADD     virseq        vidseq
119500060308     C                   IF        DEPOP1 = 'O03'
119600060308     C                   MOVEL     *blanks       vidpoi
119700060308     C                   ELSE
119800060308     C                   MOVE      virpoi        vidpoi
119900060308     C                   ENDIF
120000991026     C                   MOVEL     virdes        VIDdes
120100991026     C                   MOVEL     virpgt        VIDpgt
120200991026     C                   MOVEL     virppt        VIDppt
120300991026     C                   MOVEL     virpgi        VIDpgi
120400991026     C                   MOVEL     virppi        VIDppi
120500991026     C                   MOVEL     virfid        VIDfid
120600991026     C                   MOVEL     virfis        VIDfis
120700991026     C                   MOVEL     virfit        VIDfit
120800000710     C                   MOVEL     virfi3        VIDfi3
120900000710     C                   MOVEL     virfi4        VIDfi4
121000010123     C                   MOVEL     virfi2        VIDfi2
121100030526     C                   MOVEL     virpth        DVIR01
121200030526     C                   MOVEL     §VIR01TM      VIDtmbd
121300000710  ***C*                  MOVEL     virctl        VIDctl
121400991026     C                   Z-ADD     virdti        G08INV
121500991026     C                   Z-ADD     *ZEROS        G08DAT
121600991026     C                   MOVEL     '3'           G08ERR
121700991026     C                   CALL      'XSRDA8'
121800991026     C                   PARM                    WLBDA8
121900991026     C                   Z-ADD     G08DAT        viddti
122000970527     C*
122100991026     C                   Z-ADD     virdtf        G08INV
122200991026     C                   Z-ADD     *ZEROS        G08DAT
122300991026     C                   MOVEL     '3'           G08ERR
122400991026     C                   CALL      'XSRDA8'
122500991026     C                   PARM                    WLBDA8
122600991026     C                   Z-ADD     G08DAT        viddtf
122700020218     C                   MOVEL     virfi1        vidisv
122800051122     C*
122900051122     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
123000051122     C                   IF        DEPOP1 = 'F06' OR
123100051122     C                             DEPOP1 = 'O03' OR
123200051122     C                             DEPOP1 = 'O07'
123300051122     C                   EVAL      VIDDTI = DATCORINV
123400060726     C                   EVAL      VIDDTF = 31122039
123500051122     C                   ENDIF
123600020218     C*
123700970526     C                   ENDSR
123800970526     C*------------------------------------------------------------------------*
123900970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
124000970526     C*------------------------------------------------------------------------*
124100970526     C     AZZVID        BEGSR
124200970526     C* CAMPI
124300991026     C                   MOVEL     *blank        VIDksc
124400000710  ***C*                  MOVEL     *blank        VIDpt1
124500000710  ***C*                  MOVEL     *blank        VIDpt2
124600010123     C                   z-add     0             vidseq
124700010123     C                   move      *zeros        vidpoi
124800060726     C                   EVAL      VIDDTI = DATCORINV
124900060726     C                   EVAL      VIDDTF = 31122039
125000991025     C                   MOVEL     *blank        VIDtip
125100991025     C                   MOVEL     *blank        VIDdes
125200991025     C                   MOVEL     *blank        VIDpgt
125300991025     C                   MOVEL     *blank        VIDppt
125400991025     C                   MOVEL     *blank        VIDpgi
125500991025     C                   MOVEL     *blank        VIDppi
125600991025     C                   MOVEL     *blank        VIDfid
125700991025     C                   MOVEL     *blank        VIDfis
125800991025     C                   MOVEL     *blank        VIDfit
125900000710     C                   MOVEL     *blank        VIDfi3
126000000710     C                   MOVEL     *blank        VIDfi4
126100010123     C                   MOVEL     *blank        VIDfi2
126200000710  ***C*                  MOVEL     *blank        VIDctl
126300080603     C                   MOVEL     *blank        VIDeml
126400970526     C*
126500970526     C                   ENDSR
126600970526     C*------------------------------------------------------------------------*
126700991026     C* IMPvir - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
126800970526     C*------------------------------------------------------------------------*
126900991026     C     IMPvir        BEGSR
127000970526     C*
127100991027     C                   move      VIDpoi        virpoi
127200991026     C                   movel     VIDdes        virdes
127300991026     C                   movel     VIDpgt        virpgt
127400991026     C                   movel     VIDppt        virppt
127500991026     C                   movel     VIDpgi        virpgi
127600991026     C                   movel     VIDppi        virppi
127700991026     C                   movel     VIDfid        virfid
127800991026     C                   movel     VIDfis        virfis
127900991026     C                   movel     VIDfit        virfit
128000000710     C                   movel     VIDfi3        virfi3
128100000710     C                   movel     VIDfi4        virfi4
128200010123     C                   movel     VIDfi2        virfi2
128300030526     C                   MOVEL     VIDtmbd       §VIR01TM
128400030526     C                   MOVEL     DVIR01        virpth
128500000710  ***C*                  movel     VIDctl        virctl
128600991026     C                   Z-ADD     viddtf        G08dat
128700991026     C                   Z-ADD     *ZEROS        G08inv
128800991026     C                   MOVEL     '0'           G08ERR
128900991026     C                   CALL      'XSRDA8'
129000991026     C                   PARM                    WLBDA8
129100991026     C                   Z-ADD     G08inv        virdtf
129200000628     C                   movel     VIDisv        virfi1
129300970526     C*
129400970526     C                   ENDSR
129500970526     C*------------------------------------------------------------------------*
129600970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
129700970526     C*------------------------------------------------------------------------*
129800970526     C     IMPFUN        BEGSR
129900101013     C*
130000101026     C                   SETOFF                                       44
130100101026     C                   SETOFF                                       30
130200101026     C                   SETOFF                                       31
130300101026     C                   SETOFF                                       32
130400101026     C                   SETOFF                                       33
130500970526     C*
130600970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
130700970526IF  1C     DEPOP1        IFEQ      'O05'
130800970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
130900970526     C                   MOVEL     'N'           FUDATT
131000970526     C                   MOVEL     FUDRIG        FUD(6)
131100970526E   1C                   ENDIF
131200970526     C*
131300970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
131400970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
131500970526     C                   EXSR      FUNGES
131600970526     C*
131700970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
131800970526     C*---
131900970526     C* 90 -> PROTEZIONE CHIAVE
132000970526     C* 91 -> PROTEZIONE TOTALE
132100970526     C*---
132200970528IF  1C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
132300970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
132400970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
132500970526     C                   SETOFF                                       91
132600970526E   2C                   ENDIF
132700101013IF  2C     DEPOP1        IFEQ      'O03'                                        *INSERIMENTO
132800101013E   2C                   ENDIF
132900970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
133000991014IF  2C     DEPOP1        oreq      'O01'                                        *MODIFICA
133100970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
133200970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
133300970526E   2C                   ENDIF
133400970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
133500970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
133600970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
133700970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
133800970526     C                   SETON                                        91
133900970526E   2C                   ENDIF
134000970526X   1C                   ELSE                                                   *I N T E R R O G A
134100970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
134200970526     C                   SETON                                        91
134300970526E   1C                   ENDIF
134400970526     C*
134500970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
134600970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
134700991025     C                   MOVEL     S20tip        KCEtip
134800991026     C                   MOVEL     S20ksc        KCEksc
134900000420     C     KEYvi2        CHAIN     tivir03L                           9998
135000080603IF  2C     *IN99         IFEQ      *ON
135100970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
135200970526     C                   MOVEL     MSG(6)        DSMSMS
135300970526X   2C                   ELSE
135400080603     C*
135500080603     C* Aggancio anche il record d estensione regole clienti/tipi-file => TRC 01
135600080603     C                   EVAL      KCEdti  = virDTI
135700080603     C                   EVAL      vireTRC = '01'
135800080603     C     KEYvire1_C    CHAIN     tivire1l                           9998
135900080603     C                   IF        %found(tivire1l)
136000080603     C                   EVAL      DVIRE01 = vireFLO
136100080603     C                   ENDIF
136200080603     C*
136300080603     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
136400980128IF  3C     DEPOP1        IFEQ      'O05'
136500991014IF  3C     DEPOP1        oreq      'O02'
136600101026     C*
136700101026     C* Se trattasi d tipo servizio "idoneo" reperisco anche i dati utente lato "BARTVAS"
136800101026     C     virFI1        lookup    isvftpeml                              80
136900101026     C                   IF        *IN80
137000101026      /FREE
137100101026
137200101026         if UBBVSQL01_RtvCliTipFile(
137300101026                         pInConn
137400101026                       : virKSC
137500101026                       : virTIP
137600101026                       : pOutNumRec
137700101026                       : pOutCliTipTras
137800101026                       : pOutCliMskChk
137900101026                       : pOutCliMskFil
138000101026                       : pOutCliMskSem
138100101026                       : pOutCliPerc
138200101026                       : pOutCliEml1
138300101026                       : pOutCliEml2
138400101026                       : pOutCliEml3
138500101026                       : pOutCliEml4
138600101026                       : pOutCliEml5
138700101026                       : pOutCliSogg
138800101026                       : pOutCliTesto
138900101026                       : pOutCliFtpAddr
139000101026                       : pOutCliFtpDir
139100101026                       : pOutCliFtpUsr
139200101026                       : pOutCliFtpPwd
139300101026                       : pOutCliFtpFAS4
139400101026                       : pOutCliPercOld
139500101026                       : pOutCliTentat
139600101026                       : pOutCliRitardo
139700101026                       : pOutCliLibAS4
139800101026                       : pOutCliFilAS4
139900101026                       : pOutCliMbrAS4
140000101026                       : pOutCliProc
140100101026                       : pOutCliSched
140200101026                       : pOutCliAccorp
140300101026                       : pOutCliMskZip
140400101026                       : pOutCliDataStr
140500101026                       : pOutCliDataEnd
140600101026                       : pOutCliCodComp
140700101026                       : pOutCliFtpCmd1
140800101026                       : pOutCliEmlErr
140900101026                       : pOutCliHtUrlTo
141000101026                       : pOutCliHtUsrTo
141100101026                       : pOutCliHtPwdTo
141200101026                       : pOutCliHtConTy
141300101026                       : pOutCliHtSndEx
141400101026                       : pOutCliHtSndPr
141500101026                       : pOutCliHtUsPxy
141600101026                       : pOutCliUsrIns
141700101026                       : pOutCliUsrUpd
141800101026                       : pOutCliDatUpd
141900101026                       : pOutCliNote
142000101026                       : pOutCliFtpPrt
142100101026                       : pOutCliFtpTy) = 0;
142200101026              if pOutNumRec = 1;
142300101026                 VIDPPIMC = pOutCliMskChk;
142400101026                 if %len(%trim(pOutCliMskChk)) > %size(VIDPPIMC);
142500101026                    *in30 = *on;
142600101026                 endif;
142700101026                 VIDPPIMF = pOutCliMskFil;
142800101026                 if %len(%trim(pOutCliMskFil)) > %size(VIDPPIMF);
142900101026                    *in31 = *on;
143000101026                 endif;
143100101026                 VIDPPIMS = pOutCliMskSem;
143200101026                 if %len(%trim(pOutCliMskSem)) > %size(VIDPPIMS);
143300101026                    *in32 = *on;
143400101026                 endif;
143500101026                 VIDPPIMZ = pOutCliMskZip;
143600101026                 if %len(%trim(pOutCliMskZip)) > %size(VIDPPIMZ);
143700101026                    *in33 = *on;
143800101026                 endif;
143900101026              endif;
144000101026         endif;
144100101026
144200101026      /END-FREE
144300101026     C
144400101026     C                   ELSE
144500101026     C                   SETON                                        44
144600101026     C                   ENDIF
144700101026     C*
144800980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
144900980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
145000980128E   3C                   ENDIF
145100970526E   2C                   ENDIF
145200970526X   1C                   ELSE                                                   *INSERIMENTO
145300970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
145400970526E   1C                   ENDIF
145500970526     C*
145600970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
145700970526IF  1C     DEPOP1        IFEQ      'O02'
145800970526     C                   MOVEL     DEC(1)        VIDFUN
145900970526E   1C                   ENDIF
146000991014IF  1C     DEPOP1        IFEQ      'O03'
146100991014     C                   MOVEL     DEC(6)        VIDFUN
146200991014E   1C                   ENDIF
146300970526IF  1C     DEPOP1        IFEQ      'O04'
146400970526     C                   MOVEL     DEC(2)        VIDFUN
146500970526E   1C                   ENDIF
146600970526IF  1C     DEPOP1        IFEQ      'O05'
146700970526     C                   MOVEL     DEC(3)        VIDFUN
146800970526E   1C                   ENDIF
146900970526IF  1C     DEPOP1        IFEQ      'O07'
147000970526     C                   MOVEL     DEC(4)        VIDFUN
147100970526E   1C                   ENDIF
147200970526IF  1C     DEPOP1        IFEQ      'F06'
147300970526     C                   MOVEL     DEC(5)        VIDFUN
147400970526E   1C                   ENDIF
147500970526     C*
147600970526     C                   ENDSR
147700101026
147800101026
147900101026
148000101026     C*--------------------------------------------------------------------------------------------*
148100101026     C* CARTAB - carica le tabelle occorrenti
148200101026     C*--------------------------------------------------------------------------------------------*
148300101026     C     CARTAB        BEGSR
148400101026     C*
148500101026     C* Tipi servizi soggetti a controllo unificazioni
148600101026     C                   z-add     *zeros        ww                3 0
148700101026     C                   eval      tbecod = 'ISV'
148800101026     C     tbecod        setll     tntbe01l
148900101026     C     tbecod        reade     tntbe01l
149000101026     C                   dow       not %eof(tntbe01l)
149100101026     C                   movel     tbeuni        disv
149200101026     C                   if        §isvproc = 'F'
149300101026     C                   add       1             ww
149400101026     C                   eval      isvftpeml(ww) = tbeke1
149500101026     C                   endif
149600101026     C     tbecod        reade     tntbe01l
149700101026     C                   enddo
149800101026     C*
149900101026     C                   ENDSR
150000101026
150100101026     c*------------------------------------------------------------------------*
150200101026     c* opnCONN - Apertura connessione
150300101026     c*------------------------------------------------------------------------*
150400101026     c     OPNCONN       BEGSR
150500101026      /FREE
150600101026
150700101026        if not *in40;
150800101026           if UBBVSQL01_Connect('BARTVAS' : pOutConn) = 0;
150900101026              pInConn = pOutConn;
151000101026           else;
151100101026              *in40 = *on;
151200101026              DSMSMS = MSG(28);
151300101026           endif;
151400101026        endif;
151500101026
151600101026      /END-FREE
151700101026      *
151800101026     c                   ENDSR
151900101026     c*------------------------------------------------------------------------*
152000101026
152100101026
152200101026
152300101026     c*------------------------------------------------------------------------*
152400101026     c* clsCONN - Chiusura connessione
152500101026     c*------------------------------------------------------------------------*
152600101026     c     CLSCONN       BEGSR
152700101026      /FREE
152800101026
152900101026           if UBBVSQL01_Disconnect(pInConn) = 0;
153000101026           else;
153100101026              *in40 = *on;
153200101026           endif;
153300101026
153400101026      /END-FREE
153500101026      *
153600101026     c                   ENDSR
153700101026     c*------------------------------------------------------------------------*
153800101026
153900101026
154000101026
154100970526     C*------------------------------------------------------------------------*
154200970526     C* *INZSR - OPERAZIONI INIZIALI
154300970526     C*------------------------------------------------------------------------*
154400970526     C     *INZSR        BEGSR
154500970526     C*---
154600970526     C* RICEVIMENTO PARAMETRI
154700970526     C*---
154800970526     C     *ENTRY        PLIST
154900970612     C                   PARM                    KPJBA
155000991028     C                   movel     kpjbu         tis720
155100000420     C*
155200000420     C* Inverto subito la data presa in input
155300000420     C                   Z-ADD     s20dti        wda8
155400000420     C                   Z-ADD     *ZEROS        da8w
155500000420     C                   EXSR      CHKDA8
155600000420     C                   Z-ADD     DA8W          KCEdti
155700970526     C*
155800970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
155900970526     C     XMSGPA        PLIST
156000970526     C                   PARM                    DSMSMS
156100970526     C                   PARM                    DSMSPG
156200970526     C                   PARM                    DSMSKE
156300970526     C                   PARM                    DSMSER
156400991108     C*---
156500970526     C*
156600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
156700970526     C                   CALL      'XKEY'
156800970526     C                   PARM                    DSKEY
156900101026     C*
157000101026     C* CARICA LE TABELLE OCCORRENTI
157100101027     C                   EXSR      CARTAB
157200101026     C*
157300101026     C* Apro la connessione al DB "BARTVAS"
157400101026     C                   exsr      opnCONN
157500970526     C*---
157600970526     C* VARIABILI NON RIFERITE AL DATA BASE
157700970526     C*---
157800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
157900991025     C     *LIKE         DEFINE    S20OP0        DEPOP0
158000991025     C     *LIKE         DEFINE    S20OP1        DEPOP1
158100991025     C     *LIKE         DEFINE    S20OP0        WOP0
158200991025     C     *LIKE         DEFINE    S20OP1        WOP1
158300970526     C*---
158400970526     C* VARIABILI DI CONTROLLO
158500970526     C*---
158600970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
158700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
158800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
158900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
159000970526     C*
159100970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
159200991026     C                   MOVEL     'TIS723R'     DSMSPG
159300991026     C                   MOVEL     'TIS723R'     MSGQUE
159400970526     C                   MOVEL     *BLANKS       MSGKEY
159500970526     C                   MOVEL     *BLANKS       DSMSMS
159600970526     C*---
159700970526     C* VARIABILI RIFERITE AL DATA BASE
159800970526     C*---
159900991026     C     *LIKE         DEFINE    virtip        KCEtip
160000991026     C     *LIKE         DEFINE    virksc        KCEksc
160100000420     C     *LIKE         DEFINE    virdti        KCEdti
160200970526     C*---
160300970526     C* CHIAVI DI LETTURA
160400970526     C*---
160500000420     C* LETTURA tivir03L - INCOMPLETA
160600991026     C     KEYvir        KLIST
160700080603     C                   KFLD                    KCEtip
160800080603     C                   KFLD                    KCEksc
160900080603     C* LETTURA tivir03L - COMPLETA
161000080603     C     KEYvi2        KLIST
161100080603     C                   KFLD                    KCEtip
161200080603     C                   KFLD                    KCEksc
161300080603     C                   KFLD                    KCEdti
161400080603     C* LETTURA tivire1L - COMPLETA
161500080603     C     KEYvire1_C    KLIST
161600080603     C                   KFLD                    KCEksc
161700080603     C                   KFLD                    KCEtip
161800080603     C                   KFLD                    KCEdti
161900080603     C                   KFLD                    vireTRC
162000080603     C* LETTURA tntbe01L - PARZIALE
162100040816     C     KEYtbe1_P     KLIST
162200040816     C                   KFLD                    TBEcod                         *CODICE TABELLA
162300970528     C*
162400970528     C* CALCOLA LA DATA CORRENTE
162500970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
162600970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
162700970528     C                   Z-ADD     WN8           G08DAT
162800051122     C                   Z-ADD     WN8           DATCORINV         8 0
162900970528     C                   Z-ADD     *ZEROS        G08INV
163000970528     C                   MOVEL     '0'           G08ERR
163100970528     C                   CALL      'XSRDA8'
163200970528     C                   PARM                    WLBDA8
163300970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
163400970526     C*
163500970526     C* DEPOSITA LE OPZIONI DI ENTRATA
163600991025     C                   MOVEL     S20OP0        DEPOP0                         *LIVELLO PROCEDURA
163700991025     C                   MOVEL     S20OP1        DEPOP1                         *LIVELLO PROGRAMMA
163800051122     C*
163900051122     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
164000051122     C                   IF        DEPOP1 = 'F06' OR
164100051122     C                             DEPOP1 = 'O03' OR
164200051122     C                             DEPOP1 = 'O07'
164300051122     C                   EVAL      VIDDTI = DATCORINV
164400060726     C                   EVAL      VIDDTF = 31122039
164500051122     C                   ENDIF
164600000629     C*
164700000629     C* EFFETTUO CONSIDERAZIONI SULLE OPZIONI DI ENTRATA
164800000629     C                   IF        DEPOP1 = 'F06' OR
164900000629     C                             DEPOP1 = 'O03'
165000000629     C                   SETOFF                                       92
165100000629     C                   ELSE
165200000629     C                   SETON                                        92
165300000629     C                   ENDIF
165400970526     C*
165500970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
165600970526     C                   EXSR      IMPFUN
165700970526     C*
165800970526     C                   ENDSR
165900970526     C*------------------------------------------------------------------------*
166000970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
166100970526     C*------------------------------------------------------------------------*
166200970526     C     FUNGES        BEGSR
166300970526     C*
166400970526     C* PULISCO CAMPI DI LAVORO
166500970526     C                   Z-ADD     0             W                 5 0
166600970526     C*
166700970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
166800970526     C* E CARICO TABELLA FUNZIONI ATTIVE
166900970526     C     1             DO        24            I
167000970526     C                   MOVEL     FUD(I)        FUDRIG
167100970526     C*
167200970526     C* PULIZIA FUNZIONI PRECEDENTI
167300970526     C                   MOVEL     *BLANKS       FUV(I)
167400970526     C*
167500970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
167600970526     C* (SALTA F24)
167700970526     C     FUDATT        IFEQ      'S'
167800970526     C     FUDFUN        ANDNE     '24'
167900970526     C                   ADD       1             W
168000970526     C                   MOVEL     FUDDES        FUV(W)
168100970526     C                   END
168200970526     C                   END                                                    DO WHILE
168300970526     C*
168400970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
168500970526     C* ALTRIMENTI DISATTIVO F24
168600970526     C     1             DO        24            I
168700970526     C                   MOVEL     FUD(I)        FUDRIG
168800970526     C*
168900970526     C     FUDFUN        IFEQ      '24'
169000970526     C     W             IFGT      6
169100970526     C                   MOVEL     'S'           FUDATT
169200970526     C                   ELSE
169300970526     C                   MOVEL     *BLANKS       FUDATT
169400970526     C                   END                                                    W GT 8
169500970526     C                   MOVEL     FUDRIG        FUD(I)
169600970526     C                   END
169700970526     C                   END                                                    DO WHILE
169800970526     C*
169900970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
170000970526     C     W             DIV       6             I
170100970526     C                   MVR                     Z
170200970526     C*
170300970526     C     Z             IFNE      0
170400970526     C     I             ADD       1             Z
170500970526     C                   ELSE
170600970526     C                   Z-ADD     I             Z                 5 0
170700970526     C                   END
170800970526     C*
170900970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
171000970526     C                   ADD       1             FUNGRU
171100970526     C     FUNGRU        IFGT      Z
171200970526     C     FUNGRU        ORLT      1
171300970526     C                   Z-ADD     1             FUNGRU
171400970526     C                   END
171500970526     C*
171600970526     C* PRIMO GRUPPO DI 6 FUNZIONI
171700970526     C     FUNGRU        IFEQ      1
171800970526     C                   MOVEL     FUV(1)        FUN001
171900970526     C                   MOVEL     FUV(2)        FUN002
172000970526     C                   MOVEL     FUV(3)        FUN003
172100970526     C                   MOVEL     FUV(4)        FUN004
172200970526     C                   MOVEL     FUV(5)        FUN005
172300970526     C                   MOVEL     FUV(6)        FUN006
172400970526     C                   END
172500970526     C*
172600970526     C* SECONDO GRUPPO DI 6 FUNZIONI
172700970526     C     FUNGRU        IFEQ      2
172800970526     C                   MOVEL     FUV(7)        FUN001
172900970526     C                   MOVEL     FUV(8)        FUN002
173000970526     C                   MOVEL     FUV(9)        FUN003
173100970526     C                   MOVEL     FUV(10)       FUN004
173200970526     C                   MOVEL     FUV(11)       FUN005
173300970526     C                   MOVEL     FUV(12)       FUN006
173400970526     C                   END
173500970526     C*
173600970526     C* TERZO GRUPPO DI 6 FUNZIONI
173700970526     C     FUNGRU        IFEQ      3
173800970526     C                   MOVEL     FUV(13)       FUN001
173900970526     C                   MOVEL     FUV(14)       FUN002
174000970526     C                   MOVEL     FUV(15)       FUN003
174100970526     C                   MOVEL     FUV(16)       FUN004
174200970526     C                   MOVEL     FUV(17)       FUN005
174300970526     C                   MOVEL     FUV(18)       FUN006
174400970526     C                   END
174500970526     C*
174600970526     C* QUARTO GRUPPO DI 6 FUNZIONI
174700970526     C     FUNGRU        IFEQ      4
174800970526     C                   MOVEL     FUV(19)       FUN001
174900970526     C                   MOVEL     FUV(20)       FUN002
175000970526     C                   MOVEL     FUV(21)       FUN003
175100970526     C                   MOVEL     FUV(22)       FUN004
175200970526     C                   MOVEL     *BLANKS       FUN005
175300970526     C                   MOVEL     *BLANKS       FUN006
175400970526     C                   END
175500970526     C*
175600970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
175700970526     C                   MOVEL     *BLANKS       FUN007
175800970526     C                   MOVEL     *BLANKS       FUN008
175900970526     C*
176000970526     C     1             DO        24            I
176100970526     C                   MOVEL     FUD(I)        FUDRIG
176200970526     C     FUDFUN        IFEQ      '24'
176300970526     C     FUDATT        ANDEQ     'S'
176400970526     C                   MOVEL     FUDDES        FUN008
176500970526     C                   END
176600970526     C                   END                                                    DO
176700970526     C*
176800970526     C                   ENDSR
176900970526     C*------------------------------------------------------------------------*
177000970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
177100970526     C*------------------------------------------------------------------------*
177200970526     C     FUNCON        BEGSR
177300970526     C*
177400970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
177500970526     C                   EXSR      FUNVAR
177600970526     C*
177700970526     C* PULISCO CAMPI DI LAVORO
177800970526     C                   Z-ADD     0             I                 5 0
177900970526     C                   Z-ADD     0             W                 5 0
178000970526     C*
178100970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
178200970526     C     FUNKEY        IFNE      *BLANKS
178300970526     C     1             DO        24            I
178400970526     C                   MOVEL     FUD(I)        FUDRIG
178500970526     C*
178600970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
178700970526     C     FUNKEY        IFEQ      FUDFUN
178800970526     C     FUDATT        ANDEQ     'S'
178900970526     C                   Z-ADD     I             W
179000970526     C                   END
179100970526     C                   END                                                    DO WHILE
179200970526     C*
179300970526     C* FUNZIONE NON ATTIVA (ERRORE)
179400970526     C     W             IFEQ      0
179500970526     C*
179600970526     C* EMISSIONE MESSAGGIO DI ERRORE
179700970526     C                   MOVEL     MSG(1)        DSMSMS
179800970526     C*
179900970526     C                   END                                                    W=0
180000970526     C                   END                                                    FUNKEY NE *BLANKS
180100970526     C*
180200970526     C* FUNZIONE ATTIVA (PROCEDI ...)
180300970526     C     W             IFNE      0
180400970526     C     FUNKEY        OREQ      *BLANKS
180500970526     C     FUNKEY        IFEQ      *BLANKS
180600970526     C                   EXSR      ENTGES
180700970526     C                   END
180800031229     C     FUNKEY        IFEQ      '01'
180900031229     C                   EXSR      F01GES
181000031229     C                   END
181100970526     C     FUNKEY        IFEQ      '03'
181200970526     C                   EXSR      F03GES
181300970526     C                   END
181400970526     C     FUNKEY        IFEQ      '06'
181500970526     C                   EXSR      F06GES
181600970526     C                   END
181700970526     C     FUNKEY        IFEQ      '12'
181800970526     C                   EXSR      F12GES
181900970526     C                   END
182000970526     C     FUNKEY        IFEQ      '24'
182100970526     C                   EXSR      F24GES
182200970526     C                   END
182300970526     C                   END
182400970526     C*
182500970526     C                   ENDSR
182600970526     C*------------------------------------------------------------------------*
182700970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
182800970526     C*------------------------------------------------------------------------*
182900970526     C     FUNVAR        BEGSR
183000970526     C*
183100970526     C* INIZIALIZZA VARIABILE -FUNKEY-
183200970526     C*
183300970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
183400970526     C     KEY           IFEQ      ENTER
183500970526     C                   MOVEL     *BLANKS       FUNKEY
183600970526     C                   END
183700970526     C     KEY           IFEQ      F1
183800970526     C                   MOVEL     '01'          FUNKEY
183900970526     C                   END
184000970526     C     KEY           IFEQ      F2
184100970526     C                   MOVEL     '02'          FUNKEY
184200970526     C                   END
184300970526     C     KEY           IFEQ      F3
184400970526     C                   MOVEL     '03'          FUNKEY
184500970526     C                   END
184600970526     C     KEY           IFEQ      F4
184700970526     C                   MOVEL     '04'          FUNKEY
184800970526     C                   END
184900970526     C     KEY           IFEQ      F5
185000970526     C                   MOVEL     '05'          FUNKEY
185100970526     C                   END
185200970526     C     KEY           IFEQ      F6
185300970526     C                   MOVEL     '06'          FUNKEY
185400970526     C                   END
185500970526     C     KEY           IFEQ      F7
185600970526     C                   MOVEL     '07'          FUNKEY
185700970526     C                   END
185800970526     C     KEY           IFEQ      F8
185900970526     C                   MOVEL     '08'          FUNKEY
186000970526     C                   END
186100970526     C     KEY           IFEQ      F9
186200970526     C                   MOVEL     '09'          FUNKEY
186300970526     C                   END
186400970526     C     KEY           IFEQ      F10
186500970526     C                   MOVEL     '10'          FUNKEY
186600970526     C                   END
186700970526     C     KEY           IFEQ      F11
186800970526     C                   MOVEL     '11'          FUNKEY
186900970526     C                   END
187000970526     C     KEY           IFEQ      F12
187100970526     C                   MOVEL     '12'          FUNKEY
187200970526     C                   END
187300970526     C     KEY           IFEQ      F13
187400970526     C                   MOVEL     '13'          FUNKEY
187500970526     C                   END
187600970526     C     KEY           IFEQ      F14
187700970526     C                   MOVEL     '14'          FUNKEY
187800970526     C                   END
187900970526     C     KEY           IFEQ      F15
188000970526     C                   MOVEL     '15'          FUNKEY
188100970526     C                   END
188200970526     C     KEY           IFEQ      F16
188300970526     C                   MOVEL     '16'          FUNKEY
188400970526     C                   END
188500970526     C     KEY           IFEQ      F17
188600970526     C                   MOVEL     '17'          FUNKEY
188700970526     C                   END
188800970526     C     KEY           IFEQ      F18
188900970526     C                   MOVEL     '18'          FUNKEY
189000970526     C                   END
189100970526     C     KEY           IFEQ      F19
189200970526     C                   MOVEL     '19'          FUNKEY
189300970526     C                   END
189400970526     C     KEY           IFEQ      F20
189500970526     C                   MOVEL     '20'          FUNKEY
189600970526     C                   END
189700970526     C     KEY           IFEQ      F21
189800970526     C                   MOVEL     '21'          FUNKEY
189900970526     C                   END
190000970526     C     KEY           IFEQ      F22
190100970526     C                   MOVEL     '22'          FUNKEY
190200970526     C                   END
190300970526     C     KEY           IFEQ      F23
190400970526     C                   MOVEL     '23'          FUNKEY
190500970526     C                   END
190600970526     C     KEY           IFEQ      F24
190700970526     C                   MOVEL     '24'          FUNKEY
190800970526     C                   END
190900970526     C     KEY           IFEQ      ROLLUP
191000970526     C                   MOVEL     'RU'          FUNKEY
191100970526     C                   END
191200970526     C     KEY           IFEQ      HELP
191300970526     C                   MOVEL     'HP'          FUNKEY
191400970526     C                   END
191500970526     C                   ENDSR
191600970526     C*------------------------------------------------------------------------*
191700970526     C* NOTE COSTRUZIONE SCHIERE
191800970526     C*------------------------------------------------------------------------*
191900970526     C*
192000970526     C* -MSG-MESSAGGI
192100970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
192200970526     C* (NON TOCCARE MSG,1 E MSG,2)
192300970526     C*
192400970526     C* -CMD-COMANDI
192500970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
192600970526     C*
192700970526     C* -OPD-OPZIONI
192800970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
192900970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
193000970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
193100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
193200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
193300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
193400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
193500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
193600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
193700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
193800970526     C*
193900970526     C* -FUD-FUNZIONI
194000970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
194100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
194200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
194300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
194400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
194500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
194600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
194700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
194800970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
194900970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
195000970526     O*------------------------------------------------------------------------*
195100970526** DEC - DECODIFICHE
195200970526    MODIFICA                                                      1
195300970526  ANNULLAMENTO                                                    2
195400970526VISUALIZZAZIONE                                                   3
195500970526   RIPRISTINO                                                     4
195600970526  INSERIMENTO                                                     5
195700991014     COPIA                                                        6
195800970526** MSG - MESSAGGI
195900991026TIS723R- Tasto funzionale non gestito                                          1
196000991026TIS723R- Inserimento effettuato                                                2
196100991026TIS723R- Modifica effettuata                                                   3
196200991026TIS723R- Annullamento effettuato                                               4
196300991026TIS723R- Ripristino effettuato                                                 5
196400991026TIS723R- Richiesta funzione per record inesistente                             6
196500991026TIS723R- Record momentaneamente allocato: riprovare più tardi                  7
196600991026TIS723R- Richiesta funzione per record esistente                               8
196700991026TIS723R- Questo codice cliente è già presente con questo tipo file             9
196800991027TIS723R- Il codice tipo file non è fra quelli presenti in anagrafico           10
196900991027TIS723R- Il codice cliente non è presente come padre nell'archivio UNIFICANTI  11
197000000724TIS723R- Campo obbligatorio                                                    12
197100991027TIS723R- Codice filiale inesistente o di categoria V=varia                     13
197200000724TIS723R- Data errata                                                           14
197300000724TIS723R- La data fine deve essere maggiore della data inizio                   15
197400000724TIS723R- Il record non può scadere in un giorno già trascorso                  16
197500000724TIS723R- Il record immesso si sovrappone ad uno già in decorrenza              17
197600000724TIS723R- Codice tipo servizio non presente in tabella                          18
197700001120TIS723R- Oggetto inesistente, verificare e correggere.                         19
197800001120TIS723R- File inesistente, verificare e correggere.                            20
197900001120TIS723R- Libreria inesistente, verificare e correggere.                        21
198000030603TIS723R- Tipo membro non impostabile x il tipo servizio indicato.              22
198100040107TIS723R- Per il traduttore indicato non esiste descrizione dettaglio parametri 23
198200040319TIS723R- Tipo file e servizio incompatibili, correggere.                       24
198300040816TIS723R- Tipo file e/o programma traduttore e/o file tradotto incompatibili.   25
198400051026TIS723R- ATTENZIONE presenti log a stato 0 impossibile cambiare traduttore!    26
198500060616TIS723R- Valori incompatibili, correggere!                                     27
198600101026TIS723R- Errore BartVAS: fatal error                                           28
198700101027TIS723R- ATTENZIONE: modifica effettuata lato AS/400 ma non lato BARTVAS       29
198800101026TIS723R-                                                                       30
198900970526** FUD - FUNZIONI
199000031229F01=Parametri      -        (S/N)=S  <== S-FISSO           funz.01¬
199100970526                   -        (S/N)=N                        funz.02¬
199200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
199300970526                   -        (S/N)=N                        funz.04¬
199400970526                   -        (S/N)=N                        funz.05¬
199500970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
199600970527                   -        (S/N)=N                        funz.07¬
199700970527                   -        (S/N)=N                        funz.08¬
199800970527                   -        (S/N)=N                        funz.09¬
199900970527                   -        (S/N)=N                        funz.10¬
200000970526                   -        (S/N)=N                        funz.11¬
200100970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
200200970526                   -        (S/N)=N                        funz.13¬
200300970526                   -        (S/N)=N                        funz.14¬
200400970526                   -        (S/N)=N                        funz.15¬
200500970526                   -        (S/N)=N                        funz.16¬
200600970526                   -        (S/N)=N                        funz.17¬
200700970526                   -        (S/N)=N                        funz.18¬
200800970526                   -        (S/N)=N                        funz.19¬
200900970526                   -        (S/N)=N                        funz.20¬
201000970526                   -        (S/N)=N                        funz.21¬
201100970526                   -        (S/N)=N                        funz.22¬
201200970526                   -        (S/N)=N                        funz.23¬
201300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
