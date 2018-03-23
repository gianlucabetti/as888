000100120104     H*------------------------------------------------------------------------*
000200120104     H* Gestione tipi file internet
000300970526     H*------------------------------------------------------------------------*
000400111216     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000500970526     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900991026     Ftis723d   cf   e             WORKSTN INFDS(DEVDS1)
001000120109     F                                     SFILE(s723DMS:MSGULT)
001100970527     F*------------
001200991027     Ftivir03L  UF A E           K DISK
001300080603     Ftivire1L  UF A E           K DISK
001400991027     Ftivtf01l  if   e           k disk
001500991027     Fazorg01l  if   e           k disk
001600000626     Ftntbe01l  if   e           k disk
001700000727     Ftianv01l  if   e           k disk
001800120111     FTIVIR06L  IF   E           K DISK    prefix(x_) rename(TIVIR000:TIVIR06)
001900970526     D*------------------------------------------------------------------------*
002000970526     D* SCHIERE
002100970526     D*------------------------------------------------------------------------*
002200970526     D*------------
002300970526     D* DECODIFICHE
002400970526     D*------------
002500991014     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
002600970526     D*------------
002700970526     D* MESSAGGI
002800970526     D*------------
002900120119     D MSG             S             78    DIM(34) CTDATA PERRCD(1)
003000970526     D*------------
003100970526     D* DESCRIZIONI TASTI FUNZIONALI
003200970526     D*------------
003300970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003400970526     D*------------
003500970526     D* FUNZIONI DA CARICARE A VIDEO
003600970526     D*------------
003700970526     D FUV             S             19    DIM(24)
003800970526     D*------------------------------------------------------------------------*
003900970526     D* INPUT
004000970526     D*------------------------------------------------------------------------*
004100970526     D*------------
004200970526     D* RIDEFINIZIONE FUNZIONI VIDEO
004300970526     D*------------
004400970526     D FUDDS           DS
004500970526     D  FUDDE1                 1      1
004600970526     D  FUDFUN                 2      3
004700970526     D  FUDDE2                 4     19
004800970526     D  FUDFI0                20     34
004900970526     D  FUDATT                35     35
005000970526     D  FUDFI1                36     80
005100970526     D  FUDDES                 1     19
005200970526     D  FUDRIG                 1     80
005300970526     D*------------
005400970526     D* DS GESTIONE TASTI FUNZIONALI
005500970526     D*------------
005600970526     D DSKEY         E DS
005700970526     D DEVDS1          DS
005800970526     D  KEY                  369    369
005900970526     D*------------
006000970526     D* DS "XMSG" - GESTIONE MESSAGGI
006100970526     D*------------
006200970526     D DSMSG         E DS
006300970526     D*------------
006400020906     D* TABELLE
006500020906     D*------------
006600991108     D TIBS02DS      E DS
006700970528     D*------------------
006800970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
006900970528     D*------------------
007000970528     D WLBDA8          DS                  INZ
007100970528     D  G08DAT                 1      8  0
007200970528     D  G08INV                 9     16  0
007300970528     D  G08ERR                17     17
007400970528     D  G08TGI                18     22  0
007500020218     D*------------
007600020218     D* DS "ITF" - TABELLA SCAMBIO TIPI FILE
007700020218     D*------------
007800020218     D DITF          E DS
007900970526     D*------------
008000970526     D* DS DI PROCEDURA
008100970526     D*------------
008200991025     D tis720        E DS                  EXTNAME(tis720ds)
008300030526     D DVIR01        E DS
008400030603     D DISV          E DS
008500040816     D DIFT          E DS
008600101013     D DVTFPPI       E DS
008700120109     D TIVTF00F      E DS                  extname(TIVTF00F)
008800120109     D TIVTF_Save      DS           300
008900970526     D*------------
009000970526     D* ARCHITETTURA
009100970526     D*------------
009200970526     D KPJBA         E DS
009300080603     D*------------
009400080603     D* ESTENSIONE ANAGRAFICA CLIENTI/TIPI FILE
009500080603     D*------------
009600080603     D DVIRE01       E DS
009700040816     D*------------
009800040816     D* DS REPERIMENTO DATI CLIENTI
009900040816     D*------------
010000040816     DBS69DS         e ds                  extname(TIBS69DS)
010100040816     DACODS          e ds                  extname(cnaco00f)
010200040816     DINDDS          e ds                  extname(cnind00f)
010300040816     DCLPDS          e ds                  extname(cnclp00f)
010400040816     DCLSDS          e ds                  extname(fncls00f)
010500081020
010600081020     D*------------
010700081020     D* DS x richiamo *pgm utilità d controllo indirizzi e-mail
010800081020     D*------------
010900081020     D DSEMAIL       E DS
011000040816     D*------------
011100040816     D* VARIABILI D WRK
011200040816     D*------------
011300040816     D ksc             s              7  0 dim(30)
011400040816     D ksa             s              4    dim(30)
011500101026     D isvftpeml       s              2    dim(100)
011600040816     D skTipF          s              2    dim(500)
011700040816     D skPgt           s             10    dim(500)
011800040816     D skFileT         s             10    dim(500)
011900100311     D skMembroT       s             10    dim(500)
012000040816     D j               s              3  0 inz
012100040816     D wVIDPGT         s                   like(VIDPGT)
012200040816     D wVIDFIT         s                   like(VIDFIT)
012300120109     D MaxCiclo        s              3  0 inz
012400120119     D* Tips e Ksc DEVONO avere lo stesso numero di elementi
012500120109     D Tips            s              2    dim(3)
012600120119     D Kscs            s              8    dim(3)
012700111215     D*------------------
012800111215     D* Variabili x PASSAGGIO PARAMETRI A PROCEDURE UBRTVNETA
012900111215     D*------------------
013000111215     D currSysNeta     s              8A   inz('*NULL')
013100111215     D AS400_ok        s              1A
013200111215     D  PiOutSysNeta   S              8A
013300101026     D*-------------------
013400101026     D* DEFINIZIONI PROCEDURE ESTERNE
013500101026     D*-------------------
013600101026     D/COPY GAITRASRC/SRCPROTOPI,UBBVSQL01
013700101026     D/COPY GAITRASRC/SRCPROTOPR,UBBVSQL01
013800101027     D/COPY GAITRASRC/SRCPROTOPI,UBBVSKT01
013900101027     D/COPY GAITRASRC/SRCPROTOPR,UBBVSKT01
014000111215     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
014100970526     C*------------------------------------------------------------------------*
014200970526     C* MAIN LINES
014300970526     C*------------------------------------------------------------------------*
014400970526     C*
014500970526     C* NON CI SONO ERRORI IN ENTRATA
014600970526IF  1C     ERRL00        IFEQ      '0'
014700970526     C*
014800970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
014900970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
015000970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
015100970526     C     FINL00        OREQ      '1'                                          *FINE PGM
015200970526     C*
015300970526     C* EMETTE FORMATO VIDEO
015400970526     C                   EXSR      BUTFMR
015500970526     C*
015600970526     C* GESTIONE TASTI FUNZIONALI
015700970526     C                   EXSR      FUNCON
015800970526E   2C                   ENDDO
015900970526     C*
016000970526     C* CI SONO ERRORI IN ENTRATA
016100970527X   1C                   ELSE
016200991025     C                   MOVEL     '1'           S20err
016300991025     C                   MOVEL     DSMSMS        S20msg
016400970526E   1C                   ENDIF
016500970526     C* FINE PGM
016600991210     c                   exsr      finsr
016700101026     C*
016800101026     C* Chiudo la connessione
016900111215     C                   IF        AS400_ok = *on
017000111215     C                   exsr      clsCONN
017100111215     C                   ENDIF
017200101026     C*
017300970526     C                   SETON                                        LR
017400970526     C*------------------------------------------------------------------------*
017500970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
017600970526     C*------------------------------------------------------------------------*
017700970526     C     BUTFMR        BEGSR
017800970526     C*
017900970526     C* EMETTE LA TESTATA
018000991026     C                   WRITE     s723D01
018100970526     C*
018200970526     C* SPEDISCE EVENTUALE MESSAGGIO
018300970526     C                   EXSR      SNDMSG
018400970526     C*
018500970526     C* EMETTE LE FUNZIONI
018600991026     C                   WRITE     s723D02
018700970526     C*
018800970526     C* EMETTE IL VIDEO
018900991026     C                   EXFMT     s723D03
019000970526     C*
019100970526     C* REIMPOSTA FLAG DI ERRORE
019200970526     C                   MOVEL     '0'           ERRL00
019300970526     C*
019400970526     C* REIMPOSTA INDICATORI DI ERRORE
019500120119     C                   SETOFF                                       495051
019600120119     C                   SETOFF                                       525354
019700120119     C                   SETOFF                                       555657
019800970526     C                   SETOFF                                         5859
019900970526     C                   SETOFF                                       606162
020000970526     C                   SETOFF                                       636465
020100970526     C                   SETOFF                                       666768
020200970526     C                   SETOFF                                       697071
020300970526     C                   SETOFF                                       727374
020400120109     C                   SETOFF                                       757677
020500970526     C                   SETOFF                                       787980
020600970526     C                   SETOFF                                       818283
020700970526     C                   SETOFF                                       848586
020800970526     C                   SETOFF                                       878889
020900970526     C*
021000970526     C                   ENDSR
021100970526     C*------------------------------------------------------------------------*
021200970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
021300970526     C*------------------------------------------------------------------------*
021400970526     C     SNDMSG        BEGSR
021500970526     C*
021600970526     C* PULISCE IL SFILE MESSAGGI
021700970526     C                   MOVEL     '1'           *IN40
021800991026     C                   WRITE     s723DMC
021900970526     C                   MOVEL     '0'           *IN40
022000970526     C*
022100970526     C* VISUALIZZA IL MESSAGGIO
022200970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
022300970526     C                   CALL      'XMSG'        XMSGPA
022400970526IF  1C     DSMSER        IFEQ      *ZEROS
022500970526     C                   MOVEL     DSMSKE        MSGKEY
022600970526E   1C                   ENDIF
022700970526     C                   Z-ADD     1             MSGULT
022800991026     C                   WRITE     s723DMS
022900991026     C                   WRITE     s723DMC
023000970526     C*
023100970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
023200970526     C                   MOVEL     *BLANKS       DSMSMS
023300970526     C*
023400970526     C                   ENDSR
023500970526     C*------------------------------------------------------------------------*
023600970526     C* ENTGES - GESTIONE ENTER -> INVIO
023700970526     C*------------------------------------------------------------------------*
023800970526     C     ENTGES        BEGSR
023900970526     C*
024000970526     C* CONTROLLA I CAMPI IMMESSI
024100970526     C                   EXSR      DECODI
024200970526     C*
024300970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
024400970526IF  1C     DEPOP1        IFEQ      'O05'
024500970526     C                   MOVEL     '1'           FINL00
024600970526E   1C                   ENDIF
024700970526     C*
024800970526     C                   ENDSR
024900031229     C*------------------------------------------------------------------------*
025000031229     C* F01GES - GESTIONE F01 -> VISUALIZZA REGOLE PARAMETRI TRADUTTORE
025100031229     C*------------------------------------------------------------------------*
025200031229     C     F01GES        BEGSR
025300031229     C*
025400031229     C* Passo o il traduttore standard relativo al tipo file corrente o se inserito il
025500031229     C* traduttore specifico
025600031229     C                   IF        VIDPGT <> *blanks
025700031229     C                   EVAL      wPGT = VIDPGT
025800031229     C                   ELSE
025900031229     C                   EVAL      wPGT = VIDPGTD
026000031229     C                   ENDIF
026100031229     C*
026200031229     C* Visualizzo le regole relative
026300031229     C                   CALL      'TIS723C'
026400031229     C                   PARM                    wPGT             10
026500040107     C                   PARM                    wERR              1
026600040107     C*
026700040107     C* Verifico se trovato membro con descrizoine dettaglio parametri traduttore
026800040107     C                   IF        wERR = '1'
026900040107     C                   MOVEL     MSG(23)       DSMSMS
027000040107     C                   ENDIF
027100031229     C*
027200031229     C                   ENDSR
027300970526     C*------------------------------------------------------------------------*
027400970526     C* F03GES - GESTIONE F03 -> FINE
027500970526     C*------------------------------------------------------------------------*
027600970526     C     F03GES        BEGSR
027700970526     C*
027800970526     C* IMPOSTA "PREMUTO F03"
027900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
028000991025     C                   MOVEL     '1'           S20F03                         *DI PROCEDURA
028100970526     C*
028200970526     C                   ENDSR
028300970526     C*------------------------------------------------------------------------*
028400970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
028500970526     C*------------------------------------------------------------------------*
028600970526     C     F06GES        BEGSR
028700970526     C*
028800970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
028900970526IF  1C     DEPOP1        IFEQ      'F06'
029000970526     C                   EXSR      INSERI
029100970526E   1C                   ENDIF
029200970526IF  1C     DEPOP1        IFEQ      'O02'
029300970526     C                   EXSR      MODIFI
029400970526E   1C                   ENDIF
029500991014IF  1C     DEPOP1        IFEQ      'O03'
029600011129     C                   EXSR      COPIA
029700991014E   1C                   ENDIF
029800991104IF  1C*    DEPOP1        IFEQ      'O04'
029900991104     C*                  EXSR      ANNULL
030000991104E   1C*                  ENDIF
030100970526IF  1C     DEPOP1        IFEQ      'O07'
030200970526     C                   EXSR      RIPRIS
030300970526E   1C                   ENDIF
030400080603     C*
030500080603     C* Gestione estensione anagrafica clienti/tipi file => TRC 01
030600080603     C                   CLEAR                   DVIRE01
030700080603     C                   EVAL      KCEdti  = virDTI
030800080603     C                   EVAL      vireTRC = '01'
030900080603     C     KEYvire1_C    CHAIN     tivire1l
031000080603     C                   IF        %found(tivire1l)
031100080603     C                   IF        vidEML <> *blanks
031200080603     C                   EVAL      §vire01INV = 'S'
031300080603     C                   EVAL      §vire01EML = vidEML
031400080603     C                   EVAL      vireFLO = DVIRE01
031500080603     C                   UPDATE    tivire00
031600080603     C                   ELSE
031700080603     C                   DELETE    tivire00
031800080603     C                   ENDIF
031900080603     C                   ELSE
032000080603     C                   IF        vidEML <> *blanks
032100080603     C                   EVAL      vireKSC = virKSC
032200080603     C                   EVAL      vireTIP = virTIP
032300080603     C                   EVAL      vireDTI = virDTI
032400080603     C                   EVAL      vireTRC = '01'
032500080603     C                   EVAL      §vire01INV = 'S'
032600080603     C                   EVAL      §vire01EML = vidEML
032700080603     C                   EVAL      vireFLO = DVIRE01
032800080603     C                   WRITE     tivire00
032900080603     C                   ENDIF
033000080603     C                   ENDIF
033100970526     C*
033200011129     C* Dopo aver effettuato le operazioni richieste apro la tabela correlata al tipo file.
033300011129     C                   IF        errl00 = '0'                                 *NO ERRORI
033400011129     C                   CALL      'TIS70UR'
033500011129     C                   PARM                    KPJBA
033600011129     C                   PARM                    VIDTIP
033700011129     C                   PARM                    VIDKSC
033800011129     C                   PARM      *blanks       flgope            1
033900011129     C                   ENDIF
034000011129     C*
034100970526     C                   ENDSR
034200970526     C*------------------------------------------------------------------------*
034300970526     C* F12GES - GESTIONE F12 -> RITORNO
034400970526     C*------------------------------------------------------------------------*
034500970526     C     F12GES        BEGSR
034600970526     C*
034700970526     C* IMPOSTA "PREMUTO F12"
034800970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
034900991025     C                   MOVEL     '1'           S20F12                         *DI PROCEDURA
035000970526     C*
035100970526     C                   ENDSR
035200970526     C*------------------------------------------------------------------------*
035300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
035400970526     C*------------------------------------------------------------------------*
035500970526     C     F24GES        BEGSR
035600970526     C*
035700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
035800970526     C*
035900970526     C                   ENDSR
036000970526     C*------------------------------------------------------------------------*
036100970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
036200970526     C*------------------------------------------------------------------------*
036300970526     C     INSERI        BEGSR
036400970526     C*
036500970526     C* CONTROLLA I CAMPI IMMESSI
036600970526     C                   EXSR      DECODI
036700970526     C*
036800970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
036900991026IF  1C                   if        errl00 = '0'                                 *NO ERRORI
037000991026     C                   movel     VIDTip        virtip
037100991026     C                   move      VIDksc        virksc
037200991026     C                   z-add     VIDseq        virseq
037300991026     C                   Z-ADD     viddti        G08dat
037400991026     C                   Z-ADD     *ZEROS        G08inv
037500991026     C                   MOVEL     '0'           G08ERR
037600991026     C                   CALL      'XSRDA8'
037700991026     C                   PARM                    WLBDA8
037800991026     C                   Z-ADD     G08inv        virdti
037900991026     c                   exsr      impvir
038000991026     C                   WRITE     tivir000
038100991025     C                   MOVEL     MSG(2)        S20MSG                         *MESSAGGIO
038200970602     C*
038300970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
038400020228     C****               EXSR      AZZVID
038500970526E   1C                   ENDIF
038600970526     C*
038700970526     C                   ENDSR
038800970526     C*------------------------------------------------------------------------*
038900970526     C* MODIFI - OPERAZIONI DI MODIFICA
039000970526     C*------------------------------------------------------------------------*
039100970526     C     MODIFI        BEGSR
039200970526     C*
039300970526     C* CONTROLLA I CAMPI IMMESSI
039400970526     C                   EXSR      DECODI
039500970526     C*
039600970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
039700970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
039800991026     C                   EXSR      LETvir
039900970526     C*
040000970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
040100970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
040200991026     C                   EXSR      IMPvir                                       *IMPOSTA I CAMPI
040300991026     C                   UPDATE    tivir000
040400101027     C                   EVAL      S20MSG = MSG(3)
040500101027     C                   EVAL      DSMSMS = MSG(3)
040600101027     C*
040700101027     C* Se trattasi d tipo servizio "idoneo" modifico anche i dati utente lato "BARTVAS"
040800101027     C                   IF        *IN80
040900111215     C                             and AS400_ok = *on
041000101027      /FREE
041100101027
041200101027         if *in30 = *off;
041300101027           pInMaskChk  = VIDPPIMC;
041400101027         else;
041500101027           pInMaskChk  = pOutCliMskChk;
041600101027         endif;
041700101027
041800101027         if *in31 = *off;
041900101027           pInMaskFile = VIDPPIMF;
042000101027         else;
042100101027           pInMaskFile = pOutCliMskFil;
042200101027         endif;
042300101027
042400101027         if *in32 = *off;
042500101027           pInMaskSem  = VIDPPIMS;
042600101027         else;
042700101027           pInMaskSem  = pOutCliMskSem;
042800101027         endif;
042900101027
043000101027         if *in33 = *off;
043100101027           pInMaskZip  = VIDPPIMZ;
043200101027         else;
043300101027           pInMaskZip  = pOutCliMskZip;
043400101027         endif;
043500101027
043600101027
043700101027         if UBBVSQL01_Upd01UnqCliTipFile(
043800101027                         pInConn
043900101027                       : VIDKSC
044000101027                       : VIDTIP
044100101027                       : pInMaskChk
044200101027                       : pInMaskFile
044300101027                       : pInMaskSem
044400101027                       : pInMaskZip) = 0;
044500101027         else;
044600101027              S20MSG = MSG(29);
044700101027              DSMSMS = MSG(29);
044800101027         endif;
044900101027
045000101027      /END-FREE
045100101027     C
045200101027     C*
045300101027     C* Quindi (già che c sono ricarico anche il job
045400101027     C                   callp(e)  UBBVSKT01_ReloadCliTipF(
045500101027     C                                           VIDKSC
045600101027     C                                          :VIDTIP
045700101027     C                                          :pOutCliProc)
045800101027     C                   ENDIF
045900101027     C*
046000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
046100970526E   2C                   ENDIF
046200970526E   1C                   ENDIF
046300970526     C*
046400970526     C                   ENDSR
046500991014     C*------------------------------------------------------------------------*
046600991014     C* copia  - OPERAZIONI DI copia
046700991014     C*------------------------------------------------------------------------*
046800991014     C     copia         BEGSR
046900991014     C*
047000991014     C* CONTROLLA I CAMPI IMMESSI
047100991014     C                   EXSR      DECODI
047200991014     C*
047300991014     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
047400991014IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
047500991026     C                   movel     VIDTip        virtip
047600991110     C                   move      VIDksc        virksc
047700991110     C                   z-add     VIDseq        virseq
047800991110     C                   Z-ADD     viddti        G08dat
047900991110     C                   Z-ADD     *ZEROS        G08inv
048000991110     C                   MOVEL     '0'           G08ERR
048100991110     C                   CALL      'XSRDA8'
048200991110     C                   PARM                    WLBDA8
048300991110     C                   Z-ADD     G08inv        virdti
048400991026     C                   EXSR      IMPvir                                       *IMPOSTA I CAMPI
048500991026     C                   write     tivir000
048600991025     C                   MOVEL     MSG(3)        S20MSG
048700991014     C                   MOVEL     '1'           FINL00                         *FINE PGM
048800991014E   2C                   ENDIF
048900991014     C*
049000991014     C                   ENDSR
049100970526     C*------------------------------------------------------------------------*
049200970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
049300970526     C*------------------------------------------------------------------------*
049400991104     C*    ANNULL        BEGSR
049500991025     C*
049600991025     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
049700991104     C*                  EXSR      LETvir
049800991025     C*
049900991025     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
050000991104IF  1C*    ERRL00        IFEQ      '0'                                          *NO ERRORI
050100991104     C*                  DELETE    tivir000                                     *CANCELLA
050200991104     C*                  MOVEL     MSG(4)        s20MSG                         *MESSAGGIO
050300991104     C*                  MOVEL     '1'           FINL00                         *FINE PGM
050400991104E   1C*                  ENDIF
050500970526     C*
050600991104     C*                  ENDSR
050700970526     C*------------------------------------------------------------------------*
050800970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
050900970526     C*------------------------------------------------------------------------*
051000970526     C     RIPRIS        BEGSR
051100970526     C                   ENDSR
051200970526     C*------------------------------------------------------------------------*
051300991026     C* LETvir - LEGGE IL RECORD PERL'AGGIORNAMENTO
051400970526     C*------------------------------------------------------------------------*
051500991026     C     LETvir        BEGSR
051600970526     C*
051700991025     C                   MOVEL     S20tip        KCEtip
051800991026     C                   MOVEL     S20ksc        KCEksc
051900000420     C     KEYvi2        CHAIN     tivir03L                           9998
052000970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
052100970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
052200991025     C                   MOVEL     MSG(7)        S20MSG                         *MESSAGGIO
052300970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
052400970526E   1C                   ENDIF
052500970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
052600970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
052700991025     C                   MOVEL     MSG(6)        S20MSG                         *MESSAGGIO
052800970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
052900970526E   1C                   ENDIF
053000970526     C*
053100970526     C                   ENDSR
053200970526     C*------------------------------------------------------------------------*
053300970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
053400970526     C*------------------------------------------------------------------------*
053500970526     C     DECODI        BEGSR
053600001120     C*
053700001120     C*----------------------
053800001120     C* Controllo del codice
053900001120     C*----------------------
054000001120     C     ERRL00        IFEQ      '0'
054100001120     C     VIDksc        IFne      *blank
054200001120     C     VIDksc        andne     *zeros
054300001120     C                   exsr      cntksc
054400001120     C                   ELSE
054500991027     C                   MOVEL     '1'           ERRL00
054600991027     C                   SETON                                        65
054700991027     C                   MOVEL     MSG(12)       DSMSMS
054800001120     C                   ENDIF
054900001120     C                   ENDIF
055000101026     C*
055100101026     C* Verifico se x il servizio a video è richiesta l'indicazione della password FTP
055200101026     C     ERRL00        IFEQ      '0'
055300101026     C     VIDisv        lookup    isvftpeml                              80
055400101026 xxx C***                setoff                                       80
055500101026     C                   ENDIF
055600001120     C*----------------------
055700001120     C* Controllo della sequenza
055800001120     C*----------------------
055900001120     C     ERRL00        IFEQ      '0'
056000001120     C     VIDseq        IFne      0
056100001120     C                   ELSE
056200991027     C                   MOVEL     '1'           ERRL00
056300991027     C                   SETON                                        67
056400991027     C                   MOVEL     MSG(12)       DSMSMS
056500001120     C                   ENDIF
056600001120     C                   ENDIF
056700120109     C*----------------------
056800120109     C* Controllo tipo file
056900120109     C*----------------------
057000120109     C     ERRL00        IFEQ      '0'
057100120109     C     VIDtip        IFne      *blank
057200120109     C                   exsr      cnttip
057300120109     C                   ELSE
057400120109     C                   MOVEL     '1'           ERRL00
057500120109     C                   SETON                                        66
057600120109     C                   MOVEL     MSG(12)       DSMSMS
057700120109     C                   ENDIF
057800120109     C                   ENDIF
057900120109     C*----------------------
058000120109     C* Tipo servizio
058100120109     C*----------------------
058200120109     C     ERRL00        IFEQ      '0'
058300120109     C     VIDisv        IFne      *blank
058400120109     C                   exsr      cntisv
058500120109     C* Verifico la compatibilità del servizio rispetto al tipo-file
058600120109     C                   IF        VTFUD <> §ISVVERS
058700120109     C                   MOVEL     '1'           ERRL00
058800120109     C                   SETON                                        6670
058900120109     C                   MOVEL     MSG(24)       DSMSMS
059000120109     C                   ENDIF
059100120109     C                   ELSE
059200120109     C                   MOVEL     '1'           ERRL00
059300120109     C                   SETON                                        70
059400120109     C                   MOVEL     MSG(12)       DSMSMS
059500120109     C                   ENDIF
059600120109     C                   ENDIF
059700120109     C*--------------------
059800120109     C* Dati solo x upload no in download
059900120109     C*--------------------
060000120109     C* a seconde se siamo in Download o Upload sproteggo alcuni campi e ne proteggo altri
060100120109     C* e se non abbiamo errori pulisco i campi protetti (se abbiamo un errore potrebbe essere
060200120109     C*  che non siamo in D o U e i valori nei campi servono)
060300120109     C                   IF        §ISVVers   = 'D'
060400120109     c                   SETON                                        93
060500120109     C     ERRL00        IFEQ      '0'
060600120109     C                   MOVEL     *blanks       VIDpoi
060700120109     C                   MOVEL     *blanks       VIDEmL
060800120109     C                   MOVEL     *blanks       VIDfi2
060900120109     C                   MOVEL     *blanks       VIDfi4
061000120109     C                   MOVEL     *blanks       VIDfi4D
061100120109     C                   ENDIF
061200120109     C                   ENDIF
061300120109     C                   IF        §ISVVers   = 'U'
061400120109     C                   SETOFF                                       93
061500120109     C     ERRL00        IFEQ      '0'
061600120109     C                   MOVEL     *blanks       VIDTMbD
061700120109     C                   MOVEL     *blanks       VIDTip2
061800120109     C                   MOVEL     *blanks       VIDTip3
061900120109     C                   MOVEL     *blanks       VIDTip4
062000120119     C                   MOVEL     *blanks       VIDKsc2
062100120119     C                   MOVEL     *blanks       VIDKsc3
062200120119     C                   MOVEL     *blanks       VIDKsc4
062300120109     C                   ENDIF
062400120109     C                   ENDIF
062500100614     C*--------------------
062600100614     C* Tipo membro d default...  solo x anagrafiche d download (ovviamente no in visualizz.)
062700100614     C*--------------------
062800100614     C     ERRL00        IFEQ      '0'
062900100614     C                   IF        DEPOP1 = 'F06' OR
063000100614     C                             DEPOP1 = 'O03'
063100100614     C                   IF        §ISVPROC = 'F' AND
063200100614     C                             §ISVVERS = 'D'
063300100614     C                   IF        VIDtmbd = *blanks AND
063400100614     C                             vtfud   = 'D'
063500100614     C                   MOVEL     'M'           VIDtmbd
063600100614     C                   ENDIF
063700100614     C                   ENDIF
063800100614     C                   ENDIF
063900100614     C                   ENDIF
064000030603     C*----------------------
064100030603     C* Tipo membro
064200030603     C*----------------------
064300030603     C     ERRL00        IFEQ      '0'
064400030603     C     VIDtmbd       IFNE      *blank
064500030603     C                   IF        §ISVPROC = 'F' AND
064600030603     C                             §ISVVERS = 'D'
064700030603     C                   ELSE
064800030603     C                   MOVEL     '1'           ERRL00
064900030603     C                   SETON                                        61
065000030603     C                   MOVEL     MSG(22)       DSMSMS
065100030603     C                   ENDIF
065200030603     C                   ENDIF
065300030603     C                   ENDIF
065400120109     C*----------------------
065500120109     C* Campi duplicazione automatica su tipi file
065600120109     C*----------------------
065700120109     C     ERRL00        IFEQ      '0'
065800120109     C*
065900120109     C                   EVAL      MaxCiclo = %elem(Tips)
066000120109     C                   EVAL      Tips(1)  = VIDTip2
066100120109     C                   EVAL      Tips(2)  = VIDTip3
066200120109     C                   EVAL      Tips(3)  = VIDTip4
066300120119     C* se il tipo file è valorizzato e il cliente relativo no (o 0 o ' ')
066400120119     C* lo forzo col cliente principale
066500120119     C                   IF        (VIDKsc2 = *zeros or
066600120119     C                              VIDKsc2 = *blank   ) and
066700120119     C                             VIDTip2 <> *blank
066800120119     C                   EVAL      VIDKsc2 =  VIDKsc
066900120119     C                   ENDIF
067000120119     C                   IF        (VIDKsc3 = *zeros or
067100120119     C                              VIDKsc3 = *blank   ) and
067200120119     C                             VIDTip3 <> *blank
067300120119     C                   EVAL      VIDKsc3 =  VIDKsc
067400120119     C                   ENDIF
067500120119     C                   IF        (VIDKsc4 = *zeros or
067600120119     C                              VIDKsc4 = *blank   ) and
067700120119     C                             VIDTip4 <> *blank
067800120119     C                   EVAL      VIDKsc4 =  VIDKsc
067900120119     C                   ENDIF
068000120119     C                   EVAL      Kscs(1)  = VIDKsc2
068100120119     C                   EVAL      Kscs(2)  = VIDKsc3
068200120119     C                   EVAL      Kscs(3)  = VIDKsc4
068300120109     C     1             DO        MaxCiclo      i
068400120109     C     Tips(i)       IFNE      *blank
068500120119     C     Kscs(i)       ORNE      *blank
068600120119     C     Kscs(i)       ANDNE     *zeros
068700120109     C                   IF        §ISVVERS = 'D'
068800120119     C                   EXSR      CntTip234
068900120111     C* se mi è stato dato un errore, esco dal ciclo
069000120111     C                   IF        ERRL00 <> '0'
069100120111     C                   LEAVE
069200120111     C                   ENDIF
069300120109     C                   ELSE
069400120109     C                   SELECT
069500120109     C                   WHEN      i = 1
069600120119     C                   SETON                                        7549
069700120109     C                   WHEN      i = 2
069800120119     C                   SETON                                        7650
069900120109     C                   WHEN      i = 3
070000120119     C                   SETON                                        7751
070100120109     C                   ENDSL
070200120109     C                   MOVEL     '1'           ERRL00
070300120109     C                   MOVEL     MSG(30)       DSMSMS
070400120109     C                   ENDIF
070500120109     C                   ENDIF
070600120109     C                   ENDDO
070700120109     C*
070800120109     C                   ENDIF
070900001120     C*----------------------
071000001120     C* Controllo della descrizione
071100001120     C*----------------------
071200001120     C     ERRL00        IFEQ      '0'
071300001120     C     VIDdes        IFne      *blanks
071400001120     C                   ELSE
071500991122     C*                  MOVEL     '1'           ERRL00
071600991122     C*                  SETON                                        62
071700991122     C*                  MOVEL     MSG(12)       DSMSMS
071800991122     C                   MOVEL     VDDTIP        VIDDES
071900001120     C                   ENDIF
072000001120     C                   ENDIF
072100991026     C*--------------------
072200991026     C* DATA inizio
072300991026     C*--------------------
072400991026     C     *LIKE         DEFINE    VIDDti        DtiVID
072500001120     C     ERRL00        IFEQ      '0'
072600991026     C                   Z-ADD     VIDDti        wda8
072700991026     C                   Z-ADD     *ZEROS        da8w
072800991026     C                   EXSR      CHKDA8
072900991026     C                   Z-ADD     WDA8          viddti
073000991026     C                   Z-ADD     DA8W          dtivid
073100001120     C     ERRL00        IFEQ      '1'
073200991026     C                   SETON                                        63
073300991026     C                   MOVEL     MSG(14)       dsmsms
073400001120     C                   ENDIF
073500001120     C                   ENDIF
073600991026     C* INSERIMENTO -> RECORD INESISTENTE
073700001120     C     ERRL00        IFEQ      '0'
073800001120     C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
073900001120     C     DEPOP1        orEQ      'O03'                                        *INSERIMENTO
074000991026     C                   move      vidksc        KCEksc
074100000420     C                   move      vidtip        KCEtip
074200000420     C                   z-add     dtivid        KCEdti
074300000420     C     KEYvi2        CHAIN(N)  tivir03L                           99
074400001120     C     *IN99         IFEQ      *OFF
074500991026     C                   MOVEL     '1'           ERRL00
074600991026     C                   MOVEL     MSG(8)        DSMSMS
074700991026     C                   SETON                                        676566
074800991026     C                   SETON                                        63
074900001120     C                   ENDIF
075000001120     C                   ENDIF
075100001120     C                   ENDIF
075200001120     C*--------------------
075300991026     C* DATA SCADENZA
075400991026     C*--------------------
075500991026     C     *LIKE         DEFINE    VIDDtf        DtfVID
075600001120     C     ERRL00        IFEQ      '0'
075700991026     C                   Z-ADD     VIDDtf        WDA8
075800991026     C                   Z-ADD     *ZEROS        DA8W
075900991026     C                   EXSR      CHKDA8
076000991026     C                   Z-ADD     WDA8          VIDDtf
076100991026     C                   Z-ADD     DA8W          DtfVID
076200001120     C     ERRL00        IFEQ      '1'
076300991026     C                   SETON                                        68
076400991026     C                   MOVEL     MSG(14)       DSMSMS
076500001120     C                   ENDIF
076600001120     C                   ENDIF
076700991026     C* SCADENZA < DECORRENZA
076800001120     C     ERRL00        IFEQ      '0'
076900001120     C     DtfVID        IFLT      DtiVID
077000991026     C                   SETON                                        68
077100991026     C                   MOVEL     MSG(15)       DSMSMS
077200991026     C                   MOVEL     '1'           ERRL00
077300001120     C                   ENDIF
077400001120     C                   ENDIF
077500991026     C* SCADENZA >= DATA CORRENTE
077600001120     C     ERRL00        IFEQ      '0'
077700001120     C     DEPOP1        IFEQ      'O02'
077800991026     C     DEPOP1        OREQ      'O07'
077900991026     C     DEPOP1        OREQ      'F06'
078000001120     C     DtfVID        IFLT      DATCOR
078100001120     C                   SETON                                        68
078200991026     C                   MOVEL     MSG(16)       DSMSMS
078300991026     C                   MOVEL     '1'           ERRL00
078400001120     C                   ENDIF
078500001120     C                   ENDIF
078600001120     C                   ENDIF
078700991026     C*---
078800991026     C* DECORRENZA + SCADENZA
078900991026     C*---
079000991026     C* ACCAVALLAMENTO DATI
079100001120     C     ERRL00        IFEQ      '0'
079200991026     C                   MOVEL     VIDtip        KCEtip
079300991026     C                   MOVEL     vidksc        KCEksc
079400991026     C     KEyvir        SETLL     tivir03L
079500991026     C     KEyvir        READE(N)  tivir03L                               99
079600001120     C     *IN99         DOWEQ     *OFF
079700001120     C     DEPOP1        IFNE      'F06'                                        *NON INSERIMENTO
079800991026     C     DtiVID        ANDNE     virdti                                       *RECORD IN LINEA
079900991026     C     DEPOP1        OREQ      'F06'                                        *INSERIMENTO
080000001120     C     DtiVID        IFGE      virdti                                       *DECORRENZA
080100991026     C     DtiVID        ANDLE     virdtf
080200991026     C     DtfVID        ORGE      virdti                                       *SCADENZA
080300991026     C     DtfVID        ANDLE     virdtf
080400991026     C     virdti        ORGE      DtiVID                                       *DECORRENZA
080500991026     C     virdtf        ANDLE     DtfVID
080600991026     C                   SETON                                        666765
080700991026     C                   SETON                                        6368
080800991026     C                   MOVEL     MSG(17)       DSMSMS
080900991026     C                   MOVEL     '1'           ERRL00
081000991026     C                   LEAVE
081100001120     C                   ENDIF
081200001120     C                   ENDIF
081300991026     C     KEyvir        READE(N)  tivir03L                               99
081400001120     C                   ENDDO
081500001120     C                   ENDIF
081600001120     C*--------------------
081700001120     C* Controllo filiale
081800001120     C*--------------------
081900001120     C     ERRL00        IFEQ      '0'
082000001120     C     VIDpoi        IFne      *blank
082100001120     C     VIDpoi        andne     *zeros
082200001120     C                   exsr      cntpoi
082300001120     C                   ELSE
082400120109     C     §ISVVers      IFNE      'D'
082500991027     C                   MOVEL     '1'           ERRL00
082600000724     C                   MOVEL     MSG(12)       DSMSMS
082700991027     C                   SETON                                        69
082800991122     C                   ENDIF
082900001120     C                   ENDIF
083000001120     C                   ENDIF
083100001120     C*--------------------
083200011016     C* Programma traduttore
083300001120     C*--------------------
083400001120     C     ERRL00        IFEQ      '0'
083500001120     C     VIDpgt        IFNE      *blank
083600001120     C                   CALL(e)   'TIS721C'
083700050112     C                   PARM      *blanks       LIBRERIA         10
083800001120     C                   PARM      VIDpgt        OBJECT           10
083900001120     C                   PARM      '*PGM   '     TYPE              7
084000001120     C                   PARM      '0'           ESITO             1
084100001120     C                   IF        ESITO = '1'
084200001120     C                   MOVEL     '1'           ERRL00
084300001120     C                   SETON                                        54
084400001120     C                   MOVEL     MSG(19)       DSMSMS
084500001120     C                   ENDIF
084600001120     C                   ENDIF
084700001120     C                   ENDIF
084800001120     C*--------------------
084900001120     C* Programma post-traduzione
085000001120     C*--------------------
085100001120     C     ERRL00        IFEQ      '0'
085200001120     C     VIDpgi        IFNE      *blank
085300001120     C                   CALL(e)   'TIS721C'
085400050112     C                   PARM      *blanks       LIBRERIA         10
085500001120     C                   PARM      VIDpgi        OBJECT           10
085600001120     C                   PARM      '*PGM   '     TYPE              7
085700001120     C                   PARM      '0'           ESITO             1
085800001120     C                   IF        ESITO = '1'
085900001120     C                   MOVEL     '1'           ERRL00
086000001120     C                   SETON                                        55
086100001120     C                   MOVEL     MSG(19)       DSMSMS
086200001120     C                   ENDIF
086300001120     C                   ENDIF
086400001120     C                   ENDIF
086500001120     C*--------------------
086600001120     C* File da tradurre
086700001120     C*--------------------
086800001120     C     ERRL00        IFEQ      '0'
086900001120     C     VIDfid        IFNE      *blank
087000001120     C                   CALL(e)   'TIS721C'
087100050112     C                   PARM      *blanks       LIBRERIA         10
087200001120     C                   PARM      VIDfid        OBJECT           10
087300001120     C                   PARM      '*FILE  '     TYPE              7
087400001120     C                   PARM      '0'           ESITO             1
087500001120     C                   IF        ESITO = '1'
087600001120     C                   MOVEL     '1'           ERRL00
087700001120     C                   SETON                                        53
087800001120     C                   MOVEL     MSG(20)       DSMSMS
087900001120     C                   ENDIF
088000001120     C                   ENDIF
088100001120     C                   ENDIF
088200001120     C*--------------------
088300001120     C* File storico
088400001120     C*--------------------
088500001120     C     ERRL00        IFEQ      '0'
088600001120     C     VIDfis        IFNE      *blank
088700001120     C                   CALL(e)   'TIS721C'
088800050112     C                   PARM      *blanks       LIBRERIA         10
088900001120     C                   PARM      VIDfis        OBJECT           10
089000001120     C                   PARM      '*FILE  '     TYPE              7
089100001120     C                   PARM      '0'           ESITO             1
089200001120     C                   IF        ESITO = '1'
089300001120     C                   MOVEL     '1'           ERRL00
089400001120     C                   SETON                                        56
089500001120     C                   MOVEL     MSG(20)       DSMSMS
089600001120     C                   ENDIF
089700001120     C                   ENDIF
089800001120     C                   ENDIF
089900001120     C*--------------------
090000001120     C* File tradotto
090100001120     C*--------------------
090200001120     C     ERRL00        IFEQ      '0'
090300001120     C     VIDfit        IFNE      *blank
090400001120     C                   CALL(e)   'TIS721C'
090500050112     C                   PARM      VIDfi3        LIBRERIA         10
090600001120     C                   PARM      VIDfit        OBJECT           10
090700001120     C                   PARM      '*FILE  '     TYPE              7
090800001120     C                   PARM      '0'           ESITO             1
090900001120     C                   IF        ESITO = '1'
091000001120     C                   MOVEL     '1'           ERRL00
091100001120     C                   SETON                                        57
091200001120     C                   MOVEL     MSG(20)       DSMSMS
091300001120     C                   ENDIF
091400001120     C                   ENDIF
091500001120     C                   ENDIF
091600001120     C*--------------------
091700001120     C* Libreria file tradotto
091800001120     C*--------------------
091900001120     C     ERRL00        IFEQ      '0'
092000001120     C     VIDfi3        IFNE      *blank
092100001120     C                   CALL(e)   'TIS721C'
092200050112     C                   PARM      *blanks       LIBRERIA         10
092300001120     C                   PARM      VIDfi3        OBJECT           10
092400001120     C                   PARM      '*LIB   '     TYPE              7
092500001120     C                   PARM      '0'           ESITO             1
092600001120     C                   IF        ESITO = '1'
092700001120     C                   MOVEL     '1'           ERRL00
092800001120     C                   SETON                                        52
092900001120     C                   MOVEL     MSG(21)       DSMSMS
093000001120     C                   ENDIF
093100001120     C                   ENDIF
093200001120     C                   ENDIF
093300040816     C*--------------------
093400100311     C* Controllo corrispondenza Tipo File / Programma Traduttore / File Tradotto / Tipo membro Trad
093500040816     C*--------------------
093600040816     C     ERRL00        IFEQ      '0'
093700040816     C     VIDfit        IFne      *blank
093800040816     C     VIDpgt        ORne      *blank
093900040816     C                   EVAL      TBEcod = 'IFT'
094000040816     C     KEYtbe1_P     SETLL     tntbe01l
094100040816     C                   IF        %equal(tntbe01l)
094200040816     C* Inizializzo indici e schiere
094300040816     C                   Z-ADD     *zeros        j
094400040816     C                   CLEAR                   skTipF
094500040816     C                   CLEAR                   skPgt
094600040816     C                   CLEAR                   skFileT
094700100311     C                   CLEAR                   skMembroT
094800040816     C* Carico le schiere da tabella 'IFT'
094900040816     C     KEYtbe1_P     READE     tntbe01l
095000040816     C                   DOW       not %eof(tntbe01l)
095100040816     C                   IF        TBEatb = *blanks
095200040816     C                   EVAL      DIFT = TBEuni
095300040816     C                   ADD       1             j
095400100311     C                   EVAL      skTipF(j)    = TBEke1
095500100311     C                   EVAL      skPgt(j)     = §IFTPGT
095600100311     C                   EVAL      skFileT(j)   = §IFTFILE
095700100311     C                   EVAL      skMembroT(j) = §IFTTMBD
095800040816     C                   ENDIF
095900040816     C     KEYtbe1_P     READE     tntbe01l
096000040816     C                   ENDDO
096100040816     C* Imposto cosa testare
096200040816     C                   IF        VIDPGT <> *blanks
096300040816     C                   EVAL      wVIDPGT = VIDPGT
096400040816     C                   ELSE
096500040816     C                   EVAL      wVIDPGT = VIDPGTD
096600040816     C                   ENDIF
096700040816     C                   IF        VIDFIT <> *blanks
096800040816     C                   EVAL      wVIDFIT = VIDFIT
096900040816     C                   ELSE
097000040816     C                   EVAL      wVIDFIT = VIDFITD
097100040816     C                   ENDIF
097200040816     C* Testo se il traduttore indicato è tra quelli da controllare (in tabella 'IFT')
097300040823     C                   MOVEL     '0'           wFlgIFTok         1
097400100223     C                   SETOFF                                       16
097500040816     C     wVIDPGT       LOOKUP    skPgt                                  15
097600040816     C                   IF        %found
097700040816     C* Testo se vi è corrispondenza
097800040816     C                   EVAL      j = 1
097900040816     C                   DOW       j <= %elem(skTipF)
098000100223     C                   IF        skTipF(j)  = VIDTIP  AND
098100100223     C                             skPgt(j)   = wVIDPGT
098200100223     C                   SETON                                        16
098300100311     C*
098400100223     C                   IF        skFileT(j) = wVIDFIT
098500040816     C                   MOVEL     '1'           wFlgIFTok
098600100311     C*
098700100311     C                   IF        DEPOP1 = 'O02' OR
098800100311     C                             DEPOP1 = 'O03' OR
098900100311     C                             DEPOP1 = 'F06'
099000100312     C                   IF        VIDtmbd      =  *blanks AND
099100100312     C                             skMembroT(j) <> *blanks
099200100311     C                   EVAL      VIDtmbd = skMembroT(j)
099300100311     C                   SETON                                        61
099400100311     C                   ENDIF
099500100311     C                   ENDIF
099600100311     C*
099700040816     C                   LEAVE
099800040816     C                   ENDIF
099900100311     C*
100000100223     C                   ENDIF
100100040816     C                   ADD       1             j
100200040816     C                   ENDDO
100300040816     C* Se nn trovata esatta corrispondenza => errore
100400100223     C                   IF        *IN16 and wFlgIFTok = '0'
100500040816     C                   MOVEL     '1'           ERRL00
100600040816     C                   MOVEL     MSG(25)       DSMSMS
100700040816     C                   SETON                                        665457
100800040816     C                   ENDIF
100900040816     C                   ENDIF
101000040816     C                   ENDIF
101100040816     C                   ENDIF
101200040823     C                   ENDIF
101300051026     C*--------------------
101400051026     C* Controllo particolare x anagrafiche d download: se si sta cambiando il traduttore
101500051026     C* verifico prima che nn vi siano attualmente record d log a stato zero
101600051026     C*--------------------
101700051026     C     ERRL00        IFEQ      '0'
101800120109     C     §ISVVers      IFEQ      'D'
101900051026     C     VIDpgt        IFNE      virPGT
102000051026     C     VIDpgi        ORNE      virPGI
102100051026     C                   CALL      'TIS723R1'
102200051026     C                   PARM                    VIDKSC
102300051026     C                   PARM                    VIDTIP
102400120109     C                   PARM                    §ISVVers
102500051026     C                   PARM      *zeros        wRecZero          9 0
102600051026     C                   IF        wRecZero > *zeros
102700051026     C                   MOVEL     '1'           ERRL00
102800051026     C                   MOVEL     MSG(26)       DSMSMS
102900051026     C                   SETON                                        5455
103000051026     C                   ENDIF
103100051026     C                   ENDIF
103200051026     C                   ENDIF
103300051026     C                   ENDIF
103400060616     C*--------------------
103500060616     C* File Da tradurre / File Storico / Programma Post-Traduzione
103600060616     C*--------------------
103700060616     C     ERRL00        IFEQ      '0'
103800060616     C                   IF        (VIDfid='*' AND VIDfis='*' AND VIDpgi='*') OR
103900060616     C                             (VIDfid<>'*' AND VIDfis<>'*' AND VIDpgi<>'*')
104000060616     C                   ELSE
104100060616     C                   MOVEL     '1'           ERRL00
104200060616     C                   SETON                                        535556
104300060616     C                   MOVEL     MSG(27)       DSMSMS
104400060616     C                   ENDIF
104500060616     C                   ENDIF
104600081020     C*--------------------
104700081020     C* Mail x eventuale feed-back richiesto
104800081020     C*--------------------
104900081020     C     ERRL00        IFEQ      '0'
105000081020     C                   CLEAR                   DSEMAIL
105100081020     C                   EVAL      §EMLINDI = VIDEML
105200081020     C                   CALL      'XEMAIL'
105300081020     C                   PARM                    DSEMAIL
105400081020     C                   IF        §EMLERRO = '1'
105500081020     C                   MOVEL     '1'           ERRL00
105600081020     C                   SETON                                        64
105700081020     C                   MOVEL     §EMLMSGO      DSMSMS
105800081020     C                   ENDIF
105900081020     C                   ENDIF
106000001120     C*
106100001120     C                   ENDSR
106200000626     C*------------------------------------------------------------------------*
106300000626     C* Cntisv - CONTROLLA codice tipo legame
106400000626     C*------------------------------------------------------------------------*
106500001120     C     cntisv        begsr
106600001120     C                   clear                   TIBS02DS
106700001120     C*
106800101027     C     '?'           SCAN      Vidisv                                 28
106900001120     C*  Ricerca
107000001120     C                   eval      t02tla = 'L'
107100101027     C                   IF        *IN28 = *ON
107200001120     C                   movel     'R'           t02mod
107300001120     C                   else
107400001120     C                   movel     'C'           t02mod
107500001120     C                   movel(p)  vidisv        t02ke1
107600001120     C                   end
107700000626     C                   movel     'ISV'         t02cod
107800001120     C                   movel     knsif         t02sif
107900001120     C                   CALL      'TIBS02R'
108000001120     C                   parm                    KPJBA
108100001120     C                   parm                    TIBS02DS
108200001120     C
108300001120     C*
108400001120     C                   IF        T02err = *blanks
108500000626     C                   movel     T02ke1        Vidisv
108600000626     C                   movel     T02uni        vddisv
108700030603     C                   movel(P)  T02uni        DISV
108800001120     C                   else
108900001120     C                   movel     '1'           errl00
109000001120     C                   seton                                        70
109100001120     C                   MOVEL     MSG(18)       DSMSMS
109200001120     C                   ENDIF
109300001120     C
109400001120     C                   endsr
109500991027     C*------------------------------------------------------------------------*
109600991027     C* cnttip - Controlla esistenza tipo file su tivt00f
109700991027     C*------------------------------------------------------------------------*
109800001120     C     cnttip        begsr
109900001120     C                   clear                   TIBS02DS
110000000626     C*
110100101027     C     '?'           SCAN      vidtip                                 28
110200001120     C*  Ricerca
110300001120     C                   eval      t02tla = 'L'
110400101027     C                   IF        *IN28 = *ON
110500001120     C                   movel     'R'           t02mod
110600001120     C                   else
110700001120     C                   movel     'C'           t02mod
110800001120     C                   movel(p)  vidtip        t02ke1
110900001120     C                   end
111000991108     C                   movel     'ITF'         t02cod
111100001120     C                   movel     knsif         t02sif
111200991108     C                   CALL      'TIBS02R'
111300001120     C                   parm                    KPJBA
111400001120     C                   parm                    TIBS02DS
111500001120     C
111600001120     C*
111700001120     C                   IF        T02err = *blanks
111800991108     C                   movel     T02ke1        vidtip
111900020218     C                   movel     T02uni        DITF
112000020218     C                   movel     §ITFDESC      vddtip
112100021003     C* Sostituisco con il dato proveniente dalla tabella solo se nn già indicato
112200021003     C* qualcosa nelle regole specifiche x il cliente/tipo file.
112300021014     C                   if        vidfit = *blanks
112400021014     C                   movel     §ITFLIBD      vidfi2
112500021014     C                   endif
112600001120     C                   else
112700001120     C                   movel     '1'           errl00
112800991108     C                   movel     *blank        vddtip
112900111215     C* pulisco anche il cod. tabella associata
113000111215     C                   movel     *blank        vidtab
113100001120     C                   seton                                        66
113200991108     C                   MOVEL     msg(11)       DSMSMS
113300991108     C                   ENDIF
113400001120     C                   if        errl00 = '0'
113500120109     C     vidtip        chain     tivtf01l                           97
113600001120     C                   if        not *in97
113700001120     C                   movel     vtfpgt        vidpgtd
113800001120     C                   movel     vtfpgi        vidpgid
113900001120     C                   movel     vtffid        vidfidd
114000001120     C                   movel     vtffis        vidfisd
114100001120     C                   movel     vtffit        vidfitd
114200111214     C                   movel     vtffi1        vidtab
114300101013     C                   movel     vtfppi        DVTFPPI
114400101026     C                   if        DEPOP1 = 'F06' OR
114500101026     C                             DEPOP1 = 'O03'
114600101026     C                   select
114700101026     C                   when      vtfUD = 'U'
114800101026     C                   movel     D§VTFPPIMF    vidppiMC
114900101026     C                   when      vtfUD = 'D'
115000101026     C                   movel     D§VTFPPIMF    vidppiMF
115100101026     C                   endsl
115200101026     C                   movel     D§VTFPPIMS    vidppiMS
115300101026     C                   movel     D§VTFPPIMZ    vidppiMZ
115400101026     C                   endif
115500100308     C                   if        vidfi4d <> *blanks and vidfi4d <> '*'
115600001120     C                   eval      vidfi4d = %subst(vidfitd:1:5) + '00F'
115700100308     C                   endif
115800001120     C                   else
115900001120     C                   movel     *blank        vidpgtd
116000001120     C                   movel     *blank        vidpgid
116100001120     C                   movel     *blank        vidfidd
116200001120     C                   movel     *blank        vidfisd
116300001120     C                   movel     *blank        vidfitd
116400001120     C                   movel     *blank        vidfi4d
116500001120     C                   endif
116600001120     C                   endif
116700001120     C*
116800001120     C                   endsr
116900991027     C*------------------------------------------------------------------------*
117000120109     C* CntTip234 - Controlla esistenza altri tipi file su tivt00f
117100991027     C*------------------------------------------------------------------------*
117200120109     C     CntTip234     BEGSR
117300120119     C*
117400120119     C* se ho indicato un cliente devo indicare il tipo file
117500120119     C                   IF        Kscs(i) <>*blank and
117600120119     C                             Kscs(i) <>*zeros and
117700120119     C                             Tips(i) = *blank
117800120119     C                   SELECT
117900120119     C                   WHEN      i = 1
118000120119     C                   SETON                                        7549
118100120119     C                   WHEN      i = 2
118200120119     C                   SETON                                        7650
118300120119     C                   WHEN      i = 3
118400120119     C                   SETON                                        7751
118500120119     C                   ENDSL
118600120119     C                   MOVEL     '1'           ERRL00
118700120119     C                   MOVEL     MSG(34)       DSMSMS
118800120119     C                   LEAVESR
118900120119     C                   ENDIF
119000010913     C*
119100120119     C* il tipo file / cliente indicati non devono essere quelli in gestione
119200120119     C                   IF        Tips(i) = VIDTip and
119300120119     C                             Kscs(i) = VIDKsc
119400120111     C                   SELECT
119500120111     C                   WHEN      i = 1
119600120119     C                   SETON                                        7549
119700120111     C                   WHEN      i = 2
119800120119     C                   SETON                                        7650
119900120111     C                   WHEN      i = 3
120000120119     C                   SETON                                        7751
120100120111     C                   ENDSL
120200120111     C                   MOVEL     '1'           ERRL00
120300120111     C                   MOVEL     MSG(33)       DSMSMS
120400120111     C                   LEAVESR
120500120111     C                   ENDIF
120600120111     C*
120700120111     C* il tipo file indicato deve esistere ed essere abilitato su questa anagrafica per il cliente
120800120119     C* indicato
120900120111     C* (per abilitato basta che la data fine validità sia maggiore di oggi; se anche la data inizio
121000120111     C*  è maggiore di oggi non importa, sarà valido nel futuro)
121100120119     C                   EVAL      x_VIRKsc = Kscs(i)
121200120111     C                   EVAL      x_VIRTip = Tips(i)
121300120111     C                   EVAL      x_VIRDtF = %dec(%date() : *ISO)
121400120111     C     K03VIR06      SETLL     TIVIR06L
121500120111     C     K02VIR06      READE     TIVIR06L
121600120111     C                   if        %eof
121700120109     C                   SELECT
121800120109     C                   WHEN      i = 1
121900120109     C                   SETON                                        75
122000120109     C                   WHEN      i = 2
122100120109     C                   SETON                                        76
122200120109     C                   WHEN      i = 3
122300120109     C                   SETON                                        77
122400120109     C                   ENDSL
122500120109     C                   MOVEL     '1'           ERRL00
122600120109     C                   MOVEL     MSG(31)       DSMSMS
122700120119     C* se il tipo file / cliente per duplica esiste,
122800120119     C* questo non deve contenere nelle sue coppie per duplica
122900120119     C* il tipo file / cliente in gestione
123000120111     C                   ELSE
123100120119     C*
123200120119     C                   MOVEL     x_VIRPTH      DVIR01
123300120119     C                   IF        §VIR01Tip2 = VIDTip and
123400120119     C                             §VIR01Ksc2 = VIDKsc     or
123500120119     C                             §VIR01Tip3 = VIDTip and
123600120119     C                             §VIR01Ksc3 = VIDKsc     or
123700120119     C                             §VIR01Tip4 = VIDTip and
123800120119     C                             §VIR01Ksc4 = VIDKsc
123900120111     C                   SELECT
124000120111     C                   WHEN      i = 1
124100120119     C                   SETON                                        7549
124200120111     C                   WHEN      i = 2
124300120119     C                   SETON                                        7650
124400120111     C                   WHEN      i = 3
124500120119     C                   SETON                                        7751
124600120111     C                   ENDSL
124700120111     C                   MOVEL     '1'           ERRL00
124800120111     C                   MOVEL     MSG(32)       DSMSMS
124900120111     C                   ENDIF
125000120111     C                   ENDIF
125100120109     C*
125200120109     C                   ENDSR
125300120109     C*------------------------------------------------------------------------*
125400120109     C* cntksc - Controlla esistenza codice
125500120109     C*------------------------------------------------------------------------*
125600120109     C     cntksc        begsr
125700120109     C*
125800010913     C     vidksc        chain     tianv01l
125900010913     C                   if        %found
126000010913     C                   movel     ANVDES        vddksc
126100010913     C                   else
126200001120     C                   move      vidksc        wcli              7 0
126300991210     C                   CLEAR                   BS69DS
126400991210     C                   CLEAR                   ACODS
126500991210     C                   MOVEL     KNSIF         I69SIF
126600991210     C                   Z-ADD     WCLI          I69KAC
126700991210     C                   CALL      'TIBS69R'
126800991210     C                   PARM                    BS69DS
126900991210     C                   PARM                    ACODS
127000991210     C                   PARM                    INDDS
127100991210     C                   PARM                    CLPDS
127200991210     C                   PARM                    CLSDS
127300991210     C     O69ERR        IFNE      '1'
127400991210     C                   MOVEL     ACORAG        vddksc
127500991210     C                   ELSE
127600991210     C                   MOVEL     *ALL'*'       vddksc
127700010913     C                   MOVE      '1'           errl00
127800010913     C                   MOVEL     msg(11)       dsmsms
127900010913     C                   ENDIF
128000000727     C                   ENDIF
128100010913     C*
128200010913     C                   ENDSR
128300991027     C*------------------------------------------------------------------------*
128400040901     C* cntpoi - Controlla codice filiale
128500991027     C*------------------------------------------------------------------------*
128600001120     C     cntpoi        begsr
128700001120     C*
128800010529     C                   if        vidpoi <> '046' and
128900020809     C                             vidpoi <> '999' and
129000020902     C                             vidpoi <> '500' and
129100020902     C                             vidpoi <> '789'
129200001120     C*
129300001120     C                   move      *blank        vddpoi
129400001120     C     '?'           scan      vidpoi                                 97
129500001120     C                   if        *in97
129600001120     C                   movel     *blank        cod1
129700001120     C                   movel     *blank        des1
129800001120     C                   movel     *blank        pra1
129900001120     C                   call      'TNSD24R'
130000001120     C                   parm                    cod1              3
130100001120     C                   parm                    tip1              1
130200001120     C                   parm                    des1             25
130300001120     C                   parm                    pra1              3
130400001120     C                   movel     cod1          vidpoi
130500001120     C                   endif
130600001120     C                   move      vidpoi        cod1n             3 0
130700001120     C     cod1n         chain     azorg01l                           98
130800001120     C                   if        not *in98 and orgfag <>'V'
130900001120     C                   movel     orgdes        vddpoi
131000001120     C                   else
131100001120     C                   seton                                        69
131200001120     C                   move      '1'           errl00
131300001120     C                   movel     msg(13)       dsmsms
131400001120     C                   endif
131500001120     C*
131600001120     C                   else
131700001120     C                   movel     'SEDE BOLOGNA'vddpoi
131800001120     C                   endif
131900001120     C*
132000001120     C                   endsr
132100991210     C*------------------------------------------------------------------------*
132200991210     C* FINSR - OPERAZIONI FINALI
132300991210     C*------------------------------------------------------------------------*
132400991210     C     FINSR         BEGSR
132500991210     C*
132600991210     C* CHIUDE I FILE DEI PGM CHIAMATI
132700991210     C                   CLEAR                   bs69ds
132800991210     C                   CLEAR                   acods
132900991210     C                   CLEAR                   indds
133000991210     C                   CLEAR                   clpds
133100991210     C                   CLEAR                   clsds
133200991210     C                   MOVEL     'C'           i69tla
133300991210     C                   MOVEL     knsif         i69sif
133400991210     C                   CALL      'TIBS69R'
133500991210     C                   PARM                    bs69ds
133600991210     C                   PARM                    acods
133700991210     C                   PARM                    indds
133800991210     C                   PARM                    clpds
133900991210     C                   PARM                    clsds
134000991210     C*
134100991210     C                   ENDSR
134200991026     C*------------------------------------------------------------------------*
134300991026     C* CHKDA8 - CONTROLLA LA DATA (8)
134400991026     C*------------------------------------------------------------------------*
134500991026     C     CHKDA8        BEGSR
134600991026     C*
134700991026     C     *LIKE         DEFINE    VIDDti        WDA8
134800991026     C     *LIKE         DEFINE    VIDDti        DA8W
134900991026     C*
135000991026     C                   Z-ADD     WDA8          G08DAT
135100991026     C                   Z-ADD     *ZEROS        G08INV
135200991026     C                   MOVEL     '0'           G08ERR
135300991026     C                   CALL      'XSRDA8'
135400991026     C                   PARM                    WLBDA8
135500991026     C                   Z-ADD     G08DAT        WDA8
135600991026     C                   Z-ADD     G08INV        DA8W
135700991026IF  1C     G08ERR        IFEQ      '1'
135800991026     C                   MOVEL     '1'           ERRL00
135900991026E   1C                   ENDIF
136000991026     C*
136100991026     C                   ENDSR
136200970526     C*------------------------------------------------------------------------*
136300970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
136400970526     C*------------------------------------------------------------------------*
136500970526     C     IREVID        BEGSR
136600080604     C*
136700080604     C* REPERISCO INFO DA ESTENSIONE ANAGRAFICA CLIENTI/TIPI FILE => TRC 01
136800080604     C                   IF        §vire01INV = 'S'
136900080604     C                   EVAL      VIDEML = §vire01EML
137000080604     C                   ENDIF
137100970526     C* CAMPI
137200991026     C                   MOVEL     virtip        VIDtip
137300020218     C                   MOVE      virksc        vidksc
137400020218     C                   Z-ADD     virseq        vidseq
137500060308     C                   IF        DEPOP1 = 'O03'
137600060308     C                   MOVEL     *blanks       vidpoi
137700060308     C                   ELSE
137800060308     C                   MOVE      virpoi        vidpoi
137900060308     C                   ENDIF
138000991026     C                   MOVEL     virdes        VIDdes
138100991026     C                   MOVEL     virpgt        VIDpgt
138200991026     C                   MOVEL     virppt        VIDppt
138300991026     C                   MOVEL     virpgi        VIDpgi
138400991026     C                   MOVEL     virppi        VIDppi
138500991026     C                   MOVEL     virfid        VIDfid
138600991026     C                   MOVEL     virfis        VIDfis
138700991026     C                   MOVEL     virfit        VIDfit
138800000710     C                   MOVEL     virfi3        VIDfi3
138900000710     C                   MOVEL     virfi4        VIDfi4
139000010123     C                   MOVEL     virfi2        VIDfi2
139100030526     C                   MOVEL     virpth        DVIR01
139200030526     C                   MOVEL     §VIR01TM      VIDtmbd
139300120109     C                   MOVEL     §VIR01Tip2    VIDTip2
139400120109     C                   MOVEL     §VIR01Tip3    VIDTip3
139500120109     C                   MOVEL     §VIR01Tip4    VIDTip4
139600120119     C                   MOVEL     §VIR01Ksc2    VIDKsc2
139700120119     C                   MOVEL     §VIR01Ksc3    VIDKsc3
139800120119     C                   MOVEL     §VIR01Ksc4    VIDKsc4
139900000710  ***C*                  MOVEL     virctl        VIDctl
140000991026     C                   Z-ADD     virdti        G08INV
140100991026     C                   Z-ADD     *ZEROS        G08DAT
140200991026     C                   MOVEL     '3'           G08ERR
140300991026     C                   CALL      'XSRDA8'
140400991026     C                   PARM                    WLBDA8
140500991026     C                   Z-ADD     G08DAT        viddti
140600970527     C*
140700991026     C                   Z-ADD     virdtf        G08INV
140800991026     C                   Z-ADD     *ZEROS        G08DAT
140900991026     C                   MOVEL     '3'           G08ERR
141000991026     C                   CALL      'XSRDA8'
141100991026     C                   PARM                    WLBDA8
141200991026     C                   Z-ADD     G08DAT        viddtf
141300020218     C                   MOVEL     virfi1        vidisv
141400051122     C*
141500051122     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
141600051122     C                   IF        DEPOP1 = 'F06' OR
141700051122     C                             DEPOP1 = 'O03' OR
141800051122     C                             DEPOP1 = 'O07'
141900051122     C                   EVAL      VIDDTI = DATCORINV
142000060726     C                   EVAL      VIDDTF = 31122039
142100051122     C                   ENDIF
142200020218     C*
142300970526     C                   ENDSR
142400970526     C*------------------------------------------------------------------------*
142500970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
142600970526     C*------------------------------------------------------------------------*
142700970526     C     AZZVID        BEGSR
142800970526     C* CAMPI
142900991026     C                   MOVEL     *blank        VIDksc
143000000710  ***C*                  MOVEL     *blank        VIDpt1
143100000710  ***C*                  MOVEL     *blank        VIDpt2
143200010123     C                   z-add     0             vidseq
143300010123     C                   move      *zeros        vidpoi
143400060726     C                   EVAL      VIDDTI = DATCORINV
143500060726     C                   EVAL      VIDDTF = 31122039
143600991025     C                   MOVEL     *blank        VIDtip
143700991025     C                   MOVEL     *blank        VIDdes
143800991025     C                   MOVEL     *blank        VIDpgt
143900991025     C                   MOVEL     *blank        VIDppt
144000991025     C                   MOVEL     *blank        VIDpgi
144100991025     C                   MOVEL     *blank        VIDppi
144200991025     C                   MOVEL     *blank        VIDfid
144300991025     C                   MOVEL     *blank        VIDfis
144400991025     C                   MOVEL     *blank        VIDfit
144500000710     C                   MOVEL     *blank        VIDfi3
144600000710     C                   MOVEL     *blank        VIDfi4
144700010123     C                   MOVEL     *blank        VIDfi2
144800000710  ***C*                  MOVEL     *blank        VIDctl
144900080603     C                   MOVEL     *blank        VIDeml
145000970526     C*
145100970526     C                   ENDSR
145200970526     C*------------------------------------------------------------------------*
145300991026     C* IMPvir - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
145400970526     C*------------------------------------------------------------------------*
145500991026     C     IMPvir        BEGSR
145600970526     C*
145700991027     C                   move      VIDpoi        virpoi
145800991026     C                   movel     VIDdes        virdes
145900991026     C                   movel     VIDpgt        virpgt
146000991026     C                   movel     VIDppt        virppt
146100991026     C                   movel     VIDpgi        virpgi
146200991026     C                   movel     VIDppi        virppi
146300991026     C                   movel     VIDfid        virfid
146400991026     C                   movel     VIDfis        virfis
146500991026     C                   movel     VIDfit        virfit
146600000710     C                   movel     VIDfi3        virfi3
146700000710     C                   movel     VIDfi4        virfi4
146800010123     C                   movel     VIDfi2        virfi2
146900030526     C                   MOVEL     VIDtmbd       §VIR01TM
147000120109     C                   EVAL      §VIR01Tip2 =  VIDTip2
147100120109     C                   EVAL      §VIR01Tip3 =  VIDTip3
147200120109     C                   EVAL      §VIR01Tip4 =  VIDTip4
147300120119     C                   IF        VIDKsc2 = *zeros
147400120119     C                   EVAL      VIDKsc2 = *blank
147500120119     C                   ENDIF
147600120119     C                   IF        VIDKsc3 = *zeros
147700120119     C                   EVAL      VIDKsc3 = *blank
147800120119     C                   ENDIF
147900120119     C                   IF        VIDKsc4 = *zeros
148000120119     C                   EVAL      VIDKsc4 = *blank
148100120119     C                   ENDIF
148200120119     C                   EVAL      §VIR01Ksc2 =  VIDKsc2
148300120119     C                   EVAL      §VIR01Ksc3 =  VIDKsc3
148400120119     C                   EVAL      §VIR01Ksc4 =  VIDKsc4
148500030526     C                   MOVEL     DVIR01        virpth
148600000710  ***C*                  movel     VIDctl        virctl
148700991026     C                   Z-ADD     viddtf        G08dat
148800991026     C                   Z-ADD     *ZEROS        G08inv
148900991026     C                   MOVEL     '0'           G08ERR
149000991026     C                   CALL      'XSRDA8'
149100991026     C                   PARM                    WLBDA8
149200991026     C                   Z-ADD     G08inv        virdtf
149300000628     C                   movel     VIDisv        virfi1
149400970526     C*
149500970526     C                   ENDSR
149600970526     C*------------------------------------------------------------------------*
149700970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
149800970526     C*------------------------------------------------------------------------*
149900970526     C     IMPFUN        BEGSR
150000101013     C*
150100101026     C                   SETOFF                                       44
150200101026     C                   SETOFF                                       30
150300101026     C                   SETOFF                                       31
150400101026     C                   SETOFF                                       32
150500101026     C                   SETOFF                                       33
150600970526     C*
150700970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
150800970526IF  1C     DEPOP1        IFEQ      'O05'
150900970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
151000970526     C                   MOVEL     'N'           FUDATT
151100970526     C                   MOVEL     FUDRIG        FUD(6)
151200970526E   1C                   ENDIF
151300970526     C*
151400970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
151500970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
151600970526     C                   EXSR      FUNGES
151700970526     C*
151800970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
151900970526     C*---
152000970526     C* 90 -> PROTEZIONE CHIAVE
152100970526     C* 91 -> PROTEZIONE TOTALE
152200970526     C*---
152300970528IF  1C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
152400970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
152500970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
152600970526     C                   SETOFF                                       91
152700970526E   2C                   ENDIF
152800101013IF  2C     DEPOP1        IFEQ      'O03'                                        *INSERIMENTO
152900101013E   2C                   ENDIF
153000970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
153100991014IF  2C     DEPOP1        oreq      'O01'                                        *MODIFICA
153200970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
153300970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
153400970526E   2C                   ENDIF
153500970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
153600970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
153700970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
153800970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
153900970526     C                   SETON                                        91
154000970526E   2C                   ENDIF
154100970526X   1C                   ELSE                                                   *I N T E R R O G A
154200970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
154300970526     C                   SETON                                        91
154400970526E   1C                   ENDIF
154500970526     C*
154600970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
154700970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
154800991025     C                   MOVEL     S20tip        KCEtip
154900991026     C                   MOVEL     S20ksc        KCEksc
155000000420     C     KEYvi2        CHAIN     tivir03L                           9998
155100080603IF  2C     *IN99         IFEQ      *ON
155200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
155300970526     C                   MOVEL     MSG(6)        DSMSMS
155400970526X   2C                   ELSE
155500080603     C*
155600080603     C* Aggancio anche il record d estensione regole clienti/tipi-file => TRC 01
155700080603     C                   EVAL      KCEdti  = virDTI
155800080603     C                   EVAL      vireTRC = '01'
155900080603     C     KEYvire1_C    CHAIN     tivire1l                           9998
156000080603     C                   IF        %found(tivire1l)
156100080603     C                   EVAL      DVIRE01 = vireFLO
156200080603     C                   ENDIF
156300080603     C*
156400080603     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
156500980128IF  3C     DEPOP1        IFEQ      'O05'
156600991014IF  3C     DEPOP1        oreq      'O02'
156700101026     C*
156800101026     C* Se trattasi d tipo servizio "idoneo" reperisco anche i dati utente lato "BARTVAS"
156900101026     C     virFI1        lookup    isvftpeml                              80
157000101026     C                   IF        *IN80
157100111215     C                             and AS400_ok = *on
157200101026      /FREE
157300101026
157400101026         if UBBVSQL01_RtvCliTipFile(
157500101026                         pInConn
157600101026                       : virKSC
157700101026                       : virTIP
157800101026                       : pOutNumRec
157900101026                       : pOutCliTipTras
158000101026                       : pOutCliMskChk
158100101026                       : pOutCliMskFil
158200101026                       : pOutCliMskSem
158300101026                       : pOutCliPerc
158400101026                       : pOutCliEml1
158500101026                       : pOutCliEml2
158600101026                       : pOutCliEml3
158700101026                       : pOutCliEml4
158800101026                       : pOutCliEml5
158900101026                       : pOutCliSogg
159000101026                       : pOutCliTesto
159100101026                       : pOutCliFtpAddr
159200101026                       : pOutCliFtpDir
159300101026                       : pOutCliFtpUsr
159400101026                       : pOutCliFtpPwd
159500101026                       : pOutCliFtpFAS4
159600101026                       : pOutCliPercOld
159700101026                       : pOutCliTentat
159800101026                       : pOutCliRitardo
159900101026                       : pOutCliLibAS4
160000101026                       : pOutCliFilAS4
160100101026                       : pOutCliMbrAS4
160200101026                       : pOutCliProc
160300101026                       : pOutCliSched
160400101026                       : pOutCliAccorp
160500101026                       : pOutCliMskZip
160600101026                       : pOutCliDataStr
160700101026                       : pOutCliDataEnd
160800101026                       : pOutCliCodComp
160900101026                       : pOutCliFtpCmd1
161000101026                       : pOutCliEmlErr
161100101026                       : pOutCliHtUrlTo
161200101026                       : pOutCliHtUsrTo
161300101026                       : pOutCliHtPwdTo
161400101026                       : pOutCliHtConTy
161500101026                       : pOutCliHtSndEx
161600101026                       : pOutCliHtSndPr
161700101026                       : pOutCliHtUsPxy
161800101026                       : pOutCliUsrIns
161900101026                       : pOutCliUsrUpd
162000101026                       : pOutCliDatUpd
162100101026                       : pOutCliNote
162200101026                       : pOutCliFtpPrt
162300101026                       : pOutCliFtpTy) = 0;
162400101026              if pOutNumRec = 1;
162500101026                 VIDPPIMC = pOutCliMskChk;
162600101026                 if %len(%trim(pOutCliMskChk)) > %size(VIDPPIMC);
162700101026                    *in30 = *on;
162800101026                 endif;
162900101026                 VIDPPIMF = pOutCliMskFil;
163000101026                 if %len(%trim(pOutCliMskFil)) > %size(VIDPPIMF);
163100101026                    *in31 = *on;
163200101026                 endif;
163300101026                 VIDPPIMS = pOutCliMskSem;
163400101026                 if %len(%trim(pOutCliMskSem)) > %size(VIDPPIMS);
163500101026                    *in32 = *on;
163600101026                 endif;
163700101026                 VIDPPIMZ = pOutCliMskZip;
163800101026                 if %len(%trim(pOutCliMskZip)) > %size(VIDPPIMZ);
163900101026                    *in33 = *on;
164000101026                 endif;
164100101026              endif;
164200101026         endif;
164300101026
164400101026      /END-FREE
164500101026     C
164600101026     C                   ELSE
164700101026     C                   SETON                                        44
164800101026     C                   ENDIF
164900101026     C*
165000980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
165100980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
165200980128E   3C                   ENDIF
165300970526E   2C                   ENDIF
165400970526X   1C                   ELSE                                                   *INSERIMENTO
165500970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
165600970526E   1C                   ENDIF
165700970526     C*
165800970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
165900970526IF  1C     DEPOP1        IFEQ      'O02'
166000970526     C                   MOVEL     DEC(1)        VIDFUN
166100970526E   1C                   ENDIF
166200991014IF  1C     DEPOP1        IFEQ      'O03'
166300991014     C                   MOVEL     DEC(6)        VIDFUN
166400991014E   1C                   ENDIF
166500970526IF  1C     DEPOP1        IFEQ      'O04'
166600970526     C                   MOVEL     DEC(2)        VIDFUN
166700970526E   1C                   ENDIF
166800970526IF  1C     DEPOP1        IFEQ      'O05'
166900970526     C                   MOVEL     DEC(3)        VIDFUN
167000970526E   1C                   ENDIF
167100970526IF  1C     DEPOP1        IFEQ      'O07'
167200970526     C                   MOVEL     DEC(4)        VIDFUN
167300970526E   1C                   ENDIF
167400970526IF  1C     DEPOP1        IFEQ      'F06'
167500970526     C                   MOVEL     DEC(5)        VIDFUN
167600970526E   1C                   ENDIF
167700970526     C*
167800970526     C                   ENDSR
167900101026
168000101026
168100101026
168200101026     C*--------------------------------------------------------------------------------------------*
168300101026     C* CARTAB - carica le tabelle occorrenti
168400101026     C*--------------------------------------------------------------------------------------------*
168500101026     C     CARTAB        BEGSR
168600101026     C*
168700101026     C* Tipi servizi soggetti a controllo unificazioni
168800101026     C                   z-add     *zeros        ww                3 0
168900101026     C                   eval      tbecod = 'ISV'
169000101026     C     tbecod        setll     tntbe01l
169100101026     C     tbecod        reade     tntbe01l
169200101026     C                   dow       not %eof(tntbe01l)
169300101026     C                   movel     tbeuni        disv
169400101026     C                   if        §isvproc = 'F'
169500101026     C                   add       1             ww
169600101026     C                   eval      isvftpeml(ww) = tbeke1
169700101026     C                   endif
169800101026     C     tbecod        reade     tntbe01l
169900101026     C                   enddo
170000101026     C*
170100101026     C                   ENDSR
170200101026
170300101026     c*------------------------------------------------------------------------*
170400101026     c* opnCONN - Apertura connessione
170500101026     c*------------------------------------------------------------------------*
170600101026     c     OPNCONN       BEGSR
170700101026      /FREE
170800101026
170900101026        if not *in40;
171000101026           if UBBVSQL01_Connect('BARTVAS' : pOutConn) = 0;
171100101026              pInConn = pOutConn;
171200101026           else;
171300101026              *in40 = *on;
171400101026              DSMSMS = MSG(28);
171500101026           endif;
171600101026        endif;
171700101026
171800101026      /END-FREE
171900101026      *
172000101026     c                   ENDSR
172100101026     c*------------------------------------------------------------------------*
172200101026
172300101026
172400101026
172500101026     c*------------------------------------------------------------------------*
172600101026     c* clsCONN - Chiusura connessione
172700101026     c*------------------------------------------------------------------------*
172800101026     c     CLSCONN       BEGSR
172900101026      /FREE
173000101026
173100101026           if UBBVSQL01_Disconnect(pInConn) = 0;
173200101026           else;
173300101026              *in40 = *on;
173400101026           endif;
173500101026
173600101026      /END-FREE
173700101026      *
173800101026     c                   ENDSR
173900101026     c*------------------------------------------------------------------------*
174000101026
174100101026
174200101026
174300970526     C*------------------------------------------------------------------------*
174400970526     C* *INZSR - OPERAZIONI INIZIALI
174500970526     C*------------------------------------------------------------------------*
174600970526     C     *INZSR        BEGSR
174700970526     C*---
174800970526     C* RICEVIMENTO PARAMETRI
174900970526     C*---
175000970526     C     *ENTRY        PLIST
175100970612     C                   PARM                    KPJBA
175200991028     C                   movel     kpjbu         tis720
175300000420     C*
175400000420     C* Inverto subito la data presa in input
175500000420     C                   Z-ADD     s20dti        wda8
175600000420     C                   Z-ADD     *ZEROS        da8w
175700000420     C                   EXSR      CHKDA8
175800000420     C                   Z-ADD     DA8W          KCEdti
175900970526     C*
176000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
176100970526     C     XMSGPA        PLIST
176200970526     C                   PARM                    DSMSMS
176300970526     C                   PARM                    DSMSPG
176400970526     C                   PARM                    DSMSKE
176500970526     C                   PARM                    DSMSER
176600991108     C*---
176700970526     C*
176800970526     C* IMPOSTA LA DS -TASTI PREMUTI-
176900970526     C                   CALL      'XKEY'
177000970526     C                   PARM                    DSKEY
177100101026     C*
177200101026     C* CARICA LE TABELLE OCCORRENTI
177300101027     C                   EXSR      CARTAB
177400111215     C*
177500111215     C* Reperisco il sistema AS/400 corrente
177600111215     C                   EVAL      AS400_ok = *off
177700111215     C                   callp     UBRTVNETA_Rtv(currSysNeta)
177800111215     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
177900111215     C                   EVAL      AS400_ok = *on
178000111215     C                   ENDIF
178100101026     C*
178200101026     C* Apro la connessione al DB "BARTVAS"
178300111215     C                   IF        AS400_ok = *on
178400111215     C                   exsr      opnCONN
178500111215     C                   ENDIF
178600970526     C*---
178700970526     C* VARIABILI NON RIFERITE AL DATA BASE
178800970526     C*---
178900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
179000991025     C     *LIKE         DEFINE    S20OP0        DEPOP0
179100991025     C     *LIKE         DEFINE    S20OP1        DEPOP1
179200991025     C     *LIKE         DEFINE    S20OP0        WOP0
179300991025     C     *LIKE         DEFINE    S20OP1        WOP1
179400970526     C*---
179500970526     C* VARIABILI DI CONTROLLO
179600970526     C*---
179700970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
179800970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
179900970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
180000970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
180100970526     C*
180200970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
180300991026     C                   MOVEL     'TIS723R'     DSMSPG
180400991026     C                   MOVEL     'TIS723R'     MSGQUE
180500970526     C                   MOVEL     *BLANKS       MSGKEY
180600970526     C                   MOVEL     *BLANKS       DSMSMS
180700970526     C*---
180800970526     C* VARIABILI RIFERITE AL DATA BASE
180900970526     C*---
181000991026     C     *LIKE         DEFINE    virtip        KCEtip
181100991026     C     *LIKE         DEFINE    virksc        KCEksc
181200000420     C     *LIKE         DEFINE    virdti        KCEdti
181300970526     C*---
181400970526     C* CHIAVI DI LETTURA
181500970526     C*---
181600000420     C* LETTURA tivir03L - INCOMPLETA
181700991026     C     KEYvir        KLIST
181800080603     C                   KFLD                    KCEtip
181900080603     C                   KFLD                    KCEksc
182000080603     C* LETTURA tivir03L - COMPLETA
182100080603     C     KEYvi2        KLIST
182200080603     C                   KFLD                    KCEtip
182300080603     C                   KFLD                    KCEksc
182400080603     C                   KFLD                    KCEdti
182500080603     C* LETTURA tivire1L - COMPLETA
182600080603     C     KEYvire1_C    KLIST
182700080603     C                   KFLD                    KCEksc
182800080603     C                   KFLD                    KCEtip
182900080603     C                   KFLD                    KCEdti
183000080603     C                   KFLD                    vireTRC
183100080603     C* LETTURA tntbe01L - PARZIALE
183200040816     C     KEYtbe1_P     KLIST
183300040816     C                   KFLD                    TBEcod                         *CODICE TABELLA
183400120111     C* LETTURA tivir06L - COMPLETA
183500120111     C     K03VIR06      KLIST
183600120111     C                   KFLD                    x_VIRKsc
183700120111     C                   KFLD                    x_VIRTip
183800120111     C                   KFLD                    x_VIRDtF
183900120111     C* LETTURA tivir06L - PARZIALE
184000120111     C     K02VIR06      KLIST
184100120111     C                   KFLD                    x_VIRKsc
184200120111     C                   KFLD                    x_VIRTip
184300970528     C*
184400970528     C* CALCOLA LA DATA CORRENTE
184500970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
184600970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
184700970528     C                   Z-ADD     WN8           G08DAT
184800051122     C                   Z-ADD     WN8           DATCORINV         8 0
184900970528     C                   Z-ADD     *ZEROS        G08INV
185000970528     C                   MOVEL     '0'           G08ERR
185100970528     C                   CALL      'XSRDA8'
185200970528     C                   PARM                    WLBDA8
185300970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
185400970526     C*
185500970526     C* DEPOSITA LE OPZIONI DI ENTRATA
185600991025     C                   MOVEL     S20OP0        DEPOP0                         *LIVELLO PROCEDURA
185700991025     C                   MOVEL     S20OP1        DEPOP1                         *LIVELLO PROGRAMMA
185800051122     C*
185900051122     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
186000051122     C                   IF        DEPOP1 = 'F06' OR
186100051122     C                             DEPOP1 = 'O03' OR
186200051122     C                             DEPOP1 = 'O07'
186300051122     C                   EVAL      VIDDTI = DATCORINV
186400060726     C                   EVAL      VIDDTF = 31122039
186500051122     C                   ENDIF
186600000629     C*
186700000629     C* EFFETTUO CONSIDERAZIONI SULLE OPZIONI DI ENTRATA
186800000629     C                   IF        DEPOP1 = 'F06' OR
186900000629     C                             DEPOP1 = 'O03'
187000000629     C                   SETOFF                                       92
187100000629     C                   ELSE
187200000629     C                   SETON                                        92
187300000629     C                   ENDIF
187400970526     C*
187500970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
187600970526     C                   EXSR      IMPFUN
187700970526     C*
187800970526     C                   ENDSR
187900970526     C*------------------------------------------------------------------------*
188000970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
188100970526     C*------------------------------------------------------------------------*
188200970526     C     FUNGES        BEGSR
188300970526     C*
188400970526     C* PULISCO CAMPI DI LAVORO
188500970526     C                   Z-ADD     0             W                 5 0
188600970526     C*
188700970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
188800970526     C* E CARICO TABELLA FUNZIONI ATTIVE
188900970526     C     1             DO        24            I
189000970526     C                   MOVEL     FUD(I)        FUDRIG
189100970526     C*
189200970526     C* PULIZIA FUNZIONI PRECEDENTI
189300970526     C                   MOVEL     *BLANKS       FUV(I)
189400970526     C*
189500970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
189600970526     C* (SALTA F24)
189700970526     C     FUDATT        IFEQ      'S'
189800970526     C     FUDFUN        ANDNE     '24'
189900970526     C                   ADD       1             W
190000970526     C                   MOVEL     FUDDES        FUV(W)
190100970526     C                   END
190200970526     C                   END                                                    DO WHILE
190300970526     C*
190400970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
190500970526     C* ALTRIMENTI DISATTIVO F24
190600970526     C     1             DO        24            I
190700970526     C                   MOVEL     FUD(I)        FUDRIG
190800970526     C*
190900970526     C     FUDFUN        IFEQ      '24'
191000970526     C     W             IFGT      6
191100970526     C                   MOVEL     'S'           FUDATT
191200970526     C                   ELSE
191300970526     C                   MOVEL     *BLANKS       FUDATT
191400970526     C                   END                                                    W GT 8
191500970526     C                   MOVEL     FUDRIG        FUD(I)
191600970526     C                   END
191700970526     C                   END                                                    DO WHILE
191800970526     C*
191900970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
192000970526     C     W             DIV       6             I
192100970526     C                   MVR                     Z
192200970526     C*
192300970526     C     Z             IFNE      0
192400970526     C     I             ADD       1             Z
192500970526     C                   ELSE
192600970526     C                   Z-ADD     I             Z                 5 0
192700970526     C                   END
192800970526     C*
192900970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
193000970526     C                   ADD       1             FUNGRU
193100970526     C     FUNGRU        IFGT      Z
193200970526     C     FUNGRU        ORLT      1
193300970526     C                   Z-ADD     1             FUNGRU
193400970526     C                   END
193500970526     C*
193600970526     C* PRIMO GRUPPO DI 6 FUNZIONI
193700970526     C     FUNGRU        IFEQ      1
193800970526     C                   MOVEL     FUV(1)        FUN001
193900970526     C                   MOVEL     FUV(2)        FUN002
194000970526     C                   MOVEL     FUV(3)        FUN003
194100970526     C                   MOVEL     FUV(4)        FUN004
194200970526     C                   MOVEL     FUV(5)        FUN005
194300970526     C                   MOVEL     FUV(6)        FUN006
194400970526     C                   END
194500970526     C*
194600970526     C* SECONDO GRUPPO DI 6 FUNZIONI
194700970526     C     FUNGRU        IFEQ      2
194800970526     C                   MOVEL     FUV(7)        FUN001
194900970526     C                   MOVEL     FUV(8)        FUN002
195000970526     C                   MOVEL     FUV(9)        FUN003
195100970526     C                   MOVEL     FUV(10)       FUN004
195200970526     C                   MOVEL     FUV(11)       FUN005
195300970526     C                   MOVEL     FUV(12)       FUN006
195400970526     C                   END
195500970526     C*
195600970526     C* TERZO GRUPPO DI 6 FUNZIONI
195700970526     C     FUNGRU        IFEQ      3
195800970526     C                   MOVEL     FUV(13)       FUN001
195900970526     C                   MOVEL     FUV(14)       FUN002
196000970526     C                   MOVEL     FUV(15)       FUN003
196100970526     C                   MOVEL     FUV(16)       FUN004
196200970526     C                   MOVEL     FUV(17)       FUN005
196300970526     C                   MOVEL     FUV(18)       FUN006
196400970526     C                   END
196500970526     C*
196600970526     C* QUARTO GRUPPO DI 6 FUNZIONI
196700970526     C     FUNGRU        IFEQ      4
196800970526     C                   MOVEL     FUV(19)       FUN001
196900970526     C                   MOVEL     FUV(20)       FUN002
197000970526     C                   MOVEL     FUV(21)       FUN003
197100970526     C                   MOVEL     FUV(22)       FUN004
197200970526     C                   MOVEL     *BLANKS       FUN005
197300970526     C                   MOVEL     *BLANKS       FUN006
197400970526     C                   END
197500970526     C*
197600970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
197700970526     C                   MOVEL     *BLANKS       FUN007
197800970526     C                   MOVEL     *BLANKS       FUN008
197900970526     C*
198000970526     C     1             DO        24            I
198100970526     C                   MOVEL     FUD(I)        FUDRIG
198200970526     C     FUDFUN        IFEQ      '24'
198300970526     C     FUDATT        ANDEQ     'S'
198400970526     C                   MOVEL     FUDDES        FUN008
198500970526     C                   END
198600970526     C                   END                                                    DO
198700970526     C*
198800970526     C                   ENDSR
198900970526     C*------------------------------------------------------------------------*
199000970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
199100970526     C*------------------------------------------------------------------------*
199200970526     C     FUNCON        BEGSR
199300970526     C*
199400970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
199500970526     C                   EXSR      FUNVAR
199600970526     C*
199700970526     C* PULISCO CAMPI DI LAVORO
199800970526     C                   Z-ADD     0             I                 5 0
199900970526     C                   Z-ADD     0             W                 5 0
200000970526     C*
200100970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
200200970526     C     FUNKEY        IFNE      *BLANKS
200300970526     C     1             DO        24            I
200400970526     C                   MOVEL     FUD(I)        FUDRIG
200500970526     C*
200600970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
200700970526     C     FUNKEY        IFEQ      FUDFUN
200800970526     C     FUDATT        ANDEQ     'S'
200900970526     C                   Z-ADD     I             W
201000970526     C                   END
201100970526     C                   END                                                    DO WHILE
201200970526     C*
201300970526     C* FUNZIONE NON ATTIVA (ERRORE)
201400970526     C     W             IFEQ      0
201500970526     C*
201600970526     C* EMISSIONE MESSAGGIO DI ERRORE
201700970526     C                   MOVEL     MSG(1)        DSMSMS
201800970526     C*
201900970526     C                   END                                                    W=0
202000970526     C                   END                                                    FUNKEY NE *BLANKS
202100970526     C*
202200970526     C* FUNZIONE ATTIVA (PROCEDI ...)
202300970526     C     W             IFNE      0
202400970526     C     FUNKEY        OREQ      *BLANKS
202500970526     C     FUNKEY        IFEQ      *BLANKS
202600970526     C                   EXSR      ENTGES
202700970526     C                   END
202800031229     C     FUNKEY        IFEQ      '01'
202900031229     C                   EXSR      F01GES
203000031229     C                   END
203100970526     C     FUNKEY        IFEQ      '03'
203200970526     C                   EXSR      F03GES
203300970526     C                   END
203400970526     C     FUNKEY        IFEQ      '06'
203500970526     C                   EXSR      F06GES
203600970526     C                   END
203700970526     C     FUNKEY        IFEQ      '12'
203800970526     C                   EXSR      F12GES
203900970526     C                   END
204000970526     C     FUNKEY        IFEQ      '24'
204100970526     C                   EXSR      F24GES
204200970526     C                   END
204300970526     C                   END
204400970526     C*
204500970526     C                   ENDSR
204600970526     C*------------------------------------------------------------------------*
204700970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
204800970526     C*------------------------------------------------------------------------*
204900970526     C     FUNVAR        BEGSR
205000970526     C*
205100970526     C* INIZIALIZZA VARIABILE -FUNKEY-
205200970526     C*
205300970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
205400970526     C     KEY           IFEQ      ENTER
205500970526     C                   MOVEL     *BLANKS       FUNKEY
205600970526     C                   END
205700970526     C     KEY           IFEQ      F1
205800970526     C                   MOVEL     '01'          FUNKEY
205900970526     C                   END
206000970526     C     KEY           IFEQ      F2
206100970526     C                   MOVEL     '02'          FUNKEY
206200970526     C                   END
206300970526     C     KEY           IFEQ      F3
206400970526     C                   MOVEL     '03'          FUNKEY
206500970526     C                   END
206600970526     C     KEY           IFEQ      F4
206700970526     C                   MOVEL     '04'          FUNKEY
206800970526     C                   END
206900970526     C     KEY           IFEQ      F5
207000970526     C                   MOVEL     '05'          FUNKEY
207100970526     C                   END
207200970526     C     KEY           IFEQ      F6
207300970526     C                   MOVEL     '06'          FUNKEY
207400970526     C                   END
207500970526     C     KEY           IFEQ      F7
207600970526     C                   MOVEL     '07'          FUNKEY
207700970526     C                   END
207800970526     C     KEY           IFEQ      F8
207900970526     C                   MOVEL     '08'          FUNKEY
208000970526     C                   END
208100970526     C     KEY           IFEQ      F9
208200970526     C                   MOVEL     '09'          FUNKEY
208300970526     C                   END
208400970526     C     KEY           IFEQ      F10
208500970526     C                   MOVEL     '10'          FUNKEY
208600970526     C                   END
208700970526     C     KEY           IFEQ      F11
208800970526     C                   MOVEL     '11'          FUNKEY
208900970526     C                   END
209000970526     C     KEY           IFEQ      F12
209100970526     C                   MOVEL     '12'          FUNKEY
209200970526     C                   END
209300970526     C     KEY           IFEQ      F13
209400970526     C                   MOVEL     '13'          FUNKEY
209500970526     C                   END
209600970526     C     KEY           IFEQ      F14
209700970526     C                   MOVEL     '14'          FUNKEY
209800970526     C                   END
209900970526     C     KEY           IFEQ      F15
210000970526     C                   MOVEL     '15'          FUNKEY
210100970526     C                   END
210200970526     C     KEY           IFEQ      F16
210300970526     C                   MOVEL     '16'          FUNKEY
210400970526     C                   END
210500970526     C     KEY           IFEQ      F17
210600970526     C                   MOVEL     '17'          FUNKEY
210700970526     C                   END
210800970526     C     KEY           IFEQ      F18
210900970526     C                   MOVEL     '18'          FUNKEY
211000970526     C                   END
211100970526     C     KEY           IFEQ      F19
211200970526     C                   MOVEL     '19'          FUNKEY
211300970526     C                   END
211400970526     C     KEY           IFEQ      F20
211500970526     C                   MOVEL     '20'          FUNKEY
211600970526     C                   END
211700970526     C     KEY           IFEQ      F21
211800970526     C                   MOVEL     '21'          FUNKEY
211900970526     C                   END
212000970526     C     KEY           IFEQ      F22
212100970526     C                   MOVEL     '22'          FUNKEY
212200970526     C                   END
212300970526     C     KEY           IFEQ      F23
212400970526     C                   MOVEL     '23'          FUNKEY
212500970526     C                   END
212600970526     C     KEY           IFEQ      F24
212700970526     C                   MOVEL     '24'          FUNKEY
212800970526     C                   END
212900970526     C     KEY           IFEQ      ROLLUP
213000970526     C                   MOVEL     'RU'          FUNKEY
213100970526     C                   END
213200970526     C     KEY           IFEQ      HELP
213300970526     C                   MOVEL     'HP'          FUNKEY
213400970526     C                   END
213500970526     C                   ENDSR
213600970526     C*------------------------------------------------------------------------*
213700970526     C* NOTE COSTRUZIONE SCHIERE
213800970526     C*------------------------------------------------------------------------*
213900970526     C*
214000970526     C* -MSG-MESSAGGI
214100970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
214200970526     C* (NON TOCCARE MSG,1 E MSG,2)
214300970526     C*
214400970526     C* -CMD-COMANDI
214500970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
214600970526     C*
214700970526     C* -OPD-OPZIONI
214800970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
214900970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
215000970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
215100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
215200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
215300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
215400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
215500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
215600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
215700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
215800970526     C*
215900970526     C* -FUD-FUNZIONI
216000970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
216100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
216200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
216300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
216400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
216500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
216600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
216700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
216800970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
216900970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
217000970526     O*------------------------------------------------------------------------*
217100970526** DEC - DECODIFICHE
217200970526    MODIFICA                                                      1
217300970526  ANNULLAMENTO                                                    2
217400970526VISUALIZZAZIONE                                                   3
217500970526   RIPRISTINO                                                     4
217600970526  INSERIMENTO                                                     5
217700991014     COPIA                                                        6
217800970526** MSG - MESSAGGI
217900991026TIS723R- Tasto funzionale non gestito                                          1
218000991026TIS723R- Inserimento effettuato                                                2
218100991026TIS723R- Modifica effettuata                                                   3
218200991026TIS723R- Annullamento effettuato                                               4
218300991026TIS723R- Ripristino effettuato                                                 5
218400991026TIS723R- Richiesta funzione per record inesistente                             6
218500991026TIS723R- Record momentaneamente allocato: riprovare più tardi                  7
218600991026TIS723R- Richiesta funzione per record esistente                               8
218700991026TIS723R- Questo codice cliente è già presente con questo tipo file             9
218800991027TIS723R- Il codice tipo file non è fra quelli presenti in anagrafico           10
218900991027TIS723R- Il codice cliente non è presente come padre nell'archivio UNIFICANTI  11
219000000724TIS723R- Campo obbligatorio                                                    12
219100991027TIS723R- Codice filiale inesistente o di categoria V=varia                     13
219200000724TIS723R- Data errata                                                           14
219300000724TIS723R- La data fine deve essere maggiore della data inizio                   15
219400000724TIS723R- Il record non può scadere in un giorno già trascorso                  16
219500000724TIS723R- Il record immesso si sovrappone ad uno già in decorrenza              17
219600000724TIS723R- Codice tipo servizio non presente in tabella                          18
219700001120TIS723R- Oggetto inesistente, verificare e correggere.                         19
219800001120TIS723R- File inesistente, verificare e correggere.                            20
219900001120TIS723R- Libreria inesistente, verificare e correggere.                        21
220000030603TIS723R- Tipo membro non impostabile x il tipo servizio indicato.              22
220100040107TIS723R- Per il traduttore indicato non esiste descrizione dettaglio parametri 23
220200040319TIS723R- Tipo file e servizio incompatibili, correggere.                       24
220300040816TIS723R- Tipo file e/o programma traduttore e/o file tradotto incompatibili.   25
220400051026TIS723R- ATTENZIONE presenti log a stato 0 impossibile cambiare traduttore!    26
220500060616TIS723R- Valori incompatibili, correggere!                                     27
220600101026TIS723R- Errore BartVAS: fatal error                                           28
220700101027TIS723R- ATTENZIONE: modifica effettuata lato AS/400 ma non lato BARTVAS       29
220800120109TIS723R- Tipo file per Internet non impostabile x il tipo servizio indicato.   30
220900120119TIS723R- Tipo file non abilitato per cliente indicato                          31
221000120119TIS723R- Loop tra il tipo file / cliente corrente e quello per duplica         32
221100120119TIS723R- Tipo file / cliente per duplica non possono essere quelli correnti    33
221200120119TIS723R- Valorizzato il cliente per duplica ma non il rispettivo tipo file     34
221300970526** FUD - FUNZIONI
221400031229F01=Parametri      -        (S/N)=S  <== S-FISSO           funz.01¬
221500970526                   -        (S/N)=N                        funz.02¬
221600970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
221700970526                   -        (S/N)=N                        funz.04¬
221800970526                   -        (S/N)=N                        funz.05¬
221900970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
222000970527                   -        (S/N)=N                        funz.07¬
222100970527                   -        (S/N)=N                        funz.08¬
222200970527                   -        (S/N)=N                        funz.09¬
222300970527                   -        (S/N)=N                        funz.10¬
222400970526                   -        (S/N)=N                        funz.11¬
222500970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
222600970526                   -        (S/N)=N                        funz.13¬
222700970526                   -        (S/N)=N                        funz.14¬
222800970526                   -        (S/N)=N                        funz.15¬
222900970526                   -        (S/N)=N                        funz.16¬
223000970526                   -        (S/N)=N                        funz.17¬
223100970526                   -        (S/N)=N                        funz.18¬
223200970526                   -        (S/N)=N                        funz.19¬
223300970526                   -        (S/N)=N                        funz.20¬
223400970526                   -        (S/N)=N                        funz.21¬
223500970526                   -        (S/N)=N                        funz.22¬
223600970526                   -        (S/N)=N                        funz.23¬
223700970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
