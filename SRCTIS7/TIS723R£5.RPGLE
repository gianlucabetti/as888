000100120104     H*------------------------------------------------------------------------*
000200120104     H* Gestione tipi file internet
000300970526     H*------------------------------------------------------------------------*
000400120120     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR':'TIS723BND') ACTGRP('BARTVAS')
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
014100120120     D/COPY GAITRASRC/SRCPROTOPR,TIS723R2U
014200970526     C*------------------------------------------------------------------------*
014300970526     C* MAIN LINES
014400970526     C*------------------------------------------------------------------------*
014500970526     C*
014600970526     C* NON CI SONO ERRORI IN ENTRATA
014700970526IF  1C     ERRL00        IFEQ      '0'
014800970526     C*
014900970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
015000970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
015100970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
015200970526     C     FINL00        OREQ      '1'                                          *FINE PGM
015300970526     C*
015400970526     C* EMETTE FORMATO VIDEO
015500970526     C                   EXSR      BUTFMR
015600970526     C*
015700970526     C* GESTIONE TASTI FUNZIONALI
015800970526     C                   EXSR      FUNCON
015900970526E   2C                   ENDDO
016000970526     C*
016100970526     C* CI SONO ERRORI IN ENTRATA
016200970527X   1C                   ELSE
016300991025     C                   MOVEL     '1'           S20err
016400991025     C                   MOVEL     DSMSMS        S20msg
016500970526E   1C                   ENDIF
016600970526     C* FINE PGM
016700991210     c                   exsr      finsr
016800101026     C*
016900101026     C* Chiudo la connessione
017000111215     C                   IF        AS400_ok = *on
017100111215     C                   exsr      clsCONN
017200111215     C                   ENDIF
017300101026     C*
017400970526     C                   SETON                                        LR
017500970526     C*------------------------------------------------------------------------*
017600970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
017700970526     C*------------------------------------------------------------------------*
017800970526     C     BUTFMR        BEGSR
017900970526     C*
018000970526     C* EMETTE LA TESTATA
018100991026     C                   WRITE     s723D01
018200970526     C*
018300970526     C* SPEDISCE EVENTUALE MESSAGGIO
018400970526     C                   EXSR      SNDMSG
018500970526     C*
018600970526     C* EMETTE LE FUNZIONI
018700991026     C                   WRITE     s723D02
018800970526     C*
018900970526     C* EMETTE IL VIDEO
019000991026     C                   EXFMT     s723D03
019100970526     C*
019200970526     C* REIMPOSTA FLAG DI ERRORE
019300970526     C                   MOVEL     '0'           ERRL00
019400970526     C*
019500970526     C* REIMPOSTA INDICATORI DI ERRORE
019600120119     C                   SETOFF                                       495051
019700120119     C                   SETOFF                                       525354
019800120119     C                   SETOFF                                       555657
019900970526     C                   SETOFF                                         5859
020000970526     C                   SETOFF                                       606162
020100970526     C                   SETOFF                                       636465
020200970526     C                   SETOFF                                       666768
020300970526     C                   SETOFF                                       697071
020400970526     C                   SETOFF                                       727374
020500120109     C                   SETOFF                                       757677
020600970526     C                   SETOFF                                       787980
020700970526     C                   SETOFF                                       818283
020800970526     C                   SETOFF                                       848586
020900970526     C                   SETOFF                                       878889
021000970526     C*
021100970526     C                   ENDSR
021200970526     C*------------------------------------------------------------------------*
021300970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
021400970526     C*------------------------------------------------------------------------*
021500970526     C     SNDMSG        BEGSR
021600970526     C*
021700970526     C* PULISCE IL SFILE MESSAGGI
021800970526     C                   MOVEL     '1'           *IN40
021900991026     C                   WRITE     s723DMC
022000970526     C                   MOVEL     '0'           *IN40
022100970526     C*
022200970526     C* VISUALIZZA IL MESSAGGIO
022300970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
022400970526     C                   CALL      'XMSG'        XMSGPA
022500970526IF  1C     DSMSER        IFEQ      *ZEROS
022600970526     C                   MOVEL     DSMSKE        MSGKEY
022700970526E   1C                   ENDIF
022800970526     C                   Z-ADD     1             MSGULT
022900991026     C                   WRITE     s723DMS
023000991026     C                   WRITE     s723DMC
023100970526     C*
023200970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
023300970526     C                   MOVEL     *BLANKS       DSMSMS
023400970526     C*
023500970526     C                   ENDSR
023600970526     C*------------------------------------------------------------------------*
023700970526     C* ENTGES - GESTIONE ENTER -> INVIO
023800970526     C*------------------------------------------------------------------------*
023900970526     C     ENTGES        BEGSR
024000970526     C*
024100970526     C* CONTROLLA I CAMPI IMMESSI
024200970526     C                   EXSR      DECODI
024300970526     C*
024400970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
024500970526IF  1C     DEPOP1        IFEQ      'O05'
024600970526     C                   MOVEL     '1'           FINL00
024700970526E   1C                   ENDIF
024800970526     C*
024900970526     C                   ENDSR
025000031229     C*------------------------------------------------------------------------*
025100031229     C* F01GES - GESTIONE F01 -> VISUALIZZA REGOLE PARAMETRI TRADUTTORE
025200031229     C*------------------------------------------------------------------------*
025300031229     C     F01GES        BEGSR
025400031229     C*
025500031229     C* Passo o il traduttore standard relativo al tipo file corrente o se inserito il
025600031229     C* traduttore specifico
025700031229     C                   IF        VIDPGT <> *blanks
025800031229     C                   EVAL      wPGT = VIDPGT
025900031229     C                   ELSE
026000031229     C                   EVAL      wPGT = VIDPGTD
026100031229     C                   ENDIF
026200031229     C*
026300031229     C* Visualizzo le regole relative
026400031229     C                   CALL      'TIS723C'
026500031229     C                   PARM                    wPGT             10
026600040107     C                   PARM                    wERR              1
026700040107     C*
026800040107     C* Verifico se trovato membro con descrizoine dettaglio parametri traduttore
026900040107     C                   IF        wERR = '1'
027000040107     C                   MOVEL     MSG(23)       DSMSMS
027100040107     C                   ENDIF
027200031229     C*
027300031229     C                   ENDSR
027400970526     C*------------------------------------------------------------------------*
027500970526     C* F03GES - GESTIONE F03 -> FINE
027600970526     C*------------------------------------------------------------------------*
027700970526     C     F03GES        BEGSR
027800970526     C*
027900970526     C* IMPOSTA "PREMUTO F03"
028000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
028100991025     C                   MOVEL     '1'           S20F03                         *DI PROCEDURA
028200970526     C*
028300970526     C                   ENDSR
028400970526     C*------------------------------------------------------------------------*
028500970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
028600970526     C*------------------------------------------------------------------------*
028700970526     C     F06GES        BEGSR
028800970526     C*
028900970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
029000970526IF  1C     DEPOP1        IFEQ      'F06'
029100970526     C                   EXSR      INSERI
029200970526E   1C                   ENDIF
029300970526IF  1C     DEPOP1        IFEQ      'O02'
029400970526     C                   EXSR      MODIFI
029500970526E   1C                   ENDIF
029600991014IF  1C     DEPOP1        IFEQ      'O03'
029700011129     C                   EXSR      COPIA
029800991014E   1C                   ENDIF
029900991104IF  1C*    DEPOP1        IFEQ      'O04'
030000991104     C*                  EXSR      ANNULL
030100991104E   1C*                  ENDIF
030200970526IF  1C     DEPOP1        IFEQ      'O07'
030300970526     C                   EXSR      RIPRIS
030400970526E   1C                   ENDIF
030500080603     C*
030600080603     C* Gestione estensione anagrafica clienti/tipi file => TRC 01
030700080603     C                   CLEAR                   DVIRE01
030800080603     C                   EVAL      KCEdti  = virDTI
030900080603     C                   EVAL      vireTRC = '01'
031000080603     C     KEYvire1_C    CHAIN     tivire1l
031100080603     C                   IF        %found(tivire1l)
031200080603     C                   IF        vidEML <> *blanks
031300080603     C                   EVAL      §vire01INV = 'S'
031400080603     C                   EVAL      §vire01EML = vidEML
031500080603     C                   EVAL      vireFLO = DVIRE01
031600080603     C                   UPDATE    tivire00
031700080603     C                   ELSE
031800080603     C                   DELETE    tivire00
031900080603     C                   ENDIF
032000080603     C                   ELSE
032100080603     C                   IF        vidEML <> *blanks
032200080603     C                   EVAL      vireKSC = virKSC
032300080603     C                   EVAL      vireTIP = virTIP
032400080603     C                   EVAL      vireDTI = virDTI
032500080603     C                   EVAL      vireTRC = '01'
032600080603     C                   EVAL      §vire01INV = 'S'
032700080603     C                   EVAL      §vire01EML = vidEML
032800080603     C                   EVAL      vireFLO = DVIRE01
032900080603     C                   WRITE     tivire00
033000080603     C                   ENDIF
033100080603     C                   ENDIF
033200970526     C*
033300011129     C* Dopo aver effettuato le operazioni richieste apro la tabela correlata al tipo file.
033400011129     C                   IF        errl00 = '0'                                 *NO ERRORI
033500011129     C                   CALL      'TIS70UR'
033600011129     C                   PARM                    KPJBA
033700011129     C                   PARM                    VIDTIP
033800011129     C                   PARM                    VIDKSC
033900011129     C                   PARM      *blanks       flgope            1
034000011129     C                   ENDIF
034100011129     C*
034200970526     C                   ENDSR
034300970526     C*------------------------------------------------------------------------*
034400970526     C* F12GES - GESTIONE F12 -> RITORNO
034500970526     C*------------------------------------------------------------------------*
034600970526     C     F12GES        BEGSR
034700970526     C*
034800970526     C* IMPOSTA "PREMUTO F12"
034900970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
035000991025     C                   MOVEL     '1'           S20F12                         *DI PROCEDURA
035100970526     C*
035200970526     C                   ENDSR
035300970526     C*------------------------------------------------------------------------*
035400970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
035500970526     C*------------------------------------------------------------------------*
035600970526     C     F24GES        BEGSR
035700970526     C*
035800970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
035900970526     C*
036000970526     C                   ENDSR
036100970526     C*------------------------------------------------------------------------*
036200970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
036300970526     C*------------------------------------------------------------------------*
036400970526     C     INSERI        BEGSR
036500970526     C*
036600970526     C* CONTROLLA I CAMPI IMMESSI
036700970526     C                   EXSR      DECODI
036800970526     C*
036900970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
037000991026IF  1C                   if        errl00 = '0'                                 *NO ERRORI
037100991026     C                   movel     VIDTip        virtip
037200991026     C                   move      VIDksc        virksc
037300991026     C                   z-add     VIDseq        virseq
037400991026     C                   Z-ADD     viddti        G08dat
037500991026     C                   Z-ADD     *ZEROS        G08inv
037600991026     C                   MOVEL     '0'           G08ERR
037700991026     C                   CALL      'XSRDA8'
037800991026     C                   PARM                    WLBDA8
037900991026     C                   Z-ADD     G08inv        virdti
038000991026     c                   exsr      impvir
038100991026     C                   WRITE     tivir000
038200991025     C                   MOVEL     MSG(2)        S20MSG                         *MESSAGGIO
038300970602     C*
038400970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
038500020228     C****               EXSR      AZZVID
038600970526E   1C                   ENDIF
038700970526     C*
038800970526     C                   ENDSR
038900970526     C*------------------------------------------------------------------------*
039000970526     C* MODIFI - OPERAZIONI DI MODIFICA
039100970526     C*------------------------------------------------------------------------*
039200970526     C     MODIFI        BEGSR
039300970526     C*
039400970526     C* CONTROLLA I CAMPI IMMESSI
039500970526     C                   EXSR      DECODI
039600970526     C*
039700970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
039800970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
039900991026     C                   EXSR      LETvir
040000970526     C*
040100970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
040200970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
040300991026     C                   EXSR      IMPvir                                       *IMPOSTA I CAMPI
040400991026     C                   UPDATE    tivir000
040500101027     C                   EVAL      S20MSG = MSG(3)
040600101027     C                   EVAL      DSMSMS = MSG(3)
040700101027     C*
040800101027     C* Se trattasi d tipo servizio "idoneo" modifico anche i dati utente lato "BARTVAS"
040900101027     C                   IF        *IN80
041000111215     C                             and AS400_ok = *on
041100101027      /FREE
041200101027
041300101027         if *in30 = *off;
041400101027           pInMaskChk  = VIDPPIMC;
041500101027         else;
041600101027           pInMaskChk  = pOutCliMskChk;
041700101027         endif;
041800101027
041900101027         if *in31 = *off;
042000101027           pInMaskFile = VIDPPIMF;
042100101027         else;
042200101027           pInMaskFile = pOutCliMskFil;
042300101027         endif;
042400101027
042500101027         if *in32 = *off;
042600101027           pInMaskSem  = VIDPPIMS;
042700101027         else;
042800101027           pInMaskSem  = pOutCliMskSem;
042900101027         endif;
043000101027
043100101027         if *in33 = *off;
043200101027           pInMaskZip  = VIDPPIMZ;
043300101027         else;
043400101027           pInMaskZip  = pOutCliMskZip;
043500101027         endif;
043600101027
043700101027
043800101027         if UBBVSQL01_Upd01UnqCliTipFile(
043900101027                         pInConn
044000101027                       : VIDKSC
044100101027                       : VIDTIP
044200101027                       : pInMaskChk
044300101027                       : pInMaskFile
044400101027                       : pInMaskSem
044500101027                       : pInMaskZip) = 0;
044600101027         else;
044700101027              S20MSG = MSG(29);
044800101027              DSMSMS = MSG(29);
044900101027         endif;
045000101027
045100101027      /END-FREE
045200101027     C
045300101027     C*
045400101027     C* Quindi (già che c sono ricarico anche il job
045500101027     C                   callp(e)  UBBVSKT01_ReloadCliTipF(
045600101027     C                                           VIDKSC
045700101027     C                                          :VIDTIP
045800101027     C                                          :pOutCliProc)
045900101027     C                   ENDIF
046000101027     C*
046100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
046200970526E   2C                   ENDIF
046300970526E   1C                   ENDIF
046400970526     C*
046500970526     C                   ENDSR
046600991014     C*------------------------------------------------------------------------*
046700991014     C* copia  - OPERAZIONI DI copia
046800991014     C*------------------------------------------------------------------------*
046900991014     C     copia         BEGSR
047000991014     C*
047100991014     C* CONTROLLA I CAMPI IMMESSI
047200991014     C                   EXSR      DECODI
047300991014     C*
047400991014     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
047500991014IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
047600991026     C                   movel     VIDTip        virtip
047700991110     C                   move      VIDksc        virksc
047800991110     C                   z-add     VIDseq        virseq
047900991110     C                   Z-ADD     viddti        G08dat
048000991110     C                   Z-ADD     *ZEROS        G08inv
048100991110     C                   MOVEL     '0'           G08ERR
048200991110     C                   CALL      'XSRDA8'
048300991110     C                   PARM                    WLBDA8
048400991110     C                   Z-ADD     G08inv        virdti
048500991026     C                   EXSR      IMPvir                                       *IMPOSTA I CAMPI
048600991026     C                   write     tivir000
048700991025     C                   MOVEL     MSG(3)        S20MSG
048800991014     C                   MOVEL     '1'           FINL00                         *FINE PGM
048900991014E   2C                   ENDIF
049000991014     C*
049100991014     C                   ENDSR
049200970526     C*------------------------------------------------------------------------*
049300970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
049400970526     C*------------------------------------------------------------------------*
049500991104     C*    ANNULL        BEGSR
049600991025     C*
049700991025     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
049800991104     C*                  EXSR      LETvir
049900991025     C*
050000991025     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
050100991104IF  1C*    ERRL00        IFEQ      '0'                                          *NO ERRORI
050200991104     C*                  DELETE    tivir000                                     *CANCELLA
050300991104     C*                  MOVEL     MSG(4)        s20MSG                         *MESSAGGIO
050400991104     C*                  MOVEL     '1'           FINL00                         *FINE PGM
050500991104E   1C*                  ENDIF
050600970526     C*
050700991104     C*                  ENDSR
050800970526     C*------------------------------------------------------------------------*
050900970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
051000970526     C*------------------------------------------------------------------------*
051100970526     C     RIPRIS        BEGSR
051200970526     C                   ENDSR
051300970526     C*------------------------------------------------------------------------*
051400991026     C* LETvir - LEGGE IL RECORD PERL'AGGIORNAMENTO
051500970526     C*------------------------------------------------------------------------*
051600991026     C     LETvir        BEGSR
051700970526     C*
051800991025     C                   MOVEL     S20tip        KCEtip
051900991026     C                   MOVEL     S20ksc        KCEksc
052000000420     C     KEYvi2        CHAIN     tivir03L                           9998
052100970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
052200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
052300991025     C                   MOVEL     MSG(7)        S20MSG                         *MESSAGGIO
052400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
052500970526E   1C                   ENDIF
052600970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
052700970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
052800991025     C                   MOVEL     MSG(6)        S20MSG                         *MESSAGGIO
052900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
053000970526E   1C                   ENDIF
053100970526     C*
053200970526     C                   ENDSR
053300970526     C*------------------------------------------------------------------------*
053400970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
053500970526     C*------------------------------------------------------------------------*
053600970526     C     DECODI        BEGSR
053700001120     C*
053800001120     C*----------------------
053900001120     C* Controllo del codice
054000001120     C*----------------------
054100001120     C     ERRL00        IFEQ      '0'
054200001120     C     VIDksc        IFne      *blank
054300001120     C     VIDksc        andne     *zeros
054400001120     C                   exsr      cntksc
054500001120     C                   ELSE
054600991027     C                   MOVEL     '1'           ERRL00
054700991027     C                   SETON                                        65
054800991027     C                   MOVEL     MSG(12)       DSMSMS
054900001120     C                   ENDIF
055000001120     C                   ENDIF
055100101026     C*
055200101026     C* Verifico se x il servizio a video è richiesta l'indicazione della password FTP
055300101026     C     ERRL00        IFEQ      '0'
055400101026     C     VIDisv        lookup    isvftpeml                              80
055500101026 xxx C***                setoff                                       80
055600101026     C                   ENDIF
055700001120     C*----------------------
055800001120     C* Controllo della sequenza
055900001120     C*----------------------
056000001120     C     ERRL00        IFEQ      '0'
056100001120     C     VIDseq        IFne      0
056200001120     C                   ELSE
056300991027     C                   MOVEL     '1'           ERRL00
056400991027     C                   SETON                                        67
056500991027     C                   MOVEL     MSG(12)       DSMSMS
056600001120     C                   ENDIF
056700001120     C                   ENDIF
056800120109     C*----------------------
056900120109     C* Controllo tipo file
057000120109     C*----------------------
057100120109     C     ERRL00        IFEQ      '0'
057200120109     C     VIDtip        IFne      *blank
057300120109     C                   exsr      cnttip
057400120109     C                   ELSE
057500120109     C                   MOVEL     '1'           ERRL00
057600120109     C                   SETON                                        66
057700120109     C                   MOVEL     MSG(12)       DSMSMS
057800120109     C                   ENDIF
057900120109     C                   ENDIF
058000120109     C*----------------------
058100120109     C* Tipo servizio
058200120109     C*----------------------
058300120109     C     ERRL00        IFEQ      '0'
058400120109     C     VIDisv        IFne      *blank
058500120109     C                   exsr      cntisv
058600120109     C* Verifico la compatibilità del servizio rispetto al tipo-file
058700120109     C                   IF        VTFUD <> §ISVVERS
058800120109     C                   MOVEL     '1'           ERRL00
058900120109     C                   SETON                                        6670
059000120109     C                   MOVEL     MSG(24)       DSMSMS
059100120109     C                   ENDIF
059200120109     C                   ELSE
059300120109     C                   MOVEL     '1'           ERRL00
059400120109     C                   SETON                                        70
059500120109     C                   MOVEL     MSG(12)       DSMSMS
059600120109     C                   ENDIF
059700120109     C                   ENDIF
059800120109     C*--------------------
059900120109     C* Dati solo x upload no in download
060000120109     C*--------------------
060100120109     C* a seconde se siamo in Download o Upload sproteggo alcuni campi e ne proteggo altri
060200120109     C* e se non abbiamo errori pulisco i campi protetti (se abbiamo un errore potrebbe essere
060300120109     C*  che non siamo in D o U e i valori nei campi servono)
060400120109     C                   IF        §ISVVers   = 'D'
060500120109     c                   SETON                                        93
060600120109     C     ERRL00        IFEQ      '0'
060700120109     C                   MOVEL     *blanks       VIDpoi
060800120109     C                   MOVEL     *blanks       VIDEmL
060900120109     C                   MOVEL     *blanks       VIDfi2
061000120109     C                   MOVEL     *blanks       VIDfi4
061100120109     C                   MOVEL     *blanks       VIDfi4D
061200120109     C                   ENDIF
061300120109     C                   ENDIF
061400120109     C                   IF        §ISVVers   = 'U'
061500120109     C                   SETOFF                                       93
061600120109     C     ERRL00        IFEQ      '0'
061700120109     C                   MOVEL     *blanks       VIDTMbD
061800120109     C                   MOVEL     *blanks       VIDTip2
061900120109     C                   MOVEL     *blanks       VIDTip3
062000120109     C                   MOVEL     *blanks       VIDTip4
062100120119     C                   MOVEL     *blanks       VIDKsc2
062200120119     C                   MOVEL     *blanks       VIDKsc3
062300120119     C                   MOVEL     *blanks       VIDKsc4
062400120109     C                   ENDIF
062500120109     C                   ENDIF
062600100614     C*--------------------
062700100614     C* Tipo membro d default...  solo x anagrafiche d download (ovviamente no in visualizz.)
062800100614     C*--------------------
062900100614     C     ERRL00        IFEQ      '0'
063000100614     C                   IF        DEPOP1 = 'F06' OR
063100100614     C                             DEPOP1 = 'O03'
063200100614     C                   IF        §ISVPROC = 'F' AND
063300100614     C                             §ISVVERS = 'D'
063400100614     C                   IF        VIDtmbd = *blanks AND
063500100614     C                             vtfud   = 'D'
063600100614     C                   MOVEL     'M'           VIDtmbd
063700100614     C                   ENDIF
063800100614     C                   ENDIF
063900100614     C                   ENDIF
064000100614     C                   ENDIF
064100030603     C*----------------------
064200030603     C* Tipo membro
064300030603     C*----------------------
064400030603     C     ERRL00        IFEQ      '0'
064500030603     C     VIDtmbd       IFNE      *blank
064600030603     C                   IF        §ISVPROC = 'F' AND
064700030603     C                             §ISVVERS = 'D'
064800030603     C                   ELSE
064900030603     C                   MOVEL     '1'           ERRL00
065000030603     C                   SETON                                        61
065100030603     C                   MOVEL     MSG(22)       DSMSMS
065200030603     C                   ENDIF
065300030603     C                   ENDIF
065400030603     C                   ENDIF
065500120109     C*----------------------
065600120109     C* Campi duplicazione automatica su tipi file
065700120109     C*----------------------
065800120109     C     ERRL00        IFEQ      '0'
065900120109     C*
066000120109     C                   EVAL      MaxCiclo = %elem(Tips)
066100120109     C                   EVAL      Tips(1)  = VIDTip2
066200120109     C                   EVAL      Tips(2)  = VIDTip3
066300120109     C                   EVAL      Tips(3)  = VIDTip4
066400120119     C* se il tipo file è valorizzato e il cliente relativo no (o 0 o ' ')
066500120119     C* lo forzo col cliente principale
066600120119     C                   IF        (VIDKsc2 = *zeros or
066700120119     C                              VIDKsc2 = *blank   ) and
066800120119     C                             VIDTip2 <> *blank
066900120119     C                   EVAL      VIDKsc2 =  VIDKsc
067000120119     C                   ENDIF
067100120119     C                   IF        (VIDKsc3 = *zeros or
067200120119     C                              VIDKsc3 = *blank   ) and
067300120119     C                             VIDTip3 <> *blank
067400120119     C                   EVAL      VIDKsc3 =  VIDKsc
067500120119     C                   ENDIF
067600120119     C                   IF        (VIDKsc4 = *zeros or
067700120119     C                              VIDKsc4 = *blank   ) and
067800120119     C                             VIDTip4 <> *blank
067900120119     C                   EVAL      VIDKsc4 =  VIDKsc
068000120119     C                   ENDIF
068100120119     C                   EVAL      Kscs(1)  = VIDKsc2
068200120119     C                   EVAL      Kscs(2)  = VIDKsc3
068300120119     C                   EVAL      Kscs(3)  = VIDKsc4
068400120109     C     1             DO        MaxCiclo      i
068500120109     C     Tips(i)       IFNE      *blank
068600120119     C     Kscs(i)       ORNE      *blank
068700120119     C     Kscs(i)       ANDNE     *zeros
068800120109     C                   IF        §ISVVERS = 'D'
068900120119     C                   EXSR      CntTip234
069000120111     C* se mi è stato dato un errore, esco dal ciclo
069100120111     C                   IF        ERRL00 <> '0'
069200120111     C                   LEAVE
069300120111     C                   ENDIF
069400120109     C                   ELSE
069500120109     C                   SELECT
069600120109     C                   WHEN      i = 1
069700120119     C                   SETON                                        7549
069800120109     C                   WHEN      i = 2
069900120119     C                   SETON                                        7650
070000120109     C                   WHEN      i = 3
070100120119     C                   SETON                                        7751
070200120109     C                   ENDSL
070300120109     C                   MOVEL     '1'           ERRL00
070400120109     C                   MOVEL     MSG(30)       DSMSMS
070500120109     C                   ENDIF
070600120109     C                   ENDIF
070700120109     C                   ENDDO
070800120109     C*
070900120109     C                   ENDIF
071000001120     C*----------------------
071100001120     C* Controllo della descrizione
071200001120     C*----------------------
071300001120     C     ERRL00        IFEQ      '0'
071400001120     C     VIDdes        IFne      *blanks
071500001120     C                   ELSE
071600991122     C*                  MOVEL     '1'           ERRL00
071700991122     C*                  SETON                                        62
071800991122     C*                  MOVEL     MSG(12)       DSMSMS
071900991122     C                   MOVEL     VDDTIP        VIDDES
072000001120     C                   ENDIF
072100001120     C                   ENDIF
072200991026     C*--------------------
072300991026     C* DATA inizio
072400991026     C*--------------------
072500991026     C     *LIKE         DEFINE    VIDDti        DtiVID
072600001120     C     ERRL00        IFEQ      '0'
072700991026     C                   Z-ADD     VIDDti        wda8
072800991026     C                   Z-ADD     *ZEROS        da8w
072900991026     C                   EXSR      CHKDA8
073000991026     C                   Z-ADD     WDA8          viddti
073100991026     C                   Z-ADD     DA8W          dtivid
073200001120     C     ERRL00        IFEQ      '1'
073300991026     C                   SETON                                        63
073400991026     C                   MOVEL     MSG(14)       dsmsms
073500001120     C                   ENDIF
073600001120     C                   ENDIF
073700991026     C* INSERIMENTO -> RECORD INESISTENTE
073800001120     C     ERRL00        IFEQ      '0'
073900001120     C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
074000001120     C     DEPOP1        orEQ      'O03'                                        *INSERIMENTO
074100991026     C                   move      vidksc        KCEksc
074200000420     C                   move      vidtip        KCEtip
074300000420     C                   z-add     dtivid        KCEdti
074400000420     C     KEYvi2        CHAIN(N)  tivir03L                           99
074500001120     C     *IN99         IFEQ      *OFF
074600991026     C                   MOVEL     '1'           ERRL00
074700991026     C                   MOVEL     MSG(8)        DSMSMS
074800991026     C                   SETON                                        676566
074900991026     C                   SETON                                        63
075000001120     C                   ENDIF
075100001120     C                   ENDIF
075200001120     C                   ENDIF
075300001120     C*--------------------
075400991026     C* DATA SCADENZA
075500991026     C*--------------------
075600991026     C     *LIKE         DEFINE    VIDDtf        DtfVID
075700001120     C     ERRL00        IFEQ      '0'
075800991026     C                   Z-ADD     VIDDtf        WDA8
075900991026     C                   Z-ADD     *ZEROS        DA8W
076000991026     C                   EXSR      CHKDA8
076100991026     C                   Z-ADD     WDA8          VIDDtf
076200991026     C                   Z-ADD     DA8W          DtfVID
076300001120     C     ERRL00        IFEQ      '1'
076400991026     C                   SETON                                        68
076500991026     C                   MOVEL     MSG(14)       DSMSMS
076600001120     C                   ENDIF
076700001120     C                   ENDIF
076800991026     C* SCADENZA < DECORRENZA
076900001120     C     ERRL00        IFEQ      '0'
077000001120     C     DtfVID        IFLT      DtiVID
077100991026     C                   SETON                                        68
077200991026     C                   MOVEL     MSG(15)       DSMSMS
077300991026     C                   MOVEL     '1'           ERRL00
077400001120     C                   ENDIF
077500001120     C                   ENDIF
077600991026     C* SCADENZA >= DATA CORRENTE
077700001120     C     ERRL00        IFEQ      '0'
077800001120     C     DEPOP1        IFEQ      'O02'
077900991026     C     DEPOP1        OREQ      'O07'
078000991026     C     DEPOP1        OREQ      'F06'
078100001120     C     DtfVID        IFLT      DATCOR
078200001120     C                   SETON                                        68
078300991026     C                   MOVEL     MSG(16)       DSMSMS
078400991026     C                   MOVEL     '1'           ERRL00
078500001120     C                   ENDIF
078600001120     C                   ENDIF
078700001120     C                   ENDIF
078800991026     C*---
078900991026     C* DECORRENZA + SCADENZA
079000991026     C*---
079100991026     C* ACCAVALLAMENTO DATI
079200001120     C     ERRL00        IFEQ      '0'
079300991026     C                   MOVEL     VIDtip        KCEtip
079400991026     C                   MOVEL     vidksc        KCEksc
079500991026     C     KEyvir        SETLL     tivir03L
079600991026     C     KEyvir        READE(N)  tivir03L                               99
079700001120     C     *IN99         DOWEQ     *OFF
079800001120     C     DEPOP1        IFNE      'F06'                                        *NON INSERIMENTO
079900991026     C     DtiVID        ANDNE     virdti                                       *RECORD IN LINEA
080000991026     C     DEPOP1        OREQ      'F06'                                        *INSERIMENTO
080100001120     C     DtiVID        IFGE      virdti                                       *DECORRENZA
080200991026     C     DtiVID        ANDLE     virdtf
080300991026     C     DtfVID        ORGE      virdti                                       *SCADENZA
080400991026     C     DtfVID        ANDLE     virdtf
080500991026     C     virdti        ORGE      DtiVID                                       *DECORRENZA
080600991026     C     virdtf        ANDLE     DtfVID
080700991026     C                   SETON                                        666765
080800991026     C                   SETON                                        6368
080900991026     C                   MOVEL     MSG(17)       DSMSMS
081000991026     C                   MOVEL     '1'           ERRL00
081100991026     C                   LEAVE
081200001120     C                   ENDIF
081300001120     C                   ENDIF
081400991026     C     KEyvir        READE(N)  tivir03L                               99
081500001120     C                   ENDDO
081600001120     C                   ENDIF
081700001120     C*--------------------
081800001120     C* Controllo filiale
081900001120     C*--------------------
082000001120     C     ERRL00        IFEQ      '0'
082100001120     C     VIDpoi        IFne      *blank
082200001120     C     VIDpoi        andne     *zeros
082300001120     C                   exsr      cntpoi
082400001120     C                   ELSE
082500120109     C     §ISVVers      IFNE      'D'
082600991027     C                   MOVEL     '1'           ERRL00
082700000724     C                   MOVEL     MSG(12)       DSMSMS
082800991027     C                   SETON                                        69
082900991122     C                   ENDIF
083000001120     C                   ENDIF
083100001120     C                   ENDIF
083200001120     C*--------------------
083300011016     C* Programma traduttore
083400001120     C*--------------------
083500001120     C     ERRL00        IFEQ      '0'
083600001120     C     VIDpgt        IFNE      *blank
083700001120     C                   CALL(e)   'TIS721C'
083800050112     C                   PARM      *blanks       LIBRERIA         10
083900001120     C                   PARM      VIDpgt        OBJECT           10
084000001120     C                   PARM      '*PGM   '     TYPE              7
084100001120     C                   PARM      '0'           ESITO             1
084200001120     C                   IF        ESITO = '1'
084300001120     C                   MOVEL     '1'           ERRL00
084400001120     C                   SETON                                        54
084500001120     C                   MOVEL     MSG(19)       DSMSMS
084600001120     C                   ENDIF
084700001120     C                   ENDIF
084800001120     C                   ENDIF
084900001120     C*--------------------
085000001120     C* Programma post-traduzione
085100001120     C*--------------------
085200001120     C     ERRL00        IFEQ      '0'
085300001120     C     VIDpgi        IFNE      *blank
085400001120     C                   CALL(e)   'TIS721C'
085500050112     C                   PARM      *blanks       LIBRERIA         10
085600001120     C                   PARM      VIDpgi        OBJECT           10
085700001120     C                   PARM      '*PGM   '     TYPE              7
085800001120     C                   PARM      '0'           ESITO             1
085900001120     C                   IF        ESITO = '1'
086000001120     C                   MOVEL     '1'           ERRL00
086100001120     C                   SETON                                        55
086200001120     C                   MOVEL     MSG(19)       DSMSMS
086300001120     C                   ENDIF
086400001120     C                   ENDIF
086500001120     C                   ENDIF
086600001120     C*--------------------
086700001120     C* File da tradurre
086800001120     C*--------------------
086900001120     C     ERRL00        IFEQ      '0'
087000001120     C     VIDfid        IFNE      *blank
087100001120     C                   CALL(e)   'TIS721C'
087200050112     C                   PARM      *blanks       LIBRERIA         10
087300001120     C                   PARM      VIDfid        OBJECT           10
087400001120     C                   PARM      '*FILE  '     TYPE              7
087500001120     C                   PARM      '0'           ESITO             1
087600001120     C                   IF        ESITO = '1'
087700001120     C                   MOVEL     '1'           ERRL00
087800001120     C                   SETON                                        53
087900001120     C                   MOVEL     MSG(20)       DSMSMS
088000001120     C                   ENDIF
088100001120     C                   ENDIF
088200001120     C                   ENDIF
088300001120     C*--------------------
088400001120     C* File storico
088500001120     C*--------------------
088600001120     C     ERRL00        IFEQ      '0'
088700001120     C     VIDfis        IFNE      *blank
088800001120     C                   CALL(e)   'TIS721C'
088900050112     C                   PARM      *blanks       LIBRERIA         10
089000001120     C                   PARM      VIDfis        OBJECT           10
089100001120     C                   PARM      '*FILE  '     TYPE              7
089200001120     C                   PARM      '0'           ESITO             1
089300001120     C                   IF        ESITO = '1'
089400001120     C                   MOVEL     '1'           ERRL00
089500001120     C                   SETON                                        56
089600001120     C                   MOVEL     MSG(20)       DSMSMS
089700001120     C                   ENDIF
089800001120     C                   ENDIF
089900001120     C                   ENDIF
090000001120     C*--------------------
090100001120     C* File tradotto
090200001120     C*--------------------
090300001120     C     ERRL00        IFEQ      '0'
090400001120     C     VIDfit        IFNE      *blank
090500001120     C                   CALL(e)   'TIS721C'
090600050112     C                   PARM      VIDfi3        LIBRERIA         10
090700001120     C                   PARM      VIDfit        OBJECT           10
090800001120     C                   PARM      '*FILE  '     TYPE              7
090900001120     C                   PARM      '0'           ESITO             1
091000001120     C                   IF        ESITO = '1'
091100001120     C                   MOVEL     '1'           ERRL00
091200001120     C                   SETON                                        57
091300001120     C                   MOVEL     MSG(20)       DSMSMS
091400001120     C                   ENDIF
091500001120     C                   ENDIF
091600001120     C                   ENDIF
091700001120     C*--------------------
091800001120     C* Libreria file tradotto
091900001120     C*--------------------
092000001120     C     ERRL00        IFEQ      '0'
092100001120     C     VIDfi3        IFNE      *blank
092200001120     C                   CALL(e)   'TIS721C'
092300050112     C                   PARM      *blanks       LIBRERIA         10
092400001120     C                   PARM      VIDfi3        OBJECT           10
092500001120     C                   PARM      '*LIB   '     TYPE              7
092600001120     C                   PARM      '0'           ESITO             1
092700001120     C                   IF        ESITO = '1'
092800001120     C                   MOVEL     '1'           ERRL00
092900001120     C                   SETON                                        52
093000001120     C                   MOVEL     MSG(21)       DSMSMS
093100001120     C                   ENDIF
093200001120     C                   ENDIF
093300001120     C                   ENDIF
093400040816     C*--------------------
093500100311     C* Controllo corrispondenza Tipo File / Programma Traduttore / File Tradotto / Tipo membro Trad
093600040816     C*--------------------
093700040816     C     ERRL00        IFEQ      '0'
093800040816     C     VIDfit        IFne      *blank
093900040816     C     VIDpgt        ORne      *blank
094000040816     C                   EVAL      TBEcod = 'IFT'
094100040816     C     KEYtbe1_P     SETLL     tntbe01l
094200040816     C                   IF        %equal(tntbe01l)
094300040816     C* Inizializzo indici e schiere
094400040816     C                   Z-ADD     *zeros        j
094500040816     C                   CLEAR                   skTipF
094600040816     C                   CLEAR                   skPgt
094700040816     C                   CLEAR                   skFileT
094800100311     C                   CLEAR                   skMembroT
094900040816     C* Carico le schiere da tabella 'IFT'
095000040816     C     KEYtbe1_P     READE     tntbe01l
095100040816     C                   DOW       not %eof(tntbe01l)
095200040816     C                   IF        TBEatb = *blanks
095300040816     C                   EVAL      DIFT = TBEuni
095400040816     C                   ADD       1             j
095500100311     C                   EVAL      skTipF(j)    = TBEke1
095600100311     C                   EVAL      skPgt(j)     = §IFTPGT
095700100311     C                   EVAL      skFileT(j)   = §IFTFILE
095800100311     C                   EVAL      skMembroT(j) = §IFTTMBD
095900040816     C                   ENDIF
096000040816     C     KEYtbe1_P     READE     tntbe01l
096100040816     C                   ENDDO
096200040816     C* Imposto cosa testare
096300040816     C                   IF        VIDPGT <> *blanks
096400040816     C                   EVAL      wVIDPGT = VIDPGT
096500040816     C                   ELSE
096600040816     C                   EVAL      wVIDPGT = VIDPGTD
096700040816     C                   ENDIF
096800040816     C                   IF        VIDFIT <> *blanks
096900040816     C                   EVAL      wVIDFIT = VIDFIT
097000040816     C                   ELSE
097100040816     C                   EVAL      wVIDFIT = VIDFITD
097200040816     C                   ENDIF
097300040816     C* Testo se il traduttore indicato è tra quelli da controllare (in tabella 'IFT')
097400040823     C                   MOVEL     '0'           wFlgIFTok         1
097500100223     C                   SETOFF                                       16
097600040816     C     wVIDPGT       LOOKUP    skPgt                                  15
097700040816     C                   IF        %found
097800040816     C* Testo se vi è corrispondenza
097900040816     C                   EVAL      j = 1
098000040816     C                   DOW       j <= %elem(skTipF)
098100100223     C                   IF        skTipF(j)  = VIDTIP  AND
098200100223     C                             skPgt(j)   = wVIDPGT
098300100223     C                   SETON                                        16
098400100311     C*
098500100223     C                   IF        skFileT(j) = wVIDFIT
098600040816     C                   MOVEL     '1'           wFlgIFTok
098700100311     C*
098800100311     C                   IF        DEPOP1 = 'O02' OR
098900100311     C                             DEPOP1 = 'O03' OR
099000100311     C                             DEPOP1 = 'F06'
099100100312     C                   IF        VIDtmbd      =  *blanks AND
099200100312     C                             skMembroT(j) <> *blanks
099300100311     C                   EVAL      VIDtmbd = skMembroT(j)
099400100311     C                   SETON                                        61
099500100311     C                   ENDIF
099600100311     C                   ENDIF
099700100311     C*
099800040816     C                   LEAVE
099900040816     C                   ENDIF
100000100311     C*
100100100223     C                   ENDIF
100200040816     C                   ADD       1             j
100300040816     C                   ENDDO
100400040816     C* Se nn trovata esatta corrispondenza => errore
100500100223     C                   IF        *IN16 and wFlgIFTok = '0'
100600040816     C                   MOVEL     '1'           ERRL00
100700040816     C                   MOVEL     MSG(25)       DSMSMS
100800040816     C                   SETON                                        665457
100900040816     C                   ENDIF
101000040816     C                   ENDIF
101100040816     C                   ENDIF
101200040816     C                   ENDIF
101300040823     C                   ENDIF
101400051026     C*--------------------
101500051026     C* Controllo particolare x anagrafiche d download: se si sta cambiando il traduttore
101600051026     C* verifico prima che nn vi siano attualmente record d log a stato zero
101700051026     C*--------------------
101800051026     C     ERRL00        IFEQ      '0'
101900120109     C     §ISVVers      IFEQ      'D'
102000051026     C     VIDpgt        IFNE      virPGT
102100051026     C     VIDpgi        ORNE      virPGI
102200051026     C                   CALL      'TIS723R1'
102300051026     C                   PARM                    VIDKSC
102400051026     C                   PARM                    VIDTIP
102500120109     C                   PARM                    §ISVVers
102600051026     C                   PARM      *zeros        wRecZero          9 0
102700051026     C                   IF        wRecZero > *zeros
102800051026     C                   MOVEL     '1'           ERRL00
102900051026     C                   MOVEL     MSG(26)       DSMSMS
103000051026     C                   SETON                                        5455
103100051026     C                   ENDIF
103200051026     C                   ENDIF
103300051026     C                   ENDIF
103400051026     C                   ENDIF
103500060616     C*--------------------
103600060616     C* File Da tradurre / File Storico / Programma Post-Traduzione
103700060616     C*--------------------
103800060616     C     ERRL00        IFEQ      '0'
103900060616     C                   IF        (VIDfid='*' AND VIDfis='*' AND VIDpgi='*') OR
104000060616     C                             (VIDfid<>'*' AND VIDfis<>'*' AND VIDpgi<>'*')
104100060616     C                   ELSE
104200060616     C                   MOVEL     '1'           ERRL00
104300060616     C                   SETON                                        535556
104400060616     C                   MOVEL     MSG(27)       DSMSMS
104500060616     C                   ENDIF
104600060616     C                   ENDIF
104700081020     C*--------------------
104800081020     C* Mail x eventuale feed-back richiesto
104900081020     C*--------------------
105000081020     C     ERRL00        IFEQ      '0'
105100081020     C                   CLEAR                   DSEMAIL
105200081020     C                   EVAL      §EMLINDI = VIDEML
105300081020     C                   CALL      'XEMAIL'
105400081020     C                   PARM                    DSEMAIL
105500081020     C                   IF        §EMLERRO = '1'
105600081020     C                   MOVEL     '1'           ERRL00
105700081020     C                   SETON                                        64
105800081020     C                   MOVEL     §EMLMSGO      DSMSMS
105900081020     C                   ENDIF
106000081020     C                   ENDIF
106100001120     C*
106200001120     C                   ENDSR
106300000626     C*------------------------------------------------------------------------*
106400000626     C* Cntisv - CONTROLLA codice tipo legame
106500000626     C*------------------------------------------------------------------------*
106600001120     C     cntisv        begsr
106700001120     C                   clear                   TIBS02DS
106800001120     C*
106900101027     C     '?'           SCAN      Vidisv                                 28
107000001120     C*  Ricerca
107100001120     C                   eval      t02tla = 'L'
107200101027     C                   IF        *IN28 = *ON
107300001120     C                   movel     'R'           t02mod
107400001120     C                   else
107500001120     C                   movel     'C'           t02mod
107600001120     C                   movel(p)  vidisv        t02ke1
107700001120     C                   end
107800000626     C                   movel     'ISV'         t02cod
107900001120     C                   movel     knsif         t02sif
108000001120     C                   CALL      'TIBS02R'
108100001120     C                   parm                    KPJBA
108200001120     C                   parm                    TIBS02DS
108300001120     C
108400001120     C*
108500001120     C                   IF        T02err = *blanks
108600000626     C                   movel     T02ke1        Vidisv
108700000626     C                   movel     T02uni        vddisv
108800030603     C                   movel(P)  T02uni        DISV
108900001120     C                   else
109000001120     C                   movel     '1'           errl00
109100001120     C                   seton                                        70
109200001120     C                   MOVEL     MSG(18)       DSMSMS
109300001120     C                   ENDIF
109400001120     C
109500001120     C                   endsr
109600991027     C*------------------------------------------------------------------------*
109700991027     C* cnttip - Controlla esistenza tipo file su tivt00f
109800991027     C*------------------------------------------------------------------------*
109900001120     C     cnttip        begsr
110000001120     C                   clear                   TIBS02DS
110100000626     C*
110200101027     C     '?'           SCAN      vidtip                                 28
110300001120     C*  Ricerca
110400001120     C                   eval      t02tla = 'L'
110500101027     C                   IF        *IN28 = *ON
110600001120     C                   movel     'R'           t02mod
110700001120     C                   else
110800001120     C                   movel     'C'           t02mod
110900001120     C                   movel(p)  vidtip        t02ke1
111000001120     C                   end
111100991108     C                   movel     'ITF'         t02cod
111200001120     C                   movel     knsif         t02sif
111300991108     C                   CALL      'TIBS02R'
111400001120     C                   parm                    KPJBA
111500001120     C                   parm                    TIBS02DS
111600001120     C
111700001120     C*
111800001120     C                   IF        T02err = *blanks
111900991108     C                   movel     T02ke1        vidtip
112000020218     C                   movel     T02uni        DITF
112100020218     C                   movel     §ITFDESC      vddtip
112200021003     C* Sostituisco con il dato proveniente dalla tabella solo se nn già indicato
112300021003     C* qualcosa nelle regole specifiche x il cliente/tipo file.
112400021014     C                   if        vidfit = *blanks
112500021014     C                   movel     §ITFLIBD      vidfi2
112600021014     C                   endif
112700001120     C                   else
112800001120     C                   movel     '1'           errl00
112900991108     C                   movel     *blank        vddtip
113000111215     C* pulisco anche il cod. tabella associata
113100111215     C                   movel     *blank        vidtab
113200001120     C                   seton                                        66
113300991108     C                   MOVEL     msg(11)       DSMSMS
113400991108     C                   ENDIF
113500001120     C                   if        errl00 = '0'
113600120109     C     vidtip        chain     tivtf01l                           97
113700001120     C                   if        not *in97
113800001120     C                   movel     vtfpgt        vidpgtd
113900001120     C                   movel     vtfpgi        vidpgid
114000001120     C                   movel     vtffid        vidfidd
114100001120     C                   movel     vtffis        vidfisd
114200001120     C                   movel     vtffit        vidfitd
114300111214     C                   movel     vtffi1        vidtab
114400101013     C                   movel     vtfppi        DVTFPPI
114500101026     C                   if        DEPOP1 = 'F06' OR
114600101026     C                             DEPOP1 = 'O03'
114700101026     C                   select
114800101026     C                   when      vtfUD = 'U'
114900101026     C                   movel     D§VTFPPIMF    vidppiMC
115000101026     C                   when      vtfUD = 'D'
115100101026     C                   movel     D§VTFPPIMF    vidppiMF
115200101026     C                   endsl
115300101026     C                   movel     D§VTFPPIMS    vidppiMS
115400101026     C                   movel     D§VTFPPIMZ    vidppiMZ
115500101026     C                   endif
115600100308     C                   if        vidfi4d <> *blanks and vidfi4d <> '*'
115700001120     C                   eval      vidfi4d = %subst(vidfitd:1:5) + '00F'
115800100308     C                   endif
115900001120     C                   else
116000001120     C                   movel     *blank        vidpgtd
116100001120     C                   movel     *blank        vidpgid
116200001120     C                   movel     *blank        vidfidd
116300001120     C                   movel     *blank        vidfisd
116400001120     C                   movel     *blank        vidfitd
116500001120     C                   movel     *blank        vidfi4d
116600001120     C                   endif
116700001120     C                   endif
116800001120     C*
116900001120     C                   endsr
117000991027     C*------------------------------------------------------------------------*
117100120109     C* CntTip234 - Controlla esistenza altri tipi file su tivt00f
117200991027     C*------------------------------------------------------------------------*
117300120109     C     CntTip234     BEGSR
117400120119     C*
117500120119     C* se ho indicato un cliente devo indicare il tipo file
117600120119     C                   IF        Kscs(i) <>*blank and
117700120119     C                             Kscs(i) <>*zeros and
117800120119     C                             Tips(i) = *blank
117900120119     C                   SELECT
118000120119     C                   WHEN      i = 1
118100120119     C                   SETON                                        7549
118200120119     C                   WHEN      i = 2
118300120119     C                   SETON                                        7650
118400120119     C                   WHEN      i = 3
118500120119     C                   SETON                                        7751
118600120119     C                   ENDSL
118700120119     C                   MOVEL     '1'           ERRL00
118800120119     C                   MOVEL     MSG(34)       DSMSMS
118900120119     C                   LEAVESR
119000120119     C                   ENDIF
119100010913     C*
119200120119     C* il tipo file / cliente indicati non devono essere quelli in gestione
119300120119     C                   IF        Tips(i) = VIDTip and
119400120119     C                             Kscs(i) = VIDKsc
119500120111     C                   SELECT
119600120111     C                   WHEN      i = 1
119700120119     C                   SETON                                        7549
119800120111     C                   WHEN      i = 2
119900120119     C                   SETON                                        7650
120000120111     C                   WHEN      i = 3
120100120119     C                   SETON                                        7751
120200120111     C                   ENDSL
120300120111     C                   MOVEL     '1'           ERRL00
120400120111     C                   MOVEL     MSG(33)       DSMSMS
120500120111     C                   LEAVESR
120600120111     C                   ENDIF
120700120111     C*
120800120111     C* il tipo file indicato deve esistere ed essere abilitato su questa anagrafica per il cliente
120900120119     C* indicato
121000120111     C* (per abilitato basta che la data fine validità sia maggiore di oggi; se anche la data inizio
121100120111     C*  è maggiore di oggi non importa, sarà valido nel futuro)
121200120119     C                   EVAL      x_VIRKsc = Kscs(i)
121300120111     C                   EVAL      x_VIRTip = Tips(i)
121400120111     C                   EVAL      x_VIRDtF = %dec(%date() : *ISO)
121500120111     C     K03VIR06      SETLL     TIVIR06L
121600120111     C     K02VIR06      READE     TIVIR06L
121700120111     C                   if        %eof
121800120109     C                   SELECT
121900120109     C                   WHEN      i = 1
122000120109     C                   SETON                                        75
122100120109     C                   WHEN      i = 2
122200120109     C                   SETON                                        76
122300120109     C                   WHEN      i = 3
122400120109     C                   SETON                                        77
122500120109     C                   ENDSL
122600120109     C                   MOVEL     '1'           ERRL00
122700120109     C                   MOVEL     MSG(31)       DSMSMS
122800120119     C* se il tipo file / cliente per duplica esiste,
122900120119     C* questo non deve contenere nelle sue coppie per duplica
123000120119     C* il tipo file / cliente in gestione
123100120111     C                   ELSE
123200120119     C*
123300120120     C                   if        TIS723R2U_Ctrl_Chain_Dup(
123400120120     C                              VIDTip       :
123500120120     C                              VIDKsc       :
123600120120     C                              Tips(i)      :
123700120120     C                              Kscs(i)      ) = '1'
123800120120     C                   SELECT
123900120120     C                   WHEN      i = 1
124000120120     C                   SETON                                        7549
124100120120     C                   WHEN      i = 2
124200120120     C                   SETON                                        7650
124300120120     C                   WHEN      i = 3
124400120120     C                   SETON                                        7751
124500120120     C                   ENDSL
124600120120     C                   MOVEL     '1'           ERRL00
124700120120     C                   MOVEL     MSG(32)       DSMSMS
124800120120     C                   ENDIF
124900120120     C***
125000120120     C***                MOVEL     x_VIRPTH      DVIR01
125100120120     C***                IF        §VIR01Tip2 = VIDTip and
125200120120     C***                          §VIR01Ksc2 = VIDKsc     or
125300120120     C***                          §VIR01Tip3 = VIDTip and
125400120120     C***                          §VIR01Ksc3 = VIDKsc     or
125500120120     C***                          §VIR01Tip4 = VIDTip and
125600120120     C***                          §VIR01Ksc4 = VIDKsc
125700120120     C***                SELECT
125800120120     C***                WHEN      i = 1
125900120120     C***                SETON                                        7549
126000120120     C***                WHEN      i = 2
126100120120     C***                SETON                                        7650
126200120120     C***                WHEN      i = 3
126300120120     C***                SETON                                        7751
126400120120     C***                ENDSL
126500120120     C***                MOVEL     '1'           ERRL00
126600120120     C***                MOVEL     MSG(32)       DSMSMS
126700120120     C***                ENDIF
126800120120     C***
126900120111     C                   ENDIF
127000120109     C*
127100120109     C                   ENDSR
127200120109     C*------------------------------------------------------------------------*
127300120109     C* cntksc - Controlla esistenza codice
127400120109     C*------------------------------------------------------------------------*
127500120109     C     cntksc        begsr
127600120109     C*
127700010913     C     vidksc        chain     tianv01l
127800010913     C                   if        %found
127900010913     C                   movel     ANVDES        vddksc
128000010913     C                   else
128100001120     C                   move      vidksc        wcli              7 0
128200991210     C                   CLEAR                   BS69DS
128300991210     C                   CLEAR                   ACODS
128400991210     C                   MOVEL     KNSIF         I69SIF
128500991210     C                   Z-ADD     WCLI          I69KAC
128600991210     C                   CALL      'TIBS69R'
128700991210     C                   PARM                    BS69DS
128800991210     C                   PARM                    ACODS
128900991210     C                   PARM                    INDDS
129000991210     C                   PARM                    CLPDS
129100991210     C                   PARM                    CLSDS
129200991210     C     O69ERR        IFNE      '1'
129300991210     C                   MOVEL     ACORAG        vddksc
129400991210     C                   ELSE
129500991210     C                   MOVEL     *ALL'*'       vddksc
129600010913     C                   MOVE      '1'           errl00
129700010913     C                   MOVEL     msg(11)       dsmsms
129800010913     C                   ENDIF
129900000727     C                   ENDIF
130000010913     C*
130100010913     C                   ENDSR
130200991027     C*------------------------------------------------------------------------*
130300040901     C* cntpoi - Controlla codice filiale
130400991027     C*------------------------------------------------------------------------*
130500001120     C     cntpoi        begsr
130600001120     C*
130700010529     C                   if        vidpoi <> '046' and
130800020809     C                             vidpoi <> '999' and
130900020902     C                             vidpoi <> '500' and
131000020902     C                             vidpoi <> '789'
131100001120     C*
131200001120     C                   move      *blank        vddpoi
131300001120     C     '?'           scan      vidpoi                                 97
131400001120     C                   if        *in97
131500001120     C                   movel     *blank        cod1
131600001120     C                   movel     *blank        des1
131700001120     C                   movel     *blank        pra1
131800001120     C                   call      'TNSD24R'
131900001120     C                   parm                    cod1              3
132000001120     C                   parm                    tip1              1
132100001120     C                   parm                    des1             25
132200001120     C                   parm                    pra1              3
132300001120     C                   movel     cod1          vidpoi
132400001120     C                   endif
132500001120     C                   move      vidpoi        cod1n             3 0
132600001120     C     cod1n         chain     azorg01l                           98
132700001120     C                   if        not *in98 and orgfag <>'V'
132800001120     C                   movel     orgdes        vddpoi
132900001120     C                   else
133000001120     C                   seton                                        69
133100001120     C                   move      '1'           errl00
133200001120     C                   movel     msg(13)       dsmsms
133300001120     C                   endif
133400001120     C*
133500001120     C                   else
133600001120     C                   movel     'SEDE BOLOGNA'vddpoi
133700001120     C                   endif
133800001120     C*
133900001120     C                   endsr
134000991210     C*------------------------------------------------------------------------*
134100991210     C* FINSR - OPERAZIONI FINALI
134200991210     C*------------------------------------------------------------------------*
134300991210     C     FINSR         BEGSR
134400991210     C*
134500991210     C* CHIUDE I FILE DEI PGM CHIAMATI
134600991210     C                   CLEAR                   bs69ds
134700991210     C                   CLEAR                   acods
134800991210     C                   CLEAR                   indds
134900991210     C                   CLEAR                   clpds
135000991210     C                   CLEAR                   clsds
135100991210     C                   MOVEL     'C'           i69tla
135200991210     C                   MOVEL     knsif         i69sif
135300991210     C                   CALL      'TIBS69R'
135400991210     C                   PARM                    bs69ds
135500991210     C                   PARM                    acods
135600991210     C                   PARM                    indds
135700991210     C                   PARM                    clpds
135800991210     C                   PARM                    clsds
135900991210     C*
136000991210     C                   ENDSR
136100991026     C*------------------------------------------------------------------------*
136200991026     C* CHKDA8 - CONTROLLA LA DATA (8)
136300991026     C*------------------------------------------------------------------------*
136400991026     C     CHKDA8        BEGSR
136500991026     C*
136600991026     C     *LIKE         DEFINE    VIDDti        WDA8
136700991026     C     *LIKE         DEFINE    VIDDti        DA8W
136800991026     C*
136900991026     C                   Z-ADD     WDA8          G08DAT
137000991026     C                   Z-ADD     *ZEROS        G08INV
137100991026     C                   MOVEL     '0'           G08ERR
137200991026     C                   CALL      'XSRDA8'
137300991026     C                   PARM                    WLBDA8
137400991026     C                   Z-ADD     G08DAT        WDA8
137500991026     C                   Z-ADD     G08INV        DA8W
137600991026IF  1C     G08ERR        IFEQ      '1'
137700991026     C                   MOVEL     '1'           ERRL00
137800991026E   1C                   ENDIF
137900991026     C*
138000991026     C                   ENDSR
138100970526     C*------------------------------------------------------------------------*
138200970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
138300970526     C*------------------------------------------------------------------------*
138400970526     C     IREVID        BEGSR
138500080604     C*
138600080604     C* REPERISCO INFO DA ESTENSIONE ANAGRAFICA CLIENTI/TIPI FILE => TRC 01
138700080604     C                   IF        §vire01INV = 'S'
138800080604     C                   EVAL      VIDEML = §vire01EML
138900080604     C                   ENDIF
139000970526     C* CAMPI
139100991026     C                   MOVEL     virtip        VIDtip
139200020218     C                   MOVE      virksc        vidksc
139300020218     C                   Z-ADD     virseq        vidseq
139400060308     C                   IF        DEPOP1 = 'O03'
139500060308     C                   MOVEL     *blanks       vidpoi
139600060308     C                   ELSE
139700060308     C                   MOVE      virpoi        vidpoi
139800060308     C                   ENDIF
139900991026     C                   MOVEL     virdes        VIDdes
140000991026     C                   MOVEL     virpgt        VIDpgt
140100991026     C                   MOVEL     virppt        VIDppt
140200991026     C                   MOVEL     virpgi        VIDpgi
140300991026     C                   MOVEL     virppi        VIDppi
140400991026     C                   MOVEL     virfid        VIDfid
140500991026     C                   MOVEL     virfis        VIDfis
140600991026     C                   MOVEL     virfit        VIDfit
140700000710     C                   MOVEL     virfi3        VIDfi3
140800000710     C                   MOVEL     virfi4        VIDfi4
140900010123     C                   MOVEL     virfi2        VIDfi2
141000030526     C                   MOVEL     virpth        DVIR01
141100030526     C                   MOVEL     §VIR01TM      VIDtmbd
141200120109     C                   MOVEL     §VIR01Tip2    VIDTip2
141300120109     C                   MOVEL     §VIR01Tip3    VIDTip3
141400120109     C                   MOVEL     §VIR01Tip4    VIDTip4
141500120119     C                   MOVEL     §VIR01Ksc2    VIDKsc2
141600120119     C                   MOVEL     §VIR01Ksc3    VIDKsc3
141700120119     C                   MOVEL     §VIR01Ksc4    VIDKsc4
141800000710  ***C*                  MOVEL     virctl        VIDctl
141900991026     C                   Z-ADD     virdti        G08INV
142000991026     C                   Z-ADD     *ZEROS        G08DAT
142100991026     C                   MOVEL     '3'           G08ERR
142200991026     C                   CALL      'XSRDA8'
142300991026     C                   PARM                    WLBDA8
142400991026     C                   Z-ADD     G08DAT        viddti
142500970527     C*
142600991026     C                   Z-ADD     virdtf        G08INV
142700991026     C                   Z-ADD     *ZEROS        G08DAT
142800991026     C                   MOVEL     '3'           G08ERR
142900991026     C                   CALL      'XSRDA8'
143000991026     C                   PARM                    WLBDA8
143100991026     C                   Z-ADD     G08DAT        viddtf
143200020218     C                   MOVEL     virfi1        vidisv
143300051122     C*
143400051122     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
143500051122     C                   IF        DEPOP1 = 'F06' OR
143600051122     C                             DEPOP1 = 'O03' OR
143700051122     C                             DEPOP1 = 'O07'
143800051122     C                   EVAL      VIDDTI = DATCORINV
143900060726     C                   EVAL      VIDDTF = 31122039
144000051122     C                   ENDIF
144100020218     C*
144200970526     C                   ENDSR
144300970526     C*------------------------------------------------------------------------*
144400970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
144500970526     C*------------------------------------------------------------------------*
144600970526     C     AZZVID        BEGSR
144700970526     C* CAMPI
144800991026     C                   MOVEL     *blank        VIDksc
144900000710  ***C*                  MOVEL     *blank        VIDpt1
145000000710  ***C*                  MOVEL     *blank        VIDpt2
145100010123     C                   z-add     0             vidseq
145200010123     C                   move      *zeros        vidpoi
145300060726     C                   EVAL      VIDDTI = DATCORINV
145400060726     C                   EVAL      VIDDTF = 31122039
145500991025     C                   MOVEL     *blank        VIDtip
145600991025     C                   MOVEL     *blank        VIDdes
145700991025     C                   MOVEL     *blank        VIDpgt
145800991025     C                   MOVEL     *blank        VIDppt
145900991025     C                   MOVEL     *blank        VIDpgi
146000991025     C                   MOVEL     *blank        VIDppi
146100991025     C                   MOVEL     *blank        VIDfid
146200991025     C                   MOVEL     *blank        VIDfis
146300991025     C                   MOVEL     *blank        VIDfit
146400000710     C                   MOVEL     *blank        VIDfi3
146500000710     C                   MOVEL     *blank        VIDfi4
146600010123     C                   MOVEL     *blank        VIDfi2
146700000710  ***C*                  MOVEL     *blank        VIDctl
146800080603     C                   MOVEL     *blank        VIDeml
146900970526     C*
147000970526     C                   ENDSR
147100970526     C*------------------------------------------------------------------------*
147200991026     C* IMPvir - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
147300970526     C*------------------------------------------------------------------------*
147400991026     C     IMPvir        BEGSR
147500970526     C*
147600991027     C                   move      VIDpoi        virpoi
147700991026     C                   movel     VIDdes        virdes
147800991026     C                   movel     VIDpgt        virpgt
147900991026     C                   movel     VIDppt        virppt
148000991026     C                   movel     VIDpgi        virpgi
148100991026     C                   movel     VIDppi        virppi
148200991026     C                   movel     VIDfid        virfid
148300991026     C                   movel     VIDfis        virfis
148400991026     C                   movel     VIDfit        virfit
148500000710     C                   movel     VIDfi3        virfi3
148600000710     C                   movel     VIDfi4        virfi4
148700010123     C                   movel     VIDfi2        virfi2
148800030526     C                   MOVEL     VIDtmbd       §VIR01TM
148900120109     C                   EVAL      §VIR01Tip2 =  VIDTip2
149000120109     C                   EVAL      §VIR01Tip3 =  VIDTip3
149100120109     C                   EVAL      §VIR01Tip4 =  VIDTip4
149200120119     C                   IF        VIDKsc2 = *zeros
149300120119     C                   EVAL      VIDKsc2 = *blank
149400120119     C                   ENDIF
149500120119     C                   IF        VIDKsc3 = *zeros
149600120119     C                   EVAL      VIDKsc3 = *blank
149700120119     C                   ENDIF
149800120119     C                   IF        VIDKsc4 = *zeros
149900120119     C                   EVAL      VIDKsc4 = *blank
150000120119     C                   ENDIF
150100120119     C                   EVAL      §VIR01Ksc2 =  VIDKsc2
150200120119     C                   EVAL      §VIR01Ksc3 =  VIDKsc3
150300120119     C                   EVAL      §VIR01Ksc4 =  VIDKsc4
150400030526     C                   MOVEL     DVIR01        virpth
150500000710  ***C*                  movel     VIDctl        virctl
150600991026     C                   Z-ADD     viddtf        G08dat
150700991026     C                   Z-ADD     *ZEROS        G08inv
150800991026     C                   MOVEL     '0'           G08ERR
150900991026     C                   CALL      'XSRDA8'
151000991026     C                   PARM                    WLBDA8
151100991026     C                   Z-ADD     G08inv        virdtf
151200000628     C                   movel     VIDisv        virfi1
151300970526     C*
151400970526     C                   ENDSR
151500970526     C*------------------------------------------------------------------------*
151600970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
151700970526     C*------------------------------------------------------------------------*
151800970526     C     IMPFUN        BEGSR
151900101013     C*
152000101026     C                   SETOFF                                       44
152100101026     C                   SETOFF                                       30
152200101026     C                   SETOFF                                       31
152300101026     C                   SETOFF                                       32
152400101026     C                   SETOFF                                       33
152500970526     C*
152600970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
152700970526IF  1C     DEPOP1        IFEQ      'O05'
152800970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
152900970526     C                   MOVEL     'N'           FUDATT
153000970526     C                   MOVEL     FUDRIG        FUD(6)
153100970526E   1C                   ENDIF
153200970526     C*
153300970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
153400970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
153500970526     C                   EXSR      FUNGES
153600970526     C*
153700970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
153800970526     C*---
153900970526     C* 90 -> PROTEZIONE CHIAVE
154000970526     C* 91 -> PROTEZIONE TOTALE
154100970526     C*---
154200970528IF  1C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
154300970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
154400970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
154500970526     C                   SETOFF                                       91
154600970526E   2C                   ENDIF
154700101013IF  2C     DEPOP1        IFEQ      'O03'                                        *INSERIMENTO
154800101013E   2C                   ENDIF
154900970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
155000991014IF  2C     DEPOP1        oreq      'O01'                                        *MODIFICA
155100970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
155200970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
155300970526E   2C                   ENDIF
155400970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
155500970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
155600970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
155700970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
155800970526     C                   SETON                                        91
155900970526E   2C                   ENDIF
156000970526X   1C                   ELSE                                                   *I N T E R R O G A
156100970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
156200970526     C                   SETON                                        91
156300970526E   1C                   ENDIF
156400970526     C*
156500970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
156600970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
156700991025     C                   MOVEL     S20tip        KCEtip
156800991026     C                   MOVEL     S20ksc        KCEksc
156900000420     C     KEYvi2        CHAIN     tivir03L                           9998
157000080603IF  2C     *IN99         IFEQ      *ON
157100970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
157200970526     C                   MOVEL     MSG(6)        DSMSMS
157300970526X   2C                   ELSE
157400080603     C*
157500080603     C* Aggancio anche il record d estensione regole clienti/tipi-file => TRC 01
157600080603     C                   EVAL      KCEdti  = virDTI
157700080603     C                   EVAL      vireTRC = '01'
157800080603     C     KEYvire1_C    CHAIN     tivire1l                           9998
157900080603     C                   IF        %found(tivire1l)
158000080603     C                   EVAL      DVIRE01 = vireFLO
158100080603     C                   ENDIF
158200080603     C*
158300080603     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
158400980128IF  3C     DEPOP1        IFEQ      'O05'
158500991014IF  3C     DEPOP1        oreq      'O02'
158600101026     C*
158700101026     C* Se trattasi d tipo servizio "idoneo" reperisco anche i dati utente lato "BARTVAS"
158800101026     C     virFI1        lookup    isvftpeml                              80
158900101026     C                   IF        *IN80
159000111215     C                             and AS400_ok = *on
159100101026      /FREE
159200101026
159300101026         if UBBVSQL01_RtvCliTipFile(
159400101026                         pInConn
159500101026                       : virKSC
159600101026                       : virTIP
159700101026                       : pOutNumRec
159800101026                       : pOutCliTipTras
159900101026                       : pOutCliMskChk
160000101026                       : pOutCliMskFil
160100101026                       : pOutCliMskSem
160200101026                       : pOutCliPerc
160300101026                       : pOutCliEml1
160400101026                       : pOutCliEml2
160500101026                       : pOutCliEml3
160600101026                       : pOutCliEml4
160700101026                       : pOutCliEml5
160800101026                       : pOutCliSogg
160900101026                       : pOutCliTesto
161000101026                       : pOutCliFtpAddr
161100101026                       : pOutCliFtpDir
161200101026                       : pOutCliFtpUsr
161300101026                       : pOutCliFtpPwd
161400101026                       : pOutCliFtpFAS4
161500101026                       : pOutCliPercOld
161600101026                       : pOutCliTentat
161700101026                       : pOutCliRitardo
161800101026                       : pOutCliLibAS4
161900101026                       : pOutCliFilAS4
162000101026                       : pOutCliMbrAS4
162100101026                       : pOutCliProc
162200101026                       : pOutCliSched
162300101026                       : pOutCliAccorp
162400101026                       : pOutCliMskZip
162500101026                       : pOutCliDataStr
162600101026                       : pOutCliDataEnd
162700101026                       : pOutCliCodComp
162800101026                       : pOutCliFtpCmd1
162900101026                       : pOutCliEmlErr
163000101026                       : pOutCliHtUrlTo
163100101026                       : pOutCliHtUsrTo
163200101026                       : pOutCliHtPwdTo
163300101026                       : pOutCliHtConTy
163400101026                       : pOutCliHtSndEx
163500101026                       : pOutCliHtSndPr
163600101026                       : pOutCliHtUsPxy
163700101026                       : pOutCliUsrIns
163800101026                       : pOutCliUsrUpd
163900101026                       : pOutCliDatUpd
164000101026                       : pOutCliNote
164100101026                       : pOutCliFtpPrt
164200101026                       : pOutCliFtpTy) = 0;
164300101026              if pOutNumRec = 1;
164400101026                 VIDPPIMC = pOutCliMskChk;
164500101026                 if %len(%trim(pOutCliMskChk)) > %size(VIDPPIMC);
164600101026                    *in30 = *on;
164700101026                 endif;
164800101026                 VIDPPIMF = pOutCliMskFil;
164900101026                 if %len(%trim(pOutCliMskFil)) > %size(VIDPPIMF);
165000101026                    *in31 = *on;
165100101026                 endif;
165200101026                 VIDPPIMS = pOutCliMskSem;
165300101026                 if %len(%trim(pOutCliMskSem)) > %size(VIDPPIMS);
165400101026                    *in32 = *on;
165500101026                 endif;
165600101026                 VIDPPIMZ = pOutCliMskZip;
165700101026                 if %len(%trim(pOutCliMskZip)) > %size(VIDPPIMZ);
165800101026                    *in33 = *on;
165900101026                 endif;
166000101026              endif;
166100101026         endif;
166200101026
166300101026      /END-FREE
166400101026     C
166500101026     C                   ELSE
166600101026     C                   SETON                                        44
166700101026     C                   ENDIF
166800101026     C*
166900980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
167000980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
167100980128E   3C                   ENDIF
167200970526E   2C                   ENDIF
167300970526X   1C                   ELSE                                                   *INSERIMENTO
167400970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
167500970526E   1C                   ENDIF
167600970526     C*
167700970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
167800970526IF  1C     DEPOP1        IFEQ      'O02'
167900970526     C                   MOVEL     DEC(1)        VIDFUN
168000970526E   1C                   ENDIF
168100991014IF  1C     DEPOP1        IFEQ      'O03'
168200991014     C                   MOVEL     DEC(6)        VIDFUN
168300991014E   1C                   ENDIF
168400970526IF  1C     DEPOP1        IFEQ      'O04'
168500970526     C                   MOVEL     DEC(2)        VIDFUN
168600970526E   1C                   ENDIF
168700970526IF  1C     DEPOP1        IFEQ      'O05'
168800970526     C                   MOVEL     DEC(3)        VIDFUN
168900970526E   1C                   ENDIF
169000970526IF  1C     DEPOP1        IFEQ      'O07'
169100970526     C                   MOVEL     DEC(4)        VIDFUN
169200970526E   1C                   ENDIF
169300970526IF  1C     DEPOP1        IFEQ      'F06'
169400970526     C                   MOVEL     DEC(5)        VIDFUN
169500970526E   1C                   ENDIF
169600970526     C*
169700970526     C                   ENDSR
169800101026
169900101026
170000101026
170100101026     C*--------------------------------------------------------------------------------------------*
170200101026     C* CARTAB - carica le tabelle occorrenti
170300101026     C*--------------------------------------------------------------------------------------------*
170400101026     C     CARTAB        BEGSR
170500101026     C*
170600101026     C* Tipi servizi soggetti a controllo unificazioni
170700101026     C                   z-add     *zeros        ww                3 0
170800101026     C                   eval      tbecod = 'ISV'
170900101026     C     tbecod        setll     tntbe01l
171000101026     C     tbecod        reade     tntbe01l
171100101026     C                   dow       not %eof(tntbe01l)
171200101026     C                   movel     tbeuni        disv
171300101026     C                   if        §isvproc = 'F'
171400101026     C                   add       1             ww
171500101026     C                   eval      isvftpeml(ww) = tbeke1
171600101026     C                   endif
171700101026     C     tbecod        reade     tntbe01l
171800101026     C                   enddo
171900101026     C*
172000101026     C                   ENDSR
172100101026
172200101026     c*------------------------------------------------------------------------*
172300101026     c* opnCONN - Apertura connessione
172400101026     c*------------------------------------------------------------------------*
172500101026     c     OPNCONN       BEGSR
172600101026      /FREE
172700101026
172800101026        if not *in40;
172900101026           if UBBVSQL01_Connect('BARTVAS' : pOutConn) = 0;
173000101026              pInConn = pOutConn;
173100101026           else;
173200101026              *in40 = *on;
173300101026              DSMSMS = MSG(28);
173400101026           endif;
173500101026        endif;
173600101026
173700101026      /END-FREE
173800101026      *
173900101026     c                   ENDSR
174000101026     c*------------------------------------------------------------------------*
174100101026
174200101026
174300101026
174400101026     c*------------------------------------------------------------------------*
174500101026     c* clsCONN - Chiusura connessione
174600101026     c*------------------------------------------------------------------------*
174700101026     c     CLSCONN       BEGSR
174800101026      /FREE
174900101026
175000101026           if UBBVSQL01_Disconnect(pInConn) = 0;
175100101026           else;
175200101026              *in40 = *on;
175300101026           endif;
175400101026
175500101026      /END-FREE
175600101026      *
175700101026     c                   ENDSR
175800101026     c*------------------------------------------------------------------------*
175900101026
176000101026
176100101026
176200970526     C*------------------------------------------------------------------------*
176300970526     C* *INZSR - OPERAZIONI INIZIALI
176400970526     C*------------------------------------------------------------------------*
176500970526     C     *INZSR        BEGSR
176600970526     C*---
176700970526     C* RICEVIMENTO PARAMETRI
176800970526     C*---
176900970526     C     *ENTRY        PLIST
177000970612     C                   PARM                    KPJBA
177100991028     C                   movel     kpjbu         tis720
177200000420     C*
177300000420     C* Inverto subito la data presa in input
177400000420     C                   Z-ADD     s20dti        wda8
177500000420     C                   Z-ADD     *ZEROS        da8w
177600000420     C                   EXSR      CHKDA8
177700000420     C                   Z-ADD     DA8W          KCEdti
177800970526     C*
177900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
178000970526     C     XMSGPA        PLIST
178100970526     C                   PARM                    DSMSMS
178200970526     C                   PARM                    DSMSPG
178300970526     C                   PARM                    DSMSKE
178400970526     C                   PARM                    DSMSER
178500991108     C*---
178600970526     C*
178700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
178800970526     C                   CALL      'XKEY'
178900970526     C                   PARM                    DSKEY
179000101026     C*
179100101026     C* CARICA LE TABELLE OCCORRENTI
179200101027     C                   EXSR      CARTAB
179300111215     C*
179400111215     C* Reperisco il sistema AS/400 corrente
179500111215     C                   EVAL      AS400_ok = *off
179600111215     C                   callp     UBRTVNETA_Rtv(currSysNeta)
179700111215     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
179800111215     C                   EVAL      AS400_ok = *on
179900111215     C                   ENDIF
180000101026     C*
180100101026     C* Apro la connessione al DB "BARTVAS"
180200111215     C                   IF        AS400_ok = *on
180300111215     C                   exsr      opnCONN
180400111215     C                   ENDIF
180500970526     C*---
180600970526     C* VARIABILI NON RIFERITE AL DATA BASE
180700970526     C*---
180800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
180900991025     C     *LIKE         DEFINE    S20OP0        DEPOP0
181000991025     C     *LIKE         DEFINE    S20OP1        DEPOP1
181100991025     C     *LIKE         DEFINE    S20OP0        WOP0
181200991025     C     *LIKE         DEFINE    S20OP1        WOP1
181300970526     C*---
181400970526     C* VARIABILI DI CONTROLLO
181500970526     C*---
181600970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
181700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
181800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
181900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
182000970526     C*
182100970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
182200991026     C                   MOVEL     'TIS723R'     DSMSPG
182300991026     C                   MOVEL     'TIS723R'     MSGQUE
182400970526     C                   MOVEL     *BLANKS       MSGKEY
182500970526     C                   MOVEL     *BLANKS       DSMSMS
182600970526     C*---
182700970526     C* VARIABILI RIFERITE AL DATA BASE
182800970526     C*---
182900991026     C     *LIKE         DEFINE    virtip        KCEtip
183000991026     C     *LIKE         DEFINE    virksc        KCEksc
183100000420     C     *LIKE         DEFINE    virdti        KCEdti
183200970526     C*---
183300970526     C* CHIAVI DI LETTURA
183400970526     C*---
183500000420     C* LETTURA tivir03L - INCOMPLETA
183600991026     C     KEYvir        KLIST
183700080603     C                   KFLD                    KCEtip
183800080603     C                   KFLD                    KCEksc
183900080603     C* LETTURA tivir03L - COMPLETA
184000080603     C     KEYvi2        KLIST
184100080603     C                   KFLD                    KCEtip
184200080603     C                   KFLD                    KCEksc
184300080603     C                   KFLD                    KCEdti
184400080603     C* LETTURA tivire1L - COMPLETA
184500080603     C     KEYvire1_C    KLIST
184600080603     C                   KFLD                    KCEksc
184700080603     C                   KFLD                    KCEtip
184800080603     C                   KFLD                    KCEdti
184900080603     C                   KFLD                    vireTRC
185000080603     C* LETTURA tntbe01L - PARZIALE
185100040816     C     KEYtbe1_P     KLIST
185200040816     C                   KFLD                    TBEcod                         *CODICE TABELLA
185300120111     C* LETTURA tivir06L - COMPLETA
185400120111     C     K03VIR06      KLIST
185500120111     C                   KFLD                    x_VIRKsc
185600120111     C                   KFLD                    x_VIRTip
185700120111     C                   KFLD                    x_VIRDtF
185800120111     C* LETTURA tivir06L - PARZIALE
185900120111     C     K02VIR06      KLIST
186000120111     C                   KFLD                    x_VIRKsc
186100120111     C                   KFLD                    x_VIRTip
186200970528     C*
186300970528     C* CALCOLA LA DATA CORRENTE
186400970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
186500970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
186600970528     C                   Z-ADD     WN8           G08DAT
186700051122     C                   Z-ADD     WN8           DATCORINV         8 0
186800970528     C                   Z-ADD     *ZEROS        G08INV
186900970528     C                   MOVEL     '0'           G08ERR
187000970528     C                   CALL      'XSRDA8'
187100970528     C                   PARM                    WLBDA8
187200970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
187300970526     C*
187400970526     C* DEPOSITA LE OPZIONI DI ENTRATA
187500991025     C                   MOVEL     S20OP0        DEPOP0                         *LIVELLO PROCEDURA
187600991025     C                   MOVEL     S20OP1        DEPOP1                         *LIVELLO PROGRAMMA
187700051122     C*
187800051122     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
187900051122     C                   IF        DEPOP1 = 'F06' OR
188000051122     C                             DEPOP1 = 'O03' OR
188100051122     C                             DEPOP1 = 'O07'
188200051122     C                   EVAL      VIDDTI = DATCORINV
188300060726     C                   EVAL      VIDDTF = 31122039
188400051122     C                   ENDIF
188500000629     C*
188600000629     C* EFFETTUO CONSIDERAZIONI SULLE OPZIONI DI ENTRATA
188700000629     C                   IF        DEPOP1 = 'F06' OR
188800000629     C                             DEPOP1 = 'O03'
188900000629     C                   SETOFF                                       92
189000000629     C                   ELSE
189100000629     C                   SETON                                        92
189200000629     C                   ENDIF
189300970526     C*
189400970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
189500970526     C                   EXSR      IMPFUN
189600970526     C*
189700970526     C                   ENDSR
189800970526     C*------------------------------------------------------------------------*
189900970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
190000970526     C*------------------------------------------------------------------------*
190100970526     C     FUNGES        BEGSR
190200970526     C*
190300970526     C* PULISCO CAMPI DI LAVORO
190400970526     C                   Z-ADD     0             W                 5 0
190500970526     C*
190600970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
190700970526     C* E CARICO TABELLA FUNZIONI ATTIVE
190800970526     C     1             DO        24            I
190900970526     C                   MOVEL     FUD(I)        FUDRIG
191000970526     C*
191100970526     C* PULIZIA FUNZIONI PRECEDENTI
191200970526     C                   MOVEL     *BLANKS       FUV(I)
191300970526     C*
191400970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
191500970526     C* (SALTA F24)
191600970526     C     FUDATT        IFEQ      'S'
191700970526     C     FUDFUN        ANDNE     '24'
191800970526     C                   ADD       1             W
191900970526     C                   MOVEL     FUDDES        FUV(W)
192000970526     C                   END
192100970526     C                   END                                                    DO WHILE
192200970526     C*
192300970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
192400970526     C* ALTRIMENTI DISATTIVO F24
192500970526     C     1             DO        24            I
192600970526     C                   MOVEL     FUD(I)        FUDRIG
192700970526     C*
192800970526     C     FUDFUN        IFEQ      '24'
192900970526     C     W             IFGT      6
193000970526     C                   MOVEL     'S'           FUDATT
193100970526     C                   ELSE
193200970526     C                   MOVEL     *BLANKS       FUDATT
193300970526     C                   END                                                    W GT 8
193400970526     C                   MOVEL     FUDRIG        FUD(I)
193500970526     C                   END
193600970526     C                   END                                                    DO WHILE
193700970526     C*
193800970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
193900970526     C     W             DIV       6             I
194000970526     C                   MVR                     Z
194100970526     C*
194200970526     C     Z             IFNE      0
194300970526     C     I             ADD       1             Z
194400970526     C                   ELSE
194500970526     C                   Z-ADD     I             Z                 5 0
194600970526     C                   END
194700970526     C*
194800970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
194900970526     C                   ADD       1             FUNGRU
195000970526     C     FUNGRU        IFGT      Z
195100970526     C     FUNGRU        ORLT      1
195200970526     C                   Z-ADD     1             FUNGRU
195300970526     C                   END
195400970526     C*
195500970526     C* PRIMO GRUPPO DI 6 FUNZIONI
195600970526     C     FUNGRU        IFEQ      1
195700970526     C                   MOVEL     FUV(1)        FUN001
195800970526     C                   MOVEL     FUV(2)        FUN002
195900970526     C                   MOVEL     FUV(3)        FUN003
196000970526     C                   MOVEL     FUV(4)        FUN004
196100970526     C                   MOVEL     FUV(5)        FUN005
196200970526     C                   MOVEL     FUV(6)        FUN006
196300970526     C                   END
196400970526     C*
196500970526     C* SECONDO GRUPPO DI 6 FUNZIONI
196600970526     C     FUNGRU        IFEQ      2
196700970526     C                   MOVEL     FUV(7)        FUN001
196800970526     C                   MOVEL     FUV(8)        FUN002
196900970526     C                   MOVEL     FUV(9)        FUN003
197000970526     C                   MOVEL     FUV(10)       FUN004
197100970526     C                   MOVEL     FUV(11)       FUN005
197200970526     C                   MOVEL     FUV(12)       FUN006
197300970526     C                   END
197400970526     C*
197500970526     C* TERZO GRUPPO DI 6 FUNZIONI
197600970526     C     FUNGRU        IFEQ      3
197700970526     C                   MOVEL     FUV(13)       FUN001
197800970526     C                   MOVEL     FUV(14)       FUN002
197900970526     C                   MOVEL     FUV(15)       FUN003
198000970526     C                   MOVEL     FUV(16)       FUN004
198100970526     C                   MOVEL     FUV(17)       FUN005
198200970526     C                   MOVEL     FUV(18)       FUN006
198300970526     C                   END
198400970526     C*
198500970526     C* QUARTO GRUPPO DI 6 FUNZIONI
198600970526     C     FUNGRU        IFEQ      4
198700970526     C                   MOVEL     FUV(19)       FUN001
198800970526     C                   MOVEL     FUV(20)       FUN002
198900970526     C                   MOVEL     FUV(21)       FUN003
199000970526     C                   MOVEL     FUV(22)       FUN004
199100970526     C                   MOVEL     *BLANKS       FUN005
199200970526     C                   MOVEL     *BLANKS       FUN006
199300970526     C                   END
199400970526     C*
199500970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
199600970526     C                   MOVEL     *BLANKS       FUN007
199700970526     C                   MOVEL     *BLANKS       FUN008
199800970526     C*
199900970526     C     1             DO        24            I
200000970526     C                   MOVEL     FUD(I)        FUDRIG
200100970526     C     FUDFUN        IFEQ      '24'
200200970526     C     FUDATT        ANDEQ     'S'
200300970526     C                   MOVEL     FUDDES        FUN008
200400970526     C                   END
200500970526     C                   END                                                    DO
200600970526     C*
200700970526     C                   ENDSR
200800970526     C*------------------------------------------------------------------------*
200900970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
201000970526     C*------------------------------------------------------------------------*
201100970526     C     FUNCON        BEGSR
201200970526     C*
201300970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
201400970526     C                   EXSR      FUNVAR
201500970526     C*
201600970526     C* PULISCO CAMPI DI LAVORO
201700970526     C                   Z-ADD     0             I                 5 0
201800970526     C                   Z-ADD     0             W                 5 0
201900970526     C*
202000970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
202100970526     C     FUNKEY        IFNE      *BLANKS
202200970526     C     1             DO        24            I
202300970526     C                   MOVEL     FUD(I)        FUDRIG
202400970526     C*
202500970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
202600970526     C     FUNKEY        IFEQ      FUDFUN
202700970526     C     FUDATT        ANDEQ     'S'
202800970526     C                   Z-ADD     I             W
202900970526     C                   END
203000970526     C                   END                                                    DO WHILE
203100970526     C*
203200970526     C* FUNZIONE NON ATTIVA (ERRORE)
203300970526     C     W             IFEQ      0
203400970526     C*
203500970526     C* EMISSIONE MESSAGGIO DI ERRORE
203600970526     C                   MOVEL     MSG(1)        DSMSMS
203700970526     C*
203800970526     C                   END                                                    W=0
203900970526     C                   END                                                    FUNKEY NE *BLANKS
204000970526     C*
204100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
204200970526     C     W             IFNE      0
204300970526     C     FUNKEY        OREQ      *BLANKS
204400970526     C     FUNKEY        IFEQ      *BLANKS
204500970526     C                   EXSR      ENTGES
204600970526     C                   END
204700031229     C     FUNKEY        IFEQ      '01'
204800031229     C                   EXSR      F01GES
204900031229     C                   END
205000970526     C     FUNKEY        IFEQ      '03'
205100970526     C                   EXSR      F03GES
205200970526     C                   END
205300970526     C     FUNKEY        IFEQ      '06'
205400970526     C                   EXSR      F06GES
205500970526     C                   END
205600970526     C     FUNKEY        IFEQ      '12'
205700970526     C                   EXSR      F12GES
205800970526     C                   END
205900970526     C     FUNKEY        IFEQ      '24'
206000970526     C                   EXSR      F24GES
206100970526     C                   END
206200970526     C                   END
206300970526     C*
206400970526     C                   ENDSR
206500970526     C*------------------------------------------------------------------------*
206600970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
206700970526     C*------------------------------------------------------------------------*
206800970526     C     FUNVAR        BEGSR
206900970526     C*
207000970526     C* INIZIALIZZA VARIABILE -FUNKEY-
207100970526     C*
207200970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
207300970526     C     KEY           IFEQ      ENTER
207400970526     C                   MOVEL     *BLANKS       FUNKEY
207500970526     C                   END
207600970526     C     KEY           IFEQ      F1
207700970526     C                   MOVEL     '01'          FUNKEY
207800970526     C                   END
207900970526     C     KEY           IFEQ      F2
208000970526     C                   MOVEL     '02'          FUNKEY
208100970526     C                   END
208200970526     C     KEY           IFEQ      F3
208300970526     C                   MOVEL     '03'          FUNKEY
208400970526     C                   END
208500970526     C     KEY           IFEQ      F4
208600970526     C                   MOVEL     '04'          FUNKEY
208700970526     C                   END
208800970526     C     KEY           IFEQ      F5
208900970526     C                   MOVEL     '05'          FUNKEY
209000970526     C                   END
209100970526     C     KEY           IFEQ      F6
209200970526     C                   MOVEL     '06'          FUNKEY
209300970526     C                   END
209400970526     C     KEY           IFEQ      F7
209500970526     C                   MOVEL     '07'          FUNKEY
209600970526     C                   END
209700970526     C     KEY           IFEQ      F8
209800970526     C                   MOVEL     '08'          FUNKEY
209900970526     C                   END
210000970526     C     KEY           IFEQ      F9
210100970526     C                   MOVEL     '09'          FUNKEY
210200970526     C                   END
210300970526     C     KEY           IFEQ      F10
210400970526     C                   MOVEL     '10'          FUNKEY
210500970526     C                   END
210600970526     C     KEY           IFEQ      F11
210700970526     C                   MOVEL     '11'          FUNKEY
210800970526     C                   END
210900970526     C     KEY           IFEQ      F12
211000970526     C                   MOVEL     '12'          FUNKEY
211100970526     C                   END
211200970526     C     KEY           IFEQ      F13
211300970526     C                   MOVEL     '13'          FUNKEY
211400970526     C                   END
211500970526     C     KEY           IFEQ      F14
211600970526     C                   MOVEL     '14'          FUNKEY
211700970526     C                   END
211800970526     C     KEY           IFEQ      F15
211900970526     C                   MOVEL     '15'          FUNKEY
212000970526     C                   END
212100970526     C     KEY           IFEQ      F16
212200970526     C                   MOVEL     '16'          FUNKEY
212300970526     C                   END
212400970526     C     KEY           IFEQ      F17
212500970526     C                   MOVEL     '17'          FUNKEY
212600970526     C                   END
212700970526     C     KEY           IFEQ      F18
212800970526     C                   MOVEL     '18'          FUNKEY
212900970526     C                   END
213000970526     C     KEY           IFEQ      F19
213100970526     C                   MOVEL     '19'          FUNKEY
213200970526     C                   END
213300970526     C     KEY           IFEQ      F20
213400970526     C                   MOVEL     '20'          FUNKEY
213500970526     C                   END
213600970526     C     KEY           IFEQ      F21
213700970526     C                   MOVEL     '21'          FUNKEY
213800970526     C                   END
213900970526     C     KEY           IFEQ      F22
214000970526     C                   MOVEL     '22'          FUNKEY
214100970526     C                   END
214200970526     C     KEY           IFEQ      F23
214300970526     C                   MOVEL     '23'          FUNKEY
214400970526     C                   END
214500970526     C     KEY           IFEQ      F24
214600970526     C                   MOVEL     '24'          FUNKEY
214700970526     C                   END
214800970526     C     KEY           IFEQ      ROLLUP
214900970526     C                   MOVEL     'RU'          FUNKEY
215000970526     C                   END
215100970526     C     KEY           IFEQ      HELP
215200970526     C                   MOVEL     'HP'          FUNKEY
215300970526     C                   END
215400970526     C                   ENDSR
215500970526     C*------------------------------------------------------------------------*
215600970526     C* NOTE COSTRUZIONE SCHIERE
215700970526     C*------------------------------------------------------------------------*
215800970526     C*
215900970526     C* -MSG-MESSAGGI
216000970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
216100970526     C* (NON TOCCARE MSG,1 E MSG,2)
216200970526     C*
216300970526     C* -CMD-COMANDI
216400970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
216500970526     C*
216600970526     C* -OPD-OPZIONI
216700970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
216800970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
216900970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
217000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
217100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
217200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
217300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
217400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
217500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
217600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
217700970526     C*
217800970526     C* -FUD-FUNZIONI
217900970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
218000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
218100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
218200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
218300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
218400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
218500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
218600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
218700970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
218800970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
218900970526     O*------------------------------------------------------------------------*
219000970526** DEC - DECODIFICHE
219100970526    MODIFICA                                                      1
219200970526  ANNULLAMENTO                                                    2
219300970526VISUALIZZAZIONE                                                   3
219400970526   RIPRISTINO                                                     4
219500970526  INSERIMENTO                                                     5
219600991014     COPIA                                                        6
219700970526** MSG - MESSAGGI
219800991026TIS723R- Tasto funzionale non gestito                                          1
219900991026TIS723R- Inserimento effettuato                                                2
220000991026TIS723R- Modifica effettuata                                                   3
220100991026TIS723R- Annullamento effettuato                                               4
220200991026TIS723R- Ripristino effettuato                                                 5
220300991026TIS723R- Richiesta funzione per record inesistente                             6
220400991026TIS723R- Record momentaneamente allocato: riprovare più tardi                  7
220500991026TIS723R- Richiesta funzione per record esistente                               8
220600991026TIS723R- Questo codice cliente è già presente con questo tipo file             9
220700991027TIS723R- Il codice tipo file non è fra quelli presenti in anagrafico           10
220800991027TIS723R- Il codice cliente non è presente come padre nell'archivio UNIFICANTI  11
220900000724TIS723R- Campo obbligatorio                                                    12
221000991027TIS723R- Codice filiale inesistente o di categoria V=varia                     13
221100000724TIS723R- Data errata                                                           14
221200000724TIS723R- La data fine deve essere maggiore della data inizio                   15
221300000724TIS723R- Il record non può scadere in un giorno già trascorso                  16
221400000724TIS723R- Il record immesso si sovrappone ad uno già in decorrenza              17
221500000724TIS723R- Codice tipo servizio non presente in tabella                          18
221600001120TIS723R- Oggetto inesistente, verificare e correggere.                         19
221700001120TIS723R- File inesistente, verificare e correggere.                            20
221800001120TIS723R- Libreria inesistente, verificare e correggere.                        21
221900030603TIS723R- Tipo membro non impostabile x il tipo servizio indicato.              22
222000040107TIS723R- Per il traduttore indicato non esiste descrizione dettaglio parametri 23
222100040319TIS723R- Tipo file e servizio incompatibili, correggere.                       24
222200040816TIS723R- Tipo file e/o programma traduttore e/o file tradotto incompatibili.   25
222300051026TIS723R- ATTENZIONE presenti log a stato 0 impossibile cambiare traduttore!    26
222400060616TIS723R- Valori incompatibili, correggere!                                     27
222500101026TIS723R- Errore BartVAS: fatal error                                           28
222600101027TIS723R- ATTENZIONE: modifica effettuata lato AS/400 ma non lato BARTVAS       29
222700120109TIS723R- Tipo file per Internet non impostabile x il tipo servizio indicato.   30
222800120119TIS723R- Tipo file non abilitato per cliente indicato                          31
222900120119TIS723R- Loop tra il tipo file / cliente corrente e quello per duplica         32
223000120119TIS723R- Tipo file / cliente per duplica non possono essere quelli correnti    33
223100120119TIS723R- Valorizzato il cliente per duplica ma non il rispettivo tipo file     34
223200970526** FUD - FUNZIONI
223300031229F01=Parametri      -        (S/N)=S  <== S-FISSO           funz.01¬
223400970526                   -        (S/N)=N                        funz.02¬
223500970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
223600970526                   -        (S/N)=N                        funz.04¬
223700970526                   -        (S/N)=N                        funz.05¬
223800970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
223900970527                   -        (S/N)=N                        funz.07¬
224000970527                   -        (S/N)=N                        funz.08¬
224100970527                   -        (S/N)=N                        funz.09¬
224200970527                   -        (S/N)=N                        funz.10¬
224300970526                   -        (S/N)=N                        funz.11¬
224400970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
224500970526                   -        (S/N)=N                        funz.13¬
224600970526                   -        (S/N)=N                        funz.14¬
224700970526                   -        (S/N)=N                        funz.15¬
224800970526                   -        (S/N)=N                        funz.16¬
224900970526                   -        (S/N)=N                        funz.17¬
225000970526                   -        (S/N)=N                        funz.18¬
225100970526                   -        (S/N)=N                        funz.19¬
225200970526                   -        (S/N)=N                        funz.20¬
225300970526                   -        (S/N)=N                        funz.21¬
225400970526                   -        (S/N)=N                        funz.22¬
225500970526                   -        (S/N)=N                        funz.23¬
225600970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
