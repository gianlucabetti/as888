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
006700130215     D*------------
006800130215     D* Dati utente/azienda
006900130215     D*------------
007000130215     D TIBS34DS      E DS                                                       *Profili utente
007100130215     D DDATIUTE      E DS                                                       *Dati utente
007200130215     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007300970528     D*------------------
007400970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
007500970528     D*------------------
007600970528     D WLBDA8          DS                  INZ
007700970528     D  G08DAT                 1      8  0
007800970528     D  G08INV                 9     16  0
007900970528     D  G08ERR                17     17
008000970528     D  G08TGI                18     22  0
008100020218     D*------------
008200020218     D* DS "ITF" - TABELLA SCAMBIO TIPI FILE
008300020218     D*------------
008400020218     D DITF          E DS
008500970526     D*------------
008600970526     D* DS DI PROCEDURA
008700970526     D*------------
008800991025     D tis720        E DS                  EXTNAME(tis720ds)
008900030526     D DVIR01        E DS
009000030603     D DISV          E DS
009100040816     D DIFT          E DS
009200101013     D DVTFPPI       E DS
009300120109     D TIVTF00F      E DS                  extname(TIVTF00F)
009400120109     D TIVTF_Save      DS           300
009500970526     D*------------
009600970526     D* ARCHITETTURA
009700970526     D*------------
009800970526     D KPJBA         E DS
009900080603     D*------------
010000080603     D* ESTENSIONE ANAGRAFICA CLIENTI/TIPI FILE
010100080603     D*------------
010200080603     D DVIRE01       E DS
010300040816     D*------------
010400040816     D* DS REPERIMENTO DATI CLIENTI
010500040816     D*------------
010600040816     DBS69DS         e ds                  extname(TIBS69DS)
010700040816     DACODS          e ds                  extname(cnaco00f)
010800040816     DINDDS          e ds                  extname(cnind00f)
010900040816     DCLPDS          e ds                  extname(cnclp00f)
011000040816     DCLSDS          e ds                  extname(fncls00f)
011100081020
011200081020     D*------------
011300081020     D* DS x richiamo *pgm utilità d controllo indirizzi e-mail
011400081020     D*------------
011500081020     D DSEMAIL       E DS
011600040816     D*------------
011700040816     D* VARIABILI D WRK
011800040816     D*------------
011900040816     D ksc             s              7  0 dim(30)
012000040816     D ksa             s              4    dim(30)
012100101026     D isvftpeml       s              2    dim(100)
012200040816     D skTipF          s              2    dim(500)
012300040816     D skPgt           s             10    dim(500)
012400040816     D skFileT         s             10    dim(500)
012500100311     D skMembroT       s             10    dim(500)
012600040816     D j               s              3  0 inz
012700040816     D wVIDPGT         s                   like(VIDPGT)
012800040816     D wVIDFIT         s                   like(VIDFIT)
012900120109     D MaxCiclo        s              3  0 inz
013000120119     D* Tips e Ksc DEVONO avere lo stesso numero di elementi
013100120109     D Tips            s              2    dim(3)
013200120119     D Kscs            s              8    dim(3)
013300111215     D*------------------
013400111215     D* Variabili x PASSAGGIO PARAMETRI A PROCEDURE UBRTVNETA
013500111215     D*------------------
013600111215     D currSysNeta     s              8A   inz('*NULL')
013700111215     D AS400_ok        s              1A
013800111215     D  PiOutSysNeta   S              8A
013900101026     D*-------------------
014000101026     D* DEFINIZIONI PROCEDURE ESTERNE
014100101026     D*-------------------
014200101026     D/COPY GAITRASRC/SRCPROTOPI,UBBVSQL01
014300101026     D/COPY GAITRASRC/SRCPROTOPR,UBBVSQL01
014400101027     D/COPY GAITRASRC/SRCPROTOPI,UBBVSKT01
014500101027     D/COPY GAITRASRC/SRCPROTOPR,UBBVSKT01
014600111215     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
014700120120     D/COPY GAITRASRC/SRCPROTOPR,TIS723R2U
014800970526     C*------------------------------------------------------------------------*
014900970526     C* MAIN LINES
015000970526     C*------------------------------------------------------------------------*
015100970526     C*
015200970526     C* NON CI SONO ERRORI IN ENTRATA
015300970526IF  1C     ERRL00        IFEQ      '0'
015400970526     C*
015500970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
015600970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
015700970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
015800970526     C     FINL00        OREQ      '1'                                          *FINE PGM
015900970526     C*
016000970526     C* EMETTE FORMATO VIDEO
016100970526     C                   EXSR      BUTFMR
016200970526     C*
016300970526     C* GESTIONE TASTI FUNZIONALI
016400970526     C                   EXSR      FUNCON
016500970526E   2C                   ENDDO
016600970526     C*
016700970526     C* CI SONO ERRORI IN ENTRATA
016800970527X   1C                   ELSE
016900991025     C                   MOVEL     '1'           S20err
017000991025     C                   MOVEL     DSMSMS        S20msg
017100970526E   1C                   ENDIF
017200970526     C* FINE PGM
017300991210     c                   exsr      finsr
017400101026     C*
017500101026     C* Chiudo la connessione
017600130215     C* (questa attività costa risorse, per cui si permette solo in gestione oppure anche in
017700130215     C*  visualizzazione se l'utente è di sede;
017800130215     C*  testo solo se l'utente è di sede, perché quelli di filiale non possono andare in gestione,
017900130215     C*  quindi non potrebbero comunque)
018000130215     C                   IF        DUtLPO = 'S'
018100111215     C                   IF        AS400_ok = *on
018200111215     C                   exsr      clsCONN
018300111215     C                   ENDIF
018400130214     C                   ENDIF
018500101026     C*
018600970526     C                   SETON                                        LR
018700970526     C*------------------------------------------------------------------------*
018800970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
018900970526     C*------------------------------------------------------------------------*
019000970526     C     BUTFMR        BEGSR
019100970526     C*
019200970526     C* EMETTE LA TESTATA
019300991026     C                   WRITE     s723D01
019400970526     C*
019500970526     C* SPEDISCE EVENTUALE MESSAGGIO
019600970526     C                   EXSR      SNDMSG
019700970526     C*
019800970526     C* EMETTE LE FUNZIONI
019900991026     C                   WRITE     s723D02
020000970526     C*
020100970526     C* EMETTE IL VIDEO
020200991026     C                   EXFMT     s723D03
020300970526     C*
020400970526     C* REIMPOSTA FLAG DI ERRORE
020500970526     C                   MOVEL     '0'           ERRL00
020600970526     C*
020700970526     C* REIMPOSTA INDICATORI DI ERRORE
020800120119     C                   SETOFF                                       495051
020900120119     C                   SETOFF                                       525354
021000120119     C                   SETOFF                                       555657
021100970526     C                   SETOFF                                         5859
021200970526     C                   SETOFF                                       606162
021300970526     C                   SETOFF                                       636465
021400970526     C                   SETOFF                                       666768
021500970526     C                   SETOFF                                       697071
021600970526     C                   SETOFF                                       727374
021700120109     C                   SETOFF                                       757677
021800970526     C                   SETOFF                                       787980
021900970526     C                   SETOFF                                       818283
022000970526     C                   SETOFF                                       848586
022100970526     C                   SETOFF                                       878889
022200970526     C*
022300970526     C                   ENDSR
022400970526     C*------------------------------------------------------------------------*
022500970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
022600970526     C*------------------------------------------------------------------------*
022700970526     C     SNDMSG        BEGSR
022800970526     C*
022900970526     C* PULISCE IL SFILE MESSAGGI
023000970526     C                   MOVEL     '1'           *IN40
023100991026     C                   WRITE     s723DMC
023200970526     C                   MOVEL     '0'           *IN40
023300970526     C*
023400970526     C* VISUALIZZA IL MESSAGGIO
023500970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
023600970526     C                   CALL      'XMSG'        XMSGPA
023700970526IF  1C     DSMSER        IFEQ      *ZEROS
023800970526     C                   MOVEL     DSMSKE        MSGKEY
023900970526E   1C                   ENDIF
024000970526     C                   Z-ADD     1             MSGULT
024100991026     C                   WRITE     s723DMS
024200991026     C                   WRITE     s723DMC
024300970526     C*
024400970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
024500970526     C                   MOVEL     *BLANKS       DSMSMS
024600970526     C*
024700970526     C                   ENDSR
024800970526     C*------------------------------------------------------------------------*
024900970526     C* ENTGES - GESTIONE ENTER -> INVIO
025000970526     C*------------------------------------------------------------------------*
025100970526     C     ENTGES        BEGSR
025200970526     C*
025300970526     C* CONTROLLA I CAMPI IMMESSI
025400970526     C                   EXSR      DECODI
025500970526     C*
025600970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
025700970526IF  1C     DEPOP1        IFEQ      'O05'
025800970526     C                   MOVEL     '1'           FINL00
025900970526E   1C                   ENDIF
026000970526     C*
026100970526     C                   ENDSR
026200031229     C*------------------------------------------------------------------------*
026300031229     C* F01GES - GESTIONE F01 -> VISUALIZZA REGOLE PARAMETRI TRADUTTORE
026400031229     C*------------------------------------------------------------------------*
026500031229     C     F01GES        BEGSR
026600031229     C*
026700031229     C* Passo o il traduttore standard relativo al tipo file corrente o se inserito il
026800031229     C* traduttore specifico
026900031229     C                   IF        VIDPGT <> *blanks
027000031229     C                   EVAL      wPGT = VIDPGT
027100031229     C                   ELSE
027200031229     C                   EVAL      wPGT = VIDPGTD
027300031229     C                   ENDIF
027400031229     C*
027500031229     C* Visualizzo le regole relative
027600031229     C                   CALL      'TIS723C'
027700031229     C                   PARM                    wPGT             10
027800040107     C                   PARM                    wERR              1
027900040107     C*
028000040107     C* Verifico se trovato membro con descrizoine dettaglio parametri traduttore
028100040107     C                   IF        wERR = '1'
028200040107     C                   MOVEL     MSG(23)       DSMSMS
028300040107     C                   ENDIF
028400031229     C*
028500031229     C                   ENDSR
028600970526     C*------------------------------------------------------------------------*
028700970526     C* F03GES - GESTIONE F03 -> FINE
028800970526     C*------------------------------------------------------------------------*
028900970526     C     F03GES        BEGSR
029000970526     C*
029100970526     C* IMPOSTA "PREMUTO F03"
029200970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
029300991025     C                   MOVEL     '1'           S20F03                         *DI PROCEDURA
029400970526     C*
029500970526     C                   ENDSR
029600970526     C*------------------------------------------------------------------------*
029700970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
029800970526     C*------------------------------------------------------------------------*
029900970526     C     F06GES        BEGSR
030000970526     C*
030100970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
030200970526IF  1C     DEPOP1        IFEQ      'F06'
030300970526     C                   EXSR      INSERI
030400970526E   1C                   ENDIF
030500970526IF  1C     DEPOP1        IFEQ      'O02'
030600970526     C                   EXSR      MODIFI
030700970526E   1C                   ENDIF
030800991014IF  1C     DEPOP1        IFEQ      'O03'
030900011129     C                   EXSR      COPIA
031000991014E   1C                   ENDIF
031100991104IF  1C*    DEPOP1        IFEQ      'O04'
031200991104     C*                  EXSR      ANNULL
031300991104E   1C*                  ENDIF
031400970526IF  1C     DEPOP1        IFEQ      'O07'
031500970526     C                   EXSR      RIPRIS
031600970526E   1C                   ENDIF
031700080603     C*
031800080603     C* Gestione estensione anagrafica clienti/tipi file => TRC 01
031900080603     C                   CLEAR                   DVIRE01
032000080603     C                   EVAL      KCEdti  = virDTI
032100080603     C                   EVAL      vireTRC = '01'
032200080603     C     KEYvire1_C    CHAIN     tivire1l
032300080603     C                   IF        %found(tivire1l)
032400080603     C                   IF        vidEML <> *blanks
032500080603     C                   EVAL      §vire01INV = 'S'
032600080603     C                   EVAL      §vire01EML = vidEML
032700080603     C                   EVAL      vireFLO = DVIRE01
032800080603     C                   UPDATE    tivire00
032900080603     C                   ELSE
033000080603     C                   DELETE    tivire00
033100080603     C                   ENDIF
033200080603     C                   ELSE
033300080603     C                   IF        vidEML <> *blanks
033400080603     C                   EVAL      vireKSC = virKSC
033500080603     C                   EVAL      vireTIP = virTIP
033600080603     C                   EVAL      vireDTI = virDTI
033700080603     C                   EVAL      vireTRC = '01'
033800080603     C                   EVAL      §vire01INV = 'S'
033900080603     C                   EVAL      §vire01EML = vidEML
034000080603     C                   EVAL      vireFLO = DVIRE01
034100080603     C                   WRITE     tivire00
034200080603     C                   ENDIF
034300080603     C                   ENDIF
034400970526     C*
034500011129     C* Dopo aver effettuato le operazioni richieste apro la tabela correlata al tipo file.
034600011129     C                   IF        errl00 = '0'                                 *NO ERRORI
034700011129     C                   CALL      'TIS70UR'
034800011129     C                   PARM                    KPJBA
034900011129     C                   PARM                    VIDTIP
035000011129     C                   PARM                    VIDKSC
035100011129     C                   PARM      *blanks       flgope            1
035200011129     C                   ENDIF
035300011129     C*
035400970526     C                   ENDSR
035500970526     C*------------------------------------------------------------------------*
035600970526     C* F12GES - GESTIONE F12 -> RITORNO
035700970526     C*------------------------------------------------------------------------*
035800970526     C     F12GES        BEGSR
035900970526     C*
036000970526     C* IMPOSTA "PREMUTO F12"
036100970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
036200991025     C                   MOVEL     '1'           S20F12                         *DI PROCEDURA
036300970526     C*
036400970526     C                   ENDSR
036500970526     C*------------------------------------------------------------------------*
036600970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
036700970526     C*------------------------------------------------------------------------*
036800970526     C     F24GES        BEGSR
036900970526     C*
037000970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
037100970526     C*
037200970526     C                   ENDSR
037300970526     C*------------------------------------------------------------------------*
037400970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
037500970526     C*------------------------------------------------------------------------*
037600970526     C     INSERI        BEGSR
037700970526     C*
037800970526     C* CONTROLLA I CAMPI IMMESSI
037900970526     C                   EXSR      DECODI
038000970526     C*
038100970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
038200991026IF  1C                   if        errl00 = '0'                                 *NO ERRORI
038300991026     C                   movel     VIDTip        virtip
038400991026     C                   move      VIDksc        virksc
038500991026     C                   z-add     VIDseq        virseq
038600991026     C                   Z-ADD     viddti        G08dat
038700991026     C                   Z-ADD     *ZEROS        G08inv
038800991026     C                   MOVEL     '0'           G08ERR
038900991026     C                   CALL      'XSRDA8'
039000991026     C                   PARM                    WLBDA8
039100991026     C                   Z-ADD     G08inv        virdti
039200991026     c                   exsr      impvir
039300991026     C                   WRITE     tivir000
039400991025     C                   MOVEL     MSG(2)        S20MSG                         *MESSAGGIO
039500970602     C*
039600970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
039700020228     C****               EXSR      AZZVID
039800970526E   1C                   ENDIF
039900970526     C*
040000970526     C                   ENDSR
040100970526     C*------------------------------------------------------------------------*
040200970526     C* MODIFI - OPERAZIONI DI MODIFICA
040300970526     C*------------------------------------------------------------------------*
040400970526     C     MODIFI        BEGSR
040500970526     C*
040600970526     C* CONTROLLA I CAMPI IMMESSI
040700970526     C                   EXSR      DECODI
040800970526     C*
040900970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
041000970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
041100991026     C                   EXSR      LETvir
041200970526     C*
041300970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
041400970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
041500991026     C                   EXSR      IMPvir                                       *IMPOSTA I CAMPI
041600991026     C                   UPDATE    tivir000
041700101027     C                   EVAL      S20MSG = MSG(3)
041800101027     C                   EVAL      DSMSMS = MSG(3)
041900101027     C*
042000101027     C* Se trattasi d tipo servizio "idoneo" modifico anche i dati utente lato "BARTVAS"
042100101027     C                   IF        *IN80
042200111215     C                             and AS400_ok = *on
042300101027      /FREE
042400101027
042500101027         if *in30 = *off;
042600101027           pInMaskChk  = VIDPPIMC;
042700101027         else;
042800101027           pInMaskChk  = pOutCliMskChk;
042900101027         endif;
043000101027
043100101027         if *in31 = *off;
043200101027           pInMaskFile = VIDPPIMF;
043300101027         else;
043400101027           pInMaskFile = pOutCliMskFil;
043500101027         endif;
043600101027
043700101027         if *in32 = *off;
043800101027           pInMaskSem  = VIDPPIMS;
043900101027         else;
044000101027           pInMaskSem  = pOutCliMskSem;
044100101027         endif;
044200101027
044300101027         if *in33 = *off;
044400101027           pInMaskZip  = VIDPPIMZ;
044500101027         else;
044600101027           pInMaskZip  = pOutCliMskZip;
044700101027         endif;
044800101027
044900101027
045000101027         if UBBVSQL01_Upd01UnqCliTipFile(
045100101027                         pInConn
045200101027                       : VIDKSC
045300101027                       : VIDTIP
045400101027                       : pInMaskChk
045500101027                       : pInMaskFile
045600101027                       : pInMaskSem
045700101027                       : pInMaskZip) = 0;
045800101027         else;
045900101027              S20MSG = MSG(29);
046000101027              DSMSMS = MSG(29);
046100101027         endif;
046200101027
046300101027      /END-FREE
046400101027     C
046500101027     C*
046600101027     C* Quindi (già che c sono ricarico anche il job
046700101027     C                   callp(e)  UBBVSKT01_ReloadCliTipF(
046800101027     C                                           VIDKSC
046900101027     C                                          :VIDTIP
047000101027     C                                          :pOutCliProc)
047100101027     C                   ENDIF
047200101027     C*
047300970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
047400970526E   2C                   ENDIF
047500970526E   1C                   ENDIF
047600970526     C*
047700970526     C                   ENDSR
047800991014     C*------------------------------------------------------------------------*
047900991014     C* copia  - OPERAZIONI DI copia
048000991014     C*------------------------------------------------------------------------*
048100991014     C     copia         BEGSR
048200991014     C*
048300991014     C* CONTROLLA I CAMPI IMMESSI
048400991014     C                   EXSR      DECODI
048500991014     C*
048600991014     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
048700991014IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
048800991026     C                   movel     VIDTip        virtip
048900991110     C                   move      VIDksc        virksc
049000991110     C                   z-add     VIDseq        virseq
049100991110     C                   Z-ADD     viddti        G08dat
049200991110     C                   Z-ADD     *ZEROS        G08inv
049300991110     C                   MOVEL     '0'           G08ERR
049400991110     C                   CALL      'XSRDA8'
049500991110     C                   PARM                    WLBDA8
049600991110     C                   Z-ADD     G08inv        virdti
049700991026     C                   EXSR      IMPvir                                       *IMPOSTA I CAMPI
049800991026     C                   write     tivir000
049900991025     C                   MOVEL     MSG(3)        S20MSG
050000991014     C                   MOVEL     '1'           FINL00                         *FINE PGM
050100991014E   2C                   ENDIF
050200991014     C*
050300991014     C                   ENDSR
050400970526     C*------------------------------------------------------------------------*
050500970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
050600970526     C*------------------------------------------------------------------------*
050700991104     C*    ANNULL        BEGSR
050800991025     C*
050900991025     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
051000991104     C*                  EXSR      LETvir
051100991025     C*
051200991025     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
051300991104IF  1C*    ERRL00        IFEQ      '0'                                          *NO ERRORI
051400991104     C*                  DELETE    tivir000                                     *CANCELLA
051500991104     C*                  MOVEL     MSG(4)        s20MSG                         *MESSAGGIO
051600991104     C*                  MOVEL     '1'           FINL00                         *FINE PGM
051700991104E   1C*                  ENDIF
051800970526     C*
051900991104     C*                  ENDSR
052000970526     C*------------------------------------------------------------------------*
052100970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
052200970526     C*------------------------------------------------------------------------*
052300970526     C     RIPRIS        BEGSR
052400970526     C                   ENDSR
052500970526     C*------------------------------------------------------------------------*
052600991026     C* LETvir - LEGGE IL RECORD PERL'AGGIORNAMENTO
052700970526     C*------------------------------------------------------------------------*
052800991026     C     LETvir        BEGSR
052900970526     C*
053000991025     C                   MOVEL     S20tip        KCEtip
053100991026     C                   MOVEL     S20ksc        KCEksc
053200000420     C     KEYvi2        CHAIN     tivir03L                           9998
053300970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
053400970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
053500991025     C                   MOVEL     MSG(7)        S20MSG                         *MESSAGGIO
053600970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
053700970526E   1C                   ENDIF
053800970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
053900970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
054000991025     C                   MOVEL     MSG(6)        S20MSG                         *MESSAGGIO
054100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
054200970526E   1C                   ENDIF
054300970526     C*
054400970526     C                   ENDSR
054500970526     C*------------------------------------------------------------------------*
054600970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
054700970526     C*------------------------------------------------------------------------*
054800970526     C     DECODI        BEGSR
054900001120     C*
055000001120     C*----------------------
055100001120     C* Controllo del codice
055200001120     C*----------------------
055300001120     C     ERRL00        IFEQ      '0'
055400001120     C     VIDksc        IFne      *blank
055500001120     C     VIDksc        andne     *zeros
055600001120     C                   exsr      cntksc
055700001120     C                   ELSE
055800991027     C                   MOVEL     '1'           ERRL00
055900991027     C                   SETON                                        65
056000991027     C                   MOVEL     MSG(12)       DSMSMS
056100001120     C                   ENDIF
056200001120     C                   ENDIF
056300101026     C*
056400101026     C* Verifico se x il servizio a video è richiesta l'indicazione della password FTP
056500101026     C     ERRL00        IFEQ      '0'
056600101026     C     VIDisv        lookup    isvftpeml                              80
056700101026 xxx C***                setoff                                       80
056800101026     C                   ENDIF
056900001120     C*----------------------
057000001120     C* Controllo della sequenza
057100001120     C*----------------------
057200001120     C     ERRL00        IFEQ      '0'
057300001120     C     VIDseq        IFne      0
057400001120     C                   ELSE
057500991027     C                   MOVEL     '1'           ERRL00
057600991027     C                   SETON                                        67
057700991027     C                   MOVEL     MSG(12)       DSMSMS
057800001120     C                   ENDIF
057900001120     C                   ENDIF
058000120109     C*----------------------
058100120109     C* Controllo tipo file
058200120109     C*----------------------
058300120109     C     ERRL00        IFEQ      '0'
058400120109     C     VIDtip        IFne      *blank
058500120109     C                   exsr      cnttip
058600120109     C                   ELSE
058700120109     C                   MOVEL     '1'           ERRL00
058800120109     C                   SETON                                        66
058900120109     C                   MOVEL     MSG(12)       DSMSMS
059000120109     C                   ENDIF
059100120109     C                   ENDIF
059200120109     C*----------------------
059300120109     C* Tipo servizio
059400120109     C*----------------------
059500120109     C     ERRL00        IFEQ      '0'
059600120109     C     VIDisv        IFne      *blank
059700120109     C                   exsr      cntisv
059800120109     C* Verifico la compatibilità del servizio rispetto al tipo-file
059900120109     C                   IF        VTFUD <> §ISVVERS
060000120109     C                   MOVEL     '1'           ERRL00
060100120109     C                   SETON                                        6670
060200120109     C                   MOVEL     MSG(24)       DSMSMS
060300120109     C                   ENDIF
060400120109     C                   ELSE
060500120109     C                   MOVEL     '1'           ERRL00
060600120109     C                   SETON                                        70
060700120109     C                   MOVEL     MSG(12)       DSMSMS
060800120109     C                   ENDIF
060900120109     C                   ENDIF
061000120109     C*--------------------
061100120109     C* Dati solo x upload no in download
061200120109     C*--------------------
061300120109     C* a seconde se siamo in Download o Upload sproteggo alcuni campi e ne proteggo altri
061400120109     C* e se non abbiamo errori pulisco i campi protetti (se abbiamo un errore potrebbe essere
061500120109     C*  che non siamo in D o U e i valori nei campi servono)
061600120109     C                   IF        §ISVVers   = 'D'
061700120109     c                   SETON                                        93
061800120109     C     ERRL00        IFEQ      '0'
061900120109     C                   MOVEL     *blanks       VIDpoi
062000120109     C                   MOVEL     *blanks       VIDEmL
062100120109     C                   MOVEL     *blanks       VIDfi2
062200120109     C                   MOVEL     *blanks       VIDfi4
062300120109     C                   MOVEL     *blanks       VIDfi4D
062400120109     C                   ENDIF
062500120109     C                   ENDIF
062600120109     C                   IF        §ISVVers   = 'U'
062700120109     C                   SETOFF                                       93
062800120109     C     ERRL00        IFEQ      '0'
062900120109     C                   MOVEL     *blanks       VIDTMbD
063000120109     C                   MOVEL     *blanks       VIDTip2
063100120109     C                   MOVEL     *blanks       VIDTip3
063200120109     C                   MOVEL     *blanks       VIDTip4
063300120119     C                   MOVEL     *blanks       VIDKsc2
063400120119     C                   MOVEL     *blanks       VIDKsc3
063500120119     C                   MOVEL     *blanks       VIDKsc4
063600120109     C                   ENDIF
063700120109     C                   ENDIF
063800100614     C*--------------------
063900100614     C* Tipo membro d default...  solo x anagrafiche d download (ovviamente no in visualizz.)
064000100614     C*--------------------
064100100614     C     ERRL00        IFEQ      '0'
064200100614     C                   IF        DEPOP1 = 'F06' OR
064300100614     C                             DEPOP1 = 'O03'
064400100614     C                   IF        §ISVPROC = 'F' AND
064500100614     C                             §ISVVERS = 'D'
064600100614     C                   IF        VIDtmbd = *blanks AND
064700100614     C                             vtfud   = 'D'
064800100614     C                   MOVEL     'M'           VIDtmbd
064900100614     C                   ENDIF
065000100614     C                   ENDIF
065100100614     C                   ENDIF
065200100614     C                   ENDIF
065300030603     C*----------------------
065400030603     C* Tipo membro
065500030603     C*----------------------
065600030603     C     ERRL00        IFEQ      '0'
065700030603     C     VIDtmbd       IFNE      *blank
065800030603     C                   IF        §ISVPROC = 'F' AND
065900030603     C                             §ISVVERS = 'D'
066000030603     C                   ELSE
066100030603     C                   MOVEL     '1'           ERRL00
066200030603     C                   SETON                                        61
066300030603     C                   MOVEL     MSG(22)       DSMSMS
066400030603     C                   ENDIF
066500030603     C                   ENDIF
066600030603     C                   ENDIF
066700120109     C*----------------------
066800120109     C* Campi duplicazione automatica su tipi file
066900120109     C*----------------------
067000120109     C     ERRL00        IFEQ      '0'
067100120109     C*
067200120109     C                   EVAL      MaxCiclo = %elem(Tips)
067300120109     C                   EVAL      Tips(1)  = VIDTip2
067400120109     C                   EVAL      Tips(2)  = VIDTip3
067500120109     C                   EVAL      Tips(3)  = VIDTip4
067600120119     C* se il tipo file è valorizzato e il cliente relativo no (o 0 o ' ')
067700120119     C* lo forzo col cliente principale
067800120119     C                   IF        (VIDKsc2 = *zeros or
067900120119     C                              VIDKsc2 = *blank   ) and
068000120119     C                             VIDTip2 <> *blank
068100120119     C                   EVAL      VIDKsc2 =  VIDKsc
068200120119     C                   ENDIF
068300120119     C                   IF        (VIDKsc3 = *zeros or
068400120119     C                              VIDKsc3 = *blank   ) and
068500120119     C                             VIDTip3 <> *blank
068600120119     C                   EVAL      VIDKsc3 =  VIDKsc
068700120119     C                   ENDIF
068800120119     C                   IF        (VIDKsc4 = *zeros or
068900120119     C                              VIDKsc4 = *blank   ) and
069000120119     C                             VIDTip4 <> *blank
069100120119     C                   EVAL      VIDKsc4 =  VIDKsc
069200120119     C                   ENDIF
069300120119     C                   EVAL      Kscs(1)  = VIDKsc2
069400120119     C                   EVAL      Kscs(2)  = VIDKsc3
069500120119     C                   EVAL      Kscs(3)  = VIDKsc4
069600120109     C     1             DO        MaxCiclo      i
069700120109     C     Tips(i)       IFNE      *blank
069800120119     C     Kscs(i)       ORNE      *blank
069900120119     C     Kscs(i)       ANDNE     *zeros
070000120109     C                   IF        §ISVVERS = 'D'
070100120119     C                   EXSR      CntTip234
070200120111     C* se mi è stato dato un errore, esco dal ciclo
070300120111     C                   IF        ERRL00 <> '0'
070400120111     C                   LEAVE
070500120111     C                   ENDIF
070600120109     C                   ELSE
070700120109     C                   SELECT
070800120109     C                   WHEN      i = 1
070900120119     C                   SETON                                        7549
071000120109     C                   WHEN      i = 2
071100120119     C                   SETON                                        7650
071200120109     C                   WHEN      i = 3
071300120119     C                   SETON                                        7751
071400120109     C                   ENDSL
071500120109     C                   MOVEL     '1'           ERRL00
071600120109     C                   MOVEL     MSG(30)       DSMSMS
071700120109     C                   ENDIF
071800120109     C                   ENDIF
071900120109     C                   ENDDO
072000120109     C*
072100120109     C                   ENDIF
072200001120     C*----------------------
072300001120     C* Controllo della descrizione
072400001120     C*----------------------
072500001120     C     ERRL00        IFEQ      '0'
072600001120     C     VIDdes        IFne      *blanks
072700001120     C                   ELSE
072800991122     C*                  MOVEL     '1'           ERRL00
072900991122     C*                  SETON                                        62
073000991122     C*                  MOVEL     MSG(12)       DSMSMS
073100991122     C                   MOVEL     VDDTIP        VIDDES
073200001120     C                   ENDIF
073300001120     C                   ENDIF
073400991026     C*--------------------
073500991026     C* DATA inizio
073600991026     C*--------------------
073700991026     C     *LIKE         DEFINE    VIDDti        DtiVID
073800001120     C     ERRL00        IFEQ      '0'
073900991026     C                   Z-ADD     VIDDti        wda8
074000991026     C                   Z-ADD     *ZEROS        da8w
074100991026     C                   EXSR      CHKDA8
074200991026     C                   Z-ADD     WDA8          viddti
074300991026     C                   Z-ADD     DA8W          dtivid
074400001120     C     ERRL00        IFEQ      '1'
074500991026     C                   SETON                                        63
074600991026     C                   MOVEL     MSG(14)       dsmsms
074700001120     C                   ENDIF
074800001120     C                   ENDIF
074900991026     C* INSERIMENTO -> RECORD INESISTENTE
075000001120     C     ERRL00        IFEQ      '0'
075100001120     C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
075200001120     C     DEPOP1        orEQ      'O03'                                        *INSERIMENTO
075300991026     C                   move      vidksc        KCEksc
075400000420     C                   move      vidtip        KCEtip
075500000420     C                   z-add     dtivid        KCEdti
075600000420     C     KEYvi2        CHAIN(N)  tivir03L                           99
075700001120     C     *IN99         IFEQ      *OFF
075800991026     C                   MOVEL     '1'           ERRL00
075900991026     C                   MOVEL     MSG(8)        DSMSMS
076000991026     C                   SETON                                        676566
076100991026     C                   SETON                                        63
076200001120     C                   ENDIF
076300001120     C                   ENDIF
076400001120     C                   ENDIF
076500001120     C*--------------------
076600991026     C* DATA SCADENZA
076700991026     C*--------------------
076800991026     C     *LIKE         DEFINE    VIDDtf        DtfVID
076900001120     C     ERRL00        IFEQ      '0'
077000991026     C                   Z-ADD     VIDDtf        WDA8
077100991026     C                   Z-ADD     *ZEROS        DA8W
077200991026     C                   EXSR      CHKDA8
077300991026     C                   Z-ADD     WDA8          VIDDtf
077400991026     C                   Z-ADD     DA8W          DtfVID
077500001120     C     ERRL00        IFEQ      '1'
077600991026     C                   SETON                                        68
077700991026     C                   MOVEL     MSG(14)       DSMSMS
077800001120     C                   ENDIF
077900001120     C                   ENDIF
078000991026     C* SCADENZA < DECORRENZA
078100001120     C     ERRL00        IFEQ      '0'
078200001120     C     DtfVID        IFLT      DtiVID
078300991026     C                   SETON                                        68
078400991026     C                   MOVEL     MSG(15)       DSMSMS
078500991026     C                   MOVEL     '1'           ERRL00
078600001120     C                   ENDIF
078700001120     C                   ENDIF
078800991026     C* SCADENZA >= DATA CORRENTE
078900001120     C     ERRL00        IFEQ      '0'
079000001120     C     DEPOP1        IFEQ      'O02'
079100991026     C     DEPOP1        OREQ      'O07'
079200991026     C     DEPOP1        OREQ      'F06'
079300001120     C     DtfVID        IFLT      DATCOR
079400001120     C                   SETON                                        68
079500991026     C                   MOVEL     MSG(16)       DSMSMS
079600991026     C                   MOVEL     '1'           ERRL00
079700001120     C                   ENDIF
079800001120     C                   ENDIF
079900001120     C                   ENDIF
080000991026     C*---
080100991026     C* DECORRENZA + SCADENZA
080200991026     C*---
080300991026     C* ACCAVALLAMENTO DATI
080400001120     C     ERRL00        IFEQ      '0'
080500991026     C                   MOVEL     VIDtip        KCEtip
080600991026     C                   MOVEL     vidksc        KCEksc
080700991026     C     KEyvir        SETLL     tivir03L
080800991026     C     KEyvir        READE(N)  tivir03L                               99
080900001120     C     *IN99         DOWEQ     *OFF
081000001120     C     DEPOP1        IFNE      'F06'                                        *NON INSERIMENTO
081100991026     C     DtiVID        ANDNE     virdti                                       *RECORD IN LINEA
081200991026     C     DEPOP1        OREQ      'F06'                                        *INSERIMENTO
081300001120     C     DtiVID        IFGE      virdti                                       *DECORRENZA
081400991026     C     DtiVID        ANDLE     virdtf
081500991026     C     DtfVID        ORGE      virdti                                       *SCADENZA
081600991026     C     DtfVID        ANDLE     virdtf
081700991026     C     virdti        ORGE      DtiVID                                       *DECORRENZA
081800991026     C     virdtf        ANDLE     DtfVID
081900991026     C                   SETON                                        666765
082000991026     C                   SETON                                        6368
082100991026     C                   MOVEL     MSG(17)       DSMSMS
082200991026     C                   MOVEL     '1'           ERRL00
082300991026     C                   LEAVE
082400001120     C                   ENDIF
082500001120     C                   ENDIF
082600991026     C     KEyvir        READE(N)  tivir03L                               99
082700001120     C                   ENDDO
082800001120     C                   ENDIF
082900001120     C*--------------------
083000001120     C* Controllo filiale
083100001120     C*--------------------
083200001120     C     ERRL00        IFEQ      '0'
083300001120     C     VIDpoi        IFne      *blank
083400001120     C     VIDpoi        andne     *zeros
083500001120     C                   exsr      cntpoi
083600001120     C                   ELSE
083700120109     C     §ISVVers      IFNE      'D'
083800991027     C                   MOVEL     '1'           ERRL00
083900000724     C                   MOVEL     MSG(12)       DSMSMS
084000991027     C                   SETON                                        69
084100991122     C                   ENDIF
084200001120     C                   ENDIF
084300001120     C                   ENDIF
084400001120     C*--------------------
084500011016     C* Programma traduttore
084600001120     C*--------------------
084700001120     C     ERRL00        IFEQ      '0'
084800001120     C     VIDpgt        IFNE      *blank
084900001120     C                   CALL(e)   'TIS721C'
085000050112     C                   PARM      *blanks       LIBRERIA         10
085100001120     C                   PARM      VIDpgt        OBJECT           10
085200001120     C                   PARM      '*PGM   '     TYPE              7
085300001120     C                   PARM      '0'           ESITO             1
085400001120     C                   IF        ESITO = '1'
085500001120     C                   MOVEL     '1'           ERRL00
085600001120     C                   SETON                                        54
085700001120     C                   MOVEL     MSG(19)       DSMSMS
085800001120     C                   ENDIF
085900001120     C                   ENDIF
086000001120     C                   ENDIF
086100001120     C*--------------------
086200001120     C* Programma post-traduzione
086300001120     C*--------------------
086400001120     C     ERRL00        IFEQ      '0'
086500001120     C     VIDpgi        IFNE      *blank
086600001120     C                   CALL(e)   'TIS721C'
086700050112     C                   PARM      *blanks       LIBRERIA         10
086800001120     C                   PARM      VIDpgi        OBJECT           10
086900001120     C                   PARM      '*PGM   '     TYPE              7
087000001120     C                   PARM      '0'           ESITO             1
087100001120     C                   IF        ESITO = '1'
087200001120     C                   MOVEL     '1'           ERRL00
087300001120     C                   SETON                                        55
087400001120     C                   MOVEL     MSG(19)       DSMSMS
087500001120     C                   ENDIF
087600001120     C                   ENDIF
087700001120     C                   ENDIF
087800001120     C*--------------------
087900001120     C* File da tradurre
088000001120     C*--------------------
088100001120     C     ERRL00        IFEQ      '0'
088200001120     C     VIDfid        IFNE      *blank
088300001120     C                   CALL(e)   'TIS721C'
088400050112     C                   PARM      *blanks       LIBRERIA         10
088500001120     C                   PARM      VIDfid        OBJECT           10
088600001120     C                   PARM      '*FILE  '     TYPE              7
088700001120     C                   PARM      '0'           ESITO             1
088800001120     C                   IF        ESITO = '1'
088900001120     C                   MOVEL     '1'           ERRL00
089000001120     C                   SETON                                        53
089100001120     C                   MOVEL     MSG(20)       DSMSMS
089200001120     C                   ENDIF
089300001120     C                   ENDIF
089400001120     C                   ENDIF
089500001120     C*--------------------
089600001120     C* File storico
089700001120     C*--------------------
089800001120     C     ERRL00        IFEQ      '0'
089900001120     C     VIDfis        IFNE      *blank
090000001120     C                   CALL(e)   'TIS721C'
090100050112     C                   PARM      *blanks       LIBRERIA         10
090200001120     C                   PARM      VIDfis        OBJECT           10
090300001120     C                   PARM      '*FILE  '     TYPE              7
090400001120     C                   PARM      '0'           ESITO             1
090500001120     C                   IF        ESITO = '1'
090600001120     C                   MOVEL     '1'           ERRL00
090700001120     C                   SETON                                        56
090800001120     C                   MOVEL     MSG(20)       DSMSMS
090900001120     C                   ENDIF
091000001120     C                   ENDIF
091100001120     C                   ENDIF
091200001120     C*--------------------
091300001120     C* File tradotto
091400001120     C*--------------------
091500001120     C     ERRL00        IFEQ      '0'
091600001120     C     VIDfit        IFNE      *blank
091700001120     C                   CALL(e)   'TIS721C'
091800050112     C                   PARM      VIDfi3        LIBRERIA         10
091900001120     C                   PARM      VIDfit        OBJECT           10
092000001120     C                   PARM      '*FILE  '     TYPE              7
092100001120     C                   PARM      '0'           ESITO             1
092200001120     C                   IF        ESITO = '1'
092300001120     C                   MOVEL     '1'           ERRL00
092400001120     C                   SETON                                        57
092500001120     C                   MOVEL     MSG(20)       DSMSMS
092600001120     C                   ENDIF
092700001120     C                   ENDIF
092800001120     C                   ENDIF
092900001120     C*--------------------
093000001120     C* Libreria file tradotto
093100001120     C*--------------------
093200001120     C     ERRL00        IFEQ      '0'
093300001120     C     VIDfi3        IFNE      *blank
093400001120     C                   CALL(e)   'TIS721C'
093500050112     C                   PARM      *blanks       LIBRERIA         10
093600001120     C                   PARM      VIDfi3        OBJECT           10
093700001120     C                   PARM      '*LIB   '     TYPE              7
093800001120     C                   PARM      '0'           ESITO             1
093900001120     C                   IF        ESITO = '1'
094000001120     C                   MOVEL     '1'           ERRL00
094100001120     C                   SETON                                        52
094200001120     C                   MOVEL     MSG(21)       DSMSMS
094300001120     C                   ENDIF
094400001120     C                   ENDIF
094500001120     C                   ENDIF
094600040816     C*--------------------
094700100311     C* Controllo corrispondenza Tipo File / Programma Traduttore / File Tradotto / Tipo membro Trad
094800040816     C*--------------------
094900040816     C     ERRL00        IFEQ      '0'
095000040816     C     VIDfit        IFne      *blank
095100040816     C     VIDpgt        ORne      *blank
095200040816     C                   EVAL      TBEcod = 'IFT'
095300040816     C     KEYtbe1_P     SETLL     tntbe01l
095400040816     C                   IF        %equal(tntbe01l)
095500040816     C* Inizializzo indici e schiere
095600040816     C                   Z-ADD     *zeros        j
095700040816     C                   CLEAR                   skTipF
095800040816     C                   CLEAR                   skPgt
095900040816     C                   CLEAR                   skFileT
096000100311     C                   CLEAR                   skMembroT
096100040816     C* Carico le schiere da tabella 'IFT'
096200040816     C     KEYtbe1_P     READE     tntbe01l
096300040816     C                   DOW       not %eof(tntbe01l)
096400040816     C                   IF        TBEatb = *blanks
096500040816     C                   EVAL      DIFT = TBEuni
096600040816     C                   ADD       1             j
096700100311     C                   EVAL      skTipF(j)    = TBEke1
096800100311     C                   EVAL      skPgt(j)     = §IFTPGT
096900100311     C                   EVAL      skFileT(j)   = §IFTFILE
097000100311     C                   EVAL      skMembroT(j) = §IFTTMBD
097100040816     C                   ENDIF
097200040816     C     KEYtbe1_P     READE     tntbe01l
097300040816     C                   ENDDO
097400040816     C* Imposto cosa testare
097500040816     C                   IF        VIDPGT <> *blanks
097600040816     C                   EVAL      wVIDPGT = VIDPGT
097700040816     C                   ELSE
097800040816     C                   EVAL      wVIDPGT = VIDPGTD
097900040816     C                   ENDIF
098000040816     C                   IF        VIDFIT <> *blanks
098100040816     C                   EVAL      wVIDFIT = VIDFIT
098200040816     C                   ELSE
098300040816     C                   EVAL      wVIDFIT = VIDFITD
098400040816     C                   ENDIF
098500040816     C* Testo se il traduttore indicato è tra quelli da controllare (in tabella 'IFT')
098600040823     C                   MOVEL     '0'           wFlgIFTok         1
098700100223     C                   SETOFF                                       16
098800040816     C     wVIDPGT       LOOKUP    skPgt                                  15
098900040816     C                   IF        %found
099000040816     C* Testo se vi è corrispondenza
099100040816     C                   EVAL      j = 1
099200040816     C                   DOW       j <= %elem(skTipF)
099300100223     C                   IF        skTipF(j)  = VIDTIP  AND
099400100223     C                             skPgt(j)   = wVIDPGT
099500100223     C                   SETON                                        16
099600100311     C*
099700100223     C                   IF        skFileT(j) = wVIDFIT
099800040816     C                   MOVEL     '1'           wFlgIFTok
099900100311     C*
100000100311     C                   IF        DEPOP1 = 'O02' OR
100100100311     C                             DEPOP1 = 'O03' OR
100200100311     C                             DEPOP1 = 'F06'
100300100312     C                   IF        VIDtmbd      =  *blanks AND
100400100312     C                             skMembroT(j) <> *blanks
100500100311     C                   EVAL      VIDtmbd = skMembroT(j)
100600100311     C                   SETON                                        61
100700100311     C                   ENDIF
100800100311     C                   ENDIF
100900100311     C*
101000040816     C                   LEAVE
101100040816     C                   ENDIF
101200100311     C*
101300100223     C                   ENDIF
101400040816     C                   ADD       1             j
101500040816     C                   ENDDO
101600040816     C* Se nn trovata esatta corrispondenza => errore
101700100223     C                   IF        *IN16 and wFlgIFTok = '0'
101800040816     C                   MOVEL     '1'           ERRL00
101900040816     C                   MOVEL     MSG(25)       DSMSMS
102000040816     C                   SETON                                        665457
102100040816     C                   ENDIF
102200040816     C                   ENDIF
102300040816     C                   ENDIF
102400040816     C                   ENDIF
102500040823     C                   ENDIF
102600051026     C*--------------------
102700051026     C* Controllo particolare x anagrafiche d download: se si sta cambiando il traduttore
102800051026     C* verifico prima che nn vi siano attualmente record d log a stato zero
102900051026     C*--------------------
103000051026     C     ERRL00        IFEQ      '0'
103100120109     C     §ISVVers      IFEQ      'D'
103200051026     C     VIDpgt        IFNE      virPGT
103300051026     C     VIDpgi        ORNE      virPGI
103400051026     C                   CALL      'TIS723R1'
103500051026     C                   PARM                    VIDKSC
103600051026     C                   PARM                    VIDTIP
103700120109     C                   PARM                    §ISVVers
103800051026     C                   PARM      *zeros        wRecZero          9 0
103900051026     C                   IF        wRecZero > *zeros
104000051026     C                   MOVEL     '1'           ERRL00
104100051026     C                   MOVEL     MSG(26)       DSMSMS
104200051026     C                   SETON                                        5455
104300051026     C                   ENDIF
104400051026     C                   ENDIF
104500051026     C                   ENDIF
104600051026     C                   ENDIF
104700060616     C*--------------------
104800060616     C* File Da tradurre / File Storico / Programma Post-Traduzione
104900060616     C*--------------------
105000060616     C     ERRL00        IFEQ      '0'
105100060616     C                   IF        (VIDfid='*' AND VIDfis='*' AND VIDpgi='*') OR
105200060616     C                             (VIDfid<>'*' AND VIDfis<>'*' AND VIDpgi<>'*')
105300060616     C                   ELSE
105400060616     C                   MOVEL     '1'           ERRL00
105500060616     C                   SETON                                        535556
105600060616     C                   MOVEL     MSG(27)       DSMSMS
105700060616     C                   ENDIF
105800060616     C                   ENDIF
105900081020     C*--------------------
106000081020     C* Mail x eventuale feed-back richiesto
106100081020     C*--------------------
106200081020     C     ERRL00        IFEQ      '0'
106300081020     C                   CLEAR                   DSEMAIL
106400081020     C                   EVAL      §EMLINDI = VIDEML
106500081020     C                   CALL      'XEMAIL'
106600081020     C                   PARM                    DSEMAIL
106700081020     C                   IF        §EMLERRO = '1'
106800081020     C                   MOVEL     '1'           ERRL00
106900081020     C                   SETON                                        64
107000081020     C                   MOVEL     §EMLMSGO      DSMSMS
107100081020     C                   ENDIF
107200081020     C                   ENDIF
107300001120     C*
107400001120     C                   ENDSR
107500000626     C*------------------------------------------------------------------------*
107600000626     C* Cntisv - CONTROLLA codice tipo legame
107700000626     C*------------------------------------------------------------------------*
107800001120     C     cntisv        begsr
107900001120     C                   clear                   TIBS02DS
108000001120     C*
108100101027     C     '?'           SCAN      Vidisv                                 28
108200001120     C*  Ricerca
108300001120     C                   eval      t02tla = 'L'
108400101027     C                   IF        *IN28 = *ON
108500001120     C                   movel     'R'           t02mod
108600001120     C                   else
108700001120     C                   movel     'C'           t02mod
108800001120     C                   movel(p)  vidisv        t02ke1
108900001120     C                   end
109000000626     C                   movel     'ISV'         t02cod
109100001120     C                   movel     knsif         t02sif
109200001120     C                   CALL      'TIBS02R'
109300001120     C                   parm                    KPJBA
109400001120     C                   parm                    TIBS02DS
109500001120     C
109600001120     C*
109700001120     C                   IF        T02err = *blanks
109800000626     C                   movel     T02ke1        Vidisv
109900000626     C                   movel     T02uni        vddisv
110000030603     C                   movel(P)  T02uni        DISV
110100001120     C                   else
110200001120     C                   movel     '1'           errl00
110300001120     C                   seton                                        70
110400001120     C                   MOVEL     MSG(18)       DSMSMS
110500001120     C                   ENDIF
110600001120     C
110700001120     C                   endsr
110800991027     C*------------------------------------------------------------------------*
110900991027     C* cnttip - Controlla esistenza tipo file su tivt00f
111000991027     C*------------------------------------------------------------------------*
111100001120     C     cnttip        begsr
111200001120     C                   clear                   TIBS02DS
111300000626     C*
111400101027     C     '?'           SCAN      vidtip                                 28
111500001120     C*  Ricerca
111600001120     C                   eval      t02tla = 'L'
111700101027     C                   IF        *IN28 = *ON
111800001120     C                   movel     'R'           t02mod
111900001120     C                   else
112000001120     C                   movel     'C'           t02mod
112100001120     C                   movel(p)  vidtip        t02ke1
112200001120     C                   end
112300991108     C                   movel     'ITF'         t02cod
112400001120     C                   movel     knsif         t02sif
112500991108     C                   CALL      'TIBS02R'
112600001120     C                   parm                    KPJBA
112700001120     C                   parm                    TIBS02DS
112800001120     C
112900001120     C*
113000001120     C                   IF        T02err = *blanks
113100991108     C                   movel     T02ke1        vidtip
113200020218     C                   movel     T02uni        DITF
113300020218     C                   movel     §ITFDESC      vddtip
113400021003     C* Sostituisco con il dato proveniente dalla tabella solo se nn già indicato
113500021003     C* qualcosa nelle regole specifiche x il cliente/tipo file.
113600021014     C                   if        vidfit = *blanks
113700021014     C                   movel     §ITFLIBD      vidfi2
113800021014     C                   endif
113900001120     C                   else
114000001120     C                   movel     '1'           errl00
114100991108     C                   movel     *blank        vddtip
114200111215     C* pulisco anche il cod. tabella associata
114300111215     C                   movel     *blank        vidtab
114400001120     C                   seton                                        66
114500991108     C                   MOVEL     msg(11)       DSMSMS
114600991108     C                   ENDIF
114700001120     C                   if        errl00 = '0'
114800120109     C     vidtip        chain     tivtf01l                           97
114900001120     C                   if        not *in97
115000001120     C                   movel     vtfpgt        vidpgtd
115100001120     C                   movel     vtfpgi        vidpgid
115200001120     C                   movel     vtffid        vidfidd
115300001120     C                   movel     vtffis        vidfisd
115400001120     C                   movel     vtffit        vidfitd
115500111214     C                   movel     vtffi1        vidtab
115600101013     C                   movel     vtfppi        DVTFPPI
115700101026     C                   if        DEPOP1 = 'F06' OR
115800101026     C                             DEPOP1 = 'O03'
115900101026     C                   select
116000101026     C                   when      vtfUD = 'U'
116100101026     C                   movel     D§VTFPPIMF    vidppiMC
116200101026     C                   when      vtfUD = 'D'
116300101026     C                   movel     D§VTFPPIMF    vidppiMF
116400101026     C                   endsl
116500101026     C                   movel     D§VTFPPIMS    vidppiMS
116600101026     C                   movel     D§VTFPPIMZ    vidppiMZ
116700101026     C                   endif
116800100308     C                   if        vidfi4d <> *blanks and vidfi4d <> '*'
116900001120     C                   eval      vidfi4d = %subst(vidfitd:1:5) + '00F'
117000100308     C                   endif
117100001120     C                   else
117200001120     C                   movel     *blank        vidpgtd
117300001120     C                   movel     *blank        vidpgid
117400001120     C                   movel     *blank        vidfidd
117500001120     C                   movel     *blank        vidfisd
117600001120     C                   movel     *blank        vidfitd
117700001120     C                   movel     *blank        vidfi4d
117800001120     C                   endif
117900001120     C                   endif
118000001120     C*
118100001120     C                   endsr
118200991027     C*------------------------------------------------------------------------*
118300120109     C* CntTip234 - Controlla esistenza altri tipi file su tivt00f
118400991027     C*------------------------------------------------------------------------*
118500120109     C     CntTip234     BEGSR
118600120119     C*
118700120119     C* se ho indicato un cliente devo indicare il tipo file
118800120119     C                   IF        Kscs(i) <>*blank and
118900120119     C                             Kscs(i) <>*zeros and
119000120119     C                             Tips(i) = *blank
119100120119     C                   SELECT
119200120119     C                   WHEN      i = 1
119300120119     C                   SETON                                        7549
119400120119     C                   WHEN      i = 2
119500120119     C                   SETON                                        7650
119600120119     C                   WHEN      i = 3
119700120119     C                   SETON                                        7751
119800120119     C                   ENDSL
119900120119     C                   MOVEL     '1'           ERRL00
120000120119     C                   MOVEL     MSG(34)       DSMSMS
120100120119     C                   LEAVESR
120200120119     C                   ENDIF
120300010913     C*
120400120119     C* il tipo file / cliente indicati non devono essere quelli in gestione
120500120119     C                   IF        Tips(i) = VIDTip and
120600120119     C                             Kscs(i) = VIDKsc
120700120111     C                   SELECT
120800120111     C                   WHEN      i = 1
120900120119     C                   SETON                                        7549
121000120111     C                   WHEN      i = 2
121100120119     C                   SETON                                        7650
121200120111     C                   WHEN      i = 3
121300120119     C                   SETON                                        7751
121400120111     C                   ENDSL
121500120111     C                   MOVEL     '1'           ERRL00
121600120111     C                   MOVEL     MSG(33)       DSMSMS
121700120111     C                   LEAVESR
121800120111     C                   ENDIF
121900120111     C*
122000120111     C* il tipo file indicato deve esistere ed essere abilitato su questa anagrafica per il cliente
122100120119     C* indicato
122200120111     C* (per abilitato basta che la data fine validità sia maggiore di oggi; se anche la data inizio
122300120111     C*  è maggiore di oggi non importa, sarà valido nel futuro)
122400120119     C                   EVAL      x_VIRKsc = Kscs(i)
122500120111     C                   EVAL      x_VIRTip = Tips(i)
122600120111     C                   EVAL      x_VIRDtF = %dec(%date() : *ISO)
122700120111     C     K03VIR06      SETLL     TIVIR06L
122800120111     C     K02VIR06      READE     TIVIR06L
122900120111     C                   if        %eof
123000120109     C                   SELECT
123100120109     C                   WHEN      i = 1
123200120109     C                   SETON                                        75
123300120109     C                   WHEN      i = 2
123400120109     C                   SETON                                        76
123500120109     C                   WHEN      i = 3
123600120109     C                   SETON                                        77
123700120109     C                   ENDSL
123800120109     C                   MOVEL     '1'           ERRL00
123900120109     C                   MOVEL     MSG(31)       DSMSMS
124000120119     C* se il tipo file / cliente per duplica esiste,
124100120119     C* questo non deve contenere nelle sue coppie per duplica
124200120119     C* il tipo file / cliente in gestione
124300120111     C                   ELSE
124400120119     C*
124500120120     C                   if        TIS723R2U_Ctrl_Chain_Dup(
124600120120     C                              VIDTip       :
124700120120     C                              VIDKsc       :
124800120120     C                              Tips(i)      :
124900120120     C                              Kscs(i)      ) = '1'
125000120120     C                   SELECT
125100120120     C                   WHEN      i = 1
125200120120     C                   SETON                                        7549
125300120120     C                   WHEN      i = 2
125400120120     C                   SETON                                        7650
125500120120     C                   WHEN      i = 3
125600120120     C                   SETON                                        7751
125700120120     C                   ENDSL
125800120120     C                   MOVEL     '1'           ERRL00
125900120120     C                   MOVEL     MSG(32)       DSMSMS
126000120120     C                   ENDIF
126100120120     C***
126200120120     C***                MOVEL     x_VIRPTH      DVIR01
126300120120     C***                IF        §VIR01Tip2 = VIDTip and
126400120120     C***                          §VIR01Ksc2 = VIDKsc     or
126500120120     C***                          §VIR01Tip3 = VIDTip and
126600120120     C***                          §VIR01Ksc3 = VIDKsc     or
126700120120     C***                          §VIR01Tip4 = VIDTip and
126800120120     C***                          §VIR01Ksc4 = VIDKsc
126900120120     C***                SELECT
127000120120     C***                WHEN      i = 1
127100120120     C***                SETON                                        7549
127200120120     C***                WHEN      i = 2
127300120120     C***                SETON                                        7650
127400120120     C***                WHEN      i = 3
127500120120     C***                SETON                                        7751
127600120120     C***                ENDSL
127700120120     C***                MOVEL     '1'           ERRL00
127800120120     C***                MOVEL     MSG(32)       DSMSMS
127900120120     C***                ENDIF
128000120120     C***
128100120111     C                   ENDIF
128200120109     C*
128300120109     C                   ENDSR
128400120109     C*------------------------------------------------------------------------*
128500120109     C* cntksc - Controlla esistenza codice
128600120109     C*------------------------------------------------------------------------*
128700120109     C     cntksc        begsr
128800120109     C*
128900010913     C     vidksc        chain     tianv01l
129000010913     C                   if        %found
129100010913     C                   movel     ANVDES        vddksc
129200010913     C                   else
129300001120     C                   move      vidksc        wcli              7 0
129400991210     C                   CLEAR                   BS69DS
129500991210     C                   CLEAR                   ACODS
129600991210     C                   MOVEL     KNSIF         I69SIF
129700991210     C                   Z-ADD     WCLI          I69KAC
129800991210     C                   CALL      'TIBS69R'
129900991210     C                   PARM                    BS69DS
130000991210     C                   PARM                    ACODS
130100991210     C                   PARM                    INDDS
130200991210     C                   PARM                    CLPDS
130300991210     C                   PARM                    CLSDS
130400991210     C     O69ERR        IFNE      '1'
130500991210     C                   MOVEL     ACORAG        vddksc
130600991210     C                   ELSE
130700991210     C                   MOVEL     *ALL'*'       vddksc
130800010913     C                   MOVE      '1'           errl00
130900010913     C                   MOVEL     msg(11)       dsmsms
131000010913     C                   ENDIF
131100000727     C                   ENDIF
131200010913     C*
131300010913     C                   ENDSR
131400991027     C*------------------------------------------------------------------------*
131500040901     C* cntpoi - Controlla codice filiale
131600991027     C*------------------------------------------------------------------------*
131700001120     C     cntpoi        begsr
131800001120     C*
131900010529     C                   if        vidpoi <> '046' and
132000020809     C                             vidpoi <> '999' and
132100020902     C                             vidpoi <> '500' and
132200020902     C                             vidpoi <> '789'
132300001120     C*
132400001120     C                   move      *blank        vddpoi
132500001120     C     '?'           scan      vidpoi                                 97
132600001120     C                   if        *in97
132700001120     C                   movel     *blank        cod1
132800001120     C                   movel     *blank        des1
132900001120     C                   movel     *blank        pra1
133000001120     C                   call      'TNSD24R'
133100001120     C                   parm                    cod1              3
133200001120     C                   parm                    tip1              1
133300001120     C                   parm                    des1             25
133400001120     C                   parm                    pra1              3
133500001120     C                   movel     cod1          vidpoi
133600001120     C                   endif
133700001120     C                   move      vidpoi        cod1n             3 0
133800001120     C     cod1n         chain     azorg01l                           98
133900001120     C                   if        not *in98 and orgfag <>'V'
134000001120     C                   movel     orgdes        vddpoi
134100001120     C                   else
134200001120     C                   seton                                        69
134300001120     C                   move      '1'           errl00
134400001120     C                   movel     msg(13)       dsmsms
134500001120     C                   endif
134600001120     C*
134700001120     C                   else
134800001120     C                   movel     'SEDE BOLOGNA'vddpoi
134900001120     C                   endif
135000001120     C*
135100001120     C                   endsr
135200991210     C*------------------------------------------------------------------------*
135300991210     C* FINSR - OPERAZIONI FINALI
135400991210     C*------------------------------------------------------------------------*
135500991210     C     FINSR         BEGSR
135600991210     C*
135700991210     C* CHIUDE I FILE DEI PGM CHIAMATI
135800991210     C                   CLEAR                   bs69ds
135900991210     C                   CLEAR                   acods
136000991210     C                   CLEAR                   indds
136100991210     C                   CLEAR                   clpds
136200991210     C                   CLEAR                   clsds
136300991210     C                   MOVEL     'C'           i69tla
136400991210     C                   MOVEL     knsif         i69sif
136500991210     C                   CALL      'TIBS69R'
136600991210     C                   PARM                    bs69ds
136700991210     C                   PARM                    acods
136800991210     C                   PARM                    indds
136900991210     C                   PARM                    clpds
137000991210     C                   PARM                    clsds
137100991210     C*
137200991210     C                   ENDSR
137300991026     C*------------------------------------------------------------------------*
137400991026     C* CHKDA8 - CONTROLLA LA DATA (8)
137500991026     C*------------------------------------------------------------------------*
137600991026     C     CHKDA8        BEGSR
137700991026     C*
137800991026     C     *LIKE         DEFINE    VIDDti        WDA8
137900991026     C     *LIKE         DEFINE    VIDDti        DA8W
138000991026     C*
138100991026     C                   Z-ADD     WDA8          G08DAT
138200991026     C                   Z-ADD     *ZEROS        G08INV
138300991026     C                   MOVEL     '0'           G08ERR
138400991026     C                   CALL      'XSRDA8'
138500991026     C                   PARM                    WLBDA8
138600991026     C                   Z-ADD     G08DAT        WDA8
138700991026     C                   Z-ADD     G08INV        DA8W
138800991026IF  1C     G08ERR        IFEQ      '1'
138900991026     C                   MOVEL     '1'           ERRL00
139000991026E   1C                   ENDIF
139100991026     C*
139200991026     C                   ENDSR
139300970526     C*------------------------------------------------------------------------*
139400970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
139500970526     C*------------------------------------------------------------------------*
139600970526     C     IREVID        BEGSR
139700080604     C*
139800080604     C* REPERISCO INFO DA ESTENSIONE ANAGRAFICA CLIENTI/TIPI FILE => TRC 01
139900080604     C                   IF        §vire01INV = 'S'
140000080604     C                   EVAL      VIDEML = §vire01EML
140100080604     C                   ENDIF
140200970526     C* CAMPI
140300991026     C                   MOVEL     virtip        VIDtip
140400020218     C                   MOVE      virksc        vidksc
140500020218     C                   Z-ADD     virseq        vidseq
140600060308     C                   IF        DEPOP1 = 'O03'
140700060308     C                   MOVEL     *blanks       vidpoi
140800060308     C                   ELSE
140900060308     C                   MOVE      virpoi        vidpoi
141000060308     C                   ENDIF
141100991026     C                   MOVEL     virdes        VIDdes
141200991026     C                   MOVEL     virpgt        VIDpgt
141300991026     C                   MOVEL     virppt        VIDppt
141400991026     C                   MOVEL     virpgi        VIDpgi
141500991026     C                   MOVEL     virppi        VIDppi
141600991026     C                   MOVEL     virfid        VIDfid
141700991026     C                   MOVEL     virfis        VIDfis
141800991026     C                   MOVEL     virfit        VIDfit
141900000710     C                   MOVEL     virfi3        VIDfi3
142000000710     C                   MOVEL     virfi4        VIDfi4
142100010123     C                   MOVEL     virfi2        VIDfi2
142200030526     C                   MOVEL     virpth        DVIR01
142300030526     C                   MOVEL     §VIR01TM      VIDtmbd
142400120109     C                   MOVEL     §VIR01Tip2    VIDTip2
142500120109     C                   MOVEL     §VIR01Tip3    VIDTip3
142600120109     C                   MOVEL     §VIR01Tip4    VIDTip4
142700120119     C                   MOVEL     §VIR01Ksc2    VIDKsc2
142800120119     C                   MOVEL     §VIR01Ksc3    VIDKsc3
142900120119     C                   MOVEL     §VIR01Ksc4    VIDKsc4
143000000710  ***C*                  MOVEL     virctl        VIDctl
143100991026     C                   Z-ADD     virdti        G08INV
143200991026     C                   Z-ADD     *ZEROS        G08DAT
143300991026     C                   MOVEL     '3'           G08ERR
143400991026     C                   CALL      'XSRDA8'
143500991026     C                   PARM                    WLBDA8
143600991026     C                   Z-ADD     G08DAT        viddti
143700970527     C*
143800991026     C                   Z-ADD     virdtf        G08INV
143900991026     C                   Z-ADD     *ZEROS        G08DAT
144000991026     C                   MOVEL     '3'           G08ERR
144100991026     C                   CALL      'XSRDA8'
144200991026     C                   PARM                    WLBDA8
144300991026     C                   Z-ADD     G08DAT        viddtf
144400020218     C                   MOVEL     virfi1        vidisv
144500051122     C*
144600051122     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
144700051122     C                   IF        DEPOP1 = 'F06' OR
144800051122     C                             DEPOP1 = 'O03' OR
144900051122     C                             DEPOP1 = 'O07'
145000051122     C                   EVAL      VIDDTI = DATCORINV
145100060726     C                   EVAL      VIDDTF = 31122039
145200051122     C                   ENDIF
145300020218     C*
145400970526     C                   ENDSR
145500970526     C*------------------------------------------------------------------------*
145600970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
145700970526     C*------------------------------------------------------------------------*
145800970526     C     AZZVID        BEGSR
145900970526     C* CAMPI
146000991026     C                   MOVEL     *blank        VIDksc
146100000710  ***C*                  MOVEL     *blank        VIDpt1
146200000710  ***C*                  MOVEL     *blank        VIDpt2
146300010123     C                   z-add     0             vidseq
146400010123     C                   move      *zeros        vidpoi
146500060726     C                   EVAL      VIDDTI = DATCORINV
146600060726     C                   EVAL      VIDDTF = 31122039
146700991025     C                   MOVEL     *blank        VIDtip
146800991025     C                   MOVEL     *blank        VIDdes
146900991025     C                   MOVEL     *blank        VIDpgt
147000991025     C                   MOVEL     *blank        VIDppt
147100991025     C                   MOVEL     *blank        VIDpgi
147200991025     C                   MOVEL     *blank        VIDppi
147300991025     C                   MOVEL     *blank        VIDfid
147400991025     C                   MOVEL     *blank        VIDfis
147500991025     C                   MOVEL     *blank        VIDfit
147600000710     C                   MOVEL     *blank        VIDfi3
147700000710     C                   MOVEL     *blank        VIDfi4
147800010123     C                   MOVEL     *blank        VIDfi2
147900000710  ***C*                  MOVEL     *blank        VIDctl
148000080603     C                   MOVEL     *blank        VIDeml
148100970526     C*
148200970526     C                   ENDSR
148300970526     C*------------------------------------------------------------------------*
148400991026     C* IMPvir - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
148500970526     C*------------------------------------------------------------------------*
148600991026     C     IMPvir        BEGSR
148700970526     C*
148800991027     C                   move      VIDpoi        virpoi
148900991026     C                   movel     VIDdes        virdes
149000991026     C                   movel     VIDpgt        virpgt
149100991026     C                   movel     VIDppt        virppt
149200991026     C                   movel     VIDpgi        virpgi
149300991026     C                   movel     VIDppi        virppi
149400991026     C                   movel     VIDfid        virfid
149500991026     C                   movel     VIDfis        virfis
149600991026     C                   movel     VIDfit        virfit
149700000710     C                   movel     VIDfi3        virfi3
149800000710     C                   movel     VIDfi4        virfi4
149900010123     C                   movel     VIDfi2        virfi2
150000030526     C                   MOVEL     VIDtmbd       §VIR01TM
150100120109     C                   EVAL      §VIR01Tip2 =  VIDTip2
150200120109     C                   EVAL      §VIR01Tip3 =  VIDTip3
150300120109     C                   EVAL      §VIR01Tip4 =  VIDTip4
150400120119     C                   IF        VIDKsc2 = *zeros
150500120119     C                   EVAL      VIDKsc2 = *blank
150600120119     C                   ENDIF
150700120119     C                   IF        VIDKsc3 = *zeros
150800120119     C                   EVAL      VIDKsc3 = *blank
150900120119     C                   ENDIF
151000120119     C                   IF        VIDKsc4 = *zeros
151100120119     C                   EVAL      VIDKsc4 = *blank
151200120119     C                   ENDIF
151300120119     C                   EVAL      §VIR01Ksc2 =  VIDKsc2
151400120119     C                   EVAL      §VIR01Ksc3 =  VIDKsc3
151500120119     C                   EVAL      §VIR01Ksc4 =  VIDKsc4
151600030526     C                   MOVEL     DVIR01        virpth
151700000710  ***C*                  movel     VIDctl        virctl
151800991026     C                   Z-ADD     viddtf        G08dat
151900991026     C                   Z-ADD     *ZEROS        G08inv
152000991026     C                   MOVEL     '0'           G08ERR
152100991026     C                   CALL      'XSRDA8'
152200991026     C                   PARM                    WLBDA8
152300991026     C                   Z-ADD     G08inv        virdtf
152400000628     C                   movel     VIDisv        virfi1
152500970526     C*
152600970526     C                   ENDSR
152700970526     C*------------------------------------------------------------------------*
152800970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
152900970526     C*------------------------------------------------------------------------*
153000970526     C     IMPFUN        BEGSR
153100101013     C*
153200101026     C                   SETOFF                                       44
153300101026     C                   SETOFF                                       30
153400101026     C                   SETOFF                                       31
153500101026     C                   SETOFF                                       32
153600101026     C                   SETOFF                                       33
153700970526     C*
153800970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
153900970526IF  1C     DEPOP1        IFEQ      'O05'
154000970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
154100970526     C                   MOVEL     'N'           FUDATT
154200970526     C                   MOVEL     FUDRIG        FUD(6)
154300970526E   1C                   ENDIF
154400970526     C*
154500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
154600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
154700970526     C                   EXSR      FUNGES
154800970526     C*
154900970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
155000970526     C*---
155100970526     C* 90 -> PROTEZIONE CHIAVE
155200970526     C* 91 -> PROTEZIONE TOTALE
155300970526     C*---
155400970528IF  1C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
155500970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
155600970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
155700970526     C                   SETOFF                                       91
155800970526E   2C                   ENDIF
155900101013IF  2C     DEPOP1        IFEQ      'O03'                                        *INSERIMENTO
156000101013E   2C                   ENDIF
156100970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
156200991014IF  2C     DEPOP1        oreq      'O01'                                        *MODIFICA
156300970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
156400970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
156500970526E   2C                   ENDIF
156600970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
156700970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
156800970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
156900970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
157000970526     C                   SETON                                        91
157100970526E   2C                   ENDIF
157200970526X   1C                   ELSE                                                   *I N T E R R O G A
157300970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
157400970526     C                   SETON                                        91
157500970526E   1C                   ENDIF
157600970526     C*
157700970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
157800970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
157900991025     C                   MOVEL     S20tip        KCEtip
158000991026     C                   MOVEL     S20ksc        KCEksc
158100000420     C     KEYvi2        CHAIN     tivir03L                           9998
158200080603IF  2C     *IN99         IFEQ      *ON
158300970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
158400970526     C                   MOVEL     MSG(6)        DSMSMS
158500970526X   2C                   ELSE
158600080603     C*
158700080603     C* Aggancio anche il record d estensione regole clienti/tipi-file => TRC 01
158800080603     C                   EVAL      KCEdti  = virDTI
158900080603     C                   EVAL      vireTRC = '01'
159000080603     C     KEYvire1_C    CHAIN     tivire1l                           9998
159100080603     C                   IF        %found(tivire1l)
159200080603     C                   EVAL      DVIRE01 = vireFLO
159300080603     C                   ENDIF
159400080603     C*
159500080603     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
159600980128IF  3C     DEPOP1        IFEQ      'O05'
159700991014IF  3C     DEPOP1        oreq      'O02'
159800101026     C*
159900101026     C* Se trattasi d tipo servizio "idoneo" reperisco anche i dati utente lato "BARTVAS"
160000101026     C     virFI1        lookup    isvftpeml                              80
160100101026     C                   IF        *IN80
160200111215     C                             and AS400_ok = *on
160300101026      /FREE
160400101026
160500130215       // (questa attività costa risorse, per cui si permette solo in gestione oppure anche in
160600130215       //  visualizzazione se l'utente è di sede;
160700130215       //  testo solo se l'utente è di sede, perché quelli di filiale non possono andare in gestione
160800130215       //  quindi non potrebbero comunque)
160900130215       if DUtLPO = 'S';
161000101026         if UBBVSQL01_RtvCliTipFile(
161100101026                         pInConn
161200101026                       : virKSC
161300101026                       : virTIP
161400101026                       : pOutNumRec
161500101026                       : pOutCliTipTras
161600101026                       : pOutCliMskChk
161700101026                       : pOutCliMskFil
161800101026                       : pOutCliMskSem
161900101026                       : pOutCliPerc
162000101026                       : pOutCliEml1
162100101026                       : pOutCliEml2
162200101026                       : pOutCliEml3
162300101026                       : pOutCliEml4
162400101026                       : pOutCliEml5
162500101026                       : pOutCliSogg
162600101026                       : pOutCliTesto
162700101026                       : pOutCliFtpAddr
162800101026                       : pOutCliFtpDir
162900101026                       : pOutCliFtpUsr
163000101026                       : pOutCliFtpPwd
163100101026                       : pOutCliFtpFAS4
163200101026                       : pOutCliPercOld
163300101026                       : pOutCliTentat
163400101026                       : pOutCliRitardo
163500101026                       : pOutCliLibAS4
163600101026                       : pOutCliFilAS4
163700101026                       : pOutCliMbrAS4
163800101026                       : pOutCliProc
163900101026                       : pOutCliSched
164000101026                       : pOutCliAccorp
164100101026                       : pOutCliMskZip
164200101026                       : pOutCliDataStr
164300101026                       : pOutCliDataEnd
164400101026                       : pOutCliCodComp
164500101026                       : pOutCliFtpCmd1
164600101026                       : pOutCliEmlErr
164700101026                       : pOutCliHtUrlTo
164800101026                       : pOutCliHtUsrTo
164900101026                       : pOutCliHtPwdTo
165000101026                       : pOutCliHtConTy
165100101026                       : pOutCliHtSndEx
165200101026                       : pOutCliHtSndPr
165300101026                       : pOutCliHtUsPxy
165400101026                       : pOutCliUsrIns
165500101026                       : pOutCliUsrUpd
165600101026                       : pOutCliDatUpd
165700101026                       : pOutCliNote
165800101026                       : pOutCliFtpPrt
165900101026                       : pOutCliFtpTy) = 0;
166000101026              if pOutNumRec = 1;
166100101026                 VIDPPIMC = pOutCliMskChk;
166200101026                 if %len(%trim(pOutCliMskChk)) > %size(VIDPPIMC);
166300101026                    *in30 = *on;
166400101026                 endif;
166500101026                 VIDPPIMF = pOutCliMskFil;
166600101026                 if %len(%trim(pOutCliMskFil)) > %size(VIDPPIMF);
166700101026                    *in31 = *on;
166800101026                 endif;
166900101026                 VIDPPIMS = pOutCliMskSem;
167000101026                 if %len(%trim(pOutCliMskSem)) > %size(VIDPPIMS);
167100101026                    *in32 = *on;
167200101026                 endif;
167300101026                 VIDPPIMZ = pOutCliMskZip;
167400101026                 if %len(%trim(pOutCliMskZip)) > %size(VIDPPIMZ);
167500101026                    *in33 = *on;
167600101026                 endif;
167700101026              endif;
167800101026         endif;
167900130214       endif;
168000101026
168100101026      /END-FREE
168200101026     C
168300101026     C                   ELSE
168400101026     C                   SETON                                        44
168500101026     C                   ENDIF
168600101026     C*
168700980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
168800980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
168900980128E   3C                   ENDIF
169000970526E   2C                   ENDIF
169100970526X   1C                   ELSE                                                   *INSERIMENTO
169200970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
169300970526E   1C                   ENDIF
169400970526     C*
169500970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
169600970526IF  1C     DEPOP1        IFEQ      'O02'
169700970526     C                   MOVEL     DEC(1)        VIDFUN
169800970526E   1C                   ENDIF
169900991014IF  1C     DEPOP1        IFEQ      'O03'
170000991014     C                   MOVEL     DEC(6)        VIDFUN
170100991014E   1C                   ENDIF
170200970526IF  1C     DEPOP1        IFEQ      'O04'
170300970526     C                   MOVEL     DEC(2)        VIDFUN
170400970526E   1C                   ENDIF
170500970526IF  1C     DEPOP1        IFEQ      'O05'
170600970526     C                   MOVEL     DEC(3)        VIDFUN
170700970526E   1C                   ENDIF
170800970526IF  1C     DEPOP1        IFEQ      'O07'
170900970526     C                   MOVEL     DEC(4)        VIDFUN
171000970526E   1C                   ENDIF
171100970526IF  1C     DEPOP1        IFEQ      'F06'
171200970526     C                   MOVEL     DEC(5)        VIDFUN
171300970526E   1C                   ENDIF
171400970526     C*
171500970526     C                   ENDSR
171600101026
171700101026
171800101026
171900101026     C*--------------------------------------------------------------------------------------------*
172000101026     C* CARTAB - carica le tabelle occorrenti
172100101026     C*--------------------------------------------------------------------------------------------*
172200101026     C     CARTAB        BEGSR
172300101026     C*
172400101026     C* Tipi servizi soggetti a controllo unificazioni
172500101026     C                   z-add     *zeros        ww                3 0
172600101026     C                   eval      tbecod = 'ISV'
172700101026     C     tbecod        setll     tntbe01l
172800101026     C     tbecod        reade     tntbe01l
172900101026     C                   dow       not %eof(tntbe01l)
173000101026     C                   movel     tbeuni        disv
173100101026     C                   if        §isvproc = 'F'
173200101026     C                   add       1             ww
173300101026     C                   eval      isvftpeml(ww) = tbeke1
173400101026     C                   endif
173500101026     C     tbecod        reade     tntbe01l
173600101026     C                   enddo
173700101026     C*
173800101026     C                   ENDSR
173900101026
174000101026     c*------------------------------------------------------------------------*
174100101026     c* opnCONN - Apertura connessione
174200101026     c*------------------------------------------------------------------------*
174300101026     c     OPNCONN       BEGSR
174400101026      /FREE
174500101026
174600101026        if not *in40;
174700101026           if UBBVSQL01_Connect('BARTVAS' : pOutConn) = 0;
174800101026              pInConn = pOutConn;
174900101026           else;
175000101026              *in40 = *on;
175100101026              DSMSMS = MSG(28);
175200101026           endif;
175300101026        endif;
175400101026
175500101026      /END-FREE
175600101026      *
175700101026     c                   ENDSR
175800101026     c*------------------------------------------------------------------------*
175900101026
176000101026
176100101026
176200101026     c*------------------------------------------------------------------------*
176300101026     c* clsCONN - Chiusura connessione
176400101026     c*------------------------------------------------------------------------*
176500101026     c     CLSCONN       BEGSR
176600101026      /FREE
176700101026
176800101026           if UBBVSQL01_Disconnect(pInConn) = 0;
176900101026           else;
177000101026              *in40 = *on;
177100101026           endif;
177200101026
177300101026      /END-FREE
177400101026      *
177500101026     c                   ENDSR
177600101026     c*------------------------------------------------------------------------*
177700101026
177800101026
177900101026
178000970526     C*------------------------------------------------------------------------*
178100970526     C* *INZSR - OPERAZIONI INIZIALI
178200970526     C*------------------------------------------------------------------------*
178300970526     C     *INZSR        BEGSR
178400970526     C*---
178500970526     C* RICEVIMENTO PARAMETRI
178600970526     C*---
178700970526     C     *ENTRY        PLIST
178800970612     C                   PARM                    KPJBA
178900991028     C                   movel     kpjbu         tis720
179000000420     C*
179100000420     C* Inverto subito la data presa in input
179200000420     C                   Z-ADD     s20dti        wda8
179300000420     C                   Z-ADD     *ZEROS        da8w
179400000420     C                   EXSR      CHKDA8
179500000420     C                   Z-ADD     DA8W          KCEdti
179600130215     C*
179700130215     C     *DTAARA       DEFINE    §azute        azuteds
179800130215     C     *DTAARA       DEFINE    §datiute      ddatiute
179900130215     C                   IN(E)     *DTAARA
180000130215if  1C                   IF        %Error
180100130215     c                   EVAL      I34Tla = 'L'
180200130215     C                   CALL      'TIBS34R'
180300130215     C                   PARM                    Tibs34Ds
180400130215     C                   IN        *DTAARA
180500130215e   1C                   ENDIF
180600970526     C*
180700970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
180800970526     C     XMSGPA        PLIST
180900970526     C                   PARM                    DSMSMS
181000970526     C                   PARM                    DSMSPG
181100970526     C                   PARM                    DSMSKE
181200970526     C                   PARM                    DSMSER
181300991108     C*---
181400970526     C*
181500970526     C* IMPOSTA LA DS -TASTI PREMUTI-
181600970526     C                   CALL      'XKEY'
181700970526     C                   PARM                    DSKEY
181800101026     C*
181900101026     C* CARICA LE TABELLE OCCORRENTI
182000101027     C                   EXSR      CARTAB
182100111215     C*
182200111215     C* Reperisco il sistema AS/400 corrente
182300111215     C                   EVAL      AS400_ok = *off
182400111215     C                   callp     UBRTVNETA_Rtv(currSysNeta)
182500111215     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
182600111215     C                   EVAL      AS400_ok = *on
182700111215     C                   ENDIF
182800101026     C*
182900130214     C* Apro la connessione al DB "BARTVAS" (solo se no solo visualizzazione)
183000130215     C* (questa attività costa risorse, per cui si permette solo in gestione oppure anche in
183100130215     C*  visualizzazione se l'utente è di sede;
183200130215     C*  testo solo se l'utente è di sede, perché quelli di filiale non possono andare in gestione,
183300130215     C*  quindi non potrebbero comunque)
183400130215     C                   IF        DUtLPO = 'S'
183500111215     C                   IF        AS400_ok = *on
183600111215     C                   exsr      opnCONN
183700111215     C                   ENDIF
183800130214     C                   ENDIF
183900970526     C*---
184000970526     C* VARIABILI NON RIFERITE AL DATA BASE
184100970526     C*---
184200970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
184300991025     C     *LIKE         DEFINE    S20OP0        DEPOP0
184400991025     C     *LIKE         DEFINE    S20OP1        DEPOP1
184500991025     C     *LIKE         DEFINE    S20OP0        WOP0
184600991025     C     *LIKE         DEFINE    S20OP1        WOP1
184700970526     C*---
184800970526     C* VARIABILI DI CONTROLLO
184900970526     C*---
185000970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
185100970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
185200970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
185300970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
185400970526     C*
185500970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
185600991026     C                   MOVEL     'TIS723R'     DSMSPG
185700991026     C                   MOVEL     'TIS723R'     MSGQUE
185800970526     C                   MOVEL     *BLANKS       MSGKEY
185900970526     C                   MOVEL     *BLANKS       DSMSMS
186000970526     C*---
186100970526     C* VARIABILI RIFERITE AL DATA BASE
186200970526     C*---
186300991026     C     *LIKE         DEFINE    virtip        KCEtip
186400991026     C     *LIKE         DEFINE    virksc        KCEksc
186500000420     C     *LIKE         DEFINE    virdti        KCEdti
186600970526     C*---
186700970526     C* CHIAVI DI LETTURA
186800970526     C*---
186900000420     C* LETTURA tivir03L - INCOMPLETA
187000991026     C     KEYvir        KLIST
187100080603     C                   KFLD                    KCEtip
187200080603     C                   KFLD                    KCEksc
187300080603     C* LETTURA tivir03L - COMPLETA
187400080603     C     KEYvi2        KLIST
187500080603     C                   KFLD                    KCEtip
187600080603     C                   KFLD                    KCEksc
187700080603     C                   KFLD                    KCEdti
187800080603     C* LETTURA tivire1L - COMPLETA
187900080603     C     KEYvire1_C    KLIST
188000080603     C                   KFLD                    KCEksc
188100080603     C                   KFLD                    KCEtip
188200080603     C                   KFLD                    KCEdti
188300080603     C                   KFLD                    vireTRC
188400080603     C* LETTURA tntbe01L - PARZIALE
188500040816     C     KEYtbe1_P     KLIST
188600040816     C                   KFLD                    TBEcod                         *CODICE TABELLA
188700120111     C* LETTURA tivir06L - COMPLETA
188800120111     C     K03VIR06      KLIST
188900120111     C                   KFLD                    x_VIRKsc
189000120111     C                   KFLD                    x_VIRTip
189100120111     C                   KFLD                    x_VIRDtF
189200120111     C* LETTURA tivir06L - PARZIALE
189300120111     C     K02VIR06      KLIST
189400120111     C                   KFLD                    x_VIRKsc
189500120111     C                   KFLD                    x_VIRTip
189600970528     C*
189700970528     C* CALCOLA LA DATA CORRENTE
189800970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
189900970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
190000970528     C                   Z-ADD     WN8           G08DAT
190100051122     C                   Z-ADD     WN8           DATCORINV         8 0
190200970528     C                   Z-ADD     *ZEROS        G08INV
190300970528     C                   MOVEL     '0'           G08ERR
190400970528     C                   CALL      'XSRDA8'
190500970528     C                   PARM                    WLBDA8
190600970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
190700970526     C*
190800970526     C* DEPOSITA LE OPZIONI DI ENTRATA
190900991025     C                   MOVEL     S20OP0        DEPOP0                         *LIVELLO PROCEDURA
191000991025     C                   MOVEL     S20OP1        DEPOP1                         *LIVELLO PROGRAMMA
191100051122     C*
191200051122     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
191300051122     C                   IF        DEPOP1 = 'F06' OR
191400051122     C                             DEPOP1 = 'O03' OR
191500051122     C                             DEPOP1 = 'O07'
191600051122     C                   EVAL      VIDDTI = DATCORINV
191700060726     C                   EVAL      VIDDTF = 31122039
191800051122     C                   ENDIF
191900000629     C*
192000000629     C* EFFETTUO CONSIDERAZIONI SULLE OPZIONI DI ENTRATA
192100000629     C                   IF        DEPOP1 = 'F06' OR
192200000629     C                             DEPOP1 = 'O03'
192300000629     C                   SETOFF                                       92
192400000629     C                   ELSE
192500000629     C                   SETON                                        92
192600000629     C                   ENDIF
192700970526     C*
192800970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
192900970526     C                   EXSR      IMPFUN
193000970526     C*
193100970526     C                   ENDSR
193200970526     C*------------------------------------------------------------------------*
193300970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
193400970526     C*------------------------------------------------------------------------*
193500970526     C     FUNGES        BEGSR
193600970526     C*
193700970526     C* PULISCO CAMPI DI LAVORO
193800970526     C                   Z-ADD     0             W                 5 0
193900970526     C*
194000970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
194100970526     C* E CARICO TABELLA FUNZIONI ATTIVE
194200970526     C     1             DO        24            I
194300970526     C                   MOVEL     FUD(I)        FUDRIG
194400970526     C*
194500970526     C* PULIZIA FUNZIONI PRECEDENTI
194600970526     C                   MOVEL     *BLANKS       FUV(I)
194700970526     C*
194800970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
194900970526     C* (SALTA F24)
195000970526     C     FUDATT        IFEQ      'S'
195100970526     C     FUDFUN        ANDNE     '24'
195200970526     C                   ADD       1             W
195300970526     C                   MOVEL     FUDDES        FUV(W)
195400970526     C                   END
195500970526     C                   END                                                    DO WHILE
195600970526     C*
195700970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
195800970526     C* ALTRIMENTI DISATTIVO F24
195900970526     C     1             DO        24            I
196000970526     C                   MOVEL     FUD(I)        FUDRIG
196100970526     C*
196200970526     C     FUDFUN        IFEQ      '24'
196300970526     C     W             IFGT      6
196400970526     C                   MOVEL     'S'           FUDATT
196500970526     C                   ELSE
196600970526     C                   MOVEL     *BLANKS       FUDATT
196700970526     C                   END                                                    W GT 8
196800970526     C                   MOVEL     FUDRIG        FUD(I)
196900970526     C                   END
197000970526     C                   END                                                    DO WHILE
197100970526     C*
197200970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
197300970526     C     W             DIV       6             I
197400970526     C                   MVR                     Z
197500970526     C*
197600970526     C     Z             IFNE      0
197700970526     C     I             ADD       1             Z
197800970526     C                   ELSE
197900970526     C                   Z-ADD     I             Z                 5 0
198000970526     C                   END
198100970526     C*
198200970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
198300970526     C                   ADD       1             FUNGRU
198400970526     C     FUNGRU        IFGT      Z
198500970526     C     FUNGRU        ORLT      1
198600970526     C                   Z-ADD     1             FUNGRU
198700970526     C                   END
198800970526     C*
198900970526     C* PRIMO GRUPPO DI 6 FUNZIONI
199000970526     C     FUNGRU        IFEQ      1
199100970526     C                   MOVEL     FUV(1)        FUN001
199200970526     C                   MOVEL     FUV(2)        FUN002
199300970526     C                   MOVEL     FUV(3)        FUN003
199400970526     C                   MOVEL     FUV(4)        FUN004
199500970526     C                   MOVEL     FUV(5)        FUN005
199600970526     C                   MOVEL     FUV(6)        FUN006
199700970526     C                   END
199800970526     C*
199900970526     C* SECONDO GRUPPO DI 6 FUNZIONI
200000970526     C     FUNGRU        IFEQ      2
200100970526     C                   MOVEL     FUV(7)        FUN001
200200970526     C                   MOVEL     FUV(8)        FUN002
200300970526     C                   MOVEL     FUV(9)        FUN003
200400970526     C                   MOVEL     FUV(10)       FUN004
200500970526     C                   MOVEL     FUV(11)       FUN005
200600970526     C                   MOVEL     FUV(12)       FUN006
200700970526     C                   END
200800970526     C*
200900970526     C* TERZO GRUPPO DI 6 FUNZIONI
201000970526     C     FUNGRU        IFEQ      3
201100970526     C                   MOVEL     FUV(13)       FUN001
201200970526     C                   MOVEL     FUV(14)       FUN002
201300970526     C                   MOVEL     FUV(15)       FUN003
201400970526     C                   MOVEL     FUV(16)       FUN004
201500970526     C                   MOVEL     FUV(17)       FUN005
201600970526     C                   MOVEL     FUV(18)       FUN006
201700970526     C                   END
201800970526     C*
201900970526     C* QUARTO GRUPPO DI 6 FUNZIONI
202000970526     C     FUNGRU        IFEQ      4
202100970526     C                   MOVEL     FUV(19)       FUN001
202200970526     C                   MOVEL     FUV(20)       FUN002
202300970526     C                   MOVEL     FUV(21)       FUN003
202400970526     C                   MOVEL     FUV(22)       FUN004
202500970526     C                   MOVEL     *BLANKS       FUN005
202600970526     C                   MOVEL     *BLANKS       FUN006
202700970526     C                   END
202800970526     C*
202900970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
203000970526     C                   MOVEL     *BLANKS       FUN007
203100970526     C                   MOVEL     *BLANKS       FUN008
203200970526     C*
203300970526     C     1             DO        24            I
203400970526     C                   MOVEL     FUD(I)        FUDRIG
203500970526     C     FUDFUN        IFEQ      '24'
203600970526     C     FUDATT        ANDEQ     'S'
203700970526     C                   MOVEL     FUDDES        FUN008
203800970526     C                   END
203900970526     C                   END                                                    DO
204000970526     C*
204100970526     C                   ENDSR
204200970526     C*------------------------------------------------------------------------*
204300970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
204400970526     C*------------------------------------------------------------------------*
204500970526     C     FUNCON        BEGSR
204600970526     C*
204700970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
204800970526     C                   EXSR      FUNVAR
204900970526     C*
205000970526     C* PULISCO CAMPI DI LAVORO
205100970526     C                   Z-ADD     0             I                 5 0
205200970526     C                   Z-ADD     0             W                 5 0
205300970526     C*
205400970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
205500970526     C     FUNKEY        IFNE      *BLANKS
205600970526     C     1             DO        24            I
205700970526     C                   MOVEL     FUD(I)        FUDRIG
205800970526     C*
205900970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
206000970526     C     FUNKEY        IFEQ      FUDFUN
206100970526     C     FUDATT        ANDEQ     'S'
206200970526     C                   Z-ADD     I             W
206300970526     C                   END
206400970526     C                   END                                                    DO WHILE
206500970526     C*
206600970526     C* FUNZIONE NON ATTIVA (ERRORE)
206700970526     C     W             IFEQ      0
206800970526     C*
206900970526     C* EMISSIONE MESSAGGIO DI ERRORE
207000970526     C                   MOVEL     MSG(1)        DSMSMS
207100970526     C*
207200970526     C                   END                                                    W=0
207300970526     C                   END                                                    FUNKEY NE *BLANKS
207400970526     C*
207500970526     C* FUNZIONE ATTIVA (PROCEDI ...)
207600970526     C     W             IFNE      0
207700970526     C     FUNKEY        OREQ      *BLANKS
207800970526     C     FUNKEY        IFEQ      *BLANKS
207900970526     C                   EXSR      ENTGES
208000970526     C                   END
208100031229     C     FUNKEY        IFEQ      '01'
208200031229     C                   EXSR      F01GES
208300031229     C                   END
208400970526     C     FUNKEY        IFEQ      '03'
208500970526     C                   EXSR      F03GES
208600970526     C                   END
208700970526     C     FUNKEY        IFEQ      '06'
208800970526     C                   EXSR      F06GES
208900970526     C                   END
209000970526     C     FUNKEY        IFEQ      '12'
209100970526     C                   EXSR      F12GES
209200970526     C                   END
209300970526     C     FUNKEY        IFEQ      '24'
209400970526     C                   EXSR      F24GES
209500970526     C                   END
209600970526     C                   END
209700970526     C*
209800970526     C                   ENDSR
209900970526     C*------------------------------------------------------------------------*
210000970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
210100970526     C*------------------------------------------------------------------------*
210200970526     C     FUNVAR        BEGSR
210300970526     C*
210400970526     C* INIZIALIZZA VARIABILE -FUNKEY-
210500970526     C*
210600970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
210700970526     C     KEY           IFEQ      ENTER
210800970526     C                   MOVEL     *BLANKS       FUNKEY
210900970526     C                   END
211000970526     C     KEY           IFEQ      F1
211100970526     C                   MOVEL     '01'          FUNKEY
211200970526     C                   END
211300970526     C     KEY           IFEQ      F2
211400970526     C                   MOVEL     '02'          FUNKEY
211500970526     C                   END
211600970526     C     KEY           IFEQ      F3
211700970526     C                   MOVEL     '03'          FUNKEY
211800970526     C                   END
211900970526     C     KEY           IFEQ      F4
212000970526     C                   MOVEL     '04'          FUNKEY
212100970526     C                   END
212200970526     C     KEY           IFEQ      F5
212300970526     C                   MOVEL     '05'          FUNKEY
212400970526     C                   END
212500970526     C     KEY           IFEQ      F6
212600970526     C                   MOVEL     '06'          FUNKEY
212700970526     C                   END
212800970526     C     KEY           IFEQ      F7
212900970526     C                   MOVEL     '07'          FUNKEY
213000970526     C                   END
213100970526     C     KEY           IFEQ      F8
213200970526     C                   MOVEL     '08'          FUNKEY
213300970526     C                   END
213400970526     C     KEY           IFEQ      F9
213500970526     C                   MOVEL     '09'          FUNKEY
213600970526     C                   END
213700970526     C     KEY           IFEQ      F10
213800970526     C                   MOVEL     '10'          FUNKEY
213900970526     C                   END
214000970526     C     KEY           IFEQ      F11
214100970526     C                   MOVEL     '11'          FUNKEY
214200970526     C                   END
214300970526     C     KEY           IFEQ      F12
214400970526     C                   MOVEL     '12'          FUNKEY
214500970526     C                   END
214600970526     C     KEY           IFEQ      F13
214700970526     C                   MOVEL     '13'          FUNKEY
214800970526     C                   END
214900970526     C     KEY           IFEQ      F14
215000970526     C                   MOVEL     '14'          FUNKEY
215100970526     C                   END
215200970526     C     KEY           IFEQ      F15
215300970526     C                   MOVEL     '15'          FUNKEY
215400970526     C                   END
215500970526     C     KEY           IFEQ      F16
215600970526     C                   MOVEL     '16'          FUNKEY
215700970526     C                   END
215800970526     C     KEY           IFEQ      F17
215900970526     C                   MOVEL     '17'          FUNKEY
216000970526     C                   END
216100970526     C     KEY           IFEQ      F18
216200970526     C                   MOVEL     '18'          FUNKEY
216300970526     C                   END
216400970526     C     KEY           IFEQ      F19
216500970526     C                   MOVEL     '19'          FUNKEY
216600970526     C                   END
216700970526     C     KEY           IFEQ      F20
216800970526     C                   MOVEL     '20'          FUNKEY
216900970526     C                   END
217000970526     C     KEY           IFEQ      F21
217100970526     C                   MOVEL     '21'          FUNKEY
217200970526     C                   END
217300970526     C     KEY           IFEQ      F22
217400970526     C                   MOVEL     '22'          FUNKEY
217500970526     C                   END
217600970526     C     KEY           IFEQ      F23
217700970526     C                   MOVEL     '23'          FUNKEY
217800970526     C                   END
217900970526     C     KEY           IFEQ      F24
218000970526     C                   MOVEL     '24'          FUNKEY
218100970526     C                   END
218200970526     C     KEY           IFEQ      ROLLUP
218300970526     C                   MOVEL     'RU'          FUNKEY
218400970526     C                   END
218500970526     C     KEY           IFEQ      HELP
218600970526     C                   MOVEL     'HP'          FUNKEY
218700970526     C                   END
218800970526     C                   ENDSR
218900970526     C*------------------------------------------------------------------------*
219000970526     C* NOTE COSTRUZIONE SCHIERE
219100970526     C*------------------------------------------------------------------------*
219200970526     C*
219300970526     C* -MSG-MESSAGGI
219400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
219500970526     C* (NON TOCCARE MSG,1 E MSG,2)
219600970526     C*
219700970526     C* -CMD-COMANDI
219800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
219900970526     C*
220000970526     C* -OPD-OPZIONI
220100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
220200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
220300970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
220400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
220500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
220600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
220700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
220800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
220900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
221000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
221100970526     C*
221200970526     C* -FUD-FUNZIONI
221300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
221400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
221500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
221600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
221700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
221800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
221900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
222000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
222100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
222200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
222300970526     O*------------------------------------------------------------------------*
222400970526** DEC - DECODIFICHE
222500970526    MODIFICA                                                      1
222600970526  ANNULLAMENTO                                                    2
222700970526VISUALIZZAZIONE                                                   3
222800970526   RIPRISTINO                                                     4
222900970526  INSERIMENTO                                                     5
223000991014     COPIA                                                        6
223100970526** MSG - MESSAGGI
223200991026TIS723R- Tasto funzionale non gestito                                          1
223300991026TIS723R- Inserimento effettuato                                                2
223400991026TIS723R- Modifica effettuata                                                   3
223500991026TIS723R- Annullamento effettuato                                               4
223600991026TIS723R- Ripristino effettuato                                                 5
223700991026TIS723R- Richiesta funzione per record inesistente                             6
223800991026TIS723R- Record momentaneamente allocato: riprovare più tardi                  7
223900991026TIS723R- Richiesta funzione per record esistente                               8
224000991026TIS723R- Questo codice cliente è già presente con questo tipo file             9
224100991027TIS723R- Il codice tipo file non è fra quelli presenti in anagrafico           10
224200991027TIS723R- Il codice cliente non è presente come padre nell'archivio UNIFICANTI  11
224300000724TIS723R- Campo obbligatorio                                                    12
224400991027TIS723R- Codice filiale inesistente o di categoria V=varia                     13
224500000724TIS723R- Data errata                                                           14
224600000724TIS723R- La data fine deve essere maggiore della data inizio                   15
224700000724TIS723R- Il record non può scadere in un giorno già trascorso                  16
224800000724TIS723R- Il record immesso si sovrappone ad uno già in decorrenza              17
224900000724TIS723R- Codice tipo servizio non presente in tabella                          18
225000001120TIS723R- Oggetto inesistente, verificare e correggere.                         19
225100001120TIS723R- File inesistente, verificare e correggere.                            20
225200001120TIS723R- Libreria inesistente, verificare e correggere.                        21
225300030603TIS723R- Tipo membro non impostabile x il tipo servizio indicato.              22
225400040107TIS723R- Per il traduttore indicato non esiste descrizione dettaglio parametri 23
225500040319TIS723R- Tipo file e servizio incompatibili, correggere.                       24
225600040816TIS723R- Tipo file e/o programma traduttore e/o file tradotto incompatibili.   25
225700051026TIS723R- ATTENZIONE presenti log a stato 0 impossibile cambiare traduttore!    26
225800060616TIS723R- Valori incompatibili, correggere!                                     27
225900101026TIS723R- Errore BartVAS: fatal error                                           28
226000101027TIS723R- ATTENZIONE: modifica effettuata lato AS/400 ma non lato BARTVAS       29
226100120109TIS723R- Tipo file per Internet non impostabile x il tipo servizio indicato.   30
226200120119TIS723R- Tipo file non abilitato per cliente indicato                          31
226300120119TIS723R- Loop tra il tipo file / cliente corrente e quello per duplica         32
226400120119TIS723R- Tipo file / cliente per duplica non possono essere quelli correnti    33
226500120119TIS723R- Valorizzato il cliente per duplica ma non il rispettivo tipo file     34
226600970526** FUD - FUNZIONI
226700031229F01=Parametri      -        (S/N)=S  <== S-FISSO           funz.01¬
226800970526                   -        (S/N)=N                        funz.02¬
226900970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
227000970526                   -        (S/N)=N                        funz.04¬
227100970526                   -        (S/N)=N                        funz.05¬
227200970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
227300970527                   -        (S/N)=N                        funz.07¬
227400970527                   -        (S/N)=N                        funz.08¬
227500970527                   -        (S/N)=N                        funz.09¬
227600970527                   -        (S/N)=N                        funz.10¬
227700970526                   -        (S/N)=N                        funz.11¬
227800970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
227900970526                   -        (S/N)=N                        funz.13¬
228000970526                   -        (S/N)=N                        funz.14¬
228100970526                   -        (S/N)=N                        funz.15¬
228200970526                   -        (S/N)=N                        funz.16¬
228300970526                   -        (S/N)=N                        funz.17¬
228400970526                   -        (S/N)=N                        funz.18¬
228500970526                   -        (S/N)=N                        funz.19¬
228600970526                   -        (S/N)=N                        funz.20¬
228700970526                   -        (S/N)=N                        funz.21¬
228800970526                   -        (S/N)=N                        funz.22¬
228900970526                   -        (S/N)=N                        funz.23¬
229000970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
