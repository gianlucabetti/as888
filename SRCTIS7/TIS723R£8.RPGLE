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
009300141111     D TRULKPJDS     E DS
009400120109     D TIVTF00F      E DS                  extname(TIVTF00F)
009500120109     D TIVTF_Save      DS           300
009600970526     D*------------
009700970526     D* ARCHITETTURA
009800970526     D*------------
009900970526     D KPJBA         E DS
010000080603     D*------------
010100080603     D* ESTENSIONE ANAGRAFICA CLIENTI/TIPI FILE
010200080603     D*------------
010300080603     D DVIRE01       E DS
010400141111     D DVIREPE       E DS
010500040816     D*------------
010600040816     D* DS REPERIMENTO DATI CLIENTI
010700040816     D*------------
010800040816     DBS69DS         e ds                  extname(TIBS69DS)
010900040816     DACODS          e ds                  extname(cnaco00f)
011000040816     DINDDS          e ds                  extname(cnind00f)
011100040816     DCLPDS          e ds                  extname(cnclp00f)
011200040816     DCLSDS          e ds                  extname(fncls00f)
011300081020
011400081020     D*------------
011500081020     D* DS x richiamo *pgm utilità d controllo indirizzi e-mail
011600081020     D*------------
011700081020     D DSEMAIL       E DS
011800040816     D*------------
011900040816     D* VARIABILI D WRK
012000040816     D*------------
012100040816     D ksc             s              7  0 dim(30)
012200040816     D ksa             s              4    dim(30)
012300101026     D isvftpeml       s              2    dim(100)
012400040816     D skTipF          s              2    dim(500)
012500040816     D skPgt           s             10    dim(500)
012600040816     D skFileT         s             10    dim(500)
012700100311     D skMembroT       s             10    dim(500)
012800040816     D j               s              3  0 inz
012900040816     D wVIDPGT         s                   like(VIDPGT)
013000040816     D wVIDFIT         s                   like(VIDFIT)
013100120109     D MaxCiclo        s              3  0 inz
013200120119     D* Tips e Ksc DEVONO avere lo stesso numero di elementi
013300120109     D Tips            s              2    dim(3)
013400120119     D Kscs            s              8    dim(3)
013500111215     D*------------------
013600111215     D* Variabili x PASSAGGIO PARAMETRI A PROCEDURE UBRTVNETA
013700111215     D*------------------
013800111215     D currSysNeta     s              8A   inz('*NULL')
013900111215     D AS400_ok        s              1A
014000111215     D  PiOutSysNeta   S              8A
014100101026     D*-------------------
014200101026     D* DEFINIZIONI PROCEDURE ESTERNE
014300101026     D*-------------------
014400101026     D/COPY GAITRASRC/SRCPROTOPI,UBBVSQL01
014500101026     D/COPY GAITRASRC/SRCPROTOPR,UBBVSQL01
014600101027     D/COPY GAITRASRC/SRCPROTOPI,UBBVSKT01
014700101027     D/COPY GAITRASRC/SRCPROTOPR,UBBVSKT01
014800111215     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
014900120120     D/COPY GAITRASRC/SRCPROTOPR,TIS723R2U
015000970526     C*------------------------------------------------------------------------*
015100970526     C* MAIN LINES
015200970526     C*------------------------------------------------------------------------*
015300970526     C*
015400970526     C* NON CI SONO ERRORI IN ENTRATA
015500970526IF  1C     ERRL00        IFEQ      '0'
015600970526     C*
015700970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
015800970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
015900970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
016000970526     C     FINL00        OREQ      '1'                                          *FINE PGM
016100970526     C*
016200970526     C* EMETTE FORMATO VIDEO
016300970526     C                   EXSR      BUTFMR
016400970526     C*
016500970526     C* GESTIONE TASTI FUNZIONALI
016600970526     C                   EXSR      FUNCON
016700970526E   2C                   ENDDO
016800970526     C*
016900970526     C* CI SONO ERRORI IN ENTRATA
017000970527X   1C                   ELSE
017100991025     C                   MOVEL     '1'           S20err
017200991025     C                   MOVEL     DSMSMS        S20msg
017300970526E   1C                   ENDIF
017400970526     C* FINE PGM
017500991210     c                   exsr      finsr
017600101026     C*
017700101026     C* Chiudo la connessione
017800130215     C* (questa attività costa risorse, per cui si permette solo in gestione oppure anche in
017900130215     C*  visualizzazione se l'utente è di sede;
018000130215     C*  testo solo se l'utente è di sede, perché quelli di filiale non possono andare in gestione,
018100130215     C*  quindi non potrebbero comunque)
018200130215     C                   IF        DUtLPO = 'S'
018300111215     C                   IF        AS400_ok = *on
018400111215     C                   exsr      clsCONN
018500111215     C                   ENDIF
018600130214     C                   ENDIF
018700101026     C*
018800970526     C                   SETON                                        LR
018900970526     C*------------------------------------------------------------------------*
019000970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
019100970526     C*------------------------------------------------------------------------*
019200970526     C     BUTFMR        BEGSR
019300970526     C*
019400970526     C* EMETTE LA TESTATA
019500991026     C                   WRITE     s723D01
019600970526     C*
019700970526     C* SPEDISCE EVENTUALE MESSAGGIO
019800970526     C                   EXSR      SNDMSG
019900970526     C*
020000970526     C* EMETTE LE FUNZIONI
020100991026     C                   WRITE     s723D02
020200970526     C*
020300970526     C* EMETTE IL VIDEO
020400991026     C                   EXFMT     s723D03
020500970526     C*
020600970526     C* REIMPOSTA FLAG DI ERRORE
020700970526     C                   MOVEL     '0'           ERRL00
020800970526     C*
020900970526     C* REIMPOSTA INDICATORI DI ERRORE
021000120119     C                   SETOFF                                       495051
021100120119     C                   SETOFF                                       525354
021200120119     C                   SETOFF                                       555657
021300970526     C                   SETOFF                                         5859
021400970526     C                   SETOFF                                       606162
021500970526     C                   SETOFF                                       636465
021600970526     C                   SETOFF                                       666768
021700970526     C                   SETOFF                                       697071
021800970526     C                   SETOFF                                       727374
021900120109     C                   SETOFF                                       757677
022000970526     C                   SETOFF                                       787980
022100970526     C                   SETOFF                                       818283
022200970526     C                   SETOFF                                       848586
022300970526     C                   SETOFF                                       878889
022400970526     C*
022500970526     C                   ENDSR
022600970526     C*------------------------------------------------------------------------*
022700970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
022800970526     C*------------------------------------------------------------------------*
022900970526     C     SNDMSG        BEGSR
023000970526     C*
023100970526     C* PULISCE IL SFILE MESSAGGI
023200970526     C                   MOVEL     '1'           *IN40
023300991026     C                   WRITE     s723DMC
023400970526     C                   MOVEL     '0'           *IN40
023500970526     C*
023600970526     C* VISUALIZZA IL MESSAGGIO
023700970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
023800970526     C                   CALL      'XMSG'        XMSGPA
023900970526IF  1C     DSMSER        IFEQ      *ZEROS
024000970526     C                   MOVEL     DSMSKE        MSGKEY
024100970526E   1C                   ENDIF
024200970526     C                   Z-ADD     1             MSGULT
024300991026     C                   WRITE     s723DMS
024400991026     C                   WRITE     s723DMC
024500970526     C*
024600970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
024700970526     C                   MOVEL     *BLANKS       DSMSMS
024800970526     C*
024900970526     C                   ENDSR
025000970526     C*------------------------------------------------------------------------*
025100970526     C* ENTGES - GESTIONE ENTER -> INVIO
025200970526     C*------------------------------------------------------------------------*
025300970526     C     ENTGES        BEGSR
025400970526     C*
025500970526     C* CONTROLLA I CAMPI IMMESSI
025600970526     C                   EXSR      DECODI
025700970526     C*
025800970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
025900970526IF  1C     DEPOP1        IFEQ      'O05'
026000970526     C                   MOVEL     '1'           FINL00
026100970526E   1C                   ENDIF
026200970526     C*
026300970526     C                   ENDSR
026400031229     C*------------------------------------------------------------------------*
026500031229     C* F01GES - GESTIONE F01 -> VISUALIZZA REGOLE PARAMETRI TRADUTTORE
026600031229     C*------------------------------------------------------------------------*
026700031229     C     F01GES        BEGSR
026800031229     C*
026900031229     C* Passo o il traduttore standard relativo al tipo file corrente o se inserito il
027000031229     C* traduttore specifico
027100031229     C                   IF        VIDPGT <> *blanks
027200031229     C                   EVAL      wPGT = VIDPGT
027300031229     C                   ELSE
027400031229     C                   EVAL      wPGT = VIDPGTD
027500031229     C                   ENDIF
027600031229     C*
027700031229     C* Visualizzo le regole relative
027800031229     C                   CALL      'TIS723C'
027900031229     C                   PARM                    wPGT             10
028000040107     C                   PARM                    wERR              1
028100040107     C*
028200040107     C* Verifico se trovato membro con descrizoine dettaglio parametri traduttore
028300040107     C                   IF        wERR = '1'
028400040107     C                   MOVEL     MSG(23)       DSMSMS
028500040107     C                   ENDIF
028600031229     C*
028700031229     C                   ENDSR
028800141111     C*------------------------------------------------------------------------*
028900141111     C* F02GES - GESTIONE F02 -> PARAMETRI AGGIUNTIVI
029000141111     C*------------------------------------------------------------------------*
029100141111     C     F02GES        BEGSR
029200141111     C*
029300141111     C                   clear                   trulkpjds
029400141111     C                   call      'TRULKPJR'
029500141111     C                   parm                    kpjba
029600141111     C                   parm                    trulkpjds
029700141111     C*
029800141111     C* Gestione estensione anagrafica clienti/tipi file => TRC PE
029900141111     C                   CLEAR                   DVIREPE
030000141111     C                   EVAL      KCEdti  = virDTI
030100141111     C                   EVAL      vireTRC = 'PE'
030200141111     C     KEYvire1_C    CHAIN     tivire1l
030300141111     C                   IF        %found(tivire1l)
030400141111     C                   IF        OKPJKPJBU <> *blanks
030500141111     C                   EVAL      §virePEKPJ = OKPJKPJBU
030600141111     C                   EVAL      vireFLO = DVIREPE
030700141111     C                   UPDATE    tivire00
030800141111     C                   ELSE
030900141111     C                   DELETE    tivire00
031000141111     C                   ENDIF
031100141111     C                   ELSE
031200141111     C                   IF        OKPJKPJBU <> *blanks
031300141111     C                   EVAL      vireKSC = virKSC
031400141111     C                   EVAL      vireTIP = virTIP
031500141111     C                   EVAL      vireDTI = virDTI
031600141111     C                   EVAL      vireTRC = 'PE'
031700141111     C                   EVAL      §virePEKPJ = OKPJKPJBU
031800141111     C                   EVAL      vireFLO = DVIREPE
031900141111     C                   WRITE     tivire00
032000141111     C                   ENDIF
032100141111     C                   ENDIF
032200141111     C*
032300141111     C                   ENDSR
032400970526     C*------------------------------------------------------------------------*
032500970526     C* F03GES - GESTIONE F03 -> FINE
032600970526     C*------------------------------------------------------------------------*
032700970526     C     F03GES        BEGSR
032800970526     C*
032900970526     C* IMPOSTA "PREMUTO F03"
033000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
033100991025     C                   MOVEL     '1'           S20F03                         *DI PROCEDURA
033200970526     C*
033300970526     C                   ENDSR
033400970526     C*------------------------------------------------------------------------*
033500970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
033600970526     C*------------------------------------------------------------------------*
033700970526     C     F06GES        BEGSR
033800970526     C*
033900970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
034000970526IF  1C     DEPOP1        IFEQ      'F06'
034100970526     C                   EXSR      INSERI
034200970526E   1C                   ENDIF
034300970526IF  1C     DEPOP1        IFEQ      'O02'
034400970526     C                   EXSR      MODIFI
034500970526E   1C                   ENDIF
034600991014IF  1C     DEPOP1        IFEQ      'O03'
034700011129     C                   EXSR      COPIA
034800991014E   1C                   ENDIF
034900991104IF  1C*    DEPOP1        IFEQ      'O04'
035000991104     C*                  EXSR      ANNULL
035100991104E   1C*                  ENDIF
035200970526IF  1C     DEPOP1        IFEQ      'O07'
035300970526     C                   EXSR      RIPRIS
035400970526E   1C                   ENDIF
035500080603     C*
035600080603     C* Gestione estensione anagrafica clienti/tipi file => TRC 01
035700080603     C                   CLEAR                   DVIRE01
035800080603     C                   EVAL      KCEdti  = virDTI
035900080603     C                   EVAL      vireTRC = '01'
036000080603     C     KEYvire1_C    CHAIN     tivire1l
036100080603     C                   IF        %found(tivire1l)
036200080603     C                   IF        vidEML <> *blanks
036300080603     C                   EVAL      §vire01INV = 'S'
036400080603     C                   EVAL      §vire01EML = vidEML
036500080603     C                   EVAL      vireFLO = DVIRE01
036600080603     C                   UPDATE    tivire00
036700080603     C                   ELSE
036800080603     C                   DELETE    tivire00
036900080603     C                   ENDIF
037000080603     C                   ELSE
037100080603     C                   IF        vidEML <> *blanks
037200080603     C                   EVAL      vireKSC = virKSC
037300080603     C                   EVAL      vireTIP = virTIP
037400080603     C                   EVAL      vireDTI = virDTI
037500080603     C                   EVAL      vireTRC = '01'
037600080603     C                   EVAL      §vire01INV = 'S'
037700080603     C                   EVAL      §vire01EML = vidEML
037800080603     C                   EVAL      vireFLO = DVIRE01
037900080603     C                   WRITE     tivire00
038000080603     C                   ENDIF
038100080603     C                   ENDIF
038200970526     C*
038300011129     C* Dopo aver effettuato le operazioni richieste apro la tabela correlata al tipo file.
038400011129     C                   IF        errl00 = '0'                                 *NO ERRORI
038500011129     C                   CALL      'TIS70UR'
038600011129     C                   PARM                    KPJBA
038700011129     C                   PARM                    VIDTIP
038800011129     C                   PARM                    VIDKSC
038900011129     C                   PARM      *blanks       flgope            1
039000011129     C                   ENDIF
039100011129     C*
039200970526     C                   ENDSR
039300970526     C*------------------------------------------------------------------------*
039400970526     C* F12GES - GESTIONE F12 -> RITORNO
039500970526     C*------------------------------------------------------------------------*
039600970526     C     F12GES        BEGSR
039700970526     C*
039800970526     C* IMPOSTA "PREMUTO F12"
039900970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
040000991025     C                   MOVEL     '1'           S20F12                         *DI PROCEDURA
040100970526     C*
040200970526     C                   ENDSR
040300970526     C*------------------------------------------------------------------------*
040400970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
040500970526     C*------------------------------------------------------------------------*
040600970526     C     F24GES        BEGSR
040700970526     C*
040800970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
040900970526     C*
041000970526     C                   ENDSR
041100970526     C*------------------------------------------------------------------------*
041200970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
041300970526     C*------------------------------------------------------------------------*
041400970526     C     INSERI        BEGSR
041500970526     C*
041600970526     C* CONTROLLA I CAMPI IMMESSI
041700970526     C                   EXSR      DECODI
041800970526     C*
041900970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
042000991026IF  1C                   if        errl00 = '0'                                 *NO ERRORI
042100991026     C                   movel     VIDTip        virtip
042200991026     C                   move      VIDksc        virksc
042300991026     C                   z-add     VIDseq        virseq
042400991026     C                   Z-ADD     viddti        G08dat
042500991026     C                   Z-ADD     *ZEROS        G08inv
042600991026     C                   MOVEL     '0'           G08ERR
042700991026     C                   CALL      'XSRDA8'
042800991026     C                   PARM                    WLBDA8
042900991026     C                   Z-ADD     G08inv        virdti
043000991026     c                   exsr      impvir
043100991026     C                   WRITE     tivir000
043200991025     C                   MOVEL     MSG(2)        S20MSG                         *MESSAGGIO
043300970602     C*
043400970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
043500020228     C****               EXSR      AZZVID
043600970526E   1C                   ENDIF
043700970526     C*
043800970526     C                   ENDSR
043900970526     C*------------------------------------------------------------------------*
044000970526     C* MODIFI - OPERAZIONI DI MODIFICA
044100970526     C*------------------------------------------------------------------------*
044200970526     C     MODIFI        BEGSR
044300970526     C*
044400970526     C* CONTROLLA I CAMPI IMMESSI
044500970526     C                   EXSR      DECODI
044600970526     C*
044700970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
044800970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
044900991026     C                   EXSR      LETvir
045000970526     C*
045100970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
045200970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
045300991026     C                   EXSR      IMPvir                                       *IMPOSTA I CAMPI
045400991026     C                   UPDATE    tivir000
045500101027     C                   EVAL      S20MSG = MSG(3)
045600101027     C                   EVAL      DSMSMS = MSG(3)
045700101027     C*
045800101027     C* Se trattasi d tipo servizio "idoneo" modifico anche i dati utente lato "BARTVAS"
045900101027     C                   IF        *IN80
046000111215     C                             and AS400_ok = *on
046100101027      /FREE
046200101027
046300101027         if *in30 = *off;
046400101027           pInMaskChk  = VIDPPIMC;
046500101027         else;
046600101027           pInMaskChk  = pOutCliMskChk;
046700101027         endif;
046800101027
046900101027         if *in31 = *off;
047000101027           pInMaskFile = VIDPPIMF;
047100101027         else;
047200101027           pInMaskFile = pOutCliMskFil;
047300101027         endif;
047400101027
047500101027         if *in32 = *off;
047600101027           pInMaskSem  = VIDPPIMS;
047700101027         else;
047800101027           pInMaskSem  = pOutCliMskSem;
047900101027         endif;
048000101027
048100101027         if *in33 = *off;
048200101027           pInMaskZip  = VIDPPIMZ;
048300101027         else;
048400101027           pInMaskZip  = pOutCliMskZip;
048500101027         endif;
048600101027
048700101027
048800101027         if UBBVSQL01_Upd01UnqCliTipFile(
048900101027                         pInConn
049000101027                       : VIDKSC
049100101027                       : VIDTIP
049200101027                       : pInMaskChk
049300101027                       : pInMaskFile
049400101027                       : pInMaskSem
049500101027                       : pInMaskZip) = 0;
049600101027         else;
049700101027              S20MSG = MSG(29);
049800101027              DSMSMS = MSG(29);
049900101027         endif;
050000101027
050100101027      /END-FREE
050200101027     C
050300101027     C*
050400101027     C* Quindi (già che c sono ricarico anche il job
050500101027     C                   callp(e)  UBBVSKT01_ReloadCliTipF(
050600101027     C                                           VIDKSC
050700101027     C                                          :VIDTIP
050800101027     C                                          :pOutCliProc)
050900101027     C                   ENDIF
051000101027     C*
051100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
051200970526E   2C                   ENDIF
051300970526E   1C                   ENDIF
051400970526     C*
051500970526     C                   ENDSR
051600991014     C*------------------------------------------------------------------------*
051700991014     C* copia  - OPERAZIONI DI copia
051800991014     C*------------------------------------------------------------------------*
051900991014     C     copia         BEGSR
052000991014     C*
052100991014     C* CONTROLLA I CAMPI IMMESSI
052200991014     C                   EXSR      DECODI
052300991014     C*
052400991014     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
052500991014IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
052600991026     C                   movel     VIDTip        virtip
052700991110     C                   move      VIDksc        virksc
052800991110     C                   z-add     VIDseq        virseq
052900991110     C                   Z-ADD     viddti        G08dat
053000991110     C                   Z-ADD     *ZEROS        G08inv
053100991110     C                   MOVEL     '0'           G08ERR
053200991110     C                   CALL      'XSRDA8'
053300991110     C                   PARM                    WLBDA8
053400991110     C                   Z-ADD     G08inv        virdti
053500991026     C                   EXSR      IMPvir                                       *IMPOSTA I CAMPI
053600991026     C                   write     tivir000
053700991025     C                   MOVEL     MSG(3)        S20MSG
053800991014     C                   MOVEL     '1'           FINL00                         *FINE PGM
053900991014E   2C                   ENDIF
054000991014     C*
054100991014     C                   ENDSR
054200970526     C*------------------------------------------------------------------------*
054300970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
054400970526     C*------------------------------------------------------------------------*
054500991104     C*    ANNULL        BEGSR
054600991025     C*
054700991025     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
054800991104     C*                  EXSR      LETvir
054900991025     C*
055000991025     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
055100991104IF  1C*    ERRL00        IFEQ      '0'                                          *NO ERRORI
055200991104     C*                  DELETE    tivir000                                     *CANCELLA
055300991104     C*                  MOVEL     MSG(4)        s20MSG                         *MESSAGGIO
055400991104     C*                  MOVEL     '1'           FINL00                         *FINE PGM
055500991104E   1C*                  ENDIF
055600970526     C*
055700991104     C*                  ENDSR
055800970526     C*------------------------------------------------------------------------*
055900970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
056000970526     C*------------------------------------------------------------------------*
056100970526     C     RIPRIS        BEGSR
056200970526     C                   ENDSR
056300970526     C*------------------------------------------------------------------------*
056400991026     C* LETvir - LEGGE IL RECORD PERL'AGGIORNAMENTO
056500970526     C*------------------------------------------------------------------------*
056600991026     C     LETvir        BEGSR
056700970526     C*
056800991025     C                   MOVEL     S20tip        KCEtip
056900991026     C                   MOVEL     S20ksc        KCEksc
057000000420     C     KEYvi2        CHAIN     tivir03L                           9998
057100970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
057200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
057300991025     C                   MOVEL     MSG(7)        S20MSG                         *MESSAGGIO
057400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
057500970526E   1C                   ENDIF
057600970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
057700970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
057800991025     C                   MOVEL     MSG(6)        S20MSG                         *MESSAGGIO
057900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
058000970526E   1C                   ENDIF
058100970526     C*
058200970526     C                   ENDSR
058300970526     C*------------------------------------------------------------------------*
058400970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
058500970526     C*------------------------------------------------------------------------*
058600970526     C     DECODI        BEGSR
058700001120     C*
058800001120     C*----------------------
058900001120     C* Controllo del codice
059000001120     C*----------------------
059100001120     C     ERRL00        IFEQ      '0'
059200001120     C     VIDksc        IFne      *blank
059300001120     C     VIDksc        andne     *zeros
059400001120     C                   exsr      cntksc
059500001120     C                   ELSE
059600991027     C                   MOVEL     '1'           ERRL00
059700991027     C                   SETON                                        65
059800991027     C                   MOVEL     MSG(12)       DSMSMS
059900001120     C                   ENDIF
060000001120     C                   ENDIF
060100101026     C*
060200101026     C* Verifico se x il servizio a video è richiesta l'indicazione della password FTP
060300101026     C     ERRL00        IFEQ      '0'
060400101026     C     VIDisv        lookup    isvftpeml                              80
060500101026 xxx C***                setoff                                       80
060600101026     C                   ENDIF
060700001120     C*----------------------
060800001120     C* Controllo della sequenza
060900001120     C*----------------------
061000001120     C     ERRL00        IFEQ      '0'
061100001120     C     VIDseq        IFne      0
061200001120     C                   ELSE
061300991027     C                   MOVEL     '1'           ERRL00
061400991027     C                   SETON                                        67
061500991027     C                   MOVEL     MSG(12)       DSMSMS
061600001120     C                   ENDIF
061700001120     C                   ENDIF
061800120109     C*----------------------
061900120109     C* Controllo tipo file
062000120109     C*----------------------
062100120109     C     ERRL00        IFEQ      '0'
062200120109     C     VIDtip        IFne      *blank
062300120109     C                   exsr      cnttip
062400120109     C                   ELSE
062500120109     C                   MOVEL     '1'           ERRL00
062600120109     C                   SETON                                        66
062700120109     C                   MOVEL     MSG(12)       DSMSMS
062800120109     C                   ENDIF
062900120109     C                   ENDIF
063000120109     C*----------------------
063100120109     C* Tipo servizio
063200120109     C*----------------------
063300120109     C     ERRL00        IFEQ      '0'
063400120109     C     VIDisv        IFne      *blank
063500120109     C                   exsr      cntisv
063600120109     C* Verifico la compatibilità del servizio rispetto al tipo-file
063700120109     C                   IF        VTFUD <> §ISVVERS
063800120109     C                   MOVEL     '1'           ERRL00
063900120109     C                   SETON                                        6670
064000120109     C                   MOVEL     MSG(24)       DSMSMS
064100120109     C                   ENDIF
064200120109     C                   ELSE
064300120109     C                   MOVEL     '1'           ERRL00
064400120109     C                   SETON                                        70
064500120109     C                   MOVEL     MSG(12)       DSMSMS
064600120109     C                   ENDIF
064700120109     C                   ENDIF
064800120109     C*--------------------
064900120109     C* Dati solo x upload no in download
065000120109     C*--------------------
065100120109     C* a seconde se siamo in Download o Upload sproteggo alcuni campi e ne proteggo altri
065200120109     C* e se non abbiamo errori pulisco i campi protetti (se abbiamo un errore potrebbe essere
065300120109     C*  che non siamo in D o U e i valori nei campi servono)
065400120109     C                   IF        §ISVVers   = 'D'
065500120109     c                   SETON                                        93
065600120109     C     ERRL00        IFEQ      '0'
065700120109     C                   MOVEL     *blanks       VIDpoi
065800120109     C                   MOVEL     *blanks       VIDEmL
065900120109     C                   MOVEL     *blanks       VIDfi2
066000120109     C                   MOVEL     *blanks       VIDfi4
066100120109     C                   MOVEL     *blanks       VIDfi4D
066200120109     C                   ENDIF
066300120109     C                   ENDIF
066400120109     C                   IF        §ISVVers   = 'U'
066500120109     C                   SETOFF                                       93
066600120109     C     ERRL00        IFEQ      '0'
066700120109     C                   MOVEL     *blanks       VIDTMbD
066800120109     C                   MOVEL     *blanks       VIDTip2
066900120109     C                   MOVEL     *blanks       VIDTip3
067000120109     C                   MOVEL     *blanks       VIDTip4
067100120119     C                   MOVEL     *blanks       VIDKsc2
067200120119     C                   MOVEL     *blanks       VIDKsc3
067300120119     C                   MOVEL     *blanks       VIDKsc4
067400120109     C                   ENDIF
067500120109     C                   ENDIF
067600100614     C*--------------------
067700100614     C* Tipo membro d default...  solo x anagrafiche d download (ovviamente no in visualizz.)
067800100614     C*--------------------
067900100614     C     ERRL00        IFEQ      '0'
068000100614     C                   IF        DEPOP1 = 'F06' OR
068100100614     C                             DEPOP1 = 'O03'
068200100614     C                   IF        §ISVPROC = 'F' AND
068300100614     C                             §ISVVERS = 'D'
068400100614     C                   IF        VIDtmbd = *blanks AND
068500100614     C                             vtfud   = 'D'
068600100614     C                   MOVEL     'M'           VIDtmbd
068700100614     C                   ENDIF
068800100614     C                   ENDIF
068900100614     C                   ENDIF
069000100614     C                   ENDIF
069100030603     C*----------------------
069200030603     C* Tipo membro
069300030603     C*----------------------
069400030603     C     ERRL00        IFEQ      '0'
069500030603     C     VIDtmbd       IFNE      *blank
069600030603     C                   IF        §ISVPROC = 'F' AND
069700030603     C                             §ISVVERS = 'D'
069800030603     C                   ELSE
069900030603     C                   MOVEL     '1'           ERRL00
070000030603     C                   SETON                                        61
070100030603     C                   MOVEL     MSG(22)       DSMSMS
070200030603     C                   ENDIF
070300030603     C                   ENDIF
070400030603     C                   ENDIF
070500120109     C*----------------------
070600120109     C* Campi duplicazione automatica su tipi file
070700120109     C*----------------------
070800120109     C     ERRL00        IFEQ      '0'
070900120109     C*
071000120109     C                   EVAL      MaxCiclo = %elem(Tips)
071100120109     C                   EVAL      Tips(1)  = VIDTip2
071200120109     C                   EVAL      Tips(2)  = VIDTip3
071300120109     C                   EVAL      Tips(3)  = VIDTip4
071400120119     C* se il tipo file è valorizzato e il cliente relativo no (o 0 o ' ')
071500120119     C* lo forzo col cliente principale
071600120119     C                   IF        (VIDKsc2 = *zeros or
071700120119     C                              VIDKsc2 = *blank   ) and
071800120119     C                             VIDTip2 <> *blank
071900120119     C                   EVAL      VIDKsc2 =  VIDKsc
072000120119     C                   ENDIF
072100120119     C                   IF        (VIDKsc3 = *zeros or
072200120119     C                              VIDKsc3 = *blank   ) and
072300120119     C                             VIDTip3 <> *blank
072400120119     C                   EVAL      VIDKsc3 =  VIDKsc
072500120119     C                   ENDIF
072600120119     C                   IF        (VIDKsc4 = *zeros or
072700120119     C                              VIDKsc4 = *blank   ) and
072800120119     C                             VIDTip4 <> *blank
072900120119     C                   EVAL      VIDKsc4 =  VIDKsc
073000120119     C                   ENDIF
073100120119     C                   EVAL      Kscs(1)  = VIDKsc2
073200120119     C                   EVAL      Kscs(2)  = VIDKsc3
073300120119     C                   EVAL      Kscs(3)  = VIDKsc4
073400120109     C     1             DO        MaxCiclo      i
073500120109     C     Tips(i)       IFNE      *blank
073600120119     C     Kscs(i)       ORNE      *blank
073700120119     C     Kscs(i)       ANDNE     *zeros
073800120109     C                   IF        §ISVVERS = 'D'
073900120119     C                   EXSR      CntTip234
074000120111     C* se mi è stato dato un errore, esco dal ciclo
074100120111     C                   IF        ERRL00 <> '0'
074200120111     C                   LEAVE
074300120111     C                   ENDIF
074400120109     C                   ELSE
074500120109     C                   SELECT
074600120109     C                   WHEN      i = 1
074700120119     C                   SETON                                        7549
074800120109     C                   WHEN      i = 2
074900120119     C                   SETON                                        7650
075000120109     C                   WHEN      i = 3
075100120119     C                   SETON                                        7751
075200120109     C                   ENDSL
075300120109     C                   MOVEL     '1'           ERRL00
075400120109     C                   MOVEL     MSG(30)       DSMSMS
075500120109     C                   ENDIF
075600120109     C                   ENDIF
075700120109     C                   ENDDO
075800120109     C*
075900120109     C                   ENDIF
076000001120     C*----------------------
076100001120     C* Controllo della descrizione
076200001120     C*----------------------
076300001120     C     ERRL00        IFEQ      '0'
076400001120     C     VIDdes        IFne      *blanks
076500001120     C                   ELSE
076600991122     C*                  MOVEL     '1'           ERRL00
076700991122     C*                  SETON                                        62
076800991122     C*                  MOVEL     MSG(12)       DSMSMS
076900991122     C                   MOVEL     VDDTIP        VIDDES
077000001120     C                   ENDIF
077100001120     C                   ENDIF
077200991026     C*--------------------
077300991026     C* DATA inizio
077400991026     C*--------------------
077500991026     C     *LIKE         DEFINE    VIDDti        DtiVID
077600001120     C     ERRL00        IFEQ      '0'
077700991026     C                   Z-ADD     VIDDti        wda8
077800991026     C                   Z-ADD     *ZEROS        da8w
077900991026     C                   EXSR      CHKDA8
078000991026     C                   Z-ADD     WDA8          viddti
078100991026     C                   Z-ADD     DA8W          dtivid
078200001120     C     ERRL00        IFEQ      '1'
078300991026     C                   SETON                                        63
078400991026     C                   MOVEL     MSG(14)       dsmsms
078500001120     C                   ENDIF
078600001120     C                   ENDIF
078700991026     C* INSERIMENTO -> RECORD INESISTENTE
078800001120     C     ERRL00        IFEQ      '0'
078900001120     C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
079000001120     C     DEPOP1        orEQ      'O03'                                        *INSERIMENTO
079100991026     C                   move      vidksc        KCEksc
079200000420     C                   move      vidtip        KCEtip
079300000420     C                   z-add     dtivid        KCEdti
079400000420     C     KEYvi2        CHAIN(N)  tivir03L                           99
079500001120     C     *IN99         IFEQ      *OFF
079600991026     C                   MOVEL     '1'           ERRL00
079700991026     C                   MOVEL     MSG(8)        DSMSMS
079800991026     C                   SETON                                        676566
079900991026     C                   SETON                                        63
080000001120     C                   ENDIF
080100001120     C                   ENDIF
080200001120     C                   ENDIF
080300001120     C*--------------------
080400991026     C* DATA SCADENZA
080500991026     C*--------------------
080600991026     C     *LIKE         DEFINE    VIDDtf        DtfVID
080700001120     C     ERRL00        IFEQ      '0'
080800991026     C                   Z-ADD     VIDDtf        WDA8
080900991026     C                   Z-ADD     *ZEROS        DA8W
081000991026     C                   EXSR      CHKDA8
081100991026     C                   Z-ADD     WDA8          VIDDtf
081200991026     C                   Z-ADD     DA8W          DtfVID
081300001120     C     ERRL00        IFEQ      '1'
081400991026     C                   SETON                                        68
081500991026     C                   MOVEL     MSG(14)       DSMSMS
081600001120     C                   ENDIF
081700001120     C                   ENDIF
081800991026     C* SCADENZA < DECORRENZA
081900001120     C     ERRL00        IFEQ      '0'
082000001120     C     DtfVID        IFLT      DtiVID
082100991026     C                   SETON                                        68
082200991026     C                   MOVEL     MSG(15)       DSMSMS
082300991026     C                   MOVEL     '1'           ERRL00
082400001120     C                   ENDIF
082500001120     C                   ENDIF
082600991026     C* SCADENZA >= DATA CORRENTE
082700141021     C     ERRL00        IFEQ      '0'
082800141021     C***  DEPOP1        IFEQ      'O02'
082900141021     C***  DEPOP1        OREQ      'O07'
083000141021     C***  DEPOP1        OREQ      'F06'
083100141021     C     DEPOP1        IFEQ      'O07'
083200141021     C     DEPOP1        OREQ      'F06'
083300001120     C     DtfVID        IFLT      DATCOR
083400001120     C                   SETON                                        68
083500991026     C                   MOVEL     MSG(16)       DSMSMS
083600991026     C                   MOVEL     '1'           ERRL00
083700001120     C                   ENDIF
083800001120     C                   ENDIF
083900001120     C                   ENDIF
084000991026     C*---
084100991026     C* DECORRENZA + SCADENZA
084200991026     C*---
084300991026     C* ACCAVALLAMENTO DATI
084400001120     C     ERRL00        IFEQ      '0'
084500991026     C                   MOVEL     VIDtip        KCEtip
084600991026     C                   MOVEL     vidksc        KCEksc
084700991026     C     KEyvir        SETLL     tivir03L
084800991026     C     KEyvir        READE(N)  tivir03L                               99
084900001120     C     *IN99         DOWEQ     *OFF
085000001120     C     DEPOP1        IFNE      'F06'                                        *NON INSERIMENTO
085100991026     C     DtiVID        ANDNE     virdti                                       *RECORD IN LINEA
085200991026     C     DEPOP1        OREQ      'F06'                                        *INSERIMENTO
085300001120     C     DtiVID        IFGE      virdti                                       *DECORRENZA
085400991026     C     DtiVID        ANDLE     virdtf
085500991026     C     DtfVID        ORGE      virdti                                       *SCADENZA
085600991026     C     DtfVID        ANDLE     virdtf
085700991026     C     virdti        ORGE      DtiVID                                       *DECORRENZA
085800991026     C     virdtf        ANDLE     DtfVID
085900991026     C                   SETON                                        666765
086000991026     C                   SETON                                        6368
086100991026     C                   MOVEL     MSG(17)       DSMSMS
086200991026     C                   MOVEL     '1'           ERRL00
086300991026     C                   LEAVE
086400001120     C                   ENDIF
086500001120     C                   ENDIF
086600991026     C     KEyvir        READE(N)  tivir03L                               99
086700001120     C                   ENDDO
086800001120     C                   ENDIF
086900001120     C*--------------------
087000001120     C* Controllo filiale
087100001120     C*--------------------
087200001120     C     ERRL00        IFEQ      '0'
087300001120     C     VIDpoi        IFne      *blank
087400001120     C     VIDpoi        andne     *zeros
087500001120     C                   exsr      cntpoi
087600001120     C                   ELSE
087700120109     C     §ISVVers      IFNE      'D'
087800991027     C                   MOVEL     '1'           ERRL00
087900000724     C                   MOVEL     MSG(12)       DSMSMS
088000991027     C                   SETON                                        69
088100991122     C                   ENDIF
088200001120     C                   ENDIF
088300001120     C                   ENDIF
088400001120     C*--------------------
088500011016     C* Programma traduttore
088600001120     C*--------------------
088700001120     C     ERRL00        IFEQ      '0'
088800001120     C     VIDpgt        IFNE      *blank
088900001120     C                   CALL(e)   'TIS721C'
089000050112     C                   PARM      *blanks       LIBRERIA         10
089100001120     C                   PARM      VIDpgt        OBJECT           10
089200001120     C                   PARM      '*PGM   '     TYPE              7
089300001120     C                   PARM      '0'           ESITO             1
089400001120     C                   IF        ESITO = '1'
089500001120     C                   MOVEL     '1'           ERRL00
089600001120     C                   SETON                                        54
089700001120     C                   MOVEL     MSG(19)       DSMSMS
089800001120     C                   ENDIF
089900001120     C                   ENDIF
090000001120     C                   ENDIF
090100001120     C*--------------------
090200001120     C* Programma post-traduzione
090300001120     C*--------------------
090400001120     C     ERRL00        IFEQ      '0'
090500001120     C     VIDpgi        IFNE      *blank
090600001120     C                   CALL(e)   'TIS721C'
090700050112     C                   PARM      *blanks       LIBRERIA         10
090800001120     C                   PARM      VIDpgi        OBJECT           10
090900001120     C                   PARM      '*PGM   '     TYPE              7
091000001120     C                   PARM      '0'           ESITO             1
091100001120     C                   IF        ESITO = '1'
091200001120     C                   MOVEL     '1'           ERRL00
091300001120     C                   SETON                                        55
091400001120     C                   MOVEL     MSG(19)       DSMSMS
091500001120     C                   ENDIF
091600001120     C                   ENDIF
091700001120     C                   ENDIF
091800001120     C*--------------------
091900001120     C* File da tradurre
092000001120     C*--------------------
092100001120     C     ERRL00        IFEQ      '0'
092200001120     C     VIDfid        IFNE      *blank
092300001120     C                   CALL(e)   'TIS721C'
092400050112     C                   PARM      *blanks       LIBRERIA         10
092500001120     C                   PARM      VIDfid        OBJECT           10
092600001120     C                   PARM      '*FILE  '     TYPE              7
092700001120     C                   PARM      '0'           ESITO             1
092800001120     C                   IF        ESITO = '1'
092900001120     C                   MOVEL     '1'           ERRL00
093000001120     C                   SETON                                        53
093100001120     C                   MOVEL     MSG(20)       DSMSMS
093200001120     C                   ENDIF
093300001120     C                   ENDIF
093400001120     C                   ENDIF
093500001120     C*--------------------
093600001120     C* File storico
093700001120     C*--------------------
093800001120     C     ERRL00        IFEQ      '0'
093900001120     C     VIDfis        IFNE      *blank
094000001120     C                   CALL(e)   'TIS721C'
094100050112     C                   PARM      *blanks       LIBRERIA         10
094200001120     C                   PARM      VIDfis        OBJECT           10
094300001120     C                   PARM      '*FILE  '     TYPE              7
094400001120     C                   PARM      '0'           ESITO             1
094500001120     C                   IF        ESITO = '1'
094600001120     C                   MOVEL     '1'           ERRL00
094700001120     C                   SETON                                        56
094800001120     C                   MOVEL     MSG(20)       DSMSMS
094900001120     C                   ENDIF
095000001120     C                   ENDIF
095100001120     C                   ENDIF
095200001120     C*--------------------
095300001120     C* File tradotto
095400001120     C*--------------------
095500001120     C     ERRL00        IFEQ      '0'
095600001120     C     VIDfit        IFNE      *blank
095700001120     C                   CALL(e)   'TIS721C'
095800050112     C                   PARM      VIDfi3        LIBRERIA         10
095900001120     C                   PARM      VIDfit        OBJECT           10
096000001120     C                   PARM      '*FILE  '     TYPE              7
096100001120     C                   PARM      '0'           ESITO             1
096200001120     C                   IF        ESITO = '1'
096300001120     C                   MOVEL     '1'           ERRL00
096400001120     C                   SETON                                        57
096500001120     C                   MOVEL     MSG(20)       DSMSMS
096600001120     C                   ENDIF
096700001120     C                   ENDIF
096800001120     C                   ENDIF
096900001120     C*--------------------
097000001120     C* Libreria file tradotto
097100001120     C*--------------------
097200001120     C     ERRL00        IFEQ      '0'
097300001120     C     VIDfi3        IFNE      *blank
097400001120     C                   CALL(e)   'TIS721C'
097500050112     C                   PARM      *blanks       LIBRERIA         10
097600001120     C                   PARM      VIDfi3        OBJECT           10
097700001120     C                   PARM      '*LIB   '     TYPE              7
097800001120     C                   PARM      '0'           ESITO             1
097900001120     C                   IF        ESITO = '1'
098000001120     C                   MOVEL     '1'           ERRL00
098100001120     C                   SETON                                        52
098200001120     C                   MOVEL     MSG(21)       DSMSMS
098300001120     C                   ENDIF
098400001120     C                   ENDIF
098500001120     C                   ENDIF
098600040816     C*--------------------
098700100311     C* Controllo corrispondenza Tipo File / Programma Traduttore / File Tradotto / Tipo membro Trad
098800040816     C*--------------------
098900040816     C     ERRL00        IFEQ      '0'
099000040816     C     VIDfit        IFne      *blank
099100040816     C     VIDpgt        ORne      *blank
099200040816     C                   EVAL      TBEcod = 'IFT'
099300040816     C     KEYtbe1_P     SETLL     tntbe01l
099400040816     C                   IF        %equal(tntbe01l)
099500040816     C* Inizializzo indici e schiere
099600040816     C                   Z-ADD     *zeros        j
099700040816     C                   CLEAR                   skTipF
099800040816     C                   CLEAR                   skPgt
099900040816     C                   CLEAR                   skFileT
100000100311     C                   CLEAR                   skMembroT
100100040816     C* Carico le schiere da tabella 'IFT'
100200040816     C     KEYtbe1_P     READE     tntbe01l
100300040816     C                   DOW       not %eof(tntbe01l)
100400040816     C                   IF        TBEatb = *blanks
100500040816     C                   EVAL      DIFT = TBEuni
100600040816     C                   ADD       1             j
100700100311     C                   EVAL      skTipF(j)    = TBEke1
100800100311     C                   EVAL      skPgt(j)     = §IFTPGT
100900100311     C                   EVAL      skFileT(j)   = §IFTFILE
101000100311     C                   EVAL      skMembroT(j) = §IFTTMBD
101100040816     C                   ENDIF
101200040816     C     KEYtbe1_P     READE     tntbe01l
101300040816     C                   ENDDO
101400040816     C* Imposto cosa testare
101500040816     C                   IF        VIDPGT <> *blanks
101600040816     C                   EVAL      wVIDPGT = VIDPGT
101700040816     C                   ELSE
101800040816     C                   EVAL      wVIDPGT = VIDPGTD
101900040816     C                   ENDIF
102000040816     C                   IF        VIDFIT <> *blanks
102100040816     C                   EVAL      wVIDFIT = VIDFIT
102200040816     C                   ELSE
102300040816     C                   EVAL      wVIDFIT = VIDFITD
102400040816     C                   ENDIF
102500040816     C* Testo se il traduttore indicato è tra quelli da controllare (in tabella 'IFT')
102600040823     C                   MOVEL     '0'           wFlgIFTok         1
102700100223     C                   SETOFF                                       16
102800040816     C     wVIDPGT       LOOKUP    skPgt                                  15
102900040816     C                   IF        %found
103000040816     C* Testo se vi è corrispondenza
103100040816     C                   EVAL      j = 1
103200040816     C                   DOW       j <= %elem(skTipF)
103300100223     C                   IF        skTipF(j)  = VIDTIP  AND
103400100223     C                             skPgt(j)   = wVIDPGT
103500100223     C                   SETON                                        16
103600100311     C*
103700100223     C                   IF        skFileT(j) = wVIDFIT
103800040816     C                   MOVEL     '1'           wFlgIFTok
103900100311     C*
104000100311     C                   IF        DEPOP1 = 'O02' OR
104100100311     C                             DEPOP1 = 'O03' OR
104200100311     C                             DEPOP1 = 'F06'
104300100312     C                   IF        VIDtmbd      =  *blanks AND
104400100312     C                             skMembroT(j) <> *blanks
104500100311     C                   EVAL      VIDtmbd = skMembroT(j)
104600100311     C                   SETON                                        61
104700100311     C                   ENDIF
104800100311     C                   ENDIF
104900100311     C*
105000040816     C                   LEAVE
105100040816     C                   ENDIF
105200100311     C*
105300100223     C                   ENDIF
105400040816     C                   ADD       1             j
105500040816     C                   ENDDO
105600040816     C* Se nn trovata esatta corrispondenza => errore
105700100223     C                   IF        *IN16 and wFlgIFTok = '0'
105800040816     C                   MOVEL     '1'           ERRL00
105900040816     C                   MOVEL     MSG(25)       DSMSMS
106000040816     C                   SETON                                        665457
106100040816     C                   ENDIF
106200040816     C                   ENDIF
106300040816     C                   ENDIF
106400040816     C                   ENDIF
106500040823     C                   ENDIF
106600051026     C*--------------------
106700051026     C* Controllo particolare x anagrafiche d download: se si sta cambiando il traduttore
106800051026     C* verifico prima che nn vi siano attualmente record d log a stato zero
106900051026     C*--------------------
107000051026     C     ERRL00        IFEQ      '0'
107100120109     C     §ISVVers      IFEQ      'D'
107200051026     C     VIDpgt        IFNE      virPGT
107300051026     C     VIDpgi        ORNE      virPGI
107400051026     C                   CALL      'TIS723R1'
107500051026     C                   PARM                    VIDKSC
107600051026     C                   PARM                    VIDTIP
107700120109     C                   PARM                    §ISVVers
107800051026     C                   PARM      *zeros        wRecZero          9 0
107900051026     C                   IF        wRecZero > *zeros
108000051026     C                   MOVEL     '1'           ERRL00
108100051026     C                   MOVEL     MSG(26)       DSMSMS
108200051026     C                   SETON                                        5455
108300051026     C                   ENDIF
108400051026     C                   ENDIF
108500051026     C                   ENDIF
108600051026     C                   ENDIF
108700060616     C*--------------------
108800060616     C* File Da tradurre / File Storico / Programma Post-Traduzione
108900060616     C*--------------------
109000060616     C     ERRL00        IFEQ      '0'
109100060616     C                   IF        (VIDfid='*' AND VIDfis='*' AND VIDpgi='*') OR
109200060616     C                             (VIDfid<>'*' AND VIDfis<>'*' AND VIDpgi<>'*')
109300060616     C                   ELSE
109400060616     C                   MOVEL     '1'           ERRL00
109500060616     C                   SETON                                        535556
109600060616     C                   MOVEL     MSG(27)       DSMSMS
109700060616     C                   ENDIF
109800060616     C                   ENDIF
109900081020     C*--------------------
110000081020     C* Mail x eventuale feed-back richiesto
110100081020     C*--------------------
110200081020     C     ERRL00        IFEQ      '0'
110300081020     C                   CLEAR                   DSEMAIL
110400081020     C                   EVAL      §EMLINDI = VIDEML
110500081020     C                   CALL      'XEMAIL'
110600081020     C                   PARM                    DSEMAIL
110700081020     C                   IF        §EMLERRO = '1'
110800081020     C                   MOVEL     '1'           ERRL00
110900081020     C                   SETON                                        64
111000081020     C                   MOVEL     §EMLMSGO      DSMSMS
111100081020     C                   ENDIF
111200081020     C                   ENDIF
111300001120     C*
111400001120     C                   ENDSR
111500000626     C*------------------------------------------------------------------------*
111600000626     C* Cntisv - CONTROLLA codice tipo legame
111700000626     C*------------------------------------------------------------------------*
111800001120     C     cntisv        begsr
111900001120     C                   clear                   TIBS02DS
112000001120     C*
112100101027     C     '?'           SCAN      Vidisv                                 28
112200001120     C*  Ricerca
112300001120     C                   eval      t02tla = 'L'
112400101027     C                   IF        *IN28 = *ON
112500001120     C                   movel     'R'           t02mod
112600001120     C                   else
112700001120     C                   movel     'C'           t02mod
112800001120     C                   movel(p)  vidisv        t02ke1
112900001120     C                   end
113000000626     C                   movel     'ISV'         t02cod
113100001120     C                   movel     knsif         t02sif
113200001120     C                   CALL      'TIBS02R'
113300001120     C                   parm                    KPJBA
113400001120     C                   parm                    TIBS02DS
113500001120     C
113600001120     C*
113700001120     C                   IF        T02err = *blanks
113800000626     C                   movel     T02ke1        Vidisv
113900000626     C                   movel     T02uni        vddisv
114000030603     C                   movel(P)  T02uni        DISV
114100001120     C                   else
114200001120     C                   movel     '1'           errl00
114300001120     C                   seton                                        70
114400001120     C                   MOVEL     MSG(18)       DSMSMS
114500001120     C                   ENDIF
114600001120     C
114700001120     C                   endsr
114800991027     C*------------------------------------------------------------------------*
114900991027     C* cnttip - Controlla esistenza tipo file su tivt00f
115000991027     C*------------------------------------------------------------------------*
115100001120     C     cnttip        begsr
115200001120     C                   clear                   TIBS02DS
115300000626     C*
115400101027     C     '?'           SCAN      vidtip                                 28
115500001120     C*  Ricerca
115600001120     C                   eval      t02tla = 'L'
115700101027     C                   IF        *IN28 = *ON
115800001120     C                   movel     'R'           t02mod
115900001120     C                   else
116000001120     C                   movel     'C'           t02mod
116100001120     C                   movel(p)  vidtip        t02ke1
116200001120     C                   end
116300991108     C                   movel     'ITF'         t02cod
116400001120     C                   movel     knsif         t02sif
116500991108     C                   CALL      'TIBS02R'
116600001120     C                   parm                    KPJBA
116700001120     C                   parm                    TIBS02DS
116800001120     C
116900001120     C*
117000001120     C                   IF        T02err = *blanks
117100991108     C                   movel     T02ke1        vidtip
117200020218     C                   movel     T02uni        DITF
117300020218     C                   movel     §ITFDESC      vddtip
117400021003     C* Sostituisco con il dato proveniente dalla tabella solo se nn già indicato
117500021003     C* qualcosa nelle regole specifiche x il cliente/tipo file.
117600021014     C                   if        vidfit = *blanks
117700021014     C                   movel     §ITFLIBD      vidfi2
117800021014     C                   endif
117900001120     C                   else
118000001120     C                   movel     '1'           errl00
118100991108     C                   movel     *blank        vddtip
118200111215     C* pulisco anche il cod. tabella associata
118300111215     C                   movel     *blank        vidtab
118400001120     C                   seton                                        66
118500991108     C                   MOVEL     msg(11)       DSMSMS
118600991108     C                   ENDIF
118700001120     C                   if        errl00 = '0'
118800120109     C     vidtip        chain     tivtf01l                           97
118900001120     C                   if        not *in97
119000001120     C                   movel     vtfpgt        vidpgtd
119100001120     C                   movel     vtfpgi        vidpgid
119200001120     C                   movel     vtffid        vidfidd
119300001120     C                   movel     vtffis        vidfisd
119400001120     C                   movel     vtffit        vidfitd
119500111214     C                   movel     vtffi1        vidtab
119600101013     C                   movel     vtfppi        DVTFPPI
119700101026     C                   if        DEPOP1 = 'F06' OR
119800101026     C                             DEPOP1 = 'O03'
119900101026     C                   select
120000101026     C                   when      vtfUD = 'U'
120100101026     C                   movel     D§VTFPPIMF    vidppiMC
120200101026     C                   when      vtfUD = 'D'
120300101026     C                   movel     D§VTFPPIMF    vidppiMF
120400101026     C                   endsl
120500101026     C                   movel     D§VTFPPIMS    vidppiMS
120600101026     C                   movel     D§VTFPPIMZ    vidppiMZ
120700101026     C                   endif
120800100308     C                   if        vidfi4d <> *blanks and vidfi4d <> '*'
120900001120     C                   eval      vidfi4d = %subst(vidfitd:1:5) + '00F'
121000100308     C                   endif
121100001120     C                   else
121200001120     C                   movel     *blank        vidpgtd
121300001120     C                   movel     *blank        vidpgid
121400001120     C                   movel     *blank        vidfidd
121500001120     C                   movel     *blank        vidfisd
121600001120     C                   movel     *blank        vidfitd
121700001120     C                   movel     *blank        vidfi4d
121800001120     C                   endif
121900001120     C                   endif
122000001120     C*
122100001120     C                   endsr
122200991027     C*------------------------------------------------------------------------*
122300120109     C* CntTip234 - Controlla esistenza altri tipi file su tivt00f
122400991027     C*------------------------------------------------------------------------*
122500120109     C     CntTip234     BEGSR
122600120119     C*
122700120119     C* se ho indicato un cliente devo indicare il tipo file
122800120119     C                   IF        Kscs(i) <>*blank and
122900120119     C                             Kscs(i) <>*zeros and
123000120119     C                             Tips(i) = *blank
123100120119     C                   SELECT
123200120119     C                   WHEN      i = 1
123300120119     C                   SETON                                        7549
123400120119     C                   WHEN      i = 2
123500120119     C                   SETON                                        7650
123600120119     C                   WHEN      i = 3
123700120119     C                   SETON                                        7751
123800120119     C                   ENDSL
123900120119     C                   MOVEL     '1'           ERRL00
124000120119     C                   MOVEL     MSG(34)       DSMSMS
124100120119     C                   LEAVESR
124200120119     C                   ENDIF
124300010913     C*
124400120119     C* il tipo file / cliente indicati non devono essere quelli in gestione
124500120119     C                   IF        Tips(i) = VIDTip and
124600120119     C                             Kscs(i) = VIDKsc
124700120111     C                   SELECT
124800120111     C                   WHEN      i = 1
124900120119     C                   SETON                                        7549
125000120111     C                   WHEN      i = 2
125100120119     C                   SETON                                        7650
125200120111     C                   WHEN      i = 3
125300120119     C                   SETON                                        7751
125400120111     C                   ENDSL
125500120111     C                   MOVEL     '1'           ERRL00
125600120111     C                   MOVEL     MSG(33)       DSMSMS
125700120111     C                   LEAVESR
125800120111     C                   ENDIF
125900120111     C*
126000120111     C* il tipo file indicato deve esistere ed essere abilitato su questa anagrafica per il cliente
126100120119     C* indicato
126200120111     C* (per abilitato basta che la data fine validità sia maggiore di oggi; se anche la data inizio
126300120111     C*  è maggiore di oggi non importa, sarà valido nel futuro)
126400120119     C                   EVAL      x_VIRKsc = Kscs(i)
126500120111     C                   EVAL      x_VIRTip = Tips(i)
126600120111     C                   EVAL      x_VIRDtF = %dec(%date() : *ISO)
126700120111     C     K03VIR06      SETLL     TIVIR06L
126800120111     C     K02VIR06      READE     TIVIR06L
126900120111     C                   if        %eof
127000120109     C                   SELECT
127100120109     C                   WHEN      i = 1
127200120109     C                   SETON                                        75
127300120109     C                   WHEN      i = 2
127400120109     C                   SETON                                        76
127500120109     C                   WHEN      i = 3
127600120109     C                   SETON                                        77
127700120109     C                   ENDSL
127800120109     C                   MOVEL     '1'           ERRL00
127900120109     C                   MOVEL     MSG(31)       DSMSMS
128000120119     C* se il tipo file / cliente per duplica esiste,
128100120119     C* questo non deve contenere nelle sue coppie per duplica
128200120119     C* il tipo file / cliente in gestione
128300120111     C                   ELSE
128400120119     C*
128500120120     C                   if        TIS723R2U_Ctrl_Chain_Dup(
128600120120     C                              VIDTip       :
128700120120     C                              VIDKsc       :
128800120120     C                              Tips(i)      :
128900120120     C                              Kscs(i)      ) = '1'
129000120120     C                   SELECT
129100120120     C                   WHEN      i = 1
129200120120     C                   SETON                                        7549
129300120120     C                   WHEN      i = 2
129400120120     C                   SETON                                        7650
129500120120     C                   WHEN      i = 3
129600120120     C                   SETON                                        7751
129700120120     C                   ENDSL
129800120120     C                   MOVEL     '1'           ERRL00
129900120120     C                   MOVEL     MSG(32)       DSMSMS
130000120120     C                   ENDIF
130100120120     C***
130200120120     C***                MOVEL     x_VIRPTH      DVIR01
130300120120     C***                IF        §VIR01Tip2 = VIDTip and
130400120120     C***                          §VIR01Ksc2 = VIDKsc     or
130500120120     C***                          §VIR01Tip3 = VIDTip and
130600120120     C***                          §VIR01Ksc3 = VIDKsc     or
130700120120     C***                          §VIR01Tip4 = VIDTip and
130800120120     C***                          §VIR01Ksc4 = VIDKsc
130900120120     C***                SELECT
131000120120     C***                WHEN      i = 1
131100120120     C***                SETON                                        7549
131200120120     C***                WHEN      i = 2
131300120120     C***                SETON                                        7650
131400120120     C***                WHEN      i = 3
131500120120     C***                SETON                                        7751
131600120120     C***                ENDSL
131700120120     C***                MOVEL     '1'           ERRL00
131800120120     C***                MOVEL     MSG(32)       DSMSMS
131900120120     C***                ENDIF
132000120120     C***
132100120111     C                   ENDIF
132200120109     C*
132300120109     C                   ENDSR
132400120109     C*------------------------------------------------------------------------*
132500120109     C* cntksc - Controlla esistenza codice
132600120109     C*------------------------------------------------------------------------*
132700120109     C     cntksc        begsr
132800120109     C*
132900010913     C     vidksc        chain     tianv01l
133000010913     C                   if        %found
133100010913     C                   movel     ANVDES        vddksc
133200010913     C                   else
133300001120     C                   move      vidksc        wcli              7 0
133400991210     C                   CLEAR                   BS69DS
133500991210     C                   CLEAR                   ACODS
133600991210     C                   MOVEL     KNSIF         I69SIF
133700991210     C                   Z-ADD     WCLI          I69KAC
133800991210     C                   CALL      'TIBS69R'
133900991210     C                   PARM                    BS69DS
134000991210     C                   PARM                    ACODS
134100991210     C                   PARM                    INDDS
134200991210     C                   PARM                    CLPDS
134300991210     C                   PARM                    CLSDS
134400991210     C     O69ERR        IFNE      '1'
134500991210     C                   MOVEL     ACORAG        vddksc
134600991210     C                   ELSE
134700991210     C                   MOVEL     *ALL'*'       vddksc
134800010913     C                   MOVE      '1'           errl00
134900010913     C                   MOVEL     msg(11)       dsmsms
135000010913     C                   ENDIF
135100000727     C                   ENDIF
135200010913     C*
135300010913     C                   ENDSR
135400991027     C*------------------------------------------------------------------------*
135500040901     C* cntpoi - Controlla codice filiale
135600991027     C*------------------------------------------------------------------------*
135700001120     C     cntpoi        begsr
135800001120     C*
135900010529     C                   if        vidpoi <> '046' and
136000020809     C                             vidpoi <> '999' and
136100020902     C                             vidpoi <> '500' and
136200020902     C                             vidpoi <> '789'
136300001120     C*
136400001120     C                   move      *blank        vddpoi
136500001120     C     '?'           scan      vidpoi                                 97
136600001120     C                   if        *in97
136700001120     C                   movel     *blank        cod1
136800001120     C                   movel     *blank        des1
136900001120     C                   movel     *blank        pra1
137000001120     C                   call      'TNSD24R'
137100001120     C                   parm                    cod1              3
137200001120     C                   parm                    tip1              1
137300001120     C                   parm                    des1             25
137400001120     C                   parm                    pra1              3
137500001120     C                   movel     cod1          vidpoi
137600001120     C                   endif
137700001120     C                   move      vidpoi        cod1n             3 0
137800001120     C     cod1n         chain     azorg01l                           98
137900001120     C                   if        not *in98 and orgfag <>'V'
138000001120     C                   movel     orgdes        vddpoi
138100001120     C                   else
138200001120     C                   seton                                        69
138300001120     C                   move      '1'           errl00
138400001120     C                   movel     msg(13)       dsmsms
138500001120     C                   endif
138600001120     C*
138700001120     C                   else
138800001120     C                   movel     'SEDE BOLOGNA'vddpoi
138900001120     C                   endif
139000001120     C*
139100001120     C                   endsr
139200991210     C*------------------------------------------------------------------------*
139300991210     C* FINSR - OPERAZIONI FINALI
139400991210     C*------------------------------------------------------------------------*
139500991210     C     FINSR         BEGSR
139600991210     C*
139700991210     C* CHIUDE I FILE DEI PGM CHIAMATI
139800991210     C                   CLEAR                   bs69ds
139900991210     C                   CLEAR                   acods
140000991210     C                   CLEAR                   indds
140100991210     C                   CLEAR                   clpds
140200991210     C                   CLEAR                   clsds
140300991210     C                   MOVEL     'C'           i69tla
140400991210     C                   MOVEL     knsif         i69sif
140500991210     C                   CALL      'TIBS69R'
140600991210     C                   PARM                    bs69ds
140700991210     C                   PARM                    acods
140800991210     C                   PARM                    indds
140900991210     C                   PARM                    clpds
141000991210     C                   PARM                    clsds
141100991210     C*
141200991210     C                   ENDSR
141300991026     C*------------------------------------------------------------------------*
141400991026     C* CHKDA8 - CONTROLLA LA DATA (8)
141500991026     C*------------------------------------------------------------------------*
141600991026     C     CHKDA8        BEGSR
141700991026     C*
141800991026     C     *LIKE         DEFINE    VIDDti        WDA8
141900991026     C     *LIKE         DEFINE    VIDDti        DA8W
142000991026     C*
142100991026     C                   Z-ADD     WDA8          G08DAT
142200991026     C                   Z-ADD     *ZEROS        G08INV
142300991026     C                   MOVEL     '0'           G08ERR
142400991026     C                   CALL      'XSRDA8'
142500991026     C                   PARM                    WLBDA8
142600991026     C                   Z-ADD     G08DAT        WDA8
142700991026     C                   Z-ADD     G08INV        DA8W
142800991026IF  1C     G08ERR        IFEQ      '1'
142900991026     C                   MOVEL     '1'           ERRL00
143000991026E   1C                   ENDIF
143100991026     C*
143200991026     C                   ENDSR
143300970526     C*------------------------------------------------------------------------*
143400970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
143500970526     C*------------------------------------------------------------------------*
143600970526     C     IREVID        BEGSR
143700080604     C*
143800080604     C* REPERISCO INFO DA ESTENSIONE ANAGRAFICA CLIENTI/TIPI FILE => TRC 01
143900080604     C                   IF        §vire01INV = 'S'
144000080604     C                   EVAL      VIDEML = §vire01EML
144100080604     C                   ENDIF
144200970526     C* CAMPI
144300991026     C                   MOVEL     virtip        VIDtip
144400020218     C                   MOVE      virksc        vidksc
144500020218     C                   Z-ADD     virseq        vidseq
144600060308     C                   IF        DEPOP1 = 'O03'
144700060308     C                   MOVEL     *blanks       vidpoi
144800060308     C                   ELSE
144900060308     C                   MOVE      virpoi        vidpoi
145000060308     C                   ENDIF
145100991026     C                   MOVEL     virdes        VIDdes
145200991026     C                   MOVEL     virpgt        VIDpgt
145300991026     C                   MOVEL     virppt        VIDppt
145400991026     C                   MOVEL     virpgi        VIDpgi
145500991026     C                   MOVEL     virppi        VIDppi
145600991026     C                   MOVEL     virfid        VIDfid
145700991026     C                   MOVEL     virfis        VIDfis
145800991026     C                   MOVEL     virfit        VIDfit
145900000710     C                   MOVEL     virfi3        VIDfi3
146000000710     C                   MOVEL     virfi4        VIDfi4
146100010123     C                   MOVEL     virfi2        VIDfi2
146200030526     C                   MOVEL     virpth        DVIR01
146300030526     C                   MOVEL     §VIR01TM      VIDtmbd
146400120109     C                   MOVEL     §VIR01Tip2    VIDTip2
146500120109     C                   MOVEL     §VIR01Tip3    VIDTip3
146600120109     C                   MOVEL     §VIR01Tip4    VIDTip4
146700120119     C                   MOVEL     §VIR01Ksc2    VIDKsc2
146800120119     C                   MOVEL     §VIR01Ksc3    VIDKsc3
146900120119     C                   MOVEL     §VIR01Ksc4    VIDKsc4
147000000710  ***C*                  MOVEL     virctl        VIDctl
147100991026     C                   Z-ADD     virdti        G08INV
147200991026     C                   Z-ADD     *ZEROS        G08DAT
147300991026     C                   MOVEL     '3'           G08ERR
147400991026     C                   CALL      'XSRDA8'
147500991026     C                   PARM                    WLBDA8
147600991026     C                   Z-ADD     G08DAT        viddti
147700970527     C*
147800991026     C                   Z-ADD     virdtf        G08INV
147900991026     C                   Z-ADD     *ZEROS        G08DAT
148000991026     C                   MOVEL     '3'           G08ERR
148100991026     C                   CALL      'XSRDA8'
148200991026     C                   PARM                    WLBDA8
148300991026     C                   Z-ADD     G08DAT        viddtf
148400020218     C                   MOVEL     virfi1        vidisv
148500051122     C*
148600051122     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
148700051122     C                   IF        DEPOP1 = 'F06' OR
148800051122     C                             DEPOP1 = 'O03' OR
148900051122     C                             DEPOP1 = 'O07'
149000051122     C                   EVAL      VIDDTI = DATCORINV
149100060726     C                   EVAL      VIDDTF = 31122039
149200051122     C                   ENDIF
149300020218     C*
149400970526     C                   ENDSR
149500970526     C*------------------------------------------------------------------------*
149600970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
149700970526     C*------------------------------------------------------------------------*
149800970526     C     AZZVID        BEGSR
149900970526     C* CAMPI
150000991026     C                   MOVEL     *blank        VIDksc
150100000710  ***C*                  MOVEL     *blank        VIDpt1
150200000710  ***C*                  MOVEL     *blank        VIDpt2
150300010123     C                   z-add     0             vidseq
150400010123     C                   move      *zeros        vidpoi
150500060726     C                   EVAL      VIDDTI = DATCORINV
150600060726     C                   EVAL      VIDDTF = 31122039
150700991025     C                   MOVEL     *blank        VIDtip
150800991025     C                   MOVEL     *blank        VIDdes
150900991025     C                   MOVEL     *blank        VIDpgt
151000991025     C                   MOVEL     *blank        VIDppt
151100991025     C                   MOVEL     *blank        VIDpgi
151200991025     C                   MOVEL     *blank        VIDppi
151300991025     C                   MOVEL     *blank        VIDfid
151400991025     C                   MOVEL     *blank        VIDfis
151500991025     C                   MOVEL     *blank        VIDfit
151600000710     C                   MOVEL     *blank        VIDfi3
151700000710     C                   MOVEL     *blank        VIDfi4
151800010123     C                   MOVEL     *blank        VIDfi2
151900000710  ***C*                  MOVEL     *blank        VIDctl
152000080603     C                   MOVEL     *blank        VIDeml
152100970526     C*
152200970526     C                   ENDSR
152300970526     C*------------------------------------------------------------------------*
152400991026     C* IMPvir - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
152500970526     C*------------------------------------------------------------------------*
152600991026     C     IMPvir        BEGSR
152700970526     C*
152800991027     C                   move      VIDpoi        virpoi
152900991026     C                   movel     VIDdes        virdes
153000991026     C                   movel     VIDpgt        virpgt
153100991026     C                   movel     VIDppt        virppt
153200991026     C                   movel     VIDpgi        virpgi
153300991026     C                   movel     VIDppi        virppi
153400991026     C                   movel     VIDfid        virfid
153500991026     C                   movel     VIDfis        virfis
153600991026     C                   movel     VIDfit        virfit
153700000710     C                   movel     VIDfi3        virfi3
153800000710     C                   movel     VIDfi4        virfi4
153900010123     C                   movel     VIDfi2        virfi2
154000030526     C                   MOVEL     VIDtmbd       §VIR01TM
154100120109     C                   EVAL      §VIR01Tip2 =  VIDTip2
154200120109     C                   EVAL      §VIR01Tip3 =  VIDTip3
154300120109     C                   EVAL      §VIR01Tip4 =  VIDTip4
154400120119     C                   IF        VIDKsc2 = *zeros
154500120119     C                   EVAL      VIDKsc2 = *blank
154600120119     C                   ENDIF
154700120119     C                   IF        VIDKsc3 = *zeros
154800120119     C                   EVAL      VIDKsc3 = *blank
154900120119     C                   ENDIF
155000120119     C                   IF        VIDKsc4 = *zeros
155100120119     C                   EVAL      VIDKsc4 = *blank
155200120119     C                   ENDIF
155300120119     C                   EVAL      §VIR01Ksc2 =  VIDKsc2
155400120119     C                   EVAL      §VIR01Ksc3 =  VIDKsc3
155500120119     C                   EVAL      §VIR01Ksc4 =  VIDKsc4
155600030526     C                   MOVEL     DVIR01        virpth
155700000710  ***C*                  movel     VIDctl        virctl
155800991026     C                   Z-ADD     viddtf        G08dat
155900991026     C                   Z-ADD     *ZEROS        G08inv
156000991026     C                   MOVEL     '0'           G08ERR
156100991026     C                   CALL      'XSRDA8'
156200991026     C                   PARM                    WLBDA8
156300991026     C                   Z-ADD     G08inv        virdtf
156400000628     C                   movel     VIDisv        virfi1
156500970526     C*
156600970526     C                   ENDSR
156700970526     C*------------------------------------------------------------------------*
156800970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
156900970526     C*------------------------------------------------------------------------*
157000970526     C     IMPFUN        BEGSR
157100101013     C*
157200101026     C                   SETOFF                                       44
157300101026     C                   SETOFF                                       30
157400101026     C                   SETOFF                                       31
157500101026     C                   SETOFF                                       32
157600101026     C                   SETOFF                                       33
157700970526     C*
157800970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
157900970526IF  1C     DEPOP1        IFEQ      'O05'
158000970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
158100970526     C                   MOVEL     'N'           FUDATT
158200970526     C                   MOVEL     FUDRIG        FUD(6)
158300970526E   1C                   ENDIF
158400141111     C*
158500141111     C* TASTO FUNZIONE F02 ATTIVO SOLO PER OPZIONI "DISPOSITIVE"
158600141111     C                   IF        DEPOP1 = 'O02' OR
158700141111     C                             DEPOP1 = 'O03' OR
158800141111     C                             DEPOP1 = 'F06'
158900141111     C                   MOVEL     FUD(2)        FUDRIG                         *PARAMETRI EXT.
159000141111     C                   MOVEL     'S'           FUDATT
159100141111     C                   MOVEL     FUDRIG        FUD(2)
159200141111     C                   ENDIF
159300970526     C*
159400970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
159500970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
159600970526     C                   EXSR      FUNGES
159700970526     C*
159800970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
159900970526     C*---
160000970526     C* 90 -> PROTEZIONE CHIAVE
160100970526     C* 91 -> PROTEZIONE TOTALE
160200970526     C*---
160300970528IF  1C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
160400970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
160500970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
160600970526     C                   SETOFF                                       91
160700970526E   2C                   ENDIF
160800101013IF  2C     DEPOP1        IFEQ      'O03'                                        *INSERIMENTO
160900101013E   2C                   ENDIF
161000970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
161100991014IF  2C     DEPOP1        oreq      'O01'                                        *MODIFICA
161200970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
161300970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
161400970526E   2C                   ENDIF
161500970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
161600970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
161700970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
161800970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
161900970526     C                   SETON                                        91
162000970526E   2C                   ENDIF
162100970526X   1C                   ELSE                                                   *I N T E R R O G A
162200970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
162300970526     C                   SETON                                        91
162400970526E   1C                   ENDIF
162500970526     C*
162600970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
162700970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
162800991025     C                   MOVEL     S20tip        KCEtip
162900991026     C                   MOVEL     S20ksc        KCEksc
163000000420     C     KEYvi2        CHAIN     tivir03L                           9998
163100080603IF  2C     *IN99         IFEQ      *ON
163200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
163300970526     C                   MOVEL     MSG(6)        DSMSMS
163400970526X   2C                   ELSE
163500080603     C*
163600080603     C* Aggancio anche il record d estensione regole clienti/tipi-file => TRC 01
163700080603     C                   EVAL      KCEdti  = virDTI
163800080603     C                   EVAL      vireTRC = '01'
163900080603     C     KEYvire1_C    CHAIN     tivire1l                           9998
164000080603     C                   IF        %found(tivire1l)
164100080603     C                   EVAL      DVIRE01 = vireFLO
164200080603     C                   ENDIF
164300080603     C*
164400080603     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
164500980128IF  3C     DEPOP1        IFEQ      'O05'
164600991014IF  3C     DEPOP1        oreq      'O02'
164700101026     C*
164800101026     C* Se trattasi d tipo servizio "idoneo" reperisco anche i dati utente lato "BARTVAS"
164900101026     C     virFI1        lookup    isvftpeml                              80
165000101026     C                   IF        *IN80
165100111215     C                             and AS400_ok = *on
165200101026      /FREE
165300101026
165400130215       // (questa attività costa risorse, per cui si permette solo in gestione oppure anche in
165500130215       //  visualizzazione se l'utente è di sede;
165600130215       //  testo solo se l'utente è di sede, perché quelli di filiale non possono andare in gestione
165700130215       //  quindi non potrebbero comunque)
165800130215       if DUtLPO = 'S';
165900101026         if UBBVSQL01_RtvCliTipFile(
166000101026                         pInConn
166100101026                       : virKSC
166200101026                       : virTIP
166300101026                       : pOutNumRec
166400101026                       : pOutCliTipTras
166500101026                       : pOutCliMskChk
166600101026                       : pOutCliMskFil
166700101026                       : pOutCliMskSem
166800101026                       : pOutCliPerc
166900101026                       : pOutCliEml1
167000101026                       : pOutCliEml2
167100101026                       : pOutCliEml3
167200101026                       : pOutCliEml4
167300101026                       : pOutCliEml5
167400101026                       : pOutCliSogg
167500101026                       : pOutCliTesto
167600101026                       : pOutCliFtpAddr
167700101026                       : pOutCliFtpDir
167800101026                       : pOutCliFtpUsr
167900101026                       : pOutCliFtpPwd
168000101026                       : pOutCliFtpFAS4
168100101026                       : pOutCliPercOld
168200101026                       : pOutCliTentat
168300101026                       : pOutCliRitardo
168400101026                       : pOutCliLibAS4
168500101026                       : pOutCliFilAS4
168600101026                       : pOutCliMbrAS4
168700101026                       : pOutCliProc
168800101026                       : pOutCliSched
168900101026                       : pOutCliAccorp
169000101026                       : pOutCliMskZip
169100101026                       : pOutCliDataStr
169200101026                       : pOutCliDataEnd
169300101026                       : pOutCliCodComp
169400101026                       : pOutCliFtpCmd1
169500101026                       : pOutCliEmlErr
169600101026                       : pOutCliHtUrlTo
169700101026                       : pOutCliHtUsrTo
169800101026                       : pOutCliHtPwdTo
169900101026                       : pOutCliHtConTy
170000101026                       : pOutCliHtSndEx
170100101026                       : pOutCliHtSndPr
170200101026                       : pOutCliHtUsPxy
170300101026                       : pOutCliUsrIns
170400101026                       : pOutCliUsrUpd
170500101026                       : pOutCliDatUpd
170600101026                       : pOutCliNote
170700101026                       : pOutCliFtpPrt
170800101026                       : pOutCliFtpTy) = 0;
170900101026              if pOutNumRec = 1;
171000101026                 VIDPPIMC = pOutCliMskChk;
171100101026                 if %len(%trim(pOutCliMskChk)) > %size(VIDPPIMC);
171200101026                    *in30 = *on;
171300101026                 endif;
171400101026                 VIDPPIMF = pOutCliMskFil;
171500101026                 if %len(%trim(pOutCliMskFil)) > %size(VIDPPIMF);
171600101026                    *in31 = *on;
171700101026                 endif;
171800101026                 VIDPPIMS = pOutCliMskSem;
171900101026                 if %len(%trim(pOutCliMskSem)) > %size(VIDPPIMS);
172000101026                    *in32 = *on;
172100101026                 endif;
172200101026                 VIDPPIMZ = pOutCliMskZip;
172300101026                 if %len(%trim(pOutCliMskZip)) > %size(VIDPPIMZ);
172400101026                    *in33 = *on;
172500101026                 endif;
172600101026              endif;
172700101026         endif;
172800130214       endif;
172900101026
173000101026      /END-FREE
173100101026     C
173200101026     C                   ELSE
173300101026     C                   SETON                                        44
173400101026     C                   ENDIF
173500101026     C*
173600980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
173700980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
173800980128E   3C                   ENDIF
173900970526E   2C                   ENDIF
174000970526X   1C                   ELSE                                                   *INSERIMENTO
174100970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
174200970526E   1C                   ENDIF
174300970526     C*
174400970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
174500970526IF  1C     DEPOP1        IFEQ      'O02'
174600970526     C                   MOVEL     DEC(1)        VIDFUN
174700970526E   1C                   ENDIF
174800991014IF  1C     DEPOP1        IFEQ      'O03'
174900991014     C                   MOVEL     DEC(6)        VIDFUN
175000991014E   1C                   ENDIF
175100970526IF  1C     DEPOP1        IFEQ      'O04'
175200970526     C                   MOVEL     DEC(2)        VIDFUN
175300970526E   1C                   ENDIF
175400970526IF  1C     DEPOP1        IFEQ      'O05'
175500970526     C                   MOVEL     DEC(3)        VIDFUN
175600970526E   1C                   ENDIF
175700970526IF  1C     DEPOP1        IFEQ      'O07'
175800970526     C                   MOVEL     DEC(4)        VIDFUN
175900970526E   1C                   ENDIF
176000970526IF  1C     DEPOP1        IFEQ      'F06'
176100970526     C                   MOVEL     DEC(5)        VIDFUN
176200970526E   1C                   ENDIF
176300970526     C*
176400970526     C                   ENDSR
176500101026
176600101026
176700101026
176800101026     C*--------------------------------------------------------------------------------------------*
176900101026     C* CARTAB - carica le tabelle occorrenti
177000101026     C*--------------------------------------------------------------------------------------------*
177100101026     C     CARTAB        BEGSR
177200101026     C*
177300101026     C* Tipi servizi soggetti a controllo unificazioni
177400101026     C                   z-add     *zeros        ww                3 0
177500101026     C                   eval      tbecod = 'ISV'
177600101026     C     tbecod        setll     tntbe01l
177700101026     C     tbecod        reade     tntbe01l
177800101026     C                   dow       not %eof(tntbe01l)
177900101026     C                   movel     tbeuni        disv
178000101026     C                   if        §isvproc = 'F'
178100101026     C                   add       1             ww
178200101026     C                   eval      isvftpeml(ww) = tbeke1
178300101026     C                   endif
178400101026     C     tbecod        reade     tntbe01l
178500101026     C                   enddo
178600101026     C*
178700101026     C                   ENDSR
178800101026
178900101026     c*------------------------------------------------------------------------*
179000101026     c* opnCONN - Apertura connessione
179100101026     c*------------------------------------------------------------------------*
179200101026     c     OPNCONN       BEGSR
179300101026      /FREE
179400101026
179500101026        if not *in40;
179600101026           if UBBVSQL01_Connect('BARTVAS' : pOutConn) = 0;
179700101026              pInConn = pOutConn;
179800101026           else;
179900101026              *in40 = *on;
180000101026              DSMSMS = MSG(28);
180100101026           endif;
180200101026        endif;
180300101026
180400101026      /END-FREE
180500101026      *
180600101026     c                   ENDSR
180700101026     c*------------------------------------------------------------------------*
180800101026
180900101026
181000101026
181100101026     c*------------------------------------------------------------------------*
181200101026     c* clsCONN - Chiusura connessione
181300101026     c*------------------------------------------------------------------------*
181400101026     c     CLSCONN       BEGSR
181500101026      /FREE
181600101026
181700101026           if UBBVSQL01_Disconnect(pInConn) = 0;
181800101026           else;
181900101026              *in40 = *on;
182000101026           endif;
182100101026
182200101026      /END-FREE
182300101026      *
182400101026     c                   ENDSR
182500101026     c*------------------------------------------------------------------------*
182600101026
182700101026
182800101026
182900970526     C*------------------------------------------------------------------------*
183000970526     C* *INZSR - OPERAZIONI INIZIALI
183100970526     C*------------------------------------------------------------------------*
183200970526     C     *INZSR        BEGSR
183300970526     C*---
183400970526     C* RICEVIMENTO PARAMETRI
183500970526     C*---
183600970526     C     *ENTRY        PLIST
183700970612     C                   PARM                    KPJBA
183800991028     C                   movel     kpjbu         tis720
183900000420     C*
184000000420     C* Inverto subito la data presa in input
184100000420     C                   Z-ADD     s20dti        wda8
184200000420     C                   Z-ADD     *ZEROS        da8w
184300000420     C                   EXSR      CHKDA8
184400000420     C                   Z-ADD     DA8W          KCEdti
184500130215     C*
184600130215     C     *DTAARA       DEFINE    §azute        azuteds
184700130215     C     *DTAARA       DEFINE    §datiute      ddatiute
184800130215     C                   IN(E)     *DTAARA
184900130215if  1C                   IF        %Error
185000130215     c                   EVAL      I34Tla = 'L'
185100130215     C                   CALL      'TIBS34R'
185200130215     C                   PARM                    Tibs34Ds
185300130215     C                   IN        *DTAARA
185400130215e   1C                   ENDIF
185500970526     C*
185600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
185700970526     C     XMSGPA        PLIST
185800970526     C                   PARM                    DSMSMS
185900970526     C                   PARM                    DSMSPG
186000970526     C                   PARM                    DSMSKE
186100970526     C                   PARM                    DSMSER
186200991108     C*---
186300970526     C*
186400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
186500970526     C                   CALL      'XKEY'
186600970526     C                   PARM                    DSKEY
186700101026     C*
186800101026     C* CARICA LE TABELLE OCCORRENTI
186900101027     C                   EXSR      CARTAB
187000111215     C*
187100111215     C* Reperisco il sistema AS/400 corrente
187200111215     C                   EVAL      AS400_ok = *off
187300111215     C                   callp     UBRTVNETA_Rtv(currSysNeta)
187400111215     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
187500111215     C                   EVAL      AS400_ok = *on
187600111215     C                   ENDIF
187700101026     C*
187800130214     C* Apro la connessione al DB "BARTVAS" (solo se no solo visualizzazione)
187900130215     C* (questa attività costa risorse, per cui si permette solo in gestione oppure anche in
188000130215     C*  visualizzazione se l'utente è di sede;
188100130215     C*  testo solo se l'utente è di sede, perché quelli di filiale non possono andare in gestione,
188200130215     C*  quindi non potrebbero comunque)
188300130215     C                   IF        DUtLPO = 'S'
188400111215     C                   IF        AS400_ok = *on
188500111215     C                   exsr      opnCONN
188600111215     C                   ENDIF
188700130214     C                   ENDIF
188800970526     C*---
188900970526     C* VARIABILI NON RIFERITE AL DATA BASE
189000970526     C*---
189100970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
189200991025     C     *LIKE         DEFINE    S20OP0        DEPOP0
189300991025     C     *LIKE         DEFINE    S20OP1        DEPOP1
189400991025     C     *LIKE         DEFINE    S20OP0        WOP0
189500991025     C     *LIKE         DEFINE    S20OP1        WOP1
189600970526     C*---
189700970526     C* VARIABILI DI CONTROLLO
189800970526     C*---
189900970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
190000970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
190100970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
190200970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
190300970526     C*
190400970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
190500991026     C                   MOVEL     'TIS723R'     DSMSPG
190600991026     C                   MOVEL     'TIS723R'     MSGQUE
190700970526     C                   MOVEL     *BLANKS       MSGKEY
190800970526     C                   MOVEL     *BLANKS       DSMSMS
190900970526     C*---
191000970526     C* VARIABILI RIFERITE AL DATA BASE
191100970526     C*---
191200991026     C     *LIKE         DEFINE    virtip        KCEtip
191300991026     C     *LIKE         DEFINE    virksc        KCEksc
191400000420     C     *LIKE         DEFINE    virdti        KCEdti
191500970526     C*---
191600970526     C* CHIAVI DI LETTURA
191700970526     C*---
191800000420     C* LETTURA tivir03L - INCOMPLETA
191900991026     C     KEYvir        KLIST
192000080603     C                   KFLD                    KCEtip
192100080603     C                   KFLD                    KCEksc
192200080603     C* LETTURA tivir03L - COMPLETA
192300080603     C     KEYvi2        KLIST
192400080603     C                   KFLD                    KCEtip
192500080603     C                   KFLD                    KCEksc
192600080603     C                   KFLD                    KCEdti
192700080603     C* LETTURA tivire1L - COMPLETA
192800080603     C     KEYvire1_C    KLIST
192900080603     C                   KFLD                    KCEksc
193000080603     C                   KFLD                    KCEtip
193100080603     C                   KFLD                    KCEdti
193200080603     C                   KFLD                    vireTRC
193300080603     C* LETTURA tntbe01L - PARZIALE
193400040816     C     KEYtbe1_P     KLIST
193500040816     C                   KFLD                    TBEcod                         *CODICE TABELLA
193600120111     C* LETTURA tivir06L - COMPLETA
193700120111     C     K03VIR06      KLIST
193800120111     C                   KFLD                    x_VIRKsc
193900120111     C                   KFLD                    x_VIRTip
194000120111     C                   KFLD                    x_VIRDtF
194100120111     C* LETTURA tivir06L - PARZIALE
194200120111     C     K02VIR06      KLIST
194300120111     C                   KFLD                    x_VIRKsc
194400120111     C                   KFLD                    x_VIRTip
194500970528     C*
194600970528     C* CALCOLA LA DATA CORRENTE
194700970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
194800970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
194900970528     C                   Z-ADD     WN8           G08DAT
195000051122     C                   Z-ADD     WN8           DATCORINV         8 0
195100970528     C                   Z-ADD     *ZEROS        G08INV
195200970528     C                   MOVEL     '0'           G08ERR
195300970528     C                   CALL      'XSRDA8'
195400970528     C                   PARM                    WLBDA8
195500970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
195600970526     C*
195700970526     C* DEPOSITA LE OPZIONI DI ENTRATA
195800991025     C                   MOVEL     S20OP0        DEPOP0                         *LIVELLO PROCEDURA
195900991025     C                   MOVEL     S20OP1        DEPOP1                         *LIVELLO PROGRAMMA
196000051122     C*
196100051122     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
196200051122     C                   IF        DEPOP1 = 'F06' OR
196300051122     C                             DEPOP1 = 'O03' OR
196400051122     C                             DEPOP1 = 'O07'
196500051122     C                   EVAL      VIDDTI = DATCORINV
196600060726     C                   EVAL      VIDDTF = 31122039
196700051122     C                   ENDIF
196800000629     C*
196900000629     C* EFFETTUO CONSIDERAZIONI SULLE OPZIONI DI ENTRATA
197000000629     C                   IF        DEPOP1 = 'F06' OR
197100000629     C                             DEPOP1 = 'O03'
197200000629     C                   SETOFF                                       92
197300000629     C                   ELSE
197400000629     C                   SETON                                        92
197500000629     C                   ENDIF
197600970526     C*
197700970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
197800970526     C                   EXSR      IMPFUN
197900970526     C*
198000970526     C                   ENDSR
198100970526     C*------------------------------------------------------------------------*
198200970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
198300970526     C*------------------------------------------------------------------------*
198400970526     C     FUNGES        BEGSR
198500970526     C*
198600970526     C* PULISCO CAMPI DI LAVORO
198700970526     C                   Z-ADD     0             W                 5 0
198800970526     C*
198900970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
199000970526     C* E CARICO TABELLA FUNZIONI ATTIVE
199100970526     C     1             DO        24            I
199200970526     C                   MOVEL     FUD(I)        FUDRIG
199300970526     C*
199400970526     C* PULIZIA FUNZIONI PRECEDENTI
199500970526     C                   MOVEL     *BLANKS       FUV(I)
199600970526     C*
199700970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
199800970526     C* (SALTA F24)
199900970526     C     FUDATT        IFEQ      'S'
200000970526     C     FUDFUN        ANDNE     '24'
200100970526     C                   ADD       1             W
200200970526     C                   MOVEL     FUDDES        FUV(W)
200300970526     C                   END
200400970526     C                   END                                                    DO WHILE
200500970526     C*
200600970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
200700970526     C* ALTRIMENTI DISATTIVO F24
200800970526     C     1             DO        24            I
200900970526     C                   MOVEL     FUD(I)        FUDRIG
201000970526     C*
201100970526     C     FUDFUN        IFEQ      '24'
201200970526     C     W             IFGT      6
201300970526     C                   MOVEL     'S'           FUDATT
201400970526     C                   ELSE
201500970526     C                   MOVEL     *BLANKS       FUDATT
201600970526     C                   END                                                    W GT 8
201700970526     C                   MOVEL     FUDRIG        FUD(I)
201800970526     C                   END
201900970526     C                   END                                                    DO WHILE
202000970526     C*
202100970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
202200970526     C     W             DIV       6             I
202300970526     C                   MVR                     Z
202400970526     C*
202500970526     C     Z             IFNE      0
202600970526     C     I             ADD       1             Z
202700970526     C                   ELSE
202800970526     C                   Z-ADD     I             Z                 5 0
202900970526     C                   END
203000970526     C*
203100970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
203200970526     C                   ADD       1             FUNGRU
203300970526     C     FUNGRU        IFGT      Z
203400970526     C     FUNGRU        ORLT      1
203500970526     C                   Z-ADD     1             FUNGRU
203600970526     C                   END
203700970526     C*
203800970526     C* PRIMO GRUPPO DI 6 FUNZIONI
203900970526     C     FUNGRU        IFEQ      1
204000970526     C                   MOVEL     FUV(1)        FUN001
204100970526     C                   MOVEL     FUV(2)        FUN002
204200970526     C                   MOVEL     FUV(3)        FUN003
204300970526     C                   MOVEL     FUV(4)        FUN004
204400970526     C                   MOVEL     FUV(5)        FUN005
204500970526     C                   MOVEL     FUV(6)        FUN006
204600970526     C                   END
204700970526     C*
204800970526     C* SECONDO GRUPPO DI 6 FUNZIONI
204900970526     C     FUNGRU        IFEQ      2
205000970526     C                   MOVEL     FUV(7)        FUN001
205100970526     C                   MOVEL     FUV(8)        FUN002
205200970526     C                   MOVEL     FUV(9)        FUN003
205300970526     C                   MOVEL     FUV(10)       FUN004
205400970526     C                   MOVEL     FUV(11)       FUN005
205500970526     C                   MOVEL     FUV(12)       FUN006
205600970526     C                   END
205700970526     C*
205800970526     C* TERZO GRUPPO DI 6 FUNZIONI
205900970526     C     FUNGRU        IFEQ      3
206000970526     C                   MOVEL     FUV(13)       FUN001
206100970526     C                   MOVEL     FUV(14)       FUN002
206200970526     C                   MOVEL     FUV(15)       FUN003
206300970526     C                   MOVEL     FUV(16)       FUN004
206400970526     C                   MOVEL     FUV(17)       FUN005
206500970526     C                   MOVEL     FUV(18)       FUN006
206600970526     C                   END
206700970526     C*
206800970526     C* QUARTO GRUPPO DI 6 FUNZIONI
206900970526     C     FUNGRU        IFEQ      4
207000970526     C                   MOVEL     FUV(19)       FUN001
207100970526     C                   MOVEL     FUV(20)       FUN002
207200970526     C                   MOVEL     FUV(21)       FUN003
207300970526     C                   MOVEL     FUV(22)       FUN004
207400970526     C                   MOVEL     *BLANKS       FUN005
207500970526     C                   MOVEL     *BLANKS       FUN006
207600970526     C                   END
207700970526     C*
207800970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
207900970526     C                   MOVEL     *BLANKS       FUN007
208000970526     C                   MOVEL     *BLANKS       FUN008
208100970526     C*
208200970526     C     1             DO        24            I
208300970526     C                   MOVEL     FUD(I)        FUDRIG
208400970526     C     FUDFUN        IFEQ      '24'
208500970526     C     FUDATT        ANDEQ     'S'
208600970526     C                   MOVEL     FUDDES        FUN008
208700970526     C                   END
208800970526     C                   END                                                    DO
208900970526     C*
209000970526     C                   ENDSR
209100970526     C*------------------------------------------------------------------------*
209200970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
209300970526     C*------------------------------------------------------------------------*
209400970526     C     FUNCON        BEGSR
209500970526     C*
209600970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
209700970526     C                   EXSR      FUNVAR
209800970526     C*
209900970526     C* PULISCO CAMPI DI LAVORO
210000970526     C                   Z-ADD     0             I                 5 0
210100970526     C                   Z-ADD     0             W                 5 0
210200970526     C*
210300970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
210400970526     C     FUNKEY        IFNE      *BLANKS
210500970526     C     1             DO        24            I
210600970526     C                   MOVEL     FUD(I)        FUDRIG
210700970526     C*
210800970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
210900970526     C     FUNKEY        IFEQ      FUDFUN
211000970526     C     FUDATT        ANDEQ     'S'
211100970526     C                   Z-ADD     I             W
211200970526     C                   END
211300970526     C                   END                                                    DO WHILE
211400970526     C*
211500970526     C* FUNZIONE NON ATTIVA (ERRORE)
211600970526     C     W             IFEQ      0
211700970526     C*
211800970526     C* EMISSIONE MESSAGGIO DI ERRORE
211900970526     C                   MOVEL     MSG(1)        DSMSMS
212000970526     C*
212100970526     C                   END                                                    W=0
212200970526     C                   END                                                    FUNKEY NE *BLANKS
212300970526     C*
212400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
212500970526     C     W             IFNE      0
212600970526     C     FUNKEY        OREQ      *BLANKS
212700970526     C     FUNKEY        IFEQ      *BLANKS
212800970526     C                   EXSR      ENTGES
212900970526     C                   END
213000031229     C     FUNKEY        IFEQ      '01'
213100031229     C                   EXSR      F01GES
213200031229     C                   END
213300141111     C     FUNKEY        IFEQ      '02'
213400141111     C                   EXSR      F02GES
213500141111     C                   END
213600970526     C     FUNKEY        IFEQ      '03'
213700970526     C                   EXSR      F03GES
213800970526     C                   END
213900970526     C     FUNKEY        IFEQ      '06'
214000970526     C                   EXSR      F06GES
214100970526     C                   END
214200970526     C     FUNKEY        IFEQ      '12'
214300970526     C                   EXSR      F12GES
214400970526     C                   END
214500970526     C     FUNKEY        IFEQ      '24'
214600970526     C                   EXSR      F24GES
214700970526     C                   END
214800970526     C                   END
214900970526     C*
215000970526     C                   ENDSR
215100970526     C*------------------------------------------------------------------------*
215200970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
215300970526     C*------------------------------------------------------------------------*
215400970526     C     FUNVAR        BEGSR
215500970526     C*
215600970526     C* INIZIALIZZA VARIABILE -FUNKEY-
215700970526     C*
215800970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
215900970526     C     KEY           IFEQ      ENTER
216000970526     C                   MOVEL     *BLANKS       FUNKEY
216100970526     C                   END
216200970526     C     KEY           IFEQ      F1
216300970526     C                   MOVEL     '01'          FUNKEY
216400970526     C                   END
216500970526     C     KEY           IFEQ      F2
216600970526     C                   MOVEL     '02'          FUNKEY
216700970526     C                   END
216800970526     C     KEY           IFEQ      F3
216900970526     C                   MOVEL     '03'          FUNKEY
217000970526     C                   END
217100970526     C     KEY           IFEQ      F4
217200970526     C                   MOVEL     '04'          FUNKEY
217300970526     C                   END
217400970526     C     KEY           IFEQ      F5
217500970526     C                   MOVEL     '05'          FUNKEY
217600970526     C                   END
217700970526     C     KEY           IFEQ      F6
217800970526     C                   MOVEL     '06'          FUNKEY
217900970526     C                   END
218000970526     C     KEY           IFEQ      F7
218100970526     C                   MOVEL     '07'          FUNKEY
218200970526     C                   END
218300970526     C     KEY           IFEQ      F8
218400970526     C                   MOVEL     '08'          FUNKEY
218500970526     C                   END
218600970526     C     KEY           IFEQ      F9
218700970526     C                   MOVEL     '09'          FUNKEY
218800970526     C                   END
218900970526     C     KEY           IFEQ      F10
219000970526     C                   MOVEL     '10'          FUNKEY
219100970526     C                   END
219200970526     C     KEY           IFEQ      F11
219300970526     C                   MOVEL     '11'          FUNKEY
219400970526     C                   END
219500970526     C     KEY           IFEQ      F12
219600970526     C                   MOVEL     '12'          FUNKEY
219700970526     C                   END
219800970526     C     KEY           IFEQ      F13
219900970526     C                   MOVEL     '13'          FUNKEY
220000970526     C                   END
220100970526     C     KEY           IFEQ      F14
220200970526     C                   MOVEL     '14'          FUNKEY
220300970526     C                   END
220400970526     C     KEY           IFEQ      F15
220500970526     C                   MOVEL     '15'          FUNKEY
220600970526     C                   END
220700970526     C     KEY           IFEQ      F16
220800970526     C                   MOVEL     '16'          FUNKEY
220900970526     C                   END
221000970526     C     KEY           IFEQ      F17
221100970526     C                   MOVEL     '17'          FUNKEY
221200970526     C                   END
221300970526     C     KEY           IFEQ      F18
221400970526     C                   MOVEL     '18'          FUNKEY
221500970526     C                   END
221600970526     C     KEY           IFEQ      F19
221700970526     C                   MOVEL     '19'          FUNKEY
221800970526     C                   END
221900970526     C     KEY           IFEQ      F20
222000970526     C                   MOVEL     '20'          FUNKEY
222100970526     C                   END
222200970526     C     KEY           IFEQ      F21
222300970526     C                   MOVEL     '21'          FUNKEY
222400970526     C                   END
222500970526     C     KEY           IFEQ      F22
222600970526     C                   MOVEL     '22'          FUNKEY
222700970526     C                   END
222800970526     C     KEY           IFEQ      F23
222900970526     C                   MOVEL     '23'          FUNKEY
223000970526     C                   END
223100970526     C     KEY           IFEQ      F24
223200970526     C                   MOVEL     '24'          FUNKEY
223300970526     C                   END
223400970526     C     KEY           IFEQ      ROLLUP
223500970526     C                   MOVEL     'RU'          FUNKEY
223600970526     C                   END
223700970526     C     KEY           IFEQ      HELP
223800970526     C                   MOVEL     'HP'          FUNKEY
223900970526     C                   END
224000970526     C                   ENDSR
224100970526     C*------------------------------------------------------------------------*
224200970526     C* NOTE COSTRUZIONE SCHIERE
224300970526     C*------------------------------------------------------------------------*
224400970526     C*
224500970526     C* -MSG-MESSAGGI
224600970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
224700970526     C* (NON TOCCARE MSG,1 E MSG,2)
224800970526     C*
224900970526     C* -CMD-COMANDI
225000970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
225100970526     C*
225200970526     C* -OPD-OPZIONI
225300970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
225400970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
225500970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
225600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
225700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
225800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
225900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
226000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
226100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
226200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
226300970526     C*
226400970526     C* -FUD-FUNZIONI
226500970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
226600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
226700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
226800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
226900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
227000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
227100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
227200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
227300970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
227400970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
227500970526     O*------------------------------------------------------------------------*
227600970526** DEC - DECODIFICHE
227700970526    MODIFICA                                                      1
227800970526  ANNULLAMENTO                                                    2
227900970526VISUALIZZAZIONE                                                   3
228000970526   RIPRISTINO                                                     4
228100970526  INSERIMENTO                                                     5
228200991014     COPIA                                                        6
228300970526** MSG - MESSAGGI
228400991026TIS723R- Tasto funzionale non gestito                                          1
228500991026TIS723R- Inserimento effettuato                                                2
228600991026TIS723R- Modifica effettuata                                                   3
228700991026TIS723R- Annullamento effettuato                                               4
228800991026TIS723R- Ripristino effettuato                                                 5
228900991026TIS723R- Richiesta funzione per record inesistente                             6
229000991026TIS723R- Record momentaneamente allocato: riprovare più tardi                  7
229100991026TIS723R- Richiesta funzione per record esistente                               8
229200991026TIS723R- Questo codice cliente è già presente con questo tipo file             9
229300991027TIS723R- Il codice tipo file non è fra quelli presenti in anagrafico           10
229400991027TIS723R- Il codice cliente non è presente come padre nell'archivio UNIFICANTI  11
229500000724TIS723R- Campo obbligatorio                                                    12
229600991027TIS723R- Codice filiale inesistente o di categoria V=varia                     13
229700000724TIS723R- Data errata                                                           14
229800000724TIS723R- La data fine deve essere maggiore della data inizio                   15
229900000724TIS723R- Il record non può scadere in un giorno già trascorso                  16
230000000724TIS723R- Il record immesso si sovrappone ad uno già in decorrenza              17
230100000724TIS723R- Codice tipo servizio non presente in tabella                          18
230200001120TIS723R- Oggetto inesistente, verificare e correggere.                         19
230300001120TIS723R- File inesistente, verificare e correggere.                            20
230400001120TIS723R- Libreria inesistente, verificare e correggere.                        21
230500030603TIS723R- Tipo membro non impostabile x il tipo servizio indicato.              22
230600040107TIS723R- Per il traduttore indicato non esiste descrizione dettaglio parametri 23
230700040319TIS723R- Tipo file e servizio incompatibili, correggere.                       24
230800040816TIS723R- Tipo file e/o programma traduttore e/o file tradotto incompatibili.   25
230900051026TIS723R- ATTENZIONE presenti log a stato 0 impossibile cambiare traduttore!    26
231000060616TIS723R- Valori incompatibili, correggere!                                     27
231100101026TIS723R- Errore BartVAS: fatal error                                           28
231200101027TIS723R- ATTENZIONE: modifica effettuata lato AS/400 ma non lato BARTVAS       29
231300120109TIS723R- Tipo file per Internet non impostabile x il tipo servizio indicato.   30
231400120119TIS723R- Tipo file non abilitato per cliente indicato                          31
231500120119TIS723R- Loop tra il tipo file / cliente corrente e quello per duplica         32
231600120119TIS723R- Tipo file / cliente per duplica non possono essere quelli correnti    33
231700120119TIS723R- Valorizzato il cliente per duplica ma non il rispettivo tipo file     34
231800970526** FUD - FUNZIONI
231900141111F01=Desc.Parametri -        (S/N)=S  <== S-FISSO           funz.01¬
232000141111F02=Parametri      -        (S/N)=N                        funz.02¬
232100970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
232200970526                   -        (S/N)=N                        funz.04¬
232300970526                   -        (S/N)=N                        funz.05¬
232400970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
232500970527                   -        (S/N)=N                        funz.07¬
232600970527                   -        (S/N)=N                        funz.08¬
232700970527                   -        (S/N)=N                        funz.09¬
232800970527                   -        (S/N)=N                        funz.10¬
232900970526                   -        (S/N)=N                        funz.11¬
233000970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
233100970526                   -        (S/N)=N                        funz.13¬
233200970526                   -        (S/N)=N                        funz.14¬
233300970526                   -        (S/N)=N                        funz.15¬
233400970526                   -        (S/N)=N                        funz.16¬
233500970526                   -        (S/N)=N                        funz.17¬
233600970526                   -        (S/N)=N                        funz.18¬
233700970526                   -        (S/N)=N                        funz.19¬
233800970526                   -        (S/N)=N                        funz.20¬
233900970526                   -        (S/N)=N                        funz.21¬
234000970526                   -        (S/N)=N                        funz.22¬
234100970526                   -        (S/N)=N                        funz.23¬
234200970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
