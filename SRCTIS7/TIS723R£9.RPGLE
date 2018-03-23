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
021700150114     C                   SETOFF                                       6970
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
029200150113     C*
029300150113     C                   clear                   trulkpjds
029400150113     C*
029500150113     C* Gestione estensione anagrafica clienti/tipi file => TRC PE
029600150113     C                   CLEAR                   DVIREPE
029700150113     C                   EVAL      KCEdti  = virDTI
029800150113     C                   EVAL      vireTRC = 'PE'
029900150113     C     KEYvire1_C    CHAIN     tivire1l
030000150113     C                   IF        %found(tivire1l)
030100150113     C                   EVAL      DVIREPE = vireFLO
030200150113     C                   EVAL      OKPJKPJBU = §virePEKPJ
030300150113     C                   ENDIF
030400141111     C*
030500141111     C                   call      'TRULKPJR'
030600141111     C                   parm                    kpjba
030700141111     C                   parm                    trulkpjds
030800141111     C*
030900141111     C* Gestione estensione anagrafica clienti/tipi file => TRC PE
031000150113     C                   IF        OKPJOPZ = 'F06'
031100141111     C                   CLEAR                   DVIREPE
031200141111     C                   EVAL      KCEdti  = virDTI
031300141111     C                   EVAL      vireTRC = 'PE'
031400141111     C     KEYvire1_C    CHAIN     tivire1l
031500141111     C                   IF        %found(tivire1l)
031600141111     C                   IF        OKPJKPJBU <> *blanks
031700141111     C                   EVAL      §virePEKPJ = OKPJKPJBU
031800141111     C                   EVAL      vireFLO = DVIREPE
031900141111     C                   UPDATE    tivire00
032000141111     C                   ELSE
032100141111     C                   DELETE    tivire00
032200141111     C                   ENDIF
032300141111     C                   ELSE
032400141111     C                   IF        OKPJKPJBU <> *blanks
032500141111     C                   EVAL      vireKSC = virKSC
032600141111     C                   EVAL      vireTIP = virTIP
032700141111     C                   EVAL      vireDTI = virDTI
032800141111     C                   EVAL      vireTRC = 'PE'
032900141111     C                   EVAL      §virePEKPJ = OKPJKPJBU
033000141111     C                   EVAL      vireFLO = DVIREPE
033100141111     C                   WRITE     tivire00
033200141111     C                   ENDIF
033300141111     C                   ENDIF
033400150113     C                   ENDIF
033500141111     C*
033600141111     C                   ENDSR
033700970526     C*------------------------------------------------------------------------*
033800970526     C* F03GES - GESTIONE F03 -> FINE
033900970526     C*------------------------------------------------------------------------*
034000970526     C     F03GES        BEGSR
034100970526     C*
034200970526     C* IMPOSTA "PREMUTO F03"
034300970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
034400991025     C                   MOVEL     '1'           S20F03                         *DI PROCEDURA
034500970526     C*
034600970526     C                   ENDSR
034700970526     C*------------------------------------------------------------------------*
034800970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
034900970526     C*------------------------------------------------------------------------*
035000970526     C     F06GES        BEGSR
035100970526     C*
035200970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
035300970526IF  1C     DEPOP1        IFEQ      'F06'
035400970526     C                   EXSR      INSERI
035500970526E   1C                   ENDIF
035600970526IF  1C     DEPOP1        IFEQ      'O02'
035700970526     C                   EXSR      MODIFI
035800970526E   1C                   ENDIF
035900991014IF  1C     DEPOP1        IFEQ      'O03'
036000011129     C                   EXSR      COPIA
036100991014E   1C                   ENDIF
036200991104IF  1C*    DEPOP1        IFEQ      'O04'
036300991104     C*                  EXSR      ANNULL
036400991104E   1C*                  ENDIF
036500970526IF  1C     DEPOP1        IFEQ      'O07'
036600970526     C                   EXSR      RIPRIS
036700970526E   1C                   ENDIF
036800080603     C*
036900080603     C* Gestione estensione anagrafica clienti/tipi file => TRC 01
037000080603     C                   CLEAR                   DVIRE01
037100080603     C                   EVAL      KCEdti  = virDTI
037200080603     C                   EVAL      vireTRC = '01'
037300080603     C     KEYvire1_C    CHAIN     tivire1l
037400080603     C                   IF        %found(tivire1l)
037500080603     C                   IF        vidEML <> *blanks
037600080603     C                   EVAL      §vire01INV = 'S'
037700080603     C                   EVAL      §vire01EML = vidEML
037800080603     C                   EVAL      vireFLO = DVIRE01
037900080603     C                   UPDATE    tivire00
038000080603     C                   ELSE
038100080603     C                   DELETE    tivire00
038200080603     C                   ENDIF
038300080603     C                   ELSE
038400080603     C                   IF        vidEML <> *blanks
038500080603     C                   EVAL      vireKSC = virKSC
038600080603     C                   EVAL      vireTIP = virTIP
038700080603     C                   EVAL      vireDTI = virDTI
038800080603     C                   EVAL      vireTRC = '01'
038900080603     C                   EVAL      §vire01INV = 'S'
039000080603     C                   EVAL      §vire01EML = vidEML
039100080603     C                   EVAL      vireFLO = DVIRE01
039200080603     C                   WRITE     tivire00
039300080603     C                   ENDIF
039400080603     C                   ENDIF
039500970526     C*
039600011129     C* Dopo aver effettuato le operazioni richieste apro la tabela correlata al tipo file.
039700011129     C                   IF        errl00 = '0'                                 *NO ERRORI
039800011129     C                   CALL      'TIS70UR'
039900011129     C                   PARM                    KPJBA
040000011129     C                   PARM                    VIDTIP
040100011129     C                   PARM                    VIDKSC
040200011129     C                   PARM      *blanks       flgope            1
040300011129     C                   ENDIF
040400011129     C*
040500970526     C                   ENDSR
040600970526     C*------------------------------------------------------------------------*
040700970526     C* F12GES - GESTIONE F12 -> RITORNO
040800970526     C*------------------------------------------------------------------------*
040900970526     C     F12GES        BEGSR
041000970526     C*
041100970526     C* IMPOSTA "PREMUTO F12"
041200970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
041300991025     C                   MOVEL     '1'           S20F12                         *DI PROCEDURA
041400970526     C*
041500970526     C                   ENDSR
041600970526     C*------------------------------------------------------------------------*
041700970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
041800970526     C*------------------------------------------------------------------------*
041900970526     C     F24GES        BEGSR
042000970526     C*
042100970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
042200970526     C*
042300970526     C                   ENDSR
042400970526     C*------------------------------------------------------------------------*
042500970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
042600970526     C*------------------------------------------------------------------------*
042700970526     C     INSERI        BEGSR
042800970526     C*
042900970526     C* CONTROLLA I CAMPI IMMESSI
043000970526     C                   EXSR      DECODI
043100970526     C*
043200970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
043300991026IF  1C                   if        errl00 = '0'                                 *NO ERRORI
043400991026     C                   movel     VIDTip        virtip
043500991026     C                   move      VIDksc        virksc
043600991026     C                   z-add     VIDseq        virseq
043700991026     C                   Z-ADD     viddti        G08dat
043800991026     C                   Z-ADD     *ZEROS        G08inv
043900991026     C                   MOVEL     '0'           G08ERR
044000991026     C                   CALL      'XSRDA8'
044100991026     C                   PARM                    WLBDA8
044200991026     C                   Z-ADD     G08inv        virdti
044300991026     c                   exsr      impvir
044400991026     C                   WRITE     tivir000
044500991025     C                   MOVEL     MSG(2)        S20MSG                         *MESSAGGIO
044600970602     C*
044700970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
044800020228     C****               EXSR      AZZVID
044900970526E   1C                   ENDIF
045000970526     C*
045100970526     C                   ENDSR
045200970526     C*------------------------------------------------------------------------*
045300970526     C* MODIFI - OPERAZIONI DI MODIFICA
045400970526     C*------------------------------------------------------------------------*
045500970526     C     MODIFI        BEGSR
045600970526     C*
045700970526     C* CONTROLLA I CAMPI IMMESSI
045800970526     C                   EXSR      DECODI
045900970526     C*
046000970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
046100970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
046200991026     C                   EXSR      LETvir
046300970526     C*
046400970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
046500970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
046600991026     C                   EXSR      IMPvir                                       *IMPOSTA I CAMPI
046700991026     C                   UPDATE    tivir000
046800101027     C                   EVAL      S20MSG = MSG(3)
046900101027     C                   EVAL      DSMSMS = MSG(3)
047000101027     C*
047100101027     C* Se trattasi d tipo servizio "idoneo" modifico anche i dati utente lato "BARTVAS"
047200101027     C                   IF        *IN80
047300111215     C                             and AS400_ok = *on
047400101027      /FREE
047500101027
047600101027         if *in30 = *off;
047700101027           pInMaskChk  = VIDPPIMC;
047800101027         else;
047900101027           pInMaskChk  = pOutCliMskChk;
048000101027         endif;
048100101027
048200101027         if *in31 = *off;
048300101027           pInMaskFile = VIDPPIMF;
048400101027         else;
048500101027           pInMaskFile = pOutCliMskFil;
048600101027         endif;
048700101027
048800101027         if *in32 = *off;
048900101027           pInMaskSem  = VIDPPIMS;
049000101027         else;
049100101027           pInMaskSem  = pOutCliMskSem;
049200101027         endif;
049300101027
049400101027         if *in33 = *off;
049500101027           pInMaskZip  = VIDPPIMZ;
049600101027         else;
049700101027           pInMaskZip  = pOutCliMskZip;
049800101027         endif;
049900101027
050000101027
050100101027         if UBBVSQL01_Upd01UnqCliTipFile(
050200101027                         pInConn
050300101027                       : VIDKSC
050400101027                       : VIDTIP
050500101027                       : pInMaskChk
050600101027                       : pInMaskFile
050700101027                       : pInMaskSem
050800101027                       : pInMaskZip) = 0;
050900101027         else;
051000101027              S20MSG = MSG(29);
051100101027              DSMSMS = MSG(29);
051200101027         endif;
051300101027
051400101027      /END-FREE
051500101027     C
051600101027     C*
051700101027     C* Quindi (già che c sono ricarico anche il job
051800101027     C                   callp(e)  UBBVSKT01_ReloadCliTipF(
051900101027     C                                           VIDKSC
052000101027     C                                          :VIDTIP
052100101027     C                                          :pOutCliProc)
052200101027     C                   ENDIF
052300101027     C*
052400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
052500970526E   2C                   ENDIF
052600970526E   1C                   ENDIF
052700970526     C*
052800970526     C                   ENDSR
052900991014     C*------------------------------------------------------------------------*
053000991014     C* copia  - OPERAZIONI DI copia
053100991014     C*------------------------------------------------------------------------*
053200991014     C     copia         BEGSR
053300991014     C*
053400991014     C* CONTROLLA I CAMPI IMMESSI
053500991014     C                   EXSR      DECODI
053600991014     C*
053700991014     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
053800991014IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
053900991026     C                   movel     VIDTip        virtip
054000991110     C                   move      VIDksc        virksc
054100991110     C                   z-add     VIDseq        virseq
054200991110     C                   Z-ADD     viddti        G08dat
054300991110     C                   Z-ADD     *ZEROS        G08inv
054400991110     C                   MOVEL     '0'           G08ERR
054500991110     C                   CALL      'XSRDA8'
054600991110     C                   PARM                    WLBDA8
054700991110     C                   Z-ADD     G08inv        virdti
054800991026     C                   EXSR      IMPvir                                       *IMPOSTA I CAMPI
054900991026     C                   write     tivir000
055000991025     C                   MOVEL     MSG(3)        S20MSG
055100991014     C                   MOVEL     '1'           FINL00                         *FINE PGM
055200991014E   2C                   ENDIF
055300991014     C*
055400991014     C                   ENDSR
055500970526     C*------------------------------------------------------------------------*
055600970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
055700970526     C*------------------------------------------------------------------------*
055800991104     C*    ANNULL        BEGSR
055900991025     C*
056000991025     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
056100991104     C*                  EXSR      LETvir
056200991025     C*
056300991025     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
056400991104IF  1C*    ERRL00        IFEQ      '0'                                          *NO ERRORI
056500991104     C*                  DELETE    tivir000                                     *CANCELLA
056600991104     C*                  MOVEL     MSG(4)        s20MSG                         *MESSAGGIO
056700991104     C*                  MOVEL     '1'           FINL00                         *FINE PGM
056800991104E   1C*                  ENDIF
056900970526     C*
057000991104     C*                  ENDSR
057100970526     C*------------------------------------------------------------------------*
057200970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
057300970526     C*------------------------------------------------------------------------*
057400970526     C     RIPRIS        BEGSR
057500970526     C                   ENDSR
057600970526     C*------------------------------------------------------------------------*
057700991026     C* LETvir - LEGGE IL RECORD PERL'AGGIORNAMENTO
057800970526     C*------------------------------------------------------------------------*
057900991026     C     LETvir        BEGSR
058000970526     C*
058100991025     C                   MOVEL     S20tip        KCEtip
058200991026     C                   MOVEL     S20ksc        KCEksc
058300000420     C     KEYvi2        CHAIN     tivir03L                           9998
058400970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
058500970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
058600991025     C                   MOVEL     MSG(7)        S20MSG                         *MESSAGGIO
058700970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
058800970526E   1C                   ENDIF
058900970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
059000970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
059100991025     C                   MOVEL     MSG(6)        S20MSG                         *MESSAGGIO
059200970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
059300970526E   1C                   ENDIF
059400970526     C*
059500970526     C                   ENDSR
059600970526     C*------------------------------------------------------------------------*
059700970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
059800970526     C*------------------------------------------------------------------------*
059900970526     C     DECODI        BEGSR
060000001120     C*
060100001120     C*----------------------
060200001120     C* Controllo del codice
060300001120     C*----------------------
060400001120     C     ERRL00        IFEQ      '0'
060500001120     C     VIDksc        IFne      *blank
060600001120     C     VIDksc        andne     *zeros
060700001120     C                   exsr      cntksc
060800001120     C                   ELSE
060900991027     C                   MOVEL     '1'           ERRL00
061000991027     C                   SETON                                        65
061100991027     C                   MOVEL     MSG(12)       DSMSMS
061200001120     C                   ENDIF
061300001120     C                   ENDIF
061400101026     C*
061500101026     C* Verifico se x il servizio a video è richiesta l'indicazione della password FTP
061600101026     C     ERRL00        IFEQ      '0'
061700101026     C     VIDisv        lookup    isvftpeml                              80
061800101026 xxx C***                setoff                                       80
061900101026     C                   ENDIF
062000001120     C*----------------------
062100001120     C* Controllo della sequenza
062200001120     C*----------------------
062300001120     C     ERRL00        IFEQ      '0'
062400001120     C     VIDseq        IFne      0
062500001120     C                   ELSE
062600991027     C                   MOVEL     '1'           ERRL00
062700991027     C                   SETON                                        67
062800991027     C                   MOVEL     MSG(12)       DSMSMS
062900001120     C                   ENDIF
063000001120     C                   ENDIF
063100120109     C*----------------------
063200120109     C* Controllo tipo file
063300120109     C*----------------------
063400120109     C     ERRL00        IFEQ      '0'
063500120109     C     VIDtip        IFne      *blank
063600120109     C                   exsr      cnttip
063700120109     C                   ELSE
063800120109     C                   MOVEL     '1'           ERRL00
063900120109     C                   SETON                                        66
064000120109     C                   MOVEL     MSG(12)       DSMSMS
064100120109     C                   ENDIF
064200120109     C                   ENDIF
064300120109     C*----------------------
064400120109     C* Tipo servizio
064500120109     C*----------------------
064600120109     C     ERRL00        IFEQ      '0'
064700120109     C     VIDisv        IFne      *blank
064800120109     C                   exsr      cntisv
064900120109     C* Verifico la compatibilità del servizio rispetto al tipo-file
065000120109     C                   IF        VTFUD <> §ISVVERS
065100120109     C                   MOVEL     '1'           ERRL00
065200120109     C                   SETON                                        6670
065300120109     C                   MOVEL     MSG(24)       DSMSMS
065400120109     C                   ENDIF
065500120109     C                   ELSE
065600120109     C                   MOVEL     '1'           ERRL00
065700120109     C                   SETON                                        70
065800120109     C                   MOVEL     MSG(12)       DSMSMS
065900120109     C                   ENDIF
066000120109     C                   ENDIF
066100120109     C*--------------------
066200120109     C* Dati solo x upload no in download
066300120109     C*--------------------
066400120109     C* a seconde se siamo in Download o Upload sproteggo alcuni campi e ne proteggo altri
066500120109     C* e se non abbiamo errori pulisco i campi protetti (se abbiamo un errore potrebbe essere
066600120109     C*  che non siamo in D o U e i valori nei campi servono)
066700120109     C                   IF        §ISVVers   = 'D'
066800120109     c                   SETON                                        93
066900120109     C     ERRL00        IFEQ      '0'
067000120109     C                   MOVEL     *blanks       VIDpoi
067100120109     C                   MOVEL     *blanks       VIDEmL
067200120109     C                   MOVEL     *blanks       VIDfi2
067300120109     C                   MOVEL     *blanks       VIDfi4
067400120109     C                   MOVEL     *blanks       VIDfi4D
067500120109     C                   ENDIF
067600120109     C                   ENDIF
067700120109     C                   IF        §ISVVers   = 'U'
067800120109     C                   SETOFF                                       93
067900120109     C     ERRL00        IFEQ      '0'
068000120109     C                   MOVEL     *blanks       VIDTMbD
068100120109     C                   MOVEL     *blanks       VIDTip2
068200120109     C                   MOVEL     *blanks       VIDTip3
068300120109     C                   MOVEL     *blanks       VIDTip4
068400120119     C                   MOVEL     *blanks       VIDKsc2
068500120119     C                   MOVEL     *blanks       VIDKsc3
068600120119     C                   MOVEL     *blanks       VIDKsc4
068700120109     C                   ENDIF
068800120109     C                   ENDIF
068900100614     C*--------------------
069000100614     C* Tipo membro d default...  solo x anagrafiche d download (ovviamente no in visualizz.)
069100100614     C*--------------------
069200100614     C     ERRL00        IFEQ      '0'
069300100614     C                   IF        DEPOP1 = 'F06' OR
069400100614     C                             DEPOP1 = 'O03'
069500100614     C                   IF        §ISVPROC = 'F' AND
069600100614     C                             §ISVVERS = 'D'
069700100614     C                   IF        VIDtmbd = *blanks AND
069800100614     C                             vtfud   = 'D'
069900100614     C                   MOVEL     'M'           VIDtmbd
070000100614     C                   ENDIF
070100100614     C                   ENDIF
070200100614     C                   ENDIF
070300100614     C                   ENDIF
070400030603     C*----------------------
070500030603     C* Tipo membro
070600030603     C*----------------------
070700030603     C     ERRL00        IFEQ      '0'
070800030603     C     VIDtmbd       IFNE      *blank
070900030603     C                   IF        §ISVPROC = 'F' AND
071000030603     C                             §ISVVERS = 'D'
071100030603     C                   ELSE
071200030603     C                   MOVEL     '1'           ERRL00
071300030603     C                   SETON                                        61
071400030603     C                   MOVEL     MSG(22)       DSMSMS
071500030603     C                   ENDIF
071600030603     C                   ENDIF
071700030603     C                   ENDIF
071800120109     C*----------------------
071900120109     C* Campi duplicazione automatica su tipi file
072000120109     C*----------------------
072100120109     C     ERRL00        IFEQ      '0'
072200120109     C*
072300120109     C                   EVAL      MaxCiclo = %elem(Tips)
072400120109     C                   EVAL      Tips(1)  = VIDTip2
072500120109     C                   EVAL      Tips(2)  = VIDTip3
072600120109     C                   EVAL      Tips(3)  = VIDTip4
072700120119     C* se il tipo file è valorizzato e il cliente relativo no (o 0 o ' ')
072800120119     C* lo forzo col cliente principale
072900120119     C                   IF        (VIDKsc2 = *zeros or
073000120119     C                              VIDKsc2 = *blank   ) and
073100120119     C                             VIDTip2 <> *blank
073200120119     C                   EVAL      VIDKsc2 =  VIDKsc
073300120119     C                   ENDIF
073400120119     C                   IF        (VIDKsc3 = *zeros or
073500120119     C                              VIDKsc3 = *blank   ) and
073600120119     C                             VIDTip3 <> *blank
073700120119     C                   EVAL      VIDKsc3 =  VIDKsc
073800120119     C                   ENDIF
073900120119     C                   IF        (VIDKsc4 = *zeros or
074000120119     C                              VIDKsc4 = *blank   ) and
074100120119     C                             VIDTip4 <> *blank
074200120119     C                   EVAL      VIDKsc4 =  VIDKsc
074300120119     C                   ENDIF
074400120119     C                   EVAL      Kscs(1)  = VIDKsc2
074500120119     C                   EVAL      Kscs(2)  = VIDKsc3
074600120119     C                   EVAL      Kscs(3)  = VIDKsc4
074700120109     C     1             DO        MaxCiclo      i
074800120109     C     Tips(i)       IFNE      *blank
074900120119     C     Kscs(i)       ORNE      *blank
075000120119     C     Kscs(i)       ANDNE     *zeros
075100120109     C                   IF        §ISVVERS = 'D'
075200120119     C                   EXSR      CntTip234
075300120111     C* se mi è stato dato un errore, esco dal ciclo
075400120111     C                   IF        ERRL00 <> '0'
075500120111     C                   LEAVE
075600120111     C                   ENDIF
075700120109     C                   ELSE
075800120109     C                   SELECT
075900120109     C                   WHEN      i = 1
076000120119     C                   SETON                                        7549
076100120109     C                   WHEN      i = 2
076200120119     C                   SETON                                        7650
076300120109     C                   WHEN      i = 3
076400120119     C                   SETON                                        7751
076500120109     C                   ENDSL
076600120109     C                   MOVEL     '1'           ERRL00
076700120109     C                   MOVEL     MSG(30)       DSMSMS
076800120109     C                   ENDIF
076900120109     C                   ENDIF
077000120109     C                   ENDDO
077100120109     C*
077200120109     C                   ENDIF
077300001120     C*----------------------
077400001120     C* Controllo della descrizione
077500001120     C*----------------------
077600001120     C     ERRL00        IFEQ      '0'
077700001120     C     VIDdes        IFne      *blanks
077800001120     C                   ELSE
077900991122     C*                  MOVEL     '1'           ERRL00
078000991122     C*                  SETON                                        62
078100991122     C*                  MOVEL     MSG(12)       DSMSMS
078200991122     C                   MOVEL     VDDTIP        VIDDES
078300001120     C                   ENDIF
078400001120     C                   ENDIF
078500991026     C*--------------------
078600991026     C* DATA inizio
078700991026     C*--------------------
078800991026     C     *LIKE         DEFINE    VIDDti        DtiVID
078900001120     C     ERRL00        IFEQ      '0'
079000991026     C                   Z-ADD     VIDDti        wda8
079100991026     C                   Z-ADD     *ZEROS        da8w
079200991026     C                   EXSR      CHKDA8
079300991026     C                   Z-ADD     WDA8          viddti
079400991026     C                   Z-ADD     DA8W          dtivid
079500001120     C     ERRL00        IFEQ      '1'
079600991026     C                   SETON                                        63
079700991026     C                   MOVEL     MSG(14)       dsmsms
079800001120     C                   ENDIF
079900001120     C                   ENDIF
080000991026     C* INSERIMENTO -> RECORD INESISTENTE
080100001120     C     ERRL00        IFEQ      '0'
080200001120     C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
080300001120     C     DEPOP1        orEQ      'O03'                                        *INSERIMENTO
080400991026     C                   move      vidksc        KCEksc
080500000420     C                   move      vidtip        KCEtip
080600000420     C                   z-add     dtivid        KCEdti
080700000420     C     KEYvi2        CHAIN(N)  tivir03L                           99
080800001120     C     *IN99         IFEQ      *OFF
080900991026     C                   MOVEL     '1'           ERRL00
081000991026     C                   MOVEL     MSG(8)        DSMSMS
081100991026     C                   SETON                                        676566
081200991026     C                   SETON                                        63
081300001120     C                   ENDIF
081400001120     C                   ENDIF
081500001120     C                   ENDIF
081600001120     C*--------------------
081700991026     C* DATA SCADENZA
081800991026     C*--------------------
081900991026     C     *LIKE         DEFINE    VIDDtf        DtfVID
082000001120     C     ERRL00        IFEQ      '0'
082100991026     C                   Z-ADD     VIDDtf        WDA8
082200991026     C                   Z-ADD     *ZEROS        DA8W
082300991026     C                   EXSR      CHKDA8
082400991026     C                   Z-ADD     WDA8          VIDDtf
082500991026     C                   Z-ADD     DA8W          DtfVID
082600001120     C     ERRL00        IFEQ      '1'
082700991026     C                   SETON                                        68
082800991026     C                   MOVEL     MSG(14)       DSMSMS
082900001120     C                   ENDIF
083000001120     C                   ENDIF
083100991026     C* SCADENZA < DECORRENZA
083200001120     C     ERRL00        IFEQ      '0'
083300001120     C     DtfVID        IFLT      DtiVID
083400991026     C                   SETON                                        68
083500991026     C                   MOVEL     MSG(15)       DSMSMS
083600991026     C                   MOVEL     '1'           ERRL00
083700001120     C                   ENDIF
083800001120     C                   ENDIF
083900991026     C* SCADENZA >= DATA CORRENTE
084000141021     C     ERRL00        IFEQ      '0'
084100141021     C***  DEPOP1        IFEQ      'O02'
084200141021     C***  DEPOP1        OREQ      'O07'
084300141021     C***  DEPOP1        OREQ      'F06'
084400141021     C     DEPOP1        IFEQ      'O07'
084500141021     C     DEPOP1        OREQ      'F06'
084600001120     C     DtfVID        IFLT      DATCOR
084700001120     C                   SETON                                        68
084800991026     C                   MOVEL     MSG(16)       DSMSMS
084900991026     C                   MOVEL     '1'           ERRL00
085000001120     C                   ENDIF
085100001120     C                   ENDIF
085200001120     C                   ENDIF
085300991026     C*---
085400991026     C* DECORRENZA + SCADENZA
085500991026     C*---
085600991026     C* ACCAVALLAMENTO DATI
085700001120     C     ERRL00        IFEQ      '0'
085800991026     C                   MOVEL     VIDtip        KCEtip
085900991026     C                   MOVEL     vidksc        KCEksc
086000991026     C     KEyvir        SETLL     tivir03L
086100991026     C     KEyvir        READE(N)  tivir03L                               99
086200001120     C     *IN99         DOWEQ     *OFF
086300001120     C     DEPOP1        IFNE      'F06'                                        *NON INSERIMENTO
086400991026     C     DtiVID        ANDNE     virdti                                       *RECORD IN LINEA
086500991026     C     DEPOP1        OREQ      'F06'                                        *INSERIMENTO
086600001120     C     DtiVID        IFGE      virdti                                       *DECORRENZA
086700991026     C     DtiVID        ANDLE     virdtf
086800991026     C     DtfVID        ORGE      virdti                                       *SCADENZA
086900991026     C     DtfVID        ANDLE     virdtf
087000991026     C     virdti        ORGE      DtiVID                                       *DECORRENZA
087100991026     C     virdtf        ANDLE     DtfVID
087200991026     C                   SETON                                        666765
087300991026     C                   SETON                                        6368
087400991026     C                   MOVEL     MSG(17)       DSMSMS
087500991026     C                   MOVEL     '1'           ERRL00
087600991026     C                   LEAVE
087700001120     C                   ENDIF
087800001120     C                   ENDIF
087900991026     C     KEyvir        READE(N)  tivir03L                               99
088000001120     C                   ENDDO
088100001120     C                   ENDIF
088200001120     C*--------------------
088300001120     C* Controllo filiale
088400001120     C*--------------------
088500001120     C     ERRL00        IFEQ      '0'
088600001120     C     VIDpoi        IFne      *blank
088700001120     C     VIDpoi        andne     *zeros
088800001120     C                   exsr      cntpoi
088900001120     C                   ELSE
089000120109     C     §ISVVers      IFNE      'D'
089100991027     C                   MOVEL     '1'           ERRL00
089200000724     C                   MOVEL     MSG(12)       DSMSMS
089300991027     C                   SETON                                        69
089400991122     C                   ENDIF
089500001120     C                   ENDIF
089600001120     C                   ENDIF
089700001120     C*--------------------
089800011016     C* Programma traduttore
089900001120     C*--------------------
090000001120     C     ERRL00        IFEQ      '0'
090100001120     C     VIDpgt        IFNE      *blank
090200001120     C                   CALL(e)   'TIS721C'
090300050112     C                   PARM      *blanks       LIBRERIA         10
090400001120     C                   PARM      VIDpgt        OBJECT           10
090500001120     C                   PARM      '*PGM   '     TYPE              7
090600001120     C                   PARM      '0'           ESITO             1
090700001120     C                   IF        ESITO = '1'
090800001120     C                   MOVEL     '1'           ERRL00
090900001120     C                   SETON                                        54
091000001120     C                   MOVEL     MSG(19)       DSMSMS
091100001120     C                   ENDIF
091200001120     C                   ENDIF
091300001120     C                   ENDIF
091400001120     C*--------------------
091500001120     C* Programma post-traduzione
091600001120     C*--------------------
091700001120     C     ERRL00        IFEQ      '0'
091800001120     C     VIDpgi        IFNE      *blank
091900001120     C                   CALL(e)   'TIS721C'
092000050112     C                   PARM      *blanks       LIBRERIA         10
092100001120     C                   PARM      VIDpgi        OBJECT           10
092200001120     C                   PARM      '*PGM   '     TYPE              7
092300001120     C                   PARM      '0'           ESITO             1
092400001120     C                   IF        ESITO = '1'
092500001120     C                   MOVEL     '1'           ERRL00
092600001120     C                   SETON                                        55
092700001120     C                   MOVEL     MSG(19)       DSMSMS
092800001120     C                   ENDIF
092900001120     C                   ENDIF
093000001120     C                   ENDIF
093100001120     C*--------------------
093200001120     C* File da tradurre
093300001120     C*--------------------
093400001120     C     ERRL00        IFEQ      '0'
093500001120     C     VIDfid        IFNE      *blank
093600001120     C                   CALL(e)   'TIS721C'
093700050112     C                   PARM      *blanks       LIBRERIA         10
093800001120     C                   PARM      VIDfid        OBJECT           10
093900001120     C                   PARM      '*FILE  '     TYPE              7
094000001120     C                   PARM      '0'           ESITO             1
094100001120     C                   IF        ESITO = '1'
094200001120     C                   MOVEL     '1'           ERRL00
094300001120     C                   SETON                                        53
094400001120     C                   MOVEL     MSG(20)       DSMSMS
094500001120     C                   ENDIF
094600001120     C                   ENDIF
094700001120     C                   ENDIF
094800001120     C*--------------------
094900001120     C* File storico
095000001120     C*--------------------
095100001120     C     ERRL00        IFEQ      '0'
095200001120     C     VIDfis        IFNE      *blank
095300001120     C                   CALL(e)   'TIS721C'
095400050112     C                   PARM      *blanks       LIBRERIA         10
095500001120     C                   PARM      VIDfis        OBJECT           10
095600001120     C                   PARM      '*FILE  '     TYPE              7
095700001120     C                   PARM      '0'           ESITO             1
095800001120     C                   IF        ESITO = '1'
095900001120     C                   MOVEL     '1'           ERRL00
096000001120     C                   SETON                                        56
096100001120     C                   MOVEL     MSG(20)       DSMSMS
096200001120     C                   ENDIF
096300001120     C                   ENDIF
096400001120     C                   ENDIF
096500001120     C*--------------------
096600001120     C* File tradotto
096700001120     C*--------------------
096800001120     C     ERRL00        IFEQ      '0'
096900001120     C     VIDfit        IFNE      *blank
097000001120     C                   CALL(e)   'TIS721C'
097100050112     C                   PARM      VIDfi3        LIBRERIA         10
097200001120     C                   PARM      VIDfit        OBJECT           10
097300001120     C                   PARM      '*FILE  '     TYPE              7
097400001120     C                   PARM      '0'           ESITO             1
097500001120     C                   IF        ESITO = '1'
097600001120     C                   MOVEL     '1'           ERRL00
097700001120     C                   SETON                                        57
097800001120     C                   MOVEL     MSG(20)       DSMSMS
097900001120     C                   ENDIF
098000001120     C                   ENDIF
098100001120     C                   ENDIF
098200001120     C*--------------------
098300001120     C* Libreria file tradotto
098400001120     C*--------------------
098500001120     C     ERRL00        IFEQ      '0'
098600001120     C     VIDfi3        IFNE      *blank
098700001120     C                   CALL(e)   'TIS721C'
098800050112     C                   PARM      *blanks       LIBRERIA         10
098900001120     C                   PARM      VIDfi3        OBJECT           10
099000001120     C                   PARM      '*LIB   '     TYPE              7
099100001120     C                   PARM      '0'           ESITO             1
099200001120     C                   IF        ESITO = '1'
099300001120     C                   MOVEL     '1'           ERRL00
099400001120     C                   SETON                                        52
099500001120     C                   MOVEL     MSG(21)       DSMSMS
099600001120     C                   ENDIF
099700001120     C                   ENDIF
099800001120     C                   ENDIF
099900040816     C*--------------------
100000100311     C* Controllo corrispondenza Tipo File / Programma Traduttore / File Tradotto / Tipo membro Trad
100100040816     C*--------------------
100200040816     C     ERRL00        IFEQ      '0'
100300040816     C     VIDfit        IFne      *blank
100400040816     C     VIDpgt        ORne      *blank
100500040816     C                   EVAL      TBEcod = 'IFT'
100600040816     C     KEYtbe1_P     SETLL     tntbe01l
100700040816     C                   IF        %equal(tntbe01l)
100800040816     C* Inizializzo indici e schiere
100900040816     C                   Z-ADD     *zeros        j
101000040816     C                   CLEAR                   skTipF
101100040816     C                   CLEAR                   skPgt
101200040816     C                   CLEAR                   skFileT
101300100311     C                   CLEAR                   skMembroT
101400040816     C* Carico le schiere da tabella 'IFT'
101500040816     C     KEYtbe1_P     READE     tntbe01l
101600040816     C                   DOW       not %eof(tntbe01l)
101700040816     C                   IF        TBEatb = *blanks
101800040816     C                   EVAL      DIFT = TBEuni
101900040816     C                   ADD       1             j
102000100311     C                   EVAL      skTipF(j)    = TBEke1
102100100311     C                   EVAL      skPgt(j)     = §IFTPGT
102200100311     C                   EVAL      skFileT(j)   = §IFTFILE
102300100311     C                   EVAL      skMembroT(j) = §IFTTMBD
102400040816     C                   ENDIF
102500040816     C     KEYtbe1_P     READE     tntbe01l
102600040816     C                   ENDDO
102700040816     C* Imposto cosa testare
102800040816     C                   IF        VIDPGT <> *blanks
102900040816     C                   EVAL      wVIDPGT = VIDPGT
103000040816     C                   ELSE
103100040816     C                   EVAL      wVIDPGT = VIDPGTD
103200040816     C                   ENDIF
103300040816     C                   IF        VIDFIT <> *blanks
103400040816     C                   EVAL      wVIDFIT = VIDFIT
103500040816     C                   ELSE
103600040816     C                   EVAL      wVIDFIT = VIDFITD
103700040816     C                   ENDIF
103800040816     C* Testo se il traduttore indicato è tra quelli da controllare (in tabella 'IFT')
103900040823     C                   MOVEL     '0'           wFlgIFTok         1
104000100223     C                   SETOFF                                       16
104100040816     C     wVIDPGT       LOOKUP    skPgt                                  15
104200040816     C                   IF        %found
104300040816     C* Testo se vi è corrispondenza
104400040816     C                   EVAL      j = 1
104500040816     C                   DOW       j <= %elem(skTipF)
104600100223     C                   IF        skTipF(j)  = VIDTIP  AND
104700100223     C                             skPgt(j)   = wVIDPGT
104800100223     C                   SETON                                        16
104900100311     C*
105000100223     C                   IF        skFileT(j) = wVIDFIT
105100040816     C                   MOVEL     '1'           wFlgIFTok
105200100311     C*
105300100311     C                   IF        DEPOP1 = 'O02' OR
105400100311     C                             DEPOP1 = 'O03' OR
105500100311     C                             DEPOP1 = 'F06'
105600100312     C                   IF        VIDtmbd      =  *blanks AND
105700100312     C                             skMembroT(j) <> *blanks
105800100311     C                   EVAL      VIDtmbd = skMembroT(j)
105900100311     C                   SETON                                        61
106000100311     C                   ENDIF
106100100311     C                   ENDIF
106200100311     C*
106300040816     C                   LEAVE
106400040816     C                   ENDIF
106500100311     C*
106600100223     C                   ENDIF
106700040816     C                   ADD       1             j
106800040816     C                   ENDDO
106900040816     C* Se nn trovata esatta corrispondenza => errore
107000100223     C                   IF        *IN16 and wFlgIFTok = '0'
107100040816     C                   MOVEL     '1'           ERRL00
107200040816     C                   MOVEL     MSG(25)       DSMSMS
107300040816     C                   SETON                                        665457
107400040816     C                   ENDIF
107500040816     C                   ENDIF
107600040816     C                   ENDIF
107700040816     C                   ENDIF
107800040823     C                   ENDIF
107900051026     C*--------------------
108000051026     C* Controllo particolare x anagrafiche d download: se si sta cambiando il traduttore
108100051026     C* verifico prima che nn vi siano attualmente record d log a stato zero
108200051026     C*--------------------
108300051026     C     ERRL00        IFEQ      '0'
108400120109     C     §ISVVers      IFEQ      'D'
108500051026     C     VIDpgt        IFNE      virPGT
108600051026     C     VIDpgi        ORNE      virPGI
108700051026     C                   CALL      'TIS723R1'
108800051026     C                   PARM                    VIDKSC
108900051026     C                   PARM                    VIDTIP
109000120109     C                   PARM                    §ISVVers
109100051026     C                   PARM      *zeros        wRecZero          9 0
109200051026     C                   IF        wRecZero > *zeros
109300051026     C                   MOVEL     '1'           ERRL00
109400051026     C                   MOVEL     MSG(26)       DSMSMS
109500051026     C                   SETON                                        5455
109600051026     C                   ENDIF
109700051026     C                   ENDIF
109800051026     C                   ENDIF
109900051026     C                   ENDIF
110000060616     C*--------------------
110100060616     C* File Da tradurre / File Storico / Programma Post-Traduzione
110200060616     C*--------------------
110300060616     C     ERRL00        IFEQ      '0'
110400060616     C                   IF        (VIDfid='*' AND VIDfis='*' AND VIDpgi='*') OR
110500060616     C                             (VIDfid<>'*' AND VIDfis<>'*' AND VIDpgi<>'*')
110600060616     C                   ELSE
110700060616     C                   MOVEL     '1'           ERRL00
110800060616     C                   SETON                                        535556
110900060616     C                   MOVEL     MSG(27)       DSMSMS
111000060616     C                   ENDIF
111100060616     C                   ENDIF
111200081020     C*--------------------
111300081020     C* Mail x eventuale feed-back richiesto
111400081020     C*--------------------
111500081020     C     ERRL00        IFEQ      '0'
111600081020     C                   CLEAR                   DSEMAIL
111700081020     C                   EVAL      §EMLINDI = VIDEML
111800081020     C                   CALL      'XEMAIL'
111900081020     C                   PARM                    DSEMAIL
112000081020     C                   IF        §EMLERRO = '1'
112100081020     C                   MOVEL     '1'           ERRL00
112200081020     C                   SETON                                        64
112300081020     C                   MOVEL     §EMLMSGO      DSMSMS
112400081020     C                   ENDIF
112500081020     C                   ENDIF
112600001120     C*
112700001120     C                   ENDSR
112800000626     C*------------------------------------------------------------------------*
112900000626     C* Cntisv - CONTROLLA codice tipo legame
113000000626     C*------------------------------------------------------------------------*
113100001120     C     cntisv        begsr
113200001120     C                   clear                   TIBS02DS
113300001120     C*
113400101027     C     '?'           SCAN      Vidisv                                 28
113500001120     C*  Ricerca
113600001120     C                   eval      t02tla = 'L'
113700101027     C                   IF        *IN28 = *ON
113800001120     C                   movel     'R'           t02mod
113900001120     C                   else
114000001120     C                   movel     'C'           t02mod
114100001120     C                   movel(p)  vidisv        t02ke1
114200001120     C                   end
114300000626     C                   movel     'ISV'         t02cod
114400001120     C                   movel     knsif         t02sif
114500001120     C                   CALL      'TIBS02R'
114600001120     C                   parm                    KPJBA
114700001120     C                   parm                    TIBS02DS
114800001120     C
114900001120     C*
115000001120     C                   IF        T02err = *blanks
115100000626     C                   movel     T02ke1        Vidisv
115200000626     C                   movel     T02uni        vddisv
115300030603     C                   movel(P)  T02uni        DISV
115400001120     C                   else
115500001120     C                   movel     '1'           errl00
115600001120     C                   seton                                        70
115700001120     C                   MOVEL     MSG(18)       DSMSMS
115800001120     C                   ENDIF
115900001120     C
116000001120     C                   endsr
116100991027     C*------------------------------------------------------------------------*
116200991027     C* cnttip - Controlla esistenza tipo file su tivt00f
116300991027     C*------------------------------------------------------------------------*
116400001120     C     cnttip        begsr
116500001120     C                   clear                   TIBS02DS
116600000626     C*
116700101027     C     '?'           SCAN      vidtip                                 28
116800001120     C*  Ricerca
116900001120     C                   eval      t02tla = 'L'
117000101027     C                   IF        *IN28 = *ON
117100001120     C                   movel     'R'           t02mod
117200001120     C                   else
117300001120     C                   movel     'C'           t02mod
117400001120     C                   movel(p)  vidtip        t02ke1
117500001120     C                   end
117600991108     C                   movel     'ITF'         t02cod
117700001120     C                   movel     knsif         t02sif
117800991108     C                   CALL      'TIBS02R'
117900001120     C                   parm                    KPJBA
118000001120     C                   parm                    TIBS02DS
118100001120     C
118200001120     C*
118300001120     C                   IF        T02err = *blanks
118400991108     C                   movel     T02ke1        vidtip
118500020218     C                   movel     T02uni        DITF
118600020218     C                   movel     §ITFDESC      vddtip
118700021003     C* Sostituisco con il dato proveniente dalla tabella solo se nn già indicato
118800021003     C* qualcosa nelle regole specifiche x il cliente/tipo file.
118900021014     C                   if        vidfit = *blanks
119000021014     C                   movel     §ITFLIBD      vidfi2
119100021014     C                   endif
119200001120     C                   else
119300001120     C                   movel     '1'           errl00
119400991108     C                   movel     *blank        vddtip
119500111215     C* pulisco anche il cod. tabella associata
119600111215     C                   movel     *blank        vidtab
119700001120     C                   seton                                        66
119800991108     C                   MOVEL     msg(11)       DSMSMS
119900991108     C                   ENDIF
120000001120     C                   if        errl00 = '0'
120100120109     C     vidtip        chain     tivtf01l                           97
120200001120     C                   if        not *in97
120300001120     C                   movel     vtfpgt        vidpgtd
120400001120     C                   movel     vtfpgi        vidpgid
120500001120     C                   movel     vtffid        vidfidd
120600001120     C                   movel     vtffis        vidfisd
120700001120     C                   movel     vtffit        vidfitd
120800111214     C                   movel     vtffi1        vidtab
120900101013     C                   movel     vtfppi        DVTFPPI
121000101026     C                   if        DEPOP1 = 'F06' OR
121100101026     C                             DEPOP1 = 'O03'
121200101026     C                   select
121300101026     C                   when      vtfUD = 'U'
121400101026     C                   movel     D§VTFPPIMF    vidppiMC
121500101026     C                   when      vtfUD = 'D'
121600101026     C                   movel     D§VTFPPIMF    vidppiMF
121700101026     C                   endsl
121800101026     C                   movel     D§VTFPPIMS    vidppiMS
121900101026     C                   movel     D§VTFPPIMZ    vidppiMZ
122000101026     C                   endif
122100100308     C                   if        vidfi4d <> *blanks and vidfi4d <> '*'
122200001120     C                   eval      vidfi4d = %subst(vidfitd:1:5) + '00F'
122300100308     C                   endif
122400001120     C                   else
122500001120     C                   movel     *blank        vidpgtd
122600001120     C                   movel     *blank        vidpgid
122700001120     C                   movel     *blank        vidfidd
122800001120     C                   movel     *blank        vidfisd
122900001120     C                   movel     *blank        vidfitd
123000001120     C                   movel     *blank        vidfi4d
123100001120     C                   endif
123200001120     C                   endif
123300001120     C*
123400001120     C                   endsr
123500991027     C*------------------------------------------------------------------------*
123600120109     C* CntTip234 - Controlla esistenza altri tipi file su tivt00f
123700991027     C*------------------------------------------------------------------------*
123800120109     C     CntTip234     BEGSR
123900120119     C*
124000120119     C* se ho indicato un cliente devo indicare il tipo file
124100120119     C                   IF        Kscs(i) <>*blank and
124200120119     C                             Kscs(i) <>*zeros and
124300120119     C                             Tips(i) = *blank
124400120119     C                   SELECT
124500120119     C                   WHEN      i = 1
124600120119     C                   SETON                                        7549
124700120119     C                   WHEN      i = 2
124800120119     C                   SETON                                        7650
124900120119     C                   WHEN      i = 3
125000120119     C                   SETON                                        7751
125100120119     C                   ENDSL
125200120119     C                   MOVEL     '1'           ERRL00
125300120119     C                   MOVEL     MSG(34)       DSMSMS
125400120119     C                   LEAVESR
125500120119     C                   ENDIF
125600010913     C*
125700120119     C* il tipo file / cliente indicati non devono essere quelli in gestione
125800120119     C                   IF        Tips(i) = VIDTip and
125900120119     C                             Kscs(i) = VIDKsc
126000120111     C                   SELECT
126100120111     C                   WHEN      i = 1
126200120119     C                   SETON                                        7549
126300120111     C                   WHEN      i = 2
126400120119     C                   SETON                                        7650
126500120111     C                   WHEN      i = 3
126600120119     C                   SETON                                        7751
126700120111     C                   ENDSL
126800120111     C                   MOVEL     '1'           ERRL00
126900120111     C                   MOVEL     MSG(33)       DSMSMS
127000120111     C                   LEAVESR
127100120111     C                   ENDIF
127200120111     C*
127300120111     C* il tipo file indicato deve esistere ed essere abilitato su questa anagrafica per il cliente
127400120119     C* indicato
127500120111     C* (per abilitato basta che la data fine validità sia maggiore di oggi; se anche la data inizio
127600120111     C*  è maggiore di oggi non importa, sarà valido nel futuro)
127700120119     C                   EVAL      x_VIRKsc = Kscs(i)
127800120111     C                   EVAL      x_VIRTip = Tips(i)
127900120111     C                   EVAL      x_VIRDtF = %dec(%date() : *ISO)
128000120111     C     K03VIR06      SETLL     TIVIR06L
128100120111     C     K02VIR06      READE     TIVIR06L
128200120111     C                   if        %eof
128300120109     C                   SELECT
128400120109     C                   WHEN      i = 1
128500120109     C                   SETON                                        75
128600120109     C                   WHEN      i = 2
128700120109     C                   SETON                                        76
128800120109     C                   WHEN      i = 3
128900120109     C                   SETON                                        77
129000120109     C                   ENDSL
129100120109     C                   MOVEL     '1'           ERRL00
129200120109     C                   MOVEL     MSG(31)       DSMSMS
129300120119     C* se il tipo file / cliente per duplica esiste,
129400120119     C* questo non deve contenere nelle sue coppie per duplica
129500120119     C* il tipo file / cliente in gestione
129600120111     C                   ELSE
129700120119     C*
129800120120     C                   if        TIS723R2U_Ctrl_Chain_Dup(
129900120120     C                              VIDTip       :
130000120120     C                              VIDKsc       :
130100120120     C                              Tips(i)      :
130200120120     C                              Kscs(i)      ) = '1'
130300120120     C                   SELECT
130400120120     C                   WHEN      i = 1
130500120120     C                   SETON                                        7549
130600120120     C                   WHEN      i = 2
130700120120     C                   SETON                                        7650
130800120120     C                   WHEN      i = 3
130900120120     C                   SETON                                        7751
131000120120     C                   ENDSL
131100120120     C                   MOVEL     '1'           ERRL00
131200120120     C                   MOVEL     MSG(32)       DSMSMS
131300120120     C                   ENDIF
131400120120     C***
131500120120     C***                MOVEL     x_VIRPTH      DVIR01
131600120120     C***                IF        §VIR01Tip2 = VIDTip and
131700120120     C***                          §VIR01Ksc2 = VIDKsc     or
131800120120     C***                          §VIR01Tip3 = VIDTip and
131900120120     C***                          §VIR01Ksc3 = VIDKsc     or
132000120120     C***                          §VIR01Tip4 = VIDTip and
132100120120     C***                          §VIR01Ksc4 = VIDKsc
132200120120     C***                SELECT
132300120120     C***                WHEN      i = 1
132400120120     C***                SETON                                        7549
132500120120     C***                WHEN      i = 2
132600120120     C***                SETON                                        7650
132700120120     C***                WHEN      i = 3
132800120120     C***                SETON                                        7751
132900120120     C***                ENDSL
133000120120     C***                MOVEL     '1'           ERRL00
133100120120     C***                MOVEL     MSG(32)       DSMSMS
133200120120     C***                ENDIF
133300120120     C***
133400120111     C                   ENDIF
133500120109     C*
133600120109     C                   ENDSR
133700120109     C*------------------------------------------------------------------------*
133800120109     C* cntksc - Controlla esistenza codice
133900120109     C*------------------------------------------------------------------------*
134000120109     C     cntksc        begsr
134100120109     C*
134200010913     C     vidksc        chain     tianv01l
134300010913     C                   if        %found
134400010913     C                   movel     ANVDES        vddksc
134500010913     C                   else
134600001120     C                   move      vidksc        wcli              7 0
134700991210     C                   CLEAR                   BS69DS
134800991210     C                   CLEAR                   ACODS
134900991210     C                   MOVEL     KNSIF         I69SIF
135000991210     C                   Z-ADD     WCLI          I69KAC
135100991210     C                   CALL      'TIBS69R'
135200991210     C                   PARM                    BS69DS
135300991210     C                   PARM                    ACODS
135400991210     C                   PARM                    INDDS
135500991210     C                   PARM                    CLPDS
135600991210     C                   PARM                    CLSDS
135700991210     C     O69ERR        IFNE      '1'
135800991210     C                   MOVEL     ACORAG        vddksc
135900991210     C                   ELSE
136000991210     C                   MOVEL     *ALL'*'       vddksc
136100010913     C                   MOVE      '1'           errl00
136200010913     C                   MOVEL     msg(11)       dsmsms
136300010913     C                   ENDIF
136400000727     C                   ENDIF
136500010913     C*
136600010913     C                   ENDSR
136700991027     C*------------------------------------------------------------------------*
136800040901     C* cntpoi - Controlla codice filiale
136900991027     C*------------------------------------------------------------------------*
137000001120     C     cntpoi        begsr
137100001120     C*
137200010529     C                   if        vidpoi <> '046' and
137300020809     C                             vidpoi <> '999' and
137400020902     C                             vidpoi <> '500' and
137500020902     C                             vidpoi <> '789'
137600001120     C*
137700001120     C                   move      *blank        vddpoi
137800001120     C     '?'           scan      vidpoi                                 97
137900001120     C                   if        *in97
138000001120     C                   movel     *blank        cod1
138100001120     C                   movel     *blank        des1
138200001120     C                   movel     *blank        pra1
138300001120     C                   call      'TNSD24R'
138400001120     C                   parm                    cod1              3
138500001120     C                   parm                    tip1              1
138600001120     C                   parm                    des1             25
138700001120     C                   parm                    pra1              3
138800001120     C                   movel     cod1          vidpoi
138900001120     C                   endif
139000001120     C                   move      vidpoi        cod1n             3 0
139100001120     C     cod1n         chain     azorg01l                           98
139200001120     C                   if        not *in98 and orgfag <>'V'
139300001120     C                   movel     orgdes        vddpoi
139400001120     C                   else
139500001120     C                   seton                                        69
139600001120     C                   move      '1'           errl00
139700001120     C                   movel     msg(13)       dsmsms
139800001120     C                   endif
139900001120     C*
140000001120     C                   else
140100001120     C                   movel     'SEDE BOLOGNA'vddpoi
140200001120     C                   endif
140300001120     C*
140400001120     C                   endsr
140500991210     C*------------------------------------------------------------------------*
140600991210     C* FINSR - OPERAZIONI FINALI
140700991210     C*------------------------------------------------------------------------*
140800991210     C     FINSR         BEGSR
140900991210     C*
141000991210     C* CHIUDE I FILE DEI PGM CHIAMATI
141100991210     C                   CLEAR                   bs69ds
141200991210     C                   CLEAR                   acods
141300991210     C                   CLEAR                   indds
141400991210     C                   CLEAR                   clpds
141500991210     C                   CLEAR                   clsds
141600991210     C                   MOVEL     'C'           i69tla
141700991210     C                   MOVEL     knsif         i69sif
141800991210     C                   CALL      'TIBS69R'
141900991210     C                   PARM                    bs69ds
142000991210     C                   PARM                    acods
142100991210     C                   PARM                    indds
142200991210     C                   PARM                    clpds
142300991210     C                   PARM                    clsds
142400991210     C*
142500991210     C                   ENDSR
142600991026     C*------------------------------------------------------------------------*
142700991026     C* CHKDA8 - CONTROLLA LA DATA (8)
142800991026     C*------------------------------------------------------------------------*
142900991026     C     CHKDA8        BEGSR
143000991026     C*
143100991026     C     *LIKE         DEFINE    VIDDti        WDA8
143200991026     C     *LIKE         DEFINE    VIDDti        DA8W
143300991026     C*
143400991026     C                   Z-ADD     WDA8          G08DAT
143500991026     C                   Z-ADD     *ZEROS        G08INV
143600991026     C                   MOVEL     '0'           G08ERR
143700991026     C                   CALL      'XSRDA8'
143800991026     C                   PARM                    WLBDA8
143900991026     C                   Z-ADD     G08DAT        WDA8
144000991026     C                   Z-ADD     G08INV        DA8W
144100991026IF  1C     G08ERR        IFEQ      '1'
144200991026     C                   MOVEL     '1'           ERRL00
144300991026E   1C                   ENDIF
144400991026     C*
144500991026     C                   ENDSR
144600970526     C*------------------------------------------------------------------------*
144700970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
144800970526     C*------------------------------------------------------------------------*
144900970526     C     IREVID        BEGSR
145000080604     C*
145100080604     C* REPERISCO INFO DA ESTENSIONE ANAGRAFICA CLIENTI/TIPI FILE => TRC 01
145200080604     C                   IF        §vire01INV = 'S'
145300080604     C                   EVAL      VIDEML = §vire01EML
145400080604     C                   ENDIF
145500970526     C* CAMPI
145600991026     C                   MOVEL     virtip        VIDtip
145700020218     C                   MOVE      virksc        vidksc
145800020218     C                   Z-ADD     virseq        vidseq
145900060308     C                   IF        DEPOP1 = 'O03'
146000060308     C                   MOVEL     *blanks       vidpoi
146100060308     C                   ELSE
146200060308     C                   MOVE      virpoi        vidpoi
146300060308     C                   ENDIF
146400991026     C                   MOVEL     virdes        VIDdes
146500991026     C                   MOVEL     virpgt        VIDpgt
146600991026     C                   MOVEL     virppt        VIDppt
146700991026     C                   MOVEL     virpgi        VIDpgi
146800991026     C                   MOVEL     virppi        VIDppi
146900991026     C                   MOVEL     virfid        VIDfid
147000991026     C                   MOVEL     virfis        VIDfis
147100991026     C                   MOVEL     virfit        VIDfit
147200000710     C                   MOVEL     virfi3        VIDfi3
147300000710     C                   MOVEL     virfi4        VIDfi4
147400010123     C                   MOVEL     virfi2        VIDfi2
147500030526     C                   MOVEL     virpth        DVIR01
147600030526     C                   MOVEL     §VIR01TM      VIDtmbd
147700120109     C                   MOVEL     §VIR01Tip2    VIDTip2
147800120109     C                   MOVEL     §VIR01Tip3    VIDTip3
147900120109     C                   MOVEL     §VIR01Tip4    VIDTip4
148000120119     C                   MOVEL     §VIR01Ksc2    VIDKsc2
148100120119     C                   MOVEL     §VIR01Ksc3    VIDKsc3
148200120119     C                   MOVEL     §VIR01Ksc4    VIDKsc4
148300000710  ***C*                  MOVEL     virctl        VIDctl
148400991026     C                   Z-ADD     virdti        G08INV
148500991026     C                   Z-ADD     *ZEROS        G08DAT
148600991026     C                   MOVEL     '3'           G08ERR
148700991026     C                   CALL      'XSRDA8'
148800991026     C                   PARM                    WLBDA8
148900991026     C                   Z-ADD     G08DAT        viddti
149000970527     C*
149100991026     C                   Z-ADD     virdtf        G08INV
149200991026     C                   Z-ADD     *ZEROS        G08DAT
149300991026     C                   MOVEL     '3'           G08ERR
149400991026     C                   CALL      'XSRDA8'
149500991026     C                   PARM                    WLBDA8
149600991026     C                   Z-ADD     G08DAT        viddtf
149700020218     C                   MOVEL     virfi1        vidisv
149800051122     C*
149900051122     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
150000051122     C                   IF        DEPOP1 = 'F06' OR
150100051122     C                             DEPOP1 = 'O03' OR
150200051122     C                             DEPOP1 = 'O07'
150300051122     C                   EVAL      VIDDTI = DATCORINV
150400060726     C                   EVAL      VIDDTF = 31122039
150500051122     C                   ENDIF
150600020218     C*
150700970526     C                   ENDSR
150800970526     C*------------------------------------------------------------------------*
150900970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
151000970526     C*------------------------------------------------------------------------*
151100970526     C     AZZVID        BEGSR
151200970526     C* CAMPI
151300991026     C                   MOVEL     *blank        VIDksc
151400000710  ***C*                  MOVEL     *blank        VIDpt1
151500000710  ***C*                  MOVEL     *blank        VIDpt2
151600010123     C                   z-add     0             vidseq
151700010123     C                   move      *zeros        vidpoi
151800060726     C                   EVAL      VIDDTI = DATCORINV
151900060726     C                   EVAL      VIDDTF = 31122039
152000991025     C                   MOVEL     *blank        VIDtip
152100991025     C                   MOVEL     *blank        VIDdes
152200991025     C                   MOVEL     *blank        VIDpgt
152300991025     C                   MOVEL     *blank        VIDppt
152400991025     C                   MOVEL     *blank        VIDpgi
152500991025     C                   MOVEL     *blank        VIDppi
152600991025     C                   MOVEL     *blank        VIDfid
152700991025     C                   MOVEL     *blank        VIDfis
152800991025     C                   MOVEL     *blank        VIDfit
152900000710     C                   MOVEL     *blank        VIDfi3
153000000710     C                   MOVEL     *blank        VIDfi4
153100010123     C                   MOVEL     *blank        VIDfi2
153200000710  ***C*                  MOVEL     *blank        VIDctl
153300080603     C                   MOVEL     *blank        VIDeml
153400970526     C*
153500970526     C                   ENDSR
153600970526     C*------------------------------------------------------------------------*
153700991026     C* IMPvir - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
153800970526     C*------------------------------------------------------------------------*
153900991026     C     IMPvir        BEGSR
154000970526     C*
154100991027     C                   move      VIDpoi        virpoi
154200991026     C                   movel     VIDdes        virdes
154300991026     C                   movel     VIDpgt        virpgt
154400991026     C                   movel     VIDppt        virppt
154500991026     C                   movel     VIDpgi        virpgi
154600991026     C                   movel     VIDppi        virppi
154700991026     C                   movel     VIDfid        virfid
154800991026     C                   movel     VIDfis        virfis
154900991026     C                   movel     VIDfit        virfit
155000000710     C                   movel     VIDfi3        virfi3
155100000710     C                   movel     VIDfi4        virfi4
155200010123     C                   movel     VIDfi2        virfi2
155300030526     C                   MOVEL     VIDtmbd       §VIR01TM
155400120109     C                   EVAL      §VIR01Tip2 =  VIDTip2
155500120109     C                   EVAL      §VIR01Tip3 =  VIDTip3
155600120109     C                   EVAL      §VIR01Tip4 =  VIDTip4
155700120119     C                   IF        VIDKsc2 = *zeros
155800120119     C                   EVAL      VIDKsc2 = *blank
155900120119     C                   ENDIF
156000120119     C                   IF        VIDKsc3 = *zeros
156100120119     C                   EVAL      VIDKsc3 = *blank
156200120119     C                   ENDIF
156300120119     C                   IF        VIDKsc4 = *zeros
156400120119     C                   EVAL      VIDKsc4 = *blank
156500120119     C                   ENDIF
156600120119     C                   EVAL      §VIR01Ksc2 =  VIDKsc2
156700120119     C                   EVAL      §VIR01Ksc3 =  VIDKsc3
156800120119     C                   EVAL      §VIR01Ksc4 =  VIDKsc4
156900030526     C                   MOVEL     DVIR01        virpth
157000000710  ***C*                  movel     VIDctl        virctl
157100991026     C                   Z-ADD     viddtf        G08dat
157200991026     C                   Z-ADD     *ZEROS        G08inv
157300991026     C                   MOVEL     '0'           G08ERR
157400991026     C                   CALL      'XSRDA8'
157500991026     C                   PARM                    WLBDA8
157600991026     C                   Z-ADD     G08inv        virdtf
157700000628     C                   movel     VIDisv        virfi1
157800970526     C*
157900970526     C                   ENDSR
158000970526     C*------------------------------------------------------------------------*
158100970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
158200970526     C*------------------------------------------------------------------------*
158300970526     C     IMPFUN        BEGSR
158400101013     C*
158500101026     C                   SETOFF                                       44
158600101026     C                   SETOFF                                       30
158700101026     C                   SETOFF                                       31
158800101026     C                   SETOFF                                       32
158900101026     C                   SETOFF                                       33
159000970526     C*
159100970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
159200970526IF  1C     DEPOP1        IFEQ      'O05'
159300970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
159400970526     C                   MOVEL     'N'           FUDATT
159500970526     C                   MOVEL     FUDRIG        FUD(6)
159600970526E   1C                   ENDIF
159700141111     C*
159800141111     C* TASTO FUNZIONE F02 ATTIVO SOLO PER OPZIONI "DISPOSITIVE"
159900141111     C                   IF        DEPOP1 = 'O02' OR
160000141111     C                             DEPOP1 = 'O03' OR
160100141111     C                             DEPOP1 = 'F06'
160200141111     C                   MOVEL     FUD(2)        FUDRIG                         *PARAMETRI EXT.
160300141111     C                   MOVEL     'S'           FUDATT
160400141111     C                   MOVEL     FUDRIG        FUD(2)
160500141111     C                   ENDIF
160600970526     C*
160700970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
160800970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
160900970526     C                   EXSR      FUNGES
161000970526     C*
161100970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
161200970526     C*---
161300970526     C* 90 -> PROTEZIONE CHIAVE
161400970526     C* 91 -> PROTEZIONE TOTALE
161500970526     C*---
161600970528IF  1C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
161700970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
161800970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
161900970526     C                   SETOFF                                       91
162000970526E   2C                   ENDIF
162100101013IF  2C     DEPOP1        IFEQ      'O03'                                        *INSERIMENTO
162200101013E   2C                   ENDIF
162300970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
162400991014IF  2C     DEPOP1        oreq      'O01'                                        *MODIFICA
162500970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
162600970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
162700970526E   2C                   ENDIF
162800970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
162900970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
163000970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
163100970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
163200970526     C                   SETON                                        91
163300970526E   2C                   ENDIF
163400970526X   1C                   ELSE                                                   *I N T E R R O G A
163500970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
163600970526     C                   SETON                                        91
163700970526E   1C                   ENDIF
163800970526     C*
163900970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
164000970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
164100991025     C                   MOVEL     S20tip        KCEtip
164200991026     C                   MOVEL     S20ksc        KCEksc
164300000420     C     KEYvi2        CHAIN     tivir03L                           9998
164400080603IF  2C     *IN99         IFEQ      *ON
164500970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
164600970526     C                   MOVEL     MSG(6)        DSMSMS
164700970526X   2C                   ELSE
164800080603     C*
164900080603     C* Aggancio anche il record d estensione regole clienti/tipi-file => TRC 01
165000080603     C                   EVAL      KCEdti  = virDTI
165100080603     C                   EVAL      vireTRC = '01'
165200080603     C     KEYvire1_C    CHAIN     tivire1l                           9998
165300080603     C                   IF        %found(tivire1l)
165400080603     C                   EVAL      DVIRE01 = vireFLO
165500080603     C                   ENDIF
165600150114     C*
165700150114     C* Aggancio anche il record d estensione parametri clienti/tipi-file => TRC PE
165800150114     C                   SETOFF                                       71
165900150114     C                   EVAL      KCEdti  = virDTI
166000150114     C                   EVAL      vireTRC = 'PE'
166100150114     C     KEYvire1_C    CHAIN     tivire1l
166200150114     C                   IF        %found(tivire1l)
166300150114     C                   SETON                                        71
166400150114     C                   ENDIF
166500080603     C*
166600080603     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
166700980128IF  3C     DEPOP1        IFEQ      'O05'
166800991014IF  3C     DEPOP1        oreq      'O02'
166900101026     C*
167000101026     C* Se trattasi d tipo servizio "idoneo" reperisco anche i dati utente lato "BARTVAS"
167100101026     C     virFI1        lookup    isvftpeml                              80
167200101026     C                   IF        *IN80
167300111215     C                             and AS400_ok = *on
167400101026      /FREE
167500101026
167600130215       // (questa attività costa risorse, per cui si permette solo in gestione oppure anche in
167700130215       //  visualizzazione se l'utente è di sede;
167800130215       //  testo solo se l'utente è di sede, perché quelli di filiale non possono andare in gestione
167900130215       //  quindi non potrebbero comunque)
168000130215       if DUtLPO = 'S';
168100101026         if UBBVSQL01_RtvCliTipFile(
168200101026                         pInConn
168300101026                       : virKSC
168400101026                       : virTIP
168500101026                       : pOutNumRec
168600101026                       : pOutCliTipTras
168700101026                       : pOutCliMskChk
168800101026                       : pOutCliMskFil
168900101026                       : pOutCliMskSem
169000101026                       : pOutCliPerc
169100101026                       : pOutCliEml1
169200101026                       : pOutCliEml2
169300101026                       : pOutCliEml3
169400101026                       : pOutCliEml4
169500101026                       : pOutCliEml5
169600101026                       : pOutCliSogg
169700101026                       : pOutCliTesto
169800101026                       : pOutCliFtpAddr
169900101026                       : pOutCliFtpDir
170000101026                       : pOutCliFtpUsr
170100101026                       : pOutCliFtpPwd
170200101026                       : pOutCliFtpFAS4
170300101026                       : pOutCliPercOld
170400101026                       : pOutCliTentat
170500101026                       : pOutCliRitardo
170600101026                       : pOutCliLibAS4
170700101026                       : pOutCliFilAS4
170800101026                       : pOutCliMbrAS4
170900101026                       : pOutCliProc
171000101026                       : pOutCliSched
171100101026                       : pOutCliAccorp
171200101026                       : pOutCliMskZip
171300101026                       : pOutCliDataStr
171400101026                       : pOutCliDataEnd
171500101026                       : pOutCliCodComp
171600101026                       : pOutCliFtpCmd1
171700101026                       : pOutCliEmlErr
171800101026                       : pOutCliHtUrlTo
171900101026                       : pOutCliHtUsrTo
172000101026                       : pOutCliHtPwdTo
172100101026                       : pOutCliHtConTy
172200101026                       : pOutCliHtSndEx
172300101026                       : pOutCliHtSndPr
172400101026                       : pOutCliHtUsPxy
172500101026                       : pOutCliUsrIns
172600101026                       : pOutCliUsrUpd
172700101026                       : pOutCliDatUpd
172800101026                       : pOutCliNote
172900101026                       : pOutCliFtpPrt
173000101026                       : pOutCliFtpTy) = 0;
173100101026              if pOutNumRec = 1;
173200101026                 VIDPPIMC = pOutCliMskChk;
173300101026                 if %len(%trim(pOutCliMskChk)) > %size(VIDPPIMC);
173400101026                    *in30 = *on;
173500101026                 endif;
173600101026                 VIDPPIMF = pOutCliMskFil;
173700101026                 if %len(%trim(pOutCliMskFil)) > %size(VIDPPIMF);
173800101026                    *in31 = *on;
173900101026                 endif;
174000101026                 VIDPPIMS = pOutCliMskSem;
174100101026                 if %len(%trim(pOutCliMskSem)) > %size(VIDPPIMS);
174200101026                    *in32 = *on;
174300101026                 endif;
174400101026                 VIDPPIMZ = pOutCliMskZip;
174500101026                 if %len(%trim(pOutCliMskZip)) > %size(VIDPPIMZ);
174600101026                    *in33 = *on;
174700101026                 endif;
174800101026              endif;
174900101026         endif;
175000130214       endif;
175100101026
175200101026      /END-FREE
175300101026     C
175400101026     C                   ELSE
175500101026     C                   SETON                                        44
175600101026     C                   ENDIF
175700101026     C*
175800980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
175900980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
176000980128E   3C                   ENDIF
176100970526E   2C                   ENDIF
176200970526X   1C                   ELSE                                                   *INSERIMENTO
176300970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
176400970526E   1C                   ENDIF
176500970526     C*
176600970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
176700970526IF  1C     DEPOP1        IFEQ      'O02'
176800970526     C                   MOVEL     DEC(1)        VIDFUN
176900970526E   1C                   ENDIF
177000991014IF  1C     DEPOP1        IFEQ      'O03'
177100991014     C                   MOVEL     DEC(6)        VIDFUN
177200991014E   1C                   ENDIF
177300970526IF  1C     DEPOP1        IFEQ      'O04'
177400970526     C                   MOVEL     DEC(2)        VIDFUN
177500970526E   1C                   ENDIF
177600970526IF  1C     DEPOP1        IFEQ      'O05'
177700970526     C                   MOVEL     DEC(3)        VIDFUN
177800970526E   1C                   ENDIF
177900970526IF  1C     DEPOP1        IFEQ      'O07'
178000970526     C                   MOVEL     DEC(4)        VIDFUN
178100970526E   1C                   ENDIF
178200970526IF  1C     DEPOP1        IFEQ      'F06'
178300970526     C                   MOVEL     DEC(5)        VIDFUN
178400970526E   1C                   ENDIF
178500970526     C*
178600970526     C                   ENDSR
178700101026
178800101026
178900101026
179000101026     C*--------------------------------------------------------------------------------------------*
179100101026     C* CARTAB - carica le tabelle occorrenti
179200101026     C*--------------------------------------------------------------------------------------------*
179300101026     C     CARTAB        BEGSR
179400101026     C*
179500101026     C* Tipi servizi soggetti a controllo unificazioni
179600101026     C                   z-add     *zeros        ww                3 0
179700101026     C                   eval      tbecod = 'ISV'
179800101026     C     tbecod        setll     tntbe01l
179900101026     C     tbecod        reade     tntbe01l
180000101026     C                   dow       not %eof(tntbe01l)
180100101026     C                   movel     tbeuni        disv
180200101026     C                   if        §isvproc = 'F'
180300101026     C                   add       1             ww
180400101026     C                   eval      isvftpeml(ww) = tbeke1
180500101026     C                   endif
180600101026     C     tbecod        reade     tntbe01l
180700101026     C                   enddo
180800101026     C*
180900101026     C                   ENDSR
181000101026
181100101026     c*------------------------------------------------------------------------*
181200101026     c* opnCONN - Apertura connessione
181300101026     c*------------------------------------------------------------------------*
181400101026     c     OPNCONN       BEGSR
181500101026      /FREE
181600101026
181700101026        if not *in40;
181800101026           if UBBVSQL01_Connect('BARTVAS' : pOutConn) = 0;
181900101026              pInConn = pOutConn;
182000101026           else;
182100101026              *in40 = *on;
182200101026              DSMSMS = MSG(28);
182300101026           endif;
182400101026        endif;
182500101026
182600101026      /END-FREE
182700101026      *
182800101026     c                   ENDSR
182900101026     c*------------------------------------------------------------------------*
183000101026
183100101026
183200101026
183300101026     c*------------------------------------------------------------------------*
183400101026     c* clsCONN - Chiusura connessione
183500101026     c*------------------------------------------------------------------------*
183600101026     c     CLSCONN       BEGSR
183700101026      /FREE
183800101026
183900101026           if UBBVSQL01_Disconnect(pInConn) = 0;
184000101026           else;
184100101026              *in40 = *on;
184200101026           endif;
184300101026
184400101026      /END-FREE
184500101026      *
184600101026     c                   ENDSR
184700101026     c*------------------------------------------------------------------------*
184800101026
184900101026
185000101026
185100970526     C*------------------------------------------------------------------------*
185200970526     C* *INZSR - OPERAZIONI INIZIALI
185300970526     C*------------------------------------------------------------------------*
185400970526     C     *INZSR        BEGSR
185500970526     C*---
185600970526     C* RICEVIMENTO PARAMETRI
185700970526     C*---
185800970526     C     *ENTRY        PLIST
185900970612     C                   PARM                    KPJBA
186000991028     C                   movel     kpjbu         tis720
186100000420     C*
186200000420     C* Inverto subito la data presa in input
186300000420     C                   Z-ADD     s20dti        wda8
186400000420     C                   Z-ADD     *ZEROS        da8w
186500000420     C                   EXSR      CHKDA8
186600000420     C                   Z-ADD     DA8W          KCEdti
186700130215     C*
186800130215     C     *DTAARA       DEFINE    §azute        azuteds
186900130215     C     *DTAARA       DEFINE    §datiute      ddatiute
187000130215     C                   IN(E)     *DTAARA
187100130215if  1C                   IF        %Error
187200130215     c                   EVAL      I34Tla = 'L'
187300130215     C                   CALL      'TIBS34R'
187400130215     C                   PARM                    Tibs34Ds
187500130215     C                   IN        *DTAARA
187600130215e   1C                   ENDIF
187700970526     C*
187800970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
187900970526     C     XMSGPA        PLIST
188000970526     C                   PARM                    DSMSMS
188100970526     C                   PARM                    DSMSPG
188200970526     C                   PARM                    DSMSKE
188300970526     C                   PARM                    DSMSER
188400991108     C*---
188500970526     C*
188600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
188700970526     C                   CALL      'XKEY'
188800970526     C                   PARM                    DSKEY
188900101026     C*
189000101026     C* CARICA LE TABELLE OCCORRENTI
189100101027     C                   EXSR      CARTAB
189200111215     C*
189300111215     C* Reperisco il sistema AS/400 corrente
189400111215     C                   EVAL      AS400_ok = *off
189500111215     C                   callp     UBRTVNETA_Rtv(currSysNeta)
189600111215     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
189700111215     C                   EVAL      AS400_ok = *on
189800111215     C                   ENDIF
189900101026     C*
190000130214     C* Apro la connessione al DB "BARTVAS" (solo se no solo visualizzazione)
190100130215     C* (questa attività costa risorse, per cui si permette solo in gestione oppure anche in
190200130215     C*  visualizzazione se l'utente è di sede;
190300130215     C*  testo solo se l'utente è di sede, perché quelli di filiale non possono andare in gestione,
190400130215     C*  quindi non potrebbero comunque)
190500130215     C                   IF        DUtLPO = 'S'
190600111215     C                   IF        AS400_ok = *on
190700111215     C                   exsr      opnCONN
190800111215     C                   ENDIF
190900130214     C                   ENDIF
191000970526     C*---
191100970526     C* VARIABILI NON RIFERITE AL DATA BASE
191200970526     C*---
191300970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
191400991025     C     *LIKE         DEFINE    S20OP0        DEPOP0
191500991025     C     *LIKE         DEFINE    S20OP1        DEPOP1
191600991025     C     *LIKE         DEFINE    S20OP0        WOP0
191700991025     C     *LIKE         DEFINE    S20OP1        WOP1
191800970526     C*---
191900970526     C* VARIABILI DI CONTROLLO
192000970526     C*---
192100970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
192200970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
192300970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
192400970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
192500970526     C*
192600970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
192700991026     C                   MOVEL     'TIS723R'     DSMSPG
192800991026     C                   MOVEL     'TIS723R'     MSGQUE
192900970526     C                   MOVEL     *BLANKS       MSGKEY
193000970526     C                   MOVEL     *BLANKS       DSMSMS
193100970526     C*---
193200970526     C* VARIABILI RIFERITE AL DATA BASE
193300970526     C*---
193400991026     C     *LIKE         DEFINE    virtip        KCEtip
193500991026     C     *LIKE         DEFINE    virksc        KCEksc
193600000420     C     *LIKE         DEFINE    virdti        KCEdti
193700970526     C*---
193800970526     C* CHIAVI DI LETTURA
193900970526     C*---
194000000420     C* LETTURA tivir03L - INCOMPLETA
194100991026     C     KEYvir        KLIST
194200080603     C                   KFLD                    KCEtip
194300080603     C                   KFLD                    KCEksc
194400080603     C* LETTURA tivir03L - COMPLETA
194500080603     C     KEYvi2        KLIST
194600080603     C                   KFLD                    KCEtip
194700080603     C                   KFLD                    KCEksc
194800080603     C                   KFLD                    KCEdti
194900080603     C* LETTURA tivire1L - COMPLETA
195000080603     C     KEYvire1_C    KLIST
195100080603     C                   KFLD                    KCEksc
195200080603     C                   KFLD                    KCEtip
195300080603     C                   KFLD                    KCEdti
195400080603     C                   KFLD                    vireTRC
195500080603     C* LETTURA tntbe01L - PARZIALE
195600040816     C     KEYtbe1_P     KLIST
195700040816     C                   KFLD                    TBEcod                         *CODICE TABELLA
195800120111     C* LETTURA tivir06L - COMPLETA
195900120111     C     K03VIR06      KLIST
196000120111     C                   KFLD                    x_VIRKsc
196100120111     C                   KFLD                    x_VIRTip
196200120111     C                   KFLD                    x_VIRDtF
196300120111     C* LETTURA tivir06L - PARZIALE
196400120111     C     K02VIR06      KLIST
196500120111     C                   KFLD                    x_VIRKsc
196600120111     C                   KFLD                    x_VIRTip
196700970528     C*
196800970528     C* CALCOLA LA DATA CORRENTE
196900970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
197000970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
197100970528     C                   Z-ADD     WN8           G08DAT
197200051122     C                   Z-ADD     WN8           DATCORINV         8 0
197300970528     C                   Z-ADD     *ZEROS        G08INV
197400970528     C                   MOVEL     '0'           G08ERR
197500970528     C                   CALL      'XSRDA8'
197600970528     C                   PARM                    WLBDA8
197700970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
197800970526     C*
197900970526     C* DEPOSITA LE OPZIONI DI ENTRATA
198000991025     C                   MOVEL     S20OP0        DEPOP0                         *LIVELLO PROCEDURA
198100991025     C                   MOVEL     S20OP1        DEPOP1                         *LIVELLO PROGRAMMA
198200051122     C*
198300051122     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
198400051122     C                   IF        DEPOP1 = 'F06' OR
198500051122     C                             DEPOP1 = 'O03' OR
198600051122     C                             DEPOP1 = 'O07'
198700051122     C                   EVAL      VIDDTI = DATCORINV
198800060726     C                   EVAL      VIDDTF = 31122039
198900051122     C                   ENDIF
199000000629     C*
199100000629     C* EFFETTUO CONSIDERAZIONI SULLE OPZIONI DI ENTRATA
199200000629     C                   IF        DEPOP1 = 'F06' OR
199300000629     C                             DEPOP1 = 'O03'
199400000629     C                   SETOFF                                       92
199500000629     C                   ELSE
199600000629     C                   SETON                                        92
199700000629     C                   ENDIF
199800970526     C*
199900970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
200000970526     C                   EXSR      IMPFUN
200100970526     C*
200200970526     C                   ENDSR
200300970526     C*------------------------------------------------------------------------*
200400970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
200500970526     C*------------------------------------------------------------------------*
200600970526     C     FUNGES        BEGSR
200700970526     C*
200800970526     C* PULISCO CAMPI DI LAVORO
200900970526     C                   Z-ADD     0             W                 5 0
201000970526     C*
201100970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
201200970526     C* E CARICO TABELLA FUNZIONI ATTIVE
201300970526     C     1             DO        24            I
201400970526     C                   MOVEL     FUD(I)        FUDRIG
201500970526     C*
201600970526     C* PULIZIA FUNZIONI PRECEDENTI
201700970526     C                   MOVEL     *BLANKS       FUV(I)
201800970526     C*
201900970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
202000970526     C* (SALTA F24)
202100970526     C     FUDATT        IFEQ      'S'
202200970526     C     FUDFUN        ANDNE     '24'
202300970526     C                   ADD       1             W
202400970526     C                   MOVEL     FUDDES        FUV(W)
202500970526     C                   END
202600970526     C                   END                                                    DO WHILE
202700970526     C*
202800970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
202900970526     C* ALTRIMENTI DISATTIVO F24
203000970526     C     1             DO        24            I
203100970526     C                   MOVEL     FUD(I)        FUDRIG
203200970526     C*
203300970526     C     FUDFUN        IFEQ      '24'
203400970526     C     W             IFGT      6
203500970526     C                   MOVEL     'S'           FUDATT
203600970526     C                   ELSE
203700970526     C                   MOVEL     *BLANKS       FUDATT
203800970526     C                   END                                                    W GT 8
203900970526     C                   MOVEL     FUDRIG        FUD(I)
204000970526     C                   END
204100970526     C                   END                                                    DO WHILE
204200970526     C*
204300970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
204400970526     C     W             DIV       6             I
204500970526     C                   MVR                     Z
204600970526     C*
204700970526     C     Z             IFNE      0
204800970526     C     I             ADD       1             Z
204900970526     C                   ELSE
205000970526     C                   Z-ADD     I             Z                 5 0
205100970526     C                   END
205200970526     C*
205300970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
205400970526     C                   ADD       1             FUNGRU
205500970526     C     FUNGRU        IFGT      Z
205600970526     C     FUNGRU        ORLT      1
205700970526     C                   Z-ADD     1             FUNGRU
205800970526     C                   END
205900970526     C*
206000970526     C* PRIMO GRUPPO DI 6 FUNZIONI
206100970526     C     FUNGRU        IFEQ      1
206200970526     C                   MOVEL     FUV(1)        FUN001
206300970526     C                   MOVEL     FUV(2)        FUN002
206400970526     C                   MOVEL     FUV(3)        FUN003
206500970526     C                   MOVEL     FUV(4)        FUN004
206600970526     C                   MOVEL     FUV(5)        FUN005
206700970526     C                   MOVEL     FUV(6)        FUN006
206800970526     C                   END
206900970526     C*
207000970526     C* SECONDO GRUPPO DI 6 FUNZIONI
207100970526     C     FUNGRU        IFEQ      2
207200970526     C                   MOVEL     FUV(7)        FUN001
207300970526     C                   MOVEL     FUV(8)        FUN002
207400970526     C                   MOVEL     FUV(9)        FUN003
207500970526     C                   MOVEL     FUV(10)       FUN004
207600970526     C                   MOVEL     FUV(11)       FUN005
207700970526     C                   MOVEL     FUV(12)       FUN006
207800970526     C                   END
207900970526     C*
208000970526     C* TERZO GRUPPO DI 6 FUNZIONI
208100970526     C     FUNGRU        IFEQ      3
208200970526     C                   MOVEL     FUV(13)       FUN001
208300970526     C                   MOVEL     FUV(14)       FUN002
208400970526     C                   MOVEL     FUV(15)       FUN003
208500970526     C                   MOVEL     FUV(16)       FUN004
208600970526     C                   MOVEL     FUV(17)       FUN005
208700970526     C                   MOVEL     FUV(18)       FUN006
208800970526     C                   END
208900970526     C*
209000970526     C* QUARTO GRUPPO DI 6 FUNZIONI
209100970526     C     FUNGRU        IFEQ      4
209200970526     C                   MOVEL     FUV(19)       FUN001
209300970526     C                   MOVEL     FUV(20)       FUN002
209400970526     C                   MOVEL     FUV(21)       FUN003
209500970526     C                   MOVEL     FUV(22)       FUN004
209600970526     C                   MOVEL     *BLANKS       FUN005
209700970526     C                   MOVEL     *BLANKS       FUN006
209800970526     C                   END
209900970526     C*
210000970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
210100970526     C                   MOVEL     *BLANKS       FUN007
210200970526     C                   MOVEL     *BLANKS       FUN008
210300970526     C*
210400970526     C     1             DO        24            I
210500970526     C                   MOVEL     FUD(I)        FUDRIG
210600970526     C     FUDFUN        IFEQ      '24'
210700970526     C     FUDATT        ANDEQ     'S'
210800970526     C                   MOVEL     FUDDES        FUN008
210900970526     C                   END
211000970526     C                   END                                                    DO
211100970526     C*
211200970526     C                   ENDSR
211300970526     C*------------------------------------------------------------------------*
211400970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
211500970526     C*------------------------------------------------------------------------*
211600970526     C     FUNCON        BEGSR
211700970526     C*
211800970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
211900970526     C                   EXSR      FUNVAR
212000970526     C*
212100970526     C* PULISCO CAMPI DI LAVORO
212200970526     C                   Z-ADD     0             I                 5 0
212300970526     C                   Z-ADD     0             W                 5 0
212400970526     C*
212500970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
212600970526     C     FUNKEY        IFNE      *BLANKS
212700970526     C     1             DO        24            I
212800970526     C                   MOVEL     FUD(I)        FUDRIG
212900970526     C*
213000970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
213100970526     C     FUNKEY        IFEQ      FUDFUN
213200970526     C     FUDATT        ANDEQ     'S'
213300970526     C                   Z-ADD     I             W
213400970526     C                   END
213500970526     C                   END                                                    DO WHILE
213600970526     C*
213700970526     C* FUNZIONE NON ATTIVA (ERRORE)
213800970526     C     W             IFEQ      0
213900970526     C*
214000970526     C* EMISSIONE MESSAGGIO DI ERRORE
214100970526     C                   MOVEL     MSG(1)        DSMSMS
214200970526     C*
214300970526     C                   END                                                    W=0
214400970526     C                   END                                                    FUNKEY NE *BLANKS
214500970526     C*
214600970526     C* FUNZIONE ATTIVA (PROCEDI ...)
214700970526     C     W             IFNE      0
214800970526     C     FUNKEY        OREQ      *BLANKS
214900970526     C     FUNKEY        IFEQ      *BLANKS
215000970526     C                   EXSR      ENTGES
215100970526     C                   END
215200031229     C     FUNKEY        IFEQ      '01'
215300031229     C                   EXSR      F01GES
215400031229     C                   END
215500141111     C     FUNKEY        IFEQ      '02'
215600141111     C                   EXSR      F02GES
215700141111     C                   END
215800970526     C     FUNKEY        IFEQ      '03'
215900970526     C                   EXSR      F03GES
216000970526     C                   END
216100970526     C     FUNKEY        IFEQ      '06'
216200970526     C                   EXSR      F06GES
216300970526     C                   END
216400970526     C     FUNKEY        IFEQ      '12'
216500970526     C                   EXSR      F12GES
216600970526     C                   END
216700970526     C     FUNKEY        IFEQ      '24'
216800970526     C                   EXSR      F24GES
216900970526     C                   END
217000970526     C                   END
217100970526     C*
217200970526     C                   ENDSR
217300970526     C*------------------------------------------------------------------------*
217400970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
217500970526     C*------------------------------------------------------------------------*
217600970526     C     FUNVAR        BEGSR
217700970526     C*
217800970526     C* INIZIALIZZA VARIABILE -FUNKEY-
217900970526     C*
218000970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
218100970526     C     KEY           IFEQ      ENTER
218200970526     C                   MOVEL     *BLANKS       FUNKEY
218300970526     C                   END
218400970526     C     KEY           IFEQ      F1
218500970526     C                   MOVEL     '01'          FUNKEY
218600970526     C                   END
218700970526     C     KEY           IFEQ      F2
218800970526     C                   MOVEL     '02'          FUNKEY
218900970526     C                   END
219000970526     C     KEY           IFEQ      F3
219100970526     C                   MOVEL     '03'          FUNKEY
219200970526     C                   END
219300970526     C     KEY           IFEQ      F4
219400970526     C                   MOVEL     '04'          FUNKEY
219500970526     C                   END
219600970526     C     KEY           IFEQ      F5
219700970526     C                   MOVEL     '05'          FUNKEY
219800970526     C                   END
219900970526     C     KEY           IFEQ      F6
220000970526     C                   MOVEL     '06'          FUNKEY
220100970526     C                   END
220200970526     C     KEY           IFEQ      F7
220300970526     C                   MOVEL     '07'          FUNKEY
220400970526     C                   END
220500970526     C     KEY           IFEQ      F8
220600970526     C                   MOVEL     '08'          FUNKEY
220700970526     C                   END
220800970526     C     KEY           IFEQ      F9
220900970526     C                   MOVEL     '09'          FUNKEY
221000970526     C                   END
221100970526     C     KEY           IFEQ      F10
221200970526     C                   MOVEL     '10'          FUNKEY
221300970526     C                   END
221400970526     C     KEY           IFEQ      F11
221500970526     C                   MOVEL     '11'          FUNKEY
221600970526     C                   END
221700970526     C     KEY           IFEQ      F12
221800970526     C                   MOVEL     '12'          FUNKEY
221900970526     C                   END
222000970526     C     KEY           IFEQ      F13
222100970526     C                   MOVEL     '13'          FUNKEY
222200970526     C                   END
222300970526     C     KEY           IFEQ      F14
222400970526     C                   MOVEL     '14'          FUNKEY
222500970526     C                   END
222600970526     C     KEY           IFEQ      F15
222700970526     C                   MOVEL     '15'          FUNKEY
222800970526     C                   END
222900970526     C     KEY           IFEQ      F16
223000970526     C                   MOVEL     '16'          FUNKEY
223100970526     C                   END
223200970526     C     KEY           IFEQ      F17
223300970526     C                   MOVEL     '17'          FUNKEY
223400970526     C                   END
223500970526     C     KEY           IFEQ      F18
223600970526     C                   MOVEL     '18'          FUNKEY
223700970526     C                   END
223800970526     C     KEY           IFEQ      F19
223900970526     C                   MOVEL     '19'          FUNKEY
224000970526     C                   END
224100970526     C     KEY           IFEQ      F20
224200970526     C                   MOVEL     '20'          FUNKEY
224300970526     C                   END
224400970526     C     KEY           IFEQ      F21
224500970526     C                   MOVEL     '21'          FUNKEY
224600970526     C                   END
224700970526     C     KEY           IFEQ      F22
224800970526     C                   MOVEL     '22'          FUNKEY
224900970526     C                   END
225000970526     C     KEY           IFEQ      F23
225100970526     C                   MOVEL     '23'          FUNKEY
225200970526     C                   END
225300970526     C     KEY           IFEQ      F24
225400970526     C                   MOVEL     '24'          FUNKEY
225500970526     C                   END
225600970526     C     KEY           IFEQ      ROLLUP
225700970526     C                   MOVEL     'RU'          FUNKEY
225800970526     C                   END
225900970526     C     KEY           IFEQ      HELP
226000970526     C                   MOVEL     'HP'          FUNKEY
226100970526     C                   END
226200970526     C                   ENDSR
226300970526     C*------------------------------------------------------------------------*
226400970526     C* NOTE COSTRUZIONE SCHIERE
226500970526     C*------------------------------------------------------------------------*
226600970526     C*
226700970526     C* -MSG-MESSAGGI
226800970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
226900970526     C* (NON TOCCARE MSG,1 E MSG,2)
227000970526     C*
227100970526     C* -CMD-COMANDI
227200970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
227300970526     C*
227400970526     C* -OPD-OPZIONI
227500970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
227600970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
227700970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
227800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
227900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
228000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
228100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
228200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
228300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
228400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
228500970526     C*
228600970526     C* -FUD-FUNZIONI
228700970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
228800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
228900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
229000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
229100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
229200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
229300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
229400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
229500970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
229600970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
229700970526     O*------------------------------------------------------------------------*
229800970526** DEC - DECODIFICHE
229900970526    MODIFICA                                                      1
230000970526  ANNULLAMENTO                                                    2
230100970526VISUALIZZAZIONE                                                   3
230200970526   RIPRISTINO                                                     4
230300970526  INSERIMENTO                                                     5
230400991014     COPIA                                                        6
230500970526** MSG - MESSAGGI
230600991026TIS723R- Tasto funzionale non gestito                                          1
230700991026TIS723R- Inserimento effettuato                                                2
230800991026TIS723R- Modifica effettuata                                                   3
230900991026TIS723R- Annullamento effettuato                                               4
231000991026TIS723R- Ripristino effettuato                                                 5
231100991026TIS723R- Richiesta funzione per record inesistente                             6
231200991026TIS723R- Record momentaneamente allocato: riprovare più tardi                  7
231300991026TIS723R- Richiesta funzione per record esistente                               8
231400991026TIS723R- Questo codice cliente è già presente con questo tipo file             9
231500991027TIS723R- Il codice tipo file non è fra quelli presenti in anagrafico           10
231600991027TIS723R- Il codice cliente non è presente come padre nell'archivio UNIFICANTI  11
231700000724TIS723R- Campo obbligatorio                                                    12
231800991027TIS723R- Codice filiale inesistente o di categoria V=varia                     13
231900000724TIS723R- Data errata                                                           14
232000000724TIS723R- La data fine deve essere maggiore della data inizio                   15
232100000724TIS723R- Il record non può scadere in un giorno già trascorso                  16
232200000724TIS723R- Il record immesso si sovrappone ad uno già in decorrenza              17
232300000724TIS723R- Codice tipo servizio non presente in tabella                          18
232400001120TIS723R- Oggetto inesistente, verificare e correggere.                         19
232500001120TIS723R- File inesistente, verificare e correggere.                            20
232600001120TIS723R- Libreria inesistente, verificare e correggere.                        21
232700030603TIS723R- Tipo membro non impostabile x il tipo servizio indicato.              22
232800040107TIS723R- Per il traduttore indicato non esiste descrizione dettaglio parametri 23
232900040319TIS723R- Tipo file e servizio incompatibili, correggere.                       24
233000040816TIS723R- Tipo file e/o programma traduttore e/o file tradotto incompatibili.   25
233100051026TIS723R- ATTENZIONE presenti log a stato 0 impossibile cambiare traduttore!    26
233200060616TIS723R- Valori incompatibili, correggere!                                     27
233300101026TIS723R- Errore BartVAS: fatal error                                           28
233400101027TIS723R- ATTENZIONE: modifica effettuata lato AS/400 ma non lato BARTVAS       29
233500120109TIS723R- Tipo file per Internet non impostabile x il tipo servizio indicato.   30
233600120119TIS723R- Tipo file non abilitato per cliente indicato                          31
233700120119TIS723R- Loop tra il tipo file / cliente corrente e quello per duplica         32
233800120119TIS723R- Tipo file / cliente per duplica non possono essere quelli correnti    33
233900120119TIS723R- Valorizzato il cliente per duplica ma non il rispettivo tipo file     34
234000970526** FUD - FUNZIONI
234100141111F01=Desc.Parametri -        (S/N)=S  <== S-FISSO           funz.01¬
234200141111F02=Parametri      -        (S/N)=N                        funz.02¬
234300970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
234400970526                   -        (S/N)=N                        funz.04¬
234500970526                   -        (S/N)=N                        funz.05¬
234600970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
234700970527                   -        (S/N)=N                        funz.07¬
234800970527                   -        (S/N)=N                        funz.08¬
234900970527                   -        (S/N)=N                        funz.09¬
235000970527                   -        (S/N)=N                        funz.10¬
235100970526                   -        (S/N)=N                        funz.11¬
235200970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
235300970526                   -        (S/N)=N                        funz.13¬
235400970526                   -        (S/N)=N                        funz.14¬
235500970526                   -        (S/N)=N                        funz.15¬
235600970526                   -        (S/N)=N                        funz.16¬
235700970526                   -        (S/N)=N                        funz.17¬
235800970526                   -        (S/N)=N                        funz.18¬
235900970526                   -        (S/N)=N                        funz.19¬
236000970526                   -        (S/N)=N                        funz.20¬
236100970526                   -        (S/N)=N                        funz.21¬
236200970526                   -        (S/N)=N                        funz.22¬
236300970526                   -        (S/N)=N                        funz.23¬
236400970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
