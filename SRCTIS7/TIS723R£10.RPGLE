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
030100150114     C                   SETON                                        71
030200150113     C                   EVAL      DVIREPE = vireFLO
030300150113     C                   EVAL      OKPJKPJBU = §virePEKPJ
030400150113     C                   ENDIF
030500141111     C*
030600141111     C                   call      'TRULKPJR'
030700141111     C                   parm                    kpjba
030800141111     C                   parm                    trulkpjds
030900141111     C*
031000141111     C* Gestione estensione anagrafica clienti/tipi file => TRC PE
031100150113     C                   IF        OKPJOPZ = 'F06'
031200141111     C                   CLEAR                   DVIREPE
031300141111     C                   EVAL      KCEdti  = virDTI
031400141111     C                   EVAL      vireTRC = 'PE'
031500141111     C     KEYvire1_C    CHAIN     tivire1l
031600141111     C                   IF        %found(tivire1l)
031700141111     C                   IF        OKPJKPJBU <> *blanks
031800141111     C                   EVAL      §virePEKPJ = OKPJKPJBU
031900141111     C                   EVAL      vireFLO = DVIREPE
032000141111     C                   UPDATE    tivire00
032100150114     C                   SETON                                        71
032200141111     C                   ELSE
032300141111     C                   DELETE    tivire00
032400150114     C                   SETOFF                                       71
032500141111     C                   ENDIF
032600141111     C                   ELSE
032700141111     C                   IF        OKPJKPJBU <> *blanks
032800141111     C                   EVAL      vireKSC = virKSC
032900141111     C                   EVAL      vireTIP = virTIP
033000141111     C                   EVAL      vireDTI = virDTI
033100141111     C                   EVAL      vireTRC = 'PE'
033200141111     C                   EVAL      §virePEKPJ = OKPJKPJBU
033300141111     C                   EVAL      vireFLO = DVIREPE
033400141111     C                   WRITE     tivire00
033500150114     C                   SETON                                        71
033600141111     C                   ENDIF
033700141111     C                   ENDIF
033800150113     C                   ENDIF
033900141111     C*
034000141111     C                   ENDSR
034100970526     C*------------------------------------------------------------------------*
034200970526     C* F03GES - GESTIONE F03 -> FINE
034300970526     C*------------------------------------------------------------------------*
034400970526     C     F03GES        BEGSR
034500970526     C*
034600970526     C* IMPOSTA "PREMUTO F03"
034700970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
034800991025     C                   MOVEL     '1'           S20F03                         *DI PROCEDURA
034900970526     C*
035000970526     C                   ENDSR
035100970526     C*------------------------------------------------------------------------*
035200970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
035300970526     C*------------------------------------------------------------------------*
035400970526     C     F06GES        BEGSR
035500970526     C*
035600970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
035700970526IF  1C     DEPOP1        IFEQ      'F06'
035800970526     C                   EXSR      INSERI
035900970526E   1C                   ENDIF
036000970526IF  1C     DEPOP1        IFEQ      'O02'
036100970526     C                   EXSR      MODIFI
036200970526E   1C                   ENDIF
036300991014IF  1C     DEPOP1        IFEQ      'O03'
036400011129     C                   EXSR      COPIA
036500991014E   1C                   ENDIF
036600991104IF  1C*    DEPOP1        IFEQ      'O04'
036700991104     C*                  EXSR      ANNULL
036800991104E   1C*                  ENDIF
036900970526IF  1C     DEPOP1        IFEQ      'O07'
037000970526     C                   EXSR      RIPRIS
037100970526E   1C                   ENDIF
037200080603     C*
037300080603     C* Gestione estensione anagrafica clienti/tipi file => TRC 01
037400080603     C                   CLEAR                   DVIRE01
037500080603     C                   EVAL      KCEdti  = virDTI
037600080603     C                   EVAL      vireTRC = '01'
037700080603     C     KEYvire1_C    CHAIN     tivire1l
037800080603     C                   IF        %found(tivire1l)
037900080603     C                   IF        vidEML <> *blanks
038000080603     C                   EVAL      §vire01INV = 'S'
038100080603     C                   EVAL      §vire01EML = vidEML
038200080603     C                   EVAL      vireFLO = DVIRE01
038300080603     C                   UPDATE    tivire00
038400080603     C                   ELSE
038500080603     C                   DELETE    tivire00
038600080603     C                   ENDIF
038700080603     C                   ELSE
038800080603     C                   IF        vidEML <> *blanks
038900080603     C                   EVAL      vireKSC = virKSC
039000080603     C                   EVAL      vireTIP = virTIP
039100080603     C                   EVAL      vireDTI = virDTI
039200080603     C                   EVAL      vireTRC = '01'
039300080603     C                   EVAL      §vire01INV = 'S'
039400080603     C                   EVAL      §vire01EML = vidEML
039500080603     C                   EVAL      vireFLO = DVIRE01
039600080603     C                   WRITE     tivire00
039700080603     C                   ENDIF
039800080603     C                   ENDIF
039900970526     C*
040000011129     C* Dopo aver effettuato le operazioni richieste apro la tabela correlata al tipo file.
040100011129     C                   IF        errl00 = '0'                                 *NO ERRORI
040200011129     C                   CALL      'TIS70UR'
040300011129     C                   PARM                    KPJBA
040400011129     C                   PARM                    VIDTIP
040500011129     C                   PARM                    VIDKSC
040600011129     C                   PARM      *blanks       flgope            1
040700011129     C                   ENDIF
040800011129     C*
040900970526     C                   ENDSR
041000970526     C*------------------------------------------------------------------------*
041100970526     C* F12GES - GESTIONE F12 -> RITORNO
041200970526     C*------------------------------------------------------------------------*
041300970526     C     F12GES        BEGSR
041400970526     C*
041500970526     C* IMPOSTA "PREMUTO F12"
041600970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
041700991025     C                   MOVEL     '1'           S20F12                         *DI PROCEDURA
041800970526     C*
041900970526     C                   ENDSR
042000970526     C*------------------------------------------------------------------------*
042100970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
042200970526     C*------------------------------------------------------------------------*
042300970526     C     F24GES        BEGSR
042400970526     C*
042500970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
042600970526     C*
042700970526     C                   ENDSR
042800970526     C*------------------------------------------------------------------------*
042900970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
043000970526     C*------------------------------------------------------------------------*
043100970526     C     INSERI        BEGSR
043200970526     C*
043300970526     C* CONTROLLA I CAMPI IMMESSI
043400970526     C                   EXSR      DECODI
043500970526     C*
043600970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
043700991026IF  1C                   if        errl00 = '0'                                 *NO ERRORI
043800991026     C                   movel     VIDTip        virtip
043900991026     C                   move      VIDksc        virksc
044000991026     C                   z-add     VIDseq        virseq
044100991026     C                   Z-ADD     viddti        G08dat
044200991026     C                   Z-ADD     *ZEROS        G08inv
044300991026     C                   MOVEL     '0'           G08ERR
044400991026     C                   CALL      'XSRDA8'
044500991026     C                   PARM                    WLBDA8
044600991026     C                   Z-ADD     G08inv        virdti
044700991026     c                   exsr      impvir
044800991026     C                   WRITE     tivir000
044900991025     C                   MOVEL     MSG(2)        S20MSG                         *MESSAGGIO
045000970602     C*
045100970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
045200020228     C****               EXSR      AZZVID
045300970526E   1C                   ENDIF
045400970526     C*
045500970526     C                   ENDSR
045600970526     C*------------------------------------------------------------------------*
045700970526     C* MODIFI - OPERAZIONI DI MODIFICA
045800970526     C*------------------------------------------------------------------------*
045900970526     C     MODIFI        BEGSR
046000970526     C*
046100970526     C* CONTROLLA I CAMPI IMMESSI
046200970526     C                   EXSR      DECODI
046300970526     C*
046400970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
046500970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
046600991026     C                   EXSR      LETvir
046700970526     C*
046800970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
046900970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
047000991026     C                   EXSR      IMPvir                                       *IMPOSTA I CAMPI
047100991026     C                   UPDATE    tivir000
047200101027     C                   EVAL      S20MSG = MSG(3)
047300101027     C                   EVAL      DSMSMS = MSG(3)
047400101027     C*
047500101027     C* Se trattasi d tipo servizio "idoneo" modifico anche i dati utente lato "BARTVAS"
047600101027     C                   IF        *IN80
047700111215     C                             and AS400_ok = *on
047800101027      /FREE
047900101027
048000101027         if *in30 = *off;
048100101027           pInMaskChk  = VIDPPIMC;
048200101027         else;
048300101027           pInMaskChk  = pOutCliMskChk;
048400101027         endif;
048500101027
048600101027         if *in31 = *off;
048700101027           pInMaskFile = VIDPPIMF;
048800101027         else;
048900101027           pInMaskFile = pOutCliMskFil;
049000101027         endif;
049100101027
049200101027         if *in32 = *off;
049300101027           pInMaskSem  = VIDPPIMS;
049400101027         else;
049500101027           pInMaskSem  = pOutCliMskSem;
049600101027         endif;
049700101027
049800101027         if *in33 = *off;
049900101027           pInMaskZip  = VIDPPIMZ;
050000101027         else;
050100101027           pInMaskZip  = pOutCliMskZip;
050200101027         endif;
050300101027
050400101027
050500101027         if UBBVSQL01_Upd01UnqCliTipFile(
050600101027                         pInConn
050700101027                       : VIDKSC
050800101027                       : VIDTIP
050900101027                       : pInMaskChk
051000101027                       : pInMaskFile
051100101027                       : pInMaskSem
051200101027                       : pInMaskZip) = 0;
051300101027         else;
051400101027              S20MSG = MSG(29);
051500101027              DSMSMS = MSG(29);
051600101027         endif;
051700101027
051800101027      /END-FREE
051900101027     C
052000101027     C*
052100101027     C* Quindi (già che c sono ricarico anche il job
052200101027     C                   callp(e)  UBBVSKT01_ReloadCliTipF(
052300101027     C                                           VIDKSC
052400101027     C                                          :VIDTIP
052500101027     C                                          :pOutCliProc)
052600101027     C                   ENDIF
052700101027     C*
052800970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
052900970526E   2C                   ENDIF
053000970526E   1C                   ENDIF
053100970526     C*
053200970526     C                   ENDSR
053300991014     C*------------------------------------------------------------------------*
053400991014     C* copia  - OPERAZIONI DI copia
053500991014     C*------------------------------------------------------------------------*
053600991014     C     copia         BEGSR
053700991014     C*
053800991014     C* CONTROLLA I CAMPI IMMESSI
053900991014     C                   EXSR      DECODI
054000991014     C*
054100991014     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
054200991014IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
054300991026     C                   movel     VIDTip        virtip
054400991110     C                   move      VIDksc        virksc
054500991110     C                   z-add     VIDseq        virseq
054600991110     C                   Z-ADD     viddti        G08dat
054700991110     C                   Z-ADD     *ZEROS        G08inv
054800991110     C                   MOVEL     '0'           G08ERR
054900991110     C                   CALL      'XSRDA8'
055000991110     C                   PARM                    WLBDA8
055100991110     C                   Z-ADD     G08inv        virdti
055200991026     C                   EXSR      IMPvir                                       *IMPOSTA I CAMPI
055300991026     C                   write     tivir000
055400991025     C                   MOVEL     MSG(3)        S20MSG
055500991014     C                   MOVEL     '1'           FINL00                         *FINE PGM
055600991014E   2C                   ENDIF
055700991014     C*
055800991014     C                   ENDSR
055900970526     C*------------------------------------------------------------------------*
056000970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
056100970526     C*------------------------------------------------------------------------*
056200991104     C*    ANNULL        BEGSR
056300991025     C*
056400991025     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
056500991104     C*                  EXSR      LETvir
056600991025     C*
056700991025     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
056800991104IF  1C*    ERRL00        IFEQ      '0'                                          *NO ERRORI
056900991104     C*                  DELETE    tivir000                                     *CANCELLA
057000991104     C*                  MOVEL     MSG(4)        s20MSG                         *MESSAGGIO
057100991104     C*                  MOVEL     '1'           FINL00                         *FINE PGM
057200991104E   1C*                  ENDIF
057300970526     C*
057400991104     C*                  ENDSR
057500970526     C*------------------------------------------------------------------------*
057600970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
057700970526     C*------------------------------------------------------------------------*
057800970526     C     RIPRIS        BEGSR
057900970526     C                   ENDSR
058000970526     C*------------------------------------------------------------------------*
058100991026     C* LETvir - LEGGE IL RECORD PERL'AGGIORNAMENTO
058200970526     C*------------------------------------------------------------------------*
058300991026     C     LETvir        BEGSR
058400970526     C*
058500991025     C                   MOVEL     S20tip        KCEtip
058600991026     C                   MOVEL     S20ksc        KCEksc
058700000420     C     KEYvi2        CHAIN     tivir03L                           9998
058800970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
058900970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
059000991025     C                   MOVEL     MSG(7)        S20MSG                         *MESSAGGIO
059100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
059200970526E   1C                   ENDIF
059300970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
059400970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
059500991025     C                   MOVEL     MSG(6)        S20MSG                         *MESSAGGIO
059600970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
059700970526E   1C                   ENDIF
059800970526     C*
059900970526     C                   ENDSR
060000970526     C*------------------------------------------------------------------------*
060100970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
060200970526     C*------------------------------------------------------------------------*
060300970526     C     DECODI        BEGSR
060400001120     C*
060500001120     C*----------------------
060600001120     C* Controllo del codice
060700001120     C*----------------------
060800001120     C     ERRL00        IFEQ      '0'
060900001120     C     VIDksc        IFne      *blank
061000001120     C     VIDksc        andne     *zeros
061100001120     C                   exsr      cntksc
061200001120     C                   ELSE
061300991027     C                   MOVEL     '1'           ERRL00
061400991027     C                   SETON                                        65
061500991027     C                   MOVEL     MSG(12)       DSMSMS
061600001120     C                   ENDIF
061700001120     C                   ENDIF
061800101026     C*
061900101026     C* Verifico se x il servizio a video è richiesta l'indicazione della password FTP
062000101026     C     ERRL00        IFEQ      '0'
062100101026     C     VIDisv        lookup    isvftpeml                              80
062200101026 xxx C***                setoff                                       80
062300101026     C                   ENDIF
062400001120     C*----------------------
062500001120     C* Controllo della sequenza
062600001120     C*----------------------
062700001120     C     ERRL00        IFEQ      '0'
062800001120     C     VIDseq        IFne      0
062900001120     C                   ELSE
063000991027     C                   MOVEL     '1'           ERRL00
063100991027     C                   SETON                                        67
063200991027     C                   MOVEL     MSG(12)       DSMSMS
063300001120     C                   ENDIF
063400001120     C                   ENDIF
063500120109     C*----------------------
063600120109     C* Controllo tipo file
063700120109     C*----------------------
063800120109     C     ERRL00        IFEQ      '0'
063900120109     C     VIDtip        IFne      *blank
064000120109     C                   exsr      cnttip
064100120109     C                   ELSE
064200120109     C                   MOVEL     '1'           ERRL00
064300120109     C                   SETON                                        66
064400120109     C                   MOVEL     MSG(12)       DSMSMS
064500120109     C                   ENDIF
064600120109     C                   ENDIF
064700120109     C*----------------------
064800120109     C* Tipo servizio
064900120109     C*----------------------
065000120109     C     ERRL00        IFEQ      '0'
065100120109     C     VIDisv        IFne      *blank
065200120109     C                   exsr      cntisv
065300120109     C* Verifico la compatibilità del servizio rispetto al tipo-file
065400120109     C                   IF        VTFUD <> §ISVVERS
065500120109     C                   MOVEL     '1'           ERRL00
065600120109     C                   SETON                                        6670
065700120109     C                   MOVEL     MSG(24)       DSMSMS
065800120109     C                   ENDIF
065900120109     C                   ELSE
066000120109     C                   MOVEL     '1'           ERRL00
066100120109     C                   SETON                                        70
066200120109     C                   MOVEL     MSG(12)       DSMSMS
066300120109     C                   ENDIF
066400120109     C                   ENDIF
066500120109     C*--------------------
066600120109     C* Dati solo x upload no in download
066700120109     C*--------------------
066800120109     C* a seconde se siamo in Download o Upload sproteggo alcuni campi e ne proteggo altri
066900120109     C* e se non abbiamo errori pulisco i campi protetti (se abbiamo un errore potrebbe essere
067000120109     C*  che non siamo in D o U e i valori nei campi servono)
067100120109     C                   IF        §ISVVers   = 'D'
067200120109     c                   SETON                                        93
067300120109     C     ERRL00        IFEQ      '0'
067400120109     C                   MOVEL     *blanks       VIDpoi
067500120109     C                   MOVEL     *blanks       VIDEmL
067600120109     C                   MOVEL     *blanks       VIDfi2
067700120109     C                   MOVEL     *blanks       VIDfi4
067800120109     C                   MOVEL     *blanks       VIDfi4D
067900120109     C                   ENDIF
068000120109     C                   ENDIF
068100120109     C                   IF        §ISVVers   = 'U'
068200120109     C                   SETOFF                                       93
068300120109     C     ERRL00        IFEQ      '0'
068400120109     C                   MOVEL     *blanks       VIDTMbD
068500120109     C                   MOVEL     *blanks       VIDTip2
068600120109     C                   MOVEL     *blanks       VIDTip3
068700120109     C                   MOVEL     *blanks       VIDTip4
068800120119     C                   MOVEL     *blanks       VIDKsc2
068900120119     C                   MOVEL     *blanks       VIDKsc3
069000120119     C                   MOVEL     *blanks       VIDKsc4
069100120109     C                   ENDIF
069200120109     C                   ENDIF
069300100614     C*--------------------
069400100614     C* Tipo membro d default...  solo x anagrafiche d download (ovviamente no in visualizz.)
069500100614     C*--------------------
069600100614     C     ERRL00        IFEQ      '0'
069700100614     C                   IF        DEPOP1 = 'F06' OR
069800100614     C                             DEPOP1 = 'O03'
069900100614     C                   IF        §ISVPROC = 'F' AND
070000100614     C                             §ISVVERS = 'D'
070100100614     C                   IF        VIDtmbd = *blanks AND
070200100614     C                             vtfud   = 'D'
070300100614     C                   MOVEL     'M'           VIDtmbd
070400100614     C                   ENDIF
070500100614     C                   ENDIF
070600100614     C                   ENDIF
070700100614     C                   ENDIF
070800030603     C*----------------------
070900030603     C* Tipo membro
071000030603     C*----------------------
071100030603     C     ERRL00        IFEQ      '0'
071200030603     C     VIDtmbd       IFNE      *blank
071300030603     C                   IF        §ISVPROC = 'F' AND
071400030603     C                             §ISVVERS = 'D'
071500030603     C                   ELSE
071600030603     C                   MOVEL     '1'           ERRL00
071700030603     C                   SETON                                        61
071800030603     C                   MOVEL     MSG(22)       DSMSMS
071900030603     C                   ENDIF
072000030603     C                   ENDIF
072100030603     C                   ENDIF
072200120109     C*----------------------
072300120109     C* Campi duplicazione automatica su tipi file
072400120109     C*----------------------
072500120109     C     ERRL00        IFEQ      '0'
072600120109     C*
072700120109     C                   EVAL      MaxCiclo = %elem(Tips)
072800120109     C                   EVAL      Tips(1)  = VIDTip2
072900120109     C                   EVAL      Tips(2)  = VIDTip3
073000120109     C                   EVAL      Tips(3)  = VIDTip4
073100120119     C* se il tipo file è valorizzato e il cliente relativo no (o 0 o ' ')
073200120119     C* lo forzo col cliente principale
073300120119     C                   IF        (VIDKsc2 = *zeros or
073400120119     C                              VIDKsc2 = *blank   ) and
073500120119     C                             VIDTip2 <> *blank
073600120119     C                   EVAL      VIDKsc2 =  VIDKsc
073700120119     C                   ENDIF
073800120119     C                   IF        (VIDKsc3 = *zeros or
073900120119     C                              VIDKsc3 = *blank   ) and
074000120119     C                             VIDTip3 <> *blank
074100120119     C                   EVAL      VIDKsc3 =  VIDKsc
074200120119     C                   ENDIF
074300120119     C                   IF        (VIDKsc4 = *zeros or
074400120119     C                              VIDKsc4 = *blank   ) and
074500120119     C                             VIDTip4 <> *blank
074600120119     C                   EVAL      VIDKsc4 =  VIDKsc
074700120119     C                   ENDIF
074800120119     C                   EVAL      Kscs(1)  = VIDKsc2
074900120119     C                   EVAL      Kscs(2)  = VIDKsc3
075000120119     C                   EVAL      Kscs(3)  = VIDKsc4
075100120109     C     1             DO        MaxCiclo      i
075200120109     C     Tips(i)       IFNE      *blank
075300120119     C     Kscs(i)       ORNE      *blank
075400120119     C     Kscs(i)       ANDNE     *zeros
075500120109     C                   IF        §ISVVERS = 'D'
075600120119     C                   EXSR      CntTip234
075700120111     C* se mi è stato dato un errore, esco dal ciclo
075800120111     C                   IF        ERRL00 <> '0'
075900120111     C                   LEAVE
076000120111     C                   ENDIF
076100120109     C                   ELSE
076200120109     C                   SELECT
076300120109     C                   WHEN      i = 1
076400120119     C                   SETON                                        7549
076500120109     C                   WHEN      i = 2
076600120119     C                   SETON                                        7650
076700120109     C                   WHEN      i = 3
076800120119     C                   SETON                                        7751
076900120109     C                   ENDSL
077000120109     C                   MOVEL     '1'           ERRL00
077100120109     C                   MOVEL     MSG(30)       DSMSMS
077200120109     C                   ENDIF
077300120109     C                   ENDIF
077400120109     C                   ENDDO
077500120109     C*
077600120109     C                   ENDIF
077700001120     C*----------------------
077800001120     C* Controllo della descrizione
077900001120     C*----------------------
078000001120     C     ERRL00        IFEQ      '0'
078100001120     C     VIDdes        IFne      *blanks
078200001120     C                   ELSE
078300991122     C*                  MOVEL     '1'           ERRL00
078400991122     C*                  SETON                                        62
078500991122     C*                  MOVEL     MSG(12)       DSMSMS
078600991122     C                   MOVEL     VDDTIP        VIDDES
078700001120     C                   ENDIF
078800001120     C                   ENDIF
078900991026     C*--------------------
079000991026     C* DATA inizio
079100991026     C*--------------------
079200991026     C     *LIKE         DEFINE    VIDDti        DtiVID
079300001120     C     ERRL00        IFEQ      '0'
079400991026     C                   Z-ADD     VIDDti        wda8
079500991026     C                   Z-ADD     *ZEROS        da8w
079600991026     C                   EXSR      CHKDA8
079700991026     C                   Z-ADD     WDA8          viddti
079800991026     C                   Z-ADD     DA8W          dtivid
079900001120     C     ERRL00        IFEQ      '1'
080000991026     C                   SETON                                        63
080100991026     C                   MOVEL     MSG(14)       dsmsms
080200001120     C                   ENDIF
080300001120     C                   ENDIF
080400991026     C* INSERIMENTO -> RECORD INESISTENTE
080500001120     C     ERRL00        IFEQ      '0'
080600001120     C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
080700001120     C     DEPOP1        orEQ      'O03'                                        *INSERIMENTO
080800991026     C                   move      vidksc        KCEksc
080900000420     C                   move      vidtip        KCEtip
081000000420     C                   z-add     dtivid        KCEdti
081100000420     C     KEYvi2        CHAIN(N)  tivir03L                           99
081200001120     C     *IN99         IFEQ      *OFF
081300991026     C                   MOVEL     '1'           ERRL00
081400991026     C                   MOVEL     MSG(8)        DSMSMS
081500991026     C                   SETON                                        676566
081600991026     C                   SETON                                        63
081700001120     C                   ENDIF
081800001120     C                   ENDIF
081900001120     C                   ENDIF
082000001120     C*--------------------
082100991026     C* DATA SCADENZA
082200991026     C*--------------------
082300991026     C     *LIKE         DEFINE    VIDDtf        DtfVID
082400001120     C     ERRL00        IFEQ      '0'
082500991026     C                   Z-ADD     VIDDtf        WDA8
082600991026     C                   Z-ADD     *ZEROS        DA8W
082700991026     C                   EXSR      CHKDA8
082800991026     C                   Z-ADD     WDA8          VIDDtf
082900991026     C                   Z-ADD     DA8W          DtfVID
083000001120     C     ERRL00        IFEQ      '1'
083100991026     C                   SETON                                        68
083200991026     C                   MOVEL     MSG(14)       DSMSMS
083300001120     C                   ENDIF
083400001120     C                   ENDIF
083500991026     C* SCADENZA < DECORRENZA
083600001120     C     ERRL00        IFEQ      '0'
083700001120     C     DtfVID        IFLT      DtiVID
083800991026     C                   SETON                                        68
083900991026     C                   MOVEL     MSG(15)       DSMSMS
084000991026     C                   MOVEL     '1'           ERRL00
084100001120     C                   ENDIF
084200001120     C                   ENDIF
084300991026     C* SCADENZA >= DATA CORRENTE
084400141021     C     ERRL00        IFEQ      '0'
084500141021     C***  DEPOP1        IFEQ      'O02'
084600141021     C***  DEPOP1        OREQ      'O07'
084700141021     C***  DEPOP1        OREQ      'F06'
084800141021     C     DEPOP1        IFEQ      'O07'
084900141021     C     DEPOP1        OREQ      'F06'
085000001120     C     DtfVID        IFLT      DATCOR
085100001120     C                   SETON                                        68
085200991026     C                   MOVEL     MSG(16)       DSMSMS
085300991026     C                   MOVEL     '1'           ERRL00
085400001120     C                   ENDIF
085500001120     C                   ENDIF
085600001120     C                   ENDIF
085700991026     C*---
085800991026     C* DECORRENZA + SCADENZA
085900991026     C*---
086000991026     C* ACCAVALLAMENTO DATI
086100001120     C     ERRL00        IFEQ      '0'
086200991026     C                   MOVEL     VIDtip        KCEtip
086300991026     C                   MOVEL     vidksc        KCEksc
086400991026     C     KEyvir        SETLL     tivir03L
086500991026     C     KEyvir        READE(N)  tivir03L                               99
086600001120     C     *IN99         DOWEQ     *OFF
086700001120     C     DEPOP1        IFNE      'F06'                                        *NON INSERIMENTO
086800991026     C     DtiVID        ANDNE     virdti                                       *RECORD IN LINEA
086900991026     C     DEPOP1        OREQ      'F06'                                        *INSERIMENTO
087000001120     C     DtiVID        IFGE      virdti                                       *DECORRENZA
087100991026     C     DtiVID        ANDLE     virdtf
087200991026     C     DtfVID        ORGE      virdti                                       *SCADENZA
087300991026     C     DtfVID        ANDLE     virdtf
087400991026     C     virdti        ORGE      DtiVID                                       *DECORRENZA
087500991026     C     virdtf        ANDLE     DtfVID
087600991026     C                   SETON                                        666765
087700991026     C                   SETON                                        6368
087800991026     C                   MOVEL     MSG(17)       DSMSMS
087900991026     C                   MOVEL     '1'           ERRL00
088000991026     C                   LEAVE
088100001120     C                   ENDIF
088200001120     C                   ENDIF
088300991026     C     KEyvir        READE(N)  tivir03L                               99
088400001120     C                   ENDDO
088500001120     C                   ENDIF
088600001120     C*--------------------
088700001120     C* Controllo filiale
088800001120     C*--------------------
088900001120     C     ERRL00        IFEQ      '0'
089000001120     C     VIDpoi        IFne      *blank
089100001120     C     VIDpoi        andne     *zeros
089200001120     C                   exsr      cntpoi
089300001120     C                   ELSE
089400120109     C     §ISVVers      IFNE      'D'
089500991027     C                   MOVEL     '1'           ERRL00
089600000724     C                   MOVEL     MSG(12)       DSMSMS
089700991027     C                   SETON                                        69
089800991122     C                   ENDIF
089900001120     C                   ENDIF
090000001120     C                   ENDIF
090100001120     C*--------------------
090200011016     C* Programma traduttore
090300001120     C*--------------------
090400001120     C     ERRL00        IFEQ      '0'
090500001120     C     VIDpgt        IFNE      *blank
090600001120     C                   CALL(e)   'TIS721C'
090700050112     C                   PARM      *blanks       LIBRERIA         10
090800001120     C                   PARM      VIDpgt        OBJECT           10
090900001120     C                   PARM      '*PGM   '     TYPE              7
091000001120     C                   PARM      '0'           ESITO             1
091100001120     C                   IF        ESITO = '1'
091200001120     C                   MOVEL     '1'           ERRL00
091300001120     C                   SETON                                        54
091400001120     C                   MOVEL     MSG(19)       DSMSMS
091500001120     C                   ENDIF
091600001120     C                   ENDIF
091700001120     C                   ENDIF
091800001120     C*--------------------
091900001120     C* Programma post-traduzione
092000001120     C*--------------------
092100001120     C     ERRL00        IFEQ      '0'
092200001120     C     VIDpgi        IFNE      *blank
092300001120     C                   CALL(e)   'TIS721C'
092400050112     C                   PARM      *blanks       LIBRERIA         10
092500001120     C                   PARM      VIDpgi        OBJECT           10
092600001120     C                   PARM      '*PGM   '     TYPE              7
092700001120     C                   PARM      '0'           ESITO             1
092800001120     C                   IF        ESITO = '1'
092900001120     C                   MOVEL     '1'           ERRL00
093000001120     C                   SETON                                        55
093100001120     C                   MOVEL     MSG(19)       DSMSMS
093200001120     C                   ENDIF
093300001120     C                   ENDIF
093400001120     C                   ENDIF
093500001120     C*--------------------
093600001120     C* File da tradurre
093700001120     C*--------------------
093800001120     C     ERRL00        IFEQ      '0'
093900001120     C     VIDfid        IFNE      *blank
094000001120     C                   CALL(e)   'TIS721C'
094100050112     C                   PARM      *blanks       LIBRERIA         10
094200001120     C                   PARM      VIDfid        OBJECT           10
094300001120     C                   PARM      '*FILE  '     TYPE              7
094400001120     C                   PARM      '0'           ESITO             1
094500001120     C                   IF        ESITO = '1'
094600001120     C                   MOVEL     '1'           ERRL00
094700001120     C                   SETON                                        53
094800001120     C                   MOVEL     MSG(20)       DSMSMS
094900001120     C                   ENDIF
095000001120     C                   ENDIF
095100001120     C                   ENDIF
095200001120     C*--------------------
095300001120     C* File storico
095400001120     C*--------------------
095500001120     C     ERRL00        IFEQ      '0'
095600001120     C     VIDfis        IFNE      *blank
095700001120     C                   CALL(e)   'TIS721C'
095800050112     C                   PARM      *blanks       LIBRERIA         10
095900001120     C                   PARM      VIDfis        OBJECT           10
096000001120     C                   PARM      '*FILE  '     TYPE              7
096100001120     C                   PARM      '0'           ESITO             1
096200001120     C                   IF        ESITO = '1'
096300001120     C                   MOVEL     '1'           ERRL00
096400001120     C                   SETON                                        56
096500001120     C                   MOVEL     MSG(20)       DSMSMS
096600001120     C                   ENDIF
096700001120     C                   ENDIF
096800001120     C                   ENDIF
096900001120     C*--------------------
097000001120     C* File tradotto
097100001120     C*--------------------
097200001120     C     ERRL00        IFEQ      '0'
097300001120     C     VIDfit        IFNE      *blank
097400001120     C                   CALL(e)   'TIS721C'
097500050112     C                   PARM      VIDfi3        LIBRERIA         10
097600001120     C                   PARM      VIDfit        OBJECT           10
097700001120     C                   PARM      '*FILE  '     TYPE              7
097800001120     C                   PARM      '0'           ESITO             1
097900001120     C                   IF        ESITO = '1'
098000001120     C                   MOVEL     '1'           ERRL00
098100001120     C                   SETON                                        57
098200001120     C                   MOVEL     MSG(20)       DSMSMS
098300001120     C                   ENDIF
098400001120     C                   ENDIF
098500001120     C                   ENDIF
098600001120     C*--------------------
098700001120     C* Libreria file tradotto
098800001120     C*--------------------
098900001120     C     ERRL00        IFEQ      '0'
099000001120     C     VIDfi3        IFNE      *blank
099100001120     C                   CALL(e)   'TIS721C'
099200050112     C                   PARM      *blanks       LIBRERIA         10
099300001120     C                   PARM      VIDfi3        OBJECT           10
099400001120     C                   PARM      '*LIB   '     TYPE              7
099500001120     C                   PARM      '0'           ESITO             1
099600001120     C                   IF        ESITO = '1'
099700001120     C                   MOVEL     '1'           ERRL00
099800001120     C                   SETON                                        52
099900001120     C                   MOVEL     MSG(21)       DSMSMS
100000001120     C                   ENDIF
100100001120     C                   ENDIF
100200001120     C                   ENDIF
100300040816     C*--------------------
100400100311     C* Controllo corrispondenza Tipo File / Programma Traduttore / File Tradotto / Tipo membro Trad
100500040816     C*--------------------
100600040816     C     ERRL00        IFEQ      '0'
100700040816     C     VIDfit        IFne      *blank
100800040816     C     VIDpgt        ORne      *blank
100900040816     C                   EVAL      TBEcod = 'IFT'
101000040816     C     KEYtbe1_P     SETLL     tntbe01l
101100040816     C                   IF        %equal(tntbe01l)
101200040816     C* Inizializzo indici e schiere
101300040816     C                   Z-ADD     *zeros        j
101400040816     C                   CLEAR                   skTipF
101500040816     C                   CLEAR                   skPgt
101600040816     C                   CLEAR                   skFileT
101700100311     C                   CLEAR                   skMembroT
101800040816     C* Carico le schiere da tabella 'IFT'
101900040816     C     KEYtbe1_P     READE     tntbe01l
102000040816     C                   DOW       not %eof(tntbe01l)
102100040816     C                   IF        TBEatb = *blanks
102200040816     C                   EVAL      DIFT = TBEuni
102300040816     C                   ADD       1             j
102400100311     C                   EVAL      skTipF(j)    = TBEke1
102500100311     C                   EVAL      skPgt(j)     = §IFTPGT
102600100311     C                   EVAL      skFileT(j)   = §IFTFILE
102700100311     C                   EVAL      skMembroT(j) = §IFTTMBD
102800040816     C                   ENDIF
102900040816     C     KEYtbe1_P     READE     tntbe01l
103000040816     C                   ENDDO
103100040816     C* Imposto cosa testare
103200040816     C                   IF        VIDPGT <> *blanks
103300040816     C                   EVAL      wVIDPGT = VIDPGT
103400040816     C                   ELSE
103500040816     C                   EVAL      wVIDPGT = VIDPGTD
103600040816     C                   ENDIF
103700040816     C                   IF        VIDFIT <> *blanks
103800040816     C                   EVAL      wVIDFIT = VIDFIT
103900040816     C                   ELSE
104000040816     C                   EVAL      wVIDFIT = VIDFITD
104100040816     C                   ENDIF
104200040816     C* Testo se il traduttore indicato è tra quelli da controllare (in tabella 'IFT')
104300040823     C                   MOVEL     '0'           wFlgIFTok         1
104400100223     C                   SETOFF                                       16
104500040816     C     wVIDPGT       LOOKUP    skPgt                                  15
104600040816     C                   IF        %found
104700040816     C* Testo se vi è corrispondenza
104800040816     C                   EVAL      j = 1
104900040816     C                   DOW       j <= %elem(skTipF)
105000100223     C                   IF        skTipF(j)  = VIDTIP  AND
105100100223     C                             skPgt(j)   = wVIDPGT
105200100223     C                   SETON                                        16
105300100311     C*
105400100223     C                   IF        skFileT(j) = wVIDFIT
105500040816     C                   MOVEL     '1'           wFlgIFTok
105600100311     C*
105700100311     C                   IF        DEPOP1 = 'O02' OR
105800100311     C                             DEPOP1 = 'O03' OR
105900100311     C                             DEPOP1 = 'F06'
106000100312     C                   IF        VIDtmbd      =  *blanks AND
106100100312     C                             skMembroT(j) <> *blanks
106200100311     C                   EVAL      VIDtmbd = skMembroT(j)
106300100311     C                   SETON                                        61
106400100311     C                   ENDIF
106500100311     C                   ENDIF
106600100311     C*
106700040816     C                   LEAVE
106800040816     C                   ENDIF
106900100311     C*
107000100223     C                   ENDIF
107100040816     C                   ADD       1             j
107200040816     C                   ENDDO
107300040816     C* Se nn trovata esatta corrispondenza => errore
107400100223     C                   IF        *IN16 and wFlgIFTok = '0'
107500040816     C                   MOVEL     '1'           ERRL00
107600040816     C                   MOVEL     MSG(25)       DSMSMS
107700040816     C                   SETON                                        665457
107800040816     C                   ENDIF
107900040816     C                   ENDIF
108000040816     C                   ENDIF
108100040816     C                   ENDIF
108200040823     C                   ENDIF
108300051026     C*--------------------
108400051026     C* Controllo particolare x anagrafiche d download: se si sta cambiando il traduttore
108500051026     C* verifico prima che nn vi siano attualmente record d log a stato zero
108600051026     C*--------------------
108700051026     C     ERRL00        IFEQ      '0'
108800120109     C     §ISVVers      IFEQ      'D'
108900051026     C     VIDpgt        IFNE      virPGT
109000051026     C     VIDpgi        ORNE      virPGI
109100051026     C                   CALL      'TIS723R1'
109200051026     C                   PARM                    VIDKSC
109300051026     C                   PARM                    VIDTIP
109400120109     C                   PARM                    §ISVVers
109500051026     C                   PARM      *zeros        wRecZero          9 0
109600051026     C                   IF        wRecZero > *zeros
109700051026     C                   MOVEL     '1'           ERRL00
109800051026     C                   MOVEL     MSG(26)       DSMSMS
109900051026     C                   SETON                                        5455
110000051026     C                   ENDIF
110100051026     C                   ENDIF
110200051026     C                   ENDIF
110300051026     C                   ENDIF
110400060616     C*--------------------
110500060616     C* File Da tradurre / File Storico / Programma Post-Traduzione
110600060616     C*--------------------
110700060616     C     ERRL00        IFEQ      '0'
110800060616     C                   IF        (VIDfid='*' AND VIDfis='*' AND VIDpgi='*') OR
110900060616     C                             (VIDfid<>'*' AND VIDfis<>'*' AND VIDpgi<>'*')
111000060616     C                   ELSE
111100060616     C                   MOVEL     '1'           ERRL00
111200060616     C                   SETON                                        535556
111300060616     C                   MOVEL     MSG(27)       DSMSMS
111400060616     C                   ENDIF
111500060616     C                   ENDIF
111600081020     C*--------------------
111700081020     C* Mail x eventuale feed-back richiesto
111800081020     C*--------------------
111900081020     C     ERRL00        IFEQ      '0'
112000081020     C                   CLEAR                   DSEMAIL
112100081020     C                   EVAL      §EMLINDI = VIDEML
112200081020     C                   CALL      'XEMAIL'
112300081020     C                   PARM                    DSEMAIL
112400081020     C                   IF        §EMLERRO = '1'
112500081020     C                   MOVEL     '1'           ERRL00
112600081020     C                   SETON                                        64
112700081020     C                   MOVEL     §EMLMSGO      DSMSMS
112800081020     C                   ENDIF
112900081020     C                   ENDIF
113000001120     C*
113100001120     C                   ENDSR
113200000626     C*------------------------------------------------------------------------*
113300000626     C* Cntisv - CONTROLLA codice tipo legame
113400000626     C*------------------------------------------------------------------------*
113500001120     C     cntisv        begsr
113600001120     C                   clear                   TIBS02DS
113700001120     C*
113800101027     C     '?'           SCAN      Vidisv                                 28
113900001120     C*  Ricerca
114000001120     C                   eval      t02tla = 'L'
114100101027     C                   IF        *IN28 = *ON
114200001120     C                   movel     'R'           t02mod
114300001120     C                   else
114400001120     C                   movel     'C'           t02mod
114500001120     C                   movel(p)  vidisv        t02ke1
114600001120     C                   end
114700000626     C                   movel     'ISV'         t02cod
114800001120     C                   movel     knsif         t02sif
114900001120     C                   CALL      'TIBS02R'
115000001120     C                   parm                    KPJBA
115100001120     C                   parm                    TIBS02DS
115200001120     C
115300001120     C*
115400001120     C                   IF        T02err = *blanks
115500000626     C                   movel     T02ke1        Vidisv
115600000626     C                   movel     T02uni        vddisv
115700030603     C                   movel(P)  T02uni        DISV
115800001120     C                   else
115900001120     C                   movel     '1'           errl00
116000001120     C                   seton                                        70
116100001120     C                   MOVEL     MSG(18)       DSMSMS
116200001120     C                   ENDIF
116300001120     C
116400001120     C                   endsr
116500991027     C*------------------------------------------------------------------------*
116600991027     C* cnttip - Controlla esistenza tipo file su tivt00f
116700991027     C*------------------------------------------------------------------------*
116800001120     C     cnttip        begsr
116900001120     C                   clear                   TIBS02DS
117000000626     C*
117100101027     C     '?'           SCAN      vidtip                                 28
117200001120     C*  Ricerca
117300001120     C                   eval      t02tla = 'L'
117400101027     C                   IF        *IN28 = *ON
117500001120     C                   movel     'R'           t02mod
117600001120     C                   else
117700001120     C                   movel     'C'           t02mod
117800001120     C                   movel(p)  vidtip        t02ke1
117900001120     C                   end
118000991108     C                   movel     'ITF'         t02cod
118100001120     C                   movel     knsif         t02sif
118200991108     C                   CALL      'TIBS02R'
118300001120     C                   parm                    KPJBA
118400001120     C                   parm                    TIBS02DS
118500001120     C
118600001120     C*
118700001120     C                   IF        T02err = *blanks
118800991108     C                   movel     T02ke1        vidtip
118900020218     C                   movel     T02uni        DITF
119000020218     C                   movel     §ITFDESC      vddtip
119100021003     C* Sostituisco con il dato proveniente dalla tabella solo se nn già indicato
119200021003     C* qualcosa nelle regole specifiche x il cliente/tipo file.
119300021014     C                   if        vidfit = *blanks
119400021014     C                   movel     §ITFLIBD      vidfi2
119500021014     C                   endif
119600001120     C                   else
119700001120     C                   movel     '1'           errl00
119800991108     C                   movel     *blank        vddtip
119900111215     C* pulisco anche il cod. tabella associata
120000111215     C                   movel     *blank        vidtab
120100001120     C                   seton                                        66
120200991108     C                   MOVEL     msg(11)       DSMSMS
120300991108     C                   ENDIF
120400001120     C                   if        errl00 = '0'
120500120109     C     vidtip        chain     tivtf01l                           97
120600001120     C                   if        not *in97
120700001120     C                   movel     vtfpgt        vidpgtd
120800001120     C                   movel     vtfpgi        vidpgid
120900001120     C                   movel     vtffid        vidfidd
121000001120     C                   movel     vtffis        vidfisd
121100001120     C                   movel     vtffit        vidfitd
121200111214     C                   movel     vtffi1        vidtab
121300101013     C                   movel     vtfppi        DVTFPPI
121400101026     C                   if        DEPOP1 = 'F06' OR
121500101026     C                             DEPOP1 = 'O03'
121600101026     C                   select
121700101026     C                   when      vtfUD = 'U'
121800101026     C                   movel     D§VTFPPIMF    vidppiMC
121900101026     C                   when      vtfUD = 'D'
122000101026     C                   movel     D§VTFPPIMF    vidppiMF
122100101026     C                   endsl
122200101026     C                   movel     D§VTFPPIMS    vidppiMS
122300101026     C                   movel     D§VTFPPIMZ    vidppiMZ
122400101026     C                   endif
122500100308     C                   if        vidfi4d <> *blanks and vidfi4d <> '*'
122600001120     C                   eval      vidfi4d = %subst(vidfitd:1:5) + '00F'
122700100308     C                   endif
122800001120     C                   else
122900001120     C                   movel     *blank        vidpgtd
123000001120     C                   movel     *blank        vidpgid
123100001120     C                   movel     *blank        vidfidd
123200001120     C                   movel     *blank        vidfisd
123300001120     C                   movel     *blank        vidfitd
123400001120     C                   movel     *blank        vidfi4d
123500001120     C                   endif
123600001120     C                   endif
123700001120     C*
123800001120     C                   endsr
123900991027     C*------------------------------------------------------------------------*
124000120109     C* CntTip234 - Controlla esistenza altri tipi file su tivt00f
124100991027     C*------------------------------------------------------------------------*
124200120109     C     CntTip234     BEGSR
124300120119     C*
124400120119     C* se ho indicato un cliente devo indicare il tipo file
124500120119     C                   IF        Kscs(i) <>*blank and
124600120119     C                             Kscs(i) <>*zeros and
124700120119     C                             Tips(i) = *blank
124800120119     C                   SELECT
124900120119     C                   WHEN      i = 1
125000120119     C                   SETON                                        7549
125100120119     C                   WHEN      i = 2
125200120119     C                   SETON                                        7650
125300120119     C                   WHEN      i = 3
125400120119     C                   SETON                                        7751
125500120119     C                   ENDSL
125600120119     C                   MOVEL     '1'           ERRL00
125700120119     C                   MOVEL     MSG(34)       DSMSMS
125800120119     C                   LEAVESR
125900120119     C                   ENDIF
126000010913     C*
126100120119     C* il tipo file / cliente indicati non devono essere quelli in gestione
126200120119     C                   IF        Tips(i) = VIDTip and
126300120119     C                             Kscs(i) = VIDKsc
126400120111     C                   SELECT
126500120111     C                   WHEN      i = 1
126600120119     C                   SETON                                        7549
126700120111     C                   WHEN      i = 2
126800120119     C                   SETON                                        7650
126900120111     C                   WHEN      i = 3
127000120119     C                   SETON                                        7751
127100120111     C                   ENDSL
127200120111     C                   MOVEL     '1'           ERRL00
127300120111     C                   MOVEL     MSG(33)       DSMSMS
127400120111     C                   LEAVESR
127500120111     C                   ENDIF
127600120111     C*
127700120111     C* il tipo file indicato deve esistere ed essere abilitato su questa anagrafica per il cliente
127800120119     C* indicato
127900120111     C* (per abilitato basta che la data fine validità sia maggiore di oggi; se anche la data inizio
128000120111     C*  è maggiore di oggi non importa, sarà valido nel futuro)
128100120119     C                   EVAL      x_VIRKsc = Kscs(i)
128200120111     C                   EVAL      x_VIRTip = Tips(i)
128300120111     C                   EVAL      x_VIRDtF = %dec(%date() : *ISO)
128400120111     C     K03VIR06      SETLL     TIVIR06L
128500120111     C     K02VIR06      READE     TIVIR06L
128600120111     C                   if        %eof
128700120109     C                   SELECT
128800120109     C                   WHEN      i = 1
128900120109     C                   SETON                                        75
129000120109     C                   WHEN      i = 2
129100120109     C                   SETON                                        76
129200120109     C                   WHEN      i = 3
129300120109     C                   SETON                                        77
129400120109     C                   ENDSL
129500120109     C                   MOVEL     '1'           ERRL00
129600120109     C                   MOVEL     MSG(31)       DSMSMS
129700120119     C* se il tipo file / cliente per duplica esiste,
129800120119     C* questo non deve contenere nelle sue coppie per duplica
129900120119     C* il tipo file / cliente in gestione
130000120111     C                   ELSE
130100120119     C*
130200120120     C                   if        TIS723R2U_Ctrl_Chain_Dup(
130300120120     C                              VIDTip       :
130400120120     C                              VIDKsc       :
130500120120     C                              Tips(i)      :
130600120120     C                              Kscs(i)      ) = '1'
130700120120     C                   SELECT
130800120120     C                   WHEN      i = 1
130900120120     C                   SETON                                        7549
131000120120     C                   WHEN      i = 2
131100120120     C                   SETON                                        7650
131200120120     C                   WHEN      i = 3
131300120120     C                   SETON                                        7751
131400120120     C                   ENDSL
131500120120     C                   MOVEL     '1'           ERRL00
131600120120     C                   MOVEL     MSG(32)       DSMSMS
131700120120     C                   ENDIF
131800120120     C***
131900120120     C***                MOVEL     x_VIRPTH      DVIR01
132000120120     C***                IF        §VIR01Tip2 = VIDTip and
132100120120     C***                          §VIR01Ksc2 = VIDKsc     or
132200120120     C***                          §VIR01Tip3 = VIDTip and
132300120120     C***                          §VIR01Ksc3 = VIDKsc     or
132400120120     C***                          §VIR01Tip4 = VIDTip and
132500120120     C***                          §VIR01Ksc4 = VIDKsc
132600120120     C***                SELECT
132700120120     C***                WHEN      i = 1
132800120120     C***                SETON                                        7549
132900120120     C***                WHEN      i = 2
133000120120     C***                SETON                                        7650
133100120120     C***                WHEN      i = 3
133200120120     C***                SETON                                        7751
133300120120     C***                ENDSL
133400120120     C***                MOVEL     '1'           ERRL00
133500120120     C***                MOVEL     MSG(32)       DSMSMS
133600120120     C***                ENDIF
133700120120     C***
133800120111     C                   ENDIF
133900120109     C*
134000120109     C                   ENDSR
134100120109     C*------------------------------------------------------------------------*
134200120109     C* cntksc - Controlla esistenza codice
134300120109     C*------------------------------------------------------------------------*
134400120109     C     cntksc        begsr
134500120109     C*
134600010913     C     vidksc        chain     tianv01l
134700010913     C                   if        %found
134800010913     C                   movel     ANVDES        vddksc
134900010913     C                   else
135000001120     C                   move      vidksc        wcli              7 0
135100991210     C                   CLEAR                   BS69DS
135200991210     C                   CLEAR                   ACODS
135300991210     C                   MOVEL     KNSIF         I69SIF
135400991210     C                   Z-ADD     WCLI          I69KAC
135500991210     C                   CALL      'TIBS69R'
135600991210     C                   PARM                    BS69DS
135700991210     C                   PARM                    ACODS
135800991210     C                   PARM                    INDDS
135900991210     C                   PARM                    CLPDS
136000991210     C                   PARM                    CLSDS
136100991210     C     O69ERR        IFNE      '1'
136200991210     C                   MOVEL     ACORAG        vddksc
136300991210     C                   ELSE
136400991210     C                   MOVEL     *ALL'*'       vddksc
136500010913     C                   MOVE      '1'           errl00
136600010913     C                   MOVEL     msg(11)       dsmsms
136700010913     C                   ENDIF
136800000727     C                   ENDIF
136900010913     C*
137000010913     C                   ENDSR
137100991027     C*------------------------------------------------------------------------*
137200040901     C* cntpoi - Controlla codice filiale
137300991027     C*------------------------------------------------------------------------*
137400001120     C     cntpoi        begsr
137500001120     C*
137600010529     C                   if        vidpoi <> '046' and
137700020809     C                             vidpoi <> '999' and
137800020902     C                             vidpoi <> '500' and
137900020902     C                             vidpoi <> '789'
138000001120     C*
138100001120     C                   move      *blank        vddpoi
138200001120     C     '?'           scan      vidpoi                                 97
138300001120     C                   if        *in97
138400001120     C                   movel     *blank        cod1
138500001120     C                   movel     *blank        des1
138600001120     C                   movel     *blank        pra1
138700001120     C                   call      'TNSD24R'
138800001120     C                   parm                    cod1              3
138900001120     C                   parm                    tip1              1
139000001120     C                   parm                    des1             25
139100001120     C                   parm                    pra1              3
139200001120     C                   movel     cod1          vidpoi
139300001120     C                   endif
139400001120     C                   move      vidpoi        cod1n             3 0
139500001120     C     cod1n         chain     azorg01l                           98
139600001120     C                   if        not *in98 and orgfag <>'V'
139700001120     C                   movel     orgdes        vddpoi
139800001120     C                   else
139900001120     C                   seton                                        69
140000001120     C                   move      '1'           errl00
140100001120     C                   movel     msg(13)       dsmsms
140200001120     C                   endif
140300001120     C*
140400001120     C                   else
140500001120     C                   movel     'SEDE BOLOGNA'vddpoi
140600001120     C                   endif
140700001120     C*
140800001120     C                   endsr
140900991210     C*------------------------------------------------------------------------*
141000991210     C* FINSR - OPERAZIONI FINALI
141100991210     C*------------------------------------------------------------------------*
141200991210     C     FINSR         BEGSR
141300991210     C*
141400991210     C* CHIUDE I FILE DEI PGM CHIAMATI
141500991210     C                   CLEAR                   bs69ds
141600991210     C                   CLEAR                   acods
141700991210     C                   CLEAR                   indds
141800991210     C                   CLEAR                   clpds
141900991210     C                   CLEAR                   clsds
142000991210     C                   MOVEL     'C'           i69tla
142100991210     C                   MOVEL     knsif         i69sif
142200991210     C                   CALL      'TIBS69R'
142300991210     C                   PARM                    bs69ds
142400991210     C                   PARM                    acods
142500991210     C                   PARM                    indds
142600991210     C                   PARM                    clpds
142700991210     C                   PARM                    clsds
142800991210     C*
142900991210     C                   ENDSR
143000991026     C*------------------------------------------------------------------------*
143100991026     C* CHKDA8 - CONTROLLA LA DATA (8)
143200991026     C*------------------------------------------------------------------------*
143300991026     C     CHKDA8        BEGSR
143400991026     C*
143500991026     C     *LIKE         DEFINE    VIDDti        WDA8
143600991026     C     *LIKE         DEFINE    VIDDti        DA8W
143700991026     C*
143800991026     C                   Z-ADD     WDA8          G08DAT
143900991026     C                   Z-ADD     *ZEROS        G08INV
144000991026     C                   MOVEL     '0'           G08ERR
144100991026     C                   CALL      'XSRDA8'
144200991026     C                   PARM                    WLBDA8
144300991026     C                   Z-ADD     G08DAT        WDA8
144400991026     C                   Z-ADD     G08INV        DA8W
144500991026IF  1C     G08ERR        IFEQ      '1'
144600991026     C                   MOVEL     '1'           ERRL00
144700991026E   1C                   ENDIF
144800991026     C*
144900991026     C                   ENDSR
145000970526     C*------------------------------------------------------------------------*
145100970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
145200970526     C*------------------------------------------------------------------------*
145300970526     C     IREVID        BEGSR
145400080604     C*
145500080604     C* REPERISCO INFO DA ESTENSIONE ANAGRAFICA CLIENTI/TIPI FILE => TRC 01
145600080604     C                   IF        §vire01INV = 'S'
145700080604     C                   EVAL      VIDEML = §vire01EML
145800080604     C                   ENDIF
145900970526     C* CAMPI
146000991026     C                   MOVEL     virtip        VIDtip
146100020218     C                   MOVE      virksc        vidksc
146200020218     C                   Z-ADD     virseq        vidseq
146300060308     C                   IF        DEPOP1 = 'O03'
146400060308     C                   MOVEL     *blanks       vidpoi
146500060308     C                   ELSE
146600060308     C                   MOVE      virpoi        vidpoi
146700060308     C                   ENDIF
146800991026     C                   MOVEL     virdes        VIDdes
146900991026     C                   MOVEL     virpgt        VIDpgt
147000991026     C                   MOVEL     virppt        VIDppt
147100991026     C                   MOVEL     virpgi        VIDpgi
147200991026     C                   MOVEL     virppi        VIDppi
147300991026     C                   MOVEL     virfid        VIDfid
147400991026     C                   MOVEL     virfis        VIDfis
147500991026     C                   MOVEL     virfit        VIDfit
147600000710     C                   MOVEL     virfi3        VIDfi3
147700000710     C                   MOVEL     virfi4        VIDfi4
147800010123     C                   MOVEL     virfi2        VIDfi2
147900030526     C                   MOVEL     virpth        DVIR01
148000030526     C                   MOVEL     §VIR01TM      VIDtmbd
148100120109     C                   MOVEL     §VIR01Tip2    VIDTip2
148200120109     C                   MOVEL     §VIR01Tip3    VIDTip3
148300120109     C                   MOVEL     §VIR01Tip4    VIDTip4
148400120119     C                   MOVEL     §VIR01Ksc2    VIDKsc2
148500120119     C                   MOVEL     §VIR01Ksc3    VIDKsc3
148600120119     C                   MOVEL     §VIR01Ksc4    VIDKsc4
148700000710  ***C*                  MOVEL     virctl        VIDctl
148800991026     C                   Z-ADD     virdti        G08INV
148900991026     C                   Z-ADD     *ZEROS        G08DAT
149000991026     C                   MOVEL     '3'           G08ERR
149100991026     C                   CALL      'XSRDA8'
149200991026     C                   PARM                    WLBDA8
149300991026     C                   Z-ADD     G08DAT        viddti
149400970527     C*
149500991026     C                   Z-ADD     virdtf        G08INV
149600991026     C                   Z-ADD     *ZEROS        G08DAT
149700991026     C                   MOVEL     '3'           G08ERR
149800991026     C                   CALL      'XSRDA8'
149900991026     C                   PARM                    WLBDA8
150000991026     C                   Z-ADD     G08DAT        viddtf
150100020218     C                   MOVEL     virfi1        vidisv
150200051122     C*
150300051122     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
150400051122     C                   IF        DEPOP1 = 'F06' OR
150500051122     C                             DEPOP1 = 'O03' OR
150600051122     C                             DEPOP1 = 'O07'
150700051122     C                   EVAL      VIDDTI = DATCORINV
150800060726     C                   EVAL      VIDDTF = 31122039
150900051122     C                   ENDIF
151000020218     C*
151100970526     C                   ENDSR
151200970526     C*------------------------------------------------------------------------*
151300970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
151400970526     C*------------------------------------------------------------------------*
151500970526     C     AZZVID        BEGSR
151600970526     C* CAMPI
151700991026     C                   MOVEL     *blank        VIDksc
151800000710  ***C*                  MOVEL     *blank        VIDpt1
151900000710  ***C*                  MOVEL     *blank        VIDpt2
152000010123     C                   z-add     0             vidseq
152100010123     C                   move      *zeros        vidpoi
152200060726     C                   EVAL      VIDDTI = DATCORINV
152300060726     C                   EVAL      VIDDTF = 31122039
152400991025     C                   MOVEL     *blank        VIDtip
152500991025     C                   MOVEL     *blank        VIDdes
152600991025     C                   MOVEL     *blank        VIDpgt
152700991025     C                   MOVEL     *blank        VIDppt
152800991025     C                   MOVEL     *blank        VIDpgi
152900991025     C                   MOVEL     *blank        VIDppi
153000991025     C                   MOVEL     *blank        VIDfid
153100991025     C                   MOVEL     *blank        VIDfis
153200991025     C                   MOVEL     *blank        VIDfit
153300000710     C                   MOVEL     *blank        VIDfi3
153400000710     C                   MOVEL     *blank        VIDfi4
153500010123     C                   MOVEL     *blank        VIDfi2
153600000710  ***C*                  MOVEL     *blank        VIDctl
153700080603     C                   MOVEL     *blank        VIDeml
153800970526     C*
153900970526     C                   ENDSR
154000970526     C*------------------------------------------------------------------------*
154100991026     C* IMPvir - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
154200970526     C*------------------------------------------------------------------------*
154300991026     C     IMPvir        BEGSR
154400970526     C*
154500991027     C                   move      VIDpoi        virpoi
154600991026     C                   movel     VIDdes        virdes
154700991026     C                   movel     VIDpgt        virpgt
154800991026     C                   movel     VIDppt        virppt
154900991026     C                   movel     VIDpgi        virpgi
155000991026     C                   movel     VIDppi        virppi
155100991026     C                   movel     VIDfid        virfid
155200991026     C                   movel     VIDfis        virfis
155300991026     C                   movel     VIDfit        virfit
155400000710     C                   movel     VIDfi3        virfi3
155500000710     C                   movel     VIDfi4        virfi4
155600010123     C                   movel     VIDfi2        virfi2
155700030526     C                   MOVEL     VIDtmbd       §VIR01TM
155800120109     C                   EVAL      §VIR01Tip2 =  VIDTip2
155900120109     C                   EVAL      §VIR01Tip3 =  VIDTip3
156000120109     C                   EVAL      §VIR01Tip4 =  VIDTip4
156100120119     C                   IF        VIDKsc2 = *zeros
156200120119     C                   EVAL      VIDKsc2 = *blank
156300120119     C                   ENDIF
156400120119     C                   IF        VIDKsc3 = *zeros
156500120119     C                   EVAL      VIDKsc3 = *blank
156600120119     C                   ENDIF
156700120119     C                   IF        VIDKsc4 = *zeros
156800120119     C                   EVAL      VIDKsc4 = *blank
156900120119     C                   ENDIF
157000120119     C                   EVAL      §VIR01Ksc2 =  VIDKsc2
157100120119     C                   EVAL      §VIR01Ksc3 =  VIDKsc3
157200120119     C                   EVAL      §VIR01Ksc4 =  VIDKsc4
157300030526     C                   MOVEL     DVIR01        virpth
157400000710  ***C*                  movel     VIDctl        virctl
157500991026     C                   Z-ADD     viddtf        G08dat
157600991026     C                   Z-ADD     *ZEROS        G08inv
157700991026     C                   MOVEL     '0'           G08ERR
157800991026     C                   CALL      'XSRDA8'
157900991026     C                   PARM                    WLBDA8
158000991026     C                   Z-ADD     G08inv        virdtf
158100000628     C                   movel     VIDisv        virfi1
158200970526     C*
158300970526     C                   ENDSR
158400970526     C*------------------------------------------------------------------------*
158500970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
158600970526     C*------------------------------------------------------------------------*
158700970526     C     IMPFUN        BEGSR
158800101013     C*
158900101026     C                   SETOFF                                       44
159000101026     C                   SETOFF                                       30
159100101026     C                   SETOFF                                       31
159200101026     C                   SETOFF                                       32
159300101026     C                   SETOFF                                       33
159400970526     C*
159500970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
159600970526IF  1C     DEPOP1        IFEQ      'O05'
159700970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
159800970526     C                   MOVEL     'N'           FUDATT
159900970526     C                   MOVEL     FUDRIG        FUD(6)
160000970526E   1C                   ENDIF
160100141111     C*
160200141111     C* TASTO FUNZIONE F02 ATTIVO SOLO PER OPZIONI "DISPOSITIVE"
160300141111     C                   IF        DEPOP1 = 'O02' OR
160400141111     C                             DEPOP1 = 'O03' OR
160500141111     C                             DEPOP1 = 'F06'
160600141111     C                   MOVEL     FUD(2)        FUDRIG                         *PARAMETRI EXT.
160700141111     C                   MOVEL     'S'           FUDATT
160800141111     C                   MOVEL     FUDRIG        FUD(2)
160900141111     C                   ENDIF
161000970526     C*
161100970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
161200970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
161300970526     C                   EXSR      FUNGES
161400970526     C*
161500970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
161600970526     C*---
161700970526     C* 90 -> PROTEZIONE CHIAVE
161800970526     C* 91 -> PROTEZIONE TOTALE
161900970526     C*---
162000970528IF  1C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
162100970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
162200970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
162300970526     C                   SETOFF                                       91
162400970526E   2C                   ENDIF
162500101013IF  2C     DEPOP1        IFEQ      'O03'                                        *INSERIMENTO
162600101013E   2C                   ENDIF
162700970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
162800991014IF  2C     DEPOP1        oreq      'O01'                                        *MODIFICA
162900970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
163000970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
163100970526E   2C                   ENDIF
163200970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
163300970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
163400970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
163500970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
163600970526     C                   SETON                                        91
163700970526E   2C                   ENDIF
163800970526X   1C                   ELSE                                                   *I N T E R R O G A
163900970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
164000970526     C                   SETON                                        91
164100970526E   1C                   ENDIF
164200970526     C*
164300970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
164400970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
164500991025     C                   MOVEL     S20tip        KCEtip
164600991026     C                   MOVEL     S20ksc        KCEksc
164700000420     C     KEYvi2        CHAIN     tivir03L                           9998
164800080603IF  2C     *IN99         IFEQ      *ON
164900970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
165000970526     C                   MOVEL     MSG(6)        DSMSMS
165100970526X   2C                   ELSE
165200080603     C*
165300080603     C* Aggancio anche il record d estensione regole clienti/tipi-file => TRC 01
165400080603     C                   EVAL      KCEdti  = virDTI
165500080603     C                   EVAL      vireTRC = '01'
165600080603     C     KEYvire1_C    CHAIN     tivire1l                           9998
165700080603     C                   IF        %found(tivire1l)
165800080603     C                   EVAL      DVIRE01 = vireFLO
165900080603     C                   ENDIF
166000150114     C*
166100150114     C* Aggancio anche il record d estensione parametri clienti/tipi-file => TRC PE
166200150114     C                   SETOFF                                       71
166300150114     C                   EVAL      KCEdti  = virDTI
166400150114     C                   EVAL      vireTRC = 'PE'
166500150114     C     KEYvire1_C    CHAIN     tivire1l
166600150114     C                   IF        %found(tivire1l)
166700150114     C                   SETON                                        71
166800150114     C                   ENDIF
166900080603     C*
167000080603     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
167100980128IF  3C     DEPOP1        IFEQ      'O05'
167200991014IF  3C     DEPOP1        oreq      'O02'
167300101026     C*
167400101026     C* Se trattasi d tipo servizio "idoneo" reperisco anche i dati utente lato "BARTVAS"
167500101026     C     virFI1        lookup    isvftpeml                              80
167600101026     C                   IF        *IN80
167700111215     C                             and AS400_ok = *on
167800101026      /FREE
167900101026
168000130215       // (questa attività costa risorse, per cui si permette solo in gestione oppure anche in
168100130215       //  visualizzazione se l'utente è di sede;
168200130215       //  testo solo se l'utente è di sede, perché quelli di filiale non possono andare in gestione
168300130215       //  quindi non potrebbero comunque)
168400130215       if DUtLPO = 'S';
168500101026         if UBBVSQL01_RtvCliTipFile(
168600101026                         pInConn
168700101026                       : virKSC
168800101026                       : virTIP
168900101026                       : pOutNumRec
169000101026                       : pOutCliTipTras
169100101026                       : pOutCliMskChk
169200101026                       : pOutCliMskFil
169300101026                       : pOutCliMskSem
169400101026                       : pOutCliPerc
169500101026                       : pOutCliEml1
169600101026                       : pOutCliEml2
169700101026                       : pOutCliEml3
169800101026                       : pOutCliEml4
169900101026                       : pOutCliEml5
170000101026                       : pOutCliSogg
170100101026                       : pOutCliTesto
170200101026                       : pOutCliFtpAddr
170300101026                       : pOutCliFtpDir
170400101026                       : pOutCliFtpUsr
170500101026                       : pOutCliFtpPwd
170600101026                       : pOutCliFtpFAS4
170700101026                       : pOutCliPercOld
170800101026                       : pOutCliTentat
170900101026                       : pOutCliRitardo
171000101026                       : pOutCliLibAS4
171100101026                       : pOutCliFilAS4
171200101026                       : pOutCliMbrAS4
171300101026                       : pOutCliProc
171400101026                       : pOutCliSched
171500101026                       : pOutCliAccorp
171600101026                       : pOutCliMskZip
171700101026                       : pOutCliDataStr
171800101026                       : pOutCliDataEnd
171900101026                       : pOutCliCodComp
172000101026                       : pOutCliFtpCmd1
172100101026                       : pOutCliEmlErr
172200101026                       : pOutCliHtUrlTo
172300101026                       : pOutCliHtUsrTo
172400101026                       : pOutCliHtPwdTo
172500101026                       : pOutCliHtConTy
172600101026                       : pOutCliHtSndEx
172700101026                       : pOutCliHtSndPr
172800101026                       : pOutCliHtUsPxy
172900101026                       : pOutCliUsrIns
173000101026                       : pOutCliUsrUpd
173100101026                       : pOutCliDatUpd
173200101026                       : pOutCliNote
173300101026                       : pOutCliFtpPrt
173400101026                       : pOutCliFtpTy) = 0;
173500101026              if pOutNumRec = 1;
173600101026                 VIDPPIMC = pOutCliMskChk;
173700101026                 if %len(%trim(pOutCliMskChk)) > %size(VIDPPIMC);
173800101026                    *in30 = *on;
173900101026                 endif;
174000101026                 VIDPPIMF = pOutCliMskFil;
174100101026                 if %len(%trim(pOutCliMskFil)) > %size(VIDPPIMF);
174200101026                    *in31 = *on;
174300101026                 endif;
174400101026                 VIDPPIMS = pOutCliMskSem;
174500101026                 if %len(%trim(pOutCliMskSem)) > %size(VIDPPIMS);
174600101026                    *in32 = *on;
174700101026                 endif;
174800101026                 VIDPPIMZ = pOutCliMskZip;
174900101026                 if %len(%trim(pOutCliMskZip)) > %size(VIDPPIMZ);
175000101026                    *in33 = *on;
175100101026                 endif;
175200101026              endif;
175300101026         endif;
175400130214       endif;
175500101026
175600101026      /END-FREE
175700101026     C
175800101026     C                   ELSE
175900101026     C                   SETON                                        44
176000101026     C                   ENDIF
176100101026     C*
176200980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
176300980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
176400980128E   3C                   ENDIF
176500970526E   2C                   ENDIF
176600970526X   1C                   ELSE                                                   *INSERIMENTO
176700970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
176800970526E   1C                   ENDIF
176900970526     C*
177000970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
177100970526IF  1C     DEPOP1        IFEQ      'O02'
177200970526     C                   MOVEL     DEC(1)        VIDFUN
177300970526E   1C                   ENDIF
177400991014IF  1C     DEPOP1        IFEQ      'O03'
177500991014     C                   MOVEL     DEC(6)        VIDFUN
177600991014E   1C                   ENDIF
177700970526IF  1C     DEPOP1        IFEQ      'O04'
177800970526     C                   MOVEL     DEC(2)        VIDFUN
177900970526E   1C                   ENDIF
178000970526IF  1C     DEPOP1        IFEQ      'O05'
178100970526     C                   MOVEL     DEC(3)        VIDFUN
178200970526E   1C                   ENDIF
178300970526IF  1C     DEPOP1        IFEQ      'O07'
178400970526     C                   MOVEL     DEC(4)        VIDFUN
178500970526E   1C                   ENDIF
178600970526IF  1C     DEPOP1        IFEQ      'F06'
178700970526     C                   MOVEL     DEC(5)        VIDFUN
178800970526E   1C                   ENDIF
178900970526     C*
179000970526     C                   ENDSR
179100101026
179200101026
179300101026
179400101026     C*--------------------------------------------------------------------------------------------*
179500101026     C* CARTAB - carica le tabelle occorrenti
179600101026     C*--------------------------------------------------------------------------------------------*
179700101026     C     CARTAB        BEGSR
179800101026     C*
179900101026     C* Tipi servizi soggetti a controllo unificazioni
180000101026     C                   z-add     *zeros        ww                3 0
180100101026     C                   eval      tbecod = 'ISV'
180200101026     C     tbecod        setll     tntbe01l
180300101026     C     tbecod        reade     tntbe01l
180400101026     C                   dow       not %eof(tntbe01l)
180500101026     C                   movel     tbeuni        disv
180600101026     C                   if        §isvproc = 'F'
180700101026     C                   add       1             ww
180800101026     C                   eval      isvftpeml(ww) = tbeke1
180900101026     C                   endif
181000101026     C     tbecod        reade     tntbe01l
181100101026     C                   enddo
181200101026     C*
181300101026     C                   ENDSR
181400101026
181500101026     c*------------------------------------------------------------------------*
181600101026     c* opnCONN - Apertura connessione
181700101026     c*------------------------------------------------------------------------*
181800101026     c     OPNCONN       BEGSR
181900101026      /FREE
182000101026
182100101026        if not *in40;
182200101026           if UBBVSQL01_Connect('BARTVAS' : pOutConn) = 0;
182300101026              pInConn = pOutConn;
182400101026           else;
182500101026              *in40 = *on;
182600101026              DSMSMS = MSG(28);
182700101026           endif;
182800101026        endif;
182900101026
183000101026      /END-FREE
183100101026      *
183200101026     c                   ENDSR
183300101026     c*------------------------------------------------------------------------*
183400101026
183500101026
183600101026
183700101026     c*------------------------------------------------------------------------*
183800101026     c* clsCONN - Chiusura connessione
183900101026     c*------------------------------------------------------------------------*
184000101026     c     CLSCONN       BEGSR
184100101026      /FREE
184200101026
184300101026           if UBBVSQL01_Disconnect(pInConn) = 0;
184400101026           else;
184500101026              *in40 = *on;
184600101026           endif;
184700101026
184800101026      /END-FREE
184900101026      *
185000101026     c                   ENDSR
185100101026     c*------------------------------------------------------------------------*
185200101026
185300101026
185400101026
185500970526     C*------------------------------------------------------------------------*
185600970526     C* *INZSR - OPERAZIONI INIZIALI
185700970526     C*------------------------------------------------------------------------*
185800970526     C     *INZSR        BEGSR
185900970526     C*---
186000970526     C* RICEVIMENTO PARAMETRI
186100970526     C*---
186200970526     C     *ENTRY        PLIST
186300970612     C                   PARM                    KPJBA
186400991028     C                   movel     kpjbu         tis720
186500000420     C*
186600000420     C* Inverto subito la data presa in input
186700000420     C                   Z-ADD     s20dti        wda8
186800000420     C                   Z-ADD     *ZEROS        da8w
186900000420     C                   EXSR      CHKDA8
187000000420     C                   Z-ADD     DA8W          KCEdti
187100130215     C*
187200130215     C     *DTAARA       DEFINE    §azute        azuteds
187300130215     C     *DTAARA       DEFINE    §datiute      ddatiute
187400130215     C                   IN(E)     *DTAARA
187500130215if  1C                   IF        %Error
187600130215     c                   EVAL      I34Tla = 'L'
187700130215     C                   CALL      'TIBS34R'
187800130215     C                   PARM                    Tibs34Ds
187900130215     C                   IN        *DTAARA
188000130215e   1C                   ENDIF
188100970526     C*
188200970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
188300970526     C     XMSGPA        PLIST
188400970526     C                   PARM                    DSMSMS
188500970526     C                   PARM                    DSMSPG
188600970526     C                   PARM                    DSMSKE
188700970526     C                   PARM                    DSMSER
188800991108     C*---
188900970526     C*
189000970526     C* IMPOSTA LA DS -TASTI PREMUTI-
189100970526     C                   CALL      'XKEY'
189200970526     C                   PARM                    DSKEY
189300101026     C*
189400101026     C* CARICA LE TABELLE OCCORRENTI
189500101027     C                   EXSR      CARTAB
189600111215     C*
189700111215     C* Reperisco il sistema AS/400 corrente
189800111215     C                   EVAL      AS400_ok = *off
189900111215     C                   callp     UBRTVNETA_Rtv(currSysNeta)
190000111215     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
190100111215     C                   EVAL      AS400_ok = *on
190200111215     C                   ENDIF
190300101026     C*
190400130214     C* Apro la connessione al DB "BARTVAS" (solo se no solo visualizzazione)
190500130215     C* (questa attività costa risorse, per cui si permette solo in gestione oppure anche in
190600130215     C*  visualizzazione se l'utente è di sede;
190700130215     C*  testo solo se l'utente è di sede, perché quelli di filiale non possono andare in gestione,
190800130215     C*  quindi non potrebbero comunque)
190900130215     C                   IF        DUtLPO = 'S'
191000111215     C                   IF        AS400_ok = *on
191100111215     C                   exsr      opnCONN
191200111215     C                   ENDIF
191300130214     C                   ENDIF
191400970526     C*---
191500970526     C* VARIABILI NON RIFERITE AL DATA BASE
191600970526     C*---
191700970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
191800991025     C     *LIKE         DEFINE    S20OP0        DEPOP0
191900991025     C     *LIKE         DEFINE    S20OP1        DEPOP1
192000991025     C     *LIKE         DEFINE    S20OP0        WOP0
192100991025     C     *LIKE         DEFINE    S20OP1        WOP1
192200970526     C*---
192300970526     C* VARIABILI DI CONTROLLO
192400970526     C*---
192500970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
192600970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
192700970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
192800970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
192900970526     C*
193000970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
193100991026     C                   MOVEL     'TIS723R'     DSMSPG
193200991026     C                   MOVEL     'TIS723R'     MSGQUE
193300970526     C                   MOVEL     *BLANKS       MSGKEY
193400970526     C                   MOVEL     *BLANKS       DSMSMS
193500970526     C*---
193600970526     C* VARIABILI RIFERITE AL DATA BASE
193700970526     C*---
193800991026     C     *LIKE         DEFINE    virtip        KCEtip
193900991026     C     *LIKE         DEFINE    virksc        KCEksc
194000000420     C     *LIKE         DEFINE    virdti        KCEdti
194100970526     C*---
194200970526     C* CHIAVI DI LETTURA
194300970526     C*---
194400000420     C* LETTURA tivir03L - INCOMPLETA
194500991026     C     KEYvir        KLIST
194600080603     C                   KFLD                    KCEtip
194700080603     C                   KFLD                    KCEksc
194800080603     C* LETTURA tivir03L - COMPLETA
194900080603     C     KEYvi2        KLIST
195000080603     C                   KFLD                    KCEtip
195100080603     C                   KFLD                    KCEksc
195200080603     C                   KFLD                    KCEdti
195300080603     C* LETTURA tivire1L - COMPLETA
195400080603     C     KEYvire1_C    KLIST
195500080603     C                   KFLD                    KCEksc
195600080603     C                   KFLD                    KCEtip
195700080603     C                   KFLD                    KCEdti
195800080603     C                   KFLD                    vireTRC
195900080603     C* LETTURA tntbe01L - PARZIALE
196000040816     C     KEYtbe1_P     KLIST
196100040816     C                   KFLD                    TBEcod                         *CODICE TABELLA
196200120111     C* LETTURA tivir06L - COMPLETA
196300120111     C     K03VIR06      KLIST
196400120111     C                   KFLD                    x_VIRKsc
196500120111     C                   KFLD                    x_VIRTip
196600120111     C                   KFLD                    x_VIRDtF
196700120111     C* LETTURA tivir06L - PARZIALE
196800120111     C     K02VIR06      KLIST
196900120111     C                   KFLD                    x_VIRKsc
197000120111     C                   KFLD                    x_VIRTip
197100970528     C*
197200970528     C* CALCOLA LA DATA CORRENTE
197300970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
197400970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
197500970528     C                   Z-ADD     WN8           G08DAT
197600051122     C                   Z-ADD     WN8           DATCORINV         8 0
197700970528     C                   Z-ADD     *ZEROS        G08INV
197800970528     C                   MOVEL     '0'           G08ERR
197900970528     C                   CALL      'XSRDA8'
198000970528     C                   PARM                    WLBDA8
198100970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
198200970526     C*
198300970526     C* DEPOSITA LE OPZIONI DI ENTRATA
198400991025     C                   MOVEL     S20OP0        DEPOP0                         *LIVELLO PROCEDURA
198500991025     C                   MOVEL     S20OP1        DEPOP1                         *LIVELLO PROGRAMMA
198600051122     C*
198700051122     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
198800051122     C                   IF        DEPOP1 = 'F06' OR
198900051122     C                             DEPOP1 = 'O03' OR
199000051122     C                             DEPOP1 = 'O07'
199100051122     C                   EVAL      VIDDTI = DATCORINV
199200060726     C                   EVAL      VIDDTF = 31122039
199300051122     C                   ENDIF
199400000629     C*
199500000629     C* EFFETTUO CONSIDERAZIONI SULLE OPZIONI DI ENTRATA
199600000629     C                   IF        DEPOP1 = 'F06' OR
199700000629     C                             DEPOP1 = 'O03'
199800000629     C                   SETOFF                                       92
199900000629     C                   ELSE
200000000629     C                   SETON                                        92
200100000629     C                   ENDIF
200200970526     C*
200300970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
200400970526     C                   EXSR      IMPFUN
200500970526     C*
200600970526     C                   ENDSR
200700970526     C*------------------------------------------------------------------------*
200800970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
200900970526     C*------------------------------------------------------------------------*
201000970526     C     FUNGES        BEGSR
201100970526     C*
201200970526     C* PULISCO CAMPI DI LAVORO
201300970526     C                   Z-ADD     0             W                 5 0
201400970526     C*
201500970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
201600970526     C* E CARICO TABELLA FUNZIONI ATTIVE
201700970526     C     1             DO        24            I
201800970526     C                   MOVEL     FUD(I)        FUDRIG
201900970526     C*
202000970526     C* PULIZIA FUNZIONI PRECEDENTI
202100970526     C                   MOVEL     *BLANKS       FUV(I)
202200970526     C*
202300970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
202400970526     C* (SALTA F24)
202500970526     C     FUDATT        IFEQ      'S'
202600970526     C     FUDFUN        ANDNE     '24'
202700970526     C                   ADD       1             W
202800970526     C                   MOVEL     FUDDES        FUV(W)
202900970526     C                   END
203000970526     C                   END                                                    DO WHILE
203100970526     C*
203200970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
203300970526     C* ALTRIMENTI DISATTIVO F24
203400970526     C     1             DO        24            I
203500970526     C                   MOVEL     FUD(I)        FUDRIG
203600970526     C*
203700970526     C     FUDFUN        IFEQ      '24'
203800970526     C     W             IFGT      6
203900970526     C                   MOVEL     'S'           FUDATT
204000970526     C                   ELSE
204100970526     C                   MOVEL     *BLANKS       FUDATT
204200970526     C                   END                                                    W GT 8
204300970526     C                   MOVEL     FUDRIG        FUD(I)
204400970526     C                   END
204500970526     C                   END                                                    DO WHILE
204600970526     C*
204700970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
204800970526     C     W             DIV       6             I
204900970526     C                   MVR                     Z
205000970526     C*
205100970526     C     Z             IFNE      0
205200970526     C     I             ADD       1             Z
205300970526     C                   ELSE
205400970526     C                   Z-ADD     I             Z                 5 0
205500970526     C                   END
205600970526     C*
205700970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
205800970526     C                   ADD       1             FUNGRU
205900970526     C     FUNGRU        IFGT      Z
206000970526     C     FUNGRU        ORLT      1
206100970526     C                   Z-ADD     1             FUNGRU
206200970526     C                   END
206300970526     C*
206400970526     C* PRIMO GRUPPO DI 6 FUNZIONI
206500970526     C     FUNGRU        IFEQ      1
206600970526     C                   MOVEL     FUV(1)        FUN001
206700970526     C                   MOVEL     FUV(2)        FUN002
206800970526     C                   MOVEL     FUV(3)        FUN003
206900970526     C                   MOVEL     FUV(4)        FUN004
207000970526     C                   MOVEL     FUV(5)        FUN005
207100970526     C                   MOVEL     FUV(6)        FUN006
207200970526     C                   END
207300970526     C*
207400970526     C* SECONDO GRUPPO DI 6 FUNZIONI
207500970526     C     FUNGRU        IFEQ      2
207600970526     C                   MOVEL     FUV(7)        FUN001
207700970526     C                   MOVEL     FUV(8)        FUN002
207800970526     C                   MOVEL     FUV(9)        FUN003
207900970526     C                   MOVEL     FUV(10)       FUN004
208000970526     C                   MOVEL     FUV(11)       FUN005
208100970526     C                   MOVEL     FUV(12)       FUN006
208200970526     C                   END
208300970526     C*
208400970526     C* TERZO GRUPPO DI 6 FUNZIONI
208500970526     C     FUNGRU        IFEQ      3
208600970526     C                   MOVEL     FUV(13)       FUN001
208700970526     C                   MOVEL     FUV(14)       FUN002
208800970526     C                   MOVEL     FUV(15)       FUN003
208900970526     C                   MOVEL     FUV(16)       FUN004
209000970526     C                   MOVEL     FUV(17)       FUN005
209100970526     C                   MOVEL     FUV(18)       FUN006
209200970526     C                   END
209300970526     C*
209400970526     C* QUARTO GRUPPO DI 6 FUNZIONI
209500970526     C     FUNGRU        IFEQ      4
209600970526     C                   MOVEL     FUV(19)       FUN001
209700970526     C                   MOVEL     FUV(20)       FUN002
209800970526     C                   MOVEL     FUV(21)       FUN003
209900970526     C                   MOVEL     FUV(22)       FUN004
210000970526     C                   MOVEL     *BLANKS       FUN005
210100970526     C                   MOVEL     *BLANKS       FUN006
210200970526     C                   END
210300970526     C*
210400970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
210500970526     C                   MOVEL     *BLANKS       FUN007
210600970526     C                   MOVEL     *BLANKS       FUN008
210700970526     C*
210800970526     C     1             DO        24            I
210900970526     C                   MOVEL     FUD(I)        FUDRIG
211000970526     C     FUDFUN        IFEQ      '24'
211100970526     C     FUDATT        ANDEQ     'S'
211200970526     C                   MOVEL     FUDDES        FUN008
211300970526     C                   END
211400970526     C                   END                                                    DO
211500970526     C*
211600970526     C                   ENDSR
211700970526     C*------------------------------------------------------------------------*
211800970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
211900970526     C*------------------------------------------------------------------------*
212000970526     C     FUNCON        BEGSR
212100970526     C*
212200970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
212300970526     C                   EXSR      FUNVAR
212400970526     C*
212500970526     C* PULISCO CAMPI DI LAVORO
212600970526     C                   Z-ADD     0             I                 5 0
212700970526     C                   Z-ADD     0             W                 5 0
212800970526     C*
212900970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
213000970526     C     FUNKEY        IFNE      *BLANKS
213100970526     C     1             DO        24            I
213200970526     C                   MOVEL     FUD(I)        FUDRIG
213300970526     C*
213400970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
213500970526     C     FUNKEY        IFEQ      FUDFUN
213600970526     C     FUDATT        ANDEQ     'S'
213700970526     C                   Z-ADD     I             W
213800970526     C                   END
213900970526     C                   END                                                    DO WHILE
214000970526     C*
214100970526     C* FUNZIONE NON ATTIVA (ERRORE)
214200970526     C     W             IFEQ      0
214300970526     C*
214400970526     C* EMISSIONE MESSAGGIO DI ERRORE
214500970526     C                   MOVEL     MSG(1)        DSMSMS
214600970526     C*
214700970526     C                   END                                                    W=0
214800970526     C                   END                                                    FUNKEY NE *BLANKS
214900970526     C*
215000970526     C* FUNZIONE ATTIVA (PROCEDI ...)
215100970526     C     W             IFNE      0
215200970526     C     FUNKEY        OREQ      *BLANKS
215300970526     C     FUNKEY        IFEQ      *BLANKS
215400970526     C                   EXSR      ENTGES
215500970526     C                   END
215600031229     C     FUNKEY        IFEQ      '01'
215700031229     C                   EXSR      F01GES
215800031229     C                   END
215900141111     C     FUNKEY        IFEQ      '02'
216000141111     C                   EXSR      F02GES
216100141111     C                   END
216200970526     C     FUNKEY        IFEQ      '03'
216300970526     C                   EXSR      F03GES
216400970526     C                   END
216500970526     C     FUNKEY        IFEQ      '06'
216600970526     C                   EXSR      F06GES
216700970526     C                   END
216800970526     C     FUNKEY        IFEQ      '12'
216900970526     C                   EXSR      F12GES
217000970526     C                   END
217100970526     C     FUNKEY        IFEQ      '24'
217200970526     C                   EXSR      F24GES
217300970526     C                   END
217400970526     C                   END
217500970526     C*
217600970526     C                   ENDSR
217700970526     C*------------------------------------------------------------------------*
217800970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
217900970526     C*------------------------------------------------------------------------*
218000970526     C     FUNVAR        BEGSR
218100970526     C*
218200970526     C* INIZIALIZZA VARIABILE -FUNKEY-
218300970526     C*
218400970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
218500970526     C     KEY           IFEQ      ENTER
218600970526     C                   MOVEL     *BLANKS       FUNKEY
218700970526     C                   END
218800970526     C     KEY           IFEQ      F1
218900970526     C                   MOVEL     '01'          FUNKEY
219000970526     C                   END
219100970526     C     KEY           IFEQ      F2
219200970526     C                   MOVEL     '02'          FUNKEY
219300970526     C                   END
219400970526     C     KEY           IFEQ      F3
219500970526     C                   MOVEL     '03'          FUNKEY
219600970526     C                   END
219700970526     C     KEY           IFEQ      F4
219800970526     C                   MOVEL     '04'          FUNKEY
219900970526     C                   END
220000970526     C     KEY           IFEQ      F5
220100970526     C                   MOVEL     '05'          FUNKEY
220200970526     C                   END
220300970526     C     KEY           IFEQ      F6
220400970526     C                   MOVEL     '06'          FUNKEY
220500970526     C                   END
220600970526     C     KEY           IFEQ      F7
220700970526     C                   MOVEL     '07'          FUNKEY
220800970526     C                   END
220900970526     C     KEY           IFEQ      F8
221000970526     C                   MOVEL     '08'          FUNKEY
221100970526     C                   END
221200970526     C     KEY           IFEQ      F9
221300970526     C                   MOVEL     '09'          FUNKEY
221400970526     C                   END
221500970526     C     KEY           IFEQ      F10
221600970526     C                   MOVEL     '10'          FUNKEY
221700970526     C                   END
221800970526     C     KEY           IFEQ      F11
221900970526     C                   MOVEL     '11'          FUNKEY
222000970526     C                   END
222100970526     C     KEY           IFEQ      F12
222200970526     C                   MOVEL     '12'          FUNKEY
222300970526     C                   END
222400970526     C     KEY           IFEQ      F13
222500970526     C                   MOVEL     '13'          FUNKEY
222600970526     C                   END
222700970526     C     KEY           IFEQ      F14
222800970526     C                   MOVEL     '14'          FUNKEY
222900970526     C                   END
223000970526     C     KEY           IFEQ      F15
223100970526     C                   MOVEL     '15'          FUNKEY
223200970526     C                   END
223300970526     C     KEY           IFEQ      F16
223400970526     C                   MOVEL     '16'          FUNKEY
223500970526     C                   END
223600970526     C     KEY           IFEQ      F17
223700970526     C                   MOVEL     '17'          FUNKEY
223800970526     C                   END
223900970526     C     KEY           IFEQ      F18
224000970526     C                   MOVEL     '18'          FUNKEY
224100970526     C                   END
224200970526     C     KEY           IFEQ      F19
224300970526     C                   MOVEL     '19'          FUNKEY
224400970526     C                   END
224500970526     C     KEY           IFEQ      F20
224600970526     C                   MOVEL     '20'          FUNKEY
224700970526     C                   END
224800970526     C     KEY           IFEQ      F21
224900970526     C                   MOVEL     '21'          FUNKEY
225000970526     C                   END
225100970526     C     KEY           IFEQ      F22
225200970526     C                   MOVEL     '22'          FUNKEY
225300970526     C                   END
225400970526     C     KEY           IFEQ      F23
225500970526     C                   MOVEL     '23'          FUNKEY
225600970526     C                   END
225700970526     C     KEY           IFEQ      F24
225800970526     C                   MOVEL     '24'          FUNKEY
225900970526     C                   END
226000970526     C     KEY           IFEQ      ROLLUP
226100970526     C                   MOVEL     'RU'          FUNKEY
226200970526     C                   END
226300970526     C     KEY           IFEQ      HELP
226400970526     C                   MOVEL     'HP'          FUNKEY
226500970526     C                   END
226600970526     C                   ENDSR
226700970526     C*------------------------------------------------------------------------*
226800970526     C* NOTE COSTRUZIONE SCHIERE
226900970526     C*------------------------------------------------------------------------*
227000970526     C*
227100970526     C* -MSG-MESSAGGI
227200970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
227300970526     C* (NON TOCCARE MSG,1 E MSG,2)
227400970526     C*
227500970526     C* -CMD-COMANDI
227600970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
227700970526     C*
227800970526     C* -OPD-OPZIONI
227900970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
228000970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
228100970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
228200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
228300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
228400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
228500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
228600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
228700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
228800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
228900970526     C*
229000970526     C* -FUD-FUNZIONI
229100970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
229200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
229300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
229400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
229500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
229600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
229700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
229800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
229900970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
230000970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
230100970526     O*------------------------------------------------------------------------*
230200970526** DEC - DECODIFICHE
230300970526    MODIFICA                                                      1
230400970526  ANNULLAMENTO                                                    2
230500970526VISUALIZZAZIONE                                                   3
230600970526   RIPRISTINO                                                     4
230700970526  INSERIMENTO                                                     5
230800991014     COPIA                                                        6
230900970526** MSG - MESSAGGI
231000991026TIS723R- Tasto funzionale non gestito                                          1
231100991026TIS723R- Inserimento effettuato                                                2
231200991026TIS723R- Modifica effettuata                                                   3
231300991026TIS723R- Annullamento effettuato                                               4
231400991026TIS723R- Ripristino effettuato                                                 5
231500991026TIS723R- Richiesta funzione per record inesistente                             6
231600991026TIS723R- Record momentaneamente allocato: riprovare più tardi                  7
231700991026TIS723R- Richiesta funzione per record esistente                               8
231800991026TIS723R- Questo codice cliente è già presente con questo tipo file             9
231900991027TIS723R- Il codice tipo file non è fra quelli presenti in anagrafico           10
232000991027TIS723R- Il codice cliente non è presente come padre nell'archivio UNIFICANTI  11
232100000724TIS723R- Campo obbligatorio                                                    12
232200991027TIS723R- Codice filiale inesistente o di categoria V=varia                     13
232300000724TIS723R- Data errata                                                           14
232400000724TIS723R- La data fine deve essere maggiore della data inizio                   15
232500000724TIS723R- Il record non può scadere in un giorno già trascorso                  16
232600000724TIS723R- Il record immesso si sovrappone ad uno già in decorrenza              17
232700000724TIS723R- Codice tipo servizio non presente in tabella                          18
232800001120TIS723R- Oggetto inesistente, verificare e correggere.                         19
232900001120TIS723R- File inesistente, verificare e correggere.                            20
233000001120TIS723R- Libreria inesistente, verificare e correggere.                        21
233100030603TIS723R- Tipo membro non impostabile x il tipo servizio indicato.              22
233200040107TIS723R- Per il traduttore indicato non esiste descrizione dettaglio parametri 23
233300040319TIS723R- Tipo file e servizio incompatibili, correggere.                       24
233400040816TIS723R- Tipo file e/o programma traduttore e/o file tradotto incompatibili.   25
233500051026TIS723R- ATTENZIONE presenti log a stato 0 impossibile cambiare traduttore!    26
233600060616TIS723R- Valori incompatibili, correggere!                                     27
233700101026TIS723R- Errore BartVAS: fatal error                                           28
233800101027TIS723R- ATTENZIONE: modifica effettuata lato AS/400 ma non lato BARTVAS       29
233900120109TIS723R- Tipo file per Internet non impostabile x il tipo servizio indicato.   30
234000120119TIS723R- Tipo file non abilitato per cliente indicato                          31
234100120119TIS723R- Loop tra il tipo file / cliente corrente e quello per duplica         32
234200120119TIS723R- Tipo file / cliente per duplica non possono essere quelli correnti    33
234300120119TIS723R- Valorizzato il cliente per duplica ma non il rispettivo tipo file     34
234400970526** FUD - FUNZIONI
234500141111F01=Desc.Parametri -        (S/N)=S  <== S-FISSO           funz.01¬
234600141111F02=Parametri      -        (S/N)=N                        funz.02¬
234700970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
234800970526                   -        (S/N)=N                        funz.04¬
234900970526                   -        (S/N)=N                        funz.05¬
235000970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
235100970527                   -        (S/N)=N                        funz.07¬
235200970527                   -        (S/N)=N                        funz.08¬
235300970527                   -        (S/N)=N                        funz.09¬
235400970527                   -        (S/N)=N                        funz.10¬
235500970526                   -        (S/N)=N                        funz.11¬
235600970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
235700970526                   -        (S/N)=N                        funz.13¬
235800970526                   -        (S/N)=N                        funz.14¬
235900970526                   -        (S/N)=N                        funz.15¬
236000970526                   -        (S/N)=N                        funz.16¬
236100970526                   -        (S/N)=N                        funz.17¬
236200970526                   -        (S/N)=N                        funz.18¬
236300970526                   -        (S/N)=N                        funz.19¬
236400970526                   -        (S/N)=N                        funz.20¬
236500970526                   -        (S/N)=N                        funz.21¬
236600970526                   -        (S/N)=N                        funz.22¬
236700970526                   -        (S/N)=N                        funz.23¬
236800970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
