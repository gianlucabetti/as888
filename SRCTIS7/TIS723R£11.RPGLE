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
002900170119     D MSG             S             78    DIM(37) CTDATA PERRCD(1)
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
108300170119     C*--------------------
108400170119     C* Controllo particolare x anagrafiche d download:
108500170119     C* - se indicati parametri di traduzione      => obbligatorio *pgm traduttore
108600170119     C* - se indicati parametri di post-traduzione => obbligatorio *pgm post-traduzione
108700170119     C* - se indicatp *pgm traduttore              => obbligatorio file tradotto
108800170119     C*--------------------
108900170119     C     ERRL00        IFEQ      '0'
109000170119     C     §ISVVers      IFEQ      'D'
109100170119     C                   IF        VIDppt <> *blanks AND VIDpgt = *blanks
109200170119     C                   MOVEL     '1'           ERRL00
109300170119     C                   MOVEL     MSG(35)       DSMSMS
109400170119     C                   SETON                                        54
109500170119     C                   ENDIF
109600170119     C*
109700170119     C                   IF        VIDpgt <> *blanks AND VIDfit = *blanks
109800170119     C                   MOVEL     '1'           ERRL00
109900170119     C                   MOVEL     MSG(36)       DSMSMS
110000170119     C                   SETON                                        57
110100170119     C                   ENDIF
110200170119     C*
110300170119     C                   IF        VIDppi <> *blanks AND VIDpgi = *blanks
110400170119     C                   MOVEL     '1'           ERRL00
110500170119     C                   MOVEL     MSG(37)       DSMSMS
110600170119     C                   SETON                                        55
110700170119     C                   ENDIF
110800170119     C*
110900170119     C                   ENDIF
111000170119     C                   ENDIF
111100051026     C*--------------------
111200051026     C* Controllo particolare x anagrafiche d download: se si sta cambiando il traduttore
111300051026     C* verifico prima che nn vi siano attualmente record d log a stato zero
111400051026     C*--------------------
111500051026     C     ERRL00        IFEQ      '0'
111600120109     C     §ISVVers      IFEQ      'D'
111700051026     C     VIDpgt        IFNE      virPGT
111800051026     C     VIDpgi        ORNE      virPGI
111900051026     C                   CALL      'TIS723R1'
112000051026     C                   PARM                    VIDKSC
112100051026     C                   PARM                    VIDTIP
112200120109     C                   PARM                    §ISVVers
112300051026     C                   PARM      *zeros        wRecZero          9 0
112400051026     C                   IF        wRecZero > *zeros
112500051026     C                   MOVEL     '1'           ERRL00
112600051026     C                   MOVEL     MSG(26)       DSMSMS
112700051026     C                   SETON                                        5455
112800051026     C                   ENDIF
112900051026     C                   ENDIF
113000051026     C                   ENDIF
113100051026     C                   ENDIF
113200060616     C*--------------------
113300060616     C* File Da tradurre / File Storico / Programma Post-Traduzione
113400060616     C*--------------------
113500060616     C     ERRL00        IFEQ      '0'
113600060616     C                   IF        (VIDfid='*' AND VIDfis='*' AND VIDpgi='*') OR
113700060616     C                             (VIDfid<>'*' AND VIDfis<>'*' AND VIDpgi<>'*')
113800060616     C                   ELSE
113900060616     C                   MOVEL     '1'           ERRL00
114000060616     C                   SETON                                        535556
114100060616     C                   MOVEL     MSG(27)       DSMSMS
114200060616     C                   ENDIF
114300060616     C                   ENDIF
114400081020     C*--------------------
114500081020     C* Mail x eventuale feed-back richiesto
114600081020     C*--------------------
114700081020     C     ERRL00        IFEQ      '0'
114800081020     C                   CLEAR                   DSEMAIL
114900081020     C                   EVAL      §EMLINDI = VIDEML
115000081020     C                   CALL      'XEMAIL'
115100081020     C                   PARM                    DSEMAIL
115200081020     C                   IF        §EMLERRO = '1'
115300081020     C                   MOVEL     '1'           ERRL00
115400081020     C                   SETON                                        64
115500081020     C                   MOVEL     §EMLMSGO      DSMSMS
115600081020     C                   ENDIF
115700081020     C                   ENDIF
115800001120     C*
115900001120     C                   ENDSR
116000000626     C*------------------------------------------------------------------------*
116100000626     C* Cntisv - CONTROLLA codice tipo legame
116200000626     C*------------------------------------------------------------------------*
116300001120     C     cntisv        begsr
116400001120     C                   clear                   TIBS02DS
116500001120     C*
116600101027     C     '?'           SCAN      Vidisv                                 28
116700001120     C*  Ricerca
116800001120     C                   eval      t02tla = 'L'
116900101027     C                   IF        *IN28 = *ON
117000001120     C                   movel     'R'           t02mod
117100001120     C                   else
117200001120     C                   movel     'C'           t02mod
117300001120     C                   movel(p)  vidisv        t02ke1
117400001120     C                   end
117500000626     C                   movel     'ISV'         t02cod
117600001120     C                   movel     knsif         t02sif
117700001120     C                   CALL      'TIBS02R'
117800001120     C                   parm                    KPJBA
117900001120     C                   parm                    TIBS02DS
118000001120     C
118100001120     C*
118200001120     C                   IF        T02err = *blanks
118300000626     C                   movel     T02ke1        Vidisv
118400000626     C                   movel     T02uni        vddisv
118500030603     C                   movel(P)  T02uni        DISV
118600001120     C                   else
118700001120     C                   movel     '1'           errl00
118800001120     C                   seton                                        70
118900001120     C                   MOVEL     MSG(18)       DSMSMS
119000001120     C                   ENDIF
119100001120     C
119200001120     C                   endsr
119300991027     C*------------------------------------------------------------------------*
119400991027     C* cnttip - Controlla esistenza tipo file su tivt00f
119500991027     C*------------------------------------------------------------------------*
119600001120     C     cnttip        begsr
119700001120     C                   clear                   TIBS02DS
119800000626     C*
119900101027     C     '?'           SCAN      vidtip                                 28
120000001120     C*  Ricerca
120100001120     C                   eval      t02tla = 'L'
120200101027     C                   IF        *IN28 = *ON
120300001120     C                   movel     'R'           t02mod
120400001120     C                   else
120500001120     C                   movel     'C'           t02mod
120600001120     C                   movel(p)  vidtip        t02ke1
120700001120     C                   end
120800991108     C                   movel     'ITF'         t02cod
120900001120     C                   movel     knsif         t02sif
121000991108     C                   CALL      'TIBS02R'
121100001120     C                   parm                    KPJBA
121200001120     C                   parm                    TIBS02DS
121300001120     C
121400001120     C*
121500001120     C                   IF        T02err = *blanks
121600991108     C                   movel     T02ke1        vidtip
121700020218     C                   movel     T02uni        DITF
121800020218     C                   movel     §ITFDESC      vddtip
121900021003     C* Sostituisco con il dato proveniente dalla tabella solo se nn già indicato
122000021003     C* qualcosa nelle regole specifiche x il cliente/tipo file.
122100021014     C                   if        vidfit = *blanks
122200021014     C                   movel     §ITFLIBD      vidfi2
122300021014     C                   endif
122400001120     C                   else
122500001120     C                   movel     '1'           errl00
122600991108     C                   movel     *blank        vddtip
122700111215     C* pulisco anche il cod. tabella associata
122800111215     C                   movel     *blank        vidtab
122900001120     C                   seton                                        66
123000991108     C                   MOVEL     msg(11)       DSMSMS
123100991108     C                   ENDIF
123200001120     C                   if        errl00 = '0'
123300120109     C     vidtip        chain     tivtf01l                           97
123400001120     C                   if        not *in97
123500001120     C                   movel     vtfpgt        vidpgtd
123600001120     C                   movel     vtfpgi        vidpgid
123700001120     C                   movel     vtffid        vidfidd
123800001120     C                   movel     vtffis        vidfisd
123900001120     C                   movel     vtffit        vidfitd
124000111214     C                   movel     vtffi1        vidtab
124100101013     C                   movel     vtfppi        DVTFPPI
124200101026     C                   if        DEPOP1 = 'F06' OR
124300101026     C                             DEPOP1 = 'O03'
124400101026     C                   select
124500101026     C                   when      vtfUD = 'U'
124600101026     C                   movel     D§VTFPPIMF    vidppiMC
124700101026     C                   when      vtfUD = 'D'
124800101026     C                   movel     D§VTFPPIMF    vidppiMF
124900101026     C                   endsl
125000101026     C                   movel     D§VTFPPIMS    vidppiMS
125100101026     C                   movel     D§VTFPPIMZ    vidppiMZ
125200101026     C                   endif
125300100308     C                   if        vidfi4d <> *blanks and vidfi4d <> '*'
125400001120     C                   eval      vidfi4d = %subst(vidfitd:1:5) + '00F'
125500100308     C                   endif
125600001120     C                   else
125700001120     C                   movel     *blank        vidpgtd
125800001120     C                   movel     *blank        vidpgid
125900001120     C                   movel     *blank        vidfidd
126000001120     C                   movel     *blank        vidfisd
126100001120     C                   movel     *blank        vidfitd
126200001120     C                   movel     *blank        vidfi4d
126300001120     C                   endif
126400001120     C                   endif
126500001120     C*
126600001120     C                   endsr
126700991027     C*------------------------------------------------------------------------*
126800120109     C* CntTip234 - Controlla esistenza altri tipi file su tivt00f
126900991027     C*------------------------------------------------------------------------*
127000120109     C     CntTip234     BEGSR
127100120119     C*
127200120119     C* se ho indicato un cliente devo indicare il tipo file
127300120119     C                   IF        Kscs(i) <>*blank and
127400120119     C                             Kscs(i) <>*zeros and
127500120119     C                             Tips(i) = *blank
127600120119     C                   SELECT
127700120119     C                   WHEN      i = 1
127800120119     C                   SETON                                        7549
127900120119     C                   WHEN      i = 2
128000120119     C                   SETON                                        7650
128100120119     C                   WHEN      i = 3
128200120119     C                   SETON                                        7751
128300120119     C                   ENDSL
128400120119     C                   MOVEL     '1'           ERRL00
128500120119     C                   MOVEL     MSG(34)       DSMSMS
128600120119     C                   LEAVESR
128700120119     C                   ENDIF
128800010913     C*
128900120119     C* il tipo file / cliente indicati non devono essere quelli in gestione
129000120119     C                   IF        Tips(i) = VIDTip and
129100120119     C                             Kscs(i) = VIDKsc
129200120111     C                   SELECT
129300120111     C                   WHEN      i = 1
129400120119     C                   SETON                                        7549
129500120111     C                   WHEN      i = 2
129600120119     C                   SETON                                        7650
129700120111     C                   WHEN      i = 3
129800120119     C                   SETON                                        7751
129900120111     C                   ENDSL
130000120111     C                   MOVEL     '1'           ERRL00
130100120111     C                   MOVEL     MSG(33)       DSMSMS
130200120111     C                   LEAVESR
130300120111     C                   ENDIF
130400120111     C*
130500120111     C* il tipo file indicato deve esistere ed essere abilitato su questa anagrafica per il cliente
130600120119     C* indicato
130700120111     C* (per abilitato basta che la data fine validità sia maggiore di oggi; se anche la data inizio
130800120111     C*  è maggiore di oggi non importa, sarà valido nel futuro)
130900120119     C                   EVAL      x_VIRKsc = Kscs(i)
131000120111     C                   EVAL      x_VIRTip = Tips(i)
131100120111     C                   EVAL      x_VIRDtF = %dec(%date() : *ISO)
131200120111     C     K03VIR06      SETLL     TIVIR06L
131300120111     C     K02VIR06      READE     TIVIR06L
131400120111     C                   if        %eof
131500120109     C                   SELECT
131600120109     C                   WHEN      i = 1
131700120109     C                   SETON                                        75
131800120109     C                   WHEN      i = 2
131900120109     C                   SETON                                        76
132000120109     C                   WHEN      i = 3
132100120109     C                   SETON                                        77
132200120109     C                   ENDSL
132300120109     C                   MOVEL     '1'           ERRL00
132400120109     C                   MOVEL     MSG(31)       DSMSMS
132500120119     C* se il tipo file / cliente per duplica esiste,
132600120119     C* questo non deve contenere nelle sue coppie per duplica
132700120119     C* il tipo file / cliente in gestione
132800120111     C                   ELSE
132900120119     C*
133000120120     C                   if        TIS723R2U_Ctrl_Chain_Dup(
133100120120     C                              VIDTip       :
133200120120     C                              VIDKsc       :
133300120120     C                              Tips(i)      :
133400120120     C                              Kscs(i)      ) = '1'
133500120120     C                   SELECT
133600120120     C                   WHEN      i = 1
133700120120     C                   SETON                                        7549
133800120120     C                   WHEN      i = 2
133900120120     C                   SETON                                        7650
134000120120     C                   WHEN      i = 3
134100120120     C                   SETON                                        7751
134200120120     C                   ENDSL
134300120120     C                   MOVEL     '1'           ERRL00
134400120120     C                   MOVEL     MSG(32)       DSMSMS
134500120120     C                   ENDIF
134600120120     C***
134700120120     C***                MOVEL     x_VIRPTH      DVIR01
134800120120     C***                IF        §VIR01Tip2 = VIDTip and
134900120120     C***                          §VIR01Ksc2 = VIDKsc     or
135000120120     C***                          §VIR01Tip3 = VIDTip and
135100120120     C***                          §VIR01Ksc3 = VIDKsc     or
135200120120     C***                          §VIR01Tip4 = VIDTip and
135300120120     C***                          §VIR01Ksc4 = VIDKsc
135400120120     C***                SELECT
135500120120     C***                WHEN      i = 1
135600120120     C***                SETON                                        7549
135700120120     C***                WHEN      i = 2
135800120120     C***                SETON                                        7650
135900120120     C***                WHEN      i = 3
136000120120     C***                SETON                                        7751
136100120120     C***                ENDSL
136200120120     C***                MOVEL     '1'           ERRL00
136300120120     C***                MOVEL     MSG(32)       DSMSMS
136400120120     C***                ENDIF
136500120120     C***
136600120111     C                   ENDIF
136700120109     C*
136800120109     C                   ENDSR
136900120109     C*------------------------------------------------------------------------*
137000120109     C* cntksc - Controlla esistenza codice
137100120109     C*------------------------------------------------------------------------*
137200120109     C     cntksc        begsr
137300120109     C*
137400010913     C     vidksc        chain     tianv01l
137500010913     C                   if        %found
137600010913     C                   movel     ANVDES        vddksc
137700010913     C                   else
137800001120     C                   move      vidksc        wcli              7 0
137900991210     C                   CLEAR                   BS69DS
138000991210     C                   CLEAR                   ACODS
138100991210     C                   MOVEL     KNSIF         I69SIF
138200991210     C                   Z-ADD     WCLI          I69KAC
138300991210     C                   CALL      'TIBS69R'
138400991210     C                   PARM                    BS69DS
138500991210     C                   PARM                    ACODS
138600991210     C                   PARM                    INDDS
138700991210     C                   PARM                    CLPDS
138800991210     C                   PARM                    CLSDS
138900991210     C     O69ERR        IFNE      '1'
139000991210     C                   MOVEL     ACORAG        vddksc
139100991210     C                   ELSE
139200991210     C                   MOVEL     *ALL'*'       vddksc
139300010913     C                   MOVE      '1'           errl00
139400010913     C                   MOVEL     msg(11)       dsmsms
139500010913     C                   ENDIF
139600000727     C                   ENDIF
139700010913     C*
139800010913     C                   ENDSR
139900991027     C*------------------------------------------------------------------------*
140000040901     C* cntpoi - Controlla codice filiale
140100991027     C*------------------------------------------------------------------------*
140200001120     C     cntpoi        begsr
140300001120     C*
140400010529     C                   if        vidpoi <> '046' and
140500020809     C                             vidpoi <> '999' and
140600020902     C                             vidpoi <> '500' and
140700020902     C                             vidpoi <> '789'
140800001120     C*
140900001120     C                   move      *blank        vddpoi
141000001120     C     '?'           scan      vidpoi                                 97
141100001120     C                   if        *in97
141200001120     C                   movel     *blank        cod1
141300001120     C                   movel     *blank        des1
141400001120     C                   movel     *blank        pra1
141500001120     C                   call      'TNSD24R'
141600001120     C                   parm                    cod1              3
141700001120     C                   parm                    tip1              1
141800001120     C                   parm                    des1             25
141900001120     C                   parm                    pra1              3
142000001120     C                   movel     cod1          vidpoi
142100001120     C                   endif
142200001120     C                   move      vidpoi        cod1n             3 0
142300001120     C     cod1n         chain     azorg01l                           98
142400001120     C                   if        not *in98 and orgfag <>'V'
142500001120     C                   movel     orgdes        vddpoi
142600001120     C                   else
142700001120     C                   seton                                        69
142800001120     C                   move      '1'           errl00
142900001120     C                   movel     msg(13)       dsmsms
143000001120     C                   endif
143100001120     C*
143200001120     C                   else
143300001120     C                   movel     'SEDE BOLOGNA'vddpoi
143400001120     C                   endif
143500001120     C*
143600001120     C                   endsr
143700991210     C*------------------------------------------------------------------------*
143800991210     C* FINSR - OPERAZIONI FINALI
143900991210     C*------------------------------------------------------------------------*
144000991210     C     FINSR         BEGSR
144100991210     C*
144200991210     C* CHIUDE I FILE DEI PGM CHIAMATI
144300991210     C                   CLEAR                   bs69ds
144400991210     C                   CLEAR                   acods
144500991210     C                   CLEAR                   indds
144600991210     C                   CLEAR                   clpds
144700991210     C                   CLEAR                   clsds
144800991210     C                   MOVEL     'C'           i69tla
144900991210     C                   MOVEL     knsif         i69sif
145000991210     C                   CALL      'TIBS69R'
145100991210     C                   PARM                    bs69ds
145200991210     C                   PARM                    acods
145300991210     C                   PARM                    indds
145400991210     C                   PARM                    clpds
145500991210     C                   PARM                    clsds
145600991210     C*
145700991210     C                   ENDSR
145800991026     C*------------------------------------------------------------------------*
145900991026     C* CHKDA8 - CONTROLLA LA DATA (8)
146000991026     C*------------------------------------------------------------------------*
146100991026     C     CHKDA8        BEGSR
146200991026     C*
146300991026     C     *LIKE         DEFINE    VIDDti        WDA8
146400991026     C     *LIKE         DEFINE    VIDDti        DA8W
146500991026     C*
146600991026     C                   Z-ADD     WDA8          G08DAT
146700991026     C                   Z-ADD     *ZEROS        G08INV
146800991026     C                   MOVEL     '0'           G08ERR
146900991026     C                   CALL      'XSRDA8'
147000991026     C                   PARM                    WLBDA8
147100991026     C                   Z-ADD     G08DAT        WDA8
147200991026     C                   Z-ADD     G08INV        DA8W
147300991026IF  1C     G08ERR        IFEQ      '1'
147400991026     C                   MOVEL     '1'           ERRL00
147500991026E   1C                   ENDIF
147600991026     C*
147700991026     C                   ENDSR
147800970526     C*------------------------------------------------------------------------*
147900970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
148000970526     C*------------------------------------------------------------------------*
148100970526     C     IREVID        BEGSR
148200080604     C*
148300080604     C* REPERISCO INFO DA ESTENSIONE ANAGRAFICA CLIENTI/TIPI FILE => TRC 01
148400080604     C                   IF        §vire01INV = 'S'
148500080604     C                   EVAL      VIDEML = §vire01EML
148600080604     C                   ENDIF
148700970526     C* CAMPI
148800991026     C                   MOVEL     virtip        VIDtip
148900020218     C                   MOVE      virksc        vidksc
149000020218     C                   Z-ADD     virseq        vidseq
149100060308     C                   IF        DEPOP1 = 'O03'
149200060308     C                   MOVEL     *blanks       vidpoi
149300060308     C                   ELSE
149400060308     C                   MOVE      virpoi        vidpoi
149500060308     C                   ENDIF
149600991026     C                   MOVEL     virdes        VIDdes
149700991026     C                   MOVEL     virpgt        VIDpgt
149800991026     C                   MOVEL     virppt        VIDppt
149900991026     C                   MOVEL     virpgi        VIDpgi
150000991026     C                   MOVEL     virppi        VIDppi
150100991026     C                   MOVEL     virfid        VIDfid
150200991026     C                   MOVEL     virfis        VIDfis
150300991026     C                   MOVEL     virfit        VIDfit
150400000710     C                   MOVEL     virfi3        VIDfi3
150500000710     C                   MOVEL     virfi4        VIDfi4
150600010123     C                   MOVEL     virfi2        VIDfi2
150700030526     C                   MOVEL     virpth        DVIR01
150800030526     C                   MOVEL     §VIR01TM      VIDtmbd
150900120109     C                   MOVEL     §VIR01Tip2    VIDTip2
151000120109     C                   MOVEL     §VIR01Tip3    VIDTip3
151100120109     C                   MOVEL     §VIR01Tip4    VIDTip4
151200120119     C                   MOVEL     §VIR01Ksc2    VIDKsc2
151300120119     C                   MOVEL     §VIR01Ksc3    VIDKsc3
151400120119     C                   MOVEL     §VIR01Ksc4    VIDKsc4
151500000710  ***C*                  MOVEL     virctl        VIDctl
151600991026     C                   Z-ADD     virdti        G08INV
151700991026     C                   Z-ADD     *ZEROS        G08DAT
151800991026     C                   MOVEL     '3'           G08ERR
151900991026     C                   CALL      'XSRDA8'
152000991026     C                   PARM                    WLBDA8
152100991026     C                   Z-ADD     G08DAT        viddti
152200970527     C*
152300991026     C                   Z-ADD     virdtf        G08INV
152400991026     C                   Z-ADD     *ZEROS        G08DAT
152500991026     C                   MOVEL     '3'           G08ERR
152600991026     C                   CALL      'XSRDA8'
152700991026     C                   PARM                    WLBDA8
152800991026     C                   Z-ADD     G08DAT        viddtf
152900020218     C                   MOVEL     virfi1        vidisv
153000051122     C*
153100051122     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
153200051122     C                   IF        DEPOP1 = 'F06' OR
153300051122     C                             DEPOP1 = 'O03' OR
153400051122     C                             DEPOP1 = 'O07'
153500051122     C                   EVAL      VIDDTI = DATCORINV
153600060726     C                   EVAL      VIDDTF = 31122039
153700051122     C                   ENDIF
153800020218     C*
153900970526     C                   ENDSR
154000970526     C*------------------------------------------------------------------------*
154100970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
154200970526     C*------------------------------------------------------------------------*
154300970526     C     AZZVID        BEGSR
154400970526     C* CAMPI
154500991026     C                   MOVEL     *blank        VIDksc
154600000710  ***C*                  MOVEL     *blank        VIDpt1
154700000710  ***C*                  MOVEL     *blank        VIDpt2
154800010123     C                   z-add     0             vidseq
154900010123     C                   move      *zeros        vidpoi
155000060726     C                   EVAL      VIDDTI = DATCORINV
155100060726     C                   EVAL      VIDDTF = 31122039
155200991025     C                   MOVEL     *blank        VIDtip
155300991025     C                   MOVEL     *blank        VIDdes
155400991025     C                   MOVEL     *blank        VIDpgt
155500991025     C                   MOVEL     *blank        VIDppt
155600991025     C                   MOVEL     *blank        VIDpgi
155700991025     C                   MOVEL     *blank        VIDppi
155800991025     C                   MOVEL     *blank        VIDfid
155900991025     C                   MOVEL     *blank        VIDfis
156000991025     C                   MOVEL     *blank        VIDfit
156100000710     C                   MOVEL     *blank        VIDfi3
156200000710     C                   MOVEL     *blank        VIDfi4
156300010123     C                   MOVEL     *blank        VIDfi2
156400000710  ***C*                  MOVEL     *blank        VIDctl
156500080603     C                   MOVEL     *blank        VIDeml
156600970526     C*
156700970526     C                   ENDSR
156800970526     C*------------------------------------------------------------------------*
156900991026     C* IMPvir - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
157000970526     C*------------------------------------------------------------------------*
157100991026     C     IMPvir        BEGSR
157200970526     C*
157300991027     C                   move      VIDpoi        virpoi
157400991026     C                   movel     VIDdes        virdes
157500991026     C                   movel     VIDpgt        virpgt
157600991026     C                   movel     VIDppt        virppt
157700991026     C                   movel     VIDpgi        virpgi
157800991026     C                   movel     VIDppi        virppi
157900991026     C                   movel     VIDfid        virfid
158000991026     C                   movel     VIDfis        virfis
158100991026     C                   movel     VIDfit        virfit
158200000710     C                   movel     VIDfi3        virfi3
158300000710     C                   movel     VIDfi4        virfi4
158400010123     C                   movel     VIDfi2        virfi2
158500030526     C                   MOVEL     VIDtmbd       §VIR01TM
158600120109     C                   EVAL      §VIR01Tip2 =  VIDTip2
158700120109     C                   EVAL      §VIR01Tip3 =  VIDTip3
158800120109     C                   EVAL      §VIR01Tip4 =  VIDTip4
158900120119     C                   IF        VIDKsc2 = *zeros
159000120119     C                   EVAL      VIDKsc2 = *blank
159100120119     C                   ENDIF
159200120119     C                   IF        VIDKsc3 = *zeros
159300120119     C                   EVAL      VIDKsc3 = *blank
159400120119     C                   ENDIF
159500120119     C                   IF        VIDKsc4 = *zeros
159600120119     C                   EVAL      VIDKsc4 = *blank
159700120119     C                   ENDIF
159800120119     C                   EVAL      §VIR01Ksc2 =  VIDKsc2
159900120119     C                   EVAL      §VIR01Ksc3 =  VIDKsc3
160000120119     C                   EVAL      §VIR01Ksc4 =  VIDKsc4
160100030526     C                   MOVEL     DVIR01        virpth
160200000710  ***C*                  movel     VIDctl        virctl
160300991026     C                   Z-ADD     viddtf        G08dat
160400991026     C                   Z-ADD     *ZEROS        G08inv
160500991026     C                   MOVEL     '0'           G08ERR
160600991026     C                   CALL      'XSRDA8'
160700991026     C                   PARM                    WLBDA8
160800991026     C                   Z-ADD     G08inv        virdtf
160900000628     C                   movel     VIDisv        virfi1
161000970526     C*
161100970526     C                   ENDSR
161200970526     C*------------------------------------------------------------------------*
161300970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
161400970526     C*------------------------------------------------------------------------*
161500970526     C     IMPFUN        BEGSR
161600101013     C*
161700101026     C                   SETOFF                                       44
161800101026     C                   SETOFF                                       30
161900101026     C                   SETOFF                                       31
162000101026     C                   SETOFF                                       32
162100101026     C                   SETOFF                                       33
162200970526     C*
162300970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
162400970526IF  1C     DEPOP1        IFEQ      'O05'
162500970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
162600970526     C                   MOVEL     'N'           FUDATT
162700970526     C                   MOVEL     FUDRIG        FUD(6)
162800970526E   1C                   ENDIF
162900141111     C*
163000141111     C* TASTO FUNZIONE F02 ATTIVO SOLO PER OPZIONI "DISPOSITIVE"
163100141111     C                   IF        DEPOP1 = 'O02' OR
163200141111     C                             DEPOP1 = 'O03' OR
163300141111     C                             DEPOP1 = 'F06'
163400141111     C                   MOVEL     FUD(2)        FUDRIG                         *PARAMETRI EXT.
163500141111     C                   MOVEL     'S'           FUDATT
163600141111     C                   MOVEL     FUDRIG        FUD(2)
163700141111     C                   ENDIF
163800970526     C*
163900970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
164000970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
164100970526     C                   EXSR      FUNGES
164200970526     C*
164300970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
164400970526     C*---
164500970526     C* 90 -> PROTEZIONE CHIAVE
164600970526     C* 91 -> PROTEZIONE TOTALE
164700970526     C*---
164800970528IF  1C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
164900970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
165000970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
165100970526     C                   SETOFF                                       91
165200970526E   2C                   ENDIF
165300101013IF  2C     DEPOP1        IFEQ      'O03'                                        *INSERIMENTO
165400101013E   2C                   ENDIF
165500970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
165600991014IF  2C     DEPOP1        oreq      'O01'                                        *MODIFICA
165700970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
165800970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
165900970526E   2C                   ENDIF
166000970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
166100970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
166200970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
166300970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
166400970526     C                   SETON                                        91
166500970526E   2C                   ENDIF
166600970526X   1C                   ELSE                                                   *I N T E R R O G A
166700970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
166800970526     C                   SETON                                        91
166900970526E   1C                   ENDIF
167000970526     C*
167100970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
167200970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
167300991025     C                   MOVEL     S20tip        KCEtip
167400991026     C                   MOVEL     S20ksc        KCEksc
167500000420     C     KEYvi2        CHAIN     tivir03L                           9998
167600080603IF  2C     *IN99         IFEQ      *ON
167700970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
167800970526     C                   MOVEL     MSG(6)        DSMSMS
167900970526X   2C                   ELSE
168000080603     C*
168100080603     C* Aggancio anche il record d estensione regole clienti/tipi-file => TRC 01
168200080603     C                   EVAL      KCEdti  = virDTI
168300080603     C                   EVAL      vireTRC = '01'
168400080603     C     KEYvire1_C    CHAIN     tivire1l                           9998
168500080603     C                   IF        %found(tivire1l)
168600080603     C                   EVAL      DVIRE01 = vireFLO
168700080603     C                   ENDIF
168800150114     C*
168900150114     C* Aggancio anche il record d estensione parametri clienti/tipi-file => TRC PE
169000150114     C                   SETOFF                                       71
169100150114     C                   EVAL      KCEdti  = virDTI
169200150114     C                   EVAL      vireTRC = 'PE'
169300150114     C     KEYvire1_C    CHAIN     tivire1l
169400150114     C                   IF        %found(tivire1l)
169500150114     C                   SETON                                        71
169600150114     C                   ENDIF
169700080603     C*
169800080603     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
169900980128IF  3C     DEPOP1        IFEQ      'O05'
170000991014IF  3C     DEPOP1        oreq      'O02'
170100101026     C*
170200101026     C* Se trattasi d tipo servizio "idoneo" reperisco anche i dati utente lato "BARTVAS"
170300101026     C     virFI1        lookup    isvftpeml                              80
170400101026     C                   IF        *IN80
170500111215     C                             and AS400_ok = *on
170600101026      /FREE
170700101026
170800130215       // (questa attività costa risorse, per cui si permette solo in gestione oppure anche in
170900130215       //  visualizzazione se l'utente è di sede;
171000130215       //  testo solo se l'utente è di sede, perché quelli di filiale non possono andare in gestione
171100130215       //  quindi non potrebbero comunque)
171200130215       if DUtLPO = 'S';
171300101026         if UBBVSQL01_RtvCliTipFile(
171400101026                         pInConn
171500101026                       : virKSC
171600101026                       : virTIP
171700101026                       : pOutNumRec
171800101026                       : pOutCliTipTras
171900101026                       : pOutCliMskChk
172000101026                       : pOutCliMskFil
172100101026                       : pOutCliMskSem
172200101026                       : pOutCliPerc
172300101026                       : pOutCliEml1
172400101026                       : pOutCliEml2
172500101026                       : pOutCliEml3
172600101026                       : pOutCliEml4
172700101026                       : pOutCliEml5
172800101026                       : pOutCliSogg
172900101026                       : pOutCliTesto
173000101026                       : pOutCliFtpAddr
173100101026                       : pOutCliFtpDir
173200101026                       : pOutCliFtpUsr
173300101026                       : pOutCliFtpPwd
173400101026                       : pOutCliFtpFAS4
173500101026                       : pOutCliPercOld
173600101026                       : pOutCliTentat
173700101026                       : pOutCliRitardo
173800101026                       : pOutCliLibAS4
173900101026                       : pOutCliFilAS4
174000101026                       : pOutCliMbrAS4
174100101026                       : pOutCliProc
174200101026                       : pOutCliSched
174300101026                       : pOutCliAccorp
174400101026                       : pOutCliMskZip
174500101026                       : pOutCliDataStr
174600101026                       : pOutCliDataEnd
174700101026                       : pOutCliCodComp
174800101026                       : pOutCliFtpCmd1
174900101026                       : pOutCliEmlErr
175000101026                       : pOutCliHtUrlTo
175100101026                       : pOutCliHtUsrTo
175200101026                       : pOutCliHtPwdTo
175300101026                       : pOutCliHtConTy
175400101026                       : pOutCliHtSndEx
175500101026                       : pOutCliHtSndPr
175600101026                       : pOutCliHtUsPxy
175700101026                       : pOutCliUsrIns
175800101026                       : pOutCliUsrUpd
175900101026                       : pOutCliDatUpd
176000101026                       : pOutCliNote
176100101026                       : pOutCliFtpPrt
176200101026                       : pOutCliFtpTy) = 0;
176300101026              if pOutNumRec = 1;
176400101026                 VIDPPIMC = pOutCliMskChk;
176500101026                 if %len(%trim(pOutCliMskChk)) > %size(VIDPPIMC);
176600101026                    *in30 = *on;
176700101026                 endif;
176800101026                 VIDPPIMF = pOutCliMskFil;
176900101026                 if %len(%trim(pOutCliMskFil)) > %size(VIDPPIMF);
177000101026                    *in31 = *on;
177100101026                 endif;
177200101026                 VIDPPIMS = pOutCliMskSem;
177300101026                 if %len(%trim(pOutCliMskSem)) > %size(VIDPPIMS);
177400101026                    *in32 = *on;
177500101026                 endif;
177600101026                 VIDPPIMZ = pOutCliMskZip;
177700101026                 if %len(%trim(pOutCliMskZip)) > %size(VIDPPIMZ);
177800101026                    *in33 = *on;
177900101026                 endif;
178000101026              endif;
178100101026         endif;
178200130214       endif;
178300101026
178400101026      /END-FREE
178500101026     C
178600101026     C                   ELSE
178700101026     C                   SETON                                        44
178800101026     C                   ENDIF
178900101026     C*
179000980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
179100980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
179200980128E   3C                   ENDIF
179300970526E   2C                   ENDIF
179400970526X   1C                   ELSE                                                   *INSERIMENTO
179500970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
179600970526E   1C                   ENDIF
179700970526     C*
179800970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
179900970526IF  1C     DEPOP1        IFEQ      'O02'
180000970526     C                   MOVEL     DEC(1)        VIDFUN
180100970526E   1C                   ENDIF
180200991014IF  1C     DEPOP1        IFEQ      'O03'
180300991014     C                   MOVEL     DEC(6)        VIDFUN
180400991014E   1C                   ENDIF
180500970526IF  1C     DEPOP1        IFEQ      'O04'
180600970526     C                   MOVEL     DEC(2)        VIDFUN
180700970526E   1C                   ENDIF
180800970526IF  1C     DEPOP1        IFEQ      'O05'
180900970526     C                   MOVEL     DEC(3)        VIDFUN
181000970526E   1C                   ENDIF
181100970526IF  1C     DEPOP1        IFEQ      'O07'
181200970526     C                   MOVEL     DEC(4)        VIDFUN
181300970526E   1C                   ENDIF
181400970526IF  1C     DEPOP1        IFEQ      'F06'
181500970526     C                   MOVEL     DEC(5)        VIDFUN
181600970526E   1C                   ENDIF
181700970526     C*
181800970526     C                   ENDSR
181900101026
182000101026
182100101026
182200101026     C*--------------------------------------------------------------------------------------------*
182300101026     C* CARTAB - carica le tabelle occorrenti
182400101026     C*--------------------------------------------------------------------------------------------*
182500101026     C     CARTAB        BEGSR
182600101026     C*
182700101026     C* Tipi servizi soggetti a controllo unificazioni
182800101026     C                   z-add     *zeros        ww                3 0
182900101026     C                   eval      tbecod = 'ISV'
183000101026     C     tbecod        setll     tntbe01l
183100101026     C     tbecod        reade     tntbe01l
183200101026     C                   dow       not %eof(tntbe01l)
183300101026     C                   movel     tbeuni        disv
183400101026     C                   if        §isvproc = 'F'
183500101026     C                   add       1             ww
183600101026     C                   eval      isvftpeml(ww) = tbeke1
183700101026     C                   endif
183800101026     C     tbecod        reade     tntbe01l
183900101026     C                   enddo
184000101026     C*
184100101026     C                   ENDSR
184200101026
184300101026     c*------------------------------------------------------------------------*
184400101026     c* opnCONN - Apertura connessione
184500101026     c*------------------------------------------------------------------------*
184600101026     c     OPNCONN       BEGSR
184700101026      /FREE
184800101026
184900101026        if not *in40;
185000101026           if UBBVSQL01_Connect('BARTVAS' : pOutConn) = 0;
185100101026              pInConn = pOutConn;
185200101026           else;
185300101026              *in40 = *on;
185400101026              DSMSMS = MSG(28);
185500101026           endif;
185600101026        endif;
185700101026
185800101026      /END-FREE
185900101026      *
186000101026     c                   ENDSR
186100101026     c*------------------------------------------------------------------------*
186200101026
186300101026
186400101026
186500101026     c*------------------------------------------------------------------------*
186600101026     c* clsCONN - Chiusura connessione
186700101026     c*------------------------------------------------------------------------*
186800101026     c     CLSCONN       BEGSR
186900101026      /FREE
187000101026
187100101026           if UBBVSQL01_Disconnect(pInConn) = 0;
187200101026           else;
187300101026              *in40 = *on;
187400101026           endif;
187500101026
187600101026      /END-FREE
187700101026      *
187800101026     c                   ENDSR
187900101026     c*------------------------------------------------------------------------*
188000101026
188100101026
188200101026
188300970526     C*------------------------------------------------------------------------*
188400970526     C* *INZSR - OPERAZIONI INIZIALI
188500970526     C*------------------------------------------------------------------------*
188600970526     C     *INZSR        BEGSR
188700970526     C*---
188800970526     C* RICEVIMENTO PARAMETRI
188900970526     C*---
189000970526     C     *ENTRY        PLIST
189100970612     C                   PARM                    KPJBA
189200991028     C                   movel     kpjbu         tis720
189300000420     C*
189400000420     C* Inverto subito la data presa in input
189500000420     C                   Z-ADD     s20dti        wda8
189600000420     C                   Z-ADD     *ZEROS        da8w
189700000420     C                   EXSR      CHKDA8
189800000420     C                   Z-ADD     DA8W          KCEdti
189900130215     C*
190000130215     C     *DTAARA       DEFINE    §azute        azuteds
190100130215     C     *DTAARA       DEFINE    §datiute      ddatiute
190200130215     C                   IN(E)     *DTAARA
190300130215if  1C                   IF        %Error
190400130215     c                   EVAL      I34Tla = 'L'
190500130215     C                   CALL      'TIBS34R'
190600130215     C                   PARM                    Tibs34Ds
190700130215     C                   IN        *DTAARA
190800130215e   1C                   ENDIF
190900970526     C*
191000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
191100970526     C     XMSGPA        PLIST
191200970526     C                   PARM                    DSMSMS
191300970526     C                   PARM                    DSMSPG
191400970526     C                   PARM                    DSMSKE
191500970526     C                   PARM                    DSMSER
191600991108     C*---
191700970526     C*
191800970526     C* IMPOSTA LA DS -TASTI PREMUTI-
191900970526     C                   CALL      'XKEY'
192000970526     C                   PARM                    DSKEY
192100101026     C*
192200101026     C* CARICA LE TABELLE OCCORRENTI
192300101027     C                   EXSR      CARTAB
192400111215     C*
192500111215     C* Reperisco il sistema AS/400 corrente
192600111215     C                   EVAL      AS400_ok = *off
192700111215     C                   callp     UBRTVNETA_Rtv(currSysNeta)
192800111215     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
192900111215     C                   EVAL      AS400_ok = *on
193000111215     C                   ENDIF
193100101026     C*
193200130214     C* Apro la connessione al DB "BARTVAS" (solo se no solo visualizzazione)
193300130215     C* (questa attività costa risorse, per cui si permette solo in gestione oppure anche in
193400130215     C*  visualizzazione se l'utente è di sede;
193500130215     C*  testo solo se l'utente è di sede, perché quelli di filiale non possono andare in gestione,
193600130215     C*  quindi non potrebbero comunque)
193700130215     C                   IF        DUtLPO = 'S'
193800111215     C                   IF        AS400_ok = *on
193900111215     C                   exsr      opnCONN
194000111215     C                   ENDIF
194100130214     C                   ENDIF
194200970526     C*---
194300970526     C* VARIABILI NON RIFERITE AL DATA BASE
194400970526     C*---
194500970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
194600991025     C     *LIKE         DEFINE    S20OP0        DEPOP0
194700991025     C     *LIKE         DEFINE    S20OP1        DEPOP1
194800991025     C     *LIKE         DEFINE    S20OP0        WOP0
194900991025     C     *LIKE         DEFINE    S20OP1        WOP1
195000970526     C*---
195100970526     C* VARIABILI DI CONTROLLO
195200970526     C*---
195300970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
195400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
195500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
195600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
195700970526     C*
195800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
195900991026     C                   MOVEL     'TIS723R'     DSMSPG
196000991026     C                   MOVEL     'TIS723R'     MSGQUE
196100970526     C                   MOVEL     *BLANKS       MSGKEY
196200970526     C                   MOVEL     *BLANKS       DSMSMS
196300970526     C*---
196400970526     C* VARIABILI RIFERITE AL DATA BASE
196500970526     C*---
196600991026     C     *LIKE         DEFINE    virtip        KCEtip
196700991026     C     *LIKE         DEFINE    virksc        KCEksc
196800000420     C     *LIKE         DEFINE    virdti        KCEdti
196900970526     C*---
197000970526     C* CHIAVI DI LETTURA
197100970526     C*---
197200000420     C* LETTURA tivir03L - INCOMPLETA
197300991026     C     KEYvir        KLIST
197400080603     C                   KFLD                    KCEtip
197500080603     C                   KFLD                    KCEksc
197600080603     C* LETTURA tivir03L - COMPLETA
197700080603     C     KEYvi2        KLIST
197800080603     C                   KFLD                    KCEtip
197900080603     C                   KFLD                    KCEksc
198000080603     C                   KFLD                    KCEdti
198100080603     C* LETTURA tivire1L - COMPLETA
198200080603     C     KEYvire1_C    KLIST
198300080603     C                   KFLD                    KCEksc
198400080603     C                   KFLD                    KCEtip
198500080603     C                   KFLD                    KCEdti
198600080603     C                   KFLD                    vireTRC
198700080603     C* LETTURA tntbe01L - PARZIALE
198800040816     C     KEYtbe1_P     KLIST
198900040816     C                   KFLD                    TBEcod                         *CODICE TABELLA
199000120111     C* LETTURA tivir06L - COMPLETA
199100120111     C     K03VIR06      KLIST
199200120111     C                   KFLD                    x_VIRKsc
199300120111     C                   KFLD                    x_VIRTip
199400120111     C                   KFLD                    x_VIRDtF
199500120111     C* LETTURA tivir06L - PARZIALE
199600120111     C     K02VIR06      KLIST
199700120111     C                   KFLD                    x_VIRKsc
199800120111     C                   KFLD                    x_VIRTip
199900970528     C*
200000970528     C* CALCOLA LA DATA CORRENTE
200100970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
200200970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
200300970528     C                   Z-ADD     WN8           G08DAT
200400051122     C                   Z-ADD     WN8           DATCORINV         8 0
200500970528     C                   Z-ADD     *ZEROS        G08INV
200600970528     C                   MOVEL     '0'           G08ERR
200700970528     C                   CALL      'XSRDA8'
200800970528     C                   PARM                    WLBDA8
200900970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
201000970526     C*
201100970526     C* DEPOSITA LE OPZIONI DI ENTRATA
201200991025     C                   MOVEL     S20OP0        DEPOP0                         *LIVELLO PROCEDURA
201300991025     C                   MOVEL     S20OP1        DEPOP1                         *LIVELLO PROGRAMMA
201400051122     C*
201500051122     C* VERIFICA L'OPZIONE D ENTRATA X PILOTARE DETERMINATE FORZATURE A VIDEO
201600051122     C                   IF        DEPOP1 = 'F06' OR
201700051122     C                             DEPOP1 = 'O03' OR
201800051122     C                             DEPOP1 = 'O07'
201900051122     C                   EVAL      VIDDTI = DATCORINV
202000060726     C                   EVAL      VIDDTF = 31122039
202100051122     C                   ENDIF
202200000629     C*
202300000629     C* EFFETTUO CONSIDERAZIONI SULLE OPZIONI DI ENTRATA
202400000629     C                   IF        DEPOP1 = 'F06' OR
202500000629     C                             DEPOP1 = 'O03'
202600000629     C                   SETOFF                                       92
202700000629     C                   ELSE
202800000629     C                   SETON                                        92
202900000629     C                   ENDIF
203000970526     C*
203100970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
203200970526     C                   EXSR      IMPFUN
203300970526     C*
203400970526     C                   ENDSR
203500970526     C*------------------------------------------------------------------------*
203600970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
203700970526     C*------------------------------------------------------------------------*
203800970526     C     FUNGES        BEGSR
203900970526     C*
204000970526     C* PULISCO CAMPI DI LAVORO
204100970526     C                   Z-ADD     0             W                 5 0
204200970526     C*
204300970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
204400970526     C* E CARICO TABELLA FUNZIONI ATTIVE
204500970526     C     1             DO        24            I
204600970526     C                   MOVEL     FUD(I)        FUDRIG
204700970526     C*
204800970526     C* PULIZIA FUNZIONI PRECEDENTI
204900970526     C                   MOVEL     *BLANKS       FUV(I)
205000970526     C*
205100970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
205200970526     C* (SALTA F24)
205300970526     C     FUDATT        IFEQ      'S'
205400970526     C     FUDFUN        ANDNE     '24'
205500970526     C                   ADD       1             W
205600970526     C                   MOVEL     FUDDES        FUV(W)
205700970526     C                   END
205800970526     C                   END                                                    DO WHILE
205900970526     C*
206000970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
206100970526     C* ALTRIMENTI DISATTIVO F24
206200970526     C     1             DO        24            I
206300970526     C                   MOVEL     FUD(I)        FUDRIG
206400970526     C*
206500970526     C     FUDFUN        IFEQ      '24'
206600970526     C     W             IFGT      6
206700970526     C                   MOVEL     'S'           FUDATT
206800970526     C                   ELSE
206900970526     C                   MOVEL     *BLANKS       FUDATT
207000970526     C                   END                                                    W GT 8
207100970526     C                   MOVEL     FUDRIG        FUD(I)
207200970526     C                   END
207300970526     C                   END                                                    DO WHILE
207400970526     C*
207500970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
207600970526     C     W             DIV       6             I
207700970526     C                   MVR                     Z
207800970526     C*
207900970526     C     Z             IFNE      0
208000970526     C     I             ADD       1             Z
208100970526     C                   ELSE
208200970526     C                   Z-ADD     I             Z                 5 0
208300970526     C                   END
208400970526     C*
208500970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
208600970526     C                   ADD       1             FUNGRU
208700970526     C     FUNGRU        IFGT      Z
208800970526     C     FUNGRU        ORLT      1
208900970526     C                   Z-ADD     1             FUNGRU
209000970526     C                   END
209100970526     C*
209200970526     C* PRIMO GRUPPO DI 6 FUNZIONI
209300970526     C     FUNGRU        IFEQ      1
209400970526     C                   MOVEL     FUV(1)        FUN001
209500970526     C                   MOVEL     FUV(2)        FUN002
209600970526     C                   MOVEL     FUV(3)        FUN003
209700970526     C                   MOVEL     FUV(4)        FUN004
209800970526     C                   MOVEL     FUV(5)        FUN005
209900970526     C                   MOVEL     FUV(6)        FUN006
210000970526     C                   END
210100970526     C*
210200970526     C* SECONDO GRUPPO DI 6 FUNZIONI
210300970526     C     FUNGRU        IFEQ      2
210400970526     C                   MOVEL     FUV(7)        FUN001
210500970526     C                   MOVEL     FUV(8)        FUN002
210600970526     C                   MOVEL     FUV(9)        FUN003
210700970526     C                   MOVEL     FUV(10)       FUN004
210800970526     C                   MOVEL     FUV(11)       FUN005
210900970526     C                   MOVEL     FUV(12)       FUN006
211000970526     C                   END
211100970526     C*
211200970526     C* TERZO GRUPPO DI 6 FUNZIONI
211300970526     C     FUNGRU        IFEQ      3
211400970526     C                   MOVEL     FUV(13)       FUN001
211500970526     C                   MOVEL     FUV(14)       FUN002
211600970526     C                   MOVEL     FUV(15)       FUN003
211700970526     C                   MOVEL     FUV(16)       FUN004
211800970526     C                   MOVEL     FUV(17)       FUN005
211900970526     C                   MOVEL     FUV(18)       FUN006
212000970526     C                   END
212100970526     C*
212200970526     C* QUARTO GRUPPO DI 6 FUNZIONI
212300970526     C     FUNGRU        IFEQ      4
212400970526     C                   MOVEL     FUV(19)       FUN001
212500970526     C                   MOVEL     FUV(20)       FUN002
212600970526     C                   MOVEL     FUV(21)       FUN003
212700970526     C                   MOVEL     FUV(22)       FUN004
212800970526     C                   MOVEL     *BLANKS       FUN005
212900970526     C                   MOVEL     *BLANKS       FUN006
213000970526     C                   END
213100970526     C*
213200970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
213300970526     C                   MOVEL     *BLANKS       FUN007
213400970526     C                   MOVEL     *BLANKS       FUN008
213500970526     C*
213600970526     C     1             DO        24            I
213700970526     C                   MOVEL     FUD(I)        FUDRIG
213800970526     C     FUDFUN        IFEQ      '24'
213900970526     C     FUDATT        ANDEQ     'S'
214000970526     C                   MOVEL     FUDDES        FUN008
214100970526     C                   END
214200970526     C                   END                                                    DO
214300970526     C*
214400970526     C                   ENDSR
214500970526     C*------------------------------------------------------------------------*
214600970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
214700970526     C*------------------------------------------------------------------------*
214800970526     C     FUNCON        BEGSR
214900970526     C*
215000970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
215100970526     C                   EXSR      FUNVAR
215200970526     C*
215300970526     C* PULISCO CAMPI DI LAVORO
215400970526     C                   Z-ADD     0             I                 5 0
215500970526     C                   Z-ADD     0             W                 5 0
215600970526     C*
215700970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
215800970526     C     FUNKEY        IFNE      *BLANKS
215900970526     C     1             DO        24            I
216000970526     C                   MOVEL     FUD(I)        FUDRIG
216100970526     C*
216200970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
216300970526     C     FUNKEY        IFEQ      FUDFUN
216400970526     C     FUDATT        ANDEQ     'S'
216500970526     C                   Z-ADD     I             W
216600970526     C                   END
216700970526     C                   END                                                    DO WHILE
216800970526     C*
216900970526     C* FUNZIONE NON ATTIVA (ERRORE)
217000970526     C     W             IFEQ      0
217100970526     C*
217200970526     C* EMISSIONE MESSAGGIO DI ERRORE
217300970526     C                   MOVEL     MSG(1)        DSMSMS
217400970526     C*
217500970526     C                   END                                                    W=0
217600970526     C                   END                                                    FUNKEY NE *BLANKS
217700970526     C*
217800970526     C* FUNZIONE ATTIVA (PROCEDI ...)
217900970526     C     W             IFNE      0
218000970526     C     FUNKEY        OREQ      *BLANKS
218100970526     C     FUNKEY        IFEQ      *BLANKS
218200970526     C                   EXSR      ENTGES
218300970526     C                   END
218400031229     C     FUNKEY        IFEQ      '01'
218500031229     C                   EXSR      F01GES
218600031229     C                   END
218700141111     C     FUNKEY        IFEQ      '02'
218800141111     C                   EXSR      F02GES
218900141111     C                   END
219000970526     C     FUNKEY        IFEQ      '03'
219100970526     C                   EXSR      F03GES
219200970526     C                   END
219300970526     C     FUNKEY        IFEQ      '06'
219400970526     C                   EXSR      F06GES
219500970526     C                   END
219600970526     C     FUNKEY        IFEQ      '12'
219700970526     C                   EXSR      F12GES
219800970526     C                   END
219900970526     C     FUNKEY        IFEQ      '24'
220000970526     C                   EXSR      F24GES
220100970526     C                   END
220200970526     C                   END
220300970526     C*
220400970526     C                   ENDSR
220500970526     C*------------------------------------------------------------------------*
220600970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
220700970526     C*------------------------------------------------------------------------*
220800970526     C     FUNVAR        BEGSR
220900970526     C*
221000970526     C* INIZIALIZZA VARIABILE -FUNKEY-
221100970526     C*
221200970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
221300970526     C     KEY           IFEQ      ENTER
221400970526     C                   MOVEL     *BLANKS       FUNKEY
221500970526     C                   END
221600970526     C     KEY           IFEQ      F1
221700970526     C                   MOVEL     '01'          FUNKEY
221800970526     C                   END
221900970526     C     KEY           IFEQ      F2
222000970526     C                   MOVEL     '02'          FUNKEY
222100970526     C                   END
222200970526     C     KEY           IFEQ      F3
222300970526     C                   MOVEL     '03'          FUNKEY
222400970526     C                   END
222500970526     C     KEY           IFEQ      F4
222600970526     C                   MOVEL     '04'          FUNKEY
222700970526     C                   END
222800970526     C     KEY           IFEQ      F5
222900970526     C                   MOVEL     '05'          FUNKEY
223000970526     C                   END
223100970526     C     KEY           IFEQ      F6
223200970526     C                   MOVEL     '06'          FUNKEY
223300970526     C                   END
223400970526     C     KEY           IFEQ      F7
223500970526     C                   MOVEL     '07'          FUNKEY
223600970526     C                   END
223700970526     C     KEY           IFEQ      F8
223800970526     C                   MOVEL     '08'          FUNKEY
223900970526     C                   END
224000970526     C     KEY           IFEQ      F9
224100970526     C                   MOVEL     '09'          FUNKEY
224200970526     C                   END
224300970526     C     KEY           IFEQ      F10
224400970526     C                   MOVEL     '10'          FUNKEY
224500970526     C                   END
224600970526     C     KEY           IFEQ      F11
224700970526     C                   MOVEL     '11'          FUNKEY
224800970526     C                   END
224900970526     C     KEY           IFEQ      F12
225000970526     C                   MOVEL     '12'          FUNKEY
225100970526     C                   END
225200970526     C     KEY           IFEQ      F13
225300970526     C                   MOVEL     '13'          FUNKEY
225400970526     C                   END
225500970526     C     KEY           IFEQ      F14
225600970526     C                   MOVEL     '14'          FUNKEY
225700970526     C                   END
225800970526     C     KEY           IFEQ      F15
225900970526     C                   MOVEL     '15'          FUNKEY
226000970526     C                   END
226100970526     C     KEY           IFEQ      F16
226200970526     C                   MOVEL     '16'          FUNKEY
226300970526     C                   END
226400970526     C     KEY           IFEQ      F17
226500970526     C                   MOVEL     '17'          FUNKEY
226600970526     C                   END
226700970526     C     KEY           IFEQ      F18
226800970526     C                   MOVEL     '18'          FUNKEY
226900970526     C                   END
227000970526     C     KEY           IFEQ      F19
227100970526     C                   MOVEL     '19'          FUNKEY
227200970526     C                   END
227300970526     C     KEY           IFEQ      F20
227400970526     C                   MOVEL     '20'          FUNKEY
227500970526     C                   END
227600970526     C     KEY           IFEQ      F21
227700970526     C                   MOVEL     '21'          FUNKEY
227800970526     C                   END
227900970526     C     KEY           IFEQ      F22
228000970526     C                   MOVEL     '22'          FUNKEY
228100970526     C                   END
228200970526     C     KEY           IFEQ      F23
228300970526     C                   MOVEL     '23'          FUNKEY
228400970526     C                   END
228500970526     C     KEY           IFEQ      F24
228600970526     C                   MOVEL     '24'          FUNKEY
228700970526     C                   END
228800970526     C     KEY           IFEQ      ROLLUP
228900970526     C                   MOVEL     'RU'          FUNKEY
229000970526     C                   END
229100970526     C     KEY           IFEQ      HELP
229200970526     C                   MOVEL     'HP'          FUNKEY
229300970526     C                   END
229400970526     C                   ENDSR
229500970526     C*------------------------------------------------------------------------*
229600970526     C* NOTE COSTRUZIONE SCHIERE
229700970526     C*------------------------------------------------------------------------*
229800970526     C*
229900970526     C* -MSG-MESSAGGI
230000970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
230100970526     C* (NON TOCCARE MSG,1 E MSG,2)
230200970526     C*
230300970526     C* -CMD-COMANDI
230400970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
230500970526     C*
230600970526     C* -OPD-OPZIONI
230700970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
230800970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
230900970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
231000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
231100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
231200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
231300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
231400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
231500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
231600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
231700970526     C*
231800970526     C* -FUD-FUNZIONI
231900970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
232000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
232100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
232200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
232300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
232400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
232500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
232600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
232700970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
232800970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
232900970526     O*------------------------------------------------------------------------*
233000970526** DEC - DECODIFICHE
233100970526    MODIFICA                                                      1
233200970526  ANNULLAMENTO                                                    2
233300970526VISUALIZZAZIONE                                                   3
233400970526   RIPRISTINO                                                     4
233500970526  INSERIMENTO                                                     5
233600991014     COPIA                                                        6
233700970526** MSG - MESSAGGI
233800991026TIS723R- Tasto funzionale non gestito                                          1
233900991026TIS723R- Inserimento effettuato                                                2
234000991026TIS723R- Modifica effettuata                                                   3
234100991026TIS723R- Annullamento effettuato                                               4
234200991026TIS723R- Ripristino effettuato                                                 5
234300991026TIS723R- Richiesta funzione per record inesistente                             6
234400991026TIS723R- Record momentaneamente allocato: riprovare più tardi                  7
234500991026TIS723R- Richiesta funzione per record esistente                               8
234600991026TIS723R- Questo codice cliente è già presente con questo tipo file             9
234700991027TIS723R- Il codice tipo file non è fra quelli presenti in anagrafico           10
234800991027TIS723R- Il codice cliente non è presente come padre nell'archivio UNIFICANTI  11
234900000724TIS723R- Campo obbligatorio                                                    12
235000991027TIS723R- Codice filiale inesistente o di categoria V=varia                     13
235100000724TIS723R- Data errata                                                           14
235200000724TIS723R- La data fine deve essere maggiore della data inizio                   15
235300000724TIS723R- Il record non può scadere in un giorno già trascorso                  16
235400000724TIS723R- Il record immesso si sovrappone ad uno già in decorrenza              17
235500000724TIS723R- Codice tipo servizio non presente in tabella                          18
235600001120TIS723R- Oggetto inesistente, verificare e correggere.                         19
235700001120TIS723R- File inesistente, verificare e correggere.                            20
235800001120TIS723R- Libreria inesistente, verificare e correggere.                        21
235900030603TIS723R- Tipo membro non impostabile x il tipo servizio indicato.              22
236000040107TIS723R- Per il traduttore indicato non esiste descrizione dettaglio parametri 23
236100040319TIS723R- Tipo file e servizio incompatibili, correggere.                       24
236200040816TIS723R- Tipo file e/o programma traduttore e/o file tradotto incompatibili.   25
236300051026TIS723R- ATTENZIONE presenti log a stato 0 impossibile cambiare traduttore!    26
236400060616TIS723R- Valori incompatibili, correggere!                                     27
236500101026TIS723R- Errore BartVAS: fatal error                                           28
236600101027TIS723R- ATTENZIONE: modifica effettuata lato AS/400 ma non lato BARTVAS       29
236700120109TIS723R- Tipo file per Internet non impostabile x il tipo servizio indicato.   30
236800120119TIS723R- Tipo file non abilitato per cliente indicato                          31
236900120119TIS723R- Loop tra il tipo file / cliente corrente e quello per duplica         32
237000120119TIS723R- Tipo file / cliente per duplica non possono essere quelli correnti    33
237100120119TIS723R- Valorizzato il cliente per duplica ma non il rispettivo tipo file     34
237200170119TIS723R- Se presenti parametri è richiersto anche *pgm traduttore              35
237300170119TIS723R- Se presente *pgm traduttore è richiesto anche file tradotto           36
237400170119TIS723R- Se presenti parametri è richiersto anche *pgm post-traduzione         37
237500970526** FUD - FUNZIONI
237600141111F01=Desc.Parametri -        (S/N)=S  <== S-FISSO           funz.01¬
237700141111F02=Parametri      -        (S/N)=N                        funz.02¬
237800970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
237900970526                   -        (S/N)=N                        funz.04¬
238000970526                   -        (S/N)=N                        funz.05¬
238100970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
238200970527                   -        (S/N)=N                        funz.07¬
238300970527                   -        (S/N)=N                        funz.08¬
238400970527                   -        (S/N)=N                        funz.09¬
238500970527                   -        (S/N)=N                        funz.10¬
238600970526                   -        (S/N)=N                        funz.11¬
238700970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
238800970526                   -        (S/N)=N                        funz.13¬
238900970526                   -        (S/N)=N                        funz.14¬
239000970526                   -        (S/N)=N                        funz.15¬
239100970526                   -        (S/N)=N                        funz.16¬
239200970526                   -        (S/N)=N                        funz.17¬
239300970526                   -        (S/N)=N                        funz.18¬
239400970526                   -        (S/N)=N                        funz.19¬
239500970526                   -        (S/N)=N                        funz.20¬
239600970526                   -        (S/N)=N                        funz.21¬
239700970526                   -        (S/N)=N                        funz.22¬
239800970526                   -        (S/N)=N                        funz.23¬
239900970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
